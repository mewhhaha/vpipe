module Vpipe.Resource.Lifetime (
  ResourceGeneration,
  newResourceGeneration,
  LifetimeGate,
  newLifetimeGate,
  acquireLifetimeLease,
  withLifetimeLease,
  quarantineLifetimeGate,
  sealLifetimeGate,
  closeLifetimeGate,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, modifyMVarMasked_, newMVar, readMVar, takeMVar, tryPutMVar)
import Control.Exception (finally, mask, mask_, throwIO)
import Control.Monad (void, when)
import Data.Unique (Unique, newUnique)

import Vpipe.Error (VpipeError (ResourceQuarantined))

newtype ResourceGeneration = ResourceGeneration Unique
  deriving stock (Eq, Ord)

newResourceGeneration :: IO ResourceGeneration
newResourceGeneration = ResourceGeneration <$> newUnique

data LifetimeStatus = LifetimeOpen | LifetimeSealed | LifetimeQuarantined
  deriving stock (Eq)

data LifetimeState = LifetimeState
  { lifetimeStatus :: LifetimeStatus
  , lifetimeActiveLeases :: Int
  }

data LifetimeGate = LifetimeGate (MVar LifetimeState) (MVar ())

newLifetimeGate :: IO LifetimeGate
newLifetimeGate = LifetimeGate <$> newMVar (LifetimeState LifetimeOpen 0) <*> newMVar ()

{- | Returns 'Nothing' after normal sealing and throws 'ResourceQuarantined'
after quarantine. The returned action may be called more than once.
-}
acquireLifetimeLease :: LifetimeGate -> IO (Maybe (IO ()))
acquireLifetimeLease gate@(LifetimeGate stateVariable drained) = mask_ $ do
  result <-
    modifyMVarMasked stateVariable $ \state ->
      case lifetimeStatus state of
        LifetimeQuarantined -> pure (state, LifetimeLeaseQuarantined)
        LifetimeSealed -> pure (state, LifetimeLeaseRejected)
        LifetimeOpen -> do
          when (lifetimeActiveLeases state == 0) (takeMVar drained)
          pure (state{lifetimeActiveLeases = lifetimeActiveLeases state + 1}, LifetimeLeaseAcquired)
  case result of
    LifetimeLeaseAcquired -> Just <$> newLifetimeLeaseRelease gate
    LifetimeLeaseRejected -> pure Nothing
    LifetimeLeaseQuarantined -> throwIO ResourceQuarantined

data LifetimeLeaseResult = LifetimeLeaseAcquired | LifetimeLeaseRejected | LifetimeLeaseQuarantined

newLifetimeLeaseRelease :: LifetimeGate -> IO (IO ())
newLifetimeLeaseRelease gate = do
  released <- newMVar False
  pure $
    modifyMVarMasked_ released $ \done ->
      if done
        then pure True
        else releaseLifetimeLease gate >> pure True

releaseLifetimeLease :: LifetimeGate -> IO ()
releaseLifetimeLease (LifetimeGate stateVariable drained) =
  modifyMVarMasked_ stateVariable $ \state -> do
    let remaining = lifetimeActiveLeases state - 1
    if remaining == 0
      then do
        void (tryPutMVar drained ())
        pure state{lifetimeActiveLeases = 0}
      else pure state{lifetimeActiveLeases = remaining}

withLifetimeLease :: LifetimeGate -> IO a -> IO a -> IO a
withLifetimeLease gate closed action = mask $ \restore -> do
  lease <- acquireLifetimeLease gate
  case lease of
    Nothing -> closed
    Just release -> restore action `finally` release

{- | Permanently prevents new leases. A close waiting for active leases wakes
and fails, leaving destruction to context recreation rather than risking an
object that an uncertain submission may still use.
-}
quarantineLifetimeGate :: LifetimeGate -> IO ()
quarantineLifetimeGate (LifetimeGate stateVariable drained) = mask_ $
  modifyMVarMasked_ stateVariable $ \state -> do
    void (tryPutMVar drained ())
    pure state{lifetimeStatus = LifetimeQuarantined}

-- | Prevents new leases without waiting for active leases to finish.
sealLifetimeGate :: LifetimeGate -> IO ()
sealLifetimeGate (LifetimeGate stateVariable _) =
  modifyMVarMasked_ stateVariable $ \state ->
    case lifetimeStatus state of
      LifetimeQuarantined -> pure state
      LifetimeSealed -> pure state
      LifetimeOpen -> pure state{lifetimeStatus = LifetimeSealed}

{- | Prevents new leases and waits for the active ones to finish. Repeated
calls are harmless.
-}
closeLifetimeGate :: LifetimeGate -> IO ()
closeLifetimeGate gate@(LifetimeGate stateVariable drained) = mask_ $ do
  sealLifetimeGate gate
  void (readMVar drained)
  state <- readMVar stateVariable
  when (lifetimeStatus state == LifetimeQuarantined) (throwIO ResourceQuarantined)
