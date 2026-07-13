{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK hide #-}

module Vpipe.Graphics.Submission.Internal (
  OwnedActions,
  SubmittedWorkStatus (..),
  newOwnedActions,
  releaseOwnedActions,
  transferOwnedActions,
  releaseActions,
  retireOwnedActions,
  confirmSubmittedWork,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, modifyMVarMasked_, newMVar, readMVar)
import Control.Exception (SomeException, finally, mask_, try, uninterruptibleMask_)
import Data.Maybe (fromMaybe)

data ActionOwnership
  = ActionsOwned [IO ()]
  | ActionsReleased
  | ActionsTransferred

newtype OwnedActions = OwnedActions (MVar ActionOwnership)

data SubmittedWorkStatus
  = SubmittedWorkComplete
  | SubmittedWorkCompleteAfterFailure SomeException
  | SubmittedWorkUncertain SomeException SomeException

newOwnedActions :: [IO ()] -> IO OwnedActions
newOwnedActions actions = OwnedActions <$> newMVar (ActionsOwned actions)

{- | Releases locally owned actions exactly once. Transferred actions are owned
by their recipient and are deliberately left untouched.
-}
releaseOwnedActions :: OwnedActions -> IO ()
releaseOwnedActions (OwnedActions ownership) = mask_ $ do
  actions <-
    modifyMVarMasked ownership $ \case
      ActionsOwned values -> pure (ActionsReleased, values)
      ActionsReleased -> pure (ActionsReleased, [])
      ActionsTransferred -> pure (ActionsTransferred, [])
  releaseActions actions

{- | Transfers locally owned actions exactly once. 'Nothing' means ownership
was already released or transferred.
-}
transferOwnedActions :: OwnedActions -> IO (Maybe [IO ()])
transferOwnedActions (OwnedActions ownership) = mask_ $
  modifyMVarMasked ownership $ \case
    ActionsOwned actions -> pure (ActionsTransferred, Just actions)
    ActionsReleased -> pure (ActionsReleased, Nothing)
    ActionsTransferred -> pure (ActionsTransferred, Nothing)

releaseActions :: [IO ()] -> IO ()
releaseActions actions = case actions of
  [] -> pure ()
  action : rest -> action `finally` releaseActions rest

{- | Registers deferred cleanup before transferring ownership. The registrar
must defer the supplied action until after this call returns, as Context
finalizers do. If registration fails, ownership is still abandoned locally:
unknown GPU work makes a leak safer than premature destruction.
-}
retireOwnedActions :: (IO () -> IO ()) -> [OwnedActions] -> IO (Either SomeException ())
retireOwnedActions registerFinalizer owners = mask_ $ do
  payload <- newMVar Nothing
  registration <- try (registerFinalizer (readMVar payload >>= maybe (pure ()) releaseActions))
  transferred <- traverse transferOwnedActions owners
  case registration of
    Right () ->
      modifyMVarMasked_ payload (const (pure (Just (concatMap (fromMaybe []) transferred))))
    Left _ -> pure ()
  pure registration

{- | Runs the normal post-submit completion path. If it fails, an
uninterruptible fallback wait determines whether caller-owned Vulkan objects
may be reclaimed locally.
-}
confirmSubmittedWork :: IO () -> IO () -> IO SubmittedWorkStatus
confirmSubmittedWork primary fallback = do
  primaryResult <- try primary
  case primaryResult of
    Right () -> pure SubmittedWorkComplete
    Left primaryFailure -> do
      fallbackResult <- try (uninterruptibleMask_ fallback)
      pure $ case fallbackResult of
        Right () -> SubmittedWorkCompleteAfterFailure primaryFailure
        Left fallbackFailure -> SubmittedWorkUncertain primaryFailure fallbackFailure
