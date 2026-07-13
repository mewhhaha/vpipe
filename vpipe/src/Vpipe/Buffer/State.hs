{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Transactional last-use tracking shared by uploads and command recording.
module Vpipe.Buffer.State (
  BufferState,
  BufferCompletion (..),
  BufferUse (..),
  Reservation,
  BufferUsePending (..),
  newBufferState,
  beginBufferUse,
  quarantineBufferState,
  quarantineBufferUse,
  reservationPreviousUse,
  commitBufferUse,
  cancelBufferUse,
  lastBufferUse,
) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, retry, throwSTM, writeTVar)
import Control.Exception (Exception, uninterruptibleMask_)
import Data.Word (Word32, Word64)
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlags2)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)

import Vpipe.Context.Queue (Queue, queueFamilyIndex)
import Vpipe.Error (VpipeError (ResourceQuarantined))

data BufferCompletion = BufferCompletion
  { bufferCompletionQueue :: Queue
  , bufferCompletionQueueFamily :: Word32
  , bufferCompletionTimeline :: Word64
  }

instance Eq BufferCompletion where
  left == right =
    bufferCompletionQueueFamily left == bufferCompletionQueueFamily right
      && bufferCompletionTimeline left == bufferCompletionTimeline right

instance Show BufferCompletion where
  showsPrec precedence completion =
    showParen (precedence > 10) $
      showString "BufferCompletion "
        . shows (queueFamilyIndex (bufferCompletionQueue completion))
        . showChar ' '
        . shows (bufferCompletionTimeline completion)

data BufferUse = BufferUse
  { bufferUseStage :: PipelineStageFlags2
  , bufferUseAccess :: AccessFlags2
  , bufferUseCompletion :: Maybe BufferCompletion
  }
  deriving stock (Eq)

instance Show BufferUse where
  showsPrec precedence use =
    showParen (precedence > 10) $
      showString "BufferUse "
        . shows (bufferUseStage use)
        . showChar ' '
        . shows (bufferUseAccess use)
        . showChar ' '
        . shows (bufferUseCompletion use)

data BufferUsePending = BufferUsePending
  deriving stock (Eq, Show)

instance Exception BufferUsePending

{-# DEPRECATED BufferUsePending "Reservations now wait for the active use to finish; this exception is no longer thrown." #-}

data State = State
  { bufferStateQuarantined :: Bool
  , nextReservationToken :: Integer
  , activeReservation :: Maybe Integer
  , committedUse :: Maybe BufferUse
  }

newtype BufferState = BufferState (TVar State)

data Reservation = Reservation
  { reservationState :: TVar State
  , reservationToken :: Integer
  , reservationPreviousUse :: Maybe BufferUse
  }

newBufferState :: IO BufferState
newBufferState = BufferState <$> newTVarIO (State False 0 Nothing Nothing)

beginBufferUse :: BufferState -> IO Reservation
beginBufferUse (BufferState stateVariable) = atomically $ do
  state <- readTVar stateVariable
  if bufferStateQuarantined state
    then throwSTM ResourceQuarantined
    else case activeReservation state of
      Just _ -> retry
      Nothing -> do
        let token = nextReservationToken state
            reservation = Reservation stateVariable token (committedUse state)
        writeTVar
          stateVariable
          state
            { nextReservationToken = token + 1
            , activeReservation = Just token
            }
        pure reservation

-- | Permanently prevents new use and wakes reservations waiting for the slot.
quarantineBufferState :: BufferState -> IO ()
quarantineBufferState (BufferState stateVariable) = atomically $ do
  state <- readTVar stateVariable
  writeTVar stateVariable state{bufferStateQuarantined = True, activeReservation = Nothing}

quarantineBufferUse :: Reservation -> IO ()
quarantineBufferUse = quarantineBufferState . BufferState . reservationState

{- | Returns 'True' only when this reservation still owns the state slot.
A stale commit is ignored, so an old asynchronous continuation cannot
overwrite a newer use.
-}
commitBufferUse :: Reservation -> BufferUse -> IO Bool
commitBufferUse reservation use =
  -- A submitted GPU use must become visible even if its caller is cancelled.
  -- The state critical section is finite and never performs external IO.
  uninterruptibleMask_ $
    atomically $ do
      state <- readTVar (reservationState reservation)
      if not (bufferStateQuarantined state) && activeReservation state == Just (reservationToken reservation)
        then do
          writeTVar (reservationState reservation) state{activeReservation = Nothing, committedUse = Just use}
          pure True
        else pure False

-- | Returns 'True' only when this reservation was active and got cancelled.
cancelBufferUse :: Reservation -> IO Bool
cancelBufferUse reservation =
  atomically $ do
    state <- readTVar (reservationState reservation)
    if not (bufferStateQuarantined state) && activeReservation state == Just (reservationToken reservation)
      then do
        writeTVar (reservationState reservation) state{activeReservation = Nothing}
        pure True
      else pure False

lastBufferUse :: BufferState -> IO (Maybe BufferUse)
lastBufferUse (BufferState stateVariable) = atomically $ do
  state <- readTVar stateVariable
  if bufferStateQuarantined state
    then throwSTM ResourceQuarantined
    else pure (committedUse state)
