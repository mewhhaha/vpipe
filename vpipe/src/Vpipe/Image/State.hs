{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Transactional, per-subresource image layout and synchronization tracking.
module Vpipe.Image.State (
  ImageState,
  ImageSubresource (..),
  ImageCompletion (..),
  ImageUse (..),
  ImageUsePending (..),
  ImageSubresourceOutOfBounds (..),
  ImageReservation,
  newImageState,
  beginImageUse,
  quarantineImageState,
  quarantineImageUse,
  reservationPreviousUses,
  commitImageUse,
  cancelImageUse,
  lastImageUse,
  allImageUses,
  imageSubresourceLayout,
) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVar, retry, throwSTM, writeTVar)
import Control.Exception (Exception, uninterruptibleMask_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Word (Word32, Word64)
import Vulkan.Core10.Enums.ImageLayout qualified as Layout
import Vulkan.Core13.Enums.AccessFlags2 (AccessFlags2)
import Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)

import Vpipe.Context.Queue.Internal (Queue)
import Vpipe.Error (VpipeError (ResourceQuarantined))
import Vpipe.Image.Types (ImageSubresource (..), ImageSubresourceOutOfBounds (..))

data ImageCompletion = ImageCompletion
  { imageCompletionQueue :: Queue
  , imageCompletionQueueFamily :: Word32
  , imageCompletionTimeline :: Word64
  }

instance Eq ImageCompletion where
  left == right =
    imageCompletionQueueFamily left == imageCompletionQueueFamily right
      && imageCompletionTimeline left == imageCompletionTimeline right

instance Show ImageCompletion where
  showsPrec precedence completion =
    showParen (precedence > 10) $
      showString "ImageCompletion "
        . shows (imageCompletionQueueFamily completion)
        . showChar ' '
        . shows (imageCompletionTimeline completion)

-- | The state needed to construct the source side of a synchronization2 barrier.
data ImageUse = ImageUse
  { imageUseLayout :: Layout.ImageLayout
  , imageUseStage :: PipelineStageFlags2
  , imageUseAccess :: AccessFlags2
  , imageUseCompletion :: Maybe ImageCompletion
  }
  deriving stock (Eq, Show)

data ImageUsePending = ImageUsePending
  deriving stock (Eq, Show)

instance Exception ImageUsePending

{-# DEPRECATED ImageUsePending "Reservations now wait for active subresources to finish; this exception is no longer thrown." #-}

data SubresourceState = SubresourceState
  { activeReservation :: Maybe Integer
  , committedUse :: Maybe ImageUse
  }

data State = State
  { imageStateQuarantined :: Bool
  , nextReservationToken :: Integer
  , imageMipLevels :: Word32
  , imageArrayLayers :: Word32
  , subresourceStates :: Map ImageSubresource SubresourceState
  }

newtype ImageState = ImageState (TVar State)

data ImageReservation = ImageReservation
  { reservationState :: TVar State
  , reservationToken :: Integer
  , reservationSubresources :: [ImageSubresource]
  , reservationPreviousUses :: [(ImageSubresource, Maybe ImageUse)]
  }

-- | Creates a tracker with every subresource initially in Vulkan's undefined layout.
newImageState :: Word32 -> Word32 -> IO ImageState
newImageState mipLevels arrayLayers =
  ImageState <$> newTVarIO (State False 0 mipLevels arrayLayers Map.empty)

beginImageUse :: ImageState -> [ImageSubresource] -> IO ImageReservation
beginImageUse (ImageState stateVariable) requested = atomically $ do
  state <- readTVar stateVariable
  if imageStateQuarantined state
    then throwSTM ResourceQuarantined
    else do
      mapM_ (checkBounds state) requested
      let subresources = uniqueSubresources requested
      if any (isReserved state) subresources
        then retry
        else do
          let token = nextReservationToken state
              previousUses = fmap (\subresource -> (subresource, committedFor state subresource)) subresources
              reserve subresource = (stateFor state subresource){activeReservation = Just token}
              reservation = ImageReservation stateVariable token subresources previousUses
              updated = foldr (\subresource -> Map.insert subresource (reserve subresource)) (subresourceStates state) subresources
          writeTVar stateVariable state{nextReservationToken = token + 1, subresourceStates = updated}
          pure reservation

-- | Permanently prevents new use and wakes overlapping reservations.
quarantineImageState :: ImageState -> IO ()
quarantineImageState (ImageState stateVariable) = atomically $ do
  state <- readTVar stateVariable
  let clearReservation subresource = subresource{activeReservation = Nothing}
  writeTVar
    stateVariable
    state
      { imageStateQuarantined = True
      , subresourceStates = fmap clearReservation (subresourceStates state)
      }

quarantineImageUse :: ImageReservation -> IO ()
quarantineImageUse = quarantineImageState . ImageState . reservationState

-- | Commits all reserved subresources atomically, or ignores a stale reservation.
commitImageUse :: ImageReservation -> ImageUse -> IO Bool
commitImageUse reservation use =
  uninterruptibleMask_ $
    atomically $ do
      state <- readTVar (reservationState reservation)
      if not (imageStateQuarantined state) && all (ownedByReservation state reservation) (reservationSubresources reservation)
        then do
          let commit subresource = (stateFor state subresource){activeReservation = Nothing, committedUse = Just use}
              updated = foldr (\subresource -> Map.insert subresource (commit subresource)) (subresourceStates state) (reservationSubresources reservation)
          writeTVar (reservationState reservation) state{subresourceStates = updated}
          pure True
        else pure False

-- | Cancels all still-owned reservations. Stale or partially stale requests are ignored.
cancelImageUse :: ImageReservation -> IO Bool
cancelImageUse reservation =
  atomically $ do
    state <- readTVar (reservationState reservation)
    if not (imageStateQuarantined state) && all (ownedByReservation state reservation) (reservationSubresources reservation)
      then do
        let release subresource = (stateFor state subresource){activeReservation = Nothing}
            updated = foldr (\subresource -> Map.insert subresource (release subresource)) (subresourceStates state) (reservationSubresources reservation)
        writeTVar (reservationState reservation) state{subresourceStates = updated}
        pure True
      else pure False

lastImageUse :: ImageState -> ImageSubresource -> IO (Maybe ImageUse)
lastImageUse (ImageState stateVariable) subresource = atomically $ do
  state <- readTVar stateVariable
  if imageStateQuarantined state
    then throwSTM ResourceQuarantined
    else do
      checkBounds state subresource
      pure (committedFor state subresource)

allImageUses :: ImageState -> IO [ImageUse]
allImageUses (ImageState stateVariable) = atomically $ do
  state <- readTVar stateVariable
  if imageStateQuarantined state
    then throwSTM ResourceQuarantined
    else pure [use | subresource <- Map.elems (subresourceStates state), Just use <- [committedUse subresource]]

-- | Returns the current layout; untouched subresources begin at @UNDEFINED@.
imageSubresourceLayout :: ImageState -> ImageSubresource -> IO Layout.ImageLayout
imageSubresourceLayout state subresource = do
  previousUse <- lastImageUse state subresource
  pure $ maybe Layout.IMAGE_LAYOUT_UNDEFINED imageUseLayout previousUse

stateFor :: State -> ImageSubresource -> SubresourceState
stateFor state subresource = Map.findWithDefault (SubresourceState Nothing Nothing) subresource (subresourceStates state)

committedFor :: State -> ImageSubresource -> Maybe ImageUse
committedFor state = committedUse . stateFor state

isReserved :: State -> ImageSubresource -> Bool
isReserved state = isJust . activeReservation . stateFor state

ownedByReservation :: State -> ImageReservation -> ImageSubresource -> Bool
ownedByReservation state reservation subresource = activeReservation (stateFor state subresource) == Just (reservationToken reservation)

checkBounds :: State -> ImageSubresource -> STM ()
checkBounds state subresource
  | imageMipLevel subresource < imageMipLevels state && imageArrayLayer subresource < imageArrayLayers state = pure ()
  | otherwise = throwSTM (ImageSubresourceOutOfBounds subresource (imageMipLevels state) (imageArrayLayers state))

uniqueSubresources :: [ImageSubresource] -> [ImageSubresource]
uniqueSubresources = Map.keys . Map.fromList . fmap (,())
