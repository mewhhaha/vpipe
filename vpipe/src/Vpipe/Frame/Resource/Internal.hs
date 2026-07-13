{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Ordered frame commands and transactional resource planning.
module Vpipe.Frame.Resource.Internal (
  FrameCommand,
  FrameBufferUse (..),
  FrameImageUse (..),
  BufferBindingMetadata (..),
  ImageBindingMetadata (..),
  QueueDependency (..),
  FrameResourcePlan,
  FrameResourceError (..),
  FrameBufferSource (..),
  FrameImageSource (..),
  FrameTransition (..),
  newFrameCommand,
  frameCommandRuntimeHandles,
  frameCommandRenderedTargets,
  planFrameResources,
  framePlanRuntimeHandles,
  framePlanDependencies,
  framePlanTransitions,
  framePlanRenderedTargets,
  recordFrameCommands,
  cancelFrameResourcePlan,
  quarantineFrameResourcePlan,
  commitFrameResourcePlan,
  commitFrameResourcePlanWithFamily,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, newMVar, withMVar)
import Control.Exception (Exception, mask, onException, throwIO, uninterruptibleMask_)
import Control.Monad (foldM, unless, void, when)
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.Vector qualified as Vector
import Data.Word (Word32, Word64)
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.Enums.ImageLayout qualified as Layout
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Core10.ImageView qualified as ImageView
import Vulkan.Core13.Enums.AccessFlags2 qualified as Access2
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 qualified as Sync2
import Vulkan.Zero (zero)

import Vpipe.Buffer.State qualified as BufferState
import Vpipe.Context.Queue.Internal (Queue, QueueDependency (..), queueFamilyIndex)
import Vpipe.Image.State qualified as ImageState
import Vpipe.Pipeline.Internal (RuntimeHandle)
import Vpipe.Pipeline.Resource.Internal (BufferBindingMetadata (..), ImageBindingMetadata (..))
import Vpipe.Pipeline.Resource.Internal qualified as Resource

data FrameBufferUse = FrameBufferUse
  { frameBufferHandle :: RuntimeHandle
  , frameBufferMetadata :: Resource.BufferBindingMetadata
  , frameBufferStage :: Stage2.PipelineStageFlags2
  , frameBufferAccess :: Access2.AccessFlags2
  , frameBufferByteSize :: Word64
  }

data FrameImageUse = FrameImageUse
  { frameImageHandle :: RuntimeHandle
  , frameImageMetadata :: Resource.ImageBindingMetadata
  , frameImageSubresources :: [ImageState.ImageSubresource]
  , frameImageStage :: Stage2.PipelineStageFlags2
  , frameImageAccess :: Access2.AccessFlags2
  , frameImageLayout :: Layout.ImageLayout
  }

data FrameCommand = FrameCommand
  { commandRuntimeHandles :: [RuntimeHandle]
  , commandBufferUses :: [FrameBufferUse]
  , commandImageUses :: [FrameImageUse]
  , commandRenderedTargets :: [RuntimeHandle]
  , commandRecorder :: Handles.CommandBuffer -> [RuntimeHandle] -> IO ()
  }

data FrameResourceError
  = FrameBufferAliasConflict RuntimeHandle
  | FrameImageAliasConflict RuntimeHandle
  | FrameResourcePlanInactive
  | FrameResourceCommitStale
  deriving stock (Eq, Show)

instance Exception FrameResourceError

data FrameBufferSource
  = FrameBufferExternal (Maybe BufferState.BufferUse)
  | FrameBufferIntra Int FrameBufferUse

data FrameImageSource
  = FrameImageExternal (Maybe ImageState.ImageUse)
  | FrameImageIntra Int FrameImageUse

data FrameTransition
  = FrameBufferTransition
      { frameTransitionCommandIndex :: Int
      , frameTransitionBufferSource :: FrameBufferSource
      , frameTransitionBufferDestination :: FrameBufferUse
      }
  | FrameImageTransition
      { frameTransitionCommandIndex :: Int
      , frameTransitionImageSubresource :: ImageState.ImageSubresource
      , frameTransitionImageSource :: FrameImageSource
      , frameTransitionImageDestination :: FrameImageUse
      }

data IndexedBufferUse = IndexedBufferUse Int FrameBufferUse
data IndexedImageUse = IndexedImageUse Int FrameImageUse

data BufferGroup = BufferGroup
  { bufferGroupHandle :: RuntimeHandle
  , bufferGroupMetadata :: Resource.BufferBindingMetadata
  , bufferGroupByteSize :: Word64
  , bufferGroupUses :: [IndexedBufferUse]
  }

data ImageGroup = ImageGroup
  { imageGroupHandle :: RuntimeHandle
  , imageGroupMetadata :: Resource.ImageBindingMetadata
  , imageGroupSubresources :: [ImageState.ImageSubresource]
  , imageGroupUses :: [IndexedImageUse]
  }

data ReservedBuffer = ReservedBuffer BufferGroup BufferState.Reservation
data ReservedImage = ReservedImage ImageGroup ImageState.ImageReservation

data PlanStatus = PlanActive | PlanCancelled | PlanQuarantined | PlanCommitted

data FrameResourcePlan = FrameResourcePlan
  { plannedCommands :: [FrameCommand]
  , plannedBuffers :: [ReservedBuffer]
  , plannedImages :: [ReservedImage]
  , plannedTransitions :: [FrameTransition]
  , plannedDependencies :: [QueueDependency]
  , plannedRuntimeHandles :: [RuntimeHandle]
  , plannedRenderedTargets :: [RuntimeHandle]
  , plannedStatus :: MVar PlanStatus
  }

newFrameCommand :: [RuntimeHandle] -> [FrameBufferUse] -> [FrameImageUse] -> [RuntimeHandle] -> (Handles.CommandBuffer -> [RuntimeHandle] -> IO ()) -> IO FrameCommand
newFrameCommand handles buffers images renderedTargets recorder = do
  mergedBuffers <- foldM mergeCommandBufferUse [] buffers
  mergedImages <- foldM mergeCommandImageUse [] images
  let allHandles =
        uniqueHandles
          ( handles
              <> fmap frameBufferHandle mergedBuffers
              <> fmap frameImageHandle mergedImages
              <> renderedTargets
          )
  pure
    FrameCommand
      { commandRuntimeHandles = allHandles
      , commandBufferUses = mergedBuffers
      , commandImageUses = mergedImages
      , commandRenderedTargets = uniqueHandles renderedTargets
      , commandRecorder = recorder
      }

{- | Read-only target metadata used to validate a public @renderTo@ scope
before resource reservation begins.
-}
frameCommandRenderedTargets :: FrameCommand -> [RuntimeHandle]
frameCommandRenderedTargets = commandRenderedTargets

frameCommandRuntimeHandles :: FrameCommand -> [RuntimeHandle]
frameCommandRuntimeHandles = commandRuntimeHandles

planFrameResources :: [FrameCommand] -> IO FrameResourcePlan
planFrameResources commands = mask $ \_ -> do
  bufferGroups <- either throwIO pure (collectBufferGroups commands)
  imageGroups <- either throwIO pure (collectImageGroups commands)
  buffers <- reserveBuffers [] (sortOn bufferGroupOrder bufferGroups)
  images <- reserveImages buffers [] (sortOn imageGroupOrder imageGroups)
  let transitions =
        sortOn
          frameTransitionCommandIndex
          (concatMap bufferTransitions buffers <> concatMap imageTransitions images)
      dependencies = concatMap bufferInitialDependencies buffers <> concatMap imageInitialDependencies images
      handles = uniqueHandles (concatMap commandRuntimeHandles commands)
      renderedTargets = uniqueHandles (concatMap commandRenderedTargets commands)
  status <- newMVar PlanActive
  pure
    FrameResourcePlan
      { plannedCommands = commands
      , plannedBuffers = buffers
      , plannedImages = images
      , plannedTransitions = transitions
      , plannedDependencies = dependencies
      , plannedRuntimeHandles = handles
      , plannedRenderedTargets = renderedTargets
      , plannedStatus = status
      }
 where
  reserveBuffers reserved groups = case groups of
    [] -> pure (reverse reserved)
    group : rest -> do
      reservation <-
        BufferState.beginBufferUse (Resource.bufferBindingState (bufferGroupMetadata group))
          `onException` traverse_ cancelReservedBuffer reserved
      reserveBuffers (ReservedBuffer group reservation : reserved) rest
  reserveImages buffers reserved groups = case groups of
    [] -> pure (reverse reserved)
    group : rest -> do
      reservation <-
        ImageState.beginImageUse
          (Resource.imageBindingState (imageGroupMetadata group))
          (imageGroupSubresources group)
          `onException` do
            traverse_ cancelReservedImage reserved
            traverse_ cancelReservedBuffer (reverse buffers)
      reserveImages buffers (ReservedImage group reservation : reserved) rest

  bufferGroupOrder group =
    let Handles.Buffer word = Resource.bufferBindingRawHandle (bufferGroupMetadata group)
     in word

  imageGroupOrder group =
    let Handles.Image word = Resource.imageBindingRawHandle (imageGroupMetadata group)
     in word

framePlanRuntimeHandles :: FrameResourcePlan -> [RuntimeHandle]
framePlanRuntimeHandles = plannedRuntimeHandles

framePlanDependencies :: FrameResourcePlan -> [QueueDependency]
framePlanDependencies = plannedDependencies

framePlanTransitions :: FrameResourcePlan -> [FrameTransition]
framePlanTransitions = plannedTransitions

framePlanRenderedTargets :: FrameResourcePlan -> [RuntimeHandle]
framePlanRenderedTargets = plannedRenderedTargets

recordFrameCommands :: Queue -> Handles.CommandBuffer -> FrameResourcePlan -> IO ()
recordFrameCommands queue commandBuffer plan =
  withMVar (plannedStatus plan) $ \case
    PlanActive -> go 0 [] (plannedCommands plan)
    _ -> throwIO FrameResourcePlanInactive
 where
  go _ _ [] = pure ()
  go commandIndex renderedTargets (command : rest) = do
    recordTransitions queue commandBuffer commandIndex (plannedTransitions plan)
    commandRecorder command commandBuffer renderedTargets
    let renderedTargets' = uniqueHandles (renderedTargets <> commandRenderedTargets command)
    go (commandIndex + 1) renderedTargets' rest

cancelFrameResourcePlan :: FrameResourcePlan -> IO ()
cancelFrameResourcePlan plan = uninterruptibleMask_ $ do
  shouldCancel <-
    modifyMVarMasked (plannedStatus plan) $ \case
      PlanActive -> pure (PlanCancelled, True)
      PlanCancelled -> pure (PlanCancelled, False)
      PlanQuarantined -> pure (PlanQuarantined, False)
      PlanCommitted -> pure (PlanCommitted, False)
  when shouldCancel $ do
    traverse_ cancelReservedImage (reverse (plannedImages plan))
    traverse_ cancelReservedBuffer (reverse (plannedBuffers plan))

quarantineFrameResourcePlan :: FrameResourcePlan -> IO ()
quarantineFrameResourcePlan plan = uninterruptibleMask_ $ do
  shouldQuarantine <-
    modifyMVarMasked (plannedStatus plan) $ \case
      PlanActive -> pure (PlanQuarantined, True)
      PlanCancelled -> pure (PlanCancelled, False)
      PlanQuarantined -> pure (PlanQuarantined, False)
      PlanCommitted -> pure (PlanCommitted, False)
  when shouldQuarantine $ do
    traverse_ quarantineReservedImage (plannedImages plan)
    traverse_ quarantineReservedBuffer (plannedBuffers plan)

commitFrameResourcePlan :: Queue -> Word64 -> FrameResourcePlan -> IO ()
commitFrameResourcePlan queue = commitFrameResourcePlanWithFamily queue (queueFamilyIndex queue)

commitFrameResourcePlanWithFamily :: Queue -> Word32 -> Word64 -> FrameResourcePlan -> IO ()
commitFrameResourcePlanWithFamily queue family timeline plan = uninterruptibleMask_ $ do
  shouldCommit <-
    modifyMVarMasked (plannedStatus plan) $ \status -> case status of
      PlanActive -> pure (PlanCommitted, True)
      _ -> pure (status, False)
  unless shouldCommit (throwIO FrameResourcePlanInactive)
  bufferResults <- traverse (commitReservedBuffer queue family timeline) (plannedBuffers plan)
  imageResults <- traverse (commitReservedImage queue family timeline) (plannedImages plan)
  unless (and (bufferResults <> imageResults)) (throwIO FrameResourceCommitStale)

collectBufferGroups :: [FrameCommand] -> Either FrameResourceError [BufferGroup]
collectBufferGroups commands =
  foldM
    add
    []
    [ IndexedBufferUse commandIndex use
    | (commandIndex, command) <- zip [0 ..] commands
    , use <- commandBufferUses command
    ]
 where
  add groups indexed@(IndexedBufferUse _ requested) =
    case break ((== frameBufferHandle requested) . bufferGroupHandle) groups of
      (_, [])
        | any (sameRawBuffer requested . bufferGroupMetadata) groups ->
            Left (FrameBufferAliasConflict (frameBufferHandle requested))
        | otherwise ->
            Right
              ( groups
                  <> [ BufferGroup
                         (frameBufferHandle requested)
                         (frameBufferMetadata requested)
                         (frameBufferByteSize requested)
                         [indexed]
                     ]
              )
      (before, group : after)
        | compatibleBufferGroup group requested ->
            Right (before <> [group{bufferGroupUses = bufferGroupUses group <> [indexed]}] <> after)
        | otherwise -> Left (FrameBufferAliasConflict (frameBufferHandle requested))

collectImageGroups :: [FrameCommand] -> Either FrameResourceError [ImageGroup]
collectImageGroups commands =
  foldM
    add
    []
    [ IndexedImageUse commandIndex use
    | (commandIndex, command) <- zip [0 ..] commands
    , use <- commandImageUses command
    ]
 where
  add groups indexed@(IndexedImageUse _ requested) =
    case break ((== frameImageHandle requested) . imageGroupHandle) groups of
      (_, [])
        | any (sameRawImage requested . imageGroupMetadata) groups ->
            Left (FrameImageAliasConflict (frameImageHandle requested))
        | otherwise ->
            Right
              ( groups
                  <> [ ImageGroup
                         (frameImageHandle requested)
                         (frameImageMetadata requested)
                         (uniqueSubresources (frameImageSubresources requested))
                         [indexed]
                     ]
              )
      (before, group : after)
        | compatibleImageGroup group requested ->
            Right (before <> [group{imageGroupUses = imageGroupUses group <> [indexed]}] <> after)
        | otherwise -> Left (FrameImageAliasConflict (frameImageHandle requested))

mergeCommandBufferUse :: [FrameBufferUse] -> FrameBufferUse -> IO [FrameBufferUse]
mergeCommandBufferUse uses requested =
  case break ((== frameBufferHandle requested) . frameBufferHandle) uses of
    (_, [])
      | any (sameRawBuffer requested . frameBufferMetadata) uses ->
          throwIO (FrameBufferAliasConflict (frameBufferHandle requested))
      | otherwise -> pure (uses <> [requested])
    (before, existing : after)
      | compatibleBufferUses existing requested ->
          pure
            ( before
                <> [ existing
                       { frameBufferStage = frameBufferStage existing .|. frameBufferStage requested
                       , frameBufferAccess = frameBufferAccess existing .|. frameBufferAccess requested
                       }
                   ]
                <> after
            )
      | otherwise -> throwIO (FrameBufferAliasConflict (frameBufferHandle requested))

mergeCommandImageUse :: [FrameImageUse] -> FrameImageUse -> IO [FrameImageUse]
mergeCommandImageUse uses requested =
  case break ((== frameImageHandle requested) . frameImageHandle) uses of
    (_, [])
      | any (sameRawImage requested . frameImageMetadata) uses ->
          throwIO (FrameImageAliasConflict (frameImageHandle requested))
      | otherwise -> pure (uses <> [requested{frameImageSubresources = uniqueSubresources (frameImageSubresources requested)}])
    (before, existing : after)
      | compatibleImageUses existing requested && frameImageLayout existing == frameImageLayout requested ->
          pure
            ( before
                <> [ existing
                       { frameImageStage = frameImageStage existing .|. frameImageStage requested
                       , frameImageAccess = frameImageAccess existing .|. frameImageAccess requested
                       }
                   ]
                <> after
            )
      | otherwise -> throwIO (FrameImageAliasConflict (frameImageHandle requested))

bufferTransitions :: ReservedBuffer -> [FrameTransition]
bufferTransitions (ReservedBuffer group reservation) = case bufferGroupUses group of
  [] -> []
  first : rest ->
    firstTransition first
      : zipWith intraTransition (first : rest) rest
 where
  firstTransition (IndexedBufferUse commandIndex use) =
    FrameBufferTransition commandIndex (FrameBufferExternal (BufferState.reservationPreviousUse reservation)) use
  intraTransition (IndexedBufferUse previousIndex previous) (IndexedBufferUse commandIndex use) =
    FrameBufferTransition commandIndex (FrameBufferIntra previousIndex previous) use

imageTransitions :: ReservedImage -> [FrameTransition]
imageTransitions (ReservedImage group reservation) =
  concatMap transitionsFor (ImageState.reservationPreviousUses reservation)
 where
  transitionsFor (subresource, previousUse) = case imageGroupUses group of
    [] -> []
    first : rest ->
      firstTransition subresource previousUse first
        : zipWith (intraTransition subresource) (first : rest) rest
  firstTransition subresource previousUse (IndexedImageUse commandIndex use) =
    FrameImageTransition commandIndex subresource (FrameImageExternal previousUse) use
  intraTransition subresource (IndexedImageUse previousIndex previous) (IndexedImageUse commandIndex use) =
    FrameImageTransition commandIndex subresource (FrameImageIntra previousIndex previous) use

bufferInitialDependencies :: ReservedBuffer -> [QueueDependency]
bufferInitialDependencies (ReservedBuffer group reservation) = case (bufferGroupUses group, BufferState.reservationPreviousUse reservation) of
  (IndexedBufferUse _ first : _, Just previous) ->
    [ QueueDependency
        (BufferState.bufferCompletionQueue completion)
        (BufferState.bufferCompletionTimeline completion)
        (frameBufferStage first)
    | Just completion <- [BufferState.bufferUseCompletion previous]
    ]
  _ -> []

imageInitialDependencies :: ReservedImage -> [QueueDependency]
imageInitialDependencies (ReservedImage group reservation) = case imageGroupUses group of
  IndexedImageUse _ first : _ ->
    [ QueueDependency
        (ImageState.imageCompletionQueue completion)
        (ImageState.imageCompletionTimeline completion)
        (frameImageStage first)
    | (_, Just previous) <- ImageState.reservationPreviousUses reservation
    , Just completion <- [ImageState.imageUseCompletion previous]
    ]
  [] -> []

recordTransitions :: Queue -> Handles.CommandBuffer -> Int -> [FrameTransition] -> IO ()
recordTransitions queue commandBuffer commandIndex transitions = do
  let current = filter ((== commandIndex) . frameTransitionCommandIndex) transitions
      buffers = mapMaybe (bufferBarrier queue) current
      images = mapMaybe (imageBarrier queue) current
  unless (null buffers && null images) $
    Sync2.cmdPipelineBarrier2
      commandBuffer
      ( (zero :: Sync2.DependencyInfo)
          { Sync2.bufferMemoryBarriers = Vector.fromList buffers
          , Sync2.imageMemoryBarriers = Vector.fromList images
          }
      )

bufferBarrier :: Queue -> FrameTransition -> Maybe (Chain.SomeStruct Sync2.BufferMemoryBarrier2)
bufferBarrier queue transition = case transition of
  FrameBufferTransition _ source destination ->
    Just . Chain.SomeStruct $
      (zero :: Sync2.BufferMemoryBarrier2 '[])
        { Sync2.srcStageMask = bufferSourceStage queue source
        , Sync2.srcAccessMask = bufferSourceAccess queue source
        , Sync2.dstStageMask = frameBufferStage destination
        , Sync2.dstAccessMask = frameBufferAccess destination
        , Sync2.srcQueueFamilyIndex = maxBound
        , Sync2.dstQueueFamilyIndex = maxBound
        , Sync2.buffer = Resource.bufferBindingRawHandle (frameBufferMetadata destination)
        , Sync2.offset = Resource.bufferBindingByteOffset (frameBufferMetadata destination)
        , Sync2.size = frameBufferByteSize destination
        }
  _ -> Nothing

imageBarrier :: Queue -> FrameTransition -> Maybe (Chain.SomeStruct Sync2.ImageMemoryBarrier2)
imageBarrier queue transition = case transition of
  FrameImageTransition _ subresource source destination ->
    Just . Chain.SomeStruct $
      (zero :: Sync2.ImageMemoryBarrier2 '[])
        { Sync2.srcStageMask = imageSourceStage queue source
        , Sync2.srcAccessMask = imageSourceAccess queue source
        , Sync2.dstStageMask = frameImageStage destination
        , Sync2.dstAccessMask = frameImageAccess destination
        , Sync2.oldLayout = imageSourceLayout source
        , Sync2.newLayout = frameImageLayout destination
        , Sync2.srcQueueFamilyIndex = maxBound
        , Sync2.dstQueueFamilyIndex = maxBound
        , Sync2.image = Resource.imageBindingRawHandle metadata
        , Sync2.subresourceRange =
            ImageView.ImageSubresourceRange
              (Resource.imageBindingAspect metadata)
              (ImageState.imageMipLevel subresource)
              1
              (ImageState.imageArrayLayer subresource)
              1
        }
   where
    metadata = frameImageMetadata destination
  _ -> Nothing

bufferSourceStage :: Queue -> FrameBufferSource -> Stage2.PipelineStageFlags2
bufferSourceStage queue source = case source of
  FrameBufferExternal Nothing -> zero
  FrameBufferExternal (Just previous)
    | sameBufferQueueFamily queue previous -> BufferState.bufferUseStage previous
    | otherwise -> zero
  FrameBufferIntra _ previous -> frameBufferStage previous

bufferSourceAccess :: Queue -> FrameBufferSource -> Access2.AccessFlags2
bufferSourceAccess queue source = case source of
  FrameBufferExternal Nothing -> zero
  FrameBufferExternal (Just previous)
    | sameBufferQueueFamily queue previous -> BufferState.bufferUseAccess previous
    | otherwise -> zero
  FrameBufferIntra _ previous -> frameBufferAccess previous

imageSourceStage :: Queue -> FrameImageSource -> Stage2.PipelineStageFlags2
imageSourceStage queue source = case source of
  FrameImageExternal Nothing -> Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT
  FrameImageExternal (Just previous)
    | sameImageQueueFamily queue previous -> ImageState.imageUseStage previous
    | otherwise -> zero
  FrameImageIntra _ previous -> frameImageStage previous

imageSourceAccess :: Queue -> FrameImageSource -> Access2.AccessFlags2
imageSourceAccess queue source = case source of
  FrameImageExternal Nothing -> zero
  FrameImageExternal (Just previous)
    | sameImageQueueFamily queue previous -> ImageState.imageUseAccess previous
    | otherwise -> zero
  FrameImageIntra _ previous -> frameImageAccess previous

imageSourceLayout :: FrameImageSource -> Layout.ImageLayout
imageSourceLayout source = case source of
  FrameImageExternal Nothing -> Layout.IMAGE_LAYOUT_UNDEFINED
  FrameImageExternal (Just previous) -> ImageState.imageUseLayout previous
  FrameImageIntra _ previous -> frameImageLayout previous

sameImageQueueFamily :: Queue -> ImageState.ImageUse -> Bool
sameImageQueueFamily queue use =
  maybe
    True
    ((== queueFamilyIndex queue) . ImageState.imageCompletionQueueFamily)
    (ImageState.imageUseCompletion use)

sameBufferQueueFamily :: Queue -> BufferState.BufferUse -> Bool
sameBufferQueueFamily queue use =
  maybe
    True
    ((== queueFamilyIndex queue) . BufferState.bufferCompletionQueueFamily)
    (BufferState.bufferUseCompletion use)

commitReservedBuffer :: Queue -> Word32 -> Word64 -> ReservedBuffer -> IO Bool
commitReservedBuffer queue family timeline (ReservedBuffer group reservation) = case reverse (bufferGroupUses group) of
  IndexedBufferUse _ final : _ ->
    BufferState.commitBufferUse
      reservation
      BufferState.BufferUse
        { BufferState.bufferUseStage = frameBufferStage final
        , BufferState.bufferUseAccess = frameBufferAccess final
        , BufferState.bufferUseCompletion =
            Just
              BufferState.BufferCompletion
                { BufferState.bufferCompletionQueue = queue
                , BufferState.bufferCompletionQueueFamily = family
                , BufferState.bufferCompletionTimeline = timeline
                }
        }
  [] -> pure False

commitReservedImage :: Queue -> Word32 -> Word64 -> ReservedImage -> IO Bool
commitReservedImage queue family timeline (ReservedImage group reservation) = case reverse (imageGroupUses group) of
  IndexedImageUse _ final : _ ->
    ImageState.commitImageUse
      reservation
      ImageState.ImageUse
        { ImageState.imageUseLayout = frameImageLayout final
        , ImageState.imageUseStage = frameImageStage final
        , ImageState.imageUseAccess = frameImageAccess final
        , ImageState.imageUseCompletion =
            Just
              ImageState.ImageCompletion
                { ImageState.imageCompletionQueue = queue
                , ImageState.imageCompletionQueueFamily = family
                , ImageState.imageCompletionTimeline = timeline
                }
        }
  [] -> pure False

cancelReservedBuffer :: ReservedBuffer -> IO ()
cancelReservedBuffer (ReservedBuffer _ reservation) = void (BufferState.cancelBufferUse reservation)

cancelReservedImage :: ReservedImage -> IO ()
cancelReservedImage (ReservedImage _ reservation) = void (ImageState.cancelImageUse reservation)

quarantineReservedBuffer :: ReservedBuffer -> IO ()
quarantineReservedBuffer (ReservedBuffer group _) =
  BufferState.quarantineBufferState (Resource.bufferBindingState (bufferGroupMetadata group))

quarantineReservedImage :: ReservedImage -> IO ()
quarantineReservedImage (ReservedImage group _) =
  ImageState.quarantineImageState (Resource.imageBindingState (imageGroupMetadata group))

compatibleBufferGroup :: BufferGroup -> FrameBufferUse -> Bool
compatibleBufferGroup group requested =
  bufferGroupHandle group == frameBufferHandle requested
    && sameBufferMetadata (bufferGroupMetadata group) (frameBufferMetadata requested)
    && bufferGroupByteSize group == frameBufferByteSize requested

compatibleBufferUses :: FrameBufferUse -> FrameBufferUse -> Bool
compatibleBufferUses left right =
  frameBufferHandle left == frameBufferHandle right
    && sameBufferMetadata (frameBufferMetadata left) (frameBufferMetadata right)
    && frameBufferByteSize left == frameBufferByteSize right

sameBufferMetadata :: Resource.BufferBindingMetadata -> Resource.BufferBindingMetadata -> Bool
sameBufferMetadata left right =
  Resource.bufferBindingRawHandle left == Resource.bufferBindingRawHandle right
    && Resource.bufferBindingElementCount left == Resource.bufferBindingElementCount right
    && Resource.bufferBindingStride left == Resource.bufferBindingStride right
    && Resource.bufferBindingByteOffset left == Resource.bufferBindingByteOffset right
    && Resource.bufferBindingUsage left == Resource.bufferBindingUsage right

sameRawBuffer :: FrameBufferUse -> Resource.BufferBindingMetadata -> Bool
sameRawBuffer requested metadata =
  Resource.bufferBindingRawHandle (frameBufferMetadata requested) == Resource.bufferBindingRawHandle metadata

compatibleImageGroup :: ImageGroup -> FrameImageUse -> Bool
compatibleImageGroup group requested =
  imageGroupHandle group == frameImageHandle requested
    && sameImageMetadata (imageGroupMetadata group) (frameImageMetadata requested)
    && imageGroupSubresources group == uniqueSubresources (frameImageSubresources requested)

compatibleImageUses :: FrameImageUse -> FrameImageUse -> Bool
compatibleImageUses left right =
  frameImageHandle left == frameImageHandle right
    && sameImageMetadata (frameImageMetadata left) (frameImageMetadata right)
    && uniqueSubresources (frameImageSubresources left) == uniqueSubresources (frameImageSubresources right)

sameImageMetadata :: Resource.ImageBindingMetadata -> Resource.ImageBindingMetadata -> Bool
sameImageMetadata left right =
  Resource.imageBindingRawHandle left == Resource.imageBindingRawHandle right
    && Resource.imageBindingRawView left == Resource.imageBindingRawView right
    && Resource.imageBindingExtent left == Resource.imageBindingExtent right
    && Resource.imageBindingFormat left == Resource.imageBindingFormat right
    && Resource.imageBindingAspect left == Resource.imageBindingAspect right
    && Resource.imageBindingSamples left == Resource.imageBindingSamples right
    && Resource.imageBindingMipLevel left == Resource.imageBindingMipLevel right
    && Resource.imageBindingArrayLayer left == Resource.imageBindingArrayLayer right
    && Resource.imageBindingMipLevels left == Resource.imageBindingMipLevels right
    && Resource.imageBindingArrayLayers left == Resource.imageBindingArrayLayers right
    && Resource.imageBindingUsage left == Resource.imageBindingUsage right

sameRawImage :: FrameImageUse -> Resource.ImageBindingMetadata -> Bool
sameRawImage requested metadata =
  Resource.imageBindingRawHandle (frameImageMetadata requested) == Resource.imageBindingRawHandle metadata

uniqueHandles :: [RuntimeHandle] -> [RuntimeHandle]
uniqueHandles = foldl add []
 where
  add handles handle
    | handle `elem` handles = handles
    | otherwise = handles <> [handle]

uniqueSubresources :: [ImageState.ImageSubresource] -> [ImageState.ImageSubresource]
uniqueSubresources = foldl add [] . sortOn identity
 where
  identity value = value
  add values value
    | value `elem` values = values
    | otherwise = values <> [value]
