{-# LANGUAGE ScopedTypeVariables #-}

module Vpipe.FrameResourceTest (frameResourceTests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryReadMVar)
import Control.Exception (try)
import Data.Foldable (traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Unique (newUnique)
import Data.Word (Word32, Word64)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Vulkan.Core10.Enums.Format qualified as Format
import Vulkan.Core10.Enums.ImageAspectFlagBits qualified as Aspect
import Vulkan.Core10.Enums.ImageLayout qualified as Layout
import Vulkan.Core10.Enums.SampleCountFlagBits qualified as Samples
import Vulkan.Core10.FundamentalTypes qualified as Fundamental
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Core13.Enums.AccessFlags2 qualified as Access2
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Zero (zero)

import Vpipe.Buffer.State qualified as BufferState
import Vpipe.Compute.Frame.Internal (prepareComputeFrameCommandWith)
import Vpipe.Context.Queue (Queue)
import Vpipe.Error (VpipeError (ResourceQuarantined))
import Vpipe.Frame.Resource.Internal
import Vpipe.Image.State qualified as ImageState
import Vpipe.Pipeline.Internal (RuntimeHandle (..))
import Vpipe.Pipeline.Resource.Internal qualified as Resource
import Vpipe.Resource.Lifetime qualified as Lifetime

frameResourceTests :: TestTree
frameResourceTests =
  testGroup
    "frame resource planning"
    [ testCase "repeated handles reserve once and retain ordered buffer hazards" repeatedBufferCase
    , testCase "managed handle equality and ordering use owner kind and generation" managedHandleIdentityCase
    , testCase "the same managed buffer identity deduplicates frame uses" managedBufferDedupeCase
    , testCase "distinct buffer generations sharing one raw handle are rejected" managedBufferAliasCase
    , testCase "distinct image generations sharing one raw image are rejected" managedImageAliasCase
    , testCase "opposite buffer request orders complete without deadlock" oppositeBufferOrdersCase
    , testCase "image layouts transition from color output to sampling" imageTransitionCase
    , testCase "dependencies come only from the initial external use" initialDependencyCase
    , testCase "commit publishes each resource's final frame use" finalCommitCase
    , testCase "incompatible aliases are rejected before reservation" aliasRejectionCase
    , testCase "cancellation is idempotent" cancelCase
    , testCase "quarantine is terminal and prevents resource-state reuse" quarantineCase
    , testCase "rendered targets clear first and load later" renderedTargetCase
    , testCase "zero compute validation skips command construction" zeroComputeCase
    ]

managedHandleIdentityCase :: IO ()
managedHandleIdentityCase = do
  owner <- newUnique
  foreignOwner <- newUnique
  generation <- Lifetime.newResourceGeneration
  replacementGeneration <- Lifetime.newResourceGeneration
  let acquire = pure (pure ())
      managed = Resource.managedRuntimeHandle owner generation 41 acquire
      sameLogicalResource = Resource.managedRuntimeHandle owner generation 99 acquire
      replacement = Resource.managedRuntimeHandle owner replacementGeneration 41 acquire
      foreignResource = Resource.managedRuntimeHandle foreignOwner generation 41 acquire
      raw = RuntimeHandle 41
      handles = [managed, sameLogicalResource, replacement, foreignResource, raw]
  assertBool "the same logical resource must compare equal" (managed == sameLogicalResource)
  assertBool "a replacement generation must not compare equal" (managed /= replacement)
  assertBool "a foreign Context owner must not compare equal" (managed /= foreignResource)
  assertBool "a raw fixture must never equal a managed resource" (managed /= raw)
  assertBool "managed owner must be retained" (Resource.runtimeHandleOwner managed == Just owner)
  assertBool "managed generation must be retained" (Resource.runtimeHandleGeneration managed == Just generation)
  traverse_
    ( \(left, right) ->
        assertBool
          "compare returned EQ inconsistently with equality"
          ( case compare left right of
              EQ -> left == right
              _ -> left /= right
          )
    )
    [(left, right) | left <- handles, right <- handles]

managedBufferDedupeCase :: IO ()
managedBufferDedupeCase = do
  owner <- newUnique
  generation <- Lifetime.newResourceGeneration
  state <- BufferState.newBufferState
  let metadata = bufferMetadata state 0
      handle = Resource.managedBufferRuntimeHandle owner generation (pure (pure ())) metadata
      writeUse = bufferUse handle metadata Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT Access2.ACCESS_2_SHADER_WRITE_BIT
      readUse = bufferUse handle metadata Stage2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT Access2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT
  first <- command [writeUse] [] [] noRecord
  second <- command [readUse] [] [] noRecord
  plan <- planFrameResources [first, second]
  framePlanRuntimeHandles plan @?= [handle]
  length (bufferTransitions (framePlanTransitions plan)) @?= 2
  cancelFrameResourcePlan plan

managedBufferAliasCase :: IO ()
managedBufferAliasCase = do
  owner <- newUnique
  firstGeneration <- Lifetime.newResourceGeneration
  secondGeneration <- Lifetime.newResourceGeneration
  state <- BufferState.newBufferState
  let metadata = bufferMetadata state 0
      firstHandle = Resource.managedBufferRuntimeHandle owner firstGeneration (pure (pure ())) metadata
      secondHandle = Resource.managedBufferRuntimeHandle owner secondGeneration (pure (pure ())) metadata
  first <- command [bufferUse firstHandle metadata Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT Access2.ACCESS_2_SHADER_WRITE_BIT] [] [] noRecord
  second <- command [bufferUse secondHandle metadata Stage2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT Access2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT] [] [] noRecord
  result <- try (planFrameResources [first, second]) :: IO (Either FrameResourceError FrameResourcePlan)
  case result of
    Left (FrameBufferAliasConflict actual) -> actual @?= secondHandle
    Left error' -> assertFailure ("unexpected frame error: " <> show error')
    Right plan -> cancelFrameResourcePlan plan >> assertFailure "distinct buffer generations were silently deduplicated"

managedImageAliasCase :: IO ()
managedImageAliasCase = do
  owner <- newUnique
  firstGeneration <- Lifetime.newResourceGeneration
  secondGeneration <- Lifetime.newResourceGeneration
  state <- ImageState.newImageState 1 1
  let metadata = imageMetadata state
      firstHandle = Resource.managedImageRuntimeHandle owner firstGeneration (pure (pure ())) metadata
      secondHandle = Resource.managedImageRuntimeHandle owner secondGeneration (pure (pure ())) metadata
      firstUse = imageUse firstHandle metadata Stage2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT Access2.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT Layout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
      secondUse = imageUse secondHandle metadata Stage2.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT Access2.ACCESS_2_SHADER_SAMPLED_READ_BIT Layout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  first <- command [] [firstUse] [] noRecord
  second <- command [] [secondUse] [] noRecord
  result <- try (planFrameResources [first, second]) :: IO (Either FrameResourceError FrameResourcePlan)
  case result of
    Left (FrameImageAliasConflict actual) -> actual @?= secondHandle
    Left error' -> assertFailure ("unexpected frame error: " <> show error')
    Right plan -> cancelFrameResourcePlan plan >> assertFailure "distinct image generations were silently deduplicated"

repeatedBufferCase :: IO ()
repeatedBufferCase = do
  state <- BufferState.newBufferState
  let metadata = bufferMetadata state 0
      handle = RuntimeHandle 1
      computeWrite = bufferUse handle metadata Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT Access2.ACCESS_2_SHADER_WRITE_BIT
      vertexRead = bufferUse handle metadata Stage2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT Access2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT
  first <- command [computeWrite] [] [] noRecord
  second <- command [vertexRead] [] [] noRecord
  plan <- planFrameResources [first, second]
  case bufferTransitions (framePlanTransitions plan) of
    [ FrameBufferTransition 0 (FrameBufferExternal Nothing) firstUse
      , FrameBufferTransition 1 (FrameBufferIntra 0 previousUse) secondUse
      ] -> do
        frameBufferAccess firstUse @?= Access2.ACCESS_2_SHADER_WRITE_BIT
        frameBufferAccess previousUse @?= Access2.ACCESS_2_SHADER_WRITE_BIT
        frameBufferStage secondUse @?= Stage2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT
        frameBufferAccess secondUse @?= Access2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT
    transitions -> assertFailure ("unexpected transitions: " <> show (length transitions))
  cancelFrameResourcePlan plan

oppositeBufferOrdersCase :: IO ()
oppositeBufferOrdersCase = do
  leftState <- BufferState.newBufferState
  rightState <- BufferState.newBufferState
  blocker <- BufferState.beginBufferUse rightState
  let left = bufferUse (RuntimeHandle 9) (bufferMetadataWithHandle leftState 100 0) Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT Access2.ACCESS_2_SHADER_WRITE_BIT
      right = bufferUse (RuntimeHandle 10) (bufferMetadataWithHandle rightState 200 0) Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT Access2.ACCESS_2_SHADER_WRITE_BIT
  leftThenRight <- command [left, right] [] [] noRecord
  rightThenLeft <- command [right, left] [] [] noRecord
  firstResult <- newEmptyMVar
  secondResult <- newEmptyMVar
  _ <- forkIO (planFrameResources [leftThenRight] >>= putMVar firstResult)
  threadDelay 10000
  _ <- forkIO (planFrameResources [rightThenLeft] >>= putMVar secondResult)
  threadDelay 10000
  tryReadMVar secondResult >>= maybe (pure ()) (const (assertFailure "second plan unexpectedly completed while the blocker was active"))
  BufferState.cancelBufferUse blocker >>= (@?= True)
  firstPlan <- timeout 1000000 (takeMVar firstResult) >>= maybe (assertFailure "ordered first plan did not complete") pure
  cancelFrameResourcePlan firstPlan
  secondPlan <- timeout 1000000 (takeMVar secondResult) >>= maybe (assertFailure "opposite-order plan deadlocked") pure
  cancelFrameResourcePlan secondPlan

imageTransitionCase :: IO ()
imageTransitionCase = do
  state <- ImageState.newImageState 1 1
  let metadata = imageMetadata state
      handle = RuntimeHandle 2
      colorWrite = imageUse handle metadata Stage2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT Access2.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT Layout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
      sampledRead = imageUse handle metadata Stage2.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT Access2.ACCESS_2_SHADER_SAMPLED_READ_BIT Layout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  first <- command [] [colorWrite] [handle] noRecord
  second <- command [] [sampledRead] [] noRecord
  plan <- planFrameResources [first, second]
  case imageTransitions (framePlanTransitions plan) of
    [ FrameImageTransition 0 _ (FrameImageExternal Nothing) firstUse
      , FrameImageTransition 1 _ (FrameImageIntra 0 previousUse) secondUse
      ] -> do
        frameImageLayout firstUse @?= Layout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        frameImageLayout previousUse @?= Layout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
        frameImageLayout secondUse @?= Layout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    transitions -> assertFailure ("unexpected image transitions: " <> show (length transitions))
  cancelFrameResourcePlan plan

initialDependencyCase :: IO ()
initialDependencyCase = do
  state <- BufferState.newBufferState
  reservation <- BufferState.beginBufferUse state
  _ <-
    BufferState.commitBufferUse
      reservation
      BufferState.BufferUse
        { BufferState.bufferUseStage = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
        , BufferState.bufferUseAccess = Access2.ACCESS_2_TRANSFER_WRITE_BIT
        , BufferState.bufferUseCompletion =
            Just
              BufferState.BufferCompletion
                { BufferState.bufferCompletionQueue = fakeQueue
                , BufferState.bufferCompletionQueueFamily = 4
                , BufferState.bufferCompletionTimeline = 17
                }
        }
  let metadata = bufferMetadata state 0
      handle = RuntimeHandle 3
      firstUse = bufferUse handle metadata Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT Access2.ACCESS_2_SHADER_READ_BIT
      secondUse = bufferUse handle metadata Stage2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT Access2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT
  first <- command [firstUse] [] [] noRecord
  second <- command [secondUse] [] [] noRecord
  plan <- planFrameResources [first, second]
  case framePlanDependencies plan of
    [dependency] -> do
      dependencyTimeline dependency @?= 17
      dependencyDestinationStage dependency @?= Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT
    dependencies -> assertFailure ("expected one initial dependency, got " <> show (length dependencies))
  cancelFrameResourcePlan plan

finalCommitCase :: IO ()
finalCommitCase = do
  bufferState <- BufferState.newBufferState
  imageState <- ImageState.newImageState 1 1
  let buffer = bufferMetadata bufferState 0
      image = imageMetadata imageState
      bufferHandle = RuntimeHandle 4
      imageHandle = RuntimeHandle 5
      computeWrite = bufferUse bufferHandle buffer Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT Access2.ACCESS_2_SHADER_WRITE_BIT
      vertexRead = bufferUse bufferHandle buffer Stage2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT Access2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT
      colorWrite = imageUse imageHandle image Stage2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT Access2.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT Layout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
      sampledRead = imageUse imageHandle image Stage2.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT Access2.ACCESS_2_SHADER_SAMPLED_READ_BIT Layout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  first <- command [computeWrite] [colorWrite] [imageHandle] noRecord
  second <- command [vertexRead] [sampledRead] [] noRecord
  plan <- planFrameResources [first, second]
  commitFrameResourcePlanWithFamily fakeQueue 9 41 plan
  committedBuffer <- BufferState.lastBufferUse bufferState
  case committedBuffer of
    Just use -> do
      BufferState.bufferUseStage use @?= Stage2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT
      BufferState.bufferUseAccess use @?= Access2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT
      case BufferState.bufferUseCompletion use of
        Just completion -> do
          BufferState.bufferCompletionQueueFamily completion @?= 9
          BufferState.bufferCompletionTimeline completion @?= 41
        Nothing -> assertFailure "buffer final use has no completion"
    Nothing -> assertFailure "buffer final use was not committed"
  committedImage <- ImageState.lastImageUse imageState (ImageState.ImageSubresource 0 0)
  case committedImage of
    Just use -> do
      ImageState.imageUseLayout use @?= Layout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
      ImageState.imageUseStage use @?= Stage2.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT
      case ImageState.imageUseCompletion use of
        Just completion -> do
          ImageState.imageCompletionQueueFamily completion @?= 9
          ImageState.imageCompletionTimeline completion @?= 41
        Nothing -> assertFailure "image final use has no completion"
    Nothing -> assertFailure "image final use was not committed"

aliasRejectionCase :: IO ()
aliasRejectionCase = do
  state <- BufferState.newBufferState
  let handle = RuntimeHandle 6
      firstUse = bufferUse handle (bufferMetadata state 0) Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT Access2.ACCESS_2_SHADER_WRITE_BIT
      aliasedUse = bufferUse handle (bufferMetadata state 4) Stage2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT Access2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT
  first <- command [firstUse] [] [] noRecord
  second <- command [aliasedUse] [] [] noRecord
  result <- try (planFrameResources [first, second]) :: IO (Either FrameResourceError FrameResourcePlan)
  case result of
    Left (FrameBufferAliasConflict actual) -> actual @?= handle
    Left error' -> assertFailure ("unexpected frame error: " <> show error')
    Right plan -> cancelFrameResourcePlan plan >> assertFailure "incompatible alias was accepted"
  available <- BufferState.beginBufferUse state
  _ <- BufferState.cancelBufferUse available
  pure ()

cancelCase :: IO ()
cancelCase = do
  bufferState <- BufferState.newBufferState
  imageState <- ImageState.newImageState 1 1
  let image = imageMetadata imageState
      imageHandle = RuntimeHandle 8
      imageWrite = imageUse imageHandle image Stage2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT Access2.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT Layout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  one <- command [bufferUse (RuntimeHandle 7) (bufferMetadata bufferState 0) Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT Access2.ACCESS_2_SHADER_WRITE_BIT] [imageWrite] [] noRecord
  plan <- planFrameResources [one]
  cancelFrameResourcePlan plan
  cancelFrameResourcePlan plan
  availableBuffer <- BufferState.beginBufferUse bufferState
  BufferState.cancelBufferUse availableBuffer >>= (@?= True)
  availableImage <- ImageState.beginImageUse imageState [ImageState.ImageSubresource 0 0]
  ImageState.cancelImageUse availableImage >>= (@?= True)

quarantineCase :: IO ()
quarantineCase = do
  bufferState <- BufferState.newBufferState
  imageState <- ImageState.newImageState 1 1
  let buffer = bufferMetadata bufferState 0
      image = imageMetadata imageState
      bufferHandle = RuntimeHandle 70
      imageHandle = RuntimeHandle 80
      bufferWrite = bufferUse bufferHandle buffer Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT Access2.ACCESS_2_SHADER_WRITE_BIT
      imageWrite = imageUse imageHandle image Stage2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT Access2.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT Layout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
  one <- command [bufferWrite] [imageWrite] [] noRecord
  plan <- planFrameResources [one]
  quarantineFrameResourcePlan plan
  quarantineFrameResourcePlan plan
  cancelFrameResourcePlan plan

  recorded <- timeout 2_000_000 (try (recordFrameCommands fakeQueue fakeCommandBuffer plan) :: IO (Either FrameResourceError ()))
  recorded @?= Just (Left FrameResourcePlanInactive)
  committed <- timeout 2_000_000 (try (commitFrameResourcePlanWithFamily fakeQueue 9 41 plan) :: IO (Either FrameResourceError ()))
  committed @?= Just (Left FrameResourcePlanInactive)

  bufferReuse <- timeout 2_000_000 (try (BufferState.beginBufferUse bufferState) :: IO (Either VpipeError BufferState.Reservation))
  case bufferReuse of
    Just (Left ResourceQuarantined) -> pure ()
    Just (Left error') -> assertFailure ("quarantined buffer state returned " <> show error')
    Just (Right reservation) -> BufferState.cancelBufferUse reservation >> assertFailure "quarantined buffer state accepted a new reservation"
    Nothing -> assertFailure "quarantined buffer state reuse did not fail promptly"
  imageReuse <- timeout 2_000_000 (try (ImageState.beginImageUse imageState [ImageState.ImageSubresource 0 0]) :: IO (Either VpipeError ImageState.ImageReservation))
  case imageReuse of
    Just (Left ResourceQuarantined) -> pure ()
    Just (Left error') -> assertFailure ("quarantined image state returned " <> show error')
    Just (Right reservation) -> ImageState.cancelImageUse reservation >> assertFailure "quarantined image state accepted a new reservation"
    Nothing -> assertFailure "quarantined image state reuse did not fail promptly"

renderedTargetCase :: IO ()
renderedTargetCase = do
  observations <- newIORef []
  let target = RuntimeHandle 8
      recorder _ prior = modifyIORef' observations (<> [target `elem` prior])
  first <- command [] [] [target] recorder
  second <- command [] [] [target] recorder
  frameCommandRenderedTargets first @?= [target]
  plan <- planFrameResources [first, second]
  framePlanRenderedTargets plan @?= [target]
  recordFrameCommands fakeQueue fakeCommandBuffer plan
  readIORef observations >>= (@?= [False, True])
  cancelFrameResourcePlan plan

zeroComputeCase :: IO ()
zeroComputeCase = do
  validated <- newIORef False
  built <- newIORef False
  planned <-
    prepareComputeFrameCommandWith
      (writeIORef validated True)
      (writeIORef built True >> error "zero dispatch constructed a command")
      (0, 4, 1)
  case planned of
    Nothing -> pure ()
    Just _ -> assertFailure "zero dispatch produced a frame command"
  readIORef validated >>= (@?= True)
  readIORef built >>= (@?= False)

command :: [FrameBufferUse] -> [FrameImageUse] -> [RuntimeHandle] -> (Handles.CommandBuffer -> [RuntimeHandle] -> IO ()) -> IO FrameCommand
command = newFrameCommand []

bufferUse :: RuntimeHandle -> BufferBindingMetadata -> Stage2.PipelineStageFlags2 -> Access2.AccessFlags2 -> FrameBufferUse
bufferUse handle metadata stage access =
  FrameBufferUse
    { frameBufferHandle = handle
    , frameBufferMetadata = metadata
    , frameBufferStage = stage
    , frameBufferAccess = access
    , frameBufferByteSize = 16
    }

imageUse :: RuntimeHandle -> ImageBindingMetadata -> Stage2.PipelineStageFlags2 -> Access2.AccessFlags2 -> Layout.ImageLayout -> FrameImageUse
imageUse handle metadata stage access layout =
  FrameImageUse
    { frameImageHandle = handle
    , frameImageMetadata = metadata
    , frameImageSubresources = [ImageState.ImageSubresource 0 0]
    , frameImageStage = stage
    , frameImageAccess = access
    , frameImageLayout = layout
    }

bufferMetadata :: BufferState.BufferState -> Word32 -> BufferBindingMetadata
bufferMetadata state = bufferMetadataWithHandle state 101

bufferMetadataWithHandle :: BufferState.BufferState -> Word64 -> Word32 -> BufferBindingMetadata
bufferMetadataWithHandle state rawHandle offset =
  BufferBindingMetadata
    { bufferBindingRawHandle = Handles.Buffer rawHandle
    , bufferBindingState = state
    , bufferBindingElementCount = 4
    , bufferBindingStride = 4
    , bufferBindingByteOffset = fromIntegral offset
    , bufferBindingUsage = zero
    }

imageMetadata :: ImageState.ImageState -> ImageBindingMetadata
imageMetadata state =
  ImageBindingMetadata
    { imageBindingRawHandle = Handles.Image 201
    , imageBindingRawView = Handles.ImageView 202
    , imageBindingState = state
    , imageBindingExtent = Fundamental.Extent3D 16 16 1
    , imageBindingFormat = Format.FORMAT_R8G8B8A8_UNORM
    , imageBindingAspect = Aspect.IMAGE_ASPECT_COLOR_BIT
    , imageBindingSamples = Samples.SAMPLE_COUNT_1_BIT
    , imageBindingMipLevel = 0
    , imageBindingArrayLayer = 0
    , imageBindingMipLevels = 1
    , imageBindingArrayLayers = 1
    , imageBindingUsage = zero
    }

bufferTransitions :: [FrameTransition] -> [FrameTransition]
bufferTransitions = filter isBuffer
 where
  isBuffer FrameBufferTransition{} = True
  isBuffer _ = False

imageTransitions :: [FrameTransition] -> [FrameTransition]
imageTransitions = filter isImage
 where
  isImage FrameImageTransition{} = True
  isImage _ = False

noRecord :: Handles.CommandBuffer -> [RuntimeHandle] -> IO ()
noRecord _ _ = pure ()

fakeQueue :: Queue
fakeQueue = error "pure frame test forced a Vulkan Queue"

fakeCommandBuffer :: Handles.CommandBuffer
fakeCommandBuffer = error "pure frame test forced a Vulkan CommandBuffer"
