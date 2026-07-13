{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Ordered frame construction and the swapchain submission protocol.
module Vpipe.Frame.Internal (
  Frame,
  Pass,
  frame,
  frameColorTarget,
  frameSlotIndexInternal,
  renderTo,
  render,
  computePass,
  computePassFor,
  withDynamicUniform,
  withDynamicStorage,
  copyPass,
  passActionForTest,
  preparePassForTest,
  passStepCountForTest,
  preservePrimaryAfterForTest,
  frameWithSubmissionHooksForTest,
  QueueSubmitDriverOutcome (..),
  FrameAcquireOutcome (..),
  acquireForFrameWith,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, newMVar, withMVar)
import Control.Exception (SomeException, catch, mask, mask_, onException, throwIO, try)
import Control.Monad (foldM, unless, void, when)
import Control.Monad.State.Strict (State, runState, state)
import Data.Foldable (toList, traverse_)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Data.Word (Word32, Word64)
import GHC.TypeLits (KnownNat)
import Vulkan.Core10.CommandBuffer qualified as CommandBuffer
import Vulkan.Core10.CommandBufferBuilding qualified as CommandBuilding
import Vulkan.Core10.Enums.ImageLayout qualified as Layout
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Core13.Enums.AccessFlags2 qualified as Access2
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Exception qualified as Vulkan
import Vulkan.Zero (zero)

import Vpipe.Buffer qualified as Buffer
import Vpipe.Buffer.Dynamic.Internal qualified as Dynamic
import Vpipe.Buffer.Format (BufferFormat, HostFormat)
import Vpipe.Buffer.Internal qualified as BufferInternal
import Vpipe.Compute.Frame.Internal (prepareComputeFrameCommandForLeased, prepareComputeFrameCommandLeased, preparedComputeFrameContext, preparedComputeFrameDescriptorLayout)
import Vpipe.Compute.Runtime.Internal (PreparedCompute (..))
import Vpipe.Context.Internal (Context, contextIdentity, graphicsQueue, logImageSubresourceTransition, registerContextFinalizerLeased)
import Vpipe.Context.Queue.Internal (BinarySemaphoreSignal (..), BinarySemaphoreWait (..), QueueSubmitDriverOutcome (..), SubmissionPublicationOutcome (..), runVulkanQueueSubmit, submitCommandBuffersWithPublicationLeased, submitCommandBuffersWithPublicationUsingLeased)
import Vpipe.Error (VpipeError (..))
import Vpipe.Format (Format (B8G8R8A8Srgb))
import Vpipe.Frame.Resource.Internal (FrameBufferUse (..), FrameCommand, FrameImageSource (..), FrameImageUse (..), FrameResourcePlan, FrameTransition (..), cancelFrameResourcePlan, commitFrameResourcePlan, frameCommandRenderedTargets, frameCommandRuntimeHandles, framePlanDependencies, framePlanRuntimeHandles, framePlanTransitions, newFrameCommand, planFrameResources, quarantineFrameResourcePlan, recordFrameCommands)
import Vpipe.Graphics.Frame.Internal (PreparedGraphicsPipeline (..), prepareGraphicsFrameCommandsLeased, preparedGraphicsContext, preparedGraphicsDescriptorLayout)
import Vpipe.Graphics.Submission.Internal (OwnedActions, newOwnedActions, releaseActions, releaseOwnedActions, retireOwnedActions, transferOwnedActions)
import Vpipe.Image.State qualified as ImageState
import Vpipe.Pipeline.Internal qualified as Pipeline
import Vpipe.Pipeline.Resource.Internal qualified as Resource
import Vpipe.Swapchain.Internal (AcquireOutcome (..), DeferredReason (..), FrameSlot (..), Generation (generationExtent), GenerationImage (..), LockedSwapchain, PresentResult (..), QueuePresentOutcome (..), Swapchain, acquireNextImageLocked, descriptorFrameForSlotLocked, lockedSwapchainContext, lockedSwapchainFrameDomain, poisonSwapchainLocked, presentGenerationImageLocked, publishAcceptedSubmissionLocked, publishAcquireRecoveryLocked, replaceGenerationLocked, takeRecreationNotificationLocked, withSwapchainOperation)

data Frame = Frame
  { activeSwapchain :: LockedSwapchain
  , activeSlot :: FrameSlot
  , activeImage :: GenerationImage
  , activeDynamicBuffers :: MVar (Set Dynamic.DynamicBufferKey)
  }

newtype Pass a = Pass (State (Seq PassStep) a)
  deriving newtype (Functor, Applicative, Monad)

newtype PassStep = PassStep
  { preparePassStep :: Frame -> IO [FrameCommand]
  }

data FrameAcquireOutcome a
  = FrameAcquireReady a
  | FrameAcquireDeferred DeferredReason
  | FrameAcquireNeedsRecreation
  deriving stock (Eq, Show)

data RuntimeLeaseReleases = RuntimeLeaseReleases
  { releasesAfterStateCommit :: [IO ()]
  , releasesAfterSlotRetirement :: [IO ()]
  }

{- | Execute one acquire/record/submit/present protocol under one Context
lease and one swapchain operation lock.
-}
frame :: Swapchain -> (Frame -> Pass ()) -> IO PresentResult
frame = runFrame runVulkanQueueSubmit (const (pure ()))

frameWithSubmissionHooksForTest :: (IO () -> IO QueueSubmitDriverOutcome) -> (Word64 -> IO ()) -> Swapchain -> (Frame -> Pass ()) -> IO PresentResult
frameWithSubmissionHooksForTest = runFrame

runFrame :: (IO () -> IO QueueSubmitDriverOutcome) -> (Word64 -> IO ()) -> Swapchain -> (Frame -> Pass ()) -> IO PresentResult
runFrame runQueueSubmit beforePublication swapchain build =
  withSwapchainOperation swapchain $ \locked ->
    mask $ \restore -> do
      acquisition <- acquireForFrame locked
      case acquisition of
        FrameAcquireDeferred reason -> pure (PresentDeferred reason)
        FrameAcquireNeedsRecreation -> pure (PresentDeferred RecreatePending)
        FrameAcquireReady (slot, generation, image, recreateAfterPresent) ->
          runAcquiredFrame restore runQueueSubmit beforePublication locked slot generation image recreateAfterPresent build

frameColorTarget :: Frame -> Pipeline.ColorImage 'B8G8R8A8Srgb
frameColorTarget = generationColorTarget . activeImage

{- | Hidden seam for transient-resource adapters which rotate by the selected
frame slot without exposing frames-in-flight in the public API.
-}
frameSlotIndexInternal :: Frame -> Int
frameSlotIndexInternal = frameSlotIndex . activeSlot

{- | Require every graphics command emitted by the nested scope to include
the supplied target, and require the scope to render it at least once.
Compute-only commands may be interleaved in the scope.
-}
renderTo :: Pipeline.ColorImage format -> Pass a -> Pass a
renderTo target nested = do
  let (result, nestedSteps) = unPassSteps nested
  appendPassStep $ \current -> do
    commands <- preparePassSteps current nestedSteps
    validateRenderScope (Pipeline.colorImageHandle target) commands
    pure commands
  pure result

render :: PreparedGraphicsPipeline environment -> environment -> Pass ()
render prepared environment =
  appendPassStep $ \current -> do
    validatePreparedContext "graphics pass" (preparedGraphicsContext prepared) current
    withMVar (preparedRenderLock prepared) $ \_ -> do
      descriptorFrame <-
        descriptorFrameForSlotLocked
          (activeSwapchain current)
          (activeSlot current)
          (preparedGraphicsDescriptorLayout prepared)
      prepareGraphicsFrameCommandsLeased prepared descriptorFrame environment

computePass :: forall environment x y z. (KnownNat x, KnownNat y, KnownNat z) => PreparedCompute environment x y z -> environment -> (Int, Int, Int) -> Pass ()
computePass prepared environment counts =
  appendPassStep $ \current -> do
    validatePreparedContext "compute pass" (preparedComputeFrameContext prepared) current
    wordCounts <- validateHostCounts counts
    withMVar (preparedComputeLock prepared) $ \_ -> do
      descriptorFrame <-
        descriptorFrameForSlotLocked
          (activeSwapchain current)
          (activeSlot current)
          (preparedComputeFrameDescriptorLayout prepared)
      maybe [] pure <$> prepareComputeFrameCommandLeased prepared descriptorFrame environment wordCounts

computePassFor :: forall environment x y z. (KnownNat x, KnownNat y, KnownNat z) => PreparedCompute environment x y z -> environment -> (Integer, Integer, Integer) -> Pass ()
computePassFor prepared environment totals =
  appendPassStep $ \current -> do
    validatePreparedContext "compute pass" (preparedComputeFrameContext prepared) current
    withMVar (preparedComputeLock prepared) $ \_ -> do
      descriptorFrame <-
        descriptorFrameForSlotLocked
          (activeSwapchain current)
          (activeSlot current)
          (preparedComputeFrameDescriptorLayout prepared)
      maybe [] pure <$> prepareComputeFrameCommandForLeased prepared descriptorFrame environment totals

withDynamicUniform :: (BufferFormat a, Buffer.HasUsage 'Buffer.Uniform usages) => Dynamic.FrameDynamicBuffer usages a -> Int -> [HostFormat a] -> (Pipeline.UniformBuffer a -> Pass ()) -> Pass ()
withDynamicUniform dynamicBuffer elementOffset values continuation =
  appendPassStep $ \current -> do
    claimDynamicBuffer current dynamicBuffer
    Dynamic.withFrameDynamicSlice
      dynamicBuffer
      (lockedSwapchainFrameDomain (activeSwapchain current))
      (frameSlotIndexInternal current)
      elementOffset
      values
      ( \slice -> do
          let runtimeHandle =
                Resource.managedBufferRuntimeHandleWithQuarantine
                  (Dynamic.dynamicSliceOwner slice)
                  (Dynamic.dynamicSliceGeneration slice)
                  (Dynamic.acquireDynamicSliceLease slice)
                  (Dynamic.quarantineDynamicSlice slice)
                  Resource.BufferBindingMetadata
                    { Resource.bufferBindingRawHandle = Dynamic.dynamicSliceHandle slice
                    , Resource.bufferBindingState = Dynamic.dynamicSliceState slice
                    , Resource.bufferBindingElementCount = Dynamic.dynamicSliceElements slice
                    , Resource.bufferBindingStride = Dynamic.dynamicSliceElementBytes slice
                    , Resource.bufferBindingByteOffset = Dynamic.dynamicSliceByteOffset slice
                    , Resource.bufferBindingUsage = Dynamic.dynamicSliceUsageFlags slice
                    }
          preparePassCommands current (continuation (Pipeline.UniformBuffer runtimeHandle))
      )

withDynamicStorage :: (BufferFormat a, Buffer.HasUsage 'Buffer.Storage usages) => Dynamic.FrameDynamicBuffer usages a -> Int -> [HostFormat a] -> (Pipeline.StorageBuffer a -> Pass ()) -> Pass ()
withDynamicStorage dynamicBuffer elementOffset values continuation =
  appendPassStep $ \current -> do
    claimDynamicBuffer current dynamicBuffer
    Dynamic.withFrameDynamicSlice
      dynamicBuffer
      (lockedSwapchainFrameDomain (activeSwapchain current))
      (frameSlotIndexInternal current)
      elementOffset
      values
      ( \slice -> do
          let runtimeHandle =
                Resource.managedBufferRuntimeHandleWithQuarantine
                  (Dynamic.dynamicSliceOwner slice)
                  (Dynamic.dynamicSliceGeneration slice)
                  (Dynamic.acquireDynamicSliceLease slice)
                  (Dynamic.quarantineDynamicSlice slice)
                  Resource.BufferBindingMetadata
                    { Resource.bufferBindingRawHandle = Dynamic.dynamicSliceHandle slice
                    , Resource.bufferBindingState = Dynamic.dynamicSliceState slice
                    , Resource.bufferBindingElementCount = Dynamic.dynamicSliceElements slice
                    , Resource.bufferBindingStride = Dynamic.dynamicSliceElementBytes slice
                    , Resource.bufferBindingByteOffset = Dynamic.dynamicSliceByteOffset slice
                    , Resource.bufferBindingUsage = Dynamic.dynamicSliceUsageFlags slice
                    }
          preparePassCommands current (continuation (Pipeline.StorageBuffer runtimeHandle))
      )

copyPass :: (BufferFormat a, Buffer.HasUsage 'Buffer.CopySrc sourceUsages, Buffer.HasUsage 'Buffer.CopyDst destinationUsages) => Buffer.Buffer sourceUsages a -> Int -> Buffer.Buffer destinationUsages a -> Int -> Int -> Pass ()
copyPass source sourceOffset destination destinationOffset elementCount =
  appendPassStep $ \current -> do
    validateCopyContext current "source" source
    validateCopyContext current "destination" destination
    let sourceElements = Buffer.bufferLength source
        destinationElements = Buffer.bufferLength destination
        sourceStride = Buffer.bufferStride source
        destinationStride = Buffer.bufferStride destination
    when
      (sourceOffset < 0 || elementCount < 0 || sourceOffset > sourceElements || elementCount > sourceElements - sourceOffset)
      (throwIO (BufferElementRangeInvalid sourceOffset elementCount sourceElements))
    when
      (destinationOffset < 0 || elementCount < 0 || destinationOffset > destinationElements || elementCount > destinationElements - destinationOffset)
      (throwIO (BufferElementRangeInvalid destinationOffset elementCount destinationElements))
    unless (sourceStride == destinationStride) $
      throwIO (BufferCopyStrideMismatch sourceStride destinationStride)
    if elementCount == 0
      then pure []
      else do
        let sourceByteOffset = sourceOffset * sourceStride
            destinationByteOffset = destinationOffset * destinationStride
            byteCount = elementCount * sourceStride
            sourceEnd = sourceByteOffset + byteCount
            destinationEnd = destinationByteOffset + byteCount
            sourceHandle = Resource.bufferRuntimeHandle source
            destinationHandle = Resource.bufferRuntimeHandle destination
            sourceByteSize = fromIntegral (sourceElements * sourceStride)
            destinationByteSize = fromIntegral (destinationElements * destinationStride)
        sourceMetadata <-
          maybe
            (frameFailure "copy pass" "source buffer binding metadata is unavailable")
            pure
            (Resource.runtimeBufferMetadata sourceHandle)
        destinationMetadata <-
          maybe
            (frameFailure "copy pass" "destination buffer binding metadata is unavailable")
            pure
            (Resource.runtimeBufferMetadata destinationHandle)
        when
          ( BufferInternal.bufferRawHandle source == BufferInternal.bufferRawHandle destination
              && sourceByteOffset < destinationEnd
              && destinationByteOffset < sourceEnd
          )
          (throwIO (BufferCopyOverlap sourceOffset destinationOffset elementCount))
        command <-
          newFrameCommand
            [sourceHandle, destinationHandle]
            [ FrameBufferUse
                { frameBufferHandle = sourceHandle
                , frameBufferMetadata = sourceMetadata
                , frameBufferStage = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
                , frameBufferAccess = Access2.ACCESS_2_TRANSFER_READ_BIT
                , frameBufferByteSize = sourceByteSize
                }
            , FrameBufferUse
                { frameBufferHandle = destinationHandle
                , frameBufferMetadata = destinationMetadata
                , frameBufferStage = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
                , frameBufferAccess = Access2.ACCESS_2_TRANSFER_WRITE_BIT
                , frameBufferByteSize = destinationByteSize
                }
            ]
            []
            []
            ( \commandBuffer _ ->
                CommandBuilding.cmdCopyBuffer
                  commandBuffer
                  (BufferInternal.bufferRawHandle source)
                  (BufferInternal.bufferRawHandle destination)
                  ( Vector.singleton
                      ( CommandBuilding.BufferCopy
                          (fromIntegral sourceByteOffset)
                          (fromIntegral destinationByteOffset)
                          (fromIntegral byteCount)
                      )
                  )
            )
        pure [command]
 where
  validateCopyContext current role buffer =
    unless
      ( contextIdentity (BufferInternal.bufferRawContext buffer)
          == contextIdentity (lockedSwapchainContext (activeSwapchain current))
      )
      (frameFailure "copy pass" (role <> " buffer belongs to a different Context than the swapchain"))

claimDynamicBuffer :: Frame -> Dynamic.FrameDynamicBuffer usages a -> IO ()
claimDynamicBuffer current dynamicBuffer =
  modifyMVarMasked (activeDynamicBuffers current) $ \claimed -> do
    let key = Dynamic.frameDynamicBufferKey dynamicBuffer
    if Set.member key claimed
      then throwIO FrameDynamicBufferAlreadyUsed
      else pure (Set.insert key claimed, ())

appendPassStep :: (Frame -> IO [FrameCommand]) -> Pass ()
appendPassStep action = Pass (state (\steps -> ((), steps |> PassStep action)))

unPassSteps :: Pass a -> (a, Seq PassStep)
unPassSteps (Pass action) = runState action Seq.empty

preparePassCommands :: Frame -> Pass a -> IO [FrameCommand]
preparePassCommands current pass = preparePassSteps current (snd (unPassSteps pass))

preparePassSteps :: Frame -> Seq PassStep -> IO [FrameCommand]
preparePassSteps current steps = concat <$> traverse (`preparePassStep` current) (toList steps)

validateRenderScope :: Pipeline.RuntimeHandle -> [FrameCommand] -> IO ()
validateRenderScope target commands = do
  let renderedByCommand = filter (not . null) (fmap frameCommandRenderedTargets commands)
  when (null renderedByCommand) $
    frameFailure "renderTo" "the scope emitted no graphics commands"
  unless (all (target `elem`) renderedByCommand) $
    frameFailure "renderTo" "a graphics command in the scope did not render to the declared target"

validatePreparedContext :: String -> Context -> Frame -> IO ()
validatePreparedContext operation preparedContext current =
  unless (contextIdentity preparedContext == contextIdentity (lockedSwapchainContext (activeSwapchain current))) $
    frameFailure operation "the prepared pipeline belongs to a different Context than the swapchain"

validateHostCounts :: (Int, Int, Int) -> IO (Word32, Word32, Word32)
validateHostCounts (x, y, z) =
  (,,) <$> dimension "x" x <*> dimension "y" y <*> dimension "z" z
 where
  dimension label value
    | value < 0 = frameFailure "compute pass" (label <> " workgroup count must be non-negative")
    | toInteger value > toInteger (maxBound :: Word32) =
        frameFailure "compute pass" (label <> " workgroup count exceeds Word32")
    | otherwise = pure (fromIntegral value)

acquireForFrame :: LockedSwapchain -> IO (FrameAcquireOutcome (FrameSlot, Generation, GenerationImage, Bool))
acquireForFrame locked =
  acquireForFrameWith
    (toFrameAcquireOutcome <$> acquireNextImageLocked locked)
    (void (replaceGenerationLocked locked))

acquireForFrameWith :: IO (FrameAcquireOutcome a) -> IO () -> IO (FrameAcquireOutcome a)
acquireForFrameWith acquire recreate = do
  outcome <- acquire
  case outcome of
    FrameAcquireNeedsRecreation -> do
      recreate
      reacquired <- acquire
      case reacquired of
        FrameAcquireNeedsRecreation -> pure (FrameAcquireDeferred RecreatePending)
        _ -> pure reacquired
    FrameAcquireDeferred FramebufferMinimized -> recreate >> pure (FrameAcquireDeferred FramebufferMinimized)
    _ -> pure outcome

toFrameAcquireOutcome :: AcquireOutcome -> FrameAcquireOutcome (FrameSlot, Generation, GenerationImage, Bool)
toFrameAcquireOutcome outcome = case outcome of
  AcquireReady slot generation image recreateAfterPresent ->
    FrameAcquireReady (slot, generation, image, recreateAfterPresent)
  AcquireDeferredNow reason -> FrameAcquireDeferred reason
  AcquireNeedsRecreation -> FrameAcquireNeedsRecreation

runAcquiredFrame :: (forall a. IO a -> IO a) -> (IO () -> IO QueueSubmitDriverOutcome) -> (Word64 -> IO ()) -> LockedSwapchain -> FrameSlot -> Generation -> GenerationImage -> Bool -> (Frame -> Pass ()) -> IO PresentResult
runAcquiredFrame restore runQueueSubmit beforePublication locked slot generation image recreateAfterPresent build = do
  claimedDynamicBuffers <- newMVar Set.empty
  let current = Frame locked slot image claimedDynamicBuffers
      prepare = do
        userCommands <- preparePassCommands current (build current)
        presentCommand <- finalPresentCommand image
        pure (userCommands <> [presentCommand])
  commandsResult <- try (restore prepare)
  case commandsResult of
    Left (primary :: SomeException) -> recoverBeforeSubmit locked slot Nothing primary
    Right commands -> do
      let context = lockedSwapchainContext locked
          handles = uniqueRuntimeHandles (concatMap frameCommandRuntimeHandles commands)
      releasesResult <- try (acquireRuntimeLeases context handles)
      case releasesResult of
        Left (primary :: SomeException) -> recoverBeforeSubmit locked slot Nothing primary
        Right releases -> do
          -- Reservation ownership must be installed while the outer frame mask
          -- is still in force. Restoring async exceptions here could cancel
          -- after the plan returns but before the caller receives it.
          planResult <- try (planFrameResources commands `onException` releaseRuntimeLeases releases)
          case planResult of
            Left (primary :: SomeException) -> recoverBeforeSubmit locked slot Nothing primary
            Right plan -> runPlannedFrame restore runQueueSubmit beforePublication locked slot generation image recreateAfterPresent plan releases

runPlannedFrame :: (forall a. IO a -> IO a) -> (IO () -> IO QueueSubmitDriverOutcome) -> (Word64 -> IO ()) -> LockedSwapchain -> FrameSlot -> Generation -> GenerationImage -> Bool -> FrameResourcePlan -> RuntimeLeaseReleases -> IO PresentResult
runPlannedFrame restore runQueueSubmit beforePublication locked slot generation image recreateAfterPresent plan releases = do
  let context = lockedSwapchainContext locked
  recording <-
    try
      ( restore (recordFramePlan context slot plan)
          `onException` releaseRuntimeLeases releases
      )
  case recording of
    Left (primary :: SomeException) -> recoverBeforeSubmit locked slot (Just plan) primary
    Right () -> do
      stateOwned <-
        newOwnedActions (releasesAfterStateCommit releases)
          `onException` releaseRuntimeLeases releases
      slotOwned <-
        newOwnedActions (releasesAfterStateCommit releases <> releasesAfterSlotRetirement releases)
          `onException` releaseRuntimeLeases releases
      submitPlannedFrame runQueueSubmit beforePublication locked slot generation image recreateAfterPresent plan stateOwned slotOwned

recordFramePlan :: Context -> FrameSlot -> FrameResourcePlan -> IO ()
recordFramePlan context slot plan = mapVulkan "frame command recording" $ do
  let commandBuffer = frameSlotCommandBuffer slot
      beginInfo =
        (zero :: CommandBuffer.CommandBufferBeginInfo '[])
          { CommandBuffer.flags = CommandBuffer.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
          }
  CommandBuffer.beginCommandBuffer commandBuffer beginInfo
  logFrameImageTransitions context plan
  recordFrameCommands (graphicsQueue context) commandBuffer plan
  CommandBuffer.endCommandBuffer commandBuffer

logFrameImageTransitions :: Context -> FrameResourcePlan -> IO ()
logFrameImageTransitions context plan =
  traverse_ logTransition (framePlanTransitions plan)
 where
  logTransition transition = case transition of
    FrameImageTransition _ subresource source destination -> do
      let Handles.Image image = Resource.imageBindingRawHandle (frameImageMetadata destination)
      logImageSubresourceTransition
        context
        image
        (frameImageSourceLayout source)
        (frameImageLayout destination)
        (ImageState.imageMipLevel subresource)
        1
        (ImageState.imageArrayLayer subresource)
        1
    FrameBufferTransition{} -> pure ()

frameImageSourceLayout :: FrameImageSource -> Layout.ImageLayout
frameImageSourceLayout source = case source of
  FrameImageExternal Nothing -> Layout.IMAGE_LAYOUT_UNDEFINED
  FrameImageExternal (Just previous) -> ImageState.imageUseLayout previous
  FrameImageIntra _ previous -> frameImageLayout previous

submitPlannedFrame :: (IO () -> IO QueueSubmitDriverOutcome) -> (Word64 -> IO ()) -> LockedSwapchain -> FrameSlot -> Generation -> GenerationImage -> Bool -> FrameResourcePlan -> OwnedActions -> OwnedActions -> IO PresentResult
submitPlannedFrame runQueueSubmit beforePublication locked slot generation image recreateAfterPresent plan stateOwned slotOwned = do
  let context = lockedSwapchainContext locked
      queue = graphicsQueue context
      publish timeline = publishOwnedSubmission beforePublication context locked slot image timeline slotOwned
  submission <-
    try $
      submitCommandBuffersWithPublicationUsingLeased
        runQueueSubmit
        queue
        (framePlanDependencies plan)
        [BinarySemaphoreWait (frameSlotImageAvailable slot) Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT]
        [BinarySemaphoreSignal (generationRenderFinished image) Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT]
        (Vector.singleton (frameSlotCommandBuffer slot))
        publish
  case submission of
    Left (primary :: SomeException) -> quarantineUnknownFrame primary
    Right (SubmissionRejected primary) ->
      preservePrimaryAfterForTest
        primary
        [ cancelFrameResourcePlan plan
        , releaseOwnedActions stateOwned
        , releaseOwnedActions slotOwned
        , recoverAcquireSemaphore locked slot
        , void (replaceGenerationLocked locked)
        ]
    Right (SubmissionAcceptanceUnknown primary) -> quarantineUnknownFrame primary
    Right (SubmissionAcceptedPublicationFailed timeline primary) -> do
      acceptedState <- try (commitFrameResourcePlan queue timeline plan >> releaseOwnedActions stateOwned)
      case acceptedState of
        Left (_ :: SomeException) -> void (retireOwnedActions (registerContextFinalizerLeased context) [stateOwned])
        Right () -> pure ()
      bestEffort (recoverAcceptedPublication locked slot generation image recreateAfterPresent timeline slotOwned)
      case acceptedState of
        Left (_ :: SomeException) -> bestEffort (poisonSwapchainLocked locked)
        Right () -> pure ()
      throwIO primary
    Right (SubmissionAccepted timeline) -> do
      commitResult <- try (commitFrameResourcePlan queue timeline plan >> releaseOwnedActions stateOwned)
      case commitResult of
        Left (primary :: SomeException) -> do
          void (retireOwnedActions (registerContextFinalizerLeased context) [stateOwned])
          bestEffort (poisonSwapchainLocked locked)
          throwIO primary
        Right () -> finishPresentation locked generation image recreateAfterPresent
 where
  quarantineUnknownFrame primary =
    mask_ $
      preservePrimaryAfterForTest
        primary
        ( [quarantineFrameResourcePlan plan]
            <> fmap Resource.runtimeHandleQuarantine (framePlanRuntimeHandles plan)
            <> [ void (retireOwnedActions (registerContextFinalizerLeased (lockedSwapchainContext locked)) [stateOwned, slotOwned])
               , poisonSwapchainLocked locked
               ]
        )

publishOwnedSubmission :: (Word64 -> IO ()) -> Context -> LockedSwapchain -> FrameSlot -> GenerationImage -> Word64 -> OwnedActions -> IO ()
publishOwnedSubmission beforePublication context locked slot image timeline owned = do
  beforePublication timeline
  transferred <- transferOwnedActions owned
  releases <- maybe (frameFailure "frame submission publication" "resource-release ownership was already transferred") pure transferred
  publishAcceptedSubmissionLocked locked slot image timeline releases
    `catch` \(primary :: SomeException) -> do
      bestEffort (registerContextFinalizerLeased context (releaseActions releases))
      throwIO primary

recoverAcceptedPublication :: LockedSwapchain -> FrameSlot -> Generation -> GenerationImage -> Bool -> Word64 -> OwnedActions -> IO ()
recoverAcceptedPublication locked slot generation image recreateAfterPresent timeline slotOwned =
  recover `onException` bestEffort (poisonSwapchainLocked locked)
 where
  recover = do
    let context = lockedSwapchainContext locked
    publication <- try (publishOwnedSubmission (const (pure ())) context locked slot image timeline slotOwned)
    case publication of
      Right () -> pure ()
      Left (_ :: SomeException) -> publishAcceptedSubmissionLocked locked slot image timeline []
    void (finishPresentation locked generation image recreateAfterPresent)

finishPresentation :: LockedSwapchain -> Generation -> GenerationImage -> Bool -> IO PresentResult
finishPresentation locked generation image recreateAfterPresent = do
  presented <- try (presentGenerationImageLocked locked generation image)
  case presented of
    Left (primary :: SomeException) -> do
      bestEffort (poisonSwapchainLocked locked)
      throwIO primary
    Right QueuePresentNeedsRecreation -> do
      void (replaceGenerationLocked locked)
      pure (PresentDeferred RecreatePending)
    Right (QueuePresentComplete presentSuboptimal) -> do
      when (recreateAfterPresent || presentSuboptimal) (void (replaceGenerationLocked locked))
      recreated <- takeRecreationNotificationLocked locked
      pure (Presented (generationExtent generation) recreated)

recoverBeforeSubmit :: LockedSwapchain -> FrameSlot -> Maybe FrameResourcePlan -> SomeException -> IO a
recoverBeforeSubmit locked slot plan primary =
  preservePrimaryAfterForTest
    primary
    [ traverse_ cancelFrameResourcePlan plan
    , recoverAcquireSemaphore locked slot
    , void (replaceGenerationLocked locked)
    ]

recoverAcquireSemaphore :: LockedSwapchain -> FrameSlot -> IO ()
recoverAcquireSemaphore locked slot = do
  let context = lockedSwapchainContext locked
      queue = graphicsQueue context
      publish timeline = publishAcquireRecoveryLocked locked slot timeline []
  recovery <-
    try $
      submitCommandBuffersWithPublicationLeased
        queue
        []
        [BinarySemaphoreWait (frameSlotImageAvailable slot) Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT]
        []
        Vector.empty
        publish
  case recovery of
    Right (SubmissionAccepted _) -> pure ()
    Right (SubmissionRejected recoveryFailure) -> do
      bestEffort (poisonSwapchainLocked locked)
      throwIO recoveryFailure
    Right (SubmissionAcceptanceUnknown recoveryFailure) -> do
      bestEffort (poisonSwapchainLocked locked)
      throwIO recoveryFailure
    Right (SubmissionAcceptedPublicationFailed _ recoveryFailure) -> do
      bestEffort (poisonSwapchainLocked locked)
      throwIO recoveryFailure
    Left (recoveryFailure :: SomeException) -> do
      bestEffort (poisonSwapchainLocked locked)
      throwIO recoveryFailure

finalPresentCommand :: GenerationImage -> IO FrameCommand
finalPresentCommand image = do
  let target = generationColorTarget image
      handle = Pipeline.colorImageHandle target
  metadata <-
    maybe
      (frameFailure "frame present transition" "the swapchain target has no managed image metadata")
      pure
      (Resource.runtimeImageMetadata handle)
  newFrameCommand
    [handle]
    []
    [ FrameImageUse
        { frameImageHandle = handle
        , frameImageMetadata = metadata
        , frameImageSubresources = [ImageState.ImageSubresource 0 0]
        , frameImageStage = Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT
        , frameImageAccess = zero :: Access2.AccessFlags2
        , frameImageLayout = Layout.IMAGE_LAYOUT_PRESENT_SRC_KHR
        }
    ]
    []
    (\_ _ -> pure ())

acquireRuntimeLeases :: Context -> [Pipeline.RuntimeHandle] -> IO RuntimeLeaseReleases
acquireRuntimeLeases context handles = mask_ $ do
  validated <- traverse validate handles
  foldM acquire (RuntimeLeaseReleases [] []) validated
 where
  validate handle = do
    unless (Resource.runtimeHandleOwner handle == Just (contextIdentity context)) $
      frameFailure "frame resource validation" ("resource handle " <> show handle <> " is unmanaged or belongs to a different Context")
    acquireLease <-
      maybe
        (frameFailure "frame resource validation" ("resource handle " <> show handle <> " has no managed lifetime"))
        pure
        (Resource.runtimeHandleLease handle)
    pure (handle, acquireLease)
  acquire releases (handle, acquireLease) = do
    release <- acquireLease `onException` releaseRuntimeLeases releases
    idempotent <- idempotentAction release `onException` releaseActions (release : runtimeLeaseActions releases)
    pure $ case Resource.runtimeHandleKind handle of
      Resource.RuntimeObjectBuffer ->
        releases{releasesAfterStateCommit = idempotent : releasesAfterStateCommit releases}
      Resource.RuntimeObjectImageView ->
        releases{releasesAfterStateCommit = idempotent : releasesAfterStateCommit releases}
      _ ->
        releases{releasesAfterSlotRetirement = idempotent : releasesAfterSlotRetirement releases}

uniqueRuntimeHandles :: [Pipeline.RuntimeHandle] -> [Pipeline.RuntimeHandle]
uniqueRuntimeHandles = foldl add []
 where
  add handles handle
    | handle `elem` handles = handles
    | otherwise = handles <> [handle]

releaseRuntimeLeases :: RuntimeLeaseReleases -> IO ()
releaseRuntimeLeases = releaseActions . runtimeLeaseActions

runtimeLeaseActions :: RuntimeLeaseReleases -> [IO ()]
runtimeLeaseActions releases =
  releasesAfterStateCommit releases <> releasesAfterSlotRetirement releases

idempotentAction :: IO () -> IO (IO ())
idempotentAction action = do
  pending <- newMVar (Just action)
  pure $
    modifyMVarMasked pending $ \case
      Nothing -> pure (Nothing, ())
      Just release -> release >> pure (Nothing, ())

preservePrimaryAfterForTest :: SomeException -> [IO ()] -> IO a
preservePrimaryAfterForTest primary actions = do
  traverse_ bestEffort actions
  throwIO primary

bestEffort :: IO () -> IO ()
bestEffort action = void (try action :: IO (Either SomeException ()))

passActionForTest :: IO [FrameCommand] -> Pass ()
passActionForTest action = appendPassStep (const action)

preparePassForTest :: Pass a -> IO [FrameCommand]
preparePassForTest = preparePassCommands fakeFrame
 where
  fakeFrame = Frame (error "test Frame has no swapchain") (error "test Frame has no slot") (error "test Frame has no image") (error "test Frame has no dynamic-buffer claims")

passStepCountForTest :: Pass a -> Int
passStepCountForTest = Seq.length . snd . unPassSteps

mapVulkan :: String -> IO a -> IO a
mapVulkan operation action =
  action `catch` \(error' :: Vulkan.VulkanException) ->
    if Vulkan.vulkanExceptionResult error' == Result.ERROR_DEVICE_LOST
      then throwIO DeviceLost
      else throwIO (VulkanFailure operation (show (Vulkan.vulkanExceptionResult error')))

frameFailure :: String -> String -> IO a
frameFailure operation detail = throwIO (VulkanFailure operation detail)
