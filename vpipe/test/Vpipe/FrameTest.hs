{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Vpipe.FrameTest (framePureTests, frameTests) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, tryPutMVar, tryReadMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, bracket, fromException, throwIO, toException, try)
import Control.Monad (replicateM_, unless, void, when)
import Data.Foldable (traverse_)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word32)
import Linear (V3, V4 (..))
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Vulkan.Extensions.VK_EXT_headless_surface qualified as Headless
import Vulkan.Extensions.VK_KHR_surface qualified as Surface
import Vulkan.Zero (zero)

import Vpipe.Buffer (Buffer, destroyBuffer, newBuffer, readBuffer, writeBuffer)
import Vpipe.Buffer qualified as Buffer
import Vpipe.Buffer.Dynamic (FrameDynamicBuffer, destroyFrameDynamicBuffer, newFrameDynamicBuffer)
import Vpipe.Compute
import Vpipe.Compute.Frame.Internal (prepareComputeFrameCommandLeased, prepareComputeFrameCommandWithDescriptorHookLeased, preparedComputeFrameDescriptorLayout)
import Vpipe.Context (Context, VpipeConfig (..), defaultVpipeConfig, graphicsQueue, withVpipe)
import Vpipe.Context.Internal (contextIdentity, withContextLease)
import Vpipe.Context.Queue.Internal (currentTimelineValueForTest)
import Vpipe.Descriptor.Internal (DescriptorStats (..), descriptorFrameStats, newDescriptorFrame)
import Vpipe.Error (VpipeError (..))
import Vpipe.Expr qualified as Expr
import Vpipe.Format (Format (B8G8R8A8Srgb, R8G8B8A8Unorm))
import Vpipe.Frame
import Vpipe.Frame.Internal (FrameAcquireOutcome (..), QueueSubmitDriverOutcome (..), acquireForFrameWith, frameWithSubmissionHooksForTest, passActionForTest, passStepCountForTest, preparePassForTest, preservePrimaryAfterForTest)
import Vpipe.Frame.Resource.Internal (FrameCommand, newFrameCommand)
import Vpipe.Graphics (PreparedGraphicsPipeline, newGraphicsRuntime, prepareGraphicsPipeline)
import Vpipe.Image (Image, destroyImage, imageExtent2D, newImage)
import Vpipe.Image.Types qualified as ImageTypes
import Vpipe.Pipeline.Internal qualified as Pipeline
import Vpipe.Pipeline.Resource.Internal qualified as Resource
import Vpipe.Resource.Lifetime qualified as Lifetime
import Vpipe.Surface (withVpipeSurfaces)
import Vpipe.Surface.Driver (mkSurfaceFactoryWithExtents)
import Vpipe.Swapchain (DeferredReason (..), PresentResult (..), Swapchain, defaultSwapchainConfig, destroySwapchain, newSwapchain)
import Vpipe.Swapchain.Internal (lookupOrCreateSlotValueForTest, resetSlotValuesForTest)

frameTests :: TestTree
frameTests =
  testGroup
    "frame"
    [ framePureTests
    , testCase "frame descriptor preparation releases temporary descriptor leases" descriptorPreparationOwnershipTest
    , testCase "copyPass accepts zero and endpoint ranges" copyPassRangeTest
    , testCase "copyPass rejects incompatible frame-time inputs" copyPassValidationTest
    , testCase "cancellation during lease acquisition releases an immediately destroyed resource" cancelledLeaseAcquisitionTest
    , testCase "frame handle prevalidation leases neither an earlier buffer nor image" laterInvalidHandlePrevalidationTest
    , testCase "rejected frame submission cancels reservations and releases resources" rejectedSubmissionTest
    , testCase "unknown frame submission quarantines reuse and retains cleanup" unknownSubmissionTest
    , testCase "accepted publication failure commits reservations and releases resources" publicationFailureTest
    , testCase "submitted frame resources may be destroyed immediately" submittedResourceLifetimeTest
    , testCase "headless mixed compute and graphics use one submit per presented frame" liveMixedFrameTest
    ]

framePureTests :: TestTree
framePureTests =
  testGroup
    "frame pure"
    [ testCase "Pass prepares actions in monadic program order" passOrderingTest
    , testCase "renderTo validates the actual commands in its scope" renderScopeTest
    , testCase "acquire-side recreation retries once before deferring a rejected replacement" acquireSideRecreationTest
    , testCase "descriptor storage rotates by slot and resets only that slot" descriptorSlotTest
    , testCase "recovery preserves its primary failure and runs every cleanup" recoveryOwnershipTest
    , testCase "10k ordered pass steps add no implicit stalls or submissions" passStressTest
    ]

passOrderingTest :: IO ()
passOrderingTest = do
  events <- newIORef []
  let record value = passActionForTest (modifyIORef' events (<> [value]) >> pure [])
      passes = record (1 :: Int) >> record 2 >> record 3
  passStepCountForTest passes @?= 3
  preparePassForTest passes >>= (@?= 0) . length
  readIORef events >>= (@?= [1, 2, 3])

renderScopeTest :: IO ()
renderScopeTest = do
  let targetHandle = Pipeline.RuntimeHandle 101
      otherHandle = Pipeline.RuntimeHandle 102
      target = Pipeline.ColorImage targetHandle :: Pipeline.ColorImage 'B8G8R8A8Srgb
      command targets = newFrameCommand [] [] [] targets (\_ _ -> pure ())
      oneCommand targets = command targets >>= \value -> pure [value]
      matching = renderTo target (passActionForTest (oneCommand [targetHandle, Pipeline.RuntimeHandle 103]))
      mismatching =
        renderTo target $ do
          passActionForTest (oneCommand [targetHandle])
          passActionForTest (oneCommand [otherHandle])
      emptyScope = renderTo target (passActionForTest (pure []))
  preparePassForTest matching >>= (@?= 1) . length
  mismatch <- try (preparePassForTest mismatching) :: IO (Either VpipeError [FrameCommand])
  case mismatch of
    Left VulkanFailure{vulkanOperation = "renderTo"} -> pure ()
    unexpected -> assertFailure ("expected renderTo target mismatch, got " <> showResult unexpected)
  empty <- try (preparePassForTest emptyScope) :: IO (Either VpipeError [FrameCommand])
  case empty of
    Left VulkanFailure{vulkanOperation = "renderTo"} -> pure ()
    unexpected -> assertFailure ("expected empty renderTo rejection, got " <> showResult unexpected)

acquireSideRecreationTest :: IO ()
acquireSideRecreationTest = do
  let runAcquisition pendingOutcomes = do
        pending <- newIORef pendingOutcomes
        events <- newIORef []
        let acquire =
              atomicModifyIORef' pending $ \case
                [] -> ([], FrameAcquireDeferred AcquireTimedOut)
                outcome : rest -> (rest, outcome)
            recreate = modifyIORef' events (<> ["recreate"])
        result <- acquireForFrameWith acquire recreate
        case result of
          FrameAcquireReady value -> modifyIORef' events (<> ["callback " <> value])
          _ -> pure ()
        (,,) result <$> readIORef events <*> readIORef pending

  ready <- runAcquisition [FrameAcquireNeedsRecreation, FrameAcquireReady "replacement"]
  ready @?= (FrameAcquireReady "replacement", ["recreate", "callback replacement"], [])

  replacementRejected <- runAcquisition [FrameAcquireNeedsRecreation, FrameAcquireNeedsRecreation]
  replacementRejected @?= (FrameAcquireDeferred RecreatePending, ["recreate"], [])

  minimized <- runAcquisition [FrameAcquireDeferred FramebufferMinimized, FrameAcquireReady "unexpected"]
  minimized @?= (FrameAcquireDeferred FramebufferMinimized, ["recreate"], [FrameAcquireReady "unexpected"])

descriptorSlotTest :: IO ()
descriptorSlotTest = do
  firstSlot <- newMVar []
  secondSlot <- newMVar []
  nextValue <- newIORef (0 :: Int)
  resets <- newIORef []
  let create = atomicModifyIORef' nextValue (\value -> let next = value + 1 in (next, next))
  first <- lookupOrCreateSlotValueForTest firstSlot "layout" create
  firstAgain <- lookupOrCreateSlotValueForTest firstSlot "layout" create
  second <- lookupOrCreateSlotValueForTest secondSlot "layout" create
  (first, firstAgain, second) @?= (1, 1, 2)
  resetSlotValuesForTest firstSlot (\value -> modifyIORef' resets (<> [value]))
  readIORef resets >>= (@?= [1])
  lookupOrCreateSlotValueForTest secondSlot "layout" create >>= (@?= 2)
  readIORef nextValue >>= (@?= 2)

recoveryOwnershipTest :: IO ()
recoveryOwnershipTest = do
  events <- newMVar []
  let record value = modifyMVar_ events (pure . (<> [value]))
      primary = toException SurfaceLost
  outcome <-
    try
      ( preservePrimaryAfterForTest
          primary
          [ record "cancel reservations"
          , record "recovery submit" >> throwIO DeviceLost
          , record "conservative recreate"
          ] ::
          IO ()
      ) ::
      IO (Either VpipeError ())
  outcome @?= Left SurfaceLost
  readMVar events >>= (@?= ["cancel reservations", "recovery submit", "conservative recreate"])

passStressTest :: IO ()
passStressTest = do
  progress <- newIORef (0 :: Int, False)
  let count = 10_000
      step expected =
        passActionForTest $ do
          atomicModifyIORef' progress $ \(next, outOfOrder) ->
            ((next + 1, outOfOrder || next /= expected), ())
          pure []
      passes = traverse_ step [0 .. count - 1]
  passStepCountForTest passes @?= count
  preparePassForTest passes >>= (@?= 0) . length
  readIORef progress >>= (@?= (count, False))

copyPassRangeTest :: IO ()
copyPassRangeTest = withHeadlessSwapchain $ \context swapchain -> do
  source <- newBuffer context 3 :: IO (Buffer '[ 'Buffer.CopySrc] Word32)
  destination <- newBuffer context 3 :: IO (Buffer '[ 'Buffer.CopySrc, 'Buffer.CopyDst] Word32)
  writeBuffer source 0 [1, 2, 3]
  writeBuffer destination 0 [9, 9, 9]
  assertPresented =<< frame swapchain (\_ -> copyPass source 3 destination 3 0)
  readBuffer destination 0 3 >>= (@?= [9, 9, 9])
  assertPresented =<< frame swapchain (\_ -> copyPass source 2 destination 2 1)
  readBuffer destination 0 3 >>= (@?= [9, 9, 3])

copyPassValidationTest :: IO ()
copyPassValidationTest = withHeadlessSwapchain $ \context swapchain -> do
  mismatchSource <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.CopySrc] (V3 Float))
  mismatchDestination <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Vertex, 'Buffer.CopyDst] (V3 Float))
  mismatch <- try (frame swapchain (\_ -> copyPass mismatchSource 0 mismatchDestination 0 1))
  case mismatch of
    Left BufferCopyStrideMismatch{} -> pure ()
    unexpected -> assertFailure ("expected copy stride mismatch, got " <> showPresentResult unexpected)

  overlap <- newBuffer context 3 :: IO (Buffer '[ 'Buffer.CopySrc, 'Buffer.CopyDst] Word32)
  overlapResult <- try (frame swapchain (\_ -> copyPass overlap 0 overlap 1 2))
  case overlapResult of
    Left BufferCopyOverlap{} -> pure ()
    unexpected -> assertFailure ("expected overlapping copy rejection, got " <> showPresentResult unexpected)

  foreignConfig <- frameTestConfig
  withVpipe foreignConfig $ \foreignContext -> do
    foreignDestination <- newBuffer foreignContext 1 :: IO (Buffer '[ 'Buffer.CopyDst] Word32)
    foreignResult <- try (frame swapchain (\_ -> copyPass overlap 0 foreignDestination 0 1))
    case foreignResult of
      Left VulkanFailure{vulkanOperation = "copy pass"} -> pure ()
      unexpected -> assertFailure ("expected foreign-context copy rejection, got " <> showPresentResult unexpected)

cancelledLeaseAcquisitionTest :: IO ()
cancelledLeaseAcquisitionTest = withHeadlessSwapchain $ \context swapchain -> do
  buffer <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Storage] Word32)
  let bufferHandle = Resource.bufferRuntimeHandle buffer
  owner <-
    maybe
      (throwIO (VulkanFailure "frame cancellation test" "the buffer has no managed owner"))
      pure
      (Resource.runtimeHandleOwner bufferHandle)
  generation <-
    maybe
      (throwIO (VulkanFailure "frame cancellation test" "the buffer has no resource generation"))
      pure
      (Resource.runtimeHandleGeneration bufferHandle)
  acquireBufferLease <-
    maybe
      (throwIO (VulkanFailure "frame cancellation test" "the buffer has no managed lifetime"))
      pure
      (Resource.runtimeHandleLease bufferHandle)
  acquisitionStarted <- newEmptyMVar
  cancellationRequested <- newIORef False
  acquisitionProgress <- newIORef (0 :: Int)
  let cancellationBurnIterations = 5_000_000
      awaitCancellationRequest = do
        requested <- readIORef cancellationRequested
        unless requested awaitCancellationRequest
  let acquireDelayedLease = do
        release <- acquireBufferLease
        putMVar acquisitionStarted ()
        awaitCancellationRequest
        replicateM_ cancellationBurnIterations (atomicModifyIORef' acquisitionProgress (\value -> (value + 1, ())))
        pure release
      delayedHandle =
        Resource.managedRuntimeHandle
          owner
          generation
          (Resource.runtimeHandleWord bufferHandle)
          acquireDelayedLease
  command <- newFrameCommand [delayedHandle] [] [] [] (\_ _ -> pure ())
  frameFinished <- newEmptyMVar
  frameThread <-
    forkIO $ do
      result <- try (frame swapchain (\_ -> passActionForTest (pure [command]))) :: IO (Either SomeException PresentResult)
      putMVar frameFinished result
  started <- timeout 2_000_000 (takeMVar acquisitionStarted)
  case started of
    Just () -> pure ()
    Nothing -> do
      writeIORef cancellationRequested True
      killThread frameThread
      assertFailure "frame lease acquisition did not start within two seconds"
  writeIORef cancellationRequested True
  killThread frameThread
  readIORef acquisitionProgress >>= (@?= cancellationBurnIterations)
  assertDestructionCompletes "buffer cancelled during frame lease acquisition" (destroyBuffer buffer)
  finished <- timeout 2_000_000 (takeMVar frameFinished)
  case finished of
    Just (Left failure) -> (fromException failure :: Maybe AsyncException) @?= Just ThreadKilled
    Just (Right result) -> assertFailure ("cancelled frame unexpectedly completed: " <> show result)
    Nothing -> assertFailure "cancelled frame did not finish within two seconds"

laterInvalidHandlePrevalidationTest :: IO ()
laterInvalidHandlePrevalidationTest = withHeadlessSwapchain $ \context swapchain -> do
  buffer <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Storage] Word32)
  image <- newImage context (imageExtent2D 1 1) 1 1 :: IO (Image 'ImageTypes.D2 'R8G8B8A8Unorm '[ 'ImageTypes.ColorTarget])
  imageBinding <- Pipeline.colorImageBinding image
  let bufferHandle = Resource.bufferRuntimeHandle buffer
      imageHandle = Pipeline.colorImageHandle imageBinding
      invalidHandle = Pipeline.RuntimeHandle 0xBAD5A
  command <- newFrameCommand [bufferHandle, imageHandle, invalidHandle] [] [] [] (\_ _ -> pure ())
  result <- try (frame swapchain (\_ -> passActionForTest (pure [command]))) :: IO (Either VpipeError PresentResult)
  case result of
    Left (VulkanFailure "frame resource validation" detail)
      | "unmanaged" `elem` words detail -> pure ()
    unexpected -> assertFailure ("expected frame handle owner rejection, got " <> showPresentResult unexpected)
  assertDestructionCompletes "buffer after frame handle prevalidation" (destroyBuffer buffer)
  assertDestructionCompletes "image after frame handle prevalidation" (destroyImage image)

rejectedSubmissionTest :: IO ()
rejectedSubmissionTest = withHeadlessSwapchain $ \context swapchain -> do
  source <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.CopySrc] Word32)
  destination <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.CopyDst] Word32)
  writeBuffer source 0 [11]
  before <- currentTimelineValueForTest (graphicsQueue context)
  result <-
    try $
      frameWithSubmissionHooksForTest
        (const (pure (QueueSubmitRejected (toException SurfaceLost))))
        (const (pure ()))
        swapchain
        (\_ -> copyPass source 0 destination 0 1)
  result @?= Left SurfaceLost
  currentTimelineValueForTest (graphicsQueue context) >>= (@?= before + 1)
  reservationProbe <- timeout 2_000_000 $ do
    writeBuffer source 0 [12]
    writeBuffer destination 0 [13]
  reservationProbe @?= Just ()
  retry <- timeout 2_000_000 (frame swapchain (\_ -> copyPass source 0 destination 0 1))
  maybe (assertFailure "frame after rejected submission did not complete within two seconds") assertPresented retry
  assertDestructionCompletes "source buffer after rejected submission" (destroyBuffer source)
  assertDestructionCompletes "destination buffer after rejected submission" (destroyBuffer destination)

unknownSubmissionTest :: IO ()
unknownSubmissionTest = do
  contextOpened <- newIORef False
  failingHandleQuarantined <- newEmptyMVar
  laterHandleQuarantined <- newEmptyMVar
  leaseReleased <- newEmptyMVar
  withHeadlessSwapchain $ \context swapchain -> do
    writeIORef contextOpened True
    source <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.CopySrc] Word32)
    destination <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.CopyDst] Word32)
    sourceMetadata <-
      maybe
        (throwIO (VulkanFailure "unknown submission test" "the source buffer has no managed metadata"))
        pure
        (Resource.runtimeBufferMetadata (Resource.bufferRuntimeHandle source))
    failingGeneration <- Lifetime.newResourceGeneration
    laterGeneration <- Lifetime.newResourceGeneration
    let failingHandle =
          Resource.managedBufferRuntimeHandleWithQuarantine
            (contextIdentity context)
            failingGeneration
            (pure (void (tryPutMVar leaseReleased ())))
            (void (tryPutMVar failingHandleQuarantined ()) >> throwIO DeviceLost)
            sourceMetadata
        laterHandle =
          Resource.managedBufferRuntimeHandleWithQuarantine
            (contextIdentity context)
            laterGeneration
            (pure (pure ()))
            (void (tryPutMVar laterHandleQuarantined ()))
            sourceMetadata
    trackedCommand <- newFrameCommand [failingHandle, laterHandle] [] [] [] (\_ _ -> pure ())
    before <- currentTimelineValueForTest (graphicsQueue context)
    result <-
      timeout
        2_000_000
        ( try
            ( frameWithSubmissionHooksForTest
                (\submitToVulkan -> submitToVulkan >> pure (QueueSubmitAcceptanceUnknown (toException SurfaceLost)))
                (const (pure ()))
                swapchain
                ( \_ -> do
                    passActionForTest (pure [trackedCommand])
                    copyPass source 0 destination 0 1
                )
            ) ::
            IO (Either VpipeError PresentResult)
        )
    result @?= Just (Left SurfaceLost)
    currentTimelineValueForTest (graphicsQueue context) >>= (@?= before)
    tryReadMVar failingHandleQuarantined >>= (@?= Just ())
    tryReadMVar laterHandleQuarantined >>= (@?= Just ())
    tryReadMVar leaseReleased >>= (@?= Nothing)

    retry <- timeout 2_000_000 (try (frame swapchain (const (pure ()))) :: IO (Either VpipeError PresentResult))
    retry @?= Just (Left SwapchainPoisoned)
    assertPromptResourceQuarantine "source state reuse" (writeBuffer source 0 [12])
    assertPromptResourceQuarantine "destination state reuse" (writeBuffer destination 0 [13])
    assertPromptResourceQuarantine "source destruction" (destroyBuffer source)
    assertPromptResourceQuarantine "destination destruction" (destroyBuffer destination)
  opened <- readIORef contextOpened
  when opened (timeout 2_000_000 (takeMVar leaseReleased) >>= (@?= Just ()))

assertPromptResourceQuarantine :: String -> IO () -> IO ()
assertPromptResourceQuarantine description action = do
  result <- timeout 2_000_000 (try action :: IO (Either VpipeError ()))
  case result of
    Just (Left ResourceQuarantined) -> pure ()
    Just outcome -> assertFailure (description <> " returned " <> show outcome <> " instead of ResourceQuarantined")
    Nothing -> assertFailure (description <> " did not fail promptly")

publicationFailureTest :: IO ()
publicationFailureTest = withHeadlessSwapchain $ \context swapchain -> do
  source <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.CopySrc] Word32)
  destination <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.CopyDst] Word32)
  writeBuffer source 0 [21]
  before <- currentTimelineValueForTest (graphicsQueue context)
  result <-
    try $
      frameWithSubmissionHooksForTest
        (\submitToVulkan -> submitToVulkan >> pure QueueSubmitAccepted)
        (const (throwIO SurfaceLost))
        swapchain
        (\_ -> copyPass source 0 destination 0 1)
  result @?= Left SurfaceLost
  currentTimelineValueForTest (graphicsQueue context) >>= (@?= before + 1)
  reservationProbe <- timeout 2_000_000 $ do
    writeBuffer source 0 [22]
    writeBuffer destination 0 [23]
  reservationProbe @?= Just ()
  retry <- timeout 2_000_000 (frame swapchain (\_ -> copyPass source 0 destination 0 1))
  maybe (assertFailure "frame after accepted publication failure did not complete within two seconds") assertPresented retry
  assertDestructionCompletes "source buffer after accepted publication failure" (destroyBuffer source)
  assertDestructionCompletes "destination buffer after accepted publication failure" (destroyBuffer destination)

submittedResourceLifetimeTest :: IO ()
submittedResourceLifetimeTest = withHeadlessSwapchain $ \context swapchain -> do
  preparedCompute <- prepareMixedCompute context
  preparedGraphics <- prepareMixedGraphics context
  buffer <- newBuffer context 3 :: IO (Buffer '[ 'Buffer.Storage, 'Buffer.Vertex] (V4 Float))
  writeBuffer buffer 0 [V4 (-0.8) (-0.8) 0 1, V4 0.8 (-0.8) 0 1, V4 0 0.8 0 1]
  dynamic <- newFrameDynamicBuffer swapchain 3 :: IO (FrameDynamicBuffer '[ 'Buffer.Storage] (V4 Float))
  image <- newImage context (imageExtent2D 16 16) 1 1 :: IO (Image 'ImageTypes.D2 'B8G8R8A8Srgb '[ 'ImageTypes.ColorTarget])
  target <- Pipeline.colorImageBinding image
  let bufferEnvironment =
        MixedEnvironment
          { mixedStorage = Pipeline.storageBufferBinding buffer
          , mixedVertices = Pipeline.vertexBufferBinding buffer
          , mixedTarget = target
          }
      dynamicValues = [V4 0 0 0 1, V4 0 0 0 1, V4 0 0 0 1]
  assertPresented
    =<< frame
      swapchain
      ( \_ -> do
          computePass preparedCompute bufferEnvironment (3, 1, 1)
          withDynamicStorage dynamic 0 dynamicValues $ \dynamicStorage ->
            computePass preparedCompute bufferEnvironment{mixedStorage = dynamicStorage} (3, 1, 1)
          renderTo target (render preparedGraphics bufferEnvironment)
      )
  bufferDestroyed <- newEmptyMVar
  dynamicDestroyed <- newEmptyMVar
  imageDestroyed <- newEmptyMVar
  _ <- forkIO ((try (destroyBuffer buffer) :: IO (Either VpipeError ())) >>= putMVar bufferDestroyed)
  _ <- forkIO ((try (destroyFrameDynamicBuffer dynamic) :: IO (Either VpipeError ())) >>= putMVar dynamicDestroyed)
  _ <- forkIO ((try (destroyImage image) :: IO (Either VpipeError ())) >>= putMVar imageDestroyed)
  timeout 2_000_000 (takeMVar bufferDestroyed) >>= (@?= Just (Right ()))
  timeout 2_000_000 (takeMVar dynamicDestroyed) >>= (@?= Just (Right ()))
  timeout 2_000_000 (takeMVar imageDestroyed) >>= (@?= Just (Right ()))

assertDestructionCompletes :: String -> IO () -> IO ()
assertDestructionCompletes description destroy = do
  completed <- newEmptyMVar
  _ <- forkIO ((try destroy :: IO (Either VpipeError ())) >>= putMVar completed)
  result <- timeout 2_000_000 (takeMVar completed)
  case result of
    Nothing -> assertFailure (description <> " did not complete within two seconds")
    Just outcome -> outcome @?= Right ()

assertPresented :: PresentResult -> IO ()
assertPresented result = case result of
  Presented{} -> pure ()
  PresentDeferred reason -> assertFailure ("headless frame unexpectedly deferred: " <> show reason)

showPresentResult :: Either VpipeError PresentResult -> String
showPresentResult result = case result of
  Left error' -> show error'
  Right (Presented extent recreated) -> "Right (Presented " <> show extent <> " " <> show recreated <> ")"
  Right (PresentDeferred reason) -> "Right (PresentDeferred " <> show reason <> ")"

frameTestConfig :: IO VpipeConfig
frameTestConfig = do
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  pure defaultVpipeConfig{vpipeValidationStrict = requested == Just "lavapipe"}

withHeadlessSwapchain :: (Context -> Swapchain -> IO ()) -> IO ()
withHeadlessSwapchain action = do
  config <- frameTestConfig
  let factory =
        mkSurfaceFactoryWithExtents
          [Surface.KHR_SURFACE_EXTENSION_NAME, Headless.EXT_HEADLESS_SURFACE_EXTENSION_NAME]
          ( \instanceHandle -> do
              rawSurface <- Headless.createHeadlessSurfaceEXT instanceHandle zero Nothing
              pure ((), (rawSurface, pure (64, 64)) :| [])
          )
          (const (pure ()))
  result <-
    try $
      withVpipeSurfaces config factory $ \context (surface :| _) () ->
        bracket (newSwapchain context surface defaultSwapchainConfig) destroySwapchain (action context)
  case result of
    Left (NoVulkanIcd detail)
      | vpipeValidationStrict config -> throwIO (NoVulkanIcd detail)
      | otherwise -> putStrLn ("SKIP: Vulkan ICD unavailable: " <> detail)
    Left (RequiredInstanceExtensionsUnavailable missing)
      | vpipeValidationStrict config -> throwIO (RequiredInstanceExtensionsUnavailable missing)
      | otherwise -> putStrLn ("SKIP: headless surface extensions unavailable: " <> show missing)
    Left (SwapchainFormatUnavailable formats) ->
      putStrLn ("SKIP: headless surface does not expose the required SRGB swapchain format: " <> show formats)
    Left error' -> throwIO (error' :: VpipeError)
    Right value -> pure value

descriptorPreparationOwnershipTest :: IO ()
descriptorPreparationOwnershipTest = do
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  let config = defaultVpipeConfig{vpipeValidationStrict = requested == Just "lavapipe"}
  withVpipe config $ \context -> do
    prepared <- prepareMixedCompute context
    rejectedFrame <- newDescriptorFrame (preparedComputeFrameDescriptorLayout prepared)
    rejectedBuffer <- newBuffer context 3 :: IO (Buffer '[ 'Buffer.Storage] (V4 Float))
    rejected <-
      try $
        withContextLease context $
          prepareComputeFrameCommandWithDescriptorHookLeased
            (destroyBuffer rejectedBuffer)
            prepared
            rejectedFrame
            (descriptorEnvironment rejectedBuffer)
            (1, 1, 1)
    case rejected of
      Left BufferReleased -> pure ()
      unexpected -> assertFailure ("expected closed descriptor resource rejection, got " <> showPreparedResult unexpected)
    rejectedStats <- descriptorFrameStats rejectedFrame
    descriptorWrites rejectedStats @?= 0

    descriptorFrame <- newDescriptorFrame (preparedComputeFrameDescriptorLayout prepared)
    buffer <- newBuffer context 3 :: IO (Buffer '[ 'Buffer.Storage] (V4 Float))
    preparedCommand <-
      withContextLease context $
        prepareComputeFrameCommandLeased
          prepared
          descriptorFrame
          (descriptorEnvironment buffer)
          (1, 1, 1)
    case preparedCommand of
      Nothing -> assertFailure "nonzero descriptor preparation skipped its command"
      Just _ -> pure ()
    stats <- descriptorFrameStats descriptorFrame
    descriptorWrites stats @?= 1
    destroyed <- newEmptyMVar
    _ <- forkIO ((try (destroyBuffer buffer) :: IO (Either VpipeError ())) >>= putMVar destroyed)
    result <- timeout 100_000 (takeMVar destroyed)
    result @?= Just (Right ())
    descriptorFrameStats descriptorFrame >>= (@?= stats)

showPreparedResult :: Either VpipeError (Maybe FrameCommand) -> String
showPreparedResult result = case result of
  Left error' -> show error'
  Right Nothing -> "Right Nothing"
  Right (Just _) -> "Right (Just <command>)"

descriptorEnvironment :: Buffer '[ 'Buffer.Storage] (V4 Float) -> MixedEnvironment
descriptorEnvironment buffer =
  MixedEnvironment
    { mixedStorage = Pipeline.storageBufferBinding buffer
    , mixedVertices = Pipeline.VertexBuffer (Pipeline.RuntimeHandle 901)
    , mixedTarget = Pipeline.ColorImage (Pipeline.RuntimeHandle 902)
    }

data MixedEnvironment = MixedEnvironment
  { mixedStorage :: Pipeline.StorageBuffer (V4 Float)
  , mixedVertices :: Pipeline.VertexBuffer (V4 Float)
  , mixedTarget :: Pipeline.ColorImage 'B8G8R8A8Srgb
  }

mixedCompute :: ComputeM MixedEnvironment ()
mixedCompute = do
  positions <- storageBuffer mixedStorage
  invocation <- globalInvocationId
  let index = globalInvocationX invocation
  whenInBounds positions index $ \position -> writeAt positions index position

mixedGraphics :: Pipeline.PipelineM MixedEnvironment ()
mixedGraphics = do
  positions <-
    Pipeline.vertexInput
      (Pipeline.vertexSource "positions" mixedVertices :: Pipeline.VertexSource MixedEnvironment 'Pipeline.Triangles (V4 Float))
  fragments <-
    Pipeline.rasterize
      Pipeline.defaultRaster
      (fmap (,Pipeline.Smooth (Expr.constant (0 :: Float) :: Expr.V Float)) positions)
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "swapchain" mixedTarget)
    (fmap (const (Expr.vec4 (Expr.constant 1) (Expr.constant 0) (Expr.constant 0) (Expr.constant 1))) fragments)

liveMixedFrameTest :: IO ()
liveMixedFrameTest = do
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  let config =
        defaultVpipeConfig
          { vpipeValidationStrict = requested == Just "lavapipe"
          , vpipeLogger = print
          }
      factory =
        mkSurfaceFactoryWithExtents
          [Surface.KHR_SURFACE_EXTENSION_NAME, Headless.EXT_HEADLESS_SURFACE_EXTENSION_NAME]
          ( \instanceHandle -> do
              rawSurface <- Headless.createHeadlessSurfaceEXT instanceHandle zero Nothing
              pure ((), (rawSurface, pure (64, 64)) :| [])
          )
          (const (pure ()))
      exercise =
        withVpipeSurfaces config factory $ \context (surface :| _) () -> do
          preparedCompute <- prepareMixedCompute context
          preparedGraphics <- prepareMixedGraphics context
          positions <-
            newBuffer context 3 ::
              IO
                ( Buffer
                    '[ 'Buffer.Storage, 'Buffer.Vertex]
                    (V4 Float)
                )
          writeBuffer
            positions
            0
            [ V4 (-0.8) (-0.8) 0 1
            , V4 0.8 (-0.8) 0 1
            , V4 0 0.8 0 1
            ]
          recoveryBuffer <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Storage] (V4 Float))
          writeBuffer recoveryBuffer 0 [V4 0 0 0 0]
          swapchain <- newSwapchain context surface defaultSwapchainConfig
          beforeRecovery <- currentTimelineValueForTest (graphicsQueue context)
          recoveryResult <-
            try $
              frame swapchain $ \current ->
                let environment =
                      MixedEnvironment
                        { mixedStorage = Pipeline.storageBufferBinding recoveryBuffer
                        , mixedVertices = Pipeline.VertexBuffer (Pipeline.RuntimeHandle 903)
                        , mixedTarget = frameColorTarget current
                        }
                 in computePass preparedCompute environment (1, 1, 1)
                      >> passActionForTest (throwIO SurfaceLost)
          recoveryResult @?= Left SurfaceLost
          destroyBuffer recoveryBuffer
          afterRecovery <- currentTimelineValueForTest (graphicsQueue context)
          afterRecovery @?= beforeRecovery + 1
          replicateM_ 3 $ do
            before <- currentTimelineValueForTest (graphicsQueue context)
            result <-
              frame swapchain $ \current ->
                let target = frameColorTarget current
                    environment =
                      MixedEnvironment
                        { mixedStorage = Pipeline.storageBufferBinding positions
                        , mixedVertices = Pipeline.vertexBufferBinding positions
                        , mixedTarget = target
                        }
                 in computePassFor preparedCompute environment (3, 1, 1)
                      >> renderTo target (render preparedGraphics environment)
            case result of
              Presented extent _ -> extent @?= (64, 64)
              PresentDeferred reason -> assertFailure ("headless frame unexpectedly deferred: " <> show reason)
            after <- currentTimelineValueForTest (graphicsQueue context)
            after @?= before + 1
          destroySwapchain swapchain
  result <- try exercise :: IO (Either VpipeError ())
  case result of
    Left (NoVulkanIcd detail)
      | requested /= Just "lavapipe" -> putStrLn ("SKIP: Vulkan ICD unavailable: " <> detail)
    Left (RequiredInstanceExtensionsUnavailable missing)
      | requested /= Just "lavapipe" -> putStrLn ("SKIP: headless surface extensions unavailable: " <> show missing)
    Left (SwapchainFormatUnavailable formats) ->
      putStrLn ("SKIP: headless surface does not expose the required SRGB swapchain format: " <> show formats)
    Left error' -> throwIO error'
    Right () -> pure ()

prepareMixedCompute :: Context -> IO (PreparedCompute MixedEnvironment 64 1 1)
prepareMixedCompute context = do
  compiled <- compileCompute (Dispatch @64 @1 @1) mixedCompute >>= either (assertFailure . show) pure
  runtime <- newComputeRuntime context
  prepareComputePipeline runtime compiled

prepareMixedGraphics :: Context -> IO (PreparedGraphicsPipeline MixedEnvironment)
prepareMixedGraphics context = do
  compiled <- Pipeline.compilePipeline mixedGraphics >>= either (assertFailure . show) pure
  runtime <- newGraphicsRuntime context
  prepareGraphicsPipeline runtime compiled

showResult :: Either VpipeError [FrameCommand] -> String
showResult result = case result of
  Left error' -> show error'
  Right commands -> "Right <" <> show (length commands) <> " commands>"
