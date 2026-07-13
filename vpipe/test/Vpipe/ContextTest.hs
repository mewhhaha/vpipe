{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vpipe.ContextTest (contextPureTests, contextTests) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, tryReadMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, bracket, fromException, throwIO, toException, try)
import Control.Monad (forM_, void)
import Data.ByteString qualified as ByteString
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Vector qualified as Vector
import Data.Word (Word32, Word64)
import System.Directory (createDirectory, getTemporaryDirectory, listDirectory, removeFile, removePathForcibly)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, openBinaryTempFile)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Vulkan.Core10.Device qualified as Device
import Vulkan.Core10.DeviceInitialization qualified as Vk
import Vulkan.Core10.Enums.PhysicalDeviceType qualified as DeviceType
import Vulkan.Core10.Enums.QueueFlagBits qualified as QueueFlags
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Exception qualified as Vulkan
import Vulkan.Zero (zero)

import Vpipe.Context (
  DebugMessage (..),
  StructuredLog (logMessageId),
  VpipeConfig (vpipeLogicalDeviceBuilder, vpipeValidationStrict),
  contextDeviceIsCpu,
  contextDeviceName,
  debugSinkDropped,
  defaultVpipeConfig,
  freeDebugSink,
  graphicsQueue,
  newDebugSink,
  popDebugMessage,
  withVpipe,
 )
import Vpipe.Context.Device (CandidateDevice (..), chooseDevice, createLogicalDeviceWith, defaultLogicalDeviceBuilder, deviceRejectionReasons, familyWith, selectedDevice)
import Vpipe.Context.Internal (ContextRelease (..), finishSurfaceContextRelease, reportPendingValidationMessages, resolveManagedOutcome, runCleanupWithOuterForTest, runContextShutdownForTest, runFinalizersForTest, runManagedWithOuterCleanupForTest, testDebugSinkCallback)
import Vpipe.Context.Queue (
  Queue,
  submitEmpty,
  timelineCompletedValue,
  waitTimeline,
 )
import Vpipe.Context.Queue.Internal (LifecycleGate, closeLifecycleGate, currentTimelineValueForTest, exerciseBinarySemaphoreChainForTest, exerciseQueueDependencyForTest, lifecycleGateClosed, newLifecycleGate, nextTimelineSignalForTest, submitCommandBuffers, withLifecycleLease)
import Vpipe.Error (VpipeError (..), rejectedReasons)
import Vpipe.Graphics.Cache.Internal (GraphicsCache, newGraphicsCacheWithPipelineCache)
import Vpipe.Graphics.Cache.Persistence (pipelineCachePath, readPipelineCacheFile, writePipelineCacheFile)
import Vpipe.Resource.Lifetime qualified as Lifetime

contextTests :: TestTree
contextTests =
  testGroup
    "context"
    (contextPureCases <> [testCase "headless context submits and advances its timeline" liveContextTest])

contextPureTests :: TestTree
contextPureTests = testGroup "context" contextPureCases

contextPureCases :: [TestTree]
contextPureCases =
  [ testCase "chooseDevice prefers the highest scoring compatible candidate" $ do
      let rejected = candidate "missing" 400 ["dynamicRendering is unavailable"]
          accepted = candidate "lavapipe" 100 []
      case chooseDevice candidateScore [rejected, accepted] of
        Right selection -> candidateName (selectedDevice selection) @?= "lavapipe"
        Left rejections -> assertFailure ("expected a compatible candidate, got " <> show rejections)
  , testCase "chooseDevice keeps the first compatible candidate when scores tie" $ do
      let first = candidate "first" 100 []
          second = candidate "second" 100 []
      case chooseDevice candidateScore [first, second] of
        Right selection -> candidateName (selectedDevice selection) @?= "first"
        Left rejections -> assertFailure ("expected a compatible candidate, got " <> show rejections)
  , testCase "device inspection retains version feature queue and extension rejections" $ do
      deviceRejectionReasons 0 False False False False ["VK_TEST_missing"]
        @?= [ "Vulkan API version is below 1.3"
            , "dynamicRendering is unavailable"
            , "synchronization2 is unavailable"
            , "timelineSemaphore is unavailable"
            , "no graphics-capable queue family"
            , "required device extension is unavailable: VK_TEST_missing"
            ]
  , testCase "queue families with zero queues are rejected" $ do
      let unavailable =
            (zero :: Vk.QueueFamilyProperties)
              { Vk.queueFlags = QueueFlags.QUEUE_GRAPHICS_BIT
              , Vk.queueCount = 0
              }
      familyWith QueueFlags.QUEUE_GRAPHICS_BIT [(0, unavailable)] @?= Nothing
  , testCase "typed device creation rejects a missing graphics family before creating a handle" $ do
      let invalid = (candidate "invalid" 0 []){candidateGraphicsFamily = Nothing}
      result <- try (createLogicalDeviceWith invalid (zero :: Device.DeviceCreateInfo '[]))
      case result of
        Left NoSuitableDevice{} -> pure ()
        Left error' -> assertFailure ("expected NoSuitableDevice before vkCreateDevice, got " <> show error')
        Right _ -> assertFailure "expected NoSuitableDevice before vkCreateDevice, but a device was created"
  , testCase "chooseDevice preserves each candidate rejection" $ do
      let rejected = candidate "old driver" 0 ["timelineSemaphore is unavailable"]
      case chooseDevice candidateScore [rejected] of
        Left [rejection] -> rejectedReasons rejection @?= ["timelineSemaphore is unavailable"]
        result -> assertFailure ("expected exactly one rejected candidate, got " <> showSelection result)
  , testCase "lifecycle close waits for active work and rejects retained handles" lifecycleGateTest
  , testCase "lifetime sealing is nonblocking while close still drains active leases" lifetimeSealTest
  , testCase "lifetime quarantine wakes explicit close and rejects later leases" lifetimeQuarantineTest
  , testCase "context cleanup seals resources without waiting for later lease finalizers" nonblockingResourceFinalizerTest
  , testCase "successful device idle closes context resources" successfulDeviceIdleCleanupTest
  , testCase "device loss closes context resources and remains the primary failure" deviceLostCleanupTest
  , testCase "uncertain device idle failure retains context resources" uncertainDeviceIdleCleanupTest
  , testCase "uncertain surfaced shutdown retains payload instance and validation ownership" retainedSurfaceOwnershipTest
  , testCase "successful surfaced shutdown releases ownership before final validation" successfulSurfaceOwnershipTest
  , testCase "surfaced cleanup failures preserve precedence and do not skip later stages" surfaceCleanupFailureContinuationTest
  , testCase "a surfaced action exception wins after every released cleanup stage" surfacedPrimaryExceptionTest
  , testCase "cleanup continues in LIFO order after a finalizer throws" cleanupContinuationTest
  , testCase "cancellation during context shutdown releases surfaced ownership after finalizers finish" cancellationSafeCleanupTest
  , testCase "a primary action exception wins while cleanup stages run exactly once" primaryExceptionTest
  , testCase "timeline allocation detects exhaustion and device difference limits" timelineLimitTest
  , testCase "the C callback sink preserves fields and counts overflow" debugSinkTest
  , testCase "pipeline cache files encode UUIDs and replace their contents atomically" pipelineCacheFileTest
  , testCase "a restarted pipeline cache constructor receives the persisted bytes" pipelineCacheRestartInputTest
  , testCase "pipeline cache creation propagates Vulkan failures" pipelineCacheCreationFailureTest
  ]

candidate :: String -> Int -> [String] -> CandidateDevice
candidate name score reasons =
  CandidateDevice
    { candidateHandle = zero
    , candidateName = name
    , candidateDeviceType = DeviceType.PHYSICAL_DEVICE_TYPE_CPU
    , candidateScore = score
    , candidateRejection = reasons
    , candidateGraphicsFamily = Just (0 :: Word32)
    , candidateComputeFamily = Just 0
    , candidateTransferFamily = Just 0
    , candidatePresentFamilies = []
    , candidateEnabledExtensions = []
    , candidateMaxTimelineDifference = maxBound
    , candidateSamplerAnisotropy = True
    }

showSelection :: Either a b -> String
showSelection (Left _) = "a different rejection set"
showSelection (Right _) = "a selected device"

pipelineCacheFileTest :: IO ()
pipelineCacheFileTest = do
  temporaryDirectory <- getTemporaryDirectory
  (directory, handle) <- openBinaryTempFile temporaryDirectory "vpipe-pipeline-cache-test"
  hClose handle
  removeFile directory
  createDirectory directory
  bracket (pure directory) removePathForcibly $ \cacheDirectory -> do
    let uuid = ByteString.pack ([0x00, 0x0f, 0xff] <> replicate 13 0)
        path = pipelineCachePath cacheDirectory uuid
    path @?= cacheDirectory </> "vpipe" </> "pipeline-cache" </> ("000fff" <> replicate 26 '0' <> ".bin")
    readPipelineCacheFile path >>= (@?= ByteString.empty)
    writePipelineCacheFile path "pipeline cache bytes"
    readPipelineCacheFile path >>= (@?= "pipeline cache bytes")
    listDirectory (takeDirectory path) >>= (@?= [takeFileName path])

pipelineCacheRestartInputTest :: IO ()
pipelineCacheRestartInputTest = do
  temporaryDirectory <- getTemporaryDirectory
  (directory, handle) <- openBinaryTempFile temporaryDirectory "vpipe-pipeline-cache-restart-test"
  hClose handle
  removeFile directory
  createDirectory directory
  bracket (pure directory) removePathForcibly $ \cacheDirectory -> do
    let path = pipelineCachePath cacheDirectory (ByteString.replicate 16 0x42)
        persistedBytes = "first context pipeline cache"
    writePipelineCacheFile path persistedBytes
    restartedBytes <- readPipelineCacheFile path
    receivedBytes <- newEmptyMVar
    _ <-
      newGraphicsCacheWithPipelineCache
        (zero :: Handles.Device)
        (\bytes -> putMVar receivedBytes bytes >> pure (Handles.PipelineCache 0))
        restartedBytes
        (\_ _ _ -> pure ())
    takeMVar receivedBytes >>= (@?= persistedBytes)

pipelineCacheCreationFailureTest :: IO ()
pipelineCacheCreationFailureTest =
  forM_
    [ Result.ERROR_OUT_OF_HOST_MEMORY
    , Result.ERROR_OUT_OF_DEVICE_MEMORY
    , Result.ERROR_DEVICE_LOST
    , Result.ERROR_UNKNOWN
    ]
    $ \expectedResult -> do
      result <-
        try
          ( newGraphicsCacheWithPipelineCache
              (zero :: Handles.Device)
              (\_ -> throwIO (Vulkan.VulkanException expectedResult))
              "persisted pipeline cache"
              (\_ _ _ -> pure ())
          )
      case result :: Either Vulkan.VulkanException GraphicsCache of
        Left actualException -> Vulkan.vulkanExceptionResult actualException @?= expectedResult
        Right _ -> assertFailure ("expected vkCreatePipelineCache to propagate " <> show expectedResult)

lifecycleGateTest :: IO ()
lifecycleGateTest = do
  gate <- newLifecycleGate
  started <- newEmptyMVar
  releaseWork <- newEmptyMVar
  workFinished <- newEmptyMVar
  closeFinished <- newEmptyMVar
  _ <- forkIO $ withLifecycleLease gate (putMVar started () >> takeMVar releaseWork) >> putMVar workFinished ()
  takeMVar started
  _ <- forkIO $ void (closeLifecycleGate gate) >> putMVar closeFinished ()
  waitForClosed gate 1000
  tryReadMVar closeFinished >>= (@?= Nothing)
  putMVar releaseWork ()
  takeMVar workFinished
  takeMVar closeFinished
  result <- try (withLifecycleLease gate (pure ()))
  result @?= Left ContextClosed

lifetimeSealTest :: IO ()
lifetimeSealTest = do
  gate <- Lifetime.newLifetimeGate
  release <-
    Lifetime.acquireLifetimeLease gate
      >>= maybe (assertFailure "new lifetime gate rejected its first lease") pure
  timeout 100_000 (Lifetime.sealLifetimeGate gate)
    >>= maybe (assertFailure "sealing waited for an active lease") pure
  Lifetime.acquireLifetimeLease gate >>= \case
    Nothing -> pure ()
    Just releaseUnexpected -> releaseUnexpected >> assertFailure "sealed lifetime gate accepted a new lease"
  closeFinished <- newEmptyMVar
  _ <- forkIO (Lifetime.closeLifetimeGate gate >> putMVar closeFinished ())
  threadDelay 10_000
  tryReadMVar closeFinished >>= (@?= Nothing)
  release
  timeout 100_000 (takeMVar closeFinished)
    >>= maybe (assertFailure "close did not finish after the active lease drained") pure

lifetimeQuarantineTest :: IO ()
lifetimeQuarantineTest = do
  gate <- Lifetime.newLifetimeGate
  release <-
    Lifetime.acquireLifetimeLease gate
      >>= maybe (assertFailure "new lifetime gate rejected its first lease") pure
  Lifetime.sealLifetimeGate gate
  rawDestroyed <- newEmptyMVar
  closeResult <- newEmptyMVar
  _ <- forkIO $ do
    result <- try (Lifetime.closeLifetimeGate gate >> putMVar rawDestroyed ())
    putMVar closeResult (result :: Either VpipeError ())
  threadDelay 10_000
  tryReadMVar closeResult >>= (@?= Nothing)
  Lifetime.quarantineLifetimeGate gate
  timeout 100_000 (takeMVar closeResult)
    >>= maybe (assertFailure "quarantine did not wake explicit close") (@?= Left ResourceQuarantined)
  tryReadMVar rawDestroyed >>= (@?= Nothing)
  acquired <- try (Lifetime.acquireLifetimeLease gate)
  case acquired :: Either VpipeError (Maybe (IO ())) of
    Left ResourceQuarantined -> pure ()
    Left error' -> assertFailure ("expected ResourceQuarantined, got " <> show error')
    Right Nothing -> assertFailure "quarantined lifetime looked normally sealed"
    Right (Just releaseUnexpected) -> releaseUnexpected >> assertFailure "quarantined lifetime accepted a lease"
  -- Context finalizers run only after context-level queue quiescence.
  Lifetime.sealLifetimeGate gate
  release

nonblockingResourceFinalizerTest :: IO ()
nonblockingResourceFinalizerTest = do
  gate <- Lifetime.newLifetimeGate
  release <-
    Lifetime.acquireLifetimeLease gate
      >>= maybe (assertFailure "new lifetime gate rejected its first lease") pure
  rawDestroyed <- newEmptyMVar
  cleanup <-
    timeout
      100_000
      ( runFinalizersForTest
          [ release
          , Lifetime.sealLifetimeGate gate >> putMVar rawDestroyed ()
          ]
      )
  case cleanup of
    Nothing -> assertFailure "sealed resource finalizer blocked on an active lease"
    Just failures -> length failures @?= 0
  tryReadMVar rawDestroyed >>= (@?= Just ())
  Lifetime.acquireLifetimeLease gate >>= \case
    Nothing -> pure ()
    Just releaseUnexpected -> releaseUnexpected >> assertFailure "context-finalized lifetime accepted a new lease"

waitForClosed :: LifecycleGate -> Int -> IO ()
waitForClosed gate remaining
  | remaining == 0 = assertFailure "lifecycle gate did not begin closing"
  | otherwise = do
      closed <- lifecycleGateClosed gate
      if closed then pure () else threadDelay 1000 >> waitForClosed gate (remaining - 1)

successfulDeviceIdleCleanupTest :: IO ()
successfulDeviceIdleCleanupTest = do
  events <- newMVar ([] :: [String])
  let record event = modifyMVar_ events (pure . (<> [event]))
  runContextShutdownForTest (record "device idle") (record "resource cleanup")
  readMVar events >>= (@?= ["device idle", "resource cleanup"])

deviceLostCleanupTest :: IO ()
deviceLostCleanupTest = do
  events <- newMVar ([] :: [String])
  let record event = modifyMVar_ events (pure . (<> [event]))
  result <-
    try
      ( runContextShutdownForTest
          (record "device lost" >> throwIO DeviceLost)
          (record "resource cleanup" >> throwIO (CleanupFailed ["cleanup failed"]))
      )
  result @?= Left DeviceLost
  readMVar events >>= (@?= ["device lost", "resource cleanup"])

uncertainDeviceIdleCleanupTest :: IO ()
uncertainDeviceIdleCleanupTest = do
  resourceCleanupRan <- newEmptyMVar
  result <-
    try
      ( runContextShutdownForTest
          (throwIO (VulkanFailure "injected vkDeviceWaitIdle" "ERROR_OUT_OF_HOST_MEMORY"))
          (putMVar resourceCleanupRan ())
      )
  case result of
    Left (CleanupFailed [failure]) -> do
      assertBool "failure must identify the idle operation" ("test device idle wait" `isInfixOf` failure)
      assertBool "failure must state that resources were retained" ("resources were retained" `isInfixOf` failure)
      assertBool "failure must include the Vulkan cause" ("ERROR_OUT_OF_HOST_MEMORY" `isInfixOf` failure)
    Left error' -> assertFailure ("expected an uncertain-shutdown CleanupFailed, got " <> show error')
    Right () -> assertFailure "expected an uncertain device idle failure"
  tryReadMVar resourceCleanupRan >>= (@?= Nothing)

retainedSurfaceOwnershipTest :: IO ()
retainedSurfaceOwnershipTest = do
  events <- newMVar ([] :: [String])
  let record event = modifyMVar_ events (pure . (<> [event]))
      retainedFailure = toException (CleanupFailed ["injected uncertain shutdown"])
  cleanupResult <-
    finishSurfaceContextRelease
      (ContextResourcesRetained retainedFailure)
      (record "payload" >> record "window" >> record "terminate")
      (record "instance")
      (record "final validation")
  case cleanupResult of
    Left actualFailure -> show actualFailure @?= show retainedFailure
    Right () -> assertFailure "expected retained surfaced shutdown to fail"
  readMVar events >>= (@?= [])

successfulSurfaceOwnershipTest :: IO ()
successfulSurfaceOwnershipTest = do
  events <- newMVar ([] :: [String])
  let record event = modifyMVar_ events (pure . (<> [event]))
      postInstanceMessage = DebugMessage 256 1 "VUID-post-instance" "reported after vkDestroyInstance"
      reportPostInstanceValidation =
        reportPendingValidationMessages
          False
          (pure ([postInstanceMessage], 0))
          (record . ("validation:" <>) . logMessageId)
  cleanupResult <-
    finishSurfaceContextRelease
      (ContextResourcesReleased (Right ()))
      (record "payload" >> record "window-1" >> record "window-2" >> record "terminate")
      (record "instance")
      reportPostInstanceValidation
  case cleanupResult of
    Left error' -> assertFailure ("expected surfaced cleanup to succeed, got " <> show error')
    Right () -> pure ()
  readMVar events
    >>= (@?= ["payload", "window-1", "window-2", "terminate", "instance", "validation:VUID-post-instance"])

surfaceCleanupFailureContinuationTest :: IO ()
surfaceCleanupFailureContinuationTest = do
  events <- newMVar ([] :: [String])
  let record event = modifyMVar_ events (pure . (<> [event]))
      contextFailure = toException (CleanupFailed ["logger failed before payload release"])
  cleanupResult <-
    finishSurfaceContextRelease
      (ContextResourcesReleased (Left contextFailure))
      (record "payload" >> throwIO (CleanupFailed ["payload failed"]))
      (record "instance" >> throwIO (CleanupFailed ["instance failed"]))
      (record "post-instance validation" >> throwIO (CleanupFailed ["logger failed"]))
  case cleanupResult of
    Left actualFailure -> fromException actualFailure @?= Just (CleanupFailed ["logger failed before payload release"])
    Right () -> assertFailure "expected the first surfaced cleanup failure"
  readMVar events >>= (@?= ["payload", "instance", "post-instance validation"])

surfacedPrimaryExceptionTest :: IO ()
surfacedPrimaryExceptionTest = do
  events <- newMVar ([] :: [String])
  let record event = modifyMVar_ events (pure . (<> [event]))
  outcome <- try (throwIO DeviceLost :: IO ())
  cleanupResult <-
    finishSurfaceContextRelease
      (ContextResourcesReleased (Left (toException (ValidationFailed 1 0))))
      (record "payload" >> throwIO (CleanupFailed ["payload failed"]))
      (record "instance")
      (record "post-instance validation")
  result <- try (resolveManagedOutcome outcome cleanupResult) :: IO (Either SomeException ())
  case result of
    Left error' -> fromException error' @?= Just DeviceLost
    Right () -> assertFailure "expected the primary surfaced action failure"
  readMVar events >>= (@?= ["payload", "instance", "post-instance validation"])

cleanupContinuationTest :: IO ()
cleanupContinuationTest = do
  events <- newMVar ([] :: [String])
  let record name = modifyMVar_ events (pure . (<> [name]))
  failures <-
    runFinalizersForTest
      [ record "first"
      , record "second" >> throwIO (CleanupFailed ["second failed"])
      , record "third"
      ]
  readMVar events >>= (@?= ["third", "second", "first"])
  length failures @?= 1

cancellationSafeCleanupTest :: IO ()
cancellationSafeCleanupTest = do
  finalizerStarted <- newEmptyMVar
  releaseFinalizer <- newEmptyMVar
  finalizerFinished <- newEmptyMVar
  events <- newMVar ([] :: [String])
  waiterResult <- newEmptyMVar
  let record event = modifyMVar_ events (pure . (<> [event]))
  waiter <-
    forkIO $ do
      result <-
        try
          ( runCleanupWithOuterForTest
              (putMVar finalizerStarted () >> takeMVar releaseFinalizer >> record "resource finalizer" >> putMVar finalizerFinished ())
              (record "payload" >> record "window")
              (record "instance")
              (record "final validation")
          ) ::
          IO (Either SomeException ())
      putMVar waiterResult result
  takeMVar finalizerStarted
  killThread waiter
  readMVar events >>= (@?= [])
  putMVar releaseFinalizer ()
  takeMVar finalizerFinished
  result <- takeMVar waiterResult
  case result of
    Left error' -> fromException error' @?= Just ThreadKilled
    Right () -> assertFailure "cancellation was not delivered after cleanup"
  readMVar events >>= (@?= ["resource finalizer", "payload", "window", "instance", "final validation"])

primaryExceptionTest :: IO ()
primaryExceptionTest = do
  events <- newMVar ([] :: [String])
  let record event = modifyMVar_ events (pure . (<> [event]))
  result <-
    try
      ( runManagedWithOuterCleanupForTest
          (throwIO DeviceLost :: IO ())
          (record "resource cleanup" >> throwIO (CleanupFailed ["cleanup failed"]))
          (record "outer cleanup" >> throwIO ThreadKilled)
      ) ::
      IO (Either SomeException ())
  case result of
    Left error' -> fromException error' @?= Just DeviceLost
    Right () -> assertFailure "expected the primary action failure"
  readMVar events >>= (@?= ["resource cleanup", "outer cleanup"])

timelineLimitTest :: IO ()
timelineLimitTest = do
  nextTimelineSignalForTest 4 7 10 @?= Right 11
  nextTimelineSignalForTest 3 7 10 @?= Left (TimelineValueDifferenceExceeded 7 11 3)
  nextTimelineSignalForTest maxBound 0 maxBound @?= Left TimelineValueExhausted

debugSinkTest :: IO ()
debugSinkTest = bracket (newDebugSink 1) freeDebugSink $ \sink -> do
  let first = DebugMessage 16 2 "VUID-test" "validation text"
      overflow = DebugMessage 256 1 "overflow" "dropped"
  testDebugSinkCallback sink first
  testDebugSinkCallback sink overflow
  popDebugMessage sink >>= (@?= Just first)
  popDebugMessage sink >>= (@?= Nothing)
  debugSinkDropped sink >>= (@?= 1)

liveContextTest :: IO ()
liveContextTest = do
  builderCandidate <- newEmptyMVar
  selectedCandidate <- newEmptyMVar
  retainedQueue <- newEmptyMVar
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  case requested of
    Just "skip" -> putStrLn "SKIP: VPIPE_TEST_DEVICE=skip"
    Just unexpected
      | unexpected /= "any" && unexpected /= "lavapipe" ->
          assertFailure "VPIPE_TEST_DEVICE must be skip, any, or lavapipe"
    _ -> do
      let config =
            defaultVpipeConfig
              { vpipeValidationStrict = requested == Just "lavapipe"
              , vpipeLogicalDeviceBuilder = \candidate' -> do
                  putMVar builderCandidate (candidateName candidate')
                  defaultLogicalDeviceBuilder candidate'
              }
      result <- try $ withVpipe config $ \context -> do
        putMVar retainedQueue (graphicsQueue context)
        case requested of
          Just "lavapipe" -> do
            assertBool "lavapipe must expose a CPU physical device" (contextDeviceIsCpu context)
            let identity = map toLower (contextDeviceName context)
            assertBool
              ("expected lavapipe/llvmpipe, got " <> identity)
              ("lavapipe" `isInfixOf` identity || "llvmpipe" `isInfixOf` identity)
          _ -> pure ()
        putMVar selectedCandidate (contextDeviceName context)
        submitted <- submitEmpty (graphicsQueue context)
        submitted @?= 1
        currentTimelineValueForTest (graphicsQueue context) >>= (@?= 1)
        awaitTimeline (graphicsQueue context) submitted 1000
        (sourceSignal, dependentSignal) <- exerciseQueueDependencyForTest (graphicsQueue context)
        assertBool "dependency source signal must advance" (sourceSignal > submitted)
        dependentSignal @?= 1
        binaryChainSignal <- exerciseBinarySemaphoreChainForTest (graphicsQueue context)
        assertBool "binary semaphore chain must advance the timeline" (binaryChainSignal > submitted)
      case result of
        Left (NoVulkanIcd detail) | requested == Just "any" -> putStrLn ("SKIP: Vulkan ICD unavailable: " <> detail)
        Left error' -> throwIO (error' :: VpipeError)
        Right () -> do
          selectedName <- takeMVar selectedCandidate
          takeMVar builderCandidate >>= (@?= selectedName)
          queue <- takeMVar retainedQueue
          forM_
            [ void (submitEmpty queue)
            , void (submitCommandBuffers queue Vector.empty)
            , waitTimeline queue 0
            , void (timelineCompletedValue queue)
            ]
            assertContextClosed

assertContextClosed :: IO () -> IO ()
assertContextClosed action = do
  result <- try action
  result @?= Left ContextClosed

awaitTimeline :: Queue -> Word64 -> Int -> IO ()
awaitTimeline queue expected remaining
  | remaining == 0 = assertFailure ("timeline semaphore did not reach " <> show expected)
  | otherwise = do
      completed <- timelineCompletedValue queue
      if completed >= expected then pure () else threadDelay 1000 >> awaitTimeline queue expected (remaining - 1)
