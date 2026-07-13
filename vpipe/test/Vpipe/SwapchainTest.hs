{-# LANGUAGE LambdaCase #-}

module Vpipe.SwapchainTest (swapchainPureTests, swapchainTests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, tryReadMVar)
import Control.Exception (AsyncException (ThreadKilled), throwIO, try)
import Control.Monad (when)
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Vector qualified as Vector
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.CommandBuffer qualified as CommandBuffer
import Vulkan.Core10.Enums.Format qualified as Format
import Vulkan.Core10.Enums.ImageAspectFlagBits qualified as Aspect
import Vulkan.Core10.Enums.ImageLayout qualified as Layout
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.FundamentalTypes qualified as Fundamental
import Vulkan.Core10.ImageView qualified as ImageView
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 qualified as Sync2
import Vulkan.Exception qualified as Vulkan
import Vulkan.Extensions.VK_EXT_headless_surface qualified as Headless
import Vulkan.Extensions.VK_KHR_surface qualified as Surface
import Vulkan.Zero (zero)

import Vpipe.Context (VpipeConfig (..), defaultVpipeConfig)
import Vpipe.Descriptor.Internal (newDescriptorLayoutIdentityForTest)
import Vpipe.Error (VpipeError (..))
import Vpipe.Surface (withVpipeSurfaces)
import Vpipe.Surface.Driver (mkSurfaceFactoryWithExtents)
import Vpipe.Swapchain.Internal

swapchainTests :: TestTree
swapchainTests =
  testGroup
    "swapchain"
    [ swapchainPureTests
    , testCase "headless surface creates, recreates, and destroys a live swapchain" liveHeadlessSwapchainTest
    , testCase "destroying a poisoned swapchain leaves it for the context finalizer" poisonedSwapchainDestroyTest
    ]

swapchainPureTests :: TestTree
swapchainPureTests =
  testGroup
    "swapchain pure"
    [ testCase "frames in flight must be positive" $ do
        validateFramesInFlight 2 @?= Right ()
        validateFramesInFlight 0 @?= Left (InvalidFramesInFlight 0)
        validateFramesInFlight (-1) @?= Left (InvalidFramesInFlight (-1))
    , testCase "default acquire timeout is finite" $
        acquireTimeoutNanoseconds defaultSwapchainConfig @?= 1_000_000_000
    , testCase "acquire timeout is capped at the documented practical maximum" $ do
        finiteAcquireTimeoutNanoseconds 9 @?= 9
        finiteAcquireTimeoutNanoseconds maximumAcquireTimeoutNanoseconds @?= maximumAcquireTimeoutNanoseconds
        finiteAcquireTimeoutNanoseconds (maximumAcquireTimeoutNanoseconds + 1) @?= maximumAcquireTimeoutNanoseconds
        finiteAcquireTimeoutNanoseconds maxBound @?= maximumAcquireTimeoutNanoseconds
    , testCase "format choice requires the exact sRGB pair" $ do
        let unorm = Surface.SurfaceFormatKHR Format.FORMAT_B8G8R8A8_UNORM Surface.COLOR_SPACE_SRGB_NONLINEAR_KHR
            srgb = Surface.SurfaceFormatKHR Format.FORMAT_B8G8R8A8_SRGB Surface.COLOR_SPACE_SRGB_NONLINEAR_KHR
        chooseSurfaceFormat [unorm, srgb] @?= Right srgb
        chooseSurfaceFormat [unorm] @?= Left (SwapchainFormatUnavailable [show unorm])
    , testCase "image count requests one above minimum and respects maximum" $ do
        chooseImageCount 2 0 @?= 3
        chooseImageCount 2 2 @?= 2
        chooseImageCount (maxBound :: Word32) 0 @?= maxBound
    , testCase "incomplete swapchain image enumeration retries" swapchainImageEnumerationRetryTest
    , testCase "present preference falls back to FIFO" $ do
        choosePresentMode Mailbox [Surface.PRESENT_MODE_FIFO_KHR, Surface.PRESENT_MODE_MAILBOX_KHR] @?= Surface.PRESENT_MODE_MAILBOX_KHR
        choosePresentMode Immediate [Surface.PRESENT_MODE_FIFO_KHR] @?= Surface.PRESENT_MODE_FIFO_KHR
    , testCase "extent uses fixed values, clamps provider values, and defers zero" $ do
        let flexible = Fundamental.Extent2D maxBound maxBound
            minimumExtent = Fundamental.Extent2D 64 32
            maximumExtent = Fundamental.Extent2D 1920 1080
        chooseExtent (Fundamental.Extent2D 800 600) minimumExtent maximumExtent Nothing @?= Right (ExtentReady (Fundamental.Extent2D 800 600))
        chooseExtent flexible minimumExtent maximumExtent (Just (4000, 10)) @?= Right (ExtentReady (Fundamental.Extent2D 1920 32))
        chooseExtent flexible minimumExtent maximumExtent (Just (0, 720)) @?= Right ExtentMinimized
        chooseExtent flexible minimumExtent maximumExtent Nothing @?= Left SwapchainExtentUnavailable
    , testCase "split graphics and present families select concurrent sharing" $ do
        chooseFamilySharing 3 3 @?= ExclusiveFamily 3
        chooseFamilySharing 3 7 @?= ConcurrentFamilies 3 7
    , testCase "core state release is idempotent and rejects later use" $ do
        releaseCoreState (CoreActive (42 :: Int)) @?= (CoreRetiring 42, Just 42)
        releaseCoreState (CoreRetiring (42 :: Int)) @?= (CoreRetiring 42, Just 42)
        releaseCoreState (CoreReleased :: CoreState Int) @?= (CoreReleased, Nothing)
        activeCoreState (CoreReleased :: CoreState Int) @?= Left SwapchainReleased
        activeCoreState (CorePoisoned (42 :: Int)) @?= Left SwapchainPoisoned
        releaseCoreState (CorePoisoned (42 :: Int)) @?= (CoreRetiring 42, Just 42)
    , testCase "recreation preflight failure keeps the old generation active" recreationPreflightFailureTest
    , testCase "native recreation failure retires old and retries with a null old handle" recreationNativeFailureTest
    , testCase "post-native recreation failure retains raw cleanup without an active generation" recreationBuildFailureTest
    , testCase "operation mutex serializes whole protocols" $ do
        mutex <- newMVar ()
        firstEntered <- newEmptyMVar
        releaseFirst <- newEmptyMVar
        secondEntered <- newEmptyMVar
        _ <- forkIO (withOperationMutexForTest mutex (putMVar firstEntered () >> takeMVar releaseFirst))
        takeMVar firstEntered
        _ <- forkIO (withOperationMutexForTest mutex (putMVar secondEntered ()))
        threadDelay 20_000
        tryReadMVar secondEntered >>= (@?= Nothing)
        putMVar releaseFirst ()
        takeMVar secondEntered
    , testCase "slot protocol only retires submitted ownership" slotProtocolTest
    , testCase "retiring one submitted slot leaves other slots unchanged" retiresSelectedSlotOnlyTest
    , testCase "render-finished semaphore is reusable only after same-image reacquire" renderFinishedProtocolTest
    , testCase "wait failure preserves submitted state and retained releases" waitFailurePreservesSubmissionTest
    , testCase "failed retained release alone is retried in exact order" releaseFailureRetryTest
    , testCase "DeviceLost force drain precedes gates and object destruction" deviceLostDrainOrderingTest
    , testCase "cleanup retry ledger rethrows async exceptions" cleanupAsyncExceptionTest
    , testCase "acquire driver results map explicitly" acquireDriverResultMappingTest
    , testCase "present driver results map wait acceptance explicitly" presentDriverResultMappingTest
    , testCase "acquire image index is bounds checked" acquireImageIndexBoundsTest
    , testCase "driver call seams distinguish Vulkan, synchronous, and async exceptions" driverExceptionBoundaryTest
    , testCase "ambiguous acquire ownership blocks reset and reuse" ambiguousAcquireRetentionTest
    , testCase "descriptor frame storage keeps distinct layout identities separate" descriptorFrameStorageIdentityTest
    ]

data RecreationModel = RecreationModel
  { recreationActive :: Maybe String
  , recreationRetired :: [String]
  , recreationPartial :: Maybe String
  }
  deriving (Eq, Show)

initialRecreationModel :: RecreationModel
initialRecreationModel = RecreationModel (Just "old") [] Nothing

retireRecreationModel :: RecreationModel -> plan -> RecreationModel
retireRecreationModel model _ =
  model
    { recreationActive = Nothing
    , recreationRetired = maybe id (:) (recreationActive model) (recreationRetired model)
    , recreationPartial = Nothing
    }

publishRecreationRaw :: RecreationModel -> String -> RecreationModel
publishRecreationRaw model raw = model{recreationPartial = Just raw}

finishRecreationModel :: RecreationModel -> String -> String -> RecreationModel
finishRecreationModel model _ ready = model{recreationActive = Just ready, recreationPartial = Nothing}

recreationPreflightFailureTest :: IO ()
recreationPreflightFailureTest = do
  state <- newMVar (CoreActive initialRecreationModel)
  result <-
    try
      ( runIrreversibleRecreationForTest
          state
          (const (throwIO DeviceLost :: IO String))
          retireRecreationModel
          pure
          publishRecreationRaw
          pure
          finishRecreationModel
      )
  result @?= Left DeviceLost
  readMVar state >>= (@?= CoreActive initialRecreationModel)

recreationNativeFailureTest :: IO ()
recreationNativeFailureTest = do
  state <- newMVar (CoreActive initialRecreationModel)
  observedOldHandles <- newIORef []
  let preflight model = pure (fromMaybe "VK_NULL_HANDLE" (recreationActive model))
      createFail oldHandle = do
        atomicModifyIORef' observedOldHandles (\handles -> (handles <> [oldHandle], ()))
        throwIO DeviceLost :: IO String
      createReady oldHandle = do
        atomicModifyIORef' observedOldHandles (\handles -> (handles <> [oldHandle], ()))
        pure "raw-new"
  first <-
    try
      ( runIrreversibleRecreationForTest
          state
          preflight
          retireRecreationModel
          createFail
          publishRecreationRaw
          pure
          finishRecreationModel
      )
  first @?= Left DeviceLost
  readMVar state
    >>= ( @?=
            CoreActive
              RecreationModel
                { recreationActive = Nothing
                , recreationRetired = ["old"]
                , recreationPartial = Nothing
                }
        )
  second <-
    runIrreversibleRecreationForTest
      state
      preflight
      retireRecreationModel
      createReady
      publishRecreationRaw
      (const (pure "ready-new"))
      finishRecreationModel
  second @?= "ready-new"
  readMVar state
    >>= ( @?=
            CoreActive
              RecreationModel
                { recreationActive = Just "ready-new"
                , recreationRetired = ["old"]
                , recreationPartial = Nothing
                }
        )
  readIORef observedOldHandles >>= (@?= ["old", "VK_NULL_HANDLE"])

recreationBuildFailureTest :: IO ()
recreationBuildFailureTest = do
  state <- newMVar (CoreActive initialRecreationModel)
  result <-
    try
      ( runIrreversibleRecreationForTest
          state
          (const (pure "old"))
          retireRecreationModel
          (const (pure "raw-new"))
          publishRecreationRaw
          (const (throwIO SurfaceLost :: IO String))
          finishRecreationModel
      )
  result @?= Left SurfaceLost
  readMVar state
    >>= ( @?=
            CoreActive
              RecreationModel
                { recreationActive = Nothing
                , recreationRetired = ["old"]
                , recreationPartial = Just "raw-new"
                }
        )

swapchainImageEnumerationRetryTest :: IO ()
swapchainImageEnumerationRetryTest = do
  outcomes <-
    newIORef
      [ (Result.INCOMPLETE, Vector.singleton (1 :: Word32))
      , (Result.SUCCESS, Vector.fromList [2, 3])
      ]
  let next =
        atomicModifyIORef' outcomes $ \case
          [] -> ([], (Result.SUCCESS, Vector.empty))
          outcome : rest -> (rest, outcome)
  enumerateCompleteForTest "swapchain image enumeration test" next
    >>= (@?= Vector.fromList [2, 3])
  readIORef outcomes >>= (@?= [])

descriptorFrameStorageIdentityTest :: IO ()
descriptorFrameStorageIdentityTest = do
  firstIdentity <- newDescriptorLayoutIdentityForTest
  secondIdentity <- newDescriptorLayoutIdentityForTest
  storage <- newMVar []
  nextValue <- newIORef (0 :: Int)
  let create = atomicModifyIORef' nextValue (\value -> let next = value + 1 in (next, next))
  first <- lookupOrCreateSlotValueForTest storage firstIdentity create
  firstAgain <- lookupOrCreateSlotValueForTest storage firstIdentity create
  second <- lookupOrCreateSlotValueForTest storage secondIdentity create
  (first, firstAgain, second) @?= (1, 1, 2)

slotProtocolTest :: IO ()
slotProtocolTest = do
  let ownership = FrameOwnership 7 2
      acquired = SlotStateAcquired ownership
      submitted = SlotStateSubmitted ownership 19
  acquireSlotTransition ownership SlotStateIdle @?= Right acquired
  submitSlotTransition 19 acquired @?= Right submitted
  completeSlotTransition submitted @?= Right SlotStateIdle
  assertBool "an acquired slot cannot be acquired again" (isLeft (acquireSlotTransition ownership acquired))
  assertBool "an acquired slot cannot be reset/retired" (isLeft (completeSlotTransition acquired))
  assertBool "an idle slot cannot publish submission" (isLeft (submitSlotTransition 19 SlotStateIdle))

retiresSelectedSlotOnlyTest :: IO ()
retiresSelectedSlotOnlyTest = do
  events <- newMVar []
  let record event = modifyMVar_ events (pure . (<> [event]))
      firstOwnership = FrameOwnership 14 0
      selectedOwnership = FrameOwnership 14 1
      thirdOwnership = FrameOwnership 14 2
  firstSlot <- newSubmittedSlotStateForTest firstOwnership 101 []
  selectedSlot <- newSubmittedSlotStateForTest selectedOwnership 202 []
  thirdSlot <- newSubmittedSlotStateForTest thirdOwnership 303 []

  retireSlotStateForTest selectedSlot (\timeline -> record ("wait " <> show timeline)) (record "reset selected")

  readMVar events >>= (@?= ["wait 202", "reset selected"])
  inspectSlotStateForTest firstSlot >>= (@?= SlotStateSubmitted firstOwnership 101)
  inspectSlotStateForTest selectedSlot >>= (@?= SlotStateIdle)
  inspectSlotStateForTest thirdSlot >>= (@?= SlotStateSubmitted thirdOwnership 303)

renderFinishedProtocolTest :: IO ()
renderFinishedProtocolTest = do
  submitRenderFinishedTransition 23 RenderFinishedIdle @?= Right (RenderFinishedSignalSubmitted 23)
  queuePresentWaitTransition (RenderFinishedSignalSubmitted 23) @?= Right (RenderFinishedPresentWaitQueued 23)
  reacquireRenderFinishedTransition (RenderFinishedPresentWaitQueued 23) @?= Right RenderFinishedIdle
  reacquireRenderFinishedTransition RenderFinishedIdle @?= Right RenderFinishedIdle
  assertBool
    "a submitted signal is not reusable before a present wait is queued and its image reacquired"
    (isLeft (submitRenderFinishedTransition 24 (RenderFinishedSignalSubmitted 23)))
  assertBool
    "queueing the present wait alone does not permit another signal"
    (isLeft (submitRenderFinishedTransition 24 (RenderFinishedPresentWaitQueued 23)))
  assertBool
    "a signal without a queued present wait cannot be retired by reacquire"
    (isLeft (reacquireRenderFinishedTransition (RenderFinishedSignalSubmitted 23)))

waitFailurePreservesSubmissionTest :: IO ()
waitFailurePreservesSubmissionTest = do
  events <- newMVar []
  let ownership = FrameOwnership 4 1
      record event = modifyMVar_ events (pure . (<> [event]))
      failingWait timeline = record ("wait " <> show timeline) >> throwIO SurfaceLost
      successfulWait timeline = record ("wait " <> show timeline)
  state <-
    newSubmittedSlotStateForTest
      ownership
      41
      [record "release image", record "release descriptor"]
  first <- timeout 1_000_000 (try (retireSlotStateForTest state failingWait (record "reset")) :: IO (Either VpipeError ()))
  first @?= Just (Left SurfaceLost)
  inspectSlotStateForTest state >>= (@?= SlotStateSubmitted ownership 41)
  readMVar events >>= (@?= ["wait 41"])
  second <- timeout 1_000_000 (retireSlotStateForTest state successfulWait (record "reset"))
  second @?= Just ()
  inspectSlotStateForTest state >>= (@?= SlotStateIdle)
  readMVar events >>= (@?= ["wait 41", "wait 41", "release image", "release descriptor", "reset"])

releaseFailureRetryTest :: IO ()
releaseFailureRetryTest = do
  events <- newMVar []
  failingAttempts <- newIORef (0 :: Int)
  let ownership = FrameOwnership 9 0
      record event = modifyMVar_ events (pure . (<> [event]))
      flakyRelease = do
        attempt <- atomicModifyIORef' failingAttempts (\current -> let next = current + 1 in (next, next))
        record ("release descriptor " <> show attempt)
        when (attempt == 1) (throwIO SurfaceLost)
  state <-
    newSubmittedSlotStateForTest
      ownership
      57
      [ record "release frame image"
      , flakyRelease
      , record "release staging slice"
      ]
  first <- timeout 1_000_000 (try (retireSlotStateForTest state (const (record "wait")) (record "reset")) :: IO (Either VpipeError ()))
  case first of
    Just (Left CleanupFailed{}) -> pure ()
    _ -> assertFailure ("expected retained-release cleanup failure, got " <> show first)
  inspectSlotStateForTest state >>= (@?= SlotStateSubmitted ownership 57)
  readMVar events >>= (@?= ["wait", "release frame image", "release descriptor 1", "release staging slice"])
  second <- timeout 1_000_000 (retireSlotStateForTest state (const (record "wait")) (record "reset"))
  second @?= Just ()
  inspectSlotStateForTest state >>= (@?= SlotStateIdle)
  readMVar events
    >>= ( @?=
            [ "wait"
            , "release frame image"
            , "release descriptor 1"
            , "release staging slice"
            , "wait"
            , "release descriptor 2"
            , "reset"
            ]
        )

deviceLostDrainOrderingTest :: IO ()
deviceLostDrainOrderingTest = do
  events <- newMVar []
  let ownership = FrameOwnership 12 3
      record event = modifyMVar_ events (pure . (<> [event]))
  slotState <- newSubmittedSlotStateForTest ownership 88 [record "release frame image", record "release descriptor frame"]
  objectCleanup <-
    newCleanupActionsForTest
      [ record "destroy render-finished"
      , record "destroy image view"
      , record "destroy raw swapchain"
      , record "destroy command pool"
      , record "destroy acquire semaphore"
      ]
  outcome <- timeout 1_000_000 $ do
    waitResult <- try (retireSlotStateForTest slotState (const (record "wait DeviceLost" >> throwIO DeviceLost)) (pure ())) :: IO (Either VpipeError ())
    waitResult @?= Left DeviceLost
    forceDrainSlotStateForTest slotState
    record "close generation gate"
    runCleanupActionsForTest objectCleanup
  outcome @?= Just ()
  inspectSlotStateForTest slotState >>= (@?= SlotStateIdle)
  readMVar events
    >>= ( @?=
            [ "wait DeviceLost"
            , "release frame image"
            , "release descriptor frame"
            , "close generation gate"
            , "destroy render-finished"
            , "destroy image view"
            , "destroy raw swapchain"
            , "destroy command pool"
            , "destroy acquire semaphore"
            ]
        )

cleanupAsyncExceptionTest :: IO ()
cleanupAsyncExceptionTest = do
  actions <- newCleanupActionsForTest [throwIO ThreadKilled]
  result <- timeout 1_000_000 (try (runCleanupActionsForTest actions) :: IO (Either AsyncException ()))
  result @?= Just (Left ThreadKilled)
  pendingCleanupActionsForTest actions >>= (@?= 1)

acquireDriverResultMappingTest :: IO ()
acquireDriverResultMappingTest = do
  classifyAcquireDriverResult Result.SUCCESS 3 @?= AcquireDriverSuccess 3
  classifyAcquireDriverResult Result.SUBOPTIMAL_KHR 4 @?= AcquireDriverSuboptimal 4
  classifyAcquireDriverResult Result.TIMEOUT 99 @?= AcquireDriverTimeout
  classifyAcquireDriverResult Result.NOT_READY 99 @?= AcquireDriverNotReady
  classifyAcquireDriverResult Result.ERROR_OUT_OF_DATE_KHR 99 @?= AcquireDriverOutOfDate
  classifyAcquireDriverResult Result.ERROR_SURFACE_LOST_KHR 99 @?= AcquireDriverSurfaceLost
  classifyAcquireDriverResult Result.ERROR_DEVICE_LOST 99 @?= AcquireDriverDeviceLost
  classifyAcquireDriverResult Result.ERROR_OUT_OF_HOST_MEMORY 99 @?= AcquireDriverUnexpected Result.ERROR_OUT_OF_HOST_MEMORY

presentDriverResultMappingTest :: IO ()
presentDriverResultMappingTest = do
  let accepted =
        [ PresentDriverSuccess
        , PresentDriverSuboptimal
        , PresentDriverOutOfDate
        , PresentDriverSurfaceLost
        , PresentDriverAcceptedFailure Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
        ]
      notAccepted =
        [ PresentDriverDeviceLost
        , PresentDriverRejected Result.ERROR_OUT_OF_HOST_MEMORY
        , PresentDriverRejected Result.ERROR_OUT_OF_DEVICE_MEMORY
        , PresentDriverAmbiguous Result.TIMEOUT
        , PresentDriverAmbiguous Result.NOT_READY
        , PresentDriverAmbiguous Result.ERROR_UNKNOWN
        ]
  classifyPresentDriverResult Result.SUCCESS @?= PresentDriverSuccess
  classifyPresentDriverResult Result.SUBOPTIMAL_KHR @?= PresentDriverSuboptimal
  classifyPresentDriverResult Result.ERROR_OUT_OF_DATE_KHR @?= PresentDriverOutOfDate
  classifyPresentDriverResult Result.ERROR_SURFACE_LOST_KHR @?= PresentDriverSurfaceLost
  classifyPresentDriverResult Result.ERROR_DEVICE_LOST @?= PresentDriverDeviceLost
  classifyPresentDriverResult Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT @?= PresentDriverAcceptedFailure Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  classifyPresentDriverResult Result.ERROR_OUT_OF_HOST_MEMORY @?= PresentDriverRejected Result.ERROR_OUT_OF_HOST_MEMORY
  classifyPresentDriverResult Result.ERROR_OUT_OF_DEVICE_MEMORY @?= PresentDriverRejected Result.ERROR_OUT_OF_DEVICE_MEMORY
  classifyPresentDriverResult Result.TIMEOUT @?= PresentDriverAmbiguous Result.TIMEOUT
  classifyPresentDriverResult Result.NOT_READY @?= PresentDriverAmbiguous Result.NOT_READY
  classifyPresentDriverResult Result.ERROR_UNKNOWN @?= PresentDriverAmbiguous Result.ERROR_UNKNOWN
  traverse_ (\outcome -> assertBool ("expected accepted present wait for " <> show outcome) (presentDriverWaitAccepted outcome)) accepted
  traverse_ (\outcome -> assertBool ("expected unconfirmed present wait for " <> show outcome) (not (presentDriverWaitAccepted outcome))) notAccepted
  traverse_
    ( \outcome -> do
        state <- newMVar (RenderFinishedSignalSubmitted 44)
        publishPresentDriverOutcomeForTest state outcome
        readMVar state >>= (@?= RenderFinishedPresentWaitQueued 44)
    )
    accepted
  traverse_
    ( \outcome -> do
        state <- newMVar (RenderFinishedSignalSubmitted 44)
        publishPresentDriverOutcomeForTest state outcome
        readMVar state >>= (@?= RenderFinishedSignalSubmitted 44)
    )
    notAccepted

acquireImageIndexBoundsTest :: IO ()
acquireImageIndexBoundsTest = do
  validateAcquiredImageIndex 2 0 @?= Right 0
  validateAcquiredImageIndex 2 1 @?= Right 1
  case validateAcquiredImageIndex 2 2 of
    Left VulkanFailure{vulkanOperation = "vkAcquireNextImageKHR"} -> pure ()
    result -> assertFailure ("expected an out-of-bounds acquire failure, got " <> show result)
  assertBool "Word32 maximum cannot wrap into an Int index" (isLeft (validateAcquiredImageIndex 2 maxBound))

driverExceptionBoundaryTest :: IO ()
driverExceptionBoundaryTest = do
  runAcquireDriverCallForTest (pure (Result.SUBOPTIMAL_KHR, 5)) >>= (@?= Right (AcquireDriverSuboptimal 5))
  runAcquireDriverCallForTest (throwIO (Vulkan.VulkanException Result.ERROR_OUT_OF_DATE_KHR)) >>= (@?= Right AcquireDriverOutOfDate)
  runAcquireDriverCallForTest (throwIO (Vulkan.VulkanException Result.ERROR_SURFACE_LOST_KHR)) >>= (@?= Right AcquireDriverSurfaceLost)
  runAcquireDriverCallForTest (throwIO (Vulkan.VulkanException Result.ERROR_DEVICE_LOST)) >>= (@?= Right AcquireDriverDeviceLost)
  runPresentDriverCallForTest (throwIO (Vulkan.VulkanException Result.ERROR_OUT_OF_DATE_KHR)) >>= (@?= Right PresentDriverOutOfDate)
  runPresentDriverCallForTest (throwIO (Vulkan.VulkanException Result.ERROR_SURFACE_LOST_KHR)) >>= (@?= Right PresentDriverSurfaceLost)
  runPresentDriverCallForTest (throwIO (Vulkan.VulkanException Result.ERROR_DEVICE_LOST)) >>= (@?= Right PresentDriverDeviceLost)
  runAcquireDriverCallForTest (throwIO SurfaceLost) >>= (@?= Left "SurfaceLost")
  runPresentDriverCallForTest (throwIO SurfaceLost) >>= (@?= Left "SurfaceLost")
  acquireAsync <- try (runAcquireDriverCallForTest (throwIO ThreadKilled)) :: IO (Either AsyncException (Either String AcquireDriverOutcome))
  acquireAsync @?= Left ThreadKilled
  presentAsync <- try (runPresentDriverCallForTest (throwIO ThreadKilled)) :: IO (Either AsyncException (Either String PresentDriverOutcome))
  presentAsync @?= Left ThreadKilled

ambiguousAcquireRetentionTest :: IO ()
ambiguousAcquireRetentionTest = do
  state <- newIdleSlotStateForTest
  events <- newMVar []
  let uncertain = FrameOwnership 31 maxBound
      replacement = FrameOwnership 31 0
      record event = modifyMVar_ events (pure . (<> [event]))
  retainAcquiredSlotForTest state uncertain
  retainAcquiredSlotForTest state replacement
  inspectSlotStateForTest state >>= (@?= SlotStateAcquired uncertain)
  retirement <- try (retireSlotStateForTest state (const (record "wait")) (record "reset")) :: IO (Either VpipeError ())
  case retirement of
    Left VulkanFailure{vulkanOperation = "retire swapchain slot"} -> pure ()
    result -> assertFailure ("expected retained ambiguous acquisition to reject retirement, got " <> show result)
  inspectSlotStateForTest state >>= (@?= SlotStateAcquired uncertain)
  readMVar events >>= (@?= [])
  forceDrainSlotStateForTest state
  inspectSlotStateForTest state >>= (@?= SlotStateIdle)

liveHeadlessSwapchainTest :: IO ()
liveHeadlessSwapchainTest = do
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  case requested of
    Just "skip" -> putStrLn "SKIP: VPIPE_TEST_DEVICE=skip"
    Just unexpected
      | unexpected /= "any" && unexpected /= "lavapipe" ->
          assertFailure "VPIPE_TEST_DEVICE must be skip, any, or lavapipe"
    _ -> do
      let config = defaultVpipeConfig{vpipeValidationStrict = requested == Just "lavapipe"}
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
              swapchain <- newSwapchain context surface defaultSwapchainConfig
              swapchainExtent swapchain >>= (@?= (64, 64))
              submitAndPresentHeadlessFrame swapchain >>= (@?= True)
              swapchainExtent swapchain >>= (@?= (64, 64))
              destroySwapchain swapchain
              destroySwapchain swapchain
      result <- try exercise :: IO (Either VpipeError ())
      case result of
        Left (NoVulkanIcd detail) | requested /= Just "lavapipe" -> putStrLn ("SKIP: Vulkan ICD unavailable: " <> detail)
        Left (RequiredInstanceExtensionsUnavailable missing)
          | requested /= Just "lavapipe" ->
              putStrLn ("SKIP: headless surface extensions unavailable: " <> show missing)
        Left (SwapchainFormatUnavailable formats) ->
          putStrLn ("SKIP: headless surface does not expose the required SRGB swapchain format: " <> show formats)
        Left error' -> throwIO error'
        Right () -> pure ()

poisonedSwapchainDestroyTest :: IO ()
poisonedSwapchainDestroyTest = do
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  case requested of
    Just "skip" -> putStrLn "SKIP: VPIPE_TEST_DEVICE=skip"
    Just unexpected
      | unexpected /= "any" && unexpected /= "lavapipe" ->
          assertFailure "VPIPE_TEST_DEVICE must be skip, any, or lavapipe"
    _ -> do
      let config = defaultVpipeConfig{vpipeValidationStrict = requested == Just "lavapipe"}
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
              swapchain <- newSwapchain context surface defaultSwapchainConfig
              withSwapchainOperation swapchain poisonSwapchainLocked
              timeout 1_000_000 (destroySwapchain swapchain) >>= (@?= Just ())
              result <- try (swapchainExtent swapchain) :: IO (Either VpipeError (Word32, Word32))
              result @?= Left SwapchainPoisoned
      result <- try exercise :: IO (Either VpipeError ())
      case result of
        Left (NoVulkanIcd detail) | requested /= Just "lavapipe" -> putStrLn ("SKIP: Vulkan ICD unavailable: " <> detail)
        Left (RequiredInstanceExtensionsUnavailable missing)
          | requested /= Just "lavapipe" ->
              putStrLn ("SKIP: headless surface extensions unavailable: " <> show missing)
        Left (SwapchainFormatUnavailable formats) ->
          putStrLn ("SKIP: headless surface does not expose the required SRGB swapchain format: " <> show formats)
        Left error' -> throwIO error'
        Right () -> pure ()

submitAndPresentHeadlessFrame :: Swapchain -> IO Bool
submitAndPresentHeadlessFrame swapchain =
  withSwapchainOperation swapchain $ \locked -> do
    acquisition <- acquireNextImageLocked locked
    case acquisition of
      AcquireReady slot generation image _ -> do
        recordPresentLayoutTransition slot image
        _ <- submitPreparedFrameForTest locked slot image
        presentation <- presentGenerationImageLocked locked generation image
        case presentation of
          QueuePresentComplete _ -> pure ()
          QueuePresentNeedsRecreation -> pure ()
        replaceGenerationLocked locked
      AcquireDeferredNow reason -> assertFailure ("headless acquire unexpectedly deferred: " <> show reason) >> pure False
      AcquireNeedsRecreation -> assertFailure "fresh headless swapchain was immediately out of date" >> pure False

recordPresentLayoutTransition :: FrameSlot -> GenerationImage -> IO ()
recordPresentLayoutTransition slot image = do
  let commandBuffer = frameSlotCommandBuffer slot
      beginInfo = (zero :: CommandBuffer.CommandBufferBeginInfo '[]){CommandBuffer.flags = CommandBuffer.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}
      barrier =
        Chain.SomeStruct
          ( (zero :: Sync2.ImageMemoryBarrier2 '[])
              { Sync2.srcStageMask = zero
              , Sync2.srcAccessMask = zero
              , Sync2.dstStageMask = Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT
              , Sync2.dstAccessMask = zero
              , Sync2.oldLayout = Layout.IMAGE_LAYOUT_UNDEFINED
              , Sync2.newLayout = Layout.IMAGE_LAYOUT_PRESENT_SRC_KHR
              , Sync2.srcQueueFamilyIndex = maxBound
              , Sync2.dstQueueFamilyIndex = maxBound
              , Sync2.image = generationImageHandle image
              , Sync2.subresourceRange = ImageView.ImageSubresourceRange Aspect.IMAGE_ASPECT_COLOR_BIT 0 1 0 1
              }
          )
      dependency = (zero :: Sync2.DependencyInfo){Sync2.imageMemoryBarriers = Vector.singleton barrier}
  CommandBuffer.beginCommandBuffer commandBuffer beginInfo
  Sync2.cmdPipelineBarrier2 commandBuffer dependency
  CommandBuffer.endCommandBuffer commandBuffer
