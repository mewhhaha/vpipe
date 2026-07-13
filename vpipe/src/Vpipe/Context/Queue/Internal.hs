{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vpipe.Context.Queue.Internal (
  Queue,
  QueueRole (..),
  LifecycleGate,
  ClosedLifecycleGate,
  newLifecycleGate,
  withLifecycleLease,
  closeLifecycleGate,
  lifecycleGateClosed,
  newQueue,
  queueFamilyIndex,
  submitEmpty,
  submitCommandBuffers,
  submitCommandBuffersLeased,
  QueueDependency (..),
  submitCommandBuffersAfterLeased,
  BinarySemaphoreWait (..),
  BinarySemaphoreSignal (..),
  submitCommandBuffersWithLeased,
  QueueSubmitDriverOutcome (..),
  SubmissionPublicationOutcome (..),
  submitCommandBuffersWithPublicationLeased,
  submitCommandBuffersWithPublicationUsingLeased,
  runVulkanQueueSubmit,
  withQueueHandleLockedLeased,
  waitTimeline,
  waitTimelineLeased,
  timelineCompletedValue,
  timelineCompletedValueLeased,
  currentTimelineValueForTest,
  nextTimelineSignalForTest,
  exerciseQueueDependencyForTest,
  exerciseBinarySemaphoreChainForTest,
  withQueuesLockedForShutdown,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, modifyMVarMasked_, newMVar, putMVar, readMVar, takeMVar, withMVar)
import Control.Exception (SomeException, bracket, catch, finally, fromException, mask, mask_, onException, throwIO, toException, try, uninterruptibleMask_)
import Control.Monad (foldM, void, when)
import Data.Bits ((.|.))
import Data.Vector qualified as Vector
import Data.Word (Word32, Word64)
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.CommandBuffer qualified as CommandBuffer
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Core10.Handles qualified as Vk
import Vulkan.Core10.QueueSemaphore qualified as Semaphore
import Vulkan.Core12.Enums.SemaphoreType qualified as SemaphoreType
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore qualified as Timeline
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 qualified as Sync2
import Vulkan.Exception qualified as Vulkan
import Vulkan.Zero (zero)

import Vpipe.Error (VpipeError (..))

data QueueRole = GraphicsQueue | ComputeQueue | TransferQueue
  deriving (Eq, Ord, Show, Enum, Bounded)

data GateState = GateState
  { gateClosing :: Bool
  , gateActiveLeases :: Int
  }

data LifecycleGate = LifecycleGate (MVar GateState) (MVar ())
newtype ClosedLifecycleGate = ClosedLifecycleGate LifecycleGate

newLifecycleGate :: IO LifecycleGate
newLifecycleGate = LifecycleGate <$> newMVar (GateState False 0) <*> newMVar ()

acquireLifecycleLease :: LifecycleGate -> IO ()
acquireLifecycleLease (LifecycleGate stateVariable drained) =
  modifyMVarMasked_ stateVariable $ \state -> do
    if gateClosing state
      then throwIO ContextClosed
      else do
        when (gateActiveLeases state == 0) (takeMVar drained)
        pure state{gateActiveLeases = gateActiveLeases state + 1}

releaseLifecycleLease :: LifecycleGate -> IO ()
releaseLifecycleLease (LifecycleGate stateVariable drained) =
  modifyMVarMasked_ stateVariable $ \state -> do
    let remaining = gateActiveLeases state - 1
    if remaining == 0
      then do
        putMVar drained ()
        pure state{gateActiveLeases = 0}
      else pure state{gateActiveLeases = remaining}

-- | Prevents close from passing the gate while an operation is in progress.
withLifecycleLease :: LifecycleGate -> IO a -> IO a
withLifecycleLease gate action = mask $ \restore -> do
  acquireLifecycleLease gate
  restore action `finally` releaseLifecycleLease gate

closeLifecycleGate :: LifecycleGate -> IO ClosedLifecycleGate
closeLifecycleGate (LifecycleGate stateVariable drained) = mask_ $ do
  modifyMVarMasked_ stateVariable (\state -> pure state{gateClosing = True})
  void (readMVar drained)
  pure (ClosedLifecycleGate (LifecycleGate stateVariable drained))

lifecycleGateClosed :: LifecycleGate -> IO Bool
lifecycleGateClosed (LifecycleGate stateVariable _) = gateClosing <$> readMVar stateVariable

data QueueAcceptance
  = QueueAcceptanceKnown
  | QueueAcceptanceUnknown
  deriving (Eq)

data QueueState = QueueState
  { lastSubmittedTimelineValue :: Word64
  , queueAcceptance :: QueueAcceptance
  }

data Queue = Queue
  { rawDevice :: Vk.Device
  , rawQueue :: Vk.Queue
  , rawQueueSubmitLock :: MVar ()
  , familyIndex :: Word32
  , timelineSemaphore :: Vk.Semaphore
  , maximumTimelineDifference :: Word64
  , queueGate :: LifecycleGate
  , queueState :: MVar QueueState
  , nameQueueObject :: ObjectType.ObjectType -> String -> Word64 -> IO ()
  }

data QueueDependency = QueueDependency
  { dependencyQueue :: Queue
  , dependencyTimeline :: Word64
  , dependencyDestinationStage :: Stage2.PipelineStageFlags2
  }

-- | A binary semaphore that must be signalled before this submission executes.
data BinarySemaphoreWait = BinarySemaphoreWait
  { binaryWaitSemaphore :: Vk.Semaphore
  , binaryWaitDestinationStage :: Stage2.PipelineStageFlags2
  }

-- | A binary semaphore that this submission signals on completion.
data BinarySemaphoreSignal = BinarySemaphoreSignal
  { binarySignalSemaphore :: Vk.Semaphore
  , binarySignalSourceStage :: Stage2.PipelineStageFlags2
  }

data SubmissionPublicationOutcome
  = SubmissionRejected SomeException
  | SubmissionAcceptanceUnknown SomeException
  | SubmissionAccepted Word64
  | SubmissionAcceptedPublicationFailed Word64 SomeException

data QueueSubmitDriverOutcome
  = QueueSubmitRejected SomeException
  | QueueSubmitAcceptanceUnknown SomeException
  | QueueSubmitAccepted

newQueue :: Vk.Device -> Vk.Queue -> Word32 -> Vk.Semaphore -> Word64 -> LifecycleGate -> (ObjectType.ObjectType -> String -> Word64 -> IO ()) -> IO Queue
newQueue device queue family semaphore maximumDifference gate nameObject = do
  submitLock <- newMVar ()
  newQueueWithSubmitLock device queue submitLock family semaphore maximumDifference gate nameObject

newQueueWithSubmitLock :: Vk.Device -> Vk.Queue -> MVar () -> Word32 -> Vk.Semaphore -> Word64 -> LifecycleGate -> (ObjectType.ObjectType -> String -> Word64 -> IO ()) -> IO Queue
newQueueWithSubmitLock device queue submitLock family semaphore maximumDifference gate nameObject = do
  state <- newMVar (QueueState 0 QueueAcceptanceKnown)
  pure (Queue device queue submitLock family semaphore maximumDifference gate state nameObject)

queueFamilyIndex :: Queue -> Word32
queueFamilyIndex = familyIndex

withQueuesLockedForShutdown :: ClosedLifecycleGate -> [Queue] -> IO a -> IO a
withQueuesLockedForShutdown closedGate queues action
  | all (belongsToClosedGate closedGate) queues = foldr lockQueue action queues
  | otherwise = throwIO (CleanupFailed ["shutdown queue does not belong to the closed context gate"])
 where
  lockQueue queue next =
    withMVar (rawQueueSubmitLock queue) $ \_ ->
      withMVar (queueState queue) (const next)

belongsToClosedGate :: ClosedLifecycleGate -> Queue -> Bool
belongsToClosedGate (ClosedLifecycleGate (LifecycleGate closedState _)) queue =
  case queueGate queue of
    LifecycleGate queueStateVariable _ -> closedState == queueStateVariable

submitEmpty :: Queue -> IO Word64
submitEmpty queue = submitCommandBuffers queue Vector.empty

submitCommandBuffers :: Queue -> Vector.Vector CommandBuffer.CommandBuffer -> IO Word64
submitCommandBuffers queue commandBuffers =
  withLifecycleLease (queueGate queue) (submitCommandBuffersLeased queue commandBuffers)

-- | Internal variant for callers which already hold the owning Context lease.
submitCommandBuffersLeased :: Queue -> Vector.Vector CommandBuffer.CommandBuffer -> IO Word64
submitCommandBuffersLeased queue = submitCommandBuffersWithLeased queue [] [] []

{- | Submits work after GPU-side timeline dependencies. Dependencies on the
destination queue are omitted because queue submission order already
provides their execution dependency.
-}
submitCommandBuffersAfterLeased :: Queue -> [QueueDependency] -> Vector.Vector CommandBuffer.CommandBuffer -> IO Word64
submitCommandBuffersAfterLeased queue dependencies = submitCommandBuffersWithLeased queue dependencies [] []

{- | Submits command buffers with timeline dependencies and binary semaphore
waits/signals. The caller must already hold the owning Context lifecycle lease.
-}
submitCommandBuffersWithLeased :: Queue -> [QueueDependency] -> [BinarySemaphoreWait] -> [BinarySemaphoreSignal] -> Vector.Vector CommandBuffer.CommandBuffer -> IO Word64
submitCommandBuffersWithLeased queue dependencies binaryWaits binarySignals commandBuffers = do
  outcome <-
    submitCommandBuffersWithPublicationLeased
      queue
      dependencies
      binaryWaits
      binarySignals
      commandBuffers
      (const (pure ()))
  case outcome of
    SubmissionRejected failure -> throwIO failure
    SubmissionAcceptanceUnknown failure -> throwIO failure
    SubmissionAccepted timeline -> pure timeline
    SubmissionAcceptedPublicationFailed _ failure -> throwIO failure

{- | Submit once and publish caller-owned accepted-submission state before the
queue boundary is released. The tagged result preserves whether Vulkan rejected
the submission, accepted it before publication failed, or reported device loss
without guaranteeing whether the submission took effect.
-}
submitCommandBuffersWithPublicationLeased :: Queue -> [QueueDependency] -> [BinarySemaphoreWait] -> [BinarySemaphoreSignal] -> Vector.Vector CommandBuffer.CommandBuffer -> (Word64 -> IO ()) -> IO SubmissionPublicationOutcome
submitCommandBuffersWithPublicationLeased = submitCommandBuffersWithPublicationUsingLeased runVulkanQueueSubmit

submitCommandBuffersWithPublicationUsingLeased :: (IO () -> IO QueueSubmitDriverOutcome) -> Queue -> [QueueDependency] -> [BinarySemaphoreWait] -> [BinarySemaphoreSignal] -> Vector.Vector CommandBuffer.CommandBuffer -> (Word64 -> IO ()) -> IO SubmissionPublicationOutcome
submitCommandBuffersWithPublicationUsingLeased runQueueSubmit queue dependencies binaryWaits binarySignals commandBuffers publishAccepted = mask_ $ do
  prepared <- try (prepareDependencies queue dependencies)
  case prepared of
    Left (failure :: SomeException) -> pure (SubmissionRejected failure)
    Right waits -> do
      submitted <- try (withQueueHandleLockedLeased queue (const (submitLocked waits)))
      case submitted of
        Left (failure :: SomeException) -> pure (SubmissionRejected failure)
        Right outcome -> pure outcome
 where
  submitLocked waits = do
    modifyMVarMasked (queueState queue) $ \state -> do
      when (queueAcceptance state == QueueAcceptanceUnknown) $
        throwIO (VulkanFailure "queue submission" "a previous submission has unknown acceptance")
      completed <- mapVulkanFailure "vkGetSemaphoreCounterValue" (Timeline.getSemaphoreCounterValue (rawDevice queue) (timelineSemaphore queue))
      next <- either throwIO pure (nextTimelineSignalForTest (maximumTimelineDifference queue) completed (lastSubmittedTimelineValue state))
      let timelineSignal = Sync2.SemaphoreSubmitInfo (timelineSemaphore queue) next Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT 0
          binaryWaitInfos = map binaryWaitInfo binaryWaits
          binarySignalInfos = map binarySignalInfo binarySignals
          commandInfos = Vector.map (\commandBuffer -> Chain.SomeStruct (Sync2.CommandBufferSubmitInfo () (Handles.commandBufferHandle commandBuffer) 0)) commandBuffers
          submit = Sync2.SubmitInfo2 () zero (Vector.fromList (waits <> binaryWaitInfos)) commandInfos (Vector.fromList (timelineSignal : binarySignalInfos))
          submitToVulkan = Sync2.queueSubmit2 (rawQueue queue) (Vector.singleton (Chain.SomeStruct submit)) zero
      driverResult <- try (runQueueSubmit submitToVulkan)
      case driverResult of
        Left (failure :: SomeException) ->
          pure (state{queueAcceptance = QueueAcceptanceUnknown}, SubmissionAcceptanceUnknown failure)
        Right (QueueSubmitRejected failure) -> pure (state, SubmissionRejected failure)
        Right (QueueSubmitAcceptanceUnknown failure) ->
          pure (state{queueAcceptance = QueueAcceptanceUnknown}, SubmissionAcceptanceUnknown failure)
        Right QueueSubmitAccepted -> do
          publication <- try (publishAccepted next)
          let outcome = case publication of
                Left (failure :: SomeException) -> SubmissionAcceptedPublicationFailed next failure
                Right () -> SubmissionAccepted next
          pure (state{lastSubmittedTimelineValue = next}, outcome)

  binaryWaitInfo binaryWait =
    Sync2.SemaphoreSubmitInfo (binaryWaitSemaphore binaryWait) 0 (binaryWaitDestinationStage binaryWait) 0
  binarySignalInfo binarySignal =
    Sync2.SemaphoreSubmitInfo (binarySignalSemaphore binarySignal) 0 (binarySignalSourceStage binarySignal) 0

runVulkanQueueSubmit :: IO () -> IO QueueSubmitDriverOutcome
runVulkanQueueSubmit submitToVulkan = uninterruptibleMask_ $ do
  result <- try submitToVulkan
  pure $ case result of
    Right () -> QueueSubmitAccepted
    Left failure -> classifyVulkanQueueSubmitFailure failure

classifyVulkanQueueSubmitFailure :: SomeException -> QueueSubmitDriverOutcome
classifyVulkanQueueSubmitFailure failure =
  case fromException failure of
    Just error'
      | Vulkan.vulkanExceptionResult error' == Result.ERROR_DEVICE_LOST -> QueueSubmitAcceptanceUnknown (toException DeviceLost)
      | otherwise -> QueueSubmitRejected (toException (VulkanFailure "vkQueueSubmit2" (show (Vulkan.vulkanExceptionResult error'))))
    Nothing -> QueueSubmitAcceptanceUnknown failure

{- | Serializes access to the raw queue handle with submissions and shutdown.
The caller must hold an outer Context lifecycle lease.
-}
withQueueHandleLockedLeased :: Queue -> (Vk.Queue -> IO a) -> IO a
withQueueHandleLockedLeased queue action = withMVar (rawQueueSubmitLock queue) (const (action (rawQueue queue)))

prepareDependencies :: Queue -> [QueueDependency] -> IO [Sync2.SemaphoreSubmitInfo]
prepareDependencies destination dependencies = do
  combined <- foldM addDependency [] dependencies
  traverse validateDependency combined
 where
  addDependency combined dependency
    | sameQueue destination (dependencyQueue dependency) = pure combined
    | otherwise = pure (mergeDependency dependency combined)
  validateDependency dependency = do
    let source = dependencyQueue dependency
        requested = dependencyTimeline dependency
    sourceState <- readMVar (queueState source)
    when (queueAcceptance sourceState == QueueAcceptanceUnknown) $
      throwIO (VulkanFailure "queue dependency" "the source queue has a submission with unknown acceptance")
    let submitted = lastSubmittedTimelineValue sourceState
    when (requested > submitted) (throwIO (TimelineValueNotSubmitted requested submitted))
    pure (Sync2.SemaphoreSubmitInfo (timelineSemaphore source) requested (dependencyDestinationStage dependency) 0)

mergeDependency :: QueueDependency -> [QueueDependency] -> [QueueDependency]
mergeDependency requested dependencies = case dependencies of
  [] -> [requested]
  existing : rest
    | sameQueue (dependencyQueue requested) (dependencyQueue existing) ->
        existing
          { dependencyTimeline = max (dependencyTimeline requested) (dependencyTimeline existing)
          , dependencyDestinationStage = dependencyDestinationStage requested .|. dependencyDestinationStage existing
          }
          : rest
    | otherwise -> existing : mergeDependency requested rest

sameQueue :: Queue -> Queue -> Bool
sameQueue left right = timelineSemaphore left == timelineSemaphore right

waitTimeline :: Queue -> Word64 -> IO ()
waitTimeline queue value = withLifecycleLease (queueGate queue) (waitTimelineLeased queue value)

-- | Internal variant for callers which already hold the owning Context lease.
waitTimelineLeased :: Queue -> Word64 -> IO ()
waitTimelineLeased queue value = do
  submitted <- lastSubmittedTimelineValue <$> readMVar (queueState queue)
  when (value > submitted) (throwIO (TimelineValueNotSubmitted value submitted))
  wait
 where
  wait = do
    result <-
      mapVulkanFailure "vkWaitSemaphores" $
        Timeline.waitSemaphoresSafe
          (rawDevice queue)
          (Timeline.SemaphoreWaitInfo zero (Vector.singleton (timelineSemaphore queue)) (Vector.singleton value))
          timelineWaitPollTimeoutNanoseconds
    case result of
      Result.SUCCESS -> pure ()
      Result.TIMEOUT -> wait
      _ -> throwIO (VulkanFailure "vkWaitSemaphores" (show result))

-- Ten milliseconds balances cancellation responsiveness against driver-call
-- overhead during masked fallback waits.
timelineWaitPollTimeoutNanoseconds :: Word64
timelineWaitPollTimeoutNanoseconds = 10_000_000

timelineCompletedValue :: Queue -> IO Word64
timelineCompletedValue queue = withLifecycleLease (queueGate queue) (timelineCompletedValueLeased queue)

-- | Internal variant for callers which already hold the owning Context lease.
timelineCompletedValueLeased :: Queue -> IO Word64
timelineCompletedValueLeased queue =
  withMVar (queueState queue) $ \_ ->
    mapVulkanFailure "vkGetSemaphoreCounterValue" (Timeline.getSemaphoreCounterValue (rawDevice queue) (timelineSemaphore queue))

currentTimelineValueForTest :: Queue -> IO Word64
currentTimelineValueForTest queue = lastSubmittedTimelineValue <$> readMVar (queueState queue)

nextTimelineSignalForTest :: Word64 -> Word64 -> Word64 -> Either VpipeError Word64
nextTimelineSignalForTest maximumDifference completed submitted
  | submitted == maxBound = Left TimelineValueExhausted
  | pendingDifference > maximumDifference = Left (TimelineValueDifferenceExceeded completed next maximumDifference)
  | otherwise = Right next
 where
  next = submitted + 1
  pendingDifference = if next > completed then next - completed else 0

{- | Exercises a GPU timeline dependency between independently tracked queue
wrappers, even when the driver exposes only one raw queue.
-}
exerciseQueueDependencyForTest :: Queue -> IO (Word64, Word64)
exerciseQueueDependencyForTest queue =
  withLifecycleLease (queueGate queue) $
    bracket createAlias destroyAlias $ \alias -> do
      sourceSignal <- submitCommandBuffersLeased queue Vector.empty
      dependentSignal <-
        submitCommandBuffersAfterLeased
          alias
          [QueueDependency queue sourceSignal Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT]
          Vector.empty
      waitTimelineLeased alias dependentSignal
      pure (sourceSignal, dependentSignal)
 where
  createAlias = do
    semaphore <-
      mapVulkanFailure "vkCreateSemaphore(queue dependency test)" $
        Semaphore.createSemaphore
          (rawDevice queue)
          ( Semaphore.SemaphoreCreateInfo
              ( (zero :: Timeline.SemaphoreTypeCreateInfo)
                  { Timeline.semaphoreType = SemaphoreType.SEMAPHORE_TYPE_TIMELINE
                  , Timeline.initialValue = 0
                  }
                  Chain.:& ()
              )
              zero
          )
          Nothing
    let destroySemaphore = Semaphore.destroySemaphore (rawDevice queue) semaphore Nothing
        handle = semaphoreHandleWord semaphore
    nameQueueObject queue ObjectType.OBJECT_TYPE_SEMAPHORE "semaphore-queue-timeline-test" handle
      `onException` destroySemaphore
    newQueueWithSubmitLock
      (rawDevice queue)
      (rawQueue queue)
      (rawQueueSubmitLock queue)
      (familyIndex queue)
      semaphore
      (maximumTimelineDifference queue)
      (queueGate queue)
      (nameQueueObject queue)
      `onException` destroySemaphore
  destroyAlias alias = do
    submitted <- lastSubmittedTimelineValue <$> readMVar (queueState alias)
    when (submitted > 0) (waitTimelineLeased alias submitted)
    Semaphore.destroySemaphore (rawDevice alias) (timelineSemaphore alias) Nothing

-- | Exercises binary acquire/present-style semaphore chaining on one queue.
exerciseBinarySemaphoreChainForTest :: Queue -> IO Word64
exerciseBinarySemaphoreChainForTest queue =
  withLifecycleLease (queueGate queue) $
    bracket (createBinarySemaphore queue "semaphore-queue-binary-test-first") destroySemaphore $ \first ->
      bracket (createBinarySemaphore queue "semaphore-queue-binary-test-second") destroySemaphore $ \second -> do
        firstSignal <- submitCommandBuffersWithLeased queue [] [] [BinarySemaphoreSignal first Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT] Vector.empty
        secondSignal <- submitCommandBuffersWithLeased queue [] [BinarySemaphoreWait first Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT] [BinarySemaphoreSignal second Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT] Vector.empty
        finalSignal <- submitCommandBuffersWithLeased queue [] [BinarySemaphoreWait second Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT] [] Vector.empty
        waitTimelineLeased queue finalSignal
        when (secondSignal <= firstSignal) (throwIO (VulkanFailure "binary semaphore chain test" "timeline did not advance"))
        pure finalSignal
 where
  destroySemaphore semaphore = Semaphore.destroySemaphore (rawDevice queue) semaphore Nothing

createBinarySemaphore :: Queue -> String -> IO Vk.Semaphore
createBinarySemaphore queue category = do
  semaphore <-
    mapVulkanFailure "vkCreateSemaphore(binary queue test)" $
      Semaphore.createSemaphore (rawDevice queue) (Semaphore.SemaphoreCreateInfo () zero) Nothing
  let destroySemaphore = Semaphore.destroySemaphore (rawDevice queue) semaphore Nothing
      handle = semaphoreHandleWord semaphore
  nameQueueObject queue ObjectType.OBJECT_TYPE_SEMAPHORE category handle
    `onException` destroySemaphore
  pure semaphore

semaphoreHandleWord :: Vk.Semaphore -> Word64
semaphoreHandleWord (Vk.Semaphore handle) = handle

mapVulkanFailure :: String -> IO a -> IO a
mapVulkanFailure operation action =
  action `catch` \(error' :: Vulkan.VulkanException) ->
    if Vulkan.vulkanExceptionResult error' == Result.ERROR_DEVICE_LOST
      then throwIO DeviceLost
      else throwIO (VulkanFailure operation (show (Vulkan.vulkanExceptionResult error')))
