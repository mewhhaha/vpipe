{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Context resource creation is thread-safe. Queues are opaque and reject
submissions once their owning context begins closing.
-}
module Vpipe.Context.Internal (
  Context,
  VpipeConfig (..),
  StructuredLog (..),
  defaultVpipeConfig,
  withVpipe,
  withVpipeSurfacesInternal,
  registerContextFinalizer,
  registerContextFinalizerLeased,
  withContextLease,
  graphicsQueue,
  computeQueue,
  transferQueue,
  contextDeviceName,
  contextDeviceIsCpu,
  contextDevice,
  contextPhysicalDevice,
  contextAllocator,
  contextStagingRuntime,
  contextQueueFamilyIndices,
  contextQueueForFamily,
  contextOwnsSurface,
  contextUniformBufferOffsetAlignment,
  contextStorageBufferOffsetAlignment,
  contextNonCoherentAtomSize,
  contextMaxComputeWorkGroupCount,
  contextMaxComputeWorkGroupInvocations,
  contextMaxComputeWorkGroupSize,
  contextSamplerCache,
  contextSamplerAnisotropyEnabled,
  contextMaxSamplerAnisotropy,
  contextMaxSamplerLodBias,
  logImageSubresourceTransition,
  contextGraphicsCache,
  setObjectNameLeased,
  derivedObjectName,
  drainValidationMessages,
  Instance.DebugSink,
  Instance.DebugMessage (..),
  Instance.newDebugSink,
  Instance.popDebugMessage,
  Instance.debugSinkDropped,
  Instance.freeDebugSink,
  Instance.testDebugSinkCallback,
  runFinalizersForTest,
  runManagedForTest,
  runCleanupWithOuterForTest,
  runManagedWithOuterCleanupForTest,
  runContextShutdownForTest,
  ContextRelease (..),
  finishSurfaceContextRelease,
  reportPendingValidationMessages,
  resolveManagedOutcome,
  contextAllocationCountForTest,
  contextIdentity,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar)
import Control.Exception (AsyncException, SomeException, catch, fromException, mask, mask_, onException, throwIO, toException, try, uninterruptibleMask_)
import Control.Monad (unless, void, when)
import Control.Monad.Trans.Resource (InternalState, closeInternalState, createInternalState, register, runInternalState)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString8
import Data.Foldable (traverse_)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Unique (Unique, newUnique)
import Data.Word (Word32, Word64)
import Numeric (showHex)
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.APIConstants qualified as API
import Vulkan.Core10.Device qualified as Vk
import Vulkan.Core10.DeviceInitialization qualified as Init
import Vulkan.Core10.Enums.ImageLayout qualified as Layout
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.PhysicalDeviceType qualified as DeviceType
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Handles qualified as Vk
import Vulkan.Core10.Queue qualified as Vk
import Vulkan.Core10.QueueSemaphore qualified as Semaphore
import Vulkan.Core12.Enums.SemaphoreType qualified as SemaphoreType
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore qualified as Timeline
import Vulkan.Exception qualified as Vulkan
import Vulkan.Extensions.VK_EXT_debug_utils qualified as Debug
import Vulkan.Extensions.VK_KHR_surface qualified as Surface
import Vulkan.Zero (zero)
import VulkanMemoryAllocator qualified as VMA
import VulkanMemoryAllocator.Utils qualified as VMAUtils

import Vpipe.Buffer.Staging (StagingRuntime)
import Vpipe.Buffer.Staging qualified as Staging
import Vpipe.Context.Device qualified as Device
import Vpipe.Context.Instance qualified as Instance
import Vpipe.Context.Queue.Internal (LifecycleGate, Queue, QueueRole (..), closeLifecycleGate, newLifecycleGate, newQueue, queueFamilyIndex, withLifecycleLease, withQueuesLockedForShutdown)
import Vpipe.Error (DeviceRejection (..), VpipeError (..))
import Vpipe.Graphics.Cache.Internal (GraphicsCache, destroyGraphicsCache, newGraphicsCache)
import Vpipe.Graphics.Cache.Persistence (pipelineCacheFile, readPipelineCacheFile)
import Vpipe.Sampler.Types (SamplerCacheEntry, SamplerDescription)
import Vpipe.Surface.Internal (Surface, SurfaceFactory (..), SurfaceSource (..), newSurface, surfaceBelongsTo)

data StructuredLog = StructuredLog
  { logSeverity :: String
  , logType :: String
  , logMessageId :: String
  , logMessage :: String
  }
  deriving (Eq, Show)

data VpipeConfig = VpipeConfig
  { vpipeApplicationName :: ByteString
  , vpipeEnableValidation :: Bool
  , vpipeValidationStrict :: Bool
  , vpipeImageTransitionLogging :: Bool
  , vpipeLogger :: StructuredLog -> IO ()
  , vpipeDeviceScore :: Device.CandidateDevice -> Int
  , vpipeDeviceRequirements :: Device.CandidateDevice -> IO [String]
  , vpipeLogicalDeviceBuilder :: Device.LogicalDeviceBuilder
  , extraInstanceExtensions :: [ByteString]
  , extraDeviceExtensions :: [ByteString]
  }

defaultVpipeConfig :: VpipeConfig
defaultVpipeConfig =
  VpipeConfig
    { vpipeApplicationName = "vpipe"
    , vpipeEnableValidation = True
    , vpipeValidationStrict = False
    , vpipeImageTransitionLogging = False
    , vpipeLogger = \_ -> pure ()
    , vpipeDeviceScore = Device.candidateScore
    , vpipeDeviceRequirements = const (pure [])
    , vpipeLogicalDeviceBuilder = Device.defaultLogicalDeviceBuilder
    , extraInstanceExtensions = []
    , extraDeviceExtensions = []
    }

data Context = Context
  { ownedInstance :: Instance.InstanceOwner
  , ownedPhysicalDevice :: Vk.PhysicalDevice
  , ownedDevice :: Vk.Device
  , ownedAllocator :: VMA.Allocator
  , ownedStagingRuntime :: StagingRuntime
  , ownedGraphicsQueue :: Queue
  , ownedComputeQueue :: Queue
  , ownedTransferQueue :: Queue
  , ownedQueues :: [Queue]
  , resourceState :: InternalState
  , lifecycleGate :: LifecycleGate
  , cleanupErrors :: MVar [String]
  , contextLogger :: StructuredLog -> IO ()
  , contextValidationStrict :: Bool
  , contextImageTransitionLogging :: Bool
  , selectedDeviceName :: String
  , selectedDeviceIsCpu :: Bool
  , uniformBufferOffsetAlignment :: Word64
  , storageBufferOffsetAlignment :: Word64
  , nonCoherentAtomSize :: Word64
  , maxComputeWorkGroupCount :: (Word32, Word32, Word32)
  , maxComputeWorkGroupInvocations :: Word32
  , maxComputeWorkGroupSize :: (Word32, Word32, Word32)
  , ownedSamplerCache :: MVar (Map SamplerDescription SamplerCacheEntry)
  , samplerAnisotropyEnabled :: Bool
  , maxSamplerAnisotropy :: Float
  , maxSamplerLodBias :: Float
  , ownedGraphicsCache :: GraphicsCache
  , contextIdentity :: Unique
  }

graphicsQueue, computeQueue, transferQueue :: Context -> Queue
graphicsQueue = ownedGraphicsQueue
computeQueue = ownedComputeQueue
transferQueue = ownedTransferQueue

contextDeviceName :: Context -> String
contextDeviceName = selectedDeviceName

contextDeviceIsCpu :: Context -> Bool
contextDeviceIsCpu = selectedDeviceIsCpu

{- | Raw handles are exposed for resource modules; applications should use the
typed resource API rather than retaining them.
-}
contextDevice :: Context -> Vk.Device
contextDevice = ownedDevice

{- | Assign a debug-utils name while the caller holds the Context lease. The
extension is enabled opportunistically and this is a no-op when the loader does
not advertise it.
-}
setObjectNameLeased :: Context -> ObjectType.ObjectType -> Word64 -> ByteString -> IO ()
setObjectNameLeased context objectType objectHandle name =
  when (Instance.instanceDebugUtilsEnabled (ownedInstance context)) $
    mapVulkanOperation
      "vkSetDebugUtilsObjectNameEXT"
      ( Debug.setDebugUtilsObjectNameEXT
          (contextDevice context)
          (Debug.DebugUtilsObjectNameInfoEXT objectType objectHandle (Just name))
      )

derivedObjectName :: String -> Word64 -> ByteString
derivedObjectName category handle = ByteString8.pack (category <> ":" <> showHex handle "")

contextPhysicalDevice :: Context -> Vk.PhysicalDevice
contextPhysicalDevice = ownedPhysicalDevice

contextAllocator :: Context -> VMA.Allocator
contextAllocator = ownedAllocator

contextStagingRuntime :: Context -> StagingRuntime
contextStagingRuntime = ownedStagingRuntime

contextQueueFamilyIndices :: Context -> [Word32]
contextQueueFamilyIndices = fmap queueFamilyIndex . ownedQueues

contextQueueForFamily :: Context -> Word32 -> Maybe Queue
contextQueueForFamily context family = find ((== family) . queueFamilyIndex) (ownedQueues context)

contextOwnsSurface :: Context -> Surface -> Bool
contextOwnsSurface context = surfaceBelongsTo (contextIdentity context)

contextUniformBufferOffsetAlignment :: Context -> Word64
contextUniformBufferOffsetAlignment = uniformBufferOffsetAlignment

contextStorageBufferOffsetAlignment :: Context -> Word64
contextStorageBufferOffsetAlignment = storageBufferOffsetAlignment

contextNonCoherentAtomSize :: Context -> Word64
contextNonCoherentAtomSize = nonCoherentAtomSize

contextMaxComputeWorkGroupCount :: Context -> (Word32, Word32, Word32)
contextMaxComputeWorkGroupCount = maxComputeWorkGroupCount

contextMaxComputeWorkGroupInvocations :: Context -> Word32
contextMaxComputeWorkGroupInvocations = maxComputeWorkGroupInvocations

contextMaxComputeWorkGroupSize :: Context -> (Word32, Word32, Word32)
contextMaxComputeWorkGroupSize = maxComputeWorkGroupSize

contextSamplerCache :: Context -> MVar (Map SamplerDescription SamplerCacheEntry)
contextSamplerCache = ownedSamplerCache

contextSamplerAnisotropyEnabled :: Context -> Bool
contextSamplerAnisotropyEnabled = samplerAnisotropyEnabled

contextMaxSamplerAnisotropy :: Context -> Float
contextMaxSamplerAnisotropy = maxSamplerAnisotropy

contextMaxSamplerLodBias :: Context -> Float
contextMaxSamplerLodBias = maxSamplerLodBias

contextGraphicsCache :: Context -> GraphicsCache
contextGraphicsCache = ownedGraphicsCache

withContextLease :: Context -> IO a -> IO a
withContextLease context = withLifecycleLease (lifecycleGate context)

registerContextFinalizer :: Context -> IO () -> IO ()
registerContextFinalizer context finalizer =
  withContextLease context (registerContextFinalizerLeased context finalizer)

{- | Internal variant for resource constructors already protected by
'withContextLease'. It preserves the outer lease across Context shutdown.
-}
registerContextFinalizerLeased :: Context -> IO () -> IO ()
registerContextFinalizerLeased context finalizer =
  mask_ $
    void $
      runInternalState (register (recordCleanupError (cleanupErrors context) finalizer)) (resourceState context)

recordCleanupError :: MVar [String] -> IO () -> IO ()
recordCleanupError failures finalizer = do
  result <- try finalizer
  case result of
    Right () -> pure ()
    Left (error' :: SomeException) -> modifyMVar_ failures (pure . (show error' :))

withVpipe :: VpipeConfig -> (Context -> IO a) -> IO a
withVpipe config action = mask $ \restore -> do
  state <- createInternalState
  failures <- newMVar []
  contextResult <- try (acquireContext config state failures)
  case contextResult of
    Left (primary :: SomeException) -> do
      closeResult <- try (runCleanupWorker (closeInternalState state))
      case closeResult of
        Left (cleanup :: SomeException) -> modifyMVar_ failures (pure . (("resource cleanup: " <> show cleanup) :))
        Right () -> pure ()
      reportAcquisitionCleanup config failures
      throwIO primary
    Right context -> do
      outcome <- try (restore (action context))
      releaseResult <- releaseContext context
      resolveManagedOutcome outcome (contextReleaseOutcome releaseResult)

withVpipeSurfacesInternal :: VpipeConfig -> SurfaceFactory payload -> (Context -> NonEmpty Surface -> payload -> IO a) -> IO a
withVpipeSurfacesInternal config factory action = mask $ \restore -> do
  state <- createInternalState
  failures <- newMVar []
  let instanceExtensions = deduplicate (extraInstanceExtensions config <> surfaceFactoryExtensions factory)
  instanceOwner <- Instance.createVulkanInstance (vpipeApplicationName config) (vpipeEnableValidation config) (vpipeValidationStrict config) instanceExtensions
  let closeState = do
        closeResult <- try (runCleanupWorker (closeInternalState state))
        case closeResult of
          Left (error' :: SomeException) -> modifyMVar_ failures (pure . (("resource cleanup: " <> show error') :))
          Right () -> pure ()
      destroyInstance = Instance.destroyVulkanInstance instanceOwner
      recordInstanceCleanup = do
        result <- try destroyInstance
        case result of
          Left (error' :: SomeException) -> modifyMVar_ failures (pure . (("instance cleanup: " <> show error') :))
          Right () -> pure ()
      cleanupWithoutPayload = closeState >> recordInstanceCleanup
  acquired <- try (acquireSurfaces factory (Instance.instanceHandle instanceOwner))
  case acquired of
    Left (primary :: SomeException) -> do
      cleanupWithoutPayload
      reportAcquisitionCleanup config failures
      throwIO primary
    Right (payload, rawSurfaces) -> do
      let releasePayload = releaseSurfacePayload factory payload
          cleanup = do
            closeState
            payloadResult <- try releasePayload
            case payloadResult of
              Left (error' :: SomeException) -> modifyMVar_ failures (pure . (("surface payload cleanup: " <> show error') :))
              Right () -> pure ()
            recordInstanceCleanup
      contextResult <- try (acquireContextWithInstance config state failures instanceOwner (NonEmpty.toList rawSurfaces))
      case contextResult of
        Left (primary :: SomeException) -> cleanup >> reportAcquisitionCleanup config failures >> throwIO primary
        Right (context, surfaces) ->
          case NonEmpty.nonEmpty surfaces of
            Nothing -> do
              let primary = CleanupFailed ["surface acquisition produced no context surfaces"]
              cleanup
              reportAcquisitionCleanup config failures
              throwIO primary
            Just nonEmptySurfaces -> do
              outcome <- try (restore (action context nonEmptySurfaces payload))
              releaseResult <- releaseContext context
              cleanupResult <-
                finishSurfaceContextRelease
                  releaseResult
                  releasePayload
                  destroyInstance
                  (drainPostInstanceValidation context)
              resolveManagedOutcome outcome cleanupResult

acquireContext :: VpipeConfig -> InternalState -> MVar [String] -> IO Context
acquireContext config state cleanupFailures = do
  instanceOwner <-
    Instance.createVulkanInstance
      (vpipeApplicationName config)
      (vpipeEnableValidation config)
      (vpipeValidationStrict config)
      (extraInstanceExtensions config)
  fst <$> acquireContextWithInstance config state cleanupFailures instanceOwner []

acquireContextWithInstance :: VpipeConfig -> InternalState -> MVar [String] -> Instance.InstanceOwner -> [SurfaceSource] -> IO (Context, [Surface])
acquireContextWithInstance config state cleanupFailures instanceOwner rawSurfaces = do
  gate <- newLifecycleGate
  let registerOwned finalizer =
        void $ runInternalState (register (recordCleanupError cleanupFailures finalizer)) state
  when (null rawSurfaces) (registerOwned (Instance.destroyVulkanInstance instanceOwner))
  traverse_ (\surface -> registerOwned (Surface.destroySurfaceKHR (Instance.instanceHandle instanceOwner) (sourceHandle surface) Nothing)) rawSurfaces
  candidates <-
    mapEnumerationFailure $
      Device.enumerateCandidates (Instance.instanceHandle instanceOwner) (extraDeviceExtensions config) (map sourceHandle rawSurfaces)
  qualifiedCandidates <- traverse addConfiguredRequirements candidates
  selection <- either (throwIO . NoSuitableDevice) pure (Device.chooseDevice (vpipeDeviceScore config) qualifiedCandidates)
  let selected = Device.selectedDevice selection
  properties <- Init.getPhysicalDeviceProperties (Device.candidateHandle selected)
  let deviceLimits = Init.limits properties
  (device, rawQueues) <- vpipeLogicalDeviceBuilder config selected
  registerOwned (Vk.destroyDevice device Nothing)
  let setContextObjectName objectType category objectHandle =
        when (Instance.instanceDebugUtilsEnabled instanceOwner) $
          mapVulkanOperation
            "vkSetDebugUtilsObjectNameEXT"
            ( Debug.setDebugUtilsObjectNameEXT
                device
                (Debug.DebugUtilsObjectNameInfoEXT objectType objectHandle (Just (derivedObjectName category objectHandle)))
            )
      nameContextObject category (objectType, objectHandle) =
        setContextObjectName objectType category objectHandle
  nameContextObject "instance" (API.objectTypeAndHandle (Instance.instanceHandle instanceOwner))
  nameContextObject "device" (API.objectTypeAndHandle device)
  -- Some layer-owned messengers have no driver-visible handle; naming them crashes known VVL/lavapipe combinations.
  allocator <-
    mapVulkanOperation "vmaCreateAllocator" $
      VMA.createAllocator
        (VMAUtils.allocatorCreateInfo zero 4206592 (Instance.instanceHandle instanceOwner) (Device.candidateHandle selected) device)
  registerOwned (VMA.destroyAllocator allocator)
  graphicsFamily <- either throwIO pure (roleFamily GraphicsQueue selected)
  computeFamily <- either throwIO pure (roleFamily ComputeQueue selected)
  transferFamily <- either throwIO pure (roleFamily TransferQueue selected)
  let queueFor family = do
        rawQueue <-
          maybe
            (throwIO (CleanupFailed ["created device did not expose queue family " <> show family]))
            pure
            (lookup family rawQueues)
        nameContextObject ("queue-family-" <> show family) (API.objectTypeAndHandle rawQueue)
        timeline <- mapVulkanOperation "vkCreateSemaphore" (createTimelineSemaphore device)
        let timelineHandle = snd (API.objectTypeAndHandle timeline)
            destroyTimeline = Semaphore.destroySemaphore device timeline Nothing
        setContextObjectName ObjectType.OBJECT_TYPE_SEMAPHORE ("semaphore-queue-timeline-family-" <> show family) timelineHandle
          `onException` destroyTimeline
        registerOwned destroyTimeline `onException` destroyTimeline
        newQueue device rawQueue family timeline (Device.candidateMaxTimelineDifference selected) gate setContextObjectName
  allQueues <- traverse (\family -> (family,) <$> queueFor family) (Device.queueFamilyUnion selected)
  let lookupQueue family = maybe (throwIO (CleanupFailed ["created device did not expose queue family " <> show family])) pure (lookup family allQueues)
  graphics <- lookupQueue graphicsFamily
  compute <- lookupQueue computeFamily
  transfer <- lookupQueue transferFamily
  let setStagingObjectName objectType category objectHandle =
        when (Instance.instanceDebugUtilsEnabled instanceOwner) $
          mapVulkanOperation
            "vkSetDebugUtilsObjectNameEXT"
            ( Debug.setDebugUtilsObjectNameEXT
                device
                (Debug.DebugUtilsObjectNameInfoEXT objectType objectHandle (Just (derivedObjectName category objectHandle)))
            )
  staging <-
    mapVulkanOperation
      "create staging runtime"
      ( Staging.newStagingRuntime
          allocator
          device
          transfer
          setStagingObjectName
          (1024 * 1024)
          (Init.nonCoherentAtomSize deviceLimits)
      )
  registerOwned (Staging.destroyStagingRuntime staging)
    `onException` Staging.destroyStagingRuntime staging
  samplerCache <- newMVar Map.empty
  pipelineCachePath <- pipelineCacheFile (Init.pipelineCacheUUID properties)
  initialPipelineCache <- readPipelineCacheFile pipelineCachePath
  graphicsCache <- mapVulkanOperation "vkCreatePipelineCache" (newGraphicsCache device initialPipelineCache setContextObjectName)
  registerOwned (destroyGraphicsCache device pipelineCachePath graphicsCache)
    `onException` destroyGraphicsCache device pipelineCachePath graphicsCache
  identity <- newUnique
  let context =
        Context
          { ownedInstance = instanceOwner
          , ownedPhysicalDevice = Device.candidateHandle selected
          , ownedDevice = device
          , ownedAllocator = allocator
          , ownedStagingRuntime = staging
          , ownedGraphicsQueue = graphics
          , ownedComputeQueue = compute
          , ownedTransferQueue = transfer
          , ownedQueues = map snd allQueues
          , resourceState = state
          , lifecycleGate = gate
          , cleanupErrors = cleanupFailures
          , contextLogger = vpipeLogger config
          , contextValidationStrict = vpipeValidationStrict config
          , contextImageTransitionLogging = vpipeImageTransitionLogging config
          , selectedDeviceName = Device.candidateName selected
          , selectedDeviceIsCpu = Device.candidateDeviceType selected == DeviceType.PHYSICAL_DEVICE_TYPE_CPU
          , uniformBufferOffsetAlignment = Init.minUniformBufferOffsetAlignment deviceLimits
          , storageBufferOffsetAlignment = Init.minStorageBufferOffsetAlignment deviceLimits
          , nonCoherentAtomSize = Init.nonCoherentAtomSize deviceLimits
          , maxComputeWorkGroupCount = Init.maxComputeWorkGroupCount deviceLimits
          , maxComputeWorkGroupInvocations = Init.maxComputeWorkGroupInvocations deviceLimits
          , maxComputeWorkGroupSize = Init.maxComputeWorkGroupSize deviceLimits
          , ownedSamplerCache = samplerCache
          , samplerAnisotropyEnabled = Device.candidateSamplerAnisotropy selected
          , maxSamplerAnisotropy = Init.maxSamplerAnisotropy deviceLimits
          , maxSamplerLodBias = Init.maxSamplerLodBias deviceLimits
          , ownedGraphicsCache = graphicsCache
          , contextIdentity = identity
          }
  let presentFamilies = Device.candidatePresentFamilies selected
  when (length rawSurfaces /= length presentFamilies) $
    throwIO (CleanupFailed ["selected device returned a present-family count that does not match the acquired surfaces"])
  surfaces <- traverse (\(raw, family) -> newSurface identity raw <$> lookupQueue family) (zip rawSurfaces presentFamilies)
  pure (context, surfaces)
 where
  addConfiguredRequirements candidate = do
    reasons <- vpipeDeviceRequirements config candidate
    pure (Device.addCandidateRejections (const reasons) candidate)

data ContextRelease
  = ContextResourcesReleased (Either SomeException ())
  | ContextResourcesReleasedWithInterruption (Either SomeException ()) SomeException
  | ContextResourcesRetained SomeException

data ContextShutdown
  = ContextShutdownCompleted (Either SomeException ()) (Either SomeException ())
  | ContextShutdownUncertain SomeException

releaseContext :: Context -> IO ContextRelease
releaseContext context = mask_ $ do
  (shutdownResult, interruption) <-
    waitForCleanupWorker $ do
      closedGate <- closeLifecycleGate (lifecycleGate context)
      synchronizeAndCloseResources
        (withQueuesLockedForShutdown closedGate (ownedQueues context) (mapVulkanOperation "vkDeviceWaitIdle" (Vk.deviceWaitIdleSafe (ownedDevice context))))
        (closeInternalState (resourceState context))
  release <- case shutdownResult of
    Left error' -> retainContextAfterShutdownFailure context "context shutdown synchronization" error'
    Right (ContextShutdownUncertain idleError) -> do
      validationResult <- try (drainValidationMessagesUnlocked context) :: IO (Either SomeException ())
      recordReleaseException context "device idle" (Left idleError :: Either SomeException ())
      recordReleaseException context "validation drain" validationResult
      _ <- try (reportCleanupFailures context) :: IO (Either SomeException ())
      pure (ContextResourcesRetained (uncertainShutdownFailure "vkDeviceWaitIdle" idleError))
    Right (ContextShutdownCompleted idleResult closeResult) -> do
      validationResult <- try (drainValidationMessagesUnlocked context)
      recordReleaseException context "device idle" idleResult
      recordReleaseException context "resource cleanup" closeResult
      recordReleaseException context "validation drain" validationResult
      reportResult <- try (reportCleanupFailures context)
      failures <- readMVar (cleanupErrors context)
      pure . ContextResourcesReleased $ case (idleResult, validationResult, closeResult, reportResult, failures) of
        (Left error', _, _, _, _) -> Left error'
        (_, Left error', _, _, _) -> Left error'
        (_, _, Left error', _, _) -> Left error'
        (_, _, _, Left error', _) -> Left error'
        (_, _, _, _, []) -> Right ()
        (_, _, _, _, errors) -> Left (toException (CleanupFailed (reverse errors)))
  pure (deferInterruption interruption release)

contextReleaseOutcome :: ContextRelease -> Either SomeException ()
contextReleaseOutcome release = case release of
  ContextResourcesReleased result -> result
  ContextResourcesReleasedWithInterruption result interruption -> firstFailure [result, Left interruption]
  ContextResourcesRetained error' -> Left error'

finishSurfaceContextRelease :: ContextRelease -> IO () -> IO () -> IO () -> IO (Either SomeException ())
finishSurfaceContextRelease release releasePayload destroyInstance drainFinalValidation = case release of
  ContextResourcesRetained error' -> pure (Left error')
  ContextResourcesReleased contextReleaseResult -> do
    payloadResult <- try releasePayload
    instanceResult <- try destroyInstance
    validationResult <- try drainFinalValidation
    pure (firstFailure [contextReleaseResult, payloadResult, instanceResult, validationResult])
  ContextResourcesReleasedWithInterruption contextReleaseResult interruption -> do
    payloadResult <- try releasePayload
    instanceResult <- try destroyInstance
    validationResult <- try drainFinalValidation
    pure (firstFailure [contextReleaseResult, payloadResult, instanceResult, validationResult, Left interruption])

deferInterruption :: Maybe SomeException -> ContextRelease -> ContextRelease
deferInterruption Nothing release = release
deferInterruption (Just interruption) release = case release of
  ContextResourcesReleased result -> ContextResourcesReleasedWithInterruption result interruption
  ContextResourcesReleasedWithInterruption result earlierInterruption ->
    ContextResourcesReleasedWithInterruption (firstFailure [result, Left earlierInterruption]) interruption
  ContextResourcesRetained error' -> ContextResourcesRetained error'

retainContextAfterShutdownFailure :: Context -> String -> SomeException -> IO ContextRelease
retainContextAfterShutdownFailure context operation error' = do
  recordReleaseException context operation (Left error' :: Either SomeException ())
  _ <- try (reportCleanupFailures context) :: IO (Either SomeException ())
  pure (ContextResourcesRetained (uncertainShutdownFailure operation error'))

uncertainShutdownFailure :: String -> SomeException -> SomeException
uncertainShutdownFailure operation error' =
  toException . CleanupFailed $
    [ operation
        <> " failed before device quiescence was proven; the context device and child resources were retained to avoid destroying resources that may still be executing: "
        <> show error'
    ]

synchronizeAndCloseResources :: IO () -> IO () -> IO ContextShutdown
synchronizeAndCloseResources waitForDeviceIdle closeResources = do
  idleResult <- try waitForDeviceIdle
  case idleResult of
    Right () -> ContextShutdownCompleted idleResult <$> try closeResources
    Left error'
      | isDeviceLostException error' -> ContextShutdownCompleted idleResult <$> try closeResources
      | otherwise -> pure (ContextShutdownUncertain error')

isDeviceLostException :: SomeException -> Bool
isDeviceLostException error' = case fromException error' of
  Just DeviceLost -> True
  _ -> False

{- | The caller may be cancelled while waiting, but cleanup keeps ownership of
the context until the gate, device, and resource finalizers have completed.
-}
runCleanupWorker :: forall a. IO a -> IO a
runCleanupWorker cleanup = do
  (result, interruption) <- waitForCleanupWorker cleanup
  case result of
    Left cleanupError -> throwIO cleanupError
    Right value -> maybe (pure value) throwIO interruption

waitForCleanupWorker :: forall a. IO a -> IO (Either SomeException a, Maybe SomeException)
waitForCleanupWorker cleanup = mask $ \restore -> do
  completion <- newEmptyMVar
  void . forkIO . mask_ $ do
    result <- try cleanup :: IO (Either SomeException a)
    putMVar completion result
  waitResult <- try (restore (readMVar completion))
  case waitResult of
    Right result -> pure (result, Nothing)
    Left (interruption :: SomeException) -> do
      result <- uninterruptibleMask_ (readMVar completion)
      pure (result, Just interruption)

recordReleaseException :: (Show e) => Context -> String -> Either e () -> IO ()
recordReleaseException _ _ (Right ()) = pure ()
recordReleaseException context label (Left error') =
  modifyMVar_ (cleanupErrors context) (pure . ((label <> ": " <> show error') :))

reportCleanupFailures :: Context -> IO ()
reportCleanupFailures context = do
  failures <- reverse <$> readMVar (cleanupErrors context)
  traverse_ (deliverLog context . StructuredLog "error" "cleanup" "vpipe.cleanup") failures

reportAcquisitionCleanup :: VpipeConfig -> MVar [String] -> IO ()
reportAcquisitionCleanup config failures = do
  captured <- reverse <$> readMVar failures
  traverse_
    (void . tryLogger (vpipeLogger config) . StructuredLog "error" "cleanup" "vpipe.acquire.cleanup")
    captured

deliverLog :: Context -> StructuredLog -> IO ()
deliverLog context message = do
  result <- tryLogger (contextLogger context) message
  case result of
    Right () -> pure ()
    Left error' -> modifyMVar_ (cleanupErrors context) (pure . (("logger: " <> show error') :))

logImageSubresourceTransition :: Context -> Word64 -> Layout.ImageLayout -> Layout.ImageLayout -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
logImageSubresourceTransition context image oldLayout newLayout mipLevel mipCount arrayLayer arrayLayerCount
  | not (contextImageTransitionLogging context) = pure ()
  | oldLayout == newLayout = pure ()
  | otherwise =
      deliverLog
        context
        StructuredLog
          { logSeverity = "debug"
          , logType = "image-transition"
          , logMessageId = "vpipe.image.transition"
          , logMessage =
              "image=0x"
                <> showHex image ""
                <> " oldLayout="
                <> show oldLayout
                <> " newLayout="
                <> show newLayout
                <> " mipRange="
                <> show mipLevel
                <> "+"
                <> show mipCount
                <> " layerRange="
                <> show arrayLayer
                <> "+"
                <> show arrayLayerCount
          }

tryLogger :: (StructuredLog -> IO ()) -> StructuredLog -> IO (Either SomeException ())
tryLogger logger message = do
  result <- try (logger message)
  case result of
    Left error'
      | Just asynchronous <- fromException error' -> throwIO (asynchronous :: AsyncException)
    _ -> pure result

drainValidationMessages :: Context -> IO ()
drainValidationMessages context = withContextLease context (drainValidationMessagesUnlocked context)

drainValidationMessagesUnlocked :: Context -> IO ()
drainValidationMessagesUnlocked context = do
  traverse_
    (deliverLog context . StructuredLog "warning" "validation-setup" "vpipe.validation.unavailable")
    (Instance.instanceValidationNotice (ownedInstance context))
  drainPendingValidationMessagesUnlocked context

drainPendingValidationMessagesUnlocked :: Context -> IO ()
drainPendingValidationMessagesUnlocked context =
  reportPendingValidationMessages
    (contextValidationStrict context)
    (Instance.drainInstanceDebugMessages (ownedInstance context))
    (deliverLog context)

reportPendingValidationMessages :: Bool -> IO ([Instance.DebugMessage], Word64) -> (StructuredLog -> IO ()) -> IO ()
reportPendingValidationMessages validationStrict drainPending deliver = do
  (messages, dropped) <- drainPending
  traverse_ (deliver . validationLog) messages
  when (dropped > 0) $
    deliver (StructuredLog "warning" "validation" "vpipe.validation.dropped" (show dropped <> " validation messages were dropped"))
  when (validationStrict && (not (null messages) || dropped > 0)) $
    throwIO (ValidationFailed (length messages) dropped)
 where
  validationLog message =
    StructuredLog
      { logSeverity = show (Instance.debugSeverity message)
      , logType = show (Instance.debugType message)
      , logMessageId = Instance.debugMessageId message
      , logMessage = Instance.debugMessageText message
      }

drainPostInstanceValidation :: Context -> IO ()
drainPostInstanceValidation context = do
  validationResult <- try (drainPendingValidationMessagesUnlocked context)
  failures <- reverse <$> readMVar (cleanupErrors context)
  let recordedFailure =
        if null failures
          then Right ()
          else Left (toException (CleanupFailed failures))
  either throwIO pure (firstFailure [validationResult, recordedFailure])

resolveManagedOutcome :: Either SomeException a -> Either SomeException () -> IO a
resolveManagedOutcome (Left primary) _ = throwIO primary
resolveManagedOutcome (Right _) (Left cleanup) = throwIO cleanup
resolveManagedOutcome (Right value) (Right ()) = pure value

firstFailure :: [Either SomeException ()] -> Either SomeException ()
firstFailure = foldr step (Right ())
 where
  step result rest = case result of
    Left error' -> Left error'
    Right () -> rest

runFinalizersForTest :: [IO ()] -> IO [String]
runFinalizersForTest finalizers = runCleanupWorker $ mask_ $ do
  state <- createInternalState
  failures <- newMVar []
  traverse_ (\finalizer -> void $ runInternalState (register (recordCleanupError failures finalizer)) state) finalizers
  closeInternalState state
  reverse <$> readMVar failures

runManagedForTest :: IO a -> [IO ()] -> IO a
runManagedForTest action finalizers = mask $ \restore -> do
  outcome <- try (restore action)
  cleanupResult <- try $ do
    failures <- runFinalizersForTest finalizers
    unless (null failures) (throwIO (CleanupFailed failures))
  resolveManagedOutcome outcome cleanupResult

runCleanupWithOuterForTest :: IO () -> IO () -> IO () -> IO () -> IO ()
runCleanupWithOuterForTest inner releasePayload destroyInstance drainFinalValidation = do
  (innerResult, interruption) <- waitForCleanupWorker inner
  let release = deferInterruption interruption $ case innerResult of
        Left error' -> ContextResourcesRetained error'
        Right () -> ContextResourcesReleased (Right ())
  cleanupResult <- finishSurfaceContextRelease release releasePayload destroyInstance drainFinalValidation
  either throwIO pure cleanupResult

runContextShutdownForTest :: IO () -> IO () -> IO ()
runContextShutdownForTest waitForDeviceIdle closeResources = do
  shutdown <- runCleanupWorker (synchronizeAndCloseResources waitForDeviceIdle closeResources)
  case shutdown of
    ContextShutdownCompleted (Left idleError) _ -> throwIO idleError
    ContextShutdownCompleted _ (Left cleanupError) -> throwIO cleanupError
    ContextShutdownCompleted _ _ -> pure ()
    ContextShutdownUncertain idleError -> throwIO (uncertainShutdownFailure "test device idle wait" idleError)

runManagedWithOuterCleanupForTest :: IO a -> IO () -> IO () -> IO a
runManagedWithOuterCleanupForTest action inner outer = mask $ \restore -> do
  outcome <- try (restore action)
  innerResult <- try (runCleanupWorker inner)
  outerResult <- try outer
  resolveManagedOutcome outcome (firstFailure [innerResult, outerResult])

contextAllocationCountForTest :: Context -> IO Word32
contextAllocationCountForTest context = withContextLease context $ do
  VMA.TotalStatistics _ _ (VMA.DetailedStatistics (VMA.Statistics _ allocationCount _ _) _ _ _ _ _) <-
    VMA.calculateStatistics (ownedAllocator context)
  pure allocationCount

deduplicate :: (Eq a) => [a] -> [a]
deduplicate = foldl add []
 where
  add values value
    | value `elem` values = values
    | otherwise = values <> [value]

roleFamily :: QueueRole -> Device.CandidateDevice -> Either VpipeError Word32
roleFamily role candidate = case Device.candidateGraphicsFamily candidate of
  Nothing -> Left (NoSuitableDevice [DeviceRejection (Device.candidateName candidate) ["no graphics-capable queue family"]])
  Just graphics -> Right $ case role of
    GraphicsQueue -> graphics
    ComputeQueue -> fromMaybe graphics (Device.candidateComputeFamily candidate)
    TransferQueue -> fromMaybe graphics (Device.candidateTransferFamily candidate)

createTimelineSemaphore :: Vk.Device -> IO Vk.Semaphore
createTimelineSemaphore device =
  Semaphore.createSemaphore
    device
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

mapEnumerationFailure :: IO a -> IO a
mapEnumerationFailure action =
  action `catch` \(error' :: Vulkan.VulkanException) ->
    if Vulkan.vulkanExceptionResult error' == Result.ERROR_INITIALIZATION_FAILED
      then throwIO (NoVulkanIcd (show error'))
      else
        if Vulkan.vulkanExceptionResult error' == Result.ERROR_SURFACE_LOST_KHR
          then throwIO SurfaceLost
          else throwIO (VulkanFailure "Vulkan physical-device enumeration" (show (Vulkan.vulkanExceptionResult error')))

mapVulkanOperation :: String -> IO a -> IO a
mapVulkanOperation operation action =
  action `catch` \(error' :: Vulkan.VulkanException) ->
    if Vulkan.vulkanExceptionResult error' == Result.ERROR_DEVICE_LOST
      then throwIO DeviceLost
      else throwIO (VulkanFailure operation (show (Vulkan.vulkanExceptionResult error')))
