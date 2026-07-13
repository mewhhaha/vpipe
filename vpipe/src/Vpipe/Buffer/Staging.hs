{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Context-owned upload memory and command-buffer retirement.
module Vpipe.Buffer.Staging (
  StagingRuntime,
  StagingSubmissionOutcome (..),
  newStagingRuntime,
  destroyStagingRuntime,
  stagingCapacity,
  submitUploadAfter,
  submitCopyAfter,
  submitUploadCommandAfter,
  submitCommandAfter,
  reclaimStaging,
  RingReservation (..),
  planRingReservation,
  retirementActionForTest,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked_, newMVar, readMVar, withMVar)
import Control.Exception (SomeException, catch, mask, mask_, onException, throwIO)
import Control.Monad (when)
import Data.Bits ((.|.))
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Foreign.Ptr (Ptr, nullPtr, plusPtr, ptrToWordPtr)
import Vulkan.Core10.Buffer qualified as Buffer
import Vulkan.Core10.CommandBuffer qualified as CommandBuffer
import Vulkan.Core10.CommandBufferBuilding qualified as CommandBuilding
import Vulkan.Core10.CommandPool qualified as CommandPool
import Vulkan.Core10.Enums.BufferUsageFlagBits qualified as BufferUsage
import Vulkan.Core10.Enums.CommandBufferLevel qualified as CommandLevel
import Vulkan.Core10.Enums.CommandBufferUsageFlagBits qualified as CommandUsage
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits qualified as PoolUsage
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Handles qualified as Vk
import Vulkan.Core13.Enums.AccessFlags2 qualified as Access2
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 qualified as Sync2
import Vulkan.Exception qualified as Vulkan
import Vulkan.Zero (zero)
import VulkanMemoryAllocator qualified as VMA

import Vpipe.Context.Queue.Internal (Queue, QueueDependency, SubmissionPublicationOutcome (..), queueFamilyIndex, submitCommandBuffersWithPublicationLeased, timelineCompletedValueLeased, waitTimelineLeased)
import Vpipe.Error (VpipeError (..))

data StagingRuntime = StagingRuntime
  { runtimeAllocator :: VMA.Allocator
  , runtimeDevice :: Vk.Device
  , runtimeQueue :: Queue
  , runtimeBuffer :: Vk.Buffer
  , runtimeAllocation :: VMA.Allocation
  , runtimePointer :: Ptr ()
  , runtimeCapacity :: Int
  , runtimeNonCoherentAtomSize :: Int
  , runtimeCommandPool :: Vk.CommandPool
  , runtimeSetObjectName :: ObjectNameSetter
  , runtimeOperationLock :: MVar ()
  , runtimeState :: MVar StagingState
  }

type ObjectNameSetter = ObjectType.ObjectType -> String -> Word64 -> IO ()

data OneShotCleanup = OneShotCleanup (MVar Bool) (IO ())

data SubmissionResources = SubmissionResources
  { submissionCleanup :: OneShotCleanup
  , submissionCommandBufferCleanup :: OneShotCleanup
  }

data PendingUpload = PendingUpload
  { pendingRingStart :: Maybe Int
  , pendingTimeline :: Word64
  , pendingResources :: SubmissionResources
  }

newtype QuarantinedUpload = QuarantinedUpload
  { quarantinedResources :: SubmissionResources
  }

data StagingHealth
  = StagingHealthy
  | StagingPoisoned SomeException

data StagingState = StagingState
  { nextRingOffset :: Int
  , pendingUploads :: [PendingUpload]
  , quarantinedUploads :: [QuarantinedUpload]
  , stagingHealth :: StagingHealth
  }

data StagingSubmissionOutcome
  = StagingSubmissionAccepted Word64
  | StagingSubmissionAcceptanceUnknown SomeException
  | StagingSubmissionAcceptedPublicationFailed Word64 SomeException

newStagingRuntime :: VMA.Allocator -> Vk.Device -> Queue -> ObjectNameSetter -> Int -> Word64 -> IO StagingRuntime
newStagingRuntime allocator device queue setObjectName capacity nonCoherentAtomSize
  | capacity <= 0 = throwIO (VulkanFailure "staging runtime" "capacity must be positive")
  | otherwise = mask $ \_ -> do
      atomSize <- checkedDeviceSize "non-coherent atom size" nonCoherentAtomSize
      let createInfo =
            (zero :: Buffer.BufferCreateInfo '[])
              { Buffer.size = fromIntegral capacity
              , Buffer.usage = BufferUsage.BUFFER_USAGE_TRANSFER_SRC_BIT
              }
          allocationInfo =
            (zero :: VMA.AllocationCreateInfo)
              { VMA.usage = VMA.MEMORY_USAGE_AUTO_PREFER_HOST
              , VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT .|. VMA.ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
              }
      (buffer, allocation, information) <- mapVulkanFailure "vmaCreateBuffer(staging ring)" (VMA.createBuffer allocator createInfo allocationInfo)
      let pointer = VMA.mappedData information
          destroyBuffer = VMA.destroyBuffer allocator buffer allocation
      setObjectName ObjectType.OBJECT_TYPE_BUFFER "buffer-staging-ring" (bufferHandleWord buffer)
        `onException` destroyBuffer
      when (pointer == nullPtr) $ do
        destroyBuffer
        throwIO (VulkanFailure "vmaCreateBuffer(staging ring)" "mapped allocation returned a null pointer")
      let poolInfo =
            (zero :: CommandPool.CommandPoolCreateInfo)
              { CommandPool.flags = PoolUsage.COMMAND_POOL_CREATE_TRANSIENT_BIT
              , CommandPool.queueFamilyIndex = queueFamilyIndex queue
              }
      pool <- mapVulkanFailure "vkCreateCommandPool(staging)" (CommandPool.createCommandPool device poolInfo Nothing) `onException` destroyBuffer
      let destroyPool = CommandPool.destroyCommandPool device pool Nothing
      setObjectName ObjectType.OBJECT_TYPE_COMMAND_POOL "command-pool-staging" (commandPoolHandleWord pool)
        `onException` (destroyPool >> destroyBuffer)
      operationLock <- newMVar ()
      state <- newMVar (StagingState 0 [] [] StagingHealthy)
      pure
        StagingRuntime
          { runtimeAllocator = allocator
          , runtimeDevice = device
          , runtimeQueue = queue
          , runtimeBuffer = buffer
          , runtimeAllocation = allocation
          , runtimePointer = pointer
          , runtimeCapacity = capacity
          , runtimeNonCoherentAtomSize = atomSize
          , runtimeCommandPool = pool
          , runtimeSetObjectName = setObjectName
          , runtimeOperationLock = operationLock
          , runtimeState = state
          }

destroyStagingRuntime :: StagingRuntime -> IO ()
destroyStagingRuntime runtime = mask_ $
  withMVar (runtimeOperationLock runtime) $ \_ -> do
    state <- readMVar (runtimeState runtime)
    mapM_ retirePendingUpload (pendingUploads state)
    mapM_ (retireSubmissionResources . quarantinedResources) (quarantinedUploads state)
    CommandPool.destroyCommandPool (runtimeDevice runtime) (runtimeCommandPool runtime) Nothing
    VMA.destroyBuffer (runtimeAllocator runtime) (runtimeBuffer runtime) (runtimeAllocation runtime)
    modifyMVarMasked_ (runtimeState runtime) (const (pure (StagingState 0 [] [] StagingHealthy)))

stagingCapacity :: StagingRuntime -> Int
stagingCapacity = runtimeCapacity

{- | Marshals bytes into the persistent ring (or an oversized one-shot
allocation), records an optional destination barrier, and submits a copy.
An accepted outcome's timeline value owns the source range until retirement.
-}
submitUploadAfter ::
  StagingRuntime ->
  [QueueDependency] ->
  Int ->
  Int ->
  (Ptr () -> IO ()) ->
  (Vk.CommandBuffer -> IO ()) ->
  Vk.Buffer ->
  Int ->
  IO StagingSubmissionOutcome
submitUploadAfter runtime dependencies alignment byteCount marshal recordBarrier destination destinationOffset
  | byteCount <= 0 = throwIO (VulkanFailure "staging upload" "copy size must be positive")
  | otherwise = mask $ \restore ->
      withHealthyOperation runtime $ do
        reclaimed <- reclaimCompleted runtime
        occupiedBytes <- checkedOccupiedBytes runtime byteCount
        if occupiedBytes > runtimeCapacity runtime
          then submitDedicated restore reclaimed
          else do
            (reserved, sourceOffset) <- reserveRing runtime alignment byteCount reclaimed
            restore (marshal (runtimePointer runtime `plusPtr` sourceOffset))
            mapVulkanFailure
              "vmaFlushAllocation(staging ring)"
              (VMA.flushAllocation (runtimeAllocator runtime) (runtimeAllocation runtime) (fromIntegral sourceOffset) (fromIntegral byteCount))
            cleanup <- newOneShotCleanup (pure ())
            recordCopyAfter runtime reserved (Just sourceOffset) cleanup dependencies recordBarrier (const (pure ())) (runtimeBuffer runtime) sourceOffset destination destinationOffset byteCount
 where
  submitDedicated restore state = do
    let allocator = runtimeAllocator runtime
        createInfo =
          (zero :: Buffer.BufferCreateInfo '[])
            { Buffer.size = fromIntegral byteCount
            , Buffer.usage = BufferUsage.BUFFER_USAGE_TRANSFER_SRC_BIT
            }
        allocationInfo =
          (zero :: VMA.AllocationCreateInfo)
            { VMA.usage = VMA.MEMORY_USAGE_AUTO_PREFER_HOST
            , VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT .|. VMA.ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
            }
    (buffer, allocation, information) <- mapVulkanFailure "vmaCreateBuffer(oversized staging)" (VMA.createBuffer allocator createInfo allocationInfo)
    cleanup <- newOneShotCleanup (VMA.destroyBuffer allocator buffer allocation)
    let releaseBuffer = runOneShotCleanup cleanup
        pointer = VMA.mappedData information
    runtimeSetObjectName runtime ObjectType.OBJECT_TYPE_BUFFER "buffer-staging-oversized" (bufferHandleWord buffer)
      `onException` releaseBuffer
    when (pointer == nullPtr) $ do
      releaseBuffer
      throwIO (VulkanFailure "vmaCreateBuffer(oversized staging)" "mapped allocation returned a null pointer")
    ( do
        restore (marshal pointer)
        mapVulkanFailure "vmaFlushAllocation(oversized staging)" (VMA.flushAllocation allocator allocation 0 (fromIntegral byteCount))
        recordCopyAfter runtime state Nothing cleanup dependencies recordBarrier (const (pure ())) buffer 0 destination destinationOffset byteCount
      )
      `onException` releaseBuffer

{- | Submits an arbitrary buffer copy through the context-owned command pool.
The caller owns both buffers and must retain them through the returned
accepted timeline point.
-}
submitCopyAfter ::
  StagingRuntime ->
  [QueueDependency] ->
  (Vk.CommandBuffer -> IO ()) ->
  (Vk.CommandBuffer -> IO ()) ->
  Vk.Buffer ->
  Int ->
  Vk.Buffer ->
  Int ->
  Int ->
  IO () ->
  IO StagingSubmissionOutcome
submitCopyAfter runtime dependencies recordBarrier recordCompletionBarrier source sourceOffset destination destinationOffset byteCount retire
  | byteCount <= 0 = throwIO (VulkanFailure "buffer copy" "copy size must be positive")
  | otherwise = mask $ \_ ->
      withHealthyOperation runtime $ do
        reclaimed <- reclaimCompleted runtime
        cleanup <- newOneShotCleanup retire
        recordCopyAfter runtime reclaimed Nothing cleanup dependencies recordBarrier recordCompletionBarrier source sourceOffset destination destinationOffset byteCount

{- | Uploads bytes through the persistent ring (or a dedicated oversized
allocation) and lets the caller record the command that consumes them.
-}

-- | Uploads bytes after the supplied GPU timeline dependencies.
submitUploadCommandAfter :: StagingRuntime -> [QueueDependency] -> Int -> Int -> (Ptr () -> IO ()) -> (Vk.Buffer -> Int -> Vk.CommandBuffer -> IO ()) -> IO StagingSubmissionOutcome
submitUploadCommandAfter runtime dependencies alignment byteCount marshal record
  | byteCount <= 0 = throwIO (VulkanFailure "staging upload command" "copy size must be positive")
  | otherwise = mask $ \restore ->
      withHealthyOperation runtime $ do
        reclaimed <- reclaimCompleted runtime
        occupiedBytes <- checkedOccupiedBytes runtime byteCount
        if occupiedBytes > runtimeCapacity runtime
          then submitDedicated restore reclaimed
          else do
            (reserved, sourceOffset) <- reserveRing runtime alignment byteCount reclaimed
            restore (marshal (runtimePointer runtime `plusPtr` sourceOffset))
            mapVulkanFailure "vmaFlushAllocation(staging ring)" (VMA.flushAllocation (runtimeAllocator runtime) (runtimeAllocation runtime) (fromIntegral sourceOffset) (fromIntegral byteCount))
            cleanup <- newOneShotCleanup (pure ())
            recordCommandAfter runtime reserved (Just sourceOffset) cleanup dependencies $ \commandBuffer -> do
              recordAllocationAliasBarrier commandBuffer
              record (runtimeBuffer runtime) sourceOffset commandBuffer
 where
  submitDedicated restore state = do
    let allocator = runtimeAllocator runtime
        createInfo = (zero :: Buffer.BufferCreateInfo '[]){Buffer.size = fromIntegral byteCount, Buffer.usage = BufferUsage.BUFFER_USAGE_TRANSFER_SRC_BIT}
        allocationInfo = (zero :: VMA.AllocationCreateInfo){VMA.usage = VMA.MEMORY_USAGE_AUTO_PREFER_HOST, VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT .|. VMA.ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT}
    (buffer, allocation, information) <- mapVulkanFailure "vmaCreateBuffer(image upload)" (VMA.createBuffer allocator createInfo allocationInfo)
    cleanup <- newOneShotCleanup (VMA.destroyBuffer allocator buffer allocation)
    let releaseBuffer = runOneShotCleanup cleanup
        pointer = VMA.mappedData information
    runtimeSetObjectName runtime ObjectType.OBJECT_TYPE_BUFFER "buffer-staging-oversized" (bufferHandleWord buffer)
      `onException` releaseBuffer
    when (pointer == nullPtr) $ releaseBuffer >> throwIO (VulkanFailure "vmaCreateBuffer(image upload)" "mapped allocation returned a null pointer")
    ( do
        restore (marshal pointer)
        mapVulkanFailure "vmaFlushAllocation(image upload)" (VMA.flushAllocation allocator allocation 0 (fromIntegral byteCount))
        recordCommandAfter runtime state Nothing cleanup dependencies $ \commandBuffer -> do
          recordAllocationAliasBarrier commandBuffer
          record buffer 0 commandBuffer
      )
      `onException` releaseBuffer

{- | Records and submits commands which do not need staging memory.  The
optional retirement action runs after the queue timeline reaches the signal.
-}

-- | Records and submits commands after the supplied GPU timeline dependencies.
submitCommandAfter :: StagingRuntime -> [QueueDependency] -> (Vk.CommandBuffer -> IO ()) -> IO () -> IO StagingSubmissionOutcome
submitCommandAfter runtime dependencies record retire = mask $ \_ ->
  withHealthyOperation runtime $ do
    reclaimed <- reclaimCompleted runtime
    cleanup <- newOneShotCleanup retire
    recordCommandAfter runtime reclaimed Nothing cleanup dependencies record

reclaimStaging :: StagingRuntime -> IO ()
reclaimStaging runtime = mask_ $
  withHealthyOperation runtime $ do
    _ <- reclaimCompleted runtime
    pure ()

recordCopyAfter ::
  StagingRuntime ->
  StagingState ->
  Maybe Int ->
  OneShotCleanup ->
  [QueueDependency] ->
  (Vk.CommandBuffer -> IO ()) ->
  (Vk.CommandBuffer -> IO ()) ->
  Vk.Buffer ->
  Int ->
  Vk.Buffer ->
  Int ->
  Int ->
  IO StagingSubmissionOutcome
recordCopyAfter runtime state ringStart cleanup dependencies recordBarrier recordCompletionBarrier source sourceOffset destination destinationOffset byteCount = do
  recordCommandAfter runtime state ringStart cleanup dependencies $ \commandBuffer -> do
    recordAllocationAliasBarrier commandBuffer
    recordBarrier commandBuffer
    CommandBuilding.cmdCopyBuffer
      commandBuffer
      source
      destination
      (Vector.singleton (CommandBuilding.BufferCopy (fromIntegral sourceOffset) (fromIntegral destinationOffset) (fromIntegral byteCount)))
    recordCompletionBarrier commandBuffer

recordAllocationAliasBarrier :: Vk.CommandBuffer -> IO ()
recordAllocationAliasBarrier commandBuffer = do
  -- VMA may reuse a retired allocation at the same memory range. Queue order
  -- alone does not make earlier accesses available to the new resource.
  let barrier =
        Sync2.MemoryBarrier2
          { Sync2.srcStageMask = Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT
          , Sync2.srcAccessMask = Access2.ACCESS_2_MEMORY_READ_BIT .|. Access2.ACCESS_2_MEMORY_WRITE_BIT
          , Sync2.dstStageMask = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
          , Sync2.dstAccessMask = Access2.ACCESS_2_TRANSFER_READ_BIT .|. Access2.ACCESS_2_TRANSFER_WRITE_BIT
          }
  Sync2.cmdPipelineBarrier2 commandBuffer (Sync2.DependencyInfo zero (Vector.singleton barrier) Vector.empty Vector.empty)

recordCommandAfter :: StagingRuntime -> StagingState -> Maybe Int -> OneShotCleanup -> [QueueDependency] -> (Vk.CommandBuffer -> IO ()) -> IO StagingSubmissionOutcome
recordCommandAfter runtime state ringStart cleanup dependencies record = do
  commandBuffers <-
    mapVulkanFailure
      "vkAllocateCommandBuffers(staging)"
      ( CommandBuffer.allocateCommandBuffers
          (runtimeDevice runtime)
          (CommandBuffer.CommandBufferAllocateInfo (runtimeCommandPool runtime) CommandLevel.COMMAND_BUFFER_LEVEL_PRIMARY 1)
      )
  case Vector.toList commandBuffers of
    [commandBuffer] -> do
      commandBufferCleanup <-
        newOneShotCleanup
          (CommandBuffer.freeCommandBuffers (runtimeDevice runtime) (runtimeCommandPool runtime) (Vector.singleton commandBuffer))
      let resources = SubmissionResources cleanup commandBufferCleanup
          prepareCommands = do
            mapVulkanFailure
              "vkBeginCommandBuffer(staging)"
              ( CommandBuffer.beginCommandBuffer
                  commandBuffer
                  ((zero :: CommandBuffer.CommandBufferBeginInfo '[]){CommandBuffer.flags = CommandUsage.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT})
              )
            record commandBuffer
            mapVulkanFailure "vkEndCommandBuffer(staging)" (CommandBuffer.endCommandBuffer commandBuffer)
          freeCommandBuffer = runOneShotCleanup commandBufferCleanup
      runtimeSetObjectName runtime ObjectType.OBJECT_TYPE_COMMAND_BUFFER "command-buffer-staging" (commandBufferHandleWord commandBuffer)
        `onException` freeCommandBuffer
      prepareCommands `onException` freeCommandBuffer
      outcome <-
        submitCommandBuffersWithPublicationLeased
          (runtimeQueue runtime)
          dependencies
          []
          []
          (Vector.singleton commandBuffer)
          (publishAcceptedSubmission runtime state ringStart resources)
      case outcome of
        SubmissionRejected failure -> do
          freeCommandBuffer
          throwIO failure
        SubmissionAcceptanceUnknown failure -> do
          publishUnknownSubmission runtime state resources failure
          pure (StagingSubmissionAcceptanceUnknown failure)
        SubmissionAccepted timeline -> pure (StagingSubmissionAccepted timeline)
        SubmissionAcceptedPublicationFailed timeline primaryFailure -> do
          publishAcceptedSubmission runtime state ringStart resources timeline
          pure (StagingSubmissionAcceptedPublicationFailed timeline primaryFailure)
    values -> do
      CommandBuffer.freeCommandBuffers (runtimeDevice runtime) (runtimeCommandPool runtime) commandBuffers
      throwIO (VulkanFailure "vkAllocateCommandBuffers(staging)" ("expected one command buffer, received " <> show (length values)))

reserveRing :: StagingRuntime -> Int -> Int -> StagingState -> IO (StagingState, Int)
reserveRing runtime requestedAlignment byteCount state =
  case planRingReservation capacity atomSize requestedAlignment byteCount headOffset tailOffset of
    Just reservation ->
      pure
        ( state{nextRingOffset = ringReservationNextOffset reservation}
        , ringReservationOffset reservation
        )
    Nothing -> case pendingUploads state of
      [] -> throwIO (VulkanFailure "staging ring" "failed to reserve an empty ring")
      oldest : _ -> do
        waitTimelineLeased (runtimeQueue runtime) (pendingTimeline oldest)
        reclaimed <- reclaimCompleted runtime
        reserveRing runtime requestedAlignment byteCount reclaimed
 where
  capacity = runtimeCapacity runtime
  atomSize = runtimeNonCoherentAtomSize runtime
  ringPending = find hasRingRange (pendingUploads state)
  headOffset = if isNothing ringPending then 0 else nextRingOffset state
  tailOffset = pendingStart <$> ringPending
  hasRingRange pending = case pendingRingStart pending of
    Just _ -> True
    Nothing -> False
  pendingStart = fromMaybe 0 . pendingRingStart

data RingReservation = RingReservation
  { ringReservationOffset :: Int
  , ringReservationNextOffset :: Int
  }
  deriving (Eq, Show)

{- | Plans one ring reservation. The start satisfies both the copy and
non-coherent atom alignments, while the occupied span covers whole atoms.
-}
planRingReservation :: Int -> Int -> Int -> Int -> Int -> Maybe Int -> Maybe RingReservation
planRingReservation capacity atomSize copyAlignment payloadBytes headOffset tailOffset = do
  alignment <- checkedLcm atomSize copyAlignment
  occupiedBytes <- checkedAlignUp payloadBytes atomSize
  alignedHead <- checkedAlignUp headOffset alignment
  start <- availableStart alignedHead occupiedBytes
  pure (RingReservation start (start + occupiedBytes))
 where
  availableStart alignedHead occupiedBytes
    | capacity <= 0 || occupiedBytes <= 0 || occupiedBytes > capacity = Nothing
    | Nothing <- tailOffset = Just 0
    | Just tailStart <- tailOffset
    , headOffset >= tailStart
    , alignedHead <= capacity
    , occupiedBytes <= capacity - alignedHead =
        Just alignedHead
    | Just tailStart <- tailOffset
    , headOffset >= tailStart
    , occupiedBytes <= tailStart =
        Just 0
    | Just tailStart <- tailOffset
    , headOffset < tailStart
    , alignedHead <= tailStart
    , occupiedBytes <= tailStart - alignedHead =
        Just alignedHead
    | otherwise = Nothing

reclaimCompleted :: StagingRuntime -> IO StagingState
reclaimCompleted runtime = do
  state <- readMVar (runtimeState runtime)
  ensureHealthy state
  completed <- timelineCompletedValueLeased (runtimeQueue runtime)
  let (finished, remaining) = span ((<= completed) . pendingTimeline) (pendingUploads state)
  mapM_ retirePendingUpload finished
  let reclaimed =
        state
          { nextRingOffset = if anyRingPending remaining then nextRingOffset state else 0
          , pendingUploads = remaining
          }
  modifyMVarMasked_ (runtimeState runtime) (const (pure reclaimed))
  pure reclaimed

withHealthyOperation :: StagingRuntime -> IO a -> IO a
withHealthyOperation runtime action =
  withMVar (runtimeOperationLock runtime) $ \_ -> do
    readMVar (runtimeState runtime) >>= ensureHealthy
    action

ensureHealthy :: StagingState -> IO ()
ensureHealthy state = case stagingHealth state of
  StagingHealthy -> pure ()
  StagingPoisoned failure -> throwIO failure

publishAcceptedSubmission :: StagingRuntime -> StagingState -> Maybe Int -> SubmissionResources -> Word64 -> IO ()
publishAcceptedSubmission runtime state ringStart resources timeline = mask_ $
  modifyMVarMasked_ (runtimeState runtime) $ \_ ->
    pure state{pendingUploads = pendingUploads state <> [PendingUpload ringStart timeline resources]}

publishUnknownSubmission :: StagingRuntime -> StagingState -> SubmissionResources -> SomeException -> IO ()
publishUnknownSubmission runtime state resources failure = mask_ $
  modifyMVarMasked_ (runtimeState runtime) $ \_ ->
    pure
      state
        { quarantinedUploads = quarantinedUploads state <> [QuarantinedUpload resources]
        , stagingHealth = StagingPoisoned failure
        }

retirePendingUpload :: PendingUpload -> IO ()
retirePendingUpload = retireSubmissionResources . pendingResources

retireSubmissionResources :: SubmissionResources -> IO ()
retireSubmissionResources resources = do
  runOneShotCleanup (submissionCleanup resources)
  runOneShotCleanup (submissionCommandBufferCleanup resources)

newOneShotCleanup :: IO () -> IO OneShotCleanup
newOneShotCleanup action = OneShotCleanup <$> newMVar False <*> pure action

runOneShotCleanup :: OneShotCleanup -> IO ()
runOneShotCleanup (OneShotCleanup state action) =
  modifyMVarMasked_ state $ \completed ->
    if completed
      then pure True
      else action >> pure True

retirementActionForTest :: IO () -> IO () -> IO (IO ())
retirementActionForTest cleanup releaseCommandBuffer = do
  cleanupOnce <- newOneShotCleanup cleanup
  commandBufferCleanup <- newOneShotCleanup releaseCommandBuffer
  pure $ do
    runOneShotCleanup cleanupOnce
    runOneShotCleanup commandBufferCleanup

anyRingPending :: [PendingUpload] -> Bool
anyRingPending = any (isJust . pendingRingStart)

checkedOccupiedBytes :: StagingRuntime -> Int -> IO Int
checkedOccupiedBytes runtime byteCount =
  maybe
    (throwIO (VulkanFailure "staging ring" ("cannot align " <> show byteCount <> " bytes to non-coherent atom size " <> show (runtimeNonCoherentAtomSize runtime))))
    pure
    (checkedAlignUp byteCount (runtimeNonCoherentAtomSize runtime))

checkedDeviceSize :: String -> Word64 -> IO Int
checkedDeviceSize description value
  | value == 0 = throwIO (VulkanFailure "staging runtime" (description <> " must be positive, but was 0"))
  | value > fromIntegral (maxBound :: Int) = throwIO (VulkanFailure "staging runtime" (description <> " " <> show value <> " exceeds Int range"))
  | otherwise = pure (fromIntegral value)

checkedAlignUp :: Int -> Int -> Maybe Int
checkedAlignUp value alignment
  | value < 0 || alignment <= 0 = Nothing
  | value > maxBound - (alignment - 1) = Nothing
  | otherwise = Just (((value + alignment - 1) `div` alignment) * alignment)

checkedLcm :: Int -> Int -> Maybe Int
checkedLcm left right
  | left <= 0 || right <= 0 = Nothing
  | factor > maxBound `div` right = Nothing
  | otherwise = Just (factor * right)
 where
  factor = left `div` gcd left right

mapVulkanFailure :: String -> IO a -> IO a
mapVulkanFailure operation action =
  action `catch` \(error' :: Vulkan.VulkanException) ->
    if Vulkan.vulkanExceptionResult error' == Result.ERROR_DEVICE_LOST
      then throwIO DeviceLost
      else throwIO (VulkanFailure operation (show (Vulkan.vulkanExceptionResult error')))

bufferHandleWord :: Vk.Buffer -> Word64
bufferHandleWord (Vk.Buffer handle) = handle

commandPoolHandleWord :: Vk.CommandPool -> Word64
commandPoolHandleWord (Vk.CommandPool handle) = handle

commandBufferHandleWord :: Vk.CommandBuffer -> Word64
commandBufferHandleWord = fromIntegral . ptrToWordPtr . Vk.commandBufferHandle
