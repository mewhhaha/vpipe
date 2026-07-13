{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Typed Vulkan buffers.  Element offsets and counts are always expressed in
host elements, never bytes.
-}
module Vpipe.Buffer.Internal (
  Usage (..),
  HasUsage,
  ValidUsages,
  BufferLayout,
  KnownUsages,
  KnownLayout,
  reflectedUsageFlags,
  reflectedLayout,
  Buffer,
  bufferRawHandle,
  bufferRawState,
  bufferRawUsageFlags,
  bufferRawContext,
  bufferGeneration,
  acquireBufferBindingLease,
  quarantineBufferBinding,
  RestartIndex,
  normalIndex,
  primitiveRestartIndex,
  restartIndexWord32,
  newBuffer,
  destroyBuffer,
  writeBuffer,
  writeIndexBuffer,
  readBuffer,
  bufferLength,
  bufferLayout,
  bufferStride,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked_, newEmptyMVar, newMVar, putMVar, readMVar, tryPutMVar, withMVar)
import Control.Exception (SomeException, bracket, catch, finally, mask, mask_, onException, throwIO, try)
import Control.Monad (unless, void, when)
import Data.Bits ((.|.))
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import Data.Vector qualified as Vector
import Data.Word (Word32, Word64)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.APIConstants qualified as API
import Vulkan.Core10.Buffer qualified as Vk
import Vulkan.Core10.Enums.BufferUsageFlagBits qualified as Usage
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Enums.SharingMode qualified as Sharing
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Core13.Enums.AccessFlags2 qualified as Access2
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 qualified as Sync2
import Vulkan.Exception qualified as Vulkan
import Vulkan.Zero (zero)
import VulkanMemoryAllocator qualified as VMA

import Vpipe.Buffer.Format (BufferFormat (..), HostFormat, LayoutStandard)
import Vpipe.Buffer.Format qualified as Format
import Vpipe.Buffer.Staging qualified as Staging
import Vpipe.Buffer.State qualified as State
import Vpipe.Context.Internal (Context, contextAllocator, contextQueueFamilyIndices, contextStagingRuntime, derivedObjectName, registerContextFinalizerLeased, setObjectNameLeased, transferQueue, withContextLease)
import Vpipe.Context.Queue.Internal (Queue, QueueDependency (..), queueFamilyIndex, waitTimelineLeased)
import Vpipe.Error (VpipeError (..))
import Vpipe.Resource.Lifetime qualified as Lifetime

data Usage = Vertex | Index | Uniform | Storage | CopySrc | CopyDst

{- | A primitive-restart aware index.  'normalIndex' intentionally cannot
construct the reserved all-ones value.
-}
newtype RestartIndex = RestartIndex Word32
  deriving stock (Eq, Ord, Show)

normalIndex :: Word32 -> Maybe RestartIndex
normalIndex value
  | value == maxBound = Nothing
  | otherwise = Just (RestartIndex value)

primitiveRestartIndex :: RestartIndex
primitiveRestartIndex = RestartIndex maxBound

restartIndexWord32 :: RestartIndex -> Word32
restartIndexWord32 (RestartIndex value) = value

type family HasUsage (needed :: Usage) (usages :: [Usage]) :: Constraint where
  HasUsage needed '[] =
    TypeError
      ( 'Text "Buffer operation requires usage "
          ':<>: UsageName needed
          ':<>: 'Text ", but this buffer's usage list does not contain it."
      )
  HasUsage needed (needed ': usages) = ()
  HasUsage needed (_ ': usages) = HasUsage needed usages

type ValidUsages usages a = (ValidUsageList usages a, ValidUsageCombination usages)

type family ValidUsageList (usages :: [Usage]) a :: Constraint where
  ValidUsageList '[] _ = TypeError ('Text "Buffer usage lists must not be empty.")
  ValidUsageList (usage ': usages) a = (UsageNotRepeated usage usages, IndexElementIsWord32 usage a, ValidUsageTail usages a)

type family ValidUsageTail (usages :: [Usage]) a :: Constraint where
  ValidUsageTail '[] _ = ()
  ValidUsageTail usages a = ValidUsageList usages a

type family UsageNotRepeated (usage :: Usage) (usages :: [Usage]) :: Constraint where
  UsageNotRepeated usage '[] = ()
  UsageNotRepeated usage (usage ': _) =
    TypeError ('Text "Buffer usage list contains duplicate usage " ':<>: UsageName usage ':<>: 'Text ".")
  UsageNotRepeated usage (_ ': usages) = UsageNotRepeated usage usages

type family UsageName (usage :: Usage) :: ErrorMessage where
  UsageName 'Vertex = 'Text "Vertex"
  UsageName 'Index = 'Text "Index"
  UsageName 'Uniform = 'Text "Uniform"
  UsageName 'Storage = 'Text "Storage"
  UsageName 'CopySrc = 'Text "CopySrc"
  UsageName 'CopyDst = 'Text "CopyDst"

type family IndexElementIsWord32 (usage :: Usage) a :: Constraint where
  IndexElementIsWord32 'Index Word32 = ()
  IndexElementIsWord32 'Index a =
    TypeError ('Text "Index buffers require Word32 elements, but received " ':<>: 'ShowType a ':<>: 'Text ".")
  IndexElementIsWord32 _ _ = ()

type family ContainsUsage (needed :: Usage) (usages :: [Usage]) :: Bool where
  ContainsUsage _ '[] = 'False
  ContainsUsage needed (needed ': _) = 'True
  ContainsUsage needed (_ ': usages) = ContainsUsage needed usages

type family ValidUsageCombination (usages :: [Usage]) :: Constraint where
  ValidUsageCombination usages = ValidateVertexCombination (ContainsUsage 'Vertex usages) usages

type family ValidateVertexCombination (hasVertex :: Bool) (usages :: [Usage]) :: Constraint where
  ValidateVertexCombination 'True usages = ValidateVertexStorage (ContainsUsage 'Uniform usages) (ContainsUsage 'Storage usages) usages
  ValidateVertexCombination 'False usages = ValidateUniformStorage (ContainsUsage 'Uniform usages) (ContainsUsage 'Storage usages)

type family ValidateVertexStorage (hasUniform :: Bool) (hasStorage :: Bool) (usages :: [Usage]) :: Constraint where
  ValidateVertexStorage 'True _ _ = TypeError ('Text "Vertex buffers cannot be combined with Uniform usage.")
  ValidateVertexStorage _ _ _ = ()

type family ValidateUniformStorage (hasUniform :: Bool) (hasStorage :: Bool) :: Constraint where
  ValidateUniformStorage 'True 'True = TypeError ('Text "Uniform and Storage usages cannot be combined because a buffer has one physical layout.")
  ValidateUniformStorage _ _ = ()

type family BufferLayout (usages :: [Usage]) :: LayoutStandard where
  BufferLayout usages = ChooseVertexLayout (ContainsUsage 'Vertex usages) usages

type family ChooseVertexLayout (hasVertex :: Bool) (usages :: [Usage]) :: LayoutStandard where
  ChooseVertexLayout 'True _ = 'Format.Vertex
  ChooseVertexLayout 'False usages = ChooseUniformLayout (ContainsUsage 'Uniform usages)

type family ChooseUniformLayout (hasUniform :: Bool) :: LayoutStandard where
  ChooseUniformLayout 'True = 'Format.Std140
  ChooseUniformLayout 'False = 'Format.Std430

class KnownLayout (layout :: LayoutStandard) where
  reflectedLayout :: Proxy layout -> LayoutStandard

instance KnownLayout 'Format.Vertex where reflectedLayout _ = Format.Vertex
instance KnownLayout 'Format.Std140 where reflectedLayout _ = Format.Std140
instance KnownLayout 'Format.Std430 where reflectedLayout _ = Format.Std430

class KnownUsages (usages :: [Usage]) where
  reflectedUsageFlags :: Proxy usages -> Usage.BufferUsageFlags

instance KnownUsages '[] where
  reflectedUsageFlags _ = zero

instance (KnownUsages usages) => KnownUsages ('Vertex ': usages) where
  reflectedUsageFlags _ = Usage.BUFFER_USAGE_VERTEX_BUFFER_BIT .|. reflectedUsageFlags (Proxy @usages)

instance (KnownUsages usages) => KnownUsages ('Index ': usages) where
  reflectedUsageFlags _ = Usage.BUFFER_USAGE_INDEX_BUFFER_BIT .|. reflectedUsageFlags (Proxy @usages)

instance (KnownUsages usages) => KnownUsages ('Uniform ': usages) where
  reflectedUsageFlags _ = Usage.BUFFER_USAGE_UNIFORM_BUFFER_BIT .|. reflectedUsageFlags (Proxy @usages)

instance (KnownUsages usages) => KnownUsages ('Storage ': usages) where
  reflectedUsageFlags _ = Usage.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. reflectedUsageFlags (Proxy @usages)

instance (KnownUsages usages) => KnownUsages ('CopySrc ': usages) where
  reflectedUsageFlags _ = Usage.BUFFER_USAGE_TRANSFER_SRC_BIT .|. reflectedUsageFlags (Proxy @usages)

instance (KnownUsages usages) => KnownUsages ('CopyDst ': usages) where
  reflectedUsageFlags _ = Usage.BUFFER_USAGE_TRANSFER_DST_BIT .|. reflectedUsageFlags (Proxy @usages)

data Buffer (usages :: [Usage]) a = Buffer
  { bufferAllocator :: VMA.Allocator
  , bufferRawContext :: Context
  , bufferRawHandle :: Vk.Buffer
  , bufferElements :: Int
  , bufferElementBytes :: Int
  , bufferLayoutStandard :: LayoutStandard
  , bufferLock :: MVar ()
  , bufferRawState :: State.BufferState
  , bufferRawUsageFlags :: Usage.BufferUsageFlags
  , bufferGeneration :: Lifetime.ResourceGeneration
  , bufferLifetimeGate :: Lifetime.LifetimeGate
  , bufferReleaseAction :: IO ()
  }

type role Buffer nominal nominal

bufferLength :: Buffer usages a -> Int
bufferLength = bufferElements

bufferLayout :: Buffer usages a -> LayoutStandard
bufferLayout = bufferLayoutStandard

bufferStride :: Buffer usages a -> Int
bufferStride = bufferElementBytes

newBuffer :: forall usages a. (BufferFormat a, KnownUsages usages, ValidUsages usages a, KnownLayout (BufferLayout usages)) => Context -> Int -> IO (Buffer usages a)
newBuffer context elements = withContextLease context $ mask $ \_ -> do
  when (elements <= 0) (throwIO (BufferElementCountInvalid elements))
  let layout = reflectedLayout (Proxy @(BufferLayout usages))
      elementBytes = bufferSizeFor layout (Proxy @a)
  totalBytes <- checkedByteCount elements elementBytes
  lock <- newMVar ()
  state <- State.newBufferState
  lifetimeGate <- Lifetime.newLifetimeGate
  generation <- Lifetime.newResourceGeneration
  -- The public usage list controls typed consumers. Transfer bits are added
  -- internally so uploads and diagnostic readback do not require a second
  -- resource or queue-family ownership transfer.
  let queueFamilies = contextQueueFamilyIndices context
      (sharingMode, sharedFamilies) =
        if length queueFamilies > 1
          then (Sharing.SHARING_MODE_CONCURRENT, Vector.fromList queueFamilies)
          else (Sharing.SHARING_MODE_EXCLUSIVE, Vector.empty)
      actualUsageFlags = usageFlags (Proxy @usages) .|. Usage.BUFFER_USAGE_TRANSFER_SRC_BIT .|. Usage.BUFFER_USAGE_TRANSFER_DST_BIT
      createInfo =
        (zero :: Vk.BufferCreateInfo '[])
          { Vk.size = fromIntegral totalBytes
          , Vk.usage = actualUsageFlags
          , Vk.sharingMode = sharingMode
          , Vk.queueFamilyIndices = sharedFamilies
          }
      allocationInfo = deviceLocalAllocationCreateInfo
      allocator = contextAllocator context
  (handle, allocation, _) <- mapVma "vmaCreateBuffer" (VMA.createBuffer allocator createInfo allocationInfo)
  setObjectNameLeased context ObjectType.OBJECT_TYPE_BUFFER (bufferHandleWord handle) (derivedObjectName "buffer" (bufferHandleWord handle))
    `onException` VMA.destroyBuffer allocator handle allocation
  releaseState <- newMVar False
  let release = do
        Lifetime.sealLifetimeGate lifetimeGate
        releaseOnce releaseState (VMA.destroyBuffer allocator handle allocation)
  registerContextFinalizerLeased context release `onException` release
  pure (Buffer allocator context handle elements elementBytes layout lock state actualUsageFlags generation lifetimeGate release)

-- | Releases a buffer before its context closes. Repeated calls are harmless.
destroyBuffer :: Buffer usages a -> IO ()
destroyBuffer buffer = withContextLease (bufferRawContext buffer) $ mask_ $ do
  Lifetime.closeLifetimeGate (bufferLifetimeGate buffer)
  withMVar (bufferLock buffer) $ \_ -> do
    previous <- State.lastBufferUse (bufferRawState buffer)
    awaitPreviousUse (transferQueue (bufferRawContext buffer)) previous
    Staging.reclaimStaging (contextStagingRuntime (bufferRawContext buffer))
    bufferReleaseAction buffer

writeBuffer :: forall usages a. (BufferFormat a) => Buffer usages a -> Int -> [HostFormat a] -> IO ()
writeBuffer buffer offset values = withBufferLease buffer $ \_ ->
  mask $ \restore -> do
    checkRange buffer offset (length values)
    unless (null values) $ do
      reservation <- State.beginBufferUse (bufferRawState buffer)
      let byteOffset = offset * bufferElementBytes buffer
          byteCount = length values * bufferElementBytes buffer
          queue = transferQueue (bufferRawContext buffer)
          cancel = void (State.cancelBufferUse reservation)
      ( do
          let dependencies = bufferDependencies Stage2.PIPELINE_STAGE_2_TRANSFER_BIT (State.reservationPreviousUse reservation)
          submission <-
            Staging.submitUploadAfter
              (contextStagingRuntime (bufferRawContext buffer))
              dependencies
              (bufferAlignmentFor (bufferLayout buffer) (Proxy @a))
              byteCount
              (\pointer -> restore (writeValues pointer values))
              (recordTransferBarrier (State.reservationPreviousUse reservation) queue Access2.ACCESS_2_TRANSFER_WRITE_BIT (bufferRawHandle buffer) 0 (bufferElements buffer * bufferElementBytes buffer))
              (bufferRawHandle buffer)
              byteOffset
          case submission of
            Staging.StagingSubmissionAccepted signal -> do
              committed <-
                State.commitBufferUse
                  reservation
                  (transferUse queue Access2.ACCESS_2_TRANSFER_WRITE_BIT signal)
              unless committed (throwIO (VulkanFailure "buffer upload" "stale buffer-state reservation"))
            Staging.StagingSubmissionAcceptanceUnknown primaryFailure -> do
              quarantineBufferBinding buffer
              throwIO primaryFailure
            Staging.StagingSubmissionAcceptedPublicationFailed signal primaryFailure -> do
              publication <-
                try $ do
                  committed <-
                    State.commitBufferUse
                      reservation
                      (transferUse queue Access2.ACCESS_2_TRANSFER_WRITE_BIT signal)
                  unless committed (throwIO (VulkanFailure "buffer upload" "stale buffer-state reservation"))
              case publication of
                Right () -> throwIO primaryFailure
                Left (_publicationFailure :: SomeException) -> do
                  quarantineBufferBinding buffer
                  throwIO primaryFailure
        )
        `onException` cancel
 where
  writeValues :: Ptr () -> [HostFormat a] -> IO ()
  writeValues _ [] = pure ()
  writeValues pointer (value : remaining) = do
    pokeBufferFor (bufferLayout buffer) (Proxy @a) pointer value
    writeValues (pointer `plusPtr` bufferElementBytes buffer) remaining

writeIndexBuffer :: (HasUsage Index usages) => Buffer usages Word32 -> Int -> [RestartIndex] -> IO ()
writeIndexBuffer buffer offset = writeBuffer buffer offset . fmap restartIndexWord32

readBuffer :: forall usages a. (BufferFormat a, HasUsage CopySrc usages) => Buffer usages a -> Int -> Int -> IO [HostFormat a]
readBuffer buffer offset count = withBufferLease buffer $ \_ ->
  mask $ \restore -> do
    checkRange buffer offset count
    if count == 0
      then pure []
      else do
        reservation <- State.beginBufferUse (bufferRawState buffer)
        let byteOffset = offset * bufferElementBytes buffer
            byteCount = count * bufferElementBytes buffer
            queue = transferQueue (bufferRawContext buffer)
            runtime = contextStagingRuntime (bufferRawContext buffer)
            cancel = void (State.cancelBufferUse reservation)
        ( do
            retirementGate <- newEmptyMVar
            (stagingHandle, stagingAllocation, cleanup) <- acquireReadbackBuffer buffer byteCount
            let retire = readMVar retirementGate >> cleanup
            submission <-
              ( do
                  Staging.submitCopyAfter
                    runtime
                    (bufferDependencies Stage2.PIPELINE_STAGE_2_TRANSFER_BIT (State.reservationPreviousUse reservation))
                    ( \commandBuffer -> do
                        recordTransferBarrier
                          (State.reservationPreviousUse reservation)
                          queue
                          Access2.ACCESS_2_TRANSFER_READ_BIT
                          (bufferRawHandle buffer)
                          0
                          (bufferElements buffer * bufferElementBytes buffer)
                          commandBuffer
                        -- VMA may reuse a retired readback allocation. Queue
                        -- order alone does not make writes to aliased memory
                        -- available to the next transfer destination.
                        let aliasBarrier =
                              Sync2.MemoryBarrier2
                                { Sync2.srcStageMask = Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT
                                , Sync2.srcAccessMask = Access2.ACCESS_2_MEMORY_WRITE_BIT
                                , Sync2.dstStageMask = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
                                , Sync2.dstAccessMask = Access2.ACCESS_2_TRANSFER_WRITE_BIT
                                }
                            dependency =
                              (zero :: Sync2.DependencyInfo)
                                { Sync2.memoryBarriers = Vector.singleton aliasBarrier
                                }
                        Sync2.cmdPipelineBarrier2 commandBuffer dependency
                    )
                    (recordHostReadBarrier stagingHandle 0 byteCount)
                    (bufferRawHandle buffer)
                    byteOffset
                    stagingHandle
                    0
                    byteCount
                    retire
              )
                `onException` cleanup
            case submission of
              Staging.StagingSubmissionAcceptanceUnknown primaryFailure -> do
                void (tryPutMVar retirementGate ())
                quarantineBufferBinding buffer
                throwIO primaryFailure
              Staging.StagingSubmissionAccepted signal -> do
                values <-
                  ( do
                      committed <-
                        State.commitBufferUse
                          reservation
                          (transferUse queue Access2.ACCESS_2_TRANSFER_READ_BIT signal)
                      unless committed (throwIO (VulkanFailure "buffer readback" "stale buffer-state reservation"))
                      -- The timeline-owned cleanup waits for this gate, because
                      -- the CPU still needs the allocation after GPU completion.
                      waitTimelineLeased queue signal
                      bracket
                        (mapVma "vmaMapMemory" (VMA.mapMemory (bufferAllocator buffer) stagingAllocation))
                        (const (VMA.unmapMemory (bufferAllocator buffer) stagingAllocation))
                        ( \mapped -> do
                            mapVma "vmaInvalidateAllocation" (VMA.invalidateAllocation (bufferAllocator buffer) stagingAllocation 0 (fromIntegral byteCount))
                            restore (readValues mapped count)
                        )
                  )
                    `finally` putMVar retirementGate ()
                Staging.reclaimStaging runtime
                pure values
              Staging.StagingSubmissionAcceptedPublicationFailed signal primaryFailure -> do
                void (tryPutMVar retirementGate ())
                publication <-
                  try $ do
                    committed <-
                      State.commitBufferUse
                        reservation
                        (transferUse queue Access2.ACCESS_2_TRANSFER_READ_BIT signal)
                    unless committed (throwIO (VulkanFailure "buffer readback" "stale buffer-state reservation"))
                case publication of
                  Right () -> throwIO primaryFailure
                  Left (_publicationFailure :: SomeException) -> do
                    quarantineBufferBinding buffer
                    throwIO primaryFailure
          )
          `onException` cancel
 where
  readValues :: Ptr () -> Int -> IO [HostFormat a]
  readValues _ 0 = pure []
  readValues pointer remaining = do
    value <- peekBufferFor (bufferLayout buffer) (Proxy @a) pointer
    (value :) <$> readValues (pointer `plusPtr` bufferElementBytes buffer) (remaining - 1)

checkRange :: Buffer usages a -> Int -> Int -> IO ()
checkRange buffer offset count
  | offset < 0 || count < 0 = invalid
  | offset > bufferElements buffer = invalid
  | count > bufferElements buffer - offset = invalid
  | otherwise = pure ()
 where
  invalid = throwIO (BufferElementRangeInvalid offset count (bufferElements buffer))

checkedByteCount :: Int -> Int -> IO Int
checkedByteCount elements elementBytes
  | elements <= 0 = throwIO (BufferElementCountInvalid elements)
  | elementBytes <= 0 = throwIO (BufferSizeOverflow elements elementBytes)
  | elements > maxBound `div` elementBytes = throwIO (BufferSizeOverflow elements elementBytes)
  | otherwise = pure (elements * elementBytes)

usageFlags :: (KnownUsages usages) => Proxy usages -> Usage.BufferUsageFlags
usageFlags = reflectedUsageFlags

mapVma :: String -> IO b -> IO b
mapVma operation action =
  action `catch` \(error' :: Vulkan.VulkanException) ->
    if Vulkan.vulkanExceptionResult error' == Result.ERROR_DEVICE_LOST
      then throwIO DeviceLost
      else throwIO (VulkanFailure operation (show (Vulkan.vulkanExceptionResult error')))

acquireReadbackBuffer :: Buffer usages a -> Int -> IO (Vk.Buffer, VMA.Allocation, IO ())
acquireReadbackBuffer buffer bytes = mask $ \_ -> do
  let allocator = bufferAllocator buffer
      createInfo = (zero :: Vk.BufferCreateInfo '[]){Vk.size = fromIntegral bytes, Vk.usage = Usage.BUFFER_USAGE_TRANSFER_DST_BIT}
      allocationInfo = (zero :: VMA.AllocationCreateInfo){VMA.usage = VMA.MEMORY_USAGE_AUTO_PREFER_HOST, VMA.flags = VMA.ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT}
  (handle, allocation, _) <- mapVma "vmaCreateBuffer(readback)" (VMA.createBuffer allocator createInfo allocationInfo)
  let context = bufferRawContext buffer
  setObjectNameLeased context ObjectType.OBJECT_TYPE_BUFFER (bufferHandleWord handle) (derivedObjectName "buffer-readback" (bufferHandleWord handle))
    `onException` VMA.destroyBuffer allocator handle allocation
  pure (handle, allocation, VMA.destroyBuffer allocator handle allocation)

bufferHandleWord :: Handles.Buffer -> Word64
bufferHandleWord (Handles.Buffer handle) = handle

withBufferLease :: Buffer usages a -> (() -> IO b) -> IO b
withBufferLease buffer action =
  withContextLease (bufferRawContext buffer) $
    Lifetime.withLifetimeLease (bufferLifetimeGate buffer) (throwIO BufferReleased) $
      withMVar (bufferLock buffer) action

acquireBufferBindingLease :: Buffer usages a -> IO (IO ())
acquireBufferBindingLease buffer = do
  lease <- Lifetime.acquireLifetimeLease (bufferLifetimeGate buffer)
  maybe (throwIO BufferReleased) pure lease

quarantineBufferBinding :: Buffer usages a -> IO ()
quarantineBufferBinding buffer = mask_ $ do
  State.quarantineBufferState (bufferRawState buffer)
  Lifetime.quarantineLifetimeGate (bufferLifetimeGate buffer)

releaseOnce :: MVar Bool -> IO () -> IO ()
releaseOnce state release =
  modifyMVarMasked_ state $ \released ->
    if released
      then pure True
      else release >> pure True

deviceLocalAllocationCreateInfo :: VMA.AllocationCreateInfo
deviceLocalAllocationCreateInfo =
  VMA.AllocationCreateInfo
    zero
    VMA.MEMORY_USAGE_AUTO_PREFER_DEVICE
    zero
    zero
    0
    zero
    nullPtr
    0

awaitPreviousUse :: Queue -> Maybe State.BufferUse -> IO ()
awaitPreviousUse _ Nothing = pure ()
awaitPreviousUse _ (Just previous) = case State.bufferUseCompletion previous of
  Nothing -> pure ()
  Just completion ->
    waitTimelineLeased
      (State.bufferCompletionQueue completion)
      (State.bufferCompletionTimeline completion)

bufferDependencies :: Stage2.PipelineStageFlags2 -> Maybe State.BufferUse -> [QueueDependency]
bufferDependencies _ Nothing = []
bufferDependencies destinationStage (Just previous) = case State.bufferUseCompletion previous of
  Nothing -> []
  Just completion ->
    [ QueueDependency
        (State.bufferCompletionQueue completion)
        (State.bufferCompletionTimeline completion)
        destinationStage
    ]

transferUse :: Queue -> Access2.AccessFlags2 -> Word64 -> State.BufferUse
transferUse queue access signal =
  State.BufferUse
    { State.bufferUseStage = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
    , State.bufferUseAccess = access
    , State.bufferUseCompletion =
        Just
          State.BufferCompletion
            { State.bufferCompletionQueue = queue
            , State.bufferCompletionQueueFamily = queueFamilyIndex queue
            , State.bufferCompletionTimeline = signal
            }
    }

recordTransferBarrier ::
  Maybe State.BufferUse ->
  Queue ->
  Access2.AccessFlags2 ->
  Handles.Buffer ->
  Int ->
  Int ->
  Handles.CommandBuffer ->
  IO ()
recordTransferBarrier Nothing _ _ _ _ _ _ = pure ()
recordTransferBarrier (Just previous) queue destinationAccess handle offset bytes commandBuffer = do
  let sameFamily = case State.bufferUseCompletion previous of
        Nothing -> True
        Just completion -> State.bufferCompletionQueueFamily completion == queueFamilyIndex queue
      barrier =
        (zero :: Sync2.BufferMemoryBarrier2 '[])
          { Sync2.srcStageMask = if sameFamily then State.bufferUseStage previous else zero
          , Sync2.srcAccessMask = if sameFamily then State.bufferUseAccess previous else zero
          , Sync2.dstStageMask = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
          , Sync2.dstAccessMask = destinationAccess
          , Sync2.srcQueueFamilyIndex = API.QUEUE_FAMILY_IGNORED
          , Sync2.dstQueueFamilyIndex = API.QUEUE_FAMILY_IGNORED
          , Sync2.buffer = handle
          , Sync2.offset = fromIntegral offset
          , Sync2.size = fromIntegral bytes
          }
      dependency =
        (zero :: Sync2.DependencyInfo)
          { Sync2.bufferMemoryBarriers = Vector.singleton (Chain.SomeStruct barrier)
          }
  Sync2.cmdPipelineBarrier2 commandBuffer dependency

recordHostReadBarrier :: Handles.Buffer -> Int -> Int -> Handles.CommandBuffer -> IO ()
recordHostReadBarrier handle offset bytes commandBuffer = do
  let barrier =
        (zero :: Sync2.BufferMemoryBarrier2 '[])
          { Sync2.srcStageMask = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
          , Sync2.srcAccessMask = Access2.ACCESS_2_TRANSFER_WRITE_BIT
          , Sync2.dstStageMask = Stage2.PIPELINE_STAGE_2_HOST_BIT
          , Sync2.dstAccessMask = Access2.ACCESS_2_HOST_READ_BIT
          , Sync2.srcQueueFamilyIndex = API.QUEUE_FAMILY_IGNORED
          , Sync2.dstQueueFamilyIndex = API.QUEUE_FAMILY_IGNORED
          , Sync2.buffer = handle
          , Sync2.offset = fromIntegral offset
          , Sync2.size = fromIntegral bytes
          }
      dependency =
        (zero :: Sync2.DependencyInfo)
          { Sync2.bufferMemoryBarriers = Vector.singleton (Chain.SomeStruct barrier)
          }
  Sync2.cmdPipelineBarrier2 commandBuffer dependency
