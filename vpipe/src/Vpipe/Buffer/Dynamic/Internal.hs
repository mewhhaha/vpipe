{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- | Persistently mapped buffer slices.  Copy selection is explicit: this
module deliberately has no frame-rotation policy.
-}
module Vpipe.Buffer.Dynamic.Internal (
  DynamicBuffer,
  FrameDynamicBuffer,
  DynamicBufferKey,
  DynamicSlice (..),
  dynamicBufferHandle,
  newDynamicBuffer,
  newFrameDynamicBuffer,
  destroyDynamicBuffer,
  destroyFrameDynamicBuffer,
  dynamicCopyCount,
  dynamicElementsPerCopy,
  dynamicStride,
  dynamicSliceBytes,
  dynamicSliceOffset,
  frameDynamicElements,
  frameDynamicStride,
  frameDynamicSliceBytes,
  frameDynamicBufferKey,
  withFrameDynamicSlice,
  writeDynamicBuffer,
  readDynamicBuffer,
  flushDynamicBuffer,
  invalidateDynamicBuffer,
  checkDynamicDescriptorOffset,
  commitDynamicHostWrite,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked_, newMVar, withMVar)
import Control.Exception (catch, mask, mask_, onException, throwIO)
import Control.Monad (unless, void, when)
import Data.Bits ((.&.), (.|.))
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Data.Unique (Unique)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Vulkan.Core10.Buffer qualified as Vk
import Vulkan.Core10.Enums.BufferUsageFlagBits qualified as Usage
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Enums.SharingMode qualified as Sharing
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Core13.Enums.AccessFlags2 qualified as Access2
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Exception qualified as Vulkan
import Vulkan.Zero (zero)
import VulkanMemoryAllocator qualified as VMA

import Vpipe.Buffer (BufferLayout, KnownLayout, KnownUsages, Usage, ValidUsages, reflectedLayout, reflectedUsageFlags)
import Vpipe.Buffer.Format (BufferFormat (..), HostFormat)
import Vpipe.Buffer.Format qualified as Format
import Vpipe.Buffer.State qualified as State
import Vpipe.Context.Internal (Context, contextAllocator, contextIdentity, contextNonCoherentAtomSize, contextQueueFamilyIndices, contextStorageBufferOffsetAlignment, contextUniformBufferOffsetAlignment, derivedObjectName, registerContextFinalizerLeased, setObjectNameLeased, withContextLease)
import Vpipe.Context.Queue.Internal (waitTimelineLeased)
import Vpipe.Error (VpipeError (..))
import Vpipe.Resource.Lifetime qualified as Lifetime
import Vpipe.Swapchain.Internal (FrameDomain, Swapchain, lockedSwapchainFrameConfiguration, withSwapchainOperation)

data DynamicBuffer (usages :: [Usage]) a = DynamicBuffer
  { dynamicAllocator :: VMA.Allocator
  , dynamicContext :: Context
  , dynamicBufferHandle :: Vk.Buffer
  , dynamicAllocation :: VMA.Allocation
  , dynamicMappedPointer :: Ptr ()
  , dynamicCopies :: Int
  , dynamicElements :: Int
  , dynamicElementBytes :: Int
  , dynamicSliceSize :: Int
  , dynamicDescriptorOffsetAlignment :: Int
  , dynamicLayout :: Format.LayoutStandard
  , dynamicUsageFlags :: Usage.BufferUsageFlags
  , dynamicLock :: MVar ()
  , dynamicStates :: Vector State.BufferState
  , dynamicGeneration :: Lifetime.ResourceGeneration
  , dynamicLifetimeGate :: Lifetime.LifetimeGate
  , dynamicReleaseAction :: IO ()
  }

type role DynamicBuffer nominal nominal

data FrameDynamicBuffer (usages :: [Usage]) a = FrameDynamicBuffer
  { frameDynamicDomain :: FrameDomain
  , frameDynamicBuffer :: DynamicBuffer usages a
  }

type role FrameDynamicBuffer nominal nominal

data DynamicSlice = DynamicSlice
  { dynamicSliceOwner :: Unique
  , dynamicSliceGeneration :: Lifetime.ResourceGeneration
  , dynamicSliceHandle :: Vk.Buffer
  , dynamicSliceState :: State.BufferState
  , dynamicSliceElements :: Int
  , dynamicSliceElementBytes :: Int
  , dynamicSliceByteOffset :: Word64
  , dynamicSliceUsageFlags :: Usage.BufferUsageFlags
  , acquireDynamicSliceLease :: IO (IO ())
  , quarantineDynamicSlice :: IO ()
  }

data DynamicBufferKey = DynamicBufferKey Unique Lifetime.ResourceGeneration
  deriving stock (Eq, Ord)

dynamicCopyCount :: DynamicBuffer usages a -> Int
dynamicCopyCount = dynamicCopies

dynamicElementsPerCopy :: DynamicBuffer usages a -> Int
dynamicElementsPerCopy = dynamicElements

dynamicStride :: DynamicBuffer usages a -> Int
dynamicStride = dynamicElementBytes

dynamicSliceBytes :: DynamicBuffer usages a -> Int
dynamicSliceBytes = dynamicSliceSize

dynamicSliceOffset :: DynamicBuffer usages a -> Int -> Maybe Int
dynamicSliceOffset buffer copyIndex
  | copyIndex < 0 || copyIndex >= dynamicCopies buffer = Nothing
  | otherwise = Just (copyIndex * dynamicSliceSize buffer)

frameDynamicElements :: FrameDynamicBuffer usages a -> Int
frameDynamicElements = dynamicElementsPerCopy . frameDynamicBuffer

frameDynamicStride :: FrameDynamicBuffer usages a -> Int
frameDynamicStride = dynamicStride . frameDynamicBuffer

frameDynamicSliceBytes :: FrameDynamicBuffer usages a -> Int
frameDynamicSliceBytes = dynamicSliceBytes . frameDynamicBuffer

frameDynamicBufferKey :: FrameDynamicBuffer usages a -> DynamicBufferKey
frameDynamicBufferKey framed =
  let buffer = frameDynamicBuffer framed
   in DynamicBufferKey (contextIdentity (dynamicContext buffer)) (dynamicGeneration buffer)

newDynamicBuffer :: forall usages a. (BufferFormat a, KnownUsages usages, ValidUsages usages a, KnownLayout (BufferLayout usages)) => Context -> Int -> Int -> IO (DynamicBuffer usages a)
newDynamicBuffer context copies elements = withContextLease context $ mask $ \_ -> do
  when (copies <= 0) (throwIO (BufferElementCountInvalid copies))
  when (elements <= 0) (throwIO (BufferElementCountInvalid elements))
  let layout = reflectedLayout (Proxy @(BufferLayout usages))
      stride = bufferSizeFor layout (Proxy @a)
      usageFlags = reflectedUsageFlags (Proxy @usages)
  payloadBytes <- checkedProduct elements stride
  descriptorAlignment <- checkedDescriptorAlignment context usageFlags
  atomAlignment <- checkedDeviceAlignment (contextNonCoherentAtomSize context)
  descriptorAndBufferAlignment <- checkedLcm descriptorAlignment (bufferAlignmentFor layout (Proxy @a))
  sliceAlignment <- checkedLcm descriptorAndBufferAlignment atomAlignment
  sliceBytes <- checkedAlignUp payloadBytes sliceAlignment
  totalBytes <- checkedProduct copies sliceBytes
  lock <- newMVar ()
  states <- Vector.replicateM copies State.newBufferState
  lifetimeGate <- Lifetime.newLifetimeGate
  generation <- Lifetime.newResourceGeneration
  releaseState <- newMVar False
  let actualUsageFlags = usageFlags .|. Usage.BUFFER_USAGE_TRANSFER_SRC_BIT .|. Usage.BUFFER_USAGE_TRANSFER_DST_BIT
      queueFamilies = contextQueueFamilyIndices context
      (sharingMode, sharedFamilies) =
        if length queueFamilies > 1
          then (Sharing.SHARING_MODE_CONCURRENT, Vector.fromList queueFamilies)
          else (Sharing.SHARING_MODE_EXCLUSIVE, Vector.empty)
      createInfo =
        (zero :: Vk.BufferCreateInfo '[])
          { Vk.size = fromIntegral totalBytes
          , Vk.usage = actualUsageFlags
          , Vk.sharingMode = sharingMode
          , Vk.queueFamilyIndices = sharedFamilies
          }
      allocator = contextAllocator context
      allocationInfo =
        (zero :: VMA.AllocationCreateInfo)
          { VMA.usage = VMA.MEMORY_USAGE_AUTO_PREFER_HOST
          , VMA.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT .|. VMA.ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
          }
  (handle, allocation, information) <- mapVma "vmaCreateBuffer(dynamic)" (VMA.createBuffer allocator createInfo allocationInfo)
  let cleanup = do
        Lifetime.sealLifetimeGate lifetimeGate
        releaseDynamicOnce releaseState (VMA.destroyBuffer allocator handle allocation)
      pointer = VMA.mappedData information
  when (pointer == nullPtr) $ cleanup >> throwIO (VulkanFailure "vmaCreateBuffer(dynamic)" "mapped allocation returned a null pointer")
  setObjectNameLeased context ObjectType.OBJECT_TYPE_BUFFER (bufferHandleWord handle) (derivedObjectName "buffer-dynamic" (bufferHandleWord handle))
    `onException` cleanup
  registerContextFinalizerLeased context cleanup `onException` cleanup
  pure (DynamicBuffer allocator context handle allocation pointer copies elements stride sliceBytes descriptorAlignment layout actualUsageFlags lock states generation lifetimeGate cleanup)

newFrameDynamicBuffer :: forall usages a. (BufferFormat a, KnownUsages usages, ValidUsages usages a, KnownLayout (BufferLayout usages)) => Swapchain -> Int -> IO (FrameDynamicBuffer usages a)
newFrameDynamicBuffer swapchain elements =
  withSwapchainOperation swapchain $ \locked -> do
    (context, domain, copies) <- lockedSwapchainFrameConfiguration locked
    FrameDynamicBuffer domain <$> newDynamicBuffer context copies elements

-- | Releases a dynamic buffer before its context closes. Repeated calls are harmless.
destroyDynamicBuffer :: DynamicBuffer usages a -> IO ()
destroyDynamicBuffer buffer = withContextLease (dynamicContext buffer) $ mask_ $ do
  Lifetime.closeLifetimeGate (dynamicLifetimeGate buffer)
  withMVar (dynamicLock buffer) $ \_ -> do
    traverse_ awaitStateCompletion (dynamicStates buffer)
    dynamicReleaseAction buffer

destroyFrameDynamicBuffer :: FrameDynamicBuffer usages a -> IO ()
destroyFrameDynamicBuffer = destroyDynamicBuffer . frameDynamicBuffer

writeDynamicBuffer :: forall usages a. (BufferFormat a) => DynamicBuffer usages a -> Int -> Int -> [HostFormat a] -> IO ()
writeDynamicBuffer buffer copyIndex offset values =
  withDynamicBufferLease buffer $
    withMVar (dynamicLock buffer) $ \_ ->
      writeDynamicSliceLocked buffer copyIndex offset values

{- | Nonzero element offsets must produce a byte offset aligned for every
uniform or storage descriptor usage declared by the buffer.
-}
withFrameDynamicSlice :: forall usages a b. (BufferFormat a) => FrameDynamicBuffer usages a -> FrameDomain -> Int -> Int -> [HostFormat a] -> (DynamicSlice -> IO b) -> IO b
withFrameDynamicSlice framed currentDomain copyIndex offset values continuation = do
  unless (frameDynamicDomain framed == currentDomain) (throwIO FrameDynamicBufferDomainMismatch)
  when (null values) (throwIO (BufferElementCountInvalid 0))
  let buffer = frameDynamicBuffer framed
  withDynamicBufferLease buffer $ do
    slice <- withMVar (dynamicLock buffer) $ \_ -> do
      selectedSlice <- dynamicSliceFor buffer copyIndex offset (length values)
      writeDynamicSliceLocked buffer copyIndex offset values
      pure selectedSlice
    continuation slice

writeDynamicSliceLocked :: forall usages a. (BufferFormat a) => DynamicBuffer usages a -> Int -> Int -> [HostFormat a] -> IO ()
writeDynamicSliceLocked buffer copyIndex offset values = do
  byteOffset <- checkedRange buffer copyIndex offset (length values)
  unless (null values) $ do
    state <- dynamicStateFor buffer copyIndex
    commitDynamicHostWrite state $ do
      writeValues (dynamicMappedPointer buffer `plusPtr` byteOffset) values
      mapVma
        "vmaFlushAllocation(dynamic)"
        ( VMA.flushAllocation
            (dynamicAllocator buffer)
            (dynamicAllocation buffer)
            (fromIntegral byteOffset)
            (fromIntegral (length values * dynamicElementBytes buffer))
        )
 where
  writeValues _ [] = pure ()
  writeValues pointer (value : remaining) = do
    pokeBufferFor (dynamicLayout buffer) (Proxy @a) pointer value
    writeValues (pointer `plusPtr` dynamicElementBytes buffer) remaining

commitDynamicHostWrite :: State.BufferState -> IO () -> IO ()
commitDynamicHostWrite state writeAndFlush = mask $ \restore -> do
  reservation <- State.beginBufferUse state
  let cancel = void (State.cancelBufferUse reservation)
  restore (awaitUseCompletion (State.reservationPreviousUse reservation)) `onException` cancel
  restore writeAndFlush `onException` cancel
  committed <- State.commitBufferUse reservation hostWriteUse
  unless committed (throwIO (VulkanFailure "dynamic buffer write" "stale buffer-state reservation"))

hostWriteUse :: State.BufferUse
hostWriteUse =
  State.BufferUse
    { State.bufferUseStage = Stage2.PIPELINE_STAGE_2_HOST_BIT
    , State.bufferUseAccess = Access2.ACCESS_2_HOST_WRITE_BIT
    , State.bufferUseCompletion = Nothing
    }

dynamicSliceFor :: DynamicBuffer usages a -> Int -> Int -> Int -> IO DynamicSlice
dynamicSliceFor buffer copyIndex elementOffset elementCount = do
  state <- dynamicStateFor buffer copyIndex
  byteOffset <- checkedRange buffer copyIndex elementOffset elementCount
  either throwIO pure (checkDynamicDescriptorOffset (dynamicDescriptorOffsetAlignment buffer) elementOffset byteOffset)
  pure
    DynamicSlice
      { dynamicSliceOwner = contextIdentity (dynamicContext buffer)
      , dynamicSliceGeneration = dynamicGeneration buffer
      , dynamicSliceHandle = dynamicBufferHandle buffer
      , dynamicSliceState = state
      , dynamicSliceElements = elementCount
      , dynamicSliceElementBytes = dynamicElementBytes buffer
      , dynamicSliceByteOffset = fromIntegral byteOffset
      , dynamicSliceUsageFlags = dynamicUsageFlags buffer
      , acquireDynamicSliceLease = acquireDynamicBufferLease buffer
      , quarantineDynamicSlice = mask_ $ do
          State.quarantineBufferState state
          Lifetime.quarantineLifetimeGate (dynamicLifetimeGate buffer)
      }

checkDynamicDescriptorOffset :: Int -> Int -> Int -> Either VpipeError ()
checkDynamicDescriptorOffset requiredAlignment elementOffset byteOffset
  | requiredAlignment > 0 && byteOffset `mod` requiredAlignment == 0 = Right ()
  | otherwise =
      Left
        DynamicBufferDescriptorOffsetMisaligned
          { dynamicBufferDescriptorElementOffset = elementOffset
          , dynamicBufferDescriptorByteOffset = byteOffset
          , dynamicBufferDescriptorRequiredAlignment = requiredAlignment
          }

dynamicStateFor :: DynamicBuffer usages a -> Int -> IO State.BufferState
dynamicStateFor buffer copyIndex =
  maybe
    (throwIO (VulkanFailure "dynamic buffer" "copy index is outside the allocation's fixed copy count"))
    pure
    (dynamicStates buffer Vector.!? copyIndex)

readDynamicBuffer :: forall usages a. (BufferFormat a) => DynamicBuffer usages a -> Int -> Int -> Int -> IO [HostFormat a]
readDynamicBuffer buffer copyIndex offset count = withDynamicBufferLease buffer $ withMVar (dynamicLock buffer) $ \_ -> do
  byteOffset <- checkedRange buffer copyIndex offset count
  state <- dynamicStateFor buffer copyIndex
  State.lastBufferUse state >>= awaitUseCompletion
  when (count > 0) $
    mapVma "vmaInvalidateAllocation(dynamic)" (VMA.invalidateAllocation (dynamicAllocator buffer) (dynamicAllocation buffer) (fromIntegral byteOffset) (fromIntegral (count * dynamicElementBytes buffer)))
  readValues (dynamicMappedPointer buffer `plusPtr` byteOffset) count
 where
  readValues _ 0 = pure []
  readValues pointer remaining = do
    value <- peekBufferFor (dynamicLayout buffer) (Proxy @a) pointer
    (value :) <$> readValues (pointer `plusPtr` dynamicElementBytes buffer) (remaining - 1)

flushDynamicBuffer :: DynamicBuffer usages a -> Int -> Int -> Int -> IO ()
flushDynamicBuffer buffer copyIndex offset count = withDynamicBufferLease buffer $ withMVar (dynamicLock buffer) $ \_ -> do
  byteOffset <- checkedRange buffer copyIndex offset count
  state <- dynamicStateFor buffer copyIndex
  State.lastBufferUse state >>= awaitUseCompletion
  when (count > 0) $
    mapVma "vmaFlushAllocation(dynamic)" (VMA.flushAllocation (dynamicAllocator buffer) (dynamicAllocation buffer) (fromIntegral byteOffset) (fromIntegral (count * dynamicElementBytes buffer)))

invalidateDynamicBuffer :: DynamicBuffer usages a -> Int -> Int -> Int -> IO ()
invalidateDynamicBuffer buffer copyIndex offset count = withDynamicBufferLease buffer $ withMVar (dynamicLock buffer) $ \_ -> do
  byteOffset <- checkedRange buffer copyIndex offset count
  state <- dynamicStateFor buffer copyIndex
  State.lastBufferUse state >>= awaitUseCompletion
  when (count > 0) $
    mapVma "vmaInvalidateAllocation(dynamic)" (VMA.invalidateAllocation (dynamicAllocator buffer) (dynamicAllocation buffer) (fromIntegral byteOffset) (fromIntegral (count * dynamicElementBytes buffer)))

checkedRange :: DynamicBuffer usages a -> Int -> Int -> Int -> IO Int
checkedRange buffer copyIndex offset count
  | copyIndex < 0 || copyIndex >= dynamicCopies buffer = invalid
  | offset < 0 || count < 0 = invalid
  | offset > dynamicElements buffer = invalid
  | count > dynamicElements buffer - offset = invalid
  | otherwise = pure (copyIndex * dynamicSliceSize buffer + offset * dynamicElementBytes buffer)
 where
  invalid = throwIO (BufferElementRangeInvalid offset count (dynamicElements buffer))

checkedProduct :: Int -> Int -> IO Int
checkedProduct left right
  | left <= 0 = throwIO (BufferElementCountInvalid left)
  | right <= 0 = throwIO (BufferSizeOverflow left right)
  | left > maxBound `div` right = throwIO (BufferSizeOverflow left right)
  | otherwise = pure (left * right)

checkedAlignUp :: Int -> Int -> IO Int
checkedAlignUp value alignment
  | alignment <= 0 = throwIO (BufferSizeOverflow value alignment)
  | value > maxBound - (alignment - 1) = throwIO (BufferSizeOverflow value alignment)
  | otherwise = pure (((value + alignment - 1) `div` alignment) * alignment)

checkedDescriptorAlignment :: Context -> Usage.BufferUsageFlags -> IO Int
checkedDescriptorAlignment context usageFlags = do
  uniformAlignment <-
    if usageFlags .&. Usage.BUFFER_USAGE_UNIFORM_BUFFER_BIT /= zero
      then checkedDeviceAlignment (contextUniformBufferOffsetAlignment context)
      else pure 1
  storageAlignment <-
    if usageFlags .&. Usage.BUFFER_USAGE_STORAGE_BUFFER_BIT /= zero
      then checkedDeviceAlignment (contextStorageBufferOffsetAlignment context)
      else pure 1
  checkedLcm uniformAlignment storageAlignment

checkedDeviceAlignment :: Word64 -> IO Int
checkedDeviceAlignment alignment
  | alignment == 0 = pure 1
  | alignment > fromIntegral (maxBound :: Int) = throwIO (BufferSizeOverflow 1 maxBound)
  | otherwise = pure (fromIntegral alignment)

checkedLcm :: Int -> Int -> IO Int
checkedLcm left right
  | left <= 0 || right <= 0 = throwIO (BufferSizeOverflow left right)
  | factor > maxBound `div` right = throwIO (BufferSizeOverflow factor right)
  | otherwise = pure (factor * right)
 where
  factor = left `div` gcd left right

withDynamicBufferLease :: DynamicBuffer usages a -> IO b -> IO b
withDynamicBufferLease buffer action =
  withContextLease (dynamicContext buffer) $
    Lifetime.withLifetimeLease
      (dynamicLifetimeGate buffer)
      (throwIO BufferReleased)
      action

acquireDynamicBufferLease :: DynamicBuffer usages a -> IO (IO ())
acquireDynamicBufferLease buffer = do
  lease <- Lifetime.acquireLifetimeLease (dynamicLifetimeGate buffer)
  maybe (throwIO BufferReleased) pure lease

awaitStateCompletion :: State.BufferState -> IO ()
awaitStateCompletion state = State.lastBufferUse state >>= awaitUseCompletion

awaitUseCompletion :: Maybe State.BufferUse -> IO ()
awaitUseCompletion Nothing = pure ()
awaitUseCompletion (Just use) = case State.bufferUseCompletion use of
  Nothing -> pure ()
  Just completion ->
    waitTimelineLeased
      (State.bufferCompletionQueue completion)
      (State.bufferCompletionTimeline completion)

releaseDynamicOnce :: MVar Bool -> IO () -> IO ()
releaseDynamicOnce state release =
  modifyMVarMasked_ state $ \released ->
    if released
      then pure True
      else release >> pure True

bufferHandleWord :: Handles.Buffer -> Word64
bufferHandleWord (Handles.Buffer handle) = handle

mapVma :: String -> IO b -> IO b
mapVma operation action =
  action `catch` \(error' :: Vulkan.VulkanException) ->
    if Vulkan.vulkanExceptionResult error' == Result.ERROR_DEVICE_LOST
      then throwIO DeviceLost
      else throwIO (VulkanFailure operation (show (Vulkan.vulkanExceptionResult error')))
