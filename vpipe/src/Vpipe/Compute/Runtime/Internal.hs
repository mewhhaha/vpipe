{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Synchronous compute preparation, recording, submission, and retirement.
module Vpipe.Compute.Runtime.Internal (
  ComputeRuntime (..),
  PreparedCompute (..),
  ComputeHealth (..),
  ComputeStats (..),
  newComputeRuntime,
  computeStats,
  prepareComputePipeline,
  dispatch,
  dispatchFor,
  recordDispatch,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked_, newMVar, readMVar, withMVar)
import Control.Exception (SomeException, catch, finally, mask, mask_, onException, throwIO)
import Control.Monad (foldM, unless, void, when)
import Data.Bits ((.&.), (.|.))
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.Proxy (Proxy (..))
import Data.Vector qualified as Vector
import Data.Word (Word32, Word64)
import Foreign.Ptr (castPtr, ptrToWordPtr)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.CommandBuffer qualified as CommandBuffer
import Vulkan.Core10.CommandBufferBuilding qualified as Command
import Vulkan.Core10.CommandPool qualified as CommandPool
import Vulkan.Core10.Enums.BufferUsageFlagBits qualified as BufferUsage
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.PipelineBindPoint qualified as PipelineBindPoint
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Enums.ShaderStageFlagBits qualified as ShaderStage
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Core13.Enums.AccessFlags2 qualified as Access2
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 qualified as Sync2
import Vulkan.Exception qualified as Vulkan
import Vulkan.Zero (zero)

import Vpipe.Buffer.Format qualified as BufferFormat
import Vpipe.Buffer.State qualified as BufferState
import Vpipe.Compute.Compile.Internal (CompiledCompute (..), Dispatch, resolveComputeBindings, resolveComputePushConstants, workgroupCounts)
import Vpipe.Compute.Pipeline.Internal (ComputePipelineDescription (..), acquireComputePipelineLeased)
import Vpipe.Context.Internal (Context, contextDevice, contextGraphicsCache, contextIdentity, contextMaxComputeWorkGroupCount, contextMaxComputeWorkGroupInvocations, contextMaxComputeWorkGroupSize, contextStorageBufferOffsetAlignment, derivedObjectName, graphicsQueue, registerContextFinalizerLeased, setObjectNameLeased, withContextLease)
import Vpipe.Context.Queue.Internal (Queue, QueueDependency (..), SubmissionPublicationOutcome (..), queueFamilyIndex, submitCommandBuffersWithPublicationLeased, waitTimelineLeased)
import Vpipe.Descriptor.Internal (DescriptorFrame, DescriptorLayout, descriptorLayoutPipelineLayoutHandle, newDescriptorFrameLeased, newDescriptorLayoutLeased, resetDescriptorFrameLeased, resolveDescriptorFrameForRecordingLeased)
import Vpipe.Diagnostics.Dump.Internal (ShaderDump (..), ShaderDumpStage (DumpCompute))
import Vpipe.Error (VpipeError (..))
import Vpipe.Graphics.Cache.Internal (ComputeCacheStats (..), GraphicsCache (..))
import Vpipe.Graphics.Submission.Internal (OwnedActions, SubmittedWorkStatus (..), confirmSubmittedWork, newOwnedActions, releaseActions, releaseOwnedActions, retireOwnedActions, transferOwnedActions)
import Vpipe.Pipeline.Internal qualified as Pipeline
import Vpipe.Pipeline.Resource.Internal qualified as Resource
import Vpipe.SpirV.Assembler (moduleBytes)

data ComputeRuntime = ComputeRuntime
  { computeRuntimeContext :: Context
  , computeRuntimeCache :: GraphicsCache
  }

data PreparedCompute env (x :: Nat) (y :: Nat) (z :: Nat) = PreparedCompute
  { preparedComputeRuntime :: ComputeRuntime
  , preparedComputeCompiled :: CompiledCompute env x y z
  , preparedComputeDescriptorLayout :: DescriptorLayout
  , preparedComputeDescriptorFrame :: DescriptorFrame
  , preparedComputePipeline :: Handles.Pipeline
  , preparedComputeLock :: MVar ()
  , preparedComputeHealth :: MVar ComputeHealth
  }

type role PreparedCompute nominal nominal nominal nominal

data ComputeStats = ComputeStats
  { computeShaderModuleCreations :: Int
  , computePipelineCreations :: Int
  }
  deriving stock (Eq, Show)

data ComputeHealth = ComputeHealthy | ComputePoisoned
  deriving stock (Eq)

data BufferIntent = BufferIntent
  { bufferIntentHandle :: Pipeline.RuntimeHandle
  , bufferIntentMetadata :: Resource.BufferBindingMetadata
  , bufferIntentStage :: Stage2.PipelineStageFlags2
  , bufferIntentAccess :: Access2.AccessFlags2
  , bufferIntentByteSize :: Word64
  }

data ReservedBuffer = ReservedBuffer BufferIntent BufferState.Reservation

newComputeRuntime :: Context -> IO ComputeRuntime
newComputeRuntime context =
  withContextLease context (pure (ComputeRuntime context (contextGraphicsCache context)))

computeStats :: ComputeRuntime -> IO ComputeStats
computeStats runtime =
  withContextLease (computeRuntimeContext runtime) $ do
    stats <- readMVar (cachedComputeStats (computeRuntimeCache runtime))
    pure
      ComputeStats
        { computeShaderModuleCreations = cachedComputeShaderModuleCreations stats
        , computePipelineCreations = cachedComputePipelineCreations stats
        }

prepareComputePipeline :: forall env x y z. (KnownNat x, KnownNat y, KnownNat z) => ComputeRuntime -> CompiledCompute env x y z -> IO (PreparedCompute env x y z)
prepareComputePipeline runtime compiled =
  withContextLease context $ do
    validateLocalSize (compiledComputeDispatch compiled) context
    layout <- newDescriptorLayoutLeased context interface
    frame <- newDescriptorFrameLeased layout
    let description =
          ComputePipelineDescription
            { computeShaderBytes =
                LazyByteString.toStrict (moduleBytes (compiledComputeModule compiled))
            , computeShaderDump =
                ShaderDump
                  { shaderDumpName = computeDumpName (compiledComputeDispatch compiled)
                  , shaderDumpStage = DumpCompute
                  , shaderDumpModule = compiledComputeModule compiled
                  , shaderDumpInterface = Pipeline.renderPipelineInterfaceTable interface
                  }
            , computePipelineLayoutStructure = pipelineLayoutKey interface
            }
    pipeline <-
      acquireComputePipelineLeased
        context
        (computeRuntimeCache runtime)
        (descriptorLayoutPipelineLayoutHandle layout)
        description
    lock <- newMVar ()
    health <- newMVar ComputeHealthy
    pure
      PreparedCompute
        { preparedComputeRuntime = runtime
        , preparedComputeCompiled = compiled
        , preparedComputeDescriptorLayout = layout
        , preparedComputeDescriptorFrame = frame
        , preparedComputePipeline = pipeline
        , preparedComputeLock = lock
        , preparedComputeHealth = health
        }
 where
  context = computeRuntimeContext runtime
  interface = compiledComputeInterface compiled

computeDumpName :: forall x y z. (KnownNat x, KnownNat y, KnownNat z) => Dispatch x y z -> String
computeDumpName _ =
  "compute-"
    <> show (natVal (Proxy @x))
    <> "x"
    <> show (natVal (Proxy @y))
    <> "x"
    <> show (natVal (Proxy @z))

dispatch :: PreparedCompute env x y z -> env -> (Int, Int, Int) -> IO ()
dispatch prepared environment counts =
  withPreparedCompute prepared $ do
    wordCounts <- validateHostCounts counts
    dispatchWord32Leased prepared environment wordCounts

dispatchFor :: forall env x y z. (KnownNat x, KnownNat y, KnownNat z) => PreparedCompute env x y z -> env -> (Integer, Integer, Integer) -> IO ()
dispatchFor prepared environment totals =
  withPreparedCompute prepared $ do
    counts <-
      either
        (computeFailure "compute workload validation" . show)
        pure
        (workgroupCounts (compiledComputeDispatch (preparedComputeCompiled prepared)) totals)
    dispatchWord32Leased prepared environment counts

withPreparedCompute :: PreparedCompute env x y z -> IO a -> IO a
withPreparedCompute prepared action =
  withMVar (preparedComputeLock prepared) $ \_ -> do
    withContextLease context $ do
      ensureComputeHealthy prepared
      action
 where
  context = computeRuntimeContext (preparedComputeRuntime prepared)

dispatchWord32Leased :: PreparedCompute env x y z -> env -> (Word32, Word32, Word32) -> IO ()
dispatchWord32Leased prepared environment counts = do
  validateDeviceCounts context counts
  unless (hasZeroDimension counts) $
    mask $ \restore -> do
      resolved <-
        either
          (computeFailure "compute binding resolution" . show)
          pure
          (resolveComputeBindings compiled environment)
      pushes <- resolveComputePushConstants compiled environment
      validatePushConstants interface pushes
      intents <- validateStorageBindings context interface resolved
      let handles = uniqueHandles (fmap bufferIntentHandle intents)
      releases <- acquireRuntimeLeases handles
      resourceReleases <- newOwnedActions releases
      let finishDispatch = do
            health <- readMVar (preparedComputeHealth prepared)
            when (health == ComputeHealthy) (resetDescriptorFrameLeased frame)
              `finally` releaseOwnedActions resourceReleases
      ( do
          descriptorSet <-
            resolveDescriptorFrameForRecordingLeased layout frame resolved
          reservations <- reserveBuffers intents
          reservationCleanup <- newOwnedActions [traverse_ cancelReservedBuffer reservations]
          ( do
              pool <- newCommandPool context queue
              poolCleanup <-
                newOwnedActions
                  [CommandPool.destroyCommandPool (contextDevice context) pool Nothing]
              recordSubmitWait
                restore
                prepared
                handles
                resourceReleases
                reservationCleanup
                poolCleanup
                pool
                reservations
                descriptorSet
                pushes
                counts
                `finally` releaseOwnedActions poolCleanup
            )
            `finally` releaseOwnedActions reservationCleanup
        )
        `finally` finishDispatch
 where
  runtime = preparedComputeRuntime prepared
  context = computeRuntimeContext runtime
  compiled = preparedComputeCompiled prepared
  interface = compiledComputeInterface compiled
  layout = preparedComputeDescriptorLayout prepared
  frame = preparedComputeDescriptorFrame prepared
  queue = graphicsQueue context

validateHostCounts :: (Int, Int, Int) -> IO (Word32, Word32, Word32)
validateHostCounts (x, y, z) =
  (,,) <$> dimension "x" x <*> dimension "y" y <*> dimension "z" z
 where
  dimension label value
    | value < 0 =
        computeFailure
          "compute dispatch validation"
          (label <> " workgroup count must be non-negative")
    | toInteger value > toInteger (maxBound :: Word32) =
        computeFailure
          "compute dispatch validation"
          (label <> " workgroup count exceeds Word32")
    | otherwise = pure (fromIntegral value)

validateDeviceCounts :: Context -> (Word32, Word32, Word32) -> IO ()
validateDeviceCounts context requested = do
  let limit = contextMaxComputeWorkGroupCount context
  unless (within3 requested limit) $
    computeFailure
      "compute dispatch validation"
      ("workgroup counts " <> show requested <> " exceed the device limit " <> show limit)

hasZeroDimension :: (Word32, Word32, Word32) -> Bool
hasZeroDimension (x, y, z) = x == 0 || y == 0 || z == 0

validateLocalSize :: forall x y z. (KnownNat x, KnownNat y, KnownNat z) => Dispatch x y z -> Context -> IO ()
validateLocalSize _ context = do
  let requested =
        ( natVal (Proxy @x)
        , natVal (Proxy @y)
        , natVal (Proxy @z)
        )
      (limitX, limitY, limitZ) = contextMaxComputeWorkGroupSize context
      limit = (toInteger limitX, toInteger limitY, toInteger limitZ)
      invocations = product3 requested
      invocationLimit = toInteger (contextMaxComputeWorkGroupInvocations context)
  when (any3 (<= 0) requested) $
    computeFailure "compute preparation" "local workgroup dimensions must be positive"
  unless (within3 requested limit) $
    computeFailure
      "compute preparation"
      ("local workgroup size " <> show requested <> " exceeds the device limit " <> show limit)
  when (invocations > invocationLimit) $
    computeFailure
      "compute preparation"
      ("local workgroup product " <> show invocations <> " exceeds the device limit " <> show invocationLimit)

within3 :: (Ord a) => (a, a, a) -> (a, a, a) -> Bool
within3 (x, y, z) (limitX, limitY, limitZ) =
  x <= limitX && y <= limitY && z <= limitZ

any3 :: (a -> Bool) -> (a, a, a) -> Bool
any3 predicate (x, y, z) = predicate x || predicate y || predicate z

product3 :: (Num a) => (a, a, a) -> a
product3 (x, y, z) = x * y * z

pipelineLayoutKey :: Pipeline.PipelineInterface -> ByteString.ByteString
pipelineLayoutKey interface =
  ByteString8.pack
    (show (Pipeline.pipelineResources interface, Pipeline.pipelinePushConstants interface))

ensureComputeHealthy :: PreparedCompute env x y z -> IO ()
ensureComputeHealthy prepared = do
  health <- readMVar (preparedComputeHealth prepared)
  when (health == ComputePoisoned) $
    computeFailure
      "compute dispatch"
      "this prepared pipeline has an unretired submission; destroy its Context"

validatePushConstants :: Pipeline.PipelineInterface -> [Pipeline.ResolvedPushConstant] -> IO ()
validatePushConstants interface resolved = do
  let expected = Pipeline.pipelinePushConstants interface
  unless (length expected == length resolved) $
    computeFailure
      "compute push constants"
      "resolved push-constant count does not match the pipeline layout"
  traverse_ validate (zip expected resolved)
 where
  validate (range, value) = do
    unless (Pipeline.pushConstantName range == Pipeline.resolvedPushConstantName value) $
      computeFailure
        "compute push constants"
        "resolved push-constant names are out of order"
    unless (Pipeline.pushConstantOffset range == Pipeline.resolvedPushConstantOffset value) $
      computeFailure
        "compute push constants"
        "resolved push-constant offsets do not match the pipeline layout"
    unless
      ( Pipeline.pushConstantSize range
          == ByteString.length (Pipeline.resolvedPushConstantBytes value)
      )
      $ computeFailure
        "compute push constants"
        "resolved push-constant byte size does not match the pipeline layout"

validateStorageBindings :: Context -> Pipeline.PipelineInterface -> Pipeline.ResolvedBindingPlan -> IO [BufferIntent]
validateStorageBindings context interface resolved = do
  unless (null (Pipeline.resolvedVertexBuffers resolved)) $
    unexpected "vertex buffers"
  unless (null (Pipeline.resolvedUniformBuffers resolved)) $
    unexpected "uniform buffers"
  unless (null (Pipeline.resolvedTextures resolved)) $
    unexpected "textures"
  unless (null (Pipeline.resolvedColorImages resolved)) $
    unexpected "color images"
  unless (null (Pipeline.resolvedDepthImages resolved)) $
    unexpected "depth images"
  let expected = Pipeline.pipelineResources interface
      actual = Pipeline.resolvedStorageBuffers resolved
  unless (length expected == length actual) $
    computeFailure
      "compute resource validation"
      ( "resolved storage-buffer count does not match the interface: expected "
          <> show (length expected)
          <> ", received "
          <> show (length actual)
      )
  requested <- traverse validateOne (zip expected actual)
  foldM mergeBufferIntent [] requested
 where
  unexpected kind =
    computeFailure
      "compute resource validation"
      ("a compute binding plan unexpectedly contains " <> kind)
  validateOne
    ( binding
      , Pipeline.ResolvedStorageBuffer name set bindingIndex handle
      ) = do
      unless
        ( name == Pipeline.resourceBindingName binding
            && set == Pipeline.resourceBindingSet binding
            && bindingIndex == Pipeline.resourceBindingBinding binding
        )
        $ computeFailure
          "compute resource validation"
          ("resolved storage binding does not match " <> show (Pipeline.resourceBindingName binding))
      (fieldLayout, access) <- case Pipeline.resourceBindingShape binding of
        Pipeline.StorageArrayShape _ layout access -> pure (layout, access)
        shape ->
          computeFailure
            "compute resource validation"
            ("compute interface contains a non-runtime-array resource: " <> show shape)
      metadata <- requireBufferMetadata context name handle
      requireStorageUsage name metadata
      let expectedStride =
            BufferFormat.layoutSize
              (BufferFormat.layoutOf BufferFormat.Std430 fieldLayout)
      unless (Resource.bufferBindingStride metadata == expectedStride) $
        computeFailure
          "compute resource validation"
          ( "storage buffer "
              <> show name
              <> " has stride "
              <> show (Resource.bufferBindingStride metadata)
              <> ", expected "
              <> show expectedStride
          )
      byteSize <- validateBufferRange context name metadata
      pure
        BufferIntent
          { bufferIntentHandle = handle
          , bufferIntentMetadata = metadata
          , bufferIntentStage = Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT
          , bufferIntentAccess = storageAccessFlags access
          , bufferIntentByteSize = byteSize
          }

requireBufferMetadata :: Context -> String -> Pipeline.RuntimeHandle -> IO Resource.BufferBindingMetadata
requireBufferMetadata context name handle = do
  unless (Resource.runtimeHandleOwner handle == Just (contextIdentity context)) $
    computeFailure
      "compute resource validation"
      ("storage buffer " <> show name <> " is unmanaged or belongs to a different context")
  unless (Resource.runtimeHandleKind handle == Resource.RuntimeObjectBuffer) $
    computeFailure
      "compute resource validation"
      ("storage buffer " <> show name <> " must be a managed Buffer handle")
  metadata <-
    maybe
      (computeFailure "compute resource validation" ("storage buffer " <> show name <> " lacks buffer metadata"))
      pure
      (Resource.runtimeBufferMetadata handle)
  let Handles.Buffer rawWord = Resource.bufferBindingRawHandle metadata
  unless (rawWord == Resource.runtimeHandleWord handle) $
    computeFailure
      "compute resource validation"
      ("storage buffer " <> show name <> " has inconsistent raw-handle metadata")
  pure metadata

requireStorageUsage :: String -> Resource.BufferBindingMetadata -> IO ()
requireStorageUsage name metadata =
  unless
    ( (BufferUsage.BUFFER_USAGE_STORAGE_BUFFER_BIT .&. Resource.bufferBindingUsage metadata)
        /= zero
    )
    $ computeFailure
      "compute resource validation"
      ("storage buffer " <> show name <> " lacks STORAGE usage")

validateBufferCount :: String -> Resource.BufferBindingMetadata -> IO Word64
validateBufferCount name metadata = do
  let count = Resource.bufferBindingElementCount metadata
      stride = Resource.bufferBindingStride metadata
      byteSize = toInteger count * toInteger stride
  when (count <= 0 || toInteger count > toInteger (maxBound :: Word32)) $
    computeFailure
      "compute resource validation"
      ("storage buffer " <> show name <> " has an invalid element count " <> show count)
  when (stride <= 0) $
    computeFailure
      "compute resource validation"
      ("storage buffer " <> show name <> " has an invalid stride " <> show stride)
  when (byteSize > toInteger (maxBound :: Word64)) $
    computeFailure
      "compute resource validation"
      ("storage buffer " <> show name <> " byte size exceeds Word64")
  pure (fromInteger byteSize)

validateBufferRange :: Context -> String -> Resource.BufferBindingMetadata -> IO Word64
validateBufferRange context name metadata = do
  byteSize <- validateBufferCount name metadata
  let byteOffset = Resource.bufferBindingByteOffset metadata
      requiredAlignment = max 1 (contextStorageBufferOffsetAlignment context)
      rangeEnd = toInteger byteOffset + toInteger byteSize
  unless (byteOffset `mod` requiredAlignment == 0) $
    computeFailure
      "compute resource validation"
      ( "storage buffer "
          <> show name
          <> " has byte offset "
          <> show byteOffset
          <> ", which is not aligned to "
          <> show requiredAlignment
      )
  when (rangeEnd > toInteger (maxBound :: Word64)) $
    computeFailure
      "compute resource validation"
      ("storage buffer " <> show name <> " byte range overflows VkDeviceSize")
  pure byteSize

storageAccessFlags :: Pipeline.StorageAccess -> Access2.AccessFlags2
storageAccessFlags access = case access of
  Pipeline.StorageReadOnly -> Access2.ACCESS_2_SHADER_READ_BIT
  Pipeline.StorageWriteOnly -> Access2.ACCESS_2_SHADER_WRITE_BIT
  Pipeline.StorageReadWrite ->
    Access2.ACCESS_2_SHADER_READ_BIT .|. Access2.ACCESS_2_SHADER_WRITE_BIT
  Pipeline.StorageAtomic ->
    Access2.ACCESS_2_SHADER_READ_BIT .|. Access2.ACCESS_2_SHADER_WRITE_BIT

mergeBufferIntent :: [BufferIntent] -> BufferIntent -> IO [BufferIntent]
mergeBufferIntent intents requested =
  case break ((== bufferIntentHandle requested) . bufferIntentHandle) intents of
    (_, [])
      | any (sameRawBufferIntent requested) intents ->
          computeFailure
            "compute resource validation"
            ( "distinct logical buffers reference the same raw buffer "
                <> show (Resource.bufferBindingRawHandle (bufferIntentMetadata requested))
            )
      | otherwise -> pure (intents <> [requested])
    (before, existing : after) -> do
      unless (sameBufferRange existing requested) $
        computeFailure
          "compute resource validation"
          "one buffer handle resolved to incompatible metadata"
      pure
        ( before
            <> [ existing
                   { bufferIntentStage =
                       bufferIntentStage existing .|. bufferIntentStage requested
                   , bufferIntentAccess =
                       bufferIntentAccess existing .|. bufferIntentAccess requested
                   }
               ]
            <> after
        )

sameBufferRange :: BufferIntent -> BufferIntent -> Bool
sameBufferRange left right =
  Resource.bufferBindingRawHandle (bufferIntentMetadata left)
    == Resource.bufferBindingRawHandle (bufferIntentMetadata right)
    && Resource.bufferBindingByteOffset (bufferIntentMetadata left)
      == Resource.bufferBindingByteOffset (bufferIntentMetadata right)
    && Resource.bufferBindingStride (bufferIntentMetadata left)
      == Resource.bufferBindingStride (bufferIntentMetadata right)
    && Resource.bufferBindingElementCount (bufferIntentMetadata left)
      == Resource.bufferBindingElementCount (bufferIntentMetadata right)
    && bufferIntentByteSize left == bufferIntentByteSize right

sameRawBufferIntent :: BufferIntent -> BufferIntent -> Bool
sameRawBufferIntent left right =
  Resource.bufferBindingRawHandle (bufferIntentMetadata left)
    == Resource.bufferBindingRawHandle (bufferIntentMetadata right)

uniqueHandles :: [Pipeline.RuntimeHandle] -> [Pipeline.RuntimeHandle]
uniqueHandles = foldl add []
 where
  add handles handle
    | handle `elem` handles = handles
    | otherwise = handles <> [handle]

acquireRuntimeLeases :: [Pipeline.RuntimeHandle] -> IO [IO ()]
acquireRuntimeLeases = foldM acquire []
 where
  acquire releases handle = do
    acquireLease <-
      maybe
        (computeFailure "compute resource validation" "a storage buffer has no managed lifetime")
        pure
        (Resource.runtimeHandleLease handle)
    release <- acquireLease `onException` releaseActions releases
    pure (release : releases)

reserveBuffers :: [BufferIntent] -> IO [ReservedBuffer]
reserveBuffers = go [] . sortOn bufferIntentOrder
 where
  go reserved [] = pure (reverse reserved)
  go reserved (intent : rest) = do
    reservation <-
      BufferState.beginBufferUse
        (Resource.bufferBindingState (bufferIntentMetadata intent))
        `onException` traverse_ cancelReservedBuffer reserved
    go (ReservedBuffer intent reservation : reserved) rest

  bufferIntentOrder intent =
    let Handles.Buffer word = Resource.bufferBindingRawHandle (bufferIntentMetadata intent)
     in word

cancelReservedBuffer :: ReservedBuffer -> IO ()
cancelReservedBuffer (ReservedBuffer _ reservation) =
  void (BufferState.cancelBufferUse reservation)

newCommandPool :: Context -> Queue -> IO Handles.CommandPool
newCommandPool context queue = do
  let device = contextDevice context
  pool <-
    mapVulkan
      "vkCreateCommandPool(compute)"
      ( CommandPool.createCommandPool
          device
          ( (zero :: CommandPool.CommandPoolCreateInfo)
              { CommandPool.flags = CommandPool.COMMAND_POOL_CREATE_TRANSIENT_BIT
              , CommandPool.queueFamilyIndex = queueFamilyIndex queue
              }
          )
          Nothing
      )
  setObjectNameLeased context ObjectType.OBJECT_TYPE_COMMAND_POOL (commandPoolHandleWord pool) (derivedObjectName "command-pool-compute" (commandPoolHandleWord pool))
    `onException` CommandPool.destroyCommandPool device pool Nothing
  pure pool

recordSubmitWait :: (IO () -> IO ()) -> PreparedCompute env x y z -> [Pipeline.RuntimeHandle] -> OwnedActions -> OwnedActions -> OwnedActions -> Handles.CommandPool -> [ReservedBuffer] -> Handles.DescriptorSet -> [Pipeline.ResolvedPushConstant] -> (Word32, Word32, Word32) -> IO ()
recordSubmitWait restore prepared handles resourceReleases reservationCleanup poolCleanup pool reservations descriptorSet pushes counts = do
  let runtime = preparedComputeRuntime prepared
      context = computeRuntimeContext runtime
      queue = graphicsQueue context
      layout = descriptorLayoutPipelineLayoutHandle (preparedComputeDescriptorLayout prepared)
  commandBuffer <- allocateCommandBuffer context pool
  let beginInfo =
        (zero :: CommandBuffer.CommandBufferBeginInfo '[])
          { CommandBuffer.flags = CommandBuffer.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
          }
  mapVulkan
    "vkBeginCommandBuffer(compute)"
    (CommandBuffer.beginCommandBuffer commandBuffer beginInfo)
  recordBufferBarriers queue commandBuffer reservations
  recordDispatch
    commandBuffer
    (preparedComputePipeline prepared)
    layout
    descriptorSet
    pushes
    counts
  mapVulkan
    "vkEndCommandBuffer(compute)"
    (CommandBuffer.endCommandBuffer commandBuffer)
  submission <-
    submitCommandBuffersWithPublicationLeased
      queue
      (bufferDependencies reservations)
      []
      []
      (Vector.singleton commandBuffer)
      (\signal -> commitBufferReservations queue signal reservations)
  case submission of
    SubmissionRejected primaryFailure -> throwIO primaryFailure
    SubmissionAcceptanceUnknown primaryFailure ->
      quarantineUnknownDispatchLeased primaryFailure prepared handles resourceReleases reservationCleanup poolCleanup reservations
    SubmissionAcceptedPublicationFailed signal primaryFailure -> do
      let wait = waitTimelineLeased queue signal
      completion <- confirmSubmittedWork (restore wait) wait
      case completion of
        SubmittedWorkComplete -> throwIO primaryFailure
        SubmittedWorkCompleteAfterFailure _waitFailure -> throwIO primaryFailure
        SubmittedWorkUncertain _waitFailure _fallbackFailure ->
          quarantineUnknownDispatchLeased primaryFailure prepared handles resourceReleases reservationCleanup poolCleanup reservations
    SubmissionAccepted signal -> finishAcceptedDispatch restore prepared resourceReleases poolCleanup queue signal

finishAcceptedDispatch :: (IO () -> IO ()) -> PreparedCompute env x y z -> OwnedActions -> OwnedActions -> Queue -> Word64 -> IO ()
finishAcceptedDispatch restore prepared resourceReleases poolCleanup queue signal = do
  let wait = waitTimelineLeased queue signal
  completion <- confirmSubmittedWork (restore wait) wait
  case completion of
    SubmittedWorkComplete -> pure ()
    SubmittedWorkCompleteAfterFailure primaryFailure -> throwIO primaryFailure
    SubmittedWorkUncertain primaryFailure _fallbackFailure -> do
      retireUnknownDispatchLeased prepared resourceReleases poolCleanup
      throwIO primaryFailure

quarantineUnknownDispatchLeased :: SomeException -> PreparedCompute env x y z -> [Pipeline.RuntimeHandle] -> OwnedActions -> OwnedActions -> OwnedActions -> [ReservedBuffer] -> IO a
quarantineUnknownDispatchLeased primaryFailure prepared handles resourceReleases reservationCleanup poolCleanup reservations = mask_ $ do
  bestEffort (void (transferOwnedActions reservationCleanup))
  traverse_ (bestEffort . quarantineReservedBufferState) reservations
  traverse_ (bestEffort . Resource.runtimeHandleQuarantine) handles
  bestEffort $
    modifyMVarMasked_
      (preparedComputeHealth prepared)
      (const (pure ComputePoisoned))
  void $
    retireOwnedActions
      (registerContextFinalizerLeased (computeRuntimeContext (preparedComputeRuntime prepared)))
      [poolCleanup, resourceReleases]
  throwIO primaryFailure

quarantineReservedBufferState :: ReservedBuffer -> IO ()
quarantineReservedBufferState (ReservedBuffer intent _) =
  BufferState.quarantineBufferState (Resource.bufferBindingState (bufferIntentMetadata intent))

retireUnknownDispatchLeased :: PreparedCompute env x y z -> OwnedActions -> OwnedActions -> IO ()
retireUnknownDispatchLeased prepared resourceReleases poolCleanup = do
  modifyMVarMasked_
    (preparedComputeHealth prepared)
    (const (pure ComputePoisoned))
  void $
    retireOwnedActions
      (registerContextFinalizerLeased (computeRuntimeContext (preparedComputeRuntime prepared)))
      [poolCleanup, resourceReleases]

allocateCommandBuffer :: Context -> Handles.CommandPool -> IO Handles.CommandBuffer
allocateCommandBuffer context pool = do
  commandBuffers <-
    mapVulkan
      "vkAllocateCommandBuffers(compute)"
      ( CommandBuffer.allocateCommandBuffers
          (contextDevice context)
          ( CommandBuffer.CommandBufferAllocateInfo
              pool
              CommandBuffer.COMMAND_BUFFER_LEVEL_PRIMARY
              1
          )
      )
  case Vector.toList commandBuffers of
    [commandBuffer] -> do
      setObjectNameLeased context ObjectType.OBJECT_TYPE_COMMAND_BUFFER (commandBufferHandleWord commandBuffer) (derivedObjectName "command-buffer-compute" (commandBufferHandleWord commandBuffer))
        `onException` CommandBuffer.freeCommandBuffers (contextDevice context) pool (Vector.singleton commandBuffer)
      pure commandBuffer
    values ->
      computeFailure
        "vkAllocateCommandBuffers(compute)"
        ("expected one command buffer, received " <> show (length values))

commandPoolHandleWord :: Handles.CommandPool -> Word64
commandPoolHandleWord (Handles.CommandPool handle) = handle

commandBufferHandleWord :: Handles.CommandBuffer -> Word64
commandBufferHandleWord = fromIntegral . ptrToWordPtr . Handles.commandBufferHandle

recordBufferBarriers :: Queue -> Handles.CommandBuffer -> [ReservedBuffer] -> IO ()
recordBufferBarriers queue commandBuffer buffers =
  unless (null buffers) $
    Sync2.cmdPipelineBarrier2
      commandBuffer
      ( (zero :: Sync2.DependencyInfo)
          { Sync2.bufferMemoryBarriers =
              Vector.fromList (fmap (bufferBarrier queue) buffers)
          }
      )

bufferBarrier :: Queue -> ReservedBuffer -> Chain.SomeStruct Sync2.BufferMemoryBarrier2
bufferBarrier queue (ReservedBuffer intent reservation) =
  Chain.SomeStruct
    ( (zero :: Sync2.BufferMemoryBarrier2 '[])
        { Sync2.srcStageMask = sourceBufferStage queue previous
        , Sync2.srcAccessMask = sourceBufferAccess queue previous
        , Sync2.dstStageMask = bufferIntentStage intent
        , Sync2.dstAccessMask = bufferIntentAccess intent
        , Sync2.srcQueueFamilyIndex = maxBound
        , Sync2.dstQueueFamilyIndex = maxBound
        , Sync2.buffer = Resource.bufferBindingRawHandle metadata
        , Sync2.offset = Resource.bufferBindingByteOffset metadata
        , Sync2.size = bufferIntentByteSize intent
        }
    )
 where
  previous = BufferState.reservationPreviousUse reservation
  metadata = bufferIntentMetadata intent

sourceBufferStage :: Queue -> Maybe BufferState.BufferUse -> Stage2.PipelineStageFlags2
sourceBufferStage _ Nothing = zero
sourceBufferStage queue (Just previous) =
  if bufferUseIsLocal queue previous then BufferState.bufferUseStage previous else zero

sourceBufferAccess :: Queue -> Maybe BufferState.BufferUse -> Access2.AccessFlags2
sourceBufferAccess _ Nothing = zero
sourceBufferAccess queue (Just previous) =
  if bufferUseIsLocal queue previous then BufferState.bufferUseAccess previous else zero

bufferUseIsLocal :: Queue -> BufferState.BufferUse -> Bool
bufferUseIsLocal queue previous = case BufferState.bufferUseCompletion previous of
  Nothing -> True
  Just completion -> BufferState.bufferCompletionQueueFamily completion == queueFamilyIndex queue

bufferDependencies :: [ReservedBuffer] -> [QueueDependency]
bufferDependencies buffers =
  [ QueueDependency
      (BufferState.bufferCompletionQueue completion)
      (BufferState.bufferCompletionTimeline completion)
      (bufferIntentStage intent)
  | ReservedBuffer intent reservation <- buffers
  , Just previous <- [BufferState.reservationPreviousUse reservation]
  , Just completion <- [BufferState.bufferUseCompletion previous]
  ]

commitBufferReservations :: Queue -> Word64 -> [ReservedBuffer] -> IO ()
commitBufferReservations queue signal buffers = do
  results <- traverse commit buffers
  unless (and results) $
    computeFailure
      "compute submission"
      "a submitted buffer-state reservation became stale"
 where
  commit (ReservedBuffer intent reservation) =
    BufferState.commitBufferUse
      reservation
      BufferState.BufferUse
        { BufferState.bufferUseStage = bufferIntentStage intent
        , BufferState.bufferUseAccess = bufferIntentAccess intent
        , BufferState.bufferUseCompletion =
            Just
              BufferState.BufferCompletion
                { BufferState.bufferCompletionQueue = queue
                , BufferState.bufferCompletionQueueFamily = queueFamilyIndex queue
                , BufferState.bufferCompletionTimeline = signal
                }
        }

{- | Record the bind/push/dispatch portion of a compute command buffer. Resource
barriers are recorded separately because they depend on reserved buffer state.
-}
recordDispatch :: Handles.CommandBuffer -> Handles.Pipeline -> Handles.PipelineLayout -> Handles.DescriptorSet -> [Pipeline.ResolvedPushConstant] -> (Word32, Word32, Word32) -> IO ()
recordDispatch commandBuffer pipeline layout descriptorSet pushes counts =
  unless (hasZeroDimension counts) $ do
    Command.cmdBindPipeline
      commandBuffer
      PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE
      pipeline
    Command.cmdBindDescriptorSets
      commandBuffer
      PipelineBindPoint.PIPELINE_BIND_POINT_COMPUTE
      layout
      0
      (Vector.singleton descriptorSet)
      Vector.empty
    traverse_ (recordPushConstant commandBuffer layout) pushes
    let (x, y, z) = counts
    Command.cmdDispatch commandBuffer x y z

recordPushConstant :: Handles.CommandBuffer -> Handles.PipelineLayout -> Pipeline.ResolvedPushConstant -> IO ()
recordPushConstant commandBuffer layout value =
  ByteString.useAsCStringLen
    (Pipeline.resolvedPushConstantBytes value)
    $ \(pointer, size) ->
      Command.cmdPushConstants
        commandBuffer
        layout
        ShaderStage.SHADER_STAGE_ALL
        (fromIntegral (Pipeline.resolvedPushConstantOffset value))
        (fromIntegral size)
        (castPtr pointer)

computeFailure :: String -> String -> IO a
computeFailure operation detail = throwIO (VulkanFailure operation detail)

bestEffort :: IO () -> IO ()
bestEffort action = action `catch` \(_ :: SomeException) -> pure ()

mapVulkan :: String -> IO a -> IO a
mapVulkan operation action =
  action `catch` \(error' :: Vulkan.VulkanException) ->
    if Vulkan.vulkanExceptionResult error' == Result.ERROR_DEVICE_LOST
      then throwIO DeviceLost
      else throwIO (VulkanFailure operation (show (Vulkan.vulkanExceptionResult error')))
