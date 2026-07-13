{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Compute lowering into a submission-free frame command.
module Vpipe.Compute.Frame.Internal (
  preparedComputeFrameContext,
  preparedComputeFrameDescriptorLayout,
  prepareComputeFrameCommandLeased,
  prepareComputeFrameCommandForLeased,
  prepareComputeFrameCommandWithDescriptorHookLeased,
  prepareComputeFrameCommandWith,
) where

import Control.Concurrent.MVar (readMVar)
import Control.Exception (throwIO)
import Control.Monad (unless, when)
import Data.Bits ((.&.), (.|.))
import Data.ByteString qualified as ByteString
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Data.Word (Word32, Word64)
import GHC.TypeLits (KnownNat, natVal)
import Vulkan.Core10.Enums.BufferUsageFlagBits qualified as BufferUsage
import Vulkan.Core13.Enums.AccessFlags2 qualified as Access2
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Zero (zero)

import Vpipe.Buffer.Format qualified as BufferFormat
import Vpipe.Compute.Compile.Internal (CompiledCompute (..), Dispatch, resolveComputeBindings, resolveComputePushConstants, workgroupCounts)
import Vpipe.Compute.Runtime.Internal (ComputeHealth (..), ComputeRuntime (..), PreparedCompute (..), recordDispatch)
import Vpipe.Context.Internal (Context, contextIdentity, contextMaxComputeWorkGroupCount, contextMaxComputeWorkGroupInvocations, contextMaxComputeWorkGroupSize, contextStorageBufferOffsetAlignment)
import Vpipe.Descriptor.Internal (DescriptorFrame, DescriptorLayout, descriptorLayoutPipelineLayoutHandle, resolveDescriptorFrameForRecordingLeased)
import Vpipe.Error (VpipeError (VulkanFailure))
import Vpipe.Frame.Resource.Internal (FrameBufferUse (..), FrameCommand, newFrameCommand)
import Vpipe.Pipeline.Internal qualified as Pipeline
import Vpipe.Pipeline.Resource.Internal qualified as Resource

preparedComputeFrameContext :: PreparedCompute env x y z -> Context
preparedComputeFrameContext = computeRuntimeContext . preparedComputeRuntime

preparedComputeFrameDescriptorLayout :: PreparedCompute env x y z -> DescriptorLayout
preparedComputeFrameDescriptorLayout = preparedComputeDescriptorLayout

prepareComputeFrameCommandLeased :: forall env x y z. (KnownNat x, KnownNat y, KnownNat z) => PreparedCompute env x y z -> DescriptorFrame -> env -> (Word32, Word32, Word32) -> IO (Maybe FrameCommand)
prepareComputeFrameCommandLeased = prepareComputeFrameCommandWithDescriptorHookLeased (pure ())

prepareComputeFrameCommandWithDescriptorHookLeased :: forall env x y z. (KnownNat x, KnownNat y, KnownNat z) => IO () -> PreparedCompute env x y z -> DescriptorFrame -> env -> (Word32, Word32, Word32) -> IO (Maybe FrameCommand)
prepareComputeFrameCommandWithDescriptorHookLeased beforeDescriptorWrite prepared descriptorFrame environment counts =
  prepareComputeFrameCommandWith
    (validatePreparedCompute @x @y @z prepared counts)
    (buildComputeCommand beforeDescriptorWrite prepared descriptorFrame environment counts)
    counts

prepareComputeFrameCommandForLeased :: forall env x y z. (KnownNat x, KnownNat y, KnownNat z) => PreparedCompute env x y z -> DescriptorFrame -> env -> (Integer, Integer, Integer) -> IO (Maybe FrameCommand)
prepareComputeFrameCommandForLeased prepared descriptorFrame environment totals = do
  counts <-
    either
      (computeFailure "compute workload validation" . show)
      pure
      (workgroupCounts (compiledComputeDispatch (preparedComputeCompiled prepared)) totals)
  prepareComputeFrameCommandLeased prepared descriptorFrame environment counts

{- | Injected zero-dispatch seam. Validation always runs, while command
construction is skipped whenever any workgroup dimension is zero.
-}
prepareComputeFrameCommandWith :: IO () -> IO FrameCommand -> (Word32, Word32, Word32) -> IO (Maybe FrameCommand)
prepareComputeFrameCommandWith validate build counts = do
  validate
  if hasZeroDimension counts
    then pure Nothing
    else Just <$> build

validatePreparedCompute :: forall x y z env. (KnownNat x, KnownNat y, KnownNat z) => PreparedCompute env x y z -> (Word32, Word32, Word32) -> IO ()
validatePreparedCompute prepared counts = do
  health <- readMVar (preparedComputeHealth prepared)
  when (health == ComputePoisoned) $
    computeFailure "compute frame preparation" "this prepared pipeline has an unretired submission; destroy its Context"
  validateLocalSize (compiledComputeDispatch (preparedComputeCompiled prepared)) context
  let countLimit = contextMaxComputeWorkGroupCount context
  unless (within3 counts countLimit) $
    computeFailure "compute dispatch validation" ("workgroup counts " <> show counts <> " exceed the device limit " <> show countLimit)
 where
  context = preparedComputeFrameContext prepared

validateLocalSize :: forall x y z. (KnownNat x, KnownNat y, KnownNat z) => Dispatch x y z -> Context -> IO ()
validateLocalSize _ context = do
  let requested = (natVal (Proxy @x), natVal (Proxy @y), natVal (Proxy @z))
      (limitX, limitY, limitZ) = contextMaxComputeWorkGroupSize context
      limit = (toInteger limitX, toInteger limitY, toInteger limitZ)
      invocations = product3 requested
      invocationLimit = toInteger (contextMaxComputeWorkGroupInvocations context)
  when (any3 (<= 0) requested) $
    computeFailure "compute preparation" "local workgroup dimensions must be positive"
  unless (within3 requested limit) $
    computeFailure "compute preparation" ("local workgroup size " <> show requested <> " exceeds the device limit " <> show limit)
  when (invocations > invocationLimit) $
    computeFailure "compute preparation" ("local workgroup product " <> show invocations <> " exceeds the device limit " <> show invocationLimit)

buildComputeCommand :: IO () -> PreparedCompute env x y z -> DescriptorFrame -> env -> (Word32, Word32, Word32) -> IO FrameCommand
buildComputeCommand beforeDescriptorWrite prepared descriptorFrame environment counts = do
  resolved <-
    either
      (computeFailure "compute binding resolution" . show)
      pure
      (resolveComputeBindings compiled environment)
  pushes <- resolveComputePushConstants compiled environment
  validatePushConstants interface pushes
  buffers <- validateStorageBindings context interface resolved
  beforeDescriptorWrite
  descriptorSet <-
    resolveDescriptorFrameForRecordingLeased
      layout
      descriptorFrame
      resolved
  let pipelineLayout = descriptorLayoutPipelineLayoutHandle layout
  newFrameCommand
    (fmap frameBufferHandle buffers)
    buffers
    []
    []
    (\commandBuffer _ -> recordDispatch commandBuffer (preparedComputePipeline prepared) pipelineLayout descriptorSet pushes counts)
 where
  compiled = preparedComputeCompiled prepared
  interface = compiledComputeInterface compiled
  context = preparedComputeFrameContext prepared
  layout = preparedComputeDescriptorLayout prepared

validatePushConstants :: Pipeline.PipelineInterface -> [Pipeline.ResolvedPushConstant] -> IO ()
validatePushConstants interface resolved = do
  let expected = Pipeline.pipelinePushConstants interface
  unless (length expected == length resolved) $
    computeFailure "compute push constants" "resolved push-constant count does not match the pipeline layout"
  traverse_ validate (zip expected resolved)
 where
  validate (range, value) = do
    unless (Pipeline.pushConstantName range == Pipeline.resolvedPushConstantName value) $
      computeFailure "compute push constants" "resolved push-constant names are out of order"
    unless (Pipeline.pushConstantOffset range == Pipeline.resolvedPushConstantOffset value) $
      computeFailure "compute push constants" "resolved push-constant offsets do not match the pipeline layout"
    unless (Pipeline.pushConstantSize range == ByteString.length (Pipeline.resolvedPushConstantBytes value)) $
      computeFailure "compute push constants" "resolved push-constant byte size does not match the pipeline layout"

validateStorageBindings :: Context -> Pipeline.PipelineInterface -> Pipeline.ResolvedBindingPlan -> IO [FrameBufferUse]
validateStorageBindings context interface resolved = do
  unless (null (Pipeline.resolvedVertexBuffers resolved)) (unexpected "vertex buffers")
  unless (null (Pipeline.resolvedUniformBuffers resolved)) (unexpected "uniform buffers")
  unless (null (Pipeline.resolvedTextures resolved)) (unexpected "textures")
  unless (null (Pipeline.resolvedColorImages resolved)) (unexpected "color images")
  unless (null (Pipeline.resolvedDepthImages resolved)) (unexpected "depth images")
  let expected = Pipeline.pipelineResources interface
      actual = Pipeline.resolvedStorageBuffers resolved
  unless (length expected == length actual) $
    computeFailure "compute resource validation" "resolved storage-buffer count does not match the interface"
  traverse validateOne (zip expected actual)
 where
  unexpected kind = computeFailure "compute resource validation" ("a compute binding plan unexpectedly contains " <> kind)
  validateOne (binding, Pipeline.ResolvedStorageBuffer name set bindingIndex handle) = do
    unless
      ( name == Pipeline.resourceBindingName binding
          && set == Pipeline.resourceBindingSet binding
          && bindingIndex == Pipeline.resourceBindingBinding binding
      )
      $ computeFailure "compute resource validation" ("resolved storage binding does not match " <> show (Pipeline.resourceBindingName binding))
    (fieldLayout, access) <- case Pipeline.resourceBindingShape binding of
      Pipeline.StorageArrayShape _ layout access -> pure (layout, access)
      shape -> computeFailure "compute resource validation" ("compute interface contains a non-runtime-array resource: " <> show shape)
    metadata <- requireBufferMetadata context name handle
    unless ((BufferUsage.BUFFER_USAGE_STORAGE_BUFFER_BIT .&. Resource.bufferBindingUsage metadata) /= zero) $
      computeFailure "compute resource validation" ("storage buffer " <> show name <> " lacks STORAGE usage")
    let expectedStride = BufferFormat.layoutSize (BufferFormat.layoutOf BufferFormat.Std430 fieldLayout)
    unless (Resource.bufferBindingStride metadata == expectedStride) $
      computeFailure "compute resource validation" ("storage buffer " <> show name <> " has an incompatible stride")
    byteSize <- validateBufferRange context name metadata
    pure
      FrameBufferUse
        { frameBufferHandle = handle
        , frameBufferMetadata = metadata
        , frameBufferStage = Stage2.PIPELINE_STAGE_2_COMPUTE_SHADER_BIT
        , frameBufferAccess = storageAccessFlags access
        , frameBufferByteSize = byteSize
        }

requireBufferMetadata :: Context -> String -> Pipeline.RuntimeHandle -> IO Resource.BufferBindingMetadata
requireBufferMetadata context name handle = do
  unless (Resource.runtimeHandleOwner handle == Just (contextIdentity context)) $
    computeFailure "compute resource validation" ("storage buffer " <> show name <> " is unmanaged or belongs to a different context")
  unless (Resource.runtimeHandleKind handle == Resource.RuntimeObjectBuffer) $
    computeFailure "compute resource validation" ("storage buffer " <> show name <> " must be a managed Buffer handle")
  maybe
    (computeFailure "compute resource validation" ("storage buffer " <> show name <> " lacks buffer metadata"))
    pure
    (Resource.runtimeBufferMetadata handle)

validateBufferCount :: String -> Resource.BufferBindingMetadata -> IO Word64
validateBufferCount name metadata = do
  let count = Resource.bufferBindingElementCount metadata
      stride = Resource.bufferBindingStride metadata
      byteSize = toInteger count * toInteger stride
  when (count <= 0 || toInteger count > toInteger (maxBound :: Word32)) $
    computeFailure "compute resource validation" ("storage buffer " <> show name <> " has an invalid element count")
  when (stride <= 0 || byteSize > toInteger (maxBound :: Word64)) $
    computeFailure "compute resource validation" ("storage buffer " <> show name <> " has an invalid byte size")
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
  Pipeline.StorageReadWrite -> Access2.ACCESS_2_SHADER_READ_BIT .|. Access2.ACCESS_2_SHADER_WRITE_BIT
  Pipeline.StorageAtomic -> Access2.ACCESS_2_SHADER_READ_BIT .|. Access2.ACCESS_2_SHADER_WRITE_BIT

hasZeroDimension :: (Word32, Word32, Word32) -> Bool
hasZeroDimension (x, y, z) = x == 0 || y == 0 || z == 0

within3 :: (Ord a) => (a, a, a) -> (a, a, a) -> Bool
within3 (x, y, z) (limitX, limitY, limitZ) = x <= limitX && y <= limitY && z <= limitZ

any3 :: (a -> Bool) -> (a, a, a) -> Bool
any3 predicate (x, y, z) = predicate x || predicate y || predicate z

product3 :: (Num a) => (a, a, a) -> a
product3 (x, y, z) = x * y * z

computeFailure :: String -> String -> IO a
computeFailure operation detail = throwIO (VulkanFailure operation detail)
