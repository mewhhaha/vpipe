{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vpipe.Descriptor.Internal (
  DescriptorLayout,
  DescriptorLayoutIdentity,
  DescriptorFrame,
  DescriptorRuntime,
  DescriptorStats (..),
  newDescriptorRuntime,
  newDescriptorRuntimeLeased,
  newDescriptorLayout,
  newDescriptorLayoutLeased,
  newDescriptorFrame,
  newDescriptorFrameLeased,
  destroyDescriptorFrameLeased,
  resetDescriptorFrame,
  resetDescriptorFrameLeased,
  resolveDescriptorFrame,
  resolveDescriptorFrameLeased,
  resolveDescriptorFrameForRecording,
  resolveDescriptorFrameForRecordingLeased,
  beginDescriptorFrame,
  beginDescriptorFrameLeased,
  resolveDescriptors,
  resolveDescriptorsLeased,
  descriptorStats,
  descriptorFrameStats,
  descriptorPipelineLayoutHandle,
  descriptorSetLayoutHandle,
  descriptorLayoutPipelineLayoutHandle,
  descriptorLayoutSetLayoutHandle,
  descriptorLayoutIdentity,
  descriptorRuntimeLayoutValue,
  poisonDescriptorFrameForTest,
  runDescriptorCleanupActionsForTest,
  newDescriptorLayoutIdentityForTest,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked, newMVar, readMVar)
import Control.Exception (SomeException, catch, displayException, mask, onException, throwIO, toException, try)
import Control.Monad (unless)
import Data.Bits ((.&.))
import Data.Foldable (traverse_)
import Data.List (intercalate, sortOn)
import Data.Unique (Unique, hashUnique, newUnique)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Numeric (showHex)
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.DescriptorSet qualified as Descriptor
import Vulkan.Core10.Enums.BufferUsageFlagBits qualified as BufferUsage
import Vulkan.Core10.Enums.DescriptorPoolCreateFlagBits qualified as PoolFlags
import Vulkan.Core10.Enums.DescriptorType qualified as DescriptorType
import Vulkan.Core10.Enums.ImageLayout qualified as ImageLayout
import Vulkan.Core10.Enums.ImageUsageFlagBits qualified as ImageUsage
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Enums.ShaderStageFlagBits qualified as ShaderStage
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Core10.PipelineLayout qualified as PipelineLayout
import Vulkan.Exception qualified as Vulkan
import Vulkan.Zero (zero)

import Vpipe.Context.Internal (Context, contextDevice, contextIdentity, contextStorageBufferOffsetAlignment, contextUniformBufferOffsetAlignment, derivedObjectName, registerContextFinalizerLeased, setObjectNameLeased, withContextLease)
import Vpipe.Error (VpipeError (..))
import Vpipe.Pipeline.Internal (PipelineInterface (..), PushConstantRange (..), ResolvedBindingPlan (..), ResolvedStorageBuffer (..), ResolvedTexture (..), ResolvedUniformBuffer (..), ResourceBinding (..), ResourceKind (..), RuntimeHandle (..), resourceBindingKind)
import Vpipe.Pipeline.Resource.Internal (BufferBindingMetadata (..), ImageBindingMetadata (..), RuntimeObjectKind (..), runtimeBufferMetadata, runtimeHandleKind, runtimeHandleLease, runtimeHandleOwner, runtimeHandleWord, runtimeImageMetadata)

data DescriptorStats = DescriptorStats
  { descriptorCacheHits :: Int
  , descriptorCacheMisses :: Int
  , descriptorWrites :: Int
  }
  deriving (Eq, Show)

data FrameState = FrameState
  { frameCache :: [(DescriptorCacheKey, Descriptor.DescriptorSet)]
  , framePools :: [PoolChunk]
  , frameResourceLeases :: [(RuntimeHandle, IO ())]
  , frameStats :: DescriptorStats
  , frameHealth :: FrameHealth
  }

data FrameHealth = FrameHealthy | FramePoisoned
  deriving (Eq)

data PoolChunk = PoolChunk
  { poolChunkHandle :: Descriptor.DescriptorPool
  , poolChunkCapacity :: Int
  , poolChunkAllocated :: Int
  }

data PoolSelection = PoolSelection
  { selectedPool :: PoolChunk
  , selectedPriorPools :: [PoolChunk]
  , selectedFreshPool :: Maybe Descriptor.DescriptorPool
  }

data DescriptorRuntime = DescriptorRuntime
  { descriptorRuntimeLayout :: DescriptorLayout
  , descriptorRuntimeFrame :: DescriptorFrame
  }

data DescriptorLayout = DescriptorLayout
  { descriptorLayoutContext :: Context
  , descriptorLayoutIdentityValue :: DescriptorLayoutIdentity
  , descriptorLayoutSetLayout :: Descriptor.DescriptorSetLayout
  , descriptorLayoutPipelineLayout :: PipelineLayout.PipelineLayout
  , descriptorLayoutPoolSizes :: [Descriptor.DescriptorPoolSize]
  , descriptorLayoutExpectedBindings :: [DescriptorBindingKey]
  }

newtype DescriptorLayoutIdentity = DescriptorLayoutIdentity Unique
  deriving (Eq)

data DescriptorFrame = DescriptorFrame
  { descriptorFrameLayout :: DescriptorLayout
  , descriptorFrameState :: MVar FrameState
  }

data DescriptorCacheKey
  = DescriptorCacheKey
      [BufferDescriptorCacheKey]
      [BufferDescriptorCacheKey]
      [ResolvedTexture]
  deriving (Eq)

data BufferDescriptorCacheKey = BufferDescriptorCacheKey
  { cachedBufferBindingName :: String
  , cachedBufferBindingSet :: Int
  , cachedBufferBindingIndex :: Int
  , cachedBufferHandle :: RuntimeHandle
  , cachedBufferByteOffset :: Word64
  , cachedBufferElementCount :: Int
  , cachedBufferStride :: Int
  }
  deriving (Eq)

data DescriptorBindingKey = DescriptorBindingKey Int Int ResourceKind
  deriving (Eq, Ord, Show)

newDescriptorRuntime :: Context -> PipelineInterface -> IO DescriptorRuntime
newDescriptorRuntime context interface =
  withContextLease context (newDescriptorRuntimeLeased context interface)

-- | Internal variant for a caller which already owns the Context lease.
newDescriptorRuntimeLeased :: Context -> PipelineInterface -> IO DescriptorRuntime
newDescriptorRuntimeLeased context interface = mask $ \_ -> do
  layout <- newDescriptorLayoutLeased context interface
  frame <- newDescriptorFrameLeased layout
  pure (DescriptorRuntime layout frame)

newDescriptorLayout :: Context -> PipelineInterface -> IO DescriptorLayout
newDescriptorLayout context interface =
  withContextLease context (newDescriptorLayoutLeased context interface)

-- | Internal variant for a caller which already owns the Context lease.
newDescriptorLayoutLeased :: Context -> PipelineInterface -> IO DescriptorLayout
newDescriptorLayoutLeased context interface = mask $ \_ -> do
  identity <- DescriptorLayoutIdentity <$> newUnique
  let device = contextDevice context
      bindings = Vector.fromList (fmap descriptorBinding (pipelineResources interface))
      layoutInfo = (zero :: Descriptor.DescriptorSetLayoutCreateInfo '[]){Descriptor.bindings = bindings}
  setLayout <- mapVulkan "vkCreateDescriptorSetLayout" (Descriptor.createDescriptorSetLayout device layoutInfo Nothing)
  let destroyLayout = Descriptor.destroyDescriptorSetLayout device setLayout Nothing
      sizesPerSet = poolSizes (pipelineResources interface) 1
  setObjectNameLeased context ObjectType.OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT (descriptorSetLayoutHandleWord setLayout) (derivedObjectName "descriptor-set-layout" (descriptorSetLayoutHandleWord setLayout))
    `onException` destroyLayout
  let pipelineInfo =
        (zero :: PipelineLayout.PipelineLayoutCreateInfo)
          { PipelineLayout.setLayouts = Vector.singleton setLayout
          , PipelineLayout.pushConstantRanges = Vector.fromList (fmap pushConstantRange (pipelinePushConstants interface))
          }
  layout <- mapVulkan "vkCreatePipelineLayout" (PipelineLayout.createPipelineLayout device pipelineInfo Nothing) `onException` destroyLayout
  setObjectNameLeased context ObjectType.OBJECT_TYPE_PIPELINE_LAYOUT (pipelineLayoutHandleWord layout) (derivedObjectName "pipeline-layout" (pipelineLayoutHandleWord layout))
    `onException` (PipelineLayout.destroyPipelineLayout device layout Nothing >> destroyLayout)
  let descriptorLayout = DescriptorLayout context identity setLayout layout sizesPerSet (fmap resourceBindingKey (pipelineResources interface))
      cleanup = destroyDescriptorLayout descriptorLayout
  registerContextFinalizerLeased context cleanup `onException` cleanup
  pure descriptorLayout

newDescriptorFrame :: DescriptorLayout -> IO DescriptorFrame
newDescriptorFrame layout =
  withContextLease (descriptorLayoutContext layout) (newDescriptorFrameLeased layout)

-- | Internal variant for a caller which already owns the layout Context lease.
newDescriptorFrameLeased :: DescriptorLayout -> IO DescriptorFrame
newDescriptorFrameLeased layout = mask $ \_ -> do
  pool <- createPool layout 64
  state <- newMVar (FrameState [] [PoolChunk pool 64 0] [] (DescriptorStats 0 0 0) FrameHealthy) `onException` destroyPool layout pool
  let frame = DescriptorFrame layout state
      cleanup = destroyDescriptorFrame frame
  registerContextFinalizerLeased (descriptorLayoutContext layout) cleanup `onException` cleanup
  pure frame

{- | Invalidates the per-frame cache and resets the backing pool.  The caller
must establish GPU completion before calling this operation.
-}
beginDescriptorFrame :: DescriptorRuntime -> IO ()
beginDescriptorFrame runtime =
  withContextLease (descriptorLayoutContext (descriptorRuntimeLayout runtime)) (beginDescriptorFrameLeased runtime)

-- | Internal variant for a caller which already owns the Context lease.
beginDescriptorFrameLeased :: DescriptorRuntime -> IO ()
beginDescriptorFrameLeased runtime = resetDescriptorFrameLeased (descriptorRuntimeFrame runtime)

resetDescriptorFrame :: DescriptorFrame -> IO ()
resetDescriptorFrame frame =
  withContextLease (descriptorLayoutContext (descriptorFrameLayout frame)) (resetDescriptorFrameLeased frame)

-- | The caller must establish GPU completion before resetting this frame.
resetDescriptorFrameLeased :: DescriptorFrame -> IO ()
resetDescriptorFrameLeased frame = mask $ \_ -> do
  failures <-
    modifyMVarMasked (descriptorFrameState frame) $ \state -> do
      let layout = descriptorFrameLayout frame
          reset chunk = mapVulkan "vkResetDescriptorPool" (Descriptor.resetDescriptorPool (contextDevice (descriptorLayoutContext layout)) (poolChunkHandle chunk) zero)
      resetFailures <- collectCleanupFailures (fmap reset (framePools state))
      leaseFailures <- collectCleanupFailures (fmap snd (frameResourceLeases state))
      let failures' = resetFailures <> leaseFailures
          resetSucceeded = null resetFailures
          pools =
            if resetSucceeded
              then fmap (\chunk -> chunk{poolChunkAllocated = 0}) (framePools state)
              else framePools state
          health = if null failures' then FrameHealthy else FramePoisoned
          state' = state{frameCache = [], framePools = pools, frameResourceLeases = [], frameHealth = health}
      pure (state', failures')
  case failures of
    [] -> pure ()
    primary : _ -> throwIO primary

resolveDescriptors :: DescriptorRuntime -> ResolvedBindingPlan -> IO Descriptor.DescriptorSet
resolveDescriptors runtime resolved =
  withContextLease (descriptorLayoutContext (descriptorRuntimeLayout runtime)) (resolveDescriptorsLeased runtime resolved)

-- | Internal variant for a caller which already owns the Context lease.
resolveDescriptorsLeased :: DescriptorRuntime -> ResolvedBindingPlan -> IO Descriptor.DescriptorSet
resolveDescriptorsLeased runtime =
  resolveDescriptorFrameLeased (descriptorRuntimeLayout runtime) (descriptorRuntimeFrame runtime)

resolveDescriptorFrame :: DescriptorLayout -> DescriptorFrame -> ResolvedBindingPlan -> IO Descriptor.DescriptorSet
resolveDescriptorFrame layout frame resolved =
  withContextLease (descriptorLayoutContext layout) (resolveDescriptorFrameLeased layout frame resolved)

-- | Resolve a descriptor set and retain its resources until the frame is reset.
resolveDescriptorFrameLeased :: DescriptorLayout -> DescriptorFrame -> ResolvedBindingPlan -> IO Descriptor.DescriptorSet
resolveDescriptorFrameLeased = resolveDescriptorFrameWithLeases True

resolveDescriptorFrameForRecording :: DescriptorLayout -> DescriptorFrame -> ResolvedBindingPlan -> IO Descriptor.DescriptorSet
resolveDescriptorFrameForRecording layout frame resolved =
  withContextLease (descriptorLayoutContext layout) (resolveDescriptorFrameForRecordingLeased layout frame resolved)

{- | Resolve for a higher-level frame recorder which owns resource leases itself.
This validates and writes descriptors, but retains no RuntimeHandle leases. The
caller must centrally retain every RuntimeHandle in the resolved plan until GPU
completion for the recorded frame.
-}
resolveDescriptorFrameForRecordingLeased :: DescriptorLayout -> DescriptorFrame -> ResolvedBindingPlan -> IO Descriptor.DescriptorSet
resolveDescriptorFrameForRecordingLeased layout frame resolved
  | not (descriptorLayoutsMatch layout (descriptorFrameLayout frame)) = throwDescriptorFrameLayoutMismatch layout frame
  | otherwise = mask $ \restore -> do
      state <- readMVar (descriptorFrameState frame)
      ensureDescriptorFrameHealthy state
      validateResolvedBindings layout resolved
      (temporary, _) <- acquireDescriptorLeases [] (descriptorRuntimeHandles resolved)
      outcome <- try (restore (resolveDescriptorFrameWithLeases False layout frame resolved))
      case outcome of
        Left (primary :: SomeException) -> do
          _ <- collectCleanupFailures (fmap snd temporary)
          throwIO primary
        Right set -> do
          runCleanupActions "descriptor recording lease release" (fmap snd temporary)
          pure set

resolveDescriptorFrameWithLeases :: Bool -> DescriptorLayout -> DescriptorFrame -> ResolvedBindingPlan -> IO Descriptor.DescriptorSet
resolveDescriptorFrameWithLeases retainLeases layout frame resolved
  | not (descriptorLayoutsMatch layout (descriptorFrameLayout frame)) = throwDescriptorFrameLayoutMismatch layout frame
  | otherwise = do
      outcome <- modifyMVarMasked (descriptorFrameState frame) $ \state -> do
        ensureDescriptorFrameHealthy state
        validateResolvedBindings layout resolved
        let key = descriptorCacheKey resolved
            layoutContext = descriptorLayoutContext layout
        (newLeases, retainedLeases) <-
          if retainLeases
            then acquireDescriptorLeases (frameResourceLeases state) (descriptorRuntimeHandles resolved)
            else pure ([], frameResourceLeases state)
        case lookup key (frameCache state) of
          Just set ->
            let stats = frameStats state
             in pure (state{frameResourceLeases = retainedLeases, frameStats = stats{descriptorCacheHits = descriptorCacheHits stats + 1}}, Right set)
          Nothing ->
            do
              selection <- selectPool layout (framePools state)
              let pool = selectedPool selection
                  pools = selectedPriorPools selection
                  cleanup =
                    fmap snd newLeases
                      <> maybe [] (pure . destroyPool layout) (selectedFreshPool selection)
                  trackFailedAllocation count primary =
                    let usedPool = pool{poolChunkAllocated = poolChunkAllocated pool + count}
                        updatedPools = usedPool : filter ((/= poolChunkHandle pool) . poolChunkHandle) pools
                        poisonedState = state{framePools = updatedPools, frameResourceLeases = retainedLeases, frameHealth = FramePoisoned}
                     in pure (poisonedState, Left primary)
              preservingPrimaryException cleanup $ do
                sets <-
                  mapVulkan
                    "vkAllocateDescriptorSets"
                    ( Descriptor.allocateDescriptorSets
                        (contextDevice layoutContext)
                        ( Descriptor.DescriptorSetAllocateInfo zero (poolChunkHandle pool) (Vector.singleton (descriptorLayoutSetLayout layout)) ::
                            Descriptor.DescriptorSetAllocateInfo '[]
                        )
                    )
                case Vector.toList sets of
                  [set] -> do
                    let setHandle = descriptorSetHandleWord set
                        setCategory =
                          "descriptor-set-pool-"
                            <> showHex (descriptorPoolHandleWord (poolChunkHandle pool)) ""
                            <> "-slot-"
                            <> show (poolChunkAllocated pool)
                        freeSet =
                          mapVulkan
                            "vkFreeDescriptorSets"
                            (Descriptor.freeDescriptorSets (contextDevice layoutContext) (poolChunkHandle pool) (Vector.singleton set))
                    updateResult <- try $ do
                      setObjectNameLeased layoutContext ObjectType.OBJECT_TYPE_DESCRIPTOR_SET setHandle (derivedObjectName setCategory setHandle)
                      writes <- descriptorWritesFor set resolved
                      let stats = frameStats state
                          updated = stats{descriptorCacheMisses = descriptorCacheMisses stats + 1, descriptorWrites = descriptorWrites stats + if Vector.null writes then 0 else 1}
                      mapVulkan "vkUpdateDescriptorSets" (Descriptor.updateDescriptorSets (contextDevice layoutContext) writes Vector.empty)
                      pure updated
                    case updateResult of
                      Right updated -> do
                        let usedPool = pool{poolChunkAllocated = poolChunkAllocated pool + 1}
                            updatedPools = usedPool : filter ((/= poolChunkHandle pool) . poolChunkHandle) pools
                        pure (state{frameCache = (key, set) : frameCache state, framePools = updatedPools, frameResourceLeases = retainedLeases, frameStats = updated}, Right set)
                      Left (primary :: SomeException) -> do
                        freeResult <- try freeSet
                        case freeResult of
                          Right () -> throwIO primary
                          Left (_ :: SomeException) -> trackFailedAllocation 1 primary
                  values -> do
                    let primary = toException (VulkanFailure "descriptor resolution" ("expected one allocated descriptor set, received " <> show (length values)))
                    freeResult <-
                      try $
                        mapVulkan
                          "vkFreeDescriptorSets"
                          (Descriptor.freeDescriptorSets (contextDevice layoutContext) (poolChunkHandle pool) (Vector.fromList values))
                    case freeResult of
                      Right () -> throwIO primary
                      Left (_ :: SomeException) -> trackFailedAllocation (length values) primary
      either throwIO pure outcome

ensureDescriptorFrameHealthy :: FrameState -> IO ()
ensureDescriptorFrameHealthy state =
  unless (frameHealth state == FrameHealthy) $
    throwIO (VulkanFailure "descriptor resolution" "descriptor frame is poisoned; reset it after GPU completion before resolving descriptors")

descriptorStats :: DescriptorRuntime -> IO DescriptorStats
descriptorStats = fmap frameStats . readMVar . descriptorFrameState . descriptorRuntimeFrame

descriptorFrameStats :: DescriptorFrame -> IO DescriptorStats
descriptorFrameStats = fmap frameStats . readMVar . descriptorFrameState

descriptorPipelineLayoutHandle :: DescriptorRuntime -> PipelineLayout.PipelineLayout
descriptorPipelineLayoutHandle = descriptorLayoutPipelineLayout . descriptorRuntimeLayout

descriptorSetLayoutHandle :: DescriptorRuntime -> Descriptor.DescriptorSetLayout
descriptorSetLayoutHandle = descriptorLayoutSetLayout . descriptorRuntimeLayout

descriptorLayoutPipelineLayoutHandle :: DescriptorLayout -> PipelineLayout.PipelineLayout
descriptorLayoutPipelineLayoutHandle = descriptorLayoutPipelineLayout

descriptorLayoutSetLayoutHandle :: DescriptorLayout -> Descriptor.DescriptorSetLayout
descriptorLayoutSetLayoutHandle = descriptorLayoutSetLayout

descriptorLayoutIdentity :: DescriptorLayout -> DescriptorLayoutIdentity
descriptorLayoutIdentity = descriptorLayoutIdentityValue

descriptorRuntimeLayoutValue :: DescriptorRuntime -> DescriptorLayout
descriptorRuntimeLayoutValue = descriptorRuntimeLayout

descriptorLayoutsMatch :: DescriptorLayout -> DescriptorLayout -> Bool
descriptorLayoutsMatch requested frame =
  descriptorLayoutIdentity requested == descriptorLayoutIdentity frame

throwDescriptorFrameLayoutMismatch :: DescriptorLayout -> DescriptorFrame -> IO a
throwDescriptorFrameLayoutMismatch requested frame =
  throwIO
    ( VulkanFailure
        "descriptor resolution"
        ( "descriptor frame "
            <> describeDescriptorLayout (descriptorFrameLayout frame)
            <> " does not match requested "
            <> describeDescriptorLayout requested
        )
    )

describeDescriptorLayout :: DescriptorLayout -> String
describeDescriptorLayout layout =
  "layout identity "
    <> showDescriptorLayoutIdentity (descriptorLayoutIdentity layout)
    <> " with VkDescriptorSetLayout handle 0x"
    <> showHex (descriptorSetLayoutHandleWord (descriptorLayoutSetLayout layout)) ""

showDescriptorLayoutIdentity :: DescriptorLayoutIdentity -> String
showDescriptorLayoutIdentity (DescriptorLayoutIdentity identity) = show (hashUnique identity)

descriptorCacheKey :: ResolvedBindingPlan -> DescriptorCacheKey
descriptorCacheKey resolved =
  DescriptorCacheKey
    (fmap uniformCacheKey (resolvedUniformBuffers resolved))
    (fmap storageCacheKey (resolvedStorageBuffers resolved))
    (resolvedTextures resolved)
 where
  uniformCacheKey (ResolvedUniformBuffer name set binding handle) = bufferCacheKey name set binding handle
  storageCacheKey (ResolvedStorageBuffer name set binding handle) = bufferCacheKey name set binding handle
  bufferCacheKey name set binding handle =
    case runtimeBufferMetadata handle of
      Just metadata ->
        BufferDescriptorCacheKey
          { cachedBufferBindingName = name
          , cachedBufferBindingSet = set
          , cachedBufferBindingIndex = binding
          , cachedBufferHandle = handle
          , cachedBufferByteOffset = bufferBindingByteOffset metadata
          , cachedBufferElementCount = bufferBindingElementCount metadata
          , cachedBufferStride = bufferBindingStride metadata
          }
      Nothing ->
        BufferDescriptorCacheKey
          { cachedBufferBindingName = name
          , cachedBufferBindingSet = set
          , cachedBufferBindingIndex = binding
          , cachedBufferHandle = handle
          , cachedBufferByteOffset = 0
          , cachedBufferElementCount = 0
          , cachedBufferStride = 0
          }

validateResolvedBindings :: DescriptorLayout -> ResolvedBindingPlan -> IO ()
validateResolvedBindings layout resolved =
  if sortOn identity actual /= sortOn identity expected
    then throwIO (VulkanFailure "descriptor resolution" ("binding plan does not match its layout: expected " <> show expected <> ", received " <> show actual))
    else do
      traverse_ validateUniform (resolvedUniformBuffers resolved)
      traverse_ validateStorage (resolvedStorageBuffers resolved)
      traverse_ validateTexture (resolvedTextures resolved)
 where
  identity value = value
  expected = descriptorLayoutExpectedBindings layout
  actual =
    [DescriptorBindingKey set binding UniformResource | ResolvedUniformBuffer _ set binding _ <- resolvedUniformBuffers resolved]
      <> [DescriptorBindingKey set binding StorageResource | ResolvedStorageBuffer _ set binding _ <- resolvedStorageBuffers resolved]
      <> [DescriptorBindingKey set binding TextureResource | ResolvedTexture _ set binding _ _ <- resolvedTextures resolved]
  validateOwner handle =
    if runtimeHandleOwner handle == Just (contextIdentity (descriptorLayoutContext layout))
      then pure ()
      else throwIO (VulkanFailure "descriptor resolution" "resource handle is unmanaged or belongs to a different context")
  validateUniform (ResolvedUniformBuffer name _ _ handle) = do
    metadata <- requireBuffer name "uniform buffer" handle
    requireBufferUsage name "uniform buffer" BufferUsage.BUFFER_USAGE_UNIFORM_BUFFER_BIT metadata
    requireBufferAlignment name "uniform buffer" (contextUniformBufferOffsetAlignment context) metadata
  validateStorage (ResolvedStorageBuffer name _ _ handle) = do
    metadata <- requireBuffer name "storage buffer" handle
    requireBufferUsage name "storage buffer" BufferUsage.BUFFER_USAGE_STORAGE_BUFFER_BIT metadata
    requireBufferAlignment name "storage buffer" (contextStorageBufferOffsetAlignment context) metadata
  validateTexture (ResolvedTexture name _ _ image sampler) = do
    metadata <- requireImage name "sampled texture image" image
    unless ((ImageUsage.IMAGE_USAGE_SAMPLED_BIT .&. imageBindingUsage metadata) /= zero) $
      throwIO (VulkanFailure "descriptor resolution" ("texture " <> show name <> " sampled texture image lacks SAMPLED usage"))
    validateOwner sampler
    unless (runtimeHandleKind sampler == RuntimeObjectSampler) $
      throwIO (VulkanFailure "descriptor resolution" ("texture " <> show name <> " sampler must be a managed Sampler handle"))
  requireBuffer name role handle = do
    validateOwner handle
    unless (runtimeHandleKind handle == RuntimeObjectBuffer) $
      throwIO (VulkanFailure "descriptor resolution" (role <> " " <> show name <> " must be a managed Buffer handle"))
    maybe (throwIO (VulkanFailure "descriptor resolution" (role <> " " <> show name <> " lacks buffer metadata"))) pure (runtimeBufferMetadata handle)
  requireImage name role handle = do
    validateOwner handle
    unless (runtimeHandleKind handle == RuntimeObjectImageView) $
      throwIO (VulkanFailure "descriptor resolution" (role <> " " <> show name <> " must be a managed ImageView handle"))
    maybe (throwIO (VulkanFailure "descriptor resolution" (role <> " " <> show name <> " lacks image metadata"))) pure (runtimeImageMetadata handle)
  requireBufferUsage name role usage metadata =
    unless ((usage .&. bufferBindingUsage metadata) /= zero) $
      throwIO (VulkanFailure "descriptor resolution" (role <> " " <> show name <> " lacks required Vulkan usage " <> show usage))
  requireBufferAlignment name role deviceAlignment metadata =
    let byteOffset = bufferBindingByteOffset metadata
        requiredAlignment = max 1 deviceAlignment
     in unless (byteOffset `mod` requiredAlignment == 0) $
          throwIO
            ( VulkanFailure
                "descriptor resolution"
                ( role
                    <> " "
                    <> show name
                    <> " has byte offset "
                    <> show byteOffset
                    <> ", which is not aligned to "
                    <> show requiredAlignment
                )
            )
  context = descriptorLayoutContext layout

resourceBindingKey :: ResourceBinding -> DescriptorBindingKey
resourceBindingKey resource =
  DescriptorBindingKey
    (resourceBindingSet resource)
    (resourceBindingBinding resource)
    (resourceBindingKind resource)

descriptorWritesFor :: Descriptor.DescriptorSet -> ResolvedBindingPlan -> IO (Vector.Vector (Chain.SomeStruct Descriptor.WriteDescriptorSet))
descriptorWritesFor set resolved = do
  uniformWrites <- traverse uniformWrite (resolvedUniformBuffers resolved)
  storageWrites <- traverse storageWrite (resolvedStorageBuffers resolved)
  pure (Vector.fromList (uniformWrites <> storageWrites <> textureWrites))
 where
  textureWrites = fmap textureWrite (resolvedTextures resolved)
  uniformWrite (ResolvedUniformBuffer _ _ binding runtimeHandle) =
    descriptorWrite DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER binding runtimeHandle
  storageWrite (ResolvedStorageBuffer _ _ binding runtimeHandle) =
    descriptorWrite DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER binding runtimeHandle
  descriptorWrite descriptorType binding runtimeHandle = do
    metadata <- maybe (throwIO (VulkanFailure "descriptor resolution" "buffer binding lacks range metadata")) pure (runtimeBufferMetadata runtimeHandle)
    range <- checkedDescriptorRange metadata
    pure
      ( Chain.SomeStruct
          ( (zero :: Descriptor.WriteDescriptorSet '[])
              { Descriptor.dstSet = set
              , Descriptor.dstBinding = fromIntegral binding
              , Descriptor.descriptorCount = 1
              , Descriptor.descriptorType = descriptorType
              , Descriptor.bufferInfo =
                  Vector.singleton
                    ( Descriptor.DescriptorBufferInfo
                        (bufferBindingRawHandle metadata)
                        (bufferBindingByteOffset metadata)
                        range
                    )
              }
          )
      )
  textureWrite (ResolvedTexture _ _ binding imageHandle samplerHandle) =
    let image = runtimeHandleWord imageHandle
        sampler = runtimeHandleWord samplerHandle
     in Chain.SomeStruct ((zero :: Descriptor.WriteDescriptorSet '[]){Descriptor.dstSet = set, Descriptor.dstBinding = fromIntegral binding, Descriptor.descriptorCount = 1, Descriptor.descriptorType = DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, Descriptor.imageInfo = Vector.singleton (Descriptor.DescriptorImageInfo (Handles.Sampler sampler) (Handles.ImageView image) ImageLayout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL)})

checkedDescriptorRange :: BufferBindingMetadata -> IO Word64
checkedDescriptorRange metadata = do
  let elements = bufferBindingElementCount metadata
      stride = bufferBindingStride metadata
      bytes = toInteger elements * toInteger stride
      end = toInteger (bufferBindingByteOffset metadata) + bytes
  unless (elements > 0 && stride > 0 && bytes <= toInteger (maxBound :: Word64) && end <= toInteger (maxBound :: Word64)) $
    throwIO (VulkanFailure "descriptor resolution" "buffer descriptor range is invalid or overflows VkDeviceSize")
  pure (fromInteger bytes)

descriptorRuntimeHandles :: ResolvedBindingPlan -> [RuntimeHandle]
descriptorRuntimeHandles resolved =
  [handle | ResolvedUniformBuffer _ _ _ handle <- resolvedUniformBuffers resolved]
    <> [handle | ResolvedStorageBuffer _ _ _ handle <- resolvedStorageBuffers resolved]
    <> concat [[image, sampler] | ResolvedTexture _ _ _ image sampler <- resolvedTextures resolved]

acquireDescriptorLeases :: [(RuntimeHandle, IO ())] -> [RuntimeHandle] -> IO ([(RuntimeHandle, IO ())], [(RuntimeHandle, IO ())])
acquireDescriptorLeases = acquire []
 where
  acquire acquired leases handles = case handles of
    [] -> pure (acquired, leases)
    handle : rest
      | any ((== handle) . fst) leases -> acquire acquired leases rest
      | otherwise -> do
          acquireLease <-
            maybe
              (throwIO (VulkanFailure "descriptor resolution" ("resource handle " <> show handle <> " has no managed lifetime")))
              pure
              (runtimeHandleLease handle)
          release <- acquireLease
          preservingPrimaryException [release] $
            acquire ((handle, release) : acquired) ((handle, release) : leases) rest

descriptorBinding :: ResourceBinding -> Descriptor.DescriptorSetLayoutBinding
descriptorBinding resource =
  Descriptor.DescriptorSetLayoutBinding
    { Descriptor.binding = fromIntegral (resourceBindingBinding resource)
    , Descriptor.descriptorType = case resourceBindingKind resource of
        UniformResource -> DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER
        StorageResource -> DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER
        TextureResource -> DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
    , Descriptor.descriptorCount = 1
    , Descriptor.stageFlags = ShaderStage.SHADER_STAGE_ALL
    , Descriptor.immutableSamplers = Vector.empty
    }

poolSizes :: [ResourceBinding] -> Int -> [Descriptor.DescriptorPoolSize]
poolSizes resources capacity = fmap makeSize kinds
 where
  resourceKinds = fmap descriptorKind resources
  kinds = unique resourceKinds
  makeSize kind =
    Descriptor.DescriptorPoolSize
      kind
      (fromIntegral (capacity * length (filter (== kind) resourceKinds)))
  descriptorKind resource = case resourceBindingKind resource of
    UniformResource -> DescriptorType.DESCRIPTOR_TYPE_UNIFORM_BUFFER
    StorageResource -> DescriptorType.DESCRIPTOR_TYPE_STORAGE_BUFFER
    TextureResource -> DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  unique = foldr (\value values -> if value `elem` values then values else value : values) []

pushConstantRange :: PushConstantRange -> PipelineLayout.PushConstantRange
pushConstantRange range =
  let offset = pushConstantOffset range
      size = pushConstantSize range
   in PipelineLayout.PushConstantRange ShaderStage.SHADER_STAGE_ALL (fromIntegral offset) (fromIntegral size)

selectPool :: DescriptorLayout -> [PoolChunk] -> IO PoolSelection
selectPool layout chunks = case filter hasCapacity chunks of
  chunk : _ -> pure (PoolSelection chunk chunks Nothing)
  [] -> do
    let capacity = case chunks of
          [] -> 64
          chunk : _ -> poolChunkCapacity chunk * 2
    pool <- createPool layout capacity
    pure (PoolSelection (PoolChunk pool capacity 0) chunks (Just pool))
 where
  hasCapacity chunk = poolChunkAllocated chunk < poolChunkCapacity chunk

scalePoolSizes :: Int -> [Descriptor.DescriptorPoolSize] -> [Descriptor.DescriptorPoolSize]
scalePoolSizes capacity = fmap scale
 where
  scale (Descriptor.DescriptorPoolSize descriptorType descriptorsPerSet) =
    Descriptor.DescriptorPoolSize descriptorType (fromIntegral capacity * descriptorsPerSet)

createPool :: DescriptorLayout -> Int -> IO Descriptor.DescriptorPool
createPool layout capacity = do
  let context = descriptorLayoutContext layout
      device = contextDevice context
      sizes = scalePoolSizes capacity (descriptorLayoutPoolSizes layout)
      info =
        (zero :: Descriptor.DescriptorPoolCreateInfo '[])
          { Descriptor.flags = PoolFlags.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
          , Descriptor.maxSets = fromIntegral capacity
          , Descriptor.poolSizes = Vector.fromList sizes
          }
  pool <- mapVulkan "vkCreateDescriptorPool" (Descriptor.createDescriptorPool device info Nothing)
  setObjectNameLeased context ObjectType.OBJECT_TYPE_DESCRIPTOR_POOL (descriptorPoolHandleWord pool) (derivedObjectName "descriptor-pool" (descriptorPoolHandleWord pool))
    `onException` Descriptor.destroyDescriptorPool device pool Nothing
  pure pool

descriptorSetLayoutHandleWord :: Handles.DescriptorSetLayout -> Word64
descriptorSetLayoutHandleWord (Handles.DescriptorSetLayout handle) = handle

pipelineLayoutHandleWord :: Handles.PipelineLayout -> Word64
pipelineLayoutHandleWord (Handles.PipelineLayout handle) = handle

descriptorPoolHandleWord :: Handles.DescriptorPool -> Word64
descriptorPoolHandleWord (Handles.DescriptorPool handle) = handle

descriptorSetHandleWord :: Handles.DescriptorSet -> Word64
descriptorSetHandleWord (Handles.DescriptorSet handle) = handle

destroyDescriptorLayout :: DescriptorLayout -> IO ()
destroyDescriptorLayout layout = do
  let device = contextDevice (descriptorLayoutContext layout)
  PipelineLayout.destroyPipelineLayout device (descriptorLayoutPipelineLayout layout) Nothing
  Descriptor.destroyDescriptorSetLayout device (descriptorLayoutSetLayout layout) Nothing

destroyDescriptorFrame :: DescriptorFrame -> IO ()
destroyDescriptorFrame frame = mask $ \_ -> do
  pools <- modifyMVarMasked (descriptorFrameState frame) $ \state ->
    pure (state{framePools = [], frameResourceLeases = [], frameCache = [], frameHealth = FramePoisoned}, (framePools state, frameResourceLeases state))
  let layout = descriptorFrameLayout frame
      cleanupActions = fmap snd (snd pools) <> fmap (destroyPool layout . poolChunkHandle) (fst pools)
  runCleanupActions "descriptor frame cleanup" cleanupActions

{- | Internal variant for an owner which already holds the layout Context
lease. The Context finalizer remains idempotent after explicit teardown.
-}
destroyDescriptorFrameLeased :: DescriptorFrame -> IO ()
destroyDescriptorFrameLeased = destroyDescriptorFrame

destroyPool :: DescriptorLayout -> Descriptor.DescriptorPool -> IO ()
destroyPool layout pool = Descriptor.destroyDescriptorPool (contextDevice (descriptorLayoutContext layout)) pool Nothing

preservingPrimaryException :: [IO ()] -> IO a -> IO a
preservingPrimaryException cleanup action =
  action `catch` \(primary :: SomeException) -> do
    _ <- collectCleanupFailures cleanup
    throwIO primary

collectCleanupFailures :: [IO ()] -> IO [SomeException]
collectCleanupFailures = fmap concat . traverse collect
 where
  collect action = do
    result <- try action
    pure $ case result of
      Left failure -> [failure]
      Right () -> []

runCleanupActions :: String -> [IO ()] -> IO ()
runCleanupActions operation actions = mask $ \_ -> do
  failures <- collectCleanupFailures actions
  unless (null failures) $
    throwIO (VulkanFailure operation (intercalate "; " (fmap displayException failures)))

poisonDescriptorFrameForTest :: DescriptorFrame -> IO ()
poisonDescriptorFrameForTest frame =
  modifyMVarMasked (descriptorFrameState frame) $ \state ->
    pure (state{frameCache = [], frameHealth = FramePoisoned}, ())

runDescriptorCleanupActionsForTest :: [IO ()] -> IO ()
runDescriptorCleanupActionsForTest = runCleanupActions "descriptor cleanup test"

newDescriptorLayoutIdentityForTest :: IO DescriptorLayoutIdentity
newDescriptorLayoutIdentityForTest = DescriptorLayoutIdentity <$> newUnique

mapVulkan :: String -> IO a -> IO a
mapVulkan operation action =
  action `catch` \(error' :: Vulkan.VulkanException) ->
    if Vulkan.vulkanExceptionResult error' == Result.ERROR_DEVICE_LOST
      then throwIO DeviceLost
      else throwIO (VulkanFailure operation (show (Vulkan.vulkanExceptionResult error')))
