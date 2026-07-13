{-# LANGUAGE DataKinds #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Graphics lowering into ordered, submission-free frame commands.
module Vpipe.Graphics.Frame.Internal (
  GraphicsRuntime (..),
  PreparedGraphicsPipeline (..),
  RenderHealth (..),
  preparedGraphicsContext,
  preparedGraphicsDescriptorLayout,
  prepareGraphicsFrameCommandsLeased,
) where

import Control.Concurrent.MVar (MVar, readMVar)
import Control.Exception (throwIO)
import Control.Monad (foldM, unless, when)
import Data.Bits ((.&.), (.|.))
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (traverse_)
import Data.List (find)
import Data.Maybe (maybeToList)
import Data.Vector qualified as Vector
import Data.Word (Word32, Word64)
import Foreign.Ptr (castPtr)
import Vulkan.Core10.CommandBufferBuilding qualified as Command
import Vulkan.Core10.Enums.AttachmentLoadOp qualified as AttachmentLoad
import Vulkan.Core10.Enums.AttachmentStoreOp qualified as AttachmentStore
import Vulkan.Core10.Enums.BufferUsageFlagBits qualified as BufferUsage
import Vulkan.Core10.Enums.ImageAspectFlagBits qualified as Aspect
import Vulkan.Core10.Enums.ImageLayout qualified as Layout
import Vulkan.Core10.Enums.ImageUsageFlagBits qualified as ImageUsage
import Vulkan.Core10.Enums.IndexType qualified as IndexType
import Vulkan.Core10.Enums.PipelineBindPoint qualified as PipelineBindPoint
import Vulkan.Core10.Enums.SampleCountFlagBits qualified as Samples
import Vulkan.Core10.Enums.ShaderStageFlagBits qualified as ShaderStage
import Vulkan.Core10.FundamentalTypes qualified as Fundamental
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Core10.Pipeline qualified as VkPipeline
import Vulkan.Core13.Enums.AccessFlags2 qualified as Access2
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering qualified as Rendering
import Vulkan.Zero (zero)

import Vpipe.Context.Internal (Context, contextIdentity)
import Vpipe.Descriptor.Internal (DescriptorFrame, DescriptorLayout, DescriptorRuntime, descriptorLayoutPipelineLayoutHandle, descriptorRuntimeLayoutValue, resolveDescriptorFrameForRecordingLeased)
import Vpipe.Diagnostics.Dump.Internal (ShaderDump (..), ShaderDumpStage (DumpFragment, DumpVertex))
import Vpipe.Error (VpipeError (VulkanFailure))
import Vpipe.Frame.Resource.Internal (FrameBufferUse (..), FrameCommand, FrameImageUse (..), newFrameCommand)
import Vpipe.Graphics.Cache.Internal (GraphicsCache)
import Vpipe.Graphics.Compile.Internal (LoweredColorOutput (..), LoweredDepthOutput (..), LoweredDraw (..))
import Vpipe.Graphics.Pipeline.Internal (PipelineDescription (..), acquireGraphicsPipelineLeased)
import Vpipe.Image.State qualified as ImageState
import Vpipe.Pipeline.Internal qualified as Source
import Vpipe.Pipeline.Resource.Internal qualified as Resource
import Vpipe.SpirV.Assembler (moduleBytes)

data GraphicsRuntime = GraphicsRuntime
  { runtimeContext :: Context
  , runtimeCache :: GraphicsCache
  }

data PreparedGraphicsPipeline env = PreparedGraphicsPipeline
  { preparedRuntime :: GraphicsRuntime
  , preparedCompiled :: Source.CompiledPipeline env
  , preparedLowered :: [LoweredDraw]
  , preparedDescriptors :: DescriptorRuntime
  , preparedRenderLock :: MVar ()
  , preparedRenderHealth :: MVar RenderHealth
  }

data RenderHealth = RenderHealthy | RenderPoisoned
  deriving stock (Eq)

data BufferIntent = BufferIntent
  { bufferIntentHandle :: Source.RuntimeHandle
  , bufferIntentMetadata :: Resource.BufferBindingMetadata
  , bufferIntentStage :: Stage2.PipelineStageFlags2
  , bufferIntentAccess :: Access2.AccessFlags2
  }

data ImageRole = SampledImageRole | ColorImageRole | DepthImageRole
  deriving stock (Eq, Show)

data ImageIntent = ImageIntent
  { imageIntentHandle :: Source.RuntimeHandle
  , imageIntentMetadata :: Resource.ImageBindingMetadata
  , imageIntentStage :: Stage2.PipelineStageFlags2
  , imageIntentAccess :: Access2.AccessFlags2
  , imageIntentLayout :: Layout.ImageLayout
  , imageIntentRole :: ImageRole
  }

newtype VertexBinding = VertexBinding
  { vertexBindingIntent :: BufferIntent
  }

data DrawPlan = DrawPlan
  { drawPlanLowered :: LoweredDraw
  , drawPlanVertexBindings :: [VertexBinding]
  , drawPlanIndexBinding :: Maybe BufferIntent
  , drawPlanBufferIntents :: [BufferIntent]
  , drawPlanImageIntents :: [ImageIntent]
  , drawPlanColorAttachments :: [ImageIntent]
  , drawPlanDepthAttachment :: Maybe ImageIntent
  , drawPlanExtent :: Fundamental.Extent2D
  , drawPlanSamples :: Samples.SampleCountFlagBits
  , drawPlanVertexCount :: Word32
  , drawPlanIndexCount :: Maybe Word32
  }

preparedGraphicsContext :: PreparedGraphicsPipeline env -> Context
preparedGraphicsContext = runtimeContext . preparedRuntime

preparedGraphicsDescriptorLayout :: PreparedGraphicsPipeline env -> DescriptorLayout
preparedGraphicsDescriptorLayout = descriptorRuntimeLayoutValue . preparedDescriptors

prepareGraphicsFrameCommandsLeased :: PreparedGraphicsPipeline env -> DescriptorFrame -> env -> IO [FrameCommand]
prepareGraphicsFrameCommandsLeased prepared descriptorFrame environment = do
  ensureHealthy prepared
  resolved <-
    either
      (graphicsFailure "graphics binding resolution" . show)
      pure
      (Source.resolvePipelineBindings compiled environment)
  pushes <- Source.resolvePipelinePushConstants compiled environment
  validatePushConstants interface pushes
  plans <- traverse (resolveDrawPlan context resolved) (preparedLowered prepared)
  descriptorSet <-
    resolveDescriptorFrameForRecordingLeased
      descriptorLayout
      descriptorFrame
      resolved
  traverse (prepareDrawCommand resolved pushes descriptorSet) plans
 where
  runtime = preparedRuntime prepared
  context = runtimeContext runtime
  compiled = preparedCompiled prepared
  interface = Source.compiledPipelineInterface compiled
  descriptorLayout = preparedGraphicsDescriptorLayout prepared
  layout = descriptorLayoutPipelineLayoutHandle descriptorLayout
  prepareDrawCommand resolved pushes descriptorSet plan = do
    let lowered = drawPlanLowered plan
        dumpName = "graphics-draw-" <> show (loweredDrawIdentifier lowered)
        description =
          PipelineDescription
            { pipelineVertexShaderBytes = LazyByteString.toStrict (moduleBytes (loweredVertexModule lowered))
            , pipelineFragmentShaderBytes = LazyByteString.toStrict (moduleBytes (loweredFragmentModule lowered))
            , pipelineVertexShaderDump = ShaderDump dumpName DumpVertex (loweredVertexModule lowered) (Source.renderPipelineInterfaceTable interface)
            , pipelineFragmentShaderDump = ShaderDump dumpName DumpFragment (loweredFragmentModule lowered) (Source.renderPipelineInterfaceTable interface)
            , pipelinePrimitiveTopology = loweredDrawTopology lowered
            , pipelineRaster = loweredDrawRaster lowered
            , pipelineVertexBindings = loweredVertexBindings lowered
            , pipelineVertexAttributes = loweredVertexAttributes lowered
            , pipelineColorOutputs = [(loweredColorFormat output, loweredColorBlend output) | output <- loweredColorOutputs lowered]
            , pipelineDepthOutput = fmap (\output -> (loweredDepthFormat output, loweredDepthState output)) (loweredDepthOutput lowered)
            , pipelineSampleCount = drawPlanSamples plan
            , pipelineLayoutStructure = pipelineLayoutKey interface
            }
    pipeline <- acquireGraphicsPipelineLeased context (runtimeCache runtime) layout description
    buffers <- traverse toFrameBufferUse (drawPlanBufferIntents plan)
    let images = fmap toFrameImageUse (drawPlanImageIntents plan)
        renderedTargets =
          fmap imageIntentHandle (drawPlanColorAttachments plan)
            <> fmap imageIntentHandle (maybeToList (drawPlanDepthAttachment plan))
    newFrameCommand
      (resolvedHandles resolved)
      buffers
      images
      renderedTargets
      (recordDrawBody plan pipeline layout descriptorSet pushes)

ensureHealthy :: PreparedGraphicsPipeline env -> IO ()
ensureHealthy prepared = do
  health <- readMVar (preparedRenderHealth prepared)
  when (health == RenderPoisoned) $
    graphicsFailure "graphics frame preparation" "this prepared pipeline has an unretired submission; destroy its Context"

resolvedHandles :: Source.ResolvedBindingPlan -> [Source.RuntimeHandle]
resolvedHandles resolved =
  [handle | Source.ResolvedVertexBuffer _ _ handle <- Source.resolvedVertexBuffers resolved]
    <> [handle | Source.ResolvedIndexBuffer _ handle <- Source.resolvedIndexBuffers resolved]
    <> [handle | Source.ResolvedUniformBuffer _ _ _ handle <- Source.resolvedUniformBuffers resolved]
    <> [handle | Source.ResolvedStorageBuffer _ _ _ handle <- Source.resolvedStorageBuffers resolved]
    <> concat [[image, sampler] | Source.ResolvedTexture _ _ _ image sampler <- Source.resolvedTextures resolved]
    <> [handle | Source.ResolvedColorImage _ _ _ handle <- Source.resolvedColorImages resolved]
    <> [handle | Source.ResolvedDepthImage _ handle <- Source.resolvedDepthImages resolved]

validatePushConstants :: Source.PipelineInterface -> [Source.ResolvedPushConstant] -> IO ()
validatePushConstants interface resolved = do
  let expected = Source.pipelinePushConstants interface
  unless (length expected == length resolved) $
    graphicsFailure "graphics push constants" "resolved push-constant count does not match the pipeline layout"
  traverse_ validate (zip expected resolved)
 where
  validate (range, value) = do
    unless (Source.pushConstantName range == Source.resolvedPushConstantName value) $
      graphicsFailure "graphics push constants" "resolved push-constant names are out of order"
    unless (Source.pushConstantOffset range == Source.resolvedPushConstantOffset value) $
      graphicsFailure "graphics push constants" "resolved push-constant offsets do not match the pipeline layout"
    unless (Source.pushConstantSize range == ByteString.length (Source.resolvedPushConstantBytes value)) $
      graphicsFailure "graphics push constants" "resolved push-constant byte size does not match the pipeline layout"

resolveDrawPlan :: Context -> Source.ResolvedBindingPlan -> LoweredDraw -> IO DrawPlan
resolveDrawPlan context resolved lowered = do
  vertices <- traverse (resolveVertexBinding context resolved) (loweredVertexBindings lowered)
  when (null vertices) $
    graphicsFailure "graphics draw validation" "a draw has no active vertex source and therefore no vertex count"
  vertexCount <- commonVertexCount vertices
  indexBinding <- traverse (resolveIndexBinding context resolved) (loweredDrawIndexSource lowered)
  indexCount <- traverse (indexBindingCount . snd) indexBinding
  uniforms <- traverse (uniformIntent context) (Source.resolvedUniformBuffers resolved)
  storages <- traverse (storageIntent context) (Source.resolvedStorageBuffers resolved)
  textures <- traverse (textureIntent context) (Source.resolvedTextures resolved)
  colors <- traverse (colorIntent context resolved lowered) (zip [0 ..] (loweredColorOutputs lowered))
  depth <- traverse (depthIntent context resolved) (loweredDepthOutput lowered)
  validateAttachmentAliases colors depth
  validateSampledAttachmentAliases textures colors depth
  (extent, samples) <- attachmentShape colors depth
  buffers <- foldM mergeBufferIntent [] (fmap vertexBindingIntent vertices <> maybeToList (snd <$> indexBinding) <> uniforms <> storages)
  images <- foldM mergeImageIntent [] (textures <> colors <> maybeToList depth)
  pure
    DrawPlan
      { drawPlanLowered = lowered
      , drawPlanVertexBindings = vertices
      , drawPlanIndexBinding = fmap snd indexBinding
      , drawPlanBufferIntents = buffers
      , drawPlanImageIntents = images
      , drawPlanColorAttachments = colors
      , drawPlanDepthAttachment = depth
      , drawPlanExtent = extent
      , drawPlanSamples = samples
      , drawPlanVertexCount = vertexCount
      , drawPlanIndexCount = indexCount
      }

resolveIndexBinding :: Context -> Source.ResolvedBindingPlan -> String -> IO (String, BufferIntent)
resolveIndexBinding context resolved sourceName = do
  Source.ResolvedIndexBuffer _ handle <-
    maybe
      (graphicsFailure "graphics index validation" ("missing index source " <> show sourceName))
      pure
      (find matches (Source.resolvedIndexBuffers resolved))
  metadata <- requireBufferMetadata context "index source" handle
  requireBufferUsage "index source" BufferUsage.BUFFER_USAGE_INDEX_BUFFER_BIT metadata
  unless (Resource.bufferBindingStride metadata == 4) $
    graphicsFailure "graphics index validation" "Word32 index buffers must have stride 4"
  unless (Resource.bufferBindingByteOffset metadata `mod` 4 == 0) $
    graphicsFailure "graphics index validation" "Word32 index buffer offset must be aligned to 4 bytes"
  pure (sourceName, BufferIntent handle metadata Stage2.PIPELINE_STAGE_2_INDEX_INPUT_BIT Access2.ACCESS_2_INDEX_READ_BIT)
 where
  matches (Source.ResolvedIndexBuffer name _) = name == sourceName

indexBindingCount :: BufferIntent -> IO Word32
indexBindingCount intent = do
  let count = Resource.bufferBindingElementCount (bufferIntentMetadata intent)
  when (count <= 0 || toInteger count > toInteger (maxBound :: Word32)) $
    graphicsFailure "graphics index validation" ("index count is outside Vulkan's Word32 range: " <> show count)
  pure (fromIntegral count)

resolveVertexBinding :: Context -> Source.ResolvedBindingPlan -> Source.VertexBindingLayout -> IO VertexBinding
resolveVertexBinding context resolved binding = do
  Source.ResolvedVertexBuffer _ _ handle <-
    maybe
      (graphicsFailure "graphics vertex validation" ("missing vertex source " <> show sourceName))
      pure
      (find matches (Source.resolvedVertexBuffers resolved))
  metadata <- requireBufferMetadata context "vertex source" handle
  requireBufferUsage "vertex source" BufferUsage.BUFFER_USAGE_VERTEX_BUFFER_BIT metadata
  unless (Resource.bufferBindingStride metadata == Source.vertexBindingStride binding) $
    graphicsFailure "graphics vertex validation" ("vertex stride mismatch for " <> show sourceName)
  pure
    ( VertexBinding
        ( BufferIntent
            handle
            metadata
            Stage2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT
            Access2.ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT
        )
    )
 where
  sourceName = Source.vertexBindingSourceName binding
  matches (Source.ResolvedVertexBuffer name _ _) = name == sourceName

commonVertexCount :: [VertexBinding] -> IO Word32
commonVertexCount bindings = case fmap (Resource.bufferBindingElementCount . bufferIntentMetadata . vertexBindingIntent) bindings of
  [] -> graphicsFailure "graphics vertex validation" "no vertex buffers were resolved"
  first : rest -> do
    unless (all (== first) rest) $
      graphicsFailure "graphics vertex validation" "active vertex sources have different element counts"
    when (first <= 0 || toInteger first > toInteger (maxBound :: Word32)) $
      graphicsFailure "graphics vertex validation" ("vertex count is outside Vulkan's Word32 range: " <> show first)
    pure (fromIntegral first)

uniformIntent :: Context -> Source.ResolvedUniformBuffer -> IO BufferIntent
uniformIntent context (Source.ResolvedUniformBuffer _ _ _ handle) = do
  metadata <- requireBufferMetadata context "uniform buffer" handle
  requireBufferUsage "uniform buffer" BufferUsage.BUFFER_USAGE_UNIFORM_BUFFER_BIT metadata
  pure (BufferIntent handle metadata shaderStages Access2.ACCESS_2_UNIFORM_READ_BIT)

storageIntent :: Context -> Source.ResolvedStorageBuffer -> IO BufferIntent
storageIntent context (Source.ResolvedStorageBuffer _ _ _ handle) = do
  metadata <- requireBufferMetadata context "storage buffer" handle
  requireBufferUsage "storage buffer" BufferUsage.BUFFER_USAGE_STORAGE_BUFFER_BIT metadata
  pure (BufferIntent handle metadata shaderStages (Access2.ACCESS_2_SHADER_READ_BIT .|. Access2.ACCESS_2_SHADER_WRITE_BIT))

textureIntent :: Context -> Source.ResolvedTexture -> IO ImageIntent
textureIntent context (Source.ResolvedTexture _ _ _ imageHandle _samplerHandle) = do
  metadata <- requireImageMetadata context "sampled texture" imageHandle
  requireImageUsage "sampled texture" ImageUsage.IMAGE_USAGE_SAMPLED_BIT metadata
  pure (ImageIntent imageHandle metadata shaderStages Access2.ACCESS_2_SHADER_SAMPLED_READ_BIT Layout.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL SampledImageRole)

colorIntent :: Context -> Source.ResolvedBindingPlan -> LoweredDraw -> (Int, LoweredColorOutput) -> IO ImageIntent
colorIntent context resolved lowered (location, output) = do
  let name = loweredColorTargetName output
  Source.ResolvedColorImage _ _ _ handle <-
    maybe
      (graphicsFailure "graphics attachment validation" ("missing color target " <> show name))
      pure
      (find matches (Source.resolvedColorImages resolved))
  metadata <- requireImageMetadata context "color target" handle
  requireImageUsage "color target" ImageUsage.IMAGE_USAGE_COLOR_ATTACHMENT_BIT metadata
  requireSingleAttachmentSubresource "color target" metadata
  unless (Resource.imageBindingFormat metadata == loweredColorFormat output) $
    graphicsFailure "graphics attachment validation" ("color format mismatch for " <> show name)
  unless ((Resource.imageBindingAspect metadata .&. Aspect.IMAGE_ASPECT_COLOR_BIT) /= zero) $
    graphicsFailure "graphics attachment validation" ("color target has no color aspect: " <> show name)
  pure (ImageIntent handle metadata Stage2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT (Access2.ACCESS_2_COLOR_ATTACHMENT_READ_BIT .|. Access2.ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT) Layout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL ColorImageRole)
 where
  matches (Source.ResolvedColorImage resolvedName drawIdentifier resolvedLocation _) =
    resolvedName == loweredColorTargetName output
      && drawIdentifier == loweredDrawIdentifier lowered
      && resolvedLocation == location

depthIntent :: Context -> Source.ResolvedBindingPlan -> LoweredDepthOutput -> IO ImageIntent
depthIntent context resolved output = do
  let name = loweredDepthTargetName output
  Source.ResolvedDepthImage _ handle <-
    maybe
      (graphicsFailure "graphics attachment validation" ("missing depth target " <> show name))
      pure
      (find matches (Source.resolvedDepthImages resolved))
  metadata <- requireImageMetadata context "depth target" handle
  requireImageUsage "depth target" ImageUsage.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT metadata
  requireSingleAttachmentSubresource "depth target" metadata
  unless (Resource.imageBindingFormat metadata == loweredDepthFormat output) $
    graphicsFailure "graphics attachment validation" ("depth format mismatch for " <> show name)
  unless ((Resource.imageBindingAspect metadata .&. Aspect.IMAGE_ASPECT_DEPTH_BIT) /= zero) $
    graphicsFailure "graphics attachment validation" ("depth target has no depth aspect: " <> show name)
  pure (ImageIntent handle metadata depthStages depthAccess Layout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL DepthImageRole)
 where
  matches (Source.ResolvedDepthImage resolvedName _) = resolvedName == loweredDepthTargetName output
  depthStages = Stage2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT .|. Stage2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT
  depthAccess = Access2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT .|. Access2.ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT

shaderStages :: Stage2.PipelineStageFlags2
shaderStages = Stage2.PIPELINE_STAGE_2_VERTEX_SHADER_BIT .|. Stage2.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT

requireBufferMetadata :: Context -> String -> Source.RuntimeHandle -> IO Resource.BufferBindingMetadata
requireBufferMetadata context label handle = do
  validateOwner context label handle
  unless (Resource.runtimeHandleKind handle == Resource.RuntimeObjectBuffer) $
    graphicsFailure "graphics resource validation" (label <> " must be a managed Buffer handle")
  maybe (graphicsFailure "graphics resource validation" (label <> " does not carry buffer metadata")) pure (Resource.runtimeBufferMetadata handle)

requireImageMetadata :: Context -> String -> Source.RuntimeHandle -> IO Resource.ImageBindingMetadata
requireImageMetadata context label handle = do
  validateOwner context label handle
  unless (Resource.runtimeHandleKind handle == Resource.RuntimeObjectImageView) $
    graphicsFailure "graphics resource validation" (label <> " must be a managed ImageView handle")
  maybe (graphicsFailure "graphics resource validation" (label <> " does not carry image metadata")) pure (Resource.runtimeImageMetadata handle)

validateOwner :: Context -> String -> Source.RuntimeHandle -> IO ()
validateOwner context label handle =
  unless (Resource.runtimeHandleOwner handle == Just (contextIdentity context)) $
    graphicsFailure "graphics resource validation" (label <> " is unmanaged or belongs to a different context")

requireBufferUsage :: String -> BufferUsage.BufferUsageFlagBits -> Resource.BufferBindingMetadata -> IO ()
requireBufferUsage label usage metadata =
  unless ((usage .&. Resource.bufferBindingUsage metadata) /= zero) $
    graphicsFailure "graphics resource validation" (label <> " lacks required Vulkan usage " <> show usage)

requireImageUsage :: String -> ImageUsage.ImageUsageFlagBits -> Resource.ImageBindingMetadata -> IO ()
requireImageUsage label usage metadata =
  unless ((usage .&. Resource.imageBindingUsage metadata) /= zero) $
    graphicsFailure "graphics resource validation" (label <> " lacks required Vulkan usage " <> show usage)

requireSingleAttachmentSubresource :: String -> Resource.ImageBindingMetadata -> IO ()
requireSingleAttachmentSubresource label metadata =
  unless (Resource.imageBindingMipLevels metadata == 1 && Resource.imageBindingArrayLayers metadata == 1) $
    graphicsFailure "graphics attachment validation" (label <> " must use exactly one mip level and array layer")

mergeBufferIntent :: [BufferIntent] -> BufferIntent -> IO [BufferIntent]
mergeBufferIntent intents requested = case break ((== bufferIntentHandle requested) . bufferIntentHandle) intents of
  (_, [])
    | any (sameRawBufferIntent requested) intents ->
        graphicsFailure
          "graphics resource validation"
          ( "distinct logical buffers reference the same raw buffer "
              <> show (Resource.bufferBindingRawHandle (bufferIntentMetadata requested))
          )
    | otherwise -> pure (intents <> [requested])
  (before, existing : after) -> do
    unless (sameBufferRange existing requested) $
      graphicsFailure "graphics resource validation" "one buffer handle resolved to incompatible byte ranges"
    pure (before <> [existing{bufferIntentStage = bufferIntentStage existing .|. bufferIntentStage requested, bufferIntentAccess = bufferIntentAccess existing .|. bufferIntentAccess requested}] <> after)

sameBufferRange :: BufferIntent -> BufferIntent -> Bool
sameBufferRange left right =
  Resource.bufferBindingRawHandle (bufferIntentMetadata left) == Resource.bufferBindingRawHandle (bufferIntentMetadata right)
    && Resource.bufferBindingByteOffset (bufferIntentMetadata left) == Resource.bufferBindingByteOffset (bufferIntentMetadata right)
    && bufferIntentByteSizeInteger left == bufferIntentByteSizeInteger right

sameRawBufferIntent :: BufferIntent -> BufferIntent -> Bool
sameRawBufferIntent left right =
  Resource.bufferBindingRawHandle (bufferIntentMetadata left)
    == Resource.bufferBindingRawHandle (bufferIntentMetadata right)

mergeImageIntent :: [ImageIntent] -> ImageIntent -> IO [ImageIntent]
mergeImageIntent intents requested = case break ((== imageIntentHandle requested) . imageIntentHandle) intents of
  (_, [])
    | any (sameRawImageIntent requested) intents ->
        graphicsFailure
          "graphics resource validation"
          ( "distinct logical images reference the same raw image "
              <> show (Resource.imageBindingRawHandle (imageIntentMetadata requested))
          )
    | otherwise -> pure (intents <> [requested])
  (before, existing : after)
    | imageIntentRole existing == imageIntentRole requested
        && imageIntentLayout existing == imageIntentLayout requested
        && sameImageSubresource existing requested ->
        pure (before <> [existing{imageIntentStage = imageIntentStage existing .|. imageIntentStage requested, imageIntentAccess = imageIntentAccess existing .|. imageIntentAccess requested}] <> after)
    | otherwise -> graphicsFailure "graphics resource validation" "one image is requested in incompatible roles within a draw"

sameImageSubresource :: ImageIntent -> ImageIntent -> Bool
sameImageSubresource left right =
  Resource.imageBindingRawHandle (imageIntentMetadata left) == Resource.imageBindingRawHandle (imageIntentMetadata right)
    && Resource.imageBindingMipLevel (imageIntentMetadata left) == Resource.imageBindingMipLevel (imageIntentMetadata right)
    && Resource.imageBindingArrayLayer (imageIntentMetadata left) == Resource.imageBindingArrayLayer (imageIntentMetadata right)
    && Resource.imageBindingMipLevels (imageIntentMetadata left) == Resource.imageBindingMipLevels (imageIntentMetadata right)
    && Resource.imageBindingArrayLayers (imageIntentMetadata left) == Resource.imageBindingArrayLayers (imageIntentMetadata right)

sameRawImageIntent :: ImageIntent -> ImageIntent -> Bool
sameRawImageIntent left right =
  Resource.imageBindingRawHandle (imageIntentMetadata left)
    == Resource.imageBindingRawHandle (imageIntentMetadata right)

validateAttachmentAliases :: [ImageIntent] -> Maybe ImageIntent -> IO ()
validateAttachmentAliases colors depth = do
  let attachments = colors <> maybeToList depth
  when (hasRawImageAliases attachments) $
    graphicsFailure "graphics attachment validation" "the same image cannot occupy multiple attachment slots in one draw"

validateSampledAttachmentAliases :: [ImageIntent] -> [ImageIntent] -> Maybe ImageIntent -> IO ()
validateSampledAttachmentAliases textures colors depth = do
  let attachments = colors <> maybeToList depth
  when (any (\texture -> any (sameRawImageIntent texture) attachments) textures) $
    graphicsFailure "graphics attachment validation" "sampling from an image while writing it as an attachment is not supported"

hasRawImageAliases :: [ImageIntent] -> Bool
hasRawImageAliases intents = case intents of
  [] -> False
  intent : remaining -> any (sameRawImageIntent intent) remaining || hasRawImageAliases remaining

attachmentShape :: [ImageIntent] -> Maybe ImageIntent -> IO (Fundamental.Extent2D, Samples.SampleCountFlagBits)
attachmentShape colors depth = case colors <> maybeToList depth of
  [] -> graphicsFailure "graphics attachment validation" "a draw has no resolved attachments"
  first : rest -> do
    let firstMetadata = imageIntentMetadata first
        firstExtent = Resource.imageBindingExtent firstMetadata
        firstSamples = Resource.imageBindingSamples firstMetadata
    validateD2Extent firstExtent
    traverse_ (validateCompatible firstExtent firstSamples) rest
    let Fundamental.Extent3D width height _ = firstExtent
    pure (Fundamental.Extent2D width height, firstSamples)
 where
  validateCompatible expectedExtent expectedSamples intent = do
    let metadata = imageIntentMetadata intent
        extent = Resource.imageBindingExtent metadata
    validateD2Extent extent
    unless (extent == expectedExtent) $
      graphicsFailure "graphics attachment validation" "all attachments in a draw must have identical extents"
    unless (Resource.imageBindingSamples metadata == expectedSamples) $
      graphicsFailure "graphics attachment validation" "all attachments in a draw must have identical sample counts"

validateD2Extent :: Fundamental.Extent3D -> IO ()
validateD2Extent (Fundamental.Extent3D width height depth) =
  when (width == 0 || height == 0 || depth /= 1) $
    graphicsFailure "graphics attachment validation" "dynamic rendering requires a non-empty 2D attachment extent"

toFrameBufferUse :: BufferIntent -> IO FrameBufferUse
toFrameBufferUse intent = do
  let byteSize = bufferIntentByteSizeInteger intent
  when (byteSize <= 0 || byteSize > toInteger (maxBound :: Word64)) $
    graphicsFailure "graphics resource validation" "buffer byte range is outside Word64"
  pure
    FrameBufferUse
      { frameBufferHandle = bufferIntentHandle intent
      , frameBufferMetadata = bufferIntentMetadata intent
      , frameBufferStage = bufferIntentStage intent
      , frameBufferAccess = bufferIntentAccess intent
      , frameBufferByteSize = fromInteger byteSize
      }

toFrameImageUse :: ImageIntent -> FrameImageUse
toFrameImageUse intent =
  FrameImageUse
    { frameImageHandle = imageIntentHandle intent
    , frameImageMetadata = imageIntentMetadata intent
    , frameImageSubresources = imageSubresources intent
    , frameImageStage = imageIntentStage intent
    , frameImageAccess = imageIntentAccess intent
    , frameImageLayout = imageIntentLayout intent
    }

imageSubresources :: ImageIntent -> [ImageState.ImageSubresource]
imageSubresources intent =
  [ ImageState.ImageSubresource mipLevel arrayLayer
  | mipLevel <- range (Resource.imageBindingMipLevel metadata) (Resource.imageBindingMipLevels metadata)
  , arrayLayer <- range (Resource.imageBindingArrayLayer metadata) (Resource.imageBindingArrayLayers metadata)
  ]
 where
  metadata = imageIntentMetadata intent
  range start count = [start .. start + count - 1]

bufferIntentByteSizeInteger :: BufferIntent -> Integer
bufferIntentByteSizeInteger intent =
  toInteger (Resource.bufferBindingElementCount metadata) * toInteger (Resource.bufferBindingStride metadata)
 where
  metadata = bufferIntentMetadata intent

recordDrawBody :: DrawPlan -> Handles.Pipeline -> Handles.PipelineLayout -> Handles.DescriptorSet -> [Source.ResolvedPushConstant] -> Handles.CommandBuffer -> [Source.RuntimeHandle] -> IO ()
recordDrawBody plan pipeline layout descriptorSet pushes commandBuffer previouslyRendered = do
  let Fundamental.Extent2D width height = drawPlanExtent plan
      renderArea = Fundamental.Rect2D (Fundamental.Offset2D 0 0) (drawPlanExtent plan)
      viewport = VkPipeline.Viewport 0 0 (fromIntegral width) (fromIntegral height) 0 1
      renderingInfo =
        (zero :: Rendering.RenderingInfo '[])
          { Rendering.renderArea = renderArea
          , Rendering.layerCount = 1
          , Rendering.colorAttachments = Vector.fromList (fmap (colorAttachmentInfo previouslyRendered) (drawPlanColorAttachments plan))
          , Rendering.depthAttachment = depthAttachmentInfo previouslyRendered <$> drawPlanDepthAttachment plan
          }
  Rendering.cmdBeginRendering commandBuffer renderingInfo
  Command.cmdBindPipeline commandBuffer PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS pipeline
  Command.cmdBindDescriptorSets commandBuffer PipelineBindPoint.PIPELINE_BIND_POINT_GRAPHICS layout 0 (Vector.singleton descriptorSet) Vector.empty
  let vertexIntents = fmap vertexBindingIntent (drawPlanVertexBindings plan)
      vertexHandles = Vector.fromList (fmap (Resource.bufferBindingRawHandle . bufferIntentMetadata) vertexIntents)
      vertexOffsets = Vector.fromList (fmap (fromIntegral . Resource.bufferBindingByteOffset . bufferIntentMetadata) vertexIntents)
  Command.cmdBindVertexBuffers commandBuffer 0 vertexHandles vertexOffsets
  traverse_ (bindIndexBuffer commandBuffer) (drawPlanIndexBinding plan)
  traverse_ (recordPushConstant commandBuffer layout) pushes
  Command.cmdSetViewport commandBuffer 0 (Vector.singleton viewport)
  Command.cmdSetScissor commandBuffer 0 (Vector.singleton renderArea)
  case drawPlanIndexCount plan of
    Nothing -> Command.cmdDraw commandBuffer (drawPlanVertexCount plan) 1 0 0
    Just count -> Command.cmdDrawIndexed commandBuffer count 1 0 0 0
  Rendering.cmdEndRendering commandBuffer

bindIndexBuffer :: Handles.CommandBuffer -> BufferIntent -> IO ()
bindIndexBuffer commandBuffer intent =
  Command.cmdBindIndexBuffer
    commandBuffer
    (Resource.bufferBindingRawHandle (bufferIntentMetadata intent))
    (fromIntegral (Resource.bufferBindingByteOffset (bufferIntentMetadata intent)))
    IndexType.INDEX_TYPE_UINT32

recordPushConstant :: Handles.CommandBuffer -> Handles.PipelineLayout -> Source.ResolvedPushConstant -> IO ()
recordPushConstant commandBuffer layout value =
  ByteString.useAsCStringLen (Source.resolvedPushConstantBytes value) $ \(pointer, size) ->
    Command.cmdPushConstants commandBuffer layout ShaderStage.SHADER_STAGE_ALL (fromIntegral (Source.resolvedPushConstantOffset value)) (fromIntegral size) (castPtr pointer)

colorAttachmentInfo :: [Source.RuntimeHandle] -> ImageIntent -> Rendering.RenderingAttachmentInfo
colorAttachmentInfo previouslyRendered intent =
  (zero :: Rendering.RenderingAttachmentInfo)
    { Rendering.imageView = Resource.imageBindingRawView (imageIntentMetadata intent)
    , Rendering.imageLayout = Layout.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    , Rendering.loadOp = attachmentLoadOp previouslyRendered intent
    , Rendering.storeOp = AttachmentStore.ATTACHMENT_STORE_OP_STORE
    , Rendering.clearValue = Command.Color (Command.Float32 0 0 0 1)
    }

depthAttachmentInfo :: [Source.RuntimeHandle] -> ImageIntent -> Rendering.RenderingAttachmentInfo
depthAttachmentInfo previouslyRendered intent =
  (zero :: Rendering.RenderingAttachmentInfo)
    { Rendering.imageView = Resource.imageBindingRawView (imageIntentMetadata intent)
    , Rendering.imageLayout = Layout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
    , Rendering.loadOp = attachmentLoadOp previouslyRendered intent
    , Rendering.storeOp = AttachmentStore.ATTACHMENT_STORE_OP_STORE
    , Rendering.clearValue = Command.DepthStencil (Command.ClearDepthStencilValue 1 0)
    }

attachmentLoadOp :: [Source.RuntimeHandle] -> ImageIntent -> AttachmentLoad.AttachmentLoadOp
attachmentLoadOp previouslyRendered intent
  | imageIntentHandle intent `elem` previouslyRendered = AttachmentLoad.ATTACHMENT_LOAD_OP_LOAD
  | otherwise = AttachmentLoad.ATTACHMENT_LOAD_OP_CLEAR

pipelineLayoutKey :: Source.PipelineInterface -> ByteString.ByteString
pipelineLayoutKey interface =
  ByteString8.pack (show (Source.pipelineResources interface, Source.pipelinePushConstants interface))

graphicsFailure :: String -> String -> IO a
graphicsFailure operation detail = throwIO (VulkanFailure operation detail)
