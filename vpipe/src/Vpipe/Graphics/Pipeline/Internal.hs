{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Context-owned Vulkan graphics pipeline creation for lowered draws.
module Vpipe.Graphics.Pipeline.Internal (
  PipelineDescription (..),
  acquireGraphicsPipeline,
  acquireGraphicsPipelineLeased,
  acquireShaderLeased,
) where

import Control.Concurrent.MVar (modifyMVarMasked, modifyMVarMasked_, newMVar)
import Control.Exception (catch, onException, throwIO)
import Control.Monad (when)
import Data.Bifunctor (second)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.List (findIndex)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.Enums.BlendFactor qualified as VkBlendFactor
import Vulkan.Core10.Enums.BlendOp qualified as VkBlendOp
import Vulkan.Core10.Enums.ColorComponentFlagBits qualified as ColorComponent
import Vulkan.Core10.Enums.CompareOp qualified as CompareOp
import Vulkan.Core10.Enums.CullModeFlagBits qualified as CullMode
import Vulkan.Core10.Enums.DynamicState qualified as DynamicState
import Vulkan.Core10.Enums.Format qualified as Format
import Vulkan.Core10.Enums.FrontFace qualified as FrontFace
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.PolygonMode qualified as PolygonMode
import Vulkan.Core10.Enums.PrimitiveTopology qualified as Topology
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Enums.SampleCountFlagBits qualified as Samples
import Vulkan.Core10.Enums.ShaderStageFlagBits qualified as ShaderStage
import Vulkan.Core10.Handles qualified as Vk
import Vulkan.Core10.Pipeline qualified as Pipeline
import Vulkan.Core10.Shader qualified as Shader
import Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering qualified as Rendering
import Vulkan.Exception qualified as Vulkan
import Vulkan.Zero (zero)

import Vpipe.Context.Internal (Context, contextDevice, derivedObjectName, registerContextFinalizerLeased, setObjectNameLeased, withContextLease)
import Vpipe.Diagnostics.Dump.Internal (ShaderDump, retainShaderFailureArtifact, throwShaderDriverFailureWith)
import Vpipe.Error (VpipeError (..))
import Vpipe.Graphics.Cache.Internal (CachedPipeline (..), CachedShader (..), GraphicsCache (..), GraphicsStats (..), PipelineKey (..), PrimitiveTopologyKey (..))
import Vpipe.Pipeline.Internal qualified as Source

data PipelineDescription = PipelineDescription
  { pipelineVertexShaderBytes :: ByteString
  , pipelineFragmentShaderBytes :: ByteString
  , pipelineVertexShaderDump :: ShaderDump
  , pipelineFragmentShaderDump :: ShaderDump
  , pipelinePrimitiveTopology :: Source.PrimitiveTopology
  , pipelineRaster :: Source.Raster
  , pipelineVertexBindings :: [Source.VertexBindingLayout]
  , pipelineVertexAttributes :: [Source.VertexAttribute]
  , pipelineColorOutputs :: [(Format.Format, Source.Blend)]
  , pipelineDepthOutput :: Maybe (Format.Format, Source.Depth)
  , pipelineSampleCount :: Samples.SampleCountFlagBits
  , pipelineLayoutStructure :: ByteString
  {- ^ An exact encoding of descriptor set layouts and push constant ranges.
  The native pipeline-layout handle is intentionally not part of the key.
  -}
  }

acquireGraphicsPipeline :: Context -> GraphicsCache -> Vk.PipelineLayout -> PipelineDescription -> IO Vk.Pipeline
acquireGraphicsPipeline context cache layout description =
  withContextLease context (acquireGraphicsPipelineLeased context cache layout description)

-- | Internal variant for a caller which already owns the Context lease.
acquireGraphicsPipelineLeased :: Context -> GraphicsCache -> Vk.PipelineLayout -> PipelineDescription -> IO Vk.Pipeline
acquireGraphicsPipelineLeased context cache layout description =
  modifyMVarMasked (cachedPipelines cache) $ \cached -> do
    let key = pipelineKey description
    case findPipeline key cached of
      Just pipeline -> pure (cached, pipeline)
      Nothing -> do
        vertexShader <- acquireGraphicsShader context cache "vertex" (pipelineVertexShaderDump description) (pipelineVertexShaderBytes description)
        fragmentShader <- acquireGraphicsShader context cache "fragment" (pipelineFragmentShaderDump description) (pipelineFragmentShaderBytes description)
        pipeline <- createPipeline context cache layout description vertexShader fragmentShader
        modifyMVarMasked_ (cachedGraphicsStats cache) $ \stats ->
          pure stats{graphicsPipelineCreations = graphicsPipelineCreations stats + 1}
        pure (CachedPipeline key pipeline : cached, pipeline)

pipelineKey :: PipelineDescription -> PipelineKey
pipelineKey description =
  GraphicsPipelineKey
    (pipelineVertexShaderBytes description)
    (pipelineFragmentShaderBytes description)
    (topologyKey (pipelinePrimitiveTopology description))
    [(Source.vertexBindingSourceName binding, fromIntegral (Source.vertexBindingStride binding)) | binding <- pipelineVertexBindings description]
    [ ( Source.vertexAttributeSourceName attribute
      , Source.vertexAttributeName attribute
      , fromIntegral (Source.vertexAttributeLocation attribute)
      , Source.vertexAttributeFormat attribute
      , fromIntegral (Source.vertexAttributeOffset attribute)
      )
    | attribute <- pipelineVertexAttributes description
    ]
    [(format, blendKey blend) | (format, blend) <- pipelineColorOutputs description]
    (second depthKey <$> pipelineDepthOutput description)
    (rasterKey (pipelineRaster description))
    (pipelineSampleCount description)
    (pipelineLayoutStructure description)

topologyKey :: Source.PrimitiveTopology -> PrimitiveTopologyKey
topologyKey Source.Points = PointTopology
topologyKey Source.Lines = LineTopology
topologyKey Source.Triangles = TriangleTopology

findPipeline :: PipelineKey -> [CachedPipeline] -> Maybe Vk.Pipeline
findPipeline _ [] = Nothing
findPipeline key (CachedPipeline cachedKey pipeline : rest)
  | key == cachedKey = Just pipeline
  | otherwise = findPipeline key rest

acquireGraphicsShader :: Context -> GraphicsCache -> String -> ShaderDump -> ByteString -> IO Vk.ShaderModule
acquireGraphicsShader context cache stage artifact bytes = do
  (shader, created) <- acquireShaderLeased context cache stage artifact bytes
  when created $
    modifyMVarMasked_ (cachedGraphicsStats cache) $ \stats ->
      pure stats{shaderModuleCreations = shaderModuleCreations stats + 1}
  pure shader

{- | Acquire an exact-byte cached shader module without attributing its creation
to either the graphics or compute statistics. The caller owns that attribution.
-}
acquireShaderLeased :: Context -> GraphicsCache -> String -> ShaderDump -> ByteString -> IO (Vk.ShaderModule, Bool)
acquireShaderLeased context cache stage artifact bytes =
  acquireShaderLeasedWith context cache bytes $ \error' ->
    let result = Vulkan.vulkanExceptionResult error'
     in throwShaderDriverFailureWith
          (retainShaderFailureArtifact artifact)
          "vkCreateShaderModule"
          ("vkCreateShaderModule rejected vpipe's generated " <> stage <> " shader with " <> show result)
          result

acquireShaderLeasedWith :: Context -> GraphicsCache -> ByteString -> (Vulkan.VulkanException -> IO Vk.ShaderModule) -> IO (Vk.ShaderModule, Bool)
acquireShaderLeasedWith context cache bytes onFailure =
  modifyMVarMasked (cachedShaders cache) $ \cached ->
    case findShader bytes cached of
      Just shader -> pure (cached, (shader, False))
      Nothing -> do
        let device = contextDevice context
            info = (zero :: Shader.ShaderModuleCreateInfo '[]){Shader.code = bytes}
        shader <- Shader.createShaderModule device info Nothing `catch` onFailure
        cleanup <- releaseOnce (Shader.destroyShaderModule device shader Nothing)
        setObjectNameLeased context ObjectType.OBJECT_TYPE_SHADER_MODULE (shaderModuleHandleWord shader) (derivedObjectName "shader-module" (shaderModuleHandleWord shader))
          `onException` cleanup
        registerContextFinalizerLeased context cleanup `onException` cleanup
        pure (CachedShader bytes shader : cached, (shader, True))

findShader :: ByteString -> [CachedShader] -> Maybe Vk.ShaderModule
findShader _ [] = Nothing
findShader bytes (CachedShader cachedBytes shader : rest)
  | bytes == cachedBytes = Just shader
  | otherwise = findShader bytes rest

createPipeline :: Context -> GraphicsCache -> Vk.PipelineLayout -> PipelineDescription -> Vk.ShaderModule -> Vk.ShaderModule -> IO Vk.Pipeline
createPipeline context cache layout description vertexShader fragmentShader = do
  attributes <-
    either
      (throwIO . VulkanFailure "graphics pipeline validation")
      pure
      (vertexAttributes (pipelineVertexBindings description) (pipelineVertexAttributes description))
  let device = contextDevice context
      stages = Vector.fromList [shaderStage ShaderStage.SHADER_STAGE_VERTEX_BIT vertexShader, shaderStage ShaderStage.SHADER_STAGE_FRAGMENT_BIT fragmentShader]
      vertexInput =
        Chain.SomeStruct
          ( (zero :: Pipeline.PipelineVertexInputStateCreateInfo '[])
              { Pipeline.vertexBindingDescriptions = Vector.fromList (vertexBindings (pipelineVertexBindings description))
              , Pipeline.vertexAttributeDescriptions = Vector.fromList attributes
              }
          )
      inputAssembly =
        (zero :: Pipeline.PipelineInputAssemblyStateCreateInfo)
          { Pipeline.topology = vkTopology (pipelinePrimitiveTopology description)
          }
      viewport = Chain.SomeStruct ((zero :: Pipeline.PipelineViewportStateCreateInfo '[]){Pipeline.viewportCount = 1, Pipeline.scissorCount = 1})
      raster =
        Chain.SomeStruct
          ( (zero :: Pipeline.PipelineRasterizationStateCreateInfo '[])
              { Pipeline.polygonMode = PolygonMode.POLYGON_MODE_FILL
              , Pipeline.cullMode = vkCullMode (Source.cullMode (pipelineRaster description))
              , Pipeline.frontFace = vkFrontFace (Source.frontFace (pipelineRaster description))
              , Pipeline.lineWidth = 1
              }
          )
      multisample = Chain.SomeStruct ((zero :: Pipeline.PipelineMultisampleStateCreateInfo '[]){Pipeline.rasterizationSamples = pipelineSampleCount description})
      colorBlend =
        Chain.SomeStruct
          ( (zero :: Pipeline.PipelineColorBlendStateCreateInfo '[])
              { Pipeline.attachmentCount = fromIntegral (length (pipelineColorOutputs description))
              , Pipeline.attachments = Vector.fromList [colorWriteAttachment blend | (_, blend) <- pipelineColorOutputs description]
              }
          )
      dynamic = (zero :: Pipeline.PipelineDynamicStateCreateInfo){Pipeline.dynamicStates = Vector.fromList [DynamicState.DYNAMIC_STATE_VIEWPORT, DynamicState.DYNAMIC_STATE_SCISSOR]}
      rendering = Rendering.PipelineRenderingCreateInfo 0 (Vector.fromList (fmap fst (pipelineColorOutputs description))) (maybe zero fst (pipelineDepthOutput description)) zero
      createInfo =
        Pipeline.GraphicsPipelineCreateInfo
          (rendering Chain.:& ())
          zero
          2
          stages
          (Just vertexInput)
          (Just inputAssembly)
          Nothing
          (Just viewport)
          (Just raster)
          (Just multisample)
          (depthStencil (pipelineDepthOutput description))
          (Just colorBlend)
          (Just dynamic)
          layout
          zero
          0
          zero
          (-1)
  (result, pipelines) <-
    Pipeline.createGraphicsPipelines device (rawPipelineCache cache) (Vector.singleton (Chain.SomeStruct createInfo)) Nothing
      `catch` graphicsPipelineException description
  case (result, Vector.toList pipelines) of
    (Result.SUCCESS, [pipeline]) -> do
      cleanup <- releaseOnce (Pipeline.destroyPipeline device pipeline Nothing)
      setObjectNameLeased context ObjectType.OBJECT_TYPE_PIPELINE (pipelineHandleWord pipeline) (derivedObjectName "graphics-pipeline" (pipelineHandleWord pipeline))
        `onException` cleanup
      registerContextFinalizerLeased context cleanup `onException` cleanup
      pure pipeline
    _ -> do
      traverse_ (Pipeline.destroyPipeline device `flip` Nothing) pipelines
      throwGraphicsPipelineFailure description result (Just (Vector.length pipelines))

graphicsPipelineException :: PipelineDescription -> Vulkan.VulkanException -> IO (Result.Result, Vector.Vector Vk.Pipeline)
graphicsPipelineException description error' =
  throwGraphicsPipelineFailure description (Vulkan.vulkanExceptionResult error') Nothing

throwGraphicsPipelineFailure :: PipelineDescription -> Result.Result -> Maybe Int -> IO a
throwGraphicsPipelineFailure description result pipelineCount =
  throwShaderDriverFailureWith
    retainGraphicsArtifacts
    "vkCreateGraphicsPipelines"
    detail
    result
 where
  detail = show result <> maybe "" (\count -> " returned " <> show count <> " pipelines") pipelineCount
  retainGraphicsArtifacts = do
    fragmentPath <- retainShaderFailureArtifact (pipelineFragmentShaderDump description)
    _ <- retainShaderFailureArtifact (pipelineVertexShaderDump description)
    pure fragmentPath

shaderModuleHandleWord :: Vk.ShaderModule -> Word64
shaderModuleHandleWord (Vk.ShaderModule handle) = handle

pipelineHandleWord :: Vk.Pipeline -> Word64
pipelineHandleWord (Vk.Pipeline handle) = handle

shaderStage :: ShaderStage.ShaderStageFlagBits -> Vk.ShaderModule -> Chain.SomeStruct Pipeline.PipelineShaderStageCreateInfo
shaderStage stage module' = Chain.SomeStruct ((zero :: Pipeline.PipelineShaderStageCreateInfo '[]){Pipeline.stage = stage, Pipeline.module' = module', Pipeline.name = "main"})

vertexBindings :: [Source.VertexBindingLayout] -> [Pipeline.VertexInputBindingDescription]
vertexBindings bindings =
  [ Pipeline.VertexInputBindingDescription (fromIntegral index) (fromIntegral (Source.vertexBindingStride binding)) Pipeline.VERTEX_INPUT_RATE_VERTEX
  | (index, binding) <- zip [0 :: Int ..] bindings
  ]

vertexAttributes :: [Source.VertexBindingLayout] -> [Source.VertexAttribute] -> Either String [Pipeline.VertexInputAttributeDescription]
vertexAttributes bindings = traverse attribute
 where
  attribute source = do
    binding <- bindingIndex source
    pure
      ( Pipeline.VertexInputAttributeDescription
          (fromIntegral (Source.vertexAttributeLocation source))
          binding
          (Source.vertexAttributeFormat source)
          (fromIntegral (Source.vertexAttributeOffset source))
      )
  bindingIndex source =
    case findIndex ((== Source.vertexAttributeSourceName source) . Source.vertexBindingSourceName) bindings of
      Just index -> Right (fromIntegral index)
      Nothing -> Left ("vertex attribute " <> show (Source.vertexAttributeName source) <> " has no active binding")

colorWriteAttachment :: Source.Blend -> Pipeline.PipelineColorBlendAttachmentState
colorWriteAttachment blend =
  (zero :: Pipeline.PipelineColorBlendAttachmentState)
    { Pipeline.blendEnable = Source.blendEnabled blend
    , Pipeline.srcColorBlendFactor = vkBlendFactor (Source.blendSourceColorFactor blend)
    , Pipeline.dstColorBlendFactor = vkBlendFactor (Source.blendDestinationColorFactor blend)
    , Pipeline.colorBlendOp = vkBlendOp (Source.blendColorOp blend)
    , Pipeline.srcAlphaBlendFactor = vkBlendFactor (Source.blendSourceAlphaFactor blend)
    , Pipeline.dstAlphaBlendFactor = vkBlendFactor (Source.blendDestinationAlphaFactor blend)
    , Pipeline.alphaBlendOp = vkBlendOp (Source.blendAlphaOp blend)
    , Pipeline.colorWriteMask = ColorComponent.COLOR_COMPONENT_R_BIT .|. ColorComponent.COLOR_COMPONENT_G_BIT .|. ColorComponent.COLOR_COMPONENT_B_BIT .|. ColorComponent.COLOR_COMPONENT_A_BIT
    }

depthStencil :: Maybe (Format.Format, Source.Depth) -> Maybe Pipeline.PipelineDepthStencilStateCreateInfo
depthStencil Nothing = Nothing
depthStencil (Just (_, depth)) =
  Just
    ( (zero :: Pipeline.PipelineDepthStencilStateCreateInfo)
        { Pipeline.depthTestEnable = Source.depthTestEnabled depth
        , Pipeline.depthWriteEnable = Source.depthWriteEnabled depth
        , Pipeline.depthCompareOp = vkDepthCompareOp (Source.depthCompareOp depth)
        }
    )

rasterKey :: Source.Raster -> (Int, Int)
rasterKey raster = (fromEnum (Source.cullMode raster), fromEnum (Source.frontFace raster))

blendKey :: Source.Blend -> (Bool, Int, Int, Int, Int, Int, Int)
blendKey blend = (Source.blendEnabled blend, fromEnum (Source.blendSourceColorFactor blend), fromEnum (Source.blendDestinationColorFactor blend), fromEnum (Source.blendColorOp blend), fromEnum (Source.blendSourceAlphaFactor blend), fromEnum (Source.blendDestinationAlphaFactor blend), fromEnum (Source.blendAlphaOp blend))

depthKey :: Source.Depth -> (Bool, Bool, Int)
depthKey depth = (Source.depthTestEnabled depth, Source.depthWriteEnabled depth, fromEnum (Source.depthCompareOp depth))

vkCullMode :: Source.CullMode -> CullMode.CullModeFlags
vkCullMode Source.CullNone = CullMode.CULL_MODE_NONE
vkCullMode Source.CullFront = CullMode.CULL_MODE_FRONT_BIT
vkCullMode Source.CullBack = CullMode.CULL_MODE_BACK_BIT

vkFrontFace :: Source.FrontFace -> FrontFace.FrontFace
vkFrontFace Source.FrontClockwise = FrontFace.FRONT_FACE_CLOCKWISE
vkFrontFace Source.FrontCounterClockwise = FrontFace.FRONT_FACE_COUNTER_CLOCKWISE

vkBlendFactor :: Source.BlendFactor -> VkBlendFactor.BlendFactor
vkBlendFactor Source.Zero = VkBlendFactor.BLEND_FACTOR_ZERO
vkBlendFactor Source.One = VkBlendFactor.BLEND_FACTOR_ONE
vkBlendFactor Source.SourceColor = VkBlendFactor.BLEND_FACTOR_SRC_COLOR
vkBlendFactor Source.OneMinusSourceColor = VkBlendFactor.BLEND_FACTOR_ONE_MINUS_SRC_COLOR
vkBlendFactor Source.DestinationColor = VkBlendFactor.BLEND_FACTOR_DST_COLOR
vkBlendFactor Source.OneMinusDestinationColor = VkBlendFactor.BLEND_FACTOR_ONE_MINUS_DST_COLOR
vkBlendFactor Source.SourceAlpha = VkBlendFactor.BLEND_FACTOR_SRC_ALPHA
vkBlendFactor Source.OneMinusSourceAlpha = VkBlendFactor.BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
vkBlendFactor Source.DestinationAlpha = VkBlendFactor.BLEND_FACTOR_DST_ALPHA
vkBlendFactor Source.OneMinusDestinationAlpha = VkBlendFactor.BLEND_FACTOR_ONE_MINUS_DST_ALPHA
vkBlendFactor Source.SourceAlphaSaturate = VkBlendFactor.BLEND_FACTOR_SRC_ALPHA_SATURATE

vkBlendOp :: Source.BlendOp -> VkBlendOp.BlendOp
vkBlendOp Source.Add = VkBlendOp.BLEND_OP_ADD
vkBlendOp Source.Subtract = VkBlendOp.BLEND_OP_SUBTRACT
vkBlendOp Source.ReverseSubtract = VkBlendOp.BLEND_OP_REVERSE_SUBTRACT
vkBlendOp Source.Min = VkBlendOp.BLEND_OP_MIN
vkBlendOp Source.Max = VkBlendOp.BLEND_OP_MAX

vkDepthCompareOp :: Source.DepthCompareOp -> CompareOp.CompareOp
vkDepthCompareOp Source.DepthNever = CompareOp.COMPARE_OP_NEVER
vkDepthCompareOp Source.DepthLess = CompareOp.COMPARE_OP_LESS
vkDepthCompareOp Source.DepthEqual = CompareOp.COMPARE_OP_EQUAL
vkDepthCompareOp Source.DepthLessOrEqual = CompareOp.COMPARE_OP_LESS_OR_EQUAL
vkDepthCompareOp Source.DepthGreater = CompareOp.COMPARE_OP_GREATER
vkDepthCompareOp Source.DepthNotEqual = CompareOp.COMPARE_OP_NOT_EQUAL
vkDepthCompareOp Source.DepthGreaterOrEqual = CompareOp.COMPARE_OP_GREATER_OR_EQUAL
vkDepthCompareOp Source.DepthAlways = CompareOp.COMPARE_OP_ALWAYS

vkTopology :: Source.PrimitiveTopology -> Topology.PrimitiveTopology
vkTopology Source.Points = Topology.PRIMITIVE_TOPOLOGY_POINT_LIST
vkTopology Source.Lines = Topology.PRIMITIVE_TOPOLOGY_LINE_LIST
vkTopology Source.Triangles = Topology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST

releaseOnce :: IO () -> IO (IO ())
releaseOnce release = do
  released <- newMVar False
  pure $
    modifyMVarMasked_ released $ \done ->
      if done then pure True else release >> pure True
