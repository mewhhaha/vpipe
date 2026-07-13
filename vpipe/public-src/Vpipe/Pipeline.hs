{-# LANGUAGE PackageImports #-}

{- | A typed graphics pipeline DSL with direct SPIR-V compilation.

Compilation records shader interfaces, reifies stage roots, lowers them to
SPIR-V modules, and retains static pipeline state plus a plan for resolving
concrete environment resources. Vulkan object creation consumes that compiled
description in a later layer.

Compile a pipeline description after recording its stages and resources:

@
module Main (main) where

import Linear (V3)
import Vpipe.Expr
import Vpipe.Format (Format (R8G8B8A8Srgb))
import Vpipe.Pipeline

data Environment = Environment
  { environmentPositions :: VertexBuffer (V3 Float)
  , environmentColor :: ColorImage 'R8G8B8A8Srgb
  }

pipelineDescription :: PipelineM Environment ()
pipelineDescription = do
  positions <-
    vertexInput
      (vertexSource "positions" environmentPositions :: VertexSource Environment 'Triangles (V3 Float))
  fragments <-
    rasterize
      defaultRaster
      (fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (x position))) positions)
  drawColor
    defaultBlend
    (colorTarget "color" environmentColor)
    (fmap (\(Smooth red) -> vec4 red (constant 0) (constant 0) (constant 1)) fragments)

main :: IO ()
main = do
  result <- compilePipeline pipelineDescription
  case result of
    Left pipelineError -> fail (show pipelineError)
    Right _ -> putStrLn "Pipeline compiled."
@
-}
module Vpipe.Pipeline (
  PipelineM,
  PipelineError,
  PrimitiveTopology (..),
  CullMode (..),
  FrontFace (..),
  Raster (..),
  defaultRaster,
  BlendFactor (..),
  BlendOp (..),
  Blend (..),
  defaultBlend,
  DepthCompareOp (..),
  Depth (..),
  defaultDepth,
  KnownTopology,
  PrimitiveStream,
  zipStreams,
  FragmentStream,
  fragmentValue,
  mapFragments,
  Smooth (..),
  Flat (..),
  NoPerspective (..),
  VertexInput (VertexInputShader),
  GenericVertex (..),
  FragmentInput,
  ColorOutput,
  ColorOutputMatches,
  VertexBuffer,
  IndexBuffer,
  UniformBuffer,
  StorageBuffer,
  TextureBinding,
  TypedTextureBinding,
  ComparisonTextureBinding,
  ColorImage,
  DepthImage,
  vertexBufferBinding,
  indexBufferBinding,
  uniformBufferBinding,
  storageBufferBinding,
  textureBinding,
  typedTextureBinding,
  comparisonTextureBinding,
  colorImageBinding,
  depthImageBinding,
  VertexSource,
  IndexSource,
  Uniform,
  ShaderBlockValue,
  Storage,
  StorageRef,
  Texture,
  SampledTexture,
  ComparisonTexture,
  ColorTarget,
  DepthTarget,
  vertexSource,
  indexSource,
  uniformSource,
  storageSource,
  textureSource,
  sampledTextureSource,
  comparisonTextureSource,
  colorTarget,
  depthTarget,
  vertexInput,
  uniform,
  pushConstant,
  storageBuffer,
  texture,
  sampledTexture,
  comparisonTexture,
  VertexStagePosition,
  rasterize,
  rasterizeIndexed,
  discardWhen,
  writeDepth,
  drawColor,
  drawDepth,
  compilePipeline,
  CompiledPipeline,
) where

import "vpipe" Vpipe.Pipeline.Internal
