{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Graphics preparation artifacts retained by 'Pipeline.CompiledPipeline'.
module Vpipe.Graphics.Compile.Internal (
  GraphicsCompileError (..),
  LoweredColorOutput (..),
  LoweredDepthOutput (..),
  LoweredDraw (..),
  lowerCompiledDraw,
) where

import Vpipe.Format (VkFormat)
import Vpipe.Pipeline.Internal qualified as Pipeline
import Vpipe.SpirV.Assembler (SpirVModule)
import Vpipe.SpirV.Codegen qualified as Codegen

data GraphicsCompileError = GraphicsCompileError
  { graphicsCompileDrawIdentifier :: Int
  , graphicsCompileStage :: Codegen.ShaderStage
  , graphicsCompileInvariant :: String
  }
  deriving stock (Eq, Show)

data LoweredColorOutput = LoweredColorOutput
  { loweredColorTargetName :: String
  , loweredColorFormat :: VkFormat
  , loweredColorBlend :: Pipeline.Blend
  }
  deriving stock (Eq, Show)

data LoweredDepthOutput = LoweredDepthOutput
  { loweredDepthTargetName :: String
  , loweredDepthFormat :: VkFormat
  , loweredDepthState :: Pipeline.Depth
  }
  deriving stock (Eq, Show)

data LoweredDraw = LoweredDraw
  { loweredDrawIdentifier :: Int
  , loweredDrawTopology :: Pipeline.PrimitiveTopology
  , loweredDrawRaster :: Pipeline.Raster
  , loweredDrawIndexSource :: Maybe String
  , loweredVertexModule :: SpirVModule
  , loweredFragmentModule :: SpirVModule
  , loweredVertexBindings :: [Pipeline.VertexBindingLayout]
  , loweredVertexAttributes :: [Pipeline.VertexAttribute]
  , loweredColorOutputs :: [LoweredColorOutput]
  , loweredDepthOutput :: Maybe LoweredDepthOutput
  }
  deriving stock (Eq, Show)

{- | Project the already-compiled graphics artifacts into the runtime shape.
The interface argument remains for source compatibility; code generation and
validation have already completed in 'Pipeline.compilePipeline'.
-}
lowerCompiledDraw :: Pipeline.PipelineInterface -> Pipeline.CompiledDraw -> Either GraphicsCompileError LoweredDraw
lowerCompiledDraw _ draw =
  Right
    LoweredDraw
      { loweredDrawIdentifier = Pipeline.compiledDrawIdentifier draw
      , loweredDrawTopology = Pipeline.compiledDrawTopology draw
      , loweredDrawRaster = Pipeline.compiledDrawRaster draw
      , loweredDrawIndexSource = Pipeline.compiledDrawIndexSource draw
      , loweredVertexModule = Pipeline.compiledVertexModule draw
      , loweredFragmentModule = Pipeline.compiledFragmentModule draw
      , loweredVertexBindings = Pipeline.compiledVertexBindings draw
      , loweredVertexAttributes = Pipeline.compiledVertexAttributes draw
      , loweredColorOutputs =
          [ LoweredColorOutput
              (Pipeline.compiledColorTargetName output)
              (Pipeline.compiledColorFormat output)
              (Pipeline.compiledColorBlend output)
          | output <- Pipeline.compiledColorOutputs draw
          ]
      , loweredDepthOutput =
          (\output -> LoweredDepthOutput (Pipeline.compiledDepthTargetName output) (Pipeline.compiledDepthFormat output) (Pipeline.compiledDepthState output))
            <$> Pipeline.compiledDepthOutput draw
      }
