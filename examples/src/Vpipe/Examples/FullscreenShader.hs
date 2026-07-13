{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Vpipe.Examples.FullscreenShader (
  FullscreenShader (..),
  runFullscreenShader,
) where

import Control.Monad (forM_)
import GHC.Clock (getMonotonicTimeNSec)
import Linear (V2 (..), V4 (..))
import Vpipe.Buffer (Buffer, newBuffer, writeBuffer)
import Vpipe.Buffer qualified as Buffer
import Vpipe.Context (Context)
import Vpipe.Expr qualified as Expr
import Vpipe.Format (Blendable, ColorRenderable, Format (B8G8R8A8Srgb), KnownFormat)
import Vpipe.Frame (frame, frameColorTarget, render, renderTo)
import Vpipe.GLFW (Window, windowSurface)
import Vpipe.Graphics (newGraphicsRuntime, prepareGraphicsPipeline, renderGraphicsPipeline)
import Vpipe.Pipeline qualified as Pipeline
import Vpipe.Swapchain (PresentResult, defaultSwapchainConfig, newSwapchain)

import Vpipe.Examples.Common (ExampleOptions (exampleScreenshot), captureScreenshot, compilePipelineOrFail, newFullscreenQuad, newScreenshotTarget, offscreenFrameCount, runWindowFrames, withExampleContext)

data FullscreenShader = FullscreenShader
  { fullscreenShaderLabel :: String
  , fullscreenShaderTitle :: String
  , fullscreenShaderFragment :: Expr.F (V4 Float) -> Expr.F (V2 Float) -> Expr.F (V4 Float)
  , fullscreenShaderParameters :: Maybe Window -> Float -> IO (V4 Float)
  }

data ShaderEnvironment format = ShaderEnvironment
  { shaderQuad :: Pipeline.VertexBuffer (V2 Float, V2 Float)
  , shaderParameters :: Pipeline.UniformBuffer (V4 Float)
  , shaderTarget :: Pipeline.ColorImage format
  }

shaderPipeline :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => FullscreenShader -> Pipeline.PipelineM (ShaderEnvironment format) ()
shaderPipeline shader = do
  parameters <-
    Pipeline.uniform (Pipeline.uniformSource "shader-inputs" shaderParameters) ::
      Pipeline.PipelineM (ShaderEnvironment format) (Expr.F (V4 Float))
  vertices <-
    Pipeline.vertexInput
      ( Pipeline.vertexSource "quad" shaderQuad ::
          Pipeline.VertexSource (ShaderEnvironment format) 'Pipeline.Triangles (V2 Float, V2 Float)
      )
  fragments <- Pipeline.rasterize Pipeline.defaultRaster (fmap fullscreenVertex vertices)
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "shader-output" shaderTarget)
    (fmap (\(Pipeline.Smooth uv) -> fullscreenShaderFragment shader parameters uv) fragments)

fullscreenVertex :: (Expr.V (V2 Float), Expr.V (V2 Float)) -> (Expr.V (V4 Float), Pipeline.Smooth 'Expr.Vertex (V2 Float))
fullscreenVertex (position, uv) =
  (Expr.vec4 (Expr.x position) (Expr.y position) (Expr.constant 0) (Expr.constant 1), Pipeline.Smooth uv)

runFullscreenShader :: FullscreenShader -> ExampleOptions -> IO ()
runFullscreenShader shader options = case exampleScreenshot options of
  Just _ -> runShaderScreenshot shader options
  Nothing -> runWindowFrames options (fullscreenShaderTitle shader) (runShaderWindow shader)

runShaderScreenshot :: FullscreenShader -> ExampleOptions -> IO ()
runShaderScreenshot shader options = withExampleContext $ \context -> do
  compiled <- compilePipelineOrFail (fullscreenShaderLabel shader) (shaderPipeline shader)
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  quad <- newFullscreenQuad context
  parameters <- newParameterBuffer context
  target <- newScreenshotTarget context
  targetBinding <- Pipeline.colorImageBinding target
  let environment =
        ShaderEnvironment
          { shaderQuad = Pipeline.vertexBufferBinding quad
          , shaderParameters = Pipeline.uniformBufferBinding parameters
          , shaderTarget = targetBinding
          }

  forM_ [0 .. offscreenFrameCount options - 1] $ \frameIndex -> do
    inputs <- fullscreenShaderParameters shader Nothing (deterministicTime frameIndex)
    writeBuffer parameters 0 [inputs]
    renderGraphicsPipeline prepared environment

  _ <- captureScreenshot options target
  pure ()

runShaderWindow :: FullscreenShader -> Context -> Window -> IO (IO PresentResult)
runShaderWindow shader context window = do
  compiled <- compilePipelineOrFail (fullscreenShaderLabel shader) (shaderPipeline shader)
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  quad <- newFullscreenQuad context
  parameters <- newParameterBuffer context
  swapchain <- newSwapchain context (windowSurface window) defaultSwapchainConfig
  startedAt <- getMonotonicTimeNSec
  let environment target =
        ShaderEnvironment
          { shaderQuad = Pipeline.vertexBufferBinding quad
          , shaderParameters = Pipeline.uniformBufferBinding parameters
          , shaderTarget = target
          }
  pure $ do
    currentTime <- getMonotonicTimeNSec
    let elapsedSeconds = fromIntegral (currentTime - startedAt) / 1_000_000_000
    inputs <- fullscreenShaderParameters shader (Just window) elapsedSeconds
    writeBuffer parameters 0 [inputs]
    frame swapchain $ \current -> do
      let target = frameColorTarget current
      renderTo target (render prepared (environment target :: ShaderEnvironment 'B8G8R8A8Srgb))

newParameterBuffer :: Context -> IO (Buffer '[ 'Buffer.Uniform] (V4 Float))
newParameterBuffer context = do
  parameters <- newBuffer context 1
  writeBuffer parameters 0 [V4 0 0 0 0]
  pure parameters

deterministicTime :: Int -> Float
deterministicTime frameIndex = fromIntegral frameIndex * 0.35
