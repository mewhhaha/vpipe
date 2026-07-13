{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- | A full-screen fragment playground. One compact uniform carries time and
mouse-like coordinates, making this a convenient starting point for changing
the expression in 'shade' without touching resource or pipeline setup.
-}
module Vpipe.Examples.Shadertoy (runShadertoy) where

import Control.Monad (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef)
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

data ShadertoyEnvironment format = ShadertoyEnvironment
  { shadertoyQuad :: Pipeline.VertexBuffer (V2 Float, V2 Float)
  , shadertoyParameters :: Pipeline.UniformBuffer (V4 Float)
  , shadertoyTarget :: Pipeline.ColorImage format
  }

shadertoyPipeline :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Pipeline.PipelineM (ShadertoyEnvironment format) ()
shadertoyPipeline = do
  parameters <-
    Pipeline.uniform (Pipeline.uniformSource "time-and-mouse" shadertoyParameters) ::
      Pipeline.PipelineM (ShadertoyEnvironment format) (Expr.F (V4 Float))
  vertices <-
    Pipeline.vertexInput
      ( Pipeline.vertexSource "quad" shadertoyQuad ::
          Pipeline.VertexSource (ShadertoyEnvironment format) 'Pipeline.Triangles (V2 Float, V2 Float)
      )
  fragments <- Pipeline.rasterize Pipeline.defaultRaster (fmap fullscreenVertex vertices)
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "playground" shadertoyTarget)
    (fmap (\(Pipeline.Smooth uv) -> shade parameters uv) fragments)

shade :: Expr.F (V4 Float) -> Expr.F (V2 Float) -> Expr.F (V4 Float)
shade parameters uv =
  let time = Expr.x parameters
      mouse = Expr.vec2 (Expr.y parameters) (Expr.z parameters)
      delta = uv - mouse
      distanceSquared = Expr.dot delta delta
      glow = Expr.clamp (Expr.constant 0.025 / (distanceSquared + Expr.constant 0.008)) (Expr.constant 0) (Expr.constant 1)
      red = Expr.constant 0.5 + Expr.constant 0.5 * sin (Expr.x uv * Expr.constant 9 + time)
      green = Expr.constant 0.5 + Expr.constant 0.5 * sin (Expr.y uv * Expr.constant 11 - time * Expr.constant 1.3)
      blue = Expr.constant 0.5 + Expr.constant 0.5 * sin ((Expr.x uv + Expr.y uv) * Expr.constant 7 + time * Expr.constant 0.7)
   in Expr.vec4
        (Expr.clamp (red + glow) (Expr.constant 0) (Expr.constant 1))
        (Expr.clamp (green + glow * Expr.constant 0.6) (Expr.constant 0) (Expr.constant 1))
        (Expr.clamp (blue + glow * Expr.constant 0.25) (Expr.constant 0) (Expr.constant 1))
        (Expr.constant 1)

fullscreenVertex :: (Expr.V (V2 Float), Expr.V (V2 Float)) -> (Expr.V (V4 Float), Pipeline.Smooth 'Expr.Vertex (V2 Float))
fullscreenVertex (position, uv) =
  (Expr.vec4 (Expr.x position) (Expr.y position) (Expr.constant 0) (Expr.constant 1), Pipeline.Smooth uv)

runShadertoy :: ExampleOptions -> IO ()
runShadertoy options = case exampleScreenshot options of
  Just _ -> runShadertoyScreenshot options
  Nothing -> runWindowFrames options "vpipe shadertoy" runShadertoyWindow

runShadertoyScreenshot :: ExampleOptions -> IO ()
runShadertoyScreenshot options = withExampleContext $ \context -> do
  compiled <- compilePipelineOrFail "shadertoy" shadertoyPipeline
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  quad <- newFullscreenQuad context
  parameters <- newParameterBuffer context
  target <- newScreenshotTarget context
  targetBinding <- Pipeline.colorImageBinding target
  let environment =
        ShadertoyEnvironment
          { shadertoyQuad = Pipeline.vertexBufferBinding quad
          , shadertoyParameters = Pipeline.uniformBufferBinding parameters
          , shadertoyTarget = targetBinding
          }

  forM_ [0 .. offscreenFrameCount options - 1] $ \frameIndex -> do
    writeBuffer parameters 0 [frameParameters frameIndex]
    renderGraphicsPipeline prepared environment

  _ <- captureScreenshot options target
  pure ()

runShadertoyWindow :: Context -> Window -> IO (IO PresentResult)
runShadertoyWindow context window = do
  compiled <- compilePipelineOrFail "shadertoy" shadertoyPipeline
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  quad <- newFullscreenQuad context
  parameters <- newParameterBuffer context
  swapchain <- newSwapchain context (windowSurface window) defaultSwapchainConfig
  frameNumber <- newIORef 0
  let environment target =
        ShadertoyEnvironment
          { shadertoyQuad = Pipeline.vertexBufferBinding quad
          , shadertoyParameters = Pipeline.uniformBufferBinding parameters
          , shadertoyTarget = target
          }
  pure $ do
    modifyIORef' frameNumber (+ 1)
    currentFrame <- readIORef frameNumber
    writeBuffer parameters 0 [frameParameters currentFrame]
    frame swapchain $ \current -> do
      let target = frameColorTarget current
      renderTo target (render prepared (environment target :: ShadertoyEnvironment 'B8G8R8A8Srgb))

newParameterBuffer :: Context -> IO (Buffer '[ 'Buffer.Uniform] (V4 Float))
newParameterBuffer context = do
  parameters <- newBuffer context 1
  writeBuffer parameters 0 [frameParameters 0]
  pure parameters

frameParameters :: Int -> V4 Float
frameParameters frameIndex =
  let time = fromIntegral frameIndex * 0.35
   in V4 time (0.5 + 0.18 * cos time) (0.5 + 0.18 * sin time) 0
