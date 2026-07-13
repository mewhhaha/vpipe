{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Linear (V3 (..), V4 (..))
import Vpipe.Buffer (Buffer, newBuffer, writeBuffer)
import Vpipe.Buffer qualified as Buffer
import Vpipe.Context (Context)
import Vpipe.Expr qualified as Expr
import Vpipe.Format (Blendable, ColorRenderable, Format (B8G8R8A8Srgb), KnownFormat)
import Vpipe.Frame (frame, frameColorTarget, render, renderTo)
import Vpipe.GLFW (Window, windowSurface)
import Vpipe.Graphics (PreparedGraphicsPipeline, newGraphicsRuntime, prepareGraphicsPipeline, renderGraphicsPipeline)
import Vpipe.Pipeline qualified as Pipeline
import Vpipe.Swapchain (PresentResult, defaultSwapchainConfig, newSwapchain)

import Vpipe.Examples.Common (ExampleOptions (exampleScreenshot), captureScreenshot, compilePipelineOrFail, newScreenshotTarget, offscreenFrameCount, parseExampleOptions, renderFrames, runWindowFrames, withExampleContext)

data Environment format = Environment
  { positions :: Pipeline.VertexBuffer (V3 Float)
  , target :: Pipeline.ColorImage format
  }

pipeline :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Pipeline.PipelineM (Environment format) ()
pipeline = do
  vertices <-
    Pipeline.vertexInput
      ( Pipeline.vertexSource "positions" positions ::
          Pipeline.VertexSource (Environment format) 'Pipeline.Triangles (V3 Float)
      )
  fragments <- Pipeline.rasterize Pipeline.defaultRaster (fmap vertex vertices)
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "color" target)
    (fmap Pipeline.unSmooth fragments)
 where
  vertex position =
    ( Expr.vec4 (Expr.x position) (Expr.y position) (Expr.z position) (Expr.constant 1)
    , Pipeline.Smooth (Expr.constant (V4 1 0.25 0.1 1) :: Expr.V (V4 Float))
    )

main :: IO ()
main = do
  options <- parseExampleOptions
  case exampleScreenshot options of
    Just _ -> runScreenshot options
    Nothing -> runWindowFrames options "vpipe guide — part 1" runWindow

runScreenshot :: ExampleOptions -> IO ()
runScreenshot options = withExampleContext $ \context -> do
  prepared <- prepare context
  vertexBuffer <- newPositions context
  screenshot <- newScreenshotTarget context
  screenshotBinding <- Pipeline.colorImageBinding screenshot
  let environment = Environment (Pipeline.vertexBufferBinding vertexBuffer) screenshotBinding
  renderFrames (offscreenFrameCount options) (renderGraphicsPipeline prepared environment)
  _ <- captureScreenshot options screenshot
  pure ()

runWindow :: Context -> Window -> IO (IO PresentResult)
runWindow context window = do
  prepared <- prepare context
  vertexBuffer <- newPositions context
  swapchain <- newSwapchain context (windowSurface window) defaultSwapchainConfig
  pure $ frame swapchain $ \current -> do
    let color = frameColorTarget current
        environment = Environment (Pipeline.vertexBufferBinding vertexBuffer) color :: Environment 'B8G8R8A8Srgb
    renderTo color (render prepared environment)

prepare :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Context -> IO (PreparedGraphicsPipeline (Environment format))
prepare context = do
  compiled <- compilePipelineOrFail "guide part 1" pipeline
  runtime <- newGraphicsRuntime context
  prepareGraphicsPipeline runtime compiled

newPositions :: Context -> IO (Buffer '[ 'Buffer.Vertex] (V3 Float))
newPositions context = do
  vertexBuffer <- newBuffer context 3
  writeBuffer
    vertexBuffer
    0
    [ V3 (-0.8) (-0.8) 0
    , V3 0.8 (-0.8) 0
    , V3 0 0.8 0
    ]
  pure vertexBuffer
