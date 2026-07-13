{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Word (Word32)
import Linear (V2 (..), V4 (..))
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
  { vertices :: Pipeline.VertexBuffer (V2 Float, V4 Float)
  , indices :: Pipeline.IndexBuffer
  , target :: Pipeline.ColorImage format
  }

pipeline :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Pipeline.PipelineM (Environment format) ()
pipeline = do
  vertexStream <-
    Pipeline.vertexInput
      ( Pipeline.vertexSource "quad" vertices ::
          Pipeline.VertexSource (Environment format) 'Pipeline.Triangles (V2 Float, V4 Float)
      )
  fragments <-
    Pipeline.rasterizeIndexed
      Pipeline.defaultRaster
      (Pipeline.indexSource "quad-indices" indices)
      (fmap vertex vertexStream)
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "color" target)
    (fmap Pipeline.unSmooth fragments)
 where
  vertex (position, color) =
    ( Expr.vec4 (Expr.x position) (Expr.y position) (Expr.constant 0) (Expr.constant 1)
    , Pipeline.Smooth color
    )

main :: IO ()
main = do
  options <- parseExampleOptions
  case exampleScreenshot options of
    Just _ -> runScreenshot options
    Nothing -> runWindowFrames options "vpipe guide — part 2" runWindow

runScreenshot :: ExampleOptions -> IO ()
runScreenshot options = withExampleContext $ \context -> do
  prepared <- prepare context
  (vertexBuffer, indexBuffer) <- newGeometry context
  screenshot <- newScreenshotTarget context
  screenshotBinding <- Pipeline.colorImageBinding screenshot
  let environment = bindings (Pipeline.vertexBufferBinding vertexBuffer) (Pipeline.indexBufferBinding indexBuffer) screenshotBinding
  renderFrames (offscreenFrameCount options) (renderGraphicsPipeline prepared environment)
  _ <- captureScreenshot options screenshot
  pure ()

runWindow :: Context -> Window -> IO (IO PresentResult)
runWindow context window = do
  prepared <- prepare context
  (vertexBuffer, indexBuffer) <- newGeometry context
  swapchain <- newSwapchain context (windowSurface window) defaultSwapchainConfig
  pure $ frame swapchain $ \current -> do
    let color = frameColorTarget current
        environment = bindings (Pipeline.vertexBufferBinding vertexBuffer) (Pipeline.indexBufferBinding indexBuffer) color :: Environment 'B8G8R8A8Srgb
    renderTo color (render prepared environment)

bindings :: Pipeline.VertexBuffer (V2 Float, V4 Float) -> Pipeline.IndexBuffer -> Pipeline.ColorImage format -> Environment format
bindings vertexBuffer indexBuffer color =
  Environment
    { vertices = vertexBuffer
    , indices = indexBuffer
    , target = color
    }

prepare :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Context -> IO (PreparedGraphicsPipeline (Environment format))
prepare context = do
  compiled <- compilePipelineOrFail "guide part 2" pipeline
  runtime <- newGraphicsRuntime context
  prepareGraphicsPipeline runtime compiled

newGeometry :: Context -> IO (Buffer '[ 'Buffer.Vertex] (V2 Float, V4 Float), Buffer '[ 'Buffer.Index] Word32)
newGeometry context = do
  vertexBuffer <- newBuffer context 4
  writeBuffer
    vertexBuffer
    0
    [ (V2 (-0.8) (-0.8), V4 1 0.15 0.1 1)
    , (V2 0.8 (-0.8), V4 0.1 0.8 0.25 1)
    , (V2 0.8 0.8, V4 0.1 0.35 1 1)
    , (V2 (-0.8) 0.8, V4 1 0.85 0.1 1)
    ]
  indexBuffer <- newBuffer context 6
  writeBuffer indexBuffer 0 [0, 1, 2, 0, 2, 3]
  pure (vertexBuffer, indexBuffer)
