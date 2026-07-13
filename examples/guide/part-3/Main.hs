{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

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
import Vpipe.Graphics (PreparedGraphicsPipeline, newGraphicsRuntime, prepareGraphicsPipeline, renderGraphicsPipeline)
import Vpipe.Pipeline qualified as Pipeline
import Vpipe.Swapchain (PresentResult, defaultSwapchainConfig, newSwapchain)

import Vpipe.Examples.Common (ExampleOptions (exampleScreenshot), captureScreenshot, compilePipelineOrFail, newScreenshotTarget, offscreenFrameCount, parseExampleOptions, runWindowFrames, withExampleContext)

data Environment format = Environment
  { vertices :: Pipeline.VertexBuffer (V2 Float, V4 Float)
  , angle :: Pipeline.UniformBuffer Float
  , target :: Pipeline.ColorImage format
  }

pipeline :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Pipeline.PipelineM (Environment format) ()
pipeline = do
  rotation <-
    Pipeline.uniform (Pipeline.uniformSource "angle" angle) ::
      Pipeline.PipelineM (Environment format) (Expr.V Float)
  vertexStream <-
    Pipeline.vertexInput
      ( Pipeline.vertexSource "triangle" vertices ::
          Pipeline.VertexSource (Environment format) 'Pipeline.Triangles (V2 Float, V4 Float)
      )
  fragments <- Pipeline.rasterize Pipeline.defaultRaster (fmap (rotateVertex rotation) vertexStream)
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "color" target)
    (fmap Pipeline.unSmooth fragments)

rotateVertex :: Expr.V Float -> (Expr.V (V2 Float), Expr.V (V4 Float)) -> (Expr.V (V4 Float), Pipeline.Smooth 'Expr.Vertex (V4 Float))
rotateVertex rotation (position, color) =
  let cosine = cos rotation
      sine = sin rotation
      rotatedX = Expr.x position * cosine - Expr.y position * sine
      rotatedY = Expr.x position * sine + Expr.y position * cosine
   in ( Expr.vec4 rotatedX rotatedY (Expr.constant 0) (Expr.constant 1)
      , Pipeline.Smooth color
      )

main :: IO ()
main = do
  options <- parseExampleOptions
  case exampleScreenshot options of
    Just _ -> runScreenshot options
    Nothing -> runWindowFrames options "vpipe guide — part 3" runWindow

runScreenshot :: ExampleOptions -> IO ()
runScreenshot options = withExampleContext $ \context -> do
  prepared <- prepare context
  vertexBuffer <- newVertices context
  angleBuffer <- newAngleBuffer context
  screenshot <- newScreenshotTarget context
  screenshotBinding <- Pipeline.colorImageBinding screenshot
  let environment = bindings (Pipeline.vertexBufferBinding vertexBuffer) (Pipeline.uniformBufferBinding angleBuffer) screenshotBinding
  forM_ [0 .. offscreenFrameCount options - 1] $ \frameNumber -> do
    writeBuffer angleBuffer 0 [frameAngle frameNumber]
    renderGraphicsPipeline prepared environment
  _ <- captureScreenshot options screenshot
  pure ()

runWindow :: Context -> Window -> IO (IO PresentResult)
runWindow context window = do
  prepared <- prepare context
  vertexBuffer <- newVertices context
  angleBuffer <- newAngleBuffer context
  swapchain <- newSwapchain context (windowSurface window) defaultSwapchainConfig
  frameNumber <- newIORef 0
  pure $ do
    modifyIORef' frameNumber (+ 1)
    currentFrame <- readIORef frameNumber
    writeBuffer angleBuffer 0 [frameAngle currentFrame]
    frame swapchain $ \current -> do
      let color = frameColorTarget current
          environment = bindings (Pipeline.vertexBufferBinding vertexBuffer) (Pipeline.uniformBufferBinding angleBuffer) color :: Environment 'B8G8R8A8Srgb
      renderTo color (render prepared environment)

bindings :: Pipeline.VertexBuffer (V2 Float, V4 Float) -> Pipeline.UniformBuffer Float -> Pipeline.ColorImage format -> Environment format
bindings vertexBuffer angleBuffer color =
  Environment
    { vertices = vertexBuffer
    , angle = angleBuffer
    , target = color
    }

prepare :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Context -> IO (PreparedGraphicsPipeline (Environment format))
prepare context = do
  compiled <- compilePipelineOrFail "guide part 3" pipeline
  runtime <- newGraphicsRuntime context
  prepareGraphicsPipeline runtime compiled

newVertices :: Context -> IO (Buffer '[ 'Buffer.Vertex] (V2 Float, V4 Float))
newVertices context = do
  vertexBuffer <- newBuffer context 3
  writeBuffer
    vertexBuffer
    0
    [ (V2 0 0.78, V4 1 0.2 0.1 1)
    , (V2 (-0.68) (-0.48), V4 0.1 1 0.3 1)
    , (V2 0.68 (-0.48), V4 0.15 0.35 1 1)
    ]
  pure vertexBuffer

newAngleBuffer :: Context -> IO (Buffer '[ 'Buffer.Uniform] Float)
newAngleBuffer context = do
  angleBuffer <- newBuffer context 1
  writeBuffer angleBuffer 0 [0]
  pure angleBuffer

frameAngle :: Int -> Float
frameAngle frameNumber = fromIntegral frameNumber * 0.025
