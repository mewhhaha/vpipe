{-# LANGUAGE DataKinds #-}

module Main (main) where

import Linear (V3 (..), V4 (..))
import Vpipe.Buffer (Buffer, newBuffer, writeBuffer)
import Vpipe.Buffer qualified as Buffer
import Vpipe.Expr (V, constant, vec4, x, y, z)
import Vpipe.Format (Format (B8G8R8A8Srgb))
import Vpipe.Frame (frame, frameColorTarget, render, renderTo)
import Vpipe.Graphics (newGraphicsRuntime, prepareGraphicsPipeline)
import Vpipe.Pipeline (ColorImage, ColorTarget, PipelineM, PrimitiveTopology (Triangles), Smooth (..), VertexBuffer, VertexSource, colorTarget, defaultBlend, defaultRaster, drawColor, rasterize, vertexBufferBinding, vertexInput, vertexSource)
import Vpipe.Swapchain (defaultSwapchainConfig, newSwapchain)

import Vpipe.Examples.Common (ExampleOptions (exampleScreenshot), compilePipelineOrFail, parseExampleOptions, runOffscreenTriangle, runWindowFrames)
import Vpipe.GLFW (windowSurface)

data Environment = Environment {positions :: VertexBuffer (V3 Float), target :: ColorImage 'B8G8R8A8Srgb}

main :: IO ()
main = do
  options <- parseExampleOptions
  case exampleScreenshot options of
    Just _ -> runOffscreenTriangle options
    Nothing -> runWindowFrames options "vpipe triangle" $ \context window -> do
      compiled <- compilePipelineOrFail "triangle" pipeline
      runtime <- newGraphicsRuntime context
      prepared <- prepareGraphicsPipeline runtime compiled
      buffer <- newBuffer context 3 :: IO (Buffer '[ 'Buffer.Vertex] (V3 Float))
      writeBuffer buffer 0 [V3 (-0.8) (-0.8) 0, V3 0.8 (-0.8) 0, V3 0 0.8 0]
      swapchain <- newSwapchain context (windowSurface window) defaultSwapchainConfig
      pure $ frame swapchain $ \current -> renderTo (frameColorTarget current) (render prepared (Environment (vertexBufferBinding buffer) (frameColorTarget current)))

pipeline :: PipelineM Environment ()
pipeline = do
  input <- vertexInput (vertexSource "positions" positions :: VertexSource Environment 'Triangles (V3 Float))
  fragments <- rasterize defaultRaster (fmap vertex input)
  drawColor defaultBlend (colorTarget "color" target :: ColorTarget Environment 'B8G8R8A8Srgb) (fmap unSmooth fragments)
 where
  vertex position = (vec4 (x position) (y position) (z position) (constant 1), Smooth (constant (V4 1 0 0 1) :: V (V4 Float)))
