{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Word (Word8)
import Linear (V2 (..), V4 (..))
import Vpipe.Context (Context)
import Vpipe.Expr qualified as Expr
import Vpipe.Format (Blendable, ColorRenderable, Format (B8G8R8A8Srgb, R8G8B8A8Unorm), KnownFormat)
import Vpipe.Frame (frame, frameColorTarget, render, renderTo)
import Vpipe.GLFW (Window, windowSurface)
import Vpipe.Graphics (PreparedGraphicsPipeline, newGraphicsRuntime, prepareGraphicsPipeline, renderGraphicsPipeline)
import Vpipe.Image (Image, ImageSubresource (..), imageExtent2D, newImage, writeImage)
import Vpipe.Image.Types (Dim (D2))
import Vpipe.Image.Types qualified as ImageTypes
import Vpipe.Pipeline qualified as Pipeline
import Vpipe.Sampler (defaultSamplerDescription, newSampler)
import Vpipe.Swapchain (PresentResult, defaultSwapchainConfig, newSwapchain)

import Vpipe.Examples.Common (ExampleOptions (exampleScreenshot), captureScreenshot, compilePipelineOrFail, newFullscreenQuad, newScreenshotTarget, offscreenFrameCount, parseExampleOptions, renderFrames, runWindowFrames, withExampleContext)

data Environment format = Environment
  { quad :: Pipeline.VertexBuffer (V2 Float, V2 Float)
  , checker :: Pipeline.TypedTextureBinding 'D2 'R8G8B8A8Unorm
  , target :: Pipeline.ColorImage format
  }

pipeline :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Pipeline.PipelineM (Environment format) ()
pipeline = do
  texture <- Pipeline.sampledTexture (Pipeline.sampledTextureSource "checker" checker)
  vertices <-
    Pipeline.vertexInput
      ( Pipeline.vertexSource "quad" quad ::
          Pipeline.VertexSource (Environment format) 'Pipeline.Triangles (V2 Float, V2 Float)
      )
  fragments <- Pipeline.rasterize Pipeline.defaultRaster (fmap fullscreenVertex vertices)
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "color" target)
    (fmap (\(Pipeline.Smooth uv) -> Expr.sample texture uv) fragments)

fullscreenVertex :: (Expr.V (V2 Float), Expr.V (V2 Float)) -> (Expr.V (V4 Float), Pipeline.Smooth 'Expr.Vertex (V2 Float))
fullscreenVertex (position, uv) =
  ( Expr.vec4 (Expr.x position) (Expr.y position) (Expr.constant 0) (Expr.constant 1)
  , Pipeline.Smooth uv
  )

main :: IO ()
main = do
  options <- parseExampleOptions
  case exampleScreenshot options of
    Just _ -> runScreenshot options
    Nothing -> runWindowFrames options "vpipe guide — part 4" runWindow

runScreenshot :: ExampleOptions -> IO ()
runScreenshot options = withExampleContext $ \context -> do
  prepared <- prepare context
  quadBuffer <- newFullscreenQuad context
  textureBinding <- newCheckerBinding context
  screenshot <- newScreenshotTarget context
  screenshotBinding <- Pipeline.colorImageBinding screenshot
  let environment = bindings (Pipeline.vertexBufferBinding quadBuffer) textureBinding screenshotBinding
  renderFrames (offscreenFrameCount options) (renderGraphicsPipeline prepared environment)
  _ <- captureScreenshot options screenshot
  pure ()

runWindow :: Context -> Window -> IO (IO PresentResult)
runWindow context window = do
  prepared <- prepare context
  quadBuffer <- newFullscreenQuad context
  textureBinding <- newCheckerBinding context
  swapchain <- newSwapchain context (windowSurface window) defaultSwapchainConfig
  pure $ frame swapchain $ \current -> do
    let color = frameColorTarget current
        environment = bindings (Pipeline.vertexBufferBinding quadBuffer) textureBinding color :: Environment 'B8G8R8A8Srgb
    renderTo color (render prepared environment)

bindings :: Pipeline.VertexBuffer (V2 Float, V2 Float) -> Pipeline.TypedTextureBinding 'D2 'R8G8B8A8Unorm -> Pipeline.ColorImage format -> Environment format
bindings quadBuffer textureBinding color =
  Environment
    { quad = quadBuffer
    , checker = textureBinding
    , target = color
    }

prepare :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Context -> IO (PreparedGraphicsPipeline (Environment format))
prepare context = do
  compiled <- compilePipelineOrFail "guide part 4" pipeline
  runtime <- newGraphicsRuntime context
  prepareGraphicsPipeline runtime compiled

newCheckerBinding :: Context -> IO (Pipeline.TypedTextureBinding 'D2 'R8G8B8A8Unorm)
newCheckerBinding context = do
  texture <-
    newImage context (imageExtent2D checkerEdge checkerEdge) 1 1 ::
      IO (Image 'D2 'R8G8B8A8Unorm '[ 'ImageTypes.Sampled, 'ImageTypes.CopyDst])
  writeImage texture (ImageSubresource 0 0) checkerPixels
  sampler <- newSampler context defaultSamplerDescription
  Pipeline.typedTextureBinding texture sampler

checkerEdge :: Int
checkerEdge = 8

checkerPixels :: [V4 Word8]
checkerPixels =
  [ if even (column `div` 2 + row `div` 2)
      then V4 245 180 35 255
      else V4 25 65 220 255
  | row <- [0 .. checkerEdge - 1]
  , column <- [0 .. checkerEdge - 1]
  ]
