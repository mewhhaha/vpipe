{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Linear (V2 (..), V4 (..))
import Vpipe.Context (Context)
import Vpipe.Expr ((>.))
import Vpipe.Expr qualified as Expr
import Vpipe.Format (Blendable, ColorRenderable, Format (B8G8R8A8Srgb, D32Sfloat, R8G8B8A8Unorm), KnownFormat)
import Vpipe.Frame (frame, frameColorTarget, render, renderTo)
import Vpipe.GLFW (Window, windowSurface)
import Vpipe.Graphics (PreparedGraphicsPipeline, newGraphicsRuntime, prepareGraphicsPipeline, renderGraphicsPipeline)
import Vpipe.Image (Image, imageExtent2D, newImage)
import Vpipe.Image.Types (Dim (D2))
import Vpipe.Image.Types qualified as ImageTypes
import Vpipe.Pipeline qualified as Pipeline
import Vpipe.Sampler (defaultSamplerDescription, newSampler)
import Vpipe.Swapchain (PresentResult, defaultSwapchainConfig, newSwapchain)

import Vpipe.Examples.Common (ExampleOptions (exampleScreenshot), captureScreenshot, compilePipelineOrFail, newFullscreenQuad, newScreenshotTarget, offscreenFrameCount, parseExampleOptions, renderFrames, runWindowFrames, withExampleContext)

data OffscreenEnvironment = OffscreenEnvironment
  { offscreenQuad :: Pipeline.VertexBuffer (V2 Float, V2 Float)
  , offscreenColor :: Pipeline.ColorImage 'R8G8B8A8Unorm
  , offscreenDepth :: Pipeline.DepthImage 'D32Sfloat
  }

data PresentEnvironment format = PresentEnvironment
  { presentQuad :: Pipeline.VertexBuffer (V2 Float, V2 Float)
  , presentTexture :: Pipeline.TypedTextureBinding 'D2 'R8G8B8A8Unorm
  , presentColor :: Pipeline.ColorImage format
  }

offscreenPipeline :: Pipeline.PipelineM OffscreenEnvironment ()
offscreenPipeline = do
  vertices <-
    Pipeline.vertexInput
      ( Pipeline.vertexSource "offscreen-quad" offscreenQuad ::
          Pipeline.VertexSource OffscreenEnvironment 'Pipeline.Triangles (V2 Float, V2 Float)
      )
  fragments <- Pipeline.rasterize Pipeline.defaultRaster (fmap fullscreenVertex vertices)
  let uv = Pipeline.unSmooth (Pipeline.fragmentValue fragments)
      centered = uv - Expr.constant (V2 0.5 0.5)
      outsideCircle = Expr.dot centered centered >. Expr.constant 0.22
      visible = Pipeline.discardWhen outsideCircle fragments
      color =
        fmap
          ( \(Pipeline.Smooth coordinates) ->
              Expr.vec4
                (Expr.x coordinates)
                (Expr.constant 0.25)
                (Expr.y coordinates)
                (Expr.constant 1)
          )
          visible
      depth = fmap (const (Expr.constant 0.4)) visible
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "offscreen-color" offscreenColor)
    color
  Pipeline.drawDepth
    Pipeline.defaultDepth
    (Pipeline.depthTarget "offscreen-depth" offscreenDepth)
    depth

presentPipeline :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Pipeline.PipelineM (PresentEnvironment format) ()
presentPipeline = do
  texture <- Pipeline.sampledTexture (Pipeline.sampledTextureSource "offscreen-texture" presentTexture)
  vertices <-
    Pipeline.vertexInput
      ( Pipeline.vertexSource "present-quad" presentQuad ::
          Pipeline.VertexSource (PresentEnvironment format) 'Pipeline.Triangles (V2 Float, V2 Float)
      )
  fragments <- Pipeline.rasterize Pipeline.defaultRaster (fmap fullscreenVertex vertices)
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "present-color" presentColor)
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
    Nothing -> runWindowFrames options "vpipe guide — part 5" runWindow

runScreenshot :: ExampleOptions -> IO ()
runScreenshot options = withExampleContext $ \context -> do
  (offscreenPrepared, presentPrepared) <- prepare context
  resources <- newGuideResources context
  screenshot <- newScreenshotTarget context
  screenshotBinding <- Pipeline.colorImageBinding screenshot
  let (offscreenEnvironment, presentEnvironment) = environments resources screenshotBinding
      drawFrame = do
        renderGraphicsPipeline offscreenPrepared offscreenEnvironment
        renderGraphicsPipeline presentPrepared presentEnvironment
  renderFrames (offscreenFrameCount options) drawFrame
  _ <- captureScreenshot options screenshot
  pure ()

runWindow :: Context -> Window -> IO (IO PresentResult)
runWindow context window = do
  (offscreenPrepared, presentPrepared) <- prepare context
  resources <- newGuideResources context
  swapchain <- newSwapchain context (windowSurface window) defaultSwapchainConfig
  pure $ frame swapchain $ \current -> do
    let color = frameColorTarget current
        (offscreenEnvironment, presentEnvironment) = environments resources color :: (OffscreenEnvironment, PresentEnvironment 'B8G8R8A8Srgb)
    renderTo (offscreenColor offscreenEnvironment) (render offscreenPrepared offscreenEnvironment)
    renderTo color (render presentPrepared presentEnvironment)

data GuideResources = GuideResources
  { guideQuad :: Pipeline.VertexBuffer (V2 Float, V2 Float)
  , guideOffscreenColor :: Pipeline.ColorImage 'R8G8B8A8Unorm
  , guideOffscreenDepth :: Pipeline.DepthImage 'D32Sfloat
  , guideOffscreenTexture :: Pipeline.TypedTextureBinding 'D2 'R8G8B8A8Unorm
  }

newGuideResources :: Context -> IO GuideResources
newGuideResources context = do
  quadBuffer <- newFullscreenQuad context
  intermediate <-
    newImage context (imageExtent2D 64 64) 1 1 ::
      IO (Image 'D2 'R8G8B8A8Unorm '[ 'ImageTypes.ColorTarget, 'ImageTypes.Sampled])
  depth <-
    newImage context (imageExtent2D 64 64) 1 1 ::
      IO (Image 'D2 'D32Sfloat '[ 'ImageTypes.DepthTarget])
  intermediateColor <- Pipeline.colorImageBinding intermediate
  depthBinding <- Pipeline.depthImageBinding depth
  sampler <- newSampler context defaultSamplerDescription
  intermediateTexture <- Pipeline.typedTextureBinding intermediate sampler
  pure
    GuideResources
      { guideQuad = Pipeline.vertexBufferBinding quadBuffer
      , guideOffscreenColor = intermediateColor
      , guideOffscreenDepth = depthBinding
      , guideOffscreenTexture = intermediateTexture
      }

environments :: GuideResources -> Pipeline.ColorImage format -> (OffscreenEnvironment, PresentEnvironment format)
environments resources finalColor =
  ( OffscreenEnvironment
      { offscreenQuad = guideQuad resources
      , offscreenColor = guideOffscreenColor resources
      , offscreenDepth = guideOffscreenDepth resources
      }
  , PresentEnvironment
      { presentQuad = guideQuad resources
      , presentTexture = guideOffscreenTexture resources
      , presentColor = finalColor
      }
  )

prepare :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Context -> IO (PreparedGraphicsPipeline OffscreenEnvironment, PreparedGraphicsPipeline (PresentEnvironment format))
prepare context = do
  offscreenCompiled <- compilePipelineOrFail "guide part 5 offscreen" offscreenPipeline
  presentCompiled <- compilePipelineOrFail "guide part 5 present" presentPipeline
  runtime <- newGraphicsRuntime context
  offscreenPrepared <- prepareGraphicsPipeline runtime offscreenCompiled
  presentPrepared <- prepareGraphicsPipeline runtime presentCompiled
  pure (offscreenPrepared, presentPrepared)
