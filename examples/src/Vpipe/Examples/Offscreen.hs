{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- | Two synchronous graphics passes demonstrate target/texture duality. The
first writes a UV gradient into an offscreen image; the second samples that
same image and darkens its edges with a fragment-side vignette.
-}
module Vpipe.Examples.Offscreen (runOffscreen) where

import Linear (V2 (..), V4 (..))
import Vpipe.Context (Context)
import Vpipe.Expr qualified as Expr
import Vpipe.Format (Blendable, ColorRenderable, Format (B8G8R8A8Srgb, R8G8B8A8Unorm), KnownFormat)
import Vpipe.Frame (frame, frameColorTarget, render, renderTo)
import Vpipe.GLFW (Window, windowSurface)
import Vpipe.Graphics (newGraphicsRuntime, prepareGraphicsPipeline, renderGraphicsPipeline)
import Vpipe.Image (Image, imageExtent2D, newImage)
import Vpipe.Image.Types (Dim (D2))
import Vpipe.Image.Types qualified as ImageTypes
import Vpipe.Pipeline qualified as Pipeline
import Vpipe.Sampler (defaultSamplerDescription, newSampler)
import Vpipe.Swapchain (PresentResult, defaultSwapchainConfig, newSwapchain)

import Vpipe.Examples.Common (ExampleOptions (exampleScreenshot), captureScreenshot, compilePipelineOrFail, newFullscreenQuad, newScreenshotTarget, offscreenFrameCount, renderFrames, runWindowFrames, withExampleContext)

data FirstPassEnvironment = FirstPassEnvironment
  { firstPassQuad :: Pipeline.VertexBuffer (V2 Float, V2 Float)
  , firstPassTarget :: Pipeline.ColorImage 'R8G8B8A8Unorm
  }

data VignetteEnvironment format = VignetteEnvironment
  { vignetteQuad :: Pipeline.VertexBuffer (V2 Float, V2 Float)
  , vignetteTexture :: Pipeline.TypedTextureBinding 'D2 'R8G8B8A8Unorm
  , vignetteTarget :: Pipeline.ColorImage format
  }

firstPassPipeline :: Pipeline.PipelineM FirstPassEnvironment ()
firstPassPipeline = do
  vertices <-
    Pipeline.vertexInput
      ( Pipeline.vertexSource "quad" firstPassQuad ::
          Pipeline.VertexSource FirstPassEnvironment 'Pipeline.Triangles (V2 Float, V2 Float)
      )
  fragments <- Pipeline.rasterize Pipeline.defaultRaster (fmap fullscreenVertex vertices)
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "offscreen" firstPassTarget)
    ( fmap
        (\(Pipeline.Smooth uv) -> Expr.vec4 (Expr.x uv) (Expr.y uv) (Expr.constant 0.35) (Expr.constant 1))
        fragments
    )

vignettePipeline :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Pipeline.PipelineM (VignetteEnvironment format) ()
vignettePipeline = do
  texture <- Pipeline.sampledTexture (Pipeline.sampledTextureSource "offscreen" vignetteTexture)
  vertices <-
    Pipeline.vertexInput
      ( Pipeline.vertexSource "quad" vignetteQuad ::
          Pipeline.VertexSource (VignetteEnvironment format) 'Pipeline.Triangles (V2 Float, V2 Float)
      )
  fragments <- Pipeline.rasterize Pipeline.defaultRaster (fmap fullscreenVertex vertices)
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "final" vignetteTarget)
    (fmap (shadeVignette texture) fragments)
 where
  shadeVignette texture (Pipeline.Smooth uv) =
    let centered = uv - Expr.constant (V2 0.5 0.5)
        radiusSquared = Expr.dot centered centered
        vignette = Expr.clamp (Expr.constant 1 - radiusSquared * Expr.constant 2.8) (Expr.constant 0.12) (Expr.constant 1)
        sampled = Expr.sample texture uv
     in sampled * Expr.vec4 vignette vignette vignette (Expr.constant 1)

fullscreenVertex :: (Expr.V (V2 Float), Expr.V (V2 Float)) -> (Expr.V (V4 Float), Pipeline.Smooth 'Expr.Vertex (V2 Float))
fullscreenVertex (position, uv) =
  (Expr.vec4 (Expr.x position) (Expr.y position) (Expr.constant 0) (Expr.constant 1), Pipeline.Smooth uv)

runOffscreen :: ExampleOptions -> IO ()
runOffscreen options = case exampleScreenshot options of
  Just _ -> runOffscreenScreenshot options
  Nothing -> runWindowFrames options "vpipe offscreen" runOffscreenWindow

runOffscreenScreenshot :: ExampleOptions -> IO ()
runOffscreenScreenshot options = withExampleContext $ \context -> do
  firstCompiled <- compilePipelineOrFail "offscreen first pass" firstPassPipeline
  vignetteCompiled <- compilePipelineOrFail "offscreen vignette pass" vignettePipeline
  runtime <- newGraphicsRuntime context
  firstPrepared <- prepareGraphicsPipeline runtime firstCompiled
  vignettePrepared <- prepareGraphicsPipeline runtime vignetteCompiled

  quad <- newFullscreenQuad context
  intermediate <- newIntermediateTarget context
  intermediateColor <- Pipeline.colorImageBinding intermediate
  sampler <- newSampler context defaultSamplerDescription
  intermediateTexture <- Pipeline.typedTextureBinding intermediate sampler
  finalTarget <- newScreenshotTarget context
  finalColor <- Pipeline.colorImageBinding finalTarget
  let firstEnvironment = FirstPassEnvironment (Pipeline.vertexBufferBinding quad) intermediateColor
      vignetteEnvironment = VignetteEnvironment (Pipeline.vertexBufferBinding quad) intermediateTexture finalColor :: VignetteEnvironment 'R8G8B8A8Unorm
      renderPasses = do
        renderGraphicsPipeline firstPrepared firstEnvironment
        renderGraphicsPipeline vignettePrepared vignetteEnvironment

  renderFrames (offscreenFrameCount options) renderPasses
  _ <- captureScreenshot options finalTarget
  pure ()

runOffscreenWindow :: Context -> Window -> IO (IO PresentResult)
runOffscreenWindow context window = do
  firstCompiled <- compilePipelineOrFail "offscreen first pass" firstPassPipeline
  vignetteCompiled <- compilePipelineOrFail "offscreen vignette pass" vignettePipeline
  runtime <- newGraphicsRuntime context
  firstPrepared <- prepareGraphicsPipeline runtime firstCompiled
  vignettePrepared <- prepareGraphicsPipeline runtime vignetteCompiled
  quad <- newFullscreenQuad context
  intermediate <- newIntermediateTarget context
  intermediateColor <- Pipeline.colorImageBinding intermediate
  sampler <- newSampler context defaultSamplerDescription
  intermediateTexture <- Pipeline.typedTextureBinding intermediate sampler
  swapchain <- newSwapchain context (windowSurface window) defaultSwapchainConfig
  let firstEnvironment = FirstPassEnvironment (Pipeline.vertexBufferBinding quad) intermediateColor
  pure $ frame swapchain $ \current -> do
    let target = frameColorTarget current
        vignetteEnvironment = VignetteEnvironment (Pipeline.vertexBufferBinding quad) intermediateTexture target :: VignetteEnvironment 'B8G8R8A8Srgb
    renderTo intermediateColor (render firstPrepared firstEnvironment)
    renderTo target (render vignettePrepared vignetteEnvironment)

newIntermediateTarget :: Context -> IO (Image 'D2 'R8G8B8A8Unorm '[ 'ImageTypes.ColorTarget, 'ImageTypes.Sampled])
newIntermediateTarget context = newImage context (imageExtent2D 64 64) 1 1
