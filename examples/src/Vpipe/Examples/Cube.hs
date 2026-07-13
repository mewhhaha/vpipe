{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- | A textured cube rendered from indexed face vertices. The uniform
matrix acts as the camera/projection transform while the model transform is a
push constant, keeping the two update rates visibly separate.
-}
module Vpipe.Examples.Cube (runCube) where

import Control.Monad (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Word (Word32, Word8)
import Linear (M44, V2 (..), V3 (..), V4 (..))
import Vpipe.Buffer (Buffer, newBuffer, writeBuffer)
import Vpipe.Buffer qualified as Buffer
import Vpipe.Buffer.Format qualified as BufferFormat
import Vpipe.Context (Context)
import Vpipe.Expr qualified as Expr
import Vpipe.Format (Blendable, ColorRenderable, Format (B8G8R8A8Srgb, D32Sfloat, R8G8B8A8Unorm), KnownFormat)
import Vpipe.Frame (frame, frameColorTarget, render, renderTo)
import Vpipe.GLFW (Window, windowSurface)
import Vpipe.Graphics (newGraphicsRuntime, prepareGraphicsPipeline, renderGraphicsPipeline)
import Vpipe.Image (Image, ImageSubresource (..), destroyImage, generateMips, imageExtent2D, newImage, writeImage)
import Vpipe.Image.Types (Dim (D2))
import Vpipe.Image.Types qualified as ImageTypes
import Vpipe.Pipeline qualified as Pipeline
import Vpipe.Sampler (defaultSamplerDescription, newSampler)
import Vpipe.Swapchain (PresentResult, defaultSwapchainConfig, newSwapchain, swapchainExtent)

import Vpipe.Examples.Common (ExampleOptions (exampleScreenshot), captureScreenshot, compilePipelineOrFail, newScreenshotTarget, offscreenFrameCount, runWindowFrames, withExampleContext)

data CubeEnvironment format = CubeEnvironment
  { cubeMesh :: Pipeline.VertexBuffer (V3 Float, V2 Float)
  , cubeIndexBuffer :: Pipeline.IndexBuffer
  , cubeMvp :: Pipeline.UniformBuffer (BufferFormat.MatrixBuffer 4 4 Float)
  , cubeModel :: BufferFormat.MatrixBuffer 4 4 Float
  , cubeTexture :: Pipeline.TypedTextureBinding 'D2 'R8G8B8A8Unorm
  , cubeColor :: Pipeline.ColorImage format
  , cubeDepth :: Pipeline.DepthImage 'D32Sfloat
  }

cubePipeline :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Pipeline.PipelineM (CubeEnvironment format) ()
cubePipeline = do
  texture <- Pipeline.sampledTexture (Pipeline.sampledTextureSource "checker" cubeTexture)
  mvp <- Pipeline.uniform (Pipeline.uniformSource "mvp" cubeMvp :: Pipeline.Uniform (CubeEnvironment format) (M44 Float))
  model <- Pipeline.pushConstant cubeModel :: Pipeline.PipelineM (CubeEnvironment format) (Expr.V (M44 Float))
  vertices <-
    Pipeline.vertexInput
      ( Pipeline.vertexSource "cube" cubeMesh ::
          Pipeline.VertexSource (CubeEnvironment format) 'Pipeline.Triangles (V3 Float, V2 Float)
      )
  fragments <-
    Pipeline.rasterizeIndexed
      Pipeline.defaultRaster
      (Pipeline.indexSource "cube-indices" cubeIndexBuffer)
      (fmap (projectVertex mvp model) vertices)
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "color" cubeColor)
    (fmap (\(Pipeline.Smooth uv, _) -> Expr.sample texture uv) fragments)
  Pipeline.drawDepth
    Pipeline.defaultDepth
    (Pipeline.depthTarget "depth" cubeDepth)
    (fmap (\(_, Pipeline.Smooth depth) -> depth) fragments)
 where
  projectVertex mvp model (position, uv) =
    let clip = mvp Expr.!* (model Expr.!* Expr.vec4 (Expr.x position) (Expr.y position) (Expr.z position) (Expr.constant 1))
     in (clip, (Pipeline.Smooth uv, Pipeline.Smooth (Expr.z clip)))

runCube :: ExampleOptions -> IO ()
runCube options = case exampleScreenshot options of
  Just _ -> runCubeScreenshot options
  Nothing -> runWindowFrames options "vpipe cube" runCubeWindow

runCubeScreenshot :: ExampleOptions -> IO ()
runCubeScreenshot options = withExampleContext $ \context -> do
  compiled <- compilePipelineOrFail "cube" cubePipeline
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled

  mesh <-
    newBuffer context (length cubeVertices) ::
      IO (Buffer '[ 'Buffer.Vertex] (V3 Float, V2 Float))
  writeBuffer mesh 0 cubeVertices
  indices <-
    newBuffer context (length cubeIndices) ::
      IO (Buffer '[ 'Buffer.Index] Word32)
  writeBuffer indices 0 cubeIndices
  mvp <-
    newBuffer context 1 ::
      IO (Buffer '[ 'Buffer.Uniform] (BufferFormat.MatrixBuffer 4 4 Float))

  texture <- newCheckerTexture context
  sampler <- newSampler context defaultSamplerDescription
  textureBinding <- Pipeline.typedTextureBinding texture sampler
  target <- newScreenshotTarget context
  targetBinding <- Pipeline.colorImageBinding target
  depth <- newDepthTarget context
  depthBinding <- Pipeline.depthImageBinding depth
  let environment =
        CubeEnvironment
          { cubeMesh = Pipeline.vertexBufferBinding mesh
          , cubeIndexBuffer = Pipeline.indexBufferBinding indices
          , cubeMvp = Pipeline.uniformBufferBinding mvp
          , cubeModel = BufferFormat.toMatrixBuffer modelMatrix
          , cubeTexture = textureBinding
          , cubeColor = targetBinding
          , cubeDepth = depthBinding
          }

  forM_ [0 .. offscreenFrameCount options - 1] $ \frameIndex -> do
    writeBuffer mvp 0 [BufferFormat.toMatrixBuffer (projectionMatrix frameIndex)]
    renderGraphicsPipeline prepared environment
  _ <- captureScreenshot options target
  pure ()

runCubeWindow :: Context -> Window -> IO (IO PresentResult)
runCubeWindow context window = do
  compiled <- compilePipelineOrFail "cube" cubePipeline
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  mesh <- newBuffer context (length cubeVertices) :: IO (Buffer '[ 'Buffer.Vertex] (V3 Float, V2 Float))
  writeBuffer mesh 0 cubeVertices
  indices <- newBuffer context (length cubeIndices) :: IO (Buffer '[ 'Buffer.Index] Word32)
  writeBuffer indices 0 cubeIndices
  mvp <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Uniform] (BufferFormat.MatrixBuffer 4 4 Float))
  texture <- newCheckerTexture context
  sampler <- newSampler context defaultSamplerDescription
  textureBinding <- Pipeline.typedTextureBinding texture sampler
  swapchain <- newSwapchain context (windowSurface window) defaultSwapchainConfig
  depthRef <- newIORef Nothing
  frameNumber <- newIORef 0
  let environment =
        CubeEnvironment (Pipeline.vertexBufferBinding mesh) (Pipeline.indexBufferBinding indices) (Pipeline.uniformBufferBinding mvp) (BufferFormat.toMatrixBuffer modelMatrix) textureBinding
  pure $ do
    modifyIORef' frameNumber (+ 1)
    currentFrame <- readIORef frameNumber
    writeBuffer mvp 0 [BufferFormat.toMatrixBuffer (projectionMatrix currentFrame)]
    extent <- swapchainExtent swapchain
    previous <- readIORef depthRef
    depthBinding <- case previous of
      Just (knownExtent, _, binding) | knownExtent == extent -> pure binding
      _ -> do
        let (width, height) = extent
        depth <- newImage context (imageExtent2D (fromIntegral width) (fromIntegral height)) 1 1 :: IO (Image 'D2 'D32Sfloat '[ 'ImageTypes.DepthTarget])
        binding <- Pipeline.depthImageBinding depth
        writeIORef depthRef (Just (extent, depth, binding))
        forM_ previous $ \(_, oldDepth, _) -> destroyImage oldDepth
        pure binding
    frame swapchain $ \current -> do
      let target = frameColorTarget current
      renderTo target (render prepared (environment target depthBinding :: CubeEnvironment 'B8G8R8A8Srgb))

newCheckerTexture :: Context -> IO (Image 'D2 'R8G8B8A8Unorm '[ 'ImageTypes.Sampled, 'ImageTypes.CopySrc, 'ImageTypes.CopyDst])
newCheckerTexture context = do
  texture <- newImage context (imageExtent2D textureEdge textureEdge) 4 1
  writeImage texture (ImageSubresource 0 0) checkerPixels
  generateMips texture 0
  pure texture

newDepthTarget :: Context -> IO (Image 'D2 'D32Sfloat '[ 'ImageTypes.DepthTarget])
newDepthTarget context = newImage context (imageExtent2D 64 64) 1 1

textureEdge :: Int
textureEdge = 8

checkerPixels :: [V4 Word8]
checkerPixels =
  [ if even (column `div` 2 + row `div` 2)
      then V4 255 190 32 255
      else V4 25 80 230 255
  | row <- [0 .. textureEdge - 1]
  , column <- [0 .. textureEdge - 1]
  ]

projectionMatrix :: Int -> M44 Float
projectionMatrix frameIndex =
  V4
    (V4 scale 0 0 0)
    (V4 0 scale 0 0)
    (V4 0 0 0.5 0.5)
    (V4 0 0 0 1)
 where
  -- Frame 1 retains the established two-frame golden while frame 0 exercises
  -- a distinct uniform upload.
  scale = 1.2 + 0.05 * fromIntegral frameIndex

modelMatrix :: M44 Float
modelMatrix =
  let angleX = 0.48
      angleY = 0.67
      sineX = sin angleX
      cosineX = cos angleX
      sineY = sin angleY
      cosineY = cos angleY
   in V4
        (V4 cosineY (sineY * sineX) (sineY * cosineX) 0)
        (V4 0 cosineX (-sineX) 0)
        (V4 (-sineY) (cosineY * sineX) (cosineY * cosineX) 0)
        (V4 0 0 0 1)

cubeVertices :: [(V3 Float, V2 Float)]
cubeVertices =
  concat
    [ face (V3 (-0.5) (-0.5) 0.5) (V3 0.5 (-0.5) 0.5) (V3 0.5 0.5 0.5) (V3 (-0.5) 0.5 0.5)
    , face (V3 0.5 (-0.5) (-0.5)) (V3 (-0.5) (-0.5) (-0.5)) (V3 (-0.5) 0.5 (-0.5)) (V3 0.5 0.5 (-0.5))
    , face (V3 (-0.5) (-0.5) (-0.5)) (V3 (-0.5) (-0.5) 0.5) (V3 (-0.5) 0.5 0.5) (V3 (-0.5) 0.5 (-0.5))
    , face (V3 0.5 (-0.5) 0.5) (V3 0.5 (-0.5) (-0.5)) (V3 0.5 0.5 (-0.5)) (V3 0.5 0.5 0.5)
    , face (V3 (-0.5) 0.5 0.5) (V3 0.5 0.5 0.5) (V3 0.5 0.5 (-0.5)) (V3 (-0.5) 0.5 (-0.5))
    , face (V3 (-0.5) (-0.5) (-0.5)) (V3 0.5 (-0.5) (-0.5)) (V3 0.5 (-0.5) 0.5) (V3 (-0.5) (-0.5) 0.5)
    ]
 where
  face lowerLeft lowerRight upperRight upperLeft =
    [ (lowerLeft, V2 0 0)
    , (lowerRight, V2 1 0)
    , (upperRight, V2 1 1)
    , (upperLeft, V2 0 1)
    ]

cubeIndices :: [Word32]
cubeIndices = concatMap faceIndices [0, 4 .. 20]
 where
  faceIndices offset = [offset, offset + 1, offset + 2, offset, offset + 2, offset + 3]
