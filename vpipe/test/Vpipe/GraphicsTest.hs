{-# LANGUAGE DataKinds #-}

module Vpipe.GraphicsTest (graphicsTests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, throwIO, try)
import Control.Monad (forM_, replicateM_)
import Data.Bifunctor (bimap)
import Data.List (isInfixOf)
import Data.Word (Word32, Word8)
import Linear (V2 (..), V3 (..), V4 (..))
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getFileSize, getTemporaryDirectory, listDirectory, removeFile, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Vpipe.Buffer (Buffer, destroyBuffer, newBuffer, writeBuffer)
import Vpipe.Buffer qualified as Buffer
import Vpipe.Context (Context, VpipeConfig (vpipeLogger, vpipeValidationStrict), defaultVpipeConfig, withVpipe)
import Vpipe.Context.Internal (contextAllocationCountForTest)
import Vpipe.Descriptor.Internal (DescriptorStats (..), descriptorStats)
import Vpipe.Error (VpipeError (NoVulkanIcd, VulkanFailure))
import Vpipe.Expr
import Vpipe.Expr.Reify (ReifiedForest (..))
import Vpipe.Format (Format (D32Sfloat, R8G8B8A8Unorm))
import Vpipe.Graphics
import Vpipe.Graphics.Frame.Internal (PreparedGraphicsPipeline (..))
import Vpipe.Image (Image, ImageSubresource (..), destroyImage, generateMips, imageExtent2D, imageExtent2DArray, imageExtentCube, newImage, readImage, writeImage)
import Vpipe.Image.Types (Dim (Cube, D2, D2Array))
import Vpipe.Image.Types qualified as ImageTypes
import Vpipe.Pipeline.Internal
import Vpipe.Sampler (AddressMode (ClampToEdge), Filter (Nearest), MipmapMode (NearestMipmap), SamplerDescription (..), defaultSamplerDescription, newSampler)

graphicsTests :: TestTree
graphicsTests =
  testGroup
    "graphics"
    [ testCase "preparing an EDSL pipeline twice reuses its compiled modules" cachedTypedTriangle
    , testCase "a native pipeline cache survives a context restart and rehydrates a rendered pipeline" nativePipelineCacheRestartTest
    , testCase "static state changes pipeline identity without duplicating shader modules" staticStateCacheTest
    , testCase "one thousand vertex-buffer uploads interleave with draws without retaining allocations" interleavedBufferDrawStressTest
    , testCase "two uniforms a texture and a push constant affect one rendered pixel" mixedResourceRenderTest
    , testCase "an 8x8 upload samples into an exact 8x8 rendered fixture" textureUploadFixtureTest
    , testCase "a cube texture samples a generated mip at an explicit LOD" cubeTextureMipRenderTest
    , testCase "a multi-mip texture and depth target render together" textureAndDepthTest
    , testCase "a D2Array texture samples a nonzero layer" arrayLayerTextureTest
    , testCase "one draw writes multiple color targets" multipleColorTargetsTest
    , testCase "sequential draws preserve the first target contents" sequentialDrawLoadTest
    , testCase "an indexed draw reorders and repeats Word32 vertices" indexedDrawTest
    , testCase "a uniform-only buffer forged as a vertex source is rejected before recording" forgedVertexUsageTest
    , testCase "a sampled-only image forged as a color target is rejected before recording" forgedColorUsageTest
    , testCase "a later raw sampler is rejected before earlier graphics resources are leased" laterRawSamplerPrevalidationTest
    ]

data GraphicsEnvironment = GraphicsEnvironment
  { environmentPositions :: VertexBuffer (V3 Float)
  , environmentIntensity :: UniformBuffer Float
  , environmentTarget :: ColorImage 'R8G8B8A8Unorm
  , environmentPush :: Float
  }

positionsSource :: VertexSource GraphicsEnvironment 'Triangles (V3 Float)
positionsSource = vertexSource "positions" environmentPositions

intensitySource :: Uniform GraphicsEnvironment Float
intensitySource = uniformSource "intensity" environmentIntensity

colorTargetSource :: ColorTarget GraphicsEnvironment 'R8G8B8A8Unorm
colorTargetSource = colorTarget "color" environmentTarget

typedTriangle :: PipelineM GraphicsEnvironment ()
typedTriangle = typedTriangleWithRaster defaultRaster

typedTriangleWithRaster :: Raster -> PipelineM GraphicsEnvironment ()
typedTriangleWithRaster raster = do
  intensity <- uniform intensitySource
  pushed <- pushConstant environmentPush
  positions <- vertexInput positionsSource
  fragments <-
    rasterize
      raster
      ( fmap
          (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (constant (0 :: Float) :: V Float)))
          positions
      )
  drawColor
    defaultBlend
    colorTargetSource
    (fmap (const (vec4 (intensity + pushed) (constant 0) (constant 0) (constant 1))) fragments)

cachedTypedTriangle :: IO ()
cachedTypedTriangle = withTestContext $ \context -> do
  compiled <- compileSuccessfully typedTriangle
  let unavailableForest = ReifiedForest [] [] []
      retained =
        compiled
          { compiledPipelineDraws =
              [ draw{compiledVertexForest = unavailableForest, compiledFragmentForest = unavailableForest}
              | draw <- compiledPipelineDraws compiled
              ]
          }
  firstRuntime <- newGraphicsRuntime context
  secondRuntime <- newGraphicsRuntime context
  firstPrepared <- prepareGraphicsPipeline firstRuntime retained
  secondPrepared <- prepareGraphicsPipeline secondRuntime retained
  positions <- newBuffer context 3 :: IO (Buffer '[ 'Buffer.Vertex] (V3 Float))
  writeBuffer positions 0 [V3 (-0.8) (-0.8) 0, V3 0.8 (-0.8) 0, V3 0 0.8 0]
  intensity <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Uniform] Float)
  writeBuffer intensity 0 [0.75]
  firstTarget <- newTarget context 32 32
  secondTarget <- newTarget context 48 24
  firstColor <- colorImageBinding firstTarget
  secondColor <- colorImageBinding secondTarget
  let common target =
        GraphicsEnvironment
          { environmentPositions = vertexBufferBinding positions
          , environmentIntensity = uniformBufferBinding intensity
          , environmentTarget = target
          , environmentPush = 0.25
          }

  renderGraphicsPipeline firstPrepared (common firstColor)
  assertTrianglePixels 32 32 =<< readImage firstTarget (ImageSubresource 0 0)
  renderGraphicsPipeline firstPrepared (common firstColor)
  renderGraphicsPipeline secondPrepared (common secondColor)
  assertTrianglePixels 48 24 =<< readImage secondTarget (ImageSubresource 0 0)

  graphicsStats firstRuntime >>= (@?= GraphicsStats 2 1)
  graphicsStats secondRuntime >>= (@?= GraphicsStats 2 1)

nativePipelineCacheRestartTest :: IO ()
nativePipelineCacheRestartTest =
  withIsolatedPipelineCache $ \cacheDirectory -> do
    withTestContext renderPipelineCacheFixture
    firstArtifact <- persistedPipelineCacheArtifact cacheDirectory
    withTestContext renderPipelineCacheFixture
    secondArtifact <- persistedPipelineCacheArtifact cacheDirectory
    secondArtifact @?= firstArtifact

renderPipelineCacheFixture :: Context -> IO ()
renderPipelineCacheFixture context = do
  compiled <- compileSuccessfully typedTriangle
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  positions <- newTriangleBuffer context
  intensity <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Uniform] Float)
  writeBuffer intensity 0 [0.75]
  target <- newTarget context 8 8
  targetBinding <- colorImageBinding target
  renderGraphicsPipeline
    prepared
    GraphicsEnvironment
      { environmentPositions = vertexBufferBinding positions
      , environmentIntensity = uniformBufferBinding intensity
      , environmentTarget = targetBinding
      , environmentPush = 0.25
      }
  assertTrianglePixels 8 8 =<< readImage target (ImageSubresource 0 0)

staticStateCacheTest :: IO ()
staticStateCacheTest = withTestContext $ \context -> do
  defaultCompiled <- compileSuccessfully typedTriangle
  culledCompiled <- compileSuccessfully (typedTriangleWithRaster defaultRaster{cullMode = CullFront})
  runtime <- newGraphicsRuntime context
  defaultPrepared <- prepareGraphicsPipeline runtime defaultCompiled
  culledPrepared <- prepareGraphicsPipeline runtime culledCompiled
  positions <- newTriangleBuffer context
  intensity <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Uniform] Float)
  writeBuffer intensity 0 [1]
  target <- newTarget context 16 16
  targetBinding <- colorImageBinding target
  let environment =
        GraphicsEnvironment
          { environmentPositions = vertexBufferBinding positions
          , environmentIntensity = uniformBufferBinding intensity
          , environmentTarget = targetBinding
          , environmentPush = 0
          }
  renderGraphicsPipeline defaultPrepared environment
  renderGraphicsPipeline culledPrepared environment
  graphicsStats runtime >>= (@?= GraphicsStats 2 2)

interleavedBufferDrawStressTest :: IO ()
interleavedBufferDrawStressTest = withTestContext $ \context -> do
  compiled <- compileSuccessfully typedTriangle
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  intensity <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Uniform] Float)
  writeBuffer intensity 0 [1]
  target <- newTarget context 8 8
  targetBinding <- colorImageBinding target
  baselineAllocations <- contextAllocationCountForTest context
  let environment positions =
        GraphicsEnvironment
          { environmentPositions = vertexBufferBinding positions
          , environmentIntensity = uniformBufferBinding intensity
          , environmentTarget = targetBinding
          , environmentPush = 0
          }
      triangle = [V3 (-0.8) (-0.8) 0, V3 0.8 (-0.8) 0, V3 0 0.8 0]
  replicateM_ 1000 $
    bracket
      (newBuffer context 3 :: IO (Buffer '[ 'Buffer.Vertex] (V3 Float)))
      destroyBuffer
      ( \positions -> do
          writeBuffer positions 0 triangle
          renderGraphicsPipeline prepared (environment positions)
      )
  contextAllocationCountForTest context >>= (@?= baselineAllocations)
  pixels <- readImage target (ImageSubresource 0 0)
  pixelAt 8 4 4 pixels @?= V4 255 0 0 255

data MixedResourceEnvironment = MixedResourceEnvironment
  { mixedResourcePositions :: VertexBuffer (V3 Float)
  , mixedResourceFirstUniform :: UniformBuffer Float
  , mixedResourceSecondUniform :: UniformBuffer Float
  , mixedResourceTexture :: TypedTextureBinding 'D2 'R8G8B8A8Unorm
  , mixedResourceTarget :: ColorImage 'R8G8B8A8Unorm
  , mixedResourcePush :: Float
  }

mixedResourcePipeline :: PipelineM MixedResourceEnvironment ()
mixedResourcePipeline = do
  firstUniform <- uniform (uniformSource "first" mixedResourceFirstUniform)
  secondUniform <- uniform (uniformSource "second" mixedResourceSecondUniform)
  sampled <- sampledTexture (sampledTextureSource "texture" mixedResourceTexture)
  pushed <- pushConstant mixedResourcePush
  positions <- vertexInput (vertexSource "positions" mixedResourcePositions :: VertexSource MixedResourceEnvironment 'Triangles (V3 Float))
  fragments <- rasterize defaultRaster (fmap clipWithDummy positions)
  drawColor
    defaultBlend
    (colorTarget "color" mixedResourceTarget)
    (fmap (const (vec4 firstUniform secondUniform (z (sampleLod sampled (vec2 (constant 0.5) (constant 0.5)) (constant 0))) pushed)) fragments)

mixedResourceRenderTest :: IO ()
mixedResourceRenderTest = withTestContext $ \context -> do
  compiled <- compileSuccessfully mixedResourcePipeline
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  positions <- newFullscreenPositionBuffer context
  firstUniform <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Uniform] Float)
  secondUniform <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Uniform] Float)
  writeBuffer firstUniform 0 [64 / 255]
  writeBuffer secondUniform 0 [128 / 255]
  textureImage <- newImage context (imageExtent2D 1 1) 1 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'ImageTypes.Sampled, 'ImageTypes.CopyDst])
  writeImage textureImage (ImageSubresource 0 0) [V4 0 0 192 255]
  textureSampler <- newSampler context nearestClampedSampler
  sampledTextureBinding <- typedTextureBinding textureImage textureSampler
  target <- newTarget context 8 8
  targetBinding <- colorImageBinding target
  renderGraphicsPipeline
    prepared
    MixedResourceEnvironment
      { mixedResourcePositions = vertexBufferBinding positions
      , mixedResourceFirstUniform = uniformBufferBinding firstUniform
      , mixedResourceSecondUniform = uniformBufferBinding secondUniform
      , mixedResourceTexture = sampledTextureBinding
      , mixedResourceTarget = targetBinding
      , mixedResourcePush = 224 / 255
      }
  pixels <- readImage target (ImageSubresource 0 0)
  pixelAt 8 4 4 pixels @?= V4 64 128 192 224

data TextureFixtureEnvironment = TextureFixtureEnvironment
  { textureFixtureVertices :: VertexBuffer (V3 Float, V2 Float)
  , textureFixtureTexture :: TypedTextureBinding 'D2 'R8G8B8A8Unorm
  , textureFixtureTarget :: ColorImage 'R8G8B8A8Unorm
  }

textureFixturePipeline :: PipelineM TextureFixtureEnvironment ()
textureFixturePipeline = do
  vertices <- vertexInput (vertexSource "vertices" textureFixtureVertices :: VertexSource TextureFixtureEnvironment 'Triangles (V3 Float, V2 Float))
  sampled <- sampledTexture (sampledTextureSource "texture" textureFixtureTexture)
  let projected = fmap (bimap clipWithW Smooth) vertices
  fragments <- rasterize defaultRaster projected
  drawColor
    defaultBlend
    (colorTarget "color" textureFixtureTarget)
    (fmap (\(Smooth uv) -> sampleLod sampled uv (constant 0)) fragments)

textureUploadFixtureTest :: IO ()
textureUploadFixtureTest = withTestContext $ \context -> do
  compiled <- compileSuccessfully textureFixturePipeline
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  vertices <- newFullscreenTextureVertexBuffer context
  textureImage <- newImage context (imageExtent2D 8 8) 1 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'ImageTypes.Sampled, 'ImageTypes.CopyDst])
  let uploadedPixels = textureFixturePixels
  writeImage textureImage (ImageSubresource 0 0) uploadedPixels
  textureSampler <- newSampler context nearestClampedSampler
  sampledTextureBinding <- typedTextureBinding textureImage textureSampler
  target <- newTarget context 8 8
  targetBinding <- colorImageBinding target
  renderGraphicsPipeline
    prepared
    TextureFixtureEnvironment
      { textureFixtureVertices = vertexBufferBinding vertices
      , textureFixtureTexture = sampledTextureBinding
      , textureFixtureTarget = targetBinding
      }
  renderedPixels <- readImage target (ImageSubresource 0 0)
  renderedPixels @?= uploadedPixels

data CubeTextureEnvironment = CubeTextureEnvironment
  { cubeTexturePositions :: VertexBuffer (V3 Float)
  , cubeTextureBinding :: TypedTextureBinding 'Cube 'R8G8B8A8Unorm
  , cubeTextureTarget :: ColorImage 'R8G8B8A8Unorm
  }

cubeTextureMipPipeline :: PipelineM CubeTextureEnvironment ()
cubeTextureMipPipeline = do
  sampled <- sampledTexture (sampledTextureSource "cube" cubeTextureBinding)
  positions <- vertexInput (vertexSource "positions" cubeTexturePositions :: VertexSource CubeTextureEnvironment 'Triangles (V3 Float))
  fragments <- rasterize defaultRaster (fmap clipWithDummy positions)
  drawColor
    defaultBlend
    (colorTarget "color" cubeTextureTarget)
    (fmap (const (sampleLod sampled (vec3 (constant 1) (constant 0) (constant 0)) (constant 1))) fragments)

cubeTextureMipRenderTest :: IO ()
cubeTextureMipRenderTest = withTestContext $ \context -> do
  compiled <- compileSuccessfully cubeTextureMipPipeline
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  positions <- newFullscreenPositionBuffer context
  cube <- newImage context (imageExtentCube 4) 3 6 :: IO (Image 'Cube 'R8G8B8A8Unorm '[ 'ImageTypes.Sampled, 'ImageTypes.CopySrc, 'ImageTypes.CopyDst])
  mapM_ (uploadCubeFace cube) (zip [0 ..] cubeFacePixels)
  mapM_ (generateMips cube) [0 .. 5]
  cubeSampler <- newSampler context nearestClampedSampler
  cubeBinding <- typedTextureBinding cube cubeSampler
  target <- newTarget context 8 8
  targetBinding <- colorImageBinding target
  renderGraphicsPipeline
    prepared
    CubeTextureEnvironment
      { cubeTexturePositions = vertexBufferBinding positions
      , cubeTextureBinding = cubeBinding
      , cubeTextureTarget = targetBinding
      }
  pixels <- readImage target (ImageSubresource 0 0)
  pixelAt 8 4 4 pixels @?= V4 255 0 0 255

data TextureDepthEnvironment = TextureDepthEnvironment
  { textureDepthPositions :: VertexBuffer (V3 Float)
  , textureDepthTexture :: TypedTextureBinding 'D2 'R8G8B8A8Unorm
  , textureDepthColor :: ColorImage 'R8G8B8A8Unorm
  , textureDepthDepth :: DepthImage 'D32Sfloat
  }

textureDepthPipeline :: PipelineM TextureDepthEnvironment ()
textureDepthPipeline = do
  sampled <- sampledTexture (sampledTextureSource "sampled" textureDepthTexture)
  positions <- vertexInput (vertexSource "positions" textureDepthPositions :: VertexSource TextureDepthEnvironment 'Triangles (V3 Float))
  fragments <- rasterize defaultRaster (fmap clipWithDummy positions)
  drawColor
    defaultBlend
    (colorTarget "color" textureDepthColor)
    (fmap (const (sampleLod sampled (vec2 (constant 0.5) (constant 0.5)) (constant 1))) fragments)
  drawDepth
    defaultDepth
    (depthTarget "depth" textureDepthDepth)
    (fmap (const (constant 0.5)) fragments)

textureAndDepthTest :: IO ()
textureAndDepthTest = withTestContext $ \context -> do
  compiled <- compileSuccessfully textureDepthPipeline
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  positions <- newTriangleBuffer context
  textureImage <- newImage context (imageExtent2D 2 2) 2 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'ImageTypes.Sampled, 'ImageTypes.CopyDst])
  writeImage textureImage (ImageSubresource 0 0) (replicate 4 (V4 255 0 0 255))
  writeImage textureImage (ImageSubresource 1 0) [V4 0 255 0 255]
  textureSampler <- newSampler context defaultSamplerDescription
  sampledBinding <- typedTextureBinding textureImage textureSampler
  colorTargetImage <- newTarget context 24 24
  colorBinding <- colorImageBinding colorTargetImage
  depthTargetImage <- newImage context (imageExtent2D 24 24) 1 1 :: IO (Image 'D2 'D32Sfloat '[ 'ImageTypes.DepthTarget, 'ImageTypes.CopySrc])
  depthBinding <- depthImageBinding depthTargetImage
  renderGraphicsPipeline
    prepared
    TextureDepthEnvironment
      { textureDepthPositions = vertexBufferBinding positions
      , textureDepthTexture = sampledBinding
      , textureDepthColor = colorBinding
      , textureDepthDepth = depthBinding
      }
  colorPixels <- readImage colorTargetImage (ImageSubresource 0 0)
  pixelAt 24 12 12 colorPixels @?= V4 0 255 0 255
  depthPixels <- readImage depthTargetImage (ImageSubresource 0 0)
  pixelAt 24 12 12 depthPixels @?= 0.5
  pixelAt 24 0 0 depthPixels @?= 1

data ArrayTextureEnvironment = ArrayTextureEnvironment
  { arrayTexturePositions :: VertexBuffer (V3 Float)
  , arrayTextureBinding :: TypedTextureBinding 'D2Array 'R8G8B8A8Unorm
  , arrayTextureTarget :: ColorImage 'R8G8B8A8Unorm
  }

arrayTexturePipeline :: PipelineM ArrayTextureEnvironment ()
arrayTexturePipeline = do
  sampled <- sampledTexture (sampledTextureSource "array" arrayTextureBinding)
  positions <- vertexInput (vertexSource "positions" arrayTexturePositions :: VertexSource ArrayTextureEnvironment 'Triangles (V3 Float))
  fragments <- rasterize defaultRaster (fmap clipWithDummy positions)
  drawColor
    defaultBlend
    (colorTarget "color" arrayTextureTarget)
    (fmap (const (sampleLod sampled (vec3 (constant 0.5) (constant 0.5) (constant 1)) (constant 0))) fragments)

arrayLayerTextureTest :: IO ()
arrayLayerTextureTest = withTestContext $ \context -> do
  compiled <- compileSuccessfully arrayTexturePipeline
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  positions <- newTriangleBuffer context
  textureImage <- newImage context (imageExtent2DArray 1 1) 1 2 :: IO (Image 'D2Array 'R8G8B8A8Unorm '[ 'ImageTypes.Sampled, 'ImageTypes.CopyDst])
  writeImage textureImage (ImageSubresource 0 0) [V4 255 0 0 255]
  writeImage textureImage (ImageSubresource 0 1) [V4 0 0 255 255]
  textureSampler <- newSampler context defaultSamplerDescription
  sampledBinding <- typedTextureBinding textureImage textureSampler
  target <- newTarget context 20 20
  targetBinding <- colorImageBinding target
  renderGraphicsPipeline
    prepared
    ArrayTextureEnvironment
      { arrayTexturePositions = vertexBufferBinding positions
      , arrayTextureBinding = sampledBinding
      , arrayTextureTarget = targetBinding
      }
  pixels <- readImage target (ImageSubresource 0 0)
  pixelAt 20 10 10 pixels @?= V4 0 0 255 255

data MultiTargetEnvironment = MultiTargetEnvironment
  { multiTargetPositions :: VertexBuffer (V3 Float)
  , multiTargetFirst :: ColorImage 'R8G8B8A8Unorm
  , multiTargetSecond :: ColorImage 'R8G8B8A8Unorm
  }

multipleColorPipeline :: PipelineM MultiTargetEnvironment ()
multipleColorPipeline = do
  positions <- vertexInput (vertexSource "positions" multiTargetPositions :: VertexSource MultiTargetEnvironment 'Triangles (V3 Float))
  fragments <- rasterize defaultRaster (fmap clipWithDummy positions)
  drawColor defaultBlend (colorTarget "first" multiTargetFirst) (fmap (const (vec4 (constant 1) (constant 0) (constant 0) (constant 1))) fragments)
  drawColor defaultBlend (colorTarget "second" multiTargetSecond) (fmap (const (vec4 (constant 0) (constant 1) (constant 0) (constant 1))) fragments)

multipleColorTargetsTest :: IO ()
multipleColorTargetsTest = withTestContext $ \context -> do
  compiled <- compileSuccessfully multipleColorPipeline
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  positions <- newTriangleBuffer context
  first <- newTarget context 18 18
  second <- newTarget context 18 18
  firstBinding <- colorImageBinding first
  secondBinding <- colorImageBinding second
  renderGraphicsPipeline
    prepared
    MultiTargetEnvironment
      { multiTargetPositions = vertexBufferBinding positions
      , multiTargetFirst = firstBinding
      , multiTargetSecond = secondBinding
      }
  firstPixels <- readImage first (ImageSubresource 0 0)
  secondPixels <- readImage second (ImageSubresource 0 0)
  pixelAt 18 9 9 firstPixels @?= V4 255 0 0 255
  pixelAt 18 9 9 secondPixels @?= V4 0 255 0 255

data SequentialEnvironment = SequentialEnvironment
  { sequentialFirstPositions :: VertexBuffer (V3 Float)
  , sequentialSecondPositions :: VertexBuffer (V3 Float)
  , sequentialTarget :: ColorImage 'R8G8B8A8Unorm
  }

sequentialPipeline :: PipelineM SequentialEnvironment ()
sequentialPipeline = do
  firstPositions <- vertexInput (vertexSource "first-positions" sequentialFirstPositions :: VertexSource SequentialEnvironment 'Triangles (V3 Float))
  firstFragments <- rasterize defaultRaster (fmap clipWithDummy firstPositions)
  drawColor defaultBlend (colorTarget "color" sequentialTarget) (fmap (const (vec4 (constant 1) (constant 0) (constant 0) (constant 1))) firstFragments)
  secondPositions <- vertexInput (vertexSource "second-positions" sequentialSecondPositions :: VertexSource SequentialEnvironment 'Triangles (V3 Float))
  secondFragments <- rasterize defaultRaster (fmap clipWithDummy secondPositions)
  drawColor defaultBlend (colorTarget "color" sequentialTarget) (fmap (const (vec4 (constant 0) (constant 1) (constant 0) (constant 1))) secondFragments)

sequentialDrawLoadTest :: IO ()
sequentialDrawLoadTest = withTestContext $ \context -> do
  compiled <- compileSuccessfully sequentialPipeline
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  firstPositions <- newBuffer context 3 :: IO (Buffer '[ 'Buffer.Vertex] (V3 Float))
  writeBuffer firstPositions 0 [V3 (-0.95) (-0.8) 0, V3 0 (-0.8) 0, V3 (-0.5) 0.8 0]
  secondPositions <- newBuffer context 3 :: IO (Buffer '[ 'Buffer.Vertex] (V3 Float))
  writeBuffer secondPositions 0 [V3 0 (-0.8) 0, V3 0.95 (-0.8) 0, V3 0.5 0.8 0]
  target <- newTarget context 32 24
  targetBinding <- colorImageBinding target
  renderGraphicsPipeline
    prepared
    SequentialEnvironment
      { sequentialFirstPositions = vertexBufferBinding firstPositions
      , sequentialSecondPositions = vertexBufferBinding secondPositions
      , sequentialTarget = targetBinding
      }
  pixels <- readImage target (ImageSubresource 0 0)
  pixelAt 32 8 12 pixels @?= V4 255 0 0 255
  pixelAt 32 24 12 pixels @?= V4 0 255 0 255

data IndexedEnvironment = IndexedEnvironment
  { indexedPositions :: VertexBuffer (V3 Float)
  , indexedIndices :: IndexBuffer
  , indexedTarget :: ColorImage 'R8G8B8A8Unorm
  }

indexedPipeline :: Bool -> PipelineM IndexedEnvironment ()
indexedPipeline useIndices = do
  positions <- vertexInput (vertexSource "positions" indexedPositions :: VertexSource IndexedEnvironment 'Triangles (V3 Float))
  let projected = fmap clipWithDummy positions
  fragments <-
    if useIndices
      then rasterizeIndexed defaultRaster (indexSource "indices" indexedIndices) projected
      else rasterize defaultRaster projected
  drawColor
    defaultBlend
    (colorTarget "color" indexedTarget)
    (fmap (const (vec4 (constant 1) (constant 0) (constant 0) (constant 1))) fragments)

indexedDrawTest :: IO ()
indexedDrawTest = withTestContext $ \context -> do
  directCompiled <- compileSuccessfully (indexedPipeline False)
  indexedCompiled <- compileSuccessfully (indexedPipeline True)
  runtime <- newGraphicsRuntime context
  directPrepared <- prepareGraphicsPipeline runtime directCompiled
  indexedPrepared <- prepareGraphicsPipeline runtime indexedCompiled
  positions <- newBuffer context 4 :: IO (Buffer '[ 'Buffer.Vertex] (V3 Float))
  writeBuffer
    positions
    0
    [ V3 (-0.8) (-0.8) 0
    , V3 0.8 (-0.8) 0
    , V3 0.8 0.8 0
    , V3 (-0.8) 0.8 0
    ]
  indices <- newBuffer context 6 :: IO (Buffer '[ 'Buffer.Index] Word32)
  writeBuffer indices 0 [0, 1, 2, 2, 3, 0]
  directTarget <- newTarget context 24 24
  indexedTargetImage <- newTarget context 24 24
  directTargetBinding <- colorImageBinding directTarget
  indexedTargetBinding <- colorImageBinding indexedTargetImage
  let environment target =
        IndexedEnvironment
          { indexedPositions = vertexBufferBinding positions
          , indexedIndices = indexBufferBinding indices
          , indexedTarget = target
          }
  renderGraphicsPipeline directPrepared (environment directTargetBinding)
  renderGraphicsPipeline indexedPrepared (environment indexedTargetBinding)
  directPixels <- readImage directTarget (ImageSubresource 0 0)
  indexedPixels <- readImage indexedTargetImage (ImageSubresource 0 0)
  pixelAt 24 5 18 directPixels @?= V4 0 0 0 255
  pixelAt 24 5 18 indexedPixels @?= V4 255 0 0 255
  graphicsStats runtime >>= (@?= GraphicsStats 2 1)

forgedVertexUsageTest :: IO ()
forgedVertexUsageTest = withTestContext $ \context -> do
  compiled <- compileSuccessfully typedTriangle
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  uniformBuffer <- newBuffer context 3 :: IO (Buffer '[ 'Buffer.Uniform] (V3 Float))
  intensity <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Uniform] Float)
  target <- newTarget context 16 16
  color <- colorImageBinding target
  let environment =
        GraphicsEnvironment
          { environmentPositions = VertexBuffer (uniformBufferHandle (uniformBufferBinding uniformBuffer))
          , environmentIntensity = uniformBufferBinding intensity
          , environmentTarget = color
          , environmentPush = 0
          }
  statsBefore <- descriptorStats (preparedDescriptors prepared)
  result <- try (renderGraphicsPipeline prepared environment) :: IO (Either VpipeError ())
  case result of
    Left (VulkanFailure "graphics resource validation" detail)
      | "VERTEX" `isInfixOf` detail -> pure ()
    unexpected -> assertFailure ("expected vertex usage rejection, got " <> show unexpected)
  statsAfter <- descriptorStats (preparedDescriptors prepared)
  statsAfter @?= statsBefore
  descriptorWrites statsAfter @?= 0

forgedColorUsageTest :: IO ()
forgedColorUsageTest = withTestContext $ \context -> do
  compiled <- compileSuccessfully typedTriangle
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  positions <- newTriangleBuffer context
  intensity <- newBuffer context 1 :: IO (Buffer '[ 'Buffer.Uniform] Float)
  textureImage <- newImage context (imageExtent2D 1 1) 1 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'ImageTypes.Sampled])
  textureSampler <- newSampler context defaultSamplerDescription
  sampledBinding <- textureBinding textureImage textureSampler
  let forgedTarget = ColorImage (textureImageHandle sampledBinding)
      environment =
        GraphicsEnvironment
          { environmentPositions = vertexBufferBinding positions
          , environmentIntensity = uniformBufferBinding intensity
          , environmentTarget = forgedTarget
          , environmentPush = 0
          }
  statsBefore <- descriptorStats (preparedDescriptors prepared)
  result <- try (renderGraphicsPipeline prepared environment) :: IO (Either VpipeError ())
  case result of
    Left (VulkanFailure "graphics resource validation" detail)
      | "COLOR_ATTACHMENT" `isInfixOf` detail -> pure ()
    unexpected -> assertFailure ("expected color usage rejection, got " <> show unexpected)
  statsAfter <- descriptorStats (preparedDescriptors prepared)
  statsAfter @?= statsBefore
  descriptorWrites statsAfter @?= 0

data RawSamplerEnvironment = RawSamplerEnvironment
  { rawSamplerPositions :: VertexBuffer (V3 Float)
  , rawSamplerTexture :: TextureBinding
  , rawSamplerTarget :: ColorImage 'R8G8B8A8Unorm
  }

rawSamplerPipeline :: PipelineM RawSamplerEnvironment ()
rawSamplerPipeline = do
  sampled <- texture (textureSource "texture" rawSamplerTexture)
  positions <- vertexInput (vertexSource "positions" rawSamplerPositions :: VertexSource RawSamplerEnvironment 'Triangles (V3 Float))
  fragments <- rasterize defaultRaster (fmap clipWithDummy positions)
  drawColor
    defaultBlend
    (colorTarget "color" rawSamplerTarget)
    (fmap (const (sampleLod sampled (vec2 (constant 0.5) (constant 0.5)) (constant 0))) fragments)

laterRawSamplerPrevalidationTest :: IO ()
laterRawSamplerPrevalidationTest = withTestContext $ \context -> do
  compiled <- compileSuccessfully rawSamplerPipeline
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  positions <- newTriangleBuffer context
  textureImage <- newImage context (imageExtent2D 1 1) 1 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'ImageTypes.Sampled])
  textureSampler <- newSampler context defaultSamplerDescription
  validTexture <- textureBinding textureImage textureSampler
  target <- newTarget context 8 8
  targetBinding <- colorImageBinding target
  let forgedTexture = TextureBinding (textureImageHandle validTexture) (RuntimeHandle 0xBAD5A)
      environment =
        RawSamplerEnvironment
          { rawSamplerPositions = vertexBufferBinding positions
          , rawSamplerTexture = forgedTexture
          , rawSamplerTarget = targetBinding
          }
  statsBefore <- descriptorStats (preparedDescriptors prepared)
  result <- try (renderGraphicsPipeline prepared environment) :: IO (Either VpipeError ())
  case result of
    Left (VulkanFailure "graphics resource validation" detail)
      | "unmanaged" `isInfixOf` detail -> pure ()
    unexpected -> assertFailure ("expected raw sampler rejection, got " <> show unexpected)
  descriptorStats (preparedDescriptors prepared) >>= (@?= statsBefore)
  forM_
    [ ("vertex buffer", destroyBuffer positions)
    , ("sampled image", destroyImage textureImage)
    ]
    $ \(description, destroy) -> do
      completed <- newEmptyMVar
      _ <- forkIO $ (try destroy :: IO (Either VpipeError ())) >>= putMVar completed
      outcome <- timeout 2_000_000 (takeMVar completed)
      case outcome of
        Nothing -> assertFailure (description <> " destruction did not complete within two seconds")
        Just destroyed -> destroyed @?= Right ()

clipWithDummy :: V (V3 Float) -> (V (V4 Float), Smooth 'Vertex Float)
clipWithDummy position =
  ( vec4 (x position) (y position) (z position) (constant 1)
  , Smooth (constant (0 :: Float))
  )

clipWithW :: V (V3 Float) -> V (V4 Float)
clipWithW position = vec4 (x position) (y position) (z position) (constant 1)

newTriangleBuffer :: Context -> IO (Buffer '[ 'Buffer.Vertex] (V3 Float))
newTriangleBuffer context = do
  positions <- newBuffer context 3
  writeBuffer positions 0 [V3 (-0.8) (-0.8) 0, V3 0.8 (-0.8) 0, V3 0 0.8 0]
  pure positions

newFullscreenPositionBuffer :: Context -> IO (Buffer '[ 'Buffer.Vertex] (V3 Float))
newFullscreenPositionBuffer context = do
  positions <- newBuffer context 3
  writeBuffer positions 0 [V3 (-1) (-1) 0, V3 3 (-1) 0, V3 (-1) 3 0]
  pure positions

newFullscreenTextureVertexBuffer :: Context -> IO (Buffer '[ 'Buffer.Vertex] (V3 Float, V2 Float))
newFullscreenTextureVertexBuffer context = do
  vertices <- newBuffer context 3
  writeBuffer
    vertices
    0
    [ (V3 (-1) (-1) 0, V2 0 0)
    , (V3 3 (-1) 0, V2 2 0)
    , (V3 (-1) 3 0, V2 0 2)
    ]
  pure vertices

nearestClampedSampler :: SamplerDescription
nearestClampedSampler =
  defaultSamplerDescription
    { samplerMagFilter = Nearest
    , samplerMinFilter = Nearest
    , samplerMipmapMode = NearestMipmap
    , samplerAddressModeU = ClampToEdge
    , samplerAddressModeV = ClampToEdge
    , samplerAddressModeW = ClampToEdge
    }

textureFixturePixels :: [V4 Word8]
textureFixturePixels =
  [ V4 (fromIntegral (column * 31)) (fromIntegral (row * 29)) (fromIntegral ((column + row * 8) * 3)) 255
  | row <- [0 .. 7 :: Int]
  , column <- [0 .. 7 :: Int]
  ]

cubeFacePixels :: [V4 Word8]
cubeFacePixels =
  [ V4 255 0 0 255
  , V4 0 255 0 255
  , V4 0 0 255 255
  , V4 255 255 0 255
  , V4 255 0 255 255
  , V4 0 255 255 255
  ]

uploadCubeFace :: Image 'Cube 'R8G8B8A8Unorm '[ 'ImageTypes.Sampled, 'ImageTypes.CopySrc, 'ImageTypes.CopyDst] -> (Int, V4 Word8) -> IO ()
uploadCubeFace cube (layer, pixel) = writeImage cube (ImageSubresource 0 (fromIntegral layer)) (replicate 16 pixel)

compileSuccessfully :: PipelineM env () -> IO (CompiledPipeline env)
compileSuccessfully pipeline = do
  result <- compilePipeline pipeline
  case result of
    Left error' -> assertFailure (show error') >> fail "unreachable"
    Right compiled -> pure compiled

newTarget :: Context -> Int -> Int -> IO (Image 'D2 'R8G8B8A8Unorm '[ 'ImageTypes.ColorTarget, 'ImageTypes.CopySrc])
newTarget context width height = newImage context (imageExtent2D width height) 1 1

assertTrianglePixels :: Int -> Int -> [V4 Word8] -> IO ()
assertTrianglePixels width height pixels =
  case (pixels, drop centerIndex pixels) of
    (corner : _, center : _) -> do
      center @?= V4 255 0 0 255
      corner @?= V4 0 0 0 255
    _ -> assertFailure ("expected " <> show (width * height) <> " readback pixels, received " <> show (length pixels))
 where
  centerIndex = (height `div` 2) * width + width `div` 2

pixelAt :: Int -> Int -> Int -> [a] -> a
pixelAt width column row pixels = pixels !! (row * width + column)

withIsolatedPipelineCache :: (FilePath -> IO a) -> IO a
withIsolatedPipelineCache action =
  withTemporaryDirectory $ \cacheDirectory ->
    bracket
      (lookupEnv "XDG_CACHE_HOME")
      restoreCacheDirectory
      (\_ -> setEnv "XDG_CACHE_HOME" cacheDirectory >> action cacheDirectory)

persistedPipelineCacheArtifact :: FilePath -> IO FilePath
persistedPipelineCacheArtifact cacheDirectory = do
  let pipelineCacheDirectory = cacheDirectory </> "vpipe" </> "pipeline-cache"
  exists <- doesDirectoryExist pipelineCacheDirectory
  if not exists
    then assertFailure ("expected pipeline cache directory at " <> pipelineCacheDirectory) >> fail "unreachable"
    else do
      cacheFiles <- listDirectory pipelineCacheDirectory
      case cacheFiles of
        [cacheFile] -> do
          let cacheArtifact = pipelineCacheDirectory </> cacheFile
          artifactExists <- doesFileExist cacheArtifact
          assertBool ("expected pipeline cache artifact at " <> cacheArtifact) artifactExists
          cacheSize <- getFileSize cacheArtifact
          assertBool ("expected non-empty pipeline cache artifact at " <> cacheArtifact) (cacheSize > 0)
          pure cacheArtifact
        _ -> assertFailure ("expected one pipeline cache artifact in " <> pipelineCacheDirectory <> ", found " <> show cacheFiles) >> fail "unreachable"

restoreCacheDirectory :: Maybe FilePath -> IO ()
restoreCacheDirectory previousDirectory =
  case previousDirectory of
    Just directory -> setEnv "XDG_CACHE_HOME" directory
    Nothing -> unsetEnv "XDG_CACHE_HOME"

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory = bracket acquire removePathForcibly
 where
  acquire = do
    temporaryDirectory <- getTemporaryDirectory
    (directory, handle) <- openBinaryTempFile temporaryDirectory "vpipe-graphics-cache-test"
    hClose handle
    removeFile directory
    createDirectory directory
    pure directory

withTestContext :: (Context -> IO a) -> IO a
withTestContext action = do
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  let config = defaultVpipeConfig{vpipeValidationStrict = requested == Just "lavapipe", vpipeLogger = print}
  result <- try (withVpipe config action)
  case result of
    Left (NoVulkanIcd detail) | requested /= Just "lavapipe" -> assertFailure ("Vulkan ICD unavailable: " <> detail)
    Left error' -> throwIO (error' :: VpipeError)
    Right value -> pure value
