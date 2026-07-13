{-# LANGUAGE DataKinds #-}

module Vpipe.Examples.Common (
  ExampleOptions (..),
  parseExampleOptions,
  offscreenFrameCount,
  renderFrames,
  runWindowFrames,
  runWindowFramesWith,
  withExampleContext,
  withExampleContextWith,
  compilePipelineOrFail,
  screenshotWidth,
  screenshotHeight,
  ScreenshotTarget,
  newScreenshotTarget,
  readScreenshotTarget,
  writeExampleScreenshot,
  captureScreenshot,
  newFullscreenQuad,
  runOffscreenTriangle,
  writeRgba8,
) where

import Codec.Picture (PixelRGBA8 (..), generateImage, writePng)
import Codec.Picture qualified as Picture
import Control.Concurrent (runInBoundThread)
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Linear (V2 (..), V3 (..), V4 (..))
import System.Environment (getArgs)
import System.Environment qualified as Environment
import System.IO (hPrint, stderr)
import Text.Read (readMaybe)
import Vpipe.Buffer (Buffer, newBuffer, writeBuffer)
import Vpipe.Buffer qualified as Buffer
import Vpipe.Context (Context, VpipeConfig (vpipeLogger, vpipeValidationStrict), defaultVpipeConfig, withVpipe)
import Vpipe.Expr (V, constant, vec4, x, y, z)
import Vpipe.Format (Format (R8G8B8A8Unorm))
import Vpipe.GLFW (Window, WindowConfig (windowTitle), defaultWindowConfig, pollEvents, windowShouldClose, withWindow)
import Vpipe.Graphics (newGraphicsRuntime, prepareGraphicsPipeline, renderGraphicsPipeline)
import Vpipe.Image (Image, ImageSubresource (..), imageExtent2D, newImage, readImage)
import Vpipe.Image.Types (Dim (D2))
import Vpipe.Image.Types qualified as ImageTypes
import Vpipe.Pipeline (ColorImage, ColorTarget, CompiledPipeline, PipelineM, PrimitiveTopology (Triangles), Smooth (..), VertexBuffer, VertexSource, colorImageBinding, colorTarget, compilePipeline, defaultBlend, defaultRaster, drawColor, rasterize, vertexBufferBinding, vertexInput, vertexSource)
import Vpipe.Swapchain (PresentResult)

data ExampleOptions = ExampleOptions
  { exampleFrameLimit :: Maybe Int
  , exampleScreenshot :: Maybe FilePath
  }
  deriving stock (Eq, Show)

parseExampleOptions :: IO ExampleOptions
parseExampleOptions = parse (ExampleOptions Nothing Nothing) =<< getArgs
 where
  parse options arguments = case arguments of
    [] -> pure options
    "--frames" : value : rest -> do
      count <- maybe (fail "--frames expects a positive integer") pure (readMaybe value)
      unless (count > 0) (fail "--frames expects a positive integer")
      parse options{exampleFrameLimit = Just count} rest
    "--screenshot" : path : rest ->
      parse options{exampleScreenshot = Just path} rest
    unknown : _ ->
      fail ("unknown example argument: " <> unknown <> "; use --frames N and --screenshot FILE")

-- | Screenshot rendering is deliberately one frame unless explicitly bounded.
offscreenFrameCount :: ExampleOptions -> Int
offscreenFrameCount = fromMaybe 1 . exampleFrameLimit

renderFrames :: Int -> IO a -> IO a
renderFrames count renderOne
  | count <= 0 = fail "frame count must be positive"
  | count == 1 = renderOne
  | otherwise = renderOne >> renderFrames (count - 1) renderOne

{- | Run presentation attempts until the user closes the window or the optional
bound is exhausted. Events are polled before every attempt.
-}
runWindowFrames :: ExampleOptions -> String -> (Context -> Window -> IO (IO PresentResult)) -> IO ()
runWindowFrames = runWindowFramesWith id

runWindowFramesWith :: (VpipeConfig -> VpipeConfig) -> ExampleOptions -> String -> (Context -> Window -> IO (IO PresentResult)) -> IO ()
runWindowFramesWith configure options title action = runInBoundThread $ do
  config <- exampleContextConfig configure
  withWindow config defaultWindowConfig{windowTitle = title} $ \context window -> do
    renderOne <- action context window
    loop window (exampleFrameLimit options) renderOne
 where
  loop window remaining renderOne = do
    pollEvents
    closed <- windowShouldClose window
    case (closed, remaining) of
      (True, _) -> pure ()
      (_, Just 0) -> pure ()
      _ -> do
        _ <- renderOne
        loop window (fmap pred remaining) renderOne

{- | Render the example triangle through Vulkan and optionally save its final
64x64 RGBA8 target. Setting @VPIPE_TEST_DEVICE=lavapipe@ also makes
validation and synchronization-validation availability strict.
-}
runOffscreenTriangle :: ExampleOptions -> IO ()
runOffscreenTriangle options = withExampleContext $ \context -> do
  compiled <- compileTriangle
  runtime <- newGraphicsRuntime context
  prepared <- prepareGraphicsPipeline runtime compiled
  positions <- newTriangleBuffer context
  target <- newScreenshotTarget context
  targetBinding <- colorImageBinding target
  let environment = TriangleEnvironment (vertexBufferBinding positions) targetBinding
  renderFrames (offscreenFrameCount options) (renderGraphicsPipeline prepared environment)
  pixels <- readScreenshotTarget target
  validateTrianglePixels pixels
  writeExampleScreenshot options pixels

{- | Run an example with validation enabled. Lavapipe mode is deliberately
strict so missing synchronization validation or any reported message fails.
-}
withExampleContext :: (Context -> IO a) -> IO a
withExampleContext = withExampleContextWith id

-- | Variant for examples which need an additional advertised device feature.
withExampleContextWith :: (VpipeConfig -> VpipeConfig) -> (Context -> IO a) -> IO a
withExampleContextWith configure action = do
  config <- exampleContextConfig configure
  withVpipe config action

exampleContextConfig :: (VpipeConfig -> VpipeConfig) -> IO VpipeConfig
exampleContextConfig configure = do
  requestedDevice <- Environment.lookupEnv "VPIPE_TEST_DEVICE"
  pure $
    configure
      defaultVpipeConfig
        { vpipeValidationStrict = requestedDevice == Just "lavapipe"
        , vpipeLogger = hPrint stderr
        }

compilePipelineOrFail :: String -> PipelineM env () -> IO (CompiledPipeline env)
compilePipelineOrFail label pipeline = do
  result <- compilePipeline pipeline
  case result of
    Left error' -> fail (label <> " pipeline compilation failed: " <> show error')
    Right compiled -> pure compiled

screenshotWidth, screenshotHeight :: Int
screenshotWidth = 64
screenshotHeight = 64

type ScreenshotTarget = Image 'D2 'R8G8B8A8Unorm '[ 'ImageTypes.ColorTarget, 'ImageTypes.CopySrc]

newScreenshotTarget :: Context -> IO ScreenshotTarget
newScreenshotTarget context = newImage context (imageExtent2D screenshotWidth screenshotHeight) 1 1

readScreenshotTarget :: ScreenshotTarget -> IO [V4 Word8]
readScreenshotTarget target = do
  pixels <- readImage target (ImageSubresource 0 0)
  let expectedCount = screenshotWidth * screenshotHeight
  unless (length pixels == expectedCount) $
    fail ("screenshot readback pixel count mismatch: expected " <> show expectedCount <> ", received " <> show (length pixels))
  pure pixels

writeExampleScreenshot :: ExampleOptions -> [V4 Word8] -> IO ()
writeExampleScreenshot options pixels = case exampleScreenshot options of
  Nothing -> pure ()
  Just path -> writeRgba8 path screenshotWidth screenshotHeight (concatMap rgbaBytes pixels)

captureScreenshot :: ExampleOptions -> ScreenshotTarget -> IO [V4 Word8]
captureScreenshot options target = do
  pixels <- readScreenshotTarget target
  writeExampleScreenshot options pixels
  pure pixels

newFullscreenQuad :: Context -> IO (Buffer '[ 'Buffer.Vertex] (V2 Float, V2 Float))
newFullscreenQuad context = do
  vertices <- newBuffer context 6
  writeBuffer
    vertices
    0
    [ (V2 (-1) (-1), V2 0 0)
    , (V2 1 (-1), V2 1 0)
    , (V2 1 1, V2 1 1)
    , (V2 (-1) (-1), V2 0 0)
    , (V2 1 1, V2 1 1)
    , (V2 (-1) 1, V2 0 1)
    ]
  pure vertices

data TriangleEnvironment = TriangleEnvironment
  { trianglePositions :: VertexBuffer (V3 Float)
  , triangleTarget :: ColorImage 'R8G8B8A8Unorm
  }

trianglePositionsSource :: VertexSource TriangleEnvironment 'Triangles (V3 Float)
trianglePositionsSource = vertexSource "positions" trianglePositions

triangleColorTarget :: ColorTarget TriangleEnvironment 'R8G8B8A8Unorm
triangleColorTarget = colorTarget "color" triangleTarget

trianglePipeline :: PipelineM TriangleEnvironment ()
trianglePipeline = do
  positions <- vertexInput trianglePositionsSource
  fragments <- rasterize defaultRaster (fmap vertex positions)
  drawColor defaultBlend triangleColorTarget (fmap unSmooth fragments)
 where
  vertex position =
    ( vec4 (x position) (y position) (z position) (constant 1)
    , Smooth (constant (V4 1 0 0 1) :: V (V4 Float))
    )

compileTriangle :: IO (CompiledPipeline TriangleEnvironment)
compileTriangle = compilePipelineOrFail "triangle" trianglePipeline

newTriangleBuffer :: Context -> IO (Buffer '[ 'Buffer.Vertex] (V3 Float))
newTriangleBuffer context = do
  positions <- newBuffer context 3
  writeBuffer positions 0 [V3 (-0.8) (-0.8) 0, V3 0.8 (-0.8) 0, V3 0 0.8 0]
  pure positions

validateTrianglePixels :: [V4 Word8] -> IO ()
validateTrianglePixels pixels = do
  let expectedCount = screenshotWidth * screenshotHeight
  unless (length pixels == expectedCount) $
    fail ("triangle readback pixel count mismatch: expected " <> show expectedCount <> ", received " <> show (length pixels))
  case (pixels, drop centerIndex pixels) of
    (corner : _, center : _) -> do
      unless (corner == V4 0 0 0 255) (fail ("triangle corner pixel was not opaque black: " <> show corner))
      unless (center == V4 255 0 0 255) (fail ("triangle center pixel was not opaque red: " <> show center))
    _ -> fail "triangle readback unexpectedly contained no pixels"
 where
  centerIndex = (screenshotHeight `div` 2) * screenshotWidth + screenshotWidth `div` 2

rgbaBytes :: V4 Word8 -> [Word8]
rgbaBytes (V4 red green blue alpha) = [red, green, blue, alpha]

writeRgba8 :: FilePath -> Int -> Int -> [Word8] -> IO ()
writeRgba8 path width height bytes
  | width <= 0 || height <= 0 = fail "screenshot dimensions must be positive"
  | length bytes /= width * height * 4 =
      fail
        ( "RGBA8 screenshot byte count mismatch: expected "
            <> show (width * height * 4)
            <> ", received "
            <> show (length bytes)
        )
  | otherwise = writePng path image
 where
  image :: Picture.Image PixelRGBA8
  image = generateImage pixel width height
  pixel column row = case drop (4 * (row * width + column)) bytes of
    red : green : blue : alpha : _ -> PixelRGBA8 red green blue alpha
    _ -> error "writeRgba8: validated RGBA8 buffer was unexpectedly short"
