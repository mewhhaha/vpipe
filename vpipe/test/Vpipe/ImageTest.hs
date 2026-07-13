{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vpipe.ImageTest (imageTests) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryReadMVar)
import Control.Exception (SomeException, displayException, throwIO, try)
import Control.Monad (forM_, void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Word (Word64, Word8)
import Linear (V2 (..), V4 (..))
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import Vpipe.Context (Context, StructuredLog (..), VpipeConfig (vpipeEnableValidation, vpipeImageTransitionLogging, vpipeLogger, vpipeValidationStrict), defaultVpipeConfig, transferQueue, withVpipe)
import Vpipe.Context.Internal (contextAllocationCountForTest)
import Vpipe.Context.Queue (Queue)
import Vpipe.Context.Queue.Internal (currentTimelineValueForTest)
import Vpipe.Error (VpipeError (ContextClosed, ImageReleased, NoVulkanIcd, VulkanFailure))
import Vpipe.Format (Format (R32G32Sfloat, R8G8B8A8Unorm, R8Unorm))
import Vpipe.Image
import Vpipe.Image.Types (Dim (Cube, D2, D2Array), ImageUsage (CopyDst, CopySrc))

imageTests :: TestTree
imageTests =
  testGroup
    "image"
    [ testCase "round trips an 8x8 RGBA upload" roundTrip
    , testCase "aligns consecutive mixed-format image uploads" mixedFormatUploadAlignment
    , testCase "keeps array layers independent" arrayLayerRoundTrip
    , testCase "matches the filtered checkerboard reference at every mip" mipGeneration
    , testCase "generates every face of a cube mip chain" cubeMipGeneration
    , testCase "rejects invalid subresources descriptions and texel counts" imageBounds
    , testCase "releases images early and rejects later use" imageRelease
    , testCase "retained images reject operations after context close" retainedImageClosure
    , testCase "cancelling submitted image readback retains temporary memory" cancelledImageReadback
    , testCase "logs image layout transitions only when enabled" imageTransitionLogging
    ]

roundTrip :: IO ()
roundTrip = withTestContext $ \context -> do
  image <- newImage context (imageExtent2D 8 8) 1 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'CopySrc, 'CopyDst])
  let pixels = [V4 x (255 - x) 17 255 | x <- [0 .. 63 :: Word8]]
  writeImage image (ImageSubresource 0 0) pixels
  readImage image (ImageSubresource 0 0) >>= (@?= pixels)
  destroyImage image

mixedFormatUploadAlignment :: IO ()
mixedFormatUploadAlignment = withTestContext $ \context -> do
  narrow <- newImage context (imageExtent2D 1 1) 1 1 :: IO (Image 'D2 'R8Unorm '[ 'CopySrc, 'CopyDst])
  wide <- newImage context (imageExtent2D 1 1) 1 1 :: IO (Image 'D2 'R32G32Sfloat '[ 'CopySrc, 'CopyDst])
  writeImage narrow (ImageSubresource 0 0) [17]
  writeImage wide (ImageSubresource 0 0) [V2 1.25 2.5]
  readImage narrow (ImageSubresource 0 0) >>= (@?= [17])
  readImage wide (ImageSubresource 0 0) >>= (@?= [V2 1.25 2.5])

arrayLayerRoundTrip :: IO ()
arrayLayerRoundTrip = withTestContext $ \context -> do
  image <- newImage context (imageExtent2DArray 4 4) 1 2 :: IO (Image 'D2Array 'R8G8B8A8Unorm '[ 'CopySrc, 'CopyDst])
  let first = replicate 16 (V4 17 18 19 255)
      second = replicate 16 (V4 29 30 31 255)
  writeImage image (ImageSubresource 0 0) first
  writeImage image (ImageSubresource 0 1) second
  readImage image (ImageSubresource 0 0) >>= (@?= first)
  readImage image (ImageSubresource 0 1) >>= (@?= second)

mipGeneration :: IO ()
mipGeneration = withTestContext $ \context -> do
  image <- newImage context (imageExtent2D 8 8) 4 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'CopySrc, 'CopyDst])
  let checkerboard =
        [ if even (x + y) then V4 0 0 0 255 else V4 255 255 255 255
        | y <- [0 .. 7 :: Int]
        , x <- [0 .. 7 :: Int]
        ]
  writeImage image (ImageSubresource 0 0) checkerboard
  generateMips image 0
  readImage image (ImageSubresource 0 0) >>= (@?= checkerboard)
  forM_ [(1, 4), (2, 2), (3, 1)] $ \(mipLevel, edge) -> do
    mip <- readImage image (ImageSubresource mipLevel 0)
    length mip @?= edge * edge
    forM_ mip $ \(V4 red green blue alpha) -> do
      assertBool "checkerboard downsample must remain within one UNORM step of half intensity" (all (\component -> component >= 127 && component <= 128) [red, green, blue])
      alpha @?= 255

cubeMipGeneration :: IO ()
cubeMipGeneration = withTestContext $ \context -> do
  image <- newImage context (imageExtentCube 4) 3 6 :: IO (Image 'Cube 'R8G8B8A8Unorm '[ 'CopySrc, 'CopyDst])
  let faceColors =
        [ V4 17 18 19 255
        , V4 41 42 43 255
        , V4 67 68 69 255
        , V4 89 90 91 255
        , V4 113 114 115 255
        , V4 137 138 139 255
        ]
  forM_ (zip [0 ..] faceColors) $ \(layer, color) -> do
    writeImage image (ImageSubresource 0 layer) (replicate 16 color)
    generateMips image (fromIntegral layer)
  forM_ (zip [0 ..] faceColors) $ \(layer, color) ->
    readImage image (ImageSubresource 2 layer) >>= (@?= [color])

imageBounds :: IO ()
imageBounds = withTestContext $ \context -> do
  image <- newImage context (imageExtent2D 4 4) 1 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'CopySrc, 'CopyDst])
  wrongCount <- try (writeImage image (ImageSubresource 0 0) []) :: IO (Either VpipeError ())
  case wrongCount of
    Left VulkanFailure{} -> pure ()
    unexpected -> assertFailure ("expected texel-count failure, got " <> show unexpected)
  invalidSubresource <- try (readImage image (ImageSubresource 1 0)) :: IO (Either ImageSubresourceOutOfBounds [V4 Word8])
  case invalidSubresource of
    Left _ -> pure ()
    Right _ -> assertFailure "expected image subresource bounds failure"
  invalidDescription <- try (newImage context (imageExtent2D 4 4) 4 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'CopySrc]))
  case invalidDescription of
    Left VulkanFailure{} -> pure ()
    unexpected -> assertFailure ("expected mip-count failure, got " <> show (void unexpected))

imageRelease :: IO ()
imageRelease = withTestContext $ \context -> do
  baseline <- contextAllocationCountForTest context
  image <- newImage context (imageExtent2D 2 2) 1 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'CopySrc, 'CopyDst])
  destroyImage image
  destroyImage image
  contextAllocationCountForTest context >>= (@?= baseline)
  result <- try (readImage image (ImageSubresource 0 0))
  result @?= Left ImageReleased
  displayException ImageReleased @?= "This image has already been released; create a new image before using it again."

retainedImageClosure :: IO ()
retainedImageClosure = do
  image <- withTestContext $ \context -> newImage context (imageExtent2D 2 2) 1 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'CopySrc, 'CopyDst])
  forM_
    [ writeImage image (ImageSubresource 0 0) (replicate 4 (V4 0 0 0 0))
    , void (readImage image (ImageSubresource 0 0))
    , destroyImage image
    ]
    assertContextClosed

cancelledImageReadback :: IO ()
cancelledImageReadback = withTestContext $ \context -> do
  let width = 1024
      height = 1024
      pixel = V4 41 42 43 255
      queue = transferQueue context
  image <- newImage context (imageExtent2D width height) 1 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'CopySrc, 'CopyDst])
  writeImage image (ImageSubresource 0 0) (replicate (width * height) pixel)
  before <- currentTimelineValueForTest queue
  completion <- newEmptyMVar
  worker <- forkIO (try (readImage image (ImageSubresource 0 0)) >>= putMVar completion)
  waitForSubmission queue before completion 100000
  killThread worker
  outcome <- takeMVar completion
  case outcome of
    Left (_ :: SomeException) -> pure ()
    Right _ -> assertFailure "image readback completed before cancellation"
  marker <- newImage context (imageExtent2D 1 1) 1 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'CopySrc, 'CopyDst])
  writeImage marker (ImageSubresource 0 0) [pixel]
  readImage marker (ImageSubresource 0 0) >>= (@?= [pixel])

imageTransitionLogging :: IO ()
imageTransitionLogging = do
  silentLogs <- newIORef []
  withTestContextConfigured
    (\config -> config{vpipeEnableValidation = False, vpipeLogger = recordLog silentLogs})
    runTransitions
  silentTransitions <- transitionLogs <$> readIORef silentLogs
  silentTransitions @?= []

  loggedTransitions <- newIORef []
  withTestContextConfigured
    (\config -> config{vpipeEnableValidation = False, vpipeImageTransitionLogging = True, vpipeLogger = recordLog loggedTransitions})
    runTransitions
  messages <- fmap logMessage . transitionLogs <$> readIORef loggedTransitions
  length messages @?= 2
  messages @?= filter isExpectedTransition messages
  assertBool
    "logs the initial upload layout transition"
    (any (isInfixOf "oldLayout=IMAGE_LAYOUT_UNDEFINED newLayout=IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL") messages)
  assertBool
    "logs the readback layout transition"
    (any (isInfixOf "oldLayout=IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL newLayout=IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL") messages)
 where
  runTransitions context = do
    image <- newImage context (imageExtent2D 1 1) 1 1 :: IO (Image 'D2 'R8G8B8A8Unorm '[ 'CopySrc, 'CopyDst])
    let pixel = V4 17 18 19 255
    writeImage image (ImageSubresource 0 0) [pixel]
    writeImage image (ImageSubresource 0 0) [pixel]
    readImage image (ImageSubresource 0 0) >>= (@?= [pixel])

  recordLog logs entry = modifyIORef' logs (<> [entry])
  transitionLogs = filter ((== "vpipe.image.transition") . logMessageId)
  isExpectedTransition message =
    all
      (`isInfixOf` message)
      [ "image=0x"
      , "mipRange=0+1"
      , "layerRange=0+1"
      ]

waitForSubmission :: Queue -> Word64 -> MVar (Either SomeException [V4 Word8]) -> Int -> IO ()
waitForSubmission queue previous completion remaining
  | remaining == 0 = assertFailure "image readback did not submit before timeout"
  | otherwise = do
      finished <- tryReadMVar completion
      case finished of
        Just _ -> assertFailure "image readback completed before submission was observed"
        Nothing -> do
          current <- currentTimelineValueForTest queue
          if current > previous
            then pure ()
            else threadDelay 10 >> waitForSubmission queue previous completion (remaining - 1)

assertContextClosed :: IO () -> IO ()
assertContextClosed action = do
  result <- try action
  result @?= Left ContextClosed

withTestContext :: (Context -> IO a) -> IO a
withTestContext = withTestContextConfigured id

withTestContextConfigured :: (VpipeConfig -> VpipeConfig) -> (Context -> IO a) -> IO a
withTestContextConfigured configure action = do
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  let config = configure defaultVpipeConfig{vpipeValidationStrict = requested == Just "lavapipe", vpipeLogger = print}
  result <- try (withVpipe config action)
  case result of
    Left (NoVulkanIcd detail) | requested /= Just "lavapipe" -> error ("SKIP: Vulkan ICD unavailable: " <> detail)
    Left error' -> throwIO (error' :: VpipeError)
    Right value -> pure value
