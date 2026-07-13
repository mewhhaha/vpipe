{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent (forkOS, newEmptyMVar, putMVar, runInBoundThread, takeMVar, threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless, when)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word32)
import Linear (V4 (..))
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Text.Read (readMaybe)

import Vpipe.Buffer (Buffer, destroyBuffer, readBuffer, writeBuffer)
import Vpipe.Buffer qualified as Buffer
import Vpipe.Buffer.Dynamic (FrameDynamicBuffer, destroyFrameDynamicBuffer, newFrameDynamicBuffer)
import Vpipe.Compute
import Vpipe.Context (Context, VpipeConfig (..), defaultVpipeConfig)
import Vpipe.Error (VpipeError (..))
import Vpipe.Expr qualified as Expr
import Vpipe.Format (Format (B8G8R8A8Srgb))
import Vpipe.Frame
import Vpipe.GLFW (Window, WindowConfig (..), defaultWindowConfig, getFramebufferSize, pollEvents, requestWindowClose, resizeWindow, windowShouldClose, windowSurface, withWindows)
import Vpipe.Graphics (GraphicsRuntime, PreparedGraphicsPipeline, graphicsStats, newGraphicsRuntime, prepareGraphicsPipeline)
import Vpipe.Pipeline qualified as Pipeline
import Vpipe.Swapchain (PresentMode (Immediate), PresentResult (..), defaultSwapchainConfig, destroySwapchain, newSwapchain, presentModePreference)

main :: IO ()
main = runInBoundThread frameDynamicAndCopyTest

data Environment = Environment
  { environmentInput :: Pipeline.StorageBuffer Word32
  , environmentOutput :: Pipeline.StorageBuffer Word32
  , environmentTint :: Pipeline.UniformBuffer (V4 Float)
  , environmentVertices :: Pipeline.VertexBuffer (V4 Float)
  , environmentTarget :: Pipeline.ColorImage 'B8G8R8A8Srgb
  }

computeProgram :: ComputeM Environment ()
computeProgram = do
  input <- storageBuffer environmentInput
  output <- storageBuffer environmentOutput
  invocation <- globalInvocationId
  let index = globalInvocationX invocation
  whenInBounds input index $ \value ->
    whenInBounds output index $ \_ ->
      writeAt output index value

graphicsProgram :: Pipeline.PipelineM Environment ()
graphicsProgram = do
  positions <-
    Pipeline.vertexInput
      (Pipeline.vertexSource "positions" environmentVertices :: Pipeline.VertexSource Environment 'Pipeline.Triangles (V4 Float))
  tint <-
    Pipeline.uniform
      (Pipeline.uniformSource "tint" environmentTint :: Pipeline.Uniform Environment (V4 Float))
  fragments <-
    Pipeline.rasterize
      Pipeline.defaultRaster
      (fmap (\position -> (position, Pipeline.Smooth (Expr.constant (0 :: Float) :: Expr.V Float))) positions)
  Pipeline.drawColor
    Pipeline.defaultBlend
    (Pipeline.colorTarget "swapchain" environmentTarget)
    (fmap (const tint) fragments)

frameDynamicAndCopyTest :: IO ()
frameDynamicAndCopyTest = do
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  case requested of
    Just "skip" -> putStrLn "SKIP: GLFW integration test disabled by VPIPE_TEST_DEVICE=skip"
    _ -> do
      let config =
            defaultVpipeConfig
              { vpipeValidationStrict = requested == Just "lavapipe"
              , vpipeLogger = print
              }
          firstWindow =
            defaultWindowConfig
              { windowWidth = 64
              , windowHeight = 64
              , windowTitle = "vpipe frame integration A"
              }
          secondWindow = firstWindow{windowTitle = "vpipe frame integration B"}
          exercise =
            withWindows config (firstWindow :| [secondWindow]) $ \context windows -> case windows of
              first :| [second] -> exerciseWindows context first second
              _ -> testFailure "GLFW did not return exactly two requested windows"
      result <- try exercise :: IO (Either VpipeError ())
      case result of
        Left (NoVulkanIcd detail)
          | requested /= Just "lavapipe" -> putStrLn ("SKIP: Vulkan ICD unavailable: " <> detail)
        Left (VulkanFailure operation detail)
          | requested /= Just "lavapipe"
          , operation == "glfwInit" || operation == "glfwCreateWindow" ->
              putStrLn ("SKIP: " <> operation <> " failed with " <> detail)
        Left error' -> throwIO error'
        Right () -> putStrLn "PASS: GLFW frames preserve dynamic/copy behavior across worker rendering, resize, recreation, close, and allocation stress"

exerciseWindows :: Context -> Window -> Window -> IO ()
exerciseWindows context firstWindow secondWindow = do
  firstExtent <- currentFramebufferExtent firstWindow
  secondExtent <- currentFramebufferExtent secondWindow
  preparedCompute <- prepareCompute context
  graphicsRuntime <- newGraphicsRuntime context
  preparedGraphics <- prepareGraphics graphicsRuntime
  vertices <- Buffer.newBuffer context 3 :: IO (Buffer '[ 'Buffer.Vertex] (V4 Float))
  writeBuffer
    vertices
    0
    [ V4 (-0.8) (-0.8) 0 1
    , V4 0.8 (-0.8) 0 1
    , V4 0 0.8 0 1
    ]
  output <- Buffer.newBuffer context 1 :: IO (Buffer '[ 'Buffer.Storage, 'Buffer.CopySrc] Word32)
  writeBuffer output 0 [0]
  let stressSwapchainConfig = defaultSwapchainConfig{presentModePreference = Immediate}
  firstSwapchain <- newSwapchain context (windowSurface firstWindow) stressSwapchainConfig
  secondSwapchain <- newSwapchain context (windowSurface secondWindow) defaultSwapchainConfig
  dynamicStorage <- newFrameDynamicBuffer firstSwapchain 1 :: IO (FrameDynamicBuffer '[ 'Buffer.Storage] Word32)
  dynamicUniform <- newFrameDynamicBuffer firstSwapchain 1 :: IO (FrameDynamicBuffer '[ 'Buffer.Uniform] (V4 Float))

  let submitDynamicFrame value =
        frame firstSwapchain $ \current ->
          withDynamicStorage dynamicStorage 0 [value] $ \input ->
            withDynamicUniform dynamicUniform 0 [V4 0 1 0 1] $ \tint ->
              let target = frameColorTarget current
                  environment =
                    Environment
                      { environmentInput = input
                      , environmentOutput = Pipeline.storageBufferBinding output
                      , environmentTint = tint
                      , environmentVertices = Pipeline.vertexBufferBinding vertices
                      , environmentTarget = target
                      }
               in computePassFor preparedCompute environment (1, 1, 1)
                    >> renderTo target (render preparedGraphics environment)
      expectPresented expectedExtent result = case result of
        Presented extent _ -> assertEqual "presented framebuffer extent" expectedExtent extent
        PresentDeferred reason -> testFailure ("window frame unexpectedly deferred: " <> show reason)

  pollEvents
  submitDynamicFrame 41 >>= expectPresented firstExtent
  readBuffer output 0 1 >>= assertEqual "first dynamic storage result" [41]
  submitDynamicFrame 43 >>= expectPresented firstExtent
  readBuffer output 0 1 >>= assertEqual "second dynamic storage result" [43]
  workerFrameResult <- newEmptyMVar
  _ <- forkOS $ do
    result <- try (submitDynamicFrame 47) :: IO (Either SomeException PresentResult)
    putMVar workerFrameResult result
  completedWorkerFrame <- timeout 5_000_000 (takeMVar workerFrameResult)
  case completedWorkerFrame of
    Nothing -> testFailure "worker-thread frame did not complete within five seconds"
    Just (Left failure) -> throwIO failure
    Just (Right result) -> expectPresented firstExtent result
  readBuffer output 0 1 >>= assertEqual "worker-thread dynamic storage result" [47]
  graphicsStatsBeforeStress <- graphicsStats graphicsRuntime

  duplicate <-
    try
      ( frame firstSwapchain $ \_ -> do
          withDynamicStorage dynamicStorage 0 [47] (const (pure ()))
          withDynamicStorage dynamicStorage 0 [53] (const (pure ()))
      ) ::
      IO (Either VpipeError PresentResult)
  assertEqual "duplicate dynamic binding" (Left FrameDynamicBufferAlreadyUsed) duplicate

  wrongSwapchain <-
    try
      ( frame secondSwapchain $ \_ ->
          withDynamicStorage dynamicStorage 0 [59] (const (pure ()))
      ) ::
      IO (Either VpipeError PresentResult)
  assertEqual "cross-swapchain dynamic binding" (Left FrameDynamicBufferDomainMismatch) wrongSwapchain

  source <- Buffer.newBuffer context 4 :: IO (Buffer '[ 'Buffer.CopySrc] Word32)
  destination <- Buffer.newBuffer context 4 :: IO (Buffer '[ 'Buffer.CopyDst, 'Buffer.CopySrc] Word32)
  writeBuffer source 0 [10, 20, 30, 40]
  writeBuffer destination 0 [0, 0, 0, 0]
  frame secondSwapchain (\_ -> copyPass source 1 destination 2 2) >>= expectPresented secondExtent
  readBuffer destination 0 4 >>= assertEqual "copied element range" [0, 0, 20, 30]

  overlap <-
    try
      (frame secondSwapchain (\_ -> copyPass destination 0 destination 1 2)) ::
      IO (Either VpipeError PresentResult)
  assertEqual "overlapping same-buffer copy" (Left (BufferCopyOverlap 0 1 2)) overlap

  submitDynamicFrame 61 >>= expectPresented firstExtent
  readBuffer output 0 1 >>= assertEqual "dynamic binding claim resets after submission" [61]
  stressFrameCount <- readPositiveEnvironment "VPIPE_GLFW_STRESS_FRAMES" 64
  resizeInterval <- readPositiveEnvironment "VPIPE_GLFW_STRESS_RESIZE_INTERVAL" 8
  recreationCount <- runFrameStress firstWindow stressFrameCount resizeInterval submitDynamicFrame
  graphicsStats graphicsRuntime >>= assertEqual "graphics pipeline stats after resize stress" graphicsStatsBeforeStress
  let expectedRecreations = stressFrameCount `div` resizeInterval
  when (recreationCount < expectedRecreations) $
    testFailure
      ( "swapchain recreation count after resize stress: expected at least "
          <> show expectedRecreations
          <> ", received "
          <> show recreationCount
      )

  windowShouldClose firstWindow >>= assertEqual "first window close flag initially" False
  requestWindowClose firstWindow
  windowShouldClose firstWindow >>= assertEqual "first window close flag after request" True
  windowShouldClose secondWindow >>= assertEqual "second window close flag remains clear" False

  assertResourceReleased "dynamic uniform buffer" (destroyFrameDynamicBuffer dynamicUniform)
  assertResourceReleased "dynamic storage buffer" (destroyFrameDynamicBuffer dynamicStorage)
  assertResourceReleased "frame output buffer" (destroyBuffer output)
  assertResourceReleased "frame vertex buffer" (destroyBuffer vertices)
  assertResourceReleased "copy destination buffer" (destroyBuffer destination)
  assertResourceReleased "copy source buffer" (destroyBuffer source)
  assertResourceReleased "second swapchain" (destroySwapchain secondSwapchain)
  assertResourceReleased "first swapchain" (destroySwapchain firstSwapchain)

prepareCompute :: Context -> IO (PreparedCompute Environment 1 1 1)
prepareCompute context = do
  compiled <- compileCompute (Dispatch @1 @1 @1) computeProgram >>= either (testFailure . show) pure
  runtime <- newComputeRuntime context
  prepareComputePipeline runtime compiled

prepareGraphics :: GraphicsRuntime -> IO (PreparedGraphicsPipeline Environment)
prepareGraphics runtime = do
  compiled <- Pipeline.compilePipeline graphicsProgram >>= either (testFailure . show) pure
  prepareGraphicsPipeline runtime compiled

runFrameStress :: Window -> Int -> Int -> (Word32 -> IO PresentResult) -> IO Int
runFrameStress window stressFrameCount resizeInterval submitFrame = loop 1 0
 where
  resizeDimensions = [(96, 64), (64, 96), (80, 80), (112, 72)]
  loop frameNumber recreationCount
    | frameNumber > stressFrameCount = pure recreationCount
    | otherwise = do
        let resized = frameNumber `mod` resizeInterval == 0
        when resized $ do
          previousExtent <- currentFramebufferExtent window
          let requestedExtent = resizeDimensions !! ((frameNumber `div` resizeInterval - 1) `mod` length resizeDimensions)
          resizeWindow window (fst requestedExtent) (snd requestedExtent)
          awaitFramebufferResize window requestedExtent previousExtent
        recreated <- presentStressFrame window frameNumber (submitFrame (fromIntegral (frameNumber + 100)))
        when (resized && not recreated) $ do
          observedExtent <- currentFramebufferExtent window
          testFailure
            ( "resize stress frame "
                <> show frameNumber
                <> " presented without a swapchain recreation; framebuffer extent "
                <> show observedExtent
                <> ", completed recreations "
                <> show recreationCount
            )
        loop (frameNumber + 1) (recreationCount + if recreated then 1 else 0)

presentStressFrame :: Window -> Int -> IO PresentResult -> IO Bool
presentStressFrame window frameNumber submitFrame = go 1 False []
 where
  maximumPresentationAttempts = 20 :: Int
  go attempt recreationObserved observations = do
    before <- currentFramebufferExtent window
    result <- submitFrame
    after <- currentFramebufferExtent window
    let updatedObservations = observations <> [(before, after, result)]
    case result of
      Presented extent recreated
        | extent == after -> pure (recreationObserved || recreated)
        | attempt >= maximumPresentationAttempts ->
            testFailure
              ( "resize stress frame "
                  <> show frameNumber
                  <> " did not converge on the GLFW framebuffer extent after "
                  <> show maximumPresentationAttempts
                  <> " attempts"
                  <> "; observations "
                  <> show updatedObservations
              )
        | otherwise -> do
            pollEvents
            threadDelay 10_000
            go (attempt + 1) (recreationObserved || recreated) updatedObservations
      PresentDeferred _
        | attempt >= maximumPresentationAttempts ->
            testFailure
              ( "resize stress frame "
                  <> show frameNumber
                  <> " did not present after "
                  <> show maximumPresentationAttempts
                  <> " attempts; observations "
                  <> show updatedObservations
              )
        | otherwise -> do
            pollEvents
            threadDelay 10_000
            go (attempt + 1) recreationObserved updatedObservations

awaitFramebufferResize :: Window -> (Int, Int) -> (Word32, Word32) -> IO ()
awaitFramebufferResize window requestedExtent previousExtent = go 1 previousExtent
 where
  maximumResizeAttempts = 100 :: Int
  go attempt observedExtent
    | attempt > maximumResizeAttempts =
        testFailure
          ( "GLFW resize to "
              <> show requestedExtent
              <> " did not change framebuffer extent from "
              <> show previousExtent
              <> " after "
              <> show maximumResizeAttempts
              <> " event polls; last observed extent "
              <> show observedExtent
          )
    | otherwise = do
        pollEvents
        currentExtent <- currentFramebufferExtent window
        if currentExtent /= previousExtent
          then pure ()
          else threadDelay 10_000 >> go (attempt + 1) currentExtent

currentFramebufferExtent :: Window -> IO (Word32, Word32)
currentFramebufferExtent window = do
  dimensions@(width, height) <- getFramebufferSize window
  unless (width > 0 && height > 0) $
    testFailure ("GLFW framebuffer has invalid extent " <> show dimensions)
  pure (fromIntegral width, fromIntegral height)

readPositiveEnvironment :: String -> Int -> IO Int
readPositiveEnvironment name fallback = do
  requested <- lookupEnv name
  case requested of
    Nothing -> pure fallback
    Just value -> case readMaybe value of
      Just count | count > 0 -> pure count
      _ -> testFailure (name <> " must be a positive integer, received " <> show value)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $
    testFailure (label <> ": expected " <> show expected <> ", received " <> show actual)

assertResourceReleased :: String -> IO () -> IO ()
assertResourceReleased label release = do
  completed <- timeout 5_000_000 release
  case completed of
    Nothing -> testFailure (label <> " destruction did not complete after its submitted frame finished")
    Just () -> pure ()

testFailure :: String -> IO a
testFailure = throwIO . userError
