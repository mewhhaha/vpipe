module Main (main) where

import Control.Exception (throwIO, try)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, defaultMain, testGroup)

import Vpipe.BufferTest (bufferTests)
import Vpipe.ComputeTest (computePureTests, computeTests)
import Vpipe.Context (defaultVpipeConfig, withVpipe)
import Vpipe.ContextTest (contextPureTests, contextTests)
import Vpipe.DescriptorTest (descriptorTests)
import Vpipe.DiagnosticsTest (diagnosticsTests)
import Vpipe.Error (VpipeError (..))
import Vpipe.ErrorTest (errorTests)
import Vpipe.ExprTest (exprTests)
import Vpipe.FormatTest (formatTests)
import Vpipe.FrameResourceTest (frameResourceTests)
import Vpipe.FrameTest (framePureTests, frameTests)
import Vpipe.GraphicsCompileTest (graphicsCompileTests)
import Vpipe.GraphicsSubmissionTest (graphicsSubmissionTests)
import Vpipe.GraphicsTest (graphicsTests)
import Vpipe.ImageStateTest (imageStateTests)
import Vpipe.ImageTest (imageTests)
import Vpipe.InstanceSmokeTest (instanceSmokeTests)
import Vpipe.PipelineTest (pipelineTests)
import Vpipe.SamplerTest (samplerTests)
import Vpipe.SpirVAssemblerTest (spirVAssemblerTests)
import Vpipe.SpirVCodegenTest (spirVCodegenTests)
import Vpipe.SurfaceTest (surfaceTests)
import Vpipe.SwapchainTest (swapchainPureTests, swapchainTests)
import Vpipe.TypeErrorTest (typeErrorTests)

main :: IO ()
main = chooseTestTree >>= defaultMain

chooseTestTree :: IO TestTree
chooseTestTree = do
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  case requested of
    Just "skip" -> announcePure "VPIPE_TEST_DEVICE=skip"
    Just "lavapipe" -> pure allTests
    Just "any" -> testsForAvailableDevice
    Nothing -> testsForAvailableDevice
    Just value ->
      fail
        ( "VPIPE_TEST_DEVICE must be one of lavapipe, any, or skip; received "
            <> show value
        )

testsForAvailableDevice :: IO TestTree
testsForAvailableDevice = do
  result <- try (withVpipe defaultVpipeConfig (const (pure ())))
  case result of
    Right () -> pure allTests
    Left (NoVulkanIcd detail) -> announcePure ("no Vulkan ICD is available: " <> detail)
    Left NoSuitableDevice{} -> announcePure "no suitable Vulkan 1.3 device is available"
    Left error' -> throwIO (error' :: VpipeError)

announcePure :: String -> IO TestTree
announcePure reason = do
  putStrLn ("Running pure tests only (" <> reason <> ").")
  pure pureTests

allTests :: TestTree
allTests =
  testGroup
    "vpipe"
    [ formatTests
    , computeTests
    , diagnosticsTests
    , errorTests
    , frameResourceTests
    , frameTests
    , graphicsTests
    , graphicsCompileTests
    , graphicsSubmissionTests
    , imageStateTests
    , imageTests
    , bufferTests
    , exprTests
    , typeErrorTests
    , pipelineTests
    , samplerTests
    , spirVAssemblerTests
    , spirVCodegenTests
    , instanceSmokeTests
    , contextTests
    , descriptorTests
    , surfaceTests
    , swapchainTests
    ]

pureTests :: TestTree
pureTests =
  testGroup
    "vpipe"
    [ formatTests
    , computePureTests
    , diagnosticsTests
    , errorTests
    , frameResourceTests
    , framePureTests
    , graphicsCompileTests
    , graphicsSubmissionTests
    , imageStateTests
    , exprTests
    , typeErrorTests
    , pipelineTests
    , spirVAssemblerTests
    , spirVCodegenTests
    , contextPureTests
    , surfaceTests
    , swapchainPureTests
    ]
