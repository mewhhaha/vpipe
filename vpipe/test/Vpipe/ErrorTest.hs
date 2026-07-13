{-# LANGUAGE OverloadedStrings #-}

module Vpipe.ErrorTest (errorTests) where

import Control.Exception (displayException)
import Data.List (isInfixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Vpipe.Error (VpipeError (..))

errorTests :: TestTree
errorTests =
  testGroup
    "errors"
    [ testCase "no ICD names an installable software implementation" $ do
        let message = displayException (NoVulkanIcd "initialization failed")
        assertContains "Install a Vulkan ICD" message
        assertContains "lavapipe" message
    , testCase "missing window extensions names the GLFW fix" $ do
        let message = displayException (RequiredInstanceExtensionsUnavailable ["VK_KHR_surface"])
        assertContains "Vpipe.GLFW.withWindow" message
        assertContains "Vpipe.GLFW.requiredInstanceExtensions" message
    , testCase "internal shader failures ask for a bug report and dump" $ do
        let path = "/tmp/vpipe-dump/triangle.fragment.spvasm"
            message = displayException (ShaderCompileBug "spirv-val rejected OpPhi" path)
        assertContains "vpipe bug" message
        assertContains "please file it" message
        assertContains path message
    , testCase "device loss names the capture and retry remedy" $ do
        let message = displayException DeviceLost
        assertContains "RenderDoc" message
        assertContains "retry" message
    , testCase "surface loss names both resources that must be recreated" $ do
        let message = displayException SurfaceLost
        assertContains "surface" message
        assertContains "swapchain" message
    ]

assertContains :: String -> String -> IO ()
assertContains expected actual =
  assertBool
    ("expected " <> show expected <> " in " <> show actual)
    (expected `isInfixOf` actual)
