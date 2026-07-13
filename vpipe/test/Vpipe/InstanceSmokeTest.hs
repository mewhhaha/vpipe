{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vpipe.InstanceSmokeTest (instanceSmokeTests) where

import Control.Exception (IOException, bracket, throwIO, try)
import Data.ByteString (ByteString)
import Data.Vector qualified as Vector
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Vulkan.Core10.DeviceInitialization (
  ApplicationInfo (..),
  InstanceCreateInfo (..),
  createInstance,
  destroyInstance,
  enumeratePhysicalDevices,
 )
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.LayerDiscovery (
  LayerProperties (layerName),
  enumerateInstanceLayerProperties,
 )
import Vulkan.Core13 (pattern API_VERSION_1_3)
import Vulkan.Exception (VulkanException (..))
import Vulkan.Zero (zero)

validationLayerName :: ByteString
validationLayerName = "VK_LAYER_KHRONOS_validation"

instanceSmokeTests :: TestTree
instanceSmokeTests = testGroup "Vulkan instance" [testCase "creates a Vulkan 1.3 instance" smokeTest]

smokeTest :: IO ()
smokeTest = do
  requestedDevice <- lookupEnv "VPIPE_TEST_DEVICE"
  let reportUnavailable reason
        | requestedDevice == Just "lavapipe" =
            assertFailure ("VPIPE_TEST_DEVICE=lavapipe requires a usable Vulkan loader, ICD, and physical device: " <> reason)
        | otherwise = putStrLn ("SKIP: " <> reason)
  availableLayers <- try enumerateInstanceLayerProperties
  case availableLayers of
    Left (loaderError :: IOException) ->
      reportUnavailable ("Vulkan loader unavailable: " <> show loaderError)
    Right (_, layers) -> do
      let enabledLayers =
            [validationLayerName | any ((== validationLayerName) . layerName) (Vector.toList layers)]
          createInfo =
            (zero :: InstanceCreateInfo '[])
              { applicationInfo =
                  Just
                    (zero :: ApplicationInfo)
                      { applicationName = Just "vpipe smoke test"
                      , apiVersion = API_VERSION_1_3
                      }
              , enabledLayerNames = Vector.fromList enabledLayers
              }
      instanceResult <- try (createInstance createInfo Nothing)
      case instanceResult of
        Left (exception :: VulkanException)
          | vulkanExceptionResult exception == Result.ERROR_INCOMPATIBLE_DRIVER ->
              reportUnavailable ("no Vulkan ICD supports instance creation: " <> show exception)
          | otherwise -> throwIO exception
        Right instanceHandle ->
          bracket (pure instanceHandle) (`destroyInstance` Nothing) $ \instance' -> do
            (enumerationResult, physicalDevices) <- enumeratePhysicalDevices instance'
            case enumerationResult of
              Result.SUCCESS
                | Vector.null physicalDevices ->
                    reportUnavailable "Vulkan loader found no physical devices"
                | otherwise -> pure ()
              Result.ERROR_INITIALIZATION_FAILED ->
                reportUnavailable "no usable Vulkan ICD is available"
              unexpectedResult ->
                assertFailure ("physical-device enumeration failed: " <> show unexpectedResult)
