{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vpipe.Context.Instance (
  InstanceOwner,
  DebugSink,
  DebugMessage (..),
  createVulkanInstance,
  destroyVulkanInstance,
  instanceHandle,
  instanceDebugUtilsEnabled,
  instanceDebugSink,
  instanceValidationNotice,
  drainInstanceDebugMessages,
  newDebugSink,
  popDebugMessage,
  debugSinkDropped,
  freeDebugSink,
  testDebugSinkCallback,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (Handler (..), IOException, catches, finally, mask_, onException, throwIO)
import Control.Monad (unless, when)
import Data.Bits ((.|.))
import Data.ByteString qualified as ByteString
import Data.Foldable (traverse_)
import Data.Vector qualified as Vector
import Data.Word (Word32, Word64)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt (..), CSize (..), CUInt (..), CULLong (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.DeviceInitialization qualified as Vk
import Vulkan.Core10.Enums.InstanceCreateFlagBits qualified as InstanceFlags
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.ExtensionDiscovery qualified as Extensions
import Vulkan.Core10.FundamentalTypes qualified as Fundamental
import Vulkan.Core10.LayerDiscovery qualified as Vk
import Vulkan.Core13 qualified as Vk
import Vulkan.Exception qualified as Vulkan
import Vulkan.Extensions.VK_EXT_debug_utils qualified as Debug
import Vulkan.Extensions.VK_EXT_layer_settings qualified as LayerSettings
import Vulkan.Extensions.VK_EXT_validation_features qualified as Validation
import Vulkan.Extensions.VK_KHR_portability_enumeration qualified as Portability
import Vulkan.Zero (zero)

import Vpipe.Error (VpipeError (..))

validationLayer :: ByteString.ByteString
validationLayer = "VK_LAYER_KHRONOS_validation"

-- VVL emits this setup notification with LogInfo during vkCreateInstance.
-- Suppressing the exact self-status ID leaves every validation finding visible.
validationLayerStatusMessage :: String
validationLayerStatusMessage = "WARNING-CreateInstance-status-message"

data CDebugSink
newtype DebugSink = DebugSink (Ptr CDebugSink)

data DebugMessage = DebugMessage
  { debugSeverity :: Word32
  , debugType :: Word32
  , debugMessageId :: String
  , debugMessageText :: String
  }
  deriving (Eq, Show)

data InstanceOwner = InstanceOwner
  { ownedHandle :: Vk.Instance
  , ownedDebugUtilsEnabled :: Bool
  , ownedMessenger :: Maybe Debug.DebugUtilsMessengerEXT
  , ownedDebugSink :: Maybe DebugSink
  , ownedDebugCapture :: Maybe (MVar (Maybe DebugSink, [DebugMessage], Word64))
  , ownedValidationNotice :: Maybe String
  }

instanceHandle :: InstanceOwner -> Vk.Instance
instanceHandle = ownedHandle

instanceDebugUtilsEnabled :: InstanceOwner -> Bool
instanceDebugUtilsEnabled = ownedDebugUtilsEnabled

instanceDebugSink :: InstanceOwner -> Maybe DebugSink
instanceDebugSink = ownedDebugSink

instanceValidationNotice :: InstanceOwner -> Maybe String
instanceValidationNotice = ownedValidationNotice

drainInstanceDebugMessages :: InstanceOwner -> IO ([DebugMessage], Word64)
drainInstanceDebugMessages owner = case ownedDebugCapture owner of
  Just capture -> modifyMVar capture $ \(liveSink, pending, droppedCheckpoint) -> case liveSink of
    Just sink -> do
      messages <- drainSink sink
      dropped <- debugSinkDropped sink
      pure
        ( (liveSink, [], dropped)
        , (filter validationFinding (pending <> messages), dropped - droppedCheckpoint)
        )
    Nothing -> pure ((Nothing, [], 0), (filter validationFinding pending, droppedCheckpoint))
  Nothing -> pure ([], 0)
 where
  validationFinding message = debugMessageId message /= validationLayerStatusMessage

foreign import ccall unsafe "vpipe_debug_sink_create" cDebugSinkCreate :: CSize -> IO (Ptr CDebugSink)
foreign import ccall unsafe "vpipe_debug_sink_pop" cDebugSinkPop :: Ptr CDebugSink -> Ptr CUInt -> Ptr CUInt -> CString -> CSize -> CString -> CSize -> IO CInt
foreign import ccall unsafe "vpipe_debug_sink_dropped" cDebugSinkDropped :: Ptr CDebugSink -> IO CULLong
foreign import ccall unsafe "vpipe_debug_sink_free" cDebugSinkFree :: Ptr CDebugSink -> IO ()
foreign import ccall unsafe "vpipe_debug_sink_test_callback" cDebugSinkTestCallback :: Ptr CDebugSink -> CUInt -> CUInt -> CString -> CString -> IO ()
foreign import ccall unsafe "&vpipe_debug_callback" debugCallback :: Debug.PFN_vkDebugUtilsMessengerCallbackEXT

newDebugSink :: Int -> IO DebugSink
newDebugSink capacity = do
  pointer <- cDebugSinkCreate (fromIntegral capacity)
  when (pointer == nullPtr) (throwIO (CleanupFailed ["could not allocate Vulkan debug message sink"]))
  pure (DebugSink pointer)

freeDebugSink :: DebugSink -> IO ()
freeDebugSink (DebugSink pointer) = cDebugSinkFree pointer

popDebugMessage :: DebugSink -> IO (Maybe DebugMessage)
popDebugMessage (DebugSink pointer) =
  alloca $ \severityPointer ->
    alloca $ \typePointer ->
      allocaBytes 128 $ \messageIdPointer ->
        allocaBytes 1024 $ \messagePointer -> do
          present <- cDebugSinkPop pointer severityPointer typePointer messageIdPointer 128 messagePointer 1024
          if present == 0
            then pure Nothing
            else do
              severity <- peek severityPointer
              type' <- peek typePointer
              messageId <- peekCString messageIdPointer
              message <- peekCString messagePointer
              pure (Just (DebugMessage (fromIntegral severity) (fromIntegral type') messageId message))

debugSinkDropped :: DebugSink -> IO Word64
debugSinkDropped (DebugSink pointer) = fromIntegral <$> cDebugSinkDropped pointer

testDebugSinkCallback :: DebugSink -> DebugMessage -> IO ()
testDebugSinkCallback (DebugSink pointer) message =
  withCString (debugMessageId message) $ \messageIdPointer ->
    withCString (debugMessageText message) $ \messagePointer ->
      cDebugSinkTestCallback pointer (fromIntegral (debugSeverity message)) (fromIntegral (debugType message)) messageIdPointer messagePointer

createVulkanInstance :: ByteString.ByteString -> Bool -> Bool -> [ByteString.ByteString] -> IO InstanceOwner
createVulkanInstance applicationName validationRequested validationStrict extraExtensions = mask_ $ do
  (_, availableLayers) <- catchNoIcd Vk.enumerateInstanceLayerProperties
  (_, globalExtensions) <- catchNoIcd (Extensions.enumerateInstanceExtensionProperties Nothing)
  let layerPresent = any ((== validationLayer) . Vk.layerName) (Vector.toList availableLayers)
  layerExtensions <-
    if validationRequested && layerPresent
      then snd <$> catchNoIcd (Extensions.enumerateInstanceExtensionProperties (Just validationLayer))
      else pure Vector.empty
  let globalExtensionNames = map Extensions.extensionName (Vector.toList globalExtensions)
      layerExtensionNames = map Extensions.extensionName (Vector.toList layerExtensions)
      debugPresent = Debug.EXT_DEBUG_UTILS_EXTENSION_NAME `elem` globalExtensionNames
      syncValidationPresent = Validation.EXT_VALIDATION_FEATURES_EXTENSION_NAME `elem` layerExtensionNames
      layerSettingsPresent = LayerSettings.EXT_LAYER_SETTINGS_EXTENSION_NAME `elem` layerExtensionNames
      portabilityPresent = Portability.KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME `elem` globalExtensionNames
      missingRequested = filter (`notElem` globalExtensionNames) extraExtensions
      validationEnabled = validationRequested && layerPresent && debugPresent
      validationNotice = validationUnavailableNotice validationRequested layerPresent debugPresent syncValidationPresent
  unless (null missingRequested) (throwIO (RequiredInstanceExtensionsUnavailable missingRequested))
  when validationStrict (traverse_ (throwIO . ValidationUnavailable) validationNotice)
  if validationEnabled
    then createDebugOwner portabilityPresent syncValidationPresent layerSettingsPresent validationNotice
    else createPlainOwner portabilityPresent debugPresent validationNotice
 where
  applicationInfo =
    (zero :: Vk.ApplicationInfo)
      { Vk.applicationName = Just applicationName
      , Vk.apiVersion = Vk.API_VERSION_1_3
      }
  createPlainOwner portabilityPresent debugPresent notice = do
    let debugExtensions = [Debug.EXT_DEBUG_UTILS_EXTENSION_NAME | debugPresent]
        enabledExtensions = enabledInstanceExtensions portabilityPresent debugExtensions extraExtensions
        createFlags = if portabilityPresent then InstanceFlags.INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR else zero
        createInfo :: Vk.InstanceCreateInfo '[]
        createInfo = Vk.InstanceCreateInfo () createFlags (Just applicationInfo) Vector.empty (Vector.fromList enabledExtensions)
    instance' <- catchNoIcd (Vk.createInstance createInfo Nothing)
    pure (InstanceOwner instance' debugPresent Nothing Nothing Nothing notice)
  createDebugOwner portabilityPresent syncValidationPresent layerSettingsPresent notice =
    with (zero :: Fundamental.Bool32) $ \cacheDisabledPointer ->
      withCString validationLayerStatusMessage $ \statusMessage ->
        with statusMessage $ \statusMessagePointer -> do
          sink@(DebugSink sinkPointer) <- newDebugSink 256
          capture <- newMVar (Just sink, [], 0)
          let createDebugInfo =
                (zero :: Debug.DebugUtilsMessengerCreateInfoEXT)
                  { Debug.messageSeverity = debugMessageSeverities
                  , Debug.messageType =
                      Debug.DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                  , Debug.pfnUserCallback = debugCallback
                  , Debug.userData = castPtr sinkPointer
                  }
              messengerDebugInfo =
                createDebugInfo
                  { Debug.messageSeverity = debugMessageSeverities
                  }
              validationFeatures =
                (zero :: Validation.ValidationFeaturesEXT)
                  { Validation.enabledValidationFeatures = Vector.singleton Validation.VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT
                  , Validation.disabledValidationFeatures = Vector.singleton Validation.VALIDATION_FEATURE_DISABLE_SHADER_VALIDATION_CACHE_EXT
                  }
              layerSettings =
                LayerSettings.LayerSettingsCreateInfoEXT
                  ( Vector.fromList
                      [ LayerSettings.LayerSettingEXT
                          validationLayer
                          "check_shaders_caching"
                          LayerSettings.LAYER_SETTING_TYPE_BOOL32_EXT
                          1
                          (castPtr cacheDisabledPointer)
                      , LayerSettings.LayerSettingEXT
                          validationLayer
                          "message_id_filter"
                          LayerSettings.LAYER_SETTING_TYPE_STRING_EXT
                          1
                          (castPtr statusMessagePointer)
                      ]
                  )
              validationExtensions = [Validation.EXT_VALIDATION_FEATURES_EXTENSION_NAME | syncValidationPresent]
              settingsExtensions = [LayerSettings.EXT_LAYER_SETTINGS_EXTENSION_NAME | layerSettingsPresent]
              enabledExtensions = enabledInstanceExtensions portabilityPresent (Debug.EXT_DEBUG_UTILS_EXTENSION_NAME : validationExtensions <> settingsExtensions) extraExtensions
              createFlags = if portabilityPresent then InstanceFlags.INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR else zero
          instance' <-
            ( case (syncValidationPresent, layerSettingsPresent) of
                (True, True) -> do
                  let syncSettingsCreateInfo :: Vk.InstanceCreateInfo '[LayerSettings.LayerSettingsCreateInfoEXT, Validation.ValidationFeaturesEXT, Debug.DebugUtilsMessengerCreateInfoEXT]
                      syncSettingsCreateInfo = Vk.InstanceCreateInfo (layerSettings Chain.:& validationFeatures Chain.:& createDebugInfo Chain.:& ()) createFlags (Just applicationInfo) (Vector.singleton validationLayer) (Vector.fromList enabledExtensions)
                  catchNoIcd (Vk.createInstance syncSettingsCreateInfo Nothing)
                (True, False) -> do
                  let syncCreateInfo :: Vk.InstanceCreateInfo '[Validation.ValidationFeaturesEXT, Debug.DebugUtilsMessengerCreateInfoEXT]
                      syncCreateInfo = Vk.InstanceCreateInfo (validationFeatures Chain.:& createDebugInfo Chain.:& ()) createFlags (Just applicationInfo) (Vector.singleton validationLayer) (Vector.fromList enabledExtensions)
                  catchNoIcd (Vk.createInstance syncCreateInfo Nothing)
                (False, True) -> do
                  let settingsCreateInfo :: Vk.InstanceCreateInfo '[LayerSettings.LayerSettingsCreateInfoEXT, Debug.DebugUtilsMessengerCreateInfoEXT]
                      settingsCreateInfo = Vk.InstanceCreateInfo (layerSettings Chain.:& createDebugInfo Chain.:& ()) createFlags (Just applicationInfo) (Vector.singleton validationLayer) (Vector.fromList enabledExtensions)
                  catchNoIcd (Vk.createInstance settingsCreateInfo Nothing)
                (False, False) -> do
                  let debugCreateInfo :: Vk.InstanceCreateInfo '[Debug.DebugUtilsMessengerCreateInfoEXT]
                      debugCreateInfo = Vk.InstanceCreateInfo (createDebugInfo Chain.:& ()) createFlags (Just applicationInfo) (Vector.singleton validationLayer) (Vector.fromList enabledExtensions)
                  catchNoIcd (Vk.createInstance debugCreateInfo Nothing)
            )
              `onException` freeDebugSink sink
          messenger <-
            mapInstanceRuntimeFailure "vkCreateDebugUtilsMessengerEXT" (Debug.createDebugUtilsMessengerEXT instance' messengerDebugInfo Nothing)
              `onException` (Vk.destroyInstance instance' Nothing `finally` freeDebugSink sink)
          pure (InstanceOwner instance' True (Just messenger) (Just sink) (Just capture) notice)

  debugMessageSeverities =
    Debug.DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
      .|. Debug.DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
      .|. Debug.DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
      .|. Debug.DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT

destroyVulkanInstance :: InstanceOwner -> IO ()
destroyVulkanInstance owner =
  mask_ $
    maybe (pure ()) (\messenger -> Debug.destroyDebugUtilsMessengerEXT (ownedHandle owner) messenger Nothing) (ownedMessenger owner)
      `finally` (Vk.destroyInstance (ownedHandle owner) Nothing `finally` captureAndFree)
 where
  captureAndFree = case ownedDebugSink owner of
    Nothing -> pure ()
    Just sink ->
      ( do
          messages <- drainSink sink
          dropped <- debugSinkDropped sink
          traverse_ (\capture -> modifyMVar_ capture (\(_, pending, droppedCheckpoint) -> pure (Nothing, pending <> messages, dropped - droppedCheckpoint))) (ownedDebugCapture owner)
      )
        `finally` freeDebugSink sink

drainSink :: DebugSink -> IO [DebugMessage]
drainSink sink = go []
 where
  go messages = do
    next <- popDebugMessage sink
    case next of
      Nothing -> pure (reverse messages)
      Just message -> go (message : messages)

enabledInstanceExtensions :: Bool -> [ByteString.ByteString] -> [ByteString.ByteString] -> [ByteString.ByteString]
enabledInstanceExtensions portabilityPresent automatic requested =
  deduplicate (automatic <> [Portability.KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME | portabilityPresent] <> requested)

deduplicate :: (Eq a) => [a] -> [a]
deduplicate = foldl' (\seen value -> if value `elem` seen then seen else seen <> [value]) []

validationUnavailableNotice :: Bool -> Bool -> Bool -> Bool -> Maybe String
validationUnavailableNotice validationRequested layerPresent debugPresent syncValidationPresent
  | not validationRequested = Nothing
  | not layerPresent = Just "VK_LAYER_KHRONOS_validation is not installed"
  | not debugPresent = Just "VK_EXT_debug_utils is not supported"
  | not syncValidationPresent = Just "VK_EXT_validation_features is unavailable, so synchronization validation cannot be enabled"
  | otherwise = Nothing

catchNoIcd :: IO a -> IO a
catchNoIcd action =
  action
    `catches` [ Handler (\(error' :: IOException) -> throwIO (NoVulkanIcd (show error')))
              , Handler (\(error' :: Vulkan.VulkanException) -> mapVulkanFailure error')
              ]
 where
  mapVulkanFailure error'
    | result == Result.ERROR_INCOMPATIBLE_DRIVER || result == Result.ERROR_INITIALIZATION_FAILED = throwIO (NoVulkanIcd (show error'))
    | otherwise = throwIO (VulkanFailure "Vulkan instance operation" (show result))
   where
    result = Vulkan.vulkanExceptionResult error'

mapInstanceRuntimeFailure :: String -> IO a -> IO a
mapInstanceRuntimeFailure operation action =
  action
    `catches` [ Handler (\(error' :: IOException) -> throwIO (VulkanFailure operation (show error')))
              , Handler (\(error' :: Vulkan.VulkanException) -> throwIO (VulkanFailure operation (show (Vulkan.vulkanExceptionResult error'))))
              ]
