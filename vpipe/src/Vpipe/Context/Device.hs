{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Vulkan physical-device discovery, selection, and logical-device creation.

Most applications use the default selection policy through 'Vpipe.Context.withVpipe'.
The candidate fields let an application adjust that policy without handling
device enumeration itself:

@
module Main (main) where

import Vpipe.Context
  ( VpipeConfig (vpipeDeviceScore)
  , contextDeviceName
  , defaultVpipeConfig
  , withVpipe
  )
import Vpipe.Context.Device
  ( CandidateDevice (candidateSamplerAnisotropy, candidateScore)
  )

main :: IO ()
main =
  withVpipe
    defaultVpipeConfig
      { vpipeDeviceScore = \candidate ->
          candidateScore candidate
            + if candidateSamplerAnisotropy candidate then 10 else 0
      }
    (putStrLn . contextDeviceName)
@
-}
module Vpipe.Context.Device (
  CandidateDevice (..),
  DeviceSelection (..),
  LogicalDeviceBuilder,
  chooseDevice,
  enumerateCandidates,
  addCandidateRejections,
  deviceRejectionReasons,
  familyWith,
  createLogicalDevice,
  createLogicalDeviceWith,
  defaultLogicalDeviceBuilder,
  choosePresentFamily,
  presentFamilyFor,
  queueFamilyUnion,
) where

import Control.Applicative ((<|>))
import Control.Exception (catch, mask_, onException, throwIO)
import Control.Monad (unless, when)
import Data.Bits ((.&.), (.|.))
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Maybe (catMaybes, isJust)
import Data.Vector qualified as Vector
import Data.Word (Word32, Word64)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, castFunPtr, castPtr)
import Foreign.Storable (peek)
import GHC.Records (getField)
import Vulkan.CStruct (withCStruct)
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.Device qualified as Device
import Vulkan.Core10.DeviceInitialization qualified as Init
import Vulkan.Core10.Enums.PhysicalDeviceType qualified as DeviceType
import Vulkan.Core10.Enums.QueueFlagBits qualified as QueueFlags
import Vulkan.Core10.Enums.Result (Result (..))
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.ExtensionDiscovery qualified as Extensions
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Core10.Queue qualified as Queue
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 qualified as Features
import Vulkan.Core12 (PhysicalDeviceVulkan12Features (timelineSemaphore))
import Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore qualified as Timeline
import Vulkan.Core13 (PhysicalDeviceVulkan13Features (dynamicRendering, synchronization2))
import Vulkan.Core13 qualified as Vulkan13
import Vulkan.Dynamic qualified as Dynamic
import Vulkan.Exception qualified as Vulkan
import Vulkan.Extensions.Handles qualified as Surfaces
import Vulkan.Extensions.VK_KHR_portability_subset qualified as Portability
import Vulkan.Extensions.VK_KHR_surface qualified as Surface
import Vulkan.Extensions.VK_KHR_swapchain qualified as Swapchain
import Vulkan.Zero (zero)

import Vpipe.Error (DeviceRejection (..), VpipeError (..))

type CreateDeviceFunction =
  Ptr Handles.PhysicalDevice_T ->
  Ptr () ->
  Ptr () ->
  Ptr (Ptr Handles.Device_T) ->
  IO Result

foreign import ccall unsafe "vpipe_create_device" cCreateDeviceWithoutLayers :: FunPtr CreateDeviceFunction -> Ptr Handles.PhysicalDevice_T -> Ptr () -> Ptr (Ptr Handles.Device_T) -> IO Result

data CandidateDevice = CandidateDevice
  { candidateHandle :: Handles.PhysicalDevice
  , candidateName :: String
  , candidateDeviceType :: DeviceType.PhysicalDeviceType
  , candidateScore :: Int
  , candidateRejection :: [String]
  , candidateGraphicsFamily :: Maybe Word32
  , candidateComputeFamily :: Maybe Word32
  , candidateTransferFamily :: Maybe Word32
  , candidatePresentFamilies :: [Word32]
  , candidateEnabledExtensions :: [ByteString.ByteString]
  , candidateMaxTimelineDifference :: Word64
  , candidateSamplerAnisotropy :: Bool
  }

data DeviceSelection = DeviceSelection
  { selectedDevice :: CandidateDevice
  , rejectedDevices :: [DeviceRejection]
  }

{- | Receives a device which has already passed vpipe's required-capability
checks. A custom builder can use 'createLogicalDeviceWith' to submit an
extended Vulkan feature chain, and must enable queues for every family in
'queueFamilyUnion' (graphics, compute, transfer, and all present families),
while retaining every 'candidateEnabledExtensions' entry.
-}
type LogicalDeviceBuilder = CandidateDevice -> IO (Handles.Device, [(Word32, Handles.Queue)])

chooseDevice :: (CandidateDevice -> Int) -> [CandidateDevice] -> Either [DeviceRejection] DeviceSelection
chooseDevice score candidates =
  case filter (null . candidateRejection) candidates of
    [] -> Left (map rejection candidates)
    firstUsable : remainingUsable -> Right (DeviceSelection (foldl preferFirst firstUsable remainingUsable) (map rejection (filter (not . null . candidateRejection) candidates)))
 where
  rejection candidate = DeviceRejection (candidateName candidate) (candidateRejection candidate)
  preferFirst best candidate
    | score candidate > score best = candidate
    | otherwise = best

addCandidateRejections :: (CandidateDevice -> [String]) -> CandidateDevice -> CandidateDevice
addCandidateRejections requirements candidate =
  candidate{candidateRejection = candidateRejection candidate <> requirements candidate}

enumerateCandidates :: Handles.Instance -> [ByteString.ByteString] -> [Surfaces.SurfaceKHR] -> IO [CandidateDevice]
enumerateCandidates instance' requiredExtensions surfaces = do
  (_, physicalDevices) <- Init.enumeratePhysicalDevices instance'
  case Vector.toList physicalDevices of
    [] -> throwIO (NoVulkanIcd "the active ICD reported no physical devices")
    devices -> traverse (inspectDevice requiredExtensions surfaces) devices

inspectDevice :: [ByteString.ByteString] -> [Surfaces.SurfaceKHR] -> Handles.PhysicalDevice -> IO CandidateDevice
inspectDevice requiredExtensions surfaces physicalDevice = do
  Features.PhysicalDeviceProperties2 (timelineProperties Chain.:& ()) properties <-
    Features.getPhysicalDeviceProperties2 physicalDevice ::
      IO (Features.PhysicalDeviceProperties2 '[Timeline.PhysicalDeviceTimelineSemaphoreProperties])
  features <-
    Features.getPhysicalDeviceFeatures2 physicalDevice ::
      IO (Features.PhysicalDeviceFeatures2 '[PhysicalDeviceVulkan13Features, PhysicalDeviceVulkan12Features])
  families <- Init.getPhysicalDeviceQueueFamilyProperties physicalDevice
  (_, extensionProperties) <- Extensions.enumerateDeviceExtensionProperties physicalDevice Nothing
  let availableExtensions = map Extensions.extensionName (Vector.toList extensionProperties)
      portabilityAdvertised = Portability.KHR_PORTABILITY_SUBSET_EXTENSION_NAME `elem` availableExtensions
      requiredDeviceExtensions = requiredExtensions <> [Swapchain.KHR_SWAPCHAIN_EXTENSION_NAME | not (null surfaces)]
      missingExtensions = filter (`notElem` availableExtensions) requiredDeviceExtensions
      enabledExtensions = deduplicate ([Portability.KHR_PORTABILITY_SUBSET_EXTENSION_NAME | portabilityAdvertised] <> requiredDeviceExtensions)
      supportedApiVersion = getField @"apiVersion" properties
      Features.PhysicalDeviceFeatures2 (features13 Chain.:& features12 Chain.:& ()) baseFeatures = features
      indexedFamilies = zip [0 ..] (Vector.toList families)
      graphics = familyWith QueueFlags.QUEUE_GRAPHICS_BIT indexedFamilies
      compute = dedicatedFamily QueueFlags.QUEUE_COMPUTE_BIT QueueFlags.QUEUE_GRAPHICS_BIT indexedFamilies graphics
      transfer = dedicatedFamily QueueFlags.QUEUE_TRANSFER_BIT (QueueFlags.QUEUE_GRAPHICS_BIT .|. QueueFlags.QUEUE_COMPUTE_BIT) indexedFamilies graphics
  presentFamilies <- traverse (presentFamilyFor physicalDevice indexedFamilies) surfaces
  let reasons =
        deviceRejectionReasons
          supportedApiVersion
          (dynamicRendering features13)
          (synchronization2 features13)
          (timelineSemaphore features12)
          (isJust graphics)
          missingExtensions
          <> ["no present-capable queue family for surface index " <> show index | (index, Nothing) <- zip [0 :: Int ..] presentFamilies]
  pure
    CandidateDevice
      { candidateHandle = physicalDevice
      , candidateName = ByteString.Char8.unpack (Init.deviceName properties)
      , candidateDeviceType = Init.deviceType properties
      , candidateScore = deviceTypeScore (Init.deviceType properties)
      , candidateRejection = reasons
      , candidateGraphicsFamily = graphics
      , candidateComputeFamily = compute
      , candidateTransferFamily = transfer
      , candidatePresentFamilies = catMaybes presentFamilies
      , candidateEnabledExtensions = enabledExtensions
      , candidateMaxTimelineDifference = Timeline.maxTimelineSemaphoreValueDifference timelineProperties
      , candidateSamplerAnisotropy = Init.samplerAnisotropy baseFeatures
      }

{- | Select presentation independently for each ordered surface. Prefer the
graphics family, then the first present-capable family with queues.
-}
presentFamilyFor :: Handles.PhysicalDevice -> [(Word32, Init.QueueFamilyProperties)] -> Surfaces.SurfaceKHR -> IO (Maybe Word32)
presentFamilyFor physicalDevice families surface = do
  supported <- traverse supports families
  pure (choosePresentFamily (familyWith QueueFlags.QUEUE_GRAPHICS_BIT families) supported)
 where
  supports (family, properties)
    | Init.queueCount properties == 0 = pure (family, False)
    | otherwise = (family,) <$> Surface.getPhysicalDeviceSurfaceSupportKHR physicalDevice family surface

choosePresentFamily :: Maybe Word32 -> [(Word32, Bool)] -> Maybe Word32
choosePresentFamily graphics supported =
  case [family | (family, True) <- supported] of
    [] -> Nothing
    available@(first : _) ->
      Just $ case graphics of
        Just family | family `elem` available -> family
        _ -> first

queueFamilyUnion :: CandidateDevice -> [Word32]
queueFamilyUnion candidate =
  deduplicate
    ( maybe [] pure (candidateGraphicsFamily candidate)
        <> maybe [] pure (candidateComputeFamily candidate)
        <> maybe [] pure (candidateTransferFamily candidate)
        <> candidatePresentFamilies candidate
    )

deviceRejectionReasons :: Word32 -> Bool -> Bool -> Bool -> Bool -> [ByteString.ByteString] -> [String]
deviceRejectionReasons supportedApiVersion hasDynamicRendering hasSynchronization2 hasTimelineSemaphore hasGraphicsQueue missingExtensions =
  ["Vulkan API version is below 1.3" | supportedApiVersion < Vulkan13.API_VERSION_1_3]
    <> ["dynamicRendering is unavailable" | not hasDynamicRendering]
    <> ["synchronization2 is unavailable" | not hasSynchronization2]
    <> ["timelineSemaphore is unavailable" | not hasTimelineSemaphore]
    <> ["no graphics-capable queue family" | not hasGraphicsQueue]
    <> map (\extension -> "required device extension is unavailable: " <> ByteString.Char8.unpack extension) missingExtensions

familyWith :: QueueFlags.QueueFlagBits -> [(Word32, Init.QueueFamilyProperties)] -> Maybe Word32
familyWith capability = fmap fst . foldr select Nothing
 where
  select entry@(_, properties) found
    | Init.queueCount properties > 0 && Init.queueFlags properties .&. capability /= zero = Just entry
    | otherwise = found

dedicatedFamily :: QueueFlags.QueueFlagBits -> QueueFlags.QueueFlagBits -> [(Word32, Init.QueueFamilyProperties)] -> Maybe Word32 -> Maybe Word32
dedicatedFamily required avoided families fallback =
  familyWith required (filter (\(_, properties) -> Init.queueFlags properties .&. avoided == zero) families) <|> fallback

deviceTypeScore :: DeviceType.PhysicalDeviceType -> Int
deviceTypeScore deviceType
  | deviceType == DeviceType.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = 400
  | deviceType == DeviceType.PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = 300
  | deviceType == DeviceType.PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = 200
  | deviceType == DeviceType.PHYSICAL_DEVICE_TYPE_CPU = 100
  | otherwise = 0

createLogicalDevice :: CandidateDevice -> IO (Handles.Device, [(Word32, Handles.Queue)])
createLogicalDevice candidate = mask_ $ do
  unless (isJust (candidateGraphicsFamily candidate)) $
    throwIO (NoSuitableDevice [DeviceRejection (candidateName candidate) ["no graphics-capable queue family"]])
  let families = queueFamilyUnion candidate
      queueInfos = Vector.fromList [Chain.SomeStruct (Device.DeviceQueueCreateInfo () zero family (Vector.singleton 1)) | family <- families]
      enabled13 = (zero :: PhysicalDeviceVulkan13Features){dynamicRendering = True, synchronization2 = True}
      enabled12 = (zero :: PhysicalDeviceVulkan12Features){timelineSemaphore = True}
      enabled10 = (zero :: Init.PhysicalDeviceFeatures){Init.samplerAnisotropy = candidateSamplerAnisotropy candidate}
      createInfo =
        Device.DeviceCreateInfo
          (enabled13 Chain.:& enabled12 Chain.:& ())
          zero
          queueInfos
          Vector.empty
          (Vector.fromList (candidateEnabledExtensions candidate))
          (Just enabled10)
  device <- mapDeviceFailure "vkCreateDevice" (createDeviceWithoutLayers (candidateHandle candidate) createInfo)
  queues <-
    traverse (\family -> (family,) <$> Queue.getDeviceQueue device family 0) families
      `onException` Device.destroyDevice device Nothing
  pure (device, queues)

{- | Creates a device from a caller-supplied, typed feature chain. The caller
is responsible for retaining vpipe's required features, every
'candidateEnabledExtensions' entry (including @VK_KHR_swapchain@ for surfaced
contexts), and all families in 'queueFamilyUnion'.
-}
createLogicalDeviceWith :: (Chain.Extendss Device.DeviceCreateInfo features, Chain.PokeChain features) => CandidateDevice -> Device.DeviceCreateInfo features -> IO (Handles.Device, [(Word32, Handles.Queue)])
createLogicalDeviceWith candidate createInfo = mask_ $ do
  unless (isJust (candidateGraphicsFamily candidate)) $
    throwIO (NoSuitableDevice [DeviceRejection (candidateName candidate) ["no graphics-capable queue family"]])
  let families = queueFamilyUnion candidate
  device <- mapDeviceFailure "vkCreateDevice" (createDeviceWithoutLayers (candidateHandle candidate) createInfo)
  queues <-
    traverse (\family -> (family,) <$> Queue.getDeviceQueue device family 0) families
      `onException` Device.destroyDevice device Nothing
  pure (device, queues)

defaultLogicalDeviceBuilder :: LogicalDeviceBuilder
defaultLogicalDeviceBuilder = createLogicalDevice

createDeviceWithoutLayers :: (Chain.Extendss Device.DeviceCreateInfo features, Chain.PokeChain features) => Handles.PhysicalDevice -> Device.DeviceCreateInfo features -> IO Handles.Device
createDeviceWithoutLayers physicalDevice createInfo =
  withCStruct createInfo $ \createInfoPointer ->
    alloca $ \devicePointer -> do
      let Handles.PhysicalDevice physicalDeviceHandle instanceCommands = physicalDevice
      result <-
        cCreateDeviceWithoutLayers
          (castFunPtr (Dynamic.pVkCreateDevice instanceCommands))
          physicalDeviceHandle
          (castPtr createInfoPointer)
          devicePointer
      when (result < SUCCESS) (throwIO (Vulkan.VulkanException result))
      rawDevice <- peek devicePointer
      deviceCommands <- Dynamic.initDeviceCmds instanceCommands rawDevice
      pure (Handles.Device rawDevice deviceCommands)

deduplicate :: (Eq a) => [a] -> [a]
deduplicate = foldl' (\seen value -> if value `elem` seen then seen else seen <> [value]) []

mapDeviceFailure :: String -> IO a -> IO a
mapDeviceFailure operation action =
  action `catch` \(error' :: Vulkan.VulkanException) ->
    if Vulkan.vulkanExceptionResult error' == Result.ERROR_DEVICE_LOST
      then throwIO DeviceLost
      else throwIO (VulkanFailure operation (show (Vulkan.vulkanExceptionResult error')))
