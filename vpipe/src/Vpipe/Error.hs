{- | The exception hierarchy used by managed vpipe operations.

Errors include an actionable remedy in their @displayException@ text:

@
module Main (main) where

import Control.Exception (displayException)
import Vpipe.Error (VpipeError (NoVulkanIcd))

main :: IO ()
main = putStrLn (displayException (NoVulkanIcd "initialization failed"))
@
-}
module Vpipe.Error (
  DeviceRejection (..),
  VpipeError (..),
) where

import Control.Exception (Exception (..))
import Data.ByteString (ByteString)
import Data.Word (Word64)

data DeviceRejection = DeviceRejection
  { rejectedDeviceName :: String
  , rejectedReasons :: [String]
  }
  deriving (Eq, Show)

data VpipeError
  = NoVulkanIcd {vpipeErrorDetail :: String}
  | NoSuitableDevice {candidateRejections :: [DeviceRejection]}
  | RequiredInstanceExtensionsUnavailable {missingInstanceExtensions :: [ByteString]}
  | ValidationUnavailable {validationUnavailableReason :: String}
  | ValidationFailed {validationMessageCount :: Int, validationDroppedCount :: Word64}
  | TimelineValueExhausted
  | TimelineValueDifferenceExceeded
      { completedTimelineValue :: Word64
      , requestedTimelineValue :: Word64
      , maximumTimelineValueDifference :: Word64
      }
  | TimelineValueNotSubmitted
      { awaitedTimelineValue :: Word64
      , availableSubmittedTimelineValue :: Word64
      }
  | ContextClosed
  | ResourceQuarantined
  | CleanupFailed {cleanupFailures :: [String]}
  | ShaderCompileBug
      { shaderCompileBugDetail :: String
      , shaderCompileBugDumpPath :: FilePath
      }
  | VulkanFailure {vulkanOperation :: String, vulkanResult :: String}
  | DeviceLost
  | SurfaceLost
  | SwapchainFormatUnavailable {availableSwapchainFormats :: [String]}
  | SwapchainUsageUnsupported {supportedSwapchainUsage :: String}
  | SwapchainExtentUnavailable
  | SurfaceContextMismatch
  | SwapchainReleased
  | SwapchainPoisoned
  | FrameExpired
  | FrameDynamicBufferDomainMismatch
  | FrameDynamicBufferAlreadyUsed
  | InvalidFramesInFlight {requestedFramesInFlight :: Int}
  | BufferElementRangeInvalid
      { bufferElementOffset :: Int
      , bufferElementCount :: Int
      , bufferCapacity :: Int
      }
  | BufferElementCountInvalid {requestedBufferElements :: Int}
  | BufferSizeOverflow {overflowingBufferElements :: Int, overflowingElementBytes :: Int}
  | BufferReleased
  | ImageReleased
  | DynamicBufferDescriptorOffsetMisaligned
      { dynamicBufferDescriptorElementOffset :: Int
      , dynamicBufferDescriptorByteOffset :: Int
      , dynamicBufferDescriptorRequiredAlignment :: Int
      }
  | BufferCopyStrideMismatch {sourceBufferStride :: Int, destinationBufferStride :: Int}
  | BufferCopyOverlap
      { sourceCopyElementOffset :: Int
      , destinationCopyElementOffset :: Int
      , copyElementCount :: Int
      }
  deriving (Eq, Show)

instance Exception VpipeError where
  displayException (NoVulkanIcd detail) =
    "Vulkan could not create an instance (" <> detail <> "). Install a Vulkan ICD; for headless Linux tests, install Mesa lavapipe."
  displayException (NoSuitableDevice rejections) =
    "No suitable Vulkan 1.3 device was found. Candidate rejections: "
      <> show rejections
      <> ". Install/update a Vulkan driver, or use Mesa lavapipe for headless testing."
  displayException (RequiredInstanceExtensionsUnavailable extensions) =
    "Required Vulkan instance extensions are unavailable: "
      <> show extensions
      <> ". Install the window-system Vulkan integration. For GLFW, create the context with Vpipe.GLFW.withWindow; custom integrations must pass Vpipe.GLFW.requiredInstanceExtensions before instance creation."
  displayException (ValidationUnavailable reason) =
    "Vulkan validation was requested strictly, but could not be enabled: " <> reason
  displayException (ValidationFailed count dropped) =
    "Strict Vulkan validation captured " <> show count <> " message(s) and dropped " <> show dropped <> " message(s)."
  displayException TimelineValueExhausted = "The queue timeline semaphore reached Word64 maximum; recreate the context before submitting more work."
  displayException (TimelineValueDifferenceExceeded completed requested maximumDifference) =
    "Timeline signal "
      <> show requested
      <> " is too far ahead of completed value "
      <> show completed
      <> "; this device permits a maximum difference of "
      <> show maximumDifference
      <> "."
  displayException (TimelineValueNotSubmitted awaited submitted) =
    "Cannot wait for queue timeline value "
      <> show awaited
      <> " because only values through "
      <> show submitted
      <> " have been submitted."
  displayException ContextClosed = "This vpipe context is closing or already closed; no new resources can be created."
  displayException ResourceQuarantined =
    "A queue submission may have been accepted, so vpipe quarantined the affected resource instead of reusing or destroying it. Recreate the context before continuing."
  displayException (CleanupFailed failures) = "vpipe cleanup reported failures: " <> show failures
  displayException (ShaderCompileBug detail dumpPath) =
    "vpipe generated an invalid shader ("
      <> detail
      <> "). This is a vpipe bug; please file it and attach the SPIR-V artifact at "
      <> dumpPath
      <> "."
  displayException (VulkanFailure operation result) = operation <> " failed with " <> result <> "."
  displayException DeviceLost = "The Vulkan device was lost. Capture with RenderDoc and retry; this may indicate a driver bug."
  displayException SurfaceLost = "The Vulkan surface was lost. Recreate the surface and swapchain."
  displayException (SwapchainFormatUnavailable formats) =
    "The surface does not advertise the required B8G8R8A8_SRGB/SRGB_NONLINEAR format pair. Available formats: " <> show formats
  displayException (SwapchainUsageUnsupported usage) =
    "The surface cannot use swapchain images as color attachments. Supported usage flags: " <> usage
  displayException SwapchainExtentUnavailable =
    "The surface requires an application-selected extent, but no framebuffer extent provider is available."
  displayException SurfaceContextMismatch = "The surface belongs to a different vpipe context."
  displayException SwapchainReleased = "This swapchain has already been released."
  displayException SwapchainPoisoned = "This swapchain is poisoned after an unrecoverable presentation failure. Recreate it."
  displayException FrameExpired = "This frame belongs to an expired swapchain generation."
  displayException FrameDynamicBufferDomainMismatch =
    "This frame-dynamic buffer belongs to a different swapchain; create one FrameDynamicBuffer per swapchain."
  displayException FrameDynamicBufferAlreadyUsed =
    "A FrameDynamicBuffer may be written once per frame; put every use of its binding inside one withDynamic scope."
  displayException (InvalidFramesInFlight count) = "Swapchain framesInFlight must be positive, but was " <> show count <> "."
  displayException (BufferElementRangeInvalid offset count capacity) =
    "Buffer element range offset "
      <> show offset
      <> " and count "
      <> show count
      <> " is outside capacity "
      <> show capacity
      <> "."
  displayException (BufferElementCountInvalid elements) =
    "Buffer element count must be positive, but was " <> show elements <> "."
  displayException (BufferSizeOverflow elements elementBytes) =
    "Buffer size overflows Int: " <> show elements <> " elements of " <> show elementBytes <> " bytes."
  displayException BufferReleased = "This buffer has already been released; create a new buffer before using it again."
  displayException ImageReleased = "This image has already been released; create a new image before using it again."
  displayException (DynamicBufferDescriptorOffsetMisaligned elementOffset byteOffset requiredAlignment) =
    "Dynamic buffer element offset "
      <> show elementOffset
      <> " begins at byte offset "
      <> show byteOffset
      <> ", which is not aligned to the required descriptor offset alignment of "
      <> show requiredAlignment
      <> " bytes. Choose an element offset whose byte offset is a multiple of "
      <> show requiredAlignment
      <> "."
  displayException (BufferCopyStrideMismatch source destination) =
    "copyPass requires identical source and destination element strides, but received "
      <> show source
      <> " and "
      <> show destination
      <> " bytes."
  displayException (BufferCopyOverlap sourceOffset destinationOffset count) =
    "copyPass does not allow overlapping ranges in the same buffer; source offset "
      <> show sourceOffset
      <> ", destination offset "
      <> show destinationOffset
      <> ", and count "
      <> show count
      <> " overlap."
