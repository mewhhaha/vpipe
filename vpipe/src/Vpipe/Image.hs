{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- | Typed, device-local Vulkan images. Layout transitions and transfer
synchronization are recorded internally for each mip/layer subresource.

An image extent and promoted format determine the resource type; transfers
use ordinary host lists of texels:

@
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Linear (V4 (..))
import Vpipe.Context (defaultVpipeConfig, withVpipe)
import Vpipe.Format (Format (R32G32B32A32Sfloat))
import Vpipe.Image (Image, ImageSubresource (..), destroyImage, imageExtent2D, newImage, readImage, writeImage)
import Vpipe.Image.Types (Dim (D2), ImageUsage (CopyDst, CopySrc))

main :: IO ()
main = withVpipe defaultVpipeConfig $ \context -> do
  image <- newImage context (imageExtent2D 1 1) 1 1 :: IO (Image 'D2 'R32G32B32A32Sfloat '[ 'CopySrc, 'CopyDst])
  writeImage image (ImageSubresource 0 0) [V4 1 0 0 1]
  print =<< readImage image (ImageSubresource 0 0)
  destroyImage image
@
-}
module Vpipe.Image (
  Image,
  ImageExtent,
  ImageSubresource (..),
  ImageSubresourceOutOfBounds (..),
  imageExtent1D,
  imageExtent2D,
  imageExtent3D,
  imageExtentCube,
  imageExtent2DArray,
  imageWidth,
  imageHeight,
  imageDepth,
  ImageFormat (..),
  newImage,
  destroyImage,
  writeImage,
  readImage,
  generateMips,
  imageExtent,
  imageMipLevels,
  imageArrayLayers,
) where

import Control.Concurrent.MVar (MVar, modifyMVarMasked_, newEmptyMVar, newMVar, putMVar, readMVar, withMVar)
import Control.Exception (SomeException, bracket, catch, finally, mask, mask_, onException, throwIO, try)
import Control.Monad (foldM, unless, void, when)
import Data.Bits ((.&.), (.|.))
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Data.Vector qualified as Vector
import Data.Word (Word32, Word64, Word8)
import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr (Ptr)
import Foreign.Ptr qualified as Ptr
import Foreign.Storable (Storable, peek, poke)
import Foreign.Storable qualified as Storable
import Linear (V2, V3, V4)
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.Buffer qualified as Buffer
import Vulkan.Core10.CommandBuffer qualified as CommandBuffer
import Vulkan.Core10.CommandBufferBuilding qualified as Command
import Vulkan.Core10.CommandPool qualified as CommandPool
import Vulkan.Core10.DeviceInitialization qualified as Device
import Vulkan.Core10.Enums.BufferUsageFlagBits qualified as BufferUsage
import Vulkan.Core10.Enums.CommandBufferLevel qualified as CommandLevel
import Vulkan.Core10.Enums.CommandBufferUsageFlagBits qualified as CommandUsage
import Vulkan.Core10.Enums.CommandPoolCreateFlagBits qualified as PoolUsage
import Vulkan.Core10.Enums.Filter qualified as Filter
import Vulkan.Core10.Enums.FormatFeatureFlagBits qualified as FormatFeature
import Vulkan.Core10.Enums.ImageAspectFlagBits qualified as Aspect
import Vulkan.Core10.Enums.ImageLayout qualified as Layout
import Vulkan.Core10.Enums.ImageTiling qualified as Tiling
import Vulkan.Core10.Enums.ImageUsageFlagBits qualified as Usage
import Vulkan.Core10.Enums.ImageViewType qualified as ViewType
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Enums.SampleCountFlagBits qualified as Samples
import Vulkan.Core10.Enums.SharingMode qualified as Sharing
import Vulkan.Core10.FundamentalTypes qualified as Vk
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Core10.Image qualified as Vk
import Vulkan.Core10.ImageView qualified as View
import Vulkan.Core13.Enums.AccessFlags2 qualified as Access2
import Vulkan.Core13.Enums.PipelineStageFlags2 qualified as Stage2
import Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 qualified as Sync2
import Vulkan.Exception qualified as Vulkan
import Vulkan.Zero (zero)
import VulkanMemoryAllocator qualified as VMA

import Vpipe.Buffer.Staging qualified as Staging
import Vpipe.Context.Internal (Context, contextAllocator, contextDevice, contextPhysicalDevice, contextQueueFamilyIndices, contextStagingRuntime, derivedObjectName, graphicsQueue, logImageSubresourceTransition, registerContextFinalizerLeased, setObjectNameLeased, transferQueue, withContextLease)
import Vpipe.Context.Queue.Internal (Queue, QueueDependency (..), SubmissionPublicationOutcome (..), queueFamilyIndex, submitCommandBuffersWithPublicationLeased, waitTimelineLeased)
import Vpipe.Error (VpipeError (..))
import Vpipe.Format (Format (..), KnownFormat (..))
import Vpipe.Graphics.Submission.Internal (OwnedActions, SubmittedWorkStatus (..), confirmSubmittedWork, newOwnedActions, releaseOwnedActions, retireOwnedActions, transferOwnedActions)
import Vpipe.Image.Internal (Image (..), quarantineImageBinding, withImageLifetimeLease)
import Vpipe.Image.State qualified as State
import Vpipe.Image.Types (Dim (..), HasImageUsage, ImageSubresource (..), ImageSubresourceOutOfBounds (..), ImageUsage (CopyDst, CopySrc), KnownDim (..), KnownImageUsages (..), ValidImageUsages)
import Vpipe.Resource.Lifetime qualified as Lifetime

data ImageExtent (dim :: Dim) = ImageExtent !Int !Int !Int
  deriving (Eq, Show)

imageExtent1D :: Int -> ImageExtent 'D1
imageExtent1D width = ImageExtent width 1 1

imageExtent2D :: Int -> Int -> ImageExtent 'D2
imageExtent2D width height = ImageExtent width height 1

imageExtent3D :: Int -> Int -> Int -> ImageExtent 'D3
imageExtent3D = ImageExtent

imageExtentCube :: Int -> ImageExtent 'Cube
imageExtentCube edge = ImageExtent edge edge 1

imageExtent2DArray :: Int -> Int -> ImageExtent 'D2Array
imageExtent2DArray width height = ImageExtent width height 1

imageWidth, imageHeight, imageDepth :: ImageExtent dim -> Int
imageWidth (ImageExtent width _ _) = width
imageHeight (ImageExtent _ height _) = height
imageDepth (ImageExtent _ _ depth) = depth

class (KnownFormat format, Storable (ImageTexel format)) => ImageFormat (format :: Format) where
  type ImageTexel format
  imageTexelAspect :: Proxy format -> Aspect.ImageAspectFlags

instance ImageFormat 'R8Unorm where
  type ImageTexel 'R8Unorm = Word8
  imageTexelAspect _ = Aspect.IMAGE_ASPECT_COLOR_BIT
instance ImageFormat 'R8G8B8A8Unorm where
  type ImageTexel 'R8G8B8A8Unorm = V4 Word8
  imageTexelAspect _ = Aspect.IMAGE_ASPECT_COLOR_BIT
instance ImageFormat 'R8G8B8A8Srgb where
  type ImageTexel 'R8G8B8A8Srgb = V4 Word8
  imageTexelAspect _ = Aspect.IMAGE_ASPECT_COLOR_BIT
instance ImageFormat 'B8G8R8A8Unorm where
  type ImageTexel 'B8G8R8A8Unorm = V4 Word8
  imageTexelAspect _ = Aspect.IMAGE_ASPECT_COLOR_BIT
instance ImageFormat 'B8G8R8A8Srgb where
  type ImageTexel 'B8G8R8A8Srgb = V4 Word8
  imageTexelAspect _ = Aspect.IMAGE_ASPECT_COLOR_BIT
instance ImageFormat 'R32Sfloat where
  type ImageTexel 'R32Sfloat = Float
  imageTexelAspect _ = Aspect.IMAGE_ASPECT_COLOR_BIT
instance ImageFormat 'R32G32Sfloat where
  type ImageTexel 'R32G32Sfloat = V2 Float
  imageTexelAspect _ = Aspect.IMAGE_ASPECT_COLOR_BIT
instance ImageFormat 'R32G32B32Sfloat where
  type ImageTexel 'R32G32B32Sfloat = V3 Float
  imageTexelAspect _ = Aspect.IMAGE_ASPECT_COLOR_BIT
instance ImageFormat 'R32G32B32A32Sfloat where
  type ImageTexel 'R32G32B32A32Sfloat = V4 Float
  imageTexelAspect _ = Aspect.IMAGE_ASPECT_COLOR_BIT
instance ImageFormat 'D32Sfloat where
  type ImageTexel 'D32Sfloat = Float
  imageTexelAspect _ = Aspect.IMAGE_ASPECT_DEPTH_BIT

newImage :: forall dim format usages. (KnownDim dim, ImageFormat format, KnownImageUsages usages, ValidImageUsages format usages) => Context -> ImageExtent dim -> Int -> Int -> IO (Image dim format usages)
newImage context extent mipLevels arrayLayers = withContextLease context $ mask $ \_ -> do
  validateImageDescription @dim extent mipLevels arrayLayers
  lock <- newMVar ()
  state <- State.newImageState (fromIntegral mipLevels) (fromIntegral arrayLayers)
  lifetimeGate <- Lifetime.newLifetimeGate
  generation <- Lifetime.newResourceGeneration
  let physicalExtent = extentToVk extent
      usageFlags = reflectedImageUsageFlags (Proxy @usages)
      families = contextQueueFamilyIndices context
      (sharing, queueFamilies) = if length families > 1 then (Sharing.SHARING_MODE_CONCURRENT, Vector.fromList families) else (Sharing.SHARING_MODE_EXCLUSIVE, Vector.empty)
      createInfo =
        (zero :: Vk.ImageCreateInfo '[])
          { Vk.flags = reflectedImageCreateFlags (Proxy @dim)
          , Vk.imageType = reflectedImageType (Proxy @dim)
          , Vk.format = formatVal @format
          , Vk.extent = physicalExtent
          , Vk.mipLevels = fromIntegral mipLevels
          , Vk.arrayLayers = fromIntegral arrayLayers
          , Vk.samples = Samples.SAMPLE_COUNT_1_BIT
          , Vk.tiling = Tiling.IMAGE_TILING_OPTIMAL
          , Vk.usage = usageFlags
          , Vk.sharingMode = sharing
          , Vk.queueFamilyIndices = queueFamilies
          , Vk.initialLayout = Layout.IMAGE_LAYOUT_UNDEFINED
          }
      allocationInfo = deviceLocalImageAllocationCreateInfo
      allocator = contextAllocator context
  properties <-
    mapVma "vkGetPhysicalDeviceImageFormatProperties" $
      Device.getPhysicalDeviceImageFormatProperties
        (contextPhysicalDevice context)
        (formatVal @format)
        (reflectedImageType (Proxy @dim))
        Tiling.IMAGE_TILING_OPTIMAL
        usageFlags
        (reflectedImageCreateFlags (Proxy @dim))
  validateImageFormatProperties physicalExtent mipLevels arrayLayers properties
  (handle, allocation, _) <- mapVma "vmaCreateImage" (VMA.createImage allocator createInfo allocationInfo)
  setObjectNameLeased context ObjectType.OBJECT_TYPE_IMAGE (imageHandleWord handle) (derivedObjectName "image" (imageHandleWord handle))
    `onException` VMA.destroyImage allocator handle allocation
  let range = View.ImageSubresourceRange (imageTexelAspect (Proxy @format)) 0 (fromIntegral mipLevels) 0 (fromIntegral arrayLayers)
      viewInfo = (zero :: View.ImageViewCreateInfo '[]){View.image = handle, View.viewType = reflectedImageViewType (Proxy @dim), View.format = formatVal @format, View.subresourceRange = range}
  view <-
    if imageUsageNeedsView usageFlags
      then Just <$> (mapVma "vkCreateImageView" (View.createImageView (contextDevice context) viewInfo Nothing) `onException` VMA.destroyImage allocator handle allocation)
      else pure Nothing
  traverse_
    (\viewHandle -> setObjectNameLeased context ObjectType.OBJECT_TYPE_IMAGE_VIEW (imageViewHandleWord viewHandle) (derivedObjectName "image-view" (imageViewHandleWord viewHandle)))
    view
    `onException` (traverse_ (\viewHandle -> View.destroyImageView (contextDevice context) viewHandle Nothing) view >> VMA.destroyImage allocator handle allocation)
  released <- newMVar False
  let destroyView = traverse_ (\handle' -> View.destroyImageView (contextDevice context) handle' Nothing) view
      release = do
        Lifetime.sealLifetimeGate lifetimeGate
        releaseOnce released (destroyView >> VMA.destroyImage allocator handle allocation)
  registerContextFinalizerLeased context release `onException` release
  pure Image{imageContext = context, imageAllocator = allocator, imageHandle = handle, imageView = view, imageAllocation = allocation, imageRawExtent3D = physicalExtent, imageRawFormat = formatVal @format, imageRawAspect = imageTexelAspect (Proxy @format), imageRawUsageFlags = usageFlags, imageMipCount = mipLevels, imageLayerCount = arrayLayers, imageState = state, imageLock = lock, imageGeneration = generation, imageLifetimeGate = lifetimeGate, imageReleased = released, imageRelease = release}

imageHandleWord :: Handles.Image -> Word64
imageHandleWord (Handles.Image handle) = handle

imageViewHandleWord :: Handles.ImageView -> Word64
imageViewHandleWord (Handles.ImageView handle) = handle

destroyImage :: Image dim format usages -> IO ()
destroyImage image = withContextLease (imageContext image) $ mask_ $ do
  Lifetime.closeLifetimeGate (imageLifetimeGate image)
  withMVar (imageLock image) $ \_ -> do
    previous <- State.allImageUses (imageState image)
    traverse_ awaitImageUse previous
    imageRelease image

writeImage :: forall dim format usages. (ImageFormat format, HasImageUsage 'CopyDst usages) => Image dim format usages -> ImageSubresource -> [ImageTexel format] -> IO ()
writeImage image subresource texels = withImageLease image $ \queue -> mask $ \_ -> do
  reservation <- State.beginImageUse (imageState image) [subresource]
  reservationCleanup <- newOwnedActions [void (State.cancelImageUse reservation)]
  ( do
      checkTexelCount image subresource texels
      bytes <- checkedImageByteCount (length texels) (imageTexelBytes @format)
      let dependencies = imageDependencies Stage2.PIPELINE_STAGE_2_TRANSFER_BIT (State.reservationPreviousUses reservation)
      let sourceAlignment = lcm 4 (imageTexelBytes @format)
      submission <-
        Staging.submitUploadCommandAfter (contextStagingRuntime (imageContext image)) dependencies sourceAlignment bytes (pokeTexels texels) $ \source sourceOffset commandBuffer -> do
          imageBarrier image queue (State.reservationPreviousUses reservation) Layout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Access2.ACCESS_2_TRANSFER_WRITE_BIT commandBuffer
          Command.cmdCopyBufferToImage commandBuffer source (imageHandle image) Layout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL (Vector.singleton (copyRegion sourceOffset image subresource))
      case submission of
        Staging.StagingSubmissionAccepted signal ->
          commitAcceptedImageUse image reservationCleanup reservation queue Layout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Access2.ACCESS_2_TRANSFER_WRITE_BIT signal
        Staging.StagingSubmissionAcceptedPublicationFailed signal primaryFailure ->
          rethrowAfterImageUsePublication primaryFailure image reservationCleanup reservation queue Layout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Access2.ACCESS_2_TRANSFER_WRITE_BIT signal
        Staging.StagingSubmissionAcceptanceUnknown primaryFailure ->
          quarantineUnknownImageSubmissionLeased primaryFailure image reservationCleanup
    )
    `finally` releaseOwnedActions reservationCleanup

readImage :: forall dim format usages. (ImageFormat format, HasImageUsage 'CopySrc usages) => Image dim format usages -> ImageSubresource -> IO [ImageTexel format]
readImage image subresource = withImageLease image $ \queue -> mask $ \restore -> do
  reservation <- State.beginImageUse (imageState image) [subresource]
  reservationCleanup <- newOwnedActions [void (State.cancelImageUse reservation)]
  ( do
      let count = texelCount image subresource
      bytes <- checkedImageByteCount count (imageTexelBytes @format)
      (buffer, allocation, cleanup) <- readbackBuffer image bytes
      retirementGate <- newEmptyMVar
      let retire = readMVar retirementGate >> cleanup
      submission <-
        Staging.submitCommandAfter
          (contextStagingRuntime (imageContext image))
          (imageDependencies Stage2.PIPELINE_STAGE_2_TRANSFER_BIT (State.reservationPreviousUses reservation))
          ( \commandBuffer -> do
              imageBarrier image queue (State.reservationPreviousUses reservation) Layout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL Access2.ACCESS_2_TRANSFER_READ_BIT commandBuffer
              -- VMA may reuse a retired readback allocation. Queue order
              -- alone does not make writes to aliased memory available to
              -- the next transfer destination.
              let aliasBarrier =
                    Sync2.MemoryBarrier2
                      { Sync2.srcStageMask = Stage2.PIPELINE_STAGE_2_ALL_COMMANDS_BIT
                      , Sync2.srcAccessMask = Access2.ACCESS_2_MEMORY_WRITE_BIT
                      , Sync2.dstStageMask = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
                      , Sync2.dstAccessMask = Access2.ACCESS_2_TRANSFER_WRITE_BIT
                      }
                  dependency =
                    (zero :: Sync2.DependencyInfo)
                      { Sync2.memoryBarriers = Vector.singleton aliasBarrier
                      }
              Sync2.cmdPipelineBarrier2 commandBuffer dependency
              Command.cmdCopyImageToBuffer commandBuffer (imageHandle image) Layout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL buffer (Vector.singleton (copyRegion 0 image subresource))
              recordHostReadBarrier buffer 0 bytes commandBuffer
          )
          retire
          `onException` cleanup
      case submission of
        Staging.StagingSubmissionAcceptanceUnknown primaryFailure -> do
          bestEffort (putMVar retirementGate ())
          quarantineUnknownImageSubmissionLeased primaryFailure image reservationCleanup
        Staging.StagingSubmissionAcceptedPublicationFailed signal primaryFailure -> do
          bestEffort (putMVar retirementGate ())
          rethrowAfterImageUsePublication primaryFailure image reservationCleanup reservation queue Layout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL Access2.ACCESS_2_TRANSFER_READ_BIT signal
        Staging.StagingSubmissionAccepted signal -> do
          values <-
            ( do
                commitAcceptedImageUse image reservationCleanup reservation queue Layout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL Access2.ACCESS_2_TRANSFER_READ_BIT signal
                restore (waitTimelineLeased queue signal)
                bracket (mapVma "vmaMapMemory(image readback)" (VMA.mapMemory (imageAllocator image) allocation)) (const (VMA.unmapMemory (imageAllocator image) allocation)) $ \pointer -> do
                  mapVma "vmaInvalidateAllocation(image readback)" (VMA.invalidateAllocation (imageAllocator image) allocation 0 (fromIntegral bytes))
                  restore (peekTexels pointer count)
            )
              `finally` putMVar retirementGate ()
          Staging.reclaimStaging (contextStagingRuntime (imageContext image))
          pure values
    )
    `finally` releaseOwnedActions reservationCleanup

generateMips :: forall dim format usages. (ImageFormat format, HasImageUsage 'CopySrc usages, HasImageUsage 'CopyDst usages) => Image dim format usages -> Int -> IO ()
generateMips image layer = withImageLease image $ \_ -> mask $ \restore -> do
  when (layer < 0 || layer >= imageLayerCount image) (throwIO (VulkanFailure "generateMips" "array layer is out of bounds"))
  unless (imageMipCount image < 2) $ do
    checkLinearBlit image
    let subs = [ImageSubresource (fromIntegral mip) (fromIntegral layer) | mip <- [0 .. imageMipCount image - 1]]
    reservation <- State.beginImageUse (imageState image) subs
    reservationCleanup <- newOwnedActions [void (State.cancelImageUse reservation)]
    ( do
        let queue = graphicsQueue (imageContext image)
        (submission, poolCleanup) <- submitGraphicsMips image queue reservation layer
        handleMipSubmission restore image queue reservationCleanup poolCleanup submission
          `finally` releaseOwnedActions poolCleanup
      )
      `finally` releaseOwnedActions reservationCleanup

handleMipSubmission :: (IO () -> IO ()) -> Image dim format usages -> Queue -> OwnedActions -> OwnedActions -> SubmissionPublicationOutcome -> IO ()
handleMipSubmission restore image queue reservationCleanup poolCleanup submission =
  case submission of
    SubmissionRejected primaryFailure -> throwIO primaryFailure
    SubmissionAcceptanceUnknown primaryFailure ->
      quarantineUnknownMipLeased primaryFailure image reservationCleanup poolCleanup
    SubmissionAcceptedPublicationFailed signal primaryFailure -> do
      let wait = waitTimelineLeased queue signal
      completion <- confirmSubmittedWork (restore wait) wait
      case completion of
        SubmittedWorkComplete -> throwIO primaryFailure
        SubmittedWorkCompleteAfterFailure _waitFailure -> throwIO primaryFailure
        SubmittedWorkUncertain _waitFailure _fallbackFailure ->
          quarantineUnknownMipLeased primaryFailure image reservationCleanup poolCleanup
    SubmissionAccepted signal -> do
      let wait = waitTimelineLeased queue signal
      completion <- confirmSubmittedWork (restore wait) wait
      case completion of
        SubmittedWorkComplete -> pure ()
        SubmittedWorkCompleteAfterFailure primaryFailure -> throwIO primaryFailure
        SubmittedWorkUncertain primaryFailure _fallbackFailure -> do
          void $
            retireOwnedActions
              (registerContextFinalizerLeased (imageContext image))
              [poolCleanup]
          throwIO primaryFailure

quarantineUnknownMipLeased :: SomeException -> Image dim format usages -> OwnedActions -> OwnedActions -> IO a
quarantineUnknownMipLeased primaryFailure image reservationCleanup poolCleanup = mask_ $ do
  publishUnknownImageSubmissionLeased image reservationCleanup
  void $
    retireOwnedActions
      (registerContextFinalizerLeased (imageContext image))
      [poolCleanup]
  throwIO primaryFailure

quarantineUnknownImageSubmissionLeased :: SomeException -> Image dim format usages -> OwnedActions -> IO a
quarantineUnknownImageSubmissionLeased primaryFailure image reservationCleanup = mask_ $ do
  publishUnknownImageSubmissionLeased image reservationCleanup
  throwIO primaryFailure

commitAcceptedImageUse :: Image dim format usages -> OwnedActions -> State.ImageReservation -> Queue -> Layout.ImageLayout -> Access2.AccessFlags2 -> Word64 -> IO ()
commitAcceptedImageUse image reservationCleanup reservation queue layout access signal = mask_ $ do
  publication <- try (commit image reservation queue layout access signal)
  case publication of
    Right () -> pure ()
    Left (primaryFailure :: SomeException) ->
      quarantineUnknownImageSubmissionLeased primaryFailure image reservationCleanup

rethrowAfterImageUsePublication :: SomeException -> Image dim format usages -> OwnedActions -> State.ImageReservation -> Queue -> Layout.ImageLayout -> Access2.AccessFlags2 -> Word64 -> IO a
rethrowAfterImageUsePublication primaryFailure image reservationCleanup reservation queue layout access signal = mask_ $ do
  publication <- try (commit image reservation queue layout access signal)
  case publication of
    Right () -> throwIO primaryFailure
    Left (_publicationFailure :: SomeException) ->
      quarantineUnknownImageSubmissionLeased primaryFailure image reservationCleanup

publishUnknownImageSubmissionLeased :: Image dim format usages -> OwnedActions -> IO ()
publishUnknownImageSubmissionLeased image reservationCleanup = do
  bestEffort (void (transferOwnedActions reservationCleanup))
  bestEffort (quarantineImageBinding image)

imageExtent :: Image dim format usages -> ImageExtent dim
imageExtent image = let Vk.Extent3D width height depth = imageRawExtent3D image in ImageExtent (fromIntegral width) (fromIntegral height) (fromIntegral depth)
imageMipLevels :: Image dim format usages -> Int
imageMipLevels = imageMipCount
imageArrayLayers :: Image dim format usages -> Int
imageArrayLayers = imageLayerCount

withImageLease :: Image dim format usages -> (Queue -> IO a) -> IO a
withImageLease image action = withImageLifetimeLease image $ withMVar (imageLock image) $ \_ ->
  action (transferQueue (imageContext image))

validateImageDescription :: forall dim. (KnownDim dim) => ImageExtent dim -> Int -> Int -> IO ()
validateImageDescription extent mipLevels layers
  | any (<= 0) dimensions = invalid "image extent dimensions must be positive"
  | any (> fromIntegral (maxBound :: Word32)) dimensions = invalid "image extent exceeds Vulkan's Word32 dimensions"
  | mipLevels <= 0 = invalid "mip level count must be positive"
  | mipLevels > maximumMipLevels extent = invalid ("mip level count exceeds the extent maximum of " <> show (maximumMipLevels extent))
  | layers <= 0 = invalid "array layer count must be positive"
  | layers > fromIntegral (maxBound :: Word32) = invalid "array layer count exceeds Vulkan's Word32 limit"
  | viewType == ViewType.IMAGE_VIEW_TYPE_1D && (imageHeight extent /= 1 || imageDepth extent /= 1 || layers /= 1) = invalid "D1 images require height=1, depth=1, and one array layer"
  | viewType == ViewType.IMAGE_VIEW_TYPE_2D && (imageDepth extent /= 1 || layers /= 1) = invalid "D2 images require depth=1 and one array layer"
  | viewType == ViewType.IMAGE_VIEW_TYPE_3D && layers /= 1 = invalid "D3 images require one array layer"
  | viewType == ViewType.IMAGE_VIEW_TYPE_CUBE && (imageWidth extent /= imageHeight extent || imageDepth extent /= 1 || layers /= 6) = invalid "cube images require a square extent, depth=1, and exactly six array layers"
  | viewType == ViewType.IMAGE_VIEW_TYPE_2D_ARRAY && imageDepth extent /= 1 = invalid "D2Array images require depth=1"
  | otherwise = void (checkedProduct dimensions)
 where
  dimensions = [imageWidth extent, imageHeight extent, imageDepth extent]
  viewType = reflectedImageViewType (Proxy @dim)
  invalid = throwIO . VulkanFailure "newImage"

validateImageFormatProperties :: Vk.Extent3D -> Int -> Int -> Device.ImageFormatProperties -> IO ()
validateImageFormatProperties requestedExtent requestedMipLevels requestedLayers properties = do
  let Vk.Extent3D requestedWidth requestedHeight requestedDepth = requestedExtent
      Vk.Extent3D maximumWidth maximumHeight maximumDepth = Device.maxExtent properties
      exceedsExtent = requestedWidth > maximumWidth || requestedHeight > maximumHeight || requestedDepth > maximumDepth
  when exceedsExtent (invalid "image extent exceeds the physical device's format/usage limit")
  when (fromIntegral requestedMipLevels > Device.maxMipLevels properties) (invalid "mip level count exceeds the physical device's format/usage limit")
  when (fromIntegral requestedLayers > Device.maxArrayLayers properties) (invalid "array layer count exceeds the physical device's format/usage limit")
  unless (Device.sampleCounts properties .&. Samples.SAMPLE_COUNT_1_BIT == Samples.SAMPLE_COUNT_1_BIT) (invalid "format/usage combination does not support single-sample images")
 where
  invalid = throwIO . VulkanFailure "newImage"

imageUsageNeedsView :: Usage.ImageUsageFlags -> Bool
imageUsageNeedsView usageFlags = usageFlags .&. viewUsages /= zero
 where
  viewUsages =
    Usage.IMAGE_USAGE_SAMPLED_BIT
      .|. Usage.IMAGE_USAGE_STORAGE_BIT
      .|. Usage.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
      .|. Usage.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT

maximumMipLevels :: ImageExtent dim -> Int
maximumMipLevels extent = 1 + floorLog2 (maximum [imageWidth extent, imageHeight extent, imageDepth extent])

floorLog2 :: Int -> Int
floorLog2 value
  | value <= 1 = 0
  | otherwise = 1 + floorLog2 (value `div` 2)

checkedProduct :: [Int] -> IO Int
checkedProduct = foldM step 1
 where
  step current next =
    if next > maxBound `div` current
      then throwIO (VulkanFailure "image size" "image texel count overflows Int")
      else pure (current * next)

extentToVk :: ImageExtent dim -> Vk.Extent3D
extentToVk extent = Vk.Extent3D (fromIntegral (imageWidth extent)) (fromIntegral (imageHeight extent)) (fromIntegral (imageDepth extent))

imageTexelBytes :: forall format. (ImageFormat format) => Int
imageTexelBytes = Storable.sizeOf (undefined :: ImageTexel format)

pokeTexels :: (Storable a) => [a] -> Ptr () -> IO ()
pokeTexels values pointer = go values (Ptr.castPtr pointer)
 where
  go [] _ = pure (); go (value : rest) target = poke target value >> go rest (advancePtr target 1)
peekTexels :: (Storable a) => Ptr () -> Int -> IO [a]
peekTexels pointer = go (Ptr.castPtr pointer)
 where
  go _ 0 = pure []; go target remaining = (:) <$> peek target <*> go (advancePtr target 1) (remaining - 1)

checkTexelCount :: forall dim format usages. (ImageFormat format) => Image dim format usages -> ImageSubresource -> [ImageTexel format] -> IO ()
checkTexelCount image subresource values = do
  let expected = texelCount image subresource
  unless (length values == expected) (throwIO (VulkanFailure "writeImage" ("expected " <> show expected <> " texels, received " <> show (length values))))

texelCount :: Image dim format usages -> ImageSubresource -> Int
texelCount image (ImageSubresource mip _) = fromIntegral width * fromIntegral height * fromIntegral depth
 where
  ImageExtent baseWidth baseHeight baseDepth = imageExtent image
  divisor = 2 ^ (fromIntegral mip :: Int)
  width = max 1 (baseWidth `div` divisor)
  height = max 1 (baseHeight `div` divisor)
  depth = max 1 (baseDepth `div` divisor)

checkedImageByteCount :: Int -> Int -> IO Int
checkedImageByteCount texels bytesPerTexel
  | texels > maxBound `div` bytesPerTexel = throwIO (VulkanFailure "image size" "image byte count overflows Int")
  | otherwise = pure (texels * bytesPerTexel)

copyRegion :: forall dim format usages. (ImageFormat format) => Int -> Image dim format usages -> ImageSubresource -> Command.BufferImageCopy
copyRegion offset image (ImageSubresource mip layer) = Command.BufferImageCopy (fromIntegral offset) 0 0 (Command.ImageSubresourceLayers (imageTexelAspect (Proxy @format)) mip layer 1) (Vk.Offset3D 0 0 0) mipExtent
 where
  ImageExtent width height depth = imageExtent image
  scale value = fromIntegral (max 1 (value `div` (2 ^ (fromIntegral mip :: Int))))
  mipExtent = Vk.Extent3D (scale width) (scale height) (scale depth)

imageBarrier :: forall dim format usages. (ImageFormat format) => Image dim format usages -> Queue -> [(ImageSubresource, Maybe State.ImageUse)] -> Layout.ImageLayout -> Access2.AccessFlags2 -> Handles.CommandBuffer -> IO ()
imageBarrier image queue previousUses destinationLayout destinationAccess commandBuffer = do
  traverse_ logTransition previousUses
  Sync2.cmdPipelineBarrier2 commandBuffer ((zero :: Sync2.DependencyInfo){Sync2.imageMemoryBarriers = Vector.fromList (fmap barrier previousUses)})
 where
  logTransition (ImageSubresource mip layer, previous) =
    logImageSubresourceTransition
      (imageContext image)
      (imageHandleWord (imageHandle image))
      (maybe Layout.IMAGE_LAYOUT_UNDEFINED State.imageUseLayout previous)
      destinationLayout
      mip
      1
      layer
      1
  barrier (ImageSubresource mip layer, previous) =
    let sameQueueFamily use = maybe True ((== queueFamilyIndex queue) . State.imageCompletionQueueFamily) (State.imageUseCompletion use)
        sourceStage = maybe zero (\use -> if sameQueueFamily use then State.imageUseStage use else zero) previous
        sourceAccess = maybe zero (\use -> if sameQueueFamily use then State.imageUseAccess use else zero) previous
     in Chain.SomeStruct ((zero :: Sync2.ImageMemoryBarrier2 '[]){Sync2.srcStageMask = sourceStage, Sync2.srcAccessMask = sourceAccess, Sync2.dstStageMask = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT, Sync2.dstAccessMask = destinationAccess, Sync2.oldLayout = maybe Layout.IMAGE_LAYOUT_UNDEFINED State.imageUseLayout previous, Sync2.newLayout = destinationLayout, Sync2.srcQueueFamilyIndex = 0xffffffff, Sync2.dstQueueFamilyIndex = 0xffffffff, Sync2.image = imageHandle image, Sync2.subresourceRange = View.ImageSubresourceRange (imageTexelAspect (Proxy @format)) mip 1 layer 1})

recordHostReadBarrier :: Handles.Buffer -> Int -> Int -> Handles.CommandBuffer -> IO ()
recordHostReadBarrier handle offset bytes commandBuffer = do
  let barrier =
        (zero :: Sync2.BufferMemoryBarrier2 '[])
          { Sync2.srcStageMask = Stage2.PIPELINE_STAGE_2_TRANSFER_BIT
          , Sync2.srcAccessMask = Access2.ACCESS_2_TRANSFER_WRITE_BIT
          , Sync2.dstStageMask = Stage2.PIPELINE_STAGE_2_HOST_BIT
          , Sync2.dstAccessMask = Access2.ACCESS_2_HOST_READ_BIT
          , Sync2.srcQueueFamilyIndex = 0xffffffff
          , Sync2.dstQueueFamilyIndex = 0xffffffff
          , Sync2.buffer = handle
          , Sync2.offset = fromIntegral offset
          , Sync2.size = fromIntegral bytes
          }
      dependency =
        (zero :: Sync2.DependencyInfo)
          { Sync2.bufferMemoryBarriers = Vector.singleton (Chain.SomeStruct barrier)
          }
  Sync2.cmdPipelineBarrier2 commandBuffer dependency

commit :: Image dim format usages -> State.ImageReservation -> Queue -> Layout.ImageLayout -> Access2.AccessFlags2 -> Word64 -> IO ()
commit _ reservation queue layout access signal = do
  let completion = State.ImageCompletion queue (queueFamilyIndex queue) signal
  committed <- State.commitImageUse reservation (State.ImageUse layout Stage2.PIPELINE_STAGE_2_TRANSFER_BIT access (Just completion))
  unless committed (throwIO (VulkanFailure "image operation" "stale image-state reservation"))

awaitImageUse :: State.ImageUse -> IO ()
awaitImageUse use =
  traverse_
    (\completion -> waitTimelineLeased (State.imageCompletionQueue completion) (State.imageCompletionTimeline completion))
    (State.imageUseCompletion use)

imageDependencies :: Stage2.PipelineStageFlags2 -> [(ImageSubresource, Maybe State.ImageUse)] -> [QueueDependency]
imageDependencies destinationStage previousUses =
  [ QueueDependency
      (State.imageCompletionQueue completion)
      (State.imageCompletionTimeline completion)
      destinationStage
  | (_, Just use) <- previousUses
  , Just completion <- [State.imageUseCompletion use]
  ]

recordMips :: forall dim format usages. (ImageFormat format) => Image dim format usages -> Queue -> State.ImageReservation -> Int -> Handles.CommandBuffer -> IO ()
recordMips image queue reservation layer commandBuffer = do
  imageBarrier image queue (State.reservationPreviousUses reservation) Layout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Access2.ACCESS_2_TRANSFER_WRITE_BIT commandBuffer
  mapM_ blit [0 .. imageMipCount image - 2]
  let finalMip = ImageSubresource (fromIntegral (imageMipCount image - 1)) (fromIntegral layer)
  imageBarrier image queue [(finalMip, Just (State.ImageUse Layout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Stage2.PIPELINE_STAGE_2_TRANSFER_BIT Access2.ACCESS_2_TRANSFER_WRITE_BIT Nothing))] Layout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL Access2.ACCESS_2_TRANSFER_READ_BIT commandBuffer
 where
  blit mip = do
    let source = ImageSubresource (fromIntegral mip) (fromIntegral layer)
        destination = ImageSubresource (fromIntegral (mip + 1)) (fromIntegral layer)
    imageBarrier image queue [(source, Just (State.ImageUse Layout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Stage2.PIPELINE_STAGE_2_TRANSFER_BIT Access2.ACCESS_2_TRANSFER_WRITE_BIT Nothing))] Layout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL Access2.ACCESS_2_TRANSFER_READ_BIT commandBuffer
    Command.cmdBlitImage commandBuffer (imageHandle image) Layout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL (imageHandle image) Layout.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL (Vector.singleton (blitRegion image source destination)) Filter.FILTER_LINEAR

{- | Blits are submitted through a graphics-family pool, rather than the
transfer staging pool: a dedicated transfer queue need not support blits.
-}
submitGraphicsMips :: (ImageFormat format) => Image dim format usages -> Queue -> State.ImageReservation -> Int -> IO (SubmissionPublicationOutcome, OwnedActions)
submitGraphicsMips image queue reservation layer = mask $ \_ -> do
  let device = contextDevice (imageContext image)
      poolInfo = (zero :: CommandPool.CommandPoolCreateInfo){CommandPool.flags = PoolUsage.COMMAND_POOL_CREATE_TRANSIENT_BIT, CommandPool.queueFamilyIndex = queueFamilyIndex queue}
  pool <- mapVma "vkCreateCommandPool(image mips)" (CommandPool.createCommandPool device poolInfo Nothing)
  setObjectNameLeased (imageContext image) ObjectType.OBJECT_TYPE_COMMAND_POOL (commandPoolHandleWord pool) (derivedObjectName "command-pool-image-mips" (commandPoolHandleWord pool))
    `onException` CommandPool.destroyCommandPool device pool Nothing
  poolCleanup <- newOwnedActions [CommandPool.destroyCommandPool device pool Nothing]
  ( do
      commandBuffers <- mapVma "vkAllocateCommandBuffers(image mips)" (CommandBuffer.allocateCommandBuffers device (CommandBuffer.CommandBufferAllocateInfo pool CommandLevel.COMMAND_BUFFER_LEVEL_PRIMARY 1))
      commandBuffer <- case Vector.toList commandBuffers of
        [value] -> pure value
        values -> throwIO (VulkanFailure "generateMips" ("expected one command buffer, received " <> show (length values)))
      setObjectNameLeased (imageContext image) ObjectType.OBJECT_TYPE_COMMAND_BUFFER (commandBufferHandleWord commandBuffer) (derivedObjectName "command-buffer-image-mips" (commandBufferHandleWord commandBuffer))
      mapVma "vkBeginCommandBuffer(image mips)" (CommandBuffer.beginCommandBuffer commandBuffer ((zero :: CommandBuffer.CommandBufferBeginInfo '[]){CommandBuffer.flags = CommandUsage.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT}))
      recordMips image queue reservation layer commandBuffer
      mapVma "vkEndCommandBuffer(image mips)" (CommandBuffer.endCommandBuffer commandBuffer)
      let dependencies = imageDependencies Stage2.PIPELINE_STAGE_2_TRANSFER_BIT (State.reservationPreviousUses reservation)
      submission <-
        submitCommandBuffersWithPublicationLeased
          queue
          dependencies
          []
          []
          (Vector.singleton commandBuffer)
          (commit image reservation queue Layout.IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL Access2.ACCESS_2_TRANSFER_READ_BIT)
      pure (submission, poolCleanup)
    )
    `onException` releaseOwnedActions poolCleanup

blitRegion :: forall dim format usages. (ImageFormat format) => Image dim format usages -> ImageSubresource -> ImageSubresource -> Command.ImageBlit
blitRegion image source destination = Command.ImageBlit (layers source) (offsets source) (layers destination) (offsets destination)
 where
  layers (ImageSubresource mip layer) = Command.ImageSubresourceLayers (imageTexelAspect (Proxy @format)) mip layer 1
  offsets subresource = (Vk.Offset3D 0 0 0, let Vk.Extent3D width height depth = regionExtent subresource in Vk.Offset3D (fromIntegral width) (fromIntegral height) (fromIntegral depth))
  regionExtent (ImageSubresource mip _) = let ImageExtent width height depth = imageExtent image; scale n = max 1 (n `div` (2 ^ (fromIntegral mip :: Int))) in Vk.Extent3D (fromIntegral (scale width)) (fromIntegral (scale height)) (fromIntegral (scale depth))

checkLinearBlit :: forall dim format usages. (ImageFormat format) => Image dim format usages -> IO ()
checkLinearBlit image = do
  properties <- Device.getPhysicalDeviceFormatProperties (contextPhysicalDevice (imageContext image)) (formatVal @format)
  let features = Device.optimalTilingFeatures properties
      required = FormatFeature.FORMAT_FEATURE_BLIT_SRC_BIT .|. FormatFeature.FORMAT_FEATURE_BLIT_DST_BIT .|. FormatFeature.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
  unless (features .&. required == required) (throwIO (VulkanFailure "generateMips" "format does not support filtered source/destination blits"))

readbackBuffer :: Image dim format usages -> Int -> IO (Buffer.Buffer, VMA.Allocation, IO ())
readbackBuffer image bytes = do
  let allocator = imageAllocator image
      createInfo = (zero :: Buffer.BufferCreateInfo '[]){Buffer.size = fromIntegral bytes, Buffer.usage = BufferUsage.BUFFER_USAGE_TRANSFER_DST_BIT}
      allocationInfo = (zero :: VMA.AllocationCreateInfo){VMA.usage = VMA.MEMORY_USAGE_AUTO_PREFER_HOST, VMA.flags = VMA.ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT}
  (handle, allocation, _) <- mapVma "vmaCreateBuffer(image readback)" (VMA.createBuffer allocator createInfo allocationInfo)
  setObjectNameLeased (imageContext image) ObjectType.OBJECT_TYPE_BUFFER (bufferHandleWord handle) (derivedObjectName "buffer-image-readback" (bufferHandleWord handle))
    `onException` VMA.destroyBuffer allocator handle allocation
  released <- newMVar False
  pure (handle, allocation, releaseOnce released (VMA.destroyBuffer allocator handle allocation))

bufferHandleWord :: Handles.Buffer -> Word64
bufferHandleWord (Handles.Buffer handle) = handle

commandPoolHandleWord :: Handles.CommandPool -> Word64
commandPoolHandleWord (Handles.CommandPool handle) = handle

commandBufferHandleWord :: Handles.CommandBuffer -> Word64
commandBufferHandleWord = fromIntegral . Ptr.ptrToWordPtr . Handles.commandBufferHandle

releaseOnce :: MVar Bool -> IO () -> IO ()
releaseOnce state release = modifyMVarMasked_ state (\released -> if released then pure True else release >> pure True)

bestEffort :: IO () -> IO ()
bestEffort action = action `catch` \(_ :: SomeException) -> pure ()

deviceLocalImageAllocationCreateInfo :: VMA.AllocationCreateInfo
deviceLocalImageAllocationCreateInfo =
  VMA.AllocationCreateInfo
    zero
    VMA.MEMORY_USAGE_AUTO_PREFER_DEVICE
    zero
    zero
    0
    zero
    Ptr.nullPtr
    0

mapVma :: String -> IO a -> IO a
mapVma operation action = action `catch` \(error' :: Vulkan.VulkanException) -> if Vulkan.vulkanExceptionResult error' == Result.ERROR_DEVICE_LOST then throwIO DeviceLost else throwIO (VulkanFailure operation (show (Vulkan.vulkanExceptionResult error')))
