{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}

-- | Vulkan handles intentionally kept out of the public image API.
module Vpipe.Image.Internal (
  Image (..),
  imageRawHandle,
  imageRawView,
  imageRawState,
  acquireImageBindingLease,
  quarantineImageBinding,
  withImageLifetimeLease,
) where

import Control.Concurrent.MVar (MVar)
import Control.Exception (mask_, throwIO)
import Vulkan.Core10.Enums.Format qualified as Format
import Vulkan.Core10.Enums.ImageAspectFlagBits qualified as Aspect
import Vulkan.Core10.Enums.ImageUsageFlagBits qualified as Usage
import Vulkan.Core10.FundamentalTypes qualified as Fundamental
import Vulkan.Core10.Image qualified as Vk
import Vulkan.Core10.ImageView qualified as Vk
import VulkanMemoryAllocator qualified as VMA

import Vpipe.Context.Internal (Context, withContextLease)
import Vpipe.Error (VpipeError (ImageReleased))
import Vpipe.Format (Format)
import Vpipe.Image.State (ImageState)
import Vpipe.Image.State qualified
import Vpipe.Image.Types (Dim, ImageUsage)
import Vpipe.Resource.Lifetime qualified as Lifetime

data Image (dim :: Dim) (format :: Format) (usages :: [ImageUsage]) = Image
  { imageContext :: Context
  , imageAllocator :: VMA.Allocator
  , imageHandle :: Vk.Image
  , imageView :: Maybe Vk.ImageView
  , imageAllocation :: VMA.Allocation
  , imageRawExtent3D :: Fundamental.Extent3D
  , imageRawFormat :: Format.Format
  , imageRawAspect :: Aspect.ImageAspectFlags
  , imageRawUsageFlags :: Usage.ImageUsageFlags
  , imageMipCount :: Int
  , imageLayerCount :: Int
  , imageState :: ImageState
  , imageLock :: MVar ()
  , imageGeneration :: Lifetime.ResourceGeneration
  , imageLifetimeGate :: Lifetime.LifetimeGate
  , imageReleased :: MVar Bool
  , imageRelease :: IO ()
  }

type role Image nominal nominal nominal

imageRawHandle :: Image dim format usages -> Vk.Image
imageRawHandle = imageHandle

imageRawView :: Image dim format usages -> Maybe Vk.ImageView
imageRawView = imageView

imageRawState :: Image dim format usages -> ImageState
imageRawState = imageState

acquireImageBindingLease :: Image dim format usages -> IO (IO ())
acquireImageBindingLease image = do
  lease <- Lifetime.acquireLifetimeLease (imageLifetimeGate image)
  maybe (throwIO ImageReleased) pure lease

quarantineImageBinding :: Image dim format usages -> IO ()
quarantineImageBinding image = mask_ $ do
  Vpipe.Image.State.quarantineImageState (imageState image)
  Lifetime.quarantineLifetimeGate (imageLifetimeGate image)

withImageLifetimeLease :: Image dim format usages -> IO a -> IO a
withImageLifetimeLease image action =
  withContextLease (imageContext image) $
    Lifetime.withLifetimeLease (imageLifetimeGate image) (throwIO ImageReleased) action
