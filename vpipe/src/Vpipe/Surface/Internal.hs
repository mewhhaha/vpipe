module Vpipe.Surface.Internal (
  Surface,
  SurfaceFactory (..),
  SurfaceSource (..),
  newSurface,
  surfaceHandle,
  surfacePresentQueue,
  surfaceFramebufferExtent,
  surfaceBelongsTo,
) where

import Data.ByteString qualified
import Data.List.NonEmpty qualified
import Data.Unique (Unique)
import Vulkan.Core10.Handles qualified
import Vulkan.Extensions.Handles qualified as Vk

import Vpipe.Context.Queue.Internal (Queue)

data SurfaceSource = SurfaceSource
  { sourceHandle :: Vk.SurfaceKHR
  , sourceFramebufferExtent :: Maybe (IO (Int, Int))
  }

data SurfaceFactory payload = SurfaceFactory
  { surfaceFactoryExtensions :: [Data.ByteString.ByteString]
  , acquireSurfaces :: Vulkan.Core10.Handles.Instance -> IO (payload, Data.List.NonEmpty.NonEmpty SurfaceSource)
  , releaseSurfacePayload :: payload -> IO ()
  }

{- | A surface is tied to the context which selected its presentation queue.
It is deliberately opaque so a future swapchain API can reject a surface
from another context before making a Vulkan call.
-}
data Surface = Surface
  { surfaceIdentity :: Unique
  , surfaceRawHandle :: Vk.SurfaceKHR
  , surfaceQueue :: Queue
  , surfaceExtentProvider :: Maybe (IO (Int, Int))
  }

newSurface :: Unique -> SurfaceSource -> Queue -> Surface
newSurface identity source queue = Surface identity (sourceHandle source) queue (sourceFramebufferExtent source)

surfaceHandle :: Surface -> Vk.SurfaceKHR
surfaceHandle = surfaceRawHandle

surfacePresentQueue :: Surface -> Queue
surfacePresentQueue = surfaceQueue

surfaceFramebufferExtent :: Surface -> Maybe (IO (Int, Int))
surfaceFramebufferExtent = surfaceExtentProvider

surfaceBelongsTo :: Unique -> Surface -> Bool
surfaceBelongsTo identity surface = identity == surfaceIdentity surface
