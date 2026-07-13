{- | Low-level construction of window-system surface factories.

Platform integrations provide instance extensions, an acquisition callback,
and a payload release callback:

@
module Main (main) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Vulkan.Core10.Handles qualified as Vk
import Vulkan.Extensions.Handles qualified as Vk
import Vpipe.Surface (SurfaceFactory)
import Vpipe.Surface.Driver (mkSurfaceFactory)

newSurfaceFactory :: [ByteString] -> (Vk.Instance -> IO ((), NonEmpty Vk.SurfaceKHR)) -> SurfaceFactory ()
newSurfaceFactory extensions acquire =
  mkSurfaceFactory extensions acquire (const (pure ()))

main :: IO ()
main = pure ()
@
-}
module Vpipe.Surface.Driver (mkSurfaceFactory, mkSurfaceFactoryWithExtents) where

import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Vulkan.Core10.Handles qualified as Vk
import Vulkan.Extensions.Handles qualified as Vk

import Vpipe.Surface (SurfaceFactory)
import Vpipe.Surface.Internal (SurfaceFactory (..), SurfaceSource (..))

{- | Construct a factory from platform integration callbacks. If acquisition
creates some native/Vulkan surfaces and then fails, the platform callback is
responsible for releasing that partial acquisition before throwing.
-}
mkSurfaceFactory :: [ByteString] -> (Vk.Instance -> IO (payload, NonEmpty Vk.SurfaceKHR)) -> (payload -> IO ()) -> SurfaceFactory payload
mkSurfaceFactory extensions acquire =
  SurfaceFactory extensions (fmap (second (fmap (`SurfaceSource` Nothing))) . acquire)

{- | Extended variant for integrations that can report the drawable's current
framebuffer extent. The providers retain the same order as their surfaces.
-}
mkSurfaceFactoryWithExtents :: [ByteString] -> (Vk.Instance -> IO (payload, NonEmpty (Vk.SurfaceKHR, IO (Int, Int)))) -> (payload -> IO ()) -> SurfaceFactory payload
mkSurfaceFactoryWithExtents extensions acquire =
  SurfaceFactory extensions (fmap (second (fmap (\(surface, extent) -> SurfaceSource surface (Just extent)))) . acquire)
