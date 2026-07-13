{- | Presentation surfaces whose native payload remains alive for the entire
managed Context lifetime.

Applications normally obtain surfaces from @vpipe-glfw@:

@
module Main (main) where

import Control.Concurrent (runInBoundThread)
import Vpipe.Context (defaultVpipeConfig)
import Vpipe.GLFW
import Vpipe.Surface (Surface)

main :: IO ()
main = runInBoundThread $
  withWindow defaultVpipeConfig defaultWindowConfig $ \_ window -> do
    let surface :: Surface
        surface = windowSurface window
    surface `seq` print =<< getFramebufferSize window
@

Custom window-system integrations construct a @SurfaceFactory@ through
"Vpipe.Surface.Driver" and pass it to 'withVpipeSurfaces'.
-}
module Vpipe.Surface (
  Surface,
  SurfaceFactory,
  withVpipeSurfaces,
) where

import Data.List.NonEmpty (NonEmpty)

import Vpipe.Context.Internal (Context, VpipeConfig, withVpipeSurfacesInternal)
import Vpipe.Surface.Internal (Surface, SurfaceFactory)

{- | Acquire a context capable of presenting to every supplied surface. The
payload release callback runs on this caller's thread after Vulkan resources
have been cleaned up and before the instance is destroyed.
-}
withVpipeSurfaces :: VpipeConfig -> SurfaceFactory payload -> (Context -> NonEmpty Surface -> payload -> IO a) -> IO a
withVpipeSurfaces = withVpipeSurfacesInternal
