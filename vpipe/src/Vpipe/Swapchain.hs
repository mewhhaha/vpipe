{- | Managed Vulkan presentation swapchains.

With @vpipe-glfw@, construct a swapchain from the window's presentation
surface while both the context and native window are alive:

@
module Main (main) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket)
import Vpipe.Context (defaultVpipeConfig)
import Vpipe.GLFW (defaultWindowConfig, windowSurface, withWindow)
import Vpipe.Swapchain

main :: IO ()
main = runInBoundThread $
  withWindow defaultVpipeConfig defaultWindowConfig $ \context window -> do
    bracket
      (newSwapchain context (windowSurface window) defaultSwapchainConfig)
      destroySwapchain
      (\swapchain -> print =<< swapchainExtent swapchain)
@

The owning context also retains an idempotent finalizer, so explicit
'destroySwapchain' is optional when lexical cleanup is sufficient.

The configured acquire timeout is capped at five seconds. This keeps a lost
or permanently hidden surface from turning one frame attempt into an
effectively infinite host wait.

Vulkan makes swapchain replacement irreversible once @vkCreateSwapchainKHR@
is called with a non-null @oldSwapchain@: the old swapchain is retired even if
creation fails. vpipe completes surface queries and validation first, then
moves the old generation to retired ownership immediately before that native
call. A failure beyond this boundary leaves no active generation; the next
frame retries with @VK_NULL_HANDLE@ while retired resources drain after their
outstanding frame timeline values complete.
-}
module Vpipe.Swapchain (
  Swapchain,
  SwapchainConfig (..),
  PresentMode (..),
  PresentResult (..),
  DeferredReason (..),
  defaultSwapchainConfig,
  newSwapchain,
  destroySwapchain,
  swapchainExtent,
) where

import Vpipe.Swapchain.Internal
