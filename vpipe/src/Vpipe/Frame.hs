{-# LANGUAGE DataKinds #-}

{- | Ordered graphics and compute passes presented through a managed swapchain.

A frame callback receives the current swapchain image as a typed color target.
Passes are recorded in ordinary monadic program order and submitted together:

@
module Main (main) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (bracket)
import Vpipe.Context (defaultVpipeConfig)
import Vpipe.Frame (frame, frameColorTarget, renderTo)
import Vpipe.GLFW (defaultWindowConfig, windowSurface, withWindow)
import Vpipe.Swapchain (defaultSwapchainConfig, destroySwapchain, newSwapchain)

main :: IO ()
main = runInBoundThread $
  withWindow defaultVpipeConfig defaultWindowConfig $ \context window ->
    bracket
      (newSwapchain context (windowSurface window) defaultSwapchainConfig)
      destroySwapchain
      (\swapchain -> print =<< frame swapchain (\current -> renderTo (frameColorTarget current) (pure ())))
@

Successful frames use one slot command buffer, one queue submission, and one
presentation operation. Descriptor storage, resource barriers, and
frames-in-flight retirement are internal to the frame protocol.

Frame-owned dynamic slices are created with
'Vpipe.Buffer.Dynamic.newFrameDynamicBuffer'. 'withDynamicUniform' and
'withDynamicStorage' select the current frame slot, write the supplied
element range, and pass their continuation a binding for that exact subrange.
A frame dynamic buffer is tied to its creating swapchain and can be bound at
most once per frame, whether as uniform or storage data.

'copyPass' records an ordered buffer transfer. Its offsets and count are in
elements, not bytes; its source and destination require the corresponding
@CopySrc@ and @CopyDst@ usages. It validates context, range, stride, and
overlap before recording the transfer, and participates in the same
frame-owned synchronization as rendering and compute.

If initial image acquisition reports @VK_ERROR_OUT_OF_DATE_KHR@, 'frame'
recreates the swapchain and reacquires once. A ready replacement runs the
callback normally. On the acquire path, @PresentDeferred RecreatePending@
means that replacement was also rejected; the callback was not run, so the
application should yield or poll before retrying.
-}
module Vpipe.Frame (
  Frame,
  Pass,
  frame,
  frameColorTarget,
  renderTo,
  render,
  computePass,
  computePassFor,
  withDynamicUniform,
  withDynamicStorage,
  copyPass,
) where

import Vpipe.Frame.Internal
