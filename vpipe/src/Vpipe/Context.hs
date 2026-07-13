{- | Managed Vulkan context creation and queue access.

Managed resource constructors and destruction are safe to call concurrently
from multiple Haskell threads. All public operations acquire a lifecycle
lease, so retained values reject work once their owning context begins
closing. Queue submission is serialized per Vulkan queue.

Frame recording is scoped to the callback passed to @Vpipe.Frame.frame@.
Treat its @Frame@ value as thread-confined and do not retain it: the value
expires when the callback returns. Independent swapchains may record frames
on different threads, while GLFW event processing remains subject to the
main-thread contract documented by @Vpipe.GLFW@.

@
module Main (main) where

import Vpipe.Context (contextDeviceName, defaultVpipeConfig, withVpipe)

main :: IO ()
main = withVpipe defaultVpipeConfig (putStrLn . contextDeviceName)
@
-}
module Vpipe.Context (
  Context,
  VpipeConfig (..),
  StructuredLog (..),
  defaultVpipeConfig,
  withVpipe,
  graphicsQueue,
  computeQueue,
  transferQueue,
  contextDeviceName,
  contextDeviceIsCpu,
  contextUniformBufferOffsetAlignment,
  contextStorageBufferOffsetAlignment,
  contextNonCoherentAtomSize,
  contextMaxSamplerLodBias,
  drainValidationMessages,
  DebugSink,
  DebugMessage (..),
  newDebugSink,
  popDebugMessage,
  debugSinkDropped,
  freeDebugSink,
) where

import Vpipe.Context.Internal
