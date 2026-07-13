{- | Opaque timeline-semaphore queues and low-level progress probes.

Most programs use higher-level graphics, compute, or frame operations. A queue
can be probed directly when integrating custom Vulkan work:

@
import Vpipe.Context
import Vpipe.Context.Queue

main :: IO ()
main = withVpipe defaultVpipeConfig $ \context -> do
  value <- submitEmpty (graphicsQueue context)
  waitTimeline (graphicsQueue context) value
@
-}
module Vpipe.Context.Queue (
  Queue,
  QueueRole (..),
  queueFamilyIndex,
  submitEmpty,
  waitTimeline,
  timelineCompletedValue,
) where

import Vpipe.Context.Queue.Internal
