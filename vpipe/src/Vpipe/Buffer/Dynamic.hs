{- | Persistently mapped buffer slices. A 'DynamicBuffer' leaves copy
selection explicit; a 'FrameDynamicBuffer' is tied to a swapchain frame
domain and lets the frame runtime select the active copy.

The copy index for a 'DynamicBuffer' is supplied by the caller, making
frame-rotation policy explicit:

@
module Main (main) where

import Vpipe.Buffer (Usage (Storage))
import Vpipe.Buffer.Dynamic
import Vpipe.Context (Context)

newRotatingStorageBuffer :: Context -> IO (DynamicBuffer '[Storage] Float)
newRotatingStorageBuffer context = newDynamicBuffer context 2 4

writeActiveCopy :: DynamicBuffer '[Storage] Float -> Int -> IO ()
writeActiveCopy buffer copyIndex =
  writeDynamicBuffer buffer copyIndex 0 [1, 2, 3, 4]

main :: IO ()
main = pure ()
@

'newFrameDynamicBuffer' allocates one copy for each configured frame slot.
Within 'Vpipe.Frame.frame', 'Vpipe.Frame.withDynamicUniform' and
'Vpipe.Frame.withDynamicStorage' select the current slot, write the supplied
element range, and bind precisely that range rather than the whole dynamic
allocation. A frame dynamic buffer belongs to its creating swapchain's frame
domain and may be bound only once in a frame, across both helpers.
-}
module Vpipe.Buffer.Dynamic (
  DynamicBuffer,
  FrameDynamicBuffer,
  newDynamicBuffer,
  newFrameDynamicBuffer,
  destroyDynamicBuffer,
  destroyFrameDynamicBuffer,
  dynamicCopyCount,
  dynamicElementsPerCopy,
  dynamicStride,
  dynamicSliceBytes,
  dynamicSliceOffset,
  frameDynamicElements,
  frameDynamicStride,
  frameDynamicSliceBytes,
  writeDynamicBuffer,
  readDynamicBuffer,
  flushDynamicBuffer,
  invalidateDynamicBuffer,
) where

import Vpipe.Buffer.Dynamic.Internal
