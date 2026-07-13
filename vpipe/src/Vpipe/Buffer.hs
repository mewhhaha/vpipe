{- | Typed Vulkan buffers. Element offsets and counts are expressed in host
elements, never bytes; raw Vulkan handles remain internal.

Allocate a buffer from a managed context and use host-element offsets when
writing it:

@
module Main (main) where

import Vpipe.Buffer
import Vpipe.Context (Context)

newVertexBuffer :: Context -> IO (Buffer '[Vertex] Float)
newVertexBuffer context = newBuffer context 4

writeVertices :: Buffer '[Vertex] Float -> IO ()
writeVertices buffer = writeBuffer buffer 0 [0, 1, 2, 3]

main :: IO ()
main = pure ()
@
-}
module Vpipe.Buffer (
  Usage (..),
  HasUsage,
  ValidUsages,
  BufferLayout,
  KnownUsages,
  KnownLayout,
  reflectedUsageFlags,
  reflectedLayout,
  Buffer,
  RestartIndex,
  normalIndex,
  primitiveRestartIndex,
  restartIndexWord32,
  newBuffer,
  destroyBuffer,
  writeBuffer,
  writeIndexBuffer,
  readBuffer,
  bufferLength,
  bufferLayout,
  bufferStride,
) where

import Vpipe.Buffer.Internal
