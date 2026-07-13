{-# LANGUAGE DataKinds #-}

module VertexUniform where

import Data.Word (Word32)
import Vpipe.Buffer (Buffer, Usage (Uniform, Vertex), newBuffer)
import Vpipe.Context (Context)

-- EXPECT: Vertex buffers cannot be combined with Uniform usage.
vertexUniform :: Context -> IO (Buffer '[Vertex, Uniform] Word32)
vertexUniform context = newBuffer context 1
