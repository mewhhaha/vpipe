{-# LANGUAGE DataKinds #-}

module MissingIndexUsage where

import Data.Word (Word32)
import Vpipe.Buffer (Buffer, Usage (CopySrc))
import Vpipe.Pipeline (IndexBuffer, indexBufferBinding)

-- EXPECT: Buffer operation requires usage Index
invalidIndexBinding :: Buffer '[ 'CopySrc] Word32 -> IndexBuffer
invalidIndexBinding = indexBufferBinding
