{-# LANGUAGE DataKinds #-}

module ReadWithoutCopySrc where

import Data.Word (Word32)
import Vpipe.Buffer (Buffer, Usage (CopyDst), readBuffer)

-- EXPECT: Buffer operation requires usage CopySrc
readWithoutCopySource :: Buffer '[CopyDst] Word32 -> IO [Word32]
readWithoutCopySource buffer = readBuffer buffer 0 1
