{-# LANGUAGE DataKinds #-}

module CopyWithoutSourceUsage where

import Data.Word (Word32)
import Vpipe.Buffer (Buffer, Usage (CopyDst))
import Vpipe.Frame (Pass, copyPass)

-- EXPECT: Buffer operation requires usage CopySrc
invalidCopy :: Buffer '[ 'CopyDst] Word32 -> Buffer '[ 'CopyDst] Word32 -> Pass ()
invalidCopy source destination = copyPass source 0 destination 0 1
