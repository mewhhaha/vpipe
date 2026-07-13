{-# LANGUAGE DataKinds #-}

module CopyWithoutDestinationUsage where

import Data.Word (Word32)
import Vpipe.Buffer (Buffer, Usage (CopySrc))
import Vpipe.Frame (Pass, copyPass)

-- EXPECT: Buffer operation requires usage CopyDst
invalidCopy :: Buffer '[ 'CopySrc] Word32 -> Buffer '[ 'CopySrc] Word32 -> Pass ()
invalidCopy source destination = copyPass source 0 destination 0 1
