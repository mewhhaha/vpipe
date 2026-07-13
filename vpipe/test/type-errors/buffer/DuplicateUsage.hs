{-# LANGUAGE DataKinds #-}

module DuplicateUsage where

import Data.Word (Word32)
import Vpipe.Buffer (Buffer, Usage (CopySrc), newBuffer)
import Vpipe.Context (Context)

-- EXPECT: Buffer usage list contains duplicate usage CopySrc
duplicateUsage :: Context -> IO (Buffer '[CopySrc, CopySrc] Word32)
duplicateUsage context = newBuffer context 1
