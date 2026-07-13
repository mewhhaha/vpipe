{-# LANGUAGE DataKinds #-}

module UniformStorage where

import Data.Word (Word32)
import Vpipe.Buffer (Buffer, Usage (Storage, Uniform), newBuffer)
import Vpipe.Context (Context)

-- EXPECT: Uniform and Storage usages cannot be combined
uniformStorage :: Context -> IO (Buffer '[Uniform, Storage] Word32)
uniformStorage context = newBuffer context 1
