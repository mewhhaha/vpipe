{-# LANGUAGE DataKinds #-}

module IndexNotWord32 where

import Vpipe.Buffer (Buffer, Usage (Index), newBuffer)
import Vpipe.Context (Context)

-- EXPECT: Index buffers require Word32 elements
indexNotWord32 :: Context -> IO (Buffer '[Index] Float)
indexNotWord32 context = newBuffer context 1
