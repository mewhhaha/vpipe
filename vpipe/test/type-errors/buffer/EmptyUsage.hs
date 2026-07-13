{-# LANGUAGE DataKinds #-}

module EmptyUsage where

import Data.Word (Word32)
import Vpipe.Buffer (Buffer, newBuffer)
import Vpipe.Context (Context)

-- EXPECT: Buffer usage lists must not be empty.
emptyUsage :: Context -> IO (Buffer '[] Word32)
emptyUsage context = newBuffer context 1
