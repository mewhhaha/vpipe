{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- EXPECT: MatrixBuffer requires Float components
-- EXPECT: received Int32
-- EXPECT: Fix: use MatrixBuffer with Float components.
module MatrixInt32 where

import Data.Int (Int32)
import Data.Proxy (Proxy (..))
import Vpipe.Buffer.Format (BufferFormat (bufferSize), MatrixBuffer)

rejectedIntegerMatrix :: Int
rejectedIntegerMatrix = bufferSize (Proxy @(MatrixBuffer 2 2 Int32))
