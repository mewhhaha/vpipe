{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- EXPECT: MatrixBuffer requires Float components
-- EXPECT: received Bool
-- EXPECT: Fix: use MatrixBuffer with Float components.
module MatrixBool where

import Data.Proxy (Proxy (..))
import Vpipe.Buffer.Format (BufferFormat (bufferSize), MatrixBuffer)

rejectedBooleanMatrix :: Int
rejectedBooleanMatrix = bufferSize (Proxy @(MatrixBuffer 4 4 Bool))
