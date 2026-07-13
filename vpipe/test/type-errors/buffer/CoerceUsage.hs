{-# LANGUAGE DataKinds #-}

module CoerceUsage where

import Data.Coerce (coerce)
import Vpipe.Buffer (Buffer, Usage (CopySrc, Uniform))

-- EXPECT: Couldn't match type
invalidUsage :: Buffer '[ 'CopySrc] Float -> Buffer '[ 'Uniform] Float
invalidUsage = coerce
