{-# LANGUAGE DataKinds #-}

module CoerceDynamicUsage where

import Data.Coerce (coerce)
import Vpipe.Buffer (Usage (CopySrc, Uniform))
import Vpipe.Buffer.Dynamic (DynamicBuffer)

-- EXPECT: Couldn't match type
invalidUsage :: DynamicBuffer '[ 'CopySrc] Float -> DynamicBuffer '[ 'Uniform] Float
invalidUsage = coerce
