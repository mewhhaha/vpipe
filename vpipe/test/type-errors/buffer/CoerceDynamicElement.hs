{-# LANGUAGE DataKinds #-}

module CoerceDynamicElement where

import Data.Coerce (coerce)
import Data.Word (Word32)
import Vpipe.Buffer (Usage (CopySrc))
import Vpipe.Buffer.Dynamic (DynamicBuffer)

-- EXPECT: Couldn't match type
invalidElement :: DynamicBuffer '[ 'CopySrc] Float -> DynamicBuffer '[ 'CopySrc] Word32
invalidElement = coerce
