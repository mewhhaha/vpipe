{-# LANGUAGE DataKinds #-}

module CoerceElement where

import Data.Coerce (coerce)
import Data.Word (Word32)
import Vpipe.Buffer (Buffer, Usage (CopySrc))

-- EXPECT: Couldn't match type
invalidElement :: Buffer '[ 'CopySrc] Float -> Buffer '[ 'CopySrc] Word32
invalidElement = coerce
