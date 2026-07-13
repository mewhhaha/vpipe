{-# LANGUAGE DataKinds #-}

-- EXPECT: Couldn't match type
module CoerceUsage where

import Data.Coerce (coerce)
import Vpipe.Format (Format (R8Unorm))
import Vpipe.Image (Image)
import Vpipe.Image.Types (Dim (D2), ImageUsage (CopyDst, CopySrc))

invalidCoerce :: Image 'D2 'R8Unorm '[ 'CopySrc] -> Image 'D2 'R8Unorm '[ 'CopyDst]
invalidCoerce = coerce
