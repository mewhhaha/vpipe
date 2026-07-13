{-# LANGUAGE DataKinds #-}

-- EXPECT: Image operation requires usage ColorTarget
module ColorBindingWithoutTarget where

import Vpipe.Format (Format (R8G8B8A8Unorm))
import Vpipe.Image
import Vpipe.Image.Types
import Vpipe.Pipeline (ColorImage, colorImageBinding)

invalidColorBinding :: Image 'D2 'R8G8B8A8Unorm '[ 'CopySrc] -> IO (ColorImage 'R8G8B8A8Unorm)
invalidColorBinding = colorImageBinding
