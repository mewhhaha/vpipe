{-# LANGUAGE DataKinds #-}

-- EXPECT: Image operation requires usage DepthTarget
module DepthBindingWithoutTarget where

import Vpipe.Format (Format (D32Sfloat))
import Vpipe.Image
import Vpipe.Image.Types
import Vpipe.Pipeline (DepthImage, depthImageBinding)

invalidDepthBinding :: Image 'D2 'D32Sfloat '[ 'CopySrc] -> IO (DepthImage 'D32Sfloat)
invalidDepthBinding = depthImageBinding
