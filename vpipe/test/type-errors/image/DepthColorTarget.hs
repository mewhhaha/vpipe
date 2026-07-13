{-# LANGUAGE DataKinds #-}

-- EXPECT: A depth format cannot be used as a colour attachment.
module DepthColorTarget where

import Vpipe.Context (Context)
import Vpipe.Format (Format (D32Sfloat))
import Vpipe.Image
import Vpipe.Image.Types

invalidImage :: Context -> IO (Image 'D2 'D32Sfloat '[ 'ColorTarget])
invalidImage context = newImage context (imageExtent2D 1 1) 1 1
