{-# LANGUAGE DataKinds #-}

-- EXPECT: Image usage list contains duplicate usage Sampled.
module DuplicateUsage where

import Vpipe.Context (Context)
import Vpipe.Format (Format (R8Unorm))
import Vpipe.Image
import Vpipe.Image.Types

invalidImage :: Context -> IO (Image 'D2 'R8Unorm '[ 'Sampled, 'Sampled])
invalidImage context = newImage context (imageExtent2D 1 1) 1 1
