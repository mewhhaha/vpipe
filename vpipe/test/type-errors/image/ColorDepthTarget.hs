{-# LANGUAGE DataKinds #-}

-- EXPECT: A colour format cannot be used as a depth attachment.
module ColorDepthTarget where

import Vpipe.Context (Context)
import Vpipe.Format (Format (R8G8B8A8Unorm))
import Vpipe.Image
import Vpipe.Image.Types

invalidImage :: Context -> IO (Image 'D2 'R8G8B8A8Unorm '[ 'DepthTarget])
invalidImage context = newImage context (imageExtent2D 1 1) 1 1
