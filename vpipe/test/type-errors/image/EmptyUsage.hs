{-# LANGUAGE DataKinds #-}

-- EXPECT: Image usage lists must not be empty.
module EmptyUsage where

import Vpipe.Context (Context)
import Vpipe.Format (Format (R8Unorm))
import Vpipe.Image
import Vpipe.Image.Types

invalidImage :: Context -> IO (Image 'D2 'R8Unorm '[])
invalidImage context = newImage context (imageExtent2D 1 1) 1 1
