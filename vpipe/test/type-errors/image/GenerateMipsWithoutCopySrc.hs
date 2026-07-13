{-# LANGUAGE DataKinds #-}

-- EXPECT: Image operation requires usage CopySrc
module GenerateMipsWithoutCopySrc where

import Vpipe.Format (Format (R8Unorm))
import Vpipe.Image
import Vpipe.Image.Types

invalidGenerateMips :: Image 'D2 'R8Unorm '[ 'CopyDst] -> IO ()
invalidGenerateMips image = generateMips image 0
