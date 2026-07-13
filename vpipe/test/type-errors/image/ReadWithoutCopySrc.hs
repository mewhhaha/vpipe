{-# LANGUAGE DataKinds #-}

-- EXPECT: Image operation requires usage CopySrc
module ReadWithoutCopySrc where

import Data.Word (Word8)
import Vpipe.Format (Format (R8Unorm))
import Vpipe.Image
import Vpipe.Image.Types

invalidRead :: Image 'D2 'R8Unorm '[ 'CopyDst] -> IO [Word8]
invalidRead image = readImage image (ImageSubresource 0 0)
