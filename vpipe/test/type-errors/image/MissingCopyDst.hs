{-# LANGUAGE DataKinds #-}

-- EXPECT: Image operation requires usage CopyDst
module MissingCopyDst where

import Data.Word (Word8)
import Vpipe.Format (Format (R8Unorm))
import Vpipe.Image
import Vpipe.Image.Types

invalidWrite :: Image 'D2 'R8Unorm '[ 'CopySrc] -> IO ()
invalidWrite image = writeImage image (ImageSubresource 0 0) ([0] :: [Word8])
