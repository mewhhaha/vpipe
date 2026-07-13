{-# LANGUAGE DataKinds #-}

module CoerceAttachmentKinds where

import Data.Coerce (coerce)
import Vpipe.Format (Format (..))
import Vpipe.Pipeline (ColorImage, DepthImage)

-- EXPECT: Couldn't match representation
badAttachment :: DepthImage 'D32Sfloat -> ColorImage 'R8G8B8A8Unorm
badAttachment = coerce
