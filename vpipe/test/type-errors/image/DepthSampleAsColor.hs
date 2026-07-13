{-# LANGUAGE DataKinds #-}

-- EXPECT: Couldn't match type
-- EXPECT: V4 Float
module DepthSampleAsColor where

import Linear (V2, V4)
import Vpipe.Expr
import Vpipe.Format (Format (D32Sfloat))
import Vpipe.Image.Types (Dim (D2))

depthImage :: SampledImage 'D2 'D32Sfloat 'Fragment
depthImage = sampledImage (imageResource "depth.image") (sampler "depth.sampler")

invalidDepthColor :: Expr 'Fragment (V4 Float)
invalidDepthColor = sample depthImage (constant (0 :: V2 Float))
