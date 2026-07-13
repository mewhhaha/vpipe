{-# LANGUAGE DataKinds #-}

module CubeCoordinates where

import Linear (V2, V4)
import Vpipe.Expr
import Vpipe.Format (Format (R8G8B8A8Unorm))
import Vpipe.Image.Types (Dim (Cube))

cubeTexture :: SampledImage 'Cube 'R8G8B8A8Unorm 'Fragment
cubeTexture = sampledImage (imageResource "cube.image") (sampler "cube.sampler")

-- EXPECT: Couldn't match type
-- EXPECT: V3 Float
invalidCoordinates :: F (V4 Float)
invalidCoordinates = sample cubeTexture (constant (0 :: V2 Float))
