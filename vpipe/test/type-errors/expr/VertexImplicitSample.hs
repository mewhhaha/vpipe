{-# LANGUAGE DataKinds #-}

-- EXPECT: Couldn't match type ‘Vertex’ with ‘Fragment’
module VertexImplicitSample where

import Linear (V2 (..), V4)
import Vpipe.Expr

invalidVertexSample :: V (V4 Float)
invalidVertexSample = sample (sampler2D "texture") (constant (V2 0 0) :: V (V2 Float))
