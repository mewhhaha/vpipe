{-# LANGUAGE DataKinds #-}

-- EXPECT: Shader derivatives require fragment-stage values.
-- EXPECT: Move dFdx, dFdy, or fwidth after rasterize
module VertexDerivative where

import Vpipe.Expr

invalidVertexDerivative :: V Float
invalidVertexDerivative = dFdx (constant 1 :: V Float)
