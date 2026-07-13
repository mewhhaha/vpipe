{-# LANGUAGE DataKinds #-}

-- EXPECT: Couldn't match type ‘Fragment’ with ‘Vertex’
module CoerceDerivativeStage where

import Data.Coerce (coerce)
import Vpipe.Expr

fragmentDerivative :: F Float
fragmentDerivative = dFdx (constant 1)

invalidStageCoercion :: V Float
invalidStageCoercion = coerce fragmentDerivative
