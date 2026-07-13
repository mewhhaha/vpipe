{-# LANGUAGE DataKinds #-}

-- EXPECT: Integral varyings must use Flat interpolation
module SmoothInt where

import Data.Int (Int32)
import Linear (V3)
import Vpipe.Expr
import Vpipe.Pipeline

data Environment

invalidSmoothIntegerPipeline :: PipelineM Environment ()
invalidSmoothIntegerPipeline = do
  positions <- vertexInput (vertexSource "position" (const undefined) :: VertexSource Environment 'Triangles (V3 Float))
  let vertices = fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (constant 7 :: V Int32))) positions
  _ <- rasterize defaultRaster vertices
  pure ()
