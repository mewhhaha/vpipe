{-# LANGUAGE DataKinds #-}

-- EXPECT: Flat interpolation is unavailable for Bool
module FlatBool where

import Linear (V3)
import Vpipe.Expr
import Vpipe.Pipeline

data Environment

invalidFlatBoolPipeline :: PipelineM Environment ()
invalidFlatBoolPipeline = do
  positions <- vertexInput (vertexSource "position" (const undefined) :: VertexSource Environment 'Triangles (V3 Float))
  _ <- rasterize defaultRaster (fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Flat (constant True :: V Bool))) positions)
  pure ()
