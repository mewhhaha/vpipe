{-# LANGUAGE DataKinds #-}

-- EXPECT: Flat interpolation is unavailable for Bool
module FlatBoolThenVector where

import Linear (V2, V3)
import Vpipe.Expr
import Vpipe.Pipeline

data Environment

invalidFlatBoolThenVectorPipeline :: PipelineM Environment ()
invalidFlatBoolThenVectorPipeline = do
  positions <- vertexInput (vertexSource "position" (const undefined) :: VertexSource Environment 'Triangles (V3 Float))
  _ <- rasterize defaultRaster (fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), (Flat (constant True :: V Bool), Flat (constant undefined :: V (V2 Float))))) positions)
  pure ()
