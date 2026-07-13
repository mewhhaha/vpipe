{-# LANGUAGE DataKinds #-}

-- EXPECT: Flat interpolation is unavailable for Linear.V4.V4
module FlatMatrix where

import Linear (M44, V3)
import Vpipe.Expr
import Vpipe.Pipeline

data Environment

invalidFlatMatrixPipeline :: PipelineM Environment ()
invalidFlatMatrixPipeline = do
  positions <- vertexInput (vertexSource "position" (const undefined) :: VertexSource Environment 'Triangles (V3 Float))
  _ <- rasterize defaultRaster (fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Flat (constant undefined :: V (M44 Float)))) positions)
  pure ()
