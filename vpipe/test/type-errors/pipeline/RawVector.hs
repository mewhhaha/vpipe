{-# LANGUAGE DataKinds #-}

-- EXPECT: Raw varying V2
module RawVector where

import Linear (V2, V3)
import Vpipe.Expr
import Vpipe.Pipeline

data Environment

invalidRawVectorPipeline :: PipelineM Environment ()
invalidRawVectorPipeline = do
  positions <- vertexInput (vertexSource "position" (const undefined) :: VertexSource Environment 'Triangles (V3 Float))
  _ <- rasterize defaultRaster (fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), constant undefined :: V (V2 Float))) positions)
  pure ()
