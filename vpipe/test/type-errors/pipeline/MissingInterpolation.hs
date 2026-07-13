{-# LANGUAGE DataKinds #-}

-- EXPECT: Raw varying Float has no interpolation qualifier
module MissingInterpolation where

import Linear (V3)
import Vpipe.Expr
import Vpipe.Pipeline

data Environment

invalidUnqualifiedFloatPipeline :: PipelineM Environment ()
invalidUnqualifiedFloatPipeline = do
  positions <- vertexInput (vertexSource "position" (const undefined) :: VertexSource Environment 'Triangles (V3 Float))
  let vertices = fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), x position)) positions
  _ <- rasterize defaultRaster vertices
  pure ()
