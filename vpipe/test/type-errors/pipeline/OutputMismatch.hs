{-# LANGUAGE DataKinds #-}

-- EXPECT: drawColor output does not match target format R8G8B8A8Srgb.
-- EXPECT: Expected color value: V4 Float
-- EXPECT: Actual color value: Float
-- EXPECT: Fix: pass a fragment stream containing the expected scalar/vector value; use vec2, vec3, or vec4 for vector formats.
module OutputMismatch where

import Linear (V3, V4 (..))
import Vpipe.Expr
import Vpipe.Format (Format (R8G8B8A8Srgb))
import Vpipe.Pipeline

data Environment

invalidOutputPipeline :: PipelineM Environment ()
invalidOutputPipeline = do
  positions <- vertexInput (vertexSource "position" (const undefined) :: VertexSource Environment 'Triangles (V3 Float))
  let vertices = fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (constant (V4 1 1 1 1) :: V (V4 Float)))) positions
  fragments <- rasterize defaultRaster vertices
  drawColor
    defaultBlend
    (colorTarget "color" (const undefined) :: ColorTarget Environment 'R8G8B8A8Srgb)
    (fmap (const (constant 1 :: F Float)) fragments)
