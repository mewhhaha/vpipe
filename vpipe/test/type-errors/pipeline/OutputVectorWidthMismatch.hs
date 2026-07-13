{-# LANGUAGE DataKinds #-}

-- EXPECT: drawColor output does not match target format R32G32Sfloat.
-- EXPECT: Expected color value: V2 Float
-- EXPECT: Actual color value: V3 Float
-- EXPECT: Fix: pass a fragment stream containing the expected scalar/vector value; use vec2, vec3, or vec4 for vector formats.
module OutputVectorWidthMismatch where

import Linear (V2, V3 (..), V4 (..))
import Vpipe.Expr
import Vpipe.Format (Format (R32G32Sfloat))
import Vpipe.Pipeline

data Environment

invalidOutputPipeline :: PipelineM Environment ()
invalidOutputPipeline = do
  positions <- vertexInput (vertexSource "position" (const undefined) :: VertexSource Environment 'Triangles (V3 Float))
  let vertices = fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (constant (V4 1 1 1 1) :: V (V4 Float)))) positions
  fragments <- rasterize defaultRaster vertices
  drawColor
    defaultBlend
    (colorTarget "color" (const undefined) :: ColorTarget Environment 'R32G32Sfloat)
    (fmap (const (constant (V3 1 1 1) :: F (V3 Float))) fragments)
