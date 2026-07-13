{-# LANGUAGE DataKinds #-}

-- EXPECT: drawColor cannot write to target format D32Sfloat because it is a depth format.
-- EXPECT: Fix: use drawDepth with a DepthTarget instead.
module DrawColorDepthTarget where

import Linear (V3)
import Vpipe.Expr
import Vpipe.Format (Format (D32Sfloat))
import Vpipe.Pipeline

data Environment

invalidDepthColorPipeline :: PipelineM Environment ()
invalidDepthColorPipeline = do
  positions <- vertexInput (vertexSource "position" (const undefined) :: VertexSource Environment 'Triangles (V3 Float))
  fragments <-
    rasterize
      defaultRaster
      (fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (constant (0 :: Float) :: V Float))) positions)
  drawColor
    defaultBlend
    (colorTarget "depth" (const undefined) :: ColorTarget Environment 'D32Sfloat)
    (fmap (const (constant 1 :: F Float)) fragments)
