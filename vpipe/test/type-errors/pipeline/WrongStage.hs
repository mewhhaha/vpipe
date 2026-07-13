{-# LANGUAGE DataKinds #-}

-- EXPECT: rasterize requires a vertex-stage clip position.
-- EXPECT: Build the position from vertexInput and give it type V (V4 Float).
module WrongStage where

import Linear (V3, V4 (..))
import Vpipe.Expr
import Vpipe.Pipeline

data Environment

invalidStagePipeline :: PipelineM Environment ()
invalidStagePipeline = do
  positions <- vertexInput (vertexSource "position" (const undefined) :: VertexSource Environment 'Triangles (V3 Float))
  let vertices = fmap (\position -> (constant (V4 0 0 0 1) :: F (V4 Float), Smooth position)) positions
  _ <- rasterize defaultRaster vertices
  pure ()
