{-# LANGUAGE DataKinds #-}

-- EXPECT: zipStreams cannot combine primitive streams with different topologies.
-- EXPECT: Expected topology: Triangles
-- EXPECT: Actual topology: Lines
-- EXPECT: Fix: give both vertex sources the same PrimitiveTopology (Points, Lines, or Triangles).
module WrongTopology where

import Linear (V3)
import Vpipe.Pipeline

data Environment

invalidTopologyPipeline :: PipelineM Environment ()
invalidTopologyPipeline = do
  triangles <- vertexInput (vertexSource "triangles" (const undefined) :: VertexSource Environment 'Triangles (V3 Float))
  lines <- vertexInput (vertexSource "lines" (const undefined) :: VertexSource Environment 'Lines (V3 Float))
  let _combined = zipStreams triangles lines
  pure ()
