{-# LANGUAGE DataKinds #-}

module RawMatrixPushConstant where

import Linear (M44, V4 (..))
import Vpipe.Expr (Expr, Stage (Vertex))
import Vpipe.Pipeline (PipelineM, pushConstant)

rawMatrix :: M44 Float
rawMatrix = V4 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) (V4 13 14 15 16)

-- EXPECT: MatrixBuffer 4 4 Float
invalidRawMatrixAbi :: PipelineM () (Expr 'Vertex (M44 Float))
invalidRawMatrixAbi = pushConstant (const rawMatrix)
