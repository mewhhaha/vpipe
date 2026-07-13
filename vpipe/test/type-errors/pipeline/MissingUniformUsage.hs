{-# LANGUAGE DataKinds #-}

module MissingUniformUsage where

import Vpipe.Buffer (Buffer, Usage (CopySrc))
import Vpipe.Pipeline (UniformBuffer, uniformBufferBinding)

-- EXPECT: Buffer operation requires usage Uniform
invalidUniformBinding :: Buffer '[ 'CopySrc] Float -> UniformBuffer Float
invalidUniformBinding = uniformBufferBinding
