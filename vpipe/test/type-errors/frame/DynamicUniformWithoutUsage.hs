{-# LANGUAGE DataKinds #-}

module DynamicUniformWithoutUsage where

import Vpipe.Buffer (Usage (CopySrc))
import Vpipe.Buffer.Dynamic (FrameDynamicBuffer)
import Vpipe.Frame (Pass, withDynamicUniform)

-- EXPECT: Buffer operation requires usage Uniform
invalidUniform :: FrameDynamicBuffer '[ 'CopySrc] Float -> Pass ()
invalidUniform buffer = withDynamicUniform buffer 0 [1] (const (pure ()))
