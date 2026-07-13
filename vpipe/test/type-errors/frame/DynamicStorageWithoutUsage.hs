{-# LANGUAGE DataKinds #-}

module DynamicStorageWithoutUsage where

import Vpipe.Buffer (Usage (CopySrc))
import Vpipe.Buffer.Dynamic (FrameDynamicBuffer)
import Vpipe.Frame (Pass, withDynamicStorage)

-- EXPECT: Buffer operation requires usage Storage
invalidStorage :: FrameDynamicBuffer '[ 'CopySrc] Float -> Pass ()
invalidStorage buffer = withDynamicStorage buffer 0 [1] (const (pure ()))
