{-# LANGUAGE DataKinds #-}

module CoerceCompiledDispatch where

import Data.Coerce (coerce)
import Vpipe.Compute (CompiledCompute)

-- EXPECT: Couldn't match type
badCompiled :: CompiledCompute env 64 1 1 -> CompiledCompute env 32 1 1
badCompiled = coerce
