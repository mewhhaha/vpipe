{-# LANGUAGE DataKinds #-}

module CoerceDispatch where

import Data.Coerce (coerce)
import Vpipe.Compute (Dispatch)

-- EXPECT: Couldn't match type
badDispatch :: Dispatch 64 1 1 -> Dispatch 32 1 1
badDispatch = coerce
