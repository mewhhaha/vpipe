{-# LANGUAGE DataKinds #-}

-- EXPECT: Couldn't match type ‘Float’ with ‘Bool’
module CoerceValue where

import Data.Coerce (coerce)
import Vpipe.Expr

invalidValueCoercion :: V Bool
invalidValueCoercion = coerce (constant 1 :: V Float)
