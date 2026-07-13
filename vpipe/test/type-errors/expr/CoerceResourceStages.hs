{-# LANGUAGE DataKinds #-}

-- EXPECT: invalidImageCoercion
-- EXPECT: invalidSamplerCoercion
-- EXPECT: invalidCombinedCoercion
module CoerceResourceStages where

import Data.Coerce (coerce)
import Vpipe.Expr

invalidImageCoercion :: Image2D 'Vertex
invalidImageCoercion = coerce (image2D "image" :: Image2D 'Fragment)

invalidSamplerCoercion :: Sampler 'Vertex
invalidSamplerCoercion = coerce (sampler "sampler" :: Sampler 'Fragment)

invalidCombinedCoercion :: Sampler2D 'Vertex
invalidCombinedCoercion = coerce (sampler2D "combined" :: Sampler2D 'Fragment)
