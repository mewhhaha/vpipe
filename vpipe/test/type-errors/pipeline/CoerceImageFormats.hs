{-# LANGUAGE DataKinds #-}

module CoerceImageFormats where

import Data.Coerce (coerce)
import Vpipe.Format (Format (..))
import Vpipe.Image.Types (Dim (D2))
import Vpipe.Pipeline (TypedTextureBinding)

-- EXPECT: Couldn't match type
badTexture :: TypedTextureBinding 'D2 'R8G8B8A8Unorm -> TypedTextureBinding 'D2 'R32Sfloat
badTexture = coerce
