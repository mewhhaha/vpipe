{-# LANGUAGE DataKinds #-}

-- EXPECT: Image operation requires usage Sampled
module TextureBindingWithoutSampled where

import Vpipe.Format (Format (R8G8B8A8Unorm))
import Vpipe.Image
import Vpipe.Image.Types
import Vpipe.Pipeline (TypedTextureBinding, typedTextureBinding)
import Vpipe.Sampler (Sampler)

invalidTextureBinding :: Image 'D2 'R8G8B8A8Unorm '[ 'CopyDst] -> Sampler -> IO (TypedTextureBinding 'D2 'R8G8B8A8Unorm)
invalidTextureBinding = typedTextureBinding
