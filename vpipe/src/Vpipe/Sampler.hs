{- | Context-owned, immutable Vulkan samplers.

Equivalent descriptions created in the same context share one Vulkan sampler.
The context destroys that sampler during shutdown.

Use the shared default description when no filtering or addressing overrides
are needed:

@
module Main (main) where

import Vpipe.Context (Context)
import Vpipe.Sampler

newDefaultSampler :: Context -> IO Sampler
newDefaultSampler context = newSampler context defaultSamplerDescription

main :: IO ()
main = pure ()
@
-}
module Vpipe.Sampler (
  Filter (..),
  MipmapMode (..),
  AddressMode (..),
  CompareOp (..),
  BorderColor (..),
  SamplerDescription (..),
  defaultSamplerDescription,
  Sampler,
  newSampler,
  samplerDescription,
) where

import Vpipe.Sampler.Internal (Sampler, newSampler, samplerDescription)
import Vpipe.Sampler.Types
