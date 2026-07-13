{- | Descriptions for Vulkan image samplers.

The types in this module deliberately use closed enums rather than exposing
Vulkan constants. This keeps sampler descriptions stable and comparable, so a
context can safely share equivalent samplers.
-}
module Vpipe.Sampler.Types (
  Filter (..),
  MipmapMode (..),
  AddressMode (..),
  CompareOp (..),
  BorderColor (..),
  SamplerDescription (..),
  SamplerCacheEntry (..),
  defaultSamplerDescription,
) where

import Vulkan.Core10.Handles qualified as Handles

import Vpipe.Resource.Lifetime (LifetimeGate, ResourceGeneration)

data Filter
  = Nearest
  | Linear
  deriving stock (Eq, Ord, Show)

data MipmapMode
  = NearestMipmap
  | LinearMipmap
  deriving stock (Eq, Ord, Show)

data AddressMode
  = Repeat
  | MirroredRepeat
  | ClampToEdge
  | ClampToBorder
  deriving stock (Eq, Ord, Show)

data CompareOp
  = Never
  | Less
  | Equal
  | LessOrEqual
  | Greater
  | NotEqual
  | GreaterOrEqual
  | Always
  deriving stock (Eq, Ord, Show)

data BorderColor
  = FloatTransparentBlack
  | IntTransparentBlack
  | FloatOpaqueBlack
  | IntOpaqueBlack
  | FloatOpaqueWhite
  | IntOpaqueWhite
  deriving stock (Eq, Ord, Show)

data SamplerDescription = SamplerDescription
  { samplerMagFilter :: Filter
  , samplerMinFilter :: Filter
  , samplerMipmapMode :: MipmapMode
  , samplerAddressModeU :: AddressMode
  , samplerAddressModeV :: AddressMode
  , samplerAddressModeW :: AddressMode
  , samplerMipLodBias :: Float
  , samplerLodMinimum :: Float
  , samplerLodMaximum :: Float
  , samplerAnisotropy :: Maybe Float
  , samplerCompareOp :: Maybe CompareOp
  , samplerBorderColor :: BorderColor
  , samplerUnnormalizedCoordinates :: Bool
  }
  deriving stock (Eq, Ord, Show)

data SamplerCacheEntry = SamplerCacheEntry
  { cachedSamplerHandle :: Handles.Sampler
  , cachedSamplerGeneration :: ResourceGeneration
  , cachedSamplerLifetimeGate :: LifetimeGate
  }

defaultSamplerDescription :: SamplerDescription
defaultSamplerDescription =
  SamplerDescription
    { samplerMagFilter = Linear
    , samplerMinFilter = Linear
    , samplerMipmapMode = LinearMipmap
    , samplerAddressModeU = Repeat
    , samplerAddressModeV = Repeat
    , samplerAddressModeW = Repeat
    , samplerMipLodBias = 0
    , samplerLodMinimum = 0
    , samplerLodMaximum = 1000
    , samplerAnisotropy = Nothing
    , samplerCompareOp = Nothing
    , samplerBorderColor = FloatTransparentBlack
    , samplerUnnormalizedCoordinates = False
    }
