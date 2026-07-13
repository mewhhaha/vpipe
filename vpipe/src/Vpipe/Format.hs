{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Image and vertex formats shared by the typed resource and shader APIs.

Formats are promoted with @DataKinds@ and reflected when native Vulkan objects
are created:

@
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Vpipe.Format
import Vulkan.Core10.Enums.Format qualified as Vk

isRgba8 :: Bool
isRgba8 = formatVal @'R8G8B8A8Unorm == Vk.FORMAT_R8G8B8A8_UNORM

main :: IO ()
main = print isRgba8
@
-}
module Vpipe.Format (
  Format (..),
  VkFormat,
  KnownFormat (..),
  VertexFormat (..),
  TexelOf,
  FormatComponentType,
  FormatChannels,
  ColorRenderable,
  Blendable,
  DepthRenderable,
)
where

import Data.Kind (Constraint, Type)
import Data.Word (Word8)
import GHC.TypeLits (ErrorMessage (..), Nat, TypeError)
import Linear (V2, V3, V4)
import Vulkan.Core10.Enums.Format qualified as Vk

{- | The Vulkan image and vertex formats currently expressible by vpipe.
More formats can be added without changing the reflection interface.
-}
data Format
  = R8Unorm
  | R8G8B8A8Unorm
  | R8G8B8A8Srgb
  | B8G8R8A8Unorm
  | B8G8R8A8Srgb
  | R32Sfloat
  | R32G32Sfloat
  | R32G32B32Sfloat
  | R32G32B32A32Sfloat
  | D32Sfloat

-- | The runtime Vulkan representation of a promoted @Format@.
type VkFormat = Vk.Format

-- | Reflect a type-level format to the corresponding Vulkan value.
class KnownFormat (format :: Format) where
  formatVal :: VkFormat

{- | Connect host vertex attributes, their Vulkan format, and the value shape
visible to a vertex shader. Shader expressions reuse the linear shape;
task 03 supplies the expression wrapper around it.
-}
class VertexFormat a where
  type VertexInputFormat a :: Format
  type VertexShaderType a :: Type

instance VertexFormat Float where
  type VertexInputFormat Float = 'R32Sfloat
  type VertexShaderType Float = Float

instance VertexFormat (V2 Float) where
  type VertexInputFormat (V2 Float) = 'R32G32Sfloat
  type VertexShaderType (V2 Float) = V2 Float

instance VertexFormat (V3 Float) where
  type VertexInputFormat (V3 Float) = 'R32G32B32Sfloat
  type VertexShaderType (V3 Float) = V3 Float

instance VertexFormat (V4 Float) where
  type VertexInputFormat (V4 Float) = 'R32G32B32A32Sfloat
  type VertexShaderType (V4 Float) = V4 Float

instance KnownFormat 'R8Unorm where
  formatVal = Vk.FORMAT_R8_UNORM

instance KnownFormat 'R8G8B8A8Unorm where
  formatVal = Vk.FORMAT_R8G8B8A8_UNORM

instance KnownFormat 'R8G8B8A8Srgb where
  formatVal = Vk.FORMAT_R8G8B8A8_SRGB

instance KnownFormat 'B8G8R8A8Unorm where
  formatVal = Vk.FORMAT_B8G8R8A8_UNORM

instance KnownFormat 'B8G8R8A8Srgb where
  formatVal = Vk.FORMAT_B8G8R8A8_SRGB

instance KnownFormat 'R32Sfloat where
  formatVal = Vk.FORMAT_R32_SFLOAT

instance KnownFormat 'R32G32Sfloat where
  formatVal = Vk.FORMAT_R32G32_SFLOAT

instance KnownFormat 'R32G32B32Sfloat where
  formatVal = Vk.FORMAT_R32G32B32_SFLOAT

instance KnownFormat 'R32G32B32A32Sfloat where
  formatVal = Vk.FORMAT_R32G32B32A32_SFLOAT

instance KnownFormat 'D32Sfloat where
  formatVal = Vk.FORMAT_D32_SFLOAT

type family FormatComponentType (format :: Format) :: Type where
  FormatComponentType 'R8Unorm = Word8
  FormatComponentType 'R8G8B8A8Unorm = Word8
  FormatComponentType 'R8G8B8A8Srgb = Word8
  FormatComponentType 'B8G8R8A8Unorm = Word8
  FormatComponentType 'B8G8R8A8Srgb = Word8
  FormatComponentType 'R32Sfloat = Float
  FormatComponentType 'R32G32Sfloat = Float
  FormatComponentType 'R32G32B32Sfloat = Float
  FormatComponentType 'R32G32B32A32Sfloat = Float
  FormatComponentType 'D32Sfloat = Float

{- | The value shape returned when sampling an image of this format.
Normalized formats are converted to floating point by Vulkan sampling.
-}
type family TexelOf (format :: Format) :: Type where
  TexelOf 'R8Unorm = Float
  TexelOf 'R8G8B8A8Unorm = V4 Float
  TexelOf 'R8G8B8A8Srgb = V4 Float
  TexelOf 'B8G8R8A8Unorm = V4 Float
  TexelOf 'B8G8R8A8Srgb = V4 Float
  TexelOf 'R32Sfloat = Float
  TexelOf 'R32G32Sfloat = V2 Float
  TexelOf 'R32G32B32Sfloat = V3 Float
  TexelOf 'R32G32B32A32Sfloat = V4 Float
  TexelOf 'D32Sfloat = Float

type family FormatChannels (format :: Format) :: Nat where
  FormatChannels 'R8Unorm = 1
  FormatChannels 'R8G8B8A8Unorm = 4
  FormatChannels 'R8G8B8A8Srgb = 4
  FormatChannels 'B8G8R8A8Unorm = 4
  FormatChannels 'B8G8R8A8Srgb = 4
  FormatChannels 'R32Sfloat = 1
  FormatChannels 'R32G32Sfloat = 2
  FormatChannels 'R32G32B32Sfloat = 3
  FormatChannels 'R32G32B32A32Sfloat = 4
  FormatChannels 'D32Sfloat = 1

-- | Formats legal as dynamic-rendering colour attachments.
type family ColorRenderable (format :: Format) :: Constraint where
  ColorRenderable 'R8Unorm = ()
  ColorRenderable 'R8G8B8A8Unorm = ()
  ColorRenderable 'R8G8B8A8Srgb = ()
  ColorRenderable 'B8G8R8A8Unorm = ()
  ColorRenderable 'B8G8R8A8Srgb = ()
  ColorRenderable 'R32Sfloat = ()
  ColorRenderable 'R32G32Sfloat = ()
  ColorRenderable 'R32G32B32Sfloat = ()
  ColorRenderable 'R32G32B32A32Sfloat = ()
  ColorRenderable 'D32Sfloat = TypeErrorDepthFormatCannotBeColor

{- | Formats whose colour values may participate in fixed-function blending.
This is deliberately closed alongside @ColorRenderable@, so adding an
integer colour format requires an explicit blending decision.
-}
type family Blendable (format :: Format) :: Constraint where
  Blendable 'R8Unorm = ()
  Blendable 'R8G8B8A8Unorm = ()
  Blendable 'R8G8B8A8Srgb = ()
  Blendable 'B8G8R8A8Unorm = ()
  Blendable 'B8G8R8A8Srgb = ()
  Blendable 'R32Sfloat = ()
  Blendable 'R32G32Sfloat = ()
  Blendable 'R32G32B32Sfloat = ()
  Blendable 'R32G32B32A32Sfloat = ()
  Blendable 'D32Sfloat = TypeErrorDepthFormatCannotBeColor

-- | Formats legal as dynamic-rendering depth attachments.
type family DepthRenderable (format :: Format) :: Constraint where
  DepthRenderable 'D32Sfloat = ()
  DepthRenderable 'R8Unorm = TypeErrorColorFormatCannotBeDepth
  DepthRenderable 'R8G8B8A8Unorm = TypeErrorColorFormatCannotBeDepth
  DepthRenderable 'R8G8B8A8Srgb = TypeErrorColorFormatCannotBeDepth
  DepthRenderable 'B8G8R8A8Unorm = TypeErrorColorFormatCannotBeDepth
  DepthRenderable 'B8G8R8A8Srgb = TypeErrorColorFormatCannotBeDepth
  DepthRenderable 'R32Sfloat = TypeErrorColorFormatCannotBeDepth
  DepthRenderable 'R32G32Sfloat = TypeErrorColorFormatCannotBeDepth
  DepthRenderable 'R32G32B32Sfloat = TypeErrorColorFormatCannotBeDepth
  DepthRenderable 'R32G32B32A32Sfloat = TypeErrorColorFormatCannotBeDepth

type TypeErrorDepthFormatCannotBeColor =
  TypeError
    ('Text "A depth format cannot be used as a colour attachment.")

type TypeErrorColorFormatCannotBeDepth =
  TypeError
    ('Text "A colour format cannot be used as a depth attachment.")
