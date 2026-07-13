{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Type-level image descriptions and their Vulkan reflection.

Use promoted dimensions and usages in signatures so invalid combinations are
rejected before Vulkan resource creation:

@
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Data.Proxy (Proxy (..))
import Vpipe.Image.Types (Dim (D2), KnownDim (reflectedImageType))

main :: IO ()
main = print (reflectedImageType (Proxy @'D2))
@
-}
module Vpipe.Image.Types (
  Dim (..),
  ImageSubresource (..),
  ImageSubresourceOutOfBounds (..),
  KnownDim (..),
  ImageUsage (..),
  HasImageUsage,
  ValidImageUsages,
  KnownImageUsages (..),
) where

import Control.Exception (Exception)
import Data.Bits ((.|.))
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Vulkan.Core10.Enums.ImageCreateFlagBits qualified as ImageCreate
import Vulkan.Core10.Enums.ImageType qualified as ImageType
import Vulkan.Core10.Enums.ImageUsageFlagBits qualified as ImageUsage
import Vulkan.Core10.Enums.ImageViewType qualified as ImageViewType
import Vulkan.Zero (zero)

import Vpipe.Format (ColorRenderable, DepthRenderable, Format)

-- | Dimensionality of an image and its default full-image view.
data Dim = D1 | D2 | D3 | Cube | D2Array

-- | A zero-based mip level and array layer selected for transfer operations.
data ImageSubresource = ImageSubresource
  { imageMipLevel :: Word32
  , imageArrayLayer :: Word32
  }
  deriving stock (Eq, Ord, Show)

data ImageSubresourceOutOfBounds = ImageSubresourceOutOfBounds
  { requestedImageSubresource :: ImageSubresource
  , trackedImageMipLevels :: Word32
  , trackedImageArrayLayers :: Word32
  }
  deriving stock (Eq, Show)

instance Exception ImageSubresourceOutOfBounds

class KnownDim (dim :: Dim) where
  reflectedImageCreateFlags :: Proxy dim -> ImageCreate.ImageCreateFlags
  reflectedImageType :: Proxy dim -> ImageType.ImageType
  reflectedImageViewType :: Proxy dim -> ImageViewType.ImageViewType

instance KnownDim 'D1 where
  reflectedImageCreateFlags _ = zero
  reflectedImageType _ = ImageType.IMAGE_TYPE_1D
  reflectedImageViewType _ = ImageViewType.IMAGE_VIEW_TYPE_1D

instance KnownDim 'D2 where
  reflectedImageCreateFlags _ = zero
  reflectedImageType _ = ImageType.IMAGE_TYPE_2D
  reflectedImageViewType _ = ImageViewType.IMAGE_VIEW_TYPE_2D

instance KnownDim 'D3 where
  reflectedImageCreateFlags _ = zero
  reflectedImageType _ = ImageType.IMAGE_TYPE_3D
  reflectedImageViewType _ = ImageViewType.IMAGE_VIEW_TYPE_3D

instance KnownDim 'Cube where
  reflectedImageCreateFlags _ = ImageCreate.IMAGE_CREATE_CUBE_COMPATIBLE_BIT
  reflectedImageType _ = ImageType.IMAGE_TYPE_2D
  reflectedImageViewType _ = ImageViewType.IMAGE_VIEW_TYPE_CUBE

instance KnownDim 'D2Array where
  reflectedImageCreateFlags _ = zero
  reflectedImageType _ = ImageType.IMAGE_TYPE_2D
  reflectedImageViewType _ = ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY

-- | Roles an image can serve during command recording.
data ImageUsage = Sampled | ColorTarget | DepthTarget | Storage | CopySrc | CopyDst

type family HasImageUsage (needed :: ImageUsage) (usages :: [ImageUsage]) :: Constraint where
  HasImageUsage needed '[] =
    TypeError
      ( 'Text "Image operation requires usage "
          ':<>: 'ShowType needed
          ':<>: 'Text ", but this image's usage list does not contain it."
      )
  HasImageUsage needed (needed ': usages) = ()
  HasImageUsage needed (_ ': usages) = HasImageUsage needed usages

type ValidImageUsages format usages = (ValidImageUsageList format usages, ValidImageUsageCombination format usages)

type family ValidImageUsageList (format :: Format) (usages :: [ImageUsage]) :: Constraint where
  ValidImageUsageList _ '[] = TypeError ('Text "Image usage lists must not be empty.")
  ValidImageUsageList format (usage ': usages) = (ImageUsageNotRepeated usage usages, ImageUsageFormatLegal format usage, ValidImageUsageTail format usages)

type family ValidImageUsageTail (format :: Format) (usages :: [ImageUsage]) :: Constraint where
  ValidImageUsageTail _ '[] = ()
  ValidImageUsageTail format usages = ValidImageUsageList format usages

type family ImageUsageNotRepeated (usage :: ImageUsage) (usages :: [ImageUsage]) :: Constraint where
  ImageUsageNotRepeated _ '[] = ()
  ImageUsageNotRepeated usage (usage ': _) =
    TypeError ('Text "Image usage list contains duplicate usage " ':<>: 'ShowType usage ':<>: 'Text ".")
  ImageUsageNotRepeated usage (_ ': usages) = ImageUsageNotRepeated usage usages

type family ImageUsageFormatLegal (format :: Format) (usage :: ImageUsage) :: Constraint where
  ImageUsageFormatLegal format 'ColorTarget = ColorRenderable format
  ImageUsageFormatLegal format 'DepthTarget = DepthRenderable format
  ImageUsageFormatLegal _ _ = ()

type family ContainsImageUsage (needed :: ImageUsage) (usages :: [ImageUsage]) :: Bool where
  ContainsImageUsage _ '[] = 'False
  ContainsImageUsage needed (needed ': _) = 'True
  ContainsImageUsage needed (_ ': usages) = ContainsImageUsage needed usages

type family ValidImageUsageCombination (format :: Format) (usages :: [ImageUsage]) :: Constraint where
  ValidImageUsageCombination _ usages = ValidateAttachmentTargets (ContainsImageUsage 'ColorTarget usages) (ContainsImageUsage 'DepthTarget usages)

type family ValidateAttachmentTargets (hasColor :: Bool) (hasDepth :: Bool) :: Constraint where
  ValidateAttachmentTargets 'True 'True =
    TypeError ('Text "ColorTarget and DepthTarget usages cannot be combined on one image.")
  ValidateAttachmentTargets _ _ = ()

class KnownImageUsages (usages :: [ImageUsage]) where
  reflectedImageUsageFlags :: Proxy usages -> ImageUsage.ImageUsageFlags

instance KnownImageUsages '[] where
  reflectedImageUsageFlags _ = zero

instance (KnownImageUsages usages) => KnownImageUsages ('Sampled ': usages) where
  reflectedImageUsageFlags _ = ImageUsage.IMAGE_USAGE_SAMPLED_BIT .|. reflectedImageUsageFlags (Proxy @usages)

instance (KnownImageUsages usages) => KnownImageUsages ('ColorTarget ': usages) where
  reflectedImageUsageFlags _ = ImageUsage.IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|. reflectedImageUsageFlags (Proxy @usages)

instance (KnownImageUsages usages) => KnownImageUsages ('DepthTarget ': usages) where
  reflectedImageUsageFlags _ = ImageUsage.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT .|. reflectedImageUsageFlags (Proxy @usages)

instance (KnownImageUsages usages) => KnownImageUsages ('Storage ': usages) where
  reflectedImageUsageFlags _ = ImageUsage.IMAGE_USAGE_STORAGE_BIT .|. reflectedImageUsageFlags (Proxy @usages)

instance (KnownImageUsages usages) => KnownImageUsages ('CopySrc ': usages) where
  reflectedImageUsageFlags _ = ImageUsage.IMAGE_USAGE_TRANSFER_SRC_BIT .|. reflectedImageUsageFlags (Proxy @usages)

instance (KnownImageUsages usages) => KnownImageUsages ('CopyDst ': usages) where
  reflectedImageUsageFlags _ = ImageUsage.IMAGE_USAGE_TRANSFER_DST_BIT .|. reflectedImageUsageFlags (Proxy @usages)
