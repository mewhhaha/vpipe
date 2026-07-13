{-# LANGUAGE ScopedTypeVariables #-}

module Vpipe.Sampler.Internal (
  Sampler,
  newSampler,
  samplerDescription,
  rawSamplerHandle,
  samplerOwnerContext,
  samplerGeneration,
  acquireSamplerBindingLease,
  quarantineSamplerBinding,
) where

import Control.Concurrent.MVar (modifyMVarMasked)
import Control.Exception (Handler (..), catches, mask, mask_, onException, throwIO)
import Control.Monad (unless, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Unique (Unique)
import Data.Word (Word64)
import Vulkan.Core10.Enums.BorderColor qualified as VkBorderColor
import Vulkan.Core10.Enums.CompareOp qualified as VkCompareOp
import Vulkan.Core10.Enums.Filter qualified as VkFilter
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Enums.SamplerAddressMode qualified as VkAddressMode
import Vulkan.Core10.Enums.SamplerMipmapMode qualified as VkMipmapMode
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Core10.Sampler qualified as Vk
import Vulkan.Exception qualified as Vulkan
import Vulkan.Zero (zero)

import Vpipe.Context.Internal (Context, contextDevice, contextIdentity, contextMaxSamplerAnisotropy, contextMaxSamplerLodBias, contextSamplerAnisotropyEnabled, contextSamplerCache, derivedObjectName, registerContextFinalizerLeased, setObjectNameLeased, withContextLease)
import Vpipe.Error (VpipeError (..))
import Vpipe.Resource.Lifetime qualified as Lifetime
import Vpipe.Sampler.Types

data Sampler = Sampler
  { samplerOwnerContext :: Context
  , samplerContextIdentity :: Unique
  , samplerDescription :: SamplerDescription
  , rawSamplerHandle :: Vk.Sampler
  , samplerGeneration :: Lifetime.ResourceGeneration
  , samplerLifetimeGate :: Lifetime.LifetimeGate
  }

instance Eq Sampler where
  left == right =
    samplerContextIdentity left == samplerContextIdentity right
      && samplerGeneration left == samplerGeneration right

newSampler :: Context -> SamplerDescription -> IO Sampler
newSampler context description = withContextLease context $ mask $ \_ -> do
  validateDescription context description
  modifyMVarMasked (contextSamplerCache context) $ \cache ->
    case Map.lookup description cache of
      Just entry -> pure (cache, samplerFromCacheEntry context description entry)
      Nothing -> do
        let device = contextDevice context
            createInfo = samplerCreateInfo description
        generation <- Lifetime.newResourceGeneration
        lifetimeGate <- Lifetime.newLifetimeGate
        handle <- mapVulkanFailure "vkCreateSampler" (Vk.createSampler device createInfo Nothing)
        let release = Lifetime.sealLifetimeGate lifetimeGate >> Vk.destroySampler device handle Nothing
        setObjectNameLeased context ObjectType.OBJECT_TYPE_SAMPLER (samplerHandleWord handle) (derivedObjectName "sampler" (samplerHandleWord handle))
          `onException` release
        registerContextFinalizerLeased context release `onException` release
        let entry = SamplerCacheEntry handle generation lifetimeGate
        pure (Map.insert description entry cache, samplerFromCacheEntry context description entry)

samplerFromCacheEntry :: Context -> SamplerDescription -> SamplerCacheEntry -> Sampler
samplerFromCacheEntry context description entry =
  Sampler
    { samplerOwnerContext = context
    , samplerContextIdentity = contextIdentity context
    , samplerDescription = description
    , rawSamplerHandle = cachedSamplerHandle entry
    , samplerGeneration = cachedSamplerGeneration entry
    , samplerLifetimeGate = cachedSamplerLifetimeGate entry
    }

acquireSamplerBindingLease :: Sampler -> IO (IO ())
acquireSamplerBindingLease sampler = do
  lease <- Lifetime.acquireLifetimeLease (samplerLifetimeGate sampler)
  maybe (throwIO ContextClosed) pure lease

quarantineSamplerBinding :: Sampler -> IO ()
quarantineSamplerBinding = mask_ . Lifetime.quarantineLifetimeGate . samplerLifetimeGate

samplerHandleWord :: Handles.Sampler -> Word64
samplerHandleWord (Handles.Sampler handle) = handle

validateDescription :: Context -> SamplerDescription -> IO ()
validateDescription context description = do
  validateFinite "mip LOD bias" (samplerMipLodBias description)
  when (abs (samplerMipLodBias description) > contextMaxSamplerLodBias context) $
    invalid
      ( "absolute mip LOD bias "
          <> show (abs (samplerMipLodBias description))
          <> " exceeds the device limit "
          <> show (contextMaxSamplerLodBias context)
      )
  validateNonnegative "minimum LOD" (samplerLodMinimum description)
  validateNonnegative "maximum LOD" (samplerLodMaximum description)
  when (samplerLodMinimum description > samplerLodMaximum description) $
    invalid "minimum LOD must not exceed maximum LOD"
  case samplerAnisotropy description of
    Nothing -> pure ()
    Just anisotropy -> do
      validateFinite "anisotropy" anisotropy
      when (anisotropy < 1) (invalid "anisotropy must be at least 1")
      unless (contextSamplerAnisotropyEnabled context) $
        invalid "anisotropy was requested, but sampler anisotropy is not enabled on this device"
      when (anisotropy > contextMaxSamplerAnisotropy context) $
        invalid
          ( "anisotropy "
              <> show anisotropy
              <> " exceeds the device limit "
              <> show (contextMaxSamplerAnisotropy context)
          )
  when (samplerUnnormalizedCoordinates description) (validateUnnormalizedCoordinates description)

validateFinite :: String -> Float -> IO ()
validateFinite label value =
  when (isNaN value || isInfinite value) $
    invalid (label <> " must be finite, but was " <> show value)

validateNonnegative :: String -> Float -> IO ()
validateNonnegative label value = do
  validateFinite label value
  when (value < 0) (invalid (label <> " must be nonnegative, but was " <> show value))

validateUnnormalizedCoordinates :: SamplerDescription -> IO ()
validateUnnormalizedCoordinates description = do
  unless (samplerMagFilter description == Nearest && samplerMinFilter description == Nearest) $
    invalid "unnormalized coordinates require nearest minification and magnification filters"
  unless (samplerMipmapMode description == NearestMipmap) $
    invalid "unnormalized coordinates require nearest mipmap mode"
  unless (samplerLodMinimum description == 0 && samplerLodMaximum description == 0) $
    invalid "unnormalized coordinates require a zero LOD range"
  unless (samplerAddressModeU description `elem` [ClampToEdge, ClampToBorder]) $
    invalid "unnormalized coordinates require U addressing to clamp to edge or border"
  unless (samplerAddressModeV description `elem` [ClampToEdge, ClampToBorder]) $
    invalid "unnormalized coordinates require V addressing to clamp to edge or border"
  when (isJust (samplerAnisotropy description)) $
    invalid "unnormalized coordinates cannot use anisotropy"
  when (isJust (samplerCompareOp description)) $
    invalid "unnormalized coordinates cannot use comparison sampling"

invalid :: String -> IO a
invalid detail = throwIO (VulkanFailure "sampler description" detail)

samplerCreateInfo :: SamplerDescription -> Vk.SamplerCreateInfo '[]
samplerCreateInfo description =
  (zero :: Vk.SamplerCreateInfo '[])
    { Vk.magFilter = reflectedFilter (samplerMagFilter description)
    , Vk.minFilter = reflectedFilter (samplerMinFilter description)
    , Vk.mipmapMode = reflectedMipmapMode (samplerMipmapMode description)
    , Vk.addressModeU = reflectedAddressMode (samplerAddressModeU description)
    , Vk.addressModeV = reflectedAddressMode (samplerAddressModeV description)
    , Vk.addressModeW = reflectedAddressMode (samplerAddressModeW description)
    , Vk.mipLodBias = samplerMipLodBias description
    , Vk.anisotropyEnable = isJust (samplerAnisotropy description)
    , Vk.maxAnisotropy = fromMaybe 1 (samplerAnisotropy description)
    , Vk.compareEnable = isJust (samplerCompareOp description)
    , Vk.compareOp = maybe VkCompareOp.COMPARE_OP_NEVER reflectedCompareOp (samplerCompareOp description)
    , Vk.minLod = samplerLodMinimum description
    , Vk.maxLod = samplerLodMaximum description
    , Vk.borderColor = reflectedBorderColor (samplerBorderColor description)
    , Vk.unnormalizedCoordinates = samplerUnnormalizedCoordinates description
    }

reflectedFilter :: Filter -> VkFilter.Filter
reflectedFilter Nearest = VkFilter.FILTER_NEAREST
reflectedFilter Linear = VkFilter.FILTER_LINEAR

reflectedMipmapMode :: MipmapMode -> VkMipmapMode.SamplerMipmapMode
reflectedMipmapMode NearestMipmap = VkMipmapMode.SAMPLER_MIPMAP_MODE_NEAREST
reflectedMipmapMode LinearMipmap = VkMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR

reflectedAddressMode :: AddressMode -> VkAddressMode.SamplerAddressMode
reflectedAddressMode Repeat = VkAddressMode.SAMPLER_ADDRESS_MODE_REPEAT
reflectedAddressMode MirroredRepeat = VkAddressMode.SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
reflectedAddressMode ClampToEdge = VkAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
reflectedAddressMode ClampToBorder = VkAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER

reflectedCompareOp :: CompareOp -> VkCompareOp.CompareOp
reflectedCompareOp Never = VkCompareOp.COMPARE_OP_NEVER
reflectedCompareOp Less = VkCompareOp.COMPARE_OP_LESS
reflectedCompareOp Equal = VkCompareOp.COMPARE_OP_EQUAL
reflectedCompareOp LessOrEqual = VkCompareOp.COMPARE_OP_LESS_OR_EQUAL
reflectedCompareOp Greater = VkCompareOp.COMPARE_OP_GREATER
reflectedCompareOp NotEqual = VkCompareOp.COMPARE_OP_NOT_EQUAL
reflectedCompareOp GreaterOrEqual = VkCompareOp.COMPARE_OP_GREATER_OR_EQUAL
reflectedCompareOp Always = VkCompareOp.COMPARE_OP_ALWAYS

reflectedBorderColor :: BorderColor -> VkBorderColor.BorderColor
reflectedBorderColor FloatTransparentBlack = VkBorderColor.BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
reflectedBorderColor IntTransparentBlack = VkBorderColor.BORDER_COLOR_INT_TRANSPARENT_BLACK
reflectedBorderColor FloatOpaqueBlack = VkBorderColor.BORDER_COLOR_FLOAT_OPAQUE_BLACK
reflectedBorderColor IntOpaqueBlack = VkBorderColor.BORDER_COLOR_INT_OPAQUE_BLACK
reflectedBorderColor FloatOpaqueWhite = VkBorderColor.BORDER_COLOR_FLOAT_OPAQUE_WHITE
reflectedBorderColor IntOpaqueWhite = VkBorderColor.BORDER_COLOR_INT_OPAQUE_WHITE

mapVulkanFailure :: String -> IO a -> IO a
mapVulkanFailure operation action =
  action `catches` [Handler mapException]
 where
  mapException :: Vulkan.VulkanException -> IO a
  mapException error'
    | Vulkan.vulkanExceptionResult error' == Result.ERROR_DEVICE_LOST = throwIO DeviceLost
    | otherwise = throwIO (VulkanFailure operation (show (Vulkan.vulkanExceptionResult error')))
