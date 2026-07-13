module Vpipe.Pipeline.Resource.Internal (
  RuntimeHandle (RuntimeHandle),
  RuntimeObjectKind (..),
  BufferBindingMetadata (..),
  ImageBindingMetadata (..),
  managedRuntimeHandle,
  managedRuntimeHandleWithQuarantine,
  managedBufferRuntimeHandle,
  managedBufferRuntimeHandleWithQuarantine,
  bufferRuntimeHandle,
  managedImageRuntimeHandle,
  managedImageRuntimeHandleWithQuarantine,
  managedSamplerRuntimeHandle,
  managedSamplerRuntimeHandleWithQuarantine,
  runtimeHandleWord,
  runtimeHandleOwner,
  runtimeHandleGeneration,
  runtimeHandleKind,
  runtimeHandleLease,
  runtimeHandleQuarantine,
  runtimeBufferMetadata,
  runtimeImageMetadata,
) where

import Control.Exception (mask_)
import Data.Unique (Unique)
import Data.Word (Word32, Word64)
import Vulkan.Core10.Enums.BufferUsageFlagBits qualified as BufferUsage
import Vulkan.Core10.Enums.Format qualified as Format
import Vulkan.Core10.Enums.ImageAspectFlagBits qualified as Aspect
import Vulkan.Core10.Enums.ImageUsageFlagBits qualified as ImageUsage
import Vulkan.Core10.Enums.SampleCountFlagBits qualified as Samples
import Vulkan.Core10.FundamentalTypes qualified as Fundamental
import Vulkan.Core10.Handles qualified as Handles

import Vpipe.Buffer.Internal qualified as Buffer
import Vpipe.Buffer.State qualified as BufferState
import Vpipe.Context.Internal (contextIdentity)
import Vpipe.Image.State qualified as ImageState
import Vpipe.Resource.Lifetime (ResourceGeneration)

data BufferBindingMetadata = BufferBindingMetadata
  { bufferBindingRawHandle :: Handles.Buffer
  , bufferBindingState :: BufferState.BufferState
  , bufferBindingElementCount :: Int
  , bufferBindingStride :: Int
  , bufferBindingByteOffset :: Word64
  , bufferBindingUsage :: BufferUsage.BufferUsageFlags
  }

data ImageBindingMetadata = ImageBindingMetadata
  { imageBindingRawHandle :: Handles.Image
  , imageBindingRawView :: Handles.ImageView
  , imageBindingState :: ImageState.ImageState
  , imageBindingExtent :: Fundamental.Extent3D
  , imageBindingFormat :: Format.Format
  , imageBindingAspect :: Aspect.ImageAspectFlags
  , imageBindingSamples :: Samples.SampleCountFlagBits
  , imageBindingMipLevel :: Word32
  , imageBindingArrayLayer :: Word32
  , imageBindingMipLevels :: Word32
  , imageBindingArrayLayers :: Word32
  , imageBindingUsage :: ImageUsage.ImageUsageFlags
  }

data RuntimeMetadata
  = BufferMetadata BufferBindingMetadata
  | ImageMetadata ImageBindingMetadata

data RuntimeObjectKind
  = RuntimeObjectRaw
  | RuntimeObjectBuffer
  | RuntimeObjectImageView
  | RuntimeObjectSampler
  deriving stock (Eq, Ord, Show)

data RuntimeHandle
  = RuntimeHandle Word64
  | ManagedRuntimeHandle Unique ResourceGeneration RuntimeObjectKind Word64 (IO (IO ())) (IO ()) (Maybe RuntimeMetadata)

instance Eq RuntimeHandle where
  left == right = runtimeHandleKey left == runtimeHandleKey right

instance Ord RuntimeHandle where
  compare left right = compare (runtimeHandleKey left) (runtimeHandleKey right)

instance Show RuntimeHandle where
  showsPrec precedence handle =
    showParen (precedence > 10) $
      showString constructorName . showChar ' ' . shows (runtimeHandleWord handle)
   where
    constructorName = case handle of
      RuntimeHandle _ -> "RuntimeHandle"
      ManagedRuntimeHandle{} -> "ManagedRuntimeHandle"

managedRuntimeHandle :: Unique -> ResourceGeneration -> Word64 -> IO (IO ()) -> RuntimeHandle
managedRuntimeHandle owner generation word acquire =
  managedRuntimeHandleWithQuarantine owner generation word acquire (pure ())

managedRuntimeHandleWithQuarantine :: Unique -> ResourceGeneration -> Word64 -> IO (IO ()) -> IO () -> RuntimeHandle
managedRuntimeHandleWithQuarantine owner generation word acquire quarantine =
  ManagedRuntimeHandle owner generation RuntimeObjectRaw word acquire (mask_ quarantine) Nothing

managedBufferRuntimeHandle :: Unique -> ResourceGeneration -> IO (IO ()) -> BufferBindingMetadata -> RuntimeHandle
managedBufferRuntimeHandle owner generation acquire metadata =
  managedBufferRuntimeHandleWithQuarantine owner generation acquire (BufferState.quarantineBufferState (bufferBindingState metadata)) metadata

managedBufferRuntimeHandleWithQuarantine :: Unique -> ResourceGeneration -> IO (IO ()) -> IO () -> BufferBindingMetadata -> RuntimeHandle
managedBufferRuntimeHandleWithQuarantine owner generation acquire quarantine metadata =
  ManagedRuntimeHandle owner generation RuntimeObjectBuffer (bufferHandleWord (bufferBindingRawHandle metadata)) acquire (mask_ quarantine) (Just (BufferMetadata metadata))

bufferRuntimeHandle :: Buffer.Buffer usages a -> RuntimeHandle
bufferRuntimeHandle buffer =
  managedBufferRuntimeHandleWithQuarantine
    (contextIdentity (Buffer.bufferRawContext buffer))
    (Buffer.bufferGeneration buffer)
    (Buffer.acquireBufferBindingLease buffer)
    (Buffer.quarantineBufferBinding buffer)
    BufferBindingMetadata
      { bufferBindingRawHandle = Buffer.bufferRawHandle buffer
      , bufferBindingState = Buffer.bufferRawState buffer
      , bufferBindingElementCount = Buffer.bufferLength buffer
      , bufferBindingStride = Buffer.bufferStride buffer
      , bufferBindingByteOffset = 0
      , bufferBindingUsage = Buffer.bufferRawUsageFlags buffer
      }

managedImageRuntimeHandle :: Unique -> ResourceGeneration -> IO (IO ()) -> ImageBindingMetadata -> RuntimeHandle
managedImageRuntimeHandle owner generation acquire metadata =
  managedImageRuntimeHandleWithQuarantine owner generation acquire (ImageState.quarantineImageState (imageBindingState metadata)) metadata

managedImageRuntimeHandleWithQuarantine :: Unique -> ResourceGeneration -> IO (IO ()) -> IO () -> ImageBindingMetadata -> RuntimeHandle
managedImageRuntimeHandleWithQuarantine owner generation acquire quarantine metadata =
  ManagedRuntimeHandle owner generation RuntimeObjectImageView (imageViewHandleWord (imageBindingRawView metadata)) acquire (mask_ quarantine) (Just (ImageMetadata metadata))

managedSamplerRuntimeHandle :: Unique -> ResourceGeneration -> Word64 -> IO (IO ()) -> RuntimeHandle
managedSamplerRuntimeHandle owner generation word acquire =
  managedSamplerRuntimeHandleWithQuarantine owner generation word acquire (pure ())

managedSamplerRuntimeHandleWithQuarantine :: Unique -> ResourceGeneration -> Word64 -> IO (IO ()) -> IO () -> RuntimeHandle
managedSamplerRuntimeHandleWithQuarantine owner generation word acquire quarantine =
  ManagedRuntimeHandle owner generation RuntimeObjectSampler word acquire (mask_ quarantine) Nothing

runtimeHandleWord :: RuntimeHandle -> Word64
runtimeHandleWord handle = case handle of
  RuntimeHandle word -> word
  ManagedRuntimeHandle _ _ _ word _ _ _ -> word

runtimeHandleOwner :: RuntimeHandle -> Maybe Unique
runtimeHandleOwner handle = case handle of
  RuntimeHandle _ -> Nothing
  ManagedRuntimeHandle owner _ _ _ _ _ _ -> Just owner

runtimeHandleGeneration :: RuntimeHandle -> Maybe ResourceGeneration
runtimeHandleGeneration handle = case handle of
  RuntimeHandle _ -> Nothing
  ManagedRuntimeHandle _ generation _ _ _ _ _ -> Just generation

runtimeHandleLease :: RuntimeHandle -> Maybe (IO (IO ()))
runtimeHandleLease handle = case handle of
  RuntimeHandle _ -> Nothing
  ManagedRuntimeHandle _ _ _ _ acquire _ _ -> Just acquire

runtimeHandleQuarantine :: RuntimeHandle -> IO ()
runtimeHandleQuarantine handle = case handle of
  RuntimeHandle _ -> pure ()
  ManagedRuntimeHandle _ _ _ _ _ quarantine _ -> quarantine

runtimeBufferMetadata :: RuntimeHandle -> Maybe BufferBindingMetadata
runtimeBufferMetadata handle = case handle of
  ManagedRuntimeHandle _ _ _ _ _ _ (Just (BufferMetadata metadata)) -> Just metadata
  _ -> Nothing

runtimeImageMetadata :: RuntimeHandle -> Maybe ImageBindingMetadata
runtimeImageMetadata handle = case handle of
  ManagedRuntimeHandle _ _ _ _ _ _ (Just (ImageMetadata metadata)) -> Just metadata
  _ -> Nothing

data RuntimeHandleKey
  = RawRuntimeHandleKey Word64
  | ManagedRuntimeHandleKey Unique RuntimeObjectKind ResourceGeneration
  deriving stock (Eq, Ord)

runtimeHandleKey :: RuntimeHandle -> RuntimeHandleKey
runtimeHandleKey handle = case handle of
  RuntimeHandle word -> RawRuntimeHandleKey word
  ManagedRuntimeHandle owner generation kind _ _ _ _ -> ManagedRuntimeHandleKey owner kind generation

runtimeHandleKind :: RuntimeHandle -> RuntimeObjectKind
runtimeHandleKind handle = case handle of
  RuntimeHandle _ -> RuntimeObjectRaw
  ManagedRuntimeHandle _ _ kind _ _ _ _ -> kind

bufferHandleWord :: Handles.Buffer -> Word64
bufferHandleWord (Handles.Buffer word) = word

imageViewHandleWord :: Handles.ImageView -> Word64
imageViewHandleWord (Handles.ImageView word) = word
