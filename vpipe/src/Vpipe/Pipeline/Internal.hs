{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | A typed graphics pipeline DSL with direct SPIR-V compilation.

Compilation records shader interfaces, reifies stage roots, lowers them to
SPIR-V modules, and retains static pipeline state plus a plan for resolving
concrete environment resources. Vulkan object creation consumes that compiled
description in a later layer.

Compile a pipeline description after recording its stages and resources:

@
module Main (main) where

import Linear (V3)
import Vpipe.Expr
import Vpipe.Format (Format (R8G8B8A8Srgb))
import Vpipe.Pipeline

data Environment = Environment
  { environmentPositions :: VertexBuffer (V3 Float)
  , environmentColor :: ColorImage 'R8G8B8A8Srgb
  }

pipelineDescription :: PipelineM Environment ()
pipelineDescription = do
  positions <-
    vertexInput
      (vertexSource "positions" environmentPositions :: VertexSource Environment 'Triangles (V3 Float))
  fragments <-
    rasterize
      defaultRaster
      (fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (x position))) positions)
  drawColor
    defaultBlend
    (colorTarget "color" environmentColor)
    (fmap (\(Smooth red) -> vec4 red (constant 0) (constant 0) (constant 1)) fragments)

main :: IO ()
main = do
  result <- compilePipeline pipelineDescription
  case result of
    Left pipelineError -> fail (show pipelineError)
    Right compiled -> print (length (compiledPipelineDraws compiled))
@
-}
module Vpipe.Pipeline.Internal (
  PipelineM,
  PipelineError (..),
  PrimitiveTopology (..),
  CullMode (..),
  FrontFace (..),
  Raster (..),
  defaultRaster,
  BlendFactor (..),
  BlendOp (..),
  Blend (..),
  defaultBlend,
  DepthCompareOp (..),
  Depth (..),
  defaultDepth,
  KnownTopology,
  PrimitiveStream,
  zipStreams,
  FragmentStream,
  fragmentValue,
  mapFragments,
  Smooth (..),
  Flat (..),
  NoPerspective (..),
  VertexInput (..),
  GenericVertex (..),
  FragmentInput,
  ColorOutput,
  ColorOutputMatches,
  RuntimeHandle (RuntimeHandle),
  VertexBuffer (..),
  IndexBuffer,
  UniformBuffer (..),
  StorageBuffer (..),
  TextureBinding (..),
  TypedTextureBinding,
  ComparisonTextureBinding,
  ColorImage (..),
  DepthImage (..),
  vertexBufferBinding,
  indexBufferBinding,
  uniformBufferBinding,
  storageBufferBinding,
  textureBinding,
  typedTextureBinding,
  comparisonTextureBinding,
  colorImageBinding,
  depthImageBinding,
  VertexSource,
  IndexSource,
  Uniform,
  ShaderBlockValue,
  Storage,
  StorageRef,
  Texture,
  SampledTexture,
  ComparisonTexture,
  ColorTarget,
  DepthTarget,
  vertexSource,
  indexSource,
  uniformSource,
  storageSource,
  textureSource,
  sampledTextureSource,
  comparisonTextureSource,
  colorTarget,
  depthTarget,
  vertexInput,
  uniform,
  pushConstant,
  PushConstantRange (..),
  ResolvedPushConstant (..),
  storageBuffer,
  texture,
  sampledTexture,
  comparisonTexture,
  VertexStagePosition,
  rasterize,
  rasterizeIndexed,
  discardWhen,
  writeDepth,
  drawColor,
  drawDepth,
  compilePipeline,
  renderPipelineInterfaceTable,
  CompiledPipeline (..),
  CompiledDraw (..),
  CompiledVarying (..),
  CompiledColorOutput (..),
  CompiledDepthOutput (..),
  PipelineInterface (..),
  VertexBindingLayout (..),
  VertexAttribute (..),
  ResourceBinding (..),
  ShaderResourceShape (..),
  StorageAccess (..),
  resourceBindingKind,
  resourceBindingShaderType,
  ResourceKind (..),
  ColorAttachment (..),
  DepthAttachment (..),
  Interpolation (..),
  BindingPlan (..),
  PushConstantPlan (..),
  EnvironmentResolver (..),
  ResolvedBindingPlan (..),
  ResolvedVertexBuffer (..),
  ResolvedIndexBuffer (..),
  ResolvedUniformBuffer (..),
  ResolvedStorageBuffer (..),
  ResolvedTexture (..),
  ResolvedColorImage (..),
  ResolvedDepthImage (..),
  resolveBindingPlan,
  resolvePushConstantPlan,
  resolvePipelineBindings,
  resolvePipelinePushConstants,
) where

import Control.Exception (throwIO)
import Control.Monad (foldM, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (StateT, get, put, runStateT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Foldable (traverse_)
import Data.Int (Int32)
import Data.Kind (Constraint, Type)
import Data.List (find, intercalate, nub, sort)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (castPtr)
import GHC.Generics (Generic, K1, M1, Rep, (:*:) (..))
import GHC.TypeError (Unsatisfiable)
import GHC.TypeLits (ErrorMessage (..), KnownNat, TypeError, type (<=?))
import Linear (V2, V3, V4)
import Vpipe.Buffer.Format (BufferAlignment, BufferFormat (..), BufferSize, FieldLayout (..), HostFormat, Layout (..), LayoutStandard (Std140, Vertex), ScalarType (Boolean32), ShaderBlockFormat, layoutOf, staticBufferAlignment, staticBufferSize)
import Vpipe.Buffer.Format qualified as BufferFormat
import Vpipe.Buffer.Internal qualified as Buffer
import Vpipe.Context.Internal (contextIdentity)
import Vpipe.Diagnostics.Dump.Internal (ShaderDump (..), ShaderDumpStage (DumpFragment, DumpVertex), dumpCompiledModule)
import Vpipe.Error (VpipeError (VulkanFailure))
import Vpipe.Expr (BoolE, ComparisonSampledImage, Expr, F, KnownSampleDimension, SampledImage, Sampler2D, ShaderValue, V, sampleDimension)
import Vpipe.Expr qualified as Expr
import Vpipe.Expr.Internal (ImageDimension (Image2D), ShaderTy (..), SomeExpr (..), valueTy)
import Vpipe.Expr.Reify (NodeId, ReifiedForest (..), ReifiedNode (..), ReifiedOp (..), reifyExprForest)
import Vpipe.Format (
  Blendable,
  ColorRenderable,
  DepthRenderable,
  Format (..),
  KnownFormat,
  VertexFormat (..),
  VkFormat,
  formatVal,
 )
import Vpipe.Image.Internal qualified as Image
import Vpipe.Image.Types (Dim (D2), HasImageUsage)
import Vpipe.Image.Types qualified as ImageTypes
import Vpipe.Pipeline.Resource.Internal (BufferBindingMetadata (..), ImageBindingMetadata (..), RuntimeHandle (RuntimeHandle), bufferRuntimeHandle, managedImageRuntimeHandleWithQuarantine, managedSamplerRuntimeHandleWithQuarantine, runtimeBufferMetadata, runtimeHandleOwner)
import Vpipe.Sampler.Internal qualified as Sampler
import Vpipe.Sampler.Types qualified as SamplerTypes
import Vpipe.SpirV.Assembler (SpirVModule)
import Vpipe.SpirV.Codegen qualified as Codegen
import Vulkan.Core10.Enums.SampleCountFlagBits qualified as Samples
import Vulkan.Core10.Handles qualified as Handles

data PrimitiveTopology = Points | Lines | Triangles
  deriving stock (Eq, Ord, Show)

data CullMode = CullNone | CullFront | CullBack
  deriving stock (Eq, Ord, Show, Enum)

data FrontFace = FrontClockwise | FrontCounterClockwise
  deriving stock (Eq, Ord, Show, Enum)

data Raster = Raster
  { cullMode :: CullMode
  , frontFace :: FrontFace
  }
  deriving stock (Eq, Ord, Show)

defaultRaster :: Raster
defaultRaster = Raster CullNone FrontCounterClockwise

data BlendFactor
  = Zero
  | One
  | SourceColor
  | OneMinusSourceColor
  | DestinationColor
  | OneMinusDestinationColor
  | SourceAlpha
  | OneMinusSourceAlpha
  | DestinationAlpha
  | OneMinusDestinationAlpha
  | SourceAlphaSaturate
  deriving stock (Eq, Ord, Show, Enum)

data BlendOp = Add | Subtract | ReverseSubtract | Min | Max
  deriving stock (Eq, Ord, Show, Enum)

data Blend = Blend
  { blendEnabled :: Bool
  , blendSourceColorFactor :: BlendFactor
  , blendDestinationColorFactor :: BlendFactor
  , blendColorOp :: BlendOp
  , blendSourceAlphaFactor :: BlendFactor
  , blendDestinationAlphaFactor :: BlendFactor
  , blendAlphaOp :: BlendOp
  }
  deriving stock (Eq, Ord, Show)

defaultBlend :: Blend
defaultBlend = Blend False One Zero Add One Zero Add

data DepthCompareOp = DepthNever | DepthLess | DepthEqual | DepthLessOrEqual | DepthGreater | DepthNotEqual | DepthGreaterOrEqual | DepthAlways
  deriving stock (Eq, Ord, Show, Enum)

data Depth = Depth
  { depthTestEnabled :: Bool
  , depthWriteEnabled :: Bool
  , depthCompareOp :: DepthCompareOp
  }
  deriving stock (Eq, Ord, Show)

defaultDepth :: Depth
defaultDepth = Depth True True DepthLessOrEqual

class KnownTopology (topology :: PrimitiveTopology) where
  topologyValue :: proxy topology -> PrimitiveTopology

instance KnownTopology 'Points where topologyValue _ = Points
instance KnownTopology 'Lines where topologyValue _ = Lines
instance KnownTopology 'Triangles where topologyValue _ = Triangles

newtype PrimitiveStream (topology :: PrimitiveTopology) a = PrimitiveStream a
  deriving stock (Functor)

type family MatchingTopology (expected :: PrimitiveTopology) (actual :: PrimitiveTopology) :: Constraint where
  MatchingTopology topology topology = ()
  MatchingTopology expected actual =
    TypeError
      ( 'Text "zipStreams cannot combine primitive streams with different topologies."
          ':$$: 'Text "Expected topology: "
          ':<>: 'ShowType expected
          ':$$: 'Text "Actual topology: "
          ':<>: 'ShowType actual
          ':$$: 'Text "Fix: give both vertex sources the same PrimitiveTopology (Points, Lines, or Triangles)."
      )

zipStreams :: (MatchingTopology expected actual) => PrimitiveStream expected a -> PrimitiveStream actual b -> PrimitiveStream expected (a, b)
zipStreams (PrimitiveStream first) (PrimitiveStream second) = PrimitiveStream (first, second)

newtype VertexBuffer a = VertexBuffer {vertexBufferHandle :: RuntimeHandle}
  deriving stock (Eq, Ord, Show)
type role VertexBuffer nominal

{- | A whole-@Word32@ index buffer. Primitive restart values are stored
faithfully, but primitive restart is not enabled by @rasterizeIndexed@.
-}
newtype IndexBuffer = IndexBuffer {indexBufferHandle :: RuntimeHandle}
  deriving stock (Eq, Ord, Show)

newtype UniformBuffer a = UniformBuffer {uniformBufferHandle :: RuntimeHandle}
  deriving stock (Eq, Ord, Show)
type role UniformBuffer nominal

newtype StorageBuffer a = StorageBuffer {storageBufferHandle :: RuntimeHandle}
  deriving stock (Eq, Ord, Show)
type role StorageBuffer nominal

data TextureBinding = TextureBinding
  { textureImageHandle :: RuntimeHandle
  , textureSamplerHandle :: RuntimeHandle
  }
  deriving stock (Eq, Ord, Show)

data TypedTextureBinding (dim :: ImageTypes.Dim) (format :: Format) = TypedTextureBinding
  { typedTextureImageHandle :: RuntimeHandle
  , typedTextureSamplerHandle :: RuntimeHandle
  }
  deriving stock (Eq, Ord, Show)
type role TypedTextureBinding nominal nominal

data ComparisonTextureBinding (dim :: ImageTypes.Dim) = ComparisonTextureBinding
  { comparisonTextureImageHandle :: RuntimeHandle
  , comparisonTextureSamplerHandle :: RuntimeHandle
  }
  deriving stock (Eq, Ord, Show)
type role ComparisonTextureBinding nominal

newtype ColorImage (format :: Format) = ColorImage {colorImageHandle :: RuntimeHandle}
  deriving stock (Eq, Ord, Show)
type role ColorImage nominal

newtype DepthImage (format :: Format) = DepthImage {depthImageHandle :: RuntimeHandle}
  deriving stock (Eq, Ord, Show)
type role DepthImage nominal

vertexBufferBinding :: (Buffer.HasUsage 'Buffer.Vertex usages) => Buffer.Buffer usages a -> VertexBuffer a
vertexBufferBinding = VertexBuffer . bufferRuntimeHandle

indexBufferBinding :: (Buffer.HasUsage 'Buffer.Index usages) => Buffer.Buffer usages Word32 -> IndexBuffer
indexBufferBinding = IndexBuffer . bufferRuntimeHandle

uniformBufferBinding :: (Buffer.HasUsage 'Buffer.Uniform usages) => Buffer.Buffer usages a -> UniformBuffer a
uniformBufferBinding = UniformBuffer . bufferRuntimeHandle

storageBufferBinding :: (Buffer.HasUsage 'Buffer.Storage usages) => Buffer.Buffer usages a -> StorageBuffer a
storageBufferBinding = StorageBuffer . bufferRuntimeHandle

textureBinding :: (HasImageUsage 'ImageTypes.Sampled usages) => Image.Image 'D2 'R8G8B8A8Unorm usages -> Sampler.Sampler -> IO TextureBinding
textureBinding image samplerValue = do
  rejectComparisonSampler "regular texture binding" samplerValue
  ensureTextureOwnersMatch image samplerValue
  view <- imageViewRuntimeHandle image
  pure (TextureBinding view (samplerRuntimeHandle samplerValue))

typedTextureBinding :: (HasImageUsage 'ImageTypes.Sampled usages) => Image.Image dim format usages -> Sampler.Sampler -> IO (TypedTextureBinding dim format)
typedTextureBinding image samplerValue = do
  rejectComparisonSampler "regular typed texture binding" samplerValue
  ensureTextureOwnersMatch image samplerValue
  view <- imageViewRuntimeHandle image
  pure (TypedTextureBinding view (samplerRuntimeHandle samplerValue))

comparisonTextureBinding :: (HasImageUsage 'ImageTypes.Sampled usages) => Image.Image dim 'D32Sfloat usages -> Sampler.Sampler -> IO (ComparisonTextureBinding dim)
comparisonTextureBinding image samplerValue = do
  requireComparisonSampler samplerValue
  ensureTextureOwnersMatch image samplerValue
  view <- imageViewRuntimeHandle image
  pure (ComparisonTextureBinding view (samplerRuntimeHandle samplerValue))

colorImageBinding :: (HasImageUsage 'ImageTypes.ColorTarget usages) => Image.Image 'D2 format usages -> IO (ColorImage format)
colorImageBinding image = ColorImage <$> imageViewRuntimeHandle image

depthImageBinding :: (HasImageUsage 'ImageTypes.DepthTarget usages) => Image.Image 'D2 format usages -> IO (DepthImage format)
depthImageBinding image = DepthImage <$> imageViewRuntimeHandle image

samplerRuntimeHandle :: Sampler.Sampler -> RuntimeHandle
samplerRuntimeHandle samplerValue = case Sampler.rawSamplerHandle samplerValue of
  Handles.Sampler handle ->
    managedSamplerRuntimeHandleWithQuarantine
      (contextIdentity (Sampler.samplerOwnerContext samplerValue))
      (Sampler.samplerGeneration samplerValue)
      handle
      (Sampler.acquireSamplerBindingLease samplerValue)
      (Sampler.quarantineSamplerBinding samplerValue)

imageViewRuntimeHandle :: Image.Image dim format usages -> IO RuntimeHandle
imageViewRuntimeHandle image = case Image.imageRawView image of
  Just (Handles.ImageView handle) ->
    pure
      ( managedImageRuntimeHandleWithQuarantine
          (contextIdentity (Image.imageContext image))
          (Image.imageGeneration image)
          (Image.acquireImageBindingLease image)
          (Image.quarantineImageBinding image)
          ImageBindingMetadata
            { imageBindingRawHandle = Image.imageRawHandle image
            , imageBindingRawView = Handles.ImageView handle
            , imageBindingState = Image.imageRawState image
            , imageBindingExtent = Image.imageRawExtent3D image
            , imageBindingFormat = Image.imageRawFormat image
            , imageBindingAspect = Image.imageRawAspect image
            , imageBindingSamples = Samples.SAMPLE_COUNT_1_BIT
            , imageBindingMipLevel = 0
            , imageBindingArrayLayer = 0
            , imageBindingMipLevels = fromIntegral (Image.imageMipCount image)
            , imageBindingArrayLayers = fromIntegral (Image.imageLayerCount image)
            , imageBindingUsage = Image.imageRawUsageFlags image
            }
      )
  Nothing -> throwIO (VulkanFailure "pipeline image binding" "the typed image usage requires a view, but no view was created")

rejectComparisonSampler :: String -> Sampler.Sampler -> IO ()
rejectComparisonSampler operation samplerValue =
  when (isJust (SamplerTypes.samplerCompareOp (Sampler.samplerDescription samplerValue))) $
    throwIO (VulkanFailure operation "comparison-enabled samplers require comparisonTextureBinding")

requireComparisonSampler :: Sampler.Sampler -> IO ()
requireComparisonSampler samplerValue =
  unless (isJust (SamplerTypes.samplerCompareOp (Sampler.samplerDescription samplerValue))) $
    throwIO (VulkanFailure "comparison texture binding" "the sampler description has no comparison operation")

ensureTextureOwnersMatch :: Image.Image dim format usages -> Sampler.Sampler -> IO ()
ensureTextureOwnersMatch image samplerValue =
  when (contextIdentity (Image.imageContext image) /= contextIdentity (Sampler.samplerOwnerContext samplerValue)) $
    throwIO (VulkanFailure "pipeline texture binding" "image and sampler belong to different contexts")

data VertexSource env (topology :: PrimitiveTopology) a = VertexSource String (env -> VertexBuffer a)
data IndexSource env = IndexSource String (env -> IndexBuffer)
class (ShaderValue shader, BufferFormat (ShaderBlockFormat shader), HostFormat (ShaderBlockFormat shader) ~ ShaderBlockFormat shader) => ShaderBlockValue shader

instance ShaderBlockValue Float
instance ShaderBlockValue Int32
instance ShaderBlockValue Bool
instance ShaderBlockValue (V2 Float)
instance ShaderBlockValue (V3 Float)
instance ShaderBlockValue (V4 Float)
instance ShaderBlockValue (V2 (V2 Float))
instance ShaderBlockValue (V2 (V3 Float))
instance ShaderBlockValue (V2 (V4 Float))
instance ShaderBlockValue (V3 (V2 Float))
instance ShaderBlockValue (V3 (V3 Float))
instance ShaderBlockValue (V3 (V4 Float))
instance ShaderBlockValue (V4 (V2 Float))
instance ShaderBlockValue (V4 (V3 Float))
instance ShaderBlockValue (V4 (V4 Float))

data Uniform env shader = Uniform String (env -> UniformBuffer (ShaderBlockFormat shader))
data Storage env a = Storage String (env -> StorageBuffer a)
newtype StorageRef stage a = StorageRef String
data Texture env = Texture String (env -> TextureBinding)
data SampledTexture env (dim :: ImageTypes.Dim) (format :: Format) = SampledTexture String (env -> TypedTextureBinding dim format)
data ComparisonTexture env (dim :: ImageTypes.Dim) = ComparisonTexture String (env -> ComparisonTextureBinding dim)
data ColorTarget env (format :: Format) = ColorTarget String (env -> ColorImage format)
data DepthTarget env (format :: Format) = DepthTarget String (env -> DepthImage format)

vertexSource :: String -> (env -> VertexBuffer a) -> VertexSource env topology a
vertexSource = VertexSource

indexSource :: String -> (env -> IndexBuffer) -> IndexSource env
indexSource = IndexSource

uniformSource :: String -> (env -> UniformBuffer (ShaderBlockFormat shader)) -> Uniform env shader
uniformSource = Uniform

storageSource :: String -> (env -> StorageBuffer a) -> Storage env a
storageSource = Storage

textureSource :: String -> (env -> TextureBinding) -> Texture env
textureSource = Texture

sampledTextureSource :: String -> (env -> TypedTextureBinding dim format) -> SampledTexture env dim format
sampledTextureSource = SampledTexture

comparisonTextureSource :: String -> (env -> ComparisonTextureBinding dim) -> ComparisonTexture env dim
comparisonTextureSource = ComparisonTexture

colorTarget :: String -> (env -> ColorImage format) -> ColorTarget env format
colorTarget = ColorTarget

depthTarget :: String -> (env -> DepthImage format) -> DepthTarget env format
depthTarget = DepthTarget

class VertexInput host where
  type VertexInputShader host :: Type
  describeVertexInput :: proxy host -> String -> Int -> (VertexInputShader host, [VertexAttribute], Int)

instance VertexInput Float where
  type VertexInputShader Float = V (VertexShaderType Float)
  describeVertexInput _ = describeSingleVertexInput @Float

instance VertexInput (V2 Float) where
  type VertexInputShader (V2 Float) = V (VertexShaderType (V2 Float))
  describeVertexInput _ = describeSingleVertexInput @(V2 Float)

instance VertexInput (V3 Float) where
  type VertexInputShader (V3 Float) = V (VertexShaderType (V3 Float))
  describeVertexInput _ = describeSingleVertexInput @(V3 Float)

instance VertexInput (V4 Float) where
  type VertexInputShader (V4 Float) = V (VertexShaderType (V4 Float))
  describeVertexInput _ = describeSingleVertexInput @(V4 Float)

instance (VertexInput first, VertexInput second) => VertexInput (first, second) where
  type VertexInputShader (first, second) = (VertexInputShader first, VertexInputShader second)
  describeVertexInput _ name firstLocation =
    let (firstValue, firstAttributes, secondLocation) = describeVertexInput (Proxy @first) (name <> ".0") firstLocation
        (secondValue, secondAttributes, nextLocation) = describeVertexInput (Proxy @second) (name <> ".1") secondLocation
     in ((firstValue, secondValue), firstAttributes <> secondAttributes, nextLocation)

newtype GenericVertex host = GenericVertex {unGenericVertex :: host}

instance (BufferFormat (BufferFormat.Generically host), HostFormat (BufferFormat.Generically host) ~ host) => BufferFormat (GenericVertex host) where
  type HostFormat (GenericVertex host) = host
  type BufferShapeOf (GenericVertex host) = BufferShapeOf (BufferFormat.Generically host)
  type BufferAlignment (GenericVertex host) = BufferAlignment (BufferFormat.Generically host)
  type BufferOccupiedSize (GenericVertex host) = BufferOccupiedSize (BufferFormat.Generically host)
  type BufferSize (GenericVertex host) = BufferSize (BufferFormat.Generically host)
  bufferFieldLayout _ = bufferFieldLayout (Proxy @(BufferFormat.Generically host))
  bufferAlignmentFor standard _ = bufferAlignmentFor standard (Proxy @(BufferFormat.Generically host))
  bufferSizeFor standard _ = bufferSizeFor standard (Proxy @(BufferFormat.Generically host))
  pokeBufferFor standard _ = pokeBufferFor standard (Proxy @(BufferFormat.Generically host))
  peekBufferFor standard _ = peekBufferFor standard (Proxy @(BufferFormat.Generically host))

instance (Generic host, GVertexInput (Rep host)) => VertexInput (GenericVertex host) where
  type VertexInputShader (GenericVertex host) = GVertexInputShader (Rep host)
  describeVertexInput _ = describeGenericVertexInput (Proxy @(Rep host))

class GVertexInput representation where
  type GVertexInputShader representation :: Type
  describeGenericVertexInput :: proxy representation -> String -> Int -> (GVertexInputShader representation, [VertexAttribute], Int)

instance (GVertexInput representation) => GVertexInput (M1 metadata constructor representation) where
  type GVertexInputShader (M1 metadata constructor representation) = GVertexInputShader representation
  describeGenericVertexInput _ = describeGenericVertexInput (Proxy @representation)

instance (GVertexInput first, GVertexInput second) => GVertexInput (first :*: second) where
  type GVertexInputShader (first :*: second) = (GVertexInputShader first, GVertexInputShader second)
  describeGenericVertexInput _ name firstLocation =
    let (firstValue, firstAttributes, secondLocation) = describeGenericVertexInput (Proxy @first) (name <> ".0") firstLocation
        (secondValue, secondAttributes, nextLocation) = describeGenericVertexInput (Proxy @second) (name <> ".1") secondLocation
     in ((firstValue, secondValue), firstAttributes <> secondAttributes, nextLocation)

instance (VertexInput field) => GVertexInput (K1 metadata field) where
  type GVertexInputShader (K1 metadata field) = VertexInputShader field
  describeGenericVertexInput _ = describeVertexInput (Proxy @field)

describeSingleVertexInput :: forall host. (VertexFormat host, KnownFormat (VertexInputFormat host), ShaderValue (VertexShaderType host)) => String -> Int -> (V (VertexShaderType host), [VertexAttribute], Int)
describeSingleVertexInput name location =
  ( Expr.input ("vertex." <> name)
  , [VertexAttribute name name location (formatVal @(VertexInputFormat host)) (valueTy (Proxy @(VertexShaderType host))) 0]
  , location + 1
  )

newtype Smooth stage a = Smooth {unSmooth :: Expr stage a}
newtype Flat stage a = Flat {unFlat :: Expr stage a}
newtype NoPerspective stage a = NoPerspective {unNoPerspective :: Expr stage a}

data Interpolation = SmoothInterpolation | FlatInterpolation | NoPerspectiveInterpolation
  deriving stock (Eq, Ord, Show)

type family SmoothInterpolationAllowed a :: Constraint where
  SmoothInterpolationAllowed Float = ()
  SmoothInterpolationAllowed (V2 Float) = ()
  SmoothInterpolationAllowed (V3 Float) = ()
  SmoothInterpolationAllowed (V4 Float) = ()
  SmoothInterpolationAllowed Int32 =
    TypeError
      ('Text "Integral varyings must use Flat interpolation; Smooth and NoPerspective are invalid for Int32.")
  SmoothInterpolationAllowed a =
    TypeError
      ( 'Text "Smooth interpolation is unavailable for "
          ':<>: 'ShowType a
          ':<>: 'Text ". Use Flat or a floating scalar/vector value."
      )

type family FlatInterpolationAllowed a :: Constraint where
  FlatInterpolationAllowed Float = ()
  FlatInterpolationAllowed Int32 = ()
  FlatInterpolationAllowed (V2 Float) = ()
  FlatInterpolationAllowed (V3 Float) = ()
  FlatInterpolationAllowed (V4 Float) = ()
  FlatInterpolationAllowed a =
    TypeError
      ( 'Text "Flat interpolation is unavailable for "
          ':<>: 'ShowType a
          ':<>: 'Text ". Flat supports Float, Int32, and V2/V3/V4 Float."
      )

type family RawFragmentInputAllowed a :: Constraint where
  RawFragmentInputAllowed Int32 = ()
  RawFragmentInputAllowed a =
    TypeError
      ( 'Text "Raw varying "
          ':<>: 'ShowType a
          ':<>: 'Text " has no interpolation qualifier. Wrap it in Smooth, Flat, or NoPerspective."
      )

data FragmentInputs fragment = FragmentInputs fragment [PendingVarying] Int

class FragmentInput vertex fragment | vertex -> fragment where
  buildFragmentInputs :: Int -> vertex -> FragmentInputs fragment

instance forall a. (ShaderValue a, SmoothInterpolationAllowed a) => FragmentInput (Smooth 'Expr.Vertex a) (Smooth 'Expr.Fragment a) where
  buildFragmentInputs location (Smooth vertexValue) =
    let name = varyingName location
        fragmentValue = Expr.input name
     in FragmentInputs
          (Smooth fragmentValue)
          [PendingVarying location SmoothInterpolation name (valueTy (Proxy @a)) vertexValue fragmentValue]
          (location + 1)

instance forall a. (ShaderValue a, FlatInterpolationAllowed a) => FragmentInput (Flat 'Expr.Vertex a) (Flat 'Expr.Fragment a) where
  buildFragmentInputs location (Flat vertexValue) =
    let name = varyingName location
        fragmentValue = Expr.input name
     in FragmentInputs
          (Flat fragmentValue)
          [PendingVarying location FlatInterpolation name (valueTy (Proxy @a)) vertexValue fragmentValue]
          (location + 1)

instance forall a. (ShaderValue a, SmoothInterpolationAllowed a) => FragmentInput (NoPerspective 'Expr.Vertex a) (NoPerspective 'Expr.Fragment a) where
  buildFragmentInputs location (NoPerspective vertexValue) =
    let name = varyingName location
        fragmentValue = Expr.input name
     in FragmentInputs
          (NoPerspective fragmentValue)
          [PendingVarying location NoPerspectiveInterpolation name (valueTy (Proxy @a)) vertexValue fragmentValue]
          (location + 1)

instance forall a. (ShaderValue a, RawFragmentInputAllowed a) => FragmentInput (V a) (F a) where
  buildFragmentInputs location vertexValue =
    let name = varyingName location
        fragmentValue = Expr.input name
     in FragmentInputs
          fragmentValue
          [PendingVarying location FlatInterpolation name (valueTy (Proxy @a)) vertexValue fragmentValue]
          (location + 1)

instance (FragmentInput firstVertex firstFragment, FragmentInput secondVertex secondFragment) => FragmentInput (firstVertex, secondVertex) (firstFragment, secondFragment) where
  buildFragmentInputs firstLocation (firstVertex, secondVertex) =
    let FragmentInputs firstFragment firstVaryings secondLocation = buildFragmentInputs firstLocation firstVertex
        FragmentInputs secondFragment secondVaryings nextLocation = buildFragmentInputs secondLocation secondVertex
     in FragmentInputs (firstFragment, secondFragment) (firstVaryings <> secondVaryings) nextLocation

data FragmentStream a = FragmentStream
  { fragmentValue :: a
  , fragmentDrawIdentifier :: Int
  , fragmentDiscards :: [BoolE 'Expr.Fragment]
  , fragmentDepthOverride :: Maybe (F Float)
  }

instance Functor FragmentStream where
  fmap transform stream = stream{fragmentValue = transform (fragmentValue stream)}

mapFragments :: (a -> b) -> FragmentStream a -> FragmentStream b
mapFragments = fmap

discardWhen :: BoolE 'Expr.Fragment -> FragmentStream a -> FragmentStream a
discardWhen condition stream = stream{fragmentDiscards = fragmentDiscards stream <> [condition]}

{- | An explicit depth value takes precedence over the value passed to
'drawDepth'.
-}
writeDepth :: F Float -> FragmentStream a -> FragmentStream a
writeDepth value stream = stream{fragmentDepthOverride = Just value}

type family ColorOutput (format :: Format) :: Type where
  ColorOutput 'R8Unorm = Float
  ColorOutput 'R8G8B8A8Unorm = V4 Float
  ColorOutput 'R8G8B8A8Srgb = V4 Float
  ColorOutput 'B8G8R8A8Unorm = V4 Float
  ColorOutput 'B8G8R8A8Srgb = V4 Float
  ColorOutput 'R32Sfloat = Float
  ColorOutput 'R32G32Sfloat = V2 Float
  ColorOutput 'R32G32B32Sfloat = V3 Float
  ColorOutput 'R32G32B32A32Sfloat = V4 Float
  ColorOutput 'D32Sfloat =
    TypeError ('Text "D32Sfloat is a depth format and cannot receive a color output.")

type ColorOutputMatches format output =
  ColorOutputDiagnostic format output

type family ColorOutputDiagnostic (format :: Format) (output :: Type) :: Constraint where
  ColorOutputDiagnostic 'D32Sfloat output = Unsatisfiable (ColorOutputMismatch 'D32Sfloat output)
  ColorOutputDiagnostic format output = ColorOutputDiagnosticResult format (ColorOutput format) output

type family ColorOutputDiagnosticResult (format :: Format) (expected :: Type) (actual :: Type) :: Constraint where
  ColorOutputDiagnosticResult _ output output = ()
  ColorOutputDiagnosticResult format _ actual = Unsatisfiable (ColorOutputMismatch format actual)

type family ColorOutputMismatch (format :: Format) (output :: Type) :: ErrorMessage where
  ColorOutputMismatch 'D32Sfloat _ =
    'Text "drawColor cannot write to target format D32Sfloat because it is a depth format."
      ':$$: 'Text "Fix: use drawDepth with a DepthTarget instead."
  ColorOutputMismatch format output =
    'Text "drawColor output does not match target format "
      ':<>: 'ShowType format
      ':<>: 'Text "."
      ':$$: 'Text "Expected color value: "
      ':<>: 'ShowType (ColorOutput format)
      ':$$: 'Text "Actual color value: "
      ':<>: 'ShowType output
      ':$$: 'Text "Fix: pass a fragment stream containing the expected scalar/vector value; use vec2, vec3, or vec4 for vector formats."

type family PushConstantSizeAllowed a :: Constraint where
  PushConstantSizeAllowed a = PushConstantSizeCheck (BufferSize a <=? 128) a

type family PushConstantSizeCheck fits a :: Constraint where
  PushConstantSizeCheck 'True _ = ()
  PushConstantSizeCheck 'False a =
    TypeError
      ( 'Text "pushConstant values must occupy at most 128 bytes; "
          ':<>: 'ShowType a
          ':<>: 'Text " exceeds that limit."
      )

data ResourceKind = UniformResource | TextureResource | StorageResource
  deriving stock (Eq, Ord, Show)

data VertexAttribute = VertexAttribute
  { vertexAttributeSourceName :: String
  , vertexAttributeName :: String
  , vertexAttributeLocation :: Int
  , vertexAttributeFormat :: VkFormat
  , vertexAttributeShaderType :: ShaderTy
  , vertexAttributeOffset :: Int
  }
  deriving stock (Eq, Show)

data VertexBindingLayout = VertexBindingLayout
  { vertexBindingSourceName :: String
  , vertexBindingStride :: Int
  }
  deriving stock (Eq, Show)

data ShaderResourceShape
  = UniformShape ShaderTy FieldLayout
  | CombinedTextureShape ImageDimension
  | StorageShape
  | StorageArrayShape ShaderTy FieldLayout StorageAccess
  deriving stock (Eq, Show)

data StorageAccess = StorageReadOnly | StorageWriteOnly | StorageReadWrite | StorageAtomic
  deriving stock (Eq, Ord, Show)

data ResourceBinding = ResourceBinding
  { resourceBindingName :: String
  , resourceBindingSet :: Int
  , resourceBindingBinding :: Int
  , resourceBindingShape :: ShaderResourceShape
  }
  deriving stock (Eq, Show)

resourceBindingKind :: ResourceBinding -> ResourceKind
resourceBindingKind binding = case resourceBindingShape binding of
  UniformShape{} -> UniformResource
  CombinedTextureShape{} -> TextureResource
  StorageShape -> StorageResource
  StorageArrayShape{} -> StorageResource

resourceBindingShaderType :: ResourceBinding -> Maybe ShaderTy
resourceBindingShaderType binding = case resourceBindingShape binding of
  UniformShape shaderType _ -> Just shaderType
  CombinedTextureShape{} -> Nothing
  StorageShape -> Nothing
  StorageArrayShape shaderType _ _ -> Just shaderType

data ColorAttachment = ColorAttachment
  { colorAttachmentName :: String
  , colorAttachmentLocation :: Int
  , colorAttachmentFormat :: VkFormat
  }
  deriving stock (Eq, Show)

data DepthAttachment = DepthAttachment
  { depthAttachmentName :: String
  , depthAttachmentFormat :: VkFormat
  }
  deriving stock (Eq, Show)

data PipelineInterface = PipelineInterface
  { pipelineVertexAttributes :: [VertexAttribute]
  , pipelineVertexBindings :: [VertexBindingLayout]
  , pipelineResources :: [ResourceBinding]
  , pipelineColorAttachments :: [ColorAttachment]
  , pipelineDepthAttachments :: [DepthAttachment]
  , pipelinePushConstants :: [PushConstantRange]
  }
  deriving stock (Eq, Show)

-- | Render the stable, tabular interface text included in shader diagnostics.
renderPipelineInterfaceTable :: PipelineInterface -> String
renderPipelineInterfaceTable interface =
  unlines
    ( section
        "vertex bindings"
        ["source", "stride"]
        [ [show (vertexBindingSourceName binding), show (vertexBindingStride binding)]
        | binding <- pipelineVertexBindings interface
        ]
        <> section
          "vertex attributes"
          ["location", "source", "name", "format", "offset", "shader type"]
          [ [ show (vertexAttributeLocation attribute)
            , show (vertexAttributeSourceName attribute)
            , show (vertexAttributeName attribute)
            , show (vertexAttributeFormat attribute)
            , show (vertexAttributeOffset attribute)
            , show (vertexAttributeShaderType attribute)
            ]
          | attribute <- pipelineVertexAttributes interface
          ]
        <> section
          "resources"
          ["set", "binding", "kind", "name", "shape"]
          [ [ show (resourceBindingSet resource)
            , show (resourceBindingBinding resource)
            , show (resourceBindingKind resource)
            , show (resourceBindingName resource)
            , show (resourceBindingShape resource)
            ]
          | resource <- pipelineResources interface
          ]
        <> section
          "color attachments"
          ["location", "name", "format"]
          [ [ show (colorAttachmentLocation attachment)
            , show (colorAttachmentName attachment)
            , show (colorAttachmentFormat attachment)
            ]
          | attachment <- pipelineColorAttachments interface
          ]
        <> section
          "depth attachments"
          ["name", "format"]
          [ [show (depthAttachmentName attachment), show (depthAttachmentFormat attachment)]
          | attachment <- pipelineDepthAttachments interface
          ]
        <> section
          "push constants"
          ["offset", "size", "name", "shader type", "layout"]
          [ [ show (pushConstantOffset range)
            , show (pushConstantSize range)
            , show (pushConstantName range)
            , show (pushConstantShaderType range)
            , show (pushConstantFieldLayout range)
            ]
          | range <- pipelinePushConstants interface
          ]
    )
 where
  section label headings rows =
    ["", label <> ":"]
      <> case rows of
        [] -> ["(none)"]
        _ -> intercalate "\t" headings : fmap (intercalate "\t") rows

data PushConstantRange = PushConstantRange
  { pushConstantName :: String
  , pushConstantOffset :: Int
  , pushConstantSize :: Int
  , pushConstantShaderType :: ShaderTy
  , pushConstantFieldLayout :: FieldLayout
  }
  deriving stock (Eq, Show)

data ResolvedPushConstant = ResolvedPushConstant
  { resolvedPushConstantName :: String
  , resolvedPushConstantOffset :: Int
  , resolvedPushConstantBytes :: ByteString
  }
  deriving stock (Eq, Show)

data PipelineError
  = VertexSourceConflict String [VkFormat] [VkFormat]
  | ResourceConflict String ResourceKind (Maybe ShaderTy) ResourceKind (Maybe ShaderTy)
  | ColorTargetConflict String VkFormat VkFormat
  | DepthTargetConflict String VkFormat VkFormat
  | UnsupportedBooleanBufferLayout String FieldLayout
  | TargetKindConflict String
  | AccessorConflict String
  | RuntimeResourceAlias RuntimeHandle
  | PushConstantTotalSizeExceeded Int
  | MultipleDepthTargets Int String String
  | MissingDraw Int
  | ReificationRootMismatch String Int Int
  | GraphicsShaderCompilationFailed Int Codegen.ShaderStage String
  deriving stock (Eq, Show)

data CompiledVarying = CompiledVarying
  { compiledVaryingLocation :: Int
  , compiledVaryingInterpolation :: Interpolation
  , compiledVaryingName :: String
  , compiledVaryingShaderType :: ShaderTy
  , compiledVertexVaryingRoot :: NodeId
  , compiledFragmentVaryingRoot :: NodeId
  }
  deriving stock (Eq, Show)

data CompiledColorOutput = CompiledColorOutput
  { compiledColorTargetName :: String
  , compiledColorLocation :: Int
  , compiledColorFormat :: VkFormat
  , compiledColorBlend :: Blend
  , compiledColorRoot :: NodeId
  }
  deriving stock (Eq, Show)

data CompiledDepthOutput = CompiledDepthOutput
  { compiledDepthTargetName :: String
  , compiledDepthFormat :: VkFormat
  , compiledDepthState :: Depth
  , compiledDepthRoot :: NodeId
  }
  deriving stock (Eq, Show)

data CompiledDraw = CompiledDraw
  { compiledDrawTopology :: PrimitiveTopology
  , compiledDrawRaster :: Raster
  , compiledDrawIdentifier :: Int
  , compiledDrawIndexSource :: Maybe String
  , compiledVertexModule :: SpirVModule
  , compiledFragmentModule :: SpirVModule
  , compiledVertexBindings :: [VertexBindingLayout]
  , compiledVertexAttributes :: [VertexAttribute]
  , compiledVertexForest :: ReifiedForest
  , compiledClipPositionRoot :: NodeId
  , compiledVaryings :: [CompiledVarying]
  , compiledFragmentForest :: ReifiedForest
  , compiledColorOutputs :: [CompiledColorOutput]
  , compiledDepthOutput :: Maybe CompiledDepthOutput
  , compiledDiscardPredicates :: [NodeId]
  }
  deriving stock (Eq, Show)

data ResolvedVertexBuffer = ResolvedVertexBuffer String [Int] RuntimeHandle
  deriving stock (Eq, Show)

data ResolvedIndexBuffer = ResolvedIndexBuffer String RuntimeHandle
  deriving stock (Eq, Show)

data ResolvedUniformBuffer = ResolvedUniformBuffer String Int Int RuntimeHandle
  deriving stock (Eq, Show)

data ResolvedStorageBuffer = ResolvedStorageBuffer String Int Int RuntimeHandle
  deriving stock (Eq, Show)

data ResolvedTexture = ResolvedTexture String Int Int RuntimeHandle RuntimeHandle
  deriving stock (Eq, Show)

data ResolvedColorImage = ResolvedColorImage String Int Int RuntimeHandle
  deriving stock (Eq, Show)

data ResolvedDepthImage = ResolvedDepthImage String RuntimeHandle
  deriving stock (Eq, Show)

data ResolvedBindingPlan = ResolvedBindingPlan
  { resolvedVertexBuffers :: [ResolvedVertexBuffer]
  , resolvedIndexBuffers :: [ResolvedIndexBuffer]
  , resolvedUniformBuffers :: [ResolvedUniformBuffer]
  , resolvedStorageBuffers :: [ResolvedStorageBuffer]
  , resolvedTextures :: [ResolvedTexture]
  , resolvedColorImages :: [ResolvedColorImage]
  , resolvedDepthImages :: [ResolvedDepthImage]
  }
  deriving stock (Eq, Show)

newtype BindingPlan env = BindingPlan [EnvironmentResolver env]

newtype PushConstantPlan env = PushConstantPlan [env -> IO ResolvedPushConstant]

data EnvironmentResolver env
  = ResolveVertexBuffer (env -> ResolvedVertexBuffer)
  | ResolveIndexBuffer (env -> ResolvedIndexBuffer)
  | ResolveUniformBuffer (env -> ResolvedUniformBuffer)
  | ResolveStorageBuffer (env -> ResolvedStorageBuffer)
  | ResolveTexture (env -> ResolvedTexture)
  | ResolveColorImage (env -> ResolvedColorImage)
  | ResolveDepthImage (env -> ResolvedDepthImage)

data CompiledPipeline env = CompiledPipeline
  { compiledPipelineInterface :: PipelineInterface
  , compiledPipelineDraws :: [CompiledDraw]
  , compiledPipelineBindingPlan :: BindingPlan env
  , compiledPipelinePushConstantPlan :: PushConstantPlan env
  }

resolveBindingPlan :: BindingPlan env -> env -> Either PipelineError ResolvedBindingPlan
resolveBindingPlan (BindingPlan resolvers) environment = foldM resolve emptyResolvedPlan resolvers
 where
  resolve plan resolver = case resolver of
    ResolveVertexBuffer accessor -> coalesceVertex plan (accessor environment)
    ResolveIndexBuffer accessor -> coalesceIndex plan (accessor environment)
    ResolveUniformBuffer accessor -> coalesceUniform plan (accessor environment)
    ResolveStorageBuffer accessor -> coalesceStorage plan (accessor environment)
    ResolveTexture accessor -> coalesceTexture plan (accessor environment)
    ResolveColorImage accessor -> coalesceColor plan (accessor environment)
    ResolveDepthImage accessor -> coalesceDepth plan (accessor environment)

resolvePushConstantPlan :: PushConstantPlan env -> env -> IO [ResolvedPushConstant]
resolvePushConstantPlan (PushConstantPlan resolvers) environment = traverse (\resolve -> resolve environment) resolvers

resolvePipelineBindings :: CompiledPipeline env -> env -> Either PipelineError ResolvedBindingPlan
resolvePipelineBindings pipeline = resolveBindingPlan (compiledPipelineBindingPlan pipeline)

resolvePipelinePushConstants :: CompiledPipeline env -> env -> IO [ResolvedPushConstant]
resolvePipelinePushConstants pipeline = resolvePushConstantPlan (compiledPipelinePushConstantPlan pipeline)

emptyResolvedPlan :: ResolvedBindingPlan
emptyResolvedPlan = ResolvedBindingPlan [] [] [] [] [] [] []

coalesceVertex :: ResolvedBindingPlan -> ResolvedVertexBuffer -> Either PipelineError ResolvedBindingPlan
coalesceVertex plan value =
  coalesceResolved vertexName value (resolvedVertexBuffers plan) (\values -> plan{resolvedVertexBuffers = values})

coalesceIndex :: ResolvedBindingPlan -> ResolvedIndexBuffer -> Either PipelineError ResolvedBindingPlan
coalesceIndex plan value =
  coalesceResolved indexName value (resolvedIndexBuffers plan) (\values -> plan{resolvedIndexBuffers = values})

coalesceUniform :: ResolvedBindingPlan -> ResolvedUniformBuffer -> Either PipelineError ResolvedBindingPlan
coalesceUniform plan value =
  case find (sameRuntimeBuffer (resolvedUniformHandle value) . resolvedStorageHandle) (resolvedStorageBuffers plan) of
    Just _ -> Left (RuntimeResourceAlias (resolvedUniformHandle value))
    Nothing -> coalesceResolved uniformName value (resolvedUniformBuffers plan) (\values -> plan{resolvedUniformBuffers = values})

coalesceStorage :: ResolvedBindingPlan -> ResolvedStorageBuffer -> Either PipelineError ResolvedBindingPlan
coalesceStorage plan value =
  case find (sameRuntimeBuffer (resolvedStorageHandle value) . resolvedUniformHandle) (resolvedUniformBuffers plan) of
    Just _ -> Left (RuntimeResourceAlias (resolvedStorageHandle value))
    Nothing -> coalesceResolved storageName value (resolvedStorageBuffers plan) (\values -> plan{resolvedStorageBuffers = values})

coalesceTexture :: ResolvedBindingPlan -> ResolvedTexture -> Either PipelineError ResolvedBindingPlan
coalesceTexture plan value =
  coalesceResolved textureName value (resolvedTextures plan) (\values -> plan{resolvedTextures = values})

coalesceColor :: ResolvedBindingPlan -> ResolvedColorImage -> Either PipelineError ResolvedBindingPlan
coalesceColor plan value =
  case find (sameColorSlot value) (resolvedColorImages plan) of
    Nothing -> Right plan{resolvedColorImages = resolvedColorImages plan <> [value]}
    Just existing
      | existing == value -> Right plan
      | otherwise -> Left (AccessorConflict (resolvedColorName value))

coalesceDepth :: ResolvedBindingPlan -> ResolvedDepthImage -> Either PipelineError ResolvedBindingPlan
coalesceDepth plan value =
  coalesceResolved depthName value (resolvedDepthImages plan) (\values -> plan{resolvedDepthImages = values})

coalesceResolved :: (Eq value) => (value -> String) -> value -> [value] -> ([value] -> ResolvedBindingPlan) -> Either PipelineError ResolvedBindingPlan
coalesceResolved identity value values buildPlan =
  case find ((== identity value) . identity) values of
    Nothing -> Right (buildPlan (values <> [value]))
    Just existing
      | existing == value -> Right (buildPlan values)
      | otherwise -> Left (AccessorConflict (identity value))

sameRuntimeBuffer :: RuntimeHandle -> RuntimeHandle -> Bool
sameRuntimeBuffer left right
  | left == right = True
  | otherwise = case (runtimeHandleOwner left, runtimeHandleOwner right, runtimeBufferMetadata left, runtimeBufferMetadata right) of
      (Just leftOwner, Just rightOwner, Just leftMetadata, Just rightMetadata) ->
        leftOwner == rightOwner
          && bufferBindingRawHandle leftMetadata == bufferBindingRawHandle rightMetadata
      _ -> False

vertexName :: ResolvedVertexBuffer -> String
vertexName (ResolvedVertexBuffer name _ _) = name

indexName :: ResolvedIndexBuffer -> String
indexName (ResolvedIndexBuffer name _) = name

uniformName :: ResolvedUniformBuffer -> String
uniformName (ResolvedUniformBuffer name _ _ _) = name

resolvedUniformHandle :: ResolvedUniformBuffer -> RuntimeHandle
resolvedUniformHandle (ResolvedUniformBuffer _ _ _ handle) = handle

storageName :: ResolvedStorageBuffer -> String
storageName (ResolvedStorageBuffer name _ _ _) = name

resolvedStorageHandle :: ResolvedStorageBuffer -> RuntimeHandle
resolvedStorageHandle (ResolvedStorageBuffer _ _ _ handle) = handle

textureName :: ResolvedTexture -> String
textureName (ResolvedTexture name _ _ _ _) = name

resolvedColorName :: ResolvedColorImage -> String
resolvedColorName (ResolvedColorImage name _ _ _) = name

sameColorSlot :: ResolvedColorImage -> ResolvedColorImage -> Bool
sameColorSlot (ResolvedColorImage name drawIdentifier _ _) (ResolvedColorImage otherName otherDrawIdentifier _ _) =
  name == otherName && drawIdentifier == otherDrawIdentifier

depthName :: ResolvedDepthImage -> String
depthName (ResolvedDepthImage name _) = name

data PendingVarying = forall a. (ShaderValue a) => PendingVarying Int Interpolation String ShaderTy (V a) (F a)
data SomeFragmentRoot = forall a. (ShaderValue a) => SomeFragmentRoot (F a)
data PendingColorOutput = PendingColorOutput ColorAttachment Int Blend SomeFragmentRoot
data PendingDepthOutput = PendingDepthOutput DepthAttachment Depth SomeFragmentRoot

data PendingDraw = PendingDraw
  { pendingDrawIdentifier :: Int
  , pendingDrawTopology :: PrimitiveTopology
  , pendingDrawRaster :: Raster
  , pendingDrawIndexSource :: Maybe String
  , pendingClipPosition :: SomeExpr
  , pendingVaryings :: [PendingVarying]
  , pendingColorOutputs :: [PendingColorOutput]
  , pendingDepthOutput :: Maybe PendingDepthOutput
  , pendingDiscardPredicates :: [BoolE 'Expr.Fragment]
  }

data Recorder env = Recorder
  { recordedInterface :: PipelineInterface
  , recordedDraws :: [PendingDraw]
  , recordedResolvers :: [EnvironmentResolver env]
  , recordedPushConstantResolvers :: [env -> IO ResolvedPushConstant]
  , nextDrawIdentifier :: Int
  }

newtype PipelineM env a = PipelineM {unPipelineM :: StateT (Recorder env) (Either PipelineError) a}
  deriving newtype (Functor, Applicative, Monad)

emptyInterface :: PipelineInterface
emptyInterface = PipelineInterface [] [] [] [] [] []

emptyRecorder :: Recorder env
emptyRecorder = Recorder emptyInterface [] [] [] 0

vertexInput :: forall host env topology. (VertexInput host, BufferFormat host) => VertexSource env topology host -> PipelineM env (PrimitiveStream topology (VertexInputShader host))
vertexInput (VertexSource sourceName accessor) = PipelineM $ do
  recorder <- get
  let interface = recordedInterface recorder
      existing = filter ((== sourceName) . vertexAttributeSourceName) (pipelineVertexAttributes interface)
      existingBinding = find ((== sourceName) . vertexBindingSourceName) (pipelineVertexBindings interface)
      existingFormats = fmap vertexAttributeFormat existing
      firstLocation = case existing of
        attribute : _ -> vertexAttributeLocation attribute
        [] -> length (pipelineVertexAttributes interface)
      (shaderValues, describedAttributes, _) = describeVertexInput (Proxy @host) sourceName firstLocation
      offsets = vertexLeafOffsets (bufferFieldLayout (Proxy @host))
      attributes = zipWith (\attribute offset -> attribute{vertexAttributeSourceName = sourceName, vertexAttributeOffset = offset}) describedAttributes offsets
      requestedFormats = fmap vertexAttributeFormat attributes
      requestedBinding = VertexBindingLayout sourceName (layoutSize (layoutOf Vertex (bufferFieldLayout (Proxy @host))))
  if not (null existing) && existingFormats /= requestedFormats
    then throwError (VertexSourceConflict sourceName existingFormats requestedFormats)
    else do
      let isNew = null existing
          updatedAttributes = if isNew then pipelineVertexAttributes interface <> attributes else pipelineVertexAttributes interface
          updatedBindings = case existingBinding of
            Nothing -> pipelineVertexBindings interface <> [requestedBinding]
            Just _ -> pipelineVertexBindings interface
          locations = fmap vertexAttributeLocation (if isNew then attributes else existing)
          updatedResolvers = recordedResolvers recorder <> [ResolveVertexBuffer (resolveVertex sourceName locations accessor)]
      put
        recorder
          { recordedInterface = interface{pipelineVertexAttributes = updatedAttributes, pipelineVertexBindings = updatedBindings}
          , recordedResolvers = updatedResolvers
          }
      pure (PrimitiveStream shaderValues)

uniform :: forall shader stage env. (ShaderBlockValue shader) => Uniform env shader -> PipelineM env (Expr stage shader)
uniform (Uniform name accessor) = PipelineM $ do
  let shaderType = valueTy (Proxy @shader)
      fieldLayout = bufferFieldLayout (Proxy @(ShaderBlockFormat shader))
  rejectBooleanBufferLayout name fieldLayout
  recordResource name (UniformShape shaderType fieldLayout) (\binding -> ResolveUniformBuffer (resolveUniform binding accessor))
  pure (Expr.input ("uniform." <> name))

pushConstant :: forall shader stage env. (ShaderBlockValue shader, KnownNat (BufferAlignment (ShaderBlockFormat shader)), KnownNat (BufferSize (ShaderBlockFormat shader)), PushConstantSizeAllowed (ShaderBlockFormat shader)) => (env -> ShaderBlockFormat shader) -> PipelineM env (Expr stage shader)
pushConstant accessor = PipelineM $ do
  recorder <- get
  let interface = recordedInterface recorder
      ranges = pipelinePushConstants interface
      name = "push." <> show (length ranges)
      previousEnd = maximum (0 : [pushConstantOffset range + pushConstantSize range | range <- ranges])
      alignment = staticBufferAlignment (Proxy @(ShaderBlockFormat shader))
      offset = alignUp previousEnd alignment
      size = staticBufferSize (Proxy @(ShaderBlockFormat shader))
      end = offset + size
      shaderType = valueTy (Proxy @shader)
      fieldLayout = bufferFieldLayout (Proxy @(ShaderBlockFormat shader))
  when (end > 128) (throwError (PushConstantTotalSizeExceeded end))
  rejectBooleanBufferLayout name fieldLayout
  put
    recorder
      { recordedInterface = interface{pipelinePushConstants = ranges <> [PushConstantRange name offset size shaderType fieldLayout]}
      , recordedPushConstantResolvers = recordedPushConstantResolvers recorder <> [resolvePushConstant name offset size accessor]
      }
  pure (Expr.input name)

alignUp :: Int -> Int -> Int
alignUp value alignment = ((value + alignment - 1) `div` alignment) * alignment

resolvePushConstant :: forall block env. (BufferFormat block, HostFormat block ~ block) => String -> Int -> Int -> (env -> block) -> env -> IO ResolvedPushConstant
resolvePushConstant name offset size accessor environment = do
  bytes <- allocaBytes size $ \pointer -> do
    fillBytes pointer 0 size
    pokeBuffer (Proxy @block) (castPtr pointer) (accessor environment)
    ByteString.packCStringLen (castPtr pointer, size)
  pure (ResolvedPushConstant name offset bytes)

storageBuffer :: Storage env a -> PipelineM env (StorageRef 'Expr.Fragment a)
storageBuffer (Storage name accessor) = PipelineM $ do
  recordResource name StorageShape (\binding -> ResolveStorageBuffer (resolveStorage binding accessor))
  pure (StorageRef name)

texture :: Texture env -> PipelineM env (Sampler2D 'Expr.Fragment)
texture (Texture name accessor) = PipelineM $ do
  recordResource name (CombinedTextureShape Image2D) (\binding -> ResolveTexture (resolveTexture binding accessor))
  pure (Expr.sampler2D ("texture." <> name))

sampledTexture :: forall dim format env. (KnownSampleDimension dim) => SampledTexture env dim format -> PipelineM env (SampledImage dim format 'Expr.Fragment)
sampledTexture (SampledTexture name accessor) = PipelineM $ do
  recordResource name (CombinedTextureShape (sampleDimension (Proxy @dim))) (\binding -> ResolveTexture (resolveTypedTexture binding accessor))
  pure (Expr.sampledImage (Expr.imageResource ("texture." <> name <> ".image")) (Expr.sampler ("texture." <> name <> ".sampler")))

comparisonTexture :: forall dim env. (KnownSampleDimension dim) => ComparisonTexture env dim -> PipelineM env (ComparisonSampledImage dim 'Expr.Fragment)
comparisonTexture (ComparisonTexture name accessor) = PipelineM $ do
  recordResource name (CombinedTextureShape (sampleDimension (Proxy @dim))) (\binding -> ResolveTexture (resolveComparisonTexture binding accessor))
  pure (Expr.comparisonSampledImage (Expr.imageResource ("texture." <> name <> ".image")) (Expr.comparisonSampler ("texture." <> name <> ".sampler")))

{- | Require the clip-space position passed to @rasterize@ to be built in the
vertex stage, with a diagnostic that names the correct alias and operation.
-}
type family VertexStagePosition (stage :: Expr.Stage) :: Constraint where
  VertexStagePosition 'Expr.Vertex = ()
  VertexStagePosition stage =
    TypeError
      ( 'Text "rasterize requires a vertex-stage clip position."
          ':$$: 'Text "Build the position from vertexInput and give it type V (V4 Float)."
          ':$$: 'Text "Fragment values only exist after rasterize; the supplied position has stage "
          ':<>: 'ShowType stage
          ':<>: 'Text "."
      )

rasterize :: forall stage vertex fragment topology env. (KnownTopology topology, VertexStagePosition stage, FragmentInput vertex fragment) => Raster -> PrimitiveStream topology (Expr.Expr stage (V4 Float), vertex) -> PipelineM env (FragmentStream fragment)
rasterize raster = rasterizeWithIndex raster Nothing

{- | Records a direct indexed draw. The index buffer is a fixed-function input,
not a shader resource. Primitive restart remains disabled for this path.
-}
rasterizeIndexed :: forall stage vertex fragment topology env. (KnownTopology topology, VertexStagePosition stage, FragmentInput vertex fragment) => Raster -> IndexSource env -> PrimitiveStream topology (Expr.Expr stage (V4 Float), vertex) -> PipelineM env (FragmentStream fragment)
rasterizeIndexed raster (IndexSource name accessor) stream = PipelineM $ do
  recorder <- get
  put recorder{recordedResolvers = recordedResolvers recorder <> [ResolveIndexBuffer (resolveIndex name accessor)]}
  unPipelineM (rasterizeWithIndex raster (Just name) stream)

rasterizeWithIndex :: forall stage vertex fragment topology env. (KnownTopology topology, VertexStagePosition stage, FragmentInput vertex fragment) => Raster -> Maybe String -> PrimitiveStream topology (Expr.Expr stage (V4 Float), vertex) -> PipelineM env (FragmentStream fragment)
rasterizeWithIndex raster indexName' (PrimitiveStream (position, vertexValues)) = PipelineM $ do
  recorder <- get
  let FragmentInputs fragmentValues varyings _ = buildFragmentInputs 0 vertexValues
      identifier = nextDrawIdentifier recorder
      pendingDraw = PendingDraw identifier (topologyValue (Proxy @topology)) raster indexName' (SomeExpr position) varyings [] Nothing []
  put
    recorder
      { recordedDraws = recordedDraws recorder <> [pendingDraw]
      , nextDrawIdentifier = identifier + 1
      }
  pure (FragmentStream fragmentValues identifier [] Nothing)

drawColor :: forall format output env. (ColorRenderable format, Blendable format, KnownFormat format, ShaderValue output, ColorOutputMatches format output) => Blend -> ColorTarget env format -> FragmentStream (F output) -> PipelineM env ()
drawColor blend (ColorTarget name accessor) stream = PipelineM $ do
  let identifier = fragmentDrawIdentifier stream
  attachment <- recordColorTarget (ColorTarget name accessor)
  location <- do
    recorder <- get
    case find ((== identifier) . pendingDrawIdentifier) (recordedDraws recorder) of
      Nothing -> throwError (MissingDraw identifier)
      Just draw -> pure (maybe (length (pendingColorOutputs draw)) (\(PendingColorOutput _ location _ _) -> location) (find sameTarget (pendingColorOutputs draw)))
  updateDraw identifier $ \draw ->
    pure
      draw
        { pendingColorOutputs = upsertColorOutput attachment blend (SomeFragmentRoot (fragmentValue stream)) (pendingColorOutputs draw)
        , pendingDiscardPredicates = pendingDiscardPredicates draw <> fragmentDiscards stream
        }
  recorder <- get
  put recorder{recordedResolvers = recordedResolvers recorder <> [ResolveColorImage (resolveColor attachment identifier location accessor)]}
 where
  sameTarget (PendingColorOutput existing _ _ _) = colorAttachmentName existing == name

drawDepth :: forall format env. (DepthRenderable format, KnownFormat format) => Depth -> DepthTarget env format -> FragmentStream (F Float) -> PipelineM env ()
drawDepth depth target stream = PipelineM $ do
  attachment <- recordDepthTarget target
  updateDraw (fragmentDrawIdentifier stream) $ \draw -> do
    case pendingDepthOutput draw of
      Just (PendingDepthOutput existing _ _)
        | depthAttachmentName existing /= depthAttachmentName attachment ->
            throwError (MultipleDepthTargets (pendingDrawIdentifier draw) (depthAttachmentName existing) (depthAttachmentName attachment))
      _ ->
        pure
          draw
            { pendingDepthOutput = Just (PendingDepthOutput attachment depth (SomeFragmentRoot depthValue))
            , pendingDiscardPredicates = pendingDiscardPredicates draw <> fragmentDiscards stream
            }
 where
  depthValue = fromMaybe (fragmentValue stream) (fragmentDepthOverride stream)

compilePipeline :: PipelineM env () -> IO (Either PipelineError (CompiledPipeline env))
compilePipeline pipeline = case runStateT (unPipelineM pipeline) emptyRecorder of
  Left pipelineError -> pure (Left pipelineError)
  Right (_, recorder) -> do
    reifiedDraws <- traverse reifyDraw (filter drawHasOutputs (recordedDraws recorder))
    case do
      assembledDraws <- sequence reifiedDraws
      draws <- traverse (compileDraw (recordedInterface recorder)) assembledDraws
      Right
        ( CompiledPipeline
            (recordedInterface recorder)
            draws
            (BindingPlan (recordedResolvers recorder))
            (PushConstantPlan (recordedPushConstantResolvers recorder))
        ) of
      Left pipelineError -> pure (Left pipelineError)
      Right compiled -> do
        dumpCompiledPipeline compiled
        pure (Right compiled)

dumpCompiledPipeline :: CompiledPipeline env -> IO ()
dumpCompiledPipeline compiled =
  traverse_ dumpDraw (compiledPipelineDraws compiled)
 where
  interface = renderPipelineInterfaceTable (compiledPipelineInterface compiled)
  dumpDraw draw = do
    let name = "graphics-draw-" <> show (compiledDrawIdentifier draw)
    dumpCompiledModule (ShaderDump name DumpVertex (compiledVertexModule draw) interface)
    dumpCompiledModule (ShaderDump name DumpFragment (compiledFragmentModule draw) interface)

data ReifiedDraw = ReifiedDraw
  { reifiedDrawPending :: PendingDraw
  , reifiedDrawVertexForest :: ReifiedForest
  , reifiedDrawClipPositionRoot :: NodeId
  , reifiedDrawVaryings :: [CompiledVarying]
  , reifiedDrawFragmentForest :: ReifiedForest
  , reifiedDrawColorOutputs :: [CompiledColorOutput]
  , reifiedDrawDepthOutput :: Maybe CompiledDepthOutput
  , reifiedDrawDiscardPredicates :: [NodeId]
  }

reifyDraw :: PendingDraw -> IO (Either PipelineError ReifiedDraw)
reifyDraw draw = do
  vertexForest <- reifyExprForest (pendingClipPosition draw : fmap pendingVertexExpression (pendingVaryings draw))
  fragmentForest <- reifyExprForest (fragmentExpressions draw)
  pure (assembleDraw draw vertexForest fragmentForest)

assembleDraw :: PendingDraw -> ReifiedForest -> ReifiedForest -> Either PipelineError ReifiedDraw
assembleDraw draw vertexForest fragmentForest = do
  (clipPosition, vertexVaryingRoots) <- splitVertexRoots (length (pendingVaryings draw)) (forestRoots vertexForest)
  (fragmentVaryingRoots, colorRoots, depthRoot, discardRoots) <- splitFragmentRoots draw (forestRoots fragmentForest)
  let varyings = zipWith3 compileVarying (pendingVaryings draw) vertexVaryingRoots fragmentVaryingRoots
      colors = zipWith compileColorOutput (pendingColorOutputs draw) colorRoots
      depth = compileDepthOutput <$> pendingDepthOutput draw <*> depthRoot
  pure
    ReifiedDraw
      { reifiedDrawPending = draw
      , reifiedDrawVertexForest = vertexForest
      , reifiedDrawClipPositionRoot = clipPosition
      , reifiedDrawVaryings = varyings
      , reifiedDrawFragmentForest = fragmentForest
      , reifiedDrawColorOutputs = colors
      , reifiedDrawDepthOutput = depth
      , reifiedDrawDiscardPredicates = discardRoots
      }

compileDraw :: PipelineInterface -> ReifiedDraw -> Either PipelineError CompiledDraw
compileDraw interface draw = do
  attributes <- activeVertexAttributes interface draw
  validateLocations draw Codegen.VertexShader "vertex attributes" (fmap vertexAttributeLocation attributes)
  validateDenseLocations draw Codegen.FragmentShader "varyings" (fmap compiledVaryingLocation (reifiedDrawVaryings draw))
  validateDenseLocations draw Codegen.FragmentShader "color outputs" (fmap compiledColorLocation (reifiedDrawColorOutputs draw))
  bindings <- activeVertexBindings draw interface attributes
  resources <- lowerResources interface draw
  vertexModule <- compileShader draw Codegen.VertexShader (vertexShader attributes resources draw)
  fragment <- fragmentShader resources draw
  fragmentModule <- compileShader draw Codegen.FragmentShader fragment
  let pending = reifiedDrawPending draw
  pure
    CompiledDraw
      { compiledDrawTopology = pendingDrawTopology pending
      , compiledDrawRaster = pendingDrawRaster pending
      , compiledDrawIdentifier = pendingDrawIdentifier pending
      , compiledDrawIndexSource = pendingDrawIndexSource pending
      , compiledVertexModule = vertexModule
      , compiledFragmentModule = fragmentModule
      , compiledVertexBindings = bindings
      , compiledVertexAttributes = attributes
      , compiledVertexForest = reifiedDrawVertexForest draw
      , compiledClipPositionRoot = reifiedDrawClipPositionRoot draw
      , compiledVaryings = reifiedDrawVaryings draw
      , compiledFragmentForest = reifiedDrawFragmentForest draw
      , compiledColorOutputs = reifiedDrawColorOutputs draw
      , compiledDepthOutput = reifiedDrawDepthOutput draw
      , compiledDiscardPredicates = reifiedDrawDiscardPredicates draw
      }

compileShader :: ReifiedDraw -> Codegen.ShaderStage -> Codegen.ShaderModule -> Either PipelineError SpirVModule
compileShader draw stage shader =
  case Codegen.compileShaderModule shader of
    Left error' -> Left (graphicsShaderError draw stage (show error'))
    Right spirV -> Right spirV

activeVertexAttributes :: PipelineInterface -> ReifiedDraw -> Either PipelineError [VertexAttribute]
activeVertexAttributes interface draw = do
  let inputs = inputSymbols (reifiedDrawVertexForest draw)
      attributes = filter (\attribute -> ("vertex." <> vertexAttributeName attribute) `elem` inputs) (pipelineVertexAttributes interface)
  traverse_ ensureAttributeInput attributes
  pure attributes
 where
  ensureAttributeInput attribute =
    let symbol = "vertex." <> vertexAttributeName attribute
     in if symbol `elem` inputSymbols (reifiedDrawVertexForest draw)
          then Right ()
          else Left (graphicsShaderError draw Codegen.VertexShader ("active attribute " <> show symbol <> " has no vertex input"))

activeVertexBindings :: ReifiedDraw -> PipelineInterface -> [VertexAttribute] -> Either PipelineError [VertexBindingLayout]
activeVertexBindings draw interface attributes = traverse bindingFor (nub (fmap vertexAttributeSourceName attributes))
 where
  bindingFor sourceName = case find ((== sourceName) . vertexBindingSourceName) (pipelineVertexBindings interface) of
    Nothing -> Left (graphicsShaderError draw Codegen.VertexShader ("active vertex source " <> show sourceName <> " has no binding layout"))
    Just binding -> Right binding

lowerResources :: PipelineInterface -> ReifiedDraw -> Either PipelineError [Codegen.ResourceDeclaration]
lowerResources interface draw = do
  rejectReferencedStorage
  pure (uniformResources <> textureResources <> pushConstantResources)
 where
  uniformResources =
    [ Codegen.UniformBlockResource
        Codegen.UniformBlockDeclaration
          { Codegen.uniformBlockName = resourceBindingName binding
          , Codegen.uniformBlockLocation = descriptorLocation binding
          , Codegen.uniformBlockStandard = Std140
          , Codegen.uniformBlockLayout = Struct [fieldLayout]
          , Codegen.uniformBlockLeaves = [Codegen.UniformLeaf ("uniform." <> resourceBindingName binding) [0] shaderType]
          }
    | binding <- pipelineResources interface
    , UniformShape shaderType fieldLayout <- [resourceBindingShape binding]
    ]
  textureResources =
    [ Codegen.CombinedImageSamplerResource
        Codegen.CombinedImageSamplerDeclaration
          { Codegen.combinedDescriptorName = resourceBindingName binding
          , Codegen.combinedImageSymbol = "texture." <> resourceBindingName binding <> ".image"
          , Codegen.combinedSamplerSymbol = "texture." <> resourceBindingName binding <> ".sampler"
          , Codegen.combinedDescriptorLocation = descriptorLocation binding
          , Codegen.combinedImageDimension = dimension
          }
    | binding <- pipelineResources interface
    , CombinedTextureShape dimension <- [resourceBindingShape binding]
    ]
  pushConstantResources = case pipelinePushConstants interface of
    [] -> []
    ranges ->
      [ Codegen.PushConstantResource
          Codegen.PushConstantDeclaration
            { Codegen.pushConstantBlockName = "pushConstants"
            , Codegen.pushConstantLayout = Struct (fmap pushConstantFieldLayout ranges)
            , Codegen.pushConstantLeaves = zipWith pushLeaf [0 ..] ranges
            }
      ]
  pushLeaf index range = Codegen.UniformLeaf (pushConstantName range) [index] (pushConstantShaderType range)
  descriptorLocation binding = Codegen.DescriptorLocation (fromIntegral (resourceBindingSet binding)) (fromIntegral (resourceBindingBinding binding))
  referenced = resourceSymbols (reifiedDrawVertexForest draw) <> resourceSymbols (reifiedDrawFragmentForest draw)
  rejectReferencedStorage = traverse_ rejectStorage [binding | binding <- pipelineResources interface, StorageShape <- [resourceBindingShape binding]]
  rejectStorage binding
    | any (`elem` storageSymbols binding) referenced = Left (graphicsShaderError draw Codegen.FragmentShader ("storage resource " <> show (resourceBindingName binding) <> " is referenced, but storage operations are not supported"))
    | otherwise = Right ()
  storageSymbols binding = [resourceBindingName binding, "storage." <> resourceBindingName binding]

vertexShader :: [VertexAttribute] -> [Codegen.ResourceDeclaration] -> ReifiedDraw -> Codegen.ShaderModule
vertexShader attributes resources draw =
  Codegen.ShaderModule
    { Codegen.shaderCodegenConfig = Codegen.defaultCodegenConfig
    , Codegen.shaderStage = Codegen.VertexShader
    , Codegen.shaderEntryPoint = "main"
    , Codegen.shaderLocalSize = Nothing
    , Codegen.shaderInputs = fmap attributeInput attributes
    , Codegen.shaderOutputs = positionOutput : fmap varyingOutput (reifiedDrawVaryings draw)
    , Codegen.shaderResources = resources
    , Codegen.shaderForest = reifiedDrawVertexForest draw
    , Codegen.shaderActions = Codegen.StoreOutput "position" (reifiedDrawClipPositionRoot draw) : fmap varyingAction (reifiedDrawVaryings draw)
    }
 where
  attributeInput attribute = Codegen.StageInput ("vertex." <> vertexAttributeName attribute) (expressionType (vertexAttributeShaderType attribute)) (Codegen.Location (intLocation (vertexAttributeLocation attribute)) Codegen.Smooth)
  positionOutput = Codegen.StageOutput "position" (expressionType vector4Type) (Codegen.BuiltIn Codegen.Position)
  varyingOutput varying = Codegen.StageOutput (compiledVaryingName varying) (expressionType (compiledVaryingShaderType varying)) (Codegen.Location (intLocation (compiledVaryingLocation varying)) (codegenInterpolation (compiledVaryingInterpolation varying)))
  varyingAction varying = Codegen.StoreOutput (compiledVaryingName varying) (compiledVertexVaryingRoot varying)

fragmentShader :: [Codegen.ResourceDeclaration] -> ReifiedDraw -> Either PipelineError Codegen.ShaderModule
fragmentShader resources draw = do
  colors <- traverse colorOutput (reifiedDrawColorOutputs draw)
  depth <- traverse depthOutput (reifiedDrawDepthOutput draw)
  pure
    Codegen.ShaderModule
      { Codegen.shaderCodegenConfig = Codegen.defaultCodegenConfig
      , Codegen.shaderStage = Codegen.FragmentShader
      , Codegen.shaderEntryPoint = "main"
      , Codegen.shaderLocalSize = Nothing
      , Codegen.shaderInputs = fmap varyingInput (reifiedDrawVaryings draw)
      , Codegen.shaderOutputs = colors <> maybe [] pure depth
      , Codegen.shaderResources = resources
      , Codegen.shaderForest = fragmentActionForest draw
      , Codegen.shaderActions = fmap colorAction (reifiedDrawColorOutputs draw) <> maybe [] (pure . depthAction) (reifiedDrawDepthOutput draw) <> fmap Codegen.DiscardWhen (reifiedDrawDiscardPredicates draw)
      }
 where
  varyingInput varying = Codegen.StageInput (compiledVaryingName varying) (expressionType (compiledVaryingShaderType varying)) (Codegen.Location (intLocation (compiledVaryingLocation varying)) (codegenInterpolation (compiledVaryingInterpolation varying)))
  colorOutput output = do
    shaderType <- nodeType draw Codegen.FragmentShader (reifiedDrawFragmentForest draw) (compiledColorRoot output)
    pure (Codegen.StageOutput (colorSymbol output) (expressionType shaderType) (Codegen.Location (intLocation (compiledColorLocation output)) Codegen.Smooth))
  depthOutput output = do
    shaderType <- nodeType draw Codegen.FragmentShader (reifiedDrawFragmentForest draw) (compiledDepthRoot output)
    pure (Codegen.StageOutput "depth" (expressionType shaderType) (Codegen.BuiltIn Codegen.FragDepth))
  colorAction output = Codegen.StoreOutput (colorSymbol output) (compiledColorRoot output)
  depthAction output = Codegen.StoreOutput "depth" (compiledDepthRoot output)
  colorSymbol output = "color." <> show (compiledColorLocation output)

fragmentActionForest :: ReifiedDraw -> ReifiedForest
fragmentActionForest draw =
  (reifiedDrawFragmentForest draw)
    { forestRoots = fmap compiledColorRoot (reifiedDrawColorOutputs draw) <> maybe [] (pure . compiledDepthRoot) (reifiedDrawDepthOutput draw) <> reifiedDrawDiscardPredicates draw
    }

inputSymbols :: ReifiedForest -> [String]
inputSymbols forest = [symbol | ReifiedNode _ _ (RInput symbol) <- forestNodes forest]

resourceSymbols :: ReifiedForest -> [String]
resourceSymbols forest = [symbol | ReifiedNode _ _ (RResource symbol) <- forestNodes forest]

nodeType :: ReifiedDraw -> Codegen.ShaderStage -> ReifiedForest -> NodeId -> Either PipelineError ShaderTy
nodeType draw stage forest root = case find ((== root) . reifiedId) (forestNodes forest) of
  Just node -> Right (reifiedTy node)
  Nothing -> Left (graphicsShaderError draw stage ("compiled draw refers to missing node " <> show root))

expressionType :: ShaderTy -> Codegen.InterfaceValueType
expressionType = Codegen.ExpressionType

vector4Type :: ShaderTy
vector4Type = TyVector 4

codegenInterpolation :: Interpolation -> Codegen.Interpolation
codegenInterpolation value = case value of
  SmoothInterpolation -> Codegen.Smooth
  FlatInterpolation -> Codegen.Flat
  NoPerspectiveInterpolation -> Codegen.NoPerspective

intLocation :: Int -> Word32
intLocation = fromIntegral

validateLocations :: ReifiedDraw -> Codegen.ShaderStage -> String -> [Int] -> Either PipelineError ()
validateLocations draw stage label locations
  | any (< 0) locations = Left (graphicsShaderError draw stage (label <> " contain a negative location: " <> show locations))
  | length locations /= length (nub locations) = Left (graphicsShaderError draw stage (label <> " contain duplicate locations: " <> show locations))
  | otherwise = Right ()

validateDenseLocations :: ReifiedDraw -> Codegen.ShaderStage -> String -> [Int] -> Either PipelineError ()
validateDenseLocations draw stage label locations = do
  validateLocations draw stage label locations
  if sort locations == [0 .. length locations - 1]
    then Right ()
    else Left (graphicsShaderError draw stage (label <> " must be dense from zero: " <> show locations))

graphicsShaderError :: ReifiedDraw -> Codegen.ShaderStage -> String -> PipelineError
graphicsShaderError draw = GraphicsShaderCompilationFailed (pendingDrawIdentifier (reifiedDrawPending draw))

splitVertexRoots :: Int -> [NodeId] -> Either PipelineError (NodeId, [NodeId])
splitVertexRoots varyingCount roots = case roots of
  clipPosition : varyingRoots
    | length varyingRoots == varyingCount -> Right (clipPosition, varyingRoots)
  _ -> Left (ReificationRootMismatch "vertex stage" (varyingCount + 1) (length roots))

splitFragmentRoots :: PendingDraw -> [NodeId] -> Either PipelineError ([NodeId], [NodeId], Maybe NodeId, [NodeId])
splitFragmentRoots draw roots
  | length roots /= expectedCount = Left (ReificationRootMismatch "fragment stage" expectedCount (length roots))
  | otherwise =
      let (varyingRoots, afterVaryings) = splitAt varyingCount roots
          (colorRoots, afterColors) = splitAt colorCount afterVaryings
          (depthRoots, discardRoots) = splitAt depthCount afterColors
       in Right (varyingRoots, colorRoots, singleDepthRoot depthRoots, discardRoots)
 where
  varyingCount = length (pendingVaryings draw)
  colorCount = length (pendingColorOutputs draw)
  depthCount = maybe 0 (const 1) (pendingDepthOutput draw)
  expectedCount = varyingCount + colorCount + depthCount + length (pendingDiscardPredicates draw)

singleDepthRoot :: [NodeId] -> Maybe NodeId
singleDepthRoot roots = case roots of
  [root] -> Just root
  _ -> Nothing

compileVarying :: PendingVarying -> NodeId -> NodeId -> CompiledVarying
compileVarying (PendingVarying location interpolation name shaderType _ _) = CompiledVarying location interpolation name shaderType

compileColorOutput :: PendingColorOutput -> NodeId -> CompiledColorOutput
compileColorOutput (PendingColorOutput attachment location blend _) =
  CompiledColorOutput
    (colorAttachmentName attachment)
    location
    (colorAttachmentFormat attachment)
    blend

compileDepthOutput :: PendingDepthOutput -> NodeId -> CompiledDepthOutput
compileDepthOutput (PendingDepthOutput attachment depth _) =
  CompiledDepthOutput
    (depthAttachmentName attachment)
    (depthAttachmentFormat attachment)
    depth

pendingVertexExpression :: PendingVarying -> SomeExpr
pendingVertexExpression (PendingVarying _ _ _ _ expression _) = SomeExpr expression

pendingFragmentExpression :: PendingVarying -> SomeExpr
pendingFragmentExpression (PendingVarying _ _ _ _ _ expression) = SomeExpr expression

pendingColorExpression :: PendingColorOutput -> SomeExpr
pendingColorExpression (PendingColorOutput _ _ _ (SomeFragmentRoot expression)) = SomeExpr expression

pendingDepthExpression :: PendingDepthOutput -> SomeExpr
pendingDepthExpression (PendingDepthOutput _ _ (SomeFragmentRoot expression)) = SomeExpr expression

fragmentExpressions :: PendingDraw -> [SomeExpr]
fragmentExpressions draw =
  fmap pendingFragmentExpression (pendingVaryings draw)
    <> fmap pendingColorExpression (pendingColorOutputs draw)
    <> maybe [] (pure . pendingDepthExpression) (pendingDepthOutput draw)
    <> fmap SomeExpr (pendingDiscardPredicates draw)

drawHasOutputs :: PendingDraw -> Bool
drawHasOutputs draw = not (null (pendingColorOutputs draw)) || isJust (pendingDepthOutput draw)

upsertColorOutput :: ColorAttachment -> Blend -> SomeFragmentRoot -> [PendingColorOutput] -> [PendingColorOutput]
upsertColorOutput attachment blend root outputs =
  case break (sameTarget attachment) outputs of
    (before, PendingColorOutput _ location _ _ : after) -> before <> [PendingColorOutput attachment location blend root] <> after
    _ -> outputs <> [PendingColorOutput attachment (length outputs) blend root]
 where
  sameTarget requested (PendingColorOutput existing _ _ _) = colorAttachmentName requested == colorAttachmentName existing

updateDraw :: Int -> (PendingDraw -> StateT (Recorder env) (Either PipelineError) PendingDraw) -> StateT (Recorder env) (Either PipelineError) ()
updateDraw identifier transform = do
  recorder <- get
  case break ((== identifier) . pendingDrawIdentifier) (recordedDraws recorder) of
    (_, []) -> throwError (MissingDraw identifier)
    (before, draw : after) -> do
      updated <- transform draw
      put recorder{recordedDraws = before <> [updated] <> after}

recordResource :: String -> ShaderResourceShape -> (ResourceBinding -> EnvironmentResolver env) -> StateT (Recorder env) (Either PipelineError) ()
recordResource name shape makeResolver = do
  recorder <- get
  let interface = recordedInterface recorder
      resources = pipelineResources interface
  case find ((== name) . resourceBindingName) resources of
    Just existing
      | resourceBindingShape existing == shape ->
          put recorder{recordedResolvers = recordedResolvers recorder <> [makeResolver existing]}
      | otherwise ->
          throwError
            ( ResourceConflict
                name
                (resourceBindingKind existing)
                (resourceBindingShaderType existing)
                (resourceBindingKind (ResourceBinding name 0 0 shape))
                (resourceBindingShaderType (ResourceBinding name 0 0 shape))
            )
    Nothing -> do
      let binding = ResourceBinding name 0 (length resources) shape
      put
        recorder
          { recordedInterface = interface{pipelineResources = resources <> [binding]}
          , recordedResolvers = recordedResolvers recorder <> [makeResolver binding]
          }

rejectBooleanBufferLayout :: String -> FieldLayout -> StateT (Recorder env) (Either PipelineError) ()
rejectBooleanBufferLayout name fieldLayout =
  when (containsBoolean fieldLayout) (throwError (UnsupportedBooleanBufferLayout name fieldLayout))

containsBoolean :: FieldLayout -> Bool
containsBoolean fieldLayout = case fieldLayout of
  Scalar Boolean32 -> True
  Vector _ Boolean32 -> True
  Matrix _ _ Boolean32 -> True
  Array _ element -> containsBoolean element
  Struct fields -> any containsBoolean fields
  _ -> False

vertexLeafOffsets :: FieldLayout -> [Int]
vertexLeafOffsets = go 0
 where
  go offset fieldLayout = case fieldLayout of
    Scalar{} -> [offset]
    Vector{} -> [offset]
    Struct fields ->
      concat
        ( zipWith
            (\fieldOffset -> go (offset + fieldOffset))
            (layoutFieldOffsets (layoutOf Vertex (Struct fields)))
            fields
        )
    Array{} -> []
    Matrix{} -> []

recordColorTarget :: forall format env. (KnownFormat format) => ColorTarget env format -> StateT (Recorder env) (Either PipelineError) ColorAttachment
recordColorTarget (ColorTarget name _accessor) = do
  recorder <- get
  let interface = recordedInterface recorder
      colors = pipelineColorAttachments interface
      requestedFormat = formatVal @format
  case find ((== name) . depthAttachmentName) (pipelineDepthAttachments interface) of
    Just _ -> throwError (TargetKindConflict name)
    Nothing -> case find ((== name) . colorAttachmentName) colors of
      Just existing
        | colorAttachmentFormat existing == requestedFormat -> do
            pure existing
        | otherwise -> throwError (ColorTargetConflict name (colorAttachmentFormat existing) requestedFormat)
      Nothing -> do
        let attachment = ColorAttachment name (length colors) requestedFormat
        put
          recorder
            { recordedInterface = interface{pipelineColorAttachments = colors <> [attachment]}
            }
        pure attachment

recordDepthTarget :: forall format env. (KnownFormat format) => DepthTarget env format -> StateT (Recorder env) (Either PipelineError) DepthAttachment
recordDepthTarget (DepthTarget name accessor) = do
  recorder <- get
  let interface = recordedInterface recorder
      depths = pipelineDepthAttachments interface
      requestedFormat = formatVal @format
  case find ((== name) . colorAttachmentName) (pipelineColorAttachments interface) of
    Just _ -> throwError (TargetKindConflict name)
    Nothing -> case find ((== name) . depthAttachmentName) depths of
      Just existing
        | depthAttachmentFormat existing == requestedFormat -> do
            put recorder{recordedResolvers = recordedResolvers recorder <> [ResolveDepthImage (resolveDepth name accessor)]}
            pure existing
        | otherwise -> throwError (DepthTargetConflict name (depthAttachmentFormat existing) requestedFormat)
      Nothing -> do
        let attachment = DepthAttachment name requestedFormat
        put
          recorder
            { recordedInterface = interface{pipelineDepthAttachments = depths <> [attachment]}
            , recordedResolvers = recordedResolvers recorder <> [ResolveDepthImage (resolveDepth name accessor)]
            }
        pure attachment

resolveVertex :: String -> [Int] -> (env -> VertexBuffer a) -> env -> ResolvedVertexBuffer
resolveVertex name locations accessor environment = ResolvedVertexBuffer name locations (vertexBufferHandle (accessor environment))

resolveIndex :: String -> (env -> IndexBuffer) -> env -> ResolvedIndexBuffer
resolveIndex name accessor environment = ResolvedIndexBuffer name (indexBufferHandle (accessor environment))

resolveUniform :: ResourceBinding -> (env -> UniformBuffer a) -> env -> ResolvedUniformBuffer
resolveUniform binding accessor environment =
  ResolvedUniformBuffer
    (resourceBindingName binding)
    (resourceBindingSet binding)
    (resourceBindingBinding binding)
    (uniformBufferHandle (accessor environment))

resolveStorage :: ResourceBinding -> (env -> StorageBuffer a) -> env -> ResolvedStorageBuffer
resolveStorage binding accessor environment =
  ResolvedStorageBuffer
    (resourceBindingName binding)
    (resourceBindingSet binding)
    (resourceBindingBinding binding)
    (storageBufferHandle (accessor environment))

resolveTexture :: ResourceBinding -> (env -> TextureBinding) -> env -> ResolvedTexture
resolveTexture metadata accessor environment =
  let binding = accessor environment
   in ResolvedTexture
        (resourceBindingName metadata)
        (resourceBindingSet metadata)
        (resourceBindingBinding metadata)
        (textureImageHandle binding)
        (textureSamplerHandle binding)

resolveTypedTexture :: ResourceBinding -> (env -> TypedTextureBinding dim format) -> env -> ResolvedTexture
resolveTypedTexture metadata accessor environment =
  let binding = accessor environment
   in ResolvedTexture
        (resourceBindingName metadata)
        (resourceBindingSet metadata)
        (resourceBindingBinding metadata)
        (typedTextureImageHandle binding)
        (typedTextureSamplerHandle binding)

resolveComparisonTexture :: ResourceBinding -> (env -> ComparisonTextureBinding dim) -> env -> ResolvedTexture
resolveComparisonTexture metadata accessor environment =
  let binding = accessor environment
   in ResolvedTexture
        (resourceBindingName metadata)
        (resourceBindingSet metadata)
        (resourceBindingBinding metadata)
        (comparisonTextureImageHandle binding)
        (comparisonTextureSamplerHandle binding)

resolveColor :: ColorAttachment -> Int -> Int -> (env -> ColorImage format) -> env -> ResolvedColorImage
resolveColor attachment drawIdentifier location accessor environment =
  ResolvedColorImage
    (colorAttachmentName attachment)
    drawIdentifier
    location
    (colorImageHandle (accessor environment))

resolveDepth :: String -> (env -> DepthImage format) -> env -> ResolvedDepthImage
resolveDepth name accessor environment = ResolvedDepthImage name (depthImageHandle (accessor environment))

varyingName :: Int -> String
varyingName location = "varying." <> show location
