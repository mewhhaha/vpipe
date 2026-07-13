{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Typed shader expressions.

Instance coverage:

[@Num@] @Float@, @Int32@, @Word32@, @V2/V3/V4 Float@, and every @M22..M44 Float@.

[@Fractional/Floating@] @Float@ and @V2/V3/V4 Float@.

['EqE'] scalars, float and unsigned vectors, and float matrices.

['OrdE'] @Float@ and @Int32@ only.

['FloatVector'] @V2/V3/V4 Float@; 'cross' is intentionally restricted to
@V3 Float@. Matrix products are restricted by 'MatrixProduct' and
'MatrixVectorProduct', so incompatible dimensions do not typecheck.

['Swizzle'] @V2/V3/V4 Float@. '_x' and '_xy' are statically unavailable on
scalars and matrices; @x@ and @xy@ are compatibility aliases.

'whileE' is the only v1 structured loop. A structured @forE@ over
@Expr s Int32@ ranges is the planned v1.1 escape hatch; general mutable
variables are intentionally not part of the expression language.

@discard@ is intentionally absent: discarding is a fragment-stream
statement owned by @Vpipe.Shader@ (task 05), not a value-producing @Expr@.

Expressions are pure descriptions; build one by naming an input and applying
the usual typed operators:

@
module Main (main) where

import Vpipe.Expr (Expr, constant, input)

bright :: Expr s Float
bright = input "exposure" * constant 2

main :: IO ()
main = pure ()
@
-}
module Vpipe.Expr (
  Stage (..),
  Expr,
  V,
  F,
  C,
  BoolE,
  ShaderValue,
  ShaderArithmetic,
  ShaderFloat,
  DerivativeStage,
  constant,
  input,
  EqE,
  (==.),
  (/=.),
  OrdE,
  (<.),
  (<=.),
  (>.),
  (>=.),
  ifE,
  ifThenElseE,
  whileE,
  vec2,
  vec3,
  vec4,
  x,
  HasY (y),
  HasZ (z),
  HasW (w),
  xy,
  WordX (wordX),
  WordY (wordY),
  WordZ (wordZ),
  WordW (wordW),
  Swizzle (..),
  clamp,
  mix,
  smoothstep,
  FloatVector,
  normalize,
  dot,
  cross,
  reflect,
  MatrixValue,
  MatrixProduct,
  MatrixVectorProduct,
  matrixMultiply,
  matrixVectorMultiply,
  (!*!),
  (!*),
  dFdx,
  dFdy,
  fwidth,
  ImageResource,
  Image2D,
  Sampler,
  ComparisonSampler,
  SampledImage,
  ComparisonSampledImage,
  Sampler2D,
  SampledFormat,
  KnownSampleDimension,
  SampleCoordinates,
  sampleDimension,
  imageResource,
  image2D,
  sampler,
  comparisonSampler,
  sampledImage,
  comparisonSampledImage,
  sampledImage2D,
  sampler2D,
  sample,
  sampleLod,
  sampleCompare,
  sampleCompareLod,
) where

import Data.Int (Int32)
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Linear (
  M22,
  M23,
  M24,
  M32,
  M33,
  M34,
  M42,
  M43,
  M44,
  V2 (..),
  V3 (..),
  V4 (..),
 )
import Vpipe.Expr.Internal (Expr, ShaderArithmetic, ShaderFloat, ShaderValue, Stage (..))
import Vpipe.Expr.Internal qualified as I
import Vpipe.Format (Format (..), TexelOf)
import Vpipe.Image.Types (Dim (..))

type V a = Expr 'Vertex a
type F a = Expr 'Fragment a
type C a = Expr 'Compute a
type BoolE s = Expr s Bool

constant :: (ShaderValue a) => a -> Expr s a
constant = I.literal

input :: (ShaderValue a) => String -> Expr s a
input = I.input

class (ShaderValue a) => EqE a
instance EqE Float
instance EqE Int32
instance EqE Word32
instance EqE Bool
instance EqE (V2 Float)
instance EqE (V3 Float)
instance EqE (V4 Float)
instance EqE (V2 Word32)
instance EqE (V3 Word32)
instance EqE (V4 Word32)
instance EqE (M22 Float)
instance EqE (M23 Float)
instance EqE (M24 Float)
instance EqE (M32 Float)
instance EqE (M33 Float)
instance EqE (M34 Float)
instance EqE (M42 Float)
instance EqE (M43 Float)
instance EqE (M44 Float)

(==.) :: (EqE a) => Expr s a -> Expr s a -> BoolE s
(==.) = I.compareExpr I.EqualE

(/=.) :: (EqE a) => Expr s a -> Expr s a -> BoolE s
(/=.) = I.compareExpr I.NotEqualE

infix 4 ==., /=.

class (EqE a) => OrdE a
instance OrdE Float
instance OrdE Int32
instance OrdE Word32

(<.) :: (OrdE a) => Expr s a -> Expr s a -> BoolE s
(<.) = I.compareExpr I.LessE

(<=.) :: (OrdE a) => Expr s a -> Expr s a -> BoolE s
(<=.) = I.compareExpr I.LessEqualE

(>.) :: (OrdE a) => Expr s a -> Expr s a -> BoolE s
(>.) = I.compareExpr I.GreaterE

(>=.) :: (OrdE a) => Expr s a -> Expr s a -> BoolE s
(>=.) = I.compareExpr I.GreaterEqualE

infix 4 <., <=., >., >=.

ifE :: BoolE s -> Expr s a -> Expr s a -> Expr s a
ifE = I.select

ifThenElseE :: BoolE s -> Expr s a -> Expr s a -> Expr s a
ifThenElseE = I.branch

whileE :: (ShaderValue a) => (Expr s a -> BoolE s) -> (Expr s a -> Expr s a) -> Expr s a -> Expr s a
whileE = I.while

vec2 :: Expr s Float -> Expr s Float -> Expr s (V2 Float)
vec2 first second = I.constructVector [first, second]

vec3 :: Expr s Float -> Expr s Float -> Expr s Float -> Expr s (V3 Float)
vec3 first second third = I.constructVector [first, second, third]

vec4 :: Expr s Float -> Expr s Float -> Expr s Float -> Expr s Float -> Expr s (V4 Float)
vec4 first second third fourth = I.constructVector [first, second, third, fourth]

-- | Dimension-safe scalar and prefix swizzles for float vectors.
class (ShaderValue a) => Swizzle a where
  _x :: Expr s a -> Expr s Float
  _xy :: Expr s a -> Expr s (V2 Float)

class (ShaderValue a) => HasY a where y :: Expr s a -> Expr s Float
class (ShaderValue a) => HasZ a where z :: Expr s a -> Expr s Float
class (ShaderValue a) => HasW a where w :: Expr s a -> Expr s Float

instance Swizzle (V2 Float) where
  _x = I.extract I.TyFloat [0]
  _xy = I.extract (I.TyVector 2) [0, 1]
instance Swizzle (V3 Float) where
  _x = I.extract I.TyFloat [0]
  _xy = I.extract (I.TyVector 2) [0, 1]
instance Swizzle (V4 Float) where
  _x = I.extract I.TyFloat [0]
  _xy = I.extract (I.TyVector 2) [0, 1]
instance HasY (V2 Float) where y = I.extract I.TyFloat [1]
instance HasY (V3 Float) where y = I.extract I.TyFloat [1]
instance HasY (V4 Float) where y = I.extract I.TyFloat [1]
instance HasZ (V3 Float) where z = I.extract I.TyFloat [2]
instance HasZ (V4 Float) where z = I.extract I.TyFloat [2]
instance HasW (V4 Float) where w = I.extract I.TyFloat [3]

x :: (Swizzle a) => Expr s a -> Expr s Float
x = _x

xy :: (Swizzle a) => Expr s a -> Expr s (V2 Float)
xy = _xy

class WordX vector where wordX :: Expr s vector -> Expr s Word32
class WordY vector where wordY :: Expr s vector -> Expr s Word32
class WordZ vector where wordZ :: Expr s vector -> Expr s Word32
class WordW vector where wordW :: Expr s vector -> Expr s Word32

instance WordX (V2 Word32) where wordX = I.extract I.TyWord [0]
instance WordX (V3 Word32) where wordX = I.extract I.TyWord [0]
instance WordX (V4 Word32) where wordX = I.extract I.TyWord [0]
instance WordY (V2 Word32) where wordY = I.extract I.TyWord [1]
instance WordY (V3 Word32) where wordY = I.extract I.TyWord [1]
instance WordY (V4 Word32) where wordY = I.extract I.TyWord [1]
instance WordZ (V3 Word32) where wordZ = I.extract I.TyWord [2]
instance WordZ (V4 Word32) where wordZ = I.extract I.TyWord [2]
instance WordW (V4 Word32) where wordW = I.extract I.TyWord [3]

clamp :: (ShaderFloat a) => Expr s a -> Expr s a -> Expr s a -> Expr s a
clamp value lower = I.numericBinary I.MinE (I.numericBinary I.MaxE value lower)

mix :: (ShaderFloat a) => Expr s a -> Expr s a -> Expr s Float -> Expr s a
mix = I.mixExpr

smoothstep :: (ShaderFloat a) => Expr s a -> Expr s a -> Expr s a -> Expr s a
smoothstep = I.smoothstepExpr

class (ShaderFloat a) => FloatVector a
instance FloatVector (V2 Float)
instance FloatVector (V3 Float)
instance FloatVector (V4 Float)

normalize :: (FloatVector a) => Expr s a -> Expr s a
normalize = I.normalizeExpr

dot :: (FloatVector a) => Expr s a -> Expr s a -> Expr s Float
dot = I.dotExpr

cross :: Expr s (V3 Float) -> Expr s (V3 Float) -> Expr s (V3 Float)
cross = I.crossExpr

reflect :: (FloatVector a) => Expr s a -> Expr s a -> Expr s a
reflect = I.reflectExpr

class (ShaderValue matrix) => MatrixValue matrix
instance MatrixValue (M22 Float)
instance MatrixValue (M23 Float)
instance MatrixValue (M24 Float)
instance MatrixValue (M32 Float)
instance MatrixValue (M33 Float)
instance MatrixValue (M34 Float)
instance MatrixValue (M42 Float)
instance MatrixValue (M43 Float)
instance MatrixValue (M44 Float)

class (MatrixValue left, MatrixValue right, MatrixValue result) => MatrixProduct left right result | left right -> result

instance MatrixProduct (M22 Float) (M22 Float) (M22 Float)
instance MatrixProduct (M22 Float) (M23 Float) (M23 Float)
instance MatrixProduct (M22 Float) (M24 Float) (M24 Float)
instance MatrixProduct (M23 Float) (M32 Float) (M22 Float)
instance MatrixProduct (M23 Float) (M33 Float) (M23 Float)
instance MatrixProduct (M23 Float) (M34 Float) (M24 Float)
instance MatrixProduct (M24 Float) (M42 Float) (M22 Float)
instance MatrixProduct (M24 Float) (M43 Float) (M23 Float)
instance MatrixProduct (M24 Float) (M44 Float) (M24 Float)
instance MatrixProduct (M32 Float) (M22 Float) (M32 Float)
instance MatrixProduct (M32 Float) (M23 Float) (M33 Float)
instance MatrixProduct (M32 Float) (M24 Float) (M34 Float)
instance MatrixProduct (M33 Float) (M32 Float) (M32 Float)
instance MatrixProduct (M33 Float) (M33 Float) (M33 Float)
instance MatrixProduct (M33 Float) (M34 Float) (M34 Float)
instance MatrixProduct (M34 Float) (M42 Float) (M32 Float)
instance MatrixProduct (M34 Float) (M43 Float) (M33 Float)
instance MatrixProduct (M34 Float) (M44 Float) (M34 Float)
instance MatrixProduct (M42 Float) (M22 Float) (M42 Float)
instance MatrixProduct (M42 Float) (M23 Float) (M43 Float)
instance MatrixProduct (M42 Float) (M24 Float) (M44 Float)
instance MatrixProduct (M43 Float) (M32 Float) (M42 Float)
instance MatrixProduct (M43 Float) (M33 Float) (M43 Float)
instance MatrixProduct (M43 Float) (M34 Float) (M44 Float)
instance MatrixProduct (M44 Float) (M42 Float) (M42 Float)
instance MatrixProduct (M44 Float) (M43 Float) (M43 Float)
instance MatrixProduct (M44 Float) (M44 Float) (M44 Float)

matrixMultiply :: (MatrixProduct left right result) => Expr s left -> Expr s right -> Expr s result
matrixMultiply = I.matrixMultiplyExpr

(!*!) :: (MatrixProduct left right result) => Expr s left -> Expr s right -> Expr s result
(!*!) = matrixMultiply

infixl 7 !*!

class (MatrixValue matrix, ShaderValue vector, ShaderValue result) => MatrixVectorProduct matrix vector result | matrix vector -> result
instance MatrixVectorProduct (M22 Float) (V2 Float) (V2 Float)
instance MatrixVectorProduct (M23 Float) (V3 Float) (V2 Float)
instance MatrixVectorProduct (M24 Float) (V4 Float) (V2 Float)
instance MatrixVectorProduct (M32 Float) (V2 Float) (V3 Float)
instance MatrixVectorProduct (M33 Float) (V3 Float) (V3 Float)
instance MatrixVectorProduct (M34 Float) (V4 Float) (V3 Float)
instance MatrixVectorProduct (M42 Float) (V2 Float) (V4 Float)
instance MatrixVectorProduct (M43 Float) (V3 Float) (V4 Float)
instance MatrixVectorProduct (M44 Float) (V4 Float) (V4 Float)

matrixVectorMultiply :: (MatrixVectorProduct matrix vector result) => Expr s matrix -> Expr s vector -> Expr s result
matrixVectorMultiply = I.matrixVectorExpr

(!*) :: (MatrixVectorProduct matrix vector result) => Expr s matrix -> Expr s vector -> Expr s result
(!*) = matrixVectorMultiply

infixl 7 !*

type family DerivativeStage (stage :: Stage) :: Constraint where
  DerivativeStage 'Fragment = ()
  DerivativeStage stage =
    TypeError
      ( 'Text "Shader derivatives require fragment-stage values."
          ':$$: 'Text "Move dFdx, dFdy, or fwidth after rasterize, where values have type F a."
          ':$$: 'Text "The value supplied here belongs to stage "
          ':<>: 'ShowType stage
          ':<>: 'Text "."
      )

dFdx :: (ShaderFloat a, DerivativeStage stage) => Expr stage a -> Expr stage a
dFdx = I.derivative I.DfdxE

dFdy :: (ShaderFloat a, DerivativeStage stage) => Expr stage a -> Expr stage a
dFdy = I.derivative I.DfdyE

fwidth :: (ShaderFloat a, DerivativeStage stage) => Expr stage a -> Expr stage a
fwidth = I.derivative I.FwidthE

data Image2DHandle
data SamplerHandle
data ComparisonSamplerHandle
type role ImageResource nominal nominal nominal
newtype ImageResource (dim :: Dim) (format :: Format) s = ImageResource (Expr s Image2DHandle)
type Image2D s = ImageResource 'D2 'R8G8B8A8Unorm s
type role Sampler nominal
newtype Sampler s = Sampler (Expr s SamplerHandle)
type role ComparisonSampler nominal
newtype ComparisonSampler s = ComparisonSampler (Expr s ComparisonSamplerHandle)
type role SampledImage nominal nominal nominal
data SampledImage (dim :: Dim) (format :: Format) s = SampledImage (ImageResource dim format s) (Sampler s)
type role ComparisonSampledImage nominal nominal
data ComparisonSampledImage (dim :: Dim) s = ComparisonSampledImage (ImageResource dim 'D32Sfloat s) (ComparisonSampler s)
type Sampler2D s = SampledImage 'D2 'R8G8B8A8Unorm s

class KnownSampleDimension (dim :: Dim) where
  type SampleCoordinates dim
  sampleDimension :: Proxy dim -> I.ImageDimension

instance KnownSampleDimension 'D1 where
  type SampleCoordinates 'D1 = Float
  sampleDimension _ = I.Image1D

instance KnownSampleDimension 'D2 where
  type SampleCoordinates 'D2 = V2 Float
  sampleDimension _ = I.Image2D

instance KnownSampleDimension 'D3 where
  type SampleCoordinates 'D3 = V3 Float
  sampleDimension _ = I.Image3D

instance KnownSampleDimension 'Cube where
  type SampleCoordinates 'Cube = V3 Float
  sampleDimension _ = I.ImageCube

instance KnownSampleDimension 'D2Array where
  type SampleCoordinates 'D2Array = V3 Float
  sampleDimension _ = I.Image2DArray

imageResource :: forall dim format s. (KnownSampleDimension dim) => String -> ImageResource dim format s
imageResource name = ImageResource (I.resource (I.imageShaderType (sampleDimension (Proxy @dim))) name)

image2D :: String -> Image2D s
image2D = imageResource

sampler :: String -> Sampler s
sampler name = Sampler (I.resource I.TySampler name)

comparisonSampler :: String -> ComparisonSampler s
comparisonSampler name = ComparisonSampler (I.resource I.TySampler name)

sampledImage :: ImageResource dim format s -> Sampler s -> SampledImage dim format s
sampledImage = SampledImage

comparisonSampledImage :: ImageResource dim 'D32Sfloat s -> ComparisonSampler s -> ComparisonSampledImage dim s
comparisonSampledImage = ComparisonSampledImage

sampledImage2D :: Image2D s -> Sampler s -> Sampler2D s
sampledImage2D = sampledImage

sampler2D :: String -> Sampler2D s
sampler2D name = SampledImage (image2D (name <> ".image")) (sampler (name <> ".sampler"))

class SampledFormat (format :: Format) where
  projectSample :: Proxy format -> Expr s (V4 Float) -> Expr s (TexelOf format)

instance SampledFormat 'R8Unorm where projectSample _ = I.extract I.TyFloat [0]
instance SampledFormat 'R8G8B8A8Unorm where projectSample _ = id
instance SampledFormat 'R8G8B8A8Srgb where projectSample _ = id
instance SampledFormat 'B8G8R8A8Unorm where projectSample _ = id
instance SampledFormat 'B8G8R8A8Srgb where projectSample _ = id
instance SampledFormat 'R32Sfloat where projectSample _ = I.extract I.TyFloat [0]
instance SampledFormat 'R32G32Sfloat where projectSample _ = I.extract (I.TyVector 2) [0, 1]
instance SampledFormat 'R32G32B32Sfloat where projectSample _ = I.extract (I.TyVector 3) [0, 1, 2]
instance SampledFormat 'R32G32B32A32Sfloat where projectSample _ = id
instance SampledFormat 'D32Sfloat where projectSample _ = I.extract I.TyFloat [0]

sample :: forall dim format. (KnownSampleDimension dim, SampledFormat format) => SampledImage dim format 'Fragment -> Expr 'Fragment (SampleCoordinates dim) -> Expr 'Fragment (TexelOf format)
sample (SampledImage (ImageResource image) (Sampler samplerValue)) coordinates =
  projectSample (Proxy @format) (I.sampleExpr I.ImplicitLod image samplerValue coordinates Nothing)

sampleLod :: forall dim format s. (KnownSampleDimension dim, SampledFormat format) => SampledImage dim format s -> Expr s (SampleCoordinates dim) -> Expr s Float -> Expr s (TexelOf format)
sampleLod (SampledImage (ImageResource image) (Sampler samplerValue)) coordinates lod =
  projectSample (Proxy @format) (I.sampleExpr I.ExplicitLod image samplerValue coordinates (Just lod))

sampleCompare :: forall dim. (KnownSampleDimension dim) => ComparisonSampledImage dim 'Fragment -> Expr 'Fragment (SampleCoordinates dim) -> Expr 'Fragment Float -> Expr 'Fragment Float
sampleCompare (ComparisonSampledImage (ImageResource image) (ComparisonSampler samplerValue)) coordinates reference =
  I.sampleCompareExpr I.ImplicitLod image samplerValue coordinates reference Nothing

sampleCompareLod :: forall dim s. (KnownSampleDimension dim) => ComparisonSampledImage dim s -> Expr s (SampleCoordinates dim) -> Expr s Float -> Expr s Float -> Expr s Float
sampleCompareLod (ComparisonSampledImage (ImageResource image) (ComparisonSampler samplerValue)) coordinates reference lod =
  I.sampleCompareExpr I.ExplicitLod image samplerValue coordinates reference (Just lod)
