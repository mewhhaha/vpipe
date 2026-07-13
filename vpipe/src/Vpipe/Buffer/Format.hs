{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

{- | Host marshalling and Vulkan @Vertex@, @Std140@, and @Std430@ layout
calculation.

The reflected descriptions are useful when inspecting an interface:

@
module Main (main) where

import Vpipe.Buffer.Format

main :: IO ()
main = print (layoutSize (layoutOf Std430 (Vector 4 Float32)))
@

Matrix dimensions in @FieldLayout@ are columns followed by rows. The
@MatrixBuffer@ wrapper makes that ABI choice explicit for host values.
-}
module Vpipe.Buffer.Format (
  BufferFormat (..),
  BufferShape (..),
  MatrixComponent,
  MatrixBuffer (..),
  MatrixValue,
  ShaderBlockFormat,
  LinearMatrixBuffer (..),
  Generically (..),
  GenericHost (..),
  LayoutStandard (..),
  ScalarType (..),
  FieldLayout (..),
  Layout (..),
  layoutOf,
  staticBufferAlignment,
  staticBufferSize,
)
where

import Data.Int (Int32)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peek, poke)
import GHC.Generics (
  Generic (..),
  K1 (..),
  M1 (..),
  U1 (..),
  (:*:) (..),
 )
import GHC.TypeError (Unsatisfiable)
import GHC.TypeLits (
  CmpNat,
  Div,
  ErrorMessage (..),
  KnownNat,
  Nat,
  natVal,
  type (*),
  type (+),
  type (-),
 )
import Linear (V2 (..), V3 (..), V4 (..))

{- | The buffer representation used for a shader block value.  Linear matrix
types are row-major nested vectors, so their column-major buffer form has the
dimensions reversed.
-}
type family ShaderBlockFormat shader = block | block -> shader where
  ShaderBlockFormat Float = Float
  ShaderBlockFormat Int32 = Int32
  ShaderBlockFormat Bool = Bool
  ShaderBlockFormat (V2 Float) = V2 Float
  ShaderBlockFormat (V3 Float) = V3 Float
  ShaderBlockFormat (V4 Float) = V4 Float
  ShaderBlockFormat (V2 (V2 Float)) = MatrixBuffer 2 2 Float
  ShaderBlockFormat (V2 (V3 Float)) = MatrixBuffer 3 2 Float
  ShaderBlockFormat (V2 (V4 Float)) = MatrixBuffer 4 2 Float
  ShaderBlockFormat (V3 (V2 Float)) = MatrixBuffer 2 3 Float
  ShaderBlockFormat (V3 (V3 Float)) = MatrixBuffer 3 3 Float
  ShaderBlockFormat (V3 (V4 Float)) = MatrixBuffer 4 3 Float
  ShaderBlockFormat (V4 (V2 Float)) = MatrixBuffer 2 4 Float
  ShaderBlockFormat (V4 (V3 Float)) = MatrixBuffer 3 4 Float
  ShaderBlockFormat (V4 (V4 Float)) = MatrixBuffer 4 4 Float

data LayoutStandard = Vertex | Std140 | Std430
  deriving stock (Eq, Show)

data ScalarType = Float32 | SignedInt32 | UnsignedInt32 | Boolean32
  deriving stock (Eq, Show)

{- | A TypeRep-free description shared by memory marshalling and SPIR-V
decoration generation. Matrix dimensions are columns followed by rows.
-}
data FieldLayout
  = Scalar ScalarType
  | Vector Int ScalarType
  | Matrix Int Int ScalarType
  | Array Int FieldLayout
  | Struct [FieldLayout]
  deriving stock (Eq, Show)

data Layout = Layout
  { layoutAlignment :: Int
  , layoutOccupiedSize :: Int
  , layoutSize :: Int
  , layoutStride :: Maybe Int
  , layoutMatrixStride :: Maybe Int
  , layoutFieldOffsets :: [Int]
  }
  deriving stock (Eq, Show)

layoutOf :: LayoutStandard -> FieldLayout -> Layout
layoutOf standard field = case field of
  Scalar _ -> Layout 4 4 4 Nothing Nothing []
  Vector channels _ ->
    let alignment = vectorAlignment standard channels
        occupiedSize = 4 * channels
     in Layout alignment occupiedSize (alignUp occupiedSize alignment) Nothing Nothing []
  Matrix columns rows scalar ->
    let columnLayout = layoutOf standard (Vector rows scalar)
        alignment = arrayAlignment standard columnLayout
        stride = matrixColumnStride standard rows scalar
        size = columns * stride
     in Layout alignment size size Nothing (Just stride) []
  Array elementCount element ->
    let elementLayout = layoutOf standard element
        alignment = arrayAlignment standard elementLayout
        stride = alignUp (layoutSize elementLayout) alignment
        size = elementCount * stride
     in Layout alignment size size (Just stride) Nothing []
  Struct fields ->
    let fieldLayouts = fmap (layoutOf standard) fields
        alignment = structAlignment standard fieldLayouts
        offsets = fieldOffsets fieldLayouts
        end = case reverse (zip offsets fieldLayouts) of
          [] -> 0
          (offset, fieldLayout) : _ -> offset + layoutOccupiedSize fieldLayout
        size = alignUp end alignment
     in Layout alignment size size Nothing Nothing offsets

vectorAlignment :: LayoutStandard -> Int -> Int
vectorAlignment Vertex _ = 4
vectorAlignment _ 1 = 4
vectorAlignment _ 2 = 8
vectorAlignment _ _ = 16

arrayAlignment :: LayoutStandard -> Layout -> Int
arrayAlignment Std140 element = max 16 (layoutAlignment element)
arrayAlignment _ element = layoutAlignment element

structAlignment :: LayoutStandard -> [Layout] -> Int
structAlignment Std140 fields = max 16 (maximumOrOne (fmap layoutAlignment fields))
structAlignment _ fields = maximumOrOne (fmap layoutAlignment fields)

alignUp :: Int -> Int -> Int
alignUp value alignment = ((value + alignment - 1) `div` alignment) * alignment

maximumOrOne :: [Int] -> Int
maximumOrOne [] = 1
maximumOrOne values = maximum values

fieldOffsets :: [Layout] -> [Int]
fieldOffsets = snd . foldl addField (0, [])
 where
  addField (offset, offsets) field =
    let fieldOffset = alignUp offset (layoutAlignment field)
     in (fieldOffset + layoutOccupiedSize field, offsets <> [fieldOffset])

data BufferShape = ScalarShape | AggregateShape

data StaticField = StaticField Nat Nat

type AlignUpNat value alignment = ((value + alignment - 1) `Div` alignment) * alignment

type family MaxNat (left :: Nat) (right :: Nat) :: Nat where
  MaxNat left right = MaxNatByOrdering (CmpNat left right) left right

type family MaxNatByOrdering (ordering :: Ordering) (left :: Nat) (right :: Nat) :: Nat where
  MaxNatByOrdering 'LT _ right = right
  MaxNatByOrdering 'EQ left _ = left
  MaxNatByOrdering 'GT left _ = left

type family Append (left :: [value]) (right :: [value]) :: [value] where
  Append '[] right = right
  Append (value ': values) right = value ': Append values right

type family StaticAlignment (fields :: [StaticField]) :: Nat where
  StaticAlignment '[] = 1
  StaticAlignment ('StaticField alignment _ ': fields) = MaxNat alignment (StaticAlignment fields)

type family StaticEnd (offset :: Nat) (fields :: [StaticField]) :: Nat where
  StaticEnd offset '[] = offset
  StaticEnd offset ('StaticField alignment occupiedSize ': fields) =
    StaticEnd (AlignUpNat offset alignment + occupiedSize) fields

type StaticSize fields = AlignUpNat (StaticEnd 0 fields) (StaticAlignment fields)

type family GenericFields (representation :: Type -> Type) :: [StaticField] where
  GenericFields (M1 _ _ representation) = GenericFields representation
  GenericFields (K1 _ field) = '[ 'StaticField (BufferAlignment field) (BufferOccupiedSize field)]
  GenericFields U1 = '[]
  GenericFields (left :*: right) = Append (GenericFields left) (GenericFields right)

type family VectorAlignmentNat (components :: Nat) :: Nat where
  VectorAlignmentNat 1 = 4
  VectorAlignmentNat 2 = 8
  VectorAlignmentNat _ = 16

type family ContainerAlignment (components :: Nat) (shape :: BufferShape) (elementAlignment :: Nat) :: Nat where
  ContainerAlignment components 'ScalarShape _ = VectorAlignmentNat components
  ContainerAlignment _ 'AggregateShape elementAlignment = elementAlignment

type family
  ContainerOccupiedSize
    (components :: Nat)
    (shape :: BufferShape)
    (elementOccupiedSize :: Nat)
    (elementSize :: Nat)
    (elementAlignment :: Nat) ::
    Nat
  where
  ContainerOccupiedSize components 'ScalarShape elementOccupiedSize _ _ = components * elementOccupiedSize
  ContainerOccupiedSize components 'AggregateShape _ elementSize elementAlignment =
    components * AlignUpNat elementSize elementAlignment

type ContainerSize components shape elementOccupiedSize elementSize elementAlignment =
  AlignUpNat
    (ContainerOccupiedSize components shape elementOccupiedSize elementSize elementAlignment)
    (ContainerAlignment components shape elementAlignment)

{- | @a@ describes the GPU representation while @HostFormat@ is the CPU value
accepted and returned by the marshaller.
-}
class BufferFormat a where
  type HostFormat a :: Type

  -- These associated naturals describe the convenient std430 defaults.
  -- Use the standard-aware value methods below for vertex and std140 layouts.
  type BufferShapeOf a :: BufferShape
  type BufferAlignment a :: Nat
  type BufferOccupiedSize a :: Nat
  type BufferSize a :: Nat

  bufferFieldLayout :: Proxy a -> FieldLayout

  bufferAlignmentFor :: LayoutStandard -> Proxy a -> Int
  bufferAlignmentFor standard = layoutAlignment . layoutOf standard . bufferFieldLayout

  bufferSizeFor :: LayoutStandard -> Proxy a -> Int
  bufferSizeFor standard = layoutSize . layoutOf standard . bufferFieldLayout

  pokeBufferFor :: LayoutStandard -> Proxy a -> Ptr () -> HostFormat a -> IO ()
  peekBufferFor :: LayoutStandard -> Proxy a -> Ptr () -> IO (HostFormat a)

  bufferAlignment :: Proxy a -> Int
  bufferAlignment = bufferAlignmentFor Std430

  bufferSize :: Proxy a -> Int
  bufferSize = bufferSizeFor Std430

  pokeBuffer :: Proxy a -> Ptr () -> HostFormat a -> IO ()
  pokeBuffer = pokeBufferFor Std430

  peekBuffer :: Proxy a -> Ptr () -> IO (HostFormat a)
  peekBuffer = peekBufferFor Std430

-- | Reflect the type-level std430 alignment.
staticBufferAlignment :: forall a. (BufferFormat a, KnownNat (BufferAlignment a)) => Proxy a -> Int
staticBufferAlignment _ = fromIntegral (natVal (Proxy @(BufferAlignment a)))

-- | Reflect the type-level std430 padded size.
staticBufferSize :: forall a. (BufferFormat a, KnownNat (BufferSize a)) => Proxy a -> Int
staticBufferSize _ = fromIntegral (natVal (Proxy @(BufferSize a)))

type family MatrixComponent (component :: Type) :: Constraint where
  MatrixComponent Float = ()
  MatrixComponent component = Unsatisfiable (MatrixComponentMismatch component)

type family MatrixComponentMismatch (component :: Type) :: ErrorMessage where
  MatrixComponentMismatch component =
    'Text "MatrixBuffer requires Float components, but received "
      ':<>: 'ShowType component
      ':<>: 'Text "."
      ':$$: 'Text "Fix: use MatrixBuffer with Float components."

instance BufferFormat Float where
  type HostFormat Float = Float
  type BufferShapeOf Float = 'ScalarShape
  type BufferAlignment Float = 4
  type BufferOccupiedSize Float = 4
  type BufferSize Float = 4
  bufferFieldLayout _ = Scalar Float32
  pokeBufferFor _ _ pointer = poke (castPtr pointer)
  peekBufferFor _ _ pointer = peek (castPtr pointer)

instance BufferFormat Int32 where
  type HostFormat Int32 = Int32
  type BufferShapeOf Int32 = 'ScalarShape
  type BufferAlignment Int32 = 4
  type BufferOccupiedSize Int32 = 4
  type BufferSize Int32 = 4
  bufferFieldLayout _ = Scalar SignedInt32
  pokeBufferFor _ _ pointer = poke (castPtr pointer)
  peekBufferFor _ _ pointer = peek (castPtr pointer)

instance BufferFormat Word32 where
  type HostFormat Word32 = Word32
  type BufferShapeOf Word32 = 'ScalarShape
  type BufferAlignment Word32 = 4
  type BufferOccupiedSize Word32 = 4
  type BufferSize Word32 = 4
  bufferFieldLayout _ = Scalar UnsignedInt32
  pokeBufferFor _ _ pointer = poke (castPtr pointer)
  peekBufferFor _ _ pointer = peek (castPtr pointer)

instance BufferFormat Bool where
  type HostFormat Bool = Bool
  type BufferShapeOf Bool = 'ScalarShape
  type BufferAlignment Bool = 4
  type BufferOccupiedSize Bool = 4
  type BufferSize Bool = 4
  bufferFieldLayout _ = Scalar Boolean32
  pokeBufferFor _ _ pointer value = poke (castPtr pointer :: Ptr Word32) (if value then 1 else 0)
  peekBufferFor _ _ pointer = (/= 0) <$> peek (castPtr pointer :: Ptr Word32)

instance (BufferFormat a) => BufferFormat (V2 a) where
  type HostFormat (V2 a) = V2 (HostFormat a)
  type BufferShapeOf (V2 a) = 'AggregateShape
  type BufferAlignment (V2 a) = ContainerAlignment 2 (BufferShapeOf a) (BufferAlignment a)
  type
    BufferOccupiedSize (V2 a) =
      ContainerOccupiedSize 2 (BufferShapeOf a) (BufferOccupiedSize a) (BufferSize a) (BufferAlignment a)
  type
    BufferSize (V2 a) =
      ContainerSize 2 (BufferShapeOf a) (BufferOccupiedSize a) (BufferSize a) (BufferAlignment a)
  bufferFieldLayout _ = vectorOrArray 2 (bufferFieldLayout (Proxy @a))
  pokeBufferFor standard _ pointer (V2 x y) = do
    pokeBufferFor standard (Proxy @a) (pointerAt pointer 0) x
    pokeBufferFor standard (Proxy @a) (pointerAt pointer stride) y
   where
    stride = containerElementStride standard (Proxy @a)
  peekBufferFor standard _ pointer = do
    x <- peekBufferFor standard (Proxy @a) (pointerAt pointer 0)
    y <- peekBufferFor standard (Proxy @a) (pointerAt pointer stride)
    pure (V2 x y)
   where
    stride = containerElementStride standard (Proxy @a)

instance (BufferFormat a) => BufferFormat (V3 a) where
  type HostFormat (V3 a) = V3 (HostFormat a)
  type BufferShapeOf (V3 a) = 'AggregateShape
  type BufferAlignment (V3 a) = ContainerAlignment 3 (BufferShapeOf a) (BufferAlignment a)
  type
    BufferOccupiedSize (V3 a) =
      ContainerOccupiedSize 3 (BufferShapeOf a) (BufferOccupiedSize a) (BufferSize a) (BufferAlignment a)
  type
    BufferSize (V3 a) =
      ContainerSize 3 (BufferShapeOf a) (BufferOccupiedSize a) (BufferSize a) (BufferAlignment a)
  bufferFieldLayout _ = vectorOrArray 3 (bufferFieldLayout (Proxy @a))
  pokeBufferFor standard _ pointer (V3 x y z) = do
    pokeBufferFor standard (Proxy @a) (pointerAt pointer 0) x
    pokeBufferFor standard (Proxy @a) (pointerAt pointer stride) y
    pokeBufferFor standard (Proxy @a) (pointerAt pointer (2 * stride)) z
   where
    stride = containerElementStride standard (Proxy @a)
  peekBufferFor standard _ pointer = do
    x <- peekBufferFor standard (Proxy @a) (pointerAt pointer 0)
    y <- peekBufferFor standard (Proxy @a) (pointerAt pointer stride)
    z <- peekBufferFor standard (Proxy @a) (pointerAt pointer (2 * stride))
    pure (V3 x y z)
   where
    stride = containerElementStride standard (Proxy @a)

instance (BufferFormat a) => BufferFormat (V4 a) where
  type HostFormat (V4 a) = V4 (HostFormat a)
  type BufferShapeOf (V4 a) = 'AggregateShape
  type BufferAlignment (V4 a) = ContainerAlignment 4 (BufferShapeOf a) (BufferAlignment a)
  type
    BufferOccupiedSize (V4 a) =
      ContainerOccupiedSize 4 (BufferShapeOf a) (BufferOccupiedSize a) (BufferSize a) (BufferAlignment a)
  type
    BufferSize (V4 a) =
      ContainerSize 4 (BufferShapeOf a) (BufferOccupiedSize a) (BufferSize a) (BufferAlignment a)
  bufferFieldLayout _ = vectorOrArray 4 (bufferFieldLayout (Proxy @a))
  pokeBufferFor standard _ pointer (V4 x y z w) = do
    pokeBufferFor standard (Proxy @a) (pointerAt pointer 0) x
    pokeBufferFor standard (Proxy @a) (pointerAt pointer stride) y
    pokeBufferFor standard (Proxy @a) (pointerAt pointer (2 * stride)) z
    pokeBufferFor standard (Proxy @a) (pointerAt pointer (3 * stride)) w
   where
    stride = containerElementStride standard (Proxy @a)
  peekBufferFor standard _ pointer = do
    x <- peekBufferFor standard (Proxy @a) (pointerAt pointer 0)
    y <- peekBufferFor standard (Proxy @a) (pointerAt pointer stride)
    z <- peekBufferFor standard (Proxy @a) (pointerAt pointer (2 * stride))
    w <- peekBufferFor standard (Proxy @a) (pointerAt pointer (3 * stride))
    pure (V4 x y z w)
   where
    stride = containerElementStride standard (Proxy @a)

type family VectorValue (components :: Nat) a :: Type where
  VectorValue 2 a = V2 a
  VectorValue 3 a = V3 a
  VectorValue 4 a = V4 a

type MatrixValue columns rows a = VectorValue columns (VectorValue rows a)

{- | An explicit column-major matrix representation. The wrapper disambiguates
matrices from nested linear vectors, which remain useful as fixed arrays.
-}
newtype MatrixBuffer (columns :: Nat) (rows :: Nat) a
  = MatrixBuffer {unMatrixBuffer :: MatrixValue columns rows a}

deriving stock instance (Eq (MatrixValue columns rows a)) => Eq (MatrixBuffer columns rows a)

deriving stock instance (Show (MatrixValue columns rows a)) => Show (MatrixBuffer columns rows a)

{- | Convert between Linear's row-major matrix aliases and the explicit
column-major buffer representation used by shader blocks.
-}
class LinearMatrixBuffer matrix (columns :: Nat) (rows :: Nat) | matrix -> columns rows, columns rows -> matrix where
  toMatrixBuffer :: matrix -> MatrixBuffer columns rows Float
  fromMatrixBuffer :: MatrixBuffer columns rows Float -> matrix

instance LinearMatrixBuffer (V2 (V2 Float)) 2 2 where
  toMatrixBuffer (V2 (V2 a b) (V2 c d)) = MatrixBuffer (V2 (V2 a c) (V2 b d))
  fromMatrixBuffer (MatrixBuffer (V2 (V2 a c) (V2 b d))) = V2 (V2 a b) (V2 c d)

instance LinearMatrixBuffer (V2 (V3 Float)) 3 2 where
  toMatrixBuffer (V2 (V3 a b c) (V3 d e f)) = MatrixBuffer (V3 (V2 a d) (V2 b e) (V2 c f))
  fromMatrixBuffer (MatrixBuffer (V3 (V2 a d) (V2 b e) (V2 c f))) = V2 (V3 a b c) (V3 d e f)

instance LinearMatrixBuffer (V2 (V4 Float)) 4 2 where
  toMatrixBuffer (V2 (V4 a b c d) (V4 e f g h)) = MatrixBuffer (V4 (V2 a e) (V2 b f) (V2 c g) (V2 d h))
  fromMatrixBuffer (MatrixBuffer (V4 (V2 a e) (V2 b f) (V2 c g) (V2 d h))) = V2 (V4 a b c d) (V4 e f g h)

instance LinearMatrixBuffer (V3 (V2 Float)) 2 3 where
  toMatrixBuffer (V3 (V2 a b) (V2 c d) (V2 e f)) = MatrixBuffer (V2 (V3 a c e) (V3 b d f))
  fromMatrixBuffer (MatrixBuffer (V2 (V3 a c e) (V3 b d f))) = V3 (V2 a b) (V2 c d) (V2 e f)

instance LinearMatrixBuffer (V3 (V3 Float)) 3 3 where
  toMatrixBuffer (V3 (V3 a b c) (V3 d e f) (V3 g h i)) = MatrixBuffer (V3 (V3 a d g) (V3 b e h) (V3 c f i))
  fromMatrixBuffer (MatrixBuffer (V3 (V3 a d g) (V3 b e h) (V3 c f i))) = V3 (V3 a b c) (V3 d e f) (V3 g h i)

instance LinearMatrixBuffer (V3 (V4 Float)) 4 3 where
  toMatrixBuffer (V3 (V4 a b c d) (V4 e f g h) (V4 i j k l)) = MatrixBuffer (V4 (V3 a e i) (V3 b f j) (V3 c g k) (V3 d h l))
  fromMatrixBuffer (MatrixBuffer (V4 (V3 a e i) (V3 b f j) (V3 c g k) (V3 d h l))) = V3 (V4 a b c d) (V4 e f g h) (V4 i j k l)

instance LinearMatrixBuffer (V4 (V2 Float)) 2 4 where
  toMatrixBuffer (V4 (V2 a b) (V2 c d) (V2 e f) (V2 g h)) = MatrixBuffer (V2 (V4 a c e g) (V4 b d f h))
  fromMatrixBuffer (MatrixBuffer (V2 (V4 a c e g) (V4 b d f h))) = V4 (V2 a b) (V2 c d) (V2 e f) (V2 g h)

instance LinearMatrixBuffer (V4 (V3 Float)) 3 4 where
  toMatrixBuffer (V4 (V3 a b c) (V3 d e f) (V3 g h i) (V3 j k l)) = MatrixBuffer (V3 (V4 a d g j) (V4 b e h k) (V4 c f i l))
  fromMatrixBuffer (MatrixBuffer (V3 (V4 a d g j) (V4 b e h k) (V4 c f i l))) = V4 (V3 a b c) (V3 d e f) (V3 g h i) (V3 j k l)

instance LinearMatrixBuffer (V4 (V4 Float)) 4 4 where
  toMatrixBuffer (V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)) = MatrixBuffer (V4 (V4 a e i m) (V4 b f j n) (V4 c g k o) (V4 d h l p))
  fromMatrixBuffer (MatrixBuffer (V4 (V4 a e i m) (V4 b f j n) (V4 c g k o) (V4 d h l p))) = V4 (V4 a b c d) (V4 e f g h) (V4 i j k l) (V4 m n o p)

type MatrixAlignment rows = VectorAlignmentNat rows

type MatrixColumnSize rows = AlignUpNat (rows * 4) (MatrixAlignment rows)

instance
  ( KnownNat columns
  , KnownNat rows
  , MatrixComponent a
  , FixedVector columns
  , FixedVector rows
  , BufferFormat a
  ) =>
  BufferFormat (MatrixBuffer columns rows a)
  where
  type HostFormat (MatrixBuffer columns rows a) = MatrixBuffer columns rows (HostFormat a)
  type BufferShapeOf (MatrixBuffer columns rows a) = 'AggregateShape
  type BufferAlignment (MatrixBuffer columns rows a) = MatrixAlignment rows
  type BufferOccupiedSize (MatrixBuffer columns rows a) = columns * MatrixColumnSize rows
  type BufferSize (MatrixBuffer columns rows a) = columns * MatrixColumnSize rows
  bufferFieldLayout _ =
    Matrix
      (fromIntegral (natVal (Proxy @columns)))
      (fromIntegral (natVal (Proxy @rows)))
      Float32
  pokeBufferFor standard _ pointer (MatrixBuffer value) =
    pokeFixedVector
      @columns
      (pokeFixedVector @rows (pokeBufferFor standard (Proxy @a)) 4)
      (matrixColumnStride standard (fromIntegral (natVal (Proxy @rows))) Float32)
      pointer
      value
  peekBufferFor standard _ pointer =
    MatrixBuffer
      <$> peekFixedVector
        @columns
        (peekFixedVector @rows (peekBufferFor standard (Proxy @a)) 4)
        (matrixColumnStride standard (fromIntegral (natVal (Proxy @rows))) Float32)
        pointer

class FixedVector (components :: Nat) where
  pokeFixedVector :: (Ptr () -> a -> IO ()) -> Int -> Ptr () -> VectorValue components a -> IO ()
  peekFixedVector :: (Ptr () -> IO a) -> Int -> Ptr () -> IO (VectorValue components a)

instance FixedVector 2 where
  pokeFixedVector pokeElement stride pointer (V2 x y) = do
    pokeElement (pointerAt pointer 0) x
    pokeElement (pointerAt pointer stride) y
  peekFixedVector peekElement stride pointer =
    V2
      <$> peekElement (pointerAt pointer 0)
      <*> peekElement (pointerAt pointer stride)

instance FixedVector 3 where
  pokeFixedVector pokeElement stride pointer (V3 x y z) = do
    pokeElement (pointerAt pointer 0) x
    pokeElement (pointerAt pointer stride) y
    pokeElement (pointerAt pointer (2 * stride)) z
  peekFixedVector peekElement stride pointer =
    V3
      <$> peekElement (pointerAt pointer 0)
      <*> peekElement (pointerAt pointer stride)
      <*> peekElement (pointerAt pointer (2 * stride))

instance FixedVector 4 where
  pokeFixedVector pokeElement stride pointer (V4 x y z w) = do
    pokeElement (pointerAt pointer 0) x
    pokeElement (pointerAt pointer stride) y
    pokeElement (pointerAt pointer (2 * stride)) z
    pokeElement (pointerAt pointer (3 * stride)) w
  peekFixedVector peekElement stride pointer =
    V4
      <$> peekElement (pointerAt pointer 0)
      <*> peekElement (pointerAt pointer stride)
      <*> peekElement (pointerAt pointer (2 * stride))
      <*> peekElement (pointerAt pointer (3 * stride))

instance (BufferFormat a, BufferFormat b) => BufferFormat (a, b) where
  type HostFormat (a, b) = (HostFormat a, HostFormat b)
  type BufferShapeOf (a, b) = 'AggregateShape
  type BufferAlignment (a, b) = MaxNat (BufferAlignment a) (BufferAlignment b)
  type
    BufferOccupiedSize (a, b) =
      AlignUpNat
        (AlignUpNat (BufferOccupiedSize a) (BufferAlignment b) + BufferOccupiedSize b)
        (MaxNat (BufferAlignment a) (BufferAlignment b))
  type BufferSize (a, b) = BufferOccupiedSize (a, b)
  bufferFieldLayout _ = Struct [bufferFieldLayout (Proxy @a), bufferFieldLayout (Proxy @b)]
  pokeBufferFor standard _ pointer (firstValue, secondValue) = do
    pokeBufferFor standard (Proxy @a) (pointerAt pointer firstOffset) firstValue
    pokeBufferFor standard (Proxy @b) (pointerAt pointer secondOffset) secondValue
   where
    (firstOffset, secondOffset) =
      pairOffsets standard (bufferFieldLayout (Proxy @a)) (bufferFieldLayout (Proxy @b))
  peekBufferFor standard _ pointer = do
    firstValue <- peekBufferFor standard (Proxy @a) (pointerAt pointer firstOffset)
    secondValue <- peekBufferFor standard (Proxy @b) (pointerAt pointer secondOffset)
    pure (firstValue, secondValue)
   where
    (firstOffset, secondOffset) =
      pairOffsets standard (bufferFieldLayout (Proxy @a)) (bufferFieldLayout (Proxy @b))

newtype Generically a = Generically {unGenerically :: a}

{- | Pair structurally isomorphic GPU and host records when one or more GPU
fields have a non-identity @HostFormat@. Unlike @Generically@, this wrapper
is used explicitly as the buffer representation rather than via coercion.
-}
data GenericHost gpu host = GenericHost

instance (Generic a, GBufferHost (Rep a) (Rep a)) => BufferFormat (Generically a) where
  type HostFormat (Generically a) = a
  type BufferShapeOf (Generically a) = 'AggregateShape
  type BufferAlignment (Generically a) = StaticAlignment (GenericFields (Rep a))
  type BufferOccupiedSize (Generically a) = StaticSize (GenericFields (Rep a))
  type BufferSize (Generically a) = StaticSize (GenericFields (Rep a))
  bufferFieldLayout _ = Struct (gHostFieldLayouts (Proxy @(Rep a)) (Proxy @(Rep a)))
  pokeBufferFor standard _ pointer value = do
    _ <- gPokeHostFields (Proxy @(Rep a)) (Proxy @(Rep a)) standard pointer [] (from value)
    pure ()
  peekBufferFor standard _ pointer = do
    (value, _) <- gPeekHostFields (Proxy @(Rep a)) (Proxy @(Rep a)) standard pointer []
    pure (to value)

instance
  (Generic gpu, Generic host, GBufferHost (Rep gpu) (Rep host)) =>
  BufferFormat (GenericHost gpu host)
  where
  type HostFormat (GenericHost gpu host) = host
  type BufferShapeOf (GenericHost gpu host) = 'AggregateShape
  type BufferAlignment (GenericHost gpu host) = StaticAlignment (GenericFields (Rep gpu))
  type BufferOccupiedSize (GenericHost gpu host) = StaticSize (GenericFields (Rep gpu))
  type BufferSize (GenericHost gpu host) = StaticSize (GenericFields (Rep gpu))
  bufferFieldLayout _ = Struct (gHostFieldLayouts (Proxy @(Rep gpu)) (Proxy @(Rep host)))
  pokeBufferFor standard _ pointer value = do
    _ <- gPokeHostFields (Proxy @(Rep gpu)) (Proxy @(Rep host)) standard pointer [] (from value)
    pure ()
  peekBufferFor standard _ pointer = do
    (value, _) <- gPeekHostFields (Proxy @(Rep gpu)) (Proxy @(Rep host)) standard pointer []
    pure (to value)

class GBufferHost gpuRepresentation hostRepresentation where
  gHostFieldLayouts :: Proxy gpuRepresentation -> Proxy hostRepresentation -> [FieldLayout]
  gPokeHostFields ::
    Proxy gpuRepresentation ->
    Proxy hostRepresentation ->
    LayoutStandard ->
    Ptr () ->
    [FieldLayout] ->
    hostRepresentation p ->
    IO [FieldLayout]
  gPeekHostFields ::
    Proxy gpuRepresentation ->
    Proxy hostRepresentation ->
    LayoutStandard ->
    Ptr () ->
    [FieldLayout] ->
    IO (hostRepresentation p, [FieldLayout])

instance
  (GBufferHost gpuRepresentation hostRepresentation) =>
  GBufferHost (M1 gpuIndex gpuMetadata gpuRepresentation) (M1 hostIndex hostMetadata hostRepresentation)
  where
  gHostFieldLayouts _ _ =
    gHostFieldLayouts (Proxy @gpuRepresentation) (Proxy @hostRepresentation)
  gPokeHostFields _ _ standard pointer fields (M1 value) =
    gPokeHostFields
      (Proxy @gpuRepresentation)
      (Proxy @hostRepresentation)
      standard
      pointer
      fields
      value
  gPeekHostFields _ _ standard pointer fields = do
    (value, remainingFields) <-
      gPeekHostFields
        (Proxy @gpuRepresentation)
        (Proxy @hostRepresentation)
        standard
        pointer
        fields
    pure (M1 value, remainingFields)

instance
  (BufferFormat gpuField, HostFormat gpuField ~ hostField) =>
  GBufferHost (K1 gpuIndex gpuField) (K1 hostIndex hostField)
  where
  gHostFieldLayouts _ _ = [bufferFieldLayout (Proxy @gpuField)]
  gPokeHostFields _ _ standard pointer previousFields (K1 value) = do
    let field = bufferFieldLayout (Proxy @gpuField)
        offset = nextFieldOffset standard previousFields field
    pokeBufferFor standard (Proxy @gpuField) (pointerAt pointer offset) value
    pure (previousFields <> [field])
  gPeekHostFields _ _ standard pointer previousFields = do
    let field = bufferFieldLayout (Proxy @gpuField)
        offset = nextFieldOffset standard previousFields field
    value <- peekBufferFor standard (Proxy @gpuField) (pointerAt pointer offset)
    pure (K1 value, previousFields <> [field])

instance GBufferHost U1 U1 where
  gHostFieldLayouts _ _ = []
  gPokeHostFields _ _ _ _ fields U1 = pure fields
  gPeekHostFields _ _ _ _ fields = pure (U1, fields)

instance
  ( GBufferHost gpuLeft hostLeft
  , GBufferHost gpuRight hostRight
  ) =>
  GBufferHost (gpuLeft :*: gpuRight) (hostLeft :*: hostRight)
  where
  gHostFieldLayouts _ _ =
    gHostFieldLayouts (Proxy @gpuLeft) (Proxy @hostLeft)
      <> gHostFieldLayouts (Proxy @gpuRight) (Proxy @hostRight)
  gPokeHostFields _ _ standard pointer fields (left :*: right) = do
    remainingFields <-
      gPokeHostFields (Proxy @gpuLeft) (Proxy @hostLeft) standard pointer fields left
    gPokeHostFields
      (Proxy @gpuRight)
      (Proxy @hostRight)
      standard
      pointer
      remainingFields
      right
  gPeekHostFields _ _ standard pointer fields = do
    (left, remainingFields) <-
      gPeekHostFields (Proxy @gpuLeft) (Proxy @hostLeft) standard pointer fields
    (right, finalFields) <-
      gPeekHostFields
        (Proxy @gpuRight)
        (Proxy @hostRight)
        standard
        pointer
        remainingFields
    pure (left :*: right, finalFields)

pairOffsets :: LayoutStandard -> FieldLayout -> FieldLayout -> (Int, Int)
pairOffsets standard first second =
  (nextFieldOffset standard [] first, nextFieldOffset standard [first] second)

nextFieldOffset :: LayoutStandard -> [FieldLayout] -> FieldLayout -> Int
nextFieldOffset standard previousFields field =
  alignUp (fieldsEnd standard previousFields) (layoutAlignment (layoutOf standard field))

fieldsEnd :: LayoutStandard -> [FieldLayout] -> Int
fieldsEnd standard = foldl addField 0
 where
  addField offset field =
    let fieldLayout = layoutOf standard field
        fieldOffset = alignUp offset (layoutAlignment fieldLayout)
     in fieldOffset + layoutOccupiedSize fieldLayout

containerElementStride :: forall a proxy. (BufferFormat a) => LayoutStandard -> proxy a -> Int
containerElementStride standard _ = case bufferFieldLayout (Proxy @a) of
  Scalar _ -> layoutOccupiedSize (layoutOf standard (bufferFieldLayout (Proxy @a)))
  element ->
    let elementLayout = layoutOf standard element
        alignment = arrayAlignment standard elementLayout
     in alignUp (layoutSize elementLayout) alignment

pointerAt :: Ptr () -> Int -> Ptr ()
pointerAt pointer offset = pointer `plusPtr` offset

vectorOrArray :: Int -> FieldLayout -> FieldLayout
vectorOrArray components element = case element of
  Scalar scalar -> Vector components scalar
  _ -> Array components element

matrixColumnStride :: LayoutStandard -> Int -> ScalarType -> Int
matrixColumnStride standard rows scalar =
  let columnLayout = layoutOf standard (Vector rows scalar)
      alignment = arrayAlignment standard columnLayout
   in alignUp (layoutSize columnLayout) alignment
