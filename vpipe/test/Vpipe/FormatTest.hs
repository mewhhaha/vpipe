{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Vpipe.FormatTest (formatTests) where

import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Int (Int32)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, peekByteOff, poke)
import GHC.Generics (Generic)
import Linear (V2 (..), V3 (..), V4 (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (
  Arbitrary (..),
  Property,
  chooseInt,
  counterexample,
  elements,
  ioProperty,
  sized,
  testProperty,
  vectorOf,
 )
import Vpipe.Buffer.Format
import Vpipe.Format
import Vulkan.Core10.Enums.Format qualified as Vk

data TestVertex = TestVertex
  { position :: V3 Float
  , textureCoordinate :: V2 Float
  }
  deriving stock (Eq, Generic, Show)
  deriving (BufferFormat) via Generically TestVertex

data InnerBlock = InnerBlock
  { innerWeight :: Float
  , innerDirection :: V2 Float
  }
  deriving stock (Eq, Generic, Show)
  deriving (BufferFormat) via Generically InnerBlock

data OuterBlock = OuterBlock
  { outerScale :: Float
  , outerInner :: InnerBlock
  , outerNormal :: V3 Float
  }
  deriving stock (Eq, Generic, Show)
  deriving (BufferFormat) via Generically OuterBlock

data FloatSlot

instance BufferFormat FloatSlot where
  type HostFormat FloatSlot = Float
  type BufferShapeOf FloatSlot = 'ScalarShape
  type BufferAlignment FloatSlot = 4
  type BufferOccupiedSize FloatSlot = 4
  type BufferSize FloatSlot = 4
  bufferFieldLayout _ = Scalar Float32
  pokeBufferFor _ _ pointer = poke (castPtr pointer)
  peekBufferFor _ _ pointer = peek (castPtr pointer)

data GpuMaterial = GpuMaterial FloatSlot (V2 Float)
  deriving stock (Generic)

data HostMaterial = HostMaterial Float (V2 Float)
  deriving stock (Eq, Generic, Show)

formatTests :: TestTree
formatTests =
  testGroup
    "formats"
    [ testCase "reflects promoted Vulkan formats" $ do
        formatVal @'R8G8B8A8Srgb @?= Vk.FORMAT_R8G8B8A8_SRGB
        formatVal @'B8G8R8A8Srgb @?= Vk.FORMAT_B8G8R8A8_SRGB
    , recordedLayoutFixtures
    , goldenVsString "records a reference aggregate layout" "test/golden/format/reference-aggregate.layout" referenceAggregateLayout
    , matrixShapeFixtures
    , matrixMarshallingFixtures
    , testCase "converts every Linear matrix shape to column-major buffers" matrixBufferConversions
    , testCase "reflects fixed std430 type-level sizes and alignments" $ do
        staticBufferAlignment (Proxy @(V3 Float)) @?= 16
        staticBufferSize (Proxy @(V3 Float)) @?= 16
        staticBufferSize (Proxy @(V2 (V3 Float))) @?= 32
        staticBufferAlignment (Proxy @(MatrixBuffer 2 3 Float)) @?= 16
        staticBufferSize (Proxy @(MatrixBuffer 2 3 Float)) @?= 32
        staticBufferSize (Proxy @TestVertex) @?= 32
    , testCase "marshals a std140 vec2 array with sixteen-byte stride" testStd140Vec2Array
    , testCase "marshals a std430 vec3 array with sixteen-byte stride" testStd430Vec3Array
    , testCase "marshals a matrix from the same matrix layout description" testMatrixRoundTrip
    , testCase "marshals nested generic records using std140 member offsets" testNestedGenericStd140
    , testCase "marshals an unwrapped HostFormat through Generically" testHostFormatBoundary
    , testCase "pairs generic GPU and host records with different field types" testGenericHostBoundary
    , testCase "keeps raw nested vectors as arrays rather than matrices" $ do
        bufferFieldLayout (Proxy @(V2 (V3 Float)))
          @?= Array 2 (Vector 3 Float32)
        bufferFieldLayout (Proxy @(V2 (V2 Int32)))
          @?= Array 2 (Vector 2 SignedInt32)
        bufferFieldLayout (Proxy @(V2 (V2 Word32)))
          @?= Array 2 (Vector 2 UnsignedInt32)
        bufferFieldLayout (Proxy @(V2 (V2 Bool)))
          @?= Array 2 (Vector 2 Boolean32)
    , testProperty "layout offsets and total size are aligned without overlap" propWellFormedLayouts
    , testProperty "std430 defaults round-trip a generically derived record" propRoundTripVertex
    , testProperty "supported buffer formats round-trip through their declared layouts" propRoundTripSupportedFormats
    ]

matrixBufferConversions :: Assertion
matrixBufferConversions = do
  assertConversion (V2 (V2 1 2) (V2 3 4) :: V2 (V2 Float)) (V2 (V2 1 3) (V2 2 4))
  assertConversion (V2 (V3 1 2 3) (V3 4 5 6) :: V2 (V3 Float)) (V3 (V2 1 4) (V2 2 5) (V2 3 6))
  assertConversion (V2 (V4 1 2 3 4) (V4 5 6 7 8) :: V2 (V4 Float)) (V4 (V2 1 5) (V2 2 6) (V2 3 7) (V2 4 8))
  assertConversion (V3 (V2 1 2) (V2 3 4) (V2 5 6) :: V3 (V2 Float)) (V2 (V3 1 3 5) (V3 2 4 6))
  assertConversion (V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9) :: V3 (V3 Float)) (V3 (V3 1 4 7) (V3 2 5 8) (V3 3 6 9))
  assertConversion (V3 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) :: V3 (V4 Float)) (V4 (V3 1 5 9) (V3 2 6 10) (V3 3 7 11) (V3 4 8 12))
  assertConversion (V4 (V2 1 2) (V2 3 4) (V2 5 6) (V2 7 8) :: V4 (V2 Float)) (V2 (V4 1 3 5 7) (V4 2 4 6 8))
  assertConversion (V4 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9) (V3 10 11 12) :: V4 (V3 Float)) (V3 (V4 1 4 7 10) (V4 2 5 8 11) (V4 3 6 9 12))
  assertConversion (V4 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) (V4 13 14 15 16) :: V4 (V4 Float)) (V4 (V4 1 5 9 13) (V4 2 6 10 14) (V4 3 7 11 15) (V4 4 8 12 16))
 where
  assertConversion matrix expected = do
    unMatrixBuffer (toMatrixBuffer matrix) @?= expected
    fromMatrixBuffer (toMatrixBuffer matrix) @?= matrix

recordedLayoutFixtures :: TestTree
recordedLayoutFixtures =
  -- These constants were recorded from the glslang source kept beside the
  -- aggregate golden; the test expectation is independent of layoutOf.
  testGroup
    "recorded GLSL layout fixtures"
    [ fixture
        "std140 vec2[2]"
        Std140
        (Array 2 (Vector 2 Float32))
        (Layout 16 32 32 (Just 16) Nothing [])
    , fixture
        "std430 vec3[2]"
        Std430
        (Array 2 (Vector 3 Float32))
        (Layout 16 32 32 (Just 16) Nothing [])
    , fixture
        "std140 mat2"
        Std140
        (Matrix 2 2 Float32)
        (Layout 16 32 32 Nothing (Just 16) [])
    , fixture
        "std140 nested block"
        Std140
        (Struct [Scalar Float32, Struct [Scalar Float32, Vector 2 Float32], Vector 3 Float32])
        (Layout 16 48 48 Nothing Nothing [0, 16, 32])
    , fixture
        "std430 vec3 followed by scalar"
        Std430
        (Struct [Vector 3 Float32, Scalar Float32])
        (Layout 16 16 16 Nothing Nothing [0, 12])
    ]
 where
  fixture name standard field expected = testCase name (layoutOf standard field @?= expected)

matrixShapeFixtures :: TestTree
matrixShapeFixtures =
  testGroup
    "matrix shapes"
    [ matrixFixture
        "2x2"
        (bufferFieldLayout (Proxy @(MatrixBuffer 2 2 Float)))
        (bufferSize (Proxy @(MatrixBuffer 2 2 Float)))
        2
        2
        16
    , matrixFixture
        "2x3"
        (bufferFieldLayout (Proxy @(MatrixBuffer 2 3 Float)))
        (bufferSize (Proxy @(MatrixBuffer 2 3 Float)))
        2
        3
        32
    , matrixFixture
        "2x4"
        (bufferFieldLayout (Proxy @(MatrixBuffer 2 4 Float)))
        (bufferSize (Proxy @(MatrixBuffer 2 4 Float)))
        2
        4
        32
    , matrixFixture
        "3x2"
        (bufferFieldLayout (Proxy @(MatrixBuffer 3 2 Float)))
        (bufferSize (Proxy @(MatrixBuffer 3 2 Float)))
        3
        2
        24
    , matrixFixture
        "3x3"
        (bufferFieldLayout (Proxy @(MatrixBuffer 3 3 Float)))
        (bufferSize (Proxy @(MatrixBuffer 3 3 Float)))
        3
        3
        48
    , matrixFixture
        "3x4"
        (bufferFieldLayout (Proxy @(MatrixBuffer 3 4 Float)))
        (bufferSize (Proxy @(MatrixBuffer 3 4 Float)))
        3
        4
        48
    , matrixFixture
        "4x2"
        (bufferFieldLayout (Proxy @(MatrixBuffer 4 2 Float)))
        (bufferSize (Proxy @(MatrixBuffer 4 2 Float)))
        4
        2
        32
    , matrixFixture
        "4x3"
        (bufferFieldLayout (Proxy @(MatrixBuffer 4 3 Float)))
        (bufferSize (Proxy @(MatrixBuffer 4 3 Float)))
        4
        3
        64
    , matrixFixture
        "4x4"
        (bufferFieldLayout (Proxy @(MatrixBuffer 4 4 Float)))
        (bufferSize (Proxy @(MatrixBuffer 4 4 Float)))
        4
        4
        64
    ]

matrixFixture ::
  String ->
  FieldLayout ->
  Int ->
  Int ->
  Int ->
  Int ->
  TestTree
matrixFixture name field size columns rows expectedSize =
  testCase name $ do
    field @?= Matrix columns rows Float32
    size @?= expectedSize

matrixMarshallingFixtures :: TestTree
matrixMarshallingFixtures =
  testGroup
    "matrix marshalling"
    ( concat
        [ matrixStandards
            "2x2"
            (MatrixBuffer (V2 (V2 1 2) (V2 3 4)) :: HostFormat (MatrixBuffer 2 2 Float))
            (\standard -> bufferSizeFor standard (Proxy @(MatrixBuffer 2 2 Float)))
            (\standard -> pokeBufferFor standard (Proxy @(MatrixBuffer 2 2 Float)))
            (\standard -> peekBufferFor standard (Proxy @(MatrixBuffer 2 2 Float)))
            [0, 4, 16, 20]
            [0, 4, 8, 12]
            [1 .. 4]
        , matrixStandards
            "2x3"
            (MatrixBuffer (V2 (V3 1 2 3) (V3 4 5 6)) :: HostFormat (MatrixBuffer 2 3 Float))
            (\standard -> bufferSizeFor standard (Proxy @(MatrixBuffer 2 3 Float)))
            (\standard -> pokeBufferFor standard (Proxy @(MatrixBuffer 2 3 Float)))
            (\standard -> peekBufferFor standard (Proxy @(MatrixBuffer 2 3 Float)))
            [0, 4, 8, 16, 20, 24]
            [0, 4, 8, 16, 20, 24]
            [1 .. 6]
        , matrixStandards
            "2x4"
            (MatrixBuffer (V2 (V4 1 2 3 4) (V4 5 6 7 8)) :: HostFormat (MatrixBuffer 2 4 Float))
            (\standard -> bufferSizeFor standard (Proxy @(MatrixBuffer 2 4 Float)))
            (\standard -> pokeBufferFor standard (Proxy @(MatrixBuffer 2 4 Float)))
            (\standard -> peekBufferFor standard (Proxy @(MatrixBuffer 2 4 Float)))
            [0, 4, 8, 12, 16, 20, 24, 28]
            [0, 4, 8, 12, 16, 20, 24, 28]
            [1 .. 8]
        , matrixStandards
            "3x2"
            (MatrixBuffer (V3 (V2 1 2) (V2 3 4) (V2 5 6)) :: HostFormat (MatrixBuffer 3 2 Float))
            (\standard -> bufferSizeFor standard (Proxy @(MatrixBuffer 3 2 Float)))
            (\standard -> pokeBufferFor standard (Proxy @(MatrixBuffer 3 2 Float)))
            (\standard -> peekBufferFor standard (Proxy @(MatrixBuffer 3 2 Float)))
            [0, 4, 16, 20, 32, 36]
            [0, 4, 8, 12, 16, 20]
            [1 .. 6]
        , matrixStandards
            "3x3"
            (MatrixBuffer (V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9)) :: HostFormat (MatrixBuffer 3 3 Float))
            (\standard -> bufferSizeFor standard (Proxy @(MatrixBuffer 3 3 Float)))
            (\standard -> pokeBufferFor standard (Proxy @(MatrixBuffer 3 3 Float)))
            (\standard -> peekBufferFor standard (Proxy @(MatrixBuffer 3 3 Float)))
            [0, 4, 8, 16, 20, 24, 32, 36, 40]
            [0, 4, 8, 16, 20, 24, 32, 36, 40]
            [1 .. 9]
        , matrixStandards
            "3x4"
            ( MatrixBuffer (V3 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12)) ::
                HostFormat (MatrixBuffer 3 4 Float)
            )
            (\standard -> bufferSizeFor standard (Proxy @(MatrixBuffer 3 4 Float)))
            (\standard -> pokeBufferFor standard (Proxy @(MatrixBuffer 3 4 Float)))
            (\standard -> peekBufferFor standard (Proxy @(MatrixBuffer 3 4 Float)))
            [0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44]
            [0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44]
            [1 .. 12]
        , matrixStandards
            "4x2"
            ( MatrixBuffer (V4 (V2 1 2) (V2 3 4) (V2 5 6) (V2 7 8)) ::
                HostFormat (MatrixBuffer 4 2 Float)
            )
            (\standard -> bufferSizeFor standard (Proxy @(MatrixBuffer 4 2 Float)))
            (\standard -> pokeBufferFor standard (Proxy @(MatrixBuffer 4 2 Float)))
            (\standard -> peekBufferFor standard (Proxy @(MatrixBuffer 4 2 Float)))
            [0, 4, 16, 20, 32, 36, 48, 52]
            [0, 4, 8, 12, 16, 20, 24, 28]
            [1 .. 8]
        , matrixStandards
            "4x3"
            ( MatrixBuffer (V4 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9) (V3 10 11 12)) ::
                HostFormat (MatrixBuffer 4 3 Float)
            )
            (\standard -> bufferSizeFor standard (Proxy @(MatrixBuffer 4 3 Float)))
            (\standard -> pokeBufferFor standard (Proxy @(MatrixBuffer 4 3 Float)))
            (\standard -> peekBufferFor standard (Proxy @(MatrixBuffer 4 3 Float)))
            [0, 4, 8, 16, 20, 24, 32, 36, 40, 48, 52, 56]
            [0, 4, 8, 16, 20, 24, 32, 36, 40, 48, 52, 56]
            [1 .. 12]
        , matrixStandards
            "4x4"
            ( MatrixBuffer (V4 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) (V4 13 14 15 16)) ::
                HostFormat (MatrixBuffer 4 4 Float)
            )
            (\standard -> bufferSizeFor standard (Proxy @(MatrixBuffer 4 4 Float)))
            (\standard -> pokeBufferFor standard (Proxy @(MatrixBuffer 4 4 Float)))
            (\standard -> peekBufferFor standard (Proxy @(MatrixBuffer 4 4 Float)))
            [0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60]
            [0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60]
            [1 .. 16]
        ]
    )

matrixStandards ::
  (Eq host, Show host) =>
  String ->
  host ->
  (LayoutStandard -> Int) ->
  (LayoutStandard -> Ptr () -> host -> IO ()) ->
  (LayoutStandard -> Ptr () -> IO host) ->
  [Int] ->
  [Int] ->
  [Float] ->
  [TestTree]
matrixStandards name hostValue sizeFor pokeFor peekFor std140Offsets std430Offsets expectedValues =
  [ matrixMarshalCase (name <> " std140") Std140 std140Offsets
  , matrixMarshalCase (name <> " std430") Std430 std430Offsets
  ]
 where
  matrixMarshalCase caseName standard offsets = testCase caseName $ do
    memory <- mallocForeignPtrBytes (sizeFor standard)
    withForeignPtr memory $ \pointer -> do
      fillBytes pointer 0x7f (sizeFor standard)
      let matrixPointer = castPtr pointer
      pokeFor standard matrixPointer hostValue
      readFloats matrixPointer offsets >>= (@?= expectedValues)
      result <- peekFor standard matrixPointer
      result @?= hostValue

testStd140Vec2Array :: Assertion
testStd140Vec2Array = do
  let value = V2 (V2 1 2) (V2 3 4) :: HostFormat (V2 (V2 Float))
  bufferSizeFor Std140 (Proxy @(V2 (V2 Float))) @?= 32
  withMarshalled Std140 (Proxy @(V2 (V2 Float))) value $ \pointer -> do
    readFloats pointer [0, 4, 16, 20] >>= (@?= [1, 2, 3, 4])
    result <- peekBufferFor Std140 (Proxy @(V2 (V2 Float))) pointer
    result @?= value

testStd430Vec3Array :: Assertion
testStd430Vec3Array = do
  let value = V2 (V3 1 2 3) (V3 4 5 6) :: HostFormat (V2 (V3 Float))
  bufferSizeFor Std430 (Proxy @(V2 (V3 Float))) @?= 32
  withMarshalled Std430 (Proxy @(V2 (V3 Float))) value $ \pointer -> do
    readFloats pointer [0, 4, 8, 16, 20, 24] >>= (@?= [1, 2, 3, 4, 5, 6])
    result <- peekBufferFor Std430 (Proxy @(V2 (V3 Float))) pointer
    result @?= value

testMatrixRoundTrip :: Assertion
testMatrixRoundTrip = do
  let columns = V2 (V2 1 2) (V2 3 4)
      value = MatrixBuffer columns :: HostFormat (MatrixBuffer 2 2 Float)
  bufferFieldLayout (Proxy @(MatrixBuffer 2 2 Float)) @?= Matrix 2 2 Float32
  bufferSizeFor Std140 (Proxy @(MatrixBuffer 2 2 Float)) @?= 32
  withMarshalled Std140 (Proxy @(MatrixBuffer 2 2 Float)) value $ \pointer -> do
    readFloats pointer [0, 4, 16, 20] >>= (@?= [1, 2, 3, 4])
    MatrixBuffer result <- peekBufferFor Std140 (Proxy @(MatrixBuffer 2 2 Float)) pointer
    result @?= columns

testNestedGenericStd140 :: Assertion
testNestedGenericStd140 = do
  let value = OuterBlock 1 (InnerBlock 2 (V2 3 4)) (V3 5 6 7) :: HostFormat OuterBlock
  layoutOf Std140 (bufferFieldLayout (Proxy @OuterBlock))
    @?= Layout 16 48 48 Nothing Nothing [0, 16, 32]
  withMarshalled Std140 (Proxy @OuterBlock) value $ \pointer -> do
    readFloats pointer [0, 16, 24, 28, 32, 36, 40] >>= (@?= [1, 2, 3, 4, 5, 6, 7])
    result <- peekBufferFor Std140 (Proxy @OuterBlock) pointer
    result @?= value

testHostFormatBoundary :: Assertion
testHostFormatBoundary = do
  let hostValue = TestVertex (V3 1 2 3) (V2 4 5) :: HostFormat (Generically TestVertex)
  withMarshalled Std430 (Proxy @(Generically TestVertex)) hostValue $ \pointer -> do
    result <- peekBufferFor Std430 (Proxy @(Generically TestVertex)) pointer
    result @?= hostValue

testGenericHostBoundary :: Assertion
testGenericHostBoundary = do
  let proxy = Proxy @(GenericHost GpuMaterial HostMaterial)
      hostValue = HostMaterial 1 (V2 2 3) :: HostFormat (GenericHost GpuMaterial HostMaterial)
  layoutOf Std430 (bufferFieldLayout proxy)
    @?= Layout 8 16 16 Nothing Nothing [0, 8]
  withMarshalled Std430 proxy hostValue $ \pointer -> do
    readFloats pointer [0, 8, 12] >>= (@?= [1, 2, 3])
    result <- peekBufferFor Std430 proxy pointer
    result @?= hostValue

withMarshalled ::
  forall representation result.
  (BufferFormat representation) =>
  LayoutStandard ->
  Proxy representation ->
  HostFormat representation ->
  (Ptr () -> IO result) ->
  IO result
withMarshalled standard proxy value action = do
  memory <- mallocForeignPtrBytes (bufferSizeFor standard proxy)
  withForeignPtr memory $ \pointer -> do
    fillBytes pointer 0x7f (bufferSizeFor standard proxy)
    let typedPointer = castPtr pointer
    pokeBufferFor standard proxy typedPointer value
    action typedPointer

referenceAggregateLayout :: IO BL8.ByteString
referenceAggregateLayout =
  pure
    ( BL8.pack
        ( unlines
            [ "Reference aggregate field layout"
            , "source=test/golden/format/reference-aggregate.comp"
            , "compiler=glslangValidator 16.3.0"
            , "command=glslangValidator -V --target-env vulkan1.3 -S comp reference-aggregate.comp"
            , "SPIR-V evidence: std140 offsets=[0,16,48,96], matrix stride=16; std430 offsets=[0,16,48,72], matrix stride=8"
            , renderFieldLayout referenceAggregateField
            , ""
            , "Vertex"
            , renderLayout (layoutOf Vertex referenceAggregateField)
            , ""
            , "Std140"
            , renderLayout (layoutOf Std140 referenceAggregateField)
            , ""
            , "Std430"
            , renderLayout (layoutOf Std430 referenceAggregateField)
            ]
        )
    )

referenceAggregateField :: FieldLayout
referenceAggregateField =
  Struct
    [ Scalar Float32
    , Array 2 (Vector 3 Float32)
    , Matrix 3 2 Float32
    , Struct [Scalar UnsignedInt32, Vector 2 Float32]
    ]

renderFieldLayout :: FieldLayout -> String
renderFieldLayout field = case field of
  Scalar scalar -> renderScalarType scalar
  Vector channels scalar -> "vec" <> show channels <> "<" <> renderScalarType scalar <> ">"
  Matrix columns rows scalar -> "mat" <> show columns <> "x" <> show rows <> "<" <> renderScalarType scalar <> ">"
  Array count element -> "array[" <> show count <> "]<" <> renderFieldLayout element <> ">"
  Struct fields -> "struct<" <> joinWith ", " (fmap renderFieldLayout fields) <> ">"

renderScalarType :: ScalarType -> String
renderScalarType scalar = case scalar of
  Float32 -> "float32"
  SignedInt32 -> "int32"
  UnsignedInt32 -> "uint32"
  Boolean32 -> "bool32"

renderLayout :: Layout -> String
renderLayout layout =
  "alignment="
    <> show (layoutAlignment layout)
    <> ", occupied="
    <> show (layoutOccupiedSize layout)
    <> ", size="
    <> show (layoutSize layout)
    <> ", array stride="
    <> renderOptionalInt (layoutStride layout)
    <> ", matrix stride="
    <> renderOptionalInt (layoutMatrixStride layout)
    <> ", field offsets=["
    <> joinWith ", " (fmap show (layoutFieldOffsets layout))
    <> "]"

renderOptionalInt :: Maybe Int -> String
renderOptionalInt = maybe "none" show

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [value] = value
joinWith separator (value : values) = value <> separator <> joinWith separator values

readFloats :: Ptr a -> [Int] -> IO [Float]
readFloats pointer = traverse (peekByteOff pointer)

newtype NestedLayout = NestedLayout FieldLayout
  deriving stock (Show)

instance Arbitrary NestedLayout where
  arbitrary = NestedLayout <$> sized arbitraryLayout
   where
    arbitraryLayout 0 = scalarOrVector
    arbitraryLayout size = do
      choice <- chooseInt (0, 3)
      case choice of
        0 -> scalarOrVector
        1 -> Array <$> chooseInt (0, 4) <*> arbitraryLayout (size `div` 2)
        2 -> Matrix <$> chooseInt (2, 4) <*> chooseInt (2, 4) <*> pure Float32
        _ -> Struct <$> vectorOf 2 (arbitraryLayout (size `div` 2))
    scalarOrVector = do
      channels <- chooseInt (1, 4)
      pure (Vector channels Float32)

propWellFormedLayouts :: NestedLayout -> Property
propWellFormedLayouts (NestedLayout field) =
  counterexample (show field) $
    all (wellFormed field) [Vertex, Std140, Std430]
 where
  wellFormed layoutField standard =
    let layout = layoutOf standard layoutField
     in layoutSize layout `mod` layoutAlignment layout == 0
          && membersAreWellFormed layoutField standard
  membersAreWellFormed layoutField standard = case layoutField of
    Struct fields ->
      let offsets = layoutFieldOffsets (layoutOf standard layoutField)
          memberLayouts = fmap (layoutOf standard) fields
          aligned = and (zipWith (\offset member -> offset `mod` layoutAlignment member == 0) offsets memberLayouts)
          nonOverlapping =
            and
              ( zipWith
                  (\(offset, member) nextOffset -> offset + layoutOccupiedSize member <= nextOffset)
                  (zip offsets memberLayouts)
                  (drop 1 offsets)
              )
       in aligned && nonOverlapping && all (`membersAreWellFormed` standard) fields
    Array elementCount member ->
      let arrayLayout = layoutOf standard layoutField
          memberLayout = layoutOf standard member
       in case layoutStride arrayLayout of
            Nothing -> False
            Just stride ->
              stride `mod` layoutAlignment arrayLayout == 0
                && stride >= layoutSize memberLayout
                && layoutSize arrayLayout == elementCount * stride
                && membersAreWellFormed member standard
    Matrix columns rows scalar ->
      let matrixLayout = layoutOf standard layoutField
          columnLayout = layoutOf standard (Vector rows scalar)
       in case layoutMatrixStride matrixLayout of
            Nothing -> False
            Just stride ->
              stride `mod` layoutAlignment matrixLayout == 0
                && stride >= layoutSize columnLayout
                && layoutSize matrixLayout == columns * stride
    _ -> True

propRoundTripVertex :: Int -> Int -> Int -> Int -> Int -> Property
propRoundTripVertex x y z u v =
  let vertex =
        TestVertex
          (V3 (fromIntegral x) (fromIntegral y) (fromIntegral z))
          (V2 (fromIntegral u) (fromIntegral v)) ::
          HostFormat TestVertex
   in ioProperty $ do
        withMarshalled Std430 (Proxy @TestVertex) vertex $ \pointer -> do
          result <- peekBuffer (Proxy @TestVertex) pointer
          pure (result == vertex)

data MarshalledValue where
  MarshalledValue ::
    (BufferFormat representation, Eq (HostFormat representation), Show (HostFormat representation)) =>
    String ->
    LayoutStandard ->
    Proxy representation ->
    HostFormat representation ->
    MarshalledValue

instance Show MarshalledValue where
  show (MarshalledValue shape standard _ value) = shape <> " under " <> show standard <> ": " <> show value

instance Arbitrary MarshalledValue where
  arbitrary = do
    standard <- elements [Vertex, Std140, Std430]
    shape <- chooseInt (0, 8)
    case shape of
      0 -> MarshalledValue "float" standard (Proxy @Float) <$> randomFloat
      1 -> MarshalledValue "int32" standard (Proxy @Int32) . fromIntegral <$> chooseInt (-1000, 1000)
      2 -> MarshalledValue "uint32" standard (Proxy @Word32) . fromIntegral <$> chooseInt (0, 1000)
      3 -> MarshalledValue "bool" standard (Proxy @Bool) <$> elements [False, True]
      4 -> MarshalledValue "vec4<float>" standard (Proxy @(V4 Float)) <$> randomV4
      5 -> MarshalledValue "vec2<vec3<float>>" standard (Proxy @(V2 (V3 Float))) <$> (V2 <$> randomV3 <*> randomV3)
      6 -> MarshalledValue "mat3x2<float>" standard (Proxy @(MatrixBuffer 3 2 Float)) . MatrixBuffer <$> (V3 <$> randomV2 <*> randomV2 <*> randomV2)
      7 -> MarshalledValue "generic vertex" standard (Proxy @TestVertex) <$> (TestVertex <$> randomV3 <*> randomV2)
      _ -> MarshalledValue "nested generic record" standard (Proxy @OuterBlock) <$> (OuterBlock <$> randomFloat <*> (InnerBlock <$> randomFloat <*> randomV2) <*> randomV3)
   where
    randomFloat = fromIntegral <$> chooseInt (-1000, 1000)
    randomV2 = V2 <$> randomFloat <*> randomFloat
    randomV3 = V3 <$> randomFloat <*> randomFloat <*> randomFloat
    randomV4 = V4 <$> randomFloat <*> randomFloat <*> randomFloat <*> randomFloat

propRoundTripSupportedFormats :: MarshalledValue -> Property
propRoundTripSupportedFormats marshalled =
  counterexample (show marshalled) . ioProperty $
    case marshalled of
      MarshalledValue _ standard proxy expected ->
        withMarshalled standard proxy expected $ \pointer -> do
          actual <- peekBufferFor standard proxy pointer
          pure (actual == expected)
