{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Vpipe.ExprTest (exprTests) where

import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Word (Word32)
import GHC.Clock (getMonotonicTimeNSec)
import Linear (M22, M23, M24, M32, M33, M34, M42, M43, M44, V2 (..), V3 (..), V4 (..))
import Linear qualified as L
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, Property, counterexample, frequency, sized, testProperty, (===))
import Vpipe.Expr
import Vpipe.Expr.Eval
import Vpipe.Expr.Internal (BinaryOp (..), BinderId (..), HostValue (..), SamplingKind (..), SamplingMode (..), ShaderTy (..), UnaryOp (..))
import Vpipe.Expr.Internal qualified as I
import Vpipe.Expr.Reify
import Vpipe.Format (Format (D32Sfloat, R32Sfloat))
import Vpipe.Image.Types (Dim (D2))

exprTests :: TestTree
exprTests =
  testGroup
    "expressions"
    [ testProperty "scalar addition agrees with its host operation" propScalarAddition
    , testProperty "scalar multiplication agrees with its host operation" propScalarMultiplication
    , testProperty "float division agrees with its host operation" propFloatDivision
    , testProperty "recursive well-typed scalar trees agree with a host oracle" propRecursiveScalarTree
    , testCase "M2 vertex and fragment expressions compile and reify" m2ExpressionCase
    , testCase "evaluates vector construction and scalar swizzles" vectorConstructionCase
    , testCase "evaluates the xy swizzle without losing its dimension" xySwizzleCase
    , testCase "evaluates vector standard-library operations" vectorLibraryCase
    , testCase "evaluates clamp, mix, and smoothstep" interpolationCase
    , testCase "evaluates component-wise vector and matrix arithmetic" compositeArithmeticCase
    , testCase "evaluates matrix multiplication" matrixMultiplicationCase
    , testCase "evaluates dimension-correct matrix-vector multiplication" matrixVectorCase
    , matrixProductCoverage
    , matrixVectorCoverage
    , testCase "round-trips rectangular matrix constants" rectangularMatrixCase
    , testCase "round-trips every supported matrix shape" allMatrixShapesCase
    , testCase "round-trips and evaluates unsigned scalars and vectors" unsignedValueCase
    , testCase "reifies one shared small diamond to four nodes" smallSharingCase
    , testCase "reified graph edges point at the one shared node" sharedEdgesCase
    , testCase "reifies a thirty-level shared diamond to thirty-one nodes" deepSharingCase
    , testCase "assigns deterministic post-order IDs" deterministicIdsCase
    , testCase "represents branch arms as explicit regions" branchRegionsCase
    , testCase "evaluates nested loops without binder capture" nestedLoopCase
    , testCase "loop binders cannot capture an input with a similar name" inputCaptureCase
    , testCase "reified nested loops have distinct binders" nestedLoopBinderCase
    , testCase "reports loop fuel exhaustion" loopFuelCase
    , testCase "does not evaluate the unselected branch" lazyBranchCase
    , testCase "select evaluates both value operands" eagerSelectCase
    , testCase "reports a missing input" missingInputCase
    , testCase "reports an input type mismatch" inputTypeMismatchCase
    , testCase "float division and reciprocal by zero follow IEEE host semantics" floatDivisionByZeroCase
    , testCase "malformed integer division is rejected without overflowing" integerDivisionCase
    , testCase "reports derivative evaluation as unsupported" derivativeErrorCase
    , testCase "reports sampling evaluation as unsupported" samplingErrorCase
    , testCase "reports direct resource and local evaluation as unsupported" internalUnsupportedCase
    , testCase "malformed internal extraction returns an error instead of crashing" malformedExtractionCase
    , testCase "retains implicit sample mode without a LOD edge" implicitLodCase
    , testCase "retains explicit sample LOD as a graph edge" explicitLodCase
    , testCase "different explicit LOD expressions produce different sample graphs" differingLodCase
    , testCase "projects sampled texels to the format-derived shader type" typedSamplingCase
    , testCase "reifies depth-comparison sampling as a scalar operation" comparisonSamplingCase
    ]

propScalarAddition :: Int -> Int -> Property
propScalarAddition left right =
  let leftValue = fromIntegral (left `mod` 10000) :: Float
      rightValue = fromIntegral (right `mod` 10000) :: Float
      expression = constant leftValue + constant rightValue :: V Float
   in evalExpr expression === Right (leftValue + rightValue)

propScalarMultiplication :: Int -> Int -> Property
propScalarMultiplication left right =
  let leftValue = fromIntegral (left `mod` 1000) :: Float
      rightValue = fromIntegral (right `mod` 1000) :: Float
      expression = constant leftValue * constant rightValue :: C Float
   in evalExpr expression === Right (leftValue * rightValue)

propFloatDivision :: Int -> Int -> Property
propFloatDivision numerator denominator =
  let numeratorValue = fromIntegral (numerator `mod` 1000) :: Float
      denominatorValue = fromIntegral (1 + abs (denominator `mod` 999)) :: Float
      expression = constant numeratorValue / constant denominatorValue :: C Float
   in evalExpr expression === Right (numeratorValue / denominatorValue)

data ScalarTree
  = ScalarLiteral Int
  | ScalarAdd ScalarTree ScalarTree
  | ScalarSubtract ScalarTree ScalarTree
  | ScalarMultiply ScalarTree ScalarTree
  | ScalarNegate ScalarTree
  | ScalarAbsolute ScalarTree
  deriving stock (Show)

instance Arbitrary ScalarTree where
  arbitrary = sized (generateScalarTree . min 5)
  shrink tree = case tree of
    ScalarLiteral value -> ScalarLiteral <$> shrink value
    ScalarAdd left right -> [left, right] <> [ScalarAdd left' right | left' <- shrink left] <> [ScalarAdd left right' | right' <- shrink right]
    ScalarSubtract left right -> [left, right] <> [ScalarSubtract left' right | left' <- shrink left] <> [ScalarSubtract left right' | right' <- shrink right]
    ScalarMultiply left right -> [left, right] <> [ScalarMultiply left' right | left' <- shrink left] <> [ScalarMultiply left right' | right' <- shrink right]
    ScalarNegate value -> value : fmap ScalarNegate (shrink value)
    ScalarAbsolute value -> value : fmap ScalarAbsolute (shrink value)

generateScalarTree :: Int -> Gen ScalarTree
generateScalarTree depth
  | depth <= 0 = ScalarLiteral <$> arbitrary
  | otherwise =
      frequency
        [ (3, ScalarLiteral <$> arbitrary)
        , (2, binary ScalarAdd)
        , (2, binary ScalarSubtract)
        , (2, binary ScalarMultiply)
        , (1, ScalarNegate <$> child)
        , (1, ScalarAbsolute <$> child)
        ]
 where
  child = generateScalarTree (depth - 1)
  binary constructor = constructor <$> child <*> child

propRecursiveScalarTree :: ScalarTree -> Property
propRecursiveScalarTree tree =
  counterexample ("tree: " <> show tree) $
    evalExpr (scalarTreeExpression tree) === Right (scalarTreeHost tree)

scalarTreeExpression :: ScalarTree -> C Float
scalarTreeExpression tree = case tree of
  ScalarLiteral value -> constant (boundedLiteral value)
  ScalarAdd left right -> scalarTreeExpression left + scalarTreeExpression right
  ScalarSubtract left right -> scalarTreeExpression left - scalarTreeExpression right
  ScalarMultiply left right -> scalarTreeExpression left * scalarTreeExpression right
  ScalarNegate value -> negate (scalarTreeExpression value)
  ScalarAbsolute value -> abs (scalarTreeExpression value)

scalarTreeHost :: ScalarTree -> Float
scalarTreeHost tree = case tree of
  ScalarLiteral value -> boundedLiteral value
  ScalarAdd left right -> scalarTreeHost left + scalarTreeHost right
  ScalarSubtract left right -> scalarTreeHost left - scalarTreeHost right
  ScalarMultiply left right -> scalarTreeHost left * scalarTreeHost right
  ScalarNegate value -> negate (scalarTreeHost value)
  ScalarAbsolute value -> abs (scalarTreeHost value)

boundedLiteral :: Int -> Float
boundedLiteral value = fromIntegral (value `mod` 9 - 4)

m2ExpressionCase :: IO ()
m2ExpressionCase = do
  vertexGraph <- reifyExpr m2VertexExpression
  fragmentGraph <- reifyExpr m2FragmentExpression
  reifiedTy <$> lookupNode vertexGraph (reifiedRoot vertexGraph) @?= Just (TyVector 4)
  reifiedTy <$> lookupNode fragmentGraph (reifiedRoot fragmentGraph) @?= Just (TyVector 4)

vectorConstructionCase :: IO ()
vectorConstructionCase = do
  let vector = vec4 (constant 1) (constant 2) (constant 3) (constant 4) :: V (V4 Float)
  evalExpr (_x vector) @?= Right 1
  evalExpr (_xy vector) @?= Right (V2 1 2)
  evalExpr (x vector) @?= Right 1
  evalExpr (y vector) @?= Right 2
  evalExpr (z vector) @?= Right 3
  evalExpr (w vector) @?= Right 4

xySwizzleCase :: IO ()
xySwizzleCase =
  evalExpr (xy (constant (V3 3 4 5 :: V3 Float)) :: C (V2 Float)) @?= Right (V2 3 4)

vectorLibraryCase :: IO ()
vectorLibraryCase = do
  evalExpr (dot (constant (V3 1 2 3 :: V3 Float)) (constant (V3 4 5 6 :: V3 Float)) :: V Float) @?= Right 32
  evalExpr (cross (constant (V3 1 0 0)) (constant (V3 0 1 0)) :: V (V3 Float)) @?= Right (V3 0 0 1)
  evalExpr (normalize (constant (V2 3 4)) :: C (V2 Float)) @?= Right (V2 0.6 0.8)
  evalExpr (reflect (constant (V2 1 (-1))) (constant (V2 0 1)) :: C (V2 Float)) @?= Right (V2 1 1)

interpolationCase :: IO ()
interpolationCase = do
  evalExpr (clamp (constant 4) (constant 0) (constant 2) :: C Float) @?= Right 2
  evalExpr (mix (constant (V2 0 2)) (constant (V2 2 4)) (constant 0.25) :: V (V2 Float)) @?= Right (V2 0.5 2.5)
  evalExpr (smoothstep (constant 0) (constant 1) (constant 0.5) :: C Float) @?= Right 0.5

compositeArithmeticCase :: IO ()
compositeArithmeticCase = do
  evalExpr (constant (V3 1 2 3) + constant (V3 4 5 6) :: V (V3 Float)) @?= Right (V3 5 7 9)
  let left = V2 (V2 1 2) (V2 3 4) :: M22 Float
      right = V2 (V2 2 3) (V2 4 5) :: M22 Float
  evalExpr (constant left * constant right :: C (M22 Float)) @?= Right (V2 (V2 2 6) (V2 12 20))

matrixMultiplicationCase :: IO ()
matrixMultiplicationCase = do
  let left = V2 (V2 1 2) (V2 3 4) :: M22 Float
      right = V2 (V2 5 6) (V2 7 8) :: M22 Float
      expected = V2 (V2 19 22) (V2 43 50)
  evalExpr (matrixMultiply (constant left) (constant right) :: V (M22 Float)) @?= Right expected

matrixVectorCase :: IO ()
matrixVectorCase = do
  let matrix = V2 (V3 1 2 3) (V3 4 5 6) :: M23 Float
  evalExpr (matrixVectorMultiply (constant matrix) (constant (V3 1 0 1 :: V3 Float)) :: C (V2 Float)) @?= Right (V2 4 10)

rectangularMatrixCase :: IO ()
rectangularMatrixCase = do
  let value = V3 (V2 1 2) (V2 3 4) (V2 5 6) :: M32 Float
  evalExpr (constant value :: V (M32 Float)) @?= Right value

allMatrixShapesCase :: IO ()
allMatrixShapesCase = do
  roundTrip (V2 (V2 1 2) (V2 3 4) :: M22 Float)
  roundTrip (V2 (V3 1 2 3) (V3 4 5 6) :: M23 Float)
  roundTrip (V2 (V4 1 2 3 4) (V4 5 6 7 8) :: M24 Float)
  roundTrip (V3 (V2 1 2) (V2 3 4) (V2 5 6) :: M32 Float)
  roundTrip (V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9) :: M33 Float)
  roundTrip (V3 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) :: M34 Float)
  roundTrip (V4 (V2 1 2) (V2 3 4) (V2 5 6) (V2 7 8) :: M42 Float)
  roundTrip (V4 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9) (V3 10 11 12) :: M43 Float)
  roundTrip (V4 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) (V4 13 14 15 16) :: M44 Float)
 where
  roundTrip :: (Eq a, Show a, ShaderValue a) => a -> IO ()
  roundTrip value = evalExpr (constant value) @?= Right value

unsignedValueCase :: IO ()
unsignedValueCase = do
  let scalar = maxBound :: Word32
      vector = V3 1 2 maxBound :: V3 Word32
      first = I.extract TyWord [0] (constant vector :: C (V3 Word32)) :: C Word32
  evalExpr (constant scalar :: V Word32) @?= Right scalar
  evalExpr ((constant scalar :: V Word32) + constant 2) @?= Right 1
  evalExpr ((constant 2 :: V Word32) - constant 3) @?= Right maxBound
  evalExpr ((constant 7 :: V Word32) * constant 6) @?= Right 42
  evalExpr (negate (constant 1 :: V Word32)) @?= Right maxBound
  evalExpr (abs (constant scalar :: V Word32)) @?= Right scalar
  evalExpr (signum (constant 9 :: V Word32)) @?= Right 1
  evalExpr (constant (V2 1 2 :: V2 Word32)) @?= Right (V2 1 2)
  evalExpr (constant vector :: V (V3 Word32)) @?= Right vector
  evalExpr (constant (V4 1 2 3 4 :: V4 Word32)) @?= Right (V4 1 2 3 4)
  evalExpr first @?= Right 1
  evalExpr (wordX (constant vector :: V (V3 Word32))) @?= Right 1
  evalExpr ((constant vector :: V (V3 Word32)) ==. constant vector) @?= Right True
  evalExpr ((constant scalar :: V Word32) >. constant 1) @?= Right True

data MatrixProductCase
  = forall left right result.
    (MatrixProduct left right result, Eq result, Show result) =>
    MatrixProductCase String left right result

matrixProductCoverage :: TestTree
matrixProductCoverage =
  testGroup "all 27 matrix products" (map runMatrixProduct matrixProductCases)

runMatrixProduct :: MatrixProductCase -> TestTree
runMatrixProduct (MatrixProductCase name left right expected) =
  testCase name $ evalExpr (constant left !*! constant right) @?= Right expected

matrixProductCases :: [MatrixProductCase]
matrixProductCases =
  [ MatrixProductCase "M22 * M22 -> M22" matrix22 matrix22 (matrix22 L.!*! matrix22)
  , MatrixProductCase "M22 * M23 -> M23" matrix22 matrix23 (matrix22 L.!*! matrix23)
  , MatrixProductCase "M22 * M24 -> M24" matrix22 matrix24 (matrix22 L.!*! matrix24)
  , MatrixProductCase "M23 * M32 -> M22" matrix23 matrix32 (matrix23 L.!*! matrix32)
  , MatrixProductCase "M23 * M33 -> M23" matrix23 matrix33 (matrix23 L.!*! matrix33)
  , MatrixProductCase "M23 * M34 -> M24" matrix23 matrix34 (matrix23 L.!*! matrix34)
  , MatrixProductCase "M24 * M42 -> M22" matrix24 matrix42 (matrix24 L.!*! matrix42)
  , MatrixProductCase "M24 * M43 -> M23" matrix24 matrix43 (matrix24 L.!*! matrix43)
  , MatrixProductCase "M24 * M44 -> M24" matrix24 matrix44 (matrix24 L.!*! matrix44)
  , MatrixProductCase "M32 * M22 -> M32" matrix32 matrix22 (matrix32 L.!*! matrix22)
  , MatrixProductCase "M32 * M23 -> M33" matrix32 matrix23 (matrix32 L.!*! matrix23)
  , MatrixProductCase "M32 * M24 -> M34" matrix32 matrix24 (matrix32 L.!*! matrix24)
  , MatrixProductCase "M33 * M32 -> M32" matrix33 matrix32 (matrix33 L.!*! matrix32)
  , MatrixProductCase "M33 * M33 -> M33" matrix33 matrix33 (matrix33 L.!*! matrix33)
  , MatrixProductCase "M33 * M34 -> M34" matrix33 matrix34 (matrix33 L.!*! matrix34)
  , MatrixProductCase "M34 * M42 -> M32" matrix34 matrix42 (matrix34 L.!*! matrix42)
  , MatrixProductCase "M34 * M43 -> M33" matrix34 matrix43 (matrix34 L.!*! matrix43)
  , MatrixProductCase "M34 * M44 -> M34" matrix34 matrix44 (matrix34 L.!*! matrix44)
  , MatrixProductCase "M42 * M22 -> M42" matrix42 matrix22 (matrix42 L.!*! matrix22)
  , MatrixProductCase "M42 * M23 -> M43" matrix42 matrix23 (matrix42 L.!*! matrix23)
  , MatrixProductCase "M42 * M24 -> M44" matrix42 matrix24 (matrix42 L.!*! matrix24)
  , MatrixProductCase "M43 * M32 -> M42" matrix43 matrix32 (matrix43 L.!*! matrix32)
  , MatrixProductCase "M43 * M33 -> M43" matrix43 matrix33 (matrix43 L.!*! matrix33)
  , MatrixProductCase "M43 * M34 -> M44" matrix43 matrix34 (matrix43 L.!*! matrix34)
  , MatrixProductCase "M44 * M42 -> M42" matrix44 matrix42 (matrix44 L.!*! matrix42)
  , MatrixProductCase "M44 * M43 -> M43" matrix44 matrix43 (matrix44 L.!*! matrix43)
  , MatrixProductCase "M44 * M44 -> M44" matrix44 matrix44 (matrix44 L.!*! matrix44)
  ]

data MatrixVectorCase
  = forall matrix vector result.
    (MatrixVectorProduct matrix vector result, Eq result, Show result) =>
    MatrixVectorCase String matrix vector result

matrixVectorCoverage :: TestTree
matrixVectorCoverage =
  testGroup "all 9 matrix-vector products" (map runMatrixVector matrixVectorCases)

runMatrixVector :: MatrixVectorCase -> TestTree
runMatrixVector (MatrixVectorCase name matrix vector expected) =
  testCase name $ evalExpr (constant matrix !* constant vector) @?= Right expected

matrixVectorCases :: [MatrixVectorCase]
matrixVectorCases =
  [ MatrixVectorCase "M22 * V2 -> V2" matrix22 vector2 (matrix22 L.!* vector2)
  , MatrixVectorCase "M23 * V3 -> V2" matrix23 vector3 (matrix23 L.!* vector3)
  , MatrixVectorCase "M24 * V4 -> V2" matrix24 vector4 (matrix24 L.!* vector4)
  , MatrixVectorCase "M32 * V2 -> V3" matrix32 vector2 (matrix32 L.!* vector2)
  , MatrixVectorCase "M33 * V3 -> V3" matrix33 vector3 (matrix33 L.!* vector3)
  , MatrixVectorCase "M34 * V4 -> V3" matrix34 vector4 (matrix34 L.!* vector4)
  , MatrixVectorCase "M42 * V2 -> V4" matrix42 vector2 (matrix42 L.!* vector2)
  , MatrixVectorCase "M43 * V3 -> V4" matrix43 vector3 (matrix43 L.!* vector3)
  , MatrixVectorCase "M44 * V4 -> V4" matrix44 vector4 (matrix44 L.!* vector4)
  ]

matrix22 :: M22 Float
matrix22 = V2 (V2 1 2) (V2 3 4)

matrix23 :: M23 Float
matrix23 = V2 (V3 1 2 3) (V3 4 5 6)

matrix24 :: M24 Float
matrix24 = V2 (V4 1 2 3 4) (V4 5 6 7 8)

matrix32 :: M32 Float
matrix32 = V3 (V2 1 2) (V2 3 4) (V2 5 6)

matrix33 :: M33 Float
matrix33 = V3 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9)

matrix34 :: M34 Float
matrix34 = V3 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12)

matrix42 :: M42 Float
matrix42 = V4 (V2 1 2) (V2 3 4) (V2 5 6) (V2 7 8)

matrix43 :: M43 Float
matrix43 = V4 (V3 1 2 3) (V3 4 5 6) (V3 7 8 9) (V3 10 11 12)

matrix44 :: M44 Float
matrix44 = V4 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) (V4 13 14 15 16)

vector2 :: V2 Float
vector2 = V2 2 3

vector3 :: V3 Float
vector3 = V3 2 3 4

vector4 :: V4 Float
vector4 = V4 2 3 4 5

smallSharingCase :: IO ()
smallSharingCase = do
  graph <- reifyExpr smallDiamond
  length (reifiedNodes graph) @?= 4
  reifiedRoot graph @?= NodeId 3

sharedEdgesCase :: IO ()
sharedEdgesCase = do
  graph <- reifyExpr smallDiamond
  case lookupNode graph (reifiedRoot graph) of
    Just (ReifiedNode _ TyFloat (RBinary AddE left right)) -> do
      left @?= right
      left @?= NodeId 2
    other -> assertFailure ("unexpected shared root: " <> show other)

deepSharingCase :: IO ()
deepSharingCase = do
  started <- getMonotonicTimeNSec
  graph <- reifyExpr (diamond 30 :: V Float)
  finished <- getMonotonicTimeNSec
  length (reifiedNodes graph) @?= 31
  let elapsedMilliseconds = fromIntegral (finished - started) / 1_000_000 :: Double
  assertBool
    ("thirty-level shared expression took " <> show elapsedMilliseconds <> "ms")
    (elapsedMilliseconds < 250)

deterministicIdsCase :: IO ()
deterministicIdsCase = do
  first <- reifyExpr smallDiamond
  second <- reifyExpr smallDiamond
  map reifiedId (reifiedNodes first) @?= map NodeId [0 .. 3]
  first @?= second

branchRegionsCase :: IO ()
branchRegionsCase = do
  graph <- reifyExpr (ifThenElseE (constant True) (constant 1) (constant 2) :: F Float)
  length (reifiedRegions graph) @?= 2
  case lookupNode graph (reifiedRoot graph) of
    Just (ReifiedNode _ TyFloat (RBranch _ yesRegion noRegion)) -> assertBool "branch regions differ" (yesRegion /= noRegion)
    other -> assertFailure ("unexpected branch root: " <> show other)

nestedLoopCase :: IO ()
nestedLoopCase = do
  let inner = whileE (\value -> value <. constant 2) (+ 1) (constant 0) :: C Float
      outer = whileE (\value -> value <. constant 5) (+ inner) (constant 0) :: C Float
  evalExpr outer @?= Right 6

inputCaptureCase :: IO ()
inputCaptureCase = do
  let threshold = input "$loop" :: C Float
      expression = whileE (<. threshold) (+ 1) (constant 0) :: C Float
      environment = bindInput "$loop" (3 :: Float) emptyEnv
  evalExprWith environment expression @?= Right 3

nestedLoopBinderCase :: IO ()
nestedLoopBinderCase = do
  let inner = whileE (\value -> value <. constant 1) (+ 1) (constant 0) :: C Float
      outer = whileE (\value -> value <. constant 1) (+ inner) (constant 0) :: C Float
  graph <- reifyExpr outer
  let binders = [binder | ReifiedNode _ _ (RWhile _ binder _ _) <- reifiedNodes graph]
  case binders of
    [first, second] -> assertBool "nested loop binders differ" (first /= second)
    _ -> assertFailure ("expected two loop binders, got " <> show binders)

loopFuelCase :: IO ()
loopFuelCase =
  evalExprWithFuel 4 emptyEnv (whileE (const (constant True)) id (constant 0) :: C Float) @?= Left LoopFuelExhausted

lazyBranchCase :: IO ()
lazyBranchCase =
  evalExpr (ifThenElseE (constant True) (constant 7) (input "unselected") :: F Float) @?= Right 7

eagerSelectCase :: IO ()
eagerSelectCase =
  evalExpr (ifE (constant True) (constant 7) (input "eager operand") :: F Float) @?= Left (MissingVariable "eager operand")

missingInputCase :: IO ()
missingInputCase =
  evalExpr (input "position" :: V Float) @?= Left (MissingVariable "position")

inputTypeMismatchCase :: IO ()
inputTypeMismatchCase =
  evalExprWith (Map.singleton "position" (HBool True)) (input "position" :: V Float)
    @?= Left (TypeMismatch TyFloat (HBool True))

floatDivisionByZeroCase :: IO ()
floatDivisionByZeroCase = do
  let divided = evalExpr (constant 1 / constant 0 :: C Float)
      reciprocal = evalExpr (recip (constant 0) :: C Float)
      indeterminate = evalExpr (constant 0 / constant 0 :: C Float)
  case (divided, reciprocal, indeterminate) of
    (Right dividedValue, Right reciprocalValue, Right indeterminateValue) -> do
      assertBool "division produces positive infinity" (isInfinite dividedValue && dividedValue > 0)
      assertBool "reciprocal produces positive infinity" (isInfinite reciprocalValue && reciprocalValue > 0)
      assertBool "zero divided by zero produces NaN" (isNaN indeterminateValue)
    unexpected -> assertFailure ("unexpected zero-division results: " <> show unexpected)

integerDivisionCase :: IO ()
integerDivisionCase = do
  let left = I.literal (minBound :: Int32) :: C Int32
      right = I.literal (-1 :: Int32) :: C Int32
      malformed = I.Expr (I.ExprObject TyInt (I.BinaryNode I.DivideE (I.someExpr left) (I.someExpr right))) :: C Int32
  evalExpr malformed @?= Left (InvalidOperation "integer divide" [HInt minBound, HInt (-1)])

derivativeErrorCase :: IO ()
derivativeErrorCase =
  evalExpr (dFdx (constant 1) :: F Float) @?= Left (UnsupportedOperation (DerivativeEvaluation DfdxE))

samplingErrorCase :: IO ()
samplingErrorCase =
  evalExpr (sample (sampler2D "texture") (constant (V2 0 0)) :: F (V4 Float))
    @?= Left (UnsupportedOperation (SamplingEvaluation ImplicitLod))

internalUnsupportedCase :: IO ()
internalUnsupportedCase = do
  evalExpr (I.resource TyImage2D "image" :: V Float)
    @?= Left (UnsupportedOperation (ResourceEvaluation TyImage2D "image"))
  evalExpr (I.local TyFloat (BinderId 7) :: V Float)
    @?= Left (UnsupportedOperation (LocalEvaluation (BinderId 7)))

malformedExtractionCase :: IO ()
malformedExtractionCase = do
  let vector = constant (V2 1 2) :: V (V2 Float)
      malformed = I.Expr (I.ExprObject TyFloat (I.ExtractNode [4] (I.someExpr vector))) :: V Float
  evalExpr malformed @?= Left (InvalidOperation "vector index out of bounds" [])

implicitLodCase :: IO ()
implicitLodCase = do
  graph <- reifyExpr (sample (sampler2D "texture") (constant (V2 0 0)) :: F (V4 Float))
  case lookupNode graph (reifiedRoot graph) of
    Just (ReifiedNode _ (TyVector 4) (RSample RegularSample ImplicitLod _ _ _ Nothing Nothing)) -> pure ()
    other -> assertFailure ("unexpected implicit sample root: " <> show other)

explicitLodCase :: IO ()
explicitLodCase = do
  graph <- reifyExpr (sampleLod (sampler2D "texture") (constant (V2 0 0)) (constant 2) :: V (V4 Float))
  case lookupNode graph (reifiedRoot graph) of
    Just (ReifiedNode _ (TyVector 4) (RSample RegularSample ExplicitLod _ _ _ Nothing (Just lodId))) ->
      lookupNode graph lodId @?= Just (ReifiedNode lodId TyFloat (RLiteral (HFloat 2)))
    other -> assertFailure ("unexpected sample root: " <> show other)

differingLodCase :: IO ()
differingLodCase = do
  low <- reifyExpr (sampleLod (sampler2D "texture") (constant (V2 0 0)) (constant 0) :: V (V4 Float))
  high <- reifyExpr (sampleLod (sampler2D "texture") (constant (V2 0 0)) (constant 3) :: V (V4 Float))
  sampleLodLiteral low @?= Just 0
  sampleLodLiteral high @?= Just 3

typedSamplingCase :: IO ()
typedSamplingCase = do
  let image = imageResource "scalar.image" :: ImageResource 'D2 'R32Sfloat 'Vertex
      sampled = sampledImage image (sampler "scalar.sampler")
  graph <- reifyExpr (sampleLod sampled (constant (V2 0 0)) (constant 0) :: V Float)
  case lookupNode graph (reifiedRoot graph) of
    Just (ReifiedNode _ TyFloat (RExtract [0] sampledNode)) ->
      case lookupNode graph sampledNode of
        Just (ReifiedNode _ (TyVector 4) (RSample RegularSample _ _ _ _ Nothing _)) -> pure ()
        other -> assertFailure ("unexpected typed sample node: " <> show other)
    other -> assertFailure ("unexpected typed projection root: " <> show other)

comparisonSamplingCase :: IO ()
comparisonSamplingCase = do
  let image = imageResource "shadow.image" :: ImageResource 'D2 'D32Sfloat 'Fragment
      shadow = comparisonSampledImage image (comparisonSampler "shadow.sampler")
  graph <- reifyExpr (sampleCompare shadow (constant (V2 0.5 0.5)) (constant 0.25))
  case lookupNode graph (reifiedRoot graph) of
    Just (ReifiedNode _ TyFloat (RSample ComparisonSample ImplicitLod _ _ _ (Just referenceId) Nothing)) ->
      lookupNode graph referenceId @?= Just (ReifiedNode referenceId TyFloat (RLiteral (HFloat 0.25)))
    other -> assertFailure ("unexpected comparison sample root: " <> show other)

m2VertexExpression :: V (V4 Float)
m2VertexExpression =
  let position = input "position" :: V (V3 Float)
      transform = input "transform" :: V (M44 Float)
   in transform !* vec4 (_x position) (y position) (z position) (constant 1)

m2FragmentExpression :: F (V4 Float)
m2FragmentExpression =
  let coordinates = input "textureCoordinate" :: F (V2 Float)
   in sample (sampler2D "albedo") coordinates

smallDiamond :: V Float
smallDiamond =
  let shared = constant 1 + constant 2
   in shared + shared

diamond :: Int -> Expr s Float
diamond 0 = constant 1
diamond level =
  let shared = diamond (level - 1)
   in shared + shared

lookupNode :: ReifiedExpr -> NodeId -> Maybe ReifiedNode
lookupNode graph identifier = listToMaybe [node | node <- reifiedNodes graph, reifiedId node == identifier]

sampleLodLiteral :: ReifiedExpr -> Maybe Float
sampleLodLiteral graph = do
  ReifiedNode _ _ (RSample RegularSample ExplicitLod _ _ _ Nothing (Just lodId)) <- lookupNode graph (reifiedRoot graph)
  ReifiedNode _ TyFloat (RLiteral (HFloat value)) <- lookupNode graph lodId
  pure value
