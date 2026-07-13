{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}

{- | Untyped, shader-type-tagged expression nodes and their checked builders.
The public module deliberately does not export the constructors in here.
-}
module Vpipe.Expr.Internal (
  Stage (..),
  ImageDimension (..),
  ShaderTy (..),
  HostValue (..),
  BinderId (..),
  Expr (..),
  ExprObject (..),
  SomeExpr (..),
  ExprNode (..),
  LoopSpec (..),
  SamplingKind (..),
  SamplingMode (..),
  UnaryOp (..),
  BinaryOp (..),
  CompareOp (..),
  ShaderValue (..),
  ShaderArithmetic (..),
  ShaderFloat (..),
  shaderTy,
  someShaderTy,
  someExpr,
  literal,
  input,
  local,
  resource,
  storageRead,
  storageLength,
  numericUnary,
  numericBinary,
  floatingUnary,
  compareExpr,
  constructVector,
  extract,
  select,
  branch,
  while,
  dotExpr,
  crossExpr,
  normalizeExpr,
  reflectExpr,
  mixExpr,
  smoothstepExpr,
  matrixMultiplyExpr,
  matrixVectorExpr,
  derivative,
  sampleExpr,
  sampleCompareExpr,
  imageShaderType,
  shaderImageDimension,
) where

import Data.Int (Int32)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
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

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

data Stage = Vertex | Fragment | Compute
  deriving stock (Eq, Ord, Show)

data ImageDimension = Image1D | Image2D | Image3D | ImageCube | Image2DArray
  deriving stock (Eq, Ord, Show)

-- | Matrix dimensions follow @linear@: the outer and inner vector sizes.
data ShaderTy
  = TyFloat
  | TyInt
  | TyWord
  | TyBool
  | TyVector Int
  | TyWordVector Int
  | TyMatrix Int Int
  | TyImage1D
  | TyImage2D
  | TyImage3D
  | TyImageCube
  | TyImage2DArray
  | TySampler
  deriving stock (Eq, Ord, Show)

data HostValue
  = HFloat Float
  | HInt Int32
  | HWord Word32
  | HBool Bool
  | HVector [Float]
  | HWordVector [Word32]
  | HMatrix Int Int [Float]
  deriving stock (Eq, Show)

newtype BinderId = BinderId Int
  deriving stock (Eq, Ord, Show)

{- | A real heap node. Child nodes retain existential @Expr@ references, so a
shared Haskell binding remains the same object all the way to reification.
-}
type role Expr nominal nominal

data Expr (s :: Stage) a = Expr ExprObject

data ExprObject = ExprObject
  { objectTy :: ShaderTy
  , objectNode :: ExprNode
  }

data SomeExpr = forall s a. SomeExpr (Expr s a)

data LoopSpec = forall s a. (ShaderValue a) => LoopSpec
  { loopInitial :: Expr s a
  , loopPredicate :: Expr s a -> Expr s Bool
  , loopStep :: Expr s a -> Expr s a
  }

data SamplingMode = ImplicitLod | ExplicitLod
  deriving stock (Eq, Ord, Show)

data SamplingKind = RegularSample | ComparisonSample
  deriving stock (Eq, Ord, Show)

data UnaryOp
  = NegateE
  | AbsE
  | SignumE
  | RecipE
  | SinE
  | CosE
  | TanE
  | AsinE
  | AcosE
  | AtanE
  | ExpE
  | LogE
  | SqrtE
  | NormalizeE
  | DfdxE
  | DfdyE
  | FwidthE
  deriving stock (Eq, Ord, Show)

data BinaryOp
  = AddE
  | SubtractE
  | MultiplyE
  | DivideE
  | PowerE
  | MinE
  | MaxE
  | DotE
  | CrossE
  | ReflectE
  | MatrixMultiplyE
  | MatrixVectorMultiplyE
  deriving stock (Eq, Ord, Show)

data CompareOp = EqualE | NotEqualE | LessE | LessEqualE | GreaterE | GreaterEqualE
  deriving stock (Eq, Ord, Show)

data ExprNode
  = LiteralNode HostValue
  | InputNode String
  | LocalNode BinderId
  | ResourceNode String
  | StorageReadNode String SomeExpr
  | StorageLengthNode String
  | UnaryNode UnaryOp SomeExpr
  | BinaryNode BinaryOp SomeExpr SomeExpr
  | CompareNode CompareOp SomeExpr SomeExpr
  | ConstructNode [SomeExpr]
  | ExtractNode [Int] SomeExpr
  | SelectNode SomeExpr SomeExpr SomeExpr
  | BranchNode SomeExpr SomeExpr SomeExpr
  | WhileNode LoopSpec
  | MixNode SomeExpr SomeExpr SomeExpr
  | SmoothstepNode SomeExpr SomeExpr SomeExpr
  | SampleNode SamplingKind SamplingMode SomeExpr SomeExpr SomeExpr (Maybe SomeExpr) (Maybe SomeExpr)

shaderTy :: Expr s a -> ShaderTy
shaderTy (Expr object) = objectTy object

someShaderTy :: SomeExpr -> ShaderTy
someShaderTy (SomeExpr expression) = shaderTy expression

someExpr :: Expr s a -> SomeExpr
someExpr = SomeExpr

class ShaderValue a where
  valueTy :: proxy a -> ShaderTy
  toHostValue :: a -> HostValue
  fromHostValue :: HostValue -> Maybe a

instance ShaderValue Float where
  valueTy _ = TyFloat
  toHostValue = HFloat
  fromHostValue (HFloat value) = Just value
  fromHostValue _ = Nothing

instance ShaderValue Int32 where
  valueTy _ = TyInt
  toHostValue = HInt
  fromHostValue (HInt value) = Just value
  fromHostValue _ = Nothing

instance ShaderValue Word32 where
  valueTy _ = TyWord
  toHostValue = HWord
  fromHostValue (HWord value) = Just value
  fromHostValue _ = Nothing

instance ShaderValue Bool where
  valueTy _ = TyBool
  toHostValue = HBool
  fromHostValue (HBool value) = Just value
  fromHostValue _ = Nothing

instance ShaderValue (V2 Float) where
  valueTy _ = TyVector 2
  toHostValue (V2 xValue yValue) = HVector [xValue, yValue]
  fromHostValue (HVector [xValue, yValue]) = Just (V2 xValue yValue)
  fromHostValue _ = Nothing

instance ShaderValue (V3 Float) where
  valueTy _ = TyVector 3
  toHostValue (V3 xValue yValue zValue) = HVector [xValue, yValue, zValue]
  fromHostValue (HVector [xValue, yValue, zValue]) = Just (V3 xValue yValue zValue)
  fromHostValue _ = Nothing

instance ShaderValue (V4 Float) where
  valueTy _ = TyVector 4
  toHostValue (V4 xValue yValue zValue wValue) = HVector [xValue, yValue, zValue, wValue]
  fromHostValue (HVector [xValue, yValue, zValue, wValue]) = Just (V4 xValue yValue zValue wValue)
  fromHostValue _ = Nothing

instance ShaderValue (V3 Word32) where
  valueTy _ = TyWordVector 3
  toHostValue (V3 xValue yValue zValue) = HWordVector [xValue, yValue, zValue]
  fromHostValue (HWordVector [xValue, yValue, zValue]) = Just (V3 xValue yValue zValue)
  fromHostValue _ = Nothing

instance ShaderValue (V2 Word32) where
  valueTy _ = TyWordVector 2
  toHostValue (V2 xValue yValue) = HWordVector [xValue, yValue]
  fromHostValue (HWordVector [xValue, yValue]) = Just (V2 xValue yValue)
  fromHostValue _ = Nothing

instance ShaderValue (V4 Word32) where
  valueTy _ = TyWordVector 4
  toHostValue (V4 xValue yValue zValue wValue) = HWordVector [xValue, yValue, zValue, wValue]
  fromHostValue (HWordVector [xValue, yValue, zValue, wValue]) = Just (V4 xValue yValue zValue wValue)
  fromHostValue _ = Nothing

instance ShaderValue (V2 (V2 Float)) where
  valueTy _ = TyMatrix 2 2
  toHostValue (V2 a b) = matrixValue 2 2 [v2 a, v2 b]
  fromHostValue value
    | matrixHasShape 2 2 value = V2 <$> matrixV2 0 value <*> matrixV2 1 value
    | otherwise = Nothing

instance ShaderValue (V2 (V3 Float)) where
  valueTy _ = TyMatrix 2 3
  toHostValue (V2 a b) = matrixValue 2 3 [v3 a, v3 b]
  fromHostValue value
    | matrixHasShape 2 3 value = V2 <$> matrixV3 0 value <*> matrixV3 1 value
    | otherwise = Nothing

instance ShaderValue (V2 (V4 Float)) where
  valueTy _ = TyMatrix 2 4
  toHostValue (V2 a b) = matrixValue 2 4 [v4 a, v4 b]
  fromHostValue value
    | matrixHasShape 2 4 value = V2 <$> matrixV4 0 value <*> matrixV4 1 value
    | otherwise = Nothing

instance ShaderValue (V3 (V2 Float)) where
  valueTy _ = TyMatrix 3 2
  toHostValue (V3 a b c) = matrixValue 3 2 [v2 a, v2 b, v2 c]
  fromHostValue value
    | matrixHasShape 3 2 value = V3 <$> matrixV2 0 value <*> matrixV2 1 value <*> matrixV2 2 value
    | otherwise = Nothing

instance ShaderValue (V3 (V3 Float)) where
  valueTy _ = TyMatrix 3 3
  toHostValue (V3 a b c) = matrixValue 3 3 [v3 a, v3 b, v3 c]
  fromHostValue value
    | matrixHasShape 3 3 value = V3 <$> matrixV3 0 value <*> matrixV3 1 value <*> matrixV3 2 value
    | otherwise = Nothing

instance ShaderValue (V3 (V4 Float)) where
  valueTy _ = TyMatrix 3 4
  toHostValue (V3 a b c) = matrixValue 3 4 [v4 a, v4 b, v4 c]
  fromHostValue value
    | matrixHasShape 3 4 value = V3 <$> matrixV4 0 value <*> matrixV4 1 value <*> matrixV4 2 value
    | otherwise = Nothing

instance ShaderValue (V4 (V2 Float)) where
  valueTy _ = TyMatrix 4 2
  toHostValue (V4 a b c d) = matrixValue 4 2 [v2 a, v2 b, v2 c, v2 d]
  fromHostValue value
    | matrixHasShape 4 2 value = V4 <$> matrixV2 0 value <*> matrixV2 1 value <*> matrixV2 2 value <*> matrixV2 3 value
    | otherwise = Nothing

instance ShaderValue (V4 (V3 Float)) where
  valueTy _ = TyMatrix 4 3
  toHostValue (V4 a b c d) = matrixValue 4 3 [v3 a, v3 b, v3 c, v3 d]
  fromHostValue value
    | matrixHasShape 4 3 value = V4 <$> matrixV3 0 value <*> matrixV3 1 value <*> matrixV3 2 value <*> matrixV3 3 value
    | otherwise = Nothing

instance ShaderValue (V4 (V4 Float)) where
  valueTy _ = TyMatrix 4 4
  toHostValue (V4 a b c d) = matrixValue 4 4 [v4 a, v4 b, v4 c, v4 d]
  fromHostValue value
    | matrixHasShape 4 4 value = V4 <$> matrixV4 0 value <*> matrixV4 1 value <*> matrixV4 2 value <*> matrixV4 3 value
    | otherwise = Nothing

class (ShaderValue a) => ShaderArithmetic a where
  splatInteger :: Integer -> a

class (ShaderArithmetic a) => ShaderFloat a where
  splatRational :: Rational -> a

instance ShaderArithmetic Float where splatInteger = fromInteger
instance ShaderFloat Float where splatRational = fromRational
instance ShaderArithmetic Int32 where splatInteger = fromInteger
instance ShaderArithmetic Word32 where splatInteger = fromInteger
instance ShaderArithmetic (V2 Float) where splatInteger value = pure (fromInteger value)
instance ShaderFloat (V2 Float) where splatRational value = pure (fromRational value)
instance ShaderArithmetic (V3 Float) where splatInteger value = pure (fromInteger value)
instance ShaderFloat (V3 Float) where splatRational value = pure (fromRational value)
instance ShaderArithmetic (V4 Float) where splatInteger value = pure (fromInteger value)
instance ShaderFloat (V4 Float) where splatRational value = pure (fromRational value)
instance ShaderArithmetic (M22 Float) where splatInteger value = pure (pure (fromInteger value))
instance ShaderArithmetic (M23 Float) where splatInteger value = pure (pure (fromInteger value))
instance ShaderArithmetic (M24 Float) where splatInteger value = pure (pure (fromInteger value))
instance ShaderArithmetic (M32 Float) where splatInteger value = pure (pure (fromInteger value))
instance ShaderArithmetic (M33 Float) where splatInteger value = pure (pure (fromInteger value))
instance ShaderArithmetic (M34 Float) where splatInteger value = pure (pure (fromInteger value))
instance ShaderArithmetic (M42 Float) where splatInteger value = pure (pure (fromInteger value))
instance ShaderArithmetic (M43 Float) where splatInteger value = pure (pure (fromInteger value))
instance ShaderArithmetic (M44 Float) where splatInteger value = pure (pure (fromInteger value))

instance (ShaderArithmetic a) => Num (Expr s a) where
  (+) = numericBinary AddE
  (-) = numericBinary SubtractE
  (*) = numericBinary MultiplyE
  negate = numericUnary NegateE
  abs = numericUnary AbsE
  signum = numericUnary SignumE
  fromInteger = literal . splatInteger

instance (ShaderFloat a) => Fractional (Expr s a) where
  (/) = numericBinary DivideE
  recip = numericUnary RecipE
  fromRational = literal . splatRational

instance (ShaderFloat a) => Floating (Expr s a) where
  pi = literal (splatRational (toRational (pi :: Double)))
  exp = floatingUnary ExpE
  log = floatingUnary LogE
  sqrt = floatingUnary SqrtE
  sin = floatingUnary SinE
  cos = floatingUnary CosE
  tan = floatingUnary TanE
  asin = floatingUnary AsinE
  acos = floatingUnary AcosE
  atan = floatingUnary AtanE
  (**) = numericBinary PowerE
  logBase base value = log value / log base
  sinh value = (exp value - exp (negate value)) / 2
  cosh value = (exp value + exp (negate value)) / 2
  tanh value = sinh value / cosh value
  asinh value = log (value + sqrt (value * value + 1))
  acosh value = log (value + sqrt (value * value - 1))
  atanh value = log ((1 + value) / (1 - value)) / 2

literal :: forall s a. (ShaderValue a) => a -> Expr s a
literal value = makeExpr (valueTy (Proxy @a)) (LiteralNode (toHostValue value))

input :: forall s a. (ShaderValue a) => String -> Expr s a
input name = makeExpr (valueTy (Proxy @a)) (InputNode name)

local :: ShaderTy -> BinderId -> Expr s a
local ty binder = makeExpr ty (LocalNode binder)

resource :: ShaderTy -> String -> Expr s a
resource ty name
  | isImageType ty || ty == TySampler = makeExpr ty (ResourceNode name)
  | otherwise = internalTypeError "resource" [ty]

storageRead :: ShaderTy -> String -> Expr s Word32 -> Expr s a
storageRead ty name index
  | shaderTy index == TyWord = makeExpr ty (StorageReadNode name (someExpr index))
  | otherwise = internalTypeError "storage read index" [shaderTy index]

storageLength :: String -> Expr s Word32
storageLength name = makeExpr TyWord (StorageLengthNode name)

numericUnary :: UnaryOp -> Expr s a -> Expr s a
numericUnary operation expression
  | legalNumericUnary operation (shaderTy expression) = makeExpr (shaderTy expression) (UnaryNode operation (someExpr expression))
  | otherwise = internalTypeError (show operation) [shaderTy expression]

floatingUnary :: UnaryOp -> Expr s a -> Expr s a
floatingUnary operation expression
  | isFloatShape (shaderTy expression) && operation `elem` [SinE, CosE, TanE, AsinE, AcosE, AtanE, ExpE, LogE, SqrtE] = makeExpr (shaderTy expression) (UnaryNode operation (someExpr expression))
  | otherwise = internalTypeError (show operation) [shaderTy expression]

numericBinary :: BinaryOp -> Expr s a -> Expr s a -> Expr s a
numericBinary operation left right
  | shaderTy left == shaderTy right && legalNumericBinary operation (shaderTy left) = makeExpr (shaderTy left) (BinaryNode operation (someExpr left) (someExpr right))
  | otherwise = internalTypeError (show operation) [shaderTy left, shaderTy right]

compareExpr :: CompareOp -> Expr s a -> Expr s a -> Expr s Bool
compareExpr operation left right
  | shaderTy left == shaderTy right && legalComparison operation (shaderTy left) = makeExpr TyBool (CompareNode operation (someExpr left) (someExpr right))
  | otherwise = internalTypeError (show operation) [shaderTy left, shaderTy right]

constructVector :: [Expr s Float] -> Expr s a
constructVector components
  | length components `elem` [2, 3, 4] = makeExpr (TyVector (length components)) (ConstructNode (map someExpr components))
  | otherwise = internalTypeError "vector constructor" (map shaderTy components)

extract :: ShaderTy -> [Int] -> Expr s a -> Expr s b
extract resultTy indices expression
  | validExtraction resultTy indices (shaderTy expression) = makeExpr resultTy (ExtractNode indices (someExpr expression))
  | otherwise = internalTypeError "vector extraction" [resultTy, shaderTy expression]

select :: Expr s Bool -> Expr s a -> Expr s a -> Expr s a
select = selection SelectNode "select"

branch :: Expr s Bool -> Expr s a -> Expr s a -> Expr s a
branch = selection BranchNode "branch"

while :: (ShaderValue a) => (Expr s a -> Expr s Bool) -> (Expr s a -> Expr s a) -> Expr s a -> Expr s a
while predicate step initial = makeExpr (shaderTy initial) (WhileNode (LoopSpec initial predicate step))

dotExpr :: Expr s a -> Expr s a -> Expr s Float
dotExpr left right
  | shaderTy left == shaderTy right && isFloatVector (shaderTy left) = makeExpr TyFloat (BinaryNode DotE (someExpr left) (someExpr right))
  | otherwise = internalTypeError "dot" [shaderTy left, shaderTy right]

crossExpr :: Expr s a -> Expr s a -> Expr s a
crossExpr left right
  | shaderTy left == TyVector 3 && shaderTy right == TyVector 3 = makeExpr (TyVector 3) (BinaryNode CrossE (someExpr left) (someExpr right))
  | otherwise = internalTypeError "cross" [shaderTy left, shaderTy right]

normalizeExpr :: Expr s a -> Expr s a
normalizeExpr expression
  | isFloatVector (shaderTy expression) = makeExpr (shaderTy expression) (UnaryNode NormalizeE (someExpr expression))
  | otherwise = internalTypeError "normalize" [shaderTy expression]

reflectExpr :: Expr s a -> Expr s a -> Expr s a
reflectExpr incident normal
  | shaderTy incident == shaderTy normal && isFloatVector (shaderTy incident) = makeExpr (shaderTy incident) (BinaryNode ReflectE (someExpr incident) (someExpr normal))
  | otherwise = internalTypeError "reflect" [shaderTy incident, shaderTy normal]

mixExpr :: Expr s a -> Expr s a -> Expr s Float -> Expr s a
mixExpr left right factor
  | shaderTy left == shaderTy right && isFloatShape (shaderTy left) && shaderTy factor == TyFloat = makeExpr (shaderTy left) (MixNode (someExpr left) (someExpr right) (someExpr factor))
  | otherwise = internalTypeError "mix" [shaderTy left, shaderTy right, shaderTy factor]

smoothstepExpr :: Expr s a -> Expr s a -> Expr s a -> Expr s a
smoothstepExpr edge0 edge1 value
  | all (== shaderTy value) [shaderTy edge0, shaderTy edge1] && isFloatShape (shaderTy value) = makeExpr (shaderTy value) (SmoothstepNode (someExpr edge0) (someExpr edge1) (someExpr value))
  | otherwise = internalTypeError "smoothstep" [shaderTy edge0, shaderTy edge1, shaderTy value]

matrixMultiplyExpr :: Expr s a -> Expr s b -> Expr s c
matrixMultiplyExpr left right = case (shaderTy left, shaderTy right) of
  (TyMatrix outer inner, TyMatrix inner' resultInner)
    | inner == inner' -> makeExpr (TyMatrix outer resultInner) (BinaryNode MatrixMultiplyE (someExpr left) (someExpr right))
  _ -> internalTypeError "matrixMultiply" [shaderTy left, shaderTy right]

matrixVectorExpr :: Expr s a -> Expr s b -> Expr s c
matrixVectorExpr matrix vector = case (shaderTy matrix, shaderTy vector) of
  (TyMatrix outer inner, TyVector size)
    | inner == size -> makeExpr (TyVector outer) (BinaryNode MatrixVectorMultiplyE (someExpr matrix) (someExpr vector))
  _ -> internalTypeError "matrixVectorMultiply" [shaderTy matrix, shaderTy vector]

derivative :: UnaryOp -> Expr s a -> Expr s a
derivative operation expression
  | operation `elem` [DfdxE, DfdyE, FwidthE] && isFloatShape (shaderTy expression) = makeExpr (shaderTy expression) (UnaryNode operation (someExpr expression))
  | otherwise = internalTypeError (show operation) [shaderTy expression]

sampleExpr :: SamplingMode -> Expr s image -> Expr s sampler -> Expr s coordinates -> Maybe (Expr s Float) -> Expr s result
sampleExpr mode image sampler coordinates lod
  | expectedSampleCoordinateType (shaderTy image) == Just (shaderTy coordinates)
      && shaderTy sampler == TySampler
      && validLod mode lod =
      makeExpr (TyVector 4) (SampleNode RegularSample mode (someExpr image) (someExpr sampler) (someExpr coordinates) Nothing (someExpr <$> lod))
  | otherwise = internalTypeError "sample" ([shaderTy image, shaderTy sampler, shaderTy coordinates] <> maybe [] (pure . shaderTy) lod)

sampleCompareExpr :: SamplingMode -> Expr s image -> Expr s sampler -> Expr s coordinates -> Expr s Float -> Maybe (Expr s Float) -> Expr s Float
sampleCompareExpr mode image sampler coordinates reference lod
  | expectedSampleCoordinateType (shaderTy image) == Just (shaderTy coordinates)
      && shaderTy sampler == TySampler
      && shaderTy reference == TyFloat
      && validLod mode lod =
      makeExpr TyFloat (SampleNode ComparisonSample mode (someExpr image) (someExpr sampler) (someExpr coordinates) (Just (someExpr reference)) (someExpr <$> lod))
  | otherwise = internalTypeError "sampleCompare" ([shaderTy image, shaderTy sampler, shaderTy coordinates, shaderTy reference] <> maybe [] (pure . shaderTy) lod)

makeExpr :: ShaderTy -> ExprNode -> Expr s a
makeExpr ty node = Expr (ExprObject ty node)

selection :: (SomeExpr -> SomeExpr -> SomeExpr -> ExprNode) -> String -> Expr s Bool -> Expr s a -> Expr s a -> Expr s a
selection constructor name condition yes no
  | shaderTy condition == TyBool && shaderTy yes == shaderTy no = makeExpr (shaderTy yes) (constructor (someExpr condition) (someExpr yes) (someExpr no))
  | otherwise = internalTypeError name [shaderTy condition, shaderTy yes, shaderTy no]

isArithmetic :: ShaderTy -> Bool
isArithmetic TyFloat = True
isArithmetic TyInt = True
isArithmetic TyWord = True
isArithmetic (TyVector size) = size `elem` [2, 3, 4]
isArithmetic (TyMatrix outer inner) = outer `elem` [2, 3, 4] && inner `elem` [2, 3, 4]
isArithmetic _ = False

legalNumericUnary :: UnaryOp -> ShaderTy -> Bool
legalNumericUnary operation ty = case operation of
  NegateE -> isArithmetic ty
  AbsE -> isArithmetic ty
  SignumE -> isArithmetic ty
  RecipE -> isFloatShape ty
  _ -> False

legalNumericBinary :: BinaryOp -> ShaderTy -> Bool
legalNumericBinary operation ty = case operation of
  AddE -> isArithmetic ty
  SubtractE -> isArithmetic ty
  MultiplyE -> isArithmetic ty
  DivideE -> isFloatShape ty
  PowerE -> isFloatShape ty
  MinE -> isFloatShape ty
  MaxE -> isFloatShape ty
  _ -> False

isFloatShape :: ShaderTy -> Bool
isFloatShape TyFloat = True
isFloatShape ty = isFloatVector ty

isFloatVector :: ShaderTy -> Bool
isFloatVector (TyVector size) = size `elem` [2, 3, 4]
isFloatVector _ = False

legalComparison :: CompareOp -> ShaderTy -> Bool
legalComparison operation ty = case operation of
  EqualE -> equalityType ty
  NotEqualE -> equalityType ty
  _ -> ty == TyFloat || ty == TyInt || ty == TyWord

equalityType :: ShaderTy -> Bool
equalityType ty = ty == TyFloat || ty == TyInt || ty == TyWord || ty == TyBool || isFloatVector ty || isWordVector ty || isMatrix ty

isWordVector :: ShaderTy -> Bool
isWordVector (TyWordVector size) = size `elem` [2, 3, 4]
isWordVector _ = False

isMatrix :: ShaderTy -> Bool
isMatrix (TyMatrix _ _) = True
isMatrix _ = False

validExtraction :: ShaderTy -> [Int] -> ShaderTy -> Bool
validExtraction resultTy indices (TyVector size) =
  not (null indices)
    && all (\index -> index >= 0 && index < size) indices
    && case indices of
      [_] -> resultTy == TyFloat
      _ -> resultTy == TyVector (length indices) && length indices `elem` [2, 3, 4]
validExtraction resultTy indices (TyWordVector size) =
  not (null indices)
    && all (\index -> index >= 0 && index < size) indices
    && case indices of
      [_] -> resultTy == TyWord
      _ -> resultTy == TyWordVector (length indices) && length indices `elem` [2, 3, 4]
validExtraction _ _ _ = False

validLod :: SamplingMode -> Maybe (Expr s Float) -> Bool
validLod ImplicitLod Nothing = True
validLod ExplicitLod (Just expression) = shaderTy expression == TyFloat
validLod _ _ = False

imageShaderType :: ImageDimension -> ShaderTy
imageShaderType dimension = case dimension of
  Image1D -> TyImage1D
  Image2D -> TyImage2D
  Image3D -> TyImage3D
  ImageCube -> TyImageCube
  Image2DArray -> TyImage2DArray

shaderImageDimension :: ShaderTy -> Maybe ImageDimension
shaderImageDimension shaderType = case shaderType of
  TyImage1D -> Just Image1D
  TyImage2D -> Just Image2D
  TyImage3D -> Just Image3D
  TyImageCube -> Just ImageCube
  TyImage2DArray -> Just Image2DArray
  _ -> Nothing

isImageType :: ShaderTy -> Bool
isImageType = isJust . shaderImageDimension

expectedSampleCoordinateType :: ShaderTy -> Maybe ShaderTy
expectedSampleCoordinateType shaderType = case shaderImageDimension shaderType of
  Just Image1D -> Just TyFloat
  Just Image2D -> Just (TyVector 2)
  Just Image3D -> Just (TyVector 3)
  Just ImageCube -> Just (TyVector 3)
  Just Image2DArray -> Just (TyVector 3)
  Nothing -> Nothing

internalTypeError :: String -> [ShaderTy] -> a
internalTypeError operation types = error ("Vpipe.Expr.Internal: invalid " <> operation <> " operands " <> show types)

v2 :: V2 Float -> [Float]
v2 (V2 a b) = [a, b]

v3 :: V3 Float -> [Float]
v3 (V3 a b c) = [a, b, c]

v4 :: V4 Float -> [Float]
v4 (V4 a b c d) = [a, b, c, d]

matrixValue :: Int -> Int -> [[Float]] -> HostValue
matrixValue outer inner vectors = HMatrix outer inner (concat vectors)

matrixHasShape :: Int -> Int -> HostValue -> Bool
matrixHasShape expectedOuter expectedInner (HMatrix outer inner values) =
  outer == expectedOuter && inner == expectedInner && length values == outer * inner
matrixHasShape _ _ _ = False

matrixV2 :: Int -> HostValue -> Maybe (V2 Float)
matrixV2 index (HMatrix outer 2 values)
  | index < outer = case take 2 (drop (index * 2) values) of
      [a, b] -> Just (V2 a b)
      _ -> Nothing
matrixV2 _ _ = Nothing

matrixV3 :: Int -> HostValue -> Maybe (V3 Float)
matrixV3 index (HMatrix outer 3 values)
  | index < outer = case take 3 (drop (index * 3) values) of
      [a, b, c] -> Just (V3 a b c)
      _ -> Nothing
matrixV3 _ _ = Nothing

matrixV4 :: Int -> HostValue -> Maybe (V4 Float)
matrixV4 index (HMatrix outer 4 values)
  | index < outer = case take 4 (drop (index * 4) values) of
      [a, b, c, d] -> Just (V4 a b c d)
      _ -> Nothing
matrixV4 _ _ = Nothing
