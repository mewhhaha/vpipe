{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK hide #-}

{- | Total host evaluator for shader expressions. GPU-context operations are
represented by structured errors rather than approximated on the CPU.

Inputs are bound by their shader names before evaluation:

@
module Main (main) where

import Vpipe.Expr (Expr, input)
import Vpipe.Expr.Eval (bindInput, emptyEnv, evalExprWith)

expression :: Expr s Float
expression = input "exposure"

main :: IO ()
main = print (evalExprWith (bindInput "exposure" (1.0 :: Float) emptyEnv) expression)
@
-}
module Vpipe.Expr.Eval (
  EvalError (..),
  EvalFeature (..),
  EvalEnv,
  emptyEnv,
  bindInput,
  evalExpr,
  evalExprWith,
  evalExprWithFuel,
) where

import Data.Int (Int32)
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Vpipe.Expr.Internal

type EvalEnv = Map.Map String HostValue

emptyEnv :: EvalEnv
emptyEnv = Map.empty

bindInput :: (ShaderValue a) => String -> a -> EvalEnv -> EvalEnv
bindInput name value = Map.insert name (toHostValue value)

data EvalFeature
  = DerivativeEvaluation UnaryOp
  | SamplingEvaluation SamplingMode
  | ResourceEvaluation ShaderTy String
  | StorageEvaluation String
  | LocalEvaluation BinderId
  deriving stock (Eq, Show)

data EvalError
  = MissingVariable String
  | TypeMismatch ShaderTy HostValue
  | UnsupportedOperation EvalFeature
  | DivisionByZero
  | LoopFuelExhausted
  | InvalidOperation String [HostValue]
  deriving stock (Eq, Show)

evalExpr :: (ShaderValue a) => Expr s a -> Either EvalError a
evalExpr = evalExprWith emptyEnv

evalExprWith :: (ShaderValue a) => EvalEnv -> Expr s a -> Either EvalError a
evalExprWith = evalExprWithFuel 10000

evalExprWithFuel :: forall s a. (ShaderValue a) => Int -> EvalEnv -> Expr s a -> Either EvalError a
evalExprWithFuel initialFuel environment expression = do
  (value, _) <- evaluate environment initialFuel expression
  case fromHostValue value of
    Just result -> Right result
    Nothing -> Left (TypeMismatch (valueTy (Proxy @a)) value)

evaluateSome :: EvalEnv -> Int -> SomeExpr -> Either EvalError (HostValue, Int)
evaluateSome environment fuel (SomeExpr expression) = evaluate environment fuel expression

evaluate :: EvalEnv -> Int -> Expr s a -> Either EvalError (HostValue, Int)
evaluate environment fuel (Expr object) = case objectNode object of
  LiteralNode value -> Right (value, fuel)
  InputNode name -> case Map.lookup name environment of
    Nothing -> Left (MissingVariable name)
    Just value -> (,fuel) <$> requireType (objectTy object) value
  LocalNode binder -> Left (UnsupportedOperation (LocalEvaluation binder))
  ResourceNode name -> Left (UnsupportedOperation (ResourceEvaluation (objectTy object) name))
  StorageReadNode name _ -> Left (UnsupportedOperation (StorageEvaluation name))
  StorageLengthNode name -> Left (UnsupportedOperation (StorageEvaluation name))
  UnaryNode operation child -> do
    (value, remaining) <- evaluateSome environment fuel child
    result <- applyUnary operation value
    pure (result, remaining)
  BinaryNode operation left right -> do
    (leftValue, afterLeft) <- evaluateSome environment fuel left
    (rightValue, afterRight) <- evaluateSome environment afterLeft right
    result <- applyBinary operation leftValue rightValue
    pure (result, afterRight)
  CompareNode operation left right -> do
    (leftValue, afterLeft) <- evaluateSome environment fuel left
    (rightValue, afterRight) <- evaluateSome environment afterLeft right
    result <- applyCompare operation leftValue rightValue
    pure (HBool result, afterRight)
  ConstructNode components -> do
    (values, remaining) <- evaluateMany environment fuel components
    scalars <- traverse expectFloat values
    pure (HVector scalars, remaining)
  ExtractNode indices value -> do
    (hostValue, remaining) <- evaluateSome environment fuel value
    case hostValue of
      HVector vector -> do
        selected <- traverse (`indexAt` vector) indices
        pure (case selected of [component] -> HFloat component; components -> HVector components, remaining)
      HWordVector vector -> do
        selected <- traverse (`indexAt` vector) indices
        pure (case selected of [component] -> HWord component; components -> HWordVector components, remaining)
      _ -> Left (InvalidOperation "extract" [hostValue])
  SelectNode condition yes no -> do
    (conditionValue, afterCondition) <- evaluateSome environment fuel condition
    chooseYes <- expectBool conditionValue
    (yesValue, afterYes) <- evaluateSome environment afterCondition yes
    (noValue, remaining) <- evaluateSome environment afterYes no
    pure (if chooseYes then yesValue else noValue, remaining)
  BranchNode condition yes no -> evaluateConditional environment fuel condition yes no
  WhileNode specification -> evaluateLoop environment fuel specification
  MixNode left right factor -> do
    (leftValue, afterLeft) <- evaluateSome environment fuel left
    (rightValue, afterRight) <- evaluateSome environment afterLeft right
    (factorValue, remaining) <- evaluateSome environment afterRight factor
    factorScalar <- expectFloat factorValue
    result <- zipFloatShapes "mix" (\a b -> a * (1 - factorScalar) + b * factorScalar) leftValue rightValue
    pure (result, remaining)
  SmoothstepNode edge0 edge1 value -> do
    (lowValue, afterLow) <- evaluateSome environment fuel edge0
    (highValue, afterHigh) <- evaluateSome environment afterLow edge1
    (inputValue, remaining) <- evaluateSome environment afterHigh value
    result <- smoothstepValues lowValue highValue inputValue
    pure (result, remaining)
  SampleNode _ mode _ _ _ _ _ -> Left (UnsupportedOperation (SamplingEvaluation mode))

evaluateMany :: EvalEnv -> Int -> [SomeExpr] -> Either EvalError ([HostValue], Int)
evaluateMany _ fuel [] = Right ([], fuel)
evaluateMany environment fuel (expression : rest) = do
  (value, afterValue) <- evaluateSome environment fuel expression
  (values, remaining) <- evaluateMany environment afterValue rest
  pure (value : values, remaining)

evaluateConditional :: EvalEnv -> Int -> SomeExpr -> SomeExpr -> SomeExpr -> Either EvalError (HostValue, Int)
evaluateConditional environment fuel condition yes no = do
  (conditionValue, remaining) <- evaluateSome environment fuel condition
  chooseYes <- expectBool conditionValue
  evaluateSome environment remaining (if chooseYes then yes else no)

evaluateLoop :: EvalEnv -> Int -> LoopSpec -> Either EvalError (HostValue, Int)
evaluateLoop environment fuel (LoopSpec initial predicate step) = do
  (initialValue, remaining) <- evaluate environment fuel initial
  current <- decodeLoopValue initialValue
  run current remaining
 where
  run current remaining
    | remaining <= 0 = Left LoopFuelExhausted
    | otherwise = do
        let currentExpression = literal current
        (conditionValue, afterCondition) <- evaluate environment (remaining - 1) (predicate currentExpression)
        continue <- expectBool conditionValue
        if continue
          then do
            (nextValue, afterStep) <- evaluate environment afterCondition (step currentExpression)
            next <- decodeLoopValue nextValue
            run next afterStep
          else Right (toHostValue current, afterCondition)
  decodeLoopValue value = case fromHostValue value of
    Just decoded -> Right decoded
    Nothing -> Left (TypeMismatch (shaderTy initial) value)

requireType :: ShaderTy -> HostValue -> Either EvalError HostValue
requireType expected value
  | hostValueTy value == expected = Right value
  | otherwise = Left (TypeMismatch expected value)

hostValueTy :: HostValue -> ShaderTy
hostValueTy value = case value of
  HFloat _ -> TyFloat
  HInt _ -> TyInt
  HWord _ -> TyWord
  HBool _ -> TyBool
  HVector components -> TyVector (length components)
  HWordVector components -> TyWordVector (length components)
  HMatrix outer inner _ -> TyMatrix outer inner

applyUnary :: UnaryOp -> HostValue -> Either EvalError HostValue
applyUnary operation value = case operation of
  NegateE -> mapArithmetic "negate" negate negate negate value
  AbsE -> mapArithmetic "abs" abs abs abs value
  SignumE -> mapArithmetic "signum" signum signum signum value
  RecipE -> mapFloatShape "recip" recip value
  SinE -> mapFloatShape "sin" sin value
  CosE -> mapFloatShape "cos" cos value
  TanE -> mapFloatShape "tan" tan value
  AsinE -> mapFloatShape "asin" asin value
  AcosE -> mapFloatShape "acos" acos value
  AtanE -> mapFloatShape "atan" atan value
  ExpE -> mapFloatShape "exp" exp value
  LogE -> mapFloatShape "log" log value
  SqrtE -> mapFloatShape "sqrt" sqrt value
  NormalizeE -> normalizeValue value
  DfdxE -> Left (UnsupportedOperation (DerivativeEvaluation DfdxE))
  DfdyE -> Left (UnsupportedOperation (DerivativeEvaluation DfdyE))
  FwidthE -> Left (UnsupportedOperation (DerivativeEvaluation FwidthE))

applyBinary :: BinaryOp -> HostValue -> HostValue -> Either EvalError HostValue
applyBinary operation left right = case operation of
  AddE -> zipArithmetic "add" (+) (+) (+) left right
  SubtractE -> zipArithmetic "subtract" (-) (-) (-) left right
  MultiplyE -> zipArithmetic "multiply" (*) (*) (*) left right
  DivideE -> case (left, right) of
    (HInt _, HInt _) -> Left (InvalidOperation "integer divide" [left, right])
    _ -> zipFloatShapes "divide" (/) left right
  PowerE -> zipFloatShapes "power" (**) left right
  MinE -> zipArithmetic "min" min min min left right
  MaxE -> zipArithmetic "max" max max max left right
  DotE -> HFloat <$> dotValues left right
  CrossE -> crossValues left right
  ReflectE -> reflectValues left right
  MatrixMultiplyE -> multiplyMatrices left right
  MatrixVectorMultiplyE -> multiplyMatrixVector left right

applyCompare :: CompareOp -> HostValue -> HostValue -> Either EvalError Bool
applyCompare operation left right = case operation of
  EqualE -> comparableEquality (==)
  NotEqualE -> comparableEquality (/=)
  LessE -> ordered (<) (<) (<)
  LessEqualE -> ordered (<=) (<=) (<=)
  GreaterE -> ordered (>) (>) (>)
  GreaterEqualE -> ordered (>=) (>=) (>=)
 where
  comparableEquality comparison
    | hostValueTy left == hostValueTy right = Right (comparison left right)
    | otherwise = Left (InvalidOperation "equality" [left, right])
  ordered floatComparison intComparison wordComparison = case (left, right) of
    (HFloat a, HFloat b) -> Right (floatComparison a b)
    (HInt a, HInt b) -> Right (intComparison a b)
    (HWord a, HWord b) -> Right (wordComparison a b)
    _ -> Left (InvalidOperation "ordered comparison" [left, right])

mapArithmetic :: String -> (Float -> Float) -> (Int32 -> Int32) -> (Word32 -> Word32) -> HostValue -> Either EvalError HostValue
mapArithmetic _ floatFunction _ _ (HFloat value) = Right (HFloat (floatFunction value))
mapArithmetic _ _ intFunction _ (HInt value) = Right (HInt (intFunction value))
mapArithmetic _ _ _ wordFunction (HWord value) = Right (HWord (wordFunction value))
mapArithmetic _ floatFunction _ _ (HVector values) = Right (HVector (map floatFunction values))
mapArithmetic _ floatFunction _ _ (HMatrix outer inner values) = Right (HMatrix outer inner (map floatFunction values))
mapArithmetic name _ _ _ value = Left (InvalidOperation name [value])

mapFloatShape :: String -> (Float -> Float) -> HostValue -> Either EvalError HostValue
mapFloatShape _ function (HFloat value) = Right (HFloat (function value))
mapFloatShape _ function (HVector values) = Right (HVector (map function values))
mapFloatShape name _ value = Left (InvalidOperation name [value])

zipArithmetic :: String -> (Float -> Float -> Float) -> (Int32 -> Int32 -> Int32) -> (Word32 -> Word32 -> Word32) -> HostValue -> HostValue -> Either EvalError HostValue
zipArithmetic _ floatFunction _ _ (HFloat left) (HFloat right) = Right (HFloat (floatFunction left right))
zipArithmetic _ _ intFunction _ (HInt left) (HInt right) = Right (HInt (intFunction left right))
zipArithmetic _ _ _ wordFunction (HWord left) (HWord right) = Right (HWord (wordFunction left right))
zipArithmetic _ floatFunction _ _ (HVector left) (HVector right)
  | length left == length right = Right (HVector (zipWith floatFunction left right))
zipArithmetic _ floatFunction _ _ (HMatrix outer inner left) (HMatrix outer' inner' right)
  | outer == outer' && inner == inner' && length left == length right = Right (HMatrix outer inner (zipWith floatFunction left right))
zipArithmetic name _ _ _ left right = Left (InvalidOperation name [left, right])

zipFloatShapes :: String -> (Float -> Float -> Float) -> HostValue -> HostValue -> Either EvalError HostValue
zipFloatShapes _ function (HFloat left) (HFloat right) = Right (HFloat (function left right))
zipFloatShapes _ function (HVector left) (HVector right)
  | length left == length right = Right (HVector (zipWith function left right))
zipFloatShapes name _ left right = Left (InvalidOperation name [left, right])

ensureNoZero :: HostValue -> Either EvalError ()
ensureNoZero value
  | containsZero value = Left DivisionByZero
  | otherwise = Right ()

containsZero :: HostValue -> Bool
containsZero (HFloat value) = value == 0
containsZero (HInt value) = value == 0
containsZero (HWord value) = value == 0
containsZero (HVector values) = 0 `elem` values
containsZero (HWordVector values) = 0 `elem` values
containsZero (HMatrix _ _ values) = 0 `elem` values
containsZero (HBool _) = False

normalizeValue :: HostValue -> Either EvalError HostValue
normalizeValue (HVector values) =
  let magnitude = sqrt (sum (map (\value -> value * value) values))
   in if magnitude == 0
        then Right (HVector values)
        else Right (HVector (map (/ magnitude) values))
normalizeValue value = Left (InvalidOperation "normalize" [value])

dotValues :: HostValue -> HostValue -> Either EvalError Float
dotValues (HVector left) (HVector right)
  | length left == length right = Right (sum (zipWith (*) left right))
dotValues left right = Left (InvalidOperation "dot" [left, right])

crossValues :: HostValue -> HostValue -> Either EvalError HostValue
crossValues (HVector [a, b, c]) (HVector [d, e, f]) =
  Right (HVector [b * f - c * e, c * d - a * f, a * e - b * d])
crossValues left right = Left (InvalidOperation "cross" [left, right])

reflectValues :: HostValue -> HostValue -> Either EvalError HostValue
reflectValues incident@(HVector incidentValues) normal@(HVector normalValues)
  | length incidentValues == length normalValues = do
      projection <- dotValues incident normal
      pure (HVector (zipWith (\i n -> i - 2 * projection * n) incidentValues normalValues))
reflectValues incident normal = Left (InvalidOperation "reflect" [incident, normal])

smoothstepValues :: HostValue -> HostValue -> HostValue -> Either EvalError HostValue
smoothstepValues low high value = do
  numerator <- zipFloatShapes "smoothstep" (-) value low
  denominator <- zipFloatShapes "smoothstep" (-) high low
  ensureNoZero denominator
  ratio <- zipFloatShapes "smoothstep" (/) numerator denominator
  clamped <- mapFloatShape "smoothstep" (max 0 . min 1) ratio
  squared <- zipFloatShapes "smoothstep" (*) clamped clamped
  cubicFactor <- mapFloatShape "smoothstep" (\component -> 3 - 2 * component) clamped
  zipFloatShapes "smoothstep" (*) squared cubicFactor

multiplyMatrices :: HostValue -> HostValue -> Either EvalError HostValue
multiplyMatrices left@(HMatrix outer inner leftValues) right@(HMatrix rightOuter resultInner rightValues)
  | inner == rightOuter
      && length leftValues == outer * inner
      && length rightValues == rightOuter * resultInner = do
      values <- traverse resultAt [(row, column) | row <- [0 .. outer - 1], column <- [0 .. resultInner - 1]]
      pure (HMatrix outer resultInner values)
  | otherwise = Left (InvalidOperation "matrix multiply" [left, right])
 where
  resultAt (row, column) = sum <$> traverse (term row column) [0 .. inner - 1]
  term row column shared = (*) <$> matrixAt inner leftValues row shared <*> matrixAt resultInner rightValues shared column
multiplyMatrices left right = Left (InvalidOperation "matrix multiply" [left, right])

multiplyMatrixVector :: HostValue -> HostValue -> Either EvalError HostValue
multiplyMatrixVector matrix@(HMatrix outer inner values) vector@(HVector components)
  | length values == outer * inner && length components == inner = do
      result <- traverse rowValue [0 .. outer - 1]
      pure (HVector result)
  | otherwise = Left (InvalidOperation "matrix vector multiply" [matrix, vector])
 where
  rowValue row = sum <$> traverse (term row) [0 .. inner - 1]
  term row column = (*) <$> matrixAt inner values row column <*> indexAt column components
multiplyMatrixVector matrix vector = Left (InvalidOperation "matrix vector multiply" [matrix, vector])

matrixAt :: Int -> [Float] -> Int -> Int -> Either EvalError Float
matrixAt inner values outerIndex innerIndex = indexAt (outerIndex * inner + innerIndex) values

expectFloat :: HostValue -> Either EvalError Float
expectFloat (HFloat value) = Right value
expectFloat value = Left (TypeMismatch TyFloat value)

expectBool :: HostValue -> Either EvalError Bool
expectBool (HBool value) = Right value
expectBool value = Left (TypeMismatch TyBool value)

indexAt :: Int -> [a] -> Either EvalError a
indexAt index values
  | index < 0 = Left (InvalidOperation "negative vector index" [])
  | otherwise = findIndex index values
 where
  findIndex _ [] = Left (InvalidOperation "vector index out of bounds" [])
  findIndex 0 (value : _) = Right value
  findIndex remaining (_ : rest) = findIndex (remaining - 1) rest
