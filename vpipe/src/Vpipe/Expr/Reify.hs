{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_HADDOCK hide #-}

{- | Stable-name reification of pure expression objects into a region-aware
graph. Node IDs are deterministic left-to-right post-order IDs.

Reify a root expression when inspecting or lowering a shader graph:

@
module Main (main) where

import Vpipe.Expr (Expr, constant)
import Vpipe.Expr.Reify (reifyExpr)

expression :: Expr s Float
expression = constant 1

main :: IO ()
main = reifyExpr expression >>= print
@
-}
module Vpipe.Expr.Reify (
  NodeId (..),
  RegionId (..),
  ReifiedExpr (..),
  ReifiedForest (..),
  ReifiedRegion (..),
  ReifiedNode (..),
  ReifiedOp (..),
  ReifyError (..),
  reifyExpr,
  reifyExprForest,
  nodeChildren,
) where

import Control.Exception (Exception, throwIO)
import Data.IORef
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import GHC.StableName (StableName, eqStableName, hashStableName, makeStableName)
import Vpipe.Expr.Internal

newtype NodeId = NodeId Int
  deriving stock (Eq, Ord, Show)

newtype RegionId = RegionId Int
  deriving stock (Eq, Ord, Show)

data ReifiedExpr = ReifiedExpr
  { reifiedRoot :: NodeId
  , reifiedNodes :: [ReifiedNode]
  , reifiedRegions :: [ReifiedRegion]
  }
  deriving stock (Eq, Show)

data ReifiedForest = ReifiedForest
  { forestRoots :: [NodeId]
  , forestNodes :: [ReifiedNode]
  , forestRegions :: [ReifiedRegion]
  }
  deriving stock (Eq, Show)

data ReifiedRegion = ReifiedRegion
  { regionId :: RegionId
  , regionBinder :: Maybe BinderId
  , regionRoot :: NodeId
  }
  deriving stock (Eq, Show)

data ReifiedNode = ReifiedNode
  { reifiedId :: NodeId
  , reifiedTy :: ShaderTy
  , reifiedOp :: ReifiedOp
  }
  deriving stock (Eq, Show)

data ReifiedOp
  = RLiteral HostValue
  | RInput String
  | RLocal BinderId
  | RResource String
  | RStorageRead String NodeId
  | RStorageLength String
  | RUnary UnaryOp NodeId
  | RBinary BinaryOp NodeId NodeId
  | RCompare CompareOp NodeId NodeId
  | RConstruct [NodeId]
  | RExtract [Int] NodeId
  | RSelect NodeId NodeId NodeId
  | RBranch NodeId RegionId RegionId
  | RWhile NodeId BinderId RegionId RegionId
  | RMix NodeId NodeId NodeId
  | RSmoothstep NodeId NodeId NodeId
  | RSample SamplingKind SamplingMode NodeId NodeId NodeId (Maybe NodeId) (Maybe NodeId)
  deriving stock (Eq, Show)

data Seen = Seen (StableName ExprObject) NodeId
type Buckets = IntMap.IntMap [Seen]

data ReifyError = ReifyCycle
  deriving stock (Eq, Show)

instance Exception ReifyError

data ReifyState = ReifyState
  { seenObjects :: IORef Buckets
  , activeObjects :: IORef Buckets
  , completedNodes :: IORef [ReifiedNode]
  , completedRegions :: IORef [ReifiedRegion]
  , nextNode :: IORef Int
  , nextRegion :: IORef Int
  , nextBinder :: IORef Int
  }

reifyExpr :: Expr s a -> IO ReifiedExpr
reifyExpr expression = do
  state <- newState
  root <- visit state expression
  nodes <- reverse <$> readIORef (completedNodes state)
  regions <- sortOn regionId <$> readIORef (completedRegions state)
  pure
    ReifiedExpr
      { reifiedRoot = root
      , reifiedNodes = nodes
      , reifiedRegions = regions
      }

reifyExprForest :: [SomeExpr] -> IO ReifiedForest
reifyExprForest expressions = do
  state <- newState
  roots <- traverse (visitSome state) expressions
  nodes <- reverse <$> readIORef (completedNodes state)
  regions <- sortOn regionId <$> readIORef (completedRegions state)
  pure
    ReifiedForest
      { forestRoots = roots
      , forestNodes = nodes
      , forestRegions = regions
      }

newState :: IO ReifyState
newState =
  ReifyState
    <$> newIORef IntMap.empty
    <*> newIORef IntMap.empty
    <*> newIORef []
    <*> newIORef []
    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef 0

visitSome :: ReifyState -> SomeExpr -> IO NodeId
visitSome state (SomeExpr expression) = visit state expression

visit :: ReifyState -> Expr s a -> IO NodeId
visit state expression@(Expr object) = do
  stable <- makeStableName object
  known <- lookupStable (seenObjects state) stable
  case known of
    Just identifier -> pure identifier
    Nothing -> do
      active <- lookupStable (activeObjects state) stable
      case active of
        Just _ -> throwIO ReifyCycle
        Nothing -> pure ()
      insertStable (activeObjects state) stable (NodeId (-1))
      operation <- reifyNode state (objectNode object)
      identifier <- freshNode state
      insertStable (seenObjects state) stable identifier
      removeStable (activeObjects state) stable
      modifyIORef' (completedNodes state) (ReifiedNode identifier (shaderTy expression) operation :)
      pure identifier

reifyNode :: ReifyState -> ExprNode -> IO ReifiedOp
reifyNode state node = case node of
  LiteralNode value -> pure (RLiteral value)
  InputNode name -> pure (RInput name)
  LocalNode binder -> pure (RLocal binder)
  ResourceNode name -> pure (RResource name)
  StorageReadNode name index -> RStorageRead name <$> visitSome state index
  StorageLengthNode name -> pure (RStorageLength name)
  UnaryNode operation child -> RUnary operation <$> visitSome state child
  BinaryNode operation left right -> RBinary operation <$> visitSome state left <*> visitSome state right
  CompareNode operation left right -> RCompare operation <$> visitSome state left <*> visitSome state right
  ConstructNode components -> RConstruct <$> traverse (visitSome state) components
  ExtractNode indices value -> RExtract indices <$> visitSome state value
  SelectNode condition yes no -> RSelect <$> visitSome state condition <*> visitSome state yes <*> visitSome state no
  BranchNode condition yes no -> do
    conditionId <- visitSome state condition
    yesRegion <- reifyRegion state Nothing yes
    noRegion <- reifyRegion state Nothing no
    pure (RBranch conditionId yesRegion noRegion)
  WhileNode specification -> reifyLoop state specification
  MixNode left right factor -> RMix <$> visitSome state left <*> visitSome state right <*> visitSome state factor
  SmoothstepNode edge0 edge1 value -> RSmoothstep <$> visitSome state edge0 <*> visitSome state edge1 <*> visitSome state value
  SampleNode kind mode image sampler coordinates reference lod ->
    RSample kind mode
      <$> visitSome state image
      <*> visitSome state sampler
      <*> visitSome state coordinates
      <*> traverse (visitSome state) reference
      <*> traverse (visitSome state) lod

reifyLoop :: ReifyState -> LoopSpec -> IO ReifiedOp
reifyLoop state (LoopSpec initial predicate step) = do
  initialId <- visit state initial
  binder <- freshBinder state
  let parameter = local (shaderTy initial) binder
      predicateExpression = predicate parameter
      stepExpression = step parameter
  predicateRegion <- reifyRegion state (Just binder) (SomeExpr predicateExpression)
  stepRegion <- reifyRegion state (Just binder) (SomeExpr stepExpression)
  pure (RWhile initialId binder predicateRegion stepRegion)

reifyRegion :: ReifyState -> Maybe BinderId -> SomeExpr -> IO RegionId
reifyRegion state binder expression = do
  identifier <- freshRegion state
  root <- visitSome state expression
  modifyIORef' (completedRegions state) (ReifiedRegion identifier binder root :)
  pure identifier

freshNode :: ReifyState -> IO NodeId
freshNode state = NodeId <$> atomicModifyIORef' (nextNode state) (\identifier -> (identifier + 1, identifier))

freshRegion :: ReifyState -> IO RegionId
freshRegion state = RegionId <$> atomicModifyIORef' (nextRegion state) (\identifier -> (identifier + 1, identifier))

freshBinder :: ReifyState -> IO BinderId
freshBinder state = BinderId <$> atomicModifyIORef' (nextBinder state) (\identifier -> (identifier + 1, identifier))

lookupStable :: IORef Buckets -> StableName ExprObject -> IO (Maybe NodeId)
lookupStable reference stable = do
  buckets <- readIORef reference
  pure (findStable (IntMap.findWithDefault [] (hashStableName stable) buckets))
 where
  findStable [] = Nothing
  findStable (Seen candidate identifier : rest)
    | eqStableName candidate stable = Just identifier
    | otherwise = findStable rest

insertStable :: IORef Buckets -> StableName ExprObject -> NodeId -> IO ()
insertStable reference stable identifier =
  modifyIORef' reference (IntMap.insertWith (<>) (hashStableName stable) [Seen stable identifier])

removeStable :: IORef Buckets -> StableName ExprObject -> IO ()
removeStable reference stable =
  modifyIORef' reference (IntMap.update removeBucket (hashStableName stable))
 where
  removeBucket bucket = case filter (not . isStable) bucket of
    [] -> Nothing
    remaining -> Just remaining
  isStable (Seen candidate _) = eqStableName candidate stable

nodeChildren :: ReifiedOp -> [NodeId]
nodeChildren operation = case operation of
  RLiteral _ -> []
  RInput _ -> []
  RLocal _ -> []
  RResource _ -> []
  RStorageRead _ index -> [index]
  RStorageLength _ -> []
  RUnary _ child -> [child]
  RBinary _ left right -> [left, right]
  RCompare _ left right -> [left, right]
  RConstruct children -> children
  RExtract _ child -> [child]
  RSelect condition yes no -> [condition, yes, no]
  RBranch condition _ _ -> [condition]
  RWhile initial _ _ _ -> [initial]
  RMix left right factor -> [left, right, factor]
  RSmoothstep edge0 edge1 value -> [edge0, edge1, value]
  RSample _ _ image sampler coordinates reference lod ->
    [image, sampler, coordinates] <> maybe [] pure reference <> maybe [] pure lod
