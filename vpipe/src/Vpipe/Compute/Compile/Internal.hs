{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vpipe.Compute.Compile.Internal (
  Dispatch (..),
  CompiledCompute (..),
  compileCompute,
  workgroupCounts,
  resolveComputeBindings,
  resolveComputePushConstants,
) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (runStateT)
import Data.ByteString qualified as ByteString
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (castPtr)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Vpipe.Buffer.Format qualified as Buffer
import Vpipe.Compute.IR.Internal
import Vpipe.Diagnostics.Dump.Internal (ShaderDump (..), ShaderDumpStage (DumpCompute), dumpCompiledModule)
import Vpipe.Expr.Internal (BinderId (..), ShaderTy (..), SomeExpr)
import Vpipe.Expr.Reify
import Vpipe.Pipeline.Internal qualified as Pipeline
import Vpipe.SpirV.Assembler (SpirVModule)
import Vpipe.SpirV.Codegen qualified as Codegen

data Dispatch (x :: Nat) (y :: Nat) (z :: Nat) = Dispatch
  deriving stock (Eq, Show)

type role Dispatch nominal nominal nominal

data CompiledCompute env (x :: Nat) (y :: Nat) (z :: Nat) = CompiledCompute
  { compiledComputeDispatch :: Dispatch x y z
  , compiledComputeModule :: SpirVModule
  , compiledComputeInterface :: Pipeline.PipelineInterface
  , compiledComputeBindingPlan :: Pipeline.BindingPlan env
  , compiledComputePushConstantPlan :: Pipeline.PushConstantPlan env
  , compiledComputeForest :: ReifiedForest
  , compiledComputeActions :: [Codegen.ShaderAction]
  }

type role CompiledCompute nominal nominal nominal nominal

compileCompute :: forall x y z env. (KnownNat x, KnownNat y, KnownNat z) => Dispatch x y z -> ComputeM env () -> IO (Either ComputeCompileError (CompiledCompute env x y z))
compileCompute dispatch computation = case validateDispatch dispatch of
  Left error' -> pure (Left error')
  Right localSize -> case runStateT (unComputeM computation) emptyComputeRecorder of
    Left error' -> pure (Left error')
    Right (_, recorder) -> do
      reified <- reifyStatements (computeStatements recorder)
      let result = do
            (forest, actions) <- reified
            let expectedRoots = concatMap actionRoots actions
            when (expectedRoots /= forestRoots forest) $
              Left (ComputeRootMismatch (forestRoots forest) expectedRoots)
            (pushDeclaration, pushRanges, pushPlan) <- buildPushConstants (computePushResources recorder)
            let accesses = inferAccess forest actions
                storageDeclarations = zipWith (storageDeclaration accesses) [0 ..] (computeStorageResources recorder)
                resources = fmap Codegen.StorageBufferResource storageDeclarations <> maybe [] (pure . Codegen.PushConstantResource) pushDeclaration
                shader =
                  Codegen.ShaderModule
                    { Codegen.shaderCodegenConfig = Codegen.defaultCodegenConfig
                    , Codegen.shaderStage = Codegen.ComputeShader
                    , Codegen.shaderEntryPoint = "main"
                    , Codegen.shaderLocalSize = Just localSize
                    , Codegen.shaderInputs =
                        [ Codegen.StageInput
                            "globalInvocationId"
                            (Codegen.ExpressionType (TyWordVector 3))
                            (Codegen.BuiltIn Codegen.GlobalInvocationId)
                        ]
                    , Codegen.shaderOutputs = []
                    , Codegen.shaderResources = resources
                    , Codegen.shaderForest = forest
                    , Codegen.shaderActions = actions
                    }
            spirV <- either (Left . ComputeCodegenError) Right (Codegen.compileShaderModule shader)
            let bindings = zipWith (resourceBinding accesses) [0 ..] (computeStorageResources recorder)
                interface = Pipeline.PipelineInterface [] [] bindings [] [] pushRanges
                bindingPlan = Pipeline.BindingPlan (zipWith storageResolver [0 ..] (computeStorageResources recorder))
            Right
              CompiledCompute
                { compiledComputeDispatch = dispatch
                , compiledComputeModule = spirV
                , compiledComputeInterface = interface
                , compiledComputeBindingPlan = bindingPlan
                , compiledComputePushConstantPlan = pushPlan
                , compiledComputeForest = forest
                , compiledComputeActions = actions
                }
      case result of
        Left _ -> pure result
        Right compiled -> do
          dumpCompiledModule
            ShaderDump
              { shaderDumpName = localSizeName localSize
              , shaderDumpStage = DumpCompute
              , shaderDumpModule = compiledComputeModule compiled
              , shaderDumpInterface = Pipeline.renderPipelineInterfaceTable (compiledComputeInterface compiled)
              }
          pure result

resolveComputeBindings :: CompiledCompute env x y z -> env -> Either Pipeline.PipelineError Pipeline.ResolvedBindingPlan
resolveComputeBindings compiled = Pipeline.resolveBindingPlan (compiledComputeBindingPlan compiled)

resolveComputePushConstants :: CompiledCompute env x y z -> env -> IO [Pipeline.ResolvedPushConstant]
resolveComputePushConstants compiled = Pipeline.resolvePushConstantPlan (compiledComputePushConstantPlan compiled)

workgroupCounts :: forall x y z. (KnownNat x, KnownNat y, KnownNat z) => Dispatch x y z -> (Integer, Integer, Integer) -> Either ComputeCompileError (Word32, Word32, Word32)
workgroupCounts dispatch (totalX, totalY, totalZ) = do
  Codegen.LocalSize x y z <- validateDispatch dispatch
  when (any (< 0) [totalX, totalY, totalZ]) $
    Left (InvalidWorkload "element totals must be non-negative")
  (,,) <$> groups x totalX <*> groups y totalY <*> groups z totalZ
 where
  groups local total =
    let count = if total == 0 then 0 else 1 + ((total - 1) `div` fromIntegral local)
     in if count > fromIntegral (maxBound :: Word32)
          then Left WorkgroupCountOverflow
          else Right (fromIntegral count)

localSizeName :: Codegen.LocalSize -> String
localSizeName (Codegen.LocalSize x y z) =
  "compute-" <> show x <> "x" <> show y <> "x" <> show z

validateDispatch :: forall x y z. (KnownNat x, KnownNat y, KnownNat z) => Dispatch x y z -> Either ComputeCompileError Codegen.LocalSize
validateDispatch _ = do
  x <- dimension (natVal (Proxy @x))
  y <- dimension (natVal (Proxy @y))
  z <- dimension (natVal (Proxy @z))
  Right (Codegen.LocalSize x y z)
 where
  dimension value
    | value <= 0 = Left (InvalidDispatch "local dimensions must be positive")
    | value > toInteger (maxBound :: Word32) = Left (InvalidDispatch "local dimension exceeds Word32")
    | otherwise = Right (fromInteger value)

data ForestOffsets = ForestOffsets
  { nodeOffset :: Int
  , regionOffset :: Int
  , binderOffset :: Int
  }

emptyForest :: ReifiedForest
emptyForest = ReifiedForest [] [] []

reifyStatements :: [ComputeStatement] -> IO (Either ComputeCompileError (ReifiedForest, [Codegen.ShaderAction]))
reifyStatements statements = runExceptT $ do
  (_, forest, actions) <- go (ForestOffsets 0 0 0) emptyForest statements
  pure (forest, actions)
 where
  go offsets forest [] = pure (offsets, forest, [])
  go offsets forest (statement : rest) = do
    (afterStatement, withStatement, action) <- reifyStatement offsets forest statement
    (finalOffsets, finalForest, actions) <- go afterStatement withStatement rest
    pure (finalOffsets, finalForest, action : actions)

reifyStatement :: ForestOffsets -> ReifiedForest -> ComputeStatement -> ExceptT ComputeCompileError IO (ForestOffsets, ReifiedForest, Codegen.ShaderAction)
reifyStatement offsets forest statement = case statement of
  WriteStatement symbol index value -> do
    (offsets', forest', roots) <- appendExpressions offsets forest [index, value]
    case roots of
      [indexRoot, valueRoot] -> pure (offsets', forest', Codegen.StoreStorage symbol indexRoot valueRoot)
      _ -> throwError (ComputeRootArityMismatch 2 (length roots))
  AtomicAddStatement symbol index value -> do
    (offsets', forest', roots) <- appendExpressions offsets forest [index, value]
    case roots of
      [indexRoot, valueRoot] -> pure (offsets', forest', Codegen.AtomicAddStorage symbol indexRoot valueRoot)
      _ -> throwError (ComputeRootArityMismatch 2 (length roots))
  WhenStatement condition body -> do
    (afterCondition, withCondition, roots) <- appendExpressions offsets forest [condition]
    conditionRoot <- case roots of
      [root] -> pure root
      _ -> throwError (ComputeRootArityMismatch 1 (length roots))
    (afterBody, withBody, bodyActions) <- reifyBody afterCondition withCondition body
    pure (afterBody, withBody, Codegen.ComputeWhen conditionRoot bodyActions)
 where
  reifyBody currentOffsets currentForest [] = pure (currentOffsets, currentForest, [])
  reifyBody currentOffsets currentForest (current : rest) = do
    (nextOffsets, nextForest, action) <- reifyStatement currentOffsets currentForest current
    (finalOffsets, finalForest, actions) <- reifyBody nextOffsets nextForest rest
    pure (finalOffsets, finalForest, action : actions)

appendExpressions :: ForestOffsets -> ReifiedForest -> [SomeExpr] -> ExceptT ComputeCompileError IO (ForestOffsets, ReifiedForest, [NodeId])
appendExpressions offsets accumulated expressions = do
  local <- liftIO (reifyExprForest expressions)
  let shifted = shiftForest offsets local
      nextOffsets =
        ForestOffsets
          { nodeOffset = nodeOffset offsets + length (forestNodes local)
          , regionOffset = regionOffset offsets + length (forestRegions local)
          , binderOffset = binderOffset offsets + binderCount local
          }
      combined =
        ReifiedForest
          { forestRoots = forestRoots accumulated <> forestRoots shifted
          , forestNodes = forestNodes accumulated <> forestNodes shifted
          , forestRegions = forestRegions accumulated <> forestRegions shifted
          }
  pure (nextOffsets, combined, forestRoots shifted)

shiftForest :: ForestOffsets -> ReifiedForest -> ReifiedForest
shiftForest offsets forest =
  ReifiedForest
    { forestRoots = fmap shiftNode (forestRoots forest)
    , forestNodes = fmap shiftReifiedNode (forestNodes forest)
    , forestRegions = fmap shiftRegion (forestRegions forest)
    }
 where
  shiftNode (NodeId value) = NodeId (value + nodeOffset offsets)
  shiftRegionId (RegionId value) = RegionId (value + regionOffset offsets)
  shiftBinder (BinderId value) = BinderId (value + binderOffset offsets)
  shiftReifiedNode node =
    node
      { reifiedId = shiftNode (reifiedId node)
      , reifiedOp = shiftOperation (reifiedOp node)
      }
  shiftRegion region =
    region
      { regionId = shiftRegionId (regionId region)
      , regionBinder = fmap shiftBinder (regionBinder region)
      , regionRoot = shiftNode (regionRoot region)
      }
  shiftOperation operation = case operation of
    RLiteral value -> RLiteral value
    RInput symbol -> RInput symbol
    RLocal binder -> RLocal (shiftBinder binder)
    RResource symbol -> RResource symbol
    RStorageRead symbol index -> RStorageRead symbol (shiftNode index)
    RStorageLength symbol -> RStorageLength symbol
    RUnary op child -> RUnary op (shiftNode child)
    RBinary op left right -> RBinary op (shiftNode left) (shiftNode right)
    RCompare op left right -> RCompare op (shiftNode left) (shiftNode right)
    RConstruct children -> RConstruct (fmap shiftNode children)
    RExtract indices child -> RExtract indices (shiftNode child)
    RSelect condition yes no -> RSelect (shiftNode condition) (shiftNode yes) (shiftNode no)
    RBranch condition yes no -> RBranch (shiftNode condition) (shiftRegionId yes) (shiftRegionId no)
    RWhile initial binder predicate step -> RWhile (shiftNode initial) (shiftBinder binder) (shiftRegionId predicate) (shiftRegionId step)
    RMix left right factor -> RMix (shiftNode left) (shiftNode right) (shiftNode factor)
    RSmoothstep edge0 edge1 value -> RSmoothstep (shiftNode edge0) (shiftNode edge1) (shiftNode value)
    RSample kind mode image sampler coordinates reference lod ->
      RSample kind mode (shiftNode image) (shiftNode sampler) (shiftNode coordinates) (fmap shiftNode reference) (fmap shiftNode lod)

binderCount :: ReifiedForest -> Int
binderCount forest = case binderIds of
  [] -> 0
  values -> 1 + maximum values
 where
  binderIds =
    [value | ReifiedNode _ _ (RLocal (BinderId value)) <- forestNodes forest]
      <> [value | ReifiedNode _ _ (RWhile _ (BinderId value) _ _) <- forestNodes forest]
      <> [value | ReifiedRegion _ (Just (BinderId value)) _ <- forestRegions forest]

actionRoots :: Codegen.ShaderAction -> [NodeId]
actionRoots action = case action of
  Codegen.StoreOutput _ root -> [root]
  Codegen.DiscardWhen root -> [root]
  Codegen.StoreStorage _ index value -> [index, value]
  Codegen.AtomicAddStorage _ index value -> [index, value]
  Codegen.ComputeWhen condition body -> condition : concatMap actionRoots body

data AccessUse = AccessUse
  { usedRead :: Bool
  , usedWrite :: Bool
  , usedAtomic :: Bool
  }

inferAccess :: ReifiedForest -> [Codegen.ShaderAction] -> Map.Map String Pipeline.StorageAccess
inferAccess forest actions = Map.map classify (foldr noteAction readUses actions)
 where
  readUses = foldr noteRead Map.empty (forestNodes forest)
  noteRead node uses = case reifiedOp node of
    RStorageRead symbol _ -> mark symbol (\use -> use{usedRead = True}) uses
    RStorageLength _ -> uses
    _ -> uses
  noteAction action uses = case action of
    Codegen.StoreStorage symbol _ _ -> mark symbol (\use -> use{usedWrite = True}) uses
    Codegen.AtomicAddStorage symbol _ _ -> mark symbol (\use -> use{usedAtomic = True}) uses
    Codegen.ComputeWhen _ body -> foldr noteAction uses body
    _ -> uses
  mark symbol update = Map.alter (Just . update . fromMaybe (AccessUse False False False)) symbol
  classify use
    | usedAtomic use = Pipeline.StorageAtomic
    | usedRead use && usedWrite use = Pipeline.StorageReadWrite
    | usedWrite use = Pipeline.StorageWriteOnly
    | otherwise = Pipeline.StorageReadOnly

storageDeclaration :: Map.Map String Pipeline.StorageAccess -> Int -> StorageResource env -> Codegen.StorageBufferDeclaration
storageDeclaration accesses binding resource =
  Codegen.StorageBufferDeclaration
    { Codegen.storageBufferName = storageResourceSymbol resource
    , Codegen.storageBufferLocation = Codegen.DescriptorLocation 0 (fromIntegral binding)
    , Codegen.storageBufferElementType = storageResourceType resource
    , Codegen.storageBufferElementLayout = storageResourceLayout resource
    , Codegen.storageBufferAccess = codegenAccess (Map.findWithDefault Pipeline.StorageReadOnly (storageResourceSymbol resource) accesses)
    }

resourceBinding :: Map.Map String Pipeline.StorageAccess -> Int -> StorageResource env -> Pipeline.ResourceBinding
resourceBinding accesses binding resource =
  Pipeline.ResourceBinding
    { Pipeline.resourceBindingName = storageResourceSymbol resource
    , Pipeline.resourceBindingSet = 0
    , Pipeline.resourceBindingBinding = binding
    , Pipeline.resourceBindingShape =
        Pipeline.StorageArrayShape
          (storageResourceType resource)
          (storageResourceLayout resource)
          (Map.findWithDefault Pipeline.StorageReadOnly (storageResourceSymbol resource) accesses)
    }

storageResolver :: Int -> StorageResource env -> Pipeline.EnvironmentResolver env
storageResolver binding (StorageResource symbol accessor _ _) =
  Pipeline.ResolveStorageBuffer $ \environment ->
    Pipeline.ResolvedStorageBuffer
      symbol
      0
      binding
      (Pipeline.storageBufferHandle (accessor environment))

codegenAccess :: Pipeline.StorageAccess -> Codegen.StorageAccess
codegenAccess access = case access of
  Pipeline.StorageReadOnly -> Codegen.StorageReadOnly
  Pipeline.StorageWriteOnly -> Codegen.StorageWriteOnly
  Pipeline.StorageReadWrite -> Codegen.StorageReadWrite
  Pipeline.StorageAtomic -> Codegen.StorageAtomic

buildPushConstants :: [PushResource env] -> Either ComputeCompileError (Maybe Codegen.PushConstantDeclaration, [Pipeline.PushConstantRange], Pipeline.PushConstantPlan env)
buildPushConstants [] = Right (Nothing, [], Pipeline.PushConstantPlan [])
buildPushConstants resources = do
  let fields = fmap pushResourceLayout resources
      structLayout = Buffer.Struct fields
      layout = Buffer.layoutOf Buffer.Std430 structLayout
      offsets = Buffer.layoutFieldOffsets layout
      totalSize = Buffer.layoutSize layout
  when (totalSize > 128) (Left (PushConstantLimitExceeded totalSize))
  let leaves = zipWith3 pushLeaf [0 :: Int ..] offsets resources
      declaration = Codegen.PushConstantDeclaration "compute.push" structLayout (fmap (\(leaf, _, _) -> leaf) leaves)
      ranges = fmap (\(_, range, _) -> range) leaves
      resolvers = fmap (\(_, _, resolver) -> resolver) leaves
  Right (Just declaration, ranges, Pipeline.PushConstantPlan resolvers)
 where
  pushLeaf index offset resource =
    let size = pushSize resource
        leaf = Codegen.UniformLeaf (pushResourceSymbol resource) [fromIntegral index] (pushResourceType resource)
        range = Pipeline.PushConstantRange (pushResourceSymbol resource) offset size (pushResourceType resource) (pushResourceLayout resource)
     in (leaf, range, resolvePush offset size resource)

pushSize :: PushResource env -> Int
pushSize (PushResource _ (_ :: env -> a) _ _) = Buffer.bufferSizeFor Buffer.Std430 (Proxy @a)

resolvePush :: Int -> Int -> PushResource env -> env -> IO Pipeline.ResolvedPushConstant
resolvePush offset size (PushResource symbol (accessor :: env -> a) _ _) environment = do
  bytes <- allocaBytes size $ \pointer -> do
    fillBytes pointer 0 size
    Buffer.pokeBufferFor Buffer.Std430 (Proxy @a) (castPtr pointer) (accessor environment)
    ByteString.packCStringLen (castPtr pointer, size)
  pure (Pipeline.ResolvedPushConstant symbol offset bytes)
