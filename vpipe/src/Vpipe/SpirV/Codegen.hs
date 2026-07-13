{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_HADDOCK hide #-}

{- | Standalone lowering from reified shader expressions to SPIR-V 1.6.

Code generation consumes a reified expression forest and an explicit stage
configuration, returning either a validated shader module or @CodegenError@.
Debug names are enabled in the default deterministic configuration:

@
module Main (main) where

import Vpipe.SpirV.Codegen (codegenDebugNames, defaultCodegenConfig)

main :: IO ()
main = print (codegenDebugNames defaultCodegenConfig)
@
-}
module Vpipe.SpirV.Codegen (
  CodegenConfig (..),
  defaultCodegenConfig,
  ShaderStage (..),
  LocalSize (..),
  Interpolation (..),
  BuiltIn (..),
  InterfaceSlot (..),
  InterfaceValueType (..),
  StageInput (..),
  StageOutput (..),
  DescriptorLocation (..),
  UniformLeaf (..),
  UniformBlockDeclaration (..),
  PushConstantDeclaration (..),
  CombinedImageSamplerDeclaration (..),
  ImageDeclaration (..),
  SamplerDeclaration (..),
  StorageAccess (..),
  StorageBufferDeclaration (..),
  ResourceDeclaration (..),
  ShaderAction (..),
  ShaderModule (..),
  CodegenError (..),
  compileShaderModule,
) where

import Control.Monad (foldM, foldM_, unless, when)
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (StateT (StateT), evalStateT, gets, modify')
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Set qualified as Set
import Data.Word (Word32)
import GHC.Float (castFloatToWord32)
import Vpipe.Buffer.Format qualified as Buffer
import Vpipe.Expr.Internal (
  BinaryOp (..),
  BinderId,
  CompareOp (..),
  HostValue (..),
  ImageDimension (..),
  SamplingKind (..),
  SamplingMode (..),
  ShaderTy (..),
  UnaryOp (..),
  imageShaderType,
  shaderImageDimension,
 )
import Vpipe.Expr.Reify
import Vpipe.SpirV.Assembler qualified as SpirV
import Vpipe.SpirV.Generated qualified as SpirV

newtype CodegenConfig = CodegenConfig
  { codegenDebugNames :: Bool
  }
  deriving stock (Eq, Show)

defaultCodegenConfig :: CodegenConfig
defaultCodegenConfig = CodegenConfig{codegenDebugNames = True}

data ShaderStage = VertexShader | FragmentShader | ComputeShader
  deriving stock (Eq, Ord, Show)

data LocalSize = LocalSize Word32 Word32 Word32
  deriving stock (Eq, Show)

data Interpolation = Smooth | Flat | NoPerspective
  deriving stock (Eq, Ord, Show)

data BuiltIn
  = Position
  | FragCoord
  | FragDepth
  | VertexIndex
  | InstanceIndex
  | GlobalInvocationId
  deriving stock (Eq, Ord, Show)

data InterfaceSlot
  = Location Word32 Interpolation
  | BuiltIn BuiltIn
  deriving stock (Eq, Ord, Show)

newtype InterfaceValueType = ExpressionType ShaderTy
  deriving stock (Eq, Ord, Show)

data StageInput = StageInput
  { stageInputSymbol :: String
  , stageInputType :: InterfaceValueType
  , stageInputSlot :: InterfaceSlot
  }
  deriving stock (Eq, Show)

data StageOutput = StageOutput
  { stageOutputSymbol :: String
  , stageOutputType :: InterfaceValueType
  , stageOutputSlot :: InterfaceSlot
  }
  deriving stock (Eq, Show)

data DescriptorLocation = DescriptorLocation
  { descriptorSet :: Word32
  , descriptorBinding :: Word32
  }
  deriving stock (Eq, Ord, Show)

data UniformLeaf = UniformLeaf
  { uniformLeafSymbol :: String
  , uniformLeafPath :: [Word32]
  , uniformLeafType :: ShaderTy
  }
  deriving stock (Eq, Show)

data UniformBlockDeclaration = UniformBlockDeclaration
  { uniformBlockName :: String
  , uniformBlockLocation :: DescriptorLocation
  , uniformBlockStandard :: Buffer.LayoutStandard
  , uniformBlockLayout :: Buffer.FieldLayout
  , uniformBlockLeaves :: [UniformLeaf]
  }
  deriving stock (Eq, Show)

data PushConstantDeclaration = PushConstantDeclaration
  { pushConstantBlockName :: String
  , pushConstantLayout :: Buffer.FieldLayout
  , pushConstantLeaves :: [UniformLeaf]
  }
  deriving stock (Eq, Show)

data CombinedImageSamplerDeclaration = CombinedImageSamplerDeclaration
  { combinedDescriptorName :: String
  , combinedImageSymbol :: String
  , combinedSamplerSymbol :: String
  , combinedDescriptorLocation :: DescriptorLocation
  , combinedImageDimension :: ImageDimension
  }
  deriving stock (Eq, Show)

data ImageDeclaration = ImageDeclaration
  { imageDescriptorName :: String
  , imageSymbol :: String
  , imageDescriptorLocation :: DescriptorLocation
  , declaredImageDimension :: ImageDimension
  }
  deriving stock (Eq, Show)

data SamplerDeclaration = SamplerDeclaration
  { samplerDescriptorName :: String
  , samplerSymbol :: String
  , samplerDescriptorLocation :: DescriptorLocation
  }
  deriving stock (Eq, Show)

data StorageAccess = StorageReadOnly | StorageWriteOnly | StorageReadWrite | StorageAtomic
  deriving stock (Eq, Ord, Show)

data StorageBufferDeclaration = StorageBufferDeclaration
  { storageBufferName :: String
  , storageBufferLocation :: DescriptorLocation
  , storageBufferElementType :: ShaderTy
  , storageBufferElementLayout :: Buffer.FieldLayout
  , storageBufferAccess :: StorageAccess
  }
  deriving stock (Eq, Show)

data ResourceDeclaration
  = UniformBlockResource UniformBlockDeclaration
  | PushConstantResource PushConstantDeclaration
  | CombinedImageSamplerResource CombinedImageSamplerDeclaration
  | SeparateImageResource ImageDeclaration
  | SeparateSamplerResource SamplerDeclaration
  | StorageBufferResource StorageBufferDeclaration
  deriving stock (Eq, Show)

data ShaderAction
  = StoreOutput String NodeId
  | DiscardWhen NodeId
  | StoreStorage String NodeId NodeId
  | AtomicAddStorage String NodeId NodeId
  | ComputeWhen NodeId [ShaderAction]
  deriving stock (Eq, Show)

data ShaderModule = ShaderModule
  { shaderCodegenConfig :: CodegenConfig
  , shaderStage :: ShaderStage
  , shaderEntryPoint :: String
  , shaderLocalSize :: Maybe LocalSize
  , shaderInputs :: [StageInput]
  , shaderOutputs :: [StageOutput]
  , shaderResources :: [ResourceDeclaration]
  , shaderForest :: ReifiedForest
  , shaderActions :: [ShaderAction]
  }
  deriving stock (Eq, Show)

data CodegenError
  = DuplicateNode NodeId
  | MissingNode NodeId
  | DuplicateRegion RegionId
  | MissingRegion RegionId
  | RootWithoutAction NodeId
  | ActionWithoutRoot NodeId
  | RecursiveGraph String
  | DuplicateSymbol String
  | MissingSymbol String
  | DuplicateInterfaceSlot String InterfaceSlot
  | DuplicateDescriptor DescriptorLocation
  | InvalidNodeType NodeId String
  | InvalidStage String
  | InvalidBuiltIn String
  | InvalidLocalSize LocalSize
  | InvalidResourcePair NodeId String
  | InvalidUniformLayout String
  | UnboundLocal BinderId
  | DuplicateBinderOwner BinderId
  | InvalidLocalScope NodeId BinderId
  | MissingCurrentBlock
  | AssemblerError SpirV.AssemblerError
  deriving stock (Eq, Show)

data PreparedModule = PreparedModule
  { preparedShader :: ShaderModule
  , preparedNodes :: Map NodeId ReifiedNode
  , preparedRegions :: Map RegionId ReifiedRegion
  , preparedExpressionSymbols :: Map String ShaderTy
  , preparedHandles :: Map String HandleDeclaration
  }

data HandleDeclaration
  = CombinedHandle String String String ImageDimension
  | ImageHandle String ImageDimension
  | SamplerHandle String
  deriving stock (Eq, Show)

data GraphVertex = NodeVertex NodeId | RegionVertex RegionId
  deriving stock (Eq, Ord, Show)

compileShaderModule :: ShaderModule -> Either CodegenError SpirV.SpirVModule
compileShaderModule shader = do
  prepared <- prepareModule shader
  let assemblerConfig =
        SpirV.defaultAssemblerConfig
          { SpirV.debugNames = codegenDebugNames (shaderCodegenConfig shader)
          }
      assembled =
        SpirV.runAssemblerWith
          assemblerConfig
          (evalStateT (runExceptT emitModule) (initialEmissionState prepared))
  case assembled of
    Left assemblerError -> Left (AssemblerError assemblerError)
    Right (Left codegenError) -> Left codegenError
    Right (Right spirVModule) -> Right spirVModule

prepareModule :: ShaderModule -> Either CodegenError PreparedModule
prepareModule shader = do
  nodeMap <- uniqueMap DuplicateNode reifiedId (forestNodes forest)
  regionMap <- uniqueMap DuplicateRegion regionId (forestRegions forest)
  validateRoots forest nodeMap
  validateReferences nodeMap regionMap
  validateAcyclic forest nodeMap regionMap
  validateBinderScopes forest nodeMap regionMap
  validateLocalSize shader
  validateInterfaces shader
  validateDescriptors shader
  expressionSymbols <- expressionSymbolTable shader
  handles <- handleTable shader
  validateUniformBlocks shader
  validateActions shader nodeMap expressionSymbols
  traverse_ (validateNode shader nodeMap regionMap expressionSymbols handles) (forestNodes forest)
  pure
    PreparedModule
      { preparedShader = shader
      , preparedNodes = nodeMap
      , preparedRegions = regionMap
      , preparedExpressionSymbols = expressionSymbols
      , preparedHandles = handles
      }
 where
  forest = shaderForest shader

uniqueMap :: (Ord key) => (key -> CodegenError) -> (value -> key) -> [value] -> Either CodegenError (Map key value)
uniqueMap duplicateError keyOf = foldM insertUnique Map.empty
 where
  insertUnique values value =
    let key = keyOf value
     in if Map.member key values
          then Left (duplicateError key)
          else Right (Map.insert key value values)

validateRoots :: ReifiedForest -> Map NodeId ReifiedNode -> Either CodegenError ()
validateRoots forest nodes = do
  traverse_ requireNode (forestRoots forest)
 where
  requireNode nodeId
    | Map.member nodeId nodes = Right ()
    | otherwise = Left (MissingNode nodeId)

validateReferences :: Map NodeId ReifiedNode -> Map RegionId ReifiedRegion -> Either CodegenError ()
validateReferences nodes regions = do
  traverse_ validateNodeReferences (Map.elems nodes)
  traverse_ validateRegionReference (Map.elems regions)
 where
  validateNodeReferences node = do
    traverse_ requireNode (nodeChildren (reifiedOp node))
    traverse_ requireRegion (operationRegions (reifiedOp node))
  validateRegionReference = requireNode . regionRoot
  requireNode nodeId
    | Map.member nodeId nodes = Right ()
    | otherwise = Left (MissingNode nodeId)
  requireRegion regionIdentifier
    | Map.member regionIdentifier regions = Right ()
    | otherwise = Left (MissingRegion regionIdentifier)

validateAcyclic :: ReifiedForest -> Map NodeId ReifiedNode -> Map RegionId ReifiedRegion -> Either CodegenError ()
validateAcyclic forest nodes regions =
  foldM_ (visit Set.empty) Set.empty startingVertices
 where
  startingVertices =
    map NodeVertex (forestRoots forest)
      ++ map (NodeVertex . reifiedId) (forestNodes forest)
      ++ map (RegionVertex . regionId) (forestRegions forest)
  visit active completed vertex
    | Set.member vertex active = Left (RecursiveGraph (show vertex))
    | Set.member vertex completed = Right completed
    | otherwise = do
        visited <- foldM (visit (Set.insert vertex active)) completed (edges vertex)
        pure (Set.insert vertex visited)
  edges (NodeVertex nodeId) = case Map.lookup nodeId nodes of
    Nothing -> []
    Just node ->
      map NodeVertex (nodeChildren (reifiedOp node))
        ++ map RegionVertex (operationRegions (reifiedOp node))
  edges (RegionVertex regionIdentifier) =
    maybe [] (pure . NodeVertex . regionRoot) (Map.lookup regionIdentifier regions)

operationRegions :: ReifiedOp -> [RegionId]
operationRegions operation = case operation of
  RBranch _ yesRegion noRegion -> [yesRegion, noRegion]
  RWhile _ _ predicateRegion stepRegion -> [predicateRegion, stepRegion]
  _ -> []

validateBinderScopes :: ReifiedForest -> Map NodeId ReifiedNode -> Map RegionId ReifiedRegion -> Either CodegenError ()
validateBinderScopes forest nodes regions = do
  owners <- foldM addOwner Map.empty (Map.elems nodes)
  traverse_ (validateRegionOwner owners) (Map.elems regions)
  visited <- foldM (visitNode owners Map.empty) Set.empty (forestRoots forest)
  let reachedNodes = Set.fromList [nodeId | (nodeId, _) <- Set.toList visited]
  case find (unvisitedLocal reachedNodes) (Map.elems nodes) of
    Just node -> case reifiedOp node of
      RLocal binder -> Left (InvalidLocalScope (reifiedId node) binder)
      _ -> Right ()
    Nothing -> Right ()
 where
  addOwner owners node = case reifiedOp node of
    RWhile _ binder predicateRegion stepRegion
      | Map.member binder owners -> Left (DuplicateBinderOwner binder)
      | predicateRegion == stepRegion -> Left (InvalidNodeType (reifiedId node) "loop predicate and step must use distinct regions")
      | otherwise -> Right (Map.insert binder (reifiedId node, reifiedTy node, Set.fromList [predicateRegion, stepRegion]) owners)
    _ -> Right owners

  validateRegionOwner owners region = case regionBinder region of
    Nothing -> Right ()
    Just binder -> case Map.lookup binder owners of
      Just (_, _, ownedRegions)
        | Set.member (regionId region) ownedRegions -> Right ()
      _ -> Left (InvalidLocalScope (regionRoot region) binder)

  visitNode owners binders visited nodeId
    | Set.member context visited = Right visited
    | otherwise = do
        node <- maybe (Left (MissingNode nodeId)) Right (Map.lookup nodeId nodes)
        let withNode = Set.insert context visited
        case reifiedOp node of
          RLocal binder -> case Map.lookup binder binders of
            Nothing -> Left (InvalidLocalScope nodeId binder)
            Just expectedType
              | expectedType == reifiedTy node -> Right withNode
              | otherwise -> Left (InvalidNodeType nodeId ("local binder type mismatch: expected " ++ show expectedType ++ ", got " ++ show (reifiedTy node)))
          RBranch condition yesRegion noRegion -> do
            afterCondition <- visitNode owners binders withNode condition
            afterYes <- visitRegion owners binders Nothing afterCondition yesRegion
            visitRegion owners binders Nothing afterYes noRegion
          RWhile initial binder predicateRegion stepRegion -> do
            case Map.lookup binder owners of
              Just (owner, _, _) | owner == nodeId -> pure ()
              _ -> Left (InvalidLocalScope nodeId binder)
            afterInitial <- visitNode owners binders withNode initial
            let loopBinders = Map.insert binder (reifiedTy node) binders
            afterPredicate <- visitRegion owners loopBinders (Just binder) afterInitial predicateRegion
            visitRegion owners loopBinders (Just binder) afterPredicate stepRegion
          operation -> foldM (visitNode owners binders) withNode (nodeChildren operation)
   where
    context = (nodeId, Map.toAscList binders)

  visitRegion owners binders expectedBinder visited regionIdentifier = do
    region <- maybe (Left (MissingRegion regionIdentifier)) Right (Map.lookup regionIdentifier regions)
    unless (regionBinder region == expectedBinder) $
      Left (InvalidNodeType (regionRoot region) ("region binder mismatch: expected " ++ show expectedBinder ++ ", got " ++ show (regionBinder region)))
    visitNode owners binders visited (regionRoot region)

  unvisitedLocal reachedNodes node = case reifiedOp node of
    RLocal _ -> Set.notMember (reifiedId node) reachedNodes
    _ -> False

validateLocalSize :: ShaderModule -> Either CodegenError ()
validateLocalSize shader = case (shaderStage shader, shaderLocalSize shader) of
  (ComputeShader, Just size@(LocalSize x y z))
    | x > 0 && y > 0 && z > 0 -> Right ()
    | otherwise -> Left (InvalidLocalSize size)
  (ComputeShader, Nothing) -> Left (InvalidStage "compute shaders require a local size")
  (_, Nothing) -> Right ()
  (_, Just _) -> Left (InvalidStage "only compute shaders may declare a local size")

validateInterfaces :: ShaderModule -> Either CodegenError ()
validateInterfaces shader = do
  validateDirection "input" (shaderInputs shader) stageInputSymbol stageInputType stageInputSlot
  validateDirection "output" (shaderOutputs shader) stageOutputSymbol stageOutputType stageOutputSlot
  traverse_ (validateInputBuiltIn (shaderStage shader)) (shaderInputs shader)
  traverse_ (validateOutputBuiltIn (shaderStage shader)) (shaderOutputs shader)
  validatePositionOutput shader
  traverse_ (validateInputInterpolation (shaderStage shader)) (shaderInputs shader)
  traverse_ (validateOutputInterpolation (shaderStage shader)) (shaderOutputs shader)
 where
  validateDirection direction declarations symbolOf typeOf slotOf = do
    case firstDuplicate (map slotOf declarations) of
      Just slot -> Left (DuplicateInterfaceSlot direction slot)
      Nothing -> pure ()
    traverse_ (validateInterfaceType direction . typeOf) declarations
    case firstDuplicate (map symbolOf declarations) of
      Just symbol -> Left (DuplicateSymbol symbol)
      Nothing -> pure ()

validateInterfaceType :: String -> InterfaceValueType -> Either CodegenError ()
validateInterfaceType _ (ExpressionType TyFloat) = Right ()
validateInterfaceType _ (ExpressionType TyInt) = Right ()
validateInterfaceType _ (ExpressionType TyWord) = Right ()
validateInterfaceType _ (ExpressionType (TyVector size))
  | size `elem` [2, 3, 4] = Right ()
validateInterfaceType _ (ExpressionType (TyWordVector size))
  | size `elem` [2, 3, 4] = Right ()
validateInterfaceType direction valueType =
  Left (InvalidStage (direction ++ " does not support " ++ show valueType))

validateInputBuiltIn :: ShaderStage -> StageInput -> Either CodegenError ()
validateInputBuiltIn stage declaration = case stageInputSlot declaration of
  Location _ _ -> Right ()
  BuiltIn builtIn -> validateBuiltIn stage True builtIn (stageInputType declaration)

validateOutputBuiltIn :: ShaderStage -> StageOutput -> Either CodegenError ()
validateOutputBuiltIn stage declaration = case stageOutputSlot declaration of
  Location _ _ -> Right ()
  BuiltIn builtIn -> validateBuiltIn stage False builtIn (stageOutputType declaration)

validateBuiltIn :: ShaderStage -> Bool -> BuiltIn -> InterfaceValueType -> Either CodegenError ()
validateBuiltIn stage isInput builtIn valueType
  | (stage, isInput, builtIn, valueType) `elem` legal = Right ()
  | otherwise = Left (InvalidBuiltIn (show (stage, isInput, builtIn, valueType)))
 where
  legal =
    [ (VertexShader, False, Position, ExpressionType (TyVector 4))
    , (FragmentShader, True, FragCoord, ExpressionType (TyVector 4))
    , (FragmentShader, False, FragDepth, ExpressionType TyFloat)
    , (VertexShader, True, VertexIndex, ExpressionType TyInt)
    , (VertexShader, True, InstanceIndex, ExpressionType TyInt)
    , (ComputeShader, True, GlobalInvocationId, ExpressionType (TyWordVector 3))
    ]

validateInputInterpolation :: ShaderStage -> StageInput -> Either CodegenError ()
validateInputInterpolation stage declaration = case stageInputSlot declaration of
  BuiltIn _ -> Right ()
  Location _ interpolation
    | stage == ComputeShader -> Left (InvalidStage "compute shaders cannot declare location inputs")
    | stage == VertexShader && interpolation /= Smooth -> Left (InvalidStage "vertex inputs cannot be interpolated")
    | stage == FragmentShader && requiresFlat (stageInputType declaration) && interpolation /= Flat ->
        Left (InvalidStage "fragment integer inputs require Flat interpolation")
    | stage == FragmentShader && not (requiresFlat (stageInputType declaration)) && interpolation == Flat -> Right ()
    | otherwise -> Right ()

requiresFlat :: InterfaceValueType -> Bool
requiresFlat (ExpressionType shaderType) = case shaderType of
  TyInt -> True
  TyWord -> True
  TyWordVector _ -> True
  _ -> False

validateOutputInterpolation :: ShaderStage -> StageOutput -> Either CodegenError ()
validateOutputInterpolation stage declaration = case stageOutputSlot declaration of
  BuiltIn _ -> Right ()
  Location _ interpolation
    | stage == ComputeShader -> Left (InvalidStage "compute shaders cannot declare location outputs")
    | stage == FragmentShader && interpolation /= Smooth -> Left (InvalidStage "fragment outputs cannot be interpolated")
    | otherwise -> Right ()

validatePositionOutput :: ShaderModule -> Either CodegenError ()
validatePositionOutput shader
  | shaderStage shader /= VertexShader = Right ()
  | any isPosition (shaderOutputs shader) = Right ()
  | otherwise = Left (InvalidBuiltIn "vertex shaders require a V4 Position output")
 where
  isPosition declaration =
    stageOutputSlot declaration == BuiltIn Position
      && stageOutputType declaration == ExpressionType (TyVector 4)

validateDescriptors :: ShaderModule -> Either CodegenError ()
validateDescriptors shader = case firstDuplicate locations of
  Just location -> Left (DuplicateDescriptor location)
  Nothing -> Right ()
 where
  locations = mapMaybe resourceLocation (shaderResources shader)

resourceLocation :: ResourceDeclaration -> Maybe DescriptorLocation
resourceLocation resource = case resource of
  UniformBlockResource declaration -> Just (uniformBlockLocation declaration)
  PushConstantResource _ -> Nothing
  CombinedImageSamplerResource declaration -> Just (combinedDescriptorLocation declaration)
  SeparateImageResource declaration -> Just (imageDescriptorLocation declaration)
  SeparateSamplerResource declaration -> Just (samplerDescriptorLocation declaration)
  StorageBufferResource declaration -> Just (storageBufferLocation declaration)

expressionSymbolTable :: ShaderModule -> Either CodegenError (Map String ShaderTy)
expressionSymbolTable shader = do
  let interfaceSymbols =
        [ (stageInputSymbol declaration, ty)
        | declaration <- shaderInputs shader
        , ExpressionType ty <- [stageInputType declaration]
        ]
      uniformSymbols =
        [ (uniformLeafSymbol leaf, uniformLeafType leaf)
        | UniformBlockResource block <- shaderResources shader
        , leaf <- uniformBlockLeaves block
        ]
      pushConstantSymbols =
        [ (uniformLeafSymbol leaf, uniformLeafType leaf)
        | PushConstantResource block <- shaderResources shader
        , leaf <- pushConstantLeaves block
        ]
      outputSymbols = map stageOutputSymbol (shaderOutputs shader)
      handleSymbols = concatMap resourceHandleSymbols (shaderResources shader)
      allSymbols = map fst interfaceSymbols ++ map fst uniformSymbols ++ map fst pushConstantSymbols ++ outputSymbols ++ handleSymbols
  case firstDuplicate allSymbols of
    Just symbol -> Left (DuplicateSymbol symbol)
    Nothing -> Right (Map.fromList (interfaceSymbols ++ uniformSymbols ++ pushConstantSymbols))

handleTable :: ShaderModule -> Either CodegenError (Map String HandleDeclaration)
handleTable shader = foldM addResource Map.empty (shaderResources shader)
 where
  addResource handles resource = case resource of
    UniformBlockResource _ -> Right handles
    PushConstantResource _ -> Right handles
    CombinedImageSamplerResource declaration -> do
      let descriptor = combinedDescriptorName declaration
          imageName = combinedImageSymbol declaration
          samplerName = combinedSamplerSymbol declaration
          dimension = combinedImageDimension declaration
      withUniqueHandle samplerName (CombinedHandle descriptor imageName samplerName dimension)
        =<< withUniqueHandle imageName (CombinedHandle descriptor imageName samplerName dimension) handles
    SeparateImageResource declaration ->
      withUniqueHandle (imageSymbol declaration) (ImageHandle (imageDescriptorName declaration) (declaredImageDimension declaration)) handles
    SeparateSamplerResource declaration ->
      withUniqueHandle (samplerSymbol declaration) (SamplerHandle (samplerDescriptorName declaration)) handles
    StorageBufferResource _ -> Right handles
  withUniqueHandle symbol declaration handles
    | Map.member symbol handles = Left (DuplicateSymbol symbol)
    | otherwise = Right (Map.insert symbol declaration handles)

resourceHandleSymbols :: ResourceDeclaration -> [String]
resourceHandleSymbols resource = case resource of
  UniformBlockResource _ -> []
  PushConstantResource _ -> []
  CombinedImageSamplerResource declaration -> [combinedImageSymbol declaration, combinedSamplerSymbol declaration]
  SeparateImageResource declaration -> [imageSymbol declaration]
  SeparateSamplerResource declaration -> [samplerSymbol declaration]
  StorageBufferResource declaration -> [storageBufferName declaration]

validateUniformBlocks :: ShaderModule -> Either CodegenError ()
validateUniformBlocks shader = do
  when (shaderStage shader /= ComputeShader && any isStorageBuffer (shaderResources shader)) $
    Left (InvalidStage "runtime storage arrays are compute-only")
  traverse_ validateUniformBlock [block | UniformBlockResource block <- shaderResources shader]
    *> traverse_ validatePushConstantBlock [block | PushConstantResource block <- shaderResources shader]
    *> traverse_ validateStorageBlock [block | StorageBufferResource block <- shaderResources shader]
 where
  isStorageBuffer StorageBufferResource{} = True
  isStorageBuffer _ = False
  validateUniformBlock block = do
    when (uniformBlockStandard block == Buffer.Vertex) $ Left (InvalidUniformLayout (uniformBlockName block ++ " uses Vertex layout"))
    validateBlock (uniformBlockName block) (uniformBlockLayout block) (uniformBlockLeaves block)
  validatePushConstantBlock block =
    validateBlock (pushConstantBlockName block) (pushConstantLayout block) (pushConstantLeaves block)
  validateStorageBlock block = do
    let name = storageBufferName block
        field = storageBufferElementLayout block
    validateFieldLayout name field
    unless (shaderTyMatchesField (storageBufferElementType block) field) $
      Left (InvalidUniformLayout (name ++ " storage element type does not match " ++ show field))
    unless (storageBufferElementType block `elem` [TyFloat, TyInt, TyWord, TyVector 2, TyVector 4]) $
      Left (InvalidUniformLayout (name ++ " has unsupported runtime-array element type"))
  validateBlock name layout leaves = do
    validateFieldLayout name layout
    case layout of
      Buffer.Struct _ -> pure ()
      _ -> Left (InvalidUniformLayout (name ++ " must have a struct root"))
    when (containsBoolean layout) $ Left (InvalidUniformLayout (name ++ " contains Boolean32"))
    traverse_ (validateLeaf layout) leaves
  validateLeaf layout leaf = case fieldAtPath layout (uniformLeafPath leaf) of
    Nothing -> Left (InvalidUniformLayout (uniformLeafSymbol leaf ++ " has invalid path " ++ show (uniformLeafPath leaf)))
    Just field
      | shaderTyMatchesField (uniformLeafType leaf) field -> Right ()
      | otherwise -> Left (InvalidUniformLayout (uniformLeafSymbol leaf ++ " type does not match " ++ show field))

validateFieldLayout :: String -> Buffer.FieldLayout -> Either CodegenError ()
validateFieldLayout blockName = go []
 where
  invalid path message = Left (InvalidUniformLayout (blockName ++ " " ++ path ++ " " ++ message))
  go path field = case field of
    Buffer.Scalar Buffer.Boolean32 -> invalid path "contains Boolean32"
    Buffer.Scalar _ -> Right ()
    Buffer.Vector channels scalar
      | channels `notElem` [2, 3, 4] -> invalid path ("has unsupported vector width " ++ show channels)
      | scalar == Buffer.Boolean32 -> invalid path "contains Boolean32"
      | otherwise -> Right ()
    Buffer.Matrix columns rows scalar
      | columns `notElem` [2, 3, 4] || rows `notElem` [2, 3, 4] -> invalid path ("has unsupported matrix dimensions " ++ show (columns, rows))
      | scalar /= Buffer.Float32 -> invalid path "has a non-float matrix"
      | otherwise -> Right ()
    Buffer.Array count element
      | count <= 0 -> invalid path ("has non-positive array length " ++ show count)
      | otherwise -> go (path ++ "[]") element
    Buffer.Struct fields -> traverse_ (uncurry (goMember path)) (zip [0 :: Int ..] fields)
  goMember path index = go (path ++ "." ++ show index)

containsBoolean :: Buffer.FieldLayout -> Bool
containsBoolean field = case field of
  Buffer.Scalar Buffer.Boolean32 -> True
  Buffer.Vector _ Buffer.Boolean32 -> True
  Buffer.Matrix _ _ Buffer.Boolean32 -> True
  Buffer.Array _ element -> containsBoolean element
  Buffer.Struct fields -> any containsBoolean fields
  _ -> False

fieldAtPath :: Buffer.FieldLayout -> [Word32] -> Maybe Buffer.FieldLayout
fieldAtPath field [] = Just field
fieldAtPath (Buffer.Struct fields) (index : rest) =
  atIndex fields index >>= flip fieldAtPath rest
fieldAtPath (Buffer.Array count element) (index : rest)
  | index < fromIntegral count = fieldAtPath element rest
fieldAtPath _ _ = Nothing

shaderTyMatchesField :: ShaderTy -> Buffer.FieldLayout -> Bool
shaderTyMatchesField shaderType field = case (shaderType, field) of
  (TyFloat, Buffer.Scalar Buffer.Float32) -> True
  (TyInt, Buffer.Scalar Buffer.SignedInt32) -> True
  (TyWord, Buffer.Scalar Buffer.UnsignedInt32) -> True
  (TyVector size, Buffer.Vector fieldSize Buffer.Float32) -> size == fieldSize
  (TyMatrix rows columns, Buffer.Matrix fieldColumns fieldRows Buffer.Float32) ->
    rows == fieldRows && columns == fieldColumns
  _ -> False

validateActions :: ShaderModule -> Map NodeId ReifiedNode -> Map String ShaderTy -> Either CodegenError ()
validateActions shader nodes _ = do
  let roots = forestRoots (shaderForest shader)
      rootsFromActions = concatMap actionRoots (shaderActions shader)
  unless (roots == rootsFromActions) $
    case (find (`notElem` rootsFromActions) roots, find (`notElem` roots) rootsFromActions) of
      (Just root, _) -> Left (RootWithoutAction root)
      (_, Just root) -> Left (ActionWithoutRoot root)
      _ -> Left (InvalidStage "shader action root count/order mismatch")
  traverse_ validateAction (shaderActions shader)
  traverse_ requireStoredOutput (shaderOutputs shader)
 where
  validateAction action = case action of
    StoreOutput symbol nodeId -> do
      output <- maybe (Left (MissingSymbol symbol)) Right (find ((== symbol) . stageOutputSymbol) (shaderOutputs shader))
      node <- requireNode nodeId
      case stageOutputType output of
        ExpressionType expected
          | expected == reifiedTy node -> Right ()
          | otherwise -> Left (InvalidNodeType nodeId "output type mismatch")
    DiscardWhen nodeId -> do
      when (shaderStage shader /= FragmentShader) $ Left (InvalidStage "discard is fragment-only")
      node <- requireNode nodeId
      unless (reifiedTy node == TyBool) $ Left (InvalidNodeType nodeId "discard condition must be Bool")
    StoreStorage symbol indexId valueId -> do
      when (shaderStage shader /= ComputeShader) $ Left (InvalidStage "storage stores are compute-only")
      declaration <- requireStorage symbol
      index <- requireNode indexId
      value <- requireNode valueId
      unless (reifiedTy index == TyWord) $ Left (InvalidNodeType indexId "storage index must be Word32")
      unless (reifiedTy value == storageBufferElementType declaration) $ Left (InvalidNodeType valueId "storage value type mismatch")
      when (storageBufferAccess declaration == StorageReadOnly) $ Left (InvalidStage (symbol ++ " is read-only"))
    AtomicAddStorage symbol indexId valueId -> do
      when (shaderStage shader /= ComputeShader) $ Left (InvalidStage "atomics are compute-only")
      declaration <- requireStorage symbol
      index <- requireNode indexId
      value <- requireNode valueId
      unless (reifiedTy index == TyWord) $ Left (InvalidNodeType indexId "atomic index must be Word32")
      unless (reifiedTy value == storageBufferElementType declaration && reifiedTy value `elem` [TyInt, TyWord]) $
        Left (InvalidNodeType valueId "atomic add requires matching Int32 or Word32")
      unless (storageBufferAccess declaration `elem` [StorageReadWrite, StorageAtomic]) $
        Left (InvalidStage (symbol ++ " does not permit atomics"))
    ComputeWhen conditionId body -> do
      when (shaderStage shader /= ComputeShader) $ Left (InvalidStage "compute condition is compute-only")
      condition <- requireNode conditionId
      unless (reifiedTy condition == TyBool) $ Left (InvalidNodeType conditionId "compute condition must be Bool")
      traverse_ validateAction body
  requireNode nodeId = maybe (Left (MissingNode nodeId)) Right (Map.lookup nodeId nodes)
  requireStoredOutput output =
    unless (any (stores (stageOutputSymbol output)) (shaderActions shader)) $
      Left (InvalidStage ("output " ++ stageOutputSymbol output ++ " has no store action"))
  stores symbol (StoreOutput candidate _) = symbol == candidate
  stores symbol (ComputeWhen _ body) = any (stores symbol) body
  stores _ _ = False
  requireStorage symbol =
    case [declaration | StorageBufferResource declaration <- shaderResources shader, storageBufferName declaration == symbol] of
      [declaration] -> Right declaration
      _ -> Left (MissingSymbol symbol)

actionRoots :: ShaderAction -> [NodeId]
actionRoots action = case action of
  StoreOutput _ root -> [root]
  DiscardWhen root -> [root]
  StoreStorage _ index value -> [index, value]
  AtomicAddStorage _ index value -> [index, value]
  ComputeWhen condition body -> condition : concatMap actionRoots body

validateNode :: ShaderModule -> Map NodeId ReifiedNode -> Map RegionId ReifiedRegion -> Map String ShaderTy -> Map String HandleDeclaration -> ReifiedNode -> Either CodegenError ()
validateNode shader nodes regions symbols handles node = case reifiedOp node of
  RLiteral value -> validateLiteralType node value
  RInput symbol -> case Map.lookup symbol symbols of
    Nothing -> Left (MissingSymbol symbol)
    Just symbolType
      | symbolType == reifiedTy node -> Right ()
      | otherwise -> invalid "input symbol type mismatch"
  RLocal binder
    | any ((== Just binder) . regionBinder) (Map.elems regions) -> Right ()
    | otherwise -> Left (UnboundLocal binder)
  RResource symbol -> case Map.lookup symbol handles of
    Nothing -> Left (MissingSymbol symbol)
    Just declaration -> case (reifiedTy node, declaration) of
      (imageType, CombinedHandle _ _ _ dimension)
        | imageType == imageShaderType dimension -> Right ()
      (TySampler, CombinedHandle{}) -> Right ()
      (imageType, ImageHandle _ dimension)
        | imageType == imageShaderType dimension -> Right ()
      (TySampler, SamplerHandle _) -> Right ()
      _ -> invalid "resource handle type mismatch"
  RStorageRead symbol index -> do
    unless (shaderStage shader == ComputeShader) $ Left (InvalidStage "storage reads are compute-only")
    indexType <- nodeType index
    unless (indexType == TyWord) $ invalid "storage read index must be Word32"
    declaration <- storageDeclaration symbol
    unless (reifiedTy node == storageBufferElementType declaration) $ invalid "storage read element type mismatch"
    when (storageBufferAccess declaration == StorageWriteOnly) $ invalid "read from write-only storage buffer"
  RStorageLength symbol -> do
    unless (shaderStage shader == ComputeShader) $ Left (InvalidStage "storage length is compute-only")
    unless (reifiedTy node == TyWord) $ invalid "storage length must be Word32"
    _ <- storageDeclaration symbol
    pure ()
  RUnary operation child -> do
    childType <- nodeType child
    validateUnaryType (shaderStage shader) (reifiedId node) operation childType (reifiedTy node)
  RBinary operation left right -> do
    leftType <- nodeType left
    rightType <- nodeType right
    validateBinaryType (reifiedId node) operation leftType rightType (reifiedTy node)
  RCompare operation left right -> do
    leftType <- nodeType left
    rightType <- nodeType right
    validateCompareType (reifiedId node) operation leftType rightType (reifiedTy node)
  RConstruct children -> do
    childTypes <- traverse nodeType children
    case reifiedTy node of
      TyVector size
        | size == length children && all (== TyFloat) childTypes -> Right ()
      _ -> invalid "vector construction has incompatible components"
  RExtract indices child -> do
    childType <- nodeType child
    case (childType, indices, reifiedTy node) of
      (TyVector size, [_], TyFloat)
        | validIndices size indices -> Right ()
      (TyVector size, _, TyVector resultSize)
        | validIndices size indices && resultSize == length indices && resultSize `elem` [2, 3, 4] -> Right ()
      (TyWordVector size, [_], TyWord)
        | validIndices size indices -> Right ()
      (TyWordVector size, _, TyWordVector resultSize)
        | validIndices size indices && resultSize == length indices && resultSize `elem` [2, 3, 4] -> Right ()
      _ -> invalid "invalid vector extraction"
  RSelect condition yes no -> do
    conditionType <- nodeType condition
    yesType <- nodeType yes
    noType <- nodeType no
    unless (conditionType == TyBool && yesType == noType && yesType == reifiedTy node) $ invalid "select type mismatch"
  RBranch condition yesRegion noRegion -> do
    conditionType <- nodeType condition
    yesType <- regionType yesRegion
    noType <- regionType noRegion
    unless (conditionType == TyBool && yesType == noType && yesType == reifiedTy node) $ invalid "branch type mismatch"
    traverse_ requireNoBinder [yesRegion, noRegion]
  RWhile initial binder predicateRegion stepRegion -> do
    initialType <- nodeType initial
    predicateType <- regionType predicateRegion
    stepType <- regionType stepRegion
    unless (initialType == reifiedTy node && predicateType == TyBool && stepType == reifiedTy node) $ invalid "while type mismatch"
    traverse_ (requireBinder binder) [predicateRegion, stepRegion]
  RMix left right factor -> do
    types <- traverse nodeType [left, right, factor]
    case types of
      [leftType, rightType, TyFloat]
        | leftType == rightType && leftType == reifiedTy node && isFloatShape leftType -> Right ()
      _ -> invalid "mix type mismatch"
  RSmoothstep edge0 edge1 value -> do
    types <- traverse nodeType [edge0, edge1, value]
    unless (all (== reifiedTy node) types && isFloatShape (reifiedTy node)) $ invalid "smoothstep type mismatch"
  RSample kind mode image sampler coordinates reference lod ->
    validateSample shader nodes handles node kind mode image sampler coordinates reference lod
 where
  invalid :: String -> Either CodegenError a
  invalid = Left . InvalidNodeType (reifiedId node)
  storageDeclaration symbol =
    case [declaration | StorageBufferResource declaration <- shaderResources shader, storageBufferName declaration == symbol] of
      [declaration] -> Right declaration
      _ -> Left (MissingSymbol symbol)
  nodeType identifier = maybe (Left (MissingNode identifier)) (Right . reifiedTy) (Map.lookup identifier nodes)
  regionType identifier = do
    region <- maybe (Left (MissingRegion identifier)) Right (Map.lookup identifier regions)
    nodeType (regionRoot region)
  requireNoBinder identifier = do
    region <- maybe (Left (MissingRegion identifier)) Right (Map.lookup identifier regions)
    unless (isNothing (regionBinder region)) $ invalid "branch region unexpectedly declares a binder"
  requireBinder binder identifier = do
    region <- maybe (Left (MissingRegion identifier)) Right (Map.lookup identifier regions)
    unless (regionBinder region == Just binder) $ invalid "loop region binder mismatch"

validateLiteralType :: ReifiedNode -> HostValue -> Either CodegenError ()
validateLiteralType node value = unless valid $ Left (InvalidNodeType (reifiedId node) "literal payload type mismatch")
 where
  valid = case (reifiedTy node, value) of
    (TyFloat, HFloat _) -> True
    (TyInt, HInt _) -> True
    (TyWord, HWord _) -> True
    (TyBool, HBool _) -> True
    (TyVector size, HVector values) -> size == length values && size `elem` [2, 3, 4]
    (TyWordVector size, HWordVector values) -> size == length values && size `elem` [2, 3, 4]
    (TyMatrix rows columns, HMatrix valueRows valueColumns values) ->
      rows == valueRows && columns == valueColumns && length values == rows * columns
    _ -> False

validateUnaryType :: ShaderStage -> NodeId -> UnaryOp -> ShaderTy -> ShaderTy -> Either CodegenError ()
validateUnaryType stage nodeId operation childType resultType
  | childType /= resultType = invalid "unary result type mismatch"
  | operation `elem` [DfdxE, DfdyE, FwidthE] && stage /= FragmentShader = Left (InvalidStage (show operation ++ " is fragment-only"))
  | operation == NegateE && isArithmetic childType = Right ()
  | operation `elem` [AbsE, SignumE] && isArithmetic childType = Right ()
  | operation == RecipE && isFloatShape childType = Right ()
  | operation `elem` [SinE, CosE, TanE, AsinE, AcosE, AtanE, ExpE, LogE, SqrtE] && isFloatShape childType = Right ()
  | operation == NormalizeE && isFloatVector childType = Right ()
  | operation `elem` [DfdxE, DfdyE, FwidthE] && isFloatShape childType = Right ()
  | otherwise = invalid ("unsupported unary operation " ++ show operation ++ " on " ++ show childType)
 where
  invalid = Left . InvalidNodeType nodeId

validateBinaryType :: NodeId -> BinaryOp -> ShaderTy -> ShaderTy -> ShaderTy -> Either CodegenError ()
validateBinaryType nodeId operation leftType rightType resultType
  | operation `elem` [AddE, SubtractE, MultiplyE]
      && leftType == rightType
      && resultType == leftType
      && isArithmetic leftType =
      Right ()
  | operation `elem` [DivideE, PowerE, MinE, MaxE]
      && leftType == rightType
      && resultType == leftType
      && isFloatShape leftType =
      Right ()
  | operation == DotE && leftType == rightType && isFloatVector leftType && resultType == TyFloat = Right ()
  | operation `elem` [CrossE, ReflectE] && leftType == rightType && resultType == leftType && isFloatVector leftType = Right ()
  | operation == MatrixMultiplyE && validMatrixProduct leftType rightType resultType = Right ()
  | operation == MatrixVectorMultiplyE && validMatrixVectorProduct leftType rightType resultType = Right ()
  | otherwise = Left (InvalidNodeType nodeId ("invalid binary operation " ++ show operation ++ " on " ++ show (leftType, rightType, resultType)))

validateCompareType :: NodeId -> CompareOp -> ShaderTy -> ShaderTy -> ShaderTy -> Either CodegenError ()
validateCompareType nodeId operation leftType rightType resultType
  | resultType /= TyBool || leftType /= rightType = invalid
  | operation `elem` [EqualE, NotEqualE] && isEqualityType leftType = Right ()
  | operation `elem` [LessE, LessEqualE, GreaterE, GreaterEqualE] && leftType `elem` [TyFloat, TyInt, TyWord] = Right ()
  | otherwise = invalid
 where
  invalid = Left (InvalidNodeType nodeId ("invalid comparison " ++ show operation ++ " on " ++ show leftType))

validateSample :: ShaderModule -> Map NodeId ReifiedNode -> Map String HandleDeclaration -> ReifiedNode -> SamplingKind -> SamplingMode -> NodeId -> NodeId -> NodeId -> Maybe NodeId -> Maybe NodeId -> Either CodegenError ()
validateSample shader nodes handles node kind mode image sampler coordinates reference lod = do
  let expectedResult = case kind of
        RegularSample -> TyVector 4
        ComparisonSample -> TyFloat
  unless (reifiedTy node == expectedResult) $ invalid "sample result type does not match its sampling kind"
  imageType <- nodeType image
  dimension <- maybe (invalid "sample image is not an image resource") pure (shaderImageDimension imageType)
  imageSymbolName <- resourceSymbol image imageType
  samplerSymbolName <- resourceSymbol sampler TySampler
  coordinateType <- nodeType coordinates
  unless (coordinateType == sampleCoordinateType dimension) $ invalid ("sample coordinates do not match " ++ show dimension)
  case (kind, reference) of
    (RegularSample, Nothing) -> pure ()
    (ComparisonSample, Just referenceNode) -> do
      referenceType <- nodeType referenceNode
      unless (referenceType == TyFloat) $ invalid "comparison reference must be Float"
    (RegularSample, Just _) -> invalid "regular sample unexpectedly has a comparison reference"
    (ComparisonSample, Nothing) -> invalid "comparison sample has no reference"
  case (mode, shaderStage shader, lod) of
    (ImplicitLod, FragmentShader, Nothing) -> pure ()
    (ImplicitLod, _, _) -> Left (InvalidStage "implicit LOD sampling is fragment-only")
    (ExplicitLod, _, Just lodNode) -> do
      lodType <- nodeType lodNode
      unless (lodType == TyFloat) $ invalid "explicit LOD must be Float"
    (ExplicitLod, _, Nothing) -> invalid "explicit LOD sample has no LOD node"
  case (Map.lookup imageSymbolName handles, Map.lookup samplerSymbolName handles) of
    (Just (CombinedHandle imageDescriptor imageName samplerName imageDimension), Just (CombinedHandle samplerDescriptor imageName' samplerName' samplerDimension))
      | imageDescriptor == samplerDescriptor
          && imageName == imageName'
          && samplerName == samplerName'
          && imageSymbolName == imageName
          && samplerSymbolName == samplerName
          && imageDimension == dimension
          && samplerDimension == dimension ->
          Right ()
    (Just (ImageHandle _ imageDimension), Just (SamplerHandle _))
      | imageDimension == dimension -> Right ()
    _ -> Left (InvalidResourcePair (reifiedId node) (imageSymbolName ++ " / " ++ samplerSymbolName))
 where
  invalid :: String -> Either CodegenError a
  invalid = Left . InvalidNodeType (reifiedId node)
  nodeType identifier = maybe (Left (MissingNode identifier)) (Right . reifiedTy) (Map.lookup identifier nodes)
  resourceSymbol identifier expectedType = case Map.lookup identifier nodes of
    Just resourceNode
      | reifiedTy resourceNode == expectedType -> case reifiedOp resourceNode of
          RResource symbol -> Right symbol
          _ -> invalid "sample handle is not a resource node"
    _ -> invalid "sample handle type mismatch"

sampleCoordinateType :: ImageDimension -> ShaderTy
sampleCoordinateType dimension = case dimension of
  Image1D -> TyFloat
  Image2D -> TyVector 2
  Image3D -> TyVector 3
  ImageCube -> TyVector 3
  Image2DArray -> TyVector 3

validIndices :: Int -> [Int] -> Bool
validIndices size indices = not (null indices) && all (\index -> index >= 0 && index < size) indices

isArithmetic :: ShaderTy -> Bool
isArithmetic shaderType = case shaderType of
  TyFloat -> True
  TyInt -> True
  TyWord -> True
  TyVector size -> size `elem` [2, 3, 4]
  TyMatrix rows columns -> rows `elem` [2, 3, 4] && columns `elem` [2, 3, 4]
  _ -> False

isFloatShape :: ShaderTy -> Bool
isFloatShape TyFloat = True
isFloatShape shaderType = isFloatVector shaderType

isFloatVector :: ShaderTy -> Bool
isFloatVector (TyVector size) = size `elem` [2, 3, 4]
isFloatVector _ = False

isEqualityType :: ShaderTy -> Bool
isEqualityType shaderType = shaderType `elem` [TyFloat, TyInt, TyWord, TyBool] || isFloatVector shaderType || isWordVector shaderType || isMatrix shaderType

isWordVector :: ShaderTy -> Bool
isWordVector (TyWordVector size) = size `elem` [2, 3, 4]
isWordVector _ = False

isMatrix :: ShaderTy -> Bool
isMatrix (TyMatrix _ _) = True
isMatrix _ = False

validMatrixProduct :: ShaderTy -> ShaderTy -> ShaderTy -> Bool
validMatrixProduct (TyMatrix leftRows leftColumns) (TyMatrix rightRows rightColumns) (TyMatrix resultRows resultColumns) =
  leftColumns == rightRows && resultRows == leftRows && resultColumns == rightColumns
validMatrixProduct _ _ _ = False

validMatrixVectorProduct :: ShaderTy -> ShaderTy -> ShaderTy -> Bool
validMatrixVectorProduct (TyMatrix rows columns) (TyVector vectorSize) (TyVector resultSize) =
  columns == vectorSize && rows == resultSize
validMatrixVectorProduct _ _ _ = False

firstDuplicate :: (Ord a) => [a] -> Maybe a
firstDuplicate = go Set.empty
 where
  go _ [] = Nothing
  go seen (value : values)
    | Set.member value seen = Just value
    | otherwise = go (Set.insert value seen) values

atIndex :: [a] -> Word32 -> Maybe a
atIndex = go
 where
  go [] _ = Nothing
  go (value : _) 0 = Just value
  go (_ : rest) remaining = go rest (remaining - 1)

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ action = foldr ((*>) . action) (pure ())

-- Emission -------------------------------------------------------------------

newtype EmissionRegion = EmissionRegion Int
  deriving stock (Eq, Ord, Show)

data SymbolBinding
  = StageInputBinding SpirV.Id ShaderTy
  | UniformLeafBinding SpirV.Id Word32 ShaderTy [Word32]
  | OutputBinding SpirV.Id InterfaceValueType
  | CombinedResourceBinding String SpirV.Id SpirV.Id
  | ImageResourceBinding SpirV.Id SpirV.Id
  | SamplerResourceBinding SpirV.Id SpirV.Id
  | StorageResourceBinding SpirV.Id ShaderTy

data BufferTypeRole = BlockRoot | AggregateMember
  deriving stock (Eq)

data EmissionState = EmissionState
  { emissionPrepared :: PreparedModule
  , emittedValues :: Map (NodeId, EmissionRegion) SpirV.Id
  , emittedSymbols :: Map String SymbolBinding
  , binderValues :: Map BinderId SpirV.Id
  , currentRegion :: EmissionRegion
  , nextRegionNumber :: Int
  , currentBlock :: Maybe SpirV.Id
  , entryPointInterfaces :: [SpirV.Id]
  , glslInstructionSet :: Maybe SpirV.Id
  , bufferTypeCache :: [((BufferTypeRole, Buffer.LayoutStandard, Buffer.FieldLayout), SpirV.Id)]
  }

type Emit = ExceptT CodegenError (StateT EmissionState SpirV.Assembler)

initialEmissionState :: PreparedModule -> EmissionState
initialEmissionState prepared =
  EmissionState
    { emissionPrepared = prepared
    , emittedValues = Map.empty
    , emittedSymbols = Map.empty
    , binderValues = Map.empty
    , currentRegion = EmissionRegion 0
    , nextRegionNumber = 1
    , currentBlock = Nothing
    , entryPointInterfaces = []
    , glslInstructionSet = Nothing
    , bufferTypeCache = []
    }

emitModule :: Emit SpirV.SpirVModule
emitModule = do
  prepared <- gets emissionPrepared
  let shader = preparedShader prepared
  glsl <- assemble (SpirV.importExtInst "GLSL.std.450")
  modify' (\state -> state{glslInstructionSet = Just glsl})
  traverse_ emitInputDeclaration (shaderInputs shader)
  traverse_ emitOutputDeclaration (shaderOutputs shader)
  traverse_ emitResourceDeclaration (shaderResources shader)
  voidType <- emitShaderType TyVoidInternal
  functionType <- assemble (SpirV.typeFunction voidType [])
  function <- assemble (SpirV.emitFunction voidType functionType)
  interfaces <- gets entryPointInterfaces
  assemble (SpirV.emitEntryPoint (executionModel (shaderStage shader)) function (shaderEntryPoint shader) interfaces)
  case shaderStage shader of
    FragmentShader -> do
      assemble (SpirV.emitExecutionMode function (SpirV.enumerant "ExecutionMode" "OriginUpperLeft") [])
      when (any ((== BuiltIn FragDepth) . stageOutputSlot) (shaderOutputs shader)) $
        assemble (SpirV.emitExecutionMode function (SpirV.enumerant "ExecutionMode" "DepthReplacing") [])
    ComputeShader -> case shaderLocalSize shader of
      Just (LocalSize x y z) -> assemble (SpirV.emitExecutionMode function (SpirV.enumerant "ExecutionMode" "LocalSize") [x, y, z])
      Nothing -> throwError (InvalidStage "compute local size disappeared after preflight")
    VertexShader -> pure ()
  _ <- emitFreshLabel
  traverse_ emitActionBoundary (shaderActions shader)
  assemble SpirV.emitReturn
  assemble SpirV.emitFunctionEnd
  assemble SpirV.finishModule

data InternalShaderType = TyVoidInternal | TyExpression ShaderTy

emitShaderType :: InternalShaderType -> Emit SpirV.Id
emitShaderType internalType = case internalType of
  TyVoidInternal -> assemble SpirV.typeVoid
  TyExpression shaderType -> emitExpressionType shaderType

emitExpressionType :: ShaderTy -> Emit SpirV.Id
emitExpressionType shaderType = case shaderType of
  TyFloat -> assemble (SpirV.typeFloat 32)
  TyInt -> assemble (SpirV.typeInt 32 1)
  TyWord -> assemble (SpirV.typeInt 32 0)
  TyBool -> assemble SpirV.typeBool
  TyVector size -> do
    float <- emitExpressionType TyFloat
    assemble (SpirV.typeVector float (fromIntegral size))
  TyWordVector size -> do
    word <- emitExpressionType TyWord
    assemble (SpirV.typeVector word (fromIntegral size))
  TyMatrix rows columns -> do
    columnType <- emitExpressionType (TyVector rows)
    assemble (SpirV.typeMatrix columnType (fromIntegral columns))
  TyImage1D -> emitImageType Image1D
  TyImage2D -> emitImageType Image2D
  TyImage3D -> emitImageType Image3D
  TyImageCube -> emitImageType ImageCube
  TyImage2DArray -> emitImageType Image2DArray
  TySampler -> assemble SpirV.typeSampler

emitInterfaceType :: InterfaceValueType -> Emit SpirV.Id
emitInterfaceType valueType = case valueType of
  ExpressionType shaderType -> emitExpressionType shaderType

emitInputDeclaration :: StageInput -> Emit ()
emitInputDeclaration declaration = do
  valueType <- emitInterfaceType (stageInputType declaration)
  let storageClass = SpirV.enumerant "StorageClass" "Input"
  pointerType <- assemble (SpirV.typePointer storageClass valueType)
  variable <- assemble (SpirV.emitGlobalVariable pointerType storageClass Nothing)
  decorateInterface variable (stageInputSlot declaration)
  assemble (SpirV.emitName variable (stageInputSymbol declaration))
  addInterface variable
  case stageInputType declaration of
    ExpressionType shaderType -> insertSymbol (stageInputSymbol declaration) (StageInputBinding variable shaderType)

emitOutputDeclaration :: StageOutput -> Emit ()
emitOutputDeclaration declaration = do
  valueType <- emitInterfaceType (stageOutputType declaration)
  let storageClass = SpirV.enumerant "StorageClass" "Output"
  pointerType <- assemble (SpirV.typePointer storageClass valueType)
  variable <- assemble (SpirV.emitGlobalVariable pointerType storageClass Nothing)
  decorateInterface variable (stageOutputSlot declaration)
  assemble (SpirV.emitName variable (stageOutputSymbol declaration))
  addInterface variable
  insertSymbol (stageOutputSymbol declaration) (OutputBinding variable (stageOutputType declaration))

decorateInterface :: SpirV.Id -> InterfaceSlot -> Emit ()
decorateInterface variable slot = case slot of
  Location location interpolation -> do
    assemble (SpirV.emitDecorate variable (SpirV.enumerant "Decoration" "Location") [location])
    case interpolation of
      Smooth -> pure ()
      Flat -> assemble (SpirV.emitDecorate variable (SpirV.enumerant "Decoration" "Flat") [])
      NoPerspective -> assemble (SpirV.emitDecorate variable (SpirV.enumerant "Decoration" "NoPerspective") [])
  BuiltIn builtIn ->
    assemble (SpirV.emitDecorate variable (SpirV.enumerant "Decoration" "BuiltIn") [builtInValue builtIn])

builtInValue :: BuiltIn -> Word32
builtInValue builtIn = SpirV.enumerant "BuiltIn" $ case builtIn of
  Position -> "Position"
  FragCoord -> "FragCoord"
  FragDepth -> "FragDepth"
  VertexIndex -> "VertexIndex"
  InstanceIndex -> "InstanceIndex"
  GlobalInvocationId -> "GlobalInvocationId"

emitResourceDeclaration :: ResourceDeclaration -> Emit ()
emitResourceDeclaration resource = case resource of
  UniformBlockResource block -> emitUniformBlock block
  PushConstantResource block -> emitPushConstantBlock block
  CombinedImageSamplerResource declaration -> emitCombinedImageSampler declaration
  SeparateImageResource declaration -> emitSeparateImage declaration
  SeparateSamplerResource declaration -> emitSeparateSampler declaration
  StorageBufferResource declaration -> emitStorageBuffer declaration

emitStorageBuffer :: StorageBufferDeclaration -> Emit ()
emitStorageBuffer declaration = do
  elementType <- emitExpressionType (storageBufferElementType declaration)
  runtimeArray <- assemble (SpirV.typeDecoratedRuntimeArray elementType)
  let layout = Buffer.layoutOf Buffer.Std430 (storageBufferElementLayout declaration)
      stride = Buffer.layoutSize layout
  assemble (SpirV.emitDecorate runtimeArray (SpirV.enumerant "Decoration" "ArrayStride") [fromIntegral stride])
  blockType <- assemble (SpirV.typeDecoratedStruct [runtimeArray])
  assemble (SpirV.emitDecorate blockType (SpirV.enumerant "Decoration" "Block") [])
  assemble (SpirV.emitMemberDecorate blockType 0 (SpirV.enumerant "Decoration" "Offset") [0])
  let storageClass = SpirV.enumerant "StorageClass" "StorageBuffer"
  pointerType <- assemble (SpirV.typePointer storageClass blockType)
  variable <- assemble (SpirV.emitGlobalVariable pointerType storageClass Nothing)
  decorateDescriptor variable (storageBufferLocation declaration)
  case storageBufferAccess declaration of
    StorageReadOnly -> assemble (SpirV.emitDecorate variable (SpirV.enumerant "Decoration" "NonWritable") [])
    StorageWriteOnly -> assemble (SpirV.emitDecorate variable (SpirV.enumerant "Decoration" "NonReadable") [])
    StorageReadWrite -> pure ()
    StorageAtomic -> pure ()
  assemble (SpirV.emitName variable (storageBufferName declaration))
  addInterface variable
  insertSymbol (storageBufferName declaration) (StorageResourceBinding variable (storageBufferElementType declaration))

emitUniformBlock :: UniformBlockDeclaration -> Emit ()
emitUniformBlock block = do
  blockType <- emitBufferBlockType (uniformBlockStandard block) (uniformBlockLayout block)
  let storageClass = case uniformBlockStandard block of
        Buffer.Std140 -> SpirV.enumerant "StorageClass" "Uniform"
        Buffer.Std430 -> SpirV.enumerant "StorageClass" "StorageBuffer"
        Buffer.Vertex -> SpirV.enumerant "StorageClass" "Uniform"
  pointerType <- assemble (SpirV.typePointer storageClass blockType)
  variable <- assemble (SpirV.emitGlobalVariable pointerType storageClass Nothing)
  decorateDescriptor variable (uniformBlockLocation block)
  assemble (SpirV.emitName variable (uniformBlockName block))
  addInterface variable
  traverse_
    (\leaf -> insertSymbol (uniformLeafSymbol leaf) (UniformLeafBinding variable storageClass (uniformLeafType leaf) (uniformLeafPath leaf)))
    (uniformBlockLeaves block)

emitPushConstantBlock :: PushConstantDeclaration -> Emit ()
emitPushConstantBlock block = do
  blockType <- emitBufferBlockType Buffer.Std430 (pushConstantLayout block)
  let storageClass = SpirV.enumerant "StorageClass" "PushConstant"
  pointerType <- assemble (SpirV.typePointer storageClass blockType)
  variable <- assemble (SpirV.emitGlobalVariable pointerType storageClass Nothing)
  assemble (SpirV.emitName variable (pushConstantBlockName block))
  addInterface variable
  traverse_
    (\leaf -> insertSymbol (uniformLeafSymbol leaf) (UniformLeafBinding variable storageClass (uniformLeafType leaf) (uniformLeafPath leaf)))
    (pushConstantLeaves block)

emitCombinedImageSampler :: CombinedImageSamplerDeclaration -> Emit ()
emitCombinedImageSampler declaration = do
  imageType <- emitImageType (combinedImageDimension declaration)
  sampledImageType <- assemble (SpirV.typeSampledImage imageType)
  variable <- emitDescriptorVariable sampledImageType (combinedDescriptorName declaration) (combinedDescriptorLocation declaration)
  let binding = CombinedResourceBinding (combinedDescriptorName declaration) variable sampledImageType
  insertSymbol (combinedImageSymbol declaration) binding
  insertSymbol (combinedSamplerSymbol declaration) binding

emitSeparateImage :: ImageDeclaration -> Emit ()
emitSeparateImage declaration = do
  imageType <- emitImageType (declaredImageDimension declaration)
  variable <- emitDescriptorVariable imageType (imageDescriptorName declaration) (imageDescriptorLocation declaration)
  insertSymbol (imageSymbol declaration) (ImageResourceBinding variable imageType)

emitSeparateSampler :: SamplerDeclaration -> Emit ()
emitSeparateSampler declaration = do
  samplerType <- assemble SpirV.typeSampler
  variable <- emitDescriptorVariable samplerType (samplerDescriptorName declaration) (samplerDescriptorLocation declaration)
  insertSymbol (samplerSymbol declaration) (SamplerResourceBinding variable samplerType)

emitImageType :: ImageDimension -> Emit SpirV.Id
emitImageType dimension = do
  float <- emitExpressionType TyFloat
  let (spirvDimension, arrayed) = case dimension of
        Image1D -> ("1D", 0)
        Image2D -> ("2D", 0)
        Image3D -> ("3D", 0)
        ImageCube -> ("Cube", 0)
        Image2DArray -> ("2D", 1)
  assemble
    ( SpirV.typeImage
        SpirV.ImageType
          { SpirV.imageSampledType = float
          , SpirV.imageDimension = SpirV.enumerant "Dim" spirvDimension
          , SpirV.imageDepth = 2
          , SpirV.imageArrayed = arrayed
          , SpirV.imageMultisampled = 0
          , SpirV.imageSampled = 1
          , SpirV.imageFormat = SpirV.enumerant "ImageFormat" "Unknown"
          , SpirV.imageAccessQualifier = Nothing
          }
    )

emitDescriptorVariable :: SpirV.Id -> String -> DescriptorLocation -> Emit SpirV.Id
emitDescriptorVariable valueType name location = do
  let storageClass = SpirV.enumerant "StorageClass" "UniformConstant"
  pointerType <- assemble (SpirV.typePointer storageClass valueType)
  variable <- assemble (SpirV.emitGlobalVariable pointerType storageClass Nothing)
  decorateDescriptor variable location
  assemble (SpirV.emitName variable name)
  addInterface variable
  pure variable

decorateDescriptor :: SpirV.Id -> DescriptorLocation -> Emit ()
decorateDescriptor variable location = do
  assemble (SpirV.emitDecorate variable (SpirV.enumerant "Decoration" "DescriptorSet") [descriptorSet location])
  assemble (SpirV.emitDecorate variable (SpirV.enumerant "Decoration" "Binding") [descriptorBinding location])

emitBufferType :: Buffer.LayoutStandard -> Buffer.FieldLayout -> Emit SpirV.Id
emitBufferType = emitBufferTypeFor AggregateMember

emitBufferBlockType :: Buffer.LayoutStandard -> Buffer.FieldLayout -> Emit SpirV.Id
emitBufferBlockType = emitBufferTypeFor BlockRoot

emitBufferTypeFor :: BufferTypeRole -> Buffer.LayoutStandard -> Buffer.FieldLayout -> Emit SpirV.Id
emitBufferTypeFor role standard field = do
  cached <- gets (lookup (role, standard, field) . bufferTypeCache)
  case cached of
    Just typeId -> pure typeId
    Nothing -> do
      typeId <- emitNewBufferType standard field
      when (role == BlockRoot) $
        assemble (SpirV.emitDecorate typeId (SpirV.enumerant "Decoration" "Block") [])
      modify' (\state -> state{bufferTypeCache = ((role, standard, field), typeId) : bufferTypeCache state})
      pure typeId

emitNewBufferType :: Buffer.LayoutStandard -> Buffer.FieldLayout -> Emit SpirV.Id
emitNewBufferType standard field = case field of
  Buffer.Scalar scalar -> emitBufferScalar scalar
  Buffer.Vector size scalar -> do
    component <- emitBufferScalar scalar
    assemble (SpirV.typeVector component (fromIntegral size))
  Buffer.Matrix columns rows scalar -> do
    component <- emitBufferScalar scalar
    column <- assemble (SpirV.typeVector component (fromIntegral rows))
    assemble (SpirV.typeMatrix column (fromIntegral columns))
  Buffer.Array count element -> do
    elementType <- emitBufferType standard element
    unsigned <- assemble (SpirV.typeInt 32 0)
    countConstant <- assemble (SpirV.constantWord unsigned [fromIntegral count])
    arrayType <- assemble (SpirV.typeDecoratedArray elementType countConstant)
    case Buffer.layoutStride (Buffer.layoutOf standard field) of
      Just stride -> assemble (SpirV.emitDecorate arrayType (SpirV.enumerant "Decoration" "ArrayStride") [fromIntegral stride])
      Nothing -> throwError (InvalidUniformLayout "array layout has no stride")
    pure arrayType
  Buffer.Struct fields -> do
    memberTypes <- traverse (emitBufferType standard) fields
    structType <- assemble (SpirV.typeDecoratedStruct memberTypes)
    let offsets = Buffer.layoutFieldOffsets (Buffer.layoutOf standard field)
    traverse_ (decorateMember structType) (zip3 [0 ..] fields offsets)
    pure structType
 where
  decorateMember structType (member, memberField, offset) = do
    assemble (SpirV.emitMemberName structType member ("member" ++ show member))
    assemble (SpirV.emitMemberDecorate structType member (SpirV.enumerant "Decoration" "Offset") [fromIntegral offset])
    case matrixStride standard memberField of
      Nothing -> pure ()
      Just stride -> do
        assemble (SpirV.emitMemberDecorate structType member (SpirV.enumerant "Decoration" "MatrixStride") [fromIntegral stride])
        assemble (SpirV.emitMemberDecorate structType member (SpirV.enumerant "Decoration" "ColMajor") [])

emitBufferScalar :: Buffer.ScalarType -> Emit SpirV.Id
emitBufferScalar scalar = case scalar of
  Buffer.Float32 -> emitExpressionType TyFloat
  Buffer.SignedInt32 -> emitExpressionType TyInt
  Buffer.UnsignedInt32 -> assemble (SpirV.typeInt 32 0)
  Buffer.Boolean32 -> throwError (InvalidUniformLayout "Boolean32 reached buffer emission")

matrixStride :: Buffer.LayoutStandard -> Buffer.FieldLayout -> Maybe Int
matrixStride standard field = case field of
  Buffer.Matrix{} -> Buffer.layoutMatrixStride (Buffer.layoutOf standard field)
  Buffer.Array _ element -> matrixStride standard element
  _ -> Nothing

insertSymbol :: String -> SymbolBinding -> Emit ()
insertSymbol symbol binding = do
  symbols <- gets emittedSymbols
  if Map.member symbol symbols
    then throwError (DuplicateSymbol symbol)
    else modify' (\state -> state{emittedSymbols = Map.insert symbol binding symbols})

addInterface :: SpirV.Id -> Emit ()
addInterface variable = modify' (\state -> state{entryPointInterfaces = entryPointInterfaces state ++ [variable]})

executionModel :: ShaderStage -> Word32
executionModel stage = SpirV.enumerant "ExecutionModel" $ case stage of
  VertexShader -> "Vertex"
  FragmentShader -> "Fragment"
  ComputeShader -> "GLCompute"

emitAction :: ShaderAction -> Emit ()
emitAction action = case action of
  StoreOutput symbol nodeId -> do
    value <- emitNode nodeId
    binding <- lookupSymbol symbol
    case binding of
      OutputBinding variable _ -> assemble (SpirV.emitStore variable value)
      _ -> throwError (MissingSymbol symbol)
  DiscardWhen nodeId -> emitDiscard nodeId
  StoreStorage symbol indexNode valueNode -> do
    index <- emitNode indexNode
    value <- emitNode valueNode
    (pointer, _) <- emitStorageElementPointer symbol index
    assemble (SpirV.emitStore pointer value)
  AtomicAddStorage symbol indexNode valueNode -> do
    index <- emitNode indexNode
    value <- emitNode valueNode
    (pointer, elementType) <- emitStorageElementPointer symbol index
    resultType <- emitExpressionType elementType
    wordType <- emitExpressionType TyWord
    scope <- assemble (SpirV.constantWord wordType [SpirV.enumerant "Scope" "Device"])
    let semanticsBits =
          SpirV.enumerant "MemorySemantics" "AcquireRelease"
            + SpirV.enumerant "MemorySemantics" "UniformMemory"
    semantics <- assemble (SpirV.constantWord wordType [semanticsBits])
    _ <- assemble (SpirV.emitAtomicIAdd resultType pointer scope semantics value)
    pure ()
  ComputeWhen conditionNode body -> emitComputeWhen conditionNode body

emitActionBoundary :: ShaderAction -> Emit ()
emitActionBoundary action = do
  modify' (\state -> state{emittedValues = Map.empty})
  emitAction action

emitComputeWhen :: NodeId -> [ShaderAction] -> Emit ()
emitComputeWhen conditionNode body = do
  condition <- emitNode conditionNode
  bodyLabel <- assemble SpirV.freshId
  mergeLabel <- assemble SpirV.freshId
  assemble (SpirV.emitSelectionMerge mergeLabel 0)
  assemble (SpirV.emitBranchConditional condition bodyLabel mergeLabel Nothing)
  emitLabelId bodyLabel
  modify' (\state -> state{emittedValues = Map.empty})
  traverse_ emitActionBoundary body
  assemble (SpirV.emitBranch mergeLabel)
  emitLabelId mergeLabel

emitDiscard :: NodeId -> Emit ()
emitDiscard conditionNode = do
  condition <- emitNode conditionNode
  killLabel <- assemble SpirV.freshId
  continueLabel <- assemble SpirV.freshId
  assemble (SpirV.emitSelectionMerge continueLabel 0)
  assemble (SpirV.emitBranchConditional condition killLabel continueLabel Nothing)
  emitLabelId killLabel
  assemble SpirV.emitKill
  emitLabelId continueLabel

emitNode :: NodeId -> Emit SpirV.Id
emitNode nodeId = do
  region <- gets currentRegion
  cached <- gets (Map.lookup (nodeId, region) . emittedValues)
  case cached of
    Just value -> pure value
    Nothing -> do
      node <- lookupNode nodeId
      value <- emitNodeOperation node
      modify' (\state -> state{emittedValues = Map.insert (nodeId, region) value (emittedValues state)})
      pure value

emitNodeOperation :: ReifiedNode -> Emit SpirV.Id
emitNodeOperation node = case reifiedOp node of
  RLiteral value -> emitLiteral (reifiedTy node) value
  RInput symbol -> emitInput symbol
  RLocal binder -> do
    values <- gets binderValues
    maybe (throwError (UnboundLocal binder)) pure (Map.lookup binder values)
  RResource symbol -> emitResource symbol
  RStorageRead symbol indexNode -> do
    index <- emitNode indexNode
    (pointer, elementType) <- emitStorageElementPointer symbol index
    resultType <- emitExpressionType elementType
    assemble (SpirV.emitLoad resultType pointer)
  RStorageLength symbol -> do
    binding <- lookupSymbol symbol
    case binding of
      StorageResourceBinding variable _ -> do
        resultType <- emitExpressionType TyWord
        assemble (SpirV.emitArrayLength resultType variable 0)
      _ -> throwError (MissingSymbol symbol)
  RUnary operation child -> emitUnaryOperation (reifiedTy node) operation child
  RBinary operation left right -> emitBinaryOperation (reifiedTy node) operation left right
  RCompare operation left right -> emitComparison operation left right
  RConstruct children -> do
    resultType <- emitExpressionType (reifiedTy node)
    constituents <- traverse emitNode children
    assemble (SpirV.emitCompositeConstruct resultType constituents)
  RExtract indices child -> emitExtraction (reifiedTy node) indices child
  RSelect condition yes no -> emitSelection (reifiedTy node) condition yes no
  RBranch condition yesRegion noRegion -> emitBranchExpression (reifiedTy node) condition yesRegion noRegion
  RWhile initial binder predicateRegion stepRegion -> emitWhileExpression (reifiedTy node) initial binder predicateRegion stepRegion
  RMix left right factor -> emitGlslInstruction (reifiedTy node) 46 [left, right, factor]
  RSmoothstep edge0 edge1 value -> emitGlslInstruction (reifiedTy node) 49 [edge0, edge1, value]
  RSample kind mode image sampler coordinates reference lod -> emitSampleOperation (reifiedTy node) kind mode image sampler coordinates reference lod

emitLiteral :: ShaderTy -> HostValue -> Emit SpirV.Id
emitLiteral shaderType value = case (shaderType, value) of
  (TyFloat, HFloat number) -> do
    resultType <- emitExpressionType TyFloat
    assemble (SpirV.constantF32 resultType (castFloatToWord32 number))
  (TyInt, HInt number) -> do
    resultType <- emitExpressionType TyInt
    assemble (SpirV.constantWord resultType [fromIntegral number])
  (TyWord, HWord number) -> do
    resultType <- emitExpressionType TyWord
    assemble (SpirV.constantWord resultType [number])
  (TyBool, HBool flag) -> do
    resultType <- emitExpressionType TyBool
    assemble (SpirV.constantBool resultType flag)
  (TyVector size, HVector values)
    | size == length values -> do
        resultType <- emitExpressionType shaderType
        components <- traverse (emitLiteral TyFloat . HFloat) values
        assemble (SpirV.constantComposite resultType components)
  (TyWordVector size, HWordVector values)
    | size == length values -> do
        resultType <- emitExpressionType shaderType
        components <- traverse (emitLiteral TyWord . HWord) values
        assemble (SpirV.constantComposite resultType components)
  (TyMatrix rows columns, HMatrix valueRows valueColumns values)
    | rows == valueRows && columns == valueColumns -> do
        resultType <- emitExpressionType shaderType
        columnValues <- traverse (emitMatrixColumn rows columns values) [0 .. columns - 1]
        assemble (SpirV.constantComposite resultType columnValues)
  _ -> throwError (InvalidUniformLayout ("literal escaped preflight: " ++ show (shaderType, value)))

emitMatrixColumn :: Int -> Int -> [Float] -> Int -> Emit SpirV.Id
emitMatrixColumn rows columns values column = do
  components <- traverse matrixElement [0 .. rows - 1]
  emitLiteral (TyVector rows) (HVector components)
 where
  matrixElement row =
    maybe
      (throwError (InvalidUniformLayout "matrix literal escaped preflight"))
      pure
      (atIndex values (fromIntegral (row * columns + column)))

emitInput :: String -> Emit SpirV.Id
emitInput symbol = do
  binding <- lookupSymbol symbol
  case binding of
    StageInputBinding variable shaderType -> do
      resultType <- emitExpressionType shaderType
      assemble (SpirV.emitLoad resultType variable)
    UniformLeafBinding blockVariable storageClass shaderType path -> do
      resultType <- emitExpressionType shaderType
      pointerType <- assemble (SpirV.typePointer storageClass resultType)
      indices <- traverse emitAccessIndex path
      pointer <- assemble (SpirV.emitAccessChain pointerType blockVariable indices)
      assemble (SpirV.emitLoad resultType pointer)
    _ -> throwError (MissingSymbol symbol)

emitAccessIndex :: Word32 -> Emit SpirV.Id
emitAccessIndex index = do
  unsigned <- assemble (SpirV.typeInt 32 0)
  assemble (SpirV.constantWord unsigned [index])

emitResource :: String -> Emit SpirV.Id
emitResource symbol = do
  binding <- lookupSymbol symbol
  case binding of
    CombinedResourceBinding _ variable sampledImageType -> assemble (SpirV.emitLoad sampledImageType variable)
    ImageResourceBinding variable imageType -> assemble (SpirV.emitLoad imageType variable)
    SamplerResourceBinding variable samplerType -> assemble (SpirV.emitLoad samplerType variable)
    _ -> throwError (MissingSymbol symbol)

emitStorageElementPointer :: String -> SpirV.Id -> Emit (SpirV.Id, ShaderTy)
emitStorageElementPointer symbol index = do
  binding <- lookupSymbol symbol
  case binding of
    StorageResourceBinding variable elementType -> do
      resultType <- emitExpressionType elementType
      let storageClass = SpirV.enumerant "StorageClass" "StorageBuffer"
      pointerType <- assemble (SpirV.typePointer storageClass resultType)
      zero <- emitAccessIndex 0
      pointer <- assemble (SpirV.emitAccessChain pointerType variable [zero, index])
      pure (pointer, elementType)
    _ -> throwError (MissingSymbol symbol)

emitUnaryOperation :: ShaderTy -> UnaryOp -> NodeId -> Emit SpirV.Id
emitUnaryOperation shaderType operation child = case shaderType of
  TyMatrix rows columns -> emitMatrixUnary rows columns operation child
  _ -> do
    operand <- emitNode child
    resultType <- emitExpressionType shaderType
    case operation of
      NegateE
        | shaderType == TyWord -> do
            zero <- emitLiteral TyWord (HWord 0)
            assemble (SpirV.emitBinary SpirV.ISub resultType zero operand)
        | otherwise -> assemble (SpirV.emitUnary (if shaderType == TyInt then SpirV.SNegate else SpirV.FNegate) resultType operand)
      AbsE
        | shaderType == TyWord -> pure operand
        | otherwise -> emitGlslWithValues resultType (if shaderType == TyInt then 5 else 4) [operand]
      SignumE
        | shaderType == TyWord -> do
            zero <- emitLiteral TyWord (HWord 0)
            one <- emitLiteral TyWord (HWord 1)
            boolType <- emitExpressionType TyBool
            nonzero <- assemble (SpirV.emitBinary SpirV.INotEqual boolType operand zero)
            assemble (SpirV.emitSelect resultType nonzero one zero)
        | otherwise -> emitGlslWithValues resultType (if shaderType == TyInt then 7 else 6) [operand]
      RecipE -> do
        one <- emitOne shaderType
        assemble (SpirV.emitBinary SpirV.FDiv resultType one operand)
      SinE -> emitGlslWithValues resultType 13 [operand]
      CosE -> emitGlslWithValues resultType 14 [operand]
      TanE -> emitGlslWithValues resultType 15 [operand]
      AsinE -> emitGlslWithValues resultType 16 [operand]
      AcosE -> emitGlslWithValues resultType 17 [operand]
      AtanE -> emitGlslWithValues resultType 18 [operand]
      ExpE -> emitGlslWithValues resultType 27 [operand]
      LogE -> emitGlslWithValues resultType 28 [operand]
      SqrtE -> emitGlslWithValues resultType 31 [operand]
      NormalizeE -> emitGlslWithValues resultType 69 [operand]
      DfdxE -> assemble (SpirV.emitUnary SpirV.DPdx resultType operand)
      DfdyE -> assemble (SpirV.emitUnary SpirV.DPdy resultType operand)
      FwidthE -> assemble (SpirV.emitUnary SpirV.Fwidth resultType operand)

emitMatrixUnary :: Int -> Int -> UnaryOp -> NodeId -> Emit SpirV.Id
emitMatrixUnary rows columns operation child = do
  matrix <- emitNode child
  matrixType <- emitExpressionType (TyMatrix rows columns)
  columnType <- emitExpressionType (TyVector rows)
  resultColumns <- traverse (emitColumn columnType matrix) [0 .. columns - 1]
  assemble (SpirV.emitCompositeConstruct matrixType resultColumns)
 where
  emitColumn columnType matrix column = do
    value <- assemble (SpirV.emitCompositeExtract columnType matrix [fromIntegral column])
    case operation of
      NegateE -> assemble (SpirV.emitUnary SpirV.FNegate columnType value)
      AbsE -> emitGlslWithValues columnType 4 [value]
      SignumE -> emitGlslWithValues columnType 6 [value]
      _ -> throwError (InvalidStage ("matrix unary escaped preflight: " ++ show operation))

emitOne :: ShaderTy -> Emit SpirV.Id
emitOne shaderType = case shaderType of
  TyFloat -> emitLiteral TyFloat (HFloat 1)
  TyVector size -> emitLiteral shaderType (HVector (replicate size 1))
  _ -> throwError (InvalidStage ("cannot construct one for " ++ show shaderType))

emitBinaryOperation :: ShaderTy -> BinaryOp -> NodeId -> NodeId -> Emit SpirV.Id
emitBinaryOperation resultShaderType operation leftNode rightNode = case operation of
  AddE -> emitArithmetic SpirV.IAdd SpirV.FAdd
  SubtractE -> emitArithmetic SpirV.ISub SpirV.FSub
  MultiplyE -> emitArithmetic SpirV.IMul SpirV.FMul
  DivideE -> emitArithmetic SpirV.SDiv SpirV.FDiv
  PowerE -> emitGlslInstruction resultShaderType 26 [leftNode, rightNode]
  MinE -> emitGlslInstruction resultShaderType 37 [leftNode, rightNode]
  MaxE -> emitGlslInstruction resultShaderType 40 [leftNode, rightNode]
  DotE -> emitCoreResult "OpDot"
  CrossE -> emitGlslInstruction resultShaderType 68 [leftNode, rightNode]
  ReflectE -> emitGlslInstruction resultShaderType 71 [leftNode, rightNode]
  MatrixMultiplyE -> emitCoreResult "OpMatrixTimesMatrix"
  MatrixVectorMultiplyE -> emitCoreResult "OpMatrixTimesVector"
 where
  emitArithmetic integerOperation floatOperation = case resultShaderType of
    TyMatrix rows columns -> emitMatrixBinary rows columns floatOperation leftNode rightNode
    _ -> do
      resultType <- emitExpressionType resultShaderType
      left <- emitNode leftNode
      right <- emitNode rightNode
      assemble (SpirV.emitBinary (if resultShaderType `elem` [TyInt, TyWord] then integerOperation else floatOperation) resultType left right)
  emitCoreResult instructionName = do
    resultType <- emitExpressionType resultShaderType
    left <- emitNode leftNode
    right <- emitNode rightNode
    assemble (SpirV.emitResultInstruction (SpirV.opcode instructionName) resultType [SpirV.idWord left, SpirV.idWord right])

emitMatrixBinary :: Int -> Int -> SpirV.BinaryOperation -> NodeId -> NodeId -> Emit SpirV.Id
emitMatrixBinary rows columns operation leftNode rightNode = do
  left <- emitNode leftNode
  right <- emitNode rightNode
  matrixType <- emitExpressionType (TyMatrix rows columns)
  columnType <- emitExpressionType (TyVector rows)
  resultColumns <- traverse (emitColumn left right columnType) [0 .. columns - 1]
  assemble (SpirV.emitCompositeConstruct matrixType resultColumns)
 where
  emitColumn left right columnType column = do
    leftColumn <- assemble (SpirV.emitCompositeExtract columnType left [fromIntegral column])
    rightColumn <- assemble (SpirV.emitCompositeExtract columnType right [fromIntegral column])
    assemble (SpirV.emitBinary operation columnType leftColumn rightColumn)

emitComparison :: CompareOp -> NodeId -> NodeId -> Emit SpirV.Id
emitComparison operation leftNode rightNode = do
  leftShaderType <- nodeShaderType leftNode
  case leftShaderType of
    TyMatrix rows columns -> emitMatrixComparison rows columns operation leftNode rightNode
    TyVector size -> emitVectorComparison size operation leftNode rightNode
    TyWordVector size -> emitWordVectorComparison size operation leftNode rightNode
    _ -> emitScalarComparison leftShaderType operation leftNode rightNode

emitScalarComparison :: ShaderTy -> CompareOp -> NodeId -> NodeId -> Emit SpirV.Id
emitScalarComparison shaderType operation leftNode rightNode = do
  boolType <- emitExpressionType TyBool
  left <- emitNode leftNode
  right <- emitNode rightNode
  case (shaderType, operation) of
    (TyWord, LessE) -> assemble (SpirV.emitBinary SpirV.ULessThan boolType left right)
    (TyWord, LessEqualE) -> assemble (SpirV.emitBinary SpirV.ULessThanEqual boolType left right)
    (TyWord, GreaterE) -> assemble (SpirV.emitBinary SpirV.UGreaterThan boolType left right)
    (TyWord, GreaterEqualE) -> assemble (SpirV.emitBinary SpirV.UGreaterThanEqual boolType left right)
    _ -> assemble (SpirV.emitBinary (comparisonOperation shaderType operation) boolType left right)

emitVectorComparison :: Int -> CompareOp -> NodeId -> NodeId -> Emit SpirV.Id
emitVectorComparison size operation leftNode rightNode = do
  boolType <- emitExpressionType TyBool
  boolVectorType <- do
    bool <- emitExpressionType TyBool
    assemble (SpirV.typeVector bool (fromIntegral size))
  left <- emitNode leftNode
  right <- emitNode rightNode
  compared <- assemble (SpirV.emitBinary (comparisonOperation TyFloat operation) boolVectorType left right)
  case operation of
    EqualE -> assemble (SpirV.emitAll boolType compared)
    NotEqualE -> assemble (SpirV.emitAny boolType compared)
    _ -> throwError (InvalidStage "vector relational comparison escaped preflight")

emitWordVectorComparison :: Int -> CompareOp -> NodeId -> NodeId -> Emit SpirV.Id
emitWordVectorComparison size operation leftNode rightNode = do
  boolType <- emitExpressionType TyBool
  boolVectorType <- do
    bool <- emitExpressionType TyBool
    assemble (SpirV.typeVector bool (fromIntegral size))
  left <- emitNode leftNode
  right <- emitNode rightNode
  compared <- assemble (SpirV.emitBinary (comparisonOperation TyWord operation) boolVectorType left right)
  case operation of
    EqualE -> assemble (SpirV.emitAll boolType compared)
    NotEqualE -> assemble (SpirV.emitAny boolType compared)
    _ -> throwError (InvalidStage "word vector relational comparison escaped preflight")

emitMatrixComparison :: Int -> Int -> CompareOp -> NodeId -> NodeId -> Emit SpirV.Id
emitMatrixComparison rows columns operation leftNode rightNode = do
  left <- emitNode leftNode
  right <- emitNode rightNode
  boolType <- emitExpressionType TyBool
  boolVectorType <- do
    bool <- emitExpressionType TyBool
    assemble (SpirV.typeVector bool (fromIntegral rows))
  columnType <- emitExpressionType (TyVector rows)
  columnResults <- traverse (compareColumn left right columnType boolVectorType boolType) [0 .. columns - 1]
  foldBoolean operation boolType columnResults
 where
  compareColumn left right columnType boolVectorType boolType column = do
    leftColumn <- assemble (SpirV.emitCompositeExtract columnType left [fromIntegral column])
    rightColumn <- assemble (SpirV.emitCompositeExtract columnType right [fromIntegral column])
    compared <- assemble (SpirV.emitBinary (comparisonOperation TyFloat operation) boolVectorType leftColumn rightColumn)
    case operation of
      EqualE -> assemble (SpirV.emitAll boolType compared)
      NotEqualE -> assemble (SpirV.emitAny boolType compared)
      _ -> throwError (InvalidStage "matrix relational comparison escaped preflight")

foldBoolean :: CompareOp -> SpirV.Id -> [SpirV.Id] -> Emit SpirV.Id
foldBoolean operation boolType values = case values of
  [] -> throwError (InvalidStage "empty matrix comparison")
  firstValue : rest -> foldM combine firstValue rest
 where
  combine left right =
    assemble
      ( SpirV.emitBinary
          (if operation == EqualE then SpirV.LogicalAnd else SpirV.LogicalOr)
          boolType
          left
          right
      )

comparisonOperation :: ShaderTy -> CompareOp -> SpirV.BinaryOperation
comparisonOperation shaderType operation = case (shaderType, operation) of
  (TyFloat, EqualE) -> SpirV.FOrdEqual
  (TyFloat, NotEqualE) -> SpirV.FUnordNotEqual
  (TyFloat, LessE) -> SpirV.FOrdLessThan
  (TyFloat, LessEqualE) -> SpirV.FOrdLessThanEqual
  (TyFloat, GreaterE) -> SpirV.FOrdGreaterThan
  (TyFloat, GreaterEqualE) -> SpirV.FOrdGreaterThanEqual
  (TyInt, EqualE) -> SpirV.IEqual
  (TyInt, NotEqualE) -> SpirV.INotEqual
  (TyInt, LessE) -> SpirV.SLessThan
  (TyInt, LessEqualE) -> SpirV.SLessThanEqual
  (TyInt, GreaterE) -> SpirV.SGreaterThan
  (TyInt, GreaterEqualE) -> SpirV.SGreaterThanEqual
  (TyWord, EqualE) -> SpirV.IEqual
  (TyWord, NotEqualE) -> SpirV.INotEqual
  (TyBool, EqualE) -> SpirV.LogicalEqual
  (TyBool, NotEqualE) -> SpirV.LogicalNotEqual
  (_, EqualE) -> SpirV.FOrdEqual
  (_, NotEqualE) -> SpirV.FUnordNotEqual
  _ -> SpirV.FOrdEqual

emitExtraction :: ShaderTy -> [Int] -> NodeId -> Emit SpirV.Id
emitExtraction resultShaderType indices childNode = do
  resultType <- emitExpressionType resultShaderType
  child <- emitNode childNode
  case indices of
    [index] -> assemble (SpirV.emitCompositeExtract resultType child [fromIntegral index])
    _ -> assemble (SpirV.emitVectorShuffle resultType child child (map fromIntegral indices))

emitSelection :: ShaderTy -> NodeId -> NodeId -> NodeId -> Emit SpirV.Id
emitSelection resultShaderType conditionNode yesNode noNode = case resultShaderType of
  TyMatrix rows columns -> do
    condition <- emitNode conditionNode
    yes <- emitNode yesNode
    no <- emitNode noNode
    matrixType <- emitExpressionType resultShaderType
    columnType <- emitExpressionType (TyVector rows)
    resultColumns <- traverse (selectColumn condition yes no columnType) [0 .. columns - 1]
    assemble (SpirV.emitCompositeConstruct matrixType resultColumns)
  _ -> do
    resultType <- emitExpressionType resultShaderType
    condition <- emitNode conditionNode
    yes <- emitNode yesNode
    no <- emitNode noNode
    assemble (SpirV.emitSelect resultType condition yes no)
 where
  selectColumn condition yes no columnType column = do
    yesColumn <- assemble (SpirV.emitCompositeExtract columnType yes [fromIntegral column])
    noColumn <- assemble (SpirV.emitCompositeExtract columnType no [fromIntegral column])
    assemble (SpirV.emitSelect columnType condition yesColumn noColumn)

emitBranchExpression :: ShaderTy -> NodeId -> RegionId -> RegionId -> Emit SpirV.Id
emitBranchExpression resultShaderType conditionNode yesRegion noRegion = do
  condition <- emitNode conditionNode
  yesLabel <- assemble SpirV.freshId
  noLabel <- assemble SpirV.freshId
  mergeLabel <- assemble SpirV.freshId
  resultType <- emitExpressionType resultShaderType
  assemble (SpirV.emitSelectionMerge mergeLabel 0)
  assemble (SpirV.emitBranchConditional condition yesLabel noLabel Nothing)
  emitLabelId yesLabel
  yesValue <- emitRegionRoot yesRegion Nothing
  yesExit <- requireCurrentBlock
  assemble (SpirV.emitBranch mergeLabel)
  emitLabelId noLabel
  noValue <- emitRegionRoot noRegion Nothing
  noExit <- requireCurrentBlock
  assemble (SpirV.emitBranch mergeLabel)
  emitLabelId mergeLabel
  assemble (SpirV.emitPhi resultType [(yesValue, yesExit), (noValue, noExit)])

emitWhileExpression :: ShaderTy -> NodeId -> BinderId -> RegionId -> RegionId -> Emit SpirV.Id
emitWhileExpression resultShaderType initialNode binder predicateRegion stepRegion = do
  initial <- emitNode initialNode
  preheader <- requireCurrentBlock
  resultType <- emitExpressionType resultShaderType
  headerLabel <- assemble SpirV.freshId
  conditionLabel <- assemble SpirV.freshId
  bodyLabel <- assemble SpirV.freshId
  continueLabel <- assemble SpirV.freshId
  mergeLabel <- assemble SpirV.freshId
  result <- assemble SpirV.freshId
  backedge <- assemble SpirV.freshId
  assemble (SpirV.emitBranch headerLabel)
  emitLabelId headerLabel
  assemble (SpirV.emitPhiWithResult result resultType [(initial, preheader), (backedge, continueLabel)])
  assemble (SpirV.emitLoopMerge mergeLabel continueLabel 0 [])
  assemble (SpirV.emitBranch conditionLabel)
  emitLabelId conditionLabel
  predicate <- emitRegionRoot predicateRegion (Just (binder, result))
  assemble (SpirV.emitBranchConditional predicate bodyLabel mergeLabel Nothing)
  emitLabelId bodyLabel
  stepValue <- emitRegionRoot stepRegion (Just (binder, result))
  assemble (SpirV.emitBranch continueLabel)
  emitLabelId continueLabel
  assemble (SpirV.emitCopyObjectWithResult backedge resultType stepValue)
  assemble (SpirV.emitBranch headerLabel)
  emitLabelId mergeLabel
  pure result

emitRegionRoot :: RegionId -> Maybe (BinderId, SpirV.Id) -> Emit SpirV.Id
emitRegionRoot regionIdentifier binding = do
  region <- lookupRegion regionIdentifier
  previousRegion <- gets currentRegion
  previousBinders <- gets binderValues
  regionNumber <- gets nextRegionNumber
  let updatedBinders = case binding of
        Nothing -> previousBinders
        Just (binder, value) -> Map.insert binder value previousBinders
  modify'
    ( \state ->
        state
          { currentRegion = EmissionRegion regionNumber
          , nextRegionNumber = regionNumber + 1
          , binderValues = updatedBinders
          }
    )
  value <- emitNode (regionRoot region)
  modify' (\state -> state{currentRegion = previousRegion, binderValues = previousBinders})
  pure value

emitSampleOperation :: ShaderTy -> SamplingKind -> SamplingMode -> NodeId -> NodeId -> NodeId -> Maybe NodeId -> Maybe NodeId -> Emit SpirV.Id
emitSampleOperation resultShaderType kind mode imageNode samplerNode coordinatesNode referenceNode lodNode = do
  resultType <- emitExpressionType resultShaderType
  coordinates <- emitNode coordinatesNode
  imageSymbolName <- nodeResourceSymbol imageNode
  samplerSymbolName <- nodeResourceSymbol samplerNode
  imageBinding <- lookupSymbol imageSymbolName
  samplerBinding <- lookupSymbol samplerSymbolName
  sampledImage <- case (imageBinding, samplerBinding) of
    (CombinedResourceBinding imageDescriptor variable sampledType, CombinedResourceBinding samplerDescriptor _ _)
      | imageDescriptor == samplerDescriptor -> assemble (SpirV.emitLoad sampledType variable)
    (ImageResourceBinding imageVariable imageType, SamplerResourceBinding samplerVariable samplerType) -> do
      image <- assemble (SpirV.emitLoad imageType imageVariable)
      sampler <- assemble (SpirV.emitLoad samplerType samplerVariable)
      sampledType <- assemble (SpirV.typeSampledImage imageType)
      assemble (SpirV.emitSampledImage sampledType image sampler)
    _ -> throwError (InvalidResourcePair imageNode (imageSymbolName ++ " / " ++ samplerSymbolName))
  reference <- traverse emitNode referenceNode
  case (kind, mode, reference, lodNode) of
    (RegularSample, ImplicitLod, Nothing, Nothing) ->
      assemble (SpirV.emitImageSampleImplicitLod resultType sampledImage coordinates)
    (RegularSample, ExplicitLod, Nothing, Just lodIdentifier) -> do
      lod <- emitNode lodIdentifier
      assemble (SpirV.emitImageSampleExplicitLod resultType sampledImage coordinates 0x2 [lod])
    (ComparisonSample, ImplicitLod, Just referenceValue, Nothing) ->
      assemble (SpirV.emitImageSampleDrefImplicitLod resultType sampledImage coordinates referenceValue)
    (ComparisonSample, ExplicitLod, Just referenceValue, Just lodIdentifier) -> do
      lod <- emitNode lodIdentifier
      assemble (SpirV.emitImageSampleDrefExplicitLod resultType sampledImage coordinates referenceValue 0x2 [lod])
    _ -> throwError (InvalidNodeType imageNode "sampling mode escaped preflight")

emitGlslInstruction :: ShaderTy -> Word32 -> [NodeId] -> Emit SpirV.Id
emitGlslInstruction resultShaderType instructionNumber operands = do
  resultType <- emitExpressionType resultShaderType
  values <- traverse emitNode operands
  emitGlslWithValues resultType instructionNumber values

emitGlslWithValues :: SpirV.Id -> Word32 -> [SpirV.Id] -> Emit SpirV.Id
emitGlslWithValues resultType instructionNumber operands = do
  instructionSet <- gets glslInstructionSet >>= maybe (throwError (InvalidStage "GLSL.std.450 was not imported")) pure
  assemble (SpirV.emitExtInst resultType instructionSet instructionNumber operands)

nodeShaderType :: NodeId -> Emit ShaderTy
nodeShaderType nodeId = reifiedTy <$> lookupNode nodeId

nodeResourceSymbol :: NodeId -> Emit String
nodeResourceSymbol nodeId = do
  node <- lookupNode nodeId
  case reifiedOp node of
    RResource symbol -> pure symbol
    _ -> throwError (InvalidResourcePair nodeId "node is not a resource")

lookupNode :: NodeId -> Emit ReifiedNode
lookupNode nodeId = do
  nodes <- gets (preparedNodes . emissionPrepared)
  maybe (throwError (MissingNode nodeId)) pure (Map.lookup nodeId nodes)

lookupRegion :: RegionId -> Emit ReifiedRegion
lookupRegion regionIdentifier = do
  regions <- gets (preparedRegions . emissionPrepared)
  maybe (throwError (MissingRegion regionIdentifier)) pure (Map.lookup regionIdentifier regions)

lookupSymbol :: String -> Emit SymbolBinding
lookupSymbol symbol = do
  symbols <- gets emittedSymbols
  maybe (throwError (MissingSymbol symbol)) pure (Map.lookup symbol symbols)

emitFreshLabel :: Emit SpirV.Id
emitFreshLabel = do
  label <- assemble SpirV.emitLabel
  modify' (\state -> state{currentBlock = Just label})
  pure label

emitLabelId :: SpirV.Id -> Emit ()
emitLabelId label = do
  assemble (SpirV.emitLabelId label)
  modify' (\state -> state{currentBlock = Just label})

requireCurrentBlock :: Emit SpirV.Id
requireCurrentBlock = gets currentBlock >>= maybe (throwError MissingCurrentBlock) pure

assemble :: SpirV.Assembler a -> Emit a
assemble instruction =
  ExceptT $
    StateT $ \state ->
      (\value -> (Right value, state)) <$> instruction
