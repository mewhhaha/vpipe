{- | Deterministic SPIR-V 1.6 binary assembly with specification-ordered
sections.

Assemble a minimal module in the @Assembler@ monad and inspect the validated
result:

@
module Main (main) where

import Vpipe.SpirV.Assembler (finishModule, moduleWords, runAssembler, typeVoid)

main :: IO ()
main = print (fmap moduleWords (runAssembler (typeVoid *> finishModule)))
@
-}
module Vpipe.SpirV.Assembler (
  Id,
  idWord,
  AssemblerConfig (..),
  defaultAssemblerConfig,
  AssemblerError (..),
  SpirVModule,
  moduleBytes,
  moduleWords,
  Assembler,
  runAssembler,
  runAssemblerWith,
  encodeInstruction,
  freshId,
  emitCapability,
  emitExtension,
  importExtInst,
  emitEntryPoint,
  emitExecutionMode,
  emitName,
  emitMemberName,
  emitDecorate,
  emitMemberDecorate,
  typeVoid,
  typeBool,
  typeInt,
  typeFloat,
  typeVector,
  typeMatrix,
  typeArray,
  typeDecoratedArray,
  typeRuntimeArray,
  typeDecoratedRuntimeArray,
  typeStruct,
  typeDecoratedStruct,
  typePointer,
  typeFunction,
  ImageType (..),
  typeImage,
  typeSampler,
  typeSampledImage,
  constantBool,
  constantWord,
  constantF32,
  constantComposite,
  emitVariable,
  emitGlobalVariable,
  emitLocalVariable,
  emitFunction,
  emitFunctionWithControl,
  emitFunctionParameter,
  emitLabel,
  emitLabelId,
  emitLoad,
  emitStore,
  emitAccessChain,
  emitArrayLength,
  emitAtomicIAdd,
  emitCompositeConstruct,
  emitCompositeExtract,
  emitCompositeInsert,
  emitVectorShuffle,
  emitSampledImage,
  emitImageSampleImplicitLod,
  emitImageSampleExplicitLod,
  emitImageSampleDrefImplicitLod,
  emitImageSampleDrefExplicitLod,
  UnaryOperation (..),
  emitUnary,
  BinaryOperation (..),
  emitBinary,
  emitSelect,
  emitAny,
  emitAll,
  emitExtInst,
  emitResultInstruction,
  emitSelectionMerge,
  emitLoopMerge,
  emitBranch,
  emitBranchConditional,
  emitPhi,
  emitPhiWithResult,
  emitCopyObjectWithResult,
  emitReturn,
  emitReturnValue,
  emitKill,
  emitFunctionEnd,
  finishModule,
) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString, word32LE)
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Word (Word16, Word32)
import Vpipe.SpirV.Generated

newtype Id = Id Word32 deriving (Eq, Ord, Show)

idWord :: Id -> Word32
idWord (Id value) = value

newtype AssemblerConfig = AssemblerConfig
  { debugNames :: Bool
  }
  deriving (Eq, Show)

defaultAssemblerConfig :: AssemblerConfig
defaultAssemblerConfig = AssemblerConfig{debugNames = True}

data AssemblerError
  = IdBoundOverflow
  | InstructionTooLong Word16 Int
  | StringContainsNul String
  deriving (Eq, Show)

newtype SpirVModule = SpirVModule [Word32] deriving (Eq, Show)

moduleWords :: SpirVModule -> [Word32]
moduleWords (SpirVModule words') = words'

moduleBytes :: SpirVModule -> BL.ByteString
moduleBytes = toLazyByteString . foldMap word32LE . moduleWords

data Section
  = Capabilities
  | Extensions
  | Imports
  | MemoryModel
  | EntryPoints
  | ExecutionModes
  | Debug
  | Annotations
  | TypesGlobals
  | Functions
  deriving (Eq, Ord)

data AssemblerState = AssemblerState
  { config :: AssemblerConfig
  , nextId :: Word32
  , sections :: Map.Map Section [[Word32]]
  , types :: Map.Map TypeKey Id
  , constants :: Map.Map ConstantKey Id
  }

newtype Assembler a = Assembler
  { unAssembler :: AssemblerState -> Either AssemblerError (a, AssemblerState)
  }

instance Functor Assembler where
  fmap f (Assembler run) = Assembler $ \state -> do
    (value, state') <- run state
    pure (f value, state')

instance Applicative Assembler where
  pure value = Assembler $ \state -> Right (value, state)
  Assembler ff <*> Assembler fa = Assembler $ \state -> do
    (f, state') <- ff state
    (a, state'') <- fa state'
    pure (f a, state'')

instance Monad Assembler where
  Assembler fa >>= f = Assembler $ \state -> do
    (a, state') <- fa state
    unAssembler (f a) state'

data ImageType = ImageType
  { imageSampledType :: Id
  , imageDimension :: Word32
  , imageDepth :: Word32
  , imageArrayed :: Word32
  , imageMultisampled :: Word32
  , imageSampled :: Word32
  , imageFormat :: Word32
  , imageAccessQualifier :: Maybe Word32
  }
  deriving (Eq, Ord, Show)

data TypeKey
  = VoidTy
  | BoolTy
  | IntTy Word32 Word32
  | FloatTy Word32
  | VectorTy Id Word32
  | MatrixTy Id Word32
  | ArrayTy Id Id
  | RuntimeArrayTy Id
  | StructTy [Id]
  | PointerTy Word32 Id
  | FunctionTy Id [Id]
  | ImageTy ImageType
  | SamplerTy
  | SampledImageTy Id
  deriving (Eq, Ord)

data ConstantKey
  = BoolConstant Id Bool
  | WordConstant Id [Word32]
  | CompositeConstant Id [Id]
  deriving (Eq, Ord)

data UnaryOperation
  = SNegate
  | FNegate
  | LogicalNot
  | ConvertSToF
  | ConvertFToS
  | Bitcast
  | DPdx
  | DPdy
  | Fwidth
  deriving (Eq, Show)

data BinaryOperation
  = IAdd
  | FAdd
  | ISub
  | FSub
  | IMul
  | FMul
  | SDiv
  | FDiv
  | SRem
  | FRem
  | FMod
  | VectorTimesScalar
  | MatrixTimesScalar
  | VectorTimesMatrix
  | MatrixTimesVector
  | MatrixTimesMatrix
  | LogicalEqual
  | LogicalNotEqual
  | LogicalOr
  | LogicalAnd
  | IEqual
  | INotEqual
  | SLessThan
  | SLessThanEqual
  | SGreaterThan
  | SGreaterThanEqual
  | ULessThan
  | ULessThanEqual
  | UGreaterThan
  | UGreaterThanEqual
  | FOrdEqual
  | FOrdNotEqual
  | FUnordNotEqual
  | FOrdLessThan
  | FOrdLessThanEqual
  | FOrdGreaterThan
  | FOrdGreaterThanEqual
  deriving (Eq, Show)

initialState :: AssemblerConfig -> AssemblerState
initialState assemblerConfig =
  AssemblerState
    { config = assemblerConfig
    , nextId = 1
    , sections = Map.empty
    , types = Map.empty
    , constants = Map.empty
    }

runAssembler :: Assembler a -> Either AssemblerError a
runAssembler = runAssemblerWith defaultAssemblerConfig

runAssemblerWith :: AssemblerConfig -> Assembler a -> Either AssemblerError a
runAssemblerWith assemblerConfig action = fst <$> unAssembler action (initialState assemblerConfig)

freshId :: Assembler Id
freshId = Assembler $ \state ->
  if nextId state == maxBound
    then Left IdBoundOverflow
    else Right (Id (nextId state), state{nextId = nextId state + 1})

emit :: Section -> Word16 -> [Word32] -> Assembler ()
emit section instructionOpcode operands = Assembler $ \state -> do
  words' <- encodeInstruction instructionOpcode operands
  let appendInstruction existing = existing ++ [words']
  pure
    ( ()
    , state
        { sections =
            Map.alter
              (Just . appendInstruction . fromMaybe [])
              section
              (sections state)
        }
    )

encodeInstruction :: Word16 -> [Word32] -> Either AssemblerError [Word32]
encodeInstruction instructionOpcode operands
  | wordCount > 0xffff = Left (InstructionTooLong instructionOpcode wordCount)
  | otherwise =
      Right
        ( ((fromIntegral wordCount `shiftL` 16) .|. fromIntegral instructionOpcode)
            : operands
        )
 where
  wordCount = length operands + 1

emitCapability :: Word32 -> Assembler ()
emitCapability capability = emit Capabilities opCapability [capability]

emitExtension :: String -> Assembler ()
emitExtension extension = emitString Extensions opExtension [] extension []

importExtInst :: String -> Assembler Id
importExtInst name = do
  result <- freshId
  emitString Imports opExtInstImport [idWord result] name []
  pure result

emitEntryPoint :: Word32 -> Id -> String -> [Id] -> Assembler ()
emitEntryPoint model functionId name interfaces =
  emitString
    EntryPoints
    opEntryPoint
    [model, idWord functionId]
    name
    (map idWord interfaces)

emitExecutionMode :: Id -> Word32 -> [Word32] -> Assembler ()
emitExecutionMode functionId mode operands =
  emit ExecutionModes opExecutionMode ([idWord functionId, mode] ++ operands)

emitName :: Id -> String -> Assembler ()
emitName target = emitDebugString opName [idWord target]

emitMemberName :: Id -> Word32 -> String -> Assembler ()
emitMemberName target member =
  emitDebugString opMemberName [idWord target, member]

emitDebugString :: Word16 -> [Word32] -> String -> Assembler ()
emitDebugString instructionOpcode prefix name = Assembler $ \state ->
  if debugNames (config state)
    then unAssembler (emitString Debug instructionOpcode prefix name []) state
    else Right ((), state)

emitDecorate :: Id -> Word32 -> [Word32] -> Assembler ()
emitDecorate target decoration operands =
  emit Annotations opDecorate ([idWord target, decoration] ++ operands)

emitMemberDecorate :: Id -> Word32 -> Word32 -> [Word32] -> Assembler ()
emitMemberDecorate target member decoration operands =
  emit
    Annotations
    opMemberDecorate
    ([idWord target, member, decoration] ++ operands)

internType :: TypeKey -> Word16 -> [Word32] -> Assembler Id
internType key instructionOpcode operands = Assembler $ \state ->
  case Map.lookup key (types state) of
    Just value -> Right (value, state)
    Nothing -> do
      (value, afterId) <- unAssembler freshId state
      ((), afterEmit) <-
        unAssembler
          (emit TypesGlobals instructionOpcode (idWord value : operands))
          afterId
      pure
        ( value
        , afterEmit{types = Map.insert key value (types afterEmit)}
        )

typeVoid, typeBool, typeSampler :: Assembler Id
typeVoid = internType VoidTy opTypeVoid []
typeBool = internType BoolTy opTypeBool []
typeSampler = internType SamplerTy (opcode "OpTypeSampler") []

typeInt :: Word32 -> Word32 -> Assembler Id
typeInt width signedness =
  internType (IntTy width signedness) opTypeInt [width, signedness]

typeFloat :: Word32 -> Assembler Id
typeFloat width = internType (FloatTy width) opTypeFloat [width]

typeVector :: Id -> Word32 -> Assembler Id
typeVector component count =
  internType (VectorTy component count) opTypeVector [idWord component, count]

typeMatrix :: Id -> Word32 -> Assembler Id
typeMatrix columnType columnCount =
  internType
    (MatrixTy columnType columnCount)
    opTypeMatrix
    [idWord columnType, columnCount]

typeArray :: Id -> Id -> Assembler Id
typeArray elementType lengthId =
  internType
    (ArrayTy elementType lengthId)
    opTypeArray
    [idWord elementType, idWord lengthId]

-- Decorations are attached to type IDs.  Layout-specific arrays therefore
-- need a fresh identity even when their structural operands are equal.
typeDecoratedArray :: Id -> Id -> Assembler Id
typeDecoratedArray elementType lengthId = do
  value <- freshId
  emit TypesGlobals opTypeArray [idWord value, idWord elementType, idWord lengthId]
  pure value

typeRuntimeArray :: Id -> Assembler Id
typeRuntimeArray elementType =
  internType (RuntimeArrayTy elementType) opTypeRuntimeArray [idWord elementType]

{- | Create a fresh runtime-array identity for layout decorations. Structural
interning is unsafe here because SPIR-V forbids applying @ArrayStride@ to the
same result ID more than once.
-}
typeDecoratedRuntimeArray :: Id -> Assembler Id
typeDecoratedRuntimeArray elementType = do
  value <- freshId
  emit TypesGlobals opTypeRuntimeArray [idWord value, idWord elementType]
  pure value

typeStruct :: [Id] -> Assembler Id
typeStruct memberTypes =
  internType (StructTy memberTypes) opTypeStruct (map idWord memberTypes)

-- See @typeDecoratedArray@: member offsets and matrix layout decorations make
-- structurally identical block types semantically distinct.
typeDecoratedStruct :: [Id] -> Assembler Id
typeDecoratedStruct memberTypes = do
  value <- freshId
  emit TypesGlobals opTypeStruct (idWord value : map idWord memberTypes)
  pure value

typePointer :: Word32 -> Id -> Assembler Id
typePointer storageClass pointee =
  internType
    (PointerTy storageClass pointee)
    opTypePointer
    [storageClass, idWord pointee]

typeFunction :: Id -> [Id] -> Assembler Id
typeFunction result parameters =
  internType
    (FunctionTy result parameters)
    opTypeFunction
    (idWord result : map idWord parameters)

typeImage :: ImageType -> Assembler Id
typeImage imageType =
  internType (ImageTy imageType) (opcode "OpTypeImage") operands
 where
  operands =
    [ idWord (imageSampledType imageType)
    , imageDimension imageType
    , imageDepth imageType
    , imageArrayed imageType
    , imageMultisampled imageType
    , imageSampled imageType
    , imageFormat imageType
    ]
      ++ maybe [] pure (imageAccessQualifier imageType)

typeSampledImage :: Id -> Assembler Id
typeSampledImage imageType =
  internType
    (SampledImageTy imageType)
    (opcode "OpTypeSampledImage")
    [idWord imageType]

internConstant :: ConstantKey -> Word16 -> Id -> [Word32] -> Assembler Id
internConstant key instructionOpcode resultType operands = Assembler $ \state ->
  case Map.lookup key (constants state) of
    Just value -> Right (value, state)
    Nothing -> do
      (value, afterId) <- unAssembler freshId state
      ((), afterEmit) <-
        unAssembler
          ( emit
              TypesGlobals
              instructionOpcode
              (idWord resultType : idWord value : operands)
          )
          afterId
      pure
        ( value
        , afterEmit{constants = Map.insert key value (constants afterEmit)}
        )

constantBool :: Id -> Bool -> Assembler Id
constantBool resultType value =
  internConstant
    (BoolConstant resultType value)
    (if value then opConstantTrue else opConstantFalse)
    resultType
    []

constantWord :: Id -> [Word32] -> Assembler Id
constantWord resultType words' =
  internConstant
    (WordConstant resultType words')
    opConstant
    resultType
    words'

constantF32 :: Id -> Word32 -> Assembler Id
constantF32 resultType value = constantWord resultType [value]

constantComposite :: Id -> [Id] -> Assembler Id
constantComposite resultType constituents =
  internConstant
    (CompositeConstant resultType constituents)
    opConstantComposite
    resultType
    (map idWord constituents)

emitVariable :: Id -> Word32 -> Assembler Id
emitVariable resultType storageClass =
  emitGlobalVariable resultType storageClass Nothing

emitGlobalVariable :: Id -> Word32 -> Maybe Id -> Assembler Id
emitGlobalVariable resultType storageClass initializer = do
  result <- freshId
  emit
    TypesGlobals
    opVariable
    ( [idWord resultType, idWord result, storageClass]
        ++ maybe [] (pure . idWord) initializer
    )
  pure result

emitLocalVariable :: Id -> Word32 -> Maybe Id -> Assembler Id
emitLocalVariable resultType storageClass initializer = do
  result <- freshId
  emit
    Functions
    opVariable
    ( [idWord resultType, idWord result, storageClass]
        ++ maybe [] (pure . idWord) initializer
    )
  pure result

emitFunction :: Id -> Id -> Assembler Id
emitFunction resultType = emitFunctionWithControl resultType 0

emitFunctionWithControl :: Id -> Word32 -> Id -> Assembler Id
emitFunctionWithControl resultType functionControl functionType = do
  result <- freshId
  emit
    Functions
    opFunction
    [idWord resultType, idWord result, functionControl, idWord functionType]
  pure result

emitFunctionParameter :: Id -> Assembler Id
emitFunctionParameter resultType =
  emitResultInstruction (opcode "OpFunctionParameter") resultType []

emitLabel :: Assembler Id
emitLabel = do
  result <- freshId
  emitLabelId result
  pure result

emitLabelId :: Id -> Assembler ()
emitLabelId label = emit Functions opLabel [idWord label]

emitLoad :: Id -> Id -> Assembler Id
emitLoad resultType pointer = emitResultInstruction opLoad resultType [idWord pointer]

emitStore :: Id -> Id -> Assembler ()
emitStore pointer object = emit Functions opStore [idWord pointer, idWord object]

emitAccessChain :: Id -> Id -> [Id] -> Assembler Id
emitAccessChain resultType base indices =
  emitResultInstruction
    (opcode "OpAccessChain")
    resultType
    (idWord base : map idWord indices)

emitArrayLength :: Id -> Id -> Word32 -> Assembler Id
emitArrayLength resultType structure member =
  emitResultInstruction
    (opcode "OpArrayLength")
    resultType
    [idWord structure, member]

emitAtomicIAdd :: Id -> Id -> Id -> Id -> Id -> Assembler Id
emitAtomicIAdd resultType pointer scope memorySemantics value =
  emitResultInstruction
    (opcode "OpAtomicIAdd")
    resultType
    [idWord pointer, idWord scope, idWord memorySemantics, idWord value]

emitCompositeConstruct :: Id -> [Id] -> Assembler Id
emitCompositeConstruct resultType constituents =
  emitResultInstruction
    (opcode "OpCompositeConstruct")
    resultType
    (map idWord constituents)

emitCompositeExtract :: Id -> Id -> [Word32] -> Assembler Id
emitCompositeExtract resultType composite indices =
  emitResultInstruction
    (opcode "OpCompositeExtract")
    resultType
    (idWord composite : indices)

emitCompositeInsert :: Id -> Id -> Id -> [Word32] -> Assembler Id
emitCompositeInsert resultType object composite indices =
  emitResultInstruction
    (opcode "OpCompositeInsert")
    resultType
    ([idWord object, idWord composite] ++ indices)

emitVectorShuffle :: Id -> Id -> Id -> [Word32] -> Assembler Id
emitVectorShuffle resultType firstVector secondVector components =
  emitResultInstruction
    (opcode "OpVectorShuffle")
    resultType
    ([idWord firstVector, idWord secondVector] ++ components)

emitSampledImage :: Id -> Id -> Id -> Assembler Id
emitSampledImage resultType image sampler =
  emitResultInstruction
    (opcode "OpSampledImage")
    resultType
    [idWord image, idWord sampler]

emitImageSampleImplicitLod :: Id -> Id -> Id -> Assembler Id
emitImageSampleImplicitLod resultType sampledImage coordinates =
  emitResultInstruction
    (opcode "OpImageSampleImplicitLod")
    resultType
    [idWord sampledImage, idWord coordinates]

emitImageSampleExplicitLod :: Id -> Id -> Id -> Word32 -> [Id] -> Assembler Id
emitImageSampleExplicitLod resultType sampledImage coordinates imageOperands operands =
  emitResultInstruction
    (opcode "OpImageSampleExplicitLod")
    resultType
    ( [idWord sampledImage, idWord coordinates, imageOperands]
        ++ map idWord operands
    )

emitImageSampleDrefImplicitLod :: Id -> Id -> Id -> Id -> Assembler Id
emitImageSampleDrefImplicitLod resultType sampledImage coordinates reference =
  emitResultInstruction
    (opcode "OpImageSampleDrefImplicitLod")
    resultType
    [idWord sampledImage, idWord coordinates, idWord reference]

emitImageSampleDrefExplicitLod :: Id -> Id -> Id -> Id -> Word32 -> [Id] -> Assembler Id
emitImageSampleDrefExplicitLod resultType sampledImage coordinates reference imageOperands operands =
  emitResultInstruction
    (opcode "OpImageSampleDrefExplicitLod")
    resultType
    ( [idWord sampledImage, idWord coordinates, idWord reference, imageOperands]
        ++ map idWord operands
    )

emitUnary :: UnaryOperation -> Id -> Id -> Assembler Id
emitUnary operation resultType operand =
  emitResultInstruction
    (unaryOpcode operation)
    resultType
    [idWord operand]

unaryOpcode :: UnaryOperation -> Word16
unaryOpcode operation = opcode $ case operation of
  SNegate -> "OpSNegate"
  FNegate -> "OpFNegate"
  LogicalNot -> "OpLogicalNot"
  ConvertSToF -> "OpConvertSToF"
  ConvertFToS -> "OpConvertFToS"
  Bitcast -> "OpBitcast"
  DPdx -> "OpDPdx"
  DPdy -> "OpDPdy"
  Fwidth -> "OpFwidth"

emitBinary :: BinaryOperation -> Id -> Id -> Id -> Assembler Id
emitBinary operation resultType left right =
  emitResultInstruction
    (binaryOpcode operation)
    resultType
    [idWord left, idWord right]

emitSelect :: Id -> Id -> Id -> Id -> Assembler Id
emitSelect resultType condition trueValue falseValue =
  emitResultInstruction
    (opcode "OpSelect")
    resultType
    [idWord condition, idWord trueValue, idWord falseValue]

emitAny :: Id -> Id -> Assembler Id
emitAny resultType vector =
  emitResultInstruction (opcode "OpAny") resultType [idWord vector]

emitAll :: Id -> Id -> Assembler Id
emitAll resultType vector =
  emitResultInstruction (opcode "OpAll") resultType [idWord vector]

binaryOpcode :: BinaryOperation -> Word16
binaryOpcode operation = opcode $ case operation of
  IAdd -> "OpIAdd"
  FAdd -> "OpFAdd"
  ISub -> "OpISub"
  FSub -> "OpFSub"
  IMul -> "OpIMul"
  FMul -> "OpFMul"
  SDiv -> "OpSDiv"
  FDiv -> "OpFDiv"
  SRem -> "OpSRem"
  FRem -> "OpFRem"
  FMod -> "OpFMod"
  VectorTimesScalar -> "OpVectorTimesScalar"
  MatrixTimesScalar -> "OpMatrixTimesScalar"
  VectorTimesMatrix -> "OpVectorTimesMatrix"
  MatrixTimesVector -> "OpMatrixTimesVector"
  MatrixTimesMatrix -> "OpMatrixTimesMatrix"
  LogicalEqual -> "OpLogicalEqual"
  LogicalNotEqual -> "OpLogicalNotEqual"
  LogicalOr -> "OpLogicalOr"
  LogicalAnd -> "OpLogicalAnd"
  IEqual -> "OpIEqual"
  INotEqual -> "OpINotEqual"
  SLessThan -> "OpSLessThan"
  SLessThanEqual -> "OpSLessThanEqual"
  SGreaterThan -> "OpSGreaterThan"
  SGreaterThanEqual -> "OpSGreaterThanEqual"
  ULessThan -> "OpULessThan"
  ULessThanEqual -> "OpULessThanEqual"
  UGreaterThan -> "OpUGreaterThan"
  UGreaterThanEqual -> "OpUGreaterThanEqual"
  FOrdEqual -> "OpFOrdEqual"
  FOrdNotEqual -> "OpFOrdNotEqual"
  FUnordNotEqual -> "OpFUnordNotEqual"
  FOrdLessThan -> "OpFOrdLessThan"
  FOrdLessThanEqual -> "OpFOrdLessThanEqual"
  FOrdGreaterThan -> "OpFOrdGreaterThan"
  FOrdGreaterThanEqual -> "OpFOrdGreaterThanEqual"

emitExtInst :: Id -> Id -> Word32 -> [Id] -> Assembler Id
emitExtInst resultType instructionSet instructionNumber operands =
  emitResultInstruction
    (opcode "OpExtInst")
    resultType
    ([idWord instructionSet, instructionNumber] ++ map idWord operands)

{- | Emit a result-producing instruction into the function-body section.
This is the escape hatch for grammar opcodes not yet given a typed wrapper;
callers cannot use it to violate module section ordering.
-}
emitResultInstruction :: Word16 -> Id -> [Word32] -> Assembler Id
emitResultInstruction instructionOpcode resultType operands = do
  result <- freshId
  emitResultInstructionWithResult instructionOpcode result resultType operands
  pure result

emitResultInstructionWithResult :: Word16 -> Id -> Id -> [Word32] -> Assembler ()
emitResultInstructionWithResult instructionOpcode result resultType operands =
  emit
    Functions
    instructionOpcode
    ([idWord resultType, idWord result] ++ operands)

emitSelectionMerge :: Id -> Word32 -> Assembler ()
emitSelectionMerge mergeBlock selectionControl =
  emit
    Functions
    (opcode "OpSelectionMerge")
    [idWord mergeBlock, selectionControl]

emitLoopMerge :: Id -> Id -> Word32 -> [Word32] -> Assembler ()
emitLoopMerge mergeBlock continueTarget loopControl operands =
  emit
    Functions
    (opcode "OpLoopMerge")
    ([idWord mergeBlock, idWord continueTarget, loopControl] ++ operands)

emitBranch :: Id -> Assembler ()
emitBranch target = emit Functions (opcode "OpBranch") [idWord target]

emitBranchConditional :: Id -> Id -> Id -> Maybe (Word32, Word32) -> Assembler ()
emitBranchConditional condition trueLabel falseLabel branchWeights =
  emit
    Functions
    (opcode "OpBranchConditional")
    ( [idWord condition, idWord trueLabel, idWord falseLabel]
        ++ maybe [] (\(trueWeight, falseWeight) -> [trueWeight, falseWeight]) branchWeights
    )

emitPhi :: Id -> [(Id, Id)] -> Assembler Id
emitPhi resultType incoming =
  emitResultInstruction
    (opcode "OpPhi")
    resultType
    (concatMap (\(value, parent) -> [idWord value, idWord parent]) incoming)

emitPhiWithResult :: Id -> Id -> [(Id, Id)] -> Assembler ()
emitPhiWithResult result resultType incoming =
  emitResultInstructionWithResult
    (opcode "OpPhi")
    result
    resultType
    (concatMap (\(value, parent) -> [idWord value, idWord parent]) incoming)

emitCopyObjectWithResult :: Id -> Id -> Id -> Assembler ()
emitCopyObjectWithResult result resultType operand =
  emitResultInstructionWithResult
    (opcode "OpCopyObject")
    result
    resultType
    [idWord operand]

emitReturn :: Assembler ()
emitReturn = emit Functions opReturn []

emitReturnValue :: Id -> Assembler ()
emitReturnValue value = emit Functions (opcode "OpReturnValue") [idWord value]

emitKill :: Assembler ()
emitKill = emit Functions (opcode "OpKill") []

emitFunctionEnd :: Assembler ()
emitFunctionEnd = emit Functions opFunctionEnd []

finishModule :: Assembler SpirVModule
finishModule = Assembler $ \state -> do
  ((), afterCapability) <- unAssembler (emitCapability capabilityShader) state
  ((), finalState) <-
    unAssembler
      ( emit
          MemoryModel
          opMemoryModel
          [addressingModelLogical, memoryModelGLSL450]
      )
      afterCapability
  let ordered =
        concatMap
          (concat . flip (Map.findWithDefault []) (sections finalState))
          sectionOrder
  pure
    ( SpirVModule
        ( [0x07230203, 0x00010600, 0, nextId finalState, 0]
            ++ ordered
        )
    , finalState
    )

emitString :: Section -> Word16 -> [Word32] -> String -> [Word32] -> Assembler ()
emitString section instructionOpcode prefix text suffix = Assembler $ \state -> do
  encoded <- stringWords text
  unAssembler
    (emit section instructionOpcode (prefix ++ encoded ++ suffix))
    state

stringWords :: String -> Either AssemblerError [Word32]
stringWords text
  | '\0' `elem` text = Left (StringContainsNul text)
  | otherwise = Right (map packWord (chunksOfFour (utf8Bytes ++ [0])))
 where
  utf8Bytes =
    map fromIntegral (BS.unpack (Text.encodeUtf8 (Text.pack text)))
  chunksOfFour [] = []
  chunksOfFour bytes =
    take 4 (bytes ++ repeat 0) : chunksOfFour (drop 4 bytes)
  packWord bytes =
    foldl'
      (\word (shift, byte) -> word .|. (byte `shiftL` shift))
      0
      (zip [0, 8, 16, 24] bytes)

sectionOrder :: [Section]
sectionOrder =
  [ Capabilities
  , Extensions
  , Imports
  , MemoryModel
  , EntryPoints
  , ExecutionModes
  , Debug
  , Annotations
  , TypesGlobals
  , Functions
  ]
