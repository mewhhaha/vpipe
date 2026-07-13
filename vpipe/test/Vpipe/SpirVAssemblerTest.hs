module Vpipe.SpirVAssemblerTest (spirVAssemblerTests) where

import Control.Exception (bracket)
import Control.Monad (replicateM)
import Data.Bits (shiftR, (.&.))
import Data.ByteString.Lazy qualified as BL
import Data.List (find, isInfixOf, nub)
import Data.Word (Word16, Word32)
import System.Directory (findExecutable, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO (hClose, openBinaryTempFile)
import System.IO.Error (catchIOError)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (NonNegative (..), Property, counterexample, testProperty, (===))
import Vpipe.SpirV
import Vpipe.SpirV.Assembler
import Vpipe.SpirV.Generated (enumerant, opcode)

spirVAssemblerTests :: TestTree
spirVAssemblerTests =
  testGroup
    "SPIR-V assembler"
    [ testCase "writes the SPIR-V 1.6 header and bound" headerCase
    , testProperty "fresh result IDs are unique and contiguous" freshIdsUniqueProperty
    , testCase "keeps parsed instructions in specification section order" sectionOrderCase
    , testCase "interns types and constants by first encounter" interningCase
    , testCase "keeps independently decorated runtime arrays distinct" decoratedRuntimeArrayCase
    , testCase "independent assemblies are byte deterministic" determinismCase
    , testCase "encodes UTF-8 strings as little-endian padded words" stringCase
    , testCase "rejects embedded NUL strings when debug names are enabled" nulStringCase
    , testCase "omits debug names when configured off" debugNamesDisabledCase
    , testCase "rejects instructions exceeding SPIR-V word count" wordCountCase
    , testCase "encodes array-length and atomic-add wrappers" storageWrapperCase
    , testCase "starter vertex selects three positions from VertexIndex" triangleVertexCase
    , validatorCase "validates the starter vertex module" vertexModule
    , validatorCase "validates the starter fragment module" fragmentModule
    , validatorCase "validates the starter compute module" computeModule
    , validatorCase "validates structured branches, arithmetic, and phi nodes" structuredComputeModule
    ]

headerCase :: IO ()
headerCase = case vertexModule of
  Left assemblerError -> assertFailure (show assemblerError)
  Right spirVModule ->
    take 5 (moduleWords spirVModule)
      @?= [0x07230203, 0x00010600, 0, 27, 0]

freshIdsUniqueProperty :: NonNegative Int -> Property
freshIdsUniqueProperty (NonNegative requested) =
  let count = requested `mod` 4096
      result = runAssembler (replicateM count freshId)
      expected = fmap fromIntegral [1 .. count]
   in counterexample ("allocated " <> show count <> " IDs") $
        case result of
          Left error' -> counterexample (show error') False
          Right identifiers ->
            let words' = fmap idWord identifiers
             in (length words' == length (nub words') && words' == expected) === True

triangleVertexCase :: IO ()
triangleVertexCase = case vertexModule of
  Left assemblerError -> assertFailure (show assemblerError)
  Right spirVModule -> case parseInstructions (drop 5 (moduleWords spirVModule)) of
    Left message -> assertFailure message
    Right instructions -> do
      let vertexIndexDecoration =
            [ operands
            | (instructionOpcode, operands) <- instructions
            , instructionOpcode == opcode "OpDecorate"
            , drop 1 operands == [enumerant "Decoration" "BuiltIn", enumerant "BuiltIn" "VertexIndex"]
            ]
      length vertexIndexDecoration @?= 1
      length (filter ((== opcode "OpConstantComposite") . fst) instructions) @?= 3
      length (filter ((== opcode "OpSelect") . fst) instructions) @?= 2

sectionOrderCase :: IO ()
sectionOrderCase = case sectionOrderingModule of
  Left assemblerError -> assertFailure (show assemblerError)
  Right spirVModule -> case parseInstructions (drop 5 (moduleWords spirVModule)) of
    Left message -> assertFailure message
    Right instructions -> do
      let opcodes = map fst instructions
      assertBool
        "capability precedes memory model"
        (opcode "OpCapability" `appearsBefore` opcode "OpMemoryModel" $ opcodes)
      assertBool
        "capability, extension, import, and memory model are ordered"
        ( (opcode "OpCapability" `appearsBefore` opcode "OpExtension" $ opcodes)
            && (opcode "OpExtension" `appearsBefore` opcode "OpExtInstImport" $ opcodes)
            && (opcode "OpExtInstImport" `appearsBefore` opcode "OpMemoryModel" $ opcodes)
        )
      assertBool
        "entry point precedes execution mode"
        (opcode "OpEntryPoint" `appearsBefore` opcode "OpExecutionMode" $ opcodes)
      assertBool
        "debug name precedes decoration"
        (opcode "OpName" `appearsBefore` opcode "OpDecorate" $ opcodes)
      assertBool
        "decoration precedes type declaration"
        (opcode "OpDecorate" `appearsBefore` opcode "OpTypeVoid" $ opcodes)
      assertBool
        "type declaration precedes function body"
        (opcode "OpTypeVoid" `appearsBefore` opcode "OpFunction" $ opcodes)

sectionOrderingModule :: Either AssemblerError SpirVModule
sectionOrderingModule = runAssembler $ do
  emitExtension "SPV_KHR_non_semantic_info"
  _ <- importExtInst "GLSL.std.450"
  void <- typeVoid
  functionType <- typeFunction void []
  float <- typeFloat 32
  vector4 <- typeVector float 4
  outputPointer <- typePointer 3 vector4
  zero <- constantF32 float 0
  outputValue <- constantComposite vector4 [zero, zero, zero, zero]
  output <- emitVariable outputPointer 3
  function <- emitFunction void functionType
  emitEntryPoint 4 function "main" [output]
  emitExecutionMode function 7 []
  emitName output "color"
  emitDecorate output 30 [0]
  _ <- emitLabel
  emitStore output outputValue
  emitReturn
  emitFunctionEnd
  finishModule

interningCase :: IO ()
interningCase = runAssembler assemble @?= Right [1, 2, 1, 2]
 where
  assemble = do
    firstFloat <- typeFloat 32
    firstZero <- constantF32 firstFloat 0
    secondFloat <- typeFloat 32
    secondZero <- constantF32 secondFloat 0
    pure (map idWord [firstFloat, firstZero, secondFloat, secondZero])

decoratedRuntimeArrayCase :: IO ()
decoratedRuntimeArrayCase =
  runAssembler assemble @?= Right [2, 3]
 where
  assemble = do
    float <- typeFloat 32
    first <- typeDecoratedRuntimeArray float
    second <- typeDecoratedRuntimeArray float
    pure (fmap idWord [first, second])

determinismCase :: IO ()
determinismCase = do
  first <- either (assertFailure . show) pure (assembleDeterministic ())
  second <- either (assertFailure . show) pure (assembleDeterministic ())
  moduleBytes first @?= moduleBytes second

assembleDeterministic :: () -> Either AssemblerError SpirVModule
assembleDeterministic () = runAssembler $ do
  void <- typeVoid
  functionType <- typeFunction void []
  function <- emitFunction void functionType
  emitEntryPoint 5 function "main" []
  emitExecutionMode function 17 [1, 1, 1]
  _ <- emitLabel
  emitReturn
  emitFunctionEnd
  finishModule

structuredComputeModule :: Either AssemblerError SpirVModule
structuredComputeModule = runAssembler $ do
  void <- typeVoid
  bool <- typeBool
  int32 <- typeInt 32 1
  functionType <- typeFunction void []
  condition <- constantBool bool True
  one <- constantWord int32 [1]
  two <- constantWord int32 [2]
  entryLabel <- freshId
  trueLabel <- freshId
  falseLabel <- freshId
  mergeLabel <- freshId
  function <- emitFunction void functionType
  emitEntryPoint (enumerant "ExecutionModel" "GLCompute") function "main" []
  emitExecutionMode function (enumerant "ExecutionMode" "LocalSize") [1, 1, 1]
  emitLabelId entryLabel
  sumValue <- emitBinary IAdd int32 one two
  emitSelectionMerge mergeLabel 0
  emitBranchConditional condition trueLabel falseLabel Nothing
  emitLabelId trueLabel
  emitBranch mergeLabel
  emitLabelId falseLabel
  emitBranch mergeLabel
  emitLabelId mergeLabel
  _ <- emitPhi int32 [(sumValue, trueLabel), (two, falseLabel)]
  emitReturn
  emitFunctionEnd
  finishModule

storageWrapperCase :: IO ()
storageWrapperCase = case runAssembler assemble of
  Left assemblerError -> assertFailure (show assemblerError)
  Right spirVModule -> case parseInstructions (drop 5 (moduleWords spirVModule)) of
    Left message -> assertFailure message
    Right instructions -> do
      assertBool "contains OpArrayLength" (any ((== opcode "OpArrayLength") . fst) instructions)
      assertBool "contains OpAtomicIAdd" (any ((== opcode "OpAtomicIAdd") . fst) instructions)
 where
  assemble = do
    void <- typeVoid
    word <- typeInt 32 0
    functionType <- typeFunction void []
    structure <- freshId
    pointer <- freshId
    scope <- constantWord word [1]
    semantics <- constantWord word [72]
    value <- constantWord word [1]
    function <- emitFunction void functionType
    emitEntryPoint (enumerant "ExecutionModel" "GLCompute") function "main" []
    emitExecutionMode function (enumerant "ExecutionMode" "LocalSize") [1, 1, 1]
    _ <- emitLabel
    _ <- emitArrayLength word structure 0
    _ <- emitAtomicIAdd word pointer scope semantics value
    emitReturn
    emitFunctionEnd
    finishModule

stringCase :: IO ()
stringCase = case runAssembler assemble of
  Left assemblerError -> assertFailure (show assemblerError)
  Right spirVModule -> case parseInstructions (drop 5 (moduleWords spirVModule)) of
    Left message -> assertFailure message
    Right instructions -> case find ((== opcode "OpName") . fst) instructions of
      Nothing -> assertFailure "OpName was not emitted"
      Just (_, operands) -> do
        operands @?= [1, 0xc3666163, 0x000000a9]
        assertBool
          "serialized bytes contain the UTF-8 text and NUL padding"
          ([0x63, 0x61, 0x66, 0xc3, 0xa9, 0, 0, 0] `isInfixOf` BL.unpack (moduleBytes spirVModule))
 where
  assemble = do
    target <- freshId
    emitName target "café"
    finishModule

nulStringCase :: IO ()
nulStringCase = case runAssembler assemble of
  Left (StringContainsNul _) -> pure ()
  result -> assertFailure ("expected StringContainsNul, got " ++ show result)
 where
  assemble = do
    target <- freshId
    emitName target "bad\0name"

debugNamesDisabledCase :: IO ()
debugNamesDisabledCase = case runAssemblerWith configWithoutNames assemble of
  Left assemblerError -> assertFailure (show assemblerError)
  Right spirVModule -> case parseInstructions (drop 5 (moduleWords spirVModule)) of
    Left message -> assertFailure message
    Right instructions ->
      assertBool
        "debug instructions are absent"
        (all ((/= opcode "OpName") . fst) instructions)
 where
  configWithoutNames = defaultAssemblerConfig{debugNames = False}
  assemble = do
    target <- freshId
    emitName target "ignored\0even-with-a-NUL"
    emitMemberName target 0 "also ignored"
    finishModule

wordCountCase :: IO ()
wordCountCase =
  encodeInstruction 1 (replicate 0xffff 0)
    @?= Left (InstructionTooLong 1 0x10000)

validatorCase :: String -> Either AssemblerError SpirVModule -> TestTree
validatorCase description assembled = testCase description $ do
  validator <- findExecutable "spirv-val"
  case validator of
    Nothing -> pure ()
    Just executable -> case assembled of
      Left assemblerError -> assertFailure (show assemblerError)
      Right spirVModule ->
        withModuleFile spirVModule $ \path -> do
          (exitCode, standardOutput, standardError) <-
            readProcessWithExitCode
              executable
              ["--target-env", "vulkan1.3", path]
              ""
          case exitCode of
            ExitSuccess -> pure ()
            ExitFailure code ->
              assertFailure
                ( "spirv-val exited with "
                    ++ show code
                    ++ "\nstdout:\n"
                    ++ standardOutput
                    ++ "\nstderr:\n"
                    ++ standardError
                )

withModuleFile :: SpirVModule -> (FilePath -> IO a) -> IO a
withModuleFile spirVModule action = do
  temporaryDirectory <- getTemporaryDirectory
  bracket
    (openBinaryTempFile temporaryDirectory "vpipe-module.spv")
    ( \(path, handle) -> do
        hClose handle `catchIOError` const (pure ())
        removeFile path `catchIOError` const (pure ())
    )
    ( \(path, handle) -> do
        BL.hPut handle (moduleBytes spirVModule)
        hClose handle
        action path
    )

parseInstructions :: [Word32] -> Either String [(Word16, [Word32])]
parseInstructions = go
 where
  go [] = Right []
  go words'@(firstWord : _) = do
    let wordCount = fromIntegral (firstWord `shiftR` 16)
    if wordCount == 0
      then Left "instruction declares a zero word count"
      else do
        let (instructionWords, remaining) = splitAt wordCount words'
        if length instructionWords /= wordCount
          then Left "instruction extends beyond the module"
          else do
            rest <- go remaining
            let instructionOpcode = fromIntegral (firstWord .&. 0xffff)
            pure ((instructionOpcode, drop 1 instructionWords) : rest)

appearsBefore :: (Eq a) => a -> a -> [a] -> Bool
appearsBefore first second values =
  case (lookupIndex first values, lookupIndex second values) of
    (Just firstIndex, Just secondIndex) -> firstIndex < secondIndex
    _ -> False

lookupIndex :: (Eq a) => a -> [a] -> Maybe Int
lookupIndex target = go 0
 where
  go _ [] = Nothing
  go index (value : values)
    | target == value = Just index
    | otherwise = go (index + 1) values
