{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module Vpipe.SpirVCodegenTest (spirVCodegenTests) where

import Control.Exception (bracket, try)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Int (Int32)
import Data.List (isInfixOf)
import Data.Word (Word32)
import DataReifyPrototype (dataReifyNodeCount)
import GHC.Clock (getMonotonicTimeNSec)
import Linear (M23, M32, V2 (..), V3 (..), V4 (..))
import System.Directory (findExecutable, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO (hClose, openBinaryTempFile)
import System.IO.Error (catchIOError)
import System.Process (readProcessWithExitCode)
import Test.QuickCheck (Gen, Property, choose, forAllBlind, frequency, ioProperty, property, sized, (.&&.), (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Vpipe.Buffer.Format qualified as Buffer
import Vpipe.Expr
import Vpipe.Expr.Internal (BinaryOp (..), BinderId (..), Expr (..), ExprNode (..), ExprObject (..), HostValue (..), ImageDimension (..), ShaderTy (..), SomeExpr (..), UnaryOp (..))
import Vpipe.Expr.Reify
import Vpipe.Format (Format (D32Sfloat, R8G8B8A8Unorm))
import Vpipe.Image.Types (Dim (Cube, D2, D2Array))
import Vpipe.SpirV.Assembler (SpirVModule, moduleBytes, moduleWords)
import Vpipe.SpirV.Codegen

spirVCodegenTests :: TestTree
spirVCodegenTests =
  testGroup
    "SPIR-V codegen"
    [ testCase "compiles and validates a constant fragment output" constantFragmentCase
    , constantFragmentGolden
    , testCase "compiles every scalar and vector unary operation" unaryOperationsCase
    , testCase "compiles arithmetic, comparisons, composites, and interpolation operations" valueOperationsCase
    , testCase "compiles branch and nested loop control flow" structuredControlFlowCase
    , testCase "preserves sharing across a diamond and across roots" sharingCase
    , testCase "compiles a thirty-level shared diamond in bounded time" deepSharingCompileCase
    , testCase "compares local and data-reify sharing on the same diamond" dataReifyPrototypeCase
    , testCase "transposes rectangular matrix literals and validates matrix operations" matrixCase
    , testCase "emits task02 uniform layouts and built-in interfaces" layoutAndBuiltInCase
    , testCase "emits std430 push-constant blocks" pushConstantCase
    , testCase "keeps std140 and std430 decorated array types distinct" distinctLayoutTypesCase
    , testCase "keeps block roots distinct from nested aggregate structs" blockRootIdentityCase
    , testCase "decorates a shared block root type exactly once" sharedBlockRootCase
    , testCase "emits DepthReplacing for FragDepth outputs" fragDepthCase
    , testCase "accepts GlobalInvocationId only as a compute uint3 input" globalInvocationIdCase
    , testCase "emits unsigned arithmetic, ordering, and vector equality" unsignedOperationsCase
    , testCase "compiles combined and separate implicit and explicit sampling" samplingCase
    , testCase "emits cube and array image dimensions" dimensionalSamplingCase
    , testCase "compiles depth-comparison sampling" comparisonSamplingCase
    , testCase "debug configuration changes names without changing validity" debugCase
    , testCase "rejects malformed declarations and graphs before emission" preflightCase
    , testCase "rejects malformed local binder ownership, scope, and types" binderPreflightCase
    , testCase "reports cyclic expressions as ReifyCycle" reifyCycleCase
    , testProperty "finite float expression forests compile deterministically" finiteForestProperty
    , testProperty "structured vector expression forests compile deterministically" structuredVectorProperty
    ]

data RootSpec
  = forall s a. OutputRoot String ShaderTy (Expr s a)
  | forall s. DiscardRoot (Expr s Bool)

constantFragmentCase :: IO ()
constantFragmentCase = constantFragmentModule >>= validateModule

constantFragmentModule :: IO SpirVModule
constantFragmentModule = compileFragment [OutputRoot "color" (TyVector 4) (constant (V4 1 0 0 1) :: F (V4 Float))] [] []

constantFragmentWords :: IO BL.ByteString
constantFragmentWords = do
  compiled <- constantFragmentModule
  pure (BL8.pack (unlines (map show (moduleWords compiled))))

data ConstantFragmentGolden = ConstantFragmentGolden FilePath BL.ByteString

constantFragmentGolden :: TestTree
constantFragmentGolden =
  goldenTest
    "renders a constant fragment with spirv-dis or raw-word golden"
    constantFragmentGoldenReference
    constantFragmentGoldenOutput
    compareConstantFragmentGolden
    writeConstantFragmentGolden

constantFragmentGoldenReference :: IO ConstantFragmentGolden
constantFragmentGoldenReference = do
  goldenPath <- constantFragmentGoldenPath
  ConstantFragmentGolden goldenPath <$> BL.readFile goldenPath

constantFragmentGoldenOutput :: IO ConstantFragmentGolden
constantFragmentGoldenOutput = do
  compiled <- constantFragmentModule
  disassembler <- findExecutable "spirv-dis"
  case disassembler of
    Nothing -> ConstantFragmentGolden rawWordsGoldenPath <$> constantFragmentWords
    Just executable -> do
      disassembly <- disassembleForGolden executable compiled
      pure (ConstantFragmentGolden disassemblyGoldenPath (BL8.pack disassembly))

constantFragmentGoldenPath :: IO FilePath
constantFragmentGoldenPath = do
  disassembler <- findExecutable "spirv-dis"
  pure $ case disassembler of
    Nothing -> rawWordsGoldenPath
    Just _ -> disassemblyGoldenPath

compareConstantFragmentGolden :: ConstantFragmentGolden -> ConstantFragmentGolden -> IO (Maybe String)
compareConstantFragmentGolden (ConstantFragmentGolden goldenPath expected) (ConstantFragmentGolden actualPath actual)
  | goldenPath /= actualPath = pure (Just ("golden selection changed from '" <> actualPath <> "' to '" <> goldenPath <> "'"))
  | expected == actual = pure Nothing
  | otherwise = pure (Just ("constant fragment output differs from '" <> goldenPath <> "'"))

writeConstantFragmentGolden :: ConstantFragmentGolden -> IO ()
writeConstantFragmentGolden (ConstantFragmentGolden goldenPath contents) = BL.writeFile goldenPath contents

rawWordsGoldenPath :: FilePath
rawWordsGoldenPath = "test/golden/spirv-codegen/constant-fragment.words"

disassemblyGoldenPath :: FilePath
disassemblyGoldenPath = "test/golden/spirv-codegen/constant-fragment.spvasm"

disassembleForGolden :: FilePath -> SpirVModule -> IO String
disassembleForGolden executable spirVModule = withModuleFile spirVModule $ \path -> do
  -- The header and friendly identifiers are presentation metadata generated
  -- by spirv-dis; retain its instruction text with stable raw SPIR-V ids.
  (exitCode, standardOutput, standardError) <- readProcessWithExitCode executable ["--no-color", "--no-header", "--no-indent", "--raw-id", path] ""
  case exitCode of
    ExitSuccess -> pure standardOutput
    ExitFailure code ->
      ioError
        ( userError
            ( "spirv-dis exited with "
                <> show code
                <> "\\n"
                <> standardOutput
                <> standardError
            )
        )

unaryOperationsCase :: IO ()
unaryOperationsCase = do
  let scalar = constant 0.5 :: F Float
      vector = constant (V3 1 2 3) :: F (V3 Float)
      outputs =
        [ OutputRoot "negate" TyFloat (negate scalar)
        , OutputRoot "abs" TyFloat (abs scalar)
        , OutputRoot "signum" TyFloat (signum scalar)
        , OutputRoot "recip" TyFloat (recip scalar)
        , OutputRoot "sin" TyFloat (sin scalar)
        , OutputRoot "cos" TyFloat (cos scalar)
        , OutputRoot "tan" TyFloat (tan scalar)
        , OutputRoot "asin" TyFloat (asin scalar)
        , OutputRoot "acos" TyFloat (acos scalar)
        , OutputRoot "atan" TyFloat (atan scalar)
        , OutputRoot "exp" TyFloat (exp scalar)
        , OutputRoot "log" TyFloat (log scalar)
        , OutputRoot "sqrt" TyFloat (sqrt scalar)
        , OutputRoot "normalize" (TyVector 3) (normalize vector)
        , OutputRoot "dfdx" TyFloat (dFdx scalar)
        , OutputRoot "dfdy" TyFloat (dFdy scalar)
        , OutputRoot "fwidth" TyFloat (fwidth scalar)
        , OutputRoot "intNegate" TyInt (negate (constant 7 :: F Int32))
        , OutputRoot "intAbs" TyInt (abs (constant (-7) :: F Int32))
        , OutputRoot "intSign" TyInt (signum (constant (-7) :: F Int32))
        ]
  compiled <- compileFragment outputs [] []
  validateModule compiled

valueOperationsCase :: IO ()
valueOperationsCase = do
  let left = constant 2 :: F Float
      right = constant 3 :: F Float
      vectorLeft = constant (V3 1 0 0) :: F (V3 Float)
      vectorRight = constant (V3 0 1 0) :: F (V3 Float)
      selected = ifE (left <. right) left right
      branched = ifThenElseE (left <. right) (left + right) (left - right)
      outputs =
        [ OutputRoot "add" TyFloat (left + right)
        , OutputRoot "subtract" TyFloat (left - right)
        , OutputRoot "multiply" TyFloat (left * right)
        , OutputRoot "divide" TyFloat (left / right)
        , OutputRoot "power" TyFloat (left ** right)
        , OutputRoot "minimum" TyFloat (clamp left (constant 0) right)
        , OutputRoot "mix" TyFloat (mix left right (constant 0.25))
        , OutputRoot "smoothstep" TyFloat (smoothstep left right (constant 2.5))
        , OutputRoot "dot" TyFloat (dot vectorLeft vectorRight)
        , OutputRoot "cross" (TyVector 3) (cross vectorLeft vectorRight)
        , OutputRoot "reflect" (TyVector 3) (reflect vectorLeft vectorRight)
        , OutputRoot "construct" (TyVector 4) (vec4 left right selected branched)
        , OutputRoot "shuffle" (TyVector 2) (xy (constant (V4 1 2 3 4) :: F (V4 Float)))
        ]
      comparisons =
        [ DiscardRoot (left ==. right)
        , DiscardRoot (left /=. right)
        , DiscardRoot (left <. right)
        , DiscardRoot (left <=. right)
        , DiscardRoot (left >. right)
        , DiscardRoot (left >=. right)
        , DiscardRoot (vectorLeft ==. vectorRight)
        , DiscardRoot (vectorLeft /=. vectorRight)
        , DiscardRoot ((constant True :: F Bool) ==. constant False)
        , DiscardRoot ((constant True :: F Bool) /=. constant False)
        ]
  compiled <- compileFragment (outputs ++ comparisons) [] []
  validateModule compiled

structuredControlFlowCase :: IO ()
structuredControlFlowCase = do
  let zero = constant 0 :: F Float
      one = constant 1 :: F Float
      inner = whileE (\value -> ifThenElseE (value <. constant 1) (value <. constant 2) (value <. constant 3)) (\value -> ifThenElseE (value <. constant 1) (value + one) (value + one))
      looped = whileE (<. constant 3) (inner . (+ one)) zero
      branched = ifThenElseE (zero <. one) looped (one + one)
  compiled <- compileFragment [OutputRoot "value" TyFloat branched] [] []
  validateModule compiled

sharingCase :: IO ()
sharingCase = do
  let inputValue = input "shared" :: F Float
      shared = inputValue * inputValue
      left = shared + constant 1
      right = shared - constant 1
  forest <- reifyExprForest [SomeExpr left, SomeExpr right]
  length (filter isMultiply (forestNodes forest)) @?= 1
  let shader =
        fragmentShaderFromForest
          defaultCodegenConfig
          [OutputSpec "left" TyFloat, OutputSpec "right" TyFloat]
          [StageInput "shared" (ExpressionType TyFloat) (Location 0 Smooth)]
          []
          forest
  first <- compileSuccessfully shader
  second <- compileSuccessfully shader
  moduleBytes first @?= moduleBytes second
  validateModule first
 where
  isMultiply node = case reifiedOp node of
    RBinary MultiplyE _ _ -> True
    _ -> False

deepSharingCompileCase :: IO ()
deepSharingCompileCase = do
  started <- getMonotonicTimeNSec
  forest <- reifyExprForest [SomeExpr (sharedDiamond 30)]
  let shader = fragmentShaderFromForest defaultCodegenConfig [OutputSpec "value" TyFloat] [] [] forest
  compiled <- compileSuccessfully shader
  finished <- getMonotonicTimeNSec
  length (forestNodes forest) @?= 31
  let elapsedMilliseconds = fromIntegral (finished - started) / 1_000_000 :: Double
  assertBool
    ("thirty-level shared expression compiled in " <> show elapsedMilliseconds <> "ms")
    (elapsedMilliseconds < 250)
  validateModule compiled

dataReifyPrototypeCase :: IO ()
dataReifyPrototypeCase = do
  let expression = SomeExpr (sharedDiamond 30)
  localStarted <- getMonotonicTimeNSec
  localForest <- reifyExprForest [expression]
  localFinished <- getMonotonicTimeNSec
  dataReifyNodes <- dataReifyNodeCount expression
  dataReifyFinished <- getMonotonicTimeNSec
  length (forestNodes localForest) @?= 31
  dataReifyNodes @?= 31
  let localMilliseconds = fromIntegral (localFinished - localStarted) / 1_000_000 :: Double
      dataReifyMilliseconds = fromIntegral (dataReifyFinished - localFinished) / 1_000_000 :: Double
  assertBool
    ("local reification took " <> show localMilliseconds <> "ms")
    (localMilliseconds < 250)
  assertBool
    ("data-reify reification took " <> show dataReifyMilliseconds <> "ms")
    (dataReifyMilliseconds < 250)

sharedDiamond :: Int -> F Float
sharedDiamond 0 = constant 1
sharedDiamond level =
  let shared = sharedDiamond (level - 1)
   in shared + shared

matrixCase :: IO ()
matrixCase = do
  let matrix23 = constant (V2 (V3 1 2 3) (V3 4 5 6)) :: F (M23 Float)
      matrix32 = constant (V3 (V2 1 2) (V2 3 4) (V2 5 6)) :: F (M32 Float)
      multiplied = matrix23 !*! matrix32
      transformed = multiplied !* (constant (V2 1 2) :: F (V2 Float))
      componentwise = (matrix23 + matrix23) !* (constant (V3 1 2 3) :: F (V3 Float))
      compared = matrix23 ==. matrix23
  compiled <- compileFragment [OutputRoot "product" (TyVector 2) transformed, OutputRoot "sum" (TyVector 2) componentwise, DiscardRoot compared] [] []
  validateModule compiled
  disassembly <- disassembleModule compiled
  case disassembly of
    Nothing -> pure ()
    Just text -> do
      assertBool "M23 is a three-column SPIR-V matrix" ("OpTypeMatrix" `isInfixOf` text)
      assertBool "matrix multiplication uses a core instruction" ("OpMatrixTimesMatrix" `isInfixOf` text)

layoutAndBuiltInCase :: IO ()
layoutAndBuiltInCase = do
  let uniformExpression = input "uniform.transform" :: V (M23 Float)
      transformed = uniformExpression !* (constant (V3 1 2 3) :: V (V3 Float))
      position = vec4 (x transformed) (y transformed) (constant 0) (constant 1)
  forest <- reifyExprForest [SomeExpr position]
  let uniformBlock =
        UniformBlockDeclaration
          { uniformBlockName = "globals"
          , uniformBlockLocation = DescriptorLocation 0 0
          , uniformBlockStandard = Buffer.Std140
          , uniformBlockLayout = Buffer.Struct [Buffer.Matrix 3 2 Buffer.Float32]
          , uniformBlockLeaves = [UniformLeaf "uniform.transform" [0] (TyMatrix 2 3)]
          }
      shader =
        ShaderModule
          { shaderCodegenConfig = defaultCodegenConfig
          , shaderStage = VertexShader
          , shaderEntryPoint = "main"
          , shaderLocalSize = Nothing
          , shaderInputs = []
          , shaderOutputs = [StageOutput "position" (ExpressionType (TyVector 4)) (BuiltIn Position)]
          , shaderResources = [UniformBlockResource uniformBlock]
          , shaderForest = forest
          , shaderActions = case forestRoots forest of
              [root] -> [StoreOutput "position" root]
              _ -> []
          }
  compiled <- compileSuccessfully shader
  validateModule compiled
  disassembly <- disassembleModule compiled
  case disassembly of
    Nothing -> pure ()
    Just text -> do
      assertBool "uniform block is decorated Block" ("OpDecorate" `isInfixOf` text && "Block" `isInfixOf` text)
      assertBool "matrix layout is column-major with a stride" ("MatrixStride 16" `isInfixOf` text && "ColMajor" `isInfixOf` text)
      assertBool "block members are named with debug info" ("OpMemberName" `isInfixOf` text)

distinctLayoutTypesCase :: IO ()
distinctLayoutTypesCase = do
  forest <- reifyExprForest [SomeExpr (constant (V4 0 0 0 1) :: V (V4 Float))]
  let layout = Buffer.Struct [Buffer.Array 2 (Buffer.Vector 2 Buffer.Float32)]
      block name location standard =
        UniformBlockResource
          UniformBlockDeclaration
            { uniformBlockName = name
            , uniformBlockLocation = location
            , uniformBlockStandard = standard
            , uniformBlockLayout = layout
            , uniformBlockLeaves = []
            }
      shader =
        ShaderModule
          { shaderCodegenConfig = defaultCodegenConfig
          , shaderStage = VertexShader
          , shaderEntryPoint = "main"
          , shaderLocalSize = Nothing
          , shaderInputs = []
          , shaderOutputs = [StageOutput "position" (ExpressionType (TyVector 4)) (BuiltIn Position)]
          , shaderResources = [block "ubo" (DescriptorLocation 0 0) Buffer.Std140, block "ssbo" (DescriptorLocation 0 1) Buffer.Std430]
          , shaderForest = forest
          , shaderActions = case forestRoots forest of
              [root] -> [StoreOutput "position" root]
              _ -> []
          }
  compiled <- compileSuccessfully shader
  validateModule compiled
  disassembly <- disassembleModule compiled
  case disassembly of
    Nothing -> pure ()
    Just text -> do
      assertBool "std140 array stride is retained" ("ArrayStride 16" `isInfixOf` text)
      assertBool "std430 array stride is retained" ("ArrayStride 8" `isInfixOf` text)

blockRootIdentityCase :: IO ()
blockRootIdentityCase = do
  forest <- reifyExprForest [SomeExpr (constant (V4 0 0 0 1) :: V (V4 Float))]
  let block name binding layout =
        UniformBlockResource
          UniformBlockDeclaration
            { uniformBlockName = name
            , uniformBlockLocation = DescriptorLocation 0 binding
            , uniformBlockStandard = Buffer.Std140
            , uniformBlockLayout = layout
            , uniformBlockLeaves = []
            }
      shader =
        ShaderModule
          { shaderCodegenConfig = defaultCodegenConfig
          , shaderStage = VertexShader
          , shaderEntryPoint = "main"
          , shaderLocalSize = Nothing
          , shaderInputs = []
          , shaderOutputs = [StageOutput "position" (ExpressionType (TyVector 4)) (BuiltIn Position)]
          , shaderResources =
              [ block "scalarBlock" 0 (Buffer.Struct [Buffer.Scalar Buffer.Float32])
              , block "nestedBlock" 1 (Buffer.Struct [Buffer.Struct [Buffer.Scalar Buffer.Float32]])
              ]
          , shaderForest = forest
          , shaderActions = case forestRoots forest of
              [root] -> [StoreOutput "position" root]
              _ -> []
          }
  compiled <- compileSuccessfully shader
  validateModule compiled
  disassembly <- disassembleModule compiled
  case disassembly of
    Nothing -> pure ()
    Just text ->
      length (filter (" Block" `isInfixOf`) (lines text)) @?= 2

sharedBlockRootCase :: IO ()
sharedBlockRootCase = do
  forest <- reifyExprForest [SomeExpr (constant (V4 0 0 0 1) :: V (V4 Float))]
  let layout = Buffer.Struct [Buffer.Scalar Buffer.Float32]
      block name binding =
        UniformBlockResource
          UniformBlockDeclaration
            { uniformBlockName = name
            , uniformBlockLocation = DescriptorLocation 0 binding
            , uniformBlockStandard = Buffer.Std140
            , uniformBlockLayout = layout
            , uniformBlockLeaves = []
            }
      shader =
        ShaderModule
          { shaderCodegenConfig = defaultCodegenConfig
          , shaderStage = VertexShader
          , shaderEntryPoint = "main"
          , shaderLocalSize = Nothing
          , shaderInputs = []
          , shaderOutputs = [StageOutput "position" (ExpressionType (TyVector 4)) (BuiltIn Position)]
          , shaderResources = [block "first" 0, block "second" 1]
          , shaderForest = forest
          , shaderActions = case forestRoots forest of
              [root] -> [StoreOutput "position" root]
              _ -> []
          }
  compiled <- compileSuccessfully shader
  validateModule compiled
  disassembly <- disassembleModule compiled
  case disassembly of
    Nothing -> pure ()
    Just text ->
      length (filter (" Block" `isInfixOf`) (lines text)) @?= 1

fragDepthCase :: IO ()
fragDepthCase = do
  forest <- reifyExprForest [SomeExpr (constant 0.5 :: F Float)]
  let shader =
        ShaderModule
          { shaderCodegenConfig = defaultCodegenConfig
          , shaderStage = FragmentShader
          , shaderEntryPoint = "main"
          , shaderLocalSize = Nothing
          , shaderInputs = []
          , shaderOutputs = [StageOutput "depth" (ExpressionType TyFloat) (BuiltIn FragDepth)]
          , shaderResources = []
          , shaderForest = forest
          , shaderActions = case forestRoots forest of
              [root] -> [StoreOutput "depth" root]
              _ -> []
          }
  compiled <- compileSuccessfully shader
  validateModule compiled
  disassembly <- disassembleModule compiled
  case disassembly of
    Nothing -> pure ()
    Just text -> assertBool "FragDepth declares DepthReplacing" ("DepthReplacing" `isInfixOf` text)

globalInvocationIdCase :: IO ()
globalInvocationIdCase = do
  forest <- reifyExprForest []
  let compute builtInType stage localSize =
        ShaderModule
          { shaderCodegenConfig = defaultCodegenConfig
          , shaderStage = stage
          , shaderEntryPoint = "main"
          , shaderLocalSize = localSize
          , shaderInputs = [StageInput "globalId" builtInType (BuiltIn GlobalInvocationId)]
          , shaderOutputs = []
          , shaderResources = []
          , shaderForest = forest
          , shaderActions = []
          }
  compiled <- compileSuccessfully (compute (ExpressionType (TyWordVector 3)) ComputeShader (Just (LocalSize 1 1 1)))
  validateModule compiled
  case compileShaderModule (compute (ExpressionType TyInt) ComputeShader (Just (LocalSize 1 1 1))) of
    Left (InvalidBuiltIn _) -> pure ()
    result -> assertFailure ("expected invalid GlobalInvocationId type, got " ++ show result)
  case compileShaderModule (compute (ExpressionType (TyWordVector 3)) VertexShader Nothing) of
    Left (InvalidBuiltIn _) -> pure ()
    result -> assertFailure ("expected invalid GlobalInvocationId stage, got " ++ show result)

unsignedOperationsCase :: IO ()
unsignedOperationsCase = do
  let added = (constant 4 :: F Word32) + constant 5
      ordered = added <. constant 10
      equalVector = (constant (V3 1 2 3) :: F (V3 Word32)) ==. constant (V3 1 2 3)
      negated = negate (constant 1 :: F Word32) ==. constant maxBound
      absolute = abs (constant maxBound :: F Word32) ==. constant maxBound
      signed = signum (constant 9 :: F Word32) ==. constant 1
  compiled <- compileFragment [DiscardRoot ordered, DiscardRoot equalVector, DiscardRoot negated, DiscardRoot absolute, DiscardRoot signed] [] []
  validateModule compiled
  disassembly <- disassembleModule compiled
  case disassembly of
    Nothing -> pure ()
    Just text -> do
      assertBool "uint addition uses OpIAdd" ("OpIAdd" `isInfixOf` text)
      assertBool "uint ordering uses OpULessThan" ("OpULessThan" `isInfixOf` text)
      assertBool "uint vector equality uses OpIEqual" ("OpIEqual" `isInfixOf` text && "OpAll" `isInfixOf` text)

samplingCase :: IO ()
samplingCase = do
  let coordinates = constant (V2 0.25 0.75) :: F (V2 Float)
      combined = sampler2D "combined"
      separate = sampledImage2D (image2D "image") (sampler "sampler")
      outputs =
        [ OutputRoot "combinedImplicit" (TyVector 4) (sample combined coordinates)
        , OutputRoot "combinedExplicit" (TyVector 4) (sampleLod combined coordinates (constant 0))
        , OutputRoot "separateImplicit" (TyVector 4) (sample separate coordinates)
        , OutputRoot "separateExplicit" (TyVector 4) (sampleLod separate coordinates (constant 1))
        ]
      resources =
        [ CombinedImageSamplerResource (CombinedImageSamplerDeclaration "combined" "combined.image" "combined.sampler" (DescriptorLocation 0 0) Image2D)
        , SeparateImageResource (ImageDeclaration "image" "image" (DescriptorLocation 0 1) Image2D)
        , SeparateSamplerResource (SamplerDeclaration "sampler" "sampler" (DescriptorLocation 0 2))
        ]
  compiled <- compileFragment outputs [] resources
  validateModule compiled

pushConstantCase :: IO ()
pushConstantCase = do
  let color = input "push.color" :: F (V4 Float)
      resources =
        [ PushConstantResource
            ( PushConstantDeclaration
                "PushConstants"
                (Buffer.Struct [Buffer.Scalar Buffer.Float32, Buffer.Vector 4 Buffer.Float32])
                [ UniformLeaf "push.scalar" [0] TyFloat
                , UniformLeaf "push.color" [1] (TyVector 4)
                ]
            )
        ]
  compiled <- compileFragment [OutputRoot "color" (TyVector 4) color] [] resources
  validateModule compiled
  disassembly <- disassembleModule compiled
  case disassembly of
    Nothing -> pure ()
    Just text -> do
      assertBool "push block uses PushConstant storage" ("PushConstant" `isInfixOf` text)
      assertBool "std430 aligns the vector member to byte 16" ("Offset 16" `isInfixOf` text)

dimensionalSamplingCase :: IO ()
dimensionalSamplingCase = do
  let cubeImage = imageResource "cube.image" :: ImageResource 'Cube 'R8G8B8A8Unorm 'Fragment
      arrayImage = imageResource "array.image" :: ImageResource 'D2Array 'R8G8B8A8Unorm 'Fragment
      cubeSampled = sampledImage cubeImage (sampler "cube.sampler")
      arraySampled = sampledImage arrayImage (sampler "array.sampler")
      outputs =
        [ OutputRoot "cubeColor" (TyVector 4) (sample cubeSampled (constant (V3 1 0 0)))
        , OutputRoot "arrayColor" (TyVector 4) (sample arraySampled (constant (V3 0.25 0.75 2)))
        ]
      resources =
        [ SeparateImageResource (ImageDeclaration "cube" "cube.image" (DescriptorLocation 0 0) ImageCube)
        , SeparateSamplerResource (SamplerDeclaration "cubeSampler" "cube.sampler" (DescriptorLocation 0 1))
        , SeparateImageResource (ImageDeclaration "array" "array.image" (DescriptorLocation 0 2) Image2DArray)
        , SeparateSamplerResource (SamplerDeclaration "arraySampler" "array.sampler" (DescriptorLocation 0 3))
        ]
  compiled <- compileFragment outputs [] resources
  validateModule compiled

comparisonSamplingCase :: IO ()
comparisonSamplingCase = do
  let depthImage = imageResource "shadow.image" :: ImageResource 'D2 'D32Sfloat 'Fragment
      shadow = comparisonSampledImage depthImage (comparisonSampler "shadow.sampler")
      outputs =
        [ OutputRoot "implicitShadow" TyFloat (sampleCompare shadow (constant (V2 0.5 0.5)) (constant 0.25))
        , OutputRoot "explicitShadow" TyFloat (sampleCompareLod shadow (constant (V2 0.25 0.75)) (constant 0.5) (constant 0))
        ]
      resources =
        [ SeparateImageResource (ImageDeclaration "shadow" "shadow.image" (DescriptorLocation 0 0) Image2D)
        , SeparateSamplerResource (SamplerDeclaration "shadowSampler" "shadow.sampler" (DescriptorLocation 0 1))
        ]
  compiled <- compileFragment outputs [] resources
  validateModule compiled

debugCase :: IO ()
debugCase = do
  forest <- reifyExprForest [SomeExpr (constant (V4 1 1 1 1) :: F (V4 Float))]
  let withDebug = fragmentShaderFromForest defaultCodegenConfig [OutputSpec "color" (TyVector 4)] [] [] forest
      withoutDebug = withDebug{shaderCodegenConfig = CodegenConfig False}
  debugModule <- compileSuccessfully withDebug
  plainModule <- compileSuccessfully withoutDebug
  assertBool "debug names affect bytes" (moduleBytes debugModule /= moduleBytes plainModule)
  validateModule debugModule
  validateModule plainModule

preflightCase :: IO ()
preflightCase = do
  let missingNodeForest = ReifiedForest [NodeId 9] [] []
      shader = fragmentShaderFromForest defaultCodegenConfig [] [] [] missingNodeForest
  compileShaderModule shader @?= Left (MissingNode (NodeId 9))
  let recursiveNode = ReifiedNode (NodeId 0) TyFloat (RUnary NegateE (NodeId 0))
      recursiveForest = ReifiedForest [NodeId 0] [recursiveNode] []
      recursiveShader = fragmentShaderFromForest defaultCodegenConfig [OutputSpec "value" TyFloat] [] [] recursiveForest
  case compileShaderModule recursiveShader of
    Left (RecursiveGraph _) -> pure ()
    result -> assertFailure ("expected RecursiveGraph, got " ++ show result)

binderPreflightCase :: IO ()
binderPreflightCase = do
  let binder = BinderId 0
      escapedLocal = ReifiedNode (NodeId 0) TyFloat (RLocal binder)
      escapedForest = ReifiedForest [NodeId 0] [escapedLocal] [ReifiedRegion (RegionId 0) (Just binder) (NodeId 0)]
      escapedShader = fragmentShaderFromForest defaultCodegenConfig [OutputSpec "value" TyFloat] [] [] escapedForest
  compileShaderModule escapedShader @?= Left (InvalidLocalScope (NodeId 0) binder)

  let initial = ReifiedNode (NodeId 0) TyFloat (RLiteral (HFloat 0))
      predicate = ReifiedNode (NodeId 1) TyBool (RLiteral (HBool True))
      mistypedLocal = ReifiedNode (NodeId 2) TyInt (RLocal binder)
      loop = ReifiedNode (NodeId 3) TyFloat (RWhile (NodeId 0) binder (RegionId 0) (RegionId 1))
      mistypedForest =
        ReifiedForest
          [NodeId 3]
          [initial, predicate, mistypedLocal, loop]
          [ReifiedRegion (RegionId 0) (Just binder) (NodeId 1), ReifiedRegion (RegionId 1) (Just binder) (NodeId 2)]
      mistypedShader = fragmentShaderFromForest defaultCodegenConfig [OutputSpec "value" TyFloat] [] [] mistypedForest
  case compileShaderModule mistypedShader of
    Left (InvalidNodeType (NodeId 2) message) -> assertBool "type mismatch identifies the binder contract" ("expected TyFloat" `isInfixOf` message)
    result -> assertFailure ("expected local binder type mismatch, got " ++ show result)

  let step = ReifiedNode (NodeId 2) TyFloat (RLiteral (HFloat 1))
      firstLoop = ReifiedNode (NodeId 3) TyFloat (RWhile (NodeId 0) binder (RegionId 0) (RegionId 1))
      secondLoop = ReifiedNode (NodeId 4) TyFloat (RWhile (NodeId 0) binder (RegionId 2) (RegionId 3))
      duplicateForest =
        ReifiedForest
          [NodeId 3, NodeId 4]
          [initial, predicate, step, firstLoop, secondLoop]
          [ ReifiedRegion (RegionId 0) (Just binder) (NodeId 1)
          , ReifiedRegion (RegionId 1) (Just binder) (NodeId 2)
          , ReifiedRegion (RegionId 2) (Just binder) (NodeId 1)
          , ReifiedRegion (RegionId 3) (Just binder) (NodeId 2)
          ]
      duplicateShader = fragmentShaderFromForest defaultCodegenConfig [OutputSpec "first" TyFloat, OutputSpec "second" TyFloat] [] [] duplicateForest
  compileShaderModule duplicateShader @?= Left (DuplicateBinderOwner binder)

reifyCycleCase :: IO ()
reifyCycleCase = do
  let cyclic :: F Float
      cyclic = Expr (ExprObject TyFloat (UnaryNode NegateE (SomeExpr cyclic)))
  result <- try (reifyExpr cyclic) :: IO (Either ReifyError ReifiedExpr)
  result @?= Left ReifyCycle

finiteForestProperty :: Property
finiteForestProperty = forAllBlind finiteFloatTree $ \expression -> ioProperty $ do
  forest <- reifyExprForest [SomeExpr (expression :: F Float)]
  let shader = fragmentShaderFromForest defaultCodegenConfig [OutputSpec "value" TyFloat] [] [] forest
      first = compileShaderModule shader
      second = compileShaderModule shader
  case (first, second) of
    (Right firstModule, Right secondModule) -> do
      validation <- validateModuleResult firstModule
      pure (validation === Right () .&&. moduleBytes firstModule === moduleBytes secondModule)
    _ -> pure (property False)

finiteFloatTree :: Gen (F Float)
finiteFloatTree = sized build
 where
  build depth
    | depth <= 0 = constant <$> choose (-10, 10)
    | otherwise = frequency [(3, constant <$> choose (-10, 10)), (2, (+) <$> child <*> child), (2, (-) <$> child <*> child), (2, (*) <$> child <*> child), (1, abs <$> child)]
   where
    child = build (depth `div` 2)

structuredVectorProperty :: Property
structuredVectorProperty = forAllBlind structuredVectorTree $ \expression -> ioProperty $ do
  forest <- reifyExprForest [SomeExpr (expression :: F (V2 Float))]
  let shader = fragmentShaderFromForest defaultCodegenConfig [OutputSpec "value" (TyVector 2)] [] [] forest
      first = compileShaderModule shader
      second = compileShaderModule shader
  case (first, second) of
    (Right firstModule, Right secondModule) -> do
      validation <- validateModuleResult firstModule
      pure (validation === Right () .&&. moduleBytes firstModule === moduleBytes secondModule)
    _ -> pure (property False)

structuredVectorTree :: Gen (F (V2 Float))
structuredVectorTree = sized build
 where
  build depth
    | depth <= 0 = vectorConstant
    | otherwise =
        frequency
          [ (3, vectorConstant)
          , (2, (+) <$> child <*> child)
          , (2, (-) <$> child <*> child)
          , (2, ifThenElseE <$> ((==.) <$> child <*> child) <*> child <*> child)
          , (1, whileE (\value -> x value <. constant 4) (\value -> value + constant (V2 1 0)) <$> child)
          ]
   where
    child = build (depth `div` 2)
  vectorConstant = constant <$> (V2 <$> choose (-10, 10) <*> choose (-10, 10))

data OutputSpec = OutputSpec String ShaderTy

compileFragment :: [RootSpec] -> [StageInput] -> [ResourceDeclaration] -> IO SpirVModule
compileFragment roots inputs resources = do
  forest <- reifyExprForest (map rootExpression roots)
  let outputs = [OutputSpec name shaderType | OutputRoot name shaderType _ <- roots]
      shader = fragmentShaderFromForest defaultCodegenConfig outputs inputs resources forest
      actions = zipWith rootAction roots (forestRoots forest)
  compileSuccessfully shader{shaderActions = actions}

rootExpression :: RootSpec -> SomeExpr
rootExpression root = case root of
  OutputRoot _ _ expression -> SomeExpr expression
  DiscardRoot expression -> SomeExpr expression

rootAction :: RootSpec -> NodeId -> ShaderAction
rootAction root nodeId = case root of
  OutputRoot name _ _ -> StoreOutput name nodeId
  DiscardRoot _ -> DiscardWhen nodeId

fragmentShaderFromForest :: CodegenConfig -> [OutputSpec] -> [StageInput] -> [ResourceDeclaration] -> ReifiedForest -> ShaderModule
fragmentShaderFromForest config outputs inputs resources forest =
  ShaderModule
    { shaderCodegenConfig = config
    , shaderStage = FragmentShader
    , shaderEntryPoint = "main"
    , shaderLocalSize = Nothing
    , shaderInputs = inputs
    , shaderOutputs = zipWith outputDeclaration [0 ..] outputs
    , shaderResources = resources
    , shaderForest = forest
    , shaderActions = zipWith (\(OutputSpec name _) root -> StoreOutput name root) outputs (forestRoots forest)
    }
 where
  outputDeclaration location (OutputSpec name shaderType) = StageOutput name (ExpressionType shaderType) (Location location Smooth)

compileSuccessfully :: ShaderModule -> IO SpirVModule
compileSuccessfully shader = case compileShaderModule shader of
  Left codegenError -> assertFailure ("codegen failed: " ++ show codegenError) >> fail "unreachable"
  Right spirVModule -> pure spirVModule

validateModule :: SpirVModule -> IO ()
validateModule spirVModule = do
  result <- validateModuleResult spirVModule
  case result of
    Left message -> assertFailure message
    Right () -> pure ()

validateModuleResult :: SpirVModule -> IO (Either String ())
validateModuleResult spirVModule = do
  validator <- findExecutable "spirv-val"
  case validator of
    Nothing -> pure (Right ())
    Just executable -> withModuleFile spirVModule $ \path -> do
      (exitCode, standardOutput, standardError) <- readProcessWithExitCode executable ["--target-env", "vulkan1.3", path] ""
      pure $ case exitCode of
        ExitSuccess -> Right ()
        ExitFailure code -> Left ("spirv-val exited with " ++ show code ++ "\n" ++ standardOutput ++ standardError)

disassembleModule :: SpirVModule -> IO (Maybe String)
disassembleModule spirVModule = do
  disassembler <- findExecutable "spirv-dis"
  case disassembler of
    Nothing -> pure Nothing
    Just executable -> withModuleFile spirVModule $ \path -> do
      (exitCode, standardOutput, _) <- readProcessWithExitCode executable [path] ""
      pure $ case exitCode of
        ExitSuccess -> Just standardOutput
        ExitFailure _ -> Nothing

withModuleFile :: SpirVModule -> (FilePath -> IO a) -> IO a
withModuleFile spirVModule action = do
  temporaryDirectory <- getTemporaryDirectory
  bracket
    (openBinaryTempFile temporaryDirectory "vpipe-codegen.spv")
    ( \(path, handle) -> do
        hClose handle `catchIOError` const (pure ())
        removeFile path `catchIOError` const (pure ())
    )
    ( \(path, handle) -> do
        BL.hPut handle (moduleBytes spirVModule)
        hClose handle
        action path
    )
