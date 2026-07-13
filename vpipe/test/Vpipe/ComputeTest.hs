{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Vpipe.ComputeTest (computePureTests, computeTests) where

import Control.Exception (bracket, throwIO, try)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.List (isInfixOf)
import Data.Word (Word32)
import Linear (V4 (..))
import System.Directory (findExecutable, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose, openBinaryTempFile)
import System.IO.Error (catchIOError)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Vpipe.Buffer (Buffer, newBuffer, readBuffer, writeBuffer)
import Vpipe.Buffer qualified as TypedBuffer
import Vpipe.Buffer.Format qualified as Buffer
import Vpipe.Compute.Internal
import Vpipe.Context (Context, VpipeConfig (vpipeLogger, vpipeValidationStrict), defaultVpipeConfig, withVpipe)
import Vpipe.Diagnostics.Dump.Internal (ShaderDump (..), ShaderDumpStage (DumpCompute), throwShaderDriverFailureWith)
import Vpipe.Error (VpipeError (ContextClosed, DeviceLost, NoVulkanIcd, ShaderCompileBug, VulkanFailure))
import Vpipe.Expr (constant, (<.))
import Vpipe.Expr qualified as ExprDsl
import Vpipe.Expr.Internal qualified as Expr
import Vpipe.Expr.Reify (NodeId (..), ReifiedForest (..), ReifiedNode (..), ReifiedOp (..), reifiedOp)
import Vpipe.Format (Format (R8G8B8A8Unorm))
import Vpipe.Graphics (newGraphicsRuntime, prepareGraphicsPipeline, renderGraphicsPipeline)
import Vpipe.Image (Image, ImageSubresource (..), imageExtent2D, newImage, readImage)
import Vpipe.Image.Types (Dim (D2))
import Vpipe.Image.Types qualified as ImageTypes
import Vpipe.Pipeline.Internal (PipelineInterface (..), ResolvedBindingPlan (..), ResourceBinding (..), RuntimeHandle (..), ShaderResourceShape (..), StorageAccess (..), StorageBuffer (..), renderPipelineInterfaceTable, storageBufferBinding)
import Vpipe.Pipeline.Internal qualified as GraphicsPipeline
import Vpipe.SpirV.Assembler (SpirVModule, moduleBytes)
import Vpipe.SpirV.Codegen qualified as Codegen
import Vulkan.Core10.Enums.Result qualified as Result

data Environment = Environment
  { floatBuffer :: StorageBuffer Float
  , intBuffer :: StorageBuffer Int32
  , wordBuffer :: StorageBuffer Word32
  , delta :: Float
  }

environment :: Environment
environment = Environment (StorageBuffer (RuntimeHandle 1)) (StorageBuffer (RuntimeHandle 2)) (StorageBuffer (RuntimeHandle 3)) 0.5

computeTests :: TestTree
computeTests =
  testGroup
    "compute"
    (computePureCases <> computeDeviceCases)

computePureTests :: TestTree
computePureTests = testGroup "compute" computePureCases

computePureCases :: [TestTree]
computePureCases =
  [ testCase "checks workgroup count arithmetic" workgroupCountsCase
  , testCase "compiles storage read/write/length and consumes GlobalInvocationId" storageCase
  , testCase "emits signed and unsigned atomic adds" atomicCase
  , testCase "does not CSE a storage load across action boundaries" actionBoundaryCase
  , testCase "preserves top-level and conditional action order" actionOrderingCase
  , testCase "array length alone does not infer a data read" lengthAccessCase
  , testCase "resolves storage and push-constant plans" resolutionCase
  , testCase "rejects unsupported and mismatched storage layouts" storageLayoutCase
  , testCase "rejects reordered action roots even when counts match" rootOrderCase
  , testCase "maps post-codegen compute shader failures without a device" computeDriverFailureCase
  ]

computeDeviceCases :: [TestTree]
computeDeviceCases =
  [ testCase "dispatches SAXPY over runtime storage arrays" saxpyRuntimeCase
  , testCase "dispatches atomic additions" atomicRuntimeCase
  , testCase "shares exact compute pipeline and shader caches" computeCacheCase
  , testCase "validates limits and preserves a prepared pipeline after rejected work" runtimeValidationCase
  , testCase "zero-count dispatch rejects a closed retained context" retainedPreparedClosureCase
  , testCase "hands a storage-written vertex buffer to graphics" graphicsInteropCase
  ]

workgroupCountsCase :: IO ()
workgroupCountsCase = do
  workgroupCounts (Dispatch @64 @2 @1) (129, 3, 0) @?= Right (3, 2, 0)
  case workgroupCounts (Dispatch @64 @1 @1) (-1, 1, 1) of
    Left (InvalidWorkload _) -> pure ()
    result -> assertFailure ("expected invalid workload, got " <> show result)
  case workgroupCounts (Dispatch @0 @1 @1) (1, 1, 1) of
    Left (InvalidDispatch _) -> pure ()
    result -> assertFailure ("expected invalid zero dispatch, got " <> show result)
  case workgroupCounts (Dispatch @4294967296 @1 @1) (1, 1, 1) of
    Left (InvalidDispatch _) -> pure ()
    result -> assertFailure ("expected oversized dispatch rejection, got " <> show result)

storageCase :: IO ()
storageCase = do
  compiled <- compileSuccessfully $ do
    buffer <- storageBuffer floatBuffer
    invocation <- globalInvocationId
    let index = globalInvocationX invocation
    whenInBounds buffer index $ \value ->
      writeAt buffer index (value + constant 1)
  validateModule (compiledComputeModule compiled)
  case pipelineResources (compiledComputeInterface compiled) of
    [ResourceBinding _ 0 0 (StorageArrayShape _ _ StorageReadWrite)] -> pure ()
    resources -> assertFailure ("unexpected storage reflection: " <> show resources)

atomicCase :: IO ()
atomicCase = do
  compiled <- compileSuccessfully $ do
    signed <- storageBuffer intBuffer
    unsigned <- storageBuffer wordBuffer
    atomicAdd signed (constant 0) (constant 1)
    atomicAdd unsigned (constant 0) (constant 1)
  validateModule (compiledComputeModule compiled)

actionBoundaryCase :: IO ()
actionBoundaryCase = do
  compiled <- compileSuccessfully $ do
    buffer <- storageBuffer floatBuffer
    let loaded = readAt buffer (constant 0)
    writeAt buffer (constant 0) loaded
    writeAt buffer (constant 1) loaded
  let loadCount = length [() | node <- forestNodes (compiledComputeForest compiled), RStorageRead{} <- [reifiedOp node]]
  loadCount @?= 2

actionOrderingCase :: IO ()
actionOrderingCase = do
  compiled <- compileSuccessfully $ do
    buffer <- storageBuffer wordBuffer
    writeAt buffer (constant 0) (constant 1)
    whenC (constant True) $
      writeAt buffer (constant 1) (constant 2)
    atomicAdd buffer (constant 2) (constant 1)
  case compiledComputeActions compiled of
    [Codegen.StoreStorage{}, Codegen.ComputeWhen _ [Codegen.StoreStorage{}], Codegen.AtomicAddStorage{}] -> pure ()
    actions -> assertFailure ("unexpected action order: " <> show actions)

lengthAccessCase :: IO ()
lengthAccessCase = do
  compiled <- compileSuccessfully $ do
    buffer <- storageBuffer floatBuffer
    whenC (constant 0 <. bufferLength buffer) $
      writeAt buffer (constant 0) (constant 1)
  validateModule (compiledComputeModule compiled)
  case pipelineResources (compiledComputeInterface compiled) of
    [ResourceBinding _ 0 0 (StorageArrayShape _ _ StorageWriteOnly)] -> pure ()
    resources -> assertFailure ("length inferred a data read: " <> show resources)

resolutionCase :: IO ()
resolutionCase = do
  compiled <- compileSuccessfully $ do
    buffer <- storageBuffer floatBuffer
    value <- pushConstant delta
    writeAt buffer (constant 0) value
  bindings <- either (assertFailure . show) pure (resolveComputeBindings compiled environment)
  length (resolvedStorageBuffers bindings) @?= 1
  pushes <- resolveComputePushConstants compiled environment
  length pushes @?= 1

storageLayoutCase :: IO ()
storageLayoutCase = do
  rejects (Codegen.StorageBufferDeclaration "bad-v3" (Codegen.DescriptorLocation 0 0) (Expr.TyVector 3) (Buffer.Vector 3 Buffer.Float32) Codegen.StorageReadOnly)
  rejects (Codegen.StorageBufferDeclaration "bad-word" (Codegen.DescriptorLocation 0 0) Expr.TyWord (Buffer.Scalar Buffer.SignedInt32) Codegen.StorageReadOnly)
 where
  rejects declaration =
    case Codegen.compileShaderModule (shader declaration) of
      Left (Codegen.InvalidUniformLayout _) -> pure ()
      result -> assertFailure ("expected invalid storage layout, got " <> show result)
  shader declaration =
    Codegen.ShaderModule
      { Codegen.shaderCodegenConfig = Codegen.defaultCodegenConfig
      , Codegen.shaderStage = Codegen.ComputeShader
      , Codegen.shaderEntryPoint = "main"
      , Codegen.shaderLocalSize = Just (Codegen.LocalSize 1 1 1)
      , Codegen.shaderInputs = []
      , Codegen.shaderOutputs = []
      , Codegen.shaderResources = [Codegen.StorageBufferResource declaration]
      , Codegen.shaderForest = ReifiedForest [] [] []
      , Codegen.shaderActions = []
      }

rootOrderCase :: IO ()
rootOrderCase =
  case Codegen.compileShaderModule shader of
    Left (Codegen.InvalidStage message)
      | "root count/order mismatch" `isInfixOf` message -> pure ()
    result -> assertFailure ("expected root-order rejection, got " <> show result)
 where
  first = ReifiedNode (NodeId 0) Expr.TyBool (RLiteral (Expr.HBool True))
  second = ReifiedNode (NodeId 1) Expr.TyBool (RLiteral (Expr.HBool False))
  shader =
    Codegen.ShaderModule
      { Codegen.shaderCodegenConfig = Codegen.defaultCodegenConfig
      , Codegen.shaderStage = Codegen.FragmentShader
      , Codegen.shaderEntryPoint = "main"
      , Codegen.shaderLocalSize = Nothing
      , Codegen.shaderInputs = []
      , Codegen.shaderOutputs = []
      , Codegen.shaderResources = []
      , Codegen.shaderForest = ReifiedForest [NodeId 0, NodeId 1] [first, second] []
      , Codegen.shaderActions = [Codegen.DiscardWhen (NodeId 1), Codegen.DiscardWhen (NodeId 0)]
      }

computeDriverFailureCase :: IO ()
computeDriverFailureCase = do
  compiled <- compileSuccessfully $ do
    buffer <- storageBuffer wordBuffer
    writeAt buffer (constant 0) (constant 1)
  retained <- newIORef Nothing
  let dump =
        ShaderDump
          { shaderDumpName = "compute-64x1x1"
          , shaderDumpStage = DumpCompute
          , shaderDumpModule = compiledComputeModule compiled
          , shaderDumpInterface = renderPipelineInterfaceTable (compiledComputeInterface compiled)
          }
      artifact = "/tmp/injected-compute.spv"
      retain = writeIORef retained (Just (shaderDumpModule dump)) >> pure artifact
      throwFailure operation result =
        throwShaderDriverFailureWith retain operation (show result) result
  shaderModuleFailure <- try (throwFailure "vkCreateShaderModule" Result.ERROR_INVALID_SHADER_NV) :: IO (Either VpipeError ())
  case shaderModuleFailure of
    Left (ShaderCompileBug _ path) -> path @?= artifact
    unexpected -> assertFailure ("expected ShaderCompileBug, received " <> show unexpected)
  readIORef retained >>= (@?= Just (compiledComputeModule compiled))
  writeIORef retained Nothing
  pipelineFailure <- try (throwFailure "vkCreateComputePipelines" Result.ERROR_INVALID_SHADER_NV) :: IO (Either VpipeError ())
  case pipelineFailure of
    Left (ShaderCompileBug _ path) -> path @?= artifact
    unexpected -> assertFailure ("expected ShaderCompileBug, received " <> show unexpected)
  readIORef retained >>= (@?= Just (compiledComputeModule compiled))
  writeIORef retained Nothing
  deviceLost <- try (throwFailure "vkCreateComputePipelines" Result.ERROR_DEVICE_LOST) :: IO (Either VpipeError ())
  case deviceLost of
    Left DeviceLost -> pure ()
    unexpected -> assertFailure ("expected DeviceLost, received " <> show unexpected)
  readIORef retained >>= (@?= Nothing)
  other <- try (throwFailure "vkCreateComputePipelines" Result.ERROR_OUT_OF_DEVICE_MEMORY) :: IO (Either VpipeError ())
  case other of
    Left (VulkanFailure "vkCreateComputePipelines" _) -> pure ()
    unexpected -> assertFailure ("expected VulkanFailure, received " <> show unexpected)

data SaxpyEnvironment = SaxpyEnvironment
  { saxpyInputX :: StorageBuffer Float
  , saxpyInputY :: StorageBuffer Float
  , saxpyOutput :: StorageBuffer Float
  , saxpyScale :: Float
  }

saxpyProgram :: ComputeM SaxpyEnvironment ()
saxpyProgram = do
  inputX <- storageBuffer saxpyInputX
  inputY <- storageBuffer saxpyInputY
  output <- storageBuffer saxpyOutput
  scale <- pushConstant saxpyScale
  invocation <- globalInvocationId
  let index = globalInvocationX invocation
  whenC (index <. bufferLength output) $
    writeAt output index (scale * readAt inputX index + readAt inputY index)

saxpyRuntimeCase :: IO ()
saxpyRuntimeCase = withTestContext $ \context -> do
  compiledResult <- compileCompute (Dispatch @64 @1 @1) saxpyProgram
  compiled <- either (assertFailure . show) pure compiledResult
  runtime <- newComputeRuntime context
  prepared <- prepareComputePipeline runtime compiled
  let elementCount = 130
      xValues = fmap fromIntegral [0 .. elementCount - 1]
      yValues = fmap (fromIntegral . (* 3)) [0 .. elementCount - 1]
      expected = zipWith (\x y -> 2 * x + y) xValues yValues
  inputX <- newBuffer context elementCount :: IO (Buffer '[ 'TypedBuffer.Storage] Float)
  inputY <- newBuffer context elementCount :: IO (Buffer '[ 'TypedBuffer.Storage] Float)
  output <- newBuffer context elementCount :: IO (Buffer '[ 'TypedBuffer.Storage, 'TypedBuffer.CopySrc] Float)
  writeBuffer inputX 0 xValues
  writeBuffer inputY 0 yValues
  writeBuffer output 0 (replicate elementCount 0)
  let environment' =
        SaxpyEnvironment
          { saxpyInputX = storageBufferBinding inputX
          , saxpyInputY = storageBufferBinding inputY
          , saxpyOutput = storageBufferBinding output
          , saxpyScale = 2
          }
  dispatchFor prepared environment' (toInteger elementCount, 1, 1)
  readBuffer output 0 elementCount >>= (@?= expected)

newtype AtomicEnvironment = AtomicEnvironment
  { atomicOutput :: StorageBuffer Word32
  }

atomicRuntimeProgram :: ComputeM AtomicEnvironment ()
atomicRuntimeProgram = do
  output <- storageBuffer atomicOutput
  atomicAdd output (constant 0) (constant 1)

atomicRuntimeCase :: IO ()
atomicRuntimeCase = withTestContext $ \context -> do
  compiledResult <- compileCompute (Dispatch @64 @1 @1) atomicRuntimeProgram
  compiled <- either (assertFailure . show) pure compiledResult
  runtime <- newComputeRuntime context
  prepared <- prepareComputePipeline runtime compiled
  output <- newBuffer context 1 :: IO (Buffer '[ 'TypedBuffer.Storage, 'TypedBuffer.CopySrc] Word32)
  writeBuffer output 0 [0]
  dispatch prepared (AtomicEnvironment (storageBufferBinding output)) (4, 1, 1)
  readBuffer output 0 1 >>= (@?= [256])

computeCacheCase :: IO ()
computeCacheCase = withTestContext $ \context -> do
  compiledResult <- compileCompute (Dispatch @64 @1 @1) saxpyProgram
  compiled <- either (assertFailure . show) pure compiledResult
  firstRuntime <- newComputeRuntime context
  secondRuntime <- newComputeRuntime context
  _ <- prepareComputePipeline firstRuntime compiled
  _ <- prepareComputePipeline secondRuntime compiled
  computeStats firstRuntime >>= (@?= ComputeStats 1 1)
  computeStats secondRuntime >>= (@?= ComputeStats 1 1)

newtype ValidationEnvironment = ValidationEnvironment
  { validationOutput :: StorageBuffer Word32
  }

validationProgram :: ComputeM ValidationEnvironment ()
validationProgram = do
  output <- storageBuffer validationOutput
  invocation <- globalInvocationId
  let index = globalInvocationX invocation
  whenC (index <. bufferLength output) $
    writeAt output index (constant 77)

runtimeValidationCase :: IO ()
runtimeValidationCase = withTestContext $ \context -> do
  oversizedResult <- compileCompute (Dispatch @65536 @1 @1) validationProgram
  oversized <- either (assertFailure . show) pure oversizedResult
  validResult <- compileCompute (Dispatch @64 @1 @1) validationProgram
  valid <- either (assertFailure . show) pure validResult
  runtime <- newComputeRuntime context
  oversizedPreparation <-
    try (prepareComputePipeline runtime oversized) ::
      IO (Either VpipeError (PreparedCompute ValidationEnvironment 65536 1 1))
  case oversizedPreparation of
    Left VulkanFailure{} -> pure ()
    unexpected -> assertFailure ("expected local-size rejection, got " <> showPrepared unexpected)
  prepared <- prepareComputePipeline runtime valid
  let forged = ValidationEnvironment (StorageBuffer (RuntimeHandle 999))
  dispatch prepared forged (0, 1, 1)
  forgedDispatch <- try (dispatch prepared forged (1, 1, 1)) :: IO (Either VpipeError ())
  case forgedDispatch of
    Left VulkanFailure{} -> pure ()
    unexpected -> assertFailure ("expected unmanaged-buffer rejection, got " <> show unexpected)
  output <- newBuffer context 1 :: IO (Buffer '[ 'TypedBuffer.Storage, 'TypedBuffer.CopySrc] Word32)
  writeBuffer output 0 [0]
  let environment' = ValidationEnvironment (storageBufferBinding output)
  oversizedDispatch <-
    try (dispatch prepared environment' (fromIntegral (maxBound :: Word32), 1, 1)) ::
      IO (Either VpipeError ())
  case oversizedDispatch of
    Left VulkanFailure{} -> pure ()
    unexpected -> assertFailure ("expected workgroup-count rejection, got " <> show unexpected)
  dispatchFor prepared environment' (1, 1, 1)
  readBuffer output 0 1 >>= (@?= [77])
 where
  showPrepared result = case result of
    Left error' -> show error'
    Right _ -> "successful preparation"

retainedPreparedClosureCase :: IO ()
retainedPreparedClosureCase = do
  compiledResult <- compileCompute (Dispatch @64 @1 @1) validationProgram
  compiled <- either (assertFailure . show) pure compiledResult
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  let config =
        defaultVpipeConfig
          { vpipeValidationStrict = requested == Just "lavapipe"
          , vpipeLogger = print
          }
  retainedResult <-
    try
      ( withVpipe config $ \context -> do
          runtime <- newComputeRuntime context
          prepareComputePipeline runtime compiled
      ) ::
      IO
        ( Either
            VpipeError
            (PreparedCompute ValidationEnvironment 64 1 1)
        )
  retained <- case retainedResult of
    Left (NoVulkanIcd detail)
      | requested /= Just "lavapipe" ->
          assertFailure ("Vulkan ICD unavailable: " <> detail)
    Left error' -> throwIO error'
    Right prepared -> pure prepared
  let forged = ValidationEnvironment (StorageBuffer (RuntimeHandle 999))
  closedDispatch <- try (dispatch retained forged (0, 1, 1)) :: IO (Either VpipeError ())
  case closedDispatch of
    Left ContextClosed -> pure ()
    unexpected -> assertFailure ("expected ContextClosed, got " <> show unexpected)
  closedDispatchFor <- try (dispatchFor retained forged (0, 1, 1)) :: IO (Either VpipeError ())
  case closedDispatchFor of
    Left ContextClosed -> pure ()
    unexpected -> assertFailure ("expected ContextClosed, got " <> show unexpected)

data InteropEnvironment = InteropEnvironment
  { interopStorage :: StorageBuffer (V4 Float)
  , interopVertices :: GraphicsPipeline.VertexBuffer (V4 Float)
  , interopTarget :: GraphicsPipeline.ColorImage 'R8G8B8A8Unorm
  }

interopComputeProgram :: ComputeM InteropEnvironment ()
interopComputeProgram = do
  positions <- storageBuffer interopStorage
  invocation <- globalInvocationId
  let index = globalInvocationX invocation
  whenInBounds positions index $ \position ->
    writeAt positions index position

interopGraphicsProgram :: GraphicsPipeline.PipelineM InteropEnvironment ()
interopGraphicsProgram = do
  positions <-
    GraphicsPipeline.vertexInput
      ( GraphicsPipeline.vertexSource "positions" interopVertices ::
          GraphicsPipeline.VertexSource InteropEnvironment 'GraphicsPipeline.Triangles (V4 Float)
      )
  fragments <-
    GraphicsPipeline.rasterize
      GraphicsPipeline.defaultRaster
      (fmap (,GraphicsPipeline.Smooth (ExprDsl.constant (0 :: Float) :: ExprDsl.V Float)) positions)
  GraphicsPipeline.drawColor
    GraphicsPipeline.defaultBlend
    (GraphicsPipeline.colorTarget "color" interopTarget)
    ( fmap
        ( const
            ( ExprDsl.vec4
                (ExprDsl.constant 1)
                (ExprDsl.constant 0)
                (ExprDsl.constant 0)
                (ExprDsl.constant 1)
            )
        )
        fragments
    )

graphicsInteropCase :: IO ()
graphicsInteropCase = withTestContext $ \context -> do
  computeResult <- compileCompute (Dispatch @64 @1 @1) interopComputeProgram
  compiledCompute <- either (assertFailure . show) pure computeResult
  graphicsResult <- GraphicsPipeline.compilePipeline interopGraphicsProgram
  compiledGraphics <- either (assertFailure . show) pure graphicsResult
  computeRuntime <- newComputeRuntime context
  preparedCompute <- prepareComputePipeline computeRuntime compiledCompute
  graphicsRuntime <- newGraphicsRuntime context
  preparedGraphics <- prepareGraphicsPipeline graphicsRuntime compiledGraphics
  positions <-
    newBuffer context 3 ::
      IO
        ( Buffer
            '[ 'TypedBuffer.Storage, 'TypedBuffer.Vertex, 'TypedBuffer.CopySrc]
            (V4 Float)
        )
  writeBuffer
    positions
    0
    [ V4 (-0.8) (-0.8) 0 1
    , V4 0.8 (-0.8) 0 1
    , V4 0 0.8 0 1
    ]
  target <-
    newImage context (imageExtent2D 16 16) 1 1 ::
      IO
        ( Image
            'D2
            'R8G8B8A8Unorm
            '[ 'ImageTypes.ColorTarget, 'ImageTypes.CopySrc]
        )
  targetBinding <- GraphicsPipeline.colorImageBinding target
  let environment' =
        InteropEnvironment
          { interopStorage = storageBufferBinding positions
          , interopVertices = GraphicsPipeline.vertexBufferBinding positions
          , interopTarget = targetBinding
          }
  dispatchFor preparedCompute environment' (3, 1, 1)
  renderGraphicsPipeline preparedGraphics environment'
  pixels <- readImage target (ImageSubresource 0 0)
  pixelAt 16 8 8 pixels @?= V4 255 0 0 255

pixelAt :: Int -> Int -> Int -> [a] -> a
pixelAt width column row pixels = pixels !! (row * width + column)

withTestContext :: (Context -> IO a) -> IO a
withTestContext action = do
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  let config =
        defaultVpipeConfig
          { vpipeValidationStrict = requested == Just "lavapipe"
          , vpipeLogger = print
          }
  result <- try (withVpipe config action)
  case result of
    Left (NoVulkanIcd detail)
      | requested /= Just "lavapipe" ->
          assertFailure ("Vulkan ICD unavailable: " <> detail)
    Left error' -> throwIO (error' :: VpipeError)
    Right value -> pure value

compileSuccessfully :: ComputeM Environment () -> IO (CompiledCompute Environment 64 1 1)
compileSuccessfully program = do
  result <- compileCompute (Dispatch @64 @1 @1) program
  either (assertFailure . show) pure result

validateModule :: SpirVModule -> IO ()
validateModule spirVModule = do
  validator <- findExecutable "spirv-val"
  case validator of
    Nothing -> pure ()
    Just executable ->
      withModuleFile spirVModule $ \path -> do
        (exitCode, standardOutput, standardError) <- readProcessWithExitCode executable ["--target-env", "vulkan1.3", path] ""
        case exitCode of
          ExitSuccess -> pure ()
          ExitFailure code -> assertFailure ("spirv-val exited with " <> show code <> "\n" <> standardOutput <> standardError)

withModuleFile :: SpirVModule -> (FilePath -> IO a) -> IO a
withModuleFile spirVModule action = do
  temporary <- getTemporaryDirectory
  bracket
    ( do
        (path, handle) <- openBinaryTempFile temporary "vpipe-compute.spv"
        BL.hPut handle (moduleBytes spirVModule)
        hClose handle
        pure path
    )
    (\path -> removeFile path `catchIOError` const (pure ()))
    action
