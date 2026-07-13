{-# LANGUAGE OverloadedStrings #-}

module Vpipe.DiagnosticsTest (diagnosticsTests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (replicateM, void)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (traverse_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import System.Directory (createDirectory, doesFileExist, doesPathExist, getTemporaryDirectory, listDirectory, removeFile, removePathForcibly)
import System.FilePath (takeExtension, (</>))
import System.IO (hClose, openBinaryTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Vulkan.Core10.Enums.Result qualified as Result

import Vpipe.Buffer.Format (FieldLayout (Scalar), ScalarType (UnsignedInt32))
import Vpipe.Diagnostics.Dump.Internal (ShaderDump (..), ShaderDumpStage (DumpCompute), ShaderFailureKind (..), classifyShaderFailure, dumpCompiledModuleWith, retainShaderFailureArtifactWith, throwShaderCompileBugWith, throwShaderDriverFailureWith)
import Vpipe.Error (VpipeError (DeviceLost, ShaderCompileBug, VulkanFailure))
import Vpipe.Expr.Internal (ShaderTy (TyWord))
import Vpipe.Pipeline.Internal (PipelineInterface (..), PushConstantRange (..), ResourceBinding (..), ShaderResourceShape (StorageArrayShape), StorageAccess (StorageReadWrite), renderPipelineInterfaceTable)
import Vpipe.SpirV (SpirVModule, computeModule, moduleBytes)

diagnosticsTests :: TestTree
diagnosticsTests =
  testGroup
    "compiler diagnostics"
    [ testCase "an unset dump destination performs no tool or filesystem work" unsetCase
    , testCase "set dumping is deterministic and writes a readable interface" deterministicCase
    , testCase "sanitized name collisions remain distinct" sanitizedCollisionCase
    , testCase "concurrent identical dumps leave complete files" concurrentCase
    , testCase "dump failures remain best-effort" bestEffortCase
    , testCase "failure artifacts return an existing binary or disassembly path" failureArtifactCase
    , testCase "internal shader rejection retains the artifact named by ShaderCompileBug" shaderCompileBugCase
    , testCase "only explicit generated-shader rejection is a shader compile bug" shaderFailureClassificationCase
    , testCase "shader driver failures retain artifacts only for generated-shader rejection" shaderDriverFailureCase
    ]

unsetCase :: IO ()
unsetCase = withTemporaryDirectory $ \parent -> do
  spirV <- sampleModule
  finderCalled <- newIORef False
  let destination = parent </> "must-not-exist"
      finder = writeIORef finderCalled True >> pure Nothing
  _ <- dumpCompiledModuleWith (pure Nothing) finder (sampleDump spirV "unsafe/name")
  doesPathExist destination >>= (@?= False)
  readIORef finderCalled >>= (@?= False)

deterministicCase :: IO ()
deterministicCase = withTemporaryDirectory $ \directory -> do
  spirV <- sampleModule
  let request = sampleDump spirV "unsafe/name"
      dump = dumpCompiledModuleWith (pure (Just directory)) (pure Nothing) request
  firstPath <- dump
  firstFiles <- sort <$> listDirectory directory
  secondPath <- dump
  secondFiles <- sort <$> listDirectory directory
  firstPath @?= secondPath
  secondFiles @?= firstFiles
  length firstFiles @?= 2
  assertBool "sanitized stable prefix" (all ("unsafe-name.compute." `isPrefixOf`) firstFiles)
  assertBool "no disassembly without spirv-dis" (not (any (".spvasm" `isSuffixOf`) firstFiles))
  spirVPath <- exactlyOne ".spv" firstFiles
  firstPath @?= Just (directory </> spirVPath)
  interfacePath <- exactlyOne ".interface.txt" firstFiles
  actualSpirV <- ByteString.readFile (directory </> spirVPath)
  actualSpirV @?= LazyByteString.toStrict (moduleBytes spirV)
  interface <- readFile (directory </> interfacePath)
  assertBool "interface title" ("vpipe shader interface" `isInfixOf` interface)
  assertBool "interface stage" ("stage: compute" `isInfixOf` interface)
  assertBool "resource table" ("resources:" `isInfixOf` interface && "StorageArrayShape" `isInfixOf` interface)
  assertBool "push-constant table" ("push constants:" `isInfixOf` interface && "scale" `isInfixOf` interface)

sanitizedCollisionCase :: IO ()
sanitizedCollisionCase = withTemporaryDirectory $ \directory -> do
  spirV <- sampleModule
  let dump name =
        dumpCompiledModuleWith
          (pure (Just directory))
          (pure Nothing)
          (sampleDump spirV name)
  _ <- dump "unsafe/name"
  _ <- dump "unsafe?name"
  files <- listDirectory directory
  length files @?= 4
  let spirVFiles = filter (".spv" `isSuffixOf`) files
  length spirVFiles @?= 2
  assertBool "both names sanitize to the same readable prefix" (all ("unsafe-name.compute." `isPrefixOf`) spirVFiles)

concurrentCase :: IO ()
concurrentCase = withTemporaryDirectory $ \directory -> do
  spirV <- sampleModule
  let request = sampleDump spirV "concurrent"
      action = void (dumpCompiledModuleWith (pure (Just directory)) (pure Nothing) request)
  completions <- replicateM 16 newEmptyMVar
  traverse_
    (\completion -> forkIO (try action >>= putMVar completion))
    completions
  results <- traverse takeMVar completions
  traverse_ (either throwIO pure) (results :: [Either SomeException ()])
  files <- listDirectory directory
  length files @?= 2
  spirVPath <- exactlyOne ".spv" files
  actual <- ByteString.readFile (directory </> spirVPath)
  actual @?= LazyByteString.toStrict (moduleBytes spirV)
  assertBool "temporary files were retired" (not (any (".tmp" `isInfixOf`) files))

bestEffortCase :: IO ()
bestEffortCase = withTemporaryDirectory $ \parent -> do
  spirV <- sampleModule
  let notDirectory = parent </> "plain-file"
  ByteString.writeFile notDirectory "occupied"
  void $
    dumpCompiledModuleWith
      (pure (Just notDirectory))
      (assertFailure "tool discovery must not run after directory creation fails")
      (sampleDump spirV "failure")

failureArtifactCase :: IO ()
failureArtifactCase = withTemporaryDirectory $ \directory -> do
  spirV <- sampleModule
  let request = sampleDump spirV "driver-rejection"
      retain disassemble =
        retainShaderFailureArtifactWith
          (pure Nothing)
          (pure directory)
          disassemble
          request
  binaryPath <- retain (\_ _ -> pure False)
  takeExtension binaryPath @?= ".spv"
  doesFileExist binaryPath >>= (@?= True)
  ByteString.readFile binaryPath >>= (@?= LazyByteString.toStrict (moduleBytes spirV))
  assemblyPath <-
    retain $ \_ destination -> do
      ByteString.writeFile destination "disassembly"
      pure True
  takeExtension assemblyPath @?= ".spvasm"
  doesFileExist assemblyPath >>= (@?= True)
  let unusableDirectory = directory </> "plain-file"
  ByteString.writeFile unusableDirectory "occupied"
  fallbackPath <-
    retainShaderFailureArtifactWith
      (pure (Just unusableDirectory))
      (pure directory)
      (\_ _ -> pure False)
      request
  doesFileExist fallbackPath >>= (@?= True)
  assertBool "unusable configured directory falls back below the temporary root" ((directory </> "vpipe-shader-failures") `isPrefixOf` fallbackPath)

shaderCompileBugCase :: IO ()
shaderCompileBugCase = withTemporaryDirectory $ \directory -> do
  spirV <- sampleModule
  let retain =
        retainShaderFailureArtifactWith
          (pure Nothing)
          (pure directory)
          (\_ _ -> pure False)
          (sampleDump spirV "rejected")
  result <- try (throwShaderCompileBugWith retain "vkCreateShaderModule returned ERROR_INVALID_SHADER_NV")
  case result of
    Left (ShaderCompileBug detail path) -> do
      assertBool "exception includes the driver rejection" ("ERROR_INVALID_SHADER_NV" `isInfixOf` detail)
      doesFileExist path >>= (@?= True)
      takeExtension path @?= ".spv"
    Left error' -> assertFailure ("expected ShaderCompileBug, received " <> show error')
    Right () -> assertFailure "expected ShaderCompileBug"

shaderFailureClassificationCase :: IO ()
shaderFailureClassificationCase = do
  classifyShaderFailure Result.ERROR_INVALID_SHADER_NV @?= GeneratedShaderRejected
  classifyShaderFailure Result.ERROR_DEVICE_LOST @?= ShaderDeviceLost
  classifyShaderFailure Result.ERROR_OUT_OF_HOST_MEMORY @?= OtherShaderFailure
  classifyShaderFailure Result.ERROR_OUT_OF_DEVICE_MEMORY @?= OtherShaderFailure
  classifyShaderFailure Result.ERROR_UNKNOWN @?= OtherShaderFailure

shaderDriverFailureCase :: IO ()
shaderDriverFailureCase = do
  retained <- newIORef False
  let artifact = "/tmp/injected-compute.spv"
      retain = writeIORef retained True >> pure artifact
      throwFailure result =
        throwShaderDriverFailureWith retain "injected shader operation" (show result) result
  rejected <- try (throwFailure Result.ERROR_INVALID_SHADER_NV) :: IO (Either VpipeError ())
  case rejected of
    Left (ShaderCompileBug _ path) -> path @?= artifact
    unexpected -> assertFailure ("expected ShaderCompileBug, received " <> show unexpected)
  readIORef retained >>= (@?= True)
  writeIORef retained False
  lost <- try (throwFailure Result.ERROR_DEVICE_LOST) :: IO (Either VpipeError ())
  case lost of
    Left DeviceLost -> pure ()
    unexpected -> assertFailure ("expected DeviceLost, received " <> show unexpected)
  readIORef retained >>= (@?= False)
  other <- try (throwFailure Result.ERROR_OUT_OF_DEVICE_MEMORY) :: IO (Either VpipeError ())
  case other of
    Left (VulkanFailure "injected shader operation" _) -> pure ()
    unexpected -> assertFailure ("expected VulkanFailure, received " <> show unexpected)
  readIORef retained >>= (@?= False)

sampleDump :: SpirVModule -> String -> ShaderDump
sampleDump spirV name =
  ShaderDump
    { shaderDumpName = name
    , shaderDumpStage = DumpCompute
    , shaderDumpModule = spirV
    , shaderDumpInterface = renderPipelineInterfaceTable sampleInterface
    }

sampleInterface :: PipelineInterface
sampleInterface =
  PipelineInterface
    { pipelineVertexAttributes = []
    , pipelineVertexBindings = []
    , pipelineResources =
        [ ResourceBinding
            { resourceBindingName = "values/buffer"
            , resourceBindingSet = 0
            , resourceBindingBinding = 2
            , resourceBindingShape =
                StorageArrayShape
                  TyWord
                  (Scalar UnsignedInt32)
                  StorageReadWrite
            }
        ]
    , pipelineColorAttachments = []
    , pipelineDepthAttachments = []
    , pipelinePushConstants =
        [ PushConstantRange
            { pushConstantName = "scale"
            , pushConstantOffset = 0
            , pushConstantSize = 4
            , pushConstantShaderType = TyWord
            , pushConstantFieldLayout = Scalar UnsignedInt32
            }
        ]
    }

sampleModule :: IO SpirVModule
sampleModule = either (assertFailure . show) pure computeModule

exactlyOne :: String -> [FilePath] -> IO FilePath
exactlyOne suffix paths = case filter (suffix `isSuffixOf`) paths of
  [path] -> pure path
  matches -> assertFailure ("expected one " <> suffix <> " file, got " <> show matches)

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory = bracket acquire removePathForcibly
 where
  acquire = do
    temporary <- getTemporaryDirectory
    (path, handle) <- openBinaryTempFile temporary "vpipe-diagnostics-test"
    hClose handle
    removeFile path
    createDirectory path
    pure path
