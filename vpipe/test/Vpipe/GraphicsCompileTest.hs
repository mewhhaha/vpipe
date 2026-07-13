{-# LANGUAGE DataKinds #-}

module Vpipe.GraphicsCompileTest (graphicsCompileTests) where

import Control.Exception (bracket)
import Control.Monad (filterM)
import Data.ByteString.Lazy qualified as BL
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Word (Word32)
import Linear (M23, M44, V2 (..), V3 (..), V4)
import System.Directory (createDirectory, doesFileExist, findExecutable, getTemporaryDirectory, listDirectory, removeFile, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, (</>))
import System.IO (hClose, openBinaryTempFile)
import System.IO.Error (catchIOError)
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Vpipe.Buffer.Format (FieldLayout (..), ScalarType (..), toMatrixBuffer)
import Vpipe.Expr
import Vpipe.Expr.Internal (ShaderTy (..))
import Vpipe.Expr.Internal qualified as ExprInternal
import Vpipe.Format (Format (D32Sfloat, R8G8B8A8Unorm))
import Vpipe.Pipeline.Internal
import Vpipe.SpirV.Assembler (SpirVModule, moduleBytes)

graphicsCompileTests :: TestTree
graphicsCompileTests =
  testGroup
    "graphics compilation"
    [ testCase "lowers a typed triangle deterministically" triangleCase
    , testCase "retains static raster, blend, and depth state" staticStateCase
    , testCase "replaces fragment varying roots with output actions" fragmentRootCase
    , testCase "filters vertex interfaces for each compiled draw" multipleDrawCase
    , testCase "lowers matrix uniform and push-constant blocks" matrixBlockCase
    , testCase "indexed draws preserve command-only state through lowering" indexedDrawCase
    , testCase "invalid shader graphs fail during compilePipeline" invalidGraphCase
    , testCase "compilePipeline dumps both graphics stages without a Context" compileDumpCase
    ]

data Environment

positions :: VertexSource Environment 'Triangles (V3 Float)
positions = vertexSource "positions" (const (VertexBuffer (RuntimeHandle 1)))

coordinates :: VertexSource Environment 'Lines (V3 Float)
coordinates = vertexSource "coordinates" (const (VertexBuffer (RuntimeHandle 2)))

color :: ColorTarget Environment 'R8G8B8A8Unorm
color = colorTarget "color" (const (ColorImage (RuntimeHandle 3)))

depth :: DepthTarget Environment 'D32Sfloat
depth = depthTarget "depth" (const (DepthImage (RuntimeHandle 4)))

indices :: IndexSource Environment
indices = indexSource "indices" (const (error "compile-only index source was resolved"))

triangleCase :: IO ()
triangleCase = do
  first <- exactlyOne . compiledPipelineDraws =<< compileSuccessfully trianglePipeline
  second <- exactlyOne . compiledPipelineDraws =<< compileSuccessfully trianglePipeline
  compiledVertexModule first @?= compiledVertexModule second
  compiledFragmentModule first @?= compiledFragmentModule second
  compiledDrawRaster first @?= defaultRaster
  exactlyOne (compiledColorOutputs first) >>= (@?= defaultBlend) . compiledColorBlend
  compiledDepthState <$> compiledDepthOutput first @?= Just defaultDepth
  fmap vertexAttributeName (compiledVertexAttributes first) @?= ["positions"]
  assertBool "vertex module has bytes" (not (BL.null (moduleBytes (compiledVertexModule first))))
  validateModule (compiledVertexModule first)
  validateModule (compiledFragmentModule first)

staticStateCase :: IO ()
staticStateCase = do
  compiled <- compileSuccessfully staticStatePipeline
  draw <- exactlyOne (compiledPipelineDraws compiled)
  compiledDrawRaster draw @?= Raster CullBack FrontClockwise
  exactlyOne (compiledColorOutputs draw)
    >>= (@?= Blend True SourceAlpha OneMinusSourceAlpha Add One Zero Add) . compiledColorBlend
  compiledDepthState <$> compiledDepthOutput draw
    @?= Just (Depth True False DepthGreater)

fragmentRootCase :: IO ()
fragmentRootCase = do
  compiled <- compileSuccessfully trianglePipeline
  draw <- exactlyOne (compiledPipelineDraws compiled)
  assertBool "fragment module compiles despite leading varying roots" (not (BL.null (moduleBytes (compiledFragmentModule draw))))

multipleDrawCase :: IO ()
multipleDrawCase = do
  compiled <- compileSuccessfully multipleDrawPipeline
  let draws = compiledPipelineDraws compiled
  fmap (fmap vertexAttributeName . compiledVertexAttributes) draws @?= [["positions"], ["coordinates"]]

matrixBlockCase :: IO ()
matrixBlockCase = do
  compiled <- compileSuccessfully matrixBlockPipeline
  draw <- exactlyOne (compiledPipelineDraws compiled)
  pipelineResources (compiledPipelineInterface compiled)
    @?= [ResourceBinding "mvp" 0 0 (UniformShape (TyMatrix 4 4) (Matrix 4 4 Float32))]
  pipelinePushConstants (compiledPipelineInterface compiled)
    @?= [PushConstantRange "push.0" 0 24 (TyMatrix 2 3) (Matrix 3 2 Float32)]
  validateModule (compiledVertexModule draw)
  validateModule (compiledFragmentModule draw)

indexedDrawCase :: IO ()
indexedDrawCase = do
  direct <- compileSuccessfully (indexComparisonPipeline False)
  indexed <- compileSuccessfully (indexComparisonPipeline True)
  directDraw <- exactlyOne (compiledPipelineDraws direct)
  indexedDraw <- exactlyOne (compiledPipelineDraws indexed)
  compiledDrawIndexSource directDraw @?= Nothing
  compiledDrawIndexSource indexedDraw @?= Just "indices"
  compiledPipelineInterface indexed @?= compiledPipelineInterface direct
  indexedDraw{compiledDrawIndexSource = Nothing} @?= directDraw

invalidGraphCase :: IO ()
invalidGraphCase = do
  result <- compilePipeline $ do
    _ <- storageBuffer (storageSource "unsupported" (const (StorageBuffer (RuntimeHandle 10))))
    inputs <- vertexInput positions
    fragments <- rasterize defaultRaster (fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (x position))) inputs)
    let invalidColor = ExprInternal.storageRead (TyVector 4) "storage.unsupported" (constant (0 :: Word32)) :: F (V4 Float)
    drawColor defaultBlend color (fmap (const invalidColor) fragments)
  case result of
    Left (GraphicsShaderCompilationFailed 0 _ detail) ->
      assertBool ("error identifies the invalid stage operation: " <> detail) ("storage reads are compute-only" `isInfixOf` detail)
    Left error' -> assertFailure ("expected GraphicsShaderCompilationFailed, received " <> show error')
    Right _ -> assertFailure "expected invalid graphics compilation to fail"

compileDumpCase :: IO ()
compileDumpCase =
  withTemporaryDirectory $ \directory ->
    withDumpDirectory directory $ do
      compiled <- compileSuccessfully trianglePipeline
      draw <- exactlyOne (compiledPipelineDraws compiled)
      files <- listDirectory directory
      vertexPath <- assertStageArtifacts directory files "vertex" (compiledVertexModule draw)
      _ <- assertStageArtifacts directory files "fragment" (compiledFragmentModule draw)
      let interfacePath = dropExtension vertexPath <> ".interface.txt"
      interface <- readFile interfacePath
      assertBool "dump includes the graphics interface" ("vertex bindings:" `isInfixOf` interface && "color attachments:" `isInfixOf` interface)
 where
  assertStageArtifacts directory files stage module' = do
    let prefix = "graphics-draw-0." <> stage <> "."
        candidates = [directory </> path | path <- files, prefix `isPrefixOf` path, ".spv" `isSuffixOf` path]
    matches <- filterM (fmap (== moduleBytes module') . BL.readFile) candidates
    spirVPath <- case matches of
      path : _ -> pure path
      [] -> assertFailure ("missing " <> stage <> " SPIR-V dump matching the compiled module") >> fail "unreachable"
    let interfacePath = dropExtension spirVPath <> ".interface.txt"
        disassemblyPath = dropExtension spirVPath <> ".spvasm"
    doesFileExist interfacePath >>= (@?= True)
    disassembler <- findExecutable "spirv-dis"
    case disassembler of
      Nothing -> pure ()
      Just _ -> doesFileExist disassemblyPath >>= (@?= True)
    pure spirVPath

trianglePipeline :: PipelineM Environment ()
trianglePipeline = do
  firstUniform <- uniform (uniformSource "first" (const (UniformBuffer (RuntimeHandle 5))) :: Uniform Environment Float)
  _ <- uniform (uniformSource "second" (const (UniformBuffer (RuntimeHandle 6))) :: Uniform Environment Float)
  _ <- texture (textureSource "albedo" (const (TextureBinding (RuntimeHandle 7) (RuntimeHandle 8))))
  fragmentPush <- pushConstant (const 0 :: Environment -> Float)
  inputs <- vertexInput positions
  fragments <- rasterize defaultRaster (fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (x position + firstUniform))) inputs)
  let shaded = discardWhen (unSmooth (fragmentValue fragments) <. constant 0) (fmap ((\value -> vec4 (value + fragmentPush) value value (constant 1)) . unSmooth) fragments)
  drawColor defaultBlend color shaded
  drawDepth defaultDepth depth (fmap (const (constant 0.5)) shaded)

staticStatePipeline :: PipelineM Environment ()
staticStatePipeline = do
  inputs <- vertexInput positions
  fragments <- rasterize (Raster CullBack FrontClockwise) (fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (x position))) inputs)
  drawColor (Blend True SourceAlpha OneMinusSourceAlpha Add One Zero Add) color (fmap (\(Smooth value) -> vec4 value value value (constant 1)) fragments)
  drawDepth (Depth True False DepthGreater) depth (fmap (const (constant 0.5)) fragments)

multipleDrawPipeline :: PipelineM Environment ()
multipleDrawPipeline = do
  triangle <- vertexInput positions
  triangleFragments <- rasterize defaultRaster (fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (x position))) triangle)
  drawColor defaultBlend color (fmap ((\value -> vec4 value value value (constant 1)) . unSmooth) triangleFragments)
  lineInputs <- vertexInput coordinates
  lineFragments <- rasterize defaultRaster (fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (x position))) lineInputs)
  drawColor defaultBlend color (fmap ((\value -> vec4 value value value (constant 1)) . unSmooth) lineFragments)

matrixBlockPipeline :: PipelineM Environment ()
matrixBlockPipeline = do
  mvp <- uniform (uniformSource "mvp" (const (UniformBuffer (RuntimeHandle 9))) :: Uniform Environment (M44 Float))
  transform <- pushConstant (const (toMatrixBuffer (V2 (V3 1 2 3) (V3 4 5 6)))) :: PipelineM Environment (V (M23 Float))
  inputs <- vertexInput positions
  fragments <- rasterize defaultRaster (fmap (\position -> (mvp !* vec4 (x position) (y position) (z position) (constant 1), Smooth (transform !* vec3 (x position) (y position) (z position)))) inputs)
  drawColor defaultBlend color (fmap (\(Smooth value) -> vec4 (x value) (y value) (constant 0) (constant 1)) fragments)

indexComparisonPipeline :: Bool -> PipelineM Environment ()
indexComparisonPipeline indexed = do
  inputs <- vertexInput positions
  let projected = fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (x position))) inputs
  fragments <-
    if indexed
      then rasterizeIndexed defaultRaster indices projected
      else rasterize defaultRaster projected
  drawColor defaultBlend color (fmap (\(Smooth value) -> vec4 value value value (constant 1)) fragments)

compileSuccessfully :: PipelineM Environment () -> IO (CompiledPipeline Environment)
compileSuccessfully pipeline = do
  result <- compilePipeline pipeline
  case result of
    Left error' -> assertFailure (show error') >> fail "unreachable"
    Right compiled -> pure compiled

exactlyOne :: [a] -> IO a
exactlyOne values = case values of
  [value] -> pure value
  _ -> assertFailure ("expected exactly one value, got " <> show (length values)) >> fail "unreachable"

validateModule :: SpirVModule -> IO ()
validateModule module' = do
  validator <- findExecutable "spirv-val"
  case validator of
    Nothing -> pure ()
    Just executable -> withModuleFile module' $ \path -> do
      (exitCode, standardOutput, standardError) <- readProcessWithExitCode executable ["--target-env", "vulkan1.3", path] ""
      case exitCode of
        ExitSuccess -> pure ()
        ExitFailure code -> assertFailure ("spirv-val exited with " <> show code <> "\n" <> standardOutput <> standardError)

withModuleFile :: SpirVModule -> (FilePath -> IO a) -> IO a
withModuleFile module' action = do
  temporaryDirectory <- getTemporaryDirectory
  bracket
    (openBinaryTempFile temporaryDirectory "vpipe-graphics-compile.spv")
    (\(path, handle) -> hClose handle `catchIOError` const (pure ()) >> removeFile path `catchIOError` const (pure ()))
    (\(path, handle) -> BL.hPut handle (moduleBytes module') >> hClose handle >> action path)

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory action = do
  temporaryDirectory <- getTemporaryDirectory
  bracket
    ( do
        (path, handle) <- openBinaryTempFile temporaryDirectory "vpipe-graphics-compile-dump"
        hClose handle
        removeFile path
        createDirectory path
        pure path
    )
    removePathForcibly
    action

withDumpDirectory :: FilePath -> IO a -> IO a
withDumpDirectory directory action =
  bracket
    (lookupEnv "VPIPE_DUMP")
    restore
    (\_ -> setEnv "VPIPE_DUMP" directory >> action)
 where
  restore Nothing = unsetEnv "VPIPE_DUMP"
  restore (Just previous) = setEnv "VPIPE_DUMP" previous
