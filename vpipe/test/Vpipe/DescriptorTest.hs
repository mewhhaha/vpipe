{-# LANGUAGE DataKinds #-}

module Vpipe.DescriptorTest (descriptorTests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception (throwIO, try)
import Control.Monad (forM, forM_, void, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import Vpipe.Buffer (Buffer, Usage (Uniform, Vertex), destroyBuffer, newBuffer)
import Vpipe.Context (Context, VpipeConfig (vpipeValidationStrict), contextUniformBufferOffsetAlignment, defaultVpipeConfig, withVpipe)
import Vpipe.Context.Internal (withContextLease)
import Vpipe.Descriptor.Internal
import Vpipe.Error (VpipeError (..))
import Vpipe.Expr (F)
import Vpipe.Format (Format (R8G8B8A8Unorm))
import Vpipe.Image (Image, destroyImage, imageExtent2D, newImage)
import Vpipe.Image.Types qualified as ImageTypes
import Vpipe.Pipeline.Internal
import Vpipe.Pipeline.Resource.Internal qualified as Resource
import Vpipe.Sampler (defaultSamplerDescription, newSampler)

newtype DescriptorEnvironment = DescriptorEnvironment
  { descriptorUniformAndPush :: (UniformBuffer Float, Float)
  }

descriptorUniform :: DescriptorEnvironment -> UniformBuffer Float
descriptorUniform = fst . descriptorUniformAndPush

descriptorPush :: DescriptorEnvironment -> Float
descriptorPush = snd . descriptorUniformAndPush

descriptorTests :: TestTree
descriptorTests =
  testGroup
    "descriptor"
    [ testCase "cache hits skip writes and changed uniforms update one set" cacheBehaviorTest
    , testCase "cache keys distinguish aligned ranges of the same buffer" bufferRangeCacheKeyTest
    , testCase "independent descriptor frames share a layout without cache interference" independentFrameTest
    , testCase "foreign descriptor frames are rejected before descriptor allocation or writes" foreignDescriptorFrameTest
    , testCase "resetting one descriptor frame does not release another frame's resources" frameResetIsolationTest
    , testCase "frame-recorder resolution does not retain resources" nonRetainingFrameResolutionTest
    , testCase "poisoned descriptor frames reject resolution until reset" poisonedFrameRecoveryTest
    , testCase "descriptor cleanup attempts every action and aggregates failures" cleanupAggregationTest
    , testCase "pool chunks grow past the initial frame capacity" poolGrowthTest
    , testCase "pool sizing counts repeated descriptor types" repeatedTypePoolSizingTest
    , testCase "resources from another context are rejected before writes" ownerMismatchTest
    , testCase "a vertex-only buffer forged as a uniform is rejected before writes" forgedUniformUsageTest
    , testCase "an image view forged as a sampler is rejected before writes" forgedSamplerKindTest
    , testCase "recording rejects a raw sampler before leasing its managed image" recordingRawSamplerPrevalidationTest
    , testCase "destroyed resources are rejected before writes" destroyedResourceTest
    , testCase "frame retirement retains bound resources" resourceRetentionTest
    , testCase "independent runtimes retain a resource until both frames retire" independentRuntimeRetentionTest
    , testCase "leased descriptor operations finish after Context shutdown begins" leasedOperationDuringShutdownTest
    ]

cacheBehaviorTest :: IO ()
cacheBehaviorTest = withTestContext $ \context -> do
  firstBuffer <- newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  secondBuffer <- newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileDescriptorPipeline
  runtime <- newDescriptorRuntime context (compiledPipelineInterface compiled)
  let firstEnvironment = DescriptorEnvironment (uniformBufferBinding firstBuffer, 1)
      pushChangedEnvironment = DescriptorEnvironment (uniformBufferBinding firstBuffer, 2)
      secondEnvironment = DescriptorEnvironment (uniformBufferBinding secondBuffer, 1)
  firstPlan <- resolveSuccessfully compiled firstEnvironment
  pushChangedPlan <- resolveSuccessfully compiled pushChangedEnvironment
  secondPlan <- resolveSuccessfully compiled secondEnvironment

  firstSet <- resolveDescriptors runtime firstPlan
  descriptorStats runtime >>= (@?= DescriptorStats 0 1 1)
  repeatedSet <- resolveDescriptors runtime firstPlan
  repeatedSet @?= firstSet
  descriptorStats runtime >>= (@?= DescriptorStats 1 1 1)
  _ <- resolveDescriptors runtime pushChangedPlan
  descriptorStats runtime >>= (@?= DescriptorStats 2 1 1)
  firstPush <- resolvePipelinePushConstants compiled firstEnvironment
  changedPush <- resolvePipelinePushConstants compiled pushChangedEnvironment
  when (firstPush == changedPush) $ assertFailure "changed push constants must resolve to different bytes"

  _ <- resolveDescriptors runtime secondPlan
  descriptorStats runtime >>= (@?= DescriptorStats 2 2 2)

  beginDescriptorFrame runtime
  _ <- resolveDescriptors runtime firstPlan
  descriptorStats runtime >>= (@?= DescriptorStats 2 3 3)

bufferRangeCacheKeyTest :: IO ()
bufferRangeCacheKeyTest = withTestContext $ \context -> do
  let alignment = max 1 (contextUniformBufferOffsetAlignment context)
      secondOffset = lcm alignment 4
      bufferElements = fromIntegral (secondOffset `div` 4) + 1
  buffer <- newBuffer context bufferElements :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileDescriptorPipeline
  runtime <- newDescriptorRuntime context (compiledPipelineInterface compiled)
  let originalHandle = uniformBufferHandle (uniformBufferBinding buffer)
  case (Resource.runtimeHandleOwner originalHandle, Resource.runtimeHandleGeneration originalHandle, Resource.runtimeHandleLease originalHandle, Resource.runtimeBufferMetadata originalHandle) of
    (Just owner, Just generation, Just acquireLease, Just metadata) -> do
      let bindingAt byteOffset =
            UniformBuffer
              ( Resource.managedBufferRuntimeHandle
                  owner
                  generation
                  acquireLease
                  metadata
                    { Resource.bufferBindingElementCount = 1
                    , Resource.bufferBindingByteOffset = byteOffset
                    }
              )
          firstEnvironment = DescriptorEnvironment (bindingAt 0, 1)
          secondEnvironment = DescriptorEnvironment (bindingAt secondOffset, 1)
      firstPlan <- resolveSuccessfully compiled firstEnvironment
      secondPlan <- resolveSuccessfully compiled secondEnvironment
      _ <- resolveDescriptors runtime firstPlan
      _ <- resolveDescriptors runtime secondPlan
      _ <- resolveDescriptors runtime secondPlan
      descriptorStats runtime >>= (@?= DescriptorStats 1 2 2)
    _ -> assertFailure "uniformBufferBinding did not produce a managed buffer handle with range metadata"

independentFrameTest :: IO ()
independentFrameTest = withTestContext $ \context -> do
  buffer <- newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileDescriptorPipeline
  layout <- newDescriptorLayout context (compiledPipelineInterface compiled)
  firstFrame <- newDescriptorFrame layout
  secondFrame <- newDescriptorFrame layout
  plan <- resolveSuccessfully compiled (DescriptorEnvironment (uniformBufferBinding buffer, 1))
  firstSet <- resolveDescriptorFrame layout firstFrame plan
  secondSet <- resolveDescriptorFrame layout secondFrame plan
  when (firstSet == secondSet) $ assertFailure "separate descriptor frames must allocate separate sets"
  descriptorFrameStats firstFrame >>= (@?= DescriptorStats 0 1 1)
  descriptorFrameStats secondFrame >>= (@?= DescriptorStats 0 1 1)
  _ <- resolveDescriptorFrame layout firstFrame plan
  descriptorFrameStats firstFrame >>= (@?= DescriptorStats 1 1 1)
  descriptorFrameStats secondFrame >>= (@?= DescriptorStats 0 1 1)

foreignDescriptorFrameTest :: IO ()
foreignDescriptorFrameTest = withTestContext $ \context -> do
  buffer <- newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileDescriptorPipeline
  requestedLayout <- newDescriptorLayout context (compiledPipelineInterface compiled)
  foreignLayout <- newDescriptorLayout context (compiledPipelineInterface compiled)
  foreignFrame <- newDescriptorFrame foreignLayout
  plan <- resolveSuccessfully compiled (DescriptorEnvironment (uniformBufferBinding buffer, 1))
  rejected <- try (void (resolveDescriptorFrame requestedLayout foreignFrame plan)) :: IO (Either VpipeError ())
  case rejected of
    Left (VulkanFailure "descriptor resolution" detail)
      | "layout identity" `isInfixOf` detail -> pure ()
    unexpected -> assertFailure ("expected foreign descriptor-frame rejection, got " <> show unexpected)
  descriptorFrameStats foreignFrame >>= (@?= DescriptorStats 0 0 0)

frameResetIsolationTest :: IO ()
frameResetIsolationTest = withTestContext $ \context -> do
  buffer <- newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileDescriptorPipeline
  layout <- newDescriptorLayout context (compiledPipelineInterface compiled)
  firstFrame <- newDescriptorFrame layout
  secondFrame <- newDescriptorFrame layout
  plan <- resolveSuccessfully compiled (DescriptorEnvironment (uniformBufferBinding buffer, 1))
  _ <- resolveDescriptorFrame layout firstFrame plan
  _ <- resolveDescriptorFrame layout secondFrame plan
  completed <- newEmptyMVar
  _ <- forkIO $ (try (destroyBuffer buffer) :: IO (Either VpipeError ())) >>= putMVar completed
  threadDelay 20000
  tryTakeMVar completed >>= (@?= Nothing)
  resetDescriptorFrame firstFrame
  timeout 20000 (takeMVar completed) >>= (@?= Nothing)
  resetDescriptorFrame secondFrame
  timeout 200000 (takeMVar completed) >>= (@?= Just (Right ()))

nonRetainingFrameResolutionTest :: IO ()
nonRetainingFrameResolutionTest = withTestContext $ \context -> do
  buffer <- newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileDescriptorPipeline
  layout <- newDescriptorLayout context (compiledPipelineInterface compiled)
  frame <- newDescriptorFrame layout
  plan <- resolveSuccessfully compiled (DescriptorEnvironment (uniformBufferBinding buffer, 1))
  _ <- resolveDescriptorFrameForRecording layout frame plan
  completed <- newEmptyMVar
  _ <- forkIO $ (try (destroyBuffer buffer) :: IO (Either VpipeError ())) >>= putMVar completed
  timeout 200000 (takeMVar completed) >>= (@?= Just (Right ()))

poisonedFrameRecoveryTest :: IO ()
poisonedFrameRecoveryTest = withTestContext $ \context -> do
  buffer <- newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileDescriptorPipeline
  layout <- newDescriptorLayout context (compiledPipelineInterface compiled)
  frame <- newDescriptorFrame layout
  plan <- resolveSuccessfully compiled (DescriptorEnvironment (uniformBufferBinding buffer, 1))
  _ <- resolveDescriptorFrame layout frame plan
  poisonDescriptorFrameForTest frame
  rejected <- try (void (resolveDescriptorFrame layout frame plan)) :: IO (Either VpipeError ())
  case rejected of
    Left (VulkanFailure "descriptor resolution" detail)
      | "poisoned" `isInfixOf` detail -> pure ()
    unexpected -> assertFailure ("expected poisoned-frame rejection, got " <> show unexpected)
  resetDescriptorFrame frame
  _ <- resolveDescriptorFrame layout frame plan
  descriptorFrameStats frame >>= (@?= DescriptorStats 0 2 2)

cleanupAggregationTest :: IO ()
cleanupAggregationTest = do
  attempted <- newIORef ([] :: [Int])
  let record value = modifyIORef' attempted (<> [value])
      actions =
        [ record 1 >> throwIO (userError "first cleanup failure")
        , record 2
        , record 3 >> throwIO (userError "second cleanup failure")
        ]
  result <- try (runDescriptorCleanupActionsForTest actions) :: IO (Either VpipeError ())
  readIORef attempted >>= (@?= [1, 2, 3])
  case result of
    Left (VulkanFailure "descriptor cleanup test" detail)
      | "first cleanup failure" `isInfixOf` detail
      , "second cleanup failure" `isInfixOf` detail ->
          pure ()
    unexpected -> assertFailure ("expected aggregated descriptor cleanup failure, got " <> show unexpected)

poolGrowthTest :: IO ()
poolGrowthTest = withTestContext $ \context -> do
  buffers <- forM [0 .. 69 :: Int] $ \_ -> newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileDescriptorPipeline
  runtime <- newDescriptorRuntime context (compiledPipelineInterface compiled)
  forM_ buffers $ \buffer -> do
    plan <- resolveSuccessfully compiled (DescriptorEnvironment (uniformBufferBinding buffer, 1))
    _ <- resolveDescriptors runtime plan
    pure ()
  descriptorStats runtime >>= (@?= DescriptorStats 0 70 70)
  beginDescriptorFrame runtime
  forM_ (take 2 buffers) $ \buffer -> do
    plan <- resolveSuccessfully compiled (DescriptorEnvironment (uniformBufferBinding buffer, 1))
    _ <- resolveDescriptors runtime plan
    pure ()
  descriptorStats runtime >>= (@?= DescriptorStats 0 72 72)

data TwoUniformEnvironment = TwoUniformEnvironment
  { firstUniform :: UniformBuffer Float
  , secondUniform :: UniformBuffer Float
  }

repeatedTypePoolSizingTest :: IO ()
repeatedTypePoolSizingTest = withTestContext $ \context -> do
  varyingBuffers <- forM [0 .. 39 :: Int] $ \_ -> newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  sharedBuffer <- newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileTwoUniformPipeline
  runtime <- newDescriptorRuntime context (compiledPipelineInterface compiled)
  forM_ varyingBuffers $ \buffer -> do
    plan <- resolveSuccessfully compiled (TwoUniformEnvironment (uniformBufferBinding buffer) (uniformBufferBinding sharedBuffer))
    _ <- resolveDescriptors runtime plan
    pure ()
  descriptorStats runtime >>= (@?= DescriptorStats 0 40 40)

ownerMismatchTest :: IO ()
ownerMismatchTest = withTestContext $ \ownerContext -> do
  foreignBuffer <- newBuffer ownerContext 1 :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileDescriptorPipeline
  plan <- resolveSuccessfully compiled (DescriptorEnvironment (uniformBufferBinding foreignBuffer, 1))
  withTestContext $ \runtimeContext -> do
    runtime <- newDescriptorRuntime runtimeContext (compiledPipelineInterface compiled)
    result <- try (void (resolveDescriptors runtime plan)) :: IO (Either VpipeError ())
    case result of
      Left (VulkanFailure "descriptor resolution" detail)
        | "different context" `isInfixOf` detail -> pure ()
      unexpected -> assertFailure ("expected descriptor owner rejection, got " <> show unexpected)

forgedUniformUsageTest :: IO ()
forgedUniformUsageTest = withTestContext $ \context -> do
  vertexBuffer <- newBuffer context 1 :: IO (Buffer '[ 'Vertex] Float)
  compiled <- compileDescriptorPipeline
  runtime <- newDescriptorRuntime context (compiledPipelineInterface compiled)
  let forgedUniform = UniformBuffer (vertexBufferHandle (vertexBufferBinding vertexBuffer))
  plan <- resolveSuccessfully compiled (DescriptorEnvironment (forgedUniform, 1))
  result <- try (void (resolveDescriptors runtime plan)) :: IO (Either VpipeError ())
  case result of
    Left (VulkanFailure "descriptor resolution" detail)
      | "UNIFORM" `isInfixOf` detail -> pure ()
    unexpected -> assertFailure ("expected uniform usage rejection, got " <> show unexpected)

newtype TextureEnvironment = TextureEnvironment
  { descriptorTexture :: TextureBinding
  }

forgedSamplerKindTest :: IO ()
forgedSamplerKindTest = withTestContext $ \context -> do
  image <- newImage context (imageExtent2D 1 1) 1 1 :: IO (Image 'ImageTypes.D2 'R8G8B8A8Unorm '[ 'ImageTypes.Sampled])
  sampler <- newSampler context defaultSamplerDescription
  validBinding <- textureBinding image sampler
  compiled <- compileTexturePipeline
  runtime <- newDescriptorRuntime context (compiledPipelineInterface compiled)
  let imageHandle = textureImageHandle validBinding
      forgedBinding = TextureBinding imageHandle imageHandle
  plan <- resolveSuccessfully compiled (TextureEnvironment forgedBinding)
  result <- try (void (resolveDescriptors runtime plan)) :: IO (Either VpipeError ())
  case result of
    Left (VulkanFailure "descriptor resolution" detail)
      | "managed Sampler" `isInfixOf` detail -> pure ()
    unexpected -> assertFailure ("expected sampler-kind rejection, got " <> show unexpected)

recordingRawSamplerPrevalidationTest :: IO ()
recordingRawSamplerPrevalidationTest = withTestContext $ \context -> do
  image <- newImage context (imageExtent2D 1 1) 1 1 :: IO (Image 'ImageTypes.D2 'R8G8B8A8Unorm '[ 'ImageTypes.Sampled])
  sampler <- newSampler context defaultSamplerDescription
  validBinding <- textureBinding image sampler
  compiled <- compileTexturePipeline
  layout <- newDescriptorLayout context (compiledPipelineInterface compiled)
  frame <- newDescriptorFrame layout
  let forgedBinding = TextureBinding (textureImageHandle validBinding) (RuntimeHandle 0xBAD5A)
  plan <- resolveSuccessfully compiled (TextureEnvironment forgedBinding)
  result <- try (void (resolveDescriptorFrameForRecording layout frame plan)) :: IO (Either VpipeError ())
  case result of
    Left (VulkanFailure "descriptor resolution" detail)
      | "unmanaged" `isInfixOf` detail -> pure ()
    unexpected -> assertFailure ("expected raw sampler rejection, got " <> show unexpected)
  descriptorFrameStats frame >>= (@?= DescriptorStats 0 0 0)
  completed <- newEmptyMVar
  _ <- forkIO $ (try (destroyImage image) :: IO (Either VpipeError ())) >>= putMVar completed
  timeout 2_000_000 (takeMVar completed) >>= (@?= Just (Right ()))

destroyedResourceTest :: IO ()
destroyedResourceTest = withTestContext $ \context -> do
  buffer <- newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileDescriptorPipeline
  runtime <- newDescriptorRuntime context (compiledPipelineInterface compiled)
  let environment = DescriptorEnvironment (uniformBufferBinding buffer, 1)
  plan <- resolveSuccessfully compiled environment
  destroyBuffer buffer
  result <- try (void (resolveDescriptors runtime plan)) :: IO (Either VpipeError ())
  case result of
    Left BufferReleased -> pure ()
    unexpected -> assertFailure ("expected BufferReleased, got " <> show unexpected)

resourceRetentionTest :: IO ()
resourceRetentionTest = withTestContext $ \context -> do
  buffer <- newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileDescriptorPipeline
  runtime <- newDescriptorRuntime context (compiledPipelineInterface compiled)
  plan <- resolveSuccessfully compiled (DescriptorEnvironment (uniformBufferBinding buffer, 1))
  _ <- resolveDescriptors runtime plan
  completed <- newEmptyMVar
  _ <- forkIO $ (try (destroyBuffer buffer) :: IO (Either VpipeError ())) >>= putMVar completed
  threadDelay 20000
  beforeRetirement <- tryTakeMVar completed
  beforeRetirement @?= Nothing
  beginDescriptorFrame runtime
  takeMVar completed >>= (@?= Right ())

independentRuntimeRetentionTest :: IO ()
independentRuntimeRetentionTest = withTestContext $ \context -> do
  buffer <- newBuffer context 1 :: IO (Buffer '[ 'Uniform] Float)
  compiled <- compileDescriptorPipeline
  firstRuntime <- newDescriptorRuntime context (compiledPipelineInterface compiled)
  secondRuntime <- newDescriptorRuntime context (compiledPipelineInterface compiled)
  plan <- resolveSuccessfully compiled (DescriptorEnvironment (uniformBufferBinding buffer, 1))
  _ <- resolveDescriptors firstRuntime plan
  _ <- resolveDescriptors secondRuntime plan
  completed <- newEmptyMVar
  _ <- forkIO $ (try (destroyBuffer buffer) :: IO (Either VpipeError ())) >>= putMVar completed
  threadDelay 20000
  timeout 20000 (takeMVar completed) >>= (@?= Nothing)
  beginDescriptorFrame firstRuntime
  timeout 20000 (takeMVar completed) >>= (@?= Nothing)
  beginDescriptorFrame secondRuntime
  result <- timeout 200000 (takeMVar completed)
  result @?= Just (Right ())

leasedOperationDuringShutdownTest :: IO ()
leasedOperationDuringShutdownTest = do
  completed <- newEmptyMVar
  withTestContext $ \context -> do
    compiled <- compileDescriptorPipeline
    runtime <- newDescriptorRuntime context (compiledPipelineInterface compiled)
    started <- newEmptyMVar
    _ <- forkIO $ do
      result <- try $ withContextLease context $ do
        putMVar started ()
        waitUntilClosing runtime 1000
        beginDescriptorFrameLeased runtime
      putMVar completed (result :: Either VpipeError ())
    takeMVar started
  timeout 200000 (takeMVar completed) >>= (@?= Just (Right ()))

waitUntilClosing :: DescriptorRuntime -> Int -> IO ()
waitUntilClosing runtime attempts
  | attempts <= 0 = assertFailure "Context shutdown did not begin"
  | otherwise = do
      result <- try (beginDescriptorFrame runtime)
      case result of
        Left ContextClosed -> pure ()
        Left error' -> throwIO (error' :: VpipeError)
        Right () -> threadDelay 1000 >> waitUntilClosing runtime (attempts - 1)

compileDescriptorPipeline :: IO (CompiledPipeline DescriptorEnvironment)
compileDescriptorPipeline = do
  result <- compilePipeline $ do
    _ <- uniform (uniformSource "value" descriptorUniform)
    _ <- pushConstant descriptorPush :: PipelineM DescriptorEnvironment (F Float)
    pure ()
  case result of
    Left pipelineError -> assertFailure ("descriptor pipeline compilation failed: " <> show pipelineError)
    Right compiled -> pure compiled

compileTexturePipeline :: IO (CompiledPipeline TextureEnvironment)
compileTexturePipeline = do
  result <- compilePipeline $ do
    _ <- texture (textureSource "texture" descriptorTexture)
    pure ()
  case result of
    Left pipelineError -> assertFailure ("texture pipeline compilation failed: " <> show pipelineError)
    Right compiled -> pure compiled

compileTwoUniformPipeline :: IO (CompiledPipeline TwoUniformEnvironment)
compileTwoUniformPipeline = do
  result <- compilePipeline $ do
    _ <- uniform (uniformSource "first" firstUniform)
    _ <- uniform (uniformSource "second" secondUniform)
    pure ()
  case result of
    Left pipelineError -> assertFailure ("two-uniform pipeline compilation failed: " <> show pipelineError)
    Right compiled -> pure compiled

resolveSuccessfully :: CompiledPipeline env -> env -> IO ResolvedBindingPlan
resolveSuccessfully compiled environment =
  case resolvePipelineBindings compiled environment of
    Left pipelineError -> assertFailure ("descriptor binding resolution failed: " <> show pipelineError)
    Right resolved -> pure resolved

withTestContext :: (Context -> IO a) -> IO a
withTestContext action = do
  requested <- lookupEnv "VPIPE_TEST_DEVICE"
  let config = defaultVpipeConfig{vpipeValidationStrict = requested == Just "lavapipe"}
  result <- try (withVpipe config action)
  case result of
    Left (NoVulkanIcd detail) | requested /= Just "lavapipe" -> error ("SKIP: Vulkan ICD unavailable: " <> detail)
    Left error' -> throwIO (error' :: VpipeError)
    Right value -> pure value
