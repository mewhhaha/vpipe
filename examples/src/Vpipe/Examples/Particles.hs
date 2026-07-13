{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- | Compute-to-graphics particle interop. The host updates one small emitter
buffer per frame; compute applies it to a shared Storage+Vertex position
buffer, and the following point draw consumes the same allocation.
-}
module Vpipe.Examples.Particles (runParticles) where

import Control.Monad (forM_, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Vector qualified as Vector
import Linear (V2 (..), V4 (..))
import Vpipe.Buffer (Buffer, newBuffer, writeBuffer)
import Vpipe.Buffer qualified as Buffer
import Vpipe.Compute qualified as Compute
import Vpipe.Context (Context, VpipeConfig (extraDeviceExtensions, vpipeLogicalDeviceBuilder))
import Vpipe.Context.Device (CandidateDevice (candidateEnabledExtensions, candidateSamplerAnisotropy), LogicalDeviceBuilder, createLogicalDeviceWith, queueFamilyUnion)
import Vpipe.Expr qualified as Expr
import Vpipe.Format (Blendable, ColorRenderable, Format (B8G8R8A8Srgb), KnownFormat)
import Vpipe.Frame (computePassFor, frame, frameColorTarget, render, renderTo)
import Vpipe.GLFW (windowSurface)
import Vpipe.Graphics (newGraphicsRuntime, prepareGraphicsPipeline, renderGraphicsPipeline)
import Vpipe.Pipeline qualified as Pipeline
import Vpipe.Swapchain (defaultSwapchainConfig, newSwapchain)
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.Device qualified as Device
import Vulkan.Core10.DeviceInitialization qualified as Init
import Vulkan.Core12 (PhysicalDeviceVulkan12Features (timelineSemaphore))
import Vulkan.Core13 (PhysicalDeviceVulkan13Features (dynamicRendering, synchronization2))
import Vulkan.Extensions.VK_KHR_maintenance5 qualified as Maintenance5
import Vulkan.Zero (zero)

import Vpipe.Examples.Common (ExampleOptions (exampleScreenshot), captureScreenshot, compilePipelineOrFail, newScreenshotTarget, offscreenFrameCount, runWindowFramesWith, withExampleContextWith)

data ParticleEnvironment format = ParticleEnvironment
  { particleStorage :: Pipeline.StorageBuffer (V4 Float)
  , particleVertices :: Pipeline.VertexBuffer (V4 Float)
  , particleEmitter :: Pipeline.StorageBuffer (V2 Float)
  , particleTarget :: Pipeline.ColorImage format
  }

particleCompute :: forall format. Compute.ComputeM (ParticleEnvironment format) ()
particleCompute = do
  positions <- Compute.storageBuffer particleStorage
  emitter <- Compute.storageBuffer particleEmitter
  invocation <- Compute.globalInvocationId
  let index = Compute.globalInvocationX invocation
      drift2 = Compute.readAt emitter (Expr.constant 0)
      drift = Expr.vec4 (Expr.x drift2) (Expr.y drift2) (Expr.constant 0) (Expr.constant 0)
  Compute.whenInBounds positions index $ \position ->
    Compute.writeAt positions index (position + drift)

particleGraphics :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, Pipeline.ColorOutputMatches format (V4 Float)) => Pipeline.PipelineM (ParticleEnvironment format) ()
particleGraphics = do
  positions <-
    Pipeline.vertexInput
      ( Pipeline.vertexSource "particles" particleVertices ::
          Pipeline.VertexSource (ParticleEnvironment format) 'Pipeline.Points (V4 Float)
      )
  fragments <-
    Pipeline.rasterize
      Pipeline.defaultRaster
      (fmap (,Pipeline.Smooth (Expr.constant (0 :: Float) :: Expr.V Float)) positions)
  Pipeline.drawColor
    additiveBlend
    (Pipeline.colorTarget "particles" particleTarget)
    (fmap (const (Expr.constant (V4 1 0.28 0.04 0.72 :: V4 Float))) fragments)

additiveBlend :: Pipeline.Blend
additiveBlend =
  Pipeline.Blend
    { Pipeline.blendEnabled = True
    , Pipeline.blendSourceColorFactor = Pipeline.SourceAlpha
    , Pipeline.blendDestinationColorFactor = Pipeline.One
    , Pipeline.blendColorOp = Pipeline.Add
    , Pipeline.blendSourceAlphaFactor = Pipeline.One
    , Pipeline.blendDestinationAlphaFactor = Pipeline.One
    , Pipeline.blendAlphaOp = Pipeline.Add
    }

runParticles :: ExampleOptions -> IO ()
runParticles options = case exampleScreenshot options of
  Just _ -> runParticlesScreenshot options
  Nothing -> runInWindow options

runParticlesScreenshot :: ExampleOptions -> IO ()
runParticlesScreenshot options = withExampleContextWith enableMaintenance5 $ \context -> do
  compute <- compileParticleCompute
  graphics <- compilePipelineOrFail "particles" particleGraphics
  computeRuntime <- Compute.newComputeRuntime context
  preparedCompute <- Compute.prepareComputePipeline computeRuntime compute
  graphicsRuntime <- newGraphicsRuntime context
  preparedGraphics <- prepareGraphicsPipeline graphicsRuntime graphics

  positions <- newParticleBuffer context
  emitter <- newEmitterBuffer context
  target <- newScreenshotTarget context
  targetBinding <- Pipeline.colorImageBinding target
  let environment =
        ParticleEnvironment
          { particleStorage = Pipeline.storageBufferBinding positions
          , particleVertices = Pipeline.vertexBufferBinding positions
          , particleEmitter = Pipeline.storageBufferBinding emitter
          , particleTarget = targetBinding
          }

  forM_ [0 .. offscreenFrameCount options - 1] $ \frameIndex -> do
    -- This tiny host write models a moving emitter/control block. The storage
    -- and vertex state trackers provide the compute -> graphics dependency.
    writeBuffer emitter 0 [emitterDrift frameIndex]
    Compute.dispatchFor preparedCompute environment (toInteger particleCount, 1, 1)
    renderGraphicsPipeline preparedGraphics environment

  _ <- captureScreenshot options target
  pure ()

runInWindow :: ExampleOptions -> IO ()
runInWindow options = runWindowFramesWith enableMaintenance5 options "vpipe particles" $ \context window -> do
  compute <- compileParticleCompute
  graphics <- compilePipelineOrFail "particles" particleGraphics
  computeRuntime <- Compute.newComputeRuntime context
  preparedCompute <- Compute.prepareComputePipeline computeRuntime compute
  graphicsRuntime <- newGraphicsRuntime context
  preparedGraphics <- prepareGraphicsPipeline graphicsRuntime graphics
  positions <- newParticleBuffer context
  emitter <- newEmitterBuffer context
  swapchain <- newSwapchain context (windowSurface window) defaultSwapchainConfig
  frameNumber <- newIORef 0
  let base target =
        ParticleEnvironment
          { particleStorage = Pipeline.storageBufferBinding positions
          , particleVertices = Pipeline.vertexBufferBinding positions
          , particleEmitter = Pipeline.storageBufferBinding emitter
          , particleTarget = target
          }
  pure $ do
    modifyIORef' frameNumber (+ 1)
    currentFrame <- readIORef frameNumber
    writeBuffer emitter 0 [emitterDrift currentFrame]
    frame swapchain $ \current -> do
      let target = frameColorTarget current
          environment = base target :: ParticleEnvironment 'B8G8R8A8Srgb
      computePassFor preparedCompute environment (toInteger particleCount, 1, 1)
      renderTo target (render preparedGraphics environment)

compileParticleCompute :: IO (Compute.CompiledCompute (ParticleEnvironment format) 64 1 1)
compileParticleCompute = do
  result <- Compute.compileCompute (Compute.Dispatch @64 @1 @1) particleCompute
  case result of
    Left error' -> fail ("particles compute compilation failed: " <> show error')
    Right compiled -> pure compiled

newParticleBuffer :: Context -> IO (Buffer '[ 'Buffer.Storage, 'Buffer.Vertex] (V4 Float))
newParticleBuffer context = do
  when (length initialParticles /= particleCount) $
    fail ("particle fixture count mismatch: expected " <> show particleCount <> ", received " <> show (length initialParticles))
  positions <- newBuffer context particleCount
  writeBuffer positions 0 initialParticles
  pure positions

newEmitterBuffer :: Context -> IO (Buffer '[ 'Buffer.Storage] (V2 Float))
newEmitterBuffer context = do
  emitter <- newBuffer context 1
  writeBuffer emitter 0 [V2 0 0]
  pure emitter

particleCount :: Int
particleCount = 100000

initialParticles :: [V4 Float]
initialParticles = concatMap (replicate 2) uniqueParticles
 where
  uniqueParticles =
    [ let turn = fromIntegral index * 0.43
          radius = 0.12 + 0.006 * fromIntegral index
       in V4 (radius * cos turn) (radius * sin turn) 0 1
    | index <- [0 .. (particleCount `div` 2 - 1)]
    ]

emitterDrift :: Int -> V2 Float
emitterDrift frameIndex =
  let direction = if even frameIndex then 1 else -0.35
   in V2 (0.0025 * direction) 0.001

-- PointSize is not yet a shader output in the public EDSL. Maintenance5 makes
-- the core one-pixel point size well-defined without that output, so request
-- and enable it explicitly rather than accepting a validation warning.
enableMaintenance5 :: VpipeConfig -> VpipeConfig
enableMaintenance5 config =
  config
    { extraDeviceExtensions = Maintenance5.KHR_MAINTENANCE_5_EXTENSION_NAME : extraDeviceExtensions config
    , vpipeLogicalDeviceBuilder = maintenance5DeviceBuilder
    }

maintenance5DeviceBuilder :: LogicalDeviceBuilder
maintenance5DeviceBuilder candidate = do
  let queueInfos =
        Vector.fromList
          [ Chain.SomeStruct (Device.DeviceQueueCreateInfo () zero family (Vector.singleton 1))
          | family <- queueFamilyUnion candidate
          ]
      enabled13 = (zero :: PhysicalDeviceVulkan13Features){dynamicRendering = True, synchronization2 = True}
      enabled12 = (zero :: PhysicalDeviceVulkan12Features){timelineSemaphore = True}
      enabledMaintenance5 = (zero :: Maintenance5.PhysicalDeviceMaintenance5FeaturesKHR){Maintenance5.maintenance5 = True}
      enabled10 = (zero :: Init.PhysicalDeviceFeatures){Init.samplerAnisotropy = candidateSamplerAnisotropy candidate}
      createInfo =
        Device.DeviceCreateInfo
          (enabledMaintenance5 Chain.:& enabled13 Chain.:& enabled12 Chain.:& ())
          zero
          queueInfos
          Vector.empty
          (Vector.fromList (candidateEnabledExtensions candidate))
          (Just enabled10)
  createLogicalDeviceWith candidate createInfo
