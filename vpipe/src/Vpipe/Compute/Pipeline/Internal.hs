{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Context-owned Vulkan compute pipeline creation and exact-key caching.
module Vpipe.Compute.Pipeline.Internal (
  ComputePipelineDescription (..),
  acquireComputePipelineLeased,
) where

import Control.Concurrent.MVar (modifyMVarMasked, modifyMVarMasked_, newMVar)
import Control.Exception (catch, onException)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Vector qualified as Vector
import Data.Word (Word64)
import Vulkan.CStruct.Extends qualified as Chain
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Enums.ShaderStageFlagBits qualified as ShaderStage
import Vulkan.Core10.Handles qualified as Vk
import Vulkan.Core10.Pipeline qualified as Pipeline
import Vulkan.Exception qualified as Vulkan
import Vulkan.Zero (zero)

import Vpipe.Context.Internal (Context, contextDevice, derivedObjectName, registerContextFinalizerLeased, setObjectNameLeased)
import Vpipe.Diagnostics.Dump.Internal (ShaderDump, retainShaderFailureArtifact, throwShaderDriverFailureWith)
import Vpipe.Graphics.Cache.Internal (CachedPipeline (..), ComputeCacheStats (..), GraphicsCache (..), PipelineKey (..))
import Vpipe.Graphics.Pipeline.Internal (acquireShaderLeased)

data ComputePipelineDescription = ComputePipelineDescription
  { computeShaderBytes :: ByteString
  , computeShaderDump :: ShaderDump
  , computePipelineLayoutStructure :: ByteString
  }

acquireComputePipelineLeased :: Context -> GraphicsCache -> Vk.PipelineLayout -> ComputePipelineDescription -> IO Vk.Pipeline
acquireComputePipelineLeased context cache layout description =
  modifyMVarMasked (cachedPipelines cache) $ \cached -> do
    let key =
          ComputePipelineKey
            (computeShaderBytes description)
            (computePipelineLayoutStructure description)
    case findPipeline key cached of
      Just pipeline -> pure (cached, pipeline)
      Nothing -> do
        (shader, shaderCreated) <- acquireShaderLeased context cache "compute" (computeShaderDump description) (computeShaderBytes description)
        when shaderCreated $
          modifyMVarMasked_ (cachedComputeStats cache) $ \stats ->
            pure
              stats
                { cachedComputeShaderModuleCreations =
                    cachedComputeShaderModuleCreations stats + 1
                }
        pipeline <- createPipeline context cache layout description shader
        modifyMVarMasked_ (cachedComputeStats cache) $ \stats ->
          pure
            stats
              { cachedComputePipelineCreations =
                  cachedComputePipelineCreations stats + 1
              }
        pure (CachedPipeline key pipeline : cached, pipeline)

findPipeline :: PipelineKey -> [CachedPipeline] -> Maybe Vk.Pipeline
findPipeline _ [] = Nothing
findPipeline key (CachedPipeline cachedKey pipeline : rest)
  | key == cachedKey = Just pipeline
  | otherwise = findPipeline key rest

createPipeline :: Context -> GraphicsCache -> Vk.PipelineLayout -> ComputePipelineDescription -> Vk.ShaderModule -> IO Vk.Pipeline
createPipeline context cache layout description shader = do
  let device = contextDevice context
      stage =
        Chain.SomeStruct
          ( (zero :: Pipeline.PipelineShaderStageCreateInfo '[])
              { Pipeline.stage = ShaderStage.SHADER_STAGE_COMPUTE_BIT
              , Pipeline.module' = shader
              , Pipeline.name = "main"
              }
          )
      createInfo =
        (zero :: Pipeline.ComputePipelineCreateInfo '[])
          { Pipeline.stage = stage
          , Pipeline.layout = layout
          , Pipeline.basePipelineIndex = -1
          }
  (result, pipelines) <-
    Pipeline.createComputePipelines
      device
      (rawPipelineCache cache)
      (Vector.singleton (Chain.SomeStruct createInfo))
      Nothing
      `catch` computePipelineException description
  case (result, Vector.toList pipelines) of
    (Result.SUCCESS, [pipeline]) -> do
      cleanup <- releaseOnce (Pipeline.destroyPipeline device pipeline Nothing)
      setObjectNameLeased context ObjectType.OBJECT_TYPE_PIPELINE (pipelineHandleWord pipeline) (derivedObjectName "compute-pipeline" (pipelineHandleWord pipeline))
        `onException` cleanup
      registerContextFinalizerLeased context cleanup `onException` cleanup
      pure pipeline
    _ -> do
      traverse_ (Pipeline.destroyPipeline device `flip` Nothing) pipelines
      throwComputePipelineFailure description result (Just (Vector.length pipelines))

computePipelineException :: ComputePipelineDescription -> Vulkan.VulkanException -> IO (Result.Result, Vector.Vector Vk.Pipeline)
computePipelineException description error' =
  throwComputePipelineFailure description (Vulkan.vulkanExceptionResult error') Nothing

throwComputePipelineFailure :: ComputePipelineDescription -> Result.Result -> Maybe Int -> IO a
throwComputePipelineFailure description result pipelineCount =
  throwShaderDriverFailureWith
    (retainShaderFailureArtifact (computeShaderDump description))
    "vkCreateComputePipelines"
    (show result <> maybe "" (\count -> " returned " <> show count <> " pipelines") pipelineCount)
    result

releaseOnce :: IO () -> IO (IO ())
releaseOnce release = do
  released <- newMVar False
  pure $
    modifyMVarMasked_ released $ \done ->
      if done then pure True else release >> pure True

pipelineHandleWord :: Vk.Pipeline -> Word64
pipelineHandleWord (Vk.Pipeline handle) = handle
