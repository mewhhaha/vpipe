{-# LANGUAGE ScopedTypeVariables #-}

module Vpipe.Graphics.Cache.Internal (
  GraphicsCache (..),
  GraphicsStats (..),
  ComputeCacheStats (..),
  CachedShader (..),
  PipelineKey (..),
  PrimitiveTopologyKey (..),
  CachedPipeline (..),
  newGraphicsCache,
  newGraphicsCacheWithPipelineCache,
  destroyGraphicsCache,
) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (AsyncException, SomeException, catch, finally, fromException, onException, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word (Word32, Word64)
import Foreign.Ptr (castPtr)
import Vulkan.Core10.Enums.Format qualified as VkFormat
import Vulkan.Core10.Enums.ObjectType qualified as ObjectType
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Enums.SampleCountFlagBits qualified as Samples
import Vulkan.Core10.Handles qualified as Vk
import Vulkan.Core10.Pipeline qualified as Pipeline
import Vulkan.Core10.PipelineCache qualified as PipelineCache
import Vulkan.Core10.Shader qualified as Shader
import Vulkan.Zero (zero)

import Vpipe.Graphics.Cache.Persistence (writePipelineCacheFile)

data GraphicsStats = GraphicsStats
  { shaderModuleCreations :: Int
  , graphicsPipelineCreations :: Int
  }
  deriving (Eq, Show)

data ComputeCacheStats = ComputeCacheStats
  { cachedComputeShaderModuleCreations :: Int
  , cachedComputePipelineCreations :: Int
  }
  deriving (Eq, Show)

data CachedShader = CachedShader ByteString Shader.ShaderModule

{- | The complete Vulkan-visible pipeline description.  The byte strings are
deliberately retained instead of hashed: a cache hit must never depend on
a probabilistic collision check.
-}
data PipelineKey
  = GraphicsPipelineKey
      ByteString
      ByteString
      PrimitiveTopologyKey
      [(String, Word32)]
      [(String, String, Word32, VkFormat.Format, Word32)]
      [(VkFormat.Format, (Bool, Int, Int, Int, Int, Int, Int))]
      (Maybe (VkFormat.Format, (Bool, Bool, Int)))
      (Int, Int)
      Samples.SampleCountFlagBits
      ByteString
  | ComputePipelineKey
      ByteString
      ByteString
  deriving (Eq)

data PrimitiveTopologyKey
  = PointTopology
  | LineTopology
  | TriangleTopology
  deriving (Eq)

data CachedPipeline = CachedPipeline PipelineKey Pipeline.Pipeline

data GraphicsCache = GraphicsCache
  { cachedShaders :: MVar [CachedShader]
  , cachedPipelines :: MVar [CachedPipeline]
  , cachedGraphicsStats :: MVar GraphicsStats
  , cachedComputeStats :: MVar ComputeCacheStats
  , rawPipelineCache :: Vk.PipelineCache
  }

newGraphicsCache :: Vk.Device -> ByteString -> (ObjectType.ObjectType -> String -> Word64 -> IO ()) -> IO GraphicsCache
newGraphicsCache device = newGraphicsCacheWithPipelineCache device createPipelineCache
 where
  -- Standard Vulkan ignores incompatible initial cache data, so failures must reach context error mapping.
  createPipelineCache bytes =
    ByteString.useAsCString bytes $ \pointer ->
      PipelineCache.createPipelineCache
        device
        ( (zero :: PipelineCache.PipelineCacheCreateInfo)
            { PipelineCache.initialDataSize = fromIntegral (ByteString.length bytes)
            , PipelineCache.initialData = castPtr pointer
            }
        )
        Nothing

newGraphicsCacheWithPipelineCache :: Vk.Device -> (ByteString -> IO Vk.PipelineCache) -> ByteString -> (ObjectType.ObjectType -> String -> Word64 -> IO ()) -> IO GraphicsCache
newGraphicsCacheWithPipelineCache device createPipelineCache initialBytes nameObject = do
  pipelineCache <- createPipelineCache initialBytes
  let destroyPipelineCache = PipelineCache.destroyPipelineCache device pipelineCache Nothing
      handle = pipelineCacheHandleWord pipelineCache
  nameObject ObjectType.OBJECT_TYPE_PIPELINE_CACHE "pipeline-cache" handle
    `onException` destroyPipelineCache
  ( GraphicsCache
      <$> newMVar []
      <*> newMVar []
      <*> newMVar (GraphicsStats 0 0)
      <*> newMVar (ComputeCacheStats 0 0)
      <*> pure pipelineCache
    )
    `onException` destroyPipelineCache

pipelineCacheHandleWord :: Vk.PipelineCache -> Word64
pipelineCacheHandleWord (Vk.PipelineCache handle) = handle

destroyGraphicsCache :: Vk.Device -> FilePath -> GraphicsCache -> IO ()
destroyGraphicsCache device path cache =
  persist `finally` PipelineCache.destroyPipelineCache device (rawPipelineCache cache) Nothing
 where
  persist =
    ( do
        (result, bytes) <- PipelineCache.getPipelineCacheData device (rawPipelineCache cache)
        case result of
          Result.SUCCESS
            | not (ByteString.null bytes) -> writePipelineCacheFile path bytes
          _ -> pure ()
    )
      `catch` \(error' :: SomeException) ->
        case fromException error' of
          Just asynchronous -> throwIO (asynchronous :: AsyncException)
          Nothing -> pure ()
