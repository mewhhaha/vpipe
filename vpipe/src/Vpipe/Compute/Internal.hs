{- | Typed compute-shader construction, compilation, and synchronous dispatch.

Workgroup counts can be derived safely from element totals:

@
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Vpipe.Compute (Dispatch (..), workgroupCounts)

main :: IO ()
main = print (workgroupCounts (Dispatch @64 @1 @1) (1000, 1, 1))
@
-}
module Vpipe.Compute.Internal (
  ComputeM,
  Dispatch (..),
  StorageBuf,
  StorageElement,
  AtomicInteger,
  storageBuffer,
  globalInvocationId,
  globalInvocationX,
  globalInvocationY,
  globalInvocationZ,
  readAt,
  bufferLength,
  writeAt,
  atomicAdd,
  whenC,
  whenInBounds,
  pushConstant,
  CompiledCompute (..),
  ComputeCompileError (..),
  compileCompute,
  workgroupCounts,
  resolveComputeBindings,
  resolveComputePushConstants,
  ComputeRuntime,
  PreparedCompute,
  ComputeStats (..),
  newComputeRuntime,
  computeStats,
  prepareComputePipeline,
  dispatch,
  dispatchFor,
) where

import Control.Monad.State.Strict (get, put)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import Linear (V3)
import Vpipe.Compute.Compile.Internal (CompiledCompute (..), Dispatch (..), compileCompute, resolveComputeBindings, resolveComputePushConstants, workgroupCounts)
import Vpipe.Compute.IR.Internal
import Vpipe.Compute.Runtime.Internal (ComputeRuntime, ComputeStats (..), PreparedCompute, computeStats, dispatch, dispatchFor, newComputeRuntime, prepareComputePipeline)
import Vpipe.Expr (C, (<.))
import Vpipe.Expr.Internal qualified as Expr
import Vpipe.Pipeline.Internal (StorageBuffer)

storageBuffer :: forall env a. (StorageElementSupported a, StorageElement a) => (env -> StorageBuffer a) -> ComputeM env (StorageBuf a)
storageBuffer accessor = ComputeM $ do
  recorder <- get
  let index = length (computeStorageResources recorder)
      symbol = "storage." <> show index
      shaderType = storageElementType (Proxy @a)
      layout = storageElementLayout (Proxy @a)
      resource = StorageResource symbol accessor shaderType layout
  put recorder{computeStorageResources = computeStorageResources recorder <> [resource]}
  pure (StorageBuf symbol shaderType layout)

globalInvocationId :: ComputeM env (C (V3 Word32))
globalInvocationId = pure (Expr.input "globalInvocationId")

globalInvocationX :: C (V3 Word32) -> C Word32
globalInvocationX = Expr.extract Expr.TyWord [0]

globalInvocationY :: C (V3 Word32) -> C Word32
globalInvocationY = Expr.extract Expr.TyWord [1]

globalInvocationZ :: C (V3 Word32) -> C Word32
globalInvocationZ = Expr.extract Expr.TyWord [2]

readAt :: StorageBuf a -> C Word32 -> C a
readAt buffer = Expr.storageRead (storageBufType buffer) (storageBufSymbol buffer)

bufferLength :: StorageBuf a -> C Word32
bufferLength = Expr.storageLength . storageBufSymbol

writeAt :: StorageBuf a -> C Word32 -> C a -> ComputeM env ()
writeAt buffer index value = appendStatement (WriteStatement (storageBufSymbol buffer) (Expr.someExpr index) (Expr.someExpr value))

atomicAdd :: (AtomicIntegerSupported a, AtomicInteger a) => StorageBuf a -> C Word32 -> C a -> ComputeM env ()
atomicAdd buffer index value = appendStatement (AtomicAddStatement (storageBufSymbol buffer) (Expr.someExpr index) (Expr.someExpr value))

whenC :: C Bool -> ComputeM env () -> ComputeM env ()
whenC condition body = ComputeM $ do
  before <- get
  put before{computeStatements = []}
  unComputeM body
  after <- get
  put
    after
      { computeStatements =
          computeStatements before
            <> [WhenStatement (Expr.someExpr condition) (computeStatements after)]
      }

whenInBounds :: StorageBuf a -> C Word32 -> (C a -> ComputeM env ()) -> ComputeM env ()
whenInBounds buffer index body =
  whenC (index <. bufferLength buffer) (body (readAt buffer index))

pushConstant :: forall env a. (StorageElementSupported a, StorageElement a) => (env -> a) -> ComputeM env (C a)
pushConstant accessor = ComputeM $ do
  recorder <- get
  let index = length (computePushResources recorder)
      symbol = "push." <> show index
      shaderType = storageElementType (Proxy @a)
      layout = storageElementLayout (Proxy @a)
      resource = PushResource symbol accessor shaderType layout
  put recorder{computePushResources = computePushResources recorder <> [resource]}
  pure (Expr.input symbol)

appendStatement :: ComputeStatement -> ComputeM env ()
appendStatement statement = ComputeM $ do
  recorder <- get
  put recorder{computeStatements = computeStatements recorder <> [statement]}
