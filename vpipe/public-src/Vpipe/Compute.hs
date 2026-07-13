{-# LANGUAGE PackageImports #-}

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
module Vpipe.Compute (
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
  CompiledCompute,
  ComputeCompileError,
  compileCompute,
  workgroupCounts,
  ComputeRuntime,
  PreparedCompute,
  ComputeStats (..),
  newComputeRuntime,
  computeStats,
  prepareComputePipeline,
  dispatch,
  dispatchFor,
) where

import "vpipe" Vpipe.Compute.Internal
