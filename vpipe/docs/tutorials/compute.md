# Compute

vpipe compute programs are ordinary Haskell values.  You describe storage
buffers and push constants once, compile that description to SPIR-V, prepare
the resulting Vulkan pipeline for a `Context`, and then dispatch it with a
plain Haskell environment.

This complete program computes `2 * x + y` for 130 elements:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Vpipe.Buffer (Buffer)
import Vpipe.Buffer qualified as Buffer
import Vpipe.Compute qualified as Compute
import Vpipe.Context (defaultVpipeConfig, withVpipe)
import Vpipe.Pipeline (StorageBuffer, storageBufferBinding)

data Environment = Environment
  { inputX :: StorageBuffer Float
  , inputY :: StorageBuffer Float
  , output :: StorageBuffer Float
  , scale :: Float
  }

saxpy :: Compute.ComputeM Environment ()
saxpy = do
  x <- Compute.storageBuffer inputX
  y <- Compute.storageBuffer inputY
  result <- Compute.storageBuffer output
  a <- Compute.pushConstant scale
  invocation <- Compute.globalInvocationId
  let index = Compute.globalInvocationX invocation
  Compute.whenInBounds result index $ \_ ->
    Compute.writeAt result index
      (a * Compute.readAt x index + Compute.readAt y index)

main :: IO ()
main = withVpipe defaultVpipeConfig $ \context -> do
  compiled <-
    Compute.compileCompute (Compute.Dispatch @64 @1 @1) saxpy
      >>= either (fail . show) pure
  runtime <- Compute.newComputeRuntime context
  prepared <- Compute.prepareComputePipeline runtime compiled

  let count = 130
      xs = fmap fromIntegral [0 .. count - 1]
      ys = fmap (fromIntegral . (* 3)) [0 .. count - 1]
  xBuffer <- Buffer.newBuffer context count
    :: IO (Buffer '[ 'Buffer.Storage] Float)
  yBuffer <- Buffer.newBuffer context count
    :: IO (Buffer '[ 'Buffer.Storage] Float)
  outputBuffer <- Buffer.newBuffer context count
    :: IO (Buffer '[ 'Buffer.Storage, 'Buffer.CopySrc] Float)
  Buffer.writeBuffer xBuffer 0 xs
  Buffer.writeBuffer yBuffer 0 ys
  Buffer.writeBuffer outputBuffer 0 (replicate count 0)

  let environment =
        Environment
          { inputX = storageBufferBinding xBuffer
          , inputY = storageBufferBinding yBuffer
          , output = storageBufferBinding outputBuffer
          , scale = 2
          }
  Compute.dispatchFor prepared environment (toInteger count, 1, 1)
  print =<< Buffer.readBuffer outputBuffer 0 count
```

The three type arguments to `Dispatch` are the local workgroup dimensions.
`dispatchFor` accepts logical element totals and rounds each non-zero dimension
up to the required workgroup count.  Use `dispatch` instead when workgroup
counts are already known.

`globalInvocationId` has three unsigned components.  Bounds checks matter when
an element total is not a multiple of the local size; `whenInBounds` is the
short form for comparing an index with `bufferLength`, reading the element,
and conditionally running a block.

## Storage access and atomics

The operations used by a program determine its reflected storage access:

- `readAt` records a shader read.
- `writeAt` records a shader write.
- `atomicAdd` is available for `Int32` and `Word32` buffers.
- `bufferLength` queries the runtime array length without claiming a data read.

vpipe preserves the order of writes, atomics, and `whenC` blocks.  It may share
pure expression work inside one action, but it never moves a storage load
across an action boundary where a write could change the result.

## Resource ownership and synchronization

An environment may only contain managed resources from the `Context` used to
prepare the pipeline.  Buffer usage and element stride are checked before a
command buffer is recorded.  vpipe derives synchronization2 barriers from the
last tracked use, so a storage buffer can be consumed later as a vertex buffer
when its type includes both usages:

```haskell
Buffer '[ 'Buffer.Storage, 'Buffer.Vertex] (V3 Float)
```

The standalone `dispatch` functions wait for completion before returning.
This makes readback and CPU-driven programs straightforward.  Inside a frame,
the frame-loop API records compute and graphics work into the frame submission
so compute-to-graphics ordering remains on the GPU.

The particle example uses exactly that path for all 100,000 particles.  The
compute write and graphics read are ordinary adjacent passes; there is no CPU
wait or application-authored barrier between them:

```haskell
frame swapchain $ \current ->
  renderTo (frameColorTarget current) $ do
    computePassFor preparedSimulation simulationEnvironment (100000, 1, 1)
    render preparedParticles particleEnvironment
```

Both commands are recorded into the frame's single submission.  Resource
tracking observes that the same buffer changes from storage-write use to
vertex-read use and emits the synchronization2 barrier at that program-order
boundary. See the complete `particles` implementation in the repository for
pipeline setup, buffer creation, and the screenshot-friendly loop.

## Deliberate v1 limits

Sampler bindings are dynamic environment values rather than immutable layout
samplers. Compute does not yet expose workgroup shared memory or workgroup
barriers. Indirect dispatch and draw, and subgroup operations, are also
deferred. These limits keep resource access, synchronization, and capability
requirements explicit; their rationale is in
[ADR 0005](../decisions/0005-descriptor-and-compute-scope.md).

## Debugging generated shaders

Set `VPIPE_DUMP` to a directory before running the program.  Successful
compilations write SPIR-V and their reflected interface there; when
`spirv-dis` is installed, vpipe writes a disassembly beside the binary.  These
artifacts are the first things to attach when reporting a code-generation bug.
