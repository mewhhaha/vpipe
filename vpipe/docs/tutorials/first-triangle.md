# Your first triangle

This tutorial starts with one window, one typed vertex buffer, and one graphics
pipeline.  By the end, the application is presenting continuously while vpipe
owns swapchain recreation, command buffers, descriptors, and synchronization.
No prior Vulkan knowledge is required.

The complete maintained program is the `triangle` example.  The version below
spells out its small loop instead of using the examples package's helper.

## Install and check Vulkan

You need GHC 9.12 or 9.14, Cabal, the Vulkan loader and headers, GLFW, and one
Vulkan driver.  On Debian or Ubuntu, the software-driver setup is:

```console
sudo apt install libvulkan-dev libglfw3-dev mesa-vulkan-drivers \
  spirv-tools vulkan-validationlayers
```

From the repository root, confirm that the packages and pure tests build:

```console
cabal build all
VPIPE_TEST_DEVICE=skip cabal test all
```

Mesa's lavapipe lets the device tests and screenshot examples run without a
physical GPU:

```console
VPIPE_TEST_DEVICE=lavapipe cabal test all
```

## The program

```haskell
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Control.Concurrent (runInBoundThread)
import Control.Monad (unless)
import Linear (V3 (..), V4 (..))
import Vpipe.Buffer (Buffer)
import Vpipe.Buffer qualified as Buffer
import Vpipe.Context (defaultVpipeConfig)
import Vpipe.Expr (V, constant, vec4, x, y, z)
import Vpipe.Format (Format (B8G8R8A8Srgb))
import Vpipe.Frame (frame, frameColorTarget, render, renderTo)
import Vpipe.GLFW
  ( defaultWindowConfig
  , pollEvents
  , windowShouldClose
  , windowSurface
  , withWindow
  )
import Vpipe.Graphics
  ( newGraphicsRuntime
  , prepareGraphicsPipeline
  )
import Vpipe.Pipeline
  ( ColorImage
  , ColorTarget
  , PipelineM
  , PrimitiveTopology (Triangles)
  , Smooth (..)
  , VertexBuffer
  , VertexSource
  , colorTarget
  , compilePipeline
  , defaultBlend
  , defaultRaster
  , drawColor
  , rasterize
  , vertexBufferBinding
  , vertexInput
  , vertexSource
  )
import Vpipe.Swapchain
  ( defaultSwapchainConfig
  , newSwapchain
  )

data Environment = Environment
  { positions :: VertexBuffer (V3 Float)
  , target :: ColorImage 'B8G8R8A8Srgb
  }

triangle :: PipelineM Environment ()
triangle = do
  input <-
    vertexInput
      (vertexSource "positions" positions :: VertexSource Environment 'Triangles (V3 Float))
  fragments <- rasterize defaultRaster (fmap vertex input)
  drawColor
    defaultBlend
    (colorTarget "color" target :: ColorTarget Environment 'B8G8R8A8Srgb)
    (fmap unSmooth fragments)
 where
  vertex position =
    ( vec4 (x position) (y position) (z position) (constant 1)
    , Smooth (constant (V4 1 0 0 1) :: V (V4 Float))
    )

main :: IO ()
main = runInBoundThread $
  withWindow defaultVpipeConfig defaultWindowConfig $ \context window -> do
    compiled <- compilePipeline triangle >>= either (fail . show) pure
    runtime <- newGraphicsRuntime context
    prepared <- prepareGraphicsPipeline runtime compiled

    vertices <-
      Buffer.newBuffer context 3
        :: IO (Buffer '[ 'Buffer.Vertex] (V3 Float))
    Buffer.writeBuffer
      vertices
      0
      [ V3 (-0.8) (-0.8) 0
      , V3 0.8 (-0.8) 0
      , V3 0 0.8 0
      ]

    swapchain <-
      newSwapchain context (windowSurface window) defaultSwapchainConfig

    let draw =
          frame swapchain $ \current ->
            renderTo (frameColorTarget current) $
              render
                prepared
                (Environment (vertexBufferBinding vertices) (frameColorTarget current))
        loop = do
          pollEvents
          closing <- windowShouldClose window
          unless closing (draw >> loop)

    loop
```

Run the repository's equivalent, which is kept intentionally below roughly
60 lines by sharing only argument and window-loop boilerplate:

```console
cabal run vpipe-examples:triangle
```

## The mental model

`PipelineM Environment ()` describes a reusable pipeline rather than drawing
immediately.  `vertexSource` says where vertex values will come from at run
time.  `vertexInput` introduces those values into the vertex stage, and
`rasterize` turns clip-space positions plus `Smooth` varyings into fragment
values.  `drawColor` connects the fragments to a typed color target.

The `Environment` is the boundary between that static description and one
draw.  Its `VertexBuffer` and `ColorImage` are bindings, not raw Vulkan
handles.  Their types let vpipe reject a missing vertex usage or an incompatible
target format before Vulkan records a command.

`compilePipeline` produces deterministic SPIR-V and a reflected resource
interface.  `prepareGraphicsPipeline` creates the device objects and caches
them in the graphics runtime.  Neither operation belongs in the per-frame
loop.

`frame` acquires the next swapchain image and passes its typed target to the
callback.  `renderTo` opens an ordered render scope; `render` adds one draw to
that scope.  A successful callback is recorded into one command buffer and
one queue submission, then presented.  The application never rotates command
pools, descriptor pools, semaphores, or frames-in-flight slots itself.

## Resize and presentation results

The triangle has no size-dependent depth image or projection, so the loop can
ignore the returned presentation result.  vpipe recreates the swapchain when
the window changes size, then reacquires once in the same `frame` call.  If
that replacement is ready, the frame callback runs normally.  An acquire-side
`PresentDeferred RecreatePending` means the replacement was also rejected and
the callback did not run; yield or poll events before retrying.

Applications with extent-dependent resources should match that result, call
`swapchainExtent`, rebuild only those resources, and then retry.  A minimized
window may return `PresentDeferred FramebufferMinimized`; continue polling
events instead of busy-waiting.

## Headless and screenshot checks

Every example accepts the same bounded screenshot mode.  It renders through
an offscreen image, so it works in CI and on a machine without a display:

```console
VPIPE_TEST_DEVICE=lavapipe \
  cabal run vpipe-examples:triangle -- \
  --frames 1 --screenshot triangle.png
```

For a minimal installation probe with no GLFW window at all, use:

```console
VPIPE_TEST_DEVICE=lavapipe \
  cabal run vpipe-examples:headless -- \
  --frames 1 --screenshot headless.png
```

## If it fails

- `NoVulkanIcd` means the loader cannot find a driver.  Install a hardware
  driver or Mesa lavapipe and check `vulkaninfo --summary`.
- A missing presentation extension usually means the context was created
  without GLFW's required instance extensions.  Use `withWindow`, which
  gathers them before context creation.
- In strict lavapipe mode, a missing validation layer is an error rather than
  a silent downgrade.  Install `vulkan-validationlayers` (the package name may
  differ by distribution).
- Set `VPIPE_DUMP=./vpipe-dump` to retain generated SPIR-V, reflected interface
  tables, and disassembly when `spirv-dis` is installed.

Continue with
[Buffers, textures, and the type system](buffers-textures-and-types.md), then
the [Compute tutorial](compute.md).
