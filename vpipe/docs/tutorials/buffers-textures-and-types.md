# Buffers, textures, and the type system

vpipe describes a resource twice: its Haskell element or texel type says what
the bytes mean, while a type-level usage list says what the GPU may do with
them. The Vulkan flags, descriptor validation, and useful compile errors all
come from that one description.

## A typed buffer

This complete program uploads three positions and reads them back. Element
offsets and sizes are always measured in Haskell values, not bytes.

```haskell
{-# LANGUAGE DataKinds #-}

import Linear (V3 (..))
import Vpipe.Buffer
import Vpipe.Context

main :: IO ()
main = withVpipe defaultVpipeConfig $ \context -> do
  positions <-
    newBuffer context 3
      :: IO (Buffer '[Vertex, CopySrc] (V3 Float))
  writeBuffer positions 0
    [ V3 (-0.8) (-0.8) 0
    , V3 0.8 (-0.8) 0
    , V3 0 0.8 0
    ]
  print =<< readBuffer positions 0 3
```

`Vertex` permits `vertexBufferBinding positions`. `CopySrc` permits
`readBuffer`. If `CopySrc` is omitted, GHC reports that the operation requires
that usage and tells you to add it. Empty and duplicate usage lists are also
rejected. Storage buffers may additionally be vertex inputs, which is the
typed basis of compute-to-graphics interoperation.

`newBuffer` allocates device-local memory. `writeBuffer` uses vpipe's staging
ring and queue timeline internally, so callers do not map device memory or
build transfer command buffers. `destroyBuffer` is optional inside `withVpipe`,
but useful for releasing a large resource early. Operations on a released
buffer fail rather than using a stale Vulkan handle.

The element must have a `BufferFormat` instance. Scalars, common `linear`
vectors and matrices, and generically derived records use the same std140 or
std430 layout calculator that descriptor and push-constant reflection use.
See the layout fixtures in
[`Vpipe.FormatTest`](../../test/Vpipe/FormatTest.hs) for concrete offsets.

## Images and subresources

An image adds a dimension and a promoted pixel format:

```haskell
{-# LANGUAGE DataKinds #-}

import Linear (V4 (..))
import Vpipe.Context
import Vpipe.Format
import Vpipe.Image
import Vpipe.Image.Types

upload :: Context -> IO ()
upload context = do
  image <-
    newImage context (imageExtent2D 2 2) 1 1
      :: IO
          ( Image
              'D2
              'R8G8B8A8Unorm
              '[Sampled, CopyDst, CopySrc]
          )
  writeImage image (ImageSubresource 0 0)
    [ V4 255 0 0 255, V4 0 255 0 255
    , V4 0 0 255 255, V4 255 255 255 255
    ]
  print =<< readImage image (ImageSubresource 0 0)
```

Mip levels and array layers have independent tracked layouts and completion
state. `generateMips` requires both `CopySrc` and `CopyDst`; sampling requires
`Sampled`; color and depth targets require formats that support the matching
role. A depth format used as a color target is therefore a compile-time error,
not a validation-layer surprise.

## Turning an image into a shader binding

Samplers are context-owned and cached by description. A sampled image and its
sampler become one typed environment value:

```haskell
sampler <- newSampler context defaultSamplerDescription
binding <- typedTextureBinding image sampler
```

The full program needs these imports:

```haskell
import Vpipe.Pipeline (typedTextureBinding)
import Vpipe.Sampler (defaultSamplerDescription, newSampler)
```

`typedTextureBinding` retains the image dimension and format in its result.
The expression EDSL consequently checks coordinate shape and sampled result
type. Cube textures need three coordinates; depth comparison sampling returns
a scalar; an ordinary RGBA texture returns a four-component value.

At pipeline preparation and draw time vpipe also checks facts the Haskell type
cannot prove: resources must be live, managed by the same `Context`, and backed
by Vulkan usage flags matching their role. Forging a constructor or mixing
contexts is rejected before descriptor writes or command recording.

## Targets and ordered reuse

`colorImageBinding` and `depthImageBinding` expose attachment views. A normal
image can be both a render target and, in a later ordered pass, a sampled
texture when its usage list includes both roles. Resource-state tracking emits
the layout transition and memory dependency. It deliberately rejects sampling
and writing the same subresource in one draw.

The current headless bridge is `renderGraphicsPipeline`; the windowed `Frame`
API uses the same compiled pipelines and bindings while retaining transient
descriptor and command resources per frame slot. No application code should
cache a raw descriptor set or image layout.

## Running without a GPU

On Debian or Ubuntu, install the loader, validation layer, and Mesa software
driver:

```console
sudo apt-get install libvulkan-dev mesa-vulkan-drivers vulkan-validationlayers spirv-tools
VPIPE_TEST_DEVICE=lavapipe cabal test all
```

Device tests enable strict validation in this mode. A validation message is a
test failure. Pure layout, expression, reflection, and SPIR-V tests remain
useful without an ICD.
