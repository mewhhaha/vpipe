# vpipe

[![CI](https://github.com/mewhhaha/vpipe/actions/workflows/ci.yml/badge.svg)](https://github.com/mewhhaha/vpipe/actions/workflows/ci.yml)

**Type-safe, stream-oriented GPU programming for Haskell on Vulkan 1.3.**

vpipe keeps the compelling part of GPipe—the ability to describe graphics as
typed Haskell data flow—while replacing the OpenGL runtime with direct SPIR-V
generation, explicit compute, and managed Vulkan synchronization.

![Triangle, textured cube, offscreen vignette, and compute particles rendered by vpipe](docs/assets/vpipe-demo.gif)

- Shader stages, vertex layouts, interpolation, formats, and resource usages
  are visible in types.
- Pipelines compile directly to deterministic SPIR-V. No GLSL toolchain or
  runtime shader compiler is required.
- Contexts, descriptors, image layouts, queue hand-offs, swapchains, and
  lifetimes are managed as one coherent runtime.
- Graphics and compute share typed buffers and synchronization state, so a
  compute dispatch can feed a later draw safely within an ordered frame.

vpipe 0.1 is currently pre-release with an intentionally bounded desktop API.
It targets Vulkan 1.3 and supports GHC 9.12.4 and 9.14.1.

## See it run

Install the platform packages listed under [Requirements](#requirements), then
build from a checkout:

```console
git clone https://github.com/mewhhaha/vpipe.git
cd vpipe
cabal update
cabal build all
VPIPE_TEST_DEVICE=any cabal run mandelbrot
```

`VPIPE_TEST_DEVICE=any` avoids making the Khronos validation layer strict, but
the example still needs an available Vulkan 1.3 device. The animated
Mandelbrot also accepts arrow or A/D keys for panning and W/S for zooming.

The repository includes these maintained programs:

| Example | What it demonstrates | Run it |
| --- | --- | --- |
| [Triangle](examples/app/triangle/Main.hs) | Minimal typed graphics pipeline and presentation | `cabal run triangle` |
| [Mandelbrot](examples/src/Vpipe/Examples/Mandelbrot.hs) | Shader loops, real-time uniforms, and keyboard input | `cabal run mandelbrot` |
| [Plasma](examples/src/Vpipe/Examples/Plasma.hs) and [rings](examples/src/Vpipe/Examples/Rings.hs) | Time-driven full-screen fragment shaders | `cabal run plasma` / `cabal run rings` |
| [Cube](examples/src/Vpipe/Examples/Cube.hs) | Indexed textured geometry with depth | `cabal run cube` |
| [Particles](examples/src/Vpipe/Examples/Particles.hs) | 100,000-particle compute-to-graphics hand-off | `cabal run particles` |
| [Offscreen](examples/src/Vpipe/Examples/Offscreen.hs) | Two-pass rendering and image-layout transitions | `cabal run offscreen` |
| [Headless](examples/app/headless/Main.hs) | Render and save a PNG without a window | `cabal run headless -- --screenshot /tmp/vpipe.png` |

Browse the [animated shader gallery](examples/shaders/README.md), or work
through the [five-part example guide](examples/guide/README.md).

## The programming model

A pipeline describes streams and targets once. This is the complete pipeline
from the maintained [triangle source](examples/app/triangle/Main.hs):

```haskell
pipeline :: PipelineM Environment ()
pipeline = do
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
```

The `Environment` is the boundary between that reusable description and the
resources for one draw. Its buffer and image binding types let vpipe reject
incompatible usages or formats before Vulkan records a command. The complete
example also shows pipeline preparation, swapchain creation, ordered frame
recording, and presentation.

Start with [Your first triangle](vpipe/docs/tutorials/first-triangle.md), then
continue with:

- [Buffers, textures, and the type system](vpipe/docs/tutorials/buffers-textures-and-types.md)
- [Compute](vpipe/docs/tutorials/compute.md)
- [Coming from GPipe](vpipe/docs/tutorials/coming-from-gpipe.md)
- [Coming from raw Vulkan](vpipe/docs/tutorials/coming-from-raw-vulkan.md)

## Using the packages

Headless and core graphics applications depend only on `vpipe`. Add
`vpipe-glfw` when you need managed GLFW windows and Vulkan presentation
surfaces:

```cabal
build-depends:
  vpipe >=0.1 && <0.2,
  vpipe-glfw >=0.1 && <0.2
```

`Vpipe` is a documentation-only landing module. Applications import capability
modules such as `Vpipe.Context`, `Vpipe.Pipeline`, `Vpipe.Expr`,
`Vpipe.Compute`, and `Vpipe.Frame` directly. Vector and matrix values come from
`linear`, so shader-facing math types also interoperate with the wider Haskell
ecosystem.

## Requirements

Building needs GHC 9.12.4 or 9.14.1, Cabal, a Vulkan loader and headers, and a
Vulkan 1.3 driver. Windowed programs additionally need GLFW. The full Vulkan
SDK is optional.

```console
# Debian / Ubuntu
sudo apt install libglfw3-dev libvulkan-dev mesa-vulkan-drivers \
  spirv-tools vulkan-validationlayers

# Fedora
sudo dnf install glfw-devel mesa-vulkan-drivers spirv-tools \
  vulkan-loader-devel vulkan-validation-layers

# Arch Linux
sudo pacman -S glfw spirv-tools vulkan-headers vulkan-icd-loader \
  vulkan-swrast vulkan-validation-layers
```

Mesa lavapipe is sufficient when no physical GPU is available. Validation
behavior is selected with `VPIPE_TEST_DEVICE`:

- `any` runs device tests when a suitable device exists without making
  validation-layer availability strict; examples still require a device.
- `lavapipe` requires a CPU Vulkan device plus validation and synchronization
  validation; this is the Linux CI configuration.
- `skip` runs the pure, property, compile-fail, interface, and SPIR-V tests
  without initializing Vulkan; it does not make GPU examples device-free.

## Current 0.1 scope

The release covers typed graphics and compute pipelines, vertex/index/uniform/
storage buffers, color and depth targets, sampled images, push constants,
integer atomics, ordered multipass frames, headless operation, and optional
GLFW presentation.

It does not currently claim Android, ray tracing, mesh shaders, stencil,
instanced or indirect drawing, indirect compute dispatch, storage images,
subgroup operations, shared workgroup memory or barriers, or a frame graph.
These boundaries are explicit so applications do not have to discover them
halfway through a design.

## Development and release confidence

```console
cabal build all
VPIPE_TEST_DEVICE=skip cabal test all
fourmolu --mode check .
hlint vpipe/src vpipe/public-src vpipe/test vpipe-glfw/src examples
```

Linux CI runs the full suite and maintained examples on pinned Mesa lavapipe
with validation and synchronization validation enabled. Windows builds and
runs software-Vulkan tests when lavapipe is available; macOS is currently
build-only. Physical Wayland and Windows runs remain release gates. The
[release checklist](docs/release-checklist.md) and
[candidate verifier](scripts/release/verify-candidate.sh) define the evidence
required before publishing the jointly versioned `vpipe` and `vpipe-glfw`
packages.

Architecture rationale lives in the [design records](docs/decisions/), while
the [project plan](tasks/00-summary.md) retains implementation history for
contributors.

## License and acknowledgements

vpipe is [MIT licensed](LICENSE). It is inspired by the MIT-licensed GPipe
project and by the BSD-3-Clause typed SPIR-V work in `fir`. It borrows ideas,
not source code; no code from either project is included. See the
[changelog](CHANGELOG.md) or report a problem in the
[issue tracker](https://github.com/mewhhaha/vpipe/issues).
