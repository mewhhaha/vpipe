# vpipe

vpipe is a type-safe GPU programming library for Haskell: the stream-oriented
programming model of GPipe, rebuilt around Vulkan 1.3, direct SPIR-V generation,
and first-class compute. The [project plan](tasks/00-summary.md) records the
architecture, milestones, and implementation decisions.

The project currently targets desktop Vulkan 1.3. Android, ray tracing, and mesh
shaders are outside the 0.1 scope.

The [diagnostics review](docs/diagnostic-review.md) records the current
beginner-error paths, their fixes, and the regression coverage that preserves
them.

The design records are concise statements of the deliberate boundaries:
[descriptor environments](docs/decisions/0001-descriptor-environment-accessors.md),
[expression reification](docs/decisions/0002-expression-stablename-reification.md),
[deferred resource capabilities](docs/decisions/0003-resource-capability-deferrals.md),
[format extensions](docs/decisions/0004-format-extension-scope.md),
[descriptor and compute scope](docs/decisions/0005-descriptor-and-compute-scope.md), and
[shader interface scope](docs/decisions/0006-shader-interface-scope.md), and
[capability-module imports](docs/decisions/0007-capability-module-imports.md).

![Triangle, textured cube, offscreen vignette, and compute particles rendered by vpipe](docs/assets/vpipe-demo.gif)

Its four design pillars are:

- Haskell types describe shader stages, formats, resource usages, and layouts.
- Pipelines compile directly to deterministic SPIR-V without a GLSL toolchain.
- Managed lifetimes, descriptors, layouts, and synchronization replace raw
  Vulkan bookkeeping.
- Graphics and compute share one resource model, including tracked
  compute-to-graphics hand-offs.

vpipe uses the `linear` package for `V2`, `V3`, `V4`, and matrix values. Those
types already interoperate with the wider Haskell graphics ecosystem; vpipe
adds Vulkan-format and buffer-layout classes instead of maintaining a second,
incompatible math vocabulary.

## The triangle

A pipeline describes streams and targets once; the frame loop supplies the
current swapchain image and records ordered passes:

```haskell
triangle :: PipelineM Environment ()
triangle = do
  input <- vertexInput (vertexSource "positions" positions)
  fragments <- rasterize defaultRaster (fmap vertex input)
  drawColor defaultBlend (colorTarget "color" target) (fmap unSmooth fragments)

drawFrame swapchain prepared vertices =
  frame swapchain $ \current ->
    renderTo (frameColorTarget current) $
      render prepared (Environment vertices (frameColorTarget current))
```

The maintained example remains below roughly 60 lines while handling a real
GLFW window, typed vertex data, compilation, resize, and presentation.  Follow
[Your first triangle](vpipe/docs/tutorials/first-triangle.md) for the complete
runnable program and an explanation of the pipeline, environment, and frame
model.

## Development environment

GHC 9.12.4 and 9.14.1 are the supported compiler series. Building needs a Vulkan
loader and headers; the full Vulkan SDK is optional. Tests that touch a device need an
installable Vulkan ICD. Mesa's lavapipe software ICD is sufficient when no GPU
is available. Install the Vulkan loader/headers, validation layers, SPIR-V
tools, and software driver with the command for your distribution:

```console
# Debian / Ubuntu
sudo apt install libvulkan-dev mesa-vulkan-drivers spirv-tools vulkan-validationlayers

# Fedora
sudo dnf install mesa-vulkan-drivers spirv-tools vulkan-loader-devel vulkan-validation-layers

# Arch Linux
sudo pacman -S spirv-tools vulkan-headers vulkan-icd-loader vulkan-swrast vulkan-validation-layers
```

Choose device-test behavior with `VPIPE_TEST_DEVICE`:

- `lavapipe` requires a CPU Vulkan device and strict validation; absence is a
  failure, as it is in Linux CI.
- `any` runs device tests when a suitable Vulkan 1.3 device is available and
  otherwise falls back to the pure suite.
- `skip` never initializes Vulkan and runs only pure/property, compile-fail,
  interface, and SPIR-V tests.

Run the local checks with:

```console
cabal build all
cabal test all
VPIPE_TEST_DEVICE=skip cabal test all
fourmolu --mode check .
hlint vpipe/src vpipe/public-src vpipe/test vpipe-glfw/src examples
```

The Vulkan instance smoke test enables `VK_LAYER_KHRONOS_validation` when the
layer is installed and skips only when no Vulkan ICD is available.

## Acknowledgements

vpipe is inspired by the MIT-licensed GPipe project and by the BSD-3-Clause
typed SPIR-V work in `fir`. It borrows ideas, not source code; no code from
either project is included.
