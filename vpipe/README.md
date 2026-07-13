# vpipe

vpipe is a type-safe GPU programming library for Haskell: the stream-oriented
programming model of GPipe, rebuilt around Vulkan 1.3, direct SPIR-V generation,
and first-class compute. The repository's project plan records the architecture,
milestones, and implementation decisions.

The project targets desktop Vulkan 1.3. Android, ray tracing, and mesh shaders
are outside the 0.1 scope.

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
GLFW window, typed vertex data, compilation, resize, and presentation. Follow
[Your first triangle](docs/tutorials/first-triangle.md) for the complete
runnable program and an explanation of the pipeline, environment, and frame
model.

The remaining tutorials cover
[buffers, textures, and the type system](docs/tutorials/buffers-textures-and-types.md),
[compute](docs/tutorials/compute.md), migration
[from GPipe](docs/tutorials/coming-from-gpipe.md), and migration
[from raw Vulkan](docs/tutorials/coming-from-raw-vulkan.md).

## Development environment

GHC 9.12.4 and 9.14.1 are the supported compiler series. Building needs a
Vulkan loader and headers; the full Vulkan SDK is optional. Device tests need
an installable Vulkan ICD, and Mesa's lavapipe software ICD is sufficient when
no GPU is available.

Choose device-test behavior with `VPIPE_TEST_DEVICE`:

- `lavapipe` requires a CPU Vulkan device and strict validation; absence is a
  failure, as it is in Linux CI.
- `any` runs device tests when a suitable Vulkan 1.3 device is available and
  otherwise falls back to the pure suite.
- `skip` never initializes Vulkan and runs only pure, property, compile-fail,
  interface, and SPIR-V tests.

## Acknowledgements

vpipe is inspired by the MIT-licensed GPipe project and by the BSD-3-Clause
typed SPIR-V work in `fir`. It borrows ideas, not source code; no code from
either project is included.
