# vpipe

**Type-safe, stream-oriented GPU programming for Haskell on Vulkan 1.3.**

vpipe brings GPipe's typed data-flow model to Vulkan with direct SPIR-V
generation, first-class compute, and a runtime that owns descriptors, image
layouts, synchronization, swapchains, and resource lifetimes.

![Triangle, textured cube, offscreen vignette, and compute particles rendered by vpipe](docs/assets/vpipe-demo.gif)

- Types describe shader stages, vertex layouts, interpolation, formats, and
  legal resource usages.
- Pipelines compile directly to deterministic SPIR-V without a GLSL toolchain.
- Graphics and compute share typed buffers and synchronization state within an
  ordered frame.
- The core package has no window-system dependency; presentation lives in the
  separate `vpipe-glfw` package.

vpipe 0.1 targets desktop Vulkan 1.3 and supports GHC 9.12.4 and 9.14.1.

## Add it to an application

Headless applications need only `vpipe`. Add `vpipe-glfw` for managed windows,
surfaces, and presentation:

```cabal
build-depends:
  vpipe >=0.1 && <0.2,
  vpipe-glfw >=0.1 && <0.2
```

`Vpipe` is a documentation-only landing module. Applications import capability
modules such as `Vpipe.Context`, `Vpipe.Pipeline`, `Vpipe.Expr`,
`Vpipe.Compute`, and `Vpipe.Frame` directly. Shader math uses the standard
`linear` vector and matrix types.

## The programming model

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

`PipelineM Environment ()` describes reusable GPU work. The environment
supplies typed buffer and image bindings for one draw. vpipe compiles the
description to SPIR-V, resolves its resources, derives the required barriers,
and records the frame in program order.

Read [Your first triangle](docs/tutorials/first-triangle.md), then continue
with:

- [Buffers, textures, and the type system](docs/tutorials/buffers-textures-and-types.md)
- [Compute](docs/tutorials/compute.md)
- [Coming from GPipe](docs/tutorials/coming-from-gpipe.md)
- [Coming from raw Vulkan](docs/tutorials/coming-from-raw-vulkan.md)

The repository also contains a runnable
[five-part guide](https://github.com/mewhhaha/vpipe/tree/main/examples/guide)
and an animated
[shader gallery](https://github.com/mewhhaha/vpipe/tree/main/examples/shaders)
with Mandelbrot, plasma, and interference-ring examples.

## Requirements and scope

Building needs a Vulkan loader and headers plus a Vulkan 1.3 driver. The full
Vulkan SDK is optional. Mesa lavapipe is sufficient for headless or CI use.
Windowed applications additionally need GLFW through `vpipe-glfw`.

The 0.1 API covers typed graphics and compute pipelines, vertex/index/uniform/
storage buffers, color and depth targets, sampled images, push constants,
integer atomics, ordered multipass frames, and compute-to-graphics hand-offs.

It does not currently claim Android, ray tracing, mesh shaders, stencil,
instanced or indirect drawing, indirect compute dispatch, storage images,
subgroup operations, shared workgroup memory or barriers, or a frame graph.

## License and acknowledgements

vpipe is MIT licensed. It is inspired by the MIT-licensed GPipe project and by
the BSD-3-Clause typed SPIR-V work in `fir`. It borrows ideas, not source code;
no code from either project is included.
