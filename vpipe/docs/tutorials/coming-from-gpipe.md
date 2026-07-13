# Coming from GPipe

vpipe keeps GPipe's most useful idea: describe GPU work with typed Haskell
streams and ordinary environment accessors. It changes the runtime beneath
that model from OpenGL to Vulkan 1.3 and makes synchronization, resource roles,
and compute explicit in the library design.

## Concept map

| GPipe concept | vpipe concept | Important difference |
| --- | --- | --- |
| `Shader os env a` | `PipelineM env a` | Pipeline recording is pure in shape; concrete resources still come from `env`. |
| `VertexArray` / primitive arrays | `VertexSource` / `PrimitiveStream` | Topology is retained in the type and incompatible streams cannot be zipped. |
| `FragmentStream` | `FragmentStream` | Interpolation wrappers (`Smooth`, `Flat`, `NoPerspective`) are explicit and checked. |
| `Render` / `render` | ordered `Pass` values executed by `frame` | Vulkan command, descriptor, and synchronization ownership is scoped to a frame slot. |
| `Buffer` | `Buffer usages a` | The type records roles such as `Vertex`, `Uniform`, `Storage`, and `CopySrc`. |
| texture and render image objects | `Image dim format usages` | Dimension, format, mip/layer state, and legal roles are tracked together. |
| window context package | separate `vpipe-glfw` package | GLFW extensions and surfaces must be known before Vulkan device creation, so `withWindow` owns the whole ordering. |

The final `Frame` spelling is documented in the triangle tutorial alongside
the windowed example. Its design is intentionally an ordered pass list, not a
frame graph: program order is GPU order, and vpipe derives the necessary
barriers.

## There is no `os` parameter

GPipe's `os` parameter prevents values tied to one OpenGL context from escaping
its scope. vpipe instead uses opaque managed handles plus runtime ownership and
lifetime gates. A resource from another `Context`, a resource used after early
destruction, or a retained resource used after context shutdown is rejected
before Vulkan sees it.

This makes normal records simpler:

```haskell
data Scene = Scene
  { scenePositions :: VertexBuffer (V3 Float)
  , sceneCamera :: UniformBuffer CameraBlock
  , sceneTarget :: ColorImage 'B8G8R8A8Srgb
  }
```

There is also no `ContextT`. Use `withVpipe` for headless work or
`Vpipe.GLFW.withWindow` for a managed window, surface, and presentation-capable
context. Resource functions run in `IO`; context and resource cleanup remain
bracketed internally.

## What becomes stricter

- Buffer and image usage mistakes are compile-time errors with curated text.
- Vertex topology, attachment format, interpolation, and shader stage remain
  visible in types.
- Descriptor environments are resolved deterministically by recorded
  accessors; applications do not assign bindings by hand.
- SPIR-V is generated directly and validated in tests. There is no GLSL driver
  compiler in the normal path.
- Vulkan layout transitions and queue timeline dependencies are derived from
  committed resource state.

## What becomes possible

`ComputeM` is a sibling of `PipelineM`. It provides typed storage-buffer reads,
ordered writes, structured conditionals, `GlobalInvocationId`, and 32-bit
integer atomics. A buffer may carry both `Storage` and `Vertex` usages so a
compute dispatch can feed a later graphics draw through the same state tracker.

## Honest differences and current limits

vpipe 0.1 targets desktop Vulkan 1.3. It does not target OpenGL, Android, ray
tracing, mesh shaders, indirect GPU-driven dispatch, or subgroup operations.
The first compute scheduler uses one graphics-capable queue. Shared workgroup
memory is deliberately deferred until its memory and barrier semantics can be
added as one coherent feature.

There is no public raw-handle escape hatch for most objects. This is a current
boundary, not an omission hidden behind “unsafe” branding: exposing handles
without a protocol for updating vpipe's lifetime and resource-state trackers
would make later automatic barriers unsound.

The design rationale and milestone history live in the repository's project
plan.
