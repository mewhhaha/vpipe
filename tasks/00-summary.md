# vpipe — Plan Summary

A typesafe, ergonomic GPU programming library for Haskell, in the spirit of
[GPipe-Core](https://github.com/tobbebex/GPipe-Core), rebuilt for the current
century: Vulkan 1.3+ instead of OpenGL 3.3, SPIR-V generation instead of GLSL
strings, compute as a first-class citizen, and modern GHC (GHC2024) throughout.

## Why redo GPipe?

GPipe 2 got the *model* right and the *target* is now obsolete:

- **What GPipe got right** — shaders are ordinary Haskell values. Vertex
  streams (`PrimitiveStream`) and fragment streams (`FragmentStream`) are
  `Functor`s you `fmap` over; the `Shader` monad composes stages; the
  expression EDSL (`S x a`) means a shader that typechecks cannot reference a
  vertex attribute or uniform that isn't bound. Buffer layout is derived from
  types (`BufferFormat`), so CPU↔GPU marshalling can't be mismatched.
- **What aged badly** — OpenGL 3.3 target, GLSL-source codegen, implicit
  global context, no compute shaders, no explicit control over memory or
  synchronization, single-threaded context ownership, and the `os` phantom
  parameter design tied to OpenGL context-sharing semantics.

vpipe keeps the model and replaces the machinery.

## Design pillars

1. **Vulkan 1.3 baseline.** Dynamic rendering (no render-pass/framebuffer
   objects), synchronization2, and timeline semaphores. Descriptor indexing is
   deferred until it can remain an internal optimization. This slashes API
   surface and matches every 2020s desktop driver
   (and lavapipe/MoltenVK for CI/macOS).
2. **Typed EDSL compiled straight to SPIR-V.** No GLSL strings, no runtime
   `shaderc`/`glslang` dependency. We own a small SPIR-V assembler. Prior art
   to study: `fir` (Sam Derbyshire's indexed-monad SPIR-V EDSL) — we borrow
   ideas, not the dependency, because vpipe's EDSL must stay GPipe-flavoured
   (streams + `fmap`) rather than monadic shader bodies.
3. **Invalid pipelines are unrepresentable.** Vertex format ↔ vertex shader
   input, shader interface ↔ descriptor set layout, fragment output ↔
   attachment format: all connected by type families so a mismatch is a
   compile error, not a validation-layer message at runtime.
4. **Explicit where it pays, automatic where it doesn't.** Users see frames,
   passes, buffers, images. They do not see barriers, image layout
   transitions, or descriptor pool management — those are derived from
   tracked resource usage.
5. **Compute is first-class**, same EDSL, same buffer types.
6. **Deterministic resource lifetimes** via `resourcet`-style scoping now,
   with the API shaped so linear types can be adopted later without breakage.

## Package layout (single repo, multi-package)

- `vpipe` — core library: EDSL, SPIR-V codegen, Vulkan runtime, buffers,
  images, pipelines, compute.
- `vpipe-glfw` — windowing/surface integration (mirrors GPipe-GLFW). Kept
  separate so headless/compute users don't link GLFW.
- Examples live in the repo as executables, not on Hackage.

## Architecture at 10,000 ft

```
 user code
   │  Shader monad, PrimitiveStream/FragmentStream/ComputeInvocation, Expr EDSL
   ▼
 staging: typed AST (Expr) + interface descriptions (vertex/descriptor/attachment)
   │  codegen                         │  reflection
   ▼                                  ▼
 SPIR-V binary                 VkPipelineLayout / vertex input / formats
   └──────────────┬───────────────────┘
                  ▼
 runtime: context, VMA memory, pipeline cache, frame loop,
          auto-barriers, descriptor allocation, swapchain
```

## Task index

Order is roughly dependency order; tasks 03–05 and 06–08 can proceed in
parallel streams (EDSL/codegen vs. Vulkan runtime) after 01–02.

| # | Task | Depends on |
|---|------|-----------|
| 01 | [Scaffolding & toolchain](01-scaffolding.md) | — |
| 02 | [Math types & format type families](02-math-and-formats.md) | 01 |
| 03 | [Expression EDSL](03-expression-edsl.md) | 02 |
| 04 | [SPIR-V code generation](04-spirv-codegen.md) | 03 |
| 05 | [Shader monad & streams](05-shader-monad-streams.md) | 03, 04 |
| 06 | [Vulkan context & devices](06-vulkan-context.md) | 01 |
| 07 | [Buffers & memory](07-buffers-memory.md) | 02, 06 |
| 08 | [Images & samplers](08-images-samplers.md) | 07 |
| 09 | [Descriptor sets & pipeline layout](09-descriptors-pipeline-layout.md) | 05, 07, 08 |
| 10 | [Graphics pipeline compilation](10-graphics-pipeline.md) | 04, 09 |
| 11 | [Swapchain & frame loop](11-swapchain-frame-loop.md) | 06, 10 |
| 12 | [Compute](12-compute.md) | 04, 09 |
| 13 | [Windowing package (vpipe-glfw)](13-windowing.md) | 11 |
| 14 | [Diagnostics & error story](14-diagnostics.md) | 06 (ongoing) |
| 15 | [Examples](15-examples.md) | 11, 12, 13 |
| 16 | [Testing & CI](16-testing-ci.md) | 01 (ongoing) |
| 17 | [Docs & release](17-docs-release.md) | everything |

## Milestones

- **M0 — skeleton builds** (task 01): multi-package cabal project, CI green,
  `vulkan` bindings link, validation layers on.
- **M1 — hardcoded triangle** (06, 07, 10, 11, 13): triangle on screen using a
  *pre-assembled* SPIR-V blob. Proves the entire runtime path before the EDSL
  exists. This is the de-risking milestone.
- **M2 — EDSL triangle** (02–05): same triangle, but the shaders are Haskell.
  Delete the SPIR-V blob. This is the moment vpipe exists.
- **M3 — real rendering** (08, 09): textured, uniform-driven, depth-tested
  spinning cube; multiple frames in flight.
- **M4 — compute** (12): particle sim writing a storage buffer consumed by the
  graphics pipeline.
- **M5 — ship it** (14–17): docs, examples, golden tests, Hackage candidate.

## Key decisions already taken (revisit only with cause)

- **Vulkan 1.3 minimum.** No 1.0/1.1 fallback paths; MoltenVK 1.3 support and
  lavapipe make this safe.
- **Own SPIR-V assembler, not `fir`, not shaderc.** Control over the
  instruction stream is the whole point; SPIR-V is a small, stable, versioned
  binary format that is *easier* to emit than GLSL is to print correctly.
- **`vulkan` (expipiplus1) bindings + `VulkanMemoryAllocator`.** The only
  maintained, complete bindings; verify current versions in task 01.
- **No `os` phantom.** Vulkan has no context-sharing weirdness; a `Frame`/
  scoped-resource design replaces it.
- **GLFW first for windowing**, SDL2 later if demanded.

## Open questions (tracked in individual tasks)

- Descriptor layouts use captured environment accessors after evaluating the
  type-level-list prototype; see task 09 and
  [ADR 0001](../docs/decisions/0001-descriptor-environment-accessors.md).
- Linear-types opt-in layer (task 07, deferred).
- Frame-graph-style multi-pass API vs. simple ordered passes (task 11 starts
  simple; revisit post-M3).
- Ray tracing / mesh shaders: explicitly out of scope until after M5.
