# 08 — Images & samplers

**Depends on:** 07
**Milestone:** M3

## Goal

Typed textures and render targets: GPipe's `Texture2D os (Format RGBFloat)`
equivalent, with Vulkan image layouts fully hidden.

## Design

- `Image (dim :: Dim) (f :: Format) (us :: [ImageUsage])` — dimensionality
  (`D1 | D2 | D3 | Cube | D2Array`), promoted format from task 02, usage
  list (`Sampled`, `ColorTarget`, `DepthTarget`, `Storage`, `CopySrc/Dst`)
  reflected to `VkImageUsageFlags` — same pattern as task 07.
- Mip levels & array layers as runtime naturals carried in the handle;
  `generateMips` via blit chain on the graphics queue.
- Upload: `writeImage` through the staging ring (JuicyPixels interop helper
  in examples only, not a core dependency).
- Layout tracking is private per-subresource state; passes transition lazily
  with `VkImageMemoryBarrier2`, and neither layouts nor tracker handles are
  exposed to users. Debug assertion mode logs every transition (task 14); see
  the transition-logging test in
  [`Vpipe.ImageTest`](../vpipe/test/Vpipe/ImageTest.hs).
- `Sampler` is a separate, context-cached object (filter, address modes,
  anisotropy, compare-op for shadow samplers), keyed by its description.
  Resolved image views and samplers are dynamic descriptor values, rather
  than immutable samplers baked into a descriptor-set layout; see
  [ADR 0005](../docs/decisions/0005-descriptor-and-compute-scope.md).
- Shader access (ties to tasks 03/09):
  `sample :: SampledImage D2 f -> Expr F (V2 Float) -> Expr F (TexelOf f)` —
  the texel type is computed from the format (`TexelOf R8G8B8A8Srgb = V4 Float`,
  `TexelOf D32Sfloat = Float`), so sampling a depth texture as color is a
  type error. Integer formats yield integer expression types.
- Render targets: `ColorTarget f` / `DepthTarget f` wrappers used by
  task 10's attachment declarations; swapchain images (task 11) satisfy the
  same interface, so rendering to texture vs screen is the same code.

## Deliverables

- `Vpipe.Image`, `Vpipe.Sampler`; readback helpers for tests.
- Tests: upload→sample→render→readback golden images on lavapipe (tiny 8×8
  fixtures, byte-exact); mip generation correctness (checkerboard downsample
  reference); layout tracker unit tests (pure).

## Acceptance criteria

- M3 cube samples a mipmapped texture with zero validation messages.
- Depth-format misuse fails to compile (negative test).

## Open questions

- Storage images are deferred for v1: `ImageUsage Storage` alone is not a
  public storage-image descriptor or EDSL feature. See
  [ADR 0003](../docs/decisions/0003-resource-capability-deferrals.md).
- Compressed formats (BCn): post-M5.
