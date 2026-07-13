# 10 — Graphics pipeline compilation & caching

**Depends on:** 04, 09
**Milestone:** M1 (hardcoded), M2 (from EDSL)

## Goal

Turn a `CompiledPipeline env` (SPIR-V + recorded interfaces + static state)
into `VkPipeline` objects, with caching that makes the "compile once, render
many" contract real.

## Design

- Dynamic rendering only (`VK_KHR_dynamic_rendering` core in 1.3): pipelines
  declare attachment *formats*, not render passes. Formats come from the
  target types in `env` — but `VkPipeline` needs them at creation, so
  pipeline creation is deferred to first use against concrete targets, then
  cached keyed by `(pipeline identity, attachment formats, sample count)`.
  In practice one pipeline is used with one target shape, so this is a
  cache with one entry — but resizing/swapchain-recreate stays free.
- Dynamic state: viewport/scissor always dynamic (no recompiles on resize).
  Everything else (blend, depth-compare, cull, topology) static in v1 —
  measure before adding `extendedDynamicState`.
- Blending/depth/raster settings: plain Haskell records with sane defaults
  (`defaultRaster { cullMode = CullBack }`) supplied at `rasterize`/`draw*`
  declaration sites in task 05's monad; typed against attachment formats
  where Vulkan constrains them (e.g. blending on float targets only —
  `Blendable f` constraint from task 02).
- `VkPipelineCache` persisted to disk (XDG cache dir, keyed by driver
  UUID) — free startup wins.
- Deterministic codegen (task 04) + interface hashing gives us a *shader*
  level dedupe too: identical pipelines in different call sites share one
  `VkShaderModule`.
- Pass recording: `render :: CompiledPipeline env -> env -> Pass ()` where
  `Pass` is the per-frame recording monad (task 11 owns it). Recording emits
  auto-barriers from task 07/08 state tracking, begins/ends
  `vkCmdBeginRendering` scopes, binds pipeline + descriptors + vertex
  buffers, draws.

## Steps

1. M1: hardcoded SPIR-V blob → `VkPipeline` → triangle to headless target.
   This lands *before* tasks 04/05/09 finish, using raw internals.
2. M2: swap the blob for task 04 output; delete the blob.
3. Cache layer + disk persistence + swapchain-recreate stress test.

## Acceptance criteria

- Window resize does not create pipelines (assert via counters).
- Two draws of the same pipeline shape share modules and pipeline.
- Zero validation messages throughout.

## Open questions

- Pipeline derivatives / libraries (`graphicsPipelineLibrary`): ignore; the
  cache design above makes them unnecessary at our scale.
