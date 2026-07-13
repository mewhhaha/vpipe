# 09 — Descriptor sets & pipeline layout

**Depends on:** 05, 07, 08
**Milestone:** M3

## Goal

Connect pipeline-declared resources (uniforms, textures, storage buffers) to
runtime values with full type safety and zero user-visible descriptor
management. This is where Vulkan is most alien to GPipe's OpenGL heritage,
and the design here is vpipe's biggest novelty.

## Design

- From task 05, compiling a `PipelineM env` records every `uniform`,
  `texture`, `storageBuffer` call with the accessor `env -> _` it captured.
  Codegen assigns set/binding deterministically (set 0 = per-pipeline
  stable resources, set 1 = per-draw, decided by declaration site — start
  with everything in set 0 and split when profiling M3 says so).
- The **environment type is the layout**. `env` is an ordinary user record;
  vpipe derives nothing from it generically — instead the accessors passed
  to `uniform`/`texture` fully determine the interface. This dodges the
  type-level-list ergonomics problem GPipe never had (OpenGL had no
  descriptor sets) and keeps inference friendly.
  - **Prototype A (chosen baseline):** accessor-capture as above; descriptor
    writes computed per draw by comparing captured values against a
    per-pipeline cache (hash of handles) — rewrite only when changed.
  - **Prototype B (evaluate, then decide):** full type-level layout
    (`Pipeline layout env` with `layout :: [Binding]`) enabling compile-time
    compatibility checks between pipelines sharing sets. Build a spike,
    measure error-message quality, then pick. Record the decision here.
- Runtime: descriptor storage is scoped to one frame slot and one prepared
  pipeline layout. Each slot/layout pair starts with a 64-set pool chunk;
  allocation grows that pair with larger chunks, and all of its chunks reset
  only after the owning slot retires. Sets are allocated and written on
  demand, while sampled image views and samplers remain dynamic descriptor
  values. `descriptorIndexing` and `update-after-bind` are deferred, rather
  than selected as a v1 optimization; see
  [ADR 0005](../docs/decisions/0005-descriptor-and-compute-scope.md) and the
  pool-growth coverage in
  [`Vpipe.DescriptorTest`](../vpipe/test/Vpipe/DescriptorTest.hs).
- Push constants: `pushConstant :: (env -> a) -> PipelineM env (Expr s a)`
  for small per-draw data (≤128 bytes enforced at compile time via task 02's
  size family), used by examples for model matrices.

## Deliverables

- Descriptor allocation/caching runtime; pipeline-layout builder from
  recorded interfaces; the `uniform`/`texture`/`storageBuffer`/
  `pushConstant` user API surfaced through task 05's monad.
- Decision record: Prototype A vs B, with the spike code linked.
  - Decision: [ADR 0001 — capture environment accessors](../docs/decisions/0001-descriptor-environment-accessors.md), with the rejected [Prototype B spike](../experiments/type-level-descriptor-layout/PrototypeB.hs).
- Tests: interface-recording goldens; descriptor-cache hit/miss behavior;
  lavapipe test binding two uniforms + texture + push constant and reading
  back the expected image.

## Acceptance criteria

- Changing one uniform buffer between draws re-writes at most one
  descriptor set; unchanged draws re-write none (assert via internal
  counters in tests).
- Binding a buffer lacking `Uniform` usage is a compile error.

## Open questions

- Aliasing rule: when captured accessors resolve a concrete environment, the
  same runtime buffer cannot supply both Storage and Uniform bindings. v1
  rejects that resolved alias before descriptor allocation; relaxing the rule
  needs an access model and validation coverage. See
  [ADR 0005](../docs/decisions/0005-descriptor-and-compute-scope.md) and
  [`Vpipe.PipelineTest`](../vpipe/test/Vpipe/PipelineTest.hs).
