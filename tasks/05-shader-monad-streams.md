# 05 — Shader monad & streams

**Depends on:** 03, 04
**Milestone:** M2

## Goal

The user-facing composition layer: a `Pipeline` description monad in which
you pull in vertex streams, transform them with `fmap`, rasterize, shade
fragments, and declare outputs — GPipe's `Shader` monad modernized.

## Design

GPipe's shape, renamed and re-plumbed for Vulkan:

```haskell
-- working sketch, names to be bikeshedded in review
trianglePipe :: PipelineM env ()
trianglePipe = do
  verts :: PrimitiveStream Triangles (Expr V (V3 Float, V3 Float))
        <- vertexInput (view #vertexBuffer)          -- typed against env
  mvp   <- uniform (view #mvpBuffer)
  let clip = fmap (\(p, c) -> (mvp !* point p, c)) verts
  frags <- rasterize (view #rasterSettings) clip     -- interpolation by type
  drawColor (view #target) (fmap shadePixel frags)
```

- `PipelineM env a`: reader-ish monad over an *environment type* `env` — the
  runtime values (which buffer, which texture, viewport) are not available at
  pipeline-build time; the pipeline is compiled **once** against accessor
  functions `env -> _`, then rendered many times with concrete `env` values.
  This is GPipe's `Shader os s a` / `CompiledShader` split and it maps
  perfectly onto Vulkan's pipeline-vs-descriptor split: compile-time choices
  (formats, topology, blending) live in the monad; per-draw choices live in
  `env`.
- `PrimitiveStream t a`: `Functor`; `t` is topology (`Points`, `Lines`,
  `Triangles`) tracked at type level into `VkPrimitiveTopology`.
- `rasterize` consumes `Expr V`-typed vertex output whose first component is
  clip position, produces `FragmentStream (Expr F ...)`; interpolation
  qualifiers (smooth/flat/noperspective) chosen per-component via a
  `FragmentInput` class (GPipe's trick: the type determines interpolability;
  integers force `flat`). V1 stage interfaces are nested tuples:
  `VertexInput` owns vertex formats and locations, while `FragmentInput` owns
  varying interpolation and locations. See [ADR 0006](../docs/decisions/0006-shader-interface-scope.md).
- Side-effect-ish fragment ops: `discardWhen :: BoolE F -> FragmentStream a -> FragmentStream a`,
  depth output override, `drawColor`/`drawDepth` declare attachments whose
  formats come from `env`'s target types (ties into tasks 08/10).
- **Compilation:** running the monad performs a *dry run* that records the
  interface (vertex attributes used, uniforms/textures touched with their
  set/binding assignment, attachments written) and hands each stage's `Expr`
  roots to task 04 codegen. Output: a `CompiledPipeline env` — SPIR-V
  modules + `VkPipeline`-ready static state + a render closure.
- Multiple entry points in one `PipelineM` (GPipe allowed several draw calls
  per shader): support N draws / N vertex inputs; each `vertexInput` +
  `draw*` pair becomes one Vulkan pipeline under the hood, compiled together.

## Deliverables

- `Vpipe.Pipeline` (monad, streams, rasterize, draw), interface-recording
  interpreter, and `CompiledPipeline`.
- `FragmentInput`/`VertexInput` classes with generic derivation aligned with
  task 02; `GenericVertex` adapts host records to tuple-shaped shader values.
- Unit tests running the recorder over representative pipelines and checking
  the extracted interfaces (no GPU needed).

## Acceptance criteria

- M2 triangle written in this API end to end.
- A pipeline referencing two uniforms and one texture records the correct
  set/binding/location tables, stable across runs.
- Type errors (wrong topology, fragment expr in vertex position, missing
  interpolation instance) are comprehensible — add custom `TypeError`
  instances where they aren't.

## Open questions

- Geometry/tessellation stages: not in v1 (dynamic rendering + modern
  guidance says skip geometry shaders anyway).
- Transform feedback equivalents: superseded by compute (task 12).
