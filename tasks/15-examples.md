# 15 — Examples

**Depends on:** 11, 12, 13
**Milestone:** each example gates its milestone

## Goal

Examples are the acceptance tests for ergonomics — each milestone's example
is written *against the API we wish we had*, and the API moves to meet it
(not vice versa). They live in `examples/` as one cabal package, built in CI
always, run manually.

## The set

1. **`triangle`** (M1→M2): the canonical minimal program. Target: under
   ~60 lines including imports at M2. This line count is a tracked metric —
   GPipe's equivalent was ~80; raw Vulkan is ~1000.
2. **`cube`** (M3): indexed draw, depth buffer, mipmapped texture, uniform
   MVP updated per frame, push-constant model matrix, window resize.
3. **`particles`** (M4): compute updates positions in a storage buffer;
   graphics renders as points with additive blending; demonstrates
   compute↔graphics interop and `writeBuffer` streaming for emitter state.
4. **`offscreen`** (M3+): render-to-texture then full-screen post pass
   (vignette in the fragment shader) — proves target/texture duality
   (task 08) and multi-pass ordering (task 11).
5. **`headless`** (M2+): no window, render one frame, write PNG
   (JuicyPixels dependency lives here only). Doubles as the readback
   documentation and the smoke test users run to verify their install.
6. **`shadertoy`** (M5, stretch): full-screen fragment shader with time/
   mouse uniforms — the "playground" entry point for new users.

## Standing rules

- Every example: `-Wall` clean, comments written as prose for the tutorial
  (task 17 lifts them), runs clean under validation.
- Each example has a `--frames N --screenshot out.png` mode so CI can
  actually run them headless on lavapipe and golden-compare screenshots
  (small tolerance; lavapipe is deterministic enough per-version — pin the
  lavapipe version in CI).

## Acceptance criteria

- All examples build in CI; `triangle`, `headless`, `cube`, `offscreen`,
  `particles` render golden-correct on lavapipe.
- Triangle line count reported in CI (fun, but it genuinely guards
  ergonomic regressions).
