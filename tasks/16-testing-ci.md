# 16 — Testing & CI

**Depends on:** 01 (infrastructure exists from day one; this task is the
strategy and its buildout)
**Milestone:** ongoing; complete at M5

## Goal

Confidence without owning a GPU farm: pure tests for everything with
semantics, lavapipe (Mesa's software Vulkan) for everything that needs a
device, golden artifacts for everything visual.

## Test pyramid

1. **Pure/property (no device):**
   - Layout calculator (task 02): alignment/offset properties + fixtures.
   - EDSL evaluator equivalences (task 03): `eval (a+b) == eval a + eval b`
     over generated trees; sharing-preservation checks.
   - SPIR-V assembler: section ordering, id uniqueness, deterministic
     output (byte-equality across runs).
   - Interface recorder (task 05): golden set/binding/location tables.
2. **Device tests (lavapipe in CI, real GPU locally):**
   - tasty option / env var `VPIPE_TEST_DEVICE=lavapipe|any|skip`.
   - Buffer/image round-trips, descriptor caching counters, swapchain-less
     render→readback goldens, compute vs CPU reference.
   - **Sync validation on** for the interop suites.
3. **Golden images (task 15's screenshot mode):** 8×8-to-64×64 fixtures,
   byte-exact where possible; store in-repo; regenerate via
   `cabal run regen-goldens` with a mandatory eyeball diff in PR review.
4. **Fuzzing:** random well-typed `Expr` trees → codegen → `spirv-val`;
   random layout descriptions → poke/peek round-trip. Nightly CI job, seeds
   logged for reproduction.

## CI matrix

- Linux (primary): two GHC versions × build+test+lint+format; lavapipe +
  SPIRV-Tools installed; nightly fuzz job.
- Windows: build + pure tests (lavapipe on Windows CI is possible —
  attempt it, don't block on it).
- macOS: build only until MoltenVK is wired (post-M5 decision).
- Artifact on failure: dump `VPIPE_DUMP` output + validation logs.

## Acceptance criteria

- A contributor with no GPU can run `cabal test all` meaningfully
  (lavapipe documented as a one-line install on major distros).
- CI catches: a layout change (golden), a codegen change (golden +
  spirv-val), a sync bug (sync validation), an ergonomic regression
  (triangle line count, type-error goldens from task 14).
