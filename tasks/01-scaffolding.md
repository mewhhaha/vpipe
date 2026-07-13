# 01 — Scaffolding & toolchain

**Depends on:** nothing
**Milestone:** M0

## Goal

Turn the `cabal init` skeleton into a multi-package project that builds
against the Vulkan bindings on a current GHC, with formatting, linting, and CI
decided up front so they never become archaeology.

## Deliverables

- `cabal.project` with packages `vpipe/` and `vpipe-glfw/` (the current
  flat layout moves into `vpipe/`; `exe/Main.lhs` and `MyLib` are deleted).
- `vpipe.cabal` rewritten: public sublibraries or exposed module tree under
  `Vpipe.*` (working names: `Vpipe`, `Vpipe.Expr`, `Vpipe.SpirV`,
  `Vpipe.Context`, `Vpipe.Buffer`, `Vpipe.Image`, `Vpipe.Pipeline`,
  `Vpipe.Compute`), test suite wired to a real framework.
- Dependency versions pinned and verified against Hackage **at execution
  time** (this plan was written offline):
  - `vulkan` (expipiplus1 bindings, 3.x line)
  - `VulkanMemoryAllocator`
  - `resourcet`
  - `vector`, `bytestring`, `containers`, `text`
  - test: `tasty`, `tasty-hunit`, `tasty-golden`, `tasty-quickcheck` (or
    hspec equivalents — pick one, stick with it)
  - `vulkan-utils` and `unliftio` were evaluated but are intentionally not
    declared while no source uses them; see
    [ADR 0005](../docs/decisions/0005-descriptor-and-compute-scope.md).
- Tooling config committed: `fourmolu.yaml` (or ormolu), `.hlint.yaml`,
  `.editorconfig`.
- GitHub Actions workflow: build + test on Linux with two GHC versions
  (current stable, previous stable); Vulkan SDK / lavapipe installed in the
  job so device-level tests can run headless later (see task 16).
- `README.md` stub stating the project vision (one paragraph, link to
  `tasks/00-summary.md`).

## Steps

1. Restructure directories: `vpipe/src`, `vpipe/test`, `vpipe-glfw/src`,
   `examples/` (cabal package with several executables, buildable but empty
   `main = pure ()` stubs for now).
2. Decide GHC floor: whatever GHC ships in the two most recent stable series
   at execution time; `default-language: GHC2024`; enable
   `-Wall -Wcompat -Wunused-packages` in a shared `common` stanza, plus the
   extensions the EDSL needs (`DataKinds`, `TypeFamilies`, `GADTs`,
   `UndecidableInstances` where justified) per-module rather than globally.
3. Add the `vulkan` dependency and write one smoke test: create a
   `VkInstance` with validation layers if available, enumerate physical
   devices, destroy it. Guard it so machines without a Vulkan ICD skip
   instead of fail.
4. Stand up CI; cache cabal store; ensure the smoke test passes on lavapipe
   in the runner.
5. Document the dev-environment requirements (Vulkan SDK optional, ICD
   required for tests) in `README.md`.

## Acceptance criteria

- `cabal build all && cabal test all` green locally and in CI.
- Instance-creation smoke test passes under lavapipe in CI.
- `fourmolu --mode check` and `hlint` run in CI and pass.

## Notes / risks

- `vulkan` bindings generate huge modules; first compile is slow. Consider
  `--ghc-options=-j` guidance and CI caching from day one.
- If `VulkanMemoryAllocator` bindings have bit-rotted against the current
  `vulkan` release, note it here and budget for vendoring or writing a thin
  allocator of our own (task 07 has a fallback design).
