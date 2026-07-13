# 13 — Windowing package (vpipe-glfw)

**Depends on:** 11
**Milestone:** M1

## Goal

A thin, separate package giving vpipe a window and surface via GLFW,
mirroring the GPipe/GPipe-GLFW split so the core stays headless-friendly.

## Design

- Depends on `GLFW-b` and `vpipe`; nothing in `vpipe` depends on it.
- API sketch:
  ```haskell
  withWindow :: Context -> WindowConfig -> (Window -> IO a) -> IO a
  windowSurface :: Window -> Surface          -- feeds task 11's swapchain
  pollEvents / windowShouldClose / getKey ... -- re-exported, GLFW-b flavored
  ```
- Surface creation via `glfwCreateWindowSurface` (GLFW-b exposes it; verify
  the binding exists in the current release, else FFI the one call
  ourselves — it's a single function).
- Instance-extension negotiation: GLFW reports required instance extensions
  *before* instance creation, so `vpipe-glfw` exports
  `requiredInstanceExtensions :: IO [ByteString]` and `VpipeConfig`
  (task 06) grows an `extraInstanceExtensions` field. Get this ordering
  right in the API: examples call vpipe-glfw first, then `withVpipe`.
- Event handling stays GLFW's — vpipe adds no input abstraction. Import the
  capability modules a program uses (for example `Vpipe.Context`) and
  `Vpipe.GLFW` as its only GLFW import. `Vpipe` remains documentation-only so
  capability names do not collide; [ADR 0007](../docs/decisions/0007-capability-module-imports.md)
  records this intentional exception to the original umbrella-import sketch.
- Main-thread constraint: GLFW event polling must happen on the main OS
  thread (macOS hard requirement). Document the canonical app skeleton
  (`main = runInBoundThread ...`); rendering may live on another thread.

## Deliverables

- `vpipe-glfw` package; M1 example switched from headless to windowed here.
- CI: Linux runs the GLFW integration test under Xvfb (X11), with the
  nightly workflow exercising the 10k-frame resize stress; see
  [CI](../.github/workflows/ci.yml),
  [nightly](../.github/workflows/nightly.yml), and
  [`vpipe-glfw`'s integration test](../vpipe-glfw/test/Main.hs). Physical
  Wayland and Windows runs remain explicit external evidence gates in the
  [release checklist](../docs/release-checklist.md), not claims satisfied by
  CI.

## Acceptance criteria

- M1 on-screen triangle runs on Linux (X11 + Wayland) and, best-effort,
  Windows; resize and close behave. Automated X11 coverage does not replace
  the physical Wayland and Windows release gates.

## Open questions

- SDL2 sibling package: post-M5, same `Surface` interface proves the
  abstraction.
