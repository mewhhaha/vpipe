# 17 — Documentation & release

**Depends on:** everything; tutorial drafts start at M2
**Milestone:** M5

## Goal

Ship something people can adopt: tutorial-first documentation, honest
comparison positioning, and a clean Hackage release train.

## Deliverables

1. **Tutorial series** (in-repo markdown, lifted from example comments):
   - "Your first triangle" (M2 example, walks the whole mental model:
     pipeline monad, streams, env, frame loop).
   - "Buffers, textures, and the type system" (M3).
   - "Compute" (M4).
   - "Coming from GPipe" — explicit migration notes: renamed concepts
     (`Shader`→`PipelineM`, `render`/`Render`→`Pass`, no `os`, no
     `ContextT`), what's better, what's missing.
   - "Coming from raw Vulkan" — what vpipe automates (sync, layouts,
     descriptors) and the escape hatches for each.
2. **Haddock**: every public module has a prose header with a runnable
   snippet; `cabal haddock` warnings are CI failures at M5.
3. **README**: the triangle, a GIF, the pillar list, honest scope
   ("desktop Vulkan 1.3; no Android/ray-tracing yet").
4. **Release engineering:**
   - PVP versioning; `vpipe` and `vpipe-glfw` released together, 0.1.0.0.
   - CHANGELOG discipline from M0 (replace the cabal-init stub).
   - Hackage candidate first; solicit review (Haskell Discourse, r/haskell,
     GPipe users) before the real upload.
   - License check: MIT (current LICENSE) — confirm all borrowed *ideas*
     (fir, GPipe) involve no borrowed *code*, or attribute properly if any
     is ported (GPipe is MIT; fir is BSD-3 — attribution section in README
     either way as a courtesy).
5. **Announcement post** draft: "GPipe for the Vulkan era" — the summary
   document (00) rewritten as prose with the triangle as the hook.

## Acceptance criteria

- A Haskeller who has never used Vulkan gets a triangle from the tutorial
  in under 30 minutes on a stock Linux box (test this on a friend).
- Hackage candidate docs build; all links resolve; examples referenced in
  docs are CI-built.
