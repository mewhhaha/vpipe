# 14 — Diagnostics & error story

**Depends on:** 06 (starts there, ongoing through all tasks)
**Milestone:** M5 polish, enforced throughout

## Goal

The difference between "typesafe and ergonomic" and "typesafe and hostile"
is error quality. This task is a standing workstream with concrete
deliverables, not a cleanup pass.

## Workstreams

1. **Type-error curation.** Every public type-level mechanism (usage lists,
   format constraints, stage phantoms, interpolation classes) gets custom
   `TypeError` messages written for the *user's* vocabulary
   ("this buffer was created without the Vertex usage; add it to the type's
   usage list" — not an unreduced type-family printout). Maintain
   a `test/type-errors/` suite asserting message text golden-style
   (deferred-type-errors + expected substrings) so refactors can't silently
   regress them.
2. **Validation-layer integration.** Debug-utils messenger (task 06) routes
   to a structured logger; vpipe object handles get `OBJECT_NAME` debug
   names from user-supplied or derived labels, so validation messages and
   RenderDoc captures say `"buffer:particle-positions"` not `0x7f3a...`.
   Policy from task 06 restated: in vpipe's own CI, any validation message
   is a failure.
3. **Runtime exceptions.** One exception hierarchy (`VpipeError`) with
   dedicated constructors for the realistic failures: no suitable device
   (lists devices and the disqualifying feature each lacked), surface lost,
   device lost (with a "capture with RenderDoc and retry" hint), shader
   compile bugs (internal — message says "this is a vpipe bug, file it"
   plus the SPIR-V disassembly path it dumped).
4. **Debug dumping.** `VPIPE_DUMP=dir` environment hook: every compiled
   pipeline writes SPIR-V (+ disassembly when spirv-dis is present) and its
   interface tables to `dir`. Cheap, invaluable.
5. **Doc-driven ergonomics review.** After M2 and M3, write the tutorial
   *first* (task 17), and every place the prose needs an apology becomes an
   issue in this task. The current source-and-fixture review is tracked in
   [Diagnostics review](../docs/diagnostic-review.md); its local items are
   deliberately not claims of external issue reports or user testing.

## Acceptance criteria

- The five most likely beginner mistakes (missing usage, wrong stage,
  format mismatch, forgot vpipe-glfw extensions, no ICD present) each
  produce a message that names the fix. Tested.
- All examples run clean under full validation + sync validation.
