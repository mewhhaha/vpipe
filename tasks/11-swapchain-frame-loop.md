# 11 — Swapchain & frame loop

**Depends on:** 06, 10
**Milestone:** M1

## Goal

The render loop users actually write: acquire, record passes, present —
with frames-in-flight, resize, and synchronization handled invisibly and
*correctly* (this is where most hand-written Vulkan is subtly wrong).

## Design

- `Swapchain` handle owning `VkSwapchainKHR`, image views, and per-frame
  sync primitives; created from a `Surface` (task 13 supplies surfaces;
  headless targets from task 08 satisfy the same `Target` interface).
- Frame protocol:
  ```haskell
  frame :: Swapchain -> (Frame -> Pass ()) -> IO PresentResult
  ```
  `frame` handles: acquire (with timeout + OUT_OF_DATE → recreate loop),
  wait for this slot's previous timeline use, command buffer reset + record,
  submit with `VkSubmitInfo2`, present, bookkeeping for N frames in flight
  (N=2 default, configurable). Steady-state waits only the selected slot.
- `Pass ()` (introduced conceptually in task 10): a monad for recording
  ordered passes — `renderTo target (do render pipeA envA; render pipeB envB)`,
  `computePass ...` (task 12), `copyPass ...`. Ordering within a frame is
  program order; barriers derived from resource-state tracking. No frame
  graph in v1 — ordered passes cover everything until proven otherwise;
  the `Pass` type keeps the door open (record-then-optimize later without
  API change).
- Per-frame transient resources: dynamic uniform ring slots (task 07),
  descriptor pools (task 09), command pools — all cycled by slot index
  inside `frame`; user never sees "frames in flight" except as a config
  number and the rule "buffer writes via `writeBuffer` are safe any time"
  (staging ring + timeline semaphores make it so).
- Resize/recreate: fully internal — `PresentResult` tells the app it
  happened only because apps often want to know (projection matrices). During
  recreation, vpipe may retire every outstanding slot and wait for the
  present queue before destroying an old generation: core Vulkan has no
  portable present-completion primitive. This conservative path is distinct
  from the selected-slot wait in steady state; see
  [`Vpipe.SwapchainTest`](../vpipe/test/Vpipe/SwapchainTest.hs).
- Present modes: FIFO default; Mailbox/Immediate opt-in via config.

## Deliverables

- `Vpipe.Frame`, `Vpipe.Swapchain`; the M1 example upgraded from headless
  to an on-screen triangle (with task 13's surface).
- Stress tests: continuous resize while rendering through the automated Linux
  X11/GLFW integration path, including the 10k-frame nightly resize stress;
  physical Wayland and Windows remain explicit external release gates (see
  the [release checklist](../docs/release-checklist.md)).

## Acceptance criteria

- Triangle at uncapped frame rate with validation on: zero messages over
  10k frames including several recreates.
- CPU does not stall except at the intended frames-in-flight fence.

## Open questions

- Multi-window: one `Swapchain` each, shared `Context` — should fall out of
  the design; add a two-window test to confirm, don't engineer for it.
