# 06 — Vulkan context & devices

**Depends on:** 01
**Milestone:** M1

## Goal

Instance, device, and queue management with good defaults and zero
boilerplate for the common case, while keeping every knob reachable.

## Design

- `withVpipe :: VpipeConfig -> (Context -> IO a) -> IO a` (bracketed;
  internally `resourcet` so sub-resources parent onto the context).
- `VpipeConfig` defaults: app name, Vulkan 1.3, validation layers **on** in
  dev builds (controlled by config, not CPP), debug-utils messenger routing
  to a pluggable logger (task 14), device selection = first discrete GPU
  that satisfies required features, overridable by a scoring callback.
- Required device features checked and enabled explicitly:
  `dynamicRendering`, `synchronization2`, `timelineSemaphore`,
  `bufferDeviceAddress` (optional at first), `descriptorIndexing` subset
  (optional at first). A clear error listing *which* missing feature ruled
  out *which* device (task 14 owns error text quality).
- Queues: one graphics+present queue, plus dedicated transfer and compute
  queues when the hardware has them (fall back to the graphics queue
  otherwise). Wrapped in a `Queue` handle that owns submission and a
  timeline-semaphore counter — all submission goes through vpipe so
  synchronization stays inferable.
- Headless mode: context without a surface for compute/tests — the CI path.
- Threading model: `Context` is thread-safe for resource creation; command
  recording happens on per-frame recorders (task 11). Document this
  contract precisely from day one.

## Deliverables

- `Vpipe.Context` with the above; smoke tests from task 01 promoted into
  real tests (device selection on lavapipe, feature negotiation, headless
  context).
- The M1 milestone driver: `examples/hardcoded-triangle` starts here — clear
  a headless render target and read pixels back, before swapchain work
  lands (task 11 upgrades it to a window).

## Acceptance criteria

- Headless context + clear + readback test passes under lavapipe in CI with
  validation layers on and **zero validation messages** (a validation
  message is a test failure — this policy starts now and never relaxes).

## Open questions

- Multi-device: out of scope, but don't bake singleton assumptions into
  handle types (handles carry their `Context`).
