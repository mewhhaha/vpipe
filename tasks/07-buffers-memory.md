# 07 — Buffers & memory

**Depends on:** 02, 06
**Milestone:** M1 (raw), M3 (typed)

## Goal

Typed GPU buffers with automatic memory management: GPipe's
`Buffer os (B4 Float)` reborn as `Buffer usage a` on top of
VulkanMemoryAllocator.

## Design

- Memory: VMA via the `VulkanMemoryAllocator` package. Fallback plan if the
  bindings fight us (see task 01 risk): a simple own allocator (one
  device-local heap arena + one host-visible ring for staging) is acceptable
  for v1 — profile-guided sophistication later.
- `Buffer (us :: [Usage]) a` where `a` has a `BufferFormat` instance
  (task 02) and `us` is a type-level usage list (`Vertex`, `Index`,
  `Uniform`, `Storage`, `CopySrc`, `CopyDst`) reflected into
  `VkBufferUsageFlags`. Functions demand usages by constraint:
  `vertexInput` needs `HasUsage Vertex us`. This is the compile-time
  version of what Vulkan validates at runtime.
- Creation & data movement:
  - `newBuffer :: BufferFormat a => Context -> Int -> IO (Buffer us a)`
  - `writeBuffer :: Buffer us a -> Int -> [HostFormat a] -> IO ()` — via a
    persistent staging ring and the transfer queue; synchronization handled
    by timeline semaphores, invisible to the caller.
  - `readBuffer` for CopySrc buffers (test/debug path).
  - Host-visible (BAR / ReBAR) fast path for uniforms updated per frame:
    `newDynamicBuffer` mapped persistently, N copies for frames-in-flight,
    handled transparently by the frame loop (task 11).
- Index buffers: `Buffer us Word32` + a newtype for restart semantics.
- Resource states for auto-barriers live in the internal
  `Vpipe.Buffer.State` transactional tracker. Each operation reserves the
  buffer's last use through STM, records against the reservation's previous
  use, then commits its stage, access, queue family, and completion timeline
  after submission (or cancels the reservation on failure). This serializes
  overlapping uses without exposing state-tracking handles in `Vpipe.Buffer`;
  see the reservation test in
  [`Vpipe.BufferTest`](../vpipe/test/Vpipe/BufferTest.hs).

## Deliverables

- `Vpipe.Buffer` public API; internal staging-ring and state-tracking
  modules.
- Tests (lavapipe): round-trip write→read for every `BufferFormat` instance;
  concurrent writes from two threads to two buffers; usage-constraint
  negative compile tests.

## Acceptance criteria

- M1 uses raw untyped buffer internals; M3 flips examples to the typed API.
- Zero validation messages under stress test (1000 buffers, interleaved
  upload/draw).

## Open questions

- Linear-types opt-in (`Buffer` as a linear resource) — design note only,
  post-M5.
- `bufferDeviceAddress`-based bindless: revisit with task 09's outcome.
