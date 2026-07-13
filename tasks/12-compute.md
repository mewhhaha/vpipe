# 12 — Compute

**Depends on:** 04, 09
**Milestone:** M4

## Goal

Compute shaders in the same EDSL and resource model — the headline feature
GPipe never had.

## Design

- `ComputeM env a`: sibling of `PipelineM` (task 05). Body gets invocation
  context as expressions:
  ```haskell
  particles :: ComputeM env ()
  particles = do
    buf <- storageBuffer (view #particles)      -- read/write, Expr C access
    dt  <- pushConstant (view #dt)
    gid <- globalInvocationId
    let i = gid ^. _x
    whenInBounds buf i $ \p -> writeAt buf i (step dt p)
  ```
- Storage buffer access is the new EDSL ground: `readAt :: StorageBuf a -> Expr C Word32 -> Expr C a`,
  `writeAt`, plus atomics (`atomicAdd` on `Word32`/`Int32` first). This
  forces task 03's tree to grow statement/effect nodes — design them
  as an ordered effect list per entry point (codegen already has the
  structured-CFG machinery from task 04).
- Workgroup size: type-level `Dispatch (x :: Nat) (y :: Nat) (z :: Nat)`
  on the compiled compute pipeline; `dispatch :: CompiledCompute env -> env -> (Int,Int,Int) -> Pass ()`
  takes workgroup *counts*; helper computes counts from element totals.
- Shared memory + barriers: `workgroupShared :: forall a. ...` and
  `workgroupBarrier` — include in v1 only if the effect-node design makes
  it natural; otherwise first minor release after M5. Record decision.
- Compute↔graphics interop: storage buffer written by compute, consumed as
  vertex input — barrier inference across queue/pass boundaries via
  task 07's state tracking; same-queue in v1 (dedicated compute queue use
  is an internal scheduling upgrade later).

## Deliverables

- `Vpipe.Compute`; EDSL effect nodes + codegen (`GlobalInvocationId`
  builtin, `OpAtomicIAdd`, storage access chains).
- Tests: prefix-sum or saxpy on lavapipe with CPU-reference comparison;
  atomics contention test; the M4 particle example.

## Acceptance criteria

- M4 example: compute updates 100k particles, graphics draws them, single
  queue, zero validation messages, correct under validation's sync checks
  (enable `VK_LAYER_KHRONOS_validation` sync validation for this test).

## Open questions

- Indirect dispatch/draw (`dispatchIndirect`, GPU-driven): post-M5.
- Subgroup ops: post-M5, gated on a capability query API.
