# Coming from raw Vulkan

vpipe is not a thin collection of Vulkan helpers. It owns the repetitive state
whose correctness spans API calls: device requirements, object lifetimes,
descriptors, resource layouts, queue timelines, and presentation generations.
The result is a smaller API, with fewer raw escape hatches than a binding such
as `vulkan`.

## What vpipe automates

### Instance, device, and queues

`withVpipe` creates a Vulkan 1.3 instance, selects a device that satisfies the
required features and queues, creates one managed queue wrapper per family,
and enables validation and synchronization validation when requested. Rejected
devices retain their names and disqualifying reasons for diagnostics.

Window integrations use a `SurfaceFactory` because platform extensions and
surface handles must exist before physical-device presentation support can be
selected. `vpipe-glfw` packages that ordering as `withWindow`/`withWindows`.

### Memory and transfers

Buffers and images use VMA-backed device-local allocations. Uploads and
readbacks use staging storage and timeline semaphores. An application specifies
element ranges or image subresources; it does not choose memory types, flush
mapped ranges, or retain temporary command pools.

### Descriptors and pipelines

`PipelineM` records resource accessors, shader interfaces, push-constant
ranges, static raster/depth/blend state, and draws. Preparation creates exact
descriptor and pipeline layouts. Descriptor pools and sets are frame-scoped,
and resources bound into them remain leased until GPU completion.

Graphics and compute pipelines are cached by complete Vulkan-visible keys,
not hash equality alone. The native pipeline cache is persisted using the
device UUID and an atomic file replacement.

### Synchronization and layouts

Each buffer and image subresource has transactional last-use state. Recording
reserves that state, derives synchronization2 barriers and external timeline
dependencies, and commits the new state only after submission is accepted.
Cancellation before submission cancels the reservation. Uncertain completion
poisons the owning prepared object and transfers cleanup to the context rather
than freeing possibly in-use Vulkan objects.

Within a frame, ordered passes supply the dependency order. The library does
not infer a global frame graph or reorder work in 0.1.

### Presentation

A `Swapchain` chooses a supported sRGB surface format, present mode, extent,
and image count; owns image views and binary semaphores; and serializes
acquire, submission, presentation, resize, and destruction. Binary semaphores
are separated by ownership: image-available per frame slot and
render-finished per swapchain image. Out-of-date generations are recreated
without exposing their handles to application code.

## What remains visible

- Resource roles and formats are part of Haskell types.
- Pipeline environments are ordinary records containing typed bindings.
- Present-mode preference and frames in flight are configuration choices.
- Compute workgroup dimensions are type-level `Dispatch x y z` values;
  workgroup counts are runtime values checked against device limits.
- Ordered pass structure remains application code, so synchronization follows
  an obvious program order.

## Escape-hatch policy

The low-level `vulkan` package remains available to applications, but vpipe
does not currently expose a supported way to inject arbitrary commands into a
managed frame. Doing so would require declaring every touched resource, stage,
access mask, layout, and lifetime. A future escape hatch must update the same
state transaction as built-in passes; a naked `VkCommandBuffer` callback would
not be sound.

Low-level window integrations can implement `SurfaceFactory` through
`Vpipe.Surface.Driver`. Device selection has configurable scoring and
requirements in `VpipeConfig`. Structured validation messages are delivered
through its logger. These are supported extension points because they preserve
the ownership protocol.

## Debugging

Run tests with Mesa lavapipe when no hardware ICD is available:

```console
sudo apt-get install libvulkan-dev mesa-vulkan-drivers vulkan-validationlayers spirv-tools
VPIPE_TEST_DEVICE=lavapipe cabal test all --test-show-details=direct
```

Strict mode treats every validation message as a failure. SPIR-V modules are
also checked with `spirv-val` when installed. Device loss reports a RenderDoc
capture hint; surface loss asks the caller to recreate its surface/window
scope. The diagnostics tutorial documents pipeline dumps and object names as
those hooks land.
