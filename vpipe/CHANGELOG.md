# Changelog

All notable changes to `vpipe` are recorded here. The project follows the
Haskell Package Versioning Policy.

## 0.1.0.0 — unreleased

- Added a typed stream-oriented graphics and compute EDSL with deterministic
  direct SPIR-V generation and no runtime shader compiler.
- Added Vulkan 1.3 context/device selection, strict validation, timeline
  queues, managed object lifetimes, VMA-backed buffers and images, descriptor
  allocation, pipeline caching, and synchronization2 resource tracking.
- Added dynamic-rendering graphics pipelines, typed attachments and sampled
  images, compute storage arrays and atomics, and ordered one-submit frames
  with swapchain recreation.
- Added direct whole-buffer `Word32` indexed draws to typed pipeline
  compilation and recording.
- Added actionable compile-time/runtime diagnostics, optional shader dumps,
  object debug names, pure/property/golden tests, and strict lavapipe device
  coverage.
