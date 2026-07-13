# Changelog

All notable changes to vpipe and vpipe-glfw are recorded here. The project uses
the Haskell Package Versioning Policy.

## 0.1.0.0 — unreleased

- Added a typed graphics pipeline DSL with direct SPIR-V generation, dynamic
  rendering, static raster/depth/blend state, descriptor caching, and managed
  graphics-pipeline caches.
- Added typed buffers, dynamic upload rings, images, samplers, mip generation,
  resource-state tracking, and synchronization2 queue hand-offs.
- Added first-class compute with reflected storage buffers, push constants,
  runtime arrays, conditional writes, integer atomics, dispatch-limit checks,
  and compute-to-graphics buffer interoperability.
- Added Vulkan 1.3 context selection, strict validation and synchronization
  validation, structured logs, timeline queues, managed lifetimes, surfaces,
  GLFW windows, and swapchain generation management.
- Added compile-fail diagnostic fixtures, property and golden SPIR-V tests,
  lavapipe device tests, multi-platform CI, nightly fuzz/property runs, and a
  non-publishing Hackage candidate verification workflow.
- Split optional GLFW integration into the jointly versioned `vpipe-glfw`
  package.
