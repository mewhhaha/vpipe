# GPipe for the Vulkan era

The appealing part of GPipe was never OpenGL. It was the idea that a graphics
pipeline could look like typed Haskell data flow: vertex streams in, fragment
streams out, ordinary records supplying resources, and the compiler rejecting
whole classes of mismatches.

vpipe carries that idea onto Vulkan 1.3.

The smallest triangle still describes positions, rasterization, and a color
output rather than command buffers, descriptor writes, image barriers, and
semaphore payloads. Underneath, vpipe emits SPIR-V directly, builds and caches
the Vulkan pipeline, resolves a typed environment, records synchronization2
barriers, and presents through a resize-safe frame loop. Those hidden pieces
are tested as ownership protocols, not collected as convenience wrappers.

Three choices shape the library:

1. **Streams remain the graphics vocabulary.** `PipelineM` records topology,
   interpolation, resources, static state, and ordered draws.
2. **Resource intent is typed.** `Buffer '[Vertex, Storage] (V4 Float)` and
   `Image 'D2 'R8G8B8A8Srgb '[ColorTarget]` say enough to reject illegal use
   early and derive the Vulkan flags later.
3. **Compute is not an add-on.** `ComputeM` shares buffers, descriptors,
   expression types, SPIR-V generation, and synchronization state with
   graphics. Ordered storage writes and integer atomics are explicit effects;
   pure expressions remain safe to share.

The direct SPIR-V backend is intentionally small and deterministic. Golden
word streams, property-generated typed expression forests, and `spirv-val`
cover it without relying on a driver GLSL compiler. Device tests run on Mesa
lavapipe with the validation and synchronization-validation layers enabled.

vpipe also takes failure paths seriously. Context shutdown waits for active
operations. Submitted transfers retain staging memory through cancellation.
Descriptor pools do not reset before their frame timeline completes.
Swapchain acquire and presentation semaphores have explicit owners. If GPU
completion becomes uncertain, vpipe keeps objects alive and poisons the scope
instead of gambling on an early free.

The 0.1 scope is deliberately desktop-sized: Vulkan 1.3, graphics, images,
typed descriptors, windowed presentation through a separate GLFW package, and
single-queue compute-to-graphics interoperation. Android, ray tracing, mesh
shaders, subgroups, indirect dispatch, and a frame graph are not claimed.

For GPipe users, there is no `os` parameter or `ContextT`; managed handles and
lifetime gates enforce the context boundary. For raw Vulkan users, the value
is not fewer struct literals by itself—it is one library owning the state that
must stay consistent across many calls.

Start with the [project overview](../tasks/00-summary.md), then read
[Your first triangle](../vpipe/docs/tutorials/first-triangle.md),
[Buffers, textures, and the type system](../vpipe/docs/tutorials/buffers-textures-and-types.md),
[Compute](../vpipe/docs/tutorials/compute.md),
[Coming from GPipe](../vpipe/docs/tutorials/coming-from-gpipe.md), or
[Coming from raw Vulkan](../vpipe/docs/tutorials/coming-from-raw-vulkan.md).

vpipe is MIT licensed. It is inspired by the MIT-licensed GPipe project and by
the BSD-3-Clause typed SPIR-V work in `fir`; it borrows ideas, not source code,
and includes no code from either project.
