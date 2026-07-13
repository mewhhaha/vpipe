# ADR 0005: Keep descriptor binding dynamic and defer advanced compute controls

Status: accepted

## Decision

Sampled-texture descriptors receive both the image view and sampler from the
resolved environment. Samplers are therefore dynamic descriptor values, not
immutable samplers baked into a descriptor-set layout. This is a deliberate
v1 divergence from task 09's initial immutable-sampler sketch: applications
can select sampler state through their ordinary environment without creating a
second pipeline layout for each choice.

Descriptor storage belongs to a frame slot *and* one prepared pipeline layout.
Each such descriptor frame starts with a pool chunk sized for 64 sets. When a
chunk fills, allocation adds a larger chunk; all chunks are reset together
only after that frame slot's GPU timeline has completed. A cache entry is thus
valid only for its descriptor frame and layout. This is the concrete form of
the planned growable per-frame pool, rather than one shared global pool.

V1 does not request or expose descriptor indexing, update-after-bind, bindless
arrays, or dynamic descriptor indices. They remain a possible internal
optimization only after their feature negotiation, allocation, update, and
lifetime rules have a design that preserves the current API.

The resolver rejects one runtime buffer handle appearing as both a uniform and
a storage binding before descriptor allocation. Storage may be written by the
shader, and v1 has no per-binding access proof strong enough to make this
aliasing safe to accept.

Compute v1 includes storage buffers and atomics, but defers workgroup shared
memory and workgroup barriers. They must arrive together with explicit memory
scope and ordering semantics. Indirect dispatch/draw and subgroup operations
are also post-0.1: indirect work needs a GPU-driven scheduling and
synchronization contract, while subgroup operations require a public
capability-query API and a portability policy.

The task-01 dependency inventory named `vulkan-utils` and `unliftio` as
possibilities. They are not library dependencies because the current source
does not use them; adding unused dependencies would enlarge the supported
surface without supplying behavior.

## Consequences

- Changing sampler state can reuse a compiled pipeline, but changes the
  resolved descriptor cache key and may require a descriptor write.
- Descriptor pressure is isolated between layouts and frame slots. A busy
  layout grows only its own chunks, which are retained until that slot can be
  safely reset.
- Programs needing bindless resources, shared-memory algorithms, GPU-driven
  indirect submission, or subgroup collectives must use another path until a
  later release defines those capabilities.
- The uniform/storage alias rule is intentionally conservative. Relaxing it
  requires a resource-access model and validation coverage, not merely
  removing the resolver error.
