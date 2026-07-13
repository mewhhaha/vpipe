# ADR 0006: Keep shader stage interfaces tuple-shaped

Status: accepted

## Decision

V1 stage interfaces remain nested tuples. `VertexInput` describes host vertex
formats and assigns vertex-attribute locations. `FragmentInput` describes the
vertex-to-fragment boundary, assigning varying locations and selecting
interpolation. `GenericVertex` is an ergonomic adapter for host records, but
its shader-side result is still a nested tuple of shader values.

V1 has no shared `ShaderInterface` class. Such a class would combine distinct
responsibilities: vertex-buffer decoding, the vertex-to-fragment stage
boundary, and pipeline resource environments. These have different locations,
validity rules, and lifetimes, so sharing one abstraction would obscure rather
than express their contracts.

Record-shaped varyings may be added later as an additive convenience over the
existing tuple representation. They must not derive descriptor or other
environment bindings: environment accessors remain the explicit pipeline
interface described by [ADR 0001](0001-descriptor-environment-accessors.md).

## Evidence

`tupleAndZipCase` exercises independently sourced vertex attributes as a
nested tuple, then carries a smooth and a flat varying across the stage
boundary at deterministic locations. `genericVertexCase` verifies that a host
record can be decomposed into vertex attributes with the expected offsets and
binding layout while the shader receives tuple-shaped values. The two paths
therefore share shader composition without pretending that host-record layout
and fragment interpolation are the same interface.

## Consequences

- Vertex formats and locations stay local to `VertexInput`.
- Varying locations and interpolation stay local to `FragmentInput`.
- Host records remain available through `GenericVertex` without imposing a
  record representation on shader expressions.
- Any future record-varying API must preserve the current tuple behavior and
  keep resource-environment bindings explicit.
