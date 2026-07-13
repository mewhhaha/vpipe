# 02 — Math types & format type families

**Depends on:** 01
**Milestone:** M2

## Goal

The vocabulary of the whole library: vector/matrix types usable both on the
CPU and inside shader expressions, and the type families that map Haskell
types to GPU memory layouts and Vulkan formats.

## Context (GPipe heritage)

GPipe used the `linear` package (`V2`,`V3`,`V4`,`M44`) plus a `BufferFormat`
class mapping host types to buffer representations (`B2 Float`, `B4 Float`,
…) and a `VertexInput` class mapping those to shader-side types. That
three-layer scheme (host ↔ buffer ↔ shader) is the load-bearing trick and we
keep it.

## Deliverables

- Decision: depend on `linear` for `V2/V3/V4/M22..M44` (recommended — huge
  ecosystem value) with vpipe-specific classes layered on top; document why
  if we diverge.
- `Vpipe.Format` module:
  - `data Format = R8Unorm | R8G8B8A8Srgb | R32G32B32A32Sfloat | D32Sfloat | ...`
    as a kind (promoted) *and* a runtime value, with singletons-style
    reflection (`KnownFormat f => formatVal :: VkFormat`). Hand-rolled
    reflection, not the singletons package.
  - Type families `FormatComponentType f`, `FormatChannels f`, and
    `ColorRenderable f`, `DepthRenderable f` constraints.
- `Vpipe.Buffer.Format`:
  - `class BufferFormat a` with associated type `HostFormat a`, alignment and
    size (type-level `Nat` where possible, value-level always), and poke/peek
    into pinned memory.
  - Instances for scalars, `V2/V3/V4` of them, matrices, tuples, and a
    deriving-via path for user records (`GHC.Generics`) so a plain
    `data Vertex = Vertex { pos :: V3 Float, uv :: V2 Float }` gets an
    instance with std430-compatible layout for free.
- Layout rules implemented and property-tested: scalar/base alignment for
  vertex buffers, std140 and std430 for uniform/storage blocks. These live in
  one module with one source of truth used by both the marshaller (this task)
  and SPIR-V decoration emission (task 04).

## Steps

1. Write the layout calculator as pure functions over a `TypeRep`-free
   description (`data FieldLayout = ...`), derived generically.
2. QuickCheck: for arbitrary nesting of supported types, offsets are aligned,
   total size is aligned, no overlaps; golden-test a handful against
   offsets printed by a reference GLSL compiler once (recorded fixtures, no
   runtime dependency).
3. Poke/peek round-trip property tests through a `ForeignPtr`.

## Acceptance criteria

- `deriving (BufferFormat) via Generically Vertex`-style derivation works for
  a record of vectors and scalars.
- std140 vs std430 differences (array stride, vec3 padding) are covered by
  tests.
- No `unsafeCoerce`, no overlapping instances.

## Open questions

- Half floats and packed formats (RGB10A2 etc.) are deferred without placeholder
  instances: each needs an honest host representation and matching ABI rather
  than a fake 32-bit layout. The existing closed reflection/layout boundaries
  make those additions explicit and additive; see
  [ADR 0004](../docs/decisions/0004-format-extension-scope.md).
