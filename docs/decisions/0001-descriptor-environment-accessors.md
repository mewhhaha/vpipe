# ADR 0001: Capture environment accessors for descriptor layouts

Status: accepted

Task 09 evaluated two ways to describe a pipeline's descriptor interface:
captured accessors (Prototype A) and a promoted list of bindings on the
pipeline type (Prototype B). The runnable type-level sketch is
[`PrototypeB.hs`](../../experiments/type-level-descriptor-layout/PrototypeB.hs).

## Decision

Use Prototype A. Calls such as `uniform`, `sampledTexture`, and
`storageBuffer` record a stable set-0 binding and retain the supplied
`env -> resource` accessor. At render time the binding plan resolves those
accessors, rejects a buffer aliased as both uniform and storage, and uses the
ordered raw-handle tuple as the per-frame descriptor-cache key. The rejection
happens before descriptor allocation or command recording; v1 does not try to
infer whether a storage binding is read-only in a particular shader.

Push constants follow the same model: their declaration order determines
offsets, while a separate resolver marshals the current environment values.

## Evidence

The Prototype B spike can express compatibility between two promoted layout
lists, but every user environment needs a recursive `ResolveBinding` instance
for each entry. Missing bindings are reported through that recursion, and
ordinary record updates become coupled to type-level ordering or additional
name lookup machinery. Prototype A instead reports usage errors at the
resource conversion site; for example, the compile-fail test for binding a
`CopySrc`-only buffer points directly at the missing `Uniform` usage.

Both prototypes can assign deterministic bindings. Prototype B therefore did
not buy a capability required by v1, while it added layout parameters to
`Pipeline`, more annotations at call sites, and a second representation that
still had to be converted into Vulkan writes.

## Consequences

- The environment remains an ordinary user record with no generic instance.
- Descriptor compatibility between independently compiled pipelines is a
  runtime/layout-cache concern, not a public type equality.
- Set 0 is declaration-order stable. A future per-draw set split can remain an
  internal optimization without changing environment types.
- Cache misses allocate and update one descriptor set; identical resolved
  handles perform no descriptor writes.
- Sampler and descriptor-pool scope choices are recorded in
  [ADR 0005](0005-descriptor-and-compute-scope.md).
