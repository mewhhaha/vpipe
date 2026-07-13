# ADR 0003: Defer linear buffers, buffer addresses, storage images, and BCn

Status: accepted

## Decision

V1 keeps ordinary, non-linear buffer handles and defers a linear-buffer API
until after M5. The opt-in must be additive: existing managed handles and
their explicit destruction semantics remain available, while a linear layer
can make ownership transfers visible at selected call sites.

V1 also does not expose `bufferDeviceAddress` or a bindless buffer API. The
current descriptor baseline is the accessor-based set-0 layout described in
[ADR 0001](0001-descriptor-environment-accessors.md). Device addresses would
change feature negotiation, descriptor/resource lifetime rules, and the
binding model, so they are reconsidered only with a concrete bindless design.

`ImageUsage` already has a `Storage` tag, but v1 does not claim a complete
storage-image feature: there is no public storage-image descriptor and EDSL
operation path. Storage buffers remain the supported compute resource.
Compressed BCn formats are likewise deferred. They are not constructors of
the current `Format` kind and have no upload, sampling, or format-feature
contract in the public API.

## Consequences

Any future storage-image or BCn work must be vertical rather than just a new
usage flag or format constructor: it needs Vulkan feature checks, resource
and view rules, descriptor reflection, shader operations where applicable,
and validation-backed tests. A buffer-address proposal must state its
ownership and descriptor-cache consequences before it changes the public API.
