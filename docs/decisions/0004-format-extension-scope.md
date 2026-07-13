# ADR 0004: Keep format extensions explicit and additive

Status: accepted

## Current scope

`Vpipe.Format` intentionally exposes a small closed set of 8-bit normalized,
32-bit floating-point, and depth formats. `Vpipe.Buffer.Format` likewise
models 32-bit scalar layouts plus `linear` vectors and matrices. Buffer
products currently have a `BufferFormat` instance for binary tuples; nested
pairs can describe larger products, while arbitrary tuple arities are not a
separate v1 promise. Generic records are the preferred public shape for
named blocks.

## Extension plan

Half-float and packed formats, such as RGB10A2, remain out of v1. They must
not be represented by pretending that an existing `Float` or vector layout
has a smaller or packed ABI. A half-float addition needs a distinct host
representation and matching layout, marshalling, shader-type, SPIR-V, and
device-feature decisions. A packed addition needs an explicit packed
representation and conversion contract rather than a normal vector layout.

Adding a promoted image or vertex format must update its Vulkan reflection,
component and texel families, renderability constraints, image transfer
representation, and any applicable vertex or buffer representation. This
keeps new formats additive while requiring every affected type-level and
runtime boundary to agree.
