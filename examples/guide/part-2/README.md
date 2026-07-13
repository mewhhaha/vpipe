# Part 2: Buffers and indexing

[Part 1](../part-1/README.md) supplied only positions. [This part](Main.hs)
stores a color beside each position and uses an index buffer to assemble two
triangles into a quad.

## Buffer layouts

The vertex buffer element is `(V2 Float, V4 Float)`. vpipe derives its input
layout from that Haskell type, so the shader receives a position and color with
the same structure. The buffer's usage list permits vertex reads, while the
index buffer's usage list permits indexed drawing:

```haskell
Buffer '[ 'Buffer.Vertex] (V2 Float, V4 Float)
Buffer '[ 'Buffer.Index] Word32
```

That distinction prevents accidentally binding one kind of resource where the
other is required.

## Indexed primitive assembly

Four vertices describe the corners. Six indices select them as two triangles,
allowing the shared corners to be reused. `rasterizeIndexed` combines the
primitive stream with the index source; the remainder of the pipeline is the
same map, rasterize, and draw flow introduced in Part 1.

GPipe's second guide also discusses instancing. vpipe 0.1 does not expose
instanced vertex arrays yet, so this part stops at the indexed path supported
by the current public API.

Run it from the repository root:

```console
VPIPE_TEST_DEVICE=any cabal run guide-part-2
```

Next: [Part 3 — Shader expressions](../part-3/README.md).

