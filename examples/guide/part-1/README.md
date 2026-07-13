# Part 1: The triangle

[Run the source](Main.hs) to draw one triangle. This first part introduces the
shape of every vpipe graphics program without hiding it behind a framework.

## Context and resources

`withExampleContext` creates the Vulkan context. `newPositions` then allocates
a buffer whose type records that it may be used as a vertex buffer:

```haskell
Buffer '[ 'Buffer.Vertex] (V3 Float)
```

The usage and element layout are therefore checked before Vulkan sees the
resource. The three positions are ordinary `linear` values written once on the
CPU.

## Pipeline and streams

`pipeline` describes GPU work independently of a particular window or image.
`vertexInput` turns the bound positions into a primitive stream. Mapping
`vertex` over that stream supplies a clip-space position and a smoothly
interpolated color for every vertex.

`rasterize` converts the primitive stream to fragments. `drawColor` writes
those fragments to the color target supplied by `Environment`.

## Drawing and presentation

`prepare` compiles the description and creates the Vulkan pipeline once. The
window path acquires a swapchain image with `frame`, records the draw with
`renderTo`, and presents it. The screenshot path binds an offscreen image to
the same generic pipeline instead.

Run it from the repository root:

```console
VPIPE_TEST_DEVICE=any cabal run guide-part-1
```

Next: [Part 2 — Buffers and indexing](../part-2/README.md).

