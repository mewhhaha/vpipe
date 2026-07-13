# Part 4: Textures

[Part 3](../part-3/README.md) passed colors through the rasterizer. [This
part](Main.hs) passes texture coordinates and uses them to sample an image in
the fragment stage.

## Fragment streams and interpolation

Each full-screen vertex contains a clip-space position and a texture
coordinate. Wrapping the coordinate in `Smooth` asks the rasterizer to
interpolate it across the triangle. After `rasterize`, the pipeline maps a
sampling expression over the resulting fragment stream.

## Images and samplers

The checkerboard is an `R8G8B8A8Unorm` image whose type records both its format
and the `Sampled` and `CopyDst` usages. `writeImage` uploads pixels through the
copy usage. A sampler separately describes how coordinates are converted into
texels, and `typedTextureBinding` combines the image and sampler for the
pipeline.

`sampledTexture` makes that binding available to the shader. `Expr.sample`
uses the interpolated two-dimensional coordinate and returns the color type
associated with the image format. The format-to-value relationship is checked
at compile time.

Run it from the repository root:

```console
VPIPE_TEST_DEVICE=any cabal run guide-part-4
```

Next: [Part 5 — Targets and multiple passes](../part-5/README.md).

