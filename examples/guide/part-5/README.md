# Part 5: Targets and multiple passes

[Part 4](../part-4/README.md) sampled an image uploaded by the CPU. [This
part](Main.hs) first renders an image on the GPU, then samples that result in a
second pipeline.

## Color and depth targets

The first environment binds an `R8G8B8A8Unorm` color image and a `D32Sfloat`
depth image. Their types record the permitted target usages and the pipeline
checks that each shader output matches its target format. `drawColor` and
`drawDepth` write the same visible fragments to the two attachments.

`discardWhen` filters fragments outside a circle before either draw. This is a
shader-side filter: rejected fragments do not update color or depth.

## Offscreen rendering

The intermediate color image has both `ColorTarget` and `Sampled` usages. The
first pipeline renders into it; the second binds it with a sampler and draws it
to the current swapchain or screenshot image. The frame commands establish
that order, while vpipe tracks the required image-layout transitions.

GPipe's fifth guide also covers stencil operations. Stencil targets are not in
vpipe 0.1's public format scope, so this part demonstrates the supported depth,
filtering, and multipass paths without suggesting otherwise.

Run it from the repository root:

```console
VPIPE_TEST_DEVICE=any cabal run guide-part-5
```

Return to the [guide index](../README.md).

