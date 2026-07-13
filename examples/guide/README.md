# The vpipe guide

This five-part guide follows the learning progression of the
[GPipe-Core tutorial series](https://github.com/tobbebex/GPipe-Core#readme),
translated to vpipe's Vulkan resource model and current public API. Each part
is a complete executable, and every part can render either to a window or to a
PNG.

## Before you start

Build and run the first part from the repository root:

```console
VPIPE_TEST_DEVICE=any cabal run guide-part-1
```

The `any` setting uses an available Vulkan 1.3 device without requiring the
Khronos validation layer. Use it if a previous example reported that
`VK_LAYER_KHRONOS_validation` is not installed. For strict validation, install
the validation layer for your distribution and use `VPIPE_TEST_DEVICE=lavapipe`.

Every windowed example accepts `--frames N`, which is useful for a quick check:

```console
VPIPE_TEST_DEVICE=any cabal run guide-part-1 -- --frames 1
```

You can also render without a window and save the result:

```console
VPIPE_TEST_DEVICE=any cabal run guide-part-1 -- \
  --frames 1 --screenshot /tmp/vpipe-guide-part-1.png
```

## Parts

| Part | Result | Main ideas |
| --- | --- | --- |
| [1. The triangle](part-1/README.md) | A colored triangle | Contexts, pipelines, rasterization, targets, and presentation |
| [2. Buffers and indexing](part-2/README.md) | An indexed colored quad | Typed vertex and index buffers, layouts, and primitive assembly |
| [3. Shader expressions](part-3/README.md) | A rotating triangle | Lifted values, primitive streams, uniforms, composition, and errors |
| [4. Textures](part-4/README.md) | A sampled checkerboard | Fragment streams, images, samplers, interpolation, and sampling |
| [5. Targets and multiple passes](part-5/README.md) | A filtered, offscreen-rendered circle | Color and depth targets, fragment discard, and multipass rendering |

The programs are cumulative in subject matter, but they do not share source
between parts. You can copy any one into a small experiment without first
reconstructing the earlier examples.

