# Animated shader gallery

These examples use one full-screen pipeline and a small uniform buffer for
interactive inputs. Windowed runs receive real elapsed time; screenshot runs
use a deterministic clock so their output can be regression tested.

Run any shader from the repository root:

```console
VPIPE_TEST_DEVICE=any cabal run mandelbrot
VPIPE_TEST_DEVICE=any cabal run plasma
VPIPE_TEST_DEVICE=any cabal run rings
VPIPE_TEST_DEVICE=any cabal run shadertoy
```

If `VK_LAYER_KHRONOS_validation` is not installed, keep
`VPIPE_TEST_DEVICE=any`. Strict `lavapipe` mode requires that layer.

## Mandelbrot

`mandelbrot` evaluates 56 orbit iterations per fragment with `Expr.whileE`.
Time continuously shifts the color palette. Hold the arrow keys or A/D to pan
and W/S to zoom while the window is focused.

## Plasma

`plasma` combines four moving sine waves, including a radial wave. The phase
is mapped into three offset color channels, producing a continuously folding
field.

## Interference rings

`rings` moves three wave sources along different orbits. Their circular waves
interfere while a colored glow tracks each source.

## Shadertoy playground

`shadertoy` remains the smallest place to experiment. Its time input moves a
glowing point through three independently animated color bands.

Every shader also supports a bounded run and an offscreen screenshot:

```console
VPIPE_TEST_DEVICE=any cabal run mandelbrot -- --frames 120
VPIPE_TEST_DEVICE=any cabal run plasma -- \
  --frames 2 --screenshot /tmp/vpipe-plasma.png
```
