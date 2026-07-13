# Part 3: Shader expressions

[Part 2](../part-2/README.md) focused on data entering the pipeline. [This
part](Main.hs) transforms that data on the GPU and supplies a changing value
from the CPU.

## Lifted values

An `Expr.V a` is a shader-stage value represented in Haskell. Arithmetic,
trigonometry, vector construction, and component access build the shader
expression graph; they do not calculate the answer on the CPU. `rotateVertex`
uses `sin` and `cos` on an `Expr.V Float` to rotate each position.

Ordinary Haskell values enter the graph with `Expr.constant`. Values that
change between frames use resources such as the uniform buffer in this part.

## Uniforms and composition

`uniform` reads the angle described by `uniformSource "angle" angle`. The CPU
writes a new angle before recording each frame, while the compiled pipeline is
reused. Mapping `rotateVertex rotation` over the primitive stream composes the
transformation with the rest of the pipeline using the stream's `Functor`
instance.

Pipeline compilation reports structural and shader errors before a prepared
pipeline is returned. Resource binding errors retain descriptor names such as
`angle`, so failures point back to the corresponding environment field.

Run it from the repository root:

```console
VPIPE_TEST_DEVICE=any cabal run guide-part-3
```

Next: [Part 4 — Textures](../part-4/README.md).

