# 03 — Expression EDSL

**Depends on:** 02
**Milestone:** M2

## Goal

The shader-expression language: a typed, first-order AST that Haskell code
builds by ordinary arithmetic and combinators, later compiled to SPIR-V.
This is vpipe's equivalent of GPipe's `S x a`.

## Design

- `newtype Expr (s :: Stage) a = Expr (ExprTree)` — phantom `a` is the
  value's type, phantom `s` tags the stage (`V`ertex, `F`ragment, `C`ompute)
  so stage-only operations (e.g. `dFdx`, texture LOD-implicit sampling) are
  constrained to the right stage, exactly like GPipe's `V`/`F` phantoms.
- Internal `ExprTree` is untyped-but-tagged (constructors carry a `ScalarTy`/
  shape); all type safety lives at the `Expr` layer. GADT-typing the internal
  tree is not worth it — codegen needs to be uniform anyway.
- **Sharing:** GPipe regenerated expression text and relied on GLSL compilers
  to CSE. We do better: observable sharing via `Data.StableName`-based
  hash-consing at compile time (like `data-reify`), so a value used twice in
  Haskell becomes one SSA id, not a duplicated subtree. This is the single
  most important ergonomic/perf improvement over GPipe — diamond-shaped
  expressions explode exponentially otherwise. Decide `data-reify` vs
  hand-rolled StableName map; prototype both on a deep diamond benchmark.
- Numeric classes: instances of `Num`, `Fractional`, `Floating` for
  `Expr s Float` etc. Comparisons can't return `Bool`, so define our own
  (GPipe used the `Boolean` package; it's unmaintained — roll our own small
  classes):
  - `type BoolE s = Expr s Bool`
  - `class EqE a where (==.), (/=.) :: a -> a -> BoolE s` (shape TBD)
  - `class OrdE a` with `(<.)`, `(>=.)`, etc.
  - `ifE :: BoolE s -> Expr s a -> Expr s a -> Expr s a` (select, not
    branching) plus a genuine branching `ifThenElseE` for expensive arms.
- Vectors: `Expr s (V3 Float)` with swizzling lenses/fields (`_x`, `_xy` —
  reuse linear's lens names via a `Swizzle` class), constructors
  (`vec3 :: Expr s Float -> ... -> Expr s (V3 Float)`), matrix ops matching
  `linear`'s API surface (`!*!`, `!*`).
- Standard library: clamp/mix/smoothstep/normalize/dot/cross/reflect, texture
  sampling ops (typed against task 08's sampler handles), derivatives
  (fragment-only), `discard` (fragment-only, statement-ish — see task 05).
- Loops and mutation: v1 ships `whileE :: (Expr s a -> BoolE s) -> (Expr s a -> Expr s a) -> Expr s a -> Expr s a`
  (GPipe's approach) — no general mutable variables. Document the escape
  hatch design (structured `forE` over `Expr s Int32` ranges) for v1.1.

## Deliverables

- `Vpipe.Expr` (public API) and `Vpipe.Expr.Internal` (tree, hash-consing).
- Instance coverage table in Haddock: which classes at which types.
- A pure "evaluator" backend (`evalExpr :: Expr s a -> HostValue a`) used
  only for testing — properties like `evalExpr (a + b) == evalExpr a + evalExpr b`
  make EDSL bugs findable without a GPU.

## Acceptance criteria

- The M2 triangle's vertex+fragment shader expressions build with no
  stage-incorrect operation compiling (negative tests via
  `should-not-typecheck` or deliberate `-fdefer-type-errors` tests).
- Diamond-sharing benchmark: a 30-level shared expression compiles in
  milliseconds and produces a linear-size tree.

## Resolved interface scope

Stage interfaces use nested tuples in v1. Host records can use
`GenericVertex`, which adapts them to tuple-shaped shader values; record
varyings remain a future additive convenience. See [ADR 0006](../docs/decisions/0006-shader-interface-scope.md).
