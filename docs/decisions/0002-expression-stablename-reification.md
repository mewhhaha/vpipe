# ADR 0002: Reify expressions with a local StableName table

Status: accepted

## Decision

Expression lowering uses the local `StableName ExprObject` table in
[`Vpipe.Expr.Reify`](../../vpipe/src/Vpipe/Expr/Reify.hs), rather than adding
`data-reify`. The table maps each object identity to one node ID, handles hash
collisions with `eqStableName`, and separately tracks objects being visited to
report cycles.

## Rationale

The reifier must produce the exact graph that the SPIR-V lowering consumes:
one forest can have several roots; branches and loops own explicit regions;
and IDs are deterministic left-to-right post-order IDs. A small local walker
can construct that representation directly, including its region and cycle
rules. Using `data-reify` would still require converting its generic graph to
this representation and would add a dependency without removing the
lowering-specific traversal.

## Evidence

The local regression benchmark is
[`deepSharingCompileCase`](../../vpipe/test/Vpipe/SpirVCodegenTest.hs). It
reifies and compiles a 30-level shared diamond, asserts that the graph has 31
nodes, and requires the complete compile to finish in under 250 ms.

[`DataReifyPrototype`](../../vpipe/test/experiments/DataReifyPrototype.hs)
adapts the literal/binary subset of the same expression objects to
`data-reify`. `dataReifyPrototypeCase` runs both reifiers over the identical
30-level object graph, requires 31 nodes from each, and gives each reification
the same 250 ms bound. Both preserve the required sharing. The local walker is
chosen because the prototype graph would still need a second conversion to
vpipe's deterministic, region-aware forest; `data-reify` therefore adds a
runtime dependency and traversal without replacing vpipe-specific logic.

## Consequences

- `data-reify` remains a test-only prototype dependency, not a library
  dependency.
- Sharing follows Haskell object identity during reification, not structural
  equality of independently constructed expressions.
- Changes to region ownership, deterministic IDs, or cycle handling belong in
  the local reifier and must preserve the regression benchmark.
