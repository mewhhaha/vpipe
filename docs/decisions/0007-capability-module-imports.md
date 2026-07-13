# ADR 0007: Keep `Vpipe` documentation-only

Status: accepted

## Decision

Applications import the small capability modules they use, such as
`Vpipe.Context`, `Vpipe.Pipeline`, and `Vpipe.Swapchain`. A GLFW application
imports `Vpipe.GLFW` as its only GLFW module; its common keyboard patterns
are sufficient for a basic event loop without importing `Graphics.UI.GLFW`.

The `Vpipe` umbrella module intentionally exports no runtime names. It is the
Haddock landing page and points readers to the capability modules instead.

## Rationale

The original task-13 sketch asked simple applications to import only `Vpipe`
and `Vpipe.GLFW`. A broad umbrella would make common resource and usage names
ambiguous as the library grows, while hiding that ambiguity behind selective
re-exports would make the import surface arbitrary. The established package
already uses capability modules throughout its examples and tutorials, so an
umbrella re-export would be a disruptive second convention.

Keeping the GLFW boundary at `Vpipe.GLFW` still gives applications one
window-system import. It owns the native window lifetime and documents a
small, portable keyboard vocabulary; applications needing GLFW's less common
input features can use GLFW-b deliberately.

## Consequences

- Documentation and examples name the vpipe capabilities they require.
- `Vpipe.GLFW` is the only GLFW import in basic applications.
- Adding a new convenience umbrella would require a collision policy and a
  migration decision; it is not implied by task 13.
