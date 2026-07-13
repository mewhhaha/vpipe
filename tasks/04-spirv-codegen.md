# 04 — SPIR-V code generation

**Depends on:** 03
**Milestone:** M2

## Goal

Compile the `Expr` AST plus stage interface descriptions into valid SPIR-V
1.6 binaries, with no external tools in the loop. This replaces GPipe's
GLSL-source printer.

## Why hand-rolled (recorded decision)

SPIR-V is a flat, versioned, word-oriented SSA format with a machine-readable
grammar (`spirv.core.grammar.json` from KhronosGroup/SPIRV-Headers). Emitting
it directly is *less* code and *more* checkable than printing GLSL was.
`fir` proves it's tractable in Haskell. We do not depend on `fir` (its EDSL
shape is different and it drags its own math types); we *do* read its
codegen for technique.

## Deliverables

- `Vpipe.SpirV.Assembler` — low-level writer:
  - id allocation, instruction encoding (opcode word + operand words),
    section ordering (capabilities → extensions → imports → memory model →
    entry points → execution modes → debug names → decorations → types/
    constants/globals → function bodies) as the spec requires.
  - Type/constant deduplication table (SPIR-V requires unique type ids).
  - Emit `OpName`/`OpMemberName` debug info behind a flag (on by default in
    dev; keeps RenderDoc usable).
- Code generator (`Vpipe.SpirV.Codegen`):
  - Hash-consed `ExprTree` → SSA: post-order emit, memoized by node id.
  - Structured control flow: `ifThenElseE` → `OpSelectionMerge` +
    `OpBranchConditional`; `whileE` → `OpLoopMerge` loop skeleton. These are
    the only control-flow shapes, which keeps us trivially inside SPIR-V's
    structured-CFG rules.
  - GLSL.std.450 extended instruction set for the math library.
  - Decorations from task 02's single-source-of-truth layout module:
    `Offset`, `ArrayStride`, `MatrixStride`, `ColMajor`, `Block`,
    `DescriptorSet`/`Binding`, `Location`, `BuiltIn` (Position, FragCoord,
    VertexIndex, InstanceIndex, GlobalInvocationId, ...).
- Consider generating opcode/enum tables from `spirv.core.grammar.json` via a
  small TH or codegen step vendored into the repo (no network at build time —
  commit the generated module).
- Module validation hook: if `spirv-val` (SPIRV-Tools) is on PATH, tests run
  every generated module through it; CI installs it (task 16).

## Steps

1. Assembler + "hello triangle" vertex/fragment modules built by hand against
   the assembler API (not the EDSL) — diff against `glslangValidator` output
   only as a sanity reference, never as a golden.
2. `spirv-val` these hand-built modules; then wire codegen from `Expr`.
3. Golden tests: EDSL shader → disassembled text via `spirv-dis` when
   available, else raw-word golden with a stable id-allocation order.
4. Fuzz: generate random well-typed `Expr` trees (from task 03's generators),
   assert `spirv-val` passes on every one.

## Acceptance criteria

- M2 triangle shaders validate with `spirv-val --target-env vulkan1.3` and
  run on lavapipe.
- Every generated module in the test suite validates.
- Codegen is deterministic (byte-identical output for identical input) — this
  is what makes golden tests and pipeline caching (task 10) work.

## Open questions

- SPIR-V version floor: 1.6 (Vulkan 1.3's default) unless MoltenVK forces
  1.3-era modules; check at execution time.
- Specialization constants: design the `Expr`-level hook now (a
  `specConst :: SpecId -> a -> Expr s a`), implement post-M3.
