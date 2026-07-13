#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root"

manifest=scripts/docs/snippets.manifest
temporary_directory=$(mktemp -d "${TMPDIR:-/tmp}/vpipe-snippets.XXXXXX")
trap 'rm -rf "$temporary_directory"' EXIT

python3 - "$manifest" "$temporary_directory" <<'PY'
from __future__ import annotations

import pathlib
import re
import sys


root = pathlib.Path.cwd()
manifest = root / sys.argv[1]
destination = pathlib.Path(sys.argv[2])
fence_start = re.compile(r"^```haskell\s*$")


def error(source: str, message: str) -> None:
    raise SystemExit(f"{source}: {message}")


def haddock_header_block(source: pathlib.Path, module_name: str, position: int) -> str:
    text = source.read_text(encoding="utf-8")
    module_declaration = re.search(rf"(?m)^module\s+{re.escape(module_name)}(?:\s|\()", text)
    if module_declaration is None:
        error(str(source), f"does not declare module {module_name}")
    header = text[: module_declaration.start()]
    blocks = re.findall(r"(?m)^@\s*$\n(.*?)(?=^@\s*$)", header, re.DOTALL)
    if position > len(blocks):
        error(str(source), f"has no Haddock header block {position}")
    return blocks[position - 1]


def markdown_haskell_block(source: pathlib.Path, position: int) -> str:
    blocks: list[str] = []
    current: list[str] | None = None
    for line in source.read_text(encoding="utf-8").splitlines(keepends=True):
        if current is None:
            if fence_start.match(line):
                current = []
        elif line.startswith("```"):
            blocks.append("".join(current))
            current = None
        else:
            current.append(line)
    if current is not None:
        error(str(source), "has an unterminated Haskell code fence")
    if position > len(blocks):
        error(str(source), f"has no Haskell code fence {position}")
    return blocks[position - 1]


def public_modules(cabal_file: pathlib.Path) -> set[str]:
    public_library = cabal_file.read_text(encoding="utf-8").split("library internal", 1)[0]
    return set(re.findall(r"\bVpipe(?:\.[A-Za-z]+)*\b", public_library))


expected_public_modules = public_modules(root / "vpipe/vpipe.cabal")
expected_public_modules.update(public_modules(root / "vpipe-glfw/vpipe-glfw.cabal"))
entries = []
registered_public_modules: set[str] = set()
for line_number, line in enumerate(manifest.read_text(encoding="utf-8").splitlines(), 1):
    if not line or line.startswith("#"):
        continue
    try:
        fixture_name, kind, source_name, selector = line.split("\t")
    except ValueError:
        error(str(manifest), f"line {line_number} must contain four tab-separated fields")
    if fixture_name.startswith("Vpipe"):
        if fixture_name in registered_public_modules:
            error(str(manifest), f"line {line_number} registers {fixture_name} more than once")
        registered_public_modules.add(fixture_name)
    source = root / source_name
    if not source.is_file():
        error(str(manifest), f"line {line_number} names missing fixture {source_name}")
    if kind == "haddock-header":
        contents = haddock_header_block(source, fixture_name, int(selector))
    elif kind == "markdown-haskell":
        contents = markdown_haskell_block(source, int(selector))
    elif kind == "source" and selector == "-":
        contents = source.read_text(encoding="utf-8")
    else:
        error(str(manifest), f"line {line_number} has unsupported fixture kind {kind!r}")
    output = destination / f"{len(entries):02d}-{fixture_name.replace(':', '-').replace('.', '-')}.hs"
    output.write_text(contents, encoding="utf-8")
    entries.append((fixture_name, source_name, output))

if not entries:
    error(str(manifest), "does not name any documentation fixtures")

missing_modules = sorted(expected_public_modules - registered_public_modules)
unexpected_modules = sorted(registered_public_modules - expected_public_modules)
if missing_modules or unexpected_modules:
    details = []
    if missing_modules:
        details.append(f"missing public modules: {', '.join(missing_modules)}")
    if unexpected_modules:
        details.append(f"unknown public modules: {', '.join(unexpected_modules)}")
    error(str(manifest), "; ".join(details))

for fixture_name, source_name, output in entries:
    print(f"Extracted {fixture_name} from {source_name} -> {output.name}")
PY

while IFS= read -r -d '' snippet; do
  echo "Typechecking ${snippet#"$temporary_directory"/}"
  cabal exec -- ghc -XGHC2024 -fno-code -iexamples/src -package vpipe -package vpipe-glfw "$snippet"
done < <(find "$temporary_directory" -maxdepth 1 -type f -name '*.hs' -print0 | sort -z)
