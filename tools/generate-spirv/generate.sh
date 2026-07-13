#!/bin/sh
set -eu

# The committed generated module is intentionally reviewed source.  A changed
# grammar/tag is explicit; builds never download grammar metadata.
grammar=vendor/spirv/spirv.core.grammar.json
test "$(git hash-object "$grammar")" = 22f5fe9c76f1fb292e93629d316ac6ff69fd193f
grep -Eq '"major_version"[[:space:]]*:[[:space:]]*1' "$grammar"
grep -Eq '"minor_version"[[:space:]]*:[[:space:]]*6' "$grammar"
target=vpipe/src/Vpipe/SpirV/Generated.hs
formatter=${FOURMOLU:-fourmolu}
if test "${1:-}" = --check; then
  temporary=$(mktemp)
  trap 'rm -f "$temporary"' EXIT
  python3 tools/generate-spirv/generate.py "$grammar" "$temporary"
  "$formatter" --config fourmolu.yaml --mode inplace "$temporary"
  cmp "$temporary" "$target"
elif test "$#" = 0; then
  python3 tools/generate-spirv/generate.py "$grammar" "$target"
  "$formatter" --config fourmolu.yaml --mode inplace "$target"
else
  echo "usage: $0 [--check]" >&2
  exit 2
fi
