#!/usr/bin/env bash
set -euo pipefail

# Run the release checks from the repository root.  All generated state lives
# below ARTIFACT_DIR (or a temporary directory) so this is safe to run locally.
ROOT_DIR=${ROOT_DIR:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}
ARTIFACT_DIR=${ARTIFACT_DIR:-"$ROOT_DIR/artifacts/release-candidate"}
mkdir -p "$ARTIFACT_DIR" "$ARTIFACT_DIR/sdist" "$ARTIFACT_DIR/logs"

log_run() {
  local name=$1
  shift
  echo "+ $*" | tee "$ARTIFACT_DIR/logs/$name.log"
  "$@" 2>&1 | tee -a "$ARTIFACT_DIR/logs/$name.log"
}

log_validation_run() {
  local name=$1
  shift
  : >"$ARTIFACT_DIR/logs/$name.log"
  echo "+ $*" | tee -a "$ARTIFACT_DIR/logs/$name.log" "$VPIPE_VALIDATION_LOG"
  "$@" 2>&1 | tee -a "$ARTIFACT_DIR/logs/$name.log" "$VPIPE_VALIDATION_LOG"
}

manifest_version() {
  awk '$1 == "version:" { print $2; exit }' "$1"
}

release_train() {
  local version=$1
  local major minor rest
  IFS=. read -r major minor rest <<<"$version"
  [[ $major =~ ^[0-9]+$ && $minor =~ ^[0-9]+$ && -n $rest ]] || {
    echo "cannot derive a PVP release train from version $version" >&2
    exit 1
  }
  printf '%s.%s\n' "$major" "$minor"
}

next_release_train() {
  local train=$1
  local major minor
  IFS=. read -r major minor <<<"$train"
  printf '%s.%s\n' "$major" "$((10#$minor + 1))"
}

require_vulkan_headers() {
  local compiler=${CXX:-c++}
  if ! command -v "$compiler" >/dev/null; then
    echo "C++ compiler $compiler is not available" >&2
    exit 1
  fi
  if ! printf '#include <vulkan/vulkan.h>\n' | "$compiler" -E -x c++ - >/dev/null 2>&1; then
    echo "$compiler cannot find vulkan/vulkan.h; install Vulkan headers or set CPATH" >&2
    exit 1
  fi
}

require_dependency_bounds() {
  local manifest=$1
  local dependency=$2
  local lower_bound=$3
  local upper_bound=$4
  awk \
    -v dependency="$dependency" \
    -v lower_bound="$lower_bound" \
    -v upper_bound="$upper_bound" '
      function check_dependency(line, normalized, expected) {
        sub(/^[[:space:]]+/, "", line)
        sub(/,[[:space:]]*$/, "", line)
        if (line !~ ("^" dependency "([[:space:]]|$)")) return
        found = 1
        normalized = line
        gsub(/[[:space:]]+/, "", normalized)
        expected = dependency ">=" lower_bound "&&<" upper_bound
        if (normalized != expected) invalid = line
      }
      /^[[:space:]]*build-depends:[[:space:]]*/ {
        line = $0
        sub(/^[[:space:]]*build-depends:[[:space:]]*/, "", line)
        check_dependency(line)
        in_build_depends = 1
        next
      }
      in_build_depends && /^[^[:space:]]/ { in_build_depends = 0 }
      in_build_depends && /^[[:space:]][[:space:]][A-Za-z-]+:/ { in_build_depends = 0 }
      in_build_depends { check_dependency($0) }
      END {
        if (!found) exit 2
        if (invalid) exit 1
      }
    ' "$manifest" && return
  case $? in
    1)
      echo "$manifest has a $dependency dependency outside >=$lower_bound && <$upper_bound" >&2
      exit 1
      ;;
    2)
      echo "$manifest has no build-depends entry for $dependency" >&2
      exit 1
      ;;
  esac
}

cd "$ROOT_DIR"
expected=$(manifest_version vpipe/vpipe.cabal)
[[ -n "$expected" ]] || { echo 'vpipe/vpipe.cabal has no version' >&2; exit 1; }
manifests=(
  vpipe/vpipe.cabal
  vpipe-glfw/vpipe-glfw.cabal
  examples/vpipe-examples.cabal
)
for manifest in "${manifests[@]}"; do
  [[ -f "$manifest" ]] || { echo "missing manifest: $manifest" >&2; exit 1; }
  actual=$(manifest_version "$manifest")
  [[ "$actual" == "$expected" ]] || {
    echo "$manifest has version $actual (expected $expected)" >&2
    exit 1
  }
done

require_vulkan_headers

# Internal dependency ranges must remain PVP-compatible for the coupled release train.
train=$(release_train "$expected")
next_train=$(next_release_train "$train")
require_dependency_bounds vpipe-glfw/vpipe-glfw.cabal vpipe "$train" "$next_train"
require_dependency_bounds examples/vpipe-examples.cabal vpipe "$train" "$next_train"
require_dependency_bounds examples/vpipe-examples.cabal vpipe-glfw "$train" "$next_train"

log_run cabal-check-vpipe bash -c 'cd vpipe && cabal check'
log_run cabal-check-vpipe-glfw bash -c 'cd vpipe-glfw && cabal check'
log_run cabal-check-examples bash -c 'cd examples && cabal check'

export VPIPE_TEST_DEVICE=${VPIPE_TEST_DEVICE:-lavapipe}
export VK_ICD_FILENAMES=${VK_ICD_FILENAMES:-/usr/share/vulkan/icd.d/lvp_icd.x86_64.json}
export VPIPE_DUMP=${VPIPE_DUMP:-"$ARTIFACT_DIR/vpipe-dump"}
export VPIPE_VALIDATION_LOG=${VPIPE_VALIDATION_LOG:-"$ARTIFACT_DIR/validation.log"}
export XDG_CACHE_HOME=${XDG_CACHE_HOME:-"$ARTIFACT_DIR/cache"}
mkdir -p "$VPIPE_DUMP" "$XDG_CACHE_HOME"

log_run build-all cabal build all
log_validation_run test-all xvfb-run --auto-servernum cabal test all --test-show-details=direct
[[ -s "$VPIPE_VALIDATION_LOG" ]] || {
  echo "test run did not produce validation evidence at $VPIPE_VALIDATION_LOG" >&2
  exit 1
}
log_run example-goldens scripts/examples/check-screenshots.sh
log_validation_run windowed-examples scripts/examples/check-windowed-validation.sh
log_run documentation-snippets scripts/docs/check-snippets.sh
log_run haddock scripts/docs/check-haddock.sh --for-hackage
log_run documentation-links python3 scripts/docs/check-links.py --check-external

mkdir -p "$ARTIFACT_DIR/docs"
for package in vpipe vpipe-glfw; do
  doc_name="${package}-${expected}-docs.tar.gz"
  mapfile -t doc_archives < <(
    find "$ROOT_DIR" -path "$ARTIFACT_DIR" -prune -o -type f -name "$doc_name" -print
  )
  [[ ${#doc_archives[@]} -eq 1 ]] || {
    echo "expected exactly one generated $doc_name, found ${#doc_archives[@]}" >&2
    exit 1
  }
  cp "${doc_archives[0]}" "$ARTIFACT_DIR/docs/$doc_name"
done

log_run sdist-vpipe cabal sdist vpipe --output-directory="$ARTIFACT_DIR/sdist"
log_run sdist-vpipe-glfw cabal sdist vpipe-glfw --output-directory="$ARTIFACT_DIR/sdist"

isolate=$(mktemp -d "${TMPDIR:-/tmp}/vpipe-candidate.XXXXXX")
trap 'rm -rf "$isolate"' EXIT
vpipe_archive="$ARTIFACT_DIR/sdist/vpipe-${expected}.tar.gz"
glfw_archive="$ARTIFACT_DIR/sdist/vpipe-glfw-${expected}.tar.gz"
[[ -f "$vpipe_archive" && -f "$glfw_archive" ]] || {
  echo 'sdist generation did not produce both expected archives' >&2
  exit 1
}

mkdir "$isolate/vpipe" "$isolate/vpipe-glfw"
tar -xzf "$vpipe_archive" -C "$isolate/vpipe" --strip-components=1
tar -xzf "$glfw_archive" -C "$isolate/vpipe-glfw" --strip-components=1

default_repo_cache=$(cabal path | awk -F': ' '$1 == "remote-repo-cache" { print $2; exit }')
[[ -n "$default_repo_cache" && -d "$default_repo_cache" ]] || {
  echo 'could not locate the updated Cabal package-index cache' >&2
  exit 1
}

printf '%s\n' \
  'packages:' \
  '  ./vpipe' \
  '  ./vpipe-glfw' \
  'tests: True' \
  >"$isolate/cabal.project"

export CABAL_DIR="$isolate/cabal-home"
export CABAL_CONFIG="$CABAL_DIR/config"
mkdir -p "$CABAL_DIR" "$isolate/cabal-store" "$isolate/cabal-logs"
log_run isolated-cabal-config \
  cabal --config-file="$CABAL_CONFIG" user-config init --force \
    --augment="remote-repo-cache: $default_repo_cache" \
    --augment="store-dir: $isolate/cabal-store" \
    --augment="logs-dir: $isolate/cabal-logs" \
    --augment="world-file: $isolate/world"

cd "$isolate"
log_run isolated-plan cabal build vpipe-glfw --dry-run
python3 - "$isolate/dist-newstyle/cache/plan.json" "$isolate/vpipe" "$isolate/vpipe-glfw" <<'PY' \
  | tee "$ARTIFACT_DIR/logs/isolated-plan-proof.log"
import json
import pathlib
import sys

plan_path, vpipe_path, glfw_path = map(pathlib.Path, sys.argv[1:])
plan = json.loads(plan_path.read_text(encoding="utf-8"))

def local_units(name, source):
    source = source.resolve()
    return [
        unit
        for unit in plan["install-plan"]
        if unit.get("pkg-name") == name
        and unit.get("style") == "local"
        and unit.get("pkg-src", {}).get("type") == "local"
        and pathlib.Path(unit["pkg-src"]["path"]).resolve() == source
    ]

vpipe_units = local_units("vpipe", vpipe_path)
glfw_units = local_units("vpipe-glfw", glfw_path)
vpipe_library_ids = {
    unit["id"] for unit in vpipe_units if unit.get("component-name") == "lib"
}
if not vpipe_library_ids or not glfw_units:
    raise SystemExit("isolated plan does not contain both unpacked local packages")
if not any(vpipe_library_ids.intersection(unit.get("depends", [])) for unit in glfw_units):
    raise SystemExit("vpipe-glfw does not resolve the unpacked local vpipe library")
print("Isolated plan resolves vpipe-glfw against the unpacked vpipe sdist.")
PY

log_run isolated-vpipe-build cabal build vpipe
log_run isolated-build-all cabal build all
log_run isolated-test-all xvfb-run --auto-servernum cabal test all --test-show-details=direct

echo "Candidate verification complete; no Hackage upload was attempted."
