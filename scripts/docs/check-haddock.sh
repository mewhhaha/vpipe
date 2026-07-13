#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
cd "$root"

# These names occur in compiler-rendered public signatures, but their defining
# modules or helper families are intentionally hidden.  Haddock cannot resolve
# such links even though the declarations are valid and usable.  Keep this
# allowlist narrow: every other warning remains fatal.
ignored_link_symbols=(
  Expr
  Graphics.UI.GLFW.Types.Key
  Graphics.UI.GLFW.Types.KeyState
  Vpipe.Expr.Internal.Expr
  Vpipe.Buffer.Format.FixedVector
  Vpipe.Buffer.Format.GBufferHost
  Vpipe.Buffer.Format.MatrixComponentMismatch
  Vpipe.Buffer.Format.VectorValue
  Vpipe.Buffer.Staging.ObjectNameSetter
  Vpipe.Buffer.State.BufferState
  Vpipe.Buffer.Internal.ChooseVertexLayout
  Vpipe.Buffer.Internal.ContainsUsage
  Vpipe.Buffer.Internal.UsageName
  Vpipe.Buffer.Internal.ValidUsageCombination
  Vpipe.Buffer.Internal.ValidUsageList
  Vpipe.Compute.IR.Internal.AtomicIntegerSupported
  Vpipe.Compute.IR.Internal.StorageElementSupported
  Vpipe.Compute.IR.Internal.storageElementLayout
  Vpipe.Compute.IR.Internal.storageElementType
  Vpipe.Context.Queue.Internal.QueueDependency
  Vpipe.Expr.Internal.HostValue
  Vpipe.Expr.Internal.ImageDimension
  Vpipe.Expr.Internal.ShaderTy
  Vpipe.Expr.Internal.fromHostValue
  Vpipe.Expr.Internal.splatInteger
  Vpipe.Expr.Internal.splatRational
  Vpipe.Expr.Internal.toHostValue
  Vpipe.Expr.Internal.valueTy
  Vpipe.Expr.Reify.NodeId
  Vpipe.Expr.Reify.ReifiedForest
  Vpipe.Expr.projectSample
  Vpipe.Format.TypeErrorColorFormatCannotBeDepth
  Vpipe.Format.TypeErrorDepthFormatCannotBeColor
  Vpipe.Image.Types.ValidImageUsageCombination
  Vpipe.Image.Types.ValidImageUsageList
  Vpipe.Image.State.ImageState
  Vpipe.Pipeline.Internal.ColorOutputDiagnostic
  Vpipe.Pipeline.Internal.ColorOutputDiagnosticResult
  Vpipe.Pipeline.Internal.FlatInterpolationAllowed
  Vpipe.Pipeline.Internal.FragmentInputs
  Vpipe.Pipeline.Internal.GVertexInput
  Vpipe.Pipeline.Internal.GVertexInputShader
  Vpipe.Pipeline.Internal.ColorOutputMismatch
  Vpipe.Pipeline.Internal.MatchingTopology
  Vpipe.Pipeline.Internal.PushConstantSizeAllowed
  Vpipe.Pipeline.Internal.RawFragmentInputAllowed
  Vpipe.Pipeline.Internal.SmoothInterpolationAllowed
  Vpipe.Pipeline.Internal.VertexAttribute
  Vpipe.Pipeline.Internal.buildFragmentInputs
  Vpipe.Pipeline.Internal.describeVertexInput
  Vpipe.Pipeline.Internal.topologyValue
  Vpipe.Sampler.Types.SamplerCacheEntry
  Vpipe.SpirV.Codegen.CodegenError
  Vpipe.SpirV.Codegen.ShaderAction
  Vpipe.SpirV.Codegen.ShaderStage
  Vpipe.Swapchain.Internal.FrameDomain
)

haddock_options=()
for symbol in "${ignored_link_symbols[@]}"; do
  haddock_options+=("--ignore-link-symbol=$symbol")
done

extra=()
if [[ ${1:-} == --for-hackage ]]; then
  extra+=(--haddock-for-hackage)
  shift
fi
if (($#)); then
  echo "usage: $0 [--for-hackage]" >&2
  exit 2
fi

extra+=(--haddock-hyperlink-source)

output=$(mktemp "${TMPDIR:-/tmp}/vpipe-haddock.XXXXXX")
trap 'rm -f "$output"' EXIT

set +e
cabal haddock vpipe:lib:internal \
  "${extra[@]}" \
  --haddock-options="${haddock_options[*]}" \
  2>&1 | tee /dev/null
status=${PIPESTATUS[0]}
set -e
((status == 0)) || exit "$status"

mapfile -t nested_internal_interfaces < <(
  find dist-newstyle/build -path '*/vpipe-*/l/internal/doc/html/vpipe/internal/internal.haddock' -type f -print
)
if ((${#nested_internal_interfaces[@]} > 1)); then
  echo "found multiple private vpipe Haddock interfaces; remove stale build directories" >&2
  exit 1
fi
if ((${#nested_internal_interfaces[@]} == 1)); then
  nested_internal_docs=${nested_internal_interfaces[0]%/internal.haddock}
  registered_internal_docs=${nested_internal_docs%/internal}
  # Cabal 3.16 records the parent directory for a private library's Haddock
  # interface while generating the files in a component-named child directory.
  cp -R "$nested_internal_docs/." "$registered_internal_docs/"
fi

set +e
cabal haddock vpipe:lib:vpipe vpipe-glfw \
  "${extra[@]}" \
  --haddock-options="${haddock_options[*]}" \
  2>&1 | tee -a "$output"
status=${PIPESTATUS[0]}
set -e
((status == 0)) || exit "$status"

# system-cxx-std-lib deliberately exposes no Haskell modules, so Cabal cannot
# install a Haddock interface for it. Haddock reports that fact even though no
# generated link can target the package. Reject every other warning.
python3 - "$output" <<'PY'
import pathlib
import re
import sys

text = pathlib.Path(sys.argv[1]).read_text(encoding="utf-8")
known = re.compile(
    r"Warning: The following packages have no Haddock documentation installed\. No\n"
    r"links will be generated to these packages: system-cxx-std-lib-1\.0\n"
)
unexpected = known.sub("", text)
if "Warning:" in unexpected:
    raise SystemExit("Haddock emitted a warning outside the reviewed link allowlist")
PY
