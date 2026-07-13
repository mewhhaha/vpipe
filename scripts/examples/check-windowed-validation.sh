#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)

if command -v xvfb-run >/dev/null; then
  example_runner=(xvfb-run --auto-servernum)
elif [[ -n ${DISPLAY:-} ]]; then
  example_runner=()
else
  echo "xvfb-run is required when DISPLAY is unset" >&2
  exit 1
fi

run_windowed_example() {
  local example=$1
  echo "+ ${example_runner[*]} cabal run $example -- --frames 1"
  "${example_runner[@]}" cabal run "$example" -- --frames 1
}

cd "$root"
run_windowed_example triangle
run_windowed_example cube
run_windowed_example offscreen
# This exercises the 100,000-particle compute-to-graphics frame path.
run_windowed_example particles
run_windowed_example shadertoy
run_windowed_example mandelbrot
run_windowed_example plasma
run_windowed_example rings
run_windowed_example guide-part-1
run_windowed_example guide-part-2
run_windowed_example guide-part-3
run_windowed_example guide-part-4
run_windowed_example guide-part-5
