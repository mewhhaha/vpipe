#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
temporary=$(mktemp -d "${TMPDIR:-/tmp}/vpipe-example-screenshots.XXXXXX")
trap 'rm -rf "$temporary"' EXIT

cd "$root"
cabal run triangle -- --frames 2 --screenshot "$temporary/triangle.png"
cabal run headless -- --frames 2 --screenshot "$temporary/headless.png"
cabal run cube -- --frames 2 --screenshot "$temporary/cube.png"
cabal run offscreen -- --frames 2 --screenshot "$temporary/offscreen.png"
cabal run particles -- --frames 2 --screenshot "$temporary/particles.png"
cabal run shadertoy -- --frames 2 --screenshot "$temporary/shadertoy.png"

python3 - "$temporary" <<'PY'
import pathlib, struct, sys, zlib

root, golden = pathlib.Path(sys.argv[1]), pathlib.Path("examples/golden")
names = ("triangle", "headless", "cube", "offscreen", "particles", "shadertoy")

def rgba(path):
    data = path.read_bytes()
    if data[:8] != b"\x89PNG\r\n\x1a\n":
        raise ValueError("not a PNG")
    pos, idat, width = 8, bytearray(), None
    while pos < len(data):
        size = struct.unpack(">I", data[pos:pos + 4])[0]
        kind, chunk = data[pos + 4:pos + 8], data[pos + 8:pos + 8 + size]
        pos += size + 12
        if kind == b"IHDR":
            width, height, depth, color, comp, filt, interlace = struct.unpack(">IIBBBBB", chunk)
            if (width, height, depth, color, comp, filt, interlace) != (64, 64, 8, 6, 0, 0, 0):
                raise ValueError(f"expected non-interlaced RGBA8 64x64, got {width}x{height}")
        elif kind == b"IDAT":
            idat.extend(chunk)
    raw, rows, stride, prior = zlib.decompress(bytes(idat)), [], width * 4, bytearray(width * 4)
    for row in range(64):
        kind, scan = raw[row * (stride + 1)], bytearray(raw[row * (stride + 1) + 1:(row + 1) * (stride + 1)])
        for i, value in enumerate(scan):
            left = scan[i - 4] if i >= 4 else 0
            up = prior[i]
            ul = prior[i - 4] if i >= 4 else 0
            if kind == 1: value += left
            elif kind == 2: value += up
            elif kind == 3: value += (left + up) // 2
            elif kind == 4:
                p = left + up - ul
                pa, pb, pc = abs(p - left), abs(p - up), abs(p - ul)
                value += left if pa <= pb and pa <= pc else (up if pb <= pc else ul)
            elif kind != 0: raise ValueError(f"unsupported PNG filter {kind}")
            scan[i] = value & 255
        rows.append(bytes(scan)); prior = scan
    return b"".join(rows)

tolerance = 2
for name in names:
    expected, actual = rgba(golden / f"{name}.png"), rgba(root / f"{name}.png")
    diffs = [abs(a - b) for a, b in zip(expected, actual)]
    bad = sum(value > tolerance for value in diffs)
    if bad:
        raise SystemExit(f"{name}: {bad} channels exceed tolerance {tolerance}; max diff={max(diffs)}")
print("All required example screenshots match their deterministic 64x64 RGBA goldens (tolerance ±2).")
PY
