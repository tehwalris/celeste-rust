#!/usr/bin/env python3
"""Replay an input sequence on a REAL PICO-8 and print the per-frame state.

The definitive fidelity check for a witness: `lua/celeste-minimal.lua`
(the exact Lua the search runs) plus the cart's map and flag data, driven
frame by frame with `btn` overridden to read the scripted inputs, headless
under `pico8 -x`. Same frame convention as `concrete_run`: frame f is the
f-th `_update();_draw()` after `_init()`, and the input byte for frame f is
`inputs[f-1]` (bit0=left bit1=right bit2=up bit3=down bit4=jump bit5=dash).

    pico8_diff/replay.py tas/room_1_0_exit_frame_100.txt
    pico8_diff/replay.py --inputs 0,0,...,42 [--frames N]
    PICO8=~/pico-8/pico8 pico8_diff/replay.py ...
"""
import argparse
import os
import re
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def read_inputs(path):
    text = "".join(l for l in open(path) if not l.startswith("#"))
    return [int(x) for x in re.split(r"[,\s]+", text.strip()) if x]


def hexbytes(path, n):
    h = re.sub(r"\s", "", open(path).read())
    assert len(h) == 2 * n, f"{path}: {len(h)//2} bytes, expected {n}"
    return bytes.fromhex(h)


def build_cart(inputs, frames, out):
    lua = open(os.path.join(ROOT, "lua", "celeste-minimal.lua")).read()
    map_data = hexbytes(os.path.join(ROOT, "cart", "map-data.txt"), 8192)
    flags = hexbytes(os.path.join(ROOT, "cart", "flag-data.txt"), 256)
    prelude = [
        # The interpreter's hooks: the interval splitter (identity on a
        # concrete value) and the state-normalization hint (a no-op).
        "function __split_by_flr(x) return x end",
        "function _hint_normalize() end",
        "__inputs = {" + ",".join(str(b) for b in inputs) + "}",
        "__frame = 0",
        "function btn(i)",
        "  local b = __inputs[__frame] or 0",
        "  return (b & (1 << i)) ~= 0",
        "end",
    ]
    driver = [
        "_init()",
        f"for f = 1, {frames} do",
        "  __frame = f",
        "  _update()",
        "  _draw()",
        "  local line = 'f'..f..' room '..room.x..','..room.y..' freeze '..freeze",
        "  for o in all(objects) do",
        "    if o.type == player then",
        "      line = line..' player '..o.x..','..o.y..' spd '..tostr(o.spd.x, true)..','..tostr(o.spd.y, true)"
        "..' rem '..tostr(o.rem.x, true)..','..tostr(o.rem.y, true)",
        "    elseif o.type == player_spawn then",
        "      line = line..' player_spawn '..o.x..','..o.y",
        "    end",
        "  end",
        "  printh('@P8@ '..line)",
        "end",
        "_init = nil _update = nil _draw = nil",
    ]
    # __map__: rows 0..31 as 32 lines of 128 bytes. Rows 32..63 live in the
    # sprite sheet's lower half (__gfx__ lines 64..127), one hex char per
    # PIXEL with the low nibble first.
    map_lines = [map_data[r * 128:(r + 1) * 128].hex() for r in range(32)]
    gfx_lines = ["0" * 128 for _ in range(64)]
    for r in range(32, 64):
        row = map_data[r * 128:(r + 1) * 128]
        gfx_lines.append("".join(f"{b & 15:x}{b >> 4:x}" for b in row))
    gff_lines = [flags[:128].hex(), flags[128:].hex()]
    with open(out, "w") as f:
        f.write("pico-8 cartridge // http://www.pico-8.com\nversion 42\n__lua__\n")
        f.write("\n".join(prelude) + "\n")
        f.write(lua + "\n")
        f.write("\n".join(driver) + "\n")
        f.write("__gfx__\n" + "\n".join(gfx_lines) + "\n")
        f.write("__gff__\n" + "\n".join(gff_lines) + "\n")
        f.write("__map__\n" + "\n".join(map_lines) + "\n")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("tas", nargs="?", help="witness file (comment lines start with #)")
    ap.add_argument("--inputs", help="comma-separated input bytes instead of a file")
    ap.add_argument("--frames", type=int, help="frames to run (default: number of inputs)")
    ap.add_argument("--keep", action="store_true", help="keep the generated cart")
    args = ap.parse_args()
    if args.inputs:
        inputs = [int(x) for x in args.inputs.split(",")]
    elif args.tas:
        inputs = read_inputs(args.tas)
    else:
        ap.error("a witness file or --inputs is required")
    frames = args.frames or len(inputs)
    pico8 = os.environ.get("PICO8", os.path.expanduser("~/pico-8/pico8"))
    work = tempfile.mkdtemp(prefix="celeste-replay-")
    cart = os.path.join(work, "replay.p8")
    build_cart(inputs, frames, cart)
    print(f"[replay] {len(inputs)} inputs, {frames} frames, cart {cart}", file=sys.stderr)
    proc = subprocess.run([pico8, "-x", cart], capture_output=True, text=True, timeout=120)
    lines = [l[len("@P8@ "):] for l in proc.stdout.splitlines() if l.startswith("@P8@ ")]
    for l in lines:
        print(l)
    if not lines:
        print("[replay] no cart output; pico8 said:", file=sys.stderr)
        print(proc.stdout[-2000:], file=sys.stderr)
        print(proc.stderr[-2000:], file=sys.stderr)
        sys.exit(1)
    if not args.keep:
        os.remove(cart)
        os.rmdir(work)


if __name__ == "__main__":
    main()
