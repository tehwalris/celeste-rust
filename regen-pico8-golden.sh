#!/usr/bin/env bash
# Regenerate the golden outputs in lua/probe/ by running each corpus in
# REAL PICO-8. Needs a PICO-8 install; the golden files are checked in so
# that the tests run without one.
#
# READ THE DIFF before committing. A change here means the model's
# reference moved, which is a much bigger deal than a test going red.
set -euo pipefail
P8="${PICO8:-$HOME/pico-8/pico8}"
if [ ! -x "$P8" ]; then
    echo "no PICO-8 at $P8 (set PICO8=/path/to/pico8)" >&2
    exit 1
fi
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp" "${new:-}"' EXIT
for lua in lua/probe/*.lua; do
    name="$(basename "$lua" .lua)"
    # `__hex`: the exact print the tracer has natively (src/trace/probe.rs).
    { printf 'pico-8 cartridge // http://www.pico-8.com\nversion 42\n__lua__\n';
      printf 'function __hex(x) printh(tostr(x, true)) end\n';
      cat "$lua"; } > "$tmp/$name.p8"
    # PICO-8 wants to be run from the cart's directory, and prints a
    # RUNNING: banner that is not part of the program's output.
    # Written beside the golden, then renamed over it where it changed: the
    # tests read these files while one of them runs this script
    # (`trace::probe`), and a truncated file read mid-write is no golden.
    new="$(mktemp "lua/probe/.$name.XXXXXX")"
    ( cd "$tmp" && SDL_VIDEODRIVER=dummy "$P8" -x "$name.p8" ) \
        | grep -v '^RUNNING:' > "$new"
    if cmp -s "$new" "lua/probe/$name.expected"; then rm "$new"; else mv "$new" "lua/probe/$name.expected"; fi
    echo "$name: $(wc -l < "lua/probe/$name.expected") lines"
done
