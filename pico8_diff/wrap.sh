#!/usr/bin/env bash
# Wrap a case into a .p8 cart that PICO-8 can run headless.
#
# The case source is copied VERBATIM - the only thing this adds is a shim
# defining the two output functions in terms of `printh`, which is what makes
# a case runnable unchanged on both sides.
#
# Every shim line is prefixed with a marker because `pico8 -x` writes its own
# lines to stdout ("RUNNING: cart.p8", and error reports), so run.sh has to be
# able to tell cart output from console output. The marker is stripped again
# before the diff.
#
# Usage: wrap.sh <case.lua> <out.p8>

set -euo pipefail

if [ "$#" -ne 2 ]; then
    echo "usage: $0 <case.lua> <out.p8>" >&2
    exit 2
fi

case_file="$1"
out_file="$2"

if [ ! -f "$case_file" ]; then
    echo "wrap.sh: no such case: $case_file" >&2
    exit 2
fi

{
    printf 'pico-8 cartridge // http://www.pico-8.com\n'
    printf 'version 42\n'
    printf '__lua__\n'
    printf 'function __print(x) printh("@P8@ "..tostr(x)) end\n'
    # tostr(x, true) is the exact 16.16 hex ("0x0000.5555"), which is what
    # __hex prints on our side too. Numbers never go through tostr(x) here:
    # its decimal form and our format_scalar_number disagree on fractions.
    printf 'function __hex(x) printh("@P8@ "..tostr(x, true)) end\n'
    cat "$case_file"
} > "$out_file"
