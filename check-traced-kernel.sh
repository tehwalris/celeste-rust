#!/usr/bin/env bash
# Does the kernel rendered from a TRACED frame actually COMPILE?
#
# `trace::kernel::render` producing 6,000 lines only says the emitter
# emitted something. Whether those lines are Rust is a separate question
# and rustc is the only thing that answers it.
#
# Then it RUNS it, against `trace::eval` on the same inputs - two
# evaluators of one graph, which is what says the emitted Rust means what
# the graph means.
#
# `traced-kernel-check/` is deliberately outside the workspace. The
# kernel is generated, ~400 KB, and changes on every emitter tweak;
# putting it under `crates/celeste-kernels` would put it inside the
# bootstrap the checked-in kernels already have, where a kernel that
# fails to compile also stops you rebuilding the tool that would fix it.
#
#   ./check-traced-kernel.sh
#
# Regenerates target/traced-kernel.rs first unless SKIP_RENDER is set.
set -euo pipefail

repo="$(cd "$(dirname "$0")" && pwd)"
out="$repo/target/traced-kernel.rs"

if [ -z "${SKIP_RENDER:-}" ]; then
    echo "== rendering (the emit probe writes $out)"
    cargo nextest run --cargo-profile quick \
        the_kernel_emitter_lowers_a_traced_graph --no-capture 2>&1 |
        grep -E '^\[emit\] (FUSED|rendered|RENDER)' || true

    # And the ROOM's set - one kernel per shape the room reaches, plus
    # the table the dispatcher indexes. Same reason as above: rendering
    # them says nothing about whether they are Rust, and the end-to-end
    # room test cannot run until they are.
    echo "== rendering the room's kernel set"
    cargo nextest run --cargo-profile quick --run-ignored all \
        renders_a_kernel_for_every_shape --no-capture 2>&1 |
        grep -E '^\[kernels\]' || true
fi

[ -f "$out" ] || { echo "no $out - did the probe run?" >&2; exit 1; }

check="$repo/traced-kernel-check"
cp "$out" "$check/src/kernel.rs"

rooms="$repo/target/traced-kernels"
if [ -d "$rooms" ]; then
    rm -rf "$check/src/kernels"
    cp -r "$rooms" "$check/src/kernels"
    echo "== room kernels: $(ls "$check/src/kernels" | grep -c '^kernel') shapes, \
$(cat "$check/src/kernels"/kernel*.rs | wc -l) lines"
fi

echo "== compiling and RUNNING $(wc -l < "$out") lines in $check"
cd "$check" && cargo test --release -- --nocapture
