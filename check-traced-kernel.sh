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
# SINGLE-FRAME kernel below is generated, ~400 KB, and changes on every
# emitter tweak; putting it under `crates/celeste-kernels` would put it
# inside the bootstrap the checked-in kernels already have, where a
# kernel that fails to compile also stops you rebuilding the tool that
# would fix it. (The ROOM set is checked in - `regen-generated.sh`
# installs it only if the workspace still builds, which is the same
# escape by a different route.)
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

    # The ROOM's set is no longer rendered here. It is CHECKED IN, one
    # crate per room (`crates/celeste-kernels-room00/src/traced` etc.,
    # aggregated by `crates/celeste-kernels`), so the workspace build
    # compiles it and `traced_kernels_are_current` catches it going
    # stale. Regenerate it with ./regen-generated.sh.
fi

[ -f "$out" ] || { echo "no $out - did the probe run?" >&2; exit 1; }

check="$repo/traced-kernel-check"
cp "$out" "$check/src/kernel.rs"

echo "== compiling and RUNNING $(wc -l < "$out") lines in $check"
cd "$check" && cargo test --release -- --nocapture
