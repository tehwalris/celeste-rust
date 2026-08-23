#!/usr/bin/env bash
# Does the kernel rendered from a TRACED frame actually COMPILE?
#
# `trace::kernel::render` producing 6,000 lines only says the emitter
# emitted something. Whether those lines are Rust is a separate question
# and rustc is the only thing that answers it.
#
# Deliberately NOT part of the build. The traced kernel is not checked in
# yet: it changes on every emitter tweak, it is ~300 KB, and putting it
# under `crates/celeste-kernels` would put it inside the bootstrap the
# checked-in kernels already have (the emitters live in a crate that
# depends on the crate they generate). Compiling it out-of-tree keeps
# that loop out of the way until the kernel is worth keeping.
#
#   ./check-traced-kernel.sh
#
# Regenerates target/traced-kernel.rs first unless SKIP_RENDER is set.
set -euo pipefail

repo="$(cd "$(dirname "$0")" && pwd)"
out="$repo/target/traced-kernel.rs"
scratch="${SCRATCH:-/tmp/traced-kernel-check}"

if [ -z "${SKIP_RENDER:-}" ]; then
    echo "== rendering (the emit probe writes $out)"
    cargo nextest run --cargo-profile quick \
        the_kernel_emitter_lowers_a_traced_graph --no-capture 2>&1 |
        grep -E '^\[emit\] (FUSED|rendered|RENDER)' || true
fi

[ -f "$out" ] || { echo "no $out - did the probe run?" >&2; exit 1; }

mkdir -p "$scratch/src"
cat > "$scratch/Cargo.toml" <<EOF
[package]
name = "traced-kernel-check"
version = "0.1.0"
edition = "2021"

[dependencies]
celeste-engine = { path = "$repo/crates/celeste-engine" }
celeste-core = { path = "$repo/crates/celeste-core" }

[workspace]
EOF
cp "$out" "$scratch/src/lib.rs"

echo "== compiling $(wc -l < "$out") lines in $scratch"
cd "$scratch" && cargo build
echo "== the traced kernel compiles"
