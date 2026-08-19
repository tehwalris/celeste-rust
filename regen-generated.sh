#!/usr/bin/env bash
# Regenerate the two CHECKED-IN generated crates:
#
#   crates/celeste-names/src/gen.rs          the interned name tables
#   crates/celeste-kernels/src/kernel_gen_*  the per-class lane kernels
#
# Run this after any change to src/transpile/, then read the diff and commit
# it. `transpile::names::tests::generated_is_current` fails until you do.
#
# READ THE DIFF. `FIELD_NAMES`' order is the canonical field ordering the
# boundary hashes, so it feeds the shape hash, the row key, and what the
# search dedups on: a reordering here is a different search, not a cosmetic
# change. The expected diff for a pure refactor is EMPTY.
#
# Generation happens into a scratch dir first and the results are only
# installed if the whole workspace still builds with them. That is not
# tidiness - it is the bootstrap. The generators live in celeste-rust, which
# depends on celeste-kernels, whose contents they produce; installing a
# kernel that does not compile means `cargo build --bin transpile` can no
# longer build the tool that would fix it. (Recovery, if you get there
# anyway: `git checkout crates/celeste-kernels/src`, build, re-run this.)
set -euo pipefail
cd "$(dirname "$0")"

SCRATCH=$(mktemp -d)
trap 'rm -rf "$SCRATCH"' EXIT

echo "==> building the generator"
cargo build --release --bin transpile

echo "==> name tables"
./target/release/transpile --recipe rewrites-compile.jsonl "$SCRATCH/gen.rs"

for class in steady dash frozen; do
    echo "==> $class kernel"
    ./target/release/transpile \
        --recipe "rewrites-trace10-$class.jsonl" \
        --kernel "crates/celeste-kernels/witness/$class-shape.json" \
        "$SCRATCH/kernel_gen_$class.rs"
done

echo "==> installing into a scratch checkout of the generated crates"
BACKUP=$(mktemp -d)
cp crates/celeste-names/src/gen.rs "$BACKUP/"
cp crates/celeste-kernels/src/kernel_gen_*.rs "$BACKUP/"
restore() { cp "$BACKUP"/gen.rs crates/celeste-names/src/;
            cp "$BACKUP"/kernel_gen_*.rs crates/celeste-kernels/src/;
            rm -rf "$BACKUP"; }

cp "$SCRATCH/gen.rs" crates/celeste-names/src/gen.rs
for class in steady dash frozen; do
    cp "$SCRATCH/kernel_gen_$class.rs" "crates/celeste-kernels/src/kernel_gen_$class.rs"
done

echo "==> checking the workspace still builds with them"
if ! cargo build --release; then
    echo "!! the regenerated code does not build - restoring the committed version" >&2
    restore
    exit 1
fi
rm -rf "$BACKUP"

echo
echo "regenerated. Now read the diff:"
git --no-pager diff --stat crates/celeste-names/src/gen.rs crates/celeste-kernels/src/
echo
echo "NOTE: the build check above rewrote target/release binaries WITHOUT the"
echo "fused feature, and a later fused rebuild may see a fresh fingerprint and"
echo "skip the relink (BENCHMARK_DATA.md, M1 stage 3). If you use the fused"
echo "engine:  touch native-probe/src/main.rs && cargo build --release -p"
echo "native-probe --features celeste-rust/fused   and check for the"
echo "'fused: lanes' stderr line before benching."
