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

# The nine generation jobs are independent processes writing to distinct
# scratch files, so they run CONCURRENTLY. Serially this was ~147 s of the
# script's ~5 min (8.5 s name tables, 3 x ~9 s for room (1,0), 5 x ~22 s
# for room (2,0)); in parallel it is one r20 kernel, ~22 s. Each peaks at
# ~48 MB, so nine at once is nothing.
#
# Output is captured per job and replayed in a fixed order afterwards -
# the emitter's census goes to stderr and is worth reading, and nine
# interleaved streams would not be.
R20_CLASSES="steady dash frozen dying_fall dying_spikes"
JOBS=""
start() {  # start NAME CMD...
    local name="$1"; shift
    "$@" >"$SCRATCH/$name.log" 2>&1 &
    JOBS="$JOBS $!:$name"
}

echo "==> generating name tables + 8 kernels (parallel)"
start names ./target/release/transpile --recipe rewrites-compile.jsonl "$SCRATCH/gen.rs"
for class in steady dash frozen; do
    start "$class" ./target/release/transpile \
        --recipe "rewrites-trace10-$class.jsonl" \
        --kernel "crates/celeste-kernels/witness/$class-shape.json" \
        "$SCRATCH/kernel_gen_$class.rs"
done
# Room (2,0) class kernels: same emitter, (2,0) overlays and witnesses.
# CELESTE_START_ROOM matters - the recipes replay against the (2,0)
# compile. dying_fall/dying_spikes: module names use underscores, recipe
# and witness file names use hyphens; ${class//_/-} maps between them.
for class in $R20_CLASSES; do
    hy=${class//_/-}
    CELESTE_START_ROOM=2,0 start "r20_$class" ./target/release/transpile \
        --recipe "rewrites-trace20-$hy.jsonl" \
        --kernel "crates/celeste-kernels/witness/r20-$hy-shape.json" \
        "$SCRATCH/kernel_gen_r20_$class.rs"
done

failed=""
for job in $JOBS; do
    pid=${job%%:*}; name=${job#*:}
    wait "$pid" || failed="$failed $name"
done
for job in $JOBS; do
    name=${job#*:}
    echo "==> $name"
    sed 's/^/    /' "$SCRATCH/$name.log"
done
if [ -n "$failed" ]; then
    echo "!! generation failed for:$failed" >&2
    exit 1
fi

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
for class in $R20_CLASSES; do
    cp "$SCRATCH/kernel_gen_r20_$class.rs" "crates/celeste-kernels/src/kernel_gen_r20_$class.rs"
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
