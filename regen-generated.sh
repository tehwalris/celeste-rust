#!/usr/bin/env bash
# Regenerate the CHECKED-IN kernel sets:
#
#   crates/celeste-kernels/src/traced/   the BASE (level-0) lattice set
#   crates/celeste-kernels/src/ladder/   the RUNG-AGNOSTIC lattice set
#   crates/celeste-kernels/src/exact/    the EXACT-REM lattice set
#
# each holding one room<x><y>/ subdirectory per generated room (rooms
# (0,0), (1,0), (2,0) today - extend ROOMS below to add one) plus a
# merged mod.rs whose SETS table is what the dispatcher flattens. Every
# set is CONSTANT-LATTICE specialized (plans/specialize.md "Spec:
# latticeify everything, all rooms, one table").
#
# Run this after any change to src/transpile/ or src/trace/, then read
# the diff and commit it. `trace::kernel::tests::{traced,ladder,exact}
# _kernels_are_current` fail until you do (they regenerate room (1,0)
# and re-check every room's fingerprint); the `#[ignore]`d
# `room00_kernels_are_current` / `room20_kernels_are_current` regenerate
# the other rooms.
#
# READ THE DIFF. The expected diff for a pure refactor is EMPTY; anything
# else is a change to what the kernels compute, and the gate on that is
# `traced_kernels_reproduce_the_interpreter` (and the per-rung and
# per-room differentials) plus a CELESTE_COMPILED_FORWARD=check run, not
# this script.
#
# (`crates/celeste-names/src/gen.rs` is NOT regenerated any more. It is
# FROZEN - its generator walked the rewritten IR and was deleted with the
# walk path, plans/delete-the-interpreter.md Phase 1 - and new names may
# only be APPENDED by hand, because `FIELD_NAMES`' order is the shape hash
# and the row key.)
#
# Generation happens into a scratch dir first and the result is only
# installed if the whole workspace still builds with it. That is not
# tidiness - it is the bootstrap. The emitter lives in celeste-rust, which
# depends on celeste-kernels, whose contents it produces; installing a
# kernel that does not compile means `cargo build --bin transpile` can no
# longer build the tool that would fix it. (Recovery, if you get there
# anyway: `git checkout crates/celeste-kernels/src`, build, re-run this.)
set -euo pipefail
cd "$(dirname "$0")"

ROOMS="0,0 1,0 2,0"
VARIANTS="traced ladder exact"

SCRATCH=$(mktemp -d)
trap 'rm -rf "$SCRATCH"' EXIT

# QUICK, not release. `transpile` prints text - no number anyone quotes
# comes out of it - so `lto = "fat"` + `codegen-units = 1` buys nothing
# here (CLAUDE.md "Tools get --profile quick").
echo "==> building the generator"
./safe-run.sh -- ./one-cargo.sh cargo build --profile quick --bin transpile

# One process per (room, variant): the start room is a process global
# (it feeds `_init`, the collision cache and the `sin` builtin), so the
# generator cannot switch rooms mid-process. The tracer walks each
# room's constant-lattice fixpoint itself - no recipe, no witness.
for variant in $VARIANTS; do
    case "$variant" in
        traced) flag=--room-kernels ;;
        ladder) flag=--room-kernels-ladder ;;
        exact)  flag=--room-kernels-exact ;;
    esac
    for room in $ROOMS; do
        echo "==> generating $variant for room ($room)"
        CELESTE_START_ROOM="$room" ./safe-run.sh -- \
            ./target/quick/transpile "$flag" "$SCRATCH/$variant"
    done
    echo "==> merging $variant"
    ./safe-run.sh -- ./target/quick/transpile --merge-kernels "$SCRATCH/$variant"
done

echo "==> installing into the generated crate"
BACKUP=$(mktemp -d)
for variant in $VARIANTS; do
    cp -r "crates/celeste-kernels/src/$variant" "$BACKUP/$variant"
done
restore() {
    for variant in $VARIANTS; do
        rm -rf "crates/celeste-kernels/src/$variant"
        cp -r "$BACKUP/$variant" "crates/celeste-kernels/src/$variant"
    done
    rm -rf "$BACKUP"
}

# rm before cp, not cp over: one shape FEWER than last time would
# otherwise leave a stale kernelN.rs that still compiles.
for variant in $VARIANTS; do
    rm -rf "crates/celeste-kernels/src/$variant"
    cp -r "$SCRATCH/$variant" "crates/celeste-kernels/src/$variant"
done

# Also quick: this step answers "does the generated code COMPILE", and
# release answers it no better for ~4x the wall time. It does not leave
# fresh release binaries behind - build those yourself before benching.
echo "==> checking the workspace still builds with it"
if ! ./safe-run.sh -- ./one-cargo.sh cargo build --profile quick; then
    echo "!! the regenerated code does not build - restoring the committed version" >&2
    restore
    exit 1
fi
rm -rf "$BACKUP"

echo
echo "regenerated. Now read the diff:"
git --no-pager diff --stat crates/celeste-kernels/src
git --no-pager status --short crates/celeste-kernels/src
echo
echo "NOTE: this script builds under [profile.quick] and does NOT refresh"
echo "target/release. Before benchmarking or running the gate, build release"
echo "yourself."
