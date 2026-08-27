#!/usr/bin/env bash
# Regenerate the CHECKED-IN kernel sets, ONE CRATE PER ROOM:
#
#   crates/celeste-kernels-room00/src/{traced,ladder,exact}/room00/
#   crates/celeste-kernels-room10/src/{traced,ladder,exact}/room10/
#   crates/celeste-kernels-room20/src/{traced,ladder,exact}/room20/
#
# (extend ROOMS below, and add a celeste-kernels-room<xy> crate, to add a
# room) plus the AGGREGATOR crate, `crates/celeste-kernels`, whose
# `src/{traced,ladder,exact}/mod.rs` re-exports every room crate's
# `KERNELS` into one `SETS` table the dispatcher flattens. Every set is
# CONSTANT-LATTICE specialized (plans/specialize.md "Spec: latticeify
# everything, all rooms, one table"). The per-room split exists so that
# touching one room's kernels only recompiles that room's crate, not the
# ~900k-line whole.
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
# depends on celeste-kernels (and its room crates), whose contents it
# produces; installing a kernel that does not compile means
# `cargo build --bin transpile` can no longer build the tool that would
# fix it. (Recovery, if you get there anyway: `git checkout
# crates/celeste-kernels/src crates/celeste-kernels-room00/src
# crates/celeste-kernels-room10/src crates/celeste-kernels-room20/src`,
# build, re-run this.)
set -euo pipefail
cd "$(dirname "$0")"

ROOMS="0,0 1,0 2,0"
VARIANTS="traced ladder exact"
TAGS="00 10 20"

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
# room's constant-lattice fixpoint itself - no recipe, no witness. Each
# (room, variant) writes into its own scratch subdir, mirroring where it
# will be installed: crates/celeste-kernels-room<tag>/src/<variant>/.
for variant in $VARIANTS; do
    case "$variant" in
        traced) flag=--room-kernels ;;
        ladder) flag=--room-kernels-ladder ;;
        exact)  flag=--room-kernels-exact ;;
    esac
    for room in $ROOMS; do
        tag="${room//,/}"
        echo "==> generating $variant for room ($room)"
        CELESTE_START_ROOM="$room" ./safe-run.sh -- \
            ./target/quick/transpile "$flag" "$SCRATCH/room$tag/$variant"
    done
done

# The merge step reads every room's freshly generated scratch dir (not
# the committed one) and writes the AGGREGATOR's mod.rs for that variant
# - this is what makes the fingerprint reflect what is about to be
# installed rather than what is already committed.
for variant in $VARIANTS; do
    echo "==> merging $variant"
    ./safe-run.sh -- ./target/quick/transpile --merge-kernels "$variant" \
        "$SCRATCH/aggregator/$variant" \
        "$SCRATCH/room00/$variant" "$SCRATCH/room10/$variant" "$SCRATCH/room20/$variant"
done

echo "==> installing into the generated crates"
BACKUP=$(mktemp -d)
for tag in $TAGS; do
    for variant in $VARIANTS; do
        cp -r "crates/celeste-kernels-room$tag/src/$variant" "$BACKUP/room$tag-$variant"
    done
done
for variant in $VARIANTS; do
    cp -r "crates/celeste-kernels/src/$variant" "$BACKUP/aggregator-$variant"
done
restore() {
    for tag in $TAGS; do
        for variant in $VARIANTS; do
            rm -rf "crates/celeste-kernels-room$tag/src/$variant"
            cp -r "$BACKUP/room$tag-$variant" "crates/celeste-kernels-room$tag/src/$variant"
        done
    done
    for variant in $VARIANTS; do
        rm -rf "crates/celeste-kernels/src/$variant"
        cp -r "$BACKUP/aggregator-$variant" "crates/celeste-kernels/src/$variant"
    done
    rm -rf "$BACKUP"
}

# rm before cp, not cp over: one shape FEWER than last time would
# otherwise leave a stale kernelN.rs that still compiles.
for tag in $TAGS; do
    for variant in $VARIANTS; do
        rm -rf "crates/celeste-kernels-room$tag/src/$variant"
        cp -r "$SCRATCH/room$tag/$variant" "crates/celeste-kernels-room$tag/src/$variant"
    done
done
for variant in $VARIANTS; do
    rm -rf "crates/celeste-kernels/src/$variant"
    cp -r "$SCRATCH/aggregator/$variant" "crates/celeste-kernels/src/$variant"
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
git --no-pager diff --stat crates/celeste-kernels/src crates/celeste-kernels-room00/src crates/celeste-kernels-room10/src crates/celeste-kernels-room20/src
git --no-pager status --short crates/celeste-kernels/src crates/celeste-kernels-room00/src crates/celeste-kernels-room10/src crates/celeste-kernels-room20/src
echo
echo "NOTE: this script builds under [profile.quick] and does NOT refresh"
echo "target/release. Before benchmarking or running the gate, build release"
echo "yourself."
