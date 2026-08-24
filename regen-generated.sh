#!/usr/bin/env bash
# Regenerate the CHECKED-IN traced kernel set:
#
#   crates/celeste-kernels/src/traced/    one kernel per heap SHAPE the
#                                         start room reaches
#
# Run this after any change to src/transpile/ or src/trace/, then read the
# diff and commit it. `trace::kernel::tests::traced_kernels_are_current`
# fails until you do.
#
# READ THE DIFF. The expected diff for a pure refactor is EMPTY; anything
# else is a change to what the kernels compute, and the gate on that is
# `traced_kernels_reproduce_the_interpreter` plus a
# CELESTE_COMPILED_FORWARD=check run, not this script.
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

SCRATCH=$(mktemp -d)
trap 'rm -rf "$SCRATCH"' EXIT

# QUICK, not release. `transpile` prints text - no number anyone quotes
# comes out of it - so `lto = "fat"` + `codegen-units = 1` buys nothing
# here (CLAUDE.md "Tools get --profile quick").
echo "==> building the generator"
./safe-run.sh -- ./one-cargo.sh cargo build --profile quick --bin transpile

# No recipe and no witness - the tracer walks the room itself.
echo "==> generating the traced set"
./safe-run.sh -- ./target/quick/transpile --room-kernels "$SCRATCH/traced"

echo "==> installing into the generated crate"
BACKUP=$(mktemp -d)
cp -r crates/celeste-kernels/src/traced "$BACKUP/traced"
restore() { rm -rf crates/celeste-kernels/src/traced;
            cp -r "$BACKUP/traced" crates/celeste-kernels/src/traced;
            rm -rf "$BACKUP"; }

# rm before cp, not cp over: one shape FEWER than last time would
# otherwise leave a stale kernelN.rs that still compiles.
rm -rf crates/celeste-kernels/src/traced
cp -r "$SCRATCH/traced" crates/celeste-kernels/src/traced

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
git --no-pager diff --stat crates/celeste-kernels/src/traced
git --no-pager status --short crates/celeste-kernels/src/traced
echo
echo "NOTE: this script builds under [profile.quick] and does NOT refresh"
echo "target/release. Before benchmarking or running the gate, build release"
echo "yourself."
