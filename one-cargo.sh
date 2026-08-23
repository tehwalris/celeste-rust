#!/usr/bin/env bash
# Run ONE cargo at a time, by waiting rather than by remembering.
#
# CLAUDE.md says "one cargo at a time", and the failure mode when you
# forget is not an error - it is `Blocking waiting for file lock on
# build directory`, printed once, into a log nobody is tailing. The
# second build then appears to take as long as the first one has left.
#
# On 2026-08-23 that cost hours: builds "taking 25 minutes" were a
# 70-second build queued behind another of mine, and it also produced a
# false alarm about a hung test.
#
#   ./one-cargo.sh cargo test --release --test room
#   ./one-cargo.sh cargo nextest run --cargo-profile quick
#
# The lock is per BUILD DIRECTORY, because that is what cargo contends
# on: two builds in different target dirs may safely overlap.
set -euo pipefail

dir="$(cd "$(dirname "$0")" && pwd)"
lock="${CARGO_TARGET_DIR:-$PWD/target}"
mkdir -p "$lock"
lockfile="$lock/.one-cargo.lock"

exec 9>"$lockfile"
if ! flock -n 9; then
    echo "one-cargo: another cargo holds $lockfile - waiting" >&2
    flock 9
    echo "one-cargo: got it" >&2
fi
"$@"
