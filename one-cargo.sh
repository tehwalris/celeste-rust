#!/usr/bin/env bash
# Run ONE cargo at a time, by waiting rather than by remembering: a second
# cargo on the same build directory prints `Blocking waiting for file lock
# on build directory` once, into a log nobody tails, and then appears to
# take as long as the first one has left.
#
#   ./one-cargo.sh cargo check --all-targets
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
