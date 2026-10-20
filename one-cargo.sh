#!/usr/bin/env bash
# Run ONE cargo at a time, by waiting rather than by remembering: a second
# cargo on the same build directory prints `Blocking waiting for file lock
# on build directory` once, into a log nobody tails, and then appears to
# take as long as the first one has left.
#
#   ./safe-run.sh -- ./one-cargo.sh cargo check --all-targets
#   ./safe-run.sh -- ./one-cargo.sh cargo nextest run --cargo-profile quick
#
# The lock is per BUILD DIRECTORY, because that is what cargo contends
# on: two builds in different target dirs may safely overlap.
set -euo pipefail

# THE MEMORY CAP (./safe-run.sh): refused without one, as the binaries
# refuse it (`settings::refuse_uncapped`): a finite memory.max on this
# process's cgroup or an ancestor's.
cg=$(sed -n 's/^0:://p' /proc/self/cgroup)
d=$cg
until [[ $(cat "/sys/fs/cgroup${d%/}/memory.max" 2>/dev/null || true) =~ ^[0-9]+$ ]]; do
    if [[ -z $d || $d == / ]]; then
        echo "one-cargo: no memory cap (cgroup ${cg:-unknown}, memory.max \`max\` up to the root): run it under ./safe-run.sh" >&2
        exit 2
    fi
    d=$(dirname "$d")
done

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
