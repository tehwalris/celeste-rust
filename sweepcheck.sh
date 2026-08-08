#!/bin/bash
# Determinism gate for the parallel sweep replay.
#
# The sweep goes through AbstractRun::step on the PHASED path, so it now
# runs its chunks on worker threads. Its output is the predecessor edge
# set, and every downstream claim (g, the bands, the witness) is written in
# terms of it - so the gate is that the edge files are byte-identical to a
# single-threaded sweep, not merely the same size.
#
# Runs on its own small universe so it cannot disturb a campaign's
# checkpoints, and so the sweep is not incrementally reusing shards.
set -euo pipefail
cd "$(dirname "$0")"
F=${1:-40}
export CELESTE_FRONTIER_ONLY=1 CELESTE_DEOPT_COLLECT_FIRST=1
DIR=/tmp/sweepcheck
rm -rf "$DIR" "$DIR-a" "$DIR-b"
./safe-run.sh -- ./target/release/rewrite bench --frames "$F" --deopt \
    --checkpoint-dir "$DIR" --save-frames --checkpoint-every "$F" > /dev/null 2>&1

run() { # $1 threads, $2 dir
  cp -a "$DIR" "$2"
  CELESTE_FRAME_THREADS=$1 CELESTE_MAX_STATE_LANES=8000 ./safe-run.sh -- \
      ./target/release/rewrite sweep --checkpoint-dir "$2" \
      --frames "$F" --horizon "$F" 2>&1
}
a=$(run 1 "$DIR-a")
b=$(run 16 "$DIR-b")
echo "1 thread : $(echo "$a" | grep -E 'abstract optimal|win seeds' | head -2 | tr '\n' ' ')"
echo "16 thread: $(echo "$b" | grep -E 'abstract optimal|win seeds' | head -2 | tr '\n' ' ')"
if diff <(cd "$DIR-a" && find . -name 'shard-*' -o -name '*.g' -o -name 'g.bin' | sort | xargs sha256sum 2>/dev/null) \
        <(cd "$DIR-b" && find . -name 'shard-*' -o -name '*.g' -o -name 'g.bin' | sort | xargs sha256sum 2>/dev/null) > /dev/null
then echo "sweep artifacts byte-identical"
else echo "*** SWEEP DIVERGED ***"; exit 1
fi
