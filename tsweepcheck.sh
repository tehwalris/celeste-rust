#!/bin/bash
# Gate for the backward sweep (rewrite::sweep_time).
#
# Two claims, both exact:
#
#   1. It reproduces the certified `g` of a level whose edge sweep is
#      already certified - element-wise, THRESHOLDED at the horizon. A
#      horizon-H sweep never looks past H, so it produces g exactly where
#      e + g <= H and cannot be compared against the full array.
#   2. Thread count is not visible in the answer: 1 and 16 threads produce
#      byte-identical g.bin.
#
# It runs on a certified BANDED level rather than a fresh small universe
# because a small universe has no wins at all - room (1,0) first exits at
# frame 90 - so its g is all-unreachable and gates nothing. That is the hole
# the deleted sweepcheck.sh had, hidden because it also compared the edge
# shards. The banded levels are the cheapest artifacts that have real wins,
# real expansions and a certified answer.
#
# Never gate on the reported optimum: a wrong sweep printed the correct
# "abstract optimal win frame 90" while g was wrong for 95% of rows,
# because the win chain happens to be stamp-monotone.
set -euo pipefail
cd "$(dirname "$0")"
K=${1:-8}
H=${2:-100}
SRC=${3:-~/celeste-checkpoints/room1-k$K}
SRC=$(eval echo "$SRC")
if [ ! -d "$SRC/frames" ]; then
  echo "no certified level at $SRC - pass one as \$3" >&2
  exit 2
fi
export CELESTE_FRONTIER_ONLY=1 CELESTE_DEOPT_COLLECT_FIRST=1
export CELESTE_MAX_STATE_LANES=8000 CELESTE_FRUIT_CHUNK_LANES=8000
export CELESTE_REM_BITS=$K

run() { # $1 threads, $2 dir
  rm -rf "$2"
  cp -a "$SRC" "$2"
  # The position graph is rebuilt per run, so its recording is gated too.
  rm -f "$2/posgraph.bin"
  CELESTE_FRAME_THREADS=$1 ./safe-run.sh -- ./target/release/rewrite sweep \
      --banded --checkpoint-dir "$2" --frames "$H" --horizon "$H" 2>&1
}

a=$(run 1 /tmp/tsweepcheck-a)
b=$(run 16 /tmp/tsweepcheck-b)
echo "1 thread : $(echo "$a" | grep -E 'can reach the exit')"
echo "16 thread: $(echo "$b" | grep -E 'can reach the exit')"

fail=0
if [ "$(sha256sum < /tmp/tsweepcheck-a/g.bin)" = "$(sha256sum < /tmp/tsweepcheck-b/g.bin)" ]
then echo "threads  : g.bin byte-identical"
else echo "threads  : *** DIVERGED ***"; fail=1
fi

if python3 tools/gdiff.py "$SRC/g.bin" /tmp/tsweepcheck-b/g.bin \
      --threshold "$H" --meta "/tmp/tsweepcheck-b/f$(printf %03d "$H")/meta.json"
then echo "certified: g matches the edge sweep's, thresholded at $H"
else echo "certified: *** DIVERGED ***"; fail=1
fi
exit $fail
