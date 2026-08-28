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
#
# ROOM/RECIPE select the room, exactly as ladder.sh does. The default room
# (1,0) is the one with a CERTIFIED g, so it is the only place claim 1 can be
# made against an independent answer. Room (0,0) has no certified g - the edge
# sweep that would have produced one is the one that OOMs - so pointing this
# at a room-(0,0) level checks claims 1' and 2 instead: that a re-run
# reproduces the level's own g element-wise, and that the thread count is not
# visible in it. That is reproducibility and thread-invariance, not
# certification, and it is the room that CAN violate batch invariance, so it
# is the one worth checking.
set -euo pipefail
cd "$(dirname "$0")"
K=${1:-8}
H=${2:-100}
ROOM=${ROOM:-1,0}
export CELESTE_START_ROOM="$ROOM"
if [ "$ROOM" = "1,0" ]; then STEM=room1; else STEM=room$(echo "$ROOM" | tr -d ' ,'); fi
RECIPE=${RECIPE:-rewrites.jsonl}
SRC=${3:-~/celeste-checkpoints/$STEM-k$K}
SRC=$(eval echo "$SRC")
if [ ! -d "$SRC/frames" ]; then
  echo "no certified level at $SRC - pass one as \$3" >&2
  exit 2
fi
export CELESTE_FRONTIER_ONLY=1 CELESTE_DEOPT_COLLECT_FIRST=1
export CELESTE_MAX_STATE_LANES=8000
export CELESTE_REM_BITS=$K

run() { # $1 threads, $2 dir
  rm -rf "$2"
  cp -a "$SRC" "$2"
  # The position graph is rebuilt per run, so its recording is gated too.
  rm -f "$2/posgraph.bin"
  CELESTE_FRAME_THREADS=$1 ./safe-run.sh -- ./target/release/rewrite \
      --recipe "$RECIPE" sweep \
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
