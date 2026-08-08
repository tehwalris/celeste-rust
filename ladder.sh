#!/bin/bash
# Full precision-ladder driver (plans/refinement-plan.md). Per horizon H:
# level 0 extends and sweeps, then levels k=1..16 run banded by level k-1's
# (e,g); the first refuting level bumps the horizon; a k=16 (exact) win is
# the concrete optimum. Level>=1 runs are tube-confined and cheap; they are
# rebuilt per (k,H).
set -euo pipefail
cd "$(dirname "$0")"
# ROOM=x,y selects the start room (default 1,0). Must match
# game_runner::room_dir_stem: "room1" for the default, "room<x><y>" else.
# RECIPE selects the rewrite recipe (default rewrites.jsonl; room (0,0)
# uses rewrites-room00.jsonl - the shape-agnostic subset).
ROOM=${ROOM:-1,0}
export CELESTE_START_ROOM="$ROOM"
if [ "$ROOM" = "1,0" ]; then STEM=room1; else STEM=room$(echo "$ROOM" | tr -d ' ,'); fi
RECIPE=${RECIPE:-rewrites.jsonl}
L0=~/celeste-checkpoints/$STEM
export CELESTE_FRONTIER_ONLY=1 CELESTE_DEOPT_COLLECT_FIRST=1
FROM=${1:-94}
TO=${2:-104}
MAXK=${3:-16}
for H in $(seq "$FROM" "$TO"); do
  echo "=== horizon $H: level 0 extend + sweep ==="
  ./safe-run.sh -- ./target/release/rewrite --recipe "$RECIPE" bench --frames "$H" --deopt \
      --checkpoint-dir "$L0" --save-frames --resume > /tmp/l0-h$H.log 2>&1
  tail -2 /tmp/l0-h$H.log
  # The sweep's origin-tagged plain replays of fruit states blow up on
  # UnknownBool branch doubling; a much tighter per-state lane cap than the
  # forward pass needs (see the h88 OOM postmortem in room00-plan.md).
  #
  # The base cap came down 100000 -> 8000 when the replay went parallel:
  # the thing the old figure bounded was the transient of ONE chunk in
  # flight, and there are now 16. 16 x 8000 is below the old 1 x 100000,
  # and small chunks are what give the threads work at all (a 250k-lane
  # sweep batch was only 3 chunks at the old cap). The fruit divisor comes
  # down with it so the fruit chunk stays ~1000 lanes rather than 80.
  CELESTE_MAX_STATE_LANES=8000 CELESTE_FRUIT_CHUNK_LANES=1000 ./safe-run.sh -- ./target/release/rewrite --recipe "$RECIPE" sweep \
      --checkpoint-dir "$L0" --frames "$H" --horizon "$H" > /tmp/l0sweep-h$H.log 2>&1
  grep -E "abstract optimal|win seeds" /tmp/l0sweep-h$H.log
  refuted=0
  for K in $(seq 1 "$MAXK"); do
    PREV_BITS=$((K - 1))
    if [ "$K" -eq 1 ]; then PREV=$L0; else PREV=~/celeste-checkpoints/$STEM-k$PREV_BITS; fi
    KDIR=~/celeste-checkpoints/$STEM-k$K
    echo "=== horizon $H: k=$K banded (band from bits $PREV_BITS) ==="
    rm -rf "$KDIR"
    CELESTE_REM_BITS=$K ./safe-run.sh -- ./target/release/rewrite --recipe "$RECIPE" bench \
        --frames "$H" --deopt --checkpoint-dir "$KDIR" --save-frames \
        --band-dir "$PREV" --band-horizon "$H" --band-prev-bits "$PREV_BITS" \
        > "/tmp/k$K-h$H.log" 2>&1
    tail -3 "/tmp/k$K-h$H.log" | head -1
    if ! grep -q "first room-exit" "/tmp/k$K-h$H.log"; then
      echo "=== horizon $H REFUTED at k=$K ==="
      refuted=1
      break
    fi
    echo "=== horizon $H: k=$K wins; sweeping level $K ==="
    CELESTE_REM_BITS=$K CELESTE_MAX_STATE_LANES=8000 CELESTE_FRUIT_CHUNK_LANES=1000 ./safe-run.sh -- ./target/release/rewrite --recipe "$RECIPE" sweep --banded \
        --checkpoint-dir "$KDIR" --frames "$H" --horizon "$H" \
        > "/tmp/k${K}sweep-h$H.log" 2>&1
    grep -E "abstract optimal|win seeds" "/tmp/k${K}sweep-h$H.log"
  done
  if [ "$refuted" -eq 0 ]; then
    echo "=== ALL LEVELS THROUGH k=$MAXK WIN AT HORIZON $H: CONCRETE OPTIMUM = $H ==="
    break
  fi
done
