#!/bin/bash
# Gate for SHARING the level-0 position graph with every banded level
# (`sweep --pos-graph-from DIR`, ladder.sh SHARE_POSGRAPH=1), which removes
# 16 rebuild stages per horizon.
#
# The claim is that level 0's table CONTAINS every finer level's, so the
# borrowed table can only shrink the sweep's candidate set - never lose a
# predecessor. That is the ladder's own over-approximation argument (the
# widened coordinates are sub-pixel; the table is keyed on whole-pixel
# cells; a banded level is restricted further still), but an argument is
# not a check, so:
#
#   1. SUBSET. Each level's own table is a subset of level 0's. Printed
#      per level, and a single pair outside is a failure.
#   2. SAME ANSWER. `g.bin` is BYTE-identical rebuilt vs borrowed. Byte
#      comparison is the right test here and only here: both sweeps read
#      the same checkpoint dir, so the row ids are the same assignment
#      (unlike posgraphcheck.sh, which compares two different forward
#      passes and has to join through the row keys).
#   3. THE DIRECTION IS ENFORCED. A FINER level's table is refused - that
#      is the unsound direction, and it must fail loudly rather than
#      quietly under-generate candidates.
#   4. IT IS PRECISION AND NOTHING ELSE. A table from a different search
#      (here: a different synthetic win) is refused even though it differs
#      in no precision component.
#
# Horizon is short and the win is SYNTHETIC (CELESTE_WIN_AT_XY, in the
# fingerprint) for the same reason as posgraphcheck.sh: a real room exit is
# 90+ frames away, and a vacuous `g` gates nothing.
set -euo pipefail
cd "$(dirname "$0")"
ROOM=${ROOM:-1,0}
RECIPE=${RECIPE:-rewrites.jsonl}
H=${H:-40}
MAXK=${MAXK:-3}
# The witness trajectory's position at frame 35 (tas/room_1_0_exit_frame_100
# .txt through concrete_run), so a concrete winning path exists inside H=40
# at EVERY level - the banded levels have to win too or there is nothing to
# compare. The gate checks this rather than trusting it: it aborts if level
# 0 finds no win.
export CELESTE_WIN_AT_XY=${CELESTE_WIN_AT_XY:-33,104}
export CELESTE_START_ROOM="$ROOM"
export CELESTE_FRONTIER_ONLY=1 CELESTE_DEOPT_COLLECT_FIRST=1
export CELESTE_MAX_STATE_LANES=8000
D=${D:-/tmp/posgraphsharecheck}
rm -rf "$D"; mkdir -p "$D"
R() { ./safe-run.sh -- ./target/release/rewrite --recipe "$RECIPE" "$@"; }
fail=0

echo "== level 0: forward (fused recording) + sweep"
R bench --frames "$H" --deopt --checkpoint-dir "$D/l0" --save-frames \
    --record-pos-graph > "$D/l0-bench.log" 2>&1
R sweep --checkpoint-dir "$D/l0" --frames "$H" --horizon "$H" > "$D/l0-sweep.log" 2>&1
grep -E "abstract optimal|no row reaches" "$D/l0-sweep.log" || true

# A vacuous level 0 would make every comparison below trivially pass.
if ! grep -q "abstract optimal" "$D/l0-sweep.log"; then
  echo "l0       : *** NO WIN at H=$H - the gate would be vacuous ***"
  exit 1
fi

compared=0
for K in $(seq 1 "$MAXK"); do
  PREV_BITS=$((K - 1))
  if [ "$K" -eq 1 ]; then PREV="$D/l0"; else PREV="$D/k$PREV_BITS"; fi
  KDIR="$D/k$K"
  echo "== k=$K: banded forward"
  env CELESTE_REM_BITS=$K ./safe-run.sh -- ./target/release/rewrite --recipe "$RECIPE" \
      bench --frames "$H" --deopt --checkpoint-dir "$KDIR" --save-frames \
      --band-dir "$PREV" --band-horizon "$H" --band-prev-bits "$PREV_BITS" \
      --record-pos-graph \
      > "$D/k$K-bench.log" 2>&1
  if ! grep -q "first room-exit" "$D/k$K-bench.log"; then
    echo "k=$K     : refuted at H=$H, no sweep to compare - stopping here"
    break
  fi

  echo "== k=$K: sweep with its OWN table (fused into the k forward above)"
  env CELESTE_REM_BITS=$K ./safe-run.sh -- ./target/release/rewrite --recipe "$RECIPE" \
      sweep --banded --checkpoint-dir "$KDIR" --frames "$H" --horizon "$H" \
      > "$D/k$K-sweep-own.log" 2>&1
  mv "$KDIR/g.bin" "$D/k$K-g-own.bin"

  # (1) subset
  python3 tools/posgraph_pairs.py --subset "$KDIR/posgraph.bin" "$D/l0/posgraph.bin" \
      | tee "$D/k$K-subset.txt"
  grep -q "^SUBSET" "$D/k$K-subset.txt" || { echo "k=$K subset: *** FAILED ***"; fail=1; }

  echo "== k=$K: sweep BORROWING level 0's table"
  # Keep the level's own table where control (3) can find it: the borrow
  # run must not see a table in its own dir, but deleting it outright is
  # how the finer-direction control ended up testing "file missing"
  # instead of "wrong direction" (2026-08-18).
  mkdir -p "$D/own-k$K"
  mv "$KDIR/posgraph.bin" "$D/own-k$K/posgraph.bin"
  env CELESTE_REM_BITS=$K ./safe-run.sh -- ./target/release/rewrite --recipe "$RECIPE" \
      sweep --banded --checkpoint-dir "$KDIR" --frames "$H" --horizon "$H" \
      --pos-graph-from "$D/l0" > "$D/k$K-sweep-borrowed.log" 2>&1
  grep -q "position graph borrowed" "$D/k$K-sweep-borrowed.log" \
      || { echo "k=$K     : *** the borrow path did not run ***"; fail=1; }
  if [ -e "$KDIR/posgraph.bin" ]; then
    echo "k=$K     : *** a borrowed table was written into the level's dir ***"
    fail=1
  fi

  # (2) same answer
  if cmp -s "$D/k$K-g-own.bin" "$KDIR/g.bin"; then
    echo "k=$K g   : byte-identical rebuilt vs borrowed"
    compared=$((compared + 1))
  else
    echo "k=$K g   : *** DIFFERS ***"
    fail=1
  fi
done

if [ "$compared" -lt 1 ]; then
  echo "levels   : *** nothing was compared - the gate is vacuous ***"
  fail=1
fi

# (3) the finer -> coarser direction must be refused. k=1's table is finer
# than level 0's, so level 0 borrowing it is the unsound direction.
echo "== control: level 0 borrowing a FINER level's table"
if R sweep --checkpoint-dir "$D/l0" --frames "$H" --horizon "$H" \
     --pos-graph-from "$D/own-k1" > "$D/control-finer.log" 2>&1; then
  echo "control  : *** a finer level's table was ACCEPTED ***"; fail=1
else
  grep -q "coarser than or equal" "$D/control-finer.log" \
    && echo "control  : a finer level's table is refused" \
    || { echo "control  : refused, but not for the right reason:"; \
         tail -3 "$D/control-finer.log"; fail=1; }
fi

# (4) a different SEARCH is refused even at the same precision.
echo "== control: a table from a different synthetic win"
CELESTE_WIN_AT_XY=30,100 ./safe-run.sh -- ./target/release/rewrite --recipe "$RECIPE" \
    bench --frames 5 --deopt --checkpoint-dir "$D/other" --save-frames \
    --record-pos-graph > "$D/other-bench.log" 2>&1
if env CELESTE_REM_BITS=1 ./safe-run.sh -- ./target/release/rewrite --recipe "$RECIPE" \
     sweep --banded --checkpoint-dir "$D/k1" --frames "$H" --horizon "$H" \
     --pos-graph-from "$D/other" > "$D/control-other.log" 2>&1; then
  echo "control  : *** another search's table was ACCEPTED ***"; fail=1
else
  grep -q "not this campaign at any precision" "$D/control-other.log" \
    && echo "control  : another search's table is refused" \
    || { echo "control  : refused, but not for the right reason:"; \
         tail -3 "$D/control-other.log"; fail=1; }
fi

exit $fail
