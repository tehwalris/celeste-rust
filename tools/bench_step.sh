#!/bin/bash
# One frame of a real forward, end to end, repeatably: a scratch copy of a
# base tree that ENDS at frame F (so the door holds exactly the states up to
# F, as mid-run) is resumed and extended to F+1 by the real code path
# (`rewrite forward --to F+1`: kernels, dedup, door, raw edge records,
# checkpoint, pos graph, the tree's level -1 filter). The base's big files
# are hard-linked (a step only ADDS files), the small mutable ones copied.
#
#   tools/bench_step.sh BASE_TREE ROOM LEVEL REPS [BIN]
#   env: whatever the base was built with (CELESTE_HUNDRED, CELESTE_LOADING_JANK,
#   CELESTE_LEVEL_MINUS_ONE, ...), CELESTE_THREADS; PAUSE_PIDS: processes
#   stopped (SIGSTOP) for the timed run and resumed after, so a long search
#   on the same machine does not skew it. KEEP=1: leave the scratch tree.
#   FRAMES=N: run N frames (each later frame's wave printed too; only the
#   first can hold a lazy build in binaries before FrameStep::warm).
set -euo pipefail
BASE=$1; ROOM=$2; LEVEL=$3; REPS=$4
BIN=${5:-$(dirname "$0")/../target/release/rewrite}
F=$(cat "$BASE/edges/done.txt")
S=${SCRATCH:-/var/tmp/bench-step-scratch}
SAFE=$(dirname "$0")/../safe-run.sh
for rep in $(seq 1 "$REPS"); do
  rm -rf "$S"; mkdir -p "$S/edges/raw"
  cp -al "$BASE/frames" "$S/frames"
  [ -d "$BASE/dropped" ] && cp -al "$BASE/dropped" "$S/dropped"
  for d in "$BASE"/edges/l* "$BASE/edges/xfer" "$BASE"/edges/raw/f*; do [ -e "$d" ] || continue; case "$d" in */raw/*) cp -al "$d" "$S/edges/raw/";; *) cp -al "$d" "$S/edges/";; esac; done
  cp "$BASE/edges/done.txt" "$S/edges/"
  for f in "$BASE"/*; do [ -f "$f" ] && cp "$f" "$S/"; done
  sync
  [ -n "${PAUSE_PIDS:-}" ] && { kill -STOP $PAUSE_PIDS 2>/dev/null || true; }
  t0=$(date +%s.%N)
  "$SAFE" --memory "${MEM:-40G}" -- "$BIN" forward --to $((F + ${FRAMES:-1})) --room "$ROOM" --level "$LEVEL" --checkpoint-dir "$S" > "$S.out" 2> "$S.err"
  t1=$(date +%s.%N)
  [ -n "${PAUSE_PIDS:-}" ] && { kill -CONT $PAUSE_PIDS 2>/dev/null || true; }
  line=$(grep "^\[fwd\] f$(printf %03d $((F + 1)))" "$S.err" | sed -E 's/.*\| wave ([0-9]+) \(idle ([0-9]+)%\)( canon ([0-9]+))? door ([0-9]+) ckpt ([0-9]+) pos ([0-9]+) total ([0-9]+) ms.*/wave \1 ms (idle \2%) canon \4 door \5 ckpt \6 pos \7 frame \8 ms/')
  resume=$(grep -o "^\[resume\].*in [0-9.]* s\|, [0-9.]* s$" "$S.err" | head -1 | grep -o "[0-9.]* s$" || true)
  for g in $(seq 2 "${FRAMES:-1}"); do
    grep "^\[fwd\] f$(printf %03d $((F + g))) " "$S.err" | sed -E "s/.*\| wave ([0-9]+) \(idle [0-9]+%\)( canon ([0-9]+))? .*/rep $rep: f$((F + g)) wave \1 ms canon \3/"
  done
  echo "rep $rep: f$F->f$((F + 1)) | $line | process $(awk "BEGIN{printf \"%.1f\", $t1 - $t0}") s (resume ${resume:-?})"
done
grep -E "^kernel (calls|util)" "$S.err" | cut -c1-200
[ -n "${KEEP:-}" ] || rm -rf "$S"
