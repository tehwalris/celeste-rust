#!/bin/bash
# Batch invariance: the guiding invariant of the whole vectorised design.
#
#   Running a set of lanes as one batch must give the same result as
#   partitioning it into subsets, running each, and unioning the outputs.
#   Threading, batch sizes and chunk caps are scheduling decisions and must
#   not be visible in the answer.
#
# The check compares the SET of canonical row keys the search has reached
# after N frames, across configurations that differ only in how lanes were
# grouped. Row IDS are assignment-order and legitimately differ; the SET
# must not. That is why this compares a sorted digest rather than bytes -
# parcheck.sh already covers the stronger byte-identity claim for threads
# alone, at a fixed grouping.
#
# Usage: ./simdcheck.sh [frames] [room] [recipe]
set -uo pipefail
cd "$(dirname "$0")"
F=${1:-45}
ROOM=${2:-1,0}
RECIPE=${3:-rewrites.jsonl}
BIN=${SIMDCHECK_BIN:-./target/release/rewrite}
export CELESTE_FRONTIER_ONLY=1 CELESTE_DEOPT_COLLECT_FIRST=1 CELESTE_START_ROOM="$ROOM"

run() { # $1 label, $2 threads, $3 cap (fruit cap removed - it is a no-op now)
  local dir=/tmp/simdcheck-$1
  rm -rf "$dir"
  CELESTE_FRAME_THREADS=$2 CELESTE_MAX_STATE_LANES=$3 \
    ./safe-run.sh -- "$BIN" --recipe "$RECIPE" bench --frames "$F" --deopt \
      --checkpoint-dir "$dir" --checkpoint-every "$F" > "/tmp/simdcheck-$1.log" 2>&1
  local d
  d=$(python3 /tmp/rowset.py "$dir"/f*/visited.bin 2>/dev/null)
  printf '%-28s %s\n' "$1" "${d:-FAILED - see /tmp/simdcheck-$1.log}" >&2
  echo "$d"
}

echo "batch invariance, room $ROOM, $F frames"
base=$(run "t16-cap8000-fruit8000"  16 8000   8000  | tail -1)
a=$(run    "t1-cap8000-fruit8000"    1 8000   8000  | tail -1)
b=$(run    "t16-cap1000-fruit1000"  16 1000   1000  | tail -1)
c=$(run    "t4-cap97-fruit97"        4   97     97  | tail -1)

fail=0
for x in "$a" "$b" "$c"; do
  [ "$x" = "$base" ] || fail=1
done
if [ "$fail" -eq 0 ]; then
  echo "INVARIANT HOLDS: the reached row set is identical under every grouping"
else
  echo "*** INVARIANT VIOLATED: how lanes were grouped changed the answer ***"
fi
exit $fail
