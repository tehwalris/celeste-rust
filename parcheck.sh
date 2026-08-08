#!/bin/bash
# Determinism gate for chunk-parallel frame execution.
#
# The claim being checked is exact, not statistical: at a FIXED chunk cap,
# running a frame's chunks on N threads must produce byte-identical
# checkpoints to running them on one. Row ids are assigned in insertion
# order and checkpoints, bands and the backward sweep are all written in
# terms of them, so anything weaker would not be enough.
#
# The chunk cap itself is a separate axis: changing it reorders the rows
# offered to the visited set within a frame, so ids differ (the row SET does
# not - the per-frame "new lanes" and "visited total" figures are equal, and
# that is checked here too).
set -euo pipefail
cd "$(dirname "$0")"
F=${1:-45}
CAP=${2:-8000}
export CELESTE_FRONTIER_ONLY=1 CELESTE_DEOPT_COLLECT_FIRST=1
export CELESTE_MAX_STATE_LANES=$CAP

run() { # $1 threads, $2 dir
  rm -rf "$2"
  CELESTE_FRAME_THREADS=$1 ./safe-run.sh -- ./target/release/rewrite bench \
      --frames "$F" --deopt --checkpoint-dir "$2" --checkpoint-every "$F" 2>&1
}

serial=$(run 1 /tmp/parcheck-serial)
par=$(run 16 /tmp/parcheck-par)
echo "serial   : $(echo "$serial" | grep -E '^rewritten')"
echo "16 thread: $(echo "$par" | grep -E '^rewritten')"

fail=0
if diff <(echo "$serial" | grep -oE '\-> [0-9]+ new lanes, visited total [0-9]+') \
        <(echo "$par"    | grep -oE '\-> [0-9]+ new lanes, visited total [0-9]+') > /dev/null
then echo "frontier : identical at every frame"
else echo "frontier : *** DIVERGED ***"; fail=1
fi

if diff <(sha256sum /tmp/parcheck-serial/f*/[sv]*.bin | cut -d' ' -f1) \
        <(sha256sum /tmp/parcheck-par/f*/[sv]*.bin    | cut -d' ' -f1) > /dev/null
then echo "artifacts: states.bin and visited.bin byte-identical"
else echo "artifacts: *** DIVERGED ***"; fail=1
fi
exit $fail
