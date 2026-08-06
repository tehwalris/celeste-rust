#!/bin/bash
# Horizon-climbing driver for the precision refinement (task #81).
# Per horizon H: extend the level-0 forward pass to fH (resume), extend the
# sweep (chunk-incremental), then probe with the k=1 banded pass. A k=1
# refutation (no win by fH) bumps the horizon; a k=1 win stops the loop for
# the next precision level.
set -euo pipefail
cd "$(dirname "$0")"
DIR=~/celeste-checkpoints/room1
K1DIR=~/celeste-checkpoints/room1-k1
export CELESTE_FRONTIER_ONLY=1 CELESTE_DEOPT_COLLECT_FIRST=1
FROM=${1:-91}
TO=${2:-104}
for H in $(seq "$FROM" "$TO"); do
  echo "=== horizon $H: extend level-0 forward ==="
  ./safe-run.sh -- ./target/release/rewrite bench --frames "$H" --deopt \
      --checkpoint-dir "$DIR" --save-frames --resume
  echo "=== horizon $H: sweep ==="
  ./safe-run.sh -- ./target/release/rewrite sweep \
      --checkpoint-dir "$DIR" --frames "$H" --horizon "$H" | tail -30
  echo "=== horizon $H: k=1 banded probe ==="
  rm -rf "$K1DIR"
  CELESTE_REM_BITS=1 ./safe-run.sh -- ./target/release/rewrite bench \
      --frames "$H" --deopt --checkpoint-dir "$K1DIR" --save-frames \
      --band-dir "$DIR" --band-horizon "$H" --band-prev-bits 0 \
      | tee "/tmp/k1h$H.log" | tail -5
  if grep -q "first room-exit" "/tmp/k1h$H.log"; then
    echo "=== K1 WINS AT HORIZON $H - stop for the next precision level ==="
    break
  fi
  echo "=== horizon $H REFUTED at k=1 ==="
done
