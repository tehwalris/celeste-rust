#!/bin/bash
# Gate for the FUSED position-graph recording (`bench --record-pos-graph`,
# ladder.sh FUSE=1), which replaces the separate replay stage
# (`rewrite pos-graph`) that is the memory high-water mark of a campaign.
#
# Three claims:
#
#   1. Recording does not change the SEARCH. The forward pass tags every
#      lane with its position cell mid-frame, which changes how lanes group
#      inside a frame - and on a room with a fruit, grouping is semantic
#      (an UnknownBool branch sends a whole chunk down both edges). So the
#      claim is checked where it matters: the visited row SETS must be
#      equal, and the sweep's `g` must be equal AS A FUNCTION OF THE ROW.
#      Row ids are assignment order and permute freely; comparing g.bin
#      bytes answers a question nobody asked (see tools/gjoin.py).
#   2. The recorded table CONTAINS the replay's. Too large is safe - the
#      table is only a candidate filter and the sweep expands what it
#      returns - too small silently loses predecessors. The fused table is
#      normally one pair larger: it sees the spawn's own first move, which
#      the replay cannot, because the replay starts from the frame-1 batch.
#   3. Recording survives --resume. A resumed pass only steps the frames it
#      runs, so it must seed itself from the table on disk; without that it
#      writes a table stamped with the full horizon holding only the tail.
#      Checked by building the same horizon in one process and in two.
#
# The horizon is deliberately short and the win is SYNTHETIC
# (CELESTE_WIN_AT_XY, in the fingerprint): a real room exit is 90+ frames
# away, so a check that used it would either take hours or compare an
# all-unreachable `g`, which gates nothing.
set -euo pipefail
cd "$(dirname "$0")"
ROOM=${ROOM:-2,0}
RECIPE=${RECIPE:-rewrites-room00.jsonl}
H=${H:-40}
SPLIT=${SPLIT:-30}
# A whole-pixel position the search reaches well before H, so `g` has real
# wins. The default is on room (2,0)'s witness trajectory (frame 35).
export CELESTE_WIN_AT_XY=${CELESTE_WIN_AT_XY:-26,108}
export CELESTE_START_ROOM="$ROOM"
export CELESTE_FRONTIER_ONLY=1 CELESTE_DEOPT_COLLECT_FIRST=1
export CELESTE_MAX_STATE_LANES=8000 CELESTE_FRUIT_CHUNK_LANES=8000
D=${D:-/tmp/posgraphcheck}
rm -rf "$D"; mkdir -p "$D"
R() { ./safe-run.sh -- ./target/release/rewrite --recipe "$RECIPE" "$@"; }

echo "== replay: bench, then a separate pos-graph stage"
R bench --frames "$H" --deopt --checkpoint-dir "$D/replay" --save-frames > "$D/replay-bench.log" 2>&1
R pos-graph --checkpoint-dir "$D/replay" --frames "$H" > "$D/replay-pg.log" 2>&1
R sweep --checkpoint-dir "$D/replay" --frames "$H" --horizon "$H" > "$D/replay-sweep.log" 2>&1

echo "== fused: one process"
R bench --frames "$H" --deopt --checkpoint-dir "$D/fused" --save-frames \
    --record-pos-graph > "$D/fused-bench.log" 2>&1
R sweep --checkpoint-dir "$D/fused" --frames "$H" --horizon "$H" > "$D/fused-sweep.log" 2>&1

echo "== fused: two processes, the second resuming at f$SPLIT"
R bench --frames "$SPLIT" --deopt --checkpoint-dir "$D/resumed" --save-frames \
    --checkpoint-every 1 --record-pos-graph > "$D/resumed-a.log" 2>&1
R bench --frames "$H" --deopt --checkpoint-dir "$D/resumed" --save-frames \
    --checkpoint-every 1 --resume --record-pos-graph > "$D/resumed-b.log" 2>&1

fail=0
F=$(printf 'f%03d' "$H")
echo
if python3 tools/rowdiff.py "$D/replay/$F/visited.bin" "$D/fused/$F/visited.bin" \
     | tee /dev/stderr | grep -q "onlyA=0 onlyB=0"
then echo "rows     : the recording does not change the row table"
else echo "rows     : *** DIVERGED ***"; fail=1
fi

if python3 tools/gjoin.py "$D/replay/g.bin" "$D/replay/$F/visited.bin" \
     "$D/fused/g.bin" "$D/fused/$F/visited.bin" | tee /dev/stderr \
     | grep -q "^IDENTICAL"
then echo "g        : identical as a function of the row"
else echo "g        : *** DIVERGED ***"; fail=1
fi

python3 - "$D/replay/posgraph.bin" "$D/fused/posgraph.bin" "$D/resumed/posgraph.bin" <<'EOF' || fail=1
import struct, sys

def load(path):
    d = open(path, 'rb').read()
    assert d[:4] == b'C8PZ', (path, d[:4])
    off = 4
    grid, origin, frames, fplen = struct.unpack_from('<iiiI', d, off); off += 16
    off += fplen
    pairs, = struct.unpack_from('<Q', d, off); off += 8
    ncell = grid * grid + 1
    offsets = struct.unpack_from('<%dI' % (ncell + 1), d, off); off += 4 * (ncell + 1)
    srcs = struct.unpack_from('<%dI' % pairs, d, off)
    return frames, {(dst, srcs[i]) for dst in range(ncell)
                    for i in range(offsets[dst], offsets[dst + 1])}

(_, replay), (_, fused), (_, resumed) = (load(p) for p in sys.argv[1:4])
ok = True
print(f"pairs    : replay={len(replay)} fused={len(fused)} resumed={len(resumed)}")
if replay <= fused:
    print(f"table    : fused contains the replay's ({len(fused - replay)} extra)")
else:
    print(f"table    : *** {len(replay - fused)} PAIRS MISSING from the fused table ***")
    ok = False
if resumed == fused:
    print("resume   : the resumed recording is the one-process table")
else:
    print(f"resume   : *** DIFFERS: {len(fused - resumed)} missing, "
          f"{len(resumed - fused)} extra ***")
    ok = False
raise SystemExit(0 if ok else 1)
EOF
exit $fail
