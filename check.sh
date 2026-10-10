#!/bin/bash
# THE CHECK LADDER (plans/devloop.md): what to run, from cheapest.
#
#   ./check.sh t0          the unit tests (nextest, quick profile)
#   ./check.sh t1 [--pin]  single frames from the fixtures (tools/fixtures.sh):
#                          the forward's exact output and metrics, KEY_CHECK,
#                          one backward frame, the storage replay, the
#                          kernels and the transfers against the reference
#   ./check.sh t2 [--pin]  short end-to-end runs: the room (1,0) gates, an
#                          objects-ladder search, a platform room's forward
#                          and one frame of it with its exact metrics
#   ./check.sh big [--pin] the reference frame (6,2) 100% f56 -> f57 and its
#                          12 GB capture replayed (fixtures: minutes, once)
#   ./check.sh all         t0, t1, t2
#
# Every step prints ok/FAIL and its wall time; the run exits non-zero if any
# step failed. `t1 --pin` writes the outputs as the new pins
# (gates/fixtures/) instead of comparing: only on evidence, and say why in
# the commit. Timings are reported, never pinned. Builds the quick binary
# first. Env: CARGO_TARGET_DIR, CELESTE_FIXTURES, CHECK_DIR (t2's scratch,
# default /var/tmp/celeste-check), THREADS (default 16: the metrics' units
# and slices depend on it).
set -uo pipefail
cd "$(dirname "$0")"
TIER=${1:-}
PIN=${2:-}
T=${CARGO_TARGET_DIR:-$PWD/target}
B=$T/quick/rewrite
export BIN=$B
F=${CELESTE_FIXTURES:-/var/tmp/celeste-fixtures}
G=gates/fixtures
C=${CHECK_DIR:-/var/tmp/celeste-check}
TH=${THREADS:-16}
L1_62="CELESTE_HUNDRED=1 CELESTE_LOADING_JANK=2 CELESTE_LEVEL_MINUS_ONE=94,5"
FAILED=()
mkdir -p "$C"
LOG=$C/log

# step NAME CMD...: run CMD (its output in $C/NAME.*), report ok/FAIL and time.
step() {
    local name=$1; shift
    local t0=$EPOCHREALTIME
    if "$@" > "$C/$name.out" 2> "$C/$name.err"; then
        printf '[%s] %-26s ok    %5.1f s\n' "$TIER" "$name" "$(awk "BEGIN{print $EPOCHREALTIME - $t0}")"
    else
        printf '[%s] %-26s FAIL  %5.1f s   (%s)\n' "$TIER" "$name" "$(awk "BEGIN{print $EPOCHREALTIME - $t0}")" "$C/$name.err"
        tail -15 "$C/$name.err" | sed 's/^/    /'
        FAILED+=("$name")
    fi
}

# same OUT PINNED: OUT equals the pinned file (or becomes it, with --pin).
same() {
    if [[ $PIN == --pin ]]; then
        cp "$1" "$2"
        echo "re-pinned $2" >&2
    else
        diff "$2" "$1" >&2
    fi
}

# frame NAME FRAME ROOM LEVEL ENV...: one forward frame from a fixture, its
# f/e lines and exact metrics against the pins; timings shown.
frame() {
    local name=$1 f=$2 room=$3 level=$4; shift 4
    env "$@" CELESTE_L1_TABLE="$F/$name/l1-table.bin" ./safe-run.sh -- "$B" bench-frame --level-dir "$F/$name" --frame "$f" --room "$room" --level "$level" --edges --threads "$TH" --metrics "$C/$name.metrics" > "$C/$name.frame" || return 1
    same "$C/$name.frame" "$G/$name.frame" || return 1
    if [[ $PIN == --pin ]]; then
        same "$C/$name.metrics" "$G/$name.metrics"
    else
        tools/metrics_diff.py "$G/$name.metrics" "$C/$name.metrics" >&2
    fi
}

build() {
    step build ./safe-run.sh -- ./one-cargo.sh cargo build --profile quick --bins
}

t0() {
    step nextest ./safe-run.sh -- ./one-cargo.sh cargo nextest run --cargo-profile quick
    grep -E 'Summary|SLOW|FAIL' "$C/nextest.err" | tail -3
}

t1() {
    build
    step fixtures tools/fixtures.sh ensure r10 r62h r42n r42x r10arc cap-r10
    grep -h '^\[fixtures\]' "$C/fixtures.err"
    # The forward, one frame each, exact (and the run's timings).
    step frame-r10 frame r10 55 1,0 r0sxh
    step frame-r62h frame r62h 50 6,2 r0sxhf $L1_62
    step frame-r42n frame r42n 60 4,2 r0sxhn CELESTE_LEVEL_MINUS_ONE=71,5
    for n in r10 r62h r42n; do grep -hE '^\[bench\] rep 0' "$C/frame-$n.err" | sed "s/^/    $n /" | cut -c1-200; done
    # Every emitted row's key recomputed from its fields (the boundary's).
    step keycheck-r10 keycheck
    # One backward frame (W f45 of h56), as the arc gate fingerprints it.
    step backward-r10arc backward
    grep -h '^\[bench-backward\] rep 0' "$C/backward-r10arc.err" | sed 's/^/    /'
    # The storage alone: r10's f56 emissions replayed (`bench-storage`).
    step storage-r10 storage r10 56 1,0 r0sxh
    # The kernels against the reference engine, sampled rows of one frame.
    step refcheck-r10 ./safe-run.sh -- "$B" ref-check --level-dir "$F/r10" --frame 55 --level r0sxh --room 1,0 --samples 32 --threads "$TH"
    step refcheck-r42x env CELESTE_LEVEL_MINUS_ONE=71,5 ./safe-run.sh -- "$B" ref-check --level-dir "$F/r42x" --frame 55 --level r0sxh --room 4,2 --samples 32 --threads "$TH"
    # The recorded transfers against the reference engine, sampled records.
    step arccheck-r10 ./safe-run.sh -- "$B" arc-check --level-dir "$F/r10" --room 1,0 --from 56 --to 56 --samples 32 --level r0sxh --threads "$TH" --path-cap 4096
    step arccheck-r42x ./safe-run.sh -- "$B" arc-check --level-dir "$F/r42x" --room 4,2 --from 56 --to 56 --samples 32 --level r0sxh --threads "$TH" --path-cap 4096
    for s in refcheck-r10 refcheck-r42x; do grep -h '^\[ref-check\] [0-9]' "$C/$s.out" | sed "s/^/    /"; done
    for s in arccheck-r10 arccheck-r42x; do grep -h '^agreement' "$C/$s.out" | sed "s/^/    /" | cut -c1-200; done
}

keycheck() {
    CELESTE_KERNEL_KEY_CHECK=1 ./safe-run.sh -- "$B" bench-frame --level-dir "$F/r10" --frame 55 --room 1,0 --level r0sxh --edges --threads "$TH" > "$C/keycheck.frame" || return 1
    diff "$G/r10.frame" "$C/keycheck.frame" >&2
}

backward() {
    ./safe-run.sh -- "$B" bench-backward --level-dir "$F/r10arc/level00" --horizon 56 --frame 45 --win-at 40,64 --threads "$TH" --reps 3 > "$C/backward.txt" || return 1
    same "$C/backward.txt" "$G/r10arc.backward"
}

# storage TREE FRAME ROOM LEVEL ENV...: the capture of TREE's frame FRAME
# replayed; its new states and edges are the frame check's (TREE.frame).
storage() {
    local tree=$1 f=$2 room=$3 level=$4; shift 4
    env "$@" ./safe-run.sh -- "$B" bench-storage --capture "$F/cap-$tree/f$(printf %03d "$f")" --tree "$F/$tree" --room "$room" --level "$level" --threads "$TH" --reps 1 > "$C/storage-$tree.txt" || return 1
    cat "$C/storage-$tree.txt" >&2
    { grep -o "f$(printf %03d "$f") [0-9]* [0-9a-f]*" "$C/storage-$tree.txt"; grep -o "e$(printf %03d "$f") [0-9]* [0-9a-f]*" "$C/storage-$tree.txt"; } | diff "$G/$tree.frame" - >&2
}

big() {
    build
    step fixtures-big tools/fixtures.sh ensure r62h57 cap-r62h57
    step frame-r62h57 frame r62h57 56 6,2 r0sxhf $L1_62
    grep -hE '^\[bench\] (kernels|rep)|^\[phases\]' "$C/frame-r62h57.err" | sed 's/^/    /' | cut -c1-220
    step storage-r62h57 storage r62h57 57 6,2 r0sxhf $L1_62
    sed 's/^/    /' "$C/storage-r62h57.txt" | cut -c1-220
}

t2() {
    build
    rm -rf "$C/t2"
    mkdir -p "$C/t2"
    step gate10-forward gate10_forward
    step gate10-arc gate10_arc
    step search42-objects search42
    step forward60-platforms forward60
    # The platform kernels' frame and exact metrics (their build is the
    # cost: ~20 s of tracing, hence here and not in t1).
    step fixtures-r60s tools/fixtures.sh ensure r60s
    step frame-r60s frame r60s 70 6,0 r0sxhfp CELESTE_SPLIT_FRAME=1
    grep -hE '^\[bench\] (kernels|rep)' "$C/frame-r60s.err" | sed 's/^/    /' | cut -c1-200
}

# The three pinned room (1,0) oracles (CLAUDE.md "Gates").
gate10_forward() {
    ./safe-run.sh -- "$B" forward --to 44 --room 1,0 --checkpoint-dir "$C/t2/g10" > "$C/t2/g10.out" || return 1
    grep '^posgraph' "$C/t2/g10.out" | diff gates/posgraph_room10_f044.txt - >&2 || return 1
    "$B" ckhash --to 44 --room 1,0 --checkpoint-dir "$C/t2/g10" | diff gates/ckhash_room10_f000-044.txt - >&2
}
gate10_arc() {
    ./safe-run.sh -- "$B" search --to 35 --win-at 9,101 --checkpoint-dir "$C/t2/arc10" --prefer gates/known_room10_win9-101.txt > "$C/t2/arc10.out" || return 1
    grep '^\[gate\]' "$C/t2/arc10.out" | diff gates/arc_room10_win9-101_h35.txt - >&2
}
# The objects ladder (r0sxhn filtering r0sxh) at a synthetic win, its
# witness as the known route: every level's [gate] lines pinned.
search42() {
    ./safe-run.sh -- "$B" search --room 4,2 --level r0sxhn,r0sxh --to 50 --win-at 32,56 --checkpoint-dir "$C/t2/s42" --prefer "$G/known_room42_win32-56.txt" > "$C/t2/s42.out" || return 1
    grep '^\[gate\]' "$C/t2/s42.out" > "$C/t2/s42.gate"
    same "$C/t2/s42.gate" "$G/search42.gate"
}
# A platform room (platforms unknown, split frame) to step 71: the tree's
# fingerprint is the r60s fixture's pin.
forward60() {
    CELESTE_SPLIT_FRAME=1 ./safe-run.sh -- "$B" forward --room 6,0 --level r0sxhfp --to 71 --checkpoint-dir "$C/t2/p60" > /dev/null || return 1
    "$B" ckhash --room 6,0 --to 71 --edges --checkpoint-dir "$C/t2/p60" 2> /dev/null | diff "$G/r60s.ckhash" - >&2
}

echo "[$TIER] $(uptime | sed 's/.*load/load/')"
T_START=$SECONDS
case $TIER in
    t0) t0 ;;
    t1) t1 ;;
    t2) t2 ;;
    big) big ;;
    all) TIER=t0; t0; TIER=t1; t1; TIER=t2; t2 ;;
    *) sed -n '2,20p' "$0"; exit 2 ;;
esac
echo "[$TIER] $((SECONDS - T_START)) s; $( ((${#FAILED[@]})) && echo "FAILED: ${FAILED[*]}" || echo "all ok")"
((${#FAILED[@]} == 0))
