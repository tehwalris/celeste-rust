#!/bin/bash
# THE CHECK LADDER (plans/devloop.md): what to run, from cheapest.
#
#   ./check.sh t0           the unit tests (nextest, quick profile; the
#                           #[ignore]d ones aside): the pre-commit run
#   ./check.sh t1 [--pin]   single frames from the fixtures (tools/fixtures.sh),
#                           16.16 and float: the forward's exact output and
#                           metrics, the keys, the widening table, a backward
#                           frame, the storage, the kernels and transfers
#                           against the reference engine. THE REFACTOR GATE
#   ./check.sh t2 [--pin]   end to end: the room (1,0) gates, float and dual,
#                           the objects ladder, the UI export, a platform
#                           room; the reference's tree against the kernels',
#                           the hull replay; and the slow tests
#                           (#[ignore = "slow, ..."])
#   ./check.sh big [--pin]  the reference frame (6,2) 100% f56 -> f57 with its
#                           capture replayed, and the platform frame's metrics
#   ./check.sh perf [--pin] user-space instructions of three bench frames
#                           against gates/perf.txt, the RELEASE binary (built
#                           here): more than 2% over FAILS; milestones and
#                           `all`, not every slice
#   ./check.sh local        the local goldens (/var/tmp/float-goldens; loud
#                           SKIPPED where the store is absent)
#   ./check.sh fixtures     every fixture rebuilt from scratch against its
#                           pinned fingerprint (minutes)
#   ./check.sh all          the pre-merge run: all of the above, and the
#                           pre-merge tests (#[ignore = "pre-merge, ..."])
#
# Every step prints ok/FAIL and its wall time; the run exits non-zero if any
# step failed. t1 and t2 run their steps in LANES side by side (each lane's
# steps in order; the reports print when every lane is done). `--pin`
# writes the outputs as the new pins (gates/) instead of comparing: only on
# evidence, and say why in the commit. Timings are reported, never pinned.
# Builds the quick binary first (perf: the release one too). Env:
# CARGO_TARGET_DIR, CELESTE_FIXTURES,
# CHECK_DIR (the scratch, default /var/tmp/celeste-check), THREADS (default
# 16; the metrics are the same at every thread count; perf always runs 16).
set -uo pipefail
cd "$(dirname "$0")"
TIER=${1:-}
PIN=${2:-}
T=${CARGO_TARGET_DIR:-$PWD/target}
B=$T/quick/rewrite
export BIN=$B
# The instruction budget's binary (perf): release, fat LTO and one codegen
# unit. The quick profile's 16 units re-partition on unrelated edits and
# moved a frame's instructions by 2-3% (2026-10-16).
RB=$T/release/rewrite
F=${CELESTE_FIXTURES:-/var/tmp/celeste-fixtures}
G=gates/fixtures
C=${CHECK_DIR:-/var/tmp/celeste-check}
TH=${THREADS:-16}
# Every step's memory cap (safe-run.sh): the steps peak under 6 GB, and a
# step that outgrows its cap fails instead of taking the machine's memory.
# A lane runs one step at a time, so a tier's caps add up to 10G a lane;
# the test runs (nextest) get 20G.
MEM=10G
L1_62="--hundred --loading-jank 2 --level-minus-one 94,5"
mkdir -p "$C"
# The failed and skipped steps, by name (lanes are subshells: a file).
: > "$C/failed"
: > "$C/skipped"

# step NAME CMD...: run CMD (its output in $C/NAME.*), report ok/FAIL and time.
step() {
    local name=$1; shift
    local t0=$EPOCHREALTIME
    if "$@" > "$C/$name.out" 2> "$C/$name.err"; then
        printf '[%s] %-26s ok    %5.1f s\n' "$TIER" "$name" "$(awk "BEGIN{print $EPOCHREALTIME - $t0}")"
    else
        printf '[%s] %-26s FAIL  %5.1f s   (%s)\n' "$TIER" "$name" "$(awk "BEGIN{print $EPOCHREALTIME - $t0}")" "$C/$name.err"
        tail -15 "$C/$name.err" | sed 's/^/    /'
        echo "$name" >> "$C/failed"
    fi
}

# lane NAME FUNC: run FUNC's steps in the background, in order; `lanes`
# waits for every lane and prints their reports in the order they started.
LANES=()
lane() {
    local name=$1; shift
    ( "$@" ) > "$C/lane-$name.txt" 2>&1 &
    LANES+=("$!:$name")
}
lanes() {
    local l
    for l in "${LANES[@]}"; do
        wait "${l%%:*}"
        cat "$C/lane-${l#*:}.txt"
    done
    LANES=()
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

# ignored TIER: a nextest filter of the #[ignore]d tests whose reason starts
# with TIER ("slow", "pre-merge", "local"; `scripts_wrapped` checks that
# every #[ignore] names one).
ignored() {
    grep -rh -A1 --include='*.rs' "#\[ignore = \"$1, " src crates | grep -oP '^\s*fn \K\w+' | sed 's|.*|test(/::&$/)|' | paste -sd'|'
}

# ignoredtests TIER NEXTEST-ARGS...: run TIER's #[ignore]d tests.
ignoredtests() {
    local tier=$1 filter; shift
    filter=$(ignored "$tier")
    [[ -n $filter ]] || { echo "no #[ignore = \"$tier, ...\"] test found" >&2; return 1; }
    ./safe-run.sh --memory 20G -- ./one-cargo.sh cargo nextest run --cargo-profile quick --run-ignored only "$@" -E "$filter"
}

build() {
    step build ./safe-run.sh --memory 20G -- ./one-cargo.sh cargo build --profile quick --bins
}

t0() {
    step nextest ./safe-run.sh --memory 20G -- ./one-cargo.sh cargo nextest run --cargo-profile quick
    grep -E 'Summary|SLOW|FAIL' "$C/nextest.err" | tail -3
    echo "    (the skipped are #[ignore]d: t2 runs the slow ones, all the pre-merge ones, local the local goldens)"
}

# frame NAME FRAME ROOM LEVEL FLAGS...: one forward frame from a fixture, its
# f/e lines and exact metrics against the pins; timings shown.
frame() {
    local name=$1 f=$2 room=$3 level=$4; shift 4
    ./safe-run.sh --memory "$MEM" -- "$B" bench-frame --level-dir "$F/$name" --frame "$f" --room "$room" --level "$level" --edges --threads "$TH" --metrics "$C/$name.metrics" --l1-table "$F/$name/l1-table.bin" "$@" > "$C/$name.frame" || return 1
    same "$C/$name.frame" "$G/$name.frame" || return 1
    if [[ $PIN == --pin ]]; then
        same "$C/$name.metrics" "$G/$name.metrics"
    else
        tools/metrics_diff.py "$G/$name.metrics" "$C/$name.metrics" >&2
    fi
}

# framef NAME FRAME ROOM LEVEL FLAGS...: `frame` without the metrics (a
# float fixture's: the kernel mix census is 16.16's).
framef() {
    local name=$1 f=$2 room=$3 level=$4; shift 4
    ./safe-run.sh --memory "$MEM" -- "$B" bench-frame --level-dir "$F/$name" --frame "$f" --room "$room" --level "$level" --edges --threads "$TH" --l1-table "$F/$name/l1-table.bin" "$@" > "$C/$name.frame" || return 1
    same "$C/$name.frame" "$G/$name.frame"
}

# keycheck NAME FRAME ROOM LEVEL FLAGS...: frame's rows' keys recomputed
# (--key-check), and the same f/e lines.
keycheck() {
    local name=$1 f=$2 room=$3 level=$4; shift 4
    ./safe-run.sh --memory "$MEM" -- "$B" bench-frame --level-dir "$F/$name" --frame "$f" --room "$room" --level "$level" --edges --threads "$TH" --key-check --l1-table "$F/$name/l1-table.bin" "$@" > "$C/keycheck-$name.frame" || return 1
    diff "$G/$name.frame" "$C/keycheck-$name.frame" >&2
}

# widencheck NAME FRAME ROOM LEVEL FLAGS...: `rewrite widen-check` of a
# fixture's frame (the stored rows, and 64 rows' emissions).
widencheck() {
    local name=$1 f=$2 room=$3 level=$4; shift 4
    ./safe-run.sh --memory "$MEM" -- "$B" widen-check --level-dir "$F/$name" --frame "$f" --room "$room" --level "$level" --emitted 64 --l1-table "$F/$name/l1-table.bin" "$@"
}

# refcheck NAME FRAME ROOM FLAGS...: the kernels against the reference
# engine, 32 sampled rows of a fixture's frame (at r0sxh).
refcheck() {
    local name=$1 f=$2 room=$3; shift 3
    ./safe-run.sh --memory "$MEM" -- "$B" ref-check --level-dir "$F/$name" --frame "$f" --level r0sxh --room "$room" --samples 32 --threads "$TH" "$@"
}

# arccheck NAME ROOM FLAGS...: the recorded transfers of a fixture's f56
# against the reference engine, 32 sampled records (at r0sxh).
arccheck() {
    local name=$1 room=$2; shift 2
    ./safe-run.sh --memory "$MEM" -- "$B" arc-check --level-dir "$F/$name" --room "$room" --from 56 --to 56 --samples 32 --level r0sxh --threads "$TH" --path-cap 4096 "$@"
}

# faultcheck NAME ROOM FLAGS...: `arccheck` with a corrupted transfer
# (`--fault action`) must FAIL, on a disagreement: the check is live.
faultcheck() {
    if arccheck "$@" --fault action 2> "$C/fault-$1.err"; then
        echo "arc-check passed a corrupted transfer" >&2
        return 1
    fi
    grep -q 'arc-check: disagreement' "$C/fault-$1.err" || { tail -5 "$C/fault-$1.err" >&2; return 1; }
}

backward() {
    ./safe-run.sh --memory "$MEM" -- "$B" bench-backward --level-dir "$F/r10arc/level00" --horizon 56 --frame 45 --win-at 40,64 --threads "$TH" --reps 3 > "$C/backward.txt" || return 1
    same "$C/backward.txt" "$G/r10arc.backward"
}
# The same backward frame over the float twin's tree (`f10arc`).
backwardf() {
    ./safe-run.sh --memory "$MEM" -- "$B" bench-backward --arith float --level-dir "$F/f10arc/level00" --horizon 56 --frame 45 --win-at 40,64 --threads "$TH" --reps 3 > "$C/backward-f.txt" || return 1
    same "$C/backward-f.txt" "$G/f10arc.backward"
}

# storage TREE FRAME ROOM LEVEL FLAGS...: the capture of TREE's frame FRAME
# replayed; its new states and edges are the frame check's (TREE.frame).
storage() {
    local tree=$1 f=$2 room=$3 level=$4; shift 4
    ./safe-run.sh --memory "$MEM" -- "$B" bench-storage --capture "$F/cap-$tree/f$(printf %03d "$f")" --tree "$F/$tree" --room "$room" --level "$level" --threads "$TH" --reps 1 --l1-table "$F/$tree/l1-table.bin" "$@" > "$C/storage-$tree.txt" || return 1
    cat "$C/storage-$tree.txt" >&2
    { grep -o "f$(printf %03d "$f") [0-9]* [0-9a-f]*" "$C/storage-$tree.txt"; grep -o "e$(printf %03d "$f") [0-9]* [0-9a-f]*" "$C/storage-$tree.txt"; } | diff <(grep -E '^[fe][0-9]' "$G/$tree.frame") - >&2
}

t1() {
    build
    step fixtures tools/fixtures.sh ensure r10 r62h r42n r42x r10arc cap-r10 f10 f42n f42x f10arc
    grep -h '^\[fixtures\]' "$C/fixtures.err"
    local FL="--arith float"
    # One lane a fixture family (a fixture's steps in order: `bench-frame`
    # writes its scratch into the fixture).
    lane r10 t1_r10
    lane r62h t1_r62h
    lane r42 t1_r42
    lane f10 t1_f10 $FL
    lane f42 t1_f42 $FL
    lanes
    for n in r10 r62h r42n; do grep -hE '^\[bench\] rep 0' "$C/frame-$n.err" | sed "s/^/    $n /" | cut -c1-200; done
    grep -h '^\[bench-backward\] rep 0' "$C/backward-r10arc.err" | sed 's/^/    /'
    for s in r10 r62h r42n f10 f42n; do grep -h '^\[widen-check\] step' "$C/widen-$s.out" | sed 's/^/    /'; done
    for s in refcheck-r10 refcheck-r42x refcheck-f10 refcheck-f42x; do grep -h '^\[ref-check\] [0-9]' "$C/$s.out" | sed "s/^/    /"; done
    for s in arccheck-r10 arccheck-r42x arccheck-f10 arccheck-f42x; do grep -h '^agreement' "$C/$s.out" | sed "s/^/    /" | cut -c1-200; done
}
# The forward, one frame each, exact (and the run's timings); every emitted
# row's key recomputed from its fields; the storage alone (the f56 capture
# replayed); the kernels and the recorded transfers against the reference
# engine, sampled, and a corrupted transfer refused; the widening table's two
# sides (every stored row, and the rows the kernels emit from 64 of them,
# kept as they are by the block side); one backward frame (W f45 of h56).
t1_r10() {
    step frame-r10 frame r10 55 1,0 r0sxh
    step keycheck-r10 keycheck r10 55 1,0 r0sxh
    step storage-r10 storage r10 56 1,0 r0sxh
    step refcheck-r10 refcheck r10 55 1,0
    step arccheck-r10 arccheck r10 1,0
    step arcfault-r10 faultcheck r10 1,0
    step widen-r10 widencheck r10 55 1,0 r0sxh
    step backward-r10arc backward
}
t1_r62h() {
    step frame-r62h frame r62h 50 6,2 r0sxhf $L1_62
    step widen-r62h widencheck r62h 50 6,2 r0sxhf $L1_62
}
t1_r42() {
    step frame-r42n frame r42n 60 4,2 r0sxhn --level-minus-one 71,5
    step keycheck-r42n keycheck r42n 60 4,2 r0sxhn --level-minus-one 71,5
    step widen-r42n widencheck r42n 60 4,2 r0sxhn --level-minus-one 71,5
    step refcheck-r42x refcheck r42x 55 4,2 --level-minus-one 71,5
    step arccheck-r42x arccheck r42x 4,2 --level-minus-one 71,5
}
# The same in FLOAT (`--arith float`, the doubles' kernels, Celia's start):
# the frames exact (their f/h/e lines; no metrics), the keys, the kernels
# and the transfers against the float reference, the widening.
t1_f10() {
    step frame-f10 framef f10 55 1,0 r0sxh "$@"
    step refcheck-f10 refcheck f10 55 1,0 "$@"
    step arccheck-f10 arccheck f10 1,0 "$@"
    step arcfault-f10 faultcheck f10 1,0 "$@"
    step widen-f10 widencheck f10 55 1,0 r0sxh "$@"
    step backward-f10arc backwardf
}
t1_f42() {
    step frame-f42n framef f42n 60 4,2 r0sxhn "$@" --level-minus-one 71,5
    step keycheck-f42n keycheck f42n 60 4,2 r0sxhn "$@" --level-minus-one 71,5
    step widen-f42n widencheck f42n 60 4,2 r0sxhn "$@" --level-minus-one 71,5
    step refcheck-f42x refcheck f42x 55 4,2 "$@" --level-minus-one 71,5
    step arccheck-f42x arccheck f42x 4,2 "$@" --level-minus-one 71,5
}

t2() {
    build
    step build-tests ./safe-run.sh --memory 20G -- ./one-cargo.sh cargo nextest run --cargo-profile quick --no-run
    step fixtures-r60s tools/fixtures.sh ensure r60s
    rm -rf "$C/t2"
    mkdir -p "$C/t2"
    lane gates t2_gates
    lane properties t2_properties
    lane platforms t2_platforms
    lane slow step slow-tests ignoredtests slow --test-threads 8
    lanes
    grep -E 'Summary' "$C/slow-tests.err" | tail -1
}
t2_gates() {
    step gate10-forward gate10_forward
    step gate10-arc gate10_arc
    step gate10f-forward gate10f_forward
    step gate10f-arc gate10f_arc
    step gate10d-dual gate10d_dual
    step search42-objects search42
    step export-ui export_ui
}
t2_properties() {
    step reference10 reference10
    step reference10f reference10 --arith float
    step hull10 hull10
    step hull10f hull10 --arith float
}
t2_platforms() {
    step forward60-platforms forward60
    # The widening table's two sides at `p`, a boundary and a mid-frame step
    # (stored rows only: the emissions would build the kernels again).
    step widen-r60s widen60
}

# The three pinned room (1,0) oracles (CLAUDE.md "Gates").
gate10_forward() {
    ./safe-run.sh --memory "$MEM" -- "$B" forward --to 44 --room 1,0 --checkpoint-dir "$C/t2/g10" > "$C/t2/g10.out" || return 1
    grep '^posgraph' "$C/t2/g10.out" | diff gates/posgraph_room10_f044.txt - >&2 || return 1
    ./safe-run.sh --memory "$MEM" -- "$B" ckhash --to 44 --room 1,0 --hulls --checkpoint-dir "$C/t2/g10" | diff gates/ckhash_room10_f000-044.txt - >&2
}
gate10_arc() {
    ./safe-run.sh --memory "$MEM" -- "$B" search --to 35 --win-at 9,101 --checkpoint-dir "$C/t2/arc10" --prefer gates/known_room10_win9-101.txt --save-marks "$C/t2/arc10-marks" > "$C/t2/arc10.out" || return 1
    grep '^\[gate\]' "$C/t2/arc10.out" | diff gates/arc_room10_win9-101_h35.txt - >&2
}
# The same oracles in float (`--arith float`, the doubles' kernels;
# plans/float.md): the forward's posgraph and fingerprint, and the arc
# gate's search with its concrete witness.
gate10f_forward() {
    ./safe-run.sh --memory "$MEM" -- "$B" forward --arith float --to 44 --room 1,0 --checkpoint-dir "$C/t2/g10f" > "$C/t2/g10f.out" || return 1
    grep '^posgraph' "$C/t2/g10f.out" | diff gates/float_posgraph_room10_f044.txt - >&2 || return 1
    ./safe-run.sh --memory "$MEM" -- "$B" ckhash --arith float --to 44 --room 1,0 --hulls --checkpoint-dir "$C/t2/g10f" | diff gates/float_ckhash_room10_f000-044.txt - >&2
}
gate10f_arc() {
    ./safe-run.sh --memory "$MEM" -- "$B" search --arith float --to 35 --win-at 9,101 --checkpoint-dir "$C/t2/arc10f" --prefer gates/known_room10_win9-101.txt --save-marks "$C/t2/arc10f-marks" > "$C/t2/arc10f.out" || return 1
    grep '^\[gate\]' "$C/t2/arc10f.out" | diff gates/float_arc_room10_win9-101_h35.txt - >&2
}
# The dual search (`rewrite dual`, plans/float.md) at the same synthetic
# win: fixed (the IL load) and Celia float, the known route valid in both;
# its bound and its witness pinned (each half's [gate] lines are the two
# gates above).
gate10d_dual() {
    ./safe-run.sh --memory "$MEM" -- "$B" dual --to 35 --win-at 9,101 --checkpoint-dir "$C/t2/dual10" --prefer gates/known_room10_win9-101.txt --save-marks "$C/t2/dual10-marks" > "$C/t2/dual10.out" || return 1
    grep -E '^DUAL BOUND|^\[gate\] h35 dual' "$C/t2/dual10.out" | diff gates/dual_room10_win9-101_h35.txt - >&2
}
# The objects ladder (r0sxhn filtering r0sxh) at a synthetic win, its
# witness as the known route: every level's [gate] lines pinned.
search42() {
    ./safe-run.sh --memory "$MEM" -- "$B" search --room 4,2 --level r0sxhn,r0sxh --to 50 --win-at 32,56 --checkpoint-dir "$C/t2/s42" --prefer "$G/known_room42_win32-56.txt" --save-marks "$C/t2/s42-marks" > "$C/t2/s42.out" || return 1
    grep '^\[gate\]' "$C/t2/s42.out" > "$C/t2/s42.gate"
    same "$C/t2/s42.gate" "$G/search42.gate"
}
# The UI export (`rewrite export-ui`) of the searches above with their
# marks: every binary file, and run.json without its timings (the search's
# wall, the forward's per-frame walls and memory), pinned by digest. The
# float gate's run and the dual's too (each tree read in its own
# arithmetic; the dual's arc pass the float half's).
export_ui() {
    for s in arc10:1,0 s42:4,2 arc10f:1,0 dual10:1,0; do
        local d=${s%%:*} room=${s##*:}
        ./safe-run.sh --memory "$MEM" -- "$B" export-ui --room "$room" --checkpoint-dir "$C/t2/$d" --arc "$C/t2/$d-marks" --out "$C/t2/ui-$d" > /dev/null || return 1
        (cd "$C/t2" && sha256sum ui-$d/*.bin)
        python3 -c '
import json, sys
r = json.load(open(sys.argv[1]))
del r["wall_s"]
for h in r["horizons"]:
    for l in h["levels"]:
        for f in l["fwd"]:
            for k in ["emit_ms", "emit_idle", "own_ms", "own_idle", "ckpt_ms", "pos_ms", "total_ms", "rss_gb"]:
                del f[k]
print(json.dumps(r, sort_keys=True))' "$C/t2/ui-$d/run.json" | sha256sum | sed "s|-\$|ui-$d/run.json (no timings)|"
    done > "$C/t2/export-ui.sha256"
    same "$C/t2/export-ui.sha256" "$G/export-ui.sha256"
}

# THE REFERENCE FORWARD'S TREE IS THE KERNELS' [ARITH]: room (1,0) at the
# exact level r0sx to f27 (~2.9k states past the spawn), frames, edges and
# hulls alike (`ckhash --edges --hulls`) - the reference engine's
# successors and transfers (its arc capture) are the kernels'. Each tree
# goes to $C/t2/{kernels,reference}10[-float].
reference10() {
    local d=$C/t2/reference10${1:+-float}
    ./safe-run.sh --memory "$MEM" -- "$B" forward "$@" --to 36 --room 1,0 --checkpoint-dir "$d-kernels" > /dev/null || return 1
    ./safe-run.sh --memory "$MEM" -- "$B" forward "$@" --reference --to 27 --room 1,0 --checkpoint-dir "$d-reference" > /dev/null || return 1
    ./safe-run.sh --memory "$MEM" -- "$B" ckhash "$@" --to 27 --room 1,0 --edges --hulls --checkpoint-dir "$d-kernels" > "$d-kernels.ckhash" || return 1
    ./safe-run.sh --memory "$MEM" -- "$B" ckhash "$@" --to 27 --room 1,0 --edges --hulls --checkpoint-dir "$d-reference" > "$d-reference.ckhash" || return 1
    diff "$d-kernels.ckhash" "$d-reference.ckhash" >&2
}
# THE HULL FORWARD'S TREE REPLAYS FROM ITS RECORDED GRAPH [ARITH]:
# reference10's kernels' tree (to f36), its hulls pushed along its recorded
# edges and transfers from the start, reaches exactly its states at every
# frame (`hull-replay` checks each): every edge was recorded. (That a known
# route holds in the tree is the gates' `--prefer`, search::known.)
hull10() {
    ./safe-run.sh --memory "$MEM" -- "$B" hull-replay "$@" --checkpoint-dir "$C/t2/reference10${1:+-float}-kernels" --to 36
}

# A platform room (platforms unknown, split frame) to step 71: the tree's
# fingerprint is the r60s fixture's pin.
forward60() {
    ./safe-run.sh --memory "$MEM" -- "$B" forward --room 6,0 --level r0sxhfp --to 71 --split-frame --checkpoint-dir "$C/t2/p60" > /dev/null || return 1
    ./safe-run.sh --memory "$MEM" -- "$B" ckhash --room 6,0 --to 71 --edges --hulls --checkpoint-dir "$C/t2/p60" 2> /dev/null | diff "$G/r60s.ckhash" - >&2
}

widen60() {
    for f in 70 71; do
        ./safe-run.sh --memory "$MEM" -- "$B" widen-check --level-dir "$F/r60s" --frame $f --room 6,0 --level r0sxhfp --split-frame || return 1
    done
}

big() {
    build
    step fixtures-big tools/fixtures.sh ensure r62h57 cap-r62h57 r60s
    step frame-r62h57 frame r62h57 56 6,2 r0sxhf $L1_62
    grep -hE '^\[bench\] (kernels|rep)|^\[phases\]' "$C/frame-r62h57.err" | sed 's/^/    /' | cut -c1-220
    step storage-r62h57 storage r62h57 57 6,2 r0sxhf $L1_62
    sed 's/^/    /' "$C/storage-r62h57.txt" | cut -c1-220
    # The platform kernels' frame (step 70 -> 71) and exact metrics: its
    # lines are t2's forward60's too; their build is ~20-40 s of tracing.
    step frame-r60s frame r60s 70 6,0 r0sxhfp --split-frame
    grep -hE '^\[bench\] (kernels|rep)' "$C/frame-r60s.err" | sed 's/^/    /' | cut -c1-200
}

# THE INSTRUCTION BUDGET (gates/perf.txt): a bench frame's one rep in
# user-space instructions, counted by `perf stat -e instructions:u` over a
# 3-rep and a 1-rep run of the RELEASE binary's `bench-frame` (their
# difference over 2: the kernel build, the tree's load and the process
# start cancel; the rep's fingerprints are in it). Instructions do not move
# with the machine's load as walls do: run to run within ~0.2% at load
# 30-190. 16 threads always (the pin's). The quick binary is not measured:
# its codegen units re-partition on unrelated edits (2-3%).
PERF_PINS=gates/perf.txt
perftier() {
    build
    step build-release ./safe-run.sh --memory 20G -- ./one-cargo.sh cargo build --release --bins
    step fixtures-perf tools/fixtures.sh ensure r62h r62h57 f42n
    step perf-r62h perfcount r62h 50 6,2 r0sxhf $L1_62
    step perf-r62h57 perfcount r62h57 56 6,2 r0sxhf $L1_62
    step perf-f42n perfcount f42n 60 4,2 r0sxhn --arith float --level-minus-one 71,5
    cat "$C"/perf-*.line 2> /dev/null | sed 's/^/    /'
}
# perfcount NAME FRAME ROOM LEVEL FLAGS...: one rep of the fixture's frame
# (its f/h/e lines the pinned ones: the work measured is the pinned work)
# against its line in gates/perf.txt: more than 2% over FAILS; more than 2%
# under passes, and says to re-pin.
perfcount() {
    local name=$1 f=$2 room=$3 level=$4; shift 4
    local n i1 i3 rep pin
    rm -f "$C/perf-$name.line"
    for n in 1 3; do
        ./safe-run.sh --memory "$MEM" -- perf stat -x, -e instructions:u -o "$C/perf-$name-$n.stat" -- "$RB" bench-frame --level-dir "$F/$name" --frame "$f" --room "$room" --level "$level" --edges --threads 16 --l1-table "$F/$name/l1-table.bin" --reps "$n" "$@" > "$C/perf-$name-$n.frame" || return 1
        diff "$G/$name.frame" "$C/perf-$name-$n.frame" >&2 || return 1
    done
    i1=$(awk -F, '$3 ~ /^instructions/ {print $1}' "$C/perf-$name-1.stat")
    i3=$(awk -F, '$3 ~ /^instructions/ {print $1}' "$C/perf-$name-3.stat")
    [[ $i1 =~ ^[0-9]+$ && $i3 =~ ^[0-9]+$ ]] || { echo "perf stat counted no instructions ($C/perf-$name-1.stat): perf missing, or perf_event_paranoid > 2?" >&2; return 1; }
    rep=$(( (i3 - i1) / 2 ))
    if [[ $PIN == --pin ]]; then
        if grep -q "^$name " "$PERF_PINS"; then sed -i "s/^$name .*/$name $rep/" "$PERF_PINS"; else echo "$name $rep" >> "$PERF_PINS"; fi
        echo "re-pinned $name: $rep instructions a rep" | tee "$C/perf-$name.line" >&2
        return
    fi
    pin=$(awk -v n="$name" '$1 == n {print $2}' "$PERF_PINS")
    [[ -n $pin ]] || { echo "$name: no pin in $PERF_PINS (./check.sh perf --pin)" >&2; return 1; }
    awk -v r="$rep" -v p="$pin" -v n="$name" 'BEGIN {
        d = 100 * (r - p) / p
        note = d > 2 ? ": OVER BUDGET" : (d < -2 ? ": faster, re-pin (./check.sh perf --pin) to keep the gain" : "")
        printf "%s: %.3f G instructions a rep, pinned %.3f G (%+.2f%%)%s\n", n, r / 1e9, p / 1e9, d, note
        exit d > 2
    }' | tee "$C/perf-$name.line" >&2
}

# The local goldens (trace::oracle): our unsubmitted witnesses' goldens,
# from the machine-local store; where it is absent the test passes and says
# SKIPPED, repeated here and in the run's last line.
localgoldens() {
    step local-goldens ignoredtests local
    grep -h '\[local goldens\]' "$C/local-goldens.out" "$C/local-goldens.err" | sed 's/^/    /'
    if grep -qh 'SKIPPED' "$C/local-goldens.out" "$C/local-goldens.err"; then
        echo "[$TIER] !!! LOCAL GOLDENS SKIPPED: the store is absent; nothing of it was checked"
        echo "local-goldens" >> "$C/skipped"
    fi
}

# The fixtures rebuilt from scratch by this checkout, each against its
# pinned fingerprint (the existing fixtures stay).
fixtures() {
    step fixtures-verify tools/fixtures.sh verify all big
    grep -h '^\[fixtures\]' "$C/fixtures-verify.err" | sed 's/^/    /'
}

# The pre-merge tests (#[ignore = "pre-merge, ..."]), one at a time (the
# floors' pin peaks at 16 GB).
premerge() {
    step premerge-tests ignoredtests pre-merge --test-threads 1
    grep -E 'Summary' "$C/premerge-tests.err" | tail -1
}

echo "[$TIER] $(uptime | sed 's/.*load/load/')"
T_START=$SECONDS
case $TIER in
    t0) t0 ;;
    t1) t1 ;;
    t2) t2 ;;
    big) big ;;
    perf) perftier ;;
    local) localgoldens ;;
    fixtures) fixtures ;;
    all) for TIER in t0 t1 t2 big perf local fixtures premerge; do
             t=$SECONDS
             case $TIER in perf) perftier ;; local) localgoldens ;; *) $TIER ;; esac
             echo "[$TIER] $((SECONDS - t)) s"
         done; TIER=all ;;
    *) sed -n '2,30p' "$0"; exit 2 ;;
esac
FAILED=$(paste -sd' ' "$C/failed")
SKIPPED=$(paste -sd' ' "$C/skipped")
echo "[$TIER] $((SECONDS - T_START)) s; $([[ -n $FAILED ]] && echo "FAILED: $FAILED" || echo "all ok")${SKIPPED:+ (SKIPPED: $SKIPPED)}"
[[ -z $FAILED ]]
