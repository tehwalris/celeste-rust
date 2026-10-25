#!/bin/bash
# THE CHECK LADDER (plans/devloop.md: what each tier runs and pins).
#
#   ./check.sh TIER... [--pin], the tiers run in order:
#     t0         the unit tests (the #[ignore]d aside): the pre-commit run
#     t1         single frames from the fixtures, 16.16 and float: THE
#                REFACTOR GATE
#     t2         end to end, and the slow tests
#     big        the reference frame (6,2) 100% f56 -> f57 and the platform
#                frame
#     perf       the RELEASE binary's instruction budget (gates/perf.txt;
#                more than 2% over FAILS)
#     local      the local goldens and the PICO-8 probe goldens (SKIPPED,
#                which FAILS the run, where the store or PICO-8 is absent)
#     fixtures   every fixture rebuilt against its pinned fingerprint
#     premerge   the pre-merge tests
#     all        every tier above
#
# Every step prints ok/FAIL and its wall time; the run exits non-zero if any
# step failed. t1 and t2 run their steps in LANES side by side. `--pin`
# writes the outputs as the new pins (gates/) instead of comparing (t1, t2,
# big, perf): only on evidence, and say why in the commit. Builds the quick binary first (perf:
# the release one too). Env: CARGO_TARGET_DIR, CELESTE_FIXTURES, CHECK_DIR
# (the scratch, default per checkout: /var/tmp/celeste-check-NAME-HASH, so
# two worktrees' runs never share it), THREADS (default 16; the metrics
# are the same at every thread count). The fixtures are shared and
# READ-ONLY (tools/fixtures.sh): a step that writes into one fails.
set -uo pipefail
cd "$(dirname "$0")"
usage() {
    sed -n '2,/^[^#]/{/^#/p}' "$0"
    exit 2
}
TIERS=()
PIN=
for a in "$@"; do
    case $a in
        --pin) PIN=--pin ;;
        t0 | t1 | t2 | big | perf | local | fixtures | premerge) TIERS+=("$a") ;;
        all) TIERS+=(t0 t1 t2 big perf local fixtures premerge) ;;
        *) usage ;;
    esac
done
[[ ${#TIERS[@]} -gt 0 ]] || usage
TIER=${TIERS[0]}
T=${CARGO_TARGET_DIR:-$PWD/target}
B=$T/quick/rewrite
export BIN=$B
# The instruction budget's binary (perf): release, fat LTO and one codegen
# unit (the quick profile's 16 units re-partition on unrelated edits, which
# moves a frame's instructions by 2-3%).
RB=$T/release/rewrite
G=gates/fixtures
C=${CHECK_DIR:-/var/tmp/celeste-check-${PWD##*/}-$(printf %s "$PWD" | sha1sum | cut -c1-8)}
TH=${THREADS:-16}
# Every step's memory cap (safe-run.sh): the steps peak under 6 GB (but
# `refcheck`, which has its own), and a step that outgrows its cap fails
# instead of taking the machine's memory. A lane runs one step at a time:
# 10G a lane; the test runs get 20G.
MEM=10G
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
# with TIER ("slow", "pre-merge", "local"; `conventions` checks that
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
    # The fixtures' store: one directory per file-format set, as the binary
    # states the formats (`tools/fixtures.sh root`).
    F=${CELESTE_FIXTURES:-$(tools/fixtures.sh root)}
}

t0() {
    step nextest ./safe-run.sh --memory 20G -- ./one-cargo.sh cargo nextest run --cargo-profile quick
    grep -E 'Summary|SLOW|FAIL' "$C/nextest.err" | tail -3
    echo "    (the skipped are #[ignore]d: t2 runs the slow ones, all the pre-merge ones, local the local goldens)"
}

# The commands that read a fixture take its settings and level from its
# records (`search::tree::TreeDir`): only the frame and the run's own flags
# are given.

# frame NAME STEP [--metrics]: one forward step from a fixture, checkpointed
# as the forward does, against its pins (`bench frame --expect`: the f/h/e
# lines, the b line over the bytes of every file it wrote; with --metrics
# the exact metrics, 16.16's kernel census); timings shown.
frame() {
    local name=$1 f=$2 m=()
    [[ ${3:-} == --metrics ]] && m=(--metrics "$G/$name.metrics")
    ./safe-run.sh --memory "$MEM" -- "$B" bench frame --tree "$F/$name" --step "$f" --edges --checkpoint --threads "$TH" --expect "$G/$name.frame" "${m[@]}" $PIN > "$C/$name.frame"
}

# keycheck NAME STEP: the step's rows' keys recomputed at the boundary
# (`check keys`), and the pinned f/h/e lines.
keycheck() {
    local name=$1 f=$2
    ./safe-run.sh --memory "$MEM" -- "$B" check keys --tree "$F/$name" --step "$f" --threads "$TH" --expect "$G/$name.frame" > "$C/keycheck-$name.frame"
}

# widencheck NAME FRAME: `rewrite check widen` of a fixture's frame (the
# stored rows, and 64 rows' emissions).
widencheck() {
    ./safe-run.sh --memory "$MEM" -- "$B" check widen --tree "$F/$1" --step "$2" --emitted 64
}

# refcheck NAME FRAME: the kernels against the reference engine, 32
# sampled rows of a fixture's frame (an r0sx fixture's). A worker holds
# every fork path of its row, and the reference forks each row's buttons,
# held buttons and jump buffer: room (4,2)'s rows peak at 8.5 GB (16.16)
# and 12 GB (doubles) on 8 workers, 15 and 21 GB on 16.
refcheck() {
    ./safe-run.sh --memory 16G -- "$B" check ref --tree "$F/$1" --step "$2" --samples 32 --threads 8
}

# arccheck NAME FLAGS...: the recorded transfers of a fixture's f56
# against the reference engine, 32 sampled records (an r0sx fixture's).
arccheck() {
    local name=$1; shift
    ./safe-run.sh --memory "$MEM" -- "$B" check arcs --tree "$F/$name" --from-step 56 --to-step 56 --samples 32 --threads "$TH" --path-cap 4096 "$@"
}

# faultcheck NAME: `arccheck` with a corrupted transfer (`--fault action`)
# must FAIL, on a disagreement: the check is live.
faultcheck() {
    if arccheck "$1" --fault action 2> "$C/fault-$1.err"; then
        echo "check arcs passed a corrupted transfer" >&2
        return 1
    fi
    grep -q 'check arcs: disagreement' "$C/fault-$1.err" || { tail -5 "$C/fault-$1.err" >&2; return 1; }
}

# backward NAME: one backward frame of a search fixture (W f45 of h56).
backward() {
    ./safe-run.sh --memory "$MEM" -- "$B" bench backward --tree "$F/$1/level00" --steps 56 --step 45 --threads "$TH" --reps 3 > "$C/backward-$1.txt" || return 1
    same "$C/backward-$1.txt" "$G/$1.backward"
}

t1() {
    build
    step fixtures tools/fixtures.sh ensure r10 r62h r42n r42x r10arc f10 f42n f42x f10arc r60s
    grep -h '^\[fixtures\]' "$C/fixtures.err"
    # One lane a fixture family.
    lane r10 t1_r10
    lane r62h t1_r62h
    lane r42 t1_r42
    lane r60 t1_r60
    lane f10 t1_f10
    lane f42 t1_f42
    lanes
    for n in r10 r62h r42n; do grep -hE '^\[bench\] rep 0' "$C/frame-$n.err" | sed "s/^/    $n /" | cut -c1-200; done
    grep -h '^\[bench backward\] rep 0' "$C/backward-r10arc.err" | sed 's/^/    /'
    for s in r10 r62h r42n f10 f42n; do grep -h '^\[check widen\] step' "$C/widen-$s.out" | sed 's/^/    /'; done
    for s in refcheck-r10 refcheck-r42x refcheck-f10 refcheck-f42x; do grep -h '^\[check ref\] [0-9]' "$C/$s.out" | sed "s/^/    /"; done
    for s in arccheck-r10 arccheck-r42x arccheck-f10 arccheck-f42x; do grep -h '^agreement' "$C/$s.out" | sed "s/^/    /" | cut -c1-200; done
}
t1_r10() {
    step frame-r10 frame r10 55 --metrics
    step keycheck-r10 keycheck r10 55
    step refcheck-r10 refcheck r10 55
    step arccheck-r10 arccheck r10
    step arcfault-r10 faultcheck r10
    step widen-r10 widencheck r10 55
    step backward-r10arc backward r10arc
}
t1_r62h() {
    step frame-r62h frame r62h 50 --metrics
    step widen-r62h widencheck r62h 50
}
t1_r42() {
    step frame-r42n frame r42n 60 --metrics
    step keycheck-r42n keycheck r42n 60
    step widen-r42n widencheck r42n 60
    step refcheck-r42x refcheck r42x 55
    step arccheck-r42x arccheck r42x
}
# The platform room: its rows' keys recomputed at the boundary, every
# FOLLOWER checked (`exact::Deps`: room (6,0)'s ten platforms keyed by the
# first one's), against the kernels' emission; the pinned f/h/e lines.
t1_r60() {
    step keycheck-r60s keycheck r60s 70
}
# The same in FLOAT (`--arith float`, Celia's start), without the metrics.
t1_f10() {
    step frame-f10 frame f10 55
    step refcheck-f10 refcheck f10 55
    step arccheck-f10 arccheck f10
    step arcfault-f10 faultcheck f10
    step widen-f10 widencheck f10 55
    step backward-f10arc backward f10arc
}
t1_f42() {
    step frame-f42n frame f42n 60
    step keycheck-f42n keycheck f42n 60
    step widen-f42n widencheck f42n 60
    step refcheck-f42x refcheck f42x 55
    step arccheck-f42x arccheck f42x
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
    step hull10f hull10 float
}
t2_platforms() {
    step forward60-platforms forward60
    # The widening table's two sides, a boundary and a mid-frame step
    # (stored rows only: the emissions would build the kernels again).
    step widen-r60s widen60
}

# The pinned room (1,0) oracles (CLAUDE.md "Gates"): the forward's states,
# edges (with their transfers) and hulls f0-f44, and the arc gate.
gate10_forward() {
    ./safe-run.sh --memory "$MEM" -- "$B" forward --steps 44 --room 1,0 --tree "$C/t2/g10" > "$C/t2/g10.out" || return 1
    ./safe-run.sh --memory "$MEM" -- "$B" ckhash --tree "$C/t2/g10" --steps 44 --edges --hulls > "$C/t2/g10.ckhash" || return 1
    same "$C/t2/g10.ckhash" gates/ckhash_room10_f000-044.txt
}
# gate NAME: the finished search $C/t2/NAME's [gate] lines (`rewrite gate`:
# per level its arc phase rerun, then its witness) into $C/t2/NAME.gate.
gate() {
    ./safe-run.sh --memory "$MEM" -- "$B" gate --search-dir "$C/t2/$1" > "$C/t2/$1.gate"
}
gate10_arc() {
    ./safe-run.sh --memory "$MEM" -- "$B" search --frames 35 --win-at 9,101 --search-dir "$C/t2/arc10" --prefer gates/known_room10_win9-101.txt --save-marks "$C/t2/arc10-marks" > "$C/t2/arc10.out" || return 1
    gate arc10 || return 1
    same "$C/t2/arc10.gate" gates/arc_room10_win9-101_h35.txt
}
# The same oracles in float (`--arith float`, plans/float.md).
gate10f_forward() {
    ./safe-run.sh --memory "$MEM" -- "$B" forward --arith float --steps 44 --room 1,0 --tree "$C/t2/g10f" > "$C/t2/g10f.out" || return 1
    ./safe-run.sh --memory "$MEM" -- "$B" ckhash --tree "$C/t2/g10f" --steps 44 --edges --hulls > "$C/t2/g10f.ckhash" || return 1
    same "$C/t2/g10f.ckhash" gates/float_ckhash_room10_f000-044.txt
}
gate10f_arc() {
    ./safe-run.sh --memory "$MEM" -- "$B" search --arith float --frames 35 --win-at 9,101 --search-dir "$C/t2/arc10f" --prefer gates/known_room10_win9-101.txt --save-marks "$C/t2/arc10f-marks" > "$C/t2/arc10f.out" || return 1
    gate arc10f || return 1
    same "$C/t2/arc10f.gate" gates/float_arc_room10_win9-101_h35.txt
}
# The dual search (`rewrite dual`) at the same synthetic win, the known
# route valid in both: its bound (stdout) and witness (`rewrite gate`)
# pinned (each half's [gate] lines are the two gates above).
gate10d_dual() {
    ./safe-run.sh --memory "$MEM" -- "$B" dual --frames 35 --win-at 9,101 --search-dir "$C/t2/dual10" --prefer gates/known_room10_win9-101.txt --save-marks "$C/t2/dual10-marks" > "$C/t2/dual10.out" || return 1
    ./safe-run.sh --memory "$MEM" -- "$B" gate --search-dir "$C/t2/dual10" > "$C/t2/dual10.lines" || return 1
    { grep '^DUAL BOUND' "$C/t2/dual10.out"; grep '^\[gate\] h35 dual' "$C/t2/dual10.lines"; } > "$C/t2/dual10.gate"
    same "$C/t2/dual10.gate" gates/dual_room10_win9-101_h35.txt
}
# The objects ladder (r0sxn filtering r0sx) at a synthetic win, its
# witness as the known route: every level's [gate] lines pinned.
search42() {
    ./safe-run.sh --memory "$MEM" -- "$B" search --room 4,2 --level r0sxn,r0sx --frames 50 --win-at 32,56 --search-dir "$C/t2/s42" --prefer "$G/known_room42_win32-56.txt" --save-marks "$C/t2/s42-marks" > "$C/t2/s42.out" || return 1
    gate s42 || return 1
    same "$C/t2/s42.gate" "$G/search42.gate"
}
# The UI export (`rewrite export-ui`) of the searches above with their
# marks, pinned by digest: every binary file and run.json (its timings are
# timings.json's).
export_ui() {
    for d in arc10 s42 arc10f dual10; do
        ./safe-run.sh --memory "$MEM" -- "$B" export-ui --search-dir "$C/t2/$d" --arc "$C/t2/$d-marks" --out "$C/t2/ui-$d" > /dev/null || return 1
        (cd "$C/t2" && sha256sum ui-$d/*.bin ui-$d/run.json)
    done > "$C/t2/export-ui.sha256"
    same "$C/t2/export-ui.sha256" "$G/export-ui.sha256"
}

# THE REFERENCE FORWARD'S TREE IS THE KERNELS' [ARITH]: room (1,0) at the
# exact level r0sx to f27, frames, edges and hulls alike (`check reference`):
# the reference engine's successors and transfers are the kernels'. The
# kernels' tree (to f36: hull10 replays it) goes to
# $C/t2/reference10[-float]-kernels.
reference10() {
    local d=$C/t2/reference10${1:+-float}
    ./safe-run.sh --memory "$MEM" -- "$B" forward "$@" --steps 36 --room 1,0 --tree "$d-kernels" > /dev/null || return 1
    ./safe-run.sh --memory "$MEM" -- "$B" check reference --tree "$d-kernels" --steps 27
}
# THE HULL FORWARD'S TREE REPLAYS FROM ITS RECORDED GRAPH [ARITH]:
# reference10's kernels' tree (to f36), its hulls pushed along its recorded
# edges and transfers from the start, reaches exactly its states at every
# frame (`check replay`): every edge was recorded.
hull10() {
    ./safe-run.sh --memory "$MEM" -- "$B" check replay --tree "$C/t2/reference10${1:+-float}-kernels" --steps 36
}

# A platform room (ten exact platforms keyed by the first one's, split
# frame) to step 71: the tree's fingerprint is the r60s fixture's pin, which
# only `tools/fixtures.sh pin r60s` writes (so not `same`: t2 --pin leaves
# it).
forward60() {
    ./safe-run.sh --memory "$MEM" -- "$B" forward --room 6,0 --level r0sxf --steps 71 --split-frame --tree "$C/t2/p60" > /dev/null || return 1
    ./safe-run.sh --memory "$MEM" -- "$B" ckhash --tree "$C/t2/p60" --steps 71 --edges --hulls 2> /dev/null | diff "$G/r60s.ckhash" - >&2
}

widen60() {
    for f in 70 71; do
        ./safe-run.sh --memory "$MEM" -- "$B" check widen --tree "$F/r60s" --step $f || return 1
    done
}

big() {
    build
    step fixtures-big tools/fixtures.sh ensure r62h57 r60s
    step frame-r62h57 frame r62h57 56 --metrics
    grep -hE '^\[bench\] (kernels|rep)|^\[phases\]' "$C/frame-r62h57.err" | sed 's/^/    /' | cut -c1-220
    # The platform kernels' frame (step 70 -> 71) and exact metrics: its
    # lines are t2's forward60's too; their build is ~20-40 s of tracing.
    step frame-r60s frame r60s 70 --metrics
    grep -hE '^\[bench\] (kernels|rep)' "$C/frame-r60s.err" | sed 's/^/    /' | cut -c1-200
}

# THE INSTRUCTION BUDGET (gates/perf.txt): a bench frame's one rep in
# user-space instructions, counted by `perf stat -e instructions:u` over a
# 3-rep and a 1-rep run of the RELEASE binary's `bench frame` (their
# difference over 2: the kernel build, the tree's load and the process
# start cancel; the rep's fingerprints are in it). Instructions do not move
# with the machine's load as walls do (run to run within ~0.2%). 16 threads
# always (the pin's).
PERF_PINS=gates/perf.txt
perftier() {
    build
    step build-release ./safe-run.sh --memory 20G -- ./one-cargo.sh cargo build --release --bins
    step fixtures-perf tools/fixtures.sh ensure r62h r62h57 f42n
    step perf-r62h perfcount r62h 50
    step perf-r62h57 perfcount r62h57 56
    step perf-f42n perfcount f42n 60
    cat "$C"/perf-*.line 2> /dev/null | sed 's/^/    /'
}
# perfcount NAME FRAME: one rep of the fixture's frame (its f/h/e lines the
# pinned ones: the work measured is the pinned work) against its line in
# gates/perf.txt: more than 2% over FAILS; more than 2% under passes, and
# says to re-pin.
perfcount() {
    local name=$1 f=$2
    local n i1 i3 rep pin
    rm -f "$C/perf-$name.line"
    # The kernel cache warmed first (`compiled::cache`): both counted runs
    # load the kernels, so their build cancels; one that built them and one
    # that loaded them would not.
    ./safe-run.sh --memory "$MEM" -- "$RB" bench frame --tree "$F/$name" --step "$f" --threads 16 --reps 0 > /dev/null 2> "$C/perf-$name-warm.err" || return 1
    for n in 1 3; do
        ./safe-run.sh --memory "$MEM" -- perf stat -x, -e instructions:u -o "$C/perf-$name-$n.stat" -- "$RB" bench frame --tree "$F/$name" --step "$f" --edges --threads 16 --expect "$G/$name.frame" --reps "$n" > "$C/perf-$name-$n.frame" || return 1
    done
    i1=$(awk -F, '$3 ~ /^instructions/ {print $1}' "$C/perf-$name-1.stat")
    i3=$(awk -F, '$3 ~ /^instructions/ {print $1}' "$C/perf-$name-3.stat")
    [[ $i1 =~ ^[0-9]+$ && $i3 =~ ^[0-9]+$ ]] || { echo "perf stat counted no instructions ($C/perf-$name-1.stat): perf missing, or perf_event_paranoid > 2?" >&2; return 1; }
    rep=$(( (i3 - i1) / 2 ))
    (( rep > 0 )) || { echo "$name: $rep instructions a rep: implausible: a run built kernels the other did not?" | tee "$C/perf-$name.line" >&2; return 1; }
    if [[ $PIN == --pin ]]; then
        if grep -q "^$name " "$PERF_PINS"; then sed -i "s/^$name .*/$name $rep/" "$PERF_PINS"; else echo "$name $rep" >> "$PERF_PINS"; fi
        echo "re-pinned $name: $rep instructions a rep" | tee "$C/perf-$name.line" >&2
        return
    fi
    pin=$(awk -v n="$name" '$1 == n {print $2}' "$PERF_PINS")
    [[ -n $pin ]] || { echo "$name: no pin in $PERF_PINS (./check.sh perf --pin)" >&2; return 1; }
    # More than 20% either way is no change of the code's: FAIL, no re-pin.
    awk -v r="$rep" -v p="$pin" -v n="$name" 'BEGIN {
        d = 100 * (r - p) / p
        implausible = d > 20 || d < -20
        note = implausible ? ": IMPLAUSIBLE: a run built kernels the other did not?" : (d > 2 ? ": OVER BUDGET" : (d < -2 ? ": faster, re-pin (./check.sh perf --pin) to keep the gain" : ""))
        printf "%s: %.3f G instructions a rep, pinned %.3f G (%+.2f%%)%s\n", n, r / 1e9, p / 1e9, d, note
        exit d > 2 || implausible
    }' | tee "$C/perf-$name.line" >&2
}

# The local tier: the local goldens (trace::oracle), from the machine-local
# store, and the PICO-8 probe goldens (trace::probe), from a real PICO-8;
# where either is absent its test passes and says SKIPPED: the run's last
# line repeats it, and the run FAILS (a check that compared nothing is no
# pass).
localgoldens() {
    step local-goldens ignoredtests local
    grep -h '\[local goldens\]' "$C/local-goldens.out" "$C/local-goldens.err" | sed 's/^/    /'
    if grep -qh 'SKIPPED' "$C/local-goldens.out" "$C/local-goldens.err"; then
        grep -h 'SKIPPED' "$C/local-goldens.out" "$C/local-goldens.err" | sed "s/^/[$TIER] !!! /"
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
# level -1 table builds).
premerge() {
    step premerge-tests ignoredtests pre-merge --test-threads 1
    grep -E 'Summary' "$C/premerge-tests.err" | tail -1
}

T_START=$SECONDS
for TIER in "${TIERS[@]}"; do
    echo "[$TIER] $(uptime | sed 's/.*load/load/')"
    t=$SECONDS
    case $TIER in perf) perftier ;; local) localgoldens ;; *) $TIER ;; esac
    echo "[$TIER] $((SECONDS - t)) s"
done
FAILED=$(paste -sd' ' "$C/failed")
SKIPPED=$(paste -sd' ' "$C/skipped")
echo "[$(IFS=+; echo "${TIERS[*]}")] $((SECONDS - T_START)) s; $([[ -n $FAILED ]] && echo "FAILED: $FAILED" || { [[ -n $SKIPPED ]] && echo "FAILED, nothing compared" || echo "all ok"; })${SKIPPED:+ (SKIPPED: $SKIPPED)}"
[[ -z $FAILED && -z $SKIPPED ]]
