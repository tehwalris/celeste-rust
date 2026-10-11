#!/bin/bash
# The MAP MOD room's single-frame benchmark (plans/map-mod.md, "Benchmark and
# projection"): one forward frame F -> F+1 of a mod-room tree, `bench-frame
# --edges`, repeatably, with its f/e lines (the frame's exact output) and its
# wall, emissions and memory - the per-state costs `tools/mod_project.py`
# projects whole runs with. Iterate on these, not on full runs.
#
#   tools/mod_bench.sh make NAME...    cut the fixture from its source tree
#   tools/mod_bench.sh run [NAME...]   run the frame(s) (default: every made fixture)
#   tools/mod_bench.sh list
#
# A fixture is a HARD-LINKED cut of a finished tree at frame F (frames,
# edges and drop notes past F removed in the cut only), under
# $MOD_FIXTURES (default /var/tmp/mod-fixtures), with the level -1 table
# pinned beside it. Never committed (the mod is private, its cart outside
# the repo: $MOD_CART, default /var/tmp/mod/taslevel.p8). The first run's
# f/e lines are kept as NAME/frame.pin; a later run that differs says so.
# Env: BIN (default $CARGO_TARGET_DIR/release/rewrite), THREADS (16), REPS (1).
set -euo pipefail
cd "$(dirname "$0")/.."
ROOT=${MOD_FIXTURES:-/var/tmp/mod-fixtures}
CART=${MOD_CART:-/var/tmp/mod/taslevel.p8}
B=${BIN:-${CARGO_TARGET_DIR:-./target}/release/rewrite}
TH=${THREADS:-16}
MID="--room 0,0 --win-at 56..72,104..112 --cart $CART"

# NAME -> SRC (a level dir) | F | LEVEL | FLAGS (the tree's settings)
spec() {
    case $1 in
        # r0sxhe (objects exact, held unknown) to the mid win, level -1 142:
        # early-busy and its peak frame (5.4M and 8.0M new states).
        he100) SRC=/var/tmp/mod-ck/dir-r0sxhe; F=100; LEVEL=r0sxhe; FLAGS="$MID --level-minus-one 142,5" ;;
        he120) SRC=/var/tmp/mod-ck/dir-r0sxhe; F=120; LEVEL=r0sxhe; FLAGS="$MID --level-minus-one 142,5" ;;
        # r0sxhne to the mid win, level -1 155: a peak-sized frame (f150 ->
        # f151: ~75M rows in) of the tree the arc-memory work uses.
        hne150) SRC=/var/tmp/mod-ck/mid155/level00; F=150; LEVEL=r0sxhne; FLAGS="$MID --level-minus-one 155,5" ;;
        *) echo "mod_bench: no fixture $1 (list)" >&2; exit 2 ;;
    esac
}
ALL="he100 he120 hne150"

make_one() {
    spec "$1"
    local dir=$ROOT/$1
    [[ -d $SRC/frames ]] || { echo "mod_bench: $1: no source tree $SRC" >&2; exit 1; }
    rm -rf "$dir" "$dir.tmp"
    mkdir -p "$ROOT"
    cp -al "$SRC" "$dir.tmp"
    rm -rf "$dir.tmp/arc-scratch"
    # Frames, edges and drop notes past F+1 go (F+1's would be overwritten
    # by the bench's scratch anyway; keep F's and earlier).
    find "$dir.tmp/frames" "$dir.tmp/edges" "$dir.tmp/dropped" -mindepth 1 -maxdepth 1 2>/dev/null \
        | while read -r p; do n=$(basename "$p" | grep -o '^f[0-9]*' | tr -d f || true); [[ -n $n ]] && ((10#$n > F)) && rm -rf "$p"; true; done
    echo "src $SRC" > "$dir.tmp/fixture.txt"
    echo "frame $F level $LEVEL flags $FLAGS" >> "$dir.tmp/fixture.txt"
    mv "$dir.tmp" "$dir"
    echo "[mod_bench] $1: cut at f$F ($(du -sh "$dir" | cut -f1) on disk, shared with the source while it exists)" >&2
}

run_one() {
    spec "$1"
    local dir=$ROOT/$1 out err
    [[ -d $dir ]] || { echo "[mod_bench] $1: not made (tools/mod_bench.sh make $1)" >&2; return 1; }
    out=$(mktemp) err=$(mktemp)
    ./safe-run.sh -- "$B" bench-frame --level-dir "$dir" --frame "$F" --edges --level "$LEVEL" $FLAGS \
        --l1-table "$dir/l1-table.bin" --threads "$TH" --reps "${REPS:-1}" > "$out" 2> "$err" \
        || { tail -5 "$err" >&2; echo "[mod_bench] $1: FAILED" >&2; return 1; }
    grep -E '^[fe][0-9]' "$out" > "$out.l" || true
    mv "$out.l" "$out"
    if [[ -f $dir/frame.pin ]]; then
        diff -q "$dir/frame.pin" "$out" > /dev/null && pin=same || pin=DIFFERS
    else
        cp "$out" "$dir/frame.pin"; pin=pinned
    fi
    local rep fwd
    rep=$(grep -E '^\[bench\] rep 0' "$err" | head -1 | cut -c1-200)
    fwd=$(grep -E '^\[bench\] kernels' "$err" | head -1 | cut -c1-120)
    echo "$1 f$F->f$((F + 1)) $LEVEL: $(tr '\n' ' ' < "$out" | cut -c1-120) [lines $pin]"
    echo "    $rep"
    echo "    $fwd"
    rm -f "$out" "$err"
}

cmd=${1:-run}
shift || true
names=${*:-$ALL}
case $cmd in
    make) for n in $names; do make_one "$n"; done ;;
    run) for n in $names; do [[ -d $ROOT/$n ]] && run_one "$n"; done ;;
    list) for n in $ALL; do spec "$n"; echo "$n: $SRC f$F $LEVEL $FLAGS $([[ -d $ROOT/$n ]] && echo '(made)')"; done ;;
    *) echo "usage: tools/mod_bench.sh make|run|list [NAME...]" >&2; exit 2 ;;
esac
