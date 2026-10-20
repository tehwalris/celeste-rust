#!/bin/bash
# THE FORWARD FRAME BENCHMARK (plans/devloop.md): one forward frame F ->
# F+1 of a fixture tree as the forward runs it (`rewrite bench frame
# --edges --checkpoint --reps N`), its lines checked against the fixture's
# PIN and its `[cost]` report printed.
#
#   tools/fwd_bench.sh make NAME F -- FLAGS...  build the fixture: `rewrite
#                                    forward --steps F FLAGS` into $FWD_BENCH/NAME
#                                    (FLAGS: the tree's settings and --level)
#   tools/fwd_bench.sh run NAME [REPS]  F -> F+1, REPS times (default 3); the
#                                    first run pins its lines (NAME/frame.pin),
#                                    a later one that differs FAILS
#   tools/fwd_bench.sh list
#
# A fixture is never committed (it may be of a private cart); each run's log
# goes to NAME/runs/. Wall times need a quiet machine (devloop.md).
# Env: FWD_BENCH (default /var/tmp/fwd-bench), BIN (default
# $CARGO_TARGET_DIR/release/rewrite: benchmarks are release builds),
# THREADS (16), MEMORY (safe-run's --memory, default 15G).
set -euo pipefail
cd "$(dirname "$0")/.."
ROOT=${FWD_BENCH:-/var/tmp/fwd-bench}
B=${BIN:-${CARGO_TARGET_DIR:-./target}/release/rewrite}
TH=${THREADS:-16}
MEM=${MEMORY:-15G}

usage() {
    echo "usage: tools/fwd_bench.sh make NAME F -- FLAGS... | run NAME [REPS] | list" >&2
    exit 2
}

make_one() {
    local name=$1 f=$2
    shift 2
    [[ ${1:-} == -- ]] || usage
    shift
    local dir=$ROOT/$name
    [[ -e $dir ]] && { echo "[fwd_bench] $name: $dir exists (delete it to rebuild)" >&2; exit 1; }
    mkdir -p "$dir.tmp"
    echo "[fwd_bench] building $name: forward --steps $f $*" >&2
    local t0=$SECONDS
    ./safe-run.sh --memory "$MEM" -- "$B" forward --steps "$f" --threads "$TH" --tree "$dir.tmp" "$@" > "$dir.tmp/build.log" 2>&1 \
        || { tail -5 "$dir.tmp/build.log" >&2; echo "[fwd_bench] $name: FAILED (left in $dir.tmp)" >&2; exit 1; }
    {
        echo "frame $f"
        echo "flags $*"
        echo "built $(date -Is) by $B ($(git rev-parse --short HEAD 2>/dev/null || echo '?')) in $((SECONDS - t0)) s"
    } > "$dir.tmp/fixture.txt"
    mv "$dir.tmp" "$dir"
    echo "[fwd_bench] $name: built in $((SECONDS - t0)) s ($(du -sh "$dir" | cut -f1))" >&2
}

run_one() {
    local name=$1 reps=${2:-3}
    local dir=$ROOT/$name
    [[ -f $dir/fixture.txt ]] || { echo "[fwd_bench] $name: no fixture (tools/fwd_bench.sh make $name F -- FLAGS)" >&2; exit 1; }
    local f l1
    f=$(awk '$1 == "frame" {print $2}' "$dir/fixture.txt")
    # The tree's settings are its record's; only its pinned level -1 table
    # is passed on.
    l1=$(sed -n 's/^flags //p' "$dir/fixture.txt" | grep -o -- '--l1-table [^ ]*' || true)
    mkdir -p "$dir/runs"
    local log
    log=$dir/runs/$(date +%Y%m%d-%H%M%S).log
    echo "[fwd_bench] $name: f$f -> f$((f + 1)), $reps reps, $TH threads, $B; load $(cut -d' ' -f1-3 /proc/loadavg); log $log" >&2
    # shellcheck disable=SC2086
    ./safe-run.sh --memory "$MEM" -- "$B" bench frame --tree "$dir" --step "$f" --edges --checkpoint --reps "$reps" --threads "$TH" $l1 \
        > "$log.out" 2> "$log" || { tail -5 "$log" >&2; echo "[fwd_bench] $name: FAILED" >&2; exit 1; }
    grep -E '^[fheb][0-9]{3} ' "$log.out" > "$log.lines" || true
    local pin
    if [[ -f $dir/frame.pin ]]; then
        if diff -q "$dir/frame.pin" "$log.lines" > /dev/null; then pin=same; else pin=DIFFERS; fi
    else
        cp "$log.lines" "$dir/frame.pin"
        pin=pinned
    fi
    grep -E '^\[cost\]' "$log" >&2
    echo "[fwd_bench] $name: lines $pin ($(tr '\n' ' ' < "$log.lines"))" >&2
    if [[ $pin == DIFFERS ]]; then
        diff "$dir/frame.pin" "$log.lines" >&2 || true
        exit 1
    fi
}

cmd=${1:-}
shift || true
case $cmd in
    make) [[ $# -ge 3 ]] || usage; make_one "$@" ;;
    run) [[ $# -ge 1 ]] || usage; run_one "$@" ;;
    list) for d in "$ROOT"/*/fixture.txt; do [[ -f $d ]] && echo "$(basename "$(dirname "$d")"): $(tr '\n' ' ' < "$d")"; done ;;
    *) usage ;;
esac
