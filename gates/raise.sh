#!/bin/bash
# A RAISED level-0 tree against fresh ones (plans/architecture.md, "Raising
# the horizon"): one tree built under level -1 at the first horizon, then
# raised to each next; at each, a fresh tree under that horizon. Per horizon
# the per-frame (key, cell) sets and recorded edges (`ckhash --edges`) and
# the pos graph must be identical; with --search, the search's [gate] lines
# and its answer too.
#
#   gates/raise.sh [--search [--extra "SEARCH ARGS"]] [--level SPEC] [--bin B] ROOM DIR H1 H2 ...
#   gates/raise.sh 1,0 /var/tmp/raise-gate 55 56 58      # forwards only
#   gates/raise.sh --search --level r0sxhn,r0sxh 7,0 /var/tmp/raise-gate70 82 83 84
#
# Env: CELESTE_THREADS etc. pass through; S (the level -1 speed bound) is 5.
set -u
cd "$(dirname "$0")/.."
SEARCH=0; LEVEL=""; EXTRA=""; B=./target/quick/rewrite
while [[ $# -gt 0 ]]; do
    case $1 in
        --search) SEARCH=1; shift ;;
        --level) LEVEL=$2; shift 2 ;;
        --bin) B=$2; shift 2 ;;
        --extra) EXTRA=$2; shift 2 ;;
        *) break ;;
    esac
done
ROOM=$1; DIR=$2; shift 2
rm -rf "$DIR"; mkdir -p "$DIR"
RAISED=$DIR/raised; FAIL=0
for H in "$@"; do
    for KIND in raised fresh; do
        T=$DIR/$KIND; [[ $KIND == fresh ]] && { T=$DIR/fresh$H; rm -rf "$T"; }
        LOG=$DIR/$KIND-$H.log
        if [[ $SEARCH == 1 ]]; then
            CELESTE_LEVEL_MINUS_ONE="$H,5" /usr/bin/time -f "%e s %M KB" ./safe-run.sh --memory 25G -- $B search --room "$ROOM" --to "$H" ${LEVEL:+--level $LEVEL} $EXTRA --checkpoint-dir "$T" > "$LOG.out" 2> "$LOG" || { echo "$KIND h$H FAILED (see $LOG)"; FAIL=1; continue; }
            L0=$T/level00
        else
            CELESTE_LEVEL_MINUS_ONE="$H,5" /usr/bin/time -f "%e s %M KB" ./safe-run.sh --memory 25G -- $B forward --room "$ROOM" --to "$H" ${LEVEL:+--level ${LEVEL%%,*}} --checkpoint-dir "$T" > "$LOG.out" 2> "$LOG" || { echo "$KIND h$H FAILED (see $LOG)"; FAIL=1; continue; }
            L0=$T
        fi
        $B ckhash --room "$ROOM" --to "$H" --edges --checkpoint-dir "$L0" > "$DIR/$KIND-$H.ck" 2>/dev/null || { echo "$KIND h$H ckhash FAILED"; FAIL=1; }
        echo "$KIND h$H: $(tail -1 "$LOG") $(grep -c '^\[raise\] f' "$LOG") frames raised"
    done
    diff -q "$DIR/raised-$H.ck" "$DIR/fresh-$H.ck" > /dev/null && echo "h$H ckhash+edges OK" || { echo "h$H ckhash+edges DIFF"; FAIL=1; }
    if [[ $SEARCH == 1 ]]; then
        diff <(cat "$DIR/raised-$H.log.out" | sed -E 's/\([^)]*[0-9.]+ s\)//g; s/witness .*//') \
             <(cat "$DIR/fresh-$H.log.out" | sed -E 's/\([^)]*[0-9.]+ s\)//g; s/witness .*//') > "$DIR/search-$H.diff" \
            && echo "h$H search OK: $(grep -v '^ARC' "$DIR/fresh-$H.log.out" | tail -1)" || { echo "h$H search DIFF ($DIR/search-$H.diff)"; FAIL=1; }
    else
        diff <(grep '^posgraph' "$DIR/raised-$H.log.out") <(grep '^posgraph' "$DIR/fresh-$H.log.out") > /dev/null && echo "h$H posgraph OK" || { echo "h$H posgraph DIFF"; FAIL=1; }
    fi
    rm -rf "$DIR/fresh$H"
done
exit $FAIL
