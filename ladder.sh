#!/bin/bash
# Full precision-ladder driver (plans/refinement-plan.md). Per horizon H:
# level 0 extends and sweeps, then levels k=1..16 run banded by level k-1's
# (e,g); the first refuting level bumps the horizon; a k=16 (exact) win is
# the concrete optimum. Level>=1 runs are tube-confined and cheap; they are
# rebuilt per (k,H).
set -euo pipefail
cd "$(dirname "$0")"
# ROOM=x,y selects the start room (default 1,0). Must match
# game_runner::room_dir_stem: "room1" for the default, "room<x><y>" else.
# RECIPE selects the rewrite recipe (default rewrites.jsonl; room (0,0)
# uses rewrites-room00.jsonl - the shape-agnostic subset).
ROOM=${ROOM:-1,0}
export CELESTE_START_ROOM="$ROOM"
if [ "$ROOM" = "1,0" ]; then STEM=room1; else STEM=room$(echo "$ROOM" | tr -d ' ,'); fi
RECIPE=${RECIPE:-rewrites.jsonl}
# VARIANTS is a whitespace-separated list of shape-dispatched variant specs,
# each 'shape|shape=RECIPE_PATH' (see --variant in bin/rewrite.rs). A state
# whose object-array shape matches runs its frames under that recipe instead
# of $RECIPE.
#
# Safe to turn on or off mid-campaign, and that is by design rather than by
# luck: a variant frame round-trips base -> canonical -> variant -> canonical
# -> base, so the boundary states it produces are IDENTICAL to the base
# program's. Checkpoints are therefore interchangeable, which is why the
# campaign fingerprint deliberately does not hash the variant list - you can
# resume a variant run from checkpoints written without one.
#
# Applied to every stage that supports it, uniformly. Not because
# correctness needs it - see above - but so that a variant which turns out
# to be WRONG shows up as a disagreement rather than as a campaign whose
# stages quietly disagree with each other.
#
# NOTE: only `bench` accepts --variant today. `pos-graph` and `sweep` replay
# the forward pass too and would benefit - on room (0,0) pos-graph was the
# largest stage at 3567s - but do not take the flag yet.
VARIANTS=${VARIANTS:-}
read -ra VARIANT_SPECS <<< "$VARIANTS"
VARIANT_ARGS=()
for spec in ${VARIANT_SPECS[@]+"${VARIANT_SPECS[@]}"}; do
  VARIANT_ARGS+=(--variant "$spec")
done
if [ ${#VARIANT_ARGS[@]} -gt 0 ]; then
  echo "shape variants: ${VARIANTS}"
fi
# L0/KROOT are overridable so a smoke run can be pointed at a scratch dir
# without touching a certified campaign's checkpoints.
L0=${L0:-~/celeste-checkpoints/$STEM}
L0=$(eval echo "$L0")
KROOT=${KROOT:-~/celeste-checkpoints/$STEM}
KROOT=$(eval echo "$KROOT")
export CELESTE_FRONTIER_ONLY=1 CELESTE_DEOPT_COLLECT_FIRST=1
# CHUNKING IS SEMANTIC ON ROOMS WITH FRUIT, so every stage of a campaign
# must use the SAME values. An UnknownBool branch sends the whole state
# down both edges, so how lanes are grouped decides how coarse the
# over-approximation is - and the sweep replays the forward pass frame by
# frame, so a different grouping produces a different successor set.
#
# Getting it wrong fails in one of two ways, and only one of them is
# visible: a sweep chunked FINER than the forward pass silently produces
# a SUBSET of its edges (g then overestimates and the bands prune viable
# rows); chunked COARSER it produces a superset and dies with "a successor
# row is not in the row table". These used to differ - forward at
# cap/10 and sweep at cap/100 - which is the quiet direction.
export CELESTE_MAX_STATE_LANES=8000 CELESTE_FRUIT_CHUNK_LANES=8000
FROM=${1:-94}
TO=${2:-104}
MAXK=${3:-16}
# Per-stage wall clock and peak RSS. The bench prints its own peak, the sweep
# does not, and neither knows how long the OTHER stages took - so a campaign
# could only ever be costed by log archaeology. One line per stage here, and
# `/usr/bin/time` wraps the binary INSIDE the cgroup scope so the RSS is the
# search process's, not systemd-run's.
STAGES=${STAGES:-/tmp/ladder-stages.tsv}
[ -f "$STAGES" ] || printf 'stage\twall_s\tpeak_gb\trc\n' > "$STAGES"
# Cgroup cap for every stage. 100G is the house default and is what room
# (1,0) needs. Room (0,0)'s position-graph replay of its LAST frame peaks at
# 101.08 GB - measured, not estimated - so that campaign has to be run with
# MEM=108G. Do not raise it past what `free` leaves after /tmp (a tmpfs):
# above that the kernel kills the machine instead of the cgroup killing the
# job, which is the whole point of the cap.
#
# Since 2026-08-16 the forward pass's visited set is the mmap engine
# (fp-runs in RAM + frames/*.rowkeys on disk; see
# plans/visited-redesign.md): the bench stages pin ~3x less for the
# visited set and its page-cache share is reclaimable, so memory
# pressure degrades to I/O instead of an OOM kill. The sweep and the
# banded levels still rebuild the full key->id map in-process (parity
# with the old visited.bin path, fed from rowkeys), so THEIR peaks are
# unchanged - the caps above still stand.
MEM=${MEM:-100G}
stage() { # $1 name, $2 logfile, rest: the command
  local name=$1 log=$2 rc=0 t=/tmp/ladder-time.$$
  shift 2
  local t0=$SECONDS
  set +e
  ./safe-run.sh --memory "$MEM" -- /usr/bin/time -v -o "$t" "$@" > "$log" 2>&1
  rc=$?
  set -e
  local wall=$((SECONDS - t0))
  local kb
  kb=$(awk '/Maximum resident set size/ {print $NF+0; exit}' "$t" 2>/dev/null)
  awk -v n="$name" -v w="$wall" -v k="${kb:-0}" -v r="$rc" \
      'BEGIN{printf "%s\t%s\t%.2f\t%s\n", n, w, k/1048576, r}' | tee -a "$STAGES"
  # A failed stage must still abort the campaign (the caller runs under
  # `set -e`), but only after its cost has been recorded.
  return $rc
}
# FUSE=1 records the position graph DURING the level-0 forward pass instead
# of replaying the whole room for it afterwards (`--record-pos-graph`). The
# l0-posgraph stage below then finds a table that already covers the horizon
# and reuses it.
#
# ON by default for rooms where posgraphcheck.sh has been run, because the
# replay is not a cheap safety margin - it is HALF of level 0. Measured on
# room (1,0) at H=72 with a synthetic win: forward 248 s + pos-graph 242 s
# unfused, against 265 s fused. The recording costs 7% of the forward pass
# and removes a 242 s stage.
#
# Gated at h40 on room (2,0) (2026-08-17) and room (1,0) (2026-08-18): the
# visited row SETS are identical with and without it (900,028 rows, onlyA=0
# onlyB=0 on (1,0)), the sweep's `g` is identical as a function of the row,
# the recorded table is a strict SUPERSET (one extra pair - the spawn's
# first move, which a replay starting from the frame-1 batch cannot see),
# and the table survives --resume. A superset is sound by construction: the
# table only shrinks the sweep's candidate set, and the EXPANSION is what
# establishes an edge.
#
# Room (0,0) is NOT gated yet, so it keeps the replay until someone runs
# `ROOM=0,0 ./posgraphcheck.sh`. FUSE=0 forces the replay anywhere.
case "${FUSE:-}" in
  1) FUSE_ARG="--record-pos-graph" ;;
  0) FUSE_ARG="" ;;
  "") case "$ROOM" in
        1,0|2,0) FUSE_ARG="--record-pos-graph" ;;
        *) FUSE_ARG=""
           echo "pos-graph: replay path (room $ROOM is not gated for fused recording;" \
                "run ROOM=$ROOM ./posgraphcheck.sh, then FUSE=1)" ;;
      esac ;;
  *) echo "FUSE must be 0 or 1" >&2; exit 1 ;;
esac
[ -n "$FUSE_ARG" ] && echo "pos-graph: recorded IN the forward pass (fused)"
# SHARE_POSGRAPH=1 (the default) gives every banded level LEVEL 0's position
# graph instead of rebuilding one per (k, H) - 16 stages of 19-22 s per
# horizon, measured on room (1,0) at H=72.
#
# Sound because the table is a projection of the reachable transition
# relation onto whole-pixel position cells, and level 0 over-approximates
# every level above it: that is the ladder's own soundness argument, the
# widened coordinates (rem, spd) are SUB-pixel, and a banded level is
# restricted further still. So level 0's table CONTAINS level k's, and a
# superset is the safe direction - the table only shrinks the sweep's
# candidate set, and the expansion is what establishes an edge.
#
# Verified on room (1,0) at H=72 rather than assumed: level 0 has 166,455
# pairs, k=1 34,137 and k=2 21,579, both strict SUBSETS (zero pairs outside
# level 0's table); and `g` is identical at k=1..3 with the borrowed table
# (posgraphsharecheck.sh). `--pos-graph-from` re-checks the fingerprint at
# every coarser precision level, so a table from a different recipe, room,
# chunk cap or win target is still refused.
SHARE_POSGRAPH=${SHARE_POSGRAPH:-1}
case "$SHARE_POSGRAPH" in
  1) echo "pos-graph: banded levels borrow level 0's table" ;;
  0) echo "pos-graph: rebuilt per level (SHARE_POSGRAPH=0)" ;;
  *) echo "SHARE_POSGRAPH must be 0 or 1" >&2; exit 1 ;;
esac
for H in $(seq "$FROM" "$TO"); do
  echo "=== horizon $H: level 0 extend + sweep ==="
  # A level-0 tree built past this horizon in one process (which is how a
  # campaign avoids paying a resume's reload and re-merge per horizon)
  # already contains this horizon's row table: the search is frontier-only
  # and each
  # frame's rows depend only on earlier frames, so f$H here is what a run
  # stopped at $H would have written. `bench --resume` would refuse it -
  # "latest checkpoint is beyond --frames" - so skip the stage rather than
  # rebuild the room.
  if [ -d "$L0/f$(printf %03d "$H")" ]; then
    echo "level 0 already covers f$H - skipping the extend"
  else
  stage "l0-bench-h$H" /tmp/l0-h$H.log \
      ./target/release/rewrite --recipe "$RECIPE" bench --frames "$H" --deopt \
      --checkpoint-dir "$L0" --save-frames --resume $FUSE_ARG \
      ${VARIANT_ARGS[@]+"${VARIANT_ARGS[@]}"}
  tail -2 /tmp/l0-h$H.log
  fi
  # The sweep's origin-tagged plain replays of fruit states blow up on
  # UnknownBool branch doubling; a much tighter per-state lane cap than the
  # forward pass needs (see the h88 OOM postmortem in room00-plan.md).
  #
  # The base cap came down 100000 -> 8000 when the replay went parallel:
  # the thing the old figure bounded was the transient of ONE chunk in
  # flight, and there are now 16. 16 x 8000 is below the old 1 x 100000,
  # and small chunks are what give the threads work at all (a 250k-lane
  # sweep batch was only 3 chunks at the old cap). The fruit divisor comes
  # down with it so the fruit chunk stays ~1000 lanes rather than 80.
  #
  # The position graph is built in its OWN process, even though the sweep
  # would build it itself through the same function. The replay's transient
  # is the biggest allocation either process makes (~76 GB on room (0,0)) and
  # glibc does not hand it back: with the table built in-process the post-index
  # RSS was 94 GB and the backward loop was OOM-killed, against 41.6 GB when the
  # same table was loaded from posgraph.bin. It also banks the replay on disk.
  #
  # On room (0,0) this stage is the memory high-water mark of the whole
  # campaign (101.08 GB at f093, hence MEM=108G) and it does NOT come down
  # with CELESTE_POSGRAPH_GROUP_LANES or with fewer threads at that depth -
  # both were measured and both still OOMed. What works below f090 is the
  # group knob (250k -> 100k took f001..f080 from 40-76 GB to 6-10 GB with a
  # BYTE-IDENTICAL table), and what works above it is one process per few
  # frames, since glibc keeps the arenas between frames. `rewrite pos-graph
  # --frames N` extends an existing table, so staging it is just a loop.
  stage "l0-posgraph-h$H" /tmp/l0posgraph-h$H.log \
      ./target/release/rewrite --recipe "$RECIPE" pos-graph \
      --checkpoint-dir "$L0" --frames "$H"
  stage "l0-sweep-h$H" /tmp/l0sweep-h$H.log \
      ./target/release/rewrite --recipe "$RECIPE" sweep \
      --checkpoint-dir "$L0" --frames "$H" --horizon "$H"
  grep -E "abstract optimal|win seeds" /tmp/l0sweep-h$H.log
  refuted=0
  for K in $(seq 1 "$MAXK"); do
    PREV_BITS=$((K - 1))
    if [ "$K" -eq 1 ]; then PREV=$L0; else PREV=$KROOT-k$PREV_BITS; fi
    KDIR=$KROOT-k$K
    echo "=== horizon $H: k=$K banded (band from bits $PREV_BITS) ==="
    rm -rf "$KDIR"
    stage "k$K-bench-h$H" "/tmp/k$K-h$H.log" \
        env CELESTE_REM_BITS=$K ./target/release/rewrite --recipe "$RECIPE" bench \
        --frames "$H" --deopt --checkpoint-dir "$KDIR" --save-frames \
        --band-dir "$PREV" --band-horizon "$H" --band-prev-bits "$PREV_BITS" \
        ${VARIANT_ARGS[@]+"${VARIANT_ARGS[@]}"}
    tail -3 "/tmp/k$K-h$H.log" | head -1
    if ! grep -q "first room-exit" "/tmp/k$K-h$H.log"; then
      echo "=== horizon $H REFUTED at k=$K ==="
      refuted=1
      break
    fi
    echo "=== horizon $H: k=$K wins; sweeping level $K ==="
    if [ "$SHARE_POSGRAPH" = 1 ]; then
      SHARE_ARGS=(--pos-graph-from "$L0")
    else
      SHARE_ARGS=()
      stage "k$K-posgraph-h$H" "/tmp/k${K}posgraph-h$H.log" \
          env CELESTE_REM_BITS=$K ./target/release/rewrite --recipe "$RECIPE" pos-graph \
          --checkpoint-dir "$KDIR" --frames "$H"
    fi
    stage "k$K-sweep-h$H" "/tmp/k${K}sweep-h$H.log" \
        env CELESTE_REM_BITS=$K ./target/release/rewrite --recipe "$RECIPE" sweep --banded \
        --checkpoint-dir "$KDIR" --frames "$H" --horizon "$H" \
        ${SHARE_ARGS[@]+"${SHARE_ARGS[@]}"}
    grep -E "abstract optimal|win seeds" "/tmp/k${K}sweep-h$H.log"
  done
  if [ "$refuted" -eq 0 ]; then
    echo "=== ALL LEVELS THROUGH k=$MAXK WIN AT HORIZON $H: CONCRETE OPTIMUM = $H ==="
    break
  fi
done
