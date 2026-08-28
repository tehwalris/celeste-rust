# Position graph memory + the work-stealing memory regression

Findings from the 2026-08-28 overnight session, for whoever picks this up.

## 1. Why `--record-pos-graph` is memory-heavy (it is NOT the graph)

The position graph itself is tiny: ~21k deduped `(src cell, dst cell)`
pairs over ~946 destination cells for room (1,0) at f40 - kilobytes. The
100 GB figure (room (0,0) at f93, `ladder.sh` MEM=108G) is NOT the graph.

It is the **origin tag**. To attribute each output lane back to the source
cell it came from, `PosObserver::tag` injects a per-lane `POS_ORIGIN` column
(the lane's cell) into every input lane. That column becomes part of the
kernel's dedup key, so mid-frame lane merges DECLINE (deliberately - a merge
would fold two source cells into one lane and the table would silently MISS a
pair, the unsound direction). The tag is stripped at the boundary
(`interpret_state_base`, `global_env.remove(POS_ORIGIN)`), so the FRONTIER
stays compact - but the per-frame transient balloons because the kernels keep
origin-distinguished lanes that would otherwise merge.

Consequence: on-the-fly recording (`bench --record-pos-graph`) and the
replay builder (`pos_graph::build_from_replay`) have the SAME origin-tag
cost. So "use on-the-fly instead of the static build" does not reduce the
memory - both tag per lane.

## 2. The cell-partition idea (Philippe, 2026-08-28) - the real memory fix

Instead of a per-lane origin tag that defeats dedup, make the SOURCE CELL
constant across each processing group, then no tag is needed: every output
of a group pairs with that one src cell (dedup across lanes is fine - we
dedup pairs anyway).

- Today `player_xy_per_lane` is a per-lane column and the pm1 partition is
  `[dash_time, djump, has_dashed, p_dash, p_jump, freeze]` - position is NOT
  partitioned. So lanes in a group mix source cells.
- The fix: partition the recording forward ALSO by the derived cell
  (`pos_graph::cell_of(quantized x,y)`). Then each group has one src cell;
  read output cells post-dedup; record `(src_cell -> each output cell)`.
- Soundness framing is the SAME as the tag today: mid-frame fragments differ
  from an untagged run, so the reachable row SET must be CHECKED equal (the
  existing gate). Constant-src-per-group makes every recorded pair valid and
  complete (no missing pairs), so it is sound in the same direction.
- Cost: partitioning by cell splits into ~hundreds of groups (bounded by
  tiles), far cheaper than the per-lane tag which forbids ALL merges. Should
  cut both memory and time, and it is the thing that would make room (0,0)
  affordable.
- Complication: the partition machinery (`resolve_partition_cells` /
  `split_states_by_partition`) partitions by NAMED cells; the pos-graph cell
  is derived/quantized, so this needs a new quantized partition key rather
  than adding a name to pm1. Moderate work; prototype behind the reachable-
  set-equal gate before trusting it.

## 3. Why the statically-built pos-graph was NOT removed (deferred)

Philippe asked to remove the statically-built path (`build_from_replay` +
the `pos-graph` subcommand + the sweep's inline fallback in
`prepare_pos_graph`) in favour of on-the-fly recording. Deferred, because:

- **Room (0,0) still depends on it.** `ladder.sh` only fuses
  (`--record-pos-graph`) for rooms (1,0) and (2,0); room (0,0) is "NOT gated
  yet" and keeps the replay. Removing `build_from_replay` breaks room (0,0),
  which has no verified on-the-fly path (its fused table is ungated -
  `ROOM=0,0 ./posgraphcheck.sh` was never run).
- **It does not achieve the stated goal.** The memory objection is the
  origin tag (section 1), which on-the-fly has too. The cell-partition work
  (section 2) is the actual memory fix.

Safe sequencing to actually remove it:
1. Land the cell-partition recorder (section 2), gated by reachable-set-equal.
2. Gate room (0,0)'s on-the-fly table (`posgraphcheck.sh` for room 0,0).
3. THEN remove `build_from_replay`, the `pos-graph` subcommand, and the
   `prepare_pos_graph` fallback; make the sweep require a loaded/borrowed
   table with a loud error otherwise.

## 4. The work-stealing forward's peak-memory regression

The 2026-08-28 work-stealing rewrite (`step_parallel`, commit e6636d1)
moved `partition_filter` + `decided_survivors` from PER-BATCH to PER-FRAME:
the old code processed states in batches of `threads`, filtering and freeing
each batch's materialized outputs before the next; the new code work-steals
the whole frame's interpret, accumulating ALL output `State`s in
`all_prepared` before a single filter. `PreparedRows` holds a full `State`,
so the transient peak is now a whole frame's boundary states instead of one
batch's.

Measured (room (1,0), quick): forward peak RSS 9.28 GB (old, release) ->
24.7 GB (new). Fine for room (1,0); a 2.7x multiplier would be fatal at
room (0,0)'s ~100 GB scale.

Fix (planned): process the frame in input-order SUPER-BATCHES (e.g. a few x
`threads`, or a lane budget), work-stealing the interpret WITHIN each
super-batch, then filter/decide/free per super-batch. Bounds the transient to
one super-batch while keeping the load-balancing win (a super-batch is >>
`threads`, so no idle-at-barrier). Byte-identical: per-super-batch filtering
gives the same result as per-frame (frozen frontier + persistent
`partition_seen`), and input order is preserved. Hold it to parcheck.

Not yet done - flagged so the speed win (-36% f50) is not mistaken for free.

## 5. COMPILED forward + backward sweep is BROKEN (found 2026-08-28)

While validating the in-process `ladder`, found that a **compiled** forward
(`CELESTE_COMPILED_FORWARD=1`) writes a checkpoint the backward sweep cannot
read: the sweep's index-build recomputes each saved-frame lane's key with
`sweep_time::row_keys()` and looks it up in the row table, and it fails at
f001 with "a saved lane's row is not in the row table". The INTERPRETER
forward is fine end-to-end.

Isolation (room (1,0), f30, quick):
- interpreter fwd + record-pos-graph -> sweep: WORKS.
- compiled fwd, NO record-pos-graph -> sweep: FAILS ("not in the row table").
- compiled fwd + record-pos-graph -> sweep: FAILS.

So it is NOT the pos-graph and NOT the lane count (compiled-no-pg and
interpreter both give 11774 lanes at f30) - it is that the **compiled
forward stores a different row key than `row_keys()` recomputes.** Almost
certainly the engine-key change (Option 1: the kernel emits the boundary
key inline, committed at 67d5a03 before this session's work-stealing): the
table now holds the kernel-emitted engine key, but the sweep's `row_keys()`
still computes the interpreter/`Rt2::boundary` key, and they are not
byte-equal for at least some rows. The frontier dedup does not notice
(both sides of a within-run comparison use the same key), which is why
parcheck and the forward-only runs passed - the mismatch only surfaces
when a SECOND stage (the sweep) recomputes keys with the other function.

Impact: the real campaign uses KERNELS=1 (compiled). If this regressed at
67d5a03, the compiled campaign's sweep is broken. BENCHMARK_DATA's
2026-08-26 KERNELS=1 campaign predates 67d5a03, so it would not have hit
this. NEEDS a fix before the compiled path can run the ladder:
- Either make the kernel-emitted engine key byte-identical to `row_keys()`
  for EVERY row (find where they diverge - likely a cell the boundary key
  includes that the engine key does not, or vice versa), OR
- have the sweep read the STORED key for each saved lane instead of
  recomputing it (the `.rowkeys` files already carry them).

Until fixed, the `ladder` subcommand runs the interpreter forward (correct,
checkpoint-compatible with the sweep). This is also what ladder.sh does by
default (compiled only under KERNELS=1).

## 6. The in-process `ladder` subcommand (landed 2026-08-28)

`rewrite ladder --from N --to M --maxk K --checkpoint-dir DIR --room x,y`
replaces ladder.sh's default path in one process: per horizon it extends
level 0 (forward + fused pos-graph on the interpreter), sweeps it, then runs
banded rungs k=1..K, REFUTING a horizon at the first rung that never reaches
the exit. Needed two fixes beyond the obvious orchestration:
- `CELESTE_FRUIT_CHUNK_LANES=8000` - CHUNKING IS SEMANTIC on fruit rooms and
  it is part of the fingerprint; missing it gave 651M rows instead of 178M.
- a settable override on `rem_precision_from_env` (`set_rem_precision_override`)
  - the env read is a read-once OnceLock, so a single process cannot express
  level 0 then k=1 then k=2 without it.
