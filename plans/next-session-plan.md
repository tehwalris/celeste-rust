# Next-session plan (agreed with Philippe 2026-08-28, post-overnight)

This is the execution plan after compaction. Ordered by priority. Each item
says WHAT, WHY, and concrete STEPS. Companion findings are in
`plans/pos-graph-and-memory.md`.

## State of the tree (committed + pushed to origin/census)
- Work-stealing forward (e6636d1), phased/sweep (fbebb72), parallel merge
  (7a14292): frame -43% at f50, byte-identical (parcheck).
- In-process `ladder` subcommand (fe1822f): `rewrite ladder --from N --to M
  --maxk K --checkpoint-dir DIR --room x,y`. Runs the INTERPRETER forward
  (compiled+sweep is broken - P0 below). Verdict byte-identical to
  ladder.sh (f89 / f93 / REFUTED).
- BENCHMARK_DATA (2215e27): tonight's quick campaign, 589 s.

## Correction to last night's notes
The `CELESTE_FRUIT_CHUNK_LANES=8000` I added to `run_ladder` is almost
certainly a NO-OP: `effective_fruit_chunk_cap()` already defaults to 8000
and the ladder sets `MAX_STATE_LANES=8000`. The 651M-vs-178M row blowup I
blamed on it was really the COMPILED forward path (origin-tag/key handling),
same root cause as the sweep bug. Verify and remove the redundant set_var in
P2/P3. (Compiled WITHOUT pos-graph gives 11774 lanes = interpreter, so
compiled dedup itself is fine; it is compiled + pos-graph + sweep that
breaks.)

---

## P0 (PRIORITY): fix compiled forward + backward sweep - unify on the ENGINE key

### The bug
A compiled forward (`CELESTE_COMPILED_FORWARD=1`) writes a checkpoint the
sweep cannot read: it stores the kernel-emitted engine key in the row table,
but `sweep::row_keys` / `virtual_merge::row_key_hashes` recompute a DIFFERENT
key (they mix shape+content into both halves of a combined `(u64,u64)` via
`contrib`/`finalize`), so the sweep's index-build fails at f001 with "a saved
lane's row is not in the row table". It has never mattered for a forward-only
run (both sides of a within-run compare use the same function) - only the
second stage (the sweep) recomputing with the other function exposes it.

### The fix (Philippe's direction)
Switch EVERYTHING to the keys the engine already uses: **`(shape_hash,
content_hash)` kept SEPARATE**, not hashed together. shape_hash = the
object-shape hash; content_hash = the hash of the state's cell contents
(the engine's cell_mix-based key). The row table / visited set becomes a map
keyed BY SHAPE (`shape_hash -> { content_hash -> id }`), usually as separate
per-shape hash maps; only concatenate to one key where a flat structure is
genuinely needed, and prefer not to.

### Why per-shape maps (not one combined key)
- Kernels are per-shape; the engine produces content hashes within a shape.
- Keeping shape as the outer key means the interpreter, the compiled engine,
  and the sweep all agree by construction - the content_hash is computed one
  way (the engine's cell_mix) and never re-derived through a second function.
- It removes the per-lane shape mixing that `row_key_hashes` does.

### Steps
1. Nail the divergence first: dump, for one saved-frame lane, the STORED key
   (compiled forward) vs `row_keys` recompute, and confirm it is the hash
   function that differs (cell_mix vs contrib/finalize), not a missing cell.
2. Pick the engine's content-hash function as canonical. Make ONE function
   both the interpreter (`visited_lane_keys` / `row_key_hashes`) and the
   sweep (`sweep::row_keys`) call, computing content_hash the engine's way.
3. Change the row table / visited set to key on `(shape_hash, content_hash)`
   with per-shape sub-maps. `crates/celeste-interp/src/interpreter/
   row_table.rs`, `visited.rs`, and the boundary in
   `crates/celeste-engine/src/runtime2.rs` (`boundary`, `boundary_dedup`).
4. Re-hash the FINGERPRINT decision: keys change, so old checkpoints are
   incompatible (fine - scratch dirs). The engine identity already feeds the
   fingerprint; make sure interpreter and compiled now share one key space so
   a compiled checkpoint is sweep-readable.
5. GATE: `parcheck.sh` byte-identical; `CELESTE_COMPILED_FORWARD=check` sets
   identical; and the NEW gate - a COMPILED `rewrite ladder` reproduces
   f89/f93/REFUTED with the same row counts as the interpreter. Then flip the
   ladder back to the compiled forward (drop the interpreter-only note in
   run_ladder) and re-benchmark.
6. Also fixes the 651M blowup: compiled + record-pos-graph currently keeps
   origin-distinguished rows in the frontier (the strip/dedup uses the
   diverging key); the unified key should dedup them to 178M. Confirm.

---

## P1: remove the statically-built pos-graph

### What
Delete `pos_graph::build_from_replay`, the `PosGraph` CLI subcommand
(rewrite.rs), and the `prepare_pos_graph` inline fallback. The sweep then
REQUIRES a pos-graph that was recorded on-the-fly (`bench --record-pos-graph`,
fused into the forward) or borrowed (`--pos-graph-from`); missing -> loud
error, never a silent in-process rebuild (which was the room-(0,0) OOM).

### Room (0,0)
It currently relies on the replay (its fused path is ungated). Handling:
make the forward record on-the-fly for ALL rooms (the ladder already fuses),
and if room (0,0)'s fused table needs the gate, run `ROOM=0,0
./posgraphcheck.sh` as part of this. If that gate is out of scope now, the
loud error is acceptable (it names the fix) - but do NOT leave a silent
rebuild. Confirm room (0,0) can fuse before deleting the replay, OR land P0's
unified key first (a cheaper/correct fused forward may make room (0,0) fine).

### Steps
1. `prepare_pos_graph`: keep load + borrow; replace the `build_from_replay`
   branch with an error.
2. Delete `build_from_replay` and the `PosGraph` subcommand + its enum
   variant + handler.
3. Update ladder.sh: drop the `l0-posgraph` / `k*-posgraph` subcommand
   stages (fused forward + borrow already cover it); keep SHARE_POSGRAPH=1.
4. The in-process ladder already skips the noop posgraph stage - no change.
5. Gate: `rewrite ladder` still reproduces f89/f93/REFUTED.

---

## P2: rewrite the precision / env config to be NICE (not shell-oriented)

### Why
`rem_precision_from_env` (and `spd_precision_from_env`, chunk caps, etc.)
are read-once OnceLocks that read env vars - written for ladder.sh spawning
one process per stage. That is why a single-process ladder needed the
`set_rem_precision_override` hack. Make it a proper settable config instead.

### What
- A `LadderConfig` (or similar) value: rem precision, spd precision, chunk
  caps, frontier-only, compiled-forward, room. Threaded through `AbstractRun`
  or held in ONE settable process global that is SET explicitly (env is just
  the default source at startup), not a read-once cache.
- Delete `set_rem_precision_override` and the REM_OVERRIDE atomic once the
  real config exists.
- Delete the redundant `CELESTE_FRUIT_CHUNK_LANES` set_var in run_ladder
  (see the correction above) - verify it is a no-op first.
- Keep env vars working as the DEFAULT/source (ladder.sh and existing tests
  set them), but make in-process overrides first-class.

### Gate
Full nextest suite; `rewrite ladder` per-rung precision correct (k=1 gives
1,163,134 rows / f93, k=2 REFUTED).

---

## P3: the fruit chunk cap is probably VESTIGIAL - verify and remove it

Philippe (2026-08-28): the fruit chunk cap was only ever needed to work
around a BUGGY interval-split interaction that has since been fixed; it
probably does nothing now. So the hypothesis is NOT "chunking is genuinely
semantic on fruit" but "this was a workaround for a bug that no longer
exists."

Steps:
- Confirm it is a no-op today: vary `CELESTE_FRUIT_CHUNK_LANES` on a fruit
  room (1,0) forward and check the row SET is invariant (only ids/order
  change), i.e. the same thing parcheck asserts for the general cap. If the
  set is invariant, the "CHUNKING IS SEMANTIC ON FRUIT" claim is stale.
- If confirmed vestigial: REMOVE the special fruit cap
  (`effective_fruit_chunk_cap`, the `CELESTE_FRUIT_CHUNK_LANES` reads, the
  run_ladder set_var, and its contribution to the config fingerprint) so a
  fruit room chunks like any other. Removing it from the fingerprint is a
  key-space change - fine on scratch dirs.
- Look at the fixed interval-split interaction (abstraction.rs:773 area,
  `split_rem_straddles`, the fruit `off` widening) to confirm the bug it
  guarded against is really gone before deleting the guard.
- If it turns out NOT vestigial (the set does change with the cap), stop and
  write up why - that would be a real cap-dependent over-approximation.

---

## P4: work-stealing forward peak-memory regression - find the REAL cause

### The observation
Forward peak RSS went 9.28 GB (pre-work-stealing) -> 24.4 GB. What changed
mechanically: `partition_filter` + `decided_survivors` moved from PER-BATCH
(the old code filtered/freed each batch of ~16 states' output rows before
the next) to PER-FRAME (all workers' output `PreparedRows` - each a full
`State` - accumulate in `all_prepared`, then ONE filter). So a whole frame's
output rows are live at once instead of a batch's.

### Philippe's note (2026-08-28) - do NOT assume the super-batch fix
Work-stealing itself does not increase memory. Reducing its granularity (the
"super-batch" idea I wrote first) should NOT be necessary to fix this
properly. Find the real reason the transient is large and bound it WITHOUT
giving back work-stealing's balance. Candidates to investigate:
- Stream survivors out INCREMENTALLY as workers produce them (filter +
  subtract + free per chunk of output) while the interpret still work-steals
  the whole frame - decouple "when we filter" from "how we schedule".
- Or the rows do not need to be fully materialized `State`s before filtering
  at all - keep only keys until a lane survives, materialize lazily.
- Check whether the peak is really `all_prepared` or something else (the
  within-frame set, the frozen-frontier probe buffers) before optimizing.

Byte-identical is the constraint (parcheck). Lower priority than P0-P2; a
memory concern, not correctness.

---

## P5: cell-partition pos-graph recorder (the real memory fix) - ON the list

This is the memory fix for `--record-pos-graph` (the origin tag balloons the
transient; section 1 of pos-graph-and-memory.md). Philippe: do NOT defer -
it is P5, on the list. Here is what "quantized partition key" means so it is
not mysterious:

- Today the pos-graph tags EVERY lane with its source cell (`POS_ORIGIN`) so
  outputs can be attributed to inputs. That tag is per-lane-distinct, so it
  forbids all mid-frame merges -> the transient balloons.
- Philippe's idea: instead, make the SOURCE CELL constant across a
  processing group, then no tag is needed (every output pairs with that one
  src cell). To do that, PARTITION the recording forward by the source cell.
- The merge already partitions by NAMED cells (pm1: dash_time, djump, ...).
  A pos-graph "cell" is not a named cell - it is `cell_of(quantized x, y)`,
  a function of the player position. So we need a partition key that is a
  QUANTIZED FUNCTION of position (which cell you are in), not a raw named
  cell value. That is the only new machinery: a partition that splits by
  `cell_of(x,y)` rather than by a cell's exact value.
- Then read output cells post-dedup and record `(group's src cell -> each
  output cell)`. Cheaper than the tag (merges allowed within a cell), and
  sound (constant-src-per-group => every pair valid and complete). Gate:
  reachable row SET equal to a tag-based run.

---

## Suggested execution order
P0 (unblocks the fast path + fixes the 651M blowup) -> P1 (remove static
pos-graph, easy, Philippe insists) -> P2 (nice config, removes the hacks) ->
P3 (fruit cap: verify vestigial and remove) -> P4 (forward memory, find the
real cause) -> P5 (cell-partition pos-graph recorder, the pos-graph memory
fix). All six are on the list.
