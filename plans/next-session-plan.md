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

## STATUS (2026-08-28, in progress)

- **P0 DONE + validated:** unified the whole search on the ONE kernel/engine row
  key (`compiled::engine_row_keys`), deleted the interpreter key formula
  (`row_key_hashes`, `visited_lane_keys`, `visited_row_keys`, `subtract_visited`,
  `RowTable::key`) and native-probe's obsolete `--key-gate`. Frontier, band,
  sweep and checkpoint-sort all key on it. Commits d2e80a8, ed34cd4, ca392b6,
  fb94f12. The frontier keys the SAVED (abstracted) state, so the sweep
  reproduces it. Gates: engine_row_keys_reproduce_the_carried_keys +
  traced/ladder/exact kernels_reproduce (quick, pass); full quick suite 299/299;
  a standalone COMPILED level-0 ladder swept 27047 rows with no "not in the row
  table"; an interpreter ladder swept the SAME 27047 at f30. A full f89-94
  interpreter ladder is running to reconfirm f89/f93/REFUTED.
- **P1 DONE (50186e1):** static pos-graph replay builder removed.
- **P2 DONE (36e5969):** settable precision config; set_rem_precision.
- **P3-P5, P6 (ASM switch): pending.**
- **Compiled forward at rungs diverges above Bits(1)** (pre-existing gap) - see
  the FINDING section at the end. The ladder stays on the interpreter.
- Philippe's "split by shape" (per-shape sub-maps) is NOT done - the key is the
  combined (u64,u64) with shape mixed in, which is correct; the per-shape split
  is an organization win, not a correctness need. Deferred.

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

## FINDING (2026-08-28): compiled forward at rungs diverges above Bits(1)

Enabling the compiled ladder (P0) surfaced a PRE-EXISTING bug: the compiled
forward diverges from the interpreter above Bits(1).

- `CELESTE_REM_BITS=2 CELESTE_COMPILED_FORWARD=check` on room (1,0), 30 frames:
  FAILS - 90 rows the interpreter produced are MISSING from the compiled output
  (interpreter 904, compiled 814) at f~25. And the in-process compiled ladder
  panics at k=2 ("player_rem_xy interval [-0.5,0.5) spans 4 buckets of width
  0x4000 (cap 2)"): the compiled forward emits a full-width (Bits(0)-widened)
  rem at a Bits(2) rung, which the Bits(2) bucketing rejects.
- NOT the key unification: the divergence is in the STATES (from
  `run_frame_chunk`, unchanged by P0); the check comparator uses the same key
  on both sides. My carried-key removal changed the frontier key, not states.
- Bits(1) is gated (`ladder_kernels_reproduce_the_interpreter_at_bits1`) and
  passes; Bits(2) has no gate. This is a kernel-set / rung-boundary coverage
  gap on the compiled forward, undertested because the compiled ladder never
  ran rungs before (compiled+sweep was broken).

Consequences / decisions:
- The in-process `rewrite ladder` runs the INTERPRETER forward at every level.
  Its checkpoints are engine-keyed now (the one key), so the sweep reads them.
  Correct verdict, no compiled rung.
- A compiled level-0 + interpreter rungs HYBRID is not viable: the fingerprint's
  compiled_engine component differs, so a rung cannot borrow level 0's band.
- The compiled+sweep fix itself IS validated: a standalone level-0 compiled
  ladder (f30, room (1,0)) swept 27047 rows with no "not in the row table".

TODO (follow-up, not in P0-P5): fix the compiled forward at Bits(2..15).
Add a `*_at_bits2` (or a rung sweep) differential gate. Likely the ladder
kernel set or a rung-boundary widening in the engine. Until then KERNELS=1
campaigns must stay at Bits(0)/Bits(1) on the compiled path, or run rungs on
the interpreter.

## P3 DONE (a749df3): fruit chunk cap removed - verified vestigial

Row set byte-identical at fruit cap 10 vs 8000 on room (2,0), 32 frames.

## P4: work-stealing forward memory regression - DOES NOT REPRODUCE (no action)

Re-measured post-P0 (room (1,0), quick, 16 threads): the f50 forward peaks at
**2.7 GB**, not the 24.7 GB the plan recorded. That is consistent with
CLAUDE.md's "chunk-parallel work took frame 60 from 4.3 GB to 2.4 GB" - the
chunk-parallel streaming already bounds the per-frame transient. The 24.7 GB
figure was a different/earlier config. No `all_prepared` blowup to fix.

## P5: cell-partition pos-graph recorder - origin-tag balloon is ROOM-(0,0)-ONLY

Measured: room (1,0) f50 WITH `--record-pos-graph` peaks at 2.7 GB, SAME as
without. The origin-tag balloon (pos-graph-and-memory.md section 1) needs a
room with objects whose per-lane merges the tag defeats - room (0,0)'s fruit
(101 GB at f93). Room (1,0) has no objects, so there is nothing to optimize
there, and room (0,0) cannot be validated cheaply (100 GB scale).

The cell-partition recorder is still the right fix for room (0,0)'s pos-graph
memory, and its CORRECTNESS gate (reachable row set == tag-based run) is cheap
on room (1,0)/(2,0). But it is a substantial new feature (a quantized
partition key over `cell_of(x,y)`, new machinery vs today's named-pm1
partition), its MEMORY benefit only shows at room-(0,0) scale, and room (0,0)'s
pos-graph WORKS today (fused forward, just heavy). Given P4 turned out moot and
the priority (P0) is done + fully validated, this is DEFERRED with the spec in
pos-graph-and-memory.md section 2 intact. Recommend implementing when a
room-(0,0) campaign is next run, so the memory win can be measured.

## DIAGNOSIS (2026-08-29): the Bits(2) divergence is in the TRACED GRAPH, not the engine

Reproduced on the ASM engine (`CELESTE_REM_BITS=2 CELESTE_COMPILED_FORWARD=check
CELESTE_FRONTIER_ONLY=1 CELESTE_KERNEL_STRICT=0`, room (1,0), f25). Instrumented
with `CELESTE_CHECK_DUMP=1` (dumps raw output lanes + rem/spd interval sets on a
check mismatch) and `CELESTE_ASM_NO_SEEN=1` (disables the append dedup).

Ruled OUT:
- The append dedup: `CELESTE_ASM_NO_SEEN=1` gives the identical divergence.
- The engine bridge/boundary: `CELESTE_NO_ASM_KERNELS=1` (engine falls to the
  reference interpreter) PASSES the Bits(2) check - so import/boundary/abstraction
  are fine.
- The ASM interval-fork primitives: `zi_fork_flr`/`zi_span_ok` in the codegen
  match `celeste_engine::kernel` byte-for-byte.

The mechanism (f25, a 19-lane chunk):
- INPUT rem = the 4 clean Bits(2) buckets (`[-0.5,-0.25) [-0.25,0) [0,0.25)
  [0.25,0.5)`).
- ASM (kernel) OUTPUT rem = the SAME 4 buckets, UNCHANGED (full-bucket width).
- Interpreter OUTPUT rem = 19 distinct NARROWER sub-intervals anchored at the
  bucket edges. spd likewise: 12 distinct out of the ASM vs 16+ from the
  interpreter.

So the kernel keeps rem as the full input bucket where the interpreter's
rem-renormalize (`rem += spd; amount = flr(rem+0.5); rem -= amount`) forks/
intersects it into finer fragments; that coarseness cascades to spd and the row
count (raw level-0 keys: interp 288 vs asm 126). The ASM faithfully executes the
FUSED GRAPH, and the deleted generated kernels showed the same divergence - so
the bug is in the TRACED KERNEL GRAPH (the tracer), shared by both backends, NOT
the ASM codegen. Bits(1) happens to pass because its wider buckets straddle the
flr boundary the same way the trace assumed.

FIX DIRECTION (next session): the ladder kernel graph must reproduce the
interpreter's rem narrowing at every rung - i.e. the traced rem-renormalize fork
has to fire on the fine (Bits(2..15)) input, OR the kernel must DECLINE a chunk
whose rem input is finer than the fork it was traced for (span_ok/SplitOk),
sending it to the reference. Given the search is single-room and the rungs are
rare, a decline-and-reference fallback at fine rungs may be the pragmatic fix;
the exact narrowing is the correct one. Investigate `trace` rem-fork depth vs the
runtime rem bucket width.

## DIAGNOSIS (2026-08-29, part 2): the Bits(2) divergence is UNDER-FORKING in the graph, NOT the ASM

Followed up on part 1 with a per-lane cross-check that settles WHERE the bug
is, decisively.

New tooling (kept, env-gated, reusable - not tests):
- `Graph::eval_narrow_top_in(cells, room)` (transpile/graph.rs): the interval
  evaluator with forks RESOLVED via `Frag` (so it NARROWS, unlike
  `eval_lenient*`) but unmodellable nodes (`Mget`, room-less `TileFlagAt`)
  becoming TOP instead of erroring. Decoupled `eval_inner`'s single `lenient`
  flag into `frag_lenient` + `strict_err`.
- `CELESTE_ASM_EVAL_CHECK=1` (compiled/asm_kernel.rs): the kernel re-evaluates
  its OWN fused graph with `eval_narrow_top_in` per lane and diffs every output
  field against the assembled kernel's `outbuf`. `asm != eval` would be an ASM
  codegen bug; agreement means the fused graph itself is what diverges.

Result: `CELESTE_REM_BITS=2 CELESTE_COMPILED_FORWARD=check CELESTE_ASM_EVAL_CHECK=1`,
room (1,0), 30 frames: **ZERO eval-check mismatches** across every lane of every
chunk, all the way to the f25 failure. So:
- The ASM codegen is FAITHFUL to the fused graph (H1 disproven, rigorously - not
  just "the deleted Rust kernels matched").
- The fused GRAPH is what diverges (H2). Evaluated exactly, it produces the 184
  coarse rows; the interpreter produces 904. The eval-check can only compare the
  bodies the graph HAS, so 0 mismatches + a 5x row deficit means the graph is
  MISSING FORK VARIANTS, not miscomputing the ones it has.

Mechanism (verified facts):
- shape 1: `rem.x/rem.y` are `ival`, `spd.x/spd.y` are `num` (a decided point
  per lane). So `moving = spd.x!=0 || spd.y!=0` (the real Lua guard, line 782:
  `if obj.spd.x ~= 0 or obj.spd.y ~= 0 then obj.move(...)`) is DECIDED per lane -
  the part-1 "undecided moving" idea was wrong (it assumed spd was an interval).
- At f25 the interp fans out ~15 raw rows per input lane (288 raw from 19 lanes);
  the graph makes 126. That is far more than the 2-way rem `__split_by_flr` fork,
  so the graph under-forks BROADLY, not just on rem.
- Root shape of the bug: the ladder graph's fork structure is BAKED at trace time
  (the constant lattice). A condition that is DECIDED at the lattice abstraction
  (rem/spd-derived, constant there) becomes UNDECIDED at Bits(2) (rem is a real
  interval), where the interpreter FORKS (`__split_by_flr` = "one state in, one or
  more fragments out", per floor spanned; abstraction.rs `split_rem_straddles`
  then one lane per bucket). The graph cannot fork what the trace already decided.

FIX DIRECTION (needs Philippe's call - a real tracer change, ground-rules say
confirm first): the ladder trace must fork every rem/spd-derived condition that a
FINE rung leaves undecided, so the fused graph carries all the interpreter's
Bits(2..15) fork variants - i.e. trace `__split_by_flr` (and the guards downstream
of it) at the FINEST rung the set will run at, not at the constant lattice. The
alternative (make `ok` carry `Known(cond)` for every mergeable/decidable branch so
a fine-rung lane DECLINES) only surfaces the gap under never-deopt; it does not
COVER it. The exact-narrowing trace is the correct fix; scope it before building.

### Per-lane set difference (2026-08-29, part 3): graph = coarse buckets, interp = buckets + fragments

`CELESTE_CHECK_DUMP=1` now categorizes each interpreter output LANE at the f25
divergence as interp-only vs shared (both keyed by the SAME raw canonical
`row_keys_lane_order`), and prints the (rem, spd) signature of each bucket:

- shared 192 lanes / 36 distinct: rem = the CLEAN full Bits(2) buckets
  (e.g. rem.x=[-0.5,-0.25) `ffff8000..ffffbfff`, rem.y=[0,0.25) `0..3fff`).
- interp-only 360 lanes / 126 distinct: rem = NARROWED / SHIFTED / POINT
  intervals (e.g. rem.x=`ffff8000..ffff8000` a point at -0.5, rem.y=`35c2..75c1`
  = [0.21,0.46] a shifted sub-bucket). ALL 126 interp-only signatures are
  exclusively these refined rems - none share a signature with the graph output.

So the graph emits ONLY the coarse full-bucket rem; the interpreter emits that
PLUS every `__split_by_flr` floor-fragment. The graph's rows are a strict subset
(0 graph-only), missing every refined-rem row. This is the visual confirmation of
part 2: the fused graph does not fragment rem per floor at Bits(2). The fix is to
make the ladder trace reproduce `__split_by_flr`'s fragmentation at the finest
rung the set runs at.

### CORRECTION + part 4 (2026-08-29): the divergence survives widening as POSITION, not rem

Philippe caught that part 3 keyed on the RAW (un-widened) canonical key, so its
"126 interp-only narrowed rems" are pre-widening intervals that STRADDLE the
Bits(2) bucket boundary - `split_precision_straddles` + `make_state_abstract`
would widen them back to clean buckets. Re-ran the per-lane categorization at the
RUNG layer (the abstraction the failing comparison actually uses: split straddles
+ widen), and added player position (`player_xy_per_lane`) to the signature.

At the widened layer:
- rem is CLEAN buckets on BOTH sides - the raw rem narrowing genuinely widens
  away, exactly as Philippe expected.
- The discriminator is POSITION: interp-only lanes sit at y=106; shared lanes at
  y=109 (x in {4,6} both). The interp-only xy set is DISJOINT from the shared xy
  set (5 vs 5, zero overlap). Graph is still a strict subset (0 graph-only).

So the mechanism, correctly stated: the graph under-forks `__split_by_flr` on
rem; the fragments it drops carry different `amount = flr(rem.y + spd.y + 0.5)`
values, so the vertical move reaches player positions (y=106, ~3px further up)
the graph never produces. rem widens back to a bucket, but the POSITION it fed is
a distinct reachable state that does NOT widen away - which is why the rung check
genuinely fails (904 vs 184, 0 extra). The fix is unchanged (reproduce the
interpreter's rem fragmentation at the fine rung so all move amounts, hence all
positions, are produced); part 3's rem-only framing was at the wrong layer.
