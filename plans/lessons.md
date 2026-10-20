# Lessons: what was tried, and the measurement that killed it

One entry per abandoned approach. Read before proposing something that
sounds like one of these. The full write-ups are in git history.

## Program transformation and kernels

- **The January CFG optimizer** (branch `interpreter-abandoned-2026-01-11`,
  ~11k lines: mem2reg, inlining, heap elimination, DCE). Mostly never wired
  in; some passes produced CFGs the interpreter cannot run; validation did
  not check dominance. A crate-wide `allow(dead_code)` of that era hid a
  whole dead dataflow framework: hence the warning-free rule.
- **Rewrite rules and generated kernel crates** (38k lines of rules, then
  ~923k generated lines, deleted 2026-08-29, `d027f7e`): rustc + LLVM took
  minutes; the AVX-512 assembler builds kernels at startup in milliseconds.
- **The old sweep search** (torn out 2026-08-31): its "proven" room (0,0)
  94 was a frame too long. Believe a result after a witness on PICO-8.
- **Local rewrites of room (6,0)'s kernels** (2026-09-26, five failures in a
  day): `ok` carried as state outlived its values (one body's cone 1,974 ->
  96,540 nodes). Deriving error from the operators took room (3,0) 6.24M
  -> 3.25M fused nodes. Abstractness as the fork trigger minted 50-90 extra
  forks a frame.
- **The eager point split** (2026-09-20): every outcome's `live` read every
  comparison, 2^70 bodies an outcome in room (6,0). Forks after the frame.
- **Platform worlds as a compile-time fork**: 2.3M bodies, 35 GB of
  kernels, a coverage gap at f36. POINTS in the split pass instead.
- **One case enumerator for the split pass and the lowering's forks**
  (audit K1, measured 2026-10-20, not built): stored selects as both-ways
  forks enumerated per outcome by `specialize_frame`. Per walk (templates,
  splits, outcomes out, sum of 2^atoms split a template): r10 767/0/767,
  r62h 3,374/0/3,238, r42n 480/204/574 (826), f42n 656/306/797 (1,175),
  platforms r60s 8,495/50,127/18,162 (2.7e11, up to 34 atoms a template).
  Idle without near floors or platforms; with them the split pass's
  ADAPTIVE order (refold after each split, `Points` per world) is what keeps
  34 atoms at 18k outcomes, which no per-outcome product of forks can.
- **Guarded regions in the fused kernels** (2026-09-14, reverted): 1.5x,
  not the promised 5-8x (a lane takes ~4-5 of 16 configurations). Kernels
  stay branch-free; the static split frame bought 10-20x in size instead.
- **Keys hashed inside the kernels**: every body hashed every lane though a
  lane takes ~22 of 13,812 bodies (0.12 IPC). Keys per emitted row: the
  bench frame ~80 s -> ~33 s. Later (2026-10-12) in-kernel key packing was
  re-measured as not worth it: the key reads are ~1.4 worker-s of a frame.
- **Diagnostics as `#[test] #[ignore]`**: five zero-assertion tests that
  only printed; nothing failed when their answer changed. Diagnostics are
  `rewrite` subcommands.
- **A register allocator for spills**: spills are 21.6% of the kernels'
  instructions but the kernels are ~2% of a frame (~0.4% of the wave).
- **Kernel regions of 8 px** (2026-10-10): -20% instructions a slice, no
  measurable wave difference, 3.2x the kernels and +1.4-5.6 s at startup.
  Kernels stay at 16 px.

## The frame and storage

- **The two-phase frame** (emit by source, own by destination, batches):
  superseded the same evening by the waves frame (room (1,0) f0-f70 peak
  9.45 -> 4.08 GB). glibc held ~23 GB more than mimalloc at f90: mimalloc
  is the allocator since.
- **The door, queues and inverted runs** (2026-09-13 .. 10-10): room (6,2)
  100% h94 search 312 s, peak 27.2 GB; storage v2 212 s, 8.9 GB.
- **Source-side edge blocks with lids, claims and the translation**
  (2026-10-10/11): 3.15 B an edge, room (1,0) h99's tree 28 GB. Bundles in
  their target regions: 0.113 B an edge, 2.4 GB. Dropped on the way: a
  route-level bundle table (DRAM-bound, region phase 31-39 worker-s
  against 5), FxHash 1.x on packed keys (long probe chains), a barrier
  between slices, re-hashing every worker's transfer table each slice
  (quadratic), relative masks (6% larger on room (1,0)).
- **Unified per-region state sets** (storage-unify study, 2026-10-10):
  region-scoped blocks 4-9% bigger than per-unit ones (wider transfer
  ranks), no record saved by persistence, 37% of persistent translations
  used once. Not built.
- **State ids** (`StateId`, frame files with id columns, drop notes by id;
  gone 2026-10-12): a row is a function of its shape's skeleton, exact key
  and cell, so ids and stored rows were 36-69% of a tree. Keys only: room
  (1,0) tree 2.50 -> 0.57 GB, peak 3.7 -> 2.1 GB, the same speed.
- **Hashed keys** (a 128-bit hash of the fields, until 2026-10-10): two
  states with equal hashes would silently be one. Exact packed keys cost
  +6% of the reference frame's instructions at first; batched emission
  later cut the key work 2.4x.
- **The raise** (re-expanding level -1's dropped sources to extend a tree's
  horizon, 2026-10-08 .. 10-13): exact for the remainder-free forward, not
  for the hull forward (a dropped push changes every later hull). Trees are
  rebuilt for a larger horizon.
- **The whole-circle forward** (every state at every remainder, until
  2026-10-13): the hull forward expanded 0.6M states against 6.6M (room
  (4,2)) and 30.6M against 315M (room (6,2) 100%), 3.4x fewer
  instructions there; room (1,0), where hulls fill, is a wash.
- **Checkpoint layouts**: a file per cell-uniform block (~17k files a
  frame, inodes); zstd bincode layers (35 s a backward iteration decoding
  rows to keep a few hundred).
- **Every level's kernels prebuilt**: ~30 GB in room (3,0). One set is
  resident; fused graphs are dropped after assembly.
- **`MIMALLOC_PURGE_DELAY` 0 or 10 ms**: a unit takes ~12 ms, so freed pages
  went before the next unit reused them (3.0M faults on the reference
  frame at 0, 0.93M at 100 ms, for ~0.25 GB of peak). `safe-run.sh` sets
  100.

## The backward

- **The kernel re-run backward** (gone 2026-10-05): the recorded graph took
  room (0,0) from 2:32 h to 23:50; an incremental version saved 15 s of 524.
- **The remainder-free BFS and the loaded rotation graph** (2026-10-04 ..
  10-11): marks were a loose superset of W's support (29% of a big tree's
  states), its CSR 12 B an edge, OOM-killed at ~104 GB. One push over the
  in-edges read from the edge files: ~15 GB on the same tree.
- **The backward as a region worklist** (`26d976d`, removed `2e75fd9`):
  regions opened 2.9 times instead of 10.2, but it read MORE (41.2M
  in-edges against 39.3M): a region opened before its successors' values
  are final is read again.
- **Marking by W non-empty instead of reach** for the ladder's filter: room
  (3,2) nodiag h145 marked 38.0M nodes against 486k reached; the filtered
  forward 681 s against 16.7 s, the same answer.

## Abstractions

- **`p_jump`/`p_dash` widened with nothing exact behind it** (2026-08-06,
  08-16): a held trail admits a ground jump then a wall jump; the ladder
  could report a spurious optimum. The rule "never widen without something
  exact that refutes it" came from it. `h` is refuted by the concrete
  search.
- **The count-up's memo keyed on the level's row key** (fixed `522de36`):
  two concrete states shared an entry, so "no concrete win within f" was no
  proof. A cache in front of an exact check must be keyed exactly.
- **Speed buckets** (2026-09-14..16): the census promised 4.5-19x; a lane
  holding a speed hull enumerates every speed's successors: room (1,0) f50
  1.11x fewer states, frames 30-36x slower.
- **The position rung** (`x2`/`y2`): halves its own forward, filters
  nothing (room (1,0) ladder 9:32 against 2:56). A coarse level only cuts
  near its own first win.
- **The time band** (8 px a frame up): a measurement, not a bound (a spring
  snap plus `spd.y = -3` is 11 px). Level -1 replaced it.
- **The fly fruit unknown above level 0**: room (3,0) f36 the exact fruit
  was SMALLER (71k against 80k states).
- **Floors fully unknown (`b`)**: room (7,0) step 80, 2.0M states against
  143k exact; `n` 5-6x smaller than `b`. `n`'s own blowup was a tracer bug
  (`7f6b96e`): look at the states before reasoning about the abstraction.
- **The timers level `t`**: 472k bodies (~16x `n`) and UNSOUND under the
  interval wrap (shaking floors never fell). Out of memory since the fix.
- **Exact speed at level 0 in spring rooms** (room (2,0)): OOM at 90 GB in
  f79, +18% a frame. Fixed by `h` + level -1.
- **Level -1 for every room** (reverted `4b31c1b`): did not rescue room
  (6,0), whose every frame is new with exact platforms (d 31 against 46).
- **Coarser level 0 for platforms by buckets**: OOM building kernels past
  60 GB. Platforms needed `p`.
- **The rem rungs and the precision ladder** (deleted 2026-10-05): a move
  straddling a bucket edge emits a sliver row the boundary widens to the
  whole bucket, so marks grew 2-2.6x a rung; six ladders of room (3,3) ran
  out of memory. The arcs refuted 171 in 74 s. The objects axis came back
  the same day as the objects ladder: room (7,0)'s concrete region grew
  6.5x a frame of slack. An abstraction's cost is its refutation's too:
  measure on the room where the bound is loosest.
- **A seeded balloon phase kept exact at `n`**: it split on every undecided
  `spr == 22` (room (4,2): 531,441 states at f9 before the player).
- **Pins as `Restrict(c, c)`**: +0.15-1.25% dynamic instructions; pins
  stay their own kind.

## Measurements behind design rules

- **Lane type as a register** (`ZN`): as `[Pico8Num; 16]` the kernel was
  61% scalar; changing only the lane type was 20x.
- **The global BDD analysis** filled its 2^22-node cap at once; a small BDD
  per node over its bounded cone finds more at ~1/150 the time.
- **`State::ended` per path** (frame-global took a kernel 78k -> 3.3M
  configurations); own errors scoped to the path's decisions, not the guard
  (456k -> 1.41M nodes); a merge selects on the separating decision (one
  `freeze` was 37 nodes on the guard); error derivation in one bottom-up
  pass (per-outcome walks: 25 s -> 448 s a trace); `flr_ways` capped (13-way
  move forks); split pass largest-cone-first (otherwise 20k splits churn).
- **Near floors store the cart's invariant where overlapped**: storing the
  computed value made room (6,1) 3,638 outcomes and 344k bodies against 108
  and 5,454.
- **The overflow checks** (2026-10-11): +1.9-3.0% kernel instructions,
  outputs identical.
- **What multiplies in an object room** (`coarse-census --erase`, now `census --erase`): room
  (3,0) f47 the fly fruit 2.30x, the speed 2.29x; room (7,0) step 108
  floors 4.6x, speed 3.2x; room (2,0) f68 `p_jump` 1.98x, speed 26.6x.
- **`mget` outside the map** is 0 in PICO-8; the branch-free kernel reads
  every lane's second column (a player at x >= 119 read column 16).
- **Re-pins** happened only on evidence that the sets did not move
  (`4e2d2e9`, `18208c6`); edge-count "re-pins" (`ffe939e`, `9921c0d`) were
  scheduling noise.
