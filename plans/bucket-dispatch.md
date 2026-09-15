# The bucket dispatch (2026-09-15)

The speed hull (a row keyed on its speed BUCKET, storing the tight
min-max actually merged into it) needs kernels that never meet a
comparison on the speed they cannot decide. This is the design agreed
with Philippe for that, what was tried on the way and rejected, and the
state of the work. The measurements behind it are in
`BENCHMARK_DATA.md` ("Bucket-dispatch probe") and the reasoning in the
2026-09-15 session.

## The rules (Philippe)

- **No runtime forks, no runtime branches.** A kernel is straight-line;
  what varies is compiled for constants and DISPATCHED to. The only
  forks are the compile-time configurations that already exist (the
  move fork, the boundary snap); nothing new is decided per lane by a
  branch, and nothing is split "when needed".
- **Everything compiled up front, in parallel.** No compile on first
  use. What has to be compiled is found by a fixpoint before any
  lowering, like the shape lattice finds the shapes; a kernel missing at
  runtime is a coverage gap (fatal), never a build.
- **The storage partition is the dispatch key.** Rows are partitioned by
  (shape, cell, the constants a kernel is specialized on), and every
  partition routes to exactly one kernel with no shuffling and no
  reserved bits in the row key. Runs inside a per-shape file are the
  accepted intermediate form (below); the layout Philippe actually
  wants is one file per partition, deferred (plans/architecture.md
  follow-up 6).
- **Loud, not lenient.** An undecided condition at a merge is a fatal
  decline (`Known(cond)` in `ok`), not a join; a stored undecided
  boolean is refused; a bucket edge missing from the table shows up as
  a decline naming the lane, not as a wrong answer.
- **Record facts where they are made.** A cell's kind lives on the
  graph (`Graph::cell_kind`, set by `symbolize`), not guessed from its
  uses; a frame carries its own fork arities and tables, not the
  arena's last ones.

## Rejected on the way

- **Sound select (join) on an undecided condition, and a boundary fork
  on undecided booleans (`fork_bools`).** Both quietly widen; both were
  cut in favour of the loud premise. (The join also turned positions
  into intervals through celeste-minimal's `spd ~= 0` guard on `move`.)
- **Changing celeste-minimal.lua** to remove that guard: no cart
  changes.
- **The refined edge table** (closing the edges under `appr`'s shifts to
  get boundary arity 3 with ~200 edges): viable, but dispatch on the
  bucket is the better shape - it folds every comparison on the input
  speed at compile time and makes the boundary arity exact per body.
- **Lazy per-bucket kernels behind a mutex**, and **sorting the lanes
  of each 64-lane id group by key at run time**: both worked and both
  are the wrong shape (see the rules).
- **Singleton buckets at every threshold**: a strict `spd > t` needs the
  edge one raw unit above `t`, a non-strict one at `t`; only an
  equality test needs `{t}` a bucket of its own.
- **The ABSOLUTE table fork** (fragment `c` = "the output is in bucket
  `c`", a constant key per body). Which bucket a lane lands in depends
  on per-lane conditions (on ground, wall, `djump`), so the fork's
  arity was every bucket any variant of a button rep reaches - ground,
  air, ice, wall jump, dash start - and x and y multiplied: 720 bodies
  per rep, 10,444 per kernel, the same compute (802 distinct values)
  checked and hashed 10,444 times per lane. Replaced by the relative
  fork (design 4).
- **Reading the dash constants field by field** in the successor reader:
  the product of four fields' values made dashes no cart state makes
  (target (2, 0) with accel (1.5, 1.5)): 12 dash sets for 8 directions.

## The design

1. **Edge tables** (`celeste_core::spd_buckets`): per axis, the rung's
   grid (2^w raw) plus the cart's thresholds, each placed by the
   comparison it serves (`Cut::Above`/`At`/`Point`), the equality tests'
   preimages under `appr` as points. Buckets nest across rungs. Every
   consumer of "the bucket of a speed" (the key widening `widen_to`,
   the key check, the trace-time snap) uses this one table.
2. **The speed key** (`trace::kernel::SpeedKey`): (x-bucket, y-bucket,
   `dash_time`, and mid-dash the dash target and accel). `dash_time`
   pinned folds the dash arm away for the vast majority of rows; the
   dash constants keyed keep the mid-dash kernels in the same size
   class as the rest (bounding them instead brought the dash arm's
   `appr` range across ~20 buckets: 40-88k bodies).
3. **The key fixpoint** (`key_fixpoint`): after the shape lattice, walk
   (shape, key) nodes from the start state's key; trace each once under
   its key (`key_frame`: speed cells bounded to the bucket, `rem` to
   [-0.5, 0.5), the key's constants pinned, all guarded in `ok`); read
   the successors off the trace (`successor_keys`): per button
   configuration and per assignment of the select conditions the
   dispatch fields depend on (so the fields are read JOINTLY), the
   buckets the output speed's pieces touch, the constant `dash_time`,
   the constant target/accel where mid-dash. Union over cases. Then one
   parallel batch of bind + lower + assemble over the whole node set.
4. **The kernels**: each traced with the key's bounds seeded as static
   ranges (`Symbolic::ranges`, `graph::pieces_of`): comparisons on the
   speed fold at trace time and the move loops shed their dead
   iterations. Both forks are RELATIVE - fragment `c` is the `c`-th cell
   from the lane's low end - with the arity ONE lane can need, the most
   cells one piece of the operand crosses (a lane's interval lies within
   one piece):
   - the `move` fork's arity comes from the operand's range at trace
     time (`Domain::flr_ways`; `move` runs before `update`, so it is
     button-independent), checked by `SplitOk`;
   - the boundary snap is a relative TABLE fork (`Op::SplitTab`,
     `SplitValidTab`, `SplitKeyTab` = the lane's bucket, `SplitOkTab` =
     the fragments cover the lane) over the buckets the output range
     crosses; per button rep the lowering takes the entries and arity
     that rep's pieces reach (`specialize_frame`'s `tabs`) and resolves
     the fork into select chains on `Lo(v)` built with exactly those, so
     the premise checks what the configurations enumerate and a lane the
     analysis got wrong declines rather than vanishing
     (`a_relative_table_fork_partitions_every_lane_it_admits`).
   `Lo`/`Hi` of a value known only as a hull over lanes are the whole
   hull, not its ends (`the_ends_of_a_hull_are_not_points`): the point
   reading let the interval fold pin `Lo(v)` and fold every premise on it.
5. **Routing**: a body's dash constants are constants; its rows'
   buckets and dash class are read per ROW off the output (the key roots
   and `dash_time`: `AsmBody::dkey_of`). The sink's queues are keyed by
   (shape, cell, key), so a piece is runs of one key, and the registry
   runs each run through its key's kernel as a contiguous range
   (`Registry::run_chunk`). The row key hashes a speed with a key
   override per lane as an INTERVAL (`Span(k, k)`) and never folds it
   into the uniform part: the boundary keys it on `widen_to`'s
   `AV::Ival` bucket, and `av_code` distinguishes a number from a point
   interval. Exact-speed levels have key `None` everywhere and are
   byte-identical to before (the pinned gates).

## Tools (the dev loop)

- `transpile --key-census N FILE`: the key fixpoint on its own (~100 s),
  its census (per class, the dash sets, the buckets per axis, the x-y
  grid), the node set written to FILE; lowers the first N as a
  microbenchmark.
- `transpile --key-probe FILE i,j,..`: re-trace and lower the nodes at
  those lines (~1 s + 1-3 s per node) with a body breakdown next to the
  exact-speed kernel of the same shape; `CELESTE_BUILD_TRACE=1` adds the
  per-outcome fork arities and the false `ok` conjuncts.

## State (2026-09-15, evening)

- The key fixpoint: 593 nodes for room (1,0) (515 not dashing, 76
  dashing over the 8 real dash sets), 1,028 traces of which 435
  re-traces, ~100 s (tracing 69 s, reading successors 32 s).
- Kernel sizes (probe, player shape; exact-speed kernel: 208 bodies,
  11,844 fused nodes): at rest 52 bodies / 1,938 nodes; mid-dash 20 /
  1,789; interval keys 624 / ~18,700; the worst probed (x in [-1,
  -0.6], y just below 0, x arity 11 through the singletons near 0)
  2,344 / 32,007. The select chains are the node growth.
- The synthetic gate with the relative forks: OPTIMAL 33, 6m25s (the
  build ~6 min), but exact-level marks 237 where 262 is required.
  `CELESTE_KERNEL_KEY_CHECK=1` (the append step's row key against
  `Rt2::boundary`) fired, and three bugs were behind it, all in how a
  speed's bucket key reached the row key - the earlier 247 had the same
  ones:
  - a constant singleton bucket was hashed as a number (fixed in 5);
  - `State::key_override` was matched to its output field by NODE, and
    at rest the speed is the same hash-consed 0 as other fields: the key
    went to another field. Now by path;
  - a shape's representative is an output state of an earlier trace and
    kept that trace's overrides, whose key nodes name its forks. Now
    cleared per frame, and a field with two overrides is refused
    (`a_speed_key_names_the_speed_fields`).
  The check itself was stale: it demanded no duplicate keys in a queue,
  but a queue takes rows from several kernel calls (the call's dedup
  cache is per call, and a bucketed chunk is one call per speed key)
  and the flush collapses equal keys - it fired on the exact-speed
  forward that reproduces every pinned gate (width 56 vs 57). It now
  compares the distinct keys, and on a mismatch prints the rows and the
  cells that tell them apart.
- With the keys right the gate marked 226 at the exact level. The
  17-level exact-speed ladder marks 262 (the target holds for any sound
  ladder), and `rewrite marks-diff` against it at h30 named 4 level-1
  states whose coarse form was IN the bucketed level-0 tree but
  unmarked: a backward loss. `AsmKernel::run` cut slices every 16 lanes
  from the run's start, and a slice's predecessors are recorded as (its
  64-lane group's first id, a bit per lane) - a speed-key run starting
  at lane 60 recorded lanes 64..75 against group 0. Slices now end at
  the group boundary too, and `run_slice` asserts it (it was a
  `debug_assert!`, off under quick and release).
- **The synthetic gate passes** (quick, key check on): exact-level marks
  262 / `7d25c8eac8fc818b`, OPTIMAL 33; level 1 equals the exact-speed
  ladder's at every horizon checked (h30 88, h31 181, h33 2418, same
  fingerprints); level 0 at h33 marks 2,865 (exact-speed level 0:
  2,853). The pinned default gates (ckhash / posgraph / marks) are
  identical. Wall time 6m31s, of which ~6 min is building the 593 key
  kernels; the exact-speed 17-level run of the same search is 12 s.

## The build (2026-09-15, evening)

`transpile --key-build` builds the level-0 bucketed set as the search
does, with a per-phase account (`lower::build_profile`, summed over
workers). Quick profile, room (1,0):

- **Before:** 356 s wall, 119 CPU-min. 7,292 CPU-s of it was the global
  BDD pass in "decide" (`simplify_until_stable`, 4 passes): on every
  interval-key kernel and the exact-speed kernel each pass filled the
  2^22-node table on the first formulas and left 5,500-8,300
  `And`/`Or`/`Not` per kernel unanalysed. It changed no bodies; pass 1
  found almost everything, ~60-75% of it the tracer's re-merge with
  syntactic complements.
- **The local pass** (`bdd::simplify_local`) replaced it everywhere: per
  node, a BDD of at most 4,096 nodes over its cone expanded to 8
  compounds (every expansion from 4 to 32 finds the same; 64+ overflows
  and finds less), once, in topological order over the rewritten graph
  (a second pass finds nothing). Same three rewrites and safety rules,
  plus comparisons with swapped operands as one variable and duplicate
  leaves dropped (exact in Kleene).
- **The census** (`bdd::census`, printed by `--key-probe`) then found the
  relative table fork's chains repeating a pick entry after entry (6,088
  selects of a 28,552-node kernel): `table_chain` skips an entry whose
  pick equals the one below it. What the census still shows is small:
  no complementary pairs, no same-condition or identical-arm selects,
  ~13-64 conjuncts a full common factor would save, ~1-4 same-direction
  comparisons.
- **After:** 123 s wall, 6 CPU-min; lowering all 593 kernels ~5 s of
  wall. Fused nodes on six probed kernels 84,923 -> 62,882 (exact-speed
  player shape 11,844 -> 7,569), bodies unchanged. The pinned gates are
  identical and the synthetic gate marks 262.
- **What is left:** the key fixpoint, ~106 s single-threaded (tracing
  70 s, reading successors 36 s - growing with the shared arena, 19 ->
  45 ms per node - and 435 of 1,028 traces re-traces); the whole-arena
  copy per `Reference` (57-102 CPU-s, no reader); the table fork's
  chains, ~40% of an interval-key kernel (a lookup op - one index, a
  gather per bound - would replace the select chains).

Open: the non-dash keys are the full bucket rectangle (the successor
reader multiplies x buckets by y buckets); ~40% of traces are
re-traces; the per-kernel "decide" costs ~1 s even for small kernels;
the h99 time against the exact-speed 2:56. Then the default gates
(ckhash / posgraph / marks identical), the suite, BENCHMARK_DATA.md,
commit.
