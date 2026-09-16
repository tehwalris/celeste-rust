# The speed abstraction: which table, whether it merges, when to refine (2026-09-16)

The bucket dispatch (plans/bucket-dispatch.md) made speed-bucketed kernels
correct and cheap to build, but the table (the cart's thresholds plus a
1 px grid) and the ladder (bucket level 0, exact speed above it) were taken
for implementation convenience, not measured. Philippe (2026-09-16): settle
the questions properly. Three questions:

1. **Which boundaries.** The thresholds are REQUIRED - they are what makes
   every comparison on the speed decidable per bucket (plans/bucket-dispatch.md).
   The grid is optional: it only sets the merge granularity (and the key
   count). Candidates: thresholds only (`s20`: a 16 px grid, which on
   +-16 px speeds adds nothing), + 4 px (`s18`), + 2 px (`s17`), + 1 px
   (`s16`, today).
2. **Realized against post-hoc merge.** A bucketed search explores an
   over-approximation, and its rows re-emit when a key's speed hull grows
   (`door::Hull`). How much of the merge a table promises survives.
3. **The interleaving.** When to refine speed relative to rem: per level,
   first-win frame (the lower bound), marks at the ceiling (what the finer
   levels explore) and rows (the cost).

## Tools

- `rewrite spd-census --level-dir D --frame F --widths 20,18,17,16 --edge-table`:
  the post-hoc CEILING - distinct states of an exact tree's frame under each
  table (every other field exact).
- `rewrite partition-census --level-dir D [--spd-w W]`: per frame rows and
  distinct states (rows per state = the hull re-emission rate), files and
  dispatch blocks.
- `rewrite forward --to N --level SPEC`: a capped realized forward at any
  level; the `[fwd]` line counts `hull growths`.
- `tools/ladder_model.py`: per level and horizon, first win, marks, rows.

## 1. The ceiling per table (post-hoc on exact level-0 trees)

| | exact states | thresholds only / + 4 px | + 2 px | + 1 px |
|---|---|---|---|---|
| (1,0) f30 | 11.8k | 1.12x | 1.11x | 1.09x |
| (1,0) f50 | 1.19M | 1.81x | 1.80x | 1.61x |
| (1,0) f70 | 5.21M | 2.05x | 2.00x | 1.77x |
| (1,0) f89 | 4.67M | 1.91x | 1.86x | 1.66x |
| (1,0) f99 | 6.77M | 2.13x | 2.09x | 1.81x |
| (2,0) f45 | 1.49M | 1.55x | 1.54x | 1.40x |
| (2,0) f60 | 22.9M | 2.62x | 2.55x | 2.24x |
| (2,0) f69 | 55.3M | **4.62x** | 4.47x | 3.87x |

- The grid adds nothing past the thresholds: a 4 px grid merges exactly
  what thresholds alone merge, 2 px barely less, 1 px noticeably less.
- The thresholds cost most of the uniform-grid census's promise (1 px
  without thresholds: 4.5x on (1,0) f89, 19x on (2,0) f69, BENCHMARK_DATA.md):
  the singleton buckets at the cart's equality tests (0, +-0.05, +-0.15,
  +-0.4, +-0.6) keep the common speeds apart.
- On room (2,0) the ceiling grows with the frontier (1.6x -> 4.6x over 24
  frames): the spring's fractional speeds are what explode there, and speed
  IS the lever in principle.

## 2. Realized (room (1,0), level 0, `s16`, the h99 bucketed tree to f50)

| | exact states | bucketed states | bucketed rows | ceiling (1 px) |
|---|---|---|---|---|
| f30 | 11.8k | 12.0k | 13.3k (1.11 per state) | 10.8k |
| f40 | 229k | 216k | 268k (1.24) | - |
| f50 | 1.19M | 1.21M | 1.73M (1.42) | 739k |

The realized search merges NOTHING on room (1,0): its distinct states track
the exact count, against a ceiling of 1.6x, and hull growth re-emits 42% more
rows by f50. Running: realized forwards at `s20` / `s18` / `s16` with the
hull-growth counter.

### Realized per table (room (1,0), level 0, f0-f50, release, 32 threads, after both fixes below)

| f50 | exact | `s20` thresholds | `s18` + 4 px | `s16` + 1 px |
|---|---|---|---|---|
| distinct states | 1.19M | 1.08M (1.11x) | 1.08M (1.11x) | 1.21M (0.98x) |
| rows kept (per state) | 1.19M (1.00) | 1.63M (1.51) | 1.62M (1.51) | 1.73M (1.42) |
| hull growths | - | 1.15M | 1.15M | 1.18M |
| raw rows through the kernels | 5.5M | 52.4M | 53.9M | 44.8M |
| f50 frame | 0.67 s | 24.0 s | 24.5 s | 19.7 s |
| forward f0-f50 | 6 s | 3:26 | 3:27 | 3:07 |
| key fixpoint | - | 344 nodes, 42 s | 348 nodes, 43 s | 593 nodes, 67 s |
| post-hoc ceiling (states) | - | 1.81x | 1.80x | 1.61x |

**Room (1,0): a bucketed level 0 is a loss at every table.** The best merge
realized is 1.11x in states (the ceiling promises 1.8x), and hull growth
re-emits half as many rows again, so the frontier is LARGER than exact
speed's. The kernels also run ~9x more raw rows, and a frame costs 30-36x.
The grid only changes the key count: `s20` and `s18` produce identical
states.

Where the cost goes, per input lane (f40-f50):

| | successor rows per lane (raw / in) | kept / raw | kernel ms per M lanes |
|---|---|---|---|
| exact | 4.5-5.2 | 22-29% | 620-760 |
| `s20` | 26-32 | 3-6% | 14,800-15,500 |
| `s16` | 22-27 | 4-5% | 9,900-11,800 |

A lane holding a speed HULL enumerates the successors of every speed in it:
the move fork's floors across the hull and the table fork's buckets across
the output. So it emits ~6x the rows, and nearly all of them are revisits
the door drops, or re-emissions when they widen a hull. The merge does not
pay for that on room (1,0).

### Room (2,0): the key fixpoint does not build (2026-09-16, release)

| | exact | `s20` | `s16` |
|---|---|---|---|
| f50 kept | 5.51M, 5.2 s | - | - |
| f55 kept | 11.78M, 12.6 s | - | - |
| f60 kept | 23.22M, 28.5 s, 13.5 GB peak | - | - |
| build | 3 s | key fixpoint: a trace for key (18, 1) grew past the 2M-node graph limit after 23 min | the same for key (18, 15) after 32 min |

Post-hoc, a uniform speed grid on the exact tree (every other field
exact):

| | 1/4096 px | 1/1024 | 1/256 | 1/64 | 1/16 | 1/4 | 1 px |
|---|---|---|---|---|---|---|---|
| f55 (11.78M) | 1.15x | 1.15x | 1.15x | 1.16x | 1.24x | 1.83x | 5.73x |
| f60 (23.22M) | 1.15x | 1.15x | 1.16x | 1.20x | 1.36x | 2.19x | 7.83x |

A fine speed precision merges nothing. The spring's speeds are spread
across the pixel, not clustered in sub-pixel noise, and only a 1 px grid
without thresholds promises much. The realized 1 px bucket did not shrink
the frontier on this room (BENCHMARK_DATA.md, 2026-09-14). So no speed
abstraction is the lever for room (2,0).

The position rung realized (`y2r0sx`: 2 px y buckets, forked into points
in-frame) is a loss on room (2,0) as well:

| | f50 kept, frame | f53 kept, frame |
|---|---|---|
| exact `r0sx` | 5.51M, 5.2 s | 8.83M, 8.8 s |
| `y2r0sx` | 6.31M, 17.3 s | 10.23M, 28.1 s |

The post-hoc census promised 1.56x fewer states. Realized, the rung keeps
16% more states at 3x the cost per frame: the boundary snap spreads
straddling rows into two buckets, and each bucket runs both points. Stopped
at f53, and `x2y2` was not run.

Exact speed grows ~14.5% per frame at f60. That projects to billions of
states by f95, so exact-speed level 0 is not viable for room (2,0). The
bucket dispatch cannot even build its kernels there: under the spring a
coarse speed bucket leaves the move loop's collision tests undecided and
the trace explodes.

### Room (2,0) end to end: where it stands (2026-09-16, night)

Every lever measured tonight fails to make room (2,0)'s level 0 reach f95:

- **Exact speed:** 23.2M states at f60, 14.5% growth per frame, which
  projects to ~2G at f94. The 2026-09-15 overnight run was killed by the
  90 GB memory cap at f79.
- **Speed buckets (dispatch):** the kernels do not build (the key fixpoint
  exceeds the graph limit). Fixed the next day by bucketing x only and
  tracing each key on its whole bucket: see "Room (2,0) at `s20x`" below.
- **Fine speed precision:** post-hoc it merges nothing (1.2x at 1/64 px).
- **Position rung `y2`:** realized it keeps 16% more states at 3x the cost.

One correction came out of it. The exact-speed trees before `180834b` were
produced with the fruit's hit test silently dropped (the kernel took the
"no hit" arm on undecided lanes). At f55 the kept set is identical, but
the old f69/f78 frontier numbers are not authoritative.

What could still work, none of it built yet:

1. A saturating coarse level whose marks prune: under `--ceiling` only
   horizons 95 and 94 run, so a coarse level's early first win costs
   nothing. But every widening measured here fans out in-frame.
2. An admissible lower bound on frames to exit, stronger than
   position-graph distance (which prunes nothing at f69).
3. The door and the frontier on disk. That is days of work and hundreds
   of GB at f94.

So the night went to the next rooms instead: (3,0), (4,0) and onward.

### Room (2,0) at `s20x`: the kernels build (2026-09-16, day)

The level buckets speed on x only (`s20x`: the thresholds, no grid inside
±16 px) and keeps y exact. Each key is traced once, on its whole bucket.
Keys and kernels are per bucket, never per hull.

| key fixpoint | keys | wall |
|---|---|---|
| serial (`e305cd9`) | 430 | 97 s |
| parallel rounds, 32 workers | 430 | 7.8 s |
| + the spring's cuts | 698 | 12.4 s |
| + every dash diagonal | 746 | 13.3 s |

Discovery runs in rounds. Every worker traces in its own copy of the walk's
tracer, reads the successors there, and binds the trace out of its arena;
the coordinator adds each new (shape, key) once. A trace costs about twice
as much under 32 workers (summed tracing 173 s against 84 s serially). The
shape lattice before it is still serial (8.4 s, 18 shapes): its
representatives live in the tracer's arena, so a worker's copy cannot hand
new ones back.

The forward found two bugs:

1. **The spring's multiply** (f40, a failed premise). The spring updates
   before the player and writes `spd.x *= 0.2`, so every threshold of the
   player's update is also compared against 0.2 times the keyed speed.
   Bucket [1, 0.05) raw held 1..5, which floor to 0 after the multiply, and
   6 and up, which do not: `spd.x > 0` was undecided and the kernel
   declined. The x table now also cuts at each threshold edge's preimage
   under the multiply (`spd_buckets::cut_edges`; test
   `every_x_threshold_is_decided_per_bucket_after_the_spring_too`).
2. **One dash diagonal** (f37, no kernel for the key). `successor_keys`
   skipped a button configuration whose seven dispatch fields had the same
   node ids as an earlier one, but specialized each configuration into its
   own graph, where symmetric configurations build different constants at
   the same ids. Of the four diagonal dash starts only the first was read,
   and a down-left dash after the spring hit had no kernel. The
   configurations now specialize into one hash-consed graph, as the
   lowering's already did (test `a_dash_start_is_read_in_every_direction`).

Found with `transpile --shape-diff A B S` (where two shapes differ: here
the spring's `delay` field, created by the hit) and the `[keysucc]` edges
`CELESTE_KEY_TRACE` prints.

With both fixed the kernels build (746 keys: lowering done at 28.5 s wall,
assembly 13.0 s more) and the forward runs past f37 with no gap. But it
keeps MORE states than exact speed (release, 32 threads):

| frame | exact kept | exact raw | `s20x` kept | `s20x` raw | `s20x` / exact |
|---|---|---|---|---|---|
| f32 | 14,028 | 21,451 | 16,078 | 113,140 | 1.15 |
| f38 | 142,671 | 352,374 | 179,111 | 1,842,292 | 1.26 |
| f44 | 1,102,245 | 3,521,944 | 1,490,668 | 18,468,246 | 1.35 |

f44 took 25 s against 0.9 s exact. Stopped at f45.

Post-hoc on the exact tree, the most each table could merge
(`spd-census --edge-table [--x-only]`):

| | f44 | f50 | f55 |
|---|---|---|---|
| x only, thresholds (`s20x`) | 1.16x | 1.22x | 1.30x |
| both axes, thresholds (`s20`) | 1.39x | 1.60x | 1.87x |
| uniform 1 px, both axes, no thresholds | - | 3.68x | 5.73x |

At f44 `s20x`'s 1,490,668 rows project onto 1,308,217 distinct states (the
x bucket, every other field exact); exact's onto 947,936. So:

- 182k rows (12%) repeat a projection: a key held under two hulls, the old
  row and the grown union.
- At least 360k projections are ones exact never reaches. Every field has
  the same distinct count in both trees except the spd.x hulls (472 against
  137 values), so these are new combinations of existing values. Erasing
  one field at a time (`spd-census --erase`) leaves the ratio between 1.29x
  and 1.40x: no single field carries them.

The likely mechanism: a hull is the convex union of the speeds merged into
it, and on the wide buckets ([-5, -3], [3.0001, 5.0002]) it spans speeds that
move to different pixels. The kernel emits every position the hull allows,
and the extra positions compound over frames. And even an exact realization
of the x-only table merges at most 1.3x by f55: the large merges need y
bucketed too and no singleton thresholds.

### The next rooms (2026-09-16, night)

| room | objects | ceiling (TAS replayed, then followed in our game) | result |
|---|---|---|---|
| (3,0) | fly fruit, 12 fall floors | 89 (TAS4; the witness search stops when the route collects the fruit: the bridge has no integer-keyed table part) | kernels do not build. With `CELESTE_MAX_TRACE_NODES=16000000` and `CELESTE_LATTICE_TRACE=1`, every successful trace adds 273–10,744 nodes. The player shape's trace exceeds 16M nodes (refused at the raised budget, then re-traced until the 50-minute timeout) in the move loop's per-step `is_solid` → `check(fall_floor)` → `collide` over the room's 12 fall floors. That is runaway growth in one trace, not a budget set slightly too low |
| (4,0) | key, chest | 76 (TAS5, replays on a real PICO-8) | **OPTIMAL 76** (3:54), after pinning the key's `spr`/`flip.x` with `frames` |
| (5,0) | balloon | 77 in the original cart; our witness does NOT exit on a real PICO-8 | open question, below |
| (1,1) | 4 fall floors | 94 (TAS10 with 25 prologue frames, replays on a real PICO-8) | killed by the 60 GB memory cap at f56 after 44:56. Level 0 grows ×1.4–1.5 per frame f41–f55 (131.9M kept at f55, 699 s/frame, 56 GB peak). The largest shape at f54 (55.9M rows): player `x` 94 values × `y` 72 × 2,022 speed pairs, `fall_floor[0].delay` 19 and `[1].delay` 16 (when each floor was touched), `dash_effect_time` 11 |
| (6,0) | fly fruit, 10 platforms | 72 (TAS7, replays on a real PICO-8) | level 0 grows ×1.6 per frame f35–f42 (16.8M kept at f42, 33 s/frame, 12 GB); stopped at f43. The main shape has 52 varying columns; ten object columns, 10 cells apart, each hold 7 distinct values within one frame, so they depend on the player, not the clock. `col-census` now names every object's fields: those are the platforms' `rem.x` (7 values each; `x`/`last` 5), and the fly fruit's `spd.y` 46, `rem.y` 44, `step` 18, `y` 15. The fly fruit starts flying when the player first dashes (`has_dashed`), so its flight phase records when each state first dashed; it cannot be pinned away (collecting it refills the dash) |

**Rooms (1,2) and (3,2): no ceiling.** Their maps hold tile 1 several times, so
three `player_spawn` objects appear at load. Replayed in the original cart
with TAS18 / TAS20 at prologue offsets 22-31, several players spawn, die and
the room restarts in a loop, and no replay leaves the room. These rooms are
not the TAS database's levels 18 and 20 as `load_room` builds them. Not
pursued.

**A per-room pin that is real but not enough.** `dash_effect_time` has one
read in the cart: `fake_wall.update`'s `hit.dash_effect_time > 0`. Fake walls
come only from the map at room load, so in a room without one the field is
gameplay-dead. It counts down from 10 after a dash and holds 11 values in room
(1,1)'s largest shape at f54. **Measured, it merges almost nothing:** `rewrite
spd-census --erase dash_effect_time` counts room (2,0) f60's distinct states
at 23,198,480 against 23,220,146 exact (0.09%), and room (4,0) f76's at
7,949,192 against 7,977,860 (0.36%). The countdown is implied by the rest of
the state (speed, position, `dash_time`), so erasing it rarely makes two
states equal: the `has_dashed` lesson again. Not built.

**The balloon's phase (room (5,0), for Philippe).** `balloon.init` draws
`offset = rnd(1)`, which fixes its bob, and PICO-8 seeds `rnd` itself. The
search models the draw as the whole interval [0, 1), so it explores every
phase, and its optimum is the fastest route over the most favourable phase:
a lower bound for the real game, which may need a different phase. The
trajectory witness that follows TAS6's positions in our model does not exit
on a real PICO-8. The route relies on a phase this replay's seed did not
produce. Options: report balloon rooms as optimal over all phases (sound as
a lower bound, not always achievable); pin the phase to what the real game
draws at room load, if the seed at load is deterministic; or fork the phase
into buckets and report per phase. Not decided tonight; room (5,0) waits.

### Two bugs the coarse tables exposed (2026-09-16)

- **The dash-constant reader** split on every undecided select in the dash
  fields' cone, innermost first. Under `s20` that reached the facing update's
  physics (collision tests, the speed clamp) and ran past 4096 cases. It now
  walks the fields' VALUE paths outermost first and splits a numeric
  comparison over a select on that select's condition (`cond_source`), so
  `spd.x ~= 0` over `flip and -1 or 1` splits on the flip and folds, and
  any other condition is opaque. The 1 px node set is byte-identical (593
  nodes, 8 dash sets); `s20` builds 333 nodes and `s18` 337, each with the 8
  real dash sets. (Taking conditions opaque without `cond_source` brought
  back 12 sets: target (2, 0) with accel (1.5, 1.5).)
- **The table put -1 on the wrong side.** `abs(spd.x) > 1` is `spd.x < -1`
  below zero (edge AT -1), while `appr`'s `val > -1` needs the edge above
  -1. With `Above` alone, -1 shared a bucket with (-1.5, -1). The 1 px grid's
  edge at -1 hid it. At `s20` and `s18`, a hull reaching -1 left the clamp
  undecided, and the merged arms crossed more buckets than the relative
  fork's arity (computed per static piece). The kernel declined the lane:
  `KERNEL COVERAGE GAP` at room (1,0) f33, the premise refusing and no row
  lost. -1 is now a point bucket at every width.

## 3. The interleaving

Not measured yet. To follow once a table is chosen: explicit ladders
(`CELESTE_LADDER`) through `tools/ladder_model.py`, first on the synthetic
room (1,0) target, then as a real `--ceiling` run on room (2,0).
