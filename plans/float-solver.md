# The float solver: the game's arithmetic as a setting (design, 2026-10-15)

Branch `float-dual` (from develop `7e814b41`). This replaces the first
brief's plan (a float reference engine and a dual concrete search only):
Philippe asked for the whole pipeline parameterized by the arithmetic,
with abstraction and refinement of the numeric fields at its core. The
dual (both-valid) search is one piece of it (section 10). Sections 0-14
are the design as proposed ("measured" marks what I ran then); section 15
records what was decided and what milestones 1 and 2 built and measured.

## 0. The goal

Today every engine models PICO-8's 16.16 fixed point. The community's
tools run the cart in Lua doubles. Some of our optimal runs die there
(gemskip 100% 2500m, room (0,3): a ~0.0001 drift from frame 21 flips a
floor at frame 63). Some community runs live only there (gemskip any%
2600m, 123f). The target:

- `--arith fixed|float`, a RECORDED setting of a search and its tree
  (`settings.txt`). Trees and engines of two arithmetics are never mixed.
  Fixed stays the default and stays byte-identical, including every pin
  and gate.
- The float pipeline is the fixed one's counterpart, piece by piece: the
  oracle, the tracer, the graph, the lowering, the assembled kernels, the
  keys, the forward, the arcs, level -1, the concrete search. Each piece
  has the same kind of exact check as its fixed counterpart.
- On top of it:
  - float optima (can UCT's 123 for 2600m be proven optimal in float?);
  - the DUAL search: the best run valid in both arithmetics, pruned by
    both arithmetics' winning sets.

## 1. What the database checks, and which floats

**Nobody runs an emulator automatically.** The tasdatabase bot
(CelesteClassic/celestebot, `cogs/tas.py`) works like this:

- It takes `!uploadtas`, queues the file for a member with the "TAS
  Verifier" role (`!verifytas`), counts `len(inputs) - 1`
  (`updateAndCommit`) and commits.
- A commented-out `run()` once started UniversalClassicTas (UCT); nothing
  calls it.
- The database's help page tells submitters to clean the file with UCT's
  U.

So the check is a person with a tool. The tools are UCT, named by the
database, and Celia (gonengazit/Celia), the community's current tool. Both
run on the same LÖVE 11.5 AppImage with LuaJIT 2.1.1700008891 on x86-64.
Both compute in Lua doubles; Celia's `--fixp` is opt-in.

**One float arithmetic, two tools.** Read from UCT's and Celia's `api.lua`,
their cart loaders, and LuaJIT's sources (`lj_opt_fold.c`,
`lj_asm_x86.h`, `vm_x64.dasc`, `lj_opt_narrow.c`, `lj_strscan.c`, v2.1).

| what | UCT | Celia (float) | the float engine computes |
|---|---|---|---|
| `+ - * /`, unary `-` | LuaJIT SSE2 scalar doubles, round to nearest even | same | IEEE f64, the same op tree, in Lua's evaluation order |
| JIT folding | FP folds are IEEE-exact by design (see below) | same | no folding except bit-identical folds |
| FMA | none: `lj_asm_x86.h` emits no FMA | same | never (we write the assembly by hand, section 5) |
| `%` | `a - floor(a/b)*b` (`vm_mod`; the JIT emits the same IR) | same | the same three ops |
| `flr` / `abs` | `math.floor` (exact) / `math.abs` | same | `floor` / `fabs` |
| `min`, `max` | `if a<b then a else b` | `a<b and a or b` | the same (identical, including which zero is returned) |
| `sin(x)` | `-math.sin((x*math.pi)*2)`, `math.sin` calling the host libm's `sin` | same | the same expression, calling glibc's `sin` |
| literals | `lj_strscan`, correctly rounded | same | `str::parse::<f64>`, correctly rounded |
| `a op= e` | `a = a op (e)` (longest parsable `e`) | same (`FormatPico8`) | as written in the traced cart, which uses the same form |
| `rnd` | `math.random` (LuaJIT's PRNG) | `love.math.random` | only the SEEDED uses matter (below) |
| start | IL load: `load_room`, `set_seeds`; a platform's `rem.x` set to `dir*0.65*0` (`-0` for `dir = -1`) | loading jank from the previous room's object count; then the seeds | per tool: a start setting (section 3) |
| seeds | balloon `offset = seed`; the chest's last frame `rnd -> offset + 1` | balloon `offset = 0 - 0 + seed` after the jank, `y` recomputed; chest `rnd -> seed + 1 + 0x0.0001`, seed in {-1, 0, 1} | per tool |

The JIT's FP folds, for the record:

- `(-a)+b -> b-a`, `a+(-b) -> a-b`, `x-(+0) -> x`;
- `x*1 -> x`, `x*-1 -> -x`, `x*2 -> x+x`;
- `x/2^k -> x*2^-k`, only when the reciprocal is exact;
- `(-a)*k -> a*(-k)`;
- constants folded with the same ops.

Every one is bit-exact. The file's comment lists the folds it refuses
(`x+0`, `0-x`, `x*0`). So the interpreter and the JIT agree on every
value.

UCT's cart Lua is byte-identical to `celeste_ocaml/celeste.lua` (but for
a trailing newline). Celia's cart adds a screenshake toggle, which only
draws and consumes `rnd`.

**What "bit-exact" can mean.** Every op above is basic IEEE except `sin`,
which is the host's libm: the AppImage links the system `libm.so.6`
(glibc 2.42 here, with its ifunc-selected `sin`). We can match THIS
machine's glibc bit for bit by calling the same symbol. A verifier on
Windows (UCRT) or macOS may differ in the last bit of `sin` on rare
arguments. Section 11 proposes a witness check against a correctly
rounded `sin` and ±1 ulp; open question Q3.

## 2. The traced program against the original cart, in float

The traced program is `lua/celeste-minimal.lua` plus the builtins. I
audited every hunk of its diff against the original cart.

**The same in float:**

- Every compound assignment: the minimal cart writes `x = x - (1)`,
  exactly UCT's and Celia's patch.
- Every untouched line, because the builtins match (section 1).

**DIFFERENT in float (three places):**

1. `move`.
   - Minimal: `rem = rem + (ox) + 0.5; rem = __split_by_flr(rem); amount =
     flr(rem); rem = rem - 0.5 - (amount)`.
   - Original: `rem = rem + (ox); amount = flr(rem + 0.5); rem = rem -
     (amount)`.
   - Equal in 16.16. In doubles the remainder differs in 43,039 of 200,000
     random (rem, ox) pairs (measured: Python, IEEE doubles).
   - The `amount` agreed on all of them, but can differ: the cut point
     below 1 is `0.5 - 2^-54`, not `0.5`. `0.49999999999999994 + 0.5`
     rounds to 1.0 (a tie, to even).
2. `_update` moves an object only `if obj.spd.x ~= 0 or obj.spd.y ~= 0`;
   the original always calls `move`. Exact in 16.16. In doubles the
   original's `move(0, 0)` does two things the guard skips:
   - it maps `rem = -0` to `+0`;
   - from `rem = 0.5 - 2^-54` it steps a pixel (`amount` 1, `rem` -0.5).
3. The loading-frame patch (`cart::apply_loading_frame`) has the same
   guard. Celia's jank moves every object unconditionally.

**Not arithmetic (as in fixed):**

- No `jbuffer`: a jump press that cannot jump is dropped instead of
  buffered for 4 frames. This is input timing, independent of the
  arithmetic. Trajectories coincide modulo re-timing presses, which is
  why witnesses are canonicalized.
- No smoke, lifeup, room_title, message or flag objects. They change the
  object list (`J` is translated by `minimal_index`); nothing in gameplay
  reads them.
- No draw-only `rnd` calls. Gameplay `rnd` is seeded in both tools.
- No draw code but the player's clamp.

**Found on the way (both arithmetics):** the native `tile_flag_at`
truncates `(x+w-1)/8` toward zero. For `x+w-1` in [-7, -1] it scans
column 0, where Lua's loop limit is negative and it scans nothing.
Probably unreachable (the player's `x >= -1`), but a silent disagreement
with the cart. Q8.

**Decided (Q4): (b), one form for both** (section 15: the cart form A2,
and why the speed guard stays). The options as proposed:

- (a) **Recommended:** a source patch applied for the float arithmetic
  only, the same mechanism as `--nodiag`'s (exactly-once replacements).
  It does three things:
  - `move` in the original's form, with a tracer hint
    `__split_by_round(v)` (fragments of `v` on which `flr(v + 0.5)` is
    constant, cut points exact in the arithmetic);
  - no speed guard in `_update`;
  - no speed guard in the loading frame.

  Fixed sources, kernels and metrics stay untouched.
- (b) One form for both. Fixed values are unchanged (16.16 addition is
  exact), so states, edges and W would not move, but kernel metrics
  would, which means a re-pin.

## 3. The float oracle (milestone 1)

- **`celeste_core::lua_f64::LuaF64`**, an f64 newtype with section 1's
  semantics.
  - Ops are `checked`: a non-finite result RAISES. This is the float form
    of "match exactly or raise"; Celeste's values never get near
    overflow.
  - `sin` calls glibc's `sin` through FFI, so Rust cannot constant-fold
    it.
  - Equality and dedup are by bits; order is IEEE's.
- **The interpreter, generic over the arithmetic** (section 5):
  - literals are parsed by the arithmetic;
  - the builtins (`flr`, `abs`, `min`, `max`, `sin`, `%`) and `rnd`'s range
    `[0, pred(x)]` dispatch to it;
  - `for` counters and table indices require integral values.

  `Concrete<A>` and `RefDomain<A>` instantiate it. `RefEngine<Float>` steps
  the patched cart, one lane and one fork path at a time, as the fixed
  one does.
- **The start per tool:** the UCT IL load with UCT's seeds, or Celia's
  jank with Celia's seeds. This extends the existing
  `--loading-jank`/`--balloon-seeds` mechanism.
  - A seed is injected as Rust's shortest round-trip text of the f64, so
    the float engine reads back exactly UCT's `tonumber`. Fixed keeps its
    truncation.
  - Celia's chest `+ 0x0.0001` and UCT's `-0` platform remainder are
    reproduced, not normalized.

**Differential tests against the real UCT**, as assertions:

- `tools/uct/golden.py` (a regen script, like `regen-pico8-golden.sh`)
  runs UCT headlessly with an EXACT trace mode (`tools/uct/exact.lua`):
  per cart update, every object's fields as hex floats.
  - It stores per input file a golden (text, compressed, not JSON).
  - Inputs:
    - community files of every category, canonicalized for `jbuffer`
      (the trace logs the jump sounds; a buffered jump's press is re-timed
      to its frame, and the trajectory is checked unchanged);
    - our witnesses;
    - random sequences that only press jump where a jump fires at once.
      UCT then never buffers, so the two carts run the same program.
- The nextest test replays each golden's inputs in `RefEngine<Float>` and
  asserts every field equal, BIT for bit, every frame. Rooms with
  balloons, fruit, fly fruit, springs and platforms are included, which
  covers `sin` and the seeds. It is under 2 minutes by design: about 20k
  reference frames.
- Celia's float mode gets the same goldens for a subset (the jank start,
  its seeds).
- Real PICO-8 stays the fixed side's authority (`regen-pico8-golden.sh`).

## 4. Intervals in float: sound without outward rounding

The kernels, the lowering's fold, `pieces_of` and level -1 compute on
intervals. Here they bound the IEEE function, not the real one. For a
monotone op, rounding is monotone, so evaluating the endpoints with the
SAME rounding gives the exact hull:

- `x ∈ [a, b]` gives `fl(x + k) ∈ [fl(a + k), fl(b + k)]`;
- the same holds for `-`, for `*` and `/` by a constant of known sign,
  for `floor`, `abs` piecewise, and `min`/`max`;
- for interval × interval, take the four rounded corners.

So float intervals need NO outward rounding. Outward rounding would even
lose the property the lowering relies on: fold equal to per-lane
evaluation (`ival::folding_is_exact_not_merely_sound`).

Two exceptions:

- `sin` is not monotone (and glibc's is not proven monotone at the ulp
  level). Of an interval it gives `[-1, 1]`, as in fixed (`Span(-1, 1)`),
  which is sound because a faithful `sin` never exceeds 1 in magnitude.
- Signed zeros are equal under every comparison, so they do not affect
  soundness.

**Folding and rewriting.** Only bit-identical rewrites in float. Every
algebraic rewrite in `Graph::fold`, `bdd::simplify_local`,
`pieces_of`/`one_operand` and the tracer's literal folding gets audited.

- Kept: `x*1`, `-(-x)`, `x*2 -> x+x`, exact reciprocals.
- Refused: `x+0 -> x`, `x*0 -> 0`, `(x+a)-a -> x`, any reassociation.

A test drives each rewrite with random doubles and edge cases (±0,
subnormals, ties) against the unrewritten graph.

## 5. Architecture: where the arithmetic parameter lives

The inventory (two read-only sweeps) shows the 16.16 representation in
about 45 files, at four depths:

- the ops (`Pico8Num`);
- every stored number as a raw `i32`: `Op::Const(i32, i32)`,
  `Stored::Range`, `Code{a, b: u32}`, `Col::N(Vec<P8>)`, kernel lanes;
- the floor and fork grid `1 << 16` (`zi_fork_flr`, `GRID_STEP`,
  `FLR_MASK`);
- the remainder's 65536-point circle (`arcs::CIRCLE`, `Hull`'s `u16`
  arcs, `AxisXfer`).

**Recommended (Q5): a compile-time trait `celeste_core::arith::Arith`**,
implemented by `Fixed` (today's `Pico8Num`, unchanged) and `Float`
(`LuaF64`). It provides:

- the number, its interval and its raw word (`u32` / `u64`);
- literal parsing and every checked op;
- the floor split and the round split (`__split_by_round`), with their
  exact cut points;
- the order key used for arcs: a double's total-order bits as a `u64`,
  the 16.16 circle point as a `u32`.

The code becomes generic over `A: Arith`:

- `Domain`, `Interp`, `RefEngine`;
- `Graph` (constants as `A::Raw`), `ival`, `pieces_of`, level -1's
  evaluator;
- `Rt2` columns, `exact::Code`, the dictionaries, frame and hull files;
- the codegen, behind a per-arithmetic instruction selector;
- `arcs`, `arc_edges`, `hull`, `arc_dp`.

`main` dispatches once on the recorded setting:
`run::<Fixed>(..)` / `run::<Float>(..)`.

Why this choice:

- A tree, a kernel set and a winning set of different arithmetics cannot
  be combined: it does not type-check.
- Fixed monomorphizes to today's code and formats, so "byte-identical"
  holds by construction, on-disk bytes included.
- The cost is compile time (the generic part built twice) and signature
  noise. The alternative, raw `u64` everywhere with a runtime tag, has
  simpler signatures but more fixed memory, changed formats and
  run-time-only mixing checks.

**The kernels in f64.**

- Lane width: `W = 16` stays. The row, mask, dispatch and emission code
  is shared. A float kernel runs its body twice per call, on lanes 0-7
  and 8-15: one zmm per value, `u8` masks combined into the `u16` the
  emitters read.
- Instructions:
  - arithmetic: `vaddpd`, `vsubpd`, `vmulpd`, `vdivpd` (IEEE division:
    no call-out);
  - `vminpd`/`vmaxpd` with the operand order that returns `b` unless
    `a < b`, as Lua's `min` does (Intel's MINPD returns the second
    operand on equality);
  - floor: `vrndscalepd $0x09`;
  - comparisons: `vcmppd`;
  - integral checks (positions, `mget` coordinates): `vrndscalepd` plus
    an equality check.
- The kernel is hand-written GAS assembly (gcc only assembles and links;
  no C is compiled, verified). So FMA contraction is impossible as long
  as the emitter has no `vfmadd*`: an assertion over the emitted text,
  plus a unit test.
- `sin` is a call-out: per lane, glibc's `sin` on the exact argument.
- `NoWrap` becomes "finite": float has no wrap, and a non-finite result
  is the lane's own error.

## 6. The remainder and the arcs in float

**The move map.** One axis, one frame:

- `r1 = fl(r + ox)`;
- `k = floor(fl(r1 + 0.5))`;
- `r' = fl(r1 - k)`.

Facts, each to be pinned by a test (exhaustive near the cut points,
random elsewhere):

1. **The remainder stays in [-0.5, 0.5).** `r1 - k` is exact (`k` is a
   multiple of `ulp(r1)`). The one rounding case is
   `r1 = 0.5 - 2^-54`: `k = 1` and `r' = fl(-0.5 - 2^-54) = -0.5` (a tie,
   to even). Verified numerically.
2. **The cut points.** `c_k = min{x : floor(fl(x + 0.5)) >= k}` is
   `k - 0.5`, except `c_1 = 0.5 - 2^-54`. The fork primitive computes the
   candidate and checks its predecessor with the exact op. It raises if
   one step does not settle it.
3. **The transfer is monotone.** For a fixed node (exact speed) and input,
   the remainder's domain falls into order-intervals of doubles (PIECES)
   on which `k` and the collision outcome are constant. On a piece the
   transfer is `T(r) = fl(fl(r + ox) - k)`: monotone non-decreasing, not
   injective (rounding merges values), not a rotation. Or it is the
   constant 0 of a blocked step.

**What carries over.** The arcs' structure is unchanged with a different
coordinate:

- A point is a double's total-order key (`u64`).
- A set is sorted disjoint segments. A winning set W is "y slabs with x
  segments" over those keys.
- A transfer records:
  - per axis its guard, the piece: an order-interval;
  - its action: `Mono(ox, k)` (the exact expression above), or
    `Const(c)`, or identity where the axis has no split.
- The PULL: the preimage of a segment `[a, b]` under monotone `T` within
  the guard is a segment, computed EXACTLY. Take the candidate from the
  real inverse, then step to the neighbouring doubles, checking with
  `T` itself (at most a few steps, else binary search over the 2^64
  order keys). So W stays exact in the remainder, as in fixed.
- The PUSH (the hull forward, `reach`): the image of `[a, b]` is enclosed
  by `[T(a), T(b)]`. Over-approximate, as both already are: a hull is a
  widening and `reach` only loosens the ladder's filter.
- The hull keeps its two charts (cut at -0.5 and at 0) as linear orders
  over the doubles.
- A hull may be stored rounded OUTWARD to a coarser grid: a widening
  that only enlarges the tree, worth measuring.
- `arc-check` in float probes at the cut points ±1 ulp as well as at
  random points.

**The box-closure lemma (what may join the arcs).** Suppose a set of
numeric fields is widened, and on every edge each one's transfer is a
function of THAT FIELD ALONE (given the node and the piece), monotone or
constant. Then the preimage of a box (a product of order-intervals) is a
box intersected with the guard box. A finite union of boxes stays one,
and the backward is exact field by field.

- The player's `rem.x`/`rem.y` given an exact speed: yes (today's 2-D
  arcs).
- A platform's `rem.x`: yes (its own constant speed). It could leave the
  `p` widening for exact arcs (later).
- `rem.x` together with `spd.x`: NO. `rem' = fl(fl(r + s) - k)` couples
  them: the preimage of a box is the staircase `{(r, s) : fl(r + s) ∈ U}`,
  with as many steps as distinct speeds, and pulls compose staircases.
  An exact set over both is no smaller than one set per speed value,
  which is what exact speeds already are.

## 7. Abstraction and refinement of the numeric fields

Philippe expects refinement on most numeric values to be the biggest
change. The framework has three treatments, each with what refutes it
EXACTLY.

| treatment | what the state holds | what refutes it | applies to |
|---|---|---|---|
| T1 exact | the value's bits in the key | nothing needed | speeds at the exact level; integers; booleans |
| T2 exact sets (arcs) | nothing (widened at the boundary); W per node holds the exact set | the backward over exact monotone transfers (section 6) | `rem.x`, `rem.y`; candidates: platform `rem.x` |
| T3 buckets | an order-interval of doubles from a fixed grid, as an interval lane | the CONCRETE SEARCH inside W (exhaustive over exact states), and where W is loose the LADDER down to T1 | speeds at coarse levels; object phases (the existing `f`/`n`/`p` flags) |

**T3 in detail.**

- **The bucket.** A field's grid (say `spd.x` in steps of `2^-m` px a
  frame) is a fixed partition of the doubles into order-intervals.
- **The boundary.** A value or interval is replaced by the bucket(s) it
  meets, one row per bucket and never their hull, so keys stay grid
  codes.
- **The kernel.** It takes a bucket as `Restrict(lo, hi)`, the same
  mechanism as the region's speed bound.
- **Forks.** An undecided comparison forks at its exact cut point (the
  first double whose image satisfies it, by monotone inversion).
- **Transfers.** A transfer reading a T3 value becomes a RELATION. For
  the remainder: `r' ∈ {fl(fl(r + s) - k) : s ∈ S}`. Its pull is
  over-approximated by the interval `{r : g(r, s_hi) >= lo} ∩
  {r : g(r, s_lo) <= hi}`, sound because `g` is monotone in `s`.
- **W.** W at a bucketed level is a SOUND over-approximation: a lower
  bound on the optimum, and no win refutes the horizon. It is not exact.
- **What refutes it.**
  - The concrete search: real game, every input, exact states, pruned
    only by W. This is exactly how the objects' flags are refuted today.
  - Where the bound is loose, the ladder: the next level (finer buckets,
    or T1) is filtered by this level's reach marks (`frame::MarkFilter`).
    This is sound because W contains every real winner and a real
    state's projection lies in its bucket. It is the objects ladder with
    precision as the rung.
- **Termination.** A level list is finite and ends in the concrete
  search, which is exact over a finite horizon and finite inputs. The
  first version has no automatic refinement loop; the ladder is a spec,
  e.g. `--level r0sxhn:v4,r0sxhn`.
- **Cost and drift.** A bucket's image straddles grid edges, so rows
  multiply: the drift that killed the remainder rungs
  (plans/lessons.md). Speeds are ATTRACTED: `appr` clamps to its target,
  so a bucket collapses to a point within frames (gravity: at most 19
  frames from -2 to 2). Their drift is bounded where the remainder's (a
  rotation) was not. This is why the remainder stays T2.

**Philippe's interval lanes for `spd.x`** are T3's kernel mechanism.

- As an EXACT representation (a bundle of exact values in one lane,
  split after the kernel), every speed-dependent output must be derived
  per value. That includes the remainder's transfer, whose piece depends
  jointly on (rem, v): a per-value guard.
- That is the speed-bundles "template" scheme in float: derive with the
  recorded IEEE op chain, check the endpoints against the kernel.
- It is exact, but it pays only if per-value storage and emission
  dominate. The speed-bundles measurements put the kernel at 2% of a
  frame.
- As an ABSTRACTION it is T3. Which of the two (or neither) is decided
  by the census (section 12).

**The initial assignment:**

- the player's `rem`: T2;
- the player's `spd`: T1, as fixed;
- `dash_target`/`dash_accel`: T1 (a few constants);
- object phases: the existing flags, T1 at exact levels;
- platform `rem.x`: T3 at `p`.

Revisit after the census.

## 8. The level flags in float

| entry | float |
|---|---|
| held buttons (`h`) | no numbers: unchanged |
| player remainder | T2, section 6 |
| dash effect `max(v, 0)` | integers: exact |
| strawberry bob | FIXED's EXACT CANONICALIZATION OF `off` FAILS in float: `sin(off/40)` is not bit-periodic in doubles. The entry stays SOUND as an over-approximation, because the only read of `off` feeds `y`, which is the band `start ± 2.5`. `fl` is monotone and `|sin| <= 1`, so every real `y` is inside. Refuted by the concrete search, as already listed ("over") |
| timers, key sprite | dead fields: exact |
| fly fruit (`f`) | `spd.y` in [-3.5, 0.5] (`appr` clamps at -3.5; `sin(..)*0.5` in [-0.5, 0.5]); `rem.y` in [-0.5, 0.5) by section 6.1; the unknowns unchanged: sound |
| fall floor / balloon countdowns, spring phase, near floor (`n`) | integers: unchanged |
| balloon phase (`n`) | `y` in `start ± 2` holds in float (as the bob) |
| platform path and remainder (`p`) | `x` integral on the path; `rem.x` in [-0.5, 0.5) by 6.1 applied to the platform: sound |
| balloon offset | the full-period canonical `[0, 1)`: sound in float, because of the interval rule `sin(full period) = [-1, 1]`; a seeded point stays exact |
| absent delays | exact |

The table's generated doc gains a float column, and `widen::SlotErrors`
checks containment per row in float as in fixed. (`e`, the map mod's
flag, is not on develop; it gets the same review when it lands.)

## 9. Keys and storage

- **Codes are the exact raw bits** (`u64` for float), so a key is still
  dictionary indices (127 bits).
  - Fewer distinct values than bits matter: the risk is more codes per
    field, which the census measures.
  - `-0` and `+0` are different codes, which is exact; a `-0 -> +0`
    canonicalization is exact only with a proof that no read tells them
    apart, so it is deferred.
  - NaN and inf are never stored (they raise).
- **Positions are integral** in both tools, so cells are
  `floor(x)`, with integrality checked.
- **W's arena words** grow from `u16` to `u64` endpoints, with deltas
  varint-coded. The census and the first float trees size this (Q9).

## 10. The concrete searches

- **Float.** The concrete search with `RefEngine<Float>`, looking states
  up in the float W. The memo is on exact states (f64 bits).
- **Dual (both-valid)**, the goal (Philippe: a run valid in either
  arithmetic counts, ideally in both; fixed-only and float-only optima are
  results too, so each half's own search stays).
  - The win under Q2 (b): at frame f the float half (Celia float, its jank
    start) exits and the fixed half (real play) has exited at f or
    before. A fixed half that exits first waits as EXITED; a float half
    that exits first is no win (the file would end before real play
    leaves) and is pruned.
  - Both trees are built to the same horizon:
    `<dir>/fixed/level..` and `<dir>/float/level..`, each recording its
    arithmetic.
  - A DUAL STATE is (the fixed exact state or EXITED at f, the float
    exact state or EXITED at f). Each half has its own start: real play
    (`--loading-jank J`) for fixed, UCT's IL or Celia's jank for float
    (Q1).
  - A successor under an input is both engines' leaves. It is admitted
    iff every live half lies in ITS OWN W at that frame.
  - Sound: a both-valid run is valid in each arithmetic, so each half
    stays in its W, and nothing else prunes.
  - Dedup and memo are on the FULL dual state (both `ExactRow`s and the
    exit flags), never a projection (`522de36`).
  - An input is skipped as equivalent only if both engines' read masks
    agree on it.
  - The bound is the larger of the two arc bounds. The breadth-first
    layers run to H, so the first dual win is the optimal both-valid
    run, and no win by H refutes every count up to H.
- **The count when the exits differ (Q2): decided (b)** (section 15):
  the count is Celia float's exit, and the fixed exit must be no later.
  The options as proposed:
  - (a) both exit by C, file length C + 1. UCT's clean save may shorten
    it to the float exit, and that shorter file must not be what gets
    re-uploaded.
  - (b) C = the float exit, with the fixed exit <= C (the UCT-cleaned
    file is valid in real play).
  - (c) equal exits.

  I propose (a) as the default, with both exit frames reported.

## 11. Checks and gates for float

- **t0:**
  - `LuaF64` unit tests: the cut points, the remainder invariant, the
    `sin` call;
  - the UCT goldens (section 3);
  - the rewrite audit (section 4);
  - the emitted-assembly FMA assertion.
- **t1:**
  - float fixtures: single frames;
  - a float `--key-check`;
  - float `ref-check`: kernels against `RefEngine<Float>`, row by row;
  - float `arc-check`: transfers probed inside and outside guards and at
    cut points ±1 ulp;
  - a backward frame.
- **t2:**
  - float oracles for room (1,0): ckhash, posgraph and the arc gate at
    the synthetic win, pinned once the checks above pass, under the same
    re-pin rules;
  - the known-route check in float: a community file that UCT finishes
    is a known FLOAT route (`--prefer`).
- **Witness portability:** `rewrite sin-margin` replays a witness with
  glibc's `sin`, with a correctly rounded `sin` (MPFR, already on the
  machine as a gcc dependency) and with ±1 ulp. It reports whether any
  outcome changes (Q3).
- **Fixed:** `./check.sh t0 t1 t2 big` plus the ignored tests stay
  identical at every milestone.

## 12. The measurement plan (early, before any kernel work)

`rewrite census --arith fixed|float --room X,Y --to F [--level SPEC]` is a
REFERENCE-ENGINE forward over NODES:

- the level's rows with the remainder widened and everything else exact;
- exact dedup, every input class, every thread.

Per frame it reports:

- new nodes;
- distinct `spd.x`/`spd.y` values (overall and per cell);
- speed bundles (nodes per node-without-`spd.x`), with their sizes;
- the float/fixed ratios.

**The fixed run checks the census itself:** its per-frame counts must
equal `gates/ckhash_room10_f000-044_widened.txt` (the widened forward is
exactly "remainder widened, everything else exact").

**Rooms:**

- (1,0) to f40 (0.90M nodes; 2.5M by f44);
- (0,0);
- (2,0), springs;
- (4,2) and (5,0), balloons;
- (0,3) gemskip, the 2500m case: springs and fall floors;
- (1,3).

The reference step's cost is unmeasured here: the concrete search's logs
put it between 0.1 and 2 ms. With ~10 input classes a node, (1,0) to f44 is
~25M steps. So the census times itself on f30 first, and stops at a
budget (minutes a room on 16 threads, at most ~5M nodes).

**Decision rule:**

- float/fixed node ratio <= ~2 everywhere: the float forward runs T1
  speeds;
- >= ~10 anywhere: T3 speed buckets (section 7) come before the float
  storage work;
- in between: per room, and this goes back to Philippe.

## 13. Milestones

Each milestone ends with fixed identical (t0/t1/t2/big plus the ignored
tests, my own fixture and scratch dirs) and small commits.

1. **The float oracle.**
   - `LuaF64`, the `Arith` trait, the interpreter generic over it;
   - the float cart patch (Q4);
   - `RefEngine<Float>` with the per-tool starts and seeds;
   - the UCT exact-trace driver, goldens and differential tests;
   - Celia goldens for a subset.

   Exit: bit-exact every frame on the golden set.
2. **The census** (section 12): numbers, then the T1/T3 decision.
3. **Float trees without kernels.** `Rt2`, codes, dictionaries, frame,
   hull, edge and out files generic. `RefEngine<Float>` as the forward's
   frame step (it already is a `FrameStep` in fixed). A float forward at
   small horizons, end to end, slow.
4. **Float arcs.**
   - transfers (`Mono`/`Const`);
   - arc sets over order keys, with the pull, push, hull and reach;
   - the backward;
   - the float concrete search;
   - float `arc-check`.

   First float optimum: room (1,0) at the synthetic win, on the
   reference-engine tree.
5. **Float kernels.**
   - the codegen (section 5), the call-outs;
   - float `ival`/fold/`pieces_of` and level -1;
   - float `ref-check` and key-check;
   - the float pins (section 11).

   Then forwards at full speed and real rooms.
6. **The dual search**, then validation:
   - gemskip 100% 2500m (on a scratch branch with `jank-keys` merged);
   - gemskip any% 2600m (the float optimum against UCT's 123, and the
     both-valid optimum in fixed point);
   - nodiag 1800m (85 against the UCT-only 86).

   Every witness through UCT, Celia (float and `--fixp`) and real PICO-8.
7. **T3 speed buckets and T2 platform remainders,** if the census or the
   real rooms call for them.

## 14. Open questions

(Q1, Q2, Q3-Q9 are answered: section 15.)

- **Q1. Which float starts must a dual-valid run satisfy?**
  - UCT: IL load, its seeds.
  - Celia: loading jank `J`, its seeds.
  - Both: the dual search takes any number of halves.
- **Q2. The count when the two exits differ.** Section 10's (a), (b) or
  (c).
- **Q3. `sin` across platforms.** Accept "bit-exact on this machine's
  glibc" plus a portability report, or REQUIRE witnesses whose outcome
  survives a correctly rounded `sin` and ±1 ulp?
- **Q4. The float cart form.** A float-only patch (fixed byte-identical,
  kernel metrics included), or one form for both and a re-pin of kernel
  metrics?
- **Q5. The arithmetic parameter.** A compile-time `A: Arith`
  (recommended) or a runtime tag over raw `u64` words?
- **Q6. The scope of "byte-identical" for fixed.** Pins and gates only,
  or on-disk tree bytes too? The generic design gives both.
- **Q7. `jbuffer`.** The float engine, like fixed, runs the minimal
  cart, and the UCT goldens are canonicalized. Agreed?
- **Q8. The native `tile_flag_at`'s truncating division** (section 2).
  Fix it to the Lua loop's semantics (raise where it differs) as a small
  separate change?
- **Q9. Budgets.** W endpoints at 8 B instead of 2; hulls possibly
  stored rounded outward. Fine to size them from the first float trees?

## 15. Status (2026-10-15): decided, built, measured

### Decisions (the answers to section 14)

- **Q1 (Philippe)**: the float tool that matters is CELIA's float mode,
  with its loading-jank start. UCT stays a secondary report. So `--arith
  float` is Celia by default, from Celia's IL load (its `J`; `--loading-jank`
  overrides it), and `--float-tool uct` is the secondary.
- **Q2 (Philippe)**: rule (b). The file's length is the float exit (Celia
  float), and real play (fixed point, PICO-8) must exit no later.
- **The policy (Philippe)**: a run valid in EITHER arithmetic counts as a
  valid submission, but ideally ours work in both. So the dual search is
  the goal, and fixed-only and float-only optima are useful results too.
- **Q3**: bit-exact on this machine's glibc (2.42) is the definition.
  Every float or dual witness also gets a robustness report (a correctly
  rounded `sin`, and ±1 ulp on every `sin` call); a witness that fails it
  is flagged loudly, not rejected.
- **Q4**: ONE cart form for both arithmetics, the original's expressions
  (below). Fixed unchanged, shown (commit `e764d83c`).
- **Q5**: the compile-time parameter (`celeste_core::arith::Arithmetic`,
  `Fixed` and `Float`).
- **Q6**: byte-identical means the pins and gates AND the trees' on-disk
  bytes; kernel metrics may move and are re-pinned with evidence.
- **Q7**: no jump buffer; community files are re-timed (below).
- **Q8**: `tile_flag_at` fixed on its own branch (`tile-flag-fix`).
- **Q9**: budgets sized from the first float trees.

### The cart form (A2) and the speed guard

`move` is the original's three statements with the hint
`__split_by_round(rem)` (the fragments on which `flr(rem + 1/2)` is one
integer: in 16.16 the floor fork of `rem + 1/2` shifted back, its arc
capture the split it was; in doubles the exact cut points), and an exact
16.16 fold `(x - r) + r -> x` where the ranges bound `x - r`, so the fixed
kernels compute what they did. Fixed is unchanged, measured (every
fixture fingerprint, gate, W and optimum; one kernel metric re-pinned:
`r42n` +6 fused nodes of 1.14M instructions; frame time equal).

The `_update` guard (an object at speed 0 skips `move`) STAYS. Dropping it
breaks the kernels and the reference engine at the object levels (a
balloon's widened `y` meets `is_solid`); keeping it only for non-players
changes the level -1 tables. Where the guard differs from the original
(which always calls `move`), in either arithmetic:

- an object at speed 0 inside a solid: the original's `move(0, 0)` runs
  one iteration of `move_x`'s loop (`sign(0) = 0`), finds the solid and
  zeroes `spd` and `rem`; ours keeps `rem`. Only where the draw's clamp
  or a spawn puts the player in a wall with a remainder;
- float only: `rem = -0` becomes `+0` in the original (`-0 + 0`), equal
  in value (no read tells them apart: no division, `tostring` or `atan2`
  of a remainder), so the trajectories are the same and only the bits of
  a state differ;
- float only: `rem = 1/2 - 2^-54` would step a pixel, but no move leaves
  it: after any move `rem` is in `[-1/2, 1/2 - 2^-53]` (`r1 = 1/2 - 2^-54`
  gives `k = 1` and `rem = -1/2`; every other `r1 - k` is exact, and for
  `|r1| >= 1/2` a multiple of `2^-53`).

The oracle compares every bit after every update and agrees on every
golden, so none of the three occurs in them.

### Milestone 1: the float oracle

`FloatRefEngine` (`ReferenceEngine<Float>`) under `--arith float`
(Celia, from its IL load's jank `J = starts::celia_jank(room)` unless
`--loading-jank` says otherwise) or `--arith float --float-tool uct`: the
traced cart (the same source as fixed's) with the tool's start and seeds (`trace::cart::tool_seeds`: UCT's chest
`offset + 1`; Celia's chest `seed + 1 + 2^-16` and its balloons reseeded
after the jank; every seed 0 where a file gives none). Only `rewrite
census` takes `--arith float` so far; every other command refuses it (the
footgun is a fixed run under a float flag).

**The goldens** (`gates/uct/`, `gates/celia/`, text traces compressed
with zstd, regenerated by `tools/uct/golden.py` and
`tools/celia/golden.py`): the tools' EXACT state after every update of a
playback (`exact.lua`: every number of every object as a hex float, read
after `_update` and `_draw`, since the player's draw clamps it), for

- every tasdatabase file of any%, nodiag, 100%, gemskip any%, gemskip
  nodiag and gemskip 100% (deaths and timeouts included: the trace goes on
  to the tool's last update);
- 2 random files a level (`random_inputs`: held directions, jump and dash
  presses at random): deaths, walls and spikes at odd remainders.

UCT plays from its IL load, Celia from its loading jank (`J` from `rewrite
starts`' `celia`). The tests `trace::oracle::tests::the_float_engine_plays_
every_golden_as_{uct,celia}_does` replay each in the float engine from the
same start and compare every number and boolean we hold with the tool's,
BIT for bit, after every update; the update that leaves the room must be
the tool's last. One relaxation: a chest's `x` while it shakes is the
tools' unseeded `rnd` (`UNSEEDED`): ours is the range, theirs must lie in
it.

**Result: bit-exact everywhere.** UCT: 166 goldens, 17,433 updates. Celia:
166 goldens, 17,376 updates. Every number and boolean equal after every
update (both tests ~6 s, quick profile). What it took, beyond section 2's
plan: the original `move` form (A2), the tools' seeds (chests, Celia's
reseed), the traces read after the draw (the clamp), the jump re-timing
extended past a file's end, and Celia's own re-timing. Not yet in the set:
our witnesses (mostly community files' twins; cheap to add).

**The jump buffer.** The original cart buffers a jump press for 4 frames;
the traced cart does not. `tools/uct/canonical_jumps.py` re-times every
buffered jump of a tool's run to its own frame (the press moved, the file
extended where the jump fires past its end) and the golden scripts check
that the re-timed file makes the same run in the tool and leaves no
buffered jump. A jump is read from the player's state (`jbuffer` positive
at the jump check, 0 after it); UCT's traces also hook the jump sounds
(`psfx` 1, 2) and the two must agree on every update that leaves a
player (Celia copies the cart every step, which defeats a hook). Celia's files are re-timed from
Celia's own run: its start differs, so its run does (any% 2,1 dies in
UCT and finishes in Celia).


### Milestone 2: the census

`rewrite census` (section 12) in both arithmetics from the same start
(the IL load: `--float-tool uct` in float, so the two columns differ only
by the arithmetic; Celia's jank start changes only the start state), 28
threads, to a budget of 80k nodes or states. Two modes:

- NODES: the level r0sx (the remainder widened to the pixel, everything
  else exact): what a float forward over today's abstraction would hold.
  The fixed column equals the pinned widened forward (the test).
- `--exact`: CONCRETE states (the remainder kept): what the concrete
  search walks, and the real diversity of speeds and remainders.

| room | nodes at | fixed | float | float/fixed | states at | fixed | float | float/fixed | spd.x | spd.y | rem.x | rem.y |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| (0,0) | f34 | 30,281 | 52,174 | 1.72 | f35 | 62,612 | 65,225 | 1.04 | 51/41 | 22/22 | 179/84 | 25/25 |
| (1,0) | f31 | 44,590 | 74,917 | 1.68 | f31 | 63,755 | 66,206 | 1.04 | 51/41 | 27/27 | 182/88 | 26/26 |
| (2,0) springs | f32 | 30,324 | 58,738 | 1.94 | f33 | 63,516 | 66,090 | 1.04 | 46/36 | 24/24 | 172/78 | 24/24 |
| (4,2) balloons, seeds 0 | f43 | 28,121 | 56,588 | 2.01 | f44 | 48,257 | 50,703 | 1.05 | 45/33 | 23/23 | 110/64 | 22/23 |
| (5,0) balloon, seed 0 | f35 | 27,634 | 51,620 | 1.87 | f36 | 47,794 | 50,392 | 1.05 | 45/33 | 19/19 | 110/64 | 16/16 |
| (0,3) gemskip | f32 | 30,564 | 60,745 | 1.99 | f33 | 61,392 | 64,170 | 1.05 | 45/35 | 26/26 | 170/76 | 30/32 |
| (1,3) | f34 | 24,099 | 62,513 | 2.59 | f33 | 60,598 | 61,455 | 1.01 | 46/36 | 18/18 | 172/78 | 6/6 |

(The last frame both columns finished under the budget, totals so far;
`spd.x` etc. are the distinct values over every concrete state so far,
fixed/float.)

The node ratio grows with the frames in most rooms: (1,3) 1.97, 2.21,
2.59, 2.91 at f32-f35 (and there the phantom nodes reach speeds no fixed
node has: 57/50 distinct `spd.x`, 38/27 `spd.y` at f35); (4,2) 1.85 to
2.12 over f41-f44; (1,0) is the exception, 1.84 down to 1.63. The first
frame where the two differ: 2-3 frames after the player spawns (nodes),
5-6 (states). The budget reaches only ~10 frames past the spawn, so
`census --exact --sample 3000` (3,000 concrete states expanded a frame,
the lowest key hashes) looks ~90 frames further; its counts are lower
bounds, fixed/float at the last frame:

| room | frame | states met | spd.x | spd.y | rem.x | rem.y |
|---|---|---|---|---|---|---|
| (1,0) | f110 | 1.22M / 1.20M | 59/59 | 78/66 | 3,613/2,612 | 784/918 |
| (2,0) springs | f110 | 1.35M / 1.35M | 114/132 | 108/118 | 3,011/2,001 | 1,824/2,242 |
| (4,2) balloons | f110 | 1.24M / 1.24M | 59/60 | 77/79 | 1,185/858 | 3,220/4,520 |
| (0,3) gemskip | f130 | 1.56M / 1.54M | 59/61 | 80/91 | 1,576/719 | 1,350/2,496 |

Deep in the rooms the speeds stay within ~15% of fixed's either way;
float has fewer `rem.x` and more `rem.y` values (up to 1.8x at (0,3)).

**Speeds: no buckets needed.** Over concrete states float is fixed's size
(x0.96-1.07 everywhere). In the first frames it has FEWER distinct `spd.x`
(about 20% fewer) and about half the distinct `rem.x` (why 16.16 makes
more values is not investigated), `spd.y` the same; deeper in, the speeds
stay within ~15%. By section 12's rule the float forward runs T1 (exact)
speeds; T3 buckets are not needed.

**The remainder's widening is NOT free in float: the top sliver.** Over
nodes float holds 1.7-2.6x fixed's (2.9x at (1,3) f35), mostly growing
with the frames. It is not the speeds. A node's widened remainder `[-1/2,
pred(1/2)]` holds the few doubles just below 1/2 whose sum with an
INTEGER speed rounds onto the next cut: `fl(r - 2) = -3/2` for `r` in
`{1/2 - 2^-54, 1/2 - 2^-53}`, so the player moves one pixel, not two. In
16.16 `r + s` is exact and the top value `1/2 - 2^-16` never reaches the
cut, so a node at an integer speed has ONE outcome per input in fixed and
TWO in float (y=110 from y=112 at `spd.y = -2`, room (1,0) f25: the first
difference). Measured: the float census with the top 1024 doubles below
1/2 left out (a diagnostic, not sound) has fixed's counts or fewer (room
(1,0) f28: 6,974 against fixed's 7,284 and float's 13,150; (2,0) f33:
48,139 / 52,313 / 104,470; (0,3) gemskip f33: 49,475 / 54,147 /
108,149).

The sliver is reachable in principle (`1/2 - 2^-53` is a possible
remainder after a move; `1/2 - 2^-54` is not: section 15's guard
argument), so dropping it from the widening is unsound. Dropping only the
unreachable `1/2 - 2^-54` (exact) removes little: room (1,0) f28 12,350
against 13,150. The arcs are
exact in the remainder, so W refutes the phantom nodes, but the float
forward pays for them. Options, for milestone 3 (decide on the first float
trees):

- (a) accept ~2x the forward's nodes;
- (b) the hull forward (the remainder's exact transfer images instead of
  the whole pixel): a node gets the sliver only where an image reaches it;
- (c) one exact bit of refinement: the remainder's domain split into the
  pixel and the sliver, the sliver part a node of its own made only by an
  edge whose exact transfer image meets it.

(b) and (c) are exact (an image is the transfer's, monotone), so nothing
new needs refuting; what they cost is the measurement.

### Milestone 3: float trees without kernels

**What was built.**

- The forward and its storage are generic over the number type
  (`celeste_core::arith::EngineNum`: blocks, codes, key spaces, layers,
  frames, hulls, routes, waves, edges, out files) and over the remainder
  model (`search::rem::Rem`: the hull, its order-free accumulator, the
  transfer pair, the push). Fixed is the default type parameter and is
  unchanged: the ladder (t0 with the ignored tests, t1, t2) passes with
  every pin, fingerprint and gate, and a float tree's files carry their own
  magic byte.
- The float remainder model (`search::rem_f64`, section 6):
  - a point is a double of `[-1/2, 1/2)` by value, `-0` being `0`
    (`2^63 - 2^54` points, in a `u64`);
  - a hull is an arc per axis with two charts, as in 16.16;
  - a transfer is `Mono(ox, k)` (`r -> fl(fl(r + ox) - k)`, the cart's two
    operations) or `Const`, on a guard found exactly by bisection with
    the operation itself;
  - the decode refuses what it does not model, as 16.16's does;
  - tested against the cart on 55 speeds (the pieces partition the points
    and act as `move` does at every probe), and the push is tested sound.
- The reference engine is generic. `RefFrameStep<N>` runs a unit's lanes
  in parallel on its pool of engines and emits them a chunk at a time.
- `rewrite forward --arith float` always runs the reference engine (there
  are no float kernels); `ckhash` reads float trees.
- Two checks on a finished tree:
  - `rewrite hull-replay` replays the hull forward over the recorded graph.
    The replay must reach exactly the tree's states at the tree's frames,
    else it is an error. It also counts the replay without the sliver's
    edges, and under (c)'s exact bit.
  - `rewrite hull-route` steps a known route concretely in the tree's
    arithmetic. Every state of it must be in the tree by its frame, with
    its remainder inside its state's hull, and every step must be a
    recorded edge whose transfer maps the remainder.
  - The test `room_10_trees_replay_and_hold_their_routes` runs both in both
    arithmetics.

**Measured** (room (1,0) and (1,3), level `r0sx`, Celia float from its
jank start, 32 threads).

- The replay reproduces every tree exactly: fixed (1,0) to f44, (1,3) to
  f40, and both float trees.
- These routes hold through the float tree of (1,0) to f44: Celia's any%,
  nodiag, both random files, and our gate witness. In fixed, the gate
  witness and the community any% hold.
- Room (1,0) in UCT float is the same tree as in Celia through f35, state
  for state (the jank does not reach the player there). So there the
  float/fixed ratio is the arithmetic's alone.

| | (1,0) f44 | (1,3) f40 |
|---|---|---|
| fixed hull forward, states (total) | 461,632 | 106,167 |
| float hull forward (b) | 500,744 (1.085x) | 132,170 (1.245x) |
| at the last frame, float/fixed | 93,807 / 86,156 (1.089x) | 34,215 / 25,936 (1.319x) |
| float without the sliver (not sound) | 428,614 (0.928x) | 102,250 (0.963x) |
| float, (c)'s exact sliver bit | 485,950 (1.053x) | 126,335 (1.190x) |
| ... of them with the bit set (c's sliver parts) | 384,522 | 83,480 |
| edges with a sliver piece | 22.7% of 18.2M | 26.6% of 3.6M |
| of them, edges whose push fires | 15.4% (3.5% of all) | 14.9% (4.0% of all) |
| float forward wall time, peak RSS | 44.5 min, 8.0 GB | 12.3 min, 8.3 GB |
| fixed kernels, the same forward | ~3 s | ~5 s |

- The per-frame ratio in (1,0) rises to 1.22x at f33 and settles at
  1.03-1.10x over f36-f44. In (1,3) it grows with the frames: 1.17x at
  f37, then 1.19x, 1.28x and 1.32x at f40.
- The sliver accounts for ALL of the gap. Without it, float holds 7%
  (1,0) and 4% (1,3) FEWER states than fixed. (M2's census found float
  has fewer distinct `rem.x` values too.)
- The float forward's time is the reference engine's: about 7 ms of wall
  time per lane at f44 on 32 threads. The fixed reference engine on (1,0)
  to f36 took 222 s (56k lanes, 8.0M leaves), the float one 356 s (64k
  lanes, 12.8M leaves). Both spend 28 µs a leaf. Float's extra cost is
  its lanes (1.14x) and the sliver's extra fragments (1.4x the leaves a
  lane), not the arithmetic.
- The peak memory is the 32 interpreters' fork paths: about 128 lanes'
  leaves held at once. The trees themselves are small (18 MB).

**Decision: (b), the hull forward.**

- (b) leaves 1.09x in (1,0) and 1.25-1.32x in (1,3), down from the widened
  forward's 1.7-2.9x.
- (c) does not pay. With its bit EXACT (an image sets it only where some
  point maps into the top `[1/2 - 2^-40, 1/2)`), it still sets the bit on
  79% (1,0) and 66% (1,3) of the states. Its node count (states plus
  sliver parts) is 1.6-1.7x (b)'s.
  - The reason: the top doubles are reached legitimately. The lower piece
    of every non-integer speed maps the doubles just below its cut to just
    below 1/2 (`r1 = c - ulp`, `r' = 1/2 - ulp`). Any hull that holds the
    top of that piece's guard holds them too. Concretely they are as rare
    as M2 found, but no interval (hull) abstraction can tell.
- (a), the widened forward, is the 1.7-2.9x of milestone 2.
- The arcs and the concrete search are exact in the remainder, so the
  sliver's states cost forward size and time, never soundness. Revisit
  only if a real room's ratio runs away (watch (1,3) past f40), e.g.
  with a finer remainder abstraction than an interval.

**Not covered yet:**

- the level flags (`h`, `n`, `f`, `p`) in float: only `r0sx` has run;
- level -1 in float;
- a float `arc-check` (milestone 4; `hull-route` checks the transfers on
  known routes only).

### Milestone 4: float arcs and the float concrete search

**What was built.**

- The arcs' sets are generic over the CIRCLE of points
  (`search::arcs::Circle`):
  - `Fixed16`: 16.16's 65536 points, `u16` words, rotations;
  - `rem_f64::Doubles`: the doubles' points, `u64` words, `Mono` and
    `Const` transfers.
  `Region`, `RegionRef` and `RegionArena` take the circle, and so does the
  pull. A `Mono` axis pulls a segment to the guard's points between two
  bisections, exactly (section 6). Its push (used by `reach` only) is the
  interval between the ends' images, a superset.
- The arc phase is generic over the remainder model `Rem` (its circle is
  `Rem::C`):
  - the backward and `Winning`;
  - `optimum`, `reach` and the concrete search, with a reference engine
    of the tree's arithmetic;
  - `solve`;
  - the known-route check (`search::known`).
- `rewrite search --arith float` grows the tree with the reference engine
  and runs the same arc phase. It refuses what float has not met yet: a
  second level (the objects ladder), level -1, and `--save-marks`.
- `rewrite arc-check` is one generic module (`search::arc_check`).
  - 16.16 steps its probes with `step_at`; its output on the ladder's
    fixtures is unchanged, byte for byte.
  - Doubles step concrete rows (level `r0sx`). The guard ends are the cut
    points, and an outside probe is one ulp past them.

**The first float optimum: room (1,0), the synthetic win at (9, 101), by
f35, Celia float from its jank start.**

- Arc bound 33, concrete optimum 33. The depth-first try at the bound wins
  at once (`--prefer` the gate's route).
- The witness is the gate's own inputs. It holds in the tools themselves:
  in Celia's exact trace of those inputs (`gates/celia/witness-room10gate_2`)
  and in UCT's, the player is at (9, 101) after update 32, i.e. frame 33.
  In 16.16 it is the gate's witness. So it is valid in both arithmetics.
- The known route survives every pruning step in float, as in fixed.
- 256 s, against fixed's 1.9 s: the reference engine's forward is nearly
  all of it. The backward and the concrete search take under a second.
- W is bigger in float.
  - Support: 2,420 nodes against fixed's 1,290 (1.9x).
  - Per frame: 987 against 418 at f30, and 921 against 261 at f31 (3.5x).
  - The sliver's states can win too, and W is exact over all of them.
    The concrete search still admits only real remainders.
- The test `arc_dp::tests::a_float_search_proves_room_10s_synthetic_optimum`
  pins the end-to-end chain on a smaller case: the win at (4, 101) by f30.
  - The arc bound is 28 in both arithmetics, with one witness for both.
  - The known route survives in both.
  - The float transfers pass `arc_check`, and a corrupted transfer fails
    it.
  - About 40 s.

**Float `arc-check`** on the (9, 101) tree, frames 1-35, 64 samples a
frame:

- 1.24M records, every one with a transfer.
- 751 records sampled: 2,863 probes inside the guards and 1,408 outside.
  Nothing disagrees, and nothing reaches a target unpredicted.
- Faults are caught:
  - `--fault action`: 621 of 1,408 inside probes disagree;
  - `--fault guard`: 326 of 910 outside probes reach the target.

**Fixed is unchanged:**

- the ladder (t0, t1, t2, and the ignored tests);
- the room (1,0) arc gate, byte for byte;
- `arc-check`'s output on the fixtures, byte for byte.

**Room (1,3) past f40** (section 15's milestone 3 open question):
the float/fixed ratio peaks and then falls; it does not run away.

| frame | f40 | f41 | f42 | f43 | f44 | f45 | f46 |
|---|---|---|---|---|---|---|---|
| fixed | 25,936 | 31,766 | 39,525 | 49,730 | 60,255 | 70,049 | 83,110 |
| float (b) | 34,215 | 42,730 | 53,860 | 64,390 | 76,058 | 86,409 | 99,317 |
| ratio | 1.32x | 1.35x | 1.36x | 1.29x | 1.26x | 1.23x | 1.20x |
| without the sliver | 0.95x | 0.97x | 0.98x | 0.99x | 0.99x | 1.02x | 1.03x |

- Through f46: 554,934 states against 440,602 (1.26x). Without the sliver
  it is 437,725 (0.99x), so the sliver is still the whole gap.
- (c)'s bit would hold 538,809 states (1.22x) plus 435,378 sliver parts:
  no better, as at f40.
- 66 min to f46 on 32 threads (the ladder ran beside part of it), peak
  12.6 GB.

### Milestone 5: float kernels (the plan)

The reference engine is the whole of a float search's time: (1,0) to f44
takes 45 min against seconds. Real rooms (exits at f70-f160) need the
kernels. The pipeline is generic over the arithmetic, as the storage and
the arcs already are. Fixed stays byte-identical (kernel metrics included)
at every step. The steps, each a milestone with its own report:

1. **The graph IR and the tracer, generic.**
   - `Graph<A>`, `Op<A>` (literals as `Arithmetic::Lit`: 16.16's raw `i32`,
     a double's bits), `Val<A>`.
   - The arithmetic's own rules sit behind one trait, whose 16.16 impl is
     the current code moved verbatim: the literal folds, the evaluator's
     interval semantics, the fork grids, the static ranges (`pieces_of`).
   - Float folds only what is bit-identical. `+` commutes. `min`/`max`
     do not: LuaJIT returns the second argument on a tie, which tells
     `-0` from `+0`. No algebraic identity that rounds: `(x - r) + r`,
     `x + 0`.
   - Float's own errors: a result that is not finite (`NoWrap` reads
     "representable"). `__split_by_round` is its own fork, at the exact cut
     points (`LuaF64Interval::round_cut`).
   - The check: the float trace of a frame, evaluated per lane, equals the
     float reference engine on a sweep of states and every input (the
     float twin of `a_traced_frame_agrees_with_the_oracle`).
2. **The float codegen.** Eight `f64` lanes a zmm.
   - Every operation in the cart's evaluation order, and nothing else:
     - IEEE `vaddpd`/`vsubpd`/`vmulpd`/`vdivpd`;
     - `flr` as `vrndscalepd` round-down (exact);
     - `%` as LuaJIT's `a - flr(a/b)*b`, three separate rounded ops;
     - `min`/`max` as compare-and-blend with LuaJIT's tie rule, never
       `vminpd`;
     - `abs`/`neg` on the sign bit.
   - No FMA: the generated text is checked to hold no fused mnemonic, and
     gcc only assembles it. Contraction is impossible, not just off.
   - `sin` is a per-lane call-out to the reference's own `LuaF64::lua_sin`
     (glibc's `sin`), or specialized away where its argument is a constant
     per lane.
   - MXCSR is checked at every float kernel call: round-to-nearest, FTZ
     and DAZ off.
   - Per operation bit-exact tests against `LuaF64`, `-0`, ties and
     subnormals included.
3. **The float kernels in the forward.** The registry, the dispatch and the
   emission take the arithmetic.
   - Checks: the float `ref-check` (lane by lane against the float
     reference engine), `--key-check`, `kernel lanes: missed 0`, and the
     float `arc-check` on kernel-built trees.
   - The float trees the kernels build must equal the reference engine's
     byte for byte (`ckhash --edges --hulls`) on (1,0) to f44 and (1,3) to
     f40.
4. **Pins**: float gates beside the fixed ones.

Not covered in float yet:

- the level flags (`h`, `n`, `f`, `p`): the reference engine's `step_at`
  and its pins are 16.16's;
- level -1;
- the objects ladder;
- the UI's marks.

The kernels start at `r0sx`, as the float searches do. The UCT goldens
stay the secondary report.

**What a committed golden may be.** Community database files, and our
routes that are already submitted. Philippe's rule is that our unsubmitted
or teased routes are never revealed, and this branch reaches `develop`,
which is pushed. So they are LOCAL goldens:

- They live in `/var/tmp/float-goldens/` (`witnesses/witnesses.txt`, and
  per tool its manifest and traces), made by `golden.py local all`.
- `trace::oracle::tests::the_local_goldens` replays them. Where the store
  is absent, it prints SKIPPED, and nextest shows that output on success
  (`.config/nextest.toml`).
