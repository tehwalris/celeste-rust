# Program rewriting plan (2026-08)

Status: design only, nothing implemented. Written after re-reading the whole
codebase and re-measuring the interpreter.

## 0. Measurements

Full numbers live in `BENCHMARK_DATA.md` (kept current). The previous contents
of that file were stale by ~25x and would have led to bad decisions.

Headline, on `interpreter` @ 9ec9778 (i.e. *with* mem2reg + block_coalesce):

| frames | wall (cumulative) | lanes at end | peak RSS |
|---|---|---|---|
| 30 | 0.62 s | 15,250 | 0.14 GB |
| 34 | 4.80 s | 92,713 | 1.17 GB |
| 37 | 17.9 s | 269,059 | 3.86 GB |
| 40 | 66.7 s | 948,319 | 15.2 GB |

Derived, and these are the numbers that matter:

- **Per-lane cost per frame is ~flat**: 18.8 µs (f30) → 23 µs (f37) → 24 µs (f40).
  Fragmentation is not getting *relatively* worse; lane count is just growing
  ~1.45x/frame.
- **Per-lane memory is ~flat at 13–16 KB.** The end-of-frame state has
  `heap_len = 280` cells, of which maybe ~120 are per-lane numeric/bool. At 8 B
  that is ~1 KB/lane of actual information. **We are carrying ~15–30x memory
  overhead.**
- Every frame ends as **exactly 1 vectorized state**. All the fragmentation is
  *intra*-frame. Frame 30: 30 states get merged back into 1 at the boundary.
- Extrapolating at 16 KB/lane, the 100 GB wall is around **frame 45**, at roughly
  5–8 minutes cumulative.

Scalar sanity check: `concrete_run` does 30 frames single-lane in 46 ms
including parse+init, i.e. a scalar frame is ~0.5 ms. So the vectorized
interpreter is already ~25x more efficient per lane than the scalar path —
vectorization is working; the problem is what happens *between* the frame
boundaries.

### Where the intra-frame time actually goes

From `trace_frame37.json` (26 MB, checked in, one frame, 318k spans):

| category | self time | share |
|---|---|---|
| `cfg` (nested CFG interpretation) | 6.05 s | 46.4% |
| `materialize` | 3.28 s | 25.2% |
| `vectorize` | 2.43 s | 18.6% |
| `gc` | 1.06 s | 8.1% |
| `filter_by_mask` | 0.21 s | 1.6% |

**236,052 nested CFG interpretations in a single frame.** `obj.collide` 35,636x,
`obj.check` 35,636x, `tile_at` 26,883x, `sign` 15,426x, `is_solid` 11,032x — for
a frame that logically calls each a handful of times.

So the user's intuition is right but the accounting is subtle: `filter_by_mask`
itself is 1.6%. The *cost of having filtered* — a fragmented state set, each
fragment re-entering every callee with its own nested worklist, then a huge
normalization pass to un-fragment — is ~80%.

Mechanically (see `glue.rs:373`, `flow.rs:268`, `flow.rs:206`):

1. Every `ConditionalBranch` does `flow_data.clone()` unconditionally, then
   `filter_by_mask` on each edge if the condition is a mixed vector.
2. The two fragments are queued as **separate work items**. There is no join —
   `FlowData::join_mut` is a plain `Vec::extend`, and merging only happens at
   `hint_normalize` blocks. There are exactly **two** `_hint_normalize()` calls
   in the whole game, both in `_update`'s `foreach`. Everything inside
   `player.update` / `move_x` / `is_solid` / `collide` runs fully split.
3. `BlockPostPhi` folds instructions over a `Vec<State>`; a `Call` runs the
   *entire nested interpreter once per caller state* and can return k states,
   multiplying all subsequent work in that block by k.

## 1. What happened last time, and what to keep

Timeline: the core vectorized interpreter was finished by 2025-12-31. On
2026-01-03 a generic CFG optimizer effort started (mem2reg, block coalescing,
call resolution, inlining, DCE, validation, builtin resolution, heap
elimination, phi cleanup, deopt insertion). On 2026-01-10 an unattended agent
loop (`loop.sh` + `AGENT_PROMPT.md`) ran 93 "cleanup:" commits whose explicit
brief was to *preserve and harden those passes*.

Honest assessment of the optimizer work:

- **Only `mem2reg` + `block_coalesce` ship.** `FixedEnv::optimize_cfg` is 68
  lines and is the entire optimizer that affects gameplay
  (`game_runner.rs:513` → `FixedEnv::new_with_optimizations()`).
- `FixedEnv::apply_interprocedural_optimizations` — the function that would
  enable call resolution + inlining — **has zero callers in the repo**. Inlining
  has never run on a real frame.
- The pipeline's output **cannot be executed**: `Terminator::Deopt` reaches
  `flow_return()`'s `_ => panic!("Unexpected flow")`. Heap elimination emits
  `unpack_slots` LocalIds with no defining instruction. It is telemetry for a
  web viewer, nothing more.
- `heap_elimination.rs` (1,269 prod lines) iterates `cfg.named` in **FxHashMap
  order**, threading one mutable fact-map through all blocks with no merge, no
  fixpoint, and no relation to program order. Its cycle handling fabricates phi
  ids it never records. Multi-exit repack "just takes the first exit".
- `cfg_validation` checks 5 things and **does not check dominance**, which is
  exactly the class of bug mem2reg/inlining produce. It also passes only
  `arg_ids`, not `arg_ids ∪ capture_ids`, so every closure reports spurious
  `UndefinedLocal` errors — ~50 warning lines per viewer run, burying real ones.
  A real defect is reproducible today: `heap_elim_final` on `btn_4` leaves
  `%17` dangling; it "passes" only because a later DCE deletes the blocks.
- Roughly **11k lines** of pass code + 3.3k lines of tests pinning it in place +
  a **28 MB `serve/cfg_analysis.json` blob checked into git** (currently dirty).

Two sibling branches are relevant:

- **`symbolic-tracing`** already tried "whole frame as a pure traced function".
  It failed: path enumeration from `UnknownBool` gave a 14x trace explosion
  (15,250 states → 211,644 traces at frame 30) and ended **5x slower** than the
  reference. Its own doc's recommended revival path is *"keep states vectorized
  during tracing, handle branches by filtering vector lanes"* — i.e. exactly
  the select-based direction below. **The past failure condemns path
  enumeration, not branch elimination.** Read
  `git show symbolic-tracing:docs/symbolic-tracing-design.md` before
  re-attempting anything in this family.
- **`barrier`** has genuinely useful *core* perf work (Vec-based COW
  `LocalEnv`/`Heap` replacing `im::HashMap`, `Arc` for `Label`/`GlobalId`),
  independent of its barrier feature. Worth harvesting later.

### Base (done)

The `rewrite` branch was created from **`612e726`** (2025-12-31,
`Revert "Add LazyVector..."`) — the direct parent of the first optimizer commit.
Clean core interpreter, no optimizer code, no `Deopt`/`CallResolved`/
`CallBuiltin` IR variants, no 28 MB blob. The core execution path (`flow.rs`,
`state.rs`, `core_interpreter.rs`, `vectorize.rs`, `glue.rs`, `main.rs`,
`game_runner.rs`) has zero imports of any pass module, so nothing of value was
lost.

Cherry-picked: `df3c68c` (op.rs unit tests — the one good artifact of the agent
loop), `73f503c` (`interpret_not` panic → `Err`), `0a02d6d` (`FromStr` for
`Pico8Num`). The rest of the candidate cherry-picks were entangled with the
deleted pass files and were not worth untangling.

The `interpreter` branch is kept as **reading material**: `inlining.rs` is a
reasonable starting point for the `inline` rule, `mem2reg.rs` for `promote_cell`.
`AGENT_PROMPT.md` and `loop.sh` are deleted — as written they instructed any
future agent loop to preserve and harden the passes, i.e. to re-create the
problem.

**Cost of the branch-off, measured**: dropping `mem2reg` + `block_coalesce`
costs ~1.3x time and ~1.9x memory at frame 40 (15.2 GB → 29.2 GB). See
`BENCHMARK_DATA.md`. That is the first debt the rewrite work has to repay, and
it gives Stage C a concrete target rather than an aesthetic one.

## 2. Target program shape, and an honest estimate of the payoff

Target: the frame body becomes **one function, one basic block, no calls, no
loads/stores except at the frame boundary, all conditionals as `select`**.

Everything the profile shows disappears in that shape: no `flow_data.clone()`,
no `filter_by_mask`, no duplicated downstream execution, no nested
`interpret_prepared_cfg`, no intra-frame `gc`/`vectorize`/`materialize`. What
remains is one `State` with `vector_size = N` and a linear fold where each
instruction is one `MaybeVector::map2` over N lanes.

**But do not expect a 100x from if-conversion alone.** Rough arithmetic:

- Today: ~19–24 µs per lane per frame.
- A branch-free kernel makes every lane execute every path. If the fully
  inlined + unrolled + if-converted kernel is K instructions and the existing
  interpreter costs ~1.5 ns per lane-op (one `Vec` alloc + closure call per
  instruction per lane), cost = `K * 1.5 ns` per lane. Break-even is K ≈ 13,000.
- K is plausibly 5k–50k. So **running the branch-free kernel on the existing
  `MaybeVector` interpreter could be anywhere from a 2x win to a 2x loss.**

What is *not* in doubt:

- **Memory: ~15–30x.** The 13–16 KB/lane collapses toward ~1 KB/lane because
  there is one heap, one local env, and no fragmented `State` clones. That alone
  buys ~6–7 frames at the 100 GB budget.
- **The kernel becomes compilable.** A straight-line, pure, fixed-shape kernel
  over i32 fixed-point lanes is exactly what you emit as SIMD (Cranelift, or
  generated Rust, or even a flat bytecode over reused `Vec<i32>` buffers with
  no per-instruction allocation). At 8–16 lanes/AVX2 op you get ~0.05–0.2 ns per
  lane-op, i.e. **10–50x** over today. That is the actual prize; if-conversion
  is the *enabling* step, not the payoff.

So the framing should be: **we are rewriting the program into a shape that can
be compiled and that stores lanes densely.** Not "we are optimizing the IR".

Two design consequences that fall out immediately:

- **Chunking is mandatory.** Today the DFS worklist keeps the live set small.
  A branch-free kernel materializes all lanes at once, and each frame multiplies
  lanes by up to 64 (six `btn()` reads) times the `__split_by_flr` factor before
  dedup. Run the kernel over lane chunks (e.g. 64k) so peak memory is bounded.
- **Not everything can be a select.** `__split_by_flr` and `btn()` *create*
  lanes; they are lane-expansion (gather/scatter) points, not elementwise ops.
  Design them as explicit expansion steps in the kernel. Likewise
  `Value::Pointer` is not vectorizable, so a `select` between two different
  pointers is impossible — pointer-valued phis must be removed by
  specialization, not if-converted.

### Step 0: the measurement that decides everything (done - see section 8)

Before building any machinery, **measure K**. Instrument the concrete
interpreter to record, over a few hundred random input sequences, the set of
distinct `(function, block)` pairs executed per frame and the dynamic
instruction count. The union of blocks reached is a good estimate of the
if-converted kernel size; the max observed `|amount|` in `move_x`/`move_y` gives
the unroll bounds. This is maybe a day of work and it tells us whether K is 5k
(clear win) or 100k (need a different plan). Do this first.

## 2b. The shape problem, and why it does not conflict with rewrites

The obvious objection to an offline list of rewrites: **the program depends on
the heap.** `foreach(objects, f)` with `obj.type.update(obj)` can only be
devirtualized, inlined or unrolled if you know how many objects there are and
what type each one is. Creating or destroying an object (dying, picking up a
strawberry) changes the heap shape, and after that it is a *different program*.
That is why the old design put the optimizer inside the interpreter as an
approximate JIT: reach a new shape, compile for that shape.

These reconcile cleanly, and the key observation is that **the engine already
computes exactly the right key.**

`vectorize::StateShape` captures the entire heap graph structure: every cell,
every table's key set, every `Pointer(HeapId)` identity, every `Nil`, and which
slots are per-lane numeric placeholders. Numeric *values* are abstracted away;
structure is not. And `vectorize_states` already groups states by it at every
frame boundary. So:

```
for each vectorized state S at frame start:
    prog = jit_cache.get_or_compile(S.shape)
    S'   = run(prog, S)
```

Three consequences fall out:

1. **Shape assumptions need no runtime guards.** If the dispatcher matched
   `S.shape` exactly, then "there is one object", "its `type` field points to
   the `player` table", "`objects[1].type.update` is closure `player.update_21`"
   are all *facts about the key*, not assumptions. A `resolve_call` rewrite
   authored under shape `S` discharges its side condition **against `S`**,
   mechanically. This is precisely the "assumptions about the initial heap
   shape" the JIT needed, except it is now an explicit, inspectable input to
   the verifier rather than something baked into a pass.

2. **The rewrite list is the JIT's output, not a competitor to it.** Recipes are
   keyed: `recipes/<shape-hash>.jsonl`, alongside a human-readable dump of the
   shape. The thing that *generates* a recipe is untrusted and can run at JIT
   time; the applier and verifier are the small trusted core and are the same
   code either way. Recipes get cached and checked in, so the slow Tier 2
   differential verification runs offline against exactly what the JIT
   produced, and a human can read and hand-edit a recipe.

3. **How many shapes are there?** For room 1: very few. The interpreter reports
   `vec: 30 -> 1 states` at frame 30, i.e. all 15,250 lanes share one shape.
   Room 1 has a single object (`player_spawn`, then `player`) - the minimal cart
   has no `smoke` and room (1,0) contains only a spawn tile. So the cache will
   have a handful of entries, and hand-authoring the recipe for the one shape
   that matters is realistic.

### Shape changes *within* a frame

The remaining hard case: the shape is fixed at frame entry, but a lane can
destroy or create an object mid-frame, so lanes can end the frame with different
shapes. Options, in increasing order of effort:

- **(v1) Speculate and re-run.** The kernel additionally computes a per-lane
  "structural event" predicate (died / picked something up / spawned). Lanes
  with an event have their result discarded and that frame re-run for them on
  the general interpreter. The kernel is pure, so discarding is free and
  correctness is unconditional. Structural events are a small minority of lanes
  per frame, so the cost is negligible. **This is the right starting point** -
  it defers all the hard design and cannot be wrong.
- **(v2) Predication over a shape envelope.** Specialize to a superset shape
  with a fixed object capacity and per-lane `alive` flags; creation/destruction
  becomes a flag write, and the actual regrouping into distinct output shapes is
  deferred to the existing frame-boundary `vectorize_states`. This is standard
  SIMT predication and is what you want eventually, especially for rooms with
  strawberries and platforms.

Note that death is not special here, per se - a lane that dies is just a lane
whose subsequent behaviour we still have to model correctly, since an optimal
route may well contain a death. What *is* special is only winning. The thing
that makes death awkward is purely mechanical: `destroy_object` changes the
object array length, hence the shape.

## 3. Architecture: untrusted search, trusted check

The program is **derived**, never edited:

```
lua/*.lua  --frontend-->  program₀  --rewrites.jsonl-->  program_n
```

- `rewrites.jsonl` is an ordered list of concrete rewrite instructions, checked
  into git. It is the artifact we author.
- The **applier** is dumb: it takes `(program, one instruction)` and produces a
  new program. It performs no whole-program analysis and no search. If the
  instruction does not match exactly what it expects, it fails loudly.
- The **verifier** is separate and may be slow.
- *How* we decide which rewrites to make is unconstrained — heuristics, scripts,
  agents, hand-authoring. That code is untrusted and does not need review,
  because the verifier checks the result.

### Addressing, and why it must be by name

If instructions are addressed by `(block, index)`, any earlier rewrite
invalidates every later one. Even addressing by `LocalId` breaks, because
`LocalId` is a dense `usize` index into `LocalEnv` and rewrites mint fresh ones.

Proposal: carry a side table `names: BiMap<Name, LocalId>` next to each function.
Names are stable strings, never reused:

- frontend-produced instructions get `f/block/%17`
- an `inline` of the call named `c` produces names `c$<callee-name>` for each
  spliced instruction
- other rules mint `<rule-id>.<n>` where `<rule-id>` is an author-chosen stable
  id on the rewrite instruction (not its position in the list)

Rewrite instructions address by `Name`. The applier resolves names → LocalIds.
LocalIds stay dense and get re-derived on every replay. Inserting a rewrite in
the middle of the list does not invalidate the ones after it. The name map is
tooling-only; the interpreter never sees it.

### Verification tiers

**Tier 0 — structural validation, after every rewrite.** Single definition per
LocalId; **defs dominate uses** (must be added — this is the missing check that
let the old bugs through); phis are a strict block prefix; phi branch labels are
exactly the block's predecessors, each once; branch targets exist; capture arity
matches callee; every block has a terminator. Cheap, catches most mechanical
mistakes.

**Tier 1 — per-rule side conditions.** Each rule kind has a checker that
validates its syntactic preconditions and re-derives the transform. Write the
checker *independently* of the applier so a shared bug does not cancel out.
Alongside each rule kind, a short written soundness argument: "given these side
conditions, the transform preserves the observation relation."

**Tier 2 — differential execution.** This is the real safety net.

The key invariant that makes it work: **rewrites may change anything inside a
frame, but must not change the cross-frame heap representation.** The frame
function loads the game state from the heap at entry and stores it back at exit,
with the same shape the original program had. Then the observation is simply:

> for each frame k, the canonicalized (GC'd, shape-normalized) set of heap
> states after frame k is byte-identical before and after the rewrite.

That is already computable — `State::gc()` + `shape_of_state` + the vectorizable
value extraction in `vectorize.rs` are exactly the canonicalizer. Two checks:

- (a) **concrete**: the known TAS plus K random input sequences through
  `concrete_run`, comparing per-frame digests. Milliseconds.
- (b) **abstract**: frames 1..N of the real search, comparing the canonical
  end-of-frame state. N=30 costs 0.6 s and covers 15,250 lanes — i.e. an
  enormous number of concrete behaviors at once. Run N=30 per rewrite, N=37 in
  CI.

This is why "slow verification is fine" is such a good constraint here: (b) is
a genuinely strong equivalence test and it is cheap enough to run on every
single rewrite.

**Tier 3 — bounded exhaustive**, for the new IR instruction semantics (`select`
over `MaybeVector`, pointer equality, etc.). Exhaustive over small domains.

### Guards make soundness cheap

Where proving something is hard, **check it at runtime instead**. A rewrite may
insert an `Assert` whose failure is a hard error. This is sound by construction —
no analysis required. And it is nearly free in this engine: a branch/assert on a
`Bool(Scalar(true))` never splits state (`flow.rs` only filters on mixed
vectors), and an assert on an all-true vector is one pass over the mask.

Guards can be removed later by an explicit `remove_guard` rewrite, with the
justification recorded in the instruction. That keeps "we became confident" as a
visible, auditable step rather than a silent assumption.

### Three categories of rewrite, kept explicitly separate

- **A — provably sound structural**: inline, promote_cell, hoist, phi_to_select,
  merge_blocks, fold, dce. Mechanically verified.
- **B — guarded specialization**: assume_eq, resolve_call. Sound by construction
  because of the inserted check.
- **C — search-level assumptions**: e.g. "prune lanes where the player dies",
  "the object list always has exactly one entry". These are *not*
  semantics-preserving; they are justified by what the search is for. They need
  explicit human sign-off, a written justification in the instruction, and a
  runtime assert. Never let one of these masquerade as category A.

## 4. Proposed rule set

Nine rules. Rules are either **pointed** (carry a specific location) or
**canonical bulk** (no location; fully determined by the program; idempotent).
Bulk rules keep the recipe file short without reintroducing "an optimizer" —
their effect is deterministic and their verification is trivial.

Small IR additions needed: `Select { cond, if_true, if_false }`,
`Assert { cond, message }`, `IsClosure { value, fun_def } -> Bool`,
`GetCapture { closure, index }`. Plus pointer equality via `BinaryOp::TwoEqual`.

### Stage A — housekeeping (bulk)

**`fold`** — constant-fold instructions whose operands are constants (using the
interpreter's own `op.rs` evaluator, so semantics cannot drift); collapse phis
with identical or single incoming values; `Select` with a constant condition;
`ConditionalBranch` on a constant → `UnconditionalBranch`.
*Verify:* re-derive independently.

**`merge_blocks`** — merge a block into its unique predecessor when that
predecessor's only successor is it. Must OR `hint_normalize`.
*Verify:* re-derive.

**`dce`** — remove instructions with no uses and no side effects, and unreachable
blocks, fixing phis in survivors.
*Verify:* removed set has no uses and no effects. **Note**: the old `dce.rs`
classified `GetField/GetIndex { create_if_missing: true }` as pure. It is not —
it mutates the receiver table. Fix that.

### Stage B — kill the calls

**`assume_eq { at, replace, with, reason }`** — the workhorse. Insert
`Assert(replace == with)` at `at`, then replace every use of `replace` dominated
by `at` with `with`. `with` is either an existing name that dominates `at`, or a
literal (the applier materialises the constant).
*Verify:* the assert is present and well-formed; `with` dominates `at`; only
dominated uses were replaced.
*Sound by construction.*

This single rule covers constant propagation, **pointer/alias specialization**
(turning alias analysis into syntactic equality — this is the trick that avoids
`heap_elimination.rs`), value specialization, and loop trip-count pinning.

**`resolve_call { at, fun }`** — turn a dynamic `Call { closure, args }` into
`CallResolved { fun_name, captures, args }`. Inserts `Assert(IsClosure(closure,
fun))` and `GetCapture(closure, i)` for each of the callee's `capture_ids`.
*Verify:* the assert exists; the capture list has exactly `callee.capture_ids`
arity and is built only from `GetCapture` on the asserted closure.
*Sound by construction.*

This is essential — the old `call_resolution.rs` refused any closure with
captures, which is most of Celeste (`obj.move`, `obj.is_solid`, `obj.collide`
all capture `obj`). It also inferred the target by stripping `_<digits>` off
the mangled `GlobalId`, which silently collides. We name the target explicitly.

**`inline { at }`** — splice a `CallResolved` callee into the caller: split the
host block, α-rename the callee, map `Label::entry()` to the spliced entry,
convert each `Return v` into a branch to the continuation contributing to a phi
bound to the call's original name.
*Verify:* re-derive the splice; Tier 0 catches the rest. Known gap in the old
implementation to not repeat: it did **not** fix up phis in the *successors* of
the split block, and it emitted no phi (leaving the result undefined) when the
callee had zero return points.

**Stage B alone gets you to "no `Call` in the frame body."** Expect maybe 1.5–2x
— it removes the per-call constant (nested worklist, accumulator maps, guards,
`LocalEnv` construction, `outer_local_envs` cloning, profiler nodes) but not the
fragmentation. It is a prerequisite for everything after.

### Stage C — kill the memory

**`promote_cell { ptr }`** — mem2reg for the single cell denoted by the SSA
pointer `ptr`. Replace `Load(ptr)` with the current SSA value, `Store(ptr, v)`
with a redefinition, inserting phis; load once where `ptr` is defined and store
back at every `Return` (this is what preserves the frame-boundary observation).

*Verify:* (i) every use of `ptr` is a `Load(ptr)` or a `Store { target: ptr }`;
(ii) **pointer canonicality** — no other instruction in the function can produce
a pointer to the same cell. Concretely: the function contains exactly one
`GetField(recv, name)` per `(recv, name)` pair, exactly one `GetGlobal(name)`
per name, and all `GetIndex` indices are constants — checked globally. Once
`assume_eq` has CSE'd duplicate accessors, this is a cheap syntactic check.

This is the hardest verification obligation in the set and deserves the most
care. It is also where the old code went off the rails (2,411 lines of shape-
directed SSA-over-heap with no dataflow merge). The reason it is tractable here
is ordering: **inline first, specialize pointers first, then promotion is
local and syntactic.**

Same rule promotes both Lua locals (`Alloc` cells) and object fields
(`GetField` cells) — nice.

### Stage D — kill the branches

**`hoist { at, to }`** — move a pure, total instruction into a dominating block.
*Verify:* the instruction is in an explicit speculatable whitelist (constants,
`UnaryOp`, `BinaryOp`, `Select` — nothing that can error, allocate, or mutate);
its operands dominate the destination.

**`phi_to_select { block }`** — collapse a diamond into selects. Precondition
(deliberately narrow, so the check is trivial): `block` has exactly two
predecessors; both are empty blocks whose only terminator is
`UnconditionalBranch → block`; both have exactly one predecessor `H`; `H`'s
terminator is `ConditionalBranch { cond, →P1, →P2 }`. Then every phi in `block`
becomes `Select { cond, v1, v2 }` and the four blocks merge into one.
*Verify:* the shape check plus re-derivation.

Decomposing if-conversion into `hoist` + `phi_to_select` (rather than one
monolithic `if_convert`) makes each half trivially checkable — the arms are
emptied first, then the shape is degenerate.

Constraint to enforce: **`Select` on `Value::Pointer` is illegal** unless both
sides are the same pointer. Pointer-valued phis must be eliminated by
`assume_eq` first.

### Stage E — kill the loops

**`peel { header }`** — peel one iteration of the loop at `header` (clone the
body, redirect the back edge, fix phis). Combined with `assume_eq` (pin the trip
count / the induction variable) and `fold` (the exit test becomes constant), this
gives bounded unrolling.
*Verify:* re-derive the clone; Tier 0 for the phi bookkeeping.

Loops that need this in room 1:
- `foreach`'s `for i=1,32767` with `#tbl < i` break — trip count 1 once
  `#objects` is pinned.
- `obj.collide`'s `for i=1,count(objects)` — trip count 1.
- `move_x`/`move_y`'s `for i=start,abs(amount)` — **data-dependent**. Needs a
  guarded bound: `Assert(abs(amount) <= B)` with B from the Step-0 measurement,
  then unroll B times with predication. This is the main source of kernel size,
  so the measured B matters a lot.
- `del`'s `for i=1,32767` — only on the death path.

### Summary

| stage | rules | outcome |
|---|---|---|
| A | `fold`, `merge_blocks`, `dce` | housekeeping |
| B | `assume_eq`, `resolve_call`, `inline` | no calls |
| C | `promote_cell` | no intra-frame heap traffic |
| D | `hoist`, `phi_to_select` | no branches |
| E | `peel` | no loops |

Stage B is 5 rules (A+B) and is a complete, independently useful milestone.

### Things about room 1 that make this feasible

- The object list has **exactly one entry** (`player_spawn`, then `player`).
  The minimal cart has no `smoke`, and room (1,0) contains only a
  `player_spawn` tile. So the "fixed heap shape" assumption is nearly free.
- No metatables, no varargs, no multiple returns, no `while`, no generic `for`,
  no `goto`, no `pcall`, no coroutines. The frontend rejects all of them. Calls
  have exactly one result and fixed arity, so inlining never deals with value
  lists.
- `tile_flag_at` is already injectable as a native Rust builtin over the
  collision cache (`inject_tile_flag_at_builtin`), and native builtins return a
  single `(State, Value)` — they never split. The 26,883 `tile_at` and 11,255
  `tile_flag_at` nested interpretations per frame in the trace suggest this
  is not fully taking effect; worth checking early, it may be a cheap win on
  its own.

## 5. Tooling

Needed before the first rewrite:

- **Textual IR printer** with stable names, and a parser for it. Everything else
  depends on being able to read and diff programs.
- **`ir diff`** — show the effect of one rewrite as a readable textual diff.
- **Dominance-aware validator** (Tier 0). This is the single highest-value piece
  of the old code to rewrite properly; `cfg_validation.rs` is the weakest link
  and the reason the old bugs survived.
- **Observation digest + differential runner** (Tier 2), with `--why`: on
  mismatch, report the first diverging frame, lane and field.
- **`rewrite bisect`** — binary-search the instruction list for the first one
  that breaks the differential check.
- **Progress dashboard** — per rewrite: remaining `Call`s, `ConditionalBranch`es,
  blocks, `Alloc`s, `Load`/`Store`s in the frame body, plus measured frame-30
  wall time and lanes/KB. This is how we know we are converging on the target
  shape rather than just churning.
- The existing web CFG viewer (`serve/`) is worth keeping in spirit — repoint it
  at the rewrite sequence instead of the old pass pipeline. Do **not** check the
  generated JSON into git this time.

## 6. Open questions

1. **What is K?** (Step 0.) Decides whether the branch-free kernel wins on the
   existing interpreter or only after codegen.
2. **Codegen target** once the kernel exists: generated Rust, Cranelift, or a
   flat bytecode over reused `Vec<i32>` buffers? The last is the least work and
   probably gets most of the win.
3. **How is death modelled?** A lane whose player dies changes the object list
   structurally. Options: a category-C "dead lane" predicate that skips the
   structural change and filters at frame end, or keep death as real control
   flow outside the kernel. Needs a decision before Stage C.
4. **How many frames do we actually need? ~80.** That is well past the current
   wall of ~frame 43, and 1.45^37 is about 10^6 more lanes. So constant-factor
   wins alone do not close the gap — the forward/backward refinement from
   `plans/strategy.md` is required as well. But refinement needs a fast forward
   pass to be usable at all, and more importantly the target here is not just
   speed: heap operations tightly interleaved with execution are what make the
   search impossible to distribute across machines or run on a GPU. Getting to a
   pure, fixed-shape kernel is the precondition for that, independent of the
   local speedup.
5. **Cheap experiments — done, see below.** All three came back negative, which
   is itself the useful result: there is no cheap lever left, so the program
   shape is the only remaining one. `barrier`'s Vec-based COW `LocalEnv`/`Heap`
   is still unharvested and is the one remaining non-structural idea.

## 7. Results of the cheap experiments (2026-08)

**(a) Is the native `tile_flag_at` injection taking effect?** Yes. In the
January trace `tile_flag_at_72` and `tile_at_73` were the #2 and #3 hot spans
(1.71 s and 1.44 s self). In a fresh trace they are absent entirely. No action.
This also means the January trace's cost split was misleading and should not be
used.

**(b) Does adding `_hint_normalize()` calls help?** Marginally, and it costs
time. Best placement found (immediately after the six `btn()` reads in
`player.update`, i.e. right after the input fan-out and before the movement
logic), at frame 37: **7.03 GB → 5.58 GB (-21%) but 23.4 s → 25.1 s (+7%)**.
Lane counts identical. Two other placements (inside `obj.move`, end of
`player.update`) were neutral or much worse (32.6 s). A ~20% memory saving buys
about half a frame. Not pursued.

This experiment was previously impossible to run: `_hint_normalize()`, which is
supposed to be a pure merge hint with no semantics, crashed the interpreter
anywhere except its two existing sites. `State::gc` called `Heap::get` on every
reachable id and panicked on cells that `Alloc` had created but no `Store` had
filled yet. Fixed in `d622121`.

**(c) Is `union_diff_states`'s missing caching a problem?** Not yet — 4.3% self
time at frame 34. Recorded so we don't re-litigate it.

### Updated cost split (frame 34, fresh trace, 6.15 s total self time)

| span | self | share | n |
|---|---|---|---|
| `filter_branch` | 2.034 s | **33.1%** | 25,220 |
| `merge_groups` | 0.860 s | 14.0% | 102 |
| `gc` | 0.820 s | 13.3% | 20,381 |
| `dedup_state` | 0.582 s | 9.5% | 267 |
| `player.update_21` | 0.406 s | 6.6% | 27 |
| `shape_grouping` | 0.300 s | 4.9% | 102 |
| `union_diff_states` | 0.264 s | 4.3% | 68 |
| `filter_dedup` | 0.073 s | 1.2% | 172 |
| `filter_split_flr` | 0.050 s | **0.8%** | 819 |

By category: filter 35.1%, vectorize 28.3%, gc 13.7%, **cfg 22.9%**.

Two things to read off this:

- **Branch-induced filtering is ~40x the semantically necessary filtering.**
  `filter_branch` is 33% and removable in principle; `filter_split_flr` — the
  interval refinement that is the search actually fanning out — is 0.8%.
- **~75% of runtime is attributable to intra-frame fragmentation**
  (filter_branch + vectorize + gc), versus 23% actual interpretation. `gc` runs
  20,381 times in 34 frames; in a branch-free world it would run ~34 times.

### This sharpens the break-even for K

Actual interpretation is 1.40 s for 92,713 lanes at frame 34 = ~15 µs/lane,
against ~66 µs/lane total. A branch-free kernel removes essentially all of the
other 51 µs but grows the interpretation term by `K / (dynamic path length)`.
With a dynamic path of roughly 5k instructions, break-even is around
**K ≈ 22,000 instructions**. Under that, we win on the existing interpreter and
the memory win is free on top; well over it, the win only arrives after codegen.

## 8. K, measured (2026-08) — the answer is yes

`cargo run --release --bin measure_k` (see `src/block_coverage.rs`).

**K ≈ 10,700–11,900 instructions**, stable across seeds, 120–400 sequences and
45–80 frame horizons. Break-even is ~22,000. **The transformation pays for
itself on the existing interpreter, before any codegen.**

Room-load frames are excluded from the measurement: `load_room` scans 16x16
tiles and runs `foreach(types, ...)` per tile, ~2,800 block executions, which
swamps everything else and gives a misleading K of 105,322. It is also a
different heap shape, so it is a separate specialization by construction — a
nice independent confirmation of the shape-keyed design in §2b.

The breakdown by instruction kind matters more than the total:

| kind | share of K | fate |
|---|---|---|
| `heap` (Alloc/Load/Store/GetField/GetIndex) | 46.6% | removed by `promote_cell` |
| `terminator` | 19.4% | becomes a select, or vanishes |
| `arith` | 9.3% | survives |
| `const` | 7.5% | survives, mostly folded |
| `global` (GetGlobal) | 7.0% | becomes a fixed slot |
| `call` | 5.3% | removed by `inline` |
| `phi` | 4.8% | becomes a select |

**~78% of K is exactly what stages B–D are designed to eliminate.** The
irreducible arithmetic core is roughly **2,500–3,000 ops per lane per frame**.

### What that implies

Today: ~66 µs per lane per frame at frame 34.

| | ops/lane | ns per op-lane | µs/lane | speedup |
|---|---|---|---|---|
| today | 2,082 executed (fragmented) | ~32 effective | 66 | 1x |
| branch-free, existing interpreter, before `promote_cell` | 11,900 | ~1.5 | ~18 | ~3.7x |
| branch-free, after `promote_cell` | ~3,000 | ~1.5 | ~4.5 | ~15x |
| compiled kernel, SIMD over i32 lanes | ~3,000 | ~0.2 | ~0.6 | ~100x |

The staging is monotonic — every stage is a win on its own, so there is no
valley to cross.

### And the state is tiny

Dumping frame 30 and counting per-lane (`MaybeVector::Vector`) slots: the
entire per-lane state of room 1 is **17 values**.

```
freeze                     objects[1].dash_time        objects[1].p_dash
has_dashed                 objects[1].djump            objects[1].p_jump
objects[1].x               objects[1].grace            objects[1].spd.x
objects[1].y               objects[1].flip.x           objects[1].spd.y
objects[1].dash_accel.x    objects[1].dash_accel.y
objects[1].dash_effect_time
objects[1].dash_target.x   objects[1].dash_target.y
```

`rem.x`/`rem.y` are absent because `make_state_abstract` widens them to a
scalar interval at each frame boundary. The other 263 heap cells are structure
(107 pointers, 70 closures, 21 object tables, 13 builtins) or per-state scalars,
all shared across lanes.

So the frame function is a pure map on **17 i32s** (68 bytes/lane), against
~14,000 bytes/lane of measured peak RSS — a ~200x transient blowup. Stored
densely, frame 40's 948k lanes are 64 MB rather than 29 GB. Memory stops being
the binding constraint somewhere around frame 60; time becomes it again.

This is also, not coincidentally, almost exactly the state the old hardcoded
Rust implementation used — except here it would be *derived from the Lua*
rather than transcribed by hand.

### Consequences for the plan

- Go. Build the rewrite system.
- `promote_cell` is the highest-value single rule: it is 46.6% of K, and
  separately it wins back the ~1.9x memory regression from dropping mem2reg.
  Doing it second (after the trivial rules prove the pipeline) still looks right.
- The endgame should target **flat SoA i32 lane arrays plus a compiled kernel**,
  processed in chunks. Not "run the branch-free CFG in the existing
  interpreter" — that is only the intermediate checkpoint that keeps
  differential verification cheap.
- Known small over-count: the frame driver CFG and the button-reset CFG are both
  named `__main`, so `__main::__entry` is counted twice (~8% of K). Harmless at
  this precision.

## 9. Stage D, surveyed (2026-08)

Stage D above proposed `hoist` + `phi_to_select` against a *diamond*. Counting
what the program actually contains says the diamond is the rare case:

| shape | count | arms fully speculatable |
|---|---|---|
| triangle (`cond_br H → A, J`; `A → J`) | 145 | **84** |
| diamond (`cond_br H → T, F`; both `→ J`) | 12 | 0 |

The triangle is what Lua `and`/`or` compiles to, which is why it dominates.
Speculatable here means the arm contains no `call`, no `store`, no
`create_if_missing` and no `assert_closure`. Arms are short: median 2
instructions, max 24, 315 instructions over all 84.

33 of the 84 are in `player.update_21`, which is the hottest function in the
profile. The rest trail off: `spikes_at_74` 8, `obj.collide_49` 7,
`fake_wall.update_37` 5.

### What this changes about the design

**One rule, not two.** With arms this short and single-entry, `hoist` buys
nothing: `if_convert { join }` can move the whole arm into the head block and
rewrite the phis in one step, and the precondition is still a local shape check.
`hoist` was only needed to empty out arms so that `phi_to_select` could see a
degenerate shape; there is nothing to empty.

**Target the triangle.** `%p = phi [H: %x, A: %y]` where `A` is the arm becomes
`%p = select %c, %y, %x` (operands ordered by which target `%c` selects). `J`
then has one predecessor and `merge_blocks` absorbs it.

**`Select` is partial, and that is the interesting part.** The value
representation has one `HeapId` per pointer and one type tag per value, not per
lane, so `select` can only produce a value when both sides are numbers, bools or
intervals - or are literally the same value. Lua's `and`/`or` returns operands
rather than booleans, so a fair number of these 84 will be selecting between a
bool and a number and will fail.

That is fine and is the intended workflow: suggest all 84, apply them one at a
time, and let the differential run reject the ones that cannot be represented.
The recipe then records exactly which sites are convertible, which is a fact
about the program worth having written down. What it must not do is silently
produce a wrong value - so `select` on mismatched representations is an error,
not a widening.

### Order of work

1. `Select` in the IR, its interpretation, and its handling in `vectorize` /
   `StateShape`.
2. `if_convert { join }`, with an independent verifier: re-derive the shape from
   the *before* program, check every moved instruction is on the speculatable
   whitelist, check each `select` was built from the phi it replaced.
3. `rewrite suggest if-convert`, replacing the throwaway script that produced
   the table above.
4. Measure. The thing to watch is the **state count arriving at the frame
   boundary**, not the `filter_branch` share - see BENCHMARK_DATA.md, where
   merging at the frame boundary is 60% of runtime and the frame itself is 39%.

### The speculation hazard, stated honestly

Executing an arm on lanes that would not have entered it is safe for the *value*
- the select discards it - but not necessarily for *execution*. If the arm reads
a field that is nil in a state that would have skipped the arm entirely, the
interpreter raises a type error where the original program had none.

The whitelist does not rule this out; only `Alloc`, constants and the pure
operators are total, and restricting to those would exclude almost all 84 arms
(389 of the moved instructions are `load` and 129 are `get_field`).

So this is accepted as a loud failure rather than proved away, which is the same
bargain `assert_closure` makes for `inline`: the interpreter's own type checking
is the guard, and a mis-speculated arm aborts the search instead of computing
the wrong number. Differential verification covers 15,250 lanes over 30 frames,
which is evidence but not proof.

## 10. What actually blocks a call-free frame (2026-08)

Stage B converged at 183 inlines with 305 calls left. Classifying them says the
remaining work is not what section 4 assumed.

| callee expression | count | what it is |
|---|---|---|
| `load (get_global "n")` | 200 | native builtins |
| `load (get_field %o.n)` | 72 | method dispatch |
| closure defined in another block | 33 | needs cross-block candidate detection |

### The 200 "global" calls are mostly not a problem

They are calls to **native builtins**, which is why `global_closure_map` does
not resolve them - there is no Lua body to splice. `error` (54) and `__print`
(54) are on death and debug paths. The rest are `mget` 17, `max` 16, `min` 16,
`__array_table_drop_last` 13, `flr` 10, `tile_flag_at` 4.

A builtin is already a leaf operation, so leaving it as a call costs nothing at
run time. It costs something at *rewrite* time, because `if_convert` rejects any
arm containing a `Call`, including `min`. Allowing a whitelist of pure builtins
to be speculated would need the same guard structure `inline` uses - an
`assert_builtin` - and would be worth doing only if such arms turn out to be on
the branches that matter.

### The 72 method calls are blocked by captures, not by dispatch

This was the surprise. `inline::apply` never cared how the closure was obtained -
it takes a call instruction and a callee name, inserts `assert_closure`, and
splices - so `get_field` dispatch would work as-is. Only `candidates` restricts
itself to globals. Pointing it at a method by hand gives:

    inline: obj.collide_49 has 1 capture(s); only capture-free callees are supported

All seven `obj.*` methods capture exactly one cell:

    obj.is_solid_47  obj.is_ice_48  obj.collide_49  obj.check_50
    obj.move_51      obj.move_x_52  obj.move_y_53

The captured cell holds the object itself. `init_object` writes `local obj = {}`,
and because Lua locals captured by a closure are mutable boxes, the frontend
allocates a cell. Inspecting one: `%784 = alloc`, then a **single**
`store %784 <- %785`, then only `load %784` and `store_closure ... [%784]`. The
box is never rewritten.

### So stage B needs two things, in order

1. **`promote_capture { fn, index }`** - turn a captured *cell* into a captured
   *value*. Precondition, checkable over the whole program: every
   `store_closure ... <- F [%c]` has `%c` an `alloc` that is stored exactly once
   and never stored again, and inside `F` the capture is only ever `load`ed.
   Transformation: each creation site captures the stored value instead, and in
   `F` every use of `%x = load %cap` is replaced by `%cap` (the IR has no copy
   instruction, so this is a substitution, not an insertion).

   Note this is a whole-program rule keyed by the callee, because one `FunDef`
   is shared by all 32 closures made from it - they must all change together.

2. **Captures in the guard.** `AssertClosure` grows a `captures: Vec<LocalId>`
   asserting the closure's captured values equal those locals, and `inline`
   binds `capture_ids[i]` to `captures[i]`.

   Where does the call site get a local holding the capture? Not from the
   arguments - these methods have no `self` parameter, because they close over
   `obj` instead of taking it:

       obj.check = function(type,ox,oy) return obj.collide(type,ox,oy) ~= nil end

   compiles to `obj.check_50(%2, %5, %8) captures [%10]`. It comes from the
   *receiver of the field access*. Every such call is `%f = get_field %o.name`
   then `call (load %f)`, and `%o` is the object the closure captured - it has
   to be, because `obj.collide = function ... end` assigned the closure onto
   that same object. So the guard is

       assert_closure %12 is obj.collide_49 with captures [%9]

   where `%9` is already the receiver in `%11 = get_field %9.collide`. That
   identity is an assumption, which is exactly why it is asserted rather than
   proved.

   Before (1) this does not work, because the capture is a *cell pointer* and no
   local at the call site holds it - only its contents. That is why the order
   matters.

Both stay within the existing soundness story: the guess about what a closure is
and what it captured is asserted at run time, not proved.

### Then stage D, and only then stage E

`if_convert` is built and correct but cannot touch a branch whose arm calls or
stores, which is every branch that matters - the `and`/`or` triangles it *can*
take were 4.6% of splits. Removing the calls is what makes the interesting arms
speculatable. Section 9's whitelist and verifier need no changes for that; the
candidate set simply grows.

The loops (`obj.move_y_53`'s `for_body_start` is 5% of splits on its own) are
stage E and come last, because peeling multiplies code size and is much cheaper
to verify against a program that is already call-free.

### One thing that will not go away

The two largest splitting sites are the `btn` reads, 26% of splits between them,
and they are the search's own branching factor rather than anything a rewrite
can remove. Section 9's measurement and the lane-expansion experiment
(BENCHMARK_DATA.md) together say what to do about them: nothing, until the frame
is otherwise branch-free. At that point - and only at that point - fanning the
inputs out across *lanes* instead of states leaves one state per frame and no
merge at all. That is the payoff the whole sequence is aimed at, and it is worth
being explicit that none of the intermediate stages collect it.
