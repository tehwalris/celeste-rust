# What is actually in a traced frame's graph (2026-08-25)

Philippe's prior: a frame of this game has very little logic and very
few outputs, and when the graph says otherwise something is wrong. This
is a node-by-node audit of the checked-in flat kernel for room (1,0)
shape 1 (the player alone in the room), done by parsing the generated
Rust (`/tmp/kan.py`, a 150-line script) rather than by adding a
diagnostic to the test suite. No code has changed for it.

## The outputs

Shape 1 has FOUR output shapes, and they are the four things that can
happen to the player in a frame:

| outcome | what it is | per-body cells | bodies emitted | floor |
|---|---|---|---|---|
| 3 | player still alive in the room | 17 + key | 48 | ~48 (x, y, spd, dash state genuinely follow the buttons; x2 for the rem fork) |
| 2 | room exit: the next room's spawn state (fruit, spring, spring, player_spawn) | `delay_restart`, `freeze`, `will_restart` + key | 48 | ~2 |
| 1 | death: objects list empty, `delay_restart` set | `freeze`, `has_dashed` + key | 37 | ~2-4 |
| 0 | the respawn object alone | key only | 1 | 1 |

The floors are from the semantics, and T12 measured the same thing
numerically in 2026-08-23 ("The variant floor": death and exit at 2,
emitted 24, alive at its floor). So: **the outputs are few, as
expected; the BODIES are not, and the excess is entirely in the two
outcomes where the buttons should not reach the output at all.**

## The nodes

13,973 nodes in the kernel (14,752 `let`s including masks and loads).

| what | nodes | share |
|---|---|---|
| boolean: `and` 3,477, `or` 2,078, `not` 930, bool-select 263 | 6,748 | 48% |
| comparisons (`gt/le/eq/lt/ge`, `tile_flag_at`) | 1,231 | 9% |
| numeric selects (branch merges) | 2,821 | 20% |
| numeric arithmetic (`add/mul/sub/max/min/div/rem/neg/abs/flr`, `mget`) | 437 | 3% |
| interval ops incl. the two forks | 46 | 0.3% |
| row-key hashing (`zw_mix1/2`, `zw_bits_*`) | 2,678 | 19% |

So the game's arithmetic is **437 nodes**. Comparisons are another
1,231. Everything else - 88% of the graph - is guard algebra, branch
merges, and hashing. By cone: 2,683 nodes feed ONLY the row key,
1,141 feed only `live`/`ok`, and the rest are shared.

The same proportions hold for shape 0 (6,582 nodes, 68 bodies, the
room (2,0) spawn state this set reaches through the exit); shape 2 (54
nodes, 2 bodies) is the respawn animation and is as small as it should
be.

## Why death needs 37 bodies: traced to the node

`freeze` in the death outcome, for any dash-pressed assignment, is

```
freeze' = sel(dashing, freeze, sel(started_dash, 2, freeze))
```

which is right: `if this.dash_time>0 ... else ... if djump>0 and dash
then ... freeze=2`. The 36 dash-pressed bodies differ ONLY in the node
for `started_dash`, and it is emitted as

```
n3054 = (n3049 & !eq0(n3027)) | (n3049 & eq0(n3027))      -- ≡ n3049
n3049 = (n3044 & !eq0(n3028)) | (n3044 & eq0(n3028))      -- ≡ n3044
n3044 = (n3039 & gt(n3028,0)) | (n3039 & le(n3028,0))     -- ≡ n3039
```

three levels of `(A & p) | (A & ¬p)`. That is the tracer merging the
dash block's direction arms (`if input~=0 then if v_input~=0 ... else
... elseif v_input~=0 ... else ...`), every one of which sets
`freeze=2`, so the merged guard is `started & (arm1 | arm2 | arm3)` and
the arms partition. Per button combination the atoms (`input`,
`v_input`) are different nodes, so 18 direction combos x 2 forks give
36 structurally distinct trees for one function.

**The BDD already proves all of this.** Complementary comparisons are
one variable (`bdd.rs:270`), so `simplify` sees `n3054 ≡ n3049 ≡ n3044
≡ n3039`. It counts them under `Stats.mergeable` and does not rewrite,
by the policy in "Why only constants and atoms": two forms of one
concrete function can approximate differently in Kleene, and the
representative might be the less decided one. T12 recorded 4,357 such
equalities on one outcome and deferred the decision until the traced
path was in the search. It is in the search now.

## CORRECTION (2026-08-26): the claim below is WRONG, and the gate caught it

The proof in this section writes `N = F(M, v)` and lets `M` range
independently of `v`, which fails when `v` shares atoms with `M`.
Counterexample: `A = (a AND NOT a) OR (d AND e)` (concretely `d AND e`),
`N = A AND (d AND e)`: at `a` unknown, `d = false`, Kleene gives
`N = false` (decided) while `A` is unknown - `false AND unknown = false`
MANUFACTURES decidedness, so an ancestor is NOT always at-least-as-
decided. The general in-cone merge was implemented and
`simplifying_preserves_what_the_graph_evaluates_to` failed on it. What
IS sound, and is implemented (e960ee8), is the cone-free COMMON-FACTOR
collapse: `OR of (C AND P_i)` with a shared post-map factor set `C` and
the BDD proving `OR of (AND P_i)` a tautology rewrites to `AND C`. See
`plans/graph-simplify.md` for the proof and the measured result (-28%
of all generated lines, -83% of room-20 boolean nodes).

## The precision worry is unfounded in the case that matters (WRONG - see above)

**Claim.** If boolean node `N` is proved concretely equal to node `M`
and `M` lies in `N`'s own cone, then on every lane `M` is at least as
decided as `N`. Substituting `N := M` never loses a decided lane.

**Proof.** Write `N = F(M, v)` for the other cone inputs `v`. The
Kleene connectives (`zb_and`, `zb_or`, `zb_not`, and `zsel_b` with a
known condition) are monotone in the information order. On a lane
where `M` is unknown: `F(⊥, v) ⊑ F(true, v) = true` and `F(⊥, v) ⊑
F(false, v) = false` (concrete equality to `M`), so `F(⊥, v) = ⊥` -
`N` is undecided wherever `M` is. Where `M` is known, `F(M, v) ⊑ F(M,
v_concrete) = M`. Either way `N ⊑ M`. ∎

`(A & p) | (A & ¬p) ≡ A` with `A` in the cone is exactly this case;
the non-cone case (two unrelated subgraphs proved equal) stays
counted-not-applied, as today. The one non-monotone primitive is
`zsel_b` with an UNKNOWN condition, which reads `val` regardless; such
a lane is in no outcome's `live` anyway - see the open question below.

## What to do, and what it should produce

1. `bdd::simplify`: when a node's BDD reference was first produced by a
   node in its cone, map to that node. Cone membership by a bounded DFS
   from the candidate's args at the moment of the `seen` hit (a few
   thousand hits x a few thousand steps). Document the claim above
   next to the existing policy text; add a test of the
   `(A&p)|(A&¬p)` shape and a property test over random three-valued
   assignments asserting the merged form is never less decided.
2. Regenerate room (1,0)'s set and report per outcome: bodies, and
   nodes, before/after. Prediction, stated so it can fail: death and
   exit drop from 37/48 bodies to single digits; alive stays ~48
   bodies but its `freeze`/`has_dashed`/`dash_accel`/`dash_target`
   fields go from 37 distinct nodes to 2; total boolean nodes fall by
   a third or more. Gates: `traced_kernels_are_current` regenerated,
   `traced_kernels_reproduce_the_interpreter`,
   `a_traced_frame_agrees_with_the_oracle`, a 30-frame
   `CELESTE_COMPILED_FORWARD=check` run, all under `./safe-run.sh`.
3. Then re-measure the flat-fork size regression (2.13x on room (2,0)),
   because the duplicated compute under each fork configuration is
   largely this same unmerged guard algebra.

## Decided with Philippe (2026-08-25)

* The select condition becomes the LOCAL branch decision, not the path
  guard. Selecting on the guard "unnecessarily selects the false branch
  in cases where the value is entirely irrelevant" - on lanes outside
  the merged state's guard the value is never read, so the only thing
  the select has to get right is which side, and the branch decision
  says that. Implemented as `State::path` + `Merged::cond`; merges are
  paired siblings-first so the separating decision exists.
* The BDD subterm merge is NOT done yet. Measure what the select change
  alone does to the graph first.
* Side idea, parked: some booleans are known never to be UNKNOWN at
  runtime (constants, comparisons of always-known numbers, anything
  built only from those). Propagating "cannot be unknown" down the
  graph marks whole subgraphs where two-valued reasoning is exact and
  the Kleene precision caveat does not apply at all - more aggressive
  rewriting is safe there. Worth doing once the two-valued/three-valued
  boundary is a property the graph carries rather than an argument.

## Open question found on the way (verify, do not assume)

A lane whose branch condition is UNKNOWN at runtime satisfies neither
`zb_holds(c)` nor `zb_holds(!c)`, so it is `live` in no outcome.
`Run::step` counts `declined = live & !ok` only; a lane live nowhere
would vanish silently. Either no UBool input can reach a branch
condition (then say where that is guaranteed), or the frame is missing
the assertion `OR(take over outcomes) | declined == ALL`. The
assertion is cheap and is what the "never deopt silently" doctrine
asks for; it belongs in the emitted `frame` regardless of the answer.

## Row-key hashing is 19% of the kernel

2,678 nodes compute the per-body 128-bit row key: the chain is
button-independent in its prefix but per-body in its suffix, and 134
bodies x ~20 mixes is most of it. Inherent to keying at write time per
body; not today's problem, noted so the number is on record.

## Room (2,0): the explosion is a fork CROSS-PRODUCT over independent objects (2026-08-25)

Generated with the post-select-fix emitter. 36 shapes, **909,660 nodes /
1,633,094 lines**. Three 14-object shapes hold 52%: shape 3 (190,915
nodes, 7,965 bodies), shape 8 (175,039 / 4,382), shape 4 (107,981 /
3,990).

Shape 3 has 14 objects and **8 forks** - 4 moving objects x (x, y), each
on a widened `rem` interval. Its worst outcomes:

| outcome | bodies | = |
|---|---|---|
| 9 (death: freeze/will_restart/delay_restart) | 3,584 | 14 buttons x 2^8 forks |
| 10 (exit) | 3,584 | 14 x 256 |

**The per-cell fork cones say the product is nearly all waste.** Outcome
9 has ~275 output cells; the cone histogram is `{0 forks: 272, 8 forks:
3}`. So 272 cells do not depend on any fork (written once at
`acc_init`), and only THREE - the death flags - depend on all 8. Yet the
emitter specializes the whole outcome over the full 2^8 cross-product,
emitting 256 bodies per button to capture the behaviour of 3 boolean-ish
cells.

Those 3 cells depend on all 8 forks because "did the room restart" =
"did the player die", and the player dies against ANY of the 4 moving
objects or its own fall: `will_restart = die_on_A(forkA) | die_on_B(forkB)
| die_on_C(forkC) | fall(forkP)`. That is a **disjunction of per-object
terms - a SUM of 4** - enumerated as a **product of 256**. Each fork
config writes the same 272 position values and differs only in the death
triple and its `live`/`ok` mask, so the boundary collapses 256 -> a
handful of rows at runtime; the 256 survive only in the emitted CODE.

Concentration, across the whole room's 283 outcomes: union of fork cone
is 0 for **177**, 2 for **94**, 4 for **8**, 8 for **4**. The blow-up is
those 12 death/exit outcomes in the big shapes, nothing else.

### The shape of a fix (NOT built, for discussion)

An outcome should not be specialized over the union of its cells' fork
cones. Options, cheapest first:
1. **Cone-0 cells cost nothing already** (acc_init), so the waste is
   purely the fork-dependent cells' compute + key + plumbing, duplicated
   2^union times. Emit the outcome body ONCE and specialize only the
   fork-dependent cells - a per-config tail over 3 cells, not 275.
2. **Keep the death disjunction symbolic over forks** rather than
   enumerating: `will_restart` stays a function of the `Frag` nodes,
   lowered to `zi_fork_flr` inline, one body. Turns 2^8 into the 4-term
   OR it already is. This is a per-cell "do not specialize this fork"
   decision, the inverse of the flat-fork default.
3. Per-object row assembly (product built at write time, compute kept as
   a sum) - the biggest change, matches the room-1 win's spirit.

Open number still worth getting: how many DISTINCT (will_restart, freeze,
delay_restart) value-triples the 256 configs actually produce - a `bdd`
distinct-reference count on the 256 roots. If it is small, option 2 is
clearly right.
