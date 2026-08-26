# Graph minimization campaign (2026-08-26)

Goal: make the generated kernels for rooms (0,0), (1,0), (2,0), all
three variants (traced/ladder/exact), genuinely minimal, WITHOUT
touching the input shapes (the constant-lattice fixpoint is fixed).
Only graph generation and simplification change. This file records the
mechanisms chosen, the proofs, and every judgment call, for review.

## Baseline (census @ 4768b8a)

Emitted `let` bindings (nodes + masks + row loads) per variant/room:

| variant/room | kernels | lines | lets |
|---|---|---|---|
| traced/room00 | 8 | 105,407 | 64,898 |
| traced/room10 | 3 | 24,455 | 13,691 |
| traced/room20 | 18 | 402,211 | 240,392 |
| ladder/room00 | 8 | 111,678 | 69,115 |
| ladder/room10 | 3 | 25,617 | 14,454 |
| ladder/room20 | 18 | 421,405 | 252,341 |
| exact/room00 | 8 | 46,302 | 19,003 |
| exact/room10 | 3 | 13,452 | 4,725 |
| exact/room20 | 18 | 185,040 | 69,872 |

Total 1,235,567 lines / 748,491 lets.

## Step 1: the boolean-guard collapse

### The audit's proposed rule is UNSOUND, and the gate caught it

`plans/graph-audit.md` proposed: if node `N`'s BDD reference was first
produced by a node `M` in `N`'s own cone, substitute `M` for `N`, with
a monotonicity proof ("A is an ingredient of N built via and/or/not
which never manufacture decidedness"). I implemented exactly that
first, and `simplifying_preserves_what_the_graph_evaluates_to` failed:
a node went from `Bool(Some(true))` to `Bool(None)` - the substitution
LOST a decided lane.

The proof's hole: it writes `N = F(M, v)` and lets `M` range
independently of `v`. When `v` shares atoms with `M` that independence
is fiction. Minimal counterexample:

    A = (a AND NOT a) OR (d AND e)     -- concretely = d AND e
    N = A AND (d AND e)                -- same BDD reference, A in N's cone

At `a = unknown, d = false`: Kleene gives `A = unknown OR false =
unknown`, but `N = unknown AND false = FALSE`. `false AND unknown =
false` MANUFACTURES decidedness, so an ancestor is NOT always
at-least-as-decided than a node built on it. The audit's "proof" and
the equivalent sketch in the task brief are both wrong in the same
step.

### What is actually safe: the common-factor collapse

The shape that dominates the measured redundancy does have a proof.
Implemented in `src/transpile/bdd.rs::factor_common`, applied inside
`simplify` (NOT in `Graph::fold`, whose contract is exactness in
Kleene - this rewrite is a refinement; and only the BDD knows
`gt`/`le` on the same operands are complementary).

Rule: `N = OR_i (AND (C_i ∪ P_i))` where every flattened disjunct
shares the same non-empty factor set `C` (compared by POST-MAP node
identity, so previous rewrites feed it), and the BDD proves
`OR_i (AND P_i) == TRUE` over the residuals. Then `N -> AND C`.
Dual for `And`-of-`Or`s with residual `== FALSE`. Covers
`(A&p)|(A&not p)` (structural or comparison complements), absorption
`A | (A&p)`, and n-ary arm partitions; cascades bottom-up.

Safety proof (in the code, on `simplify`): concretely `N = C AND
(OR_i AND P_i) = C` by distributivity + the tautology. For
decidedness: if `N` is Kleene-decided TRUE, some disjunct is all-true,
so every C-image is true. If decided FALSE, every disjunct has a
decided-false conjunct; if any is in C, `AND C` is false; if all are
residual, every `AND P_i` is decided false, concretizes to all-false,
contradicting the tautology (which the BDD proved over ALL atom
assignments - a superset of realizable ones). So `N` decided forces
the replacement decided-and-equal; `N` unknown needs nothing.

Gates: `the_merged_form_is_never_more_decided_than_its_ancestor`
(property test over tri-state assignments, compound ancestors),
`a_branch_remerge_collapses_to_its_own_ancestor` (3-deep chain incl. a
comparison-complement level, `merged == 3`), and the pre-existing
differential `simplifying_preserves_what_the_graph_evaluates_to`
(which is what killed the unsound version).

## Judgment calls

* The general cone merge stays OUT even where it might be fine in
  practice; `Stats.mergeable` keeps counting what it declines.
* `flatten` caps trees at 64 leaves - bigger trees are skipped, not
  risked. Nothing measured comes close.
* `Sel`-shaped booleans are not flattened through (a `Sel` leaf is an
  opaque conjunct); the BDD still handles it via its reference when it
  is a residual.

## Result of step 1 (committed e960ee8)

| variant/room | lines before | lines after | lets before | lets after |
|---|---|---|---|---|
| traced/room00 | 105,407 | 65,740 | 64,898 | 29,095 |
| traced/room10 | 24,455 | 16,773 | 13,691 | 6,649 |
| traced/room20 | 402,211 | 256,715 | 240,392 | 103,472 |
| ladder/room00 | 111,678 | 70,299 | 69,115 | 31,921 |
| ladder/room10 | 25,617 | 17,646 | 14,454 | 7,182 |
| ladder/room20 | 421,405 | 272,695 | 252,341 | 112,805 |
| exact/room00 | 46,302 | 36,305 | 19,003 | 9,855 |
| exact/room10 | 13,452 | 11,100 | 4,725 | 2,643 |
| exact/room20 | 185,040 | 145,708 | 69,872 | 32,838 |

Total 1,235,567 -> 892,981 lines (-27.7%); 748,491 -> 336,460 lets
(-55%). Boolean nodes in traced/room20: 148,746 -> 25,826 (-83%).
Bodies fell with the guards (mask lets in traced/room20 14,814 ->
8,214): two fork/button configurations whose guards collapse to the
same ancestor now dedup to one body.

All gates green at the checkpoint: 291 quick-suite tests including
traced/ladder/exact `*_reproduce_the_interpreter` (traced asserts
`missed_lanes == 0` and `traced_lanes > 0`), `room00/room20
_lattice_kernels_match_the_interpreter` (30-frame reachable-state-set
differential, any declined lane fails), all three `*_kernels_are
_current` fingerprints, `a_traced_frame_agrees_with_the_oracle`.

## Step 2: the stock-take, and two measured negative results

Post-collapse census of the traced sets (`let`s by op family):

| set | lets | bool | numsel | rowkey | compare | arith | mask | splat | other |
|---|---|---|---|---|---|---|---|---|---|
| room10 | 6,297 | 1,484 | 1,373 | 1,618 | 532 | 491 | 429 | 264 | 106 |
| room00 | 27,470 | 5,995 | 5,070 | 9,006 | 2,170 | 1,921 | 2,169 | 776 | 363 |
| room20 | 97,798 | 25,826 | 18,301 | 29,287 | 6,990 | 6,232 | 8,214 | 1,532 | 1,416 |

### Negative result 1: the Kleene-exact (dual-rail) merge is a no-op here

Implemented and MEASURED, then reverted. Per boolean node, the pair
(decided-true, decided-false) as BDDs over doubled atom variables,
following eval's three-valued semantics arm for arm; nodes with equal
pairs are interchangeable EXACTLY (associativity re-bracketings, De
Morgan re-spellings - things interning cannot see). Sound, and it
bought: room10 -7 lets, room00 -16, room20 -42 (~0.05%). After `fold`'s
canonicalization and the factor collapse, the surviving boolean layer
simply has one spelling per function. Dropped rather than kept: a
second BDD construction on every emit for 65 nodes across three rooms
is mispriced. (If it is ever wanted, note the runtime-zsel_b caveat:
`zsel_b` on an unknown condition picks by the arbitrary `val` bit, so
"exact" is exact only because every such lane is killed by the
`Known(cond)` validity conjunct and `zb_holds` masks unknown lanes -
the argument is in this file's history and the code in e960ee8's
successor diff, reverted.)

### Negative result 2: nested same-condition numeric selects

`sel(c, sel(c, x, y), z)` would collapse observable-equivalently to
`sel(c, x, z)`. Counted in the generated sets: room20 kernel17 has
2,689 numeric selects, ZERO nested same-condition; room10 kernel1
1,326/0. The tracer's merge discipline never produces the shape.

### What the remaining graph IS (cone split, room10 kernel1)

Of 5,192 value lets: key-only 2,412 (46%), key+mask 1,585, key+out
511, mask-only 653, out-only 31. Nearly half the kernel is the row-key
fold itself, and only 31 nodes feed an output without also feeding the
key - i.e. the key hashes exactly the non-constant outputs, as
designed. Textual-duplicate check: zero duplicate right-hand sides
(hash-consing is complete CSE). Dead-node check: the emitter only
emits `live` (root-reachable) nodes, so there is nothing dead to
remove.

### The remaining mass, category by category

* **rowkey (~30%)**: bodies x changed-cells Mix chains. Prefixes over
  agreed cells are shared by interning (the sort puts agreed cells
  first). The KEY DEFINITION is frozen - it is what the boundary
  dedups on and what check-mode compares - so the only lever is fewer
  bodies.
* **mask (~8%)**: three lets per body. Same lever.
* **bool (~26%) / numsel (~19%) / compare (~7%)**: the genuine
  path-guard lattice and branch joins of the frame - pairs
  `prefix & branch` / `prefix & other-branch` each feeding different
  selects. No two compute the same function (the BDD said so:
  everything provably-equal was merged or counted `mergeable`, and the
  remaining `mergeable` pairs are precision-relevant by construction).
* **arith (~6%)**: the game.

### The one big lever left: the fork cross-product (NOT taken)

Room20 shape 17 still emits 688 bodies; death/exit outcomes are
specialized over the full product of their fork cones even though only
the death triple depends on all forks (plans/graph-audit.md "the
explosion is a fork CROSS-PRODUCT"). Fixing it means changing the
EMITTER ARCHITECTURE (emit an outcome body once and specialize only
fork-dependent cells, or keep the death disjunction symbolic with
runtime `zi_fork_flr`) - reversing part of the flat-fork decision of
6a2672c. That is a design decision with runtime-shape consequences,
parked in the audit "for discussion", and it is Philippe's call, not a
peephole. The numbers above say it is worth roughly half of what
remains in the big room-20 shapes (rowkey+mask scale with bodies).

## Step 3 (2026-08-26): the fork cross-product RE-MEASURED - the lever is mostly spent

The section above (and the task built on it) claimed kernel17's ~600
bodies come from death/exit outcomes specialized over their full fork
cross-product, with the death triple constant under each outcome's own
guard. Before implementing guard-conditioned simplification, that
picture was verified against the checked-in `traced/room20/kernel17.rs`
(census @ 6f497be). It does not hold, in either half, and per the
task's own stop rule the fix was NOT forced in. What was measured:

### Where kernel17's 688 bodies / 607 value-groups actually are

The generated header states it outright: "per outcome they fall into
[96, 96, 96, 96, 2, 2, 2, 2, 96, 2, 5, 8, 8, 96] groups that write
identical values."

| outcomes | kind (KOut cells) | bodies | value-groups |
|---|---|---|---|
| 0,1,2,3,8,13 | ALIVE (17 player cells) | 96 each = 576 | 96 each - ZERO value duplication |
| 4,5,6,7,9 | death (freeze, has_dashed) | 8 each = 40 | 2 each (per dash button) |
| 10 | exit-ish | 8 | 5 |
| 11,12 | restart triple (will_restart, delay_restart, freeze) | 32 each | 8 each |

The mass is the six ALIVE outcomes: 24 button-reps x 4 fork configs,
and same-button/different-fork bodies differ in `flip.x`, `grace`,
`spd.x`, `spd.y`, `x`, `y` (verified on outcome 0 bodies 0/1 and
outcome 8 bodies 416/417) - genuinely fork-dependent player state, six
cells per pair, NOT the death triple. Both rem forks change the move's
outcome; a lane whose interval spans both floors takes both successor
rows. That is the search's branching, not waste.

### The triple is NOT guard-constant where it does vary

Outcome 11 decoded (per button, forks 0x3/7/b/f families):
`live = zb_holds(n9224)` with `n9224 = n8648 | n9221` - TWO merged
sub-events. Under the guard, `will_restart = n9227 = !n9221 | !n9224
== !n9221` and `delay_restart = zsel_n(n9224, n9229, 15)` with
`n9229 = zsel_n(n9221, r_c39, 15)` - i.e. the triple SELECTS which
sub-event fired, and `n9221`/`n8648` are fork-resolved die conditions.
Conditioning on `live & ok` does not make it constant; the four
value-groups per button are genuinely distinct. The 2026-08-25 audit's
2^8 cross-product (shape 3, 3,584 bodies/outcome) described the
PRE-flat-fork, pre-factor-collapse generation; `bits_of` cone-limiting
plus the e960ee8 collapse already reduced it to what is listed above.

### What guard-conditioning WOULD still buy (measured ceiling)

Whole traced set: 3,604 bodies, 3,395 value-groups, **209** redundant
bodies (kernel17: 81, kernel8: 39, kernel14/16: 21 each, rest single
digits). The mechanism is real at that scale: outcome 11's
within-family bodies 528-531 share fields AND `live` and differ in ONE
ok conjunct each - `n9228/n9232/n9234/n9236`, all
`zsel_b(n9224, n9225, nX)` with the outcome's own `live` as condition
and the same true-arm - so conditioning `ok` on `live` collapses each
family 4 -> 1 (outcomes 11/12: 32 -> 8 bodies; death outcomes
8 -> 2). But a redundant body already shares its values and key chain
and costs only ~5 mask/take lines, so the ceiling is roughly 1,000 of
340k generated lines (~0.3%), not "half of what remains in the big
room-20 shapes". That claim above is corrected: rowkey chains scale
with value-GROUPS (identical-value bodies intern to the same hash
nodes), and the groups are almost all genuinely distinct successors.

Soundness note for whoever picks up the 0.3%: conditioning `ok` on
`live` is the sound direction (`ok` is only observed where `live`
holds: `take = live & ok`, `declined = live & !ok`); conditioning
output cells on `live & ok` likewise (values only written where
taken); `live` itself must NOT be conditioned on anything.

## Judgment calls (complete list)

1. Factor collapse in `bdd::simplify`, not `Graph::fold` (exactness
   contract; comparison complements only visible to the BDD).
2. The audit's general in-cone merge REJECTED as unsound, with
   counterexample; only the factor shape is applied. `mergeable` still
   counts what is declined.
3. Kleene-exact merge implemented, measured (~0.05%), REVERTED.
4. Nested-same-cond select collapse investigated, pattern count is
   zero, not built.
5. `flatten` caps factor trees at 64 leaves (skip, not risk).
6. Fork cross-product de-specialization left to Philippe (design
   change, see above).
7. Row-key definition and FIELD_NAMES order untouched (search
   identity).
