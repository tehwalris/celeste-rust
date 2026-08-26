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
