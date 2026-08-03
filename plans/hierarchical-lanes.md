# Hierarchically ordered lanes

Status: idea, noted 2026-08-03. Not scheduled - the complexity is real and the
flat-vector representation is very fast per operation. Recorded because two
independent measurements point at exactly the weakness it would fix.

## The idea

Today a state's lanes are an unordered flat axis: every vector cell stores one
value per lane, and any operation on a vector cell costs O(lanes).

Instead, impose a *variable order* on the lanes: pick a tuple of variables
(A, B, C, ...) and sort/organize the lane axis lexicographically by it, A
varying slowest, C fastest. Consequences, in increasing order of ambition:

1. **Run-length structure.** A's column is a handful of long runs; B's column
   is runs within each A-run; only the deepest variables look like today's
   dense vectors. An operation that touches only A costs O(#runs(A)) - the
   number of distinct-value segments - not O(lanes). Putting the per-frame
   button booleans at the top makes operations on them nearly free.

2. **Subtree sharing.** If, under A=false and A=true, the B and C columns hold
   identical value sequences, there is no need to store them twice: the two
   halves of the lane axis can share one physical vector. The representation
   becomes a tree (lanes = leaves, levels = variables) with shared subtrees.
   Sometimes it is cheaper to *keep* lanes that are not strictly needed so
   that the product structure (and thus sharing) survives - a deliberate
   trade of lane count for physical size.

3. **The limit case** is a decision-diagram representation (MTBDD-like): each
   cell is a function from the top variables to values, stored as a DAG with
   hash-consed nodes. Cells that do not depend on a button are constants;
   the measured reality is that only ~17 of 279 cells vary across lanes at
   all, and most of those depend on few inputs.

## Why the measurements point here

* **`expand` is the endgame's blocker and costs 9.6% of wall for two sites**
  (task #45 re-measurement): expanding duplicates every heap cell of the
  state. Under (2)/(3), expanding a button is *introducing a new top-level
  variable*: O(1) metadata, all existing vectors shared between the two
  halves. The lane tax that killed the dash package twice would not exist.

* **Merge/dedup processes 60-90x the surviving lanes at ~50 ns/row**
  (BENCHMARK_DATA.md "The merge machinery measured against its task"), and
  that throughput is within ~20% of what the flat formulation permits - the
  volume, not the constant, is the cost. Under a lane order, duplicate
  detection becomes cheap where it is currently expensive: bottom-up
  hash-consing detects converged subtrees once, in O(distinct subtrees),
  instead of re-hashing every row at every merge point. Two input histories
  that converge to the same state become two leaves with equal hashes - the
  89-90% duplicate rate suggests enormous sharing is available.

* **The natural variable order exists already**: the per-frame button
  unknowns. 6 buttons/frame, and `make_state_abstract` resets them at the
  boundary, so the tree depth stays bounded per frame; frame-boundary merge
  is where old top variables retire.

## Costs and open questions

* Per-op indirection vs today's dense, branch-free, cache-friendly column
  loops (3 ns/element hashing, ~1 ns/element copying). Runs and DAG nodes
  add branches; for the *deep* (fast-varying) variables a dense tail
  representation would have to be kept.
* Filtering (branch masks) fragments runs; how much structure survives a
  frame's worth of `filter_branch` is unknown. The dash arm's gates are
  scalar per fragment today precisely because splitting concretized them.
* Interaction with `filter_split_flr` interval refinement and with GC.
* This is a rewrite of the interpreter's innermost data model. Do not start
  it as a side project; if the endgame stalls on `expand` cost and merge
  volume simultaneously, this is the shape of the fix.
