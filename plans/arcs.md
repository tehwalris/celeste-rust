# The rotation graph: exact remainders without a rem ladder (2026-10-04)

Branch `arc-sets`. Philippe's direction after room (3,3): stop refining the
sub-pixel remainder rung by rung; track it exactly.

## The model (checked)

Per axis, one frame does `rem := rem + ox + 1/2; amount := flr(rem);
rem := rem - 1/2 - amount` (lua `move`), then steps the player
`|amount| + 1` pixels with a collision check per pixel (`move_x`/`move_y`; a
blocked step sets `rem := 0`, `spd := 0`). Mod 1 that is a ROTATION of the
circle by `ox`, and `amount` steps by one where the rotated value crosses the
wrap (+-1/2). So, for a fixed remainder-free state ("node") and input:

* the circle is cut at ONE point per axis (none if `ox` is whole) into two
  PIECES; inside a piece the frame does one discrete thing (the same target
  node) and maps the remainder by a rotation (no collision on that axis) or
  to the constant 0 (a collision);
* the applied `ox` is the speed at the player's move, which is NOT always the
  stored `spd`: a freeze moves nothing, an object updated earlier in the frame
  (a spring) may set it.

`rewrite arc-proto` (the reference engine, every piece checked at its middle
and corners) held this on room (1,0)'s gate through layer 28 (1.2M concrete
steps), freezes and the spawn included.

A path is real iff the start remainder lies in the intersection of its
guards pulled back through its rotations; the union over all paths is
pushed along the graph exactly, because "intersect with the guard, then
rotate" distributes over unions (`search::arcs`).

## The design

1. **Forward, remainder-free (level 0's forward, as today).** The rows are the
   nodes (rem widened to the whole circle). NEW: each emitted row carries, per
   axis, two intervals the kernel already computes - the remainder after
   `rem -= 1/2 + amount` (before any collision; "image") and the remainder at
   the frame's end before the boundary widening ("final") - recorded with the
   edge, not in the row (no key changes). The input remainder is the whole
   circle, so per edge:
   * guard: the image's width, at the end it touches (the piece below the cut
     rotates onto an interval ending at +1/2, the piece above onto one
     starting at -1/2; the whole circle when there was no cut);
   * action: a rotation by `image.lo - guard.lo`, or the constant `final`
     where `final` is a point and the image is not (a collision);
   * a frame with no player split on an axis (a freeze, the spawn): identity
     on the whole circle.
   Captured where the tracer forks `__split_by_flr` on the player's own
   `rem.x`/`rem.y` (the split value's cone holds that cell), as per-body
   extra roots ("transfer roots") the append step writes into an arc-edge
   record stream beside today's edges. More than one player split per axis in
   a frame (a moving platform carrying the player) is refused, loudly, for now.

2. **Backward, arc sets.** `W_t(n)`: the remainder rectangles (x arc times y
   arc, unions of them) from which node `n` at frame `t` wins by the horizon:
   `W_t(n) = U_e guard_e  n  action_e^-1(W_{t+1}(dst_e))`, a win edge
   contributing its guard. Over the recorded edges, from the wins back,
   frame by frame, only nodes whose successors' sets changed recomputed.

3. **Forward, exact, inside W.** From the start's remainder (a point),
   pushed along the edges, keeping only points inside `W_{t+1}(dst)`. The
   first win is the optimum for the node graph's precision; the path is the
   witness. Points, not arcs: the start is exact, and W keeps them few.

4. **The remaining ladder is over everything but the remainder**: the
   objects (`n` -> exact) and the held buttons (`h` -> exact), each rung a
   remainder-free forward filtered by the previous rung's (forward n W)
   nodes. The remainder is exact at every rung - no drift, no rem rungs.

## Built (2026-10-04)

* Recording: `CELESTE_ARC_EDGES=1` on any forward (`search::arc_edges`, 50 B
  per record beside the edge runs). Checked by `rewrite arc-check` (every edge
  has its record and back; sampled transfers probed against the reference
  engine inside and outside the guards): room (1,0) gate, and room (3,3) with
  exact objects f1-f80 (207M records, 34k probes, 0 disagreements).
* `rewrite arc-search --level-dir D --horizon H --marked-only`: the level's
  remainder-free BFS gives the nodes that can win at all and their deadlines;
  only edges between them are loaded (parallel, streamed, dense indices).
* `search::arcs::Region`: a set of the torus in CANONICAL form (y slabs, each
  with its x segments), built in one sweep from pieces; `pull` is the
  preimage under an edge. Equal sets are equal values.
* `search::arc_dp::backward`: dense CSR graph, a parallel pull per node,
  INCREMENTAL - a frame recomputes only the predecessors of nodes whose set
  changed and the nodes whose deadline starts; the rest share the set.
* `arc_dp::optimum`: ONE backward gives the optimum: the graph is the same at
  every frame but for the layers, which never bind on a walk from the start
  (k steps reach layer <= k), so `point in W_t(start)` means "a win within
  `H - t`" and the optimum is `H - max t`. The witness is a greedy walk inside
  W (never backtracks). Property-tested against per-horizon backwards.

Measured (quick profile, room (3,3), objects abstract `r0sxhn`):

| case | edges | load | backward | total | peak |
|---|---|---|---|---|---|
| synthetic win (19,23), h88 | 66M | 4.3 s | 1.6 s | 6.4 s | 5.6 GB |
| same, h110 | 324M | 33 s | 17.5 s | 55 s | 27 GB |
| real exit, h171 | 346M | 66 s (38 s reading 139 GB of records) | 5.7 s | 74 s | 29 GB |

The synthetic case answers f69, as the full object ladder does (level 1
refutes 68). Room (3,3) h171: REFUTED; h172: a win at f172 (the reference).
W's fragmentation stays small: median 1-6 rectangles per node, max ~300.

## Direction (Philippe, 2026-10-04)

Arcs are the better technique: optimize the implementation and make it the
ONLY remainder treatment (the rem rungs go). Not started yet. Known costs to
attack first: the arc records (50 B per edge, written before the marks know
which edges matter: 139 GB and a 37 s read for room (3,3)), the marks BFS and
graph build, and the objects/held ladder on top of exact remainders.

## Open

* W's fragmentation (rectangles per node) - the number that decides the cost.
* Time indexing of W: per node, the sets change with the frames left; stored
  only where they change.
* Rooms with moving platforms (the player moved twice in a frame).
