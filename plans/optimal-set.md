# The optimal-path set as the search's output (2026-10-11, branch `optimal-set`)

Philippe's question: could the search save a set of states (with remainder
ranges, maybe edges or frame limits) that describes or tightly constrains
every winning path? How big would it be? Picking the one "nice" path for a TAS
(e.g. the fewest input changes) could then happen afterwards, on the fly.

Short answer: **yes, and it is tiny.** At the optimum H*:

- room (1,0) any% is 327 (node, remainder) points, 8 KB raw (3.5 KB zstd);
- room (4,2) is 41,668 points, 133 KB;
- room (6,2) 100% is 1,455 points, 16 KB.

The trees the sets came from hold 213M, 9.1M (level 0) and 345M (level 0)
states. One frame of slack multiplies the points by 4-33x and two frames by
12-194x. At H* + 2 the biggest set is still 1.6 MB raw (256 KB zstd).

A concrete search that runs the real game and is pruned only by the set
visits these states:

| room | at H* | at H* + 1 | at H* + 2 |
|---|---|---|---|
| (1,0) any% | 326 | 10.8k | 65k |
| (6,2) 100% | 3.8k | 57k | |
| (4,2) | 125k-134k | | |

The set never dropped a concrete state that W admits (a runtime guard,
below).

Decided with Philippe (2026-10-11):

- The search's OUTPUT is the abstract set (below).
- Path selection is a separate, later CONCRETE search constrained by it,
  with a preference.
- The full concrete DAG is not saved.

## What is measured: `rewrite optimal-set`

`rewrite optimal-set --level-dir D/levelNN --level SPEC --horizon H --slack K
[--concrete] [--collapsed] [--profile] [--write FILE]` takes the run's
settings flags and lives in `search::optimal_set`. It only measures; it
changes nothing in `arc_dp`. It does the following:

1. It loads the tree's rotation graph at H, runs the backward and reads the
   optimum H* off it (`arc_dp::{load, backward, optimum}`).
2. For h = H* ..= H* + K it reloads at h and computes **R_t(n)**: the
   remainders with which node n is reached at frame t from the start inside W
   and still wins by h:
   - R_0 = {start point};
   - R_{t+1}(m) = ∪_e push_e(R_t(p)) ∩ W_{t+1}(m), over the edges p -> m;
   - this is `arc_dp::reach`'s set.
3. At h = H* every point of R is on an optimal path of the level's model. A
   state reached at t that won in fewer than h - t more frames would make
   the start win before H*. At h > H* the set holds every path that wins by h.

**R is a finite set of POINTS.** R_0 is one point, and an edge maps a point
to a point (a rotation, or a constant on a collision). So R is computed
exactly, point by point. `arc_dp::reach` keeps canonical regions and widens
a set past 32 segments to W, which the objects ladder's filter can afford.
So its marks are a superset: room (4,2) level 0 at h73 marked 14,292 nodes
(260 sets widened) against the exact set's 11,453.

`--write` writes the "regions form" (below) raw, so its bytes and its zstd
size are real.

`--concrete` runs a breadth-first search over CONCRETE states:

- It uses the reference engine, every input and every `rnd` leaf, as
  `arc_dp::concrete_search` does.
- It starts from the start and runs EVERY layer to h, not just to the first
  win.
- It keeps a successor only if its (node, exact remainder) is in R_{t+1},
  and keeps each exact state once a layer.
- It keeps the parent links and then marks the states on a winning path.

It also counts the successors that are inside W but not in R. That must be 0,
because a concrete state inside W reached from the start is in R by
induction. A nonzero count fails the command (a runtime guard on R's
exactness). It was 0 in every run below.

`--collapsed` runs the same search inside the time-collapsed set: per node
its first and last frame and the union of its points (Philippe's "frame-limit
ints").

## Runs

These are the runs measured. All use the quick profile with 16 threads on
the shared machine.

| room | search | tree | wall, peak |
|---|---|---|---|
| (1,0) any% | `search --room 1,0 --to 101 --prefer tas/room_1_0_exit_frame_99.txt` (level `r0sx`, no level -1) | to f101 | 2:06, 14.3 GB |
| the arc gate | `search --room 1,0 --to 35 --win-at 9,101` (its `[gate]` lines identical to `gates/arc_room10_win9-101_h35.txt`) | | |
| (4,2) | `search --room 4,2 --level r0sxhn,r0sxh --level-minus-one 73,5 --to 73 --prefer tas/room_4_2_exit_frame_71.txt` | | 0:23 |
| (6,2) 100% | `search --room 6,2 --hundred --loading-jank 2 --level-minus-one 95,5 --level r0sxhf,r0sxh --to 95 --prefer tas/room_6_2_hundred_frame_93.txt` | | 5:09, 8.7 GB |

- Each search found its known optimum: 99, 33, 71 and 93. Each known route
  survived every pruning step.
- One tree to H* + 2 serves H*, H* + 1 and H* + 2. A level -1 or ladder
  filter at the larger horizon keeps a superset of the smaller one's
  states, and W at h only counts paths that win by h.
- For a ladder, the set is the LAST level's (`r0sxh`). The coarse level's
  own optimum is a bound (89 for (6,2) against 93), not the optimum.
- `optimal-set` itself: (1,0) took 34 s for the three horizons without
  `--concrete`. The others took seconds.

## The abstract set

The columns:

- "tree" is the states of frames 0..=h.
- "marks" is the remainder-free BFS's nodes (each can win by h with SOME
  remainder) and the edges loaded between them.
- "W support" is the nodes with a non-empty W at some frame, and the
  (node, frame) pairs.
- "set" is the nodes and (node, frame) pairs with R non-empty, the points,
  and the canonical segments.
- "edges" counts the (frame, edge) pairs that carry a point of R_t into
  R_{t+1}.

| room, h | tree | marks (nodes / edges) | W support (nodes / node-frames) | set nodes | set node-frames | points | segments | edges | bytes raw / zstd |
|---|---|---|---|---|---|---|---|---|---|
| (1,0) **99 = H*** | 212.6M (10.3G edges) | 8.82M / 214M | 1.68M / 3.66M | **319** | **319** | **327** | 327 | 1,031 | **8,092 / 3,529** |
| (1,0) 100 | 219.0M | 11.4M / 292M | 2.59M / 6.25M | 4,258 | 4,621 | 10,694 | 10,070 | 17,938 | 148,324 / 50,962 |
| (1,0) 101 | 225.3M | 14.5M / 390M | 3.67M / 9.91M | 11,655 | 16,583 | 63,367 | 57,899 | 71,939 | 636,438 / 160,606 |
| gate **33 = H*** | 92,683 | 2,853 / 24,713 | 1,717 / 2,343 | 189 | 189 | 263 | 263 | 394 | 5,104 / 2,123 |
| gate 34 | 131,957 | 5,803 / 60,505 | 3,874 / 6,217 | 405 | 588 | 988 | 972 | 1,407 | 15,054 / 4,905 |
| gate 35 | 187,387 | 11,576 / 128,240 | 7,952 / 14,169 | 916 | 1,494 | 3,086 | 2,946 | 4,151 | 38,988 / 11,674 |
| (4,2) `r0sxh` **71 = H*** | 14,145 (filtered) | 13,505 / 77,344 | 13,264 / 82,969 | **1,555** | **1,558** | **41,668** | 23,534 | 3,076 | **133,298 / 24,982** |
| (4,2) `r0sxh` 72 | 14,194 | 14,029 / 79,530 | 13,938 / 96,907 | 4,714 | 6,367 | 235,379 | 103,702 | 14,976 | 564,110 / 93,521 |
| (4,2) `r0sxh` 73 | 14,194 | 14,194 / 80,516 | 14,194 / 111,101 | 11,278 | 17,815 | 857,004 | 303,269 | 47,603 | 1,634,806 / 255,835 |
| (4,2) `r0sxhn` 71 (level 0) | 9.08M | 1.32M / 21.1M | 1.07M / 3.77M | 1,555 | 1,558 | 41,668 | 23,534 | 3,076 | the same set |
| (4,2) `r0sxhn` 73 (level 0) | 9.25M | 2.20M / 37.6M | 1.80M / 6.97M | 11,453 | 18,013 | 859,995 | 305,417 | 48,681 | |
| (6,2) 100% `r0sxh` **93 = H*** | 253,085 (filtered; level 0: 345M, 14.3G edges) | 73,297 / 542,895 | 63,920 / 384,508 | **492** | **492** | **1,455** | 1,376 | 762 | **16,102 / 5,962** |
| (6,2) 100% `r0sxh` 94 | 253,085 | 73,499 / 543,208 | 66,462 / 450,970 | 4,102 | 4,688 | 20,188 | 18,986 | 8,451 | 178,504 / 57,369 |
| (6,2) 100% `r0sxh` 95 | 253,085 | 73,556 / 543,307 | 69,443 / 520,413 | 12,033 | 16,959 | 129,778 | 112,805 | 39,287 | 827,760 / 202,168 |

What the table shows:

- **At H* a node of the set is at ONE frame**: node-frames = nodes in
  (1,0), the gate and (6,2); 1,558 for 1,555 nodes in (4,2). Frame bounds
  cost (almost) nothing there. With slack a node appears at 1.1-1.6
  frames.
- **R is far smaller than W.**
  - Room (1,0) at H*: W has non-empty sets at 1.68M nodes (6.3M segments
    over its node-frames); R has 319 nodes.
  - The backward knows which remainders win from everywhere; R keeps only
    what the start actually reaches.
  - That is why the output should be R, not W. At room (1,0) h99 the
    winning sets alone are 2.77M spans over 106k distinct sets.
- **The ladder's coarse level gives the same set at H* in (4,2)**: 1,555
  nodes and 41,668 points at `r0sxhn` and at `r0sxh`. Its spurious paths
  only start to show at H* + 2 (11,453 against 11,278 nodes).
- **Points per node-frame** at H*:
  - room (1,0): 1-2, so the path is nearly forced. 2-4 nodes a frame with a
    point each, i.e. a couple of free binary choices;
  - room (4,2): up to 880, at the exit. Frames 65-71 hold 33k of the
    41.7k points (the profile below);
  - room (6,2): up to 153.
- **Segments < points** where reached remainders are adjacent raw values:
  (4,2) 23.5k segments for 41.7k points; (1,0) none at H*.

### Layer profiles at H*

The profile columns, besides those of the table above:

- "live" is the marked nodes whose layer <= t <= deadline;
- "pts" is the set's points;
- "edges" are the node-level edges t -> t+1 that carry a point;
- "W segs" are the segments over W's nodes at t.

Room (1,0), h99 (`--profile`, every 10th frame and the end):

| t | tree layer | marks live | W nodes | set nodes | pts | edges | W segs |
|---|---|---|---|---|---|---|---|
| 0-23 | 1 | 1-11 | 1 | 1 | 1 | 1-2 | 1 |
| 24 | 24 | 30 | 6 | 2 | 2 | 2 | 78 |
| 40 | 228,801 | 93,125 | 1,148 | 4 | 4 | 12 | 3,322 |
| 50 | 1,191,326 | 544,793 | 4,452 | 2 | 2 | 8 | 7,418 |
| 60 | 2,199,846 | 162,404 | 18,610 | 4 | 4 | 16 | 82,532 |
| 70 | 5,214,680 | 1,777,715 | 96,935 | 4 | 4 | 16 | 140,404 |
| 80 | 4,199,878 | 86,166 | 26,978 | 4 | 4 | 16 | 46,954 |
| 90 | 5,017,847 | 314,192 | 179,705 | 4 | 4 | 8 | 226,297 |
| 98 | 6,918,778 | 49,814 | 49,814 | 4 | 4 | 4 | 49,814 |
| 99 | 6,771,972 | 2 | 2 | 1 (win) | 1 | | 2 |

At h101 (H* + 2) the set has 40-470 nodes a frame after the spawn and up to
2,396 points a frame (f100); the win node holds 673 points at f101.

Room (4,2) `r0sxh`, h71:

| t | tree layer | marks live | W nodes | set nodes | pts | segs | edges |
|---|---|---|---|---|---|---|---|
| 0-37 | 1 | 1-9 | 1 | 1 | 1 | 1 | 1-2 |
| 40 | 36 | 66 | 29 | 4 | 4 | 4 | 6 |
| 50 | 41 | 109 | 96 | 8 | 30 | 28 | 9 |
| 60 | 1,667 | 5,164 | 5,146 | 17 | 191 | 160 | 43 |
| 65 | 290 | 9,505 | 9,264 | 193 | 6,187 | 3,634 | 243 |
| 68 | 342 | 4,983 | 4,734 | 105 | 4,740 | 2,336 | 171 |
| 70 | 345 | 2,235 | 2,235 | 214 | 6,720 | 3,695 | 344 |
| 71 | 173 | 2 | 2 | 2 (win) | 928 | 174 | |

The (1,0) set at H* is a corridor two to four states wide. The set at
H* + 1 and H* + 2 fans out from frame 24 (the spawn): one frame of slack
lets most of the route vary.

## The concrete search inside the set

The concrete search inside the exact set (`--concrete`). "Frames run" are
reference-engine frames, one per (state, input class); inputs the frame
does not read are skipped as in `concrete_search` (`Covered`).

| room, h | states | on a winning path | links | frames run | wall (16 threads) | collapsed set: states |
|---|---|---|---|---|---|---|
| (1,0) 99 = H* | 326 | 326 | 1,047 | 3,995 | 2.2 s | 326 |
| (1,0) 100 | 10,797 | 10,797 | 36,326 | 139,547 | 14.7 s | 10,799 |
| (1,0) 101 | 65,381 | 65,381 | 224,696 | 856,587 | 70.9 s | 66,536 (+1.8%) |
| gate 33 = H* | 207 | 207 | 368 | 3,695 | 0.9 s | 207 |
| gate 34 | 796 | 796 | 1,665 | 15,011 | 1.2 s | 820 |
| gate 35 | 2,528 | 2,528 | 5,833 | 45,223 | 2.0 s | 2,618 (+3.6%) |
| (6,2) 100% 93 = H* | 3,755 | 3,473 | 9,830 | 42,603 | 20.8 s | |
| (6,2) 100% 94 | 56,748 | 53,645 | 173,265 | 671,309 | 249 s | |
| (4,2) 71 = H*, inside the `r0sxhn` set | 134,405 | 125,101 | 452,679 | 1.63M | 694 s, 13 GB | |

What the concrete search shows:

- **At an exact level (`r0sx`, room (1,0)) the concrete states ARE the
  abstract points.**
  - At h99 there are 326 states plus the win's 1 point, against the set's
    327 points; the gate gives 207 + 56 = 263.
  - Every one is on a winning path. A selection search inside the set never
    meets a dead end.
- **At `r0sxh` some admitted states are dead ends**, e.g. (6,2): 3,473 of
  3,755 are on a winning path. A (node, remainder) admits every concrete
  state that projects onto it, and the level widens fields the node does
  not key (`h`: the held buttons; which fields make the difference here was
  not traced).
  - (4,2) has 134k states for 40.7k non-win points (3.3x). It was measured
    inside the coarse level's set, which is equal to the fine one at H*.
  - The fine level admits a subset of the coarse level's states (W1 ⊆ W0
    projected), and every on-path state, so its count is between 125,101
    and 134,405. It was not rerun: it takes over 10 min.
- **The cost is the engine.**
  - Room (1,0) runs ~12k frames/s on 16 threads; (4,2) ~2.3k.
  - Rows are big in an object room. The (4,2) run held 13 GB at its widest
    layers (~47k states, plus each admitted successor's row until the
    merge; repeats within a chunk are dropped since then).
- **To find ONE optimal path** the search's own depth-first try at the bound
  takes exactly H* steps: 99 for (1,0), 71 for (4,2), 93 for (6,2) (the
  searches' `[concrete] depth-first` lines). The set adds nothing there; it
  matters when the selection search wants to compare paths.
- **The time-collapsed set** (frame-limit ints) admits the same states at H*
  and 0.02-3.6% more at H* + 1 or H* + 2. It is almost as tight as the exact
  form.

## Proposed output format

The search writes, beside the witness, `optimal_set.h{h}.bin` for h = H*
(and optionally H* + 1, H* + 2: one backward and one reach each, seconds at
these sizes):

1. **Header.** The settings record (`settings.txt`: room, category, loading
   jank, seeds, level) plus h, H* and the format version.
2. **Node identities.** Two options:
   - **(a) Exact keys** (shape, key, cell) plus the tree's key space
     (`KeySpace`: 954 codes for (1,0), 1,505 for (4,2), 3,996 for (6,2) 100%;
     tens of KB). The selection search keys concrete states exactly as the
     concrete search does today (`lookup_keys`, `NodeKeys`).
   - **(b) An 8 B content hash per node** (`ContentHash::state`). It needs a
     function that hashes a concrete state's widened projection without the
     dictionary; content hashes are sums of per-code hashes, so it is
     possible, but it is not written. It is sound as a FILTER: a collision
     only admits a state that should not be, and the selection search is
     exact.
   - Prefer (a): it is exact and small, and nothing new is needed.
3. **Per frame t,** the entries (node index, R_t(n)) as a canonical region:
   slabs of y with x segments, u16 coordinates. This is the "regions form"
   `--write` writes.
   - Sizes as measured: 8 KB, 133 KB and 16 KB raw at H* (3.5, 25 and 6 KB
     zstd), and 0.6-1.6 MB raw at H* + 2.
   - A "points form" (4 B a point) is smaller where points do not merge
     ((1,0)) and larger where they do ((4,2): 192 KB).
4. **Optional, for robustness costs:** W_t(n) at the set's node-frames, the
   margin robust-path measures against: 6 KB at (6,2) H*, 20 KB at (4,2) H*,
   66-242 KB at H* + 1 / + 2.
5. **Not needed:**
   - the node-level edges (src, dst, transfer): 12 KB at (1,0) H*, 9 KB at
     (6,2). The concrete selection search recomputes successors with the
     engine; edges serve only a purely abstract selection, which cannot be
     trusted for real play anyway;
   - the time-collapsed form: at H* it is the same set, and the exact form
     is barely bigger.

Writing it costs the search a reach at each horizon (the backward is there
already): 0.02-0.14 s here. The tree (9-345M states, hundreds of GB on disk)
can then be deleted, and a selection run months later needs only the file
and the settings.

## The selection search (sketch, not built)

**Input.** The set file and the settings it records. Optionally the
original cart (`--lua .../celeste.lua`) or Celia, for real-play checks.

**Step.** It runs the real game: the reference engine's concrete frame,
every input class, every `rnd` leaf, or one fixed seed
(`--concrete-balloon-seeds`). A successor at frame t+1 is KEPT iff its node
(by the file's identities) has R_{t+1} containing its exact remainder.

- This prunes nothing optimal. R holds every concrete optimal state's
  projection, and the runtime guard checks it on every run above.
- At an exact level it keeps nothing else.

**The preference plugs in as a cost on (state, input) edges.** Two shapes:

1. **Exhaustive then DP**, where the set is small (every measurement above
   at H*, (1,0) to H* + 2, (6,2) to H* + 1):
   - breadth-first inside the set, keeping per link the mask of the input
     bytes that make it (playable's `--save-dag`);
   - backward-mark the states on a winning path;
   - then a DP over the DAG: widest path for robustness (robust-path),
     shortest path over (state, previous byte) for input changes
     (playable's `fewest_events`), or k-best paths handed to a validator.
   - Cost: the "frames run" column (seconds for (1,0) and (6,2) at H*,
     ~4 min for (6,2) at H* + 1, ~11 min for (4,2) at H*).
2. **Best-first or beam by cost**, where the set's concrete region is large
   ((4,2) at H* + 1 or more):
   - expand states in order of the cost so far plus an admissible
     cost-to-go;
   - the frame index is fixed by the layer, and every kept state can still
     win by h at an exact level, so a greedy or beam path never runs dry
     there; at `h` levels a dead end costs a backtrack;
   - input-change costs need the previous byte in the search state (up to
     64x the states; in practice the input classes a frame reads).

**Real-play validity** has two routes:

- run the selection on the original cart. It is not the traced minimal
  cart (playable notes its 4-frame jump buffer), so a state's projection
  onto the set must be checked there, not assumed;
- or post-filter: the k best candidates through the boot chain
  (`tools/uct/check_uploads.py`), the next candidate on a failure.

**Near-optimal TASes.** A "nice" TAS may be one frame slower. The h = H* + 1
file holds every path that wins by H* + 1 (the H* ones included):

- 4-33x the points;
- (1,0): 10.8k concrete states, 15 s; (6,2) 100%: 57k, 4 min;
- the selection costs the extra frame against its preference.

## Prior work: robust-path and playable (unmerged)

Both branches are stacked on merge-base `dedeebf` (now 202 commits behind
develop).

**`origin/robust-path`** (`b589bfb`, `search --robust`, `arc_dp::robust_search`):

- After `concrete_search` finds the optimum, a SECOND concrete
  breadth-first search inside W_{H-opt+k} runs (`expand(.., all = true)`).
  Those are the sets that still win at exactly opt, which is the
  concrete side of this document's set, built from W and the tree.
- It keeps every parent link (the plain concrete search keeps only the first
  parent of each exact state), one input byte per (parent, child).
- Per state: a margin, the largest remainder shift along one axis that stays
  inside W_t, capped at 0.5 px. Then a widest-path (max-min) DP picks the
  most robust optimal path.
- Measured (gemskip any% 2500m, room (0,3), h108, optimum 105):
  - 5,514 states, 20k links, 297 s, 2.8 GB;
  - the bottleneck margin went from 0.0017 to 0.0068 px.
- No path count.

**`origin/playable`** (`db0fe0b`, `rewrite playable`, `search/playable.rs`):

- It adds the full input-byte MASK per link (`search --robust --save-dag
  FILE`) and builds `next[k][s][64]` over the same states. Dead ends are
  dropped, and `rnd` forks are refused (the room must be deterministic under
  its seeds).
- Selection:
  - a DP for the fewest button events over (state, previous byte), with
    noisy tie-breaks for 16 seeds;
  - then local search (set or shift a button over a range, shift chords and
    holds) on a cost of frame-perfect edges: `sum -ln p`, p the chance of
    hitting each edge's window at sigma = 1 frame;
  - windows are checked by walking the DAG;
  - the reference engine re-simulates shifted sequences (0 of 864
    disagree).
- Room (0,3):
  - 47 -> 19 button events, 13 -> 9 frame-perfect edges (12 -> 8 on the
    original cart);
  - 4 frame-perfect edges are on every optimal path.
- No DAG size is quoted.

**How they map onto this design.** Both are the "exhaustive then DP" shape of
the selection search:

- Their DAG is exactly what a breadth-first search inside the set produces.
  The guard above shows that inside-W and inside-R admit the same concrete
  states from the start.
- Today they need the whole tree and the arc phase in the same process. With
  the set file they become separate tools over a ~10-130 KB input:
  - robust-path's margins need W at the set's node-frames (item 4 of the
    format) or a re-simulation with shifted remainders;
  - playable needs nothing more.
- Both pick one path, one input byte per (parent, child) for robust-path.
  The set keeps them all.
- rnd: robust-path's DP takes the max over leaves, playable refuses forks,
  and this set is best-case over `rnd` (as the search is). A selection
  under a fixed seed is a subset.

## Caveats and open points

- The set is at the LEVEL's precision.
  - At `r0sx` (no objects) it is exact: concrete states = points.
  - At `r0sxh` it admits concrete states that differ in what the node does
    not key: 3.3x the points inside (4,2)'s `r0sxhn` set, and 5-8% dead
    ends ((6,2), (4,2)).
  - It never drops a winner (guarded).
- Object rooms are slow in the reference engine: (4,2)'s exhaustive pass is
  ~11 min at H*. A beam or best-first selection avoids the full pass.
- The set at H* + k grows with the slack. The measured worst is (4,2):
  41.7k -> 235k -> 857k points (1.6 MB raw) in two frames. Writing H* + 1
  routinely and H* + 2 on request looks right.
- Not built: writing the file from `search`, the reader, and the selection
  search. `rewrite optimal-set` is the measurement only. Its R computation
  is independent of `arc_dp::reach` (point by point, never widened), and
  every concrete run checks it.
