# The arc phase's memory (2026-10-11, branch `arc-memory`)

Philippe: the arc phase (everything after the forward in `rewrite search`)
"should not be necessary to use much at all". On the map mod's challenge
room (room (0,0), `r0sxhne`, synthetic win `56..72,104..112`, level -1
(155,5), `--cart taslevel.p8`; the tree `/var/tmp/mod-ck/mid155`, 1.05B
states, 39.4B edges, 127 GB) it took 62.5 GB at h140 (33 GB anonymous in
`reach` alone) and was OOM-killed at ~104 GB anonymous at h155, against a
forward that peaked at 21 GB.

Result: the backward is ONE PUSH over the recorded in-edges, read from the
edge files; no graph is loaded and no remainder-free marks are made. The
mod room at h155 now runs its arc phase in TODO GB anonymous (TODO s), under
a 30 GB cap; every `[gate]` W line, the optimum and the witness are
identical on every case measured.

## Where it went (before: develop 432a844)

The old phase: (1) a remainder-free BFS from the wins over the in-edges
(`search::edges::bfs`), marking every state that wins with SOME remainder,
with its deadline; (2) `arc_dp::load`: the edges between marked nodes as a
CSR both ways; (3) `arc_dp::backward`: W pulled frame by frame,
recomputing the predecessors of changed nodes and the nodes whose deadline
starts; (4) fingerprints, `reach`, `NodeKeys`, the concrete search.

Per structure (old), the mod room at h140: 114.7M marked nodes (29% of
390M states), 2.08G kept edges (16% of 12.8G), 103.7M spans, 6.2M
distinct sets.

| structure | bytes | mid140 | lower bound |
|---|---|---|---|
| BFS: marks (members 16 B + masks), frontier and found lists 16 B each | ~24 B a mark | 5.0 GB phase peak | 0 (W's support is the exact set) |
| node arrays: marks copy (16), ids (8), out_at (8), pred_at (8), layer (4), until (4), win (1) | 49 B a node | 5.6 GB | a few B a node of W |
| CSR: `out` (target + transfer, 8 B), `preds` (4 B) | 12 B a kept edge | 25 GB | 0 resident: the edge files are the graph |
| backward: `pos`, `stamp` (8 B a node), frame lists (10 B a present node), spans (12 B) + `Arc`-free set arena | | +2.6 GB | W where it changes |
| fingerprints: node hashes (8 B a node), `NodeKeys` (32 B a node) | 40 B a node | 40.7 GB phase peak | 24 B an entry |
| reach: graph still held, per node last/stamp/pos (12 B), its sets | | 33 GB | the reached nodes' edges |
| edge files (mmap, file-backed) | | 26.7 GB file at the BFS | reclaimable |

At h155 the BFS alone ended at 9.2 GB anonymous (phase peak 14.9) with
70.3 GB of the tree mapped; the CSR load then died at ~104 GB (an
estimated 371M marks and ~6.7G edges: ~98 GB before the backward).

The marks were the problem's root: the remainder-free superset of what can
win kept 29% of the states and 16% of the edges, every byte of which the
CSR held twice over. W's support (the nodes with a set at some frame) at
h155 is 230M nodes - but the backward never needs their edges held.

## The change: one backward push over the in-edges

`W_t(n)` only grows as `t` falls: more time never hurts, and the layers
never bind - an edge recorded at frame f leaves a state of layer f - 1 and
enters one of layer <= f, so `W_t(p)` for `t >= layer(p)` only reads
`W_{t+1}(m)` where `m` has one. Hence

    W_t(p) = W_{t+1}(p) ∪ U { pull_e(W_{t+1}(m)) : e = p -> m, m changed at t + 1 }

and the backward (`arc_dp::Backward`) is, frame by frame from the wins:

1. READ: the in-edges of the nodes whose set changed at t + 1, recorded at
   frames <= t + 1 (their sources have layers <= t): `EdgeStore::
   preds_sorted`, targets in id order, region by region (a file's regions
   found once, `EdgeFile::regions`), entries by galloping through the
   region's range of the owner index, a lid decoded once for all its
   targets' cells. In ROUNDS of at most 64M in-edges (24 B each), 256
   targets a chunk on the workers.
2. FOLD: per round, per source in parallel id ranges, its set so far (or at
   t + 1) united with the round's pulls (`Region::from_pieces`); sets made
   mid-step are the step's own until a node keeps one.
3. MERGE: a node whose set grew closes its span and opens one; a node of
   layer t + 1 closes its span (it is not present below its layer); the
   new sets enter the arena once each.

No deadlines are needed (a node no win reaches never gets a set), no
graph is held, nothing is marked first. W is exact, so the gate's W lines
are unchanged; the gate's first line (`marks`, the remainder-free marks)
becomes `support` (W's support): re-pinned in
`gates/arc_room10_win9-101_h35.txt` and `gates/fixtures/search42.gate`,
every other line unchanged. The UI's `level0.marks.bin` holds W's support
(each node to its last frame) instead of the remainder-free marks.

Storage of W (`arc_dp::Winning`): a node's set is non-empty from its layer
to its last frame and nested, so it is a few spans; per step the closed
spans are one RUN of varints (id delta, length, set); at the end the runs
are merged node by node into varints - per node its id delta, layer and
span count, per span its length and set (sets renumbered by use) - with a
block index every 16 nodes. The distinct sets live in a flat
`RegionArena` (12 B a slab, 8 a segment, 8 a set), queried through a
borrowed `RegionRef`.

The rest:

- `optimum` reads `horizon - max{t : p0 ∈ W_t(start)}` off the start's
  spans (the witness walk over out-edges was unused: only its frame was).
- `NodeKeys` (the concrete search's and the known route's lookup of a
  concrete state's node) lists the support's ENTRIES, `((region, key),
  entry)`, 24 B each, not every node at 32 B: a state's id is its region
  (shape, cell), entry and cell. Room (6,2) synthetic win: 83k entries for
  1.7M nodes.
- `reach` (only for the next level's filter and `--save-marks`) loads the
  out-edges between W's nodes that can carry a remainder - from a non-win
  source to a target with a set at or past the edge's frame - unit by unit
  in two passes (`OutGraph`, 8 B an edge), then scatters each frame's
  reached sets along them. Byte-identical `arc.marks.bin`.
- the gate's W lines come from a difference array over the spans (a
  span's hash added at its first frame, taken off past its last), not
  frame by frame.
- the edge files are advised `MADV_RANDOM` for the backward (below), back
  to normal for the scans.

## Memory and time, before and after

Quick profile, 16 threads, shared machine. "anon" is `RssAnon` at the
phase lines (`[mem]`), its phase peak sampled every 50 ms; "file" the
mapped edge files.

| case | before | after |
|---|---|---|
| r10arc (room (1,0) h56 synthetic win; 337k marks, 4.1M edges) | bfs 0.1 + load 0.2 + backward 0.12 s; arc anon peak 0.25 GB | backward 0.2 s; anon peak 0.06 GB; 153k support nodes |
| r62w (room (6,2) any% `r0sxhf` h56, win `22..30,44..52`; 11 GB tree, 2.16M marks, 46.7M edges) | bfs 0.8 + load 0.9 + backward 1.2 s; anon: graph 0.72, backward 0.77, fingerprints peak 0.90 GB; with `--save-marks` reach 0.94 | backward 0.8-1.3 s (read 0.34, pull 0.34, merge 0.13); anon peak 0.25 GB; 1.71M support nodes; reach: 25.5M out-edges (0.84 s), phase peak 0.64 GB |
| mod room h140 (from `/var/tmp/celeste-ui/mid140.log`) | bfs 95 + load 40 + backward 91 s; anon 31.2 GB graph, 40.7 peak; reach 33.6; 62.5 GB with the files | not rerun (the tree is now built at h155) |
| mod room h155 (mid155) | OOM-killed at ~104 GB anonymous in the load | TODO |

The mod room at h155, step by step (the after column's runs):

| version | backward | anon peak (backward) | W | disk read |
|---|---|---|---|---|
| push, rounds unbounded, W as fields (16+20 B a span), read-ahead | killed after 5.5 min | 15.9 GB at the 30 GB cap | | 315 GB, page cache thrashing |
| + `MADV_RANDOM` | 485 s | 21.8 GB | 12.2 GB | 70 GB (15.5M major faults) |
| + rounds, W and runs as varints | 393 s | 14.9 GB | 8.5 GB (5.4 of it 28.3M `Region`s) | |
| + the set arena, 256-target chunks | TODO | TODO | TODO | TODO |

Its backward: 430M spans (sets changed), 230M support nodes, 28.3M distinct
sets, 6.0G in-edges read (5.0G after dedup), at most 96.5M nodes present
at a frame, at most 350M in-edges at a frame; 79.7 GB of the 124 GB edge
store read logically (1.6G owner-index entries, 54 GB of lids); 182 target
regions, each read on 21.2 steps (at most 58); the optimum 141 (steps).

## The page cache

The edge files are mapped; their pages are charged to the run's cgroup
but are clean and reclaimable (`MemorySwapMax=0` only pins anonymous
memory), so they cannot OOM the run - they thrash it. Under a 30 GB cap
with ~16 GB anonymous, the first push run read 315 GB from disk in 5.5
minutes without finishing: each fault read ~170 KB (the device's 256 KB
read-ahead) for a lid of tens of bytes, and the read-ahead evicted the
pages in use. With `MADV_RANDOM` on the edge files during the backward the
whole run read 70 GB (room (6,2) cold under a 600 MB cap: 2.27 -> 0.80 GB
read, slower there - 11.6k -> 128k faults - and the same warm). Less
anonymous memory leaves more of the cap to the cache.

## The region worklist (measured, not adopted)

Philippe's proposal: each node's value its whole time-indexed set, the
backward a monotone fixpoint scheduled by target region - open a region,
apply all its pending updates over all frames, push the changed sources to
their regions' queues - so that a region is opened few times. Built as an
experiment (`26d976d`, `bench-backward --worklist`, removed in `2e75fd9`):
it reaches the same W at every frame (checked on r10arc and r62w), and

| r62w | opens a region (mean, max) | in-edges read | logical bytes | value changes | time |
|---|---|---|---|---|---|
| sweep (time-stepped push) | 10.2, 16 | 39.3M | 0.42 GB | 2.92M | 1.4 s (16 threads) |
| worklist, latest pending frame first | 2.9, 5 | 41.2M | 0.47 GB | 3.85M | 20 s (one thread) |
| worklist, largest queue first | 3.9, 8 | 44.2M | 0.56 GB | 4.08M | 24 s (one thread) |

Fewer opens, more reading. A region opened before its successors' values
are final is opened again, and a reopen re-reads its pending nodes'
in-edges over all their frames. In the sweep every value it reads is final
at its frame (W_t depends on W_{t+1} only), so a node is read once per
change of its set - 1.87 times a node on the mod room - and no schedule can
read less than once. The worklist also holds every node's value mutable at
once (the sweep only the present frame's), and its opens are sequential.
What would cut the reads is the layout: a region's in-edges sit in many
units' blocks (the lids of a target region are spread over every unit whose
sources reach it), so a target-indexed edge store ("target-region bundles")
would make a region's in-edges one contiguous read per frame.

## Follow-ups

- `reach` holds the out-edges between W's nodes (8 B an edge, 12 B a node
  while loading): fine for the ladder rooms, not measured at the mod
  room's scale (230M nodes). Loading only the reached nodes' out-edges,
  frame by frame, would bound it by the reached set.
- The concrete search was not measured on the mod room here (`--no-witness`
  runs).
- A target-indexed edge layout (above) would turn the backward's scattered
  lid reads into contiguous ones.
