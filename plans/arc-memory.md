# The arc phase's memory (2026-10-11, branch `arc-memory`)

Philippe: the arc phase (everything after the forward in `rewrite search`)
"should not be necessary to use much at all". On the map mod's challenge
room (room (0,0), `r0sxhne`, synthetic win `56..72,104..112`, level -1
(155,5), `--cart taslevel.p8`; the tree `mid155`: 1.05B states, 39.4B
edges, 127 GB on disk) it took 62.5 GB at h140 (33 GB anonymous in `reach`
alone) and was OOM-killed at ~104 GB anonymous at h155, against a forward
that peaked at 21 GB.

**Result.** The backward is ONE PUSH over the recorded in-edges, read from
the edge files: no graph is loaded and no remainder-free marks are made.
The mod room's arc phase at h155 now peaks at 6.5 GB anonymous (inside its
heaviest backward step; 2.8-3.9 GB between phases), the backward taking
277 s, under a 30 GB cap - 31% of the forward's 21 GB peak, against the
old phase's >104 GB. Every `[gate]` W line, the optimum, the witness and
the arc marks are identical on every case measured.

## Where it went (before: develop 432a844)

The old phase: (1) a remainder-free BFS from the wins over the in-edges
(`search::edges::bfs`), marking every state that wins with SOME
remainder, with its deadline; (2) `arc_dp::load`: the edges between marked
nodes as a CSR both ways; (3) `arc_dp::backward`: W pulled frame by frame,
recomputing the predecessors of changed nodes and the nodes whose deadline
starts; (4) fingerprints, `reach` over the same graph, `NodeKeys`, the
concrete search.

The mod room at h140 (`/var/tmp/celeste-ui/mid140.log`; 114.7M marked
nodes, 29% of 390M states; 2.08G kept edges, 16% of 12.8G; 103.7M spans,
6.2M distinct sets):

| structure | bytes | mid140 | lower bound |
|---|---|---|---|
| BFS: marks (16 B a member + masks), frontier and found lists (16 B) | ~24 B a mark | 5.0 GB phase peak | none: W's support is the exact set |
| node arrays: marks copy 16, ids 8, out_at 8, pred_at 8, layer 4, until 4, win 1 | 49 B a node | 5.6 GB | a few B a node of W |
| CSR: `out` (target + transfer) 8 B, `preds` 4 B | 12 B a kept edge | 25 GB | none resident: the edge files are the graph |
| backward: `pos`, `stamp` 8 B a node, frame lists 10 B a present node, spans 12 B, a `Region` (2 allocations) a set | | +2.6 GB | W where it changes |
| fingerprints: node hashes 8 B a node, `NodeKeys` 32 B a node, the full `Resolver` | 40 B a node | 40.7 GB phase peak | a key per W entry |
| reach: the graph still held, per node 12 B, its sets | | 33 GB | the reached nodes' edges |
| edge files (mmap, file-backed) | | 26.7 GB file at the BFS | reclaimable |

At h155 the BFS ended at 9.2 GB anonymous (phase peak 14.9) with 70.3 GB
of the tree mapped, and the CSR load died at ~104 GB (an estimated 371M
marks and ~6.7G edges: ~98 GB before the backward).

The marks were the root: the remainder-free superset of what can win kept
29% of the states and 16% of the edges, which the CSR held twice over.

## The change: one backward push over the in-edges

`W_t(n)` only grows as `t` falls: more time never hurts, and the layers
never bind - an edge recorded at frame f leaves a state of layer f - 1 and
enters one of layer <= f, so `W_t(p)`, `t >= layer(p)`, reads `W_{t+1}(m)`
only where `m` has one. Hence

    W_t(p) = W_{t+1}(p) ∪ U { pull_e(W_{t+1}(m)) : e = p -> m, m changed at t + 1 }

and the backward (`arc_dp::Backward`) is, frame by frame from the wins:

1. READ: the in-edges of the nodes whose set changed at t + 1, recorded at
   frames <= t + 1 (their sources have layers <= t), by target
   (`EdgeStore::preds_sorted`: region by region - a file's regions found
   once, `EdgeFile::regions` - entries by galloping through the region's
   range of the owner index, a lid decoded once for all its targets'
   cells). The work is ITEMS, (chunk of 1024 changed targets, frame), taken
   one at a time by the workers; a ROUND stops at 32M in-edges (24 B each),
   so one heavy target bounds a round by its in-edges at one frame.
2. FOLD: per round, per source in parallel id ranges, its set so far (or
   at t + 1) united with the round's pulls (`Region::from_pieces`); sets
   made mid-step are the step's own until a node keeps one.
3. MERGE: a node whose set grew closes its span and opens one; a node of
   layer t + 1 closes its span (it has none below its layer); the sets
   nodes keep enter the arena once each. The frame done last is dropped
   before the ranges' outputs are joined (two copies of a frame, not three).

No deadlines (a node no win reaches never gets a set), no graph, nothing
marked first. W is exact, so the gate's W lines are unchanged; its first
line (`marks`, the remainder-free marks) becomes `support` (W's support):
re-pinned in `gates/arc_room10_win9-101_h35.txt` and
`gates/fixtures/search42.gate`, every other line unchanged. The UI's
`level0.marks.bin` holds W's support (each node to its last frame).

**W** (`arc_dp::Winning`): a node's set is non-empty from its layer to its
last frame and nested, so it is a few spans. Each step's closed spans are
one RUN of varints (id delta, length, set); at the end the runs are merged
node by node into varints - per node its id delta, layer and span count,
per span its length and set (sets numbered by use, through an order array)
- with a block index every 16 nodes. Runs and nodes are written to scratch
files in `<level>/arc-scratch` and mapped back once complete (page cache,
unlinked once mapped; in memory without a directory: tests,
`bench-backward`). The distinct sets live in a flat `RegionArena`; a
region's bounds are stored in 16 bits (`[lo, last]`, a slab 6 B, a segment
4 B), queried through a borrowed `RegionRef`.

**The rest:**

- `optimum` reads `horizon - max{t : p0 ∈ W_t(start)}` off the start's
  spans (the old witness walk over out-edges was unused but for its frame).
- `NodeKeys` (a concrete state's node, for the concrete search and the
  known route) lists the support's ENTRIES, `((region, key), entry)`, 24 B
  each, not every node at 32 B: a state's id is its region (shape, cell),
  entry and cell. Room (6,2) synthetic win: 83k entries for 1.7M nodes.
- the `Resolver` keeps W's entries' keys alone (`load_for`); the known
  route's report, which resolves any row, loads the full one on a miss.
- `reach` (the next level's filter, `--save-marks`) loads a node's
  out-edges when it first reaches it, a frame's sources at a time
  (`ReachEdges`, `EdgeStore::out_edges`: a source's unit through the file's
  unit ranges, the units decoded whole, their lids' owners from one pass
  over the owner index), keeping those into W's nodes that can carry a
  remainder. Room (6,2): 4.4M edges for 317k reached nodes, against 25.5M
  between all of W's nodes. Byte-identical `arc.marks.bin`.
- the gate's W lines come from a difference array over the spans, not
  frame by frame.
- the edge files are advised `MADV_RANDOM` for the backward (below).
- mimalloc collects after each step: the steps' workers are new threads,
  and their heaps' freed pages came back lazily (~3 GB held on the mod
  room).

## Memory and time, before and after

Quick profile, 16 threads, a shared machine (load 7-27). "anon" is
`RssAnon` at the `[mem]` phase lines, the phase peak sampled every 50 ms.

| case | before | after |
|---|---|---|
| r10arc (room (1,0) `r0sxh` h56, win 40,64; 337k marks, 4.1M edges) | bfs 0.1 + load 0.2 + backward 0.12 s; arc anon peak 0.25 GB | backward 0.2 s; anon peak 0.06 GB; 153k support nodes |
| r62w (room (6,2) any% `r0sxhf` h56, win `22..30,44..52`; 11 GB tree; 2.16M marks, 46.7M edges) | bfs 0.8 + load 0.9 + backward 1.2 s; anon: graph 0.72, backward 0.77, fingerprints 0.90 GB; `--save-marks` reach 0.94 | backward 0.8-1.1 s (read 0.32, pull 0.34, merge 0.13); anon peak 0.16 GB; `--save-marks` reach 0.34 GB; 1.71M support nodes |
| mod room h140 | bfs 95 + load 40 + backward 91 s; 31.2 GB graph, 40.7 peak; reach 33.6; 62.5 GB with the files | not rerun (the tree is built at h155 now) |
| mod room h155 | OOM-killed at ~104 GB in the load | backward 277 s (read 116, pull 105, merge 29); anon peak 6.5 GB (the sweep), 3.5 GB (end of backward), 3.5 (fingerprints), 3.9 (node keys), 3.7 (concrete engines up); 230M support nodes |

The mod room at h155, version by version (cold page cache, 30 GB cap):

| version | backward | anon peak: sweep / end of backward / fingerprints | W | read from disk |
|---|---|---|---|---|
| push, unbounded steps, W as fields (36 B a span), read-ahead | killed after 5.5 min | 15.9 GB at the cap | | 315 GB, the page cache thrashing |
| + `MADV_RANDOM` | 485 s | 21.8 / - / 16.9 | 12.2 GB | 70 GB |
| + rounds, W and the runs as varints | 393 s | 14.9 / - / 13.0 | 8.5 GB (5.4 the sets as `Region`s) | |
| + the set arena, 256-target chunks | 343 s | 12.1 / - / 9.4 | 5.6 GB | 41 GB |
| + rounds by (chunk, frame), sets numbered without a copy | 280 s | 9.7 / 11.0 / 9.6 | 7.9 GB (capacities) | 41 GB |
| + runs and nodes spilled to files, W's entries alone resolved | 286 s | 8.1 / 4.9 / 5.0 | 5.8 GB (2.4 of it the spilled nodes) | 41 GB |
| + one frame copy fewer, 32M rounds, 16-bit bounds, mimalloc collects | 277 s | 6.5 / 3.5 / 3.5 | 4.3 GB (2.4 spilled, sets 1.8) | 40 GB (8.0M faults) |

Its backward: 430M spans (sets changed), 230M support nodes, 28.3M
distinct sets, 6.0G in-edges read (5.0G after dedup), at most 96.5M nodes
present at a frame and 350M in-edges read at one; 79.7 GB of the 124 GB
edge store read logically (1.6G owner-index entries, 54 GB of lids); 182
target regions, each read on 21.2 steps (at most 58); the optimum 141.

## The page cache

The edge files are mapped; their pages are charged to the run's cgroup but
are clean and reclaimable (`MemorySwapMax=0` pins only anonymous memory),
so they cannot OOM the run - they can thrash it. Under a 30 GB cap with ~16
GB anonymous, the first push run read 315 GB from disk in 5.5 minutes
without finishing: each fault read ~170 KB (the device's 256 KB
read-ahead) for a lid of tens of bytes, and the read-ahead evicted the
pages in use. With `MADV_RANDOM` during the backward the whole run read
70 GB (room (6,2) cold under a 600 MB cap: 2.27 -> 0.80 GB read, slower
there - 11.6k -> 128k faults - and the same warm), 41 GB once the
anonymous memory left more of the cap to the cache. The spilled runs and
nodes are written once and read back in order; they compete for the same
cap as reclaimable pages.

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
change of its set - 1.87 times a node on the mod room; reading it once
would need its whole value final first, which the time dependence (and
the graph's cycles) rule out for a schedule over whole values. The worklist also holds every node's value
mutable at once (the sweep only the present frame's) and its opens are
sequential. What would cut the reads is the layout: a target region's
in-edges are spread over the lids of every unit whose sources reach it, so
a target-indexed edge store ("target-region bundles") would make a
region's in-edges one contiguous read per frame.

## Toward ID-free, target-region edge storage

Edge storage may become ID-free: a region's IN-edges as (source state,
target state, transition) tuples of whole exact keys in the TARGET
region's compressed trie, with target-cell masks (another agent is
measuring it). The backward above already needs nothing else:

- its one storage query is "the in-edges, at frame f, of these targets"
  (`EdgeStore::preds_sorted`), asked with the changed targets sorted
  region-major - a lookup of those targets in the target region's store;
- nothing is numbered: a node is a state, carried as its storage identity
  (today a `StateId`, (region, entry, cell); with ID-free storage (region,
  key, cell)), and every set operation is a merge of state-sorted streams
  (the frame's sets, a round's in-edges by source) - no per-edge lookup,
  no global node ids, no CSR. W per state could equally be a per-region
  table keyed through the region's in-core table; the merges need only an
  order, the concrete search a lookup (per concrete state: rare).

Which schedule then: the sweep reads each node's in-edges once per change
of its set (1.87 times a node on the mod room), every value it reads final
at its frame; the region worklist opened regions 3.5x less often but read
5-12% more (above). That ranking holds while reading a region costs per target
looked up, as with the owner index today. If opening a target region costs
its whole bundle (decompressing its trie), opens dominate and the
region-batched schedule should be measured again on that layout; the
sweep's items would then be (target region, frame) rather than (1024
targets, frame).

## Follow-ups

- The sweep's peak is the largest frame twice (96.5M nodes, 16 B each)
  with a round and the step's sets so far; the arena of distinct sets
  (28.3M) is what stays.
- The concrete search itself is not this change's: on the mod room at
  h155 its depth-first try at the bound f141 was exhaustive in 51k steps
  (32.6 s, 3.7 GB anonymous with W and the 20.8M node-key entries), then
  its breadth-first search grew ~1.3x a layer inside W (26k states at layer
  122, 5.4 GB) and was stopped: W at `r0sxhne` admits too many concrete
  states there - the objects ladder's case (plans/architecture.md, "The
  objects ladder").
- A target-indexed edge layout (above).
