# Format study: a compact level-0 graph (2026-10-09)

One real mid-run tree, re-encoded as drafts. `rewrite format-study` sizes
each format (nothing is written) and times a varint round trip:

```bash
CELESTE_START_ROOM=6,2 CELESTE_HUNDRED=1 CELESTE_LOADING_JANK=2 ./safe-run.sh --memory 30G -- \
    ./target/quick/rewrite format-study --level-dir /var/tmp/h23base --to 56 --threads 4
```

(~5.5 min, 12 GB peak, mostly single-threaded edge sizing.)

The tree: 2300m 100%, room (6,2), `--level r0sxhf`, built to frame 56 under
the level -1 filter (94,5). **37.1M states** in 57 layers (the frontier,
layer 56, is 5.86M), **1.257G edges, 33.9 per state** (f56 alone 218M,
43.8 per source). 17,080 `(shape, cell)` shards. One shape holds 95% of
the states.

## 1. Today

| what | per unit | per state | total |
|---|---|---|---|
| frames on disk (v11: raw columns + 16-B key) | 72.9 B/state (54.0 columns, 16 key) | 72.9 | 2.71 GB |
| door in RAM (`(u128 key, u64 id)` + bucket index) | 24.73 B/entry | 24.7 | 0.92 GB |
| frontier as Rt2 blocks (AV = 12 B, +key 16, id 8, cell 4) | 103.8 B/row | 16.4 (×0.158) | 0.61 GB |
| edge runs on disk (v5) | 3.51 B/edge | 119 | 4.42 GB |
| raw edge records (one frame, transient) | 16 B/edge | - | 3.5 GB at f56 |
| arc `Graph` in RAM, if over every node | 33 B/node + 12 B/edge | 440 | 16 GB |

The arc `Graph` is only built over the marked nodes in practice. Of the
3.51 B in a run edge, about 2 B are the transfer (a varint of its
frequency rank) and about 1.5 B are the ids.

## 2. States

The packed exact state, per shard: each varying column is coded by the
shard's dictionary of distinct values, or by the bit width of its range
(`(max - min) / gcd`), whichever is narrower.

| measure | bits or bytes per state |
|---|---|
| bit-packed, dictionary (min with range: the same) | **37.1 bits** |
| bit-packed, range only | 87.4 bits (`spd.x` is sparse in its range) |
| mixed radix (sum of log2 of the cardinalities) | 32.4 bits |
| sorted per shard, varint deltas | **1.23 B** |
| sorted per shard, Elias-Fano (universe 2^bits / mixed radix) | 3.28 B / 2.69 B |
| one Elias-Fano run per (shard, layer) | 3.72 B |
| dictionaries (3.1M entries at 8 B) | 0.67 B |

- **The packed state is an exact key.** There are 0 duplicate packed
  states, so it can replace the 128-bit hash outright (no hash, no
  collision caveat).
- **Key width:** the widest shard needs 41 bits. 8.1% of states sit in
  shards of 32 bits or less, the other 91.8% in shards of 33-48 bits. It
  fits a u64 everywhere.
- **Column cardinalities** (main shape 0x71e1…, the maximum over shards,
  with row-weighted bits): spd.x 1220 (9.0 bits), spd.y 75 (6.0),
  dash_effect_time 11 (4.0), dash_time 5 (3.0), dash_target.x and .y 3
  each (2.0), freeze 3 (2.0), dash_accel.x and .y 3 each (1.9), djump 3
  (1.9), grace 6 (1.5), flip.x 2 (1.0), has_dashed 2 (0.9). Speed is 15 of
  the 37 bits.
- Shard sizes: the median shard holds about 500-1000 rows and the largest
  under 65,536, so a rank within a shard fits 16 bits.

**The door per entry:**

| format | B/entry |
|---|---|
| today | 24.73 |
| packed key in bytes + u32 id | 9.0 |
| u64 word: key (≤42 bits), layer (6 bits), rank in (shard, layer) (16 bits) | **8.0** |
| Elias-Fano, no id (id = rank) | 3.3 (3.7 as per-layer runs) |

The u64 word fits because the key is at most 41 bits and a (shard, layer)
holds fewer than 2^16 rows. Its id is the layer's base, plus the shard's
offset within the layer (a 57 × 17k table, 3.8 MB), plus the rank. No id
is stored and no hash is needed.

## 3. Ids

- pack_id is 64 bits and sparse: the seq is `worker*256+k`. 37.1M states
  fit a dense u32 easily (2^32 is 4.3G).
- Dense in admission order needs a global counter, or per-worker ranges
  with gaps.
- **The rank as the id works per LAYER, not per shard.** A shard keeps
  gaining states for many frames, so a rank in the shard is not stable. A
  layer is frozen once its frame ends. So id = `layer base + rank in the
  layer sorted by (shard, packed key)` (order "B") is implicit and stable.
- The cost: the frame's new states only get their final ids at the frame's
  end. Edges into the NEW layer (12% of a frame's edges at f56) carry a
  provisional id that is remapped through a u32 permutation of the new
  layer (6 M entries). Edges into old layers already have final ids. The
  remap folds into the edge sort that happens anyway.
- Order B also codes edges better than pack_id order (A) when grouped by
  target: 1.31 against 1.50 B/edge. By source the two tie (1.41 against
  1.40).

## 4. Edges

**Shape:**

- Out-degree per source: 60% have 33-64 edges, 10% have 65-128, and 1.5%
  have none.
- In-degree within one frame: mostly 2-8. Over all frames per state, 65%
  have 3-8, with a long tail past 100k (a few hub states).
- **88% of a late frame's edges go to states first seen in an EARLIER
  frame.** Over all frames, `frame - target layer` is 0 for 13%; it peaks
  at 14-17 frames back (about 5% at each lag) and reaches back about 30
  frames. So target ids are NOT local to the source's layer.
- **Transfers are not tiny:** 81,701 distinct pairs in f56's table, 103k
  over all frames. Their order-0 entropy is 13.5 bits per edge (x 8.3 + y
  5.9).
- **But given the SOURCE, the transfer has 1.9 bits.** A source uses only
  3.5 distinct transfers across its ~44 edges. Given the target it has 7.9
  bits (17.5 distinct per target).

**Bytes per edge, all frames:**

| format | ids | transfer | total |
|---|---|---|---|
| today's runs (v5) | ~1.5 | ~2.0 | 3.51 |
| fixed width, per-frame widths | | | 7.78 |
| fixed width, global widths (26+26+17 bits) | | | 8.62 |
| by target, varint, order B, transfer rank varint | 1.31 | 1.96 | 3.27 |
| attached to target (count + source deltas), order B | 1.30 | 1.96 | 3.26 |
| by source, CSR (implicit source), varint, transfer rank varint | 1.40 | 1.96 | 3.36 |
| Elias-Fano, one sorted set of (t, s) pairs | 2.51 | - | - |
| Elias-Fano CSR lists (targets, offsets, lists) | 1.88 | - | - |
| **by source + a per-source transfer table** (each distinct transfer named once at 17 bits, then a local index per edge) | 1.40 | 0.41 | **1.81** |
| by target + a per-target transfer table | 1.31 | 2.73 | 4.04 |

- **The transfer is the edge's biggest field, and only grouping by source
  makes it cheap.** Interning each source's transfer SET (rather than
  naming each transfer at 17 bits) would cut the 0.41 B toward the 1.9-bit
  bound, about 0.25 B.
- Speed (1 thread, f56, 218M edges): varint by target encodes at 3.4 ns
  and decodes at 3.2 ns per edge. A full 1.26G-edge pass is about 4 s on
  one core, or 0.3 s on 16.

## 5. The whole picture (bytes per state, this tree)

| design | states | door (RAM) | edges | frontier (RAM) | total |
|---|---|---|---|---|---|
| today | 72.9 (disk) | 24.7 | 119 (disk) | 16.4 | **233** |
| S: packed fixed 5 B + u64 door + by-source edges with per-source transfer table | 5.0 (+0.7 dicts) | 8.0 | 61 | 1.3 (8 B packed rows) | **76** |
| S', states as varint deltas (cold) | 1.2-1.5 | 8.0 | 61 | 1.3 | **~72** |
| T: as S, edges by target (order B), transfer rank varint | 5.7 | 8.0 | 111 | 1.3 | 126 |

Edges are 80% of S, and the transfer is 23% of the edges. The states (and
their 16-B keys) become noise.

**DRAM traffic for one frame f56 → f57** (estimates: about E = 260M edges,
6.8M new states, a door of 37M; ~45 GB/s). The emitted-row queues are the
same in both designs, so they are left out (cache-resident by design; only
the key shrinks, 16 → 8 B).

| step | today | design S |
|---|---|---|
| frontier in (stored → block) | 5.86M × (73 r + 104 w + RFO) ≈ 1.6 GB | 5.86M × 8 r ≈ 0.05 GB (unpacked in cache) |
| door probes: lower bound (each shard's lines once) / upper bound (2 lines and 1 line a probe) | 0.9 / 33 GB | 0.3 / 17 GB |
| door end-of-frame merge (rewrites every touched shard) | 37M × 24.7 × (r + w + RFO) ≈ 2.7 GB | 37M × 8 × 2 ≈ 0.6 GB merged, or 0.05 GB as per-layer runs |
| new states out | 6.8M × 73 (+RFO) ≈ 0.5-1 GB | 6.8M × 5 ≈ 0.03 GB |
| edges: record → sort → encode | ≈ 150-180 B/edge ≈ 40-47 GB (16 B record buffered, written, deduped, then read 3 times by compaction; 24 B `Rec` scattered with RFO and read; stream written and copied) | 12 B (s, t, x) NT-store + 12 B read in a cache-blocked source sort + 1.8 B out ≈ 26 B/edge ≈ 7 GB |
| **total** | **~45-85 GB, 1-1.9 s** | **~8-25 GB, 0.2-0.55 s** |

Today the edge pipeline alone is about 1 s of the machine's bandwidth per
late frame. The 16-B raw records also hit the disk (3.5 GB at f56).

## 6. Recommendations

**Design S (recommended): the packed exact state, a u64 door word,
layer-implicit ids, edges as a by-source CSR.**

- **States.** Per shard, the dictionary-coded packed key: 37 bits here,
  ≤42 bits by construction or the shard is widened. Dictionaries are
  assigned in first-appearance order so codes never move; a column whose
  dictionary passes a power of two rewidens its shard (rare, amortized). A
  layer's states are stored sorted by (shard, key): 5 B fixed for random
  access by id (frontier load, concrete-search lookups), or 1.2-1.5 B
  delta-coded when cold. The frontier lives packed (8 B/row) and is
  unpacked into a kernel block a batch at a time.
- **Door.** Per shard, one sorted `u64` array of `key | layer | rank`: 8 B
  an entry, exact, and the id is implicit. A probe is one binary search in
  a cache-friendly array. The end-of-frame merge moves 8 B entries instead
  of 24.
- **Ids.** u32, `layer base + rank in (shard, key) order`, final at the
  frame's end. Edges into the new layer are remapped once (12% of edges).
- **Edges.** For edges recorded at frame F, the source is implicit (CSR
  over layer F-1). Per source: out-degree, a small local table of its
  transfers, then per edge the target delta (varint, zigzag for the first)
  and a local transfer index. 1.8 B/edge against 3.5. To write it, workers
  NT-store fixed 12 B (s, t, x) into per-worker buffers, which are sorted by
  source in cache-sized blocks and encoded. Today's 16-B raw files and the
  three-pass compaction go away.
- **Forward (writing):** the cheapest by far, and the edge write order is
  the source order the frame already processes in.
- **Arc backward:** with a by-source CSR, each BFS or W-fixpoint iteration
  is a PULL sweep. Each source ORs its successors' sets through its
  transfers: the edge stream is sequential and the targets' sets are read
  at random. It parallelizes over sources with no atomics. What it loses
  is the worklist: "revisit only the predecessors of what changed" needs
  predecessors. If sweeps are too many, build a reverse index in RAM for
  the marked subgraph only (u32 preds, 4 B/edge), as `Graph` does today
  but with u32 offsets.
- **Concrete search:** it needs only the door (abstract key → id, 8 B an
  entry, frozen) and the W sets by id. It needs neither the states' values
  nor the edges.

**Design T (the alternative): as S, but edges grouped by target**
(order B, 1.31 B ids). The backward BFS can push from targets to their
predecessors directly with a worklist. But the transfer costs 2 B/edge,
since a target's predecessors share no transfer structure. That makes 3.3
B/edge, no better than today's runs. Choose it only if the backward's
worklist (predecessors of changed nodes) beats full sweeps by more than
the extra ~1.5 B/edge × 1.26G = 1.9 GB per pass.

**Caveats.**

- The dictionaries here are computed AFTER the fact (final per shard). A
  live forward sees them grow, and the first-appearance codes may be a bit
  wider than 37.1 bits on average.
- Edge-format sizes are computed from the exact coding rules, not from
  written files (except the timed by-target varint).
- The DRAM table is an estimate from these counts, not a measurement.
- The per-source transfer table is costed at 17 bits per distinct
  transfer; interning transfer sets is the next cut.
