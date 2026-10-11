# edges-target progress log

Branch `edges-target` (from `develop` e7e9f78), worktree /var/tmp/tgt-wt,
target dir /var/tmp/tgt-target, fixtures /var/tmp/tgt-fixtures. Baseline:
detached worktree /var/tmp/tgt-base at e7e9f78, target /var/tmp/tgt-base-target
(quick + release), fixtures /var/tmp/tgt-base-fixtures (hard links of the
shared format-14/edges-5 fixtures). Logs in /var/tmp/tgt-logs.
(The storage-v2 log that was here is in git history and plans/storage-v2.md.)

## The plan (2026-10-11)

Target end state (the coordinator's brief): edges ID-free, as bundles
(source key, transfer, offset, target key) -> target-cell mask, stored per
TARGET region and frame (segments), zstd; the forward routes every emission
to its target region (dedup and edge insert there: one probe and an OR);
the backward reads in-edges from target-region blocks, W keyed by state;
lids, owner index, claims, translation, unit blocks, the v5 format deleted.

Steps:
1. The block codec (`storage::bundle`): bundles, offsets, per-block key
   dictionaries, the at-rest order, zstd; round-trip tests.
2. The edge files and their reader (`storage::edges`, version 6): per
   frame one file of (target region, segment) blocks and the
   source-region -> target-region index; scan, in-edges by region,
   out-edges through the index.
3. The forward: units emit records into per-target-region chunks
   (`storage::route`), the region phase dedups and inserts bundles
   (`storage::region`), the frame end finalizes keys, numbers new entries
   canonically and DECODES the new layer from (shape, key, cell)
   (plans/rows-from-keys.md's follow-up). Slices bound the records' memory.
4. The backward on states (`storage::Sk`: region, key, cell), reading
   (region, frame) blocks; reach through the index; the concrete search and
   the known route by Sk.
5. Consumers (ckhash, arc-check, bench-storage, bench-frame, census), tests,
   fixtures, gates; deletions; measurements; docs.
6. State ids (frame files, meta entries, drop notes, raise): a proposal to
   the coordinator with numbers before any rewrite.

## Log
- 2026-10-11 ~15:00 Baseline (e7e9f78, base fixtures): t1 all ok (69 s, load 22), t2 all ok (104 s).
- 2026-10-11 ~15:20 Step 1: `storage::bundle` (codec, order generic, zstd -1), round-trip tests pass.
  Re-validated the order on REAL trees (temporary `rewrite bundle-orders`: the v5 edges
  re-stored per (target region, frame) block; B an edge with zstd -1, raw in brackets):

  | tree | edges | edges/bundle | [M o x s t] | [s x o t] relative | [s x o t] | [t s x o] |
  |---|---|---|---|---|---|---|
  | r10 f50-56 | 88.4M | 5.59 | 0.099 (0.46) | 0.105 (0.50) | 0.171 | 0.162 |
  | r42n f55-61 | 57.2M | 5.78 | 0.129 (0.49) | 0.127 (0.57) | 0.241 | 0.189 |
  | r60s f65-71 | 9.1M | 7.90 | 0.249 (0.61) | 0.236 (0.57) | 0.316 | 0.271 |
  | r62h57 f52-57 | 1048M | 7.56 | 0.081 (0.35) | 0.075 (0.40) | 0.149 | 0.263 |

  Chosen: [M o x s t], absolute masks - within 0.92-1.06x of the relative masks,
  and a block decodes alone (relative masks need the previous layer's per-key
  masks in up to four source regions at every read). Decode ~1-3 ns an edge on
  one thread (load 20-45).
- 2026-10-11 ~16:00 Step 2-4 in one switch (8dcaa56): target-region bundle segments (edge file v6),
  the routed forward (records -> region phase -> segments; layer decoded from keys), the
  backward on states (Sk) over region blocks, reach through the source-region index.
  Room (1,0) f0-44: posgraph, ckhash, ckhash --edges, frame files and xfer.bin identical to
  develop; tree 280 -> 34 MB (edges 253 -> 6.4 MB, 0.055 B an edge). Arc gate identical
  (W, optimum 33, witness, known route). Suite + ignored 196/196. Every fixture rebuilt
  (/var/tmp/tgt-fixtures) reproduces its pinned ckhash --edges/--dropped: r10 773 -> 61 MB,
  r42n 450 -> 43, r42x 140 -> 17, r60s 186 -> 25, r62h 2.7 GB -> 280 MB.
- 2026-10-11 ~16:50 The region phase made fast enough (the coordinator's fwd-perf findings):
  units BUNDLE their emissions in a unit-local table (route, s, x, o, t) -> mask and hand
  each route its bucket; the region phase dedups per unit bundle (one region-table probe +
  OR) and names targets by their table entry; a dedicated encoder (dense target ranks,
  packed 24-B sort keys) writes the block; no route-level bundle table (it was DRAM-bound
  and merged ~17% of bundles: 0.101 -> 0.113 B an edge at f57); the transfer merge takes
  each worker's NEW pairs only and writes xfer.bin once a frame; region jobs of slice k
  run with the units of slice k+1 (pipeline). Reference frame r62h57 f56->f57, 16 threads,
  interleaved, load 27-34: develop total 7.8-8.1 s (one 10.5), this 3.8-5.7 s (one 10.9
  outlier, a load spike in its units); f/e lines identical. t0 193/193; gates OK.
- 2026-10-11 ~17:25 Region phase and pipeline (05d99f1 + this): chunks carry their unit's distinct
  targets (deduped once a chunk) and sources; bundles are 24-B index records; the encoder ranks
  by dense arrays and GROUPS records by (mask, offset, transfer) (21k-135k groups for 1-2.6M
  bundles at f57) instead of sorting the block - byte-identical output; maps keyed by packed
  keys use rustc-hash 2 (1.x: long probe chains). A CONTINUOUS pipeline: one scope a frame,
  region jobs before units, a slice gathered (transfers, routes, jobs) by whoever ends its last
  unit, in order; transfers merged per slice from each worker's pairs met in it (`upto`).
  Region worker-s at f57 ~4-5.5 (31-39 at 8dcaa56); wave idle 1-3%. Reference frame A/B
  (interleaved, load 26-38, spiky): develop min 3.13 s median ~5.7; this min 3.0 median ~4.2.
- 2026-10-11 ~17:45 Determinism (room (1,0) r0sxh f0-50): 3 vs 32 threads ckhash --edges identical, frame files byte-identical, xfer.bin identical; resume f35->f50 identical; gates/raise.sh 1,0 55 56 58 OK. t1/t2/big ok (metrics re-pinned: storage keys only).
- 2026-10-11 16:58 Frame end: `RegionTable::finish_frame` renumbers only the new entries' index
  slots (a full reindex only when provisional keys were rekeyed) - the room (1,0) A/B showed the
  frame end at 11.5 s against develop's layer 7.2 s. The `[fwd]` line's `regions` is now the region
  jobs' summed worker time (it was never set), `rss wave` sampled again; the UI export reads
  `layer` as the storage's own time. Room (1,0) f44: posgraph, ckhash, frame and edge files
  byte-identical. t0 193/193.
- 2026-10-11 17:50 Determinism to the byte, measured costs:
  * Units are a 256th of the frame (1024-4096 lanes), no longer of the workers: units cut the
    bundles and slices number the transfers, so the edge files and `xfer.bin` were the same only
    at one thread count. A route's segments are filed in slice order (its jobs could run out of
    order). Room (1,0) r0sxh f0-60 and r0sx f0-56 (2 slices a frame from f50): 3 vs 32 threads and
    a resume give byte-identical frame files, edge files and xfer.bin; raise.sh 1,0 55 56 58 OK.
    t1 at 8 and 16 threads: identical metrics (the header no longer names the threads).
  * A region job owns its route's chunks (no shared slice vector); a unit's bucket vectors are
    kept for the next unit (f57: 6.8M -> 4.3M page faults over 3 reps; chunk recycling saved
    0.5M more and was dropped: no measurable time). The backward's fold merges the workers'
    sorted runs (stable sort): r42 h71 43.6G -> 41.0G instructions.
  * Measured and NOT kept: skipping a block without changed targets (1452 of 5827 r42 blocks,
    7% fewer bundles parsed, read time unchanged); `rewrite bundle-orders` deleted (its census is
    in plans/edges-target.md).
  * MIMALLOC_PURGE_DELAY=10 instead of safe-run's 0 (not changed; for the coordinator): f57 page
    faults 6.7M -> 2.2M per 3 reps, frame ~8% faster, peak +5-10% (4.22 -> 4.42-4.66 GB);
    r42 backward CPU -19% (8.3 -> 6.7 s), faults 402k -> 35k.
- 2026-10-11 18:10 Gates and docs. big ok (f57 4.18 s at load 36); t0 + ignored 196/196. The
  backward's round is sized in bytes (20M 40-B emissions, 0.8 GB, as develop's 32M 24-B ones).
  BENCHMARK_DATA.md 2026-10-11, plans/edges-target.md (numbers, the backward's memory per node,
  the state-id proposal), lessons.md (what was measured and dropped), CLAUDE.md and
  storage-v2.md pointers. Full-search A/B, two rounds (develop first, then this first): every
  [gate] file identical, known routes survive; walls within the load's noise (9-45).
