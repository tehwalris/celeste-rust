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
