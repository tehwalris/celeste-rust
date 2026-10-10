# Unified per-region sets against storage v2's lids (2026-10-10)

A design study: Philippe's UNIFIED layout (one persistent set per region
holding its own states and the targets of its edges) against what storage
v2 BUILT (per frame and unit, a table of lids). Branch `storage-unify`
(from `storage-v2` fa2b2dc). The measurement tool is `rewrite
storage-census`, which counts and encodes a finished tree's edges under
every layout below. Counting only: no timing was taken, because the
machine was shared with two benchmarking agents.

## Answer

**Do not build the unified sets, and do not build their persistent hybrids
either.** On real data they do not make the tree smaller than a pure
re-encoding of the built tables. The persistence they add does not
remove a record from the disk. It does add persistent state, a second
probe per lid, entry numbers with holes, and new resume metadata.

What does pay is compacting the built tables. Today they cost 20 B per lid:
a 4 B `starts` entry and a 16 B owner-index entry. Delta-coded with varints
and a sample every 32 entries, they cost ~7 B per lid. That saves 9.0% of
room (1,0)'s edge store at h99 (3.2 GB), 5.5% on (6,2) 100% to f57 and 7.9%
on (4,2). Only the edge file format changes. Staging is at the end.

Why the unified layout loses:

- **Region-scoped blocks are 4-9% bigger** than the built per-unit blocks
  for the same edges: (6,2) f57 +8.7%, (1,0) f70 +7.5%, (4,2) f60 +3.6%.
  The cause is the transfer field. A region has more distinct transfers
  than a unit of 4096 rows, so its ranks are wider. Cutting a region into
  parts of 4096 sources, each with its own transfer ranks, restores the
  block size exactly (800.5 against 802.2 MB). But it also restores the
  number of target records: 3,795,034 groups against 3,795,565 lids. The
  built units already sit at that point.
- **Each frame's block needs its own target directory** (a group header per
  (block, target)) whatever names the targets. Persistence only changes
  what the header holds: a local pattern number and a region delta instead
  of (region, entry). Both cost about the same once delta-coded. So the
  reuse of names across frames does not remove a record. The reuse is
  real: 48-61% of the cross-region translations were made in an earlier
  frame.
- **The persistent structures are big and mostly dead.** By (1,0) h99 there
  are 20.9M translations, 2.0x the 10.4M own entries, and 37% of them were
  used in one frame only. Held in memory that is ~0.67 GB, more than the
  whole visited set (0.47 GB). Target-only patterns add another 17% of
  entries.

## The designs, precisely

A state is (shape, key, cell), where the key holds no position. Its id is
`(region, entry, cell-in-region)`. A region is (shape, 8x8 cells). An edge
of frame f leaves a state of layer f-1.

**B, built (storage v2, fa2b2dc).**
- Units are runs of at most 4096 frontier rows in id order. A unit spans
  a few neighbouring regions of one shape, and a heavy region is several
  units.
- Per unit, a LID names each distinct target (target shape, target region,
  key).
- The block holds the unit's edges grouped lid by lid: cell, source lane,
  and transfer rank, as varints.
- On disk, per lid: a `starts` u32 (4 B) and an owner-index entry `(Q, e,
  unit, lid)` (4 x u32, 16 B). The owner index is sorted per frame, and it
  serves as both the translation and the reverse index.
- In memory during the wave: per lid a `Lid` (32 B) and an owner (8 B),
  kept for the frame.
- Owners of old targets are found when the lid is made. New ones come from
  the translation at the frame's end.

**U, Philippe's unified sets.**
- Per region R, one persistent, append-only set D_R. Each entry is a key
  with an own mask. An entry with a non-empty mask is one of R's states;
  an empty one is a target-only pattern.
- A target (Q, k, c) reached from R's sources is named in D_R by key k:
  - if R has k (own or target-only), it IS that entry;
  - otherwise a target-only entry is appended.
- An edge is (source (e_s, c_s) in R, target entry d in D_R, region delta
  Q - R, cell c, transfer).
- For a delta other than 0, the target's id needs the translation T(R, d,
  delta) = Q's entry for k. T is persistent, or recomputed by a key lookup
  in Q.
- Entry numbers are append-only, so old frames' edges stay valid as the
  sets grow.

**Hybrids considered.**
- **H1, unified sets with edges per frame.** This is U as measured: every
  variant keeps per-frame edge files, and persistence applies only to the
  names.
- **H2, per-region ghost dictionaries.** The persistent target names live
  apart from the own set, as (R, Q, e) -> local ghost number. Same counts as
  U's T. There are no holes in the own entries, and "converting" a ghost to
  an own entry never arises: the delta tells them apart.
- **H3, own-entry numbers for in-region targets** (no lid, no index entry)
  **plus a ghost table** for the rest.
- **H4, B-compact.** The built structure, with the owner index delta-varint
  coded and `starts` as varint lengths, each with an absolute sample every
  32 entries for random access.
- **H5, shape-global key numbers.** A state's entry field is a per-shape
  number of its key. Then every target is named without any translation,
  wherever it lies. This was suggested by the data (below), not by the
  brief.

## Method

```bash
# trees (storage-v2 code; the (1,0) h99 tree was the lead agent's r10_v4, since deleted)
CELESTE_HUNDRED=1 CELESTE_LOADING_JANK=2 CELESTE_LEVEL_MINUS_ONE=94,5 \
  ./safe-run.sh -- rewrite forward --to 57 --room 6,2 --level r0sxhf --checkpoint-dir T62
CELESTE_LEVEL_MINUS_ONE=71,5 ./safe-run.sh -- rewrite search --room 4,2 --ceiling 71 \
  --level r0sxhn,r0sxh --prefer tas/room_4_2_exit_frame_71.txt --checkpoint-dir R42   # OPTIMAL 71
# the census: a line per frame, totals, persistence; --encode F re-encodes frame F's
# edges per source region (modes 0-3 below) with the built blocks' scheme
./safe-run.sh -- ./target/quick/rewrite storage-census --level-dir T62 --to 57 --encode 45,57
```

The census reads the edge files and the storage metadata. It counts
exactly: lids, per-region references, target-only patterns (new, reused,
later own), translations (new, reused), pending names, and every table's
bytes in the built and the alternative encodings.

Sanity checks:
- f57 lids 3,795,565 and edges 257,724,013 equal the forward's `[fwd]`
  line.
- The census over the r42 tree is identical on fa2b2dc and on the WIP it
  was first run on.

## The numbers

### Counts over whole forwards

Units of the counts:
- `lids`: per (unit, target) records, as built.
- `P`: distinct (source region, target) records. These would be the lids
  if units were exactly regions.
- `in`: the part of P whose target lies in the source's region.

| | (6,2) 100% f1-57 | (1,0) f1-99 | (4,2) r0sxhn f1-71 |
|---|---|---|---|
| edges | 1,515M | 10,330M | 158.8M |
| edges with the target in the source's region | 66.3% | 65.4% | 70.2% |
| edges changing shape | 2.9% | 0.07% | 1.5% |
| lids (B) | 20.9M | 242.8M | 2.99M |
| lids reached from another region (H3's ghosts) | 13.8M (66%) | 152.8M (63%) | 1.85M (62%) |
| P (region records) | 12.2M (58% of lids) | 85.8M (35%) | 2.24M (75%) |
| P in-region / cross-region | 4.07M / 8.10M | 31.7M / 54.1M | 0.86M / 1.37M |
| distinct targets, summed per frame | 5.50M | 43.0M | 1.13M |
| U: cross-region patterns that are an own key of R (shared) | 3.21M | 25.3M | 0.64M |
| U: target-only patterns made | 385k | 1.79M | 47k |
| ... of which used in one frame only / later own | 162k / 75k | 356k / 751k | 18k / 15k |
| U: translations made (persistent T) | 4.18M | 20.93M | 0.65M |
| ... of which used once; reuse share of per-frame uses | 2.30M; 48% | 7.76M; 61% | 0.32M; 53% |
| own entries at the end | 2.19M | 10.41M | 0.39M |
| T / own entries | 1.91x | 2.01x | 1.67x |
| distinct keys per shape (H5) | 313k (+23k) | 418k | 41k |
| lids naming an entry new this frame / a key new to its shape | 6.05M / 3.44M | 59.0M / 6.6M | 0.90M / 0.42M |

Per frame, (1,0). "tonly" means target-only, "X" is translations, and
"cum" is the running total.

| f | edges | lids | P | in | D shared | tonly new | tonly reused | X new | X reused | own cum | tonly cum | X cum |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 40 | 8.5M | 329k | 116k | 45k | 29k | 7.5k | 7.8k | 38k | 33k | 116k | 30k | 151k |
| 50 | 54.7M | 1136k | 474k | 168k | 128k | 16.9k | 29.4k | 146k | 160k | 662k | 140k | 1121k |
| 60 | 93.6M | 2225k | 836k | 309k | 229k | 26.4k | 40.4k | 241k | 286k | 1593k | 292k | 2994k |
| 70 | 274.7M | 6933k | 2138k | 788k | 622k | 44.9k | 61.4k | 505k | 844k | 3829k | 524k | 7232k |
| 80 | 228.3M | 4855k | 1942k | 716k | 586k | 29.1k | 46.0k | 396k | 830k | 5949k | 671k | 11944k |
| 90 | 217.7M | 4869k | 1833k | 690k | 567k | 31.3k | 40.9k | 446k | 697k | 7862k | 837k | 15875k |
| 99 | 365.9M | 8359k | 2651k | 974k | 811k | 36.4k | 53.4k | 604k | 1073k | 10414k | 1035k | 20930k |

Per frame, (6,2) 100%:

| f | edges | lids | P | in | D shared | tonly new | tonly reused | X new | X reused | own cum | tonly cum | X cum |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| 40 | 11.9M | 224k | 162k | 44k | 27k | 14.5k | 16.6k | 58k | 60k | 89k | 49k | 195k |
| 45 | 20.5M | 268k | 196k | 69k | 52k | 5.9k | 4.7k | 66k | 61k | 281k | 127k | 639k |
| 50 | 71.7M | 890k | 561k | 191k | 156k | 13.0k | 10.3k | 196k | 174k | 650k | 161k | 1312k |
| 55 | 183.0M | 2577k | 1368k | 468k | 379k | 33.1k | 31.7k | 468k | 432k | 1554k | 249k | 3002k |
| 57 | 257.7M | 3796k | 1901k | 647k | 527k | 41.0k | 49.0k | 632k | 622k | 2186k | 310k | 4178k |

What these tables show about growth:
- Target-only patterns are few. Of a frame's cross-region patterns, 6-10%
  are new target-only ones, and 79-86% are one of R's own keys anyway. So
  U's set grows mostly through its own states.
- The translations dominate the persistent state. They grow by 0.4-0.6M a
  frame at the peaks, and only half of a frame's uses hit an existing one.

### Edge-block bytes: per unit against per region (`--encode`)

Each frame's edges are re-encoded with the built blocks' scheme. A group is
a target, sorted by cell, then source, then transfer rank. The `mode`
decides the source naming and the block scope:
- 0: per region, the source as its rank among the region's sources;
- 1: per region in parts of 4096 sources, with transfer ranks per region;
- 2: per region, the source as its persistent local name `entry << 6 |
  cell` (U's "local src index");
- 3: as 1, with transfer ranks per part, as a unit ranks its own.

| frame | built (units) | mode 0 | mode 1 | mode 2 | mode 3 | groups: lids / mode 0 / mode 3 |
|---|---|---|---|---|---|---|
| (6,2) f57 | 802.2 MB (3.11 B/edge) | 871.8 (+8.7%) | 870.0 | 935.4 (+16.6%) | **800.5 (-0.2%)** | 3.796M / 1.901M / 3.795M |
| (6,2) f45 | 60.06 | 62.98 (+4.9%) | 63.33 | 68.21 (+13.6%) | 60.44 | 268k / 196k / 255k |
| (1,0) f70 | 841.0 | 904.4 (+7.5%) | - | - | - | 6.93M / 2.14M |
| (1,0) f90 | 648.0 | 691.1 (+6.7%) | - | - | - | 4.87M / 1.83M |
| (1,0) f40 | 23.25 | 24.94 (+7.3%) | - | - | - | 329k / 116k |
| (4,2) f60 | 33.74 | 34.94 (+3.6%) | 35.19 | 38.62 (+14.5%) | 33.75 | 217k / 162k / 203k |

Three readings:
- Mode 1 against mode 3 isolates the cause: the transfer ranks, not the
  source width.
- Mode 2 shows that naming sources by their persistent (entry, cell) costs
  another 7-8%.
- Modes 3 and 0 bracket the trade-off. Fewer, bigger blocks have half the
  target records and 7-9% more edge bytes. The edge bytes are 85% of the
  edge store, so that trade loses.

Modes 1-3 could not be run on (1,0): its h99 tree was deleted before that
run. Modes 0 and 3 on two rooms are enough to settle the question.

### Disk: the edge store under each design, whole forward

The built bytes are measured. B-compact is measured too:
- the owner index is delta-varint coded, measured exactly;
- `starts` become varint lengths, measured exactly;
- add a 12 B sample every 32 entries of each, computed.

U and H5 are estimated as follows.
- **U, mode 0:**
  - blocks scaled by the measured per-room ratio;
  - region headers, measured with 2 B lengths;
  - a reverse index `(Q, e, R, rank)`, delta-coded and measured;
  - new target-only keys at 24 B each;
  - new translations at 6 B each.
- **U, mode 3:** blocks as built; the headers and the index cost as in
  B-compact, plus the same target-only keys and translations.
- **H5:** the same as B-compact with names in the headers. The named
  headers are measured (`hdr_global` 555.5 MB for (1,0), equal to direct
  (Q, e) names at 550.7 MB). It still needs the reverse index, so it does
  not undercut B-compact.

| design | (6,2) f1-57 | (1,0) f1-99 | (4,2) f1-71 |
|---|---|---|---|
| blocks (built) | 4,626 MB | 30,904 MB | 461.9 MB |
| target tables as built (owner 16 B + starts 4 B per lid, xfers) | 461 MB | 4,994 MB | 65.7 MB |
| **B as built, total** | **5,087** | **35,898** | **527.6** |
| B-compact: owner index varint, starts u32 | 4,857 (-4.5%) | 33,231 (-7.4%) | 492.7 (-6.6%) |
| **B-compact+: also starts as varint lengths** | **4,808 (-5.5%)** | **32,670 (-9.0%)** | **485.7 (-7.9%)** |
| U, region blocks (mode 0) | ~5,160 (+1.5%) | ~34,100 (-5.0%) | ~508 (-3.8%) |
| U, region parts (mode 3) | ~4,860 (-4.5%) | ~33,070 (-7.9%) | ~493 (-6.5%) |
| H3 on built units (no index for in-region lids) | no gain: the BFS's reverse index needs every (unit, target) | | |

B-compact+'s target tables are ~7 B a lid against 20 B:
- (1,0): owner index 1,217 MB, lengths 289 MB, samples 121 MB, transfers
  135 MB;
- (6,2): owner index 104 MB, lengths 25 MB, samples 10 MB, transfers 42 MB.

At their best, U's persistent names tie with this; at worst, they lose.

### Memory during the wave

Built, transient for the frame: a `Lid` plus an owner, 40 B per lid. That
is 152 MB at (6,2) f57 (3.80M lids) and 342 MB at (1,0) f98 (8.56M, the
peak). The worker's per-unit hash tables are L2-sized and reset per unit.
Against the wave's whole RSS growth, measured (`[fwd]` rss start -> wave):
- (6,2) f57: 1.12 -> 2.66 GB (lids ~10% of the growth);
- (1,0) f99: 1.35 -> 3.19 GB (~18%).

The rows of the requests dominate.

U, persistent and growing:
- T at ~32 B per entry (a 12 B key and a 4 B value, at load 0.5): 134 MB at
  (6,2) f57 and 670 MB at (1,0) f99. The visited set itself is 0.10 and
  0.47 GB there.
- Live target-only entries at ~40 B: 12 MB and 41 MB.
- The unit-local caches for misses, since the sets are read-only during the
  wave (below), on top of that.

Without a stored T, the reads recompute it: one key lookup per
cross-region record, 54.1M over (1,0).

### Lookups and translation work per frame

The work at (6,2) f57:

| work at (6,2) f57 | built | U (unit caches over D_R and T) | U (D_R probed per emission, N2/N3 style) |
|---|---|---|---|
| emission probes | 257.7M into the unit's L2-sized lid table | 257.7M into the unit cache | 257.7M into D_R (L2/L3 while R's emissions run) |
| per lid miss | 3.80M visited probes (~0.1-0.2 us each) | 3.80M x 2: D_R by key, then T | per pattern/translation miss: D_R and T or Q |
| frame-end inserts | requests -> 6.74M new states (one worker per region); pending lids re-probed | the same, plus 632k translations and 41k target-only patterns | the same |
| block names known when written | lids (unit-local, owners at the end) | (d, delta): pending for new patterns -> a fix-up table or blocks held to the end | same |

The lead's own bound applies here. All lid creations together are worth
~20-30 ms of a 0.86 s units phase at 16 threads (3%). No variant can save
more than that, and U adds a second probe per miss. N2/N3's per-emission
region sets ran at ~4-6 ns an emission in a harness without kernels, and
the built lid table is in the same range. Expect no measurable change in the
wave either way. A claim either way needs a `bench-storage` A/B on a quiet
machine, which this study did not do.

### What the backward reads per pass

`arc_dp::load` reads the edges in three passes:
- a BFS of `preds_at` probes: per marked target and frame, a binary search
  of the frame's owner index, then one lid decoded per naming unit;
- two streaming passes over every unit: owners derived per frame (8 B per
  lid, transient) and the blocks decoded, skipping units with no marked
  source.

The arc DP then runs on the in-memory CSR and reads no edge file.

| | built | B-compact+ | U (mode 0) |
|---|---|---|---|
| bytes streamed per pass, (1,0) h99 (upper bound: no unit skipped) | 30.9 + 3.9 (owner) + 1.0 (starts) = 35.8 GB | 30.9 + 1.6 = 32.5 GB | 33.1 + 0.8 = 33.9 GB, plus T in memory (0.67 GB) or 54M key lookups |
| BFS probe | binary search over 16 B entries | binary search over the samples, then <= 32 varint entries | the same over `(Q, e, R, rank)` |
| records the owners pass decodes | 242.8M (fixed width) | 242.8M (varint, ~1-2 ns each) | 85.8M headers, plus a T or key lookup per cross-region record |

## Every consumer under U

| consumer | under U | cost or risk |
|---|---|---|
| the backward pull (`arc_dp::load` passes) | per region block: source (R, e_s, c_s) is an id directly (no row range); target (d, delta, c) -> (R, d, c) or T(R, d, delta) | T resident or a key lookup per record; blocks 4-9% bigger (or parts = lids again) |
| the reverse walk (`edges::bfs`) | needs a per-frame reverse index `(Q, e) -> (R, d)`; persistent "namers" would probe frames where R did not name the target | the same index as built (P records instead of lids); no saving |
| marks and ranks | masks per entry per region: target-only entries are empty masks (17% of entries); ranks unchanged | memory only; with H2 (separate ghosts), none |
| CELESTE_TRIM_ROWS | unaffected: the edge files and metadata are kept, as now | - |
| resume | metadata must also record each frame's new target-only entries and translations; the visited set rebuild restores D_R and T; later frames' files dropped as now | new on-disk sections, resume code, a test |
| raised horizons | a raise appends new own entries, target-only patterns and translations after later frames' numbers (append-only, as built's entries already are); a target-only entry gaining its first own cell in a raise just sets a mask bit | must refuse a hit from a later layer as now (`StateLayers` by id: unchanged) |
| the objects ladder filter | key-based (`MarkFilter` on the row); its verdict cache keyed (d, delta, cell) per unit instead of (lid, cell) | none |
| level -1 drops | before keying, notes by source id | none |
| determinism and canonical numbering | D_R entries and T appended at the frame's end in key order per region; the blocks per region are schedule-free (nicer than built, whose unit cut depends on the thread count) | only if nothing is appended during the wave (below) |
| heavy regions | must be cut into parts for balance (the heaviest regions have 10^5+ frontier rows); parts share D_R read-only | parts with per-part transfer ranks = the built lid count again (mode 3) |
| concurrency in the wave | D_R and T read-only during the wave, as the visited set is; a unit's misses (new pattern, new translation, new key) kept unit-local and appended at the frame's end in canonical order | the unit-local lid table survives, for the misses; blocks written during the wave name misses by unit-local numbers -> a per-unit fix-up table (pending lids: 29% of lids at (6,2), 24% at (1,0)) or blocks held to the frame's end (8 B per edge: 2-3 GB at the peaks) |

## The hard problems

1. **Ids of target-only patterns.** If they share D_R's entry numbers ("IS
   that entry"), entries stop being dense over states:
   - the marks/ranks masks carry 17% empty entries;
   - the resolver must know which entries are own;
   - the metadata must persist them.

   Separate ghost dictionaries (H2) avoid the holes. They need no
   conversion either: an own entry and a ghost of the same key name
   different states, and the delta tells them apart.

2. **Old frames' edges as the sets grow.** They stay valid because all
   numbering is append-only, the built design's entries included. Raises
   append after later frames' numbers. Nothing ever renumbers.

3. **Writes to R's set while R's units run.** If units append, then:
   - numbering depends on the schedule;
   - a heavy region's parts race;
   - a pattern made by one part is invisible to another.

   The only sound rule is the built one: read-only during the wave, misses
   held per unit, canonical appends at the frame's end. That rule brings
   back exactly what U meant to remove, a unit-local table and a fix-up
   per unit.

4. **Persistent growth.**
   - T reaches 2x the own entries, and 37-55% of its entries are used once.
   - Eviction would break the naming of old frames unless T is also on
     disk per frame. That brings back a per-frame translation table.

5. **Block scope.**
   - Region-wide blocks widen the transfer ranks by 4-9%.
   - Region parts give back the record count.
   - The literal (entry, cell) source name widens the blocks by another
     7-8%.

None of these is unsolvable. But the solved version of U is the built
design with persistent extras that the numbers do not pay for.

## Hybrids, estimated

| hybrid | target records on disk | bytes vs B-compact+ | other cost |
|---|---|---|---|
| H1: unified sets, edges per frame | P (mode 0) or ~lids (parts) | +4.4% ((1,0)), +7.3% ((6,2)) / +1.1-1.5% (parts) | everything in "hard problems" |
| H2: persistent ghost dictionaries apart from the own set | same as H1 | same | T: 2x own entries, resident |
| H3: own-entry numbers for in-region targets + ghost table | lids reached from another region: 63-66% of lids in tables, but the BFS's reverse index still needs every (unit, target) | ~0 | pending fix-ups for new entries |
| H4: B-compact+ | lids, ~7 B each | 0 | `preds_at` decodes <= 32 varint entries after a sample search; `owners()` decodes varints |
| H5: shape-global key numbers | lids as B-compact, names direct in headers | ~0 (headers 555 MB, as direct (Q, e)) | marks/ranks/visited need (region, g) -> slot maps; ids sparse per region |

H5 is the data's surprise. The distinct keys per shape are 25x fewer than
the entries in (1,0) (418k against 10.4M), 7x in (6,2) and 9.5x in (4,2).
So in practice a per-region dictionary is the same few hundred thousand
keys repeated across regions. Shape-global numbers would make every target
name translation-free and fix-ups rare: lids naming a key new to its shape
are 2.7% of lids in (1,0), against 24% naming a new entry. But it buys no
bytes over H4. It is worth keeping in mind for a key-level cache in the
backward, not for storage.

## Recommendation and staging

1. **Merge storage v2 as it is.** Nothing here argues against its
   structure. Its lid tables are the right records, just encoded wide.
2. **Stage 1: compact the edge file's tables (VERSION 6).** Only
   `storage::edges` changes:
   - the owner index is delta-varint coded (region delta, entry delta,
     unit, lid), with an absolute `(Q, e, offset)` sample every 32 entries
     for `owners_of`'s binary search;
   - per unit, `starts` become varint lengths with an absolute start every
     32 lids.

   Expected savings: (1,0) h99 -3.2 GB of 35.9 (-9.0%), (6,2) f57 -0.28
   GB, (4,2) -42 MB.

   Verify:
   - `ckhash --edges` e-lines identical;
   - the arc gate, BFS counts and marks identical;
   - `arc-check`, the known-route check, determinism at 3 and 32 threads;
   - an A/B of `bfs` and `arc_dp::load` wall times on (1,0) h99, which must
     not regress by more than a few %.

   About a day's work.
3. **The bigger lever is the blocks, not the targets.** Blocks are 86-91%
   of the edge store, and the transfer field is what makes them grow. N3
   coded transfer + shift in 7.0 bits an edge, through the (source entry,
   target entry) pair's history. That is the next thing to measure against
   the built rank varint if disk matters. It needs no change to the
   naming.
4. **Not staged: U, H1, H2, H3, H5.** Keep `rewrite storage-census`. If the
   units ever change for another reason (region-aligned units, other unit
   sizes), it re-prices every layout above on a real tree in minutes.

**Risks of the recommendation.**
- Random access into varint indexes is slower per probe. The 32-entry
  sample bounds it, but the BFS and `owners()` must be measured.
- The numbers come from three rooms: an object room (4,2) at r0sxhn, and
  no platform room or gemskip-scale room. The ratios are consistent across
  the three (tables 9-14% of the edge store; compact tables ~1/3 of
  built).
- No timing was taken. The wave-speed claims are the lead's bound and the
  N2/N3 harness numbers, not a measurement of these variants.
