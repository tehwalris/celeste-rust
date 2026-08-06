# Iterative precision refinement for room optimality

The plan for closing the gap between the abstract bound (room (1,0):
frame 90) and the concrete optimum (frame 100), agreed 2026-08-06.
This is plans/strategy.md's inner/outer loop made concrete against the
current machinery. Reference implementation of the band mechanics: the
2022 solver (celeste-rust-old, src/main.rs:174-430, state_table.rs) -
hard to read but performant; we take the structure, not the code.

## Objects

- **Row**: one lane's complete canonical value tuple at a frame
  boundary = one distinct game state. Identified by a 128-bit hash
  (two seeded hashes, shape mixed in). Every row gets a **dense id**
  in discovery order (`interpreter/row_table.rs`).
- **R(f)**: states reachable from spawn at frame f. We use the
  over-approximation **R~(f) = {row : earliest arrival <= f}**, which
  needs no monotonicity assumption (reachable at f implies earliest
  arrival <= f). Because ids are discovery-ordered and the row table
  records the id counter at each frame boundary (watermarks), R~(f)
  is the PREFIX `id < watermark(f)` - one integer, nothing stored.
- **B(f)** (per horizon N): states at frame f from which the exit is
  still reachable by frame N. Inherently per-frame (it encodes time
  remaining). Stored as bitmaps over dense ids (~19 MB / 1.5e8 rows).
- **Band(f) = R~(f) ∩ B(f)**: where a finer search is allowed to be.

## Soundness stance

Every set above over-approximates. That is the only property any pass
needs: "band empty at horizon N" proves no concrete solution at N.
Achievability is NEVER concluded from the abstract side - a claimed
solution is established only by a fully concrete input-sequence replay
(concrete_run). Slack anywhere (non-monotone rooms, loose widenings,
hash risk ~1e-23) makes bands looser, never wrong.

## The loops

Outer, over horizon N (start at the k=0 abstract bound, 90):
  run the inner loop; if any level's band empties, N is impossible ->
  extend the k=0 forward pass one frame (incremental) and repeat with
  N+1. First N whose fully-concrete level yields a witness = optimum.

Inner, over rem precision k = 0..16 fractional bits:
1. **Forward at precision k**, admitted only inside Band_{k-1}: at
   each boundary, coarsen each row to k-1 bits and drop lanes whose
   coarse row is not in Band_{k-1}(f). (k=0: no restriction - that is
   today's frontier search. Precision k widening: rem -> interval
   [q, q + 2^-k), q = rem truncated to k bits; k=0 is the historic
   widening, k=16 is CELESTE_EXACT_REM.)
2. **Backward sweep** producing B_k(f): B_k(N) = rows that have won.
   For f = N-1 down: candidates = R~_k(f); run them ONE frame forward
   with the origin column attached (the deopt-v2 lane-provenance
   machinery, unchanged); keep exactly the candidates with at least
   one successor row in B_k(f+1). No predecessor edges are ever
   stored - the mapping is recomputed per sweep. Cost of a sweep ~
   one forward re-run of the reachable set (~12 min at k=0).
3. Band empties at any point -> horizon impossible. Otherwise k+1.

Why this beats brute force in the band: the unrestricted exact-rem
forward pass measured 13x lanes at f42 and still compounding
(intractable ~f55). Each +1 bit multiplies rem resolution by 2 but the
band from the previous level cuts almost all of it; the filter
tightness is the whole cost model.

## Storage and checkpoints

- Row table: `row hash -> id: u32` + per-frame watermarks. ~20 B/row
  in RAM (~4.5 GB at full-room scale).
- B_k(f): bitmaps over ids, per frame, per level; ~19 MB x frames.
- Checkpoints (bench --checkpoint-dir / --resume): JSON strictly for
  metadata (writer commit, FORMAT_VERSION, config fingerprint: recipe
  hash, lua hashes, partition cells, flags, horizon, k; counts and
  per-file byte lengths). Tables are columnar fixed-width
  little-endian binary in id order (id implicit), zstd frames with
  content checksums. Boundary states via serde binary behind the same
  strict version gate. Reader refuses ANY mismatch loudly.

## Build order and status

ALL DONE (2026-08-06). The pipeline converged on room (1,0):
CONCRETE OPTIMUM = frame 100, all levels k=1..16 (16 = exact rem)
winning at horizon 100 with sweep min(e+g) == forward first-win at
every level. L-curve: L(1)=94, L(2)=97, L(3)=L(4)=98, L(5)=99,
L(6+)=100. Gates: `rewrite trace-witness` passes all 17 levels at
every frame; `rewrite extract-tas` walks the 327-row exact band and
reproduces the reference TAS byte-identically. Driver: `ladder.sh`.
Full story: plans/overnight-2026-08-06.md.

1. DONE: row table with dense ids + watermarks; subtract_visited
   rewired; f45 frontier series bit-identical to the set-based code.
2. DONE: checkpoint save/load + bench --checkpoint-dir/--resume
   (FORMAT_VERSION 2 after the rem-straddle canonicalization fix).
3. DONE: backward sweep (origin-tagged replay, CSR edges, reverse
   BFS); k=0 horizon-90 sweep matched the forward first-win exactly.
4. DONE: precision-k rem widening + straddle split + band-filtered
   forward.
5. DONE: ladder driver + witness trace + extract-tas.
