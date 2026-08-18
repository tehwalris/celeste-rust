# Campaign cost plan (2026-08-18)

Written after the first top-down cost breakdown of the whole pipeline
(BENCHMARK_DATA.md, "Top-down campaign cost breakdown"). Everything here
is priced against measured numbers, not guesses. Setup for all figures:
room (1,0), synthetic win at (64,44), H=72, 30 cores.

Level 0 at H=72: forward 248 s, pos-graph 242 s, sweep 276 s (~12.8 min).
Banded levels ~100-135 s each; a 16-level ladder at ONE horizon ~40 min.

Where the time actually is (phases are inclusive; `fwd.*` inside the
sweep are sub-phases of `bwdt.replay`):

| | forward 248 s | sweep 276 s |
|---|---|---|
| `bwdt.replay` | - | 190 s (69%) |
| ...`fwd.interpret` | 195 s (79%) | 88 s (32%) |
| ...`fwd.merge` | 8 s (3%) | 91 s (33%) |
| `bwdt.keys` / `bwdt.index` | - | 34 s / 25 s |
| boundary stream + save | 31 s | - |

Two conclusions drive everything below:
1. The forward pass is interpreter-bound. The sweep is NOT - it is
   one-third interpreter and one-third merge/regroup, because it
   regroups candidates from many discovery frames into lane groups the
   forward pass never used.
2. The pos-graph stage is pure waste in its default configuration.

---

## P0 - pos-graph fusion by default (hours, -46% of level 0)

`bench --record-pos-graph` records the table DURING the forward pass:
265 s vs 248 + 242 = 490 s at H=72, and the recorded table is a strict
SUPERSET (166,456 pairs / 4,189 cells vs 154,937 / 3,928 - it sees the
spawn's first move, which a replay starting from the frame-1 batch
cannot). A larger table is sound by construction: it only shrinks the
sweep's candidate set, and the EXPANSION is what establishes an edge.

Steps:
1. Run `posgraphcheck.sh` with `ROOM=1,0`. **DONE 2026-08-18, all four
   checks pass**: rows identical (900,028, onlyA=0 onlyB=0), `g`
   IDENTICAL as a function of the row, table a strict superset (+1
   pair), resume correct.
   - The gate was itself broken and said so misleadingly: its row and
     `g` comparisons read `f<H>/visited.bin`, which the DEFAULT mmap
     visited engine no longer writes, so both died on a missing file and
     printed "*** DIVERGED ***" - loud, but for the wrong reason. Fixed
     by pinning `CELESTE_VISITED_ENGINE=map` in the script; the real fix
     is porting tools/rowdiff.py + gjoin.py to the rowkeys sidecars,
     which task #122 needs anyway.
2. Flip ladder.sh's `FUSE` default to on. **DONE**: on for rooms (1,0)
   and (2,0) - the gated ones - and the replay elsewhere with a printed
   reason. FUSE=0/1 still force either path.
3. Gate room (0,0) the same way before its next campaign.
4. Only then consider deleting the standalone replay builder. It is the
   conservative path for an ungated room and for extending a table
   written by an older run, so "the only impl" should wait until every
   campaign room is gated. Cheap insurance; do not rush it.

## P0b - stop rebuilding the pos-graph per k level (hours, -16 stages/horizon)

The pos-graph is HORIZON-independent by construction (pos_graph.rs says
so: it reads no horizon, band or `g`). It should also be PRECISION
independent in the direction that matters: level 0 is the coarsest, so
its reachable-position set is a superset of every k level's, and a
superset is sound. Today ladder.sh rebuilds it per (k,H) - measured
19-22 s x 16 levels x every horizon.

Steps:
1. Verify the superset claim on real data. **DONE 2026-08-18, it
   holds exactly**: level 0 has 166,455 pairs, k=1 has 34,137 and k=2
   has 21,579, and both are strict SUBSETS - zero pairs outside level
   0's table. So the level-0 table can serve every k level.
2. Let `prepare_pos_graph` accept a table whose fingerprint differs only
   in rem/spd precision, with the level-0 table passed explicitly
   (`--pos-graph-from DIR`), rather than loosening the fingerprint check
   globally - the check has caught real mistakes.
3. Gate: `g.bin` identical at k=1..3 with the reused table vs rebuilt.

## P1 - one engine behind one interface (days, the big one)

"Make the backward pass use the fast forward" is not a small change,
because of a structural fact: **native-probe depends on celeste-rust,
not the other way round.** The kernel engine, the (shape, rows) block
model, import, boundary/dedup/merge and the generated kernels all live
in the probe. The campaign literally cannot call them.

The work, in order:
1. **Move the engine into the main crate** (or a crate the main crate
   depends on): runtime2's block model + boundary/dedup/merge, import,
   kernel.rs, kernel_gen_*. Decide the generated-code story - today
   `gen.rs` (29k lines) and `kernel_gen_*.rs` are gitignored build
   artifacts of a manual `transpile` run; the main crate needs either a
   build.rs or checked-in generated sources. This is the real cost of
   P1 and it should be scoped before anything else.
2. **Define the one interface both passes call**: `(shape, rows) ->
   [(shape, rows)]` for one frame. The forward loop and the sweep's
   `bwdt.replay` both go through it. Then the kernel benefits BOTH
   without a second integration - this is the whole point of doing it
   as one interface rather than "wiring the kernel into the sweep".
3. **Coverage before speed**: kernels exist for room (1,0)'s player
   classes only. Spawn shapes and rooms (0,0)/(2,0) fall back to the
   interpreter, which is fine (the fallback is the reference) but means
   the win is room-shaped. K5 is the follow-on.
4. **Gates**: byte-identical checkpoints (task #117's gate), `g.bin`
   identical (tsweepcheck.sh), row-key set equality per frame, full
   suite. Nothing here is allowed to change a single row.

Expected value: the forward's `fwd.interpret` 195/248 s and the sweep's
88/276 s. At the probe's measured 5-20x on covered shapes that is a
large fraction of level 0 - but note it does NOT touch the sweep's other
third (P2) or its keys/index (P2b).

## P2 - the sweep's merge third (days)

`fwd.merge` is 91 s of the sweep's 190 s replay - MORE than the
interpreter there. The sweep regroups candidates from many discovery
frames, so it pays merge costs the forward pass does not. Options, in
increasing order of ambition:
1. Apply the pre-dedup lesson from the kernel work (dedup from keys
   BEFORE materializing rows) to the interpreter's merge path.
2. Preserve the forward pass's grouping for candidates instead of
   regrouping - cheaper merges, and it also shrinks the batch-invariance
   surface the sweep's soundness argument has to cover.
3. `bwdt.keys` 34 s + `bwdt.index` 25 s: the same generated-key idea
   that took the probe's row hashing off the profile.

## P3 - ladder-level waste (unmeasured, list only)

- `--variant` is not accepted by `pos-graph`/`sweep` (task #114), so
  those stages run the base recipe. On room (0,0) pos-graph was the
  largest stage at 3567 s; P0 removes most of that, but the sweep's
  replay still cannot use a variant.
- Each refuted horizon re-runs the sweep from scratch (276 s at H=72,
  and it grows faster in H than the forward does: 20 s at H=60, 160 s at
  H=70). Whether `g` can be extended across horizons instead of rebuilt
  is a research question, not a cleanup - park it until P1/P2 land.

## Suggested order

P0 and P0b first: hours of work, no new machinery, and they remove
roughly half of level 0 plus 16 redundant stages per horizon. Then P1,
scoped honestly - the crate move is the gating cost, not the kernel. P2
after P1, because P1's interface is where the merge work will land
anyway. P3 is a list, not a plan.
