//! The backward sweep, in TIME-EXPANDED space. `sweep.rs` holds what it
//! produces; this is how.
//!
//! It replaced an edge graph, which inverted the successor relation by
//! materializing it: every `(src row, dst row)` pair the forward pass ever
//! took, 10,072,724,145 of them and 58 GB of CSR shards for room (1,0), and
//! an outright OOM on room (0,0), where ONE frame produces 644,653,017 of
//! them. This answers the same question - "who steps into the rows that just
//! got marked?" - by re-deriving successors each frame from a small
//! POSITIONAL over-approximation of the predecessor relation (`pos_graph`),
//! and stores no edges at all.
//!
//! Nodes are `(row, frame)`, so every edge goes `i -> i+1` by construction.
//! Writing `R(i)` for the rows with earliest arrival `<= i` (the row table's
//! watermark prefix - one integer, not a stored set),
//!
//!   B(H) = the win rows of R(H),
//!   B(i) = { s in R(i) : some successor of s is in B(i+1) },
//!
//! and `B(i) = { s in R(i) : g(s) <= H - i }`, so a row that FIRST enters at
//! frame `i` has `g = H - i` exactly and the sweep's output converts to the
//! same `g` array `save_g` has always written. Only rows with `e + g <= H`
//! ever enter, so a horizon-H sweep produces `g` only where a full sweep's
//! `g` is thresholded at H - see `tools/gdiff.py --threshold`.
//!
//! Two things make it affordable:
//!
//! * **Monotonicity.** `B(i) ⊇ B(i+1) ∩ R(i)`, so a row that has already
//!   qualified is never re-tested and only `R(i) \ B(i+1)` is a candidate at
//!   all. That is what makes the naive time-expanded cost 20.9x the edge
//!   sweep's expansions rather than 39x.
//! * **The positional filter.** Of `R(i) \ B(i+1)`, only rows whose player
//!   position is a recorded predecessor position of a position `B(i+1)`
//!   occupies are expanded: 98.3M expansions at H=100 on room (1,0), 0.46x
//!   the edge sweep's. The filter is an over-approximation and it is the
//!   EXPANSION that establishes an edge, so only cost depends on how tight
//!   it is.
//!
//! Soundness of the regrouping. Candidates at frame `i` come from every
//! earlier discovery frame, so they are expanded in different lane groups
//! than the forward pass used, and grouping can be semantic: a comparison
//! against a widened interval that straddles in ANY lane of a chunk collapses
//! to a whole-value `UnknownBool` and sends the entire chunk down both edges.
//! That is the only known cross-lane operation left, and it is measured -
//! room (1,0) has ZERO mixed collapses and satisfies batch invariance, room
//! (0,0) is 89.0% mixed (see "Batch invariance" in `plans/roofline-plan.md`).
//! So on room (1,0) this sweep's successors are exactly the forward pass's,
//! and its `g` is exactly the edge sweep's, which is the gate. In the event
//! room (0,0) reported zero out-of-table successors too.
//!
//! That paragraph is about to get WEAKER, in the good direction: the
//! interpreter fixes that partition a mixed comparison instead of collapsing
//! it (861c4c7, b8187b6 and follow-ups) remove the mechanism it is about, so
//! there is much less left for a regrouping to change. Re-read it after
//! those land rather than trusting the percentages. Nothing below depends on
//! them: the soundness argument is that EVERY grouping over-approximates the
//! concrete relation, which does not care how coarse any of them are.
//!
//! What survives regardless is the only property the band needs: every
//! grouping over-approximates the CONCRETE transition relation - a finer
//! group merely lets a lane take the branch it would concretely have taken -
//! so the concrete successor of a concrete state is a successor of its row
//! under any grouping, and every row on a concrete winning path still
//! satisfies `g <= H - f`. A coarser grouping can also produce successor rows
//! the forward pass never reached; those cannot be in `B` (a subset of the row
//! table) and are counted, not silently dropped.

use anyhow::{anyhow, Context, Result};
use std::path::Path;

use crate::interpreter::abstraction::win_lane_mask;
use crate::interpreter::deopt_collect;
use crate::interpreter::row_table::RowTable;
use crate::interpreter::state::{State, FILTER_BAND};
use crate::interpreter::value::KeptLanes;

use super::checkpoint;
use super::pos_graph::{self, PosGraph, CELL_COUNT, CELL_WORDS, NO_CELL};
use crate::program::Program;
use super::state_mapping::StateMapping;
use super::sweep::{row_keys, G_UNREACHABLE, SWEEP_ORIGIN};
use super::run::AbstractRun;

/// Every saved boundary state, plus where each row id lives in them.
///
/// This is the answer to "how does a candidate row become a `State`?".
/// Candidates at frame `i` were discovered at ANY earlier frame, but the
/// saved batches are grouped BY discovery frame, so the obvious loop
/// re-loads batches 1..i every frame - O(H²/2) decompressions, ~1900 s on
/// room (1,0), which is more than the expansions cost. One pass builds this
/// instead and every later fetch is an array index.
pub struct RowIndex {
    /// The saved batches concatenated, in frame order.
    states: Vec<State>,
    /// Row id -> index into `states`.
    owner: Vec<u32>,
    /// Row id -> its lane in that state.
    lane: Vec<u32>,
    /// Row id -> player position cell (`NO_CELL` when the row has no player
    /// object; that is a normal node of the position graph, not a hole).
    cell: Vec<u32>,
    /// Row ids already in the next room. Absorbing: the forward pass drops
    /// them after saving, so they are never expanded here either - they are
    /// the seeds of `B(H)`.
    win: Vec<u64>,
}

impl RowIndex {
    pub fn states(&self) -> usize {
        self.states.len()
    }

    pub fn lanes(&self) -> usize {
        self.states.iter().map(|s| s.vector_size).sum()
    }
}

#[inline]
fn get_bit(bits: &[u64], i: usize) -> bool {
    bits[i / 64] & (1u64 << (i % 64)) != 0
}

/// Set bit `i`, returning true when it was not already set.
#[inline]
fn set_bit(bits: &mut [u64], i: usize) -> bool {
    let (w, b) = (i / 64, 1u64 << (i % 64));
    let was = bits[w] & b != 0;
    bits[w] |= b;
    !was
}

/// Iterate the set bit indices of a bitmap, skipping empty words. Used to
/// walk the candidate cells without scanning every row id.
fn iter_set_bits(bits: &[u64]) -> impl Iterator<Item = u32> + '_ {
    bits.iter()
        .enumerate()
        .filter(|(_, &w)| w != 0)
        .flat_map(|(w, &word)| {
            let base = (w * 64) as u32;
            (0..64).filter_map(move |b| ((word >> b) & 1 != 0).then_some(base + b))
        })
}

/// One pass over every saved batch, recording where each row's lane lives.
///
/// Also checks, per row, the property the whole index rests on: the forward
/// pass is frontier-only, so every row appears in exactly one batch, the one
/// for its earliest-arrival frame. A row in the wrong batch, or in
/// two, means the batches and the row table disagree and every candidate set
/// below would be drawn from the wrong states.
pub fn build_index(dir: &Path, frames: u32, table: &RowTable) -> Result<RowIndex> {
    let n_rows = table.len();
    let mut idx = RowIndex {
        states: Vec::new(),
        owner: vec![u32::MAX; n_rows],
        lane: vec![u32::MAX; n_rows],
        cell: vec![NO_CELL; n_rows],
        win: vec![0u64; n_rows.div_ceil(64)],
    };
    let mut placed = 0usize;
    let mut wins = 0usize;
    for f in 1..=frames {
        let t = std::time::Instant::now();
        let states = checkpoint::load_frame_states(dir, f)
            .with_context(|| format!("loading frame batch f{:03}", f))?;
        for state in states {
            if state.vector_size == 0 {
                continue;
            }
            let keys = row_keys(&state)?;
            let cells = pos_graph::state_cells(&state)?;
            let won = win_lane_mask(&state);
            if keys.len() != state.vector_size
                || cells.len() != state.vector_size
                || won.len() != state.vector_size
            {
                return Err(anyhow!(
                    "f{:03}: {} keys / {} cells / {} win flags for {} lanes",
                    f,
                    keys.len(),
                    cells.len(),
                    won.len(),
                    state.vector_size
                ));
            }
            let si = u32::try_from(idx.states.len())
                .map_err(|_| anyhow!("more than 4G saved states"))?;
            for (l, key) in keys.iter().enumerate() {
                let id = table.id_of(*key).ok_or_else(|| {
                    anyhow!("f{:03}: a saved lane's row is not in the row table", f)
                })? as usize;
                if table.earliest_frame(id as u32) != Some(f) {
                    return Err(anyhow!(
                        "f{:03}: row {} is stamped with frame {:?}, not this batch's",
                        f,
                        id,
                        table.earliest_frame(id as u32)
                    ));
                }
                if idx.owner[id] != u32::MAX {
                    return Err(anyhow!("row {} appears in two frontier batches", id));
                }
                idx.owner[id] = si;
                idx.lane[id] = l as u32;
                idx.cell[id] = cells[l];
                if won[l] {
                    set_bit(&mut idx.win, id);
                    wins += 1;
                }
                placed += 1;
            }
            idx.states.push(state);
        }
        if f % 20 == 0 || f == frames {
            println!(
                "  index f{:03}: {} of {} rows placed ({:.1}s this frame)",
                f,
                placed,
                n_rows,
                t.elapsed().as_secs_f64()
            );
        }
    }
    if placed != n_rows {
        return Err(anyhow!(
            "{} of {} rows are in the row table but in no frontier batch",
            n_rows - placed,
            n_rows
        ));
    }
    println!(
        "index: {} rows over {} saved states, {} of them already in the next room",
        placed,
        idx.states.len(),
        wins
    );
    Ok(idx)
}

/// The forward pass's states regrouped BY POSITION CELL and held COMPRESSED.
///
/// `RowIndex` above loads every frame batch and keeps it decompressed and
/// resident for the whole sweep - 25.7 GB on disk for room (1,0) at H=94,
/// which inflates past 60 GB in RAM and OOM'd the in-process kernel sweep
/// BEFORE its loop even started. This is the fix: the boundary states are
/// bucketed by their player-position cell and each sub-state is stored
/// zstd-compressed (the whole set is ~0.8 GB compressed), so the backward
/// walk decompresses only the cells its frontier touches - and the position
/// filter means most cells are never touched at all. Peak resident is the
/// compressed blobs plus one frame's working set, a few GB instead of 60+.
///
/// The build is one frame at a time and compresses each sub-state as it goes,
/// so it never holds the whole forward pass decompressed even while building.
/// It yields the SAME `(row -> cell, win)` facts `build_index` does (checked
/// by `cell_store_matches_the_hold_index`), and lossless round-trip plus the
/// grouping-invariance of `g` (module docs) make the swept `g` byte-identical
/// to the hold path's - gated end to end by `Sweep`'s `CELESTE_SWEEP_LEGACY`.
pub struct CellStore {
    /// cell -> its sub-states, each `zstd(bincode(State))`, in ASCENDING
    /// frame order (the build fills frames 1..=F in order). One entry per
    /// (saved state, cell) pair.
    blob: Vec<Vec<Vec<u8>>>,
    /// cell -> the earliest-arrival frame of each sub-state, same order as
    /// `blob[cell]`. A sub-state's rows ALL have this earliest frame, so at
    /// sweep frame `i` only sub-states with `frame <= i` can hold a candidate
    /// (`frame <= i` is exactly `row_id < r_end`) - and since the list is
    /// sorted the gather stops at the first `frame > i` instead of touching,
    /// let alone decompressing, the rest.
    sub_frame: Vec<Vec<u32>>,
    /// cell -> cumulative lane offsets into `rows[cell]`, `len + 1` entries
    /// (`sub j` covers `rows[cell][sub_off[j]..sub_off[j+1]]`).
    sub_off: Vec<Vec<u32>>,
    /// cell -> the row id of each lane, in the (sub-state, lane) order the
    /// blobs decompress to - so a decompressed cell's lanes map back to rows.
    rows: Vec<Vec<u32>>,
    /// row id -> its cell (win seeding and `mark_dst_cell` on a qualifier).
    cell: Vec<u32>,
    /// row ids already in the next room - the seeds of `B(H)`.
    win: Vec<u64>,
    n_states: usize,
    n_lanes: usize,
}

impl CellStore {
    /// Live cells (those any forward lane occupied).
    fn cells_with_data(&self) -> usize {
        self.blob.iter().filter(|v| !v.is_empty()).count()
    }
    fn lanes(&self) -> usize {
        self.n_lanes
    }
    /// Decompress one cell's sub-states. Owned and dropped by the caller, so
    /// only the touched cells are ever resident. (No cache yet: memory is the
    /// goal; a hot-cell LRU is a speed follow-up if re-decompression bites.)
    fn decompress(&self, cell: u32) -> Result<Vec<State>> {
        self.blob[cell as usize]
            .iter()
            .map(|b| zunpack_state(b))
            .collect()
    }
}

fn zpack_state(s: &State) -> Result<Vec<u8>> {
    let raw = bincode::serialize(s).context("serializing a cell-store sub-state")?;
    // Level 1: the checkpoint's own level; fast, and the payload is already
    // the compact boundary form.
    zstd::encode_all(&raw[..], 1).context("compressing a cell-store sub-state")
}

fn zunpack_state(b: &[u8]) -> Result<State> {
    let raw = zstd::decode_all(b).context("decompressing a cell-store sub-state")?;
    bincode::deserialize(&raw).context("deserializing a cell-store sub-state")
}

/// Cross-check a freshly built cell store against the hold index on the same
/// checkpoint: identical `(row -> cell, win)` placement, and every cell
/// decompresses to lanes whose row keys map back to exactly `rows[cell]` (the
/// (sub-state, lane) -> row-id alignment the gather relies on). Gated by
/// `CELESTE_SWEEP_VERIFY_STORE=1` and meant for a small checkpoint - it builds
/// the hold index too, so it needs the memory that store exists to avoid.
fn verify_cell_store_against_index(
    store: &CellStore,
    index: &RowIndex,
    table: &RowTable,
) -> Result<()> {
    if store.win != index.win {
        return Err(anyhow!("cell store and hold index disagree on the win seeds"));
    }
    if store.cell != index.cell {
        return Err(anyhow!("cell store and hold index disagree on row -> cell"));
    }
    let mut checked = 0usize;
    for cell in 0..CELL_COUNT {
        if store.blob[cell].is_empty() {
            continue;
        }
        let subs = store.decompress(cell as u32)?;
        // Metadata the gather indexes with: one frame + offset per sub, frame
        // list ascending, offsets cumulative and terminated at the row count.
        if store.sub_frame[cell].len() != subs.len()
            || store.sub_off[cell].len() != subs.len() + 1
            || store.sub_off[cell][0] != 0
        {
            return Err(anyhow!("cell {}: sub metadata length mismatch", cell));
        }
        let mut off = 0usize;
        let mut last_frame = 0u32;
        for (j, sub) in subs.iter().enumerate() {
            if store.sub_off[cell][j] as usize != off {
                return Err(anyhow!("cell {} sub {}: offset mismatch", cell, j));
            }
            if store.sub_frame[cell][j] < last_frame {
                return Err(anyhow!("cell {}: sub frames not ascending", cell));
            }
            last_frame = store.sub_frame[cell][j];
            let keys = row_keys(sub)?;
            if keys.len() != sub.vector_size {
                return Err(anyhow!("cell {}: {} keys for {} lanes", cell, keys.len(), sub.vector_size));
            }
            for (l, key) in keys.iter().enumerate() {
                let want = store.rows[cell][off + l];
                let got = table.id_of(*key).ok_or_else(|| {
                    anyhow!("cell {}: a decompressed lane's row is not in the table", cell)
                })?;
                if got != want {
                    return Err(anyhow!(
                        "cell {} lane {}: decompressed row {} but rows[] says {}",
                        cell, off + l, got, want
                    ));
                }
                // The sub's stamped frame must be every row's earliest frame.
                if table.earliest_frame(got) != Some(store.sub_frame[cell][j]) {
                    return Err(anyhow!(
                        "cell {} sub {}: row {} earliest {:?} != sub frame {}",
                        cell, j, got, table.earliest_frame(got), store.sub_frame[cell][j]
                    ));
                }
                checked += 1;
            }
            off += sub.vector_size;
        }
        if off != store.rows[cell].len()
            || *store.sub_off[cell].last().unwrap() as usize != off
        {
            return Err(anyhow!(
                "cell {}: {} decompressed lanes but {} row ids",
                cell, off, store.rows[cell].len()
            ));
        }
    }
    println!(
        "cell store verified against the hold index: {} rows, win/cell placement identical",
        checked
    );
    Ok(())
}

/// Build the position-keyed compressed store in one frame-at-a-time pass.
/// Same per-row validation as `build_index` (every row in exactly one batch,
/// stamped with that batch's frame), so a batches/row-table disagreement is
/// still caught loudly rather than drawing candidates from the wrong states.
pub fn build_cell_store(dir: &Path, frames: u32, table: &RowTable) -> Result<CellStore> {
    let n_rows = table.len();
    let mut store = CellStore {
        blob: vec![Vec::new(); CELL_COUNT],
        sub_frame: vec![Vec::new(); CELL_COUNT],
        sub_off: vec![Vec::new(); CELL_COUNT],
        rows: vec![Vec::new(); CELL_COUNT],
        cell: vec![NO_CELL; n_rows],
        win: vec![0u64; n_rows.div_ceil(64)],
        n_states: 0,
        n_lanes: 0,
    };
    let mut placed_bits = vec![0u64; n_rows.div_ceil(64)];
    let mut placed = 0usize;
    let mut wins = 0usize;
    for f in 1..=frames {
        let t = std::time::Instant::now();
        let states = checkpoint::load_frame_states(dir, f)
            .with_context(|| format!("loading frame batch f{:03}", f))?;
        for state in states {
            if state.vector_size == 0 {
                continue;
            }
            let keys = row_keys(&state)?;
            let cells = pos_graph::state_cells(&state)?;
            let won = win_lane_mask(&state);
            if keys.len() != state.vector_size
                || cells.len() != state.vector_size
                || won.len() != state.vector_size
            {
                return Err(anyhow!(
                    "f{:03}: {} keys / {} cells / {} win flags for {} lanes",
                    f,
                    keys.len(),
                    cells.len(),
                    won.len(),
                    state.vector_size
                ));
            }
            // Split this saved state by cell (usually a no-op: the pos-graph
            // forward partitions by position, so a saved state is already
            // position-uniform), then compress each piece.
            let mut by_cell: std::collections::HashMap<u32, Vec<u32>> =
                std::collections::HashMap::new();
            for (l, &c) in cells.iter().enumerate() {
                by_cell.entry(c).or_default().push(l as u32);
            }
            for (c, lanes) in by_cell {
                let c = c as usize;
                if store.sub_off[c].is_empty() {
                    store.sub_off[c].push(0);
                }
                for &l in &lanes {
                    let id = table.id_of(keys[l as usize]).ok_or_else(|| {
                        anyhow!("f{:03}: a saved lane's row is not in the row table", f)
                    })? as usize;
                    if table.earliest_frame(id as u32) != Some(f) {
                        return Err(anyhow!(
                            "f{:03}: row {} is stamped with frame {:?}, not this batch's",
                            f,
                            id,
                            table.earliest_frame(id as u32)
                        ));
                    }
                    if !set_bit(&mut placed_bits, id) {
                        return Err(anyhow!("row {} appears in two frontier batches", id));
                    }
                    store.cell[id] = c as u32;
                    if won[l as usize] {
                        set_bit(&mut store.win, id);
                        wins += 1;
                    }
                    store.rows[c].push(id as u32);
                    placed += 1;
                }
                let sub = state
                    .filter_by_kept_clone(&KeptLanes::from_sorted_indices(&lanes), FILTER_BAND);
                store.n_lanes += sub.vector_size;
                store.n_states += 1;
                store.blob[c].push(zpack_state(&sub)?);
                store.sub_frame[c].push(f);
                store.sub_off[c].push(store.rows[c].len() as u32);
            }
        }
        if f % 20 == 0 || f == frames {
            println!(
                "  cellstore f{:03}: {} of {} rows placed ({:.1}s this frame)",
                f,
                placed,
                n_rows,
                t.elapsed().as_secs_f64()
            );
        }
    }
    if placed != n_rows {
        return Err(anyhow!(
            "{} of {} rows are in the row table but in no frontier batch",
            n_rows - placed,
            n_rows
        ));
    }
    println!(
        "cellstore: {} rows over {} sub-states / {} live cells, {} already in the next room",
        placed,
        store.n_states,
        store.cells_with_data(),
        wins
    );
    Ok(store)
}

/// Add `cell` to the live destination set, folding its recorded predecessor
/// cells into the candidate mask. Idempotent - both sets only grow, because
/// `B` only grows as the sweep runs backward.
fn mark_dst_cell(graph: &PosGraph, cell: u32, dst_seen: &mut [u64], cand_cells: &mut [u64]) {
    if !set_bit(dst_seen, cell as usize) {
        return;
    }
    let srcs = graph.srcs_of(cell);
    if srcs.is_empty() {
        // Nothing was ever seen to step into this cell, so it is only
        // reachable by being there already (the start row). Dropping it
        // would lose that row; keeping only itself is the sound reading.
        set_bit(cand_cells, cell as usize);
        return;
    }
    for &s in srcs {
        set_bit(cand_cells, s as usize);
    }
}

pub struct TimeSweepResult {
    /// Min frames to the room exit per row id, `G_UNREACHABLE` where the
    /// row cannot reach it WITHIN THE HORIZON (`e + g > horizon`).
    pub g: Vec<u16>,
    /// Row-expansions performed - the unit both sweep designs pay in.
    pub expansions: u64,
    /// Successor lanes landing on rows absent from the row table. Expected
    /// only for a banded level, or from the regrouping (module docs).
    pub out_of_table: u64,
    /// `min(e + g)` over all rows - the abstract optimal win frame.
    pub optimal_frame: Option<u32>,
}

/// Register `variants` on a replay engine, or say why there are none.
///
/// The replay stages take the same `--variant` set as `bench` for a reason
/// that is NOT correctness: dispatch is semantically invisible - the
/// boundary states are identical with and without it - so a sweep may
/// legally run variant-free against a variant-recorded forward pass. What
/// applying the same set everywhere buys is that a variant which turns out
/// to be WRONG shows up as a disagreement between stages instead of as a
/// campaign whose stages quietly disagree. See the VARIANTS note in
/// ladder.sh.
fn register_variants(
    engine: &mut AbstractRun,
    base_mapping: StateMapping,
    variants: Vec<crate::search::run::Variant>,
) -> Result<()> {
    if variants.is_empty() {
        return Ok(());
    }
    for v in &variants {
        println!("variant {} registered for shapes {:?}", v.label, v.shapes);
    }
    engine.set_variants(base_mapping, variants);
    Ok(())
}

/// How a caller builds its variant registry, given the host program.
///
/// A BUILDER rather than a built `Vec<Variant>` because the two replay
/// engines here - the pos-graph recorder and the backward loop - are
/// created one after the other and each needs its own registry, and
/// `Variant` owns a `FixedEnv`, which is deliberately not `Clone`: a
/// cheap-looking clone of the whole function table in a loop is exactly
/// the mistake that type is refusing to make available. The two builds
/// are seconds against a sweep measured in minutes, and the pos-graph one
/// does not happen at all when the table is reused.
pub type VariantBuilder<'a> = dyn Fn(&Program) -> Result<Vec<crate::search::run::Variant>> + 'a;

/// Load the level's position graph, extending it over any frames it does
/// not yet cover.
///
/// The table is horizon-independent by construction (nothing in `pos_graph`
/// reads a horizon, a band or a `g`), so one build serves every horizon of
/// a level - but it is NOT frame-independent: extending the forward pass
/// adds frames whose transitions it has never seen, and a missing pair means
/// the filter never generates that candidate and nothing notices. So the
/// covered frame count is stored in the file and checked here.
///
/// By default it is built PER PRECISION LEVEL, in that level's own
/// directory, and the fingerprint in the file enforces that. `from` opts
/// into sharing a COARSER level's table instead - see `borrow_pos_graph`
/// for what that costs and what is checked.
pub fn prepare_pos_graph(
    dir: &Path,
    frames: u32,
    fingerprint: &str,
    recipe_text: &str,
    from: Option<&Path>,
) -> Result<PosGraph> {
    if let Some(src) = from {
        return borrow_pos_graph(src, frames, recipe_text);
    }
    let existing = PosGraph::load(dir)?;
    if let Some(old) = &existing {
        if old.fingerprint() != fingerprint {
            return Err(anyhow!(
                "{}/posgraph.bin was recorded from the forward pass {}, but \
                 this configuration is {} - it describes transitions that \
                 search never took, and misses ones it does. Delete it.",
                dir.display(),
                old.fingerprint(),
                fingerprint
            ));
        }
    }
    let covered = existing.as_ref().map_or(1, |g| g.frames());
    if covered >= frames {
        let graph = existing.expect("a covered range implies a loaded table");
        println!(
            "sweep: position graph reused - {} pairs over {} destination cells, \
             covering frames 1..{}",
            graph.pairs(),
            graph.live_cells(),
            graph.frames()
        );
        return Ok(graph);
    }
    // No static replay build any more (it was the room-(0,0) OOM: a whole
    // second forward pass, ~76 GB transient, glibc-pinned). The position
    // graph is recorded ON THE FLY, fused into the forward pass
    // (`bench --record-pos-graph`, which the ladder sets), or borrowed from a
    // coarser level (`--pos-graph-from`). A checkpoint that has neither is a
    // setup error, named loudly rather than silently rebuilt.
    Err(anyhow!(
        "{}/posgraph.bin covers frames 1..{} but the sweep needs 1..{}: the \
         forward pass did not record the position graph. Re-run the forward \
         with --record-pos-graph (the ladder does this), or point the sweep at \
         a coarser level's graph with --pos-graph-from. The static replay \
         builder was removed (it was the room (0,0) OOM).",
        dir.display(),
        covered,
        frames
    ))
}

/// Use a position graph recorded by ANOTHER precision level of the same
/// campaign (`--pos-graph-from`), instead of building this level its own.
///
/// Why this is sound. The table is a projection of the reachable
/// transition relation onto whole-pixel position cells. A coarser ladder
/// level over-approximates a finer one - that is the ladder's own
/// soundness argument, and it is what every band already rests on - and
/// the widened coordinates (rem, spd) are sub-pixel, so a finer level's
/// lane sits inside a coarser lane occupying the SAME cell. A banded level
/// is restricted further still. So the coarse table CONTAINS the finer
/// level's, and a superset is the safe direction: the table only shrinks
/// the sweep's candidate set, and the expansion is what establishes an
/// edge. (A subset would silently lose predecessors, which is why this is
/// one-directional and checked.)
///
/// Measured on room (1,0) at H=72: level 0 has 166,455 pairs, k=1 has
/// 34,137 and k=2 21,579 - both strict SUBSETS, zero pairs outside level
/// 0's table.
///
/// Two things are checked rather than assumed:
///
/// * The source's fingerprint must equal this campaign's at SOME precision
///   coarser than or equal to the current one. Everything else the
///   fingerprint covers - recipe, lua, room, chunk caps, frontier-only,
///   synthetic win - must still match EXACTLY. There is no "close enough"
///   here; the exemption is precision and nothing else.
/// * It must ALREADY cover `frames`. A borrowed table is never extended:
///   extending it would replay this level's batches into another level's
///   file, and a finer level's transitions do not belong in a coarser
///   level's table. Extend the source level instead.
fn borrow_pos_graph(src: &Path, frames: u32, recipe_text: &str) -> Result<PosGraph> {
    let graph = PosGraph::load(src)?.ok_or_else(|| {
        anyhow!(
            "--pos-graph-from {} has no posgraph.bin - build that level's \
             table first (its forward pass with --record-pos-graph, or \
             `rewrite pos-graph --checkpoint-dir {}`)",
            src.display(),
            src.display()
        )
    })?;
    let accepted = crate::search::checkpoint::coarser_precision_fingerprints(recipe_text);
    let level = accepted
        .iter()
        .find(|(_, fp)| fp == graph.fingerprint())
        .map(|(level, _)| *level)
        .ok_or_else(|| {
            anyhow!(
                "{}/posgraph.bin was recorded from the forward pass {}, which \
                 is not this campaign at any precision coarser than or equal \
                 to the current one. Sharing is allowed ACROSS PRECISION \
                 LEVELS ONLY - a different recipe, room, chunk cap or win \
                 target is a different search, and its table misses \
                 transitions this one takes.",
                src.display(),
                graph.fingerprint()
            )
        })?;
    if graph.frames() < frames {
        return Err(anyhow!(
            "{}/posgraph.bin covers frames 1..{} but this sweep needs 1..{}. \
             A borrowed table is never extended here - that would replay this \
             level's batches into another level's file. Extend the source \
             level instead.",
            src.display(),
            graph.frames(),
            frames
        ));
    }
    println!(
        "sweep: position graph borrowed from {} at precision {:?} - {} pairs \
         over {} destination cells, covering frames 1..{}",
        src.display(),
        level,
        graph.pairs(),
        graph.live_cells(),
        graph.frames()
    );
    Ok(graph)
}

/// The time-expanded backward sweep. See the module docs.
#[allow(clippy::too_many_arguments)]
pub fn backward_sweep_time(
    dir: &Path,
    frames: u32,
    horizon: u32,
    fingerprint: &str,
    recipe_text: &str,
    pos_graph_from: Option<&Path>,
    program: &Program,
    plain: &Program,
    mapping: StateMapping,
    variants: &VariantBuilder<'_>,
    banded: bool,
) -> Result<TimeSweepResult> {
    if horizon > frames {
        return Err(anyhow!(
            "horizon {} is beyond the forward pass's {} frames - there is \
             nothing to sweep there",
            horizon,
            frames
        ));
    }
    let graph = prepare_pos_graph(dir, frames, fingerprint, recipe_text, pos_graph_from)?;

    let ck = checkpoint::load(dir, frames, fingerprint).context("loading final checkpoint")?;
    let table = ck.visited;
    drop(ck.states);
    let n_rows = table.len();
    let watermarks = table.watermarks().to_vec();
    if (watermarks.len() as u32) < horizon {
        return Err(anyhow!(
            "the checkpoint has {} frame watermarks, fewer than the horizon {}",
            watermarks.len(),
            horizon
        ));
    }

    // Default: the streaming position-keyed compressed store. `CELESTE_SWEEP_LEGACY=1`
    // uses the old hold-everything `RowIndex` instead - kept ONLY so the two
    // can be diffed end to end (`g.bin` byte-for-byte) at a horizon small
    // enough that the hold path fits in memory.
    let legacy_index = std::env::var("CELESTE_SWEEP_LEGACY").as_deref() == Ok("1");
    let t = std::time::Instant::now();
    let index_opt = if legacy_index {
        Some(build_index(dir, frames, &table)?)
    } else {
        None
    };
    let store_opt = if legacy_index {
        None
    } else {
        Some(build_cell_store(dir, frames, &table)?)
    };
    match (&index_opt, &store_opt) {
        (Some(ix), None) => println!(
            "sweep: re-index built in {:.1}s ({} states, {} lanes)",
            t.elapsed().as_secs_f64(),
            ix.states(),
            ix.lanes()
        ),
        (None, Some(st)) => println!(
            "sweep: cell store built in {:.1}s ({} live cells, {} lanes)",
            t.elapsed().as_secs_f64(),
            st.cells_with_data(),
            st.lanes()
        ),
        _ => unreachable!(),
    }
    crate::metrics::record("bwdt.index", t.elapsed());
    // Optional structural gate: build the hold index too and cross-check the
    // store against it (placement + decompression alignment). Small
    // checkpoints only - it needs the memory the store exists to avoid.
    if let Some(store) = store_opt.as_ref() {
        if std::env::var("CELESTE_SWEEP_VERIFY_STORE").as_deref() == Ok("1") {
            let index = build_index(dir, frames, &table)?;
            verify_cell_store_against_index(store, &index, &table)?;
        }
    }
    // Read-only views over whichever store was built: the win seeds and each
    // row's cell are the same facts either way.
    let win_bits: &[u64] = match (&index_opt, &store_opt) {
        (Some(ix), None) => &ix.win,
        (None, Some(st)) => &st.win,
        _ => unreachable!(),
    };
    let row_cell: &[u32] = match (&index_opt, &store_opt) {
        (Some(ix), None) => &ix.cell,
        (None, Some(st)) => &st.cell,
        _ => unreachable!(),
    };

    let mut engine = AbstractRun::start_with_deopt(program, plain, mapping.clone(), false)?;
    register_variants(&mut engine, mapping, variants(program)?)?;
    engine.disable_frontier();
    // Turn OFF the player-position merge partition. It is a process global
    // that the forward's `--record-pos-graph` set and never cleared; the
    // sweep READS the pos-graph, it does not record, so it does not need
    // position-uniform chunks. Leaving it on splits every replayed state
    // into one-per-position sub-states for nothing - a 3x+ memory
    // multiplier on the compiled path's wide states, which OOM'd the
    // in-process kernel ladder's sweep at f94 (room (1,0)). Row keys are
    // partition-independent, so the transition relation `g` is walked over
    // is identical either way.
    crate::interpreter::vectorize::set_partition_player_position(false);
    // The origin column rides the compiled engine as block METADATA
    // (`Rt2::origin`, plans/kernel-ladder.md "the passthrough column"),
    // so this replay runs on the kernels whenever the forward pass does.
    // This loop reads (origin, row key) pairs off each output state and
    // discards the states; merging them by shape first is work thrown
    // away. See `skip_boundary_merge` for what changes - `out_of_table`
    // becomes a count with duplicates, and `newly` is idempotent.
    //
    // `CELESTE_SWEEP_MERGE=1` puts the merge back, for A/B only. `g.bin`
    // is byte-identical either way (gated at H=68); what changes is where
    // the time goes, and the trade is not one-sided - the merge's cost
    // moves partly into the per-fragment key read.
    if std::env::var("CELESTE_SWEEP_MERGE").as_deref() != Ok("1") {
        engine.skip_boundary_merge();
    }

    let mut g: Vec<u16> = vec![G_UNREACHABLE; n_rows];
    // B, grown backward. It holds B(i+1) while frame i is being tested; the
    // frame's own qualifiers land in `newly` and are folded in afterwards,
    // because a candidate at frame i must reach B(i+1), not B(i).
    let mut in_b = vec![0u64; n_rows.div_ceil(64)];
    let mut newly = vec![0u64; n_rows.div_ceil(64)];
    let mut newly_ids: Vec<u32> = Vec::new();
    // Cells B occupies, and the candidate cells their recorded predecessors
    // span. Both only grow, so they are maintained incrementally.
    let mut dst_seen = vec![0u64; CELL_WORDS];
    let mut cand_cells = vec![0u64; CELL_WORDS];

    let seed_end = watermarks[horizon as usize - 1] as usize;
    let mut seeds = 0u64;
    for (id, gv) in g[..seed_end].iter_mut().enumerate() {
        if get_bit(win_bits, id) {
            *gv = 0;
            set_bit(&mut in_b, id);
            mark_dst_cell(&graph, row_cell[id], &mut dst_seen, &mut cand_cells);
            seeds += 1;
        }
    }
    println!(
        "sweep: {} rows, horizon {}, {} win seeds in B({})",
        n_rows, horizon, seeds, horizon
    );

    let chunk_lanes_cap: usize = std::env::var("CELESTE_SWEEP_CHUNK_LANES")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(250_000);
    // Reused across frames so the per-frame bucketing is an append per
    // candidate, not an allocation. Only the legacy hold-index path uses
    // these (the cell store buckets by cell, not by owner state).
    let mut by_state: Vec<Vec<(u32, u32)>> =
        vec![Vec::new(); index_opt.as_ref().map_or(0, |ix| ix.states())];
    let mut touched: Vec<u32> = Vec::new();
    let mut expansions = 0u64;
    let mut out_of_table = 0u64;

    for i in (1..horizon).rev() {
        let t = std::time::Instant::now();
        let r_end = watermarks[i as usize - 1] as usize;

        // Candidates: R(i) \ B(i+1), restricted to the cells the position
        // graph says can step into a cell B(i+1) occupies. `flush` steps a
        // bounded group, reads its (origin, row key) pairs, folds membership
        // into `newly`, and drops everything - so the replay adds a bounded
        // transient rather than a per-frame spike. Grouping order does not
        // affect `g` (membership is order-independent and `newly` is
        // idempotent), so the two gather paths below produce the same `g`.
        let mut flush = |group: &mut Vec<State>| -> Result<()> {
            if group.is_empty() {
                return Ok(());
            }
            let deopt_events = engine.deopt_events();
            engine.restore(std::mem::take(group), None, deopt_events)?;
            crate::metrics::time("bwdt.replay", || {
                engine
                    .step()
                    .with_context(|| format!("expanding candidates at f{:03}", i))
            })?;
            let t_keys = std::time::Instant::now();
            // Taken, not borrowed: the states are dropped at the bottom of
            // this block either way, and stripping the origin column out of
            // a borrowed state meant cloning it first.
            for mut stripped in engine.take_states() {
                let origins = deopt_collect::read_origins_named(&stripped, SWEEP_ORIGIN);
                stripped.global_env.remove(SWEEP_ORIGIN);
                stripped.gc();
                let keys = row_keys(&stripped)?;
                if keys.len() != origins.len() {
                    return Err(anyhow!("sweep: origin/key length mismatch at f{:03}", i));
                }
                for (src, key) in origins.iter().zip(&keys) {
                    let src = *src as usize;
                    if get_bit(&newly, src) {
                        continue;
                    }
                    match table.id_of(*key) {
                        Some(dst) if get_bit(&in_b, dst as usize) => {
                            set_bit(&mut newly, src);
                            newly_ids.push(src as u32);
                        }
                        Some(_) => {}
                        None => out_of_table += 1,
                    }
                }
            }
            crate::metrics::record("bwdt.keys", t_keys.elapsed());
            let deopt_events = engine.deopt_events();
            engine.restore(Vec::new(), None, deopt_events)?;
            Ok(())
        };

        let mut candidates = 0u64;
        let mut group: Vec<State> = Vec::new();
        let mut group_lanes = 0usize;
        let t_scan = std::time::Instant::now();

        if let Some(index) = index_opt.as_ref() {
            // LEGACY hold-index path (CELESTE_SWEEP_LEGACY=1): scan R(i) for
            // candidate rows and bucket them by their owner state, then gather
            // from the resident, decompressed states.
            for s in touched.drain(..) {
                by_state[s as usize].clear();
            }
            for id in 0..r_end {
                if get_bit(&in_b, id) {
                    continue;
                }
                let cell = row_cell[id];
                if !get_bit(&cand_cells, cell as usize) {
                    continue;
                }
                let owner = index.owner[id] as usize;
                if by_state[owner].is_empty() {
                    touched.push(owner as u32);
                }
                by_state[owner].push((index.lane[id], id as u32));
                candidates += 1;
            }
            for &s in &touched {
                let lanes = &mut by_state[s as usize];
                lanes.sort_unstable();
                let kept: Vec<u32> = lanes.iter().map(|(l, _)| *l).collect();
                let ids: Vec<u32> = lanes.iter().map(|(_, id)| *id).collect();
                let mut sub = index.states[s as usize]
                    .filter_by_kept_clone(&KeptLanes::from_sorted_indices(&kept), FILTER_BAND);
                deopt_collect::inject_named(&mut sub, SWEEP_ORIGIN, &ids);
                group_lanes += sub.vector_size;
                expansions += sub.vector_size as u64;
                group.push(sub);
                if group_lanes >= chunk_lanes_cap {
                    flush(&mut group)?;
                    group_lanes = 0;
                }
            }
        } else {
            // STREAMING cell-store path (default): walk only the candidate
            // cells; for each, cheap-check its uncompressed row ids and skip
            // the cell entirely if it has no candidate this frame. Only a cell
            // that DOES gets decompressed, its candidate lanes filtered out,
            // and its origin (row id) column injected - so the resident set is
            // the compressed blobs plus one group, not the whole forward pass.
            let store = store_opt.as_ref().unwrap();
            for cell in iter_set_bits(&cand_cells) {
                let cell = cell as usize;
                let frames_of = &store.sub_frame[cell];
                let offs = &store.sub_off[cell];
                let rows_c = &store.rows[cell];
                for j in 0..frames_of.len() {
                    // Sub-states are frame-sorted, so once past the horizon
                    // frame none of the rest can hold a candidate either -
                    // and, crucially, we never decompress them.
                    if frames_of[j] > i {
                        break;
                    }
                    let lo = offs[j] as usize;
                    let hi = offs[j + 1] as usize;
                    let sub_rows = &rows_c[lo..hi];
                    // `frame <= i` means every row here has earliest <= i
                    // (row < r_end already), so candidacy is just "not yet in
                    // B" - and a sub-state fully in B is skipped without a
                    // decompress.
                    if !sub_rows.iter().any(|&r| !get_bit(&in_b, r as usize)) {
                        continue;
                    }
                    let sub = zunpack_state(&store.blob[cell][j])?;
                    if sub.vector_size != sub_rows.len() {
                        return Err(anyhow!(
                            "cell {} sub {}: {} decompressed lanes but {} row ids",
                            cell,
                            j,
                            sub.vector_size,
                            sub_rows.len()
                        ));
                    }
                    let mut kept: Vec<u32> = Vec::new();
                    let mut ids: Vec<u32> = Vec::new();
                    for (l, &row) in sub_rows.iter().enumerate() {
                        if !get_bit(&in_b, row as usize) {
                            kept.push(l as u32);
                            ids.push(row);
                        }
                    }
                    candidates += kept.len() as u64;
                    let mut cand = sub
                        .filter_by_kept_clone(&KeptLanes::from_sorted_indices(&kept), FILTER_BAND);
                    deopt_collect::inject_named(&mut cand, SWEEP_ORIGIN, &ids);
                    group_lanes += cand.vector_size;
                    expansions += cand.vector_size as u64;
                    group.push(cand);
                    if group_lanes >= chunk_lanes_cap {
                        flush(&mut group)?;
                        group_lanes = 0;
                    }
                }
            }
        }
        crate::metrics::record("bwdt.scan", t_scan.elapsed());
        flush(&mut group)?;

        // Fold the frame's qualifiers into B. A row that first qualifies at
        // frame i has g = H - i exactly.
        let g_here = (horizon - i) as u16;
        for id in newly_ids.drain(..) {
            let id = id as usize;
            newly[id / 64] &= !(1u64 << (id % 64));
            g[id] = g_here;
            set_bit(&mut in_b, id);
            mark_dst_cell(&graph, row_cell[id], &mut dst_seen, &mut cand_cells);
        }

        if i % 10 == 0 || i == horizon - 1 || i == 1 {
            let b_rows: u64 = in_b.iter().map(|w| w.count_ones() as u64).sum();
            let cells: u64 = cand_cells.iter().map(|w| w.count_ones() as u64).sum();
            println!(
                "  sweep f{:03}: |R|={}, {} candidates -> |B|={}, {} candidate cells, {:.1}s",
                i, r_end, candidates, b_rows, cells, t.elapsed().as_secs_f64()
            );
        }
    }
    drop(engine);

    if out_of_table > 0 {
        // A coarser lane grouping than the forward pass used can reach rows
        // it never reached (see the module docs), so this is not by itself a
        // divergence - but on a room whose comparisons never straddle it
        // must be zero, and a re-index bug would make it enormous.
        println!(
            "sweep: {} successor lanes landed on rows outside the row table \
             ({:.3e} of {} expanded){}",
            out_of_table,
            out_of_table as f64 / expansions.max(1) as f64,
            expansions,
            if banded { " - expected for a banded level" } else { "" }
        );
    }

    let mut optimal_frame: Option<u32> = None;
    for id in 0..n_rows as u32 {
        let gv = g[id as usize];
        if gv == G_UNREACHABLE {
            continue;
        }
        if let Some(e) = table.earliest_frame(id) {
            let win = e + gv as u32;
            optimal_frame = Some(optimal_frame.map_or(win, |b| b.min(win)));
        }
    }

    Ok(TimeSweepResult { g, expansions, out_of_table, optimal_frame })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::search::pos_graph::PosGraphBuilder;

    fn cells(bits: &[u64]) -> Vec<u32> {
        (0..crate::search::pos_graph::CELL_COUNT)
            .filter(|&c| get_bit(bits, c))
            .map(|c| c as u32)
            .collect()
    }

    /// The candidate mask is the union of the recorded predecessor cells of
    /// every cell `B` occupies, accumulated as `B` grows.
    #[test]
    fn marking_a_destination_folds_in_its_predecessor_cells() {
        let mut b = PosGraphBuilder::default();
        b.record(3, 10);
        b.record(4, 10);
        b.record(5, 11);
        let graph = b.build(1, "fp");

        let (mut dst_seen, mut cand) = (vec![0u64; CELL_WORDS], vec![0u64; CELL_WORDS]);
        mark_dst_cell(&graph, 10, &mut dst_seen, &mut cand);
        assert_eq!(cells(&cand), vec![3, 4]);
        // Idempotent: B only grows, so the same cell arrives many times.
        mark_dst_cell(&graph, 10, &mut dst_seen, &mut cand);
        assert_eq!(cells(&cand), vec![3, 4]);
        mark_dst_cell(&graph, 11, &mut dst_seen, &mut cand);
        assert_eq!(cells(&cand), vec![3, 4, 5]);
    }

    /// A cell nothing was ever seen to step into keeps ITSELF: the only way
    /// to be there is to have started there, and dropping it would lose the
    /// start row.
    #[test]
    fn an_unrecorded_destination_keeps_itself_as_a_candidate() {
        let graph = PosGraphBuilder::default().build(1, "fp");
        let (mut dst_seen, mut cand) = (vec![0u64; CELL_WORDS], vec![0u64; CELL_WORDS]);
        mark_dst_cell(&graph, 77, &mut dst_seen, &mut cand);
        assert_eq!(cells(&cand), vec![77]);
    }

    /// The no-player-object node is a node like any other - the countdown
    /// states after a death live there, and they have predecessors.
    #[test]
    fn the_no_position_node_participates() {
        let mut b = PosGraphBuilder::default();
        b.record(9, NO_CELL);
        let graph = b.build(1, "fp");
        let (mut dst_seen, mut cand) = (vec![0u64; CELL_WORDS], vec![0u64; CELL_WORDS]);
        mark_dst_cell(&graph, NO_CELL, &mut dst_seen, &mut cand);
        assert_eq!(cells(&cand), vec![9]);
    }

    #[test]
    fn set_bit_reports_only_the_first_set() {
        let mut bits = vec![0u64; 2];
        assert!(set_bit(&mut bits, 65));
        assert!(!set_bit(&mut bits, 65));
        assert!(get_bit(&bits, 65));
        assert!(!get_bit(&bits, 64));
    }
}
