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

use crate::interpreter::abstraction::room_x_lane_mask;
use crate::interpreter::deopt_collect;
use crate::interpreter::row_table::RowTable;
use crate::interpreter::state::{State, FILTER_BAND};
use crate::interpreter::value::KeptLanes;

use super::checkpoint;
use super::pos_graph::{self, PosGraph, CELL_WORDS, NO_CELL};
use super::program::Program;
use super::state_mapping::StateMapping;
use super::sweep::{row_keys, G_UNREACHABLE, SWEEP_ORIGIN};
use super::verify::AbstractRun;

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

/// One pass over every saved batch, recording where each row's lane lives.
///
/// Also checks, per row, the property the whole index rests on: the forward
/// pass is frontier-only, so every row appears in exactly one batch, the one
/// for its earliest-arrival frame. A row in the wrong batch, or in
/// two, means the batches and the row table disagree and every candidate set
/// below would be drawn from the wrong states.
pub fn build_index(dir: &Path, frames: u32, table: &RowTable) -> Result<RowIndex> {
    let n_rows = table.len();
    let win_x = crate::game_runner::win_room_x();
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
            let won = room_x_lane_mask(&state, win_x);
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

/// Return free heap to the OS, between the sweep's two big phases.
///
/// `Vec`'s `Drop` frees, it does not necessarily unmap: glibc holds the
/// pages in its arenas for reuse, which is right for a steady-state
/// allocator and wrong for a process with two disjoint multi-tens-of-GB
/// phases under a hard cgroup cap. No-op on anything but glibc.
fn trim_allocator() {
    #[cfg(target_env = "gnu")]
    {
        extern "C" {
            fn malloc_trim(pad: usize) -> std::os::raw::c_int;
        }
        // Safe: `malloc_trim` only walks the allocator's own free lists.
        unsafe { malloc_trim(0) };
    }
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
/// It is built PER PRECISION LEVEL, in that level's own directory. Sharing a
/// coarser level's table with a finer one is probably sound, but there is no
/// cheap runtime guard for it, so it is not done.
pub fn prepare_pos_graph(
    dir: &Path,
    frames: u32,
    program: &Program,
    plain: &Program,
    mapping: StateMapping,
) -> Result<PosGraph> {
    let existing = PosGraph::load(dir)?;
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
    println!(
        "sweep: recording the position graph over frames f{:03}..f{:03}",
        covered,
        frames - 1
    );
    let t = std::time::Instant::now();
    let mut engine = AbstractRun::start_with_deopt(program, plain, mapping, false)?;
    let fresh = pos_graph::build_from_replay(dir, covered, frames, &mut engine)?;
    drop(engine);
    // The replay's transient is the biggest allocation this process ever
    // makes - room (0,0)'s peaked around 76 GB - and dropping it does not
    // return it to the OS: glibc keeps it in its per-thread arenas, where it
    // is invisible to us and fully counted by the cgroup. Measured on room
    // (0,0): with the graph built in this process the RSS after the re-index
    // was 94 GB and the backward loop was OOM-killed at the 100 GB cap; with
    // the same graph loaded from disk instead it was 41.6 GB and the sweep
    // finished. So hand the arenas back before the re-index claims its own
    // tens of gigabytes.
    trim_allocator();
    let graph = match existing {
        Some(old) => {
            let mut b = old.into_builder();
            b.merge(fresh.into_builder());
            b.build(frames)
        }
        None => fresh,
    };
    graph.save(dir)?;
    println!(
        "sweep: position graph has {} pairs over {} destination cells, built in {:.1}s",
        graph.pairs(),
        graph.live_cells(),
        t.elapsed().as_secs_f64()
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
    program: &Program,
    plain: &Program,
    mapping: StateMapping,
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
    let graph = prepare_pos_graph(dir, frames, program, plain, mapping.clone())?;

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

    let t = std::time::Instant::now();
    let index = build_index(dir, frames, &table)?;
    println!(
        "sweep: re-index built in {:.1}s ({} states, {} lanes)",
        t.elapsed().as_secs_f64(),
        index.states(),
        index.lanes()
    );
    crate::metrics::record("bwdt.index", t.elapsed());

    let mut engine = AbstractRun::start_with_deopt(program, plain, mapping, false)?;
    engine.disable_frontier();

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
        if get_bit(&index.win, id) {
            *gv = 0;
            set_bit(&mut in_b, id);
            mark_dst_cell(&graph, index.cell[id], &mut dst_seen, &mut cand_cells);
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
    // candidate, not an allocation.
    let mut by_state: Vec<Vec<(u32, u32)>> = vec![Vec::new(); index.states()];
    let mut touched: Vec<u32> = Vec::new();
    let mut expansions = 0u64;
    let mut out_of_table = 0u64;

    for i in (1..horizon).rev() {
        let t = std::time::Instant::now();
        let r_end = watermarks[i as usize - 1] as usize;

        // Candidates: R(i) \ B(i+1), restricted to the cells the position
        // graph says can step into a cell B(i+1) occupies.
        let t_scan = std::time::Instant::now();
        for s in touched.drain(..) {
            by_state[s as usize].clear();
        }
        let mut candidates = 0u64;
        for id in 0..r_end {
            if get_bit(&in_b, id) {
                continue;
            }
            let cell = index.cell[id];
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
        crate::metrics::record("bwdt.scan", t_scan.elapsed());
        if candidates == 0 {
            continue;
        }

        // Gather the candidate lanes back into states. A saved state is up
        // to a few hundred thousand lanes wide, so this is a gather of the
        // candidate lanes, never a scan of the state per candidate.
        let t_gather = std::time::Instant::now();
        let mut batch: Vec<State> = Vec::new();
        for &s in &touched {
            let lanes = &mut by_state[s as usize];
            lanes.sort_unstable();
            let kept: Vec<u32> = lanes.iter().map(|(l, _)| *l).collect();
            let ids: Vec<u32> = lanes.iter().map(|(_, id)| *id).collect();
            let mut sub = index.states[s as usize]
                .filter_by_kept_clone(&KeptLanes::from_sorted_indices(&kept), FILTER_BAND);
            deopt_collect::inject_named(&mut sub, SWEEP_ORIGIN, &ids);
            batch.push(sub);
        }
        crate::metrics::record("bwdt.gather", t_gather.elapsed());

        // Expand in groups of bounded input-lane count: the origin column
        // prevents boundary dedup, so the successor fan-out is the full
        // pre-dedup lane count and a whole frame at once is tens of GB of
        // transient.
        let mut group: Vec<State> = Vec::new();
        let mut group_lanes = 0usize;
        let mut queue: std::collections::VecDeque<State> = batch.into();
        while let Some(state) = queue.pop_front() {
            group_lanes += state.vector_size;
            expansions += state.vector_size as u64;
            group.push(state);
            if group_lanes < chunk_lanes_cap && !queue.is_empty() {
                continue;
            }
            let deopt_events = engine.deopt_events();
            engine.restore(std::mem::take(&mut group), None, deopt_events)?;
            group_lanes = 0;
            crate::metrics::time("bwdt.replay", || {
                engine
                    .step()
                    .with_context(|| format!("expanding candidates at f{:03}", i))
            })?;
            let t_keys = std::time::Instant::now();
            for out in engine.states() {
                let origins = deopt_collect::read_origins_named(out, SWEEP_ORIGIN);
                let mut stripped = out.clone();
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
        }

        // Fold the frame's qualifiers into B. A row that first qualifies at
        // frame i has g = H - i exactly.
        let g_here = (horizon - i) as u16;
        for id in newly_ids.drain(..) {
            let id = id as usize;
            newly[id / 64] &= !(1u64 << (id % 64));
            g[id] = g_here;
            set_bit(&mut in_b, id);
            mark_dst_cell(&graph, index.cell[id], &mut dst_seen, &mut cand_cells);
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
    use crate::rewrite::pos_graph::PosGraphBuilder;

    fn cells(bits: &[u64]) -> Vec<u32> {
        (0..crate::rewrite::pos_graph::CELL_COUNT)
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
        let graph = b.build(1);

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
        let graph = PosGraphBuilder::default().build(1);
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
        let graph = b.build(1);
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
