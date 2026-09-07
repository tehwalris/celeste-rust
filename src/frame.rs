//! The frame-step interface, the block, and the minimal outer loop
//! (plans/architecture.md).
//!
//! Rebuilt core, written fresh against the agreed interfaces rather than copied
//! from `search/run.rs`. A block is OPAQUE to the loop except for two exposed
//! columns - its KEYS and its POSITIONS; the only other thing that crosses is
//! the frame-step call.
//!
//! Impls (kernel runner + `trace::refengine`) and the loop's checkpoint /
//! position-graph growth come next; this is the interface + the frame spine.

use anyhow::Result;

use crate::interpreter::state::State;
use celeste_engine::runtime2::Rt2;

/// The data currency (interface #2). A columnar batch of lanes - the engine's
/// own block (`Rt2`: one shape, one column per value cell, the key column the
/// boundary computed), carried as-is. Two columns are EXPOSED - the per-lane
/// key and the per-lane position - and nothing else; the field data stays
/// inside. A whole block serializes compactly (the lanes share structure), so
/// the loop always moves blocks, never individual lanes.
///
/// It IS the kernel's block, so there is no bridge in the loop: the frame
/// step consumes and produces it directly, `keep` is a column filter, the
/// regroup is a column append, and the checkpoint is the columns. The
/// interpreter `State` appears only at the edges - the initial state, the
/// reference engine, and the ladder filter's coarsening - through
/// `from_state` / `to_state`.
pub struct Block {
    rt2: Rt2,
}

impl Block {
    /// Wrap an engine block. Its key column must be present: every producer
    /// (the kernel boundary, `from_state`, the checkpoint loader) attaches
    /// it, and nothing downstream re-hashes.
    pub fn from_rt2(rt2: Rt2) -> Self {
        assert_eq!(
            rt2.row_keys.len(),
            rt2.width,
            "block without its key column ({} keys for {} lanes)",
            rt2.row_keys.len(),
            rt2.width
        );
        Block { rt2 }
    }

    /// A block from a reference-interpreter `State`, keyed by the one
    /// canonical rule (`Rt2::row_keys_canonical`: what `engine_row_keys`
    /// computed). The state must already be at its rung's abstraction.
    pub fn from_state(state: &State) -> Result<Self> {
        let (cart, cache) = crate::compiled::room_context()?;
        let mut rt2 = crate::compiled::bridge::import_block(state, cart, cache);
        rt2.row_keys_canonical();
        Ok(Block { rt2 })
    }

    /// The block as an interpreter `State` (`bridge::export_block`), for the
    /// reference engine and the ladder filter's coarsening.
    pub fn to_state(&self) -> State {
        crate::compiled::bridge::export_block(&self.rt2)
    }

    pub fn into_rt2(self) -> Rt2 {
        self.rt2
    }

    /// The key column: the 128-bit canonical row key per lane (shape + content).
    /// Identity for dedup, the visited set, and checkpoints. One entry per lane.
    pub fn keys(&self) -> &[(u64, u64)] {
        &self.rt2.row_keys
    }

    /// The position column: the player-position cell per lane, for grouping and
    /// the position graph. Same length as `keys`.
    pub fn positions(&self) -> Result<Vec<u32>> {
        crate::search::pos_graph::block_cells(&self.rt2)
    }

    /// Per lane: does it sit on the room's win target? The room-exit test
    /// (`room.x` past the start room) or, under `CELESTE_WIN_AT_XY`, the
    /// synthetic player-position target. Pure position; no peeking inside.
    pub fn wins(&self) -> Result<Vec<bool>> {
        use celeste_engine::runtime2::{Col, AV};
        let ids = crate::compiled::ids();
        let rt2 = &self.rt2;
        let lanes = rt2.width;
        if let Some(target) = crate::interpreter::abstraction::synthetic_win_xy() {
            let Some(obj) = crate::search::pos_graph::player_object(rt2) else {
                return Ok(vec![false; lanes]);
            };
            let axis = |f: u32| {
                rt2.obj_field_cell(obj, f)
                    .and_then(|c| crate::search::pos_graph::whole_i16_col(rt2, c))
            };
            return Ok(match (axis(ids.f_x), axis(ids.f_y)) {
                (Some(xs), Some(ys)) => {
                    xs.iter().zip(&ys).map(|(&x, &y)| (x, y) == target).collect()
                }
                _ => vec![false; lanes],
            });
        }
        let want = crate::pico8_num::Pico8Num::from_i16(crate::game_runner::win_room_x());
        let room = rt2
            .global_target(ids.g_room)
            .ok_or_else(|| anyhow::anyhow!("wins: no `room` global"))?;
        let x = rt2
            .obj_field_cell(room, ids.f_x)
            .ok_or_else(|| anyhow::anyhow!("wins: room table has no x field"))?;
        Ok(match &rt2.cols[x as usize] {
            Col::U(AV::Num(n)) => vec![*n == want; lanes],
            Col::N(vs) => vs.iter().map(|n| *n == want).collect(),
            Col::V(vs) => vs.iter().map(|v| *v == AV::Num(want)).collect(),
            other => anyhow::bail!("wins: room.x is not a number column: {:?}", other),
        })
    }

    /// Keep only the lanes whose mask entry is true, as a new block; `None` if
    /// none survive. The one splitting primitive the loop needs (dedup at the
    /// door repacks with this).
    pub fn keep(mut self, mask: &[bool]) -> Option<Block> {
        let keep: Vec<u32> = mask
            .iter()
            .enumerate()
            .filter_map(|(i, &m)| m.then_some(i as u32))
            .collect();
        if keep.is_empty() {
            return None;
        }
        self.rt2.retain_lanes(&keep);
        Some(self)
    }

    /// The block's shard cell. A regrouped block is uniform in position, so its
    /// whole position column is one value - the shard's cell.
    pub fn shard_cell(&self) -> Result<u32> {
        self.positions()?
            .first()
            .copied()
            .ok_or_else(|| anyhow::anyhow!("empty block has no shard cell"))
    }

    /// The block's shard shape. A block is one shape.
    pub fn shard_shape(&self) -> u64 {
        self.rt2.shape_hash
    }

    /// Number of lanes in this block.
    pub fn lanes(&self) -> usize {
        self.rt2.width
    }

    /// A one-lane block holding just lane `i`. Used by the DRAFT backward,
    /// which re-runs one input at a time so every output trivially belongs
    /// to it (no provenance needed). The wide+lane-bitmask version replaces
    /// this.
    pub fn lane(&self, i: usize) -> Option<Block> {
        if i >= self.lanes() {
            return None;
        }
        let mut rt2 = self.rt2.slice_lanes(i, i + 1);
        rt2.row_keys = vec![self.rt2.row_keys[i]];
        Some(Block { rt2 })
    }
}

/// Regroup surviving lanes into canonical blocks: one block per (shape, pm1
/// class) and - `by_cell`, the recorder's partition - player-position cell,
/// each the column-wise append (`Rt2::merge_many`) of its members. With
/// `by_cell` every block comes out uniform in (shape, position): the block
/// identity the pos-graph and backward are built on. Without it, position
/// is content and the grouping is merely coarser; the reachable row set is
/// the same either way.
///
/// Survivors arrive already pm1-partitioned (the engine's own regroup) and
/// `retain_lanes` preserves that, so `partition_pm1` is a check that costs
/// a scan; the cell split is real. The merged block's all-equal columns are
/// stored uniform (`collapse_uniform_cols`), which is what the next frame's
/// kernel bind keys on.
fn regroup(survivors: Vec<Rt2>, by_cell: bool) -> Result<Vec<Block>> {
    let ids = crate::compiled::ids();
    let mut index: rustc_hash::FxHashMap<(u64, u64, u32), usize> = Default::default();
    let mut groups: Vec<Vec<Rt2>> = Vec::new();
    for rt2 in survivors {
        for part in rt2.partition_pm1(ids) {
            let pm1 = part.pm1_key_hash(ids);
            let parts: Vec<(Rt2, u32)> = if by_cell {
                let cells = crate::search::pos_graph::block_cells(&part)?;
                // `partition_by_key` emits parts in first-occurrence order of
                // the key, so the distinct cells in that order label them.
                let mut order: Vec<u32> = Vec::new();
                for &c in &cells {
                    if !order.contains(&c) {
                        order.push(c);
                    }
                }
                part.partition_by_key(&cells).into_iter().zip(order).collect()
            } else {
                vec![(part, 0)]
            };
            for (p, cell) in parts {
                let key = (p.shape_hash, pm1, cell);
                let g = *index.entry(key).or_insert_with(|| {
                    groups.push(Vec::new());
                    groups.len() - 1
                });
                groups[g].push(p);
            }
        }
    }
    Ok(groups
        .into_iter()
        .map(|g| {
            let mut merged = Rt2::merge_many(g);
            merged.collapse_uniform_cols();
            Block::from_rt2(merged)
        })
        .collect())
}

/// The ladder's forward discard-filter. A state generated at precision r+1 is
/// KEPT only if its widened-to-precision-r form was marked by the previous
/// level's backward pass - "immediately, during the forward, the whole time".
/// The marked set is coarse, so this narrows every finer level to the coarse
/// winning envelope. This is what replaces the e/g/band numbering as the
/// cross-precision link: a set membership, not a distance threshold.
pub struct MarkFilter<'a> {
    /// The marked set from the previous, COARSER precision level.
    marked: &'a Visited,
    /// The coarser precision to widen down to before the membership test.
    coarser: crate::interpreter::abstraction::RemPrecision,
}

impl<'a> MarkFilter<'a> {
    pub fn new(
        marked: &'a Visited,
        coarser: crate::interpreter::abstraction::RemPrecision,
    ) -> Self {
        Self { marked, coarser }
    }

    /// Per-lane: keep lane `i` iff its widened-to-coarser form was marked.
    /// Rem widening never moves the integer cell and (coarsening) never splits
    /// a lane, so the widened block is lane-aligned with the input.
    ///
    /// Crosses to the interpreter `State` for the coarsening: the widenings
    /// live there (`abstraction.rs`) and this is the one place the loop
    /// still needs them. Paid only at levels >= 1, whose frontiers the
    /// filter itself keeps small.
    pub fn allowed(&self, block: &Block) -> Result<Vec<bool>> {
        use crate::interpreter::abstraction::{
            apply_conservative_widenings, make_state_abstract_rem,
        };
        let lanes = block.lanes();
        // The COMPLETE coarsening the coarse engine bakes into its states - rem
        // bucketing AND the conservative widenings (p_jump/p_dash/fruit), then
        // gc - matching the old `widened_row_key`. Without the conservative
        // widenings, an Exact state keeps p_jump/p_dash concrete while the coarse
        // marks have them widened, so its coarsened key matches nothing and the
        // winning path is wrongly filtered at the Bits->Exact rung.
        let mut widened = make_state_abstract_rem(block.to_state(), self.coarser);
        widened = apply_conservative_widenings(widened);
        widened.gc();
        anyhow::ensure!(
            widened.vector_size.max(1) == lanes,
            "rem widening changed the lane count ({} -> {}); the filter's lane \
             correspondence is broken",
            lanes,
            widened.vector_size.max(1)
        );
        let keys = crate::compiled::engine_row_keys(&widened)?;
        let cells = crate::search::pos_graph::state_cells(&widened)?;
        Ok(keys
            .iter()
            .zip(&cells)
            .map(|(k, &c)| self.marked.contains(*k, c))
            .collect())
    }
}

/// The one interface between the outer loop and the engines (interface #1).
/// `run` takes a block of input lanes and returns every output block - all
/// internal branching enumerated, already widened for the engine's precision,
/// each output block's key/position columns already computable. Branching,
/// widening, and keying happen INSIDE.
///
/// Two implementations behind this one trait: the compiled kernels (fast) and
/// the scalar interpreter (`trace::refengine`, the trusted reference). The loop
/// holds a `&dyn FrameStep` and never knows which.
pub trait FrameStep {
    fn run(&mut self, block: Block) -> Result<Vec<Block>>;
}

/// The minimal forward frame: run the frame step over every block in the
/// frontier and keep, from each output block, only the lanes whose key is new.
/// Returns the next frontier (the survivor blocks) and whether any survivor
/// wins.
///
/// This is the whole of it. Dedup is decided AT THE DOOR - a lane is kept iff
/// `visited.insert` reports its key as new, and the block is split to just those
/// lanes - so the pre-dedup fan-out never accumulates and there is no
/// materialize-then-filter. One structure (`visited`) covers both the
/// within-frame and across-frame dedup. No chunking, no serial/parallel/phased
/// variants: everything the old `step_inner` did beyond this served machinery we
/// are cutting.
pub fn forward_frame(
    engine: &mut dyn FrameStep,
    frontier: Vec<Block>,
    visited: &mut Visited,
    is_win: impl Fn(&Block) -> Result<bool>,
    pos: Option<&crate::search::pos_graph::PosObserver>,
    filter: Option<&MarkFilter>,
    by_cell: bool,
) -> Result<(Vec<Block>, bool, FrameStats)> {
    use std::time::Instant;
    let mut st = FrameStats::default();
    let mut survivors: Vec<Rt2> = Vec::new();
    let mut won = false;
    st.blocks_in = frontier.len();
    for block in frontier {
        st.lanes_in += block.lanes();
        // Pos-graph edge: the block is uniform in position (record mode's
        // by-cell regroup), so its single input cell reaches every output
        // cell. Record ALL raw outputs' positions (before dedup) - dedup
        // drops duplicate keys, not reachable positions.
        let c_in = match pos {
            Some(p) => Some(p.input_cell(&block.positions()?)?),
            None => None,
        };
        let t = Instant::now();
        let outputs = engine.run(block)?;
        st.t_engine += t.elapsed();
        for out in outputs {
            st.lanes_raw += out.lanes();
            let t = Instant::now();
            let cells = out.positions()?;
            st.t_keys += t.elapsed();
            // Pos-graph edge: record ALL raw outputs' positions (before dedup);
            // dedup drops duplicate keys, not reachable positions.
            if let (Some(p), Some(c_in)) = (pos, c_in) {
                p.record_dsts(c_in, &cells);
            }
            // Ladder filter (coarser level's marked set): discard a lane whose
            // widened-to-coarser form was not marked. Computed before the
            // visited insert so a discarded lane never enters the visited set.
            let t = Instant::now();
            let allow = match filter {
                Some(f) => Some(f.allowed(&out)?),
                None => None,
            };
            st.t_filter += t.elapsed();
            // Dedup at the door, SHARDED by (shape, cell): insert() reports true
            // for a (key, cell) not seen before in this or any earlier frontier.
            // Building the mask also records the survivors, so a duplicate later
            // in the same frame is caught too. `&&` short-circuits, so a
            // filtered-out lane is never inserted into `visited`.
            let t = Instant::now();
            let mask: Vec<bool> = out
                .keys()
                .iter()
                .zip(&cells)
                .enumerate()
                .map(|(i, (k, &c))| {
                    allow.as_ref().map_or(true, |a| a[i]) && visited.insert(*k, c)
                })
                .collect();
            st.t_visited += t.elapsed();
            let t = Instant::now();
            if let Some(kept) = out.keep(&mask) {
                st.lanes_kept += kept.lanes();
                won |= is_win(&kept)?;
                survivors.push(kept.into_rt2());
            }
            st.t_keep += t.elapsed();
        }
    }
    // Regroup the surviving lanes into canonical blocks (`regroup`): one
    // block per (shape, pm1 class[, cell]), a column append per group.
    let t = Instant::now();
    let next = regroup(survivors, by_cell)?;
    st.t_vectorize += t.elapsed();
    st.blocks_out = next.len();
    st.lanes_out = next.iter().map(Block::lanes).sum();
    Ok((next, won, st))
}

/// Where one forward frame's time went and what it moved, for the per-frame
/// log line and the phase totals. Phases are the loop's own seams; the frame
/// step's internal split is the engine's business (`CELESTE_CHUNK_PHASE_TIME`).
#[derive(Default, Clone, Copy)]
pub struct FrameStats {
    pub blocks_in: usize,
    pub lanes_in: usize,
    /// Output lanes as the engine handed them back, before the filter/dedup.
    pub lanes_raw: usize,
    /// Lanes that passed the filter and were new to `visited`.
    pub lanes_kept: usize,
    pub blocks_out: usize,
    /// Lanes after regrouping (cross-block duplicates collapsed).
    pub lanes_out: usize,
    pub t_engine: std::time::Duration,
    pub t_keys: std::time::Duration,
    pub t_filter: std::time::Duration,
    pub t_visited: std::time::Duration,
    pub t_keep: std::time::Duration,
    pub t_vectorize: std::time::Duration,
}

/// A block wins if any lane sits on the room's win target (`Block::wins`).
pub fn block_wins(block: &Block) -> Result<bool> {
    Ok(block.wins()?.iter().any(|&w| w))
}

/// The result of a backward pass: the marked set (sharded by shape+cell) plus
/// how many single-lane re-runs it cost - the number to compare against the
/// forward pass (backward must not exceed it; if it does, the narrowing is
/// broken).
pub struct BackwardResult {
    pub marked: Visited,
    pub reruns: u64,
}

/// Backward marking - SINGLE PASS, position-narrowed (plans/architecture.md
/// sentence 8; the horizon-anchored minimal form, no distance DP). From the win
/// states at `horizon`, walk frames horizon-1..1; a frame-i state is MARKED if
/// re-running it one frame produces an output that hits a state marked at frame
/// i+1. Candidates at frame i are narrowed, via the position graph, to the cells
/// that can step into a marked cell. The marked set is the cross-rem filter for
/// the next precision level.
///
/// DRAFT - NOT BLESSED. Two shortcuts, both correctness-preserving and both
/// flagged for follow-up: (1) it re-runs ONE input lane at a time, so every
/// output belongs to that input and no provenance is needed - the wide call with
/// a per-lane bitmask is the optimization; (2) marks are matched by key, held as
/// a sharded set rather than a dense per-shard bitmask. Built behind the
/// reference to be validated before it is trusted.
pub fn backward_run(
    engine: &mut dyn FrameStep,
    dir: &std::path::Path,
    horizon: u32,
    graph: &crate::search::pos_graph::PosGraph,
) -> Result<BackwardResult> {
    // Seed: the win lanes at the horizon become the first frontier (= the
    // frame-`horizon` marks).
    let mut seed: Vec<((u64, u64), u32)> = Vec::new();
    for block in load_frame(dir, horizon)? {
        let cells = block.positions()?;
        let wins = block.wins()?;
        for ((k, &c), w) in block.keys().iter().zip(&cells).zip(wins) {
            if w {
                seed.push((*k, c));
            }
        }
    }
    backward_walk(engine, dir, horizon, graph, seed)
}

/// The backward walk itself, given a seed frontier (the marks at `horizon`).
/// Factored out so a test can seed it directly; `backward_run` seeds from the
/// win lanes.
pub fn backward_walk(
    engine: &mut dyn FrameStep,
    dir: &std::path::Path,
    horizon: u32,
    graph: &crate::search::pos_graph::PosGraph,
    seed: Vec<((u64, u64), u32)>,
) -> Result<BackwardResult> {
    use rustc_hash::FxHashSet;

    let mut marked = Visited::new();
    let mut reruns: u64 = 0;
    let mut frontier: Vec<((u64, u64), u32)> = Vec::new();
    for (k, c) in seed {
        if marked.insert(k, c) {
            frontier.push((k, c));
        }
    }

    // Walk back. `frontier` is always the marks added at frame i+1 - the only
    // targets a frame-i state's successor may legitimately hit (single pass).
    for i in (1..horizon).rev() {
        // Candidate cells = pos-graph predecessors of the target (marked) cells.
        let mut cand_cells: FxHashSet<u32> = FxHashSet::default();
        for &(_, c) in &frontier {
            for &s in graph.srcs_of(c) {
                cand_cells.insert(s);
            }
        }
        // Targets as a fast membership set: (shape, content, cell).
        let targets: FxHashSet<(u64, u64, u32)> =
            frontier.iter().map(|&(k, c)| (k.0, k.1, c)).collect();

        let mut new_frontier: Vec<((u64, u64), u32)> = Vec::new();
        for block in load_frame_cells(dir, i, &cand_cells)? {
            let cells = block.positions()?;
            for lane in 0..block.lanes() {
                let (key, cell) = (block.keys()[lane], cells[lane]);
                if marked.contains(key, cell) {
                    continue; // already marked by another target - skip the re-run
                }
                let Some(single) = block.lane(lane) else { continue };
                reruns += 1;
                let mut hit = false;
                'outs: for out in engine.run(single)? {
                    let oc = out.positions()?;
                    for (k, &c) in out.keys().iter().zip(&oc) {
                        if targets.contains(&(k.0, k.1, c)) {
                            hit = true;
                            break 'outs;
                        }
                    }
                }
                if hit && marked.insert(key, cell) {
                    new_frontier.push((key, cell));
                }
            }
        }
        frontier = new_frontier;
    }
    Ok(BackwardResult { marked, reruns })
}

/// The forward search driver: from `initial` (frame 0), run frames until a lane
/// wins or `max_frames` is reached, checkpointing each frontier. Returns the
/// winning frame, or `None` if the frontier empties or the horizon is hit with
/// no win.
///
/// The whole outer forward loop is this: seed the visited set + checkpoint frame
/// 0, then repeatedly `forward_frame` (which dedups at the door) and checkpoint
/// the survivors. No chunking, no phased/parallel variants - the frame step and
/// `Visited` hide everything else.
pub struct ForwardResult {
    /// The frame a lane first won, if any.
    pub win_frame: Option<u32>,
    /// The number of frames actually run (the last checkpointed frame).
    pub frames: u32,
    /// The position graph, when recording was on - backward's input.
    pub pos_graph: Option<crate::search::pos_graph::PosGraph>,
}

pub fn forward_run(
    engine: &mut dyn FrameStep,
    initial: Vec<Block>,
    dir: &std::path::Path,
    max_frames: u32,
    record: bool,
    filter: Option<&MarkFilter>,
) -> Result<ForwardResult> {
    // Record mode regroups by cell, so every block (see `regroup`) is
    // uniform in position and the pos-graph's input cell is well defined.
    // Off for a forward-only run: position is content, so grouping on it
    // never changes the reachable row set, only makes the blocks finer
    // (which only the recorder pays for).
    let observer = record.then(crate::search::pos_graph::PosObserver::default);

    let mut visited = Visited::new();
    for b in &initial {
        let cells = b.positions()?;
        for (k, c) in b.keys().iter().zip(&cells) {
            visited.insert(*k, *c);
        }
    }
    checkpoint_frontier(dir, 0, &initial)?;
    drive_forward(engine, dir, initial, visited, observer, 1, max_frames, filter, record)
}

/// Resume/extend a checkpointed forward: from `from_frame` (whose frontier is on
/// disk) out to `to_frame`, rebuilding the visited set from the checkpoints
/// 0..=from_frame. This is the outer loop's cheap rem-0 EXTENSION path - bump
/// the horizon by re-running only the tail, not the whole search.
///
/// record=false for now: seeding the pos-graph across a resume is a follow-up,
/// and the outer loop rebuilds the finer levels fresh anyway. So a resumed run
/// returns no pos_graph.
pub fn forward_resume(
    engine: &mut dyn FrameStep,
    dir: &std::path::Path,
    from_frame: u32,
    to_frame: u32,
    filter: Option<&MarkFilter>,
) -> Result<ForwardResult> {
    let mut visited = Visited::new();
    for f in 0..=from_frame {
        for b in load_frame(dir, f)? {
            let cells = b.positions()?;
            for (k, c) in b.keys().iter().zip(&cells) {
                visited.insert(*k, *c);
            }
        }
    }
    let frontier = load_frame(dir, from_frame)?;
    drive_forward(engine, dir, frontier, visited, None, from_frame + 1, to_frame, filter, false)
}

/// The shared forward loop: from `frontier` (the states at `start_frame - 1`),
/// compute and checkpoint frames `start_frame..=end_frame`, stopping early on a
/// win or an empty frontier. `visited` and `observer` are already seeded.
#[allow(clippy::too_many_arguments)]
fn drive_forward(
    engine: &mut dyn FrameStep,
    dir: &std::path::Path,
    mut frontier: Vec<Block>,
    mut visited: Visited,
    observer: Option<crate::search::pos_graph::PosObserver>,
    start_frame: u32,
    end_frame: u32,
    filter: Option<&MarkFilter>,
    by_cell: bool,
) -> Result<ForwardResult> {
    let mut last = start_frame.saturating_sub(1);
    let mut win_frame = None;
    for frame in start_frame..=end_frame {
        let t_frame = std::time::Instant::now();
        let (next, won, st) = forward_frame(
            engine,
            frontier,
            &mut visited,
            block_wins,
            observer.as_ref(),
            filter,
            by_cell,
        )?;
        let t = std::time::Instant::now();
        checkpoint_frontier(dir, frame, &next)?;
        let t_ckpt = t.elapsed();
        let t = std::time::Instant::now();
        if let Some(o) = observer.as_ref() {
            o.flush();
        }
        let t_pos = t.elapsed();
        log_frame(frame, &st, t_ckpt, t_pos, t_frame.elapsed(), visited.len());
        last = frame;
        if won {
            win_frame = Some(frame);
            break;
        }
        if next.is_empty() {
            break;
        }
        frontier = next;
    }
    let pos_graph = observer.map(|o| o.build(last, "rebuild-forward"));
    Ok(ForwardResult { win_frame, frames: last, pos_graph })
}

/// One line per forward frame on stderr, plus the phase totals under
/// `metrics` (`fwd.*`), so a run's wall time is attributable without
/// re-running it under a profiler. Times in ms.
fn log_frame(
    frame: u32,
    st: &FrameStats,
    t_ckpt: std::time::Duration,
    t_pos: std::time::Duration,
    t_total: std::time::Duration,
    visited: usize,
) {
    let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
    eprintln!(
        "[fwd] f{frame:03} in {}/{} raw {} kept {} out {}/{} visited {} | \
         engine {:.0} keys {:.0} filter {:.0} visited {:.0} keep {:.0} vec {:.0} \
         ckpt {:.0} pos {:.0} total {:.0} ms | rss {:.2} GB",
        st.blocks_in,
        st.lanes_in,
        st.lanes_raw,
        st.lanes_kept,
        st.blocks_out,
        st.lanes_out,
        visited,
        ms(st.t_engine),
        ms(st.t_keys),
        ms(st.t_filter),
        ms(st.t_visited),
        ms(st.t_keep),
        ms(st.t_vectorize),
        ms(t_ckpt),
        ms(t_pos),
        ms(t_total),
        crate::metrics::peak_rss_gb(),
    );
    crate::metrics::record("fwd.engine", st.t_engine);
    crate::metrics::record("fwd.keys", st.t_keys);
    crate::metrics::record("fwd.filter", st.t_filter);
    crate::metrics::record("fwd.visited", st.t_visited);
    crate::metrics::record("fwd.keep", st.t_keep);
    crate::metrics::record("fwd.vectorize", st.t_vectorize);
    crate::metrics::record("fwd.checkpoint", t_ckpt);
    crate::metrics::record("fwd.posgraph", t_pos);
    crate::metrics::record("fwd.frame", t_total);
}

/// Checkpoint a frontier SHARDED by (shape, cell): one file per block, under
/// `frames/fNNN/`, named `c{cell}_s{shape}_{seq}.bin`. A regrouped block is
/// already uniform in (shape, position), so a block IS one shard and needs no
/// re-splitting. The cell leads the filename so backward can select the cells
/// it needs by name, without opening a file.
fn checkpoint_frontier(dir: &std::path::Path, frame: u32, frontier: &[Block]) -> Result<()> {
    let fdir = dir.join("frames").join(format!("f{:03}", frame));
    // Fresh: a re-run / resumed frame must not leave stale shard files behind.
    let _ = std::fs::remove_dir_all(&fdir);
    std::fs::create_dir_all(&fdir)?;
    for (seq, block) in frontier.iter().enumerate() {
        let cell = block.shard_cell()?;
        let shape = block.shard_shape();
        let path = fdir.join(format!("c{:010}_s{:016x}_{:04}.bin", cell, shape, seq));
        crate::search::checkpoint::save_block_to(&path, &block.rt2)?;
    }
    Ok(())
}

/// Load every block of a checkpointed frame.
pub fn load_frame(dir: &std::path::Path, frame: u32) -> Result<Vec<Block>> {
    load_frame_filtered(dir, frame, |_cell| true)
}

/// Load only the blocks whose cell is in `cells` - backward's per-cell load,
/// the whole point of the sharding. Files are selected by their name's cell
/// prefix; only the chosen shards are opened and decompressed.
pub fn load_frame_cells(
    dir: &std::path::Path,
    frame: u32,
    cells: &rustc_hash::FxHashSet<u32>,
) -> Result<Vec<Block>> {
    load_frame_filtered(dir, frame, |cell| cells.contains(&cell))
}

fn load_frame_filtered(
    dir: &std::path::Path,
    frame: u32,
    keep: impl Fn(u32) -> bool,
) -> Result<Vec<Block>> {
    let fdir = dir.join("frames").join(format!("f{:03}", frame));
    let mut out = Vec::new();
    for entry in std::fs::read_dir(&fdir)? {
        let entry = entry?;
        let name = entry.file_name();
        let name = name.to_string_lossy();
        // Shard files are `c{cell:010}_s{shape}_{seq}.bin`; skip tmp/others.
        let Some(rest) = name.strip_prefix('c') else { continue };
        if !name.ends_with(".bin") {
            continue;
        }
        let Some(cell_str) = rest.get(..10) else { continue };
        let Ok(cell) = cell_str.parse::<u32>() else { continue };
        if !keep(cell) {
            continue;
        }
        out.push(Block::from_rt2(crate::search::checkpoint::load_block_from(&entry.path())?));
    }
    Ok(out)
}

/// The visited set: the keys of every lane ever kept into a frontier. A lane is
/// kept iff its key was not already here (this is both the within-frame and the
/// across-frame dedup - one structure, one lookup at the door).
///
/// Placeholder over an in-RAM set; the production one is tiered/mmap-backed
/// (`interpreter::visited`) but that is an implementation swap behind
/// `insert`/`contains`, not part of this interface.
#[derive(Default)]
pub struct Visited {
    /// Sharded by (shape hash, cell) - the same partition as storage, the
    /// blocks and the marked bitmask. A shard holds only the CONTENT hashes
    /// seen at that (shape, cell). Two states in different shards can never be
    /// duplicates (different shape or different content -> different cell), so
    /// the sharding is a free refinement: smaller buckets, per-shard locality.
    shards: rustc_hash::FxHashMap<(u64, u32), rustc_hash::FxHashSet<u64>>,
}

impl Visited {
    pub fn new() -> Self {
        Self::default()
    }
    /// True if `key` (with its `cell`) was NOT already present - this lane is
    /// new, keep it. Shard picked by (shape, cell); membership by content hash.
    pub fn insert(&mut self, key: (u64, u64), cell: u32) -> bool {
        self.shards.entry((key.0, cell)).or_default().insert(key.1)
    }
    pub fn contains(&self, key: (u64, u64), cell: u32) -> bool {
        self.shards
            .get(&(key.0, cell))
            .is_some_and(|s| s.contains(&key.1))
    }
    pub fn len(&self) -> usize {
        self.shards.values().map(|s| s.len()).sum()
    }
    pub fn is_empty(&self) -> bool {
        self.shards.values().all(|s| s.is_empty())
    }
}

/// A fixed-horizon ladder result (`ladder_at_horizon`).
pub enum HorizonOutcome {
    /// Every precision level - through fully concrete - won by the horizon. The
    /// horizon is achievable; the concrete level yields a real winning trace.
    Confirmed,
    /// A precision level, filtered by the coarser level's marks, found no win by
    /// the horizon. A coarser level over-approximates concrete reachability, so
    /// this SOUNDLY excludes the horizon: no concrete play wins by it. `level`
    /// is the index into `precisions` that refuted.
    Refuted { level: usize },
}

/// The INNER ladder at a FIXED horizon (semantics pinned with Philippe,
/// 2026-08-30). Run every precision level in `precisions` (coarsest first,
/// ending at `RemPrecision::Exact` = fully concrete). Each level's forward is
/// filtered by the previous, coarser level's marked set. The instant a level
/// finds no win by `horizon`, return `Refuted` - the horizon is excluded. Only
/// if EVERY level, through concrete, wins by `horizon` is it `Confirmed`.
///
/// Engine-agnostic: the caller supplies `make_engine(precision)` and
/// `make_initial()`, so this composes the tested forward_run / backward_run /
/// MarkFilter without naming the compiled engine.
///
/// NOT YET VALIDATED end to end (needs a real winning room + the compiled engine
/// at each precision); the shape is correct, and the trace extraction at the
/// concrete level is still to add.
pub fn ladder_at_horizon(
    mut make_engine: impl FnMut(
        crate::interpreter::abstraction::RemPrecision,
    ) -> Result<Box<dyn FrameStep>>,
    mut make_initial: impl FnMut() -> Result<Vec<Block>>,
    base_dir: &std::path::Path,
    horizon: u32,
    precisions: &[crate::interpreter::abstraction::RemPrecision],
) -> Result<HorizonOutcome> {
    let mut prev: Option<(Visited, crate::interpreter::abstraction::RemPrecision)> = None;
    for (level, &precision) in precisions.iter().enumerate() {
        let mut engine = make_engine(precision)?;
        let level_dir = base_dir.join(format!("level{:02}", level));
        let filter = prev.as_ref().map(|(m, p)| MarkFilter::new(m, *p));
        let fwd = forward_run(
            engine.as_mut(),
            make_initial()?,
            &level_dir,
            horizon,
            true,
            filter.as_ref(),
        )?;
        let Some(h) = fwd.win_frame else {
            eprintln!("[ladder] level {level} ({precision:?}): NO WIN by {horizon} -> Refuted");
            return Ok(HorizonOutcome::Refuted { level });
        };
        let graph = fwd
            .pos_graph
            .as_ref()
            .expect("record mode always builds the pos graph");
        let bwd = backward_run(engine.as_mut(), &level_dir, h, graph)?;
        eprintln!(
            "[ladder] level {level} ({precision:?}): win at f{h}, marked {} states",
            bwd.marked.len()
        );
        prev = Some((bwd.marked, precision));
    }
    Ok(HorizonOutcome::Confirmed)
}

/// The OUTER loop: the minimal winning frame. From `first_win` (rem-0's first
/// win frame) step the horizon up by one until `ladder_at_horizon` Confirms it -
/// that horizon is the abstract optimum, and (once trace extraction lands) the
/// concrete level's winning trace is the witness. `None` if nothing confirms by
/// `max_horizon`.
///
/// FOLLOW-UP: the rem-0 forward should be EXTENDED by one frame per step, not
/// rerun from scratch (Philippe: "one more frame at rem zero"); here every
/// horizon reruns fresh. Correctness first, the incremental resume after.
pub fn find_optimum(
    mut make_engine: impl FnMut(
        crate::interpreter::abstraction::RemPrecision,
    ) -> Result<Box<dyn FrameStep>>,
    mut make_initial: impl FnMut() -> Result<Vec<Block>>,
    base_dir: &std::path::Path,
    first_win: u32,
    max_horizon: u32,
    precisions: &[crate::interpreter::abstraction::RemPrecision],
) -> Result<Option<u32>> {
    for horizon in first_win..=max_horizon {
        let dir = base_dir.join(format!("h{:03}", horizon));
        match ladder_at_horizon(&mut make_engine, &mut make_initial, &dir, horizon, precisions)? {
            HorizonOutcome::Confirmed => return Ok(Some(horizon)),
            HorizonOutcome::Refuted { .. } => continue,
        }
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::trace::refengine::RefEngine;

    /// End-to-end proof that the rebuilt outer loop runs: drive `forward_run`
    /// over the trusted reference engine for a few frames from the initial
    /// block, checkpointing each frontier, and prove the checkpoint round-trips
    /// (reload a frame's states and match the count). Uses the interpreter (the
    /// oracle), so it is slow and needs the cart on disk; run explicitly.
    #[test]
    #[ignore]
    fn forward_run_drives_the_reference_engine() {
        let mut engine = RefEngine::new().expect("ref engine");
        let init = vec![Block::from_state(&engine.initial_state().expect("initial state")).expect("block")];

        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("mkdir");

        let result = forward_run(&mut engine, init, dir, 4, true, None).expect("forward_run");
        // No win in the 4-frame intro; the run reaches the horizon.
        assert_eq!(result.win_frame, None, "unexpected early win in the intro");
        assert_eq!(result.frames, 4, "expected 4 frames run");
        // Recording was on: a position graph was built.
        let pg = result.pos_graph.expect("pos graph built in record mode");
        eprintln!("[forward_run] pos-graph: {} live cells", pg.live_cells());

        // Every frame 0..=4 was checkpointed sharded and reloads block-by-block.
        for frame in 0..=4 {
            let blocks = load_frame(dir, frame)
                .unwrap_or_else(|e| panic!("reload frame {frame}: {e}"));
            assert!(!blocks.is_empty(), "frame {frame} checkpoint is empty");
            // Per-cell load returns exactly the shards at those cells.
            let cells: rustc_hash::FxHashSet<u32> =
                blocks.iter().map(|b| b.shard_cell().unwrap()).collect();
            let by_cell = load_frame_cells(dir, frame, &cells).expect("per-cell load");
            assert_eq!(
                by_cell.len(),
                blocks.len(),
                "per-cell load lost blocks at frame {frame}"
            );
            eprintln!("[forward_run] f{frame}: {} blocks across {} cells", blocks.len(), cells.len());
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// Mechanical proof of the ladder forward filter: one frame, then re-run it
    /// with (a) a marked set containing exactly the widened frame-1 states -
    /// nothing is discarded - and (b) an empty marked set - everything is
    /// discarded and the frontier empties. Uses Bits(0) as the coarser level.
    #[test]
    #[ignore]
    fn forward_filter_keeps_marked_and_discards_unmarked() {
        use crate::interpreter::abstraction::{make_state_abstract_rem, RemPrecision};
        let bits0 = RemPrecision::Bits(0);
        let engine = RefEngine::new().expect("ref engine");
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-filter-test");
        let seed = || vec![Block::from_state(&engine_clone(&engine).initial_state().expect("init")).expect("block")];

        // Baseline one frame, no filter.
        {
            let mut e = engine_clone(&engine);
            forward_run(&mut e, seed(), dir, 1, false, None).expect("baseline");
        }
        let frame1 = load_frame(dir, 1).expect("load f1");
        assert!(!frame1.is_empty(), "baseline produced no frame 1");

        // marked_full = the widened-to-Bits(0) keys of every frame-1 state.
        let mut marked_full = Visited::new();
        for b in &frame1 {
            let w = make_state_abstract_rem(b.to_state(), bits0);
            let keys = crate::compiled::engine_row_keys(&w).expect("keys");
            let cells = crate::search::pos_graph::state_cells(&w).expect("cells");
            for (k, &c) in keys.iter().zip(&cells) {
                marked_full.insert(*k, c);
            }
        }

        // With the full marked set, frame 1 survives.
        {
            let f = MarkFilter::new(&marked_full, bits0);
            let mut e = engine_clone(&engine);
            forward_run(&mut e, seed(), dir, 1, false, Some(&f)).expect("full-filter run");
            let kept = load_frame(dir, 1).expect("load f1 full");
            assert!(!kept.is_empty(), "full marked set wrongly discarded frame 1");
        }

        // With an empty marked set, everything is discarded.
        {
            let empty = Visited::new();
            let f = MarkFilter::new(&empty, bits0);
            let mut e = engine_clone(&engine);
            forward_run(&mut e, seed(), dir, 1, false, Some(&f)).expect("empty-filter run");
            let kept = load_frame(dir, 1).expect("load f1 empty");
            assert!(kept.is_empty(), "empty marked set failed to discard frame 1");
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// A fresh reference engine (RefEngine isn't Clone; each forward_run needs
    /// its own since run takes &mut). Cheap enough for a test.
    fn engine_clone(_e: &RefEngine) -> RefEngine {
        RefEngine::new().expect("ref engine")
    }

    /// The FULL ladder to the concrete level: run every precision - Bits 0..=16
    /// then Exact - on the compiled engine with the synthetic early win, and
    /// require Confirmed (every level, including fully concrete, reaches the win
    /// when filtered by the coarser level's backward marks). This proves the new
    /// search works end to end through refinement, the gate for deleting the old
    /// search. Slow (many compiled forward+backward passes); run explicitly.
    #[test]
    #[ignore]
    fn new_ladder_confirms_to_concrete() {
        use crate::interpreter::abstraction::{set_rem_precision, RemPrecision};
        std::env::set_var("CELESTE_START_ROOM", "1,0");
        std::env::set_var("CELESTE_WIN_AT_XY", "8,107");
        let program = crate::program::frozen::rewritten("rewrites-compile.jsonl")
            .expect("program");
        let dir = std::path::Path::new("/var/tmp/celeste-frame-ladder-full");
        let _ = std::fs::remove_dir_all(dir);

        // The real ladder maps k>=16 to Exact (rewrite.rs: `if prev_bits >= 16
        // { Exact }`), so the distinct precisions are Bits(0..=15) then Exact.
        // CELESTE_LADDER_MAXBITS bisects: Bits(0..=maxbits) then Exact.
        let maxbits: u8 = std::env::var("CELESTE_LADDER_MAXBITS")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(15);
        let precisions: Vec<RemPrecision> = (0u8..=maxbits)
            .map(RemPrecision::Bits)
            .chain(std::iter::once(RemPrecision::Exact))
            .collect();

        let make_engine = |precision: RemPrecision| {
            set_rem_precision(precision);
            Ok(Box::new(crate::compiled::FrameEngine::new_for_start_room(&program)?)
                as Box<dyn FrameStep>)
        };
        let make_initial = || {
            Ok(vec![Block::from_state(
                &crate::trace::refengine::RefEngine::new()?.initial_state()?,
            )?])
        };

        let outcome =
            ladder_at_horizon(make_engine, make_initial, dir, 14, &precisions).expect("ladder");
        match outcome {
            HorizonOutcome::Confirmed => {
                eprintln!("[ladder-full] all 17 levels through Exact reached the win");
            }
            HorizonOutcome::Refuted { level } => {
                panic!("level {level} (of 17) refuted - the new ladder breaks before concrete");
            }
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// THE LADDER VALIDATION (Philippe's "validate the per-rem-level forwards,
    /// backward equality is implied"). Run the new ladder over rem 0 then rem 1
    /// on the compiled engine, with a synthetic early win at (8,107) in room
    /// (1,0) - reached ~frame 8 on the fall path, with many states per frame.
    /// rem-1's forward is filtered by rem-0's backward marks; if it still finds
    /// the win, `Confirmed`, the backward did NOT drop the winning path. A
    /// backward that under-marks would over-filter rem 1 and it would refute.
    /// This exercises forward + backward + MarkFilter end to end on the kernels.
    #[test]
    #[ignore]
    fn new_ladder_backward_preserves_win_across_rem_levels() {
        use crate::interpreter::abstraction::{set_rem_precision, RemPrecision};
        std::env::set_var("CELESTE_START_ROOM", "1,0");
        std::env::set_var("CELESTE_WIN_AT_XY", "8,107");

        let program = crate::program::frozen::rewritten("rewrites-compile.jsonl")
            .expect("program");
        let dir = std::path::Path::new("/var/tmp/celeste-frame-ladder-test");
        let _ = std::fs::remove_dir_all(dir);

        let make_engine = |precision: RemPrecision| {
            set_rem_precision(precision);
            Ok(Box::new(crate::compiled::FrameEngine::new_for_start_room(&program)?)
                as Box<dyn FrameStep>)
        };
        let make_initial = || {
            Ok(vec![Block::from_state(
                &crate::trace::refengine::RefEngine::new()?.initial_state()?,
            )?])
        };

        let outcome = ladder_at_horizon(
            make_engine,
            make_initial,
            dir,
            14,
            &[RemPrecision::Bits(0), RemPrecision::Bits(1)],
        )
        .expect("ladder");
        match outcome {
            HorizonOutcome::Confirmed => {
                eprintln!("[ladder] rem0 and rem1 both reached the synthetic win - backward preserved it");
            }
            HorizonOutcome::Refuted { level } => {
                panic!(
                    "level {level} lost the win that a coarser level found - \
                     the backward under-marked (dropped a winning-path state)"
                );
            }
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// forward_resume, extending a checkpointed forward, reproduces a fresh run:
    /// run fresh to frame 4, capture frame 4's key set, then resume from frame 2
    /// out to 4 (rebuilding visited from the checkpoints) and check frame 4 is
    /// byte-for-byte the same key set.
    #[test]
    #[ignore]
    fn forward_resume_matches_fresh() {
        use rustc_hash::FxHashSet;
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-resume-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("mkdir");
        let keyset = |frame: u32| -> FxHashSet<(u64, u64)> {
            load_frame(dir, frame)
                .expect("load")
                .iter()
                .flat_map(|b| b.keys().to_vec())
                .collect()
        };

        {
            let mut e = RefEngine::new().expect("engine");
            let init = vec![Block::from_state(&e.initial_state().expect("init")).expect("block")];
            forward_run(&mut e, init, dir, 4, false, None).expect("fresh");
        }
        let fresh4 = keyset(4);
        assert!(!fresh4.is_empty(), "fresh frame 4 empty");

        // Resume from frame 2 -> recomputes frames 3 and 4.
        {
            let mut e = RefEngine::new().expect("engine");
            let r = forward_resume(&mut e, dir, 2, 4, None).expect("resume");
            assert_eq!(r.frames, 4, "resume did not reach frame 4");
        }
        let resumed4 = keyset(4);
        assert_eq!(fresh4, resumed4, "resume diverged from fresh at frame 4");
        eprintln!("[resume] frame 4 key set identical: {} keys", fresh4.len());
        let _ = std::fs::remove_dir_all(dir);
    }

    /// End-to-end wiring of the ladder's forward+refute path: run one level with
    /// the reference over the intro (which has no win in 4 frames), and check
    /// the ladder reports Refuted. Exercises make_engine/make_initial, the
    /// filtered forward, and the no-win -> Refuted branch. (The Optimal/backward
    /// branch needs a real winning room - the "together" validation.)
    #[test]
    #[ignore]
    fn ladder_refutes_when_no_win_by_horizon() {
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-ladder-test");
        let _ = std::fs::remove_dir_all(dir);
        use crate::interpreter::abstraction::RemPrecision;
        let outcome = ladder_at_horizon(
            |_precision| Ok(Box::new(RefEngine::new()?) as Box<dyn FrameStep>),
            || Ok(vec![Block::from_state(&RefEngine::new()?.initial_state()?)?]),
            dir,
            4,
            &[RemPrecision::Bits(0)],
        )
        .expect("ladder");
        match outcome {
            HorizonOutcome::Refuted { level } => {
                assert_eq!(level, 0, "refuted at the wrong level");
                eprintln!("[ladder] refuted at level={level} (no win in the 4-frame intro)");
            }
            HorizonOutcome::Confirmed => {
                panic!("ladder wrongly reported Confirmed for the win-less intro");
            }
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    /// Mechanical proof of the backward walk (the intro has no real win, so we
    /// seed artificially). Run 4 forward frames recording the pos-graph, then
    /// seed the backward from ALL of frame 4's states and walk back. The intro
    /// is a deterministic chain (one state per frame), so backward must mark a
    /// state at every earlier frame, and - the invariant Philippe named - it
    /// must re-run no more states than forward produced.
    #[test]
    #[ignore]
    fn backward_walk_propagates_along_the_intro_chain() {
        let mut engine = RefEngine::new().expect("ref engine");
        let init = vec![Block::from_state(&engine.initial_state().expect("initial state")).expect("block")];
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-bwd-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("mkdir");

        let horizon = 4;
        let fwd = forward_run(&mut engine, init, dir, horizon, true, None).expect("forward");
        let graph = fwd.pos_graph.expect("pos graph");

        // Seed from every state at the horizon frame.
        let seed: Vec<((u64, u64), u32)> = load_frame(dir, horizon)
            .expect("load horizon")
            .iter()
            .flat_map(|b| {
                let keys = b.keys().to_vec();
                let cells = b.positions().expect("cells");
                keys.into_iter().zip(cells).collect::<Vec<_>>()
            })
            .collect();
        assert!(!seed.is_empty(), "no states at the horizon to seed from");

        let bwd = backward_walk(&mut engine, dir, horizon, &graph, seed).expect("backward");
        eprintln!(
            "[backward] marked {} states, {} re-runs",
            bwd.marked.len(),
            bwd.reruns
        );
        // The deterministic chain: a state marked at every frame 1..=horizon.
        assert!(
            bwd.marked.len() >= horizon as usize,
            "backward marked {} states, expected >= {}",
            bwd.marked.len(),
            horizon
        );
        // Philippe's cost invariant: backward re-runs a position-filtered subset,
        // so it must not exceed the forward frame count (states produced).
        let fwd_states: usize = (1..=horizon)
            .map(|f| load_frame(dir, f).expect("load").iter().map(|b| b.lanes()).sum::<usize>())
            .sum();
        assert!(
            (bwd.reruns as usize) <= fwd_states,
            "backward re-ran {} > forward {} states - narrowing is broken",
            bwd.reruns,
            fwd_states
        );
        let _ = std::fs::remove_dir_all(dir);
    }
}
