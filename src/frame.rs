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

/// The data currency (interface #2). A columnar batch of lanes, carried as one
/// opaque multi-lane `State`. Two columns are EXPOSED - the per-lane key and the
/// per-lane position - and nothing else; the field data stays inside the state.
/// A whole block serializes/compresses compactly (batches share structure), so
/// the loop always moves blocks, never individual lanes.
pub struct Block {
    /// Opaque. The loop never reads inside it - it passes the block back into
    /// the frame step, asks for its key/position columns, splits it by a lane
    /// mask, or serializes it. That is the entire vocabulary.
    state: State,
    /// The key column when the engine already computed it (interface #1 says
    /// keying happens inside the frame step - the kernels hand it back rather
    /// than have the loop re-hash). `None` means "compute on demand".
    keys: Option<Vec<(u64, u64)>>,
}

impl Block {
    pub fn new(state: State) -> Self {
        Self { state, keys: None }
    }
    /// A block whose key column the frame step already computed.
    pub fn with_keys(state: State, keys: Vec<(u64, u64)>) -> Self {
        Self { state, keys: Some(keys) }
    }
    pub fn into_state(self) -> State {
        self.state
    }
    pub fn state(&self) -> &State {
        &self.state
    }

    /// The key column: the 128-bit canonical row key per lane (shape + content).
    /// Identity for dedup, the visited set, and checkpoints. One entry per lane.
    /// Returns the engine-supplied column when present, else computes it.
    pub fn keys(&self) -> Result<Vec<(u64, u64)>> {
        match &self.keys {
            Some(k) => Ok(k.clone()),
            None => crate::compiled::engine_row_keys(&self.state),
        }
    }

    /// The position column: the player-position cell per lane, for grouping and
    /// the position graph. Same length as `keys`.
    pub fn positions(&self) -> Result<Vec<u32>> {
        crate::search::pos_graph::state_cells(&self.state)
    }

    /// Keep only the lanes whose mask entry is true, as a new block; `None` if
    /// none survive. The one splitting primitive the loop needs (dedup at the
    /// door repacks with this).
    pub fn keep(self, mask: &[bool]) -> Option<Block> {
        let (kept, _) = self.state.split_by_condition(mask, true);
        kept.map(Block::new)
    }

    /// The block's shard cell. A regrouped block is uniform in position, so its
    /// whole position column is one value - the shard's cell.
    pub fn shard_cell(&self) -> Result<u32> {
        self.positions()?
            .first()
            .copied()
            .ok_or_else(|| anyhow::anyhow!("empty block has no shard cell"))
    }

    /// The block's shard shape. A regrouped block is one shape.
    pub fn shard_shape(&self) -> u64 {
        crate::interpreter::vectorize::shape_hash_of_state(&self.state)
    }

    /// Number of lanes in this block.
    pub fn lanes(&self) -> usize {
        self.state.vector_size.max(1)
    }

    /// A one-lane block holding just lane `i` (clones the state, then keeps the
    /// single lane). Used by the DRAFT backward, which re-runs one input at a
    /// time so every output trivially belongs to it (no provenance needed). The
    /// wide+lane-bitmask version replaces this.
    pub fn lane(&self, i: usize) -> Option<Block> {
        let lanes = self.lanes();
        let mut mask = vec![false; lanes];
        if i < lanes {
            mask[i] = true;
        }
        let (kept, _) = self.state.clone().split_by_condition(&mask, true);
        kept.map(Block::new)
    }
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
    pub fn allowed(&self, state: &State) -> Result<Vec<bool>> {
        let lanes = state.vector_size.max(1);
        let widened = crate::interpreter::abstraction::make_state_abstract_rem(
            state.clone(),
            self.coarser,
        );
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
    fn run(&mut self, block: &Block) -> Result<Vec<Block>>;
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
) -> Result<(Vec<Block>, bool)> {
    let mut survivors: Vec<State> = Vec::new();
    let mut won = false;
    for block in frontier {
        // Pos-graph edge: the block is uniform in position (record mode's
        // partition), so its single input cell reaches every output cell.
        // Record ALL raw outputs' positions (before dedup) - dedup drops
        // duplicate keys, not reachable positions.
        let c_in = match pos {
            Some(p) => Some(p.input_cell(block.state())?),
            None => None,
        };
        let outputs = engine.run(&block)?;
        for out in outputs {
            let keys = out.keys()?;
            let cells = out.positions()?;
            // Pos-graph edge: record ALL raw outputs' positions (before dedup);
            // dedup drops duplicate keys, not reachable positions.
            if let (Some(p), Some(c_in)) = (pos, c_in) {
                p.record_dsts(c_in, &cells);
            }
            // Ladder filter (coarser level's marked set): discard a lane whose
            // widened-to-coarser form was not marked. Computed before the
            // visited insert so a discarded lane never enters the visited set.
            let allow = match filter {
                Some(f) => Some(f.allowed(out.state())?),
                None => None,
            };
            // Dedup at the door, SHARDED by (shape, cell): insert() reports true
            // for a (key, cell) not seen before in this or any earlier frontier.
            // Building the mask also records the survivors, so a duplicate later
            // in the same frame is caught too. `&&` short-circuits, so a
            // filtered-out lane is never inserted into `visited`.
            let mask: Vec<bool> = keys
                .iter()
                .zip(&cells)
                .enumerate()
                .map(|(i, (k, &c))| {
                    allow.as_ref().map_or(true, |a| a[i]) && visited.insert(*k, c)
                })
                .collect();
            if let Some(kept) = out.keep(&mask) {
                won |= is_win(&kept)?;
                survivors.push(kept.into_state());
            }
        }
    }
    // Regroup the surviving lanes into canonical blocks - group by (shape,
    // partition-class) and merge each group into one wide vectorized block.
    // With the position partition on (`forward_run` record mode), the class
    // includes position, so every block comes out uniform in (shape, position):
    // the block identity the pos-graph and backward are built on. This is also
    // where cross-block duplicate lanes collapse.
    let next: Vec<Block> = crate::interpreter::vectorize::vectorize_states(survivors)
        .into_iter()
        .map(Block::new)
        .collect();
    Ok((next, won))
}

/// A row wins if any lane sits on the room's win target. Pure position (the
/// existing per-lane predicate); no peeking inside the state.
pub fn block_wins(block: &Block) -> bool {
    crate::interpreter::abstraction::win_lane_mask(block.state())
        .iter()
        .any(|&w| w)
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
        let keys = block.keys()?;
        let cells = block.positions()?;
        let wins = crate::interpreter::abstraction::win_lane_mask(block.state());
        for ((k, &c), w) in keys.iter().zip(&cells).zip(wins) {
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
            let keys = block.keys()?;
            let cells = block.positions()?;
            for lane in 0..block.lanes() {
                let (key, cell) = (keys[lane], cells[lane]);
                if marked.contains(key, cell) {
                    continue; // already marked by another target - skip the re-run
                }
                let Some(single) = block.lane(lane) else { continue };
                reruns += 1;
                let mut hit = false;
                'outs: for out in engine.run(&single)? {
                    let ok = out.keys()?;
                    let oc = out.positions()?;
                    for (k, &c) in ok.iter().zip(&oc) {
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
    // Record mode installs the position partition, so every regrouped block
    // (see forward_frame) is uniform in position and the pos-graph's input cell
    // is well defined. Off for a forward-only run: position is content, so
    // partitioning on it never changes the reachable row set, only makes the
    // merge finer (which only the recorder pays for).
    crate::interpreter::vectorize::set_partition_player_position(record);
    let observer = record.then(crate::search::pos_graph::PosObserver::default);

    let mut visited = Visited::new();
    for b in &initial {
        let keys = b.keys()?;
        let cells = b.positions()?;
        for (k, c) in keys.iter().zip(&cells) {
            visited.insert(*k, *c);
        }
    }
    checkpoint_frontier(dir, 0, &initial)?;
    drive_forward(engine, dir, initial, visited, observer, 1, max_frames, filter)
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
    crate::interpreter::vectorize::set_partition_player_position(false);
    let mut visited = Visited::new();
    for f in 0..=from_frame {
        for b in load_frame(dir, f)? {
            let keys = b.keys()?;
            let cells = b.positions()?;
            for (k, c) in keys.iter().zip(&cells) {
                visited.insert(*k, *c);
            }
        }
    }
    let frontier = load_frame(dir, from_frame)?;
    drive_forward(engine, dir, frontier, visited, None, from_frame + 1, to_frame, filter)
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
) -> Result<ForwardResult> {
    let mut last = start_frame.saturating_sub(1);
    let mut win_frame = None;
    for frame in start_frame..=end_frame {
        let (next, won) = forward_frame(
            engine,
            frontier,
            &mut visited,
            |b| Ok(block_wins(b)),
            observer.as_ref(),
            filter,
        )?;
        checkpoint_frontier(dir, frame, &next)?;
        if let Some(o) = observer.as_ref() {
            o.flush();
        }
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
        crate::search::checkpoint::save_states_to(&path, &[block.state()])?;
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
        for st in crate::search::checkpoint::load_states_from(&entry.path())? {
            out.push(Block::new(st));
        }
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
            return Ok(HorizonOutcome::Refuted { level });
        };
        let graph = fwd
            .pos_graph
            .as_ref()
            .expect("record mode always builds the pos graph");
        let bwd = backward_run(engine.as_mut(), &level_dir, h, graph)?;
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
        let init = vec![Block::new(engine.initial_state().expect("initial state"))];

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
        let seed = || vec![Block::new(engine_clone(&engine).initial_state().expect("init"))];

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
            let w = make_state_abstract_rem(b.state().clone(), bits0);
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

    /// THE BRIDGE: the new forward_run on the compiled engine must produce the
    /// same per-frame row-key SETS as the old AbstractRun forward, given the same
    /// program, engine, and seed. Runs a handful of frames on the start room and
    /// compares frame by frame. If this holds, the new orchestration IS the old
    /// one on the real engine - the gate to migrating and deleting run.rs.
    #[test]
    #[ignore]
    fn new_forward_matches_old_on_compiled_engine() {
        use rustc_hash::FxHashSet;
        // Both sides do cross-frame dedup (the new forward always does; the old
        // needs frontier-only). Isolated per-process by nextest.
        std::env::set_var("CELESTE_FRONTIER_ONLY", "1");
        std::env::set_var("CELESTE_COMPILED_FORWARD", "1");

        let program = crate::program::frozen::rewritten("rewrites-compile.jsonl")
            .expect("program");
        let n: u32 = std::env::var("CELESTE_DIFF_FRAMES")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(6);

        // Old forward: AbstractRun on the compiled engine.
        let mut old = crate::search::run::AbstractRun::start(&program).expect("old start");
        let seed: Vec<State> = old.states().to_vec();

        // New forward: same seed, the compiled FrameEngine behind FrameStep.
        let dir = std::path::Path::new("/var/tmp/celeste-frame-rebuild-diff-test");
        let _ = std::fs::remove_dir_all(dir);
        std::fs::create_dir_all(dir).expect("mkdir");
        let mut engine =
            crate::compiled::FrameEngine::new_for_start_room(&program).expect("frame engine");
        let initial: Vec<Block> = seed.into_iter().map(Block::new).collect();
        forward_run(&mut engine, initial, dir, n, false, None).expect("new forward");

        let key_union = |states: &[State]| -> FxHashSet<(u64, u64)> {
            states
                .iter()
                .flat_map(|s| crate::compiled::engine_row_keys(s).expect("keys"))
                .collect()
        };

        for f in 1..=n {
            old.step().expect("old step");
            let old_keys = key_union(old.states());
            let new_blocks = load_frame(dir, f).expect("load new frame");
            let new_keys: FxHashSet<(u64, u64)> = new_blocks
                .iter()
                .flat_map(|b| b.keys().expect("keys"))
                .collect();
            assert_eq!(
                new_keys.len(),
                old_keys.len(),
                "frame {f}: new {} keys vs old {} keys",
                new_keys.len(),
                old_keys.len()
            );
            assert_eq!(new_keys, old_keys, "frame {f}: row-key sets differ");
            eprintln!("[diff] frame {f}: {} keys match", new_keys.len());
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
                .flat_map(|b| b.keys().expect("keys"))
                .collect()
        };

        {
            let mut e = RefEngine::new().expect("engine");
            let init = vec![Block::new(e.initial_state().expect("init"))];
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
            || Ok(vec![Block::new(RefEngine::new()?.initial_state()?)]),
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
        let init = vec![Block::new(engine.initial_state().expect("initial state"))];
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
                let keys = b.keys().expect("keys");
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
