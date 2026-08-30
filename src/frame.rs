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
            // Dedup at the door, SHARDED by (shape, cell): insert() reports true
            // for a (key, cell) not seen before in this or any earlier frontier.
            // Building the mask also records the survivors, so a duplicate later
            // in the same frame is caught too.
            let mask: Vec<bool> = keys
                .iter()
                .zip(&cells)
                .map(|(k, &c)| visited.insert(*k, c))
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
    let mut frontier = initial;
    let mut last = 0;
    let mut win_frame = None;
    for frame in 1..=max_frames {
        let (next, won) = forward_frame(
            engine,
            frontier,
            &mut visited,
            |b| Ok(block_wins(b)),
            observer.as_ref(),
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

        let result = forward_run(&mut engine, init, dir, 4, true).expect("forward_run");
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
}
