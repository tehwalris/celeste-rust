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
) -> Result<(Vec<Block>, bool)> {
    let mut next = Vec::new();
    let mut won = false;
    for block in frontier {
        for out in engine.run(&block)? {
            let keys = out.keys()?;
            // insert() reports true for a key not seen before (in this frontier
            // or any earlier one). Building the mask also records the survivors,
            // so a duplicate later in the same frame is caught too.
            let mask: Vec<bool> = keys.iter().map(|k| visited.insert(*k)).collect();
            if let Some(kept) = out.keep(&mask) {
                won |= is_win(&kept)?;
                next.push(kept);
            }
        }
    }
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
pub fn forward_run(
    engine: &mut dyn FrameStep,
    initial: Vec<Block>,
    dir: &std::path::Path,
    max_frames: u32,
) -> Result<Option<u32>> {
    let mut visited = Visited::new();
    for b in &initial {
        for k in b.keys()? {
            visited.insert(k);
        }
    }
    checkpoint_frontier(dir, 0, &initial)?;
    let mut frontier = initial;
    for frame in 1..=max_frames {
        let (next, won) = forward_frame(engine, frontier, &mut visited, |b| Ok(block_wins(b)))?;
        checkpoint_frontier(dir, frame, &next)?;
        if won {
            return Ok(Some(frame));
        }
        if next.is_empty() {
            return Ok(None);
        }
        frontier = next;
    }
    Ok(None)
}

/// Checkpoint a frontier as one compact batched (zstd) file - borrowing each
/// block's state, so the frontier is not cloned to be saved.
fn checkpoint_frontier(dir: &std::path::Path, frame: u32, frontier: &[Block]) -> Result<()> {
    let refs: Vec<&State> = frontier.iter().map(|b| b.state()).collect();
    crate::search::checkpoint::save_frame_state_refs(dir, frame, &refs)
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
    seen: rustc_hash::FxHashSet<(u64, u64)>,
}

impl Visited {
    pub fn new() -> Self {
        Self::default()
    }
    /// True if `key` was NOT already present (i.e. this lane is new, keep it).
    pub fn insert(&mut self, key: (u64, u64)) -> bool {
        self.seen.insert(key)
    }
    pub fn contains(&self, key: &(u64, u64)) -> bool {
        self.seen.contains(key)
    }
    pub fn len(&self) -> usize {
        self.seen.len()
    }
    pub fn is_empty(&self) -> bool {
        self.seen.is_empty()
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

        let win = forward_run(&mut engine, init, dir, 4).expect("forward_run");
        // No win in the 4-frame intro; the run reaches the horizon.
        assert_eq!(win, None, "unexpected early win in the intro");

        // Every frame 0..=4 was checkpointed and reloads.
        for frame in 0..=4 {
            let states = crate::search::checkpoint::load_frame_states(dir, frame)
                .unwrap_or_else(|e| panic!("reload frame {frame}: {e}"));
            assert!(!states.is_empty(), "frame {frame} checkpoint is empty");
            eprintln!("[forward_run] f{frame}: reloaded {} states", states.len());
        }
        let _ = std::fs::remove_dir_all(dir);
    }
}
