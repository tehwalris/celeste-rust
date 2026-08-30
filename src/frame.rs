//! The frame-step interface and the minimal outer loop (plans/architecture.md).
//!
//! This is the rebuilt core, written fresh against the agreed interfaces rather
//! than copied from `search/run.rs`. A state is OPAQUE to the loop; the only
//! things that cross are a row's KEY and POSITION, and the frame-step call.
//!
//! Not yet wired into `lib.rs` (the binaries cutter is mid-edit); wire +
//! compile-check when that lands, then grow the impls (kernel + interpreter)
//! behind `FrameStep` and grow the loop to the 10-sentence spec.

use anyhow::Result;

use crate::interpreter::state::State;

/// A row leaving the frame step: an opaque state plus the two things the outer
/// loop is allowed to see - its identity and where its player is.
pub struct Row {
    /// The 128-bit canonical row key (shape + content). Identity for dedup, the
    /// visited set, and checkpoints. Computed once, inside the frame step.
    pub key: (u64, u64),
    /// The player-position cell, for grouping and the position graph.
    pub position: u32,
    /// The opaque state. The loop never reads inside it - it only passes it
    /// back into the frame step and serializes it in a block.
    pub state: State,
}

/// The one interface between the outer loop and the engines. `run` takes a
/// batch of input lanes (one state, possibly multi-lane) and returns every
/// output row - all internal branching enumerated, already widened for the
/// engine's precision, each row carrying its key and position.
///
/// Two implementations, behind this one trait: the compiled kernels (fast) and
/// the scalar interpreter (`trace::refengine`, the trusted reference). The loop
/// holds a `&dyn FrameStep` and never knows which.
pub trait FrameStep {
    fn run(&self, state: State) -> Result<Vec<Row>>;
}

/// The minimal forward frame: drive the frame step over the frontier, keep only
/// rows whose key is new. Returns the next frontier (the survivors) and whether
/// any survivor exits the room (a win).
///
/// This is the whole of it - dedup is decided at the door (a row is kept iff
/// its key is new), so the pre-dedup fan-out never accumulates. No chunking, no
/// phased/serial/parallel variants, no materialize-then-filter. Everything the
/// old `step_inner` did beyond this served machinery we are cutting.
pub fn forward_frame(
    engine: &dyn FrameStep,
    frontier: Vec<Row>,
    visited: &mut Visited,
    is_win: impl Fn(&Row) -> bool,
) -> Result<(Vec<Row>, bool)> {
    let mut next = Vec::new();
    let mut won = false;
    for row in frontier {
        for out in engine.run(row.state)? {
            if visited.insert(out.key) {
                won |= is_win(&out);
                next.push(out);
            }
        }
    }
    Ok((next, won))
}

/// The frontier-only visited set: the keys of every row ever produced. A new
/// row is kept iff its key was not already here (this is both the within-frame
/// and the across-frame dedup - one structure, one lookup at the door).
///
/// Placeholder over an in-RAM set; the real one is tiered/mmap-backed
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
    /// True if `key` was NOT already present (i.e. this row is new, keep it).
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
