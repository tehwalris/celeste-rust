//! The program under search, collapsed to the one value the runtime reads.
//!
//! There used to be a whole pipeline here: compile the Lua to a CFG IR
//! (`celeste-ir`'s `frontend`), then replay a recipe of rewrite instructions
//! over it, then freeze the result as a checked-in artifact. All of that
//! existed only to produce a single value the runtime ever reads:
//! `Program::merge_partition_cells`. For the compile recipe that value is a
//! constant (the interpreter's `set_merge_partition_patterns` and
//! `FrameEngine::new` are its only consumers), so the `Program` is now that
//! constant and nothing else. The CFG, the frontend, the recipe rules and
//! the freeze/load machinery are gone.

pub mod frozen;

/// Field-path patterns of the merge-partition cells (the `partition_merge`
/// rule's `pm1` set): the interpreter's merges group by the values of these
/// cells in addition to shape, so branches on them route instead of
/// splitting. This is exactly what the deleted compile recipe produced.
pub const MERGE_PARTITION_CELLS: [&str; 6] =
    ["dash_time", "djump", "has_dashed", "p_dash", "p_jump", "freeze"];

#[derive(Clone)]
pub struct Program {
    /// The only field anything outside this module reads. See
    /// `MERGE_PARTITION_CELLS`.
    pub merge_partition_cells: Vec<String>,
}

impl Program {
    /// The program as every runtime caller uses it: the compile recipe's
    /// partition cells.
    pub fn partitioned() -> Self {
        Self {
            merge_partition_cells: MERGE_PARTITION_CELLS.iter().map(|s| s.to_string()).collect(),
        }
    }
}
