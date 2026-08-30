//! The rewritten program, collapsed to a constant.
//!
//! A `Program` used to be *derived*: compile the Lua, replay a recipe of
//! rewrite instructions, freeze the result as a checked-in `.program.zst`
//! artifact, and read it back here. The runtime only ever consumed one field
//! of that program - `merge_partition_cells` - and for the compile recipe
//! that field is a constant (`program::MERGE_PARTITION_CELLS`). So the whole
//! derivation and its artifact are gone; `rewritten` returns the constant.
//!
//! The `recipe` name is now vestigial. Callers still pass one (and the
//! `.jsonl` files are still checked in as the provenance of what the constant
//! encodes and as existence gates for a couple of ignored tests), but it is
//! no longer read as an input.

use anyhow::Result;

use crate::program::Program;

/// The rewritten program for a recipe: the constant partition-cell program.
///
/// Every recipe yields the same program now - the only thing the runtime
/// read from a rewritten program was `merge_partition_cells`, and that is a
/// constant. The argument is kept so the callers that name a recipe do not
/// have to change.
pub fn rewritten(_recipe: &str) -> Result<Program> {
    Ok(Program::partitioned())
}
