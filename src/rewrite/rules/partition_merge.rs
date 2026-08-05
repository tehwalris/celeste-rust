//! `partition_merge` - designate the merge-partition cells.
//!
//! Sets `Program::merge_partition_cells`: field-path patterns (resolved by
//! the interpreter against cell names, exact or ".<name>" suffix) whose
//! values join the merge grouping key. States in different classes never
//! merge, so a branch on a partition cell is uniform in every state - it
//! routes instead of splitting - and the cell stays `Scalar` per state,
//! leaving every dedup key. This is the recipe-carried form of the
//! `CELESTE_PARTITION_CELLS` pilot (which remains as an experiment
//! override): the key travels with the program and is replay-verified.
//!
//! # Soundness
//!
//! Partitioning changes merge *scheduling*, not meaning: lanes that
//! converge across classes dedup one merge later, when their current
//! values agree, and the lane set is unchanged - `rewrite verify` checks
//! the rewritten-partitioned program against the original end to end.
//! The rule itself only sets the annotation; `verify` demands the
//! programs are identical except for it.

use anyhow::Result;

use super::super::program::Program;
use super::require;

pub fn apply(program: &mut Program, cells: &[String]) -> Result<usize> {
    require(
        program.merge_partition_cells.is_empty(),
        "merge-partition cells are already set; use one entry".to_string(),
    )?;
    require(!cells.is_empty(), "empty partition cell list".to_string())?;
    program.merge_partition_cells = cells.to_vec();
    Ok(1)
}

/// Independent check: the programs are identical except that the
/// annotation went from empty to exactly `cells`.
pub fn verify(before: &Program, after: &Program, cells: &[String]) -> Result<()> {
    require(
        before.merge_partition_cells.is_empty(),
        "partition cells were already set before".to_string(),
    )?;
    require(
        after.merge_partition_cells == cells,
        "annotation does not match the entry".to_string(),
    )?;
    require(
        before.functions.len() == after.functions.len(),
        "function count changed".to_string(),
    )?;
    for (name, before_fun) in &before.functions {
        let after_fun = after.functions.get(name);
        require(
            after_fun.is_some_and(|f| f.cfg == before_fun.cfg),
            format!("{} changed, but only the annotation may", name.as_str()),
        )?;
    }
    Ok(())
}
