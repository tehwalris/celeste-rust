//! `remove_hint` - unmark a block as an early normalize point.
//!
//! The exact inverse of `add_hint`, for hints that predate the branch-free
//! rewrite. The two normalize points the program ships with (plus the
//! leftover copy in the uninlined `_update_62`) were placed for the
//! original, fragment-heavy program; on the rewritten program each one
//! costs a full mid-frame `vectorize_states` pass, and whether the
//! fragment reduction it buys still pays is a measurement, not a given.
//! Removing the flag from the *rewritten* program only (the original keeps
//! its hints) is exactly what the recipe is for, and `rewrite verify`
//! validates the removal end-to-end: merging is semantics-preserving, so
//! the lane sets must be identical with and without the merge point.
//!
//! # Soundness
//!
//! Like `add_hint`, the flag changes scheduling, not meaning: a block runs
//! lane-wise identically on merged and unmerged states, and states that
//! are not merged mid-frame meet the same shape-grouped merge at the frame
//! boundary. The rule only clears a flag; `verify` demands the programs
//! are identical except that one flag.

use anyhow::{anyhow, Result};

use crate::ir::Label;

use super::super::program::Program;
use super::require;

pub fn apply(program: &mut Program, function: &str, block: &str) -> Result<usize> {
    let fun = program.get_mut(function)?;
    let label = Label::from(block.to_string());
    let target = fun
        .cfg
        .named
        .get_mut(&label)
        .ok_or_else(|| anyhow!("{} has no block named {}", function, block))?;
    require(
        target.hint_normalize,
        format!("{}:{} is not a normalize point", function, block),
    )?;
    target.hint_normalize = false;
    Ok(1)
}

/// Independent check: the programs are identical except that the named
/// block's `hint_normalize` flag went from set to unset.
pub fn verify(before: &Program, after: &Program, function: &str, block: &str) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "function count changed",
    )?;
    let label = Label::from(block.to_string());
    for (name, before_fun) in &before.functions {
        let after_fun = after
            .functions
            .get(name)
            .ok_or_else(|| anyhow!("{} exists only before", name.as_str()))?;
        if name.as_str() != function {
            require(
                before_fun.cfg == after_fun.cfg,
                format!("{} changed, but only {} may", name.as_str(), function),
            )?;
            continue;
        }
        require(
            before_fun.cfg.entry == after_fun.cfg.entry,
            "the entry block changed",
        )?;
        require(
            before_fun.cfg.named.len() == after_fun.cfg.named.len(),
            "block count changed",
        )?;
        require(
            before_fun.cfg.named.contains_key(&label),
            format!("{} has no block named {}", function, block),
        )?;
        for (l, before_block) in &before_fun.cfg.named {
            let after_block = after_fun
                .cfg
                .named
                .get(l)
                .ok_or_else(|| anyhow!("block {} exists only before", l.as_str()))?;
            if *l == label {
                require(
                    before_block.hint_normalize && !after_block.hint_normalize,
                    format!("{} did not go set -> unset", block),
                )?;
                let mut unflagged = before_block.clone();
                unflagged.hint_normalize = false;
                require(
                    unflagged == *after_block,
                    format!("{} changed beyond the flag", block),
                )?;
            } else {
                require(
                    before_block == after_block,
                    format!("{} changed, but only {} may", l.as_str(), block),
                )?;
            }
        }
    }
    Ok(())
}
