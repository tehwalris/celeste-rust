//! `add_hint` - mark a block as an early normalize point.
//!
//! # Why
//!
//! The interpreter merges states wherever a block carries the
//! `hint_normalize` flag (and at the frame boundary): states arriving there
//! are accumulated, vectorized by shape, and row-deduped before execution
//! continues. The program ships with two such points per frame, and both sit
//! *before* the update body - nothing crushes the fan-out that the six btn
//! concretization splits stack up mid-frame (62 fragments at peak, the full
//! button product). Fragment count is what the state machinery's cost scales
//! with, so merging right after the fan-out completes - before it multiplies
//! through the rest of the frame - is the point of this rule.
//!
//! What an added merge point does today: it reduces *fragments*, not
//! *lanes*. Mid-frame, fragments from a btn split still differ in the
//! concretized button cell, so their rows stay distinct until the frame
//! boundary resets the buttons - the merged state simply carries the button
//! column as a per-lane vector. That makes this merge-based partial lane
//! expansion, with one advantage over the `expand` instruction: only columns
//! that genuinely differ widen (`merge_values` collapses equal columns back
//! to scalars), where `expand` duplicates every cell. Lane reduction
//! mid-frame is *reachable*, though: after a button's last read its cell is
//! dead, and widening it there (the boundary's `__reset_button_states`, just
//! earlier) makes the column equal again, at which point a merge dedups
//! converged lanes early. That widening rule does not exist yet.
//!
//! # Soundness
//!
//! The flag changes scheduling, not meaning: the merge it triggers is
//! `vectorize_states`, the same shape-grouped concatenation + row dedup every
//! frame boundary already applies to every state, and a block runs lane-wise
//! identically on merged and unmerged states. The rule itself only sets a
//! flag; `verify` demands the programs are identical except that one flag.

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
        !target.hint_normalize,
        format!("{}:{} is already a normalize point", function, block),
    )?;
    target.hint_normalize = true;
    Ok(1)
}

/// Independent check: the programs are identical except that the named
/// block's `hint_normalize` flag went from unset to set.
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
                    !before_block.hint_normalize && after_block.hint_normalize,
                    format!("{} did not go unset -> set", block),
                )?;
                let mut flagged = before_block.clone();
                flagged.hint_normalize = true;
                require(
                    flagged == *after_block,
                    format!("{} changed beyond the flag", block),
                )?;
            } else {
                require(
                    before_block == after_block,
                    format!("block {} changed, but only {} may", l.as_str(), block),
                )?;
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, FunDef, GlobalId, Instruction, LocalId, Terminator};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(name: &str) -> Label {
        Label::from(name.to_string())
    }

    fn block() -> Block {
        Block {
            instructions: vec![(id(1), Instruction::BoolConstant { value: true })],
            terminator: (id(2), Terminator::Return { value: None }),
            hint_normalize: false,
        }
    }

    fn program() -> Program {
        let mut map = crate::ir::new_label_map();
        map.insert(label("a"), block());
        map.insert(label("b"), block());
        let entry = Block {
            instructions: vec![],
            terminator: (id(3), Terminator::UnconditionalBranch { target: label("a") }),
            hint_normalize: false,
        };
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![],
            cfg: Cfg::new(entry, map),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions }
    }

    #[test]
    fn sets_the_flag_and_verifies() {
        let before = program();
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "a").unwrap(), 1);
        assert!(after.get("f").unwrap().cfg.named[&label("a")].hint_normalize);
        verify(&before, &after, "f", "a").unwrap();
    }

    #[test]
    fn refuses_a_missing_block() {
        let mut p = program();
        assert!(apply(&mut p, "f", "no_such_block").is_err());
    }

    #[test]
    fn refuses_an_existing_hint() {
        let mut p = program();
        apply(&mut p, "f", "a").unwrap();
        assert!(apply(&mut p, "f", "a").is_err());
    }

    #[test]
    fn verify_rejects_an_extra_change() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "f", "a").unwrap();
        // A second flag beyond the named one must be rejected.
        after
            .get_mut("f")
            .unwrap()
            .cfg
            .named
            .get_mut(&label("b"))
            .unwrap()
            .hint_normalize = true;
        assert!(verify(&before, &after, "f", "a").is_err());
    }

    #[test]
    fn verify_rejects_no_change() {
        let p = program();
        assert!(verify(&p, &p, "f", "a").is_err());
    }

    #[test]
    fn verify_rejects_the_wrong_block() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "f", "b").unwrap();
        assert!(verify(&before, &after, "f", "a").is_err());
    }
}
