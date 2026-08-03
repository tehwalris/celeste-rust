//! `widen_rem` - apply the rem interval abstraction early, at a chosen block.
//!
//! # Why
//!
//! `make_state_abstract` widens `player.rem.x/.y` to the interval
//! `[-0.5, 0.5)` at the frame boundary - that is the search's deliberate
//! over-approximation, and it is half of why the boundary merge dedups so
//! well: lanes differing only in rem stop being distinct rows. The
//! `widen_buttons` experiment showed a mid-frame merge stays ~2x more
//! expensive than the boundary one precisely because rem is still concrete
//! per lane there (67% of rows removed across 24 columns vs 89% across 17).
//! This rule inserts the same widening into the program at a chosen block:
//! load `player.rem.x/.y`, pass each through the `__widen_rem` builtin
//! (assert containment, return the full interval as a scalar), store back.
//! Placed where rem is dead, an `add_hint` merge just downstream becomes as
//! strong as the frame boundary's - scheduled before the update tail and
//! draw run.
//!
//! # Soundness
//!
//! rem is written and read only by `obj.move`, which the object loop runs
//! *before* `type.update` - so inside the update body rem is dead until next
//! frame's move, and next frame sees the same interval either way (the
//! boundary widening is idempotent on an already-widened rem; its
//! containment asserts accept an interval inside the interval). Two claims
//! are traded for loud failures rather than proven: that no rem read runs
//! between the insertion and the boundary (a violated claim reads the
//! interval where the original program read a concrete number, which the
//! differential screen catches), and that the value widened really lies in
//! `[-0.5, 0.5)` (the builtin's containment check - the same assertion
//! `make_state_abstract` makes). The object is named
//! explicitly (the update body's `this` argument) - note the `player`
//! global is the *type table*, not the instance, so it cannot be used here. Screen at full depth.

use anyhow::{anyhow, Result};

use crate::ir::{Instruction, Label, LocalId};

use super::super::program::Program;
use super::{require, LocalIdAllocator};

const WIDEN: &str = "__widen_rem";

/// The synthesized prefix. Ids come from `alloc`; `object` is the player
/// instance (the update body's `this`, an argument, so it dominates any
/// block of the function).
fn widen_sequence(alloc: &mut LocalIdAllocator, object: LocalId) -> Vec<(LocalId, Instruction)> {
    let mut out = Vec::with_capacity(12);
    let rem_cell = alloc.fresh();
    out.push((
        rem_cell,
        Instruction::GetField { receiver: object, field: "rem".to_string(), create_if_missing: false },
    ));
    let rem = alloc.fresh();
    out.push((rem, Instruction::Load { source: rem_cell }));
    let widen_cell = alloc.fresh();
    out.push((
        widen_cell,
        Instruction::GetGlobal { name: WIDEN.to_string(), create_if_missing: false },
    ));
    let widen_fun = alloc.fresh();
    out.push((widen_fun, Instruction::Load { source: widen_cell }));
    for field in ["x", "y"] {
        let coord_cell = alloc.fresh();
        out.push((
            coord_cell,
            Instruction::GetField {
                receiver: rem,
                field: field.to_string(),
                create_if_missing: false,
            },
        ));
        let coord = alloc.fresh();
        out.push((coord, Instruction::Load { source: coord_cell }));
        let widened = alloc.fresh();
        // A plain `Call` for the same reason as `widen_buttons`: the builtin
        // must stay opaque to `cse` and every speculation rule.
        out.push((widened, Instruction::Call { closure: widen_fun, args: vec![coord] }));
        let store = alloc.fresh();
        out.push((store, Instruction::Store { target: coord_cell, source: widened }));
    }
    out
}

pub fn apply(program: &mut Program, function: &str, block: &str, object: LocalId) -> Result<usize> {
    let fun = program.get(function)?;
    let label = Label::from(block.to_string());
    let target = fun
        .cfg
        .named
        .get(&label)
        .ok_or_else(|| anyhow!("{} has no block named {}", function, block))?;
    let phi_count = target.split_block_phi_instructions().0.len();
    let mut alloc = LocalIdAllocator::for_function(fun);
    let sequence = widen_sequence(&mut alloc, object);

    let fun = program.get_mut(function)?;
    let target = fun.cfg.named.get_mut(&label).unwrap();
    target.instructions.splice(phi_count..phi_count, sequence);
    Ok(1)
}

/// Independent check: the target block gained exactly the widen sequence
/// after its phis, and nothing else in the program changed.
pub fn verify(
    before: &Program,
    after: &Program,
    function: &str,
    block: &str,
    object: LocalId,
) -> Result<()> {
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
        let mut expect_alloc = LocalIdAllocator::for_function(before_fun);
        for (l, before_block) in &before_fun.cfg.named {
            let after_block = after_fun
                .cfg
                .named
                .get(l)
                .ok_or_else(|| anyhow!("block {} exists only before", l.as_str()))?;
            if *l != label {
                require(
                    before_block == after_block,
                    format!("block {} changed, but only {} may", l.as_str(), block),
                )?;
                continue;
            }
            require(
                before_block.terminator == after_block.terminator
                    && before_block.hint_normalize == after_block.hint_normalize,
                "the terminator or hint flag changed",
            )?;
            let phi_count = before_block.split_block_phi_instructions().0.len();
            let expected = widen_sequence(&mut expect_alloc, object);
            let n = expected.len();
            require(
                after_block.instructions.len() == before_block.instructions.len() + n,
                "the block did not grow by exactly the widen sequence",
            )?;
            require(
                after_block.instructions[..phi_count] == before_block.instructions[..phi_count],
                "the phis changed",
            )?;
            require(
                after_block.instructions[phi_count + n..]
                    == before_block.instructions[phi_count..],
                "the original instructions changed",
            )?;
            require(
                &after_block.instructions[phi_count..phi_count + n] == expected.as_slice(),
                "the inserted instructions are not the widen sequence",
            )?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, FunDef, GlobalId, Terminator};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(name: &str) -> Label {
        Label::from(name.to_string())
    }

    fn program() -> Program {
        let mut map = crate::ir::new_label_map();
        map.insert(
            label("a"),
            Block {
                instructions: vec![(id(10), Instruction::BoolConstant { value: true })],
                terminator: (id(11), Terminator::Return { value: None }),
                hint_normalize: false,
            },
        );
        let entry = Block {
            instructions: vec![],
            terminator: (id(12), Terminator::UnconditionalBranch { target: label("a") }),
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
    fn inserts_and_verifies() {
        let before = program();
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "a", id(2)).unwrap(), 1);
        let block = &after.get("f").unwrap().cfg.named[&label("a")];
        assert_eq!(block.instructions.len(), 1 + 12);
        assert_eq!(
            block.instructions.last().unwrap().1,
            Instruction::BoolConstant { value: true }
        );
        verify(&before, &after, "f", "a", id(2)).unwrap();
    }

    #[test]
    fn refuses_a_missing_block() {
        let mut p = program();
        assert!(apply(&mut p, "f", "no_such_block", id(2)).is_err());
    }

    #[test]
    fn verify_rejects_a_tampered_call() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "f", "a", id(2)).unwrap();
        let block = after
            .get_mut("f")
            .unwrap()
            .cfg
            .named
            .get_mut(&label("a"))
            .unwrap();
        // Redirect the first call's argument.
        let call_pos = block
            .instructions
            .iter()
            .position(|(_, i)| matches!(i, Instruction::Call { .. }))
            .unwrap();
        if let Instruction::Call { args, .. } = &mut block.instructions[call_pos].1 {
            args[0] = id(10);
        }
        assert!(verify(&before, &after, "f", "a", id(2)).is_err());
    }

    #[test]
    fn verify_rejects_no_change() {
        let p = program();
        assert!(verify(&p, &p, "f", "a", id(2)).is_err());
    }
}
