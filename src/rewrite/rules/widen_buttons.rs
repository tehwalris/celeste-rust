//! `widen_buttons` - re-abstract the button cells early, at a chosen block.
//!
//! # Why
//!
//! The frame chunk ends with `__reset_button_states()`: every `__button_states`
//! cell becomes a fresh unknown boolean *after* draw, and that is why the
//! frame-boundary merge can dedup lanes whose game states converged despite
//! different inputs - the concretized button values no longer distinguish
//! their rows. Mid-frame, those values do distinguish rows, which is why an
//! `add_hint` merge point alone reduces fragments but not lanes (measured
//! +52% at 34 - pure cost).
//!
//! This rule inserts the in-place equivalent of the reset at the head of a
//! chosen block: store a fresh unknown into each of the six button cells.
//! Placed at a block that post-dominates every `btn` read of the frame, the
//! button columns become equal again, and a merge point just downstream
//! dedups converged lanes *before* the rest of the frame runs - the lane
//! reduction the frame boundary performs, scheduled earlier.
//!
//! # Soundness
//!
//! This is sound exactly when every `btn` read of the frame precedes the
//! insertion point: the cells are then dead until the frame-end reset
//! overwrites them again, and overwriting a dead cell changes nothing
//! observable. That premise is *claimed, not proven* - it is a property of
//! the whole program, not of the block. The trade for the missing proof is
//! the usual one: if a `btn` read ever runs after the widen, it re-splits on
//! a fresh unknown and diverges from the original program's concretized
//! value, which the frame-by-frame differential screen detects loudly.
//! Screen at full depth.

use anyhow::{anyhow, Result};

use crate::ir::{Instruction, Label, LocalId};
use crate::pico8_num::Pico8Num;

use super::super::program::Program;
use super::{require, LocalIdAllocator};

const TABLE: &str = "__button_states";
const FRESH: &str = "__new_unknown_boolean";
const BUTTONS: i16 = 6;

/// The synthesized prefix: (instructions, count). Ids come from `alloc`.
fn widen_sequence(alloc: &mut LocalIdAllocator) -> Vec<(LocalId, Instruction)> {
    let mut out = Vec::with_capacity(4 + 4 * BUTTONS as usize);
    let table_cell = alloc.fresh();
    out.push((
        table_cell,
        Instruction::GetGlobal { name: TABLE.to_string(), create_if_missing: false },
    ));
    let table = alloc.fresh();
    out.push((table, Instruction::Load { source: table_cell }));
    let fresh_cell = alloc.fresh();
    out.push((
        fresh_cell,
        Instruction::GetGlobal { name: FRESH.to_string(), create_if_missing: false },
    ));
    let fresh_fun = alloc.fresh();
    out.push((fresh_fun, Instruction::Load { source: fresh_cell }));
    for i in 1..=BUTTONS {
        let index = alloc.fresh();
        out.push((
            index,
            Instruction::NumberConstant { value: Pico8Num::from_i16(i) },
        ));
        let cell = alloc.fresh();
        out.push((
            cell,
            Instruction::GetIndex { receiver: table, index, create_if_missing: false },
        ));
        let unknown = alloc.fresh();
        // A plain `Call`, deliberately not a `CallBuiltin`: the builtin is
        // impure in the one way that matters to `cse` - two calls yield two
        // *independent* unknowns, and a pure call would invite collapsing
        // them into one. `Call` is a barrier to every optimization here.
        out.push((unknown, Instruction::Call { closure: fresh_fun, args: vec![] }));
        let store = alloc.fresh();
        out.push((store, Instruction::Store { target: cell, source: unknown }));
    }
    out
}

pub fn apply(program: &mut Program, function: &str, block: &str) -> Result<usize> {
    let fun = program.get(function)?;
    let label = Label::from(block.to_string());
    let target = fun
        .cfg
        .named
        .get(&label)
        .ok_or_else(|| anyhow!("{} has no block named {}", function, block))?;
    let phi_count = target.split_block_phi_instructions().0.len();
    let mut alloc = LocalIdAllocator::for_function(fun);
    let sequence = widen_sequence(&mut alloc);

    let fun = program.get_mut(function)?;
    let target = fun.cfg.named.get_mut(&label).unwrap();
    target.instructions.splice(phi_count..phi_count, sequence);
    Ok(1)
}

/// Independent check: the target block gained exactly the widen sequence
/// (fresh, distinct ids; correct wiring) after its phis, and nothing else in
/// the program changed.
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
            let expected = widen_sequence(&mut expect_alloc);
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
            let inserted = &after_block.instructions[phi_count..phi_count + n];
            require(
                inserted == expected.as_slice(),
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
        assert_eq!(apply(&mut after, "f", "a").unwrap(), 1);
        let block = &after.get("f").unwrap().cfg.named[&label("a")];
        assert_eq!(block.instructions.len(), 1 + 4 + 4 * 6);
        // Original instruction survives at the end.
        assert_eq!(
            block.instructions.last().unwrap().1,
            Instruction::BoolConstant { value: true }
        );
        verify(&before, &after, "f", "a").unwrap();
    }

    #[test]
    fn refuses_a_missing_block() {
        let mut p = program();
        assert!(apply(&mut p, "f", "no_such_block").is_err());
    }

    #[test]
    fn verify_rejects_a_tampered_store() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "f", "a").unwrap();
        // Point the last store somewhere else.
        let block = after
            .get_mut("f")
            .unwrap()
            .cfg
            .named
            .get_mut(&label("a"))
            .unwrap();
        let last_store = block.instructions.len() - 2;
        if let Instruction::Store { target, .. } = &mut block.instructions[last_store].1 {
            *target = id(10);
        } else {
            panic!("expected a store");
        }
        assert!(verify(&before, &after, "f", "a").is_err());
    }

    #[test]
    fn verify_rejects_no_change() {
        let p = program();
        assert!(verify(&p, &p, "f", "a").is_err());
    }
}
