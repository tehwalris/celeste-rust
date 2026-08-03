//! `fold_reflexive` - a comparison of a value with itself becomes a constant.
//!
//! # Why
//!
//! The compiled `is_solid(x, y)` chains open with a gate on `y > 0`, and where
//! the call site passes a literal `0` the inliner leaves behind
//!
//! ```text
//!   %481 = num Pico8Num("0x0000")
//!   %3485 = %481 > %481
//!   %3486 = br %3485 ? in_k034_and_or_continue_416 : in_k034_and_or_join_417
//! ```
//!
//! - a branch that can never be taken, guarding a whole `check`/`collide`
//! loop. `fold` deliberately refuses arithmetic, because evaluating PICO-8
//! semantics inside the rewriter would make the verifier depend on the
//! applier's notion of truth. But a *reflexive* comparison needs no
//! arithmetic: an SSA local is one value, and a PICO-8 number compares to
//! itself the same way whatever number it is - the representation is 16.16
//! fixed point, so there is no NaN to spoil reflexivity. `x > x` is false and
//! `x <= x` is true without ever computing `x`.
//!
//! # What it does
//!
//! Every `%r = %x op %x` where `op` is one of the six comparisons *and* `%x`
//! is defined by a `NumberConstant` becomes the `BoolConstant` reflexivity
//! dictates:
//!
//! ```text
//!   ==, <=, >=  ->  true
//!   ~=, <,  >   ->  false
//! ```
//!
//! The id keeps its place; nothing else moves. Folding the branch that reads
//! the constant is `fold`'s existing job, and deleting the loop it strands is
//! `dce`'s - this rule only manufactures the constant they need.
//!
//! # Why the operand must be a number constant
//!
//! Reflexivity holds for any *number*, but the comparison operators error
//! loudly on non-numbers, and folding `t > t` for a table would silently
//! replace that loud error with `false`. Requiring the operand to be a visible
//! `NumberConstant` keeps the fold semantics-free: the original instruction
//! provably could not have failed, so replacing its result is the identity.
//! (`==`/`~=` would be safe on any operand - Lua equality is reflexive on
//! every value the IR has - but no site needs that yet, so the narrow premise
//! covers all six operators uniformly.)
//!
//! Bulk rule: no location, deterministic, idempotent (the replaced
//! instruction is a `BoolConstant`, which the detection never matches).

use anyhow::Result;
use rustc_hash::FxHashSet;

use crate::ir::{BinaryOp, FunDef, Instruction, LocalId};

use super::super::print::format_instruction;
use super::super::program::Program;
use super::require;

/// The constant a reflexive comparison folds to, or `None` if `op` is not a
/// comparison.
fn reflexive_value(op: &BinaryOp) -> Option<bool> {
    match op {
        BinaryOp::TwoEqual | BinaryOp::LessThanEqual | BinaryOp::GreaterThanEqual => Some(true),
        BinaryOp::TildeEqual | BinaryOp::LessThan | BinaryOp::GreaterThan => Some(false),
        _ => None,
    }
}

/// The ids this rule folds in `fun`, with the constant each becomes. Derived
/// from the function alone, so `apply` and `verify` agree by construction.
fn foldable(fun: &FunDef) -> Vec<(LocalId, bool)> {
    let mut number_constants: FxHashSet<LocalId> = FxHashSet::default();
    for block in fun.cfg.iter_blocks() {
        for (id, instr) in &block.instructions {
            if matches!(instr, Instruction::NumberConstant { .. }) {
                number_constants.insert(*id);
            }
        }
    }
    let mut out = Vec::new();
    for block in fun.cfg.iter_blocks() {
        for (id, instr) in &block.instructions {
            let Instruction::BinaryOp { left, op, right } = instr else { continue };
            if left != right || !number_constants.contains(left) {
                continue;
            }
            if let Some(value) = reflexive_value(op) {
                out.push((*id, value));
            }
        }
    }
    out
}

pub fn apply(program: &mut Program) -> Result<usize> {
    let mut changes = 0;
    for fun in program.functions.values_mut() {
        for (id, value) in foldable(fun) {
            for block in std::iter::once(&mut fun.cfg.entry).chain(fun.cfg.named.values_mut()) {
                for (candidate, instr) in block.instructions.iter_mut() {
                    if *candidate == id {
                        *instr = Instruction::BoolConstant { value };
                        changes += 1;
                    }
                }
            }
        }
    }
    Ok(changes)
}

/// Independent check: the after program must be the before program with
/// exactly the reflexive comparisons (re-derived from the before program)
/// replaced by their constants, and nothing else.
pub fn verify(before: &Program, after: &Program) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "fold_reflexive changed the set of functions",
    )?;
    for (name, before_fun) in &before.functions {
        let after_fun = after.functions.get(name).ok_or_else(|| {
            anyhow::anyhow!("fold_reflexive removed function {}", name.as_str())
        })?;
        let folded = foldable(before_fun);
        for key in super::blocks_sorted(&before_fun.cfg) {
            let block_name =
                key.as_ref().map(|l| l.as_str().to_string()).unwrap_or_else(|| "__entry".into());
            let before_block = super::get_block(&before_fun.cfg, &key).unwrap();
            let after_block = super::get_block(&after_fun.cfg, &key).ok_or_else(|| {
                anyhow::anyhow!(
                    "fold_reflexive removed block '{}' of {}",
                    block_name,
                    name.as_str()
                )
            })?;
            require(
                before_block.instructions.len() == after_block.instructions.len(),
                format!(
                    "fold_reflexive changed the length of '{}' in {}",
                    block_name,
                    name.as_str()
                ),
            )?;
            for ((before_id, before_instr), (after_id, after_instr)) in
                before_block.instructions.iter().zip(after_block.instructions.iter())
            {
                require(
                    before_id == after_id,
                    format!(
                        "fold_reflexive changed instruction id %{} of '{}' in {}",
                        usize::from(*before_id),
                        block_name,
                        name.as_str()
                    ),
                )?;
                let want = match folded.iter().find(|(id, _)| id == before_id) {
                    Some((_, value)) => &Instruction::BoolConstant { value: *value },
                    None => before_instr,
                };
                require(
                    want == after_instr,
                    format!(
                        "fold_reflexive changed %{} in '{}' of {}:\n  want {}\n  got  {}",
                        usize::from(*before_id),
                        block_name,
                        name.as_str(),
                        format_instruction(want),
                        format_instruction(after_instr)
                    ),
                )?;
            }
            require(
                before_block.terminator == after_block.terminator
                    && before_block.hint_normalize == after_block.hint_normalize,
                format!(
                    "fold_reflexive changed the terminator of '{}' in {}",
                    block_name,
                    name.as_str()
                ),
            )?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::super::program::Program;
    use super::*;
    use crate::ir::{Block, Cfg, GlobalId, Terminator};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn program(instructions: Vec<(LocalId, Instruction)>) -> Program {
        let entry = Block {
            instructions,
            terminator: (id(99), Terminator::Return { value: None }),
            hint_normalize: false,
        };
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            arg_ids: vec![],
            capture_ids: vec![],
            cfg: Cfg::new(entry, crate::ir::new_label_map()),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions }
    }

    fn num(value: i16) -> Instruction {
        Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(value) }
    }

    fn cmp(x: usize, op: BinaryOp) -> Instruction {
        Instruction::BinaryOp { left: id(x), op, right: id(x) }
    }

    fn entry(p: &Program) -> Block {
        p.functions.values().next().unwrap().cfg.entry.clone()
    }

    #[test]
    fn folds_a_reflexive_comparison_on_a_number_constant() {
        let mut p = program(vec![
            (id(1), num(0)),
            (id(2), cmp(1, BinaryOp::GreaterThan)),
            (id(3), cmp(1, BinaryOp::LessThanEqual)),
        ]);
        let before = p.clone();
        assert_eq!(apply(&mut p).unwrap(), 2);
        let entry = &p.functions.values().next().unwrap().cfg.entry;
        assert_eq!(entry.instructions[1].1, Instruction::BoolConstant { value: false });
        assert_eq!(entry.instructions[2].1, Instruction::BoolConstant { value: true });
        verify(&before, &p).unwrap();
    }

    #[test]
    fn leaves_a_comparison_of_two_different_locals() {
        let mut p = program(vec![
            (id(1), num(0)),
            (id(2), num(0)),
            (id(3), Instruction::BinaryOp { left: id(1), op: BinaryOp::GreaterThan, right: id(2) }),
        ]);
        let before = entry(&p);
        assert_eq!(apply(&mut p).unwrap(), 0);
        assert_eq!(entry(&p), before);
    }

    #[test]
    fn leaves_a_reflexive_comparison_on_a_non_constant() {
        // %1 is an argument-like unknown: not defined by NumberConstant.
        let mut p = program(vec![
            (id(1), Instruction::Alloc),
            (id(2), cmp(1, BinaryOp::GreaterThan)),
        ]);
        let before = entry(&p);
        assert_eq!(apply(&mut p).unwrap(), 0);
        assert_eq!(entry(&p), before);
    }

    #[test]
    fn leaves_reflexive_arithmetic() {
        let mut p = program(vec![(id(1), num(2)), (id(2), cmp(1, BinaryOp::Plus))]);
        let before = entry(&p);
        assert_eq!(apply(&mut p).unwrap(), 0);
        assert_eq!(entry(&p), before);
    }

    #[test]
    fn is_idempotent() {
        let mut p = program(vec![(id(1), num(0)), (id(2), cmp(1, BinaryOp::LessThan))]);
        assert_eq!(apply(&mut p).unwrap(), 1);
        let once = entry(&p);
        assert_eq!(apply(&mut p).unwrap(), 0);
        assert_eq!(entry(&p), once);
    }

    #[test]
    fn verify_rejects_a_wrong_constant() {
        let mut p = program(vec![(id(1), num(0)), (id(2), cmp(1, BinaryOp::GreaterThan))]);
        let before = p.clone();
        apply(&mut p).unwrap();
        // Flip the folded constant.
        p.functions.values_mut().next().unwrap().cfg.entry.instructions[1].1 =
            Instruction::BoolConstant { value: true };
        assert!(verify(&before, &p).is_err());
    }

    #[test]
    fn verify_rejects_an_unrelated_change() {
        let mut p = program(vec![(id(1), num(0)), (id(2), cmp(1, BinaryOp::GreaterThan))]);
        let before = p.clone();
        apply(&mut p).unwrap();
        p.functions.values_mut().next().unwrap().cfg.entry.instructions[0].1 = num(1);
        assert!(verify(&before, &p).is_err());
    }
}
