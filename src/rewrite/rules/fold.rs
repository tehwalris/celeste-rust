//! `fold` - local simplifications that are true regardless of context.
//!
//! Bulk rule: no location, deterministic, idempotent.
//!
//! Currently does two things:
//!
//!   * a phi all of whose branches carry the same value becomes a copy of that
//!     value (the id is substituted away, since the IR has no copy instruction)
//!   * a conditional branch whose condition is a `BoolConstant` becomes an
//!     unconditional branch
//!
//! Deliberately *not* included yet: constant folding of arithmetic. That would
//! have to evaluate PICO-8 fixed-point semantics, and the only trustworthy way
//! to do that is to call the same `op.rs` code the interpreter uses - which
//! makes the verifier depend on the applier's notion of truth. It is worth
//! doing, but it needs its own exhaustive differential test against `op.rs`
//! first, so it is left for later rather than bolted on here.

use anyhow::Result;
use rustc_hash::FxHashMap;

use crate::ir::{Block, Cfg, FunDef, Instruction, LocalId, Terminator};

use super::super::program::Program;
use super::require;

pub fn apply(program: &mut Program) -> Result<usize> {
    let mut changes = 0;
    for fun in program.functions.values_mut() {
        changes += run_on_function(fun);
    }
    Ok(changes)
}

fn run_on_function(fun: &mut FunDef) -> usize {
    let mut changes = 0;

    // Conditional branch on a constant.
    let constants = bool_constants(&fun.cfg);
    for block in blocks_mut(&mut fun.cfg) {
        let Terminator::ConditionalBranch { condition, true_target, false_target } =
            block.terminator_kind()
        else {
            continue;
        };
        let Some(value) = constants.get(condition) else { continue };
        let target = if *value { true_target } else { false_target };
        block.terminator.1 = Terminator::UnconditionalBranch { target: target.clone() };
        changes += 1;
    }

    // Phis with a single distinct incoming value.
    loop {
        let mut substitution: FxHashMap<LocalId, LocalId> = FxHashMap::default();
        for block in fun.cfg.iter_blocks() {
            for (id, instr) in &block.instructions {
                let Instruction::Phi { branches } = instr else { continue };
                if branches.is_empty() {
                    continue;
                }
                let first = branches[0].1;
                // A phi that refers to itself does not block the collapse: the
                // self-edge can only carry the value the phi already has.
                if branches.iter().all(|(_, v)| *v == first || *v == *id) && first != *id {
                    substitution.insert(*id, first);
                }
            }
        }
        if substitution.is_empty() {
            break;
        }
        changes += substitution.len();
        let resolve = |mut id: LocalId| {
            let mut guard = 0;
            while let Some(next) = substitution.get(&id) {
                id = *next;
                guard += 1;
                if guard > substitution.len() + 1 {
                    break;
                }
            }
            id
        };
        for block in blocks_mut(&mut fun.cfg) {
            block.instructions.retain(|(id, _)| !substitution.contains_key(id));
            for (_, instr) in block.instructions.iter_mut() {
                *instr = instr.map_local_ids(resolve);
            }
            block.terminator.1 = block.terminator.1.map_local_ids(resolve);
        }
    }

    changes
}

fn blocks_mut(cfg: &mut Cfg) -> impl Iterator<Item = &mut Block> {
    std::iter::once(&mut cfg.entry).chain(cfg.named.values_mut())
}

fn bool_constants(cfg: &Cfg) -> FxHashMap<LocalId, bool> {
    let mut out = FxHashMap::default();
    for block in cfg.iter_blocks() {
        for (id, instr) in &block.instructions {
            if let Instruction::BoolConstant { value } = instr {
                out.insert(*id, *value);
            }
        }
    }
    out
}

/// Independent check.
///
/// For each terminator that changed from conditional to unconditional, confirm
/// the condition really was a constant in the *before* program and that the
/// surviving target is the one that constant selects. For each phi that
/// disappeared, confirm all its branches carried the same value.
pub fn verify(before: &Program, after: &Program) -> Result<()> {
    for (name, before_fun) in &before.functions {
        let after_fun = after
            .functions
            .get(name)
            .ok_or_else(|| anyhow::anyhow!("fold removed function {}", name.as_str()))?;
        let constants = bool_constants(&before_fun.cfg);

        for key in super::blocks_sorted(&before_fun.cfg) {
            let Some(before_block) = super::get_block(&before_fun.cfg, &key) else { continue };
            let Some(after_block) = super::get_block(&after_fun.cfg, &key) else { continue };

            match (before_block.terminator_kind(), after_block.terminator_kind()) {
                (
                    Terminator::ConditionalBranch { condition, true_target, false_target },
                    Terminator::UnconditionalBranch { target },
                ) => {
                    let value = constants.get(condition).ok_or_else(|| {
                        anyhow::anyhow!(
                            "{}: fold made a branch unconditional but %{} is not a constant",
                            name.as_str(),
                            usize::from(*condition)
                        )
                    })?;
                    let expected = if *value { true_target } else { false_target };
                    require(
                        target == expected,
                        format!(
                            "{}: fold picked branch '{}' but the constant selects '{}'",
                            name.as_str(),
                            target.as_str(),
                            expected.as_str()
                        ),
                    )?;
                }
                (b, a) => {
                    require(
                        std::mem::discriminant(b) == std::mem::discriminant(a),
                        format!("{}: fold changed a terminator in an unexpected way", name.as_str()),
                    )?;
                }
            }

            let kept: Vec<LocalId> =
                after_block.instructions.iter().map(|(id, _)| *id).collect();
            for (id, instr) in &before_block.instructions {
                if kept.contains(id) {
                    continue;
                }
                let Instruction::Phi { branches } = instr else {
                    return Err(anyhow::anyhow!(
                        "{}: fold removed non-phi instruction %{}",
                        name.as_str(),
                        usize::from(*id)
                    ));
                };
                let distinct: Vec<LocalId> = {
                    let mut v: Vec<LocalId> =
                        branches.iter().map(|(_, v)| *v).filter(|v| v != id).collect();
                    v.sort_by_key(|i| usize::from(*i));
                    v.dedup();
                    v
                };
                require(
                    distinct.len() <= 1,
                    format!(
                        "{}: fold removed phi %{} which had {} distinct incoming values",
                        name.as_str(),
                        usize::from(*id),
                        distinct.len()
                    ),
                )?;
            }
        }
    }
    Ok(())
}
