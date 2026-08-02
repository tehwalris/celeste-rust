//! `dce` - remove instructions whose result is never read and which have no
//! side effects, and blocks that cannot be reached from the entry.
//!
//! Bulk rule: no location, deterministic, idempotent.
//!
//! Soundness argument: an instruction with no side effects contributes nothing
//! but its result; if nothing reads its result, deleting it cannot change any
//! observable behaviour. An unreachable block never executes. The only subtlety
//! is what counts as a side effect, and that lives in
//! `Instruction::has_side_effects` - note in particular that `GetField` and
//! `GetIndex` with `create_if_missing` *do* mutate the heap.
//!
//! Removing a block also requires fixing phis in surviving blocks that named it
//! as a predecessor. The previous optimizer's DCE did not do this.

use anyhow::Result;
use rustc_hash::FxHashSet;

use crate::ir::{Block, Cfg, FunDef, Instruction, Label, LocalId};

use super::super::program::Program;
use super::{label_of, reachable_blocks, require};

pub fn apply(program: &mut Program) -> Result<usize> {
    let mut changes = 0;
    for fun in program.functions.values_mut() {
        changes += run_on_function(fun);
    }
    Ok(changes)
}

fn run_on_function(fun: &mut FunDef) -> usize {
    let mut changes = 0;

    // 1. Drop unreachable blocks.
    let reachable = reachable_blocks(&fun.cfg);
    let unreachable: Vec<Label> = fun
        .cfg
        .named
        .keys()
        .filter(|l| !reachable.contains(&Some((*l).clone())))
        .cloned()
        .collect();
    for label in &unreachable {
        fun.cfg.named.remove(label);
        changes += 1;
    }
    if !unreachable.is_empty() {
        let removed: FxHashSet<String> =
            unreachable.iter().map(|l| l.as_str().to_string()).collect();
        drop_phi_branches(&mut fun.cfg, &removed);
    }

    // 2. Drop dead instructions, to a fixpoint. One pass is not enough: killing
    //    a load can make the instruction that produced its pointer dead too.
    loop {
        let used = live_locals(&fun.cfg);
        let mut removed_this_pass = 0;
        for block in blocks_mut(&mut fun.cfg) {
            let before = block.instructions.len();
            block.instructions.retain(|(id, instr)| {
                instr.has_side_effects() || used.contains(id)
            });
            removed_this_pass += before - block.instructions.len();
        }
        if removed_this_pass == 0 {
            break;
        }
        changes += removed_this_pass;
    }

    changes
}

fn blocks_mut(cfg: &mut Cfg) -> impl Iterator<Item = &mut Block> {
    std::iter::once(&mut cfg.entry).chain(cfg.named.values_mut())
}

/// Locals read by any surviving instruction or terminator.
fn live_locals(cfg: &Cfg) -> FxHashSet<LocalId> {
    let mut used = FxHashSet::default();
    for block in cfg.iter_blocks() {
        for (_, instr) in &block.instructions {
            if instr.has_side_effects() {
                used.extend(instr.get_used_locals());
            }
        }
        used.extend(block.terminator_kind().get_used_locals());
    }
    // A pure instruction is live if something live reads it. Iterate to a
    // fixpoint over the pure ones.
    loop {
        let mut added = false;
        for block in cfg.iter_blocks() {
            for (id, instr) in &block.instructions {
                if used.contains(id) {
                    for operand in instr.get_used_locals() {
                        if used.insert(operand) {
                            added = true;
                        }
                    }
                }
            }
        }
        if !added {
            break;
        }
    }
    used
}

fn drop_phi_branches(cfg: &mut Cfg, removed_labels: &FxHashSet<String>) {
    let fix = |block: &mut Block| {
        for (_, instr) in block.instructions.iter_mut() {
            if let Instruction::Phi { branches } = instr {
                branches.retain(|(label, _)| !removed_labels.contains(label.as_str()));
            }
        }
    };
    fix(&mut cfg.entry);
    for block in cfg.named.values_mut() {
        fix(block);
    }
}

/// Independent check that the transformation was legitimate.
///
/// Deliberately does not reuse anything from `apply`: it only inspects the two
/// programs. Every instruction that disappeared must have been side-effect free
/// and unread *in the after program*, and every block that disappeared must be
/// unreachable in the before program.
pub fn verify(before: &Program, after: &Program) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "dce must not add or remove functions",
    )?;

    for (name, before_fun) in &before.functions {
        let after_fun = after
            .functions
            .get(name)
            .ok_or_else(|| anyhow::anyhow!("dce removed function {}", name.as_str()))?;

        let reachable_before = reachable_blocks(&before_fun.cfg);
        for label in before_fun.cfg.named.keys() {
            if after_fun.cfg.named.contains_key(label) {
                continue;
            }
            require(
                !reachable_before.contains(&Some(label.clone())),
                format!(
                    "{}: dce removed reachable block '{}'",
                    name.as_str(),
                    label.as_str()
                ),
            )?;
        }

        let surviving: FxHashSet<LocalId> = after_fun
            .cfg
            .iter_blocks()
            .flat_map(|b| b.instructions.iter().map(|(id, _)| *id))
            .collect();
        let read_after = super::all_used_locals(&after_fun.cfg);

        // Anything dropped from a *surviving* block must have been dead.
        for (key, before_block) in blocks_with_key(&before_fun.cfg) {
            let Some(after_block) = super::get_block(&after_fun.cfg, &key) else {
                continue; // whole block removed; checked above
            };
            let kept: FxHashSet<LocalId> =
                after_block.instructions.iter().map(|(id, _)| *id).collect();
            for (id, instr) in &before_block.instructions {
                if kept.contains(id) {
                    continue;
                }
                require(
                    !instr.has_side_effects(),
                    format!(
                        "{}: dce removed side-effecting instruction {} from '{}'",
                        name.as_str(),
                        usize::from(*id),
                        label_of(&key).as_str()
                    ),
                )?;
                require(
                    !read_after.contains(id) && !surviving.contains(id),
                    format!(
                        "{}: dce removed {} but something still reads it",
                        name.as_str(),
                        usize::from(*id)
                    ),
                )?;
            }
            require(
                after_block.terminator_id() == before_block.terminator_id(),
                format!("{}: dce changed a terminator", name.as_str()),
            )?;
        }
    }
    Ok(())
}

fn blocks_with_key(cfg: &Cfg) -> Vec<(Option<Label>, &Block)> {
    let mut out: Vec<(Option<Label>, &Block)> = vec![(None, &cfg.entry)];
    let mut named: Vec<(&Label, &Block)> = cfg.named.iter().collect();
    named.sort_by_key(|(l, _)| l.as_str().to_string());
    out.extend(named.into_iter().map(|(l, b)| (Some(l.clone()), b)));
    out
}
