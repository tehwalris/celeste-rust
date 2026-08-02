//! The rewrite rules.
//!
//! Every rule is either **pointed** (it names a specific location in the
//! program) or **canonical bulk** (it takes no location, its effect is fully
//! determined by the program, and it is idempotent). Bulk rules keep recipes
//! short without reintroducing "an optimizer": their result is deterministic
//! and their side conditions are trivially checkable.
//!
//! Each rule comes in two halves that must be written independently:
//!
//!   * `apply`   - performs the transformation
//!   * `verify`  - checks, from the before and after programs alone, that the
//!                 transformation was legitimate
//!
//! `verify` must never call into `apply`'s helpers. A shared bug would cancel
//! out, and the whole point is that the applier can be as sloppy as we like as
//! long as the verifier is right.

use anyhow::{anyhow, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{Block, Cfg, FunDef, Instruction, Label, LocalId, Terminator};

use super::program::Program;

pub mod allocate_slots;
pub mod if_convert;
pub mod fold;
pub mod cse;
pub mod dce;
pub mod demote_create;
pub mod merge_blocks;
pub mod pin_builtin;
pub mod inline;
pub mod promote_capture;
pub mod promote_cell;

/// Applies `f` to every function in the program, returning how many changes
/// were made in total.
pub fn map_functions(program: &mut Program, mut f: impl FnMut(&mut FunDef) -> usize) -> usize {
    let mut total = 0;
    for fun in program.functions.values_mut() {
        total += f(fun);
    }
    total
}

/// Every block of a CFG, keyed by label (`None` = entry), in a deterministic
/// order. `Cfg::named` is a hash map, so anything order-sensitive must go
/// through here.
pub fn blocks_sorted(cfg: &Cfg) -> Vec<Option<Label>> {
    let mut named: Vec<&Label> = cfg.named.keys().collect();
    named.sort_by_key(|l| l.as_str().to_string());
    let mut out: Vec<Option<Label>> = vec![None];
    out.extend(named.into_iter().map(|l| Some(l.clone())));
    out
}

pub fn get_block<'a>(cfg: &'a Cfg, key: &Option<Label>) -> Option<&'a Block> {
    match key {
        None => Some(&cfg.entry),
        Some(l) => cfg.named.get(l),
    }
}

pub fn get_block_mut<'a>(cfg: &'a mut Cfg, key: &Option<Label>) -> Option<&'a mut Block> {
    match key {
        None => Some(&mut cfg.entry),
        Some(l) => cfg.named.get_mut(l),
    }
}

/// Blocks reachable from the entry.
pub fn reachable_blocks(cfg: &Cfg) -> FxHashSet<Option<Label>> {
    let mut seen: FxHashSet<Option<Label>> = FxHashSet::default();
    let mut stack: Vec<Option<Label>> = vec![None];
    while let Some(key) = stack.pop() {
        if !seen.insert(key.clone()) {
            continue;
        }
        let Some(block) = get_block(cfg, &key) else { continue };
        for label in block.terminator_kind().successor_labels() {
            stack.push(Some(label.clone()));
        }
    }
    seen
}

/// Predecessor labels of every block.
pub fn predecessors(cfg: &Cfg) -> FxHashMap<Option<Label>, Vec<Option<Label>>> {
    let mut preds: FxHashMap<Option<Label>, Vec<Option<Label>>> = FxHashMap::default();
    for key in blocks_sorted(cfg) {
        preds.entry(key).or_default();
    }
    for key in blocks_sorted(cfg) {
        let Some(block) = get_block(cfg, &key) else { continue };
        for label in block.terminator_kind().successor_labels() {
            preds.entry(Some(label.clone())).or_default().push(key.clone());
        }
    }
    preds
}

/// Every local the function reads anywhere, including from terminators.
pub fn all_used_locals(cfg: &Cfg) -> FxHashSet<LocalId> {
    let mut used = FxHashSet::default();
    for block in cfg.iter_blocks() {
        for (_, instr) in &block.instructions {
            used.extend(instr.get_used_locals());
        }
        used.extend(block.terminator_kind().get_used_locals());
    }
    used
}

/// Rewrites every phi in `cfg` so that branches naming `from` name `to`
/// instead. Used when a block is merged away.
pub fn rename_phi_label(cfg: &mut Cfg, from: &Label, to: &Label) {
    let rename = |block: &mut Block| {
        for (_, instr) in block.instructions.iter_mut() {
            if let Instruction::Phi { branches } = instr {
                for (label, _) in branches.iter_mut() {
                    if label == from {
                        *label = to.clone();
                    }
                }
            }
        }
    };
    rename(&mut cfg.entry);
    for block in cfg.named.values_mut() {
        rename(block);
    }
}

/// The label a phi uses to refer to the entry block.
pub fn entry_label() -> Label {
    Label::from("__entry".to_string())
}

pub fn label_of(key: &Option<Label>) -> Label {
    match key {
        None => entry_label(),
        Some(l) => l.clone(),
    }
}

/// Fails if a rule's precondition does not hold. Rules should be loud, not
/// quietly skip: a recipe that no longer matches the program is a bug in the
/// recipe, and silently doing nothing would hide it.
pub fn require(condition: bool, message: impl Into<String>) -> Result<()> {
    if condition {
        Ok(())
    } else {
        Err(anyhow!(message.into()))
    }
}

/// Convenience for tests and for rules that need a fresh id.
pub struct LocalIdAllocator {
    next: usize,
}

impl LocalIdAllocator {
    pub fn for_function(fun: &FunDef) -> Self {
        let mut max = 0;
        for id in fun.arg_ids.iter().flatten().chain(fun.capture_ids.iter()) {
            max = max.max(usize::from(*id) + 1);
        }
        for block in fun.cfg.iter_blocks() {
            for (id, _) in &block.instructions {
                max = max.max(usize::from(*id) + 1);
            }
            max = max.max(usize::from(block.terminator_id()) + 1);
        }
        Self { next: max }
    }

    pub fn fresh(&mut self) -> LocalId {
        let id = LocalId::from(self.next);
        self.next += 1;
        id
    }
}

/// Shorthand used by several rules: does this terminator jump unconditionally
/// to exactly one place?
pub fn unconditional_target(term: &Terminator) -> Option<&Label> {
    match term {
        Terminator::UnconditionalBranch { target } => Some(target),
        _ => None,
    }
}
