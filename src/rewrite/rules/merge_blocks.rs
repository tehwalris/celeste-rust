//! `merge_blocks` - fold a block into its unique predecessor when that
//! predecessor's only successor is it.
//!
//! Bulk rule: no location, deterministic, idempotent.
//!
//! Soundness argument: if `P` ends in `br S` and `S`'s only predecessor is `P`,
//! then control reaches `S` exactly when it reaches the end of `P`, so
//! concatenating them changes nothing. Because `S` has a single predecessor,
//! any phi in `S` has exactly one branch and collapses to a copy of that value.
//!
//! Two details the previous implementation got wrong and that the verifier here
//! checks for:
//!
//!   * `hint_normalize` must be OR-ed, not taken from either side. It marks a
//!     state-merge point for the abstract interpreter, and losing one changes
//!     how much the state set fragments.
//!   * Phis elsewhere that named `S` as a predecessor must be renamed to `P`.

use anyhow::Result;
use rustc_hash::FxHashMap;

use crate::ir::{Cfg, FunDef, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::{blocks_sorted, get_block, label_of, predecessors, rename_phi_label, require};

/// Display helper: the entry block has no label of its own.
fn key_str(key: &Option<Label>) -> &str {
    match key {
        None => "__entry",
        Some(l) => l.as_str(),
    }
}

pub fn apply(program: &mut Program) -> Result<usize> {
    let mut changes = 0;
    for fun in program.functions.values_mut() {
        changes += run_on_function(fun);
    }
    Ok(changes)
}

fn run_on_function(fun: &mut FunDef) -> usize {
    let mut changes = 0;
    loop {
        let Some((pred_key, succ_label)) = find_mergeable(&fun.cfg) else {
            break;
        };
        merge(&mut fun.cfg, &pred_key, &succ_label);
        changes += 1;
    }
    changes
}

/// Finds a (predecessor, successor) pair that can be merged. Deterministic:
/// scans blocks in sorted order and takes the first candidate.
fn find_mergeable(cfg: &Cfg) -> Option<(Option<Label>, Label)> {
    let preds = predecessors(cfg);
    for pred_key in blocks_sorted(cfg) {
        let block = get_block(cfg, &pred_key)?;
        let Terminator::UnconditionalBranch { target } = block.terminator_kind() else {
            continue;
        };
        // Never merge the entry into something, and never merge a block into
        // itself (a self-loop has itself as a predecessor).
        if Some(target.clone()) == pred_key {
            continue;
        }
        let succ_preds = preds.get(&Some(target.clone()))?;
        if succ_preds.len() != 1 {
            continue;
        }
        return Some((pred_key, target.clone()));
    }
    None
}

fn merge(cfg: &mut Cfg, pred_key: &Option<Label>, succ_label: &Label) {
    let succ = cfg
        .named
        .remove(succ_label)
        .expect("successor exists, just looked it up");

    // A phi in the successor has exactly one branch (single predecessor), so it
    // becomes a copy. Record the substitution rather than emitting a copy
    // instruction: the IR has no copy, and a phi's id must keep working.
    let mut substitution: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    let mut kept_instructions = Vec::new();
    for (id, instr) in succ.instructions {
        match instr {
            Instruction::Phi { ref branches } if branches.len() == 1 => {
                substitution.insert(id, branches[0].1);
            }
            other => kept_instructions.push((id, other)),
        }
    }

    let pred = match pred_key {
        None => &mut cfg.entry,
        Some(l) => cfg.named.get_mut(l).expect("predecessor exists"),
    };
    pred.instructions.extend(kept_instructions);
    pred.terminator = succ.terminator;
    pred.hint_normalize = pred.hint_normalize || succ.hint_normalize;

    // Apply the phi substitution everywhere, transitively.
    if !substitution.is_empty() {
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
        let apply_to = |block: &mut crate::ir::Block| {
            for (_, instr) in block.instructions.iter_mut() {
                *instr = instr.map_local_ids(resolve);
            }
            block.terminator.1 = block.terminator.1.map_local_ids(resolve);
        };
        apply_to(&mut cfg.entry);
        for block in cfg.named.values_mut() {
            apply_to(block);
        }
    }

    // Anything that named the absorbed block in a phi must now name the merged
    // one.
    let pred_label = label_of(pred_key);
    rename_phi_label(cfg, succ_label, &pred_label);
}

/// Independent check.
///
/// Merging is transitive: after `S` is folded into `P`, `P` may itself be folded
/// into *its* predecessor. So a removed block's survivor is found by walking up
/// the single-predecessor chain in the *before* program until reaching a block
/// that still exists afterwards. This is derived from the before/after programs
/// only - it does not reuse anything from `apply`.
pub fn verify(before: &Program, after: &Program) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "merge_blocks must not add or remove functions",
    )?;

    for (name, before_fun) in &before.functions {
        let after_fun = after
            .functions
            .get(name)
            .ok_or_else(|| anyhow::anyhow!("merge_blocks removed function {}", name.as_str()))?;
        let preds = predecessors(&before_fun.cfg);

        let survives = |key: &Option<Label>| match key {
            None => true, // the entry block is never removed
            Some(l) => after_fun.cfg.named.contains_key(l),
        };

        for label in before_fun.cfg.named.keys() {
            if after_fun.cfg.named.contains_key(label) {
                continue;
            }

            // Walk up to the block this one was folded into, checking at each
            // step that the merge was legal.
            let mut chain: Vec<Option<Label>> = vec![Some(label.clone())];
            let mut cursor: Label = label.clone();
            let survivor: Option<Label> = loop {
                let block_preds = preds.get(&Some(cursor.clone())).ok_or_else(|| {
                    anyhow::anyhow!(
                        "{}: no predecessor entry for '{}'",
                        name.as_str(),
                        cursor.as_str()
                    )
                })?;
                require(
                    block_preds.len() == 1,
                    format!(
                        "{}: merge_blocks removed '{}', which has {} predecessors",
                        name.as_str(),
                        cursor.as_str(),
                        block_preds.len()
                    ),
                )?;
                let pred_key = block_preds[0].clone();
                let pred_block = get_block(&before_fun.cfg, &pred_key).ok_or_else(|| {
                    anyhow::anyhow!("{}: predecessor block missing", name.as_str())
                })?;
                require(
                    matches!(
                        pred_block.terminator_kind(),
                        Terminator::UnconditionalBranch { target } if *target == cursor
                    ),
                    format!(
                        "{}: '{}' was merged but its predecessor '{}' does not branch \
                         unconditionally to it",
                        name.as_str(),
                        cursor.as_str(),
                        key_str(&pred_key)
                    ),
                )?;

                if survives(&pred_key) {
                    break pred_key;
                }
                require(
                    chain.len() <= before_fun.cfg.named.len() + 1,
                    format!("{}: merge chain does not terminate", name.as_str()),
                )?;
                chain.push(pred_key.clone());
                cursor = pred_key.expect("the entry block always survives");
            };

            // hint_normalize marks a state-merge point for the abstract
            // interpreter. Losing one changes how much the state set fragments,
            // so it must be OR-ed into the surviving block.
            let chain_wants_normalize = chain.iter().any(|k| {
                get_block(&before_fun.cfg, k).is_some_and(|b| b.hint_normalize)
            }) || get_block(&before_fun.cfg, &survivor).is_some_and(|b| b.hint_normalize);
            if chain_wants_normalize {
                let merged = get_block(&after_fun.cfg, &survivor).ok_or_else(|| {
                    anyhow::anyhow!(
                        "{}: survivor of merged block '{}' is gone",
                        name.as_str(),
                        label.as_str()
                    )
                })?;
                require(
                    merged.hint_normalize,
                    format!(
                        "{}: merging '{}' dropped hint_normalize",
                        name.as_str(),
                        label.as_str()
                    ),
                )?;
            }
        }

        // No phi anywhere may still name a block that no longer exists.
        let live_labels: Vec<String> = after_fun
            .cfg
            .named
            .keys()
            .map(|l| l.as_str().to_string())
            .chain(std::iter::once("__entry".to_string()))
            .collect();
        for block in after_fun.cfg.iter_blocks() {
            for (id, instr) in &block.instructions {
                let Instruction::Phi { branches } = instr else { continue };
                for (label, _) in branches {
                    require(
                        live_labels.iter().any(|l| l == label.as_str()),
                        format!(
                            "{}: phi %{} names '{}', which no longer exists",
                            name.as_str(),
                            usize::from(*id),
                            label.as_str()
                        ),
                    )?;
                }
            }
        }
    }
    Ok(())
}
