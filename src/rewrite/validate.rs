//! Structural validation, run after every rewrite.
//!
//! The previous optimizer's validator checked five things and **did not check
//! dominance**, which is exactly the class of bug that mem2reg and inlining
//! produce: a value defined on one arm of a branch and used after the join
//! passes a "is it defined anywhere in the CFG" test and then panics at runtime
//! with "LocalId should be set before get". So dominance is the point of this
//! module.
//!
//! It also checks the invariants the *runtime* relies on but the old validator
//! did not state: phis must be a strict block prefix, and a phi must have
//! exactly one branch per predecessor (`flow.rs` uses `exactly_one()` and
//! panics otherwise).

use std::collections::BTreeSet;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{Block, Cfg, FunDef, Instruction, Label, LocalId, Terminator};

use super::print::local_name;
use super::program::Program;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ValidationError {
    pub function: String,
    pub message: String,
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.function, self.message)
    }
}

/// Identifies a block. `None` is the entry block.
type BlockKey = Option<Label>;

fn block_label(key: &BlockKey) -> String {
    match key {
        None => "__entry".to_string(),
        Some(l) => l.as_str().to_string(),
    }
}

fn all_blocks(cfg: &Cfg) -> Vec<(BlockKey, &Block)> {
    let mut out: Vec<(BlockKey, &Block)> = vec![(None, &cfg.entry)];
    let mut named: Vec<(&Label, &Block)> = cfg.named.iter().collect();
    named.sort_by_key(|(l, _)| l.as_str().to_string());
    out.extend(named.into_iter().map(|(l, b)| (Some(l.clone()), b)));
    out
}

fn successors(block: &Block) -> Vec<BlockKey> {
    match block.terminator_kind() {
        Terminator::Return { .. } => vec![],
        Terminator::UnconditionalBranch { target } => vec![Some(target.clone())],
        Terminator::ConditionalBranch { true_target, false_target, .. } => {
            vec![Some(true_target.clone()), Some(false_target.clone())]
        }
    }
}

pub fn validate_function(fun: &FunDef) -> Vec<ValidationError> {
    let mut errors = Vec::new();
    let name = fun.name.as_str().to_string();
    macro_rules! err {
        ($($arg:tt)*) => {
            errors.push(ValidationError {
                function: name.clone(),
                message: format!($($arg)*),
            })
        };
    }

    let cfg = &fun.cfg;
    let blocks = all_blocks(cfg);
    let block_keys: FxHashSet<BlockKey> = blocks.iter().map(|(k, _)| k.clone()).collect();

    // --- single definition, and phis form a prefix ---
    let mut defined: FxHashMap<LocalId, BlockKey> = FxHashMap::default();
    for id in fun.arg_ids.iter().flatten().chain(fun.capture_ids.iter()) {
        defined.insert(*id, None);
    }
    for (key, block) in &blocks {
        let mut seen_non_phi = false;
        for (id, instr) in &block.instructions {
            if matches!(instr, Instruction::Phi { .. }) {
                if seen_non_phi {
                    err!(
                        "block '{}': phi {} appears after a non-phi instruction",
                        block_label(key),
                        local_name(*id)
                    );
                }
            } else {
                seen_non_phi = true;
            }
            if defined.insert(*id, key.clone()).is_some() {
                err!("{} is defined more than once", local_name(*id));
            }
        }
        if defined.insert(block.terminator_id(), key.clone()).is_some() {
            err!(
                "{} is defined more than once (terminator of '{}')",
                local_name(block.terminator_id()),
                block_label(key)
            );
        }
    }

    // --- branch targets exist ---
    for (key, block) in &blocks {
        for succ in successors(block) {
            if !block_keys.contains(&succ) {
                err!(
                    "block '{}' branches to '{}', which does not exist",
                    block_label(key),
                    block_label(&succ)
                );
            }
        }
    }
    if !errors.is_empty() {
        // Later checks assume the CFG is at least well-formed enough to walk.
        return errors;
    }

    // --- predecessors ---
    let mut preds: FxHashMap<BlockKey, Vec<BlockKey>> = FxHashMap::default();
    for (key, _) in &blocks {
        preds.entry(key.clone()).or_default();
    }
    for (key, block) in &blocks {
        for succ in successors(block) {
            preds.entry(succ).or_default().push(key.clone());
        }
    }

    // --- phi branches match predecessors exactly ---
    for (key, block) in &blocks {
        let pred_labels: BTreeSet<String> = preds[key].iter().map(block_label).collect();
        for (id, instr) in &block.instructions {
            let Instruction::Phi { branches } = instr else { continue };
            let mut seen: BTreeSet<String> = BTreeSet::new();
            for (label, _) in branches {
                let l = label.as_str().to_string();
                if !seen.insert(l.clone()) {
                    err!(
                        "phi {} in '{}' has two branches for predecessor '{}'",
                        local_name(*id),
                        block_label(key),
                        l
                    );
                }
                if !pred_labels.contains(&l) {
                    err!(
                        "phi {} in '{}' names '{}', which is not a predecessor",
                        local_name(*id),
                        block_label(key),
                        l
                    );
                }
            }
            for missing in pred_labels.difference(&seen) {
                err!(
                    "phi {} in '{}' has no branch for predecessor '{}'",
                    local_name(*id),
                    block_label(key),
                    missing
                );
            }
        }
    }

    // --- reachability, then dominance ---
    let reachable = compute_reachable(&blocks);
    let dom = dominators(&blocks, &preds, &reachable);
    let defs = definitions_by_block(&blocks);
    let external: FxHashSet<LocalId> = fun
        .arg_ids
        .iter()
        .flatten()
        .chain(fun.capture_ids.iter())
        .copied()
        .collect();

    for (key, block) in &blocks {
        if !reachable.contains(key) {
            continue;
        }
        let Some(dominators_of_key) = dom.get(key) else { continue };

        // Available on entry to this block: everything defined in a *strict*
        // dominator, plus arguments and captures.
        let mut available = external.clone();
        for d in dominators_of_key {
            if d == key {
                continue;
            }
            if let Some(ids) = defs.get(d) {
                available.extend(ids.iter().copied());
            }
        }

        let mut seen_non_phi = false;
        for (id, instr) in &block.instructions {
            if let Instruction::Phi { branches } = instr {
                if seen_non_phi {
                    continue; // already reported above
                }
                // A phi operand is evaluated on the incoming edge, so it must be
                // available at the end of that predecessor, not here.
                for (label, value) in branches {
                    let pred_key: BlockKey = if label.as_str() == "__entry" {
                        None
                    } else {
                        Some(label.clone())
                    };
                    if external.contains(value) {
                        continue;
                    }
                    let ok = dom
                        .get(&pred_key)
                        .map(|ds| ds.iter().any(|d| defs.get(d).is_some_and(|s| s.contains(value))))
                        .unwrap_or(false);
                    if !ok {
                        err!(
                            "phi {} in '{}': {} from '{}' is not available there",
                            local_name(*id),
                            block_label(key),
                            local_name(*value),
                            label.as_str()
                        );
                    }
                }
            } else {
                seen_non_phi = true;
                for used in instr.get_used_locals() {
                    if !available.contains(&used) {
                        err!(
                            "{} uses {}, which does not dominate it (block '{}')",
                            local_name(*id),
                            local_name(used),
                            block_label(key)
                        );
                    }
                }
            }
            available.insert(*id);
        }
        for used in block.terminator_kind().get_used_locals() {
            if !available.contains(&used) {
                err!(
                    "terminator of '{}' uses {}, which does not dominate it",
                    block_label(key),
                    local_name(used)
                );
            }
        }
    }

    errors
}

/// Locals defined by each block (instructions and terminator).
fn definitions_by_block(
    blocks: &[(BlockKey, &Block)],
) -> FxHashMap<BlockKey, FxHashSet<LocalId>> {
    blocks
        .iter()
        .map(|(key, block)| {
            let mut ids: FxHashSet<LocalId> =
                block.instructions.iter().map(|(id, _)| *id).collect();
            ids.insert(block.terminator_id());
            (key.clone(), ids)
        })
        .collect()
}

fn compute_reachable(blocks: &[(BlockKey, &Block)]) -> FxHashSet<BlockKey> {
    let by_key: FxHashMap<&BlockKey, &Block> = blocks.iter().map(|(k, b)| (k, *b)).collect();
    let mut seen: FxHashSet<BlockKey> = FxHashSet::default();
    let mut stack = vec![None];
    while let Some(key) = stack.pop() {
        if !seen.insert(key.clone()) {
            continue;
        }
        if let Some(block) = by_key.get(&key) {
            for succ in successors(block) {
                stack.push(succ);
            }
        }
    }
    seen
}

/// Dominator sets by the textbook fixpoint:
///
///     Dom(entry) = {entry}
///     Dom(n)     = {n} union (intersection over predecessors p of Dom(p))
///
/// The sets include the block itself. Chosen over the Lengauer-Tarjan or
/// Cooper-Harvey-Kennedy formulations purely because it is obviously correct
/// and these CFGs have tens of blocks, not thousands. An earlier attempt here
/// used the index-based `intersect` from Cooper-Harvey-Kennedy and got it
/// subtly wrong on loops, which is exactly the case that matters.
fn dominators(
    blocks: &[(BlockKey, &Block)],
    preds: &FxHashMap<BlockKey, Vec<BlockKey>>,
    reachable: &FxHashSet<BlockKey>,
) -> FxHashMap<BlockKey, FxHashSet<BlockKey>> {
    let all: Vec<BlockKey> = blocks
        .iter()
        .map(|(k, _)| k.clone())
        .filter(|k| reachable.contains(k))
        .collect();

    let mut dom: FxHashMap<BlockKey, FxHashSet<BlockKey>> = FxHashMap::default();
    for key in &all {
        if key.is_none() {
            // entry
            dom.insert(key.clone(), std::iter::once(key.clone()).collect());
        } else {
            dom.insert(key.clone(), all.iter().cloned().collect());
        }
    }

    let mut changed = true;
    while changed {
        changed = false;
        for key in &all {
            if key.is_none() {
                continue;
            }
            let reachable_preds: Vec<&BlockKey> = preds
                .get(key)
                .map(|ps| ps.iter().filter(|p| reachable.contains(p)).collect())
                .unwrap_or_default();
            if reachable_preds.is_empty() {
                continue;
            }
            let mut new_set: FxHashSet<BlockKey> = dom[reachable_preds[0]].clone();
            for pred in &reachable_preds[1..] {
                new_set = new_set.intersection(&dom[*pred]).cloned().collect();
            }
            new_set.insert(key.clone());
            if new_set != dom[key] {
                dom.insert(key.clone(), new_set);
                changed = true;
            }
        }
    }
    dom
}

pub fn validate_program(program: &Program) -> Vec<ValidationError> {
    let mut errors = Vec::new();
    for fun in program.functions.values() {
        errors.extend(validate_function(fun));
    }
    errors
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Cfg, FunDef, GlobalId};

    fn block(instructions: Vec<(LocalId, Instruction)>, term: (LocalId, Terminator)) -> Block {
        Block { instructions, terminator: term, hint_normalize: false }
    }

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(s: &str) -> Label {
        Label::from(s.to_string())
    }

    fn fun_with(cfg: Cfg) -> FunDef {
        FunDef {
            name: GlobalId::from("test".to_string()),
            capture_ids: vec![],
            arg_ids: vec![],
            cfg,
            source_span: None,
        }
    }

    fn num(n: i16) -> Instruction {
        Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(n) }
    }

    #[test]
    fn accepts_a_straight_line_function() {
        let cfg = Cfg {
            entry: block(
                vec![(id(0), num(1)), (id(1), num(2))],
                (id(2), Terminator::Return { value: Some(id(1)) }),
            ),
            named: Default::default(),
        };
        assert!(validate_function(&fun_with(cfg)).is_empty());
    }

    #[test]
    fn rejects_duplicate_definition() {
        let cfg = Cfg {
            entry: block(
                vec![(id(0), num(1)), (id(0), num(2))],
                (id(2), Terminator::Return { value: Some(id(0)) }),
            ),
            named: Default::default(),
        };
        let errors = validate_function(&fun_with(cfg));
        assert!(errors.iter().any(|e| e.message.contains("defined more than once")), "{:?}", errors);
    }

    /// The check the old validator did not have. `%10` is defined only on the
    /// true arm but used after the join, so it is undefined on the false path.
    /// A "defined somewhere in the CFG" test accepts this; dominance does not.
    #[test]
    fn rejects_use_that_is_not_dominated() {
        let mut named = crate::ir::new_label_map();
        named.insert(
            label("t"),
            block(vec![(id(10), num(7))], (id(11), Terminator::UnconditionalBranch { target: label("join") })),
        );
        named.insert(
            label("f"),
            block(vec![], (id(12), Terminator::UnconditionalBranch { target: label("join") })),
        );
        named.insert(
            label("join"),
            block(vec![], (id(13), Terminator::Return { value: Some(id(10)) })),
        );
        let cfg = Cfg {
            entry: block(
                vec![(id(0), Instruction::BoolConstant { value: true })],
                (
                    id(1),
                    Terminator::ConditionalBranch {
                        condition: id(0),
                        true_target: label("t"),
                        false_target: label("f"),
                    },
                ),
            ),
            named,
        };
        let errors = validate_function(&fun_with(cfg));
        assert!(
            errors.iter().any(|e| e.message.contains("does not dominate")),
            "expected a dominance error, got {:?}",
            errors
        );
    }

    #[test]
    fn accepts_a_value_defined_in_a_dominator() {
        let mut named = crate::ir::new_label_map();
        named.insert(
            label("t"),
            block(vec![], (id(11), Terminator::UnconditionalBranch { target: label("join") })),
        );
        named.insert(
            label("f"),
            block(vec![], (id(12), Terminator::UnconditionalBranch { target: label("join") })),
        );
        named.insert(
            label("join"),
            block(vec![], (id(13), Terminator::Return { value: Some(id(0)) })),
        );
        let cfg = Cfg {
            entry: block(
                vec![(id(0), Instruction::BoolConstant { value: true })],
                (
                    id(1),
                    Terminator::ConditionalBranch {
                        condition: id(0),
                        true_target: label("t"),
                        false_target: label("f"),
                    },
                ),
            ),
            named,
        };
        assert!(validate_function(&fun_with(cfg)).is_empty());
    }

    #[test]
    fn rejects_phi_missing_a_predecessor() {
        let mut named = crate::ir::new_label_map();
        named.insert(
            label("t"),
            block(vec![(id(10), num(7))], (id(11), Terminator::UnconditionalBranch { target: label("join") })),
        );
        named.insert(
            label("f"),
            block(vec![(id(20), num(8))], (id(12), Terminator::UnconditionalBranch { target: label("join") })),
        );
        named.insert(
            label("join"),
            block(
                vec![(id(30), Instruction::Phi { branches: vec![(label("t"), id(10))] })],
                (id(13), Terminator::Return { value: Some(id(30)) }),
            ),
        );
        let cfg = Cfg {
            entry: block(
                vec![(id(0), Instruction::BoolConstant { value: true })],
                (
                    id(1),
                    Terminator::ConditionalBranch {
                        condition: id(0),
                        true_target: label("t"),
                        false_target: label("f"),
                    },
                ),
            ),
            named,
        };
        let errors = validate_function(&fun_with(cfg));
        assert!(
            errors.iter().any(|e| e.message.contains("no branch for predecessor")),
            "{:?}",
            errors
        );
    }
}
