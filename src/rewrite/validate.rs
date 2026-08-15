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
pub type BlockKey = Option<Label>;

pub fn block_label(key: &BlockKey) -> String {
    match key {
        None => "__entry".to_string(),
        Some(l) => l.as_str().to_string(),
    }
}

pub fn all_blocks(cfg: &Cfg) -> Vec<(BlockKey, &Block)> {
    let mut out: Vec<(BlockKey, &Block)> = vec![(None, &cfg.entry)];
    let mut named: Vec<(&Label, &Block)> = cfg.named.iter().collect();
    named.sort_by_key(|(l, _)| l.as_str().to_string());
    out.extend(named.into_iter().map(|(l, b)| (Some(l.clone()), b)));
    out
}

pub fn successors(block: &Block) -> Vec<BlockKey> {
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
    //
    // Walked as a depth-first traversal of the *dominator tree*, carrying the
    // set of available values down and undoing it on the way back up. The
    // previous version rebuilt that set per block by unioning the definitions of
    // every dominator, which is the same answer computed
    // O(blocks x depth) times instead of once.
    let dominance = Dominance::of(cfg);
    let children = dominance.tree();
    let external: FxHashSet<LocalId> = fun
        .arg_ids
        .iter()
        .flatten()
        .chain(fun.capture_ids.iter())
        .copied()
        .collect();
    let by_key: FxHashMap<&BlockKey, &Block> = blocks.iter().map(|(k, b)| (k, *b)).collect();

    let mut available: FxHashSet<LocalId> = external.clone();
    // (block, whether we are entering it) - an explicit stack so that deep
    // dominator trees cannot blow the real one.
    let mut stack: Vec<(usize, bool)> = Vec::new();
    let mut undo: Vec<Vec<LocalId>> = Vec::new();
    if let Some(entry) = dominance.index_of(&None) {
        stack.push((entry, true));
    }
    while let Some((node, entering)) = stack.pop() {
        if !entering {
            for id in undo.pop().unwrap_or_default() {
                available.remove(&id);
            }
            continue;
        }
        let key = dominance.key(node).clone();
        let Some(block) = by_key.get(&key).copied() else { continue };
        let mut added: Vec<LocalId> = Vec::new();

        let mut seen_non_phi = false;
        for (id, instr) in &block.instructions {
            if let Instruction::Phi { branches } = instr {
                if !seen_non_phi {
                    // A phi operand is evaluated on the incoming edge, so it
                    // must be available at the end of that predecessor, not
                    // here: the block defining it must dominate that
                    // predecessor.
                    for (label, value) in branches {
                        if external.contains(value) {
                            continue;
                        }
                        let pred_key: BlockKey = if label.as_str() == "__entry" {
                            None
                        } else {
                            Some(label.clone())
                        };
                        // An edge from an unreachable predecessor can never
                        // be taken, so nothing needs to be available along
                        // it. This state exists between a `fold` that
                        // removes the last reachable edge into a loop and
                        // the `dce` that sweeps the loop - the walk here
                        // already skips the unreachable blocks' own
                        // instructions for the same reason.
                        if !dominance.is_reachable(&pred_key) {
                            continue;
                        }
                        let ok = match (
                            defined.get(value).and_then(|k| dominance.index_of(k)),
                            dominance.index_of(&pred_key),
                        ) {
                            (Some(def), Some(pred)) => dominance.dominates(def, pred),
                            _ => false,
                        };
                        if !ok {
                            err!(
                                "phi {} in '{}': {} from '{}' is not available there",
                                local_name(*id),
                                block_label(&key),
                                local_name(*value),
                                label.as_str()
                            );
                        }
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
                            block_label(&key)
                        );
                    }
                }
            }
            if available.insert(*id) {
                added.push(*id);
            }
        }
        for used in block.terminator_kind().get_used_locals() {
            if !available.contains(&used) {
                err!(
                    "terminator of '{}' uses {}, which does not dominate it",
                    block_label(&key),
                    local_name(used)
                );
            }
        }
        // The terminator's own id is defined by this block, so it is available
        // to blocks this one dominates but not within the block itself.
        if available.insert(block.terminator_id()) {
            added.push(block.terminator_id());
        }

        undo.push(added);
        stack.push((node, false));
        for &child in &children[node] {
            stack.push((child, true));
        }
    }

    // --- names refer to live instructions ---
    //
    // A name outliving the instruction it named is the failure mode that
    // would make the recipe address the wrong thing SILENTLY: the entry
    // resolves, to a local that no longer exists or - worse, once ids are
    // reused - to a different one. `Cfg::map_blocks` deliberately carries
    // names through a rewrite, so a rule that deletes an instruction is
    // responsible for pruning its name, and this is what makes forgetting
    // loud. Names are addressing metadata and cannot affect the program,
    // so this checks bookkeeping, not semantics.
    for (id, name) in cfg.names.iter() {
        if !defined.contains_key(&id) {
            err!(
                "name {:?} refers to {} which this function does not define \
                 - a rewrite deleted it without pruning the name",
                name,
                local_name(id)
            );
        }
    }

    // --- the slot map matches the CFG ---
    //
    // Run here, after every rewrite, rather than only after `allocate_slots`.
    // The dangerous case is not a bad allocation but a stale one: a rule that
    // introduces or renumbers ids while an older map is still attached. Every
    // rule that rebuilds blocks goes through `Cfg::map_blocks`, which resets the
    // map to the identity, and this is what makes that obligation enforced
    // rather than merely documented.
    if errors.is_empty() {
        for message in super::slots::check(fun) {
            err!("{}", message);
        }
    }

    errors
}

pub fn compute_reachable(blocks: &[(BlockKey, &Block)]) -> FxHashSet<BlockKey> {
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
/// ```text
/// Dom(entry) = {entry}
/// Dom(n)     = {n} union (intersection over predecessors p of Dom(p))
/// ```
///
/// The sets include the block itself. Chosen over the Lengauer-Tarjan or
/// Cooper-Harvey-Kennedy formulations purely because it is obviously correct.
/// An earlier attempt here used the index-based `intersect` from
/// Cooper-Harvey-Kennedy and got it subtly wrong on loops, which is exactly the
/// case that matters.
///
/// The sets are bitsets over block indices rather than hash sets of labels, and
/// the fixpoint visits blocks in reverse postorder. Both are representation
/// choices, not a different algorithm - the recurrence above is unchanged. They
/// matter because the doc comment here used to say "these CFGs have tens of
/// blocks, not thousands", and 84 inlines made that false: `player.update_21` is
/// now hundreds of blocks, and hashing label strings to intersect sets of them
/// was 97% of recipe replay.
pub struct Dominance {
    index: FxHashMap<BlockKey, usize>,
    keys: Vec<BlockKey>,
    /// `dom[i]` is the bitset of blocks that dominate block `i`, including `i`.
    /// Empty for unreachable blocks.
    dom: Vec<Vec<u64>>,
    reachable: Vec<bool>,
    /// Blocks in reverse postorder, reachable only.
    order: Vec<usize>,
}

fn bit_get(set: &[u64], i: usize) -> bool {
    set[i / 64] & (1u64 << (i % 64)) != 0
}

fn bit_set(set: &mut [u64], i: usize) {
    set[i / 64] |= 1u64 << (i % 64);
}

impl Dominance {
    pub fn of(cfg: &Cfg) -> Self {
        let blocks = all_blocks(cfg);
        let mut preds: FxHashMap<BlockKey, Vec<BlockKey>> = FxHashMap::default();
        for (key, _) in &blocks {
            preds.entry(key.clone()).or_default();
        }
        for (key, block) in &blocks {
            for succ in successors(block) {
                preds.entry(succ).or_default().push(key.clone());
            }
        }
        Self::new(&blocks, &preds)
    }

    fn new(blocks: &[(BlockKey, &Block)], preds: &FxHashMap<BlockKey, Vec<BlockKey>>) -> Self {
        let n = blocks.len();
        let words = n.div_ceil(64).max(1);
        let keys: Vec<BlockKey> = blocks.iter().map(|(k, _)| k.clone()).collect();
        let index: FxHashMap<BlockKey, usize> = keys
            .iter()
            .enumerate()
            .map(|(i, k)| (k.clone(), i))
            .collect();

        // Postorder DFS from the entry, which is index 0 by `all_blocks`.
        let by_index: Vec<&Block> = blocks.iter().map(|(_, b)| *b).collect();
        let mut reachable = vec![false; n];
        let mut postorder: Vec<usize> = Vec::with_capacity(n);
        let mut stack: Vec<(usize, usize)> = Vec::new();
        let entry = index.get(&None).copied();
        if let Some(entry) = entry {
            reachable[entry] = true;
            stack.push((entry, 0));
            while let Some((node, child)) = stack.pop() {
                let succs = successors(by_index[node]);
                if child < succs.len() {
                    stack.push((node, child + 1));
                    if let Some(&next) = index.get(&succs[child]) {
                        if !reachable[next] {
                            reachable[next] = true;
                            stack.push((next, 0));
                        }
                    }
                } else {
                    postorder.push(node);
                }
            }
        }
        let order: Vec<usize> = postorder.into_iter().rev().collect();

        let pred_indices: Vec<Vec<usize>> = keys
            .iter()
            .map(|key| {
                preds
                    .get(key)
                    .map(|ps| {
                        ps.iter()
                            .filter_map(|p| index.get(p).copied())
                            .filter(|p| reachable[*p])
                            .collect()
                    })
                    .unwrap_or_default()
            })
            .collect();

        let mut dom: Vec<Vec<u64>> = vec![Vec::new(); n];
        let mut everything = vec![0u64; words];
        for &i in &order {
            bit_set(&mut everything, i);
        }
        for &i in &order {
            if Some(i) == entry {
                let mut only_self = vec![0u64; words];
                bit_set(&mut only_self, i);
                dom[i] = only_self;
            } else {
                dom[i] = everything.clone();
            }
        }

        let mut scratch = vec![0u64; words];
        let mut changed = true;
        while changed {
            changed = false;
            for &i in &order {
                if Some(i) == entry || pred_indices[i].is_empty() {
                    continue;
                }
                scratch.copy_from_slice(&dom[pred_indices[i][0]]);
                for &p in &pred_indices[i][1..] {
                    for (word, other) in scratch.iter_mut().zip(&dom[p]) {
                        *word &= *other;
                    }
                }
                bit_set(&mut scratch, i);
                if scratch != dom[i] {
                    dom[i].copy_from_slice(&scratch);
                    changed = true;
                }
            }
        }

        Self { index, keys, dom, reachable, order }
    }

    pub fn index_of(&self, key: &BlockKey) -> Option<usize> {
        self.index.get(key).copied()
    }

    pub fn is_reachable(&self, key: &BlockKey) -> bool {
        self.index_of(key).is_some_and(|i| self.reachable[i])
    }

    /// Does block `dominator` dominate block `block`? A block dominates itself.
    pub fn dominates(&self, dominator: usize, block: usize) -> bool {
        self.reachable[block] && bit_get(&self.dom[block], dominator)
    }

    /// The dominator tree, as a child list per block. The immediate dominator of
    /// a block is its strict dominator with the highest reverse-postorder
    /// number, which is the one all the others dominate.
    pub fn tree(&self) -> Vec<Vec<usize>> {
        let mut rpo_number = vec![usize::MAX; self.keys.len()];
        for (position, &i) in self.order.iter().enumerate() {
            rpo_number[i] = position;
        }
        let mut children = vec![Vec::new(); self.keys.len()];
        for &i in &self.order {
            let mut best: Option<usize> = None;
            for &candidate in &self.order {
                if candidate == i || !bit_get(&self.dom[i], candidate) {
                    continue;
                }
                if best.is_none_or(|b| rpo_number[candidate] > rpo_number[b]) {
                    best = Some(candidate);
                }
            }
            if let Some(parent) = best {
                children[parent].push(i);
            }
        }
        children
    }

    /// Reachable blocks in reverse postorder.
    pub fn order(&self) -> &[usize] {
        &self.order
    }

    pub fn key(&self, i: usize) -> &BlockKey {
        &self.keys[i]
    }

}

/// Dominator sets for a function, keyed by block. Each set includes the block
/// itself. Unreachable blocks are absent.
///
/// Convenience wrapper over `Dominance` for callers that want a set they can
/// ask `contains` on. It materialises the whole relation, so prefer
/// `Dominance::dominates` where the CFG is large.
pub fn dominator_sets(cfg: &Cfg) -> FxHashMap<BlockKey, FxHashSet<BlockKey>> {
    let dominance = Dominance::of(cfg);
    let mut out: FxHashMap<BlockKey, FxHashSet<BlockKey>> = FxHashMap::default();
    for &i in dominance.order() {
        let set: FxHashSet<BlockKey> = dominance
            .order()
            .iter()
            .filter(|&&d| dominance.dominates(d, i))
            .map(|&d| dominance.key(d).clone())
            .collect();
        out.insert(dominance.key(i).clone(), set);
    }
    out
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
        let cfg = Cfg::new(block(
                vec![(id(0), num(1)), (id(1), num(2))],
                (id(2), Terminator::Return { value: Some(id(1)) }),
            ), Default::default());
        assert!(validate_function(&fun_with(cfg)).is_empty());
    }

    #[test]
    fn rejects_duplicate_definition() {
        let cfg = Cfg::new(block(
                vec![(id(0), num(1)), (id(0), num(2))],
                (id(2), Terminator::Return { value: Some(id(0)) }),
            ), Default::default());
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
        let cfg = Cfg::new(block(
                vec![(id(0), Instruction::BoolConstant { value: true })],
                (
                    id(1),
                    Terminator::ConditionalBranch {
                        condition: id(0),
                        true_target: label("t"),
                        false_target: label("f"),
                    },
                ),
            ), named);
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
        let cfg = Cfg::new(block(
                vec![(id(0), Instruction::BoolConstant { value: true })],
                (
                    id(1),
                    Terminator::ConditionalBranch {
                        condition: id(0),
                        true_target: label("t"),
                        false_target: label("f"),
                    },
                ),
            ), named);
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
        let cfg = Cfg::new(block(
                vec![(id(0), Instruction::BoolConstant { value: true })],
                (
                    id(1),
                    Terminator::ConditionalBranch {
                        condition: id(0),
                        true_target: label("t"),
                        false_target: label("f"),
                    },
                ),
            ), named);
        let errors = validate_function(&fun_with(cfg));
        assert!(
            errors.iter().any(|e| e.message.contains("no branch for predecessor")),
            "{:?}",
            errors
        );
    }

    /// The textbook fixpoint, written out again here, over hash sets of block
    /// keys. This is what `Dominance` replaced; keeping an independent copy is
    /// the point of the test.
    fn naive_dominators(
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

    /// A deterministic CFG generator. `Math::random` is not available in this
    /// project's tooling and a fixed sequence is reproducible anyway, so this is
    /// a plain LCG seeded per case.
    fn random_cfg(seed: u64, n: usize) -> Cfg {
        let mut state = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
        let mut next = move || {
            state = state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
            (state >> 33) as usize
        };
        let names: Vec<String> = (1..n).map(|i| format!("b{}", i)).collect();
        let terminator = |pick: &mut dyn FnMut() -> usize, id_base: usize| {
            let kind = pick() % 4;
            let term = match kind {
                0 => Terminator::Return { value: None },
                1 => Terminator::UnconditionalBranch {
                    target: label(&names[pick() % names.len()]),
                },
                _ => Terminator::ConditionalBranch {
                    condition: id(0),
                    true_target: label(&names[pick() % names.len()]),
                    false_target: label(&names[pick() % names.len()]),
                },
            };
            (id(id_base), term)
        };
        let entry = block(vec![], terminator(&mut next, 1000));
        let mut named = FxHashMap::default();
        for (i, name) in names.iter().enumerate() {
            named.insert(label(name), block(vec![], terminator(&mut next, 2000 + i)));
        }
        Cfg::new(entry, named.into_iter().collect())
    }

    /// The bitset dominators must agree with the textbook fixpoint on CFGs with
    /// loops, irreducible flow and unreachable blocks - which is exactly where
    /// a previous attempt at a faster formulation went subtly wrong.
    #[test]
    fn bitset_dominators_agree_with_the_naive_fixpoint() {
        for seed in 0..200u64 {
            let n = 2 + (seed as usize % 12);
            let cfg = random_cfg(seed, n);
            let blocks = all_blocks(&cfg);
            let mut preds: FxHashMap<BlockKey, Vec<BlockKey>> = FxHashMap::default();
            for (key, _) in &blocks {
                preds.entry(key.clone()).or_default();
            }
            for (key, b) in &blocks {
                for succ in successors(b) {
                    preds.entry(succ).or_default().push(key.clone());
                }
            }
            let reachable = compute_reachable(&blocks);
            let expected = naive_dominators(&blocks, &preds, &reachable);
            let actual = dominator_sets(&cfg);
            assert_eq!(
                expected.len(),
                actual.len(),
                "seed {}: different number of reachable blocks",
                seed
            );
            for (key, want) in &expected {
                let got = actual.get(key).unwrap_or_else(|| {
                    panic!("seed {}: missing block {}", seed, block_label(key))
                });
                assert_eq!(
                    want, got,
                    "seed {}: dominators of '{}' differ",
                    seed,
                    block_label(key)
                );
            }
        }
    }

    /// The dominator tree must be consistent with the dominator sets: a block's
    /// parent is a strict dominator, and the tree reaches every reachable block.
    #[test]
    fn the_dominator_tree_matches_the_dominator_sets() {
        for seed in 0..200u64 {
            let cfg = random_cfg(seed, 2 + (seed as usize % 12));
            let dominance = Dominance::of(&cfg);
            let children = dominance.tree();
            let mut seen = vec![false; children.len()];
            let Some(entry) = dominance.index_of(&None) else { continue };
            let mut stack = vec![entry];
            while let Some(node) = stack.pop() {
                assert!(!seen[node], "seed {}: dominator tree has a cycle", seed);
                seen[node] = true;
                for &child in &children[node] {
                    assert!(
                        dominance.dominates(node, child) && node != child,
                        "seed {}: tree parent does not strictly dominate its child",
                        seed
                    );
                    stack.push(child);
                }
            }
            for &i in dominance.order() {
                assert!(
                    seen[i],
                    "seed {}: reachable block '{}' is not in the dominator tree",
                    seed,
                    block_label(dominance.key(i))
                );
            }
        }
    }
}
