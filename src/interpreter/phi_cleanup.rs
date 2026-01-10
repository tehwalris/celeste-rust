//! Phi Cleanup Pass
//!
//! This pass cleans up Phi nodes after control flow changes (like Deopt insertion).
//! It handles:
//! 1. Removing Phi branches that reference blocks no longer predecessors
//! 2. Collapsing single-element Phis to direct references
//! 3. Detecting empty Phis (0 elements after cleanup)
//!
//! This is intentionally strict - it only handles Phi cleanup, nothing else.

use crate::interpreter::common::{FxHashMap, FxHashSet};
use crate::ir::{Block, Cfg, Instruction, Label, LocalId, Terminator};
#[cfg(test)]
use crate::ir::BinaryOp;

/// Result of phi cleanup
#[derive(Debug)]
pub struct PhiCleanupResult {
    /// The cleaned CFG
    pub cfg: Cfg,
    /// Number of Phi branches removed
    pub branches_removed: usize,
    /// Number of Phis collapsed to single values
    pub phis_collapsed: usize,
    /// Number of Phis that became empty (potential undefined locals)
    pub phis_emptied: usize,
    /// LocalId remapping for collapsed Phis (old_phi_id -> replacement_id)
    pub collapsed_mappings: FxHashMap<LocalId, LocalId>,
}

/// Compute actual predecessors of each block based on terminators.
/// Returns a map from block label to set of predecessor labels.
/// Entry block has no predecessors in this map.
fn compute_actual_predecessors(cfg: &Cfg) -> FxHashMap<Label, FxHashSet<Label>> {
    let mut predecessors: FxHashMap<Label, FxHashSet<Label>> = FxHashMap::default();

    // Initialize empty sets for all named blocks
    for label in cfg.named.keys() {
        predecessors.insert(label.clone(), FxHashSet::default());
    }

    // Add predecessors from entry block
    add_successors_as_predecessors(cfg.entry.terminator_kind(), &Label::from("entry".to_string()), &mut predecessors);

    // Add predecessors from named blocks
    for (label, block) in &cfg.named {
        add_successors_as_predecessors(block.terminator_kind(), label, &mut predecessors);
    }

    predecessors
}

fn add_successors_as_predecessors(
    terminator: &Terminator,
    from_label: &Label,
    predecessors: &mut FxHashMap<Label, FxHashSet<Label>>,
) {
    match terminator {
        Terminator::Return { .. } | Terminator::Deopt { .. } => {
            // No successors
        }
        Terminator::UnconditionalBranch { target } => {
            if let Some(preds) = predecessors.get_mut(target) {
                preds.insert(from_label.clone());
            }
        }
        Terminator::ConditionalBranch { true_target, false_target, .. } => {
            if let Some(preds) = predecessors.get_mut(true_target) {
                preds.insert(from_label.clone());
            }
            if let Some(preds) = predecessors.get_mut(false_target) {
                preds.insert(from_label.clone());
            }
        }
    }
}

/// Clean up a single Phi instruction based on actual predecessors.
/// Returns (new_instruction, was_collapsed, was_emptied)
fn cleanup_phi(
    phi_branches: &[(Label, LocalId)],
    actual_predecessors: &FxHashSet<Label>,
    from_entry: bool,
) -> (Vec<(Label, LocalId)>, Option<LocalId>, bool) {
    let mut new_branches = Vec::new();

    for (label, local_id) in phi_branches {
        // Special case: "entry" predecessor
        let is_valid = if label.as_str() == "entry" {
            from_entry || actual_predecessors.contains(label)
        } else {
            actual_predecessors.contains(label)
        };

        if is_valid {
            new_branches.push((label.clone(), *local_id));
        }
    }

    let was_emptied = new_branches.is_empty();
    let collapsed_to = if new_branches.len() == 1 {
        Some(new_branches[0].1)
    } else {
        None
    };

    (new_branches, collapsed_to, was_emptied)
}

/// Clean up Phis in a single block.
/// Returns (new_block, branches_removed, phis_collapsed, phis_emptied, collapsed_mappings)
fn cleanup_block_phis(
    block: &Block,
    actual_predecessors: &FxHashSet<Label>,
    from_entry: bool,
) -> (Block, usize, usize, usize, FxHashMap<LocalId, LocalId>) {
    let mut new_instructions = Vec::new();
    let mut branches_removed = 0;
    let mut phis_collapsed = 0;
    let mut phis_emptied = 0;
    let mut collapsed_mappings: FxHashMap<LocalId, LocalId> = FxHashMap::default();

    for (local_id, instruction) in &block.instructions {
        match instruction {
            Instruction::Phi { branches } => {
                let (new_branches, collapsed_to, was_emptied) =
                    cleanup_phi(branches, actual_predecessors, from_entry);

                let removed = branches.len() - new_branches.len();
                branches_removed += removed;

                if was_emptied {
                    phis_emptied += 1;
                    // Keep the Phi with empty branches - this will be an undefined local issue
                    // that the caller needs to handle
                    new_instructions.push((*local_id, Instruction::Phi { branches: vec![] }));
                } else if let Some(replacement_id) = collapsed_to {
                    // Single-element Phi - collapse it
                    phis_collapsed += 1;
                    collapsed_mappings.insert(*local_id, replacement_id);
                    // Don't emit the Phi - we'll remap references instead
                } else {
                    // Multiple elements - keep the Phi with filtered branches
                    new_instructions.push((*local_id, Instruction::Phi { branches: new_branches }));
                }
            }
            _ => {
                new_instructions.push((*local_id, instruction.clone()));
            }
        }
    }

    let new_block = Block {
        instructions: new_instructions,
        terminator: block.terminator.clone(),
        hint_normalize: block.hint_normalize,
    };

    (new_block, branches_removed, phis_collapsed, phis_emptied, collapsed_mappings)
}

/// Compute the transitive closure of a mapping.
/// If A -> B and B -> C, then A -> C.
fn transitive_closure(mappings: &FxHashMap<LocalId, LocalId>) -> FxHashMap<LocalId, LocalId> {
    let mut result = mappings.clone();
    let mut changed = true;

    while changed {
        changed = false;
        for (key, value) in mappings.iter() {
            // Follow the chain from value
            let mut final_value = *value;
            while let Some(&next) = result.get(&final_value) {
                if next == final_value {
                    break; // Avoid infinite loop on self-reference
                }
                final_value = next;
            }
            if final_value != result[key] {
                result.insert(*key, final_value);
                changed = true;
            }
        }
    }

    result
}

/// Apply collapsed Phi mappings to all LocalId references in a block.
fn remap_block_locals(block: &Block, mappings: &FxHashMap<LocalId, LocalId>) -> Block {
    if mappings.is_empty() {
        return block.clone();
    }

    let remap = |id: LocalId| -> LocalId {
        *mappings.get(&id).unwrap_or(&id)
    };

    let new_instructions: Vec<_> = block
        .instructions
        .iter()
        .map(|(id, instr)| (*id, instr.clone().map_local_ids(remap)))
        .collect();

    let new_terminator = (
        block.terminator_id(),
        match block.terminator_kind() {
            Terminator::Return { value: Some(v) } => Terminator::Return { value: Some(remap(*v)) },
            Terminator::Return { value: None } => Terminator::Return { value: None },
            Terminator::UnconditionalBranch { target } => {
                Terminator::UnconditionalBranch { target: target.clone() }
            }
            Terminator::ConditionalBranch { condition, true_target, false_target } => {
                Terminator::ConditionalBranch {
                    condition: remap(*condition),
                    true_target: true_target.clone(),
                    false_target: false_target.clone(),
                }
            }
            Terminator::Deopt { reason } => Terminator::Deopt { reason: reason.clone() },
        },
    );

    Block {
        instructions: new_instructions,
        terminator: new_terminator,
        hint_normalize: block.hint_normalize,
    }
}

/// Clean up Phi nodes in a CFG after control flow changes.
///
/// This pass:
/// 1. Computes actual predecessors based on terminators
/// 2. Removes Phi branches referencing non-predecessor blocks
/// 3. Collapses single-element Phis to direct references
/// 4. Reports empty Phis (for caller to handle undefined local issues)
pub fn cleanup_phis(cfg: &Cfg) -> PhiCleanupResult {
    let actual_predecessors = compute_actual_predecessors(cfg);

    let mut total_branches_removed = 0;
    let mut total_phis_collapsed = 0;
    let mut total_phis_emptied = 0;
    let mut all_collapsed_mappings: FxHashMap<LocalId, LocalId> = FxHashMap::default();

    // Entry block - has no predecessors in this context (Phis in entry would be weird)
    // But process it anyway for completeness
    let entry_preds = FxHashSet::default();
    let (new_entry, branches_removed, phis_collapsed, phis_emptied, collapsed_mappings) =
        cleanup_block_phis(&cfg.entry, &entry_preds, false);
    total_branches_removed += branches_removed;
    total_phis_collapsed += phis_collapsed;
    total_phis_emptied += phis_emptied;
    all_collapsed_mappings.extend(collapsed_mappings);

    // Named blocks
    let mut new_named: FxHashMap<Label, Block> = FxHashMap::default();
    for (label, block) in &cfg.named {
        let preds = actual_predecessors.get(label).cloned().unwrap_or_default();
        // Check if entry block is a predecessor
        let from_entry = match cfg.entry.terminator_kind() {
            Terminator::UnconditionalBranch { target } => target == label,
            Terminator::ConditionalBranch { true_target, false_target, .. } => {
                true_target == label || false_target == label
            }
            _ => false,
        };

        let (new_block, branches_removed, phis_collapsed, phis_emptied, collapsed_mappings) =
            cleanup_block_phis(block, &preds, from_entry);
        total_branches_removed += branches_removed;
        total_phis_collapsed += phis_collapsed;
        total_phis_emptied += phis_emptied;
        all_collapsed_mappings.extend(collapsed_mappings);
        new_named.insert(label.clone(), new_block);
    }

    // Compute transitive closure of mappings
    // This handles chains like: Phi A collapsed to %51, Phi B (defining %51) collapsed to %30
    // Without transitive closure, references to A would become %51 but %51 is undefined!
    let transitive_mappings = transitive_closure(&all_collapsed_mappings);

    // Now apply all collapsed mappings
    let final_entry = remap_block_locals(&new_entry, &transitive_mappings);
    let final_named: FxHashMap<Label, Block> = new_named
        .into_iter()
        .map(|(label, block)| (label, remap_block_locals(&block, &transitive_mappings)))
        .collect();

    PhiCleanupResult {
        cfg: Cfg {
            entry: final_entry,
            named: final_named,
        },
        branches_removed: total_branches_removed,
        phis_collapsed: total_phis_collapsed,
        phis_emptied: total_phis_emptied,
        collapsed_mappings: transitive_mappings,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pico8_num::Pico8Num;

    /// Helper to create a simple block with given instructions and terminator
    fn make_block(
        instructions: Vec<(LocalId, Instruction)>,
        terminator: (LocalId, Terminator),
    ) -> Block {
        Block {
            instructions,
            terminator,
            hint_normalize: false,
        }
    }

    #[test]
    fn test_no_change_when_predecessors_match() {
        // Entry branches to Block A, Block A branches to Block B
        // Block B has Phi referencing A - should remain valid (but collapse to single element)

        let entry = make_block(
            vec![],
            (LocalId::from(99), Terminator::UnconditionalBranch { target: Label::from("block_a".to_string()) }),
        );

        let block_a = make_block(
            vec![(LocalId::from(0), Instruction::NumberConstant { value: Pico8Num::from_i16(1) })],
            (LocalId::from(1), Terminator::UnconditionalBranch { target: Label::from("block_b".to_string()) }),
        );

        let block_b = make_block(
            vec![(
                LocalId::from(2),
                Instruction::Phi {
                    branches: vec![(Label::from("block_a".to_string()), LocalId::from(0))],
                },
            )],
            (LocalId::from(3), Terminator::Return { value: Some(LocalId::from(2)) }),
        );

        let cfg = Cfg {
            entry,
            named: [
                (Label::from("block_a".to_string()), block_a),
                (Label::from("block_b".to_string()), block_b),
            ].into_iter().collect(),
        };

        let result = cleanup_phis(&cfg);

        // Phi had only one branch, so it should be collapsed
        assert_eq!(result.phis_collapsed, 1);
        assert_eq!(result.branches_removed, 0);
        assert_eq!(result.phis_emptied, 0);

        // The collapsed mapping should map %2 -> %0
        assert_eq!(result.collapsed_mappings.get(&LocalId::from(2)), Some(&LocalId::from(0)));

        // The return should now reference %0 directly
        match result.cfg.named.get(&Label::from("block_b".to_string())).unwrap().terminator_kind() {
            Terminator::Return { value: Some(v) } => assert_eq!(*v, LocalId::from(0)),
            _ => panic!("Expected return terminator"),
        }
    }

    #[test]
    fn test_remove_branch_from_deopt_block() {
        // Entry branches to A and B
        // A was Deopt'd (now returns, doesn't branch to join)
        // B still branches to join
        // Join has Phi(A: %1, B: %2) - should remove A branch

        let entry = make_block(
            vec![
                (LocalId::from(0), Instruction::BoolConstant { value: true }),
            ],
            (
                LocalId::from(99),
                Terminator::ConditionalBranch {
                    condition: LocalId::from(0),
                    true_target: Label::from("block_a".to_string()),
                    false_target: Label::from("block_b".to_string()),
                },
            ),
        );

        // Block A now has Deopt terminator (doesn't branch to join anymore)
        let block_a = make_block(
            vec![(LocalId::from(1), Instruction::NumberConstant { value: Pico8Num::from_i16(1) })],
            (LocalId::from(10), Terminator::Deopt { reason: "test".to_string() }),
        );

        // Block B still branches to join
        let block_b = make_block(
            vec![(LocalId::from(2), Instruction::NumberConstant { value: Pico8Num::from_i16(2) })],
            (LocalId::from(11), Terminator::UnconditionalBranch { target: Label::from("join".to_string()) }),
        );

        // Join has Phi referencing both A and B
        let join = make_block(
            vec![(
                LocalId::from(3),
                Instruction::Phi {
                    branches: vec![
                        (Label::from("block_a".to_string()), LocalId::from(1)),
                        (Label::from("block_b".to_string()), LocalId::from(2)),
                    ],
                },
            )],
            (LocalId::from(12), Terminator::Return { value: Some(LocalId::from(3)) }),
        );

        let cfg = Cfg {
            entry,
            named: [
                (Label::from("block_a".to_string()), block_a),
                (Label::from("block_b".to_string()), block_b),
                (Label::from("join".to_string()), join),
            ]
            .into_iter()
            .collect(),
        };

        let result = cleanup_phis(&cfg);

        // One branch should be removed (block_a)
        assert_eq!(result.branches_removed, 1);
        // Phi now has one element, so should be collapsed
        assert_eq!(result.phis_collapsed, 1);
        assert_eq!(result.phis_emptied, 0);

        // %3 should be remapped to %2
        assert_eq!(result.collapsed_mappings.get(&LocalId::from(3)), Some(&LocalId::from(2)));
    }

    #[test]
    fn test_phi_becomes_empty() {
        // Entry branches conditionally to A and B
        // Both A and B get Deopt'd
        // Join has Phi(A, B) - becomes empty!

        let entry = make_block(
            vec![(LocalId::from(0), Instruction::BoolConstant { value: true })],
            (
                LocalId::from(99),
                Terminator::ConditionalBranch {
                    condition: LocalId::from(0),
                    true_target: Label::from("block_a".to_string()),
                    false_target: Label::from("block_b".to_string()),
                },
            ),
        );

        let block_a = make_block(
            vec![(LocalId::from(1), Instruction::NumberConstant { value: Pico8Num::from_i16(1) })],
            (LocalId::from(10), Terminator::Deopt { reason: "a".to_string() }),
        );

        let block_b = make_block(
            vec![(LocalId::from(2), Instruction::NumberConstant { value: Pico8Num::from_i16(2) })],
            (LocalId::from(11), Terminator::Deopt { reason: "b".to_string() }),
        );

        let join = make_block(
            vec![(
                LocalId::from(3),
                Instruction::Phi {
                    branches: vec![
                        (Label::from("block_a".to_string()), LocalId::from(1)),
                        (Label::from("block_b".to_string()), LocalId::from(2)),
                    ],
                },
            )],
            (LocalId::from(12), Terminator::Return { value: Some(LocalId::from(3)) }),
        );

        let cfg = Cfg {
            entry,
            named: [
                (Label::from("block_a".to_string()), block_a),
                (Label::from("block_b".to_string()), block_b),
                (Label::from("join".to_string()), join),
            ]
            .into_iter()
            .collect(),
        };

        let result = cleanup_phis(&cfg);

        // Both branches removed
        assert_eq!(result.branches_removed, 2);
        // Phi became empty
        assert_eq!(result.phis_emptied, 1);
        assert_eq!(result.phis_collapsed, 0);

        // The Phi should still exist but with empty branches
        let join_block = result.cfg.named.get(&Label::from("join".to_string())).unwrap();
        match &join_block.instructions[0].1 {
            Instruction::Phi { branches } => {
                assert!(branches.is_empty());
            }
            _ => panic!("Expected Phi instruction"),
        }
    }

    #[test]
    fn test_multi_branch_phi_partial_cleanup() {
        // Three blocks branch to join
        // One gets Deopt'd, two remain
        // Phi(A, B, C) -> Phi(B, C)

        let entry = make_block(
            vec![],
            (LocalId::from(99), Terminator::UnconditionalBranch { target: Label::from("dispatch".to_string()) }),
        );

        let dispatch = make_block(
            vec![(LocalId::from(0), Instruction::BoolConstant { value: true })],
            (
                LocalId::from(98),
                Terminator::ConditionalBranch {
                    condition: LocalId::from(0),
                    true_target: Label::from("block_a".to_string()),
                    false_target: Label::from("block_b".to_string()),
                },
            ),
        );

        let block_a = make_block(
            vec![(LocalId::from(1), Instruction::NumberConstant { value: Pico8Num::from_i16(1) })],
            (LocalId::from(10), Terminator::Deopt { reason: "a".to_string() }), // Deopt'd!
        );

        let block_b = make_block(
            vec![(LocalId::from(2), Instruction::NumberConstant { value: Pico8Num::from_i16(2) })],
            (
                LocalId::from(11),
                Terminator::ConditionalBranch {
                    condition: LocalId::from(2),
                    true_target: Label::from("block_c".to_string()),
                    false_target: Label::from("join".to_string()),
                },
            ),
        );

        let block_c = make_block(
            vec![(LocalId::from(3), Instruction::NumberConstant { value: Pico8Num::from_i16(3) })],
            (LocalId::from(12), Terminator::UnconditionalBranch { target: Label::from("join".to_string()) }),
        );

        let join = make_block(
            vec![(
                LocalId::from(4),
                Instruction::Phi {
                    branches: vec![
                        (Label::from("block_a".to_string()), LocalId::from(1)),
                        (Label::from("block_b".to_string()), LocalId::from(2)),
                        (Label::from("block_c".to_string()), LocalId::from(3)),
                    ],
                },
            )],
            (LocalId::from(13), Terminator::Return { value: Some(LocalId::from(4)) }),
        );

        let cfg = Cfg {
            entry,
            named: [
                (Label::from("dispatch".to_string()), dispatch),
                (Label::from("block_a".to_string()), block_a),
                (Label::from("block_b".to_string()), block_b),
                (Label::from("block_c".to_string()), block_c),
                (Label::from("join".to_string()), join),
            ]
            .into_iter()
            .collect(),
        };

        let result = cleanup_phis(&cfg);

        // One branch removed (block_a)
        assert_eq!(result.branches_removed, 1);
        // Phi still has 2 elements, so not collapsed
        assert_eq!(result.phis_collapsed, 0);
        assert_eq!(result.phis_emptied, 0);

        // Check the Phi now has only B and C
        let join_block = result.cfg.named.get(&Label::from("join".to_string())).unwrap();
        match &join_block.instructions[0].1 {
            Instruction::Phi { branches } => {
                assert_eq!(branches.len(), 2);
                let labels: Vec<_> = branches.iter().map(|(l, _)| l.as_str()).collect();
                assert!(labels.contains(&"block_b"));
                assert!(labels.contains(&"block_c"));
                assert!(!labels.contains(&"block_a"));
            }
            _ => panic!("Expected Phi instruction"),
        }
    }

    #[test]
    fn test_collapsed_phi_remaps_uses() {
        // Block A branches to B
        // B has Phi(%0 from A), then uses Phi result in BinaryOp
        // Phi should collapse and BinaryOp should reference %0 directly

        let entry = make_block(
            vec![(LocalId::from(0), Instruction::NumberConstant { value: Pico8Num::from_i16(1) })],
            (LocalId::from(99), Terminator::UnconditionalBranch { target: Label::from("block_b".to_string()) }),
        );

        let block_b = make_block(
            vec![
                (
                    LocalId::from(1),
                    Instruction::Phi {
                        branches: vec![(Label::from("entry".to_string()), LocalId::from(0))],
                    },
                ),
                (
                    LocalId::from(2),
                    Instruction::NumberConstant { value: Pico8Num::from_i16(2) },
                ),
                (
                    LocalId::from(3),
                    Instruction::BinaryOp {
                        op: BinaryOp::Plus,
                        left: LocalId::from(1), // References the Phi
                        right: LocalId::from(2),
                    },
                ),
            ],
            (LocalId::from(10), Terminator::Return { value: Some(LocalId::from(3)) }),
        );

        let cfg = Cfg {
            entry,
            named: [(Label::from("block_b".to_string()), block_b)].into_iter().collect(),
        };

        let result = cleanup_phis(&cfg);

        assert_eq!(result.phis_collapsed, 1);
        assert_eq!(result.collapsed_mappings.get(&LocalId::from(1)), Some(&LocalId::from(0)));

        // Check that BinaryOp now references %0 instead of %1
        let block_b = result.cfg.named.get(&Label::from("block_b".to_string())).unwrap();
        // Find the BinaryOp instruction
        let binary_op = block_b.instructions.iter().find(|(_, i)| matches!(i, Instruction::BinaryOp { .. }));
        match binary_op {
            Some((_, Instruction::BinaryOp { left, .. })) => {
                assert_eq!(*left, LocalId::from(0), "BinaryOp should reference %0 after Phi collapse");
            }
            _ => panic!("Expected BinaryOp instruction"),
        }
    }

    #[test]
    fn test_transitive_closure_of_collapsed_phis() {
        // Test the transitive closure case:
        // Entry -> block_a -> block_b -> block_c
        // block_b has Phi (%2) referencing %1 from block_a (collapses to %1)
        // block_c has Phi (%3) referencing %2 from block_b (collapses to %2, which should become %1)
        // Final use should reference %1, not %2 (which doesn't exist after collapse)

        let entry = make_block(
            vec![(LocalId::from(0), Instruction::NumberConstant { value: Pico8Num::from_i16(0) })],
            (LocalId::from(99), Terminator::UnconditionalBranch { target: Label::from("block_a".to_string()) }),
        );

        let block_a = make_block(
            vec![(LocalId::from(1), Instruction::NumberConstant { value: Pico8Num::from_i16(1) })],
            (LocalId::from(98), Terminator::UnconditionalBranch { target: Label::from("block_b".to_string()) }),
        );

        // block_b has Phi from block_a only -> collapses to %1
        let block_b = make_block(
            vec![(
                LocalId::from(2),
                Instruction::Phi {
                    branches: vec![(Label::from("block_a".to_string()), LocalId::from(1))],
                },
            )],
            (LocalId::from(97), Terminator::UnconditionalBranch { target: Label::from("block_c".to_string()) }),
        );

        // block_c has Phi from block_b only -> collapses to %2
        // But %2 is also collapsed! So transitively, this should become %1
        let block_c = make_block(
            vec![
                (
                    LocalId::from(3),
                    Instruction::Phi {
                        branches: vec![(Label::from("block_b".to_string()), LocalId::from(2))],
                    },
                ),
                // This BinaryOp uses %3 (the Phi result), which should become %1 after transitive remapping
                (
                    LocalId::from(4),
                    Instruction::BinaryOp {
                        op: BinaryOp::Plus,
                        left: LocalId::from(3),
                        right: LocalId::from(0),
                    },
                ),
            ],
            (LocalId::from(96), Terminator::Return { value: Some(LocalId::from(4)) }),
        );

        let cfg = Cfg {
            entry,
            named: [
                (Label::from("block_a".to_string()), block_a),
                (Label::from("block_b".to_string()), block_b),
                (Label::from("block_c".to_string()), block_c),
            ].into_iter().collect(),
        };

        let result = cleanup_phis(&cfg);

        // Both Phis should be collapsed
        assert_eq!(result.phis_collapsed, 2);

        // The transitive closure should give us:
        // %2 -> %1 (from block_b's Phi collapse)
        // %3 -> %1 (from block_c's Phi collapse, transitively through %2)
        assert_eq!(result.collapsed_mappings.get(&LocalId::from(2)), Some(&LocalId::from(1)));
        assert_eq!(result.collapsed_mappings.get(&LocalId::from(3)), Some(&LocalId::from(1)));

        // Check that the BinaryOp in block_c now references %1 (not %3 or %2)
        let block_c = result.cfg.named.get(&Label::from("block_c".to_string())).unwrap();
        let binary_op = block_c.instructions.iter().find(|(_, i)| matches!(i, Instruction::BinaryOp { .. }));
        match binary_op {
            Some((_, Instruction::BinaryOp { left, .. })) => {
                assert_eq!(*left, LocalId::from(1), "BinaryOp should reference %1 after transitive Phi collapse");
            }
            _ => panic!("Expected BinaryOp instruction"),
        }
    }
}
