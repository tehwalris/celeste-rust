//! Phi Cleanup Pass
//!
//! This pass cleans up Phi nodes after control flow changes (like Deopt insertion or DCE).
//! It handles:
//! 1. Removing Phi branches that reference blocks no longer predecessors
//! 2. Removing Phi branches that reference undefined locals (from DCE-removed blocks)
//! 3. Collapsing single-element Phis to direct references
//! 4. Detecting empty Phis (0 elements after cleanup)
//!
//! This is intentionally strict - it only handles Phi cleanup, nothing else.

use crate::interpreter::common::{FxHashMap, FxHashSet};
use crate::ir::{Block, BlockId, Cfg, Instruction, Label, LocalId};
#[cfg(test)]
use crate::ir::{BinaryOp, Terminator};

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

/// Convert the centralized predecessor map (BlockId -> Vec<BlockId>) to the format
/// used by phi cleanup (Label -> FxHashSet<Label>).
///
/// Only includes named blocks in the output (entry block has no predecessors in CFG form).
fn predecessors_for_phi_cleanup(cfg: &Cfg) -> FxHashMap<Label, FxHashSet<Label>> {
    let block_preds = cfg.compute_predecessors();
    let mut result: FxHashMap<Label, FxHashSet<Label>> = FxHashMap::default();

    for (block_id, preds) in block_preds {
        // Only include named blocks - entry block doesn't need predecessor info for phi cleanup
        if let BlockId::Named(label) = block_id {
            let pred_labels: FxHashSet<Label> = preds.into_iter().map(|p| p.label()).collect();
            result.insert(label, pred_labels);
        }
    }

    result
}

/// Clean up a single Phi instruction based on actual predecessors and defined locals.
/// Returns (new_instruction, was_collapsed, was_emptied)
fn cleanup_phi(
    phi_branches: &[(Label, LocalId)],
    actual_predecessors: &FxHashSet<Label>,
    defined_locals: Option<&FxHashSet<LocalId>>,
) -> (Vec<(Label, LocalId)>, Option<LocalId>, bool) {
    let mut new_branches = Vec::new();

    for (label, local_id) in phi_branches {
        // Check if this predecessor is valid (the predecessors set uses Label::entry() for entry block)
        let is_predecessor_valid = actual_predecessors.contains(label);

        // Check if the local is defined (if we're filtering by defined locals)
        let is_local_valid = defined_locals
            .map(|defined| defined.contains(local_id))
            .unwrap_or(true);

        if is_predecessor_valid && is_local_valid {
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
    defined_locals: Option<&FxHashSet<LocalId>>,
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
                    cleanup_phi(branches, actual_predecessors, defined_locals);

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

    (
        block.with_instructions(new_instructions),
        branches_removed,
        phis_collapsed,
        phis_emptied,
        collapsed_mappings,
    )
}

/// Compute the transitive closure of a mapping.
/// If A -> B and B -> C, then A -> C.
///
/// Handles cycles by detecting when we revisit a node during chain traversal.
fn transitive_closure(mappings: &FxHashMap<LocalId, LocalId>) -> FxHashMap<LocalId, LocalId> {
    let mut result = mappings.clone();
    let mut changed = true;

    while changed {
        changed = false;
        for (key, value) in mappings.iter() {
            // Follow the chain from value, tracking visited nodes to detect cycles
            let mut final_value = *value;
            let mut visited: FxHashSet<LocalId> = FxHashSet::default();
            visited.insert(*key); // The starting key is implicitly "visited"

            while let Some(&next) = result.get(&final_value) {
                if visited.contains(&next) {
                    // Cycle detected - stop here to avoid infinite loop
                    break;
                }
                visited.insert(final_value);
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
        block.terminator_kind().map_local_ids(remap),
    );

    Block {
        instructions: new_instructions,
        terminator: new_terminator,
        hint_normalize: block.hint_normalize,
    }
}

/// Collect all LocalIds defined in a CFG.
///
/// This includes:
/// - All instruction outputs (left-hand side of assignments)
/// - All terminator IDs
/// - Optionally, predefined locals (function arguments, unpack slots)
pub fn collect_defined_locals(cfg: &Cfg, predefined_locals: &[LocalId]) -> FxHashSet<LocalId> {
    let mut defined_locals: FxHashSet<LocalId> = FxHashSet::default();

    // Add predefined locals (args, unpack slots)
    for local_id in predefined_locals {
        defined_locals.insert(*local_id);
    }

    // Collect definitions from all blocks
    for block in cfg.iter_blocks() {
        for (local_id, _) in &block.instructions {
            defined_locals.insert(*local_id);
        }
        defined_locals.insert(block.terminator_id());
    }

    defined_locals
}

/// Clean up Phi nodes in a CFG after control flow changes.
///
/// This pass:
/// 1. Computes actual predecessors based on terminators
/// 2. Removes Phi branches referencing non-predecessor blocks
/// 3. Optionally removes Phi branches referencing undefined locals (from DCE-removed blocks)
/// 4. Collapses single-element Phis to direct references
/// 5. Reports empty Phis (for caller to handle undefined local issues)
///
/// If `defined_locals` is provided, branches referencing locals not in the set will be removed.
/// This is useful after DCE removes blocks that defined certain locals.
pub fn cleanup_phis(cfg: &Cfg) -> PhiCleanupResult {
    cleanup_phis_with_defined_locals(cfg, None)
}

/// Clean up Phi nodes, also filtering out branches referencing undefined locals.
///
/// This is the same as `cleanup_phis` but also removes Phi branches that reference
/// locals not in the `defined_locals` set. Use this after DCE to clean up references
/// to locals that were defined in removed blocks.
pub fn cleanup_phis_with_defined_locals(
    cfg: &Cfg,
    defined_locals: Option<&FxHashSet<LocalId>>,
) -> PhiCleanupResult {
    let actual_predecessors = predecessors_for_phi_cleanup(cfg);

    let mut total_branches_removed = 0;
    let mut total_phis_collapsed = 0;
    let mut total_phis_emptied = 0;
    let mut all_collapsed_mappings: FxHashMap<LocalId, LocalId> = FxHashMap::default();

    // Entry block - has no predecessors in this context (Phis in entry would be weird)
    // But process it anyway for completeness
    let entry_preds = FxHashSet::default();
    let (new_entry, branches_removed, phis_collapsed, phis_emptied, collapsed_mappings) =
        cleanup_block_phis(&cfg.entry, &entry_preds, defined_locals);
    total_branches_removed += branches_removed;
    total_phis_collapsed += phis_collapsed;
    total_phis_emptied += phis_emptied;
    all_collapsed_mappings.extend(collapsed_mappings);

    // Named blocks
    let mut new_named: FxHashMap<Label, Block> = FxHashMap::default();
    for (label, block) in &cfg.named {
        let preds = actual_predecessors.get(label).cloned().unwrap_or_default();

        let (new_block, branches_removed, phis_collapsed, phis_emptied, collapsed_mappings) =
            cleanup_block_phis(block, &preds, defined_locals);
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

    #[test]
    fn test_no_change_when_predecessors_match() {
        // Entry branches to Block A, Block A branches to Block B
        // Block B has Phi referencing A - should remain valid (but collapse to single element)

        let entry = Block::new_for_test(
            vec![],
            (LocalId::from(99), Terminator::UnconditionalBranch { target: Label::from("block_a".to_string()) }),
        );

        let block_a = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::num_const(1))],
            (LocalId::from(1), Terminator::UnconditionalBranch { target: Label::from("block_b".to_string()) }),
        );

        let block_b = Block::new_for_test(
            vec![(
                LocalId::from(2),
                Instruction::phi(vec![(Label::from("block_a".to_string()), LocalId::from(0))]),
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

        let entry = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::bool_const(true))],
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
        let block_a = Block::new_for_test(
            vec![(LocalId::from(1), Instruction::num_const(1))],
            (LocalId::from(10), Terminator::Deopt { reason: "test".to_string() }),
        );

        // Block B still branches to join
        let block_b = Block::new_for_test(
            vec![(LocalId::from(2), Instruction::num_const(2))],
            (LocalId::from(11), Terminator::UnconditionalBranch { target: Label::from("join".to_string()) }),
        );

        // Join has Phi referencing both A and B
        let join = Block::new_for_test(
            vec![(
                LocalId::from(3),
                Instruction::phi(vec![
                    (Label::from("block_a".to_string()), LocalId::from(1)),
                    (Label::from("block_b".to_string()), LocalId::from(2)),
                ]),
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

        let entry = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::bool_const(true))],
            (
                LocalId::from(99),
                Terminator::ConditionalBranch {
                    condition: LocalId::from(0),
                    true_target: Label::from("block_a".to_string()),
                    false_target: Label::from("block_b".to_string()),
                },
            ),
        );

        let block_a = Block::new_for_test(
            vec![(LocalId::from(1), Instruction::num_const(1))],
            (LocalId::from(10), Terminator::Deopt { reason: "a".to_string() }),
        );

        let block_b = Block::new_for_test(
            vec![(LocalId::from(2), Instruction::num_const(2))],
            (LocalId::from(11), Terminator::Deopt { reason: "b".to_string() }),
        );

        let join = Block::new_for_test(
            vec![(
                LocalId::from(3),
                Instruction::phi(vec![
                    (Label::from("block_a".to_string()), LocalId::from(1)),
                    (Label::from("block_b".to_string()), LocalId::from(2)),
                ]),
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

        let entry = Block::new_for_test(
            vec![],
            (LocalId::from(99), Terminator::UnconditionalBranch { target: Label::from("dispatch".to_string()) }),
        );

        let dispatch = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::bool_const(true))],
            (
                LocalId::from(98),
                Terminator::ConditionalBranch {
                    condition: LocalId::from(0),
                    true_target: Label::from("block_a".to_string()),
                    false_target: Label::from("block_b".to_string()),
                },
            ),
        );

        let block_a = Block::new_for_test(
            vec![(LocalId::from(1), Instruction::num_const(1))],
            (LocalId::from(10), Terminator::Deopt { reason: "a".to_string() }), // Deopt'd!
        );

        let block_b = Block::new_for_test(
            vec![(LocalId::from(2), Instruction::num_const(2))],
            (
                LocalId::from(11),
                Terminator::ConditionalBranch {
                    condition: LocalId::from(2),
                    true_target: Label::from("block_c".to_string()),
                    false_target: Label::from("join".to_string()),
                },
            ),
        );

        let block_c = Block::new_for_test(
            vec![(LocalId::from(3), Instruction::num_const(3))],
            (LocalId::from(12), Terminator::UnconditionalBranch { target: Label::from("join".to_string()) }),
        );

        let join = Block::new_for_test(
            vec![(
                LocalId::from(4),
                Instruction::phi(vec![
                    (Label::from("block_a".to_string()), LocalId::from(1)),
                    (Label::from("block_b".to_string()), LocalId::from(2)),
                    (Label::from("block_c".to_string()), LocalId::from(3)),
                ]),
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

        let entry = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::num_const(1))],
            (LocalId::from(99), Terminator::UnconditionalBranch { target: Label::from("block_b".to_string()) }),
        );

        let block_b = Block::new_for_test(
            vec![
                (
                    LocalId::from(1),
                    Instruction::phi(vec![(Label::entry(), LocalId::from(0))]),
                ),
                (LocalId::from(2), Instruction::num_const(2)),
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

        let entry = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::num_const(0))],
            (LocalId::from(99), Terminator::UnconditionalBranch { target: Label::from("block_a".to_string()) }),
        );

        let block_a = Block::new_for_test(
            vec![(LocalId::from(1), Instruction::num_const(1))],
            (LocalId::from(98), Terminator::UnconditionalBranch { target: Label::from("block_b".to_string()) }),
        );

        // block_b has Phi from block_a only -> collapses to %1
        let block_b = Block::new_for_test(
            vec![(
                LocalId::from(2),
                Instruction::phi(vec![(Label::from("block_a".to_string()), LocalId::from(1))]),
            )],
            (LocalId::from(97), Terminator::UnconditionalBranch { target: Label::from("block_c".to_string()) }),
        );

        // block_c has Phi from block_b only -> collapses to %2
        // But %2 is also collapsed! So transitively, this should become %1
        let block_c = Block::new_for_test(
            vec![
                (
                    LocalId::from(3),
                    Instruction::phi(vec![(Label::from("block_b".to_string()), LocalId::from(2))]),
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

    #[test]
    fn test_remove_phi_branch_referencing_undefined_local() {
        // Simulates what happens after DCE removes a block:
        // - Entry conditionally branches to A and B
        // - Both A and B branch to join
        // - Join has Phi(A: %1, B: %2)
        // - After DCE removes block A (and %1 with it), we call cleanup_phis_with_defined_locals
        //   with defined_locals = {%0, %2} (not including %1)
        // - The Phi should have the A branch removed, collapsing to just %2

        let entry = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::bool_const(true))],
            (
                LocalId::from(99),
                Terminator::ConditionalBranch {
                    condition: LocalId::from(0),
                    true_target: Label::from("block_a".to_string()),
                    false_target: Label::from("block_b".to_string()),
                },
            ),
        );

        // Block A still exists in this test CFG (predecessor is valid),
        // but we'll tell cleanup that %1 is not defined
        let block_a = Block::new_for_test(
            vec![(LocalId::from(1), Instruction::num_const(1))],
            (LocalId::from(10), Terminator::UnconditionalBranch { target: Label::from("join".to_string()) }),
        );

        let block_b = Block::new_for_test(
            vec![(LocalId::from(2), Instruction::num_const(2))],
            (LocalId::from(11), Terminator::UnconditionalBranch { target: Label::from("join".to_string()) }),
        );

        let join = Block::new_for_test(
            vec![(
                LocalId::from(3),
                Instruction::phi(vec![
                    (Label::from("block_a".to_string()), LocalId::from(1)),
                    (Label::from("block_b".to_string()), LocalId::from(2)),
                ]),
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

        // Define locals except %1 (simulating that block_a was removed by DCE
        // but the CFG still has it for this test - what matters is the defined_locals set)
        let mut defined_locals: FxHashSet<LocalId> = FxHashSet::default();
        defined_locals.insert(LocalId::from(0));  // from entry
        defined_locals.insert(LocalId::from(2));  // from block_b
        defined_locals.insert(LocalId::from(99)); // terminator id
        defined_locals.insert(LocalId::from(10)); // terminator id
        defined_locals.insert(LocalId::from(11)); // terminator id
        defined_locals.insert(LocalId::from(12)); // terminator id
        // Note: %1 is NOT included - simulating it was defined in a removed block

        let result = cleanup_phis_with_defined_locals(&cfg, Some(&defined_locals));

        // Branch referencing %1 should be removed (undefined local)
        assert_eq!(result.branches_removed, 1);
        // Phi now has one element, so should be collapsed
        assert_eq!(result.phis_collapsed, 1);
        assert_eq!(result.phis_emptied, 0);

        // %3 should be remapped to %2
        assert_eq!(result.collapsed_mappings.get(&LocalId::from(3)), Some(&LocalId::from(2)));

        // The return should now reference %2 directly
        match result.cfg.named.get(&Label::from("join".to_string())).unwrap().terminator_kind() {
            Terminator::Return { value: Some(v) } => assert_eq!(*v, LocalId::from(2)),
            _ => panic!("Expected return terminator"),
        }
    }

    #[test]
    fn test_all_phi_branches_reference_undefined_locals() {
        // Test what happens when ALL Phi branches reference undefined locals
        // The Phi should become empty (phis_emptied = 1)

        let entry = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::bool_const(true))],
            (
                LocalId::from(99),
                Terminator::ConditionalBranch {
                    condition: LocalId::from(0),
                    true_target: Label::from("block_a".to_string()),
                    false_target: Label::from("block_b".to_string()),
                },
            ),
        );

        let block_a = Block::new_for_test(
            vec![(LocalId::from(1), Instruction::num_const(1))],
            (LocalId::from(10), Terminator::UnconditionalBranch { target: Label::from("join".to_string()) }),
        );

        let block_b = Block::new_for_test(
            vec![(LocalId::from(2), Instruction::num_const(2))],
            (LocalId::from(11), Terminator::UnconditionalBranch { target: Label::from("join".to_string()) }),
        );

        let join = Block::new_for_test(
            vec![(
                LocalId::from(3),
                Instruction::phi(vec![
                    (Label::from("block_a".to_string()), LocalId::from(1)),
                    (Label::from("block_b".to_string()), LocalId::from(2)),
                ]),
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

        // Define only entry locals - neither %1 nor %2 are defined
        let mut defined_locals: FxHashSet<LocalId> = FxHashSet::default();
        defined_locals.insert(LocalId::from(0));
        defined_locals.insert(LocalId::from(99));
        defined_locals.insert(LocalId::from(10));
        defined_locals.insert(LocalId::from(11));
        defined_locals.insert(LocalId::from(12));
        // Note: neither %1 nor %2 is included

        let result = cleanup_phis_with_defined_locals(&cfg, Some(&defined_locals));

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

    // ==================== Unit tests for transitive_closure ====================

    #[test]
    fn test_transitive_closure_empty() {
        let mappings: FxHashMap<LocalId, LocalId> = FxHashMap::default();
        let result = transitive_closure(&mappings);
        assert!(result.is_empty());
    }

    #[test]
    fn test_transitive_closure_single_mapping() {
        let mut mappings: FxHashMap<LocalId, LocalId> = FxHashMap::default();
        mappings.insert(LocalId::from(1), LocalId::from(0));

        let result = transitive_closure(&mappings);

        assert_eq!(result.len(), 1);
        assert_eq!(result.get(&LocalId::from(1)), Some(&LocalId::from(0)));
    }

    #[test]
    fn test_transitive_closure_chain_of_three() {
        // A -> B, B -> C should give A -> C, B -> C
        let mut mappings: FxHashMap<LocalId, LocalId> = FxHashMap::default();
        mappings.insert(LocalId::from(2), LocalId::from(1)); // %2 -> %1
        mappings.insert(LocalId::from(1), LocalId::from(0)); // %1 -> %0

        let result = transitive_closure(&mappings);

        assert_eq!(result.len(), 2);
        assert_eq!(result.get(&LocalId::from(2)), Some(&LocalId::from(0))); // %2 -> %0 (transitive)
        assert_eq!(result.get(&LocalId::from(1)), Some(&LocalId::from(0))); // %1 -> %0
    }

    #[test]
    fn test_transitive_closure_deep_chain() {
        // Test a chain of 6 mappings: %5 -> %4 -> %3 -> %2 -> %1 -> %0
        let mut mappings: FxHashMap<LocalId, LocalId> = FxHashMap::default();
        mappings.insert(LocalId::from(5), LocalId::from(4));
        mappings.insert(LocalId::from(4), LocalId::from(3));
        mappings.insert(LocalId::from(3), LocalId::from(2));
        mappings.insert(LocalId::from(2), LocalId::from(1));
        mappings.insert(LocalId::from(1), LocalId::from(0));

        let result = transitive_closure(&mappings);

        assert_eq!(result.len(), 5);
        // All should resolve to %0
        assert_eq!(result.get(&LocalId::from(5)), Some(&LocalId::from(0)));
        assert_eq!(result.get(&LocalId::from(4)), Some(&LocalId::from(0)));
        assert_eq!(result.get(&LocalId::from(3)), Some(&LocalId::from(0)));
        assert_eq!(result.get(&LocalId::from(2)), Some(&LocalId::from(0)));
        assert_eq!(result.get(&LocalId::from(1)), Some(&LocalId::from(0)));
    }

    #[test]
    fn test_transitive_closure_self_reference() {
        // Edge case: A -> A (self-reference)
        // This shouldn't happen in practice but the code should handle it without infinite loop
        let mut mappings: FxHashMap<LocalId, LocalId> = FxHashMap::default();
        mappings.insert(LocalId::from(0), LocalId::from(0)); // %0 -> %0

        let result = transitive_closure(&mappings);

        // Should remain unchanged - self-reference stays as is
        assert_eq!(result.len(), 1);
        assert_eq!(result.get(&LocalId::from(0)), Some(&LocalId::from(0)));
    }

    #[test]
    fn test_transitive_closure_two_element_cycle() {
        // Edge case: A -> B, B -> A (two-element cycle)
        // This is pathological but the code should not infinite loop
        let mut mappings: FxHashMap<LocalId, LocalId> = FxHashMap::default();
        mappings.insert(LocalId::from(0), LocalId::from(1)); // %0 -> %1
        mappings.insert(LocalId::from(1), LocalId::from(0)); // %1 -> %0

        let result = transitive_closure(&mappings);

        // The algorithm will follow %0 -> %1 -> %0 and stop at the self-reference check
        // Both should stabilize (the exact result depends on iteration order, but shouldn't loop)
        assert_eq!(result.len(), 2);
        // Since the cycle is detected, mappings remain at their immediate targets or cycle back
        // The key point is this doesn't infinite loop
    }

    #[test]
    fn test_transitive_closure_independent_chains() {
        // Two independent chains: %3 -> %2 -> %1 and %6 -> %5 -> %4
        let mut mappings: FxHashMap<LocalId, LocalId> = FxHashMap::default();
        // Chain 1
        mappings.insert(LocalId::from(3), LocalId::from(2));
        mappings.insert(LocalId::from(2), LocalId::from(1));
        // Chain 2
        mappings.insert(LocalId::from(6), LocalId::from(5));
        mappings.insert(LocalId::from(5), LocalId::from(4));

        let result = transitive_closure(&mappings);

        assert_eq!(result.len(), 4);
        // Chain 1 resolves to %1
        assert_eq!(result.get(&LocalId::from(3)), Some(&LocalId::from(1)));
        assert_eq!(result.get(&LocalId::from(2)), Some(&LocalId::from(1)));
        // Chain 2 resolves to %4
        assert_eq!(result.get(&LocalId::from(6)), Some(&LocalId::from(4)));
        assert_eq!(result.get(&LocalId::from(5)), Some(&LocalId::from(4)));
    }

    #[test]
    fn test_transitive_closure_diamond() {
        // Diamond pattern: %3 -> %1, %4 -> %2, both %1 and %2 -> %0
        // This tests that multiple paths to the same target work correctly
        let mut mappings: FxHashMap<LocalId, LocalId> = FxHashMap::default();
        mappings.insert(LocalId::from(3), LocalId::from(1));
        mappings.insert(LocalId::from(4), LocalId::from(2));
        mappings.insert(LocalId::from(1), LocalId::from(0));
        mappings.insert(LocalId::from(2), LocalId::from(0));

        let result = transitive_closure(&mappings);

        assert_eq!(result.len(), 4);
        // All should resolve to %0
        assert_eq!(result.get(&LocalId::from(3)), Some(&LocalId::from(0)));
        assert_eq!(result.get(&LocalId::from(4)), Some(&LocalId::from(0)));
        assert_eq!(result.get(&LocalId::from(1)), Some(&LocalId::from(0)));
        assert_eq!(result.get(&LocalId::from(2)), Some(&LocalId::from(0)));
    }
}
