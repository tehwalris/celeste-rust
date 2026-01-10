//! Block coalescing pass: Merge straight-line blocks.
//!
//! This pass identifies blocks with unconditional branches to blocks that have
//! only one predecessor, and merges them together. This reduces control flow
//! overhead and enables further optimizations.

use crate::interpreter::common::FxHashMap;
use crate::ir::{Block, BlockId, Cfg, Instruction, Label, LocalId, Terminator};

/// Result of the block coalescing pass
#[derive(Debug)]
pub enum CoalesceResult {
    /// Successfully coalesced blocks
    Success {
        cfg: Cfg,
        /// Number of blocks removed
        blocks_removed: usize,
    },
    /// No blocks to coalesce
    NoChange,
}

/// Run the block coalescing pass on a CFG.
///
/// This merges blocks where a block with an unconditional branch
/// targets a block with only one predecessor.
pub fn coalesce_blocks(cfg: &Cfg) -> CoalesceResult {
    // Compute predecessors
    let preds = predecessors_for_coalesce(cfg);

    // Find which blocks can be coalesced (have exactly one predecessor via unconditional branch)
    let mut coalesce_targets: FxHashMap<Label, BlockSource> = FxHashMap::default();

    // Check entry block
    if let Terminator::UnconditionalBranch { target } = cfg.entry.terminator_kind() {
        if has_single_predecessor(&preds, target) {
            coalesce_targets.insert(target.clone(), BlockSource::Entry);
        }
    }

    // Check named blocks
    for (label, block) in &cfg.named {
        if let Terminator::UnconditionalBranch { target } = block.terminator_kind() {
            if has_single_predecessor(&preds, target) && !coalesce_targets.contains_key(target) {
                coalesce_targets.insert(target.clone(), BlockSource::Named(label.clone()));
            }
        }
    }

    if coalesce_targets.is_empty() {
        return CoalesceResult::NoChange;
    }

    // Build the new CFG
    let mut new_cfg = cfg.clone();
    let mut blocks_removed = 0;

    // Keep merging until no more merges are possible
    loop {
        let preds = predecessors_for_coalesce(&new_cfg);
        let mut merged = false;

        // Try to merge entry block with its target
        if let Terminator::UnconditionalBranch { target } = new_cfg.entry.terminator_kind() {
            if has_single_predecessor(&preds, target) {
                let target_label = target.clone();
                if let Some(target_block) = new_cfg.named.remove(&target_label) {
                    // Merge: append target's instructions and terminator to entry
                    // Source is entry block (None for source_label)
                    let phi_replacements = merge_blocks(&mut new_cfg.entry, &target_block, &target_label, None);
                    // Apply phi replacements globally (resolved phis need to update all uses)
                    apply_phi_replacements_globally(&mut new_cfg, &phi_replacements);
                    // Update phis that reference target_label to use the entry block label
                    update_phi_nodes_for_removed_block(&mut new_cfg, &target_label, Some(&Label::entry()));
                    blocks_removed += 1;
                    merged = true;
                }
            }
        }

        // Try to merge named blocks
        if !merged {
            let labels: Vec<_> = new_cfg.named.keys().cloned().collect();
            for label in labels {
                let block = new_cfg.named.get(&label).unwrap();
                if let Terminator::UnconditionalBranch { target } = block.terminator_kind() {
                    let target_label = target.clone();
                    if has_single_predecessor(&preds, &target_label) && target_label != label {
                        if let Some(target_block) = new_cfg.named.remove(&target_label) {
                            // Merge target into source
                            let source_block = new_cfg.named.get_mut(&label).unwrap();
                            let phi_replacements = merge_blocks(source_block, &target_block, &target_label, Some(&label));
                            // Apply phi replacements globally (resolved phis need to update all uses)
                            apply_phi_replacements_globally(&mut new_cfg, &phi_replacements);
                            // Update phi nodes that reference target_label to use source label instead
                            update_phi_nodes_for_removed_block(&mut new_cfg, &target_label, Some(&label));
                            blocks_removed += 1;
                            merged = true;
                            break;
                        }
                    }
                }
            }
        }

        if !merged {
            break;
        }
    }

    if blocks_removed == 0 {
        return CoalesceResult::NoChange;
    }

    CoalesceResult::Success {
        cfg: new_cfg,
        blocks_removed,
    }
}

/// Update phi nodes in the CFG when a block is removed.
/// Replaces references to `old_label` with `new_label` in all phi instructions.
/// If `new_label` is None, removes the phi branches entirely.
fn update_phi_nodes_for_removed_block(cfg: &mut Cfg, old_label: &Label, new_label: Option<&Label>) {
    // Update entry block
    update_block_phi_nodes(&mut cfg.entry, old_label, new_label);

    // Update named blocks
    for (_, block) in cfg.named.iter_mut() {
        update_block_phi_nodes(block, old_label, new_label);
    }
}

/// Update phi nodes in a single block
fn update_block_phi_nodes(block: &mut Block, old_label: &Label, new_label: Option<&Label>) {
    for (_, instruction) in &mut block.instructions {
        if let Instruction::Phi { branches } = instruction {
            // Update branches that reference old_label
            if let Some(new_label) = new_label {
                // Replace old_label with new_label
                for (branch_label, _) in branches.iter_mut() {
                    if branch_label == old_label {
                        *branch_label = new_label.clone();
                    }
                }
            } else {
                // Remove branches referencing old_label (entry block case)
                branches.retain(|(branch_label, _)| branch_label != old_label);
            }
        }
    }
}

/// Source of a coalesce operation
#[derive(Clone, Debug)]
enum BlockSource {
    Entry,
    Named(Label),
}

/// Apply local ID replacements across the entire CFG.
/// This is used when phis are resolved during block merging - the resolved
/// phi's local ID needs to be replaced with its value throughout the CFG.
fn apply_phi_replacements_globally(cfg: &mut Cfg, replacements: &FxHashMap<LocalId, LocalId>) {
    if replacements.is_empty() {
        return;
    }

    let apply = |id: LocalId| replacements.get(&id).copied().unwrap_or(id);

    // Update entry block
    for (_, instr) in &mut cfg.entry.instructions {
        *instr = instr.map_local_ids(&apply);
    }
    let (term_id, term) = &cfg.entry.terminator;
    cfg.entry.terminator = (*term_id, term.map_local_ids(&apply));

    // Update named blocks
    for (_, block) in cfg.named.iter_mut() {
        for (_, instr) in &mut block.instructions {
            *instr = instr.map_local_ids(&apply);
        }
        let (term_id, term) = &block.terminator;
        block.terminator = (*term_id, term.map_local_ids(&apply));
    }
}

/// Merge the target block into the source block.
/// Updates phi nodes to remove references to the target label.
/// `source_label` is the label of the source block (None for entry block).
/// Returns a map of phi replacements that should be applied globally.
fn merge_blocks(source: &mut Block, target: &Block, _target_label: &Label, source_label: Option<&Label>) -> FxHashMap<LocalId, LocalId> {
    use crate::ir::ENTRY_BLOCK_LABEL;

    // Remove phi nodes from target - they reference the source which is now the same block
    // After coalescing, we don't need phi nodes for branches from the source block
    let (target_phis, target_non_phis) = target.split_block_phi_instructions();

    // The source block's label as it appears in phi nodes
    // Entry block uses ENTRY_BLOCK_LABEL in phi nodes
    let source_ref = source_label
        .map(|l| l.as_str().to_string())
        .unwrap_or_else(|| ENTRY_BLOCK_LABEL.to_string());

    // For each phi in target, we need to resolve it to the value from source
    // Since source is the only predecessor, we find the branch matching source's label
    let mut phi_replacements: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    for (target_id, instr) in target_phis {
        if let Instruction::Phi { branches } = instr {
            // Find the branch that matches the source block's label
            for (label, value) in branches {
                if label.as_str() == source_ref {
                    phi_replacements.insert(*target_id, *value);
                    break;
                }
            }
        }
    }

    // Append target's non-phi instructions (with phi replacements applied)
    for (target_id, instr) in target_non_phis {
        let rewritten = instr.map_local_ids(|id| {
            phi_replacements.get(&id).copied().unwrap_or(id)
        });
        source.instructions.push((*target_id, rewritten));
    }

    // Replace source's terminator with target's terminator (with phi replacements applied)
    let (term_id, term) = &target.terminator;
    let new_term = term.map_local_ids(|id| {
        phi_replacements.get(&id).copied().unwrap_or(id)
    });
    source.terminator = (*term_id, new_term);

    // Preserve hint_normalize if either block has it
    source.hint_normalize = source.hint_normalize || target.hint_normalize;

    phi_replacements
}

/// Convert centralized predecessor map to the format used by block coalescing.
///
/// Returns a map from named block labels to their predecessor count.
/// Entry block is not included (it has no predecessors).
fn predecessors_for_coalesce(cfg: &Cfg) -> FxHashMap<Label, usize> {
    let block_preds = cfg.compute_predecessors();
    let mut result: FxHashMap<Label, usize> = FxHashMap::default();

    for (block_id, preds) in block_preds {
        if let BlockId::Named(label) = block_id {
            result.insert(label, preds.len());
        }
    }

    result
}

/// Check if a block has exactly one predecessor
fn has_single_predecessor(preds: &FxHashMap<Label, usize>, label: &Label) -> bool {
    preds.get(label).map(|&count| count == 1).unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::FxHashMap;
    use crate::ir::{Block, Cfg, Instruction, LocalId, Terminator};
    use crate::pico8_num::Pico8Num;

    #[test]
    fn test_simple_coalesce() {
        // Entry: const 5, branch to block_a
        // block_a: const 10, return
        // Should become: Entry: const 5, const 10, return

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::NumberConstant { value: Pico8Num::from_i16(5) }),
            ],
            (
                LocalId::from(1),
                Terminator::UnconditionalBranch { target: Label::from("block_a".to_string()) },
            ),
        );

        let block_a = Block::new_for_test(
            vec![
                (LocalId::from(2), Instruction::NumberConstant { value: Pico8Num::from_i16(10) }),
            ],
            (LocalId::from(3), Terminator::Return { value: Some(LocalId::from(2)) }),
        );

        let mut named: FxHashMap<Label, Block> = FxHashMap::default();
        named.insert(Label::from("block_a".to_string()), block_a);

        let cfg = Cfg { entry, named };

        let result = coalesce_blocks(&cfg);

        match result {
            CoalesceResult::Success { cfg: new_cfg, blocks_removed } => {
                assert_eq!(blocks_removed, 1);
                assert!(new_cfg.named.is_empty());
                assert_eq!(new_cfg.entry.instructions.len(), 2);
                assert!(matches!(new_cfg.entry.terminator_kind(), Terminator::Return { .. }));
            }
            CoalesceResult::NoChange => panic!("Expected coalescing to occur"),
        }
    }

    #[test]
    fn test_no_coalesce_multiple_preds() {
        // Entry: cond branch to block_a (true) or block_a (false)
        // block_a: return
        // Should NOT coalesce (block_a has 2 predecessors)

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::BoolConstant { value: true }),
            ],
            (
                LocalId::from(1),
                Terminator::ConditionalBranch {
                    condition: LocalId::from(0),
                    true_target: Label::from("block_a".to_string()),
                    false_target: Label::from("block_a".to_string()),
                },
            ),
        );

        let block_a = Block::new_for_test(
            vec![],
            (LocalId::from(2), Terminator::Return { value: None }),
        );

        let mut named: FxHashMap<Label, Block> = FxHashMap::default();
        named.insert(Label::from("block_a".to_string()), block_a);

        let cfg = Cfg { entry, named };

        let result = coalesce_blocks(&cfg);
        assert!(matches!(result, CoalesceResult::NoChange));
    }

    #[test]
    fn test_chain_coalesce() {
        // Entry -> block_a -> block_b -> return
        // Should coalesce all into entry

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::NumberConstant { value: Pico8Num::from_i16(1) }),
            ],
            (
                LocalId::from(1),
                Terminator::UnconditionalBranch { target: Label::from("block_a".to_string()) },
            ),
        );

        let block_a = Block::new_for_test(
            vec![
                (LocalId::from(2), Instruction::NumberConstant { value: Pico8Num::from_i16(2) }),
            ],
            (
                LocalId::from(3),
                Terminator::UnconditionalBranch { target: Label::from("block_b".to_string()) },
            ),
        );

        let block_b = Block::new_for_test(
            vec![
                (LocalId::from(4), Instruction::NumberConstant { value: Pico8Num::from_i16(3) }),
            ],
            (LocalId::from(5), Terminator::Return { value: Some(LocalId::from(4)) }),
        );

        let mut named: FxHashMap<Label, Block> = FxHashMap::default();
        named.insert(Label::from("block_a".to_string()), block_a);
        named.insert(Label::from("block_b".to_string()), block_b);

        let cfg = Cfg { entry, named };

        let result = coalesce_blocks(&cfg);

        match result {
            CoalesceResult::Success { cfg: new_cfg, blocks_removed } => {
                assert_eq!(blocks_removed, 2);
                assert!(new_cfg.named.is_empty());
                assert_eq!(new_cfg.entry.instructions.len(), 3);
            }
            CoalesceResult::NoChange => panic!("Expected coalescing to occur"),
        }
    }
}
