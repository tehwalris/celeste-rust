//! Dead Code Elimination (DCE) pass.
//!
//! This pass removes instructions whose results are never used and have no side effects.
//! After call resolution and inlining, we often have dead `GetGlobal` and `Load`
//! instructions that were only used to set up the original Call.

use crate::interpreter::common::{FxHashMap, FxHashSet};
use crate::ir::{Block, Cfg, Instruction, Label, LocalId};

/// Result of the DCE pass.
#[derive(Debug)]
pub enum DceResult {
    /// Successfully eliminated some dead code
    Success {
        cfg: Cfg,
        /// Number of instructions removed
        instructions_removed: usize,
    },
    /// No dead code found
    NoChange,
}

/// Check if an instruction has side effects (and therefore can't be eliminated even if unused).
fn has_side_effects(instruction: &Instruction) -> bool {
    match instruction {
        // These have side effects - writes to memory or function calls
        Instruction::Store { .. } => true,
        Instruction::StoreEmptyTable { .. } => true,
        Instruction::StoreClosure { .. } => true,
        Instruction::Call { .. } => true,
        Instruction::CallResolved { .. } => true,
        Instruction::CallBuiltin { .. } => true,

        // These are pure - they only compute a value
        Instruction::Alloc => false, // Pure if not stored to
        Instruction::GetGlobal { .. } => false, // Just gets a pointer
        Instruction::Load { .. } => false, // Just reads
        Instruction::GetField { .. } => false, // Just reads
        Instruction::GetIndex { .. } => false, // Just reads
        Instruction::NumberConstant { .. } => false,
        Instruction::BoolConstant { .. } => false,
        Instruction::StringConstant { .. } => false,
        Instruction::NilConstant => false,
        Instruction::BinaryOp { .. } => false,
        Instruction::UnaryOp { .. } => false,
        Instruction::Phi { .. } => false,
    }
}


/// Compute reachable blocks from entry via control flow.
/// Returns the set of reachable block labels (entry is always reachable).
fn compute_reachable_blocks(cfg: &Cfg) -> FxHashSet<Label> {
    let mut reachable: FxHashSet<Label> = FxHashSet::default();
    let mut worklist: Vec<&Label> = cfg
        .entry
        .terminator_kind()
        .get_successor_labels()
        .filter(|label| cfg.named.contains_key(*label))
        .collect();

    while let Some(label) = worklist.pop() {
        if reachable.contains(label) {
            continue;
        }
        reachable.insert(label.clone());

        if let Some(block) = cfg.named.get(label) {
            for succ in block.terminator_kind().get_successor_labels() {
                if cfg.named.contains_key(succ) {
                    worklist.push(succ);
                }
            }
        }
    }

    reachable
}

/// Run dead code elimination on a CFG.
///
/// This removes:
/// 1. Unreachable blocks (blocks not reachable from entry via control flow)
/// 2. Instructions that have no side effects and whose results are never used
pub fn eliminate_dead_code(cfg: &Cfg) -> DceResult {
    // Step 0: Compute reachable blocks
    let reachable_blocks = compute_reachable_blocks(cfg);

    // Step 1: Build use sets for each local (only from reachable blocks)
    let mut used_locals: FxHashSet<LocalId> = FxHashSet::default();

    // Collect all used locals from reachable blocks
    fn collect_block_uses(
        block: &Block,
        used_locals: &mut FxHashSet<LocalId>,
    ) {
        for (_, instruction) in &block.instructions {
            // Side-effectful instructions - mark all their operands as used
            if has_side_effects(instruction) {
                for local in instruction.get_used_locals() {
                    used_locals.insert(local);
                }
            }
        }

        // Terminator uses
        for local in block.terminator_kind().get_used_locals() {
            used_locals.insert(local);
        }
    }

    // Entry block is always reachable
    collect_block_uses(&cfg.entry, &mut used_locals);
    for (label, block) in &cfg.named {
        if reachable_blocks.contains(label) {
            collect_block_uses(block, &mut used_locals);
        }
    }

    // Step 2: Propagate uses backwards through definitions
    // If a local is used, then all locals used in its definition are also used
    // Only consider instructions from reachable blocks
    let mut all_instructions: Vec<(LocalId, &Instruction)> = Vec::new();
    for (local_id, instruction) in &cfg.entry.instructions {
        all_instructions.push((*local_id, instruction));
    }
    for (label, block) in &cfg.named {
        if reachable_blocks.contains(label) {
            for (local_id, instruction) in &block.instructions {
                all_instructions.push((*local_id, instruction));
            }
        }
    }

    // Build def-use chains
    let mut def_map: FxHashMap<LocalId, &Instruction> = FxHashMap::default();
    for (local_id, instruction) in &all_instructions {
        def_map.insert(*local_id, *instruction);
    }

    // Fixed-point iteration: if a local is used, mark all locals it depends on as used
    let mut changed = true;
    while changed {
        changed = false;
        for (local_id, instruction) in &all_instructions {
            if used_locals.contains(local_id) {
                for dep_local in instruction.get_used_locals() {
                    if !used_locals.contains(&dep_local) {
                        used_locals.insert(dep_local);
                        changed = true;
                    }
                }
            }
        }
    }

    // Step 3: Remove dead instructions
    let mut total_removed = 0;

    fn filter_block(
        block: &Block,
        used_locals: &FxHashSet<LocalId>,
        removed_count: &mut usize,
    ) -> Block {
        let mut new_instructions = Vec::new();

        for (local_id, instruction) in &block.instructions {
            // Keep if: has side effects OR its result is used
            if has_side_effects(instruction) || used_locals.contains(local_id) {
                new_instructions.push((*local_id, instruction.clone()));
            } else {
                *removed_count += 1;
            }
        }

        block.with_instructions(new_instructions)
    }

    let new_entry = filter_block(&cfg.entry, &used_locals, &mut total_removed);

    // Only include reachable blocks in output
    let mut new_named: FxHashMap<Label, Block> = FxHashMap::default();
    let mut blocks_removed = 0;
    for (label, block) in &cfg.named {
        if reachable_blocks.contains(label) {
            let new_block = filter_block(block, &used_locals, &mut total_removed);
            new_named.insert(label.clone(), new_block);
        } else {
            // Count instructions in removed block
            total_removed += block.instructions.len();
            blocks_removed += 1;
        }
    }

    if total_removed == 0 && blocks_removed == 0 {
        return DceResult::NoChange;
    }

    DceResult::Success {
        cfg: Cfg {
            entry: new_entry,
            named: new_named,
        },
        instructions_removed: total_removed,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, Instruction, LocalId, Terminator};

    #[test]
    fn test_removes_unused_get_global() {
        // Create a CFG with:
        // %0 = GetGlobal(foo)       <- dead
        // %1 = NumberConstant(42)   <- used
        // return %1

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("foo")),
                (LocalId::from(1), Instruction::num_const(42)),
            ],
            (LocalId::from(2), Terminator::ret(Some(LocalId::from(1)))),
        );

        let cfg = Cfg::single_entry(entry);

        let result = eliminate_dead_code(&cfg);
        match result {
            DceResult::Success { cfg, instructions_removed } => {
                assert_eq!(instructions_removed, 1);
                assert_eq!(cfg.entry.instructions.len(), 1);
                // Only the NumberConstant should remain
                assert!(matches!(
                    cfg.entry.instructions[0].1,
                    Instruction::NumberConstant { .. }
                ));
            }
            DceResult::NoChange => panic!("Expected dead code to be removed"),
        }
    }

    #[test]
    fn test_keeps_used_load_chain() {
        // Create a CFG with:
        // %0 = GetGlobal(foo)
        // %1 = Load(%0)
        // return %1
        //
        // All instructions should be kept because they form a chain to the return

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("foo")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
            ],
            (LocalId::from(2), Terminator::ret(Some(LocalId::from(1)))),
        );

        let cfg = Cfg::single_entry(entry);

        let result = eliminate_dead_code(&cfg);
        assert!(matches!(result, DceResult::NoChange));
    }

    #[test]
    fn test_removes_dead_load_chain_after_call_resolution() {
        // This simulates what happens after call resolution + inlining:
        // %0 = GetGlobal(foo)       <- dead after resolution
        // %1 = Load(%0)             <- dead after resolution
        // %2 = NumberConstant(42)   <- used (argument to call)
        // %3 = CallResolved(...)    <- uses %2 directly, not %1!
        // return %3

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("foo")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::num_const(42)),
                (
                    LocalId::from(3),
                    Instruction::CallResolved {
                        fun_name: "foo_1".to_string().into(),
                        captures: vec![],
                        args: vec![LocalId::from(2)],
                    },
                ),
            ],
            (
                LocalId::from(4),
                Terminator::Return {
                    value: Some(LocalId::from(3)),
                },
            ),
        );

        let cfg = Cfg::single_entry(entry);

        let result = eliminate_dead_code(&cfg);
        match result {
            DceResult::Success { cfg, instructions_removed } => {
                assert_eq!(instructions_removed, 2); // GetGlobal and Load
                assert_eq!(cfg.entry.instructions.len(), 2);
                // Should have NumberConstant and CallResolved
                assert!(matches!(
                    cfg.entry.instructions[0].1,
                    Instruction::NumberConstant { .. }
                ));
                assert!(matches!(
                    cfg.entry.instructions[1].1,
                    Instruction::CallResolved { .. }
                ));
            }
            DceResult::NoChange => panic!("Expected dead code to be removed"),
        }
    }
}
