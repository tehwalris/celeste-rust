//! Dead Code Elimination (DCE) pass.
//!
//! This pass removes instructions whose results are never used and have no side effects.
//! After call resolution and inlining, we often have dead `GetGlobal` and `Load`
//! instructions that were only used to set up the original Call.

use std::collections::{HashMap, HashSet};
use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

use crate::ir::{Block, Cfg, Instruction, Label, LocalId, Terminator};

type FxHashMap<K, V> = HashMap<K, V, BuildHasherDefault<FxHasher>>;
type FxHashSet<T> = HashSet<T, BuildHasherDefault<FxHasher>>;

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

/// Get all local IDs used by an instruction.
fn get_used_locals(instruction: &Instruction) -> Vec<LocalId> {
    let mut result = Vec::new();
    match instruction {
        Instruction::Alloc => {}
        Instruction::GetGlobal { .. } => {}
        Instruction::Load { source } => {
            result.push(*source);
        }
        Instruction::Store { target, source } => {
            result.push(*target);
            result.push(*source);
        }
        Instruction::StoreEmptyTable { target } => {
            result.push(*target);
        }
        Instruction::StoreClosure { target, captures, .. } => {
            result.push(*target);
            result.extend(captures.iter().copied());
        }
        Instruction::GetField { receiver, .. } => {
            result.push(*receiver);
        }
        Instruction::GetIndex { receiver, index, .. } => {
            result.push(*receiver);
            result.push(*index);
        }
        Instruction::NumberConstant { .. } => {}
        Instruction::BoolConstant { .. } => {}
        Instruction::StringConstant { .. } => {}
        Instruction::NilConstant => {}
        Instruction::BinaryOp { left, right, .. } => {
            result.push(*left);
            result.push(*right);
        }
        Instruction::UnaryOp { arg, .. } => {
            result.push(*arg);
        }
        Instruction::Call { closure, args } => {
            result.push(*closure);
            result.extend(args.iter().copied());
        }
        Instruction::CallResolved { captures, args, .. } => {
            result.extend(captures.iter().copied());
            result.extend(args.iter().copied());
        }
        Instruction::CallBuiltin { args, .. } => {
            result.extend(args.iter().copied());
        }
        Instruction::Phi { branches } => {
            for (_, local) in branches {
                result.push(*local);
            }
        }
    }
    result
}

/// Get all local IDs used by a terminator.
fn get_terminator_used_locals(terminator: &Terminator) -> Vec<LocalId> {
    match terminator {
        Terminator::Return { value: Some(v) } => vec![*v],
        Terminator::Return { value: None } => vec![],
        Terminator::UnconditionalBranch { .. } => vec![],
        Terminator::ConditionalBranch { condition, .. } => vec![*condition],
    }
}

/// Run dead code elimination on a CFG.
///
/// This removes instructions that:
/// 1. Have no side effects
/// 2. Their results are never used
pub fn eliminate_dead_code(cfg: &Cfg) -> DceResult {
    // Step 1: Build use sets for each local
    let mut used_locals: FxHashSet<LocalId> = FxHashSet::default();

    // Collect all used locals from all blocks
    fn collect_block_uses(
        block: &Block,
        used_locals: &mut FxHashSet<LocalId>,
    ) {
        for (_, instruction) in &block.instructions {
            // Side-effectful instructions - mark all their operands as used
            if has_side_effects(instruction) {
                for local in get_used_locals(instruction) {
                    used_locals.insert(local);
                }
            }
        }

        // Terminator uses
        for local in get_terminator_used_locals(&block.terminator.1) {
            used_locals.insert(local);
        }
    }

    collect_block_uses(&cfg.entry, &mut used_locals);
    for block in cfg.named.values() {
        collect_block_uses(block, &mut used_locals);
    }

    // Step 2: Propagate uses backwards through definitions
    // If a local is used, then all locals used in its definition are also used
    let mut all_instructions: Vec<(LocalId, &Instruction)> = Vec::new();
    for (local_id, instruction) in &cfg.entry.instructions {
        all_instructions.push((*local_id, instruction));
    }
    for block in cfg.named.values() {
        for (local_id, instruction) in &block.instructions {
            all_instructions.push((*local_id, instruction));
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
                for dep_local in get_used_locals(instruction) {
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

        Block {
            instructions: new_instructions,
            terminator: block.terminator.clone(),
            hint_normalize: block.hint_normalize,
        }
    }

    let new_entry = filter_block(&cfg.entry, &used_locals, &mut total_removed);

    let mut new_named: FxHashMap<Label, Block> = FxHashMap::default();
    for (label, block) in &cfg.named {
        let new_block = filter_block(block, &used_locals, &mut total_removed);
        new_named.insert(label.clone(), new_block);
    }

    if total_removed == 0 {
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
    use crate::pico8_num::Pico8Num;

    #[test]
    fn test_removes_unused_get_global() {
        // Create a CFG with:
        // %0 = GetGlobal(foo)       <- dead
        // %1 = NumberConstant(42)   <- used
        // return %1

        let entry = Block {
            instructions: vec![
                (
                    LocalId::from(0),
                    Instruction::GetGlobal {
                        name: "foo".to_string(),
                        create_if_missing: false,
                    },
                ),
                (
                    LocalId::from(1),
                    Instruction::NumberConstant {
                        value: Pico8Num::from_i16(42),
                    },
                ),
            ],
            terminator: (
                LocalId::from(2),
                Terminator::Return {
                    value: Some(LocalId::from(1)),
                },
            ),
            hint_normalize: false,
        };

        let cfg = Cfg {
            entry,
            named: Default::default(),
        };

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

        let entry = Block {
            instructions: vec![
                (
                    LocalId::from(0),
                    Instruction::GetGlobal {
                        name: "foo".to_string(),
                        create_if_missing: false,
                    },
                ),
                (
                    LocalId::from(1),
                    Instruction::Load {
                        source: LocalId::from(0),
                    },
                ),
            ],
            terminator: (
                LocalId::from(2),
                Terminator::Return {
                    value: Some(LocalId::from(1)),
                },
            ),
            hint_normalize: false,
        };

        let cfg = Cfg {
            entry,
            named: Default::default(),
        };

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

        let entry = Block {
            instructions: vec![
                (
                    LocalId::from(0),
                    Instruction::GetGlobal {
                        name: "foo".to_string(),
                        create_if_missing: false,
                    },
                ),
                (
                    LocalId::from(1),
                    Instruction::Load {
                        source: LocalId::from(0),
                    },
                ),
                (
                    LocalId::from(2),
                    Instruction::NumberConstant {
                        value: Pico8Num::from_i16(42),
                    },
                ),
                (
                    LocalId::from(3),
                    Instruction::CallResolved {
                        fun_name: "foo_1".to_string().into(),
                        captures: vec![],
                        args: vec![LocalId::from(2)],
                    },
                ),
            ],
            terminator: (
                LocalId::from(4),
                Terminator::Return {
                    value: Some(LocalId::from(3)),
                },
            ),
            hint_normalize: false,
        };

        let cfg = Cfg {
            entry,
            named: Default::default(),
        };

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
