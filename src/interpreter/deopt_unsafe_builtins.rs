//! Deoptimization pass for unsafe builtins.
//!
//! This pass runs after builtin resolution and replaces calls to unsafe builtins
//! with deoptimization points. Unsafe builtins are those that:
//! - Modify the heap (e.g., `add` which adds to tables)
//! - Have side effects that can't be undone (e.g., `__print`, `error`)
//!
//! After this pass, DCE can clean up code that only feeds into deopt points.

use crate::ir::{Block, Cfg, Instruction, Terminator};

/// Result of the deopt unsafe builtins pass.
#[derive(Debug)]
pub enum DeoptUnsafeBuiltinsResult {
    /// Successfully inserted deopts
    Success {
        cfg: Cfg,
        /// Number of deopt points inserted
        deopts_inserted: usize,
    },
    /// No unsafe builtins found
    NoChange,
}

/// Check if a builtin is unsafe (requires deopt).
fn is_unsafe_builtin(name: &str) -> bool {
    match name {
        // Table modification
        "add" | "del" | "deli" => true,
        // Output/errors
        "__print" | "error" | "print" | "printh" => true,
        // Array mutation
        "__array_table_drop_last" => true,
        // Everything else is considered safe for now
        _ => false,
    }
}

/// Process a block, inserting deopt if an unsafe builtin is found.
/// Returns (new_block, deopt_inserted).
fn process_block(block: &Block) -> (Block, bool) {
    let mut new_instructions = Vec::new();

    for (target_id, instruction) in &block.instructions {
        match instruction {
            Instruction::CallBuiltin { name, .. } if is_unsafe_builtin(name) => {
                // Found unsafe builtin - insert deopt here
                return (
                    Block {
                        instructions: new_instructions,
                        terminator: (
                            *target_id,
                            Terminator::Deopt {
                                reason: format!("Unsafe builtin: {}", name),
                            },
                        ),
                        hint_normalize: block.hint_normalize,
                    },
                    true,
                );
            }
            _ => {
                new_instructions.push((*target_id, instruction.clone()));
            }
        }
    }

    // No unsafe builtin found - return block unchanged
    (block.with_instructions(new_instructions), false)
}

/// Replace unsafe builtin calls with deoptimization points.
///
/// This pass scans for `CallBuiltin` instructions that call unsafe builtins
/// and replaces them with `Deopt` terminators.
pub fn deopt_unsafe_builtins(cfg: &Cfg) -> DeoptUnsafeBuiltinsResult {
    let mut deopts_inserted = 0;

    // Process entry block
    let (new_entry, entry_deopt) = process_block(&cfg.entry);
    if entry_deopt {
        deopts_inserted += 1;
    }

    // Process named blocks
    let mut new_named = cfg.named.clone();
    for (label, block) in &cfg.named {
        let (new_block, block_deopt) = process_block(block);
        if block_deopt {
            deopts_inserted += 1;
        }
        new_named.insert(label.clone(), new_block);
    }

    if deopts_inserted == 0 {
        return DeoptUnsafeBuiltinsResult::NoChange;
    }

    DeoptUnsafeBuiltinsResult::Success {
        cfg: Cfg {
            entry: new_entry,
            named: new_named,
        },
        deopts_inserted,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::LocalId;

    #[test]
    fn test_is_unsafe_builtin() {
        // Unsafe
        assert!(is_unsafe_builtin("add"));
        assert!(is_unsafe_builtin("del"));
        assert!(is_unsafe_builtin("__print"));
        assert!(is_unsafe_builtin("error"));
        assert!(is_unsafe_builtin("__array_table_drop_last"));

        // Safe
        assert!(!is_unsafe_builtin("min"));
        assert!(!is_unsafe_builtin("max"));
        assert!(!is_unsafe_builtin("abs"));
        assert!(!is_unsafe_builtin("flr"));
        assert!(!is_unsafe_builtin("mget"));
    }

    #[test]
    fn test_deopt_on_add() {
        // CFG: %0 = NumberConstant(1); %1 = CallBuiltin(add, [%0]); return
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::num_const(1)),
                (
                    LocalId::from(1),
                    Instruction::call_builtin("add", vec![LocalId::from(0)]),
                ),
            ],
            (LocalId::from(2), Terminator::Return { value: None }),
        );

        let cfg = Cfg::single_entry(entry);

        let result = deopt_unsafe_builtins(&cfg);
        match result {
            DeoptUnsafeBuiltinsResult::Success { cfg, deopts_inserted } => {
                assert_eq!(deopts_inserted, 1);
                // Entry block should end with Deopt
                match cfg.entry.terminator_kind() {
                    Terminator::Deopt { reason } => {
                        assert!(reason.contains("add"));
                    }
                    other => panic!("Expected Deopt, got {:?}", other),
                }
                // Should only have the constant instruction
                assert_eq!(cfg.entry.instructions.len(), 1);
            }
            DeoptUnsafeBuiltinsResult::NoChange => {
                panic!("Expected deopt to be inserted");
            }
        }
    }

    #[test]
    fn test_no_deopt_on_safe_builtin() {
        // CFG: %0 = NumberConstant(1); %1 = CallBuiltin(max, [%0, %0]); return %1
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::num_const(1)),
                (
                    LocalId::from(1),
                    Instruction::call_builtin("max", vec![LocalId::from(0), LocalId::from(0)]),
                ),
            ],
            (
                LocalId::from(2),
                Terminator::Return {
                    value: Some(LocalId::from(1)),
                },
            ),
        );

        let cfg = Cfg::single_entry(entry);

        let result = deopt_unsafe_builtins(&cfg);
        assert!(matches!(result, DeoptUnsafeBuiltinsResult::NoChange));
    }
}
