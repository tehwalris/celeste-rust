//! Builtin Resolution Pass: Convert calls to known builtins to CallBuiltin.
//!
//! This pass transforms `GetGlobal + Load + Call` patterns into `CallBuiltin`
//! when we know statically that the global contains a builtin function.
//!
//! For example:
//! ```text
//! %1 = GetGlobal(max)
//! %2 = Load(%1)
//! %3 = Call(%2, [args...])
//! ```
//! Becomes:
//! ```text
//! %3 = CallBuiltin("max", [args...])
//! ```
//!
//! This eliminates heap lookups for builtin function calls.

use crate::interpreter::common::{FxHashMap, FxHashSet};
use crate::ir::{Block, Cfg, Instruction, LocalId};

/// A set of known builtin function names.
pub type BuiltinSet = FxHashSet<String>;

/// Result of the builtin resolution pass.
#[derive(Debug)]
pub enum BuiltinResolutionResult {
    /// Successfully resolved some builtin calls
    Success {
        cfg: Cfg,
        /// Number of calls resolved
        calls_resolved: usize,
    },
    /// No calls could be resolved
    NoChange,
}

/// Run the builtin resolution pass on a CFG.
///
/// This looks for patterns where:
/// 1. A global is read with GetGlobal
/// 2. The global is loaded with Load
/// 3. The loaded value is called with Call
///
/// If the global name is in our builtin set, we replace with CallBuiltin.
pub fn resolve_builtins(cfg: &Cfg, builtins: &BuiltinSet) -> BuiltinResolutionResult {
    let mut total_resolved = 0;

    // Process entry block
    let (new_entry, entry_resolved) = resolve_builtins_in_block(&cfg.entry, builtins);
    total_resolved += entry_resolved;

    // Process named blocks
    let mut new_named = FxHashMap::default();
    for (label, block) in &cfg.named {
        let (new_block, block_resolved) = resolve_builtins_in_block(block, builtins);
        total_resolved += block_resolved;
        new_named.insert(label.clone(), new_block);
    }

    if total_resolved == 0 {
        return BuiltinResolutionResult::NoChange;
    }

    BuiltinResolutionResult::Success {
        cfg: Cfg {
            entry: new_entry,
            named: new_named,
        },
        calls_resolved: total_resolved,
    }
}

/// Resolve builtin calls in a single block.
///
/// We scan for patterns like:
/// ```text
/// %a = GetGlobal(name)
/// %b = Load(%a)
/// %c = Call(%b, args)
/// ```
///
/// When found and the global is a known builtin, we:
/// 1. Keep GetGlobal and Load (DCE will clean them up if unused)
/// 2. Replace Call with CallBuiltin
fn resolve_builtins_in_block(block: &Block, builtins: &BuiltinSet) -> (Block, usize) {
    // Build maps to track instruction patterns
    // get_global_map: local_id -> global_name (for GetGlobal results)
    // load_map: local_id -> source_local_id (for Load results)
    let mut get_global_map: FxHashMap<LocalId, String> = FxHashMap::default();
    let mut load_map: FxHashMap<LocalId, LocalId> = FxHashMap::default();

    // First pass: collect GetGlobal and Load information
    for (local_id, instruction) in &block.instructions {
        match instruction {
            Instruction::GetGlobal { name, .. } => {
                get_global_map.insert(*local_id, name.clone());
            }
            Instruction::Load { source } => {
                load_map.insert(*local_id, *source);
            }
            _ => {}
        }
    }

    // Second pass: resolve calls
    let mut resolved_count = 0;
    let mut new_instructions = Vec::new();

    for (local_id, instruction) in &block.instructions {
        match instruction {
            Instruction::Call { closure, args } => {
                // Check if this is a Call(Load(GetGlobal(builtin_name)))
                if let Some(load_source) = load_map.get(closure) {
                    if let Some(global_name) = get_global_map.get(load_source) {
                        if builtins.contains(global_name) {
                            // This is a call to a known builtin!
                            new_instructions.push((
                                *local_id,
                                Instruction::CallBuiltin {
                                    name: global_name.clone(),
                                    args: args.clone(),
                                },
                            ));
                            resolved_count += 1;
                            continue;
                        }
                    }
                }
                // Not a builtin call - keep as is
                new_instructions.push((*local_id, instruction.clone()));
            }
            _ => {
                new_instructions.push((*local_id, instruction.clone()));
            }
        }
    }

    (block.with_instructions(new_instructions), resolved_count)
}

/// Build a builtin set from the names of registered builtins.
pub fn build_builtin_set<'a>(builtin_names: impl Iterator<Item = &'a String>) -> BuiltinSet {
    builtin_names.cloned().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{LocalId, Terminator};

    #[test]
    fn test_resolve_simple_builtin_call() {
        // Create a CFG with: GetGlobal("max") -> Load -> Call
        let cfg = Cfg::single_entry(Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("max")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::num_const(1)),
                (LocalId::from(3), Instruction::num_const(2)),
                (LocalId::from(4), Instruction::call(LocalId::from(1), vec![LocalId::from(2), LocalId::from(3)])),
            ],
            (
                LocalId::from(5),
                Terminator::Return {
                    value: Some(LocalId::from(4)),
                },
            ),
        ));

        // Build builtin set with "max"
        let mut builtins = BuiltinSet::default();
        builtins.insert("max".to_string());

        let result = resolve_builtins(&cfg, &builtins);

        match result {
            BuiltinResolutionResult::Success { cfg: new_cfg, calls_resolved } => {
                assert_eq!(calls_resolved, 1);

                // Check that the Call was replaced with CallBuiltin
                let has_call_builtin = new_cfg.entry.instructions.iter().any(|(_, instr)| {
                    matches!(instr, Instruction::CallBuiltin { name, args }
                        if name == "max" && args.len() == 2)
                });
                assert!(has_call_builtin, "Should have CallBuiltin for max");

                // Original Call should be gone
                let has_call = new_cfg.entry.instructions.iter().any(|(_, instr)| {
                    matches!(instr, Instruction::Call { .. })
                });
                assert!(!has_call, "Should not have Call anymore");

                // GetGlobal and Load should still be there (DCE will remove them)
                let has_get_global = new_cfg.entry.instructions.iter().any(|(_, instr)| {
                    matches!(instr, Instruction::GetGlobal { name, .. } if name == "max")
                });
                assert!(has_get_global, "GetGlobal should still be present");
            }
            BuiltinResolutionResult::NoChange => {
                panic!("Expected Success, got NoChange");
            }
        }
    }

    #[test]
    fn test_no_resolve_unknown_global() {
        // Create a CFG with: GetGlobal("unknown") -> Load -> Call
        let cfg = Cfg::single_entry(Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("unknown")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::call(LocalId::from(1), vec![])),
            ],
            (
                LocalId::from(3),
                Terminator::Return {
                    value: Some(LocalId::from(2)),
                },
            ),
        ));

        // Build builtin set with only "max" (not "unknown")
        let mut builtins = BuiltinSet::default();
        builtins.insert("max".to_string());

        let result = resolve_builtins(&cfg, &builtins);

        assert!(matches!(result, BuiltinResolutionResult::NoChange));
    }

    #[test]
    fn test_resolve_multiple_builtin_calls() {
        // Create a CFG with calls to both max and min
        let cfg = Cfg::single_entry(Block::new_for_test(
            vec![
                // max call
                (LocalId::from(0), Instruction::get_global("max")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::num_const(1)),
                (LocalId::from(3), Instruction::call(LocalId::from(1), vec![LocalId::from(2)])),
                // min call
                (LocalId::from(4), Instruction::get_global("min")),
                (LocalId::from(5), Instruction::load(LocalId::from(4))),
                (LocalId::from(6), Instruction::call(LocalId::from(5), vec![LocalId::from(3)])),
            ],
            (
                LocalId::from(7),
                Terminator::Return {
                    value: Some(LocalId::from(6)),
                },
            ),
        ));

        // Build builtin set with both max and min
        let mut builtins = BuiltinSet::default();
        builtins.insert("max".to_string());
        builtins.insert("min".to_string());

        let result = resolve_builtins(&cfg, &builtins);

        match result {
            BuiltinResolutionResult::Success { cfg: new_cfg, calls_resolved } => {
                assert_eq!(calls_resolved, 2);

                // Check that both calls were replaced
                let builtin_count = new_cfg.entry.instructions.iter().filter(|(_, instr)| {
                    matches!(instr, Instruction::CallBuiltin { .. })
                }).count();
                assert_eq!(builtin_count, 2);
            }
            BuiltinResolutionResult::NoChange => {
                panic!("Expected Success, got NoChange");
            }
        }
    }
}
