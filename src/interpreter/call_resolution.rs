//! Call Resolution Pass: Convert indirect calls to direct calls.
//!
//! This pass transforms indirect calls into `CallResolved` when we know
//! statically which closure is being called.
//!
//! ## Global Function Calls
//!
//! Pattern: `GetGlobal + Load + Call`
//! ```text
//! %1 = GetGlobal(tile_flag_at)
//! %2 = Load(%1)
//! %3 = Call(%2, [args...])
//! ```
//! Becomes:
//! ```text
//! %3 = CallResolved(tile_flag_at_72, captures=[], args=[args...])
//! ```
//!
//! ## Method Calls on Known-Type Arguments
//!
//! Pattern: `Load(arg_cell) + GetField + Load + Call`
//! ```text
//! %1 = Load(%arg0)           // Load 'this' from arg cell
//! %2 = GetField(%1, is_solid) // Get method
//! %3 = Load(%2)              // Load closure
//! %4 = Call(%3, [args...])   // Call method
//! ```
//! When we know arg0 is of type "player" and "player.is_solid" maps to "obj.is_solid_47":
//! ```text
//! %4 = CallResolved(obj.is_solid_47, captures=[%1], args=[args...])
//! ```
//!
//! This enables subsequent inlining and eliminates heap lookups.

use crate::interpreter::common::FxHashMap;
use crate::ir::{Block, Cfg, GlobalId, Instruction, LocalId};

/// Information about a closure stored in a global.
#[derive(Clone, Debug)]
pub struct GlobalClosure {
    /// The function definition name
    pub fun_name: GlobalId,
    /// Whether this closure has captures (if true, we can't resolve without capture values)
    pub has_captures: bool,
}

/// A map from global names to the closures they contain.
pub type GlobalClosureMap = FxHashMap<String, GlobalClosure>;

/// Result of the call resolution pass.
#[derive(Debug)]
pub enum CallResolutionResult {
    /// Successfully resolved some calls
    Success {
        cfg: Cfg,
        /// Number of calls resolved
        calls_resolved: usize,
    },
    /// No calls could be resolved
    NoChange,
}

/// Run the call resolution pass on a CFG.
///
/// This looks for patterns where:
/// 1. A global is read with GetGlobal
/// 2. The global is loaded with Load
/// 3. The loaded value is called with Call
///
/// If the global is in our closure map and has no captures, we replace with CallResolved.
pub fn resolve_calls(cfg: &Cfg, global_closures: &GlobalClosureMap) -> CallResolutionResult {
    let mut total_resolved = 0;

    // Process entry block
    let (new_entry, entry_resolved) = resolve_calls_in_block(&cfg.entry, global_closures);
    total_resolved += entry_resolved;

    // Process named blocks
    let mut new_named = FxHashMap::default();
    for (label, block) in &cfg.named {
        let (new_block, block_resolved) = resolve_calls_in_block(block, global_closures);
        total_resolved += block_resolved;
        new_named.insert(label.clone(), new_block);
    }

    if total_resolved == 0 {
        return CallResolutionResult::NoChange;
    }

    CallResolutionResult::Success {
        cfg: Cfg {
            entry: new_entry,
            named: new_named,
        },
        calls_resolved: total_resolved,
    }
}

/// Resolve calls in a single block.
///
/// We scan for patterns like:
/// ```text
/// %a = GetGlobal(name)
/// %b = Load(%a)
/// %c = Call(%b, args)
/// ```
///
/// When found and the global is known, we:
/// 1. Keep GetGlobal and Load (they might be used elsewhere)
/// 2. Replace Call with CallResolved
fn resolve_calls_in_block(block: &Block, global_closures: &GlobalClosureMap) -> (Block, usize) {
    // Build a map of local_id -> what it contains
    // We track: GetGlobal results (which global name), Load results (what was loaded from)
    #[derive(Clone, Debug)]
    enum LocalInfo {
        GlobalCell(String),         // Result of GetGlobal(name)
        LoadedClosure(GlobalId),    // Result of Load from a global that contains a closure
    }

    let mut local_info: FxHashMap<LocalId, LocalInfo> = FxHashMap::default();
    let mut new_instructions = Vec::new();
    let mut resolved_count = 0;

    for (local_id, instruction) in &block.instructions {
        match instruction {
            Instruction::GetGlobal { name, .. } => {
                // Track that this local contains a pointer to the global cell
                local_info.insert(*local_id, LocalInfo::GlobalCell(name.clone()));
                new_instructions.push((*local_id, instruction.clone()));
            }

            Instruction::Load { source } => {
                // Check if we're loading from a known global cell
                if let Some(LocalInfo::GlobalCell(global_name)) = local_info.get(source) {
                    if let Some(closure_info) = global_closures.get(global_name) {
                        if !closure_info.has_captures {
                            // We know what closure this loads - track it
                            local_info.insert(
                                *local_id,
                                LocalInfo::LoadedClosure(closure_info.fun_name.clone()),
                            );
                        }
                    }
                }
                new_instructions.push((*local_id, instruction.clone()));
            }

            Instruction::Call { closure, args } => {
                // Check if we're calling a known closure
                if let Some(LocalInfo::LoadedClosure(fun_name)) = local_info.get(closure) {
                    // Replace with CallResolved
                    let resolved = Instruction::CallResolved {
                        fun_name: fun_name.clone(),
                        captures: vec![], // No captures since has_captures was false
                        args: args.clone(),
                    };
                    new_instructions.push((*local_id, resolved));
                    resolved_count += 1;
                } else {
                    // Keep the original call
                    new_instructions.push((*local_id, instruction.clone()));
                }
            }

            _ => {
                new_instructions.push((*local_id, instruction.clone()));
            }
        }
    }

    (block.with_instructions(new_instructions), resolved_count)
}

/// Build a GlobalClosureMap from function definitions.
///
/// This assumes that top-level function definitions are stored in globals with the same name
/// (minus the numeric suffix). For example, `tile_flag_at_72` is stored in global `tile_flag_at`.
pub fn build_global_closure_map_from_fun_defs<'a>(
    fun_defs: impl Iterator<Item = &'a crate::ir::FunDef>,
) -> GlobalClosureMap {
    let mut map = GlobalClosureMap::default();

    for fun_def in fun_defs {
        let fun_name = fun_def.name.as_str();

        // Extract the base name (remove _N suffix)
        // e.g., "tile_flag_at_72" -> "tile_flag_at"
        if let Some(underscore_pos) = fun_name.rfind('_') {
            let suffix = &fun_name[underscore_pos + 1..];
            if suffix.chars().all(|c| c.is_ascii_digit()) {
                let base_name = &fun_name[..underscore_pos];

                // Only add if no captures (top-level function)
                let has_captures = !fun_def.capture_ids.is_empty();

                map.insert(
                    base_name.to_string(),
                    GlobalClosure {
                        fun_name: fun_def.name.clone(),
                        has_captures,
                    },
                );
            }
        }
    }

    map
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, GlobalId, Instruction, LocalId, Terminator};
    use crate::pico8_num::Pico8Num;

    fn make_test_cfg() -> Cfg {
        // Create a CFG that does:
        // %0 = GetGlobal(foo)
        // %1 = Load(%0)
        // %2 = NumberConstant(42)
        // %3 = Call(%1, [%2])
        // return %3

        let entry = Block::new_for_test(
            vec![
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
                    Instruction::Call {
                        closure: LocalId::from(1),
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

        Cfg {
            entry,
            named: FxHashMap::default(),
        }
    }

    #[test]
    fn test_resolve_simple_call() {
        let cfg = make_test_cfg();

        let mut global_closures = GlobalClosureMap::default();
        global_closures.insert(
            "foo".to_string(),
            GlobalClosure {
                fun_name: GlobalId::from("foo_1".to_string()),
                has_captures: false,
            },
        );

        let result = resolve_calls(&cfg, &global_closures);

        match result {
            CallResolutionResult::Success { cfg, calls_resolved } => {
                assert_eq!(calls_resolved, 1);

                // Check that the Call was replaced with CallResolved
                let call_instr = &cfg.entry.instructions[3].1;
                match call_instr {
                    Instruction::CallResolved { fun_name, captures, args } => {
                        assert_eq!(fun_name.as_str(), "foo_1");
                        assert!(captures.is_empty());
                        assert_eq!(args.len(), 1);
                    }
                    _ => panic!("Expected CallResolved, got {:?}", call_instr),
                }
            }
            CallResolutionResult::NoChange => {
                panic!("Expected call to be resolved");
            }
        }
    }

    #[test]
    fn test_no_resolve_unknown_global() {
        let cfg = make_test_cfg();

        // Empty closure map - foo is not known
        let global_closures = GlobalClosureMap::default();

        let result = resolve_calls(&cfg, &global_closures);
        assert!(matches!(result, CallResolutionResult::NoChange));
    }

    #[test]
    fn test_no_resolve_closure_with_captures() {
        let cfg = make_test_cfg();

        let mut global_closures = GlobalClosureMap::default();
        global_closures.insert(
            "foo".to_string(),
            GlobalClosure {
                fun_name: GlobalId::from("foo_1".to_string()),
                has_captures: true, // Has captures - can't resolve without capture values
            },
        );

        let result = resolve_calls(&cfg, &global_closures);
        assert!(matches!(result, CallResolutionResult::NoChange));
    }
}
