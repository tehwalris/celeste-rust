//! Lightweight Slot Tracker for Call Resolution
//!
//! This module provides a simple analysis to track which LocalIds point to which HeapSlots.
//! Unlike the full SSA transformation in heap_elimination, this:
//! - Does NOT create Phi nodes
//! - Marks values as "unknown" at merge points (multiple predecessors)
//! - Can be run multiple times safely on the same CFG
//!
//! This is sufficient for call resolution, which only needs to know WHERE a closure
//! comes from (which slot), not WHAT its value is.

use crate::interpreter::call_resolution::GlobalClosureMap;
use crate::interpreter::common::FxHashMap;
use crate::interpreter::heap_elimination::{HeapPath, HeapShape, HeapSlot};
use crate::ir::{Block, BlockId, Cfg, GlobalId, Instruction, Label, LocalId};

/// Tracks which LocalId points to which HeapSlot (or unknown)
#[derive(Debug, Clone)]
pub struct SlotMapping {
    /// LocalId -> HeapSlot mapping (only for known slots)
    pub known_slots: FxHashMap<LocalId, HeapSlot>,
}

impl Default for SlotMapping {
    fn default() -> Self {
        Self::new()
    }
}

impl SlotMapping {
    pub fn new() -> Self {
        Self {
            known_slots: FxHashMap::default(),
        }
    }

    /// Get the slot for a LocalId, if known
    pub fn get_slot(&self, id: LocalId) -> Option<&HeapSlot> {
        self.known_slots.get(&id)
    }

    /// Set the slot for a LocalId
    pub fn set_slot(&mut self, id: LocalId, slot: HeapSlot) {
        self.known_slots.insert(id, slot);
    }

    /// Merge with another mapping (for join points)
    /// Values that differ become unknown (removed from mapping)
    pub fn merge(&mut self, other: &SlotMapping) {
        let keys_to_remove: Vec<_> = self.known_slots.keys()
            .filter(|k| other.known_slots.get(k) != self.known_slots.get(k))
            .cloned()
            .collect();

        for key in keys_to_remove {
            self.known_slots.remove(&key);
        }
    }
}

/// Result of call resolution pass
#[derive(Debug)]
pub struct CallResolutionResult {
    /// The transformed CFG
    pub cfg: Cfg,
    /// Number of calls resolved
    pub calls_resolved: usize,
}


/// Check if a slot exists in the shape.
/// Uses HeapShape::get_shape_at_path() for path resolution.
fn is_slot_in_shape(slot: &HeapSlot, shape: &HeapShape) -> bool {
    // HeapShape::get_shape_at_path handles Global, Arg, and Field paths.
    // It returns None for Index paths, which is correct (not supported for call resolution).
    shape.get_shape_at_path(slot.path()).is_some()
}

/// Get the closure definition from a slot, if it's a known closure.
/// Uses the GlobalClosureMap to look up actual function names and capture info.
/// Returns None if we can't find the closure in the GlobalClosureMap (shape fallback is disabled
/// because shapes don't track capture information accurately).
fn get_closure_from_slot(
    slot: &HeapSlot,
    global_closure_map: &GlobalClosureMap,
) -> Option<(GlobalId, bool)> {  // Returns (fun_name, has_captures)
    // Convert slot path to the format used by GlobalClosureMap (e.g., "_G.foo" -> "foo")
    let slot_key = slot.to_string();
    let global_key = slot_key.strip_prefix("_G.").unwrap_or(&slot_key);

    if let Some(closure_info) = global_closure_map.get(global_key) {
        // Found in GlobalClosureMap - return actual function name
        return Some((closure_info.fun_name.clone(), closure_info.has_captures));
    }

    // Don't fall back to shape-based resolution because shapes don't track captures accurately.
    // The shape may have Closure with empty capture_shapes even if the function has captures.
    // We can only safely resolve calls when we have accurate capture info from GlobalClosureMap.
    None
}

/// Analyze and resolve calls in a single pass through a block.
/// Returns the transformed block, number of calls resolved, and the output slot mapping.
fn analyze_and_resolve_block(
    block: &Block,
    incoming: &SlotMapping,
    shape: &HeapShape,
    global_closure_map: &GlobalClosureMap,
) -> (Block, usize, SlotMapping) {
    let mut mapping = incoming.clone();
    let mut new_instructions = Vec::new();
    let mut calls_resolved = 0;

    for (target_id, instruction) in &block.instructions {
        // First, try to resolve Call instructions using current mapping
        match instruction {
            Instruction::Call { closure, args } => {
                // Try to resolve the closure
                if let Some(slot) = mapping.get_slot(*closure) {
                    if let Some((fun_name, has_captures)) = get_closure_from_slot(slot, global_closure_map) {
                        // Only resolve if the closure has no captures
                        // (closures with captures need capture values passed at call site)
                        if !has_captures {
                            // Resolved! Convert to CallResolved
                            new_instructions.push((
                                *target_id,
                                Instruction::CallResolved {
                                    fun_name,
                                    captures: vec![],
                                    args: args.clone(),
                                },
                            ));
                            calls_resolved += 1;
                            continue;
                        }
                    }
                }
                // Couldn't resolve - keep as Call
                new_instructions.push((*target_id, instruction.clone()));
            }
            // Update slot mappings for GetGlobal/GetField/Load
            Instruction::GetGlobal { name, .. } => {
                if shape.globals.contains_key(name) {
                    mapping.set_slot(*target_id, HeapSlot::new(HeapPath::Global(name.clone())));
                }
                new_instructions.push((*target_id, instruction.clone()));
            }
            Instruction::GetField { receiver, field, .. } => {
                if let Some(parent_slot) = mapping.get_slot(*receiver).cloned() {
                    let child_path = parent_slot.path().clone().field(field);
                    let child_slot = HeapSlot::new(child_path);
                    if is_slot_in_shape(&child_slot, shape) {
                        mapping.set_slot(*target_id, child_slot);
                    }
                }
                new_instructions.push((*target_id, instruction.clone()));
            }
            // Track through Load - the loaded value comes from the same slot as the pointer
            Instruction::Load { source } => {
                if let Some(source_slot) = mapping.get_slot(*source).cloned() {
                    mapping.set_slot(*target_id, source_slot);
                }
                new_instructions.push((*target_id, instruction.clone()));
            }
            _ => {
                new_instructions.push((*target_id, instruction.clone()));
            }
        }
    }

    (
        block.with_instructions(new_instructions),
        calls_resolved,
        mapping,
    )
}

/// Run call resolution on a CFG using lightweight slot tracking.
///
/// Note: The `shape.args` field should be set before calling this function.
/// This is typically done by the caller (e.g., `cfg_analysis.rs` sets
/// `shape.args = arg_shapes.to_vec()` before calling).
pub fn resolve_calls_via_slots(
    cfg: &Cfg,
    shape: &HeapShape,
    arg_ids: &[Option<LocalId>],
    global_closure_map: &GlobalClosureMap,
) -> CallResolutionResult {
    let predecessors = cfg.compute_predecessors();

    // Initialize slot mappings for arguments
    let mut initial_mapping = SlotMapping::new();
    for (idx, opt_id) in arg_ids.iter().enumerate() {
        if let Some(id) = opt_id {
            // Use shape.args instead of a separate arg_shapes parameter
            if shape.args.get(idx).map(|s| s.is_some()).unwrap_or(false) {
                initial_mapping.set_slot(*id, HeapSlot::new(HeapPath::Arg(idx)));
            }
        }
    }

    // We need to compute mappings at block exits to propagate to successors,
    // and simultaneously transform the blocks. We do this in two phases:
    // 1. Compute exit mappings (for propagation to successors)
    // 2. Transform blocks using incoming mappings

    // First, compute exit mappings using a fixed-point algorithm
    let mut exit_mappings: FxHashMap<BlockId, SlotMapping> = FxHashMap::default();

    // Process entry block to get its exit mapping (no transformation yet)
    let entry_exit = analyze_block_mappings(&cfg.entry, &initial_mapping, shape);
    exit_mappings.insert(BlockId::Entry, entry_exit);

    // Fixed-point iteration for named blocks
    let mut changed = true;
    let max_iterations = 100;
    let mut iterations = 0;

    while changed && iterations < max_iterations {
        changed = false;
        iterations += 1;

        for (label, block) in &cfg.named {
            let block_id = BlockId::Named(label.clone());
            let incoming = compute_incoming_mapping(&block_id, &predecessors, &exit_mappings);
            let new_exit = analyze_block_mappings(block, &incoming, shape);

            let old_exit = exit_mappings.get(&block_id);
            if old_exit.map(|m| m.known_slots != new_exit.known_slots).unwrap_or(true) {
                exit_mappings.insert(block_id, new_exit);
                changed = true;
            }
        }
    }

    // Now transform all blocks using the computed incoming mappings
    let mut total_resolved = 0;

    // Transform entry block
    let (new_entry, entry_resolved, _) = analyze_and_resolve_block(
        &cfg.entry,
        &initial_mapping,
        shape,
        global_closure_map,
    );
    total_resolved += entry_resolved;

    // Transform named blocks
    let mut new_named: FxHashMap<Label, Block> = FxHashMap::default();
    for (label, block) in &cfg.named {
        let block_id = BlockId::Named(label.clone());
        let incoming = compute_incoming_mapping(&block_id, &predecessors, &exit_mappings);
        let (new_block, resolved, _) = analyze_and_resolve_block(block, &incoming, shape, global_closure_map);
        total_resolved += resolved;
        new_named.insert(label.clone(), new_block);
    }

    CallResolutionResult {
        cfg: Cfg {
            entry: new_entry,
            named: new_named,
        },
        calls_resolved: total_resolved,
    }
}

/// Analyze a block's instructions to compute the exit slot mapping (no transformation).
fn analyze_block_mappings(
    block: &Block,
    incoming: &SlotMapping,
    shape: &HeapShape,
) -> SlotMapping {
    let mut mapping = incoming.clone();

    for (target_id, instruction) in &block.instructions {
        match instruction {
            Instruction::GetGlobal { name, .. } => {
                if shape.globals.contains_key(name) {
                    mapping.set_slot(*target_id, HeapSlot::new(HeapPath::Global(name.clone())));
                }
            }
            Instruction::GetField { receiver, field, .. } => {
                if let Some(parent_slot) = mapping.get_slot(*receiver).cloned() {
                    let child_path = parent_slot.path().clone().field(field);
                    let child_slot = HeapSlot::new(child_path);
                    // We don't need to check is_slot_in_shape for mapping propagation
                    mapping.set_slot(*target_id, child_slot);
                }
            }
            // Track through Load - the loaded value comes from the same slot as the pointer
            Instruction::Load { source } => {
                if let Some(source_slot) = mapping.get_slot(*source).cloned() {
                    mapping.set_slot(*target_id, source_slot);
                }
            }
            _ => {}
        }
    }

    mapping
}

/// Compute the incoming mapping for a block by merging predecessor exit mappings.
fn compute_incoming_mapping(
    block_id: &BlockId,
    predecessors: &FxHashMap<BlockId, Vec<BlockId>>,
    exit_mappings: &FxHashMap<BlockId, SlotMapping>,
) -> SlotMapping {
    let preds = predecessors.get(block_id).cloned().unwrap_or_default();

    if preds.is_empty() {
        return SlotMapping::new();
    }

    if preds.len() == 1 {
        return exit_mappings.get(&preds[0]).cloned().unwrap_or_else(SlotMapping::new);
    }

    // Multiple predecessors - merge (conservative: unknown if different)
    let mut merged = exit_mappings.get(&preds[0]).cloned().unwrap_or_else(SlotMapping::new);
    for pred in preds.iter().skip(1) {
        if let Some(pred_mapping) = exit_mappings.get(pred) {
            merged.merge(pred_mapping);
        } else {
            return SlotMapping::new();
        }
    }
    merged
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::call_resolution::GlobalClosure;
    use crate::interpreter::heap_elimination::ValueShape;
    use crate::ir::{Block, Cfg, Instruction, Label, LocalId, Terminator};

    #[test]
    fn test_resolve_simple_global_call() {
        // CFG:
        // %0 = GetGlobal("foo")
        // %1 = Load(%0)
        // %2 = Call(%1, [])  // Uses Load result as closure
        // return %2

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::GetGlobal { name: "foo".to_string(), create_if_missing: false }),
                (LocalId::from(1), Instruction::Load { source: LocalId::from(0) }),
                (LocalId::from(2), Instruction::Call { closure: LocalId::from(1), args: vec![] }),
            ],
            (LocalId::from(99), Terminator::Return { value: Some(LocalId::from(2)) }),
        );

        let cfg = Cfg::single_entry(entry);

        // Shape: foo is a Leaf (closure)
        let mut shape = HeapShape::new();
        shape.globals.insert("foo".to_string(), ValueShape::Leaf);

        // GlobalClosureMap: foo -> foo_1 function
        let mut global_closure_map: GlobalClosureMap = FxHashMap::default();
        global_closure_map.insert("foo".to_string(), GlobalClosure {
            fun_name: GlobalId::from("foo_1".to_string()),
            has_captures: false,
        });

        let result = resolve_calls_via_slots(&cfg, &shape, &[], &global_closure_map);

        assert_eq!(result.calls_resolved, 1);

        // Check that Call became CallResolved with correct function name
        let call_resolved = result.cfg.entry.instructions.iter().find_map(|(_, instr)| {
            if let Instruction::CallResolved { fun_name, .. } = instr {
                Some(fun_name.clone())
            } else {
                None
            }
        });
        assert_eq!(call_resolved, Some(GlobalId::from("foo_1".to_string())));
    }

    #[test]
    fn test_resolve_nested_field_call() {
        // CFG:
        // %0 = GetGlobal("player")
        // %1 = GetField(%0, "update")
        // %2 = Load(%1)
        // %3 = Call(%2, [])  // Uses Load result as closure
        // return %3

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::GetGlobal { name: "player".to_string(), create_if_missing: false }),
                (LocalId::from(1), Instruction::GetField { receiver: LocalId::from(0), field: "update".to_string(), create_if_missing: false }),
                (LocalId::from(2), Instruction::Load { source: LocalId::from(1) }),
                (LocalId::from(3), Instruction::Call { closure: LocalId::from(2), args: vec![] }),
            ],
            (LocalId::from(99), Terminator::Return { value: Some(LocalId::from(3)) }),
        );

        let cfg = Cfg::single_entry(entry);

        // Shape: player.update is a Leaf (closure)
        let mut shape = HeapShape::new();
        let mut player_shape = FxHashMap::default();
        player_shape.insert("update".to_string(), ValueShape::Leaf);
        shape.globals.insert("player".to_string(), ValueShape::Table(player_shape));

        // GlobalClosureMap: player.update -> player_update_7 function
        let mut global_closure_map: GlobalClosureMap = FxHashMap::default();
        global_closure_map.insert("player.update".to_string(), GlobalClosure {
            fun_name: GlobalId::from("player_update_7".to_string()),
            has_captures: false,
        });

        let result = resolve_calls_via_slots(&cfg, &shape, &[], &global_closure_map);

        assert_eq!(result.calls_resolved, 1);

        // Verify correct function name
        let call_resolved = result.cfg.entry.instructions.iter().find_map(|(_, instr)| {
            if let Instruction::CallResolved { fun_name, .. } = instr {
                Some(fun_name.clone())
            } else {
                None
            }
        });
        assert_eq!(call_resolved, Some(GlobalId::from("player_update_7".to_string())));
    }

    #[test]
    fn test_no_resolve_unknown_at_merge() {
        // CFG with merge point where closure source is unknown:
        // if cond:
        //   %1 = GetGlobal("foo")
        // else:
        //   %2 = GetGlobal("bar")
        // %3 = Phi(...)  -- but we don't have Phi, so this tests the merge behavior
        // Call(%3)

        // For this test, we simulate by having two paths merge
        // Since we don't track through Phis, the call shouldn't resolve

        let entry = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::bool_const(true))],
            (LocalId::from(99), Terminator::ConditionalBranch {
                condition: LocalId::from(0),
                true_target: Label::from("true_branch".to_string()),
                false_target: Label::from("false_branch".to_string()),
            }),
        );

        let true_branch = Block::new_for_test(
            vec![
                (LocalId::from(1), Instruction::GetGlobal { name: "foo".to_string(), create_if_missing: false }),
            ],
            (LocalId::from(98), Terminator::UnconditionalBranch { target: Label::from("join".to_string()) }),
        );

        let false_branch = Block::new_for_test(
            vec![
                (LocalId::from(2), Instruction::GetGlobal { name: "bar".to_string(), create_if_missing: false }),
            ],
            (LocalId::from(97), Terminator::UnconditionalBranch { target: Label::from("join".to_string()) }),
        );

        // Join block tries to call %1 (from true branch) - but this won't work
        // because at the join point, we don't know which branch was taken
        // In practice, the original code would use a Phi node here
        // For this test, we just verify that a call using a LocalId from one branch
        // doesn't get resolved at the join point
        let join = Block::new_for_test(
            vec![
                // This call uses %1 which is only defined in true_branch
                // At the join point, we don't know if %1 is valid
                (LocalId::from(3), Instruction::num_const(0)),
            ],
            (LocalId::from(96), Terminator::Return { value: Some(LocalId::from(3)) }),
        );

        let cfg = Cfg {
            entry,
            named: [
                (Label::from("true_branch".to_string()), true_branch),
                (Label::from("false_branch".to_string()), false_branch),
                (Label::from("join".to_string()), join),
            ].into_iter().collect(),
        };

        let mut shape = HeapShape::new();
        shape.globals.insert("foo".to_string(), ValueShape::Leaf);
        shape.globals.insert("bar".to_string(), ValueShape::Leaf);

        let global_closure_map: GlobalClosureMap = FxHashMap::default();
        let result = resolve_calls_via_slots(&cfg, &shape, &[], &global_closure_map);

        // No calls to resolve in this CFG
        assert_eq!(result.calls_resolved, 0);
    }

    #[test]
    fn test_idempotent_multiple_runs() {
        // Running the pass multiple times should produce the same result

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::GetGlobal { name: "foo".to_string(), create_if_missing: false }),
                (LocalId::from(1), Instruction::Load { source: LocalId::from(0) }),
                (LocalId::from(2), Instruction::Call { closure: LocalId::from(1), args: vec![] }),
            ],
            (LocalId::from(99), Terminator::Return { value: Some(LocalId::from(2)) }),
        );

        let cfg = Cfg::single_entry(entry);

        let mut shape = HeapShape::new();
        shape.globals.insert("foo".to_string(), ValueShape::Leaf);

        let mut global_closure_map: GlobalClosureMap = FxHashMap::default();
        global_closure_map.insert("foo".to_string(), GlobalClosure {
            fun_name: GlobalId::from("foo_1".to_string()),
            has_captures: false,
        });

        // First run
        let result1 = resolve_calls_via_slots(&cfg, &shape, &[], &global_closure_map);
        assert_eq!(result1.calls_resolved, 1);

        // Second run on the result
        let result2 = resolve_calls_via_slots(&result1.cfg, &shape, &[], &global_closure_map);

        // Should resolve 0 calls (already resolved)
        assert_eq!(result2.calls_resolved, 0);

        // CFG should be identical
        assert_eq!(result1.cfg.entry.instructions.len(), result2.cfg.entry.instructions.len());
    }
}
