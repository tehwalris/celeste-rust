//! Heap Elimination via Shape-Specialized SSA Transformation
//!
//! Transforms a CFG that operates on a Lua heap (tables, closures, etc.) into
//! a pure SSA CFG with no heap operations in the body.
//!
//! The transformation has three parts:
//! 1. **Unpack** (at function entry): For each leaf slot in the known heap shape,
//!    emit a heap_read into a fresh SSA variable.
//! 2. **Body rewriting**: Replace heap_read with current SSA version, heap_write
//!    creates new SSA definition, add phi nodes at control flow joins.
//! 3. **Repack** (at function exits): Emit heap_writes with final SSA versions.
//!
//! Uses the Braun algorithm for on-demand SSA construction.

use std::collections::{HashMap, HashSet};
use std::hash::BuildHasherDefault;

use rustc_hash::FxHasher;

use crate::ir::{
    Block, Cfg, GlobalId, Instruction, Label, LabelGenerator, LocalId, LocalIdGenerator, Terminator,
};

type FxHashMap<K, V> = HashMap<K, V, BuildHasherDefault<FxHasher>>;
type FxHashSet<T> = HashSet<T, BuildHasherDefault<FxHasher>>;

/// A path into the heap, representing a specific memory location.
/// For example: `player.pos.x` or `globals.room`
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum HeapPath {
    /// A global variable: _G["name"]
    Global(String),
    /// A field access: base.field
    Field(Box<HeapPath>, String),
    /// An index access with a known constant index: base[index]
    Index(Box<HeapPath>, HeapIndex),
}

/// An index into a table - either a string field or numeric index
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum HeapIndex {
    String(String),
    Number(i32),
}

impl HeapPath {
    pub fn global(name: &str) -> Self {
        HeapPath::Global(name.to_string())
    }

    pub fn field(self, field: &str) -> Self {
        HeapPath::Field(Box::new(self), field.to_string())
    }

    pub fn index_str(self, index: &str) -> Self {
        HeapPath::Index(Box::new(self), HeapIndex::String(index.to_string()))
    }

    pub fn index_num(self, index: i32) -> Self {
        HeapPath::Index(Box::new(self), HeapIndex::Number(index))
    }
}

/// A heap slot identifier - uniquely identifies a memory location in the abstract heap.
/// This is similar to HeapPath but normalized and used as a key during transformation.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct HeapSlot(HeapPath);

impl HeapSlot {
    pub fn new(path: HeapPath) -> Self {
        HeapSlot(path)
    }

    pub fn path(&self) -> &HeapPath {
        &self.0
    }
}

/// The known shape of a value in the heap
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueShape {
    /// A leaf value (number, string, boolean, nil) - these are abstract
    Leaf,
    /// A table with known fields
    Table(FxHashMap<String, ValueShape>),
    /// A closure with its function name and capture shapes
    Closure {
        fun_name: GlobalId,
        capture_shapes: Vec<ValueShape>,
    },
    /// A pointer to another heap location (aliasing)
    Pointer(HeapSlot),
}

/// The concrete shape of the heap at function entry
#[derive(Clone, Debug)]
pub struct HeapShape {
    /// Known globals and their shapes
    pub globals: FxHashMap<String, ValueShape>,
    /// Known argument shapes (for function arguments that are tables)
    pub args: Vec<Option<ValueShape>>,
}

impl HeapShape {
    pub fn new() -> Self {
        HeapShape {
            globals: FxHashMap::default(),
            args: Vec::new(),
        }
    }

    /// Collect all leaf slots that need to be unpacked
    pub fn collect_leaf_slots(&self) -> Vec<HeapSlot> {
        let mut slots = Vec::new();

        for (name, shape) in &self.globals {
            self.collect_slots_recursive(
                HeapPath::Global(name.clone()),
                shape,
                &mut slots,
            );
        }

        slots
    }

    fn collect_slots_recursive(
        &self,
        path: HeapPath,
        shape: &ValueShape,
        slots: &mut Vec<HeapSlot>,
    ) {
        match shape {
            ValueShape::Leaf => {
                slots.push(HeapSlot::new(path));
            }
            ValueShape::Table(fields) => {
                for (field, field_shape) in fields {
                    self.collect_slots_recursive(
                        path.clone().field(field),
                        field_shape,
                        slots,
                    );
                }
            }
            ValueShape::Closure { .. } => {
                // Closures are not transformed - we know which function they refer to
                // but we don't need to unpack their captures
            }
            ValueShape::Pointer(target) => {
                // Pointers create aliasing - use the target slot
                slots.push(target.clone());
            }
        }
    }
}

/// Result of attempting the heap elimination transformation
#[derive(Debug)]
pub enum HeapEliminationResult {
    /// Successfully transformed the CFG
    Success(TransformedCfg),
    /// Transformation failed because the shape wasn't preserved
    ShapeNotPreserved(String),
    /// Transformation failed because of external calls
    HasExternalCalls(Vec<String>),
    /// Transformation not applicable (e.g., no heap operations)
    NotApplicable,
}

/// A successfully transformed CFG
#[derive(Clone, Debug)]
pub struct TransformedCfg {
    /// The transformed CFG with SSA operations
    pub cfg: Cfg,
    /// Mapping from heap slots to their initial SSA variables (for unpack)
    pub unpack_slots: Vec<(HeapSlot, LocalId)>,
    /// Mapping from heap slots to their final SSA variables (for repack)
    pub repack_slots: Vec<(HeapSlot, LocalId)>,
    /// Slots that were modified (need repacking)
    pub modified_slots: FxHashSet<HeapSlot>,
}

/// Block identifier - either entry or named
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum BlockId {
    Entry,
    Named(Label),
}

/// SSA construction state using the Braun algorithm
struct SsaBuilder {
    /// Current local ID generator
    local_gen: LocalIdGenerator,
    /// Label generator for any new blocks needed
    label_gen: LabelGenerator,
    /// Mapping: (block, slot) -> SSA variable at end of block
    /// This is the memoization table for the Braun algorithm
    slot_versions: FxHashMap<(BlockId, HeapSlot), LocalId>,
    /// Initial versions from unpack (for entry block lookups)
    initial_versions: FxHashMap<HeapSlot, LocalId>,
    /// Definitions within each block: (block, slot) -> LocalId
    block_definitions: FxHashMap<(BlockId, HeapSlot), LocalId>,
    /// Predecessors of each block
    predecessors: FxHashMap<BlockId, Vec<(BlockId, Label)>>,
    /// Phi nodes to insert: block -> [(target_id, slot, branches)]
    phi_nodes: FxHashMap<BlockId, Vec<(LocalId, HeapSlot, Vec<(Label, LocalId)>)>>,
    /// Blocks that are currently being processed (for cycle detection)
    in_progress: FxHashSet<(BlockId, HeapSlot)>,
}

impl SsaBuilder {
    fn new(local_gen: LocalIdGenerator, label_gen: LabelGenerator) -> Self {
        SsaBuilder {
            local_gen,
            label_gen,
            slot_versions: FxHashMap::default(),
            initial_versions: FxHashMap::default(),
            block_definitions: FxHashMap::default(),
            predecessors: FxHashMap::default(),
            phi_nodes: FxHashMap::default(),
            in_progress: FxHashSet::default(),
        }
    }

    /// Set the initial version of a slot (from unpack)
    fn set_initial_version(&mut self, slot: HeapSlot, id: LocalId) {
        self.initial_versions.insert(slot, id);
    }

    /// Record a definition of a slot within a block
    fn define_in_block(&mut self, block: &BlockId, slot: HeapSlot, id: LocalId) {
        self.block_definitions.insert((block.clone(), slot), id);
    }

    /// Get the SSA variable for a slot at the end of a block (Braun algorithm)
    fn get_at_end(&mut self, block: &BlockId, slot: &HeapSlot) -> LocalId {
        let key = (block.clone(), slot.clone());

        // Check memo table first
        if let Some(&id) = self.slot_versions.get(&key) {
            return id;
        }

        // Check if this block defines the slot
        if let Some(&id) = self.block_definitions.get(&key) {
            self.slot_versions.insert(key, id);
            return id;
        }

        // For entry block with no definition, use initial version
        if *block == BlockId::Entry {
            if let Some(&id) = self.initial_versions.get(slot) {
                self.slot_versions.insert(key, id);
                return id;
            }
            panic!("No initial version for slot {:?} in entry block", slot);
        }

        // Get predecessors
        let preds = self.predecessors.get(block).cloned().unwrap_or_default();

        if preds.is_empty() {
            // Unreachable block - use initial version
            if let Some(&id) = self.initial_versions.get(slot) {
                self.slot_versions.insert(key, id);
                return id;
            }
            panic!("No predecessors and no initial version for slot {:?}", slot);
        }

        if preds.len() == 1 {
            // Single predecessor - recurse
            let (pred_block, _) = &preds[0];
            let id = self.get_at_end(pred_block, slot);
            self.slot_versions.insert(key, id);
            return id;
        }

        // Multiple predecessors - need a phi node
        // First, check for cycles
        if self.in_progress.contains(&key) {
            // We're in a cycle - create a placeholder phi node
            let phi_id = self.local_gen.next();
            self.slot_versions.insert(key.clone(), phi_id);
            return phi_id;
        }

        // Mark as in progress
        self.in_progress.insert(key.clone());

        // Create phi node with placeholder
        let phi_id = self.local_gen.next();
        self.slot_versions.insert(key.clone(), phi_id);

        // Collect incoming values from predecessors
        let mut branches = Vec::new();
        for (pred_block, pred_label) in &preds {
            let pred_id = self.get_at_end(pred_block, slot);
            branches.push((pred_label.clone(), pred_id));
        }

        // Remove from in progress
        self.in_progress.remove(&key);

        // Record the phi node
        self.phi_nodes
            .entry(block.clone())
            .or_default()
            .push((phi_id, slot.clone(), branches));

        phi_id
    }

    /// Get the final SSA version for a slot at function exits
    fn get_final_version(&mut self, exit_block: &BlockId, slot: &HeapSlot) -> LocalId {
        self.get_at_end(exit_block, slot)
    }
}

/// Analyze a CFG to compute predecessor information
fn compute_predecessors(cfg: &Cfg) -> FxHashMap<BlockId, Vec<(BlockId, Label)>> {
    let mut preds: FxHashMap<BlockId, Vec<(BlockId, Label)>> = FxHashMap::default();

    // Entry block has no predecessors (it's the entry point)

    // Helper to add predecessor
    let mut add_pred = |target: &Label, from: BlockId, from_label: Label| {
        preds
            .entry(BlockId::Named(target.clone()))
            .or_default()
            .push((from, from_label));
    };

    // Process entry block
    match &cfg.entry.terminator.1 {
        Terminator::Return { .. } => {}
        Terminator::UnconditionalBranch { target } => {
            add_pred(target, BlockId::Entry, Label::from("entry".to_string()));
        }
        Terminator::ConditionalBranch {
            true_target,
            false_target,
            ..
        } => {
            add_pred(true_target, BlockId::Entry, Label::from("entry".to_string()));
            add_pred(false_target, BlockId::Entry, Label::from("entry".to_string()));
        }
    }

    // Process named blocks
    for (label, block) in &cfg.named {
        let block_id = BlockId::Named(label.clone());
        match &block.terminator.1 {
            Terminator::Return { .. } => {}
            Terminator::UnconditionalBranch { target } => {
                add_pred(target, block_id, label.clone());
            }
            Terminator::ConditionalBranch {
                true_target,
                false_target,
                ..
            } => {
                add_pred(true_target, block_id.clone(), label.clone());
                add_pred(false_target, block_id, label.clone());
            }
        }
    }

    preds
}

/// Find exit blocks (blocks that return)
fn find_exit_blocks(cfg: &Cfg) -> Vec<BlockId> {
    let mut exits = Vec::new();

    if matches!(cfg.entry.terminator.1, Terminator::Return { .. }) {
        exits.push(BlockId::Entry);
    }

    for (label, block) in &cfg.named {
        if matches!(block.terminator.1, Terminator::Return { .. }) {
            exits.push(BlockId::Named(label.clone()));
        }
    }

    exits
}

/// Mapping from LocalId to the heap slot it represents (if any)
struct LocalIdMapping {
    /// LocalId -> HeapSlot for pointer-typed locals
    id_to_slot: FxHashMap<LocalId, HeapSlot>,
    /// LocalId -> the value it holds (for tracking through loads/stores)
    id_to_value: FxHashMap<LocalId, LocalId>,
}

impl LocalIdMapping {
    fn new() -> Self {
        LocalIdMapping {
            id_to_slot: FxHashMap::default(),
            id_to_value: FxHashMap::default(),
        }
    }

    fn set_slot(&mut self, id: LocalId, slot: HeapSlot) {
        self.id_to_slot.insert(id, slot);
    }

    fn get_slot(&self, id: LocalId) -> Option<&HeapSlot> {
        self.id_to_slot.get(&id)
    }

    fn set_value(&mut self, id: LocalId, value: LocalId) {
        self.id_to_value.insert(id, value);
    }

    fn get_value(&self, id: LocalId) -> Option<LocalId> {
        self.id_to_value.get(&id).copied()
    }
}

/// Transform a single block, replacing heap operations with SSA operations
fn transform_block(
    block: &Block,
    block_id: &BlockId,
    ssa_builder: &mut SsaBuilder,
    id_mapping: &mut LocalIdMapping,
    shape: &HeapShape,
) -> Result<Block, String> {
    let mut new_instructions = Vec::new();

    for (target_id, instruction) in &block.instructions {
        match instruction {
            Instruction::GetGlobal { name, create_if_missing } => {
                if *create_if_missing {
                    return Err(format!("Shape-modifying GetGlobal: {}", name));
                }
                // Map this LocalId to the global slot
                let slot = HeapSlot::new(HeapPath::Global(name.clone()));
                id_mapping.set_slot(*target_id, slot);
                // Keep the instruction for now (will be used for pointer tracking)
                new_instructions.push((*target_id, instruction.clone()));
            }

            Instruction::Load { source } => {
                // Load dereferences a pointer - get the value from the slot
                if let Some(slot) = id_mapping.get_slot(*source).cloned() {
                    // Get the SSA version of this slot
                    let ssa_var = ssa_builder.get_at_end(block_id, &slot);
                    // Map the target to this value
                    id_mapping.set_value(*target_id, ssa_var);
                    // Replace with a copy operation (or just track the mapping)
                    // For now, emit a "virtual" load that will be resolved
                    new_instructions.push((
                        *target_id,
                        Instruction::Load { source: ssa_var },
                    ));
                } else {
                    // Loading from a non-tracked pointer - keep as is
                    new_instructions.push((*target_id, instruction.clone()));
                }
            }

            Instruction::Store { target, source } => {
                // Store writes to a pointer location
                if let Some(slot) = id_mapping.get_slot(*target).cloned() {
                    // Get the value being stored
                    let value = id_mapping.get_value(*source).unwrap_or(*source);
                    // Record this definition in the block
                    ssa_builder.define_in_block(block_id, slot.clone(), value);
                    // Don't emit the store - it's now an SSA definition
                } else {
                    // Storing to a non-tracked pointer - keep as is
                    new_instructions.push((*target_id, instruction.clone()));
                }
            }

            Instruction::GetField { receiver, field, create_if_missing } => {
                if *create_if_missing {
                    return Err(format!("Shape-modifying GetField: {}", field));
                }
                // Extend the path from receiver
                if let Some(base_slot) = id_mapping.get_slot(*receiver).cloned() {
                    let new_slot = HeapSlot::new(base_slot.path().clone().field(field));
                    id_mapping.set_slot(*target_id, new_slot);
                }
                new_instructions.push((*target_id, instruction.clone()));
            }

            Instruction::GetIndex { receiver, index, create_if_missing } => {
                if *create_if_missing {
                    return Err("Shape-modifying GetIndex".to_string());
                }
                // For now, don't track dynamic indices
                new_instructions.push((*target_id, instruction.clone()));
            }

            Instruction::Alloc => {
                // Allocation creates new heap shape - check if it escapes
                // For now, conservatively reject
                return Err("Alloc instruction - may modify heap shape".to_string());
            }

            Instruction::StoreEmptyTable { target } => {
                return Err("StoreEmptyTable - modifies heap shape".to_string());
            }

            Instruction::StoreClosure { target, fun_def, captures } => {
                return Err("StoreClosure - modifies heap shape".to_string());
            }

            Instruction::Call { closure, args } => {
                // Calls may have side effects - need to check if the callee is pure
                return Err("Call instruction - may have side effects".to_string());
            }

            Instruction::CallResolved { fun_name, .. } => {
                // CallResolved is still a call - may have side effects
                return Err(format!(
                    "CallResolved instruction ({}) - may have side effects",
                    fun_name.as_str()
                ));
            }

            // Pure operations - pass through
            Instruction::NumberConstant { .. }
            | Instruction::BoolConstant { .. }
            | Instruction::StringConstant { .. }
            | Instruction::NilConstant
            | Instruction::UnaryOp { .. }
            | Instruction::BinaryOp { .. }
            | Instruction::Phi { .. } => {
                new_instructions.push((*target_id, instruction.clone()));
            }
        }
    }

    Ok(Block {
        instructions: new_instructions,
        terminator: block.terminator.clone(),
        hint_normalize: block.hint_normalize,
    })
}

/// Attempt to eliminate heap operations from a CFG given a concrete heap shape.
pub fn eliminate_heap(
    cfg: &Cfg,
    shape: &HeapShape,
    local_gen: LocalIdGenerator,
    label_gen: LabelGenerator,
) -> HeapEliminationResult {
    // Collect all leaf slots from the shape
    let leaf_slots = shape.collect_leaf_slots();

    if leaf_slots.is_empty() {
        return HeapEliminationResult::NotApplicable;
    }

    // Compute predecessors
    let predecessors = compute_predecessors(cfg);

    // Find exit blocks
    let exit_blocks = find_exit_blocks(cfg);

    // Initialize SSA builder
    let mut ssa_builder = SsaBuilder::new(local_gen, label_gen);
    ssa_builder.predecessors = predecessors;

    // Create initial versions for all slots (unpack phase conceptually)
    let mut unpack_slots = Vec::new();
    for slot in &leaf_slots {
        let initial_id = ssa_builder.local_gen.next();
        ssa_builder.set_initial_version(slot.clone(), initial_id);
        unpack_slots.push((slot.clone(), initial_id));
    }

    // Transform blocks
    let mut id_mapping = LocalIdMapping::new();
    let mut modified_slots: FxHashSet<HeapSlot> = FxHashSet::default();

    // Transform entry block
    let entry_result = transform_block(
        &cfg.entry,
        &BlockId::Entry,
        &mut ssa_builder,
        &mut id_mapping,
        shape,
    );
    let transformed_entry = match entry_result {
        Ok(block) => block,
        Err(msg) => return HeapEliminationResult::ShapeNotPreserved(msg),
    };

    // Transform named blocks
    let mut transformed_named = FxHashMap::default();
    for (label, block) in &cfg.named {
        let block_id = BlockId::Named(label.clone());
        let result = transform_block(
            block,
            &block_id,
            &mut ssa_builder,
            &mut id_mapping,
            shape,
        );
        match result {
            Ok(transformed) => {
                transformed_named.insert(label.clone(), transformed);
            }
            Err(msg) => return HeapEliminationResult::ShapeNotPreserved(msg),
        }
    }

    // Collect final versions for repack
    let mut repack_slots = Vec::new();
    for slot in &leaf_slots {
        // Get final version from each exit block
        // For simplicity, we'll handle the single-exit case first
        if exit_blocks.len() == 1 {
            let final_id = ssa_builder.get_final_version(&exit_blocks[0], slot);
            repack_slots.push((slot.clone(), final_id));

            // Check if the slot was modified
            if let Some(&initial_id) = unpack_slots.iter().find(|(s, _)| s == slot).map(|(_, id)| id) {
                if final_id != initial_id {
                    modified_slots.insert(slot.clone());
                }
            }
        } else {
            // Multiple exit blocks - would need phi at exit or per-exit repack
            // For now, just take the first exit
            if !exit_blocks.is_empty() {
                let final_id = ssa_builder.get_final_version(&exit_blocks[0], slot);
                repack_slots.push((slot.clone(), final_id));
            }
        }
    }

    // Insert phi nodes into blocks
    let mut final_entry = transformed_entry;
    let mut final_named = transformed_named;

    for (block_id, phis) in ssa_builder.phi_nodes {
        let phi_instructions: Vec<_> = phis
            .into_iter()
            .map(|(target_id, _slot, branches)| {
                (target_id, Instruction::Phi { branches })
            })
            .collect();

        match block_id {
            BlockId::Entry => {
                let mut new_instrs = phi_instructions;
                new_instrs.extend(final_entry.instructions);
                final_entry.instructions = new_instrs;
            }
            BlockId::Named(label) => {
                if let Some(block) = final_named.get_mut(&label) {
                    let mut new_instrs = phi_instructions;
                    new_instrs.extend(block.instructions.clone());
                    block.instructions = new_instrs;
                }
            }
        }
    }

    let transformed_cfg = Cfg {
        entry: final_entry,
        named: final_named,
    };

    HeapEliminationResult::Success(TransformedCfg {
        cfg: transformed_cfg,
        unpack_slots,
        repack_slots,
        modified_slots,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pico8_num::Pico8Num;

    type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

    fn make_local_gen() -> LocalIdGenerator {
        LocalIdGenerator::new()
    }

    fn make_label_gen() -> LabelGenerator {
        LabelGenerator::new()
    }

    #[test]
    fn test_heap_path_construction() {
        let path = HeapPath::global("player").field("pos").field("x");
        // player.pos.x => Field(Field(Global("player"), "pos"), "x")
        match &path {
            HeapPath::Field(inner, f2) if f2 == "x" => {
                match inner.as_ref() {
                    HeapPath::Field(inner2, f1) if f1 == "pos" => {
                        match inner2.as_ref() {
                            HeapPath::Global(name) if name == "player" => {}
                            _ => panic!("Expected Global(player)"),
                        }
                    }
                    _ => panic!("Expected Field(_, pos)"),
                }
            }
            _ => panic!("Expected Field(_, x)"),
        }
    }

    #[test]
    fn test_heap_shape_collect_slots() {
        let mut shape = HeapShape::new();

        // Create shape: { player: { x: Leaf, y: Leaf } }
        let mut player_fields = FxHashMap::default();
        player_fields.insert("x".to_string(), ValueShape::Leaf);
        player_fields.insert("y".to_string(), ValueShape::Leaf);
        shape.globals.insert("player".to_string(), ValueShape::Table(player_fields));

        let slots = shape.collect_leaf_slots();
        assert_eq!(slots.len(), 2);
    }

    #[test]
    fn test_simple_cfg_no_heap() {
        // CFG: %0 = NumberConstant(5); return %0
        // Should return NotApplicable since there's nothing to transform
        let entry = Block {
            instructions: vec![(
                LocalId::from(0),
                Instruction::NumberConstant {
                    value: Pico8Num::from_i16(5),
                },
            )],
            terminator: (
                LocalId::from(1),
                Terminator::Return {
                    value: Some(LocalId::from(0)),
                },
            ),
            hint_normalize: false,
        };

        let cfg = Cfg {
            entry,
            named: FxHashMap::default(),
        };

        let shape = HeapShape::new(); // Empty shape
        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, local_gen, label_gen);
        assert!(matches!(result, HeapEliminationResult::NotApplicable));
    }

    #[test]
    fn test_simple_global_read() {
        // CFG: %0 = GetGlobal("x"); %1 = Load(%0); return %1
        let entry = Block {
            instructions: vec![
                (
                    LocalId::from(0),
                    Instruction::GetGlobal {
                        name: "x".to_string(),
                        create_if_missing: false,
                    },
                ),
                (LocalId::from(1), Instruction::Load { source: LocalId::from(0) }),
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
            named: FxHashMap::default(),
        };

        // Shape: x is a leaf
        let mut shape = HeapShape::new();
        shape.globals.insert("x".to_string(), ValueShape::Leaf);

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, local_gen, label_gen);
        match result {
            HeapEliminationResult::Success(transformed) => {
                assert_eq!(transformed.unpack_slots.len(), 1);
                assert_eq!(transformed.repack_slots.len(), 1);
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    #[test]
    fn test_shape_modifying_rejected() {
        // CFG: %0 = GetGlobal("x", create_if_missing=true)
        // Should be rejected because it modifies shape
        let entry = Block {
            instructions: vec![(
                LocalId::from(0),
                Instruction::GetGlobal {
                    name: "x".to_string(),
                    create_if_missing: true,
                },
            )],
            terminator: (LocalId::from(1), Terminator::Return { value: None }),
            hint_normalize: false,
        };

        let cfg = Cfg {
            entry,
            named: FxHashMap::default(),
        };

        let mut shape = HeapShape::new();
        shape.globals.insert("x".to_string(), ValueShape::Leaf);

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, local_gen, label_gen);
        assert!(matches!(result, HeapEliminationResult::ShapeNotPreserved(_)));
    }
}
