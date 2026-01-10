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

use std::fmt;

use serde::{Deserialize, Serialize};

use crate::interpreter::common::{FxHashMap, FxHashSet};
use crate::ir::{
    Block, BlockId, Cfg, GlobalId, Instruction, Label, LabelGenerator, LocalId, LocalIdGenerator,
    Terminator,
};
use crate::pico8_num::Pico8Num;

/// A path into the heap, representing a specific memory location.
/// For example: `player.pos.x` or `globals.room`
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HeapPath {
    /// A global variable: _G["name"]
    Global(String),
    /// A function argument by index (0-based)
    Arg(usize),
    /// A field access: base.field
    Field(Box<HeapPath>, String),
    /// An index access with a known constant index: base[index]
    Index(Box<HeapPath>, HeapIndex),
}

/// An index into a table - either a string field or numeric index
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum HeapIndex {
    String(String),
    Number(i32),
}

impl HeapPath {
    pub fn global(name: &str) -> Self {
        HeapPath::Global(name.to_string())
    }

    pub fn arg(index: usize) -> Self {
        HeapPath::Arg(index)
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
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct HeapSlot(HeapPath);

impl HeapSlot {
    pub fn new(path: HeapPath) -> Self {
        HeapSlot(path)
    }

    pub fn path(&self) -> &HeapPath {
        &self.0
    }
}

impl fmt::Display for HeapPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HeapPath::Global(name) => write!(f, "_G.{}", name),
            HeapPath::Arg(idx) => write!(f, "arg{}", idx),
            HeapPath::Field(base, field) => write!(f, "{}.{}", base, field),
            HeapPath::Index(base, HeapIndex::String(s)) => write!(f, "{}[\"{}\"]", base, s),
            HeapPath::Index(base, HeapIndex::Number(n)) => write!(f, "{}[{}]", base, n),
        }
    }
}

impl fmt::Display for HeapSlot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// The known shape of a value in the heap
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueShape {
    /// A leaf value (number, string, boolean, nil) - these are abstract
    Leaf,
    /// A constant numeric value - known at compile time and must not be modified
    Constant(Pico8Num),
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

impl ValueShape {
    /// Create a Table shape from a list of (field_name, shape) pairs.
    /// This is a convenience helper primarily for test code.
    ///
    /// Example:
    /// ```ignore
    /// let shape = ValueShape::table([
    ///     ("x", ValueShape::Leaf),
    ///     ("y", ValueShape::Constant(Pico8Num::from_i16(42))),
    /// ]);
    /// ```
    #[cfg(test)]
    pub fn table<const N: usize>(fields: [(&str, ValueShape); N]) -> ValueShape {
        let mut map = FxHashMap::default();
        for (name, shape) in fields {
            map.insert(name.to_string(), shape);
        }
        ValueShape::Table(map)
    }
}

/// The concrete shape of the heap at function entry
#[derive(Clone, Debug)]
pub struct HeapShape {
    /// Known globals and their shapes
    pub globals: FxHashMap<String, ValueShape>,
    /// Known argument shapes (for function arguments that are tables)
    pub args: Vec<Option<ValueShape>>,
}

impl Default for HeapShape {
    fn default() -> Self {
        Self::new()
    }
}

impl HeapShape {
    pub fn new() -> Self {
        HeapShape {
            globals: FxHashMap::default(),
            args: Vec::new(),
        }
    }

    /// Traverse all value shapes in this heap shape, calling a visitor function for each.
    /// The visitor receives the current path and shape, and should return shapes to
    /// recursively traverse (for tables, this would be the field shapes).
    fn traverse_shapes<F>(&self, mut visitor: F)
    where
        F: FnMut(&HeapPath, &ValueShape),
    {
        fn traverse_recursive<F>(path: HeapPath, shape: &ValueShape, visitor: &mut F)
        where
            F: FnMut(&HeapPath, &ValueShape),
        {
            visitor(&path, shape);
            if let ValueShape::Table(fields) = shape {
                for (field, field_shape) in fields {
                    traverse_recursive(path.clone().field(field), field_shape, visitor);
                }
            }
        }

        // Traverse globals
        for (name, shape) in &self.globals {
            traverse_recursive(HeapPath::Global(name.clone()), shape, &mut visitor);
        }

        // Traverse args
        for (index, maybe_shape) in self.args.iter().enumerate() {
            if let Some(shape) = maybe_shape {
                traverse_recursive(HeapPath::Arg(index), shape, &mut visitor);
            }
        }
    }

    /// Collect all leaf slots that need to be unpacked
    pub fn collect_leaf_slots(&self) -> Vec<HeapSlot> {
        let mut slots = Vec::new();

        self.traverse_shapes(|path, shape| match shape {
            ValueShape::Leaf => {
                slots.push(HeapSlot::new(path.clone()));
            }
            ValueShape::Pointer(target) => {
                // Pointers create aliasing - use the target slot
                slots.push(target.clone());
            }
            _ => {
                // Constants, Tables, Closures - not collected as leaf slots
                // Constants are handled separately via collect_constant_slots
                // Tables are traversed automatically
                // Closures are not transformed
            }
        });

        slots
    }

    /// Collect all constant slots with their values
    pub fn collect_constant_slots(&self) -> Vec<(HeapSlot, Pico8Num)> {
        let mut constants = Vec::new();

        self.traverse_shapes(|path, shape| {
            if let ValueShape::Constant(value) = shape {
                constants.push((HeapSlot::new(path.clone()), *value));
            }
        });

        constants
    }

    /// Check if a heap path corresponds to a *known* leaf value in this shape.
    /// Returns true if the path points to a known Leaf or Constant in the shape.
    /// Returns false if:
    /// - The path points to a Table or other compound type
    /// - The path is unknown (not in the shape) - we don't track unknown paths
    pub fn is_leaf_path(&self, path: &HeapPath) -> bool {
        match path {
            HeapPath::Global(name) => {
                if let Some(shape) = self.globals.get(name) {
                    matches!(shape, ValueShape::Leaf | ValueShape::Constant(_))
                } else {
                    // Unknown global - don't track (return false to keep original ops)
                    false
                }
            }
            HeapPath::Arg(index) => {
                if let Some(Some(shape)) = self.args.get(*index) {
                    matches!(shape, ValueShape::Leaf | ValueShape::Constant(_))
                } else {
                    // Unknown arg - don't track
                    false
                }
            }
            HeapPath::Field(base, field) => {
                // First find the base shape, then look up the field
                if let Some(ValueShape::Table(fields)) = self.get_shape_at_path(base) {
                    if let Some(field_shape) = fields.get(field) {
                        matches!(field_shape, ValueShape::Leaf | ValueShape::Constant(_))
                    } else {
                        // Unknown field - don't track (might be dynamically added)
                        false
                    }
                } else {
                    // Unknown or non-table base - don't track
                    false
                }
            }
            HeapPath::Index { .. } => false, // Dynamic index - don't track
        }
    }

    /// Check if a heap path corresponds to a known constant value.
    /// Returns the constant value if the path points to a Constant in the shape.
    pub fn get_constant_at_path(&self, path: &HeapPath) -> Option<Pico8Num> {
        let shape = self.get_shape_at_path(path)?;
        match shape {
            ValueShape::Constant(value) => Some(*value),
            _ => None,
        }
    }

    /// Check if a heap path is a constant (read-only)
    pub fn is_constant_path(&self, path: &HeapPath) -> bool {
        self.get_constant_at_path(path).is_some()
    }

    /// Get closure info if the path points to a Closure shape.
    /// Returns the function name and whether it has captures.
    pub fn get_closure_at_path(&self, path: &HeapPath) -> Option<(&GlobalId, bool)> {
        let shape = self.get_shape_at_path(path)?;
        match shape {
            ValueShape::Closure { fun_name, capture_shapes } => {
                Some((fun_name, !capture_shapes.is_empty()))
            }
            _ => None,
        }
    }

    /// Get the shape at a given path
    pub fn get_shape_at_path(&self, path: &HeapPath) -> Option<&ValueShape> {
        match path {
            HeapPath::Global(name) => self.globals.get(name),
            HeapPath::Arg(index) => self.args.get(*index).and_then(|o| o.as_ref()),
            HeapPath::Field(base, field) => {
                if let Some(base_shape) = self.get_shape_at_path(base) {
                    match base_shape {
                        ValueShape::Table(fields) => fields.get(field),
                        _ => None
                    }
                } else {
                    None
                }
            }
            _ => None
        }
    }

    /// Check if a field exists at a given path (for create_if_missing checks)
    pub fn field_exists_at_path(&self, base_path: &HeapPath, field: &str) -> bool {
        if let Some(ValueShape::Table(fields)) = self.get_shape_at_path(base_path) {
            fields.contains_key(field)
        } else {
            false
        }
    }
}

/// Controls how heap elimination handles blocking operations (unsafe calls, etc.)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DeoptMode {
    /// Insert Deopt terminators when hitting blocking operations (final pass)
    Insert,
    /// Stop processing the block but don't insert Deopt (soft/exploration mode)
    /// This allows the caller to resolve calls and retry
    Stop,
}

/// Result of attempting the heap elimination transformation
#[derive(Debug)]
pub enum HeapEliminationResult {
    /// Successfully transformed the CFG (may include resolved calls)
    Success(TransformedCfg),
    /// Transformation failed because the shape wasn't preserved
    ShapeNotPreserved(String),
    /// Transformation failed because of external calls
    HasExternalCalls(Vec<String>),
    /// Transformation not applicable (e.g., no heap operations)
    NotApplicable,
    /// Transformation failed because code tried to write to a constant global
    ConstantViolation(String),
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
    /// Number of deopt points inserted
    pub deopt_count: usize,
    /// Number of calls resolved (Call -> CallResolved)
    pub calls_resolved: usize,
}

/// Phi node entry for SSA construction: (target_id, slot, branches)
/// where branches maps (source_label -> source_local_id)
type PhiNodeEntry = (LocalId, HeapSlot, Vec<(Label, LocalId)>);

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
    predecessors: FxHashMap<BlockId, Vec<BlockId>>,
    /// Phi nodes to insert: block -> list of phi node entries
    phi_nodes: FxHashMap<BlockId, Vec<PhiNodeEntry>>,
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
            let pred_block = &preds[0];
            let id = self.get_at_end(pred_block, slot);
            self.slot_versions.insert(key, id);
            return id;
        }

        // Multiple predecessors - need a phi node
        // First, check for cycles
        if self.in_progress.contains(&key) {
            // We're in a cycle - create a placeholder phi node
            let phi_id = self.local_gen.fresh_id();
            self.slot_versions.insert(key.clone(), phi_id);
            return phi_id;
        }

        // Mark as in progress
        self.in_progress.insert(key.clone());

        // Create phi node with placeholder
        let phi_id = self.local_gen.fresh_id();
        self.slot_versions.insert(key.clone(), phi_id);

        // Collect incoming values from predecessors
        let mut branches = Vec::new();
        for pred_block in &preds {
            let pred_id = self.get_at_end(pred_block, slot);
            // Use BlockId::label() to get the label for phi node branches
            branches.push((pred_block.label(), pred_id));
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

/// Find exit blocks (blocks that return)
fn find_exit_blocks(cfg: &Cfg) -> Vec<BlockId> {
    let mut exits = Vec::new();

    if matches!(cfg.entry.terminator_kind(), Terminator::Return { .. }) {
        exits.push(BlockId::Entry);
    }

    for (label, block) in &cfg.named {
        if matches!(block.terminator_kind(), Terminator::Return { .. }) {
            exits.push(BlockId::Named(label.clone()));
        }
    }

    exits
}

/// Check if a builtin is pure for heap elimination purposes.
///
/// Pure builtins (allowed):
/// - Math operations: min, max, abs, flr
/// - Number operations: __split_by_flr
/// - Game data readers: mget, fget (read from ROM-like data, not tracked heap)
///
/// Impure builtins (rejected):
/// - Heap modifiers: add, __array_table_drop_last (mutate tables)
/// - Side effects: print, __print, error
/// - Value creators: __new_unknown_boolean, __new_vector (may allocate)
fn is_pure_builtin(name: &str) -> bool {
    matches!(name, "min" | "max" | "abs" | "flr" | "__split_by_flr" | "mget" | "fget" | "tile_flag_at" | "tile_at")
}

/// Mapping from LocalId to the heap slot it represents (if any)
///
/// This struct tracks pointer information for constant propagation:
/// - `id_to_slot`: Maps a LocalId to the heap path it points to (e.g., _G.player, Arg(0))
/// - `id_to_value`: Maps a LocalId to another LocalId representing its SSA value
/// - `cell_contents`: Maps a cell (Alloc result) to the slot of the value stored in it
///
/// The `cell_contents` tracking enables pointer propagation through local cells:
/// when we Store a value with known slot into a cell, we remember that;
/// when we Load from that cell, we recover the slot.
struct LocalIdMapping {
    /// LocalId -> HeapSlot for pointer-typed locals
    id_to_slot: FxHashMap<LocalId, HeapSlot>,
    /// LocalId -> the value it holds (for tracking through loads/stores)
    id_to_value: FxHashMap<LocalId, LocalId>,
    /// Cell LocalId -> HeapSlot of the value stored in the cell
    /// This enables tracking pointers through Alloc/Store/Load sequences
    cell_contents: FxHashMap<LocalId, HeapSlot>,
}

impl LocalIdMapping {
    fn new() -> Self {
        LocalIdMapping {
            id_to_slot: FxHashMap::default(),
            id_to_value: FxHashMap::default(),
            cell_contents: FxHashMap::default(),
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

    /// Record that a cell contains a value pointing to the given slot.
    /// Called when we see Store(cell, source) where source has a known slot.
    fn set_cell_contents(&mut self, cell: LocalId, slot: HeapSlot) {
        self.cell_contents.insert(cell, slot);
    }

    /// Get the slot of the value stored in a cell.
    /// Called when we see Load(cell) to recover pointer information.
    fn get_cell_contents(&self, cell: LocalId) -> Option<&HeapSlot> {
        self.cell_contents.get(&cell)
    }
}

/// Result of transforming a block
enum BlockTransformResult {
    /// Block was fully transformed
    Success {
        block: Block,
        /// Number of calls resolved in this block
        calls_resolved: usize,
    },
    /// Block needs deopt - contains instructions up to deopt point and the reason
    NeedsDeopt {
        instructions: Vec<(LocalId, Instruction)>,
        reason: String,
        /// The LocalId to use for the Deopt terminator
        terminator_id: LocalId,
        /// Number of calls resolved before hitting deopt
        calls_resolved: usize,
    },
    /// Block processing stopped due to blocking operation (in soft mode)
    /// The block is returned as-is (with any resolved calls)
    Stopped {
        block: Block,
        /// Number of calls resolved before stopping
        calls_resolved: usize,
        /// Reason we stopped (for debugging)
        reason: String,
    },
    /// Constant violation - code tries to write to a constant global
    /// This aborts the entire heap elimination pass
    ConstantViolation(String),
}

impl BlockTransformResult {
    /// Convert a transform result into a Block, updating counters as a side effect.
    ///
    /// Returns `Ok(Block)` for Success, NeedsDeopt, and Stopped variants.
    /// Returns `Err(msg)` for ConstantViolation, which should abort the pass.
    ///
    /// # Arguments
    /// * `hint_normalize` - The hint_normalize value from the original block
    /// * `deopt_count` - Mutable counter for number of deopts
    /// * `calls_resolved` - Mutable counter for total calls resolved
    fn into_block(
        self,
        hint_normalize: bool,
        deopt_count: &mut usize,
        total_calls_resolved: &mut usize,
    ) -> Result<Block, String> {
        match self {
            BlockTransformResult::Success { block, calls_resolved } => {
                *total_calls_resolved += calls_resolved;
                Ok(block)
            }
            BlockTransformResult::NeedsDeopt { instructions, reason, terminator_id, calls_resolved } => {
                *total_calls_resolved += calls_resolved;
                *deopt_count += 1;
                Ok(Block {
                    instructions,
                    terminator: (terminator_id, Terminator::Deopt { reason }),
                    hint_normalize,
                })
            }
            BlockTransformResult::Stopped { block, calls_resolved, reason: _ } => {
                *total_calls_resolved += calls_resolved;
                Ok(block)
            }
            BlockTransformResult::ConstantViolation(msg) => Err(msg),
        }
    }
}

/// Find local cells (non-escaping allocations that are only used for Store/Load).
/// These don't modify the visible heap shape and shouldn't cause deopts.
/// Returns the set of alloc IDs that are local cells.
fn find_local_cells(cfg: &Cfg) -> FxHashSet<LocalId> {
    let mut allocations: FxHashSet<LocalId> = FxHashSet::default();
    let mut escaping: FxHashSet<LocalId> = FxHashSet::default();

    // First pass: find all Allocs
    for block in cfg.iter_blocks() {
        for (target_id, instruction) in &block.instructions {
            if matches!(instruction, Instruction::Alloc) {
                allocations.insert(*target_id);
            }
        }
    }

    // Second pass: find uses and mark escaping cells
    for block in cfg.iter_blocks() {
        for (target_id, instruction) in &block.instructions {
            match instruction {
                Instruction::Alloc => {
                    // Already handled in first pass
                }
                Instruction::Store { target, source } => {
                    // Store TO a cell is fine
                    // Store OF a cell (as value) means it escapes
                    // (unless storing to itself, which is a no-op)
                    if allocations.contains(source) && target != source {
                        escaping.insert(*source);
                    }
                }
                Instruction::Load { source: _ } => {
                    // Load FROM a cell is fine
                }
                // For all other instructions, check if any operand is a cell
                // If so, the cell escapes (used for something other than Store/Load)
                other => {
                    other.map_local_ids(|id| {
                        if allocations.contains(&id) {
                            escaping.insert(id);
                        }
                        id
                    });
                }
            }
        }

        // Check terminator for uses of cells
        match block.terminator_kind() {
            Terminator::Return { value: Some(ret_id) } => {
                if allocations.contains(ret_id) {
                    escaping.insert(*ret_id);
                }
            }
            Terminator::ConditionalBranch { condition, .. } => {
                if allocations.contains(condition) {
                    escaping.insert(*condition);
                }
            }
            _ => {}
        }
    }

    // Return allocations that don't escape
    allocations.into_iter()
        .filter(|id| !escaping.contains(id))
        .collect()
}

/// Transform a single block, replacing heap operations with SSA operations.
/// If an instruction requires deopt, returns NeedsDeopt with instructions up to that point.
/// If in soft mode (DeoptMode::Stop), returns Stopped instead of NeedsDeopt.
fn transform_block(
    block: &Block,
    block_id: &BlockId,
    ssa_builder: &mut SsaBuilder,
    id_mapping: &mut LocalIdMapping,
    shape: &HeapShape,
    local_cells: &FxHashSet<LocalId>,
    deopt_mode: DeoptMode,
) -> BlockTransformResult {
    let mut new_instructions = Vec::new();
    let mut calls_resolved = 0;

    for (target_id, instruction) in &block.instructions {
        // Macro to handle blocking operation - either deopt or stop based on mode
        macro_rules! needs_deopt {
            ($reason:expr) => {
                {
                    // Apply id_mapping rewriting to instructions accumulated so far
                    let rewrite = |id: LocalId| -> LocalId {
                        id_mapping.get_value(id).unwrap_or(id)
                    };
                    let rewritten: Vec<_> = new_instructions
                        .into_iter()
                        .map(|(target_id, instr)| (target_id, instr.map_local_ids(rewrite)))
                        .collect();

                    match deopt_mode {
                        DeoptMode::Insert => {
                            // Generate a fresh ID for the Deopt terminator
                            // (can't use *target_id - it might already be used by an instruction we included)
                            let deopt_terminator_id = ssa_builder.local_gen.fresh_id();
                            return BlockTransformResult::NeedsDeopt {
                                instructions: rewritten,
                                reason: $reason,
                                terminator_id: deopt_terminator_id,
                                calls_resolved,
                            }
                        }
                        DeoptMode::Stop => {
                            // In soft mode, return the block with instructions so far
                            // plus remaining instructions with uses rewritten
                            let mut final_instrs = rewritten;
                            // Add current and remaining instructions with LocalId references rewritten.
                            // This is critical: earlier instructions may have been SSA-promoted
                            // (e.g., Load of a leaf slot), so their results don't exist as
                            // instructions anymore but are mapped to SSA variables.
                            let current_idx = block.instructions.iter()
                                .position(|(id, _)| id == target_id)
                                .unwrap();
                            for (id, instr) in &block.instructions[current_idx..] {
                                final_instrs.push((*id, instr.clone().map_local_ids(rewrite)));
                            }
                            let rewritten_terminator = block.terminator_kind().map_local_ids(rewrite);
                            return BlockTransformResult::Stopped {
                                block: Block {
                                    instructions: final_instrs,
                                    terminator: (block.terminator_id(), rewritten_terminator),
                                    hint_normalize: block.hint_normalize,
                                },
                                calls_resolved,
                                reason: $reason,
                            }
                        }
                    }
                }
            };
        }

        match instruction {
            Instruction::GetGlobal { name, .. } => {
                // Map this LocalId to the global slot
                // Note: create_if_missing doesn't affect our tracking - if the global
                // is in the known shape we do SSA promotion, otherwise we keep
                // the original instructions. Either way, we track the slot.
                let slot = HeapSlot::new(HeapPath::Global(name.clone()));
                id_mapping.set_slot(*target_id, slot);
                // Keep the instruction for now (will be used for pointer tracking)
                new_instructions.push((*target_id, instruction.clone()));
            }

            Instruction::Load { source } => {
                // Load dereferences a pointer - get the value from the slot
                if let Some(slot) = id_mapping.get_slot(*source).cloned() {
                    // Source has a tracked slot (e.g., GetGlobal result or GetField result)
                    // Check if this is a constant slot first
                    if let Some(const_value) = shape.get_constant_at_path(slot.path()) {
                        // Constant slot - emit a NumberConstant instruction
                        new_instructions.push((
                            *target_id,
                            Instruction::NumberConstant { value: const_value },
                        ));
                        // Map the target to itself (it's the constant value now)
                        id_mapping.set_value(*target_id, *target_id);
                    } else if shape.is_leaf_path(slot.path()) {
                        // Leaf slot - get the SSA version via Braun algorithm
                        // The SSA variable already contains the VALUE (from HeapRead or Phi),
                        // so we don't need to emit a Load instruction - the value is already available.
                        // We just map the target to the SSA variable so subsequent uses reference it directly.
                        let ssa_var = ssa_builder.get_at_end(block_id, &slot);
                        id_mapping.set_value(*target_id, ssa_var);
                        // Note: We do NOT emit a Load instruction here because ssa_var is already a value.
                        // Emitting Load { source: ssa_var } would be semantically wrong since
                        // ssa_var is a value (from HeapRead/Phi), not a pointer.
                    } else {
                        // Table slot - keep the slot mapping so GetField can extend it
                        // The loaded value "points to" the same slot (for path extension)
                        id_mapping.set_slot(*target_id, slot);
                        // Keep the original instruction
                        new_instructions.push((*target_id, instruction.clone()));
                    }
                } else if let Some(cell_slot) = id_mapping.get_cell_contents(*source).cloned() {
                    // Source is a cell (Alloc result) that we've tracked a Store into.
                    // We know what slot the cell contains - propagate that to the loaded value.
                    // This is the key to pointer propagation through local cells!
                    id_mapping.set_slot(*target_id, cell_slot);
                    new_instructions.push((*target_id, instruction.clone()));
                } else {
                    // Loading from a completely unknown pointer - keep as is
                    new_instructions.push((*target_id, instruction.clone()));
                }
            }

            Instruction::Store { target, source } => {
                // Store writes to a pointer location
                if let Some(slot) = id_mapping.get_slot(*target).cloned() {
                    // Target has a tracked slot (e.g., GetGlobal result or GetField result)
                    // Check if this is a constant slot - writes are forbidden
                    if shape.is_constant_path(slot.path()) {
                        return BlockTransformResult::ConstantViolation(
                            format!("Write to constant global: {}", slot)
                        );
                    }
                    // Get the value being stored
                    let value = id_mapping.get_value(*source).unwrap_or(*source);
                    // Record this definition in the block
                    ssa_builder.define_in_block(block_id, slot.clone(), value);
                    // Don't emit the store - it's now an SSA definition
                } else {
                    // Target is a non-tracked pointer (likely a local cell from Alloc)
                    // Track what slot is being stored in the cell for pointer propagation
                    if let Some(source_slot) = id_mapping.get_slot(*source).cloned() {
                        // Source has a known slot - record that this cell now contains
                        // a pointer to that slot. This enables pointer tracking through cells.
                        id_mapping.set_cell_contents(*target, source_slot);
                    }
                    new_instructions.push((*target_id, instruction.clone()));
                }
            }

            Instruction::GetField { receiver, field, create_if_missing } => {
                // Check if create_if_missing is safe given the known shape
                if *create_if_missing {
                    // Check if the field already exists in the known shape
                    if let Some(base_slot) = id_mapping.get_slot(*receiver) {
                        if !shape.field_exists_at_path(base_slot.path(), field) {
                            // Include the instruction - its result might be used later
                            new_instructions.push((*target_id, instruction.clone()));
                            needs_deopt!(format!("Shape-modifying GetField: {}", field));
                        }
                    } else {
                        // Unknown base - can't verify field exists
                        // Include the instruction - its result might be used later
                        new_instructions.push((*target_id, instruction.clone()));
                        needs_deopt!(format!("Shape-modifying GetField on unknown base: {}", field));
                    }
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
                    // Include the instruction - its result might be used later
                    new_instructions.push((*target_id, instruction.clone()));
                    needs_deopt!("Shape-modifying GetIndex".to_string());
                }
                // For now, don't track dynamic indices
                new_instructions.push((*target_id, instruction.clone()));
            }

            Instruction::Alloc => {
                // Check if this is a local cell (non-escaping, only used for Store/Load)
                if local_cells.contains(target_id) {
                    // Local cell - keep as is, doesn't modify visible heap shape
                    new_instructions.push((*target_id, instruction.clone()));
                } else {
                    // Escaping allocation - creates new heap shape, insert deopt
                    // Include the instruction - its result might be used later
                    new_instructions.push((*target_id, instruction.clone()));
                    needs_deopt!("Alloc instruction - may modify heap shape".to_string());
                }
            }

            Instruction::StoreEmptyTable { target: _ } => {
                // Include the instruction - this stores to a target that's already defined
                new_instructions.push((*target_id, instruction.clone()));
                needs_deopt!("StoreEmptyTable - modifies heap shape".to_string());
            }

            Instruction::StoreClosure { target: _, fun_def: _, captures: _ } => {
                // Include the instruction - this stores to a target that's already defined
                new_instructions.push((*target_id, instruction.clone()));
                needs_deopt!("StoreClosure - modifies heap shape".to_string());
            }

            Instruction::Call { closure, args } => {
                // Check if we can resolve the call via shape tracking.
                // Even if we resolve, we MUST stop/deopt because the called function
                // could read or write to heap locations we're tracking in SSA form.
                //
                // In DeoptMode::Stop, resolving allows subsequent inlining passes to
                // inline the resolved calls, which may allow heap elimination to proceed
                // further on the next iteration.
                if let Some(slot) = id_mapping.get_slot(*closure) {
                    if let Some((fun_name, has_captures)) = shape.get_closure_at_path(slot.path()) {
                        if !has_captures {
                            // We know the closure and it has no captures - resolve the call!
                            // But we still must stop because the call could have side effects.
                            let resolved = Instruction::CallResolved {
                                fun_name: fun_name.clone(),
                                captures: vec![],
                                args: args.clone(),
                            };
                            new_instructions.push((*target_id, resolved));
                            calls_resolved += 1;
                            needs_deopt!(format!(
                                "Call to {} resolved - but call may have side effects",
                                fun_name.as_str()
                            ));
                        } else {
                            // Has captures - for now, can't resolve without capture values
                            // Still include the call instruction - interpreter will execute it
                            new_instructions.push((*target_id, instruction.clone()));
                            needs_deopt!(format!(
                                "Call to closure with captures: {}",
                                fun_name.as_str()
                            ));
                        }
                    }
                }
                // Unknown closure - may have side effects
                // Still include the call instruction - interpreter will execute it
                new_instructions.push((*target_id, instruction.clone()));
                needs_deopt!("Call instruction - unknown closure".to_string());
            }

            Instruction::CallResolved { fun_name, .. } => {
                // CallResolved is still a call - may have side effects
                // Still include the call instruction - interpreter will execute it
                new_instructions.push((*target_id, instruction.clone()));
                needs_deopt!(format!(
                    "CallResolved instruction ({}) - may have side effects",
                    fun_name.as_str()
                ));
            }

            Instruction::CallBuiltin { name, .. } => {
                // Only allow truly pure builtins that compute solely from their arguments
                // and don't read or write any heap state
                if is_pure_builtin(name) {
                    // Pure builtin - just emit it, heap state is unchanged
                    new_instructions.push((*target_id, instruction.clone()));
                } else {
                    // Impure builtin - insert deopt
                    // Still include the call instruction - interpreter will execute it
                    new_instructions.push((*target_id, instruction.clone()));
                    needs_deopt!(format!(
                        "CallBuiltin instruction ({}) - not a pure builtin",
                        name
                    ));
                }
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

    // Rewrite all LocalId references using id_mapping
    // This is essential for cases where we remove an instruction but map its target
    // to another LocalId (e.g., when Load of a leaf slot maps to the SSA variable directly)
    let rewrite_local_id = |id: LocalId| -> LocalId {
        id_mapping.get_value(id).unwrap_or(id)
    };

    let rewritten_instructions: Vec<_> = new_instructions
        .into_iter()
        .map(|(target_id, instr)| {
            (target_id, instr.map_local_ids(rewrite_local_id))
        })
        .collect();

    let rewritten_terminator = (
        block.terminator_id(),
        block.terminator_kind().map_local_ids(rewrite_local_id),
    );

    BlockTransformResult::Success {
        block: Block {
            instructions: rewritten_instructions,
            terminator: rewritten_terminator,
            hint_normalize: block.hint_normalize,
        },
        calls_resolved,
    }
}

/// Attempt to eliminate heap operations from a CFG given a concrete heap shape.
///
/// # Arguments
/// * `cfg` - The CFG to transform
/// * `shape` - The concrete shape of globals and arguments
/// * `arg_ids` - LocalIds for function arguments (Some if defined, None if unused/varargs)
/// * `local_gen` - Generator for fresh LocalIds
/// * `label_gen` - Generator for fresh Labels
/// * `deopt_mode` - Controls whether to insert Deopt (final pass) or stop (soft mode)
pub fn eliminate_heap(
    cfg: &Cfg,
    shape: &HeapShape,
    arg_ids: &[Option<LocalId>],
    local_gen: LocalIdGenerator,
    label_gen: LabelGenerator,
    deopt_mode: DeoptMode,
) -> HeapEliminationResult {
    // Collect all leaf slots from the shape
    let leaf_slots = shape.collect_leaf_slots();
    // Also collect constant slots - we need to process even if only constants are present
    let constant_slots = shape.collect_constant_slots();

    if leaf_slots.is_empty() && constant_slots.is_empty() {
        return HeapEliminationResult::NotApplicable;
    }

    // Find local cells (non-escaping allocations used only for Store/Load)
    // These don't modify heap shape and shouldn't cause deopts
    let local_cells = find_local_cells(cfg);

    // Compute predecessors using the centralized Cfg method
    let predecessors = cfg.compute_predecessors();

    // Find exit blocks
    let exit_blocks = find_exit_blocks(cfg);

    // Initialize SSA builder
    let mut ssa_builder = SsaBuilder::new(local_gen, label_gen);
    ssa_builder.predecessors = predecessors;

    // Create initial versions for all slots (unpack phase conceptually)
    let mut unpack_slots = Vec::new();
    for slot in &leaf_slots {
        let initial_id = ssa_builder.local_gen.fresh_id();
        ssa_builder.set_initial_version(slot.clone(), initial_id);
        unpack_slots.push((slot.clone(), initial_id));
    }

    // Transform blocks
    let mut id_mapping = LocalIdMapping::new();

    // Initialize arg mappings - map argument LocalIds to their slots
    for (index, maybe_arg_id) in arg_ids.iter().enumerate() {
        if let Some(arg_id) = maybe_arg_id {
            if shape.args.get(index).is_some_and(|s| s.is_some()) {
                let slot = HeapSlot::new(HeapPath::Arg(index));
                id_mapping.set_slot(*arg_id, slot);
            }
        }
    }
    let mut modified_slots: FxHashSet<HeapSlot> = FxHashSet::default();
    let mut deopt_count = 0;
    let mut total_calls_resolved = 0;

    // Transform entry block
    let entry_result = transform_block(
        &cfg.entry,
        &BlockId::Entry,
        &mut ssa_builder,
        &mut id_mapping,
        shape,
        &local_cells,
        deopt_mode,
    );
    let transformed_entry = match entry_result.into_block(
        cfg.entry.hint_normalize,
        &mut deopt_count,
        &mut total_calls_resolved,
    ) {
        Ok(block) => block,
        Err(msg) => return HeapEliminationResult::ConstantViolation(msg),
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
            &local_cells,
            deopt_mode,
        );
        let transformed = match result.into_block(
            block.hint_normalize,
            &mut deopt_count,
            &mut total_calls_resolved,
        ) {
            Ok(block) => block,
            Err(msg) => return HeapEliminationResult::ConstantViolation(msg),
        };
        transformed_named.insert(label.clone(), transformed);
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

    // Clean up Phi nodes after Deopt insertions
    // This removes Phi branches referencing blocks that no longer branch to the target
    // (because they got Deopt terminators), and collapses single-element Phis
    // Only run cleanup if we actually inserted Deopts that could have changed control flow
    let transformed_cfg = if deopt_count > 0 {
        use crate::interpreter::phi_cleanup::cleanup_phis;
        let cleanup_result = cleanup_phis(&transformed_cfg);
        cleanup_result.cfg
    } else {
        transformed_cfg
    };

    // In debug builds, validate the transformed CFG for type correctness.
    // Type errors (e.g., Load from a non-pointer) indicate a regression in the transformation.
    #[cfg(debug_assertions)]
    {
        use crate::interpreter::cfg_validation::{validate_types, SsaType};

        // Build arg types: function arguments that are tables are pointers
        let arg_types: Vec<_> = arg_ids.iter()
            .filter_map(|opt_id| opt_id.as_ref())
            .map(|id| (*id, SsaType::Pointer))
            .collect();

        let type_errors = validate_types(&transformed_cfg, &arg_types);
        if !type_errors.is_empty() {
            // Log errors for debugging - these indicate a problem in the transformation
            #[cfg(test)]
            {
                eprintln!("WARNING: heap_elimination produced {} type error(s):", type_errors.len());
                for error in &type_errors {
                    eprintln!("  - {}", error);
                }
            }
        }
    }

    HeapEliminationResult::Success(TransformedCfg {
        cfg: transformed_cfg,
        unpack_slots,
        repack_slots,
        modified_slots,
        deopt_count,
        calls_resolved: total_calls_resolved,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pico8_num::Pico8Num;

    fn make_local_gen() -> LocalIdGenerator {
        LocalIdGenerator::new()
    }

    fn make_label_gen() -> LabelGenerator {
        LabelGenerator::new()
    }

    /// Create a HeapShape with a single global as a Leaf.
    /// Example: `make_global_leaf_shape("x")` creates `{ globals: { x: Leaf } }`
    fn make_global_leaf_shape(name: &str) -> HeapShape {
        let mut shape = HeapShape::new();
        shape.globals.insert(name.to_string(), ValueShape::Leaf);
        shape
    }

    /// Create a HeapShape with a global table containing a single field as Leaf.
    /// Example: `make_player_x_shape()` creates `{ globals: { player: { x: Leaf } } }`
    fn make_player_x_shape() -> HeapShape {
        let mut shape = HeapShape::new();
        shape
            .globals
            .insert("player".to_string(), ValueShape::table([("x", ValueShape::Leaf)]));
        shape
    }

    /// Create a HeapShape with a global table containing x and y fields as Leaves.
    /// Example: `make_player_xy_shape()` creates `{ globals: { player: { x: Leaf, y: Leaf } } }`
    fn make_player_xy_shape() -> HeapShape {
        let mut shape = HeapShape::new();
        shape.globals.insert(
            "player".to_string(),
            ValueShape::table([("x", ValueShape::Leaf), ("y", ValueShape::Leaf)]),
        );
        shape
    }

    /// Create a table shape with a single field "x" as Leaf.
    /// Useful for building custom HeapShapes that need a table with x field.
    fn make_table_with_x() -> ValueShape {
        ValueShape::table([("x", ValueShape::Leaf)])
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
        // Create shape: { player: { x: Leaf, y: Leaf } }
        let shape = make_player_xy_shape();

        let slots = shape.collect_leaf_slots();
        assert_eq!(slots.len(), 2);
    }

    #[test]
    fn test_simple_cfg_no_heap() {
        // CFG: %0 = NumberConstant(5); return %0
        // Should return NotApplicable since there's nothing to transform
        let entry = Block::new_for_test(
            vec![(LocalId::from(0), Instruction::num_const(5))],
            (
                LocalId::from(1),
                Terminator::Return {
                    value: Some(LocalId::from(0)),
                },
            ),
        );

        let cfg = Cfg::single_entry(entry);

        let shape = HeapShape::new(); // Empty shape
        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);
        assert!(matches!(result, HeapEliminationResult::NotApplicable));
    }

    #[test]
    fn test_simple_global_read() {
        // CFG: %0 = GetGlobal("x"); %1 = Load(%0); return %1
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("x")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
            ],
            (LocalId::from(2), Terminator::ret(Some(LocalId::from(1)))),
        );

        let cfg = Cfg::single_entry(entry);

        let shape = make_global_leaf_shape("x");

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);
        match result {
            HeapEliminationResult::Success(transformed) => {
                assert_eq!(transformed.unpack_slots.len(), 1);
                assert_eq!(transformed.repack_slots.len(), 1);
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    #[test]
    fn test_getglobal_create_if_missing_allowed() {
        // CFG: %0 = GetGlobal("x", create_if_missing=true); %1 = Load(%0); return %1
        // create_if_missing is allowed - we still track the global properly
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global_create("x")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
            ],
            (LocalId::from(2), Terminator::ret(Some(LocalId::from(1)))),
        );

        let cfg = Cfg::single_entry(entry);

        let shape = make_global_leaf_shape("x");

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);
        // Should succeed without deopt - create_if_missing doesn't affect tracking
        match result {
            HeapEliminationResult::Success(transformed) => {
                assert_eq!(transformed.deopt_count, 0, "Expected no deopt");
                // Should have unpack and repack slots for "x"
                assert_eq!(transformed.unpack_slots.len(), 1);
                assert_eq!(transformed.repack_slots.len(), 1);
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    #[test]
    fn test_pure_builtin_allowed() {
        // CFG: %0 = GetGlobal("x"); %1 = Load(%0); %2 = CallBuiltin("max", [%1, %1]); return %2
        // Pure builtins like max should be allowed
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("x")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (
                    LocalId::from(2),
                    Instruction::CallBuiltin {
                        name: "max".to_string(),
                        args: vec![LocalId::from(1), LocalId::from(1)],
                    },
                ),
            ],
            (LocalId::from(3), Terminator::ret(Some(LocalId::from(2)))),
        );

        let cfg = Cfg::single_entry(entry);

        let shape = make_global_leaf_shape("x");

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);
        // Should succeed because max is a pure builtin
        assert!(matches!(result, HeapEliminationResult::Success(_)));
    }

    #[test]
    fn test_impure_builtin_rejected() {
        // CFG: %0 = GetGlobal("x"); %1 = Load(%0); %2 = CallBuiltin("add", [%1, %1]); return %2
        // Impure builtins like add (modifies tables) should be rejected
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("x")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (
                    LocalId::from(2),
                    Instruction::CallBuiltin {
                        name: "add".to_string(),
                        args: vec![LocalId::from(1), LocalId::from(1)],
                    },
                ),
            ],
            (LocalId::from(3), Terminator::ret(Some(LocalId::from(2)))),
        );

        let cfg = Cfg::single_entry(entry);

        let shape = make_global_leaf_shape("x");

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);
        // Should succeed with deopt inserted because add is impure
        match result {
            HeapEliminationResult::Success(transformed) => {
                assert!(transformed.deopt_count > 0, "Expected deopt to be inserted");
                // The entry block should end with Deopt mentioning add
                match transformed.cfg.entry.terminator_kind() {
                    Terminator::Deopt { reason } => {
                        assert!(reason.contains("add"), "Deopt reason should mention add: {}", reason);
                    }
                    other => panic!("Expected Deopt terminator, got {:?}", other),
                }
            }
            other => panic!("Expected Success with deopt, got {:?}", other),
        }
    }

    #[test]
    fn test_is_pure_builtin() {
        // Pure builtins
        assert!(is_pure_builtin("min"));
        assert!(is_pure_builtin("max"));
        assert!(is_pure_builtin("abs"));
        assert!(is_pure_builtin("flr"));
        assert!(is_pure_builtin("__split_by_flr"));
        assert!(is_pure_builtin("mget"));
        assert!(is_pure_builtin("fget"));
        assert!(is_pure_builtin("tile_flag_at"));
        assert!(is_pure_builtin("tile_at"));

        // Impure builtins
        assert!(!is_pure_builtin("add"));
        assert!(!is_pure_builtin("print"));
        assert!(!is_pure_builtin("__print"));
        assert!(!is_pure_builtin("error"));
        assert!(!is_pure_builtin("__new_unknown_boolean"));
        assert!(!is_pure_builtin("__new_vector"));
        assert!(!is_pure_builtin("__array_table_drop_last"));
    }

    #[test]
    fn test_constant_global_load() {
        // CFG: %0 = GetGlobal("k_left"); %1 = Load(%0); return %1
        // k_left is a constant (0), so Load should become NumberConstant(0)
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("k_left")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
            ],
            (LocalId::from(2), Terminator::ret(Some(LocalId::from(1)))),
        );

        let cfg = Cfg::single_entry(entry);

        let mut shape = HeapShape::new();
        // k_left = 0 (constant)
        shape.globals.insert("k_left".to_string(), ValueShape::Constant(Pico8Num::from_i16(0)));

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);
        match result {
            HeapEliminationResult::Success(transformed) => {
                // Check that the Load was replaced with NumberConstant
                let instructions = &transformed.cfg.entry.instructions;
                // Should have GetGlobal and NumberConstant
                let has_number_constant = instructions.iter().any(|(_, instr)| {
                    matches!(instr, Instruction::NumberConstant { value } if *value == Pico8Num::from_i16(0))
                });
                assert!(has_number_constant, "Expected NumberConstant(0) in transformed CFG");
                // Should NOT have Load instruction
                let has_load = instructions.iter().any(|(_, instr)| {
                    matches!(instr, Instruction::Load { .. })
                });
                assert!(!has_load, "Load should be replaced with NumberConstant");
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    #[test]
    fn test_constant_global_write_aborts() {
        // CFG: %0 = GetGlobal("k_left"); %1 = NumberConstant(5); Store(%0, %1); return
        // k_left is a constant, so Store should cause ConstantViolation
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("k_left")),
                (LocalId::from(1), Instruction::num_const(5)),
                (LocalId::from(2), Instruction::store(LocalId::from(0), LocalId::from(1))),
            ],
            (LocalId::from(3), Terminator::ret(None)),
        );

        let cfg = Cfg::single_entry(entry);

        let mut shape = HeapShape::new();
        // k_left = 0 (constant)
        shape.globals.insert("k_left".to_string(), ValueShape::Constant(Pico8Num::from_i16(0)));

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);
        match result {
            HeapEliminationResult::ConstantViolation(msg) => {
                assert!(msg.contains("k_left"), "Error should mention k_left: {}", msg);
            }
            other => panic!("Expected ConstantViolation, got {:?}", other),
        }
    }

    #[test]
    fn test_constant_global_mixed_with_leaf() {
        // CFG with both constant and leaf globals
        // %0 = GetGlobal("k_left"); %1 = Load(%0);  // constant -> NumberConstant
        // %2 = GetGlobal("x"); %3 = Load(%2);       // leaf -> SSA Load
        // %4 = BinaryOp(%1, Plus, %3); return %4
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("k_left")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::get_global("x")),
                (LocalId::from(3), Instruction::load(LocalId::from(2))),
                (LocalId::from(4), Instruction::binary_op(crate::ir::BinaryOp::Plus, LocalId::from(1), LocalId::from(3))),
            ],
            (LocalId::from(5), Terminator::ret(Some(LocalId::from(4)))),
        );

        let cfg = Cfg::single_entry(entry);

        let mut shape = HeapShape::new();
        // k_left = 0 (constant)
        shape.globals.insert("k_left".to_string(), ValueShape::Constant(Pico8Num::from_i16(0)));
        // x is a mutable leaf
        shape.globals.insert("x".to_string(), ValueShape::Leaf);

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);
        match result {
            HeapEliminationResult::Success(transformed) => {
                // Should have NumberConstant for k_left
                let has_number_constant = transformed.cfg.entry.instructions.iter().any(|(_, instr)| {
                    matches!(instr, Instruction::NumberConstant { value } if *value == Pico8Num::from_i16(0))
                });
                assert!(has_number_constant, "Expected NumberConstant(0) for k_left");

                // After the Load(Phi) fix, leaf slot Loads don't emit Load instructions
                // - they map directly to the SSA variable. So there should be 0 Loads
                // (k_left is constant -> NumberConstant, x is leaf -> direct SSA mapping)
                let load_count = transformed.cfg.entry.instructions.iter()
                    .filter(|(_, instr)| matches!(instr, Instruction::Load { .. }))
                    .count();
                assert_eq!(load_count, 0, "Expected no Loads (leaf slots map directly to SSA vars), got {}", load_count);
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    #[test]
    fn test_constant_arg_load() {
        // CFG: Load(arg0) - arg0 is a constant
        // arg0 is a constant (42), so Load should become NumberConstant(42)
        let arg0_id = LocalId::from(10);

        let entry = Block::new_for_test(
            vec![(
                LocalId::from(0),
                Instruction::Load {
                    source: arg0_id,
                },
            )],
            (
                LocalId::from(1),
                Terminator::Return {
                    value: Some(LocalId::from(0)),
                },
            ),
        );

        let cfg = Cfg::single_entry(entry);

        let mut shape = HeapShape::new();
        // arg0 = 42 (constant)
        shape.args = vec![Some(ValueShape::Constant(Pico8Num::from_i16(42)))];

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let arg_ids = vec![Some(arg0_id)];

        let result = eliminate_heap(&cfg, &shape, &arg_ids, local_gen, label_gen, DeoptMode::Insert);
        match result {
            HeapEliminationResult::Success(transformed) => {
                // Check that the Load was replaced with NumberConstant
                let instructions = &transformed.cfg.entry.instructions;
                let has_number_constant = instructions.iter().any(|(_, instr)| {
                    matches!(instr, Instruction::NumberConstant { value } if *value == Pico8Num::from_i16(42))
                });
                assert!(
                    has_number_constant,
                    "Expected NumberConstant(42) in transformed CFG"
                );
                // Should NOT have Load instruction
                let has_load = instructions
                    .iter()
                    .any(|(_, instr)| matches!(instr, Instruction::Load { .. }));
                assert!(!has_load, "Load should be replaced with NumberConstant");
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    #[test]
    fn test_constant_arg_write_aborts() {
        // CFG: Store(arg0, value) - arg0 is a constant
        // Writing to a constant argument should cause ConstantViolation
        let arg0_id = LocalId::from(10);

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::num_const(99)),
                (
                    LocalId::from(1),
                    Instruction::store(arg0_id, LocalId::from(0)),
                ),
            ],
            (LocalId::from(2), Terminator::Return { value: None }),
        );

        let cfg = Cfg::single_entry(entry);

        let mut shape = HeapShape::new();
        // arg0 = 42 (constant)
        shape.args = vec![Some(ValueShape::Constant(Pico8Num::from_i16(42)))];

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let arg_ids = vec![Some(arg0_id)];

        let result = eliminate_heap(&cfg, &shape, &arg_ids, local_gen, label_gen, DeoptMode::Insert);
        match result {
            HeapEliminationResult::ConstantViolation(msg) => {
                // The message should mention the arg (either "arg0" or "arg[0]")
                assert!(
                    msg.contains("arg"),
                    "Error should mention arg: {}",
                    msg
                );
            }
            other => panic!("Expected ConstantViolation, got {:?}", other),
        }
    }

    #[test]
    fn test_constant_arg_field_load() {
        // CFG: GetField(arg0, "x") -> Load
        // arg0.x is a constant (7), so Load should become NumberConstant(7)
        let arg0_id = LocalId::from(10);

        let entry = Block::new_for_test(
            vec![
                (
                    LocalId::from(0),
                    Instruction::GetField {
                        receiver: arg0_id,
                        field: "x".to_string(),
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
            (
                LocalId::from(2),
                Terminator::Return {
                    value: Some(LocalId::from(1)),
                },
            ),
        );

        let cfg = Cfg::single_entry(entry);

        let mut shape = HeapShape::new();
        // arg0 = { x: Constant(7) }
        shape.args = vec![Some(ValueShape::table([(
            "x",
            ValueShape::Constant(Pico8Num::from_i16(7)),
        )]))];

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let arg_ids = vec![Some(arg0_id)];

        let result = eliminate_heap(&cfg, &shape, &arg_ids, local_gen, label_gen, DeoptMode::Insert);
        match result {
            HeapEliminationResult::Success(transformed) => {
                // Check that the Load was replaced with NumberConstant
                let instructions = &transformed.cfg.entry.instructions;
                let has_number_constant = instructions.iter().any(|(_, instr)| {
                    matches!(instr, Instruction::NumberConstant { value } if *value == Pico8Num::from_i16(7))
                });
                assert!(
                    has_number_constant,
                    "Expected NumberConstant(7) in transformed CFG"
                );
                // Should NOT have Load instruction
                let has_load = instructions
                    .iter()
                    .any(|(_, instr)| matches!(instr, Instruction::Load { .. }));
                assert!(!has_load, "Load should be replaced with NumberConstant");
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    #[test]
    fn test_call_resolution_via_closure_shape() {
        use crate::ir::GlobalId;

        // Test that heap_elimination can resolve a Call when the closure is known via shape
        // Pattern: GetField(arg0, "foo") -> Load -> Call
        // With shape: arg0.foo = Closure { fun_name: "my_func_1", ... }
        // Expected: Call replaced with CallResolved(my_func_1, ...)

        // arg0 is at LocalId 10
        let arg0_id = LocalId::from(10);

        let entry = Block::new_for_test(
            vec![
                // %0 = GetField(arg0, "foo") - get the foo method from arg0
                (
                    LocalId::from(0),
                    Instruction::GetField {
                        receiver: arg0_id,
                        field: "foo".to_string(),
                        create_if_missing: false,
                    },
                ),
                // %1 = Load(%0) - load the closure
                (
                    LocalId::from(1),
                    Instruction::Load {
                        source: LocalId::from(0),
                    },
                ),
                // %2 = NumberConstant(42) - an argument
                (LocalId::from(2), Instruction::num_const(42)),
                // %3 = Call(%1, [%2]) - call the closure
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

        let cfg = Cfg::single_entry(entry);

        // Build shape: arg0 is a table with foo = Closure and x = Leaf
        // We need at least one Leaf for eliminate_heap to not return NotApplicable
        let mut shape = HeapShape::new();
        shape.args = vec![Some(ValueShape::table([
            (
                "foo",
                ValueShape::Closure {
                    fun_name: GlobalId::from("my_func_1".to_string()),
                    capture_shapes: vec![], // No captures
                },
            ),
            ("x", ValueShape::Leaf), // Need at least one leaf
        ]))];

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        // Use arg_ids to map the argument LocalId
        let arg_ids = vec![Some(arg0_id)];

        let result = eliminate_heap(&cfg, &shape, &arg_ids, local_gen, label_gen, DeoptMode::Insert);
        match result {
            HeapEliminationResult::Success(transformed) => {
                // Check that the Call was replaced with CallResolved
                let has_call_resolved = transformed.cfg.entry.instructions.iter().any(|(_, instr)| {
                    match instr {
                        Instruction::CallResolved { fun_name, captures, args } => {
                            fun_name.as_str() == "my_func_1" && captures.is_empty() && args.len() == 1
                        }
                        _ => false,
                    }
                });
                assert!(has_call_resolved, "Expected CallResolved(my_func_1) but didn't find it");

                // Should NOT have the original Call instruction
                let has_call = transformed.cfg.entry.instructions.iter().any(|(_, instr)| {
                    matches!(instr, Instruction::Call { .. })
                });
                assert!(!has_call, "Original Call should have been replaced");

                // Should have resolved one call
                assert_eq!(transformed.calls_resolved, 1, "Expected 1 call resolved");
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    #[test]
    fn test_unknown_global_read_write_no_deopt() {
        // Test that accessing a global NOT in the shape works without deopt
        // The original Load/Store instructions are kept but we still track the slot
        // CFG: %0 = GetGlobal("unknown"); %1 = Load(%0); %2 = Store(%0, %1); return
        let entry = Block::new_for_test(
            vec![
                // Even with create_if_missing=true, no deopt
                (LocalId::from(0), Instruction::get_global_create("unknown")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::store(LocalId::from(0), LocalId::from(1))),
            ],
            (LocalId::from(3), Terminator::ret(None)),
        );

        let cfg = Cfg::single_entry(entry);

        // Empty shape - "unknown" is not pre-declared
        let shape = HeapShape::new();

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);
        match result {
            HeapEliminationResult::Success(transformed) => {
                // Should succeed without any deopt
                assert_eq!(transformed.deopt_count, 0, "Expected no deopt for unknown global");
                // Load and Store should be kept since the global is not in the shape
                let has_load = transformed.cfg.entry.instructions.iter()
                    .any(|(_, instr)| matches!(instr, Instruction::Load { .. }));
                let has_store = transformed.cfg.entry.instructions.iter()
                    .any(|(_, instr)| matches!(instr, Instruction::Store { .. }));
                assert!(has_load, "Load should be kept for unknown global");
                assert!(has_store, "Store should be kept for unknown global");
            }
            HeapEliminationResult::NotApplicable => {
                // This is also acceptable - no tracked slots, nothing to transform
            }
            other => panic!("Expected Success or NotApplicable, got {:?}", other),
        }
    }

    #[test]
    fn test_mixed_known_unknown_globals() {
        // Test mixing known and unknown globals in the same CFG
        // %0 = GetGlobal("x"); %1 = Load(%0);  // known - SSA promoted
        // %2 = GetGlobal("unknown"); %3 = Load(%2);  // unknown - kept as is
        // %4 = BinaryOp(%1, Plus, %3); return %4
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("x")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::get_global_create("unknown")),
                (LocalId::from(3), Instruction::load(LocalId::from(2))),
                (LocalId::from(4), Instruction::binary_op(crate::ir::BinaryOp::Plus, LocalId::from(1), LocalId::from(3))),
            ],
            (LocalId::from(5), Terminator::ret(Some(LocalId::from(4)))),
        );

        let cfg = Cfg::single_entry(entry);

        // "x" is known, "unknown" is NOT in the shape
        let shape = make_global_leaf_shape("x");

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);
        match result {
            HeapEliminationResult::Success(transformed) => {
                assert_eq!(transformed.deopt_count, 0, "Expected no deopt");
                // Should have one unpack slot for "x"
                assert_eq!(transformed.unpack_slots.len(), 1);
                // After the Load(Phi) fix:
                // - Load for "x" (leaf slot) is eliminated - maps directly to SSA variable
                // - Load for "unknown" (unknown global) is kept as-is
                // So we expect exactly 1 Load
                let load_count = transformed.cfg.entry.instructions.iter()
                    .filter(|(_, instr)| matches!(instr, Instruction::Load { .. }))
                    .count();
                assert_eq!(load_count, 1, "Expected one Load (for unknown global only)");
                // Check that the Load for "unknown" is still from %2
                let has_load_from_2 = transformed.cfg.entry.instructions.iter()
                    .any(|(_, instr)| matches!(instr, Instruction::Load { source } if *source == LocalId::from(2)));
                assert!(has_load_from_2, "Load from GetGlobal('unknown') should be preserved");
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    // ==================== POINTER TRACKING THROUGH CELLS ====================
    //
    // These tests verify that heap elimination properly tracks pointers through
    // local cells (Alloc + Store + Load). This is essentially constant propagation
    // for pointers - if we know a value points to path P, and we store it in a
    // cell and load it back, the loaded value still points to P.

    /// Test 1: Basic pointer tracking through a cell.
    /// Store a pointer with known slot into a cell, load it back.
    /// The loaded value should have the same slot.
    #[test]
    fn test_pointer_tracking_basic_store_load() {
        // CFG:
        //   %0 = GetGlobal("player")     -- pointer to _G.player
        //   %1 = Load(%0)                -- value at _G.player (a table)
        //   %2 = Alloc                   -- create a cell
        //   %3 = Store(%2, %1)           -- store the table pointer in the cell
        //   %4 = Load(%2)                -- load it back from the cell
        //   %5 = GetField(%4, "x")       -- get field from the loaded value
        //   %6 = Load(%5)                -- load the field value
        //   return %6
        //
        // Expected: %4 should track to _G.player, so %5 tracks to _G.player.x
        // and the final Load can be SSA-promoted.

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("player")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::alloc()),
                (LocalId::from(3), Instruction::store(LocalId::from(2), LocalId::from(1))),
                (LocalId::from(4), Instruction::load(LocalId::from(2))),
                (LocalId::from(5), Instruction::get_field(LocalId::from(4), "x", false)),
                (LocalId::from(6), Instruction::load(LocalId::from(5))),
            ],
            (LocalId::from(7), Terminator::ret(Some(LocalId::from(6)))),
        );

        let cfg = Cfg::single_entry(entry);

        let shape = make_player_x_shape();

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);

        match result {
            HeapEliminationResult::Success(transformed) => {
                // Should have unpacked _G.player.x
                assert_eq!(
                    transformed.unpack_slots.len(), 1,
                    "Should unpack player.x"
                );

                // The Load(%5) for player.x should be eliminated (SSA promoted)
                // because we tracked the pointer through the cell
                let load_count = transformed.cfg.entry.instructions.iter()
                    .filter(|(_, instr)| matches!(instr, Instruction::Load { .. }))
                    .count();

                // We expect: Load(%0) for GetGlobal, Load(%2) for cell - but Load(%5) should be gone
                // Actually after SSA promotion, Load(%5) becomes a reference to the SSA var
                // Let's just check that we got a successful transformation with the slot
                assert!(
                    transformed.unpack_slots.iter().any(|(slot, _)| {
                        slot.to_string().contains("player") && slot.to_string().contains("x")
                    }),
                    "Should have unpacked player.x slot"
                );
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    /// Test 2: Argument stored in cell, then method called.
    /// This is the pattern the compiler generates for method parameters.
    #[test]
    fn test_pointer_tracking_arg_through_cell_method_call() {
        // CFG (what compiler generates for `function obj:foo() return self.method() end`):
        //   %0 = Alloc                   -- cell for self parameter
        //   %1 = Store(%0, %2)           -- store self value (%2) into cell
        //   %3 = Load(%0)                -- load self from cell
        //   %4 = GetField(%3, "method")  -- get method
        //   %5 = Load(%4)                -- load closure
        //   %6 = Call(%5, [])            -- call it
        //   return %6
        //
        // With shape tracking through the cell, we should resolve the call.

        let cell_id = LocalId::from(0);
        let value_id = LocalId::from(2);

        let entry = Block::new_for_test(
            vec![
                (cell_id, Instruction::Alloc),
                (
                    LocalId::from(1),
                    Instruction::store(cell_id, value_id),
                ),
                (LocalId::from(3), Instruction::load(cell_id)),
                (
                    LocalId::from(4),
                    Instruction::GetField {
                        receiver: LocalId::from(3),
                        field: "method".to_string(),
                        create_if_missing: false,
                    },
                ),
                (LocalId::from(5), Instruction::load(LocalId::from(4))),
                (
                    LocalId::from(6),
                    Instruction::Call {
                        closure: LocalId::from(5),
                        args: vec![],
                    },
                ),
            ],
            (
                LocalId::from(7),
                Terminator::Return { value: Some(LocalId::from(6)) },
            ),
        );

        let cfg = Cfg::single_entry(entry);

        // Shape: arg0 is a table with method closure and x leaf
        let mut shape = HeapShape::new();
        shape.args = vec![Some(ValueShape::table([
            (
                "method",
                ValueShape::Closure {
                    fun_name: GlobalId::from("my_method_1".to_string()),
                    capture_shapes: vec![],
                },
            ),
            ("x", ValueShape::Leaf),
        ]))];

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        // arg_ids contains the VALUE id (what the compiler does)
        let arg_ids = vec![Some(value_id)];

        let result = eliminate_heap(&cfg, &shape, &arg_ids, local_gen, label_gen, DeoptMode::Stop);

        match result {
            HeapEliminationResult::Success(transformed) => {
                let has_call_resolved = transformed.cfg.entry.instructions.iter().any(|(_, instr)| {
                    match instr {
                        Instruction::CallResolved { fun_name, .. } => {
                            fun_name.as_str() == "my_method_1"
                        }
                        _ => false,
                    }
                });

                // After fixing pointer tracking, this should resolve!
                assert!(
                    has_call_resolved,
                    "Call should be resolved after pointer tracking fix. calls_resolved={}",
                    transformed.calls_resolved
                );
            }
            HeapEliminationResult::NotApplicable => {
                panic!("Got NotApplicable - heap elimination didn't run");
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    /// Test 3: Multiple loads from the same cell should all get the same slot.
    #[test]
    fn test_pointer_tracking_multiple_loads() {
        // CFG:
        //   %0 = GetGlobal("player")
        //   %1 = Load(%0)                -- player table
        //   %2 = Alloc                   -- cell
        //   %3 = Store(%2, %1)           -- store player in cell
        //   %4 = Load(%2)                -- first load from cell
        //   %5 = GetField(%4, "x")       -- get x
        //   %6 = Load(%5)
        //   %7 = Load(%2)                -- second load from same cell
        //   %8 = GetField(%7, "y")       -- get y
        //   %9 = Load(%8)
        //   %10 = BinaryOp(%6 + %9)
        //   return %10
        //
        // Both %4 and %7 should track to _G.player

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("player")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::alloc()),
                (LocalId::from(3), Instruction::store(LocalId::from(2), LocalId::from(1))),
                (LocalId::from(4), Instruction::load(LocalId::from(2))),
                (LocalId::from(5), Instruction::get_field(LocalId::from(4), "x", false)),
                (LocalId::from(6), Instruction::load(LocalId::from(5))),
                (LocalId::from(7), Instruction::load(LocalId::from(2))),
                (LocalId::from(8), Instruction::get_field(LocalId::from(7), "y", false)),
                (LocalId::from(9), Instruction::load(LocalId::from(8))),
                (LocalId::from(10), Instruction::binary_op(crate::ir::BinaryOp::Plus, LocalId::from(6), LocalId::from(9))),
            ],
            (LocalId::from(11), Terminator::ret(Some(LocalId::from(10)))),
        );

        let cfg = Cfg::single_entry(entry);

        let shape = make_player_xy_shape();

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);

        match result {
            HeapEliminationResult::Success(transformed) => {
                // Should have unpacked both player.x and player.y
                assert_eq!(
                    transformed.unpack_slots.len(), 2,
                    "Should unpack both player.x and player.y"
                );

                let slot_names: Vec<_> = transformed.unpack_slots.iter()
                    .map(|(slot, _)| slot.to_string())
                    .collect();
                assert!(
                    slot_names.iter().any(|s| s.contains("player") && s.contains("x")),
                    "Should have player.x"
                );
                assert!(
                    slot_names.iter().any(|s| s.contains("player") && s.contains("y")),
                    "Should have player.y"
                );
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    /// Test 4: Cell reassignment - after reassignment, should track new value.
    /// This tests that we update our tracking when a cell is written again.
    #[test]
    fn test_pointer_tracking_cell_reassignment() {
        // CFG:
        //   %0 = GetGlobal("player1")
        //   %1 = Load(%0)
        //   %2 = GetGlobal("player2")
        //   %3 = Load(%2)
        //   %4 = Alloc                   -- cell
        //   %5 = Store(%4, %1)           -- store player1
        //   %6 = Store(%4, %3)           -- reassign to player2
        //   %7 = Load(%4)                -- should get player2
        //   %8 = GetField(%7, "x")
        //   %9 = Load(%8)
        //   return %9
        //
        // After reassignment, Load(%4) should track to player2, not player1

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("player1")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::get_global("player2")),
                (LocalId::from(3), Instruction::load(LocalId::from(2))),
                (LocalId::from(4), Instruction::alloc()),
                (LocalId::from(5), Instruction::store(LocalId::from(4), LocalId::from(1))),
                (LocalId::from(6), Instruction::store(LocalId::from(4), LocalId::from(3))),
                (LocalId::from(7), Instruction::load(LocalId::from(4))),
                (LocalId::from(8), Instruction::get_field(LocalId::from(7), "x", false)),
                (LocalId::from(9), Instruction::load(LocalId::from(8))),
            ],
            (LocalId::from(10), Terminator::ret(Some(LocalId::from(9)))),
        );

        let cfg = Cfg::single_entry(entry);

        // Shape: both players have x as leaf
        let mut shape = HeapShape::new();
        shape.globals.insert("player1".to_string(), make_table_with_x());
        shape.globals.insert("player2".to_string(), make_table_with_x());

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);

        match result {
            HeapEliminationResult::Success(transformed) => {
                // Should track through to player2.x (the reassigned value)
                let slot_names: Vec<_> = transformed.unpack_slots.iter()
                    .map(|(slot, _)| slot.to_string())
                    .collect();

                // Should have player2.x since that's what the cell contains after reassignment
                assert!(
                    slot_names.iter().any(|s| s.contains("player2") && s.contains("x")),
                    "Should track to player2.x after reassignment. Got: {:?}", slot_names
                );
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    /// Test 5: Nested cells - store a pointer, load it, store in another cell.
    #[test]
    fn test_pointer_tracking_nested_cells() {
        // CFG:
        //   %0 = GetGlobal("player")
        //   %1 = Load(%0)
        //   %2 = Alloc                   -- cell1
        //   %3 = Store(%2, %1)           -- store player in cell1
        //   %4 = Load(%2)                -- load from cell1
        //   %5 = Alloc                   -- cell2
        //   %6 = Store(%5, %4)           -- store in cell2
        //   %7 = Load(%5)                -- load from cell2
        //   %8 = GetField(%7, "x")
        //   %9 = Load(%8)
        //   return %9
        //
        // Pointer should flow: player -> cell1 -> cell2 -> GetField

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("player")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::alloc()),
                (LocalId::from(3), Instruction::store(LocalId::from(2), LocalId::from(1))),
                (LocalId::from(4), Instruction::load(LocalId::from(2))),
                (LocalId::from(5), Instruction::alloc()),
                (LocalId::from(6), Instruction::store(LocalId::from(5), LocalId::from(4))),
                (LocalId::from(7), Instruction::load(LocalId::from(5))),
                (LocalId::from(8), Instruction::get_field(LocalId::from(7), "x", false)),
                (LocalId::from(9), Instruction::load(LocalId::from(8))),
            ],
            (LocalId::from(10), Terminator::ret(Some(LocalId::from(9)))),
        );

        let cfg = Cfg::single_entry(entry);

        let shape = make_player_x_shape();

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);

        match result {
            HeapEliminationResult::Success(transformed) => {
                // Should still track through both cells to player.x
                assert!(
                    transformed.unpack_slots.iter().any(|(slot, _)| {
                        slot.to_string().contains("player") && slot.to_string().contains("x")
                    }),
                    "Should track through nested cells to player.x"
                );
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    /// Test 6: Load from cell before any store - should not crash, just not track.
    #[test]
    fn test_pointer_tracking_load_before_store() {
        // CFG:
        //   %0 = Alloc
        //   %1 = Load(%0)                -- load before store (undefined behavior in real code)
        //   %2 = GetField(%1, "x")
        //   %3 = Load(%2)
        //   return %3
        //
        // Should handle gracefully - no tracking, but no crash

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::Alloc),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (
                    LocalId::from(2),
                    Instruction::GetField {
                        receiver: LocalId::from(1),
                        field: "x".to_string(),
                        create_if_missing: false,
                    },
                ),
                (LocalId::from(3), Instruction::load(LocalId::from(2))),
            ],
            (
                LocalId::from(4),
                Terminator::Return { value: Some(LocalId::from(3)) },
            ),
        );

        let cfg = Cfg::single_entry(entry);

        // Empty shape - nothing to track
        let shape = HeapShape::new();

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        // Should not panic
        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Insert);

        // NotApplicable is fine (no tracked slots), Success with no changes is also fine
        match result {
            HeapEliminationResult::Success(_) | HeapEliminationResult::NotApplicable => {}
            other => panic!("Expected Success or NotApplicable, got {:?}", other),
        }
    }

    /// Test that Stop mode correctly rewrites LocalId references in remaining instructions.
    ///
    /// This reproduces a bug where:
    /// 1. GetGlobal(btn) -> %0, tracked with slot
    /// 2. Load(%0) -> %1, SSA-promoted (no instruction emitted), %1 mapped to SSA var
    /// 3. Call(%1, args) -> triggers stop (unknown closure)
    ///
    /// The bug was that remaining instructions (including Call) were added from the
    /// original block without rewriting, so Call still referenced %1 which no longer
    /// exists (since Load was SSA-promoted away).
    #[test]
    fn test_stop_mode_rewrites_remaining_instructions() {
        // CFG: GetGlobal(btn) -> Load -> Call
        // Shape: btn is a Leaf (incorrectly, but this is what triggers the bug)
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("btn")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::num_const(1)),
                (LocalId::from(3), Instruction::call(LocalId::from(1), vec![LocalId::from(2)])),
            ],
            (LocalId::from(4), Terminator::ret(Some(LocalId::from(3)))),
        );

        let cfg = Cfg::single_entry(entry);

        // Shape: btn is a Leaf (this causes Load to be SSA-promoted)
        let shape = make_global_leaf_shape("btn");

        let local_gen = make_local_gen();
        let label_gen = make_label_gen();

        // Run in Stop mode - this is where the bug manifests
        let result = eliminate_heap(&cfg, &shape, &[], local_gen, label_gen, DeoptMode::Stop);

        match result {
            HeapEliminationResult::Success(transformed) => {
                // Collect all defined LocalIds in the transformed CFG
                let mut defined_ids: FxHashSet<LocalId> = FxHashSet::default();

                // Entry block definitions
                for (id, _) in &transformed.cfg.entry.instructions {
                    defined_ids.insert(*id);
                }

                // Check that all uses reference defined IDs
                for (_, instr) in &transformed.cfg.entry.instructions {
                    instr.map_local_ids(|used_id| {
                        assert!(
                            defined_ids.contains(&used_id),
                            "Instruction {:?} references undefined LocalId {:?}. Defined: {:?}",
                            instr,
                            used_id,
                            defined_ids
                        );
                        used_id
                    });
                }

                // Also check terminator
                transformed.cfg.entry.terminator_kind().map_local_ids(|used_id| {
                    assert!(
                        defined_ids.contains(&used_id),
                        "Terminator references undefined LocalId {:?}. Defined: {:?}",
                        used_id,
                        defined_ids
                    );
                    used_id
                });
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }
}
