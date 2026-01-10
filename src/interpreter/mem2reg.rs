//! mem2reg pass: Promote local cell allocations to SSA form.
//!
//! This pass eliminates Alloc/Store/Load patterns for non-escaping allocations,
//! converting them to pure SSA values with phi nodes at control flow joins.
//!
//! A "local cell" is an allocation that:
//! - Is created by Alloc
//! - Is only accessed via Store(cell, value) and Load(cell)
//! - Never escapes (not stored elsewhere, not passed to calls, not returned, not captured)
//!
//! This is the classic "mem2reg" or "alloca promotion" optimization from LLVM.

use std::collections::HashSet;

use crate::common::FxHashMap;
use crate::ir::{Block, BlockId, Cfg, Instruction, Label, LocalId, LocalIdGenerator, Terminator};

/// Result of the mem2reg pass
#[derive(Debug)]
pub enum Mem2RegResult {
    /// Successfully promoted cells to SSA
    Success {
        cfg: Cfg,
        /// Number of cells promoted
        cells_promoted: usize,
    },
    /// No cells to promote
    NoCells,
    /// Some cells couldn't be promoted (reason)
    PartialSuccess {
        cfg: Cfg,
        cells_promoted: usize,
        cells_failed: usize,
        reason: String,
    },
}

/// Information about a local cell
#[derive(Debug)]
struct CellInfo {
    /// The LocalId of the Alloc instruction
    alloc_id: LocalId,
    /// All Store instructions to this cell: (block_id, instruction_index, source_value)
    stores: Vec<(BlockId, usize, LocalId)>,
    /// All Load instructions from this cell: (block_id, instruction_index, target_id)
    loads: Vec<(BlockId, usize, LocalId)>,
}

/// Run the mem2reg pass on a CFG.
///
/// This identifies non-escaping allocations and promotes them to SSA form.
pub fn mem2reg(cfg: &Cfg, local_gen: &mut LocalIdGenerator) -> Mem2RegResult {
    let mut new_cfg = cfg.clone();
    let mut total_promoted = 0;

    // Track cells that failed promotion - we won't try them again
    let mut failed_cells: HashSet<LocalId> = HashSet::new();

    // Iteratively promote cells. After each promotion, the CFG changes and
    // CellInfo needs to be rebuilt because:
    // 1. Promoted cells' Alloc/Store/Load instructions are removed
    // 2. Load targets are rewritten, changing what other cells' stores reference
    loop {
        // Find all allocations and check which ones are local cells
        let cells = find_local_cells(&new_cfg);

        if cells.is_empty() {
            break;
        }

        // Identify cells that can be safely promoted
        // A cell cannot be promoted if its Store source is another cell's alloc_id
        let all_alloc_ids: HashSet<LocalId> = cells.iter().map(|c| c.alloc_id).collect();

        // Also track all Load targets from cells - these get removed during promotion
        // and any cell whose Store source is a Load target depends on that cell
        let mut load_targets: HashSet<LocalId> = HashSet::new();
        for cell in &cells {
            for (_, _, load_target) in &cell.loads {
                load_targets.insert(*load_target);
            }
        }

        let safe_cells: Vec<&CellInfo> = cells.iter()
            .filter(|cell| {
                // Skip cells that have already failed
                if failed_cells.contains(&cell.alloc_id) {
                    return false;
                }
                // Check if any store's source is another cell's alloc_id
                for (_, _, source) in &cell.stores {
                    if all_alloc_ids.contains(source) && *source != cell.alloc_id {
                        // This cell's store source is another cell - not safe
                        return false;
                    }
                    // Also check if store source is a Load target from another cell
                    // (that Load will be removed when the other cell is promoted)
                    if load_targets.contains(source) {
                        // Check if it's from another cell, not our own
                        let is_own_load = cell.loads.iter().any(|(_, _, lt)| *lt == *source);
                        if !is_own_load {
                            return false;
                        }
                    }
                }
                true
            })
            .collect();

        if safe_cells.is_empty() {
            break;
        }

        // Promote just ONE cell at a time to avoid stale CellInfo issues
        let cell = safe_cells[0];
        if promote_cell(&mut new_cfg, cell, local_gen) {
            total_promoted += 1;
        } else {
            // If promotion failed, mark this cell and try others
            failed_cells.insert(cell.alloc_id);
            // Continue to try other cells
        }
    }

    if total_promoted == 0 {
        return Mem2RegResult::NoCells;
    }

    Mem2RegResult::Success {
        cfg: new_cfg,
        cells_promoted: total_promoted,
    }
}

/// Find all local cells (non-escaping allocations)
fn find_local_cells(cfg: &Cfg) -> Vec<CellInfo> {
    let mut allocations: FxHashMap<LocalId, CellInfo> = FxHashMap::default();
    let mut escaping: HashSet<LocalId> = HashSet::new();

    // First pass: find all Allocs
    for (_, block) in cfg.iter_blocks_with_id() {
        for (target_id, instruction) in &block.instructions {
            if matches!(instruction, Instruction::Alloc) {
                allocations.insert(*target_id, CellInfo {
                    alloc_id: *target_id,
                    stores: Vec::new(),
                    loads: Vec::new(),
                });
            }
        }
    }

    // Second pass: find uses and mark escaping cells
    for (block_id, block) in cfg.iter_blocks_with_id() {
        for (idx, (target_id, instruction)) in block.instructions.iter().enumerate() {
            match instruction {
                Instruction::Alloc => {
                    // Already handled in first pass
                }
                Instruction::Store { target, source } => {
                    // Store TO a cell is fine - track it
                    if let Some(cell) = allocations.get_mut(target) {
                        cell.stores.push((block_id.clone(), idx, *source));
                    }
                    // Store OF a cell (as value) means it escapes
                    // (unless storing to itself, which is a no-op)
                    if allocations.contains_key(source) && target != source {
                        escaping.insert(*source);
                    }
                }
                Instruction::Load { source } => {
                    // Load FROM a cell is fine - track it
                    if let Some(cell) = allocations.get_mut(source) {
                        cell.loads.push((block_id.clone(), idx, *target_id));
                    }
                }
                // For all other instructions, check if any operand is a cell
                // If so, the cell escapes (used for something other than Store/Load)
                other => {
                    // Use map_local_ids to find all LocalId uses in this instruction
                    other.map_local_ids(|id| {
                        if allocations.contains_key(&id) {
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
                if allocations.contains_key(ret_id) {
                    escaping.insert(*ret_id);
                }
            }
            Terminator::ConditionalBranch { condition, .. } => {
                if allocations.contains_key(condition) {
                    escaping.insert(*condition);
                }
            }
            _ => {}
        }
    }

    // Filter out escaping allocations
    allocations.into_iter()
        .filter(|(id, _)| !escaping.contains(id))
        .map(|(_, info)| info)
        .collect()
}

/// Promote a single cell to SSA form
fn promote_cell(cfg: &mut Cfg, cell: &CellInfo, local_gen: &mut LocalIdGenerator) -> bool {
    // For single-block CFGs, we can do a simple forward pass
    if cfg.named.is_empty() {
        return promote_cell_single_block(cfg, cell);
    }

    // For multi-block CFGs, we need proper SSA construction with phi nodes
    promote_cell_multi_block(cfg, cell, local_gen)
}

/// Simple promotion for single-block CFGs
fn promote_cell_single_block(cfg: &mut Cfg, cell: &CellInfo) -> bool {
    let block = &mut cfg.entry;

    // Map from Load target to the value it should use
    let mut load_replacements: FxHashMap<LocalId, LocalId> = FxHashMap::default();

    // Build load replacements by tracking the current value
    let mut current_value: Option<LocalId> = None;
    for (target_id, instruction) in &block.instructions {
        match instruction {
            Instruction::Store { target, source } if *target == cell.alloc_id => {
                current_value = Some(*source);
            }
            Instruction::Load { source } if *source == cell.alloc_id => {
                if let Some(val) = current_value {
                    load_replacements.insert(*target_id, val);
                } else {
                    // Load before any store - this shouldn't happen for valid code
                    // but if it does, we can't promote
                    return false;
                }
            }
            _ => {}
        }
    }

    // Third pass: rewrite instructions to use the replacements
    let new_instructions: Vec<_> = block.instructions.iter()
        .filter_map(|(target_id, instruction)| {
            // Skip Alloc for this cell
            if matches!(instruction, Instruction::Alloc) && *target_id == cell.alloc_id {
                return None;
            }
            // Skip Store to this cell
            if let Instruction::Store { target, .. } = instruction {
                if *target == cell.alloc_id {
                    return None;
                }
            }
            // Skip Load from this cell (it's replaced by direct use)
            if let Instruction::Load { source } = instruction {
                if *source == cell.alloc_id {
                    return None;
                }
            }

            // Rewrite uses of Load targets to use the source value directly
            let rewritten = instruction.map_local_ids(|id| {
                load_replacements.get(&id).copied().unwrap_or(id)
            });

            Some((*target_id, rewritten))
        })
        .collect();

    // Rewrite terminator
    let (term_id, term) = &block.terminator;
    let new_term = match term {
        Terminator::Return { value } => {
            Terminator::Return {
                value: value.map(|v| load_replacements.get(&v).copied().unwrap_or(v)),
            }
        }
        Terminator::ConditionalBranch { condition, true_target, false_target } => {
            Terminator::ConditionalBranch {
                condition: load_replacements.get(condition).copied().unwrap_or(*condition),
                true_target: true_target.clone(),
                false_target: false_target.clone(),
            }
        }
        other => other.clone(),
    };

    block.instructions = new_instructions;
    block.terminator = (*term_id, new_term);

    true
}

/// Value at a program point for SSA construction
#[derive(Clone, Debug, PartialEq, Eq)]
enum ReachingValue {
    /// No value yet (before any store)
    Undefined,
    /// A single known value from a single predecessor
    SingleFrom(Option<Label>, LocalId),
    /// A single value that reaches from all processed predecessors (same value everywhere)
    Single(LocalId),
    /// A phi node is needed - contains (predecessor_label, value) pairs
    /// Note: Entry block has no label, so we use None
    NeedsPhi(Vec<(Option<Label>, LocalId)>),
}

/// Promote a cell in a multi-block CFG using SSA construction
fn promote_cell_multi_block(cfg: &mut Cfg, cell: &CellInfo, local_gen: &mut LocalIdGenerator) -> bool {
    // Compute predecessors
    let predecessors = cfg.compute_predecessors();

    // Check if this is the simple "store once, read many" pattern
    // This is the most common case and doesn't need phi nodes
    let entry_stores: Vec<_> = cell.stores.iter()
        .filter(|(bid, _, _)| *bid == BlockId::Entry)
        .collect();

    let other_stores: Vec<_> = cell.stores.iter()
        .filter(|(bid, _, _)| *bid != BlockId::Entry)
        .collect();

    // Case 1: Single store in entry block, no other stores
    if entry_stores.len() == 1 && other_stores.is_empty() {
        let (_, _, source_value) = entry_stores[0];

        let mut load_replacements: FxHashMap<LocalId, LocalId> = FxHashMap::default();
        for (_, _, load_target) in &cell.loads {
            load_replacements.insert(*load_target, *source_value);
        }

        // Verify we have replacements for ALL loads
        if load_replacements.len() != cell.loads.len() {
            return false;
        }

        for block in cfg.iter_blocks_mut() {
            rewrite_block_for_cell(block, cell, &load_replacements);
        }

        return true;
    }

    // Case 2: Single store in a non-entry block, all loads are dominated by it
    // This is common for "local x = ..." inside an if branch
    if other_stores.len() == 1 && entry_stores.is_empty() {
        let (store_block_id, store_idx, source_value) = &other_stores[0];

        // Check if all loads are either:
        // 1. In the same block as the store, after the store
        // 2. In blocks that are dominated by the store block
        // For simplicity, just check if all loads are in the same block and after the store

        let all_loads_in_same_block_after_store = cell.loads.iter().all(|(load_block_id, load_idx, _)| {
            load_block_id == store_block_id && load_idx > store_idx
        });

        if all_loads_in_same_block_after_store {
            let mut load_replacements: FxHashMap<LocalId, LocalId> = FxHashMap::default();
            for (_, _, load_target) in &cell.loads {
                load_replacements.insert(*load_target, *source_value);
            }

            // Verify we have replacements for ALL loads
            if load_replacements.len() != cell.loads.len() {
                return false;
            }

            for block in cfg.iter_blocks_mut() {
                rewrite_block_for_cell(block, cell, &load_replacements);
            }

            return true;
        }

        // Try a more general approach: check if all loads come after the store in any path
        // For now, check if all loads are in blocks that are reachable only through the store block

        // Get the store block's successors (transitively)
        let mut reachable_from_store: HashSet<BlockId> = HashSet::new();
        let mut worklist = vec![store_block_id.clone()];

        while let Some(bid) = worklist.pop() {
            if reachable_from_store.contains(&bid) {
                continue;
            }
            reachable_from_store.insert(bid.clone());

            // Get block and add successors
            let block = match &bid {
                BlockId::Entry => &cfg.entry,
                BlockId::Named(label) => {
                    if let Some(b) = cfg.named.get(label) {
                        b
                    } else {
                        continue;
                    }
                }
            };

            match block.terminator_kind() {
                Terminator::UnconditionalBranch { target } => {
                    worklist.push(BlockId::Named(target.clone()));
                }
                Terminator::ConditionalBranch { true_target, false_target, .. } => {
                    worklist.push(BlockId::Named(true_target.clone()));
                    worklist.push(BlockId::Named(false_target.clone()));
                }
                Terminator::Return { .. } | Terminator::Deopt { .. } => {}
            }
        }

        // Check if all loads are in blocks reachable from the store block
        let all_loads_reachable = cell.loads.iter().all(|(load_block_id, load_idx, _)| {
            if load_block_id == store_block_id {
                // Same block - must be after the store
                load_idx > store_idx
            } else {
                // Different block - must be reachable from store block
                reachable_from_store.contains(load_block_id)
            }
        });

        if all_loads_reachable {
            let mut load_replacements: FxHashMap<LocalId, LocalId> = FxHashMap::default();
            for (_, _, load_target) in &cell.loads {
                load_replacements.insert(*load_target, *source_value);
            }

            // Verify we have replacements for ALL loads
            if load_replacements.len() != cell.loads.len() {
                return false;
            }

            for block in cfg.iter_blocks_mut() {
                rewrite_block_for_cell(block, cell, &load_replacements);
            }

            return true;
        }
    }

    // General case: Full SSA construction with phi nodes
    promote_cell_with_ssa(cfg, cell, local_gen, &predecessors)
}

/// Full SSA construction for a cell with multiple stores
fn promote_cell_with_ssa(
    cfg: &mut Cfg,
    cell: &CellInfo,
    local_gen: &mut LocalIdGenerator,
    predecessors: &FxHashMap<BlockId, Vec<BlockId>>,
) -> bool {
    // Step 1: Group stores and loads by block
    let mut stores_by_block: FxHashMap<BlockId, Vec<(usize, LocalId)>> = FxHashMap::default();
    for (block_id, idx, source) in &cell.stores {
        stores_by_block.entry(block_id.clone()).or_default().push((*idx, *source));
    }
    // Sort stores within each block by index
    for stores in stores_by_block.values_mut() {
        stores.sort_by_key(|(idx, _)| *idx);
    }

    let mut loads_by_block: FxHashMap<BlockId, Vec<(usize, LocalId)>> = FxHashMap::default();
    for (block_id, idx, target) in &cell.loads {
        loads_by_block.entry(block_id.clone()).or_default().push((*idx, *target));
    }
    // Sort loads within each block by index
    for loads in loads_by_block.values_mut() {
        loads.sort_by_key(|(idx, _)| *idx);
    }

    // Step 2: Compute block exit values using forward dataflow
    // For each block, what value does the cell hold when leaving the block?
    let mut block_exit_values: FxHashMap<BlockId, ReachingValue> = FxHashMap::default();

    // Collect all block IDs
    let mut all_block_ids: Vec<BlockId> = vec![BlockId::Entry];
    for label in cfg.named.keys() {
        all_block_ids.push(BlockId::Named(label.clone()));
    }

    // Compute topological order (approximate - just ensure entry comes first)
    // For proper ordering, we'd need dominance, but this simple approach works
    // for acyclic CFGs and converges for cyclic ones

    // Initialize: entry block has no incoming value
    let entry_exit = compute_block_exit_value(
        &BlockId::Entry,
        &ReachingValue::Undefined,
        &stores_by_block,
        &cfg.entry,
    );
    block_exit_values.insert(BlockId::Entry.clone(), entry_exit);

    // Iterate until fixpoint (handles loops in CFG)
    let mut changed = true;
    let mut iterations = 0;
    const MAX_ITERATIONS: usize = 100;

    while changed && iterations < MAX_ITERATIONS {
        changed = false;
        iterations += 1;

        for block_id in &all_block_ids {
            if *block_id == BlockId::Entry {
                continue; // Already handled
            }

            // Get block's predecessors
            let preds = predecessors.get(block_id).cloned().unwrap_or_default();
            if preds.is_empty() {
                continue; // Unreachable block
            }

            // Compute entry value by merging predecessor exit values
            let entry_value = compute_entry_value(&preds, &block_exit_values);

            // Get the block
            let block = match block_id {
                BlockId::Named(label) => cfg.named.get(label).unwrap(),
                BlockId::Entry => unreachable!(),
            };

            // Compute exit value
            let exit_value = compute_block_exit_value(block_id, &entry_value, &stores_by_block, block);

            // Check if changed
            let old_exit = block_exit_values.get(block_id);
            if old_exit != Some(&exit_value) {
                block_exit_values.insert(block_id.clone(), exit_value);
                changed = true;
            }
        }
    }

    if iterations >= MAX_ITERATIONS {
        // Failed to converge - bail out
        return false;
    }

    // Step 3: Compute entry values and load replacements
    let mut block_entry_values: FxHashMap<BlockId, ReachingValue> = FxHashMap::default();
    block_entry_values.insert(BlockId::Entry, ReachingValue::Undefined);

    for block_id in &all_block_ids {
        if *block_id == BlockId::Entry {
            continue;
        }
        let preds = predecessors.get(block_id).cloned().unwrap_or_default();
        let entry_value = compute_entry_value(&preds, &block_exit_values);
        block_entry_values.insert(block_id.clone(), entry_value);
    }

    // Step 4: For each load, determine what value it should use
    let mut load_replacements: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    // Track phi nodes to insert: block_id -> (phi_result_id, branches)
    let mut phi_nodes: FxHashMap<BlockId, (LocalId, Vec<(Label, LocalId)>)> = FxHashMap::default();

    for (block_id, loads) in &loads_by_block {
        let entry_value = block_entry_values.get(block_id).cloned().unwrap_or(ReachingValue::Undefined);
        let stores = stores_by_block.get(block_id).cloned().unwrap_or_default();

        // For each load, find the reaching value at that point
        for (load_idx, load_target) in loads {
            let value_at_load = compute_value_at_point(&entry_value, &stores, *load_idx);
            let normalized = normalize_reaching_value(&value_at_load);

            match normalized {
                ReachingValue::Single(v) | ReachingValue::SingleFrom(_, v) => {
                    load_replacements.insert(*load_target, v);
                }
                ReachingValue::NeedsPhi(_) => {
                    // Check if we already have a phi node for this block
                    if let Some((phi_id, _)) = phi_nodes.get(block_id) {
                        load_replacements.insert(*load_target, *phi_id);
                    } else {
                        // Create a new phi node - branches must reference immediate predecessors
                        let preds = predecessors.get(block_id).cloned().unwrap_or_default();

                        // For each immediate predecessor, find the exit value
                        let mut phi_branches: Vec<(Label, LocalId)> = Vec::new();
                        for pred in &preds {
                            let pred_exit = block_exit_values.get(pred).cloned().unwrap_or(ReachingValue::Undefined);
                            let pred_label = match pred {
                                BlockId::Entry => {
                                    // Entry block as predecessor - use the special entry label
                                    Label::entry()
                                }
                                BlockId::Named(l) => l.clone(),
                            };

                            match normalize_reaching_value(&pred_exit) {
                                ReachingValue::Single(v) | ReachingValue::SingleFrom(_, v) => {
                                    phi_branches.push((pred_label, v));
                                }
                                ReachingValue::Undefined => {
                                    // Predecessor has undefined value - can't promote
                                    return false;
                                }
                                ReachingValue::NeedsPhi(_) => {
                                    // Predecessor's exit value requires phi - shouldn't happen
                                    // since exit values are always Single after a store
                                    return false;
                                }
                            }
                        }

                        if phi_branches.is_empty() {
                            return false;
                        }

                        let phi_id = local_gen.fresh_id();
                        phi_nodes.insert(block_id.clone(), (phi_id, phi_branches));
                        load_replacements.insert(*load_target, phi_id);
                    }
                }
                ReachingValue::Undefined => {
                    // Load before any store - can't promote
                    return false;
                }
            }
        }
    }

    // Step 5: Rewrite the CFG
    // First, insert phi nodes at the beginning of blocks that need them
    for (block_id, (phi_id, branches)) in &phi_nodes {
        let block = match block_id {
            BlockId::Entry => &mut cfg.entry,
            BlockId::Named(label) => cfg.named.get_mut(label).unwrap(),
        };

        // Insert phi at the beginning
        let phi_instr = Instruction::Phi { branches: branches.clone() };
        block.instructions.insert(0, (*phi_id, phi_instr));
    }

    // Then rewrite blocks to remove cell operations and use replacements
    for block in cfg.iter_blocks_mut() {
        rewrite_block_for_cell(block, cell, &load_replacements);
    }

    true
}

/// Compute the entry value for a block by merging predecessor exit values
fn compute_entry_value(
    predecessors: &[BlockId],
    block_exit_values: &FxHashMap<BlockId, ReachingValue>,
) -> ReachingValue {
    let mut result = ReachingValue::Undefined;

    for pred in predecessors {
        let pred_exit = block_exit_values.get(pred).cloned().unwrap_or(ReachingValue::Undefined);
        let pred_label = match pred {
            BlockId::Entry => None,
            BlockId::Named(l) => Some(l.clone()),
        };

        // Merge with special handling for the predecessor label
        result = merge_with_pred(&result, &pred_exit, pred_label);
    }

    result
}

/// Merge reaching values, tracking which predecessor each value comes from.
/// This is used to build up the entry value for a block by iterating over predecessors.
/// `current` is the accumulated result so far, `incoming` is from the current predecessor.
fn merge_with_pred(current: &ReachingValue, incoming: &ReachingValue, pred_label: Option<Label>) -> ReachingValue {
    // First, normalize incoming to have the predecessor label
    let incoming_with_label = match incoming {
        ReachingValue::Undefined => ReachingValue::Undefined,
        ReachingValue::Single(v) => ReachingValue::SingleFrom(pred_label.clone(), *v),
        ReachingValue::SingleFrom(_, v) => ReachingValue::SingleFrom(pred_label.clone(), *v),
        ReachingValue::NeedsPhi(branches) => ReachingValue::NeedsPhi(branches.clone()),
    };

    match (current, &incoming_with_label) {
        // Both undefined
        (ReachingValue::Undefined, ReachingValue::Undefined) => ReachingValue::Undefined,

        // One undefined, one has value
        (ReachingValue::Undefined, other) => other.clone(),
        (other, ReachingValue::Undefined) => other.clone(),

        // Same value from different predecessors
        (ReachingValue::SingleFrom(_, v1), ReachingValue::SingleFrom(_, v2)) if v1 == v2 => {
            ReachingValue::Single(*v1)
        }
        (ReachingValue::Single(v1), ReachingValue::SingleFrom(_, v2)) if v1 == v2 => {
            ReachingValue::Single(*v1)
        }
        (ReachingValue::SingleFrom(_, v1), ReachingValue::Single(v2)) if v1 == v2 => {
            ReachingValue::Single(*v1)
        }
        (ReachingValue::Single(v1), ReachingValue::Single(v2)) if v1 == v2 => {
            ReachingValue::Single(*v1)
        }

        // Different values - need phi
        (ReachingValue::SingleFrom(l1, v1), ReachingValue::SingleFrom(l2, v2)) => {
            ReachingValue::NeedsPhi(vec![(l1.clone(), *v1), (l2.clone(), *v2)])
        }
        (ReachingValue::Single(v1), ReachingValue::SingleFrom(l2, v2)) => {
            // Single means we've seen same value from multiple preds, but now different
            ReachingValue::NeedsPhi(vec![(None, *v1), (l2.clone(), *v2)])
        }
        (ReachingValue::SingleFrom(l1, v1), ReachingValue::Single(v2)) => {
            ReachingValue::NeedsPhi(vec![(l1.clone(), *v1), (None, *v2)])
        }
        (ReachingValue::Single(v1), ReachingValue::Single(v2)) => {
            ReachingValue::NeedsPhi(vec![(None, *v1), (None, *v2)])
        }

        // Merging with existing phi
        (ReachingValue::NeedsPhi(branches), ReachingValue::SingleFrom(l, v)) => {
            let mut new_branches = branches.clone();
            if !new_branches.iter().any(|(ll, _)| *ll == *l) {
                new_branches.push((l.clone(), *v));
            }
            ReachingValue::NeedsPhi(new_branches)
        }
        (ReachingValue::NeedsPhi(branches), ReachingValue::Single(v)) => {
            let mut new_branches = branches.clone();
            new_branches.push((pred_label, *v));
            ReachingValue::NeedsPhi(new_branches)
        }
        (ReachingValue::SingleFrom(l1, v1), ReachingValue::NeedsPhi(branches)) => {
            let mut new_branches = vec![(l1.clone(), *v1)];
            for (l, v) in branches {
                if !new_branches.iter().any(|(ll, _)| ll == l) {
                    new_branches.push((l.clone(), *v));
                }
            }
            ReachingValue::NeedsPhi(new_branches)
        }
        (ReachingValue::Single(v1), ReachingValue::NeedsPhi(branches)) => {
            let mut new_branches = vec![(None, *v1)];
            for (l, v) in branches {
                if !new_branches.iter().any(|(ll, _)| ll == l) {
                    new_branches.push((l.clone(), *v));
                }
            }
            ReachingValue::NeedsPhi(new_branches)
        }
        (ReachingValue::NeedsPhi(b1), ReachingValue::NeedsPhi(b2)) => {
            let mut new_branches = b1.clone();
            for (l, v) in b2 {
                if !new_branches.iter().any(|(ll, _)| ll == l) {
                    new_branches.push((l.clone(), *v));
                }
            }
            ReachingValue::NeedsPhi(new_branches)
        }
    }
}

/// Compute the exit value for a block given its entry value and stores
fn compute_block_exit_value(
    _block_id: &BlockId,
    entry_value: &ReachingValue,
    stores_by_block: &FxHashMap<BlockId, Vec<(usize, LocalId)>>,
    _block: &Block,
) -> ReachingValue {
    let stores = stores_by_block.get(_block_id).cloned().unwrap_or_default();

    if stores.is_empty() {
        // No stores in this block - exit value equals entry value
        return entry_value.clone();
    }

    // The exit value is the value from the last store
    let (_, last_store_value) = stores.last().unwrap();
    ReachingValue::Single(*last_store_value)
}

/// Compute the value at a specific point (instruction index) within a block
fn compute_value_at_point(
    entry_value: &ReachingValue,
    stores: &[(usize, LocalId)],
    point_idx: usize,
) -> ReachingValue {
    // Find the most recent store before this point
    let mut current_value = entry_value.clone();

    for (store_idx, store_value) in stores {
        if *store_idx < point_idx {
            // After a store, we have a definite single value
            current_value = ReachingValue::Single(*store_value);
        } else {
            break; // Stores are sorted, so we can stop
        }
    }

    current_value
}

/// Normalize a ReachingValue for use in load replacement
/// SingleFrom is treated the same as Single for this purpose
fn normalize_reaching_value(value: &ReachingValue) -> ReachingValue {
    match value {
        ReachingValue::SingleFrom(_, v) => ReachingValue::Single(*v),
        other => other.clone(),
    }
}

/// Rewrite a block to remove cell operations and use replacements
fn rewrite_block_for_cell(
    block: &mut Block,
    cell: &CellInfo,
    load_replacements: &FxHashMap<LocalId, LocalId>,
) {
    let new_instructions: Vec<_> = block.instructions.iter()
        .filter_map(|(target_id, instruction)| {
            // Skip Alloc for this cell
            if matches!(instruction, Instruction::Alloc) && *target_id == cell.alloc_id {
                return None;
            }
            // Skip Store to this cell
            if let Instruction::Store { target, .. } = instruction {
                if *target == cell.alloc_id {
                    return None;
                }
            }
            // Skip Load from this cell ONLY if we have a replacement for it
            if let Instruction::Load { source } = instruction {
                if *source == cell.alloc_id && load_replacements.contains_key(target_id) {
                    return None;
                }
            }

            // Rewrite uses
            let rewritten = instruction.map_local_ids(|id| {
                load_replacements.get(&id).copied().unwrap_or(id)
            });

            Some((*target_id, rewritten))
        })
        .collect();

    // Rewrite terminator
    let (term_id, term) = &block.terminator;
    let new_term = match term {
        Terminator::Return { value } => {
            Terminator::Return {
                value: value.map(|v| load_replacements.get(&v).copied().unwrap_or(v)),
            }
        }
        Terminator::ConditionalBranch { condition, true_target, false_target } => {
            Terminator::ConditionalBranch {
                condition: load_replacements.get(condition).copied().unwrap_or(*condition),
                true_target: true_target.clone(),
                false_target: false_target.clone(),
            }
        }
        other => other.clone(),
    };

    block.instructions = new_instructions;
    block.terminator = (*term_id, new_term);
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, Instruction, LocalId, Terminator, UnaryOp};

    #[test]
    fn test_simple_cell_promotion() {
        // CFG: %0 = Alloc; Store(0, 2); %3 = Load(0); %4 = UnaryOp(Hash, 3); Return(4)
        // Should become: %4 = UnaryOp(Hash, 2); Return(4)

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::alloc()),
                (LocalId::from(1), Instruction::store(LocalId::from(0), LocalId::from(2))),
                (LocalId::from(3), Instruction::load(LocalId::from(0))),
                (LocalId::from(4), Instruction::unary_op(
                    UnaryOp::Hash,
                    LocalId::from(3),
                )),
            ],
            (LocalId::from(5), Terminator::ret(Some(LocalId::from(4)))),
        );

        let cfg = Cfg::single_entry(entry);

        let mut local_gen = LocalIdGenerator::new();
        let result = mem2reg(&cfg, &mut local_gen);

        match result {
            Mem2RegResult::Success { cfg: new_cfg, cells_promoted } => {
                assert_eq!(cells_promoted, 1);
                // The new CFG should have just UnaryOp using %2 directly
                assert_eq!(new_cfg.entry.instructions.len(), 1);
                let (target, instr) = &new_cfg.entry.instructions[0];
                assert_eq!(*target, LocalId::from(4));
                match instr {
                    Instruction::UnaryOp { op: UnaryOp::Hash, arg } => {
                        assert_eq!(*arg, LocalId::from(2));
                    }
                    _ => panic!("Expected UnaryOp"),
                }
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    #[test]
    fn test_no_cells() {
        // CFG with no Alloc
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::num_const(5)),
            ],
            (LocalId::from(1), Terminator::ret(Some(LocalId::from(0)))),
        );

        let cfg = Cfg::single_entry(entry);

        let mut local_gen = LocalIdGenerator::new();
        let result = mem2reg(&cfg, &mut local_gen);

        assert!(matches!(result, Mem2RegResult::NoCells));
    }

    #[test]
    fn test_multi_store_cell_promoted() {
        // Test that cells with multiple stores across different blocks are properly promoted
        // to SSA form without needing phi nodes (when all paths have a single reaching definition).
        //
        // Pattern (from inlined btn function):
        //   entry:
        //     %0 = Alloc
        //     Store(%0, %1)           // i = param
        //     %3 = Load(%0)           // first use of i -> should use %1
        //     branch to block_a
        //
        //   block_a:
        //     %5 = Load(%0)           // use i -> should use %1
        //     %7 = BinaryOp(Plus, %5, %6)  // i + 1
        //     Store(%0, %7)           // i = i + 1
        //     %9 = Load(%0)           // use modified i -> should use %7
        //     return %9
        //
        // Expected after promotion:
        //   entry:
        //     %1 = NumberConstant(0)
        //     branch to block_a
        //
        //   block_a:
        //     %6 = NumberConstant(1)
        //     %7 = BinaryOp(Plus, %1, %6)  // %5 replaced with %1
        //     return %7                     // %9 replaced with %7

        use crate::ir::BinaryOp;

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::alloc()),
                (LocalId::from(1), Instruction::num_const(0)),
                (LocalId::from(2), Instruction::store(LocalId::from(0), LocalId::from(1))),  // Store initial value
                (LocalId::from(3), Instruction::load(LocalId::from(0))),  // Load for first use
            ],
            (LocalId::from(4), Terminator::branch("block_a")),
        );

        let block_a = Block::new_for_test(
            vec![
                (LocalId::from(5), Instruction::load(LocalId::from(0))),  // Load i
                (LocalId::from(6), Instruction::num_const(1)),
                (LocalId::from(7), Instruction::binary_op(
                    BinaryOp::Plus,
                    LocalId::from(5),
                    LocalId::from(6),
                )),  // i + 1
                (LocalId::from(8), Instruction::store(LocalId::from(0), LocalId::from(7))),  // i = i + 1 (SECOND STORE)
                (LocalId::from(9), Instruction::load(LocalId::from(0))),  // Load modified i
            ],
            (LocalId::from(10), Terminator::ret(Some(LocalId::from(9)))),
        );

        let cfg = Cfg::with_blocks(entry, [("block_a", block_a)]);

        let mut local_gen = LocalIdGenerator::new();
        let result = mem2reg(&cfg, &mut local_gen);

        match result {
            Mem2RegResult::Success { cfg: new_cfg, cells_promoted } => {
                assert_eq!(cells_promoted, 1, "Expected 1 cell promoted");

                // Entry block should have just the NumberConstant (Alloc, Store, Load removed)
                assert_eq!(new_cfg.entry.instructions.len(), 1);
                let (id, instr) = &new_cfg.entry.instructions[0];
                assert_eq!(*id, LocalId::from(1));
                assert!(matches!(instr, Instruction::NumberConstant { .. }));

                // block_a should have NumberConstant and BinaryOp (Load and Store removed)
                let block_a = new_cfg.named.get(&Label::from("block_a".to_string())).unwrap();
                assert_eq!(block_a.instructions.len(), 2);

                // BinaryOp should use %1 directly (not %5 which was a Load)
                let (_, binary_op) = &block_a.instructions[1];
                match binary_op {
                    Instruction::BinaryOp { left, right, op: BinaryOp::Plus } => {
                        assert_eq!(*left, LocalId::from(1), "BinaryOp left should be %1 (the stored value)");
                        assert_eq!(*right, LocalId::from(6), "BinaryOp right should be %6 (constant 1)");
                    }
                    _ => panic!("Expected BinaryOp::Plus, got {:?}", binary_op),
                }

                // Return should use %7 (the BinaryOp result, not %9 which was a Load)
                match block_a.terminator_kind() {
                    Terminator::Return { value: Some(ret_val) } => {
                        assert_eq!(*ret_val, LocalId::from(7), "Return should use %7 (the incremented value)");
                    }
                    _ => panic!("Expected Return with value"),
                }
            }
            other => panic!("Expected Success, got {:?}", other),
        }
    }

    #[test]
    fn test_multi_store_with_phi_needed() {
        // Test a case where phi nodes ARE needed: different values merge at a join point.
        //
        // Pattern:
        //   entry:
        //     %0 = Alloc
        //     cond_branch %cond, block_a, block_b
        //
        //   block_a:
        //     Store(%0, %1)  // x = 1
        //     branch block_c
        //
        //   block_b:
        //     Store(%0, %2)  // x = 2
        //     branch block_c
        //
        //   block_c:
        //     %3 = Load(%0)  // x could be 1 or 2 - needs phi!
        //     return %3

        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::alloc()),
                (LocalId::from(100), Instruction::bool_const(true)), // condition
            ],
            (LocalId::from(101), Terminator::cond_branch(LocalId::from(100), "block_a", "block_b")),
        );

        let block_a = Block::new_for_test(
            vec![
                (LocalId::from(1), Instruction::num_const(1)),
                (LocalId::from(10), Instruction::store(LocalId::from(0), LocalId::from(1))),
            ],
            (LocalId::from(11), Terminator::branch("block_c")),
        );

        let block_b = Block::new_for_test(
            vec![
                (LocalId::from(2), Instruction::num_const(2)),
                (LocalId::from(20), Instruction::store(LocalId::from(0), LocalId::from(2))),
            ],
            (LocalId::from(21), Terminator::branch("block_c")),
        );

        let block_c = Block::new_for_test(
            vec![
                (LocalId::from(3), Instruction::load(LocalId::from(0))),
            ],
            (LocalId::from(30), Terminator::ret(Some(LocalId::from(3)))),
        );

        let cfg = Cfg::with_blocks(
            entry,
            [("block_a", block_a), ("block_b", block_b), ("block_c", block_c)],
        );

        let mut local_gen = LocalIdGenerator::new();
        let result = mem2reg(&cfg, &mut local_gen);

        match result {
            Mem2RegResult::Success { cfg: new_cfg, cells_promoted } => {
                assert_eq!(cells_promoted, 1, "Expected 1 cell promoted");

                // block_c should have a phi node at the beginning
                let block_c = new_cfg.named.get(&Label::from("block_c".to_string())).unwrap();

                // First instruction should be a Phi
                let (phi_id, phi_instr) = &block_c.instructions[0];
                match phi_instr {
                    Instruction::Phi { branches } => {
                        assert_eq!(branches.len(), 2, "Phi should have 2 branches");
                        // Check that it references values from block_a and block_b
                        let has_block_a = branches.iter().any(|(l, _)| l.as_str() == "block_a");
                        let has_block_b = branches.iter().any(|(l, _)| l.as_str() == "block_b");
                        assert!(has_block_a, "Phi should have branch from block_a");
                        assert!(has_block_b, "Phi should have branch from block_b");
                    }
                    _ => panic!("Expected Phi instruction, got {:?}", phi_instr),
                }

                // Return should use the phi result
                match block_c.terminator_kind() {
                    Terminator::Return { value: Some(ret_val) } => {
                        assert_eq!(*ret_val, *phi_id, "Return should use phi result");
                    }
                    _ => panic!("Expected Return with value"),
                }
            }
            other => panic!("Expected Success with phi insertion, got {:?}", other),
        }
    }
}
