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

use std::collections::{HashMap, HashSet};

use crate::ir::{Block, Cfg, Instruction, Label, LocalId, LocalIdGenerator, Terminator};

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

/// Block identifier
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum BlockId {
    Entry,
    Named(Label),
}

/// Run the mem2reg pass on a CFG.
///
/// This identifies non-escaping allocations and promotes them to SSA form.
pub fn mem2reg(cfg: &Cfg, local_gen: &mut LocalIdGenerator) -> Mem2RegResult {
    // Step 1: Find all allocations and check which ones are local cells
    let cells = find_local_cells(cfg);

    if cells.is_empty() {
        return Mem2RegResult::NoCells;
    }

    // Step 2: Build SSA form for each cell
    let mut new_cfg = cfg.clone();
    let mut cells_promoted = 0;

    for cell in &cells {
        if promote_cell(&mut new_cfg, cell, local_gen) {
            cells_promoted += 1;
        }
    }

    if cells_promoted == 0 {
        return Mem2RegResult::NoCells;
    }

    // Step 3: Remove the Alloc instructions and cell-related Store/Load
    cleanup_promoted_cells(&mut new_cfg, &cells);

    Mem2RegResult::Success {
        cfg: new_cfg,
        cells_promoted,
    }
}

/// Find all local cells (non-escaping allocations)
fn find_local_cells(cfg: &Cfg) -> Vec<CellInfo> {
    let mut allocations: HashMap<LocalId, CellInfo> = HashMap::new();
    let mut escaping: HashSet<LocalId> = HashSet::new();

    // First pass: find all Allocs and their uses
    for (block_id, block) in iter_blocks_with_id(cfg) {
        for (idx, (target_id, instruction)) in block.instructions.iter().enumerate() {
            match instruction {
                Instruction::Alloc => {
                    allocations.insert(*target_id, CellInfo {
                        alloc_id: *target_id,
                        stores: Vec::new(),
                        loads: Vec::new(),
                    });
                }
                Instruction::Store { target, source } => {
                    // Store TO a cell is fine
                    if let Some(cell) = allocations.get_mut(target) {
                        cell.stores.push((block_id.clone(), idx, *source));
                    }
                    // Store OF a cell (as value) means it escapes
                    if allocations.contains_key(source) && target != source {
                        escaping.insert(*source);
                    }
                }
                Instruction::Load { source } => {
                    if let Some(cell) = allocations.get_mut(source) {
                        cell.loads.push((block_id.clone(), idx, *target_id));
                    }
                }
                Instruction::Call { args, .. } => {
                    // Any cell passed as argument escapes
                    for arg in args {
                        if allocations.contains_key(arg) {
                            escaping.insert(*arg);
                        }
                    }
                }
                Instruction::StoreClosure { captures, .. } => {
                    // Any cell captured escapes
                    for cap in captures {
                        if allocations.contains_key(cap) {
                            escaping.insert(*cap);
                        }
                    }
                }
                _ => {}
            }
        }

        // Check terminator for returns
        if let Terminator::Return { value: Some(ret_id) } = &block.terminator.1 {
            if allocations.contains_key(ret_id) {
                escaping.insert(*ret_id);
            }
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

    // Track the current value in the cell
    let mut current_value: Option<LocalId> = None;

    // Map from Load target to the value it should use
    let mut load_replacements: HashMap<LocalId, LocalId> = HashMap::new();

    // Scan through instructions
    for (_, instruction) in &block.instructions {
        match instruction {
            Instruction::Store { target, source } if *target == cell.alloc_id => {
                current_value = Some(*source);
            }
            Instruction::Load { source } if *source == cell.alloc_id => {
                // This Load should be replaced with current_value
                // We'll handle this in a second pass
            }
            _ => {}
        }
    }

    // Second pass: build load replacements
    current_value = None;
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

/// Promote a cell in a multi-block CFG using SSA construction
fn promote_cell_multi_block(cfg: &mut Cfg, cell: &CellInfo, local_gen: &mut LocalIdGenerator) -> bool {
    // Compute predecessors
    let predecessors = compute_predecessors(cfg);

    // For each block, compute the "incoming" value at the start
    // and track definitions within the block

    // This uses a simplified version of the Braun algorithm:
    // 1. Process blocks in some order
    // 2. At each Load, look up the current value
    // 3. At block boundaries, insert phi nodes if needed

    // For now, handle the common case: cell is stored once in entry block,
    // and only read (never re-stored) in other blocks

    // Check if this is the simple "store once, read many" pattern
    let entry_stores: Vec<_> = cell.stores.iter()
        .filter(|(bid, _, _)| *bid == BlockId::Entry)
        .collect();

    let other_stores: Vec<_> = cell.stores.iter()
        .filter(|(bid, _, _)| *bid != BlockId::Entry)
        .collect();

    if entry_stores.len() == 1 && other_stores.is_empty() {
        // Simple case: one store in entry, reads in other blocks
        let (_, _, source_value) = entry_stores[0];

        // All loads should use this source value
        let mut load_replacements: HashMap<LocalId, LocalId> = HashMap::new();
        for (_, _, load_target) in &cell.loads {
            load_replacements.insert(*load_target, *source_value);
        }

        // Rewrite all blocks
        rewrite_block_for_cell(&mut cfg.entry, cell, &load_replacements);
        for (_, block) in cfg.named.iter_mut() {
            rewrite_block_for_cell(block, cell, &load_replacements);
        }

        return true;
    }

    // TODO: Handle more complex cases with proper phi node insertion
    // For now, bail on complex multi-store patterns
    false
}

/// Rewrite a block to remove cell operations and use replacements
fn rewrite_block_for_cell(
    block: &mut Block,
    cell: &CellInfo,
    load_replacements: &HashMap<LocalId, LocalId>,
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
            // Skip Load from this cell
            if let Instruction::Load { source } = instruction {
                if *source == cell.alloc_id {
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

/// Compute predecessor map for the CFG
fn compute_predecessors(cfg: &Cfg) -> HashMap<BlockId, Vec<BlockId>> {
    let mut preds: HashMap<BlockId, Vec<BlockId>> = HashMap::new();

    // Entry block's successors
    add_successors(&cfg.entry.terminator.1, BlockId::Entry, &mut preds);

    // Named blocks' successors
    for (label, block) in &cfg.named {
        add_successors(&block.terminator.1, BlockId::Named(label.clone()), &mut preds);
    }

    preds
}

fn add_successors(term: &Terminator, from: BlockId, preds: &mut HashMap<BlockId, Vec<BlockId>>) {
    match term {
        Terminator::UnconditionalBranch { target } => {
            preds.entry(BlockId::Named(target.clone())).or_default().push(from);
        }
        Terminator::ConditionalBranch { true_target, false_target, .. } => {
            preds.entry(BlockId::Named(true_target.clone())).or_default().push(from.clone());
            preds.entry(BlockId::Named(false_target.clone())).or_default().push(from);
        }
        Terminator::Return { .. } => {}
    }
}

/// Remove promoted cell operations from the CFG
fn cleanup_promoted_cells(cfg: &mut Cfg, cells: &[CellInfo]) {
    // This is now handled in the rewrite functions
    // Kept for potential future cleanup needs
}

/// Iterate over all blocks with their IDs
fn iter_blocks_with_id(cfg: &Cfg) -> Vec<(BlockId, &Block)> {
    let mut result = vec![(BlockId::Entry, &cfg.entry)];
    for (label, block) in &cfg.named {
        result.push((BlockId::Named(label.clone()), block));
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, Instruction, LocalId, Terminator, UnaryOp};
    use crate::pico8_num::Pico8Num;
    use std::collections::HashMap as StdHashMap;
    use std::hash::BuildHasherDefault;
    use rustc_hash::FxHasher;

    type FxHashMap<K, V> = StdHashMap<K, V, BuildHasherDefault<FxHasher>>;

    #[test]
    fn test_simple_cell_promotion() {
        // CFG: %0 = Alloc; Store(0, 2); %3 = Load(0); %4 = UnaryOp(Hash, 3); Return(4)
        // Should become: %4 = UnaryOp(Hash, 2); Return(4)

        let entry = Block {
            instructions: vec![
                (LocalId::from(0), Instruction::Alloc),
                (LocalId::from(1), Instruction::Store {
                    target: LocalId::from(0),
                    source: LocalId::from(2)
                }),
                (LocalId::from(3), Instruction::Load { source: LocalId::from(0) }),
                (LocalId::from(4), Instruction::UnaryOp {
                    op: UnaryOp::Hash,
                    arg: LocalId::from(3)
                }),
            ],
            terminator: (LocalId::from(5), Terminator::Return { value: Some(LocalId::from(4)) }),
            hint_normalize: false,
        };

        let cfg = Cfg {
            entry,
            named: FxHashMap::default(),
        };

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
        let entry = Block {
            instructions: vec![
                (LocalId::from(0), Instruction::NumberConstant { value: Pico8Num::from_i16(5) }),
            ],
            terminator: (LocalId::from(1), Terminator::Return { value: Some(LocalId::from(0)) }),
            hint_normalize: false,
        };

        let cfg = Cfg {
            entry,
            named: FxHashMap::default(),
        };

        let mut local_gen = LocalIdGenerator::new();
        let result = mem2reg(&cfg, &mut local_gen);

        assert!(matches!(result, Mem2RegResult::NoCells));
    }
}
