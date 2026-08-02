//! `promote_cell` - replace one heap cell with SSA values.
//!
//! Pointed rule: names the function and the `Alloc` whose cell is to be
//! promoted.
//!
//! This is the highest-value single rule. Heap traffic is 47% of the
//! instructions a branch-free frame body would contain, and separately each
//! `Alloc` is a heap slot that is part of `StateShape`, so removing one shrinks
//! both the per-lane footprint and the thing states must agree on in order to
//! merge.
//!
//! # What it does
//!
//! The frontend gives every Lua local a heap cell:
//!
//! ```text
//!   %c = alloc
//!        store %c <- %v
//!   %x = load %c
//! ```
//!
//! When the cell never escapes, this is just a long way of saying `%x = %v`.
//! Promotion deletes the alloc and the stores and rewrites every load to use
//! the stored value directly.
//!
//! # Preconditions, and why they are what they are
//!
//! 1. **The cell does not escape.** Every use of `%c` must be either
//!    `load %c` or `store %c <- _`. If the pointer is stored into another cell,
//!    captured by a closure, passed to a call, or returned, then something else
//!    can reach the cell and promotion is unsound.
//!
//!    Note `store %c <- %c` style aliasing: a `Store` reports both operands as
//!    used, so the check distinguishes the two positions. `%c` in *target*
//!    position is a write through the pointer (fine); `%c` in *source* position
//!    is the pointer itself being stored somewhere (an escape).
//!
//! 2. **Exactly one store, and it dominates every load.** With a single
//!    definition there are no phis to place, so the transformation is a
//!    substitution and nothing else. Multi-store cells need real SSA
//!    construction with phi insertion; that is a separate, harder rule and is
//!    deliberately not attempted here.
//!
//!    "Dominates" is per-block plus, within the storing block, the store must
//!    precede the load textually.
//!
//! 3. **The cell is initialised before it is read on every path.** Implied by
//!    (2): the single store dominates every load.

use anyhow::{anyhow, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{Block, Cfg, FunDef, Instruction, LocalId};

use super::super::program::Program;
use super::super::validate::{all_blocks, block_label, dominator_sets, BlockKey};
use super::require;

/// Where a cell's pointer shows up.
#[derive(Debug, Default)]
struct CellUses {
    /// (block, index within block) of each `store %c <- v`, with the value.
    stores: Vec<(BlockKey, usize, LocalId)>,
    /// (block, index within block) of each `load %c`, with the load's own id.
    loads: Vec<(BlockKey, usize, LocalId)>,
    /// Any use that is neither: the cell escapes and cannot be promoted.
    escapes: Vec<String>,
}

fn collect_uses(cfg: &Cfg, cell: LocalId) -> CellUses {
    let mut uses = CellUses::default();
    for (key, block) in all_blocks(cfg) {
        for (index, (id, instr)) in block.instructions.iter().enumerate() {
            match instr {
                Instruction::Load { source } if *source == cell => {
                    uses.loads.push((key.clone(), index, *id));
                }
                Instruction::Store { target, source } if *target == cell => {
                    // Storing the pointer into itself is still an escape.
                    if *source == cell {
                        uses.escapes.push(format!(
                            "the pointer is stored into its own cell in '{}'",
                            block_label(&key)
                        ));
                    }
                    uses.stores.push((key.clone(), index, *source));
                }
                other => {
                    if other.get_used_locals().contains(&cell) {
                        uses.escapes.push(format!(
                            "used by {} in '{}'",
                            super::super::print::format_instruction(other),
                            block_label(&key)
                        ));
                    }
                }
            }
        }
        if block.terminator_kind().get_used_locals().contains(&cell) {
            uses.escapes.push(format!(
                "used by the terminator of '{}'",
                block_label(&key)
            ));
        }
    }
    uses
}

/// Locates the `alloc` that defines `cell`.
fn find_alloc(cfg: &Cfg, cell: LocalId) -> Option<(BlockKey, usize)> {
    for (key, block) in all_blocks(cfg) {
        for (index, (id, instr)) in block.instructions.iter().enumerate() {
            if *id == cell && matches!(instr, Instruction::Alloc) {
                return Some((key, index));
            }
        }
    }
    None
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Verdict {
    /// Promotable: single store, dominating all loads.
    Ready { loads: usize },
    /// The pointer leaks somewhere promotion cannot see.
    Escapes(String),
    /// More than one store: needs phi insertion, which this rule does not do.
    MultipleStores(usize),
    /// Never written, or written after being read on some path.
    StoreDoesNotDominate,
    NotAnAlloc,
}

/// Decides whether `cell` in `fun` can be promoted. Shared by `apply`, by
/// `verify`, and by the `suggest` command - the *decision* is analysis, and
/// analysis is untrusted; what matters is that `verify` re-derives it.
pub fn classify(fun: &FunDef, cell: LocalId) -> Verdict {
    if find_alloc(&fun.cfg, cell).is_none() {
        return Verdict::NotAnAlloc;
    }
    let uses = collect_uses(&fun.cfg, cell);
    if let Some(reason) = uses.escapes.first() {
        return Verdict::Escapes(reason.clone());
    }
    if uses.stores.len() != 1 {
        return Verdict::MultipleStores(uses.stores.len());
    }
    let (store_block, store_index, _) = &uses.stores[0];
    let dom = dominator_sets(&fun.cfg);
    for (load_block, load_index, _) in &uses.loads {
        let dominated = dom
            .get(load_block)
            .is_some_and(|set| set.contains(store_block));
        if !dominated {
            return Verdict::StoreDoesNotDominate;
        }
        if load_block == store_block && load_index < store_index {
            return Verdict::StoreDoesNotDominate;
        }
    }
    Verdict::Ready { loads: uses.loads.len() }
}

/// Every cell in a function that `classify` says is ready.
pub fn candidates(fun: &FunDef) -> Vec<(LocalId, usize)> {
    let mut out = Vec::new();
    for (_, block) in all_blocks(&fun.cfg) {
        for (id, instr) in &block.instructions {
            if !matches!(instr, Instruction::Alloc) {
                continue;
            }
            if let Verdict::Ready { loads } = classify(fun, *id) {
                out.push((*id, loads));
            }
        }
    }
    out.sort_by_key(|(id, _)| usize::from(*id));
    out
}

pub fn apply(program: &mut Program, function: &str, cell: LocalId) -> Result<usize> {
    let fun = program.get_mut(function)?;
    match classify(fun, cell) {
        Verdict::Ready { .. } => {}
        other => {
            return Err(anyhow!(
                "cannot promote %{} in {}: {:?}",
                usize::from(cell),
                function,
                other
            ))
        }
    }

    let uses = collect_uses(&fun.cfg, cell);
    let stored_value = uses.stores[0].2;
    let load_ids: FxHashSet<LocalId> = uses.loads.iter().map(|(_, _, id)| *id).collect();

    // Every load of this cell becomes the stored value.
    let substitution: FxHashMap<LocalId, LocalId> =
        load_ids.iter().map(|id| (*id, stored_value)).collect();

    let mut removed = 0;
    for block in blocks_mut(&mut fun.cfg) {
        let before = block.instructions.len();
        block.instructions.retain(|(id, instr)| {
            let is_alloc = *id == cell && matches!(instr, Instruction::Alloc);
            let is_store = matches!(instr, Instruction::Store { target, .. } if *target == cell);
            let is_load = load_ids.contains(id);
            !(is_alloc || is_store || is_load)
        });
        removed += before - block.instructions.len();
    }

    let resolve = |id: LocalId| *substitution.get(&id).unwrap_or(&id);
    for block in blocks_mut(&mut fun.cfg) {
        for (_, instr) in block.instructions.iter_mut() {
            *instr = instr.map_local_ids(resolve);
        }
        block.terminator.1 = block.terminator.1.map_local_ids(resolve);
    }

    Ok(removed)
}

fn blocks_mut(cfg: &mut Cfg) -> impl Iterator<Item = &mut Block> {
    std::iter::once(&mut cfg.entry).chain(cfg.named.values_mut())
}

/// Independent check.
///
/// Re-derives the precondition from the *before* program - that is the part
/// that makes the rewrite sound - and then checks that the *after* program is
/// exactly the before program with the alloc, the store and the loads deleted
/// and every load substituted by the stored value.
pub fn verify(
    before: &Program,
    after: &Program,
    function: &str,
    cell: LocalId,
) -> Result<()> {
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;

    match classify(before_fun, cell) {
        Verdict::Ready { .. } => {}
        other => {
            return Err(anyhow!(
                "promote_cell ran on %{} in {}, which is not promotable: {:?}",
                usize::from(cell),
                function,
                other
            ))
        }
    }

    let uses = collect_uses(&before_fun.cfg, cell);
    let stored_value = uses.stores[0].2;
    let load_ids: FxHashSet<LocalId> = uses.loads.iter().map(|(_, _, id)| *id).collect();

    // The cell and everything touching it must be gone.
    for (key, block) in all_blocks(&after_fun.cfg) {
        for (id, instr) in &block.instructions {
            require(
                *id != cell,
                format!("promote_cell left %{} defined in '{}'", usize::from(cell), block_label(&key)),
            )?;
            require(
                !load_ids.contains(id),
                format!(
                    "promote_cell left a load of %{} in '{}'",
                    usize::from(cell),
                    block_label(&key)
                ),
            )?;
            require(
                !instr.get_used_locals().contains(&cell),
                format!(
                    "promote_cell left a use of %{} in '{}'",
                    usize::from(cell),
                    block_label(&key)
                ),
            )?;
        }
    }

    // Everything else must be untouched except that references to a load of the
    // cell now refer to the stored value.
    let expected = |id: LocalId| {
        if load_ids.contains(&id) {
            stored_value
        } else {
            id
        }
    };
    for (key, before_block) in all_blocks(&before_fun.cfg) {
        let after_block = super::get_block(&after_fun.cfg, &key).ok_or_else(|| {
            anyhow!("promote_cell removed block '{}'", block_label(&key))
        })?;
        let mut after_iter = after_block.instructions.iter();
        for (id, instr) in &before_block.instructions {
            if *id == cell || load_ids.contains(id) {
                continue;
            }
            if matches!(instr, Instruction::Store { target, .. } if *target == cell) {
                continue;
            }
            let (after_id, after_instr) = after_iter.next().ok_or_else(|| {
                anyhow!(
                    "promote_cell dropped %{} from '{}'",
                    usize::from(*id),
                    block_label(&key)
                )
            })?;
            require(
                after_id == id,
                format!(
                    "promote_cell reordered instructions in '{}': expected %{}, found %{}",
                    block_label(&key),
                    usize::from(*id),
                    usize::from(*after_id)
                ),
            )?;
            let want = instr.map_local_ids(expected);
            require(
                format!("{:?}", want) == format!("{:?}", after_instr),
                format!(
                    "promote_cell changed %{} in '{}' unexpectedly:\n  want {}\n  got  {}",
                    usize::from(*id),
                    block_label(&key),
                    super::super::print::format_instruction(&want),
                    super::super::print::format_instruction(after_instr)
                ),
            )?;
        }
        require(
            after_iter.next().is_none(),
            format!("promote_cell added instructions to '{}'", block_label(&key)),
        )?;

        let want_term = before_block.terminator_kind().map_local_ids(expected);
        require(
            format!("{:?}", want_term) == format!("{:?}", after_block.terminator_kind()),
            format!("promote_cell changed the terminator of '{}'", block_label(&key)),
        )?;
    }

    Ok(())
}
