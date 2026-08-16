//! `drop_dead_cell` - delete an allocated cell that nothing ever reads.
//!
//! Pointed rule: names the function and the cell's `alloc`, as `%N`.
//!
//! # Why
//!
//! Inlining a call-through-cell leaves the closure cell behind: an `alloc`,
//! a `store_closure` into it, and the `assert_closure` guard the inline
//! planted - with the call, the only real reader, gone. `dce` cannot remove
//! the cluster (stores and asserts have effects), `promote_cell` refuses
//! `store_closure` (there is no closure-literal instruction to promote it
//! to), and `speculate_region` rightly refuses to speculate an `alloc`.
//! The cluster is dead by inspection but no bulk rule may say so.
//!
//! This rule says so, for one named cell, with the whole claim checked
//! syntactically at apply time. It is deliberately NOT a change to `dce`:
//! the certified recipes replay `dce` many times, and widening a bulk
//! rule's notion of "dead" would silently change every program those
//! recipes produce. A pointed rule leaves them byte-identical.
//!
//! # The shape
//!
//! `%cell` is defined by `alloc`, and every other appearance of `%cell` in
//! the function is one of:
//!
//!   * the TARGET of a `store`/`store_empty_table`/`store_closure` - never
//!     a source, never captured, never an argument, never compared, never
//!     used by a terminator;
//!   * an `assert_closure %cell is F [caps]` - allowed only when the cell
//!     has exactly ONE store, that store is `store_closure %cell <- F
//!     [caps]` with the same function and the same capture ids, and the
//!     store dominates the assert.
//!
//! # Soundness
//!
//! No instruction reads the cell's *contents* except the tolerated asserts.
//! Because the cell's pointer never escapes (checked over every use), the
//! visible stores are the only writes, so a dominated assert compares the
//! very closure the unique `store_closure` put there against itself -
//! statically true, per lane, on every execution. Deleting it removes a
//! check that cannot fail. The stores' only effect is on contents nothing
//! sees, and the allocation's only effect is a heap cell nothing reachable
//! points to - the frame-boundary canonical walk starts from the globals
//! and never visits it. (Heap ids of later allocations shift, but ids are
//! interpreter-internal: pointer equality is identity, and every state
//! comparison walks structurally.) The differential verify and the native
//! hex oracle re-check the claim end to end, like every rule.

use anyhow::{anyhow, Result};

use crate::ir::{Block, GlobalId, Instruction, LocalId};

use super::super::program::Program;
use super::super::validate::{all_blocks, dominator_sets, BlockKey};
use super::require;

/// The role an instruction plays with respect to `cell`, or an error if it
/// uses the cell some other way.
enum Use {
    None,
    StoreInto,
    Assert { fun_def: GlobalId, captures: Vec<LocalId> },
    AssertValue,
}

fn classify(instr: &Instruction, cell: LocalId) -> Result<Use> {
    match instr {
        Instruction::Store { target, source } => {
            require(*source != cell, "cell is a store's source".to_string())?;
            Ok(if *target == cell { Use::StoreInto } else { Use::None })
        }
        Instruction::StoreEmptyTable { target } => {
            Ok(if *target == cell { Use::StoreInto } else { Use::None })
        }
        Instruction::StoreClosure { target, captures, .. } => {
            require(!captures.contains(&cell), "cell is captured".to_string())?;
            Ok(if *target == cell { Use::StoreInto } else { Use::None })
        }
        Instruction::AssertClosure { value, fun_def, captures } if *value == cell => {
            Ok(Use::Assert { fun_def: fun_def.clone(), captures: captures.clone() })
        }
        Instruction::AssertValueCell { target } if *target == cell => Ok(Use::AssertValue),
        other => {
            require(
                !other.get_used_locals().contains(&cell),
                format!("cell is read by `{:?}`", other),
            )?;
            Ok(Use::None)
        }
    }
}

/// The full analysis, shared by `apply` and `verify`: check the shape and
/// return, per block, the instruction ids to delete.
fn analyze(
    fun: &crate::ir::FunDef,
    function: &str,
    cell: LocalId,
) -> Result<Vec<LocalId>> {
    // The cell must be an alloc.
    let mut saw_alloc = false;
    let mut stores: Vec<(BlockKey, usize, LocalId, Option<(GlobalId, Vec<LocalId>)>)> = Vec::new();
    let mut asserts: Vec<(BlockKey, usize, LocalId, GlobalId, Vec<LocalId>)> = Vec::new();
    let mut value_asserts: Vec<(BlockKey, usize, LocalId)> = Vec::new();
    for (key, block) in all_blocks(&fun.cfg) {
        for (index, (id, instr)) in block.instructions.iter().enumerate() {
            if *id == cell {
                require(
                    matches!(instr, Instruction::Alloc),
                    format!("%{} in {} is not an alloc", usize::from(cell), function),
                )?;
                saw_alloc = true;
                continue;
            }
            match classify(instr, cell).map_err(|e| {
                anyhow!("%{} in {}: {}", usize::from(cell), function, e)
            })? {
                Use::None => {}
                Use::StoreInto => {
                    let closure = match instr {
                        Instruction::StoreClosure { fun_def, captures, .. } => {
                            Some((fun_def.clone(), captures.clone()))
                        }
                        _ => None,
                    };
                    stores.push((key.clone(), index, *id, closure));
                }
                Use::Assert { fun_def, captures } => {
                    asserts.push((key.clone(), index, *id, fun_def, captures));
                }
                Use::AssertValue => {
                    value_asserts.push((key.clone(), index, *id));
                }
            }
        }
        require(
            !block.terminator_kind().get_used_locals().contains(&cell),
            format!("%{} in {} is used by a terminator", usize::from(cell), function),
        )?;
    }
    require(saw_alloc, format!("%{} not defined in {}", usize::from(cell), function))?;

    if !asserts.is_empty() {
        // Guarded shape: exactly one store, a store_closure, matching and
        // dominating every assert.
        require(
            stores.len() == 1,
            format!(
                "%{} in {} has asserts but {} stores - the guarded shape needs exactly one",
                usize::from(cell),
                function,
                stores.len()
            ),
        )?;
        let (store_key, store_index, _, closure) = &stores[0];
        let (fun_def, captures) = closure.as_ref().ok_or_else(|| {
            anyhow!(
                "%{} in {}: the single store is not a store_closure",
                usize::from(cell),
                function
            )
        })?;
        let doms = dominator_sets(&fun.cfg);
        for (a_key, a_index, a_id, a_fun, a_caps) in &asserts {
            require(
                a_fun == fun_def && a_caps == captures,
                format!(
                    "assert %{} on %{} in {} names {:?} {:?}, the store names {:?} {:?}",
                    usize::from(*a_id),
                    usize::from(cell),
                    function,
                    a_fun,
                    a_caps,
                    fun_def,
                    captures
                ),
            )?;
            let dominated = if a_key == store_key {
                store_index < a_index
            } else {
                doms.get(a_key)
                    .map(|set| set.contains(store_key))
                    .unwrap_or(false)
            };
            require(
                dominated,
                format!(
                    "assert %{} on %{} in {} is not dominated by the store_closure",
                    usize::from(*a_id),
                    usize::from(cell),
                    function
                ),
            )?;
        }
    }

    if !value_asserts.is_empty() {
        // Value-assert shape: every store into the cell is a plain `store`
        // (which writes `HeapValue::Value`), and each assert is dominated by
        // at least one store - so at the assert the cell provably holds a
        // plain value and the check cannot fail. (An unstored cell has no
        // value at all - reading it aborts - so bare dominance by the alloc
        // is NOT enough; a store must have run.)
        require(
            asserts.is_empty(),
            format!(
                "%{} in {} has both closure and value asserts",
                usize::from(cell),
                function
            ),
        )?;
        require(
            stores.iter().all(|(_, _, _, closure)| closure.is_none()),
            format!(
                "%{} in {} has value asserts but a store_closure",
                usize::from(cell),
                function
            ),
        )?;
        // StoreEmptyTable also writes a non-Value; refuse it by re-checking
        // the instruction kinds directly.
        for (key, index, _, _) in &stores {
            let block = all_blocks(&fun.cfg)
                .into_iter()
                .find(|(k, _)| k == key)
                .map(|(_, b)| b)
                .ok_or_else(|| anyhow!("store block vanished"))?;
            require(
                matches!(block.instructions[*index].1, Instruction::Store { .. }),
                format!(
                    "%{} in {} has value asserts but a non-plain store",
                    usize::from(cell),
                    function
                ),
            )?;
        }
        let doms = dominator_sets(&fun.cfg);
        for (a_key, a_index, a_id) in &value_asserts {
            let dominated = stores.iter().any(|(s_key, s_index, _, _)| {
                if a_key == s_key {
                    s_index < a_index
                } else {
                    doms.get(a_key).map(|set| set.contains(s_key)).unwrap_or(false)
                }
            });
            require(
                dominated,
                format!(
                    "assert %{} on %{} in {} is not dominated by any store",
                    usize::from(*a_id),
                    usize::from(cell),
                    function
                ),
            )?;
        }
    }

    let mut doomed: Vec<LocalId> = vec![cell];
    doomed.extend(stores.iter().map(|(_, _, id, _)| *id));
    doomed.extend(asserts.iter().map(|(_, _, id, _, _)| *id));
    doomed.extend(value_asserts.iter().map(|(_, _, id)| *id));
    Ok(doomed)
}

pub fn apply(program: &mut Program, function: &str, cell: LocalId) -> Result<usize> {
    let doomed = analyze(program.get(function)?, function, cell)?;
    let fun = program.get_mut(function)?;
    let mut removed = 0;
    let blocks = std::iter::once(&mut fun.cfg.entry).chain(fun.cfg.named.values_mut());
    for block in blocks {
        let before = block.instructions.len();
        block.instructions.retain(|(id, _)| !doomed.contains(id));
        removed += before - block.instructions.len();
    }
    require(removed >= 1, "drop_dead_cell removed nothing".to_string())?;
    require(
        removed == doomed.len(),
        format!("drop_dead_cell expected to remove {}, removed {}", doomed.len(), removed),
    )?;
    Ok(removed)
}

/// Independent check: the before program satisfied the dead-cell shape, and
/// the after program is exactly the before minus the doomed cluster - block
/// for block, instruction for instruction, nothing else moved.
pub fn verify(before: &Program, after: &Program, function: &str, cell: LocalId) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "drop_dead_cell changed the set of functions".to_string(),
    )?;
    for (name, before_fun) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_fun),
            format!("drop_dead_cell on {} also changed {}", function, name.as_str()),
        )?;
    }

    let before_fun = before.get(function)?;
    let doomed = analyze(before_fun, function, cell)?;

    let after_fun = after.get(function)?;
    let before_blocks: Vec<(BlockKey, &Block)> = all_blocks(&before_fun.cfg);
    let after_blocks: Vec<(BlockKey, &Block)> = all_blocks(&after_fun.cfg);
    require(
        before_blocks.len() == after_blocks.len(),
        "drop_dead_cell changed the block count".to_string(),
    )?;
    for ((bk, b), (ak, a)) in before_blocks.iter().zip(after_blocks.iter()) {
        require(bk == ak, "drop_dead_cell changed block identity".to_string())?;
        let expected: Vec<&(LocalId, Instruction)> = b
            .instructions
            .iter()
            .filter(|(id, _)| !doomed.contains(id))
            .collect();
        let actual: Vec<&(LocalId, Instruction)> = a.instructions.iter().collect();
        require(
            expected == actual,
            "drop_dead_cell changed something besides the cell cluster".to_string(),
        )?;
        require(
            b.terminator == a.terminator && b.hint_normalize == a.hint_normalize,
            "drop_dead_cell changed a terminator or hint".to_string(),
        )?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, FunDef, Terminator};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn program(entry_instructions: Vec<(LocalId, Instruction)>) -> Program {
        let cfg = Cfg::new(
            Block {
                instructions: entry_instructions,
                terminator: (id(90), Terminator::Return { value: None }),
                hint_normalize: false,
            },
            crate::ir::new_label_map(),
        );
        let mut functions = IndexMap::new();
        functions.insert(
            crate::ir::GlobalId::from("f".to_string()),
            FunDef {
                name: crate::ir::GlobalId::from("f".to_string()),
                capture_ids: vec![],
                arg_ids: vec![],
                cfg,
                source_span: None,
            },
        );
        Program { functions, merge_partition_cells: Vec::new() }
    }

    #[test]
    fn drops_a_store_only_closure_cell_with_its_dominated_guard() {
        let before = program(vec![
            (id(0), Instruction::Alloc),
            (
                id(1),
                Instruction::StoreClosure {
                    target: id(0),
                    fun_def: crate::ir::GlobalId::from("g".to_string()),
                    captures: vec![],
                },
            ),
            (
                id(2),
                Instruction::AssertClosure {
                    value: id(0),
                    fun_def: crate::ir::GlobalId::from("g".to_string()),
                    captures: vec![],
                },
            ),
        ]);
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", id(0)).unwrap(), 3);
        verify(&before, &after, "f", id(0)).unwrap();
        assert!(after.get("f").unwrap().cfg.entry.instructions.is_empty());
    }

    #[test]
    fn refuses_a_loaded_cell() {
        let mut p = program(vec![
            (id(0), Instruction::Alloc),
            (id(1), Instruction::BoolConstant { value: true }),
            (id(2), Instruction::Store { target: id(0), source: id(1) }),
            (id(3), Instruction::Load { source: id(0) }),
        ]);
        let error = apply(&mut p, "f", id(0)).unwrap_err().to_string();
        assert!(error.contains("read"), "{}", error);
    }

    #[test]
    fn refuses_an_assert_the_store_does_not_dominate() {
        // Assert BEFORE the store: same block, store later - not dominated.
        let mut p = program(vec![
            (id(0), Instruction::Alloc),
            (
                id(2),
                Instruction::AssertClosure {
                    value: id(0),
                    fun_def: crate::ir::GlobalId::from("g".to_string()),
                    captures: vec![],
                },
            ),
            (
                id(1),
                Instruction::StoreClosure {
                    target: id(0),
                    fun_def: crate::ir::GlobalId::from("g".to_string()),
                    captures: vec![],
                },
            ),
        ]);
        let error = apply(&mut p, "f", id(0)).unwrap_err().to_string();
        assert!(error.contains("not dominated"), "{}", error);
    }

    #[test]
    fn drops_a_value_cell_with_a_dominated_value_assert() {
        let before = program(vec![
            (id(0), Instruction::Alloc),
            (id(1), Instruction::BoolConstant { value: true }),
            (id(2), Instruction::Store { target: id(0), source: id(1) }),
            (id(3), Instruction::AssertValueCell { target: id(0) }),
            (id(4), Instruction::Store { target: id(0), source: id(1) }),
        ]);
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", id(0)).unwrap(), 4);
        verify(&before, &after, "f", id(0)).unwrap();
    }

    #[test]
    fn refuses_a_value_assert_before_any_store() {
        // An unstored cell has no value at all - the assert would abort.
        let mut p = program(vec![
            (id(0), Instruction::Alloc),
            (id(3), Instruction::AssertValueCell { target: id(0) }),
            (id(1), Instruction::BoolConstant { value: true }),
            (id(2), Instruction::Store { target: id(0), source: id(1) }),
        ]);
        let error = apply(&mut p, "f", id(0)).unwrap_err().to_string();
        assert!(error.contains("not dominated"), "{}", error);
    }
}
