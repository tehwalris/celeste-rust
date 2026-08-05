//! `promote_capture` - capture a *value* instead of a *cell*.
//!
//! Pointed rule, keyed by the **callee** and a capture position. One `FunDef` is
//! shared by every closure made from it - `obj.collide_49` has 32 creation sites
//! - so all of them have to change together. That is why the key is the callee
//! rather than a location.
//!
//! # What it does
//!
//! A Lua local that a closure captures is mutable, so the frontend gives it a
//! heap cell and the closure captures a *pointer* to that cell:
//!
//! ```text
//!   in init_object:                 in obj.check_50, captures [%10]:
//!     %31 = alloc                     %9 = load %10
//!           store %31 <- %32          ... uses %9 ...
//!           store_closure %128 <- obj.check_50 [%31]
//! ```
//!
//! `init_object` writes `local obj = {}` once and never reassigns it, so the box
//! is pure overhead. Promotion makes the creation site capture the stored value
//! and turns the callee's `load` into nothing at all:
//!
//! ```text
//!           store_closure %128 <- obj.check_50 [%32]
//!                                   (uses %10 directly)
//! ```
//!
//! # Why it is worth a rule of its own
//!
//! This is what unblocks inlining the seven `obj.*` methods, which are the
//! hottest functions in the profile. `inline` already handles any call site; it
//! refuses these only because it cannot bind a callee's `capture_ids`. Binding
//! them needs a local at the call site that holds the captured value, and before
//! this rule the captured thing is a cell pointer that no local at the call site
//! holds - only its contents. See plans/rewrite-plan.md section 10.
//!
//! Separately, the capture is the *only* thing that makes `%31` escape, so
//! promoting all seven lets `promote_cell` delete the cell entirely.
//!
//! # Preconditions
//!
//! 1. **The callee only reads the capture.** Every use of `capture_ids[index]`
//!    inside the callee is `load capture`. If the callee stored through the
//!    pointer, or re-captured it into a nested closure, the cell would be doing
//!    real work and freezing its contents would change behaviour.
//!
//! 2. **At every creation site, the captured cell is a local box written exactly
//!    once, in the same block, before the capture.** Concretely: `%c = alloc`,
//!    then exactly one `store %c <- %v` in the whole function, then
//!    `store_closure ... [%c]`, all three in one basic block in that order.
//!
//!    The same-block restriction is what makes this checkable without any
//!    reasoning about loops. If the alloc were outside a loop and the store
//!    inside it, "exactly one store instruction" would still hold while the cell
//!    changed value on every iteration, and a closure created early would
//!    observe a later write. Within one block there is no such gap: the store
//!    that filled the cell is the one that just ran. All 224 `obj.*` creation
//!    sites satisfy this; the only sites that do not are `anonymous_59`'s, whose
//!    captures come from enclosing blocks.
//!
//! 3. **The cell does not escape into anything that could write it.** Every use
//!    of `%c` in the defining function is a `load`, a `store` into it, or a
//!    capture position of a `store_closure`; and every callee capturing it that
//!    way satisfies (1). Otherwise some other holder of the pointer could
//!    overwrite the cell after the closure was made.
//!
//! Note that (2) and (3) still permit the cell to survive: promoting
//! `obj.check_50`'s capture leaves the other six closures capturing `%31`. Each
//! `(callee, index)` pair is promoted independently.

use anyhow::{anyhow, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{Cfg, FunDef, GlobalId, Instruction, LocalId};

use super::super::program::Program;
use super::super::validate::{all_blocks, block_label, BlockKey};
use super::require;

/// One `store_closure` that builds a closure of the callee being promoted.
#[derive(Debug, Clone)]
pub struct CreationSite {
    /// The function containing the `store_closure`.
    pub function: String,
    pub block: BlockKey,
    /// Index of the `store_closure` within the block.
    pub index: usize,
    /// The cell currently captured.
    pub cell: LocalId,
    /// The value that cell holds, which will be captured instead.
    pub value: LocalId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Verdict {
    Ready { sites: usize, loads: usize },
    /// The callee has no such capture position.
    NoSuchCapture,
    /// The callee does something with the capture other than load it.
    CaptureIsNotReadOnly(String),
    /// A creation site does not have the cell shape this rule requires.
    BadCreationSite(String),
    /// No closure of this callee is ever created, so there is nothing to key on.
    NoCreationSites,
}

/// Every `load capture` in the callee, or a description of the first use that
/// is not one.
fn read_only_loads(callee: &FunDef, capture: LocalId) -> std::result::Result<Vec<LocalId>, String> {
    let mut loads = Vec::new();
    for (key, block) in all_blocks(&callee.cfg) {
        for (id, instr) in &block.instructions {
            match instr {
                Instruction::Load { source } if *source == capture => loads.push(*id),
                other if other.get_used_locals().contains(&capture) => {
                    return Err(format!(
                        "{} in '{}'",
                        super::super::print::format_instruction(other),
                        block_label(&key)
                    ))
                }
                _ => {}
            }
        }
        if block.terminator_kind().get_used_locals().contains(&capture) {
            return Err(format!("the terminator of '{}'", block_label(&key)));
        }
    }
    Ok(loads)
}

/// Checks precondition (2) and (3) for one `store_closure`, returning the value
/// the cell holds.
fn creation_site_value(
    cfg: &Cfg,
    key: &BlockKey,
    store_closure_index: usize,
    cell: LocalId,
) -> std::result::Result<LocalId, String> {
    let block = super::get_block(cfg, key).ok_or_else(|| "missing block".to_string())?;

    // (2) alloc, then the single store, then the capture - in this block, in
    // this order.
    let alloc_at = block
        .instructions
        .iter()
        .position(|(id, instr)| *id == cell && matches!(instr, Instruction::Alloc));
    let Some(alloc_at) = alloc_at else {
        return Err(format!(
            "%{} is not allocated in '{}'",
            usize::from(cell),
            block_label(key)
        ));
    };

    let mut store_here: Option<(usize, LocalId)> = None;
    let mut stores_anywhere = 0;
    for (other_key, other_block) in all_blocks(cfg) {
        for (index, (_, instr)) in other_block.instructions.iter().enumerate() {
            if let Instruction::Store { target, source } = instr {
                if *target == cell {
                    stores_anywhere += 1;
                    if other_key == *key {
                        store_here = Some((index, *source));
                    }
                }
            }
        }
    }
    if stores_anywhere != 1 {
        return Err(format!(
            "%{} is stored {} times; this rule needs exactly one write",
            usize::from(cell),
            stores_anywhere
        ));
    }
    let Some((store_at, value)) = store_here else {
        return Err(format!(
            "the single store to %{} is not in '{}'",
            usize::from(cell),
            block_label(key)
        ));
    };
    if !(alloc_at < store_at && store_at < store_closure_index) {
        return Err(format!(
            "%{} is not allocated, then stored, then captured within '{}'",
            usize::from(cell),
            block_label(key)
        ));
    }
    if value == cell {
        return Err(format!("%{} is stored into itself", usize::from(cell)));
    }

    // (3) the pointer goes nowhere a write could come from.
    for (other_key, other_block) in all_blocks(cfg) {
        for (_, instr) in &other_block.instructions {
            let ok = match instr {
                Instruction::Load { source } => *source == cell,
                Instruction::Store { target, source } => *target == cell && *source != cell,
                Instruction::StoreClosure { target, captures, .. } => {
                    *target != cell && captures.contains(&cell)
                }
                Instruction::Alloc => true,
                _ => false,
            };
            if !ok && instr.get_used_locals().contains(&cell) {
                return Err(format!(
                    "%{} escapes via {} in '{}'",
                    usize::from(cell),
                    super::super::print::format_instruction(instr),
                    block_label(&other_key)
                ));
            }
        }
        if other_block.terminator_kind().get_used_locals().contains(&cell) {
            return Err(format!(
                "%{} escapes via the terminator of '{}'",
                usize::from(cell),
                block_label(&other_key)
            ));
        }
    }

    Ok(value)
}

/// Every `store_closure` in the program that builds a closure of `callee`,
/// together with the value its captured cell holds.
///
/// Also enforces (1) transitively-by-refusal: any *other* callee that captures
/// the same cell must be read-only too, otherwise it could overwrite the cell
/// after this closure froze its value.
fn creation_sites(
    program: &Program,
    callee_name: &str,
    index: usize,
) -> std::result::Result<Vec<CreationSite>, String> {
    let target = GlobalId::from(callee_name.to_string());
    let mut out = Vec::new();
    for (fun_name, fun) in program.functions.iter() {
        for (key, block) in all_blocks(&fun.cfg) {
            for (sc_index, (_, instr)) in block.instructions.iter().enumerate() {
                let Instruction::StoreClosure { fun_def, captures, .. } = instr else {
                    continue;
                };
                if *fun_def != target {
                    continue;
                }
                let cell = *captures.get(index).ok_or_else(|| {
                    format!(
                        "a closure of {} in {} captures {} value(s), so there is no index {}",
                        callee_name,
                        fun_name.as_str(),
                        captures.len(),
                        index
                    )
                })?;
                let value = creation_site_value(&fun.cfg, &key, sc_index, cell)
                    .map_err(|e| format!("in {}: {}", fun_name.as_str(), e))?;

                // Anyone else capturing this cell must also only read it.
                for (other_key, other_block) in all_blocks(&fun.cfg) {
                    for (_, other) in &other_block.instructions {
                        let Instruction::StoreClosure { fun_def: other_fun, captures: other_caps, .. } =
                            other
                        else {
                            continue;
                        };
                        for (other_index, other_cell) in other_caps.iter().enumerate() {
                            if *other_cell != cell {
                                continue;
                            }
                            let other_def = program
                                .get(other_fun.as_str())
                                .map_err(|e| e.to_string())?;
                            let other_capture =
                                *other_def.capture_ids.get(other_index).ok_or_else(|| {
                                    format!(
                                        "{} captures {} value(s) but {} passes {}",
                                        other_fun.as_str(),
                                        other_def.capture_ids.len(),
                                        fun_name.as_str(),
                                        other_caps.len()
                                    )
                                })?;
                            read_only_loads(other_def, other_capture).map_err(|e| {
                                format!(
                                    "%{} is also captured by {} (at '{}' in {}), which is not \
                                     read-only: {}",
                                    usize::from(cell),
                                    other_fun.as_str(),
                                    block_label(&other_key),
                                    fun_name.as_str(),
                                    e
                                )
                            })?;
                        }
                    }
                }

                out.push(CreationSite {
                    function: fun_name.as_str().to_string(),
                    block: key.clone(),
                    index: sc_index,
                    cell,
                    value,
                });
            }
        }
    }
    Ok(out)
}

/// Decides whether capture `index` of `callee_name` can be promoted.
///
/// Shared by `apply`, by `verify` and by `suggest` - the *decision* is analysis,
/// and analysis is untrusted. What matters is that `verify` re-derives it from
/// the before program.
pub fn classify(program: &Program, callee_name: &str, index: usize) -> Verdict {
    let Ok(callee) = program.get(callee_name) else {
        return Verdict::NoSuchCapture;
    };
    let Some(capture) = callee.capture_ids.get(index).copied() else {
        return Verdict::NoSuchCapture;
    };
    let loads = match read_only_loads(callee, capture) {
        Ok(loads) => loads,
        Err(reason) => return Verdict::CaptureIsNotReadOnly(reason),
    };
    match creation_sites(program, callee_name, index) {
        Err(reason) => Verdict::BadCreationSite(reason),
        Ok(sites) if sites.is_empty() => Verdict::NoCreationSites,
        Ok(sites) => Verdict::Ready { sites: sites.len(), loads: loads.len() },
    }
}

/// Every `(callee, index)` in the program that `classify` says is ready.
pub fn candidates(program: &Program) -> Vec<(String, usize, usize)> {
    let mut out = Vec::new();
    for (name, fun) in program.functions.iter() {
        for index in 0..fun.capture_ids.len() {
            if let Verdict::Ready { sites, .. } = classify(program, name.as_str(), index) {
                out.push((name.as_str().to_string(), index, sites));
            }
        }
    }
    out.sort();
    out
}

pub fn apply(program: &mut Program, callee_name: &str, index: usize) -> Result<usize> {
    match classify(program, callee_name, index) {
        Verdict::Ready { .. } => {}
        other => {
            return Err(anyhow!(
                "cannot promote capture {} of {}: {:?}",
                index,
                callee_name,
                other
            ))
        }
    }

    let sites = creation_sites(program, callee_name, index).map_err(|e| anyhow!(e))?;
    let callee = program.get(callee_name)?;
    let capture = callee.capture_ids[index];
    let load_ids: FxHashSet<LocalId> = read_only_loads(callee, capture)
        .map_err(|e| anyhow!(e))?
        .into_iter()
        .collect();

    // --- the callee: `%x = load %cap` becomes `%cap` itself ---
    // The IR has no copy instruction, so this is a substitution rather than a
    // replacement.
    let callee = program.get_mut(callee_name)?;
    let mut changes = 0;
    for block in blocks_mut(&mut callee.cfg) {
        let before = block.instructions.len();
        block.instructions.retain(|(id, _)| !load_ids.contains(id));
        changes += before - block.instructions.len();
    }
    let resolve = |id: LocalId| if load_ids.contains(&id) { capture } else { id };
    for block in blocks_mut(&mut callee.cfg) {
        for (_, instr) in block.instructions.iter_mut() {
            *instr = instr.map_local_ids(resolve);
        }
        block.terminator.1 = block.terminator.1.map_local_ids(resolve);
    }

    // --- the creation sites: capture the value instead of the cell ---
    for site in &sites {
        let fun = program.get_mut(&site.function)?;
        let block = super::get_block_mut(&mut fun.cfg, &site.block)
            .ok_or_else(|| anyhow!("promote_capture: creation site block disappeared"))?;
        let (_, instr) = &mut block.instructions[site.index];
        let Instruction::StoreClosure { captures, .. } = instr else {
            return Err(anyhow!("promote_capture: creation site is not a store_closure"));
        };
        captures[index] = site.value;
        changes += 1;
    }

    Ok(changes)
}

fn blocks_mut(cfg: &mut Cfg) -> impl Iterator<Item = &mut crate::ir::Block> {
    std::iter::once(&mut cfg.entry).chain(cfg.named.values_mut())
}

/// Independent check.
///
/// Re-derives the precondition from the *before* program - that is the half that
/// makes the rewrite sound - then checks the after program is exactly the before
/// program with the callee's loads substituted away and each creation site's
/// capture repointed at the stored value. Everything else, in every function,
/// must be untouched.
pub fn verify(
    before: &Program,
    after: &Program,
    callee_name: &str,
    index: usize,
) -> Result<()> {
    match classify(before, callee_name, index) {
        Verdict::Ready { .. } => {}
        other => {
            return Err(anyhow!(
                "promote_capture ran on capture {} of {}, which is not promotable: {:?}",
                index,
                callee_name,
                other
            ))
        }
    }

    let before_callee = before.get(callee_name)?;
    let capture = before_callee.capture_ids[index];
    let load_ids: FxHashSet<LocalId> = read_only_loads(before_callee, capture)
        .map_err(|e| anyhow!(e))?
        .into_iter()
        .collect();
    let sites = creation_sites(before, callee_name, index).map_err(|e| anyhow!(e))?;

    // Where a creation site's capture is expected to have moved.
    let mut expected_site: FxHashMap<(String, String, usize), LocalId> = FxHashMap::default();
    for site in &sites {
        expected_site.insert(
            (site.function.clone(), block_label(&site.block), site.index),
            site.value,
        );
    }

    require(
        before.functions.len() == after.functions.len(),
        "promote_capture changed the set of functions",
    )?;

    for (name, before_fun) in before.functions.iter() {
        let after_fun = after.get(name.as_str())?;
        let is_callee = name.as_str() == callee_name;
        require(
            before_fun.capture_ids == after_fun.capture_ids
                && before_fun.arg_ids == after_fun.arg_ids,
            format!("promote_capture changed the signature of {}", name.as_str()),
        )?;

        for (key, before_block) in all_blocks(&before_fun.cfg) {
            let after_block = super::get_block(&after_fun.cfg, &key).ok_or_else(|| {
                anyhow!(
                    "promote_capture removed '{}' from {}",
                    block_label(&key),
                    name.as_str()
                )
            })?;
            let mut after_iter = after_block.instructions.iter();
            for (position, (id, instr)) in before_block.instructions.iter().enumerate() {
                if is_callee && load_ids.contains(id) {
                    continue;
                }
                let (after_id, after_instr) = after_iter.next().ok_or_else(|| {
                    anyhow!(
                        "promote_capture dropped %{} from '{}' in {}",
                        usize::from(*id),
                        block_label(&key),
                        name.as_str()
                    )
                })?;
                require(
                    after_id == id,
                    format!(
                        "promote_capture reordered '{}' in {}: expected %{}, found %{}",
                        block_label(&key),
                        name.as_str(),
                        usize::from(*id),
                        usize::from(*after_id)
                    ),
                )?;

                // What the instruction should have become.
                let mut want = if is_callee {
                    instr.map_local_ids(|id| if load_ids.contains(&id) { capture } else { id })
                } else {
                    instr.clone()
                };
                if let Some(value) =
                    expected_site.get(&(name.as_str().to_string(), block_label(&key), position))
                {
                    let Instruction::StoreClosure { captures, .. } = &mut want else {
                        return Err(anyhow!("promote_capture: creation site is not a closure"));
                    };
                    captures[index] = *value;
                }
                require(
                    format!("{:?}", want) == format!("{:?}", after_instr),
                    format!(
                        "promote_capture changed %{} in '{}' of {} unexpectedly:\n  want {}\n  got  {}",
                        usize::from(*id),
                        block_label(&key),
                        name.as_str(),
                        super::super::print::format_instruction(&want),
                        super::super::print::format_instruction(after_instr)
                    ),
                )?;
            }
            require(
                after_iter.next().is_none(),
                format!(
                    "promote_capture added instructions to '{}' in {}",
                    block_label(&key),
                    name.as_str()
                ),
            )?;

            let want_term = if is_callee {
                before_block
                    .terminator_kind()
                    .map_local_ids(|id| if load_ids.contains(&id) { capture } else { id })
            } else {
                before_block.terminator_kind().clone()
            };
            require(
                format!("{:?}", want_term) == format!("{:?}", after_block.terminator_kind()),
                format!(
                    "promote_capture changed the terminator of '{}' in {}",
                    block_label(&key),
                    name.as_str()
                ),
            )?;
        }
    }

    // Belt and braces: no load of the capture may survive in the callee.
    let after_callee = after.get(callee_name)?;
    for (key, block) in all_blocks(&after_callee.cfg) {
        for (_, instr) in &block.instructions {
            require(
                !matches!(instr, Instruction::Load { source } if *source == capture),
                format!(
                    "promote_capture left a load of the capture in '{}'",
                    block_label(&key)
                ),
            )?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Label, Terminator};
    use crate::rewrite::print::format_program;
    use indexmap::IndexMap;
    use rustc_hash::FxHashMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn block(instructions: Vec<(LocalId, Instruction)>, terminator: Terminator) -> Block {
        Block {
            instructions,
            terminator: (id(900), terminator),
            hint_normalize: false,
        }
    }

    fn fun(name: &str, captures: Vec<LocalId>, entry: Block) -> FunDef {
        FunDef {
            name: GlobalId::from(name.to_string()),
            capture_ids: captures,
            arg_ids: vec![],
            cfg: Cfg::new(entry, FxHashMap::default()),
            source_span: None,
        }
    }

    fn program(funs: Vec<FunDef>) -> Program {
        let mut functions = IndexMap::new();
        for f in funs {
            functions.insert(f.name.clone(), f);
        }
        Program { functions, merge_partition_cells: Vec::new() }
    }

    /// maker: `%1 = alloc; %2 = ...; store %1 <- %2; store_closure %3 <- callee [%1]`
    /// callee: `%11 = load %10; return %11`
    fn simple() -> Program {
        let maker = fun(
            "maker",
            vec![],
            block(
                vec![
                    (id(1), Instruction::Alloc),
                    (id(2), Instruction::NilConstant),
                    (id(4), Instruction::Store { target: id(1), source: id(2) }),
                    (id(3), Instruction::Alloc),
                    (
                        id(5),
                        Instruction::StoreClosure {
                            target: id(3),
                            fun_def: GlobalId::from("callee".to_string()),
                            captures: vec![id(1)],
                        },
                    ),
                ],
                Terminator::Return { value: None },
            ),
        );
        let callee = fun(
            "callee",
            vec![id(10)],
            block(
                vec![(id(11), Instruction::Load { source: id(10) })],
                Terminator::Return { value: Some(id(11)) },
            ),
        );
        program(vec![maker, callee])
    }

    #[test]
    fn promotes_a_write_once_box() {
        let mut p = simple();
        assert!(matches!(
            classify(&p, "callee", 0),
            Verdict::Ready { sites: 1, loads: 1 }
        ));
        let before = p.clone();
        apply(&mut p, "callee", 0).unwrap();
        verify(&before, &p, "callee", 0).unwrap();

        let text = format_program(&p);
        assert!(text.contains("store_closure %3 <- callee [%2]"), "{}", text);
        assert!(!text.contains("load %10"), "{}", text);
        assert!(text.contains("return %10"), "{}", text);
    }

    /// The cell is written twice, so the closure would not see a fixed value.
    #[test]
    fn refuses_a_box_that_is_written_twice() {
        let mut p = simple();
        let maker = p.get_mut("maker").unwrap();
        maker.cfg.entry.instructions.push((
            id(6),
            Instruction::Store { target: id(1), source: id(2) },
        ));
        assert!(matches!(
            classify(&p, "callee", 0),
            Verdict::BadCreationSite(_)
        ));
        assert!(apply(&mut p, "callee", 0).is_err());
    }

    /// The callee writes through the capture, so the box is doing real work.
    #[test]
    fn refuses_a_callee_that_writes_the_capture() {
        let mut p = simple();
        let callee = p.get_mut("callee").unwrap();
        callee.cfg.entry.instructions.push((
            id(12),
            Instruction::Store { target: id(10), source: id(11) },
        ));
        assert!(matches!(
            classify(&p, "callee", 0),
            Verdict::CaptureIsNotReadOnly(_)
        ));
    }

    /// Another closure over the same cell could overwrite it after this one
    /// froze the value, so a non-read-only sibling blocks the promotion.
    #[test]
    fn refuses_when_a_sibling_closure_writes_the_same_cell() {
        let mut p = simple();
        let sibling = fun(
            "sibling",
            vec![id(20)],
            block(
                vec![
                    (id(21), Instruction::NilConstant),
                    (id(22), Instruction::Store { target: id(20), source: id(21) }),
                ],
                Terminator::Return { value: None },
            ),
        );
        p.functions.insert(sibling.name.clone(), sibling);
        let maker = p.get_mut("maker").unwrap();
        maker.cfg.entry.instructions.push((
            id(7),
            Instruction::StoreClosure {
                target: id(3),
                fun_def: GlobalId::from("sibling".to_string()),
                captures: vec![id(1)],
            },
        ));
        match classify(&p, "callee", 0) {
            Verdict::BadCreationSite(reason) => assert!(reason.contains("sibling"), "{}", reason),
            other => panic!("expected a rejection, got {:?}", other),
        }
    }

    /// The store must have run before the capture in the same block; a store in
    /// a later block leaves the cell unfilled at capture time.
    #[test]
    fn refuses_a_store_in_another_block() {
        let mut p = simple();
        let maker = p.get_mut("maker").unwrap();
        let store = maker.cfg.entry.instructions.remove(2);
        maker.cfg.entry.terminator = (
            id(900),
            Terminator::UnconditionalBranch { target: Label::from("later".to_string()) },
        );
        maker.cfg.named.insert(
            Label::from("later".to_string()),
            block(vec![store], Terminator::Return { value: None }),
        );
        assert!(matches!(
            classify(&p, "callee", 0),
            Verdict::BadCreationSite(_)
        ));
    }

    /// Storing the pointer somewhere else means an unknown holder could write
    /// the cell later.
    #[test]
    fn refuses_an_escaping_cell() {
        let mut p = simple();
        let maker = p.get_mut("maker").unwrap();
        maker.cfg.entry.instructions.push((
            id(8),
            Instruction::Store { target: id(3), source: id(1) },
        ));
        match classify(&p, "callee", 0) {
            Verdict::BadCreationSite(reason) => assert!(reason.contains("escapes"), "{}", reason),
            other => panic!("expected a rejection, got {:?}", other),
        }
    }

    /// `verify` must reject an applier that repointed the capture at the wrong
    /// local, which is the mistake that would silently change behaviour.
    #[test]
    fn verify_rejects_a_capture_pointed_at_the_wrong_value() {
        let mut p = simple();
        let before = p.clone();
        apply(&mut p, "callee", 0).unwrap();
        let maker = p.get_mut("maker").unwrap();
        for (_, instr) in maker.cfg.entry.instructions.iter_mut() {
            if let Instruction::StoreClosure { captures, .. } = instr {
                captures[0] = id(3);
            }
        }
        assert!(verify(&before, &p, "callee", 0).is_err());
    }
}
