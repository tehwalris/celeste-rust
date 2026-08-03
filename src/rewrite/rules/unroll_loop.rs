//! `unroll_loop` - replace a counted loop of statically known trip count with
//! that many copies of its body, laid out in a straight line.
//!
//! # Why
//!
//! `mask_loop` (stage K) made the pixel-move loops run a *uniform* number of
//! iterations: the branch-deciding counter is a plain `i <= 8` on constants,
//! and the lane-varying `i <= amount` only feeds masked selects inside the
//! body. The loops still execute rolled, and that costs far more than the
//! head blocks alone: every iteration re-derives the same frame-constant
//! cells (`self.hitbox`, `self.check`, `objects`, ...) because the block-local
//! `cse forward` mode cannot see across the back edge, and the classic
//! cross-block mode kills every load at every store. Unrolled and
//! block-merged, the nine copies sit in one straight line where the existing
//! block-local machinery is already allowed to sweep them.
//!
//! # The shape
//!
//! ```text
//!   P:  ..                 br H          <- preheader (any terminator into H)
//!   H:  p1 = phi [P: a1, Bm: n1]        <- any number of loop-carried phis
//!       ..
//!       c  = counter <= bound            <- counter one of the phis; bound,
//!       br c ? B1 : X                       init and step number constants
//!   B1: .. br B2                         <- the body: a linear chain,
//!   ..                                      no phis, no other predecessors
//!   Bm: .. br H
//!   X:  ..
//! ```
//!
//! After, for a trip count of N: `P` enters `H_it0`; each `H_itj` (a renamed
//! copy of the head's non-phi instructions, phi uses substituted with that
//! iteration's values) falls through to `B1_itj .. Bm_itj`, which falls into
//! `H_itj+1`; the final `H_itN` falls out to `X`. The head and chain blocks
//! are deleted; any phi elsewhere taking an edge from `H` takes it from
//! `H_itN` instead; and every use of a head-defined value outside the loop
//! is renamed through the last copy - such a use always observes the final
//! head execution, because every path out of the loop passes through it.
//! (A *chain*-defined value cannot dominate anything outside the loop, so
//! outside uses of those are refused.)
//!
//! # Soundness
//!
//! The only thing asserted is the trip count, and it is *computed*, not
//! assumed: the counter phi's initial value, its step and its bound must all
//! resolve to `NumberConstant`s, and the rule simulates the counter with the
//! same 16.16 fixed-point arithmetic the interpreter uses until the compare
//! fails. Everything else is a renaming: each copy contains exactly the
//! original instructions in the original order, so the unrolled program
//! executes the same instruction sequence as the rolled one. The head's
//! conditional branch becomes unconditional in every copy - that is the one
//! place the simulation is load-bearing, and `verify` re-derives it
//! independently from the before program.
//!
//! The dropped compare stays in every copy as a dead instruction (`dce`
//! collects it later); its per-copy value is exactly what the simulation
//! predicted, so nothing observable changes.

use anyhow::{anyhow, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{BinaryOp, Block, FunDef, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::{blocks_sorted, get_block, predecessors, require, LocalIdAllocator};

/// Loops longer than this are refused outright. Far above anything real (the
/// pixel loops run 9 times); a bound this small also caps how much code the
/// rule can emit.
const MAX_TRIP_COUNT: usize = 64;

struct Site {
    /// Key of the preheader block (`None` = the function entry).
    preheader_key: Option<Label>,
    exit: Label,
    /// The body chain, entry first, latch last.
    chain: Vec<Label>,
    /// The head's phis in order: (id, value on the preheader edge, value on
    /// the latch edge).
    phis: Vec<(LocalId, LocalId, LocalId)>,
    /// The head's non-phi instructions, in order.
    rest: Vec<(LocalId, Instruction)>,
    /// How many times the body runs.
    trip_count: usize,
}

fn number_constant(fun: &FunDef, id: LocalId) -> Option<i32> {
    for block in fun.cfg.iter_blocks() {
        for (def, instr) in &block.instructions {
            if *def == id {
                if let Instruction::NumberConstant { value } = instr {
                    return Some(value.as_raw_u32() as i32);
                }
                return None;
            }
        }
    }
    None
}

fn site(fun: &FunDef, function: &str, head: &Label) -> Result<Site> {
    let head_block = fun
        .cfg
        .named
        .get(head)
        .ok_or_else(|| anyhow!("no block named '{}' in {}", head.as_str(), function))?;
    require(
        !head_block.hint_normalize,
        format!("'{}' is a hint_normalize block; copying it would change fragmentation", head.as_str()),
    )?;

    let Terminator::ConditionalBranch { condition, true_target, false_target } =
        head_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in a conditional branch", head.as_str()));
    };
    let (body_entry, exit) = (true_target.clone(), false_target.clone());
    require(
        body_entry != exit && body_entry != *head && exit != *head,
        format!("the branch of '{}' must leave to two distinct other blocks", head.as_str()),
    )?;

    // The head: leading phis, then anything non-phi.
    let mut phis_raw: Vec<(LocalId, &Vec<(Label, LocalId)>)> = Vec::new();
    let mut rest: Vec<(LocalId, Instruction)> = Vec::new();
    for (id, instr) in &head_block.instructions {
        match instr {
            Instruction::Phi { branches } => {
                require(
                    rest.is_empty(),
                    format!("'{}' has a phi below a non-phi instruction", head.as_str()),
                )?;
                phis_raw.push((*id, branches));
            }
            _ => rest.push((*id, instr.clone())),
        }
    }

    // The chain: single unconditional steps from the body entry back to the
    // head, each block reached only from the previous one.
    let preds = predecessors(&fun.cfg);
    let mut chain: Vec<Label> = Vec::new();
    let mut current = body_entry.clone();
    loop {
        require(
            current != exit,
            format!("the body of '{}' reaches its own exit", head.as_str()),
        )?;
        require(
            !chain.contains(&current),
            format!("the body of '{}' is not a linear chain", head.as_str()),
        )?;
        let block = fun
            .cfg
            .named
            .get(&current)
            .ok_or_else(|| anyhow!("body block '{}' does not exist", current.as_str()))?;
        require(
            !block.hint_normalize,
            format!("body block '{}' is a hint_normalize block", current.as_str()),
        )?;
        require(
            !block.instructions.iter().any(|(_, i)| matches!(i, Instruction::Phi { .. })),
            format!("body block '{}' contains a phi", current.as_str()),
        )?;
        let expected_pred = if chain.is_empty() { head.clone() } else { chain.last().unwrap().clone() };
        let block_preds = preds.get(&Some(current.clone())).cloned().unwrap_or_default();
        require(
            block_preds == vec![Some(expected_pred)],
            format!("body block '{}' has predecessors outside the chain", current.as_str()),
        )?;
        let Terminator::UnconditionalBranch { target } = block.terminator_kind() else {
            return Err(anyhow!("body block '{}' does not branch unconditionally", current.as_str()));
        };
        let target = target.clone();
        chain.push(current);
        if target == *head {
            break;
        }
        require(
            chain.len() <= MAX_TRIP_COUNT,
            format!("the body of '{}' is too long", head.as_str()),
        )?;
        current = target;
    }
    let latch = chain.last().unwrap().clone();

    // Nothing else may reach the head.
    let head_preds = preds
        .get(&Some(head.clone()))
        .ok_or_else(|| anyhow!("'{}' has no predecessors", head.as_str()))?;
    let mut others: Vec<Option<Label>> = head_preds
        .iter()
        .filter(|p| p.as_ref() != Some(&latch))
        .cloned()
        .collect();
    others.dedup();
    require(
        others.len() == 1,
        format!("'{}' must be reached from exactly one block besides its latch", head.as_str()),
    )?;
    let preheader_key = others.pop().unwrap();
    let preheader_label = super::label_of(&preheader_key);

    // Each phi: exactly one preheader edge and one latch edge.
    let mut phis: Vec<(LocalId, LocalId, LocalId)> = Vec::new();
    for (id, branches) in &phis_raw {
        require(
            branches.len() == 2,
            format!("a phi of '{}' does not have exactly two edges", head.as_str()),
        )?;
        let find = |label: &Label| branches.iter().find(|(l, _)| l == label).map(|(_, v)| *v);
        let init = find(&preheader_label).ok_or_else(|| {
            anyhow!("a phi of '{}' has no edge from the preheader", head.as_str())
        })?;
        let next = find(&latch)
            .ok_or_else(|| anyhow!("a phi of '{}' has no edge from the latch", head.as_str()))?;
        phis.push((*id, init, next));
    }

    // The branch condition: `counter <= bound` (or a mirrored / strict
    // variant) where the counter is one of the phis and everything else is a
    // number constant.
    let condition_instr = rest
        .iter()
        .find(|(id, _)| id == condition)
        .map(|(_, i)| i.clone())
        .ok_or_else(|| anyhow!("'{}' does not branch on its own compare", head.as_str()))?;
    let Instruction::BinaryOp { left, op, right } = condition_instr else {
        return Err(anyhow!("the branch condition of '{}' is not a compare", head.as_str()));
    };
    let is_phi = |id: &LocalId| phis.iter().any(|(p, _, _)| p == id);
    // Normalize to `continue while cmp(counter, bound)`.
    let (counter, bound, op) = if is_phi(&left) {
        (left, right, op)
    } else if is_phi(&right) {
        let mirrored = match op {
            BinaryOp::LessThanEqual => BinaryOp::GreaterThanEqual,
            BinaryOp::LessThan => BinaryOp::GreaterThan,
            BinaryOp::GreaterThanEqual => BinaryOp::LessThanEqual,
            BinaryOp::GreaterThan => BinaryOp::LessThan,
            _ => return Err(anyhow!("the compare of '{}' is not an order compare", head.as_str())),
        };
        (right, left, mirrored)
    } else {
        return Err(anyhow!("the compare of '{}' does not test a phi", head.as_str()));
    };
    let test = |v: i32, b: i32| match op {
        BinaryOp::LessThanEqual => v <= b,
        BinaryOp::LessThan => v < b,
        BinaryOp::GreaterThanEqual => v >= b,
        BinaryOp::GreaterThan => v > b,
        _ => false,
    };
    require(
        matches!(
            op,
            BinaryOp::LessThanEqual | BinaryOp::LessThan | BinaryOp::GreaterThanEqual | BinaryOp::GreaterThan
        ),
        format!("the compare of '{}' is not an order compare", head.as_str()),
    )?;

    let bound = number_constant(fun, bound)
        .ok_or_else(|| anyhow!("the bound of '{}' is not a number constant", head.as_str()))?;
    let (_, counter_init, counter_next) = *phis
        .iter()
        .find(|(p, _, _)| *p == counter)
        .expect("counter is a phi");
    let init = number_constant(fun, counter_init)
        .ok_or_else(|| anyhow!("the initial counter of '{}' is not a number constant", head.as_str()))?;

    // The step: `next = counter + step` computed inside the chain.
    let next_def = chain
        .iter()
        .flat_map(|l| fun.cfg.named[l].instructions.iter())
        .find(|(id, _)| *id == counter_next)
        .map(|(_, i)| i.clone())
        .ok_or_else(|| {
            anyhow!("the counter of '{}' is not stepped inside the body", head.as_str())
        })?;
    let Instruction::BinaryOp { left: step_l, op: BinaryOp::Plus, right: step_r } = next_def else {
        return Err(anyhow!("the counter of '{}' is not stepped by an addition", head.as_str()));
    };
    let step_id = if step_l == counter {
        step_r
    } else if step_r == counter {
        step_l
    } else {
        return Err(anyhow!("the counter step of '{}' does not add to the counter", head.as_str()));
    };
    let step = number_constant(fun, step_id)
        .ok_or_else(|| anyhow!("the step of '{}' is not a number constant", head.as_str()))?;
    require(step != 0, format!("the step of '{}' is zero", head.as_str()))?;

    // Simulate the counter, in the same wrapping 16.16 arithmetic the
    // interpreter uses. This is the whole trip-count argument.
    let mut value = init;
    let mut trip_count = 0usize;
    while test(value, bound) {
        trip_count += 1;
        require(
            trip_count <= MAX_TRIP_COUNT,
            format!("'{}' runs more than {} times", head.as_str(), MAX_TRIP_COUNT),
        )?;
        value = value.wrapping_add(step);
    }
    require(trip_count >= 1, format!("'{}' never runs", head.as_str()))?;

    // A *head*-defined value used outside the loop always observes the final
    // head execution - every path out passes through the last head visit - so
    // such uses are fine: `apply` substitutes them through the last copy's
    // rename map. A *chain*-defined value cannot dominate anything outside
    // the loop (the preheader->head->exit path skips the chain), so an
    // outside use of one is refused.
    let mut chain_defined: FxHashSet<LocalId> = FxHashSet::default();
    for label in &chain {
        let block = &fun.cfg.named[label];
        for (id, _) in &block.instructions {
            chain_defined.insert(*id);
        }
    }
    let loop_keys: FxHashSet<Option<Label>> = std::iter::once(Some(head.clone()))
        .chain(chain.iter().map(|l| Some(l.clone())))
        .collect();
    for key in blocks_sorted(&fun.cfg) {
        if loop_keys.contains(&key) {
            continue;
        }
        let block = get_block(&fun.cfg, &key).expect("listed block exists");
        for (_, instr) in &block.instructions {
            for used in instr.get_used_locals() {
                require(
                    !chain_defined.contains(&used),
                    format!("a chain-defined value of '{}' is used outside it", head.as_str()),
                )?;
            }
        }
        for used in block.terminator_kind().get_used_locals() {
            require(
                !chain_defined.contains(&used),
                format!("a chain-defined value of '{}' is used outside it", head.as_str()),
            )?;
        }
    }

    Ok(Site { preheader_key, exit, chain, phis, rest, trip_count })
}

fn copy_label(base: &Label, iteration: usize) -> Label {
    Label::from(format!("{}_it{}", base.as_str(), iteration))
}

/// The rename map for one copy: seeded with the phi substitutions for this
/// iteration, extended with a fresh id per instruction as the copy is built.
fn seed_map(s: &Site, previous: Option<&FxHashMap<LocalId, LocalId>>) -> FxHashMap<LocalId, LocalId> {
    let mut map = FxHashMap::default();
    for (phi, init, next) in &s.phis {
        let value = match previous {
            None => *init,
            Some(prev) => *prev.get(next).unwrap_or(next),
        };
        map.insert(*phi, value);
    }
    map
}

pub fn apply(program: &mut Program, function: &str, head: &str) -> Result<usize> {
    let head = Label::from(head.to_string());
    let fun = program.get(function)?;
    let s = site(fun, function, &head)?;

    // Fresh labels must actually be fresh.
    for j in 0..=s.trip_count {
        let labels: Vec<Label> = std::iter::once(copy_label(&head, j))
            .chain(s.chain.iter().map(|b| copy_label(b, j)))
            .collect();
        for label in labels {
            require(
                !fun.cfg.named.contains_key(&label),
                format!("label '{}' already exists", label.as_str()),
            )?;
        }
    }

    let mut allocator = LocalIdAllocator::for_function(fun);
    let chain_blocks: Vec<(Label, Block)> = s
        .chain
        .iter()
        .map(|l| (l.clone(), fun.cfg.named[l].clone()))
        .collect();

    // Build the copies.
    let mut new_blocks: Vec<(Label, Block)> = Vec::new();
    let mut previous_map: Option<FxHashMap<LocalId, LocalId>> = None;
    let mut last_map: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    for j in 0..=s.trip_count {
        let mut map = seed_map(&s, previous_map.as_ref());
        let rename = |map: &FxHashMap<LocalId, LocalId>, instr: &Instruction| {
            instr.map_local_ids(|id| *map.get(&id).unwrap_or(&id))
        };

        // The head copy: the non-phi instructions, then a fall-through.
        let mut instructions = Vec::new();
        for (id, instr) in &s.rest {
            let renamed = rename(&map, instr);
            let fresh = allocator.fresh();
            map.insert(*id, fresh);
            instructions.push((fresh, renamed));
        }
        let target = if j < s.trip_count { copy_label(&s.chain[0], j) } else { s.exit.clone() };
        new_blocks.push((
            copy_label(&head, j),
            Block {
                instructions,
                terminator: (allocator.fresh(), Terminator::UnconditionalBranch { target }),
                hint_normalize: false,
            },
        ));

        if j < s.trip_count {
            for (index, (label, block)) in chain_blocks.iter().enumerate() {
                let mut instructions = Vec::new();
                for (id, instr) in &block.instructions {
                    let renamed = rename(&map, instr);
                    let fresh = allocator.fresh();
                    map.insert(*id, fresh);
                    instructions.push((fresh, renamed));
                }
                let target = match chain_blocks.get(index + 1) {
                    Some((next_label, _)) => copy_label(next_label, j),
                    None => copy_label(&head, j + 1),
                };
                new_blocks.push((
                    copy_label(label, j),
                    Block {
                        instructions,
                        terminator: (allocator.fresh(), Terminator::UnconditionalBranch { target }),
                        hint_normalize: false,
                    },
                ));
            }
        }
        last_map = map.clone();
        previous_map = Some(map);
    }

    let fun = program.get_mut(function)?;

    // Wire the preheader into the first copy.
    let preheader = match &s.preheader_key {
        None => &mut fun.cfg.entry,
        Some(l) => fun.cfg.named.get_mut(l).expect("preheader exists"),
    };
    retarget(&mut preheader.terminator.1, &head, &copy_label(&head, 0));

    // Delete the loop.
    fun.cfg.named.remove(&head);
    for label in &s.chain {
        fun.cfg.named.remove(label);
    }

    // Every use of a head-defined value outside the loop observes the final
    // head execution, so it renames through the last copy; phis that took the
    // head edge take it from the last copy. Done before the copies are
    // inserted, so only pre-existing blocks are touched.
    let last_head = copy_label(&head, s.trip_count);
    let subst = |id: LocalId| *last_map.get(&id).unwrap_or(&id);
    let fix = |block: &mut Block| {
        for (_, instr) in block.instructions.iter_mut() {
            if let Instruction::Phi { branches } = instr {
                for (label, value) in branches.iter_mut() {
                    if *label == head {
                        *label = last_head.clone();
                    }
                    *value = subst(*value);
                }
            } else {
                *instr = instr.map_local_ids(subst);
            }
        }
        block.terminator.1 = block.terminator.1.map_local_ids(subst);
    };
    fix(&mut fun.cfg.entry);
    for block in fun.cfg.named.values_mut() {
        fix(block);
    }

    for (label, block) in new_blocks {
        fun.cfg.named.insert(label, block);
    }

    // Live ranges changed; any slot allocation is stale.
    fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(s.trip_count)
}

fn retarget(terminator: &mut Terminator, from: &Label, to: &Label) {
    match terminator {
        Terminator::Return { .. } => {}
        Terminator::UnconditionalBranch { target } => {
            if target == from {
                *target = to.clone();
            }
        }
        Terminator::ConditionalBranch { true_target, false_target, .. } => {
            if true_target == from {
                *true_target = to.clone();
            }
            if false_target == from {
                *false_target = to.clone();
            }
        }
    }
}

/// Independent check. Re-derives the site - including its own trip-count
/// simulation - from the before program, then walks the after program copy by
/// copy, building the id bijection as it goes: every copied instruction must
/// be the original under the bijection-so-far (phi uses substituted with the
/// iteration's values), every copied id fresh and previously unseen, every
/// terminator wired to exactly the prescribed place. Untouched blocks must be
/// untouched, up to the preheader retarget and the head-edge phi renames.
pub fn verify(before: &Program, after: &Program, function: &str, head: &str) -> Result<()> {
    let head = Label::from(head.to_string());
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let s = site(before_fun, function, &head)?;

    let before_max = {
        let mut allocator = LocalIdAllocator::for_function(before_fun);
        allocator.fresh()
    };
    let mut seen_fresh: FxHashSet<LocalId> = FxHashSet::default();
    let mut take_fresh = |id: LocalId| -> Result<()> {
        require(id >= before_max, "a copied id is not fresh")?;
        require(seen_fresh.insert(id), "a fresh id is used twice")?;
        Ok(())
    };

    require(
        !after_fun.cfg.named.contains_key(&head),
        "unroll_loop did not remove the head",
    )?;
    for label in &s.chain {
        require(
            !after_fun.cfg.named.contains_key(label),
            "unroll_loop did not remove the body chain",
        )?;
    }
    let added = (s.trip_count + 1) + s.trip_count * s.chain.len();
    require(
        after_fun.cfg.named.len() == before_fun.cfg.named.len() - 1 - s.chain.len() + added,
        "unroll_loop changed the set of blocks beyond the prescription",
    )?;

    // Walk the copies, building the bijection.
    let mut previous_map: Option<FxHashMap<LocalId, LocalId>> = None;
    let mut last_map: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    for j in 0..=s.trip_count {
        let mut map = seed_map(&s, previous_map.as_ref());

        let check_copy = |map: &mut FxHashMap<LocalId, LocalId>,
                          take_fresh: &mut dyn FnMut(LocalId) -> Result<()>,
                          original: &[(LocalId, Instruction)],
                          copy: &Block,
                          expected_target: &Label|
         -> Result<()> {
            require(
                copy.instructions.len() == original.len(),
                "a copy has the wrong number of instructions",
            )?;
            require(!copy.hint_normalize, "a copy carries hint_normalize")?;
            for ((id, instr), (copy_id, copy_instr)) in
                original.iter().zip(copy.instructions.iter())
            {
                let expected = instr.map_local_ids(|u| *map.get(&u).unwrap_or(&u));
                require(*copy_instr == expected, "a copied instruction differs from the original")?;
                take_fresh(*copy_id)?;
                map.insert(*id, *copy_id);
            }
            take_fresh(copy.terminator.0)?;
            require(
                matches!(
                    &copy.terminator.1,
                    Terminator::UnconditionalBranch { target } if target == expected_target
                ),
                "a copy is wired to the wrong place",
            )?;
            Ok(())
        };

        let head_copy = after_fun
            .cfg
            .named
            .get(&copy_label(&head, j))
            .ok_or_else(|| anyhow!("missing head copy {}", j))?;
        let head_target =
            if j < s.trip_count { copy_label(&s.chain[0], j) } else { s.exit.clone() };
        check_copy(&mut map, &mut take_fresh, &s.rest, head_copy, &head_target)?;

        if j < s.trip_count {
            for (index, label) in s.chain.iter().enumerate() {
                let original = &before_fun.cfg.named[label];
                let copy = after_fun
                    .cfg
                    .named
                    .get(&copy_label(label, j))
                    .ok_or_else(|| anyhow!("missing body copy '{}' {}", label.as_str(), j))?;
                let target = match s.chain.get(index + 1) {
                    Some(next) => copy_label(next, j),
                    None => copy_label(&head, j + 1),
                };
                check_copy(&mut map, &mut take_fresh, &original.instructions, copy, &target)?;
            }
        }
        last_map = map.clone();
        previous_map = Some(map);
    }

    // Untouched blocks: identical up to the preheader retarget and the
    // head-edge phi renames.
    let last_head = copy_label(&head, s.trip_count);
    let loop_keys: FxHashSet<Option<Label>> = std::iter::once(Some(head.clone()))
        .chain(s.chain.iter().map(|l| Some(l.clone())))
        .collect();
    for key in blocks_sorted(&before_fun.cfg) {
        if loop_keys.contains(&key) {
            continue;
        }
        let before_block = get_block(&before_fun.cfg, &key).expect("listed block exists");
        let after_block = get_block(&after_fun.cfg, &key).ok_or_else(|| {
            anyhow!(
                "unroll_loop removed block '{}'",
                key.as_ref().map(|l| l.as_str()).unwrap_or("__entry")
            )
        })?;
        let mut expected = before_block.clone();
        if key == s.preheader_key {
            retarget(&mut expected.terminator.1, &head, &copy_label(&head, 0));
        }
        let subst = |id: LocalId| *last_map.get(&id).unwrap_or(&id);
        for (_, instr) in expected.instructions.iter_mut() {
            if let Instruction::Phi { branches } = instr {
                for (label, value) in branches.iter_mut() {
                    if *label == head {
                        *label = last_head.clone();
                    }
                    *value = subst(*value);
                }
            } else {
                *instr = instr.map_local_ids(subst);
            }
        }
        expected.terminator.1 = expected.terminator.1.map_local_ids(subst);
        require(
            *after_block == expected,
            format!(
                "unroll_loop changed block '{}' beyond the prescription",
                key.as_ref().map(|l| l.as_str()).unwrap_or("__entry")
            ),
        )?;
    }

    require(
        before.functions.len() == after.functions.len(),
        "unroll_loop changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!("unroll_loop on {} also changed {}", function, name.as_str()),
        )?;
    }
    Ok(())
}

/// Heads this rule accepts.
pub fn candidates(program: &Program) -> Vec<(String, Label, usize)> {
    let mut out = Vec::new();
    for (name, fun) in &program.functions {
        for label in fun.cfg.named.keys() {
            if let Ok(s) = site(fun, name.as_str(), label) {
                out.push((name.as_str().to_string(), label.clone(), s.trip_count));
            }
        }
    }
    out.sort_by(|a, b| (&a.0, a.1.as_str()).cmp(&(&b.0, b.1.as_str())));
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Cfg, GlobalId};
    use crate::pico8_num::Pico8Num;
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(name: &str) -> Label {
        Label::from(name.to_string())
    }

    fn block(
        instructions: Vec<(usize, Instruction)>,
        terminator_id: usize,
        terminator: Terminator,
    ) -> Block {
        Block {
            instructions: instructions.into_iter().map(|(n, i)| (id(n), i)).collect(),
            terminator: (id(terminator_id), terminator),
            hint_normalize: false,
        }
    }

    fn br(target: &str) -> Terminator {
        Terminator::UnconditionalBranch { target: label(target) }
    }

    fn br_if(condition: usize, true_target: &str, false_target: &str) -> Terminator {
        Terminator::ConditionalBranch {
            condition: id(condition),
            true_target: label(true_target),
            false_target: label(false_target),
        }
    }

    fn num(value: i16) -> Instruction {
        Instruction::NumberConstant { value: Pico8Num::from_i16(value) }
    }

    /// A masked pixel-move shape, trip count 3:
    ///
    ///   entry: init=1, step=1, bound=3, acc0=0; br head
    ///   head:  i = phi [entry: init, latch: inc]
    ///          acc = phi [entry: acc0, latch: acc2]
    ///          c = i <= bound; br c ? body : exit
    ///   body:  acc1 = acc + i; br latch
    ///   latch: inc = i + step; acc2 = acc1 + init; br head
    ///   exit:  r = phi [head: acc]; return r
    fn loop_program() -> Program {
        let entry = block(
            vec![(1, num(1)), (2, num(1)), (3, num(3)), (4, num(0))],
            5,
            br("head"),
        );
        let head = block(
            vec![
                (
                    6,
                    Instruction::Phi {
                        branches: vec![(super::super::entry_label(), id(1)), (label("latch"), id(10))],
                    },
                ),
                (
                    7,
                    Instruction::Phi {
                        branches: vec![(super::super::entry_label(), id(4)), (label("latch"), id(12))],
                    },
                ),
                (8, Instruction::BinaryOp { left: id(6), op: BinaryOp::LessThanEqual, right: id(3) }),
            ],
            9,
            br_if(8, "body", "exit"),
        );
        let body = block(
            vec![(11, Instruction::BinaryOp { left: id(7), op: BinaryOp::Plus, right: id(6) })],
            13,
            br("latch"),
        );
        let latch = block(
            vec![
                (10, Instruction::BinaryOp { left: id(6), op: BinaryOp::Plus, right: id(2) }),
                (12, Instruction::BinaryOp { left: id(11), op: BinaryOp::Plus, right: id(1) }),
            ],
            14,
            br("head"),
        );
        let exit = block(
            vec![(
                15,
                Instruction::Phi { branches: vec![(label("head"), id(7))] },
            )],
            16,
            Terminator::Return { value: Some(id(15)) },
        );

        let mut named = crate::ir::new_label_map();
        named.insert(label("head"), head);
        named.insert(label("body"), body);
        named.insert(label("latch"), latch);
        named.insert(label("exit"), exit);
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![],
            cfg: Cfg::new(entry, named),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions }
    }

    #[test]
    fn unrolls_three_iterations() {
        let before = loop_program();
        let mut after = before.clone();
        let n = apply(&mut after, "f", "head").unwrap();
        assert_eq!(n, 3);
        verify(&before, &after, "f", "head").unwrap();

        let fun = after.get("f").unwrap();
        assert!(!fun.cfg.named.contains_key(&label("head")));
        assert!(!fun.cfg.named.contains_key(&label("body")));
        assert!(!fun.cfg.named.contains_key(&label("latch")));
        // 4 head copies + 3 * 2 body copies + exit.
        assert_eq!(fun.cfg.named.len(), 4 + 6 + 1);
        // The entry falls into the first copy.
        assert!(matches!(
            fun.cfg.entry.terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == label("head_it0")
        ));
        // The last copy falls out to the exit.
        assert!(matches!(
            fun.cfg.named[&label("head_it3")].terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == label("exit")
        ));
        // The exit phi now takes the accumulator from the last copy: the value
        // threaded through three iterations, not the original phi id.
        let exit = &fun.cfg.named[&label("exit")];
        let Instruction::Phi { branches } = &exit.instructions[0].1 else { panic!() };
        assert_eq!(branches.len(), 1);
        assert_eq!(branches[0].0, label("head_it3"));
        assert_ne!(branches[0].1, id(7));
        // The first copy's body adds `acc0 + init` - the seeded values.
        let body0 = &fun.cfg.named[&label("body_it0")];
        assert_eq!(
            body0.instructions[0].1,
            Instruction::BinaryOp { left: id(4), op: BinaryOp::Plus, right: id(1) }
        );
    }

    #[test]
    fn refuses_a_lane_varying_bound() {
        let mut program = loop_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            // The bound becomes an addition, not a constant.
            fun.cfg.entry.instructions[2].1 =
                Instruction::BinaryOp { left: id(1), op: BinaryOp::Plus, right: id(2) };
        }
        let err = apply(&mut program, "f", "head").unwrap_err();
        assert!(err.to_string().contains("bound"), "{}", err);
    }

    #[test]
    fn refuses_a_body_with_a_branch() {
        let mut program = loop_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            let body = fun.cfg.named.get_mut(&label("body")).unwrap();
            body.terminator.1 = br_if(11, "latch", "exit");
        }
        let err = apply(&mut program, "f", "head").unwrap_err();
        assert!(err.to_string().contains("unconditionally"), "{}", err);
    }

    #[test]
    fn refuses_a_chain_value_used_outside() {
        let mut program = loop_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            let exit = fun.cfg.named.get_mut(&label("exit")).unwrap();
            // The exit phi takes a chain-defined value on the head edge.
            exit.instructions[0].1 =
                Instruction::Phi { branches: vec![(label("head"), id(11))] };
        }
        let err = apply(&mut program, "f", "head").unwrap_err();
        assert!(err.to_string().contains("chain-defined"), "{}", err);
    }

    #[test]
    fn renames_a_head_value_used_directly_outside() {
        // The masked-loop shape: the exit reads the accumulator phi directly,
        // not through an exit phi. It must observe the last iteration's value.
        let mut program = loop_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            let exit = fun.cfg.named.get_mut(&label("exit")).unwrap();
            exit.instructions[0] = (
                id(15),
                Instruction::BinaryOp { left: id(7), op: BinaryOp::Plus, right: id(7) },
            );
        }
        let before = program.clone();
        let mut after = program;
        apply(&mut after, "f", "head").unwrap();
        verify(&before, &after, "f", "head").unwrap();
        let fun = after.get("f").unwrap();
        let exit = &fun.cfg.named[&label("exit")];
        let Instruction::BinaryOp { left, right, .. } = &exit.instructions[0].1 else { panic!() };
        // No longer the phi id; both operands renamed consistently.
        assert_ne!(*left, id(7));
        assert_eq!(left, right);
    }

    #[test]
    fn refuses_a_loop_that_never_runs() {
        let mut program = loop_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            fun.cfg.entry.instructions[2].1 = num(0); // bound 0 < init 1
        }
        let err = apply(&mut program, "f", "head").unwrap_err();
        assert!(err.to_string().contains("never runs"), "{}", err);
    }

    #[test]
    fn verify_rejects_a_tampered_copy() {
        let before = loop_program();
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        {
            let fun = after.functions.values_mut().next().unwrap();
            let body1 = fun.cfg.named.get_mut(&label("body_it1")).unwrap();
            // Swap the addition's operands - same shape, wrong iteration
            // threading.
            let (idx, instr) = body1.instructions[0].clone();
            let Instruction::BinaryOp { left, op, right } = instr else { panic!() };
            body1.instructions[0] = (idx, Instruction::BinaryOp { left: right, op, right: left });
        }
        let err = verify(&before, &after, "f", "head").unwrap_err();
        assert!(err.to_string().contains("differs from the original"), "{}", err);
    }

    #[test]
    fn verify_rejects_a_missing_copy() {
        let before = loop_program();
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        {
            let fun = after.functions.values_mut().next().unwrap();
            fun.cfg.named.remove(&label("body_it2"));
        }
        let err = verify(&before, &after, "f", "head").unwrap_err();
        assert!(err.to_string().contains("prescription") || err.to_string().contains("missing"), "{}", err);
    }

    #[test]
    fn finds_the_fixture_site() {
        let program = loop_program();
        let found = candidates(&program);
        assert_eq!(found, vec![("f".to_string(), label("head"), 3)]);
    }
}
