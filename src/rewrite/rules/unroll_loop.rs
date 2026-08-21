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
    /// Per chain block: `None` for the plain unconditional step, or the
    /// early-exit conditional: (condition id, exit-arm label, true when the
    /// EXIT is the true target). The arm block is copied per iteration.
    chain_exits: Vec<Option<(LocalId, Label, bool)>>,
    /// The distinct exit-arm labels, in first-use order.
    arms: Vec<Label>,
    /// The head's phis in order: (id, value on the preheader edge, value on
    /// the latch edge).
    phis: Vec<(LocalId, LocalId, LocalId)>,
    /// The head's non-phi instructions, in order.
    rest: Vec<(LocalId, Instruction)>,
    /// How many times the body runs.
    trip_count: usize,
    /// Guard mode (recipe `trip`): the bound is not a constant; the premise
    /// `bound == init + (trip-1)*step` is asserted in the first head copy.
    /// (Sufficient for the trip count under `<=` and a positive step, the
    /// collapse_loop doctrine at N > 1: equality makes exactly `trip`
    /// head tests pass. A lane whose bound differs fails the guard loudly
    /// and deopts.)
    guard: Option<(LocalId, crate::pico8_num::Pico8Num)>,
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

fn site(
    fun: &FunDef,
    function: &str,
    head: &Label,
    forced_trip: Option<usize>,
    exits: &[Label],
    invert: bool,
) -> Result<Site> {
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
    // Plain sense: continue on true, exit on false. `invert`: the compare
    // decides the EXIT (`br i > bound ? exit : body`, the inlined-`del`
    // scan), so the body is the false target and the continue-condition is
    // the negated compare - normalized below so everything downstream (trip
    // simulation, the `<=` guard-mode premise) sees the plain sense.
    let (body_entry, exit) = if invert {
        (false_target.clone(), true_target.clone())
    } else {
        (true_target.clone(), false_target.clone())
    };
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

    // The chain: single steps from the body entry back to the head, each
    // block reached only from the previous one. A chain block may end in a
    // conditional branch IF one side is a recipe-named exit arm (a
    // single-predecessor, phi-free block that unconditionally leaves the
    // loop); the other side continues the chain. Arms are copied per
    // iteration by `apply`, so the early exit keeps its branching structure
    // - the unroll stays a pure renaming.
    let preds = predecessors(&fun.cfg);
    let mut chain: Vec<Label> = Vec::new();
    let mut chain_exits: Vec<Option<(LocalId, Label, bool)>> = Vec::new();
    let mut arms: Vec<Label> = Vec::new();
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
        let target = match block.terminator_kind() {
            Terminator::UnconditionalBranch { target } => {
                chain_exits.push(None);
                target.clone()
            }
            Terminator::ConditionalBranch { condition, true_target, false_target } => {
                let (arm, cont, exit_on_true) = if exits.contains(true_target) {
                    (true_target.clone(), false_target.clone(), true)
                } else if exits.contains(false_target) {
                    (false_target.clone(), true_target.clone(), false)
                } else {
                    return Err(anyhow!(
                        "body block '{}' branches conditionally and neither target is a recipe-named exit arm",
                        current.as_str()
                    ));
                };
                require(
                    !arms.contains(&arm),
                    format!("exit arm '{}' is reached from two chain blocks", arm.as_str()),
                )?;
                let arm_block = fun
                    .cfg
                    .named
                    .get(&arm)
                    .ok_or_else(|| anyhow!("exit arm '{}' does not exist", arm.as_str()))?;
                require(
                    !arm_block.hint_normalize,
                    format!("exit arm '{}' is a hint_normalize block", arm.as_str()),
                )?;
                require(
                    !arm_block.instructions.iter().any(|(_, i)| matches!(i, Instruction::Phi { .. })),
                    format!("exit arm '{}' contains a phi", arm.as_str()),
                )?;
                let arm_preds = preds.get(&Some(arm.clone())).cloned().unwrap_or_default();
                require(
                    arm_preds == vec![Some(current.clone())],
                    format!("exit arm '{}' has predecessors outside the chain", arm.as_str()),
                )?;
                let Terminator::UnconditionalBranch { target: arm_target } =
                    arm_block.terminator_kind()
                else {
                    return Err(anyhow!(
                        "exit arm '{}' does not branch unconditionally",
                        arm.as_str()
                    ));
                };
                require(
                    arm_target != head && arm_target != &current,
                    format!("exit arm '{}' does not leave the loop", arm.as_str()),
                )?;
                chain_exits.push(Some((*condition, arm.clone(), exit_on_true)));
                arms.push(arm);
                cont
            }
            other => {
                return Err(anyhow!(
                    "body block '{}' ends in {:?}, not a branch",
                    current.as_str(),
                    other
                ))
            }
        };
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
    require(
        chain_exits.last() == Some(&None),
        format!("the latch of '{}' must branch unconditionally", head.as_str()),
    )?;
    // Arms must exit the LOOP, which is only fully known now.
    for arm in &arms {
        let Terminator::UnconditionalBranch { target } = fun.cfg.named[arm].terminator_kind()
        else {
            unreachable!("checked above");
        };
        require(
            target != head && !chain.contains(target) && !arms.contains(target),
            format!("exit arm '{}' does not leave the loop", arm.as_str()),
        )?;
    }
    for named in exits {
        require(
            arms.contains(named),
            format!("recipe-named exit arm '{}' was not found on the chain", named.as_str()),
        )?;
    }

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
    // Inverted sense: the compare is the EXIT condition; the loop continues
    // while its negation holds.
    let op = if invert {
        match op {
            BinaryOp::LessThanEqual => BinaryOp::GreaterThan,
            BinaryOp::LessThan => BinaryOp::GreaterThanEqual,
            BinaryOp::GreaterThanEqual => BinaryOp::LessThan,
            BinaryOp::GreaterThan => BinaryOp::LessThanEqual,
            _ => return Err(anyhow!("the compare of '{}' is not an order compare", head.as_str())),
        }
    } else {
        op
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

    let bound_id = bound;
    let bound_const = number_constant(fun, bound_id);
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

    // The trip count. Constant bound: simulate the counter in the same
    // wrapping 16.16 arithmetic the interpreter uses - that is the whole
    // trip-count argument, and no runtime guard is needed. Recipe `trip`:
    // the bound is dynamic; the premise `bound == init + (trip-1)*step` is
    // asserted at runtime in the first head copy. Under `<=` and a positive
    // step that equality makes exactly `trip` head tests pass, so the guard
    // IS the trip-count argument; a lane with a different bound fails it
    // loudly and deopts.
    let (trip_count, guard) = match (forced_trip, bound_const) {
        (None, Some(bound)) => {
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
            (trip_count, None)
        }
        (None, None) => {
            return Err(anyhow!(
                "the bound of '{}' is not a number constant (a recipe `trip` would guard it)",
                head.as_str()
            ))
        }
        (Some(trip), None) => {
            require(
                trip >= 1 && trip <= MAX_TRIP_COUNT,
                format!("`trip` for '{}' must be in 1..={}", head.as_str(), MAX_TRIP_COUNT),
            )?;
            require(
                op == BinaryOp::LessThanEqual,
                format!(
                    "`trip` mode for '{}' needs a `<=` compare (the equality premise argument)",
                    head.as_str()
                ),
            )?;
            require(
                step > 0,
                format!("`trip` mode for '{}' needs a positive step", head.as_str()),
            )?;
            let mut pinned = init;
            for _ in 1..trip {
                pinned = pinned.wrapping_add(step);
            }
            (trip, Some((bound_id, crate::pico8_num::Pico8Num::from_raw(pinned))))
        }
        (Some(_), Some(_)) => {
            return Err(anyhow!(
                "the bound of '{}' is a constant; drop `trip` and let the simulation count",
                head.as_str()
            ))
        }
    };

    // A *head*-defined value used outside the loop always observes the final
    // head execution - every path out passes through the last head visit - so
    // such uses are fine: `apply` substitutes them through the last copy's
    // rename map. A *chain*-defined value cannot dominate anything outside
    // the loop (the preheader->head->exit path skips the chain), so an
    // outside use of one is refused.
    let mut chain_defined: FxHashSet<LocalId> = FxHashSet::default();
    for label in chain.iter().chain(arms.iter()) {
        let block = &fun.cfg.named[label];
        for (id, _) in &block.instructions {
            chain_defined.insert(*id);
        }
    }
    let loop_keys: FxHashSet<Option<Label>> = std::iter::once(Some(head.clone()))
        .chain(chain.iter().map(|l| Some(l.clone())))
        .chain(arms.iter().map(|l| Some(l.clone())))
        .collect();
    for key in blocks_sorted(&fun.cfg) {
        if loop_keys.contains(&key) {
            continue;
        }
        let block = get_block(&fun.cfg, &key).expect("listed block exists");
        for (_, instr) in &block.instructions {
            match instr {
                // A phi value on an EXIT-ARM edge may be chain-defined: the
                // arm is copied per iteration and `apply` renames that value
                // through each copy's map, so the phi observes exactly what
                // the taken exit's iteration computed.
                Instruction::Phi { branches } => {
                    for (label, value) in branches {
                        if arms.contains(label) {
                            continue;
                        }
                        require(
                            !chain_defined.contains(value),
                            format!(
                                "a chain-defined value of '{}' is used outside it",
                                head.as_str()
                            ),
                        )?;
                    }
                }
                _ => {
                    for used in instr.get_used_locals() {
                        require(
                            !chain_defined.contains(&used),
                            format!(
                                "a chain-defined value of '{}' is used outside it",
                                head.as_str()
                            ),
                        )?;
                    }
                }
            }
        }
        for used in block.terminator_kind().get_used_locals() {
            require(
                !chain_defined.contains(&used),
                format!("a chain-defined value of '{}' is used outside it", head.as_str()),
            )?;
        }
    }

    Ok(Site { preheader_key, exit, chain, chain_exits, arms, phis, rest, trip_count, guard })
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

pub fn apply(
    program: &mut Program,
    function: &str,
    head: &str,
    trip: Option<usize>,
    exits: &[String],
    invert: bool,
) -> Result<usize> {
    let head = Label::from(head.to_string());
    let exits: Vec<Label> = exits.iter().map(|s| Label::from(s.clone())).collect();
    let fun = program.get(function)?;
    let s = site(fun, function, &head, trip, &exits, invert)?;

    // Fresh labels must actually be fresh.
    for j in 0..=s.trip_count {
        let labels: Vec<Label> = std::iter::once(copy_label(&head, j))
            .chain(s.chain.iter().map(|b| copy_label(b, j)))
            .chain(s.arms.iter().map(|b| copy_label(b, j)))
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
    let arm_blocks: Vec<(Label, Block)> = s
        .arms
        .iter()
        .map(|l| (l.clone(), fun.cfg.named[l].clone()))
        .collect();

    // Build the copies. Every iteration's final map is kept: exit-arm phi
    // edges in outside joins rename through the map of THEIR iteration.
    let mut new_blocks: Vec<(Label, Block)> = Vec::new();
    let mut previous_map: Option<FxHashMap<LocalId, LocalId>> = None;
    let mut iter_maps: Vec<FxHashMap<LocalId, LocalId>> = Vec::new();
    let mut last_map: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    for j in 0..=s.trip_count {
        let mut map = seed_map(&s, previous_map.as_ref());
        let rename = |map: &FxHashMap<LocalId, LocalId>, instr: &Instruction| {
            instr.map_local_ids(|id| *map.get(&id).unwrap_or(&id))
        };

        // The head copy: the trip guard (first copy, guard mode only), the
        // non-phi instructions, then a fall-through.
        // The trip guard goes BEFORE the copied head instructions when the
        // bound is defined outside the loop (the historical layout - kept
        // byte-stable for existing recipes), and AFTER them when the head
        // itself computes the bound (`i > #objects` recomputes `#` every
        // iteration), where it references iteration 0's renamed copy. Either
        // way it fires before any body copy runs.
        let bound_in_head =
            s.guard.map_or(false, |(bound_id, _)| s.rest.iter().any(|(id, _)| *id == bound_id));
        let mut instructions = Vec::new();
        let push_guard =
            |instructions: &mut Vec<(LocalId, Instruction)>,
             allocator: &mut LocalIdAllocator,
             bound: LocalId,
             pinned: crate::pico8_num::Pico8Num| {
                let kc = allocator.fresh();
                instructions.push((kc, Instruction::NumberConstant { value: pinned }));
                let g = allocator.fresh();
                instructions.push((
                    g,
                    Instruction::BinaryOp { left: bound, op: BinaryOp::TwoEqual, right: kc },
                ));
                let ga = allocator.fresh();
                instructions.push((ga, Instruction::AssertTrue { value: g }));
            };
        if j == 0 && !bound_in_head {
            if let Some((bound_id, pinned)) = s.guard {
                push_guard(&mut instructions, &mut allocator, bound_id, pinned);
            }
        }
        for (id, instr) in &s.rest {
            let renamed = rename(&map, instr);
            let fresh = allocator.fresh();
            map.insert(*id, fresh);
            instructions.push((fresh, renamed));
        }
        if j == 0 && bound_in_head {
            if let Some((bound_id, pinned)) = s.guard {
                let renamed_bound = *map.get(&bound_id).unwrap_or(&bound_id);
                push_guard(&mut instructions, &mut allocator, renamed_bound, pinned);
            }
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
                let next = match chain_blocks.get(index + 1) {
                    Some((next_label, _)) => copy_label(next_label, j),
                    None => copy_label(&head, j + 1),
                };
                let terminator = match &s.chain_exits[index] {
                    None => Terminator::UnconditionalBranch { target: next },
                    Some((condition, arm, exit_on_true)) => {
                        let cond = *map.get(condition).unwrap_or(condition);
                        let arm_copy = copy_label(arm, j);
                        let (true_target, false_target) = if *exit_on_true {
                            (arm_copy, next)
                        } else {
                            (next, arm_copy)
                        };
                        Terminator::ConditionalBranch { condition: cond, true_target, false_target }
                    }
                };
                new_blocks.push((
                    copy_label(label, j),
                    Block {
                        instructions,
                        terminator: (allocator.fresh(), terminator),
                        hint_normalize: false,
                    },
                ));
            }
            // The iteration's exit-arm copies: same renaming, terminator
            // unchanged (it already leaves the loop).
            for (label, block) in arm_blocks.iter() {
                let mut instructions = Vec::new();
                for (id, instr) in &block.instructions {
                    let renamed = rename(&map, instr);
                    let fresh = allocator.fresh();
                    map.insert(*id, fresh);
                    instructions.push((fresh, renamed));
                }
                new_blocks.push((
                    copy_label(label, j),
                    Block {
                        instructions,
                        terminator: (allocator.fresh(), block.terminator.1.clone()),
                        hint_normalize: false,
                    },
                ));
            }
        }
        last_map = map.clone();
        iter_maps.push(map.clone());
        previous_map = Some(map);
    }

    let fun = program.get_mut(function)?;

    // Wire the preheader into the first copy.
    let preheader = match &s.preheader_key {
        None => &mut fun.cfg.entry,
        Some(l) => fun.cfg.named.get_mut(l).expect("preheader exists"),
    };
    retarget(&mut preheader.terminator.1, &head, &copy_label(&head, 0));

    // Delete the loop, exit arms included.
    fun.cfg.named.remove(&head);
    for label in s.chain.iter().chain(s.arms.iter()) {
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
                let mut new_branches: Vec<(Label, LocalId)> = Vec::new();
                for (label, value) in branches.iter() {
                    if *label == head {
                        new_branches.push((last_head.clone(), subst(*value)));
                    } else if s.arms.contains(label) {
                        // One edge per iteration's arm copy, the value
                        // renamed through that iteration's map.
                        for (j, map) in iter_maps.iter().enumerate().take(s.trip_count) {
                            new_branches
                                .push((copy_label(label, j), *map.get(value).unwrap_or(value)));
                        }
                    } else {
                        new_branches.push((label.clone(), subst(*value)));
                    }
                }
                *instr = Instruction::Phi { branches: new_branches };
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
pub fn verify(
    before: &Program,
    after: &Program,
    function: &str,
    head: &str,
    trip: Option<usize>,
    exits: &[String],
    invert: bool,
) -> Result<()> {
    let head = Label::from(head.to_string());
    let exits: Vec<Label> = exits.iter().map(|s| Label::from(s.clone())).collect();
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let s = site(before_fun, function, &head, trip, &exits, invert)?;

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
    let added = (s.trip_count + 1) + s.trip_count * (s.chain.len() + s.arms.len());
    require(
        after_fun.cfg.named.len()
            == before_fun.cfg.named.len() - 1 - s.chain.len() - s.arms.len() + added,
        "unroll_loop changed the set of blocks beyond the prescription",
    )?;

    // Walk the copies, building the bijection.
    let mut previous_map: Option<FxHashMap<LocalId, LocalId>> = None;
    let mut iter_maps: Vec<FxHashMap<LocalId, LocalId>> = Vec::new();
    let mut last_map: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    for j in 0..=s.trip_count {
        let mut map = seed_map(&s, previous_map.as_ref());

        let check_copy = |map: &mut FxHashMap<LocalId, LocalId>,
                          take_fresh: &mut dyn FnMut(LocalId) -> Result<()>,
                          original: &[(LocalId, Instruction)],
                          copy: &Block,
                          skip: usize,
                          expected_term: &dyn Fn(&FxHashMap<LocalId, LocalId>) -> Terminator|
         -> Result<()> {
            require(
                copy.instructions.len() == original.len() + skip,
                "a copy has the wrong number of instructions",
            )?;
            require(!copy.hint_normalize, "a copy carries hint_normalize")?;
            for ((id, instr), (copy_id, copy_instr)) in
                original.iter().zip(copy.instructions.iter().skip(skip))
            {
                let expected = instr.map_local_ids(|u| *map.get(&u).unwrap_or(&u));
                require(*copy_instr == expected, "a copied instruction differs from the original")?;
                take_fresh(*copy_id)?;
                map.insert(*id, *copy_id);
            }
            take_fresh(copy.terminator.0)?;
            require(
                copy.terminator.1 == expected_term(map),
                "a copy is wired to the wrong place",
            )?;
            Ok(())
        };

        let head_copy = after_fun
            .cfg
            .named
            .get(&copy_label(&head, j))
            .ok_or_else(|| anyhow!("missing head copy {}", j))?;
        // The trip guard, first copy of guard mode only: exactly
        // `k = num(pinned); g = bound == k; assert_true g`, fresh ids. It is
        // a PREFIX when the bound is defined outside the loop (the historical
        // layout) and a SUFFIX referencing iteration 0's renamed bound when
        // the head itself computes the bound - mirroring `apply`.
        let bound_in_head =
            s.guard.map_or(false, |(bound_id, _)| s.rest.iter().any(|(id, _)| *id == bound_id));
        let guard_here = j == 0 && s.guard.is_some();
        let check_guard = |instrs: &[(LocalId, Instruction)],
                           bound: LocalId,
                           pinned: crate::pico8_num::Pico8Num,
                           take_fresh: &mut dyn FnMut(LocalId) -> Result<()>|
         -> Result<()> {
            let [(kc, kc_instr), (g, g_instr), (ga, ga_instr)] = instrs else {
                return Err(anyhow!("the first head copy is missing the trip guard"));
            };
            require(
                *kc_instr == Instruction::NumberConstant { value: pinned },
                "the trip guard's constant is wrong",
            )?;
            require(
                *g_instr == Instruction::BinaryOp { left: bound, op: BinaryOp::TwoEqual, right: *kc },
                "the trip guard's compare is wrong",
            )?;
            require(
                *ga_instr == Instruction::AssertTrue { value: *g },
                "the trip guard must be stated with `assert_true`",
            )?;
            take_fresh(*kc)?;
            take_fresh(*g)?;
            take_fresh(*ga)?;
            Ok(())
        };
        let head_target =
            if j < s.trip_count { copy_label(&s.chain[0], j) } else { s.exit.clone() };
        if guard_here && bound_in_head {
            let (bound_id, pinned) = s.guard.unwrap();
            require(
                head_copy.instructions.len() == s.rest.len() + 3,
                "the first head copy is missing the trip guard",
            )?;
            let trimmed = Block {
                instructions: head_copy.instructions[..s.rest.len()].to_vec(),
                terminator: head_copy.terminator.clone(),
                hint_normalize: head_copy.hint_normalize,
            };
            check_copy(&mut map, &mut take_fresh, &s.rest, &trimmed, 0, &|_| {
                Terminator::UnconditionalBranch { target: head_target.clone() }
            })?;
            let renamed_bound = *map.get(&bound_id).unwrap_or(&bound_id);
            check_guard(
                &head_copy.instructions[s.rest.len()..],
                renamed_bound,
                pinned,
                &mut take_fresh,
            )?;
        } else {
            let guard_len = if guard_here { 3 } else { 0 };
            if guard_len > 0 {
                let (bound_id, pinned) = s.guard.unwrap();
                require(
                    head_copy.instructions.len() >= 3,
                    "the first head copy is missing the trip guard",
                )?;
                check_guard(&head_copy.instructions[..3], bound_id, pinned, &mut take_fresh)?;
            }
            check_copy(&mut map, &mut take_fresh, &s.rest, head_copy, guard_len, &|_| {
                Terminator::UnconditionalBranch { target: head_target.clone() }
            })?;
        }

        if j < s.trip_count {
            for (index, label) in s.chain.iter().enumerate() {
                let original = &before_fun.cfg.named[label];
                let copy = after_fun
                    .cfg
                    .named
                    .get(&copy_label(label, j))
                    .ok_or_else(|| anyhow!("missing body copy '{}' {}", label.as_str(), j))?;
                let next = match s.chain.get(index + 1) {
                    Some(next) => copy_label(next, j),
                    None => copy_label(&head, j + 1),
                };
                let exit_spec = s.chain_exits[index].clone();
                check_copy(
                    &mut map,
                    &mut take_fresh,
                    &original.instructions,
                    copy,
                    0,
                    &|map| match &exit_spec {
                        None => Terminator::UnconditionalBranch { target: next.clone() },
                        Some((condition, arm, exit_on_true)) => {
                            let cond = *map.get(condition).unwrap_or(condition);
                            let arm_copy = copy_label(arm, j);
                            let (true_target, false_target) = if *exit_on_true {
                                (arm_copy, next.clone())
                            } else {
                                (next.clone(), arm_copy)
                            };
                            Terminator::ConditionalBranch { condition: cond, true_target, false_target }
                        }
                    },
                )?;
            }
            for label in s.arms.iter() {
                let original = &before_fun.cfg.named[label];
                let copy = after_fun
                    .cfg
                    .named
                    .get(&copy_label(label, j))
                    .ok_or_else(|| anyhow!("missing exit-arm copy '{}' {}", label.as_str(), j))?;
                let term = original.terminator.1.clone();
                check_copy(&mut map, &mut take_fresh, &original.instructions, copy, 0, &|_| {
                    term.clone()
                })?;
            }
        }
        last_map = map.clone();
        iter_maps.push(map.clone());
        previous_map = Some(map);
    }

    // Untouched blocks: identical up to the preheader retarget and the
    // head-edge phi renames.
    let last_head = copy_label(&head, s.trip_count);
    let loop_keys: FxHashSet<Option<Label>> = std::iter::once(Some(head.clone()))
        .chain(s.chain.iter().map(|l| Some(l.clone())))
        .chain(s.arms.iter().map(|l| Some(l.clone())))
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
                let mut new_branches: Vec<(Label, LocalId)> = Vec::new();
                for (label, value) in branches.iter() {
                    if *label == head {
                        new_branches.push((last_head.clone(), subst(*value)));
                    } else if s.arms.contains(label) {
                        for (j, m) in iter_maps.iter().enumerate().take(s.trip_count) {
                            new_branches
                                .push((copy_label(label, j), *m.get(value).unwrap_or(value)));
                        }
                    } else {
                        new_branches.push((label.clone(), subst(*value)));
                    }
                }
                *instr = Instruction::Phi { branches: new_branches };
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
            if let Ok(s) = site(fun, name.as_str(), label, None, &[], false) {
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
        Program { functions, merge_partition_cells: Vec::new() }
    }

    /// The guarded-trip + exit-arm shape: the bound is `1 + 2` (NOT a
    /// number constant), and the body conditionally exits to `arm`, which
    /// leaves the loop to `join` (whose phi reads a chain-defined value).
    fn guarded_loop_program() -> Program {
        let entry = block(
            vec![
                (1, num(1)),
                (2, num(2)),
                (3, Instruction::BinaryOp { left: id(1), op: BinaryOp::Plus, right: id(2) }),
                (4, Instruction::BoolConstant { value: false }),
                (5, num(0)),
            ],
            30,
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
                        branches: vec![(super::super::entry_label(), id(5)), (label("latch"), id(12))],
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
            br_if(4, "arm", "latch"),
        );
        let arm = block(vec![], 21, br("join"));
        let latch = block(
            vec![
                (10, Instruction::BinaryOp { left: id(6), op: BinaryOp::Plus, right: id(1) }),
                (12, Instruction::BinaryOp { left: id(11), op: BinaryOp::Plus, right: id(1) }),
            ],
            14,
            br("head"),
        );
        let exit = block(
            vec![(15, Instruction::Phi { branches: vec![(label("head"), id(7))] })],
            16,
            Terminator::Return { value: Some(id(15)) },
        );
        let join = block(
            vec![(22, Instruction::Phi { branches: vec![(label("arm"), id(11))] })],
            23,
            Terminator::Return { value: Some(id(22)) },
        );

        let mut named = crate::ir::new_label_map();
        named.insert(label("head"), head);
        named.insert(label("body"), body);
        named.insert(label("arm"), arm);
        named.insert(label("latch"), latch);
        named.insert(label("exit"), exit);
        named.insert(label("join"), join);
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![],
            cfg: Cfg::new(entry, named),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions, merge_partition_cells: Vec::new() }
    }

    #[test]
    fn guarded_trip_with_exit_arm_applies_and_verifies() {
        let before = guarded_loop_program();
        // Plain mode refuses: the bound is not a constant.
        let mut plain = before.clone();
        let err = apply(&mut plain, "f", "head", None, &[], false).unwrap_err();
        assert!(err.to_string().contains("recipe-named exit arm"), "{}", err);

        let mut after = before.clone();
        let exits = vec!["arm".to_string()];
        let n = apply(&mut after, "f", "head", Some(3), &exits, false).unwrap();
        assert_eq!(n, 3);
        verify(&before, &after, "f", "head", Some(3), &exits, false).unwrap();

        let fun = after.get("f").unwrap();
        // The first head copy leads with the trip guard.
        let h0 = &fun.cfg.named[&label("head_it0")];
        assert_eq!(h0.instructions[0].1, Instruction::NumberConstant { value: Pico8Num::from_i16(3) });
        assert!(matches!(h0.instructions[2].1, Instruction::AssertTrue { .. }));
        // One arm copy per iteration, and the join phi has one edge each.
        for j in 0..3 {
            assert!(fun.cfg.named.contains_key(&label(&format!("arm_it{}", j))));
        }
        let join = &fun.cfg.named[&label("join")];
        let Instruction::Phi { branches } = &join.instructions[0].1 else { panic!() };
        assert_eq!(branches.len(), 3);
        // Each edge's value is that iteration's copy of %11 - all distinct.
        let values: FxHashSet<LocalId> = branches.iter().map(|(_, v)| *v).collect();
        assert_eq!(values.len(), 3);
    }

    #[test]
    fn unrolls_three_iterations() {
        let before = loop_program();
        let mut after = before.clone();
        let n = apply(&mut after, "f", "head", None, &[], false).unwrap();
        assert_eq!(n, 3);
        verify(&before, &after, "f", "head", None, &[], false).unwrap();

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
        let err = apply(&mut program, "f", "head", None, &[], false).unwrap_err();
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
        let err = apply(&mut program, "f", "head", None, &[], false).unwrap_err();
        assert!(err.to_string().contains("recipe-named exit arm"), "{}", err);
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
        let err = apply(&mut program, "f", "head", None, &[], false).unwrap_err();
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
        apply(&mut after, "f", "head", None, &[], false).unwrap();
        verify(&before, &after, "f", "head", None, &[], false).unwrap();
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
        let err = apply(&mut program, "f", "head", None, &[], false).unwrap_err();
        assert!(err.to_string().contains("never runs"), "{}", err);
    }

    #[test]
    fn verify_rejects_a_tampered_copy() {
        let before = loop_program();
        let mut after = before.clone();
        apply(&mut after, "f", "head", None, &[], false).unwrap();
        {
            let fun = after.functions.values_mut().next().unwrap();
            let body1 = fun.cfg.named.get_mut(&label("body_it1")).unwrap();
            // Swap the addition's operands - same shape, wrong iteration
            // threading.
            let (idx, instr) = body1.instructions[0].clone();
            let Instruction::BinaryOp { left, op, right } = instr else { panic!() };
            body1.instructions[0] = (idx, Instruction::BinaryOp { left: right, op, right: left });
        }
        let err = verify(&before, &after, "f", "head", None, &[], false).unwrap_err();
        assert!(err.to_string().contains("differs from the original"), "{}", err);
    }

    #[test]
    fn verify_rejects_a_missing_copy() {
        let before = loop_program();
        let mut after = before.clone();
        apply(&mut after, "f", "head", None, &[], false).unwrap();
        {
            let fun = after.functions.values_mut().next().unwrap();
            fun.cfg.named.remove(&label("body_it2"));
        }
        let err = verify(&before, &after, "f", "head", None, &[], false).unwrap_err();
        assert!(err.to_string().contains("prescription") || err.to_string().contains("missing"), "{}", err);
    }

    #[test]
    fn finds_the_fixture_site() {
        let program = loop_program();
        let found = candidates(&program);
        assert_eq!(found, vec![("f".to_string(), label("head"), 3)]);
    }
}
