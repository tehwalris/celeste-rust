//! `speculate_region` - run a pure single-entry single-exit subgraph
//! unconditionally instead of behind a branch.
//!
//! # Why
//!
//! Every splitting branch left in the inlined `is_solid` chains is a
//! short-circuit gate whose *operand* is a `check`/`collide` table loop:
//!
//! ```text
//!   if_join_413:
//!     %460 = call_builtin "tile_flag_at" via %458(..)
//!     br %460 ? and_or_join_425 : and_or_continue_424   <- skip the
//!   and_or_continue_424:                                   fall_floor check
//!     ..setup..                                           when a tile hit
//!     br in_k2006_for_head_431                          <- a whole loop
//! ```
//!
//! `speculate` cannot make that `or` eager, because a loop is not an
//! instruction it can hoist. But nothing about the loop *needs* hoisting:
//! earlier stages already flattened the collide body to loads, non-creating
//! accessors, arithmetic and selects, and its trip count is per-state, so it
//! never splits. The only thing wrong with it is the conditional edge in
//! front. This rule deletes exactly that edge.
//!
//! # What it does
//!
//! The triangle, generalised to an arm that is a whole subgraph:
//!
//! ```text
//!   H:  br %c ? E : J          H:  br E
//!   E:  ..region..        =>   E:  ..region, untouched..
//!   X:  br J                   X:  br J
//!   J:  %p = phi [H: vH,       J:  %p = select %c ? vX : vH
//!               X: vX]
//! ```
//!
//! The head's branch becomes an unconditional jump into the region; the
//! region's blocks are not touched at all; the join's phis become selects on
//! the original condition. No block is added or removed, no instruction
//! moves. The region may contain arbitrary internal control flow, including
//! loops - which is the point.
//!
//! # Soundness
//!
//! Three obligations, one per thing that changes.
//!
//! **The region now runs on lanes that used to skip it.** Every instruction
//! in it must pass `is_speculatable` - so nothing in it can go wrong
//! silently, and there are no stores, allocs or creating accessors whose
//! effects would need undoing. Region-internal phis are additionally
//! allowed: single entry means their edges come from the head or from inside
//! the region, and none of those edges change. Because the region writes
//! nothing, nothing it does is visible at the join except through the phis
//! this rule rewrites - dominance already guaranteed no other block could
//! read a region-defined value.
//!
//! **The region must terminate on the states that used to skip it** -
//! otherwise the rewritten program hangs where the original returned. For
//! each cycle, the rule requires the shape the compiled `for` loops have: a
//! counter phi in the loop header, incremented by exactly 1 through the
//! latch, compared `<=`/`<` against a bound defined outside the cycle, with
//! the true side continuing the loop. That shape terminates provided the
//! counter can never wrap before exceeding the bound, and rather than argue
//! about Pico-8 overflow, the rule states the premise as a runtime guard
//! planted after the bound's definition: `bound < 32767`, checked loudly by
//! `assert_true`. (`i <= bound < 32767` implies `i + 1 < 32768`, which a
//! 16.16 fixed-point add cannot wrap.)
//!
//! **The join's phis become selects.** The region-side value dominates the
//! exit block, which after the rewrite dominates the join; the head-side
//! value and the condition dominate the head. Both operands and the
//! condition are therefore in scope, and per lane the select picks exactly
//! the value the phi would have carried. The join must have exactly the head
//! and the exit as predecessors - a join with other in-edges would need
//! phi surgery this rule does not do.
//!
//! The select is the usual remaining bargain: on states where `%c` is mixed
//! it refuses operands of different representations loudly rather than
//! widening them.
//!
//! # What this rule refuses, deliberately
//!
//! Anything with a store, call, alloc or creating accessor in the region;
//! a region reachable from anywhere but the head; more than one edge out;
//! a conditional exit; `hint_normalize` blocks; `return` inside the region;
//! loops without the counter shape. Each refusal is a shape the five
//! `is_solid` chains do not have, and a rewrite we could not check.

use anyhow::{anyhow, Result};
use rustc_hash::FxHashSet;

use crate::ir::{
    BinaryOp, FunDef, Instruction, Label, LocalId, SlotMap, Terminator,
};
use crate::pico8_num::Pico8Num;

use super::super::print::format_instruction;
use super::super::program::Program;
use super::if_convert::is_speculatable;
use super::{predecessors, require, LocalIdAllocator};

/// The bound every speculated loop is guarded against: a counter that stays
/// `<= bound < 32767` can be incremented by 1 without wrapping.
const BOUND_LIMIT: i16 = 32767;

/// One phi at the join, and the values it carries on the two edges.
struct JoinPhi {
    id: LocalId,
    head_value: LocalId,
    exit_value: LocalId,
}

/// One cycle in the region, reduced to the fact the guard needs: the bound
/// local and where it is defined.
struct LoopBound {
    bound: LocalId,
    /// `None` is the entry block.
    def_block: Option<Label>,
    def_index: usize,
}

/// The shape this rule accepts, re-derived identically by `apply`, `verify`
/// and `candidates`.
struct Site {
    condition: LocalId,
    arm_is_true: bool,
    join: Label,
    join_phis: Vec<JoinPhi>,
    /// One entry per distinct bound local, ordered by (block, index).
    bounds: Vec<LoopBound>,
}

fn defining_site(fun: &FunDef, id: LocalId) -> Option<(Option<Label>, usize)> {
    for (index, (candidate, _)) in fun.cfg.entry.instructions.iter().enumerate() {
        if *candidate == id {
            return Some((None, index));
        }
    }
    for (label, block) in &fun.cfg.named {
        for (index, (candidate, _)) in block.instructions.iter().enumerate() {
            if *candidate == id {
                return Some((Some(label.clone()), index));
            }
        }
    }
    None
}

fn defining_instruction<'a>(fun: &'a FunDef, id: LocalId) -> Option<&'a Instruction> {
    let (key, index) = defining_site(fun, id)?;
    let block = match &key {
        None => &fun.cfg.entry,
        Some(l) => fun.cfg.named.get(l)?,
    };
    Some(&block.instructions[index].1)
}

/// The region grown from `arm`: every block reachable without entering
/// `join`. Errors if it reaches the head, a `return`, or a `hint_normalize`
/// block, and returns the blocks in discovery order plus the sources of
/// edges to the join.
fn flood_fill(
    fun: &FunDef,
    head: &Label,
    arm: &Label,
    join: &Label,
) -> Result<(Vec<Label>, Vec<Label>)> {
    let mut region: Vec<Label> = Vec::new();
    let mut seen: FxHashSet<Label> = FxHashSet::default();
    let mut exit_sources: Vec<Label> = Vec::new();
    let mut stack: Vec<Label> = vec![arm.clone()];
    while let Some(label) = stack.pop() {
        if !seen.insert(label.clone()) {
            continue;
        }
        require(
            &label != head,
            format!("the region reaches back to the head '{}'", head.as_str()),
        )?;
        let block = fun
            .cfg
            .named
            .get(&label)
            .ok_or_else(|| anyhow!("region block '{}' does not exist", label.as_str()))?;
        require(
            !block.hint_normalize,
            format!("region block '{}' is a hint_normalize block", label.as_str()),
        )?;
        match block.terminator_kind() {
            Terminator::Return { .. } => {
                return Err(anyhow!(
                    "region block '{}' returns instead of reaching the join",
                    label.as_str()
                ))
            }
            terminator => {
                for successor in terminator.successor_labels() {
                    if successor == join {
                        exit_sources.push(label.clone());
                    } else {
                        stack.push(successor.clone());
                    }
                }
            }
        }
        region.push(label);
    }
    exit_sources.sort();
    exit_sources.dedup();
    Ok((region, exit_sources))
}

/// Back-edges of the region: DFS from `arm` (staying inside the region),
/// reporting edges to a block currently on the DFS stack.
fn back_edges(fun: &FunDef, region: &[Label], arm: &Label) -> Vec<(Label, Label)> {
    let in_region: FxHashSet<&Label> = region.iter().collect();
    let mut edges: Vec<(Label, Label)> = Vec::new();
    let mut finished: FxHashSet<Label> = FxHashSet::default();
    let mut on_stack: FxHashSet<Label> = FxHashSet::default();
    // (label, next successor index) - an explicit stack so `on_stack` is exact.
    let mut stack: Vec<(Label, usize)> = vec![(arm.clone(), 0)];
    on_stack.insert(arm.clone());
    while let Some((label, cursor)) = stack.pop() {
        let successors: Vec<Label> = fun.cfg.named[&label]
            .terminator_kind()
            .successor_labels()
            .into_iter()
            .filter(|s| in_region.contains(s))
            .cloned()
            .collect();
        if cursor < successors.len() {
            stack.push((label.clone(), cursor + 1));
            let next = successors[cursor].clone();
            if on_stack.contains(&next) {
                edges.push((label, next));
            } else if !finished.contains(&next) {
                on_stack.insert(next.clone());
                stack.push((next, 0));
            }
        } else {
            on_stack.remove(&label);
            finished.insert(label.clone());
        }
    }
    edges.sort();
    edges.dedup();
    edges
}

/// The blocks of the natural loop of a back-edge: the header plus everything
/// that reaches the latch without passing through the header.
fn natural_loop(
    fun: &FunDef,
    region: &[Label],
    header: &Label,
    latch: &Label,
) -> FxHashSet<Label> {
    let in_region: FxHashSet<&Label> = region.iter().collect();
    let preds = predecessors(&fun.cfg);
    let mut body: FxHashSet<Label> = FxHashSet::default();
    body.insert(header.clone());
    let mut stack = vec![latch.clone()];
    while let Some(label) = stack.pop() {
        if !body.insert(label.clone()) {
            continue;
        }
        for pred in preds.get(&Some(label)).into_iter().flatten() {
            if let Some(pred) = pred {
                if in_region.contains(pred) {
                    stack.push(pred.clone());
                }
            }
        }
    }
    body
}

/// The termination shape of one back-edge, or why it does not have it.
fn loop_bound(
    fun: &FunDef,
    region: &[Label],
    latch: &Label,
    header: &Label,
) -> Result<LocalId> {
    let header_block = &fun.cfg.named[header];
    let Terminator::ConditionalBranch { condition, true_target, false_target } =
        header_block.terminator_kind()
    else {
        return Err(anyhow!(
            "loop header '{}' does not end in a conditional branch",
            header.as_str()
        ));
    };
    let body = natural_loop(fun, region, header, latch);
    require(
        body.contains(true_target) && !body.contains(false_target),
        format!(
            "loop header '{}' must continue on the true side and leave the \
             loop on the false side",
            header.as_str()
        ),
    )?;
    // The condition: `counter <= bound` or `counter < bound`, in the header.
    let compare = header_block
        .instructions
        .iter()
        .find(|(id, _)| id == condition)
        .map(|(_, instr)| instr);
    let Some(Instruction::BinaryOp {
        left: counter,
        op: BinaryOp::LessThanEqual | BinaryOp::LessThan,
        right: bound,
    }) = compare
    else {
        return Err(anyhow!(
            "loop header '{}' does not branch on a `counter <= bound` \
             comparison defined in the header",
            header.as_str()
        ));
    };
    // The counter: a phi in the header whose latch edge is `counter + 1`.
    let counter_phi = header_block
        .instructions
        .iter()
        .find(|(id, _)| id == counter)
        .map(|(_, instr)| instr);
    let Some(Instruction::Phi { branches }) = counter_phi else {
        return Err(anyhow!(
            "the counter %{} of loop header '{}' is not a phi in the header",
            usize::from(*counter),
            header.as_str()
        ));
    };
    require(
        branches.len() == 2,
        format!(
            "the counter phi of loop header '{}' must have exactly two edges",
            header.as_str()
        ),
    )?;
    let next = branches
        .iter()
        .find(|(label, _)| label == latch)
        .map(|(_, id)| *id)
        .ok_or_else(|| {
            anyhow!(
                "the counter phi of loop header '{}' has no edge from the \
                 latch '{}'",
                header.as_str(),
                latch.as_str()
            )
        })?;
    let Some(Instruction::BinaryOp { left, op: BinaryOp::Plus, right: step }) =
        defining_instruction(fun, next)
    else {
        return Err(anyhow!(
            "the latch value %{} of loop header '{}' is not `counter + step`",
            usize::from(next),
            header.as_str()
        ));
    };
    require(
        left == counter,
        format!(
            "the increment of loop header '{}' does not add to the counter",
            header.as_str()
        ),
    )?;
    let step_is_one = matches!(
        defining_instruction(fun, *step),
        Some(Instruction::NumberConstant { value }) if *value == Pico8Num::from_i16(1)
    );
    require(
        step_is_one,
        format!(
            "the increment of loop header '{}' is not the constant 1",
            header.as_str()
        ),
    )?;
    // The bound: loop-invariant because it is defined outside the loop body.
    let (bound_block, _) = defining_site(fun, *bound).ok_or_else(|| {
        anyhow!(
            "the bound %{} of loop header '{}' has no defining instruction \
             (an argument or capture cannot be guarded here)",
            usize::from(*bound),
            header.as_str()
        )
    })?;
    let bound_in_body = match &bound_block {
        None => false,
        Some(label) => body.contains(label),
    };
    require(
        !bound_in_body,
        format!(
            "the bound %{} of loop header '{}' is defined inside the loop",
            usize::from(*bound),
            header.as_str()
        ),
    )?;
    Ok(*bound)
}

fn site(fun: &FunDef, function: &str, head: &Label, arm: &Label) -> Result<Site> {
    let head_block = fun.cfg.named.get(head).ok_or_else(|| {
        anyhow!("no block named '{}' in {}", head.as_str(), function)
    })?;
    let Terminator::ConditionalBranch { condition, true_target, false_target } =
        head_block.terminator_kind()
    else {
        return Err(anyhow!(
            "'{}' does not end in a conditional branch",
            head.as_str()
        ));
    };
    let arm_is_true = match (true_target == arm, false_target == arm) {
        (true, false) => true,
        (false, true) => false,
        _ => {
            return Err(anyhow!(
                "the branch of '{}' must name the arm '{}' exactly once",
                head.as_str(),
                arm.as_str()
            ))
        }
    };
    let join = if arm_is_true { false_target } else { true_target }.clone();
    require(
        &join != head && arm != head,
        format!("'{}' branches to itself", head.as_str()),
    )?;

    let (region, exit_sources) = flood_fill(fun, head, arm, &join)?;
    let [exit] = exit_sources.as_slice() else {
        return Err(anyhow!(
            "the region must leave through exactly one block, found {:?}",
            exit_sources.iter().map(|l| l.as_str()).collect::<Vec<_>>()
        ));
    };
    let exit_terminator = fun.cfg.named[exit].terminator_kind();
    require(
        matches!(exit_terminator, Terminator::UnconditionalBranch { target } if target == &join),
        format!(
            "the exit '{}' must branch unconditionally to the join",
            exit.as_str()
        ),
    )?;

    // Single entry: nothing outside the region reaches it, except the head's
    // edge into the arm.
    let preds = predecessors(&fun.cfg);
    let in_region: FxHashSet<&Label> = region.iter().collect();
    for label in &region {
        for pred in preds.get(&Some(label.clone())).into_iter().flatten() {
            let allowed = match pred {
                Some(pred_label) => {
                    in_region.contains(pred_label) || (label == arm && pred_label == head)
                }
                None => false,
            };
            require(
                allowed,
                format!(
                    "region block '{}' is reachable from outside the region",
                    label.as_str()
                ),
            )?;
        }
    }

    // Purity: everything in the region must be safe to run on lanes that
    // used to skip it. Phis are allowed on top of `is_speculatable`: they
    // only merge region-internal paths (single entry), and no edge changes.
    for label in &region {
        for (id, instr) in &fun.cfg.named[label].instructions {
            require(
                matches!(instr, Instruction::Phi { .. }) || is_speculatable(instr),
                format!(
                    "%{} in region block '{}' is not speculatable: {}",
                    usize::from(*id),
                    label.as_str(),
                    format_instruction(instr)
                ),
            )?;
        }
    }

    // Termination: every cycle must have the guarded counter shape.
    let mut bounds: Vec<LoopBound> = Vec::new();
    for (latch, header) in back_edges(fun, &region, arm) {
        let bound = loop_bound(fun, &region, &latch, &header)?;
        if bounds.iter().any(|b| b.bound == bound) {
            continue;
        }
        let (def_block, def_index) = defining_site(fun, bound).unwrap();
        bounds.push(LoopBound { bound, def_block, def_index });
    }
    bounds.sort_by_key(|b| (b.def_block.clone(), b.def_index));

    // The join: exactly the head and the exit as predecessors, and every phi
    // carries exactly those two edges.
    let join_preds = preds.get(&Some(join.clone())).cloned().unwrap_or_default();
    let mut sorted_preds: Vec<&Option<Label>> = join_preds.iter().collect();
    sorted_preds.sort();
    sorted_preds.dedup();
    let expected: Vec<Option<Label>> = {
        let mut v = vec![Some(head.clone()), Some(exit.clone())];
        v.sort();
        v
    };
    require(
        sorted_preds.len() == 2
            && sorted_preds[0] == &expected[0]
            && sorted_preds[1] == &expected[1]
            && join_preds.len() == 2,
        format!(
            "the join '{}' must have exactly the head and the exit as \
             predecessors",
            join.as_str()
        ),
    )?;
    let mut join_phis: Vec<JoinPhi> = Vec::new();
    for (id, instr) in &fun.cfg.named[&join].instructions {
        let Instruction::Phi { branches } = instr else { continue };
        let of = |label: &Label| {
            branches
                .iter()
                .find(|(l, _)| l == label)
                .map(|(_, v)| *v)
                .ok_or_else(|| {
                    anyhow!(
                        "phi %{} at '{}' has no edge from '{}'",
                        usize::from(*id),
                        join.as_str(),
                        label.as_str()
                    )
                })
        };
        require(
            branches.len() == 2,
            format!(
                "phi %{} at '{}' must have exactly two edges",
                usize::from(*id),
                join.as_str()
            ),
        )?;
        join_phis.push(JoinPhi {
            id: *id,
            head_value: of(head)?,
            exit_value: of(exit)?,
        });
    }

    Ok(Site {
        condition: *condition,
        arm_is_true,
        join,
        join_phis,
        bounds,
    })
}

/// The select a join phi becomes: the region-side value on the edge the arm
/// owns, the head-side value on the other.
fn select_for(site: &Site, phi: &JoinPhi) -> Instruction {
    if site.arm_is_true {
        Instruction::Select {
            condition: site.condition,
            if_true: phi.exit_value,
            if_false: phi.head_value,
        }
    } else {
        Instruction::Select {
            condition: site.condition,
            if_true: phi.head_value,
            if_false: phi.exit_value,
        }
    }
}

/// The three-instruction guard for one bound, with the given minted ids.
fn guard_for(bound: LocalId, minted: &[LocalId; 3]) -> Vec<(LocalId, Instruction)> {
    vec![
        (
            minted[0],
            Instruction::NumberConstant { value: Pico8Num::from_i16(BOUND_LIMIT) },
        ),
        (
            minted[1],
            Instruction::BinaryOp {
                left: bound,
                op: BinaryOp::LessThan,
                right: minted[0],
            },
        ),
        (minted[2], Instruction::AssertTrue { value: minted[1] }),
    ]
}

pub fn apply(program: &mut Program, function: &str, head: &str, arm: &str) -> Result<usize> {
    let head = &Label::from(head.to_string());
    let arm = &Label::from(arm.to_string());
    let fun = program.get(function)?;
    let s = site(fun, function, head, arm)?;
    let mut alloc = LocalIdAllocator::for_function(fun);
    let minted: Vec<[LocalId; 3]> = s
        .bounds
        .iter()
        .map(|_| [alloc.fresh(), alloc.fresh(), alloc.fresh()])
        .collect();

    let fun = program.get_mut(function)?;

    // The head's branch becomes an unconditional jump into the region.
    let head_block = fun.cfg.named.get_mut(head).unwrap();
    head_block.terminator.1 = Terminator::UnconditionalBranch { target: arm.clone() };

    // The join's phis become selects.
    let join_block = fun.cfg.named.get_mut(&s.join).unwrap();
    for phi in &s.join_phis {
        let slot = join_block
            .instructions
            .iter_mut()
            .find(|(id, _)| id == &phi.id)
            .unwrap();
        slot.1 = select_for(&s, phi);
    }

    // The termination guards, each right after its bound's definition.
    // Descending index order so earlier insertions do not shift later ones.
    let mut ordered: Vec<(usize, &LoopBound)> = s.bounds.iter().enumerate().collect();
    ordered.sort_by_key(|(_, b)| (b.def_block.clone(), std::cmp::Reverse(b.def_index)));
    for (minted_index, bound) in ordered {
        let block = match &bound.def_block {
            None => &mut fun.cfg.entry,
            Some(label) => fun.cfg.named.get_mut(label).unwrap(),
        };
        let guard = guard_for(bound.bound, &minted[minted_index]);
        block
            .instructions
            .splice(bound.def_index + 1..bound.def_index + 1, guard);
    }

    // New instruction ids exist, so the slot allocation is stale. Re-run
    // `allocate_slots` afterwards.
    fun.cfg.slots = std::sync::Arc::new(SlotMap::identity());

    Ok(1 + s.join_phis.len() + 3 * s.bounds.len())
}

/// Independent check: re-derives the site from the before program, then
/// requires the after program to be exactly the before program with the
/// head's branch removed, the join's phis replaced, and the guards planted -
/// and nothing else.
pub fn verify(
    before: &Program,
    after: &Program,
    function: &str,
    head: &str,
    arm: &str,
) -> Result<()> {
    let head = &Label::from(head.to_string());
    let arm = &Label::from(arm.to_string());
    let before_fun = before.get(function)?;
    let s = site(before_fun, function, head, arm)?;
    let after_fun = after.get(function)?;

    require(
        before.functions.len() == after.functions.len(),
        "speculate_region changed the set of functions",
    )?;
    for (other, before_other) in &before.functions {
        if other.as_str() == function {
            continue;
        }
        require(
            after.functions.get(other) == Some(before_other),
            format!(
                "speculate_region on {} also changed {}",
                function,
                other.as_str()
            ),
        )?;
    }
    require(
        before_fun.cfg.named.len() == after_fun.cfg.named.len(),
        "speculate_region changed the set of blocks",
    )?;
    require(
        before_fun.arg_ids == after_fun.arg_ids
            && before_fun.capture_ids == after_fun.capture_ids,
        "speculate_region changed the function signature",
    )?;

    // Every id in the before program, to check minted ids for freshness.
    let mut before_ids: FxHashSet<LocalId> = FxHashSet::default();
    before_ids.extend(before_fun.arg_ids.iter().flatten().copied());
    before_ids.extend(before_fun.capture_ids.iter().copied());
    for block in before_fun.cfg.iter_blocks() {
        before_ids.extend(block.instructions.iter().map(|(id, _)| *id));
        before_ids.insert(block.terminator_id());
    }
    let mut minted_seen: FxHashSet<LocalId> = FxHashSet::default();

    // Guards by block, ascending index, for splice reconstruction.
    let mut guards_of: std::collections::BTreeMap<Option<String>, Vec<&LoopBound>> =
        Default::default();
    for bound in &s.bounds {
        guards_of
            .entry(bound.def_block.as_ref().map(|l| l.as_str().to_string()))
            .or_default()
            .push(bound);
    }
    for list in guards_of.values_mut() {
        list.sort_by_key(|b| b.def_index);
    }

    let keys: Vec<Option<Label>> = std::iter::once(None)
        .chain(before_fun.cfg.named.keys().cloned().map(Some))
        .collect();
    for key in keys {
        let (label_str, before_block) = match &key {
            None => (None, &before_fun.cfg.entry),
            Some(l) => (
                Some(l.as_str().to_string()),
                before_fun.cfg.named.get(l).unwrap(),
            ),
        };
        let after_block = match &key {
            None => Some(&after_fun.cfg.entry),
            Some(l) => after_fun.cfg.named.get(l),
        }
        .ok_or_else(|| {
            anyhow!(
                "speculate_region removed block '{}'",
                label_str.as_deref().unwrap_or("__entry")
            )
        })?;
        let name = label_str.as_deref().unwrap_or("__entry");

        // First learn the minted ids: each guard occupies three known
        // positions in the after block (its bound's index, shifted by the
        // guards spliced in before it). Freshness and distinctness are
        // checked here; the shapes are checked by the exact comparison
        // below, built from the learned ids.
        let empty: Vec<&LoopBound> = Vec::new();
        let guards = guards_of
            .get(&label_str.clone().filter(|_| key.is_some()))
            .unwrap_or(&empty);
        let mut minted_of: Vec<[LocalId; 3]> = Vec::new();
        for (guard_number, bound) in guards.iter().enumerate() {
            let position = bound.def_index + 1 + 3 * guard_number;
            let mut minted = [LocalId::from(0); 3];
            for offset in 0..3 {
                let (after_id, _) = after_block
                    .instructions
                    .get(position + offset)
                    .ok_or_else(|| {
                        anyhow!("'{}' is too short to hold its guard", name)
                    })?;
                require(
                    !before_ids.contains(after_id),
                    format!(
                        "minted id %{} in '{}' already existed before",
                        usize::from(*after_id),
                        name
                    ),
                )?;
                require(
                    minted_seen.insert(*after_id),
                    format!(
                        "minted id %{} in '{}' is used twice",
                        usize::from(*after_id),
                        name
                    ),
                )?;
                minted[offset] = *after_id;
            }
            minted_of.push(minted);
        }

        // Expected instructions: the before block, with join phis replaced
        // and the guards (now with their learned ids) spliced in.
        let mut want: Vec<(LocalId, Instruction)> = before_block
            .instructions
            .iter()
            .map(|(id, instr)| {
                let instr = if key.as_ref() == Some(&s.join) {
                    match s.join_phis.iter().find(|p| &p.id == id) {
                        Some(phi) => select_for(&s, phi),
                        None => instr.clone(),
                    }
                } else {
                    instr.clone()
                };
                (*id, instr)
            })
            .collect();
        for (bound, minted) in guards.iter().zip(minted_of.iter()).rev() {
            want.splice(
                bound.def_index + 1..bound.def_index + 1,
                guard_for(bound.bound, minted),
            );
        }

        require(
            want.len() == after_block.instructions.len(),
            format!(
                "speculate_region changed the length of '{}': want {}, got {}",
                name,
                want.len(),
                after_block.instructions.len()
            ),
        )?;
        for (position, ((want_id, want_instr), (after_id, after_instr))) in
            want.iter().zip(after_block.instructions.iter()).enumerate()
        {
            require(
                want_id == after_id && want_instr == after_instr,
                format!(
                    "speculate_region changed %{} at position {} of '{}':\n  want {}\n  got  {}",
                    usize::from(*after_id),
                    position,
                    name,
                    format_instruction(want_instr),
                    format_instruction(after_instr)
                ),
            )?;
        }

        // Terminators.
        require(
            before_block.terminator_id() == after_block.terminator_id(),
            format!("speculate_region changed the terminator id of '{}'", name),
        )?;
        let want_terminator = if key.as_ref() == Some(head) {
            Terminator::UnconditionalBranch { target: arm.clone() }
        } else {
            before_block.terminator_kind().clone()
        };
        require(
            &want_terminator == after_block.terminator_kind(),
            format!(
                "speculate_region changed the terminator of '{}'",
                name
            ),
        )?;
        require(
            before_block.hint_normalize == after_block.hint_normalize,
            format!("speculate_region changed hint_normalize of '{}'", name),
        )?;
    }
    Ok(())
}

/// Heads whose branch skips a region this rule could run unconditionally.
pub fn candidates(program: &Program) -> Vec<(String, Label, Label)> {
    let mut out = Vec::new();
    for (function, fun) in &program.functions {
        for (label, block) in &fun.cfg.named {
            let Terminator::ConditionalBranch { true_target, false_target, .. } =
                block.terminator_kind()
            else {
                continue;
            };
            if true_target == false_target {
                continue;
            }
            for arm in [true_target, false_target] {
                if site(fun, function.as_str(), label, arm).is_ok() {
                    out.push((
                        function.as_str().to_string(),
                        label.clone(),
                        arm.clone(),
                    ));
                }
            }
        }
    }
    out.sort();
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, GlobalId};
    use crate::rewrite::print::format_function;
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(name: &str) -> Label {
        Label::from(name.to_string())
    }

    fn block(
        instructions: Vec<(LocalId, Instruction)>,
        terminator_id: usize,
        terminator: Terminator,
    ) -> Block {
        Block {
            instructions,
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

    /// entry -> h; h: br %10 ? setup : join.
    /// setup: init %11, bound %12, step %20 -> loop_head.
    /// loop_head: %13 = phi(setup: %11, loop_body: %16); %14 = %13 <= %12;
    ///            br %14 ? loop_body : cont.
    /// loop_body: %16 = %13 + %20 -> loop_head.
    /// cont: %17 = nil -> join.
    /// join: %18 = phi(h: %19, cont: %17); return %18.
    fn region_program() -> Program {
        let entry = block(vec![], 900, br("h"));
        let h = block(
            vec![
                (id(10), Instruction::BoolConstant { value: true }),
                (id(19), Instruction::NilConstant),
            ],
            901,
            br_if(10, "setup", "join"),
        );
        let setup = block(
            vec![(id(11), num(1)), (id(12), num(3)), (id(20), num(1))],
            902,
            br("loop_head"),
        );
        let loop_head = block(
            vec![
                (
                    id(13),
                    Instruction::Phi {
                        branches: vec![
                            (label("setup"), id(11)),
                            (label("loop_body"), id(16)),
                        ],
                    },
                ),
                (
                    id(14),
                    Instruction::BinaryOp {
                        left: id(13),
                        op: BinaryOp::LessThanEqual,
                        right: id(12),
                    },
                ),
            ],
            903,
            br_if(14, "loop_body", "cont"),
        );
        let loop_body = block(
            vec![(
                id(16),
                Instruction::BinaryOp { left: id(13), op: BinaryOp::Plus, right: id(20) },
            )],
            904,
            br("loop_head"),
        );
        let cont = block(vec![(id(17), Instruction::NilConstant)], 905, br("join"));
        let join = block(
            vec![(
                id(18),
                Instruction::Phi {
                    branches: vec![(label("h"), id(19)), (label("cont"), id(17))],
                },
            )],
            906,
            Terminator::Return { value: Some(id(18)) },
        );

        let mut named = crate::ir::new_label_map();
        named.insert(label("h"), h);
        named.insert(label("setup"), setup);
        named.insert(label("loop_head"), loop_head);
        named.insert(label("loop_body"), loop_body);
        named.insert(label("cont"), cont);
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
        Program { functions }
    }

    #[test]
    fn speculates_a_pure_loop_region() {
        let mut p = region_program();
        let before = p.clone();
        let changed = apply(&mut p, "f", "h", "setup").unwrap();
        assert_eq!(changed, 1 + 1 + 3);
        verify(&before, &p, "f", "h", "setup").unwrap();
        let text = format_function(p.get("f").unwrap());
        assert!(text.contains("br setup"), "{}", text);
        assert!(!text.contains("br %10"), "{}", text);
        assert!(
            text.contains("select %10 ? %17 : %19"),
            "the join phi must become a select with the region value on the true side: {}",
            text
        );
        assert!(text.contains("assert_true"), "{}", text);
        // The guard sits right after the bound's definition: between the
        // bound constant and the step constant that follows it in `setup`.
        let bound_at = text.find("num Pico8Num(\"0x30000\")").unwrap();
        let guard_at = text.find("assert_true").unwrap();
        let step_at = text.find("%20 = num").unwrap();
        assert!(bound_at < guard_at && guard_at < step_at, "{}", text);
        // The region itself is untouched.
        assert!(text.contains("phi [setup: %11, loop_body: %16]"), "{}", text);
    }

    #[test]
    fn speculates_an_arm_on_the_false_side() {
        let mut p = region_program();
        {
            let fun = p.get_mut("f").unwrap();
            let h = fun.cfg.named.get_mut(&label("h")).unwrap();
            h.terminator.1 = br_if(10, "join", "setup");
        }
        let before = p.clone();
        apply(&mut p, "f", "h", "setup").unwrap();
        verify(&before, &p, "f", "h", "setup").unwrap();
        let text = format_function(p.get("f").unwrap());
        assert!(
            text.contains("select %10 ? %19 : %17"),
            "the head value must sit on the true side: {}",
            text
        );
    }

    #[test]
    fn refuses_an_impure_region() {
        let mut p = region_program();
        {
            let fun = p.get_mut("f").unwrap();
            let body = fun.cfg.named.get_mut(&label("loop_body")).unwrap();
            body.instructions.push((
                id(30),
                Instruction::Store { target: id(13), source: id(16) },
            ));
        }
        let error = apply(&mut p, "f", "h", "setup")
            .unwrap_err()
            .to_string();
        assert!(error.contains("not speculatable"), "{}", error);
    }

    #[test]
    fn refuses_a_region_with_a_second_entry() {
        let mut p = region_program();
        {
            let fun = p.get_mut("f").unwrap();
            // A block outside the region that jumps into its exit block.
            // Unreachable from the entry, but predecessors are structural.
            fun.cfg
                .named
                .insert(label("side"), block(vec![], 907, br("cont")));
        }
        let error = apply(&mut p, "f", "h", "setup")
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("reachable from outside the region"),
            "{}",
            error
        );
    }

    #[test]
    fn refuses_a_loop_without_the_counter_shape() {
        let mut p = region_program();
        {
            let fun = p.get_mut("f").unwrap();
            // Make the increment subtract instead of add.
            let body = fun.cfg.named.get_mut(&label("loop_body")).unwrap();
            body.instructions[0].1 =
                Instruction::BinaryOp { left: id(13), op: BinaryOp::Minus, right: id(20) };
        }
        let error = apply(&mut p, "f", "h", "setup")
            .unwrap_err()
            .to_string();
        assert!(error.contains("counter + step"), "{}", error);
    }

    #[test]
    fn refuses_a_bound_defined_inside_the_loop() {
        let mut p = region_program();
        {
            let fun = p.get_mut("f").unwrap();
            // Move the bound's definition into the loop body.
            let setup = fun.cfg.named.get_mut(&label("setup")).unwrap();
            let bound = setup.instructions.remove(1);
            let body = fun.cfg.named.get_mut(&label("loop_body")).unwrap();
            body.instructions.insert(0, bound);
        }
        let error = apply(&mut p, "f", "h", "setup")
            .unwrap_err()
            .to_string();
        assert!(error.contains("defined inside the loop"), "{}", error);
    }

    #[test]
    fn refuses_a_conditional_exit() {
        let mut p = region_program();
        {
            let fun = p.get_mut("f").unwrap();
            let cont = fun.cfg.named.get_mut(&label("cont")).unwrap();
            cont.instructions
                .push((id(32), Instruction::BoolConstant { value: true }));
            cont.terminator.1 = br_if(32, "join", "loop_head");
        }
        let error = apply(&mut p, "f", "h", "setup")
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("must branch unconditionally to the join"),
            "{}",
            error
        );
    }

    #[test]
    fn refuses_a_join_with_extra_predecessors() {
        let mut p = region_program();
        {
            let fun = p.get_mut("f").unwrap();
            // A third block that also jumps to the join. Reached nowhere, but
            // predecessors are computed structurally.
            fun.cfg
                .named
                .insert(label("stray"), block(vec![], 908, br("join")));
        }
        let error = apply(&mut p, "f", "h", "setup")
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("exactly the head and the exit"),
            "{}",
            error
        );
    }

    #[test]
    fn verify_rejects_a_kept_branch() {
        let mut p = region_program();
        let before = p.clone();
        apply(&mut p, "f", "h", "setup").unwrap();
        // Tamper: restore the branch but keep the selects and guards.
        {
            let fun = p.get_mut("f").unwrap();
            let h = fun.cfg.named.get_mut(&label("h")).unwrap();
            h.terminator.1 = br_if(10, "setup", "join");
        }
        let error = verify(&before, &p, "f", "h", "setup")
            .unwrap_err()
            .to_string();
        assert!(error.contains("terminator of 'h'"), "{}", error);
    }

    #[test]
    fn verify_rejects_a_swapped_select() {
        let mut p = region_program();
        let before = p.clone();
        apply(&mut p, "f", "h", "setup").unwrap();
        {
            let fun = p.get_mut("f").unwrap();
            let join = fun.cfg.named.get_mut(&label("join")).unwrap();
            join.instructions[0].1 = Instruction::Select {
                condition: id(10),
                if_true: id(19),
                if_false: id(17),
            };
        }
        let error = verify(&before, &p, "f", "h", "setup")
            .unwrap_err()
            .to_string();
        assert!(error.contains("changed %18"), "{}", error);
    }

    #[test]
    fn verify_rejects_a_missing_guard() {
        let mut p = region_program();
        let before = p.clone();
        apply(&mut p, "f", "h", "setup").unwrap();
        {
            let fun = p.get_mut("f").unwrap();
            let setup = fun.cfg.named.get_mut(&label("setup")).unwrap();
            // Drop the assert, keeping the limit and compare.
            setup.instructions.retain(
                |(_, instr)| !matches!(instr, Instruction::AssertTrue { .. }),
            );
        }
        // The guard-position scan now reads pre-existing instructions where
        // the minted triple should be, which fails the freshness check - a
        // loud rejection either way.
        let error = verify(&before, &p, "f", "h", "setup")
            .unwrap_err()
            .to_string();
        assert!(error.contains("'setup'"), "{}", error);
    }

    #[test]
    fn candidates_finds_the_head() {
        let p = region_program();
        let found = candidates(&p);
        assert_eq!(
            found,
            vec![("f".to_string(), label("h"), label("setup"))]
        );
    }
}
