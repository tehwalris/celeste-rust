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
//! # Diamonds: serializing two regions
//!
//! With an explicit `join` in the recipe entry, the head may branch to *two*
//! regions that meet at that join - the shape the `or`-shortcircuit's
//! "skip to `true`" arm leaves behind once everything around it is eager:
//!
//! ```text
//!   H:  br %c ? A : B          H:  br A
//!   A:  ..region..        =>   A:  ..region, untouched..
//!   Xa: br J                   Xa: br B                <- serialized
//!   B:  ..region..             B:  ..region, untouched..
//!   Xb: br J                   Xb: br J
//!   J:  %p = phi [Xa: va,      J:  %p = select %c ? va : vb
//!               Xb: vb]
//! ```
//!
//! Both regions run, in sequence - the named arm first - and the join picks
//! per lane. Every obligation applies to each region separately (purity,
//! single entry, one exit, termination), the regions must be disjoint, and
//! `B`'s entry block must have no phis: its predecessor changes from the
//! head to `Xa`, which would dangle a phi edge. Pure regions commute, so
//! the order is only a convention.
//!
//! # Masked stores: regions that write
//!
//! With `"mask": true` in the recipe entry, the region may contain plain
//! `store` instructions. Each becomes the load-adjacent masked form that
//! `absorb_stores` and `mask_loop` already use, on the head's condition:
//!
//! ```text
//!   %s = store %t <- %v          %g = assert_value_cell %t
//!                          =>    %o = load %t
//!                                %m = select %c ? %v : %o
//!                                %s = store %t <- %m
//! ```
//!
//! (with the operands swapped when the store's region sits on the false
//! side). The store's id and position stay; three ids are minted per store.
//!
//! Why this needs no aliasing analysis: the load is adjacent to the store,
//! so whatever the cell aliases, `%o` is its current value, and lanes whose
//! condition picks the old value write back exactly what was there. Lanes
//! that took the region see the store land unchanged.
//!
//! Why serialization order stays sound for a diamond with stores: the two
//! regions' masks are the two sides of one condition, so each store lands
//! only on its own region's lanes. On those lanes the *other* region's
//! stores all write back the old value, so every load in the live region
//! reads exactly the state the un-serialized program would have shown it -
//! the regions no longer commute, but they no longer need to.
//!
//! The remaining bargain is the select's, as with the join phis: mixed
//! representations refuse loudly. A masked region whose stores create cells
//! is still refused - `demote_create` first, so the cell provably exists or
//! fails loudly.
//!
//! # Expansion: regions that concretize
//!
//! With `"expand": true`, the region may contain `expand` and the
//! concretization store `expand_bool` pairs it with:
//!
//! ```text
//!   %c = load %p          <- the same cell: the store's accessor is the
//!   %e = expand %c           load's, same operands, with no other store
//!   %a = <accessor>          between the load and the store-back
//!   store %a <- %e
//! ```
//!
//! Neither is masked, and no mask would be possible: on a state that
//! skipped the region the cell still holds `UnknownBool`, and a select
//! mixing that with the expanded vector is exactly the representation
//! `select` refuses. None is needed either. `expand` turns each unknown
//! lane into a true/false pair that jointly stands for the same concrete
//! states, and the store writes that exhaustive split back into the very
//! cell the value was loaded from - a per-lane refinement whichever way
//! the lane would have branched. Lanes that took the region get exactly
//! what the original program computed; lanes that skipped it get their
//! button concretized early, which changes no observable value - only the
//! lane count, which is the cost the opt-in answers for at the bench.
//! (`expand_lanes` doubles every vector in the state, the head condition's
//! included, so masks computed before the expand stay lane-consistent.)
//!
//! The premise that makes the store a refinement rather than a clobber -
//! it writes the loaded cell, and the cell still holds the loaded value -
//! is checked syntactically: load, expand and store sit in one block with
//! no other store between load and store-back, and the region admits no
//! other kind of write. The store's accessor must be the load's - same
//! lookup, same operand locals - with `create_if_missing` free to differ
//! in the comparison: a missing cell loads `Nil`, which `expand` refuses
//! loudly, so on every state that reaches the store the cell provably
//! exists. (In practice both accessors are already demoted: a creating
//! accessor fails the region's purity bar on its own, as with masked
//! stores - `demote_create` first.) What `expand` itself does on lanes
//! nobody asked to split is the usual loud bargain: a non-bool refuses, a
//! concrete bool is the identity.
//!
//! # What this rule refuses, deliberately
//!
//! Anything with a call, alloc or creating accessor in the region; a store
//! without the opt-in `mask` (and `mask` without a store to justify it);
//! `expand` without the opt-in `expand` (and the flag without an expand to
//! justify it); a store of an expanded value that is not the pinned-down
//! concretization shape - masking one would only defer to the select's
//! representation refusal; a region reachable from anywhere but the head;
//! more than one edge out; a conditional exit; `hint_normalize` blocks;
//! `return` inside the region; loops without the counter shape. Each
//! refusal is a shape the converted chains do not have, and a rewrite we
//! could not check.

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
pub(super) const BOUND_LIMIT: i16 = 32767;

/// One phi at the join, and the values it carries on the two edges. For a
/// triangle, `other_value` comes in on the head's edge; for a diamond, on
/// the second region's exit edge. `exit_value` is always the named arm's.
struct JoinPhi {
    id: LocalId,
    other_value: LocalId,
    exit_value: LocalId,
}

/// Diamond mode: the second region's entry (the head's other branch target)
/// and the named arm's exit, whose terminator is rewired into it.
struct Serialize {
    other: Label,
    arm_exit: Label,
}

/// One cycle in the region, reduced to the fact the guard needs: the bound
/// local and where it is defined.
pub(super) struct LoopBound {
    bound: LocalId,
    /// `None` is the entry block.
    def_block: Option<Label>,
    def_index: usize,
}

/// One store in a masked region: where it is, what it writes, and on which
/// side of the head's condition it lands.
struct StoreSite {
    block: Label,
    index: usize,
    target: LocalId,
    value: LocalId,
    /// True when the store's region runs on the condition's true side, so
    /// the masked select picks the new value there.
    new_on_true: bool,
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
    /// Present iff this is a diamond (an explicit `join` was given).
    serialize: Option<Serialize>,
    /// The region's stores, in region discovery order; non-empty iff the
    /// entry opted in with `mask`.
    stores: Vec<StoreSite>,
}

pub(super) fn defining_site(fun: &FunDef, id: LocalId) -> Option<(Option<Label>, usize)> {
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

pub(super) fn defining_instruction<'a>(fun: &'a FunDef, id: LocalId) -> Option<&'a Instruction> {
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
pub(super) fn back_edges(fun: &FunDef, region: &[Label], arm: &Label) -> Vec<(Label, Label)> {
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
pub(super) fn loop_bound(
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
    // A bound defined inside the loop is only invariant if nothing about it
    // can vary: a small constant qualifies (a masked inner loop's uniform
    // trip count is exactly this shape), and its termination claim is
    // checked right here instead of by a runtime guard.
    require(
        !bound_in_body || is_small_constant(fun, *bound),
        format!(
            "the bound %{} of loop header '{}' is defined inside the loop \
             and is not a small constant",
            usize::from(*bound),
            header.as_str()
        ),
    )?;
    Ok(*bound)
}

/// Is this a `NumberConstant` provably below the `< 32767` termination
/// guard's threshold? Such a bound needs no runtime guard: the check the
/// guard would make is decided at rewrite time.
pub(super) fn is_small_constant(fun: &FunDef, bound: LocalId) -> bool {
    matches!(
        defining_instruction(fun, bound),
        Some(Instruction::NumberConstant { value })
            if value.as_i16().is_some_and(|v| v < 32767)
    )
}

fn site(
    fun: &FunDef,
    function: &str,
    head: &Label,
    arm: &Label,
    join_override: Option<&Label>,
    mask: bool,
    expand: bool,
) -> Result<Site> {
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
    let other = if arm_is_true { false_target } else { true_target }.clone();
    let join = match join_override {
        None => other.clone(),
        Some(j) => {
            require(
                j != &other,
                format!(
                    "'{}' is the head's other branch target already; drop the \
                     join field for a triangle",
                    j.as_str()
                ),
            )?;
            j.clone()
        }
    };
    require(
        &join != head && arm != head && &other != head,
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

    // Diamond mode: the head's other target grows its own region, with the
    // same obligations, plus two of its own: the regions are disjoint, and
    // the second entry has no phis (its predecessor changes from the head to
    // the first region's exit, which would dangle a phi edge).
    let (serialize, other_exit) = match join_override {
        None => (None, None),
        Some(_) => {
            let (region_b, exits_b) = flood_fill(fun, head, &other, &join)?;
            let [exit_b] = exits_b.as_slice() else {
                return Err(anyhow!(
                    "the second region must leave through exactly one block, \
                     found {:?}",
                    exits_b.iter().map(|l| l.as_str()).collect::<Vec<_>>()
                ));
            };
            let exit_b_terminator = fun.cfg.named[exit_b].terminator_kind();
            require(
                matches!(exit_b_terminator, Terminator::UnconditionalBranch { target } if target == &join),
                format!(
                    "the second exit '{}' must branch unconditionally to the join",
                    exit_b.as_str()
                ),
            )?;
            let in_a: FxHashSet<&Label> = region.iter().collect();
            for label in &region_b {
                require(
                    !in_a.contains(label),
                    format!(
                        "the two regions overlap at '{}'",
                        label.as_str()
                    ),
                )?;
            }
            require(
                !fun.cfg.named[&other]
                    .instructions
                    .iter()
                    .any(|(_, i)| matches!(i, Instruction::Phi { .. })),
                format!(
                    "the second region's entry '{}' has phis, whose head edge \
                     would dangle after serialization",
                    other.as_str()
                ),
            )?;
            (
                Some(Serialize { other: other.clone(), arm_exit: exit.clone() }),
                Some((region_b, exit_b.clone())),
            )
        }
    };

    // Single entry, purity and termination, per region. Phis are allowed on
    // top of `is_speculatable`: they only merge region-internal paths
    // (single entry), and no edge changes.
    let preds = predecessors(&fun.cfg);
    let regions: Vec<(&[Label], &Label, bool)> = match &other_exit {
        None => vec![(region.as_slice(), arm, true)],
        Some((region_b, _)) => {
            vec![(region.as_slice(), arm, true), (region_b.as_slice(), &other, false)]
        }
    };
    let mut bounds: Vec<LoopBound> = Vec::new();
    let mut stores: Vec<StoreSite> = Vec::new();
    let mut saw_expand = false;
    for (blocks, entry, is_arm) in &regions {
        let in_region: FxHashSet<&Label> = blocks.iter().collect();
        for label in *blocks {
            for pred in preds.get(&Some(label.clone())).into_iter().flatten() {
                let allowed = match pred {
                    Some(pred_label) => {
                        in_region.contains(pred_label)
                            || (&label == entry && pred_label == head)
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
        for label in *blocks {
            let instructions = &fun.cfg.named[label].instructions;
            for (index, (id, instr)) in instructions.iter().enumerate() {
                if matches!(instr, Instruction::Expand { .. }) {
                    require(
                        expand,
                        format!(
                            "%{} in region block '{}' is an expand; add \
                             \"expand\":true to accept doubling the lanes of \
                             states that skip the region",
                            usize::from(*id),
                            label.as_str()
                        ),
                    )?;
                    saw_expand = true;
                    continue;
                }
                if let Instruction::Store { target, source } = instr {
                    if expand
                        && concretization_store(fun, label, instructions, index, *target, *source)?
                    {
                        // Runs unmasked: writing the exhaustive lane-split
                        // back into the loaded cell refines every lane,
                        // whichever way it would have branched.
                        continue;
                    }
                    if mask {
                        stores.push(StoreSite {
                            block: label.clone(),
                            index,
                            target: *target,
                            value: *source,
                            new_on_true: *is_arm == arm_is_true,
                        });
                        continue;
                    }
                    return Err(anyhow!(
                        "%{} in region block '{}' is a store; add \"mask\":true \
                         to mask it on the head's condition",
                        usize::from(*id),
                        label.as_str()
                    ));
                }
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
        for (latch, header) in back_edges(fun, blocks, entry) {
            let bound = loop_bound(fun, blocks, &latch, &header)?;
            if is_small_constant(fun, bound) {
                continue;
            }
            if bounds.iter().any(|b| b.bound == bound) {
                continue;
            }
            let (def_block, def_index) = defining_site(fun, bound).unwrap();
            // An earlier entry may have guarded this bound already - regions
            // converted one gate at a time overlap in their loops. The guard
            // is recognised by its exact three-instruction shape, so this
            // never skips anything weaker than what it would emit.
            if already_guarded(fun, bound, &def_block, def_index) {
                continue;
            }
            bounds.push(LoopBound { bound, def_block, def_index });
        }
    }
    bounds.sort_by_key(|b| (b.def_block.clone(), b.def_index));

    // The join: exactly the two rewritten edges as predecessors, and every
    // phi carries exactly those two. For a triangle that is the head and the
    // exit; for a diamond, the two exits.
    let other_edge: Label = match &other_exit {
        None => head.clone(),
        Some((_, exit_b)) => exit_b.clone(),
    };
    let join_preds = preds.get(&Some(join.clone())).cloned().unwrap_or_default();
    let mut sorted_preds: Vec<&Option<Label>> = join_preds.iter().collect();
    sorted_preds.sort();
    sorted_preds.dedup();
    let expected: Vec<Option<Label>> = {
        let mut v = vec![Some(other_edge.clone()), Some(exit.clone())];
        v.sort();
        v
    };
    require(
        sorted_preds.len() == 2
            && sorted_preds[0] == &expected[0]
            && sorted_preds[1] == &expected[1]
            && join_preds.len() == 2,
        format!(
            "the join '{}' must have exactly '{}' and the exit '{}' as \
             predecessors",
            join.as_str(),
            other_edge.as_str(),
            exit.as_str()
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
            other_value: of(&other_edge)?,
            exit_value: of(exit)?,
        });
    }

    require(
        !mask || !stores.is_empty(),
        "the region has no stores; drop the mask field",
    )?;
    require(
        !expand || saw_expand,
        "the region has no expand; drop the expand field",
    )?;

    Ok(Site {
        condition: *condition,
        arm_is_true,
        join,
        join_phis,
        bounds,
        serialize,
        stores,
    })
}

/// Is this store the concretization store of an `expand` in its block -
/// and therefore left untouched instead of masked? `Ok(false)` means the
/// source is not an expanded value at all (the store falls through to the
/// usual masking); an expanded value in any shape but the pinned-down one
/// is a loud error, because masking it would only defer to the select's
/// mixed-representation refusal at runtime.
///
/// The accepted shape: `%source` is `expand` of a value loaded from the
/// very cell being stored, all three in this block in order, with no other
/// store between the load and the store-back. Then the cell still holds
/// the loaded value when the store runs, and writing its exhaustive
/// lane-split back is a refinement on every lane, masked or not.
fn concretization_store(
    fun: &FunDef,
    label: &Label,
    instructions: &[(LocalId, Instruction)],
    store_index: usize,
    target: LocalId,
    source: LocalId,
) -> Result<bool> {
    if !matches!(
        defining_instruction(fun, source),
        Some(Instruction::Expand { .. })
    ) {
        return Ok(false);
    }
    let fail = |what: String| {
        anyhow!(
            "the store of expanded %{} in region block '{}' is not the \
             concretization shape: {}",
            usize::from(source),
            label.as_str(),
            what
        )
    };
    let expand_index = instructions[..store_index]
        .iter()
        .position(|(id, instr)| *id == source && matches!(instr, Instruction::Expand { .. }))
        .ok_or_else(|| fail("its expand is not above it in the same block".to_string()))?;
    let Instruction::Expand { value } = &instructions[expand_index].1 else {
        unreachable!("the position predicate matched an expand");
    };
    let load_index = instructions[..expand_index]
        .iter()
        .position(|(id, _)| id == value)
        .ok_or_else(|| {
            fail(format!(
                "the expanded %{} is not defined above the expand in the same block",
                usize::from(*value)
            ))
        })?;
    let Instruction::Load { source: loaded } = &instructions[load_index].1 else {
        return Err(fail(format!(
            "the expanded %{} is not a load",
            usize::from(*value)
        )));
    };
    if let Some((other, _)) = instructions[load_index + 1..store_index]
        .iter()
        .find(|(_, instr)| matches!(instr, Instruction::Store { .. }))
    {
        return Err(fail(format!(
            "%{} stores between the load and the store-back",
            usize::from(*other)
        )));
    }
    if !same_cell(fun, *loaded, target) {
        return Err(fail(format!(
            "the store's target %{} is not the loaded cell %{}",
            usize::from(target),
            usize::from(*loaded)
        )));
    }
    Ok(true)
}

/// Do two pointer locals provably name the same cell? Either they are one
/// local, or their defining accessors are the same lookup with the same
/// operands. `create_if_missing` is deliberately ignored: it could only
/// matter for a missing cell, and a missing cell loads `Nil`, which the
/// `expand` between the load and the store refuses loudly - so on every
/// state that reaches the store the cell exists and neither accessor
/// creates. Cell identity is otherwise stable: stores change cell values,
/// never table structure, and nothing removes a cell.
fn same_cell(fun: &FunDef, a: LocalId, b: LocalId) -> bool {
    if a == b {
        return true;
    }
    use Instruction::{GetField, GetGlobal, GetIndex};
    match (defining_instruction(fun, a), defining_instruction(fun, b)) {
        (Some(GetGlobal { name: na, .. }), Some(GetGlobal { name: nb, .. })) => na == nb,
        (
            Some(GetField { receiver: ra, field: fa, .. }),
            Some(GetField { receiver: rb, field: fb, .. }),
        ) => ra == rb && fa == fb,
        (
            Some(GetIndex { receiver: ra, index: ia, .. }),
            Some(GetIndex { receiver: rb, index: ib, .. }),
        ) => ra == rb && ia == ib,
        _ => false,
    }
}

/// The masked select one store's value goes through: the new value on the
/// side of the condition its region owns, the freshly loaded old value on
/// the other.
fn masked_select_for(site: &Site, store: &StoreSite, old: LocalId) -> Instruction {
    if store.new_on_true {
        Instruction::Select {
            condition: site.condition,
            if_true: store.value,
            if_false: old,
        }
    } else {
        Instruction::Select {
            condition: site.condition,
            if_true: old,
            if_false: store.value,
        }
    }
}

/// The select a join phi becomes: the named arm's value on the edge the arm
/// owns, the other value (head side, or second region) on the other.
fn select_for(site: &Site, phi: &JoinPhi) -> Instruction {
    if site.arm_is_true {
        Instruction::Select {
            condition: site.condition,
            if_true: phi.exit_value,
            if_false: phi.other_value,
        }
    } else {
        Instruction::Select {
            condition: site.condition,
            if_true: phi.other_value,
            if_false: phi.exit_value,
        }
    }
}

/// Does the exact guard this rule would emit already follow the bound's
/// definition? True iff the next three instructions are the limit constant,
/// the compare against this bound, and the `assert_true` - the shape
/// `guard_for` emits, on any ids.
pub(super) fn already_guarded(
    fun: &FunDef,
    bound: LocalId,
    def_block: &Option<Label>,
    def_index: usize,
) -> bool {
    let block = match def_block {
        None => &fun.cfg.entry,
        Some(label) => &fun.cfg.named[label],
    };
    let Some(window) = block.instructions.get(def_index + 1..def_index + 4) else {
        return false;
    };
    let [(limit_id, limit), (cmp_id, cmp), (_, assert)] = window else {
        return false;
    };
    matches!(
        limit,
        Instruction::NumberConstant { value } if *value == Pico8Num::from_i16(BOUND_LIMIT)
    ) && matches!(
        cmp,
        Instruction::BinaryOp { left, op: BinaryOp::LessThan, right }
            if *left == bound && right == limit_id
    ) && matches!(assert, Instruction::AssertTrue { value } if value == cmp_id)
}

/// The three-instruction guard for one bound, with the given minted ids.
pub(super) fn guard_for(bound: LocalId, minted: &[LocalId; 3]) -> Vec<(LocalId, Instruction)> {
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

pub fn apply(
    program: &mut Program,
    function: &str,
    head: &str,
    arm: &str,
    join: Option<&str>,
    mask: bool,
    expand: bool,
) -> Result<usize> {
    let head = &Label::from(head.to_string());
    let arm = &Label::from(arm.to_string());
    let join = join.map(|j| Label::from(j.to_string()));
    let fun = program.get(function)?;
    let s = site(fun, function, head, arm, join.as_ref(), mask, expand)?;
    let mut alloc = LocalIdAllocator::for_function(fun);
    let minted: Vec<[LocalId; 3]> = s
        .bounds
        .iter()
        .map(|_| [alloc.fresh(), alloc.fresh(), alloc.fresh()])
        .collect();
    let store_minted: Vec<[LocalId; 3]> = s
        .stores
        .iter()
        .map(|_| [alloc.fresh(), alloc.fresh(), alloc.fresh()])
        .collect();

    let fun = program.get_mut(function)?;

    // The head's branch becomes an unconditional jump into the region.
    let head_block = fun.cfg.named.get_mut(head).unwrap();
    head_block.terminator.1 = Terminator::UnconditionalBranch { target: arm.clone() };

    // A diamond serializes: the named arm's exit continues into the second
    // region instead of the join.
    if let Some(ser) = &s.serialize {
        let exit_block = fun.cfg.named.get_mut(&ser.arm_exit).unwrap();
        exit_block.terminator.1 =
            Terminator::UnconditionalBranch { target: ser.other.clone() };
    }

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

    // The termination guards and the masked stores, spliced per block in
    // descending position order so earlier insertions do not shift later
    // ones. A guard inserts after its bound's definition; a store's triple
    // inserts before the store, whose value is rewritten to the masked
    // select. At the same position - a guard belonging to the instruction
    // right before a store - the triple goes in first, which leaves the
    // guard before the triple in the final layout.
    let mut events: Vec<(Option<Label>, usize, u8, usize)> = Vec::new();
    for (i, bound) in s.bounds.iter().enumerate() {
        events.push((bound.def_block.clone(), bound.def_index + 1, 0, i));
    }
    for (i, store) in s.stores.iter().enumerate() {
        events.push((Some(store.block.clone()), store.index, 1, i));
    }
    events.sort_by(|a, b| {
        (a.0.as_ref().map(|l| l.as_str()), a.1, a.2)
            .cmp(&(b.0.as_ref().map(|l| l.as_str()), b.1, b.2))
            .reverse()
    });
    for (block_key, position, kind, index) in events {
        let block = match &block_key {
            None => &mut fun.cfg.entry,
            Some(label) => fun.cfg.named.get_mut(label).unwrap(),
        };
        if kind == 1 {
            let store = &s.stores[index];
            let [guard, old, sel] = store_minted[index];
            block.instructions[position].1 =
                Instruction::Store { target: store.target, source: sel };
            block.instructions.splice(
                position..position,
                [
                    (guard, Instruction::AssertValueCell { target: store.target }),
                    (old, Instruction::Load { source: store.target }),
                    (sel, masked_select_for(&s, store, old)),
                ],
            );
        } else {
            let bound = &s.bounds[index];
            let guard = guard_for(bound.bound, &minted[index]);
            block.instructions.splice(position..position, guard);
        }
    }

    // New instruction ids exist, so the slot allocation is stale. Re-run
    // `allocate_slots` afterwards.
    fun.cfg.slots = std::sync::Arc::new(SlotMap::identity());

    Ok(1
        + s.serialize.is_some() as usize
        + s.join_phis.len()
        + 3 * s.bounds.len()
        + 4 * s.stores.len())
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
    join: Option<&str>,
    mask: bool,
    expand: bool,
) -> Result<()> {
    let head = &Label::from(head.to_string());
    let arm = &Label::from(arm.to_string());
    let join = join.map(|j| Label::from(j.to_string()));
    let before_fun = before.get(function)?;
    let s = site(before_fun, function, head, arm, join.as_ref(), mask, expand)?;
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
    // Stores by block, ascending index, for the same reconstruction.
    let mut stores_of: std::collections::BTreeMap<String, Vec<&StoreSite>> = Default::default();
    for store in &s.stores {
        stores_of
            .entry(store.block.as_str().to_string())
            .or_default()
            .push(store);
    }
    for list in stores_of.values_mut() {
        list.sort_by_key(|st| st.index);
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

        // Expected instructions: walk the before block, splicing in each
        // guard after its bound's definition and each store's masked triple
        // before the store, in the order `apply` leaves them. The minted ids
        // are learned from the after block at the position each insertion
        // must occupy; freshness and distinctness are checked here, the
        // shapes by the exact comparison below, built from the learned ids.
        let empty: Vec<&LoopBound> = Vec::new();
        let guards = guards_of
            .get(&label_str.clone().filter(|_| key.is_some()))
            .unwrap_or(&empty);
        let empty_stores: Vec<&StoreSite> = Vec::new();
        let stores = label_str
            .as_ref()
            .and_then(|l| stores_of.get(l))
            .unwrap_or(&empty_stores);
        let learn = |at: usize,
                         minted_seen: &mut FxHashSet<LocalId>|
         -> Result<[LocalId; 3]> {
            let mut minted = [LocalId::from(0); 3];
            for (offset, slot) in minted.iter_mut().enumerate() {
                let (after_id, _) =
                    after_block.instructions.get(at + offset).ok_or_else(|| {
                        anyhow!("'{}' is too short to hold its insertions", name)
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
                *slot = *after_id;
            }
            Ok(minted)
        };
        let mut want: Vec<(LocalId, Instruction)> = Vec::new();
        for position in 0..=before_block.instructions.len() {
            for bound in guards.iter().filter(|b| b.def_index + 1 == position) {
                let minted = learn(want.len(), &mut minted_seen)?;
                want.extend(guard_for(bound.bound, &minted));
            }
            let Some((id, instr)) = before_block.instructions.get(position) else {
                break;
            };
            if let Some(store) = stores.iter().find(|st| st.index == position) {
                let [guard, old, sel] = learn(want.len(), &mut minted_seen)?;
                want.push((guard, Instruction::AssertValueCell { target: store.target }));
                want.push((old, Instruction::Load { source: store.target }));
                want.push((sel, masked_select_for(&s, store, old)));
                want.push((
                    *id,
                    Instruction::Store { target: store.target, source: sel },
                ));
                continue;
            }
            let instr = if key.as_ref() == Some(&s.join) {
                match s.join_phis.iter().find(|p| &p.id == id) {
                    Some(phi) => select_for(&s, phi),
                    None => instr.clone(),
                }
            } else {
                instr.clone()
            };
            want.push((*id, instr));
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
        } else if let Some(ser) =
            s.serialize.as_ref().filter(|ser| key.as_ref() == Some(&ser.arm_exit))
        {
            Terminator::UnconditionalBranch { target: ser.other.clone() }
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
                if site(fun, function.as_str(), label, arm, None, false, false).is_ok() {
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
            vec![
                (id(11), num(1)),
                // The bound is computed, not constant, so the termination guard
                // is planted (a constant bound is checked statically instead).
                (
                    id(12),
                    Instruction::BinaryOp { left: id(11), op: BinaryOp::Plus, right: id(11) },
                ),
                (id(20), num(1)),
            ],
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
        let changed = apply(&mut p, "f", "h", "setup", None, false, false).unwrap();
        assert_eq!(changed, 1 + 1 + 3);
        verify(&before, &p, "f", "h", "setup", None, false, false).unwrap();
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
        // bound and the step constant that follows it in `setup`.
        let bound_at = text.find("%12 = %11 + %11").unwrap();
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
        apply(&mut p, "f", "h", "setup", None, false, false).unwrap();
        verify(&before, &p, "f", "h", "setup", None, false, false).unwrap();
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
        let error = apply(&mut p, "f", "h", "setup", None, false, false)
            .unwrap_err()
            .to_string();
        assert!(error.contains("is a store; add \"mask\":true"), "{}", error);
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
        let error = apply(&mut p, "f", "h", "setup", None, false, false)
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
        let error = apply(&mut p, "f", "h", "setup", None, false, false)
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
        let error = apply(&mut p, "f", "h", "setup", None, false, false)
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
        let error = apply(&mut p, "f", "h", "setup", None, false, false)
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
        let error = apply(&mut p, "f", "h", "setup", None, false, false)
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("must have exactly 'h' and the exit 'cont'"),
            "{}",
            error
        );
    }

    #[test]
    fn verify_rejects_a_kept_branch() {
        let mut p = region_program();
        let before = p.clone();
        apply(&mut p, "f", "h", "setup", None, false, false).unwrap();
        // Tamper: restore the branch but keep the selects and guards.
        {
            let fun = p.get_mut("f").unwrap();
            let h = fun.cfg.named.get_mut(&label("h")).unwrap();
            h.terminator.1 = br_if(10, "setup", "join");
        }
        let error = verify(&before, &p, "f", "h", "setup", None, false, false)
            .unwrap_err()
            .to_string();
        assert!(error.contains("terminator of 'h'"), "{}", error);
    }

    #[test]
    fn verify_rejects_a_swapped_select() {
        let mut p = region_program();
        let before = p.clone();
        apply(&mut p, "f", "h", "setup", None, false, false).unwrap();
        {
            let fun = p.get_mut("f").unwrap();
            let join = fun.cfg.named.get_mut(&label("join")).unwrap();
            join.instructions[0].1 = Instruction::Select {
                condition: id(10),
                if_true: id(19),
                if_false: id(17),
            };
        }
        let error = verify(&before, &p, "f", "h", "setup", None, false, false)
            .unwrap_err()
            .to_string();
        assert!(error.contains("changed %18"), "{}", error);
    }

    #[test]
    fn verify_rejects_a_missing_guard() {
        let mut p = region_program();
        let before = p.clone();
        apply(&mut p, "f", "h", "setup", None, false, false).unwrap();
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
        let error = verify(&before, &p, "f", "h", "setup", None, false, false)
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

    /// entry -> h; h: br %10 ? a : b.
    /// a: %11 = 1 -> join.   b: %12 = 2 -> join.
    /// join: %18 = phi(a: %11, b: %12); return %18.
    ///
    /// The smallest diamond: both regions are single blocks that double as
    /// their own exits.
    fn diamond_program() -> Program {
        let entry = block(vec![], 900, br("h"));
        let h = block(
            vec![(id(10), Instruction::BoolConstant { value: true })],
            901,
            br_if(10, "a", "b"),
        );
        let a = block(vec![(id(11), num(1))], 902, br("join"));
        let b = block(vec![(id(12), num(2))], 903, br("join"));
        let join = block(
            vec![(
                id(18),
                Instruction::Phi {
                    branches: vec![(label("a"), id(11)), (label("b"), id(12))],
                },
            )],
            904,
            Terminator::Return { value: Some(id(18)) },
        );
        let mut named = crate::ir::new_label_map();
        named.insert(label("h"), h);
        named.insert(label("a"), a);
        named.insert(label("b"), b);
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

    /// With an explicit join, both regions run in sequence - named arm
    /// first - and the join picks per lane.
    #[test]
    fn serializes_a_diamond() {
        let mut p = diamond_program();
        let before = p.clone();
        let changed = apply(&mut p, "f", "h", "a", Some("join"), false, false).unwrap();
        assert_eq!(changed, 3); // head branch + exit rewire + one phi
        verify(&before, &p, "f", "h", "a", Some("join"), false, false).unwrap();

        let fun = p.get("f").unwrap();
        assert_eq!(
            fun.cfg.named[&label("h")].terminator_kind(),
            &br("a"),
        );
        assert_eq!(fun.cfg.named[&label("a")].terminator_kind(), &br("b"));
        assert_eq!(fun.cfg.named[&label("b")].terminator_kind(), &br("join"));
        assert_eq!(
            fun.cfg.named[&label("join")].instructions,
            vec![(
                id(18),
                Instruction::Select {
                    condition: id(10),
                    if_true: id(11),
                    if_false: id(12),
                },
            )],
        );
    }

    /// Naming the second arm mirrors the serialization order and the select.
    #[test]
    fn serializes_a_diamond_from_the_false_arm() {
        let mut p = diamond_program();
        let before = p.clone();
        apply(&mut p, "f", "h", "b", Some("join"), false, false).unwrap();
        verify(&before, &p, "f", "h", "b", Some("join"), false, false).unwrap();
        let fun = p.get("f").unwrap();
        assert_eq!(fun.cfg.named[&label("h")].terminator_kind(), &br("b"));
        assert_eq!(fun.cfg.named[&label("b")].terminator_kind(), &br("a"));
        assert_eq!(
            fun.cfg.named[&label("join")].instructions[0].1,
            Instruction::Select {
                condition: id(10),
                if_true: id(11),
                if_false: id(12),
            },
        );
    }

    /// A join that is the head's other branch target is a triangle wearing a
    /// costume; the explicit field is refused so each shape has one spelling.
    #[test]
    fn refuses_a_join_that_names_the_triangle() {
        let mut p = region_program();
        let error = apply(&mut p, "f", "h", "setup", Some("join"), false, false)
            .unwrap_err()
            .to_string();
        assert!(error.contains("drop the join field"), "{}", error);
    }

    /// The second region's entry must be phi-free: its predecessor changes.
    #[test]
    fn refuses_a_second_entry_with_phis() {
        let mut p = diamond_program();
        {
            let fun = p.get_mut("f").unwrap();
            let b = fun.cfg.named.get_mut(&label("b")).unwrap();
            b.instructions.insert(
                0,
                (
                    id(13),
                    Instruction::Phi { branches: vec![(label("h"), id(10))] },
                ),
            );
        }
        let error = apply(&mut p, "f", "h", "a", Some("join"), false, false)
            .unwrap_err()
            .to_string();
        assert!(error.contains("has phis"), "{}", error);
    }

    /// The verifier must reject an applier that forgot to serialize.
    #[test]
    fn verify_rejects_a_missing_serialization() {
        let mut p = diamond_program();
        let before = p.clone();
        apply(&mut p, "f", "h", "a", Some("join"), false, false).unwrap();
        {
            let fun = p.get_mut("f").unwrap();
            let a = fun.cfg.named.get_mut(&label("a")).unwrap();
            a.terminator.1 = br("join");
        }
        let error = verify(&before, &p, "f", "h", "a", Some("join"), false, false)
            .unwrap_err()
            .to_string();
        assert!(error.contains("terminator"), "{}", error);
    }

    /// A store in a masked triangle region becomes the assert-load-select-
    /// store, keeping its id, with the new value on the arm's side of the
    /// condition.
    #[test]
    fn masks_a_store_in_a_triangle_region() {
        let mut p = region_program();
        {
            let fun = p.get_mut("f").unwrap();
            let body = fun.cfg.named.get_mut(&label("loop_body")).unwrap();
            body.instructions.push((
                id(30),
                Instruction::Store { target: id(13), source: id(16) },
            ));
        }
        let before = p.clone();
        let changed = apply(&mut p, "f", "h", "setup", None, true, false).unwrap();
        assert_eq!(changed, 1 + 1 + 3 + 4);
        verify(&before, &p, "f", "h", "setup", None, true, false).unwrap();
        let body = &p.get("f").unwrap().cfg.named[&label("loop_body")];
        let n = body.instructions.len();
        let (_, cell_guard) = &body.instructions[n - 4];
        let (old, load) = &body.instructions[n - 3];
        let (sel, select) = &body.instructions[n - 2];
        let (store_id, store) = &body.instructions[n - 1];
        assert_eq!(cell_guard, &Instruction::AssertValueCell { target: id(13) });
        assert_eq!(load, &Instruction::Load { source: id(13) });
        assert_eq!(
            select,
            &Instruction::Select { condition: id(10), if_true: id(16), if_false: *old },
        );
        assert_eq!(store, &Instruction::Store { target: id(13), source: *sel });
        assert_eq!(*store_id, id(30));
    }

    /// With the arm on the false side, the masked select keeps the old
    /// value on the true side instead - no `not` is minted.
    #[test]
    fn masks_a_store_on_the_false_arm() {
        let mut p = region_program();
        {
            let fun = p.get_mut("f").unwrap();
            fun.cfg.named.get_mut(&label("h")).unwrap().terminator.1 =
                br_if(10, "join", "setup");
            let body = fun.cfg.named.get_mut(&label("loop_body")).unwrap();
            body.instructions.push((
                id(30),
                Instruction::Store { target: id(13), source: id(16) },
            ));
        }
        let before = p.clone();
        apply(&mut p, "f", "h", "setup", None, true, false).unwrap();
        verify(&before, &p, "f", "h", "setup", None, true, false).unwrap();
        let body = &p.get("f").unwrap().cfg.named[&label("loop_body")];
        let n = body.instructions.len();
        let (old, _) = &body.instructions[n - 3];
        let (_, select) = &body.instructions[n - 2];
        assert_eq!(
            select,
            &Instruction::Select { condition: id(10), if_true: *old, if_false: id(16) },
        );
    }

    /// In a masked diamond each region's stores land on its own side of the
    /// condition.
    #[test]
    fn masks_stores_in_both_diamond_regions() {
        let mut p = diamond_program();
        {
            let fun = p.get_mut("f").unwrap();
            fun.cfg.named.get_mut(&label("a")).unwrap().instructions.push((
                id(30),
                Instruction::Store { target: id(11), source: id(11) },
            ));
            fun.cfg.named.get_mut(&label("b")).unwrap().instructions.push((
                id(31),
                Instruction::Store { target: id(12), source: id(12) },
            ));
        }
        let before = p.clone();
        apply(&mut p, "f", "h", "a", Some("join"), true, false).unwrap();
        verify(&before, &p, "f", "h", "a", Some("join"), true, false).unwrap();
        let fun = p.get("f").unwrap();
        let a_block = &fun.cfg.named[&label("a")];
        let (old_a, _) = &a_block.instructions[a_block.instructions.len() - 3];
        let (_, select_a) = &a_block.instructions[a_block.instructions.len() - 2];
        assert_eq!(
            select_a,
            &Instruction::Select { condition: id(10), if_true: id(11), if_false: *old_a },
        );
        let b_block = &fun.cfg.named[&label("b")];
        let (old_b, _) = &b_block.instructions[b_block.instructions.len() - 3];
        let (_, select_b) = &b_block.instructions[b_block.instructions.len() - 2];
        assert_eq!(
            select_b,
            &Instruction::Select { condition: id(10), if_true: *old_b, if_false: id(12) },
        );
        // Still serialized: a's exit continues into b.
        assert_eq!(a_block.terminator_kind(), &br("b"));
    }

    /// `mask` on a store-free region is an unjustified premise: refuse.
    #[test]
    fn refuses_mask_without_stores() {
        let mut p = region_program();
        let error = apply(&mut p, "f", "h", "setup", None, true, false)
            .unwrap_err()
            .to_string();
        assert!(error.contains("no stores; drop the mask field"), "{}", error);
    }

    /// A guard and a store triple landing at the same position keep the
    /// guard first, and verify reproduces that layout.
    #[test]
    fn guard_and_store_at_the_same_position() {
        let mut p = region_program();
        {
            let fun = p.get_mut("f").unwrap();
            let setup = fun.cfg.named.get_mut(&label("setup")).unwrap();
            // Right after the bound's definition, where its guard also goes.
            setup.instructions.insert(
                2,
                (id(30), Instruction::Store { target: id(11), source: id(11) }),
            );
        }
        let before = p.clone();
        apply(&mut p, "f", "h", "setup", None, true, false).unwrap();
        verify(&before, &p, "f", "h", "setup", None, true, false).unwrap();
        let setup = &p.get("f").unwrap().cfg.named[&label("setup")];
        // [%11, %12, guard x3, triple x3, %30 store, %20]
        assert_eq!(setup.instructions.len(), 10);
        assert!(matches!(
            setup.instructions[2].1,
            Instruction::NumberConstant { value } if value == Pico8Num::from_i16(BOUND_LIMIT)
        ));
        assert!(matches!(
            setup.instructions[5].1,
            Instruction::AssertValueCell { target } if target == id(11)
        ));
        assert!(matches!(
            setup.instructions[8].1,
            Instruction::Store { target, .. } if target == id(11)
        ));
        assert_eq!(setup.instructions[8].0, id(30));
    }

    /// The verifier rebuilds the masked select itself, so a swapped
    /// polarity in the after program is caught.
    #[test]
    fn verify_rejects_a_swapped_mask_polarity() {
        let mut p = region_program();
        {
            let fun = p.get_mut("f").unwrap();
            let body = fun.cfg.named.get_mut(&label("loop_body")).unwrap();
            body.instructions.push((
                id(30),
                Instruction::Store { target: id(13), source: id(16) },
            ));
        }
        let before = p.clone();
        apply(&mut p, "f", "h", "setup", None, true, false).unwrap();
        {
            let fun = p.get_mut("f").unwrap();
            let body = fun.cfg.named.get_mut(&label("loop_body")).unwrap();
            let n = body.instructions.len();
            let old = body.instructions[n - 3].0;
            body.instructions[n - 2].1 = Instruction::Select {
                condition: id(10),
                if_true: old,
                if_false: id(16),
            };
        }
        let error = verify(&before, &p, "f", "h", "setup", None, true, false)
            .unwrap_err()
            .to_string();
        assert!(error.contains("changed %"), "{}", error);
    }

    /// entry: button-cell accessors and the masked-store cell.
    /// h: br %10 ? arm : join.
    /// arm: %31 = load %30; %32 = expand %31; %33 = <same accessor>;
    ///      store %33 <- %32 (concretization); %36 = store %3 <- %35
    ///      (ordinary, masked). -> join.
    /// join: %18 = phi(h: %19, arm: %35); return %18.
    ///
    /// The shape `expand_bool` leaves in the dash arm, next to a store that
    /// still needs the mask.
    fn expand_program() -> Program {
        let entry = block(
            vec![
                (
                    id(1),
                    Instruction::GetGlobal {
                        name: "__button_states".to_string(),
                        create_if_missing: false,
                    },
                ),
                (id(2), num(1)),
                (
                    id(3),
                    Instruction::GetGlobal {
                        name: "cell".to_string(),
                        create_if_missing: false,
                    },
                ),
            ],
            900,
            br("h"),
        );
        let h = block(
            vec![
                (id(10), Instruction::BoolConstant { value: true }),
                (id(19), Instruction::NilConstant),
            ],
            901,
            br_if(10, "arm", "join"),
        );
        let arm = block(
            vec![
                (
                    id(30),
                    Instruction::GetIndex {
                        receiver: id(1),
                        index: id(2),
                        create_if_missing: false,
                    },
                ),
                (id(31), Instruction::Load { source: id(30) }),
                (id(32), Instruction::Expand { value: id(31) }),
                (
                    id(33),
                    Instruction::GetIndex {
                        receiver: id(1),
                        index: id(2),
                        create_if_missing: false,
                    },
                ),
                (id(34), Instruction::Store { target: id(33), source: id(32) }),
                (id(35), num(5)),
                (id(36), Instruction::Store { target: id(3), source: id(35) }),
            ],
            902,
            br("join"),
        );
        let join = block(
            vec![(
                id(18),
                Instruction::Phi {
                    branches: vec![(label("h"), id(19)), (label("arm"), id(35))],
                },
            )],
            903,
            Terminator::Return { value: Some(id(18)) },
        );
        let mut named = crate::ir::new_label_map();
        named.insert(label("h"), h);
        named.insert(label("arm"), arm);
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

    /// The concretization pair runs untouched and unmasked; the ordinary
    /// store next to it still gets the masked triple.
    #[test]
    fn speculates_an_expand_region() {
        let mut p = expand_program();
        let before = p.clone();
        let changed = apply(&mut p, "f", "h", "arm", None, true, true).unwrap();
        assert_eq!(changed, 1 + 1 + 4); // head branch + one phi + one masked store
        verify(&before, &p, "f", "h", "arm", None, true, true).unwrap();
        let fun = p.get("f").unwrap();
        assert_eq!(
            fun.cfg.named[&label("h")].terminator_kind(),
            &br("arm"),
        );
        let arm = &fun.cfg.named[&label("arm")];
        // Accessor, load, expand, accessor, concretization store: byte-identical.
        assert_eq!(
            arm.instructions[..5],
            before.get("f").unwrap().cfg.named[&label("arm")].instructions[..5],
        );
        // The ordinary store is masked as usual, keeping its id.
        let n = arm.instructions.len();
        assert!(matches!(
            arm.instructions[n - 4].1,
            Instruction::AssertValueCell { target } if target == id(3)
        ));
        assert_eq!(arm.instructions[n - 1].0, id(36));
        // The join phi became a select on the head's condition.
        assert_eq!(
            fun.cfg.named[&label("join")].instructions[0].1,
            Instruction::Select { condition: id(10), if_true: id(35), if_false: id(19) },
        );
    }

    /// `expand` in the region without the opt-in: refuse, naming the flag.
    #[test]
    fn refuses_expand_without_the_flag() {
        let mut p = expand_program();
        let error = apply(&mut p, "f", "h", "arm", None, true, false)
            .unwrap_err()
            .to_string();
        assert!(error.contains("add \"expand\":true"), "{}", error);
    }

    /// The flag on an expand-free region is an unjustified premise: refuse.
    #[test]
    fn refuses_the_expand_flag_without_an_expand() {
        let mut p = region_program();
        let error = apply(&mut p, "f", "h", "setup", None, false, true)
            .unwrap_err()
            .to_string();
        assert!(error.contains("no expand; drop the expand field"), "{}", error);
    }

    /// A store of an expanded value into anything but the loaded cell would
    /// clobber that cell on lanes that skipped the region.
    #[test]
    fn refuses_a_concretization_store_through_a_different_cell() {
        let mut p = expand_program();
        {
            let fun = p.get_mut("f").unwrap();
            let arm = fun.cfg.named.get_mut(&label("arm")).unwrap();
            arm.instructions[3].1 = Instruction::GetIndex {
                receiver: id(1),
                index: id(3),
                create_if_missing: false,
            };
        }
        let error = apply(&mut p, "f", "h", "arm", None, true, true)
            .unwrap_err()
            .to_string();
        assert!(error.contains("is not the loaded cell"), "{}", error);
    }

    /// A store between the load and the store-back means the cell may no
    /// longer hold the loaded value, so the store-back is no refinement.
    #[test]
    fn refuses_an_intervening_store() {
        let mut p = expand_program();
        {
            let fun = p.get_mut("f").unwrap();
            let arm = fun.cfg.named.get_mut(&label("arm")).unwrap();
            arm.instructions.insert(
                2,
                (id(37), Instruction::Store { target: id(3), source: id(19) }),
            );
        }
        let error = apply(&mut p, "f", "h", "arm", None, true, true)
            .unwrap_err()
            .to_string();
        assert!(
            error.contains("stores between the load and the store-back"),
            "{}",
            error
        );
    }

    /// An expand of anything but a load has no cell whose content the
    /// store-back could be refining.
    #[test]
    fn refuses_an_expand_of_a_non_load() {
        let mut p = expand_program();
        {
            let fun = p.get_mut("f").unwrap();
            let arm = fun.cfg.named.get_mut(&label("arm")).unwrap();
            arm.instructions[1].1 = Instruction::BoolConstant { value: true };
        }
        let error = apply(&mut p, "f", "h", "arm", None, true, true)
            .unwrap_err()
            .to_string();
        assert!(error.contains("is not a load"), "{}", error);
    }

    /// The verifier pins the concretization store byte-for-byte: an applier
    /// that rewrote it - masked it, or dropped the expanded value - is caught.
    #[test]
    fn verify_rejects_a_tampered_concretization_store() {
        let mut p = expand_program();
        let before = p.clone();
        apply(&mut p, "f", "h", "arm", None, true, true).unwrap();
        {
            let fun = p.get_mut("f").unwrap();
            let arm = fun.cfg.named.get_mut(&label("arm")).unwrap();
            arm.instructions[4].1 =
                Instruction::Store { target: id(33), source: id(31) };
        }
        let error = verify(&before, &p, "f", "h", "arm", None, true, true)
            .unwrap_err()
            .to_string();
        assert!(error.contains("changed %34"), "{}", error);
    }
}
