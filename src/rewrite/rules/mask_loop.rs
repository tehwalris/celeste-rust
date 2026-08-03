//! `mask_loop` - give a loop with a per-lane trip count a uniform constant
//! trip count, and turn per-lane iteration into mask data.
//!
//! # Why
//!
//! The pixel-stepping loops in `obj.move_x`/`obj.move_y` step one pixel at a
//! time, `abs(amount)` times - and `amount` is per-lane data (speed differs
//! across lanes), so the loop-continue branch splits the state on every
//! iteration, and the `break` branch in the body splits it again. These are
//! the last splitting branches in `anonymous_61`.
//!
//! The interpreter's model is uniform control flow per state, with only
//! scalars varying per lane. A loop whose *trip count* is per-lane has no
//! uniform representation - but nothing requires the loop to be unrolled to
//! fix that. It only has to run a number of iterations every lane agrees on:
//!
//! ```text
//!   for i=0..bound:  body            for k=0..LIMIT:              (uniform)
//!     if solid: stops; break    =>     cond   = i <= bound        (per lane)
//!     else:     x += step              take   = active && cond
//!     i += 1                           do_step  = take && !solid
//!                                      do_break = take && solid
//!                                      x      = select do_step ? x+step : x
//!                                      stops       masked by do_break
//!                                      i      = select do_step ? i+1 : i
//!                                      active = select do_break ? false : active
//! ```
//!
//! The loop stays rolled; the trip count becomes the constant `LIMIT` from
//! the recipe entry, which is per-state uniform, so the head branch never
//! splits again. Which lanes are *really* iterating becomes data: `cond`
//! (the lane still has pixels to move) and `active` (the lane has not hit a
//! wall), combined into masks that guard every effect. The break edge is
//! deleted outright. This is the same masked execution the recipe already
//! does one instruction at a time - `select` is a masked move, and
//! `store p <- select c ? new : old` is a masked store - extended to
//! iteration itself.
//!
//! # The shape
//!
//! ```text
//!   P:    ..defines bound..            <- preheader, one head edge
//!   H:    i = phi [P: init, L: i']     <- exactly this phi and this compare
//!         cond = i <= bound
//!         br cond ? E : X
//!   E:    ..body region..              <- pure, may contain uniform loops
//!   C:    ..                           <- the body's single exit
//!         br bc ? L : B                <- continue or break, latch on true
//!   L:    ..stores.., i' = i + 1       <- the latch, br H
//!   B:    ..stores..                   <- the break block, br X
//!   X:    ..                           <- the exit, no phis
//! ```
//!
//! After: `H` gains a uniform counter `k` (phi, `+1` per iteration, compared
//! `<= LIMIT`) and the `active` phi; `C` computes the masks and falls
//! through to `L`; `L` keeps its instructions with every store masked by
//! `do_step`, absorbs `B`'s instructions with every store masked by
//! `do_break`, and updates `i`, `k` and `active`; `B` is deleted.
//!
//! # Soundness
//!
//! **Lanes that are done still execute the body.** `is_speculatable` for
//! every body instruction (phis allowed at internal joins), same bargain as
//! `speculate_region`: nothing can go wrong silently, loud failures are a
//! screening question. Frozen lanes feed the body their last real values,
//! so the computations are the same ones the lane's final iteration already
//! did.
//!
//! **Effects must not fire for done lanes.** Every store in the latch and
//! the break block is rewritten to the load-adjacent masked form
//! (`assert_value_cell`; load old; select; store), which reproduces the
//! cell's current value wherever the mask is false - no aliasing analysis
//! needed, exactly as in `absorb_stores`. The masks are per-lane ANDs
//! spelled as selects: `take = active && cond` catches both ways a lane can
//! be done, `do_step`/`do_break` split `take` by the branch condition, and
//! they are disjoint, so absorbing `B` behind `L` cannot reorder anything
//! observable. `i` advances under `do_step` exactly as the original latch
//! did; `active` drops to false under `do_break` and never recovers.
//!
//! **The rewritten loop must cover every lane's real iterations.** An
//! active lane increments `i` every iteration (it either steps or breaks),
//! so lane activity ends by `i > bound` at the latest - and the runtime
//! guard `assert_true(bound <= LIMIT)`, planted after the bound's
//! definition, makes `LIMIT` iterations provably enough. A state with a
//! faster lane than the recipe anticipated fails loudly; raise the limit.
//!
//! **The loop must still terminate** - now unconditionally, since it always
//! runs `LIMIT+1` iterations, and `LIMIT` is a recipe constant well below
//! wrapping range. Inner cycles of the body ran before under the same
//! per-state bounds; they keep the `speculate_region` counter-shape
//! obligation and its `bound < 32767` runtime guard (bounds already guarded
//! by an earlier entry are recognised and not guarded twice).
//!
//! **Nothing else can observe the loop.** No local defined in the loop may
//! be used outside it, and the exit block must have no phis (it loses the
//! break edge). The loop's only external effects are its stores, which are
//! exactly the masked ones.
//!
//! # The `span` and `break_to` extensions
//!
//! The `spikes_at` tile loops differ from the pixel loops in two ways, each
//! behind an opt-in recipe field so the pixel-loop entries replay
//! byte-identically.
//!
//! **`span`** replaces `limit` when the counter's *init* is per-lane too
//! (`for i = max(0, flr(x/8)), min(15, ...)`). Guarding `bound <= limit`
//! would force 16 iterations; the trips actually run
//! `floor(bound - init) + 1`, so the loop runs `span + 1` uniform iterations
//! under two guards: `assert_true(init >= 0)` and
//! `assert_true(bound - init < span + 1)`. When `init > bound` the loop ran
//! zero trips and the difference is irrelevant; when `0 <= init <= bound` it
//! cannot wrap, so the check is exact. (`limit`
//! mode's coverage argument quietly assumed a non-negative start; it now
//! requires the init to be a constant >= 0, which the pixel loops' `1`
//! satisfies.)
//!
//! **`break_to`** names an external join `J` when the break jumps over more
//! than the loop - `spikes_at`'s hit exits both tile loops at once. The
//! break block may then only feed `J`'s phis bool constants. The break edge
//! is deleted as usual, and the exit carries the break out instead, in one
//! of two shapes:
//!
//! * The exit continues elsewhere (the inner loop: its exit is the outer
//!   latch): it gains `br active ? onward : J`, and `J`'s phi entry moves to
//!   the exit edge with a minted copy of the constant. That branch is
//!   per-lane and still splits - until the outer application consumes it,
//!   because it is exactly the merged shape below.
//! * The exit already falls through to `J` (the outer loop after the inner
//!   conversion): no branch at all - `J`'s entry for the exit becomes
//!   `select active ? exhausted_value : break_constant`, the break entry
//!   disappears, and the nest is straight-line.
//!
//! In the merged shape the latch itself holds the continue/break branch
//! (`br active ? head : J`), so there is no separate break block and nothing
//! is deleted; the branch may also break on true instead of continuing on
//! true, and the masks flip accordingly.
//!
//! # What this rule refuses, deliberately
//!
//! A latch that is not the branch's true side (except under `break_to`,
//! where the polarity is read off the shape); a counter that is not
//! `phi` + `<=` + `+1`; a `limit`-mode init that is not a constant >= 0;
//! non-store non-speculatable instructions anywhere in the loop; stores in a
//! merged latch; phis in the latch, break or exit; loop values used outside
//! (save the join's break-edge phi entries, which this rule rewrites);
//! `hint_normalize` or `return` inside; inner cycles without the counter
//! shape; guard sites inside the blocks this rule rewrites. Each is a shape
//! the loops this rule has been pointed at do not have.

use anyhow::{anyhow, Result};
use rustc_hash::FxHashSet;

use crate::ir::{
    BinaryOp, Block, FunDef, Instruction, Label, LocalId, SlotMap, Terminator, UnaryOp,
};
use crate::pico8_num::Pico8Num;

use super::super::print::format_instruction;
use super::super::program::Program;
use super::if_convert::is_speculatable;
use super::speculate_region::{
    already_guarded, back_edges, defining_instruction, defining_site, guard_for,
};
use super::{predecessors, require, LocalIdAllocator};

/// One inner-cycle bound needing the `speculate_region` guard.
struct InnerGuard {
    bound: LocalId,
    def_block: Label,
    def_index: usize,
}

/// The uniform iteration count, and the guard shape that makes it enough.
#[derive(Clone, Copy)]
enum Trip {
    /// `k` runs 0..=limit; the guard is `assert_true(bound <= limit)`, and
    /// the counter must start at a constant >= 0 for that to cover the trips.
    Limit(i16),
    /// `k` runs 0..=span; the guards are `assert_true(init >= 0)` and
    /// `assert_true(bound - init < span + 1)` - strict, because a Lua `for`
    /// bound may be fractional and the trips are `floor(bound - init) + 1`.
    /// The init may be per-lane - when `init > bound` the loop ran zero
    /// trips and the (possibly wrapped) difference is irrelevant; when
    /// `init <= bound` and `init >= 0` the difference cannot wrap, so the
    /// check is exact.
    Span(i16),
}

impl Trip {
    fn k_max(self) -> i16 {
        match self {
            Trip::Limit(v) | Trip::Span(v) => v,
        }
    }
}

/// One phi at the external break target, in instruction order.
struct JPhi {
    /// The bool constant the deleted break edge contributed.
    break_value: bool,
    /// The value the exit edge contributes today (`exit_to_join` only).
    exit_value: Option<LocalId>,
}

/// The shape, re-derived identically by `apply` and `verify`.
struct Site {
    preheader: Label,
    body_entry: Label,
    exit: Label,
    cont: Label,
    latch: Label,
    /// The break block. `None` in the merged shape, where the continue/break
    /// branch sits in the latch itself and the break edge leads straight to
    /// the external join.
    brk: Option<Label>,
    /// The external break target (`break_to` mode).
    join: Option<Label>,
    /// True when `cont == latch`: the loop's single tail both branches and
    /// increments, as the second `break_to` application leaves it.
    merged: bool,
    /// True when the continue/break branch continues on its true side.
    latch_on_true: bool,
    /// `break_to` mode: the exit already falls through to the join, so the
    /// break value merges into its phi entry by select instead of an edge.
    exit_to_join: bool,
    /// The exit's unconditional successor (`break_to` mode).
    exit_target: Option<Label>,
    /// The counter phi and its compare, both in the head.
    i_id: LocalId,
    cond_id: LocalId,
    bound: LocalId,
    /// The counter's initial value: the phi's preheader edge.
    init: LocalId,
    /// Where in the preheader the trip guard goes: after the bound for
    /// `limit`, after both the bound and the init for `span`.
    guard_index: usize,
    /// The raw `i + 1` in the latch.
    next_id: LocalId,
    /// The continue/break branch's raw condition.
    bc: LocalId,
    /// Phis at the join, in instruction order (`break_to` mode).
    j_phis: Vec<JPhi>,
    inner_guards: Vec<InnerGuard>,
}

impl Site {
    /// The block whose edge into the join is deleted.
    fn brk_source(&self) -> &Label {
        self.brk.as_ref().unwrap_or(&self.latch)
    }
}

/// Every minted id, in the order `apply` allocates them and `verify` learns
/// them.
struct Ids {
    /// One triple per inner guard, in `site.inner_guards` order.
    inner: Vec<[LocalId; 3]>,
    /// The trip guard: [limit constant, `bound <= limit`, assert] for
    /// `limit`; [zero, `init >= 0`, assert, span constant, `bound - init`,
    /// `<=`, assert] for `span`.
    outer: Vec<LocalId>,
    /// Preheader tail: `k` init constant, `active` init constant.
    p_tail: [LocalId; 2],
    /// Head: `k` phi, `active` phi, trip constant, `k <= trip`.
    head: [LocalId; 4],
    /// The masks: take, then do_step / `!bc` / do_break in a
    /// polarity-dependent order (see `mask_tail`).
    cont: [LocalId; 4],
    /// One triple per store (latch's stores first, then the break's, each in
    /// block order): assert_value_cell, load, select.
    stores: Vec<[LocalId; 3]>,
    /// Latch tail: masked `i`, `1` constant, `k + 1`, `false` constant,
    /// masked `active`.
    l_tail: [LocalId; 5],
    /// `break_to` mode, per join phi: the minted break constant, plus the
    /// merging select when the exit already falls through to the join.
    x_tail: Vec<LocalId>,
}

fn store_count(block: &Block) -> usize {
    block
        .instructions
        .iter()
        .filter(|(_, i)| matches!(i, Instruction::Store { .. }))
        .count()
}

fn site(
    fun: &FunDef,
    function: &str,
    head: &Label,
    trip: Trip,
    break_to: Option<&Label>,
) -> Result<Site> {
    require(trip.k_max() >= 0, "the trip count must not be negative")?;
    if let Trip::Span(span) = trip {
        require(
            span < i16::MAX,
            "the span must leave room for the `span + 1` guard constant",
        )?;
    }
    let head_block = fun.cfg.named.get(head).ok_or_else(|| {
        anyhow!("no block named '{}' in {}", head.as_str(), function)
    })?;

    // The head: exactly the counter phi and its compare.
    let Terminator::ConditionalBranch { condition, true_target, false_target } =
        head_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in a conditional branch", head.as_str()));
    };
    let (body_entry, exit) = (true_target.clone(), false_target.clone());
    require(
        head_block.instructions.len() == 2,
        format!(
            "'{}' must hold exactly the counter phi and its compare",
            head.as_str()
        ),
    )?;
    let (i_id, i_phi) = &head_block.instructions[0];
    let (cond_id, compare) = &head_block.instructions[1];
    require(
        cond_id == condition,
        format!("'{}' must branch on its own compare", head.as_str()),
    )?;
    let Instruction::Phi { branches } = i_phi else {
        return Err(anyhow!("'{}' does not start with the counter phi", head.as_str()));
    };
    require(
        branches.len() == 2,
        format!("the counter phi of '{}' must have exactly two edges", head.as_str()),
    )?;
    let Instruction::BinaryOp { left, op: BinaryOp::LessThanEqual, right: bound } = compare
    else {
        return Err(anyhow!(
            "'{}' does not compare `counter <= bound`",
            head.as_str()
        ));
    };
    require(
        left == i_id,
        format!("the compare of '{}' does not test the counter", head.as_str()),
    )?;

    if let Some(join) = break_to {
        require(
            join != head && *join != exit && *join != body_entry,
            format!(
                "the break target '{}' collides with the loop's own blocks",
                join.as_str()
            ),
        )?;
    }

    // The loop's blocks: flood from the body entry, stopping at the head,
    // the exit, and (in `break_to` mode) the external join. The sources of
    // those stopped edges are the latch and the break block.
    let mut flood: Vec<Label> = Vec::new();
    let mut seen: FxHashSet<Label> = FxHashSet::default();
    let mut to_head: Vec<Label> = Vec::new();
    let mut to_exit: Vec<Label> = Vec::new();
    let mut to_join: Vec<Label> = Vec::new();
    let mut stack = vec![body_entry.clone()];
    while let Some(label) = stack.pop() {
        if !seen.insert(label.clone()) {
            continue;
        }
        let block = fun
            .cfg
            .named
            .get(&label)
            .ok_or_else(|| anyhow!("loop block '{}' does not exist", label.as_str()))?;
        require(
            !block.hint_normalize,
            format!("loop block '{}' is a hint_normalize block", label.as_str()),
        )?;
        match block.terminator_kind() {
            Terminator::Return { .. } => {
                return Err(anyhow!("loop block '{}' returns", label.as_str()))
            }
            terminator => {
                for successor in terminator.successor_labels() {
                    if successor == head {
                        to_head.push(label.clone());
                    } else if successor == &exit {
                        to_exit.push(label.clone());
                    } else if Some(successor) == break_to {
                        to_join.push(label.clone());
                    } else {
                        stack.push(successor.clone());
                    }
                }
            }
        }
        flood.push(label);
    }
    to_head.sort();
    to_head.dedup();
    to_exit.sort();
    to_exit.dedup();
    to_join.sort();
    to_join.dedup();
    let [latch] = to_head.as_slice() else {
        return Err(anyhow!(
            "the loop must reach its head through exactly one latch, found {:?}",
            to_head.iter().map(|l| l.as_str()).collect::<Vec<_>>()
        ));
    };
    let latch = latch.clone();
    let preds = predecessors(&fun.cfg);

    // The continue/break branch. Classic shape: `cont` picks the latch on
    // true and the break block on false, and the break block jumps to the
    // exit. `break_to` shape: the break edge leads to the external join
    // instead, either through a break block or (merged shape) straight from
    // the latch, whose terminator is then the branch itself.
    let (brk, cont, merged, latch_on_true, bc);
    if let Some(join) = break_to {
        require(
            to_exit.is_empty(),
            "with break_to, the exit must be reached only through the head",
        )?;
        let [b_src] = to_join.as_slice() else {
            return Err(anyhow!(
                "the loop must reach '{}' through exactly one break edge, \
                 found {:?}",
                join.as_str(),
                to_join.iter().map(|l| l.as_str()).collect::<Vec<_>>()
            ));
        };
        if b_src == &latch {
            // Merged: the latch both branches and increments.
            brk = None;
            cont = latch.clone();
            merged = true;
            let Terminator::ConditionalBranch { condition, true_target, false_target } =
                fun.cfg.named[&latch].terminator_kind()
            else {
                return Err(anyhow!("'{}' does not branch conditionally", latch.as_str()));
            };
            bc = *condition;
            latch_on_true = if true_target == head && false_target == join {
                true
            } else if true_target == join && false_target == head {
                false
            } else {
                return Err(anyhow!(
                    "'{}' must branch between the head and '{}'",
                    latch.as_str(),
                    join.as_str()
                ));
            };
        } else {
            let b = b_src.clone();
            merged = false;
            cont = match preds.get(&Some(latch.clone())).map(|p| p.as_slice()) {
                Some([Some(cont)]) => cont.clone(),
                _ => {
                    return Err(anyhow!(
                        "the latch '{}' must have exactly one predecessor",
                        latch.as_str()
                    ))
                }
            };
            require(
                matches!(
                    preds.get(&Some(b.clone())).map(|p| p.as_slice()),
                    Some([Some(p)]) if p == &cont
                ),
                format!(
                    "the break block '{}' must be reached only from '{}'",
                    b.as_str(),
                    cont.as_str()
                ),
            )?;
            let Terminator::ConditionalBranch { condition, true_target, false_target } =
                fun.cfg.named[&cont].terminator_kind()
            else {
                return Err(anyhow!("'{}' does not branch conditionally", cont.as_str()));
            };
            bc = *condition;
            latch_on_true = if true_target == &latch && false_target == &b {
                true
            } else if true_target == &b && false_target == &latch {
                false
            } else {
                return Err(anyhow!(
                    "'{}' must branch between the latch and the break block",
                    cont.as_str()
                ));
            };
            require(
                matches!(fun.cfg.named[&latch].terminator_kind(),
                    Terminator::UnconditionalBranch { target } if target == head),
                format!("the latch '{}' must jump back to the head", latch.as_str()),
            )?;
            require(
                matches!(fun.cfg.named[&b].terminator_kind(),
                    Terminator::UnconditionalBranch { target } if target == join),
                format!(
                    "the break block '{}' must jump to '{}'",
                    b.as_str(),
                    join.as_str()
                ),
            )?;
            brk = Some(b);
        }
    } else {
        let [b] = to_exit.as_slice() else {
            return Err(anyhow!(
                "the loop must reach the exit through exactly one break block, \
                 found {:?}",
                to_exit.iter().map(|l| l.as_str()).collect::<Vec<_>>()
            ));
        };
        let b = b.clone();
        require(latch != b, "the latch and the break block must differ")?;
        merged = false;
        cont = match preds.get(&Some(latch.clone())).map(|p| p.as_slice()) {
            Some([Some(cont)]) => cont.clone(),
            _ => {
                return Err(anyhow!(
                    "the latch '{}' must have exactly one predecessor",
                    latch.as_str()
                ))
            }
        };
        require(
            matches!(
                preds.get(&Some(b.clone())).map(|p| p.as_slice()),
                Some([Some(p)]) if p == &cont
            ),
            format!(
                "the break block '{}' must be reached only from '{}'",
                b.as_str(),
                cont.as_str()
            ),
        )?;
        let Terminator::ConditionalBranch { condition, true_target, false_target } =
            fun.cfg.named[&cont].terminator_kind()
        else {
            return Err(anyhow!("'{}' does not branch conditionally", cont.as_str()));
        };
        require(
            true_target == &latch && false_target == &b,
            format!(
                "'{}' must branch to the latch on true and the break block on false",
                cont.as_str()
            ),
        )?;
        bc = *condition;
        latch_on_true = true;
        require(
            matches!(fun.cfg.named[&latch].terminator_kind(),
                Terminator::UnconditionalBranch { target } if target == head),
            format!("the latch '{}' must jump back to the head", latch.as_str()),
        )?;
        require(
            matches!(fun.cfg.named[&b].terminator_kind(),
                Terminator::UnconditionalBranch { target } if target == &exit),
            format!("the break block '{}' must jump to the exit", b.as_str()),
        )?;
        brk = Some(b);
    }

    // The preheader: the phi's other edge, from outside the loop.
    let in_flood: FxHashSet<&Label> = flood.iter().collect();
    let preheader = branches
        .iter()
        .map(|(label, _)| label)
        .find(|label| *label != &latch)
        .cloned()
        .ok_or_else(|| anyhow!("the counter phi has no preheader edge"))?;
    require(
        branches.iter().any(|(label, _)| label == &latch),
        format!("the counter phi has no edge from the latch '{}'", latch.as_str()),
    )?;
    require(
        !in_flood.contains(&preheader) && &preheader != head,
        format!("the preheader '{}' is inside the loop", preheader.as_str()),
    )?;

    // Single entry: nothing outside the loop reaches a loop block, except
    // the head's edge into the body entry.
    for label in &flood {
        for pred in preds.get(&Some(label.clone())).into_iter().flatten() {
            let allowed = match pred {
                Some(pred_label) => {
                    in_flood.contains(pred_label)
                        || (label == &body_entry && pred_label == head)
                }
                None => false,
            };
            require(
                allowed,
                format!(
                    "loop block '{}' is reachable from outside the loop",
                    label.as_str()
                ),
            )?;
        }
    }

    // The counter's increment: `i + 1`, defined in the latch.
    let next_id = branches.iter().find(|(l, _)| l == &latch).map(|(_, v)| *v).unwrap();
    let Some(Instruction::BinaryOp { left, op: BinaryOp::Plus, right: step }) =
        defining_instruction(fun, next_id)
    else {
        return Err(anyhow!(
            "the latch value %{} is not `counter + step`",
            usize::from(next_id)
        ));
    };
    require(left == i_id, "the increment does not add to the counter")?;
    require(
        matches!(
            defining_instruction(fun, *step),
            Some(Instruction::NumberConstant { value }) if *value == Pico8Num::from_i16(1)
        ),
        "the increment is not the constant 1",
    )?;
    require(
        matches!(defining_site(fun, next_id), Some((Some(l), _)) if l == latch),
        "the increment must live in the latch",
    )?;

    // Purity. Body blocks: speculatable, phis allowed at internal joins.
    // Latch and break: speculatable or a store, no phis (their single
    // predecessor makes a phi degenerate anyway).
    if merged {
        require(
            store_count(&fun.cfg.named[&latch]) == 0,
            format!(
                "the merged latch '{}' must hold no stores: the masks are \
                 defined after its instructions",
                latch.as_str()
            ),
        )?;
    }
    for label in &flood {
        let is_tail = label == &latch || Some(label) == brk.as_ref();
        for (id, instr) in &fun.cfg.named[label].instructions {
            let ok = if is_tail {
                matches!(instr, Instruction::Store { .. }) || is_speculatable(instr)
            } else {
                matches!(instr, Instruction::Phi { .. }) || is_speculatable(instr)
            };
            require(
                ok,
                format!(
                    "%{} in loop block '{}' is not maskable: {}",
                    usize::from(*id),
                    label.as_str(),
                    format_instruction(instr)
                ),
            )?;
        }
    }

    // Nothing defined in the loop is used outside it, and the exit has no
    // phis - the loop's only external effects are its (masked) stores.
    let loop_blocks: FxHashSet<&Label> = flood.iter().chain(std::iter::once(head)).collect();
    let mut defined: FxHashSet<LocalId> = FxHashSet::default();
    for label in &loop_blocks {
        defined.extend(
            fun.cfg.named[*label].instructions.iter().map(|(id, _)| *id),
        );
    }
    let all_keys: Vec<Option<Label>> = std::iter::once(None)
        .chain(fun.cfg.named.keys().cloned().map(Some))
        .collect();
    for key in &all_keys {
        if let Some(label) = key {
            if loop_blocks.contains(label) {
                continue;
            }
        }
        let block = match key {
            None => &fun.cfg.entry,
            Some(l) => &fun.cfg.named[l],
        };
        let mut used: Vec<LocalId> = Vec::new();
        for (_, instr) in &block.instructions {
            // The join's phi entries on the break edge are the one permitted
            // outside use of loop-defined values: this rule rewrites exactly
            // those entries.
            let brk_edge = key.as_ref() == break_to;
            match instr {
                Instruction::Phi { branches } if brk_edge => used.extend(
                    branches
                        .iter()
                        .filter(|(l, _)| {
                            !(l == &latch || Some(l) == brk.as_ref())
                        })
                        .map(|(_, v)| *v),
                ),
                _ => used.extend(instr.get_used_locals()),
            }
        }
        used.extend(block.terminator_kind().get_used_locals());
        for id in used {
            require(
                !defined.contains(&id),
                format!(
                    "%{} is defined in the loop but used outside it, in '{}'",
                    usize::from(id),
                    key.as_ref().map(|l| l.as_str()).unwrap_or("__entry")
                ),
            )?;
        }
    }
    require(
        !fun.cfg.named[&exit]
            .instructions
            .iter()
            .any(|(_, i)| matches!(i, Instruction::Phi { .. })),
        format!(
            "the exit '{}' has phis, and the break edge into it goes away",
            exit.as_str()
        ),
    )?;

    // `break_to` mode: the exit is rewritten to carry the break, and the
    // join's phis must say what the break edge contributed - a bool
    // constant, so a copy can be minted where the rule needs one.
    let (exit_to_join, exit_target, j_phis) = if let Some(join) = break_to {
        let exit_block = &fun.cfg.named[&exit];
        require(
            !exit_block.hint_normalize,
            format!("the exit '{}' is a hint_normalize block", exit.as_str()),
        )?;
        let Terminator::UnconditionalBranch { target } = exit_block.terminator_kind()
        else {
            return Err(anyhow!(
                "the exit '{}' must branch unconditionally",
                exit.as_str()
            ));
        };
        let exit_to_join = target == join;
        let brk_source = brk.as_ref().unwrap_or(&latch);
        let mut j_phis: Vec<JPhi> = Vec::new();
        for (id, instr) in &fun.cfg.named[join].instructions {
            let Instruction::Phi { branches } = instr else { continue };
            let break_edge = branches
                .iter()
                .find(|(l, _)| l == brk_source)
                .map(|(_, v)| *v)
                .ok_or_else(|| {
                    anyhow!(
                        "phi %{} in '{}' has no entry for the break edge from \
                         '{}'",
                        usize::from(*id),
                        join.as_str(),
                        brk_source.as_str()
                    )
                })?;
            let Some(Instruction::BoolConstant { value }) =
                defining_instruction(fun, break_edge)
            else {
                return Err(anyhow!(
                    "phi %{} in '{}' gets %{} from the break edge, which is \
                     not a bool constant",
                    usize::from(*id),
                    join.as_str(),
                    usize::from(break_edge)
                ));
            };
            let exit_value = if exit_to_join {
                Some(
                    branches
                        .iter()
                        .find(|(l, _)| l == &exit)
                        .map(|(_, v)| *v)
                        .ok_or_else(|| {
                            anyhow!(
                                "phi %{} in '{}' has no entry for the exit \
                                 '{}'",
                                usize::from(*id),
                                join.as_str(),
                                exit.as_str()
                            )
                        })?,
                )
            } else {
                require(
                    !branches.iter().any(|(l, _)| l == &exit),
                    format!(
                        "phi %{} in '{}' already has an entry for the exit \
                         '{}', which gains a break edge",
                        usize::from(*id),
                        join.as_str(),
                        exit.as_str()
                    ),
                )?;
                None
            };
            j_phis.push(JPhi { break_value: *value, exit_value });
        }
        (exit_to_join, Some(target.clone()), j_phis)
    } else {
        (false, None, Vec::new())
    };

    // The outer bound: defined in the preheader, where the trip guard has
    // a stable home. (More placements could be supported; the two pixel
    // loops compute `abs(amount)` right there.)
    let bound_index = match defining_site(fun, *bound) {
        Some((Some(label), index)) if label == preheader => index,
        _ => {
            return Err(anyhow!(
                "the bound %{} must be defined in the preheader '{}'",
                usize::from(*bound),
                preheader.as_str()
            ))
        }
    };

    // The counter's initial value. `limit` mode needs a constant >= 0 for
    // `bound <= limit` to cover the trips; `span` mode guards the window
    // instead, and only needs the init defined in the preheader so the
    // guard has both operands in scope.
    let init = branches
        .iter()
        .find(|(l, _)| l == &preheader)
        .map(|(_, v)| *v)
        .unwrap();
    let guard_index = match trip {
        Trip::Limit(_) => {
            require(
                matches!(
                    defining_instruction(fun, init),
                    Some(Instruction::NumberConstant { value })
                        if value.as_i16().is_some_and(|v| v >= 0)
                ),
                format!(
                    "the counter must start at a constant >= 0 for \
                     `bound <= limit` to cover the trips; %{} is not one",
                    usize::from(init)
                ),
            )?;
            bound_index
        }
        Trip::Span(_) => match defining_site(fun, init) {
            Some((Some(label), index)) if label == preheader => index.max(bound_index),
            _ => {
                return Err(anyhow!(
                    "the init %{} must be defined in the preheader '{}' for \
                     the span guard to reach it",
                    usize::from(init),
                    preheader.as_str()
                ))
            }
        },
    };

    // Inner cycles: the `speculate_region` counter shape, guarded unless an
    // earlier entry already did.
    let body_region: Vec<Label> = flood
        .iter()
        .filter(|l| *l != &latch && Some(*l) != brk.as_ref())
        .cloned()
        .collect();
    let mut inner_guards: Vec<InnerGuard> = Vec::new();
    for (cycle_latch, header) in back_edges(fun, &body_region, &body_entry) {
        let cycle_bound = super::speculate_region::loop_bound(
            fun,
            &body_region,
            &cycle_latch,
            &header,
        )?;
        if super::speculate_region::is_small_constant(fun, cycle_bound) {
            continue;
        }
        if inner_guards.iter().any(|g| g.bound == cycle_bound) {
            continue;
        }
        let (def_block, def_index) = defining_site(fun, cycle_bound)
            .ok_or_else(|| anyhow!("an inner bound has no defining instruction"))?;
        let Some(def_block) = def_block else {
            return Err(anyhow!("an inner bound is defined in the entry block"));
        };
        if already_guarded(fun, cycle_bound, &Some(def_block.clone()), def_index) {
            continue;
        }
        require(
            def_block != *head
                && def_block != cont
                && def_block != latch
                && Some(&def_block) != brk.as_ref()
                && def_block != preheader
                && !(break_to.is_some() && def_block == exit),
            format!(
                "the inner bound %{} is defined in '{}', which this rule \
                 rewrites",
                usize::from(cycle_bound),
                def_block.as_str()
            ),
        )?;
        inner_guards.push(InnerGuard { bound: cycle_bound, def_block, def_index });
    }
    inner_guards.sort_by_key(|g| (g.def_block.clone(), g.def_index));

    Ok(Site {
        preheader,
        body_entry,
        exit,
        cont,
        latch,
        brk,
        join: break_to.cloned(),
        merged,
        latch_on_true,
        exit_to_join,
        exit_target,
        i_id: *i_id,
        cond_id: *cond_id,
        bound: *bound,
        init,
        guard_index,
        next_id,
        bc,
        j_phis,
        inner_guards,
    })
}

/// The masked form of one block's instructions: stores become
/// assert-load-select-store on `mask`, everything else stays. `triples`
/// supplies the minted ids, one triple per store in order.
fn masked_instructions(
    block: &Block,
    mask: LocalId,
    triples: &mut impl Iterator<Item = [LocalId; 3]>,
) -> Vec<(LocalId, Instruction)> {
    let mut out = Vec::new();
    for (id, instr) in &block.instructions {
        match instr {
            Instruction::Store { target, source } => {
                let [guard, old, sel] = triples.next().expect("a triple per store");
                out.push((guard, Instruction::AssertValueCell { target: *target }));
                out.push((old, Instruction::Load { source: *target }));
                out.push((
                    sel,
                    Instruction::Select {
                        condition: mask,
                        if_true: *source,
                        if_false: old,
                    },
                ));
                out.push((*id, Instruction::Store { target: *target, source: sel }));
            }
            _ => out.push((*id, instr.clone())),
        }
    }
    out
}

/// How many ids the trip guard mints.
fn trip_guard_len(trip: Trip) -> usize {
    match trip {
        Trip::Limit(_) => 3,
        Trip::Span(_) => 7,
    }
}

/// The guard planted in the preheader: `assert_true(bound <= limit)`, or for
/// `span` mode `assert_true(init >= 0)` plus `assert_true(bound - init <=
/// span)`.
fn trip_guard_for(s: &Site, trip: Trip, minted: &[LocalId]) -> Vec<(LocalId, Instruction)> {
    match trip {
        Trip::Limit(limit) => vec![
            (minted[0], Instruction::NumberConstant { value: Pico8Num::from_i16(limit) }),
            (
                minted[1],
                Instruction::BinaryOp {
                    left: s.bound,
                    op: BinaryOp::LessThanEqual,
                    right: minted[0],
                },
            ),
            (minted[2], Instruction::AssertTrue { value: minted[1] }),
        ],
        // `bound - init < span + 1`, strict: a Lua `for` bound may be
        // fractional (`min(15,(x+w-1)/8)` has no `flr`), and the trips are
        // `floor(bound - init) + 1`, which `<= span` on the difference would
        // over-refuse.
        Trip::Span(span) => vec![
            (minted[0], Instruction::NumberConstant { value: Pico8Num::from_i16(0) }),
            (
                minted[1],
                Instruction::BinaryOp {
                    left: s.init,
                    op: BinaryOp::GreaterThanEqual,
                    right: minted[0],
                },
            ),
            (minted[2], Instruction::AssertTrue { value: minted[1] }),
            (minted[3], Instruction::NumberConstant { value: Pico8Num::from_i16(span + 1) }),
            (
                minted[4],
                Instruction::BinaryOp { left: s.bound, op: BinaryOp::Minus, right: s.init },
            ),
            (
                minted[5],
                Instruction::BinaryOp {
                    left: minted[4],
                    op: BinaryOp::LessThan,
                    right: minted[3],
                },
            ),
            (minted[6], Instruction::AssertTrue { value: minted[5] }),
        ],
    }
}

/// The four masks appended after the continue block's instructions (or,
/// merged, the latch's). Returns them plus the (do_step, do_break) ids,
/// whose slots depend on the branch polarity.
fn mask_tail(
    s: &Site,
    active_phi: LocalId,
    minted: [LocalId; 4],
) -> (Vec<(LocalId, Instruction)>, LocalId, LocalId) {
    let [take, a, b, c] = minted;
    let mut out = vec![(
        take,
        Instruction::Select { condition: active_phi, if_true: s.cond_id, if_false: active_phi },
    )];
    if s.latch_on_true {
        // do_step = take && bc; do_break = take && !bc.
        out.push((a, Instruction::Select { condition: take, if_true: s.bc, if_false: take }));
        out.push((b, Instruction::UnaryOp { op: UnaryOp::Not, arg: s.bc }));
        out.push((c, Instruction::Select { condition: take, if_true: b, if_false: take }));
        (out, a, c)
    } else {
        // The branch breaks on true: do_step = take && !bc; do_break = take && bc.
        out.push((a, Instruction::UnaryOp { op: UnaryOp::Not, arg: s.bc }));
        out.push((b, Instruction::Select { condition: take, if_true: a, if_false: take }));
        out.push((c, Instruction::Select { condition: take, if_true: s.bc, if_false: take }));
        (out, b, c)
    }
}

/// Every rewritten block, rebuilt from the before function and the minted
/// ids. Returns (label, instructions, terminator kind); terminator ids never
/// change. Used by `apply` to mutate and by `verify` to compare.
fn rebuilt_blocks(
    fun: &FunDef,
    s: &Site,
    head: &Label,
    trip: Trip,
    ids: &Ids,
) -> Vec<(Label, Vec<(LocalId, Instruction)>, Terminator)> {
    let mut out = Vec::new();

    // Preheader: the trip guard after its operands' definitions, then the
    // `k` and `active` init constants at the end.
    let before_p = &fun.cfg.named[&s.preheader];
    let mut p = before_p.instructions.clone();
    p.splice(
        s.guard_index + 1..s.guard_index + 1,
        trip_guard_for(s, trip, &ids.outer),
    );
    p.push((ids.p_tail[0], Instruction::NumberConstant { value: Pico8Num::from_i16(0) }));
    p.push((ids.p_tail[1], Instruction::BoolConstant { value: true }));
    out.push((s.preheader.clone(), p, before_p.terminator_kind().clone()));

    // The head: the two original instructions with the phi's latch edge
    // retargeted at the masked increment, plus `k`, `active` and the uniform
    // compare the branch now uses.
    let before_h = &fun.cfg.named[head];
    let (_, i_phi) = &before_h.instructions[0];
    let Instruction::Phi { branches } = i_phi else { unreachable!("site checked") };
    let new_branches: Vec<(Label, LocalId)> = branches
        .iter()
        .map(|(label, value)| {
            if label == &s.latch {
                (label.clone(), ids.l_tail[0])
            } else {
                (label.clone(), *value)
            }
        })
        .collect();
    let [k_phi, active_phi, k_limit, u_cond] = ids.head;
    let h = vec![
        (s.i_id, Instruction::Phi { branches: new_branches }),
        (
            k_phi,
            Instruction::Phi {
                branches: vec![
                    (s.preheader.clone(), ids.p_tail[0]),
                    (s.latch.clone(), ids.l_tail[2]),
                ],
            },
        ),
        (
            active_phi,
            Instruction::Phi {
                branches: vec![
                    (s.preheader.clone(), ids.p_tail[1]),
                    (s.latch.clone(), ids.l_tail[4]),
                ],
            },
        ),
        before_h.instructions[1].clone(),
        (k_limit, Instruction::NumberConstant { value: Pico8Num::from_i16(trip.k_max()) }),
        (
            u_cond,
            Instruction::BinaryOp { left: k_phi, op: BinaryOp::LessThanEqual, right: k_limit },
        ),
    ];
    out.push((
        head.clone(),
        h,
        Terminator::ConditionalBranch {
            condition: u_cond,
            true_target: s.body_entry.clone(),
            false_target: s.exit.clone(),
        },
    ));

    // The masks. In the merged shape they sit in the latch, after its own
    // instructions; otherwise the continue block computes them and falls
    // through to the latch.
    let (masks, do_step, do_break) = mask_tail(s, active_phi, ids.cont);
    let mut l: Vec<(LocalId, Instruction)>;
    if s.merged {
        l = fun.cfg.named[&s.latch].instructions.clone();
        l.extend(masks);
    } else {
        let before_c = &fun.cfg.named[&s.cont];
        let mut c = before_c.instructions.clone();
        c.extend(masks);
        out.push((
            s.cont.clone(),
            c,
            Terminator::UnconditionalBranch { target: s.latch.clone() },
        ));

        // The latch: its own stores masked by `do_step`, the break block's
        // instructions with stores masked by `do_break`.
        let mut triples = ids.stores.iter().copied();
        l = masked_instructions(&fun.cfg.named[&s.latch], do_step, &mut triples);
        if let Some(brk) = &s.brk {
            l.extend(masked_instructions(&fun.cfg.named[brk], do_break, &mut triples));
        }
    }
    // The three loop variables.
    let [i_sel, one, k_next, false_const, active_next] = ids.l_tail;
    l.push((
        i_sel,
        Instruction::Select { condition: do_step, if_true: s.next_id, if_false: s.i_id },
    ));
    l.push((one, Instruction::NumberConstant { value: Pico8Num::from_i16(1) }));
    l.push((k_next, Instruction::BinaryOp { left: k_phi, op: BinaryOp::Plus, right: one }));
    l.push((false_const, Instruction::BoolConstant { value: false }));
    l.push((
        active_next,
        Instruction::Select { condition: do_break, if_true: false_const, if_false: active_phi },
    ));
    out.push((
        s.latch.clone(),
        l,
        Terminator::UnconditionalBranch { target: head.clone() },
    ));

    // `break_to` mode: the exit carries the break out, and the join's phis
    // swap the deleted break edge for it.
    if let Some(join) = &s.join {
        let before_x = &fun.cfg.named[&s.exit];
        let mut x = before_x.instructions.clone();
        let mut xt = ids.x_tail.iter().copied();
        // Per join phi: the value its entry will hold after the rewrite.
        let mut phi_values: Vec<LocalId> = Vec::new();
        for jp in &s.j_phis {
            let cst = xt.next().expect("a minted id per join phi");
            x.push((cst, Instruction::BoolConstant { value: jp.break_value }));
            if s.exit_to_join {
                let sel = xt.next().expect("a select per join phi");
                x.push((
                    sel,
                    Instruction::Select {
                        condition: active_phi,
                        if_true: jp.exit_value.expect("site recorded the exit value"),
                        if_false: cst,
                    },
                ));
                phi_values.push(sel);
            } else {
                phi_values.push(cst);
            }
        }
        let x_term = if s.exit_to_join {
            before_x.terminator_kind().clone()
        } else {
            Terminator::ConditionalBranch {
                condition: active_phi,
                true_target: s.exit_target.clone().expect("site recorded the exit target"),
                false_target: join.clone(),
            }
        };
        out.push((s.exit.clone(), x, x_term));

        let before_j = &fun.cfg.named[join];
        let brk_source = s.brk_source().clone();
        let mut j: Vec<(LocalId, Instruction)> = Vec::new();
        let mut next_phi = 0usize;
        for (id, instr) in &before_j.instructions {
            match instr {
                Instruction::Phi { branches } => {
                    let replacement = phi_values[next_phi];
                    next_phi += 1;
                    let new_branches: Vec<(Label, LocalId)> = if s.exit_to_join {
                        branches
                            .iter()
                            .filter(|(l, _)| l != &brk_source)
                            .map(|(l, v)| {
                                if l == &s.exit {
                                    (l.clone(), replacement)
                                } else {
                                    (l.clone(), *v)
                                }
                            })
                            .collect()
                    } else {
                        branches
                            .iter()
                            .map(|(l, v)| {
                                if l == &brk_source {
                                    (s.exit.clone(), replacement)
                                } else {
                                    (l.clone(), *v)
                                }
                            })
                            .collect()
                    };
                    j.push((*id, Instruction::Phi { branches: new_branches }));
                }
                _ => j.push((*id, instr.clone())),
            }
        }
        out.push((join.clone(), j, before_j.terminator_kind().clone()));
    }

    out
}

/// The recipe's `limit`/`span` fields, exactly one of which must be given.
fn trip_of(limit: Option<i16>, span: Option<i16>) -> Result<Trip> {
    match (limit, span) {
        (Some(limit), None) => Ok(Trip::Limit(limit)),
        (None, Some(span)) => Ok(Trip::Span(span)),
        _ => Err(anyhow!("exactly one of `limit` and `span` must be given")),
    }
}

pub fn apply(
    program: &mut Program,
    function: &str,
    head: &str,
    limit: Option<i16>,
    span: Option<i16>,
    break_to: Option<&str>,
) -> Result<usize> {
    let head = &Label::from(head.to_string());
    let trip = trip_of(limit, span)?;
    let break_to = break_to.map(|label| Label::from(label.to_string()));
    let fun = program.get(function)?;
    let s = site(fun, function, head, trip, break_to.as_ref())?;

    let mut alloc = LocalIdAllocator::for_function(fun);
    let stores = store_count(&fun.cfg.named[&s.latch])
        + s.brk.as_ref().map_or(0, |brk| store_count(&fun.cfg.named[brk]));
    let x_count = s.j_phis.len() * if s.exit_to_join { 2 } else { 1 };
    let ids = Ids {
        inner: s.inner_guards.iter().map(|_| [alloc.fresh(), alloc.fresh(), alloc.fresh()]).collect(),
        outer: (0..trip_guard_len(trip)).map(|_| alloc.fresh()).collect(),
        p_tail: [alloc.fresh(), alloc.fresh()],
        head: [alloc.fresh(), alloc.fresh(), alloc.fresh(), alloc.fresh()],
        cont: [alloc.fresh(), alloc.fresh(), alloc.fresh(), alloc.fresh()],
        stores: (0..stores).map(|_| [alloc.fresh(), alloc.fresh(), alloc.fresh()]).collect(),
        l_tail: [alloc.fresh(), alloc.fresh(), alloc.fresh(), alloc.fresh(), alloc.fresh()],
        x_tail: (0..x_count).map(|_| alloc.fresh()).collect(),
    };

    let rebuilt = rebuilt_blocks(fun, &s, head, trip, &ids);
    let changed: usize = rebuilt.iter().map(|(_, instrs, _)| instrs.len()).sum();

    let fun = program.get_mut(function)?;
    for (label, instructions, terminator) in rebuilt {
        let block = fun.cfg.named.get_mut(&label).unwrap();
        block.instructions = instructions;
        block.terminator.1 = terminator;
    }
    // Inner guards, descending index per block so splices do not shift.
    let mut ordered: Vec<(usize, &InnerGuard)> = s.inner_guards.iter().enumerate().collect();
    ordered.sort_by_key(|(_, g)| (g.def_block.clone(), std::cmp::Reverse(g.def_index)));
    for (index, guard) in ordered {
        let block = fun.cfg.named.get_mut(&guard.def_block).unwrap();
        block.instructions.splice(
            guard.def_index + 1..guard.def_index + 1,
            guard_for(guard.bound, &ids.inner[index]),
        );
    }
    if let Some(brk) = &s.brk {
        fun.cfg.named.remove(brk);
    }
    fun.cfg.slots = std::sync::Arc::new(SlotMap::identity());
    Ok(changed)
}

/// Independent check: re-derives the site from the before program, learns
/// the minted ids from the after program at positions computed from the
/// before program alone, and requires the after program to be exactly the
/// rebuilt blocks, the spliced guards, the deleted break block - and
/// nothing else.
pub fn verify(
    before: &Program,
    after: &Program,
    function: &str,
    head: &str,
    limit: Option<i16>,
    span: Option<i16>,
    break_to: Option<&str>,
) -> Result<()> {
    let head = &Label::from(head.to_string());
    let trip = trip_of(limit, span)?;
    let break_to = break_to.map(|label| Label::from(label.to_string()));
    let before_fun = before.get(function)?;
    let s = site(before_fun, function, head, trip, break_to.as_ref())?;
    let after_fun = after.get(function)?;

    require(
        before.functions.len() == after.functions.len(),
        "mask_loop changed the set of functions",
    )?;
    for (other, before_other) in &before.functions {
        if other.as_str() == function {
            continue;
        }
        require(
            after.functions.get(other) == Some(before_other),
            format!("mask_loop on {} also changed {}", function, other.as_str()),
        )?;
    }
    let deleted = usize::from(s.brk.is_some());
    require(
        after_fun.cfg.named.len() == before_fun.cfg.named.len() - deleted
            && s.brk.as_ref().is_none_or(|brk| after_fun.cfg.named.get(brk).is_none()),
        match &s.brk {
            Some(brk) => format!(
                "mask_loop must delete exactly the break block '{}'",
                brk.as_str()
            ),
            None => "mask_loop must delete no block in the merged shape".to_string(),
        },
    )?;
    require(
        before_fun.arg_ids == after_fun.arg_ids
            && before_fun.capture_ids == after_fun.capture_ids,
        "mask_loop changed the function signature",
    )?;

    // Learn the minted ids at positions computed from the before program.
    let mut before_ids: FxHashSet<LocalId> = FxHashSet::default();
    before_ids.extend(before_fun.arg_ids.iter().flatten().copied());
    before_ids.extend(before_fun.capture_ids.iter().copied());
    for block in before_fun.cfg.iter_blocks() {
        before_ids.extend(block.instructions.iter().map(|(id, _)| *id));
        before_ids.insert(block.terminator_id());
    }
    let mut minted_seen: FxHashSet<LocalId> = FxHashSet::default();
    let mut learn = |label: &Label, position: usize| -> Result<LocalId> {
        let block = after_fun
            .cfg
            .named
            .get(label)
            .ok_or_else(|| anyhow!("mask_loop removed '{}'", label.as_str()))?;
        let (id, _) = block.instructions.get(position).ok_or_else(|| {
            anyhow!("'{}' is too short for the expected emission", label.as_str())
        })?;
        require(
            !before_ids.contains(id),
            format!("minted id %{} already existed before", usize::from(*id)),
        )?;
        require(
            minted_seen.insert(*id),
            format!("minted id %{} is used twice", usize::from(*id)),
        )?;
        Ok(*id)
    };
    let learn_n = |learn: &mut dyn FnMut(&Label, usize) -> Result<LocalId>,
                   label: &Label,
                   start: usize,
                   n: usize|
     -> Result<Vec<LocalId>> {
        (0..n).map(|k| learn(label, start + k)).collect()
    };

    // Inner guards first (their blocks are otherwise untouched).
    let mut guards_of: std::collections::BTreeMap<String, Vec<(usize, &InnerGuard)>> =
        Default::default();
    for (index, guard) in s.inner_guards.iter().enumerate() {
        guards_of
            .entry(guard.def_block.as_str().to_string())
            .or_default()
            .push((index, guard));
    }
    let mut inner: Vec<[LocalId; 3]> = vec![[LocalId::from(0); 3]; s.inner_guards.len()];
    for list in guards_of.values_mut() {
        list.sort_by_key(|(_, g)| g.def_index);
        for (n, (index, guard)) in list.iter().enumerate() {
            let position = guard.def_index + 1 + 3 * n;
            let got = learn_n(&mut learn, &guard.def_block, position, 3)?;
            inner[*index] = [got[0], got[1], got[2]];
        }
    }

    // The trip guard, right after its operands in the preheader; then the
    // preheader tail, shifted by the guard.
    let guard_len = trip_guard_len(trip);
    let outer = learn_n(&mut learn, &s.preheader, s.guard_index + 1, guard_len)?;
    let before_p_len = before_fun.cfg.named[&s.preheader].instructions.len();
    let p_tail = learn_n(&mut learn, &s.preheader, before_p_len + guard_len, 2)?;

    // Head: fixed positions around the two originals.
    let head_ids = vec![
        learn(head, 1)?,
        learn(head, 2)?,
        learn(head, 4)?,
        learn(head, 5)?,
    ];

    // The masks, then the latch: a triple ahead of each store, then the
    // loop-variable tail. In the merged shape everything sits in the latch,
    // after its own (store-free) instructions.
    let (cont_ids, stores, l_tail);
    if s.merged {
        let before_l_len = before_fun.cfg.named[&s.latch].instructions.len();
        cont_ids = learn_n(&mut learn, &s.latch, before_l_len, 4)?;
        stores = Vec::new();
        l_tail = learn_n(&mut learn, &s.latch, before_l_len + 4, 5)?;
    } else {
        let before_c_len = before_fun.cfg.named[&s.cont].instructions.len();
        cont_ids = learn_n(&mut learn, &s.cont, before_c_len, 4)?;
        let mut learned: Vec<[LocalId; 3]> = Vec::new();
        let mut position = 0usize;
        for source in std::iter::once(&s.latch).chain(s.brk.as_ref()) {
            for (_, instr) in &before_fun.cfg.named[source].instructions {
                if matches!(instr, Instruction::Store { .. }) {
                    let got = learn_n(&mut learn, &s.latch, position, 3)?;
                    learned.push([got[0], got[1], got[2]]);
                    position += 4;
                } else {
                    position += 1;
                }
            }
        }
        stores = learned;
        l_tail = learn_n(&mut learn, &s.latch, position, 5)?;
    }

    // `break_to` mode: the exit's minted constants and selects.
    let x_tail = if s.join.is_some() {
        let before_x_len = before_fun.cfg.named[&s.exit].instructions.len();
        let x_count = s.j_phis.len() * if s.exit_to_join { 2 } else { 1 };
        learn_n(&mut learn, &s.exit, before_x_len, x_count)?
    } else {
        Vec::new()
    };

    let ids = Ids {
        inner,
        outer,
        p_tail: [p_tail[0], p_tail[1]],
        head: [head_ids[0], head_ids[1], head_ids[2], head_ids[3]],
        cont: [cont_ids[0], cont_ids[1], cont_ids[2], cont_ids[3]],
        stores,
        l_tail: [l_tail[0], l_tail[1], l_tail[2], l_tail[3], l_tail[4]],
        x_tail,
    };

    // The expectation: rebuilt blocks, guard splices, everything else
    // untouched. Splices go descending per block so indices stay valid.
    let mut expected: std::collections::HashMap<Label, (Vec<(LocalId, Instruction)>, Terminator)> =
        rebuilt_blocks(before_fun, &s, head, trip, &ids)
            .into_iter()
            .map(|(label, instrs, term)| (label, (instrs, term)))
            .collect();
    let mut ordered: Vec<(usize, &InnerGuard)> = s.inner_guards.iter().enumerate().collect();
    ordered.sort_by_key(|(_, g)| (g.def_block.clone(), std::cmp::Reverse(g.def_index)));
    for (index, guard) in ordered {
        let before_block = &before_fun.cfg.named[&guard.def_block];
        let (instrs, _) = expected.entry(guard.def_block.clone()).or_insert_with(|| {
            (
                before_block.instructions.clone(),
                before_block.terminator_kind().clone(),
            )
        });
        instrs.splice(
            guard.def_index + 1..guard.def_index + 1,
            guard_for(guard.bound, &ids.inner[index]),
        );
    }

    require(
        fun_entry_unchanged(before_fun, after_fun),
        "mask_loop changed the entry block",
    )?;
    for (label, before_block) in &before_fun.cfg.named {
        if Some(label) == s.brk.as_ref() {
            continue;
        }
        let after_block = after_fun.cfg.named.get(label).ok_or_else(|| {
            anyhow!("mask_loop removed block '{}'", label.as_str())
        })?;
        require(
            before_block.terminator_id() == after_block.terminator_id(),
            format!("mask_loop changed the terminator id of '{}'", label.as_str()),
        )?;
        require(
            before_block.hint_normalize == after_block.hint_normalize,
            format!("mask_loop changed hint_normalize of '{}'", label.as_str()),
        )?;
        let (want_instrs, want_term) = match expected.get(label) {
            Some((instrs, term)) => (instrs.clone(), term.clone()),
            None => (
                before_block.instructions.clone(),
                before_block.terminator_kind().clone(),
            ),
        };
        require(
            want_instrs.len() == after_block.instructions.len(),
            format!(
                "mask_loop changed the length of '{}': want {}, got {}",
                label.as_str(),
                want_instrs.len(),
                after_block.instructions.len()
            ),
        )?;
        for (pos, ((want_id, want_instr), (after_id, after_instr))) in want_instrs
            .iter()
            .zip(after_block.instructions.iter())
            .enumerate()
        {
            require(
                want_id == after_id && want_instr == after_instr,
                format!(
                    "mask_loop: '{}' differs at position {}:\n  want {} = {}\n  got  {} = {}",
                    label.as_str(),
                    pos,
                    super::super::print::local_name(*want_id),
                    format_instruction(want_instr),
                    super::super::print::local_name(*after_id),
                    format_instruction(after_instr)
                ),
            )?;
        }
        require(
            &want_term == after_block.terminator_kind(),
            format!("mask_loop: the terminator of '{}' is wrong", label.as_str()),
        )?;
    }
    Ok(())
}

fn fun_entry_unchanged(before: &FunDef, after: &FunDef) -> bool {
    before.cfg.entry == after.cfg.entry
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Cfg, GlobalId};
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
        Block { instructions, terminator: (id(terminator_id), terminator), hint_normalize: false }
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

    /// entry -> pre.
    /// pre:  %20 = 3 (bound); %21 = 0 (init); %22 = 1 (step);
    ///       %25 = x cell; %27 = y cell; %26 = 7 -> head.
    /// head: %1 = phi [pre: %21, latch: %6]; %2 = %1 <= %20;
    ///       br %2 ? cont : exit.
    /// cont: %4 = true; br %4 ? latch : brk.
    /// latch: store %25 <- %26; %6 = %1 + %22 -> head.
    /// brk:  store %27 <- %26 -> exit.
    /// exit: return.
    fn pixel_loop_program() -> Program {
        let entry = block(vec![], 900, br("pre"));
        let pre = block(
            vec![
                (id(20), num(3)),
                (id(21), num(0)),
                (id(22), num(1)),
                (
                    id(25),
                    Instruction::GetField {
                        receiver: id(0),
                        field: "x".to_string(),
                        create_if_missing: false,
                    },
                ),
                (
                    id(27),
                    Instruction::GetField {
                        receiver: id(0),
                        field: "y".to_string(),
                        create_if_missing: false,
                    },
                ),
                (id(26), num(7)),
            ],
            901,
            br("head"),
        );
        let head = block(
            vec![
                (
                    id(1),
                    Instruction::Phi {
                        branches: vec![(label("pre"), id(21)), (label("latch"), id(6))],
                    },
                ),
                (
                    id(2),
                    Instruction::BinaryOp {
                        left: id(1),
                        op: BinaryOp::LessThanEqual,
                        right: id(20),
                    },
                ),
            ],
            902,
            br_if(2, "cont", "exit"),
        );
        let cont = block(
            vec![(id(4), Instruction::BoolConstant { value: true })],
            903,
            br_if(4, "latch", "brk"),
        );
        let latch = block(
            vec![
                (id(12), Instruction::Store { target: id(25), source: id(26) }),
                (
                    id(6),
                    Instruction::BinaryOp { left: id(1), op: BinaryOp::Plus, right: id(22) },
                ),
            ],
            904,
            br("head"),
        );
        let brk = block(
            vec![(id(13), Instruction::Store { target: id(27), source: id(26) })],
            905,
            br("exit"),
        );
        let exit = block(vec![], 906, Terminator::Return { value: None });

        let mut named = crate::ir::new_label_map();
        named.insert(label("pre"), pre);
        named.insert(label("head"), head);
        named.insert(label("cont"), cont);
        named.insert(label("latch"), latch);
        named.insert(label("brk"), brk);
        named.insert(label("exit"), exit);
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![Some(id(0))],
            cfg: Cfg::new(entry, named),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions }
    }

    #[test]
    fn masks_a_pixel_loop() {
        let mut p = pixel_loop_program();
        let before = p.clone();
        apply(&mut p, "f", "head", Some(5), None, None).unwrap();
        verify(&before, &p, "f", "head", Some(5), None, None).unwrap();

        let fun = p.get("f").unwrap();
        // The break block is gone; the loop is head -> cont -> latch -> head.
        assert!(fun.cfg.named.get(&label("brk")).is_none());
        assert_eq!(fun.cfg.named[&label("cont")].terminator_kind(), &br("latch"));
        // The head branches on a fresh uniform counter, not the per-lane one.
        let head = &fun.cfg.named[&label("head")];
        let Terminator::ConditionalBranch { condition, .. } = head.terminator_kind()
        else {
            panic!("head must still branch")
        };
        assert_ne!(condition, &id(2));
        // Three phis now: i, k, active.
        let phis = head
            .instructions
            .iter()
            .filter(|(_, i)| matches!(i, Instruction::Phi { .. }))
            .count();
        assert_eq!(phis, 3);
        // Both stores live in the latch now, each behind a select.
        let latch = &fun.cfg.named[&label("latch")];
        let stores: Vec<&Instruction> = latch
            .instructions
            .iter()
            .filter(|(_, i)| matches!(i, Instruction::Store { .. }))
            .map(|(_, i)| i)
            .collect();
        assert_eq!(stores.len(), 2);
        for store in stores {
            let Instruction::Store { source, .. } = store else { unreachable!() };
            let feeding = latch.instructions.iter().find(|(id, _)| id == source).unwrap();
            assert!(
                matches!(feeding.1, Instruction::Select { .. }),
                "store must be fed by a masking select"
            );
        }
        // The preheader guards the bound against the limit.
        let pre = &fun.cfg.named[&label("pre")];
        assert!(pre
            .instructions
            .iter()
            .any(|(_, i)| matches!(i, Instruction::AssertTrue { .. })));
    }

    /// The exit loses its break edge, so phis there would dangle.
    #[test]
    fn refuses_an_exit_with_phis() {
        let mut p = pixel_loop_program();
        {
            let fun = p.get_mut("f").unwrap();
            let exit = fun.cfg.named.get_mut(&label("exit")).unwrap();
            exit.instructions.push((
                id(50),
                Instruction::Phi { branches: vec![(label("head"), id(21)), (label("brk"), id(26))] },
            ));
        }
        let error = apply(&mut p, "f", "head", Some(5), None, None).unwrap_err().to_string();
        assert!(error.contains("has phis"), "{}", error);
    }

    /// A loop value used outside the loop would observe the masked garbage.
    #[test]
    fn refuses_a_loop_value_used_outside() {
        let mut p = pixel_loop_program();
        {
            let fun = p.get_mut("f").unwrap();
            let exit = fun.cfg.named.get_mut(&label("exit")).unwrap();
            exit.terminator.1 = Terminator::Return { value: Some(id(6)) };
        }
        let error = apply(&mut p, "f", "head", Some(5), None, None).unwrap_err().to_string();
        assert!(error.contains("used outside"), "{}", error);
    }

    /// The latch must sit on the branch's true side.
    #[test]
    fn refuses_a_latch_on_the_false_side() {
        let mut p = pixel_loop_program();
        {
            let fun = p.get_mut("f").unwrap();
            let cont = fun.cfg.named.get_mut(&label("cont")).unwrap();
            cont.terminator.1 = br_if(4, "brk", "latch").clone();
        }
        let error = apply(&mut p, "f", "head", Some(5), None, None).unwrap_err().to_string();
        assert!(error.contains("on true"), "{}", error);
    }

    /// A call in the body cannot be masked.
    #[test]
    fn refuses_an_unmaskable_body() {
        let mut p = pixel_loop_program();
        {
            let fun = p.get_mut("f").unwrap();
            let cont = fun.cfg.named.get_mut(&label("cont")).unwrap();
            cont.instructions.push((
                id(51),
                Instruction::Call { closure: id(26), args: vec![] },
            ));
        }
        let error = apply(&mut p, "f", "head", Some(5), None, None).unwrap_err().to_string();
        assert!(error.contains("not maskable"), "{}", error);
    }

    /// The bound must live in the preheader, where the guard goes.
    #[test]
    fn refuses_a_bound_defined_elsewhere() {
        let mut p = pixel_loop_program();
        {
            let fun = p.get_mut("f").unwrap();
            let pre = fun.cfg.named.get_mut(&label("pre")).unwrap();
            let bound = pre.instructions.remove(0);
            fun.cfg.entry.instructions.push(bound);
        }
        let error = apply(&mut p, "f", "head", Some(5), None, None).unwrap_err().to_string();
        assert!(error.contains("preheader"), "{}", error);
    }

    /// The verifier must reject an applier that forgot a store's mask.
    #[test]
    fn verify_rejects_an_unmasked_store() {
        let mut p = pixel_loop_program();
        let before = p.clone();
        apply(&mut p, "f", "head", Some(5), None, None).unwrap();
        {
            let fun = p.get_mut("f").unwrap();
            let latch = fun.cfg.named.get_mut(&label("latch")).unwrap();
            for (sid, instr) in latch.instructions.iter_mut() {
                if *sid == id(12) {
                    *instr = Instruction::Store { target: id(25), source: id(26) };
                }
            }
        }
        let error = verify(&before, &p, "f", "head", Some(5), None, None).unwrap_err().to_string();
        assert!(error.contains("differs"), "{}", error);
    }

    /// ...or one that kept the break block around.
    #[test]
    fn verify_rejects_a_kept_break_block() {
        let mut p = pixel_loop_program();
        let before = p.clone();
        apply(&mut p, "f", "head", Some(5), None, None).unwrap();
        {
            let fun = p.get_mut("f").unwrap();
            let brk = before.get("f").unwrap().cfg.named[&label("brk")].clone();
            fun.cfg.named.insert(label("brk"), brk);
        }
        let error = verify(&before, &p, "f", "head", Some(5), None, None).unwrap_err().to_string();
        assert!(error.contains("break block"), "{}", error);
    }

    #[test]
    fn classic_refuses_a_non_constant_init() {
        let mut p = pixel_loop_program();
        let fun = p.functions.values_mut().next().unwrap();
        let pre = fun.cfg.named.get_mut(&label("pre")).unwrap();
        // The init %21 becomes `%20 + %20`: per-lane as far as the rule knows.
        pre.instructions[1].1 =
            Instruction::BinaryOp { left: id(20), op: BinaryOp::Plus, right: id(20) };
        let error = apply(&mut p, "f", "head", Some(5), None, None).unwrap_err().to_string();
        assert!(error.contains("constant >= 0"), "{}", error);
    }

    /// entry -> dispatch; dispatch: %30 = true; br %30 ? pre : other.
    /// other: %16 = false -> join.
    /// pre: %21 = 0 (init); %20 = 3 (bound); %22 = 1 -> head.
    /// head: %1 = phi [pre: %21, latch: %6]; %2 = %1 <= %20;
    ///       br %2 ? body : exit.
    /// body: %4 = true; br %4 ? brk : latch.          <- breaks on true
    /// brk:  %14 = true -> join.                      <- over the exit's head
    /// latch: %6 = %1 + %22 -> head.
    /// exit: -> after.
    /// after: return.
    /// join: %15 = phi [brk: %14, other: %16]; return %15.
    fn breaking_loop_program() -> Program {
        let entry = block(vec![], 900, br("dispatch"));
        let dispatch = block(
            vec![(id(30), Instruction::BoolConstant { value: true })],
            907,
            br_if(30, "pre", "other"),
        );
        let other = block(
            vec![(id(16), Instruction::BoolConstant { value: false })],
            908,
            br("join"),
        );
        let pre = block(
            vec![(id(21), num(0)), (id(20), num(3)), (id(22), num(1))],
            901,
            br("head"),
        );
        let head = block(
            vec![
                (
                    id(1),
                    Instruction::Phi {
                        branches: vec![(label("pre"), id(21)), (label("latch"), id(6))],
                    },
                ),
                (
                    id(2),
                    Instruction::BinaryOp {
                        left: id(1),
                        op: BinaryOp::LessThanEqual,
                        right: id(20),
                    },
                ),
            ],
            902,
            br_if(2, "body", "exit"),
        );
        let body = block(
            vec![(id(4), Instruction::BoolConstant { value: true })],
            903,
            br_if(4, "brk", "latch"),
        );
        let brk = block(
            vec![(id(14), Instruction::BoolConstant { value: true })],
            905,
            br("join"),
        );
        let latch = block(
            vec![(
                id(6),
                Instruction::BinaryOp { left: id(1), op: BinaryOp::Plus, right: id(22) },
            )],
            904,
            br("head"),
        );
        let exit = block(vec![], 906, br("after"));
        let after = block(vec![], 909, Terminator::Return { value: None });
        let join = block(
            vec![(
                id(15),
                Instruction::Phi {
                    branches: vec![(label("brk"), id(14)), (label("other"), id(16))],
                },
            )],
            910,
            Terminator::Return { value: Some(id(15)) },
        );

        let mut named = crate::ir::new_label_map();
        named.insert(label("dispatch"), dispatch);
        named.insert(label("other"), other);
        named.insert(label("pre"), pre);
        named.insert(label("head"), head);
        named.insert(label("body"), body);
        named.insert(label("brk"), brk);
        named.insert(label("latch"), latch);
        named.insert(label("exit"), exit);
        named.insert(label("after"), after);
        named.insert(label("join"), join);
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![Some(id(0))],
            cfg: Cfg::new(entry, named),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions }
    }

    #[test]
    fn span_and_break_to_reroute_a_two_level_break() {
        let mut p = breaking_loop_program();
        let before = p.clone();
        apply(&mut p, "f", "head", None, Some(2), Some("join")).unwrap();
        verify(&before, &p, "f", "head", None, Some(2), Some("join")).unwrap();

        let fun = p.get("f").unwrap();
        // The break block is gone; its constant moved into the latch.
        assert!(fun.cfg.named.get(&label("brk")).is_none());
        // The preheader grew the span guard: init >= 0 and bound - init <= 2.
        let pre = &fun.cfg.named[&label("pre")];
        assert_eq!(pre.instructions.len(), 3 + 7 + 2);
        assert_eq!(
            pre.instructions[3].1,
            Instruction::BinaryOp {
                left: id(21),
                op: BinaryOp::GreaterThanEqual,
                right: pre.instructions[2].0,
            }
        );
        assert_eq!(
            pre.instructions[6].1,
            Instruction::BinaryOp { left: id(20), op: BinaryOp::Minus, right: id(21) }
        );
        // The exit branches on `active`: back out on true, to the join on
        // false, carrying a minted copy of the break constant.
        let exit = &fun.cfg.named[&label("exit")];
        assert_eq!(exit.instructions.len(), 1);
        let (cst, minted) = &exit.instructions[0];
        assert_eq!(*minted, Instruction::BoolConstant { value: true });
        let head_block = &fun.cfg.named[&label("head")];
        let active_phi = head_block.instructions[2].0;
        assert_eq!(
            exit.terminator_kind(),
            &Terminator::ConditionalBranch {
                condition: active_phi,
                true_target: label("after"),
                false_target: label("join"),
            }
        );
        // The join's phi swapped the break edge for the exit.
        let join = &fun.cfg.named[&label("join")];
        let Instruction::Phi { branches } = &join.instructions[0].1 else { panic!() };
        assert_eq!(
            branches,
            &vec![(label("exit"), *cst), (label("other"), id(16))]
        );
    }

    #[test]
    fn break_to_refuses_a_non_constant_break_value() {
        let mut p = breaking_loop_program();
        let fun = p.functions.values_mut().next().unwrap();
        let brk = fun.cfg.named.get_mut(&label("brk")).unwrap();
        brk.instructions[0].1 =
            Instruction::Select { condition: id(4), if_true: id(4), if_false: id(4) };
        let error = apply(&mut p, "f", "head", None, Some(2), Some("join"))
            .unwrap_err()
            .to_string();
        assert!(error.contains("not a bool constant"), "{}", error);
    }

    /// The merged shape the second application sees: the latch itself
    /// branches between the head and the join, and the exit already falls
    /// through to the join.
    ///
    /// entry -> dispatch; dispatch: %30 = true; br %30 ? pre : other.
    /// other: %16 = false -> join.
    /// pre: %21 = 0 (init); %20 = 3 (bound); %22 = 1 -> head.
    /// head: %1 = phi [pre: %21, latch: %6]; %2 = %1 <= %20;
    ///       br %2 ? latch : exit.
    /// latch: %14 = true; %5 = true; %6 = %1 + %22; br %5 ? head : join.
    /// exit: %17 = false -> join.
    /// join: %15 = phi [latch: %14, exit: %17, other: %16]; return %15.
    fn merged_loop_program() -> Program {
        let entry = block(vec![], 900, br("dispatch"));
        let dispatch = block(
            vec![(id(30), Instruction::BoolConstant { value: true })],
            907,
            br_if(30, "pre", "other"),
        );
        let other = block(
            vec![(id(16), Instruction::BoolConstant { value: false })],
            908,
            br("join"),
        );
        let pre = block(
            vec![(id(21), num(0)), (id(20), num(3)), (id(22), num(1))],
            901,
            br("head"),
        );
        let head = block(
            vec![
                (
                    id(1),
                    Instruction::Phi {
                        branches: vec![(label("pre"), id(21)), (label("latch"), id(6))],
                    },
                ),
                (
                    id(2),
                    Instruction::BinaryOp {
                        left: id(1),
                        op: BinaryOp::LessThanEqual,
                        right: id(20),
                    },
                ),
            ],
            902,
            br_if(2, "latch", "exit"),
        );
        let latch = block(
            vec![
                (id(14), Instruction::BoolConstant { value: true }),
                (id(5), Instruction::BoolConstant { value: true }),
                (
                    id(6),
                    Instruction::BinaryOp { left: id(1), op: BinaryOp::Plus, right: id(22) },
                ),
            ],
            904,
            br_if(5, "head", "join"),
        );
        let exit = block(
            vec![(id(17), Instruction::BoolConstant { value: false })],
            906,
            br("join"),
        );
        let join = block(
            vec![(
                id(15),
                Instruction::Phi {
                    branches: vec![
                        (label("latch"), id(14)),
                        (label("exit"), id(17)),
                        (label("other"), id(16)),
                    ],
                },
            )],
            910,
            Terminator::Return { value: Some(id(15)) },
        );

        let mut named = crate::ir::new_label_map();
        named.insert(label("dispatch"), dispatch);
        named.insert(label("other"), other);
        named.insert(label("pre"), pre);
        named.insert(label("head"), head);
        named.insert(label("latch"), latch);
        named.insert(label("exit"), exit);
        named.insert(label("join"), join);
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![Some(id(0))],
            cfg: Cfg::new(entry, named),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions }
    }

    #[test]
    fn merged_break_to_needs_no_branch_at_all() {
        let mut p = merged_loop_program();
        let before = p.clone();
        apply(&mut p, "f", "head", None, Some(2), Some("join")).unwrap();
        verify(&before, &p, "f", "head", None, Some(2), Some("join")).unwrap();

        let fun = p.get("f").unwrap();
        // No block deleted; the latch's branch became the unconditional back
        // edge, its instructions followed by the masks and the loop tail.
        assert!(fun.cfg.named.get(&label("latch")).is_some());
        let latch = &fun.cfg.named[&label("latch")];
        assert_eq!(latch.terminator_kind(), &br("head"));
        assert_eq!(latch.instructions.len(), 3 + 4 + 5);
        // The exit stays unconditional and merges the break value by select
        // on `active`.
        let exit = &fun.cfg.named[&label("exit")];
        assert_eq!(exit.terminator_kind(), &br("join"));
        assert_eq!(exit.instructions.len(), 1 + 2);
        let (cst, minted_cst) = &exit.instructions[1];
        assert_eq!(*minted_cst, Instruction::BoolConstant { value: true });
        let head_block = &fun.cfg.named[&label("head")];
        let active_phi = head_block.instructions[2].0;
        let (sel, minted_sel) = &exit.instructions[2];
        assert_eq!(
            *minted_sel,
            Instruction::Select { condition: active_phi, if_true: id(17), if_false: *cst }
        );
        // The join lost the break edge and reads the merged value instead.
        let join = &fun.cfg.named[&label("join")];
        let Instruction::Phi { branches } = &join.instructions[0].1 else { panic!() };
        assert_eq!(
            branches,
            &vec![(label("exit"), *sel), (label("other"), id(16))]
        );
    }

    #[test]
    fn merged_refuses_a_store_in_the_latch() {
        let mut p = merged_loop_program();
        let fun = p.functions.values_mut().next().unwrap();
        let latch = fun.cfg.named.get_mut(&label("latch")).unwrap();
        latch
            .instructions
            .push((id(40), Instruction::Store { target: id(0), source: id(14) }));
        let error = apply(&mut p, "f", "head", None, Some(2), Some("join"))
            .unwrap_err()
            .to_string();
        assert!(error.contains("merged latch"), "{}", error);
    }
}
