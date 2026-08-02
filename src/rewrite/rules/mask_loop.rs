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
//! # What this rule refuses, deliberately
//!
//! A latch that is not the branch's true side; a counter that is not
//! `phi` + `<=` + `+1`; non-store non-speculatable instructions anywhere in
//! the loop; phis in the latch, break or exit; loop values used outside;
//! `hint_normalize` or `return` inside; inner cycles without the counter
//! shape; guard sites inside the blocks this rule rewrites. Each is a shape
//! the two pixel loops do not have.

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

/// The shape, re-derived identically by `apply` and `verify`.
struct Site {
    preheader: Label,
    body_entry: Label,
    exit: Label,
    cont: Label,
    latch: Label,
    brk: Label,
    /// The counter phi and its compare, both in the head.
    i_id: LocalId,
    cond_id: LocalId,
    bound: LocalId,
    /// Where in the preheader the outer bound is defined, for the limit
    /// guard.
    bound_index: usize,
    /// The raw `i + 1` in the latch.
    next_id: LocalId,
    /// The continue condition: `cont` branches to the latch on true.
    bc: LocalId,
    inner_guards: Vec<InnerGuard>,
}

/// Every minted id, in the order `apply` allocates them and `verify` learns
/// them.
struct Ids {
    /// One triple per inner guard, in `site.inner_guards` order.
    inner: Vec<[LocalId; 3]>,
    /// Outer bound guard: limit constant, `bound <= limit`, assert.
    outer: [LocalId; 3],
    /// Preheader tail: `k` init constant, `active` init constant.
    p_tail: [LocalId; 2],
    /// Head: `k` phi, `active` phi, limit constant, `k <= limit`.
    head: [LocalId; 4],
    /// Cont tail: take, do_step, `!bc`, do_break.
    cont: [LocalId; 4],
    /// One triple per store (latch's stores first, then the break's, each in
    /// block order): assert_value_cell, load, select.
    stores: Vec<[LocalId; 3]>,
    /// Latch tail: masked `i`, `1` constant, `k + 1`, `false` constant,
    /// masked `active`.
    l_tail: [LocalId; 5],
}

fn store_count(block: &Block) -> usize {
    block
        .instructions
        .iter()
        .filter(|(_, i)| matches!(i, Instruction::Store { .. }))
        .count()
}

fn site(fun: &FunDef, function: &str, head: &Label, limit: i16) -> Result<Site> {
    require(limit >= 0, "the limit must not be negative")?;
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

    // The loop's blocks: flood from the body entry, stopping at the head and
    // the exit. The sources of those stopped edges are the latch and the
    // break block.
    let mut flood: Vec<Label> = Vec::new();
    let mut seen: FxHashSet<Label> = FxHashSet::default();
    let mut to_head: Vec<Label> = Vec::new();
    let mut to_exit: Vec<Label> = Vec::new();
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
    let [latch] = to_head.as_slice() else {
        return Err(anyhow!(
            "the loop must reach its head through exactly one latch, found {:?}",
            to_head.iter().map(|l| l.as_str()).collect::<Vec<_>>()
        ));
    };
    let [brk] = to_exit.as_slice() else {
        return Err(anyhow!(
            "the loop must reach the exit through exactly one break block, \
             found {:?}",
            to_exit.iter().map(|l| l.as_str()).collect::<Vec<_>>()
        ));
    };
    let latch = latch.clone();
    let brk = brk.clone();
    require(&latch != &brk, "the latch and the break block must differ")?;

    // The continue/break branch: one block conditionally picks the latch on
    // true and the break block on false, and is their only predecessor.
    let preds = predecessors(&fun.cfg);
    let cont = match preds.get(&Some(latch.clone())).map(|p| p.as_slice()) {
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
            preds.get(&Some(brk.clone())).map(|p| p.as_slice()),
            Some([Some(p)]) if p == &cont
        ),
        format!(
            "the break block '{}' must be reached only from '{}'",
            brk.as_str(),
            cont.as_str()
        ),
    )?;
    let Terminator::ConditionalBranch {
        condition: bc,
        true_target: ct,
        false_target: cf,
    } = fun.cfg.named[&cont].terminator_kind()
    else {
        return Err(anyhow!("'{}' does not branch conditionally", cont.as_str()));
    };
    require(
        ct == &latch && cf == &brk,
        format!(
            "'{}' must branch to the latch on true and the break block on false",
            cont.as_str()
        ),
    )?;
    let bc = *bc;
    require(
        matches!(fun.cfg.named[&latch].terminator_kind(),
            Terminator::UnconditionalBranch { target } if target == head),
        format!("the latch '{}' must jump back to the head", latch.as_str()),
    )?;
    require(
        matches!(fun.cfg.named[&brk].terminator_kind(),
            Terminator::UnconditionalBranch { target } if target == &exit),
        format!("the break block '{}' must jump to the exit", brk.as_str()),
    )?;

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
    for label in &flood {
        let is_tail = label == &latch || label == &brk;
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
            used.extend(instr.get_used_locals());
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

    // The outer bound: defined in the preheader, where the limit guard has
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

    // Inner cycles: the `speculate_region` counter shape, guarded unless an
    // earlier entry already did.
    let body_region: Vec<Label> = flood
        .iter()
        .filter(|l| *l != &latch && *l != &brk)
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
                && def_block != brk
                && def_block != preheader,
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
        i_id: *i_id,
        cond_id: *cond_id,
        bound: *bound,
        bound_index,
        next_id,
        bc,
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

/// The guard planted after the outer bound: `assert_true(bound <= limit)`.
fn outer_guard_for(
    bound: LocalId,
    limit: i16,
    minted: &[LocalId; 3],
) -> Vec<(LocalId, Instruction)> {
    vec![
        (minted[0], Instruction::NumberConstant { value: Pico8Num::from_i16(limit) }),
        (
            minted[1],
            Instruction::BinaryOp {
                left: bound,
                op: BinaryOp::LessThanEqual,
                right: minted[0],
            },
        ),
        (minted[2], Instruction::AssertTrue { value: minted[1] }),
    ]
}

/// Every rewritten block, rebuilt from the before function and the minted
/// ids. Returns (label, instructions, terminator kind); terminator ids never
/// change. Used by `apply` to mutate and by `verify` to compare.
fn rebuilt_blocks(
    fun: &FunDef,
    s: &Site,
    head: &Label,
    limit: i16,
    ids: &Ids,
) -> Vec<(Label, Vec<(LocalId, Instruction)>, Terminator)> {
    let mut out = Vec::new();

    // Preheader: the outer guard after the bound's definition, then the `k`
    // and `active` init constants at the end.
    let before_p = &fun.cfg.named[&s.preheader];
    let mut p = before_p.instructions.clone();
    p.splice(
        s.bound_index + 1..s.bound_index + 1,
        outer_guard_for(s.bound, limit, &ids.outer),
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
        (k_limit, Instruction::NumberConstant { value: Pico8Num::from_i16(limit) }),
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

    // Cont: the masks, then fall through to the latch.
    let before_c = &fun.cfg.named[&s.cont];
    let mut c = before_c.instructions.clone();
    let [take, do_step, not_bc, do_break] = ids.cont;
    c.push((
        take,
        Instruction::Select { condition: active_phi, if_true: s.cond_id, if_false: active_phi },
    ));
    c.push((
        do_step,
        Instruction::Select { condition: take, if_true: s.bc, if_false: take },
    ));
    c.push((not_bc, Instruction::UnaryOp { op: UnaryOp::Not, arg: s.bc }));
    c.push((
        do_break,
        Instruction::Select { condition: take, if_true: not_bc, if_false: take },
    ));
    out.push((
        s.cont.clone(),
        c,
        Terminator::UnconditionalBranch { target: s.latch.clone() },
    ));

    // The latch: its own stores masked by `do_step`, the break block's
    // instructions with stores masked by `do_break`, then the three loop
    // variables.
    let mut triples = ids.stores.iter().copied();
    let mut l = masked_instructions(&fun.cfg.named[&s.latch], do_step, &mut triples);
    l.extend(masked_instructions(&fun.cfg.named[&s.brk], do_break, &mut triples));
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

    out
}

pub fn apply(program: &mut Program, function: &str, head: &str, limit: i16) -> Result<usize> {
    let head = &Label::from(head.to_string());
    let fun = program.get(function)?;
    let s = site(fun, function, head, limit)?;

    let mut alloc = LocalIdAllocator::for_function(fun);
    let stores = store_count(&fun.cfg.named[&s.latch]) + store_count(&fun.cfg.named[&s.brk]);
    let ids = Ids {
        inner: s.inner_guards.iter().map(|_| [alloc.fresh(), alloc.fresh(), alloc.fresh()]).collect(),
        outer: [alloc.fresh(), alloc.fresh(), alloc.fresh()],
        p_tail: [alloc.fresh(), alloc.fresh()],
        head: [alloc.fresh(), alloc.fresh(), alloc.fresh(), alloc.fresh()],
        cont: [alloc.fresh(), alloc.fresh(), alloc.fresh(), alloc.fresh()],
        stores: (0..stores).map(|_| [alloc.fresh(), alloc.fresh(), alloc.fresh()]).collect(),
        l_tail: [alloc.fresh(), alloc.fresh(), alloc.fresh(), alloc.fresh(), alloc.fresh()],
    };

    let rebuilt = rebuilt_blocks(fun, &s, head, limit, &ids);
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
    fun.cfg.named.remove(&s.brk);
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
    limit: i16,
) -> Result<()> {
    let head = &Label::from(head.to_string());
    let before_fun = before.get(function)?;
    let s = site(before_fun, function, head, limit)?;
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
    require(
        after_fun.cfg.named.len() == before_fun.cfg.named.len() - 1
            && after_fun.cfg.named.get(&s.brk).is_none(),
        format!("mask_loop must delete exactly the break block '{}'", s.brk.as_str()),
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

    // The outer guard, right after the bound in the preheader; then the
    // preheader tail, shifted by the guard.
    let outer = learn_n(&mut learn, &s.preheader, s.bound_index + 1, 3)?;
    let before_p_len = before_fun.cfg.named[&s.preheader].instructions.len();
    let p_tail = learn_n(&mut learn, &s.preheader, before_p_len + 3, 2)?;

    // Head: fixed positions around the two originals.
    let head_ids = vec![
        learn(head, 1)?,
        learn(head, 2)?,
        learn(head, 4)?,
        learn(head, 5)?,
    ];

    // Cont tail.
    let before_c_len = before_fun.cfg.named[&s.cont].instructions.len();
    let cont_ids = learn_n(&mut learn, &s.cont, before_c_len, 4)?;

    // The latch: walk the before latch and break blocks, learning a triple
    // ahead of each store.
    let mut stores: Vec<[LocalId; 3]> = Vec::new();
    let mut position = 0usize;
    for source in [&s.latch, &s.brk] {
        for (_, instr) in &before_fun.cfg.named[source].instructions {
            if matches!(instr, Instruction::Store { .. }) {
                let got = learn_n(&mut learn, &s.latch, position, 3)?;
                stores.push([got[0], got[1], got[2]]);
                position += 4;
            } else {
                position += 1;
            }
        }
    }
    let l_tail = learn_n(&mut learn, &s.latch, position, 5)?;

    let ids = Ids {
        inner,
        outer: [outer[0], outer[1], outer[2]],
        p_tail: [p_tail[0], p_tail[1]],
        head: [head_ids[0], head_ids[1], head_ids[2], head_ids[3]],
        cont: [cont_ids[0], cont_ids[1], cont_ids[2], cont_ids[3]],
        stores,
        l_tail: [l_tail[0], l_tail[1], l_tail[2], l_tail[3], l_tail[4]],
    };

    // The expectation: rebuilt blocks, guard splices, everything else
    // untouched. Splices go descending per block so indices stay valid.
    let mut expected: std::collections::HashMap<Label, (Vec<(LocalId, Instruction)>, Terminator)> =
        rebuilt_blocks(before_fun, &s, head, limit, &ids)
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
        if label == &s.brk {
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
        apply(&mut p, "f", "head", 5).unwrap();
        verify(&before, &p, "f", "head", 5).unwrap();

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
        let error = apply(&mut p, "f", "head", 5).unwrap_err().to_string();
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
        let error = apply(&mut p, "f", "head", 5).unwrap_err().to_string();
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
        let error = apply(&mut p, "f", "head", 5).unwrap_err().to_string();
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
        let error = apply(&mut p, "f", "head", 5).unwrap_err().to_string();
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
        let error = apply(&mut p, "f", "head", 5).unwrap_err().to_string();
        assert!(error.contains("preheader"), "{}", error);
    }

    /// The verifier must reject an applier that forgot a store's mask.
    #[test]
    fn verify_rejects_an_unmasked_store() {
        let mut p = pixel_loop_program();
        let before = p.clone();
        apply(&mut p, "f", "head", 5).unwrap();
        {
            let fun = p.get_mut("f").unwrap();
            let latch = fun.cfg.named.get_mut(&label("latch")).unwrap();
            for (sid, instr) in latch.instructions.iter_mut() {
                if *sid == id(12) {
                    *instr = Instruction::Store { target: id(25), source: id(26) };
                }
            }
        }
        let error = verify(&before, &p, "f", "head", 5).unwrap_err().to_string();
        assert!(error.contains("differs"), "{}", error);
    }

    /// ...or one that kept the break block around.
    #[test]
    fn verify_rejects_a_kept_break_block() {
        let mut p = pixel_loop_program();
        let before = p.clone();
        apply(&mut p, "f", "head", 5).unwrap();
        {
            let fun = p.get_mut("f").unwrap();
            let brk = before.get("f").unwrap().cfg.named[&label("brk")].clone();
            fun.cfg.named.insert(label("brk"), brk);
        }
        let error = verify(&before, &p, "f", "head", 5).unwrap_err().to_string();
        assert!(error.contains("break block"), "{}", error);
    }
}
