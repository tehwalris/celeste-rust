//! `collapse_break_loop` - collapse a sentinel-bounded loop whose real exit
//! is an in-body break, guarded by the claim that the break fires on the
//! second iteration.
//!
//! # Why
//!
//! The `foreach` and `del` builtins compile as
//!
//! ```text
//!   for i=1,32767 do
//!     if #tbl < i then break end
//!     <payload>
//!   end
//! ```
//!
//! - a sentinel head that never decides anything, plus a per-iteration
//! length check that does. `collapse_loop` refused these by design: its
//! `bound == init` guard compared `32767 == 1` and fired on frame 1. With
//! the singleton `objects` table the real behaviour is: iteration 1 runs
//! the payload, iteration 2 breaks immediately. This rule states exactly
//! that, with no static arithmetic at all - every premise is a runtime
//! assert.
//!
//! # The shape
//!
//! ```text
//!   P:    ..                 br H     <- preheader, unconditional
//!   H:    i = phi [P: init, L: inc]   <- exactly this phi and this compare
//!         c0 = i <= sentinel
//!         br c0 ? C : X
//!   C:    len = # t                   <- exactly these two instructions
//!         c = len < i    (or i > len)
//!         br c ? B : E
//!   B:    br X                        <- the break block, empty
//!   E:    ..payload region..          <- untouched; may exit anywhere
//!   L:    .., inc = i + step          <- the latch, br H (may equal E)
//!   X:    ..                          <- the exit
//! ```
//!
//! After:
//!
//! ```text
//!   P:    ..; g0 = init <= sentinel; assert_true g0;   br C
//!   C:    len = # t; c = len < init; n = not c; assert_true n;   br E
//!   E:    ..payload, i := init..
//!   L:    .., inc = init + step;   br K
//!   K:    len2 = # t; c2 = len2 < inc; assert_true c2;   br X   <- new
//! ```
//!
//! `H` and `B` are deleted.
//!
//! # Soundness
//!
//! The three asserts are the original loop's three decisions, stated as
//! premises instead of branches:
//!
//!   * `init <= sentinel`: iteration 1's head test passes, so the original
//!     enters the check at all (otherwise it would run zero iterations).
//!   * `not c` at `C`: iteration 1 does not break, so the payload runs.
//!   * `c2` at `K`: iteration 2 breaks. The materialized check re-reads
//!     `# t` *after* the payload's heap effects, exactly as the original
//!     second iteration would (`player_spawn.update` destroys and re-adds
//!     an object mid-payload; the length that matters is the one after).
//!
//! One case is subsumed rather than modelled: an original run where
//! iteration 2 leaves via the *sentinel* test (`inc > sentinel`) with the
//! break check false would exit to the same block `X`; the collapsed form
//! asserts `c2` and would abort loudly instead. That is a premise
//! (`sentinel` is 32767 here; `inc` is 2), stated like every other.
//! Likewise the collapsed form evaluates `# t` a second time even when the
//! original sentinel exit would not have - `#` is pure, so nothing
//! observable changes.
//!
//! The payload is untouched and keeps every early exit it had; a payload
//! path that never reaches the latch never reaches the new check either,
//! exactly as it never reached the head. Substituting `init` for `i` uses
//! the same always-`init` region argument as `collapse_loop`, and the
//! latch's `inc` must have no use but the head's phi (it gains the one in
//! `K`).

use anyhow::{anyhow, Result};
use rustc_hash::FxHashSet;

use crate::ir::{BinaryOp, FunDef, Instruction, Label, LocalId, Terminator, UnaryOp};

use super::super::program::Program;
use super::{blocks_sorted, get_block, predecessors, require, LocalIdAllocator};

struct Site {
    preheader_key: Option<Label>,
    check: Label,
    brk: Label,
    payload_entry: Label,
    latch: Label,
    exit: Label,
    counter: LocalId,
    init: LocalId,
    sentinel: LocalId,
    inc: LocalId,
    /// The `# t` in the check block: (id, table local).
    len: (LocalId, LocalId),
    /// The break compare in the check block: (id, flipped) -
    /// `len < i` when not flipped, `i > len` when flipped.
    compare: (LocalId, bool),
}

fn flood_avoiding(fun: &FunDef, start: &Label, stop: &Label) -> FxHashSet<Label> {
    let mut seen: FxHashSet<Label> = FxHashSet::default();
    let mut stack = vec![start.clone()];
    while let Some(label) = stack.pop() {
        if label == *stop || !seen.insert(label.clone()) {
            continue;
        }
        if let Some(block) = fun.cfg.named.get(&label) {
            for successor in block.terminator_kind().successor_labels() {
                stack.push(successor.clone());
            }
        }
    }
    seen
}

fn site(fun: &FunDef, function: &str, head: &Label) -> Result<Site> {
    let head_block = fun
        .cfg
        .named
        .get(head)
        .ok_or_else(|| anyhow!("no block named '{}' in {}", head.as_str(), function))?;
    require(
        !head_block.hint_normalize,
        format!("'{}' is a hint_normalize block", head.as_str()),
    )?;

    // The head: the counter phi, the sentinel compare, the branch.
    let Terminator::ConditionalBranch { condition, true_target, false_target } =
        head_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in a conditional branch", head.as_str()));
    };
    let (check, exit) = (true_target.clone(), false_target.clone());
    require(
        check != exit && check != *head && exit != *head,
        format!("the branch of '{}' must leave to two distinct other blocks", head.as_str()),
    )?;
    require(
        head_block.instructions.len() == 2,
        format!("'{}' must hold exactly the counter phi and its compare", head.as_str()),
    )?;
    let (counter, counter_phi) = &head_block.instructions[0];
    let (cmp_id, cmp) = &head_block.instructions[1];
    require(
        cmp_id == condition,
        format!("'{}' must branch on its own compare", head.as_str()),
    )?;
    let Instruction::Phi { branches } = counter_phi else {
        return Err(anyhow!("'{}' does not start with the counter phi", head.as_str()));
    };
    require(branches.len() == 2, "the counter phi must have exactly two edges")?;
    let Instruction::BinaryOp { left, op: BinaryOp::LessThanEqual, right: sentinel } = cmp
    else {
        return Err(anyhow!("'{}' does not compare `counter <= sentinel`", head.as_str()));
    };
    require(left == counter, "the head compare does not test the counter")?;

    // The check block: exactly `len = # t; c = len < i` (or `i > len`),
    // branching to the break block on true and the payload on false.
    let check_block = fun
        .cfg
        .named
        .get(&check)
        .ok_or_else(|| anyhow!("check block '{}' does not exist", check.as_str()))?;
    require(
        check_block.instructions.len() == 2,
        format!("'{}' must hold exactly the length read and the break compare", check.as_str()),
    )?;
    let (len_id, len_instr) = &check_block.instructions[0];
    let (c_id, c_instr) = &check_block.instructions[1];
    let Instruction::UnaryOp { op: UnaryOp::Hash, arg: table } = len_instr else {
        return Err(anyhow!("'{}' does not start with `# t`", check.as_str()));
    };
    let flipped = match c_instr {
        Instruction::BinaryOp { left, op: BinaryOp::LessThan, right }
            if left == len_id && right == counter =>
        {
            false
        }
        Instruction::BinaryOp { left, op: BinaryOp::GreaterThan, right }
            if left == counter && right == len_id =>
        {
            true
        }
        _ => {
            return Err(anyhow!(
                "'{}' does not compare the length against the counter",
                check.as_str()
            ))
        }
    };
    let Terminator::ConditionalBranch {
        condition: check_cond,
        true_target: brk,
        false_target: payload_entry,
    } = check_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in the break branch", check.as_str()));
    };
    require(check_cond == c_id, "the check block must branch on its own compare")?;
    let (brk, payload_entry) = (brk.clone(), payload_entry.clone());
    require(
        brk != check && payload_entry != check && brk != payload_entry,
        "the break branch must leave to two distinct other blocks",
    )?;

    // The break block: empty, straight to the exit.
    let brk_block = fun
        .cfg
        .named
        .get(&brk)
        .ok_or_else(|| anyhow!("break block '{}' does not exist", brk.as_str()))?;
    require(
        brk_block.instructions.is_empty()
            && matches!(
                brk_block.terminator_kind(),
                Terminator::UnconditionalBranch { target } if *target == exit
            )
            && !brk_block.hint_normalize,
        format!("'{}' must be empty and branch straight to the exit", brk.as_str()),
    )?;
    let preds = predecessors(&fun.cfg);
    require(
        preds.get(&Some(brk.clone())).map(|p| p.as_slice())
            == Some(&[Some(check.clone())][..]),
        format!("'{}' must be reached only from the check", brk.as_str()),
    )?;

    // The phi edges: latch and preheader, as in `collapse_loop`.
    let mut interpretation: Option<usize> = None;
    for (edge, (label, value)) in branches.iter().enumerate() {
        let Some(block) = fun.cfg.named.get(label) else { continue };
        let Some((_, instr)) = block.instructions.iter().find(|(id, _)| id == value) else {
            continue;
        };
        let Instruction::BinaryOp { left, op: BinaryOp::Plus, .. } = instr else { continue };
        if left != counter {
            continue;
        }
        require(
            interpretation.is_none(),
            format!("both edges of '{}' look like latch edges", head.as_str()),
        )?;
        interpretation = Some(edge);
    }
    let Some(latch_edge) = interpretation else {
        return Err(anyhow!(
            "no edge of '{}' carries `counter + step` from its own block",
            head.as_str()
        ));
    };
    let (latch, inc) = branches[latch_edge].clone();
    let (preheader_label, init) = branches[1 - latch_edge].clone();
    let preheader_key = if preheader_label == super::entry_label()
        && !fun.cfg.named.contains_key(&preheader_label)
    {
        None
    } else {
        Some(preheader_label.clone())
    };
    let preheader_block = get_block(&fun.cfg, &preheader_key)
        .ok_or_else(|| anyhow!("preheader '{}' does not exist", preheader_label.as_str()))?;
    require(
        matches!(
            preheader_block.terminator_kind(),
            Terminator::UnconditionalBranch { target } if target == head
        ),
        format!(
            "preheader '{}' must branch unconditionally to '{}'",
            preheader_label.as_str(),
            head.as_str()
        ),
    )?;
    let latch_block = fun
        .cfg
        .named
        .get(&latch)
        .ok_or_else(|| anyhow!("latch '{}' does not exist", latch.as_str()))?;
    require(
        matches!(
            latch_block.terminator_kind(),
            Terminator::UnconditionalBranch { target } if target == head
        ),
        format!("latch '{}' must branch unconditionally to '{}'", latch.as_str(), head.as_str()),
    )?;

    // Nothing else reaches the head.
    let head_preds = preds
        .get(&Some(head.clone()))
        .ok_or_else(|| anyhow!("'{}' has no predecessors", head.as_str()))?;
    let mut expected: Vec<Option<Label>> = vec![preheader_key.clone(), Some(latch.clone())];
    expected.sort();
    let mut actual = head_preds.clone();
    actual.sort();
    actual.dedup();
    require(
        actual == expected,
        format!("'{}' must be reached only from its preheader and latch", head.as_str()),
    )?;

    // Counter-use confinement, as in `collapse_loop`: uses only where the
    // counter provably still holds `init` - reachable from the check
    // without re-entering the head, minus what the exit side reaches. The
    // head's own compare and the check's are accounted separately.
    let body_region = flood_avoiding(fun, &check, head);
    let exit_region = flood_avoiding(fun, &exit, head);
    let always_init: FxHashSet<&Label> = body_region.difference(&exit_region).collect();
    require(
        always_init.contains(&check),
        "the exit side must not reach back into the loop",
    )?;
    for key in blocks_sorted(&fun.cfg) {
        let block = get_block(&fun.cfg, &key).expect("listed block exists");
        let in_head = key.as_ref() == Some(head);
        let in_always_init = matches!(&key, Some(l) if always_init.contains(l));
        for (id, instr) in &block.instructions {
            for used in instr.get_used_locals() {
                if used == *counter {
                    require(
                        in_always_init || (in_head && id == cmp_id),
                        format!(
                            "the counter of '{}' is used in '{}', where it is not \
                             always `init`",
                            head.as_str(),
                            key.as_ref().map(|l| l.as_str()).unwrap_or("__entry"),
                        ),
                    )?;
                }
                require(
                    used != *cmp_id || in_head,
                    "the head compare is used outside the head",
                )?;
                require(
                    used != inc || (matches!(instr, Instruction::Phi { .. }) && in_head),
                    format!(
                        "the increment of '{}' is used by something besides the phi",
                        head.as_str()
                    ),
                )?;
            }
            if let Instruction::Phi { branches } = instr {
                for (label, value) in branches {
                    if label == head || label == &brk {
                        require(
                            *value != *counter && *value != *cmp_id,
                            format!(
                                "a phi outside '{}' carries its counter or compare",
                                head.as_str()
                            ),
                        )?;
                    }
                }
            }
        }
        for used in block.terminator_kind().get_used_locals() {
            require(
                used != *counter || in_always_init,
                "the counter is used by a terminator outside the loop",
            )?;
            require(used != *cmp_id || in_head, "the head compare is used outside the head")?;
            require(used != inc, "the increment is used by a terminator")?;
        }
    }

    Ok(Site {
        preheader_key,
        check,
        brk,
        payload_entry,
        latch,
        exit,
        counter: *counter,
        init,
        sentinel: *sentinel,
        inc,
        len: (*len_id, *table),
        compare: (*c_id, flipped),
    })
}

/// The label of the new second-check block.
fn second_check_label(head: &Label) -> Label {
    Label::from(format!("{}_second_break", head.as_str()))
}

/// The instructions of the new second-check block, given four fresh ids.
fn second_check_instructions(s: &Site, fresh: &[LocalId; 3]) -> Vec<(LocalId, Instruction)> {
    let [len2, c2, a2] = *fresh;
    let compare = if s.compare.1 {
        Instruction::BinaryOp { left: s.inc, op: BinaryOp::GreaterThan, right: len2 }
    } else {
        Instruction::BinaryOp { left: len2, op: BinaryOp::LessThan, right: s.inc }
    };
    vec![
        (len2, Instruction::UnaryOp { op: UnaryOp::Hash, arg: s.len.1 }),
        (c2, compare),
        (a2, Instruction::AssertTrue { value: c2 }),
    ]
}

pub fn apply(program: &mut Program, function: &str, head: &str) -> Result<usize> {
    let head = Label::from(head.to_string());
    let fun = program.get(function)?;
    let s = site(fun, function, &head)?;
    let new_label = second_check_label(&head);
    require(
        !fun.cfg.named.contains_key(&new_label),
        format!("a block named '{}' already exists", new_label.as_str()),
    )?;
    let mut allocator = LocalIdAllocator::for_function(fun);
    let g0 = allocator.fresh();
    let a0 = allocator.fresh();
    let n1 = allocator.fresh();
    let a1 = allocator.fresh();
    let fresh2 = [allocator.fresh(), allocator.fresh(), allocator.fresh()];
    let terminator2 = allocator.fresh();

    let fun = program.get_mut(function)?;

    // The preheader: iteration 1's head test as a premise, then into the
    // check.
    let preheader = match &s.preheader_key {
        None => &mut fun.cfg.entry,
        Some(l) => fun.cfg.named.get_mut(l).expect("preheader exists"),
    };
    preheader.instructions.push((
        g0,
        Instruction::BinaryOp { left: s.init, op: BinaryOp::LessThanEqual, right: s.sentinel },
    ));
    preheader.instructions.push((a0, Instruction::AssertTrue { value: g0 }));
    preheader.terminator.1 = Terminator::UnconditionalBranch { target: s.check.clone() };

    // The check: iteration 1 must not break; fall into the payload.
    let check = fun.cfg.named.get_mut(&s.check).expect("check exists");
    check
        .instructions
        .push((n1, Instruction::UnaryOp { op: UnaryOp::Not, arg: s.compare.0 }));
    check.instructions.push((a1, Instruction::AssertTrue { value: n1 }));
    check.terminator.1 = Terminator::UnconditionalBranch { target: s.payload_entry.clone() };

    // The latch: fall into the new second check.
    let latch = fun.cfg.named.get_mut(&s.latch).expect("latch exists");
    latch.terminator.1 = Terminator::UnconditionalBranch { target: new_label.clone() };

    // The second check: iteration 2 must break.
    fun.cfg.named.insert(
        new_label.clone(),
        crate::ir::Block {
            instructions: second_check_instructions(&s, &fresh2),
            terminator: (
                terminator2,
                Terminator::UnconditionalBranch { target: s.exit.clone() },
            ),
            hint_normalize: false,
        },
    );

    // The head and the break block are gone; the counter is `init`.
    fun.cfg.named.remove(&head);
    fun.cfg.named.remove(&s.brk);
    let substitute = |id: LocalId| if id == s.counter { s.init } else { id };
    let apply_to = |block: &mut crate::ir::Block| {
        for (_, instr) in block.instructions.iter_mut() {
            *instr = instr.map_local_ids(substitute);
        }
        block.terminator.1 = block.terminator.1.map_local_ids(substitute);
    };
    apply_to(&mut fun.cfg.entry);
    for block in fun.cfg.named.values_mut() {
        apply_to(block);
    }

    // Phi edges into the exit: the head edge and the break edge both become
    // the new check's. Both existing at once with different values was
    // refused by `site`; with equal values the pair merges.
    if let Some(exit_block) = fun.cfg.named.get_mut(&s.exit) {
        for (_, instr) in exit_block.instructions.iter_mut() {
            if let Instruction::Phi { branches } = instr {
                let mut kept: Vec<(Label, LocalId)> = Vec::new();
                for (label, value) in branches.iter() {
                    let label = if label == &head || label == &s.brk {
                        new_label.clone()
                    } else {
                        label.clone()
                    };
                    if !kept.iter().any(|(l, v)| *l == label && *v == *value) {
                        kept.push((label, *value));
                    }
                }
                *instr = Instruction::Phi { branches: kept };
            }
        }
    }
    // The payload entry's phis: its edge from the check keeps its label -
    // nothing to rename there.

    fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(1)
}

/// Independent check: re-derives the site from the *before* program and
/// insists the after program is exactly the prescription.
pub fn verify(before: &Program, after: &Program, function: &str, head: &str) -> Result<()> {
    let head = Label::from(head.to_string());
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let s = site(before_fun, function, &head)?;
    let new_label = second_check_label(&head);

    require(
        !after_fun.cfg.named.contains_key(&head)
            && !after_fun.cfg.named.contains_key(&s.brk),
        "collapse_break_loop did not remove the head and break blocks",
    )?;
    require(
        after_fun.cfg.named.len() == before_fun.cfg.named.len() - 1,
        "collapse_break_loop must remove two blocks and add one",
    )?;

    let before_max = {
        let mut allocator = LocalIdAllocator::for_function(before_fun);
        allocator.fresh()
    };
    let all_fresh_distinct = |ids: &[LocalId]| {
        ids.iter().all(|id| *id >= before_max)
            && ids.iter().collect::<FxHashSet<_>>().len() == ids.len()
    };

    // The new second-check block.
    let second = after_fun
        .cfg
        .named
        .get(&new_label)
        .ok_or_else(|| anyhow!("no second-check block '{}'", new_label.as_str()))?;
    require(
        second.instructions.len() == 3 && !second.hint_normalize,
        "the second check must hold exactly the length read, compare and assert",
    )?;
    let fresh2 = [
        second.instructions[0].0,
        second.instructions[1].0,
        second.instructions[2].0,
    ];
    require(
        second.instructions == second_check_instructions(&s, &fresh2)
            && matches!(
                second.terminator_kind(),
                Terminator::UnconditionalBranch { target } if *target == s.exit
            ),
        "the second check is not the prescribed break re-check",
    )?;

    // The guard ids across preheader, check and the new block: all fresh.
    let before_preheader = get_block(&before_fun.cfg, &s.preheader_key).expect("site checked");
    let after_preheader = get_block(&after_fun.cfg, &s.preheader_key)
        .ok_or_else(|| anyhow!("collapse_break_loop removed the preheader"))?;
    let n = before_preheader.instructions.len();
    require(
        after_preheader.instructions.len() == n + 2,
        "the preheader must gain exactly the sentinel guard pair",
    )?;
    let (g0, g0_instr) = &after_preheader.instructions[n];
    let (a0, a0_instr) = &after_preheader.instructions[n + 1];
    let before_check = before_fun.cfg.named.get(&s.check).expect("site checked");
    let after_check = after_fun
        .cfg
        .named
        .get(&s.check)
        .ok_or_else(|| anyhow!("collapse_break_loop removed the check"))?;
    require(
        after_check.instructions.len() == before_check.instructions.len() + 2,
        "the check must gain exactly the not/assert pair",
    )?;
    let (n1, n1_instr) = &after_check.instructions[2];
    let (a1, a1_instr) = &after_check.instructions[3];
    require(
        all_fresh_distinct(&[*g0, *a0, *n1, *a1, fresh2[0], fresh2[1], fresh2[2],
            second.terminator.0]),
        "the inserted ids must be fresh and distinct",
    )?;
    require(
        *g0_instr
            == Instruction::BinaryOp {
                left: s.init,
                op: BinaryOp::LessThanEqual,
                right: s.sentinel,
            }
            && *a0_instr == Instruction::AssertTrue { value: *g0 },
        "the preheader guard must assert `init <= sentinel`",
    )?;
    require(
        *n1_instr == Instruction::UnaryOp { op: UnaryOp::Not, arg: s.compare.0 }
            && *a1_instr == Instruction::AssertTrue { value: *n1 },
        "the check guard must assert the break does not fire",
    )?;

    // Everything else: before, with the substitution, the retargets and the
    // insertions.
    let substitute = |id: LocalId| if id == s.counter { s.init } else { id };
    for key in blocks_sorted(&before_fun.cfg) {
        if key.as_ref() == Some(&head) || key.as_ref() == Some(&s.brk) {
            continue;
        }
        let before_block = get_block(&before_fun.cfg, &key).expect("listed block exists");
        let after_block = get_block(&after_fun.cfg, &key).ok_or_else(|| {
            anyhow!(
                "collapse_break_loop removed block '{}'",
                key.as_ref().map(|l| l.as_str()).unwrap_or("__entry")
            )
        })?;

        let mut expected = before_block.clone();
        for (_, instr) in expected.instructions.iter_mut() {
            *instr = instr.map_local_ids(substitute);
        }
        expected.terminator.1 = expected.terminator.1.map_local_ids(substitute);
        if key == s.preheader_key {
            expected.instructions.push((*g0, g0_instr.clone()));
            expected.instructions.push((*a0, a0_instr.clone()));
            expected.terminator.1 = Terminator::UnconditionalBranch { target: s.check.clone() };
        }
        if key.as_ref() == Some(&s.check) {
            expected.instructions.push((*n1, n1_instr.clone()));
            expected.instructions.push((*a1, a1_instr.clone()));
            expected.terminator.1 =
                Terminator::UnconditionalBranch { target: s.payload_entry.clone() };
        }
        if key.as_ref() == Some(&s.latch) {
            expected.terminator.1 =
                Terminator::UnconditionalBranch { target: new_label.clone() };
        }
        if key.as_ref() == Some(&s.exit) {
            for (_, instr) in expected.instructions.iter_mut() {
                if let Instruction::Phi { branches } = instr {
                    let mut kept: Vec<(Label, LocalId)> = Vec::new();
                    for (label, value) in branches.iter() {
                        let label = if label == &head || label == &s.brk {
                            new_label.clone()
                        } else {
                            label.clone()
                        };
                        if !kept.iter().any(|(l, v)| *l == label && *v == *value) {
                            kept.push((label, *value));
                        }
                    }
                    *instr = Instruction::Phi { branches: kept };
                }
            }
        }
        require(
            *after_block == expected,
            format!(
                "collapse_break_loop changed block '{}' beyond the prescription",
                key.as_ref().map(|l| l.as_str()).unwrap_or("__entry")
            ),
        )?;
    }

    require(
        before.functions.len() == after.functions.len(),
        "collapse_break_loop changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!(
                "collapse_break_loop on {} also changed {}",
                function,
                name.as_str()
            ),
        )?;
    }
    Ok(())
}

/// Heads this rule accepts.
pub fn candidates(program: &Program) -> Vec<(String, Label)> {
    let mut out = Vec::new();
    for (name, fun) in &program.functions {
        for label in fun.cfg.named.keys() {
            if site(fun, name.as_str(), label).is_ok() {
                out.push((name.as_str().to_string(), label.clone()));
            }
        }
    }
    out.sort_by(|a, b| (&a.0, a.1.as_str()).cmp(&(&b.0, b.1.as_str())));
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, GlobalId};
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
        Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(value) }
    }

    /// The inlined-`foreach` shape:
    ///
    ///   entry: init=1, sentinel=32767, t = objects table; br head
    ///   head:  i = phi [entry: init, payload: inc]; c0 = i <= sentinel;
    ///          br c0 ? check : exit
    ///   check: len = # t; c = len < i; br c ? brk : payload
    ///   brk:   br exit
    ///   payload: o = t[i]; call f(o); inc = i + init; br head
    ///   exit:  return
    fn foreach_program() -> Program {
        let entry = block(
            vec![
                (1, num(1)),
                (2, num(32767)),
                (
                    3,
                    Instruction::GetGlobal {
                        name: "objects".to_string(),
                        create_if_missing: false,
                    },
                ),
                (4, Instruction::Load { source: id(3) }),
                (
                    5,
                    Instruction::GetGlobal { name: "f".to_string(), create_if_missing: false },
                ),
                (6, Instruction::Load { source: id(5) }),
            ],
            7,
            br("head"),
        );
        let head = block(
            vec![
                (
                    8,
                    Instruction::Phi {
                        branches: vec![
                            (super::super::entry_label(), id(1)),
                            (label("payload"), id(15)),
                        ],
                    },
                ),
                (
                    9,
                    Instruction::BinaryOp {
                        left: id(8),
                        op: BinaryOp::LessThanEqual,
                        right: id(2),
                    },
                ),
            ],
            10,
            br_if(9, "check", "exit"),
        );
        let check = block(
            vec![
                (11, Instruction::UnaryOp { op: UnaryOp::Hash, arg: id(4) }),
                (
                    12,
                    Instruction::BinaryOp { left: id(11), op: BinaryOp::LessThan, right: id(8) },
                ),
            ],
            16,
            br_if(12, "brk", "payload"),
        );
        let brk = block(vec![], 17, br("exit"));
        let payload = block(
            vec![
                (
                    13,
                    Instruction::GetIndex {
                        receiver: id(4),
                        index: id(8),
                        create_if_missing: false,
                    },
                ),
                (18, Instruction::Load { source: id(13) }),
                (14, Instruction::Call { closure: id(6), args: vec![id(18)] }),
                (
                    15,
                    Instruction::BinaryOp { left: id(8), op: BinaryOp::Plus, right: id(1) },
                ),
            ],
            19,
            br("head"),
        );
        let exit = block(vec![], 20, Terminator::Return { value: None });

        let mut named = crate::ir::new_label_map();
        named.insert(label("head"), head);
        named.insert(label("check"), check);
        named.insert(label("brk"), brk);
        named.insert(label("payload"), payload);
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
    fn collapses_a_foreach_loop() {
        let before = foreach_program();
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        verify(&before, &after, "f", "head").unwrap();

        let fun = after.get("f").unwrap();
        assert!(!fun.cfg.named.contains_key(&label("head")));
        assert!(!fun.cfg.named.contains_key(&label("brk")));
        // The preheader asserts init <= sentinel and falls into the check.
        let tail = &fun.cfg.entry.instructions[6..];
        assert_eq!(
            tail[0].1,
            Instruction::BinaryOp { left: id(1), op: BinaryOp::LessThanEqual, right: id(2) }
        );
        assert!(matches!(tail[1].1, Instruction::AssertTrue { .. }));
        // The check asserts the break does not fire and falls into the
        // payload; its compare now tests `init`.
        let check = &fun.cfg.named[&label("check")];
        assert_eq!(
            check.instructions[1].1,
            Instruction::BinaryOp { left: id(11), op: BinaryOp::LessThan, right: id(1) }
        );
        assert!(matches!(check.instructions[2].1, Instruction::UnaryOp { op: UnaryOp::Not, .. }));
        assert!(matches!(check.instructions[3].1, Instruction::AssertTrue { .. }));
        assert!(matches!(
            check.terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == label("payload")
        ));
        // The payload indexes with `init` and falls into the second check.
        let payload = &fun.cfg.named[&label("payload")];
        assert_eq!(
            payload.instructions[0].1,
            Instruction::GetIndex { receiver: id(4), index: id(1), create_if_missing: false }
        );
        assert!(matches!(
            payload.terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == label("head_second_break")
        ));
        // The second check re-reads the length against the increment and
        // asserts the break fires.
        let second = &fun.cfg.named[&label("head_second_break")];
        assert_eq!(
            second.instructions[0].1,
            Instruction::UnaryOp { op: UnaryOp::Hash, arg: id(4) }
        );
        assert_eq!(
            second.instructions[1].1,
            Instruction::BinaryOp {
                left: second.instructions[0].0,
                op: BinaryOp::LessThan,
                right: id(15)
            }
        );
        assert!(matches!(second.instructions[2].1, Instruction::AssertTrue { .. }));
        assert!(matches!(
            second.terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == label("exit")
        ));
    }

    #[test]
    fn accepts_the_flipped_compare() {
        // `i > len` instead of `len < i`, as `del` compiles.
        let mut program = foreach_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            let check = fun.cfg.named.get_mut(&label("check")).unwrap();
            check.instructions[1].1 =
                Instruction::BinaryOp { left: id(8), op: BinaryOp::GreaterThan, right: id(11) };
        }
        let before = program.clone();
        let mut after = program;
        apply(&mut after, "f", "head").unwrap();
        verify(&before, &after, "f", "head").unwrap();
        let fun = after.get("f").unwrap();
        let second = &fun.cfg.named[&label("head_second_break")];
        assert_eq!(
            second.instructions[1].1,
            Instruction::BinaryOp {
                left: id(15),
                op: BinaryOp::GreaterThan,
                right: second.instructions[0].0
            }
        );
    }

    #[test]
    fn refuses_a_nonempty_break_block() {
        let mut program = foreach_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            let brk = fun.cfg.named.get_mut(&label("brk")).unwrap();
            brk.instructions.push((id(30), Instruction::NilConstant));
        }
        let err = apply(&mut program, "f", "head").unwrap_err();
        assert!(err.to_string().contains("must be empty"), "{}", err);
    }

    #[test]
    fn refuses_an_increment_with_other_uses() {
        let mut program = foreach_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            let exit = fun.cfg.named.get_mut(&label("exit")).unwrap();
            exit.instructions.push((
                id(30),
                Instruction::BinaryOp { left: id(15), op: BinaryOp::Plus, right: id(15) },
            ));
        }
        let err = apply(&mut program, "f", "head").unwrap_err();
        assert!(err.to_string().contains("increment"), "{}", err);
    }

    #[test]
    fn verify_rejects_a_missing_second_check() {
        let before = foreach_program();
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        {
            let fun = after.functions.values_mut().next().unwrap();
            let second = fun.cfg.named.get_mut(&label("head_second_break")).unwrap();
            second.instructions.pop();
        }
        let err = verify(&before, &after, "f", "head").unwrap_err();
        assert!(err.to_string().contains("second check"), "{}", err);
    }

    #[test]
    fn finds_the_fixture_site() {
        let program = foreach_program();
        assert_eq!(candidates(&program), vec![("f".to_string(), label("head"))]);
    }
}
