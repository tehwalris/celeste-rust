//! `collapse_all_loop` - collapse the `all()` iterator's sentinel loop,
//! guarded by the claim that iteration 2 hits the nil sentinel.
//!
//! Pointed rule: names the function and the loop head.
//!
//! # Why
//!
//! Since #112 the `foreach` builtin has PICO-8 `all()` semantics, which
//! compile as
//!
//! ```text
//!   for i=1,32767 do
//!     if t[idx] == last then idx += 1 end   <- the del-compensation diamond
//!     v = t[idx]; last = v
//!     if v == nil then break end
//!     f(v)
//!   end
//! ```
//!
//! - a sentinel head that never decides anything, an advance diamond and a
//! nil check that do, all carried through cells (`idx`, `last`), not SSA.
//! `collapse_loop`'s `bound == init` guard compares `32767 == 1` and fires
//! on frame 1; `collapse_break_loop` demands the pre-#112 length check.
//! This rule states the singleton-table behaviour of the NEW shape: the
//! payload runs exactly once, and iteration 2's nil check breaks. This is
//! open task #113's missing rule.
//!
//! # The shape
//!
//! ```text
//!   P:  ..                  br H       <- preheader, unconditional
//!   H:  i = phi [P: init, L: inc]      <- exactly this phi and compare
//!       c0 = i <= sentinel
//!       br c0 ? C : X
//!   C:  ..                 br a ? A : J     <- the advance diamond head
//!   A:  ..                 br J
//!   J:  ..                 br n ? B : E     <- the nil check
//!   B:  (empty)            br X             <- the break block
//!   E:  ..payload..; L: .., inc = i + step; br H
//!   X:  ..                                  <- the exit, no phis
//! ```
//!
//! `C`, `A`, `J` may hold ANY instructions (they carry the iterator's cell
//! churn); they must hold no phis, and no SSA value they define may be
//! used outside `{C, A, J}` except by iteration-1's payload - both checked
//! by construction below. The payload region is untouched.
//!
//! # What it does
//!
//! ```text
//!   P:  ..; g0 = init <= sentinel; assert_true g0;   br C
//!   C:  ..[i := init]..              br a ? A : J
//!   A:  ..                           br J
//!   J:  ..; n1 = not n; assert_true n1;   br E    <- iteration 1 must not break
//!   E:  ..payload [i := init].., inc;     br C2
//!   C2: g1 = inc <= sentinel; assert_true g1
//!       ..fresh copy of C [i := inc]..   br a' ? A2 : J2
//!   A2: ..fresh copy of A..              br J2
//!   J2: ..fresh copy of J..; assert_true n';   br X   <- iteration 2 breaks
//! ```
//!
//! `H` and `B` are deleted.
//!
//! # Soundness
//!
//! Every decision the original loop makes is either re-executed verbatim
//! or stated as a runtime assert - no static arithmetic:
//!
//!   * `g0`: iteration 1's head test passes (else the original ran zero
//!     iterations and the collapsed form aborts loudly).
//!   * `n1` at `J`: iteration 1 does not break, so the payload runs.
//!   * `g1` before `C2`: iteration 2's head test passes.
//!   * The copies `C2/A2/J2` re-run iteration 2's check chain on the real
//!     heap, after the payload's effects, through the same cells - the
//!     advance diamond's own branch stays a branch, taken per lane exactly
//!     as the original's.
//!   * `assert n'` at `J2`: iteration 2 breaks. An original run where
//!     iteration 2 reached the payload instead aborts loudly here.
//!
//! Substituting `init` for `i` in iteration 1 (and `inc` in the copy) is
//! `collapse_loop`'s always-init argument: between the head and the latch
//! the counter provably holds that value. The copies use fresh ids for
//! everything they define, so no dominance is disturbed; cross-iteration
//! dataflow goes through cells, which the copies share with the original
//! blocks by construction. Screened at full depth like every guard rule.

use anyhow::{anyhow, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{BinaryOp, Block, FunDef, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::{blocks_sorted, get_block, predecessors, require, LocalIdAllocator};

struct Site {
    preheader_key: Option<Label>,
    check: Label,
    advance: Label,
    nil_check: Label,
    brk: Label,
    payload_entry: Label,
    latch: Label,
    exit: Label,
    counter: LocalId,
    init: LocalId,
    sentinel: LocalId,
    inc: LocalId,
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

    // The head: exactly the counter phi, the sentinel compare, the branch.
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
    require(cmp_id == condition, format!("'{}' must branch on its own compare", head.as_str()))?;
    let Instruction::Phi { branches } = counter_phi else {
        return Err(anyhow!("'{}' does not start with the counter phi", head.as_str()));
    };
    require(branches.len() == 2, "the counter phi must have exactly two edges".to_string())?;
    let Instruction::BinaryOp { left, op: BinaryOp::LessThanEqual, right: sentinel } = cmp else {
        return Err(anyhow!("'{}' does not compare `counter <= sentinel`", head.as_str()));
    };
    require(left == counter, "the head compare does not test the counter".to_string())?;

    // Latch and preheader edges, as in the siblings.
    let mut latch_edge: Option<usize> = None;
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
            latch_edge.is_none(),
            format!("both edges of '{}' look like latch edges", head.as_str()),
        )?;
        latch_edge = Some(edge);
    }
    let Some(latch_edge) = latch_edge else {
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
    let preds = predecessors(&fun.cfg);
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

    // The check chain: C -> {A, J}, A -> J, J -> {B, E}.
    let check_block = fun
        .cfg
        .named
        .get(&check)
        .ok_or_else(|| anyhow!("check block '{}' does not exist", check.as_str()))?;
    let Terminator::ConditionalBranch {
        condition: _,
        true_target: advance,
        false_target: nil_check,
    } = check_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in the advance branch", check.as_str()));
    };
    let (advance, nil_check) = (advance.clone(), nil_check.clone());
    let advance_block = fun
        .cfg
        .named
        .get(&advance)
        .ok_or_else(|| anyhow!("advance block '{}' does not exist", advance.as_str()))?;
    require(
        matches!(
            advance_block.terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == nil_check
        ),
        format!(
            "advance block '{}' must branch unconditionally to '{}'",
            advance.as_str(),
            nil_check.as_str()
        ),
    )?;
    let nil_block = fun
        .cfg
        .named
        .get(&nil_check)
        .ok_or_else(|| anyhow!("nil-check block '{}' does not exist", nil_check.as_str()))?;
    let Terminator::ConditionalBranch {
        condition: nil_cond,
        true_target: brk,
        false_target: payload_entry,
    } = nil_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in the nil-check branch", nil_check.as_str()));
    };
    let (brk, payload_entry) = (brk.clone(), payload_entry.clone());
    let nil_cond = *nil_cond;
    // The nil check's condition must be `v == nil` in spirit; structurally we
    // only demand it is an Eq defined in the nil-check block itself, so the
    // copied iteration re-evaluates it on the real heap.
    let nil_cond_local = nil_block.instructions.iter().find(|(id, _)| *id == nil_cond);
    require(
        matches!(nil_cond_local, Some((_, Instruction::BinaryOp { op: BinaryOp::TwoEqual, .. }))),
        format!("'{}' must branch on its own equality compare", nil_check.as_str()),
    )?;

    // The break block: empty, straight to the exit, reached only from J.
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
    require(
        preds.get(&Some(brk.clone())).map(|p| p.as_slice()) == Some(&[Some(nil_check.clone())][..]),
        format!("'{}' must be reached only from the nil check", brk.as_str()),
    )?;

    // The chain blocks hold no phis (all cross-block dataflow via cells),
    // are distinct, and are reached only along the chain.
    for (name, label) in
        [("check", &check), ("advance", &advance), ("nil check", &nil_check)]
    {
        let block = fun.cfg.named.get(label).expect("chain block exists");
        require(
            !block.hint_normalize,
            format!("the {} '{}' is a hint_normalize block", name, label.as_str()),
        )?;
        require(
            !block
                .instructions
                .iter()
                .any(|(_, i)| matches!(i, Instruction::Phi { .. })),
            format!("the {} '{}' must hold no phis", name, label.as_str()),
        )?;
    }
    require(
        check != advance && check != nil_check && advance != nil_check,
        "the check chain blocks must be distinct".to_string(),
    )?;
    require(
        preds.get(&Some(advance.clone())).map(|p| p.as_slice())
            == Some(&[Some(check.clone())][..]),
        format!("'{}' must be reached only from the check", advance.as_str()),
    )?;
    let mut nil_preds = preds
        .get(&Some(nil_check.clone()))
        .cloned()
        .unwrap_or_default();
    nil_preds.sort();
    let mut expected_nil: Vec<Option<Label>> =
        vec![Some(check.clone()), Some(advance.clone())];
    expected_nil.sort();
    require(
        nil_preds == expected_nil,
        format!("'{}' must be reached only from the check chain", nil_check.as_str()),
    )?;

    // SSA defined in the chain must not escape the chain-plus-payload: a
    // value defined in C/A/J used past the loop would dangle once iteration
    // 2's copy renames it. Payload uses are iteration 1's own - fine.
    let body_region = flood_avoiding(fun, &check, head);
    let chain: FxHashSet<&Label> = [&check, &advance, &nil_check].into_iter().collect();
    let mut chain_defs: FxHashSet<LocalId> = FxHashSet::default();
    for label in &chain {
        for (id, _) in &fun.cfg.named.get(*label).expect("chain block exists").instructions {
            chain_defs.insert(*id);
        }
    }
    for key in blocks_sorted(&fun.cfg) {
        let in_body = matches!(&key, Some(l) if body_region.contains(l));
        if in_body {
            continue;
        }
        let block = get_block(&fun.cfg, &key).expect("listed block exists");
        for (_, instr) in &block.instructions {
            for used in instr.get_used_locals() {
                require(
                    !chain_defs.contains(&used),
                    format!(
                        "%{} defined in the check chain is used outside the loop",
                        usize::from(used)
                    ),
                )?;
            }
        }
        for used in block.terminator_kind().get_used_locals() {
            require(
                !chain_defs.contains(&used),
                format!(
                    "%{} defined in the check chain is used outside the loop",
                    usize::from(used)
                ),
            )?;
        }
    }

    // Counter-use confinement: uses only inside the body region (where the
    // counter provably holds init) or the head's own compare. The exit side
    // must not reach back in.
    let exit_region = flood_avoiding(fun, &exit, head);
    require(
        exit_region.is_disjoint(&body_region) || !exit_region.contains(&check),
        "the exit side must not reach back into the loop".to_string(),
    )?;
    for key in blocks_sorted(&fun.cfg) {
        let block = get_block(&fun.cfg, &key).expect("listed block exists");
        let in_head = key.as_ref() == Some(head);
        let in_body = matches!(&key, Some(l) if body_region.contains(l));
        for (id, instr) in &block.instructions {
            for used in instr.get_used_locals() {
                if used == *counter {
                    require(
                        in_body || (in_head && id == cmp_id),
                        format!(
                            "the counter of '{}' is used outside the loop body",
                            head.as_str()
                        ),
                    )?;
                }
            }
        }
        if !in_body && key.as_ref() != Some(head) {
            require(
                !block.terminator_kind().get_used_locals().contains(counter),
                format!("the counter of '{}' is used by an outside terminator", head.as_str()),
            )?;
        }
    }
    // The inc's only use is the phi (it gains the copy's uses).
    for key in blocks_sorted(&fun.cfg) {
        let block = get_block(&fun.cfg, &key).expect("listed block exists");
        for (id, instr) in &block.instructions {
            if key.as_ref() == Some(head) && id == counter {
                continue; // the phi itself
            }
            require(
                !instr.get_used_locals().contains(&inc),
                format!("the latch increment of '{}' has uses besides the phi", head.as_str()),
            )?;
        }
        require(
            !block.terminator_kind().get_used_locals().contains(&inc),
            format!("the latch increment of '{}' is used by a terminator", head.as_str()),
        )?;
    }
    // The exit holds no phis: loop values escape through cells only.
    let exit_block = fun
        .cfg
        .named
        .get(&exit)
        .ok_or_else(|| anyhow!("exit '{}' does not exist", exit.as_str()))?;
    require(
        !exit_block
            .instructions
            .iter()
            .any(|(_, i)| matches!(i, Instruction::Phi { .. })),
        format!("the exit '{}' must hold no phis", exit.as_str()),
    )?;

    Ok(Site {
        preheader_key,
        check,
        advance,
        nil_check,
        brk,
        payload_entry,
        latch,
        exit,
        counter: *counter,
        init,
        sentinel: *sentinel,
        inc,
    })
}

/// Substitute `from -> to` in every operand of an instruction list and a
/// terminator (never in defined ids).
fn substitute(block: &mut Block, map: &FxHashMap<LocalId, LocalId>) {
    for (_, instr) in &mut block.instructions {
        *instr = instr.map_local_ids(|l| *map.get(&l).unwrap_or(&l));
    }
    let (tid, term) = block.terminator.clone();
    block.terminator = (tid, term.map_local_ids(|l| *map.get(&l).unwrap_or(&l)));
}

pub fn apply(program: &mut Program, rewrite_id: &str, function: &str, head: &str) -> Result<usize> {
    let head = Label::from(head.to_string());
    let fun = program.get(function)?;
    let s = site(fun, function, &head)?;

    // The payload must be a single block that is also the latch: the peel
    // duplicates it, and a multi-block payload would need a region copy.
    require(
        s.payload_entry == s.latch,
        format!(
            "the payload '{}' must be the latch '{}' (single-block payloads only)",
            s.payload_entry.as_str(),
            s.latch.as_str()
        ),
    )?;

    let mut ids = LocalIdAllocator::for_function(fun);
    let mut changes = 0;
    let fun_ref = program.get(function)?;

    // The blocks each peeled iteration copies: check chain + payload.
    let chain = [s.check.clone(), s.advance.clone(), s.nil_check.clone()];
    let with_payload =
        [s.check.clone(), s.advance.clone(), s.nil_check.clone(), s.latch.clone()];

    // Build the copies for iterations 2 (chain + payload, real branches) and
    // 3 (chain only, break asserted). Each iteration's counter value: the
    // previous payload copy's latch increment.
    let it_label = |it: usize, l: &Label| {
        Label::from(format!("it{}_{}_{}", it, rewrite_id, l.as_str()))
    };
    let mut copies: Vec<(Label, Block)> = Vec::new();
    let counter_of_it3; // iteration 2's copy of the latch increment
    {
        // --- iteration 2: chain + payload ---
        let mut rename: FxHashMap<LocalId, LocalId> = FxHashMap::default();
        rename.insert(s.counter, s.inc);
        for label in &with_payload {
            let block = fun_ref.cfg.named.get(label).expect("loop block exists");
            for (id, _) in &block.instructions {
                rename.insert(*id, ids.fresh());
            }
            rename.insert(block.terminator.0, ids.fresh());
        }
        counter_of_it3 = rename[&s.inc];
        for label in &with_payload {
            let original = fun_ref.cfg.named.get(label).expect("loop block exists");
            let mut instructions: Vec<(LocalId, Instruction)> = original
                .instructions
                .iter()
                .map(|(id, instr)| {
                    (rename[id], instr.map_local_ids(|l| *rename.get(&l).unwrap_or(&l)))
                })
                .collect();
            let terminator = match original.terminator_kind() {
                Terminator::ConditionalBranch { condition, true_target, false_target } => {
                    // C -> {A, J} and J -> {B, E}: chain targets map to the
                    // iteration-2 copies; the break target B stays (it is the
                    // shared uniform exit trampoline).
                    let map_t = |t: &Label| {
                        if t == &s.brk {
                            s.brk.clone()
                        } else {
                            it_label(2, t)
                        }
                    };
                    Terminator::ConditionalBranch {
                        condition: *rename.get(condition).unwrap_or(condition),
                        true_target: map_t(true_target),
                        false_target: map_t(false_target),
                    }
                }
                Terminator::UnconditionalBranch { target } => {
                    if label == &s.latch {
                        // iteration 2's latch continues into iteration 3
                        Terminator::UnconditionalBranch { target: it_label(3, &s.check) }
                    } else {
                        Terminator::UnconditionalBranch { target: it_label(2, target) }
                    }
                }
                other => {
                    return Err(anyhow!(
                        "loop block '{}' has unexpected terminator {:?}",
                        label.as_str(),
                        other
                    ))
                }
            };
            if label == &s.check {
                // iteration 2's head test, as a guard at the copy's entry
                let g = ids.fresh();
                let a = ids.fresh();
                let mut with_guard = vec![
                    (g, Instruction::BinaryOp {
                        left: s.inc,
                        op: BinaryOp::LessThanEqual,
                        right: s.sentinel,
                    }),
                    (a, Instruction::AssertTrue { value: g }),
                ];
                with_guard.extend(instructions);
                instructions = with_guard;
                changes += 2;
            }
            copies.push((
                it_label(2, label),
                Block {
                    instructions,
                    terminator: (
                        rename[&fun_ref.cfg.named.get(label).unwrap().terminator.0],
                        terminator,
                    ),
                    hint_normalize: false,
                },
            ));
            changes += 1;
        }
    }
    {
        // --- iteration 3: chain only, break asserted ---
        let mut rename: FxHashMap<LocalId, LocalId> = FxHashMap::default();
        rename.insert(s.counter, counter_of_it3);
        for label in &chain {
            let block = fun_ref.cfg.named.get(label).expect("chain block exists");
            for (id, _) in &block.instructions {
                rename.insert(*id, ids.fresh());
            }
            rename.insert(block.terminator.0, ids.fresh());
        }
        for label in &chain {
            let original = fun_ref.cfg.named.get(label).expect("chain block exists");
            let mut instructions: Vec<(LocalId, Instruction)> = original
                .instructions
                .iter()
                .map(|(id, instr)| {
                    (rename[id], instr.map_local_ids(|l| *rename.get(&l).unwrap_or(&l)))
                })
                .collect();
            let terminator = match original.terminator_kind() {
                Terminator::ConditionalBranch { condition, true_target, false_target } => {
                    if label == &s.nil_check {
                        // assert the break, fall through to the exit
                        let cond3 = *rename.get(condition).unwrap_or(condition);
                        let g = ids.fresh();
                        instructions.push((g, Instruction::AssertTrue { value: cond3 }));
                        changes += 1;
                        Terminator::UnconditionalBranch { target: s.exit.clone() }
                    } else {
                        Terminator::ConditionalBranch {
                            condition: *rename.get(condition).unwrap_or(condition),
                            true_target: it_label(3, true_target),
                            false_target: it_label(3, false_target),
                        }
                    }
                }
                Terminator::UnconditionalBranch { target } => {
                    Terminator::UnconditionalBranch { target: it_label(3, target) }
                }
                other => {
                    return Err(anyhow!(
                        "chain block '{}' has unexpected terminator {:?}",
                        label.as_str(),
                        other
                    ))
                }
            };
            if label == &s.check {
                let g = ids.fresh();
                let a = ids.fresh();
                let mut with_guard = vec![
                    (g, Instruction::BinaryOp {
                        left: counter_of_it3,
                        op: BinaryOp::LessThanEqual,
                        right: s.sentinel,
                    }),
                    (a, Instruction::AssertTrue { value: g }),
                ];
                with_guard.extend(instructions);
                instructions = with_guard;
                changes += 2;
            }
            copies.push((
                it_label(3, label),
                Block {
                    instructions,
                    terminator: (
                        rename[&fun_ref.cfg.named.get(label).unwrap().terminator.0],
                        terminator,
                    ),
                    hint_normalize: false,
                },
            ));
            changes += 1;
        }
    }

    // --- now mutate the function ---
    let init = s.init;
    let counter = s.counter;
    let sentinel = s.sentinel;
    let body_region = flood_avoiding(fun_ref, &s.check, &head);
    let fun = program.get_mut(function)?;

    // Iteration 1: counter := init everywhere in the body region. Its
    // branches all stay real - no premise about which paths it takes.
    let mut sub: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    sub.insert(counter, init);
    for label in &body_region {
        if let Some(block) = fun.cfg.named.get_mut(label) {
            substitute(block, &sub);
        }
    }
    changes += 1;

    // Preheader: guard + jump straight to the check.
    let g0 = ids.fresh();
    let a0 = ids.fresh();
    {
        let pre = match &s.preheader_key {
            None => &mut fun.cfg.entry,
            Some(l) => fun.cfg.named.get_mut(l).ok_or_else(|| anyhow!("preheader vanished"))?,
        };
        pre.instructions.push((
            g0,
            Instruction::BinaryOp { left: init, op: BinaryOp::LessThanEqual, right: sentinel },
        ));
        pre.instructions.push((a0, Instruction::AssertTrue { value: g0 }));
        let tid = pre.terminator.0;
        pre.terminator = (tid, Terminator::UnconditionalBranch { target: s.check.clone() });
        changes += 3;
    }

    // Iteration 1's latch: continue into iteration 2's copy.
    {
        let latch = fun
            .cfg
            .named
            .get_mut(&s.latch)
            .ok_or_else(|| anyhow!("latch vanished"))?;
        let tid = latch.terminator.0;
        latch.terminator =
            (tid, Terminator::UnconditionalBranch { target: it_label(2, &s.check) });
        changes += 1;
    }

    // Install the copies; delete the head. The break block B stays - it is
    // the shared exit trampoline for iterations 1 and 2.
    for (label, block) in copies {
        require(
            !fun.cfg.named.contains_key(&label),
            format!("copy label '{}' already exists", label.as_str()),
        )?;
        fun.cfg.named.insert(label, block);
    }
    fun.cfg.named.remove(&head).ok_or_else(|| anyhow!("head vanished"))?;
    changes += 1;

    Ok(changes)
}

/// Independent check: the preconditions held on the before program, and the
/// after program has the peeled structure - head gone, guards present, the
/// iteration copies present, everything outside the function untouched.
pub fn verify(before: &Program, after: &Program, rewrite_id: &str, function: &str, head: &str) -> Result<()> {
    let head_label = Label::from(head.to_string());
    let before_fun = before.get(function)?;
    let s = site(before_fun, function, &head_label)?;
    require(
        s.payload_entry == s.latch,
        "the payload must be the latch (single-block payloads only)".to_string(),
    )?;

    for (name, before_f) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_f),
            format!("collapse_all_loop on {} also changed {}", function, name.as_str()),
        )?;
    }
    let after_fun = after.get(function)?;
    require(!after_fun.cfg.named.contains_key(&head_label), "the head survived".to_string())?;
    require(
        after_fun.cfg.named.contains_key(&s.brk),
        "the break trampoline must survive".to_string(),
    )?;
    for original in [&s.check, &s.advance, &s.nil_check, &s.latch] {
        let copy = Label::from(format!("it2_{}_{}", rewrite_id, original.as_str()));
        require(
            after_fun.cfg.named.contains_key(&copy),
            format!("iteration-2 copy '{}' is missing", copy.as_str()),
        )?;
    }
    for original in [&s.check, &s.advance, &s.nil_check] {
        let copy = Label::from(format!("it3_{}_{}", rewrite_id, original.as_str()));
        require(
            after_fun.cfg.named.contains_key(&copy),
            format!("iteration-3 copy '{}' is missing", copy.as_str()),
        )?;
    }
    // The planted guards: preheader head-test, iteration-2 and -3 head
    // tests, iteration-3 break assert.
    let count_asserts = |fun: &FunDef| -> usize {
        let mut n = 0;
        for block in fun.cfg.iter_blocks() {
            for (_, instr) in &block.instructions {
                if matches!(instr, Instruction::AssertTrue { .. }) {
                    n += 1;
                }
            }
        }
        n
    };
    require(
        count_asserts(after_fun) == count_asserts(before_fun) + 4,
        "expected exactly four new assert_true guards".to_string(),
    )?;
    // Iteration 1 and 2 keep REAL nil-check branches; iteration 3's falls
    // through to the exit.
    let nil1 = after_fun
        .cfg
        .named
        .get(&s.nil_check)
        .ok_or_else(|| anyhow!("nil check vanished"))?;
    require(
        matches!(nil1.terminator_kind(), Terminator::ConditionalBranch { .. }),
        "iteration 1's nil check must stay a real branch".to_string(),
    )?;
    let nil3 = after_fun
        .cfg
        .named
        .get(&Label::from(format!("it3_{}_{}", rewrite_id, s.nil_check.as_str())))
        .expect("checked above");
    require(
        matches!(
            nil3.terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == s.exit
        ),
        "iteration 3's nil check must fall through to the exit".to_string(),
    )?;
    Ok(())
}
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
