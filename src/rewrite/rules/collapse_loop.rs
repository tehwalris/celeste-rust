//! `collapse_loop` - collapse a counted loop to one guarded execution of its
//! body.
//!
//! # Why
//!
//! Room (1, 0) contains exactly one entity (checked against the cart's map
//! data), so the `objects` table is a singleton for the whole search: the
//! spawn object until the spawn animation ends, the player after. A probe
//! over the real abstract search confirmed `#objects == 1` in every lane of
//! every state through frame 40. Every `check`/`collide` loop, every
//! inlined `foreach(objects, ...)`, and the `del` search inside
//! `destroy_object` therefore runs its body exactly once - but still pays
//! the head block (phi, compare, conditional branch) per execution, still
//! keeps a `get_index objects[i]` with a varying index, and still counts as
//! a loop for every rule that must refuse loops. These loops are most of
//! `anonymous_61`, which is 64% of K.
//!
//! # The shape
//!
//! ```text
//!   P:    ..            br H          <- preheader, unconditional
//!   H:    i = phi [P: init, L: inc]   <- exactly this phi and this compare
//!         c = i <= bound
//!         br c ? B : X
//!   B:    ..body region..             <- untouched; may exit anywhere
//!   L:    .., inc = i + step          <- the latch, br H (may equal B)
//!   X:    ..                          <- the exit
//! ```
//!
//! After: `P` ends with `g = bound == init; assert_true g` and falls
//! through to `B`; `L` falls out to `X`; `H` is deleted and every use of
//! `i` becomes `init`. Nothing else changes - the body is not touched, so
//! early exits out of the loop keep working as before.
//!
//! # Soundness
//!
//! The guard states the premise out loud: `bound == init`, checked on every
//! lane, aborting the run if the table ever grows or shrinks (a death
//! empties `objects` for 15 frames; none is reachable by frame 40 today,
//! and the guard is what will tell us when that changes). Given the guard
//! and a statically positive constant `step`:
//!
//!   * the original loop enters its body exactly once, with `i = init`
//!     (`init <= bound` holds by equality), and
//!   * if the body reaches the latch, the second head test fails
//!     (`init + step <= init` is false for positive `step`) and control
//!     leaves to the exit - which is precisely the collapsed control flow.
//!
//! Substituting `init` for `i` is only valid where `i` still holds its
//! first-iteration value. That is true in blocks reachable from the body
//! entry without passing through the head again, and false on the head's
//! own exit edge (there `i` is `init + step`). The rule refuses any use of
//! `i` outside the always-`init` region, and refuses an exit-side phi that
//! carries `i` on the head edge.
//!
//! The head's phi and compare are pure, so deleting them changes no
//! observable behaviour. The compare must be used by nothing but the head's
//! own branch. The head must not carry `hint_normalize`; deleting a
//! normalization point would change how the state set fragments.

use anyhow::{anyhow, Result};
use rustc_hash::FxHashSet;

use crate::ir::{BinaryOp, FunDef, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::{blocks_sorted, get_block, predecessors, require, LocalIdAllocator};

struct Site {
    /// Key of the preheader block (`None` = the function entry).
    preheader_key: Option<Label>,
    latch: Label,
    body_entry: Label,
    exit: Label,
    /// The counter phi in the head.
    counter: LocalId,
    /// The counter's value on the preheader edge.
    init: LocalId,
    /// The counter's bound, `counter <= bound`.
    bound: LocalId,
}

/// Blocks reachable from `start` without entering `stop`. `start` itself is
/// included (unless it is `stop`).
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
        format!(
            "'{}' is a hint_normalize block; deleting it would change fragmentation",
            head.as_str()
        ),
    )?;

    // The head: exactly the counter phi, its compare, and the branch on it.
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
    require(
        head_block.instructions.len() == 2,
        format!("'{}' must hold exactly the counter phi and its compare", head.as_str()),
    )?;
    let (counter, counter_phi) = &head_block.instructions[0];
    let (compare, compare_instr) = &head_block.instructions[1];
    require(
        compare == condition,
        format!("'{}' must branch on its own compare", head.as_str()),
    )?;
    let Instruction::Phi { branches } = counter_phi else {
        return Err(anyhow!("'{}' does not start with the counter phi", head.as_str()));
    };
    require(
        branches.len() == 2,
        format!("the counter phi of '{}' must have exactly two edges", head.as_str()),
    )?;
    let Instruction::BinaryOp { left, op: BinaryOp::LessThanEqual, right: bound } = compare_instr
    else {
        return Err(anyhow!("'{}' does not compare `counter <= bound`", head.as_str()));
    };
    require(
        left == counter,
        format!("the compare of '{}' does not test the counter", head.as_str()),
    )?;

    // Tell the two phi edges apart: the latch edge carries `inc = counter +
    // step` computed in the latch block itself; the preheader edge carries
    // anything else. Exactly one assignment may fit.
    let mut interpretation: Option<(usize, LocalId)> = None; // (latch edge index, step)
    for (edge, (label, value)) in branches.iter().enumerate() {
        let Some(block) = fun.cfg.named.get(label) else { continue };
        let Some((_, instr)) = block.instructions.iter().find(|(id, _)| id == value) else {
            continue;
        };
        let Instruction::BinaryOp { left, op: BinaryOp::Plus, right } = instr else {
            continue;
        };
        if left != counter {
            continue;
        }
        require(
            interpretation.is_none(),
            format!("both edges of '{}' look like latch edges", head.as_str()),
        )?;
        interpretation = Some((edge, *right));
    }
    let Some((latch_edge, step)) = interpretation else {
        return Err(anyhow!(
            "no edge of '{}' carries `counter + step` from its own block",
            head.as_str()
        ));
    };
    let (latch, _inc) = branches[latch_edge].clone();
    let (preheader_label, init) = branches[1 - latch_edge].clone();

    // The step: a statically positive number, so `init + step <= init` is
    // false and the second head test must leave the loop.
    let step_def = fun
        .cfg
        .iter_blocks()
        .flat_map(|b| b.instructions.iter())
        .find(|(id, _)| *id == step);
    let Some((_, Instruction::NumberConstant { value: step_value })) = step_def else {
        return Err(anyhow!("the step of '{}' is not a number constant", head.as_str()));
    };
    require(
        (step_value.as_raw_u32() as i32) > 0,
        format!("the step of '{}' must be a positive constant", head.as_str()),
    )?;

    // The preheader: unconditional into the head, so the guard cannot fire
    // on a path that never entered the loop. The entry block is a valid
    // preheader; the phi then names it by its pseudo-label.
    let preheader_key = if preheader_label == super::entry_label() && !fun.cfg.named.contains_key(&preheader_label) {
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
        format!(
            "latch '{}' must branch unconditionally to '{}'",
            latch.as_str(),
            head.as_str()
        ),
    )?;

    // Nothing else may reach the head.
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
        format!(
            "'{}' must be reached only from its preheader and latch",
            head.as_str()
        ),
    )?;

    // Where does the counter still hold `init`? In blocks reachable from
    // the body entry without re-entering the head, minus blocks the exit
    // side can also reach (there the counter is `init + step`). Uses of the
    // counter must be confined to that region; the compare may be used only
    // by the head's own branch.
    let body_region = flood_avoiding(fun, &body_entry, head);
    let exit_region = flood_avoiding(fun, &exit, head);
    let always_init: FxHashSet<&Label> = body_region.difference(&exit_region).collect();
    for key in blocks_sorted(&fun.cfg) {
        let block = get_block(&fun.cfg, &key).expect("listed block exists");
        let in_head = key.as_ref() == Some(head);
        let in_always_init = matches!(&key, Some(l) if always_init.contains(l));
        for (id, instr) in &block.instructions {
            for used in instr.get_used_locals() {
                if used == *counter {
                    require(
                        in_always_init || (in_head && id == compare),
                        format!(
                            "the counter of '{}' is used in '{}', where it is not \
                             always `init`",
                            head.as_str(),
                            key.as_ref().map(|l| l.as_str()).unwrap_or("__entry"),
                        ),
                    )?;
                }
                require(
                    used != *compare || in_head,
                    format!("the compare of '{}' is used outside the head", head.as_str()),
                )?;
            }
            // An exit-side phi taking the counter on the head edge would
            // observe `init + step`; refuse. (A body-entry phi taking it
            // observes `init`, which the substitution preserves.)
            if let Instruction::Phi { branches } = instr {
                for (label, value) in branches {
                    if label == head {
                        require(
                            *value != *compare,
                            format!("a phi carries the compare of '{}'", head.as_str()),
                        )?;
                        require(
                            *value != *counter || key.as_ref() == Some(&body_entry),
                            format!(
                                "an exit-side phi carries the counter of '{}' on the \
                                 head edge",
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
                format!(
                    "the counter of '{}' is used by a terminator outside the loop",
                    head.as_str()
                ),
            )?;
            require(
                used != *compare || in_head,
                format!("the compare of '{}' is used outside the head", head.as_str()),
            )?;
        }
    }

    Ok(Site {
        preheader_key,
        latch,
        body_entry,
        exit,
        counter: *counter,
        init,
        bound: *bound,
    })
}

pub fn apply(program: &mut Program, function: &str, head: &str) -> Result<usize> {
    let head = Label::from(head.to_string());
    let fun = program.get(function)?;
    let s = site(fun, function, &head)?;
    let mut allocator = LocalIdAllocator::for_function(fun);
    let guard = allocator.fresh();
    let guard_assert = allocator.fresh();

    let fun = program.get_mut(function)?;

    // The preheader: state the premise, then fall into the body.
    let preheader = match &s.preheader_key {
        None => &mut fun.cfg.entry,
        Some(l) => fun.cfg.named.get_mut(l).expect("preheader exists"),
    };
    preheader.instructions.push((
        guard,
        Instruction::BinaryOp { left: s.bound, op: BinaryOp::TwoEqual, right: s.init },
    ));
    preheader
        .instructions
        .push((guard_assert, Instruction::AssertTrue { value: guard }));
    preheader.terminator.1 = Terminator::UnconditionalBranch { target: s.body_entry.clone() };

    // The latch: fall out to the exit.
    let latch = fun.cfg.named.get_mut(&s.latch).expect("latch exists");
    latch.terminator.1 = Terminator::UnconditionalBranch { target: s.exit.clone() };

    // The head is gone; the counter is its initial value everywhere.
    fun.cfg.named.remove(&head);
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

    // The head's successors see their incoming edge renamed: the body entry
    // is now entered from the preheader, the exit from the latch.
    let preheader_label = super::label_of(&s.preheader_key);
    if let Some(block) = fun.cfg.named.get_mut(&s.body_entry) {
        rename_phi_edges(block, &head, &preheader_label);
    }
    if let Some(block) = fun.cfg.named.get_mut(&s.exit) {
        rename_phi_edges(block, &head, &s.latch);
    }

    // Live ranges changed; any slot allocation is stale.
    fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(1)
}

fn rename_phi_edges(block: &mut crate::ir::Block, from: &Label, to: &Label) {
    for (_, instr) in block.instructions.iter_mut() {
        if let Instruction::Phi { branches } = instr {
            for (label, _) in branches.iter_mut() {
                if label == from {
                    *label = to.clone();
                }
            }
        }
    }
}

/// Independent check: re-derives the site from the *before* program -
/// including the trip-count argument and the counter-use confinement - and
/// insists the after program is exactly the prescription: the guard pair
/// appended to the preheader, the two retargets, the head gone, every use
/// of the counter now the initial value, the two phi-edge renames, and not
/// one other thing different.
pub fn verify(before: &Program, after: &Program, function: &str, head: &str) -> Result<()> {
    let head = Label::from(head.to_string());
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let s = site(before_fun, function, &head)?;

    require(
        !after_fun.cfg.named.contains_key(&head),
        "collapse_loop did not remove the head block",
    )?;
    require(
        after_fun.cfg.named.len() + 1 == before_fun.cfg.named.len(),
        "collapse_loop changed the set of blocks beyond removing the head",
    )?;

    // The guard pair: appended to the preheader, on fresh ids.
    let before_max = {
        let mut allocator = LocalIdAllocator::for_function(before_fun);
        allocator.fresh()
    };
    let before_preheader = get_block(&before_fun.cfg, &s.preheader_key).expect("site checked");
    let after_preheader = get_block(&after_fun.cfg, &s.preheader_key)
        .ok_or_else(|| anyhow!("collapse_loop removed the preheader"))?;
    let n = before_preheader.instructions.len();
    require(
        after_preheader.instructions.len() == n + 2,
        "the preheader must gain exactly the guard pair",
    )?;
    let (guard, guard_instr) = &after_preheader.instructions[n];
    let (guard_assert, assert_instr) = &after_preheader.instructions[n + 1];
    require(
        *guard >= before_max && *guard_assert >= before_max && guard != guard_assert,
        "the guard pair must use fresh, distinct ids",
    )?;
    require(
        *guard_instr
            == Instruction::BinaryOp { left: s.bound, op: BinaryOp::TwoEqual, right: s.init },
        "the guard must compare `bound == init`",
    )?;
    require(
        *assert_instr == Instruction::AssertTrue { value: *guard },
        "the guard must be stated with `assert_true`",
    )?;
    require(
        matches!(
            after_preheader.terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == s.body_entry
        ) && after_preheader.terminator.0 == before_preheader.terminator.0,
        "the preheader must fall into the body entry",
    )?;

    // Everything else: identical up to the substitution, the two retargets
    // and the two phi-edge renames.
    let substitute = |id: LocalId| if id == s.counter { s.init } else { id };
    let preheader_label = super::label_of(&s.preheader_key);
    for key in blocks_sorted(&before_fun.cfg) {
        if key.as_ref() == Some(&head) {
            continue;
        }
        let before_block = get_block(&before_fun.cfg, &key).expect("listed block exists");
        let after_block = get_block(&after_fun.cfg, &key).ok_or_else(|| {
            anyhow!(
                "collapse_loop removed block '{}'",
                key.as_ref().map(|l| l.as_str()).unwrap_or("__entry")
            )
        })?;

        let mut expected = before_block.clone();
        for (_, instr) in expected.instructions.iter_mut() {
            *instr = instr.map_local_ids(substitute);
        }
        expected.terminator.1 = expected.terminator.1.map_local_ids(substitute);
        if key == s.preheader_key {
            expected.instructions.push((*guard, guard_instr.clone()));
            expected.instructions.push((*guard_assert, assert_instr.clone()));
            expected.terminator.1 =
                Terminator::UnconditionalBranch { target: s.body_entry.clone() };
        }
        if key.as_ref() == Some(&s.latch) {
            expected.terminator.1 = Terminator::UnconditionalBranch { target: s.exit.clone() };
        }
        if key.as_ref() == Some(&s.body_entry) {
            rename_phi_edges(&mut expected, &head, &preheader_label);
        }
        if key.as_ref() == Some(&s.exit) {
            rename_phi_edges(&mut expected, &head, &s.latch);
        }
        require(
            *after_block == expected,
            format!(
                "collapse_loop changed block '{}' beyond the prescription",
                key.as_ref().map(|l| l.as_str()).unwrap_or("__entry")
            ),
        )?;
    }

    require(
        before.functions.len() == after.functions.len(),
        "collapse_loop changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!("collapse_loop on {} also changed {}", function, name.as_str()),
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
    use crate::ir::{Block, Cfg, GlobalId, UnaryOp};
    use crate::pico8_num::Pico8Num;
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(name: &str) -> Label {
        Label::from(name.to_string())
    }

    fn block(instructions: Vec<(usize, Instruction)>, terminator_id: usize, terminator: Terminator) -> Block {
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

    /// A first-match search loop over `objects`, the `check` shape:
    ///
    ///   entry: init=1, step=1, bound=#objects, table; br head
    ///   head:  i = phi [entry: init, latch: inc]; c = i <= bound;
    ///          br c ? body : for_join
    ///   body:  o = get_index table[i]; found = o == table (any compare);
    ///          br found ? hit : latch
    ///   latch: inc = i + step; br head
    ///   for_join: n = nil; br cont
    ///   hit:   br cont
    ///   cont:  r = phi [for_join: n, hit: o]; return r
    fn check_loop_program() -> Program {
        let entry = block(
            vec![
                (1, num(1)),                                          // init (also step)
                (2, Instruction::GetGlobal { name: "objects".to_string(), create_if_missing: false }),
                (3, Instruction::Load { source: id(2) }),
                (4, Instruction::UnaryOp { op: UnaryOp::Hash, arg: id(3) }),
            ],
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
                (7, Instruction::BinaryOp { left: id(6), op: BinaryOp::LessThanEqual, right: id(4) }),
            ],
            8,
            br_if(7, "body", "for_join"),
        );
        let body = block(
            vec![
                (9, Instruction::GetIndex { receiver: id(3), index: id(6), create_if_missing: false }),
                (11, Instruction::Load { source: id(9) }),
                (12, Instruction::BinaryOp { left: id(11), op: BinaryOp::TwoEqual, right: id(3) }),
            ],
            13,
            br_if(12, "hit", "latch"),
        );
        let latch = block(
            vec![(10, Instruction::BinaryOp { left: id(6), op: BinaryOp::Plus, right: id(1) })],
            14,
            br("head"),
        );
        let for_join = block(vec![(15, Instruction::NilConstant)], 16, br("cont"));
        let hit = block(vec![], 17, br("cont"));
        let cont = block(
            vec![(
                18,
                Instruction::Phi {
                    branches: vec![(label("for_join"), id(15)), (label("hit"), id(11))],
                },
            )],
            19,
            Terminator::Return { value: Some(id(18)) },
        );

        let mut named = crate::ir::new_label_map();
        named.insert(label("head"), head);
        named.insert(label("body"), body);
        named.insert(label("latch"), latch);
        named.insert(label("for_join"), for_join);
        named.insert(label("hit"), hit);
        named.insert(label("cont"), cont);
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
    fn collapses_a_check_loop() {
        let before = check_loop_program();
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        verify(&before, &after, "f", "head").unwrap();

        let fun = after.get("f").unwrap();
        assert!(!fun.cfg.named.contains_key(&label("head")));
        // The guard pair sits at the end of the entry block.
        let tail = &fun.cfg.entry.instructions[4..];
        assert_eq!(
            tail[0].1,
            Instruction::BinaryOp { left: id(4), op: BinaryOp::TwoEqual, right: id(1) }
        );
        assert!(matches!(tail[1].1, Instruction::AssertTrue { .. }));
        assert!(matches!(
            fun.cfg.entry.terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == label("body")
        ));
        // The latch leaves to the exit.
        assert!(matches!(
            fun.cfg.named[&label("latch")].terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == label("for_join")
        ));
        // The body indexes with `init` now.
        assert_eq!(
            fun.cfg.named[&label("body")].instructions[0].1,
            Instruction::GetIndex { receiver: id(3), index: id(1), create_if_missing: false }
        );
    }

    #[test]
    fn collapses_a_single_block_body() {
        // The inlined-`foreach` shape: the body is the latch.
        let mut program = check_loop_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            // body: o = t[i]; inc = i + step; br head - no early exit.
            let body = block(
                vec![
                    (9, Instruction::GetIndex { receiver: id(3), index: id(6), create_if_missing: false }),
                    (11, Instruction::Load { source: id(9) }),
                    (10, Instruction::BinaryOp { left: id(6), op: BinaryOp::Plus, right: id(1) }),
                ],
                13,
                br("head"),
            );
            fun.cfg.named.insert(label("body"), body);
            fun.cfg.named.remove(&label("latch"));
            fun.cfg.named.remove(&label("hit"));
            let head = fun.cfg.named.get_mut(&label("head")).unwrap();
            head.instructions[0].1 = Instruction::Phi {
                branches: vec![(super::super::entry_label(), id(1)), (label("body"), id(10))],
            };
            let cont = block(
                vec![(
                    18,
                    Instruction::Phi { branches: vec![(label("for_join"), id(15))] },
                )],
                19,
                Terminator::Return { value: Some(id(18)) },
            );
            fun.cfg.named.insert(label("cont"), cont);
        }
        let before = program.clone();
        let mut after = program;
        apply(&mut after, "f", "head").unwrap();
        verify(&before, &after, "f", "head").unwrap();
        let fun = after.get("f").unwrap();
        assert!(matches!(
            fun.cfg.named[&label("body")].terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == label("for_join")
        ));
    }

    #[test]
    fn refuses_a_conditional_preheader() {
        let mut program = check_loop_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            fun.cfg.entry.instructions.push((id(20), Instruction::BoolConstant { value: true }));
            fun.cfg.entry.terminator.1 = br_if(20, "head", "cont");
        }
        let err = apply(&mut program, "f", "head").unwrap_err();
        assert!(err.to_string().contains("unconditionally"), "{}", err);
    }

    #[test]
    fn refuses_a_counter_used_outside() {
        let mut program = check_loop_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            // `cont` is reachable from both the body and the exit side, so
            // the counter's value there is path-dependent.
            let cont = fun.cfg.named.get_mut(&label("cont")).unwrap();
            cont.instructions.push((
                id(20),
                Instruction::BinaryOp { left: id(6), op: BinaryOp::Plus, right: id(6) },
            ));
        }
        let err = apply(&mut program, "f", "head").unwrap_err();
        assert!(err.to_string().contains("not always `init`"), "{}", err);
    }

    #[test]
    fn refuses_a_nonpositive_step() {
        let mut program = check_loop_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            fun.cfg.entry.instructions[0].1 = num(0);
        }
        let err = apply(&mut program, "f", "head").unwrap_err();
        assert!(err.to_string().contains("positive"), "{}", err);
    }

    #[test]
    fn refuses_an_extra_head_predecessor() {
        let mut program = check_loop_program();
        {
            let fun = program.functions.values_mut().next().unwrap();
            let hit = fun.cfg.named.get_mut(&label("hit")).unwrap();
            hit.terminator.1 = br("head");
            // `cont` keeps its phi edges; the site check must fail before
            // anything looks at them.
        }
        let err = apply(&mut program, "f", "head").unwrap_err();
        assert!(
            err.to_string().contains("only from its preheader and latch"),
            "{}",
            err
        );
    }

    #[test]
    fn verify_rejects_a_missing_guard() {
        let before = check_loop_program();
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        {
            let fun = after.functions.values_mut().next().unwrap();
            fun.cfg.entry.instructions.truncate(4);
        }
        let err = verify(&before, &after, "f", "head").unwrap_err();
        assert!(err.to_string().contains("guard pair"), "{}", err);
    }

    #[test]
    fn verify_rejects_a_wrong_substitution() {
        let before = check_loop_program();
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        {
            let fun = after.functions.values_mut().next().unwrap();
            let body = fun.cfg.named.get_mut(&label("body")).unwrap();
            // Index with the bound instead of the initial value.
            body.instructions[0].1 =
                Instruction::GetIndex { receiver: id(3), index: id(4), create_if_missing: false };
        }
        let err = verify(&before, &after, "f", "head").unwrap_err();
        assert!(err.to_string().contains("beyond the prescription"), "{}", err);
    }

    #[test]
    fn finds_the_fixture_site() {
        let program = check_loop_program();
        let found = candidates(&program);
        assert_eq!(found, vec![("f".to_string(), label("head"))]);
    }
}
