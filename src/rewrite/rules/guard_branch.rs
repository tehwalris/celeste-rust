//! `guard_branch` - replace a conditional branch with a premise guard and an
//! unconditional jump to the recorded side.
//!
//! Pointed rule: names the *head* - the block whose conditional branch is
//! replaced - and the recorded direction (`taken`: `true` = the true edge).
//!
//! # The shape
//!
//! ```text
//!   H:  ...                                H:  ...
//!       br %c ? T : F          =>              %g = assert_true %c   (taken)
//!                                              br T
//!
//!   H:  ...                                H:  ...
//!       br %c ? T : F          =>              %n = not %c           (not taken)
//!                                              %g = assert_true %n
//!                                              br F
//! ```
//!
//! The not-taken target loses its phi branch for the head edge (the edge no
//! longer exists); if the head was its only predecessor it becomes
//! unreachable, and the `dce` that must follow in the recipe sweeps it.
//!
//! # Soundness
//!
//! The premise is "every lane takes the recorded edge", and `assert_true`
//! states it with exactly the runtime semantics the deopt machinery already
//! defines (`core_interpreter`'s `AssertTrue` arm, `deopt_collect`): a lane
//! that falsifies it is captured and re-run under the plain program, the
//! rest continue down the recorded edge. Case analysis on `%c`:
//!
//! * **Bool, all lanes as recorded**: the guard passes and the jump goes
//!   where the branch went. Identical.
//! * **Bool, some lanes off the record**: the branch would have split the
//!   state; the guard fails on those lanes and they deopt. The surviving
//!   lanes see exactly the recorded edge. Sound - deopted lanes are ground
//!   truth by construction.
//! * **`UnknownBool`**: the branch sends the state down both edges; the
//!   guard fails (an unconfirmable premise must not pass) and the whole
//!   state deopts. Sound, and loud in the screen.
//! * **Non-bool**: a `ConditionalBranch` applies Lua truthiness (numbers,
//!   strings and pointers are truthy, nil is falsy), but `assert_true` -
//!   and `not`, on the not-taken side - accept only bools. A site whose
//!   condition is a truthy non-bool therefore deopts on every lane even
//!   when the branch direction was recorded correctly. That is *sound*
//!   (over-strict guards only cost deopts, never wrong results) but a
//!   performance bug; the screen's deopt counts catch it, and such a site
//!   needs its condition decomposed to a bool first.
//!
//! The guard is strictly stronger than the branch's own bool handling, so
//! no state ever proceeds down the recorded edge that the original program
//! would not have sent there. The only behavioural delta is which lanes
//! run under plain - and plain is the definition of correct.
//!
//! # Why bother
//!
//! This is the workhorse of the trace-straightening campaign
//! (plans/rewrite-plan.md): the branch census shows the steady (1,0) window
//! executes 26 of 617 branch sites, none divergent. Guarding each executed
//! site at its recorded direction (and letting `dce` take the 591 dead ones)
//! leaves `__frame` straight-line - one block after `inline` and
//! `merge_blocks` - which both the interpreter and the SIMD kernel emitter
//! consume directly.

use anyhow::{anyhow, Result};

use crate::ir::{Block, FunDef, Instruction, Label, LocalId, Terminator, UnaryOp};

use super::super::program::Program;
use super::{get_block, get_block_mut, label_of, require, LocalIdAllocator};

/// The shape this rule accepts, re-derived identically by `apply` and
/// `verify`.
struct Site {
    /// The branched-on local, asserted (or `not`-ed and asserted) by the
    /// guard.
    condition: LocalId,
    /// Where every lane is recorded to go; the new unconditional target.
    taken_target: Label,
    /// The edge that disappears.
    not_taken: Label,
}

/// `"__entry"` names the entry block, like phis do.
fn head_key(head: &str) -> Option<Label> {
    if head == super::entry_label().as_str() {
        None
    } else {
        Some(Label::from(head.to_string()))
    }
}

fn site(fun: &FunDef, function: &str, head: &str, taken: bool) -> Result<Site> {
    let key = head_key(head);
    let block = get_block(&fun.cfg, &key)
        .ok_or_else(|| anyhow!("no block '{}' in {}", head, function))?;
    let Terminator::ConditionalBranch { condition, true_target, false_target } =
        block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in a conditional branch", head));
    };
    require(
        true_target != false_target,
        format!(
            "'{}' branches to '{}' on both edges; there is no direction to guard",
            head,
            true_target.as_str()
        ),
    )?;
    let (taken_target, not_taken) = if taken {
        (true_target.clone(), false_target.clone())
    } else {
        (false_target.clone(), true_target.clone())
    };
    // A guard stating "the back edge to myself is always taken" is a
    // recorded infinite loop; no real trace contains one.
    require(
        Some(&taken_target) != key.as_ref(),
        format!("'{}' would guard its own back edge as always taken", head),
    )?;
    Ok(Site { condition: *condition, taken_target, not_taken })
}

/// The phi fix the disappearing edge owes: every phi in the not-taken target
/// drops its branch for the head label. Shared shape between `apply` (which
/// performs it) and `verify` (which prescribes it).
fn drop_head_branch(block: &mut Block, head_label: &Label) {
    for (_, instr) in block.instructions.iter_mut() {
        if let Instruction::Phi { branches } = instr {
            branches.retain(|(label, _)| label != head_label);
        }
    }
}

pub fn apply(program: &mut Program, function: &str, head: &str, taken: bool) -> Result<usize> {
    let fun = program.get(function)?;
    let s = site(fun, function, head, taken)?;
    let mut alloc = LocalIdAllocator::for_function(fun);
    let guard: Vec<(LocalId, Instruction)> = if taken {
        vec![(alloc.fresh(), Instruction::AssertTrue { value: s.condition })]
    } else {
        let not_id = alloc.fresh();
        vec![
            (not_id, Instruction::UnaryOp { op: UnaryOp::Not, arg: s.condition }),
            (alloc.fresh(), Instruction::AssertTrue { value: not_id }),
        ]
    };
    let changes = guard.len() + 1;

    let key = head_key(head);
    let head_label = label_of(&key);
    let fun = program.get_mut(function)?;

    // The disappearing edge's phi branches first: the not-taken target can be
    // the head itself (a guarded loop exit drops its own back edge).
    let not_taken_block = get_block_mut(&mut fun.cfg, &Some(s.not_taken.clone()))
        .ok_or_else(|| anyhow!("no block '{}' in {}", s.not_taken.as_str(), function))?;
    drop_head_branch(not_taken_block, &head_label);

    let head_block = get_block_mut(&mut fun.cfg, &key).unwrap();
    head_block.instructions.extend(guard);
    head_block.terminator = (
        head_block.terminator.0,
        Terminator::UnconditionalBranch { target: s.taken_target.clone() },
    );

    // Live ranges changed; any slot allocation is stale.
    fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(changes)
}

/// Every id the before-function defines anywhere - a minted guard id must be
/// outside this set.
fn defined_ids(fun: &FunDef) -> rustc_hash::FxHashSet<LocalId> {
    let mut ids: rustc_hash::FxHashSet<LocalId> =
        fun.arg_ids.iter().flatten().chain(fun.capture_ids.iter()).copied().collect();
    for block in fun.cfg.iter_blocks() {
        for (id, _) in &block.instructions {
            ids.insert(*id);
        }
        ids.insert(block.terminator_id());
    }
    ids
}

/// Independent check: re-derives the site from the *before* program and
/// insists the after program is exactly the prescription - the head grew
/// exactly the guard (on fresh ids), the branch became an unconditional jump
/// to the recorded side, the not-taken target's phis lost exactly the head
/// edge, and not one other thing is different.
pub fn verify(
    before: &Program,
    after: &Program,
    function: &str,
    head: &str,
    taken: bool,
) -> Result<()> {
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let s = site(before_fun, function, head, taken)?;
    let key = head_key(head);
    let head_label = label_of(&key);
    let fresh = defined_ids(before_fun);

    // The head: instructions untouched (minus, when the not-taken target is
    // the head itself, its phis' own back-edge branches), then exactly the
    // guard, then the unconditional jump on the old terminator id.
    let not_taken_is_head = key.as_ref() == Some(&s.not_taken);
    let before_head = get_block(&before_fun.cfg, &key).unwrap();
    let after_head = get_block(&after_fun.cfg, &key)
        .ok_or_else(|| anyhow!("guard_branch removed the head block"))?;
    let mut expected_prefix = before_head.clone();
    if not_taken_is_head {
        drop_head_branch(&mut expected_prefix, &head_label);
    }
    let n = expected_prefix.instructions.len();
    let grew = if taken { 1 } else { 2 };
    require(
        after_head.instructions.len() == n + grew,
        "guard_branch did not grow the head by exactly the guard",
    )?;
    require(
        after_head.instructions[..n] == expected_prefix.instructions[..],
        "guard_branch changed the head before the guard",
    )?;
    let asserted = if taken {
        s.condition
    } else {
        let (not_id, not_instr) = &after_head.instructions[n];
        require(
            not_instr == &(Instruction::UnaryOp { op: UnaryOp::Not, arg: s.condition }),
            "the guard's `not` is not on the branched-on condition",
        )?;
        require(
            !fresh.contains(not_id),
            "the guard's `not` reuses an id the function already defines",
        )?;
        *not_id
    };
    let (assert_id, assert_instr) = after_head.instructions.last().unwrap();
    require(
        assert_instr == &(Instruction::AssertTrue { value: asserted }),
        "the head does not end with the prescribed assert_true",
    )?;
    require(
        !fresh.contains(assert_id),
        "the guard's assert reuses an id the function already defines",
    )?;
    require(
        *assert_id != asserted,
        "the guard's assert and its `not` share an id",
    )?;
    require(
        after_head.terminator.0 == before_head.terminator.0
            && matches!(
                &after_head.terminator.1,
                Terminator::UnconditionalBranch { target } if target == &s.taken_target
            )
            && after_head.hint_normalize == before_head.hint_normalize,
        "guard_branch did not make the head jump straight to the recorded side",
    )?;

    // The not-taken target: identical except its phis lost the head edge.
    // (When the not-taken target IS the head - a guarded loop exit dropping
    // its own back edge - the prefix check above already prescribed the phi
    // fix, so there is no separate block to compare.)
    if !not_taken_is_head {
        let before_nt = before_fun
            .cfg
            .named
            .get(&s.not_taken)
            .ok_or_else(|| anyhow!("no block '{}' before", s.not_taken.as_str()))?;
        let after_nt = after_fun
            .cfg
            .named
            .get(&s.not_taken)
            .ok_or_else(|| anyhow!("guard_branch removed '{}'", s.not_taken.as_str()))?;
        let mut expected = before_nt.clone();
        drop_head_branch(&mut expected, &head_label);
        require(
            *after_nt == expected,
            format!(
                "'{}' is not the before block with the head edge dropped from its phis",
                s.not_taken.as_str()
            ),
        )?;
    }

    // Everything else: untouched.
    for other_key in super::blocks_sorted(&before_fun.cfg) {
        if other_key == key || other_key.as_ref() == Some(&s.not_taken) {
            continue;
        }
        require(
            get_block(&before_fun.cfg, &other_key) == get_block(&after_fun.cfg, &other_key),
            format!(
                "guard_branch changed block '{}', which is outside the site",
                super::super::validate::block_label(&other_key)
            ),
        )?;
    }
    require(
        before_fun.cfg.named.len() == after_fun.cfg.named.len(),
        "guard_branch changed the set of blocks",
    )?;
    require(
        before.functions.len() == after.functions.len(),
        "guard_branch changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!("guard_branch on {} also changed {}", function, name.as_str()),
        )?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Cfg, GlobalId};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(s: &str) -> Label {
        Label::from(s.to_string())
    }

    /// `__entry` computes the condition, `head` branches on it, both arms
    /// define a value and rejoin at a phi in `join`.
    fn program() -> Program {
        let mut named = crate::ir::new_label_map();
        named.insert(
            label("head"),
            Block {
                instructions: vec![],
                terminator: (
                    id(9),
                    Terminator::ConditionalBranch {
                        condition: id(1),
                        true_target: label("t"),
                        false_target: label("f"),
                    },
                ),
                hint_normalize: false,
            },
        );
        named.insert(
            label("t"),
            Block {
                instructions: vec![(
                    id(10),
                    Instruction::NumberConstant {
                        value: crate::pico8_num::Pico8Num::from_i16(1),
                    },
                )],
                terminator: (id(11), Terminator::UnconditionalBranch { target: label("join") }),
                hint_normalize: false,
            },
        );
        named.insert(
            label("f"),
            Block {
                instructions: vec![(
                    id(20),
                    Instruction::NumberConstant {
                        value: crate::pico8_num::Pico8Num::from_i16(2),
                    },
                )],
                terminator: (id(21), Terminator::UnconditionalBranch { target: label("join") }),
                hint_normalize: false,
            },
        );
        named.insert(
            label("join"),
            Block {
                instructions: vec![(
                    id(30),
                    Instruction::Phi {
                        branches: vec![(label("t"), id(10)), (label("f"), id(20))],
                    },
                )],
                terminator: (id(31), Terminator::Return { value: Some(id(30)) }),
                hint_normalize: false,
            },
        );
        let cfg = Cfg::new(
            Block {
                instructions: vec![(id(1), Instruction::BoolConstant { value: true })],
                terminator: (id(8), Terminator::UnconditionalBranch { target: label("head") }),
                hint_normalize: false,
            },
            named,
        );
        let mut functions = IndexMap::new();
        functions.insert(
            GlobalId::from("g".to_string()),
            FunDef {
                name: GlobalId::from("g".to_string()),
                capture_ids: vec![],
                arg_ids: vec![],
                cfg,
                source_span: None,
            },
        );
        Program { functions, merge_partition_cells: Vec::new() }
    }

    #[test]
    fn guards_the_true_edge() {
        let before = program();
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "g", "head", true).unwrap(), 2);
        verify(&before, &after, "g", "head", true).unwrap();

        let fun = after.get("g").unwrap();
        let head = fun.cfg.named.get(&label("head")).unwrap();
        assert_eq!(head.instructions.len(), 1);
        assert_eq!(head.instructions[0].1, Instruction::AssertTrue { value: id(1) });
        assert_eq!(
            head.terminator,
            (id(9), Terminator::UnconditionalBranch { target: label("t") })
        );
        // `f` lost its edge; the join's phi lost the `f`... no - `f` still
        // jumps to the join. Only the HEAD -> f edge died, and `f` has no
        // phis. The join keeps both branches because `f` still reaches it
        // (unreachably, until dce).
        let join = fun.cfg.named.get(&label("join")).unwrap();
        let Instruction::Phi { branches } = &join.instructions[0].1 else { panic!() };
        assert_eq!(branches.len(), 2);
    }

    #[test]
    fn guards_the_false_edge_with_a_not() {
        let before = program();
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "g", "head", false).unwrap(), 3);
        verify(&before, &after, "g", "head", false).unwrap();

        let fun = after.get("g").unwrap();
        let head = fun.cfg.named.get(&label("head")).unwrap();
        assert_eq!(head.instructions.len(), 2);
        let (not_id, not_instr) = &head.instructions[0];
        assert_eq!(*not_instr, Instruction::UnaryOp { op: UnaryOp::Not, arg: id(1) });
        assert_eq!(head.instructions[1].1, Instruction::AssertTrue { value: *not_id });
        assert_eq!(
            head.terminator,
            (id(9), Terminator::UnconditionalBranch { target: label("f") })
        );
    }

    #[test]
    fn validates_after_apply() {
        for taken in [true, false] {
            let mut after = program();
            apply(&mut after, "g", "head", taken).unwrap();
            let errors = super::super::super::validate::validate_program(&after);
            assert!(errors.is_empty(), "taken={}: {:?}", taken, errors);
        }
    }

    /// A not-taken target whose phi names the head loses that branch, so the
    /// phi/predecessor invariant holds without waiting for dce.
    #[test]
    fn drops_the_head_edge_from_the_not_taken_phis() {
        let mut before = program();
        {
            let fun = before.get_mut("g").unwrap();
            // Rewire: head's false edge goes straight to the join, which
            // gains a phi branch for it.
            let head = fun.cfg.named.get_mut(&label("head")).unwrap();
            head.terminator = (
                id(9),
                Terminator::ConditionalBranch {
                    condition: id(1),
                    true_target: label("t"),
                    false_target: label("join"),
                },
            );
            fun.cfg.named.remove(&label("f"));
            let join = fun.cfg.named.get_mut(&label("join")).unwrap();
            join.instructions[0].1 = Instruction::Phi {
                branches: vec![(label("t"), id(10)), (label("head"), id(1))],
            };
        }
        let mut after = before.clone();
        apply(&mut after, "g", "head", true).unwrap();
        verify(&before, &after, "g", "head", true).unwrap();

        let fun = after.get("g").unwrap();
        let join = fun.cfg.named.get(&label("join")).unwrap();
        let Instruction::Phi { branches } = &join.instructions[0].1 else { panic!() };
        assert_eq!(branches, &vec![(label("t"), id(10))]);
        let errors = super::super::super::validate::validate_program(&after);
        assert!(errors.is_empty(), "{:?}", errors);
    }

    #[test]
    fn refuses_an_unconditional_head() {
        let mut before = program();
        let error = apply(&mut before, "g", "t", true).unwrap_err().to_string();
        assert!(error.contains("conditional branch"), "{}", error);
    }

    #[test]
    fn refuses_equal_targets() {
        let mut before = program();
        {
            let fun = before.get_mut("g").unwrap();
            let head = fun.cfg.named.get_mut(&label("head")).unwrap();
            head.terminator = (
                id(9),
                Terminator::ConditionalBranch {
                    condition: id(1),
                    true_target: label("t"),
                    false_target: label("t"),
                },
            );
        }
        let error = apply(&mut before, "g", "head", true).unwrap_err().to_string();
        assert!(error.contains("both edges"), "{}", error);
    }

    #[test]
    fn refuses_guarding_a_back_edge_as_taken() {
        let mut before = program();
        {
            let fun = before.get_mut("g").unwrap();
            let head = fun.cfg.named.get_mut(&label("head")).unwrap();
            head.terminator = (
                id(9),
                Terminator::ConditionalBranch {
                    condition: id(1),
                    true_target: label("head"),
                    false_target: label("t"),
                },
            );
        }
        let error = apply(&mut before, "g", "head", true).unwrap_err().to_string();
        assert!(error.contains("back edge"), "{}", error);
    }

    /// Dropping a loop's own back edge is the useful self-referential case:
    /// the not-taken target is the head, whose phis lose the self edge.
    #[test]
    fn guards_a_loop_exit_dropping_the_back_edge() {
        let mut before = program();
        {
            let fun = before.get_mut("g").unwrap();
            let head = fun.cfg.named.get_mut(&label("head")).unwrap();
            head.instructions = vec![(
                id(2),
                Instruction::Phi {
                    branches: vec![(label("__entry"), id(1)), (label("head"), id(1))],
                },
            )];
            head.terminator = (
                id(9),
                Terminator::ConditionalBranch {
                    condition: id(1),
                    true_target: label("t"),
                    false_target: label("head"),
                },
            );
            fun.cfg.named.remove(&label("f"));
            let join = fun.cfg.named.get_mut(&label("join")).unwrap();
            join.instructions[0].1 =
                Instruction::Phi { branches: vec![(label("t"), id(10))] };
        }
        let mut after = before.clone();
        apply(&mut after, "g", "head", true).unwrap();
        verify(&before, &after, "g", "head", true).unwrap();

        let fun = after.get("g").unwrap();
        let head = fun.cfg.named.get(&label("head")).unwrap();
        let Instruction::Phi { branches } = &head.instructions[0].1 else { panic!() };
        assert_eq!(branches, &vec![(label("__entry"), id(1))]);
        let errors = super::super::super::validate::validate_program(&after);
        assert!(errors.is_empty(), "{:?}", errors);
    }

    /// The verifier is the trusted half: it must reject an applier that
    /// jumped to the wrong side...
    #[test]
    fn verify_rejects_the_wrong_target() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "g", "head", true).unwrap();
        let fun = after.get_mut("g").unwrap();
        let head = fun.cfg.named.get_mut(&label("head")).unwrap();
        head.terminator = (id(9), Terminator::UnconditionalBranch { target: label("f") });
        let error = verify(&before, &after, "g", "head", true).unwrap_err().to_string();
        assert!(error.contains("recorded side"), "{}", error);
    }

    /// ...one that asserted the wrong value...
    #[test]
    fn verify_rejects_a_tampered_assert() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "g", "head", true).unwrap();
        let fun = after.get_mut("g").unwrap();
        let head = fun.cfg.named.get_mut(&label("head")).unwrap();
        let last = head.instructions.last_mut().unwrap();
        last.1 = Instruction::AssertTrue { value: id(10) };
        let error = verify(&before, &after, "g", "head", true).unwrap_err().to_string();
        assert!(error.contains("prescribed assert_true"), "{}", error);
    }

    /// ...and one that recycled an existing id for the guard.
    #[test]
    fn verify_rejects_a_recycled_guard_id() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "g", "head", true).unwrap();
        let fun = after.get_mut("g").unwrap();
        let head = fun.cfg.named.get_mut(&label("head")).unwrap();
        let last = head.instructions.last_mut().unwrap();
        last.0 = id(10);
        let error = verify(&before, &after, "g", "head", true).unwrap_err().to_string();
        assert!(error.contains("reuses an id"), "{}", error);
    }
}
