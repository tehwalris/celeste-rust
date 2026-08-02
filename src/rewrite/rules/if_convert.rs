//! `if_convert` - turn a branch into a `select`.
//!
//! Pointed rule: names the join block.
//!
//! # The shape
//!
//! Only the *triangle*, which is what Lua's `and` / `or` compiles to and which
//! outnumbers the diamond in this program by 145 to 12:
//!
//! ```text
//!     H:  ...                        H:  ...
//!         cond_br %c ? A : J             ...arm...
//!     A:  ...arm...            =>        %p = select %c, %y, %x
//!         br J                           br J
//!     J:  %p = phi [H: %x, A: %y]    J:  ...
//! ```
//!
//! The arm may equally be the false target, in which case the select's operands
//! swap. `J` is left with one predecessor, and `merge_blocks` absorbs it later.
//!
//! # Why this is worth doing
//!
//! A `Phi` only means anything because the state was *split* so that each arm
//! saw its own lanes. That split is the whole problem: a frame ends as ~576
//! separate states and re-merging them is 60% of runtime. A `Select` computes
//! the same value without ever splitting.
//!
//! # Soundness, and the one thing that is not proved
//!
//! Moving the arm into `H` executes it on lanes that would have skipped it.
//! For the *value* that is harmless - the select discards it. For *execution*
//! it is not automatically harmless: a `load` of a field that happens to be nil
//! in a state that would have skipped the arm raises a type error the original
//! program never hit.
//!
//! The whitelist below excludes everything that mutates - stores, calls,
//! `create_if_missing`, `alloc` (which would leak a cell and change the heap
//! shape) and `assert_closure` (whose entire purpose is to fail). What it does
//! *not* exclude is instructions that can raise on values they should never
//! have seen, because restricting it to total instructions would reject
//! essentially every arm in the program - 389 of the moved instructions are
//! `load` and 129 are `get_field`.
//!
//! So that risk is accepted as a loud failure rather than proved away, exactly
//! as `assert_closure` is for `inline`: the interpreter's own type checking is
//! the guard, and a mis-speculated arm aborts instead of computing a wrong
//! number. `Select` itself is partial for the same reason - it refuses to
//! combine values it cannot represent per lane rather than widening them.

use anyhow::{anyhow, Result};

use crate::ir::{Block, Cfg, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::{get_block, predecessors, require};

/// Can this instruction be executed on lanes that would not have reached it?
///
/// This is the rule's whole soundness argument, so it is worth being precise
/// about what it is protecting against. Speculating an instruction can go wrong
/// in two quite different ways, and only one of them is a reason to refuse:
///
/// * **Silently.** The instruction changes something observable that the
///   `select` at the join cannot take back. A `store` writes a cell on lanes
///   that should not have written it; the join can still pick the right value
///   for the phi, but the heap is already wrong and nothing says so. There is no
///   rollback here - unlike a speculating CPU, which has one - so these must
///   never be speculated. No amount of testing would reliably find the damage.
///
/// * **Loudly.** The instruction cannot corrupt anything; it can only stop the
///   run. The asserts are the whole of this category: they exist in order to
///   fail. Speculating one can produce a failure that the original program
///   would not have had, but it cannot produce a *wrong answer*. That makes
///   allowing it a screening question rather than a soundness question, which
///   is the same bargain `inline` and `demote_create` already make.
///
/// So the answer is no for the first group and yes for the second. Note this
/// only ever *offers* more triangles: each one still has to be added to the
/// recipe and survive a differential run before it counts.
pub fn is_speculatable(instr: &Instruction) -> bool {
    match instr {
        // -- can go wrong silently: never speculate ------------------------
        //
        // Writes, in one form or another.
        Instruction::Store { .. }
        | Instruction::StoreEmptyTable { .. }
        | Instruction::StoreClosure { .. }
        | Instruction::Call { .. } => false,
        // Allocating would add a cell to the heap on a path that did not take
        // the branch, which changes `StateShape` and so changes which states
        // can merge - a silent cost, not a loud failure.
        Instruction::Alloc => false,
        // These mutate the heap when the field is missing. `demote_create`
        // exists to turn them into the plain read plus assert below.
        Instruction::GetGlobal { create_if_missing, .. }
        | Instruction::GetField { create_if_missing, .. }
        | Instruction::GetIndex { create_if_missing, .. } => !*create_if_missing,
        // A phi belongs to a control-flow join, so it cannot be moved. The arm
        // has a single predecessor and therefore has none anyway.
        Instruction::Phi { .. } => false,

        // -- can only go wrong loudly: allow, and screen --------------------
        //
        // Whether a field exists is a property of the state's heap rather than
        // of a lane, so within one state the answer is the same whichever arm
        // ran. Speculating this can only fail where the field is genuinely
        // absent, which is exactly what the `demote_create` entry that planted
        // it already claims never happens. One premise, checked once.
        Instruction::AssertPointer { .. } => true,
        // Reads the heap, but only to fail: whether a cell holds a plain value
        // is per-state, not per-lane, and a spurious failure on a state that
        // would have skipped the arm is loud. Same bargain as the two above.
        Instruction::AssertValueCell { .. } => true,
        // Weaker than the above and worth stating plainly: this one really can
        // fail where the unspeculated program would not. `if a.type == player
        // then a:method() end` speculates into asserting that a non-player is a
        // player. What saves it is that the failure is loud and immediate, and
        // that an arm is only offered when *every* instruction in it is
        // speculatable - so the body behind the assert is pure reads, and an
        // assert that passes cannot let anything through. 35 triangles are
        // blocked by nothing else; the ones whose claim does not hold fail
        // screening and are simply not applied.
        Instruction::AssertClosure { .. } => true,
        // A `Call` has to be refused because it says nothing about what it
        // calls. This one names its callee, and the name is always one of
        // `fixed_env::PURE_BUILTINS` - a function of its arguments that cannot
        // reach the heap and cannot branch. What is left is the assertion, and
        // that fails loudly like the others.
        Instruction::CallBuiltin { name, .. } => {
            crate::interpreter::fixed_env::is_pure_builtin(name)
        }

        // -- pure: the extra result is just ignored -------------------------
        Instruction::Load { .. }
        | Instruction::NumberConstant { .. }
        | Instruction::BoolConstant { .. }
        | Instruction::StringConstant { .. }
        | Instruction::NilConstant
        | Instruction::UnaryOp { .. }
        | Instruction::BinaryOp { .. }
        | Instruction::Select { .. } => true,
    }
}

/// The triangle around a join block, if there is one.
pub struct Triangle {
    /// The block with the conditional branch.
    pub head: Option<Label>,
    /// The block that runs only on one side of it.
    pub arm: Label,
    pub join: Label,
    pub condition: LocalId,
    /// Whether the arm is the *true* target, which decides the select's operand
    /// order.
    pub arm_is_true: bool,
}

/// Recognises the shape. Shared by `apply` and `candidates`; `verify` re-derives
/// it independently from the before program.
pub fn triangle_at(cfg: &Cfg, join: &Label) -> Option<Triangle> {
    find_triangle(cfg, join, true)
}

/// The triangle around a join whether or not its arm can be speculated. What
/// `blockers` walks, exposed so that a diagnostic can ask how much a *blocked*
/// triangle would be worth if the blocker were removed.
pub fn triangle_shaped(cfg: &Cfg, join: &Label) -> Option<Triangle> {
    find_triangle(cfg, join, false)
}

/// Triangles whose *shape* is right but whose arm cannot be speculated, with the
/// instructions standing in the way.
///
/// This is the list that says what to work on next. `if_convert` is the last
/// structural rule, so anything it cannot reach is a job for an earlier stage,
/// and which stage depends entirely on what these instructions are: `store` and
/// `get_field create` are heap traffic and belong to `promote_cell`, a `call` is
/// a builtin that would need its own guard, an `alloc` changes `StateShape`.
pub fn blockers(fun: &crate::ir::FunDef) -> Vec<(Label, Vec<String>)> {
    blocking_instructions(fun)
        .into_iter()
        .map(|(label, instructions)| {
            (
                label,
                instructions
                    .iter()
                    .map(|(_, i)| super::super::print::format_instruction(i))
                    .collect(),
            )
        })
        .collect()
}

/// The same thing with the ids kept, so a rule can be aimed at exactly the
/// instructions that are in the way rather than at every instruction of its
/// kind in the program. `demote_create` is the first caller: of the ~1400
/// `create` accessors in the program only these are worth the risk of
/// demoting, because only these unblock anything.
pub fn blocking_instructions(fun: &crate::ir::FunDef) -> Vec<(Label, Vec<(LocalId, Instruction)>)> {
    let mut out = Vec::new();
    for label in fun.cfg.named.keys() {
        if find_triangle(&fun.cfg, label, true).is_some() {
            continue;
        }
        let Some(triangle) = find_triangle(&fun.cfg, label, false) else { continue };
        let Some(arm) = get_block(&fun.cfg, &Some(triangle.arm)) else { continue };
        let blocking: Vec<(LocalId, Instruction)> = arm
            .instructions
            .iter()
            .filter(|(_, i)| !is_speculatable(i))
            .cloned()
            .collect();
        if !blocking.is_empty() {
            out.push((label.clone(), blocking));
        }
    }
    out.sort_by_key(|(l, _)| l.as_str().to_string());
    out
}

fn find_triangle(cfg: &Cfg, join: &Label, require_speculatable: bool) -> Option<Triangle> {
    let preds = predecessors(cfg);
    let join_key = Some(join.clone());
    let join_preds = preds.get(&join_key)?;
    if join_preds.len() != 2 {
        return None;
    }

    for (arm_key, head_key) in [
        (join_preds[0].clone(), join_preds[1].clone()),
        (join_preds[1].clone(), join_preds[0].clone()),
    ] {
        let Some(arm_label) = arm_key.clone() else { continue };
        if arm_key == join_key || head_key == join_key || arm_key == head_key {
            continue;
        }
        // The arm must run only for this branch.
        if preds.get(&arm_key).map(|p| p.as_slice()) != Some(&[head_key.clone()]) {
            continue;
        }
        let arm = get_block(cfg, &arm_key)?;
        if !matches!(
            arm.terminator_kind(),
            Terminator::UnconditionalBranch { target } if target == join
        ) {
            continue;
        }
        // A single-predecessor block should have no phis, but do not assume it.
        if arm
            .instructions
            .iter()
            .any(|(_, i)| matches!(i, Instruction::Phi { .. }))
        {
            continue;
        }
        if require_speculatable && !arm.instructions.iter().all(|(_, i)| is_speculatable(i)) {
            continue;
        }

        let head = get_block(cfg, &head_key)?;
        let Terminator::ConditionalBranch { condition, true_target, false_target } =
            head.terminator_kind()
        else {
            continue;
        };
        let arm_is_true = if true_target == &arm_label && false_target == join {
            true
        } else if false_target == &arm_label && true_target == join {
            false
        } else {
            continue;
        };

        // Every phi in the join must name exactly these two predecessors. That
        // follows from the predecessor check, but the phi could still be
        // malformed, and this rule must not paper over that.
        let join_block = get_block(cfg, &join_key)?;
        let head_label = super::label_of(&head_key);
        let ok = join_block.instructions.iter().all(|(_, i)| match i {
            Instruction::Phi { branches } => {
                branches.len() == 2
                    && branches.iter().any(|(l, _)| l == &head_label)
                    && branches.iter().any(|(l, _)| l == &arm_label)
            }
            _ => true,
        });
        if !ok {
            continue;
        }

        return Some(Triangle {
            head: head_key,
            arm: arm_label,
            join: join.clone(),
            condition: *condition,
            arm_is_true,
        });
    }
    None
}

pub fn apply(program: &mut Program, function: &str, join: &str) -> Result<usize> {
    let join = Label::from(join.to_string());
    let fun = program.get_mut(function)?;
    let cfg = &mut fun.cfg;

    let t = triangle_at(cfg, &join)
        .ok_or_else(|| anyhow!("no if-convertible triangle joins at '{}'", join.as_str()))?;
    let head_label = super::label_of(&t.head);

    // Take the arm's body and its own terminator id, then drop the block.
    let arm_block = cfg
        .named
        .remove(&t.arm)
        .ok_or_else(|| anyhow!("arm block '{}' vanished", t.arm.as_str()))?;

    // The phis become selects, in place, keeping their ids.
    let join_block = cfg
        .named
        .get_mut(&join)
        .ok_or_else(|| anyhow!("join block '{}' vanished", join.as_str()))?;
    let mut converted = 0;
    for (_, instr) in join_block.instructions.iter_mut() {
        let Instruction::Phi { branches } = instr else { continue };
        let from = |label: &Label| {
            branches
                .iter()
                .find(|(l, _)| l == label)
                .map(|(_, v)| *v)
                .expect("shape check guarantees both branches")
        };
        let (from_head, from_arm) = (from(&head_label), from(&t.arm));
        let (if_true, if_false) = if t.arm_is_true {
            (from_arm, from_head)
        } else {
            (from_head, from_arm)
        };
        *instr = Instruction::Select { condition: t.condition, if_true, if_false };
        converted += 1;
    }

    // Splice the arm into the head and make the branch unconditional.
    let head_block = super::get_block_mut(cfg, &t.head)
        .ok_or_else(|| anyhow!("head block vanished"))?;
    head_block.instructions.extend(arm_block.instructions);
    head_block.terminator = (
        head_block.terminator.0,
        Terminator::UnconditionalBranch { target: join.clone() },
    );

    // The arm's terminator id is gone and live ranges have changed, so any
    // existing allocation is stale. Re-run `allocate_slots` afterwards.
    cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());

    Ok(converted + 1)
}

/// Independent check.
///
/// Re-derives the shape from the *before* program and reconstructs what the
/// after program must look like, rather than trusting anything `apply` did.
pub fn verify(before: &Program, after: &Program, function: &str, join: &str) -> Result<()> {
    let join = Label::from(join.to_string());
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;

    let t = triangle_at(&before_fun.cfg, &join)
        .ok_or_else(|| anyhow!("no if-convertible triangle joins at '{}'", join.as_str()))?;
    let head_label = super::label_of(&t.head);

    let before_arm = get_block(&before_fun.cfg, &Some(t.arm.clone()))
        .ok_or_else(|| anyhow!("arm '{}' missing from the before program", t.arm.as_str()))?;
    let before_head = get_block(&before_fun.cfg, &t.head)
        .ok_or_else(|| anyhow!("head missing from the before program"))?;
    let before_join = get_block(&before_fun.cfg, &Some(join.clone()))
        .ok_or_else(|| anyhow!("join missing from the before program"))?;

    // Every moved instruction has to be safe to run unconditionally. Checked
    // here as well as in the shape recogniser, because this is the rule's whole
    // soundness argument and it must not depend on `apply` having looked.
    for (id, instr) in &before_arm.instructions {
        require(
            is_speculatable(instr),
            format!(
                "if_convert would speculate {}, which is not safe to run \
                 unconditionally",
                super::super::print::local_name(*id)
            ),
        )?;
    }

    require(
        !after_fun.cfg.named.contains_key(&t.arm),
        format!("if_convert left the arm '{}' behind", t.arm.as_str()),
    )?;

    // The head: its own instructions, then the arm's, then an unconditional
    // branch to the join under the same terminator id.
    let after_head = get_block(&after_fun.cfg, &t.head)
        .ok_or_else(|| anyhow!("if_convert removed the head block"))?;
    let expected: Vec<(LocalId, String)> = before_head
        .instructions
        .iter()
        .chain(before_arm.instructions.iter())
        .map(|(id, i)| (*id, super::super::print::format_instruction(i)))
        .collect();
    let actual: Vec<(LocalId, String)> = after_head
        .instructions
        .iter()
        .map(|(id, i)| (*id, super::super::print::format_instruction(i)))
        .collect();
    require(
        expected == actual,
        format!(
            "if_convert did not splice the arm into the head verbatim ({} instructions \
             expected, {} found)",
            expected.len(),
            actual.len()
        ),
    )?;
    require(
        after_head.terminator.0 == before_head.terminator.0
            && matches!(
                &after_head.terminator.1,
                Terminator::UnconditionalBranch { target } if target == &join
            ),
        "if_convert did not make the head branch unconditionally to the join",
    )?;

    // The join: each phi replaced by the corresponding select, everything else
    // untouched.
    let after_join = get_block(&after_fun.cfg, &Some(join.clone()))
        .ok_or_else(|| anyhow!("if_convert removed the join block"))?;
    require(
        before_join.instructions.len() == after_join.instructions.len(),
        "if_convert changed the number of instructions in the join",
    )?;
    for ((before_id, before_instr), (after_id, after_instr)) in
        before_join.instructions.iter().zip(&after_join.instructions)
    {
        require(before_id == after_id, "if_convert renumbered the join")?;
        let Instruction::Phi { branches } = before_instr else {
            require(
                super::super::print::format_instruction(before_instr)
                    == super::super::print::format_instruction(after_instr),
                "if_convert changed a non-phi instruction in the join",
            )?;
            continue;
        };
        let value_from = |label: &Label| -> Result<LocalId> {
            branches
                .iter()
                .find(|(l, _)| l == label)
                .map(|(_, v)| *v)
                .ok_or_else(|| anyhow!("phi has no branch for '{}'", label.as_str()))
        };
        let (if_true, if_false) = if t.arm_is_true {
            (value_from(&t.arm)?, value_from(&head_label)?)
        } else {
            (value_from(&head_label)?, value_from(&t.arm)?)
        };
        let expected = Instruction::Select { condition: t.condition, if_true, if_false };
        require(
            super::super::print::format_instruction(after_instr)
                == super::super::print::format_instruction(&expected),
            format!(
                "if_convert built the wrong select for {}: expected {}, found {}",
                super::super::print::local_name(*before_id),
                super::super::print::format_instruction(&expected),
                super::super::print::format_instruction(after_instr)
            ),
        )?;
    }

    // Nothing else moved.
    require(
        before.functions.len() == after.functions.len(),
        "if_convert must not add or remove functions",
    )?;
    for (name, before_other) in &before.functions {
        let after_other = after
            .functions
            .get(name)
            .ok_or_else(|| anyhow!("if_convert removed function {}", name.as_str()))?;
        if name.as_str() == function {
            continue;
        }
        require(
            super::super::print::format_function(before_other)
                == super::super::print::format_function(after_other),
            format!("if_convert changed unrelated function {}", name.as_str()),
        )?;
    }
    let untouched = |cfg: &Cfg, skip: &[Option<Label>]| -> Vec<(String, String)> {
        super::blocks_sorted(cfg)
            .into_iter()
            .filter(|k| !skip.contains(k))
            .filter_map(|k| {
                let b: &Block = get_block(cfg, &k)?;
                Some((
                    super::super::validate::block_label(&k),
                    super::super::print::format_block(b),
                ))
            })
            .collect()
    };
    let skip = [t.head.clone(), Some(t.arm.clone()), Some(join.clone())];
    require(
        untouched(&before_fun.cfg, &skip) == untouched(&after_fun.cfg, &skip),
        "if_convert changed a block outside the triangle",
    )?;

    Ok(())
}

/// Join blocks that `if_convert` would accept.
pub fn candidates(fun: &crate::ir::FunDef) -> Vec<Label> {
    let mut out: Vec<Label> = fun
        .cfg
        .named
        .keys()
        .filter(|l| triangle_at(&fun.cfg, l).is_some())
        .cloned()
        .collect();
    out.sort_by_key(|l| l.as_str().to_string());
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{FunDef, GlobalId};

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(s: &str) -> Label {
        Label::from(s.to_string())
    }

    fn num(n: i16) -> Instruction {
        Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(n) }
    }

    /// `head` conditionally runs `arm`, and `join` merges the two values.
    fn triangle_program(arm_body: Vec<(LocalId, Instruction)>, arm_is_true: bool) -> Program {
        let (true_target, false_target) = if arm_is_true {
            (label("arm"), label("join"))
        } else {
            (label("join"), label("arm"))
        };
        let mut named = crate::ir::new_label_map();
        named.insert(
            label("arm"),
            Block {
                instructions: arm_body,
                terminator: (id(20), Terminator::UnconditionalBranch { target: label("join") }),
                hint_normalize: false,
            },
        );
        named.insert(
            label("join"),
            Block {
                instructions: vec![(
                    id(30),
                    Instruction::Phi {
                        branches: vec![
                            (label("__entry"), id(1)),
                            (label("arm"), id(10)),
                        ],
                    },
                )],
                terminator: (id(31), Terminator::Return { value: Some(id(30)) }),
                hint_normalize: false,
            },
        );
        let cfg = Cfg::new(
            Block {
                instructions: vec![(id(0), Instruction::BoolConstant { value: true }), (id(1), num(5))],
                terminator: (
                    id(2),
                    Terminator::ConditionalBranch {
                        condition: id(0),
                        true_target,
                        false_target,
                    },
                ),
                hint_normalize: false,
            },
            named,
        );
        let mut functions = indexmap::IndexMap::new();
        functions.insert(
            GlobalId::from("t".to_string()),
            FunDef {
                name: GlobalId::from("t".to_string()),
                capture_ids: vec![],
                arg_ids: vec![],
                cfg,
                source_span: None,
            },
        );
        Program { functions }
    }

    #[test]
    fn converts_a_triangle_and_verifies() {
        let before = triangle_program(vec![(id(10), num(7))], true);
        let mut after = before.clone();
        apply(&mut after, "t", "join").unwrap();
        verify(&before, &after, "t", "join").unwrap();

        let fun = after.get("t").unwrap();
        assert!(!fun.cfg.named.contains_key(&label("arm")), "arm should be gone");
        // The arm's body moved into the head.
        assert_eq!(fun.cfg.entry.instructions.len(), 3);
        let join = fun.cfg.named.get(&label("join")).unwrap();
        assert!(matches!(
            join.instructions[0].1,
            Instruction::Select { condition, if_true, if_false }
                if condition == id(0) && if_true == id(10) && if_false == id(1)
        ), "{:?}", join.instructions[0].1);
    }

    /// When the arm is the *false* target the select's operands swap. Getting
    /// this backwards computes exactly the wrong answer, so it is worth its own
    /// test.
    #[test]
    fn operand_order_follows_which_target_the_arm_is() {
        let before = triangle_program(vec![(id(10), num(7))], false);
        let mut after = before.clone();
        apply(&mut after, "t", "join").unwrap();
        verify(&before, &after, "t", "join").unwrap();
        let join = after.get("t").unwrap().cfg.named.get(&label("join")).unwrap();
        assert!(matches!(
            join.instructions[0].1,
            Instruction::Select { if_true, if_false, .. }
                if if_true == id(1) && if_false == id(10)
        ), "{:?}", join.instructions[0].1);
    }

    /// The soundness condition. A store in the arm would run on lanes that
    /// never took the branch.
    #[test]
    fn refuses_an_arm_that_writes() {
        let before = triangle_program(
            vec![
                (id(10), num(7)),
                (id(11), Instruction::Store { target: id(1), source: id(10) }),
            ],
            true,
        );
        let mut after = before.clone();
        assert!(apply(&mut after, "t", "join").is_err());
    }

    /// `alloc` is excluded for a subtler reason than the other writes: it would
    /// add a heap cell on a path that did not take the branch, changing
    /// `StateShape` and so which states can merge - a silent cost rather than a
    /// loud failure.
    #[test]
    fn refuses_an_arm_that_allocates() {
        let before = triangle_program(vec![(id(10), Instruction::Alloc)], true);
        let mut after = before.clone();
        assert!(apply(&mut after, "t", "join").is_err());
    }

    /// The other side of the same line. An assert in the arm *is* accepted,
    /// because speculating it can only stop the run, never corrupt it - and an
    /// arm is only offered when everything in it is speculatable, so the body
    /// behind the assert is pure reads and an assert that passes cannot let
    /// anything through.
    #[test]
    fn accepts_an_arm_that_only_asserts() {
        for guard in [
            Instruction::AssertPointer { value: id(1) },
            Instruction::AssertClosure {
                value: id(1),
                fun_def: crate::ir::GlobalId::from("g".to_string()),
                captures: vec![],
            },
        ] {
            let before = triangle_program(vec![(id(11), guard), (id(10), num(7))], true);
            let mut after = before.clone();
            apply(&mut after, "t", "join").unwrap();
            verify(&before, &after, "t", "join").unwrap();
            assert!(
                !after.get("t").unwrap().cfg.named.contains_key(&label("arm")),
                "arm should be gone"
            );
        }
    }

    /// The verifier must reject a wrong conversion, not just confirm a right
    /// one - it is the half that is trusted.
    #[test]
    fn verify_rejects_swapped_select_operands() {
        let before = triangle_program(vec![(id(10), num(7))], true);
        let mut after = before.clone();
        apply(&mut after, "t", "join").unwrap();
        let join = after
            .get_mut("t")
            .unwrap()
            .cfg
            .named
            .get_mut(&label("join"))
            .unwrap();
        join.instructions[0].1 = Instruction::Select {
            condition: id(0),
            if_true: id(1),
            if_false: id(10),
        };
        let err = verify(&before, &after, "t", "join").unwrap_err();
        assert!(format!("{}", err).contains("wrong select"), "{}", err);
    }
}
