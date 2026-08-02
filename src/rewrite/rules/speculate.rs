//! `speculate` - hoist an arm's speculatable instructions into the head,
//! leaving only its stores behind.
//!
//! # Why
//!
//! `sink_store` moves a store out of a triangle's arm, but to do that it must
//! load the store's cell *before* the branch - which requires the store's
//! target pointer to be defined before the branch. In three of the four
//! store-blocked triangles it is not: the arm computes the pointer itself.
//!
//! ```text
//!   if_body_97:                          <- the arm
//!     %184 = get_field %2.djump          <- the target, computed in the arm
//!     %5773 = assert_pointer %184
//!     %185 = get_global "max_djump"
//!     %186 = load %185
//!     %187 = store %184 <- %186          <- the store `sink_store` wants
//! ```
//!
//! This rule is the speculation half of `if_convert`, done early: every
//! instruction of the arm that `if_convert` would be willing to run
//! unconditionally moves into the head, in order, while the branch stays. The
//! arm is left holding only its stores, whose operands now dominate the
//! branch, which is exactly the state `sink_store` needs. The pipeline for one
//! blocked triangle is `speculate`, then `sink_store` per store (innermost
//! last-instruction first), then `if_convert` on the emptied arm.
//!
//! # Soundness
//!
//! Two separate arguments, one per direction of movement.
//!
//! **Running hoisted instructions on lanes that skip the arm** is `if_convert`'s
//! own bargain, unchanged: the hoisted set is admitted by the same
//! `is_speculatable` predicate, so nothing in it can go wrong silently - at
//! worst it fails loudly on a state the original program would not have
//! touched, and that is a screening question. See `if_convert` for the full
//! argument.
//!
//! **Moving a hoisted instruction above a store that preceded it in the arm**
//! is this rule's own obligation, and it is the reason `commutes_with_store`
//! exists. An instruction that reads the heap and crosses a write to the same
//! cell would compute a different value - *silently*. So each hoisted
//! instruction is checked against every store it crosses:
//!
//!   * Instructions that never touch the heap (constants, arithmetic,
//!     `select`, `assert_pointer` - which inspects a local, not a cell)
//!     commute with any store.
//!   * Non-creating accessors commute with any store: a `store` writes a
//!     cell's *contents* (`heap.set(cell, Value(..))`) and cannot change which
//!     cell a name maps to, which is all an accessor reads.
//!   * A pure `call_builtin` is a function of its argument values.
//!   * A `load` reads exactly what a store writes, so it may only cross a
//!     store to a **provably different cell** - see `cells_provably_distinct`.
//!   * `assert_closure` dereferences the heap (the closure lives in a cell a
//!     store could overwrite), so it never crosses a store. In practice the
//!     asserts sit before the stores they guard, so nothing is lost.

use anyhow::{anyhow, Result};

use crate::ir::{FunDef, Instruction, Label, LocalId};

use super::super::program::Program;
use super::if_convert::{is_speculatable, triangle_at, triangle_shaped, Triangle};
use super::{get_block, get_block_mut, require};

/// Are the cells behind two pointer locals certainly different?
///
/// Two facts, each an instance of the same principle: a name maps to one
/// cell, cells are only ever minted fresh, and nothing can bind one cell to
/// two names. Anything these cannot prove is treated as aliasing.
///
///   * Two `get_field`s on the **same base local** with **different field
///     names** yield different cells. Same local means same record pointer
///     (pointers are per-state, not per-lane), and a record maps each name
///     to one cell.
///   * A **global's cell and a field or index cell** are always different:
///     global cells are minted by the global environment, field and index
///     cells by their table's creating accessors - disjoint `alloc`s, and no
///     instruction rebinds a name to an existing cell. Two globals with
///     different names are distinct for the same reason.
fn cells_provably_distinct(fun: &FunDef, a: LocalId, b: LocalId) -> bool {
    match (defining_instruction(fun, a), defining_instruction(fun, b)) {
        (
            Some(Instruction::GetField { receiver: ra, field: fa, .. }),
            Some(Instruction::GetField { receiver: rb, field: fb, .. }),
        ) => ra == rb && fa != fb,
        (
            Some(Instruction::GetGlobal { .. }),
            Some(Instruction::GetField { .. } | Instruction::GetIndex { .. }),
        )
        | (
            Some(Instruction::GetField { .. } | Instruction::GetIndex { .. }),
            Some(Instruction::GetGlobal { .. }),
        ) => true,
        (
            Some(Instruction::GetGlobal { name: na, .. }),
            Some(Instruction::GetGlobal { name: nb, .. }),
        ) => na != nb,
        _ => false,
    }
}

fn defining_instruction(fun: &FunDef, id: LocalId) -> Option<&Instruction> {
    for block in fun.cfg.iter_blocks() {
        for (candidate, instr) in &block.instructions {
            if *candidate == id {
                return Some(instr);
            }
        }
    }
    None
}

/// May `instr` move above a store whose target is `store_target`?
fn commutes_with_store(fun: &FunDef, instr: &Instruction, store_target: LocalId) -> bool {
    match instr {
        // Never touch the heap. `assert_pointer` inspects the local value
        // (pointer or nil), which a store cannot change.
        Instruction::NumberConstant { .. }
        | Instruction::BoolConstant { .. }
        | Instruction::StringConstant { .. }
        | Instruction::NilConstant
        | Instruction::UnaryOp { .. }
        | Instruction::BinaryOp { .. }
        | Instruction::Select { .. }
        | Instruction::AssertPointer { .. }
        | Instruction::AssertTrue { .. } => true,
        // Structure reads: a store writes cell contents, never the name-to-cell
        // mapping these read. The creating variants mutate and must not move,
        // but they are not speculatable in the first place.
        Instruction::GetGlobal { create_if_missing, .. }
        | Instruction::GetField { create_if_missing, .. }
        | Instruction::GetIndex { create_if_missing, .. } => !*create_if_missing,
        // A function of its argument values, by the `PURE_BUILTINS` contract.
        Instruction::CallBuiltin { name, .. } => {
            crate::interpreter::fixed_env::is_pure_builtin(name)
        }
        // Reads the very thing a store writes.
        Instruction::Load { source } => cells_provably_distinct(fun, *source, store_target),
        // Reads only the stored value's tag, but that is still the cell's
        // contents - same requirement as a load.
        Instruction::AssertValueCell { target } => {
            cells_provably_distinct(fun, *target, store_target)
        }
        // Dereferences the closure's cell, which a store could overwrite.
        Instruction::AssertClosure { .. } => false,
        // Nothing else is speculatable, so nothing else is ever hoisted; if it
        // were, refusing is the safe answer.
        _ => false,
    }
}

/// The shape this rule accepts, re-derived identically by `apply`, `verify`
/// and `candidates`: an arm whose non-speculatable instructions are all plain
/// stores, with every hoist commuting with every store it crosses.
///
/// Without `arm`, the arm is the one arm of the triangle at `join`, and a
/// triangle `if_convert` could already take whole is refused. With `arm`, it
/// is that named block: one side of any conditional branch, rejoining at
/// `join` - a *diamond* arm, or the arm of a triangle whose join has other
/// predecessors and is therefore invisible to `triangle_shaped`. A diamond
/// has two arms, so each gets its own entry. The shape requirement is exactly
/// what the soundness argument needs and nothing more: the arm's only
/// predecessor is the head whose branch names it, so the arm runs iff that
/// edge is taken. Hoisted instructions run on lanes that took the other edge
/// (`is_speculatable`'s bargain), and within the arm they move only above
/// that arm's own stores (`commutes_with_store`'s). No other arm's store is
/// ever crossed - those stay behind their own branch.
fn plan(fun: &FunDef, join: &Label, arm_label: Option<&Label>) -> Result<Triangle> {
    let t = match arm_label {
        None => {
            require(
                triangle_at(&fun.cfg, join).is_none(),
                format!(
                    "the triangle at '{}' is already fully speculatable; use if_convert",
                    join.as_str()
                ),
            )?;
            triangle_shaped(&fun.cfg, join)
                .ok_or_else(|| anyhow!("no triangle joins at '{}'", join.as_str()))?
        }
        Some(arm_label) => {
            let arm_key = Some(arm_label.clone());
            let preds = super::predecessors(&fun.cfg);
            let head_key = match preds.get(&arm_key).map(|p| p.as_slice()) {
                Some([head]) => head.clone(),
                _ => {
                    return Err(anyhow!(
                        "'{}' does not hang off a single predecessor, so it is \
                         not an arm",
                        arm_label.as_str()
                    ))
                }
            };
            let head = get_block(&fun.cfg, &head_key)
                .ok_or_else(|| anyhow!("head block vanished"))?;
            let crate::ir::Terminator::ConditionalBranch {
                condition,
                true_target,
                false_target,
            } = head.terminator_kind()
            else {
                return Err(anyhow!(
                    "the predecessor of '{}' does not branch conditionally",
                    arm_label.as_str()
                ));
            };
            let arm_is_true = if true_target == arm_label && false_target != arm_label {
                true
            } else if false_target == arm_label && true_target != arm_label {
                false
            } else {
                return Err(anyhow!(
                    "'{}' is not exactly one target of its predecessor's branch",
                    arm_label.as_str()
                ));
            };
            let arm_block = get_block(&fun.cfg, &arm_key).unwrap();
            require(
                matches!(
                    arm_block.terminator_kind(),
                    crate::ir::Terminator::UnconditionalBranch { target } if target == join
                ),
                format!(
                    "'{}' does not rejoin at '{}'",
                    arm_label.as_str(),
                    join.as_str()
                ),
            )?;
            require(
                !arm_block
                    .instructions
                    .iter()
                    .any(|(_, i)| matches!(i, Instruction::Phi { .. })),
                format!("'{}' contains phis, which cannot move", arm_label.as_str()),
            )?;
            Triangle {
                head: head_key,
                arm: arm_label.clone(),
                join: join.clone(),
                condition: *condition,
                arm_is_true,
            }
        }
    };
    let arm = get_block(&fun.cfg, &Some(t.arm.clone()))
        .ok_or_else(|| anyhow!("arm block '{}' vanished", t.arm.as_str()))?;

    let mut hoists = 0;
    for (index, (id, instr)) in arm.instructions.iter().enumerate() {
        if is_speculatable(instr) {
            hoists += 1;
            // This instruction will move above every store before it.
            for (_, earlier) in &arm.instructions[..index] {
                let Instruction::Store { target, .. } = earlier else { continue };
                require(
                    commutes_with_store(fun, instr, *target),
                    format!(
                        "hoisting {} = `{}` would move it above `store %{} <- ..`, \
                         and they may touch the same cell",
                        super::super::print::local_name(*id),
                        super::super::print::format_instruction(instr),
                        usize::from(*target),
                    ),
                )?;
            }
        } else {
            require(
                matches!(instr, Instruction::Store { .. }),
                format!(
                    "the arm of '{}' is blocked by {} = `{}`, which is not a store; \
                     speculate only clears the way for sink_store",
                    join.as_str(),
                    super::super::print::local_name(*id),
                    super::super::print::format_instruction(instr),
                ),
            )?;
        }
    }
    require(
        hoists > 0,
        format!("the arm of '{}' has nothing to hoist", join.as_str()),
    )?;
    Ok(t)
}

pub fn apply(
    program: &mut Program,
    function: &str,
    join: &str,
    arm: Option<&str>,
) -> Result<usize> {
    let join = Label::from(join.to_string());
    let arm = arm.map(|a| Label::from(a.to_string()));
    let fun = program.get(function)?;
    let t = plan(fun, &join, arm.as_ref())?;

    let fun = program.get_mut(function)?;
    let arm = get_block_mut(&mut fun.cfg, &Some(t.arm.clone())).unwrap();
    let (hoisted, kept): (Vec<_>, Vec<_>) = arm
        .instructions
        .drain(..)
        .partition(|(_, instr)| is_speculatable(instr));
    arm.instructions = kept;
    let moved = hoisted.len();
    let head = get_block_mut(&mut fun.cfg, &t.head).unwrap();
    head.instructions.extend(hoisted);
    Ok(moved)
}

/// Independent check.
///
/// Re-derives the shape, the hoisted set and the commute obligations from the
/// *before* program via `plan`, then insists the after program is exactly
/// that: head extended by the hoisted instructions in order, arm reduced to
/// its stores in order, and not one other thing different.
pub fn verify(
    before: &Program,
    after: &Program,
    function: &str,
    join: &str,
    arm: Option<&str>,
) -> Result<()> {
    let join = Label::from(join.to_string());
    let arm = arm.map(|a| Label::from(a.to_string()));
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let t = plan(before_fun, &join, arm.as_ref())?;

    let before_arm = get_block(&before_fun.cfg, &Some(t.arm.clone())).unwrap();
    let before_head = get_block(&before_fun.cfg, &t.head).unwrap();
    let (hoisted, kept): (Vec<_>, Vec<_>) = before_arm
        .instructions
        .iter()
        .cloned()
        .partition(|(_, instr)| is_speculatable(instr));

    let after_head = get_block(&after_fun.cfg, &t.head)
        .ok_or_else(|| anyhow!("speculate removed the head block"))?;
    let expected_head: Vec<(LocalId, Instruction)> = before_head
        .instructions
        .iter()
        .cloned()
        .chain(hoisted)
        .collect();
    require(
        after_head.instructions == expected_head,
        "speculate did not append exactly the hoisted instructions to the head",
    )?;
    require(
        after_head.terminator == before_head.terminator,
        "speculate changed the head's terminator",
    )?;

    let after_arm = get_block(&after_fun.cfg, &Some(t.arm.clone()))
        .ok_or_else(|| anyhow!("speculate removed the arm block"))?;
    require(
        after_arm.instructions == kept,
        "speculate did not leave exactly the arm's stores behind, in order",
    )?;
    require(
        after_arm.terminator == before_arm.terminator,
        "speculate changed the arm's terminator",
    )?;

    // Nothing else: not the join, not any other block, not any other function.
    require(
        before.functions.len() == after.functions.len(),
        "speculate changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!("speculate on {} also changed {}", function, name.as_str()),
        )?;
    }
    let skip = [t.head.clone(), Some(t.arm.clone())];
    for key in super::blocks_sorted(&before_fun.cfg) {
        if skip.contains(&key) {
            continue;
        }
        let before_block = get_block(&before_fun.cfg, &key);
        let after_block = get_block(&after_fun.cfg, &key);
        require(
            before_block == after_block,
            format!(
                "speculate changed block '{}', which is outside the triangle",
                super::super::validate::block_label(&key)
            ),
        )?;
    }
    require(
        before_fun.cfg.named.len() == after_fun.cfg.named.len(),
        "speculate changed the set of blocks",
    )?;

    Ok(())
}

/// Arms this rule accepts: blocked, store-only blockers, commutes clean.
/// Plain-triangle arms come back with `None`; every other arm - diamond arms,
/// arms whose join has extra predecessors - names itself, one entry per arm.
pub fn candidates(program: &Program) -> Vec<(String, Label, Option<Label>)> {
    let mut out = Vec::new();
    for (name, fun) in &program.functions {
        for label in fun.cfg.named.keys() {
            if plan(fun, label, None).is_ok() {
                out.push((name.as_str().to_string(), label.clone(), None));
            }
            let block = fun.cfg.named.get(label).unwrap();
            let Some(join) = super::unconditional_target(block.terminator_kind()) else {
                continue;
            };
            // A plain triangle's arm is already offered without a name.
            if triangle_shaped(&fun.cfg, join).is_some_and(|t| &t.arm == label) {
                continue;
            }
            if plan(fun, join, Some(label)).is_ok() {
                out.push((name.as_str().to_string(), join.clone(), Some(label.clone())));
            }
        }
    }
    out.sort_by(|a, b| {
        (&a.0, a.1.as_str(), a.2.as_ref().map(|l| l.as_str()))
            .cmp(&(&b.0, b.1.as_str(), b.2.as_ref().map(|l| l.as_str())))
    });
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, GlobalId, Terminator};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(s: &str) -> Label {
        Label::from(s.to_string())
    }

    fn num(n: i16) -> Instruction {
        Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(n) }
    }

    fn field(receiver: usize, name: &str) -> Instruction {
        Instruction::GetField {
            receiver: id(receiver),
            field: name.to_string(),
            create_if_missing: false,
        }
    }

    fn store(target: usize, source: usize) -> Instruction {
        Instruction::Store { target: id(target), source: id(source) }
    }

    /// `__entry` conditionally runs `arm`, which joins back at `join`.
    fn triangle_program(arm_body: Vec<(LocalId, Instruction)>) -> Program {
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
                instructions: vec![],
                terminator: (id(31), Terminator::Return { value: None }),
                hint_normalize: false,
            },
        );
        let cfg = Cfg::new(
            Block {
                instructions: vec![(id(0), Instruction::BoolConstant { value: true }), (id(1), num(5))],
                terminator: (
                    id(3),
                    Terminator::ConditionalBranch {
                        condition: id(0),
                        true_target: label("arm"),
                        false_target: label("join"),
                    },
                ),
                hint_normalize: false,
            },
            named,
        );
        let mut functions = IndexMap::new();
        functions.insert(
            GlobalId::from("f".to_string()),
            FunDef {
                name: GlobalId::from("f".to_string()),
                capture_ids: vec![],
                arg_ids: vec![Some(id(2))],
                cfg,
                source_span: None,
            },
        );
        Program { functions }
    }

    /// The main shape: the pointer, its guard and the stored value hoist; the
    /// store stays.
    #[test]
    fn hoists_everything_but_the_store() {
        let before = triangle_program(vec![
            (id(10), field(2, "x")),
            (id(11), Instruction::AssertPointer { value: id(10) }),
            (id(12), num(7)),
            (id(13), store(10, 12)),
        ]);
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "join", None).unwrap(), 3);
        verify(&before, &after, "f", "join", None).unwrap();

        let fun = after.get("f").unwrap();
        let arm = fun.cfg.named.get(&label("arm")).unwrap();
        assert_eq!(arm.instructions, vec![(id(13), store(10, 12))]);
        let head_ids: Vec<LocalId> =
            fun.cfg.entry.instructions.iter().map(|(i, _)| *i).collect();
        assert_eq!(head_ids, vec![id(0), id(1), id(10), id(11), id(12)]);
    }

    /// A load may cross a store to a *sibling field of the same record* - the
    /// one distinctness fact the rule knows.
    #[test]
    fn a_load_may_cross_a_store_to_a_sibling_field() {
        let before = triangle_program(vec![
            (id(10), field(2, "x")),
            (id(13), store(10, 1)),
            (id(14), field(2, "y")),
            (id(15), Instruction::Load { source: id(14) }),
        ]);
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "join", None).unwrap(), 3);
        verify(&before, &after, "f", "join", None).unwrap();
        let arm = after.get("f").unwrap().cfg.named.get(&label("arm")).unwrap();
        assert_eq!(arm.instructions, vec![(id(13), store(10, 1))]);
    }

    /// A global's cell can never be a field's cell (disjoint mints), so a
    /// load of a global crosses any field store; and two different globals
    /// are likewise distinct.
    #[test]
    fn a_load_of_a_global_may_cross_a_field_store() {
        for global_read in [
            Instruction::GetGlobal { name: "max_djump".to_string(), create_if_missing: false },
        ] {
            let before = triangle_program(vec![
                (id(10), field(2, "grace")),
                (id(13), store(10, 1)),
                (id(14), global_read),
                (id(15), Instruction::Load { source: id(14) }),
            ]);
            let mut after = before.clone();
            assert_eq!(apply(&mut after, "f", "join", None).unwrap(), 3);
            verify(&before, &after, "f", "join", None).unwrap();
            let arm = after.get("f").unwrap().cfg.named.get(&label("arm")).unwrap();
            assert_eq!(arm.instructions, vec![(id(13), store(10, 1))]);
        }
    }

    /// ...but not a store to the same field, nor to a field of a different
    /// record - those may be the same cell, and the load would silently read a
    /// value the original program had not written yet.
    #[test]
    fn refuses_a_load_crossing_a_possibly_aliasing_store() {
        for aliasing in [field(2, "x"), field(1, "y")] {
            let before = triangle_program(vec![
                (id(10), field(2, "x")),
                (id(13), store(10, 1)),
                (id(14), aliasing),
                (id(15), Instruction::Load { source: id(14) }),
            ]);
            let mut after = before.clone();
            let error = apply(&mut after, "f", "join", None).unwrap_err().to_string();
            assert!(error.contains("same cell"), "{}", error);
        }
    }

    /// The rule exists to feed `sink_store`, so an arm blocked by anything that
    /// is not a store is refused rather than half-cleared.
    #[test]
    fn refuses_an_arm_blocked_by_a_call() {
        let before = triangle_program(vec![
            (id(10), num(7)),
            (id(13), Instruction::Call { closure: id(1), args: vec![] }),
        ]);
        let mut after = before.clone();
        let error = apply(&mut after, "f", "join", None).unwrap_err().to_string();
        assert!(error.contains("not a store"), "{}", error);
    }

    /// A fully speculatable arm is `if_convert`'s job, not this rule's.
    #[test]
    fn refuses_a_fully_speculatable_arm() {
        let before = triangle_program(vec![(id(10), num(7))]);
        let mut after = before.clone();
        let error = apply(&mut after, "f", "join", None).unwrap_err().to_string();
        assert!(error.contains("use if_convert"), "{}", error);
    }

    /// The verifier is the trusted half: it must reject an applier that
    /// quietly hoisted the store too.
    #[test]
    fn verify_rejects_a_hoisted_store() {
        let before = triangle_program(vec![
            (id(10), field(2, "x")),
            (id(13), store(10, 1)),
        ]);
        let mut after = before.clone();
        apply(&mut after, "f", "join", None).unwrap();
        // Sabotage: move the store into the head as well.
        let fun = after.get_mut("f").unwrap();
        let s = fun.cfg.named.get_mut(&label("arm")).unwrap().instructions.remove(0);
        fun.cfg.entry.instructions.push(s);
        assert!(verify(&before, &after, "f", "join", None).is_err());
    }

    /// `__entry` branches to `arm_a` or `arm_b`, which both join at `join`.
    fn diamond_program(
        a_body: Vec<(LocalId, Instruction)>,
        b_body: Vec<(LocalId, Instruction)>,
    ) -> Program {
        let mut program = triangle_program(a_body);
        let fun = program.functions.values_mut().next().unwrap();
        let old = fun.cfg.named.remove(&label("arm")).unwrap();
        fun.cfg.named.insert(label("arm_a"), old);
        fun.cfg.named.insert(
            label("arm_b"),
            Block {
                instructions: b_body,
                terminator: (id(25), Terminator::UnconditionalBranch { target: label("join") }),
                hint_normalize: false,
            },
        );
        fun.cfg.entry.terminator = (
            id(3),
            Terminator::ConditionalBranch {
                condition: id(0),
                true_target: label("arm_a"),
                false_target: label("arm_b"),
            },
        );
        program
    }

    /// A diamond arm hoists exactly like a triangle arm, once the entry says
    /// which arm it means. The sibling arm is untouched.
    #[test]
    fn hoists_a_named_diamond_arm() {
        let before = diamond_program(
            vec![(id(10), field(2, "x")), (id(12), num(7)), (id(13), store(10, 12))],
            vec![(id(15), field(2, "x")), (id(16), num(9)), (id(17), store(15, 16))],
        );
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "join", Some("arm_a")).unwrap(), 2);
        verify(&before, &after, "f", "join", Some("arm_a")).unwrap();

        let fun = after.get("f").unwrap();
        let arm_a = fun.cfg.named.get(&label("arm_a")).unwrap();
        assert_eq!(arm_a.instructions, vec![(id(13), store(10, 12))]);
        let arm_b = fun.cfg.named.get(&label("arm_b")).unwrap();
        assert_eq!(arm_b.instructions.len(), 3);
        let head_ids: Vec<LocalId> =
            fun.cfg.entry.instructions.iter().map(|(i, _)| *i).collect();
        assert_eq!(head_ids, vec![id(0), id(1), id(10), id(12)]);

        // And then the other arm, on top.
        let middle = after.clone();
        assert_eq!(apply(&mut after, "f", "join", Some("arm_b")).unwrap(), 2);
        verify(&middle, &after, "f", "join", Some("arm_b")).unwrap();
        let fun = after.get("f").unwrap();
        let arm_b = fun.cfg.named.get(&label("arm_b")).unwrap();
        assert_eq!(arm_b.instructions, vec![(id(17), store(15, 16))]);
    }

    /// An arm whose join has predecessors besides the triangle's - invisible
    /// to `triangle_shaped`, reachable by naming the arm.
    #[test]
    fn hoists_an_arm_at_a_shared_join() {
        let mut before = triangle_program(vec![
            (id(10), field(2, "x")),
            (id(12), num(7)),
            (id(13), store(10, 12)),
        ]);
        let fun = before.functions.values_mut().next().unwrap();
        fun.cfg.named.insert(
            label("elsewhere"),
            Block {
                instructions: vec![],
                terminator: (
                    id(35),
                    Terminator::UnconditionalBranch { target: label("join") },
                ),
                hint_normalize: false,
            },
        );
        let mut after = before.clone();
        // The triangle recognizer cannot see it...
        let error = apply(&mut after, "f", "join", None).unwrap_err().to_string();
        assert!(error.contains("no triangle joins"), "{}", error);
        // ...naming the arm can.
        assert_eq!(apply(&mut after, "f", "join", Some("arm")).unwrap(), 2);
        verify(&before, &after, "f", "join", Some("arm")).unwrap();
        let arm = after.get("f").unwrap().cfg.named.get(&label("arm")).unwrap();
        assert_eq!(arm.instructions, vec![(id(13), store(10, 12))]);
    }

    /// A diamond entry must name one of the diamond's own arms.
    #[test]
    fn refuses_a_block_that_is_not_an_arm() {
        let before = diamond_program(
            vec![(id(10), field(2, "x")), (id(13), store(10, 1))],
            vec![(id(15), num(9))],
        );
        let mut after = before.clone();
        let error = apply(&mut after, "f", "join", Some("join")).unwrap_err().to_string();
        assert!(error.contains("not an arm"), "{}", error);
    }

    /// Without an `arm` field a diamond is invisible: the triangle recognizer
    /// must not quietly pick a side.
    #[test]
    fn refuses_a_diamond_without_an_arm_field() {
        let before = diamond_program(
            vec![(id(10), field(2, "x")), (id(13), store(10, 1))],
            vec![(id(15), num(9))],
        );
        let mut after = before.clone();
        let error = apply(&mut after, "f", "join", None).unwrap_err().to_string();
        assert!(error.contains("no triangle joins"), "{}", error);
    }

    /// ...or one that dropped a hoisted instruction instead of moving it.
    #[test]
    fn verify_rejects_a_dropped_instruction() {
        let before = triangle_program(vec![
            (id(10), field(2, "x")),
            (id(13), store(10, 1)),
        ]);
        let mut after = before.clone();
        apply(&mut after, "f", "join", None).unwrap();
        after.get_mut("f").unwrap().cfg.entry.instructions.pop();
        let error = verify(&before, &after, "f", "join", None).unwrap_err().to_string();
        assert!(error.contains("hoisted instructions"), "{}", error);
    }
}
