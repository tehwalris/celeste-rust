//! `assume_eq` - state that two locals hold the same value, loudly, and use
//! one for the other.
//!
//! # Why
//!
//! Some equalities are facts about the program's data that no local analysis
//! can see. The motivating one: inside a collapsed object-table loop the
//! body reads `o = load (get_index objects[1])`, and the object being
//! updated arrived as the function's argument `%2` - the same object,
//! reached by two different paths, because room (1, 0)'s table is a
//! singleton. `cse` cannot fold the two (different expressions), so the
//! body's `o ~= obj` term - the reason every `check` in this room returns
//! nil - stays opaque.
//!
//! Planned in `plans/status.md` as: "handles pointers that reach one cell by
//! different paths, which CSE cannot see."
//!
//! # What it does
//!
//! For a site `(a, b)`, inserts directly after `b`'s definition
//!
//! ```text
//!   %g = %b == %a
//!   assert_true %g
//! ```
//!
//! and replaces every other use of `%b` with `%a`.
//!
//! # Soundness
//!
//! `assert_true` passes only when the equality is a concrete `true` on every
//! lane - `UnknownBool` fails it by design - so past the guard the two
//! locals hold per-lane-equal values and substituting one for the other
//! changes nothing. The guard sits immediately after `b`'s definition, and
//! every use of `b` is dominated by its definition, so no substituted use
//! can execute without the guard having run.
//!
//! Dominance of the replacement: the rule requires `a`'s definition to
//! dominate `b`'s. Every use of `b` is dominated by `b`'s definition and
//! therefore by `a`'s, so the rewritten uses are well-formed - `validate`
//! re-checks this anyway.
//!
//! The claim itself is unverifiable statically - that is the point. It is a
//! premise about the run, stated where it is used, checked on every
//! execution, and screened at full depth like every other guard.

use anyhow::{anyhow, Result};

use crate::ir::{BinaryOp, FunDef, Instruction, Label, LocalId};

use super::super::program::Program;
use super::super::validate::Dominance;
use super::{blocks_sorted, get_block, require, LocalIdAllocator};

/// Where a local is defined: which block, and at which instruction index.
struct DefSite {
    block: Option<Label>,
    index: usize,
}

fn def_site(fun: &FunDef, id: LocalId) -> Option<DefSite> {
    for key in blocks_sorted(&fun.cfg) {
        let block = get_block(&fun.cfg, &key)?;
        for (index, (def, _)) in block.instructions.iter().enumerate() {
            if *def == id {
                return Some(DefSite { block: key, index });
            }
        }
    }
    None
}

fn is_arg_or_capture(fun: &FunDef, id: LocalId) -> bool {
    fun.arg_ids.iter().flatten().any(|arg| *arg == id)
        || fun.capture_ids.iter().any(|cap| *cap == id)
}

/// Checks the site and returns `b`'s definition site.
fn site(fun: &FunDef, function: &str, a: LocalId, b: LocalId) -> Result<DefSite> {
    require(a != b, "assume_eq of a local with itself is vacuous")?;
    require(
        !is_arg_or_capture(fun, b),
        format!(
            "%{} is an argument or capture of {}; state the guard on the \
             derived value instead",
            usize::from(b),
            function
        ),
    )?;
    let b_def = def_site(fun, b).ok_or_else(|| {
        anyhow!("%{} is not defined in {}", usize::from(b), function)
    })?;

    // `a` must dominate `b`'s definition: as an argument or capture it
    // dominates everything; as an instruction its block must dominate `b`'s
    // (strictly, or the same block at an earlier index).
    if !is_arg_or_capture(fun, a) {
        let a_def = def_site(fun, a).ok_or_else(|| {
            anyhow!("%{} is not defined in {}", usize::from(a), function)
        })?;
        if a_def.block == b_def.block {
            require(
                a_def.index < b_def.index,
                format!(
                    "%{} must be defined before %{}",
                    usize::from(a),
                    usize::from(b)
                ),
            )?;
        } else {
            let dominance = Dominance::of(&fun.cfg);
            let a_index = dominance
                .index_of(&a_def.block)
                .ok_or_else(|| anyhow!("no dominance index for %{}'s block", usize::from(a)))?;
            let b_index = dominance
                .index_of(&b_def.block)
                .ok_or_else(|| anyhow!("no dominance index for %{}'s block", usize::from(b)))?;
            require(
                dominance.dominates(a_index, b_index),
                format!(
                    "%{}'s definition does not dominate %{}'s",
                    usize::from(a),
                    usize::from(b)
                ),
            )?;
        }
    }
    Ok(b_def)
}

pub fn apply(program: &mut Program, function: &str, a: LocalId, b: LocalId) -> Result<usize> {
    let fun = program.get(function)?;
    let b_def = site(fun, function, a, b)?;
    let mut allocator = LocalIdAllocator::for_function(fun);
    let guard = allocator.fresh();
    let guard_assert = allocator.fresh();

    let fun = program.get_mut(function)?;

    // Substitute first, so the guard's own read of `b` survives.
    let substitute = |id: LocalId| if id == b { a } else { id };
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

    let block = match &b_def.block {
        None => &mut fun.cfg.entry,
        Some(l) => fun.cfg.named.get_mut(l).expect("def block exists"),
    };
    block.instructions.splice(
        b_def.index + 1..b_def.index + 1,
        [
            (guard, Instruction::BinaryOp { left: b, op: BinaryOp::TwoEqual, right: a }),
            (guard_assert, Instruction::AssertTrue { value: guard }),
        ],
    );

    // Live ranges changed; any slot allocation is stale.
    fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(1)
}

/// Independent check: re-derives the site from the *before* program -
/// including the dominance requirement - and insists the after program is
/// exactly the prescription: the guard pair right after `b`'s definition,
/// every other use of `b` now `a`, and not one other thing different.
pub fn verify(before: &Program, after: &Program, function: &str, a: LocalId, b: LocalId) -> Result<()> {
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let b_def = site(before_fun, function, a, b)?;

    let before_max = {
        let mut allocator = LocalIdAllocator::for_function(before_fun);
        allocator.fresh()
    };

    require(
        before_fun.cfg.named.len() == after_fun.cfg.named.len()
            && before_fun
                .cfg
                .named
                .keys()
                .all(|l| after_fun.cfg.named.contains_key(l)),
        "assume_eq changed the set of blocks",
    )?;

    let substitute = |id: LocalId| if id == b { a } else { id };
    for key in blocks_sorted(&before_fun.cfg) {
        let before_block = get_block(&before_fun.cfg, &key).expect("listed block exists");
        let after_block = get_block(&after_fun.cfg, &key).expect("checked same block set");

        let mut expected = before_block.clone();
        for (_, instr) in expected.instructions.iter_mut() {
            *instr = instr.map_local_ids(substitute);
        }
        expected.terminator.1 = expected.terminator.1.map_local_ids(substitute);

        if key == b_def.block {
            require(
                after_block.instructions.len() == before_block.instructions.len() + 2,
                "the definition block must gain exactly the guard pair",
            )?;
            let (guard, guard_instr) = &after_block.instructions[b_def.index + 1];
            let (guard_assert, assert_instr) = &after_block.instructions[b_def.index + 2];
            require(
                *guard >= before_max && *guard_assert >= before_max && guard != guard_assert,
                "the guard pair must use fresh, distinct ids",
            )?;
            require(
                *guard_instr
                    == Instruction::BinaryOp { left: b, op: BinaryOp::TwoEqual, right: a },
                "the guard must compare `b == a`",
            )?;
            require(
                *assert_instr == Instruction::AssertTrue { value: *guard },
                "the guard must be stated with `assert_true`",
            )?;
            expected.instructions.splice(
                b_def.index + 1..b_def.index + 1,
                [(*guard, guard_instr.clone()), (*guard_assert, assert_instr.clone())],
            );
        }
        require(
            *after_block == expected,
            format!(
                "assume_eq changed block '{}' beyond the prescription",
                key.as_ref().map(|l| l.as_str()).unwrap_or("__entry")
            ),
        )?;
    }

    require(
        before.functions.len() == after.functions.len(),
        "assume_eq changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!("assume_eq on {} also changed {}", function, name.as_str()),
        )?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, GlobalId, Terminator};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(name: &str) -> Label {
        Label::from(name.to_string())
    }

    /// entry: %3 = objects cell; %4 = load %3 (the table);
    ///        %5 = 1; %6 = get_index %4[%5]; %7 = load %6; br body
    /// body:  %8 = %7 ~= %2 (arg); %9 = get_field %7.x; return %8
    fn two_path_program() -> Program {
        let entry = Block {
            instructions: vec![
                (
                    id(3),
                    Instruction::GetGlobal {
                        name: "objects".to_string(),
                        create_if_missing: false,
                    },
                ),
                (id(4), Instruction::Load { source: id(3) }),
                (
                    id(5),
                    Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(1) },
                ),
                (
                    id(6),
                    Instruction::GetIndex {
                        receiver: id(4),
                        index: id(5),
                        create_if_missing: false,
                    },
                ),
                (id(7), Instruction::Load { source: id(6) }),
            ],
            terminator: (id(10), Terminator::UnconditionalBranch { target: label("body") }),
            hint_normalize: false,
        };
        let body = Block {
            instructions: vec![
                (
                    id(8),
                    Instruction::BinaryOp { left: id(7), op: BinaryOp::TildeEqual, right: id(2) },
                ),
                (
                    id(9),
                    Instruction::GetField {
                        receiver: id(7),
                        field: "x".to_string(),
                        create_if_missing: false,
                    },
                ),
            ],
            terminator: (id(11), Terminator::Return { value: Some(id(8)) }),
            hint_normalize: false,
        };
        let mut named = crate::ir::new_label_map();
        named.insert(label("body"), body);
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![Some(id(2))],
            cfg: Cfg::new(entry, named),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions }
    }

    #[test]
    fn assumes_the_two_paths_equal() {
        let before = two_path_program();
        let mut after = before.clone();
        apply(&mut after, "f", id(2), id(7)).unwrap();
        verify(&before, &after, "f", id(2), id(7)).unwrap();

        let fun = after.get("f").unwrap();
        // Guard right after %7's definition.
        assert_eq!(
            fun.cfg.entry.instructions[5].1,
            Instruction::BinaryOp { left: id(7), op: BinaryOp::TwoEqual, right: id(2) }
        );
        assert!(matches!(fun.cfg.entry.instructions[6].1, Instruction::AssertTrue { .. }));
        // Uses rewritten: the compare is now reflexive, the field read is on
        // the argument.
        let body = &fun.cfg.named[&label("body")];
        assert_eq!(
            body.instructions[0].1,
            Instruction::BinaryOp { left: id(2), op: BinaryOp::TildeEqual, right: id(2) }
        );
        assert_eq!(
            body.instructions[1].1,
            Instruction::GetField {
                receiver: id(2),
                field: "x".to_string(),
                create_if_missing: false
            }
        );
    }

    #[test]
    fn refuses_a_backwards_pair() {
        // `a` defined after `b` in the same block.
        let mut program = two_path_program();
        let err = apply(&mut program, "f", id(7), id(4)).unwrap_err();
        assert!(err.to_string().contains("defined before"), "{}", err);
    }

    #[test]
    fn refuses_an_argument_as_b() {
        let mut program = two_path_program();
        let err = apply(&mut program, "f", id(7), id(2)).unwrap_err();
        assert!(err.to_string().contains("argument or capture"), "{}", err);
    }

    #[test]
    fn verify_rejects_a_missing_substitution() {
        let before = two_path_program();
        let mut after = before.clone();
        apply(&mut after, "f", id(2), id(7)).unwrap();
        {
            let fun = after.functions.values_mut().next().unwrap();
            let body = fun.cfg.named.get_mut(&label("body")).unwrap();
            // Put one use of %7 back.
            body.instructions[1].1 = Instruction::GetField {
                receiver: id(7),
                field: "x".to_string(),
                create_if_missing: false,
            };
        }
        let err = verify(&before, &after, "f", id(2), id(7)).unwrap_err();
        assert!(err.to_string().contains("beyond the prescription"), "{}", err);
    }

    #[test]
    fn verify_rejects_a_dropped_guard() {
        let before = two_path_program();
        let mut after = before.clone();
        apply(&mut after, "f", id(2), id(7)).unwrap();
        {
            let fun = after.functions.values_mut().next().unwrap();
            fun.cfg.entry.instructions.remove(6);
            fun.cfg.entry.instructions.remove(5);
        }
        let err = verify(&before, &after, "f", id(2), id(7)).unwrap_err();
        assert!(err.to_string().contains("guard pair"), "{}", err);
    }
}
