//! `demote_create` - turn a creating accessor into a plain read, guarded.
//!
//! # The instruction, and why it exists
//!
//! `r.f = v` compiles to two instructions, because a store needs a cell and Lua
//! says assigning to a missing field brings one into existence:
//!
//! ```text
//!   %c = get_field %r.f create
//!        store %c <- %v
//! ```
//!
//! So one instruction carries two behaviours - a read, and a heap mutation -
//! and which one happens is a property of the run, not of the program text.
//!
//! # Why that is in the way
//!
//! An `if_convert` arm cannot be speculated if it might mutate the heap, and a
//! possible field creation is exactly that. Of the accessors blocking a
//! triangle, **all of them are `create` accessors**; not one is a plain read.
//! Together with the `store` that follows, this pattern is most of what stands
//! between the program and a branch-free frame.
//!
//! # Why demoting is usually right
//!
//! Objects are built once with all their fields, and `r.f = v` thereafter finds
//! the cell already there. `create_sites` measures it, over 34 frames and
//! excluding `__init`:
//!
//! ```text
//!   get_global create    30599 found,      0 created    0.00%
//!   get_field create    107509 found,     45 created    0.04%
//!   get_index create     15060 found, 113875 created   88.32%
//! ```
//!
//! `get_index ... create` is a different instruction wearing the same name -
//! `add(objects, obj)`, Lua's array append, which creates by design. `suggest`
//! therefore never proposes one, though the rule will apply to it if a recipe
//! asks, because the transformation is the same and the guard is the same.
//!
//! # What makes it safe
//!
//! Nothing static. The 45 creations above are real: they are object
//! construction, where a fresh table genuinely gains its fields. So this rule
//! is **not** semantics-preserving in general, and it is the third place the
//! project trades a static proof for a loud runtime check - after
//! `AssertClosure` for `inline` and the partial `Select` for `if_convert`.
//!
//! The demoted accessor returns a `NilPointer` where the field is absent, and
//! the rule plants an `AssertPointer` on it immediately. A site where the field
//! is ever missing fails on the first frame that reaches it, saying which local
//! and which field. It cannot silently read or write the wrong cell.
//!
//! That is why sites are *screened* rather than proved, and screened at the
//! depth the result will be used at: the 45 creations fall in two frames out of
//! 34, so a short screening run would report a clean bill of health on exactly
//! the sites that are wrong.

use anyhow::{anyhow, Result};

use crate::ir::{FunDef, Instruction, LocalId};

use super::super::program::Program;
use super::super::validate::{all_blocks, block_label, BlockKey};
use super::{get_block, get_block_mut, require, LocalIdAllocator};

/// Is this the left-hand side of an assignment - an accessor that may create?
fn is_creating_accessor(instr: &Instruction) -> bool {
    matches!(
        instr,
        Instruction::GetGlobal { create_if_missing: true, .. }
            | Instruction::GetField { create_if_missing: true, .. }
            | Instruction::GetIndex { create_if_missing: true, .. }
    )
}

/// The same accessor with the creation taken away.
fn demoted(instr: &Instruction) -> Option<Instruction> {
    Some(match instr {
        Instruction::GetGlobal { name, create_if_missing: true } => Instruction::GetGlobal {
            name: name.clone(),
            create_if_missing: false,
        },
        Instruction::GetField { receiver, field, create_if_missing: true } => {
            Instruction::GetField {
                receiver: *receiver,
                field: field.clone(),
                create_if_missing: false,
            }
        }
        Instruction::GetIndex { receiver, index, create_if_missing: true } => {
            Instruction::GetIndex {
                receiver: *receiver,
                index: *index,
                create_if_missing: false,
            }
        }
        _ => return None,
    })
}

/// Where an id is defined: which block, and at what position in it.
fn definition_site(fun: &FunDef, at: LocalId) -> Option<(BlockKey, usize)> {
    for (key, block) in all_blocks(&fun.cfg) {
        if let Some(index) = block.instructions.iter().position(|(id, _)| *id == at) {
            return Some((key, index));
        }
    }
    None
}

pub fn apply(program: &mut Program, function: &str, at: LocalId) -> Result<usize> {
    let fun = program.get(function)?;
    let (key, index) = definition_site(fun, at).ok_or_else(|| {
        anyhow!("no instruction defines %{} in {}", usize::from(at), function)
    })?;
    let instr = &get_block(&fun.cfg, &key).unwrap().instructions[index].1;
    let replacement = demoted(instr).ok_or_else(|| {
        anyhow!(
            "%{} in {} is not a creating accessor, it is `{}`",
            usize::from(at),
            function,
            super::super::print::format_instruction(instr)
        )
    })?;
    let guard_id = LocalIdAllocator::for_function(fun).fresh();

    let fun = program.get_mut(function)?;
    let block = get_block_mut(&mut fun.cfg, &key).unwrap();
    block.instructions[index].1 = replacement;
    block
        .instructions
        .insert(index + 1, (guard_id, Instruction::AssertPointer { value: at }));
    Ok(1)
}

/// Independent check.
///
/// Written as a set of properties of the two programs rather than as a second
/// implementation of the transformation, so that a bug shared with `apply`
/// would have to be a bug in what the rule *means*, not in how it is carried
/// out. In particular it does not ask where `apply` thinks the instruction was:
/// it finds the definition itself, in the *before* program, and insists that
/// everything else in the program is untouched.
pub fn verify(before: &Program, after: &Program, function: &str, at: LocalId) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "demote_create changed the set of functions",
    )?;

    // Nothing outside the named function may move at all.
    for (name, before_fun) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_fun),
            format!(
                "demote_create on {} also changed {}",
                function,
                name.as_str()
            ),
        )?;
    }

    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let (key, index) = definition_site(before_fun, at).ok_or_else(|| {
        anyhow!("no instruction defines %{} in {}", usize::from(at), function)
    })?;

    let before_instr = &get_block(&before_fun.cfg, &key).unwrap().instructions[index].1;
    require(
        is_creating_accessor(before_instr),
        format!(
            "demote_create names %{} in {}, which is `{}` and creates nothing",
            usize::from(at),
            function,
            super::super::print::format_instruction(before_instr)
        ),
    )?;

    let before_blocks = all_blocks(&before_fun.cfg);
    require(
        before_blocks.len() == all_blocks(&after_fun.cfg).len(),
        "demote_create changed the set of blocks",
    )?;

    for (block_key, before_block) in &before_blocks {
        let after_block = get_block(&after_fun.cfg, block_key).ok_or_else(|| {
            anyhow!("demote_create removed block '{}'", block_label(block_key))
        })?;
        require(
            format!("{:?}", before_block.terminator) == format!("{:?}", after_block.terminator),
            format!(
                "demote_create changed the terminator of '{}'",
                block_label(block_key)
            ),
        )?;
        require(
            before_block.hint_normalize == after_block.hint_normalize,
            format!(
                "demote_create changed the normalize hint of '{}'",
                block_label(block_key)
            ),
        )?;

        // Every block but the one holding the accessor must be identical; that
        // one gains exactly one instruction, the guard.
        let is_target = block_key == &key;
        require(
            after_block.instructions.len()
                == before_block.instructions.len() + usize::from(is_target),
            format!(
                "demote_create changed the length of '{}' by {}, expected {}",
                block_label(block_key),
                after_block.instructions.len() as i64 - before_block.instructions.len() as i64,
                usize::from(is_target)
            ),
        )?;

        for (position, (before_id, before_one)) in before_block.instructions.iter().enumerate() {
            // Everything after the insertion point shifts by one.
            let shift = usize::from(is_target && position > index);
            let (after_id, after_one) = &after_block.instructions[position + shift];
            require(
                after_id == before_id,
                format!(
                    "demote_create reordered '{}': expected %{} at {}, found %{}",
                    block_label(block_key),
                    usize::from(*before_id),
                    position + shift,
                    usize::from(*after_id)
                ),
            )?;
            let want = if is_target && position == index {
                demoted(before_one).unwrap()
            } else {
                before_one.clone()
            };
            require(
                &want == after_one,
                format!(
                    "demote_create changed %{} in '{}':\n  want {}\n  got  {}",
                    usize::from(*before_id),
                    block_label(block_key),
                    super::super::print::format_instruction(&want),
                    super::super::print::format_instruction(after_one)
                ),
            )?;
        }
    }

    // The guard itself: immediately after the accessor, on the accessor.
    let after_block = get_block(&after_fun.cfg, &key).unwrap();
    let guard = after_block.instructions.get(index + 1).ok_or_else(|| {
        anyhow!(
            "demote_create put no guard after %{} in {}",
            usize::from(at),
            function
        )
    })?;
    require(
        guard.1 == Instruction::AssertPointer { value: at },
        format!(
            "demote_create's guard on %{} in {} is `{}`, expected `assert_pointer %{}`",
            usize::from(at),
            function,
            super::super::print::format_instruction(&guard.1),
            usize::from(at)
        ),
    )?;
    // A duplicate definition would be caught by `validate`, but the guard is
    // the one id this rule mints, so it is worth being explicit about here.
    require(
        definition_site(before_fun, guard.0).is_none(),
        format!(
            "demote_create's guard reuses %{}, which the function already defines",
            usize::from(guard.0)
        ),
    )?;

    Ok(())
}

/// Sites worth proposing: creating accessors that are currently blocking an
/// `if_convert` triangle.
///
/// Deliberately narrow. The program has ~1400 creating accessors and demoting
/// one is a claim about the run rather than a theorem, so the only ones worth
/// the claim are the ones that buy something. `get_index` is excluded because
/// it is array append and really does create - 88% of the time.
pub fn candidates(program: &Program) -> Vec<(String, LocalId)> {
    let mut out = Vec::new();
    for (name, fun) in &program.functions {
        for (_, blocking) in super::if_convert::blocking_instructions(fun) {
            for (id, instr) in blocking {
                if matches!(
                    instr,
                    Instruction::GetField { create_if_missing: true, .. }
                        | Instruction::GetGlobal { create_if_missing: true, .. }
                ) {
                    out.push((name.as_str().to_string(), id));
                }
            }
        }
    }
    out.sort();
    out.dedup();
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, GlobalId, Terminator};
    use crate::rewrite::print::format_function;
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn program_of(instructions: Vec<(LocalId, Instruction)>) -> Program {
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![Some(id(2))],
            cfg: Cfg::new(
                Block {
                    instructions,
                    terminator: (id(900), Terminator::Return { value: None }),
                    hint_normalize: false,
                },
                Default::default(),
            ),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions }
    }

    fn field(receiver: usize, name: &str, create: bool) -> Instruction {
        Instruction::GetField {
            receiver: id(receiver),
            field: name.to_string(),
            create_if_missing: create,
        }
    }

    #[test]
    fn demotes_and_guards() {
        let mut p = program_of(vec![
            (id(10), field(2, "x", true)),
            (id(11), Instruction::Store { target: id(10), source: id(2) }),
        ]);
        let before = p.clone();
        assert_eq!(apply(&mut p, "f", id(10)).unwrap(), 1);
        verify(&before, &p, "f", id(10)).unwrap();
        let text = format_function(p.get("f").unwrap());
        assert!(text.contains("%10 = get_field %2.x\n"), "{}", text);
        assert!(!text.contains("%10 = get_field %2.x create"), "{}", text);
        assert!(text.contains("assert_pointer %10"), "{}", text);
        // The guard sits between the accessor and the store, not at the end.
        let accessor = text.find("%10 = get_field").unwrap();
        let guard = text.find("assert_pointer %10").unwrap();
        let store = text.find("store %10").unwrap();
        assert!(accessor < guard && guard < store, "{}", text);
    }

    #[test]
    fn works_on_globals_and_indexes_too() {
        for instr in [
            Instruction::GetGlobal { name: "g".to_string(), create_if_missing: true },
            Instruction::GetIndex { receiver: id(2), index: id(2), create_if_missing: true },
        ] {
            let mut p = program_of(vec![(id(10), instr)]);
            let before = p.clone();
            apply(&mut p, "f", id(10)).unwrap();
            verify(&before, &p, "f", id(10)).unwrap();
            let text = format_function(p.get("f").unwrap());
            assert!(!text.contains("create"), "{}", text);
            assert!(text.contains("assert_pointer %10"), "{}", text);
        }
    }

    /// Aiming the rule at something that does not create is a mistake in the
    /// recipe, and mistakes in the recipe must be loud.
    #[test]
    fn refuses_an_accessor_that_does_not_create() {
        let mut p = program_of(vec![(id(10), field(2, "x", false))]);
        let error = apply(&mut p, "f", id(10)).unwrap_err().to_string();
        assert!(error.contains("not a creating accessor"), "{}", error);
    }

    #[test]
    fn refuses_an_id_that_is_not_defined() {
        let mut p = program_of(vec![(id(10), field(2, "x", true))]);
        assert!(apply(&mut p, "f", id(77)).is_err());
    }

    /// `verify` must not accept a demotion that left no guard behind - that is
    /// precisely the difference between a checked claim and an unchecked one.
    #[test]
    fn verify_rejects_a_demotion_without_a_guard() {
        let mut p = program_of(vec![
            (id(10), field(2, "x", true)),
            (id(11), Instruction::Store { target: id(10), source: id(2) }),
        ]);
        let before = p.clone();
        p.get_mut("f").unwrap().cfg.entry.instructions[0].1 = field(2, "x", false);
        // Caught by the block-length check before the guard check is reached,
        // which is the same thing said earlier: an unguarded demotion has one
        // instruction too few.
        let error = verify(&before, &p, "f", id(10)).unwrap_err().to_string();
        assert!(error.contains("changed the length"), "{}", error);
    }

    /// ...nor a guard on the wrong value.
    #[test]
    fn verify_rejects_a_guard_on_the_wrong_value() {
        let mut p = program_of(vec![
            (id(10), field(2, "x", true)),
            (id(11), field(2, "y", true)),
        ]);
        let before = p.clone();
        apply(&mut p, "f", id(10)).unwrap();
        let block = &mut p.get_mut("f").unwrap().cfg.entry;
        block.instructions[1].1 = Instruction::AssertPointer { value: id(11) };
        let error = verify(&before, &p, "f", id(10)).unwrap_err().to_string();
        assert!(error.contains("expected `assert_pointer %10`"), "{}", error);
    }

    /// ...nor a rule that quietly changed something else while it was there.
    #[test]
    fn verify_rejects_an_unrelated_change() {
        let mut p = program_of(vec![
            (id(10), field(2, "x", true)),
            (id(11), field(2, "y", true)),
        ]);
        let before = p.clone();
        apply(&mut p, "f", id(10)).unwrap();
        let block = &mut p.get_mut("f").unwrap().cfg.entry;
        let last = block.instructions.len() - 1;
        block.instructions[last].1 = field(2, "z", true);
        assert!(verify(&before, &p, "f", id(10)).is_err());
    }
}
