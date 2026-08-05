//! `sink_store` - move a triangle arm's trailing store past the join.
//!
//! # What it does
//!
//! The last of the three rules that turn a store-blocked triangle into
//! straight-line code (`speculate`, this, `if_convert`). Given an arm whose
//! final instruction is a store, it rewrites
//!
//! ```text
//!   H:  ...                          H:  ...
//!       br %c ? A : J                    %g = assert_value_cell %p
//!   A:  store %p <- %v      =>          %old = load %p
//!       br J                            br %c ? A : J
//!   J:  ...                         A:  br J
//!                                   J:  %sel = phi [A: %v, H: %old]
//!                                       store %p <- %sel
//!                                       ...
//! ```
//!
//! The store now runs on both paths, writing the arm's value on one and the
//! value the cell already held on the other. Once `if_convert` absorbs the
//! emptied arm, the phi becomes a select and the branch is gone - which is the
//! point: these four store sites split the state 3523 times over 34 frames,
//! 18.0% of all splits.
//!
//! # Soundness
//!
//! On the path through the arm, the store moves from the arm's end to the top
//! of the join, crossing nothing but the join's phis - it writes the same
//! value to the same cell at what is observably the same moment. That is why
//! the store must be the arm's *last* instruction: anything after it would be
//! crossed, and this rule owns no aliasing argument. (`speculate` is the rule
//! that moves things past stores, and it checks per crossing.)
//!
//! On the path that skips the arm, the program now loads the cell at the end
//! of `H` and stores that value back at `J`, with nothing in between on that
//! edge. For a cell holding a plain value that is the identity. It is **not**
//! the identity for a cell holding a closure or a table: `load` on such a cell
//! yields a pointer to the cell itself, and storing that back would replace
//! the closure with a self-pointer - silently. The `assert_value_cell` guard
//! makes exactly that case fail loudly instead, before the load. A site whose
//! cell ever holds a closure or table cannot use this transformation at all,
//! and screening will say so on the first frame that reaches it.
//!
//! The loaded value can still be `Nil` (through a `NilPointer` target the
//! guard refuses first) or a legitimate pointer *to another cell* (a field
//! holding a table reference); both store back exactly what was read.
//!
//! # What this rule requires but does not check
//!
//! The target pointer must be defined before the branch. It refuses a target
//! defined in the arm with a pointer at `speculate`, and leaves any subtler
//! dominance violation to `validate`, which re-checks the whole function
//! after every entry.

use anyhow::{anyhow, Result};

use crate::ir::{FunDef, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::if_convert::triangle_shaped;
use super::{get_block, get_block_mut, label_of, require, LocalIdAllocator};
use super::super::validate::{all_blocks, block_label, BlockKey};

/// Where an id is defined: which block, and at what position in it.
fn definition_site(fun: &FunDef, at: LocalId) -> Option<(BlockKey, usize)> {
    for (key, block) in all_blocks(&fun.cfg) {
        if let Some(index) = block.instructions.iter().position(|(id, _)| *id == at) {
            return Some((key, index));
        }
    }
    None
}

/// The shape this rule accepts, re-derived identically by `apply`, `verify`
/// and `candidates`.
struct Site {
    head: BlockKey,
    arm: Label,
    join: Label,
    target: LocalId,
    source: LocalId,
}

fn site(fun: &FunDef, function: &str, at: LocalId) -> Result<Site> {
    let (key, index) = definition_site(fun, at)
        .ok_or_else(|| anyhow!("no instruction defines %{} in {}", usize::from(at), function))?;
    let block = get_block(&fun.cfg, &key).unwrap();
    let Instruction::Store { target, source } = block.instructions[index].1 else {
        return Err(anyhow!(
            "sink_store names %{} in {}, which is `{}`, not a store",
            usize::from(at),
            function,
            super::super::print::format_instruction(&block.instructions[index].1)
        ));
    };
    require(
        index + 1 == block.instructions.len(),
        format!(
            "store %{} is not the last instruction of '{}'; sink_store owns no \
             argument for crossing what follows it",
            usize::from(at),
            block_label(&key),
        ),
    )?;
    let Some(arm) = key else {
        return Err(anyhow!("the entry block cannot be a triangle arm"));
    };
    let Terminator::UnconditionalBranch { target: join } = block.terminator_kind() else {
        return Err(anyhow!(
            "'{}' does not branch unconditionally, so it is not a triangle arm",
            arm.as_str()
        ));
    };
    let join = join.clone();
    let t = triangle_shaped(&fun.cfg, &join)
        .ok_or_else(|| anyhow!("no triangle joins at '{}'", join.as_str()))?;
    require(
        t.arm == arm,
        format!(
            "'{}' is not the arm of the triangle at '{}'",
            arm.as_str(),
            join.as_str()
        ),
    )?;
    if let Some((def_key, _)) = definition_site(fun, target) {
        require(
            def_key != Some(arm.clone()),
            format!(
                "the store's target %{} is defined in the arm itself; \
                 run speculate on '{}' first",
                usize::from(target),
                join.as_str()
            ),
        )?;
    }
    Ok(Site { head: t.head, arm, join, target, source })
}

/// How many instructions lead the block as phis.
fn phi_prefix_len(instructions: &[(LocalId, Instruction)]) -> usize {
    instructions
        .iter()
        .take_while(|(_, i)| matches!(i, Instruction::Phi { .. }))
        .count()
}

pub fn apply(program: &mut Program, function: &str, at: LocalId) -> Result<usize> {
    let fun = program.get(function)?;
    let s = site(fun, function, at)?;
    let mut ids = LocalIdAllocator::for_function(fun);
    let (guard_id, load_id, phi_id) = (ids.fresh(), ids.fresh(), ids.fresh());
    let head_label = label_of(&s.head);

    let fun = program.get_mut(function)?;
    let head = get_block_mut(&mut fun.cfg, &s.head).unwrap();
    head.instructions
        .push((guard_id, Instruction::AssertValueCell { target: s.target }));
    head.instructions
        .push((load_id, Instruction::Load { source: s.target }));

    let arm = get_block_mut(&mut fun.cfg, &Some(s.arm.clone())).unwrap();
    arm.instructions.pop();

    let join = get_block_mut(&mut fun.cfg, &Some(s.join.clone())).unwrap();
    let n = phi_prefix_len(&join.instructions);
    join.instructions.insert(
        n,
        (
            phi_id,
            Instruction::Phi {
                branches: vec![(head_label, load_id), (s.arm.clone(), s.source)],
            },
        ),
    );
    join.instructions
        .insert(n + 1, (at, Instruction::Store { target: s.target, source: phi_id }));
    Ok(1)
}

/// Independent check.
///
/// Re-derives the site from the *before* program, reads the three minted ids
/// out of the after program, checks they are fresh, and insists every block is
/// exactly what the transformation prescribes - in particular that the moved
/// store is the join's **first non-phi instruction**, which is what keeps it
/// ordered before everything the join already did.
pub fn verify(before: &Program, after: &Program, function: &str, at: LocalId) -> Result<()> {
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let s = site(before_fun, function, at)?;
    let head_label = label_of(&s.head);

    // The head: unchanged, then the guard, then the load.
    let before_head = get_block(&before_fun.cfg, &s.head).unwrap();
    let after_head = get_block(&after_fun.cfg, &s.head)
        .ok_or_else(|| anyhow!("sink_store removed the head block"))?;
    require(
        after_head.instructions.len() == before_head.instructions.len() + 2,
        "sink_store must add exactly a guard and a load to the head",
    )?;
    require(
        after_head.instructions[..before_head.instructions.len()] == before_head.instructions[..],
        "sink_store changed the head's existing instructions",
    )?;
    let (guard_id, guard) = &after_head.instructions[before_head.instructions.len()];
    let (load_id, load) = &after_head.instructions[before_head.instructions.len() + 1];
    require(
        *guard == Instruction::AssertValueCell { target: s.target },
        format!(
            "the head's first new instruction is `{}`, expected `assert_value_cell %{}`",
            super::super::print::format_instruction(guard),
            usize::from(s.target)
        ),
    )?;
    require(
        *load == Instruction::Load { source: s.target },
        format!(
            "the head's second new instruction is `{}`, expected `load %{}`",
            super::super::print::format_instruction(load),
            usize::from(s.target)
        ),
    )?;
    require(
        after_head.terminator == before_head.terminator,
        "sink_store changed the head's terminator",
    )?;

    // The arm: its store gone, nothing else touched.
    let before_arm = get_block(&before_fun.cfg, &Some(s.arm.clone())).unwrap();
    let after_arm = get_block(&after_fun.cfg, &Some(s.arm.clone()))
        .ok_or_else(|| anyhow!("sink_store removed the arm block"))?;
    require(
        after_arm.instructions[..] == before_arm.instructions[..before_arm.instructions.len() - 1]
            && after_arm.terminator == before_arm.terminator,
        "sink_store must remove exactly the arm's trailing store",
    )?;

    // The join: existing phis, the new phi, the moved store first among
    // non-phis, then everything the join already did.
    let before_join = get_block(&before_fun.cfg, &Some(s.join.clone())).unwrap();
    let after_join = get_block(&after_fun.cfg, &Some(s.join.clone()))
        .ok_or_else(|| anyhow!("sink_store removed the join block"))?;
    let n = phi_prefix_len(&before_join.instructions);
    require(
        after_join.instructions.len() == before_join.instructions.len() + 2,
        "sink_store must add exactly a phi and the moved store to the join",
    )?;
    require(
        after_join.instructions[..n] == before_join.instructions[..n],
        "sink_store changed the join's existing phis",
    )?;
    require(
        after_join.instructions[n + 2..] == before_join.instructions[n..],
        "sink_store changed the join's existing instructions",
    )?;
    require(
        after_join.terminator == before_join.terminator,
        "sink_store changed the join's terminator",
    )?;
    let (phi_id, phi) = &after_join.instructions[n];
    let Instruction::Phi { branches } = phi else {
        return Err(anyhow!(
            "the join's new instruction is `{}`, expected a phi",
            super::super::print::format_instruction(phi)
        ));
    };
    let from = |label: &Label| -> Option<LocalId> {
        branches.iter().find(|(l, _)| l == label).map(|(_, v)| *v)
    };
    require(
        branches.len() == 2
            && from(&head_label) == Some(*load_id)
            && from(&s.arm) == Some(s.source),
        format!(
            "the join's phi must merge the loaded value from '{}' with %{} from \
             '{}', found `{}`",
            head_label.as_str(),
            usize::from(s.source),
            s.arm.as_str(),
            super::super::print::format_instruction(phi)
        ),
    )?;
    require(
        after_join.instructions[n + 1]
            == (at, Instruction::Store { target: s.target, source: *phi_id }),
        format!(
            "the moved store must be the join's first non-phi instruction: \
             `store %{} <- %{}` under its old id %{}",
            usize::from(s.target),
            usize::from(*phi_id),
            usize::from(at)
        ),
    )?;

    // The minted ids must be new.
    for id in [*guard_id, *load_id, *phi_id] {
        require(
            definition_site(before_fun, id).is_none(),
            format!(
                "sink_store reuses %{}, which the function already defines",
                usize::from(id)
            ),
        )?;
    }
    require(
        guard_id != load_id && load_id != phi_id && guard_id != phi_id,
        "sink_store minted the same id twice",
    )?;

    // Nothing else: not any other block, not any other function.
    require(
        before.functions.len() == after.functions.len(),
        "sink_store changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!("sink_store on {} also changed {}", function, name.as_str()),
        )?;
    }
    let skip = [s.head.clone(), Some(s.arm.clone()), Some(s.join.clone())];
    for key in super::blocks_sorted(&before_fun.cfg) {
        if skip.contains(&key) {
            continue;
        }
        require(
            get_block(&before_fun.cfg, &key) == get_block(&after_fun.cfg, &key),
            format!(
                "sink_store changed block '{}', which is outside the triangle",
                block_label(&key)
            ),
        )?;
    }
    require(
        before_fun.cfg.named.len() == after_fun.cfg.named.len(),
        "sink_store changed the set of blocks",
    )?;

    Ok(())
}

/// Stores this rule accepts: the trailing store of a triangle arm, with a
/// target the arm does not define.
pub fn candidates(program: &Program) -> Vec<(String, LocalId)> {
    let mut out = Vec::new();
    for (name, fun) in &program.functions {
        for (_, block) in all_blocks(&fun.cfg) {
            let Some((id, Instruction::Store { .. })) = block.instructions.last() else {
                continue;
            };
            if site(fun, name.as_str(), *id).is_ok() {
                out.push((name.as_str().to_string(), *id));
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
    use crate::ir::{Block, Cfg, GlobalId};
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

    /// `__entry` defines the cell pointer %4, branches around `arm`'s store,
    /// and `join` already has one phi and one plain instruction.
    fn program_with_arm(arm_body: Vec<(LocalId, Instruction)>) -> Program {
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
                instructions: vec![
                    (
                        id(30),
                        Instruction::Phi {
                            branches: vec![(label("__entry"), id(1)), (label("arm"), id(1))],
                        },
                    ),
                    (id(32), num(9)),
                ],
                terminator: (id(31), Terminator::Return { value: Some(id(30)) }),
                hint_normalize: false,
            },
        );
        let cfg = Cfg::new(
            Block {
                instructions: vec![
                    (
                        id(4),
                        Instruction::GetField {
                            receiver: id(2),
                            field: "x".to_string(),
                            create_if_missing: false,
                        },
                    ),
                    (id(0), Instruction::BoolConstant { value: true }),
                    (id(1), num(5)),
                ],
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
        Program { functions, merge_partition_cells: Vec::new() }
    }

    fn store(target: usize, source: usize) -> Instruction {
        Instruction::Store { target: id(target), source: id(source) }
    }

    #[test]
    fn sinks_a_trailing_store() {
        let before = program_with_arm(vec![(id(13), store(4, 1))]);
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", id(13)).unwrap(), 1);
        verify(&before, &after, "f", id(13)).unwrap();

        let fun = after.get("f").unwrap();
        // Head: guard then load, appended.
        let head = &fun.cfg.entry.instructions;
        assert_eq!(head.len(), 5);
        assert!(matches!(head[3].1, Instruction::AssertValueCell { target } if target == id(4)));
        assert!(matches!(head[4].1, Instruction::Load { source } if source == id(4)));
        // Arm: empty.
        assert!(fun.cfg.named.get(&label("arm")).unwrap().instructions.is_empty());
        // Join: old phi, new phi, the store first among non-phis, then %32.
        let join = &fun.cfg.named.get(&label("join")).unwrap().instructions;
        assert_eq!(join.len(), 4);
        assert_eq!(join[0].0, id(30));
        assert!(matches!(join[1].1, Instruction::Phi { .. }));
        assert!(
            matches!(join[2], (sid, Instruction::Store { target, .. })
                if sid == id(13) && target == id(4))
        );
        assert_eq!(join[3].0, id(32));
    }

    /// The whole aliasing argument rests on the store being last.
    #[test]
    fn refuses_a_store_that_is_not_last() {
        let before = program_with_arm(vec![(id(13), store(4, 1)), (id(14), num(7))]);
        let mut after = before.clone();
        let error = apply(&mut after, "f", id(13)).unwrap_err().to_string();
        assert!(error.contains("not the last instruction"), "{}", error);
    }

    /// A target computed in the arm cannot be loaded before the branch; the
    /// error must point at the rule that fixes that.
    #[test]
    fn refuses_a_target_defined_in_the_arm() {
        let before = program_with_arm(vec![
            (
                id(10),
                Instruction::GetField {
                    receiver: id(2),
                    field: "y".to_string(),
                    create_if_missing: false,
                },
            ),
            (id(13), store(10, 1)),
        ]);
        let mut after = before.clone();
        let error = apply(&mut after, "f", id(13)).unwrap_err().to_string();
        assert!(error.contains("run speculate"), "{}", error);
    }

    #[test]
    fn refuses_an_instruction_that_is_not_a_store() {
        let before = program_with_arm(vec![(id(13), num(7))]);
        let mut after = before.clone();
        let error = apply(&mut after, "f", id(13)).unwrap_err().to_string();
        assert!(error.contains("not a store"), "{}", error);
    }

    /// The verifier must insist on the guard - it is what makes the
    /// store-back roundtrip loud instead of silently corrupting a closure
    /// or table cell.
    #[test]
    fn verify_rejects_a_missing_guard() {
        let before = program_with_arm(vec![(id(13), store(4, 1))]);
        let mut after = before.clone();
        apply(&mut after, "f", id(13)).unwrap();
        let head = &mut after.get_mut("f").unwrap().cfg.entry.instructions;
        head.remove(3);
        assert!(verify(&before, &after, "f", id(13)).is_err());
    }

    /// ...and on the phi merging the right values from the right edges.
    #[test]
    fn verify_rejects_swapped_phi_operands() {
        let before = program_with_arm(vec![(id(13), store(4, 1))]);
        let mut after = before.clone();
        apply(&mut after, "f", id(13)).unwrap();
        let join = after.get_mut("f").unwrap().cfg.named.get_mut(&label("join")).unwrap();
        let (_, Instruction::Phi { branches }) = &mut join.instructions[1] else {
            panic!("expected the new phi")
        };
        let (a, b) = (branches[0].1, branches[1].1);
        branches[0].1 = b;
        branches[1].1 = a;
        let error = verify(&before, &after, "f", id(13)).unwrap_err().to_string();
        assert!(error.contains("must merge"), "{}", error);
    }

    /// ...and on the store coming before everything the join already did -
    /// placing it later would reorder it against the join's own reads.
    #[test]
    fn verify_rejects_a_store_placed_after_the_joins_instructions() {
        let before = program_with_arm(vec![(id(13), store(4, 1))]);
        let mut after = before.clone();
        apply(&mut after, "f", id(13)).unwrap();
        let join = after.get_mut("f").unwrap().cfg.named.get_mut(&label("join")).unwrap();
        let s = join.instructions.remove(2);
        join.instructions.push(s);
        assert!(verify(&before, &after, "f", id(13)).is_err());
    }
}
