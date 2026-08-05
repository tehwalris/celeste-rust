//! `dedup_guards` - delete an assert that an identical assert already covers.
//!
//! Bulk rule: no location, deterministic, idempotent.
//!
//! # Why
//!
//! Guards are 18% of K after the pixel loops were unrolled, and much of that
//! is literal repetition: `inline` plants an `assert_closure` at every call
//! site it splices, so nine unrolled iterations carry nine identical
//! `assert_closure %v is obj.check_50 with captures [%2]` on the *same* `%v`
//! - `cse` unified the loads feeding them, but asserts produce nothing, so
//! `cse` has no key for them and `dce` sees an effect it must keep.
//!
//! # Soundness
//!
//! `assert_true`, `assert_pointer` and `assert_closure` are deterministic
//! functions of their SSA operands: `assert_true %v` inspects the value `%v`,
//! `assert_closure %v is f with captures [..]` compares the closure `%v`
//! points at against `f` and the listed capture *values* - all immutable once
//! defined. So if an identical assert (same variant, same operand ids, same
//! function, same capture list) executes on every path before this one, this
//! one is unobservable: either the first passed, and the second passes with
//! the same operands, or the first failed and execution never got here.
//! "Executes on every path before" is dominance: same block earlier, or any
//! block that strictly dominates this one.
//!
//! `assert_value_cell` is deliberately excluded: it reads the *cell* its
//! operand points to, and cell contents change. Two textually identical
//! `assert_value_cell %c` with a `store_closure %c` in between are different
//! checks.
//!
//! A chain of identical asserts is handled front to back: the first survives,
//! every dominated duplicate goes, and the survivor is what justifies each
//! deletion - `verify` insists the justifying twin is still present in the
//! *after* program at a dominating position.

use anyhow::{anyhow, Result};
use rustc_hash::FxHashMap;

use crate::ir::{Cfg, Instruction, Label, LocalId};

use super::super::program::Program;
use super::super::validate::Dominance;
use super::{blocks_sorted, get_block, require};

/// Is this an assert whose outcome depends only on its SSA operands?
fn dedupable(instr: &Instruction) -> bool {
    matches!(
        instr,
        Instruction::AssertTrue { .. }
            | Instruction::AssertPointer { .. }
            | Instruction::AssertClosure { .. }
    )
}

/// The key two asserts must share to be interchangeable: the exact
/// instruction, operands and all.
fn key(instr: &Instruction) -> String {
    format!("{:?}", instr)
}

/// For every dedupable assert, the position of each occurrence, in block
/// order and instruction order.
fn occurrences(cfg: &Cfg) -> FxHashMap<String, Vec<(Option<Label>, usize, LocalId)>> {
    let mut out: FxHashMap<String, Vec<(Option<Label>, usize, LocalId)>> = FxHashMap::default();
    for block_key in blocks_sorted(cfg) {
        let block = get_block(cfg, &block_key).expect("listed block exists");
        for (index, (id, instr)) in block.instructions.iter().enumerate() {
            if dedupable(instr) {
                out.entry(key(instr)).or_default().push((block_key.clone(), index, *id));
            }
        }
    }
    out
}

/// Does an occurrence at `(a, i)` cover one at `(b, j)`? Same block and
/// textually earlier, or a strictly dominating block.
fn covers(
    dominance: &Dominance,
    a: &(Option<Label>, usize),
    b: &(Option<Label>, usize),
) -> bool {
    if a.0 == b.0 {
        return a.1 < b.1;
    }
    let (Some(a_idx), Some(b_idx)) = (dominance.index_of(&a.0), dominance.index_of(&b.0)) else {
        return false;
    };
    dominance.dominates(a_idx, b_idx)
}

pub fn apply(program: &mut Program) -> Result<usize> {
    let mut changes = 0;
    for fun in program.functions.values_mut() {
        let dominance = Dominance::of(&fun.cfg);
        let mut doomed: FxHashMap<Option<Label>, Vec<LocalId>> = FxHashMap::default();
        for positions in occurrences(&fun.cfg).values() {
            for (b_block, b_index, b_id) in positions {
                let covered = positions.iter().any(|(a_block, a_index, a_id)| {
                    a_id != b_id
                        && covers(
                            &dominance,
                            &(a_block.clone(), *a_index),
                            &(b_block.clone(), *b_index),
                        )
                });
                if covered {
                    doomed.entry(b_block.clone()).or_default().push(*b_id);
                }
            }
        }
        for (block_key, ids) in doomed {
            let block = match &block_key {
                None => &mut fun.cfg.entry,
                Some(l) => fun.cfg.named.get_mut(l).expect("listed block exists"),
            };
            block.instructions.retain(|(id, _)| !ids.contains(id));
            changes += ids.len();
        }
    }
    Ok(changes)
}

/// Independent check: the after program must be the before program with some
/// instructions missing, nothing else - and every missing instruction must be
/// a dedupable assert with an identical twin *surviving in the after program*
/// at a position that covered the deleted one in the before program.
pub fn verify(before: &Program, after: &Program) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "dedup_guards changed the set of functions",
    )?;
    for (name, before_fun) in &before.functions {
        let after_fun = after
            .functions
            .get(name)
            .ok_or_else(|| anyhow!("dedup_guards removed function {}", name.as_str()))?;
        require(
            before_fun.arg_ids == after_fun.arg_ids
                && before_fun.capture_ids == after_fun.capture_ids,
            format!("dedup_guards changed the signature of {}", name.as_str()),
        )?;

        let dominance = Dominance::of(&before_fun.cfg);
        let survivors = occurrences(&after_fun.cfg);

        let before_keys = blocks_sorted(&before_fun.cfg);
        require(
            before_keys == blocks_sorted(&after_fun.cfg),
            format!("dedup_guards changed the block set of {}", name.as_str()),
        )?;
        for block_key in before_keys {
            let before_block = get_block(&before_fun.cfg, &block_key).expect("listed");
            let after_block = get_block(&after_fun.cfg, &block_key).expect("same set");
            require(
                before_block.terminator == after_block.terminator
                    && before_block.hint_normalize == after_block.hint_normalize,
                format!("dedup_guards changed a terminator in {}", name.as_str()),
            )?;

            // The after block must be a subsequence of the before block;
            // every gap must be a justified deletion.
            let mut after_iter = after_block.instructions.iter().peekable();
            for (index, (id, instr)) in before_block.instructions.iter().enumerate() {
                if let Some((after_id, after_instr)) = after_iter.peek() {
                    if after_id == id {
                        require(
                            *after_instr == *instr,
                            format!("dedup_guards rewrote an instruction in {}", name.as_str()),
                        )?;
                        after_iter.next();
                        continue;
                    }
                }
                // Deleted. Must be a dedupable assert with a surviving,
                // covering twin.
                require(
                    dedupable(instr),
                    format!(
                        "dedup_guards deleted a non-assert instruction in {}",
                        name.as_str()
                    ),
                )?;
                let twins = survivors.get(&key(instr)).cloned().unwrap_or_default();
                let justified = twins.iter().any(|(twin_block, twin_index, _)| {
                    covers(
                        &dominance,
                        &(twin_block.clone(), *twin_index),
                        &(block_key.clone(), index),
                    )
                });
                require(
                    justified,
                    format!(
                        "dedup_guards deleted an assert in {} with no surviving \
                         covering twin",
                        name.as_str()
                    ),
                )?;
            }
            require(
                after_iter.next().is_none(),
                format!("dedup_guards added instructions in {}", name.as_str()),
            )?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, FunDef, GlobalId, Terminator};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(name: &str) -> Label {
        Label::from(name.to_string())
    }

    fn assert_true(v: usize) -> Instruction {
        Instruction::AssertTrue { value: id(v) }
    }

    fn program(entry: Block, named: Vec<(&str, Block)>) -> Program {
        let mut map = crate::ir::new_label_map();
        for (name, block) in named {
            map.insert(label(name), block);
        }
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![],
            cfg: Cfg::new(entry, map),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions, merge_partition_cells: Vec::new() }
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

    fn ret() -> Terminator {
        Terminator::Return { value: None }
    }

    fn br(target: &str) -> Terminator {
        Terminator::UnconditionalBranch { target: label(target) }
    }

    #[test]
    fn deletes_a_dominated_duplicate() {
        let before = program(
            block(
                vec![
                    (1, Instruction::BoolConstant { value: true }),
                    (2, assert_true(1)),
                ],
                3,
                br("next"),
            ),
            vec![("next", block(vec![(4, assert_true(1))], 5, ret()))],
        );
        let mut after = before.clone();
        let n = apply(&mut after).unwrap();
        assert_eq!(n, 1);
        verify(&before, &after).unwrap();
        let fun = after.get("f").unwrap();
        assert!(fun.cfg.named[&label("next")].instructions.is_empty());
        assert_eq!(fun.cfg.entry.instructions.len(), 2);
    }

    #[test]
    fn deletes_a_same_block_duplicate_keeping_the_first() {
        let before = program(
            block(
                vec![
                    (1, Instruction::BoolConstant { value: true }),
                    (2, assert_true(1)),
                    (3, assert_true(1)),
                ],
                4,
                ret(),
            ),
            vec![],
        );
        let mut after = before.clone();
        assert_eq!(apply(&mut after).unwrap(), 1);
        verify(&before, &after).unwrap();
        let fun = after.get("f").unwrap();
        assert_eq!(fun.cfg.entry.instructions.len(), 2);
        assert_eq!(fun.cfg.entry.instructions[1].0, id(2));
    }

    #[test]
    fn keeps_asserts_on_different_values() {
        let before = program(
            block(
                vec![
                    (1, Instruction::BoolConstant { value: true }),
                    (2, Instruction::BoolConstant { value: true }),
                    (3, assert_true(1)),
                    (4, assert_true(2)),
                ],
                5,
                ret(),
            ),
            vec![],
        );
        let mut after = before.clone();
        assert_eq!(apply(&mut after).unwrap(), 0);
        verify(&before, &after).unwrap();
    }

    #[test]
    fn keeps_asserts_in_sibling_blocks() {
        // Neither arm dominates the other; both asserts stay.
        let before = program(
            block(
                vec![
                    (1, Instruction::BoolConstant { value: true }),
                    (2, Instruction::BoolConstant { value: true }),
                ],
                3,
                Terminator::ConditionalBranch {
                    condition: id(2),
                    true_target: label("a"),
                    false_target: label("b"),
                },
            ),
            vec![
                ("a", block(vec![(4, assert_true(1))], 5, ret())),
                ("b", block(vec![(6, assert_true(1))], 7, ret())),
            ],
        );
        let mut after = before.clone();
        assert_eq!(apply(&mut after).unwrap(), 0);
        verify(&before, &after).unwrap();
    }

    #[test]
    fn leaves_assert_value_cell_alone() {
        let before = program(
            block(
                vec![
                    (1, Instruction::Alloc),
                    (2, Instruction::AssertValueCell { target: id(1) }),
                    (3, Instruction::AssertValueCell { target: id(1) }),
                ],
                4,
                ret(),
            ),
            vec![],
        );
        let mut after = before.clone();
        assert_eq!(apply(&mut after).unwrap(), 0);
        verify(&before, &after).unwrap();
    }

    #[test]
    fn verify_rejects_an_unjustified_deletion() {
        let before = program(
            block(
                vec![
                    (1, Instruction::BoolConstant { value: true }),
                    (2, assert_true(1)),
                ],
                3,
                ret(),
            ),
            vec![],
        );
        let mut after = before.clone();
        {
            let fun = after.functions.values_mut().next().unwrap();
            fun.cfg.entry.instructions.truncate(1); // delete the only assert
        }
        let err = verify(&before, &after).unwrap_err();
        assert!(err.to_string().contains("no surviving covering twin"), "{}", err);
    }

    #[test]
    fn verify_rejects_a_deleted_non_assert() {
        let before = program(
            block(
                vec![
                    (1, Instruction::BoolConstant { value: true }),
                    (2, assert_true(1)),
                ],
                3,
                ret(),
            ),
            vec![],
        );
        let mut after = before.clone();
        {
            let fun = after.functions.values_mut().next().unwrap();
            fun.cfg.entry.instructions.remove(0); // delete the constant
        }
        let err = verify(&before, &after).unwrap_err();
        assert!(err.to_string().contains("non-assert"), "{}", err);
    }
}
