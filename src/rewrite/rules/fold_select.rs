//! `fold_select` - resolve selects and branches whose condition's
//! *truthiness* is static, by def-chain reasoning alone.
//!
//! # Why
//!
//! Lua's `a and b` compiles to `select a ? b : a`, so once one link of an
//! `and` chain is a constant `false`, the whole chain is falsy - but not
//! constant. In the collapsed object-table loops, `assume_eq` plus
//! `fold_reflexive` turn the `o ~= obj` link into `false`, leaving
//!
//! ```text
//!   %770 = select %767 ? %768 : %767      -- %768 = false
//!   %774 = select %770 ? %772 : %770
//!   ...
//!   %867 = br %866 ? if_body : if_join
//! ```
//!
//! `%770` is falsy whatever `%767` is (truthy picks `false`; falsy picks
//! `%767` itself), so `%774` resolves to `%770`, and so on down to the
//! branch, which can only go one way. No single-instruction fold sees this;
//! the chain does.
//!
//! # What it does
//!
//! A fixpoint over each function's def chains classifies locals:
//!
//!   * **falsy**: `nil`, `false`, or a select whose true-arm is falsy and
//!     whose false-arm is falsy or the condition itself (the `and` shape);
//!   * **truthy**: `true`, a number or string constant, an `alloc`, or a
//!     select of truthy arms - or the `or` shape, `select c ? c : t` with
//!     `t` truthy.
//!
//! Then every select whose (resolved) condition is classified is replaced
//! by the arm it must produce - by substitution, since the IR has no copy -
//! and every conditional branch on a classified condition becomes the
//! unconditional branch it must take. The substituted select instructions
//! stay in place, dead, for `dce` to sweep.
//!
//! # Soundness
//!
//! The interpreter's `select` on a *uniform* condition returns the chosen
//! arm's value verbatim, before any cross-arm type check, and every
//! classified value is uniform by construction (built from scalar constants
//! and selects of them). A falsy condition picks the false arm exactly, a
//! truthy one the true arm; `br` on the same condition takes the same edge
//! the values dictate. Classification excludes everything whose truthiness
//! the abstract interpreter cannot decide (`UnknownBool` and `NilPointer`
//! error as conditions; accessor results may be `NilPointer`, so they are
//! not classified truthy - only `alloc`, which always yields a real
//! pointer).
//!
//! A classified-but-kept instruction can still fail at runtime (a select
//! whose own condition is `UnknownBool` is an error): classification never
//! removes it, so the failure still happens where it always did. What
//! `dce` later deletes as dead is `dce`'s bargain, unchanged by this rule.
//!
//! Bulk rule: no location, deterministic, idempotent. `apply` and `verify`
//! share the classification, so they agree by construction; `verify`
//! insists the after program is exactly the before program with the derived
//! substitution and branch rewrites applied, and not one other thing
//! different.

use anyhow::Result;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{FunDef, Instruction, LocalId, Terminator};

use super::super::program::Program;
use super::require;

#[derive(Default)]
struct Classification {
    falsy: FxHashSet<LocalId>,
    truthy: FxHashSet<LocalId>,
    /// Substituted selects: id -> the arm it must produce (unresolved).
    substitution: FxHashMap<LocalId, LocalId>,
}

impl Classification {
    fn resolve(&self, mut id: LocalId) -> LocalId {
        let mut guard = 0;
        while let Some(next) = self.substitution.get(&id) {
            id = *next;
            guard += 1;
            if guard > self.substitution.len() + 1 {
                break;
            }
        }
        id
    }
}

/// Derived from the function alone, so `apply` and `verify` agree by
/// construction.
fn classify(fun: &FunDef) -> Classification {
    let mut defs: FxHashMap<LocalId, &Instruction> = FxHashMap::default();
    for block in fun.cfg.iter_blocks() {
        for (id, instr) in &block.instructions {
            defs.insert(*id, instr);
        }
    }

    let mut c = Classification::default();
    // Seed with the constants.
    for (id, instr) in &defs {
        match instr {
            Instruction::NilConstant | Instruction::BoolConstant { value: false } => {
                c.falsy.insert(*id);
            }
            Instruction::BoolConstant { value: true }
            | Instruction::NumberConstant { .. }
            | Instruction::StringConstant { .. }
            | Instruction::Alloc => {
                c.truthy.insert(*id);
            }
            _ => {}
        }
    }

    loop {
        let mut changed = false;
        for (id, instr) in &defs {
            let Instruction::Select { condition, if_true, if_false } = instr else { continue };
            let cond = c.resolve(*condition);
            let t = c.resolve(*if_true);
            let f = c.resolve(*if_false);

            // A select on a classified condition produces one arm.
            if !c.substitution.contains_key(id) {
                if c.falsy.contains(&cond) {
                    c.substitution.insert(*id, *if_false);
                    changed = true;
                } else if c.truthy.contains(&cond) {
                    c.substitution.insert(*id, *if_true);
                    changed = true;
                }
            }

            // The `and` shape: falsy either way.
            if !c.falsy.contains(id)
                && c.falsy.contains(&t)
                && (f == cond || c.falsy.contains(&f))
            {
                c.falsy.insert(*id);
                changed = true;
            }
            // Truthy either way, including the `or` shape.
            if !c.truthy.contains(id)
                && ((c.truthy.contains(&t) && c.truthy.contains(&f))
                    || (t == cond && c.truthy.contains(&f)))
            {
                c.truthy.insert(*id);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    c
}

/// The terminator `block` must end with after folding, if it changes.
fn folded_terminator(c: &Classification, terminator: &Terminator) -> Option<Terminator> {
    let Terminator::ConditionalBranch { condition, true_target, false_target } = terminator
    else {
        return None;
    };
    let cond = c.resolve(*condition);
    if c.falsy.contains(&cond) {
        Some(Terminator::UnconditionalBranch { target: false_target.clone() })
    } else if c.truthy.contains(&cond) {
        Some(Terminator::UnconditionalBranch { target: true_target.clone() })
    } else {
        None
    }
}

pub fn apply(program: &mut Program) -> Result<usize> {
    let mut changes = 0;
    for fun in program.functions.values_mut() {
        let c = classify(fun);
        let resolve = |id: LocalId| c.resolve(id);
        for block in
            std::iter::once(&mut fun.cfg.entry).chain(fun.cfg.named.values_mut())
        {
            // A substituted select itself stays in place, dead, for `dce`;
            // only its uses are rewritten.
            for (_, instr) in block.instructions.iter_mut() {
                let mapped = instr.map_local_ids(resolve);
                if mapped != *instr {
                    *instr = mapped;
                    changes += 1;
                }
            }
            if let Some(new_terminator) = folded_terminator(&c, block.terminator_kind()) {
                block.terminator.1 = new_terminator;
                changes += 1;
            } else {
                let mapped = block.terminator.1.map_local_ids(resolve);
                if mapped != block.terminator.1 {
                    block.terminator.1 = mapped;
                    changes += 1;
                }
            }
        }
    }
    Ok(changes)
}

/// Independent check in the `fold_reflexive` style: recompute the
/// classification from the before program and insist the after program is
/// exactly the prescription.
pub fn verify(before: &Program, after: &Program) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "fold_select changed the set of functions",
    )?;
    for (name, before_fun) in &before.functions {
        let after_fun = after.functions.get(name).ok_or_else(|| {
            anyhow::anyhow!("fold_select removed function {}", name.as_str())
        })?;
        let c = classify(before_fun);
        let resolve = |id: LocalId| c.resolve(id);
        for key in super::blocks_sorted(&before_fun.cfg) {
            let block_name =
                key.as_ref().map(|l| l.as_str().to_string()).unwrap_or_else(|| "__entry".into());
            let before_block = super::get_block(&before_fun.cfg, &key).unwrap();
            let after_block = super::get_block(&after_fun.cfg, &key).ok_or_else(|| {
                anyhow::anyhow!(
                    "fold_select removed block '{}' of {}",
                    block_name,
                    name.as_str()
                )
            })?;

            let mut expected = before_block.clone();
            for (_, instr) in expected.instructions.iter_mut() {
                *instr = instr.map_local_ids(resolve);
            }
            if let Some(new_terminator) = folded_terminator(&c, expected.terminator_kind()) {
                expected.terminator.1 = new_terminator;
            } else {
                expected.terminator.1 = expected.terminator.1.map_local_ids(resolve);
            }
            require(
                *after_block == expected,
                format!(
                    "fold_select changed '{}' of {} beyond the prescription",
                    block_name,
                    name.as_str()
                ),
            )?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, GlobalId, Label};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(name: &str) -> Label {
        Label::from(name.to_string())
    }

    fn select(c: usize, t: usize, f: usize) -> Instruction {
        Instruction::Select { condition: id(c), if_true: id(t), if_false: id(f) }
    }

    fn program(instructions: Vec<(LocalId, Instruction)>, terminator: Terminator) -> Program {
        let entry = Block {
            instructions,
            terminator: (id(99), terminator),
            hint_normalize: false,
        };
        let mut named = crate::ir::new_label_map();
        named.insert(
            label("a"),
            Block {
                instructions: vec![],
                terminator: (id(98), Terminator::Return { value: None }),
                hint_normalize: false,
            },
        );
        named.insert(
            label("b"),
            Block {
                instructions: vec![],
                terminator: (id(97), Terminator::Return { value: None }),
                hint_normalize: false,
            },
        );
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            arg_ids: vec![Some(id(1))],
            capture_ids: vec![],
            cfg: Cfg::new(entry, named),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions, merge_partition_cells: Vec::new() }
    }

    fn branch(c: usize) -> Terminator {
        Terminator::ConditionalBranch {
            condition: id(c),
            true_target: label("a"),
            false_target: label("b"),
        }
    }

    #[test]
    fn folds_an_and_chain_poisoned_by_false() {
        // %1 is an opaque argument; the chain is
        //   %3 = %1 and false        (select %1 ? %2(false) : %1)
        //   %4 = %3 and %1           (select %3 ? %1 : %3)
        //   br %4 ? a : b
        let mut p = program(
            vec![
                (id(2), Instruction::BoolConstant { value: false }),
                (id(3), select(1, 2, 1)),
                (id(4), select(3, 1, 3)),
            ],
            branch(4),
        );
        let before = p.clone();
        assert!(apply(&mut p).unwrap() > 0);
        verify(&before, &p).unwrap();
        let entry = &p.functions.values().next().unwrap().cfg.entry;
        // %3 is falsy (the and shape), so %4 resolves to %3 and the branch
        // must take the false edge.
        assert!(matches!(
            entry.terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == label("b")
        ));
    }

    #[test]
    fn folds_a_select_on_a_truthy_condition() {
        let mut p = program(
            vec![
                (
                    id(2),
                    Instruction::NumberConstant {
                        value: crate::pico8_num::Pico8Num::from_i16(7),
                    },
                ),
                (id(3), select(2, 1, 2)),
                // A later user of %3 sees %1 instead.
                (id(4), select(3, 3, 3)),
            ],
            branch(1),
        );
        let before = p.clone();
        assert!(apply(&mut p).unwrap() > 0);
        verify(&before, &p).unwrap();
        let entry = &p.functions.values().next().unwrap().cfg.entry;
        assert_eq!(entry.instructions[2].1, select(1, 1, 1));
        // The opaque branch stays.
        assert!(matches!(entry.terminator_kind(), Terminator::ConditionalBranch { .. }));
    }

    #[test]
    fn leaves_an_unclassified_chain() {
        let mut p = program(
            vec![
                (id(2), Instruction::BoolConstant { value: true }),
                // select %1 ? true : %1 - truthy pick of a truthy arm, but
                // the false arm is the opaque condition: not classifiable
                // as truthy (it is falsy on the path that picks it), and
                // not falsy either.
                (id(3), select(1, 2, 1)),
            ],
            branch(3),
        );
        let before = p.clone();
        assert_eq!(apply(&mut p).unwrap(), 0);
        assert_eq!(
            p.functions.values().next().unwrap().cfg.entry,
            before.functions.values().next().unwrap().cfg.entry
        );
    }

    #[test]
    fn folds_the_or_shape() {
        // %3 = %1 or 7 (select %1 ? %1 : %2) is truthy; the branch on it
        // must take the true edge.
        let mut p = program(
            vec![
                (
                    id(2),
                    Instruction::NumberConstant {
                        value: crate::pico8_num::Pico8Num::from_i16(7),
                    },
                ),
                (id(3), select(1, 1, 2)),
            ],
            branch(3),
        );
        let before = p.clone();
        assert!(apply(&mut p).unwrap() > 0);
        verify(&before, &p).unwrap();
        let entry = &p.functions.values().next().unwrap().cfg.entry;
        assert!(matches!(
            entry.terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == label("a")
        ));
    }

    #[test]
    fn verify_rejects_a_wrong_edge() {
        let mut p = program(
            vec![
                (id(2), Instruction::BoolConstant { value: false }),
                (id(3), select(1, 2, 1)),
            ],
            branch(3),
        );
        let before = p.clone();
        apply(&mut p).unwrap();
        {
            let fun = p.functions.values_mut().next().unwrap();
            fun.cfg.entry.terminator.1 =
                Terminator::UnconditionalBranch { target: label("a") };
        }
        let err = verify(&before, &p).unwrap_err();
        assert!(err.to_string().contains("beyond the prescription"), "{}", err);
    }
}
