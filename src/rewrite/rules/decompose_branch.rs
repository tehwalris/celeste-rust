//! `decompose_branch` - branch on an `and`'s condition instead of its value.
//!
//! Pointed rule: names the select, as `%N`.
//!
//! # The shape
//!
//! `x and k or <effects>` cannot collapse to selects: the `or`'s false path
//! has side effects, so the short-circuit branch must stay. What remains
//! after the `and` half was decomposed is a branch on the *mixed* value:
//!
//! ```text
//!   M:  %p = select %c ? %k : %c        M:  br %c ? J : Y
//!       br %p ? J : Y             =>
//!   J:  %r = phi [M: %p, ...]           J:  %r = phi [M: %k, ...]
//! ```
//!
//! Under scalar concretization `%p` is harmless - each fragment holds one
//! bool, so the select yields one scalar. The moment `%c` is a per-lane
//! vector (`expand_bool`), `%p` would have to hold `%k` on some lanes and
//! the Bool `%c` on others, which the representation refuses - correctly,
//! and loudly. This rule makes `%p` unnecessary instead of representable.
//!
//! # Soundness
//!
//! The same edge argument as `convert_ternary`, without touching the arms:
//! the `J` edge is taken exactly when `%p` is truthy. If `%c` is truthy,
//! `%p` is `%k`, which is **statically truthy** (demanded), so the edge is
//! taken; if `%c` is falsy, `%p` is `%c` itself, falsy, not taken. So
//! branching on `%c` takes every edge `%p` took - per lane, since a vector
//! `%c` filters the same mask either way - and on the `J` edge `%p` was
//! `%k`, which is what the phi arm becomes. `%p` has no other readers
//! (demanded: its only uses are `M`'s terminator and `J`'s phis' `M`-arms),
//! so the select is deleted with nothing left dangling.
//!
//! Note `%c` need not be a bool: a truthy non-bool sends both the original
//! branch (via `%p = %k`, truthy) and the new one down the `J` edge, and
//! `Nil` down the other, so Lua truthiness agrees lane for lane.

use anyhow::{anyhow, Result};

use crate::ir::{FunDef, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::convert_ternary::statically_truthy;
use super::{get_block, require};

/// The shape this rule accepts, re-derived identically by `apply` and
/// `verify`.
struct Site {
    /// The block holding the select and the branch on it.
    block: Label,
    /// Index of the select in that block's instructions.
    select_index: usize,
    condition: LocalId,
    /// The statically truthy `and`-arm value.
    truthy: LocalId,
    /// The branch's true target, whose phis read the select.
    join: Label,
}

fn site(fun: &FunDef, function: &str, at: LocalId) -> Result<Site> {
    // Find the select.
    let mut found = None;
    for key in super::blocks_sorted(&fun.cfg) {
        let block = get_block(&fun.cfg, &key).unwrap();
        for (index, (id, instr)) in block.instructions.iter().enumerate() {
            if *id == at {
                found = Some((key.clone(), index, instr.clone()));
            }
        }
    }
    let Some((block_key, select_index, instr)) = found else {
        return Err(anyhow!("no instruction %{} in {}", usize::from(at), function));
    };
    let Some(block_label) = block_key else {
        return Err(anyhow!(
            "%{} is in the entry block, which cannot be a phi's predecessor",
            usize::from(at)
        ));
    };
    let Instruction::Select { condition, if_true, if_false } = instr else {
        return Err(anyhow!("%{} is not a select", usize::from(at)));
    };
    require(
        if_false == condition,
        format!(
            "%{} is not the `x and k` shape - its false side is not its \
             condition",
            usize::from(at)
        ),
    )?;
    require(
        statically_truthy(fun, if_true),
        format!(
            "%{}'s true side %{} is not statically truthy, so the branch \
             swap could drop lanes where it is falsy",
            usize::from(at),
            usize::from(if_true)
        ),
    )?;

    // The branch on it, in the same block.
    let block = fun.cfg.named.get(&block_label).unwrap();
    let Terminator::ConditionalBranch { condition: branch_cond, true_target, false_target } =
        block.terminator_kind()
    else {
        return Err(anyhow!(
            "'{}' does not end in a conditional branch",
            block_label.as_str()
        ));
    };
    require(
        branch_cond == &at,
        format!("'{}' does not branch on %{}", block_label.as_str(), usize::from(at)),
    )?;
    require(
        true_target != false_target,
        format!("'{}' branches to one place either way", block_label.as_str()),
    )?;
    require(
        true_target != &block_label,
        format!("'{}' is its own join; refusing a loop", block_label.as_str()),
    )?;

    // Every use of the select: the branch, and the true target's phis on
    // this block's edge. Anything else would keep the mixed value alive.
    for key in super::blocks_sorted(&fun.cfg) {
        let other = get_block(&fun.cfg, &key).unwrap();
        for (id, other_instr) in &other.instructions {
            let allowed = match other_instr {
                Instruction::Phi { branches } => {
                    key == Some(true_target.clone())
                        && branches
                            .iter()
                            .all(|(l, v)| *v != at || l == &block_label)
                }
                _ => false,
            };
            require(
                allowed || !other_instr.get_used_locals().contains(&at),
                format!(
                    "%{} is read by %{}, which is neither the branch nor a \
                     phi of '{}' on the '{}' edge",
                    usize::from(at),
                    usize::from(*id),
                    true_target.as_str(),
                    block_label.as_str()
                ),
            )?;
        }
        if key != Some(block_label.clone()) {
            require(
                !other.terminator_kind().get_used_locals().contains(&at),
                format!(
                    "%{} is read by the terminator of '{}'",
                    usize::from(at),
                    super::super::validate::block_label(&key)
                ),
            )?;
        }
    }

    Ok(Site {
        block: block_label,
        select_index,
        condition,
        truthy: if_true,
        join: true_target.clone(),
    })
}

/// The join's phis with the `M`-edge value swapped from the select to `%k`.
fn expected_join_instructions(s: &Site, before_join: &[(LocalId, Instruction)], at: LocalId) -> Vec<(LocalId, Instruction)> {
    before_join
        .iter()
        .map(|(id, instr)| {
            let new_instr = match instr {
                Instruction::Phi { branches } => Instruction::Phi {
                    branches: branches
                        .iter()
                        .map(|(l, v)| {
                            if l == &s.block && *v == at {
                                (l.clone(), s.truthy)
                            } else {
                                (l.clone(), *v)
                            }
                        })
                        .collect(),
                },
                other => other.clone(),
            };
            (*id, new_instr)
        })
        .collect()
}

pub fn apply(program: &mut Program, function: &str, at: LocalId) -> Result<usize> {
    let fun = program.get(function)?;
    let s = site(fun, function, at)?;

    let fun = program.get_mut(function)?;
    let join_block = fun.cfg.named.get_mut(&s.join).unwrap();
    join_block.instructions =
        expected_join_instructions(&s, &join_block.instructions, at);
    let block = fun.cfg.named.get_mut(&s.block).unwrap();
    block.instructions.remove(s.select_index);
    let Terminator::ConditionalBranch { condition, .. } = &mut block.terminator.1 else {
        unreachable!("site() checked the terminator");
    };
    *condition = s.condition;

    fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(1)
}

/// Independent check: re-derives the site from the *before* program and
/// insists the after program is exactly the prescription - the select gone,
/// the branch on its condition, the join's phis reading `%k` on that edge,
/// and not one other thing different.
pub fn verify(before: &Program, after: &Program, function: &str, at: LocalId) -> Result<()> {
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let s = site(before_fun, function, at)?;

    let before_block = before_fun.cfg.named.get(&s.block).unwrap();
    let after_block = after_fun
        .cfg
        .named
        .get(&s.block)
        .ok_or_else(|| anyhow!("decompose_branch removed '{}'", s.block.as_str()))?;
    let mut expected = before_block.instructions.clone();
    expected.remove(s.select_index);
    require(
        after_block.instructions == expected,
        "the select was not removed, or something else in its block changed",
    )?;
    let expected_terminator = match before_block.terminator_kind() {
        Terminator::ConditionalBranch { true_target, false_target, .. } => {
            Terminator::ConditionalBranch {
                condition: s.condition,
                true_target: true_target.clone(),
                false_target: false_target.clone(),
            }
        }
        _ => unreachable!("site() checked the terminator"),
    };
    require(
        after_block.terminator.0 == before_block.terminator.0
            && after_block.terminator.1 == expected_terminator,
        "the branch was not moved onto the select's condition",
    )?;

    let before_join = before_fun.cfg.named.get(&s.join).unwrap();
    let after_join = after_fun
        .cfg
        .named
        .get(&s.join)
        .ok_or_else(|| anyhow!("decompose_branch removed the join"))?;
    require(
        after_join.instructions == expected_join_instructions(&s, &before_join.instructions, at)
            && after_join.terminator == before_join.terminator
            && after_join.hint_normalize == before_join.hint_normalize,
        "the join's phis are not the prescribed substitution",
    )?;

    // Everything else: untouched. (`s.block` and `s.join` are distinct;
    // `site` refuses a self-loop explicitly.)
    for key in super::blocks_sorted(&before_fun.cfg) {
        if key == Some(s.block.clone()) || key == Some(s.join.clone()) {
            continue;
        }
        require(
            get_block(&before_fun.cfg, &key) == get_block(&after_fun.cfg, &key),
            format!(
                "decompose_branch changed block '{}', which is outside the site",
                super::super::validate::block_label(&key)
            ),
        )?;
    }
    require(
        before.functions.len() == after.functions.len(),
        "decompose_branch changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!(
                "decompose_branch on {} also changed {}",
                function,
                name.as_str()
            ),
        )?;
    }
    Ok(())
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

    /// `entry` computes a bool %1 and a number %2; `m` holds the mixed
    /// select and short-circuit branch; `y` is the effectful `or` arm
    /// producing %20; `j` merges.
    fn program() -> Program {
        let mut named = crate::ir::new_label_map();
        named.insert(
            label("m"),
            Block {
                instructions: vec![(
                    id(3),
                    Instruction::Select { condition: id(1), if_true: id(2), if_false: id(1) },
                )],
                terminator: (
                    id(9),
                    Terminator::ConditionalBranch {
                        condition: id(3),
                        true_target: label("j"),
                        false_target: label("y"),
                    },
                ),
                hint_normalize: false,
            },
        );
        named.insert(
            label("y"),
            Block {
                instructions: vec![
                    (id(20), Instruction::NumberConstant {
                        value: crate::pico8_num::Pico8Num::from_i16(1),
                    }),
                    (id(21), Instruction::Store { target: id(0), source: id(20) }),
                ],
                terminator: (id(22), Terminator::UnconditionalBranch { target: label("j") }),
                hint_normalize: false,
            },
        );
        named.insert(
            label("j"),
            Block {
                instructions: vec![(
                    id(30),
                    Instruction::Phi {
                        branches: vec![(label("m"), id(3)), (label("y"), id(20))],
                    },
                )],
                terminator: (id(40), Terminator::Return { value: Some(id(30)) }),
                hint_normalize: false,
            },
        );
        let cfg = Cfg::new(
            Block {
                instructions: vec![
                    (id(0), Instruction::Alloc),
                    (id(1), Instruction::BoolConstant { value: true }),
                    (id(2), Instruction::NumberConstant {
                        value: crate::pico8_num::Pico8Num::from_i16(-1),
                    }),
                ],
                terminator: (id(8), Terminator::UnconditionalBranch { target: label("m") }),
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
                arg_ids: vec![],
                cfg,
                source_span: None,
            },
        );
        Program { functions }
    }

    #[test]
    fn swaps_the_branch_and_the_phi_arm() {
        let before = program();
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", id(3)).unwrap(), 1);
        verify(&before, &after, "f", id(3)).unwrap();

        let fun = after.get("f").unwrap();
        let m = fun.cfg.named.get(&label("m")).unwrap();
        assert!(m.instructions.is_empty());
        assert_eq!(
            m.terminator.1,
            Terminator::ConditionalBranch {
                condition: id(1),
                true_target: label("j"),
                false_target: label("y"),
            }
        );
        let j = fun.cfg.named.get(&label("j")).unwrap();
        assert_eq!(
            j.instructions[0].1,
            Instruction::Phi { branches: vec![(label("m"), id(2)), (label("y"), id(20))] }
        );

        let errors = super::super::super::validate::validate_program(&after);
        assert!(errors.is_empty(), "{:?}", errors);
    }

    /// A select whose false side is not its condition is not the `x and k`
    /// shape - the branch swap would be meaningless.
    #[test]
    fn refuses_a_general_select() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        let m = fun.cfg.named.get_mut(&label("m")).unwrap();
        m.instructions[0].1 =
            Instruction::Select { condition: id(1), if_true: id(2), if_false: id(2) };
        let error = apply(&mut before, "f", id(3)).unwrap_err().to_string();
        assert!(error.contains("x and k"), "{}", error);
    }

    /// A falsy-able true side could take the `y` edge on lanes the swapped
    /// branch would send to `j`.
    #[test]
    fn refuses_a_non_truthy_arm() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        fun.cfg.entry.instructions[2].1 = Instruction::BoolConstant { value: true };
        let error = apply(&mut before, "f", id(3)).unwrap_err().to_string();
        assert!(error.contains("statically truthy"), "{}", error);
    }

    /// A reader besides the branch and the join's phis would keep the mixed
    /// value alive.
    #[test]
    fn refuses_another_reader() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        let y = fun.cfg.named.get_mut(&label("y")).unwrap();
        y.instructions.push((
            id(23),
            Instruction::UnaryOp { op: crate::ir::UnaryOp::Not, arg: id(3) },
        ));
        let error = apply(&mut before, "f", id(3)).unwrap_err().to_string();
        assert!(error.contains("is read by"), "{}", error);
    }

    /// The verifier is the trusted half: it must reject an applier that
    /// swapped the branch but forgot the phi, which would silently hand the
    /// join a value the new edge no longer implies.
    #[test]
    fn verify_rejects_an_unswapped_phi() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "f", id(3)).unwrap();
        let fun = after.get_mut("f").unwrap();
        let j = fun.cfg.named.get_mut(&label("j")).unwrap();
        j.instructions[0].1 =
            Instruction::Phi { branches: vec![(label("m"), id(1)), (label("y"), id(20))] };
        let error = verify(&before, &after, "f", id(3)).unwrap_err().to_string();
        assert!(error.contains("prescribed substitution"), "{}", error);
    }
}
