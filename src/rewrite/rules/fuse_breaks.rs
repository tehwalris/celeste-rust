//! `fuse_breaks` - merge two consecutive early-exit branches into one.
//!
//! # Why
//!
//! The inlined `spikes_at` body tests four tile shapes in a cascade, and every
//! hit jumps straight out of both tile loops:
//!
//! ```text
//!   H:  ..compute c..    br c  ? T  : E     T:  %a = bool true; br J
//!   E:  ..compute c2..   br c2 ? T2 : F     T2: %b = bool true; br J
//!   F:  ..the next test, or the loop latch..
//! ```
//!
//! Each of those branches splits the state on a per-lane bool. `mask_loop`
//! can absorb the loops, but it needs a loop body with a *single* break
//! branch. This rule provides that shape: applied repeatedly it folds the
//! cascade into one `br (c or c2 or ...) ? T_last : latch`.
//!
//! # What it does
//!
//! Moves `E`'s instructions to the end of `H` (they are now computed eagerly,
//! by lanes that would have taken `T` - the usual speculation bargain), mints
//! `cc = select c ? c : c2` (the established `or` spelling), points `H` at
//! `br cc ? T2 : F`, and deletes the block `E`. `T` keeps its instructions,
//! its terminator, and its entries in `J`'s phis - it is simply unreachable,
//! and a later `dce` entry sweeps the block and the phi entries together,
//! exactly as in the fold/dce cascade.
//!
//! # Why the merged edge means the same thing
//!
//! A lane with `c` true used to reach `J` through `T` and contribute `T`'s
//! phi values; now it reaches `J` through `T2` and contributes `T2`'s. So the
//! rule requires, for every phi in `J`, that the `T` and `T2` values are both
//! defined by `BoolConstant`s of the same value - then the switch of edges is
//! invisible. `T` and `T2` may hold nothing but such constants (a store in
//! `T` would be lost; a store in `T2` would newly fire for `c`-lanes), and
//! `E`'s instructions must be speculatable, because `c`-lanes now execute
//! them. Nothing can go wrong silently: what speculation admits is loud
//! failures, and those are a screening question, as everywhere else.
//!
//! # Shape requirements, deliberately narrow
//!
//! `T`, `E` each reachable only from `H`; `T2` only from `E`; `T`/`T2`
//! unconditionally branch to the same `J`; `E` and the new false target `F`
//! hold no phis (their predecessor changes); no `hint_normalize` in `H` or
//! `E`. Deleting `E` outright (rather than leaving an empty husk) is what
//! keeps `T2`'s predecessor set clean for the next application of this rule.

use anyhow::{anyhow, Result};

use crate::ir::{FunDef, Instruction, Label, LocalId, SlotMap, Terminator};

use super::super::print::format_instruction;
use super::super::program::Program;
use super::if_convert::is_speculatable;
use super::speculate_region::defining_instruction;
use super::{predecessors, require, LocalIdAllocator};

/// The shape, re-derived identically by `apply` and `verify`.
struct Site {
    c: LocalId,
    e: Label,
    c2: LocalId,
    t2: Label,
    f: Label,
}

/// The single named predecessor of `label`, or a refusal.
fn sole_predecessor(
    preds: &rustc_hash::FxHashMap<Option<Label>, Vec<Option<Label>>>,
    label: &Label,
) -> Result<Label> {
    match preds.get(&Some(label.clone())).map(|p| p.as_slice()) {
        Some([Some(p)]) => Ok(p.clone()),
        _ => Err(anyhow!(
            "'{}' must have exactly one predecessor",
            label.as_str()
        )),
    }
}

fn constants_only(fun: &FunDef, label: &Label) -> Result<()> {
    for (id, instr) in &fun.cfg.named[label].instructions {
        require(
            matches!(instr, Instruction::BoolConstant { .. }),
            format!(
                "%{} in break block '{}' is `{}`, not a bool constant",
                usize::from(*id),
                label.as_str(),
                format_instruction(instr)
            ),
        )?;
    }
    Ok(())
}

fn site(fun: &FunDef, function: &str, head: &Label) -> Result<Site> {
    let head_block = fun.cfg.named.get(head).ok_or_else(|| {
        anyhow!("no block named '{}' in {}", head.as_str(), function)
    })?;
    let Terminator::ConditionalBranch { condition: c, true_target: t, false_target: e } =
        head_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in a conditional branch", head.as_str()));
    };
    let (c, t, e) = (*c, t.clone(), e.clone());
    require(t != e, "the branch's two targets must differ")?;
    require(
        !head_block.hint_normalize,
        format!("'{}' is a hint_normalize block", head.as_str()),
    )?;

    let preds = predecessors(&fun.cfg);
    require(
        sole_predecessor(&preds, &t)? == *head,
        format!("'{}' must be reached only from '{}'", t.as_str(), head.as_str()),
    )?;
    require(
        sole_predecessor(&preds, &e)? == *head,
        format!("'{}' must be reached only from '{}'", e.as_str(), head.as_str()),
    )?;

    // T: bool constants only, one unconditional jump to the join.
    constants_only(fun, &t)?;
    let Terminator::UnconditionalBranch { target: join } = fun.cfg.named[&t].terminator_kind()
    else {
        return Err(anyhow!("'{}' must branch unconditionally", t.as_str()));
    };

    // E: speculatable, phi-free, ending in the next test of the cascade.
    let e_block = &fun.cfg.named[&e];
    require(
        !e_block.hint_normalize,
        format!("'{}' is a hint_normalize block", e.as_str()),
    )?;
    for (id, instr) in &e_block.instructions {
        require(
            is_speculatable(instr),
            format!(
                "%{} in '{}' is not speculatable: {}",
                usize::from(*id),
                e.as_str(),
                format_instruction(instr)
            ),
        )?;
    }
    let Terminator::ConditionalBranch { condition: c2, true_target: t2, false_target: f } =
        e_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in a conditional branch", e.as_str()));
    };
    let (c2, t2, f) = (*c2, t2.clone(), f.clone());
    require(t2 != f, "the second branch's two targets must differ")?;
    require(t2 != t, format!("'{}' and '{}' must be distinct break blocks", t2.as_str(), t.as_str()))?;
    require(
        sole_predecessor(&preds, &t2)? == e,
        format!("'{}' must be reached only from '{}'", t2.as_str(), e.as_str()),
    )?;

    // T2: same conditions as T, same join.
    constants_only(fun, &t2)?;
    require(
        matches!(fun.cfg.named[&t2].terminator_kind(),
            Terminator::UnconditionalBranch { target } if target == join),
        format!(
            "'{}' must branch unconditionally to '{}', where '{}' goes",
            t2.as_str(),
            join.as_str(),
            t.as_str()
        ),
    )?;

    // F: gains H as a predecessor in place of E, so it must have no phis.
    // (F cannot be H itself: a phi-free block cannot head a cycle carrying
    // values, and a self-edge would be exactly that.)
    require(f != *head, "the fall-through must not loop back to the head")?;
    require(
        !fun.cfg.named[&f]
            .instructions
            .iter()
            .any(|(_, i)| matches!(i, Instruction::Phi { .. })),
        format!("the fall-through '{}' has phis", f.as_str()),
    )?;

    // The join's phis: the T and T2 edges must carry the same constant.
    for (id, instr) in &fun.cfg.named[join].instructions {
        let Instruction::Phi { branches } = instr else { continue };
        let value_from = |from: &Label| -> Result<LocalId> {
            branches
                .iter()
                .find(|(label, _)| label == from)
                .map(|(_, value)| *value)
                .ok_or_else(|| {
                    anyhow!(
                        "phi %{} in '{}' has no entry for '{}'",
                        usize::from(*id),
                        join.as_str(),
                        from.as_str()
                    )
                })
        };
        let (vt, vt2) = (value_from(&t)?, value_from(&t2)?);
        let constant = |value: LocalId| match defining_instruction(fun, value) {
            Some(Instruction::BoolConstant { value }) => Some(*value),
            _ => None,
        };
        match (constant(vt), constant(vt2)) {
            (Some(a), Some(b)) if a == b => {}
            _ => {
                return Err(anyhow!(
                    "phi %{} in '{}' gets %{} from '{}' but %{} from '{}'; both \
                     must be the same bool constant for the edges to be \
                     interchangeable",
                    usize::from(*id),
                    join.as_str(),
                    usize::from(vt),
                    t.as_str(),
                    usize::from(vt2),
                    t2.as_str()
                ))
            }
        }
    }

    Ok(Site { c, e, c2, t2, f })
}

pub fn apply(program: &mut Program, function: &str, head: &str) -> Result<usize> {
    let head = &Label::from(head.to_string());
    let fun = program.get(function)?;
    let s = site(fun, function, head)?;
    let cc = LocalIdAllocator::for_function(fun).fresh();

    let fun = program.get_mut(function)?;
    let moved = fun.cfg.named.remove(&s.e).unwrap().instructions;
    let changed = moved.len() + 2;
    let head_block = fun.cfg.named.get_mut(head).unwrap();
    head_block.instructions.extend(moved);
    head_block.instructions.push((
        cc,
        Instruction::Select { condition: s.c, if_true: s.c, if_false: s.c2 },
    ));
    head_block.terminator.1 = Terminator::ConditionalBranch {
        condition: cc,
        true_target: s.t2,
        false_target: s.f,
    };
    fun.cfg.slots = std::sync::Arc::new(SlotMap::identity());
    Ok(changed)
}

/// Independent check: re-derives the site from the before program, learns the
/// minted id from the after program at a position computed from the before
/// program alone, and requires the after program to be exactly the extended
/// head, the deleted `E` - and nothing else.
pub fn verify(before: &Program, after: &Program, function: &str, head: &str) -> Result<()> {
    let head = &Label::from(head.to_string());
    let before_fun = before.get(function)?;
    let s = site(before_fun, function, head)?;
    let after_fun = after.get(function)?;

    require(
        before.functions.len() == after.functions.len(),
        "fuse_breaks changed the set of functions",
    )?;
    for (other, before_other) in &before.functions {
        if other.as_str() == function {
            continue;
        }
        require(
            after.functions.get(other) == Some(before_other),
            format!("fuse_breaks on {} also changed {}", function, other.as_str()),
        )?;
    }
    require(
        before_fun.arg_ids == after_fun.arg_ids
            && before_fun.capture_ids == after_fun.capture_ids
            && before_fun.cfg.entry == after_fun.cfg.entry,
        "fuse_breaks changed the function's frame",
    )?;
    require(
        after_fun.cfg.named.len() == before_fun.cfg.named.len() - 1
            && after_fun.cfg.named.get(&s.e).is_none(),
        format!("fuse_breaks must delete exactly the block '{}'", s.e.as_str()),
    )?;

    let before_ids: rustc_hash::FxHashSet<LocalId> = before_fun
        .cfg
        .named
        .values()
        .chain(std::iter::once(&before_fun.cfg.entry))
        .flat_map(|b| b.instructions.iter().map(|(id, _)| *id))
        .collect();

    let before_head = &before_fun.cfg.named[head];
    let before_e = &before_fun.cfg.named[&s.e];
    let after_head = after_fun.cfg.named.get(head).ok_or_else(|| {
        anyhow!("fuse_breaks removed the head '{}'", head.as_str())
    })?;

    // The head: original instructions, E's instructions, the minted select.
    let want_len = before_head.instructions.len() + before_e.instructions.len() + 1;
    require(
        after_head.instructions.len() == want_len,
        format!(
            "'{}' has {} instructions, expected {}",
            head.as_str(),
            after_head.instructions.len(),
            want_len
        ),
    )?;
    let mut want: Vec<(LocalId, Instruction)> = before_head.instructions.clone();
    want.extend(before_e.instructions.iter().cloned());
    let (cc, minted) = &after_head.instructions[want_len - 1];
    require(
        !before_ids.contains(cc),
        format!("the merged condition %{} is not a fresh id", usize::from(*cc)),
    )?;
    require(
        *minted
            == Instruction::Select { condition: s.c, if_true: s.c, if_false: s.c2 },
        format!(
            "the merged condition must be `select %{} ? %{} : %{}`, found `{}`",
            usize::from(s.c),
            usize::from(s.c),
            usize::from(s.c2),
            format_instruction(minted)
        ),
    )?;
    for (position, pair) in want.iter().enumerate() {
        require(
            &after_head.instructions[position] == pair,
            format!(
                "'{}' diverges at position {}: expected %{}",
                head.as_str(),
                position,
                usize::from(pair.0)
            ),
        )?;
    }
    require(
        after_head.terminator.0 == before_head.terminator.0
            && *after_head.terminator_kind()
                == Terminator::ConditionalBranch {
                    condition: *cc,
                    true_target: s.t2.clone(),
                    false_target: s.f.clone(),
                },
        format!("'{}' does not branch on the merged condition", head.as_str()),
    )?;
    require(
        after_head.hint_normalize == before_head.hint_normalize,
        format!("fuse_breaks changed the normalize hint of '{}'", head.as_str()),
    )?;

    // Everything else: untouched.
    require(
        before_fun.cfg.entry == after_fun.cfg.entry,
        "fuse_breaks changed the entry block",
    )?;
    for (label, before_block) in &before_fun.cfg.named {
        if label == head || label == &s.e {
            continue;
        }
        require(
            after_fun.cfg.named.get(label) == Some(before_block),
            format!("fuse_breaks changed unrelated block '{}'", label.as_str()),
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

    fn label(name: &str) -> Label {
        Label::from(name.to_string())
    }

    fn block(
        instructions: Vec<(LocalId, Instruction)>,
        terminator_id: usize,
        terminator: Terminator,
    ) -> Block {
        Block {
            instructions,
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

    fn bool_const(value: bool) -> Instruction {
        Instruction::BoolConstant { value }
    }

    /// entry -> h. h: c=%10; br %10 ? t : e.
    /// t: %11 = true -> j. e: %12 = %10 (select); br %12 ? t2 : f.
    /// t2: %13 = true -> j. f: %14 = false -> j.
    /// j: %15 = phi(t: %11, t2: %13, f: %14); return.
    fn cascade_program(t_value: bool, t2_value: bool) -> Program {
        let entry = block(vec![], 900, br("h"));
        let h = block(vec![(id(10), bool_const(true))], 901, br_if(10, "t", "e"));
        let t = block(vec![(id(11), bool_const(t_value))], 902, br("j"));
        let e = block(
            vec![(
                id(12),
                Instruction::Select { condition: id(10), if_true: id(10), if_false: id(10) },
            )],
            903,
            br_if(12, "t2", "f"),
        );
        let t2 = block(vec![(id(13), bool_const(t2_value))], 904, br("j"));
        let f = block(vec![(id(14), bool_const(false))], 905, br("j"));
        let j = block(
            vec![(
                id(15),
                Instruction::Phi {
                    branches: vec![
                        (label("t"), id(11)),
                        (label("t2"), id(13)),
                        (label("f"), id(14)),
                    ],
                },
            )],
            906,
            Terminator::Return { value: Some(id(15)) },
        );

        let mut named = crate::ir::new_label_map();
        named.insert(label("h"), h);
        named.insert(label("t"), t);
        named.insert(label("e"), e);
        named.insert(label("t2"), t2);
        named.insert(label("f"), f);
        named.insert(label("j"), j);
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
    fn fuses_two_break_arms() {
        let mut p = cascade_program(true, true);
        let before = p.clone();
        let changed = apply(&mut p, "f", "h").unwrap();
        assert_eq!(changed, 1 + 2);
        verify(&before, &p, "f", "h").unwrap();

        let fun = p.get("f").unwrap();
        assert!(fun.cfg.named.get(&label("e")).is_none());
        let h = &fun.cfg.named[&label("h")];
        assert_eq!(h.instructions.len(), 3);
        let (cc, minted) = &h.instructions[2];
        assert_eq!(
            *minted,
            Instruction::Select { condition: id(10), if_true: id(10), if_false: id(12) }
        );
        assert_eq!(h.terminator_kind(), &br_if(usize::from(*cc), "t2", "f"));
        // T keeps its phi entry; dce sweeps it later.
        let j = &fun.cfg.named[&label("j")];
        let Instruction::Phi { branches } = &j.instructions[0].1 else { panic!() };
        assert_eq!(branches.len(), 3);
    }

    #[test]
    fn refuses_differing_phi_constants() {
        let mut p = cascade_program(true, false);
        let error = apply(&mut p, "f", "h").unwrap_err().to_string();
        assert!(error.contains("must be the same bool constant"), "{}", error);
    }

    #[test]
    fn refuses_a_store_in_the_test_block() {
        let mut p = cascade_program(true, true);
        let fun = p.functions.values_mut().next().unwrap();
        let e = fun.cfg.named.get_mut(&label("e")).unwrap();
        e.instructions.insert(
            0,
            (id(30), Instruction::Store { target: id(10), source: id(10) }),
        );
        let error = apply(&mut p, "f", "h").unwrap_err().to_string();
        assert!(error.contains("not speculatable"), "{}", error);
    }

    #[test]
    fn refuses_a_computation_in_the_break_block() {
        let mut p = cascade_program(true, true);
        let fun = p.functions.values_mut().next().unwrap();
        let t = fun.cfg.named.get_mut(&label("t")).unwrap();
        t.instructions.push((
            id(31),
            Instruction::Select { condition: id(10), if_true: id(10), if_false: id(10) },
        ));
        let error = apply(&mut p, "f", "h").unwrap_err().to_string();
        assert!(error.contains("not a bool constant"), "{}", error);
    }

    #[test]
    fn refuses_a_second_predecessor_for_t2() {
        let mut p = cascade_program(true, true);
        let fun = p.functions.values_mut().next().unwrap();
        let f = fun.cfg.named.get_mut(&label("f")).unwrap();
        f.terminator.1 = br("t2");
        let error = apply(&mut p, "f", "h").unwrap_err().to_string();
        assert!(error.contains("exactly one predecessor"), "{}", error);
    }

    #[test]
    fn verify_rejects_a_swapped_branch() {
        let mut p = cascade_program(true, true);
        let before = p.clone();
        apply(&mut p, "f", "h").unwrap();
        let fun = p.functions.values_mut().next().unwrap();
        let h = fun.cfg.named.get_mut(&label("h")).unwrap();
        let Terminator::ConditionalBranch { condition, .. } = h.terminator.1.clone() else {
            panic!()
        };
        h.terminator.1 = Terminator::ConditionalBranch {
            condition,
            true_target: label("f"),
            false_target: label("t2"),
        };
        let error = verify(&before, &p, "f", "h").unwrap_err().to_string();
        assert!(error.contains("merged condition"), "{}", error);
    }

    #[test]
    fn fuses_a_chain_twice() {
        // h -> t/e, e -> t2/e2, e2 -> t3/f: two applications collapse it.
        let mut p = cascade_program(true, true);
        {
            let fun = p.functions.values_mut().next().unwrap();
            let e2 = block(
                vec![(
                    id(20),
                    Instruction::Select {
                        condition: id(10),
                        if_true: id(10),
                        if_false: id(10),
                    },
                )],
                907,
                br_if(20, "t3", "f"),
            );
            let t3 = block(vec![(id(21), bool_const(true))], 908, br("j"));
            fun.cfg.named.insert(label("e2"), e2);
            fun.cfg.named.insert(label("t3"), t3);
            let e = fun.cfg.named.get_mut(&label("e")).unwrap();
            e.terminator.1 = br_if(12, "t2", "e2");
            let j = fun.cfg.named.get_mut(&label("j")).unwrap();
            let Instruction::Phi { branches } = &mut j.instructions[0].1 else { panic!() };
            branches.insert(2, (label("t3"), id(21)));
        }
        let before = p.clone();
        apply(&mut p, "f", "h").unwrap();
        verify(&before, &p, "f", "h").unwrap();
        let before2 = p.clone();
        apply(&mut p, "f", "h").unwrap();
        verify(&before2, &p, "f", "h").unwrap();

        let fun = p.get("f").unwrap();
        assert!(fun.cfg.named.get(&label("e")).is_none());
        assert!(fun.cfg.named.get(&label("e2")).is_none());
        let h = &fun.cfg.named[&label("h")];
        let Terminator::ConditionalBranch { true_target, false_target, .. } =
            h.terminator_kind()
        else {
            panic!()
        };
        assert_eq!(true_target, &label("t3"));
        assert_eq!(false_target, &label("f"));
    }
}
