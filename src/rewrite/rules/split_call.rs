//! `split_call` - duplicate a dynamic call under a two-way case split on a
//! discriminant, so each copy can be pinned and inlined.
//!
//! # Why
//!
//! The last real call in the frame is the object-update dispatch,
//! `obj.type.update(obj)`. Its callee is per-state uniform but takes two
//! values over the search - `player_spawn.update` during the spawn
//! animation, `player.update` after - so a single `assert_closure` can
//! never hold and `inline` has nowhere to stand.
//!
//! # What it does
//!
//! Splits the block at the call and branches on whether the discriminant
//! equals a named global's value:
//!
//! ```text
//!   B:  ..before..                    B:  ..before..
//!       r = call f(args)                  g = get_global "player"
//!       ..after..            =>           gv = load g
//!                                         c = on == gv
//!                                         br c ? Bt : Bf
//!                                     Bt: rt = call f(args); br J
//!                                     Bf: rf = call f(args); br J
//!                                     J:  r = phi [Bt: rt, Bf: rf]
//!                                         ..after..
//! ```
//!
//! # Soundness
//!
//! This rule is semantically neutral on its own: both arms perform exactly
//! the original call, and the discriminating branch is pure (a global
//! read, a load, an equality). The equality compares scalars - a pointer
//! against a pointer - so the branch is uniform per state and never splits
//! the lane set; a mixed or unknown comparison would take both edges
//! exactly as the original single path did the call once, and the
//! differential screen would say so.
//!
//! The *premises* arrive afterwards and separately: an `inline` entry per
//! arm plants its own `assert_closure` (arm true: the callee is the named
//! global's update; arm false: it is the other one), each failing loudly
//! on its own if the two-valued claim is wrong. Keeping the split neutral
//! and the claims in the arms means a third callee appearing later kills
//! the run at the arm that received it, naming the closure it expected.

use anyhow::{anyhow, Result};

use crate::ir::{BinaryOp, FunDef, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::{blocks_sorted, get_block, require, LocalIdAllocator};

struct Site {
    /// Block holding the call (`None` = entry).
    block: Option<Label>,
    /// Instruction index of the call within the block.
    index: usize,
    call: Instruction,
}

fn site(fun: &FunDef, function: &str, at: LocalId, on: LocalId) -> Result<Site> {
    let mut found: Option<Site> = None;
    for key in blocks_sorted(&fun.cfg) {
        let block = get_block(&fun.cfg, &key).expect("listed block exists");
        for (index, (id, instr)) in block.instructions.iter().enumerate() {
            if *id != at {
                continue;
            }
            let Instruction::Call { .. } = instr else {
                return Err(anyhow!("%{} in {} is not a call", usize::from(at), function));
            };
            found = Some(Site { block: key.clone(), index, call: instr.clone() });
        }
    }
    let site = found
        .ok_or_else(|| anyhow!("%{} is not defined in {}", usize::from(at), function))?;

    // The discriminant must be usable at the split point. Its definition
    // dominating the *call* is exactly what `validate` will re-check once
    // the compare reads it there, so the only check needed here is that it
    // is not the call itself.
    require(on != at, "the discriminant cannot be the call's own result")?;
    Ok(site)
}

fn arm_labels(block: &Option<Label>, at: LocalId) -> (Label, Label, Label) {
    let base = match block {
        None => format!("__entry_devirt_{}", usize::from(at)),
        Some(l) => format!("{}_devirt_{}", l.as_str(), usize::from(at)),
    };
    (
        Label::from(format!("{}_true", base)),
        Label::from(format!("{}_false", base)),
        Label::from(format!("{}_join", base)),
    )
}

pub fn apply(
    program: &mut Program,
    function: &str,
    at: LocalId,
    on: LocalId,
    global: &str,
) -> Result<usize> {
    let fun = program.get(function)?;
    let s = site(fun, function, at, on)?;
    let (label_t, label_f, label_j) = arm_labels(&s.block, at);
    for label in [&label_t, &label_f, &label_j] {
        require(
            !fun.cfg.named.contains_key(label),
            format!("a block named '{}' already exists", label.as_str()),
        )?;
    }
    let mut allocator = LocalIdAllocator::for_function(fun);
    let g = allocator.fresh();
    let gv = allocator.fresh();
    let c = allocator.fresh();
    let br_b = allocator.fresh();
    let call_t = allocator.fresh();
    let br_t = allocator.fresh();
    let call_f = allocator.fresh();
    let br_f = allocator.fresh();

    let fun = program.get_mut(function)?;
    let block = match &s.block {
        None => &mut fun.cfg.entry,
        Some(l) => fun.cfg.named.get_mut(l).expect("site block exists"),
    };

    // Split: `after` moves to the join, along with the original terminator.
    let after: Vec<(LocalId, Instruction)> = block.instructions.split_off(s.index + 1);
    let (_, call) = block.instructions.pop().expect("the call is at the split point");
    let original_terminator = block.terminator.clone();
    block.instructions.push((
        g,
        Instruction::GetGlobal { name: global.to_string(), create_if_missing: false },
    ));
    block.instructions.push((gv, Instruction::Load { source: g }));
    block
        .instructions
        .push((c, Instruction::BinaryOp { left: on, op: BinaryOp::TwoEqual, right: gv }));
    block.terminator = (
        br_b,
        Terminator::ConditionalBranch {
            condition: c,
            true_target: label_t.clone(),
            false_target: label_f.clone(),
        },
    );

    let arm = |call_id: LocalId, br_id: LocalId| crate::ir::Block {
        instructions: vec![(call_id, call.clone())],
        terminator: (br_id, Terminator::UnconditionalBranch { target: label_j.clone() }),
        hint_normalize: false,
    };
    fun.cfg.named.insert(label_t.clone(), arm(call_t, br_t));
    fun.cfg.named.insert(label_f.clone(), arm(call_f, br_f));

    let mut join_instructions = vec![(
        at,
        Instruction::Phi {
            branches: vec![(label_t.clone(), call_t), (label_f.clone(), call_f)],
        },
    )];
    join_instructions.extend(after);
    fun.cfg.named.insert(
        label_j.clone(),
        crate::ir::Block {
            instructions: join_instructions,
            terminator: original_terminator,
            hint_normalize: false,
        },
    );

    // Phis in the original block's successors now see the join as their
    // predecessor.
    let successor_labels: Vec<Label> = fun.cfg.named[&label_j]
        .terminator_kind()
        .successor_labels()
        .into_iter()
        .cloned()
        .collect();
    let old_label = super::label_of(&s.block);
    for successor in successor_labels {
        if let Some(successor_block) = fun.cfg.named.get_mut(&successor) {
            for (_, instr) in successor_block.instructions.iter_mut() {
                if let Instruction::Phi { branches } = instr {
                    for (label, _) in branches.iter_mut() {
                        if *label == old_label {
                            *label = label_j.clone();
                        }
                    }
                }
            }
        }
    }

    fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(1)
}

/// Independent check: re-derives the site from the *before* program and
/// insists the after program is exactly the prescription - the block split
/// at the call, the pure discriminating branch, one identical call per arm,
/// the phi keeping the call's id, and not one other thing different.
pub fn verify(
    before: &Program,
    after: &Program,
    function: &str,
    at: LocalId,
    on: LocalId,
    global: &str,
) -> Result<()> {
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let s = site(before_fun, function, at, on)?;
    let (label_t, label_f, label_j) = arm_labels(&s.block, at);

    require(
        after_fun.cfg.named.len() == before_fun.cfg.named.len() + 3,
        "split_call must add exactly the two arms and the join",
    )?;
    let before_max = {
        let mut allocator = LocalIdAllocator::for_function(before_fun);
        allocator.fresh()
    };

    let before_block = get_block(&before_fun.cfg, &s.block).expect("site checked");
    let after_block = get_block(&after_fun.cfg, &s.block)
        .ok_or_else(|| anyhow!("split_call removed the split block"))?;

    // The head: before-instructions, then the discriminant triple.
    require(
        after_block.instructions.len() == s.index + 3,
        "the split block must end with exactly the discriminant triple",
    )?;
    require(
        after_block.instructions[..s.index] == before_block.instructions[..s.index]
            && after_block.hint_normalize == before_block.hint_normalize,
        "split_call changed the block before the call",
    )?;
    let (g, g_instr) = &after_block.instructions[s.index];
    let (gv, gv_instr) = &after_block.instructions[s.index + 1];
    let (c, c_instr) = &after_block.instructions[s.index + 2];
    require(
        *g_instr
            == Instruction::GetGlobal { name: global.to_string(), create_if_missing: false }
            && *gv_instr == Instruction::Load { source: *g }
            && *c_instr == Instruction::BinaryOp { left: on, op: BinaryOp::TwoEqual, right: *gv },
        "the discriminant must be `on == load(get_global(global))`",
    )?;
    let Terminator::ConditionalBranch { condition, true_target, false_target } =
        after_block.terminator_kind()
    else {
        return Err(anyhow!("the split block must end in the discriminating branch"));
    };
    require(
        condition == c && *true_target == label_t && *false_target == label_f,
        "the split block must branch on the discriminant into the two arms",
    )?;

    // The arms: one identical call each, nothing else.
    let mut arm_calls = Vec::new();
    for label in [&label_t, &label_f] {
        let arm = after_fun
            .cfg
            .named
            .get(label)
            .ok_or_else(|| anyhow!("no arm block '{}'", label.as_str()))?;
        require(
            arm.instructions.len() == 1
                && arm.instructions[0].1 == s.call
                && matches!(
                    arm.terminator_kind(),
                    Terminator::UnconditionalBranch { target } if *target == label_j
                )
                && !arm.hint_normalize,
            format!("arm '{}' must hold exactly the original call", label.as_str()),
        )?;
        arm_calls.push(arm.instructions[0].0);
    }

    // The join: the phi under the call's id, then the rest of the original
    // block, under its original terminator.
    let join = after_fun
        .cfg
        .named
        .get(&label_j)
        .ok_or_else(|| anyhow!("no join block '{}'", label_j.as_str()))?;
    require(
        join.instructions.first()
            == Some(&(
                at,
                Instruction::Phi {
                    branches: vec![
                        (label_t.clone(), arm_calls[0]),
                        (label_f.clone(), arm_calls[1]),
                    ],
                },
            )),
        "the join must open with the phi under the call's id",
    )?;
    require(
        join.instructions[1..] == before_block.instructions[s.index + 1..]
            && join.terminator == before_block.terminator
            && !join.hint_normalize,
        "the join must hold the rest of the original block",
    )?;
    let fresh = [*g, *gv, *c, after_block.terminator.0, arm_calls[0], arm_calls[1]];
    require(
        fresh.iter().all(|id| *id >= before_max)
            && fresh.iter().collect::<rustc_hash::FxHashSet<_>>().len() == fresh.len(),
        "the inserted ids must be fresh and distinct",
    )?;

    // Everything else: untouched, except phis in the original successors
    // renaming the split block to the join.
    let old_label = super::label_of(&s.block);
    let successors: Vec<&Label> = before_block.terminator_kind().successor_labels();
    for key in blocks_sorted(&before_fun.cfg) {
        if key == s.block {
            continue;
        }
        let before_other = get_block(&before_fun.cfg, &key).expect("listed block exists");
        let after_other = get_block(&after_fun.cfg, &key).ok_or_else(|| {
            anyhow!(
                "split_call removed block '{}'",
                key.as_ref().map(|l| l.as_str()).unwrap_or("__entry")
            )
        })?;
        let mut expected = before_other.clone();
        if matches!(&key, Some(l) if successors.contains(&l)) {
            for (_, instr) in expected.instructions.iter_mut() {
                if let Instruction::Phi { branches } = instr {
                    for (label, _) in branches.iter_mut() {
                        if *label == old_label {
                            *label = label_j.clone();
                        }
                    }
                }
            }
        }
        require(
            *after_other == expected,
            format!(
                "split_call changed block '{}', which is outside the site",
                key.as_ref().map(|l| l.as_str()).unwrap_or("__entry")
            ),
        )?;
    }
    require(
        before.functions.len() == after.functions.len(),
        "split_call changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!("split_call on {} also changed {}", function, name.as_str()),
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

    /// entry: t = obj.type cell; tv = load; u = t.update cell; f = load;
    ///        r = call f(obj); done = nil; br next
    /// next:  p = phi [entry: r]; return p
    fn dispatch_program() -> Program {
        let entry = Block {
            instructions: vec![
                (
                    id(3),
                    Instruction::GetField {
                        receiver: id(2),
                        field: "type".to_string(),
                        create_if_missing: false,
                    },
                ),
                (id(4), Instruction::Load { source: id(3) }),
                (
                    id(5),
                    Instruction::GetField {
                        receiver: id(4),
                        field: "update".to_string(),
                        create_if_missing: false,
                    },
                ),
                (id(6), Instruction::Load { source: id(5) }),
                (id(7), Instruction::Call { closure: id(6), args: vec![id(2)] }),
                (id(8), Instruction::NilConstant),
            ],
            terminator: (id(9), Terminator::UnconditionalBranch { target: label("next") }),
            hint_normalize: false,
        };
        let next = Block {
            instructions: vec![(
                id(10),
                Instruction::Phi { branches: vec![(super::super::entry_label(), id(7))] },
            )],
            terminator: (id(11), Terminator::Return { value: Some(id(10)) }),
            hint_normalize: false,
        };
        let mut named = crate::ir::new_label_map();
        named.insert(label("next"), next);
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![Some(id(2))],
            cfg: Cfg::new(entry, named),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions, merge_partition_cells: Vec::new() }
    }

    #[test]
    fn splits_the_dispatch() {
        let before = dispatch_program();
        let mut after = before.clone();
        apply(&mut after, "f", id(7), id(4), "player").unwrap();
        verify(&before, &after, "f", id(7), id(4), "player").unwrap();

        let fun = after.get("f").unwrap();
        // The entry ends with the discriminant and branches into the arms.
        let entry = &fun.cfg.entry;
        assert_eq!(entry.instructions.len(), 7);
        assert_eq!(
            entry.instructions[4].1,
            Instruction::GetGlobal { name: "player".to_string(), create_if_missing: false }
        );
        assert!(matches!(
            entry.terminator_kind(),
            Terminator::ConditionalBranch { .. }
        ));
        // Each arm holds the original call; the join phis them under %7 and
        // carries the rest of the block.
        let join = &fun.cfg.named[&label("__entry_devirt_7_join")];
        assert_eq!(join.instructions[0].0, id(7));
        assert!(matches!(join.instructions[0].1, Instruction::Phi { .. }));
        assert_eq!(join.instructions[1].1, Instruction::NilConstant);
        assert!(matches!(
            join.terminator_kind(),
            Terminator::UnconditionalBranch { target } if *target == label("next")
        ));
        // The downstream phi now names the join.
        let next = &fun.cfg.named[&label("next")];
        let Instruction::Phi { branches } = &next.instructions[0].1 else { panic!() };
        assert_eq!(branches[0].0, label("__entry_devirt_7_join"));
    }

    #[test]
    fn refuses_a_non_call() {
        let mut program = dispatch_program();
        let err = apply(&mut program, "f", id(8), id(4), "player").unwrap_err();
        assert!(err.to_string().contains("not a call"), "{}", err);
    }

    #[test]
    fn verify_rejects_a_changed_arm() {
        let before = dispatch_program();
        let mut after = before.clone();
        apply(&mut after, "f", id(7), id(4), "player").unwrap();
        {
            let fun = after.functions.values_mut().next().unwrap();
            let arm = fun.cfg.named.get_mut(&label("__entry_devirt_7_false")).unwrap();
            arm.instructions[0].1 = Instruction::Call { closure: id(6), args: vec![] };
        }
        let err = verify(&before, &after, "f", id(7), id(4), "player").unwrap_err();
        assert!(err.to_string().contains("original call"), "{}", err);
    }
}
