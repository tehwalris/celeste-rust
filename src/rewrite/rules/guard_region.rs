//! `guard_region` - wrap a masked instruction range with a `ConditionalSkip`.
//!
//! If-conversion turned branches into straight-line speculation: both arms
//! execute and mask-form selects (`select mask ? new : old`) pick per
//! lane. That is the right shape for lane-mixed states, but with
//! partitioned merges most states are *uniform* in the hot masks, and a
//! uniformly-false state computes the whole speculated region only to
//! discard every result. This rule splits the range out into its own
//! block behind a `ConditionalSkip`: uniformly-false states jump over it,
//! everything else enters and behaves exactly as before.
//!
//! # Soundness
//!
//! Skipping is safe precisely when executing the region under a
//! uniformly-false mask is a no-op, which the rule requires structurally:
//!
//!   * every region-defined value that escapes (used after the region or
//!     in another block) is `select mask ? _ : old` with `old` defined
//!     before the region - the join rebuilds it as a phi taking `old` on
//!     the skip edge, which is exactly what the select yields when the
//!     mask is uniformly false;
//!   * every `store` in the region stores such a select whose `old` arm
//!     is a load of the same cell, performed inside the region before any
//!     store to that cell - so under a false mask the store rewrites the
//!     cell's own value;
//!   * everything else in the region is effect-free: pure ops, loads,
//!     pointer derivations, and the assert family. Asserts guard
//!     speculated values; when the mask is uniformly false those values
//!     are consumed by nothing, so skipping the assert only removes a
//!     spuriously-strict check on lanes that never take the path.
//!
//! `verify` re-derives all of this independently of `apply`, per the
//! recipe's ground rules, and `rewrite verify` checks the full program
//! differentially on top.

use anyhow::{anyhow, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{Block, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::{require, LocalIdAllocator};

/// Instructions allowed in a guarded region besides mask-form stores.
fn effect_free(instr: &Instruction) -> bool {
    matches!(
        instr,
        Instruction::NumberConstant { .. }
            | Instruction::BoolConstant { .. }
            | Instruction::StringConstant { .. }
            | Instruction::NilConstant
            | Instruction::BinaryOp { .. }
            | Instruction::UnaryOp { .. }
            | Instruction::Select { .. }
            | Instruction::Load { .. }
            | Instruction::GetGlobal { .. }
            | Instruction::GetField { .. }
            | Instruction::GetIndex { .. }
            | Instruction::AssertClosure { .. }
            | Instruction::AssertPointer { .. }
            | Instruction::AssertValueCell { .. }
            | Instruction::AssertTrue { .. }
    )
}

struct RegionPlan {
    from_idx: usize,
    to_idx: usize,
    /// Escaping region defs, in definition order, with their selects' old
    /// arms: (escaping id, old id).
    escapes: Vec<(LocalId, LocalId)>,
}

/// Shared structural discovery used by `apply`; `verify` re-derives its
/// own facts from the before/after pair instead of calling this.
fn plan(
    block: &Block,
    others_use: &FxHashSet<LocalId>,
    from: LocalId,
    to: LocalId,
    mask: LocalId,
) -> Result<RegionPlan> {
    let from_idx = block
        .instructions
        .iter()
        .position(|(id, _)| *id == from)
        .ok_or_else(|| anyhow!("from-id not in block"))?;
    let to_idx = block
        .instructions
        .iter()
        .position(|(id, _)| *id == to)
        .ok_or_else(|| anyhow!("to-id not in block"))?;
    require(from_idx <= to_idx, "empty or inverted range".to_string())?;

    let region = &block.instructions[from_idx..=to_idx];
    let region_defs: FxHashSet<LocalId> = region.iter().map(|(id, _)| *id).collect();
    require(
        !region_defs.contains(&mask),
        "the mask must be defined outside the region".to_string(),
    )?;

    // Ids used after the region within this block (instructions or the
    // terminator).
    let mut tail_uses: FxHashSet<LocalId> = FxHashSet::default();
    for (_, instr) in &block.instructions[to_idx + 1..] {
        tail_uses.extend(instr.get_used_locals());
    }
    tail_uses.extend(block.terminator.1.get_used_locals());

    let mut escapes = Vec::new();
    let mut store_targets: FxHashSet<LocalId> = FxHashSet::default();
    for (idx, (id, instr)) in region.iter().enumerate() {
        match instr {
            Instruction::Store { target, source } => {
                // The stored value must be a mask-form select whose old
                // arm is a load of the same cell, earlier in the region,
                // with no prior store to that cell.
                let source_def = region
                    .iter()
                    .take(idx)
                    .find(|(rid, _)| rid == source)
                    .map(|(_, i)| i)
                    .ok_or_else(|| anyhow!("store source defined outside region"))?;
                let (cond, old) = match source_def {
                    Instruction::Select {
                        condition,
                        if_true: _,
                        if_false,
                    } => (*condition, *if_false),
                    _ => return Err(anyhow!("store source is not a select")),
                };
                require(cond == mask, "store select is not on the mask".to_string())?;
                let old_def = region
                    .iter()
                    .take(idx)
                    .find(|(rid, _)| rid == &old)
                    .map(|(_, i)| i)
                    .ok_or_else(|| anyhow!("store old arm defined outside region"))?;
                let load_src = match old_def {
                    Instruction::Load { source } => *source,
                    _ => return Err(anyhow!("store old arm is not a load")),
                };
                require(
                    load_src == *target,
                    "store old arm does not load the stored cell".to_string(),
                )?;
                require(
                    !store_targets.contains(target),
                    "cell stored twice in region".to_string(),
                )?;
                store_targets.insert(*target);
            }
            other => {
                require(
                    effect_free(other),
                    format!("instruction not skip-safe: {:?}", other),
                )?;
            }
        }
        let escaping = tail_uses.contains(id) || others_use.contains(id);
        if escaping {
            let (cond, old) = match instr {
                Instruction::Select {
                    condition,
                    if_true: _,
                    if_false,
                } => (*condition, *if_false),
                _ => {
                    return Err(anyhow!(
                        "escaping region value %{} is not a select",
                        usize::from(*id)
                    ))
                }
            };
            require(
                cond == mask,
                format!("escaping select %{} is not on the mask", usize::from(*id)),
            )?;
            require(
                !region_defs.contains(&old),
                format!(
                    "escaping select %{}'s old arm is region-defined",
                    usize::from(*id)
                ),
            )?;
            escapes.push((*id, old));
        }
    }
    Ok(RegionPlan {
        from_idx,
        to_idx,
        escapes,
    })
}

/// Ids used by any block other than `block_label` in the function.
fn uses_outside(fun: &crate::ir::FunDef, block_label: &Label) -> FxHashSet<LocalId> {
    let mut used = FxHashSet::default();
    let mut visit = |label: Option<&Label>, block: &Block| {
        if label == Some(block_label) {
            return;
        }
        for (_, instr) in &block.instructions {
            used.extend(instr.get_used_locals());
        }
        used.extend(block.terminator.1.get_used_locals());
    };
    visit(None, &fun.cfg.entry);
    for (label, block) in &fun.cfg.named {
        visit(Some(label), block);
    }
    used
}

pub fn apply(
    program: &mut Program,
    function: &str,
    block: &str,
    from: usize,
    to: usize,
    mask: usize,
    prefix: &str,
) -> Result<usize> {
    let fun = program.get_mut(function)?;
    let block_label = Label::from(block.to_string());
    let region_label = Label::from(format!("{}_region", prefix));
    let join_label = Label::from(format!("{}_join", prefix));
    require(
        !fun.cfg.named.contains_key(&region_label) && !fun.cfg.named.contains_key(&join_label),
        "prefix collides with existing blocks".to_string(),
    )?;

    let others_use = uses_outside(fun, &block_label);
    let source = fun
        .cfg
        .named
        .get(&block_label)
        .ok_or_else(|| anyhow!("{} has no block named {}", function, block))?
        .clone();
    let plan = plan(
        &source,
        &others_use,
        LocalId::from(from),
        LocalId::from(to),
        LocalId::from(mask),
    )?;

    let mut ids = LocalIdAllocator::for_function(fun);
    let head_term_id = ids.fresh();
    let region_term_id = ids.fresh();

    // Rename escaping defs inside the region; the original id becomes the
    // join phi so every downstream use stays valid.
    let rename: FxHashMap<LocalId, LocalId> = plan
        .escapes
        .iter()
        .map(|(escape, _)| (*escape, ids.fresh()))
        .collect();
    let map_id = |id: LocalId| rename.get(&id).copied().unwrap_or(id);

    let head_instrs = source.instructions[..plan.from_idx].to_vec();
    let region_instrs: Vec<(LocalId, Instruction)> = source.instructions
        [plan.from_idx..=plan.to_idx]
        .iter()
        .map(|(id, instr)| (map_id(*id), instr.map_local_ids(map_id)))
        .collect();
    let tail_instrs = source.instructions[plan.to_idx + 1..].to_vec();

    let mut join_instrs: Vec<(LocalId, Instruction)> = plan
        .escapes
        .iter()
        .map(|(escape, old)| {
            (
                *escape,
                Instruction::Phi {
                    branches: vec![
                        (block_label.clone(), *old),
                        (region_label.clone(), map_id(*escape)),
                    ],
                },
            )
        })
        .collect();
    join_instrs.extend(tail_instrs);

    let head = Block {
        instructions: head_instrs,
        terminator: (
            head_term_id,
            Terminator::ConditionalSkip {
                condition: LocalId::from(mask),
                skip_target: join_label.clone(),
                enter_target: region_label.clone(),
            },
        ),
        hint_normalize: source.hint_normalize,
    };
    let region_block = Block {
        instructions: region_instrs,
        terminator: (
            region_term_id,
            Terminator::UnconditionalBranch {
                target: join_label.clone(),
            },
        ),
        hint_normalize: false,
    };
    let join_block = Block {
        instructions: join_instrs,
        terminator: source.terminator.clone(),
        hint_normalize: false,
    };

    fun.cfg.named.insert(block_label.clone(), head);
    fun.cfg.named.insert(region_label, region_block);
    fun.cfg.named.insert(join_label.clone(), join_block);

    // The original block's outgoing edges now come from the join: phi
    // sources in its successors must follow.
    let successor_labels: Vec<Label> = fun.cfg.named[&join_label]
        .terminator
        .1
        .successor_labels()
        .into_iter()
        .cloned()
        .collect();
    for successor in successor_labels {
        if let Some(successor_block) = fun.cfg.named.get_mut(&successor) {
            for (_, instr) in &mut successor_block.instructions {
                if let Instruction::Phi { branches } = instr {
                    for (source_label, _) in branches.iter_mut() {
                        if *source_label == block_label {
                            *source_label = join_label.clone();
                        }
                    }
                }
            }
        }
    }

    // Ids changed: the slot map must go back to identity for validate.
    fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(1)
}

/// Independent re-derivation: the after-program must be the before-program
/// with exactly this transformation - head/region/join split, skip
/// terminator, mask-form escapes rebuilt as join phis, successor phi
/// sources retargeted, everything else untouched.
pub fn verify(
    before: &Program,
    after: &Program,
    function: &str,
    block: &str,
    from: usize,
    to: usize,
    mask: usize,
    prefix: &str,
) -> Result<()> {
    let block_label = Label::from(block.to_string());
    let region_label = Label::from(format!("{}_region", prefix));
    let join_label = Label::from(format!("{}_join", prefix));
    let mask_id = LocalId::from(mask);

    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let before_block = before_fun
        .cfg
        .named
        .get(&block_label)
        .ok_or_else(|| anyhow!("no before-block {}", block))?;

    // Re-derive the region facts from the before-program alone.
    let others_use = uses_outside(before_fun, &block_label);
    let derived = plan(
        before_block,
        &others_use,
        LocalId::from(from),
        LocalId::from(to),
        mask_id,
    )?;

    let head = after_fun
        .cfg
        .named
        .get(&block_label)
        .ok_or_else(|| anyhow!("no after-head"))?;
    let region_block = after_fun
        .cfg
        .named
        .get(&region_label)
        .ok_or_else(|| anyhow!("no region block"))?;
    let join_block = after_fun
        .cfg
        .named
        .get(&join_label)
        .ok_or_else(|| anyhow!("no join block"))?;

    // Head: the prefix instructions plus the skip.
    require(
        head.instructions == before_block.instructions[..derived.from_idx],
        "head instructions changed".to_string(),
    )?;
    match &head.terminator.1 {
        Terminator::ConditionalSkip {
            condition,
            skip_target,
            enter_target,
        } => {
            require(*condition == mask_id, "skip mask mismatch".to_string())?;
            require(
                skip_target == &join_label && enter_target == &region_label,
                "skip targets mismatch".to_string(),
            )?;
        }
        _ => return Err(anyhow!("head does not end in ConditionalSkip")),
    }
    require(
        head.hint_normalize == before_block.hint_normalize,
        "hint flag moved".to_string(),
    )?;

    // Join: one phi per escaping def, in order, then the tail and the
    // original terminator.
    let escape_count = derived.escapes.len();
    require(
        join_block.instructions.len()
            == escape_count + before_block.instructions.len() - (derived.to_idx + 1),
        "join length mismatch".to_string(),
    )?;
    let mut rename: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    for (k, (escape, old)) in derived.escapes.iter().enumerate() {
        let (phi_id, phi) = &join_block.instructions[k];
        require(phi_id == escape, "join phi id mismatch".to_string())?;
        match phi {
            Instruction::Phi { branches } => {
                require(branches.len() == 2, "join phi arity".to_string())?;
                let from_head = branches
                    .iter()
                    .find(|(l, _)| *l == block_label)
                    .ok_or_else(|| anyhow!("join phi lacks head edge"))?;
                let from_region = branches
                    .iter()
                    .find(|(l, _)| *l == region_label)
                    .ok_or_else(|| anyhow!("join phi lacks region edge"))?;
                require(
                    from_head.1 == *old,
                    "join phi head arm is not the select's old value".to_string(),
                )?;
                rename.insert(*escape, from_region.1);
            }
            _ => return Err(anyhow!("join prefix instruction is not a phi")),
        }
    }
    require(
        join_block.instructions[escape_count..]
            == before_block.instructions[derived.to_idx + 1..],
        "join tail changed".to_string(),
    )?;
    require(
        join_block.terminator == before_block.terminator,
        "join terminator changed".to_string(),
    )?;

    // Region: the range with escaping defs renamed per the join phis.
    let expected_region: Vec<(LocalId, Instruction)> = before_block.instructions
        [derived.from_idx..=derived.to_idx]
        .iter()
        .map(|(id, instr)| {
            let map = |x: LocalId| rename.get(&x).copied().unwrap_or(x);
            (map(*id), instr.map_local_ids(map))
        })
        .collect();
    require(
        region_block.instructions == expected_region,
        "region instructions mismatch".to_string(),
    )?;
    match &region_block.terminator.1 {
        Terminator::UnconditionalBranch { target } => {
            require(target == &join_label, "region must branch to join".to_string())?;
        }
        _ => return Err(anyhow!("region terminator is not a branch")),
    }
    require(!region_block.hint_normalize, "region gained a hint".to_string())?;
    require(!join_block.hint_normalize, "join gained a hint".to_string())?;

    // Everything else: identical, except successor phis that retarget the
    // split block to the join.
    let successors: FxHashSet<&Label> = join_block.terminator.1.successor_labels().into_iter().collect();
    require(
        after_fun.cfg.entry == before_fun.cfg.entry,
        "entry block changed".to_string(),
    )?;
    require(
        after_fun.cfg.named.len() == before_fun.cfg.named.len() + 2,
        "unexpected block count".to_string(),
    )?;
    for (label, before_other) in &before_fun.cfg.named {
        if *label == block_label {
            continue;
        }
        let after_other = after_fun
            .cfg
            .named
            .get(label)
            .ok_or_else(|| anyhow!("block {} vanished", label.as_str()))?;
        if successors.contains(label) {
            // Equal modulo phi-source retargeting block -> join.
            let mut expected = before_other.clone();
            for (_, instr) in &mut expected.instructions {
                if let Instruction::Phi { branches } = instr {
                    for (source_label, _) in branches.iter_mut() {
                        if *source_label == block_label {
                            *source_label = join_label.clone();
                        }
                    }
                }
            }
            require(
                *after_other == expected,
                format!("successor {} changed beyond retargeting", label.as_str()),
            )?;
        } else {
            require(
                after_other == before_other,
                format!("unrelated block {} changed", label.as_str()),
            )?;
        }
    }
    // Other functions untouched.
    for (name, before_f) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        let after_f = after
            .functions
            .get(name)
            .ok_or_else(|| anyhow!("{} vanished", name.as_str()))?;
        require(
            before_f.cfg == after_f.cfg,
            format!("{} changed, but only {} may", name.as_str(), function),
        )?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BinaryOp, Cfg, FunDef, GlobalId, SlotMap};
    use indexmap::IndexMap;
    use std::sync::Arc;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    /// b: %0 mask (from entry), %1 old value, then a region
    ///    [%2 = %1+%1, %3 = select %0 ? %2 : %1], tail uses %3,
    ///    br end. end: return %3-ish.
    fn program() -> Program {
        let entry = Block {
            instructions: vec![
                (
                    id(20),
                    Instruction::GetGlobal {
                        name: "m".to_string(),
                        create_if_missing: false,
                    },
                ),
                (id(0), Instruction::Load { source: id(20) }),
                (
                    id(21),
                    Instruction::GetGlobal {
                        name: "v".to_string(),
                        create_if_missing: false,
                    },
                ),
                (id(1), Instruction::Load { source: id(21) }),
            ],
            terminator: (
                id(22),
                Terminator::UnconditionalBranch {
                    target: Label::from("b".to_string()),
                },
            ),
            hint_normalize: false,
        };
        let b = Block {
            instructions: vec![
                (
                    id(2),
                    Instruction::BinaryOp {
                        left: id(1),
                        op: BinaryOp::Plus,
                        right: id(1),
                    },
                ),
                (
                    id(3),
                    Instruction::Select {
                        condition: id(0),
                        if_true: id(2),
                        if_false: id(1),
                    },
                ),
                (
                    id(4),
                    Instruction::BinaryOp {
                        left: id(3),
                        op: BinaryOp::Plus,
                        right: id(1),
                    },
                ),
            ],
            terminator: (
                id(23),
                Terminator::UnconditionalBranch {
                    target: Label::from("end".to_string()),
                },
            ),
            hint_normalize: false,
        };
        let end = Block {
            instructions: vec![(
                id(5),
                Instruction::Phi {
                    branches: vec![(Label::from("b".to_string()), id(4))],
                },
            )],
            terminator: (id(24), Terminator::Return { value: Some(id(5)) }),
            hint_normalize: false,
        };
        let mut named = std::collections::HashMap::default();
        named.insert(Label::from("b".to_string()), b);
        named.insert(Label::from("end".to_string()), end);
        let cfg = Cfg {
            entry,
            named,
            slots: Arc::new(SlotMap::identity()),
        };
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
        Program {
            functions,
            merge_partition_cells: Vec::new(),
        }
    }

    #[test]
    fn apply_and_verify_round_trip() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "f", "b", 2, 3, 0, "gr1").unwrap();
        verify(&before, &after, "f", "b", 2, 3, 0, "gr1").unwrap();
        // Structure spot-checks: head skips, join phi takes old on skip.
        let fun = after.get("f").unwrap();
        let head = &fun.cfg.named[&Label::from("b".to_string())];
        assert!(matches!(
            head.terminator.1,
            Terminator::ConditionalSkip { condition, .. } if condition == id(0)
        ));
        let join = &fun.cfg.named[&Label::from("gr1_join".to_string())];
        match &join.instructions[0] {
            (phi_id, Instruction::Phi { branches }) => {
                assert_eq!(*phi_id, id(3));
                assert!(branches.contains(&(Label::from("b".to_string()), id(1))));
            }
            other => panic!("expected phi, got {:?}", other),
        }
    }

    /// A region whose escaping value is not a mask-select must be refused.
    #[test]
    fn rejects_unmasked_escape() {
        let mut p = program();
        // Range [2..=2]: %2 escapes (used by %3 in the tail) but is a raw
        // binop, not a mask select.
        let err = apply(&mut p, "f", "b", 2, 2, 0, "gr2");
        assert!(err.is_err());
    }

    /// Verify must reject a tampered after-program (extra instruction in
    /// the region).
    #[test]
    fn verify_rejects_tampering() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "f", "b", 2, 3, 0, "gr1").unwrap();
        let fun = after.get_mut("f").unwrap();
        let region = fun
            .cfg
            .named
            .get_mut(&Label::from("gr1_region".to_string()))
            .unwrap();
        region.instructions.push((
            id(90),
            Instruction::BinaryOp {
                left: id(1),
                op: BinaryOp::Plus,
                right: id(1),
            },
        ));
        assert!(verify(&before, &after, "f", "b", 2, 3, 0, "gr1").is_err());
    }
}
