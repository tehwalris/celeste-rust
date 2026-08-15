//! Where each `LocalId` physically lives at run time.
//!
//! A `SlotMap` is not part of the program's meaning - it is an implementation
//! choice about storage - so it gets its own module rather than living with the
//! rules that change instructions. Two things happen here:
//!
//!   * `constraints` - the sets of ids that must not share a slot
//!   * `allocate` / `check` - producing a map, and deciding whether one is valid
//!
//! # Why this is worth doing
//!
//! `LocalEnv` is a flat array indexed by slot, and `filter_by_mask` clones it
//! every time a lane-varying branch splits the state set - which is 33% of
//! runtime. Under the identity map the array is `max LocalId + 1` long:
//! `player.update_21` costs 848 entries and `__frame` costs 1508, of which at
//! most 12-18 are ever live. Inlining makes this dramatically worse (3206 ids),
//! which is why `inline` was parked until this existed.
//!
//! # What "valid" means
//!
//! Two ids may share a slot exactly when they are never simultaneously live.
//! `constraints` turns that into explicit sets, `allocate` colours them, and
//! `check` re-tests the result. Note that `allocate` and `check` share this
//! derivation rather than deriving it twice - a second, subtly different copy of
//! the interference rules would be more likely to hide a bug than to catch one.
//! The independent backstop is at run time: `LocalEnv` records which id occupies
//! each slot and panics on a mismatched read, so a wrong allocation fails loudly
//! instead of silently computing the wrong number.

use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{FunDef, Instruction, LocalId, SlotMap};

use super::liveness;
use super::print::local_name;
use super::validate::{all_blocks, block_label, compute_reachable};

/// Every `LocalId` the interpreter may write into the environment for this
/// function: arguments, captures, instruction results and terminator ids.
///
/// Terminator ids are included because `validate` treats them as definitions
/// and the map has to cover everything an id could be looked up by, even though
/// nothing writes them today.
pub fn defined_ids(fun: &FunDef) -> Vec<LocalId> {
    let mut ids: Vec<LocalId> = fun
        .arg_ids
        .iter()
        .flatten()
        .chain(fun.capture_ids.iter())
        .copied()
        .collect();
    for block in fun.cfg.iter_blocks() {
        ids.extend(block.instructions.iter().map(|(id, _)| *id));
        ids.push(block.terminator_id());
    }
    ids.sort_unstable();
    ids.dedup();
    ids
}

/// Sets of ids that must all land in different slots.
///
/// Three sources:
///
/// 1. **Simultaneous liveness.** At every program point, everything live there
///    plus anything written at that point. Walking each block backwards from
///    its live-out set reconstructs those sets exactly.
///
/// 2. **The function's entry.** Arguments and captures are all written before
///    the body runs, so they conflict with each other and with whatever is live
///    on entry - including arguments nothing reads, which still get written.
///
/// 3. **Phi sequentialisation.** `flow_block_phi` assigns a block's phis one at
///    a time, in order, reading each operand from the environment as it goes.
///    That is only correct if no phi's destination slot is some *other* phi's
///    operand slot. Note this deliberately still allows a phi to share a slot
///    with its own operand, which is the coalescing that makes phis free.
pub fn constraints(fun: &FunDef) -> Vec<Vec<LocalId>> {
    let cfg = &fun.cfg;
    let live = liveness::analyze(fun);
    let blocks = all_blocks(cfg);
    let reachable = compute_reachable(&blocks);
    let mut out: Vec<Vec<LocalId>> = Vec::new();

    fn push(out: &mut Vec<Vec<LocalId>>, set: &FxHashSet<LocalId>, extra: Option<LocalId>) {
        let mut v: Vec<LocalId> = set.iter().copied().collect();
        if let Some(id) = extra {
            if !set.contains(&id) {
                v.push(id);
            }
        }
        if v.len() > 1 {
            v.sort_unstable();
            out.push(v);
        }
    }

    for (key, block) in &blocks {
        if !reachable.contains(key) {
            // Nothing here ever executes, so nothing here conflicts. The ids
            // still need slots, which `allocate` gives them.
            continue;
        }
        let Some(live_out) = live.live_out.get(key) else { continue };

        // (1) Backwards through the block, one program point at a time.
        let mut set = live_out.clone();
        push(&mut out, &set, Some(block.terminator_id()));
        set.extend(block.terminator_kind().get_used_locals());
        push(&mut out, &set, None);
        for (id, instr) in block.instructions.iter().rev() {
            push(&mut out, &set, Some(*id));
            set.remove(id);
            // A phi consumes its operand on the incoming edge, so the operand
            // is live in the predecessor, not here.
            if !matches!(instr, Instruction::Phi { .. }) {
                set.extend(instr.get_used_locals());
            }
            push(&mut out, &set, None);
        }

        // (3) Phi destinations against other phis' operands.
        let phis: Vec<(LocalId, Vec<LocalId>)> = block
            .instructions
            .iter()
            .filter_map(|(id, instr)| match instr {
                Instruction::Phi { branches } => {
                    Some((*id, branches.iter().map(|(_, v)| *v).collect()))
                }
                _ => None,
            })
            .collect();
        for (i, (dest, _)) in phis.iter().enumerate() {
            for (j, (_, operands)) in phis.iter().enumerate() {
                if i == j {
                    continue;
                }
                for operand in operands {
                    if operand != dest {
                        out.push({
                            let mut v = vec![*dest, *operand];
                            v.sort_unstable();
                            v
                        });
                    }
                }
            }
        }
    }

    // (2) The entry point.
    let mut entry: FxHashSet<LocalId> = live
        .live_in
        .get(&None)
        .cloned()
        .unwrap_or_default();
    entry.extend(fun.arg_ids.iter().flatten().copied());
    entry.extend(fun.capture_ids.iter().copied());
    push(&mut out, &entry, None);

    out
}

/// A local's identity for ordering purposes, independent of its `LocalId`.
///
/// Rewrite-created locals have a stable name (`{entry}.{block}.{position}`);
/// source locals deliberately have none, because their ids are already
/// per-function and stable, so their id IS the stable key. Zero-padded so
/// the string order matches the numeric one.
///
/// Named and unnamed locals therefore sort into two groups. That is fine -
/// all that is required is that the key does not move when the numbering
/// does.
fn stable_key(fun: &FunDef, id: LocalId) -> String {
    match fun.cfg.names.get(id) {
        Some(name) => format!("n:{}", name),
        None => format!("i:{:08}", usize::from(id)),
    }
}

/// Greedy colouring of the conflict graph.
///
/// Deliberately unsophisticated. Max simultaneous liveness is 12-18, so the gap
/// between this and an optimal allocation is at most a couple of slots against
/// a baseline of hundreds, and a simple allocator is one fewer thing to be
/// wrong. Ordering is by descending conflict count, ties broken by
/// `stable_key`, so the result is deterministic AND survives a renumbering.
///
/// The tiebreak used to be the raw `LocalId`, which made the whole slot
/// assignment a function of the id numbering - and row keys are computed
/// from slots, so every checkpoint and every certified `g` silently depended
/// on it. Any change to how ids are handed out permuted the layout of states
/// that mean exactly the same thing. Keying the tiebreak on the local's
/// STABLE NAME instead makes the layout a function of the program, which is
/// what it should have been: see plans/recipe-stability-plan.md.
pub fn allocate(fun: &FunDef) -> SlotMap {
    let ids = defined_ids(fun);
    if ids.is_empty() {
        return SlotMap::identity();
    }

    let mut conflicts: FxHashMap<LocalId, FxHashSet<LocalId>> = FxHashMap::default();
    for id in &ids {
        conflicts.entry(*id).or_default();
    }
    for set in constraints(fun) {
        for a in &set {
            for b in &set {
                if a != b {
                    conflicts.entry(*a).or_default().insert(*b);
                }
            }
        }
    }

    // Keys built once rather than inside the comparator: `sort_by_key` calls
    // it O(n log n) times, and each call formats a string.
    let mut order: Vec<(usize, String, LocalId)> = ids
        .iter()
        .map(|id| {
            (
                usize::MAX - conflicts.get(id).map_or(0, |c| c.len()),
                stable_key(fun, *id),
                *id,
            )
        })
        .collect();
    order.sort();
    let order: Vec<LocalId> = order.into_iter().map(|(_, _, id)| id).collect();

    let mut slot_of: FxHashMap<LocalId, u32> = FxHashMap::default();
    for id in order {
        let taken: FxHashSet<u32> = conflicts
            .get(&id)
            .map(|neighbours| {
                neighbours.iter().filter_map(|n| slot_of.get(n).copied()).collect()
            })
            .unwrap_or_default();
        let mut slot = 0u32;
        while taken.contains(&slot) {
            slot += 1;
        }
        slot_of.insert(id, slot);
    }

    let highest = ids.iter().map(|id| usize::from(*id)).max().unwrap_or(0);
    let mut of_local = vec![u32::MAX; highest + 1];
    for (id, slot) in slot_of {
        of_local[usize::from(id)] = slot;
    }
    SlotMap::from_vec(of_local)
}

/// Is this function's slot map a valid place to put its values?
///
/// Run by `validate` after *every* rewrite, not just after allocation, because
/// the other way to get an invalid map is to leave a stale one behind: a rule
/// that introduces ids under a map that predates them. Coverage catches that.
pub fn check(fun: &FunDef) -> Vec<String> {
    let map = &fun.cfg.slots;
    let mut errors = Vec::new();

    for id in defined_ids(fun) {
        if map.try_slot_of(id).is_none() {
            errors.push(format!(
                "{} has no slot - the slot map is stale with respect to the CFG",
                local_name(id)
            ));
        }
    }
    if !errors.is_empty() {
        return errors;
    }

    for set in constraints(fun) {
        let mut by_slot: FxHashMap<usize, LocalId> = FxHashMap::default();
        for id in set {
            let slot = map.slot_of(id);
            if let Some(other) = by_slot.insert(slot, id) {
                errors.push(format!(
                    "{} and {} are in slot {} at the same time",
                    local_name(other.min(id)),
                    local_name(other.max(id)),
                    slot
                ));
            }
        }
    }
    errors.sort();
    errors.dedup();
    errors.truncate(10);
    errors
}

/// How many slots each function uses, for reporting.
pub fn slot_count(fun: &FunDef) -> usize {
    let map = &fun.cfg.slots;
    if map.is_identity() {
        defined_ids(fun)
            .last()
            .map_or(0, |id| usize::from(*id) + 1)
    } else {
        map.num_slots()
    }
}

/// Which block each id is defined in, for error messages elsewhere.
pub fn describe_location(fun: &FunDef, id: LocalId) -> String {
    for (key, block) in all_blocks(&fun.cfg) {
        if block.instructions.iter().any(|(i, _)| *i == id) || block.terminator_id() == id {
            return block_label(&key);
        }
    }
    "?".to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, GlobalId, Label, Terminator};

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn num(n: i16) -> Instruction {
        Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(n) }
    }

    fn fun_with(cfg: Cfg) -> FunDef {
        FunDef {
            name: GlobalId::from("t".to_string()),
            capture_ids: vec![],
            arg_ids: vec![],
            cfg,
            source_span: None,
        }
    }

    /// Three values, never more than two live at once, so two slots suffice -
    /// and the terminator id can reuse one of them.
    #[test]
    fn packs_disjoint_live_ranges_into_one_slot() {
        let cfg = Cfg::new(
            Block {
                instructions: vec![
                    (id(0), num(1)),
                    (id(1), num(2)),
                    (
                        id(2),
                        Instruction::BinaryOp {
                            left: id(0),
                            op: crate::ir::BinaryOp::Plus,
                            right: id(1),
                        },
                    ),
                ],
                terminator: (id(3), Terminator::Return { value: Some(id(2)) }),
                hint_normalize: false,
            },
            crate::ir::new_label_map(),
        );
        let mut fun = fun_with(cfg);
        let map = allocate(&fun);
        assert_eq!(map.num_slots(), 2, "expected two slots, got {:?}", map);
        assert_ne!(map.slot_of(id(0)), map.slot_of(id(1)));

        fun.cfg.slots = std::sync::Arc::new(map);
        assert!(check(&fun).is_empty(), "{:?}", check(&fun));
    }

    /// The check has to reject an allocation that puts two values live at the
    /// same moment in one slot.
    #[test]
    fn check_rejects_overlapping_live_ranges() {
        let cfg = Cfg::new(
            Block {
                instructions: vec![
                    (id(0), num(1)),
                    (id(1), num(2)),
                    (
                        id(2),
                        Instruction::BinaryOp {
                            left: id(0),
                            op: crate::ir::BinaryOp::Plus,
                            right: id(1),
                        },
                    ),
                ],
                terminator: (id(3), Terminator::Return { value: Some(id(2)) }),
                hint_normalize: false,
            },
            crate::ir::new_label_map(),
        );
        let mut fun = fun_with(cfg);
        // %0 and %1 are both live when %2 is computed.
        fun.cfg.slots = std::sync::Arc::new(SlotMap::from_vec(vec![0, 0, 1, 1]));
        let errors = check(&fun);
        assert!(
            errors.iter().any(|e| e.contains("%0 and %1")),
            "{:?}",
            errors
        );
    }

    /// A map that does not mention an id at all is stale, not merely bad.
    #[test]
    fn check_rejects_a_stale_map() {
        let cfg = Cfg::new(
            Block {
                instructions: vec![(id(0), num(1))],
                terminator: (id(1), Terminator::Return { value: Some(id(0)) }),
                hint_normalize: false,
            },
            crate::ir::new_label_map(),
        );
        let mut fun = fun_with(cfg);
        fun.cfg.slots = std::sync::Arc::new(SlotMap::from_vec(vec![0]));
        let errors = check(&fun);
        assert!(errors.iter().any(|e| e.contains("%1")), "{:?}", errors);
    }

    /// Phis in a loop header. `%2` must not be given the slot that `%3`'s
    /// operand `%0` lives in, because `flow_block_phi` writes `%2` before it
    /// reads `%0` for `%3`.
    #[test]
    fn phi_destination_never_clobbers_a_later_phis_operand() {
        let head = Label::from("head".to_string());
        let mut named = crate::ir::new_label_map();
        named.insert(
            head.clone(),
            Block {
                instructions: vec![
                    (
                        id(2),
                        Instruction::Phi {
                            branches: vec![
                                (Label::from("__entry".to_string()), id(0)),
                                (head.clone(), id(3)),
                            ],
                        },
                    ),
                    (
                        id(3),
                        Instruction::Phi {
                            branches: vec![
                                (Label::from("__entry".to_string()), id(1)),
                                (head.clone(), id(0)),
                            ],
                        },
                    ),
                ],
                terminator: (
                    id(4),
                    Terminator::UnconditionalBranch { target: head.clone() },
                ),
                hint_normalize: false,
            },
        );
        let cfg = Cfg::new(
            Block {
                instructions: vec![(id(0), num(1)), (id(1), num(2))],
                terminator: (
                    id(5),
                    Terminator::UnconditionalBranch { target: head.clone() },
                ),
                hint_normalize: false,
            },
            named,
        );
        let mut fun = fun_with(cfg);
        let map = allocate(&fun);
        assert_ne!(
            map.slot_of(id(2)),
            map.slot_of(id(0)),
            "%2 is written before %3 reads %0"
        );
        fun.cfg.slots = std::sync::Arc::new(map);
        assert!(check(&fun).is_empty(), "{:?}", check(&fun));
    }
}
