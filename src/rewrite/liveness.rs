//! Liveness, computed at transform time.
//!
//! Two uses, neither of which requires the interpreter to know anything about
//! liveness:
//!
//!   * reporting - how many locals are simultaneously live tells us how small
//!     `LocalEnv` could be made, which decides whether slot allocation is worth
//!     doing (see `plans/inline-parked.md`)
//!   * eventually, slot allocation itself: values whose live ranges do not
//!     overlap can share a `LocalId`, which keeps the id range dense no matter
//!     how much inlining happens
//!
//! Note the contrast with `src/liveness.rs`, which is a runtime hook the
//! interpreter consults on every block entry and which has never been
//! implemented. Doing the analysis here instead means the interpreter keeps
//! doing the dumb thing quickly.
//!
//! # Definitions
//!
//! A local is *live* at a program point if some path from that point reads it
//! before writing it. Phi operands are special: a phi consumes its operand on
//! the incoming *edge*, so an operand is live at the end of the predecessor
//! named by that branch, not at the top of the phi's own block.

use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{Block, Cfg, FunDef, Instruction, LocalId};

use super::validate::{all_blocks, compute_reachable, successors, BlockKey};

pub struct Liveness {
    /// Locals live on entry to each block, before its phis run.
    pub live_in: FxHashMap<BlockKey, FxHashSet<LocalId>>,
    /// Locals live after the last instruction of each block.
    pub live_out: FxHashMap<BlockKey, FxHashSet<LocalId>>,
}

impl Liveness {
    /// The largest number of locals live at any single point in the function.
    ///
    /// This is the number of slots a `LocalEnv` would actually need, as opposed
    /// to `max LocalId + 1`, which is what it currently costs.
    pub fn max_simultaneously_live(&self, cfg: &Cfg) -> usize {
        let mut max = 0;
        for (key, block) in all_blocks(cfg) {
            let Some(out) = self.live_out.get(&key) else { continue };
            // Walk the block backwards, reconstructing the live set at each
            // point, and take the high-water mark.
            let mut live = out.clone();
            max = max.max(live.len());
            live.extend(block.terminator_kind().get_used_locals());
            max = max.max(live.len());
            for (id, instr) in block.instructions.iter().rev() {
                live.remove(id);
                if !matches!(instr, Instruction::Phi { .. }) {
                    live.extend(instr.get_used_locals());
                }
                max = max.max(live.len());
            }
        }
        max
    }
}

pub fn analyze(fun: &FunDef) -> Liveness {
    let cfg = &fun.cfg;
    let blocks = all_blocks(cfg);
    let reachable = compute_reachable(&blocks);
    let by_key: FxHashMap<BlockKey, &Block> =
        blocks.iter().map(|(k, b)| (k.clone(), *b)).collect();

    let mut live_in: FxHashMap<BlockKey, FxHashSet<LocalId>> = FxHashMap::default();
    let mut live_out: FxHashMap<BlockKey, FxHashSet<LocalId>> = FxHashMap::default();
    for (key, _) in &blocks {
        live_in.insert(key.clone(), FxHashSet::default());
        live_out.insert(key.clone(), FxHashSet::default());
    }

    // Backward fixpoint. These CFGs are small, so a plain iterate-to-stable loop
    // is fine and is much easier to be confident in than a worklist.
    let mut changed = true;
    while changed {
        changed = false;
        for (key, block) in blocks.iter().rev() {
            if !reachable.contains(key) {
                continue;
            }

            // live_out(B) = union over successors S of
            //   (live_in(S) minus S's phi definitions)
            //   plus the operands S's phis take from B
            let mut out: FxHashSet<LocalId> = FxHashSet::default();
            for succ in successors(block) {
                let Some(succ_block) = by_key.get(&succ) else { continue };
                let mut from_succ = live_in.get(&succ).cloned().unwrap_or_default();
                for (id, instr) in &succ_block.instructions {
                    let Instruction::Phi { branches } = instr else { continue };
                    from_succ.remove(id);
                    for (label, value) in branches {
                        let names_this_block = match key {
                            None => label.as_str() == "__entry",
                            Some(l) => label == l,
                        };
                        if names_this_block {
                            from_succ.insert(*value);
                        }
                    }
                }
                out.extend(from_succ);
            }

            // live_in(B): walk the block backwards.
            let mut live = out.clone();
            live.extend(block.terminator_kind().get_used_locals());
            for (id, instr) in block.instructions.iter().rev() {
                live.remove(id);
                // A phi's operands belong to the incoming edges, handled above.
                if !matches!(instr, Instruction::Phi { .. }) {
                    live.extend(instr.get_used_locals());
                }
            }

            if live_out[key] != out {
                live_out.insert(key.clone(), out);
                changed = true;
            }
            if live_in[key] != live {
                live_in.insert(key.clone(), live);
                changed = true;
            }
        }
    }

    Liveness { live_in, live_out }
}

/// How much a function gains from slot allocation.
pub struct SlotReport {
    pub name: String,
    /// What `LocalEnv` costs for this function: the length of its slot map, or,
    /// while the map is still the identity, the highest `LocalId` plus one.
    pub env_slots_now: usize,
    /// The floor: no allocation can use fewer slots than the largest number of
    /// values live at one time.
    pub env_slots_packed: usize,
    pub definitions: usize,
}

pub fn slot_report(fun: &FunDef) -> SlotReport {
    let liveness = analyze(fun);
    let definitions = fun
        .cfg
        .iter_blocks()
        .map(|b| b.instructions.len())
        .sum::<usize>();
    SlotReport {
        name: fun.name.as_str().to_string(),
        env_slots_now: super::slots::slot_count(fun),
        env_slots_packed: liveness.max_simultaneously_live(&fun.cfg),
        definitions,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Cfg, GlobalId, Terminator};

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn num(n: i16) -> Instruction {
        Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(n) }
    }

    /// Three constants defined in a row, only the last one used: at most two are
    /// ever live at once, even though three ids exist.
    #[test]
    fn counts_simultaneous_liveness_not_definitions() {
        let cfg = Cfg::new(Block {
                instructions: vec![
                    (id(0), num(1)),
                    (id(1), num(2)),
                    (id(2), Instruction::BinaryOp {
                        left: id(0),
                        op: crate::ir::BinaryOp::Plus,
                        right: id(1),
                    }),
                ],
                terminator: (id(3), Terminator::Return { value: Some(id(2)) }),
                hint_normalize: false,
            }, crate::ir::new_label_map());
        let fun = FunDef {
            name: GlobalId::from("t".to_string()),
            capture_ids: vec![],
            arg_ids: vec![],
            cfg,
            source_span: None,
        };
        let report = slot_report(&fun);
        assert_eq!(report.definitions, 3);
        assert_eq!(report.env_slots_now, 4);
        // %0 and %1 are live together; %2 is live alone afterwards.
        assert_eq!(report.env_slots_packed, 2);
    }
}
