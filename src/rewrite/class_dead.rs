//! Class-dead analysis: how much of the program is determined by the
//! merge-partition cells, and how much time the determined parts cost.
//!
//! With partitioned merges, every state is uniform in the partition cells,
//! so any instruction that transitively depends *only* on those cells (and
//! constants) computes one scalar per state - and every `select` whose
//! mask is such an instruction takes one arm wholesale, per class. The arm
//! it discards, and the pure chain feeding that arm exclusively, is
//! speculated work a per-class specialized program would not contain.
//!
//! This module classifies instructions:
//!
//!   * `class-determined`: value depends only on partition cells and
//!     constants (through pure ops and loads of the cells themselves);
//!   * `at-risk`: pure instructions whose every consumer path ends in the
//!     arm of a class-determined select (one of the two arms dies per
//!     class; the union of both is the at-risk pool, and roughly half of
//!     it - lane-weighted by which class dominates - is dead in any given
//!     class).
//!
//! The caller weights both sets with a measured per-instruction time
//! profile, giving the specialization prize as time rather than counts.
//! The analysis is conservative where it must be: any value flowing
//! through a store, call, or other effectful/unmodeled instruction is
//! treated as escaping (its producers stay live).

use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{Cfg, Instruction, LocalId};

use super::program::Program;

pub struct FunctionClassDead {
    pub determined: FxHashSet<LocalId>,
    pub at_risk: FxHashSet<LocalId>,
}

/// Whether an instruction is pure enough to be class-determined when its
/// inputs are: no heap effects, no state splits, value fully a function of
/// operands.
fn pure_value(instr: &Instruction) -> bool {
    matches!(
        instr,
        Instruction::NumberConstant { .. }
            | Instruction::BoolConstant { .. }
            | Instruction::BinaryOp { .. }
            | Instruction::UnaryOp { .. }
            | Instruction::Select { .. }
            | Instruction::Phi { .. }
    )
}

/// The partition-cell entry points in the IR: `get_global` of a partition
/// pattern followed by `load`. Field-path patterns ("player.p_dash") also
/// match their final segment against globals, mirroring the interpreter's
/// suffix rule closely enough for an analysis.
fn is_partition_global(name: &str, patterns: &[String]) -> bool {
    patterns
        .iter()
        .any(|p| name == p || p.ends_with(&format!(".{}", name)))
}

pub fn analyze_function(cfg: &Cfg, patterns: &[String]) -> FunctionClassDead {
    // Instruction table over the whole CFG.
    let mut instrs: FxHashMap<LocalId, &Instruction> = FxHashMap::default();
    let mut order: Vec<LocalId> = Vec::new();
    for block in std::iter::once(&cfg.entry).chain(cfg.named.values()) {
        for (id, instr) in &block.instructions {
            instrs.insert(*id, instr);
            order.push(*id);
        }
    }

    // 1. Class-determined: fixed point over the def-use graph.
    //    Seeds: loads whose source is a get_global of a partition cell
    //    (the promoted cells read their value cell via get_global + load).
    let mut determined: FxHashSet<LocalId> = FxHashSet::default();
    let mut partition_roots: FxHashSet<LocalId> = FxHashSet::default();
    for (&id, instr) in &instrs {
        if let Instruction::GetGlobal { name, .. } = instr {
            if is_partition_global(name, patterns) {
                partition_roots.insert(id);
            }
        }
    }
    loop {
        let mut changed = false;
        for &id in &order {
            if determined.contains(&id) {
                continue;
            }
            let instr = instrs[&id];
            let ok = match instr {
                Instruction::NumberConstant { .. } | Instruction::BoolConstant { .. } => true,
                Instruction::Load { source } => partition_roots.contains(source),
                _ if pure_value(instr) => {
                    let used = instr.get_used_locals();
                    !used.is_empty()
                        && used.iter().all(|u| {
                            determined.contains(u)
                                || matches!(instrs.get(u), Some(Instruction::NumberConstant { .. }))
                                || matches!(instrs.get(u), Some(Instruction::BoolConstant { .. }))
                        })
                }
                _ => false,
            };
            if ok {
                determined.insert(id);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    // 2. At-risk: pure instructions all of whose consumers are (a) the
    //    discarded-able arm slot of a class-determined select, or (b)
    //    other at-risk instructions. Terminator uses and effectful
    //    consumers pin things live. Computed as a fixed point shrinking
    //    from "all pure non-determined instructions".
    let mut consumers: FxHashMap<LocalId, Vec<LocalId>> = FxHashMap::default();
    let mut terminator_used: FxHashSet<LocalId> = FxHashSet::default();
    for block in std::iter::once(&cfg.entry).chain(cfg.named.values()) {
        for (id, instr) in &block.instructions {
            for used in instr.get_used_locals() {
                consumers.entry(used).or_default().push(*id);
            }
        }
        let (_, terminator) = &block.terminator;
        for used in terminator.get_used_locals() {
            terminator_used.insert(used);
        }
    }

    let mut at_risk: FxHashSet<LocalId> = order
        .iter()
        .copied()
        .filter(|id| {
            let instr = instrs[id];
            pure_value(instr) && !determined.contains(id) && !terminator_used.contains(id)
        })
        .collect();
    loop {
        let drop: Vec<LocalId> = at_risk
            .iter()
            .copied()
            .filter(|&id| {
                !consumers.get(&id).map_or(false, |cs| {
                    !cs.is_empty()
                        && cs.iter().all(|c| {
                            if at_risk.contains(c) {
                                return true;
                            }
                            // A select arm under a class-determined mask:
                            // this value only survives into one class.
                            match instrs.get(c) {
                                Some(Instruction::Select {
                                    condition,
                                    if_true,
                                    if_false,
                                }) => {
                                    determined.contains(condition)
                                        && (*if_true == id || *if_false == id)
                                        && *condition != id
                                }
                                _ => false,
                            }
                        })
                })
            })
            .collect();
        if drop.is_empty() {
            break;
        }
        for id in drop {
            at_risk.remove(&id);
        }
    }

    FunctionClassDead { determined, at_risk }
}

/// Run the analysis over every function of a program.
pub fn analyze(program: &Program) -> FxHashMap<String, FunctionClassDead> {
    let patterns = &program.merge_partition_cells;
    let mut out = FxHashMap::default();
    for (name, fun) in &program.functions {
        out.insert(name.as_str().to_string(), analyze_function(&fun.cfg, patterns));
    }
    out.insert(
        "__main".to_string(),
        analyze_function(program.frame_cfg(), patterns),
    );
    out
}
