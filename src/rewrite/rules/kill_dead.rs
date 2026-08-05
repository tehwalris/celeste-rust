//! `kill_dead` - write deadness into the IR as explicit `Kill` instructions.
//!
//! Bulk rule: no location, deterministic, idempotent.
//!
//! # Why the IR and not the interpreter
//!
//! The interpreter has a hook for this (`BoundInterpreterFlow::BlockBeforeJoin`
//! prunes `LocalEnv` by a live set) and it has never done anything, because
//! `src/liveness.rs` is a no-op stub that always answers "keep everything".
//! Computing liveness at rewrite time instead means the interpreter keeps
//! doing the dumb thing quickly, and - the actual reason - the result becomes
//! *checkable*: a `Kill` is a claim in the program text that a verifier can
//! refute, rather than an analysis the interpreter has to be trusted to get
//! right on every block entry.
//!
//! # Why it should be worth something
//!
//! Measured at frame 39 of the rewritten program: branch filters cost 1.05 s,
//! gathering 4,000 vector columns across 124 filter events, of which 1,677
//! (41.9%) live in `local_env`. Every dead temporary in there is copied by
//! every filter. And because `local_env` is part of `StateShape`, a dead
//! value left on one path also stops that state merging with an otherwise
//! identical one, which feeds the merge machinery more rows than it needs.
//!
//! # Placement
//!
//! One `Kill` at the end of each block, before the terminator, naming
//! everything dead at that point. "Dead at the end of B" is
//!
//! ```text
//!   (live_in(B) u defined_in(B) u U over predecessors P of live_out(P))
//!     \ (live_out(B) u locals the terminator reads)
//! ```
//!
//! The third term is what makes the environment's contents path-independent,
//! and it is easy to miss. A value can die *on an edge*: live at the end of P
//! because some other successor of P reads it, dead on entry to B. Without
//! that term B would carry it to the next merge while a sibling path had
//! already dropped it, and two states that should have merged would differ by
//! a value neither of them can read.
//!
//! Blocks ending in `Return` are skipped: `flow.rs` already clears the
//! environment there, so a kill would be pure noise.

use anyhow::Result;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{Block, FunDef, Instruction, LocalId, Terminator};

use super::super::liveness;
use super::super::program::Program;
use super::super::validate::{all_blocks, successors, BlockKey};
use super::{blocks_sorted, get_block_mut, predecessors, require, LocalIdAllocator};

pub fn apply(program: &mut Program) -> Result<usize> {
    let mut changes = 0;
    for fun in program.functions.values_mut() {
        changes += apply_to_function(fun);
    }
    Ok(changes)
}

/// Strips every existing `Kill` and re-derives them from scratch, which is
/// what makes the rule idempotent: the second run computes the same liveness
/// over the same stripped program and emits the same instructions.
fn apply_to_function(fun: &mut FunDef) -> usize {
    let before = super::super::print::format_function(fun);
    strip_kills(fun);

    let live = liveness::analyze(fun);
    let preds = predecessors(&fun.cfg);
    let plan: Vec<(BlockKey, Vec<LocalId>)> = all_blocks(&fun.cfg)
        .into_iter()
        .filter_map(|(key, block)| {
            let dead = dead_at_end(&key, block, &live, &preds);
            (!dead.is_empty()).then_some((key, dead))
        })
        .collect();

    let mut ids = LocalIdAllocator::for_function(fun);
    for (key, values) in plan {
        let id = ids.fresh();
        let block = get_block_mut(&mut fun.cfg, &key).expect("block from all_blocks");
        block.instructions.push((id, Instruction::Kill { values }));
    }

    usize::from(super::super::print::format_function(fun) != before)
}

fn strip_kills(fun: &mut FunDef) {
    for key in blocks_sorted(&fun.cfg) {
        if let Some(block) = get_block_mut(&mut fun.cfg, &key) {
            block
                .instructions
                .retain(|(_, instr)| !matches!(instr, Instruction::Kill { .. }));
        }
    }
}

fn dead_at_end(
    key: &BlockKey,
    block: &Block,
    live: &liveness::Liveness,
    preds: &FxHashMap<BlockKey, Vec<BlockKey>>,
) -> Vec<LocalId> {
    if matches!(block.terminator_kind(), Terminator::Return { .. }) {
        return Vec::new();
    }
    let Some(live_out) = live.live_out.get(key) else {
        return Vec::new();
    };

    let mut present: FxHashSet<LocalId> = FxHashSet::default();
    if let Some(live_in) = live.live_in.get(key) {
        present.extend(live_in.iter().copied());
    }
    present.extend(block.instructions.iter().map(|(id, _)| *id));
    for pred in preds.get(key).into_iter().flatten() {
        if let Some(pred_out) = live.live_out.get(pred) {
            present.extend(pred_out.iter().copied());
        }
    }

    let mut keep: FxHashSet<LocalId> = live_out.clone();
    keep.extend(block.terminator_kind().get_used_locals());

    let mut dead: Vec<LocalId> = present.difference(&keep).copied().collect();
    // `all_blocks` and the sets above are hash-ordered; the recipe must
    // replay byte-for-byte, so sort.
    dead.sort();
    dead
}

/// Checks, without consulting the liveness analysis that placed them, that no
/// killed local is ever read again.
///
/// Deliberately a forward search rather than a second backwards liveness pass:
/// a shared bug in `liveness::analyze` would cancel out between applier and
/// verifier, and this property - "walking forward from the kill, every path
/// either redefines the local or never mentions it" - is exactly what makes a
/// kill safe, stated directly.
pub fn verify(before: &Program, after: &Program) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "kill_dead must not add or remove functions",
    )?;

    for (name, after_fun) in &after.functions {
        let before_fun = after_fun_counterpart(before, name)?;
        require(
            strip_kills_text(before_fun) == strip_kills_text(after_fun),
            format!(
                "{}: kill_dead changed something other than kill instructions",
                name.as_str()
            ),
        )?;
        for (key, block) in all_blocks(&after_fun.cfg) {
            for (index, (_, instr)) in block.instructions.iter().enumerate() {
                let Instruction::Kill { values } = instr else {
                    continue;
                };
                for value in values {
                    require(
                        !reaches_a_use(after_fun, &key, index + 1, *value),
                        format!(
                            "{}: kill of %{} in block {} is followed by a read of it",
                            name.as_str(),
                            usize::from(*value),
                            super::super::validate::block_label(&key),
                        ),
                    )?;
                }
            }
        }
    }
    Ok(())
}

fn after_fun_counterpart<'a>(
    before: &'a Program,
    name: &crate::ir::GlobalId,
) -> Result<&'a FunDef> {
    before
        .functions
        .get(name)
        .ok_or_else(|| anyhow::anyhow!("kill_dead added function {}", name.as_str()))
}

fn strip_kills_text(fun: &FunDef) -> String {
    let mut copy = fun.clone();
    strip_kills(&mut copy);
    super::super::print::format_function(&copy)
}

/// True if any path forward from `(key, start)` reads `value` before some
/// instruction redefines it.
fn reaches_a_use(fun: &FunDef, key: &BlockKey, start: usize, value: LocalId) -> bool {
    let blocks: FxHashMap<BlockKey, &Block> = all_blocks(&fun.cfg).into_iter().collect();
    // Entry points into the walk, as (block, first instruction index).
    let mut stack = vec![(key.clone(), start)];
    let mut seen: FxHashSet<BlockKey> = FxHashSet::default();
    while let Some((block_key, from)) = stack.pop() {
        // A block can be entered at an offset only the first time (from the
        // kill itself); anything reached later starts at 0, so visiting a
        // block once at index 0 is enough.
        if from == 0 && !seen.insert(block_key.clone()) {
            continue;
        }
        let Some(block) = blocks.get(&block_key) else {
            continue;
        };
        let mut redefined = false;
        for (id, instr) in block.instructions.iter().skip(from) {
            // A phi reads its operand on the incoming edge, not here, so a
            // phi naming `value` is not a use at this point - but any other
            // instruction is.
            if !matches!(instr, Instruction::Phi { .. })
                && instr.get_used_locals().contains(&value)
            {
                return true;
            }
            // A phi *operand* on an outgoing edge is a use in the successor's
            // eyes; that is covered by walking into the successor below.
            if *id == value {
                redefined = true;
                break;
            }
        }
        if redefined {
            continue;
        }
        if block.terminator_kind().get_used_locals().contains(&value) {
            return true;
        }
        for succ in successors(block) {
            // Phi operands are consumed on the edge into the successor.
            // A phi consumes its operand on one specific incoming edge, so
            // only the branch naming *this* block is a use here. Being
            // sloppy about that rejects every loop head, whose phi lists an
            // operand from the latch that the preheader edge never reads.
            let edge_label = block_key
                .as_ref()
                .map_or("__entry", |label| label.as_str());
            if let Some(target) = blocks.get(&succ) {
                let read_by_phi = target.instructions.iter().any(|(_, instr)| match instr {
                    Instruction::Phi { branches } => branches
                        .iter()
                        .any(|(label, operand)| label.as_str() == edge_label && *operand == value),
                    _ => false,
                });
                if read_by_phi {
                    return true;
                }
            }
            stack.push((succ, 0));
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Cfg, GlobalId, Label, Terminator};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(name: &str) -> Label {
        Label::from(name.to_string())
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

    fn program(entry: Block, named: Vec<(&str, Block)>) -> Program {
        let mut map = crate::ir::new_label_map();
        for (name, b) in named {
            map.insert(label(name), b);
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

    fn num(n: usize) -> Instruction {
        Instruction::NumberConstant {
            value: crate::pico8_num::Pico8Num::from_i16(n as i16),
        }
    }

    fn kills_of(program: &Program) -> Vec<Vec<usize>> {
        let fun = program.functions.values().next().unwrap();
        all_blocks(&fun.cfg)
            .into_iter()
            .flat_map(|(_, b)| b.instructions.clone())
            .filter_map(|(_, instr)| match instr {
                Instruction::Kill { values } => {
                    Some(values.iter().map(|v| usize::from(*v)).collect())
                }
                _ => None,
            })
            .collect()
    }

    /// %1 is read after the branch, %2 never is, so only %2 dies here.
    #[test]
    fn kills_the_value_nothing_reads_again() {
        let entry = block(
            vec![(1, num(1)), (2, num(2))],
            3,
            Terminator::UnconditionalBranch { target: label("tail") },
        );
        let tail = block(
            vec![(4, Instruction::UnaryOp { op: crate::ir::UnaryOp::Minus, arg: id(1) })],
            5,
            Terminator::Return { value: Some(id(4)) },
        );
        let mut p = program(entry, vec![("tail", tail)]);
        apply(&mut p).unwrap();
        assert_eq!(kills_of(&p), vec![vec![2]]);
    }

    /// Running it twice must produce the same program - it is a bulk rule,
    /// so a recipe replays it and expects byte-identical output.
    #[test]
    fn is_idempotent() {
        let entry = block(
            vec![(1, num(1)), (2, num(2))],
            3,
            Terminator::UnconditionalBranch { target: label("tail") },
        );
        let tail = block(vec![], 5, Terminator::Return { value: Some(id(1)) });
        let mut p = program(entry, vec![("tail", tail)]);
        apply(&mut p).unwrap();
        let once = super::super::super::print::format_program(&p);
        let changes = apply(&mut p).unwrap();
        assert_eq!(changes, 0);
        assert_eq!(super::super::super::print::format_program(&p), once);
    }

    /// The verifier's whole job. A kill of a value the next block reads must
    /// be refused, however it got there.
    #[test]
    fn verify_refuses_a_kill_before_a_use() {
        let before_entry = block(
            vec![(1, num(1))],
            3,
            Terminator::UnconditionalBranch { target: label("tail") },
        );
        let before_tail = block(
            vec![(4, Instruction::UnaryOp { op: crate::ir::UnaryOp::Minus, arg: id(1) })],
            5,
            Terminator::Return { value: Some(id(4)) },
        );
        let before = program(before_entry, vec![("tail", before_tail)]);

        let after_entry = block(
            vec![(1, num(1)), (9, Instruction::Kill { values: vec![id(1)] })],
            3,
            Terminator::UnconditionalBranch { target: label("tail") },
        );
        let after_tail = block(
            vec![(4, Instruction::UnaryOp { op: crate::ir::UnaryOp::Minus, arg: id(1) })],
            5,
            Terminator::Return { value: Some(id(4)) },
        );
        let after = program(after_entry, vec![("tail", after_tail)]);

        let error = format!("{:#}", verify(&before, &after).unwrap_err());
        assert!(error.contains("followed by a read"), "{}", error);
    }

    /// A loop head's phi names an operand from the latch that the preheader
    /// edge never supplies. Reading that as a use rejects every loop, which
    /// is how the first version of this verifier failed on the real program.
    #[test]
    fn a_phi_operand_is_a_use_only_on_its_own_edge() {
        let entry = block(
            vec![(1, num(1))],
            2,
            Terminator::UnconditionalBranch { target: label("head") },
        );
        let head = block(
            vec![
                (
                    3,
                    Instruction::Phi {
                        branches: vec![(label("__entry"), id(1)), (label("latch"), id(6))],
                    },
                ),
                (9, Instruction::Kill { values: vec![id(1)] }),
            ],
            4,
            Terminator::ConditionalBranch {
                condition: id(3),
                true_target: label("latch"),
                false_target: label("done"),
            },
        );
        let latch = block(
            vec![(6, num(2))],
            7,
            Terminator::UnconditionalBranch { target: label("head") },
        );
        let done = block(vec![], 8, Terminator::Return { value: Some(id(3)) });
        let after = program(
            entry.clone(),
            vec![("head", head), ("latch", latch.clone()), ("done", done.clone())],
        );

        let head_before = block(
            vec![(
                3,
                Instruction::Phi {
                    branches: vec![(label("__entry"), id(1)), (label("latch"), id(6))],
                },
            )],
            4,
            Terminator::ConditionalBranch {
                condition: id(3),
                true_target: label("latch"),
                false_target: label("done"),
            },
        );
        let before = program(
            entry,
            vec![("head", head_before), ("latch", latch), ("done", done)],
        );
        // %1 is consumed by the phi on the __entry edge, which has already
        // happened by the time the kill runs inside the head.
        verify(&before, &after).unwrap();
    }
}
