//! `cse` - one instruction per value, within a basic block.
//!
//! Bulk rule: no location, deterministic, idempotent.
//!
//! # Why this exists
//!
//! It is a prerequisite for promoting object fields to SSA values. That rule's
//! core precondition is *"no other instruction in this function can produce a
//! pointer to this cell"* - without it, promoting one accessor leaves the others
//! reading and writing the real cell behind SSA's back, which is exactly how the
//! previous attempt's heap elimination went wrong.
//!
//! The program badly violates that today. `player.update_21` alone contains
//! `get_field %2.hitbox` **124 times**, all denoting one cell, and 416 redundant
//! accessors in total; program-wide there are 1675. Inlining created most of
//! them, since each spliced copy of a callee re-derives the object's fields from
//! scratch.
//!
//! # What counts as the same value
//!
//! Two instructions are interchangeable when they are structurally identical
//! *after* substituting the replacements found so far, so that chains collapse
//! in one pass:
//!
//! ```text
//!   %683 = get_field %2.spd     %706 = get_field %2.spd     -> folds to %683
//!   %684 = load %683            %707 = load %706            -> becomes load %683,
//!                                                              folds to %684
//! ```
//!
//! # What makes it safe: barriers
//!
//! Only pure arithmetic is unconditionally repeatable. The heap accessors are
//! repeatable only while nothing has disturbed what they read:
//!
//! * `get_field` / `get_index` / `get_global` return a pointer to a *cell*.
//!   Which cell that is can change if the field did not exist and something
//!   created it, or if the table was reset.
//! * `load` returns the cell's contents, which any store can change.
//!
//! So each kind carries a set of instructions that invalidate it, and the scan
//! forgets the affected entries when one is seen. `Call` invalidates everything,
//! because a callee can do anything. Nothing is assumed about aliasing: a
//! `create_if_missing` accessor for *any* receiver invalidates every accessor,
//! not just ones with a matching receiver.
//!
//! # Why block-local
//!
//! 1196 of the 1675 redundant accessors are duplicates within a single block,
//! and a block needs no dominance or path analysis - "between" is just an
//! interval. The remaining 479 need an earlier definition in a dominating block
//! with no barrier on *any* path between, which is a genuinely harder check and
//! is deliberately left until this one is measured.

use anyhow::{anyhow, Result};
use rustc_hash::FxHashMap;

use crate::ir::{Block, Cfg, Instruction, LocalId};

use super::super::program::Program;
use super::require;

/// What a surviving instruction is remembered by. Equality of these is equality
/// of the value produced, given that no barrier has intervened.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum Key {
    Field { receiver: LocalId, field: String, create: bool },
    Index { receiver: LocalId, index: LocalId, create: bool },
    Global { name: String, create: bool },
    Load { source: LocalId },
    Pure(String),
}

/// Which barriers each kind of key cares about.
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Kind {
    /// Produces a pointer to a cell. Disturbed by anything that can add a field
    /// to a table or replace a table wholesale.
    Accessor,
    /// Reads a cell. Disturbed by anything that can write one.
    Load,
    /// A function of its operands alone. SSA means those never change, so
    /// nothing disturbs it.
    Pure,
}

fn kind_of(key: &Key) -> Kind {
    match key {
        Key::Field { .. } | Key::Index { .. } | Key::Global { .. } => Kind::Accessor,
        Key::Load { .. } => Kind::Load,
        Key::Pure(_) => Kind::Pure,
    }
}

/// The key an instruction is remembered by, if it is a candidate at all.
///
/// `resolve` maps an operand to its surviving equivalent, so that a chain of
/// duplicated instructions collapses in a single left-to-right pass.
fn key_of(instr: &Instruction, resolve: &impl Fn(LocalId) -> LocalId) -> Option<Key> {
    let resolved = instr.map_local_ids(|id| resolve(id));
    Some(match &resolved {
        Instruction::GetField { receiver, field, create_if_missing } => Key::Field {
            receiver: *receiver,
            field: field.clone(),
            create: *create_if_missing,
        },
        Instruction::GetIndex { receiver, index, create_if_missing } => Key::Index {
            receiver: *receiver,
            index: *index,
            create: *create_if_missing,
        },
        Instruction::GetGlobal { name, create_if_missing } => Key::Global {
            name: name.clone(),
            create: *create_if_missing,
        },
        Instruction::Load { source } => Key::Load { source: *source },
        // Pure functions of their operands. Keyed by their debug form, which is
        // exact for these variants because every field is an operand or a
        // literal.
        Instruction::UnaryOp { .. }
        | Instruction::BinaryOp { .. }
        | Instruction::Select { .. }
        | Instruction::NumberConstant { .. }
        | Instruction::BoolConstant { .. }
        | Instruction::StringConstant { .. }
        | Instruction::NilConstant => Key::Pure(format!("{:?}", resolved)),
        // Deliberately excluded. `Alloc` produces a fresh cell every time.
        // `Phi` belongs to a control-flow join. The stores and `AssertClosure`
        // have effects and produce nothing.
        _ => return None,
    })
}

/// Does this instruction invalidate remembered values of `kind`?
///
/// Conservative on purpose: no aliasing is assumed anywhere. A
/// `create_if_missing` accessor on any receiver invalidates every accessor, and
/// a store through any pointer invalidates every load.
fn invalidates(instr: &Instruction, kind: Kind) -> bool {
    match kind {
        Kind::Pure => false,
        Kind::Accessor => match instr {
            // A callee can create fields or replace tables.
            Instruction::Call { .. } => true,
            // Creates a cell where there was none, so an earlier accessor for
            // the same field may have returned a nil pointer instead.
            Instruction::GetGlobal { create_if_missing, .. }
            | Instruction::GetField { create_if_missing, .. }
            | Instruction::GetIndex { create_if_missing, .. } => *create_if_missing,
            // Replaces a table, discarding every field it had.
            Instruction::StoreEmptyTable { .. } => true,
            _ => false,
        },
        Kind::Load => match instr {
            Instruction::Call { .. } => true,
            Instruction::Store { .. }
            | Instruction::StoreEmptyTable { .. }
            | Instruction::StoreClosure { .. } => true,
            // A load reads a cell, and a `create` accessor can turn a nil
            // pointer into a real one.
            Instruction::GetGlobal { create_if_missing, .. }
            | Instruction::GetField { create_if_missing, .. }
            | Instruction::GetIndex { create_if_missing, .. } => *create_if_missing,
            _ => false,
        },
    }
}

/// The substitution this rule performs on one function: every removed id mapped
/// to the surviving instruction that computes the same value.
///
/// Written once and used by both halves - see the note on `verify`.
fn substitution(cfg: &Cfg) -> FxHashMap<LocalId, LocalId> {
    let mut subst: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    for (_, block) in super::super::validate::all_blocks(cfg) {
        let mut live: FxHashMap<Key, LocalId> = FxHashMap::default();
        for (id, instr) in &block.instructions {
            // Forget anything this instruction could have disturbed. Done before
            // the lookup, so an instruction never matches something its own
            // effects invalidated.
            live.retain(|key, _| !invalidates(instr, kind_of(key)));

            let resolve = |id: LocalId| *subst.get(&id).unwrap_or(&id);
            let Some(key) = key_of(instr, &resolve) else { continue };
            match live.get(&key) {
                Some(existing) => {
                    subst.insert(*id, *existing);
                }
                None => {
                    live.insert(key, *id);
                }
            }
        }
    }
    subst
}

pub fn apply(program: &mut Program) -> Result<usize> {
    let mut changes = 0;
    for fun in program.functions.values_mut() {
        let subst = substitution(&fun.cfg);
        if subst.is_empty() {
            continue;
        }
        changes += subst.len();
        let resolve = |id: LocalId| *subst.get(&id).unwrap_or(&id);
        for block in blocks_mut(&mut fun.cfg) {
            block.instructions.retain(|(id, _)| !subst.contains_key(id));
            for (_, instr) in block.instructions.iter_mut() {
                *instr = instr.map_local_ids(resolve);
            }
            block.terminator.1 = block.terminator.1.map_local_ids(resolve);
        }
    }
    Ok(changes)
}

fn blocks_mut(cfg: &mut Cfg) -> impl Iterator<Item = &mut Block> {
    std::iter::once(&mut cfg.entry).chain(cfg.named.values_mut())
}

/// Independent check.
///
/// Like the other bulk rules, this verifies by re-deriving the answer from the
/// before program and insisting the after program is exactly that. Re-derivation
/// is the right strategy here because the rule is *canonical*: its result is a
/// function of the input alone, so an independent computation of that function
/// is a complete check. What it does not do - and what would defeat the purpose
/// - is trust anything the applier recorded about what it did.
///
/// The one piece of shared code is `substitution`, which is the specification of
/// the rule rather than an implementation detail of the applier. What is checked
/// separately here is everything the applier could get wrong on top of it:
/// removing an instruction that was not in the substitution, keeping one that
/// was, failing to substitute a use, reordering, or touching a terminator.
pub fn verify(before: &Program, after: &Program) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "cse changed the set of functions",
    )?;

    for (name, before_fun) in &before.functions {
        let after_fun = after.get(name.as_str())?;
        let subst = substitution(&before_fun.cfg);
        let resolve = |id: LocalId| *subst.get(&id).unwrap_or(&id);

        // A removed instruction must be replaced by one that survives, so the
        // substitution can never chain into a hole.
        for (removed, replacement) in &subst {
            require(
                !subst.contains_key(replacement),
                format!(
                    "cse in {}: %{} was replaced by %{}, which was itself removed",
                    name.as_str(),
                    usize::from(*removed),
                    usize::from(*replacement)
                ),
            )?;
        }

        for key in super::blocks_sorted(&before_fun.cfg) {
            let before_block = super::get_block(&before_fun.cfg, &key)
                .ok_or_else(|| anyhow!("cse: missing block in the before program"))?;
            let after_block = super::get_block(&after_fun.cfg, &key).ok_or_else(|| {
                anyhow!(
                    "cse removed block '{}' from {}",
                    super::super::validate::block_label(&key),
                    name.as_str()
                )
            })?;

            let mut after_iter = after_block.instructions.iter();
            for (id, instr) in &before_block.instructions {
                if subst.contains_key(id) {
                    continue;
                }
                let (after_id, after_instr) = after_iter.next().ok_or_else(|| {
                    anyhow!(
                        "cse dropped %{} from '{}' in {}",
                        usize::from(*id),
                        super::super::validate::block_label(&key),
                        name.as_str()
                    )
                })?;
                require(
                    after_id == id,
                    format!(
                        "cse reordered '{}' in {}: expected %{}, found %{}",
                        super::super::validate::block_label(&key),
                        name.as_str(),
                        usize::from(*id),
                        usize::from(*after_id)
                    ),
                )?;
                let want = instr.map_local_ids(resolve);
                require(
                    want == *after_instr,
                    format!(
                        "cse changed %{} in '{}' of {} unexpectedly:\n  want {}\n  got  {}",
                        usize::from(*id),
                        super::super::validate::block_label(&key),
                        name.as_str(),
                        super::super::print::format_instruction(&want),
                        super::super::print::format_instruction(after_instr)
                    ),
                )?;
            }
            require(
                after_iter.next().is_none(),
                format!(
                    "cse added instructions to '{}' in {}",
                    super::super::validate::block_label(&key),
                    name.as_str()
                ),
            )?;

            let want_term = before_block.terminator_kind().map_local_ids(resolve);
            require(
                format!("{:?}", want_term) == format!("{:?}", after_block.terminator_kind()),
                format!(
                    "cse changed the terminator of '{}' in {}",
                    super::super::validate::block_label(&key),
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
    use crate::ir::{FunDef, GlobalId, Terminator};
    use crate::rewrite::print::format_function;
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn field(receiver: usize, name: &str, create: bool) -> Instruction {
        Instruction::GetField {
            receiver: id(receiver),
            field: name.to_string(),
            create_if_missing: create,
        }
    }

    fn program_of(instructions: Vec<(LocalId, Instruction)>, ret: LocalId) -> Program {
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![Some(id(2))],
            cfg: Cfg::new(
                Block {
                    instructions,
                    terminator: (id(900), Terminator::Return { value: Some(ret) }),
                    hint_normalize: false,
                },
                Default::default(),
            ),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions }
    }

    fn run(p: &mut Program) -> String {
        let before = p.clone();
        apply(p).unwrap();
        verify(&before, p).unwrap();
        format_function(p.get("f").unwrap())
    }

    #[test]
    fn collapses_repeated_field_reads() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), field(2, "x", false)),
                (id(12), Instruction::Load { source: id(11) }),
            ],
            id(12),
        );
        let text = run(&mut p);
        assert!(!text.contains("%11"), "{}", text);
        assert!(text.contains("%12 = load %10"), "{}", text);
    }

    /// The chain must collapse in one pass: once the accessor folds, the load
    /// of it becomes a duplicate too.
    #[test]
    fn collapses_a_chain_in_one_pass() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "spd", false)),
                (id(11), Instruction::Load { source: id(10) }),
                (id(12), field(2, "spd", false)),
                (id(13), Instruction::Load { source: id(12) }),
                (id(14), field(13, "x", false)),
            ],
            id(14),
        );
        let text = run(&mut p);
        assert!(!text.contains("%12"), "{}", text);
        assert!(!text.contains("%13"), "{}", text);
        assert!(text.contains("%14 = get_field %11.x"), "{}", text);
    }

    /// A store between two loads of the same cell means the second may see a
    /// different value.
    #[test]
    fn a_store_stops_a_load_from_being_reused() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Load { source: id(10) }),
                (id(12), Instruction::Store { target: id(10), source: id(2) }),
                (id(13), Instruction::Load { source: id(10) }),
            ],
            id(13),
        );
        let text = run(&mut p);
        assert!(text.contains("%13 = load %10"), "{}", text);
    }

    /// ...but a store does not stop the *accessor* from being reused, because a
    /// store writes a cell rather than adding a field.
    #[test]
    fn a_store_does_not_stop_an_accessor_from_being_reused() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Store { target: id(10), source: id(2) }),
                (id(12), field(2, "x", false)),
            ],
            id(12),
        );
        let text = run(&mut p);
        assert!(!text.contains("%12"), "{}", text);
    }

    /// A creating accessor may bring a field into existence, so an earlier
    /// non-creating read of it might have returned a nil pointer instead.
    #[test]
    fn a_creating_accessor_stops_reuse() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), field(2, "y", true)),
                (id(12), field(2, "x", false)),
            ],
            id(12),
        );
        let text = run(&mut p);
        assert!(text.contains("%12 = get_field %2.x"), "{}", text);
    }

    /// A call can do anything at all.
    #[test]
    fn a_call_stops_reuse() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Call { closure: id(2), args: vec![] }),
                (id(12), field(2, "x", false)),
            ],
            id(12),
        );
        let text = run(&mut p);
        assert!(text.contains("%12 = get_field %2.x"), "{}", text);
    }

    /// Reading a field and creating it are different instructions and must not
    /// be conflated in either direction.
    #[test]
    fn reading_and_creating_are_different_values() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), field(2, "x", true)),
            ],
            id(11),
        );
        let text = run(&mut p);
        assert!(text.contains("%11 = get_field %2.x create"), "{}", text);
    }

    /// `alloc` produces a fresh cell every time and must never be merged.
    #[test]
    fn allocs_are_never_merged() {
        let mut p = program_of(
            vec![(id(10), Instruction::Alloc), (id(11), Instruction::Alloc)],
            id(11),
        );
        let text = run(&mut p);
        assert!(text.contains("%10 = alloc") && text.contains("%11 = alloc"), "{}", text);
    }

    /// Applying the rule twice must change nothing the second time.
    #[test]
    fn is_idempotent() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), field(2, "x", false)),
                (id(12), Instruction::Load { source: id(11) }),
            ],
            id(12),
        );
        run(&mut p);
        let once = format_function(p.get("f").unwrap());
        assert_eq!(apply(&mut p).unwrap(), 0);
        assert_eq!(once, format_function(p.get("f").unwrap()));
    }

    /// `verify` must reject an applier that removed an instruction it had no
    /// licence to remove.
    #[test]
    fn verify_rejects_an_unjustified_removal() {
        let mut p = program_of(
            vec![
                (id(10), field(2, "x", false)),
                (id(11), Instruction::Call { closure: id(2), args: vec![] }),
                (id(12), field(2, "x", false)),
            ],
            id(10),
        );
        let before = p.clone();
        // Pretend the call was not a barrier.
        let fun = p.get_mut("f").unwrap();
        fun.cfg.entry.instructions.retain(|(i, _)| *i != id(12));
        fun.cfg.entry.terminator.1 = Terminator::Return { value: Some(id(10)) };
        assert!(verify(&before, &p).is_err());
    }
}
