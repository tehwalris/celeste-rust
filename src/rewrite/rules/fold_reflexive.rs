//! `fold_reflexive` - a comparison of a value with itself becomes a constant.
//!
//! # Why
//!
//! The compiled `is_solid(x, y)` chains open with a gate on `y > 0`, and where
//! the call site passes a literal `0` the inliner leaves behind
//!
//! ```text
//!   %481 = num Pico8Num("0x0000")
//!   %3485 = %481 > %481
//!   %3486 = br %3485 ? in_k034_and_or_continue_416 : in_k034_and_or_join_417
//! ```
//!
//! - a branch that can never be taken, guarding a whole `check`/`collide`
//! loop. `fold` deliberately refuses arithmetic, because evaluating PICO-8
//! semantics inside the rewriter would make the verifier depend on the
//! applier's notion of truth. But a *reflexive* comparison needs no
//! arithmetic: an SSA local is one value, and a PICO-8 number compares to
//! itself the same way whatever number it is - the representation is 16.16
//! fixed point, so there is no NaN to spoil reflexivity. `x > x` is false and
//! `x <= x` is true without ever computing `x`.
//!
//! # What it does
//!
//! Every `%r = %x op %x` where `op` is one of the six comparisons *and* `%x`
//! is defined by a `NumberConstant` becomes the `BoolConstant` reflexivity
//! dictates:
//!
//! ```text
//!   ==, <=, >=  ->  true
//!   ~=, <,  >   ->  false
//! ```
//!
//! The id keeps its place; nothing else moves. Folding the branch that reads
//! the constant is `fold`'s existing job, and deleting the loop it strands is
//! `dce`'s - this rule only manufactures the constant they need.
//!
//! # Why every family needs a static witness
//!
//! Reflexivity holds for any *number*, but the comparison operators error
//! loudly on non-numbers, and folding `t > t` for a table would silently
//! replace that loud error with `false`. Requiring the operand to be a visible
//! `NumberConstant` keeps the fold semantics-free: the original instruction
//! provably could not have failed, so replacing its result is the identity.
//!
//! `==`/`~=` never error, but they are *not* reflexive in the abstract
//! semantics: `UnknownBool == UnknownBool` is `UnknownBool` (two unknowns
//! may differ), and `NumberInterval == anything` is `false` - even against
//! itself. So the equality families demand a witness that the operand is a
//! concrete pointer: a dominating instruction that fails loudly unless it
//! is one (`get_field`/`get_index` receiver, `assert_pointer`,
//! `assert_closure`). Past the witness the operand is a real `Pointer`, and
//! pointer equality is decidable by identity: `p == p` is `true`,
//! `p == nil` is `false`. `NilConstant` against `NilConstant` needs no
//! witness at all - `nil == nil` is `true` whatever the hints say. The
//! motivating sites are what `assume_eq` leaves behind in the collapsed
//! object-table loops: `%2 ~= %2` (was `o ~= obj`) and `%2 ~= nil`, on an
//! object whose fields the surrounding code already reads.
//!
//! Bulk rule: no location, deterministic, idempotent (the replaced
//! instruction is a `BoolConstant`, which the detection never matches).

use anyhow::Result;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{BinaryOp, FunDef, Instruction, Label, LocalId};

use super::super::validate::Dominance;

use super::super::print::format_instruction;
use super::super::program::Program;
use super::require;

/// The constant a reflexive comparison folds to, or `None` if `op` is not a
/// comparison.
fn reflexive_value(op: &BinaryOp) -> Option<bool> {
    match op {
        BinaryOp::TwoEqual | BinaryOp::LessThanEqual | BinaryOp::GreaterThanEqual => Some(true),
        BinaryOp::TildeEqual | BinaryOp::LessThan | BinaryOp::GreaterThan => Some(false),
        _ => None,
    }
}

/// Does `instr` fail loudly unless `x` holds a real pointer? These are the
/// pointer witnesses: past one, `x` is a `Pointer` - not `NilPointer`, not
/// `UnknownBool`, not an interval - or execution already died.
fn is_pointer_witness(instr: &Instruction, x: LocalId) -> bool {
    match instr {
        Instruction::GetField { receiver, .. } | Instruction::GetIndex { receiver, .. } => {
            *receiver == x
        }
        Instruction::AssertPointer { value } | Instruction::AssertClosure { value, .. } => {
            *value == x
        }
        _ => false,
    }
}

/// The ids this rule folds in `fun`, with the constant each becomes. Derived
/// from the function alone, so `apply` and `verify` agree by construction.
///
/// Four families, each decidable without evaluating arithmetic:
///
///   1. `x op x` where `x` is a `NumberConstant` - any comparison; a number
///      compares to itself by reflexivity.
///   2. `x op x`, `op` in `==`/`~=`, where a *pointer witness* on `x`
///      dominates the compare. A pointer compares equal to itself; the
///      witness is what excludes the operands whose abstract semantics are
///      not reflexive (`UnknownBool == UnknownBool` is `UnknownBool`, and
///      `NumberInterval == anything` is `false` - even itself).
///   3. `x op y`, `op` in `==`/`~=`, both defined by `NilConstant`:
///      `nil == nil` is `true` whatever the hints say.
///   4. `x op y`, `op` in `==`/`~=`, one side pointer-witnessed and the
///      other a `NilConstant`: a pointer never equals nil.
///
/// Families 2-4 are behind the `pointers` opt-in: recipe entries written
/// before they existed must keep replaying byte-identically, and folding
/// more mid-recipe changes every entry after.
fn foldable(fun: &FunDef, pointers: bool) -> Vec<(LocalId, bool)> {
    let mut number_constants: FxHashSet<LocalId> = FxHashSet::default();
    let mut nil_constants: FxHashSet<LocalId> = FxHashSet::default();
    for block in fun.cfg.iter_blocks() {
        for (id, instr) in &block.instructions {
            match instr {
                Instruction::NumberConstant { .. } => {
                    number_constants.insert(*id);
                }
                Instruction::NilConstant => {
                    nil_constants.insert(*id);
                }
                _ => {}
            }
        }
    }

    // Pointer-witness positions per local, for the dominance check.
    let dominance = Dominance::of(&fun.cfg);
    let mut witnesses: FxHashMap<LocalId, Vec<(Option<Label>, usize)>> = FxHashMap::default();
    for key in super::blocks_sorted(&fun.cfg) {
        let block = super::get_block(&fun.cfg, &key).expect("listed block exists");
        for (index, (_, instr)) in block.instructions.iter().enumerate() {
            for x in instr.get_used_locals() {
                if is_pointer_witness(instr, x) {
                    witnesses.entry(x).or_default().push((key.clone(), index));
                }
            }
        }
    }
    let witnessed_at = |x: LocalId, key: &Option<Label>, index: usize| -> bool {
        let Some(sites) = witnesses.get(&x) else { return false };
        sites.iter().any(|(w_key, w_index)| {
            if w_key == key {
                return *w_index < index;
            }
            match (dominance.index_of(w_key), dominance.index_of(key)) {
                (Some(w), Some(b)) => dominance.dominates(w, b),
                _ => false,
            }
        })
    };

    let mut out = Vec::new();
    for key in super::blocks_sorted(&fun.cfg) {
        let block = super::get_block(&fun.cfg, &key).expect("listed block exists");
        for (index, (id, instr)) in block.instructions.iter().enumerate() {
            let Instruction::BinaryOp { left, op, right } = instr else { continue };
            // Family 1: reflexive on a number constant, all six comparisons.
            if left == right && number_constants.contains(left) {
                if let Some(value) = reflexive_value(op) {
                    out.push((*id, value));
                }
                continue;
            }
            // The remaining families decide only equality, and only past
            // the opt-in.
            if !pointers {
                continue;
            }
            let eq = match op {
                BinaryOp::TwoEqual => true,
                BinaryOp::TildeEqual => false,
                _ => continue,
            };
            // Family 2: reflexive on a witnessed pointer.
            if left == right && witnessed_at(*left, &key, index) {
                out.push((*id, eq));
                continue;
            }
            // Family 3: nil against nil.
            if nil_constants.contains(left) && nil_constants.contains(right) {
                out.push((*id, eq));
                continue;
            }
            // Family 4: a witnessed pointer against nil.
            let pointer_vs_nil = (witnessed_at(*left, &key, index)
                && nil_constants.contains(right))
                || (nil_constants.contains(left) && witnessed_at(*right, &key, index));
            if pointer_vs_nil {
                out.push((*id, !eq));
            }
        }
    }
    out.sort();
    out.dedup();
    out
}

pub fn apply(program: &mut Program, pointers: bool) -> Result<usize> {
    let mut changes = 0;
    for fun in program.functions.values_mut() {
        for (id, value) in foldable(fun, pointers) {
            for block in std::iter::once(&mut fun.cfg.entry).chain(fun.cfg.named.values_mut()) {
                for (candidate, instr) in block.instructions.iter_mut() {
                    if *candidate == id {
                        *instr = Instruction::BoolConstant { value };
                        changes += 1;
                    }
                }
            }
        }
    }
    Ok(changes)
}

/// Independent check: the after program must be the before program with
/// exactly the reflexive comparisons (re-derived from the before program)
/// replaced by their constants, and nothing else.
pub fn verify(before: &Program, after: &Program, pointers: bool) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "fold_reflexive changed the set of functions",
    )?;
    for (name, before_fun) in &before.functions {
        let after_fun = after.functions.get(name).ok_or_else(|| {
            anyhow::anyhow!("fold_reflexive removed function {}", name.as_str())
        })?;
        let folded = foldable(before_fun, pointers);
        for key in super::blocks_sorted(&before_fun.cfg) {
            let block_name =
                key.as_ref().map(|l| l.as_str().to_string()).unwrap_or_else(|| "__entry".into());
            let before_block = super::get_block(&before_fun.cfg, &key).unwrap();
            let after_block = super::get_block(&after_fun.cfg, &key).ok_or_else(|| {
                anyhow::anyhow!(
                    "fold_reflexive removed block '{}' of {}",
                    block_name,
                    name.as_str()
                )
            })?;
            require(
                before_block.instructions.len() == after_block.instructions.len(),
                format!(
                    "fold_reflexive changed the length of '{}' in {}",
                    block_name,
                    name.as_str()
                ),
            )?;
            for ((before_id, before_instr), (after_id, after_instr)) in
                before_block.instructions.iter().zip(after_block.instructions.iter())
            {
                require(
                    before_id == after_id,
                    format!(
                        "fold_reflexive changed instruction id %{} of '{}' in {}",
                        usize::from(*before_id),
                        block_name,
                        name.as_str()
                    ),
                )?;
                let want = match folded.iter().find(|(id, _)| id == before_id) {
                    Some((_, value)) => &Instruction::BoolConstant { value: *value },
                    None => before_instr,
                };
                require(
                    want == after_instr,
                    format!(
                        "fold_reflexive changed %{} in '{}' of {}:\n  want {}\n  got  {}",
                        usize::from(*before_id),
                        block_name,
                        name.as_str(),
                        format_instruction(want),
                        format_instruction(after_instr)
                    ),
                )?;
            }
            require(
                before_block.terminator == after_block.terminator
                    && before_block.hint_normalize == after_block.hint_normalize,
                format!(
                    "fold_reflexive changed the terminator of '{}' in {}",
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
    use super::super::super::program::Program;
    use super::*;
    use crate::ir::{Block, Cfg, GlobalId, Terminator};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn program(instructions: Vec<(LocalId, Instruction)>) -> Program {
        let entry = Block {
            instructions,
            terminator: (id(99), Terminator::Return { value: None }),
            hint_normalize: false,
        };
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            arg_ids: vec![],
            capture_ids: vec![],
            cfg: Cfg::new(entry, crate::ir::new_label_map()),
            source_span: None,
        };
        let mut functions = IndexMap::new();
        functions.insert(fun.name.clone(), fun);
        Program { functions }
    }

    fn num(value: i16) -> Instruction {
        Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(value) }
    }

    fn cmp(x: usize, op: BinaryOp) -> Instruction {
        Instruction::BinaryOp { left: id(x), op, right: id(x) }
    }

    fn entry(p: &Program) -> Block {
        p.functions.values().next().unwrap().cfg.entry.clone()
    }

    #[test]
    fn folds_a_reflexive_comparison_on_a_number_constant() {
        let mut p = program(vec![
            (id(1), num(0)),
            (id(2), cmp(1, BinaryOp::GreaterThan)),
            (id(3), cmp(1, BinaryOp::LessThanEqual)),
        ]);
        let before = p.clone();
        assert_eq!(apply(&mut p, false).unwrap(), 2);
        let entry = &p.functions.values().next().unwrap().cfg.entry;
        assert_eq!(entry.instructions[1].1, Instruction::BoolConstant { value: false });
        assert_eq!(entry.instructions[2].1, Instruction::BoolConstant { value: true });
        verify(&before, &p, false).unwrap();
    }

    #[test]
    fn leaves_a_comparison_of_two_different_locals() {
        let mut p = program(vec![
            (id(1), num(0)),
            (id(2), num(0)),
            (id(3), Instruction::BinaryOp { left: id(1), op: BinaryOp::GreaterThan, right: id(2) }),
        ]);
        let before = entry(&p);
        assert_eq!(apply(&mut p, false).unwrap(), 0);
        assert_eq!(entry(&p), before);
    }

    #[test]
    fn leaves_a_reflexive_comparison_on_a_non_constant() {
        // %1 is an argument-like unknown: not defined by NumberConstant.
        let mut p = program(vec![
            (id(1), Instruction::Alloc),
            (id(2), cmp(1, BinaryOp::GreaterThan)),
        ]);
        let before = entry(&p);
        assert_eq!(apply(&mut p, false).unwrap(), 0);
        assert_eq!(entry(&p), before);
    }

    fn get_field(x: usize) -> Instruction {
        Instruction::GetField {
            receiver: id(x),
            field: "f".to_string(),
            create_if_missing: false,
        }
    }

    #[test]
    fn folds_equality_on_a_witnessed_pointer() {
        let mut p = program(vec![
            (id(1), Instruction::Alloc),
            (id(2), get_field(1)), // the witness
            (id(3), cmp(1, BinaryOp::TildeEqual)),
            (id(4), cmp(1, BinaryOp::TwoEqual)),
        ]);
        let before = p.clone();
        assert_eq!(apply(&mut p, true).unwrap(), 2);
        let entry = entry(&p);
        assert_eq!(entry.instructions[2].1, Instruction::BoolConstant { value: false });
        assert_eq!(entry.instructions[3].1, Instruction::BoolConstant { value: true });
        verify(&before, &p, true).unwrap();
    }

    #[test]
    fn leaves_equality_when_the_witness_comes_later() {
        let mut p = program(vec![
            (id(1), Instruction::Alloc),
            (id(3), cmp(1, BinaryOp::TildeEqual)),
            (id(2), get_field(1)), // witness after the compare: no dominance
        ]);
        let before = entry(&p);
        assert_eq!(apply(&mut p, true).unwrap(), 0);
        assert_eq!(entry(&p), before);
    }

    #[test]
    fn leaves_ordering_comparisons_on_a_witnessed_pointer() {
        // The witness decides equality only; `p < p` on pointers errors at
        // runtime and must keep doing so.
        let mut p = program(vec![
            (id(1), Instruction::Alloc),
            (id(2), get_field(1)),
            (id(3), cmp(1, BinaryOp::LessThan)),
        ]);
        let before = entry(&p);
        assert_eq!(apply(&mut p, true).unwrap(), 0);
        assert_eq!(entry(&p), before);
    }

    #[test]
    fn folds_nil_against_nil_and_pointer_against_nil() {
        let mut p = program(vec![
            (id(1), Instruction::NilConstant),
            (id(2), Instruction::NilConstant),
            (id(3), Instruction::Alloc),
            (id(4), get_field(3)),
            (
                id(5),
                Instruction::BinaryOp { left: id(1), op: BinaryOp::TwoEqual, right: id(2) },
            ),
            (
                id(6),
                Instruction::BinaryOp { left: id(3), op: BinaryOp::TildeEqual, right: id(1) },
            ),
            (
                id(7),
                Instruction::BinaryOp { left: id(2), op: BinaryOp::TwoEqual, right: id(3) },
            ),
        ]);
        let before = p.clone();
        assert_eq!(apply(&mut p, true).unwrap(), 3);
        let entry = entry(&p);
        assert_eq!(entry.instructions[4].1, Instruction::BoolConstant { value: true });
        assert_eq!(entry.instructions[5].1, Instruction::BoolConstant { value: true });
        assert_eq!(entry.instructions[6].1, Instruction::BoolConstant { value: false });
        verify(&before, &p, true).unwrap();
    }

    #[test]
    fn leaves_pointer_families_without_the_opt_in() {
        // The pre-opt-in behavior must replay byte-identically: a witnessed
        // pointer equality does not fold with `pointers: false`.
        let mut p = program(vec![
            (id(1), Instruction::Alloc),
            (id(2), get_field(1)),
            (id(3), cmp(1, BinaryOp::TwoEqual)),
        ]);
        let before = entry(&p);
        assert_eq!(apply(&mut p, false).unwrap(), 0);
        assert_eq!(entry(&p), before);
    }

    #[test]
    fn leaves_reflexive_arithmetic() {
        let mut p = program(vec![(id(1), num(2)), (id(2), cmp(1, BinaryOp::Plus))]);
        let before = entry(&p);
        assert_eq!(apply(&mut p, false).unwrap(), 0);
        assert_eq!(entry(&p), before);
    }

    #[test]
    fn is_idempotent() {
        let mut p = program(vec![(id(1), num(0)), (id(2), cmp(1, BinaryOp::LessThan))]);
        assert_eq!(apply(&mut p, false).unwrap(), 1);
        let once = entry(&p);
        assert_eq!(apply(&mut p, false).unwrap(), 0);
        assert_eq!(entry(&p), once);
    }

    #[test]
    fn verify_rejects_a_wrong_constant() {
        let mut p = program(vec![(id(1), num(0)), (id(2), cmp(1, BinaryOp::GreaterThan))]);
        let before = p.clone();
        apply(&mut p, false).unwrap();
        // Flip the folded constant.
        p.functions.values_mut().next().unwrap().cfg.entry.instructions[1].1 =
            Instruction::BoolConstant { value: true };
        assert!(verify(&before, &p, false).is_err());
    }

    #[test]
    fn verify_rejects_an_unrelated_change() {
        let mut p = program(vec![(id(1), num(0)), (id(2), cmp(1, BinaryOp::GreaterThan))]);
        let before = p.clone();
        apply(&mut p, false).unwrap();
        p.functions.values_mut().next().unwrap().cfg.entry.instructions[0].1 = num(1);
        assert!(verify(&before, &p, false).is_err());
    }
}
