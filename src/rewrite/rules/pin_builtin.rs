//! `pin_builtin` - name the callee of a call to a pure builtin.
//!
//! # Why
//!
//! A `Call` says nothing about what it calls, so `if_convert` has to assume the
//! worst and refuse to speculate it. That is usually right - a Lua callee can do
//! anything - but four of the eight branch sites worth converting are blocked by
//! a call to `max`:
//!
//! ```text
//!   %1341 = get_global "max"
//!   %1342 = load %1341
//!   %1343 = %238 - %248
//!   %1344 = call %1342(%1343, %243)      <- the only thing in the arm that is
//!   %1345 = br in_i1_075_and_or_join_603     not already speculatable
//! ```
//!
//! Those four sites are the same source construct inlined four times, and they
//! split the state 2704 times over 34 frames - 13.8% of all splits in the
//! program. Nothing else in those arms is in the way.
//!
//! # What it does
//!
//! Replaces the `call` with a `call_builtin` naming the callee:
//!
//! ```text
//!   %1344 = call_builtin "max" via %1342(%1343, %243)
//! ```
//!
//! Same operands, same implementation - `fixed_env::add_pure_builtin` registers
//! one function under both signatures, so a `call` and a pinned `call_builtin`
//! of the same name cannot compute different things. The only new behaviour is
//! the assertion that `%1342` really is `BuiltinFun("max")`, which is the usual
//! bargain: `inline` asserts its callee, `demote_create` asserts its field, and
//! this asserts its builtin.
//!
//! # What makes it safe
//!
//! Two things, and the second is why the rule is narrow.
//!
//! The assertion covers the callee. Celeste never reassigns `max`, but this rule
//! does not know that and does not try to prove it - if the global is ever
//! something else, the run stops and says so.
//!
//! The whitelist covers the callee's *behaviour*. `name` must be in
//! `fixed_env::PURE_BUILTINS`, which is the claim that the implementation
//! returns the state untouched, yields exactly one result, and reads nothing
//! outside its arguments. Both halves of this rule check it, because it is an
//! invariant several other passes lean on: `if_convert` speculates a
//! `CallBuiltin` on the strength of it, and `cse` does not treat one as a
//! barrier. A `CallBuiltin` naming `add` or `__split_by_flr` would quietly
//! break both.

use anyhow::{anyhow, Result};

use crate::interpreter::fixed_env::{is_pure_builtin, PURE_BUILTINS};
use crate::ir::{FunDef, Instruction, LocalId};

use super::super::program::Program;
use super::super::validate::{all_blocks, block_label, BlockKey};
use super::{get_block, get_block_mut, require};

/// Where an id is defined: which block, and at what position in it.
fn definition_site(fun: &FunDef, at: LocalId) -> Option<(BlockKey, usize)> {
    for (key, block) in all_blocks(&fun.cfg) {
        if let Some(index) = block.instructions.iter().position(|(id, _)| *id == at) {
            return Some((key, index));
        }
    }
    None
}

/// The pinned form of a `call`, if `name` is a builtin this rule may pin to.
fn pinned(instr: &Instruction, name: &str) -> Result<Instruction> {
    let Instruction::Call { closure, args } = instr else {
        return Err(anyhow!(
            "pin_builtin names an instruction that is not a call, it is `{}`",
            super::super::print::format_instruction(instr)
        ));
    };
    require(
        is_pure_builtin(name),
        format!(
            "{:?} is not one of the pure builtins {:?}. Pinning to anything else \
             would let `if_convert` speculate it and `cse` see through it.",
            name, PURE_BUILTINS
        ),
    )?;
    Ok(Instruction::CallBuiltin {
        callee: *closure,
        name: name.to_string(),
        args: args.clone(),
    })
}

pub fn apply(program: &mut Program, function: &str, at: LocalId, name: &str) -> Result<usize> {
    let fun = program.get(function)?;
    let (key, index) = definition_site(fun, at)
        .ok_or_else(|| anyhow!("no instruction defines %{} in {}", usize::from(at), function))?;
    let replacement = pinned(&get_block(&fun.cfg, &key).unwrap().instructions[index].1, name)?;

    let fun = program.get_mut(function)?;
    get_block_mut(&mut fun.cfg, &key).unwrap().instructions[index].1 = replacement;
    Ok(1)
}

/// Independent check.
///
/// Stated as properties of the two programs rather than as a second copy of the
/// transformation: exactly one instruction changed, it was a `call`, it became a
/// `call_builtin` on the same callee and the same arguments, and the name is one
/// this rule is allowed to pin to.
pub fn verify(
    before: &Program,
    after: &Program,
    function: &str,
    at: LocalId,
    name: &str,
) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "pin_builtin changed the set of functions",
    )?;
    for (other, before_fun) in &before.functions {
        if other.as_str() == function {
            continue;
        }
        require(
            after.functions.get(other) == Some(before_fun),
            format!("pin_builtin on {} also changed {}", function, other.as_str()),
        )?;
    }

    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let (key, index) = definition_site(before_fun, at)
        .ok_or_else(|| anyhow!("no instruction defines %{} in {}", usize::from(at), function))?;

    let before_blocks = all_blocks(&before_fun.cfg);
    require(
        before_blocks.len() == all_blocks(&after_fun.cfg).len(),
        "pin_builtin changed the set of blocks",
    )?;
    for (block_key, before_block) in &before_blocks {
        let after_block = get_block(&after_fun.cfg, block_key)
            .ok_or_else(|| anyhow!("pin_builtin removed block '{}'", block_label(block_key)))?;
        require(
            format!("{:?}", before_block.terminator) == format!("{:?}", after_block.terminator),
            format!(
                "pin_builtin changed the terminator of '{}'",
                block_label(block_key)
            ),
        )?;
        require(
            before_block.instructions.len() == after_block.instructions.len(),
            format!(
                "pin_builtin changed the length of '{}'",
                block_label(block_key)
            ),
        )?;
        for (position, (before_id, before_one)) in before_block.instructions.iter().enumerate() {
            let (after_id, after_one) = &after_block.instructions[position];
            require(
                after_id == before_id,
                format!("pin_builtin reordered '{}'", block_label(block_key)),
            )?;
            let want = if block_key == &key && position == index {
                pinned(before_one, name)?
            } else {
                before_one.clone()
            };
            require(
                &want == after_one,
                format!(
                    "pin_builtin changed %{} in '{}':\n  want {}\n  got  {}",
                    usize::from(*before_id),
                    block_label(block_key),
                    super::super::print::format_instruction(&want),
                    super::super::print::format_instruction(after_one)
                ),
            )?;
        }
    }
    Ok(())
}

/// Call sites whose callee is a global that a pure builtin is bound to, and
/// which currently block an `if_convert` triangle.
///
/// Narrow twice over. Only pure builtins, because those are the only names the
/// instruction may carry; and only blocking sites, because pinning is a claim
/// about the run and there is no reason to make one that buys nothing.
///
/// The callee is recognised syntactically - `%f = get_global "max"` then
/// `%g = load %f` then `call %g(..)` - which is a guess about what the recipe
/// author means, not a proof. The assertion is what makes a wrong guess loud.
pub fn candidates(program: &Program) -> Vec<(String, LocalId, String)> {
    let mut out = Vec::new();
    for (function, fun) in &program.functions {
        // Globals loaded in this function, by the local holding the loaded value.
        let mut builtin_of: rustc_hash::FxHashMap<LocalId, String> = Default::default();
        for (_, block) in all_blocks(&fun.cfg) {
            let mut cell_of: rustc_hash::FxHashMap<LocalId, String> = Default::default();
            for (id, instr) in &block.instructions {
                match instr {
                    Instruction::GetGlobal { name, create_if_missing: false }
                        if is_pure_builtin(name) =>
                    {
                        cell_of.insert(*id, name.clone());
                    }
                    Instruction::Load { source } => {
                        if let Some(name) = cell_of.get(source) {
                            builtin_of.insert(*id, name.clone());
                        }
                    }
                    _ => {}
                }
            }
        }
        for (_, blocking) in super::if_convert::blocking_instructions(fun) {
            for (id, instr) in blocking {
                let Instruction::Call { closure, .. } = &instr else { continue };
                let Some(name) = builtin_of.get(closure) else { continue };
                out.push((function.as_str().to_string(), id, name.clone()));
            }
        }
    }
    out.sort();
    out.dedup();
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, GlobalId, Terminator};
    use crate::rewrite::print::format_function;
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn program_of(instructions: Vec<(LocalId, Instruction)>) -> Program {
        let fun = FunDef {
            name: GlobalId::from("f".to_string()),
            capture_ids: vec![],
            arg_ids: vec![Some(id(2))],
            cfg: Cfg::new(
                Block {
                    instructions,
                    terminator: (id(900), Terminator::Return { value: None }),
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

    fn call_site() -> Vec<(LocalId, Instruction)> {
        vec![
            (
                id(10),
                Instruction::GetGlobal { name: "max".to_string(), create_if_missing: false },
            ),
            (id(11), Instruction::Load { source: id(10) }),
            (id(12), Instruction::Call { closure: id(11), args: vec![id(2), id(2)] }),
        ]
    }

    #[test]
    fn pins_a_call_to_its_builtin() {
        let mut p = program_of(call_site());
        let before = p.clone();
        assert_eq!(apply(&mut p, "f", id(12), "max").unwrap(), 1);
        verify(&before, &p, "f", id(12), "max").unwrap();
        let text = format_function(p.get("f").unwrap());
        assert!(
            text.contains(r#"%12 = call_builtin "max" via %11(%2, %2)"#),
            "{}",
            text
        );
    }

    /// The whitelist is an invariant other passes lean on, so both halves of
    /// the rule enforce it rather than trusting the recipe.
    #[test]
    fn refuses_a_builtin_that_is_not_pure() {
        let mut p = program_of(call_site());
        for name in ["add", "__split_by_flr", "mget", "not_a_builtin"] {
            let error = apply(&mut p, "f", id(12), name).unwrap_err().to_string();
            assert!(error.contains("not one of the pure builtins"), "{}: {}", name, error);
        }
    }

    #[test]
    fn refuses_an_instruction_that_is_not_a_call() {
        let mut p = program_of(call_site());
        let error = apply(&mut p, "f", id(11), "max").unwrap_err().to_string();
        assert!(error.contains("not a call"), "{}", error);
    }

    /// `verify` must reject an applier that pinned to a different name than the
    /// recipe asked for - that is the difference between a checked claim and a
    /// guessed one.
    #[test]
    fn verify_rejects_a_different_name() {
        let mut p = program_of(call_site());
        let before = p.clone();
        apply(&mut p, "f", id(12), "min").unwrap();
        let error = verify(&before, &p, "f", id(12), "max").unwrap_err().to_string();
        assert!(error.contains("changed %12"), "{}", error);
    }

    /// ...or that changed the operands while it was there.
    #[test]
    fn verify_rejects_changed_operands() {
        let mut p = program_of(call_site());
        let before = p.clone();
        apply(&mut p, "f", id(12), "max").unwrap();
        p.get_mut("f").unwrap().cfg.entry.instructions[2].1 = Instruction::CallBuiltin {
            callee: id(11),
            name: "max".to_string(),
            args: vec![id(2)],
        };
        assert!(verify(&before, &p, "f", id(12), "max").is_err());
    }

    /// A pinned call must be speculatable - that is the entire point of the
    /// rule - and an unpinned one must not be.
    #[test]
    fn pinning_makes_the_call_speculatable() {
        let call = Instruction::Call { closure: id(11), args: vec![id(2)] };
        assert!(!super::super::if_convert::is_speculatable(&call));
        let pinned = pinned(&call, "max").unwrap();
        assert!(super::super::if_convert::is_speculatable(&pinned));
    }
}
