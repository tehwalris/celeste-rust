//! `allocate_slots` - decide where each `LocalId` physically lives.
//!
//! Bulk rule: no location, deterministic, idempotent.
//!
//! This rule is unusual in that it changes no instructions at all. It only
//! rebuilds `Cfg::slots`, which is why its verifier is so short: everything
//! about whether the *new map* is legitimate is checked by `validate`, which
//! runs `slots::check` on every function after every rewrite. What is left for
//! the rule to prove is the thing `validate` cannot know - that this particular
//! rewrite touched nothing else.
//!
//! Being idempotent and IR-free also means it can be re-run after any rewrite
//! that introduces ids, which is the point: `inline` explodes the id range, and
//! without re-allocation that turns into an explosion in `LocalEnv` size.

use anyhow::Result;
use std::sync::Arc;

use super::super::program::Program;
use super::super::slots;
use super::require;

pub fn apply(program: &mut Program) -> Result<usize> {
    let mut changes = 0;
    for fun in program.functions.values_mut() {
        let map = Arc::new(slots::allocate(fun));
        if map.as_ref() != fun.cfg.slots.as_ref() {
            fun.cfg.slots = map;
            changes += 1;
        }
    }
    Ok(changes)
}

/// Independent check that nothing but the slot map changed.
///
/// Compares the two programs' instructions, terminators and block structure
/// directly. `validate` separately proves the new map is a valid one.
pub fn verify(before: &Program, after: &Program) -> Result<()> {
    require(
        before.functions.len() == after.functions.len(),
        "allocate_slots must not add or remove functions",
    )?;

    for (name, before_fun) in &before.functions {
        let after_fun = after
            .functions
            .get(name)
            .ok_or_else(|| anyhow::anyhow!("allocate_slots removed function {}", name.as_str()))?;

        require(
            before_fun.arg_ids == after_fun.arg_ids
                && before_fun.capture_ids == after_fun.capture_ids,
            format!("{}: allocate_slots changed the signature", name.as_str()),
        )?;
        require(
            super::super::print::format_function(before_fun)
                == super::super::print::format_function(after_fun),
            format!("{}: allocate_slots changed the CFG", name.as_str()),
        )?;
    }
    Ok(())
}
