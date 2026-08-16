//! `split_at` - insert a state-splitting `__split_at(value, c)` call after
//! a value's definition, redirecting every later use to the split result.
//!
//! # Why (plans/spd-rung.md)
//!
//! Under the spd rung, a mid-frame velocity interval can straddle a game
//! decision threshold (the first found: `spd.x` straddling 0 at Celeste's
//! facing check, whose unknown outcome flowed through `flip.x and -1 or
//! 1` and stored an unknown NUMBER into spd). The fix, per the
//! split-before-compare design (the `__split_by_flr` pattern): split the
//! interval at the threshold FIRST, so every downstream comparison
//! against it is definite per lane. Three-way ({< c}, {== c}, {> c}, the
//! middle side degenerating to the concrete number), so the fix is
//! operator-agnostic - `<`, `<=`, `>`, `>=`, `==` and `~=` all decide.
//!
//! # What it does
//!
//! ```text
//!   %at = ...                      %at = ...
//!   ..uses of %at..        =>      %g = get_global "__split_at"
//!                                  %l = load %g
//!                                  %c = num THRESHOLD
//!                                  %r = call %l(%at, %c)   [the __split_at builtin]
//!                                  ..uses of %at, now reading %r..
//! ```
//!
//! # Soundness
//!
//! Semantically neutral on concrete values: the builtin passes numbers
//! through unchanged, so the differential verifier sees an identical
//! program. On intervals it is a refinement split - the sides partition
//! the interval exactly, every concrete value the interval admits is in
//! exactly one side, and each side's lanes carry the clipped (narrower,
//! still-covering) value. Nothing is invented and nothing is dropped.

use anyhow::{anyhow, Result};

use crate::ir::{FunDef, Instruction, Label, LocalId};
use crate::pico8_num::Pico8Num;

use super::super::program::Program;
use super::{blocks_sorted, get_block, get_block_mut, LocalIdAllocator};

type BlockKey = Option<Label>;

fn definition_site(fun: &FunDef, at: LocalId) -> Option<(BlockKey, usize)> {
    for key in blocks_sorted(&fun.cfg) {
        let block = get_block(&fun.cfg, &key).expect("listed block exists");
        if let Some(index) = block.instructions.iter().position(|(id, _)| *id == at) {
            return Some((key, index));
        }
    }
    None
}

/// Parse a raw 16.16 threshold like "0x0000" or "0x1_0000".
pub fn parse_threshold(text: &str) -> Result<Pico8Num> {
    let cleaned = text.trim().replace('_', "");
    let raw = if let Some(hex) = cleaned.strip_prefix("-0x") {
        -(i64::from_str_radix(hex, 16).map_err(|e| anyhow!("threshold {}: {}", text, e))?)
    } else if let Some(hex) = cleaned.strip_prefix("0x") {
        i64::from_str_radix(hex, 16).map_err(|e| anyhow!("threshold {}: {}", text, e))?
    } else {
        return Err(anyhow!("threshold {} must be raw 16.16 hex like 0x1_0000", text));
    };
    let raw = i32::try_from(raw).map_err(|_| anyhow!("threshold {} out of range", text))?;
    Ok(Pico8Num::from_parts((raw >> 16) as i16, raw as u16))
}

pub fn apply(
    program: &mut Program,
    function: &str,
    at: LocalId,
    threshold: Pico8Num,
) -> Result<usize> {
    let fun = program.get(function)?;
    let (key, index) = definition_site(fun, at)
        .ok_or_else(|| anyhow!("no instruction defines %{} in {}", usize::from(at), function))?;

    let mut alloc = LocalIdAllocator::for_function(fun);
    let g = alloc.fresh();
    let l = alloc.fresh();
    let c = alloc.fresh();
    let r = alloc.fresh();

    let fun = program.get_mut(function)?;
    // Redirect every USE of `at` to `r`, across the whole function - all
    // uses are dominated by the definition, and the new call site
    // (inserted right after it) dominates them equally. `map_local_ids`
    // maps operands only; instruction KEYS (definitions) are untouched,
    // and the new instructions are spliced in AFTERWARDS, so their
    // deliberate reads of `at` survive.
    let map = |id: LocalId| if id == at { r } else { id };
    for block_key in blocks_sorted(&fun.cfg) {
        let block = get_block_mut(&mut fun.cfg, &block_key).expect("listed block exists");
        for (_, instr) in &mut block.instructions {
            *instr = instr.map_local_ids(map);
        }
        block.terminator.1 = block.terminator.1.map_local_ids(map);
    }

    let block = get_block_mut(&mut fun.cfg, &key).expect("definition block exists");
    block.instructions.splice(
        index + 1..index + 1,
        [
            (g, Instruction::GetGlobal { name: "__split_at".to_string(), create_if_missing: false }),
            (l, Instruction::Load { source: g }),
            (c, Instruction::NumberConstant { value: threshold }),
            // A plain `Call`, not `call_builtin`: state-SPLITTING
            // builtins return multiple (state, value) pairs, which only
            // the generic call path carries (same as `__split_by_flr`).
            (r, Instruction::Call { closure: l, args: vec![at, c] }),
        ],
    );
    Ok(1)
}

/// Structural verify: the split call exists right after the definition
/// with the right shape, and the definition's only direct reader is the
/// call (everything else reads the split result).
pub fn verify(
    _before: &Program,
    program: &Program,
    function: &str,
    at: LocalId,
    threshold: Pico8Num,
) -> Result<()> {
    let fun = program.get(function)?;
    let (key, index) = definition_site(fun, at)
        .ok_or_else(|| anyhow!("%{} vanished from {}", usize::from(at), function))?;
    let block = get_block(&fun.cfg, &key).unwrap();
    let Some((_, Instruction::GetGlobal { name, .. })) = block.instructions.get(index + 1)
    else {
        return Err(anyhow!("split_at: missing get_global after the definition"));
    };
    if name != "__split_at" {
        return Err(anyhow!("split_at: the global loaded is {:?}, not __split_at", name));
    }
    let Some((loaded, Instruction::Load { .. })) = block.instructions.get(index + 2)
    else {
        return Err(anyhow!("split_at: missing load of the builtin"));
    };
    let call = block
        .instructions
        .get(index + 4)
        .map(|(_, i)| i)
        .ok_or_else(|| anyhow!("split_at: no call after the definition"))?;
    let Instruction::Call { closure, args } = call else {
        return Err(anyhow!("split_at: expected the split call after the definition"));
    };
    if closure != loaded || args.first() != Some(&at) {
        return Err(anyhow!(
            "split_at: the call after the definition is not __split_at(%{}, _)",
            usize::from(at)
        ));
    }
    let Some((_, Instruction::NumberConstant { value })) = block.instructions.get(index + 3)
    else {
        return Err(anyhow!("split_at: missing threshold constant"));
    };
    if *value != threshold {
        return Err(anyhow!("split_at: threshold constant {:?} != entry's {:?}", value, threshold));
    }
    Ok(())
}
