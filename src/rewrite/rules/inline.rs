//! `inline` - splice a callee's body into a call site.
//!
//! Pointed rule: names the calling function, the call, and the callee.
//!
//! # Why this matters
//!
//! A `Call` is where the interpreter's cost multiplies. `BlockPostPhi` folds
//! instructions over a `Vec<State>`; at a call it runs the *entire nested
//! interpreter once per caller state*, and the callee may return several
//! states, multiplying everything after it in the block. At frame 37 the
//! profile shows 236,052 nested CFG interpretations for a frame that logically
//! calls each function a handful of times.
//!
//! # Soundness
//!
//! Not by analysis. The rewrite inserts `assert_closure %c is G` before the
//! spliced body, so if the call would ever have gone anywhere other than `G`
//! execution fails loudly instead of silently running the wrong code. Proving
//! that a dynamic call always reaches `G` would need a whole-program
//! "is this global ever reassigned" analysis, and the previous attempt's call
//! resolution simply assumed it - inferring the target by stripping digits off
//! a mangled name, with silent collisions.
//!
//! The guard is cheap: a closure pointer is a scalar rather than a per-lane
//! value, so it costs one check per state and never splits the state set.
//!
//! # Restriction: callees with no captures
//!
//! `CallResolved`-style capture passing is not implemented, so this only
//! handles callees whose `capture_ids` is empty. That is not as narrow as it
//! sounds: the frontend captures enclosing *locals*, and anything defined at
//! the top level - including `player.update` and friends, which live in table
//! constructors at the top level - has none. What it excludes is the closures
//! built inside `init_object` (`obj.move`, `obj.is_solid`, `obj.collide`),
//! which capture `obj`. Those are the hot ones and need a follow-up rule.

use anyhow::{anyhow, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use crate::ir::{Block, Cfg, FunDef, GlobalId, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::super::validate::{all_blocks, block_label, BlockKey};
use super::{label_of, require, LocalIdAllocator};

/// Finds the block and index of the instruction defining `call_id`.
fn find_call(cfg: &Cfg, call_id: LocalId) -> Option<(BlockKey, usize)> {
    for (key, block) in all_blocks(cfg) {
        for (index, (id, _)) in block.instructions.iter().enumerate() {
            if *id == call_id {
                return Some((key, index));
            }
        }
    }
    None
}

fn call_operands(instr: &Instruction) -> Option<(LocalId, Vec<LocalId>)> {
    match instr {
        Instruction::Call { closure, args } => Some((*closure, args.clone())),
        _ => None,
    }
}

/// Labels the rewrite generates. Derived from the recipe entry's stable id, so
/// they do not shift when another entry is inserted earlier in the recipe.
fn inlined_entry_label(rewrite_id: &str) -> Label {
    Label::from(format!("in_{}_entry", rewrite_id))
}
fn body_label(rewrite_id: &str, original: &str) -> Label {
    Label::from(format!("in_{}_{}", rewrite_id, original))
}
fn cont_label(rewrite_id: &str) -> Label {
    Label::from(format!("in_{}_cont", rewrite_id))
}

pub fn apply(
    program: &mut Program,
    rewrite_id: &str,
    function: &str,
    call_id: LocalId,
    callee_name: &str,
) -> Result<usize> {
    let callee = program.get(callee_name)?.clone();
    require(
        callee.capture_ids.is_empty(),
        format!(
            "inline: {} has {} capture(s); only capture-free callees are supported",
            callee_name,
            callee.capture_ids.len()
        ),
    )?;
    require(
        !super::super::program::is_synthetic(callee_name),
        "inline: the init and frame chunks are entry points, not callees",
    )?;

    let caller = program.get_mut(function)?;
    let (call_block_key, call_index) = find_call(&caller.cfg, call_id)
        .ok_or_else(|| anyhow!("inline: no instruction {} in {}", usize::from(call_id), function))?;
    let call_block = super::get_block(&caller.cfg, &call_block_key)
        .ok_or_else(|| anyhow!("inline: call block missing"))?;
    let (closure_id, args) = call_operands(&call_block.instructions[call_index].1)
        .ok_or_else(|| anyhow!("inline: %{} is not a Call", usize::from(call_id)))?;

    let mut ids = LocalIdAllocator::for_function(caller);

    // --- rename map for the callee's locals ---
    // Parameters are bound directly to the caller's argument values rather than
    // to fresh locals, which avoids needing a copy instruction (the IR has
    // none). Missing arguments become a fresh `nil` in the caller's block.
    let mut rename: FxHashMap<LocalId, LocalId> = FxHashMap::default();
    let mut prelude: Vec<(LocalId, Instruction)> = Vec::new();
    for (position, arg_slot) in callee.arg_ids.iter().enumerate() {
        let Some(param) = arg_slot else { continue };
        match args.get(position) {
            Some(actual) => {
                rename.insert(*param, *actual);
            }
            None => {
                let nil = ids.fresh();
                prelude.push((nil, Instruction::NilConstant));
                rename.insert(*param, nil);
            }
        }
    }
    for (key, block) in all_blocks(&callee.cfg) {
        let _ = key;
        for (id, _) in &block.instructions {
            rename.entry(*id).or_insert_with(|| ids.fresh());
        }
        rename.entry(block.terminator_id()).or_insert_with(|| ids.fresh());
    }
    let resolve = |id: LocalId| *rename.get(&id).unwrap_or(&id);

    // --- label map for the callee's blocks ---
    let entry_target = inlined_entry_label(rewrite_id);
    let cont = cont_label(rewrite_id);
    let mut label_map: FxHashMap<String, Label> = FxHashMap::default();
    label_map.insert("__entry".to_string(), entry_target.clone());
    for (key, _) in all_blocks(&callee.cfg) {
        if let Some(label) = key {
            label_map.insert(
                label.as_str().to_string(),
                body_label(rewrite_id, label.as_str()),
            );
        }
    }
    let map_label = |label: &Label| -> Label {
        label_map
            .get(label.as_str())
            .cloned()
            .unwrap_or_else(|| label.clone())
    };

    // --- split the caller's block ---
    let call_block = super::get_block_mut(&mut caller.cfg, &call_block_key)
        .ok_or_else(|| anyhow!("inline: call block missing"))?;
    let tail: Vec<(LocalId, Instruction)> =
        call_block.instructions.split_off(call_index + 1);
    // Drop the Call itself and put the guard in its place.
    call_block.instructions.pop();
    call_block.instructions.extend(prelude);
    let guard_id = ids.fresh();
    call_block.instructions.push((
        guard_id,
        Instruction::AssertClosure {
            value: closure_id,
            fun_def: GlobalId::from(callee_name.to_string()),
        },
    ));
    let original_terminator = std::mem::replace(
        &mut call_block.terminator,
        (
            ids.fresh(),
            Terminator::UnconditionalBranch { target: entry_target.clone() },
        ),
    );

    // --- copy the callee's blocks, turning returns into branches ---
    let mut return_sources: Vec<(Label, LocalId)> = Vec::new();
    let mut new_blocks: Vec<(Label, Block)> = Vec::new();
    for (key, block) in all_blocks(&callee.cfg) {
        let label = match &key {
            None => entry_target.clone(),
            Some(l) => map_label(l),
        };
        let mut instructions: Vec<(LocalId, Instruction)> = block
            .instructions
            .iter()
            .map(|(id, instr)| {
                let mut copied = instr.map_local_ids(resolve);
                if let Instruction::Phi { branches } = &mut copied {
                    for (branch_label, _) in branches.iter_mut() {
                        *branch_label = map_label(branch_label);
                    }
                }
                (resolve(*id), copied)
            })
            .collect();

        let terminator = match block.terminator_kind() {
            Terminator::Return { value } => {
                let produced = match value {
                    Some(v) => resolve(*v),
                    None => {
                        let nil = ids.fresh();
                        instructions.push((nil, Instruction::NilConstant));
                        nil
                    }
                };
                return_sources.push((label.clone(), produced));
                Terminator::UnconditionalBranch { target: cont.clone() }
            }
            Terminator::UnconditionalBranch { target } => {
                Terminator::UnconditionalBranch { target: map_label(target) }
            }
            Terminator::ConditionalBranch { condition, true_target, false_target } => {
                Terminator::ConditionalBranch {
                    condition: resolve(*condition),
                    true_target: map_label(true_target),
                    false_target: map_label(false_target),
                }
            }
        };

        new_blocks.push((
            label,
            Block {
                instructions,
                terminator: (resolve(block.terminator_id()), terminator),
                hint_normalize: block.hint_normalize,
            },
        ));
    }

    require(
        !return_sources.is_empty(),
        format!("inline: {} has no return points", callee_name),
    )?;

    // --- the continuation block ---
    // The call's own id becomes a phi over the callee's return values, so
    // nothing downstream needs rewriting.
    let mut cont_instructions: Vec<(LocalId, Instruction)> = vec![(
        call_id,
        Instruction::Phi { branches: return_sources.clone() },
    )];
    cont_instructions.extend(tail);
    let cont_block = Block {
        instructions: cont_instructions,
        terminator: original_terminator,
        hint_normalize: false,
    };

    for (label, block) in new_blocks {
        caller.cfg.named.insert(label, block);
    }
    caller.cfg.named.insert(cont.clone(), cont_block);

    // --- phis in the original block's successors now come from `cont` ---
    // The original terminator moved, so any phi that named the call's block as
    // a predecessor must name the continuation instead. Missing this is what
    // made the previous inliner produce CFGs the interpreter panicked on.
    let original_label = label_of(&call_block_key);
    super::rename_phi_label(&mut caller.cfg, &original_label, &cont);
    // ...but not inside the continuation itself, whose phi legitimately refers
    // to the callee's return blocks.
    if let Some(block) = caller.cfg.named.get_mut(&cont) {
        if let Some((_, Instruction::Phi { branches })) = block.instructions.first_mut() {
            *branches = return_sources;
        }
    }

    Ok(1)
}

/// Independent check.
///
/// Confirms the guard is present and names the callee, that the callee is
/// capture-free, and that the spliced blocks are a faithful copy of the
/// callee's - matched up by walking both CFGs from their entries in parallel,
/// so the correspondence is derived from the result rather than taken on trust.
pub fn verify(
    before: &Program,
    after: &Program,
    rewrite_id: &str,
    function: &str,
    call_id: LocalId,
    callee_name: &str,
) -> Result<()> {
    let before_caller = before.get(function)?;
    let after_caller = after.get(function)?;
    let callee = before.get(callee_name)?;

    require(
        callee.capture_ids.is_empty(),
        format!("inline: {} has captures", callee_name),
    )?;

    // The call must have existed, and must be gone.
    let (call_block_key, call_index) = find_call(&before_caller.cfg, call_id)
        .ok_or_else(|| anyhow!("inline: %{} was not in {}", usize::from(call_id), function))?;
    let before_block = super::get_block(&before_caller.cfg, &call_block_key)
        .ok_or_else(|| anyhow!("inline: call block missing"))?;
    let (closure_id, _args) = call_operands(&before_block.instructions[call_index].1)
        .ok_or_else(|| anyhow!("inline: %{} was not a Call", usize::from(call_id)))?;

    for block in after_caller.cfg.iter_blocks() {
        for (_, instr) in &block.instructions {
            require(
                !matches!(instr, Instruction::Call { closure, .. } if *closure == closure_id
                          && block.instructions.iter().any(|(i, _)| *i == call_id)),
                "inline: the original call is still present",
            )?;
        }
    }

    // The guard must be in the block the call was in, on the same closure value.
    let after_call_block = super::get_block(&after_caller.cfg, &call_block_key)
        .ok_or_else(|| anyhow!("inline: the call's block disappeared"))?;
    let has_guard = after_call_block.instructions.iter().any(|(_, instr)| {
        matches!(instr, Instruction::AssertClosure { value, fun_def }
                 if *value == closure_id && fun_def.as_str() == callee_name)
    });
    require(
        has_guard,
        format!(
            "inline: no `assert_closure {} is {}` in '{}' - without it the \
             rewrite is an unproven assumption",
            usize::from(closure_id),
            callee_name,
            block_label(&call_block_key)
        ),
    )?;

    // The block must now jump into the spliced entry.
    let entry_target = inlined_entry_label(rewrite_id);
    require(
        matches!(after_call_block.terminator_kind(),
                 Terminator::UnconditionalBranch { target } if *target == entry_target),
        format!(
            "inline: '{}' does not branch to the spliced body",
            block_label(&call_block_key)
        ),
    )?;

    // The spliced body must be a faithful copy. Walk both CFGs from their
    // entries, pairing successors positionally.
    let cont = cont_label(rewrite_id);
    let mut pairing: FxHashMap<String, BlockKey> = FxHashMap::default();
    let mut queue: Vec<(Label, BlockKey)> = vec![(entry_target.clone(), None)];
    let mut seen: FxHashSet<String> = FxHashSet::default();

    while let Some((copy_label, original_key)) = queue.pop() {
        if !seen.insert(copy_label.as_str().to_string()) {
            continue;
        }
        pairing.insert(copy_label.as_str().to_string(), original_key.clone());

        let copy = after_caller.cfg.named.get(&copy_label).ok_or_else(|| {
            anyhow!("inline: spliced block '{}' is missing", copy_label.as_str())
        })?;
        let original = super::get_block(&callee.cfg, &original_key)
            .ok_or_else(|| anyhow!("inline: callee block missing"))?;

        require(
            copy.hint_normalize == original.hint_normalize,
            format!(
                "inline: '{}' lost hint_normalize from the callee",
                copy_label.as_str()
            ),
        )?;

        // Instruction kinds must line up. Operand ids differ by the renaming, so
        // compare shapes rather than exact ids; the dominance validator plus the
        // differential run cover the rest.
        let original_returns = matches!(original.terminator_kind(), Terminator::Return { .. });
        let extra = usize::from(
            original_returns && matches!(original.terminator_kind(), Terminator::Return { value: None }),
        );
        require(
            copy.instructions.len() == original.instructions.len() + extra,
            format!(
                "inline: '{}' has {} instructions, the callee's block has {}",
                copy_label.as_str(),
                copy.instructions.len(),
                original.instructions.len()
            ),
        )?;
        for ((_, copied), (_, source)) in copy.instructions.iter().zip(&original.instructions) {
            require(
                std::mem::discriminant(copied) == std::mem::discriminant(source),
                format!(
                    "inline: '{}' contains {} where the callee has {}",
                    copy_label.as_str(),
                    super::super::print::format_instruction(copied),
                    super::super::print::format_instruction(source)
                ),
            )?;
        }

        match (copy.terminator_kind(), original.terminator_kind()) {
            (Terminator::UnconditionalBranch { target }, Terminator::Return { .. }) => {
                require(
                    *target == cont,
                    format!(
                        "inline: return in '{}' does not branch to the continuation",
                        copy_label.as_str()
                    ),
                )?;
            }
            (
                Terminator::UnconditionalBranch { target: copy_target },
                Terminator::UnconditionalBranch { target: original_target },
            ) => queue.push((copy_target.clone(), Some(original_target.clone()))),
            (
                Terminator::ConditionalBranch {
                    true_target: ct, false_target: cf, ..
                },
                Terminator::ConditionalBranch {
                    true_target: ot, false_target: of, ..
                },
            ) => {
                queue.push((ct.clone(), Some(ot.clone())));
                queue.push((cf.clone(), Some(of.clone())));
            }
            (c, o) => {
                return Err(anyhow!(
                    "inline: '{}' terminator {} does not correspond to the callee's {}",
                    copy_label.as_str(),
                    super::super::print::format_terminator(c),
                    super::super::print::format_terminator(o)
                ))
            }
        }
    }

    // Every reachable block of the callee must have been copied.
    let reachable = super::reachable_blocks(&callee.cfg);
    require(
        pairing.len() >= reachable.len(),
        format!(
            "inline: copied {} blocks but {} are reachable in {}",
            pairing.len(),
            reachable.len(),
            callee_name
        ),
    )?;

    // The call's id must now be a phi in the continuation, so downstream uses
    // still see the return value.
    let cont_block = after_caller
        .cfg
        .named
        .get(&cont)
        .ok_or_else(|| anyhow!("inline: no continuation block"))?;
    require(
        matches!(cont_block.instructions.first(),
                 Some((id, Instruction::Phi { .. })) if *id == call_id),
        "inline: the continuation does not begin with a phi bound to the call's id",
    )?;

    Ok(())
}

/// Which top-level function each global holds, derived structurally from the
/// init chunk by looking for
///
/// ```text
///   %c = alloc
///        store_closure %c <- G []
///   %g = get_global "name" create
///        store %g <- %c
/// ```
///
/// The previous attempt inferred this by stripping a `_<digits>` suffix off the
/// mangled `GlobalId` and assuming the result was the global's name, which
/// silently collided whenever two functions shared a base name. Reading the
/// code instead costs nothing and cannot collide.
///
/// This is only a *suggestion* mechanism, so it does not have to be complete or
/// even correct - every inline it proposes is guarded by an `assert_closure`,
/// and the rule's verifier insists on that guard.
pub fn global_closure_map(program: &Program) -> FxHashMap<String, GlobalId> {
    let mut out = FxHashMap::default();
    let Ok(init) = program.get(super::super::program::INIT_FN) else {
        return out;
    };
    for (_, block) in all_blocks(&init.cfg) {
        // cell -> closure defined into it
        let mut closures: FxHashMap<LocalId, GlobalId> = FxHashMap::default();
        let mut globals: FxHashMap<LocalId, String> = FxHashMap::default();
        for (id, instr) in &block.instructions {
            match instr {
                Instruction::StoreClosure { target, fun_def, captures } if captures.is_empty() => {
                    closures.insert(*target, fun_def.clone());
                }
                Instruction::GetGlobal { name, .. } => {
                    globals.insert(*id, name.clone());
                }
                Instruction::Store { target, source } => {
                    if let (Some(name), Some(fun)) = (globals.get(target), closures.get(source)) {
                        out.insert(name.clone(), fun.clone());
                    }
                }
                _ => {}
            }
        }
    }
    out
}

/// Globals that hold a Lua function at compile time but a native builtin at
/// run time, so the Lua body must not be spliced in.
///
/// `game_runner::inject_tile_flag_at_builtin` overwrites the global right after
/// init, swapping the interpreted tile lookup for one backed by the precomputed
/// collision cache. Suggesting it is not *unsound* - the `assert_closure` guard
/// catches it and execution fails loudly, which is how this list was
/// discovered - but it is a guaranteed failure, so do not propose it.
///
/// This is also the concrete counterexample to "a global holding a function is
/// never reassigned", which the previous attempt's call resolution assumed
/// without checking.
const REPLACED_AT_RUNTIME: &[&str] = &["tile_flag_at"];

/// Call sites in `fun` whose target can be guessed, and whose callee takes no
/// captures.
pub fn candidates(program: &Program, fun: &FunDef) -> Vec<(LocalId, String)> {
    let map = global_closure_map(program);
    let mut out = Vec::new();
    for (_, block) in all_blocks(&fun.cfg) {
        // Within a block, track `%g = get_global "n"` and `%c = load %g`.
        let mut global_of: FxHashMap<LocalId, String> = FxHashMap::default();
        let mut loaded_global: FxHashMap<LocalId, String> = FxHashMap::default();
        for (id, instr) in &block.instructions {
            match instr {
                Instruction::GetGlobal { name, .. } => {
                    global_of.insert(*id, name.clone());
                }
                Instruction::Load { source } => {
                    if let Some(name) = global_of.get(source) {
                        loaded_global.insert(*id, name.clone());
                    }
                }
                Instruction::Call { closure, .. } => {
                    let Some(name) = loaded_global.get(closure) else { continue };
                    if REPLACED_AT_RUNTIME.contains(&name.as_str()) {
                        continue;
                    }
                    let Some(callee) = map.get(name) else { continue };
                    let Ok(callee_def) = program.get(callee.as_str()) else { continue };
                    if !callee_def.capture_ids.is_empty() {
                        continue;
                    }
                    out.push((*id, callee.as_str().to_string()));
                }
                _ => {}
            }
        }
    }
    out.sort_by_key(|(id, _)| usize::from(*id));
    out
}
