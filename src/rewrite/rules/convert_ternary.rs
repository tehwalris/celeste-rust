//! `convert_ternary` - turn the two joins of `a and b or c` into one select.
//!
//! # The shape
//!
//! Lua's ternary idiom compiles to a *pair* of triangles sharing a block: the
//! `and`'s join is the `or`'s head. `appr`, inlined four times, is the whole
//! hot population:
//!
//! ```text
//!   H:    %c = %val > %target
//!         br %c ? A1 : M
//!   A1:   %b = call_builtin "max" via ..(..)     <- the `and` arm
//!         br M
//!   M:    %p = phi [H: %c, A1: %b]               <- the `and` join
//!         br %p ? J : A2
//!   A2:   %q = call_builtin "min" via ..(..)     <- the `or` arm
//!         br J
//!   J:    %r = phi [M: %p, A2: %q]               <- the `or` join
//! ```
//!
//! # Why `if_convert` cannot take these one at a time
//!
//! Converting `M` alone materialises `%p = select %c ? %b : %c` - a Number on
//! one side, the Bool `%c` on the other, and `Select` carries one type tag per
//! value, not per lane. It fails, correctly: the mixed value genuinely cannot
//! be represented. But `%p` never escapes with the Bool in it. The `or` takes
//! `%p` only when it is truthy, and truthy means it came from the arm, so it
//! is the Number. Converted *as a pair* the mixed value never needs to exist:
//!
//! ```text
//!   J:    %r = select %c ? %b : %q               <- both Numbers
//! ```
//!
//! # Soundness
//!
//! Running both arms unconditionally is `if_convert`'s bargain, unchanged:
//! every instruction in them must pass `is_speculatable`, and anything that
//! goes wrong on a lane that would have skipped an arm goes wrong loudly.
//!
//! The step that is new here is replacing `%p` with `%b` on the `M -> J`
//! edge, and it needs one static fact. That edge is taken exactly when `%p`
//! is truthy. If `%c` held, `%p` is `%b`; if not, `%p` is `%c`, which is
//! falsy, so the edge is not taken. Hence on that edge `%p = %b` - *provided
//! `%b` can never be falsy*, otherwise `%c` truthy and `%b` falsy would take
//! the `A2` edge that the select's `%c` does not model. So the rule demands
//! `%b` be **statically truthy**: defined by a pure `call_builtin` (which
//! returns a Number, and no number is falsy in Lua - not even 0), or a
//! number or string constant. `pin_builtin` exists to make exactly this
//! visible.
//!
//! `%p` itself must then die with the conversion, so the rule refuses if it
//! is used anywhere but `M`'s own branch and `J`'s phis' `M`-edge.
//!
//! # The tail shape
//!
//! Two sites are the same construct with the `or` half **already converted**
//! by an earlier `if_convert`: no second join, just the `and` triangle whose
//! phi feeds a materialised select,
//!
//! ```text
//!   H:    br %c ? A1 : J
//!   A1:   %b = ..                                <- statically truthy
//!         br J
//!   J:    %p = phi [H: %c, A1: %b]
//!         ..
//!         %r = select %p ? %p : %q               <- the or, already a select
//! ```
//!
//! The same argument applies unchanged: `%p` truthy iff `%c` held (because
//! `%b` cannot be falsy), and on that side `%p = %b`. So the triangle is
//! spliced like `if_convert` would, the phi is deleted, and every
//! `select %p ? %p : %q` in the function becomes `select %c ? %b : %q`. The
//! rule refuses any use of `%p` that is not a select of exactly that form.
//!
//! Which shape a join is, is decided by its conditionally-branching
//! predecessor: `or` codegen branches *to* the join on truthy (pair shape),
//! `and` codegen branches *into the arm* on truthy (tail shape).

use anyhow::{anyhow, Result};

use crate::ir::{FunDef, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::if_convert::is_speculatable;
use super::{get_block, get_block_mut, label_of, predecessors, require};
use super::super::validate::BlockKey;

/// Can this value never be falsy, knowing only its defining instruction?
fn statically_truthy(fun: &FunDef, id: LocalId) -> bool {
    for block in fun.cfg.iter_blocks() {
        for (candidate, instr) in &block.instructions {
            if *candidate != id {
                continue;
            }
            return match instr {
                // Pure builtins return Numbers, and only `nil` and `false`
                // are falsy in Lua - a number never is, not even 0.
                Instruction::CallBuiltin { name, .. } => {
                    crate::interpreter::fixed_env::is_pure_builtin(name)
                }
                Instruction::NumberConstant { .. } | Instruction::StringConstant { .. } => true,
                // Arithmetic either produces a Number (concat a String) or
                // fails loudly before any select is reached; it cannot
                // produce nil or false. Comparisons produce Bools and are
                // deliberately absent.
                Instruction::UnaryOp { op: crate::ir::UnaryOp::Minus | crate::ir::UnaryOp::Hash, .. } => {
                    true
                }
                Instruction::BinaryOp { op, .. } => matches!(
                    op,
                    crate::ir::BinaryOp::Plus
                        | crate::ir::BinaryOp::Minus
                        | crate::ir::BinaryOp::Star
                        | crate::ir::BinaryOp::Slash
                        | crate::ir::BinaryOp::Percent
                        | crate::ir::BinaryOp::Caret
                        | crate::ir::BinaryOp::TwoDots
                ),
                _ => false,
            };
        }
    }
    false
}

/// The recognised pair, re-derived identically by `apply`, `verify` and
/// `candidates`.
pub struct Ternary {
    pub head: BlockKey,
    pub arm1: Label,
    pub mid: Label,
    pub arm2: Label,
    pub condition: LocalId,
    /// The mid phi - the value that must die with the conversion.
    pub p: LocalId,
    /// What the mid phi is on the `mid -> join` edge: the arm1 value.
    pub b: LocalId,
}

/// The tail shape: the `and` triangle left behind after its `or` half was
/// already converted to a select.
pub struct Tail {
    pub head: BlockKey,
    pub arm: Label,
    pub condition: LocalId,
    /// The mixed phi, consumed only by `select p ? p : q` instructions.
    pub p: LocalId,
    /// Its arm value, statically truthy.
    pub b: LocalId,
}

pub enum Recognized {
    Pair(Ternary),
    Tail(Tail),
}

/// Which of the two shapes a join is, decided by its conditionally-branching
/// predecessor: `or` codegen branches *to* the join on truthy, `and` codegen
/// branches *into the arm* on truthy.
fn recognize(fun: &FunDef, join: &Label) -> Result<Recognized> {
    let preds = predecessors(&fun.cfg);
    let join_preds = preds
        .get(&Some(join.clone()))
        .filter(|p| p.len() == 2)
        .ok_or_else(|| anyhow!("'{}' does not have exactly two predecessors", join.as_str()))?;
    let branching: Vec<&Terminator> = join_preds
        .iter()
        .filter_map(|k| get_block(&fun.cfg, k))
        .map(|b| b.terminator_kind())
        .filter(|t| matches!(t, Terminator::ConditionalBranch { .. }))
        .collect();
    let [Terminator::ConditionalBranch { true_target, .. }] = branching[..] else {
        return Err(anyhow!(
            "'{}' must have exactly one conditionally-branching predecessor",
            join.as_str()
        ));
    };
    if true_target == join {
        shape(fun, join).map(Recognized::Pair)
    } else {
        tail_shape(fun, join).map(Recognized::Tail)
    }
}

fn shape(fun: &FunDef, join: &Label) -> Result<Ternary> {
    let preds = predecessors(&fun.cfg);
    let join_key = Some(join.clone());
    let join_preds = preds
        .get(&join_key)
        .filter(|p| p.len() == 2)
        .ok_or_else(|| anyhow!("'{}' does not have exactly two predecessors", join.as_str()))?;

    // One predecessor is the `and` join `M` (branches to us), the other the
    // `or` arm `A2` (falls through to us).
    let as_pair = |mid_key: &BlockKey, arm2_key: &BlockKey| -> Option<(Label, Label, LocalId)> {
        let mid = mid_key.clone()?;
        let arm2 = arm2_key.clone()?;
        let mid_block = get_block(&fun.cfg, mid_key)?;
        let Terminator::ConditionalBranch { condition, true_target, false_target } =
            mid_block.terminator_kind()
        else {
            return None;
        };
        (true_target == join && false_target == &arm2).then_some((mid, arm2, *condition))
    };
    let (mid, arm2, p) = as_pair(&join_preds[0], &join_preds[1])
        .or_else(|| as_pair(&join_preds[1], &join_preds[0]))
        .ok_or_else(|| {
            anyhow!(
                "no predecessor of '{}' branches on a phi to it and to the other \
                 predecessor",
                join.as_str()
            )
        })?;

    let arm2_block = get_block(&fun.cfg, &Some(arm2.clone())).unwrap();
    require(
        preds.get(&Some(arm2.clone())).map(|p| p.as_slice()) == Some(&[Some(mid.clone())]),
        format!("'{}' must run only from '{}'", arm2.as_str(), mid.as_str()),
    )?;

    // `M` must be exactly the phi and the branch on it - anything else in it
    // would run on a path this conversion no longer distinguishes.
    let mid_block = get_block(&fun.cfg, &Some(mid.clone())).unwrap();
    let [(p_def, Instruction::Phi { branches })] = &mid_block.instructions[..] else {
        return Err(anyhow!(
            "'{}' must contain exactly the phi its branch tests",
            mid.as_str()
        ));
    };
    require(
        *p_def == p && branches.len() == 2,
        format!(
            "'{}' must branch on its own two-way phi, but tests %{}",
            mid.as_str(),
            usize::from(p)
        ),
    )?;

    let mid_preds = preds
        .get(&Some(mid.clone()))
        .filter(|p| p.len() == 2)
        .ok_or_else(|| anyhow!("'{}' does not have exactly two predecessors", mid.as_str()))?;
    let as_head = |head_key: &BlockKey, arm1_key: &BlockKey| -> Option<(BlockKey, Label, LocalId)> {
        let arm1 = arm1_key.clone()?;
        let head_block = get_block(&fun.cfg, head_key)?;
        let Terminator::ConditionalBranch { condition, true_target, false_target } =
            head_block.terminator_kind()
        else {
            return None;
        };
        (true_target == &arm1 && false_target == &mid)
            .then_some((head_key.clone(), arm1, *condition))
    };
    let (head, arm1, condition) = as_head(&mid_preds[0], &mid_preds[1])
        .or_else(|| as_head(&mid_preds[1], &mid_preds[0]))
        .ok_or_else(|| {
            anyhow!(
                "no predecessor of '{}' branches into the other and falls through \
                 to it",
                mid.as_str()
            )
        })?;
    let arm1_block = get_block(&fun.cfg, &Some(arm1.clone())).unwrap();
    require(
        preds.get(&Some(arm1.clone())).map(|p| p.as_slice()) == Some(&[head.clone()]),
        format!("'{}' must run only from its head", arm1.as_str()),
    )?;
    require(
        matches!(
            arm1_block.terminator_kind(),
            Terminator::UnconditionalBranch { target } if target == &mid
        ),
        format!("'{}' must fall through to '{}'", arm1.as_str(), mid.as_str()),
    )?;

    // The phi: the head edge must carry the branch condition itself (that is
    // what makes `truthy(p) == c` on the fall-through side), the arm edge
    // carries `b`.
    let head_label = label_of(&head);
    let from = |label: &Label| -> Option<LocalId> {
        branches.iter().find(|(l, _)| l == label).map(|(_, v)| *v)
    };
    require(
        from(&head_label) == Some(condition),
        format!(
            "the phi in '{}' must carry the branch condition %{} on the \
             fall-through edge",
            mid.as_str(),
            usize::from(condition)
        ),
    )?;
    let b = from(&arm1)
        .ok_or_else(|| anyhow!("the phi in '{}' names no '{}' edge", mid.as_str(), arm1.as_str()))?;
    require(
        statically_truthy(fun, b),
        format!(
            "%{} is not statically truthy, so `p truthy` and `condition held` \
             could differ; pin the arm's call first",
            usize::from(b)
        ),
    )?;

    // Both arms run unconditionally afterwards.
    for (arm_label, arm_block) in [(&arm1, arm1_block), (&arm2, arm2_block)] {
        for (id, instr) in &arm_block.instructions {
            require(
                is_speculatable(instr),
                format!(
                    "the arm '{}' contains %{} = `{}`, which is not safe to run \
                     unconditionally",
                    arm_label.as_str(),
                    usize::from(*id),
                    super::super::print::format_instruction(instr)
                ),
            )?;
        }
    }

    // `p` must die with the conversion: its only uses are `M`'s branch and
    // the `M` edge of `J`'s phis. Anything else would need the mixed value
    // materialised, which is the thing this rule exists to avoid.
    for (key, block) in super::super::validate::all_blocks(&fun.cfg) {
        for (id, instr) in &block.instructions {
            let uses_p = match instr {
                Instruction::Phi { branches } if key == join_key => {
                    require(
                        branches.iter().all(|(l, v)| l != &arm2 || *v != p),
                        format!(
                            "%{} reaches '{}' through the '{}' edge, where it is \
                             not %{}",
                            usize::from(p),
                            join.as_str(),
                            arm2.as_str(),
                            usize::from(b)
                        ),
                    )?;
                    false // the M edge is the allowed use
                }
                _ => instr.get_used_locals().contains(&p),
            };
            require(
                !uses_p,
                format!(
                    "%{} is also used by %{} in '{}'; the conversion must be able \
                     to delete it",
                    usize::from(p),
                    usize::from(*id),
                    super::super::validate::block_label(&key)
                ),
            )?;
        }
        if key != Some(mid.clone()) {
            require(
                !block.terminator_kind().get_used_locals().contains(&p),
                format!(
                    "%{} is also used by the terminator of '{}'",
                    usize::from(p),
                    super::super::validate::block_label(&key)
                ),
            )?;
        }
    }

    // Every phi in `J` must name exactly the two predecessors.
    let join_block = get_block(&fun.cfg, &join_key).unwrap();
    for (id, instr) in &join_block.instructions {
        let Instruction::Phi { branches } = instr else { continue };
        require(
            branches.len() == 2
                && branches.iter().any(|(l, _)| l == &mid)
                && branches.iter().any(|(l, _)| l == &arm2),
            format!(
                "phi %{} in '{}' does not name exactly '{}' and '{}'",
                usize::from(*id),
                join.as_str(),
                mid.as_str(),
                arm2.as_str()
            ),
        )?;
    }

    Ok(Ternary { head, arm1, mid, arm2, condition, p, b })
}

/// Is this instruction `select %p ? %p : %q` for the given `p` (with `q`
/// something else)? The one consumer shape the tail conversion can rewrite.
fn is_p_select(instr: &Instruction, p: LocalId) -> bool {
    matches!(
        instr,
        Instruction::Select { condition, if_true, if_false }
            if *condition == p && *if_true == p && *if_false != p
    )
}

fn tail_shape(fun: &FunDef, join: &Label) -> Result<Tail> {
    let t = super::if_convert::triangle_shaped(&fun.cfg, join)
        .ok_or_else(|| anyhow!("no triangle joins at '{}'", join.as_str()))?;
    require(
        t.arm_is_true,
        format!(
            "the arm of '{}' is on the falsy side, which is not `and` codegen",
            join.as_str()
        ),
    )?;
    let arm_block = get_block(&fun.cfg, &Some(t.arm.clone())).unwrap();
    for (id, instr) in &arm_block.instructions {
        require(
            is_speculatable(instr),
            format!(
                "the arm '{}' contains %{} = `{}`, which is not safe to run \
                 unconditionally",
                t.arm.as_str(),
                usize::from(*id),
                super::super::print::format_instruction(instr)
            ),
        )?;
    }

    // `p` is the phi every one of whose uses is `select p ? p : q`. There must
    // be exactly one - it is what the entry means by naming this join.
    let join_block = get_block(&fun.cfg, &Some(join.clone())).unwrap();
    let mut candidates: Vec<(LocalId, &Vec<(Label, LocalId)>)> = Vec::new();
    for (id, instr) in &join_block.instructions {
        let Instruction::Phi { branches } = instr else { continue };
        let mut uses = 0;
        let mut all_p_selects = true;
        for (_, block) in super::super::validate::all_blocks(&fun.cfg) {
            for (other, other_instr) in &block.instructions {
                if other == id || !other_instr.get_used_locals().contains(id) {
                    continue;
                }
                uses += 1;
                all_p_selects &= is_p_select(other_instr, *id);
            }
            all_p_selects &= !block.terminator_kind().get_used_locals().contains(id);
        }
        if uses > 0 && all_p_selects {
            candidates.push((*id, branches));
        }
    }
    let [(p, branches)] = candidates[..] else {
        return Err(anyhow!(
            "'{}' must have exactly one phi consumed only by `select p ? p : q` \
             instructions, found {}",
            join.as_str(),
            candidates.len()
        ));
    };

    let head_label = label_of(&t.head);
    let from = |label: &Label| -> Option<LocalId> {
        branches.iter().find(|(l, _)| l == label).map(|(_, v)| *v)
    };
    require(
        from(&head_label) == Some(t.condition),
        format!(
            "the phi %{} in '{}' must carry the branch condition %{} on the \
             fall-through edge",
            usize::from(p),
            join.as_str(),
            usize::from(t.condition)
        ),
    )?;
    let b = from(&t.arm)
        .ok_or_else(|| anyhow!("the phi %{} names no '{}' edge", usize::from(p), t.arm.as_str()))?;
    require(
        statically_truthy(fun, b),
        format!(
            "%{} is not statically truthy, so `p truthy` and `condition held` \
             could differ",
            usize::from(b)
        ),
    )?;

    Ok(Tail { head: t.head, arm: t.arm, condition: t.condition, p, b })
}

/// The select `J`'s phi becomes: condition-true takes the `M` edge value
/// (with the dead `p` replaced by `b`, which it equals on that edge),
/// condition-false takes the `A2` edge value.
fn selected(t: &Ternary, branches: &[(Label, LocalId)]) -> Instruction {
    let from = |label: &Label| {
        branches
            .iter()
            .find(|(l, _)| l == label)
            .map(|(_, v)| *v)
            .expect("shape check guarantees both branches")
    };
    let x = from(&t.mid);
    Instruction::Select {
        condition: t.condition,
        if_true: if x == t.p { t.b } else { x },
        if_false: from(&t.arm2),
    }
}

pub fn apply(program: &mut Program, function: &str, join: &str) -> Result<usize> {
    let join = Label::from(join.to_string());
    let fun = program.get(function)?;
    match recognize(fun, &join)? {
        Recognized::Pair(t) => apply_pair(program, function, &join, t),
        Recognized::Tail(t) => apply_tail(program, function, &join, t),
    }
}

fn apply_pair(program: &mut Program, function: &str, join: &Label, t: Ternary) -> Result<usize> {
    let cfg = &mut program.get_mut(function)?.cfg;
    let arm1 = cfg.named.remove(&t.arm1).unwrap();
    let arm2 = cfg.named.remove(&t.arm2).unwrap();

    let head = get_block_mut(cfg, &t.head).unwrap();
    head.instructions.extend(arm1.instructions);
    head.instructions.extend(arm2.instructions);
    head.terminator = (
        head.terminator.0,
        Terminator::UnconditionalBranch { target: t.mid.clone() },
    );

    let mid = cfg.named.get_mut(&t.mid).unwrap();
    mid.instructions.clear();
    mid.terminator = (
        mid.terminator.0,
        Terminator::UnconditionalBranch { target: join.clone() },
    );

    let join_block = cfg.named.get_mut(join).unwrap();
    let mut converted = 0;
    for (_, instr) in join_block.instructions.iter_mut() {
        let Instruction::Phi { branches } = instr else { continue };
        *instr = selected(&t, branches);
        converted += 1;
    }

    // Ids died (the phi, both arm terminators); any allocation is stale.
    cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(converted + 2)
}

fn apply_tail(program: &mut Program, function: &str, join: &Label, t: Tail) -> Result<usize> {
    let cfg = &mut program.get_mut(function)?.cfg;
    let arm = cfg.named.remove(&t.arm).unwrap();

    let head = get_block_mut(cfg, &t.head).unwrap();
    head.instructions.extend(arm.instructions);
    head.terminator = (
        head.terminator.0,
        Terminator::UnconditionalBranch { target: join.clone() },
    );

    // The mixed phi is deleted; the other phis become ordinary selects (the
    // arm is the truthy side).
    let head_label = label_of(&t.head);
    let arm_label = t.arm.clone();
    let join_block = cfg.named.get_mut(join).unwrap();
    let mut converted = 0;
    join_block.instructions = join_block
        .instructions
        .drain(..)
        .filter_map(|(id, instr)| {
            if id == t.p {
                return None;
            }
            let Instruction::Phi { branches } = &instr else { return Some((id, instr)) };
            let from = |label: &Label| {
                branches
                    .iter()
                    .find(|(l, _)| l == label)
                    .map(|(_, v)| *v)
                    .expect("triangle_shaped guarantees both branches")
            };
            converted += 1;
            Some((
                id,
                Instruction::Select {
                    condition: t.condition,
                    if_true: from(&arm_label),
                    if_false: from(&head_label),
                },
            ))
        })
        .collect();

    // Every consumer of the dead phi, wherever it is.
    let mut rewritten = 0;
    for block in std::iter::once(&mut cfg.entry).chain(cfg.named.values_mut()) {
        for (_, instr) in block.instructions.iter_mut() {
            if is_p_select(instr, t.p) {
                let Instruction::Select { if_false, .. } = *instr else { unreachable!() };
                *instr = Instruction::Select { condition: t.condition, if_true: t.b, if_false };
                rewritten += 1;
            }
        }
    }

    cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(converted + rewritten + 1)
}

/// Independent check: re-derives the shape and its side conditions from the
/// *before* program and reconstructs the after program exactly.
pub fn verify(before: &Program, after: &Program, function: &str, join: &str) -> Result<()> {
    let join = Label::from(join.to_string());
    let before_fun = before.get(function)?;
    match recognize(before_fun, &join)? {
        Recognized::Pair(t) => verify_pair(before, after, function, &join, t),
        Recognized::Tail(t) => verify_tail(before, after, function, &join, t),
    }
}

fn verify_pair(
    before: &Program,
    after: &Program,
    function: &str,
    join: &Label,
    t: Ternary,
) -> Result<()> {
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;

    for arm in [&t.arm1, &t.arm2] {
        require(
            !after_fun.cfg.named.contains_key(arm),
            format!("convert_ternary left the arm '{}' behind", arm.as_str()),
        )?;
    }
    require(
        after_fun.cfg.named.len() + 2 == before_fun.cfg.named.len(),
        "convert_ternary must remove exactly the two arm blocks",
    )?;

    // The head: its instructions, both arms' in order, one branch to mid.
    let before_head = get_block(&before_fun.cfg, &t.head).unwrap();
    let before_arm1 = get_block(&before_fun.cfg, &Some(t.arm1.clone())).unwrap();
    let before_arm2 = get_block(&before_fun.cfg, &Some(t.arm2.clone())).unwrap();
    let after_head = get_block(&after_fun.cfg, &t.head)
        .ok_or_else(|| anyhow!("convert_ternary removed the head block"))?;
    let expected: Vec<(LocalId, Instruction)> = before_head
        .instructions
        .iter()
        .chain(&before_arm1.instructions)
        .chain(&before_arm2.instructions)
        .cloned()
        .collect();
    require(
        after_head.instructions == expected,
        "convert_ternary did not splice both arms into the head verbatim",
    )?;
    require(
        after_head.terminator.0 == before_head.terminator.0
            && matches!(
                &after_head.terminator.1,
                Terminator::UnconditionalBranch { target } if target == &t.mid
            ),
        "the head must branch unconditionally to the and-join",
    )?;

    // The mid: emptied, falling through.
    let before_mid = get_block(&before_fun.cfg, &Some(t.mid.clone())).unwrap();
    let after_mid = get_block(&after_fun.cfg, &Some(t.mid.clone()))
        .ok_or_else(|| anyhow!("convert_ternary removed the and-join block"))?;
    require(
        after_mid.instructions.is_empty(),
        "the and-join must be left empty - its phi is the value being removed",
    )?;
    require(
        after_mid.terminator.0 == before_mid.terminator.0
            && matches!(
                &after_mid.terminator.1,
                Terminator::UnconditionalBranch { target } if target == join
            ),
        "the and-join must fall through to the or-join",
    )?;

    // The join: each phi the prescribed select, everything else untouched.
    let before_join = get_block(&before_fun.cfg, &Some(join.clone())).unwrap();
    let after_join = get_block(&after_fun.cfg, &Some(join.clone()))
        .ok_or_else(|| anyhow!("convert_ternary removed the or-join block"))?;
    require(
        before_join.instructions.len() == after_join.instructions.len()
            && after_join.terminator == before_join.terminator,
        "convert_ternary changed the shape of the or-join",
    )?;
    for ((before_id, before_instr), (after_id, after_instr)) in
        before_join.instructions.iter().zip(&after_join.instructions)
    {
        require(before_id == after_id, "convert_ternary renumbered the or-join")?;
        let want = match before_instr {
            Instruction::Phi { branches } => selected(&t, branches),
            other => other.clone(),
        };
        require(
            &want == after_instr,
            format!(
                "wrong conversion of %{}: expected `{}`, found `{}`",
                usize::from(*before_id),
                super::super::print::format_instruction(&want),
                super::super::print::format_instruction(after_instr)
            ),
        )?;
    }

    // Nothing else.
    require(
        before.functions.len() == after.functions.len(),
        "convert_ternary changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!("convert_ternary on {} also changed {}", function, name.as_str()),
        )?;
    }
    let skip = [
        t.head.clone(),
        Some(t.arm1.clone()),
        Some(t.arm2.clone()),
        Some(t.mid.clone()),
        Some(join.clone()),
    ];
    for key in super::blocks_sorted(&before_fun.cfg) {
        if skip.contains(&key) {
            continue;
        }
        require(
            get_block(&before_fun.cfg, &key) == get_block(&after_fun.cfg, &key),
            format!(
                "convert_ternary changed block '{}', which is outside the pair",
                super::super::validate::block_label(&key)
            ),
        )?;
    }

    Ok(())
}

fn verify_tail(
    before: &Program,
    after: &Program,
    function: &str,
    join: &Label,
    t: Tail,
) -> Result<()> {
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;

    require(
        !after_fun.cfg.named.contains_key(&t.arm),
        format!("convert_ternary left the arm '{}' behind", t.arm.as_str()),
    )?;
    require(
        after_fun.cfg.named.len() + 1 == before_fun.cfg.named.len(),
        "convert_ternary must remove exactly the arm block",
    )?;

    // The head: its instructions, then the arm's, one branch to the join.
    let before_head = get_block(&before_fun.cfg, &t.head).unwrap();
    let before_arm = get_block(&before_fun.cfg, &Some(t.arm.clone())).unwrap();
    let after_head = get_block(&after_fun.cfg, &t.head)
        .ok_or_else(|| anyhow!("convert_ternary removed the head block"))?;
    let expected: Vec<(LocalId, Instruction)> = before_head
        .instructions
        .iter()
        .chain(&before_arm.instructions)
        .cloned()
        .collect();
    require(
        after_head.instructions == expected,
        "convert_ternary did not splice the arm into the head verbatim",
    )?;
    require(
        after_head.terminator.0 == before_head.terminator.0
            && matches!(
                &after_head.terminator.1,
                Terminator::UnconditionalBranch { target } if target == join
            ),
        "the head must branch unconditionally to the join",
    )?;

    // Every block but head and arm: identical, except that the mixed phi is
    // gone, other phis in the join became the prescribed selects, and every
    // `select p ? p : q` became `select c ? b : q`.
    let head_label = label_of(&t.head);
    let expected_instr = |key: &BlockKey, id: LocalId, instr: &Instruction| -> Option<Instruction> {
        if id == t.p {
            return None;
        }
        if is_p_select(instr, t.p) {
            let Instruction::Select { if_false, .. } = *instr else { unreachable!() };
            return Some(Instruction::Select {
                condition: t.condition,
                if_true: t.b,
                if_false,
            });
        }
        if key == &Some(join.clone()) {
            if let Instruction::Phi { branches } = instr {
                let from = |label: &Label| {
                    branches.iter().find(|(l, _)| l == label).map(|(_, v)| *v)
                };
                if let (Some(if_true), Some(if_false)) = (from(&t.arm), from(&head_label)) {
                    return Some(Instruction::Select {
                        condition: t.condition,
                        if_true,
                        if_false,
                    });
                }
            }
        }
        Some(instr.clone())
    };
    for key in super::blocks_sorted(&before_fun.cfg) {
        if key == t.head || key == Some(t.arm.clone()) {
            continue;
        }
        let before_block = get_block(&before_fun.cfg, &key).unwrap();
        let after_block = get_block(&after_fun.cfg, &key).ok_or_else(|| {
            anyhow!(
                "convert_ternary removed block '{}'",
                super::super::validate::block_label(&key)
            )
        })?;
        let expected: Vec<(LocalId, Instruction)> = before_block
            .instructions
            .iter()
            .filter_map(|(id, instr)| Some((*id, expected_instr(&key, *id, instr)?)))
            .collect();
        require(
            after_block.instructions == expected
                && after_block.terminator == before_block.terminator,
            format!(
                "block '{}' is not exactly the prescribed rewrite",
                super::super::validate::block_label(&key)
            ),
        )?;
    }

    require(
        before.functions.len() == after.functions.len(),
        "convert_ternary changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!("convert_ternary on {} also changed {}", function, name.as_str()),
        )?;
    }
    Ok(())
}

/// Joins where either shape matches.
pub fn candidates(program: &Program) -> Vec<(String, Label)> {
    let mut out = Vec::new();
    for (name, fun) in &program.functions {
        for label in fun.cfg.named.keys() {
            if recognize(fun, label).is_ok() {
                out.push((name.as_str().to_string(), label.clone()));
            }
        }
    }
    out.sort_by(|a, b| (&a.0, a.1.as_str()).cmp(&(&b.0, b.1.as_str())));
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{Block, Cfg, FunDef, GlobalId};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(s: &str) -> Label {
        Label::from(s.to_string())
    }

    fn num(n: i16) -> Instruction {
        Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(n) }
    }

    /// The `appr` shape: `%5 = %1 > %2; %10 = max(..); %14 = min-like value`.
    fn ternary_program(arm1_value: Instruction) -> Program {
        let mut named = crate::ir::new_label_map();
        named.insert(
            label("arm1"),
            Block {
                instructions: vec![(id(10), arm1_value)],
                terminator: (id(11), Terminator::UnconditionalBranch { target: label("mid") }),
                hint_normalize: false,
            },
        );
        named.insert(
            label("mid"),
            Block {
                instructions: vec![(
                    id(12),
                    Instruction::Phi {
                        branches: vec![(label("__entry"), id(5)), (label("arm1"), id(10))],
                    },
                )],
                terminator: (
                    id(13),
                    Terminator::ConditionalBranch {
                        condition: id(12),
                        true_target: label("join"),
                        false_target: label("arm2"),
                    },
                ),
                hint_normalize: false,
            },
        );
        named.insert(
            label("arm2"),
            Block {
                instructions: vec![(id(14), num(3))],
                terminator: (id(15), Terminator::UnconditionalBranch { target: label("join") }),
                hint_normalize: false,
            },
        );
        named.insert(
            label("join"),
            Block {
                instructions: vec![(
                    id(20),
                    Instruction::Phi {
                        branches: vec![(label("mid"), id(12)), (label("arm2"), id(14))],
                    },
                )],
                terminator: (id(21), Terminator::Return { value: Some(id(20)) }),
                hint_normalize: false,
            },
        );
        let cfg = Cfg::new(
            Block {
                instructions: vec![(
                    id(5),
                    Instruction::BinaryOp {
                        left: id(1),
                        op: crate::ir::BinaryOp::GreaterThan,
                        right: id(2),
                    },
                )],
                terminator: (
                    id(6),
                    Terminator::ConditionalBranch {
                        condition: id(5),
                        true_target: label("arm1"),
                        false_target: label("mid"),
                    },
                ),
                hint_normalize: false,
            },
            named,
        );
        let mut functions = IndexMap::new();
        functions.insert(
            GlobalId::from("f".to_string()),
            FunDef {
                name: GlobalId::from("f".to_string()),
                capture_ids: vec![],
                arg_ids: vec![Some(id(1)), Some(id(2))],
                cfg,
                source_span: None,
            },
        );
        Program { functions }
    }

    fn pinned_max() -> Instruction {
        Instruction::CallBuiltin {
            callee: id(1),
            name: "max".to_string(),
            args: vec![id(1), id(2)],
        }
    }

    #[test]
    fn converts_the_pair_and_verifies() {
        let before = ternary_program(pinned_max());
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "join").unwrap(), 3);
        verify(&before, &after, "f", "join").unwrap();

        let fun = after.get("f").unwrap();
        assert!(!fun.cfg.named.contains_key(&label("arm1")));
        assert!(!fun.cfg.named.contains_key(&label("arm2")));
        assert!(fun.cfg.named.get(&label("mid")).unwrap().instructions.is_empty());
        // The or-join's phi is one select on the *head's* condition, with the
        // dead mid-phi replaced by the arm1 value.
        let join = fun.cfg.named.get(&label("join")).unwrap();
        assert!(
            matches!(
                join.instructions[0].1,
                Instruction::Select { condition, if_true, if_false }
                    if condition == id(5) && if_true == id(10) && if_false == id(14)
            ),
            "{:?}",
            join.instructions[0].1
        );
    }

    /// The one static fact everything rests on: if the arm value could be
    /// falsy, `p truthy` and `condition held` are different questions.
    #[test]
    fn refuses_an_arm_value_that_is_not_statically_truthy() {
        for value in [
            Instruction::BinaryOp { left: id(1), op: crate::ir::BinaryOp::LessThan, right: id(2) },
            Instruction::BoolConstant { value: true },
            Instruction::NilConstant,
            Instruction::Call { closure: id(1), args: vec![] },
        ] {
            let before = ternary_program(value);
            let mut after = before.clone();
            let error = apply(&mut after, "f", "join").unwrap_err().to_string();
            assert!(error.contains("statically truthy"), "{}", error);
        }
    }

    /// A non-pure builtin in the arm must be refused twice over - it is
    /// neither speculatable nor a truthiness witness.
    #[test]
    fn refuses_an_unpinned_call_in_an_arm() {
        let mut p = ternary_program(pinned_max());
        p.get_mut("f")
            .unwrap()
            .cfg
            .named
            .get_mut(&label("arm2"))
            .unwrap()
            .instructions
            .push((id(16), Instruction::Call { closure: id(1), args: vec![] }));
        assert!(apply(&mut p, "f", "join").is_err());
    }

    /// If the mid phi is used anywhere else, the conversion cannot delete it
    /// and must refuse rather than materialise the mixed value.
    #[test]
    fn refuses_a_mid_phi_with_another_use() {
        let mut p = ternary_program(pinned_max());
        let join = p.get_mut("f").unwrap().cfg.named.get_mut(&label("join")).unwrap();
        join.instructions.push((
            id(22),
            Instruction::UnaryOp { op: crate::ir::UnaryOp::Not, arg: id(12) },
        ));
        let error = apply(&mut p, "f", "join").unwrap_err().to_string();
        assert!(error.contains("also used"), "{}", error);
    }

    /// The mid block must hold nothing but the phi - an extra instruction
    /// there ran on both original paths but would be deleted.
    #[test]
    fn refuses_a_mid_block_with_extra_instructions() {
        let mut p = ternary_program(pinned_max());
        let mid = p.get_mut("f").unwrap().cfg.named.get_mut(&label("mid")).unwrap();
        mid.instructions.push((id(17), num(1)));
        let error = apply(&mut p, "f", "join").unwrap_err().to_string();
        assert!(error.contains("exactly the phi"), "{}", error);
    }

    /// The tail shape: the `or` half is already a select, only the `and`
    /// triangle remains, its phi consumed by `select %12 ? %12 : %14`.
    fn tail_program(arm_value: Instruction) -> Program {
        let mut named = crate::ir::new_label_map();
        named.insert(
            label("arm"),
            Block {
                instructions: vec![(id(10), arm_value)],
                terminator: (id(11), Terminator::UnconditionalBranch { target: label("join") }),
                hint_normalize: false,
            },
        );
        named.insert(
            label("join"),
            Block {
                instructions: vec![
                    (
                        id(12),
                        Instruction::Phi {
                            branches: vec![(label("__entry"), id(5)), (label("arm"), id(10))],
                        },
                    ),
                    (id(14), num(3)),
                    (
                        id(15),
                        Instruction::Select { condition: id(12), if_true: id(12), if_false: id(14) },
                    ),
                ],
                terminator: (id(16), Terminator::Return { value: Some(id(15)) }),
                hint_normalize: false,
            },
        );
        let cfg = Cfg::new(
            Block {
                instructions: vec![(
                    id(5),
                    Instruction::BinaryOp {
                        left: id(1),
                        op: crate::ir::BinaryOp::GreaterThan,
                        right: id(2),
                    },
                )],
                terminator: (
                    id(6),
                    Terminator::ConditionalBranch {
                        condition: id(5),
                        true_target: label("arm"),
                        false_target: label("join"),
                    },
                ),
                hint_normalize: false,
            },
            named,
        );
        let mut functions = IndexMap::new();
        functions.insert(
            GlobalId::from("f".to_string()),
            FunDef {
                name: GlobalId::from("f".to_string()),
                capture_ids: vec![],
                arg_ids: vec![Some(id(1)), Some(id(2))],
                cfg,
                source_span: None,
            },
        );
        Program { functions }
    }

    #[test]
    fn converts_the_tail_and_verifies() {
        let before = tail_program(pinned_max());
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "join").unwrap(), 2);
        verify(&before, &after, "f", "join").unwrap();

        let fun = after.get("f").unwrap();
        assert!(!fun.cfg.named.contains_key(&label("arm")));
        let join = fun.cfg.named.get(&label("join")).unwrap();
        // The phi is gone; the select now tests the original condition and
        // takes the arm value where the phi would have been truthy.
        assert_eq!(join.instructions.len(), 2);
        assert!(
            matches!(
                join.instructions[1].1,
                Instruction::Select { condition, if_true, if_false }
                    if condition == id(5) && if_true == id(10) && if_false == id(14)
            ),
            "{:?}",
            join.instructions[1].1
        );
    }

    /// `-1` in the arm is `unary minus of a constant`: arithmetic produces a
    /// Number or fails loudly, and no number is falsy - the `and_or_join_199`
    /// site.
    #[test]
    fn a_negated_constant_is_statically_truthy() {
        let before = tail_program(Instruction::UnaryOp {
            op: crate::ir::UnaryOp::Minus,
            arg: id(1),
        });
        let mut after = before.clone();
        apply(&mut after, "f", "join").unwrap();
        verify(&before, &after, "f", "join").unwrap();
    }

    /// A use of the phi that is not `select p ? p : q` cannot be rewritten,
    /// and the rule must refuse rather than leave a dangling reference.
    #[test]
    fn refuses_a_tail_phi_with_a_non_select_use() {
        let mut p = tail_program(pinned_max());
        let join = p.get_mut("f").unwrap().cfg.named.get_mut(&label("join")).unwrap();
        join.instructions.push((
            id(17),
            Instruction::UnaryOp { op: crate::ir::UnaryOp::Not, arg: id(12) },
        ));
        let error = apply(&mut p, "f", "join").unwrap_err().to_string();
        assert!(error.contains("consumed only by"), "{}", error);
    }

    /// The verifier must insist every consumer was rewritten.
    #[test]
    fn verify_rejects_an_unrewritten_select() {
        let before = tail_program(pinned_max());
        let mut after = before.clone();
        apply(&mut after, "f", "join").unwrap();
        let join = after.get_mut("f").unwrap().cfg.named.get_mut(&label("join")).unwrap();
        join.instructions[1].1 =
            Instruction::Select { condition: id(5), if_true: id(5), if_false: id(14) };
        let error = verify(&before, &after, "f", "join").unwrap_err().to_string();
        assert!(error.contains("prescribed rewrite"), "{}", error);
    }

    /// The verifier must reject a select built on the wrong condition - using
    /// `p`'s edge shape but `%c`'s replacement is the entire subtlety.
    #[test]
    fn verify_rejects_a_wrong_substitution() {
        let before = ternary_program(pinned_max());
        let mut after = before.clone();
        apply(&mut after, "f", "join").unwrap();
        let join = after.get_mut("f").unwrap().cfg.named.get_mut(&label("join")).unwrap();
        join.instructions[0].1 = Instruction::Select {
            condition: id(5),
            if_true: id(5),
            if_false: id(14),
        };
        let error = verify(&before, &after, "f", "join").unwrap_err().to_string();
        assert!(error.contains("wrong conversion"), "{}", error);
    }
}
