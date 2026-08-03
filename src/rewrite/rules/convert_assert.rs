//! `convert_assert` - replace an inlined `__assert` failure diamond with a
//! single `assert_true`.
//!
//! Pointed rule: names the *head* - the block whose conditional branch into
//! the failure subgraph is removed.
//!
//! # The shape
//!
//! Inlining `__assert(cond)` leaves this in the caller (the Lua body is
//! `if not cond then __print("Assertion failed") ... error(...) end`):
//!
//! ```text
//!   H:  %n = not %cond                      H:  %n = assert_true %cond
//!       br %n ? FAIL : J                        br J
//!   FAIL: __print("Assertion failed")   =>  (FAIL, E, M, X die; nothing
//!         %e = %msg == nil                   else changes, no ids minted -
//!         br %e ? E : M                      `%n` is reused for the assert)
//!   E:  error()           ; br X
//!   M:  __print(%msg); error(%msg); br X
//!   X:  br J
//! ```
//!
//! # Soundness
//!
//! Every path through the failure subgraph calls `error`, whose builtin
//! returns `Err` - it aborts the whole run, exactly like a failing
//! `assert_true`. Case analysis on `%cond`:
//!
//! * **Scalar `true`**: `not` gives `false`, the branch skips the subgraph,
//!   nothing else in it has an effect. The assert passes. Identical.
//! * **Scalar `false`**: the subgraph runs and `error` aborts. The assert
//!   fails and aborts. Both die (with different messages, which only a dead
//!   run can see).
//! * **Vector bool**: false lanes are filtered into the subgraph; if there
//!   are any, `error` aborts, and if there are none the edge is dropped
//!   empty (`flow.rs` discards a zero-lane fragment) and all lanes reach the
//!   join. The assert fails iff any lane is false. Identical.
//! * **`UnknownBool`**: the branch sends the state down *both* edges, so the
//!   subgraph runs and aborts. The assert fails on an unknown bool by
//!   design. Both die.
//! * **Anything else**: `not` itself is a hard error on non-bools
//!   (`op.rs::interpret_not`), so the original dies at `%n` and the rewrite
//!   dies at the assert. There is no truthiness gap to trade away.
//!
//! One premise is syntactic rather than semantic: the arms are matched as
//! `get_global "error"; load; call`, which reaches the aborting builtin only
//! because nothing rebinds the `error` global. If it were ever rebound to a
//! function that returns, the original could survive a failed assertion that
//! the rewrite turns into a loud abort - a divergence that only ever replaces
//! silent survival with loud failure, never the reverse.
//!
//! # Why bother
//!
//! The branch is uniform - the asserted conditions are comparisons on
//! concrete values, so the subgraph never runs - but it is still a branch:
//! it fragments regions, blocks speculation across the assert, and puts four
//! dead-in-practice blocks between every `btn` call and its concretization.
//! `assert_true` is straight-line and speculatable.

use anyhow::{anyhow, Result};

use crate::ir::{BinaryOp, FunDef, Instruction, Label, LocalId, Terminator, UnaryOp};

use super::super::program::Program;
use super::{get_block, predecessors, require};

/// The shape this rule accepts, re-derived identically by `apply` and
/// `verify`.
struct Site {
    /// The asserted condition - the operand of the head's trailing `not`.
    condition: LocalId,
    /// The `not`'s id, reused for the `assert_true`.
    not_id: LocalId,
    /// The failure entry (`if_body_9`).
    fail: Label,
    /// The nil-message arm (`if_body_13`): `error()`.
    error_arm: Label,
    /// The message arm (`if_condition_12`): `__print(msg); error(msg)`.
    message_arm: Label,
    /// The empty exit (`if_join_15`).
    exit: Label,
    /// Where execution continues (`if_join_10`).
    join: Label,
}

fn get_global(name: &str) -> Instruction {
    Instruction::GetGlobal { name: name.to_string(), create_if_missing: false }
}

/// Matches `get_global <name>; load` at the start of `rest`, returning the
/// loaded closure's id and what follows.
fn classify_global_load<'a>(
    label: &Label,
    name: &str,
    rest: &'a [(LocalId, Instruction)],
) -> Result<(LocalId, &'a [(LocalId, Instruction)])> {
    let [(global_id, global), (load_id, load), tail @ ..] = rest else {
        return Err(anyhow!("'{}' is too short to load `{}`", label.as_str(), name));
    };
    require(
        global == &get_global(name),
        format!("'{}' does not read the `{}` global where expected", label.as_str(), name),
    )?;
    require(
        load == &(Instruction::Load { source: *global_id }),
        format!("'{}' does not load the `{}` global it read", label.as_str(), name),
    )?;
    Ok((*load_id, tail))
}

/// Requires that `label` hangs off exactly the predecessors in `expected`.
fn require_predecessors(fun: &FunDef, label: &Label, expected: &[&Label]) -> Result<()> {
    let preds = predecessors(&fun.cfg);
    let mut actual = preds.get(&Some(label.clone())).cloned().unwrap_or_default();
    actual.sort_by_key(|k| super::label_of(k).as_str().to_string());
    let mut expected: Vec<Option<Label>> = expected.iter().map(|l| Some((*l).clone())).collect();
    expected.sort_by_key(|k| super::label_of(k).as_str().to_string());
    require(
        actual == expected,
        format!("'{}' has predecessors outside the assert diamond", label.as_str()),
    )
}

fn get_named<'a>(fun: &'a FunDef, label: &Label) -> Result<&'a crate::ir::Block> {
    let block = fun
        .cfg
        .named
        .get(label)
        .ok_or_else(|| anyhow!("no block '{}'", label.as_str()))?;
    require(!block.hint_normalize, format!("'{}' is a normalize block", label.as_str()))?;
    Ok(block)
}

fn site(fun: &FunDef, function: &str, head: &Label) -> Result<Site> {
    let head_block = fun
        .cfg
        .named
        .get(head)
        .ok_or_else(|| anyhow!("no block '{}' in {}", head.as_str(), function))?;

    // The head: `%n = not %cond` last, branched on and used nowhere else.
    let Some((not_id, Instruction::UnaryOp { op: UnaryOp::Not, arg: condition })) =
        head_block.instructions.last()
    else {
        return Err(anyhow!("'{}' does not end its instructions with `not`", head.as_str()));
    };
    let (not_id, condition) = (*not_id, *condition);
    let Terminator::ConditionalBranch { condition: branch_on, true_target, false_target } =
        head_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in a conditional branch", head.as_str()));
    };
    require(
        branch_on == &not_id,
        format!("'{}' does not branch on its trailing `not`", head.as_str()),
    )?;
    let (fail, join) = (true_target.clone(), false_target.clone());

    // The failure entry: print the fixed string, then branch on whether the
    // message is nil. The message local is captured here and required to be
    // what the message arm prints and errors with.
    let fail_block = get_named(fun, &fail)?;
    require_predecessors(fun, &fail, &[head])?;
    let (print_fn, rest) = classify_global_load(&fail, "__print", &fail_block.instructions)?;
    let [(str_id, string), (_, call), (nil_id, nil), (eq_id, eq)] = rest else {
        return Err(anyhow!("'{}' is not exactly the assert failure entry", fail.as_str()));
    };
    require(
        string == &(Instruction::StringConstant { value: "Assertion failed".to_string() }),
        format!("'{}' does not print \"Assertion failed\"", fail.as_str()),
    )?;
    require(
        call == &(Instruction::Call { closure: print_fn, args: vec![*str_id] }),
        format!("'{}' does not call `__print` on its string", fail.as_str()),
    )?;
    require(
        nil == &Instruction::NilConstant,
        format!("'{}' does not build a nil to compare against", fail.as_str()),
    )?;
    let Instruction::BinaryOp { left: msg, op: BinaryOp::TwoEqual, right } = eq else {
        return Err(anyhow!("'{}' does not test its message against nil", fail.as_str()));
    };
    let msg = *msg;
    require(
        right == nil_id,
        format!("'{}' compares against something other than its nil", fail.as_str()),
    )?;
    let Terminator::ConditionalBranch {
        condition: fail_branch_on,
        true_target: error_arm,
        false_target: message_arm,
    } = fail_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in a conditional branch", fail.as_str()));
    };
    require(
        fail_branch_on == eq_id,
        format!("'{}' does not branch on its nil test", fail.as_str()),
    )?;
    let (error_arm, message_arm) = (error_arm.clone(), message_arm.clone());

    // The nil-message arm: exactly `error()`.
    let error_block = get_named(fun, &error_arm)?;
    require_predecessors(fun, &error_arm, &[&fail])?;
    let (error_fn, rest) = classify_global_load(&error_arm, "error", &error_block.instructions)?;
    let [(_, call)] = rest else {
        return Err(anyhow!("'{}' is not exactly a call to `error`", error_arm.as_str()));
    };
    require(
        call == &(Instruction::Call { closure: error_fn, args: vec![] }),
        format!("'{}' does not call the `error` it loaded", error_arm.as_str()),
    )?;
    let Terminator::UnconditionalBranch { target: exit } = error_block.terminator_kind() else {
        return Err(anyhow!("'{}' does not end in a jump", error_arm.as_str()));
    };
    let exit = exit.clone();

    // The message arm: exactly `__print(msg); error(msg)`, on the same
    // message the nil test looked at.
    let message_block = get_named(fun, &message_arm)?;
    require_predecessors(fun, &message_arm, &[&fail])?;
    let (print_fn, rest) =
        classify_global_load(&message_arm, "__print", &message_block.instructions)?;
    let [(_, print_call), tail @ ..] = rest else {
        return Err(anyhow!("'{}' is too short to print its message", message_arm.as_str()));
    };
    require(
        print_call == &(Instruction::Call { closure: print_fn, args: vec![msg] }),
        format!("'{}' does not print the tested message", message_arm.as_str()),
    )?;
    let (error_fn, rest) = classify_global_load(&message_arm, "error", tail)?;
    let [(_, error_call)] = rest else {
        return Err(anyhow!("'{}' is not exactly print + error", message_arm.as_str()));
    };
    require(
        error_call == &(Instruction::Call { closure: error_fn, args: vec![msg] }),
        format!("'{}' does not error with the tested message", message_arm.as_str()),
    )?;
    let Terminator::UnconditionalBranch { target: message_exit } =
        message_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in a jump", message_arm.as_str()));
    };
    require(
        message_exit == &exit,
        format!(
            "the arms of '{}' rejoin at '{}' and '{}', not at one block",
            fail.as_str(),
            exit.as_str(),
            message_exit.as_str()
        ),
    )?;

    // The exit: empty, both arms its only predecessors, jumping to the join.
    let exit_block = get_named(fun, &exit)?;
    require_predecessors(fun, &exit, &[&error_arm, &message_arm])?;
    require(
        exit_block.instructions.is_empty(),
        format!("'{}' is not empty", exit.as_str()),
    )?;
    let Terminator::UnconditionalBranch { target: exit_target } = exit_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in a jump", exit.as_str()));
    };
    require(
        exit_target == &join,
        format!(
            "'{}' exits to '{}', not to the branch's other side '{}'",
            exit.as_str(),
            exit_target.as_str(),
            join.as_str()
        ),
    )?;

    // The join: only the head and the exit reach it, and no phi depends on
    // the exit edge that is about to disappear.
    require_predecessors(fun, &join, &[head, &exit])?;
    let join_block = get_named(fun, &join)?;
    require(
        !join_block
            .instructions
            .iter()
            .any(|(_, i)| matches!(i, Instruction::Phi { .. })),
        format!("'{}' has phis, which would lose the exit edge", join.as_str()),
    )?;

    // All six blocks are distinct - the predecessor checks above assume it.
    let mut labels: Vec<&Label> = vec![head, &fail, &error_arm, &message_arm, &exit, &join];
    labels.sort_by_key(|l| l.as_str().to_string());
    labels.dedup();
    require(labels.len() == 6, format!("the diamond at '{}' reuses a block", head.as_str()))?;

    // `%n` exists only to be branched on; the branch is going away.
    for key in super::blocks_sorted(&fun.cfg) {
        let Some(block) = get_block(&fun.cfg, &key) else { continue };
        for (id, instr) in &block.instructions {
            require(
                !instr.get_used_locals().contains(&not_id),
                format!("%{} (the `not`) is also used by %{}", usize::from(not_id), usize::from(*id)),
            )?;
        }
        if key.as_ref() != Some(head) {
            require(
                !block.terminator_kind().get_used_locals().contains(&not_id),
                format!(
                    "%{} (the `not`) is also used by the terminator of '{}'",
                    usize::from(not_id),
                    super::super::validate::block_label(&key)
                ),
            )?;
        }
    }

    Ok(Site { condition, not_id, fail, error_arm, message_arm, exit, join })
}

pub fn apply(program: &mut Program, function: &str, head: &str) -> Result<usize> {
    let head = Label::from(head.to_string());
    let fun = program.get(function)?;
    let s = site(fun, function, &head)?;

    let fun = program.get_mut(function)?;
    fun.cfg.named.remove(&s.fail);
    fun.cfg.named.remove(&s.error_arm);
    fun.cfg.named.remove(&s.message_arm);
    fun.cfg.named.remove(&s.exit);
    let head_block = fun.cfg.named.get_mut(&head).unwrap();
    let last = head_block.instructions.last_mut().unwrap();
    *last = (s.not_id, Instruction::AssertTrue { value: s.condition });
    head_block.terminator = (
        head_block.terminator.0,
        Terminator::UnconditionalBranch { target: s.join.clone() },
    );

    // Live ranges changed; any slot allocation is stale.
    fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(4)
}

/// Independent check: re-derives the site from the *before* program and
/// insists the after program is exactly the prescription - the head's `not`
/// replaced by `assert_true` on the same id, the branch made unconditional,
/// the four failure blocks gone, and not one other thing different.
pub fn verify(before: &Program, after: &Program, function: &str, head: &str) -> Result<()> {
    let head = Label::from(head.to_string());
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let s = site(before_fun, function, &head)?;

    // The head: instructions untouched except the trailing `not`, branch now
    // unconditional to the join.
    let before_head = before_fun.cfg.named.get(&head).unwrap();
    let after_head = after_fun
        .cfg
        .named
        .get(&head)
        .ok_or_else(|| anyhow!("convert_assert removed the head block"))?;
    require(
        after_head.instructions.len() == before_head.instructions.len(),
        "convert_assert changed the head's instruction count",
    )?;
    let n = before_head.instructions.len();
    require(
        after_head.instructions[..n - 1] == before_head.instructions[..n - 1],
        "convert_assert changed the head before the `not`",
    )?;
    require(
        after_head.instructions[n - 1]
            == (s.not_id, Instruction::AssertTrue { value: s.condition }),
        "the head does not end with the prescribed assert_true",
    )?;
    require(
        after_head.terminator.0 == before_head.terminator.0
            && matches!(
                &after_head.terminator.1,
                Terminator::UnconditionalBranch { target } if target == &s.join
            )
            && after_head.hint_normalize == before_head.hint_normalize,
        "convert_assert did not make the head jump straight to the join",
    )?;

    // The failure subgraph: gone, and nothing else gone.
    let removed = [&s.fail, &s.error_arm, &s.message_arm, &s.exit];
    for label in removed {
        require(
            !after_fun.cfg.named.contains_key(label),
            format!("convert_assert left '{}' behind", label.as_str()),
        )?;
    }
    require(
        after_fun.cfg.named.len() + removed.len() == before_fun.cfg.named.len(),
        "convert_assert changed the set of blocks beyond removing the diamond",
    )?;

    // Everything else, the join included: untouched.
    for key in super::blocks_sorted(&before_fun.cfg) {
        if key == Some(head.clone()) || removed.iter().any(|l| key.as_ref() == Some(*l)) {
            continue;
        }
        require(
            get_block(&before_fun.cfg, &key) == get_block(&after_fun.cfg, &key),
            format!(
                "convert_assert changed block '{}', which is outside the site",
                super::super::validate::block_label(&key)
            ),
        )?;
    }
    require(
        before.functions.len() == after.functions.len(),
        "convert_assert changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!("convert_assert on {} also changed {}", function, name.as_str()),
        )?;
    }
    Ok(())
}

/// Heads this rule accepts.
pub fn candidates(program: &Program) -> Vec<(String, Label)> {
    let mut out = Vec::new();
    for (name, fun) in &program.functions {
        for label in fun.cfg.named.keys() {
            if site(fun, name.as_str(), label).is_ok() {
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
    use crate::ir::{Block, Cfg, GlobalId};
    use indexmap::IndexMap;

    fn id(n: usize) -> LocalId {
        LocalId::from(n)
    }

    fn label(s: &str) -> Label {
        Label::from(s.to_string())
    }

    fn global_load(name: &str, base: usize) -> Vec<(LocalId, Instruction)> {
        vec![
            (id(base), get_global(name)),
            (id(base + 1), Instruction::Load { source: id(base) }),
        ]
    }

    /// `__entry` computes the condition and the (nil) message, `head` runs
    /// the inlined `__assert` diamond, `join` carries on.
    fn program() -> Program {
        let mut named = crate::ir::new_label_map();
        named.insert(
            label("head"),
            Block {
                instructions: vec![(
                    id(3),
                    Instruction::UnaryOp { op: UnaryOp::Not, arg: id(1) },
                )],
                terminator: (
                    id(9),
                    Terminator::ConditionalBranch {
                        condition: id(3),
                        true_target: label("fail"),
                        false_target: label("join"),
                    },
                ),
                hint_normalize: false,
            },
        );
        let mut fail_instructions = global_load("__print", 10);
        fail_instructions.extend([
            (id(12), Instruction::StringConstant { value: "Assertion failed".to_string() }),
            (id(13), Instruction::Call { closure: id(11), args: vec![id(12)] }),
            (id(14), Instruction::NilConstant),
            (
                id(15),
                Instruction::BinaryOp { left: id(2), op: BinaryOp::TwoEqual, right: id(14) },
            ),
        ]);
        named.insert(
            label("fail"),
            Block {
                instructions: fail_instructions,
                terminator: (
                    id(16),
                    Terminator::ConditionalBranch {
                        condition: id(15),
                        true_target: label("error_arm"),
                        false_target: label("message_arm"),
                    },
                ),
                hint_normalize: false,
            },
        );
        let mut error_instructions = global_load("error", 20);
        error_instructions.push((id(22), Instruction::Call { closure: id(21), args: vec![] }));
        named.insert(
            label("error_arm"),
            Block {
                instructions: error_instructions,
                terminator: (id(23), Terminator::UnconditionalBranch { target: label("exit") }),
                hint_normalize: false,
            },
        );
        let mut message_instructions = global_load("__print", 30);
        message_instructions
            .push((id(32), Instruction::Call { closure: id(31), args: vec![id(2)] }));
        message_instructions.extend(global_load("error", 33));
        message_instructions
            .push((id(35), Instruction::Call { closure: id(34), args: vec![id(2)] }));
        named.insert(
            label("message_arm"),
            Block {
                instructions: message_instructions,
                terminator: (id(36), Terminator::UnconditionalBranch { target: label("exit") }),
                hint_normalize: false,
            },
        );
        named.insert(
            label("exit"),
            Block {
                instructions: vec![],
                terminator: (id(40), Terminator::UnconditionalBranch { target: label("join") }),
                hint_normalize: false,
            },
        );
        named.insert(
            label("join"),
            Block {
                instructions: vec![],
                terminator: (id(50), Terminator::Return { value: None }),
                hint_normalize: false,
            },
        );
        let cfg = Cfg::new(
            Block {
                instructions: vec![
                    (id(1), Instruction::BoolConstant { value: true }),
                    (id(2), Instruction::NilConstant),
                ],
                terminator: (id(8), Terminator::UnconditionalBranch { target: label("head") }),
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
                arg_ids: vec![],
                cfg,
                source_span: None,
            },
        );
        Program { functions }
    }

    #[test]
    fn rewrites_the_diamond() {
        let before = program();
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "head").unwrap(), 4);
        verify(&before, &after, "f", "head").unwrap();

        let fun = after.get("f").unwrap();
        for gone in ["fail", "error_arm", "message_arm", "exit"] {
            assert!(!fun.cfg.named.contains_key(&label(gone)), "{} survived", gone);
        }
        let head = fun.cfg.named.get(&label("head")).unwrap();
        assert_eq!(
            head.instructions,
            vec![(id(3), Instruction::AssertTrue { value: id(1) })]
        );
        assert_eq!(
            head.terminator,
            (id(9), Terminator::UnconditionalBranch { target: label("join") })
        );
    }

    #[test]
    fn validates_after_apply() {
        let mut after = program();
        apply(&mut after, "f", "head").unwrap();
        let errors = super::super::super::validate::validate_program(&after);
        assert!(errors.is_empty(), "{:?}", errors);
    }

    /// The fixed string is the fingerprint of the inlined `__assert` body; a
    /// diamond printing anything else is not one.
    #[test]
    fn refuses_a_different_message_string() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        let fail = fun.cfg.named.get_mut(&label("fail")).unwrap();
        fail.instructions[2].1 =
            Instruction::StringConstant { value: "something else".to_string() };
        let error = apply(&mut before, "f", "head").unwrap_err().to_string();
        assert!(error.contains("Assertion failed"), "{}", error);
    }

    /// An arm with extra instructions could have effects that survive on a
    /// path the rewrite deletes.
    #[test]
    fn refuses_an_arm_with_extra_instructions() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        let arm = fun.cfg.named.get_mut(&label("error_arm")).unwrap();
        arm.instructions.push((id(24), Instruction::NilConstant));
        let error = apply(&mut before, "f", "head").unwrap_err().to_string();
        assert!(error.contains("exactly a call to `error`"), "{}", error);
    }

    /// A phi in the join would lose its exit edge when the subgraph dies.
    #[test]
    fn refuses_a_join_with_phis() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        let join = fun.cfg.named.get_mut(&label("join")).unwrap();
        join.instructions.push((
            id(51),
            Instruction::Phi {
                branches: vec![(label("head"), id(1)), (label("exit"), id(2))],
            },
        ));
        let error = apply(&mut before, "f", "head").unwrap_err().to_string();
        assert!(error.contains("phis"), "{}", error);
    }

    /// A join reachable from outside the diamond is fine - but a *failure
    /// block* reachable from outside would keep aborting paths alive that
    /// the rewrite no longer models.
    #[test]
    fn refuses_a_fail_block_with_other_predecessors() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        fun.cfg.named.insert(
            label("elsewhere"),
            Block {
                instructions: vec![],
                terminator: (
                    id(60),
                    Terminator::UnconditionalBranch { target: label("exit") },
                ),
                hint_normalize: false,
            },
        );
        let error = apply(&mut before, "f", "head").unwrap_err().to_string();
        assert!(error.contains("predecessors outside"), "{}", error);
    }

    /// The `not` must exist only to be branched on - any other use would
    /// dangle once the branch is gone. (`assert_true` reuses its id, but its
    /// result is not the negation.)
    #[test]
    fn refuses_a_not_with_other_uses() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        let join = fun.cfg.named.get_mut(&label("join")).unwrap();
        join.terminator = (id(50), Terminator::Return { value: Some(id(3)) });
        let error = apply(&mut before, "f", "head").unwrap_err().to_string();
        assert!(error.contains("also used"), "{}", error);
    }

    /// The verifier is the trusted half: it must reject an applier that left
    /// part of the failure subgraph behind...
    #[test]
    fn verify_rejects_a_kept_block() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        let exit = before.get("f").unwrap().cfg.named.get(&label("exit")).unwrap().clone();
        after.get_mut("f").unwrap().cfg.named.insert(label("exit"), exit);
        let error = verify(&before, &after, "f", "head").unwrap_err().to_string();
        assert!(error.contains("left 'exit' behind"), "{}", error);
    }

    /// ...and one that asserted the wrong value.
    #[test]
    fn verify_rejects_a_tampered_assert() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        let fun = after.get_mut("f").unwrap();
        let head = fun.cfg.named.get_mut(&label("head")).unwrap();
        head.instructions[0].1 = Instruction::AssertTrue { value: id(2) };
        let error = verify(&before, &after, "f", "head").unwrap_err().to_string();
        assert!(error.contains("prescribed assert_true"), "{}", error);
    }

    #[test]
    fn candidates_finds_the_head() {
        let found = candidates(&program());
        assert_eq!(found, vec![("f".to_string(), label("head"))]);
    }
}
