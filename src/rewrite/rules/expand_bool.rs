//! `expand_bool` - replace a concretization diamond with lane expansion.
//!
//! Pointed rule: names the *head* - the block whose conditional branch is
//! removed, which is also the block the profiler attributes the splits to.
//!
//! # The shape
//!
//! The inlined `btn` body ends in a diamond that concretizes an unknown
//! button state: branch on it, and each arm stores its constant back into the
//! button cell so every later read this frame agrees with the branch taken.
//!
//! ```text
//!   H:  br %c ? T : F              H:  br J
//!   T:  %at = <accessor>      =>   J:  %phi = expand %c
//!       %vt = bool true                %at = <accessor>
//!       store %at <- %vt              store %at <- %phi
//!       br J                          ...
//!   F:  %af = <accessor>       (same accessor in both arms; T and F die,
//!       %vf = bool false        their ids %at and the true arm's store id
//!       store %af <- %vf        are reused in J)
//!       br J
//!   J:  %phi = phi [T: %vt, F: %vf]
//!       ...
//! ```
//!
//! # Soundness
//!
//! Branching on an `UnknownBool` sends the whole state down *both* edges
//! unfiltered: fragment T is every lane with the cell (and `%phi`) forced
//! `true`, fragment F every lane forced `false`. `expand %c` builds exactly
//! that lane set in one state - each old lane once with `true`, once with
//! `false` - and the store writes the same per-lane values through the same
//! accessor the arms would have run. On a vector bool the branch would have
//! filtered each fragment by mask; `expand` is the identity and the store
//! writes `%c` itself, so each lane still receives the value of the arm it
//! would have taken. On a scalar bool exactly one arm runs in both worlds.
//! Fragmentation differs, lane-wise content does not - the same bargain every
//! branch-removing rule here makes.
//!
//! Two premises are traded for loud failures rather than proven:
//!
//! * **`%c` is a bool.** The original branch would have sent a `Nil` down the
//!   false edge and anything truthy down the true edge; `expand` refuses
//!   both. For a button cell - initialized to a bool before the first frame,
//!   only ever stored bools - the case is unreachable, and if it ever
//!   happens, the run dies at this instruction instead of silently storing a
//!   constant.
//! * **The arms' accessor finds the cell.** Both arms name syntactically the
//!   same accessor (checked), so the rewritten single execution returns
//!   whatever cell a taken arm's execution would have. No aliasing question:
//!   the accessor is re-executed, not remembered.
//!
//! # Why the store stays
//!
//! The store back into the cell is not an optimization artifact; it is the
//! concretization. Later `btn` reads of the same button this frame must see
//! the per-lane value this read fixed - a lane that saw `true` here must see
//! `true` again - and the cell is the only thing carrying that between call
//! sites. (`__reset_button_states` puts `UnknownBool` back at the frame
//! boundary, which is also what lets lanes an expansion duplicated without
//! consequence dedup again.)

use anyhow::{anyhow, Result};

use crate::ir::{FunDef, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::{get_block, predecessors, require};

/// One arm of the diamond: accessor, bool constant, store, jump to the join.
struct Arm {
    label: Label,
    accessor_id: LocalId,
    accessor: Instruction,
    const_id: LocalId,
    store_id: LocalId,
}

/// The shape this rule accepts, re-derived identically by `apply` and
/// `verify`.
struct Site {
    condition: LocalId,
    true_arm: Arm,
    false_arm: Arm,
    join: Label,
    /// The join's phi over the two arm constants; `None` in the zero-phi
    /// variant (the value's only consumer is the concretization store).
    phi_id: Option<LocalId>,
}

impl Site {
    /// The id the `expand` is bound to: the phi's when there is one, else
    /// the dying true arm's constant id - reused, so nothing is minted.
    fn expand_id(&self) -> LocalId {
        self.phi_id.unwrap_or(self.true_arm.const_id)
    }
    /// How many join instructions the prefix replaces (the phi, if any).
    fn replaced(&self) -> usize {
        usize::from(self.phi_id.is_some())
    }
}

/// Reads one arm: exactly `<accessor>; bool <value>; store` under an
/// unconditional branch, hanging off `head` alone.
fn classify_arm(fun: &FunDef, head: &Label, target: &Label, value: bool) -> Result<(Arm, Label)> {
    let preds = predecessors(&fun.cfg);
    require(
        preds.get(&Some(target.clone())).map(|p| p.as_slice())
            == Some(&[Some(head.clone())]),
        format!("'{}' does not hang off '{}' alone", target.as_str(), head.as_str()),
    )?;
    let block = fun
        .cfg
        .named
        .get(target)
        .ok_or_else(|| anyhow!("no block '{}'", target.as_str()))?;
    require(!block.hint_normalize, format!("'{}' is a normalize block", target.as_str()))?;
    let [(accessor_id, accessor), (const_id, constant), (store_id, store)] =
        block.instructions.as_slice()
    else {
        return Err(anyhow!(
            "'{}' is not exactly accessor + bool + store",
            target.as_str()
        ));
    };
    require(
        matches!(
            accessor,
            Instruction::GetField { .. } | Instruction::GetIndex { .. } | Instruction::GetGlobal { .. }
        ),
        format!("'{}' does not start with an accessor", target.as_str()),
    )?;
    require(
        constant == &Instruction::BoolConstant { value },
        format!(
            "'{}' is the branch's {} target but does not store `bool {}`",
            target.as_str(),
            value,
            value
        ),
    )?;
    require(
        store == &(Instruction::Store { target: *accessor_id, source: *const_id }),
        format!("'{}' does not store its constant through its accessor", target.as_str()),
    )?;
    let Terminator::UnconditionalBranch { target: join } = block.terminator_kind() else {
        return Err(anyhow!("'{}' does not end in a jump", target.as_str()));
    };
    Ok((
        Arm {
            label: target.clone(),
            accessor_id: *accessor_id,
            accessor: accessor.clone(),
            const_id: *const_id,
            store_id: *store_id,
        },
        join.clone(),
    ))
}

fn site(fun: &FunDef, function: &str, head: &Label) -> Result<Site> {
    let head_block = fun
        .cfg
        .named
        .get(head)
        .ok_or_else(|| anyhow!("no block '{}' in {}", head.as_str(), function))?;
    let Terminator::ConditionalBranch { condition, true_target, false_target } =
        head_block.terminator_kind()
    else {
        return Err(anyhow!("'{}' does not end in a conditional branch", head.as_str()));
    };
    require(
        true_target != false_target,
        format!("'{}' branches to one place either way", head.as_str()),
    )?;

    let (true_arm, join_t) = classify_arm(fun, head, true_target, true)?;
    let (false_arm, join_f) = classify_arm(fun, head, false_target, false)?;
    require(
        join_t == join_f,
        format!(
            "the arms of '{}' rejoin at '{}' and '{}', not at one block",
            head.as_str(),
            join_t.as_str(),
            join_f.as_str()
        ),
    )?;
    let join = join_t;
    require(&join != head, format!("'{}' is its own join", head.as_str()))?;
    require(
        true_arm.accessor == false_arm.accessor,
        format!(
            "the arms of '{}' store through different accessors, so one \
             execution cannot stand in for either",
            head.as_str()
        ),
    )?;

    // The join: the two arms are its only predecessors, and its only phi is
    // the concretized value - the two arm constants, which `expand` will
    // reproduce per lane. Any other phi would lose an edge.
    let preds = predecessors(&fun.cfg);
    let mut join_preds = preds
        .get(&Some(join.clone()))
        .cloned()
        .unwrap_or_default();
    join_preds.sort_by_key(|k| super::label_of(k).as_str().to_string());
    let mut expected = vec![Some(true_arm.label.clone()), Some(false_arm.label.clone())];
    expected.sort_by_key(|k| super::label_of(k).as_str().to_string());
    require(
        join_preds == expected,
        format!("'{}' has predecessors besides the two arms", join.as_str()),
    )?;
    let join_block = fun
        .cfg
        .named
        .get(&join)
        .ok_or_else(|| anyhow!("join block '{}' vanished", join.as_str()))?;
    let phis: Vec<&(LocalId, Instruction)> = join_block
        .instructions
        .iter()
        .filter(|(_, i)| matches!(i, Instruction::Phi { .. }))
        .collect();
    let phi_id = match phis.as_slice() {
        // Zero-phi variant: the concretized value has no consumer besides
        // the store back into the cell (a dce'd select downstream). The
        // expand + store must still happen - lanes fork into both worlds
        // and later reads of the cell see the per-lane value - there is
        // just no phi to replace; the expand reuses the dying true arm's
        // constant id instead.
        [] => None,
        [(phi_id, Instruction::Phi { branches })] => {
            let mut sorted: Vec<(&Label, LocalId)> =
                branches.iter().map(|(l, v)| (l, *v)).collect();
            sorted.sort_by_key(|(l, _)| l.as_str().to_string());
            let mut expected: Vec<(&Label, LocalId)> = vec![
                (&true_arm.label, true_arm.const_id),
                (&false_arm.label, false_arm.const_id),
            ];
            expected.sort_by_key(|(l, _)| l.as_str().to_string());
            require(
                sorted == expected,
                format!(
                    "the phi in '{}' does not merge exactly the two arm constants",
                    join.as_str()
                ),
            )?;
            require(
                &join_block.instructions[0].0 == phi_id,
                format!("the phi in '{}' is not its first instruction", join.as_str()),
            )?;
            Some(*phi_id)
        }
        _ => {
            return Err(anyhow!(
                "'{}' must have at most one phi - the concretized bool",
                join.as_str()
            ));
        }
    };

    Ok(Site { condition: *condition, true_arm, false_arm, join, phi_id })
}

/// The instructions the join starts with afterwards. Ids are reused from the
/// site - the phi's own id and the deleted true arm's accessor and store ids
/// - so nothing is minted.
fn expected_join_prefix(s: &Site) -> Vec<(LocalId, Instruction)> {
    let expand_id = s.expand_id();
    vec![
        (expand_id, Instruction::Expand { value: s.condition }),
        (s.true_arm.accessor_id, s.true_arm.accessor.clone()),
        (
            s.true_arm.store_id,
            Instruction::Store { target: s.true_arm.accessor_id, source: expand_id },
        ),
    ]
}

pub fn apply(program: &mut Program, function: &str, head: &str) -> Result<usize> {
    let head = Label::from(head.to_string());
    let fun = program.get(function)?;
    let s = site(fun, function, &head)?;
    let prefix = expected_join_prefix(&s);

    let fun = program.get_mut(function)?;
    fun.cfg.named.remove(&s.true_arm.label);
    fun.cfg.named.remove(&s.false_arm.label);
    let head_block = fun.cfg.named.get_mut(&head).unwrap();
    head_block.terminator = (
        head_block.terminator.0,
        Terminator::UnconditionalBranch { target: s.join.clone() },
    );
    let join_block = fun.cfg.named.get_mut(&s.join).unwrap();
    join_block.instructions.splice(0..s.replaced(), prefix);

    // Ids moved blocks and live ranges changed; any slot allocation is stale.
    fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(4)
}

/// Independent check: re-derives the site from the *before* program and
/// insists the after program is exactly the prescription - head jumping
/// straight to the join, both arms gone, the join's phi replaced by
/// expand + accessor + store, and not one other thing different.
pub fn verify(before: &Program, after: &Program, function: &str, head: &str) -> Result<()> {
    let head = Label::from(head.to_string());
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let s = site(before_fun, function, &head)?;

    // The head: instructions untouched, branch now unconditional to the join.
    let before_head = before_fun.cfg.named.get(&head).unwrap();
    let after_head = after_fun
        .cfg
        .named
        .get(&head)
        .ok_or_else(|| anyhow!("expand_bool removed the head block"))?;
    require(
        after_head.instructions == before_head.instructions,
        "expand_bool changed the head's instructions",
    )?;
    require(
        after_head.terminator.0 == before_head.terminator.0
            && matches!(
                &after_head.terminator.1,
                Terminator::UnconditionalBranch { target } if target == &s.join
            ),
        "expand_bool did not make the head jump straight to the join",
    )?;

    // The arms: gone, and nothing else gone.
    for label in [&s.true_arm.label, &s.false_arm.label] {
        require(
            !after_fun.cfg.named.contains_key(label),
            format!("expand_bool left the arm '{}' behind", label.as_str()),
        )?;
    }
    require(
        after_fun.cfg.named.len() + 2 == before_fun.cfg.named.len(),
        "expand_bool changed the set of blocks beyond removing the arms",
    )?;

    // The join: the phi replaced by the prescribed prefix, the rest untouched.
    let before_join = before_fun.cfg.named.get(&s.join).unwrap();
    let after_join = after_fun
        .cfg
        .named
        .get(&s.join)
        .ok_or_else(|| anyhow!("expand_bool removed the join block"))?;
    let prefix = expected_join_prefix(&s);
    require(
        after_join.instructions.len()
            == before_join.instructions.len() + prefix.len() - s.replaced(),
        "the join does not have the prescribed number of instructions",
    )?;
    require(
        after_join.instructions[..prefix.len()] == prefix[..],
        "the join does not start with the prescribed expand + accessor + store",
    )?;
    require(
        after_join.instructions[prefix.len()..] == before_join.instructions[s.replaced()..],
        "expand_bool changed the join beyond replacing its phi",
    )?;
    require(
        after_join.terminator == before_join.terminator
            && after_join.hint_normalize == before_join.hint_normalize,
        "expand_bool changed the join's terminator",
    )?;

    // Everything else: untouched.
    for key in super::blocks_sorted(&before_fun.cfg) {
        if key == Some(head.clone())
            || key == Some(s.join.clone())
            || key == Some(s.true_arm.label.clone())
            || key == Some(s.false_arm.label.clone())
        {
            continue;
        }
        require(
            get_block(&before_fun.cfg, &key) == get_block(&after_fun.cfg, &key),
            format!(
                "expand_bool changed block '{}', which is outside the site",
                super::super::validate::block_label(&key)
            ),
        )?;
    }
    require(
        before.functions.len() == after.functions.len(),
        "expand_bool changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!("expand_bool on {} also changed {}", function, name.as_str()),
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

    fn accessor() -> Instruction {
        Instruction::GetIndex { receiver: id(1), index: id(2), create_if_missing: true }
    }

    fn arm(value: bool, base: usize) -> Block {
        Block {
            instructions: vec![
                (id(base), accessor()),
                (id(base + 1), Instruction::BoolConstant { value }),
                (
                    id(base + 2),
                    Instruction::Store { target: id(base), source: id(base + 1) },
                ),
            ],
            terminator: (
                id(base + 3),
                Terminator::UnconditionalBranch { target: label("join") },
            ),
            hint_normalize: false,
        }
    }

    /// `__entry` loads the button cell, `head` branches on it, the arms store
    /// their constants back, `join` merges the concretized value and uses it.
    fn program() -> Program {
        let mut named = crate::ir::new_label_map();
        named.insert(
            label("head"),
            Block {
                instructions: vec![
                    (id(3), Instruction::GetIndex {
                        receiver: id(1),
                        index: id(2),
                        create_if_missing: false,
                    }),
                    (id(4), Instruction::Load { source: id(3) }),
                ],
                terminator: (
                    id(9),
                    Terminator::ConditionalBranch {
                        condition: id(4),
                        true_target: label("arm_t"),
                        false_target: label("arm_f"),
                    },
                ),
                hint_normalize: false,
            },
        );
        named.insert(label("arm_t"), arm(true, 20));
        named.insert(label("arm_f"), arm(false, 25));
        named.insert(
            label("join"),
            Block {
                instructions: vec![
                    (
                        id(30),
                        Instruction::Phi {
                            branches: vec![
                                (label("arm_t"), id(21)),
                                (label("arm_f"), id(26)),
                            ],
                        },
                    ),
                    (id(31), Instruction::UnaryOp {
                        op: crate::ir::UnaryOp::Not,
                        arg: id(30),
                    }),
                ],
                terminator: (id(40), Terminator::Return { value: Some(id(31)) }),
                hint_normalize: false,
            },
        );
        let cfg = Cfg::new(
            Block {
                instructions: vec![
                    (id(1), Instruction::GetGlobal {
                        name: "__button_states".to_string(),
                        create_if_missing: false,
                    }),
                    (id(2), Instruction::NumberConstant {
                        value: crate::pico8_num::Pico8Num::from_i16(1),
                    }),
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
        Program { functions, merge_partition_cells: Vec::new() }
    }

    #[test]
    fn rewrites_the_diamond() {
        let before = program();
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "head").unwrap(), 4);
        verify(&before, &after, "f", "head").unwrap();

        let fun = after.get("f").unwrap();
        assert!(!fun.cfg.named.contains_key(&label("arm_t")));
        assert!(!fun.cfg.named.contains_key(&label("arm_f")));
        let head = fun.cfg.named.get(&label("head")).unwrap();
        assert_eq!(
            head.terminator.1,
            Terminator::UnconditionalBranch { target: label("join") }
        );
        let join = fun.cfg.named.get(&label("join")).unwrap();
        assert_eq!(join.instructions.len(), 4);
        assert_eq!(join.instructions[0], (id(30), Instruction::Expand { value: id(4) }));
        assert_eq!(join.instructions[1], (id(20), accessor()));
        assert_eq!(
            join.instructions[2],
            (id(22), Instruction::Store { target: id(20), source: id(30) })
        );
        // The rest of the join is untouched and still uses the phi's id.
        assert_eq!(
            join.instructions[3],
            (id(31), Instruction::UnaryOp { op: crate::ir::UnaryOp::Not, arg: id(30) })
        );
    }

    #[test]
    fn validates_after_apply() {
        let mut after = program();
        apply(&mut after, "f", "head").unwrap();
        let errors = super::super::super::validate::validate_program(&after);
        assert!(errors.is_empty(), "{:?}", errors);
    }

    /// The arms' constants must sit on the matching sides of the branch -
    /// swapped arms would need `not`, which this rule does not emit.
    #[test]
    fn refuses_swapped_arms() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        let head = fun.cfg.named.get_mut(&label("head")).unwrap();
        let Terminator::ConditionalBranch { true_target, false_target, .. } =
            &mut head.terminator.1
        else {
            unreachable!()
        };
        std::mem::swap(true_target, false_target);
        let error = apply(&mut before, "f", "head").unwrap_err().to_string();
        assert!(error.contains("does not store `bool"), "{}", error);
    }

    /// Arms reading different cells cannot be served by one execution.
    #[test]
    fn refuses_mismatched_accessors() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        let arm_f = fun.cfg.named.get_mut(&label("arm_f")).unwrap();
        arm_f.instructions[0].1 = Instruction::GetIndex {
            receiver: id(1),
            index: id(2),
            create_if_missing: false,
        };
        let error = apply(&mut before, "f", "head").unwrap_err().to_string();
        assert!(error.contains("different accessors"), "{}", error);
    }

    /// A second phi would lose an edge when the arms disappear.
    #[test]
    fn refuses_a_join_with_extra_phis() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        let join = fun.cfg.named.get_mut(&label("join")).unwrap();
        join.instructions.insert(
            1,
            (
                id(32),
                Instruction::Phi {
                    branches: vec![(label("arm_t"), id(20)), (label("arm_f"), id(25))],
                },
            ),
        );
        let error = apply(&mut before, "f", "head").unwrap_err().to_string();
        assert!(error.contains("at most one phi"), "{}", error);
    }

    /// A join with a predecessor besides the arms would keep an edge the phi
    /// no longer describes.
    #[test]
    fn refuses_a_join_with_other_predecessors() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        fun.cfg.named.insert(
            label("elsewhere"),
            Block {
                instructions: vec![],
                terminator: (
                    id(50),
                    Terminator::UnconditionalBranch { target: label("join") },
                ),
                hint_normalize: false,
            },
        );
        let error = apply(&mut before, "f", "head").unwrap_err().to_string();
        assert!(error.contains("predecessors besides"), "{}", error);
    }

    /// An arm with anything besides accessor + constant + store is not a
    /// concretization diamond.
    #[test]
    fn refuses_an_arm_with_extra_instructions() {
        let mut before = program();
        let fun = before.get_mut("f").unwrap();
        let arm_t = fun.cfg.named.get_mut(&label("arm_t")).unwrap();
        arm_t.instructions.push((id(23), Instruction::NilConstant));
        let error = apply(&mut before, "f", "head").unwrap_err().to_string();
        assert!(error.contains("not exactly accessor"), "{}", error);
    }

    /// The verifier is the trusted half: it must reject an applier that
    /// forgot the store-back, which would silently break the agreement
    /// between btn reads of the same button.
    #[test]
    fn verify_rejects_a_dropped_store() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        let fun = after.get_mut("f").unwrap();
        let join = fun.cfg.named.get_mut(&label("join")).unwrap();
        join.instructions.remove(2);
        let error = verify(&before, &after, "f", "head").unwrap_err().to_string();
        assert!(error.contains("prescribed number"), "{}", error);
    }

    /// ...and one that left an arm behind.
    #[test]
    fn verify_rejects_a_kept_arm() {
        let before = program();
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        let arm = before.get("f").unwrap().cfg.named.get(&label("arm_t")).unwrap().clone();
        after.get_mut("f").unwrap().cfg.named.insert(label("arm_t"), arm);
        let error = verify(&before, &after, "f", "head").unwrap_err().to_string();
        assert!(error.contains("left the arm"), "{}", error);
    }

    #[test]
    fn candidates_finds_the_head() {
        let found = candidates(&program());
        assert_eq!(found, vec![("f".to_string(), label("head"))]);
    }
}
