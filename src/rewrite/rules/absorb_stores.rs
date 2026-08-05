//! `absorb_stores` - replace a branch whose arms are bare stores with a
//! select-store in the head.
//!
//! Pointed rule: names the *head* - the block whose conditional branch is
//! removed, which is also the block the profiler attributes the splits to.
//!
//! # The shapes
//!
//! After `speculate` has emptied an arm down to its single store, two shapes
//! remain. The *diamond*, where both sides store to the same cell:
//!
//! ```text
//!   H:  br %c ? A : B              H:  %sel = select %c ? %va : %vb
//!   A:  store %p <- %va      =>        store %p <- %sel
//!       br J                           br J
//!   B:  store %p <- %vb
//!       br J
//! ```
//!
//! and the *triangle*, where one side stores and the other goes straight to
//! the join:
//!
//! ```text
//!   H:  br %c ? A : J              H:  %g = assert_value_cell %p
//!   A:  store %p <- %v       =>        %old = load %p
//!       br J                           %sel = select %c ? %v : %old
//!                                      store %p <- %sel
//!                                      br J
//! ```
//!
//! # Why this exists next to `sink_store` + `if_convert`
//!
//! The triangle pipeline moves the store *past the join* and later converts
//! the join's phi to a select. That construction needs the join to accept a
//! two-entry phi, i.e. to have exactly the head and the arm as predecessors.
//! The two sites this rule was built for do not oblige: `if_condition_92`'s
//! join has a third predecessor, and a diamond's join has two arms and no
//! head. Absorbing the store into the *head* sidesteps the join entirely -
//! the join is not touched, so it may have any number of other predecessors.
//! The price is that the join must carry no phis, which is checked.
//!
//! # Soundness
//!
//! **Diamond.** Every path through `H` stored to `%p`: through `A` the value
//! `%va`, through `B` the value `%vb`. The rewritten head stores
//! `select %c ? %va : %vb` - per lane the same value to the same cell at the
//! same moment. No load, no guard, and nothing to say about the cell's old
//! contents, because no path kept them. The one thing that makes this true is
//! that both stores name the *same target local* - same local, same cell, no
//! aliasing question. (`cse` is the rule that makes two arms agree on the
//! local; this rule refuses distinct locals rather than reasoning about
//! them.)
//!
//! **Triangle.** Identical to `sink_store`'s argument, relocated: on the
//! skipping path the cell is loaded and stored back, which is the identity
//! for a plain value cell and a silent corruption for a closure or table
//! cell - `load` on those yields a self-pointer. The `assert_value_cell`
//! guard turns that case into a loud failure before the load. Screen at full
//! depth.
//!
//! In both shapes the `select` itself is the remaining bargain: on lanes
//! where `%c`'s mask varies it refuses operands of different representations
//! loudly rather than widening them, exactly as everywhere else.
//!
//! # Arms with several stores
//!
//! An arm may hold any number of stores (the `on_ground` diamond stores
//! `grace` in both arms but `djump` only in one). Stores are paired across
//! arms **by target local**: each pair becomes the unguarded select-store,
//! each unpaired store the guarded load-back triangle. Emission order is the
//! true arm's stores in their own order, then the false arm's remainder in
//! theirs - and no aliasing question arises beyond the pair's same-local
//! argument, because every triangle emission is *load-adjacent*: it loads
//! the cell immediately before storing a select of that same value back.
//! Whatever earlier emissions wrote to that cell - even through an aliasing
//! pointer - is what the load sees, so the skipped side always reproduces
//! the cell's current value and the taken side always wins. The only shape
//! refused is one arm storing twice through one local, where pairing would
//! be ambiguous.

use anyhow::{anyhow, Result};

use crate::ir::{FunDef, Instruction, Label, LocalId, Terminator};

use super::super::program::Program;
use super::{get_block, predecessors, require, LocalIdAllocator};

/// An arm: a block holding only stores, in order.
struct Arm {
    label: Label,
    /// The stores' (target pointer, value) locals, in block order.
    stores: Vec<(LocalId, LocalId)>,
}

/// The shape this rule accepts, re-derived identically by `apply`, `verify`
/// and `candidates`.
struct Site {
    condition: LocalId,
    join: Label,
    /// The branch's true side, if it is an arm rather than the join.
    true_arm: Option<Arm>,
    /// The branch's false side, likewise. At least one side is an arm.
    false_arm: Option<Arm>,
}

/// Classifies one branch target: `Some` if it is a bare-store arm hanging off
/// `head` alone, `None` if it is anything else (then it must be the join).
fn classify(fun: &FunDef, head: &Label, target: &Label) -> Option<(Arm, Label)> {
    let preds = predecessors(&fun.cfg);
    if preds.get(&Some(target.clone())).map(|p| p.as_slice())
        != Some(&[Some(head.clone())])
    {
        return None;
    }
    let block = fun.cfg.named.get(target)?;
    if block.instructions.is_empty() {
        return None;
    }
    let mut stores = Vec::new();
    for (_, instr) in &block.instructions {
        let Instruction::Store { target: store_target, source } = instr else {
            return None;
        };
        stores.push((*store_target, *source));
    }
    let Terminator::UnconditionalBranch { target: join } = block.terminator_kind() else {
        return None;
    };
    if block.hint_normalize {
        return None;
    }
    Some((Arm { label: target.clone(), stores }, join.clone()))
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
        return Err(anyhow!(
            "'{}' does not end in a conditional branch",
            head.as_str()
        ));
    };
    require(
        true_target != false_target,
        format!("'{}' branches to one place either way", head.as_str()),
    )?;

    let true_side = classify(fun, head, true_target);
    let false_side = classify(fun, head, false_target);
    let (join, true_arm, false_arm) = match (true_side, false_side) {
        (Some((a, ja)), Some((b, jb))) => {
            require(
                ja == jb,
                format!(
                    "the arms of '{}' rejoin at '{}' and '{}', not at one block",
                    head.as_str(),
                    ja.as_str(),
                    jb.as_str()
                ),
            )?;
            (ja, Some(a), Some(b))
        }
        (Some((a, ja)), None) => {
            require(
                &ja == false_target,
                format!(
                    "the arm '{}' rejoins at '{}', not at the branch's other \
                     target '{}'",
                    a.label.as_str(),
                    ja.as_str(),
                    false_target.as_str()
                ),
            )?;
            (ja, Some(a), None)
        }
        (None, Some((b, jb))) => {
            require(
                &jb == true_target,
                format!(
                    "the arm '{}' rejoins at '{}', not at the branch's other \
                     target '{}'",
                    b.label.as_str(),
                    jb.as_str(),
                    true_target.as_str()
                ),
            )?;
            (jb, None, Some(b))
        }
        (None, None) => {
            return Err(anyhow!(
                "neither target of '{}' is a bare-store arm; speculate first",
                head.as_str()
            ));
        }
    };
    require(
        &join != head,
        format!("'{}' is its own join; refusing a loop", head.as_str()),
    )?;

    // Pairing is by target local, so one local twice in one arm would be
    // ambiguous. Distinct locals need no such care: every emission except the
    // same-local pair is load-adjacent (load the cell, select, store it
    // back), which computes the right value whatever the cells turn out to
    // alias, in whatever order the stores land.
    for arm in true_arm.iter().chain(false_arm.iter()) {
        for (i, (target, _)) in arm.stores.iter().enumerate() {
            require(
                arm.stores[..i].iter().all(|(other, _)| other != target),
                format!(
                    "the arm '{}' stores twice through %{}",
                    arm.label.as_str(),
                    usize::from(*target)
                ),
            )?;
        }
    }

    // The join is left untouched, so nothing in it may depend on which edge
    // control arrived by.
    let join_block = fun
        .cfg
        .named
        .get(&join)
        .ok_or_else(|| anyhow!("join block '{}' vanished", join.as_str()))?;
    require(
        !join_block
            .instructions
            .iter()
            .any(|(_, i)| matches!(i, Instruction::Phi { .. })),
        format!(
            "the join '{}' has phis, which would lose an edge when the arm \
             disappears",
            join.as_str()
        ),
    )?;

    Ok(Site { condition: *condition, join, true_arm, false_arm })
}

/// One store the head will absorb.
enum Emission {
    /// Both arms stored through the same local: one select, no guard.
    Pair { target: LocalId, if_true: LocalId, if_false: LocalId },
    /// One arm stored, the other kept the old value: the guarded load-back.
    Triangle { target: LocalId, source: LocalId, arm_is_true: bool },
}

/// The absorbed stores in emission order: the true arm's stores in their own
/// order (each either paired with the false arm's same-local store or a
/// triangle), then the false arm's remaining stores in theirs.
fn emissions(s: &Site) -> Vec<Emission> {
    match (&s.true_arm, &s.false_arm) {
        (Some(a), Some(b)) => {
            let mut paired = vec![false; b.stores.len()];
            let mut out = Vec::new();
            for (target, if_true) in &a.stores {
                match b.stores.iter().position(|(other, _)| other == target) {
                    Some(i) => {
                        paired[i] = true;
                        out.push(Emission::Pair {
                            target: *target,
                            if_true: *if_true,
                            if_false: b.stores[i].1,
                        });
                    }
                    None => out.push(Emission::Triangle {
                        target: *target,
                        source: *if_true,
                        arm_is_true: true,
                    }),
                }
            }
            for (i, (target, source)) in b.stores.iter().enumerate() {
                if !paired[i] {
                    out.push(Emission::Triangle {
                        target: *target,
                        source: *source,
                        arm_is_true: false,
                    });
                }
            }
            out
        }
        (one_arm, other) => {
            let arm = one_arm.as_ref().or(other.as_ref()).unwrap();
            let arm_is_true = one_arm.is_some();
            arm.stores
                .iter()
                .map(|(target, source)| Emission::Triangle {
                    target: *target,
                    source: *source,
                    arm_is_true,
                })
                .collect()
        }
    }
}

/// The instructions the head gains, given the minted ids in order.
fn expected_tail(s: &Site, minted: &[LocalId]) -> Vec<(LocalId, Instruction)> {
    let mut out = Vec::new();
    let mut next = minted.iter();
    let mut fresh = || *next.next().unwrap();
    for emission in emissions(s) {
        match emission {
            Emission::Pair { target, if_true, if_false } => {
                let sel = fresh();
                let st = fresh();
                out.push((
                    sel,
                    Instruction::Select { condition: s.condition, if_true, if_false },
                ));
                out.push((st, Instruction::Store { target, source: sel }));
            }
            Emission::Triangle { target, source, arm_is_true } => {
                let guard = fresh();
                let old = fresh();
                let sel = fresh();
                let st = fresh();
                let (if_true, if_false) =
                    if arm_is_true { (source, old) } else { (old, source) };
                out.push((guard, Instruction::AssertValueCell { target }));
                out.push((old, Instruction::Load { source: target }));
                out.push((
                    sel,
                    Instruction::Select { condition: s.condition, if_true, if_false },
                ));
                out.push((st, Instruction::Store { target, source: sel }));
            }
        }
    }
    out
}

fn arm_labels(s: &Site) -> Vec<Label> {
    s.true_arm
        .iter()
        .chain(s.false_arm.iter())
        .map(|a| a.label.clone())
        .collect()
}

fn minted_count(s: &Site) -> usize {
    emissions(s)
        .iter()
        .map(|e| match e {
            Emission::Pair { .. } => 2,
            Emission::Triangle { .. } => 4,
        })
        .sum()
}

pub fn apply(program: &mut Program, function: &str, head: &str) -> Result<usize> {
    let head = Label::from(head.to_string());
    let fun = program.get(function)?;
    let s = site(fun, function, &head)?;
    let mut ids = LocalIdAllocator::for_function(fun);
    let minted: Vec<LocalId> = (0..minted_count(&s)).map(|_| ids.fresh()).collect();
    let tail = expected_tail(&s, &minted);
    let removed = arm_labels(&s);

    let fun = program.get_mut(function)?;
    for label in &removed {
        fun.cfg.named.remove(label);
    }
    let head_block = fun.cfg.named.get_mut(&head).unwrap();
    head_block.instructions.extend(tail);
    head_block.terminator = (
        head_block.terminator.0,
        Terminator::UnconditionalBranch { target: s.join.clone() },
    );

    // Arm terminator ids are gone and live ranges have changed; any existing
    // slot allocation is stale. Re-run `allocate_slots` afterwards.
    fun.cfg.slots = std::sync::Arc::new(crate::ir::SlotMap::identity());
    Ok(1 + removed.len())
}

/// Independent check: re-derives the site from the *before* program, reads
/// the minted ids out of the after program, checks they are fresh and
/// distinct, and insists the after program is exactly the prescription -
/// head extended by the select-store tail under an unconditional branch, the
/// arms gone, and not one other thing different.
pub fn verify(before: &Program, after: &Program, function: &str, head: &str) -> Result<()> {
    let head = Label::from(head.to_string());
    let before_fun = before.get(function)?;
    let after_fun = after.get(function)?;
    let s = site(before_fun, function, &head)?;
    let removed = arm_labels(&s);

    // The head: unchanged prefix, then the prescribed tail.
    let before_head = before_fun.cfg.named.get(&head).unwrap();
    let after_head = after_fun
        .cfg
        .named
        .get(&head)
        .ok_or_else(|| anyhow!("absorb_stores removed the head block"))?;
    let prefix = before_head.instructions.len();
    let count = minted_count(&s);
    require(
        after_head.instructions.len() == prefix + count,
        format!(
            "absorb_stores must add exactly {} instructions to the head",
            count
        ),
    )?;
    require(
        after_head.instructions[..prefix] == before_head.instructions[..],
        "absorb_stores changed the head's existing instructions",
    )?;
    let minted: Vec<LocalId> = after_head.instructions[prefix..]
        .iter()
        .map(|(id, _)| *id)
        .collect();
    for (i, id) in minted.iter().enumerate() {
        require(
            super::super::validate::all_blocks(&before_fun.cfg).into_iter().all(
                |(_, block)| {
                    block.instructions.iter().all(|(other, _)| other != id)
                        && block.terminator.0 != *id
                },
            ),
            format!("minted id %{} already existed", usize::from(*id)),
        )?;
        require(
            minted[..i].iter().all(|earlier| earlier != id),
            format!("minted id %{} used twice", usize::from(*id)),
        )?;
    }
    let expected = expected_tail(&s, &minted);
    require(
        after_head.instructions[prefix..] == expected[..],
        "the head's new instructions are not the prescribed select-store tail",
    )?;
    require(
        after_head.terminator.0 == before_head.terminator.0
            && matches!(
                &after_head.terminator.1,
                Terminator::UnconditionalBranch { target } if target == &s.join
            ),
        "absorb_stores did not make the head branch unconditionally to the join",
    )?;

    // The arms: gone, and nothing else gone.
    for label in &removed {
        require(
            !after_fun.cfg.named.contains_key(label),
            format!("absorb_stores left the arm '{}' behind", label.as_str()),
        )?;
    }
    require(
        after_fun.cfg.named.len() + removed.len() == before_fun.cfg.named.len(),
        "absorb_stores changed the set of blocks beyond removing the arms",
    )?;

    // Everything else: untouched.
    for key in super::blocks_sorted(&before_fun.cfg) {
        if key == Some(head.clone()) || removed.iter().any(|l| key == Some(l.clone())) {
            continue;
        }
        require(
            get_block(&before_fun.cfg, &key) == get_block(&after_fun.cfg, &key),
            format!(
                "absorb_stores changed block '{}', which is outside the site",
                super::super::validate::block_label(&key)
            ),
        )?;
    }
    require(
        before.functions.len() == after.functions.len(),
        "absorb_stores changed the set of functions",
    )?;
    for (name, before_other) in &before.functions {
        if name.as_str() == function {
            continue;
        }
        require(
            after.functions.get(name) == Some(before_other),
            format!(
                "absorb_stores on {} also changed {}",
                function,
                name.as_str()
            ),
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

    fn num(n: i16) -> Instruction {
        Instruction::NumberConstant { value: crate::pico8_num::Pico8Num::from_i16(n) }
    }

    fn store(target: usize, source: usize) -> Instruction {
        Instruction::Store { target: id(target), source: id(source) }
    }

    fn arm_block(body: Vec<(LocalId, Instruction)>, terminator_id: usize) -> Block {
        Block {
            instructions: body,
            terminator: (
                id(terminator_id),
                Terminator::UnconditionalBranch { target: label("join") },
            ),
            hint_normalize: false,
        }
    }

    /// `__entry` computes a pointer and two values, then `head` branches to
    /// the given targets; `join` returns.
    fn program(
        true_block: Option<Block>,
        false_block: Option<Block>,
        join_instructions: Vec<(LocalId, Instruction)>,
    ) -> Program {
        let mut named = crate::ir::new_label_map();
        let target = |arm: &Option<Block>, name: &str| {
            if arm.is_some() {
                label(name)
            } else {
                label("join")
            }
        };
        named.insert(
            label("head"),
            Block {
                instructions: vec![],
                terminator: (
                    id(9),
                    Terminator::ConditionalBranch {
                        condition: id(3),
                        true_target: target(&true_block, "arm_t"),
                        false_target: target(&false_block, "arm_f"),
                    },
                ),
                hint_normalize: false,
            },
        );
        if let Some(block) = true_block {
            named.insert(label("arm_t"), block);
        }
        if let Some(block) = false_block {
            named.insert(label("arm_f"), block);
        }
        named.insert(
            label("join"),
            Block {
                instructions: join_instructions,
                terminator: (id(40), Terminator::Return { value: None }),
                hint_normalize: false,
            },
        );
        let cfg = Cfg::new(
            Block {
                instructions: vec![
                    (id(1), Instruction::GetField {
                        receiver: id(0),
                        field: "x".to_string(),
                        create_if_missing: false,
                    }),
                    (id(3), Instruction::BoolConstant { value: true }),
                    (id(4), num(7)),
                    (id(5), num(9)),
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
                arg_ids: vec![Some(id(0))],
                cfg,
                source_span: None,
            },
        );
        Program { functions, merge_partition_cells: Vec::new() }
    }

    fn head_tail(program: &Program) -> Vec<(LocalId, Instruction)> {
        program.get("f").unwrap().cfg.named.get(&label("head")).unwrap().instructions.clone()
    }

    /// The diamond: both arms store to the same local, so the head gains a
    /// bare select-store with no guard and no load.
    #[test]
    fn absorbs_a_same_cell_diamond() {
        let before = program(
            Some(arm_block(vec![(id(20), store(1, 4))], 21)),
            Some(arm_block(vec![(id(25), store(1, 5))], 26)),
            vec![],
        );
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "head").unwrap(), 3);
        verify(&before, &after, "f", "head").unwrap();

        let tail = head_tail(&after);
        assert_eq!(tail.len(), 2);
        assert_eq!(
            tail[0].1,
            Instruction::Select { condition: id(3), if_true: id(4), if_false: id(5) }
        );
        assert_eq!(tail[1].1, Instruction::Store { target: id(1), source: tail[0].0 });
        let fun = after.get("f").unwrap();
        assert!(!fun.cfg.named.contains_key(&label("arm_t")));
        assert!(!fun.cfg.named.contains_key(&label("arm_f")));
    }

    /// The triangle, arm on the true side: guard, load, select, store.
    #[test]
    fn absorbs_a_true_arm_triangle() {
        let before = program(Some(arm_block(vec![(id(20), store(1, 4))], 21)), None, vec![]);
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "head").unwrap(), 2);
        verify(&before, &after, "f", "head").unwrap();

        let tail = head_tail(&after);
        assert_eq!(tail.len(), 4);
        assert_eq!(tail[0].1, Instruction::AssertValueCell { target: id(1) });
        assert_eq!(tail[1].1, Instruction::Load { source: id(1) });
        assert_eq!(
            tail[2].1,
            Instruction::Select { condition: id(3), if_true: id(4), if_false: tail[1].0 }
        );
        assert_eq!(tail[3].1, Instruction::Store { target: id(1), source: tail[2].0 });
    }

    /// ...and on the false side, with the select's operands swapped.
    #[test]
    fn absorbs_a_false_arm_triangle() {
        let before = program(None, Some(arm_block(vec![(id(25), store(1, 5))], 26)), vec![]);
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "head").unwrap(), 2);
        verify(&before, &after, "f", "head").unwrap();

        let tail = head_tail(&after);
        assert_eq!(
            tail[2].1,
            Instruction::Select { condition: id(3), if_true: tail[1].0, if_false: id(5) }
        );
    }

    /// The join is untouched, so a join with extra predecessors is fine -
    /// this is the shape `sink_store` + `if_convert` cannot reach.
    #[test]
    fn accepts_a_join_with_other_predecessors() {
        let mut before = program(Some(arm_block(vec![(id(20), store(1, 4))], 21)), None, vec![]);
        let fun = before.functions.values_mut().next().unwrap();
        fun.cfg.named.insert(
            label("elsewhere"),
            Block {
                instructions: vec![],
                terminator: (
                    id(35),
                    Terminator::UnconditionalBranch { target: label("join") },
                ),
                hint_normalize: false,
            },
        );
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "head").unwrap(), 2);
        verify(&before, &after, "f", "head").unwrap();
    }

    /// Two arms storing to different locals may or may not be the same cell;
    /// this rule refuses to guess.
    #[test]
    fn absorbs_a_diamond_with_distinct_targets_as_two_triangles() {
        let mut before = program(
            Some(arm_block(vec![(id(20), store(1, 4))], 21)),
            Some(arm_block(vec![(id(25), store(2, 5))], 26)),
            vec![],
        );
        let fun = before.functions.values_mut().next().unwrap();
        fun.cfg.entry.instructions.insert(
            1,
            (id(2), Instruction::GetField {
                receiver: id(0),
                field: "y".to_string(),
                create_if_missing: false,
            }),
        );
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "head").unwrap(), 3);
        verify(&before, &after, "f", "head").unwrap();
        let tail = head_tail(&after);
        assert_eq!(tail.len(), 8);
        // True side first: guard, load, select (arm value on the true side),
        // store; then the false side with the arm value on the false side.
        assert_eq!(tail[0].1, Instruction::AssertValueCell { target: id(1) });
        assert_eq!(
            tail[2].1,
            Instruction::Select { condition: id(3), if_true: id(4), if_false: tail[1].0 }
        );
        assert_eq!(tail[4].1, Instruction::AssertValueCell { target: id(2) });
        assert_eq!(
            tail[6].1,
            Instruction::Select { condition: id(3), if_true: tail[5].0, if_false: id(5) }
        );
    }

    /// The `on_ground` shape: one arm stores through two locals, the other
    /// through one of them. The shared local pairs into an unguarded
    /// select-store, the other gets the guarded load-back.
    #[test]
    fn absorbs_a_two_store_arm() {
        let mut before = program(
            Some(arm_block(vec![(id(20), store(1, 4)), (id(22), store(2, 5))], 21)),
            Some(arm_block(vec![(id(25), store(1, 5))], 26)),
            vec![],
        );
        let fun = before.functions.values_mut().next().unwrap();
        fun.cfg.entry.instructions.insert(
            1,
            (id(2), Instruction::GetField {
                receiver: id(0),
                field: "y".to_string(),
                create_if_missing: false,
            }),
        );
        let mut after = before.clone();
        assert_eq!(apply(&mut after, "f", "head").unwrap(), 3);
        verify(&before, &after, "f", "head").unwrap();
        let tail = head_tail(&after);
        assert_eq!(tail.len(), 6);
        // The pair for %1 first (true arm's first store), then the triangle
        // for %2.
        assert_eq!(
            tail[0].1,
            Instruction::Select { condition: id(3), if_true: id(4), if_false: id(5) }
        );
        assert_eq!(tail[1].1, Instruction::Store { target: id(1), source: tail[0].0 });
        assert_eq!(tail[2].1, Instruction::AssertValueCell { target: id(2) });
        assert_eq!(tail[3].1, Instruction::Load { source: id(2) });
        assert_eq!(
            tail[4].1,
            Instruction::Select { condition: id(3), if_true: id(5), if_false: tail[3].0 }
        );
        assert_eq!(tail[5].1, Instruction::Store { target: id(2), source: tail[4].0 });
    }

    /// One local twice in one arm would make pairing ambiguous.
    #[test]
    fn refuses_an_arm_storing_twice_through_one_local() {
        let before = program(
            Some(arm_block(vec![(id(20), store(1, 4)), (id(22), store(1, 5))], 21)),
            Some(arm_block(vec![(id(25), store(1, 5))], 26)),
            vec![],
        );
        let mut after = before.clone();
        let error = apply(&mut after, "f", "head").unwrap_err().to_string();
        assert!(error.contains("stores twice through"), "{}", error);
    }

    /// An arm holding anything besides its store is `speculate`'s leftover
    /// work, not this rule's.
    #[test]
    fn refuses_an_arm_with_extra_instructions() {
        let before = program(
            Some(arm_block(vec![(id(19), num(1)), (id(20), store(1, 19))], 21)),
            None,
            vec![],
        );
        let mut after = before.clone();
        let error = apply(&mut after, "f", "head").unwrap_err().to_string();
        assert!(error.contains("bare-store arm"), "{}", error);
    }

    /// A join with phis would lose an edge when the arm disappears.
    #[test]
    fn refuses_a_join_with_phis() {
        let before = program(
            Some(arm_block(vec![(id(20), store(1, 4))], 21)),
            None,
            vec![(
                id(30),
                Instruction::Phi {
                    branches: vec![(label("head"), id(4)), (label("arm_t"), id(5))],
                },
            )],
        );
        let mut after = before.clone();
        let error = apply(&mut after, "f", "head").unwrap_err().to_string();
        assert!(error.contains("phis"), "{}", error);
    }

    /// The verifier is the trusted half: it must reject a select whose
    /// operands were swapped.
    #[test]
    fn verify_rejects_a_swapped_select() {
        let before = program(
            Some(arm_block(vec![(id(20), store(1, 4))], 21)),
            Some(arm_block(vec![(id(25), store(1, 5))], 26)),
            vec![],
        );
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        let fun = after.get_mut("f").unwrap();
        let head = fun.cfg.named.get_mut(&label("head")).unwrap();
        let n = head.instructions.len();
        let (_, Instruction::Select { if_true, if_false, .. }) =
            &mut head.instructions[n - 2]
        else {
            panic!("expected the select");
        };
        std::mem::swap(if_true, if_false);
        let error = verify(&before, &after, "f", "head").unwrap_err().to_string();
        assert!(error.contains("prescribed"), "{}", error);
    }

    /// ...and an applier that kept an arm around.
    #[test]
    fn verify_rejects_a_kept_arm() {
        let before = program(
            Some(arm_block(vec![(id(20), store(1, 4))], 21)),
            Some(arm_block(vec![(id(25), store(1, 5))], 26)),
            vec![],
        );
        let mut after = before.clone();
        apply(&mut after, "f", "head").unwrap();
        let arm = before.get("f").unwrap().cfg.named.get(&label("arm_f")).unwrap().clone();
        after.get_mut("f").unwrap().cfg.named.insert(label("arm_f"), arm);
        let error = verify(&before, &after, "f", "head").unwrap_err().to_string();
        assert!(error.contains("left the arm"), "{}", error);
    }

    /// Both sites named by `candidates`, and only those.
    #[test]
    fn candidates_finds_the_head() {
        let before = program(
            Some(arm_block(vec![(id(20), store(1, 4))], 21)),
            Some(arm_block(vec![(id(25), store(1, 5))], 26)),
            vec![],
        );
        let found = candidates(&before);
        assert_eq!(found, vec![("f".to_string(), label("head"))]);
    }
}
