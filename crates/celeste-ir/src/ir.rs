use std::hash::BuildHasherDefault;
use std::sync::Arc;

use rustc_hash::FxHasher;
use serde::{Deserialize, Serialize};

use celeste_core::pico8_num::Pico8Num;

type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

/// A source location representing a position in the original Lua source code.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourcePosition {
    /// 1-indexed line number
    pub line: usize,
    /// 1-indexed column number (character on the line)
    pub column: usize,
    /// Byte offset from the start of the file
    pub bytes: usize,
}

/// A span in the source code, from start to end position.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceSpan {
    pub start: SourcePosition,
    pub end: SourcePosition,
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct LocalId(usize);

impl From<LocalId> for usize {
    fn from(id: LocalId) -> Self {
        id.0
    }
}

impl From<usize> for LocalId {
    fn from(id: usize) -> Self {
        Self(id)
    }
}

pub struct LocalIdGenerator {
    next_id: usize,
}

impl LocalIdGenerator {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    pub fn next(&mut self) -> LocalId {
        let id = LocalId(self.next_id);
        self.next_id += 1;
        id
    }
}

pub struct UniqueStringGenerator<T: From<String>> {
    _item_type: std::marker::PhantomData<T>,
    next_id: usize,
}

impl<T: From<String>> UniqueStringGenerator<T> {
    pub fn new() -> Self {
        Self {
            _item_type: std::marker::PhantomData,
            next_id: 0,
        }
    }

    pub fn next(&mut self, base_name: &str) -> T {
        let id = T::from(format!("{}_{}", base_name, self.next_id));
        self.next_id += 1;
        id
    }
}

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Serialize, Deserialize)]
pub struct GlobalId(String);

impl GlobalId {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for GlobalId {
    fn from(s: String) -> Self {
        Self(s)
    }
}

pub type GlobalIdGenerator = UniqueStringGenerator<GlobalId>;

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Serialize, Deserialize)]
pub struct Label(String);

impl Label {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for Label {
    fn from(s: String) -> Self {
        Self(s)
    }
}

pub type LabelGenerator = UniqueStringGenerator<Label>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
    Minus,
    Not,
    Hash,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum BinaryOp {
    Caret,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    Minus,
    Percent,
    Plus,
    Slash,
    Star,
    TildeEqual,
    TwoDots,
    TwoEqual,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    Alloc,
    GetGlobal {
        name: String,
        create_if_missing: bool,
    },
    Load {
        source: LocalId,
    },
    Store {
        target: LocalId,
        source: LocalId,
    },
    StoreEmptyTable {
        target: LocalId,
    },
    StoreClosure {
        target: LocalId,
        fun_def: GlobalId,
        captures: Vec<LocalId>,
    },
    GetField {
        receiver: LocalId,
        field: String,
        create_if_missing: bool,
    },
    GetIndex {
        receiver: LocalId,
        index: LocalId,
        create_if_missing: bool,
    },
    NumberConstant {
        value: Pico8Num,
    },
    BoolConstant {
        value: bool,
    },
    StringConstant {
        value: String,
    },
    NilConstant,
    Call {
        closure: LocalId,
        args: Vec<LocalId>,
    },
    UnaryOp {
        op: UnaryOp,
        arg: LocalId,
    },
    BinaryOp {
        left: LocalId,
        op: BinaryOp,
        right: LocalId,
    },
    Phi {
        branches: Vec<(Label, LocalId)>,
    },
    /// Per-lane choice between two values. What a `Phi` becomes once the
    /// branch that produced it is gone (see `rules::if_convert`).
    ///
    /// The point of the whole rewrite effort: a `Phi` requires the state to
    /// have been split so that each arm saw only its own lanes, and splitting
    /// is what makes a frame end as ~576 fragments. A `Select` needs no split.
    ///
    /// It is deliberately **partial**. The value representation carries one
    /// type tag and one `HeapId` per value rather than per lane, so two
    /// different pointers, or a bool and a number, cannot be combined into one
    /// value. Those cases are an error rather than a widening - a rewrite that
    /// produces one is rejected by differential verification instead of
    /// quietly losing information.
    Select {
        condition: LocalId,
        if_true: LocalId,
        if_false: LocalId,
    },
    /// Drops locals from the environment: deadness, written into the IR.
    ///
    /// The interpreter carries a `LocalEnv` full of per-lane vectors, and
    /// every branch filter gathers *all* of them, every mid-frame merge keys
    /// on *all* of them. A value the program will never read again therefore
    /// keeps costing lane copies, and - because `local_env` is part of
    /// `StateShape` - a dead temporary left behind on one path stops that
    /// state merging with an otherwise identical one. The interpreter cannot
    /// know a value is dead; `src/liveness.rs` is a no-op stub and the
    /// runtime hook it feeds has never done anything.
    ///
    /// So deadness is computed once, at rewrite time, and named explicitly
    /// here. `rules::kill_dead` places these; `validate` proves independently
    /// that nothing reads a killed local afterwards. `get_used_locals`
    /// deliberately returns nothing for this instruction - a kill is not a
    /// use, and reporting one would keep the value alive in every liveness
    /// analysis, including the one that places kills.
    Kill {
        values: Vec<LocalId>,
    },
    /// Fails execution unless `value` is a pointer to a closure of `fun_def`
    /// whose captured values are exactly `captures`.
    ///
    /// This is what makes `inline` sound without any analysis: rather than
    /// proving that a dynamic call always reaches a particular function, the
    /// rewrite asserts it and splices the body in. If the assertion ever fails
    /// we find out loudly instead of silently running the wrong code.
    ///
    /// The same trick extends to captures. Inlining a closure body needs a
    /// local bound to each of the callee's `capture_ids`, and nothing at a call
    /// site holds a captured value - `%f = get_field %o.collide; call (load %f)`
    /// mentions the receiver, not the capture. That the receiver *is* the
    /// capture is true of Celeste's objects but is not something the rewrite
    /// proves; it asserts it here instead.
    ///
    /// Cheap at runtime: a closure pointer is a scalar, not a per-lane value,
    /// so this is one check per state rather than per lane, and it never splits
    /// the state set.
    AssertClosure {
        value: LocalId,
        fun_def: GlobalId,
        captures: Vec<LocalId>,
    },
    /// Fails unless `value` is a real pointer rather than a `NilPointer`.
    ///
    /// The guard behind `rules::demote_create`. Turning `get_field %r.f create`
    /// into a plain read is only valid where the field already exists; where it
    /// does not, the read yields a `NilPointer` and the rewritten program would
    /// differ from the original. This says so out loud at the point the claim is
    /// made, rather than leaving it to whatever happens to dereference the
    /// result later.
    ///
    /// A `NilPointer` carries a hint describing what was missing (`field x`,
    /// `global y`), so the failure needs no extra data in the instruction to
    /// explain itself.
    ///
    /// Cheap at runtime for the same reason `AssertClosure` is: whether a field
    /// exists is a property of the state's heap, not of a lane, so this is one
    /// tag check per state and it never splits the state set.
    AssertPointer {
        value: LocalId,
    },
    /// Fails execution unless the cell `target` points at holds a plain value
    /// - not a closure and not a table.
    ///
    /// Planted by `rules::sink_store`, which turns a conditional `store` into
    /// a load before the branch, a phi at the join and an unconditional store
    /// after it. On the path that skipped the arm, that stores the loaded
    /// value back - which is only the identity for a plain-value cell.
    /// `load` on a closure or table cell yields a *pointer to the cell
    /// itself*, and storing that back would overwrite the closure or table
    /// with a self-pointer, silently. This says the premise out loud instead:
    /// the one place the roundtrip is not the identity is the one place this
    /// fails, before the load happens.
    ///
    /// Like `AssertPointer`, this is a property of the state's heap rather
    /// than of a lane: one tag check per state, never a split.
    AssertValueCell {
        target: LocalId,
    },
    /// Fails execution unless `value` is a bool that is true on every lane.
    ///
    /// The general-purpose loud guard: any premise that can be phrased as a
    /// boolean over existing instructions gets computed with ordinary
    /// arithmetic and then stated here. First user is `rules::speculate_region`,
    /// which asserts a speculated loop's bound is small enough that its
    /// counter can never wrap before exceeding it - turning a non-termination
    /// argument into a range check. The planned loop unroll will state its
    /// trip-count bound the same way.
    ///
    /// Unlike the guards above, this one is per-lane, not per-state: a vector
    /// bool must be true in every lane, and `UnknownBool` fails - a guard
    /// whose premise cannot be confirmed must not pass silently.
    AssertTrue {
        value: LocalId,
    },
    /// A `Call` whose callee has been pinned to a named builtin.
    ///
    /// Exactly `Call`, after asserting that `callee` holds `BuiltinFun(name)`.
    /// The point is not speed - it is that a `Call` says nothing about what it
    /// calls, so `if_convert` has to assume the worst and refuse to speculate
    /// it. Naming the callee lets the whitelist in `fixed_env::PURE_BUILTINS`
    /// answer the question instead.
    ///
    /// That whitelist is the instruction's invariant, not a hint: `name` is
    /// always a pure builtin, because `rules::pin_builtin` refuses to write
    /// anything else and its verifier re-checks it. Several passes depend on
    /// that - `cse` does not treat this as a barrier, which would be wrong for
    /// a builtin that could touch the heap.
    ///
    /// The implementation behind the name is the same function a `call` would
    /// have reached, so pinning cannot change what is computed.
    CallBuiltin {
        callee: LocalId,
        name: String,
        args: Vec<LocalId>,
    },
    /// Concretize an unknown bool by *lane expansion* instead of by branching.
    ///
    /// On an `UnknownBool`: every lane of the state is duplicated - the first
    /// copy of the lane takes `true`, the second `false` - and the result is
    /// the per-lane vector bool over the doubled state. On a value that is
    /// already a concrete bool, scalar or vector, this is the identity, which
    /// is what a concrete run with fixed inputs sees. Anything else fails
    /// loudly.
    ///
    /// This is the branch-free counterpart of what a conditional branch on an
    /// `UnknownBool` does today: the branch sends the whole state down *both*
    /// edges, and each fragment stores one constant back into the cell it was
    /// loaded from (`btn`'s concretization diamond). Expansion produces the
    /// same set of lanes - each old lane once with `true` and once with
    /// `false` - in one state instead of two fragments, which is exactly the
    /// input fan-out the search intends. See `rules::expand_bool`.
    ///
    /// Unlike every other instruction, it rewrites the whole state (every
    /// vector doubles), so it has "side effects" for every rule's purposes:
    /// never dead, never speculated, never deduplicated.
    Expand {
        value: LocalId,
    },
}

impl Instruction {
    /// Rename the block labels this instruction mentions. Only `Phi` does.
    pub fn map_labels(&self, f: &impl Fn(&Label) -> Label) -> Self {
        match self {
            Self::Phi { branches } => Self::Phi {
                branches: branches.iter().map(|(label, id)| (f(label), *id)).collect(),
            },
            other => other.clone(),
        }
    }

    pub fn map_local_ids(&self, mut f: impl FnMut(LocalId) -> LocalId) -> Self {
        match self {
            Self::Alloc => Self::Alloc,
            Self::GetGlobal {
                name,
                create_if_missing,
            } => Self::GetGlobal {
                name: name.clone(),
                create_if_missing: *create_if_missing,
            },
            Self::Load { source } => Self::Load { source: f(*source) },
            Self::Store { target, source } => Self::Store {
                target: f(*target),
                source: f(*source),
            },
            Self::StoreEmptyTable { target } => Self::StoreEmptyTable { target: f(*target) },
            Self::StoreClosure {
                target,
                fun_def,
                captures,
            } => Self::StoreClosure {
                target: f(*target),
                fun_def: fun_def.clone(),
                captures: captures.iter().map(|id| f(*id)).collect(),
            },
            Self::GetField {
                receiver,
                field,
                create_if_missing,
            } => Self::GetField {
                receiver: f(*receiver),
                field: field.clone(),
                create_if_missing: *create_if_missing,
            },
            &Self::GetIndex {
                receiver,
                index,
                create_if_missing,
            } => Self::GetIndex {
                receiver: f(receiver),
                index: f(index),
                create_if_missing: create_if_missing,
            },
            Self::NumberConstant { value } => Self::NumberConstant { value: *value },
            Self::BoolConstant { value } => Self::BoolConstant { value: *value },
            Self::StringConstant { value } => Self::StringConstant {
                value: value.clone(),
            },
            Self::NilConstant => Self::NilConstant,
            Self::Call { closure, args } => Self::Call {
                closure: f(*closure),
                args: args.iter().map(|id| f(*id)).collect(),
            },
            Self::UnaryOp { op, arg } => Self::UnaryOp {
                op: op.clone(),
                arg: f(*arg),
            },
            Self::BinaryOp { left, op, right } => Self::BinaryOp {
                left: f(*left),
                op: op.clone(),
                right: f(*right),
            },
            Self::Phi { branches } => Self::Phi {
                branches: branches
                    .iter()
                    .map(|(label, id)| (label.clone(), f(*id)))
                    .collect(),
            },
            Self::Select { condition, if_true, if_false } => Self::Select {
                condition: f(*condition),
                if_true: f(*if_true),
                if_false: f(*if_false),
            },
            Self::AssertClosure { value, fun_def, captures } => Self::AssertClosure {
                value: f(*value),
                fun_def: fun_def.clone(),
                captures: captures.iter().map(|id| f(*id)).collect(),
            },
            Self::AssertPointer { value } => Self::AssertPointer { value: f(*value) },
            Self::AssertValueCell { target } => Self::AssertValueCell { target: f(*target) },
            Self::AssertTrue { value } => Self::AssertTrue { value: f(*value) },
            Self::CallBuiltin { callee, name, args } => Self::CallBuiltin {
                callee: f(*callee),
                name: name.clone(),
                args: args.iter().map(|id| f(*id)).collect(),
            },
            Self::Expand { value } => Self::Expand { value: f(*value) },
            Self::Kill { values } => Self::Kill {
                values: values.iter().map(|v| f(*v)).collect(),
            },
        }
    }
}

impl Instruction {
    /// Locals this instruction reads. Never includes the local it defines.
    ///
    /// `Store` reports both `target` and `source`: the target is a pointer
    /// value that is read in order to be written through. Phi reports its
    /// branch values but not its labels.
    pub fn get_used_locals(&self) -> Vec<LocalId> {
        match self {
            Self::Alloc
            | Self::GetGlobal { .. }
            | Self::NumberConstant { .. }
            | Self::BoolConstant { .. }
            | Self::StringConstant { .. }
            | Self::NilConstant => vec![],
            Self::Load { source } => vec![*source],
            Self::Store { target, source } => vec![*target, *source],
            Self::StoreEmptyTable { target } => vec![*target],
            Self::StoreClosure { target, captures, .. } => {
                let mut v = vec![*target];
                v.extend(captures.iter().copied());
                v
            }
            Self::GetField { receiver, .. } => vec![*receiver],
            Self::GetIndex { receiver, index, .. } => vec![*receiver, *index],
            Self::Call { closure, args } => {
                let mut v = vec![*closure];
                v.extend(args.iter().copied());
                v
            }
            Self::UnaryOp { arg, .. } => vec![*arg],
            Self::BinaryOp { left, right, .. } => vec![*left, *right],
            Self::Phi { branches } => branches.iter().map(|(_, id)| *id).collect(),
            Self::Select { condition, if_true, if_false } => {
                vec![*condition, *if_true, *if_false]
            }
            Self::AssertClosure { value, captures, .. } => {
                let mut v = vec![*value];
                v.extend(captures.iter().copied());
                v
            }
            Self::AssertPointer { value } => vec![*value],
            Self::AssertValueCell { target } => vec![*target],
            Self::AssertTrue { value } => vec![*value],
            Self::CallBuiltin { callee, args, .. } => {
                let mut v = vec![*callee];
                v.extend(args.iter().copied());
                v
            }
            Self::Expand { value } => vec![*value],
            // Not a use - see the variant's docs.
            Self::Kill { .. } => vec![],
        }
    }

    /// True if removing this instruction when its result is unused would change
    /// program behaviour.
    ///
    /// Note `GetField`/`GetIndex` with `create_if_missing` are *not* pure: they
    /// allocate a cell and mutate the receiver table, promoting `UnknownTable`
    /// to `ObjectTable`. The previous optimizer's DCE classified them as pure
    /// and would silently change the heap shape.
    pub fn has_side_effects(&self) -> bool {
        match self {
            Self::Store { .. } | Self::StoreEmptyTable { .. } | Self::StoreClosure { .. } => true,
            Self::Call { .. } => true,
            Self::GetGlobal { create_if_missing, .. }
            | Self::GetField { create_if_missing, .. }
            | Self::GetIndex { create_if_missing, .. } => *create_if_missing,
            Self::Alloc
            | Self::Load { .. }
            | Self::NumberConstant { .. }
            | Self::BoolConstant { .. }
            | Self::StringConstant { .. }
            | Self::NilConstant
            | Self::UnaryOp { .. }
            | Self::BinaryOp { .. }
            | Self::Select { .. }
            | Self::Phi { .. } => false,
            // A kill changes the environment, so it must never be reordered
            // away or dropped as pure.
            Self::Kill { .. } => true,
            // Not a side effect on the heap, but they must never be optimised
            // away: their whole purpose is to fail.
            Self::AssertClosure { .. }
            | Self::AssertPointer { .. }
            | Self::AssertValueCell { .. }
            | Self::AssertTrue { .. } => true,
            // Pure in the heap, but it asserts, and the original `call` it
            // replaced would have run and could have failed. Same answer as
            // `Call` for the same reason.
            Self::CallBuiltin { .. } => true,
            // Rewrites the whole state: every lane doubles. Removing it when
            // the result is unused would remove the input fan-out itself.
            Self::Expand { .. } => true,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub enum Terminator {
    Return {
        value: Option<LocalId>,
    },
    UnconditionalBranch {
        target: Label,
    },
    ConditionalBranch {
        condition: LocalId,
        true_target: Label,
        false_target: Label,
    },
}

impl Terminator {
    pub fn get_used_locals(&self) -> Vec<LocalId> {
        match self {
            Self::Return { value } => value.into_iter().copied().collect(),
            Self::UnconditionalBranch { .. } => vec![],
            Self::ConditionalBranch { condition, .. } => vec![*condition],
        }
    }

    /// Rename this terminator's branch targets.
    pub fn map_labels(&self, f: &impl Fn(&Label) -> Label) -> Self {
        match self {
            Self::Return { value } => Self::Return { value: *value },
            Self::UnconditionalBranch { target } => {
                Self::UnconditionalBranch { target: f(target) }
            }
            Self::ConditionalBranch { condition, true_target, false_target } => {
                Self::ConditionalBranch {
                    condition: *condition,
                    true_target: f(true_target),
                    false_target: f(false_target),
                }
            }
        }
    }

    /// Successor labels, in order (true target then false target).
    pub fn successor_labels(&self) -> Vec<&Label> {
        match self {
            Self::Return { .. } => vec![],
            Self::UnconditionalBranch { target } => vec![target],
            Self::ConditionalBranch { true_target, false_target, .. } => {
                vec![true_target, false_target]
            }
        }
    }

    pub fn map_local_ids(&self, mut f: impl FnMut(LocalId) -> LocalId) -> Self {
        match self {
            Self::Return { value } => Self::Return {
                value: value.map(|id| f(id)),
            },
            Self::UnconditionalBranch { target } => Self::UnconditionalBranch {
                target: target.clone(),
            },
            Self::ConditionalBranch {
                condition,
                true_target,
                false_target,
            } => Self::ConditionalBranch {
                condition: f(*condition),
                true_target: true_target.clone(),
                false_target: false_target.clone(),
            },
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Block {
    pub instructions: Vec<(LocalId, Instruction)>,
    pub terminator: (LocalId, Terminator),
    pub hint_normalize: bool,
}

impl Block {
    #[inline]
    pub fn terminator_id(&self) -> LocalId {
        self.terminator.0
    }

    #[inline]
    pub fn terminator_kind(&self) -> &Terminator {
        &self.terminator.1
    }

    /// Splits block instructions into (phi_instructions, non_phi_instructions).
    /// Phi instructions must come first in the block, followed by non-phi instructions.
    pub fn split_block_phi_instructions(
        &self,
    ) -> (&[(LocalId, Instruction)], &[(LocalId, Instruction)]) {
        let is_phi = |id_and_instr| match id_and_instr {
            &(_, Instruction::Phi { .. }) => true,
            _ => false,
        };

        // Find the first non-phi instruction
        let split_index = self
            .instructions
            .iter()
            .position(|instr| !is_phi(instr))
            .unwrap_or(self.instructions.len());

        let (phi_instructions, non_phi_instructions) = self.instructions.split_at(split_index);

        // Verify all phi instructions are before the split
        if phi_instructions.iter().any(|instr| !is_phi(instr)) {
            panic!("Non-phi instructions found before phi instructions in the block");
        }

        // Verify no phi instructions after the split
        if non_phi_instructions.iter().any(is_phi) {
            panic!("Phi instructions found after non-phi instructions in the block");
        }

        (phi_instructions, non_phi_instructions)
    }
}

/// A fresh, empty map of the kind `Cfg::named` uses.
pub fn new_label_map() -> FxHashMap<Label, Block> {
    FxHashMap::default()
}

/// Maps the logical name of a value (`LocalId`, unique per definition, SSA) to
/// the physical place it lives (a slot in a small dense array).
///
/// Keeping these separate is what lets slot allocation happen *without*
/// destroying SSA. `LocalId` stays unique, so dominance checks, the
/// single-definition invariant and rewrite-recipe addressing by `%N` all keep
/// working; only the slot table changes. That in turn means allocation is not a
/// terminal transformation and can be redone whenever the program changes.
///
/// It matters because `LocalEnv` used to be indexed by `LocalId` directly, so
/// it cost `max LocalId + 1` slots, and every `filter_by_mask` clones it. After
/// inlining, `player.update_21` reached 3206 ids - but never more than 18
/// simultaneously live values. See `plans/inline.md`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SlotMap {
    /// Slot for each `LocalId`. Empty means the identity map, i.e. exactly the
    /// old behaviour, which is what an un-allocated CFG gets.
    of_local: Vec<u32>,
    num_slots: usize,
}

impl SlotMap {
    /// Every value gets its own slot, numbered by `LocalId`. Reproduces the
    /// pre-slot behaviour exactly.
    pub fn identity() -> Self {
        Self { of_local: Vec::new(), num_slots: 0 }
    }

    pub fn from_vec(of_local: Vec<u32>) -> Self {
        let num_slots = of_local
            .iter()
            .filter(|s| **s != u32::MAX)
            .map(|s| *s as usize + 1)
            .max()
            .unwrap_or(0);
        Self { of_local, num_slots }
    }

    pub fn is_identity(&self) -> bool {
        self.of_local.is_empty()
    }

    /// The slot of a `LocalId`, or `None` if the map does not cover it. An
    /// uncovered id means the map is stale with respect to the CFG.
    pub fn try_slot_of(&self, id: LocalId) -> Option<usize> {
        if self.of_local.is_empty() {
            return Some(usize::from(id));
        }
        match self.of_local.get(usize::from(id)) {
            Some(&s) if s != u32::MAX => Some(s as usize),
            _ => None,
        }
    }

    #[inline]
    pub fn slot_of(&self, id: LocalId) -> usize {
        if self.of_local.is_empty() {
            usize::from(id)
        } else {
            self.of_local
                .get(usize::from(id))
                .copied()
                .unwrap_or(u32::MAX) as usize
        }
    }

    pub fn num_slots(&self) -> usize {
        self.num_slots
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Cfg {
    pub entry: Block,
    pub named: FxHashMap<Label, Block>,
    /// Where each `LocalId` of this CFG physically lives at run time.
    ///
    /// Part of the CFG rather than the `FunDef` because the interpreter is
    /// driven by CFGs - the `__init` and `__frame` chunks have no `FunDef`
    /// behind them - and because it is exactly the ids occurring in these
    /// blocks that it has to cover.
    ///
    /// Any rewrite that introduces or renumbers ids must reset this to the
    /// identity; `validate` rejects a map that does not cover the CFG, and the
    /// occupant array in `LocalEnv` is the run-time backstop.
    pub slots: Arc<SlotMap>,
    /// Stable names for this CFG's locals; see `Names`. Empty until the
    /// naming pass runs, and invisible to `PartialEq`.
    pub names: Names,
}

impl Cfg {
    /// A CFG with no slot allocation yet, i.e. one local per `LocalId`.
    pub fn new(entry: Block, named: FxHashMap<Label, Block>) -> Self {
        Self { entry, named, slots: Arc::new(SlotMap::identity()), names: Names::default() }
    }

    pub fn iter_blocks(&self) -> impl Iterator<Item = &Block> {
        std::iter::once(&self.entry).chain(self.named.values())
    }

    /// Rewrite every block, dropping the slot allocation.
    ///
    /// Callers change instructions, and a map built for the old instructions
    /// says nothing about the new ones. Re-run `allocate_slots` afterwards.
    pub fn map_blocks(&self, f: impl Fn(&Block) -> Block) -> Self {
        let mut out = Self::new(
            f(&self.entry),
            self.named.iter().map(|(k, v)| (k.clone(), f(v))).collect(),
        );
        // Names are CARRIED, unlike slots which are deliberately dropped.
        //
        // Rewriting a block does not rename anything, so erasing the names
        // here would silently destroy the recipe's addressing the moment
        // the naming pass populates them - a rule would simply return a
        // function whose locals had no names, and the next entry that
        // referred to one would fail with "unknown name" far from the
        // cause. A name left pointing at an instruction the rewrite
        // DELETED is caught by validation instead, which is the loud
        // direction.
        out.names = self.names.clone();
        out
    }

    /// Rename every block label: the map's keys, every branch target and
    /// every phi's incoming-edge label, all at once.
    ///
    /// Renaming only some of those would leave a CFG that still looks
    /// well-formed to a casual reader but whose phis name edges that no
    /// longer exist, so this is one operation rather than three the caller
    /// has to remember. Slots and names are carried unchanged - a label is
    /// not a local.
    pub fn map_labels(&self, f: &impl Fn(&Label) -> Label) -> Self {
        let map_block = |block: &Block| Block {
            instructions: block
                .instructions
                .iter()
                .map(|(id, instruction)| (*id, instruction.map_labels(f)))
                .collect(),
            terminator: (block.terminator.0, block.terminator.1.map_labels(f)),
            hint_normalize: block.hint_normalize,
        };
        Self {
            entry: map_block(&self.entry),
            named: self.named.iter().map(|(k, v)| (f(k), map_block(v))).collect(),
            slots: self.slots.clone(),
            names: self.names.clone(),
        }
    }
}

/// Stable names for locals - the addressing the recipe wants, kept apart
/// from the `LocalId`, which is free to move.
///
/// A recipe entry that says `%foreach_1.t3` must still mean the same
/// instruction after an unrelated edit elsewhere. Ids cannot provide that:
/// they have to stay DENSE, because `SlotMap::identity()` means slot ==
/// LocalId and `LocalEnv` sizes its storage by slot, so a sparse id would
/// make every intermediate state allocate to the id's magnitude. See
/// plans/recipe-stability-plan.md - the per-entry stride scheme died on
/// exactly that.
///
/// # Why `PartialEq` ignores it
///
/// Names are ADDRESSING METADATA, not program content: two functions that
/// differ only in what their locals are called are the same function.
/// Several rule verifiers assert "nothing outside the edited region
/// changed" by comparing `FunDef`s, and they would all start failing the
/// moment naming was introduced - the same trap `apply_entry` already
/// works around for `slots` by resetting the map before every rule. The
/// `isocheck` gate is what actually checks the program did not change, and
/// it is name-blind by construction.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Names {
    of_local: FxHashMap<LocalId, String>,
    by_name: FxHashMap<String, LocalId>,
}

impl PartialEq for Names {
    /// Always equal: see the type docs. Naming a local is not a change to
    /// the program.
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl Names {
    pub fn is_empty(&self) -> bool {
        self.of_local.is_empty()
    }

    pub fn get(&self, id: LocalId) -> Option<&str> {
        self.of_local.get(&id).map(|s| s.as_str())
    }

    pub fn lookup(&self, name: &str) -> Option<LocalId> {
        self.by_name.get(name).copied()
    }

    pub fn iter(&self) -> impl Iterator<Item = (LocalId, &str)> {
        self.of_local.iter().map(|(id, n)| (*id, n.as_str()))
    }

    /// Bind a name to an id, refusing any collision.
    ///
    /// Two locals sharing a name would make a recipe entry silently address
    /// the wrong instruction, which is the failure this whole mechanism
    /// exists to prevent - so it is an error, never a last-writer-wins.
    pub fn insert(&mut self, id: LocalId, name: impl Into<String>) -> Result<(), String> {
        let name = name.into();
        if let Some(existing) = self.by_name.get(&name) {
            if *existing != id {
                return Err(format!(
                    "name {:?} is already bound to %{}, cannot bind it to %{}",
                    name,
                    usize::from(*existing),
                    usize::from(id)
                ));
            }
            return Ok(());
        }
        if let Some(old) = self.of_local.get(&id) {
            return Err(format!(
                "%{} is already named {:?}, cannot rename it to {:?}",
                usize::from(id),
                old,
                name
            ));
        }
        self.by_name.insert(name.clone(), id);
        self.of_local.insert(id, name);
        Ok(())
    }

    /// Drop names whose id no longer exists, given the ids still live.
    pub fn retain_ids(&mut self, live: &dyn Fn(LocalId) -> bool) {
        self.of_local.retain(|id, _| live(*id));
        let kept: FxHashMap<String, LocalId> =
            self.of_local.iter().map(|(id, n)| (n.clone(), *id)).collect();
        self.by_name = kept;
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct FunDef {
    pub name: GlobalId,
    pub capture_ids: Vec<LocalId>,
    pub arg_ids: Vec<Option<LocalId>>,
    pub cfg: Cfg,
    /// Source span of the function definition in the original Lua source
    pub source_span: Option<SourceSpan>,
}

#[cfg(test)]
mod names_tests {
    use super::*;

    #[test]
    fn a_name_binds_both_ways() {
        let mut n = Names::default();
        n.insert(LocalId::from(7), "foreach_1.loop_i").unwrap();
        assert_eq!(n.get(LocalId::from(7)), Some("foreach_1.loop_i"));
        assert_eq!(n.lookup("foreach_1.loop_i"), Some(LocalId::from(7)));
        assert_eq!(n.lookup("nope"), None);
    }

    /// The whole point: two locals must never share a name, or a recipe
    /// entry silently addresses the wrong instruction. Last-writer-wins
    /// would be the dangerous behaviour, so a collision is an error.
    #[test]
    fn a_colliding_name_is_refused() {
        let mut n = Names::default();
        n.insert(LocalId::from(1), "t").unwrap();
        let err = n.insert(LocalId::from(2), "t").expect_err("must refuse");
        assert!(err.contains("already bound"), "{}", err);
        // and the original binding is intact
        assert_eq!(n.lookup("t"), Some(LocalId::from(1)));
    }

    /// Renaming an already-named local is equally refused - a second name
    /// for one id would let two recipe entries disagree about what they are
    /// pointing at while both resolving.
    #[test]
    fn renaming_is_refused_but_rebinding_the_same_pair_is_fine() {
        let mut n = Names::default();
        n.insert(LocalId::from(1), "a").unwrap();
        assert!(n.insert(LocalId::from(1), "b").is_err());
        n.insert(LocalId::from(1), "a").expect("idempotent rebind is fine");
    }

    /// Names are metadata: two functions differing only in them are equal,
    /// or every rule verifier that asserts "nothing else changed" would
    /// start failing the moment naming was introduced.
    #[test]
    fn names_are_invisible_to_equality() {
        let mut a = Names::default();
        let b = Names::default();
        a.insert(LocalId::from(3), "x").unwrap();
        assert_eq!(a, b, "naming a local is not a change to the program");
    }

    #[test]
    fn retain_drops_dead_ids_from_both_directions() {
        let mut n = Names::default();
        n.insert(LocalId::from(1), "keep").unwrap();
        n.insert(LocalId::from(2), "drop").unwrap();
        n.retain_ids(&|id| usize::from(id) == 1);
        assert_eq!(n.lookup("keep"), Some(LocalId::from(1)));
        assert_eq!(n.lookup("drop"), None, "the reverse map must be pruned too");
        assert_eq!(n.get(LocalId::from(2)), None);
    }
}
