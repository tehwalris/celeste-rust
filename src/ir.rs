use std::hash::BuildHasherDefault;
use std::sync::Arc;

use rustc_hash::FxHasher;
use serde::{Deserialize, Serialize};

use crate::pico8_num::Pico8Num;

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

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    Minus,
    Not,
    Hash,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Clone, Debug)]
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
}

impl Instruction {
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
            // Not a side effect on the heap, but it must never be optimised
            // away: its whole purpose is to fail.
            Self::AssertClosure { .. } => true,
        }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Clone, Debug)]
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
}

impl Cfg {
    /// A CFG with no slot allocation yet, i.e. one local per `LocalId`.
    pub fn new(entry: Block, named: FxHashMap<Label, Block>) -> Self {
        Self { entry, named, slots: Arc::new(SlotMap::identity()) }
    }

    pub fn iter_blocks(&self) -> impl Iterator<Item = &Block> {
        std::iter::once(&self.entry).chain(self.named.values())
    }

    /// Rewrite every block, dropping the slot allocation.
    ///
    /// Callers change instructions, and a map built for the old instructions
    /// says nothing about the new ones. Re-run `allocate_slots` afterwards.
    pub fn map_blocks(&self, f: impl Fn(&Block) -> Block) -> Self {
        Self::new(
            f(&self.entry),
            self.named.iter().map(|(k, v)| (k.clone(), f(v))).collect(),
        )
    }
}

#[derive(Clone, Debug)]
pub struct FunDef {
    pub name: GlobalId,
    pub capture_ids: Vec<LocalId>,
    pub arg_ids: Vec<Option<LocalId>>,
    pub cfg: Cfg,
    /// Source span of the function definition in the original Lua source
    pub source_span: Option<SourceSpan>,
}
