use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::pico8_num::Pico8Num;

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

/// Global identifier using Arc<String> for O(1) cloning in hot paths.
#[derive(Clone, Debug)]
pub struct GlobalId(std::sync::Arc<String>);

impl GlobalId {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl PartialEq for GlobalId {
    fn eq(&self, other: &Self) -> bool {
        std::sync::Arc::ptr_eq(&self.0, &other.0) || *self.0 == *other.0
    }
}

impl Eq for GlobalId {}

impl PartialOrd for GlobalId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for GlobalId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl std::hash::Hash for GlobalId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl From<String> for GlobalId {
    fn from(s: String) -> Self {
        Self(std::sync::Arc::new(s))
    }
}

// Custom Serialize that serializes just the string content
impl Serialize for GlobalId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

// Custom Deserialize that wraps in Arc
impl<'de> Deserialize<'de> for GlobalId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(Self(std::sync::Arc::new(s)))
    }
}

pub type GlobalIdGenerator = UniqueStringGenerator<GlobalId>;

/// Block label using Arc<String> for O(1) cloning in hot paths.
#[derive(Clone, Debug)]
pub struct Label(std::sync::Arc<String>);

impl Label {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl PartialEq for Label {
    fn eq(&self, other: &Self) -> bool {
        std::sync::Arc::ptr_eq(&self.0, &other.0) || *self.0 == *other.0
    }
}

impl Eq for Label {}

impl PartialOrd for Label {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Label {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl std::hash::Hash for Label {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl From<String> for Label {
    fn from(s: String) -> Self {
        Self(std::sync::Arc::new(s))
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

/// A barrier identifier - a tuple of integers that provides total ordering.
/// Examples: vec![1], vec![1, 2], vec![2, 0, 1]
/// Barriers are ordered lexicographically.
/// Uses Arc for O(1) cloning in hot paths.
#[derive(Clone, Debug)]
pub struct BarrierId(pub std::sync::Arc<Vec<i32>>);

impl BarrierId {
    pub fn new(ids: Vec<i32>) -> Self {
        Self(std::sync::Arc::new(ids))
    }
}

impl PartialEq for BarrierId {
    fn eq(&self, other: &Self) -> bool {
        std::sync::Arc::ptr_eq(&self.0, &other.0) || *self.0 == *other.0
    }
}

impl Eq for BarrierId {}

impl PartialOrd for BarrierId {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BarrierId {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl std::hash::Hash for BarrierId {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub instructions: Vec<(LocalId, Instruction)>,
    pub terminator: (LocalId, Terminator),
    /// If Some, this block is a barrier point where states synchronize.
    /// Replaces the old hint_normalize mechanism.
    pub barrier: Option<BarrierId>,
}

impl Block {
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

#[derive(Clone, Debug)]
pub struct Cfg {
    pub entry: Block,
    pub named: HashMap<Label, Block>,
}

impl Cfg {
    pub fn iter_blocks(&self) -> impl Iterator<Item = &Block> {
        std::iter::once(&self.entry).chain(self.named.values())
    }

    pub fn map_blocks(&self, f: impl Fn(&Block) -> Block) -> Self {
        Self {
            entry: f(&self.entry),
            named: self.named.iter().map(|(k, v)| (k.clone(), f(v))).collect(),
        }
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
