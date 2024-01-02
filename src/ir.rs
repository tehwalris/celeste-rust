use std::collections::HashMap;

use crate::pico8_num::Pico8Num;

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct LocalId(i32);

pub struct LocalIdGenerator {
    next_id: i32,
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
    next_id: i32,
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

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct GlobalId(String);

impl From<String> for GlobalId {
    fn from(s: String) -> Self {
        Self(s)
    }
}

pub type GlobalIdGenerator = UniqueStringGenerator<GlobalId>;

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct Label(String);

impl From<String> for Label {
    fn from(s: String) -> Self {
        Self(s)
    }
}

pub type LabelGenerator = UniqueStringGenerator<Label>;

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
        op: String,
        arg: LocalId,
    },
    BinaryOp {
        left: LocalId,
        op: String,
        right: LocalId,
    },
    Phi {
        branches: Vec<(Label, LocalId)>,
    },
}

impl Instruction {
    pub fn map_local_ids(&self, f: impl Fn(LocalId) -> LocalId) -> Self {
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
    pub fn map_local_ids(&self, f: impl Fn(LocalId) -> LocalId) -> Self {
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
    pub fn split_block_phi_instructions(
        &self,
    ) -> (&[(LocalId, Instruction)], &[(LocalId, Instruction)]) {
        let is_phi = |id_and_instr| match id_and_instr {
            &(_, Instruction::Phi { .. }) => true,
            _ => false,
        };

        let split_index = self
            .instructions
            .iter()
            .position(is_phi)
            .unwrap_or(self.instructions.len());

        let (phi_instructions, non_phi_instructions) = self.instructions.split_at(split_index);

        if non_phi_instructions.iter().any(is_phi) {
            panic!("Phi instructions are not at the beginning of the block");
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
}
