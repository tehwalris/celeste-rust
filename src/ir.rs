use std::collections::HashMap;

use crate::pico8_num::Pico8Num;

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
struct LocalId(i32);

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
struct GlobalId(String);

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
struct Label(String);

#[derive(Clone, Debug)]
enum Instruction {
    Alloc,
    GetGlobal(String, bool),
    Load(LocalId),
    Store(LocalId, LocalId),
    StoreEmptyTable(LocalId),
    StoreClosure(LocalId, GlobalId, Vec<LocalId>),
    GetField(LocalId, String, bool),
    GetIndex(LocalId, LocalId, bool),
    NumberConstant(Pico8Num),
    BoolConstant(bool),
    StringConstant(String),
    NilConstant,
    Call(LocalId, Vec<LocalId>),
    UnaryOp(String, LocalId),
    BinaryOp(LocalId, String, LocalId),
    Phi(Vec<(Label, LocalId)>),
}

impl Instruction {
    fn map_local_ids(&self, f: impl Fn(LocalId) -> LocalId) -> Self {
        match self {
            Self::Alloc => Self::Alloc,
            Self::GetGlobal(name, flag) => Self::GetGlobal(name.clone(), *flag),
            Self::Load(var_id) => Self::Load(f(*var_id)),
            Self::Store(var_id, val_id) => Self::Store(f(*var_id), f(*val_id)),
            Self::StoreEmptyTable(var_id) => Self::StoreEmptyTable(f(*var_id)),
            Self::StoreClosure(var_id, closure_id, capture_ids) => Self::StoreClosure(
                f(*var_id),
                closure_id.clone(),
                capture_ids.iter().map(|id| f(*id)).collect(),
            ),
            Self::GetField(var_id, field_name, create_if_missing) => {
                Self::GetField(f(*var_id), field_name.clone(), *create_if_missing)
            }
            Self::GetIndex(var_id, index_id, create_if_missing) => {
                Self::GetIndex(f(*var_id), f(*index_id), *create_if_missing)
            }
            Self::NumberConstant(value) => Self::NumberConstant(*value),
            Self::BoolConstant(value) => Self::BoolConstant(*value),
            Self::StringConstant(value) => Self::StringConstant(value.clone()),
            Self::NilConstant => Self::NilConstant,
            Self::Call(closure_id, arg_ids) => {
                Self::Call(f(*closure_id), arg_ids.iter().map(|id| f(*id)).collect())
            }
            Self::UnaryOp(op, arg_id) => Self::UnaryOp(op.clone(), f(*arg_id)),
            Self::BinaryOp(left_id, op, right_id) => {
                Self::BinaryOp(f(*left_id), op.clone(), f(*right_id))
            }
            Self::Phi(branches) => Self::Phi(
                branches
                    .iter()
                    .map(|(label, id)| (label.clone(), f(*id)))
                    .collect(),
            ),
        }
    }
}

#[derive(Clone, Debug)]
enum Terminator {
    Ret(Option<LocalId>),
    Br(Label),
    Cbr(LocalId, Label, Label),
}

impl Terminator {
    fn map_local_ids(&self, f: impl Fn(LocalId) -> LocalId) -> Self {
        match self {
            Self::Ret(Some(id)) => Self::Ret(Some(f(*id))),
            Self::Ret(None) => Self::Ret(None),
            Self::Br(label) => Self::Br(label.clone()),
            Self::Cbr(val_id, l_true, l_false) => {
                Self::Cbr(f(*val_id), l_true.clone(), l_false.clone())
            }
        }
    }
}

#[derive(Clone, Debug)]
struct Block {
    instructions: Vec<(LocalId, Instruction)>,
    terminator: (LocalId, Terminator),
    hint_normalize: bool,
}

impl Block {
    fn split_block_phi_instructions(
        &self,
    ) -> (&[(LocalId, Instruction)], &[(LocalId, Instruction)]) {
        let split_index = self
            .instructions
            .iter()
            .position(|&(_, ref instr)| match instr {
                Instruction::Phi(_) => false,
                _ => true,
            })
            .unwrap_or(self.instructions.len());

        let (phi_instructions, non_phi_instructions) = self.instructions.split_at(split_index);

        if non_phi_instructions
            .iter()
            .any(|&(_, ref instr)| match instr {
                Instruction::Phi(_) => true,
                _ => false,
            })
        {
            panic!("Phi instructions are not at the beginning of the block");
        }

        (phi_instructions, non_phi_instructions)
    }
}

#[derive(Clone, Debug)]
struct Cfg {
    entry: Block,
    named: HashMap<Label, Block>,
}

impl Cfg {
    fn iter_blocks(&self) -> impl Iterator<Item = &Block> {
        std::iter::once(&self.entry).chain(self.named.values())
    }

    fn map_blocks(&self, f: impl Fn(&Block) -> Block) -> Self {
        Self {
            entry: f(&self.entry),
            named: self.named.iter().map(|(k, v)| (k.clone(), f(v))).collect(),
        }
    }
}

#[derive(Clone, Debug)]
struct FunDef {
    name: GlobalId,
    capture_ids: Vec<LocalId>,
    arg_ids: Vec<Option<LocalId>>,
    cfg: Cfg,
}
