use serde::{Deserialize, Serialize};

use crate::common::FxHashMap;
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

#[derive(Clone)]
pub struct LocalIdGenerator {
    next_id: usize,
}

impl Default for LocalIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl LocalIdGenerator {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    /// Create a generator that starts from a value higher than any existing local ID in the CFG.
    /// This prevents ID conflicts when generating new locals for transformations like inlining.
    pub fn from_cfg(cfg: &Cfg) -> Self {
        let mut max_id = 0;
        for block in cfg.iter_blocks() {
            for (local_id, _) in &block.instructions {
                max_id = max_id.max(local_id.0 + 1);
            }
            // Also check terminator
            max_id = max_id.max(usize::from(block.terminator_id()) + 1);
        }
        Self { next_id: max_id }
    }

    /// Generate a fresh unique LocalId
    pub fn fresh_id(&mut self) -> LocalId {
        let id = LocalId(self.next_id);
        self.next_id += 1;
        id
    }
}

#[derive(Clone)]
pub struct UniqueStringGenerator<T: From<String>> {
    _item_type: std::marker::PhantomData<T>,
    next_id: usize,
}

impl<T: From<String>> Default for UniqueStringGenerator<T> {
    fn default() -> Self {
        Self::new()
    }
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

/// The canonical name for the entry block when referenced in phi nodes.
/// This is a convention used throughout the codebase - the entry block
/// doesn't have a Label in cfg.named, but phi nodes need to reference it.
pub const ENTRY_BLOCK_LABEL: &str = "__entry";

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct Label(String);

impl Label {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Get the label used to reference the entry block in phi nodes.
    pub fn entry() -> Self {
        Self(ENTRY_BLOCK_LABEL.to_string())
    }
}

impl From<String> for Label {
    fn from(s: String) -> Self {
        Self(s)
    }
}

pub type LabelGenerator = UniqueStringGenerator<Label>;

impl LabelGenerator {
    /// Create a generator that starts from a value higher than any existing label ID in the CFG.
    /// This prevents ID conflicts when generating new labels.
    pub fn from_cfg(cfg: &Cfg) -> Self {
        let mut max_id = 0;
        for label in cfg.named.keys() {
            // Try to parse the label suffix number (format: "prefix_N")
            if let Some(suffix) = label.as_str().rsplit('_').next() {
                if let Ok(n) = suffix.parse::<usize>() {
                    max_id = max_id.max(n + 1);
                }
            }
        }
        Self {
            _item_type: std::marker::PhantomData,
            next_id: max_id,
        }
    }
}

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
    /// A resolved call to a known function.
    /// This is produced by call resolution when we know statically which function
    /// will be called. The captures are explicit SSA values (not loaded from heap).
    CallResolved {
        fun_name: GlobalId,
        captures: Vec<LocalId>,
        args: Vec<LocalId>,
    },
    /// A call to a known builtin function.
    /// This is produced by builtin resolution when we know statically that the
    /// global contains a builtin function. This eliminates the GetGlobal + Load overhead.
    CallBuiltin {
        name: String,
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
    /// Get all local IDs used by this instruction.
    ///
    /// This returns all LocalIds that this instruction reads from (its operands).
    /// The returned vector does NOT include the LocalId that the instruction
    /// is assigned to (the "def" in def-use terminology).
    pub fn get_used_locals(&self) -> Vec<LocalId> {
        let mut result = Vec::new();
        match self {
            Self::Alloc => {}
            Self::GetGlobal { .. } => {}
            Self::Load { source } => {
                result.push(*source);
            }
            Self::Store { target, source } => {
                result.push(*target);
                result.push(*source);
            }
            Self::StoreEmptyTable { target } => {
                result.push(*target);
            }
            Self::StoreClosure {
                target, captures, ..
            } => {
                result.push(*target);
                result.extend(captures.iter().copied());
            }
            Self::GetField { receiver, .. } => {
                result.push(*receiver);
            }
            Self::GetIndex { receiver, index, .. } => {
                result.push(*receiver);
                result.push(*index);
            }
            Self::NumberConstant { .. } => {}
            Self::BoolConstant { .. } => {}
            Self::StringConstant { .. } => {}
            Self::NilConstant => {}
            Self::BinaryOp { left, right, .. } => {
                result.push(*left);
                result.push(*right);
            }
            Self::UnaryOp { arg, .. } => {
                result.push(*arg);
            }
            Self::Call { closure, args } => {
                result.push(*closure);
                result.extend(args.iter().copied());
            }
            Self::CallResolved { captures, args, .. } => {
                result.extend(captures.iter().copied());
                result.extend(args.iter().copied());
            }
            Self::CallBuiltin { args, .. } => {
                result.extend(args.iter().copied());
            }
            Self::Phi { branches } => {
                for (_, local) in branches {
                    result.push(*local);
                }
            }
        }
        result
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
                create_if_missing,
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
            Self::CallResolved {
                fun_name,
                captures,
                args,
            } => Self::CallResolved {
                fun_name: fun_name.clone(),
                captures: captures.iter().map(|id| f(*id)).collect(),
                args: args.iter().map(|id| f(*id)).collect(),
            },
            Self::CallBuiltin { name, args } => Self::CallBuiltin {
                name: name.clone(),
                args: args.iter().map(|id| f(*id)).collect(),
            },
            Self::UnaryOp { op, arg } => Self::UnaryOp {
                op: *op,
                arg: f(*arg),
            },
            Self::BinaryOp { left, op, right } => Self::BinaryOp {
                left: f(*left),
                op: *op,
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

    /// Get a short type name for this instruction (lowercase snake_case).
    ///
    /// This is useful for profiling and statistics where you want to group
    /// instructions by type without specific parameter details.
    pub fn type_name(&self) -> &'static str {
        match self {
            Self::Alloc => "alloc",
            Self::GetGlobal { .. } => "get_global",
            Self::Load { .. } => "load",
            Self::Store { .. } => "store",
            Self::StoreEmptyTable { .. } => "store_empty_table",
            Self::StoreClosure { .. } => "store_closure",
            Self::GetField { .. } => "get_field",
            Self::GetIndex { .. } => "get_index",
            Self::NumberConstant { .. } => "number_constant",
            Self::BoolConstant { .. } => "bool_constant",
            Self::StringConstant { .. } => "string_constant",
            Self::NilConstant => "nil_constant",
            Self::Call { .. } => "call",
            Self::CallResolved { .. } => "call_resolved",
            Self::CallBuiltin { .. } => "call_builtin",
            Self::UnaryOp { .. } => "unary_op",
            Self::BinaryOp { .. } => "binary_op",
            Self::Phi { .. } => "phi",
        }
    }

    /// Get a human-readable description of this instruction for error messages and debugging.
    ///
    /// This includes relevant parameter details (e.g., function names, field names, operators)
    /// to help identify specific instructions in logs and error messages.
    pub fn describe(&self) -> String {
        match self {
            Self::Alloc => "Alloc".to_string(),
            Self::GetGlobal { name, .. } => format!("GetGlobal({})", name),
            Self::Load { .. } => "Load".to_string(),
            Self::Store { .. } => "Store".to_string(),
            Self::StoreEmptyTable { .. } => "StoreEmptyTable".to_string(),
            Self::StoreClosure { fun_def, .. } => format!("StoreClosure({})", fun_def.as_str()),
            Self::GetField { field, .. } => format!("GetField(.{})", field),
            Self::GetIndex { .. } => "GetIndex".to_string(),
            Self::NumberConstant { value } => format!("NumberConstant({:?})", value),
            Self::BoolConstant { value } => format!("BoolConstant({})", value),
            Self::StringConstant { value } => format!("StringConstant({:?})", value),
            Self::NilConstant => "NilConstant".to_string(),
            Self::Call { .. } => "Call".to_string(),
            Self::CallResolved { fun_name, .. } => format!("CallResolved({})", fun_name.as_str()),
            Self::CallBuiltin { name, .. } => format!("CallBuiltin({})", name),
            Self::UnaryOp { op, .. } => format!("UnaryOp({:?})", op),
            Self::BinaryOp { op, .. } => format!("BinaryOp({:?})", op),
            Self::Phi { branches } => format!("Phi({} branches)", branches.len()),
        }
    }

    /// Format this instruction for display in the CFG viewer.
    ///
    /// This provides a detailed string representation including all local IDs and parameters,
    /// suitable for visualization and debugging purposes. Unlike `describe()`, this includes
    /// full details of all operands.
    pub fn format(&self) -> String {
        match self {
            Self::Alloc => "Alloc".to_string(),
            Self::GetGlobal { name, create_if_missing } => {
                if *create_if_missing {
                    format!("GetGlobal({}, create)", name)
                } else {
                    format!("GetGlobal({})", name)
                }
            }
            Self::Load { source } => format!("Load({})", usize::from(*source)),
            Self::Store { target, source } => {
                format!("Store({}, {})", usize::from(*target), usize::from(*source))
            }
            Self::StoreEmptyTable { target } => {
                format!("StoreEmptyTable({})", usize::from(*target))
            }
            Self::StoreClosure { target, fun_def, captures } => {
                let caps: Vec<_> = captures.iter().map(|id| usize::from(*id).to_string()).collect();
                format!(
                    "StoreClosure({}, {}, [{}])",
                    usize::from(*target),
                    fun_def.as_str(),
                    caps.join(", ")
                )
            }
            Self::GetField { receiver, field, create_if_missing } => {
                if *create_if_missing {
                    format!("GetField({}, {}, create)", usize::from(*receiver), field)
                } else {
                    format!("GetField({}, {})", usize::from(*receiver), field)
                }
            }
            Self::GetIndex { receiver, index, create_if_missing } => {
                if *create_if_missing {
                    format!(
                        "GetIndex({}, {}, create)",
                        usize::from(*receiver),
                        usize::from(*index)
                    )
                } else {
                    format!("GetIndex({}, {})", usize::from(*receiver), usize::from(*index))
                }
            }
            Self::NumberConstant { value } => format!("NumberConstant({:?})", value),
            Self::BoolConstant { value } => format!("BoolConstant({})", value),
            Self::StringConstant { value } => format!("StringConstant({:?})", value),
            Self::NilConstant => "NilConstant".to_string(),
            Self::Call { closure, args } => {
                let arg_strs: Vec<_> = args.iter().map(|id| usize::from(*id).to_string()).collect();
                format!("Call({}, [{}])", usize::from(*closure), arg_strs.join(", "))
            }
            Self::CallResolved {
                fun_name,
                captures,
                args,
            } => {
                let cap_strs: Vec<_> = captures.iter().map(|id| usize::from(*id).to_string()).collect();
                let arg_strs: Vec<_> = args.iter().map(|id| usize::from(*id).to_string()).collect();
                format!(
                    "CallResolved({}, caps=[{}], args=[{}])",
                    fun_name.as_str(),
                    cap_strs.join(", "),
                    arg_strs.join(", ")
                )
            }
            Self::CallBuiltin { name, args } => {
                let arg_strs: Vec<_> = args.iter().map(|id| usize::from(*id).to_string()).collect();
                format!("CallBuiltin({}, [{}])", name, arg_strs.join(", "))
            }
            Self::UnaryOp { op, arg } => format!("UnaryOp({:?}, {})", op, usize::from(*arg)),
            Self::BinaryOp { left, op, right } => {
                format!(
                    "BinaryOp({}, {:?}, {})",
                    usize::from(*left),
                    op,
                    usize::from(*right)
                )
            }
            Self::Phi { branches } => {
                let branch_strs: Vec<_> = branches
                    .iter()
                    .map(|(label, id)| format!("{}:{}", label.as_str(), usize::from(*id)))
                    .collect();
                format!("Phi([{}])", branch_strs.join(", "))
            }
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
    /// Deoptimization point - abort optimized execution and fall back to interpreter.
    /// This is inserted when we encounter operations that can't be optimized
    /// (e.g., heap-modifying calls, impure builtins).
    /// At runtime, hitting a Deopt means we must restart execution with the
    /// original unoptimized CFG.
    Deopt {
        /// Human-readable reason for the deopt (for debugging/analysis)
        reason: String,
    },
}

impl Terminator {
    /// Get all local IDs used by this terminator.
    ///
    /// This returns all LocalIds that this terminator reads from.
    /// The returned vector does NOT include the LocalId that the terminator
    /// is assigned to (the "def" in def-use terminology).
    pub fn get_used_locals(&self) -> Vec<LocalId> {
        match self {
            Self::Return { value: Some(v) } => vec![*v],
            Self::Return { value: None } => vec![],
            Self::UnconditionalBranch { .. } => vec![],
            Self::ConditionalBranch { condition, .. } => vec![*condition],
            Self::Deopt { .. } => vec![],
        }
    }

    pub fn map_local_ids(&self, mut f: impl FnMut(LocalId) -> LocalId) -> Self {
        match self {
            Self::Return { value } => Self::Return {
                value: value.map(&mut f),
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
            Self::Deopt { reason } => Self::Deopt {
                reason: reason.clone(),
            },
        }
    }

    /// Format this terminator for display in the CFG viewer.
    ///
    /// This provides a detailed string representation including all local IDs and labels,
    /// suitable for visualization and debugging purposes.
    pub fn format(&self) -> String {
        match self {
            Self::Return { value } => {
                if let Some(id) = value {
                    format!("Return({})", usize::from(*id))
                } else {
                    "Return".to_string()
                }
            }
            Self::UnconditionalBranch { target } => {
                format!("Branch({})", target.as_str())
            }
            Self::ConditionalBranch {
                condition,
                true_target,
                false_target,
            } => {
                format!(
                    "CondBranch({}, true:{}, false:{})",
                    usize::from(*condition),
                    true_target.as_str(),
                    false_target.as_str()
                )
            }
            Self::Deopt { reason } => {
                format!("Deopt(\"{}\")", reason)
            }
        }
    }

    /// Get the successor labels of this terminator.
    ///
    /// Returns references to all labels this terminator can branch to.
    /// For Return and Deopt, this returns an empty iterator.
    /// For UnconditionalBranch, returns a single target.
    /// For ConditionalBranch, returns both true and false targets.
    pub fn get_successor_labels(&self) -> impl Iterator<Item = &Label> {
        let (first, second) = match self {
            Self::Return { .. } | Self::Deopt { .. } => (None, None),
            Self::UnconditionalBranch { target } => (Some(target), None),
            Self::ConditionalBranch {
                true_target,
                false_target,
                ..
            } => (Some(true_target), Some(false_target)),
        };
        first.into_iter().chain(second)
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub instructions: Vec<(LocalId, Instruction)>,
    pub terminator: (LocalId, Terminator),
    pub hint_normalize: bool,
}

/// Type alias for a slice of instructions (pairs of LocalId and Instruction).
pub type InstructionSlice<'a> = &'a [(LocalId, Instruction)];

/// Type alias for a split of phi and non-phi instructions in a block.
pub type PhiSplit<'a> = (InstructionSlice<'a>, InstructionSlice<'a>);

impl Block {
    /// Returns the LocalId assigned to the terminator instruction.
    #[inline]
    pub fn terminator_id(&self) -> LocalId {
        self.terminator.0
    }

    /// Returns a reference to the Terminator variant.
    #[inline]
    pub fn terminator_kind(&self) -> &Terminator {
        &self.terminator.1
    }

    /// Splits block instructions into (phi_instructions, non_phi_instructions).
    /// Phi instructions must come first in the block, followed by non-phi instructions.
    pub fn split_block_phi_instructions(&self) -> PhiSplit<'_> {
        let is_phi = |id_and_instr: &(LocalId, Instruction)| {
            matches!(id_and_instr, (_, Instruction::Phi { .. }))
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

    /// Format this block as a multi-line string for debugging.
    ///
    /// The output includes each instruction with its LocalId prefix and the terminator.
    /// Lines are indented with two spaces. This is suitable for test output and debugging.
    pub fn format(&self, name: &str) -> String {
        let mut lines = vec![format!("Block '{}':", name)];
        for (id, instr) in &self.instructions {
            lines.push(format!("  %{} = {}", usize::from(*id), instr.format()));
        }
        lines.push(format!(
            "  terminator: %{} = {}",
            usize::from(self.terminator_id()),
            self.terminator_kind().format()
        ));
        lines.join("\n")
    }
}

/// Identifies a block in a CFG, distinguishing between the entry block and named blocks.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BlockId {
    Entry,
    Named(Label),
}

impl BlockId {
    /// Get the label for this block (entry block uses the special entry label for phi nodes).
    pub fn label(&self) -> Label {
        match self {
            BlockId::Entry => Label::entry(),
            BlockId::Named(label) => label.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Cfg {
    pub entry: Block,
    pub named: FxHashMap<Label, Block>,
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

    /// Iterate over all blocks with their BlockId.
    pub fn iter_blocks_with_id(&self) -> impl Iterator<Item = (BlockId, &Block)> {
        std::iter::once((BlockId::Entry, &self.entry)).chain(
            self.named
                .iter()
                .map(|(label, block)| (BlockId::Named(label.clone()), block)),
        )
    }

    /// Iterate over all blocks with a string label.
    ///
    /// This is a convenience method for cases where you need block iteration
    /// with string labels (e.g., for error messages, validation). The entry
    /// block uses the label "entry" (not ENTRY_BLOCK_LABEL which is "__entry"
    /// used for phi nodes).
    pub fn iter_blocks_with_label(&self) -> impl Iterator<Item = (&str, &Block)> {
        std::iter::once(("entry", &self.entry)).chain(
            self.named
                .iter()
                .map(|(label, block)| (label.as_str(), block)),
        )
    }

    /// Compute predecessor map for all blocks.
    ///
    /// Returns a map from each block to the list of blocks that can branch to it.
    /// The entry block has no predecessors (empty Vec).
    pub fn compute_predecessors(&self) -> FxHashMap<BlockId, Vec<BlockId>> {
        let mut preds: FxHashMap<BlockId, Vec<BlockId>> = FxHashMap::default();

        // Initialize all blocks with empty predecessor lists
        preds.insert(BlockId::Entry, Vec::new());
        for label in self.named.keys() {
            preds.insert(BlockId::Named(label.clone()), Vec::new());
        }

        // Add predecessors from each block's terminator
        for (block_id, block) in self.iter_blocks_with_id() {
            for succ_label in block.terminator_kind().get_successor_labels() {
                if let Some(p) = preds.get_mut(&BlockId::Named(succ_label.clone())) {
                    p.push(block_id.clone());
                }
            }
        }

        preds
    }

    /// Format this CFG as a multi-line string for debugging.
    ///
    /// The output includes all blocks with their instructions and terminators.
    /// Blocks are separated by blank lines. This is suitable for test output
    /// and debugging.
    pub fn format(&self) -> String {
        let mut parts = vec![self.entry.format("entry")];
        for (label, block) in &self.named {
            parts.push(block.format(label.as_str()));
        }
        parts.join("\n\n")
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
