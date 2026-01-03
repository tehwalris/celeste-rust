//! CFG analysis for shape-specific optimizations.
//!
//! This module provides analysis passes that examine a CFG and determine
//! properties relevant to shape-specific optimization, such as:
//! - Whether the CFG reads from the heap
//! - Whether it writes to the heap
//! - Whether it can modify the heap shape (allocations, table operations)
//! - Whether it contains function calls

use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use crate::ir::{Block, Cfg, Instruction, LocalId, Terminator};

/// Results of analyzing a CFG for heap-related operations.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct CfgAnalysisResult {
    /// The CFG reads from the heap (Load, GetField, GetIndex)
    pub reads_heap: bool,
    /// The CFG writes to the heap (Store, StoreEmptyTable, StoreClosure)
    pub writes_heap: bool,
    /// The CFG can change externally-visible heap shape (escaping Alloc, StoreEmptyTable)
    pub modifies_heap_shape: bool,
    /// The CFG contains function calls (Call instruction)
    pub has_calls: bool,
    /// The CFG reads global variables (GetGlobal)
    pub reads_globals: bool,
    /// The CFG writes global variables (via heap writes to global pointers)
    pub writes_globals: bool,
    /// Whether any allocations escape the function (could be visible after return)
    pub has_escaping_allocs: bool,
    /// Count of local-only (non-escaping) allocations
    pub local_only_allocs: usize,
    /// Count of each instruction type for statistics
    pub instruction_counts: InstructionCounts,
    /// List of functions called (GlobalId)
    pub called_functions: Vec<String>,
    /// List of global variables accessed
    pub accessed_globals: Vec<String>,
    /// Whether heap elimination transformation could be applied
    pub heap_elimination_status: HeapEliminationStatus,
}

/// Status of attempting heap elimination on a CFG
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub enum HeapEliminationStatus {
    #[default]
    NotAttempted,
    /// Transformation succeeded
    Success {
        /// Number of heap slots unpacked
        unpack_count: usize,
        /// Number of slots that were modified
        modified_count: usize,
    },
    /// Transformation failed with reason
    Failed(String),
    /// Not applicable (e.g., no heap operations)
    NotApplicable,
}

/// Results of running the optimization pipeline
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct OptimizationResult {
    /// Result of mem2reg pass (local cell elimination)
    pub mem2reg: Mem2RegStatus,
    /// Result of heap elimination pass (for globals)
    pub heap_elimination: HeapEliminationStatus,
}

/// Status of the mem2reg pass
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub enum Mem2RegStatus {
    #[default]
    NotAttempted,
    /// Successfully promoted cells
    Success {
        cells_promoted: usize,
    },
    /// No cells to promote
    NoCells,
    /// Partial success
    Partial {
        cells_promoted: usize,
        cells_failed: usize,
    },
}

/// Counts of each instruction type in a CFG.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct InstructionCounts {
    pub alloc: usize,
    pub get_global: usize,
    pub load: usize,
    pub store: usize,
    pub store_empty_table: usize,
    pub store_closure: usize,
    pub get_field: usize,
    pub get_index: usize,
    pub number_constant: usize,
    pub bool_constant: usize,
    pub string_constant: usize,
    pub nil_constant: usize,
    pub call: usize,
    pub unary_op: usize,
    pub binary_op: usize,
    pub phi: usize,
    pub total_instructions: usize,
    pub total_blocks: usize,
}

impl CfgAnalysisResult {
    /// Returns true if the CFG has no heap operations at all
    pub fn is_heap_free(&self) -> bool {
        !self.reads_heap && !self.writes_heap && !self.modifies_heap_shape
    }

    /// Returns true if the CFG is "pure" (no side effects)
    pub fn is_pure(&self) -> bool {
        !self.writes_heap && !self.modifies_heap_shape && !self.has_calls
    }
}

/// Analyze a CFG to determine its heap-related properties.
pub fn analyze_cfg(cfg: &Cfg) -> CfgAnalysisResult {
    let mut result = CfgAnalysisResult::default();
    let mut accessed_globals = HashSet::new();
    let mut called_closures = HashSet::new();

    // Analyze entry block
    analyze_block(&cfg.entry, &mut result, &mut accessed_globals, &mut called_closures);
    result.instruction_counts.total_blocks += 1;

    // Analyze named blocks
    for (_, block) in &cfg.named {
        analyze_block(block, &mut result, &mut accessed_globals, &mut called_closures);
        result.instruction_counts.total_blocks += 1;
    }

    result.accessed_globals = accessed_globals.into_iter().collect();
    result.accessed_globals.sort();

    // Perform escape analysis to refine modifies_heap_shape
    // An allocation that doesn't escape doesn't really modify the heap shape
    let escape_info = analyze_escapes(cfg);
    result.has_escaping_allocs = escape_info.has_escaping_allocs;
    result.local_only_allocs = escape_info.local_only_alloc_count;

    // Refine modifies_heap_shape: only true if there are escaping allocs or StoreEmptyTable
    if result.instruction_counts.alloc > 0 && !escape_info.has_escaping_allocs {
        // All allocations are local-only, so they don't modify visible heap shape
        result.modifies_heap_shape = result.instruction_counts.store_empty_table > 0
            || result.instruction_counts.store_closure > 0;
    }

    // Note: called_closures contains LocalIds, not function names
    // In a real implementation, we'd need to track which closures map to which functions
    // For now, we just record that calls exist

    result
}

/// Information about which allocations escape the function
#[derive(Debug, Default)]
struct EscapeInfo {
    /// Whether any allocation escapes the function
    has_escaping_allocs: bool,
    /// Count of local-only (non-escaping) allocations
    local_only_alloc_count: usize,
}

/// Analyze which allocations escape the function.
/// An allocation escapes if it could be visible after the function returns.
fn analyze_escapes(cfg: &Cfg) -> EscapeInfo {
    let mut allocations: HashSet<LocalId> = HashSet::new();
    let mut escaping: HashSet<LocalId> = HashSet::new();

    // First pass: collect all allocations
    for block in cfg.iter_blocks() {
        for (target_id, instruction) in &block.instructions {
            if matches!(instruction, Instruction::Alloc) {
                allocations.insert(*target_id);
            }
        }
    }

    if allocations.is_empty() {
        return EscapeInfo::default();
    }

    // Second pass: check for escaping uses
    for block in cfg.iter_blocks() {
        for (_, instruction) in &block.instructions {
            check_instruction_escapes(instruction, &allocations, &mut escaping);
        }

        // Check terminator for returns
        if let Terminator::Return { value: Some(ret_id) } = &block.terminator.1 {
            if allocations.contains(ret_id) {
                escaping.insert(*ret_id);
            }
        }
    }

    EscapeInfo {
        has_escaping_allocs: !escaping.is_empty(),
        local_only_alloc_count: allocations.len() - escaping.len(),
    }
}

/// Check if an instruction causes any allocation to escape
fn check_instruction_escapes(
    instruction: &Instruction,
    allocations: &HashSet<LocalId>,
    escaping: &mut HashSet<LocalId>,
) {
    match instruction {
        // Store(target, source): if source is an alloc and target is NOT that same alloc,
        // the allocation might escape (being stored elsewhere)
        Instruction::Store { target, source } => {
            if allocations.contains(source) && target != source {
                // The allocation is being stored somewhere else - it escapes
                escaping.insert(*source);
            }
        }

        // Call arguments: any allocation passed to a call escapes
        Instruction::Call { args, .. } => {
            for arg in args {
                if allocations.contains(arg) {
                    escaping.insert(*arg);
                }
            }
        }

        // StoreClosure captures: any captured allocation escapes
        Instruction::StoreClosure { captures, .. } => {
            for cap in captures {
                if allocations.contains(cap) {
                    escaping.insert(*cap);
                }
            }
        }

        // GetField/GetIndex on an allocation doesn't cause escape by itself
        // (we're reading from it, not exposing it)

        // Other instructions don't cause escapes
        _ => {}
    }
}

/// Run the full optimization pipeline on a CFG.
///
/// Pipeline stages:
/// 1. mem2reg: Promote local cells (non-escaping allocations) to SSA
/// 2. heap_elimination: Eliminate global heap operations with known shapes
///
/// Returns the optimization result and intermediate CFGs for visualization.
pub fn run_optimization_pipeline(
    cfg: &Cfg,
    analysis: &CfgAnalysisResult,
) -> (OptimizationResult, Option<Cfg>, Option<Cfg>) {
    use crate::interpreter::heap_elimination::{
        eliminate_heap, HeapEliminationResult, HeapShape, ValueShape,
    };
    use crate::interpreter::mem2reg::{mem2reg, Mem2RegResult};
    use crate::ir::{LabelGenerator, LocalIdGenerator};

    let mut result = OptimizationResult::default();

    // Stage 1: mem2reg - eliminate local cells
    let mut local_gen = LocalIdGenerator::new();
    let (after_mem2reg, mem2reg_status) = if analysis.local_only_allocs > 0 {
        match mem2reg(cfg, &mut local_gen) {
            Mem2RegResult::Success { cfg: new_cfg, cells_promoted } => {
                (Some(new_cfg), Mem2RegStatus::Success { cells_promoted })
            }
            Mem2RegResult::NoCells => {
                (None, Mem2RegStatus::NoCells)
            }
            Mem2RegResult::PartialSuccess { cfg: new_cfg, cells_promoted, cells_failed, .. } => {
                (Some(new_cfg), Mem2RegStatus::Partial { cells_promoted, cells_failed })
            }
        }
    } else {
        (None, Mem2RegStatus::NoCells)
    };
    result.mem2reg = mem2reg_status;

    // Get the CFG to use for heap elimination (after mem2reg or original)
    let cfg_for_heap_elim = after_mem2reg.as_ref().unwrap_or(cfg);

    // Stage 2: heap elimination - for globals
    let after_heap_elim = if !analysis.accessed_globals.is_empty() {
        // Create a simple shape where each global is a leaf value
        let mut shape = HeapShape::new();
        for global in &analysis.accessed_globals {
            shape.globals.insert(global.clone(), ValueShape::Leaf);
        }

        let label_gen = LabelGenerator::new();

        match eliminate_heap(cfg_for_heap_elim, &shape, local_gen, label_gen) {
            HeapEliminationResult::Success(transformed) => {
                result.heap_elimination = HeapEliminationStatus::Success {
                    unpack_count: transformed.unpack_slots.len(),
                    modified_count: transformed.modified_slots.len(),
                };
                Some(transformed.cfg)
            }
            HeapEliminationResult::ShapeNotPreserved(reason) => {
                result.heap_elimination = HeapEliminationStatus::Failed(reason);
                None
            }
            HeapEliminationResult::HasExternalCalls(funcs) => {
                let reason = format!("External calls: {}", funcs.join(", "));
                result.heap_elimination = HeapEliminationStatus::Failed(reason);
                None
            }
            HeapEliminationResult::NotApplicable => {
                result.heap_elimination = HeapEliminationStatus::NotApplicable;
                None
            }
        }
    } else {
        result.heap_elimination = HeapEliminationStatus::NotApplicable;
        None
    };

    (result, after_mem2reg, after_heap_elim)
}

/// Try to apply heap elimination transformation to a CFG (legacy interface).
/// This runs the full pipeline and returns the final result.
pub fn try_heap_elimination(
    cfg: &Cfg,
    analysis: &CfgAnalysisResult,
) -> (HeapEliminationStatus, Option<Cfg>) {
    let (result, after_mem2reg, after_heap_elim) = run_optimization_pipeline(cfg, analysis);

    // Determine final status and CFG
    // Priority: heap_elimination success > mem2reg success > failure
    if let HeapEliminationStatus::Success { .. } = &result.heap_elimination {
        return (result.heap_elimination, after_heap_elim);
    }

    if let Mem2RegStatus::Success { cells_promoted } = &result.mem2reg {
        return (
            HeapEliminationStatus::Success {
                unpack_count: *cells_promoted,
                modified_count: 0,
            },
            after_mem2reg,
        );
    }

    // Return the heap elimination status (could be Failed or NotApplicable)
    (result.heap_elimination, None)
}

fn analyze_block(
    block: &Block,
    result: &mut CfgAnalysisResult,
    accessed_globals: &mut HashSet<String>,
    called_closures: &mut HashSet<usize>,
) {
    for (_, instruction) in &block.instructions {
        analyze_instruction(instruction, result, accessed_globals, called_closures);
        result.instruction_counts.total_instructions += 1;
    }
}

fn analyze_instruction(
    instruction: &Instruction,
    result: &mut CfgAnalysisResult,
    accessed_globals: &mut HashSet<String>,
    called_closures: &mut HashSet<usize>,
) {
    match instruction {
        Instruction::Alloc => {
            result.modifies_heap_shape = true;
            result.writes_heap = true;
            result.instruction_counts.alloc += 1;
        }
        Instruction::GetGlobal { name, .. } => {
            result.reads_globals = true;
            accessed_globals.insert(name.clone());
            result.instruction_counts.get_global += 1;
        }
        Instruction::Load { .. } => {
            result.reads_heap = true;
            result.instruction_counts.load += 1;
        }
        Instruction::Store { .. } => {
            result.writes_heap = true;
            result.instruction_counts.store += 1;
        }
        Instruction::StoreEmptyTable { .. } => {
            result.writes_heap = true;
            result.modifies_heap_shape = true;
            result.instruction_counts.store_empty_table += 1;
        }
        Instruction::StoreClosure { fun_def, .. } => {
            result.writes_heap = true;
            result.called_functions.push(fun_def.as_str().to_string());
            result.instruction_counts.store_closure += 1;
        }
        Instruction::GetField { .. } => {
            result.reads_heap = true;
            result.instruction_counts.get_field += 1;
        }
        Instruction::GetIndex { .. } => {
            result.reads_heap = true;
            result.instruction_counts.get_index += 1;
        }
        Instruction::NumberConstant { .. } => {
            result.instruction_counts.number_constant += 1;
        }
        Instruction::BoolConstant { .. } => {
            result.instruction_counts.bool_constant += 1;
        }
        Instruction::StringConstant { .. } => {
            result.instruction_counts.string_constant += 1;
        }
        Instruction::NilConstant => {
            result.instruction_counts.nil_constant += 1;
        }
        Instruction::Call { closure, .. } => {
            result.has_calls = true;
            called_closures.insert(usize::from(*closure));
            result.instruction_counts.call += 1;
        }
        Instruction::UnaryOp { .. } => {
            result.instruction_counts.unary_op += 1;
        }
        Instruction::BinaryOp { .. } => {
            result.instruction_counts.binary_op += 1;
        }
        Instruction::Phi { .. } => {
            result.instruction_counts.phi += 1;
        }
    }
}

/// Shape representation for function arguments.
/// This describes the "type" of an argument without its specific value.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ArgShape {
    /// A number (scalar or vector)
    Number,
    /// A number interval (scalar or vector)
    NumberInterval,
    /// A boolean (scalar or vector)
    Bool,
    /// Unknown boolean
    UnknownBool,
    /// A string value
    String(String),
    /// Nil with optional hint
    Nil(Option<String>),
    /// A pointer to a heap object
    Pointer(usize),
    /// A nil pointer (function that wasn't found)
    NilPointer(String),
}

/// Combined shape for a function call: heap shape + argument shapes.
/// This fully describes the "input shape" to a function.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct CallShape {
    /// Shapes of the function arguments
    pub args: Vec<ArgShape>,
    /// Number of heap entries at call time (simplified heap shape)
    pub heap_size: usize,
    // TODO: Add full heap shape representation
}

/// Serializable representation of a CFG for the viewer.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerializableCfg {
    pub entry_block: SerializableBlock,
    pub named_blocks: Vec<(String, SerializableBlock)>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerializableBlock {
    pub instructions: Vec<SerializableInstruction>,
    pub terminator: SerializableTerminator,
    pub hint_normalize: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerializableInstruction {
    pub target_id: usize,
    pub instruction_text: String,
    pub instruction_type: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerializableTerminator {
    pub target_id: usize,
    pub terminator_text: String,
}

impl From<&Cfg> for SerializableCfg {
    fn from(cfg: &Cfg) -> Self {
        let entry_block = (&cfg.entry).into();
        let mut named_blocks: Vec<_> = cfg
            .named
            .iter()
            .map(|(label, block)| (label.as_str().to_string(), block.into()))
            .collect();
        named_blocks.sort_by(|a, b| a.0.cmp(&b.0));

        SerializableCfg {
            entry_block,
            named_blocks,
        }
    }
}

impl From<&Block> for SerializableBlock {
    fn from(block: &Block) -> Self {
        let instructions = block
            .instructions
            .iter()
            .map(|(id, instr)| SerializableInstruction {
                target_id: usize::from(*id),
                instruction_text: format_instruction(instr),
                instruction_type: instruction_type_name(instr).to_string(),
            })
            .collect();

        let (term_id, term) = &block.terminator;
        let terminator = SerializableTerminator {
            target_id: usize::from(*term_id),
            terminator_text: format_terminator(term),
        };

        SerializableBlock {
            instructions,
            terminator,
            hint_normalize: block.hint_normalize,
        }
    }
}

fn format_instruction(instr: &Instruction) -> String {
    match instr {
        Instruction::Alloc => "Alloc".to_string(),
        Instruction::GetGlobal { name, create_if_missing } => {
            if *create_if_missing {
                format!("GetGlobal({}, create)", name)
            } else {
                format!("GetGlobal({})", name)
            }
        }
        Instruction::Load { source } => format!("Load({})", usize::from(*source)),
        Instruction::Store { target, source } => {
            format!("Store({}, {})", usize::from(*target), usize::from(*source))
        }
        Instruction::StoreEmptyTable { target } => {
            format!("StoreEmptyTable({})", usize::from(*target))
        }
        Instruction::StoreClosure { target, fun_def, captures } => {
            let caps: Vec<_> = captures.iter().map(|id| usize::from(*id).to_string()).collect();
            format!(
                "StoreClosure({}, {}, [{}])",
                usize::from(*target),
                fun_def.as_str(),
                caps.join(", ")
            )
        }
        Instruction::GetField { receiver, field, create_if_missing } => {
            if *create_if_missing {
                format!("GetField({}, {}, create)", usize::from(*receiver), field)
            } else {
                format!("GetField({}, {})", usize::from(*receiver), field)
            }
        }
        Instruction::GetIndex { receiver, index, create_if_missing } => {
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
        Instruction::NumberConstant { value } => format!("NumberConstant({:?})", value),
        Instruction::BoolConstant { value } => format!("BoolConstant({})", value),
        Instruction::StringConstant { value } => format!("StringConstant({:?})", value),
        Instruction::NilConstant => "NilConstant".to_string(),
        Instruction::Call { closure, args } => {
            let arg_strs: Vec<_> = args.iter().map(|id| usize::from(*id).to_string()).collect();
            format!("Call({}, [{}])", usize::from(*closure), arg_strs.join(", "))
        }
        Instruction::UnaryOp { op, arg } => format!("UnaryOp({:?}, {})", op, usize::from(*arg)),
        Instruction::BinaryOp { left, op, right } => {
            format!(
                "BinaryOp({}, {:?}, {})",
                usize::from(*left),
                op,
                usize::from(*right)
            )
        }
        Instruction::Phi { branches } => {
            let branch_strs: Vec<_> = branches
                .iter()
                .map(|(label, id)| format!("{}:{}", label.as_str(), usize::from(*id)))
                .collect();
            format!("Phi([{}])", branch_strs.join(", "))
        }
    }
}

fn format_terminator(term: &Terminator) -> String {
    match term {
        Terminator::Return { value } => {
            if let Some(id) = value {
                format!("Return({})", usize::from(*id))
            } else {
                "Return".to_string()
            }
        }
        Terminator::UnconditionalBranch { target } => {
            format!("Branch({})", target.as_str())
        }
        Terminator::ConditionalBranch {
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
    }
}

fn instruction_type_name(instr: &Instruction) -> &'static str {
    match instr {
        Instruction::Alloc => "alloc",
        Instruction::GetGlobal { .. } => "get_global",
        Instruction::Load { .. } => "load",
        Instruction::Store { .. } => "store",
        Instruction::StoreEmptyTable { .. } => "store_empty_table",
        Instruction::StoreClosure { .. } => "store_closure",
        Instruction::GetField { .. } => "get_field",
        Instruction::GetIndex { .. } => "get_index",
        Instruction::NumberConstant { .. } => "number_constant",
        Instruction::BoolConstant { .. } => "bool_constant",
        Instruction::StringConstant { .. } => "string_constant",
        Instruction::NilConstant => "nil_constant",
        Instruction::Call { .. } => "call",
        Instruction::UnaryOp { .. } => "unary_op",
        Instruction::BinaryOp { .. } => "binary_op",
        Instruction::Phi { .. } => "phi",
    }
}

/// A test case for the CFG analyzer viewer.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CfgTestCase {
    /// Name of the function
    pub name: String,
    /// The original CFG (serializable form)
    pub original_cfg: SerializableCfg,
    /// CFG after mem2reg pass (if any transformation occurred)
    pub after_mem2reg: Option<SerializableCfg>,
    /// CFG after heap elimination pass (if any transformation occurred)
    pub after_heap_elim: Option<SerializableCfg>,
    /// Final optimized CFG (whichever stage succeeded last)
    pub optimized_cfg: SerializableCfg,
    /// Analysis results
    pub analysis: CfgAnalysisResult,
    /// Optimization pipeline results
    pub optimization_result: OptimizationResult,
    /// Optional reference shape (for shape-specific analysis)
    pub reference_shape: Option<CallShape>,
    /// Source location if available
    pub source_span: Option<(usize, usize)>, // (start_line, end_line)
}

/// Collection of test cases for the viewer.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CfgTestCases {
    pub test_cases: Vec<CfgTestCase>,
    pub generated_at: String,
}

impl CfgTestCases {
    pub fn new() -> Self {
        use std::time::SystemTime;
        let timestamp = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs().to_string())
            .unwrap_or_else(|_| "unknown".to_string());
        Self {
            test_cases: Vec::new(),
            generated_at: timestamp,
        }
    }

    pub fn add_case(&mut self, case: CfgTestCase) {
        self.test_cases.push(case);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::LocalId;
    use crate::pico8_num::Pico8Num;
    use std::hash::BuildHasherDefault;
    use rustc_hash::FxHasher;

    type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

    fn make_simple_cfg() -> Cfg {
        // A simple CFG: %0 = NumberConstant(5); return %0
        let entry = Block {
            instructions: vec![(
                LocalId::from(0),
                Instruction::NumberConstant {
                    value: Pico8Num::from_i16(5),
                },
            )],
            terminator: (
                LocalId::from(1),
                Terminator::Return {
                    value: Some(LocalId::from(0)),
                },
            ),
            hint_normalize: false,
        };

        Cfg {
            entry,
            named: FxHashMap::default(),
        }
    }

    fn make_heap_read_cfg() -> Cfg {
        // A CFG that reads from heap: %0 = GetGlobal("x"); %1 = Load(%0); return %1
        let entry = Block {
            instructions: vec![
                (
                    LocalId::from(0),
                    Instruction::GetGlobal {
                        name: "x".to_string(),
                        create_if_missing: false,
                    },
                ),
                (LocalId::from(1), Instruction::Load { source: LocalId::from(0) }),
            ],
            terminator: (
                LocalId::from(2),
                Terminator::Return {
                    value: Some(LocalId::from(1)),
                },
            ),
            hint_normalize: false,
        };

        Cfg {
            entry,
            named: FxHashMap::default(),
        }
    }

    fn make_heap_write_cfg() -> Cfg {
        // A CFG that writes to heap: %0 = GetGlobal("x"); %1 = NumberConstant(5); Store(%0, %1); return
        let entry = Block {
            instructions: vec![
                (
                    LocalId::from(0),
                    Instruction::GetGlobal {
                        name: "x".to_string(),
                        create_if_missing: false,
                    },
                ),
                (
                    LocalId::from(1),
                    Instruction::NumberConstant {
                        value: Pico8Num::from_i16(5),
                    },
                ),
                (
                    LocalId::from(2),
                    Instruction::Store {
                        target: LocalId::from(0),
                        source: LocalId::from(1),
                    },
                ),
            ],
            terminator: (LocalId::from(3), Terminator::Return { value: None }),
            hint_normalize: false,
        };

        Cfg {
            entry,
            named: FxHashMap::default(),
        }
    }

    #[test]
    fn test_analyze_simple_cfg() {
        let cfg = make_simple_cfg();
        let result = analyze_cfg(&cfg);

        assert!(!result.reads_heap);
        assert!(!result.writes_heap);
        assert!(!result.modifies_heap_shape);
        assert!(!result.has_calls);
        assert!(result.is_heap_free());
        assert!(result.is_pure());
        assert_eq!(result.instruction_counts.number_constant, 1);
        assert_eq!(result.instruction_counts.total_instructions, 1);
        assert_eq!(result.instruction_counts.total_blocks, 1);
    }

    #[test]
    fn test_analyze_heap_read_cfg() {
        let cfg = make_heap_read_cfg();
        let result = analyze_cfg(&cfg);

        assert!(result.reads_heap);
        assert!(!result.writes_heap);
        assert!(!result.modifies_heap_shape);
        assert!(!result.has_calls);
        assert!(!result.is_heap_free());
        assert!(result.is_pure()); // Reading doesn't prevent purity
        assert!(result.reads_globals);
        assert!(result.accessed_globals.contains(&"x".to_string()));
    }

    #[test]
    fn test_analyze_heap_write_cfg() {
        let cfg = make_heap_write_cfg();
        let result = analyze_cfg(&cfg);

        assert!(!result.reads_heap);
        assert!(result.writes_heap);
        assert!(!result.modifies_heap_shape);
        assert!(!result.has_calls);
        assert!(!result.is_heap_free());
        assert!(!result.is_pure()); // Writing prevents purity
    }

    #[test]
    fn test_serializable_cfg() {
        let cfg = make_simple_cfg();
        let serializable: SerializableCfg = (&cfg).into();

        assert_eq!(serializable.entry_block.instructions.len(), 1);
        assert_eq!(
            serializable.entry_block.instructions[0].instruction_type,
            "number_constant"
        );
        assert!(serializable.named_blocks.is_empty());
    }
}
