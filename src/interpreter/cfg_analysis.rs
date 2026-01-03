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
    /// Result of block coalescing pass
    pub block_coalesce: BlockCoalesceStatus,
    /// Result of builtin resolution pass
    pub builtin_resolution: BuiltinResolutionStatus,
    /// Result of call resolution pass
    pub call_resolution: CallResolutionStatus,
    /// Result of inlining pass
    pub inlining: InliningStatus,
    /// Result of dead code elimination pass
    pub dce: DceStatus,
}

/// Status of the block coalescing pass
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub enum BlockCoalesceStatus {
    #[default]
    NotAttempted,
    /// Successfully coalesced blocks
    Success {
        blocks_removed: usize,
    },
    /// No blocks to coalesce
    NoChange,
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

/// Status of the call resolution pass
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub enum CallResolutionStatus {
    #[default]
    NotAttempted,
    /// Successfully resolved calls
    Success {
        calls_resolved: usize,
    },
    /// No calls to resolve
    NoChange,
}

/// Status of the inlining pass
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub enum InliningStatus {
    #[default]
    NotAttempted,
    /// Successfully inlined calls
    Success {
        calls_inlined: usize,
    },
    /// No calls to inline
    NoChange,
}

/// Status of the dead code elimination pass
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub enum DceStatus {
    #[default]
    NotAttempted,
    /// Successfully eliminated dead code
    Success {
        instructions_removed: usize,
    },
    /// No dead code found
    NoChange,
}

/// Status of the builtin resolution pass
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub enum BuiltinResolutionStatus {
    #[default]
    NotAttempted,
    /// Successfully resolved builtin calls
    Success {
        calls_resolved: usize,
    },
    /// No builtin calls to resolve
    NoChange,
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
    pub call_builtin: usize,
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

        // CallBuiltin arguments: any allocation passed to a builtin call escapes
        Instruction::CallBuiltin { args, .. } => {
            for arg in args {
                if allocations.contains(arg) {
                    escaping.insert(*arg);
                }
            }
        }

        // CallResolved arguments and captures: any allocation passed escapes
        Instruction::CallResolved { captures, args, .. } => {
            for arg in args.iter().chain(captures.iter()) {
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

/// Intermediate CFG results from the optimization pipeline
#[derive(Clone, Debug, Default)]
pub struct OptimizationCfgs {
    pub after_mem2reg: Option<Cfg>,
    pub after_heap_elim: Option<Cfg>,
    pub after_block_coalesce: Option<Cfg>,
    pub after_builtin_resolution: Option<Cfg>,
    pub after_call_resolution: Option<Cfg>,
    pub after_inlining: Option<Cfg>,
    pub after_dce: Option<Cfg>,
}

/// Run the full optimization pipeline on a CFG.
///
/// Pipeline stages:
/// 1. mem2reg: Promote local cells (non-escaping allocations) to SSA
/// 2. heap_elimination: Eliminate global heap operations with known shapes
/// 3. block_coalesce: Merge straight-line blocks
///
/// Returns the optimization result and intermediate CFGs for visualization.
pub fn run_optimization_pipeline(
    cfg: &Cfg,
    analysis: &CfgAnalysisResult,
) -> (OptimizationResult, Option<Cfg>, Option<Cfg>) {
    let (result, cfgs) = run_optimization_pipeline_full(cfg, analysis);
    (result, cfgs.after_mem2reg, cfgs.after_heap_elim)
}

/// Run the full optimization pipeline, returning all intermediate CFGs.
pub fn run_optimization_pipeline_full(
    cfg: &Cfg,
    analysis: &CfgAnalysisResult,
) -> (OptimizationResult, OptimizationCfgs) {
    use crate::interpreter::block_coalesce::{coalesce_blocks, CoalesceResult};
    use crate::interpreter::heap_elimination::{
        eliminate_heap, HeapEliminationResult, HeapShape, ValueShape,
    };
    use crate::interpreter::mem2reg::{mem2reg, Mem2RegResult};
    use crate::ir::{LabelGenerator, LocalIdGenerator};

    let mut result = OptimizationResult::default();
    let mut cfgs = OptimizationCfgs::default();

    // Stage 1: mem2reg - eliminate local cells
    let mut local_gen = LocalIdGenerator::from_cfg(cfg);
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
    cfgs.after_mem2reg = after_mem2reg.clone();

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
    cfgs.after_heap_elim = after_heap_elim.clone();

    // Stage 3: block coalescing - merge straight-line blocks
    let cfg_for_coalesce = after_heap_elim.as_ref()
        .or(after_mem2reg.as_ref())
        .unwrap_or(cfg);

    match coalesce_blocks(cfg_for_coalesce) {
        CoalesceResult::Success { cfg: new_cfg, blocks_removed } => {
            result.block_coalesce = BlockCoalesceStatus::Success { blocks_removed };
            cfgs.after_block_coalesce = Some(new_cfg);
        }
        CoalesceResult::NoChange => {
            result.block_coalesce = BlockCoalesceStatus::NoChange;
        }
    }

    (result, cfgs)
}

/// Optimized function definition with its CFG.
#[derive(Clone, Debug)]
pub struct OptimizedFunDef {
    pub name: crate::ir::GlobalId,
    pub cfg: Cfg,
}

/// Optimize all function definitions with proper dependency ordering.
///
/// This ensures that when we inline function A into function B, we use the
/// already-optimized version of A. Functions are processed in topological order
/// (callees before callers).
///
/// The `builtin_set` parameter is optional; if provided, builtin calls will be resolved
/// to `CallBuiltin` instructions, eliminating the GetGlobal + Load overhead.
///
/// Returns a map from function name to optimized CFG.
pub fn optimize_all_functions(
    fun_defs: &[&crate::ir::FunDef],
    global_closure_map: &crate::interpreter::call_resolution::GlobalClosureMap,
    builtin_set: Option<&crate::interpreter::builtin_resolution::BuiltinSet>,
) -> std::collections::HashMap<crate::ir::GlobalId, OptimizedFunDef, std::hash::BuildHasherDefault<rustc_hash::FxHasher>> {
    use crate::interpreter::block_coalesce::{coalesce_blocks, CoalesceResult};
    use crate::interpreter::builtin_resolution::{resolve_builtins, BuiltinResolutionResult};
    use crate::interpreter::call_resolution::{resolve_calls, CallResolutionResult};
    use crate::interpreter::dce::{eliminate_dead_code, DceResult};
    use crate::interpreter::inlining::{inline_calls, InliningResult};
    use crate::interpreter::mem2reg::{mem2reg, Mem2RegResult};
    use crate::ir::{GlobalId, LabelGenerator, LocalIdGenerator};
    use std::hash::BuildHasherDefault;
    use rustc_hash::FxHasher;

    type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

    // Step 1: Build call graph to determine processing order
    // For each function, collect which functions it calls (after call resolution)
    let mut call_graph: FxHashMap<GlobalId, Vec<GlobalId>> = FxHashMap::default();
    let mut all_names: Vec<GlobalId> = Vec::new();

    for fun_def in fun_defs {
        all_names.push(fun_def.name.clone());

        // First run call resolution to see what functions this calls
        let analysis = analyze_cfg(&fun_def.cfg);
        let mut local_gen = LocalIdGenerator::from_cfg(&fun_def.cfg);
        let cfg_after_mem2reg = if analysis.local_only_allocs > 0 {
            match mem2reg(&fun_def.cfg, &mut local_gen) {
                Mem2RegResult::Success { cfg, .. } | Mem2RegResult::PartialSuccess { cfg, .. } => cfg,
                Mem2RegResult::NoCells => fun_def.cfg.clone(),
            }
        } else {
            fun_def.cfg.clone()
        };

        let cfg_after_call_res = match resolve_calls(&cfg_after_mem2reg, global_closure_map) {
            CallResolutionResult::Success { cfg, .. } => cfg,
            CallResolutionResult::NoChange => cfg_after_mem2reg,
        };

        // Extract CallResolved targets
        let mut callees = Vec::new();
        for block in cfg_after_call_res.iter_blocks() {
            for (_, instr) in &block.instructions {
                if let crate::ir::Instruction::CallResolved { fun_name, .. } = instr {
                    callees.push(fun_name.clone());
                }
            }
        }
        call_graph.insert(fun_def.name.clone(), callees);
    }

    // Step 2: Topological sort (callees before callers)
    let sorted_names = topological_sort(&all_names, &call_graph);

    // Step 3: Process functions in order, building up optimized versions
    let mut optimized: FxHashMap<GlobalId, crate::ir::FunDef> = FxHashMap::default();
    let mut result: FxHashMap<GlobalId, OptimizedFunDef> = FxHashMap::default();

    // Build initial fun_def map
    let fun_def_by_name: FxHashMap<GlobalId, &crate::ir::FunDef> = fun_defs
        .iter()
        .map(|fd| (fd.name.clone(), *fd))
        .collect();

    for name in sorted_names {
        let original_fun_def = fun_def_by_name.get(&name).unwrap();
        let mut local_gen = LocalIdGenerator::from_cfg(&original_fun_def.cfg);
        let mut label_gen = LabelGenerator::new();
        let fn_name = name.as_str();
        // Combine arg_ids and capture_ids - both are available as "external" locals
        // arg_ids are Option<LocalId> because some args may be unused (None)
        let external_ids: Vec<_> = original_fun_def.arg_ids.iter()
            .filter_map(|id| *id)
            .chain(original_fun_def.capture_ids.iter().copied())
            .collect();

        // Validate input CFG
        crate::interpreter::cfg_validation::assert_valid_cfg_with_args(
            &original_fun_def.cfg,
            &external_ids,
            &format!("{} original", fn_name),
        );

        // Stage 1: mem2reg
        let analysis = analyze_cfg(&original_fun_def.cfg);
        let mut current_cfg = if analysis.local_only_allocs > 0 {
            match mem2reg(&original_fun_def.cfg, &mut local_gen) {
                Mem2RegResult::Success { cfg, .. } | Mem2RegResult::PartialSuccess { cfg, .. } => {
                    crate::interpreter::cfg_validation::assert_valid_cfg_with_args(
                        &cfg,
                        &external_ids,
                        &format!("{} after mem2reg", fn_name),
                    );
                    cfg
                }
                Mem2RegResult::NoCells => original_fun_def.cfg.clone(),
            }
        } else {
            original_fun_def.cfg.clone()
        };

        // Iterate builtin resolution + call resolution + inlining until no more progress
        let max_rounds = 10; // Safety limit
        for round in 0..max_rounds {
            // Stage 2a: builtin resolution (if builtin set is provided)
            if let Some(builtins) = builtin_set {
                current_cfg = match resolve_builtins(&current_cfg, builtins) {
                    BuiltinResolutionResult::Success { cfg, .. } => {
                        crate::interpreter::cfg_validation::assert_valid_cfg_with_args(
                            &cfg,
                            &external_ids,
                            &format!("{} after builtin_resolution round {}", fn_name, round),
                        );
                        cfg
                    }
                    BuiltinResolutionResult::NoChange => current_cfg,
                };
            }

            // Stage 2b: call resolution (for user-defined functions)
            current_cfg = match resolve_calls(&current_cfg, global_closure_map) {
                CallResolutionResult::Success { cfg, .. } => {
                    crate::interpreter::cfg_validation::assert_valid_cfg_with_args(
                        &cfg,
                        &external_ids,
                        &format!("{} after call_resolution round {}", fn_name, round),
                    );
                    cfg
                }
                CallResolutionResult::NoChange => current_cfg,
            };

            // Stage 3: inlining (using already-optimized callees!)
            match inline_calls(&current_cfg, &optimized, &mut local_gen, &mut label_gen) {
                InliningResult::Success { cfg, .. } => {
                    crate::interpreter::cfg_validation::assert_valid_cfg_with_args(
                        &cfg,
                        &external_ids,
                        &format!("{} after inlining round {}", fn_name, round),
                    );
                    current_cfg = cfg;
                    // Continue to next round - might have more calls to resolve/inline
                }
                InliningResult::NoChange => {
                    break; // No more inlining possible
                }
            }
        }

        // Stage 4: Dead code elimination
        // After inlining, there may be unused GetGlobal/Load instructions
        // that were only used to set up the original Call
        current_cfg = match eliminate_dead_code(&current_cfg) {
            DceResult::Success { cfg, .. } => {
                crate::interpreter::cfg_validation::assert_valid_cfg_with_args(
                    &cfg,
                    &external_ids,
                    &format!("{} after dce", fn_name),
                );
                cfg
            }
            DceResult::NoChange => current_cfg,
        };

        // Stage 5: block coalescing
        current_cfg = match coalesce_blocks(&current_cfg) {
            CoalesceResult::Success { cfg, .. } => {
                crate::interpreter::cfg_validation::assert_valid_cfg_with_args(
                    &cfg,
                    &external_ids,
                    &format!("{} after block_coalesce", fn_name),
                );
                cfg
            }
            CoalesceResult::NoChange => current_cfg,
        };

        // Final validation
        crate::interpreter::cfg_validation::assert_valid_cfg_with_args(
            &current_cfg,
            &external_ids,
            &format!("{} final", fn_name),
        );

        // Store the optimized version for use by later functions
        let optimized_fun_def = crate::ir::FunDef {
            name: original_fun_def.name.clone(),
            capture_ids: original_fun_def.capture_ids.clone(),
            arg_ids: original_fun_def.arg_ids.clone(),
            cfg: current_cfg.clone(),
            source_span: original_fun_def.source_span.clone(),
        };
        optimized.insert(name.clone(), optimized_fun_def);

        result.insert(name.clone(), OptimizedFunDef {
            name: name.clone(),
            cfg: current_cfg,
        });
    }

    result
}

/// Topological sort of function names based on call graph.
/// Returns functions in order such that callees come before callers.
fn topological_sort(
    names: &[crate::ir::GlobalId],
    call_graph: &std::collections::HashMap<crate::ir::GlobalId, Vec<crate::ir::GlobalId>, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>,
) -> Vec<crate::ir::GlobalId> {
    use std::collections::HashSet;

    let name_set: HashSet<_> = names.iter().cloned().collect();
    let mut visited = HashSet::new();
    let mut result = Vec::new();

    fn visit(
        name: &crate::ir::GlobalId,
        call_graph: &std::collections::HashMap<crate::ir::GlobalId, Vec<crate::ir::GlobalId>, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>,
        name_set: &HashSet<crate::ir::GlobalId>,
        visited: &mut HashSet<crate::ir::GlobalId>,
        result: &mut Vec<crate::ir::GlobalId>,
    ) {
        if visited.contains(name) {
            return;
        }
        visited.insert(name.clone());

        // Visit callees first
        if let Some(callees) = call_graph.get(name) {
            for callee in callees {
                // Only visit if it's in our set of functions (not builtins)
                if name_set.contains(callee) {
                    visit(callee, call_graph, name_set, visited, result);
                }
            }
        }

        result.push(name.clone());
    }

    for name in names {
        visit(name, call_graph, &name_set, &mut visited, &mut result);
    }

    result
}

/// Run the optimization pipeline with inter-procedural passes (call resolution and inlining).
///
/// This version uses pre-optimized function definitions for inlining, ensuring
/// that inlined code is already optimized.
///
/// Pipeline stages:
/// 1. mem2reg: Promote local cells (non-escaping allocations) to SSA
/// 2. call_resolution + inlining: Iteratively resolve and inline calls
/// 3. block_coalesce: Merge straight-line blocks
/// 4. heap_elimination: Eliminate global heap operations with known shapes
pub fn run_optimization_pipeline_with_interprocedural(
    cfg: &Cfg,
    analysis: &CfgAnalysisResult,
    global_closure_map: &crate::interpreter::call_resolution::GlobalClosureMap,
    optimized_fun_defs: &std::collections::HashMap<crate::ir::GlobalId, crate::ir::FunDef, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>,
) -> (OptimizationResult, OptimizationCfgs) {
    use crate::interpreter::block_coalesce::{coalesce_blocks, CoalesceResult};
    use crate::interpreter::call_resolution::{resolve_calls, CallResolutionResult};
    use crate::interpreter::dce::{eliminate_dead_code, DceResult};
    use crate::interpreter::heap_elimination::{
        eliminate_heap, HeapEliminationResult, HeapShape, ValueShape,
    };
    use crate::interpreter::inlining::{inline_calls, InliningResult};
    use crate::interpreter::mem2reg::{mem2reg, Mem2RegResult};
    use crate::ir::{LabelGenerator, LocalIdGenerator};

    let mut result = OptimizationResult::default();
    let mut cfgs = OptimizationCfgs::default();
    let mut local_gen = LocalIdGenerator::from_cfg(cfg);
    let mut label_gen = LabelGenerator::new();

    // Stage 1: mem2reg - eliminate local cells
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
    cfgs.after_mem2reg = after_mem2reg.clone();

    // Get the CFG for call resolution
    let cfg_for_call_res = after_mem2reg.as_ref().unwrap_or(cfg);

    // Stage 2: call resolution - convert Call to CallResolved
    let after_call_resolution = match resolve_calls(cfg_for_call_res, global_closure_map) {
        CallResolutionResult::Success { cfg: new_cfg, calls_resolved } => {
            result.call_resolution = CallResolutionStatus::Success { calls_resolved };
            Some(new_cfg)
        }
        CallResolutionResult::NoChange => {
            result.call_resolution = CallResolutionStatus::NoChange;
            None
        }
    };
    cfgs.after_call_resolution = after_call_resolution.clone();

    // Get the CFG for inlining
    let cfg_for_inlining = after_call_resolution
        .as_ref()
        .or(after_mem2reg.as_ref())
        .unwrap_or(cfg);

    // Stage 3: inlining - inline CallResolved calls using optimized callees
    // Iterate until no more progress (handles calls from inlined code)
    let mut current_cfg = cfg_for_inlining.clone();
    let mut total_calls_inlined = 0;
    let max_rounds = 10;

    for _ in 0..max_rounds {
        // Re-run call resolution to pick up any new calls from inlined code
        current_cfg = match resolve_calls(&current_cfg, global_closure_map) {
            CallResolutionResult::Success { cfg, calls_resolved } => {
                if total_calls_inlined == 0 {
                    // First round - update the count
                    result.call_resolution = CallResolutionStatus::Success { calls_resolved };
                }
                cfg
            }
            CallResolutionResult::NoChange => current_cfg,
        };

        match inline_calls(&current_cfg, optimized_fun_defs, &mut local_gen, &mut label_gen) {
            InliningResult::Success { cfg: new_cfg, calls_inlined } => {
                current_cfg = new_cfg;
                total_calls_inlined += calls_inlined;
            }
            InliningResult::NoChange => {
                break;
            }
        }
    }

    let after_inlining = if total_calls_inlined > 0 {
        result.inlining = InliningStatus::Success { calls_inlined: total_calls_inlined };
        Some(current_cfg.clone())
    } else {
        result.inlining = InliningStatus::NoChange;
        None
    };
    cfgs.after_inlining = after_inlining.clone();

    // Stage 4: Dead code elimination
    // After inlining, there may be unused GetGlobal/Load instructions that were only used
    // to set up the original Call (which has been replaced by inlined code)
    let cfg_for_dce = after_inlining
        .as_ref()
        .or(after_call_resolution.as_ref())
        .or(after_mem2reg.as_ref())
        .unwrap_or(cfg);

    let after_dce = match eliminate_dead_code(cfg_for_dce) {
        DceResult::Success { cfg: new_cfg, instructions_removed } => {
            result.dce = DceStatus::Success { instructions_removed };
            Some(new_cfg)
        }
        DceResult::NoChange => {
            result.dce = DceStatus::NoChange;
            None
        }
    };
    cfgs.after_dce = after_dce.clone();

    // Stage 5: block coalescing - merge straight-line blocks
    let cfg_for_coalesce = after_dce
        .as_ref()
        .or(after_inlining.as_ref())
        .or(after_call_resolution.as_ref())
        .or(after_mem2reg.as_ref())
        .unwrap_or(cfg);

    let after_block_coalesce = match coalesce_blocks(cfg_for_coalesce) {
        CoalesceResult::Success { cfg: new_cfg, blocks_removed } => {
            result.block_coalesce = BlockCoalesceStatus::Success { blocks_removed };
            Some(new_cfg)
        }
        CoalesceResult::NoChange => {
            result.block_coalesce = BlockCoalesceStatus::NoChange;
            None
        }
    };
    cfgs.after_block_coalesce = after_block_coalesce.clone();

    // Stage 6: heap elimination - for globals (after other transformations)
    let cfg_for_heap_elim = after_block_coalesce
        .as_ref()
        .or(after_dce.as_ref())
        .or(after_inlining.as_ref())
        .or(after_call_resolution.as_ref())
        .or(after_mem2reg.as_ref())
        .unwrap_or(cfg);

    // Re-analyze the CFG to get current accessed_globals (inlining may have added new ones)
    let current_analysis = analyze_cfg(cfg_for_heap_elim);

    if !current_analysis.accessed_globals.is_empty() {
        // Create a simple shape where each global is a leaf value
        let mut shape = HeapShape::new();
        for global in &current_analysis.accessed_globals {
            shape.globals.insert(global.clone(), ValueShape::Leaf);
        }

        match eliminate_heap(cfg_for_heap_elim, &shape, local_gen, label_gen) {
            HeapEliminationResult::Success(transformed) => {
                result.heap_elimination = HeapEliminationStatus::Success {
                    unpack_count: transformed.unpack_slots.len(),
                    modified_count: transformed.modified_slots.len(),
                };
                cfgs.after_heap_elim = Some(transformed.cfg);
            }
            HeapEliminationResult::ShapeNotPreserved(reason) => {
                result.heap_elimination = HeapEliminationStatus::Failed(reason);
            }
            HeapEliminationResult::HasExternalCalls(funcs) => {
                let reason = format!("External calls: {}", funcs.join(", "));
                result.heap_elimination = HeapEliminationStatus::Failed(reason);
            }
            HeapEliminationResult::NotApplicable => {
                result.heap_elimination = HeapEliminationStatus::NotApplicable;
            }
        }
    } else {
        result.heap_elimination = HeapEliminationStatus::NotApplicable;
    }

    (result, cfgs)
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
        Instruction::CallResolved { .. } => {
            result.has_calls = true;
            // CallResolved is a direct call, no closure local to track
            result.instruction_counts.call += 1;
        }
        Instruction::CallBuiltin { .. } => {
            result.has_calls = true;
            // CallBuiltin is a direct call to a builtin, no closure local to track
            result.instruction_counts.call_builtin += 1;
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
        Instruction::CallResolved {
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
        Instruction::CallBuiltin { name, args } => {
            let arg_strs: Vec<_> = args.iter().map(|id| usize::from(*id).to_string()).collect();
            format!("CallBuiltin({}, [{}])", name, arg_strs.join(", "))
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
        Instruction::CallResolved { .. } => "call_resolved",
        Instruction::CallBuiltin { .. } => "call_builtin",
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
    /// CFG after block coalescing pass (if any transformation occurred)
    pub after_block_coalesce: Option<SerializableCfg>,
    /// CFG after builtin resolution pass (if any transformation occurred)
    pub after_builtin_resolution: Option<SerializableCfg>,
    /// CFG after call resolution pass (if any transformation occurred)
    pub after_call_resolution: Option<SerializableCfg>,
    /// CFG after inlining pass (if any transformation occurred)
    pub after_inlining: Option<SerializableCfg>,
    /// CFG after dead code elimination pass (if any transformation occurred)
    pub after_dce: Option<SerializableCfg>,
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

    #[test]
    fn test_topological_sort() {
        // Test that topological sort orders callees before callers
        use crate::ir::GlobalId;

        let names = vec![
            GlobalId::from("a".to_string()),
            GlobalId::from("b".to_string()),
            GlobalId::from("c".to_string()),
        ];

        // Call graph: a calls b, b calls c
        // So order should be: c, b, a (callees before callers)
        let mut call_graph: FxHashMap<GlobalId, Vec<GlobalId>> = FxHashMap::default();
        call_graph.insert(GlobalId::from("a".to_string()), vec![GlobalId::from("b".to_string())]);
        call_graph.insert(GlobalId::from("b".to_string()), vec![GlobalId::from("c".to_string())]);
        call_graph.insert(GlobalId::from("c".to_string()), vec![]);

        let sorted = topological_sort(&names, &call_graph);

        // c should come before b, b should come before a
        let c_pos = sorted.iter().position(|n| n.as_str() == "c").unwrap();
        let b_pos = sorted.iter().position(|n| n.as_str() == "b").unwrap();
        let a_pos = sorted.iter().position(|n| n.as_str() == "a").unwrap();

        assert!(c_pos < b_pos, "c should come before b (c calls nothing, b calls c)");
        assert!(b_pos < a_pos, "b should come before a (a calls b)");
    }

    #[test]
    fn test_optimize_all_functions_uses_optimized_callees() {
        // Test that when inlining, we use the optimized version of callees
        //
        // Setup:
        // - Function `inner`: returns 1 + 1 (simple, will be optimized)
        // - Function `outer`: calls inner via CallResolved
        //
        // When we inline `inner` into `outer`, we should get the optimized
        // version of `inner` (which has already had mem2reg etc. applied)
        use crate::ir::{BinaryOp, FunDef, GlobalId};
        use crate::interpreter::call_resolution::GlobalClosure;

        // Create inner function: %0 = 1, %1 = 1, %2 = %0 + %1, return %2
        let inner_cfg = Cfg {
            entry: Block {
                instructions: vec![
                    (
                        LocalId::from(0),
                        Instruction::NumberConstant { value: Pico8Num::from_i16(1) },
                    ),
                    (
                        LocalId::from(1),
                        Instruction::NumberConstant { value: Pico8Num::from_i16(1) },
                    ),
                    (
                        LocalId::from(2),
                        Instruction::BinaryOp {
                            left: LocalId::from(0),
                            op: BinaryOp::Plus,
                            right: LocalId::from(1),
                        },
                    ),
                ],
                terminator: (LocalId::from(3), Terminator::Return { value: Some(LocalId::from(2)) }),
                hint_normalize: false,
            },
            named: FxHashMap::default(),
        };

        let inner_def = FunDef {
            name: GlobalId::from("inner_1".to_string()),
            capture_ids: vec![],
            arg_ids: vec![],
            cfg: inner_cfg,
            source_span: None,
        };

        // Create outer function that calls inner:
        // %0 = GetGlobal("inner"), %1 = Load(%0), %2 = Call(%1, [])
        // This pattern will be resolved to CallResolved by call_resolution
        let outer_cfg = Cfg {
            entry: Block {
                instructions: vec![
                    (
                        LocalId::from(0),
                        Instruction::GetGlobal { name: "inner".to_string(), create_if_missing: false },
                    ),
                    (
                        LocalId::from(1),
                        Instruction::Load { source: LocalId::from(0) },
                    ),
                    (
                        LocalId::from(2),
                        Instruction::Call { closure: LocalId::from(1), args: vec![] },
                    ),
                ],
                terminator: (LocalId::from(3), Terminator::Return { value: Some(LocalId::from(2)) }),
                hint_normalize: false,
            },
            named: FxHashMap::default(),
        };

        let outer_def = FunDef {
            name: GlobalId::from("outer_2".to_string()),
            capture_ids: vec![],
            arg_ids: vec![],
            cfg: outer_cfg,
            source_span: None,
        };

        // Build global closure map
        let mut global_closure_map: FxHashMap<String, GlobalClosure> = FxHashMap::default();
        global_closure_map.insert("inner".to_string(), GlobalClosure {
            fun_name: GlobalId::from("inner_1".to_string()),
            has_captures: false,
        });

        // Run optimize_all_functions
        let fun_defs = vec![&inner_def, &outer_def];
        let optimized = optimize_all_functions(&fun_defs, &global_closure_map, None);

        // Check that both functions are in the result
        assert!(optimized.contains_key(&GlobalId::from("inner_1".to_string())));
        assert!(optimized.contains_key(&GlobalId::from("outer_2".to_string())));

        // The outer function should have been modified (call resolved and inlined)
        let optimized_outer = optimized.get(&GlobalId::from("outer_2".to_string())).unwrap();

        // After optimization, outer should NOT contain a Call or CallResolved instruction
        // (it should have been inlined)
        let has_call = optimized_outer.cfg.iter_blocks().any(|block| {
            block.instructions.iter().any(|(_, instr)| {
                matches!(instr, Instruction::Call { .. } | Instruction::CallResolved { .. })
            })
        });

        assert!(!has_call, "outer should have the call to inner inlined");

        // The inlined code should contain the body of inner (BinaryOp Plus)
        let has_plus = optimized_outer.cfg.iter_blocks().any(|block| {
            block.instructions.iter().any(|(_, instr)| {
                matches!(instr, Instruction::BinaryOp { op: BinaryOp::Plus, .. })
            })
        });

        assert!(has_plus, "outer should contain the inlined Plus operation from inner");
    }
}
