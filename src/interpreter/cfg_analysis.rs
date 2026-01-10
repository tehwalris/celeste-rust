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

use crate::interpreter::common::FxHashMap;
use crate::ir::{Block, Cfg, FunDef, GlobalId, Instruction, LocalId, Terminator};

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

/// A serializable mapping from a heap slot path to its initial SSA LocalId
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SlotMapping {
    /// Human-readable path (e.g., "arg0.x", "_G.player.spd.x")
    pub path: String,
    /// The LocalId assigned to this slot's initial value
    pub local_id: u32,
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
        /// Mapping from heap slot paths to their initial SSA LocalIds
        /// This explains what synthetic LocalIds like %52, %53 represent
        #[serde(default)]
        slot_mappings: Vec<SlotMapping>,
    },
    /// Transformation failed with reason
    Failed(String),
    /// Not applicable (e.g., no heap operations)
    NotApplicable,
}

/// Type of optimization step in the pipeline
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum PipelineStepType {
    /// The original CFG before any optimization
    Original,
    /// mem2reg pass - promotes local cells to SSA
    Mem2Reg { cells_promoted: usize },
    /// Builtin resolution - resolves calls to known builtins
    BuiltinResolution { calls_resolved: usize },
    /// Heap elimination in Stop mode - resolves calls via shape tracking
    HeapElimStop { calls_resolved: usize, iteration: usize },
    /// Inlining pass - inlines resolved calls
    Inlining { calls_inlined: usize, iteration: usize },
    /// Dead code elimination
    Dce { instructions_removed: usize },
    /// Block coalescing - merges straight-line blocks
    BlockCoalesce { blocks_removed: usize },
    /// Deopt unsafe builtins - inserts deopt points for unsafe builtins
    DeoptBuiltins { deopts_inserted: usize },
    /// Final heap elimination with deopt insertion
    HeapElimFinal {
        unpack_count: usize,
        modified_count: usize,
        slot_mappings: Vec<SlotMapping>,
    },
}

/// A single step in the optimization pipeline (internal representation)
#[derive(Clone, Debug)]
pub struct PipelineStep {
    /// Human-readable name for this step
    pub name: String,
    /// Type of optimization that was applied
    pub step_type: PipelineStepType,
    /// The CFG after this step
    pub cfg: Cfg,
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
    /// Result of deopt unsafe builtins pass
    pub deopt_builtins: DeoptBuiltinsStatus,
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
    /// Skipped because no builtin set was provided
    Skipped,
}

/// Status of the deopt unsafe builtins pass
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub enum DeoptBuiltinsStatus {
    #[default]
    NotAttempted,
    /// Successfully inserted deopt points for unsafe builtins
    Success {
        deopts_inserted: usize,
    },
    /// No unsafe builtins found
    NoChange,
}

/// Context for inter-procedural optimization passes.
///
/// This bundles the various maps and information needed for call resolution,
/// inlining, and other interprocedural analyses into a single struct to reduce
/// parameter counts and improve code readability.
pub struct InterproceduralContext<'a> {
    /// Mapping from global names to their closure definitions
    pub global_closure_map: &'a crate::interpreter::call_resolution::GlobalClosureMap,
    /// Pre-optimized function definitions available for inlining
    pub optimized_fun_defs: &'a FxHashMap<GlobalId, FunDef>,
    /// Optional set of builtin functions to resolve
    pub builtin_set: Option<&'a crate::interpreter::builtin_resolution::BuiltinSet>,
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
    for block in cfg.named.values() {
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
        if let Terminator::Return { value: Some(ret_id) } = block.terminator_kind() {
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
    /// All pipeline steps in order, including iterations
    pub steps: Vec<PipelineStep>,

    // Per-pass results for visualization (most are set by run_optimization_pipeline_with_interprocedural)
    pub after_mem2reg: Option<Cfg>,
    pub after_heap_elim: Option<Cfg>,
    pub after_block_coalesce: Option<Cfg>,
    pub after_builtin_resolution: Option<Cfg>,
    pub after_call_resolution: Option<Cfg>,
    pub after_inlining: Option<Cfg>,
    pub after_dce: Option<Cfg>,
    pub after_deopt_builtins: Option<Cfg>,
}

impl OptimizationCfgs {
    /// Returns the latest CFG from the pipeline, checking passes in reverse order.
    ///
    /// The passes are checked in the order they run in the pipeline (latest first):
    /// heap_elim → deopt_builtins → block_coalesce → dce → inlining → call_resolution
    /// → builtin_resolution → mem2reg
    ///
    /// Returns `None` if no passes have produced a CFG yet.
    pub fn latest_cfg(&self) -> Option<&Cfg> {
        self.after_heap_elim
            .as_ref()
            .or(self.after_deopt_builtins.as_ref())
            .or(self.after_block_coalesce.as_ref())
            .or(self.after_dce.as_ref())
            .or(self.after_inlining.as_ref())
            .or(self.after_call_resolution.as_ref())
            .or(self.after_builtin_resolution.as_ref())
            .or(self.after_mem2reg.as_ref())
    }

    /// Returns the latest CFG up to and including the specified pass.
    ///
    /// This is useful when a pass needs to use the result of earlier passes but
    /// not later passes (e.g., when the later passes haven't run yet).
    pub fn latest_cfg_through(&self, pass: PipelinePass) -> Option<&Cfg> {
        match pass {
            PipelinePass::HeapElim => self.latest_cfg(),
            PipelinePass::DeoptBuiltins => self
                .after_deopt_builtins
                .as_ref()
                .or(self.after_block_coalesce.as_ref())
                .or(self.after_dce.as_ref())
                .or(self.after_inlining.as_ref())
                .or(self.after_call_resolution.as_ref())
                .or(self.after_builtin_resolution.as_ref())
                .or(self.after_mem2reg.as_ref()),
            PipelinePass::BlockCoalesce => self
                .after_block_coalesce
                .as_ref()
                .or(self.after_dce.as_ref())
                .or(self.after_inlining.as_ref())
                .or(self.after_call_resolution.as_ref())
                .or(self.after_builtin_resolution.as_ref())
                .or(self.after_mem2reg.as_ref()),
            PipelinePass::Dce => self
                .after_dce
                .as_ref()
                .or(self.after_inlining.as_ref())
                .or(self.after_call_resolution.as_ref())
                .or(self.after_builtin_resolution.as_ref())
                .or(self.after_mem2reg.as_ref()),
            PipelinePass::Inlining => self
                .after_inlining
                .as_ref()
                .or(self.after_call_resolution.as_ref())
                .or(self.after_builtin_resolution.as_ref())
                .or(self.after_mem2reg.as_ref()),
            PipelinePass::CallResolution => self
                .after_call_resolution
                .as_ref()
                .or(self.after_builtin_resolution.as_ref())
                .or(self.after_mem2reg.as_ref()),
            PipelinePass::BuiltinResolution => self
                .after_builtin_resolution
                .as_ref()
                .or(self.after_mem2reg.as_ref()),
            PipelinePass::Mem2Reg => self.after_mem2reg.as_ref(),
        }
    }
}

/// Pipeline passes in the order they are executed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PipelinePass {
    Mem2Reg,
    BuiltinResolution,
    CallResolution,
    Inlining,
    Dce,
    BlockCoalesce,
    DeoptBuiltins,
    HeapElim,
}

/// Optimized function definition with its CFG.
#[derive(Clone, Debug)]
pub struct OptimizedFunDef {
    pub name: crate::ir::GlobalId,
    pub cfg: Cfg,
}

/// Log a validation skip warning with consistent formatting.
///
/// This helper centralizes the formatting of validation error messages to avoid
/// duplication and ensure consistency across the optimization pipeline.
fn log_validation_skip(
    fn_name: &str,
    context: &str,
    errors: &[crate::interpreter::cfg_validation::ValidationError],
) {
    let context_suffix = if context.is_empty() {
        String::new()
    } else {
        format!(" {}", context)
    };
    eprintln!(
        "Warning: Skipping {} -{} validation failed: {} errors",
        fn_name,
        context_suffix,
        errors.len()
    );
    for (i, error) in errors.iter().take(3).enumerate() {
        eprintln!("  [{}] {}", i + 1, error);
    }
    if errors.len() > 3 {
        eprintln!("  ... and {} more", errors.len() - 3);
    }
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
    fun_defs: &[&FunDef],
    global_closure_map: &crate::interpreter::call_resolution::GlobalClosureMap,
    builtin_set: Option<&crate::interpreter::builtin_resolution::BuiltinSet>,
) -> FxHashMap<GlobalId, OptimizedFunDef> {
    use crate::interpreter::block_coalesce::{coalesce_blocks, CoalesceResult};
    use crate::interpreter::builtin_resolution::{resolve_builtins, BuiltinResolutionResult};
    use crate::interpreter::call_resolution::{resolve_calls, CallResolutionResult};
    use crate::interpreter::dce::{eliminate_dead_code, DceResult};
    use crate::interpreter::inlining::{inline_calls, InliningResult};
    use crate::interpreter::mem2reg::{mem2reg, Mem2RegResult};
    use crate::ir::{GlobalId, LabelGenerator, LocalIdGenerator};

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

    'function_loop: for name in sorted_names {
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

        // Validate input CFG - skip function if validation fails
        let validation_result = crate::interpreter::cfg_validation::validate_cfg_with_args(
            &original_fun_def.cfg,
            &external_ids,
        );
        if !validation_result.is_valid() {
            log_validation_skip(fn_name, "", &validation_result.errors);
            continue;
        }

        // Stage 1: mem2reg
        let analysis = analyze_cfg(&original_fun_def.cfg);
        let mut current_cfg = if analysis.local_only_allocs > 0 {
            match mem2reg(&original_fun_def.cfg, &mut local_gen) {
                Mem2RegResult::Success { cfg, .. } | Mem2RegResult::PartialSuccess { cfg, .. } => {
                    let validation = crate::interpreter::cfg_validation::validate_cfg_with_args(
                        &cfg,
                        &external_ids,
                    );
                    if !validation.is_valid() {
                        log_validation_skip(fn_name, "mem2reg", &validation.errors);
                        continue;
                    }
                    cfg
                }
                Mem2RegResult::NoCells => original_fun_def.cfg.clone(),
            }
        } else {
            original_fun_def.cfg.clone()
        };

        // Helper to validate and continue if valid
        macro_rules! validate_or_skip {
            ($cfg:expr, $context:expr) => {{
                let validation = crate::interpreter::cfg_validation::validate_cfg_with_args(
                    $cfg,
                    &external_ids,
                );
                if !validation.is_valid() {
                    log_validation_skip(fn_name, $context, &validation.errors);
                    continue 'function_loop;
                }
            }};
        }

        // Iterate builtin resolution + call resolution + inlining until no more progress
        let max_rounds = 10; // Safety limit
        'inlining_loop: for round in 0..max_rounds {
            // Stage 2a: builtin resolution (if builtin set is provided)
            if let Some(builtins) = builtin_set {
                current_cfg = match resolve_builtins(&current_cfg, builtins) {
                    BuiltinResolutionResult::Success { cfg, .. } => {
                        validate_or_skip!(&cfg, &format!("builtin_resolution round {}", round));
                        cfg
                    }
                    BuiltinResolutionResult::NoChange => current_cfg,
                };
            }

            // Stage 2b: call resolution (for user-defined functions)
            current_cfg = match resolve_calls(&current_cfg, global_closure_map) {
                CallResolutionResult::Success { cfg, .. } => {
                    validate_or_skip!(&cfg, &format!("call_resolution round {}", round));
                    cfg
                }
                CallResolutionResult::NoChange => current_cfg,
            };

            // Stage 3: inlining (using already-optimized callees!)
            match inline_calls(&current_cfg, &optimized, &mut local_gen, &mut label_gen) {
                InliningResult::Success { cfg, .. } => {
                    validate_or_skip!(&cfg, &format!("inlining round {}", round));
                    current_cfg = cfg;
                    // Continue to next round - might have more calls to resolve/inline
                }
                InliningResult::NoChange => {
                    break 'inlining_loop; // No more inlining possible
                }
            }
        }

        // Stage 4: Dead code elimination
        // After inlining, there may be unused GetGlobal/Load instructions
        // that were only used to set up the original Call
        current_cfg = match eliminate_dead_code(&current_cfg) {
            DceResult::Success { cfg, .. } => {
                validate_or_skip!(&cfg, "dce");
                cfg
            }
            DceResult::NoChange => current_cfg,
        };

        // Stage 5: block coalescing
        current_cfg = match coalesce_blocks(&current_cfg) {
            CoalesceResult::Success { cfg, .. } => {
                validate_or_skip!(&cfg, "block_coalesce");
                cfg
            }
            CoalesceResult::NoChange => current_cfg,
        };

        // Final validation
        validate_or_skip!(&current_cfg, "final");

        // Store the optimized version for use by later functions
        let optimized_fun_def = crate::ir::FunDef {
            name: original_fun_def.name.clone(),
            capture_ids: original_fun_def.capture_ids.clone(),
            arg_ids: original_fun_def.arg_ids.clone(),
            cfg: current_cfg.clone(),
            source_span: original_fun_def.source_span,
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
    names: &[GlobalId],
    call_graph: &FxHashMap<GlobalId, Vec<GlobalId>>,
) -> Vec<GlobalId> {
    use std::collections::HashSet;

    let name_set: HashSet<_> = names.iter().cloned().collect();
    let mut visited = HashSet::new();
    let mut result = Vec::new();

    fn visit(
        name: &GlobalId,
        call_graph: &FxHashMap<GlobalId, Vec<GlobalId>>,
        name_set: &HashSet<GlobalId>,
        visited: &mut HashSet<GlobalId>,
        result: &mut Vec<GlobalId>,
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

/// Build a HeapShape for Celeste game globals based on frame 28 checkpoint structure.
///
/// This maps global names to their ValueShape:
/// - `room` is a table with `x` and `y` fields (both numbers)
/// - Most other globals are simple leaf values
fn build_celeste_heap_shape(accessed_globals: &[String]) -> crate::interpreter::heap_elimination::HeapShape {
    use crate::interpreter::heap_elimination::{HeapShape, ValueShape};
    use crate::pico8_num::Pico8Num;

    let mut shape = HeapShape::new();

    for global in accessed_globals {
        let value_shape = match global.as_str() {
            // room is a table with x and y fields (room = { x=0, y=0 })
            "room" => {
                let mut fields = FxHashMap::default();
                fields.insert("x".to_string(), ValueShape::Leaf);
                fields.insert("y".to_string(), ValueShape::Leaf);
                ValueShape::Table(fields)
            }
            // Button key constants - these are immutable numbers
            // k_left=0, k_right=1, k_up=2, k_down=3, k_jump=4, k_dash=5
            "k_left" => ValueShape::Constant(Pico8Num::from_i16(0)),
            "k_right" => ValueShape::Constant(Pico8Num::from_i16(1)),
            "k_up" => ValueShape::Constant(Pico8Num::from_i16(2)),
            "k_down" => ValueShape::Constant(Pico8Num::from_i16(3)),
            "k_jump" => ValueShape::Constant(Pico8Num::from_i16(4)),
            "k_dash" => ValueShape::Constant(Pico8Num::from_i16(5)),
            // Most other Celeste globals are simple values (numbers, booleans, etc.)
            // or complex structures we don't need to track
            _ => ValueShape::Leaf,
        };
        shape.globals.insert(global.clone(), value_shape);
    }

    shape
}

/// Run the optimization pipeline with inter-procedural passes (call resolution and inlining).
///
/// This version uses pre-optimized function definitions for inlining, ensuring
/// that inlined code is already optimized.
///
/// Pipeline stages:
/// 1. mem2reg: Promote local cells (non-escaping allocations) to SSA
/// 2. builtin_resolution + call_resolution + inlining: Iteratively resolve and inline calls
/// 3. dce: Dead code elimination
/// 4. block_coalesce: Merge straight-line blocks
/// 5. heap_elimination: Eliminate global heap operations with known shapes
///
/// # Arguments
/// * `ctx` - Inter-procedural context containing closure maps and function definitions
/// * `arg_ids` - LocalIds for function arguments (Some if defined, None if unused/varargs)
/// * `arg_shapes` - Optional shapes for function arguments (e.g., `self` in method calls)
pub fn run_optimization_pipeline_with_interprocedural(
    cfg: &Cfg,
    analysis: &CfgAnalysisResult,
    ctx: &InterproceduralContext<'_>,
    arg_ids: &[Option<LocalId>],
    arg_shapes: &[Option<crate::interpreter::heap_elimination::ValueShape>],
) -> (OptimizationResult, OptimizationCfgs) {
    run_optimization_pipeline_with_interprocedural_inner(cfg, analysis, ctx, arg_ids, arg_shapes, true)
}

/// Version of run_optimization_pipeline_with_interprocedural that allows disabling strict validation.
/// This is useful for the cfg_viewer where we want to continue even if validation fails.
pub fn run_optimization_pipeline_with_interprocedural_lenient(
    cfg: &Cfg,
    analysis: &CfgAnalysisResult,
    ctx: &InterproceduralContext<'_>,
    arg_ids: &[Option<LocalId>],
    arg_shapes: &[Option<crate::interpreter::heap_elimination::ValueShape>],
) -> (OptimizationResult, OptimizationCfgs) {
    run_optimization_pipeline_with_interprocedural_inner(cfg, analysis, ctx, arg_ids, arg_shapes, false)
}

fn run_optimization_pipeline_with_interprocedural_inner(
    cfg: &Cfg,
    analysis: &CfgAnalysisResult,
    ctx: &InterproceduralContext<'_>,
    arg_ids: &[Option<LocalId>],
    arg_shapes: &[Option<crate::interpreter::heap_elimination::ValueShape>],
    strict_validation: bool,
) -> (OptimizationResult, OptimizationCfgs) {
    use crate::interpreter::block_coalesce::{coalesce_blocks, CoalesceResult};
    use crate::interpreter::builtin_resolution::{resolve_builtins, BuiltinResolutionResult};
    use crate::interpreter::cfg_validation::{assert_valid_cfg_with_args, validate_cfg_with_args};
    use crate::interpreter::dce::{eliminate_dead_code, DceResult};
    use crate::interpreter::deopt_unsafe_builtins::{deopt_unsafe_builtins, DeoptUnsafeBuiltinsResult};
    use crate::interpreter::heap_elimination::{eliminate_heap, DeoptMode, HeapEliminationResult};
    use crate::interpreter::inlining::{inline_calls, InliningResult};
    use crate::interpreter::mem2reg::{mem2reg, Mem2RegResult};
    use crate::interpreter::slot_tracker::resolve_calls_via_slots;
    use crate::ir::{LabelGenerator, LocalIdGenerator};

    // Helper to validate and optionally panic
    let validate = |cfg: &Cfg, predefined: &[crate::ir::LocalId], context: &str| {
        if strict_validation {
            assert_valid_cfg_with_args(cfg, predefined, context);
        } else {
            let result = validate_cfg_with_args(cfg, predefined);
            if !result.is_valid() {
                eprintln!("Warning: Validation failed ({}): {} errors", context, result.errors.len());
            }
        }
    };

    // Extract defined arg_ids for validation (filter out None values)
    let defined_arg_ids: Vec<_> = arg_ids.iter().filter_map(|id| *id).collect();

    let mut result = OptimizationResult::default();
    let mut cfgs = OptimizationCfgs::default();
    let mut local_gen = LocalIdGenerator::from_cfg(cfg);
    let mut label_gen = LabelGenerator::new();

    // Record the original CFG as the first step
    cfgs.steps.push(PipelineStep {
        name: "Original".to_string(),
        step_type: PipelineStepType::Original,
        cfg: cfg.clone(),
    });

    // Stage 1: mem2reg - eliminate local cells
    let (after_mem2reg, mem2reg_status) = if analysis.local_only_allocs > 0 {
        match mem2reg(cfg, &mut local_gen) {
            Mem2RegResult::Success { cfg: new_cfg, cells_promoted } => {
                cfgs.steps.push(PipelineStep {
                    name: format!("mem2reg ({} cells)", cells_promoted),
                    step_type: PipelineStepType::Mem2Reg { cells_promoted },
                    cfg: new_cfg.clone(),
                });
                validate(&new_cfg, &defined_arg_ids, "after mem2reg");
                (Some(new_cfg), Mem2RegStatus::Success { cells_promoted })
            }
            Mem2RegResult::NoCells => {
                (None, Mem2RegStatus::NoCells)
            }
            Mem2RegResult::PartialSuccess { cfg: new_cfg, cells_promoted, cells_failed, .. } => {
                cfgs.steps.push(PipelineStep {
                    name: format!("mem2reg ({} cells, {} failed)", cells_promoted, cells_failed),
                    step_type: PipelineStepType::Mem2Reg { cells_promoted },
                    cfg: new_cfg.clone(),
                });
                validate(&new_cfg, &defined_arg_ids, "after mem2reg (partial)");
                (Some(new_cfg), Mem2RegStatus::Partial { cells_promoted, cells_failed })
            }
        }
    } else {
        (None, Mem2RegStatus::NoCells)
    };
    result.mem2reg = mem2reg_status;
    cfgs.after_mem2reg = after_mem2reg.clone();

    // Get the CFG for builtin resolution
    let cfg_for_builtin_res = after_mem2reg.as_ref().unwrap_or(cfg);

    // Stage 1b: builtin resolution - convert GetGlobal+Load+Call to CallBuiltin
    let after_builtin_resolution = if let Some(builtins) = ctx.builtin_set {
        match resolve_builtins(cfg_for_builtin_res, builtins) {
            BuiltinResolutionResult::Success { cfg: new_cfg, calls_resolved } => {
                cfgs.steps.push(PipelineStep {
                    name: format!("builtin resolution ({} calls)", calls_resolved),
                    step_type: PipelineStepType::BuiltinResolution { calls_resolved },
                    cfg: new_cfg.clone(),
                });
                validate(&new_cfg, &defined_arg_ids, "after builtin resolution");
                result.builtin_resolution = BuiltinResolutionStatus::Success { calls_resolved };
                Some(new_cfg)
            }
            BuiltinResolutionResult::NoChange => {
                result.builtin_resolution = BuiltinResolutionStatus::NoChange;
                None
            }
        }
    } else {
        result.builtin_resolution = BuiltinResolutionStatus::Skipped;
        None
    };
    cfgs.after_builtin_resolution = after_builtin_resolution.clone();

    // Get the CFG for the iteration loop
    let cfg_for_loop = cfgs
        .latest_cfg_through(PipelinePass::BuiltinResolution)
        .unwrap_or(cfg);

    // Stage 2: Iterative heap elimination (Stop mode) + inlining
    // In each iteration:
    // 1. Run heap elimination with Stop mode - resolves calls via shape tracking
    // 2. Run builtin resolution (for newly resolved calls)
    // 3. Run inlining
    // Repeat until no progress.
    //
    // This allows heap elimination to resolve calls (Call -> CallResolved) via
    // shape tracking, which can then be inlined by subsequent inlining passes.
    let mut current_cfg = cfg_for_loop.clone();
    let mut total_calls_inlined = 0;
    let mut total_calls_resolved = 0;
    let max_rounds = 10;

    // Determine if we need to run heap elimination in the iteration loop
    let initial_analysis = analyze_cfg(&current_cfg);
    let has_globals_for_loop = !initial_analysis.accessed_globals.is_empty();
    let has_args_for_loop = arg_shapes.iter().any(|s| s.is_some());

    for iteration in 0..max_rounds {
        let mut made_progress = false;

        // Use lightweight slot tracking to resolve calls (no SSA/Phi creation)
        // This can safely run multiple times without creating duplicate definitions
        if has_globals_for_loop || has_args_for_loop {
            let loop_analysis = analyze_cfg(&current_cfg);
            let mut shape = build_celeste_heap_shape(&loop_analysis.accessed_globals);
            shape.args = arg_shapes.to_vec();

            let result = resolve_calls_via_slots(&current_cfg, &shape, arg_ids, ctx.global_closure_map);
            if result.calls_resolved > 0 {
                total_calls_resolved += result.calls_resolved;
                current_cfg = result.cfg;
                cfgs.steps.push(PipelineStep {
                    name: format!("call_resolution iter {} ({} calls resolved)", iteration + 1, result.calls_resolved),
                    step_type: PipelineStepType::HeapElimStop {
                        calls_resolved: result.calls_resolved,
                        iteration: iteration + 1,
                    },
                    cfg: current_cfg.clone(),
                });
                // Slot tracker doesn't create Phi nodes, so validation should pass
                validate(&current_cfg, &defined_arg_ids, &format!("after call_resolution iter {}", iteration + 1));
                made_progress = true;
            }
        }

        // Re-run builtin resolution to pick up any new calls from inlined code
        if let Some(builtins) = ctx.builtin_set {
            current_cfg = match resolve_builtins(&current_cfg, builtins) {
                BuiltinResolutionResult::Success { cfg, .. } => cfg,
                BuiltinResolutionResult::NoChange => current_cfg,
            };
        }

        // Try to inline resolved calls
        match inline_calls(&current_cfg, ctx.optimized_fun_defs, &mut local_gen, &mut label_gen) {
            InliningResult::Success { cfg: new_cfg, calls_inlined } => {
                current_cfg = new_cfg.clone();
                total_calls_inlined += calls_inlined;
                cfgs.steps.push(PipelineStep {
                    name: format!("inlining iter {} ({} calls)", iteration + 1, calls_inlined),
                    step_type: PipelineStepType::Inlining {
                        calls_inlined,
                        iteration: iteration + 1,
                    },
                    cfg: current_cfg.clone(),
                });
                validate(&current_cfg, &defined_arg_ids, &format!("after inlining iter {}", iteration + 1));
                made_progress = true;
            }
            InliningResult::NoChange => {}
        }

        if !made_progress {
            break;
        }
    }

    // Record call resolution results (from heap elimination's shape-tracked resolution)
    if total_calls_resolved > 0 {
        result.call_resolution = CallResolutionStatus::Success { calls_resolved: total_calls_resolved };
    } else {
        result.call_resolution = CallResolutionStatus::NoChange;
    }
    cfgs.after_call_resolution = None; // No longer a separate stage

    let after_inlining = if total_calls_inlined > 0 {
        result.inlining = InliningStatus::Success { calls_inlined: total_calls_inlined };
        Some(current_cfg.clone())
    } else {
        result.inlining = InliningStatus::NoChange;
        None
    };
    cfgs.after_inlining = after_inlining.clone();

    // Stage 3: Dead code elimination
    // After inlining, there may be unused GetGlobal/Load instructions that were only used
    // to set up the original Call (which has been replaced by inlined code)
    let cfg_for_dce = cfgs
        .latest_cfg_through(PipelinePass::Inlining)
        .unwrap_or(cfg);

    let after_dce = match eliminate_dead_code(cfg_for_dce) {
        DceResult::Success { cfg: new_cfg, instructions_removed } => {
            cfgs.steps.push(PipelineStep {
                name: format!("DCE ({} removed)", instructions_removed),
                step_type: PipelineStepType::Dce { instructions_removed },
                cfg: new_cfg.clone(),
            });
            result.dce = DceStatus::Success { instructions_removed };
            validate(&new_cfg, &defined_arg_ids, "after DCE");
            Some(new_cfg)
        }
        DceResult::NoChange => {
            result.dce = DceStatus::NoChange;
            None
        }
    };
    cfgs.after_dce = after_dce.clone();

    // Stage 4: block coalescing - merge straight-line blocks
    let cfg_for_coalesce = cfgs
        .latest_cfg_through(PipelinePass::Dce)
        .unwrap_or(cfg);

    let after_block_coalesce = match coalesce_blocks(cfg_for_coalesce) {
        CoalesceResult::Success { cfg: new_cfg, blocks_removed } => {
            cfgs.steps.push(PipelineStep {
                name: format!("block coalesce ({} removed)", blocks_removed),
                step_type: PipelineStepType::BlockCoalesce { blocks_removed },
                cfg: new_cfg.clone(),
            });
            result.block_coalesce = BlockCoalesceStatus::Success { blocks_removed };
            validate(&new_cfg, &defined_arg_ids, "after block_coalesce");
            Some(new_cfg)
        }
        CoalesceResult::NoChange => {
            result.block_coalesce = BlockCoalesceStatus::NoChange;
            None
        }
    };
    cfgs.after_block_coalesce = after_block_coalesce.clone();

    // Stage 5: deopt unsafe builtins - replace unsafe builtin calls with deopt points
    let cfg_for_deopt = cfgs
        .latest_cfg_through(PipelinePass::BlockCoalesce)
        .unwrap_or(cfg);

    let after_deopt_builtins = match deopt_unsafe_builtins(cfg_for_deopt) {
        DeoptUnsafeBuiltinsResult::Success { cfg: new_cfg, deopts_inserted } => {
            cfgs.steps.push(PipelineStep {
                name: format!("deopt builtins ({} inserted)", deopts_inserted),
                step_type: PipelineStepType::DeoptBuiltins { deopts_inserted },
                cfg: new_cfg.clone(),
            });
            result.deopt_builtins = DeoptBuiltinsStatus::Success { deopts_inserted };
            validate(&new_cfg, &defined_arg_ids, "after deopt_builtins");
            Some(new_cfg)
        }
        DeoptUnsafeBuiltinsResult::NoChange => {
            result.deopt_builtins = DeoptBuiltinsStatus::NoChange;
            None
        }
    };
    cfgs.after_deopt_builtins = after_deopt_builtins.clone();

    // Stage 6: heap elimination - for globals (final pass with deopts)
    let cfg_for_heap_elim = cfgs
        .latest_cfg_through(PipelinePass::DeoptBuiltins)
        .unwrap_or(cfg);

    // Re-analyze the CFG to get current accessed_globals (inlining may have added new ones)
    let current_analysis = analyze_cfg(cfg_for_heap_elim);

    // Build heap shape including both globals and args
    let has_globals = !current_analysis.accessed_globals.is_empty();
    let has_args = arg_shapes.iter().any(|s| s.is_some());

    if has_globals || has_args {
        // Build a heap shape based on Celeste game's actual global structure
        let mut shape = build_celeste_heap_shape(&current_analysis.accessed_globals);
        // Add arg shapes
        shape.args = arg_shapes.to_vec();

        match eliminate_heap(cfg_for_heap_elim, &shape, arg_ids, local_gen, label_gen, DeoptMode::Insert) {
            HeapEliminationResult::Success(transformed) => {
                let slot_mappings: Vec<SlotMapping> = transformed.unpack_slots
                    .iter()
                    .map(|(slot, local_id)| SlotMapping {
                        path: slot.to_string(),
                        local_id: usize::from(*local_id) as u32,
                    })
                    .collect();
                result.heap_elimination = HeapEliminationStatus::Success {
                    unpack_count: transformed.unpack_slots.len(),
                    modified_count: transformed.modified_slots.len(),
                    slot_mappings: slot_mappings.clone(),
                };

                // Record the step
                cfgs.steps.push(PipelineStep {
                    name: format!("heap_elim_final ({} slots)", transformed.unpack_slots.len()),
                    step_type: PipelineStepType::HeapElimFinal {
                        unpack_count: transformed.unpack_slots.len(),
                        modified_count: transformed.modified_slots.len(),
                        slot_mappings,
                    },
                    cfg: transformed.cfg.clone(),
                });
                // Note: We skip validation immediately after heap_elim because:
                // 1. Deopt terminators may leave downstream blocks with undefined references
                // 2. DCE_final will remove unreachable blocks, which may fix some issues
                // 3. We validate after DCE_final instead
                let unpack_ids: Vec<_> = transformed.unpack_slots.iter().map(|(_, id)| *id).collect();
                let all_predefined: Vec<_> = defined_arg_ids.iter().copied()
                    .chain(unpack_ids.iter().copied())
                    .collect();

                // Run DCE again after heap elimination to clean up dead GetGlobal/Load/GetField
                // that were only used by Call instructions that got converted to Deopt
                let final_cfg = match eliminate_dead_code(&transformed.cfg) {
                    DceResult::Success { cfg: dce_cfg, instructions_removed } => {
                        // Record the final DCE step
                        cfgs.steps.push(PipelineStep {
                            name: format!("DCE final ({} removed)", instructions_removed),
                            step_type: PipelineStepType::Dce { instructions_removed },
                            cfg: dce_cfg.clone(),
                        });
                        // Add to the DCE count for stats
                        if let DceStatus::Success { instructions_removed: prev } = &result.dce {
                            result.dce = DceStatus::Success { instructions_removed: prev + instructions_removed };
                        } else {
                            result.dce = DceStatus::Success { instructions_removed };
                        }
                        // DCE removes unreachable blocks, so we need to clean up Phi nodes
                        // that reference those removed blocks or locals defined in removed blocks.
                        // We do both cleanups in a single pass by providing the defined_locals set.
                        let defined_locals = crate::interpreter::phi_cleanup::collect_defined_locals(&dce_cfg, &all_predefined);
                        let cleanup_result = crate::interpreter::phi_cleanup::cleanup_phis_with_defined_locals(&dce_cfg, Some(&defined_locals));
                        let cleaned_cfg = cleanup_result.cfg;

                        validate(&cleaned_cfg, &all_predefined, "after DCE_final");
                        cleaned_cfg
                    }
                    DceResult::NoChange => transformed.cfg.clone(),
                };

                // Store the final CFG (after DCE) for the viewer
                cfgs.after_heap_elim = Some(final_cfg);
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
            HeapEliminationResult::ConstantViolation(reason) => {
                result.heap_elimination = HeapEliminationStatus::Failed(format!("Constant violation: {}", reason));
            }
        }
    } else {
        result.heap_elimination = HeapEliminationStatus::NotApplicable;
    }

    (result, cfgs)
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
                instruction_text: instr.format(),
                instruction_type: instr.type_name().to_string(),
            })
            .collect();

        let (term_id, term) = &block.terminator;
        let terminator = SerializableTerminator {
            target_id: usize::from(*term_id),
            terminator_text: term.format(),
        };

        SerializableBlock {
            instructions,
            terminator,
            hint_normalize: block.hint_normalize,
        }
    }
}

/// A serializable pipeline step for the CFG viewer
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SerializablePipelineStep {
    /// Human-readable name for this step
    pub name: String,
    /// Type of optimization that was applied
    pub step_type: PipelineStepType,
    /// The CFG after this step
    pub cfg: SerializableCfg,
}

/// Convert an internal PipelineStep to a serializable version
pub fn serialize_pipeline_step(step: &PipelineStep) -> SerializablePipelineStep {
    SerializablePipelineStep {
        name: step.name.clone(),
        step_type: step.step_type.clone(),
        cfg: (&step.cfg).into(),
    }
}

/// A test case for the CFG analyzer viewer.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CfgTestCase {
    /// Name of the function
    pub name: String,
    /// All pipeline steps in order (new format)
    #[serde(default)]
    pub pipeline_steps: Vec<SerializablePipelineStep>,
    /// The original CFG (serializable form) - legacy, kept for backward compatibility
    pub original_cfg: SerializableCfg,
    /// CFG after mem2reg pass (if any transformation occurred) - legacy
    pub after_mem2reg: Option<SerializableCfg>,
    /// CFG after heap elimination pass (if any transformation occurred) - legacy
    pub after_heap_elim: Option<SerializableCfg>,
    /// CFG after block coalescing pass (if any transformation occurred) - legacy
    pub after_block_coalesce: Option<SerializableCfg>,
    /// CFG after builtin resolution pass (if any transformation occurred) - legacy
    pub after_builtin_resolution: Option<SerializableCfg>,
    /// CFG after call resolution pass (if any transformation occurred) - legacy
    pub after_call_resolution: Option<SerializableCfg>,
    /// CFG after inlining pass (if any transformation occurred) - legacy
    pub after_inlining: Option<SerializableCfg>,
    /// CFG after dead code elimination pass (if any transformation occurred) - legacy
    pub after_dce: Option<SerializableCfg>,
    /// CFG after deopt unsafe builtins pass (if any transformation occurred) - legacy
    pub after_deopt_builtins: Option<SerializableCfg>,
    /// Final optimized CFG (whichever stage succeeded last) - legacy
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

impl Default for CfgTestCases {
    fn default() -> Self {
        Self::new()
    }
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

    fn make_simple_cfg() -> Cfg {
        // A simple CFG: %0 = NumberConstant(5); return %0
        let entry = Block::new_for_test(
            vec![(
                LocalId::from(0),
                Instruction::num_const(5),
            )],
            (
                LocalId::from(1),
                Terminator::Return {
                    value: Some(LocalId::from(0)),
                },
            ),
        );

        Cfg::single_entry(entry)
    }

    fn make_heap_read_cfg() -> Cfg {
        // A CFG that reads from heap: %0 = GetGlobal("x"); %1 = Load(%0); return %1
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("x")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
            ],
            (LocalId::from(2), Terminator::ret(Some(LocalId::from(1)))),
        );

        Cfg::single_entry(entry)
    }

    fn make_heap_write_cfg() -> Cfg {
        // A CFG that writes to heap: %0 = GetGlobal("x"); %1 = NumberConstant(5); Store(%0, %1); return
        let entry = Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("x")),
                (LocalId::from(1), Instruction::num_const(5)),
                (LocalId::from(2), Instruction::store(LocalId::from(0), LocalId::from(1))),
            ],
            (LocalId::from(3), Terminator::ret(None)),
        );

        Cfg::single_entry(entry)
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
        let inner_cfg = Cfg::single_entry(Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::num_const(1)),
                (LocalId::from(1), Instruction::num_const(1)),
                (
                    LocalId::from(2),
                    Instruction::binary_op(
                        BinaryOp::Plus,
                        LocalId::from(0),
                        LocalId::from(1),
                    ),
                ),
            ],
            (LocalId::from(3), Terminator::Return { value: Some(LocalId::from(2)) }),
        ));

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
        let outer_cfg = Cfg::single_entry(Block::new_for_test(
            vec![
                (LocalId::from(0), Instruction::get_global("inner")),
                (LocalId::from(1), Instruction::load(LocalId::from(0))),
                (LocalId::from(2), Instruction::call(LocalId::from(1), vec![])),
            ],
            (LocalId::from(3), Terminator::ret(Some(LocalId::from(2)))),
        ));

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

    #[test]
    fn test_builtin_resolution_in_pipeline() {
        // Test that run_optimization_pipeline_with_interprocedural properly resolves
        // builtin calls even for standalone functions (not just when inlined)

        use crate::interpreter::builtin_resolution::BuiltinSet;

        // Create a function that calls a builtin (max)
        // fn test_func(a, b) { return max(a, b) }
        //
        // This requires:
        // 1. GetGlobal(max) -> cell
        // 2. Load(cell) -> func
        // 3. Call(closure, [a, b]) where closure was created from Load
        //
        // We use arg IDs 0 and 1 as the function arguments

        let a_id = LocalId::from(0);
        let b_id = LocalId::from(1);
        let cell_id = LocalId::from(2);
        let func_id = LocalId::from(3);
        let result_id = LocalId::from(4);
        let terminator_id = LocalId::from(5);

        // Entry block:
        // cell = GetGlobal(max)
        // func = Load(cell)
        // result = Call(func, [a, b])
        // Return(result)
        let entry = Block::new_for_test(
            vec![
                (cell_id, Instruction::get_global("max")),
                (func_id, Instruction::load(cell_id)),
                (result_id, Instruction::call(func_id, vec![a_id, b_id])),
            ],
            (terminator_id, Terminator::ret(Some(result_id))),
        );

        let cfg = Cfg::single_entry(entry);

        // Analyze and run the pipeline with builtin set
        let analysis = analyze_cfg(&cfg);

        // Create empty maps (no user-defined functions to resolve)
        let global_closure_map = FxHashMap::default();
        let optimized_fun_defs: FxHashMap<crate::ir::GlobalId, crate::ir::FunDef> = FxHashMap::default();

        // Create builtin set with "max"
        let builtin_set: BuiltinSet = ["max"].iter().map(|s| s.to_string()).collect();

        // Create interprocedural context
        let ctx = InterproceduralContext {
            global_closure_map: &global_closure_map,
            optimized_fun_defs: &optimized_fun_defs,
            builtin_set: Some(&builtin_set),
        };

        // Run the pipeline with arg_ids for a and b
        let (result, cfgs) = run_optimization_pipeline_with_interprocedural(
            &cfg,
            &analysis,
            &ctx,
            &[Some(a_id), Some(b_id)],  // arg_ids for a and b
            &[],  // no arg_shapes for this test
        );

        // Verify builtin resolution succeeded
        assert!(
            matches!(result.builtin_resolution, BuiltinResolutionStatus::Success { calls_resolved: 1 }),
            "Expected builtin resolution to resolve 1 call, got {:?}",
            result.builtin_resolution
        );

        // Verify the intermediate CFG was recorded
        assert!(
            cfgs.after_builtin_resolution.is_some(),
            "Expected after_builtin_resolution CFG to be recorded"
        );

        // Verify the CFG now contains CallBuiltin instead of Call
        let final_cfg = cfgs.after_builtin_resolution.as_ref().unwrap();
        let has_call_builtin = final_cfg.iter_blocks().any(|block| {
            block.instructions.iter().any(|(_, instr)| {
                matches!(instr, Instruction::CallBuiltin { name, .. } if name == "max")
            })
        });
        assert!(has_call_builtin, "Expected CallBuiltin(max, ...) in the resolved CFG");

        // Verify no regular Call remains
        let has_regular_call = final_cfg.iter_blocks().any(|block| {
            block.instructions.iter().any(|(_, instr)| {
                matches!(instr, Instruction::Call { .. })
            })
        });
        assert!(!has_regular_call, "Expected no regular Call in the resolved CFG");
    }
}
