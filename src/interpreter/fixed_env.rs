use std::sync::Arc;

use indexmap::IndexSet;

use crate::ir::{Cfg, FunDef, GlobalId, Label, LocalId, LocalIdGenerator};

use super::block_coalesce::{coalesce_blocks, CoalesceResult};
use super::builtin_resolution::BuiltinSet;
use super::call_resolution::build_global_closure_map_from_fun_defs;
use super::cfg_analysis::optimize_all_functions;
use super::cfg_validation::assert_valid_cfg_with_args;
use super::common::FxHashMap;
use super::mem2reg::{mem2reg, Mem2RegResult};
use super::{state::State, value::Value};

/// A builtin function takes a state and argument values, returns multiple possible (state, return_value) pairs.
/// Multiple pairs are returned when the function can branch (e.g., on UnknownBool).
pub type BuiltinFun = Arc<dyn Fn(State, Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> + Send + Sync>;

/// PreparedCfg holds a CFG along with precomputed analysis data.
/// This caches the label set to avoid recomputing it on every interpret_cfg call.
#[derive(Clone)]
pub struct PreparedCfg {
    pub cfg: Cfg,
    pub labels: IndexSet<Label>,
}

impl PreparedCfg {
    pub fn new(cfg: Cfg) -> Self {
        // Pre-compute the label set (same logic as in flow_graph_of_cfg)
        let mut labels = IndexSet::new();
        for name in cfg.named.keys() {
            labels.insert_full(name.clone());
        }
        Self { cfg, labels }
    }
}

/// Apply shape-independent optimizations to a CFG.
/// These passes don't depend on the shape of the heap and can always be applied.
///
/// Passes applied:
/// 1. mem2reg - Promote local cells (non-escaping allocations) to SSA
/// 2. block_coalesce - Merge straight-line blocks
///
/// The `arg_ids` parameter is used for validation - these are the function's
/// argument local IDs which are defined externally.
pub fn optimize_cfg(cfg: &Cfg, arg_ids: &[LocalId]) -> Cfg {
    let mut current_cfg = cfg.clone();
    let mut local_gen = LocalIdGenerator::new();

    // Pass 1: mem2reg - eliminate local cells
    current_cfg = match mem2reg(&current_cfg, &mut local_gen) {
        Mem2RegResult::Success { cfg: optimized, .. } => optimized,
        Mem2RegResult::PartialSuccess { cfg: optimized, .. } => optimized,
        Mem2RegResult::NoCells => current_cfg,
    };
    assert_valid_cfg_with_args(&current_cfg, arg_ids, "optimize_cfg: after mem2reg");

    // Pass 2: block coalescing - merge straight-line blocks
    current_cfg = match coalesce_blocks(&current_cfg) {
        CoalesceResult::Success { cfg: optimized, .. } => optimized,
        CoalesceResult::NoChange => current_cfg,
    };
    assert_valid_cfg_with_args(&current_cfg, arg_ids, "optimize_cfg: after block_coalesce");

    current_cfg
}

/// FixedEnv contains the static environment for interpretation:
/// - Function definitions (with their prepared CFGs)
/// - Builtin function implementations
pub struct FixedEnv {
    pub fun_defs: FxHashMap<GlobalId, (FunDef, PreparedCfg)>,
    pub builtin_funs: FxHashMap<String, BuiltinFun>,
    /// Whether to apply shape-independent optimizations to CFGs
    optimize_cfgs: bool,
}

impl FixedEnv {
    pub fn new() -> Self {
        Self {
            fun_defs: FxHashMap::default(),
            builtin_funs: FxHashMap::default(),
            optimize_cfgs: false,
        }
    }

    /// Create a new FixedEnv with shape-independent CFG optimizations enabled.
    pub fn new_with_optimizations() -> Self {
        Self {
            fun_defs: FxHashMap::default(),
            builtin_funs: FxHashMap::default(),
            optimize_cfgs: true,
        }
    }

    /// Enable or disable shape-independent CFG optimizations.
    pub fn set_optimize_cfgs(&mut self, optimize: bool) {
        self.optimize_cfgs = optimize;
    }

    pub fn add_fun_def(&mut self, mut fun_def: FunDef) {
        // Apply shape-independent optimizations if enabled
        if self.optimize_cfgs {
            // Collect external IDs for validation (capture_ids + arg_ids are defined externally)
            let mut external_ids: Vec<LocalId> = fun_def.capture_ids.clone();
            external_ids.extend(fun_def.arg_ids.iter().filter_map(|opt| *opt));
            fun_def.cfg = optimize_cfg(&fun_def.cfg, &external_ids);
        }

        let prepared = PreparedCfg::new(fun_def.cfg.clone());
        self.fun_defs
            .insert(fun_def.name.clone(), (fun_def, prepared));
    }

    pub fn add_builtin<F>(&mut self, name: &str, f: F)
    where
        F: Fn(State, Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> + Send + Sync + 'static,
    {
        self.builtin_funs.insert(name.to_string(), Arc::new(f));
    }

    /// Apply inter-procedural optimizations after all function definitions are loaded.
    ///
    /// This runs call resolution and inlining passes which require knowledge of all
    /// function definitions to work properly. Functions are processed in topological
    /// order (callees before callers) to ensure that when we inline a function, we
    /// use its already-optimized version.
    ///
    /// Call this after all `add_fun_def` calls are complete.
    pub fn apply_interprocedural_optimizations(&mut self) {
        // Build the global closure map from all function definitions
        let global_closure_map = build_global_closure_map_from_fun_defs(
            self.fun_defs.values().map(|(fun_def, _)| fun_def),
        );

        // Build the builtin set from registered builtin names
        let builtin_set: BuiltinSet = self.builtin_funs.keys().cloned().collect();

        // Collect all function definitions
        let fun_def_refs: Vec<&FunDef> = self
            .fun_defs
            .values()
            .map(|(fun_def, _)| fun_def)
            .collect();

        // Optimize all functions in dependency order
        // This ensures callees are optimized before they are inlined into callers
        let optimized = optimize_all_functions(&fun_def_refs, &global_closure_map, Some(&builtin_set));

        // Update all function definitions with their optimized versions
        for (name, (fun_def, _)) in self.fun_defs.iter_mut() {
            if let Some(optimized_fun) = optimized.get(name) {
                fun_def.cfg = optimized_fun.cfg.clone();
            }
        }

        // Re-prepare all CFGs (update the cached label sets)
        let names: Vec<GlobalId> = self.fun_defs.keys().cloned().collect();
        for name in names {
            let (fun_def, _) = self.fun_defs.remove(&name).unwrap();
            let prepared = PreparedCfg::new(fun_def.cfg.clone());
            self.fun_defs.insert(name, (fun_def, prepared));
        }
    }
}
