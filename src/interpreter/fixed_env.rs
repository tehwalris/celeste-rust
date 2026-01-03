use std::hash::BuildHasherDefault;
use std::sync::Arc;

use indexmap::IndexSet;
use rustc_hash::FxHasher;

use crate::ir::{Cfg, FunDef, GlobalId, Label, LocalIdGenerator};

use super::block_coalesce::{coalesce_blocks, CoalesceResult};
use super::mem2reg::{mem2reg, Mem2RegResult};
use super::{state::State, value::Value};

// Use FxHashMap for faster hashing in FixedEnv
type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

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
pub fn optimize_cfg(cfg: &Cfg) -> Cfg {
    let mut current_cfg = cfg.clone();
    let mut local_gen = LocalIdGenerator::new();

    // Pass 1: mem2reg - eliminate local cells
    current_cfg = match mem2reg(&current_cfg, &mut local_gen) {
        Mem2RegResult::Success { cfg: optimized, .. } => optimized,
        Mem2RegResult::PartialSuccess { cfg: optimized, .. } => optimized,
        Mem2RegResult::NoCells => current_cfg,
    };

    // Pass 2: block coalescing - merge straight-line blocks
    current_cfg = match coalesce_blocks(&current_cfg) {
        CoalesceResult::Success { cfg: optimized, .. } => optimized,
        CoalesceResult::NoChange => current_cfg,
    };

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
            fun_def.cfg = optimize_cfg(&fun_def.cfg);
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
}
