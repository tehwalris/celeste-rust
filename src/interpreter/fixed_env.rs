use std::sync::Arc;

use indexmap::IndexSet;
use rustc_hash::FxHashMap;

use crate::ir::{Cfg, FunDef, GlobalId, Label};

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

/// FixedEnv contains the static environment for interpretation:
/// - Function definitions (with their prepared CFGs)
/// - Builtin function implementations
pub struct FixedEnv {
    pub fun_defs: FxHashMap<GlobalId, (FunDef, PreparedCfg)>,
    pub builtin_funs: FxHashMap<String, BuiltinFun>,
}

impl FixedEnv {
    pub fn new() -> Self {
        Self {
            fun_defs: FxHashMap::default(),
            builtin_funs: FxHashMap::default(),
        }
    }

    pub fn add_fun_def(&mut self, fun_def: FunDef) {
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
