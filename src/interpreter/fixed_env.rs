use std::collections::HashMap;
use std::sync::Arc;

use indexmap::IndexSet;

use crate::ir::{Cfg, FunDef, GlobalId, Label};

use super::{state::State, value::Value};

/// A builtin function takes a state and argument values, returns multiple possible (state, return_value) pairs.
/// Multiple pairs are returned when the function can branch (e.g., on UnknownBool).
pub type BuiltinFun = Arc<dyn Fn(State, Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> + Send + Sync>;

/// PreparedCfg holds a CFG along with precomputed analysis data.
/// This caches the label set to avoid recomputing it on every interpret_cfg call.
/// The CFG is wrapped in Arc to enable cheap cloning during function calls.
#[derive(Clone)]
pub struct PreparedCfg {
    pub cfg: Arc<Cfg>,
    pub labels: IndexSet<Label>,
}

impl PreparedCfg {
    pub fn new(cfg: Cfg) -> Self {
        // Pre-compute the label set (same logic as in flow_graph_of_cfg)
        let mut labels = IndexSet::new();
        for name in cfg.named.keys() {
            labels.insert_full(name.clone());
        }
        Self { cfg: Arc::new(cfg), labels }
    }
}

/// FixedEnv contains the static environment for interpretation:
/// - Function definitions (with their prepared CFGs)
/// - Builtin function implementations
pub struct FixedEnv {
    pub fun_defs: HashMap<GlobalId, (FunDef, PreparedCfg)>,
    pub builtin_funs: HashMap<String, BuiltinFun>,
}

impl FixedEnv {
    pub fn new() -> Self {
        Self {
            fun_defs: HashMap::new(),
            builtin_funs: HashMap::new(),
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
