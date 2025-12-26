use std::collections::HashMap;
use std::sync::Arc;

use crate::ir::{Cfg, FunDef, GlobalId};

use super::{state::State, value::Value};

/// A builtin function takes a state and argument values, returns multiple possible (state, return_value) pairs.
/// Multiple pairs are returned when the function can branch (e.g., on UnknownBool).
pub type BuiltinFun = Arc<dyn Fn(State, Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> + Send + Sync>;

/// PreparedCfg holds a CFG along with any precomputed analysis data.
/// In OCaml this holds the `analyze` function, but in Rust we'll compute it on demand.
#[derive(Clone)]
pub struct PreparedCfg {
    pub cfg: Cfg,
}

impl PreparedCfg {
    pub fn new(cfg: Cfg) -> Self {
        Self { cfg }
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
