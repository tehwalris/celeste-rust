use std::hash::BuildHasherDefault;
use std::sync::Arc;

use indexmap::IndexSet;
use rustc_hash::FxHasher;

use celeste_ir::ir::{Cfg, FunDef, GlobalId, Label};

use super::{state::State, value::Value};

// Use FxHashMap for faster hashing in FixedEnv
type FxHashMap<K, V> = std::collections::HashMap<K, V, BuildHasherDefault<FxHasher>>;

/// A builtin function takes a state and argument values, returns multiple possible (state, return_value) pairs.
/// Multiple pairs are returned when the function can branch (e.g., on UnknownBool).
pub type BuiltinFun = Arc<dyn Fn(State, Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> + Send + Sync>;

/// A builtin that is a function of its arguments alone: it cannot read or write
/// the heap, and it always yields exactly one result rather than branching.
pub type PureBuiltinFun = Arc<dyn Fn(&[Value]) -> anyhow::Result<Value> + Send + Sync>;

/// The builtins that `Instruction::CallBuiltin` may name.
///
/// One list, used by two things that must not drift apart: `add_pure_builtin`
/// refuses to register anything not named here, and
/// `rules::if_convert::is_speculatable` decides from it whether a `CallBuiltin`
/// can be hoisted out of a branch arm. Adding a name here is a claim that the
/// implementation takes `State` and gives it back untouched, returns a single
/// result, and reads nothing outside its arguments. Data captured at
/// construction counts as part of the implementation, not as a read - which is
/// how `tile_flag_at` qualifies: the cart and the room-(1, 0) collision cache
/// it closes over are immutable for the lifetime of the environment (see
/// `game_runner::make_builtin_tile_flag_at` for the room caveat). `mget`
/// qualifies the same way: it reads only the cart's map data, and the game
/// never calls `mset` (grep the Lua), so the map is immutable too.
pub const PURE_BUILTINS: &[&str] =
    &["min", "max", "abs", "flr", "sin", "tile_flag_at", "mget"];

pub fn is_pure_builtin(name: &str) -> bool {
    PURE_BUILTINS.contains(&name)
}

/// Builtins whose one effect is *refinement*: one state in, one or more
/// fragments out, whose union represents exactly the input state, with the
/// result value concretized per fragment (`__split_by_flr` splits by floor
/// bucket, `__split_at` three-ways around a constant). They read nothing
/// outside their arguments and write nothing.
///
/// NOT pure in `PURE_BUILTINS`' sense - purity there means "exactly one
/// result, state untouched", and multiplying states is the whole job - so
/// they cannot be pinned to `CallBuiltin`, whose interpreter channel is
/// single-value. They stay plain `Call`s, and the claim this list makes is
/// different: running one *speculatively* (on states that would have
/// skipped it) is sound, because a refinement is sound on any state - the
/// fragments jointly stand for the same concrete states, only the state
/// count changes. That is the same cost-not-correctness bargain `expand`
/// speculation makes, and the bench answers for it.
///
/// `speculate_region`'s `splits` opt-in leans on this list: it accepts a
/// region-internal `Call` only when the callee is provably
/// `load(get_global(name))` with `name` here. A rebound global would
/// miscall equally with or without speculation - the conversion adds no
/// new failure mode on that axis.
pub const REFINEMENT_BUILTINS: &[&str] = &["__split_by_flr", "__split_at"];

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
    /// The subset of `builtin_funs` that `CallBuiltin` can invoke directly.
    /// The same function is behind both entries, so a `call` and a pinned
    /// `call_builtin` of the same name cannot compute different things.
    pub pure_builtin_funs: FxHashMap<String, PureBuiltinFun>,
}

impl FixedEnv {
    pub fn new() -> Self {
        Self {
            fun_defs: FxHashMap::default(),
            builtin_funs: FxHashMap::default(),
            pure_builtin_funs: FxHashMap::default(),
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

    /// Registers a builtin under both signatures, from one implementation.
    ///
    /// A normal `call` goes through the wrapper and behaves exactly as before;
    /// a pinned `call_builtin` goes straight to `f`. Deriving one from the other
    /// is the point - two hand-written copies of `max` that disagreed on an edge
    /// case would be a rewrite that silently changes the program.
    pub fn add_pure_builtin<F>(&mut self, name: &str, f: F)
    where
        F: Fn(&[Value]) -> anyhow::Result<Value> + Send + Sync + 'static,
    {
        assert!(
            is_pure_builtin(name),
            "{} is not in PURE_BUILTINS, so CallBuiltin must not be able to name it",
            name
        );
        let f: PureBuiltinFun = Arc::new(f);
        let wrapped = f.clone();
        self.builtin_funs.insert(
            name.to_string(),
            Arc::new(move |state, args| Ok(vec![(state, wrapped(&args)?)])),
        );
        self.pure_builtin_funs.insert(name.to_string(), f);
    }
}
