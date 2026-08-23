//! The unit that rewrites operate on.
//!
//! A `Program` is everything needed to run the game: the toplevel chunk (which
//! defines all the functions and calls `_init()`), the per-frame chunk, and
//! every `FunDef` the frontend produced. It is always *derived* - built from the
//! Lua sources, then transformed by an ordered list of rewrite instructions.
//! Nothing edits a `Program` by hand.
//!
//! The toplevel and frame chunks are stored as `FunDef`s under reserved names
//! so that rewrites can address them exactly like any other function.

use anyhow::{anyhow, Result};
use indexmap::IndexMap;

use crate::game_runner::create_fixed_env_with_game_builtins;
use crate::interpreter::fixed_env::FixedEnv;
use crate::ir::{Cfg, FunDef, GlobalId};

/// The toplevel chunk: defines every function, then runs `_init()`.
pub const INIT_FN: &str = "__init";
/// The per-frame chunk: `_update()`, `_draw()`, `__reset_button_states()`.
pub const FRAME_FN: &str = "__frame";

#[derive(Clone)]
pub struct Program {
    /// Insertion-ordered so that printing and hashing are deterministic.
    pub functions: IndexMap<GlobalId, FunDef>,
    /// Field-path patterns of the merge-partition cells (see the
    /// `partition_merge` rule): the interpreter's merges group by the
    /// values of these cells in addition to shape, so branches on them
    /// route instead of splitting. Empty = unpartitioned.
    pub merge_partition_cells: Vec<String>,
}

/// The Lua that gets compiled into a `Program`. Kept here rather than in the
/// binaries so every tool agrees on what "the program" is.
pub struct Sources {
    pub builtin_level_3: String,
    pub builtin_level_4: String,
    pub game: String,
}

impl Sources {
    pub fn load_from_disk() -> Result<Self> {
        Ok(Self {
            builtin_level_3: std::fs::read_to_string("lua/builtin_level_3.lua")?,
            builtin_level_4: std::fs::read_to_string("lua/builtin_level_4.lua")?,
            game: crate::game_runner::apply_start_room(&std::fs::read_to_string(
                "lua/celeste-minimal.lua",
            )?)?,
        })
    }
}

const INIT_SUFFIX: &str = "\n_init()\n__reset_button_states()\n";
const FRAME_CODE: &str = "\n_update()\n_draw()\n__reset_button_states()\n";

impl Sources {
    /// The exact toplevel chunk text `compile` parses. Exposed for tools
    /// that save the compiled source next to their output (the profiler's
    /// source mapping).
    pub fn init_chunk_text(&self) -> String {
        format!(
            "{}\n{}\n{}\n{}",
            self.builtin_level_3, self.builtin_level_4, self.game, INIT_SUFFIX
        )
    }
}

impl Program {
    /// Compile the Lua sources into the starting program (before any rewrites).
    pub fn compile(sources: &Sources) -> Result<Self> {
        let full = sources.init_chunk_text();
        let ast = full_moon::parse(&full).map_err(|e| anyhow!("parse game: {:?}", e))?;
        let (init_cfg, fun_defs) = crate::frontend::compile(&ast)?;

        let frame_ast =
            full_moon::parse(FRAME_CODE).map_err(|e| anyhow!("parse frame: {:?}", e))?;
        let (frame_cfg, frame_fun_defs) = crate::frontend::compile(&frame_ast)?;
        if !frame_fun_defs.is_empty() {
            return Err(anyhow!("frame chunk unexpectedly defines functions"));
        }

        let mut functions = IndexMap::new();
        functions.insert(GlobalId::from(INIT_FN.to_string()), synthetic(INIT_FN, init_cfg));
        functions.insert(
            GlobalId::from(FRAME_FN.to_string()),
            synthetic(FRAME_FN, frame_cfg),
        );
        for fun_def in fun_defs {
            functions.insert(fun_def.name.clone(), fun_def);
        }
        Ok(Self { functions, merge_partition_cells: Vec::new() })
    }

    /// Pin every call to a state-zero-native builtin (Call -> CallBuiltin
    /// with a runtime callee assertion; see `pin_builtin::pin_all`).
    ///
    /// For EXECUTED plain programs only - the deopt target, sweep replays,
    /// the concrete walks, the runner, verify's baseline side. The recipe
    /// must NOT build on a pinned base: its whole-program passes (cse
    /// forward, dce) would eliminate different instructions than they did
    /// when the recipe was derived and every later id reference would
    /// drift, so the recipe carries its own pin entries at the positions
    /// its derivation needs them. `tile_flag_at` is deliberately absent
    /// from the list (native only after init); its pins stay in the
    /// recipe.
    pub fn pin_native_builtins(&mut self) -> Result<usize> {
        /// The pure builtins that have a native implementation. NOT
        /// `tile_flag_at`, which is native only after init - pinning it
        /// program-wide would break the init-time calls.
        const NATIVE: [&str; 6] = ["min", "max", "abs", "flr", "sin", "mget"];

        // Re-homed from `rules::pin_builtin::pin_all` when the rules were
        // deleted (`plans/deletion.md`). This is the only part of that
        // rule anything outside the campaign needed: the plain executable
        // program is compiled with these pinned, so the interpreter takes
        // the native path for them.
        let mut pinned = 0usize;
        for (_, fun) in self.functions.iter_mut() {
            // Which locals hold a LOADED pure builtin. A `get_global`'s
            // cell is trusted only within its block; the loaded value
            // anywhere in the function.
            let mut builtin_of: std::collections::HashMap<crate::ir::LocalId, String> =
                Default::default();
            for block in fun.cfg.iter_blocks() {
                let mut cell_of: std::collections::HashMap<crate::ir::LocalId, String> =
                    Default::default();
                for (id, instr) in &block.instructions {
                    match instr {
                        crate::ir::Instruction::GetGlobal { name, create_if_missing: false }
                            if NATIVE.contains(&name.as_str()) =>
                        {
                            cell_of.insert(*id, name.clone());
                        }
                        crate::ir::Instruction::Load { source } => {
                            if let Some(name) = cell_of.get(source) {
                                builtin_of.insert(*id, name.clone());
                            }
                        }
                        _ => {}
                    }
                }
            }
            if builtin_of.is_empty() {
                continue;
            }
            let pin_block = |block: &mut crate::ir::Block, pinned: &mut usize| {
                for (_, instr) in block.instructions.iter_mut() {
                    let crate::ir::Instruction::Call { closure, args } = instr else {
                        continue;
                    };
                    let Some(name) = builtin_of.get(closure) else {
                        continue;
                    };
                    *instr = crate::ir::Instruction::CallBuiltin {
                        callee: *closure,
                        name: name.clone(),
                        args: args.clone(),
                    };
                    *pinned += 1;
                }
            };
            pin_block(&mut fun.cfg.entry, &mut pinned);
            for block in fun.cfg.named.values_mut() {
                pin_block(block, &mut pinned);
            }
        }
        Ok(pinned)
    }

    pub fn compile_from_disk() -> Result<Self> {
        Self::compile(&Sources::load_from_disk()?)
    }

    /// `compile_from_disk` + `pin_native_builtins`: the plain program as
    /// every EXECUTION path runs it. Recipe replays must keep using
    /// `compile_from_disk` (see `pin_native_builtins` for why).
    pub fn compile_executable_from_disk() -> Result<Self> {
        let mut program = Self::compile_from_disk()?;
        program.pin_native_builtins()?;
        Ok(program)
    }

    pub fn get(&self, name: &str) -> Result<&FunDef> {
        self.functions
            .get(&GlobalId::from(name.to_string()))
            .ok_or_else(|| anyhow!("no such function: {}", name))
    }

    pub fn get_mut(&mut self, name: &str) -> Result<&mut FunDef> {
        self.functions
            .get_mut(&GlobalId::from(name.to_string()))
            .ok_or_else(|| anyhow!("no such function: {}", name))
    }

    /// Functions in deterministic order, excluding the two synthetic chunks.
    pub fn real_functions(&self) -> impl Iterator<Item = (&GlobalId, &FunDef)> {
        self.functions
            .iter()
            .filter(|(name, _)| !is_synthetic(name.as_str()))
    }

    pub fn init_cfg(&self) -> &Cfg {
        &self.get(INIT_FN).expect("init chunk always present").cfg
    }

    pub fn frame_cfg(&self) -> &Cfg {
        &self.get(FRAME_FN).expect("frame chunk always present").cfg
    }

    /// Build the interpreter environment. The synthetic chunks are *not*
    /// registered as callable functions - they are entry points, driven directly.
    pub fn fixed_env(&self) -> FixedEnv {
        let mut env = create_fixed_env_with_game_builtins();
        for (_, fun_def) in self.real_functions() {
            env.add_fun_def(fun_def.clone());
        }
        env
    }

    /// Total instruction count, for progress tracking.
    pub fn instruction_count(&self) -> usize {
        self.functions
            .values()
            .map(|f| f.cfg.iter_blocks().map(|b| b.instructions.len() + 1).sum::<usize>())
            .sum()
    }

    pub fn block_count(&self) -> usize {
        self.functions
            .values()
            .map(|f| f.cfg.iter_blocks().count())
            .sum()
    }
}

pub fn is_synthetic(name: &str) -> bool {
    name == INIT_FN || name == FRAME_FN
}

fn synthetic(name: &str, cfg: Cfg) -> FunDef {
    FunDef {
        name: GlobalId::from(name.to_string()),
        capture_ids: vec![],
        arg_ids: vec![],
        cfg,
        source_span: None,
    }
}
