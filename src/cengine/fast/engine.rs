//! `FastEngine`: the fast interpreter packaged like `RefEngine` - one
//! multi-lane boundary `State` in, every frame successor out.

use std::sync::Arc;

use anyhow::{anyhow, bail, Result};
use rustc_hash::FxHashSet;
use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;

use crate::cengine::refdomain::Cursor;
use crate::interpreter::state::State as OState;

use super::bridge::Bridge;
use super::exec::Exec;
use super::heap::{Heap, Value};
use super::program::{Builtin, Program, Sym};

pub struct FastEngine {
    exec: Exec,
    bridge: Bridge,
    /// The post-`_init` heap: the frame-0 state, and the source of the
    /// builtin globals a bridged state is missing.
    base: Heap,
    sym_reset: Sym,
    sym_update: Sym,
    sym_draw: Sym,
    /// SPIKE INSTRUMENTATION: seconds spent inside `run_frame_all` (the
    /// interpreter proper, excluding the bridge).
    pub t_exec: f64,
    /// SPIKE INSTRUMENTATION: paths run so far.
    pub paths: u64,
}

impl FastEngine {
    /// Parse and lower the cart, run its toplevel and `_init`, and keep the
    /// result as the base heap.
    pub fn new() -> Result<Self> {
        let src = crate::cengine::cart::sources()?;
        let ast = full_moon::parse(&src)?;
        let prog: &'static Program = Box::leak(Box::new(super::lower::lower(&ast)?));

        let cart = Arc::new(CartData::load("cart")?);
        let (rx, ry) = crate::game_runner::start_room();
        let cache = Arc::new(CollisionCache::new(&cart, rx, ry)?);

        let mut exec = Exec::new(prog, cart, cache)?;
        let sym = |n: &str| prog.interner.get(n).ok_or_else(|| anyhow!("program never names {:?}", n));
        for name in crate::cengine::cart::NATIVE {
            let b = Builtin::from_name(name).ok_or_else(|| anyhow!("NATIVE builtin {:?} has no enum case", name))?;
            exec.heap.set_global(sym(name)?, Value::Builtin(b));
        }
        exec.call_func(prog.top)?;
        if !exec.ok {
            bail!("the cart toplevel poisoned its path: {:?}", exec.illegal);
        }
        exec.heap.set_global(sym("tile_flag_at")?, Value::Builtin(Builtin::TileFlagAt));
        exec.call_global(sym("_init")?)?;
        if !exec.ok {
            bail!("_init poisoned its path: {:?}", exec.illegal);
        }
        let base = exec.heap.clone();
        let bridge = Bridge::new(prog)?;
        Ok(FastEngine {
            exec,
            bridge,
            base,
            sym_reset: sym("__reset_button_states")?,
            sym_update: sym("_update")?,
            sym_draw: sym("_draw")?,
            t_exec: 0.0,
            paths: 0,
        })
    }

    /// The frame-0 state as a one-lane boundary state (`RefEngine::initial_state`).
    pub fn initial_state(&self) -> Result<OState> {
        self.bridge.export(&self.base)
    }

    /// SPIKE INSTRUMENTATION: expressions evaluated so far (statements are
    /// not counted separately here).
    pub fn op_counts(&self) -> (u64, u64) {
        (self.exec.n_eval, 0)
    }

    /// Every DISTINCT leaf of one frame from `input`: the reference
    /// driver's `run_frame_all` (reset the buttons, `_update`, `_draw`; DFS
    /// over the cursor; a leaf is kept whether or not its path is legal),
    /// except that a leaf whose boundary fingerprint an earlier path of
    /// this frame already produced is dropped - it would export to the
    /// same state. `paths` counts the paths run.
    pub fn run_frame_all(&mut self, input: &Heap) -> Result<Vec<Heap>> {
        self.exec.d.cursor = Cursor::new();
        let mut out = Vec::new();
        let mut seen: FxHashSet<Vec<u64>> = FxHashSet::default();
        let mut fp = Vec::new();
        let mut paths = 0usize;
        loop {
            self.exec.d.cursor.reset();
            self.exec.prints.clear();
            self.exec.ok = true;
            self.exec.decided.clear();
            self.exec.heap.clone_from(input);
            self.exec.call_global(self.sym_reset)?;
            self.exec.call_global(self.sym_update)?;
            self.exec.call_global(self.sym_draw)?;
            paths += 1;
            self.paths += 1;
            self.bridge.fingerprint(&self.exec.heap, &mut fp);
            if !seen.contains(&fp) {
                seen.insert(fp.clone());
                out.push(self.exec.heap.clone());
            }
            if paths > 1_000_000 {
                bail!("run_frame_all: >1M paths - fork tree did not terminate");
            }
            if !self.exec.d.cursor.advance() {
                break;
            }
        }
        Ok(out)
    }

    pub fn run_frame(&mut self, input: &OState) -> Result<Vec<OState>> {
        let mut out = Vec::new();
        for lane in 0..input.vector_size.max(1) {
            let heap = self.bridge.import(input, lane, &self.base)?;
            let t0 = std::time::Instant::now();
            let leaves = self.run_frame_all(&heap)?;
            self.t_exec += t0.elapsed().as_secs_f64();
            for leaf in leaves {
                out.push(self.bridge.export(&leaf)?);
            }
        }
        Ok(out)
    }
}

impl crate::frame::FrameStep for FastEngine {
    fn run(&mut self, block: &crate::frame::Block) -> Result<Vec<crate::frame::Block>> {
        Ok(self
            .run_frame(block.state())?
            .into_iter()
            .map(crate::frame::Block::new)
            .collect())
    }
}
