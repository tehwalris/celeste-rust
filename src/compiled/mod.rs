//! ONE frame of the abstract search, behind one interface.
//!
//! `FrameEngine::step` is `(shape, rows) -> [(shape, rows)]`: the compiled
//! class kernels where they bind, the celeste-rust interpreter where they
//! do not, with the boundary abstraction, cross-block dedup and the k-way
//! same-shape merge around both. `bridge` translates an interpreter `State`
//! into a block and back - the one place in the codebase that names both
//! `State` and `Rt2`, which is why it is HERE and not in celeste-engine.
//!
//! This module is the point of the P1 crate split (task #150). All of it
//! used to be native-probe's `main.rs`, and native-probe depends on
//! celeste-rust rather than the other way round, so the campaign could not
//! call any of it. Now it can, and a kernel that lands lands in the forward
//! loop and the backward sweep at once.

use std::sync::Arc;

use anyhow::Result;
use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_engine::runtime2;
// The engine's hasher, not celeste-rust's. rustc-hash 1 and 2 hash
// differently and this crate is still on 1; the row machinery's maps
// belong to the engine, so they use the engine's.
use celeste_engine::FxHashMap;
use celeste_names as gen;

use crate::rewrite::program::Program;

pub mod bridge;
pub mod dispatch;

/// The lane kernels are the compiled engine (plans/kernel-plan.md); chunks
/// they refuse fall through to the reference. The retired tile engines
/// (CELESTE_TILE=1 concrete-button tiles, =2 dynamic expand) are gone - the
/// kernels cover every player class and are ~5x faster
/// (plans/k4-retirement-plan.md). CELESTE_KERNEL=0 routes everything to the
/// reference for A/B.
fn use_kernel() -> bool {
    std::env::var("CELESTE_KERNEL").map(|v| v != "0").unwrap_or(true)
}

/// Chunk cap. Cross-chunk dedup at the boundary makes chunking invisible to
/// the result (batching invariance is the certified doctrine), so this is
/// purely a cost knob - and pre-dedup INVERTED it. While every emitted row
/// was materialized, a chunk's mid-frame traffic dominated and small chunks
/// won; now duplicates die as a hash probe and a bigger chunk simply catches
/// more of them, so the dedup ratio wins instead. Measured at f35 (min of 5
/// reps, peak RSS), all gates exact:
///
/// ```text
///   lanes:  64      128     256     512     1024    4096
///   before: 348 ms  489     584     673     -       -
///   after:  133 ms  98      91      86      82      85
///   RSS:    0.99 GB  -      1.12    1.44    2.02    4.82
/// ```
///
/// 256 is the knee: 1.46x over the old default for +13% memory, and the mean
/// stays as tight as the min (512's does not).
fn chunk_rows() -> usize {
    std::env::var("CELESTE_CHUNK_ROWS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(256)
}

/// The same cap for `run_frame_chunk`, which has its own knee because its
/// input is ALREADY a campaign chunk (`CELESTE_MAX_STATE_LANES`, 8,000
/// under the parallel default) rather than a whole frame. Cutting 8,000
/// lanes into 256-row pieces pays the kernel's per-chunk fixed costs -
/// bind, key plan, the `seen` set, `boundary` on the output - 31 times over.
/// Measured, room (1,0), f50, frontier-only + deopt, wall clock:
///
/// ```text
///   rows:  256     512     1024    2048    8000
///   wall:  8.71 s  7.81    7.50    7.43    7.89
/// ```
///
/// Flat from 1,024 to 2,048 and up again at the campaign cap itself, where
/// there is one piece and the dedup no longer sees across pieces.
fn campaign_chunk_rows() -> usize {
    std::env::var("CELESTE_CHUNK_ROWS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(2048)
}

/// Where `run_frame_chunk`'s time goes, under `CELESTE_CHUNK_PHASE_TIME=1`.
///
/// Off by default and gated on a `OnceLock` bool rather than an env read
/// per chunk, because the chunks are small and there are a lot of them.
/// Atomics because the campaign runs one chunk per worker thread; the sum
/// over threads is what the ratios are read off, so contention on five
/// counters per chunk is acceptable where per-slice counters would not be.
pub const CHUNK_IMPORT: usize = 0;
pub const CHUNK_PARTITION: usize = 1;
pub const CHUNK_RUN: usize = 2;
pub const CHUNK_MERGE: usize = 3;
pub const CHUNK_EXPORT: usize = 4;
const CHUNK_PHASE_NAMES: [&str; 5] = ["import", "partition", "run", "dedup+merge", "export"];
static CHUNK_NS: [std::sync::atomic::AtomicU64; 5] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

fn chunk_phase_time() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var_os("CELESTE_CHUNK_PHASE_TIME").is_some())
}

struct ChunkTimer(Option<std::time::Instant>);

impl ChunkTimer {
    fn start() -> Self {
        ChunkTimer(chunk_phase_time().then(std::time::Instant::now))
    }
    fn mark(&mut self, phase: usize) {
        if let Some(at) = self.0.as_mut() {
            let now = std::time::Instant::now();
            CHUNK_NS[phase].fetch_add(
                (now - *at).as_nanos() as u64,
                std::sync::atomic::Ordering::Relaxed,
            );
            *at = now;
        }
    }
}

/// Print and reset the `run_frame_chunk` phase split. A no-op unless
/// `CELESTE_CHUNK_PHASE_TIME` is set.
pub fn print_chunk_phase_times() {
    if !chunk_phase_time() {
        return;
    }
    let ns: Vec<u64> = CHUNK_NS
        .iter()
        .map(|a| a.swap(0, std::sync::atomic::Ordering::Relaxed))
        .collect();
    let total: u64 = ns.iter().sum();
    if total == 0 {
        return;
    }
    eprintln!("compiled chunk phases (summed over worker threads):");
    for (name, n) in CHUNK_PHASE_NAMES.iter().zip(&ns) {
        eprintln!(
            "  {:12} {:8.2}s  {:5.1}%",
            name,
            *n as f64 / 1e9,
            100.0 * *n as f64 / total as f64
        );
    }
}

fn boundary_ids() -> runtime2::BoundaryIds {
    let g = |name: &str| gen::global_id(name).unwrap_or_else(|| panic!("no global {}", name));
    let f = |name: &str| gen::field_id(name).unwrap_or_else(|| panic!("no field {}", name));
    runtime2::BoundaryIds {
        g_objects: g("objects"),
        g_player: g("player"),
        g_timers: ["frames", "seconds", "minutes", "deaths"].iter().map(|n| g(n)).collect(),
        f_type: f("type"),
        f_rem: f("rem"),
        f_spd: f("spd"),
        f_x: f("x"),
        f_y: f("y"),
        f_dash_effect_time: f("dash_effect_time"),
        // The recipe's partition_merge (pm1) key. `has_dashed` and
        // `freeze` are globals; the rest are player fields.
        g_pm1: ["has_dashed", "freeze"].iter().map(|n| g(n)).collect(),
        f_pm1: ["dash_time", "djump", "p_dash", "p_jump"].iter().map(|n| f(n)).collect(),
    }
}

/// One frame of the abstract search, for whoever wants it.
///
/// `(shape, rows) -> [(shape, rows)]`. This is the interface P1 exists to
/// create: before it, the block model, the kernels and this driver all
/// lived in native-probe, which DEPENDS on celeste-rust, so the campaign
/// could not call any of it. Now the forward loop and the backward sweep's
/// replay can both go through `step`, and a kernel that lands lands in both.
///
/// The engine is a pair of paths and a policy for choosing between them.
/// The compiled path is the generated class kernels
/// (`dispatch::run_chunk_kernel`). The reference path is the celeste-rust
/// INTERPRETER, on the block exported back to a `State` - the same
/// `Program`, the same `interpret_prepared_cfg`, the same code the campaign
/// and `concrete_run` execute (plans/k4-retirement-plan.md stage 2). A
/// chunk no kernel binds takes the reference, so a coverage gap is slow and
/// never wrong. Coverage is room-shaped today: room (1,0)'s player classes
/// have kernels, spawn shapes and rooms (0,0)/(2,0) do not.
///
/// Speed of the reference path is a measured non-issue rather than a hope:
/// since the class kernels reached 100% of player lanes it runs on spawn
/// shapes only, 0.72 ms at f20.
/// The plain (unrewritten) program plus the canonical-state mapping of the
/// recipe this engine runs - the deopt path of plans/shape-tag-plan.md
/// Phase C. The kernels' deopt sub-chunks (dying representatives, class-
/// leaving rows) FAIL the specialized program's premises by construction,
/// and without this path that failure aborts the whole compiled attempt:
/// the caller's optimistic deopt arm then re-runs the ENTIRE state under
/// granular deopt and throws the kernel's rows away. Routing the sub-chunks
/// straight to the plain program (sound for every lane - it is the
/// reference semantics) keeps the attempt alive.
pub struct PlainPath {
    pub plain_cfg: crate::interpreter::fixed_env::PreparedCfg,
    pub plain_env: crate::interpreter::fixed_env::FixedEnv,
    pub mapping: crate::rewrite::state_mapping::StateMapping,
}

pub struct FrameEngine {
    ids: runtime2::BoundaryIds,
    frame_cfg: crate::interpreter::fixed_env::PreparedCfg,
    fixed_env: crate::interpreter::fixed_env::FixedEnv,
    plain: Option<PlainPath>,
    init_cfg: crate::ir::Cfg,
    /// The freeze global. Every block is pre-partitioned on it before the
    /// frame runs: the update-side freeze gate is a real per-lane branch,
    /// and splitting on it up front is what keeps the kernels' premise of
    /// a class-uniform chunk true (pm1's precedent).
    g_freeze: u32,
    cart: Arc<CartData>,
    cache: Arc<CollisionCache>,
}

impl FrameEngine {
    /// Build the engine for `program`.
    ///
    /// The program MUST be the one the kernels and the name tables were
    /// generated from - `transpile --recipe rewrites-compile.jsonl`, the
    /// REWRITTEN program, not the plain one. This cost an afternoon once:
    /// the plain program's frame boxes a captured `self` where the recipe's
    /// `demote_create` does not, so the output heap gains one cell, every
    /// later cell id shifts, and the gate reports 204 rows missing and 204
    /// extra - a total mismatch produced by an aliasing difference in ONE
    /// closure capture.
    pub fn new(program: &Program, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Self {
        // The recipe's partition_merge (pm1) cells are a PROCESS GLOBAL in
        // the interpreter, and `AbstractRun::start` sets them before it
        // runs anything. Any frame this engine interprets - the reference
        // path, and `initial_blocks` - has to run under the same setting or
        // it merges differently from the reference it claims to be. Setting
        // it here rather than asking the caller to means the engine cannot
        // be constructed into an inconsistent state.
        crate::interpreter::vectorize::set_merge_partition_patterns(
            &program.merge_partition_cells,
        );
        FrameEngine {
            ids: boundary_ids(),
            frame_cfg: crate::interpreter::fixed_env::PreparedCfg::new(
                program.frame_cfg().clone(),
            ),
            fixed_env: program.fixed_env(),
            plain: None,
            init_cfg: program.init_cfg().clone(),
            g_freeze: gen::global_id("freeze").expect("no freeze global"),
            cart,
            cache,
        }
    }

    /// The same construction with the cart and collision cache loaded for
    /// the campaign's start room.
    pub fn new_for_start_room(program: &Program) -> Result<Self> {
        let (room_x, room_y) = crate::game_runner::start_room();
        let cart = Arc::new(CartData::load("cart")?);
        let cache = Arc::new(CollisionCache::new(&cart, room_x, room_y)?);
        Ok(Self::new(program, cart, cache))
    }

    pub fn ids(&self) -> &runtime2::BoundaryIds {
        &self.ids
    }

    /// Attach the plain-program deopt path (see `PlainPath`). Without it,
    /// the kernels' deopt sub-chunks run the specialized interpreter and a
    /// premise failure aborts the whole frame chunk.
    pub fn set_plain_path(&mut self, plain: PlainPath) {
        self.plain = Some(plain);
    }

    /// One frame of a kernel deopt sub-chunk through the PLAIN program:
    /// to_canonical -> plain -> from_canonical, sound for every lane.
    fn plain_block(&self, plain: &PlainPath, block: runtime2::Rt2) -> Vec<crate::interpreter::state::State> {
        let width = block.width as u64;
        let mut state = bridge::export_block(&block);
        drop(block);
        plain
            .mapping
            .to_canonical(&mut state)
            .expect("plain path: mapping the frame input to canonical");
        let result = crate::interpreter::glue::interpret_prepared_cfg(
            &plain.plain_cfg,
            state,
            &plain.plain_env,
        )
        .expect("plain path: the frame failed under the plain program too");
        dispatch::PLAIN_ROUTED.fetch_add(width, std::sync::atomic::Ordering::Relaxed);
        result
            .into_iter()
            .map(|(mut s, _)| {
                plain
                    .mapping
                    .from_canonical(&mut s)
                    .expect("plain path: mapping a frame output back from canonical");
                // The campaign abstraction, applied HERE rather than
                // trusted to the caller: the plain program does not pin
                // the timer globals (task #75) or apply the boundary
                // widenings, so its raw outputs carry different row keys
                // than the specialized program's for the same states.
                // The campaign path would re-abstract anyway
                // (idempotent); `step` boundaries directly and needs it.
                crate::interpreter::abstraction::make_state_abstract(s)
            })
            .filter(|s| s.vector_size > 0)
            .collect()
    }

    /// One frame of ONE campaign chunk: `State -> [State]`.
    ///
    /// This is the campaign's entry point (P1 stage 3). `step` owns a whole
    /// frame - partition, run, boundary, cross-block dedup, k-way merge -
    /// but the campaign already owns the last three, and they are not
    /// interchangeable: the campaign's boundary is where the frontier
    /// subtract, the band filter and the row table's id assignment live,
    /// and those are proof-critical. So the campaign keeps them, and what
    /// it hands over is the FRAME BODY - exactly the call this replaces,
    /// `interpret_prepared_cfg` on one chunk.
    ///
    /// Consequences of that boundary, all of which the caller must live
    /// with:
    ///
    /// * The outputs are a MIXTURE. Kernel chunks come back already
    ///   canonicalized by `Rt2::boundary`; the reference path's come back
    ///   raw, exactly as the interpreter produced them. That is fine only
    ///   because the campaign re-applies its own abstraction to everything
    ///   afterwards and that abstraction is IDEMPOTENT on already-abstract
    ///   states (the same property gate 2 relies on when it funnels the
    ///   interpreter's states through `boundary` to compare keys).
    /// * `Rt2::boundary` hardcodes the LEVEL-0 rem widening, so this path
    ///   is only valid at `CELESTE_REM_BITS=0`. Checked by the caller, in
    ///   `AbstractRun::compiled_engine`, and not here, because here there
    ///   is nothing useful to say if it fails.
    /// * The output STATES are not the ones the interpreter would have
    ///   produced - different in number, in lane order and in heap layout.
    ///   The output row SET is (that is gate 2). So a compiled forward run
    ///   assigns row ids in a different order than an interpreted one, and
    ///   its `g.bin` is isomorphic to rather than byte-identical with the
    ///   interpreted run's.
    ///
    /// Serial on purpose: the campaign is already one thread per chunk.
    pub fn run_frame_chunk(&self, state: &crate::interpreter::state::State) -> Vec<crate::interpreter::state::State> {
        let mut t = ChunkTimer::start();
        let block = bridge::import_block(state, self.cart.clone(), self.cache.clone());
        t.mark(CHUNK_IMPORT);
        let mut pending: Vec<(runtime2::Rt2, bool)> = self
            .partition_chunks(vec![block], campaign_chunk_rows())
            .into_iter()
            .map(|b| (b, true))
            .collect();
        t.mark(CHUNK_PARTITION);
        let use_kernel = use_kernel();
        let mut done: Vec<runtime2::Rt2> = Vec::new();
        let mut out: Vec<crate::interpreter::state::State> = Vec::new();
        while let Some((block, kernel_ok)) = pending.pop() {
            if use_kernel && kernel_ok {
                if dispatch::run_chunk_kernel(&block, &self.ids, &mut done, &mut pending) {
                    continue;
                }
            }
            // A kernel's deopt sub-chunk (kernel_ok=false: dying
            // representatives, class-leaving rows) FAILS the specialized
            // program's premises by construction - run it under the PLAIN
            // program instead of letting the failure abort the whole
            // chunk (see `PlainPath`).
            if !kernel_ok {
                if let Some(plain) = &self.plain {
                    out.extend(self.plain_block(plain, block));
                    continue;
                }
            }
            // The reference path, but WITHOUT the re-import `step` does:
            // the campaign wants states, and re-importing only to export
            // again would be pure loss.
            out.extend(self.interpret_block(block));
        }
        t.mark(CHUNK_RUN);
        // Dedup and merge the kernel's blocks BEFORE exporting them.
        //
        // Not an optimization of the campaign's merge - the campaign merges
        // again afterwards and would reach the same set either way - but of
        // the BRIDGE. An 8,000-lane campaign chunk becomes ~31 kernel
        // chunks of 256 rows, and exporting each one separately builds ~31
        // whole interpreter heaps where one will do. Measured at f40, 40
        // frames: exporting per chunk put `fwd.interpret` at 2.17 s, ABOVE
        // the interpreter's own 1.39 s; the win only appears once the
        // bridge is crossed once per output group.
        let keeps = Self::dedup_keeps_serial(&done);
        let merged = self.regroup_and_merge(done, keeps, &mut |_| {});
        t.mark(CHUNK_MERGE);
        out.extend(merged.iter().map(bridge::export_block));
        t.mark(CHUNK_EXPORT);
        out
    }

    /// The canonical row-key SET of some interpreter states, as the
    /// boundary computes it.
    ///
    /// This is gate 2's comparator, exposed: both sides of a comparison
    /// funnel through the SAME canonicalizer (import, then `boundary`,
    /// whose widenings are idempotent on already-abstract states), so key
    /// equality means one engine's surviving row set IS the other's, not
    /// merely the same size. Set, not sequence: what the search carries
    /// forward is a set of rows, and neither the order nor the block
    /// partition is part of the answer.
    pub fn row_key_set(
        &self,
        states: &[crate::interpreter::state::State],
    ) -> rustc_hash::FxHashSet<(u64, u64)> {
        // celeste-rust's rustc-hash (1.x), not the engine's (2.x): this is
        // a comparison set, not one of the row machinery's maps, so the
        // hasher is an implementation detail and set equality does not
        // depend on it.
        let mut keys: rustc_hash::FxHashSet<(u64, u64)> = Default::default();
        for s in states {
            if s.vector_size == 0 {
                continue;
            }
            let mut b = bridge::import_block(s, self.cart.clone(), self.cache.clone());
            b.boundary(&self.ids);
            keys.extend(b.row_keys.iter().copied());
        }
        keys
    }

    /// Retain each block's surviving lanes, re-partition on the pm1 key and
    /// k-way merge same-`(shape, pm1)` blocks.
    ///
    /// Blocks stay partitioned by the recipe's fork-condition cells instead
    /// of densifying into one wide block per shape. This is the
    /// interpreter's fragment representation, and its measured 2-3x edge at
    /// depth: key-correlated columns stay `Col::U` through storage, merge,
    /// and the next frame's boundary hashing.
    fn regroup_and_merge(
        &self,
        ran: Vec<runtime2::Rt2>,
        keeps: Vec<Vec<u32>>,
        phase: &mut impl FnMut(&str),
    ) -> Vec<runtime2::Rt2> {
        let ids = &self.ids;
        let mut groups: Vec<((u64, u64), Vec<runtime2::Rt2>)> = Vec::new();
        for (mut sub, keep) in ran.into_iter().zip(keeps) {
            sub.retain_lanes(&keep);
            if sub.width == 0 {
                continue;
            }
            for part in sub.partition_pm1(ids) {
                if std::env::var("CELESTE_KERNEL_MISS").is_ok() {
                    let mixed = part
                        .pm1_cells(ids)
                        .iter()
                        .filter(|c| !matches!(part.cols[**c as usize], runtime2::Col::U(_)))
                        .count();
                    if mixed > 0 {
                        static ONCE2: std::sync::Once = std::sync::Once::new();
                        ONCE2.call_once(|| {
                            eprintln!(
                                "[partition_pm1] part width {} still has {} mixed pm1 columns; cells {:?}",
                                part.width,
                                mixed,
                                part.pm1_cells(ids)
                            );
                            for c in part.pm1_cells(ids) {
                                let d = match &part.cols[c as usize] {
                                    runtime2::Col::U(_) => "uniform".to_string(),
                                    runtime2::Col::N(vs) => {
                                        let mut s: Vec<String> =
                                            vs.iter().map(|v| format!("{:?}", v)).collect();
                                        s.sort();
                                        s.dedup();
                                        format!("N {:?}", s)
                                    }
                                    runtime2::Col::V(vs) => {
                                        let mut s: Vec<String> =
                                            vs.iter().map(|v| format!("{:?}", v)).collect();
                                        s.sort();
                                        s.dedup();
                                        format!("V {:?}", s)
                                    }
                                    runtime2::Col::I(_) => "I".to_string(),
                                };
                                eprintln!("    cell {}: {}", c, d);
                            }
                        });
                    }
                }
                let key = (part.shape_hash, part.pm1_key_hash(ids));
                match groups.iter_mut().find(|(h, _)| *h == key) {
                    Some((_, g)) => g.push(part),
                    None => groups.push((key, vec![part])),
                }
            }
        }
        phase("retain");
        let out: Vec<runtime2::Rt2> =
            groups.into_iter().map(|(_, g)| runtime2::Rt2::merge_many(g)).collect();
        phase("merge");
        out
    }

    /// Cross-block dedup, serial: keep the first occurrence of each row key
    /// across `ran` in block order. `step`'s 32-shard parallel version is
    /// the same function - a row's shard is a function of its key, so the
    /// shards are independent and the surviving SET is the same.
    fn dedup_keeps_serial(ran: &[runtime2::Rt2]) -> Vec<Vec<u32>> {
        let mut seen: FxHashMap<(u64, u64), ()> = Default::default();
        ran.iter()
            .map(|sub| {
                let mut keep = Vec::new();
                for (i, &k) in sub.row_keys.iter().enumerate() {
                    if let std::collections::hash_map::Entry::Vacant(e) = seen.entry(k) {
                        e.insert(());
                        keep.push(i as u32);
                    }
                }
                keep
            })
            .collect()
    }

    /// One frame of `block` through the interpreter, as interpreter states.
    fn interpret_block(&self, block: runtime2::Rt2) -> Vec<crate::interpreter::state::State> {
        if std::env::var_os("CELESTE_FALLBACK_ROUNDTRIP").is_some() {
            bridge::assert_block_round_trips(&block);
        }
        let state = bridge::export_block(&block);
        drop(block);
        crate::interpreter::glue::interpret_prepared_cfg(&self.frame_cfg, state, &self.fixed_env)
            .expect("the interpreter fallback failed a frame")
            .into_iter()
            .map(|(s, _)| s)
            .filter(|s| s.vector_size > 0)
            .collect()
    }

    /// The pre-partition every path shares: split on `freeze`, then on the
    /// moving key, then slice to `chunk_rows` lanes.
    ///
    /// Splitting on `freeze` up front is what keeps the kernels' premise of
    /// a class-uniform chunk true - the update-side freeze gate is a real
    /// per-lane branch (pm1's precedent).
    fn partition_chunks(&self, blocks: Vec<runtime2::Rt2>, chunk_rows: usize) -> Vec<runtime2::Rt2> {
        let mut pending = Vec::new();
        for block in blocks {
            let freeze_cell = block.globals[self.g_freeze as usize];
            assert!(freeze_cell != runtime2::NONE);
            for sub in block.partition_by_cell(freeze_cell) {
                let parts = match sub.moving_key(&self.ids) {
                    Some(key) => {
                        let key = key.clone();
                        sub.partition_by_key(&key)
                    }
                    None => vec![sub],
                };
                for part in parts {
                    if part.width <= chunk_rows {
                        pending.push(part);
                    } else {
                        let n = part.width;
                        let mut at = 0;
                        while at < n {
                            let hi = (at + chunk_rows).min(n);
                            pending.push(part.slice_lanes(at, hi));
                            at = hi;
                        }
                    }
                }
            }
        }
        pending
    }

    /// The frame-0 frontier: run `__init` through the interpreter and
    /// import the resulting states as blocks.
    ///
    /// Same construction as `verify.rs`'s `AbstractRun::start` - same
    /// program, same `create_initial_state_with_builtins`, same
    /// `inject_tile_flag_at_builtin` afterwards - which is what makes a
    /// compiled run comparable to `rewrite bench --frames N` at all. The
    /// retired scalar runtime used to reimplement this by executing the
    /// transpiled `__init` and hand-placing the builtin cells; two
    /// implementations of a starting position is one too many, and this one
    /// cannot drift.
    pub fn initial_blocks(&self) -> Vec<runtime2::Rt2> {
        let initial = crate::game_runner::create_initial_state_with_builtins(&self.fixed_env);
        let states =
            crate::interpreter::glue::interpret_cfg(self.init_cfg.clone(), initial, &self.fixed_env)
                .expect("init failed");
        states
            .into_iter()
            .map(|(mut s, _)| {
                crate::game_runner::inject_tile_flag_at_builtin(&mut s);
                bridge::import_block(&s, self.cart.clone(), self.cache.clone())
            })
            .collect()
    }
}

/// Run one frame of `block` through the interpreter and return the
/// boundary blocks. The output goes through the SAME `Rt2::boundary` the
/// compiled path uses, so the canonical form has one implementation
/// whichever engine produced the rows.
impl FrameEngine {
fn run_chunk_interpreted(&self, block: runtime2::Rt2) -> Vec<runtime2::Rt2> {
    let ids = &self.ids;
    let (cart, cache) = (block.cart.clone(), block.cache.clone());
    // CELESTE_FALLBACK_ROUNDTRIP=1 checks export against import on the
    // way in, which is what separates "the exporter lost something" from
    // "the frame did something different" when the gate disagrees.
    if std::env::var_os("CELESTE_FALLBACK_ROUNDTRIP").is_some() {
        bridge::assert_block_round_trips(&block);
    }
    let state = bridge::export_block(&block);
    drop(block);
    let outputs = crate::interpreter::glue::interpret_prepared_cfg(
        &self.frame_cfg,
        state,
        &self.fixed_env,
    )
    .expect("the interpreter fallback failed a frame");
    outputs
        .into_iter()
        .map(|(s, _)| s)
        // A frame can split a chunk into pieces and leave one empty; an
        // empty block has no rows to contribute and `import_block` has
        // nothing to build a shape from.
        .filter(|s| s.vector_size > 0)
        .map(|s| {
            let mut b = bridge::import_block(&s, cart.clone(), cache.clone());
            b.boundary(ids);
            b
        })
        .collect()
}

/// One abstract frame forward: pre-partition (freeze, moving key), chunk,
/// run tiles across threads (SplitReq -> partition + rerun), boundary,
/// cross-block dedup, k-way same-shape merge. Rows in -> rows out.
/// One abstract frame: pre-partition (freeze, then the moving key), chunk,
/// run the chunks across threads, boundary, cross-block dedup, k-way
/// same-shape merge. Rows in, rows out.
pub fn step(
    &self,
    blocks: Vec<runtime2::Rt2>,
    census_total: &mut FxHashMap<&'static str, (u64, u64, u64)>,
) -> Vec<runtime2::Rt2> {
    let ids = &self.ids;
    let use_kernel = use_kernel();
    // CELESTE_PHASE_TIME=1: print the per-frame wall split across the
    // serial/parallel phases (goal 7's measurement harness).
    let phase_time = std::env::var("CELESTE_PHASE_TIME").is_ok();
    let mut t_mark = std::time::Instant::now();
    let mut phase = |name: &str| {
        if phase_time {
            eprintln!("    phase {:8} {:9.3?}", name, t_mark.elapsed());
        }
        t_mark = std::time::Instant::now();
    };
    let mut ran: Vec<runtime2::Rt2> = Vec::new();
    let mut pending: Vec<runtime2::Rt2> = self.partition_chunks(blocks, chunk_rows());
    phase("part");
    // Chunks are independent (lane independence is the certified
    // batching-invariance property); run them across threads. Each
    // worker owns a local pending stack seeded round-robin.
    let n_workers = std::thread::available_parallelism()
        .map(|n| n.get().saturating_sub(2).max(1))
        .unwrap_or(1)
        .min(pending.len().max(1));
    // (block, kernel_ok): kernel-deopted leftovers and SplitReq halves
    // must not re-enter the kernel (a lane the kernel deopted once would
    // deopt forever - an infinite requeue).
    let queues: Vec<Vec<(runtime2::Rt2, bool)>> = {
        let mut qs: Vec<Vec<(runtime2::Rt2, bool)>> = (0..n_workers).map(|_| Vec::new()).collect();
        for (i, b) in pending.drain(..).enumerate() {
            qs[i % n_workers].push((b, true));
        }
        qs
    };
    let results: Vec<Vec<runtime2::Rt2>> = std::thread::scope(|scope| {
        let handles: Vec<_> = queues
            .into_iter()
            .map(|mut local| {
                let ids = &ids;
                let this = &self;
                scope.spawn(move || {
                    let mut done: Vec<runtime2::Rt2> = Vec::new();
                    while let Some((block, kernel_ok)) = local.pop() {
                        if use_kernel && kernel_ok {
                            // KERNEL mode (plans/kernel-plan.md K3): the
                            // steady-class lane kernel first; rows it
                            // deopts re-enter the worklist for the
                            // reference paths; a chunk it cannot bind or
                            // whose uniform premise fails falls through
                            // whole.
                            if dispatch::run_chunk_kernel(&block, ids, &mut done, &mut local) {
                                continue;
                            }
                        }
                        // A kernel deopt sub-chunk fails the specialized
                        // program's premises by construction - the plain
                        // path, as in `run_frame_chunk`.
                        //
                        // KNOWN ISSUE (2026-08-19, diagnosed further):
                        // `native-probe --abstract-bench` on this path
                        // reports 0 missing / 4.12M EXTRA keys at f066.
                        // The twin diagnosis (see the bench) shows the
                        // extra rows are HELD-BUTTON variants: engine
                        // blocks come in (shape, width)-equal pairs and
                        // quads whose discriminator is cell 246 = p_dash
                        // (and p_jump), i.e. concrete kb5/kb4 stored in
                        // the latch fields where the reference has one
                        // collapsed row. NOT the plain path (66k lanes
                        // cannot make 4.12M keys) and NOT the campaign
                        // path (all-68-frame set identity holds there) -
                        // a bench-harness/pipeline-stage divergence
                        // around button concretization. Root-cause
                        // before trusting step()-based gates with
                        // kernels enabled; the campaign gates stand.
                        if !kernel_ok {
                            if let Some(plain) = &this.plain {
                                let (cart, cache) =
                                    (block.cart.clone(), block.cache.clone());
                                done.extend(
                                    this.plain_block(plain, block).iter().map(|s| {
                                        let mut b = bridge::import_block(
                                            s,
                                            cart.clone(),
                                            cache.clone(),
                                        );
                                        b.boundary(ids);
                                        b
                                    }),
                                );
                                continue;
                            }
                        }
                        // The reference: the interpreter, on the block
                        // exported back to a State. This replaced the
                        // transpiled-program path (gen::call_fn over the
                        // Rt2 Engine impl) and its SplitReq worklist -
                        // a divergent branch used to panic out so the
                        // driver could partition the frame-start block
                        // by the condition's per-origin truth and rerun
                        // both halves; the interpreter splits internally
                        // and just returns more than one output state.
                        done.extend(this.run_chunk_interpreted(block));
                    }
                    done
                })
            })
            .collect();
        handles.into_iter().map(|h| h.join().unwrap()).collect()
    });
    phase("run");
    for done in results {
        ran.extend(done);
    }
    for sub in ran.iter_mut() {
        sub.drain_census(census_total);
    }
    // Drop rows already seen this frame (SHARDED parallel dedup: a
    // row's shard is a function of its key, so shards are
    // independent; first-occurrence order within the block sequence
    // is preserved per shard, and the surviving SET - which is all
    // identity requires - is order-independent), then k-way merge
    // same-shape blocks.
    let n_shards = 32usize;
    let keeps: Vec<Vec<u32>> = {
        // (block, lane, key) triples grouped by shard, in block order.
        let mut per_shard_keeps: Vec<Vec<Vec<u32>>> =
            (0..n_shards).map(|_| vec![Vec::new(); ran.len()]).collect();
        std::thread::scope(|scope| {
            let handles: Vec<_> = (0..n_shards)
                .map(|shard| {
                    let ran = &ran;
                    scope.spawn(move || {
                        let mut seen: FxHashMap<(u64, u64), ()> = Default::default();
                        let mut keeps: Vec<Vec<u32>> = vec![Vec::new(); ran.len()];
                        for (bi, sub) in ran.iter().enumerate() {
                            for (i, &k) in sub.row_keys.iter().enumerate() {
                                if (k.0 as usize) % n_shards != shard {
                                    continue;
                                }
                                if let std::collections::hash_map::Entry::Vacant(e) =
                                    seen.entry(k)
                                {
                                    e.insert(());
                                    keeps[bi].push(i as u32);
                                }
                            }
                        }
                        keeps
                    })
                })
                .collect();
            for (shard, h) in handles.into_iter().enumerate() {
                per_shard_keeps[shard] = h.join().unwrap();
            }
        });
        // Merge shards' keeps per block, sorted (retain_lanes needs
        // ascending indices).
        (0..ran.len())
            .map(|bi| {
                let mut keep: Vec<u32> = per_shard_keeps
                    .iter()
                    .flat_map(|s| s[bi].iter().copied())
                    .collect();
                keep.sort_unstable();
                keep
            })
            .collect()
    };
    phase("dedup");
    let out = self.regroup_and_merge(ran, keeps, &mut phase);
    if std::env::var("CELESTE_KERNEL_MISS").is_ok() {
        let mut mixed = 0usize;
        for b in &out {
            for c in b.pm1_cells(ids) {
                if !matches!(b.cols[c as usize], runtime2::Col::U(_)) {
                    mixed += 1;
                }
            }
        }
        eprintln!(
            "[frame_step] {} out blocks, {} non-uniform pm1 columns",
            out.len(),
            mixed
        );
    }
    out
}
}
