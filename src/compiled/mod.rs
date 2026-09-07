//! ONE frame of the abstract search, behind one interface.
//!
//! `FrameEngine::run_frame_block` is `(shape, rows) -> [(shape, rows)]`: the
//! runtime-assembled ASM kernels (`asm_kernel`) over the engine's columnar
//! blocks, with the pre-partition, cross-block dedup and the k-way
//! same-shape merge around them. `bridge` translates an interpreter `State`
//! into a block and back - the one place in the codebase that names both
//! `State` and `Rt2`, which is why it is HERE and not in celeste-engine.

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

pub(crate) mod asm_kernel;
pub mod bridge;
pub mod dispatch;

/// The kernel chunk cap: an input block is cut into pieces of at most this
/// many lanes before dispatch. Cutting into small pieces pays the kernel's
/// per-chunk fixed costs - bind, the `seen` set, `boundary` on the output -
/// once per piece. Measured, room (1,0), f50, on 8,000-lane inputs, wall
/// clock:
///
/// ```text
///   rows:  256     512     1024    2048    8000
///   wall:  8.71 s  7.81    7.50    7.43    7.89
/// ```
///
/// Flat from 1,024 to 2,048 and up again at 8,000, where there is one piece
/// and the dedup no longer sees across pieces. (The search's blocks are far
/// smaller than this today - see `frame::regroup` - so the cap rarely
/// bites.)
fn campaign_chunk_rows() -> usize {
    std::env::var("CELESTE_CHUNK_ROWS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(2048)
}

/// Where `run_frame_block`'s time goes, under `CELESTE_CHUNK_PHASE_TIME=1`.
/// Off by default and gated on a `OnceLock` bool rather than an env read
/// per block, because the blocks are small and there are a lot of them.
const CHUNK_PARTITION: usize = 0;
const CHUNK_RUN: usize = 1;
const CHUNK_MERGE: usize = 2;
const CHUNK_PHASE_NAMES: [&str; 3] = ["partition", "run", "dedup+merge"];
static CHUNK_NS: [std::sync::atomic::AtomicU64; 3] = [
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

/// Print and reset the `run_frame_block` phase split. A no-op unless
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
    eprintln!("compiled chunk phases:");
    for (name, n) in CHUNK_PHASE_NAMES.iter().zip(&ns) {
        eprintln!(
            "  {:12} {:8.2}s  {:5.1}%",
            name,
            *n as f64 / 1e9,
            100.0 * *n as f64 / total as f64
        );
    }
}

/// The assert-noop guard body (`dispatch::widen_noop_check`): a
/// widen-in-graph kernel's output state must be a FIXED POINT of the
/// campaign's `split_precision_straddles` + `make_state_abstract`. Keys
/// both the state as-is and its re-abstraction with the SAME
/// `engine_row_keys` (so it is a pure "did the boundary move anything"
/// test, no cross-keyer question), and panics on the first difference -
/// naming itself, under the never-deopt doctrine.
fn assert_widen_is_noop(state: &crate::interpreter::state::State) {
    use std::collections::HashSet;
    if state.vector_size == 0 {
        return;
    }
    let keys = |s: &crate::interpreter::state::State| -> HashSet<(u64, u64)> {
        engine_row_keys(s)
            .expect("widen-noop: row_keys")
            .into_iter()
            .collect()
    };
    let before = keys(state);
    let mut after: HashSet<(u64, u64)> = HashSet::new();
    for st in crate::interpreter::abstraction::split_precision_straddles(state.clone()) {
        let w = crate::interpreter::abstraction::make_state_abstract(st);
        after.extend(keys(&w));
    }
    if before != after {
        // Robust cell-by-cell diff: make_state_abstract does not change the
        // heap LENGTH (widening rewrites values in place; erase rewrites
        // nils in place), so compare each cell's Debug form directly and
        // name every one the boundary moved.
        let w = crate::interpreter::abstraction::make_state_abstract(state.clone());
        let n = state.heap.len().min(w.heap.len());
        let mut shown = 0;
        for i in 0..n {
            let id = crate::interpreter::heap::HeapId::from_raw(i);
            let (a, b) = (state.heap.get_opt(id), w.heap.get_opt(id));
            if format!("{:?}", a) != format!("{:?}", b) {
                eprintln!("[widen-noop diff] cell {}: kernel={:?} boundary={:?}", i, a, b);
                shown += 1;
                if shown >= 12 {
                    eprintln!("[widen-noop diff] ... (more)");
                    break;
                }
            }
        }
        if state.heap.len() != w.heap.len() {
            eprintln!(
                "[widen-noop diff] heap LENGTH changed: kernel {} boundary {}",
                state.heap.len(),
                w.heap.len()
            );
        }
    }
    assert_eq!(
        before, after,
        "KERNEL WIDEN-NOOP VIOLATION: re-abstracting a widen-in-graph kernel output \
         changed its row keys ({} before, {} after; {} boundary-only, {} kernel-only). \
         The graph UNDER-widened a field the campaign boundary still moves.",
        before.len(),
        after.len(),
        after.difference(&before).count(),
        before.difference(&after).count(),
    );
}

pub(crate) fn boundary_ids() -> runtime2::BoundaryIds {
    let g = |name: &str| gen::global_id(name).unwrap_or_else(|| panic!("no global {}", name));
    let f = |name: &str| gen::field_id(name).unwrap_or_else(|| panic!("no field {}", name));
    runtime2::BoundaryIds {
        g_objects: g("objects"),
        g_player: g("player"),
        g_player_spawn: g("player_spawn"),
        g_room: g("room"),
        g_timers: ["frames", "seconds", "minutes", "deaths"].iter().map(|n| g(n)).collect(),
        f_type: f("type"),
        f_rem: f("rem"),
        f_spd: f("spd"),
        f_x: f("x"),
        f_y: f("y"),
        f_dash_effect_time: f("dash_effect_time"),
        g_fruit: g("fruit"),
        f_off: f("off"),
        f_start: f("start"),
        // The recipe's partition_merge (pm1) key. `has_dashed` and
        // `freeze` are globals; the rest are player fields.
        g_pm1: ["has_dashed", "freeze"].iter().map(|n| g(n)).collect(),
        f_pm1: ["dash_time", "djump", "p_dash", "p_jump"].iter().map(|n| f(n)).collect(),
    }
}

/// The boundary ids, resolved once: the block's own column readers (the
/// search's position and win columns, `frame::Block`) need them without an
/// engine in hand.
pub fn ids() -> &'static runtime2::BoundaryIds {
    static IDS: std::sync::OnceLock<runtime2::BoundaryIds> = std::sync::OnceLock::new();
    IDS.get_or_init(boundary_ids)
}

/// The start room's cart and collision cache, loaded once. Every block
/// carries these two `Arc`s (the kernels read tiles through them), so
/// anything that builds a block outside an engine - the checkpoint loader,
/// the bridge from a reference `State` - attaches the same pair.
pub fn room_context() -> Result<(Arc<CartData>, Arc<CollisionCache>)> {
    static CTX: std::sync::OnceLock<(Arc<CartData>, Arc<CollisionCache>)> =
        std::sync::OnceLock::new();
    if let Some(c) = CTX.get() {
        return Ok(c.clone());
    }
    let (room_x, room_y) = crate::game_runner::start_room();
    let cart = Arc::new(CartData::load("cart")?);
    let cache = Arc::new(CollisionCache::new(&cart, room_x, room_y)?);
    let _ = CTX.set((cart, cache));
    Ok(CTX.get().expect("just set").clone())
}

/// The canonical (kernel/engine) row keys of a state, per lane in lane
/// order - the ONE row key of the search.
///
/// Backed by a process-wide `FrameEngine` for the start room, so the
/// ids/cart/cache match every forward's and the keys are byte-identical to
/// what a forward stored: there is exactly one key space.
static KEY_ENGINE: std::sync::OnceLock<FrameEngine> = std::sync::OnceLock::new();

pub fn engine_row_keys(
    state: &crate::interpreter::state::State,
) -> Result<Vec<(u64, u64)>> {
    if KEY_ENGINE.get().is_none() {
        // Racing initializers build identical engines; keep whichever wins.
        let _ = KEY_ENGINE.set(FrameEngine::new_for_start_room()?);
    }
    Ok(KEY_ENGINE.get().expect("just set").row_keys_lane_order(state))
}

pub struct FrameEngine {
    ids: runtime2::BoundaryIds,
    /// The freeze global. Every block is pre-partitioned on it before the
    /// frame runs: the update-side freeze gate is a real per-lane branch,
    /// and splitting on it up front is what keeps the kernels' premise of
    /// a class-uniform chunk true (pm1's precedent).
    g_freeze: u32,
    cart: Arc<CartData>,
    cache: Arc<CollisionCache>,
}

impl FrameEngine {
    pub fn new(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Self {
        FrameEngine {
            ids: boundary_ids(),
            g_freeze: gen::global_id("freeze").expect("no freeze global"),
            cart,
            cache,
        }
    }

    /// The engine for the configured start room (`room_context`).
    pub fn new_for_start_room() -> Result<Self> {
        let (cart, cache) = room_context()?;
        Ok(Self::new(cart, cache))
    }

    pub fn ids(&self) -> &runtime2::BoundaryIds {
        &self.ids
    }

    /// The room this engine was built for. Exposed so a caller comparing
    /// against it can import a state into a block the same way it does.
    pub fn cart(&self) -> Arc<CartData> {
        self.cart.clone()
    }

    pub fn cache(&self) -> Arc<CollisionCache> {
        self.cache.clone()
    }

    /// One frame of one block, block in and blocks out: partition on the
    /// frame-start uniform branches, run every chunk on its kernel, dedup
    /// and merge the outputs per (shape, pm1 class). Every output carries
    /// its row keys. This is the search's frame step.
    pub fn run_frame_block(&self, block: runtime2::Rt2) -> Vec<runtime2::Rt2> {
        let mut t = ChunkTimer::start();
        let mut pending = self.partition_chunks(vec![block], campaign_chunk_rows());
        t.mark(CHUNK_PARTITION);
        let mut done: Vec<runtime2::Rt2> = Vec::new();
        // DISPATCH EVERY CHUNK BEFORE ABORTING ON ANY: chunks are
        // independent and the output is a row SET, so running the kernel
        // pass to completion first changes no result - it only moves the
        // abort after the point where every chunk has been dispatched and
        // counted, so the miss report is complete rather than a
        // class-skewed sample of the first-popped partitions.
        let mut misses: Vec<runtime2::Rt2> = Vec::new();
        while let Some(block) = pending.pop() {
            if !dispatch::run_chunk_kernel(&block, &self.ids, &mut done) {
                misses.push(block);
            }
        }
        // FALLBACK IS ALL-OR-NOTHING AT THE STATE LEVEL. When any chunk
        // misses the kernels and a campaign frame body is available, the
        // engine's partial work on this state is discarded and the
        // ORIGINAL state - untouched, exactly as the campaign formed it -
        // runs under the CAMPAIGN program. Four chunk-granular designs
        // died on (2,0) f39/f40 before this:
        // - Whole-state deopt via panic: one bad chunk re-ran the whole
        //   state at PLAIN-program cost, and plain on fruit states blows
        //   up on UnknownBool branch doubling (ladder.sh's sweep cap).
        // - Compile-program per chunk: the (1,0) overlay on a (2,0)
        //   fruit chunk splits exponentially before failing its premise.
        // - Campaign-program per chunk, then re-vectorized batches: both
        //   ground for hours in split_by_condition on states EXPORTED
        //   from engine blocks - the (freeze, moving-key) partition plus
        //   the bridge roundtrip yields lane groupings the campaign's own
        //   flow never forms, and the interpreter's cost model (branch
        //   uniformity from the campaign's partitioning) collapses on
        //   them. The reference interprets the SAME lanes as campaign
        //   states in milliseconds.
        // The campaign program on the campaign's own state is the one
        // fallback with a certified cost model. The engine's win on a
        // partially-covered state is forfeited - at real coverage that is
        // rare, and the miss dump still records exactly what to cover
        // next.
        if !misses.is_empty() {
            // A chunk the kernels cannot take is a COVERAGE GAP, not a
            // degraded mode (plans/tracing.md "Doctrine",
            // plans/delete-the-interpreter.md): the CFG-interpreter
            // fallback was removed. Every chunk of this state was
            // dispatched before this point, so the census is complete;
            // report every distinct reason with lane counts and stop. The
            // search checkpoints per completed frame, so the run resumes
            // from the previous frame once the missing shape is traced.
            let missed_lanes: usize = misses.iter().map(|b| b.width).sum();
            panic!(
                "KERNEL COVERAGE GAP: {} lanes in {} chunks have no kernel and                  the interpreter fallback was removed; reasons:\n{}\ntrace the                  missing shape / raise the bound and resume from the last                  checkpoint",
                missed_lanes,
                misses.len(),
                dispatch::miss_report(),
            );
        }
        t.mark(CHUNK_RUN);
        // Dedup across the kernel's output chunks and merge per (shape,
        // pm1 class), so the loop sees one block per class rather than one
        // per kernel chunk.
        let keeps = Self::dedup_keeps_serial(&done);
        let merged = self.regroup_and_merge(done, keeps);
        t.mark(CHUNK_MERGE);
        if dispatch::widen_noop_check() {
            for b in &merged {
                assert_widen_is_noop(&bridge::export_block(b));
            }
        }
        merged
    }

    /// The kernel/engine row keys of one state, per lane in LANE ORDER (no
    /// dedup, no widening) - the canonical row key of the whole search.
    /// Free-function `engine_row_keys` is the usual entry point; this is the
    /// method when a `FrameEngine` is already in hand.
    ///
    /// Reproduces exactly what a compiled forward's boundary stored for the
    /// same already-abstracted content, so the backward sweep and the band
    /// filter can recompute a saved state's keys and find them in the row
    /// table. `import_block` + `Rt2::row_keys_canonical`.
    pub fn row_keys_lane_order(
        &self,
        state: &crate::interpreter::state::State,
    ) -> Vec<(u64, u64)> {
        let mut b = bridge::import_block(state, self.cart.clone(), self.cache.clone());
        b.row_keys_canonical()
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
    ) -> Vec<runtime2::Rt2> {
        let ids = &self.ids;
        let mut groups: Vec<((u64, u64), Vec<runtime2::Rt2>)> = Vec::new();
        for (mut sub, keep) in ran.into_iter().zip(keeps) {
            sub.retain_lanes(&keep);
            if sub.width == 0 {
                continue;
            }
            for part in sub.partition_pm1(ids) {
                let key = (part.shape_hash, part.pm1_key_hash(ids));
                match groups.iter_mut().find(|(h, _)| *h == key) {
                    Some((_, g)) => g.push(part),
                    None => groups.push((key, vec![part])),
                }
            }
        }
        groups.into_iter().map(|(_, g)| runtime2::Rt2::merge_many(g)).collect()
    }

    /// Cross-block dedup: keep the first occurrence of each row key across
    /// `ran` in block order.
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

    /// The pre-partition: split on `freeze`, then on the moving key, then
    /// slice to `chunk_rows` lanes.
    ///
    /// Splitting on `freeze` up front is what keeps the kernels' premise of
    /// a class-uniform chunk true - the update-side freeze gate is a real
    /// per-lane branch (pm1's precedent).
    pub(crate) fn partition_chunks(&self, blocks: Vec<runtime2::Rt2>, chunk_rows: usize) -> Vec<runtime2::Rt2> {
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

}

impl FrameEngine {
}

/// The compiled kernels as the fast `FrameStep` implementation (interface #1),
/// the counterpart to `RefEngine`. The block IS the engine's block, so this
/// is `run_frame_block` and nothing else: no bridge, and every output
/// carries the key column the kernel computed. `&mut self` per the trait;
/// the engine itself is `&self`.
impl crate::frame::FrameStep for FrameEngine {
    fn run(&mut self, block: crate::frame::Block) -> anyhow::Result<Vec<crate::frame::Block>> {
        Ok(self
            .run_frame_block(block.into_rt2())
            .into_iter()
            .map(crate::frame::Block::from_rt2)
            .collect())
    }
}
