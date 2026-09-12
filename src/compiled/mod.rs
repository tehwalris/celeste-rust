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

use celeste_names as gen;

pub(crate) mod asm_kernel;
pub mod bridge;
pub mod dispatch;

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
    cart: Arc<CartData>,
    cache: Arc<CollisionCache>,
}

impl FrameEngine {
    pub fn new(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Self {
        FrameEngine {
            ids: boundary_ids(),
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

    /// One frame of one BUCKET (one shape's block of the frontier), emitted
    /// into `sink`: every output row the kernel keeps lands in `sink.out`
    /// (per outcome, at the boundary, keyed), its pos-graph edge in
    /// `sink.edges`, and - when the sink carries the visited set - only
    /// rows new to the search are materialized at all. No pre-partition,
    /// no chunking (the kernel slices by 16 itself), no post-merge (the
    /// caller routes rows into next frame's buckets).
    pub fn run_bucket(
        &self,
        bucket: &runtime2::Rt2,
        cell_in: &[u32],
        sink: &mut crate::frame::ForwardSink,
    ) {
        if !dispatch::run_chunk_kernel(bucket, &self.ids, cell_in, sink) {
            // A chunk the kernels cannot take is a COVERAGE GAP, not a
            // degraded mode (CLAUDE.md "Never deopt to the interpreter"):
            // there is no fallback. The search checkpoints per completed
            // frame, so the run resumes from the previous frame once the
            // missing shape is traced.
            panic!(
                "KERNEL COVERAGE GAP: a {}-row bucket of shape {:#018x} has no kernel; \
                 reasons:\n{}\ntrace the missing shape and resume from the last checkpoint",
                bucket.width,
                bucket.shape_hash,
                dispatch::miss_report(),
            );
        }
        if dispatch::widen_noop_check() {
            for b in &sink.out {
                assert_widen_is_noop(&bridge::export_block(b));
            }
        }
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
}

/// The compiled kernels as the fast `FrameStep` implementation (interface #1),
/// the counterpart to `RefEngine`. The block IS the engine's block, so this
/// is `run_bucket` and nothing else: no bridge, and every output carries
/// the key column the kernel computed. `&mut self` per the trait; the
/// engine itself is `&self`.
impl crate::frame::FrameStep for FrameEngine {
    fn run(
        &mut self,
        block: crate::frame::Block,
        sink: &mut crate::frame::ForwardSink,
    ) -> anyhow::Result<()> {
        let cell_in = block.positions()?;
        self.run_bucket(&block.into_rt2(), &cell_in, sink);
        Ok(())
    }
}
