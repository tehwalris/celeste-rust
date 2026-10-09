//! ONE frame of the abstract search: `FrameEngine`, the runtime-assembled
//! ASM kernels (`asm_kernel`) over the engine's columnar blocks.

use std::sync::Arc;

use anyhow::Result;
use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_engine::runtime2;

use celeste_names as gen;

pub(crate) mod asm_kernel;
pub mod dispatch;

pub(crate) fn boundary_ids() -> runtime2::BoundaryIds {
    let g = |name: &str| gen::global_id(name).unwrap_or_else(|| panic!("no global {}", name));
    let f = |name: &str| gen::field_id(name).unwrap_or_else(|| panic!("no field {}", name));
    runtime2::BoundaryIds {
        g_objects: g("objects"),
        g_player: g("player"),
        g_player_spawn: g("player_spawn"),
        g_room: g("room"),
        g_max_djump: g("max_djump"),
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
        g_key: g("key"),
        f_spr: f("spr"),
        f_flip: f("flip"),
        f_p_jump: f("p_jump"),
        f_p_dash: f("p_dash"),
        g_fly_fruit: g("fly_fruit"),
        f_step: f("step"),
        f_fly: f("fly"),
        g_platform: g("platform"),
        f_last: f("last"),
        g_fall_floor: g("fall_floor"),
        g_big_chest: g("big_chest"),
        g_chest: g("chest"),
        g_fake_wall: g("fake_wall"),
        f_state: f("state"),
        f_delay: f("delay"),
        f_collideable: f("collideable"),
        g_balloon: g("balloon"),
        f_timer: f("timer"),
        f_offset: f("offset"),
        g_spring: g("spring"),
        f_hide_in: f("hide_in"),
        f_hide_for: f("hide_for"),
    }
}

pub fn ids() -> &'static runtime2::BoundaryIds {
    static IDS: std::sync::OnceLock<runtime2::BoundaryIds> = std::sync::OnceLock::new();
    IDS.get_or_init(boundary_ids)
}

/// The start room's cart and collision cache, loaded once. Every block
/// carries these two `Arc`s (the kernels read tiles through them); anything
/// that builds a block outside an engine attaches the same pair.
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

pub struct FrameEngine;

impl FrameEngine {
    /// The engine for the configured start room (its cart loads here).
    pub fn new_for_start_room() -> Result<Self> {
        room_context()?;
        Ok(FrameEngine)
    }

    /// One frame of one BUCKET (one shape's block of the frontier), emitted
    /// into `sink`: kept rows at the boundary, keyed, with their pos-graph
    /// edges. The kernel slices by 16 itself; the caller routes rows into
    /// the next frame's buckets.
    pub fn run_bucket(
        &self,
        bucket: &runtime2::Rt2,
        cell_in: &[u32],
        lanes: std::ops::Range<usize>,
        sink: &mut crate::frame::ForwardSink,
    ) {
        if !dispatch::run_chunk_kernel(bucket, cell_in, lanes, sink) {
            // A COVERAGE GAP, not a degraded mode: there is no fallback.
            // The search resumes from the last completed frame's checkpoint.
            panic!(
                "KERNEL COVERAGE GAP: a {}-row bucket of shape {:#018x} has no kernel; \
                 reasons:\n{}\ntrace the missing shape and resume from the last checkpoint",
                bucket.width,
                bucket.shape_hash,
                dispatch::miss_report(),
            );
        }
    }
}

/// The compiled kernels as the `FrameStep` implementation, the counterpart
/// to `RefEngine`: just `run_bucket`.
impl crate::frame::FrameStep for FrameEngine {
    fn run(
        &self,
        block: &crate::frame::Block,
        cell_in: &[u32],
        lanes: std::ops::Range<usize>,
        sink: &mut crate::frame::ForwardSink,
    ) -> anyhow::Result<()> {
        self.run_bucket(block.rt2(), cell_in, lanes, sink);
        Ok(())
    }

    fn warm(&self) {
        // A level without kernels fails loudly in `run_bucket`, not here.
        let _ = asm_kernel::registry();
    }
}
