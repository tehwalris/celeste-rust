//! ONE frame of the abstract search: `FrameEngine`, the runtime-assembled
//! ASM kernels (`asm_kernel`) over the engine's columnar blocks.

use std::sync::Arc;

use anyhow::Result;
use celeste_core::collision_cache::CollisionCache;
use celeste_engine::runtime2;

use celeste_names as gen;

pub(crate) mod asm_kernel;
pub mod dispatch;
pub mod mix;

pub use asm_kernel::{KernelLanes, Registry, Resident, Scratch};

/// THE GAME'S NAMES the search reads off a block, as ids (`celeste_names`).
/// The position object's are the engine's own (`runtime2::names`), shared.
pub struct BoundaryIds {
    pub g_objects: u32,
    /// `room` (`x`/`y`): positions are start-room-relative; wins test `room.x`.
    pub g_room: u32,
    /// `max_djump`: 2 once the orb is taken (`frame::wins_of`).
    pub g_max_djump: u32,
    pub f_type: u32,
    pub f_rem: u32,
    pub f_spd: u32,
    pub f_x: u32,
    pub f_y: u32,
    pub f_dash_effect_time: u32,
    /// The berry's sources (`frame::berry_lost`): the strawberry, the fly
    /// fruit, the key, the chest the key opens (its `state`), and the fake
    /// wall that drops one when broken.
    pub g_fruit: u32,
    pub g_fly_fruit: u32,
    pub g_key: u32,
    pub g_chest: u32,
    pub g_fake_wall: u32,
    pub f_state: u32,
    /// The orb room's big chest (`frame::orb_deadline_skip`).
    pub g_big_chest: u32,
    /// The moving platforms (a frame test).
    pub g_platform: u32,
    /// The balloon's phase and bob (`concrete::lookup_view`, a seeded state).
    pub g_balloon: u32,
    pub f_offset: u32,
    pub f_start: u32,
}

fn boundary_ids() -> BoundaryIds {
    let g = |name: &str| gen::global_id(name).unwrap_or_else(|| panic!("no global {}", name));
    let f = |name: &str| gen::field_id(name).unwrap_or_else(|| panic!("no field {}", name));
    let engine = runtime2::names();
    BoundaryIds {
        g_objects: engine.objects,
        g_room: g("room"),
        g_max_djump: g("max_djump"),
        f_type: engine.type_,
        f_rem: f("rem"),
        f_spd: f("spd"),
        f_x: engine.x,
        f_y: engine.y,
        f_dash_effect_time: f("dash_effect_time"),
        g_fruit: g("fruit"),
        g_fly_fruit: g("fly_fruit"),
        g_key: g("key"),
        g_chest: g("chest"),
        g_fake_wall: g("fake_wall"),
        f_state: f("state"),
        g_big_chest: g("big_chest"),
        g_platform: g("platform"),
        g_balloon: g("balloon"),
        f_offset: f("offset"),
        f_start: f("start"),
    }
}

pub fn ids() -> &'static BoundaryIds {
    static IDS: std::sync::OnceLock<BoundaryIds> = std::sync::OnceLock::new();
    IDS.get_or_init(boundary_ids)
}

/// Room `(room_x, room_y)`'s collision cache, built once per room (a cache
/// keyed on the room). Every block carries it (the kernels read tiles
/// through it; the map itself is `cart_data::CART`); anything that builds a
/// block outside an engine attaches the same one.
pub fn collision_cache((room_x, room_y): (i16, i16)) -> Arc<CollisionCache> {
    static CACHES: std::sync::Mutex<Vec<((i16, i16), Arc<CollisionCache>)>> = std::sync::Mutex::new(Vec::new());
    let mut all = CACHES.lock().unwrap();
    if let Some((_, c)) = all.iter().find(|(r, _)| *r == (room_x, room_y)) {
        return c.clone();
    }
    let cache = Arc::new(CollisionCache::new(room_x, room_y));
    all.push(((room_x, room_y), cache.clone()));
    cache
}

/// The kernels of one LEVEL under the run's settings
/// (`asm_kernel::registry`, built on first use), in the arithmetic of `N`.
pub struct Kernels<N: KernelLanes = celeste_core::pico8_num::Pico8Num> {
    level: crate::abstraction::Level,
    settings: Arc<crate::settings::Settings>,
    registry: std::sync::OnceLock<Arc<asm_kernel::Registry<N>>>,
}

/// 16.16's kernels: the real game's.
pub type FrameEngine = Kernels<celeste_core::pico8_num::Pico8Num>;

/// The doubles' kernels (plans/float.md, "The kernels' safety").
pub type FloatKernels = Kernels<celeste_core::lua_f64::LuaF64>;

impl<N: KernelLanes> Kernels<N> {
    /// The engine for the start room of `settings`, its frames at `level`.
    pub fn new(settings: &crate::settings::Settings, level: crate::abstraction::Level) -> Result<Self> {
        Ok(Kernels { level, settings: Arc::new(settings.clone()), registry: std::sync::OnceLock::new() })
    }

    fn registry(&self) -> &asm_kernel::Registry<N> {
        self.registry.get_or_init(|| asm_kernel::registry::<N>(self.level, &self.settings))
    }

    /// One frame of one BUCKET (one shape's block), emitted into `sink`:
    /// kept rows at the boundary, keyed, with their cells.
    pub fn run_bucket(
        &self,
        bucket: &runtime2::Rt2<N>,
        cell_in: &[u32],
        lanes: &[usize],
        sink: &mut crate::storage::unit::UnitSink<N>,
    ) {
        if !dispatch::run_chunk_kernel(self.registry(), bucket, cell_in, lanes, sink) {
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

/// The compiled `FrameStep` (the reference's: `RefEngine`).
impl<N: KernelLanes> crate::frame::FrameStep<N> for Kernels<N> {
    fn run(
        &self,
        block: &crate::frame::Block<N>,
        cell_in: &[u32],
        lanes: &[usize],
        sink: &mut crate::storage::unit::UnitSink<N>,
    ) -> anyhow::Result<()> {
        self.run_bucket(block.rt2(), cell_in, lanes, sink);
        Ok(())
    }

    fn warm(&self) {
        // A level without kernels fails loudly in `run_bucket`, not here.
        let _ = self.registry();
    }

    fn level(&self) -> crate::abstraction::Level {
        self.level
    }

    fn settings(&self) -> Arc<crate::settings::Settings> {
        self.settings.clone()
    }
}
