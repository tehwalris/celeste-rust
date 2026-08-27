//! The call-out ABI: ops the emitter cannot (or will not) vectorise inline -
//! `Div`/`Rem`/`Sin` (scalar per-lane in the Rust primitives too) and the
//! collision look-ups `Mget`/`TileFlagAt` (a cart/cache query mid-DAG) - are
//! emitted as a `call` through a function pointer carried in `AsmCtx`, the
//! kernel's third argument. Every SIMD operand is marshalled through a stack
//! buffer and the result read back, so the wrappers just invoke the exact
//! `celeste_engine::kernel` primitive and the result is bit-identical by
//! construction. The emitted code save/restores all 32 zmm around the call
//! (a call clobbers every vector register); that is correctness-first, not
//! fast - see `plans/asm-backend.md`.

use std::os::raw::c_void;

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::kernel::{
    zn_div, zn_mget, zn_rem, zn_sin, zn_tile_flag_at, zn_tile_flag_at_lanes, ZN,
};

#[inline]
unsafe fn load_zn(p: *const i32) -> ZN {
    ZN::from_array(std::array::from_fn(|i| P8::from_raw(*p.add(i))))
}
#[inline]
unsafe fn store_zn(p: *mut i32, z: ZN) {
    let a = z.to_array();
    for i in 0..16 {
        *p.add(i) = a[i].as_raw_u32() as i32;
    }
}

/// The cart/cache the collision wrappers query. Held behind `AsmCtx::env`.
#[repr(C)]
pub struct CollisionEnv<'a> {
    pub cart: &'a CartData,
    pub cache: &'a CollisionCache,
}

pub unsafe extern "C" fn co_div(out: *mut i32, a: *const i32, b: *const i32) {
    store_zn(out, zn_div(load_zn(a), load_zn(b)));
}
pub unsafe extern "C" fn co_rem(out: *mut i32, a: *const i32, b: *const i32) {
    store_zn(out, zn_rem(load_zn(a), load_zn(b)));
}
pub unsafe extern "C" fn co_sin(out: *mut i32, a: *const i32) {
    store_zn(out, zn_sin(load_zn(a)));
}
pub unsafe extern "C" fn co_mget(env: *const c_void, out: *mut i32, x: *const i32, y: *const i32) {
    let env = &*(env as *const CollisionEnv);
    store_zn(out, zn_mget(env.cart, load_zn(x), load_zn(y)));
}
/// `tile_flag_at` with a UNIFORM box (`w`/`h`/`flag` scalars). Writes the
/// 16-bit `val` mask to `*out_mask` (known is all-ones).
pub unsafe extern "C" fn co_tile_flag(
    env: *const c_void,
    out_mask: *mut u16,
    x: *const i32,
    y: *const i32,
    w: i32,
    h: i32,
    flag: i32,
) {
    let env = &*(env as *const CollisionEnv);
    let zb = zn_tile_flag_at(
        env.cache,
        env.cart,
        load_zn(x),
        load_zn(y),
        P8::from_raw(w),
        P8::from_raw(h),
        P8::from_raw(flag),
    );
    *out_mask = zb.val;
}
/// `tile_flag_at` with a PER-LANE box (`w`/`h` are columns).
pub unsafe extern "C" fn co_tile_flag_lanes(
    env: *const c_void,
    out_mask: *mut u16,
    x: *const i32,
    y: *const i32,
    w: *const i32,
    h: *const i32,
    flag: i32,
) {
    let env = &*(env as *const CollisionEnv);
    let zb = zn_tile_flag_at_lanes(
        env.cache,
        env.cart,
        load_zn(x),
        load_zn(y),
        load_zn(w),
        load_zn(h),
        P8::from_raw(flag),
    );
    *out_mask = zb.val;
}

/// The kernel's third argument: the call-out function pointers plus the
/// collision environment. Field ORDER is load-bearing - the emitter reads
/// each pointer at a fixed byte offset (see `codegen::CTX_*`).
#[repr(C)]
pub struct AsmCtx {
    pub div: unsafe extern "C" fn(*mut i32, *const i32, *const i32),
    pub rem: unsafe extern "C" fn(*mut i32, *const i32, *const i32),
    pub sin: unsafe extern "C" fn(*mut i32, *const i32),
    pub mget: unsafe extern "C" fn(*const c_void, *mut i32, *const i32, *const i32),
    pub tile_flag:
        unsafe extern "C" fn(*const c_void, *mut u16, *const i32, *const i32, i32, i32, i32),
    pub tile_flag_lanes: unsafe extern "C" fn(
        *const c_void,
        *mut u16,
        *const i32,
        *const i32,
        *const i32,
        *const i32,
        i32,
    ),
    pub env: *const c_void,
}

impl AsmCtx {
    /// A context wired to the real primitives, with `env` pointing at a
    /// `CollisionEnv` the caller keeps alive.
    pub fn new(env: *const c_void) -> AsmCtx {
        AsmCtx {
            div: co_div,
            rem: co_rem,
            sin: co_sin,
            mget: co_mget,
            tile_flag: co_tile_flag,
            tile_flag_lanes: co_tile_flag_lanes,
            env,
        }
    }
}
