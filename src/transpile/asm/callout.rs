//! The call-out ABI: ops the emitter does not vectorise inline -
//! `Div`/`Rem`/`Sin` and the collision look-ups `Mget`/`TileFlagAt` - are a
//! `call` through a function pointer in `AsmCtx`, the kernel's third
//! argument. Operands are marshalled through a stack buffer and the wrappers
//! invoke the exact `celeste_engine::kernel` primitive, so the result is
//! bit-identical by construction. The emitted code saves and restores all
//! 32 zmm around the call.
//!
//! The doubles' kernels (8 `f64` lanes) call their own: the tools' `sin`
//! (`LuaF64::lua_sin`, glibc's `sin`, lane by lane) and the collision
//! look-ups on whole-number coordinates.

use std::os::raw::c_void;

use celeste_core::cart_data::CartData;
use celeste_core::collision_cache::CollisionCache;
use celeste_core::lua_f64::LuaF64;
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

/// The doubles' lanes a call-out reads.
const F_LANES: usize = 8;

/// A whole-number coordinate of the doubles (`LuaF64::as_i16`), as the
/// 16.16 call-outs require of theirs.
#[inline]
unsafe fn whole(p: *const f64, i: usize, what: &str) -> i16 {
    LuaF64::new(*p.add(i)).and_then(LuaF64::as_i16).unwrap_or_else(|| panic!("{what}: lane {i} holds {}, not a whole number", *p.add(i)))
}

/// The tools' `sin`, lane by lane (`LuaF64::lua_sin`). A lane whose
/// argument is not finite is in error already (its operation's own); it
/// gets NaN, which nothing reads.
pub unsafe extern "C" fn co_fsin(out: *mut f64, a: *const f64) {
    for i in 0..F_LANES {
        let r = LuaF64::new(*a.add(i)).and_then(LuaF64::lua_sin);
        *out.add(i) = r.map_or(f64::NAN, LuaF64::value);
    }
}
pub unsafe extern "C" fn co_fmget(env: *const c_void, out: *mut f64, x: *const f64, y: *const f64) {
    let env = &*(env as *const CollisionEnv);
    for i in 0..F_LANES {
        *out.add(i) = env.cart.mget_whole(whole(x, i, "mget x"), whole(y, i, "mget y")) as f64;
    }
}
/// `tile_flag_at` with a UNIFORM box (`w`/`h`/`flag` whole numbers):
/// `co_tile_flag`'s, on whole-number coordinates. Writes the 8-lane mask.
pub unsafe extern "C" fn co_ftile_flag(env: *const c_void, out_mask: *mut u8, x: *const f64, y: *const f64, w: i32, h: i32, flag: i32) {
    let env = &*(env as *const CollisionEnv);
    let (xs, ys): ([i16; 16], [i16; 16]) = (std::array::from_fn(|i| if i < F_LANES { whole(x, i, "tile_flag_at x") } else { 0 }), std::array::from_fn(|i| if i < F_LANES { whole(y, i, "tile_flag_at y") } else { 0 }));
    let (xz, yz) = (ZN::from_array(xs.map(P8::from_i16)), ZN::from_array(ys.map(P8::from_i16)));
    let zb = zn_tile_flag_at(env.cache, env.cart, xz, yz, P8::from_i16(w as i16), P8::from_i16(h as i16), P8::from_i16(flag as i16));
    *out_mask = (zb.val & 0xff) as u8;
}
/// `tile_flag_at` with a PER-LANE box.
pub unsafe extern "C" fn co_ftile_flag_lanes(env: *const c_void, out_mask: *mut u8, x: *const f64, y: *const f64, w: *const f64, h: *const f64, flag: i32) {
    let env = &*(env as *const CollisionEnv);
    let mut m = 0u8;
    for i in 0..F_LANES {
        let (xi, yi) = (whole(x, i, "tile_flag_at x"), whole(y, i, "tile_flag_at y"));
        let (wi, hi) = (whole(w, i, "tile_flag_at w"), whole(h, i, "tile_flag_at h"));
        if env.cache.flag_at(env.cart, xi, yi, wi, hi, flag as i16).expect("tile_flag_at") {
            m |= 1 << i;
        }
    }
    *out_mask = m;
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
    // The doubles' (`codegen::CTX_F*`), after `env`.
    pub fsin: unsafe extern "C" fn(*mut f64, *const f64),
    pub fmget: unsafe extern "C" fn(*const c_void, *mut f64, *const f64, *const f64),
    pub ftile_flag: unsafe extern "C" fn(*const c_void, *mut u8, *const f64, *const f64, i32, i32, i32),
    pub ftile_flag_lanes: unsafe extern "C" fn(*const c_void, *mut u8, *const f64, *const f64, *const f64, *const f64, i32),
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
            fsin: co_fsin,
            fmget: co_fmget,
            ftile_flag: co_ftile_flag,
            ftile_flag_lanes: co_ftile_flag_lanes,
        }
    }
}
