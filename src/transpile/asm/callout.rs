//! The call-out ABI: ops the emitter does not vectorise inline -
//! `Div`/`Rem`/`Sin` and the collision look-ups `Mget`/`TileFlagAt` - are a
//! `call` through a function pointer in `AsmCtx`, the kernel's third
//! argument. Operands are marshalled through a stack buffer and the wrappers
//! compute the arithmetic's own operation lane by lane (`Pico8Num`'s
//! saturating `/`, `%` and `sin`; `LuaF64::lua_sin`; the map), so the result
//! is the scalar operation's by construction. The emitted code saves and
//! restores the registers live across the call (`codegen::call_saves`).
//!
//! The doubles' kernels (8 `f64` lanes) call their own: the tools' `sin`
//! (`LuaF64::lua_sin`, glibc's `sin`, lane by lane) and the collision
//! look-ups on whole-number coordinates.

use std::os::raw::c_void;

use celeste_core::cart_data::CART;
use celeste_core::collision_cache::CollisionCache;
use celeste_core::lua_f64::LuaF64;
use celeste_core::pico8_num::Pico8Num as P8;

/// 16.16's lanes a call-out reads: one zmm of `i32`s.
const LANES: usize = 16;

/// Lane `i` of a 16.16 column.
#[inline]
unsafe fn num(p: *const i32, i: usize) -> P8 {
    P8::from_raw(*p.add(i))
}

/// A whole number of a 16.16 lane (the traced cart never makes a fraction
/// where one is read: a coordinate, a box, a flag).
#[inline]
fn whole16(v: P8, what: &str) -> i16 {
    match v.as_i16() {
        Some(n) => n,
        None => not_whole16(v, what),
    }
}

/// `whole16`'s panic, out of line and by value: a panic closure capturing
/// the message's arguments made every lane store them on the hot path
/// (`co_tile_flag`: 0.4% of the r62h bench frame's instructions, 2.3% of
/// f42n's with `whole`'s).
#[cold]
#[inline(never)]
fn not_whole16(v: P8, what: &str) -> ! {
    panic!("{what}: {v:?} is not an integer")
}

/// The room's collision cache the collision wrappers query (the map is
/// `CART`'s). Held behind `AsmCtx::env`.
#[repr(C)]
pub struct CollisionEnv<'a> {
    pub cache: &'a CollisionCache,
}

impl CollisionEnv<'_> {
    /// `tile_flag_at` with one box for every lane: the precomputed map for
    /// the flag and box where there is one (`CollisionCache::flag_map`),
    /// else the scan. Bit `l` of the mask is lane `l`'s answer.
    fn tile_flags(&self, w: i16, h: i16, flag: i16, lanes: impl Iterator<Item = (i16, i16)>) -> u16 {
        let map = self.cache.flag_map(w, h, flag);
        let scan = |x: i16, y: i16| self.cache.flag_at(x, y, w, h, flag).expect("tile_flag_at");
        lanes.enumerate().fold(0u16, |m, (l, (x, y))| {
            let hit = match &map {
                Some((map, dx, dy)) => map.get(x + dx, y + dy).unwrap_or_else(|| scan(x, y)),
                None => scan(x, y),
            };
            m | (hit as u16) << l
        })
    }
}

pub unsafe extern "C" fn co_div(out: *mut i32, a: *const i32, b: *const i32) {
    for i in 0..LANES {
        *out.add(i) = num(a, i).saturating_div(num(b, i)).as_raw_u32() as i32;
    }
}
pub unsafe extern "C" fn co_rem(out: *mut i32, a: *const i32, b: *const i32) {
    for i in 0..LANES {
        *out.add(i) = (num(a, i) % num(b, i)).as_raw_u32() as i32;
    }
}
pub unsafe extern "C" fn co_sin(out: *mut i32, a: *const i32) {
    for i in 0..LANES {
        *out.add(i) = num(a, i).pico8_sin().as_raw_u32() as i32;
    }
}
/// `mget`, per lane: 0 outside the map (as PICO-8; a branch-free kernel
/// reads there on lanes that do not take the iteration).
pub unsafe extern "C" fn co_mget(_env: *const c_void, out: *mut i32, x: *const i32, y: *const i32) {
    for i in 0..LANES {
        let tile = CART.mget_whole(whole16(num(x, i), "mget: x"), whole16(num(y, i), "mget: y"));
        *out.add(i) = P8::from_i16(tile as i16).as_raw_u32() as i32;
    }
}
/// `tile_flag_at` with a UNIFORM box (`w`/`h`/`flag` raw 16.16 scalars).
/// Writes the 16-bit `val` mask to `*out_mask` (known is all-ones).
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
    let (w, h, flag) = (whole16(P8::from_raw(w), "tile_flag_at: w"), whole16(P8::from_raw(h), "tile_flag_at: h"), whole16(P8::from_raw(flag), "tile_flag_at: flag"));
    let lanes = (0..LANES).map(|i| (whole16(num(x, i), "tile_flag_at: x"), whole16(num(y, i), "tile_flag_at: y")));
    *out_mask = env.tile_flags(w, h, flag, lanes);
}
/// `tile_flag_at` with a PER-LANE box (`w`/`h` are columns; in the frame an
/// object is created its hitbox depends on whether `type.init` replaced the
/// default 8x8): always the scan, the precomputed maps being keyed by the box.
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
    let flag = whole16(P8::from_raw(flag), "tile_flag_at: flag");
    let mut m = 0u16;
    for i in 0..LANES {
        let (xi, yi) = (whole16(num(x, i), "tile_flag_at: x"), whole16(num(y, i), "tile_flag_at: y"));
        let (wi, hi) = (whole16(num(w, i), "tile_flag_at: w"), whole16(num(h, i), "tile_flag_at: h"));
        if env.cache.flag_at(xi, yi, wi, hi, flag).expect("tile_flag_at") {
            m |= 1 << i;
        }
    }
    *out_mask = m;
}

/// The doubles' lanes a call-out reads.
const F_LANES: usize = 8;

/// A whole-number coordinate of the doubles (`LuaF64::as_i16`), as the
/// 16.16 call-outs require of theirs.
#[inline]
unsafe fn whole(p: *const f64, i: usize, what: &str) -> i16 {
    let v = *p.add(i);
    match LuaF64::new(v).and_then(LuaF64::as_i16) {
        Some(n) => n,
        None => not_whole(v, i, what),
    }
}

/// `whole`'s panic, out of line (`not_whole16`).
#[cold]
#[inline(never)]
fn not_whole(v: f64, i: usize, what: &str) -> ! {
    panic!("{what}: lane {i} holds {v}, not a whole number")
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
pub unsafe extern "C" fn co_fmget(_env: *const c_void, out: *mut f64, x: *const f64, y: *const f64) {
    for i in 0..F_LANES {
        *out.add(i) = CART.mget_whole(whole(x, i, "mget x"), whole(y, i, "mget y")) as f64;
    }
}
/// `tile_flag_at` with a UNIFORM box (`w`/`h`/`flag` whole numbers):
/// `co_tile_flag`'s, on whole-number coordinates. Writes the 8-lane mask.
pub unsafe extern "C" fn co_ftile_flag(env: *const c_void, out_mask: *mut u8, x: *const f64, y: *const f64, w: i32, h: i32, flag: i32) {
    let env = &*(env as *const CollisionEnv);
    let lanes = (0..F_LANES).map(|i| (whole(x, i, "tile_flag_at x"), whole(y, i, "tile_flag_at y")));
    *out_mask = env.tile_flags(w as i16, h as i16, flag as i16, lanes) as u8;
}
/// `tile_flag_at` with a PER-LANE box.
pub unsafe extern "C" fn co_ftile_flag_lanes(env: *const c_void, out_mask: *mut u8, x: *const f64, y: *const f64, w: *const f64, h: *const f64, flag: i32) {
    let env = &*(env as *const CollisionEnv);
    let mut m = 0u8;
    for i in 0..F_LANES {
        let (xi, yi) = (whole(x, i, "tile_flag_at x"), whole(y, i, "tile_flag_at y"));
        let (wi, hi) = (whole(w, i, "tile_flag_at w"), whole(h, i, "tile_flag_at h"));
        if env.cache.flag_at(xi, yi, wi, hi, flag as i16).expect("tile_flag_at") {
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
