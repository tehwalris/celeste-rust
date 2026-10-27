//! The call-out ABI: ops the emitter does not vectorise inline -
//! `Div`/`Rem`/`Sin` and the collision look-ups `Mget`/`TileFlagAt` - are a
//! `call` through a function pointer in `AsmCtx`, the kernel's third
//! argument. Operands are marshalled through a stack buffer and the wrappers
//! compute the arithmetic's own operation lane by lane (`Pico8Num`'s
//! saturating `/`, `%` and `sin`; `LuaF64::lua_sin`; the map), so the result
//! is the scalar operation's by construction. `llc` keeps what is live
//! across the call (every vector and mask register is the caller's to save).
//!
//! The doubles' kernels (8 `f64` lanes) call their own `sin` (the tools'
//! `LuaF64::lua_sin`, glibc's, lane by lane); the collision look-ups are
//! one generic call-out per arithmetic's lanes (`CallLanes`), on whole-number
//! coordinates and a box the lowering made whole.

use std::os::raw::c_void;

use celeste_core::cart_data::CART;
use celeste_core::room::Room;
use celeste_core::lua_f64::LuaF64;
use celeste_core::pico8_num::Pico8Num as P8;

/// 16.16's lanes a call-out reads: one zmm of `i32`s.
const LANES: usize = 16;

/// Lane `i` of a 16.16 column.
#[inline]
unsafe fn num(p: *const i32, i: usize) -> P8 {
    P8::from_raw(*p.add(i))
}

/// A whole number of a 16.16 lane (`CallLanes::whole_lane`).
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

/// The room the collision wrappers query (the map is `CART`'s). Held
/// behind `AsmCtx::env`.
#[repr(C)]
pub struct CollisionEnv<'a> {
    pub room: &'a Room,
}

impl CollisionEnv<'_> {
    /// `tile_flag_at` with one box for every lane: the box's precomputed
    /// map where there is one (`Room::map`), picked once, else the scan
    /// (`Room::flag_at`). Bit `l` of the mask is lane `l`'s answer.
    fn tile_flags(&self, w: i16, h: i16, flag: i16, lanes: impl Iterator<Item = (i16, i16)>) -> u16 {
        let map = self.room.map(w, h, flag);
        let scan = |x: i16, y: i16| self.room.flag_at(x, y, w, h, flag).expect("tile_flag_at");
        lanes.enumerate().fold(0u16, |m, (l, (x, y))| {
            let hit = map.and_then(|map| map.get(x, y)).unwrap_or_else(|| scan(x, y));
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

/// The tools' `sin`, lane by lane (`LuaF64::lua_sin`). A lane whose
/// argument is not finite is in error already (its operation's own); it
/// gets NaN, which nothing reads.
pub unsafe extern "C" fn co_fsin(out: *mut f64, a: *const f64) {
    for i in 0..8 {
        let r = LuaF64::new(*a.add(i)).and_then(LuaF64::lua_sin);
        *out.add(i) = r.map_or(f64::NAN, LuaF64::value);
    }
}

/// `mget`, per lane: 0 outside the map (as PICO-8; a branch-free kernel
/// reads there on lanes that do not take the iteration).
pub unsafe extern "C" fn co_mget<L: CallLanes>(_env: *const c_void, out: *mut L::Lane, x: *const L::Lane, y: *const L::Lane) {
    for i in 0..L::N {
        *out.add(i) = L::tile(CART.mget_whole(L::whole_lane(x, i, "mget: x"), L::whole_lane(y, i, "mget: y")));
    }
}

/// `tile_flag_at` with a UNIFORM box (`w`/`h`/`flag` whole numbers, the
/// lowering's): writes the lanes' `val` mask (known is all-ones).
pub unsafe extern "C" fn co_tile_flag<L: CallLanes>(env: *const c_void, out_mask: *mut L::Mask, x: *const L::Lane, y: *const L::Lane, w: i32, h: i32, flag: i32) {
    let env = &*(env as *const CollisionEnv);
    let lanes = (0..L::N).map(|i| (L::whole_lane(x, i, "tile_flag_at: x"), L::whole_lane(y, i, "tile_flag_at: y")));
    *out_mask = L::mask(env.tile_flags(w as i16, h as i16, flag as i16, lanes));
}

/// `tile_flag_at` with a PER-LANE box (`w`/`h` are columns; in the frame an
/// object is created its hitbox depends on whether `type.init` replaced the
/// default 8x8): always the scan, the precomputed maps being keyed by the box.
pub unsafe extern "C" fn co_tile_flag_lanes<L: CallLanes>(env: *const c_void, out_mask: *mut L::Mask, x: *const L::Lane, y: *const L::Lane, w: *const L::Lane, h: *const L::Lane, flag: i32) {
    let env = &*(env as *const CollisionEnv);
    let mut m = 0u16;
    for i in 0..L::N {
        let (xi, yi) = (L::whole_lane(x, i, "tile_flag_at: x"), L::whole_lane(y, i, "tile_flag_at: y"));
        let (wi, hi) = (L::whole_lane(w, i, "tile_flag_at: w"), L::whole_lane(h, i, "tile_flag_at: h"));
        if env.room.flag_at(xi, yi, wi, hi, flag as i16).expect("tile_flag_at") {
            m |= 1 << i;
        }
    }
    *out_mask = L::mask(m);
}

/// An arithmetic's call-out lanes: its kernel's lanes a column holds (16.16
/// sixteen `i32`s, the doubles eight `f64`s a half), their mask, and its
/// own call-outs.
pub trait CallLanes: Sized {
    type Lane: Copy;
    type Mask: Copy;
    const N: usize;
    /// `/` and `%`: 16.16's call-outs; the doubles compute theirs inline.
    const DIV: Option<unsafe extern "C" fn(*mut Self::Lane, *const Self::Lane, *const Self::Lane)>;
    const REM: Option<unsafe extern "C" fn(*mut Self::Lane, *const Self::Lane, *const Self::Lane)>;
    const SIN: unsafe extern "C" fn(*mut Self::Lane, *const Self::Lane);
    /// Lane `i` of a column as a whole number (a coordinate, a box): the
    /// traced cart never makes a fraction where one is read; FATAL if it did.
    unsafe fn whole_lane(p: *const Self::Lane, i: usize, what: &str) -> i16;
    /// A tile number (`mget`'s result) as a lane.
    fn tile(t: u8) -> Self::Lane;
    /// The lanes' 16-bit answer as the kernel's mask.
    fn mask(m: u16) -> Self::Mask;
}

impl CallLanes for P8 {
    type Lane = i32;
    type Mask = u16;
    const N: usize = LANES;
    const DIV: Option<unsafe extern "C" fn(*mut i32, *const i32, *const i32)> = Some(co_div);
    const REM: Option<unsafe extern "C" fn(*mut i32, *const i32, *const i32)> = Some(co_rem);
    const SIN: unsafe extern "C" fn(*mut i32, *const i32) = co_sin;
    #[inline]
    unsafe fn whole_lane(p: *const i32, i: usize, what: &str) -> i16 {
        whole16(num(p, i), what)
    }
    fn tile(t: u8) -> i32 {
        P8::from_i16(t as i16).as_raw_u32() as i32
    }
    fn mask(m: u16) -> u16 {
        m
    }
}

impl CallLanes for LuaF64 {
    type Lane = f64;
    type Mask = u8;
    const N: usize = 8;
    const DIV: Option<unsafe extern "C" fn(*mut f64, *const f64, *const f64)> = None;
    const REM: Option<unsafe extern "C" fn(*mut f64, *const f64, *const f64)> = None;
    const SIN: unsafe extern "C" fn(*mut f64, *const f64) = co_fsin;
    /// `LuaF64::as_i16`, as 16.16's require of theirs.
    #[inline]
    unsafe fn whole_lane(p: *const f64, i: usize, what: &str) -> i16 {
        let v = *p.add(i);
        match LuaF64::new(v).and_then(LuaF64::as_i16) {
            Some(n) => n,
            None => not_whole(v, i, what),
        }
    }
    fn tile(t: u8) -> f64 {
        t as f64
    }
    fn mask(m: u16) -> u8 {
        m as u8
    }
}

/// `CallLanes::whole_lane`'s panic for the doubles, out of line (`not_whole16`).
#[cold]
#[inline(never)]
fn not_whole(v: f64, i: usize, what: &str) -> ! {
    panic!("{what}: lane {i} holds {v}, not a whole number")
}

/// The kernel's third argument: the call-out function pointers plus the
/// collision environment, one layout for both arithmetics. Field ORDER is
/// load-bearing - the emitter reads each pointer at a fixed byte offset
/// (`llvm::CTX_*`).
#[repr(C)]
pub struct AsmCtx<L: CallLanes> {
    pub div: Option<unsafe extern "C" fn(*mut L::Lane, *const L::Lane, *const L::Lane)>,
    pub rem: Option<unsafe extern "C" fn(*mut L::Lane, *const L::Lane, *const L::Lane)>,
    pub sin: unsafe extern "C" fn(*mut L::Lane, *const L::Lane),
    pub mget: unsafe extern "C" fn(*const c_void, *mut L::Lane, *const L::Lane, *const L::Lane),
    pub tile_flag: unsafe extern "C" fn(*const c_void, *mut L::Mask, *const L::Lane, *const L::Lane, i32, i32, i32),
    pub tile_flag_lanes: unsafe extern "C" fn(*const c_void, *mut L::Mask, *const L::Lane, *const L::Lane, *const L::Lane, *const L::Lane, i32),
    pub env: *const c_void,
}

impl<L: CallLanes> AsmCtx<L> {
    /// A context wired to the arithmetic's primitives, with `env` pointing
    /// at a `CollisionEnv` the caller keeps alive.
    pub fn new(env: *const c_void) -> AsmCtx<L> {
        AsmCtx { div: L::DIV, rem: L::REM, sin: L::SIN, mget: co_mget::<L>, tile_flag: co_tile_flag::<L>, tile_flag_lanes: co_tile_flag_lanes::<L>, env }
    }
}
