//! A kernel's LANES as its buffers hold them, per arithmetic (`KernelLanes`):
//! 16.16's sixteen `i32` lanes a 64-byte slot; the doubles' eight `f64`
//! lanes a 64-byte slot in each of two HALVES (`Compiled::halves`: lanes
//! 8..16 one buffer on, `half` bytes past lanes 0..8). A boolean slot is a
//! `(val, known)` pair of lane masks at +0 and +2: 16.16's 16-bit, the
//! doubles' 8-bit a half. Everything the runtime reads or writes of a
//! kernel buffer goes through here, so the slices, the emission and the
//! sink see sixteen lanes of `N::Raw` whatever the arithmetic.

use std::cell::RefCell;
use std::thread::LocalKey;

use celeste_core::arith::Arithmetic;
use celeste_core::lua_f64::LuaF64;
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::runtime2::POS_WHOLE;

use crate::transpile::garith::GraphArith;

/// The lanes of an arithmetic the kernels compute in (one the assembler
/// compiles).
pub trait KernelLanes: crate::search::rem::Rem + GraphArith + crate::transpile::asm::CallLanes {
    /// The 16 lanes of the value slot at byte `off` (`half`: the halves'
    /// distance in this buffer; 16.16 has none).
    fn col(buf: &[u8], half: usize, off: usize) -> [Self::Raw; 16];
    fn lane(buf: &[u8], half: usize, off: usize, l: usize) -> Self::Raw;
    fn put(buf: &mut [u8], half: usize, off: usize, l: usize, r: Self::Raw);
    /// A boolean slot's 16-lane `(val, known)` masks.
    fn masks(buf: &[u8], half: usize, off: usize) -> (u16, u16);
    /// Write a boolean slot's masks (`known` only where `ubool`).
    fn put_masks(buf: &mut [u8], half: usize, off: usize, val: u16, known: Option<u16>);
    fn eq_lanes(c: &[Self::Raw; 16], v: Self::Raw) -> u16;
    /// The lanes where the value slots `a` and `b` are equal.
    fn eq_slots(buf: &[u8], half: usize, a: usize, b: usize) -> u16;
    /// The lanes of a small dictionary's number word `w` (`(lo, hi)` raws).
    fn eq_word(lo: &[Self::Raw; 16], hi: &[Self::Raw; 16], w: Self::Word) -> u16;
    /// A position's code words (`exact::Code::of_pos`): the value less its
    /// low end's whole pixels.
    fn pos_words(lo: Self::Raw, hi: Self::Raw) -> (Self::Raw, Self::Raw);
    /// A position raw's whole pixels (`cells::block_cells`' reading).
    fn whole(r: Self::Raw) -> i16;
    /// This arithmetic's per-thread scratch pools (one static each).
    fn scratch() -> &'static LocalKey<RefCell<Vec<super::Scratch<Self>>>>;
    /// This arithmetic's resident kernel set (`registry`).
    fn resident() -> &'static std::sync::Mutex<super::Resident<Self>>;
    /// Before every call of a kernel of this arithmetic: the doubles check
    /// the floating-point environment (`asm::check_mxcsr`), FATAL if wrong.
    fn before_call();
    /// The kernel census (`compiled::mix`: `bench frame --metrics`),
    /// 16.16's diagnostic; another arithmetic refuses it.
    fn mix_report(inp: &crate::compiled::mix::Input<'_, Self>) -> anyhow::Result<crate::compiled::mix::Census>;
}

thread_local! {
    static SCRATCH_FIXED: RefCell<Vec<super::Scratch<P8>>> = const { RefCell::new(Vec::new()) };
    static SCRATCH_FLOAT: RefCell<Vec<super::Scratch<LuaF64>>> = const { RefCell::new(Vec::new()) };
}
static RESIDENT_FIXED: std::sync::Mutex<super::Resident<P8>> = std::sync::Mutex::new(None);
static RESIDENT_FLOAT: std::sync::Mutex<super::Resident<LuaF64>> = std::sync::Mutex::new(None);

// The kernels are AVX-512, and so are the lane compares below.
#[cfg(not(target_feature = "avx512f"))]
compile_error!(
    "the kernels need AVX-512F/DQ. The workspace's .cargo/config.toml passes \
     `-C target-cpu=native`; on an older machine add \
     `-C target-feature=+avx512f,+avx512dq`."
);

/// The 16 lanes of the 32-bit column at byte `o`.
#[inline]
fn col32(buf: &[u8], o: usize) -> [u32; 16] {
    let b = &buf[o..o + 64];
    // SAFETY: 64 readable bytes (the slice above), read as 16 words.
    unsafe { std::ptr::read_unaligned(b.as_ptr().cast::<[u32; 16]>()) }.map(u32::from_le)
}

/// The lanes where `c` holds `v` (one AVX-512 compare: every build has it,
/// above).
#[inline]
fn eq32(c: &[u32; 16], v: u32) -> u16 {
    use core::arch::x86_64::{_mm512_cmpeq_epi32_mask, _mm512_loadu_si512, _mm512_set1_epi32};
    // SAFETY: `c` is 64 readable bytes; AVX-512F is enabled in every build.
    unsafe { _mm512_cmpeq_epi32_mask(_mm512_loadu_si512(c.as_ptr().cast()), _mm512_set1_epi32(v as i32)) }
}

/// The lanes where two columns of 16 words (64 bytes each) are equal.
#[inline]
pub(super) fn eq16(a: &[u8], b: &[u8]) -> u16 {
    use core::arch::x86_64::{_mm512_cmpeq_epi32_mask, _mm512_loadu_si512};
    assert!(a.len() >= 64 && b.len() >= 64, "a column of 16 words");
    // SAFETY: both hold 64 readable bytes (checked); AVX-512F is enabled in
    // every build.
    unsafe { _mm512_cmpeq_epi32_mask(_mm512_loadu_si512(a.as_ptr().cast()), _mm512_loadu_si512(b.as_ptr().cast())) }
}

impl KernelLanes for P8 {
    #[inline]
    fn col(buf: &[u8], _half: usize, off: usize) -> [u32; 16] {
        col32(buf, off)
    }
    #[inline]
    fn lane(buf: &[u8], _half: usize, off: usize, l: usize) -> u32 {
        u32::from_le_bytes(buf[off + 4 * l..off + 4 * l + 4].try_into().expect("4 bytes"))
    }
    #[inline]
    fn put(buf: &mut [u8], _half: usize, off: usize, l: usize, r: u32) {
        buf[off + 4 * l..off + 4 * l + 4].copy_from_slice(&r.to_le_bytes());
    }
    #[inline]
    fn masks(buf: &[u8], _half: usize, o: usize) -> (u16, u16) {
        (u16::from_le_bytes([buf[o], buf[o + 1]]), u16::from_le_bytes([buf[o + 2], buf[o + 3]]))
    }
    #[inline]
    fn put_masks(buf: &mut [u8], _half: usize, off: usize, val: u16, known: Option<u16>) {
        buf[off..off + 2].copy_from_slice(&val.to_le_bytes());
        if let Some(k) = known {
            buf[off + 2..off + 4].copy_from_slice(&k.to_le_bytes());
        }
    }
    #[inline]
    fn eq_lanes(c: &[u32; 16], v: u32) -> u16 {
        eq32(c, v)
    }
    #[inline]
    fn eq_slots(buf: &[u8], _half: usize, a: usize, b: usize) -> u16 {
        eq16(&buf[a..a + 64], &buf[b..b + 64])
    }
    #[inline]
    fn eq_word(lo: &[u32; 16], hi: &[u32; 16], w: u64) -> u16 {
        eq32(lo, (w >> 32) as u32) & eq32(hi, w as u32)
    }
    #[inline]
    fn pos_words(lo: u32, hi: u32) -> (u32, u32) {
        (lo & !POS_WHOLE, hi.wrapping_sub(lo & POS_WHOLE))
    }
    #[inline]
    fn whole(r: u32) -> i16 {
        (r as i32 >> 16) as i16
    }
    fn scratch() -> &'static LocalKey<RefCell<Vec<super::Scratch<P8>>>> {
        &SCRATCH_FIXED
    }
    fn resident() -> &'static std::sync::Mutex<super::Resident<P8>> {
        &RESIDENT_FIXED
    }
    #[inline]
    fn before_call() {}
    fn mix_report(inp: &crate::compiled::mix::Input<'_, celeste_core::arith::Fixed>) -> anyhow::Result<crate::compiled::mix::Census> {
        crate::compiled::mix::report(inp)
    }
}

/// Where lane `l` of a doubles' slot at `off` lives.
#[inline]
fn at64(half: usize, off: usize, l: usize) -> usize {
    (l / 8) * half + off + 8 * (l % 8)
}

impl KernelLanes for LuaF64 {
    #[inline]
    fn col(buf: &[u8], half: usize, off: usize) -> [u64; 16] {
        std::array::from_fn(|l| Self::lane(buf, half, off, l))
    }
    #[inline]
    fn lane(buf: &[u8], half: usize, off: usize, l: usize) -> u64 {
        let at = at64(half, off, l);
        u64::from_le_bytes(buf[at..at + 8].try_into().expect("8 bytes"))
    }
    #[inline]
    fn put(buf: &mut [u8], half: usize, off: usize, l: usize, r: u64) {
        let at = at64(half, off, l);
        buf[at..at + 8].copy_from_slice(&r.to_le_bytes());
    }
    #[inline]
    fn masks(buf: &[u8], half: usize, o: usize) -> (u16, u16) {
        let m = |k: usize| buf[o + k] as u16 | (buf[half + o + k] as u16) << 8;
        (m(0), m(2))
    }
    #[inline]
    fn put_masks(buf: &mut [u8], half: usize, off: usize, val: u16, known: Option<u16>) {
        buf[off] = val as u8;
        buf[half + off] = (val >> 8) as u8;
        if let Some(k) = known {
            buf[off + 2] = k as u8;
            buf[half + off + 2] = (k >> 8) as u8;
        }
    }
    #[inline]
    fn eq_lanes(c: &[u64; 16], v: u64) -> u16 {
        c.iter().enumerate().fold(0u16, |m, (l, x)| m | ((*x == v) as u16) << l)
    }
    #[inline]
    fn eq_slots(buf: &[u8], half: usize, a: usize, b: usize) -> u16 {
        (0..16).fold(0u16, |m, l| m | ((Self::lane(buf, half, a, l) == Self::lane(buf, half, b, l)) as u16) << l)
    }
    #[inline]
    fn eq_word(lo: &[u64; 16], hi: &[u64; 16], w: u128) -> u16 {
        (0..16).fold(0u16, |m, l| m | ((LuaF64::word(lo[l], hi[l]) == w) as u16) << l)
    }
    #[inline]
    fn pos_words(lo: u64, hi: u64) -> (u64, u64) {
        let num = |r: u64| LuaF64::from_raw(r).unwrap_or_else(|| panic!("a position coordinate {r:#018x} is not a finite double"));
        let w = Self::whole(lo);
        (num(lo).less_whole(w), num(hi).less_whole(w))
    }
    #[inline]
    fn whole(r: u64) -> i16 {
        LuaF64::from_raw(r).and_then(LuaF64::whole_i16).unwrap_or_else(|| panic!("a position coordinate {r:#018x} past the i16 pixels"))
    }
    fn scratch() -> &'static LocalKey<RefCell<Vec<super::Scratch<LuaF64>>>> {
        &SCRATCH_FLOAT
    }
    fn resident() -> &'static std::sync::Mutex<super::Resident<LuaF64>> {
        &RESIDENT_FLOAT
    }
    #[inline]
    fn before_call() {
        crate::transpile::asm::check_mxcsr().unwrap_or_else(|e| panic!("a float kernel call: {e:#}"));
    }
    fn mix_report(_: &crate::compiled::mix::Input<'_, celeste_core::arith::Float>) -> anyhow::Result<crate::compiled::mix::Census> {
        anyhow::bail!("the kernel census (bench frame --metrics) is 16.16's: not under float arithmetic")
    }
}
