//! A float witness's ROBUSTNESS to the host libm (`rewrite robust`;
//! plans/float.md "The robustness report"): its first win under glibc's
//! `sin` (the definition) against MPFR's correctly rounded one and ulp
//! nudges (`lua_f64::SinNudge`). A variant whose first win differs is
//! FRAGILE: flagged, not rejected.

use std::os::raw::{c_char, c_int, c_long, c_void};
use std::sync::OnceLock;

use anyhow::{anyhow, Result};
use celeste_core::lua_f64::{with_sin_nudge, LuaF64, SinNudge};

use crate::settings::Settings;

#[link(name = "dl")]
extern "C" {
    fn dlopen(filename: *const c_char, flag: c_int) -> *mut c_void;
    fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
}

const RTLD_NOW: c_int = 2;

/// MPFR's `__mpfr_struct` (mpfr.h): precision, sign, exponent, limbs.
#[repr(C)]
struct Mpfr {
    prec: c_long,
    sign: c_int,
    exp: c_long,
    d: *mut c_void,
}

/// `MPFR_RNDN`: round to nearest, ties to even.
const RNDN: c_int = 0;

struct Lib {
    init2: unsafe extern "C" fn(*mut Mpfr, c_long),
    clear: unsafe extern "C" fn(*mut Mpfr),
    set_d: unsafe extern "C" fn(*mut Mpfr, f64, c_int) -> c_int,
    sin: unsafe extern "C" fn(*mut Mpfr, *const Mpfr, c_int) -> c_int,
    get_d: unsafe extern "C" fn(*const Mpfr, c_int) -> f64,
}

// SAFETY: plain function pointers into a library loaded once and never
// unloaded; MPFR's functions are thread-safe on distinct variables.
unsafe impl Send for Lib {}
unsafe impl Sync for Lib {}

/// MPFR, loaded once (`libmpfr.so.6`); an error where it is absent.
fn lib() -> Result<&'static Lib> {
    static LIB: OnceLock<std::result::Result<Lib, String>> = OnceLock::new();
    LIB.get_or_init(|| {
        let name = c"libmpfr.so.6";
        // SAFETY: dlopen of a system library by name; the handle is kept
        // for the process (never closed).
        let h = unsafe { dlopen(name.as_ptr(), RTLD_NOW) };
        if h.is_null() {
            return Err("libmpfr.so.6 is not loadable: no correctly rounded sin".into());
        }
        let sym = |s: &std::ffi::CStr| -> std::result::Result<*mut c_void, String> {
            // SAFETY: a symbol lookup in the handle above.
            let p = unsafe { dlsym(h, s.as_ptr()) };
            if p.is_null() {
                Err(format!("MPFR lacks {s:?}"))
            } else {
                Ok(p)
            }
        };
        // SAFETY: each symbol has the C signature mpfr.h declares for it.
        unsafe {
            Ok(Lib {
                init2: std::mem::transmute::<*mut c_void, unsafe extern "C" fn(*mut Mpfr, c_long)>(sym(c"mpfr_init2")?),
                clear: std::mem::transmute::<*mut c_void, unsafe extern "C" fn(*mut Mpfr)>(sym(c"mpfr_clear")?),
                set_d: std::mem::transmute::<*mut c_void, unsafe extern "C" fn(*mut Mpfr, f64, c_int) -> c_int>(sym(c"mpfr_set_d")?),
                sin: std::mem::transmute::<*mut c_void, unsafe extern "C" fn(*mut Mpfr, *const Mpfr, c_int) -> c_int>(sym(c"mpfr_sin")?),
                get_d: std::mem::transmute::<*mut c_void, unsafe extern "C" fn(*const Mpfr, c_int) -> f64>(sym(c"mpfr_get_d")?),
            })
        }
    })
    .as_ref()
    .map_err(|e| anyhow!("{e}"))
}

/// `sin(x)` correctly rounded to the nearest double (MPFR at 53 bits: its
/// result is the exactly rounded one). `lib()` must have loaded.
fn cr_sin(x: f64) -> f64 {
    let l = lib().expect("MPFR loaded before a correctly rounded sin is asked");
    let mut a = Mpfr { prec: 0, sign: 0, exp: 0, d: std::ptr::null_mut() };
    let mut r = Mpfr { prec: 0, sign: 0, exp: 0, d: std::ptr::null_mut() };
    // SAFETY: two variables initialized, used and cleared here; a double
    // converts exactly at 53 bits.
    unsafe {
        (l.init2)(&mut a, 53);
        (l.init2)(&mut r, 53);
        (l.set_d)(&mut a, x, RNDN);
        (l.sin)(&mut r, &a, RNDN);
        let v = (l.get_d)(&r, RNDN);
        (l.clear)(&mut a);
        (l.clear)(&mut r);
        v
    }
}

/// The report: each variant's name and the frame `inputs` first wins on
/// (`dual::first_win`, the run's float tool and seeds), glibc's first.
pub fn report(inputs: &[u8], s: &Settings, random: u64) -> Result<Vec<(String, Option<u32>)>> {
    lib()?;
    let run = |n: SinNudge| with_sin_nudge(n, || super::dual::first_win::<LuaF64>(inputs, s));
    let mut out = vec![("glibc".to_string(), run(SinNudge::Off)?)];
    out.push(("correctly rounded (MPFR)".to_string(), run(SinNudge::Replaced(cr_sin))?));
    for n in [SinNudge::Up, SinNudge::Down].into_iter().chain((1..=random).map(SinNudge::Random)) {
        out.push((format!("{n:?}"), run(n)?));
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    /// MPFR's sin agrees with glibc's on ordinary arguments, within an ulp,
    /// and is exactly 0 at 0.
    #[test]
    fn mpfr_sin_is_within_an_ulp_of_glibc() {
        if super::lib().is_err() {
            println!("SKIPPED: no libmpfr");
            return;
        }
        assert_eq!(super::cr_sin(0.0), 0.0);
        for i in 1..2000 {
            let x = i as f64 * 0.0137 - 9.0;
            let (c, g) = (super::cr_sin(x), x.sin());
            assert!(c == g || c == g.next_up() || c == g.next_down(), "x {x}: MPFR {c:e}, glibc {g:e}");
        }
    }
}
