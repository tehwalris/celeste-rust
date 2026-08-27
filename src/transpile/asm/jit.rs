//! Assemble a GAS `.s` string with `gcc` and load it with `dlopen`.
//!
//! The prototype backend emits native AVX-512 text (see `codegen`); this
//! turns that text into a callable function. `as` assembles AVX-512
//! natively (a JIT crate would have to hand-encode EVEX), and `objdump`
//! on the `.so` gives the instruction mix for free. See
//! `plans/asm-backend.md` for why this mechanism was chosen.

use std::ffi::CString;
use std::os::raw::{c_char, c_int, c_void};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use anyhow::{bail, Result};

#[link(name = "dl")]
extern "C" {
    fn dlopen(filename: *const c_char, flag: c_int) -> *mut c_void;
    fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
    fn dlclose(handle: *mut c_void) -> c_int;
}

const RTLD_NOW: c_int = 2;

/// The ABI every emitted kernel has: read packed input columns, write
/// packed output columns. See `codegen::Compiled` for the layouts.
pub type KernelFn = unsafe extern "C" fn(*const u8, *mut u8, *const std::os::raw::c_void);

static COUNTER: AtomicU64 = AtomicU64::new(0);

/// A scratch directory for the `.s` / `.so` pair. Inside `target/` so it
/// is ignored and cleaned by `cargo clean`.
fn scratch_dir() -> PathBuf {
    let mut p = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.push("target");
    p.push("asm-scratch");
    let _ = std::fs::create_dir_all(&p);
    p
}

fn unique_stem(tag: &str) -> String {
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    let pid = std::process::id();
    format!("k_{}_{}_{}", tag, pid, n)
}

/// Write `asm` to a `.s` file and assemble it into a shared object with
/// `gcc -shared -fPIC`. Returns the `.so` path. Timed separately from
/// emission by the benchmarks.
pub fn assemble(asm: &str, tag: &str) -> Result<PathBuf> {
    let dir = scratch_dir();
    let stem = unique_stem(tag);
    let src = dir.join(format!("{stem}.s"));
    let obj = dir.join(format!("{stem}.so"));
    std::fs::write(&src, asm)?;
    let out = std::process::Command::new("gcc")
        .arg("-shared")
        .arg("-fPIC")
        .arg("-o")
        .arg(&obj)
        .arg(&src)
        .output()?;
    if !out.status.success() {
        bail!(
            "gcc failed to assemble {}:\n{}",
            src.display(),
            String::from_utf8_lossy(&out.stderr)
        );
    }
    Ok(obj)
}

/// A loaded shared object plus the resolved symbol. `dlclose` on drop.
pub struct Loaded {
    handle: *mut c_void,
    path: PathBuf,
    pub func: KernelFn,
}

impl Loaded {
    /// `dlopen` `path` and resolve `sym` to a `KernelFn`.
    pub fn open(path: &Path, sym: &str) -> Result<Loaded> {
        let cpath = CString::new(path.to_str().unwrap())?;
        let handle = unsafe { dlopen(cpath.as_ptr(), RTLD_NOW) };
        if handle.is_null() {
            bail!("dlopen({}) failed", path.display());
        }
        let csym = CString::new(sym)?;
        let addr = unsafe { dlsym(handle, csym.as_ptr()) };
        if addr.is_null() {
            unsafe { dlclose(handle) };
            bail!("dlsym({}) not found", sym);
        }
        let func: KernelFn = unsafe { std::mem::transmute(addr) };
        Ok(Loaded {
            handle,
            path: path.to_path_buf(),
            func,
        })
    }
}

impl Drop for Loaded {
    fn drop(&mut self) {
        unsafe {
            dlclose(self.handle);
        }
        let _ = std::fs::remove_file(&self.path);
        let mut s = self.path.clone();
        s.set_extension("s");
        let _ = std::fs::remove_file(&s);
    }
}
