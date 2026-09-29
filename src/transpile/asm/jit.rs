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
use std::sync::OnceLock;

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

/// This process's scratch directory for the `.s` / `.so` pairs,
/// `target/asm-scratch/<pid>/`. Inside `target/` so it is ignored and
/// cleaned by `cargo clean`.
///
/// A loaded `.so` stays on disk while the process runs (a profile of a
/// live run attributes samples to its path), and the kernel registry lives
/// until exit, so nothing unlinks them on exit, a kill or an OOM. As flat
/// files, one pair per kernel per run, they reached 641 GB in 240k files
/// (2026-09-29). So the first call sweeps: the directory of every pid that
/// is no longer running is removed, and this pid's own directory (left by
/// a dead process with the same pid) is emptied.
fn scratch_dir() -> &'static Path {
    static DIR: OnceLock<PathBuf> = OnceLock::new();
    DIR.get_or_init(|| {
        let mut root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        root.push("target");
        root.push("asm-scratch");
        if let Ok(entries) = std::fs::read_dir(&root) {
            for e in entries.flatten() {
                let pid = e.file_name().to_str().and_then(|n| n.parse::<u32>().ok());
                if let Some(pid) = pid {
                    if !Path::new("/proc").join(pid.to_string()).exists() {
                        let _ = std::fs::remove_dir_all(e.path());
                    }
                }
            }
        }
        let dir = root.join(std::process::id().to_string());
        let _ = std::fs::remove_dir_all(&dir);
        let _ = std::fs::create_dir_all(&dir);
        dir
    })
}

fn unique_stem(tag: &str) -> String {
    let n = COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("k_{}_{}", tag, n)
}

/// Write `asm` to a `.s` file and assemble it into a shared object with
/// `gcc -shared -fPIC`. Returns the `.so` path. Timed separately from
/// emission by the benchmarks. The `.s` is removed once it has assembled
/// (kept on failure, for the error to name): it is the bulk of the
/// scratch, MBs of text for a big kernel against a `.so` of tens of KB.
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
    let _ = std::fs::remove_file(&src);
    Ok(obj)
}

/// A loaded shared object plus the resolved symbol. `dlclose` on drop.
pub struct Loaded {
    handle: *mut c_void,
    path: PathBuf,
    pub func: KernelFn,
}

// The `handle` is a dlopen token used only by `dlclose` on drop (single
// owner), and `func` is a pure, reentrant, stateless kernel - it reads its
// input buffer and writes its output buffer, with call-outs through the
// caller-provided `AsmCtx`, and holds no shared mutable state. So a `Loaded`
// is safe to share and call across threads (the engine dispatches chunks on
// a worker pool against one shared registry).
unsafe impl Send for Loaded {}
unsafe impl Sync for Loaded {}

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
    }
}
