//! Assemble a GAS `.s` string with `gcc` and load it with `dlopen`. `as`
//! assembles AVX-512 natively (a JIT crate would have to hand-encode EVEX).

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
/// `target/asm-scratch/<pid>/`.
///
/// A loaded `.so` stays on disk while the process runs (profiles attribute
/// samples to its path), and nothing unlinks it on a kill or an OOM, so the
/// scratch grows without bound across runs. The first call therefore
/// removes the directory of every pid no longer running and empties this
/// pid's own.
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

/// What no kernel may hold: a FUSED multiply-add (one rounding where the
/// cart rounds twice; `vfmadd*`, `vfmsub*`, `vfnmadd*`, `vfnmsub*`, the FMA4
/// forms) or an MXCSR write (`ldmxcsr`: the rounding mode and subnormal
/// handling are the process's, checked, never set). The text is ours and
/// gcc only assembles it (`-x assembler`, no compiler, no contraction), so
/// this scan of the text is the whole guarantee.
fn refuse_forbidden(asm: &str) -> Result<()> {
    for (n, line) in asm.lines().enumerate() {
        let mn = line.split_whitespace().next().unwrap_or("");
        if mn.starts_with("vfm") || mn.starts_with("vfnm") || mn.starts_with("ldmxcsr") || mn.starts_with("vldmxcsr") {
            bail!("kernel text line {}: `{}` is forbidden (a fused multiply-add or an MXCSR write)", n + 1, line.trim());
        }
    }
    Ok(())
}

/// Write `asm` to a `.s` file and assemble it into a shared object with
/// `gcc -x assembler -shared -fPIC` (assembled, never compiled). Returns the
/// `.so` path. The `.s` (the bulk of the scratch) is removed once it has
/// assembled, kept on failure for the error. A text holding a forbidden
/// instruction is refused (`refuse_forbidden`).
pub fn assemble(asm: &str, tag: &str) -> Result<PathBuf> {
    refuse_forbidden(asm)?;
    let dir = scratch_dir();
    let stem = unique_stem(tag);
    let src = dir.join(format!("{stem}.s"));
    let obj = dir.join(format!("{stem}.so"));
    std::fs::write(&src, asm)?;
    let out = std::process::Command::new("gcc")
        .arg("-x")
        .arg("assembler")
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

/// THE DOUBLES' FLOATING-POINT ENVIRONMENT: round to nearest, FTZ and DAZ
/// off, every exception masked (MXCSR `0x1f80`, the sticky flags aside), as
/// LuaJIT runs. A library built with fast-math can set FTZ/DAZ for the whole
/// process; the float kernels and the reference engine would then flush
/// subnormals alike and agree on a wrong answer. Checked at every float
/// kernel call.
pub fn check_mxcsr() -> Result<()> {
    let mut v: u32 = 0;
    // SAFETY: `stmxcsr` stores the 32-bit MXCSR to the given address.
    unsafe { std::arch::asm!("stmxcsr [{}]", in(reg) &mut v, options(nostack, preserves_flags)) };
    if v & !0x3f != 0x1f80 {
        bail!("MXCSR is {v:#06x}: the float kernels need round-to-nearest, FTZ and DAZ off and every exception masked (0x1f80)");
    }
    Ok(())
}

/// A loaded shared object plus the resolved symbol. `dlclose` on drop.
pub struct Loaded {
    handle: *mut c_void,
    path: PathBuf,
    pub func: KernelFn,
}

// SAFETY: `handle` is a dlopen token used only by `dlclose` on drop (single
// owner), and `func` is a pure, reentrant, stateless kernel: it reads its
// input buffer, writes its output buffer, calls out through the caller's
// `AsmCtx`, and holds no shared mutable state.
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
