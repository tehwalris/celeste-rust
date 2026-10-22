//! THE SOURCES' FINGERPRINT for the kernel cache (`compiled::cache`): a
//! hash of every file that compiles into the binary or that it reads as its
//! program (src/, crates/, lua/, cart/, the manifests, this script), and
//! the build's profile. A kernel set cached by one build is only ever
//! loaded by a build of the same sources.

use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

const TREES: [&str; 4] = ["src", "crates", "lua", "cart"];
const FILES: [&str; 3] = ["Cargo.toml", "Cargo.lock", "build.rs"];

fn files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else { return };
    for e in entries.flatten() {
        let p = e.path();
        if p.is_dir() {
            if p.file_name().is_some_and(|n| n != "target") {
                files(&p, out);
            }
        } else {
            out.push(p);
        }
    }
}

fn main() {
    let mut all: Vec<PathBuf> = FILES.iter().map(PathBuf::from).collect();
    for t in TREES {
        files(Path::new(t), &mut all);
        println!("cargo:rerun-if-changed={t}");
    }
    for f in FILES {
        println!("cargo:rerun-if-changed={f}");
    }
    all.sort();
    let mut h = std::collections::hash_map::DefaultHasher::new();
    for p in &all {
        p.hash(&mut h);
        std::fs::read(p).unwrap_or_else(|e| panic!("{}: {e}", p.display())).hash(&mut h);
    }
    // The profile: one checkout's quick and release builds are other code.
    for var in ["PROFILE", "OPT_LEVEL", "DEBUG"] {
        std::env::var(var).unwrap_or_default().hash(&mut h);
    }
    println!("cargo:rustc-env=KERNEL_SOURCES={:016x} ({} files)", h.finish(), all.len());
}
