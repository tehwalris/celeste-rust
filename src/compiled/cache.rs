//! THE KERNEL CACHE: a start room's kernel set kept as its recipes
//! (`asm_kernel::Recipe`) and assembled code, so a process that needs a set
//! another process already built loads it instead of tracing, lowering and
//! assembling it again (most of a check tier's time is kernel builds).
//!
//! An entry is keyed on everything the set is a function of, so a hit is
//! the set this binary would build:
//! - the sources compiled into the binary or read as its program (`build.rs`:
//!   src/, crates/, lua/, cart/, the manifests) and its profile;
//! - the traced program's text (`trace::cart::sources`);
//! - the run's settings (all of them but the thread count and the
//!   diagnostics that change no kernel) and the level;
//! - the assembler's and the C library's versions.
//!
//! Every commit misses: entries serve the processes of one build (a check
//! run's steps, a search's levels and restarts). An entry is
//! `ROOT/k<FORMAT>/<key>/`: the key's text, the recipes, one shared object
//! per kernel and a manifest of every file's hash, checked on load (a
//! mismatch is a loud miss and a rebuild, never a use). Written to a
//! process's own directory and renamed into place: of two processes storing
//! one key, whichever rename wins is the entry. Past `CAP_BYTES` the least
//! recently used entries go. `--kernel-explain` builds without it.

use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use anyhow::{anyhow, ensure, Context, Result};

use super::asm_kernel::Recipe;
use super::KernelLanes;

/// The entries' format: a change of `Recipe` or of this layout bumps it.
pub(crate) const KERNEL_CACHE_FORMAT: u32 = 1;

/// The entries' total size past which the least recently used go.
const CAP_BYTES: u64 = 4 << 30;

/// The sources' and the profile's fingerprint (`build.rs`).
const SOURCES: &str = env!("KERNEL_SOURCES");

/// Where the entries live.
pub(crate) struct Store {
    dir: PathBuf,
}

impl Default for Store {
    fn default() -> Self {
        Store::at(Path::new("/var/tmp/celeste-kernels"))
    }
}

/// An entry's key: its name (a hash) and what it hashes, as text.
pub(crate) struct Key {
    pub(crate) name: String,
    text: String,
}

/// A kernel set as an entry holds it: per kernel its recipe and code.
pub(crate) type Entry<N> = Vec<(Recipe<N>, Vec<u8>)>;

/// The 64-bit hash of `x` (the std's SipHash: the fingerprints of one build
/// only need agree with themselves).
fn hash64(x: impl Hash) -> u64 {
    let mut h = std::collections::hash_map::DefaultHasher::new();
    x.hash(&mut h);
    h.finish()
}

/// The assembler's and the C library's versions, read once.
fn toolchain() -> &'static str {
    static T: std::sync::OnceLock<String> = std::sync::OnceLock::new();
    T.get_or_init(|| {
        let gcc = std::process::Command::new("gcc").arg("-dumpfullversion").output().map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string()).unwrap_or_else(|e| format!("gcc unavailable: {e}"));
        // SAFETY: glibc returns a static NUL-terminated string.
        let glibc = unsafe { std::ffi::CStr::from_ptr(libc::gnu_get_libc_version()) }.to_string_lossy().into_owned();
        format!("gcc {gcc}, glibc {glibc}")
    })
}

/// Level `level`'s kernel set under `s`, in `N`'s arithmetic: its key.
pub(crate) fn key<N: KernelLanes>(level: crate::abstraction::Level, s: &crate::settings::Settings) -> Result<Key> {
    // The settings but those that change no kernel: the threads, and the
    // diagnostics (but the census, which an entry holds).
    let settings = crate::settings::Settings {
        threads: None,
        diag: crate::settings::Diagnostics { kernel_metrics: s.diag.kernel_metrics, ..Default::default() },
        ..s.clone()
    };
    let program = crate::trace::cart::sources(s)?;
    let text = format!(
        "kernel cache format {KERNEL_CACHE_FORMAT}\nsources {SOURCES}, debug assertions {}\n{}\narithmetic {}, level {level}\nsettings {settings:?}\nprogram {:016x} ({} bytes)\n",
        cfg!(debug_assertions),
        toolchain(),
        N::NAME,
        hash64(&program),
        program.len()
    );
    Ok(Key { name: format!("{:016x}{:016x}", hash64((1u8, &text, &program)), hash64((2u8, &text, &program))), text })
}

/// A file's manifest line: its name, size and hash.
fn manifest_line(name: &str, bytes: &[u8]) -> String {
    format!("{name} {} {:016x}\n", bytes.len(), hash64(bytes))
}

impl Store {
    pub(crate) fn at(root: &Path) -> Store {
        Store { dir: root.join(format!("k{KERNEL_CACHE_FORMAT}")) }
    }

    /// The entry of `key`, if one is stored and whole. A damaged entry (a
    /// file missing or not its manifest's) is said, removed, and a miss.
    pub(crate) fn load<N: KernelLanes>(&self, key: &Key) -> Result<Option<Entry<N>>> {
        let dir = self.dir.join(&key.name);
        if !dir.exists() {
            return Ok(None);
        }
        match read_entry::<N>(&dir, key) {
            Ok(entry) => {
                // Recently used (the eviction's order).
                let _ = std::fs::File::options().write(true).open(dir.join("used")).and_then(|f| f.set_modified(std::time::SystemTime::now()));
                Ok(Some(entry))
            }
            Err(e) => {
                eprintln!("[asm build] kernel cache entry {} is DAMAGED, rebuilt: {e:#}", dir.display());
                let _ = std::fs::remove_dir_all(&dir);
                Ok(None)
            }
        }
    }

    /// Store `entry` as `key`'s (unless another process just did), then
    /// evict past the cap.
    pub(crate) fn store<N: KernelLanes>(&self, key: &Key, entry: &[(&Recipe<N>, &[u8])]) -> Result<()> {
        std::fs::create_dir_all(&self.dir).with_context(|| format!("creating {}", self.dir.display()))?;
        let tmp = self.dir.join(format!(".{}.{}", key.name, std::process::id()));
        let _ = std::fs::remove_dir_all(&tmp);
        std::fs::create_dir(&tmp)?;
        let mut manifest = String::new();
        let mut put = |name: &str, bytes: &[u8]| -> Result<()> {
            std::fs::write(tmp.join(name), bytes)?;
            manifest.push_str(&manifest_line(name, bytes));
            Ok(())
        };
        put("key.txt", key.text.as_bytes())?;
        let recipes: Vec<&Recipe<N>> = entry.iter().map(|(r, _)| *r).collect();
        put("recipes.bin", &bincode::serialize(&recipes)?)?;
        for (i, (_, so)) in entry.iter().enumerate() {
            put(&format!("k{i}.so"), so)?;
        }
        std::fs::write(tmp.join("manifest.txt"), manifest)?;
        std::fs::write(tmp.join("used"), b"")?;
        // Whichever process's rename wins is the entry.
        if std::fs::rename(&tmp, self.dir.join(&key.name)).is_err() {
            std::fs::remove_dir_all(&tmp)?;
        }
        self.evict(&key.name)
    }

    /// The least recently used entries removed until the rest fit the cap
    /// (never `keep`).
    fn evict(&self, keep: &str) -> Result<()> {
        let mut entries: Vec<(std::time::SystemTime, u64, PathBuf)> = Vec::new();
        for e in std::fs::read_dir(&self.dir)?.flatten() {
            let p = e.path();
            let name = e.file_name().to_string_lossy().into_owned();
            if name.starts_with('.') || name == keep || !p.is_dir() {
                continue;
            }
            let used = std::fs::metadata(p.join("used")).and_then(|m| m.modified()).unwrap_or(std::time::UNIX_EPOCH);
            let size = std::fs::read_dir(&p)?.flatten().filter_map(|f| f.metadata().ok()).map(|m| m.len()).sum();
            entries.push((used, size, p));
        }
        let kept = std::fs::read_dir(self.dir.join(keep)).map(|d| d.flatten().filter_map(|f| f.metadata().ok()).map(|m| m.len()).sum()).unwrap_or(0);
        let mut total: u64 = kept + entries.iter().map(|e| e.1).sum::<u64>();
        entries.sort();
        for (_, size, p) in entries {
            if total <= CAP_BYTES {
                break;
            }
            let _ = std::fs::remove_dir_all(&p);
            total -= size;
        }
        Ok(())
    }
}

/// An entry's files, each checked against the manifest, its key against
/// `key`'s.
fn read_entry<N: KernelLanes>(dir: &Path, key: &Key) -> Result<Entry<N>> {
    let manifest = std::fs::read_to_string(dir.join("manifest.txt")).context("the manifest")?;
    let read = |name: &str| -> Result<Vec<u8>> {
        let bytes = std::fs::read(dir.join(name)).with_context(|| format!("{name}"))?;
        let line = manifest_line(name, &bytes);
        ensure!(manifest.lines().any(|l| format!("{l}\n") == line), "{name} is not the manifest's ({})", line.trim_end());
        Ok(bytes)
    };
    ensure!(read("key.txt")? == key.text.as_bytes(), "the stored key is not this one");
    let recipes: Vec<Recipe<N>> = bincode::deserialize(&read("recipes.bin")?).context("the recipes")?;
    ensure!(manifest.lines().count() == recipes.len() + 2, "the manifest lists {} files for {} kernels", manifest.lines().count(), recipes.len());
    recipes.into_iter().enumerate().map(|(i, r)| Ok((r, read(&format!("k{i}.so"))?))).collect::<Result<Entry<N>>>().map_err(|e: anyhow::Error| anyhow!("{e:#}"))
}

#[cfg(test)]
mod tests {
    use celeste_core::lua_f64::LuaF64;
    use celeste_core::pico8_num::Pico8Num;

    use crate::abstraction::Level;
    use crate::settings::{Category, Settings};

    /// A MISS FOR EVERY INPUT CHANGED: the key moves with each setting that
    /// shapes a kernel set (and with the arithmetic and the level), not
    /// with the thread count; it names the sources' fingerprint and the
    /// toolchain.
    #[test]
    fn a_kernel_cache_key_moves_with_every_input() {
        let level = Level::parse("r0sxh").expect("level");
        let base = Settings::default();
        let k = |s: &Settings, l: Level| super::key::<Pico8Num>(l, s).expect("key").name;
        let at = k(&base, level);
        assert_eq!(at, k(&Settings { threads: Some(3), ..base.clone() }, level), "the threads change no kernel");
        let changed: Vec<(&str, Settings)> = vec![
            ("room", Settings { room: (4, 2), ..base.clone() }),
            ("region", Settings { region: Some((8, 6)), ..base.clone() }),
            ("split frame", Settings { split_frame: true, ..base.clone() }),
            ("category", Settings { category: Category::Hundred, ..base.clone() }),
            ("loading jank", Settings { loading_jank: Some(1), ..base.clone() }),
            ("balloon seeds", Settings { balloon_seeds: Some("0".to_string()), ..base.clone() }),
            ("win", Settings { win_at: Some((9, 9, 101, 101)), ..base.clone() }),
            ("census", Settings { diag: crate::settings::Diagnostics { kernel_metrics: true, ..Default::default() }, ..base.clone() }),
        ];
        for (what, s) in &changed {
            assert_ne!(at, k(s, level), "{what}: the key did not move");
        }
        assert_ne!(at, k(&base, Level::parse("r0sxhn").expect("level")), "the level");
        assert_ne!(at, super::key::<LuaF64>(level, &base).expect("key").name, "the arithmetic");
        let text = super::key::<Pico8Num>(level, &base).expect("key").text;
        assert!(text.contains(super::SOURCES) && text.contains("gcc ") && text.contains("glibc "), "{text}");
    }

    /// CACHED = FRESH: per case, two fresh builds store the same entry
    /// (recipes and code, byte for byte) and a third build loads it: the
    /// same kernel text and the same KEY LAYOUTS. Room (1,0) r0sxh, and room
    /// (6,0) r0sxhf split, whose ten exact platforms are keyed by
    /// dependencies (`exact::Deps`; the followers' templates are no
    /// recipe's: `AsmKernel::of` makes them from the level, alike for both).
    /// A damaged entry is a miss, rebuilt.
    #[test]
    #[ignore = "slow, ./check.sh t2 runs it: three builds each of room (1,0)'s and room (6,0)'s kernels, ~60 s"]
    fn a_cached_kernel_set_is_the_fresh_one() {
        use super::super::asm_kernel::Registry;
        let cases = [("r0sxh", Settings::default()), ("r0sxhf", Settings { room: (6, 0), split_frame: true, ..Settings::default() })];
        for (case, (level, s)) in cases.iter().enumerate() {
            let level = Level::parse(level).expect("level");
            let roots: Vec<std::path::PathBuf> = (0..2).map(|i| std::env::temp_dir().join(format!("celeste-kernel-cache-{}-{case}-{i}", std::process::id()))).collect();
            for r in &roots {
                let _ = std::fs::remove_dir_all(r);
            }
            let stores: Vec<super::Store> = roots.iter().map(|r| super::Store::at(r)).collect();
            let key = super::key::<Pico8Num>(level, s).expect("key");
            let entry = |i: usize| -> Vec<(String, Vec<u8>)> {
                let dir = stores[i].dir.join(&key.name);
                let mut files: Vec<(String, Vec<u8>)> = std::fs::read_dir(&dir).expect("the entry").flatten().map(|f| (f.file_name().to_string_lossy().into_owned(), std::fs::read(f.path()).expect("read"))).collect();
                files.sort();
                files
            };
            let fresh: Vec<Registry<Pico8Num>> = stores.iter().map(|st| Registry::build_with(level, s, Some(st)).expect("a fresh build")).collect();
            assert_eq!(entry(0), entry(1), "room {:?}: two fresh builds store the same entry", s.room);
            assert!(entry(0).iter().filter(|(n, _)| n.ends_with(".so")).count() == fresh[0].len(), "an entry holds every kernel's code");
            let cached = Registry::<Pico8Num>::build_with(level, s, Some(&stores[0])).expect("a cached build");
            assert_eq!(cached.text_digest(), fresh[0].text_digest(), "room {:?}: the cached kernels are the fresh ones", s.room);
            let layouts = cached.key_layouts();
            assert!(layouts == fresh[0].key_layouts(), "room {:?}: the cached kernels' key layouts are the fresh ones'", s.room);
            let with_deps = layouts.iter().filter(|l| l.1.iter().any(|(r, _)| !r.deps.is_empty())).count();
            assert_eq!(with_deps > 0, s.room == (6, 0), "room {:?}: {with_deps} of {} kernels key by dependencies", s.room, layouts.len());
            if case == 0 {
                // A damaged entry is a miss: rebuilt and stored again.
                let so = stores[0].dir.join(&key.name).join("k0.so");
                let mut bytes = std::fs::read(&so).expect("k0.so");
                bytes[100] ^= 1;
                std::fs::write(&so, bytes).expect("write");
                assert!(stores[0].load::<Pico8Num>(&key).expect("load").is_none(), "a damaged entry is not used");
                assert!(!stores[0].dir.join(&key.name).exists(), "a damaged entry is removed");
            }
            for r in &roots {
                let _ = std::fs::remove_dir_all(r);
            }
        }
    }
}
