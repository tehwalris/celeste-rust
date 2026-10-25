//! THE FRAME BENCHMARK's measures (`rewrite bench frame`, plans/devloop.md
//! "The forward frame benchmark"): fingerprints of a frame's output,
//! hardware counters, the cost report and the memory report.

use anyhow::{ensure, Context, Result};
use std::path::{Path, PathBuf};

/// `ckhash --edges`'s `e{frame}` line over the frame's edge file in `dir`'s
/// `edges/` (`xfers`: the merged table; the dir holds the frame's file
/// alone, and no trusted transfer).
pub fn edge_fingerprint<N: crate::search::rem::Rem>(dir: &Path, frame: u32, xfers: &super::edges::XferTable<N>, visited: &super::visited::VisitedSet<N>, threads: usize) -> Result<(u64, u64)> {
    let store = super::edges::EdgeStore::<N>::open(dir, frame)?;
    EdgeHashes::new::<N>(&xfers.pairs).frame(&store, frame, &visited.keys.content_hash(), visited.shapes(), threads)
}

/// `ckhash --edges`'s per-edge term: `mix64(node(dst) ^ node(src) <<< 21 ^
/// pair <<< 42)`, a node `ContentHash::state` of its (shape, key, cell)
/// (`mix64(key hash ^ cell << 1)`), a pair the hash of its bytes - summed
/// over a frame's edges, with their count.
pub(crate) struct EdgeHashes {
    pairs: Vec<u64>,
}

impl EdgeHashes {
    pub(crate) fn new<N: crate::search::rem::Rem>(pairs: &[crate::search::rem::Pair<N>]) -> Self {
        let mix = celeste_engine::runtime2::mix64;
        EdgeHashes {
            pairs: pairs
                .iter()
                .map(|pair| {
                    let mut b = Vec::new();
                    N::encode_pair(&mut b, pair);
                    b.iter().fold(0u64, |h, &x| mix(h ^ x as u64))
                })
                .collect(),
        }
    }

    /// Frame `frame`'s `(count, sum of terms)` (`shapes`: the tree's by index).
    pub(crate) fn frame<N: crate::search::rem::Rem>(&self, store: &super::edges::EdgeStore<N>, frame: u32, content: &celeste_engine::exact::ContentHash, shapes: &[u64], threads: usize) -> Result<(u64, u64)> {
        let mix = celeste_engine::runtime2::mix64;
        let parts = store.par_blocks(frame, threads, || (0u64, 0u64), |(n, acc), region, blk| {
            let slot = region % crate::storage::cells::SLOTS;
            let t_shape = *shapes.get((region / crate::storage::cells::SLOTS) as usize).context("a target region of no shape")?;
            let th: Vec<u64> = blk.t_keys.iter().map(|&k| content.key(t_shape, k)).collect();
            let sh: Vec<u64> = blk.sources.iter().map(|&(s, k)| shapes.get(s as usize).map(|&h| content.key(h, k))).collect::<Option<_>>().context("a source of no shape")?;
            let mut bad = false;
            blk.records(|r| {
                let Some(&p) = self.pairs.get(r.x as usize) else {
                    bad = true;
                    return;
                };
                super::bundle::edges(r.mask, slot, r.o, |_, tc, sc| {
                    let t = mix(th[r.t as usize] ^ (tc as u64) << 1);
                    let s = mix(sh[r.s as usize] ^ (sc as u64) << 1);
                    *acc = acc.wrapping_add(mix(t ^ s.rotate_left(21) ^ p.rotate_left(42)));
                    *n += 1;
                });
            })?;
            ensure!(!bad, "f{frame}: an edge without its transfer");
            Ok(())
        })?;
        Ok(parts.into_iter().fold((0, 0), |(n, acc), (m, a)| (n + m, acc.wrapping_add(a))))
    }
}

/// One `bench frame` rep for `print_cost` (`load`: at its start; `counts`:
/// the rep's hardware counts, every thread's).
pub struct Rep {
    pub stats: super::stats::Stats,
    pub workers: usize,
    pub load: f64,
    pub counts: Option<[u64; 4]>,
}

/// HARDWARE COUNTERS over a rep, every thread it starts included
/// (`perf_event_open`, `inherit`: an exited thread's counts join its
/// parent's, and a frame joins its threads): user instructions, user
/// cycles, kernel cycles, page faults.
/// Instructions do not move with the machine's load as walls do.
pub struct Counters {
    fds: [i32; 4],
}

impl Counters {
    /// The counters, disabled (open them before the threads they count
    /// start); `None` where the kernel refuses (`perf_event_paranoid`).
    pub fn open() -> Option<Counters> {
        // `perf_event_attr` (PERF_ATTR_SIZE_VER0, 64 bytes): type, size,
        // config, sample_period, sample_type, read_format, the flag bits
        // (disabled 0, inherit 1, exclude_user 4, exclude_kernel 5,
        // exclude_hv 6), wakeup_events, bp_type, config1.
        #[repr(C)]
        struct Attr {
            kind: u32,
            size: u32,
            config: u64,
            sample_period: u64,
            sample_type: u64,
            read_format: u64,
            flags: u64,
            wakeup_events: u32,
            bp_type: u32,
            config1: u64,
        }
        const HW: u32 = 0;
        const SW: u32 = 1;
        const CYCLES: u64 = 0;
        const INSTRUCTIONS: u64 = 1;
        const PAGE_FAULTS: u64 = 2;
        let (disabled, inherit, excl_user, excl_kernel, excl_hv) = (1u64, 1u64 << 1, 1u64 << 4, 1u64 << 5, 1u64 << 6);
        let mut fds = [-1i32; 4];
        for (fd, (kind, config, excl)) in fds.iter_mut().zip([(HW, INSTRUCTIONS, excl_kernel | excl_hv), (HW, CYCLES, excl_kernel | excl_hv), (HW, CYCLES, excl_user | excl_hv), (SW, PAGE_FAULTS, 0)]) {
            let attr = Attr { kind, size: std::mem::size_of::<Attr>() as u32, config, sample_period: 0, sample_type: 0, read_format: 0, flags: disabled | inherit | excl, wakeup_events: 0, bp_type: 0, config1: 0 };
            // SAFETY: a valid attr for the calling thread (pid 0, any cpu,
            // no group, no flags); the fd is closed in `Drop`.
            let r = unsafe { libc::syscall(libc::SYS_perf_event_open, &attr as *const Attr, 0, -1, -1, 0) };
            if r < 0 {
                return None;
            }
            *fd = r as i32;
        }
        Some(Counters { fds })
    }

    /// Counting; the counts so far (`stop` subtracts: a reset would miss the
    /// exited threads' counts, which `inherit` keeps apart).
    pub fn start(&self) -> [u64; 4] {
        for &fd in &self.fds {
            // SAFETY: an open perf event fd; ENABLE 0x2400.
            unsafe { libc::ioctl(fd, 0x2400, 0) };
        }
        self.read()
    }

    /// The counts since `start` (`at`: what it returned).
    pub fn stop(&self, at: [u64; 4]) -> [u64; 4] {
        let now = self.read();
        std::array::from_fn(|k| now[k].wrapping_sub(at[k]))
    }

    /// The counts so far (every thread joined so far).
    pub(crate) fn read(&self) -> [u64; 4] {
        let mut out = [0u64; 4];
        for (o, &fd) in out.iter_mut().zip(&self.fds) {
            // SAFETY: an open perf event fd: a plain read of its u64 count.
            unsafe { libc::read(fd, o as *mut u64 as *mut libc::c_void, 8) };
        }
        out
    }
}

/// The counters a bench installed for the frame end's and checkpoint's STEPS.
static STEPS: std::sync::OnceLock<Counters> = std::sync::OnceLock::new();

/// Install `c` as the step counters (once a process); the installed ones.
pub fn count_steps(c: Counters) -> &'static Counters {
    STEPS.get_or_init(|| c)
}

/// The step counters' counts now (`None`: none installed). Read on the
/// opening thread between steps, each step's threads joined.
pub(crate) fn step_counts() -> Option<[u64; 4]> {
    STEPS.get().map(Counters::read)
}

impl Drop for Counters {
    fn drop(&mut self) {
        for &fd in &self.fds {
            // SAFETY: our fd, closed once.
            unsafe { libc::close(fd) };
        }
    }
}

/// The 1-minute load average (`/proc/loadavg`; 0 unread).
pub fn load_average() -> f64 {
    std::fs::read_to_string("/proc/loadavg").ok().and_then(|s| s.split_whitespace().next()?.parse().ok()).unwrap_or(0.0)
}

/// THE COST REPORT of a frame's reps (`[cost]`), the registry rendered: its
/// counts; per time, min and max over the reps and the min's cost per the
/// element it is per (`S::per`); a worker time's share of the wave's
/// budget (workers x wall); a wall's hardware counts where counted. Quote
/// the min, with the load.
pub fn print_cost(frame: u32, reps: &[Rep]) {
    use super::stats::{Registry, Unit, ALL, S};
    let Some(r0) = reps.first() else { return };
    let col = |s: S| reps.iter().map(|r| r.stats.get(s) as f64).collect::<Vec<f64>>();
    let mm = |v: &[f64]| (v.iter().copied().fold(f64::INFINITY, f64::min), v.iter().copied().fold(f64::NEG_INFINITY, f64::max));
    let loads: Vec<f64> = reps.iter().map(|r| r.load).collect();
    let counts: Vec<String> = ALL.iter().filter(|s| s.unit() == Unit::Count && r0.stats.get(**s) != 0).map(|&s| format!("{} {}", s.name(), r0.stats.show(s))).collect();
    eprintln!("[cost] f{frame:03}: {} reps, {} workers, load {:.1}-{:.1} | {} | min / max over the reps", reps.len(), r0.workers, mm(&loads).0, mm(&loads).1, counts.join(" "));
    let budget = mm(&reps.iter().map(|r| r.stats.get(S::Wave) as f64 * r.workers as f64).collect::<Vec<_>>()).0;
    let counted = reps.iter().all(|r| r.counts.is_some());
    eprintln!("[cost] {:<28} {:>9} {:>9}  {:>6}  {:>27}  per element (the min)", "time", "s min", "s max", "share", if counted { "G ins, user cyc, kernel cyc" } else { "" });
    for &s in ALL {
        let secs: Vec<f64> = match s.unit() {
            Unit::Wall | Unit::Work => col(s).iter().map(|v| v / 1e9).collect(),
            Unit::Ticks => col(s).iter().map(|v| v / super::stats::tsc_hz()).collect(),
            _ => continue,
        };
        let (a, b) = mm(&secs);
        if b == 0.0 {
            continue;
        }
        let share = if s.unit() == Unit::Wall { String::new() } else { format!("{:>5.1}%", 100.0 * a * 1e9 / budget.max(1.0)) };
        let hw = match counted {
            true if reps.iter().any(|r| r.stats.counts(s) != [0; 4]) => {
                let m = |k: usize| reps.iter().map(|r| r.stats.counts(s)[k]).min().unwrap_or(0) as f64 / 1e9;
                format!("{:>8.2} {:>8.2} {:>9.2}", m(0), m(1), m(2))
            }
            _ => String::new(),
        };
        let per = match s.per().map(|p| r0.stats.get(p)) {
            Some(n) if n > 0 => format!("{:>9.1} ns/{}", a * 1e9 / n as f64, s.per().expect("matched").name()),
            _ => String::new(),
        };
        eprintln!("[cost] {:<28} {:>9.3} {:>9.3}  {:>6}  {:>27}  {}", s.name(), a, b, share, hw, per);
    }
    if counted {
        let c = |k: usize| reps.iter().map(|r| r.counts.map_or(0.0, |c| c[k] as f64)).collect::<Vec<f64>>();
        let (ins, cyc, kcyc, faults) = (c(0), c(1), c(2), c(3));
        let (ia, ib) = mm(&ins);
        let per = |n: S| ia / (r0.stats.get(n).max(1) as f64);
        eprintln!(
            "[cost] counted, every thread: user instructions {:.1} G - {:.1} G ({:.0} an emission, {:.0} a lane in), user cycles {:.1} G (IPC {:.2}), kernel-mode cycles {:.1} G ({:.0}% of all), page faults {:.2} M - {:.2} M",
            ia / 1e9,
            ib / 1e9,
            per(S::Emissions),
            per(S::LanesIn),
            mm(&cyc).0 / 1e9,
            ins[0] / cyc[0].max(1.0),
            mm(&kcyc).0 / 1e9,
            100.0 * kcyc[0] / (kcyc[0] + cyc[0]).max(1.0),
            mm(&faults).0 / 1e6,
            mm(&faults).1 / 1e6
        );
    }
}

/// `(files, bytes, hash)` of every file under `dir`, by relative path:
/// `bench frame`'s `b` line, proving a frame's files byte-identical.
pub fn dir_fingerprint(dir: &Path) -> Result<(u64, u64, u64)> {
    use celeste_engine::runtime2::mix64;
    let mut files: Vec<(PathBuf, PathBuf)> = Vec::new();
    let mut stack = vec![dir.to_path_buf()];
    while let Some(d) = stack.pop() {
        for e in std::fs::read_dir(&d).with_context(|| d.display().to_string())? {
            let p = e?.path();
            if p.is_dir() {
                stack.push(p);
            } else {
                files.push((p.strip_prefix(dir)?.to_path_buf(), p));
            }
        }
    }
    files.sort();
    let (mut bytes, mut acc) = (0u64, 0u64);
    for (rel, p) in &files {
        let b = std::fs::read(p).with_context(|| p.display().to_string())?;
        let mut h = mix64(b.len() as u64 ^ rel.to_string_lossy().bytes().fold(0u64, |a, c| mix64(a ^ c as u64)));
        for w in b.chunks(8) {
            let mut x = [0u8; 8];
            x[..w.len()].copy_from_slice(w);
            h = mix64(h ^ u64::from_le_bytes(x));
        }
        acc = acc.rotate_left(5) ^ h;
        bytes += b.len() as u64;
    }
    Ok((files.len() as u64, bytes, acc))
}
