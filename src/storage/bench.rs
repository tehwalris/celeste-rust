//! THE FRAME BENCHMARK's measures (`rewrite bench frame`, plans/devloop.md
//! "The forward frame benchmark"): fingerprints of a frame's output,
//! hardware counters, the cost report and the memory report.

use crate::search::hull::HullOf;
use anyhow::{ensure, Context, Result};
use std::path::{Path, PathBuf};

/// `ckhash --edges`'s `e{frame}` line over the frame's edge file (`xfers`:
/// the merged table; the dir's file may hold only the frame's new pairs).
pub fn edge_fingerprint<N: crate::search::rem::Rem>(edges_dir: &Path, frame: u32, xfers: &super::edges::XferTable<N>, visited: &super::visited::VisitedSet<N>, threads: usize) -> Result<(u64, u64)> {
    let store = super::edges::EdgeStore::<N>::open(edges_dir, frame)?;
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
    pub(crate) fn new<N: crate::search::rem::Rem>(pairs: &[N::Pair]) -> Self {
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
        let geo = super::GEOMETRY;
        let parts = store.par_blocks(frame, threads, || (0u64, 0u64), |(n, acc), region, blk| {
            let slot = region % geo.slots;
            let t_shape = *shapes.get((region / geo.slots) as usize).context("a target region of no shape")?;
            let th: Vec<u64> = blk.t_keys.iter().map(|&k| content.key(t_shape, k)).collect();
            let sh: Vec<u64> = blk.sources.iter().map(|&(s, k)| shapes.get(s as usize).map(|&h| content.key(h, k))).collect::<Option<_>>().context("a source of no shape")?;
            let mut bad = false;
            blk.records(|r| {
                let Some(&p) = self.pairs.get(r.x as usize) else {
                    bad = true;
                    return;
                };
                let mut m = r.mask;
                while m != 0 {
                    let c = m.trailing_zeros();
                    m &= m - 1;
                    let tc = geo.cell(slot, c);
                    let sc = super::bundle::source_cell(tc, r.o);
                    let t = mix(th[r.t as usize] ^ (tc as u64) << 1);
                    let s = mix(sh[r.s as usize] ^ (sc as u64) << 1);
                    *acc = acc.wrapping_add(mix(t ^ s.rotate_left(21) ^ p.rotate_left(42)));
                    *n += 1;
                }
            })?;
            ensure!(!bad, "f{frame}: an edge without its transfer");
            Ok(())
        })?;
        Ok(parts.into_iter().fold((0, 0), |(n, acc), (m, a)| (n + m, acc.wrapping_add(a))))
    }
}

/// One `bench frame` rep for `print_cost` (`ckpt`: the checkpoint's steps
/// and the pos graph's wall, with `--checkpoint`; `load`: at its start).
pub struct Rep {
    pub stats: crate::frame::FrameStats,
    pub ckpt: Option<(crate::frame::Checkpointed, std::time::Duration)>,
    pub workers: usize,
    pub wall: std::time::Duration,
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

/// The counts between two `step_counts`.
pub(crate) fn counts_since(at: Option<[u64; 4]>) -> ([u64; 4], Option<[u64; 4]>) {
    let now = step_counts();
    let d = match (at, now) {
        (Some(a), Some(b)) => std::array::from_fn(|k| b[k].wrapping_sub(a[k])),
        _ => [0; 4],
    };
    (d, now)
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

/// A report line: label, seconds per rep, the element its cost is per (name,
/// count per rep), and a step's counts per rep.
struct Line {
    label: String,
    secs: Vec<f64>,
    per: Option<(&'static str, Vec<f64>)>,
    counts: Option<Vec<[u64; 4]>>,
}

/// THE COST REPORT of a frame's reps: walls (wave, frame end, checkpoint),
/// the wave's WORKER-SECONDS by phase with the idle share, the end's and
/// checkpoint's steps, each with its cost per element; bytes and memory.
/// Min and max over the reps (quote the min, with the load).
pub fn print_cost(frame: u32, reps: &[Rep]) {
    use crate::frame::phases as ph;
    let Some(r0) = reps.first() else { return };
    let col = |f: &dyn Fn(&Rep) -> f64| reps.iter().map(f).collect::<Vec<f64>>();
    let dur = |d: std::time::Duration| d.as_secs_f64();
    let mut lines: Vec<Line> = Vec::new();
    let mut line = |label: &str, secs: Vec<f64>, per: Option<(&'static str, Vec<f64>)>| lines.push(Line { label: label.to_string(), secs, per, counts: None });
    let n = |f: fn(&crate::frame::FrameStats) -> f64| col(&|r| f(&r.stats));
    let lanes_in = n(|s| s.lanes_in as f64);
    let emissions = n(|s| s.lanes_raw as f64);
    let new_states = n(|s| s.lanes_kept as f64);
    let frontier_out = n(|s| (s.lanes_kept + s.grown) as f64);
    let edges = n(|s| s.edge_records as f64);
    let ckpt_total = |r: &Rep| r.ckpt.map_or(0.0, |(c, pos)| c.t.iter().map(|&d| dur(d)).sum::<f64>() + dur(pos));
    line("frame (wave + end + checkpoint)", col(&|r| dur(r.stats.t_wave) + dur(r.stats.t_layer) + ckpt_total(r)), Some(("new state", new_states.clone())));
    line("  wave (units and region jobs)", col(&|r| dur(r.stats.t_wave)), Some(("emission", emissions.clone())));
    line("  frame end", col(&|r| dur(r.stats.t_layer)), Some(("new state", new_states.clone())));
    if r0.ckpt.is_some() {
        line("  checkpoint (files written, read back)", col(&ckpt_total), Some(("frontier state", frontier_out.clone())));
    }
    let wall_lines = lines.len();
    // The wave's worker-seconds.
    let p = |k: usize| col(&|r| r.stats.phase_s[k]);
    let budget = col(&|r| dur(r.stats.t_wave) * r.workers as f64);
    let idle = col(&|r| r.stats.wave_idle * dur(r.stats.t_wave) * r.workers as f64);
    let unit_parts = [ph::SETUP, ph::PACK, ph::KERNEL, ph::EMIT];
    let disjoint = [ph::DECODE, ph::UNIT, ph::END_UNIT, ph::OUT_BLOCKS, ph::REPUSH, ph::REGIONS, ph::GATHER];
    let mut ws: Vec<Line> = Vec::new();
    let mut wline = |label: &str, secs: Vec<f64>, per: Option<(&'static str, Vec<f64>)>| ws.push(Line { label: label.to_string(), secs, per, counts: None });
    wline("unit decode (rows from keys)", p(ph::DECODE), Some(("lane in", lanes_in.clone())));
    wline("slice setup", p(ph::SETUP), Some(("lane in", lanes_in.clone())));
    wline("pack", p(ph::PACK), Some(("lane in", lanes_in.clone())));
    wline("kernel", p(ph::KERNEL), Some(("lane in", lanes_in.clone())));
    wline("emit", p(ph::EMIT), Some(("emission", emissions.clone())));
    wline("unit rest (engine.run less the above)", col(&|r| r.stats.phase_s[ph::UNIT] - unit_parts.iter().map(|&k| r.stats.phase_s[k]).sum::<f64>()), Some(("lane in", lanes_in.clone())));
    wline("unit end (chunks, pushes, out rows)", p(ph::END_UNIT), Some(("emission", emissions.clone())));
    wline("unit out blocks (zstd)", p(ph::OUT_BLOCKS), Some(("edge", edges.clone())));
    wline("re-push units", p(ph::REPUSH), Some(("re-pushed edge", n(|s| s.repush_edges as f64))));
    wline("region jobs", p(ph::REGIONS), Some(("push", n(|s| s.pushes as f64))));
    wline("slice gather", p(ph::GATHER), None);
    wline("busy, untimed", col(&|r| dur(r.stats.t_wave) * r.workers as f64 * (1.0 - r.stats.wave_idle) - disjoint.iter().map(|&k| r.stats.phase_s[k]).sum::<f64>()), None);
    wline("idle", idle, None);
    // The frame end's and the checkpoint's steps.
    let mut steps: Vec<Line> = Vec::new();
    let per_end: [(&'static str, Vec<f64>); 8] = [
        ("new code", n(|s| s.new_codes as f64)),
        ("touched state", n(|s| s.touched as f64)),
        ("new state", new_states.clone()),
        ("frontier state", frontier_out.clone()),
        ("frontier state", frontier_out.clone()),
        ("new state", new_states.clone()),
        ("edge", edges.clone()),
        ("edge", edges.clone()),
    ];
    for (k, (name, per)) in crate::frame::END_STEPS.iter().zip(per_end).enumerate() {
        steps.push(Line { label: format!("end: {name}"), secs: col(&|r| dur(r.stats.t_end[k])), per: Some(per), counts: Some(reps.iter().map(|r| r.stats.end_counts[k]).collect()) });
    }
    steps.push(Line { label: "end: rest (untimed)".into(), secs: col(&|r| dur(r.stats.t_layer) - r.stats.t_end.iter().map(|&d| dur(d)).sum::<f64>()), per: None, counts: None });
    if r0.ckpt.is_some() {
        let per_ckpt = [new_states.clone(), new_states.clone(), frontier_out.clone(), frontier_out.clone()];
        for (k, (name, per)) in crate::frame::CKPT_STEPS.iter().zip(per_ckpt).enumerate() {
            steps.push(Line { label: format!("checkpoint: {name}"), secs: col(&|r| r.ckpt.map_or(0.0, |c| dur(c.0.t[k]))), per: Some(("state", per)), counts: Some(reps.iter().map(|r| r.ckpt.map_or([0; 4], |c| c.0.counts[k])).collect()) });
        }
        steps.push(Line { label: "checkpoint: pos graph".into(), secs: col(&|r| r.ckpt.map_or(0.0, |c| dur(c.1))), per: None, counts: None });
    }
    let mm = |v: &[f64]| (v.iter().copied().fold(f64::INFINITY, f64::min), v.iter().copied().fold(f64::NEG_INFINITY, f64::max));
    let per_text = |l: &Line| match &l.per {
        // The min's cost per element (the elements are the same every rep).
        Some((name, v)) if v.first().is_some_and(|&x| x > 0.0) => format!("{:>9.1} ns/{name}", mm(&l.secs).0 * 1e9 / v[0]),
        _ => String::new(),
    };
    let loads = col(&|r| r.load);
    let s = &r0.stats;
    eprintln!(
        "[cost] f{frame:03}: {} reps, {} workers, load {:.1}-{:.1} | in {} new + {} old, {} emissions, {} re-pushed edges, {} pushes, {} new + {} grown states out, {} edges | min / max over the reps",
        reps.len(),
        r0.workers,
        mm(&loads).0,
        mm(&loads).1,
        s.lanes_in,
        s.old_in,
        s.lanes_raw,
        s.repush_edges,
        s.pushes,
        s.lanes_kept,
        s.grown,
        s.edge_records
    );
    eprintln!("[cost] {:<42} {:>9} {:>9}  per element (the min)", "wall", "s min", "s max");
    for l in &lines[..wall_lines] {
        let (a, b) = mm(&l.secs);
        eprintln!("[cost] {:<42} {:>9.3} {:>9.3}  {}", l.label, a, b, per_text(l));
    }
    let (ba, _) = mm(&budget);
    eprintln!("[cost] {:<42} {:>9} {:>9}  {:>6}  per element (the min)", "wave worker-seconds", "min", "max", "share");
    for l in &ws {
        let (a, b) = mm(&l.secs);
        eprintln!("[cost] {:<42} {:>9.2} {:>9.2}  {:>5.1}%  {}", l.label, a, b, 100.0 * a / ba.max(1e-9), per_text(l));
    }
    eprintln!("[cost] {:<42} {:>9.2} {:>9.2}", "  of a budget (workers x wave)", ba, mm(&budget).1);
    let counted = reps.iter().all(|r| r.counts.is_some());
    eprintln!("[cost] {:<42} {:>9} {:>9}  {:>27}  per element (the min)", "frame end and checkpoint, wall", "s min", "s max", if counted { "G ins, user cyc, kernel cyc" } else { "" });
    for l in &steps {
        let (a, b) = mm(&l.secs);
        // A step's counts, the min over the reps of each.
        let c = match (&l.counts, counted) {
            (Some(v), true) => {
                let m = |k: usize| v.iter().map(|c| c[k]).min().unwrap_or(0) as f64 / 1e9;
                format!("{:>8.2} {:>8.2} {:>9.2}", m(0), m(1), m(2))
            }
            _ => String::new(),
        };
        eprintln!("[cost] {:<42} {:>9.3} {:>9.3}  {:>27}  {}", l.label, a, b, c, per_text(l));
    }
    if reps.iter().all(|r| r.counts.is_some()) {
        let c = |k: usize| col(&|r| r.counts.map_or(0.0, |c| c[k] as f64));
        let (ins, cyc, kcyc, faults) = (c(0), c(1), c(2), c(3));
        let (ia, ib) = mm(&ins);
        eprintln!(
            "[cost] counted, every thread: user instructions {:.1} G - {:.1} G ({:.0} an emission, {:.0} a lane in), user cycles {:.1} G (IPC {:.2}), kernel-mode cycles {:.1} G ({:.0}% of all), page faults {:.2} M - {:.2} M",
            ia / 1e9,
            ib / 1e9,
            ia / emissions[0].max(1.0),
            ia / lanes_in[0].max(1.0),
            mm(&cyc).0 / 1e9,
            ins[0] / cyc[0].max(1.0),
            mm(&kcyc).0 / 1e9,
            100.0 * kcyc[0] / (kcyc[0] + cyc[0]).max(1.0),
            mm(&faults).0 / 1e6,
            mm(&faults).1 / 1e6
        );
    }
    let gb = |b: u64| b as f64 / 1e9;
    let (fb, hb) = r0.ckpt.map_or((0, 0), |c| (c.0.frame_bytes, c.0.hull_bytes));
    eprintln!(
        "[cost] bytes written: edge file {:.3} GB, out file {:.3} GB, frame file {:.3} GB, hull file {:.3} GB | anonymous RSS start {:.2} wave {:.2} end {:.2} GB, peak (VmHWM) {:.2} GB",
        gb(s.edge_bytes),
        gb(s.out_bytes),
        gb(fb),
        gb(hb),
        s.rss_start,
        s.rss_wave,
        s.rss_end,
        crate::metrics::peak_rss_gb()
    );
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

/// THE MEMORY CENSUS between frames (`[memory]`): what the structures
/// allocate, by part, per visited state and entry, beside the resident set.
pub fn print_memory<N: crate::search::rem::Rem>(when: &str, visited: &super::visited::VisitedSet<N>, new: &super::layer::Layer<N>, hulls: &[HullOf<N>], old: &super::wave::ByLayer<N>, outs: &super::out::OutStore, xfers: &super::edges::XferTable<N>) {
    let mut parts: Vec<(&str, usize)> = visited.census();
    let f = new.set.bytes();
    parts.push(("frontier: new states' keys, masks, index", f.iter().sum()));
    parts.push(("frontier: new states' hulls", std::mem::size_of_val(hulls)));
    parts.push(("frontier: old states", old.iter().map(super::wave::OldLayer::bytes).sum::<usize>() + old.capacity() * std::mem::size_of::<super::wave::OldLayer>()));
    parts.push(("out store: block indices", outs.index_bytes()));
    let x = xfers.bytes();
    parts.push(("transfers: pairs", x[0]));
    parts.push(("transfers: index", x[1]));
    let (v, e) = (visited.len().max(1) as f64, visited.entries().max(1) as f64);
    let total: usize = parts.iter().map(|p| p.1).sum();
    let gb = |b: usize| b as f64 / 1e9;
    eprintln!("[memory] {when}: {} states in {} entries ({:.2} states an entry), {} new and {} old in the frontier", visited.len(), visited.entries(), v / e, new.states(), super::wave::by_layer_states(old));
    eprintln!("[memory] {:<44} {:>9} {:>9} {:>9}", "part", "GB", "B/state", "B/entry");
    for (name, b) in &parts {
        eprintln!("[memory] {:<44} {:>9.3} {:>9.2} {:>9.2}", name, gb(*b), *b as f64 / v, *b as f64 / e);
    }
    let before = crate::metrics::current_rss_gb();
    // SAFETY: mimalloc's documented API, callable from any thread.
    unsafe { libmimalloc_sys::mi_collect(true) };
    let anon = crate::metrics::current_rss_gb();
    eprintln!("[memory] {:<44} {:>9.3} {:>9.2}", "counted", gb(total), total as f64 / v);
    eprintln!("[memory] {:<44} {:>9.3} {:>9.2}", "anonymous resident (RssAnon)", before, before * 1e9 / v);
    eprintln!("[memory] {:<44} {:>9.3} {:>9.2}", "  after mi_collect", anon, anon * 1e9 / v);
    match visited.hull_stats(1 << 24) {
        Ok(h) => eprintln!("[memory] hulls: {h}"),
        Err(e) => eprintln!("[memory] hulls: {e:#}"),
    }
    eprintln!("[memory] {:<44} {:>9.3} {:>9.2}", "  not counted (kernels, allocator, rest)", anon - gb(total), (anon * 1e9 - total as f64) / v);
}
