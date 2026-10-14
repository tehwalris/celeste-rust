//! THE STORAGE MICROBENCHMARK: one real frame's emissions, captured in the
//! forward (`--emit-capture DIR`: every wave, `DIR/f{F}/`; `bench-frame`
//! runs one), replayed through the REAL wave (`wave::run_wave`: the units'
//! sinks and routes, the region phase, the frame's end, the edge file) by
//! `rewrite bench-storage`, from the tree's frame before - the kernels
//! replaced by the capture (`Replay`), so a tweak to the storage is timed
//! alone.
//!
//! A capture: per wave worker `w{n}.bin`, tagged little-endian records in
//! unit order - UNIT (`1`, unit index u32, lanes u32), EMIT (`2`, shape
//! u64, key u64 u64, cell u32, lane u32 (in the unit), transfer u32
//! (`u32::MAX`: none)), MISS (`4`, length u32, a bincode `exact::MissKey`:
//! the content of the next EMIT's provisional key, re-interned by the
//! replay) and SHAPE (`5`, length u32, a bincode `exact::ShapeRecord`: a
//! shape the key space lacks) - and its transfer table `x{n}.bin`
//! (`encode_pair` each, the EMIT ids index it); `env.txt`. The lanes'
//! hulls are the frontier's (the replay's wave runs from the tree).
//! Rows are not captured: the new states' wins are decoded from their keys,
//! each shape with the tree's latest skeleton of it.

use anyhow::{ensure, Context, Result};
use std::io::Write;
use std::path::{Path, PathBuf};

use super::unit::UnitSink;
use super::visited::Key;

/// One wave worker's capture stream.
pub struct CaptureWriter {
    dir: PathBuf,
    worker: u32,
    out: std::io::BufWriter<std::fs::File>,
    /// The stream's transfers: per sink id its index, and the pairs.
    xfer_ids: rustc_hash::FxHashMap<u32, u32>,
    xfers: Vec<crate::search::arc_edges::Pair>,
}

impl CaptureWriter {
    /// The stream of `worker` at `frame`, if the run captures
    /// (`DIR/f{frame}/`).
    pub fn open(frame: u32, worker: u32, level: crate::abstraction::Level, s: &crate::settings::Settings) -> Option<Self> {
        let root = s.diag.emit_capture.as_ref()?;
        let dir = root.join(format!("f{frame:03}"));
        std::fs::create_dir_all(&dir).expect("the capture dir");
        if worker == 0 {
            std::fs::write(dir.join("env.txt"), format!("frame {frame}\nlevel {level}\nsettings {}\n", s.cart_key())).expect("the capture's env");
        }
        let f = std::fs::File::create(dir.join(format!("w{worker:03}.bin"))).expect("a capture stream");
        Some(CaptureWriter { dir, worker, out: std::io::BufWriter::with_capacity(8 << 20, f), xfer_ids: Default::default(), xfers: Vec::new() })
    }

    pub fn unit(&mut self, unit: u32, lanes: u32) {
        let mut b = [0u8; 9];
        b[0] = 1;
        b[1..5].copy_from_slice(&unit.to_le_bytes());
        b[5..9].copy_from_slice(&lanes.to_le_bytes());
        self.out.write_all(&b).expect("a capture write");
    }

    /// A tagged variable-length record (`MISS`, `SHAPE`).
    fn blob(&mut self, tag: u8, v: &impl serde::Serialize) {
        let bytes = bincode::serialize(v).expect("a capture record");
        let mut b = Vec::with_capacity(5 + bytes.len());
        b.push(tag);
        b.extend_from_slice(&(bytes.len() as u32).to_le_bytes());
        b.extend_from_slice(&bytes);
        self.out.write_all(&b).expect("a capture write");
    }

    /// A shape the key space lacks.
    pub fn shape(&mut self, r: &celeste_engine::exact::ShapeRecord) {
        self.blob(5, r);
    }

    #[inline]
    pub fn emit(&mut self, shape: u64, cell: u32, key: Key, lane: u32, xfer: Option<u32>, miss: Option<celeste_engine::exact::MissKey>) {
        if let Some(m) = miss {
            self.blob(4, &m);
        }
        let mut b = [0u8; 37];
        b[0] = 2;
        b[1..9].copy_from_slice(&shape.to_le_bytes());
        b[9..17].copy_from_slice(&key.0.to_le_bytes());
        b[17..25].copy_from_slice(&key.1.to_le_bytes());
        b[25..29].copy_from_slice(&cell.to_le_bytes());
        b[29..33].copy_from_slice(&lane.to_le_bytes());
        b[33..37].copy_from_slice(&xfer.unwrap_or(u32::MAX).to_le_bytes());
        self.out.write_all(&b).expect("a capture write");
    }

    /// Transfer `x` (the sink's id) as the stream names it: an index into
    /// the stream's own table of pairs (`pair`: its pair, on first use).
    pub fn xfer(&mut self, x: u32, pair: impl FnOnce() -> crate::search::arc_edges::Pair) -> u32 {
        let n = self.xfers.len() as u32;
        *self.xfer_ids.entry(x).or_insert_with(|| {
            self.xfers.push(pair());
            n
        })
    }

    /// The stream done; its transfer table beside it.
    pub fn finish(mut self) -> Result<()> {
        self.out.flush()?;
        let mut buf = Vec::with_capacity(self.xfers.len() * crate::search::arc_edges::PAIR_BYTES);
        for p in &self.xfers {
            crate::search::arc_edges::encode_pair(&mut buf, p);
        }
        std::fs::write(self.dir.join(format!("x{:03}.bin", self.worker)), buf)?;
        Ok(())
    }
}

/// One captured unit, located in its stream.
struct CapUnit {
    unit: u32,
    lanes: u32,
    /// The worker stream and the byte range of its records.
    stream: usize,
    records: std::ops::Range<usize>,
    emits: u64,
}

/// A capture, mapped and indexed by unit.
struct Capture {
    maps: Vec<memmap2::Mmap>,
    /// Per stream, its worker's transfers.
    xfers: Vec<Vec<crate::search::arc_edges::Pair>>,
    units: Vec<CapUnit>,
    frame: u32,
}

impl Capture {
    fn open(dir: &Path) -> Result<Self> {
        let env = std::fs::read_to_string(dir.join("env.txt")).with_context(|| format!("{}: no env.txt (not a capture)", dir.display()))?;
        let frame: u32 = env.lines().find_map(|l| l.strip_prefix("frame ")).context("env.txt: no frame")?.parse()?;
        let mut maps = Vec::new();
        let mut xfers = Vec::new();
        let mut units = Vec::new();
        for w in 0.. {
            let p = dir.join(format!("w{w:03}.bin"));
            if !p.exists() {
                break;
            }
            // SAFETY: a finished capture is never modified.
            let map = unsafe { memmap2::Mmap::map(&std::fs::File::open(&p)?)? };
            let xb = std::fs::read(dir.join(format!("x{w:03}.bin"))).with_context(|| format!("{}: no transfer table", p.display()))?;
            xfers.push(xb.chunks_exact(crate::search::arc_edges::PAIR_BYTES).map(crate::search::arc_edges::decode_pair).collect());
            let b = &map[..];
            let u32_at = |o: usize| u32::from_le_bytes(b[o..o + 4].try_into().unwrap());
            let mut pos = 0usize;
            while pos < b.len() {
                ensure!(b[pos] == 1, "{}: a unit record expected at byte {pos}", p.display());
                let (unit, lanes) = (u32_at(pos + 1), u32_at(pos + 5));
                pos += 9;
                let start = pos;
                let mut emits = 0u64;
                while pos < b.len() && b[pos] != 1 {
                    match b[pos] {
                        2 => {
                            pos += 37;
                            emits += 1;
                        }
                        4 | 5 => pos += 5 + u32_at(pos + 1) as usize,
                        t => anyhow::bail!("{}: record tag {t} at byte {pos}", p.display()),
                    }
                }
                units.push(CapUnit { unit, lanes, stream: maps.len(), records: start..pos, emits });
            }
            maps.push(map);
        }
        ensure!(!units.is_empty(), "{}: no capture streams", dir.display());
        units.sort_by_key(|u| u.unit);
        ensure!(units.iter().enumerate().all(|(i, u)| u.unit == i as u32), "{}: the captured units are not 0..{}", dir.display(), units.len());
        Ok(Capture { maps, xfers, units, frame })
    }
}

/// The capture as a `FrameStep`: a unit's "kernels" replay the captured
/// unit of the same index (the units are a function of the frontier, so
/// the replay's are the capture's).
struct Replay<'a> {
    cap: &'a Capture,
    /// Per shape, the skeleton its emissions carry (the tree's latest: the
    /// replay has no kernel templates).
    skeletons: rustc_hash::FxHashMap<u64, celeste_engine::runtime2::Rt2>,
    level: crate::abstraction::Level,
    settings: std::sync::Arc<crate::settings::Settings>,
}

impl crate::frame::FrameStep for Replay<'_> {
    fn run(&self, block: &crate::frame::Block, _cell_in: &[u32], _lanes: &[usize], sink: &mut UnitSink) -> Result<()> {
        let u = self.cap.units.get(sink.unit as usize).context("a unit the capture does not have")?;
        ensure!(u.lanes as usize == block.lanes(), "unit {}: {} lanes, the capture's has {}", u.unit, block.lanes(), u.lanes);
        sink.replaying();
        let b = &self.cap.maps[u.stream][u.records.clone()];
        let u32_at = |o: usize| u32::from_le_bytes(b[o..o + 4].try_into().unwrap());
        let u64_at = |o: usize| u64::from_le_bytes(b[o..o + 8].try_into().unwrap());
        let mut pos = 0usize;
        // The next emission's provisional key, re-interned (a MISS record).
        let mut miss: Option<Key> = None;
        while pos < b.len() {
            match b[pos] {
                4 | 5 => {
                    let n = u32_at(pos + 1) as usize;
                    let bytes = &b[pos + 5..pos + 5 + n];
                    if b[pos] == 4 {
                        miss = Some(sink.provisional().intern(bincode::deserialize(bytes)?));
                    } else {
                        sink.note_shape(&bincode::deserialize(bytes)?);
                    }
                    pos += 5 + n;
                    continue;
                }
                _ => {}
            }
            let (shape, key, cell, lane, x) = (u64_at(pos + 1), (u64_at(pos + 9), u64_at(pos + 17)), u32_at(pos + 25), u32_at(pos + 29), u32_at(pos + 33));
            let key = miss.take().unwrap_or(key);
            pos += 37;
            let xfer = (x != u32::MAX).then(|| sink.xfer_id(self.cap.xfers[u.stream][x as usize]));
            let skel = self.skeletons.get(&shape).with_context(|| format!("the captured shape {shape:016x} is in no frame of the tree: the replay has no skeleton for it"))?;
            sink.emit(shape, cell, key, lane as usize, xfer, || skel.clone_block(), |_| {})?;
        }
        Ok(())
    }

    fn level(&self) -> crate::abstraction::Level {
        self.level
    }

    fn settings(&self) -> std::sync::Arc<crate::settings::Settings> {
        self.settings.clone()
    }
}

/// What `rewrite bench-storage` runs.
pub struct BenchArgs<'a> {
    pub capture: &'a Path,
    pub tree: &'a Path,
    pub level: crate::abstraction::Level,
    /// The run's settings (the capture's: threads, the room).
    pub settings: &'a crate::settings::Settings,
    pub reps: usize,
}

/// Replay a capture `reps` times (module doc), printing per rep the wave's
/// phases (the units - the sinks over the captured emissions - the region
/// phase, the frame's end) and its fingerprints as `rewrite ckhash`
/// computes them (its `f{frame}` and `e{frame}` lines).
pub fn bench_storage(a: &BenchArgs) -> Result<()> {
    use std::time::Instant;
    let t = Instant::now();
    let cap = Capture::open(a.capture)?;
    let frame = cap.frame;
    let emits: u64 = cap.units.iter().map(|u| u.emits).sum();
    eprintln!("[bench-storage] capture of f{frame}: {} units, {} streams, {emits} emissions, mapped in {:.1} s", cap.units.len(), cap.maps.len(), t.elapsed().as_secs_f64());
    let t = Instant::now();
    let mut st = crate::frame::ForwardState::at(a.tree, frame - 1, true, a.level, a.settings)?;
    eprintln!("[bench-storage] the tree at f{} ({} states, {} states in) in {:.1} s", frame - 1, st.visited_len(), st.frontier().states(), t.elapsed().as_secs_f64());
    // Every shape's skeleton: the latest frame's that holds it.
    let tree = super::layer::Tree::open(a.tree, frame - 1)?;
    let mut skeletons: rustc_hash::FxHashMap<u64, celeste_engine::runtime2::Rt2> = Default::default();
    for f in (0..frame).rev() {
        for r in tree.file(f).iter().flat_map(|x| &x.header.skeletons) {
            if !skeletons.contains_key(&r.shape_hash) {
                skeletons.insert(r.shape_hash, r.rt2_in(a.settings.room)?);
            }
        }
    }
    let engine = Replay { cap: &cap, skeletons, level: a.level, settings: std::sync::Arc::new(a.settings.clone()) };
    let scratch = std::env::temp_dir().join(format!("celeste-bench-storage-{}", std::process::id()));
    let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
    for rep in 0..a.reps {
        let _ = std::fs::remove_dir_all(&scratch);
        let t = Instant::now();
        let (wave, visited, xfers) = st.bench_wave(&engine, a.tree, Some(&scratch), rep + 1 < a.reps)?;
        let all = t.elapsed();
        let w = &wave.stats;
        let (n, acc) = wave.next.content_fingerprint(&visited.keys, visited.shapes())?;
        let (ne, ae) = edge_fingerprint(&scratch, frame, &xfers, &visited, a.settings.threads())?;
        println!(
            "[bench-storage] rep {rep}: units {:.0} ms (idle {:.0}%, regions {:.0} worker-ms) end {:.0} total {:.0} ms ({} threads; {} slices, {} routes, {} bundles, {:.3} B an edge) | f{frame:03} {n} {acc:016x} | e{frame:03} {ne} {ae:016x}",
            ms(w.t_wave),
            w.wave_idle * 100.0,
            ms(w.t_regions),
            ms(w.t_layer),
            ms(all),
            a.settings.threads(),
            w.slices,
            w.routes,
            w.edge_bundles,
            w.edge_bytes as f64 / w.edge_records.max(1) as f64,
        );
    }
    let _ = std::fs::remove_dir_all(&scratch);
    Ok(())
}

/// `rewrite ckhash --edges`'s `e{frame}` line over the frame's edge file in
/// `edges_dir` (`EdgeHashes::frame`; `xfers`: the merged table in memory,
/// the dir's file may hold only the frame's new pairs).
pub fn edge_fingerprint(edges_dir: &Path, frame: u32, xfers: &super::edges::XferTable, visited: &super::visited::VisitedSet, threads: usize) -> Result<(u64, u64)> {
    let store = super::edges::EdgeStore::open(edges_dir, frame)?;
    EdgeHashes::new(&xfers.pairs).frame(&store, frame, &visited.keys.content_hash(), visited.shapes(), threads)
}

/// `ckhash --edges`'s per-edge term: `mix64(node(dst) ^ node(src) <<< 21 ^
/// pair <<< 42)`, a node `ContentHash::state` of its (shape, key, cell)
/// (`mix64(key hash ^ cell << 1)`), a pair the hash of its bytes - summed
/// over a frame's edges, with their count. Each pair hashed once, each key
/// once a block (its dictionaries').
pub struct EdgeHashes {
    pairs: Vec<u64>,
}

impl EdgeHashes {
    pub fn new(pairs: &[crate::search::arc_edges::Pair]) -> Self {
        let mix = celeste_engine::runtime2::mix64;
        EdgeHashes {
            pairs: pairs
                .iter()
                .map(|pair| {
                    let mut b = Vec::new();
                    crate::search::arc_edges::encode_pair(&mut b, pair);
                    b.iter().fold(0u64, |h, &x| mix(h ^ x as u64))
                })
                .collect(),
        }
    }

    /// Frame `frame`'s edges of `store`: `(count, sum of terms)` (`shapes`:
    /// the tree's by index).
    pub fn frame(&self, store: &super::edges::EdgeStore, frame: u32, content: &celeste_engine::exact::ContentHash, shapes: &[u64], threads: usize) -> Result<(u64, u64)> {
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

/// One `bench-frame` rep for its COST REPORT (`print_cost`): the frame's
/// stats (`--phases` on: `FrameStats::phase_s`), its checkpoint's steps
/// and the pos graph's wall (`--checkpoint`), the wave's workers, the
/// whole rep's wall, the machine's load average at its start and the
/// rep's hardware counts (`Counters`).
pub struct Rep {
    pub stats: crate::frame::FrameStats,
    pub ckpt: Option<(crate::frame::Checkpointed, std::time::Duration)>,
    pub workers: usize,
    pub wall: std::time::Duration,
    pub load: f64,
    pub counts: Option<[u64; 4]>,
}

/// HARDWARE COUNTERS over a rep, every thread it starts included (Linux
/// `perf_event_open`, `inherit`: a thread's counts join its parent's when
/// it exits, and a frame's threads are all joined by its end): user-mode
/// instructions, user-mode cycles, kernel-mode cycles, page faults. Instructions do
/// not move with the machine's load as walls do (a shared machine's other
/// jobs slow a rep, they do not add to its instructions). `None` where the
/// kernel refuses (`perf_event_paranoid`).
pub struct Counters {
    fds: [i32; 4],
}

impl Counters {
    /// The counters, disabled (open them before the threads they count start).
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

    /// Counting; the counts so far (`stop` takes the difference: a reset
    /// would not reset the exited threads' counts, which `inherit` keeps
    /// apart).
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
    pub fn read(&self) -> [u64; 4] {
        let mut out = [0u64; 4];
        for (o, &fd) in out.iter_mut().zip(&self.fds) {
            // SAFETY: an open perf event fd: a plain read of its u64 count.
            unsafe { libc::read(fd, o as *mut u64 as *mut libc::c_void, 8) };
        }
        out
    }
}

/// The counters a bench installed for the frame's STEPS (`step_counts`):
/// the frame end's and the checkpoint's steps read them at their marks.
static STEPS: std::sync::OnceLock<Counters> = std::sync::OnceLock::new();

/// Install `c` as the step counters (once a process); the installed ones.
pub fn count_steps(c: Counters) -> &'static Counters {
    STEPS.get_or_init(|| c)
}

/// The step counters' counts now (`None`: no bench installed them). Read
/// on the thread that opened them, between steps (each step's threads
/// joined: their counts are in).
pub fn step_counts() -> Option<[u64; 4]> {
    STEPS.get().map(Counters::read)
}

/// The counts between two `step_counts`.
pub fn counts_since(at: Option<[u64; 4]>) -> ([u64; 4], Option<[u64; 4]>) {
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

/// A report line: its label, per rep its seconds, the element its cost is
/// per (name, count per rep), and per rep its counts (`Counters`; a step's).
struct Line {
    label: String,
    secs: Vec<f64>,
    per: Option<(&'static str, Vec<f64>)>,
    counts: Option<Vec<[u64; 4]>>,
}

/// THE COST REPORT of a frame's reps (`bench-frame`; plans/fwd-bench.md):
/// the frame's wall split into the wave, the frame end and the
/// checkpoint; the wave's WORKER-SECONDS by phase (`frame::phases`) with
/// the idle share; the frame end's and the checkpoint's steps (walls, on
/// one thread or all); each with its cost per element (ns a lane in, an
/// emission, a re-pushed edge, a push, a new state, ...); the bytes
/// written and the memory. Min and max over the reps (a shared machine:
/// quote the min, with the load).
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

/// The bytes of every file under `dir` (recursively, by relative path):
/// `(files, bytes, hash)` - `bench-frame`'s `b` line, so a change to how a
/// frame's files are written proves them byte-identical.
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

/// THE MEMORY CENSUS of a forward between frames (`bench-frame`'s
/// `[memory]`): what its structures allocate, by part, against the visited
/// set's states and entries, beside the process's resident set (the rest:
/// the kernels, the allocator's retained and fragmented pages, what no
/// part counts).
pub fn print_memory(when: &str, visited: &super::visited::VisitedSet, new: &super::layer::Layer, hulls: &[crate::search::hull::Hull], old: &super::wave::ByLayer, outs: &super::out::OutStore, xfers: &super::edges::XferTable) {
    let mut parts: Vec<(&str, usize)> = visited.census();
    let f = new.set.bytes();
    parts.push(("frontier: new states' keys, masks, index", f.iter().sum()));
    parts.push(("frontier: new states' hulls", std::mem::size_of_val(hulls)));
    parts.push(("frontier: old states (48 B each)", old.iter().map(|x| x.1.capacity() * std::mem::size_of::<super::wave::Old>()).sum::<usize>() + old.capacity() * 32));
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
