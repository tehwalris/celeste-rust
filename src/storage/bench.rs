//! THE STORAGE MICROBENCHMARK: one real frame's emissions, captured in the
//! forward (`CELESTE_EMIT_CAPTURE=DIR`, optionally `CELESTE_EMIT_CAPTURE_FRAME=F`),
//! replayed through the REAL storage code (`UnitSink`, the translation, the
//! layer's gather, the edge file) by `rewrite bench-storage`, against the
//! tree's visited set at the frame before - without the kernels, so a tweak
//! to the storage is timed alone.
//!
//! A capture: per wave worker `w{n}.bin`, tagged little-endian records in
//! unit order - UNIT (`1`, unit index u32, block u32, first lane u64, source
//! count u32, sources old u8, the source ids u64 each), EMIT (`2`, shape u64,
//! key u64 u64, cell u32, lane u32 (from the unit's first), transfer u32
//! (`u32::MAX`: none)), DROP (`3`, lane u32, from u32) - and its transfer
//! table `x{n}.bin` (`encode_pair` each, the EMIT ids index it); `env.txt`.
//! Rows are not captured: the replay copies a row of 16 numbers (the key
//! and cell spread over them), about a real row's size.

use anyhow::{ensure, Context, Result};
use std::io::Write;
use std::path::{Path, PathBuf};

use super::unit::{RowBuf, UnitSink};
use super::visited::{Key, VisitedSet};
use super::StateId;

/// The capture directory and frame (`CELESTE_EMIT_CAPTURE`,
/// `CELESTE_EMIT_CAPTURE_FRAME`: any frame when unset).
fn capture_target() -> Option<&'static (PathBuf, Option<u32>)> {
    static T: std::sync::OnceLock<Option<(PathBuf, Option<u32>)>> = std::sync::OnceLock::new();
    T.get_or_init(|| {
        let dir = std::env::var_os("CELESTE_EMIT_CAPTURE")?;
        let frame = std::env::var("CELESTE_EMIT_CAPTURE_FRAME").ok().map(|v| v.parse().expect("CELESTE_EMIT_CAPTURE_FRAME: a frame"));
        Some((PathBuf::from(dir), frame))
    })
    .as_ref()
}

/// One wave worker's capture stream.
pub struct CaptureWriter {
    dir: PathBuf,
    worker: u32,
    out: std::io::BufWriter<std::fs::File>,
}

impl CaptureWriter {
    /// The stream of `worker` at `frame`, if this frame is captured
    /// (`DIR/f{frame}/`).
    pub fn open(frame: u32, worker: u32) -> Option<Self> {
        let (root, only) = capture_target()?;
        if only.is_some_and(|f| f != frame) {
            return None;
        }
        let dir = root.join(format!("f{frame:03}"));
        std::fs::create_dir_all(&dir).expect("the capture dir");
        if worker == 0 {
            let vars: Vec<String> = std::env::vars().filter(|(k, _)| k.starts_with("CELESTE_")).map(|(k, v)| format!("{k}={v}")).collect();
            std::fs::write(dir.join("env.txt"), format!("frame {frame}\nlevel {}\n{}\n", crate::abstraction::current_level(), vars.join("\n"))).expect("the capture's env");
        }
        let f = std::fs::File::create(dir.join(format!("w{worker:03}.bin"))).expect("a capture stream");
        Some(CaptureWriter { dir, worker, out: std::io::BufWriter::with_capacity(8 << 20, f) })
    }

    pub fn unit(&mut self, unit: u32, block: u32, lo: usize, sources: &[StateId], old: bool) {
        let mut b = Vec::with_capacity(22 + 8 * sources.len());
        b.push(1u8);
        b.extend_from_slice(&unit.to_le_bytes());
        b.extend_from_slice(&block.to_le_bytes());
        b.extend_from_slice(&(lo as u64).to_le_bytes());
        b.extend_from_slice(&(sources.len() as u32).to_le_bytes());
        b.push(old as u8);
        for s in sources {
            b.extend_from_slice(&s.to_le_bytes());
        }
        self.out.write_all(&b).expect("a capture write");
    }

    #[inline]
    pub fn emit(&mut self, shape: u64, cell: u32, key: Key, lane: u32, xfer: Option<u32>) {
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

    #[inline]
    pub fn dropped(&mut self, lane: u32, from: u32) {
        let mut b = [0u8; 9];
        b[0] = 3;
        b[1..5].copy_from_slice(&lane.to_le_bytes());
        b[5..9].copy_from_slice(&from.to_le_bytes());
        self.out.write_all(&b).expect("a capture write");
    }

    /// The stream done; the worker's transfer table beside it.
    pub fn finish(mut self, xfers: &[crate::search::arc_edges::Pair]) -> Result<()> {
        self.out.flush()?;
        let mut buf = Vec::with_capacity(xfers.len() * crate::search::arc_edges::PAIR_BYTES);
        for p in xfers {
            crate::search::arc_edges::encode_pair(&mut buf, p);
        }
        std::fs::write(self.dir.join(format!("x{:03}.bin", self.worker)), buf)?;
        Ok(())
    }
}

/// One captured unit, located in its stream.
struct CapUnit {
    unit: u32,
    block: u32,
    lo: u64,
    old: bool,
    sources: Vec<StateId>,
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
                let (unit, block) = (u32_at(pos + 1), u32_at(pos + 5));
                let lo = u64::from_le_bytes(b[pos + 9..pos + 17].try_into().unwrap());
                let n = u32_at(pos + 17) as usize;
                let old = b[pos + 21] != 0;
                let sources: Vec<StateId> = (0..n).map(|k| u64::from_le_bytes(b[pos + 22 + 8 * k..pos + 30 + 8 * k].try_into().unwrap())).collect();
                pos += 22 + 8 * n;
                let start = pos;
                let mut emits = 0u64;
                while pos < b.len() && b[pos] != 1 {
                    match b[pos] {
                        2 => {
                            pos += 37;
                            emits += 1;
                        }
                        3 => pos += 9,
                        t => anyhow::bail!("{}: record tag {t} at byte {pos}", p.display()),
                    }
                }
                units.push(CapUnit { unit, block, lo, old, sources, stream: maps.len(), records: start..pos, emits });
            }
            maps.push(map);
        }
        ensure!(!units.is_empty(), "{}: no capture streams", dir.display());
        units.sort_by_key(|u| u.unit);
        Ok(Capture { maps, xfers, units, frame })
    }
}

/// The replay's row of a state: 16 numbers (the key's words and the cell),
/// in a synthetic block of the shape.
fn bench_skeleton(shape: u64) -> celeste_engine::runtime2::Rt2 {
    use celeste_engine::runtime2::{Cell2, Col, Rt2};
    let (cart, cache) = crate::compiled::room_context().expect("the room's cart");
    let mut b = Rt2::empty(0, 0, &[], cart, cache);
    b.structure = vec![Cell2::Val; 16];
    b.cols = (0..16).map(|_| Col::N(Vec::new())).collect();
    b.shape_hash = shape;
    b
}

fn push_bench_row(buf: &mut RowBuf, key: Key, cell: u32) {
    use super::unit::TCol;
    let words = [key.0 as u32, (key.0 >> 32) as u32, key.1 as u32, (key.1 >> 32) as u32, cell];
    for (i, (_, col)) in buf.cols.iter_mut().enumerate() {
        if let TCol::Num(v) = col {
            v.push(words[i % words.len()].rotate_left(i as u32));
        }
    }
    buf.keys.push(key);
    buf.cells.push(cell);
}

/// What `rewrite bench-storage` runs.
pub struct BenchArgs<'a> {
    pub capture: &'a Path,
    pub tree: &'a Path,
    pub threads: usize,
    pub phase: &'a str,
    pub reps: usize,
}

/// Replay a capture `reps` times (module doc), printing per rep the phases'
/// wall times and the validation: emissions, requests, lids, new states and
/// edges, and fingerprints of the new states and of the edge set computed as
/// `rewrite ckhash` computes them (its `f{frame}` and `e{frame}` lines).
pub fn bench_storage(a: &BenchArgs) -> Result<()> {
    use std::time::Instant;
    let t = Instant::now();
    let cap = Capture::open(a.capture)?;
    let frame = cap.frame;
    let emits: u64 = cap.units.iter().map(|u| u.emits).sum();
    eprintln!("[bench-storage] capture of f{frame}: {} units, {} streams, {emits} emissions, mapped in {:.1} s", cap.units.len(), cap.maps.len(), t.elapsed().as_secs_f64());
    let t = Instant::now();
    let (base, _) = crate::frame::restore_visited(a.tree, frame - 1)?;
    let base_xfers = super::edges::XferTable::load(&a.tree.join("edges"))?;
    eprintln!("[bench-storage] the tree's visited set at f{} ({} states, {} entries) in {:.1} s", frame - 1, base.len(), base.entries(), t.elapsed().as_secs_f64());
    let (do_translate, do_edges) = match a.phase {
        "units" => (false, false),
        "translate" => (true, false),
        "all" => (true, true),
        p => anyhow::bail!("--phase {p}: units, translate or all"),
    };
    let scratch = std::env::temp_dir().join(format!("celeste-bench-storage-{}", std::process::id()));
    for rep in 0..a.reps {
        let mut visited = base.clone();
        let mut xfers = base_xfers.clone();
        let _ = std::fs::remove_dir_all(&scratch);
        // THE UNITS: each replayed on a worker's sink, heaviest first.
        let t0 = Instant::now();
        let mut order: Vec<usize> = (0..cap.units.len()).collect();
        order.sort_by_key(|&i| std::cmp::Reverse(cap.units[i].emits));
        let next = std::sync::atomic::AtomicUsize::new(0);
        let shared: &VisitedSet = &visited;
        type Done = (Vec<super::unit::UnitOut>, Vec<crate::search::arc_edges::Pair>, u64, u64);
        let done: Vec<Done> = std::thread::scope(|scope| {
            let hs: Vec<_> = (0..a.threads)
                .map(|w| {
                    let (cap, order, next) = (&cap, &order, &next);
                    scope.spawn(move || -> Result<Done> {
                        let mut sink = UnitSink::new(shared, crate::frame::Filters::default(), frame, w as u32, true, false, None, None);
                        let mut xmap: Vec<Vec<u32>> = vec![Vec::new(); cap.maps.len()];
                        loop {
                            let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                            let Some(&ui) = order.get(i) else { break };
                            let u = &cap.units[ui];
                            sink.begin(u.unit, u.block, u.lo as usize, &u.sources, None, u.old);
                            replay_unit(&mut sink, cap, u, &mut xmap[u.stream])?;
                            sink.end();
                        }
                        Ok((std::mem::take(&mut sink.outs), std::mem::take(&mut sink.xfer_tab), sink.n_requests, sink.n_lids))
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("a bench worker panicked")).collect::<Result<Vec<_>>>()
        })?;
        let t_units = t0.elapsed();
        let (mut outs, mut tables, mut requests, mut lids) = (Vec::new(), Vec::new(), 0u64, 0u64);
        for (o, x, r, l) in done {
            outs.extend(o);
            tables.push(x);
            requests += r;
            lids += l;
        }
        // Units in the capture's order, as the wave numbers them.
        outs.sort_by_key(|o: &super::unit::UnitOut| o.sources.first().copied());
        let edges: u64 = outs.iter().map(|o| o.edges).sum();
        let mut line = format!("[bench-storage] rep {rep}: units {:.3} s ({} threads; requests {requests} lids {lids} edges {edges})", t_units.as_secs_f64(), a.threads);
        if do_translate {
            let t = Instant::now();
            let (news, _meta) = super::wave::translate(&mut visited, &mut outs, frame)?;
            let t_tr = t.elapsed();
            let t = Instant::now();
            let layer = super::wave::gather_layer(&visited, &outs, &news, frame, 0)?;
            let t_layer = t.elapsed();
            line += &format!(" | translate {:.3} s, new {} | layer {:.3} s, {} pieces", t_tr.as_secs_f64(), news.len(), t_layer.as_secs_f64(), layer.len());
            // The new states' fingerprint, as `ckhash`'s `f` line.
            let mut acc = 0u64;
            for b in &layer {
                for (k, id) in b.keys().iter().zip(b.ids()) {
                    let c = super::id_cell(&visited.geo, *id);
                    acc = acc.wrapping_add(celeste_engine::runtime2::mix64(k.0 ^ celeste_engine::runtime2::mix64(k.1 ^ (c as u64) << 1)));
                }
            }
            line += &format!(" | f{frame:03} {} {acc:016x}", news.len());
            if do_edges {
                let t = Instant::now();
                let remaps = xfers.merge(Some(&scratch), &tables)?;
                let bytes = super::edges::write_file(&super::edges::file_path(&scratch, frame, None), frame, &outs, remaps)?;
                let t_e = t.elapsed();
                line += &format!(" | edge file {:.3} s, {:.2} B an edge", t_e.as_secs_f64(), bytes as f64 / edges.max(1) as f64);
                // The edges' fingerprint, as `ckhash --edges`'s `e` line.
                let store = super::edges::EdgeStore::open(&scratch, frame)?;
                let node = |id: StateId| -> u64 {
                    let r = super::id_region(id);
                    let shape = visited.shape_hash(r / visited.geo.slots);
                    let k = visited.table(r).expect("a region of the set").key(super::id_entry(id));
                    let mix = celeste_engine::runtime2::mix64;
                    mix(k.0 ^ mix(k.1 ^ mix(shape ^ super::id_cell(&visited.geo, id) as u64)))
                };
                let (mut n, mut acc) = (0u64, 0u64);
                store.scan(frame, |e| {
                    let pair = store.pair(e.xfer).expect("a transfer of the table");
                    let mut b = Vec::new();
                    crate::search::arc_edges::encode_pair(&mut b, &pair);
                    let p = b.iter().fold(0u64, |h, &x| celeste_engine::runtime2::mix64(h ^ x as u64));
                    acc = acc.wrapping_add(celeste_engine::runtime2::mix64(node(e.dst) ^ node(e.src).rotate_left(21) ^ p.rotate_left(42)));
                    n += 1;
                });
                line += &format!(" | e{frame:03} {n} {acc:016x}");
            }
        }
        println!("{line}");
    }
    let _ = std::fs::remove_dir_all(&scratch);
    Ok(())
}

/// Replay one unit's records into the sink (`xmap`: the stream's transfer
/// ids as the sink's, filled on first use).
fn replay_unit(sink: &mut UnitSink, cap: &Capture, u: &CapUnit, xmap: &mut Vec<u32>) -> Result<()> {
    let b = &cap.maps[u.stream][u.records.clone()];
    let u32_at = |o: usize| u32::from_le_bytes(b[o..o + 4].try_into().unwrap());
    let u64_at = |o: usize| u64::from_le_bytes(b[o..o + 8].try_into().unwrap());
    let mut pos = 0usize;
    while pos < b.len() {
        if b[pos] == 3 {
            sink.dropped(u.lo as usize + u32_at(pos + 1) as usize, u32_at(pos + 5));
            pos += 9;
            continue;
        }
        let (shape, key, cell, lane, x) = (u64_at(pos + 1), (u64_at(pos + 9), u64_at(pos + 17)), u32_at(pos + 25), u32_at(pos + 29), u32_at(pos + 33));
        pos += 37;
        let xfer = (x != u32::MAX).then(|| {
            if xmap.len() <= x as usize {
                xmap.resize(x as usize + 1, u32::MAX);
            }
            if xmap[x as usize] == u32::MAX {
                xmap[x as usize] = sink.xfer_id(cap.xfers[u.stream][x as usize]);
            }
            xmap[x as usize]
        });
        sink.emit(shape, cell, key, u.lo as usize + lane as usize, xfer, || bench_skeleton(shape), |buf| push_bench_row(buf, key, cell))?;
    }
    Ok(())
}
