//! THE STORAGE MICROBENCHMARK: one real frame's emissions, captured in the
//! forward (`--emit-capture DIR`: every wave, `DIR/f{F}/`; `bench-frame`
//! runs one), replayed through the REAL wave (`wave::run_wave`: the units'
//! sinks and routes, the region phase, the frame's end, the edge file) by
//! `rewrite bench-storage`, from the tree's frame before - the kernels
//! replaced by the capture (`Replay`), so a tweak to the storage is timed
//! alone.
//!
//! A capture: per wave worker `w{n}.bin`, tagged little-endian records in
//! unit order - UNIT (`1`, unit index u32, block u32, first lane u64, source
//! count u32, sources old u8, the source ids u64 each), EMIT (`2`, shape u64,
//! key u64 u64, cell u32, lane u32 (from the unit's first), transfer u32
//! (`u32::MAX`: none)), DROP (`3`, lane u32, from u32), MISS (`4`, length
//! u32, a bincode `exact::MissKey`: the content of the next EMIT's
//! provisional key, re-interned by the replay) and SHAPE (`5`, length u32,
//! a bincode `exact::ShapeRecord`: a shape the key space lacks) - and its transfer
//! table `x{n}.bin` (`encode_pair` each, the EMIT ids index it); `env.txt`.
//! Rows are not captured: the new layer is decoded from the keys, each
//! shape with its first frontier piece's skeleton.

use anyhow::{ensure, Context, Result};
use std::io::Write;
use std::path::{Path, PathBuf};

use super::unit::UnitSink;
use super::visited::{Key, VisitedSet};
use super::StateId;

/// One wave worker's capture stream.
pub struct CaptureWriter {
    dir: PathBuf,
    worker: u32,
    out: std::io::BufWriter<std::fs::File>,
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
                        4 | 5 => pos += 5 + u32_at(pos + 1) as usize,
                        t => anyhow::bail!("{}: record tag {t} at byte {pos}", p.display()),
                    }
                }
                units.push(CapUnit { unit, block, lo, stream: maps.len(), records: start..pos, emits });
            }
            maps.push(map);
        }
        ensure!(!units.is_empty(), "{}: no capture streams", dir.display());
        units.sort_by_key(|u| u.unit);
        Ok(Capture { maps, xfers, units, frame })
    }
}

/// The capture as a `FrameStep`: a unit's "kernels" replay the captured
/// unit with the same block and first lane (the replay runs at the
/// capture's thread count, so the wave cuts the same units).
struct Replay<'a> {
    cap: &'a Capture,
    /// Captured units by (frontier block, first lane).
    by_start: rustc_hash::FxHashMap<(u32, u64), usize>,
    /// The frontier's blocks by (shape, seq): their index in it.
    blocks: rustc_hash::FxHashMap<(u64, u32), u32>,
    /// Per shape, the skeleton its emissions carry (its first frontier
    /// piece's: the replay has no kernel templates).
    skeletons: rustc_hash::FxHashMap<u64, celeste_engine::runtime2::Rt2>,
    level: crate::abstraction::Level,
    settings: std::sync::Arc<crate::settings::Settings>,
}

impl crate::frame::FrameStep for Replay<'_> {
    fn run(&self, block: &crate::frame::Block, _cell_in: &[u32], lanes: &[usize], sink: &mut UnitSink) -> Result<()> {
        let bi = *self.blocks.get(&(block.shard_shape(), block.seq())).context("a frontier block the capture does not know")?;
        let first = *lanes.first().context("an empty unit")? as u64;
        let &ui = self.by_start.get(&(bi, first)).with_context(|| format!("no captured unit starts at lane {first} of block {bi}: replay at the capture's thread count"))?;
        let u = &self.cap.units[ui];
        sink.replaying();
        let b = &self.cap.maps[u.stream][u.records.clone()];
        let u32_at = |o: usize| u32::from_le_bytes(b[o..o + 4].try_into().unwrap());
        let u64_at = |o: usize| u64::from_le_bytes(b[o..o + 8].try_into().unwrap());
        let mut pos = 0usize;
        // The next emission's provisional key, re-interned (a MISS record).
        let mut miss: Option<Key> = None;
        while pos < b.len() {
            match b[pos] {
                3 => {
                    sink.dropped(u.lo as usize + u32_at(pos + 1) as usize, u32_at(pos + 5));
                    pos += 9;
                    continue;
                }
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
            let skel = self.skeletons.get(&shape).with_context(|| format!("the captured shape {shape:016x} is not in the frontier: the replay has no skeleton for it"))?;
            sink.emit(shape, cell, key, u.lo as usize + lane as usize, xfer, || skel.clone_block(), |_| {})?;
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
    let st = crate::frame::ForwardState::at(a.tree, frame - 1, true, a.level, a.settings)?;
    eprintln!("[bench-storage] the tree at f{} ({} states, {} lanes in) in {:.1} s", frame - 1, st.visited_len(), st.frontier_lanes(), t.elapsed().as_secs_f64());
    let (blocks, skeletons) = st.frontier_index();
    let by_start = cap.units.iter().enumerate().map(|(i, u)| ((u.block, u.lo), i)).collect();
    let engine = Replay { cap: &cap, by_start, blocks, skeletons, level: a.level, settings: std::sync::Arc::new(a.settings.clone()) };
    let scratch = std::env::temp_dir().join(format!("celeste-bench-storage-{}", std::process::id()));
    let ms = |d: std::time::Duration| d.as_secs_f64() * 1e3;
    for rep in 0..a.reps {
        let _ = std::fs::remove_dir_all(&scratch);
        let t = Instant::now();
        let (wave, visited, xfers) = st.bench_wave(&engine, a.tree, Some(&scratch))?;
        let all = t.elapsed();
        let w = &wave.stats;
        let (n, acc) = layer_fingerprint(&wave.next, &visited);
        let (ne, ae) = edge_fingerprint(&scratch, frame, &xfers, &visited, a.settings.threads())?;
        println!(
            "[bench-storage] rep {rep}: units {:.0} ms (idle {:.0}%) regions {:.0} end {:.0} total {:.0} ms ({} threads; {} slices, {} routes, {} bundles, {:.3} B an edge) | f{frame:03} {n} {acc:016x} | e{frame:03} {ne} {ae:016x}",
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

/// `rewrite ckhash`'s `f{frame}` line over a new layer: its states'
/// count and the order-independent sum of their (key, cell) hashes.
pub fn layer_fingerprint(layer: &[crate::frame::Block], visited: &VisitedSet) -> (u64, u64) {
    let content = visited.keys.content_hash();
    let (mut n, mut acc) = (0u64, 0u64);
    for b in layer {
        for (k, id) in b.keys().iter().zip(b.ids()) {
            acc = acc.wrapping_add(content.state(b.shard_shape(), *k, super::id_cell(&visited.geo, *id)));
            n += 1;
        }
    }
    (n, acc)
}

/// `rewrite ckhash --edges`'s `e{frame}` line over the frame's edge file in
/// `edges_dir` (`EdgeHashes::frame`; `xfers`: the merged table in memory,
/// the dir's file may hold only the frame's new pairs).
pub fn edge_fingerprint(edges_dir: &Path, frame: u32, xfers: &super::edges::XferTable, visited: &VisitedSet, threads: usize) -> Result<(u64, u64)> {
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
