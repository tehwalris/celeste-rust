//! `sweep`: approach #2, a MULTITHREADED COLLECTIVE SWEEP FRONT doing the work
//! production's dedup path does (DESIGNS.md "Sweep front"):
//! - every emission (lookup or level -1 drop) checks the level -1 table
//!   (a dense per-(shape, cell) array here; production: an FxHashMap behind
//!   a run cache), drops note their source (`DropNotes`' min `from`);
//! - every kept emission becomes an EDGE RECORD (source, target, transfer):
//!   12 B (source row index, target id, transfer id) into per-thread 1M-edge
//!   segments of one arena, staged in L1 and written with non-temporal
//!   stores (production: 16-B words, delta/varint-coded per 64k chunk, to a
//!   file per (layer, worker); see `vprod`);
//! - every NEW state's 64-B row is written once, at its insert, into the
//!   inserting thread's piece (production pushes every non-cached emission's
//!   row into a queue and gathers the new ones at the flush; here the
//!   decision is immediate, so the push IS the gather);
//! - the pos-graph edges (source cell, target cell), per thread, behind a
//!   last-pair cache as production's;
//! - the frame's end (`end_frame`): new states CANONICALLY renumbered by
//!   (shard, key), the table's ids rewritten, a provisional -> canonical map
//!   kept for the edges (production's `Renumber`).
//!
//! The sweep: the lookups (prep/q.bin) and drops (sweep/d2.bin) in SOURCE
//! order, x-major, cut into small chunks at source boundaries; all threads
//! claim chunks from ONE monotone counter (`front=shared`) or one per CCD
//! over two disjoint x-bands (`front=ccd`, stealing the other band's tail).
//! Per chunk: a per-thread L2 cache of recent keys (design G; in-chunk
//! duplicates resolve to the first one's pending lookup), the misses
//! resolved by the `Probe` (probe.rs: posmask8 by default, lock-free; the
//! keyed tables v4/exact insert under a per-shard spinlock), then rows and
//! edges written.
//!
//! Usage: sweep DIR prep | sweep DIR run [threads=16] [front=shared|ccd]
//!   [probe=pm8|v4|exact] [chunk=1024] [cache=12] [split=0.5] [verify=0|1]
//!   [prefault=0|1] [read=0|1] [cpus=0,1,...]

#[path = "../../canon.rs"]
mod canon;
mod probe;

use probe::{Exact, NewState, Pending, Pm8, Probe, ProbeStats, V4};
use rustc_hash::{FxHashMap, FxHashSet};
use std::sync::atomic::{AtomicU32, AtomicU64, AtomicUsize, Ordering};
use std::time::Instant;

const REC: usize = 48;
const GRID: i32 = 512;
const ORIGIN: i32 = -64;
/// Level -1's `from` for a dropped cell, as `vprod` builds it (57 + 1000).
const DROPPED_FROM: u32 = 57 + 1000;
/// Provisional new ids: `n_door + thread * CAP + local`.
const CAP: u32 = 1 << 23;
const PEND: u32 = 1 << 31;
const SKIP: u32 = u32::MAX;
/// Edges a segment / staged before a non-temporal flush.
const SEG_E: usize = 1 << 20;
const STAGE: usize = 1024;
/// Rows a segment.
const SEG_R: usize = 1 << 16;

#[repr(C)]
#[derive(Clone, Copy)]
struct Q {
    shard: u32,
    src: u32,
    xfer: u32,
    _p: u32,
    key: u128,
}

#[repr(C)]
#[derive(Clone, Copy)]
struct D2 {
    src: u32,
    shard: u32,
}

#[repr(C)]
#[derive(Clone, Copy, Default)]
struct Edge {
    src: u32,
    tgt: u32,
    xfer: u32,
}

fn as_bytes<T>(v: &[T]) -> &[u8] {
    unsafe { std::slice::from_raw_parts(v.as_ptr() as *const u8, std::mem::size_of_val(v)) }
}
fn from_bytes<T: Copy>(b: &[u8]) -> &[T] {
    assert_eq!(b.as_ptr() as usize % std::mem::align_of::<T>(), 0);
    unsafe { std::slice::from_raw_parts(b.as_ptr() as *const T, b.len() / std::mem::size_of::<T>()) }
}
fn rd64(b: &[u8], o: usize) -> u64 {
    u64::from_le_bytes(b[o..o + 8].try_into().unwrap())
}
fn rd32(b: &[u8], o: usize) -> u32 {
    u32::from_le_bytes(b[o..o + 4].try_into().unwrap())
}
fn cell_xy(c: u32) -> (i32, i32) {
    (c as i32 % GRID + ORIGIN, c as i32 / GRID + ORIGIN)
}
/// prep's sweep order: x-major over the SOURCE's cell, then y, then source.
fn order(cell: u32, si: u32) -> u64 {
    let (x, y) = cell_xy(cell);
    (((x + 64) as u64) << 48) | (((y + 64) as u64) << 32) | si as u64
}
fn map(p: &str, populate: bool) -> memmap2::Mmap {
    let f = std::fs::File::open(p).unwrap_or_else(|e| panic!("{p}: {e}"));
    let mut o = memmap2::MmapOptions::new();
    if populate {
        o.populate();
    }
    unsafe { o.map(&f).unwrap() }
}

extern "C" {
    fn prctl(option: i32, ...) -> i32;
    fn sched_setaffinity(pid: i32, size: usize, mask: *const u64) -> i32;
}
/// perf counters over the timed part only: PR_TASK_PERF_EVENTS_ENABLE/DISABLE,
/// and every fifo in `PERF_CTL` (comma-separated; `perf stat -D -1 --control
/// fifo:F`, which also gates a system-wide `-a` run).
fn perf_on(on: bool) {
    unsafe {
        prctl(if on { 32 } else { 31 }, 0, 0, 0, 0);
    }
    if let Ok(list) = std::env::var("PERF_CTL") {
        use std::io::Write;
        for p in list.split(',').filter(|p| !p.is_empty()) {
            let mut f = std::fs::OpenOptions::new().write(true).open(p).unwrap();
            f.write_all(if on { b"enable\n" } else { b"disable\n" }).unwrap();
            f.flush().unwrap();
        }
    }
}
fn pin(cpu: usize) {
    let mut m = [0u64; 16];
    m[cpu / 64] |= 1 << (cpu % 64);
    let r = unsafe { sched_setaffinity(0, std::mem::size_of_val(&m), m.as_ptr()) };
    assert_eq!(r, 0, "sched_setaffinity cpu {cpu}");
}
/// The 7950X3D: CCD0 (96 MB L3) = cpus 0-7 + SMT 16-23, CCD1 (32 MB) = 8-15 + 24-31.
fn ccd_of(cpu: usize) -> usize {
    (cpu % 16) / 8
}
#[inline(always)]
fn rdtsc() -> u64 {
    unsafe { core::arch::x86_64::_rdtsc() }
}
fn tsc_hz() -> f64 {
    let (t, c) = (Instant::now(), rdtsc());
    std::thread::sleep(std::time::Duration::from_millis(50));
    (rdtsc() - c) as f64 / t.elapsed().as_secs_f64()
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let dir = args[1].clone();
    match args.get(2).map(|s| s.as_str()) {
        Some("prep") => prep(&dir),
        Some("run") => {
            let kv: FxHashMap<String, String> = args[3..].iter().map(|a| { let (k, v) = a.split_once('=').expect("key=value"); (k.to_string(), v.to_string()) }).collect();
            let o = Opts::from(&kv);
            match o.probe.as_str() {
                "v4" => run(&dir, &o, |cnt| V4::new(cnt, o.load.unwrap_or(0.8))),
                "exact" => run(&dir, &o, |cnt| Exact::new(cnt, o.load.unwrap_or(0.5))),
                "pm8" => {
                    let n_door = std::fs::metadata(format!("{dir}/door.bin")).unwrap().len() as usize / 40;
                    run(&dir, &o, |_| Pm8::new(&format!("{dir}/sweep"), n_door, o.load.unwrap_or(0.5)))
                }
                p => panic!("probe {p}"),
            }
        }
        _ => panic!("usage: sweep DIR prep | sweep DIR run [key=value ...]"),
    }
}

// ------------------------------------------------------------------ prep
/// DIR/sweep/{d2.bin, dropfrom.bin, shardcell.bin}: the drops WITH their
/// target shard (prep's d.bin has the source only), in d.bin's order; the
/// level -1 table as a dense per-shard array; each shard's cell. Shard ids
/// continue prep's numbering (door, then the lookups' first sight); the
/// drop-only shards are appended. Checked against q.bin and d.bin.
fn prep(dir: &str) {
    let t = Instant::now();
    let mut files: Vec<_> = std::fs::read_dir(dir).unwrap().flatten().map(|e| e.path()).filter(|p| p.file_name().unwrap().to_str().unwrap().starts_with('w')).collect();
    files.sort();
    let maps: Vec<memmap2::Mmap> = files.iter().map(|p| map(p.to_str().unwrap(), false)).collect();
    let door = map(&format!("{dir}/door.bin"), false);
    let srcf = map(&format!("{dir}/src.bin"), false);
    let n_src = srcf.len() / 20;
    let mut src_of: FxHashMap<u64, (u32, u32)> = FxHashMap::default();
    for i in 0..n_src {
        src_of.insert(rd64(&srcf, i * 20), (i as u32, rd32(&srcf, i * 20 + 8)));
    }
    let mut dir_: FxHashMap<(u64, u32), u32> = FxHashMap::default();
    let mut cells: Vec<u32> = Vec::new();
    let mut shard = |k: (u64, u32), cells: &mut Vec<u32>| -> u32 {
        *dir_.entry(k).or_insert_with(|| {
            cells.push(k.1);
            (cells.len() - 1) as u32
        })
    };
    let n_door = door.len() / 40;
    for i in 0..n_door {
        let b = &door[i * 40..];
        shard((rd64(b, 0), rd32(b, 8)), &mut cells);
    }
    let mut key_shard: FxHashMap<u128, u32> = FxHashMap::default();
    for m in &maps {
        for c in m.chunks_exact(REC) {
            if rd32(c, 40) != 0 {
                continue;
            }
            let s = shard((rd64(c, 24), rd32(c, 32)), &mut cells);
            let k = rd64(c, 8) as u128 | (rd64(c, 16) as u128) << 64;
            assert_eq!(*key_shard.entry(k).or_insert(s), s, "a key in two shards");
        }
    }
    let pd = format!("{dir}/prep");
    let n_qshards = std::fs::metadata(format!("{pd}/cnt.bin")).unwrap().len() as usize / 8;
    assert_eq!(cells.len(), n_qshards, "shard numbering differs from prep's");
    let mut d: Vec<(u64, D2)> = Vec::with_capacity(255_000_000);
    for m in &maps {
        for c in m.chunks_exact(REC) {
            if rd32(c, 40) != 1 {
                continue;
            }
            let (si, scell) = src_of[&rd64(c, 0)];
            let s = shard((rd64(c, 24), rd32(c, 32)), &mut cells);
            d.push((order(scell, si), D2 { src: si, shard: s }));
        }
    }
    d.sort_unstable_by_key(|a| a.0);
    let d2: Vec<D2> = d.into_iter().map(|a| a.1).collect();
    let dm = map(&format!("{pd}/d.bin"), false);
    let d_ref: &[u32] = from_bytes(&dm);
    assert_eq!(d_ref.len(), d2.len());
    assert!(d_ref.iter().zip(&d2).all(|(&a, b)| a == b.src), "drops not in d.bin's order");
    let mut dropfrom = vec![0u32; cells.len()];
    for x in &d2 {
        dropfrom[x.shard as usize] = DROPPED_FROM;
    }
    let qm = map(&format!("{pd}/q.bin"), false);
    let qs: &[Q] = from_bytes(&qm);
    for q in qs {
        assert_eq!(key_shard[&q.key], q.shard, "q.bin's shard is not the key's");
        assert_eq!(dropfrom[q.shard as usize], 0, "a lookup into a dropped shard");
    }
    let od = format!("{dir}/sweep");
    std::fs::create_dir_all(&od).unwrap();
    std::fs::write(format!("{od}/d2.bin"), as_bytes(&d2)).unwrap();
    std::fs::write(format!("{od}/dropfrom.bin"), as_bytes(&dropfrom)).unwrap();
    std::fs::write(format!("{od}/shardcell.bin"), as_bytes(&cells)).unwrap();
    eprintln!("[prep] {} drops, {} shards ({} with lookups, {} dropped); {:.1} s", d2.len(), cells.len(), n_qshards, dropfrom.iter().filter(|&&f| f != 0).count(), t.elapsed().as_secs_f64());
}

// ------------------------------------------------------------------ run
struct Opts {
    threads: usize,
    front: String,
    probe: String,
    chunk: usize,
    cache_bits: u32,
    split: f64,
    verify: bool,
    prefault: bool,
    read: bool,
    cpus: Vec<usize>,
    load: Option<f64>,
}

impl Opts {
    fn from(kv: &FxHashMap<String, String>) -> Opts {
        let g = |k: &str, d: &str| kv.get(k).cloned().unwrap_or(d.to_string());
        let threads: usize = g("threads", "16").parse().unwrap();
        let cpus: Vec<usize> = match kv.get("cpus") {
            Some(l) => l.split(',').map(|c| c.parse().unwrap()).collect(),
            // One thread a core over both CCDs (16), then the SMT siblings.
            None => (0..threads).collect(),
        };
        assert!(cpus.len() >= threads);
        Opts {
            threads,
            front: g("front", "shared"),
            probe: g("probe", "pm8"),
            chunk: g("chunk", "1024").parse().unwrap(),
            cache_bits: g("cache", "12").parse().unwrap(),
            split: g("split", "0.5").parse().unwrap(),
            verify: g("verify", "0") == "1",
            prefault: g("prefault", "0") == "1",
            read: g("read", "0") == "1",
            cpus,
            load: kv.get("load").map(|l| l.parse().unwrap()),
        }
    }
}

#[derive(Clone, Copy)]
struct Chunk {
    qa: u32,
    qb: u32,
    da: u32,
    db: u32,
}

#[repr(C, align(64))]
struct Counter(AtomicUsize, usize);

/// A raw shared output arena (disjoint segments per thread).
struct Out<T> {
    a: probe::Arena<T>,
    next_seg: AtomicUsize,
    seg: usize,
}
impl<T> Out<T> {
    fn new(items: usize, seg: usize, threads: usize, prefault: bool) -> Self {
        let segs = items.div_ceil(seg) + threads + 1;
        let a = probe::Arena::<T>::zeroed(segs * seg);
        if prefault {
            let p = a.ptr() as *mut u8;
            for o in (0..a.bytes()).step_by(4096) {
                unsafe { p.add(o).write_volatile(0) };
            }
        }
        Out { a, next_seg: AtomicUsize::new(0), seg }
    }
    fn take(&self) -> usize {
        let s = self.next_seg.fetch_add(1, Ordering::Relaxed);
        assert!((s + 1) * self.seg <= self.a.len(), "output arena exhausted");
        s
    }
    fn at(&self, seg: usize, i: usize) -> *mut T {
        unsafe { self.a.ptr().add(seg * self.seg + i) }
    }
}

const PH: [&str; 6] = ["claim", "drops", "scan", "probe", "emit", "finish"];

#[derive(Clone, Copy, Default)]
#[repr(C)]
struct CacheEnt {
    lo: u64,
    hi: u64,
    val: u32,
    gen: u32,
}

/// One thread's results.
#[derive(Default)]
struct Done {
    edge_segs: Vec<(usize, usize)>,
    row_segs: Vec<usize>,
    rows: u32,
    edges: u64,
    pos: FxHashSet<(u32, u32)>,
    ph: [u64; 6],
    busy: f64,
    pst: ProbeStats,
    cache_hits: u64,
    chunk_dups: u64,
    mismatches: u64,
    chunks: u64,
    steals: u64,
    drops: u64,
    read_sum: u64,
}

/// Non-temporal copy of `n16` 16-B units.
#[inline(always)]
unsafe fn nt_copy(dst: *mut u8, src: *const u8, n16: usize) {
    use core::arch::x86_64::{__m128i, _mm_loadu_si128, _mm_stream_si128};
    for i in 0..n16 {
        _mm_stream_si128((dst as *mut __m128i).add(i), _mm_loadu_si128((src as *const __m128i).add(i)));
    }
}

struct Shared<'a, P: Probe> {
    o: &'a Opts,
    qs: &'a [Q],
    ds: &'a [D2],
    chunks: &'a [Chunk],
    bands: &'a [Counter],
    dropfrom: &'a [u32],
    shard_cell: &'a [u32],
    src_cell: &'a [u32],
    notes: &'a [AtomicU32],
    probe: &'a P,
    edges: &'a Out<Edge>,
    rows: &'a Out<[u8; 64]>,
    n_door: u32,
    go: &'a std::sync::Barrier,
}

fn worker<P: Probe>(sh: &Shared<P>, t: usize, home: usize) -> Done {
    pin(sh.o.cpus[t]);
    let mut d = Done::default();
    let base_id = sh.n_door + t as u32 * CAP;
    let (mut local, mut nrows) = (0u32, 0u32);
    let cache_on = sh.o.cache_bits > 0;
    let mut cache: Vec<CacheEnt> = vec![CacheEnt::default(); if cache_on { 1 << sh.o.cache_bits } else { 0 }];
    let cmask = cache.len().wrapping_sub(1);
    let mut gen = 0u32;
    let mut stage: Vec<Edge> = Vec::with_capacity(STAGE);
    let (mut eseg, mut elen) = (usize::MAX, 0usize);
    let mut ids: Vec<u32> = Vec::with_capacity(4 * sh.o.chunk + 64);
    let mut pend: Vec<Pending> = Vec::with_capacity(4 * sh.o.chunk + 64);
    let mut last_pos = (u32::MAX, u32::MAX);
    sh.go.wait();
    let t0 = Instant::now();
    let mut c0 = rdtsc();
    macro_rules! tick {
        ($p:expr) => {{
            let c = rdtsc();
            d.ph[$p] += c - c0;
            c0 = c;
        }};
    }
    macro_rules! pos {
        ($a:expr, $b:expr) => {{
            let e = ($a, $b);
            if e != last_pos {
                last_pos = e;
                d.pos.insert(e);
            }
        }};
    }
    let flush_stage = |stage: &mut Vec<Edge>, eseg: &mut usize, elen: &mut usize, d: &mut Done, nt: bool| {
        if stage.is_empty() {
            return;
        }
        if *eseg == usize::MAX || *elen + stage.len() > SEG_E {
            if *eseg != usize::MAX {
                d.edge_segs.push((*eseg, *elen));
            }
            *eseg = sh.edges.take();
            *elen = 0;
        }
        let dst = sh.edges.at(*eseg, *elen) as *mut u8;
        unsafe {
            if nt {
                nt_copy(dst, stage.as_ptr() as *const u8, stage.len() * 12 / 16);
            } else {
                std::ptr::copy_nonoverlapping(stage.as_ptr(), dst as *mut Edge, stage.len());
            }
        }
        *elen += stage.len();
        stage.clear();
    };
    loop {
        // Claim the next chunk at the front: the home band, then steal.
        let mut got = None;
        for k in 0..sh.bands.len() {
            let b = &sh.bands[(home + k) % sh.bands.len()];
            let i = b.0.fetch_add(1, Ordering::Relaxed);
            if i < b.1 {
                got = Some(i);
                d.steals += (k > 0) as u64;
                break;
            }
        }
        let Some(ci) = got else { break };
        d.chunks += 1;
        let ch = sh.chunks[ci];
        tick!(0);
        if sh.o.read {
            // The harness's floor: read the chunk's stream, nothing else.
            let mut acc = 0u64;
            for x in &sh.ds[ch.da as usize..ch.db as usize] {
                acc = acc.wrapping_add((x.src ^ x.shard) as u64);
            }
            for q in &sh.qs[ch.qa as usize..ch.qb as usize] {
                acc = acc.wrapping_add(q.key as u64 ^ (q.src ^ q.xfer ^ q.shard) as u64);
            }
            d.read_sum = d.read_sum.wrapping_add(acc);
            tick!(2);
            continue;
        }
        // Drops: the level -1 table, the source's note (this chunk owns its
        // sources: chunks are cut at source boundaries).
        for x in &sh.ds[ch.da as usize..ch.db as usize] {
            let f = sh.dropfrom[x.shard as usize];
            if f == 0 {
                d.mismatches += 1;
                continue;
            }
            d.drops += 1;
            let n = &sh.notes[x.src as usize];
            if f < n.load(Ordering::Relaxed) {
                n.store(f, Ordering::Relaxed);
            }
            pos!(sh.src_cell[x.src as usize], sh.shard_cell[x.shard as usize]);
        }
        tick!(1);
        // Scan: the table check, the pos-graph edge, the cache.
        let qs = &sh.qs[ch.qa as usize..ch.qb as usize];
        ids.clear();
        pend.clear();
        gen = gen.wrapping_add(1);
        for (k, q) in qs.iter().enumerate() {
            if sh.dropfrom[q.shard as usize] != 0 {
                d.mismatches += 1;
                ids.push(SKIP);
                continue;
            }
            pos!(sh.src_cell[q.src as usize], sh.shard_cell[q.shard as usize]);
            let pj = PEND | pend.len() as u32;
            if cache_on {
                let (lo, hi) = (q.key as u64, (q.key >> 64) as u64);
                let e = &mut cache[hi as usize & cmask];
                if e.lo == lo && e.hi == hi {
                    if e.val & PEND == 0 {
                        d.cache_hits += 1;
                        ids.push(e.val);
                        continue;
                    }
                    if e.gen == gen {
                        d.chunk_dups += 1;
                        ids.push(e.val);
                        continue;
                    }
                }
                *e = CacheEnt { lo, hi, val: pj, gen };
            }
            ids.push(pj);
            let (ps, pk) = sh.probe.query(ch.qa as usize + k, q.shard, q.key);
            pend.push(Pending { key: pk, shard: ps, at: k as u32, id: 0, inserted: false });
        }
        tick!(2);
        {
            let mut fresh = || {
                let id = base_id + local;
                local += 1;
                assert!(local < CAP, "thread {t}: past {CAP} new states");
                id
            };
            sh.probe.resolve(&mut pend, &mut fresh, &mut d.pst);
        }
        tick!(3);
        // Emit: the new rows (64 B, one non-temporal line each), the cache, the edges.
        for (j, p) in pend.iter().enumerate() {
            if p.inserted {
                let l = nrows as usize;
                nrows += 1;
                while d.row_segs.len() <= l / SEG_R {
                    d.row_segs.push(sh.rows.take());
                }
                let q = &qs[p.at as usize];
                let mut row = [0u32; 16];
                unsafe { std::ptr::copy_nonoverlapping(q as *const Q as *const u32, row.as_mut_ptr(), 8) };
                row[8] = sh.src_cell[q.src as usize];
                row[9] = sh.shard_cell[q.shard as usize];
                row[10] = p.id;
                row[11] = ch.qa + p.at;
                row[12..16].copy_from_slice(&[q.key as u32, (q.key >> 32) as u32, (q.key >> 64) as u32, (q.key >> 96) as u32]);
                unsafe { nt_copy(sh.rows.at(d.row_segs[l / SEG_R], l % SEG_R) as *mut u8, row.as_ptr() as *const u8, 4) };
            }
            if cache_on {
                let k = qs[p.at as usize].key;
                let e = &mut cache[(k >> 64) as usize & cmask];
                if e.val == PEND | j as u32 && e.gen == gen && e.lo == k as u64 {
                    e.val = p.id;
                }
            }
        }
        for (k, q) in qs.iter().enumerate() {
            let v = ids[k];
            if v == SKIP {
                continue;
            }
            let id = if v & PEND != 0 { pend[(v & !PEND) as usize].id } else { v };
            stage.push(Edge { src: q.src, tgt: id, xfer: q.xfer });
            if stage.len() == STAGE {
                flush_stage(&mut stage, &mut eseg, &mut elen, &mut d, true);
            }
        }
        d.edges += qs.len() as u64;
        tick!(4);
    }
    flush_stage(&mut stage, &mut eseg, &mut elen, &mut d, false);
    if eseg != usize::MAX {
        d.edge_segs.push((eseg, elen));
    }
    unsafe { core::arch::x86_64::_mm_sfence() };
    tick!(5);
    std::hint::black_box(c0);
    std::hint::black_box(local);
    d.rows = nrows;
    d.busy = t0.elapsed().as_secs_f64();
    d
}

fn run<P: Probe, F: Fn(&[u64]) -> P>(dir: &str, o: &Opts, make: F) {
    let t_setup = Instant::now();
    let pd = format!("{dir}/prep");
    let sd = format!("{dir}/sweep");
    // The streams, page tables populated (untimed).
    let qm = map(&format!("{pd}/q.bin"), true);
    let dm = map(&format!("{sd}/d2.bin"), true);
    let qs: &[Q] = from_bytes(&qm);
    let ds: &[D2] = from_bytes(&dm);
    let dropfrom: Vec<u32> = from_bytes::<u32>(&std::fs::read(format!("{sd}/dropfrom.bin")).unwrap()).to_vec();
    let shard_cell: Vec<u32> = from_bytes::<u32>(&std::fs::read(format!("{sd}/shardcell.bin")).unwrap()).to_vec();
    let cnt: Vec<u64> = from_bytes::<u64>(&std::fs::read(format!("{pd}/cnt.bin")).unwrap()).to_vec();
    let door = map(&format!("{dir}/door.bin"), false);
    let srcf = map(&format!("{dir}/src.bin"), false);
    let n_door = door.len() / 40;
    let n_src = srcf.len() / 20;
    let src_cell: Vec<u32> = (0..n_src).map(|i| rd32(&srcf, i * 20 + 8)).collect();
    let n_new_expected = cnt.iter().sum::<u64>() as usize - n_door;
    assert!(n_door as u64 + o.threads as u64 * CAP as u64 <= PEND as u64, "provisional ids must fit 31 bits");
    // Chunks: consecutive sources in sweep order, weight 4 a lookup + 1 a drop.
    let ord = |si: u32| order(src_cell[si as usize], si);
    let mut chunks: Vec<Chunk> = Vec::with_capacity(qs.len() / o.chunk + 16);
    let mut weights: Vec<u64> = Vec::with_capacity(chunks.capacity());
    let (mut i, mut j) = (0usize, 0usize);
    let (nq, nd) = (qs.len(), ds.len());
    let mut last_ord = 0u64;
    while i < nq || j < nd {
        let (qa, da) = (i, j);
        let mut w = 0usize;
        while (i < nq || j < nd) && w < 4 * o.chunk {
            let oq = if i < nq { ord(qs[i].src) } else { u64::MAX };
            let od = if j < nd { ord(ds[j].src) } else { u64::MAX };
            let s = oq.min(od);
            assert!(s >= last_ord, "streams not in sweep order");
            last_ord = s;
            while i < nq && ord(qs[i].src) == s {
                i += 1;
                w += 4;
            }
            while j < nd && ord(ds[j].src) == s {
                j += 1;
                w += 1;
            }
        }
        chunks.push(Chunk { qa: qa as u32, qb: i as u32, da: da as u32, db: j as u32 });
        weights.push(w as u64);
    }
    let bands: Vec<Counter> = if o.front == "ccd" {
        let total: u64 = weights.iter().sum();
        let mut acc = 0u64;
        let cut = weights.iter().position(|&w| { acc += w; acc as f64 >= o.split * total as f64 }).unwrap_or(chunks.len());
        vec![Counter(AtomicUsize::new(0), cut), Counter(AtomicUsize::new(cut), chunks.len())]
    } else {
        assert_eq!(o.front, "shared", "front=shared|ccd");
        vec![Counter(AtomicUsize::new(0), chunks.len())]
    };
    drop(weights);
    // The table: the door's states, id = door index.
    let probe = make(&cnt);
    {
        let dsm = map(&format!("{pd}/dshard.bin"), false);
        let dsh: &[u32] = from_bytes(&dsm);
        let per = n_door.div_ceil(32);
        let bad = AtomicU64::new(0);
        // (pm8 is filled from its own key dump when it is built.)
        let prefill = probe.positional_ids().is_none();
        std::thread::scope(|s| {
            for t in (0..32).filter(|_| prefill) {
                let (probe, door, dsh, bad) = (&probe, &door, dsh, &bad);
                s.spawn(move || {
                    let mut st = ProbeStats::default();
                    for i in t * per..((t + 1) * per).min(n_door) {
                        let k = rd64(door, i * 40 + 16) as u128 | (rd64(door, i * 40 + 24) as u128) << 64;
                        if k as u64 == 0 {
                            bad.fetch_add(1, Ordering::Relaxed);
                        }
                        let (_, ins) = probe.insert(dsh[i], k, &mut || i as u32, &mut st);
                        assert!(ins, "a door key twice");
                    }
                });
            }
            // Exact's publish word: no key may have a zero low half.
            let per_q = qs.len().div_ceil(32);
            for t in 0..32 {
                let (qs, bad) = (qs, &bad);
                s.spawn(move || {
                    let n = qs[t * per_q..((t + 1) * per_q).min(qs.len())].iter().filter(|q| q.key as u64 == 0).count();
                    bad.fetch_add(n as u64, Ordering::Relaxed);
                });
            }
        });
        assert_eq!(bad.load(Ordering::Relaxed), 0, "a key with a zero low half");
    }
    let notes: Vec<AtomicU32> = (0..n_src).map(|_| AtomicU32::new(u32::MAX)).collect();
    let edges_out: Out<Edge> = Out::new(nq, SEG_E, o.threads, o.prefault);
    let rows_out: Out<[u8; 64]> = Out::new(n_new_expected, SEG_R, o.threads, o.prefault);
    eprintln!(
        "[setup] {:.1} s (untimed): {nq} lookups, {nd} drops, {} chunks ({} bands), door {n_door}, table {} {:.2} GB, edge arena {:.2} GB, row arena {:.2} GB; uptime {}",
        t_setup.elapsed().as_secs_f64(),
        chunks.len(),
        bands.len(),
        probe.name(),
        probe.bytes() as f64 / 1e9,
        edges_out.a.bytes() as f64 / 1e9,
        rows_out.a.bytes() as f64 / 1e9,
        std::fs::read_to_string("/proc/loadavg").unwrap().trim()
    );

    // ---- the wave (timed)
    let go = std::sync::Barrier::new(o.threads + 1);
    let sh = Shared { o, qs, ds, chunks: &chunks, bands: &bands, dropfrom: &dropfrom, shard_cell: &shard_cell, src_cell: &src_cell, notes: &notes, probe: &probe, edges: &edges_out, rows: &rows_out, n_door: n_door as u32, go: &go };
    let (done, t_wave) = std::thread::scope(|s| {
        let hs: Vec<_> = (0..o.threads)
            .map(|t| {
                let sh = &sh;
                let home = if bands.len() > 1 { ccd_of(o.cpus[t]) } else { 0 };
                s.spawn(move || worker(sh, t, home))
            })
            .collect();
        perf_on(true);
        let t = Instant::now();
        go.wait();
        let done: Vec<Done> = hs.into_iter().map(|h| h.join().unwrap()).collect();
        let t_wave = t.elapsed().as_secs_f64();
        (done, t_wave)
    });
    let hz = tsc_hz();
    let tag = format!("front={} probe={} threads={} chunk={} cache={}{}", o.front, probe.name(), o.threads, o.chunk, o.cache_bits, if o.read { " READ-ONLY" } else { "" });
    if o.read {
        perf_on(false);
        eprintln!("[sweep] {tag}: read floor {t_wave:.3} s (sum {:x})", done.iter().fold(0u64, |a, d| a.wrapping_add(d.read_sum)));
        return;
    }

    // ---- the frame's end (timed): canonical ids by (shard, key).
    let t_end = Instant::now();
    let n_shards = shard_cell.len();
    let row_at = |t: usize, l: usize| -> &[u8; 64] { unsafe { &*rows_out.at(done[t].row_segs[l / SEG_R], l % SEG_R) } };
    let row_shard = |r: &[u8; 64]| u32::from_le_bytes(r[0..4].try_into().unwrap());
    let row_key = |r: &[u8; 64]| u128::from_le_bytes(r[16..32].try_into().unwrap());
    let mut ph_end = [0f64; 4];
    let c = Instant::now();
    // Per thread, its rows per shard.
    let counts: Vec<Vec<u32>> = std::thread::scope(|s| {
        let hs: Vec<_> = (0..o.threads)
            .map(|t| {
                let (done, row_at) = (&done, &row_at);
                s.spawn(move || {
                    pin(o.cpus[t]);
                    let mut c = vec![0u32; n_shards];
                    for l in 0..done[t].rows as usize {
                        c[row_shard(row_at(t, l)) as usize] += 1;
                    }
                    c
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().unwrap()).collect()
    });
    // Starts per (shard, thread), shard-major.
    let mut starts: Vec<Vec<u32>> = vec![vec![0u32; n_shards]; o.threads];
    let mut shard_start = vec![0u32; n_shards + 1];
    let mut acc = 0u32;
    for sh_ in 0..n_shards {
        shard_start[sh_] = acc;
        for t in 0..o.threads {
            starts[t][sh_] = acc;
            acc += counts[t][sh_];
        }
    }
    shard_start[n_shards] = acc;
    let n_new = acc as usize;
    drop(counts);
    ph_end[0] = c.elapsed().as_secs_f64();
    let c = Instant::now();
    type It = NewState;
    let mut items: Vec<It> = Vec::with_capacity(n_new);
    let ip = items.as_mut_ptr() as usize;
    std::thread::scope(|s| {
        for (t, mut st) in starts.into_iter().enumerate() {
            let (done, row_at) = (&done, &row_at);
            s.spawn(move || {
                pin(o.cpus[t]);
                for l in 0..done[t].rows as usize {
                    let r = row_at(t, l);
                    let sh_ = row_shard(r) as usize;
                    unsafe { (ip as *mut It).add(st[sh_] as usize).write(It { key: row_key(r), shard: sh_ as u32, prov: u32::from_le_bytes(r[40..44].try_into().unwrap()), qi: u32::from_le_bytes(r[44..48].try_into().unwrap()) }) };
                    st[sh_] += 1;
                }
            });
        }
    });
    unsafe { items.set_len(n_new) };
    ph_end[1] = c.elapsed().as_secs_f64();
    let c = Instant::now();
    // Per shard: sort by key; canonical id = n_door + position; the map
    // provisional -> canonical (indexed by provisional - n_door: sparse,
    // zero-page backed); then the table's own end of frame.
    let renum: probe::Arena<AtomicU32> = probe::Arena::zeroed(probe.renum_prepare(o.threads).unwrap_or(o.threads * CAP as usize));
    {
        let next = AtomicUsize::new(0);
        let ip = items.as_mut_ptr() as usize;
        std::thread::scope(|s| {
            for t in 0..o.threads {
                let (next, shard_start, renum, probe) = (&next, &shard_start, &renum, &probe);
                s.spawn(move || {
                    pin(o.cpus[t]);
                    loop {
                        let a = next.fetch_add(64, Ordering::Relaxed);
                        if a >= n_shards {
                            break;
                        }
                        for sh_ in a..(a + 64).min(n_shards) {
                            let (lo, hi) = (shard_start[sh_] as usize, shard_start[sh_ + 1] as usize);
                            let v = unsafe { std::slice::from_raw_parts_mut((ip as *mut It).add(lo), hi - lo) };
                            v.sort_unstable_by_key(|x| x.key);
                            for (k, x) in v.iter().enumerate() {
                                // (+1: zero is "none" in the zeroed map)
                                renum.at(probe.renum_index(x.prov, n_door as u32)).store((n_door + lo + k) as u32 + 1, Ordering::Relaxed);
                            }
                        }
                    }
                });
            }
        });
    }
    ph_end[2] = c.elapsed().as_secs_f64();
    let c = Instant::now();
    let renum_of = |prov: u32| -> u32 {
        let c = renum.at(probe.renum_index(prov, n_door as u32)).load(Ordering::Relaxed);
        assert_ne!(c, 0, "a new state without a canonical id");
        c - 1
    };
    probe.end_frame(&items, &renum_of, n_door as u32, o.threads);
    ph_end[3] = c.elapsed().as_secs_f64();
    let t_end = t_end.elapsed().as_secs_f64();
    perf_on(false);

    // ---- report
    let sum = |f: &dyn Fn(&Done) -> u64| done.iter().map(f).sum::<u64>();
    let (edges, drops, mism) = (sum(&|d| d.edges), sum(&|d| d.drops), sum(&|d| d.mismatches));
    let busy: f64 = done.iter().map(|d| d.busy).sum();
    let pos_sum = sum(&|d| d.pos.len() as u64);
    let mut pos_all: FxHashSet<(u32, u32)> = FxHashSet::default();
    for d in &done {
        pos_all.extend(d.pos.iter().copied());
    }
    let noted = notes.iter().filter(|n| n.load(Ordering::Relaxed) != u32::MAX).count();
    eprintln!(
        "[sweep] {tag}: wave {t_wave:.3} s (idle {:.0}%), end {t_end:.3} s, total {:.3} s; new {n_new} ({}), edges {edges} ({}), drops {drops}, notes {noted}, level -1 mismatches {mism}, pos edges {} (summed per thread {pos_sum})",
        100.0 * (1.0 - busy / (o.threads as f64 * t_wave)),
        t_wave + t_end,
        if n_new == 6_735_699 { "OK" } else { "MISMATCH" },
        if edges == 257_724_013 { "OK" } else { "MISMATCH" },
        pos_all.len()
    );
    let budget = t_wave * o.threads as f64;
    let mut ph = [0u64; 6];
    for d in &done {
        for k in 0..6 {
            ph[k] += d.ph[k];
        }
    }
    let line: Vec<String> = PH.iter().zip(ph).map(|(n, t)| format!("{n} {:.2}s ({:.0}%)", t as f64 / hz, 100.0 * t as f64 / hz / budget)).collect();
    eprintln!("[phases] worker-seconds of {budget:.2} ({} x {t_wave:.3} s): {}; end: count {:.3} s, scatter {:.3} s, sort+renumber {:.3} s, table {:.3} s", o.threads, line.join(", "), ph_end[0], ph_end[1], ph_end[2], ph_end[3]);
    let pst = done.iter().fold(ProbeStats::default(), |a, d| ProbeStats { lookups: a.lookups + d.pst.lookups, found: a.found + d.pst.found, inserts: a.inserts + d.pst.inserts, raced: a.raced + d.pst.raced, lock_spins: a.lock_spins + d.pst.lock_spins });
    eprintln!(
        "[stats] cache hits {} ({:.1}%), in-chunk dups {}, table lookups {} (found {}, inserts {}, raced {}, lock spins {}), chunks {} (stolen {}), bytes: edges {:.2} GB, rows {:.2} GB",
        sum(&|d| d.cache_hits),
        100.0 * sum(&|d| d.cache_hits) as f64 / edges.max(1) as f64,
        sum(&|d| d.chunk_dups),
        pst.lookups,
        pst.found,
        pst.inserts,
        pst.raced,
        pst.lock_spins,
        sum(&|d| d.chunks),
        sum(&|d| d.steals),
        edges as f64 * 12.0 / 1e9,
        n_new as f64 * 64.0 / 1e9
    );
    eprintln!("[mem] AnonHugePages {} MB", canon::anon_huge_kb() / 1024);
    if !o.verify {
        return;
    }

    // ---- verification (untimed): canonical fingerprints (canon.rs), as vprod's.
    let t = Instant::now();
    let src_id: Vec<u64> = (0..n_src).map(|i| rd64(&srcf, i * 20)).collect();
    let door_kh = |i: usize| canon::key_hash(rd64(&door, i * 40 + 16), rd64(&door, i * 40 + 24));
    // (The table's end of frame consumed its provisional ids: a map from the rows.)
    let canon_of: FxHashMap<u32, u32> = items.iter().enumerate().map(|(k, it)| (it.prov, (n_door + k) as u32)).collect();
    let renum_of = |prov: u32| canon_of[&prov];
    let segs: Vec<(usize, usize)> = done.iter().flat_map(|d| d.edge_segs.iter().copied()).collect();
    let next = AtomicUsize::new(0);
    let (hs, idfp): (Vec<u64>, u64) = std::thread::scope(|s| {
        let hs: Vec<_> = (0..32)
            .map(|_| {
                let (segs, next, src_id, renum_of, items, edges_out, door_kh) = (&segs, &next, &src_id, &renum_of, &items, &edges_out, &door_kh);
                s.spawn(move || {
                    let (mut h, mut idfp) = (Vec::new(), 0u64);
                    loop {
                        let k = next.fetch_add(1, Ordering::Relaxed);
                        let Some(&(seg, len)) = segs.get(k) else { break };
                        for i in 0..len {
                            let e = unsafe { *edges_out.at(seg, i) };
                            let (cid, kh) = if (e.tgt as usize) < n_door {
                                (e.tgt, door_kh(e.tgt as usize))
                            } else {
                                let c = renum_of(e.tgt);
                                let it = &items[c as usize - n_door];
                                (c, canon::key_hash(it.key as u64, (it.key >> 64) as u64))
                            };
                            h.push(canon::edge_hash(canon::sx_hash(src_id[e.src as usize], e.xfer), kh));
                            idfp = idfp.wrapping_add(canon::mix64((e.src as u64) << 32 ^ cid as u64 ^ canon::mix64(e.xfer as u64)));
                        }
                    }
                    (h, idfp)
                })
            })
            .collect();
        let mut all = Vec::with_capacity(edges as usize);
        let mut idfp = 0u64;
        for h in hs {
            let (v, f) = h.join().unwrap();
            all.extend_from_slice(&v);
            idfp = idfp.wrapping_add(f);
        }
        (all, idfp)
    });
    let (n, distinct, fp) = canon::summarize(hs, 32);
    let (mut nn, mut nsum) = (0u64, 0u64);
    for (i, m) in notes.iter().enumerate() {
        let m = m.load(Ordering::Relaxed);
        if m != u32::MAX {
            nn += 1;
            nsum = nsum.wrapping_add(canon::note_hash(src_id[i], m));
        }
    }
    // The table after the frame's end: every new key at its canonical id.
    let bad = items.iter().enumerate().filter(|(k, it)| {
        let (ps, pk) = probe.query(it.qi as usize, it.shard, it.key);
        assert_eq!(qs[it.qi as usize].key, it.key, "a row's lookup is not its key's");
        probe.lookup(ps, pk) != Some((n_door + k) as u32)
    }).count();
    eprintln!("[verify] canon edges {n} (want 257724013) distinct {distinct} fp {fp:016x}; notes {nn} fp {nsum:016x}; canonical-id fp {idfp:016x}; table ids wrong {bad}; {:.1} s", t.elapsed().as_secs_f64());
}
