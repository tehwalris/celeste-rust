//! Standalone microbenchmark of one forward frame's dedup / frontier / edge
//! pipeline, replayed from a capture (`CELESTE_EMIT_CAPTURE`, see
//! src/capture.rs in the main crate):
//!   for each emission (source, successor key): visited? -> nothing new;
//!   else insert, new id, append to the next frontier; record the edge.
//! Usage: dedup-bench CAPTURE_DIR [variant]

use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::sync::Mutex;
use std::time::Instant;

mod bits;
mod mlp;
mod structure;
mod posregion;

const REC: usize = 48;
const PAYLOAD: usize = 64;

#[derive(Clone, Copy)]
struct Rec { src: u64, key: u128, shape: u64, cell: u32, xfer: u32, flags: u32 }

#[inline]
fn rec(b: &[u8]) -> Rec {
    let u64_ = |o: usize| u64::from_le_bytes(b[o..o + 8].try_into().unwrap());
    let u32_ = |o: usize| u32::from_le_bytes(b[o..o + 4].try_into().unwrap());
    Rec { src: u64_(0), key: (u64_(8) as u128) | ((u64_(16) as u128) << 64), shape: u64_(24), cell: u32_(32), xfer: u32_(36), flags: u32_(40) }
}

extern "C" { fn prctl(option: i32, ...) -> i32; }
/// Counters of a `perf stat` on this process count only between `perf_on(true)`
/// and `perf_on(false)` (PR_TASK_PERF_EVENTS_ENABLE / _DISABLE): the timed loop.
/// With `PERF_CTL=FIFO` (perf stat -D -1 --control fifo:FIFO), the counters
/// are switched by perf itself.
pub fn perf_on(on: bool) {
    if on { if let Ok(r) = std::fs::read_to_string("/proc/self/smaps_rollup") { eprintln!("[mem] {}", r.lines().filter(|l| l.starts_with("Rss") || l.starts_with("AnonHugePages")).map(|l| l.split_whitespace().collect::<Vec<_>>().join(" ")).collect::<Vec<_>>().join(", ")); } }
    unsafe { prctl(if on { 32 } else { 31 }, 0, 0, 0, 0); }
    if let Some(p) = std::env::var_os("PERF_CTL") {
        use std::io::Write;
        let mut f = std::fs::OpenOptions::new().write(true).open(p).unwrap();
        f.write_all(if on { b"enable\n" } else { b"disable\n" }).unwrap();
        f.flush().unwrap();
    }
}

extern "C" { fn madvise(addr: *mut u8, len: usize, advice: i32) -> i32; }
/// madvise(MADV_HUGEPAGE) the 2 MB-aligned interior of a vector's capacity.
/// THP here is `enabled=always, defrag=madvise`: without the advice a fault
/// takes a huge page only if one is free, so under memory pressure the timed
/// tables came out mostly on 4 KB pages (bitcell 2.1 -> 2.8 s, dTLB misses
/// 14x). Call before the pages are first touched.
pub fn advise_huge<T>(v: &Vec<T>) {
    const H: usize = 2 << 20;
    let a = v.as_ptr() as usize;
    let (start, end) = ((a + H - 1) & !(H - 1), (a + v.capacity() * std::mem::size_of::<T>()) & !(H - 1));
    if end > start { unsafe { madvise(start as *mut u8, end - start, 14); } }
}
/// An empty vector of capacity `n`, advised onto huge pages.
pub fn huge_cap<T>(n: usize) -> Vec<T> { let v = Vec::with_capacity(n); advise_huge(&v); v }
/// `vec![x; n]`, advised onto huge pages before it is filled.
pub fn huge_vec<T: Clone>(n: usize, x: T) -> Vec<T> { let mut v = huge_cap(n); v.resize(n, x); v }
/// AnonHugePages / Anonymous of this process (/proc/self/smaps_rollup), GB.
pub fn hp() -> String {
    let r = std::fs::read_to_string("/proc/self/smaps_rollup").unwrap_or_default();
    let kb = |k: &str| r.lines().find_map(|l| l.strip_prefix(k)).and_then(|l| l.split_whitespace().next()).and_then(|v| v.parse::<f64>().ok()).unwrap_or(0.0);
    format!("AnonHugePages {:.2} of Anonymous {:.2} GB", kb("AnonHugePages:") / 1e6, kb("Anonymous:") / 1e6)
}

/// AnonHugePages / Rss of the mappings under `v` (/proc/self/smaps), GB: did
/// THIS table get huge pages.
pub fn hp_at<T>(v: &[T]) -> String {
    let (a, b) = (v.as_ptr() as usize, v.as_ptr() as usize + std::mem::size_of_val(v));
    let r = std::fs::read_to_string("/proc/self/smaps").unwrap_or_default();
    let (mut inside, mut rss, mut huge) = (false, 0f64, 0f64);
    for l in r.lines() {
        let f = l.split_whitespace().next().unwrap_or("");
        if let Some((x, y)) = f.split_once('-').filter(|(x, _)| x.chars().all(|c| c.is_ascii_hexdigit())) {
            let (x, y) = (usize::from_str_radix(x, 16).unwrap_or(0), usize::from_str_radix(y, 16).unwrap_or(0));
            inside = x < b && y > a;
        } else if inside {
            let kb = || l.split_whitespace().nth(1).and_then(|v| v.parse::<f64>().ok()).unwrap_or(0.0);
            if l.starts_with("Rss:") { rss += kb(); } else if l.starts_with("AnonHugePages:") { huge += kb(); }
        }
    }
    format!("table on huge pages {:.2} of {:.2} GB", huge / 1e6, rss / 1e6)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let dir = &args[1];
    let variant = args.get(2).map(|s| s.as_str()).unwrap_or("v0");
    if variant == "census" { return bits::census(dir); }
    if variant == "splits" { return bits::splits(dir); }
    if variant == "packtime" { return bits::pack_time(dir); }
    if variant == "cellcensus" { return bits::cell_census(dir); }
    if variant == "structure" { return structure::structure(dir); }
    if variant == "sharing" { return structure::sharing(dir); }
    if variant == "posregion" { return posregion::posregion(dir); }
    let t0 = Instant::now();
    // Inputs, mapped.
    let mut files: Vec<_> = std::fs::read_dir(dir).unwrap().flatten().map(|e| e.path()).filter(|p| p.file_name().unwrap().to_str().unwrap().starts_with('w')).collect();
    files.sort();
    let maps: Vec<memmap2::Mmap> = files.iter().map(|p| unsafe { memmap2::Mmap::map(&std::fs::File::open(p).unwrap()).unwrap() }).collect();
    let door = unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{dir}/door.bin")).unwrap()).unwrap() };
    let srcf = unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{dir}/src.bin")).unwrap()).unwrap() };
    // Source id -> dense index (for the drop notes).
    let n_src = srcf.len() / 20;
    let mut src_index: rustc_hash::FxHashMap<u64, u32> = rustc_hash::FxHashMap::default();
    src_index.reserve(n_src);
    for i in 0..n_src {
        let id = u64::from_le_bytes(srcf[i * 20..i * 20 + 8].try_into().unwrap());
        src_index.insert(id, i as u32);
    }
    let n_door = door.len() / 40;
    let n_rec: usize = maps.iter().map(|m| m.len() / REC).sum();
    eprintln!("[load] {} worker files, {n_rec} records, door {n_door}, sources {n_src}; {:.2} s", maps.len(), t0.elapsed().as_secs_f64());

    match variant {
        "v0" => v0(&maps, &door, &src_index, n_src),
        "analyze" => analyze(&maps, &door, &srcf),
        "v1" => vcell(&maps, &door, &srcf, false),
        "v2" => vcell(&maps, &door, &srcf, true),
        "ages" => ages(&maps, &door),
        "v3" => v3(&maps, &door, &srcf),
        "prep" => prep(dir, &maps, &door, &srcf),
        "v3c" => vq(dir, &door, false),
        "v4" => vq(dir, &door, true),
        "prepbits" => bits::prep_bits(dir, &door),
        "bits" => bits::run_bits(dir, door.len() / 40, false),
        "bitsr" => bits::run_bits(dir, door.len() / 40, true),
        "bitintern" => bits::run_intern(dir, door.len() / 40),
        "bitcell" | "bitspd" | "bitspd2" | "posmask4" | "posmask8" => bits::run_words(dir, door.len() / 40, variant),
        "mlp" => mlp::main(dir, &door, &args[3], &args[4]),
        other => panic!("unknown variant {other}"),
    }
}

/// v0: the naive baseline. One global visited set, 1024 shards of
/// Mutex<HashMap<key, id>>, pre-filled; per emission one probe.
fn v0(maps: &[memmap2::Mmap], door: &[u8], src_index: &rustc_hash::FxHashMap<u64, u32>, n_src: usize) {
    const SH: usize = 1024;
    let shard_of = |k: u128| ((k as u64) >> 54) as usize & (SH - 1);
    let t = Instant::now();
    let shards: Vec<Mutex<rustc_hash::FxHashMap<u128, u32>>> = (0..SH).map(|_| Mutex::new(Default::default())).collect();
    let n_door = door.len() / 40;
    // Pre-fill in parallel by shard ownership.
    std::thread::scope(|s| {
        let threads = 16;
        for t in 0..threads {
            let shards = &shards;
            s.spawn(move || {
                for i in 0..n_door {
                    let b = &door[i * 40..i * 40 + 40];
                    let k = (u64::from_le_bytes(b[16..24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(b[24..32].try_into().unwrap()) as u128) << 64);
                    let sh = shard_of(k);
                    if sh % threads == t {
                        shards[sh].lock().unwrap().insert(k, i as u32);
                    }
                }
            });
        }
    });
    eprintln!("[v0] visited set built: {n_door} entries, {:.2} s", t.elapsed().as_secs_f64());
    let next_id = AtomicU32::new(n_door as u32);
    let drops: Vec<AtomicU32> = (0..n_src).map(|_| AtomicU32::new(u32::MAX)).collect();
    let (new_total, edges_total, dropped_total) = (AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0));
    let t = Instant::now();
    std::thread::scope(|s| {
        for m in maps {
            let (shards, next_id, drops, new_total, edges_total, dropped_total) = (&shards, &next_id, &drops, &new_total, &edges_total, &dropped_total);
            s.spawn(move || {
                let mut frontier: Vec<u8> = Vec::new();
                let mut edges: Vec<(u32, u32, u32)> = Vec::new();
                let (mut new, mut dropped) = (0u64, 0u64);
                for c in m.chunks_exact(REC) {
                    let r = rec(c);
                    if r.flags == 2 { continue; }
                    let src = src_index[&r.src];
                    if r.flags == 1 {
                        drops[src as usize].fetch_min(r.cell, Ordering::Relaxed);
                        dropped += 1;
                        continue;
                    }
                    let id = {
                        let mut sh = shards[shard_of(r.key)].lock().unwrap();
                        match sh.get(&r.key) {
                            Some(&id) => id,
                            None => {
                                let id = next_id.fetch_add(1, Ordering::Relaxed);
                                sh.insert(r.key, id);
                                new += 1;
                                frontier.extend_from_slice(&[r.shape as u8; PAYLOAD]);
                                id
                            }
                        }
                    };
                    edges.push((src, id, r.xfer));
                }
                new_total.fetch_add(new, Ordering::Relaxed);
                edges_total.fetch_add(edges.len() as u64, Ordering::Relaxed);
                dropped_total.fetch_add(dropped, Ordering::Relaxed);
                std::hint::black_box((&frontier, &edges));
            });
        }
    });
    let dt = t.elapsed().as_secs_f64();
    let new = new_total.load(Ordering::Relaxed);
    eprintln!("[v0] replay {dt:.2} s: new states {new} (want 6735699: {}), edge records {}, dropped {}",
        if new == 6735699 { "OK" } else { "MISMATCH" }, edges_total.load(Ordering::Relaxed), dropped_total.load(Ordering::Relaxed));
}


const GRID: i32 = 512;
const ORIGIN: i32 = -64;
fn cell_xy(c: u32) -> (i32, i32) { (c as i32 % GRID + ORIGIN, c as i32 / GRID + ORIGIN) }

fn hilbert(n: u32, mut x: u32, mut y: u32) -> u64 {
    let mut d: u64 = 0;
    let mut s = n / 2;
    while s > 0 {
        let rx = ((x & s) > 0) as u32;
        let ry = ((y & s) > 0) as u32;
        d += (s as u64) * (s as u64) * ((3 * rx) ^ ry) as u64;
        if ry == 0 {
            if rx == 1 { x = s - 1 - (x & (s - 1)) + (x & !(s - 1)) - (x & !(s-1)); y = s - 1 - (y & (s - 1)) + (y & !(s - 1)) - (y & !(s-1)); }
            std::mem::swap(&mut x, &mut y);
        }
        s /= 2;
    }
    d
}

/// Where the lookups' duplicates come from, and how tight a spatial sweep's
/// live window over the visited set (split by target cell) is.
fn analyze(maps: &[memmap2::Mmap], door: &[u8], srcf: &[u8]) {
    let t = Instant::now();
    let n_door = door.len() / 40;
    let mut set: rustc_hash::FxHashMap<u128, u32> = rustc_hash::FxHashMap::default();
    set.reserve(n_door + 8_000_000);
    let mut entry_cell: Vec<u32> = Vec::with_capacity(n_door + 8_000_000);
    for i in 0..n_door {
        let b = &door[i * 40..i * 40 + 40];
        let k = (u64::from_le_bytes(b[16..24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(b[24..32].try_into().unwrap()) as u128) << 64);
        set.insert(k, i as u32);
        entry_cell.push(u32::from_le_bytes(b[8..12].try_into().unwrap()));
    }
    let n_src = srcf.len() / 20;
    let mut src_cell: rustc_hash::FxHashMap<u64, u32> = rustc_hash::FxHashMap::default();
    for i in 0..n_src {
        let id = u64::from_le_bytes(srcf[i * 20..i * 20 + 8].try_into().unwrap());
        src_cell.insert(id, u32::from_le_bytes(srcf[i * 20 + 8..i * 20 + 12].try_into().unwrap()));
    }
    eprintln!("[analyze] loaded set {n_door}, sources {n_src}: {:.1} s", t.elapsed().as_secs_f64());
    // Pass 1: lookups in production order.
    let (mut hit_old, mut hit_new, mut miss) = (0u64, 0u64, 0u64);
    let mut indeg: Vec<u32> = vec![0; n_door + 8_000_000];
    let mut pairs: Vec<(u32, u32)> = Vec::with_capacity(260_000_000); // (src cell, target entry)
    for m in maps {
        for c in m.chunks_exact(REC) {
            let r = rec(c);
            if r.flags != 0 { continue; }
            let id = match set.get(&r.key) {
                Some(&id) => { if (id as usize) < n_door { hit_old += 1 } else { hit_new += 1 } id }
                None => { let id = entry_cell.len() as u32; set.insert(r.key, id); entry_cell.push(r.cell); miss += 1; id }
            };
            indeg[id as usize] += 1;
            pairs.push((src_cell[&r.src], id));
        }
    }
    let total = hit_old + hit_new + miss;
    let n_entries = entry_cell.len();
    eprintln!("[analyze] {total} lookups: {hit_old} hit states from EARLIER frames ({:.1}%), {hit_new} hit states NEW this frame ({:.1}%), {miss} inserts ({:.1}%); {:.1} s",
        100.0 * hit_old as f64 / total as f64, 100.0 * hit_new as f64 / total as f64, 100.0 * miss as f64 / total as f64, t.elapsed().as_secs_f64());
    // In-degree distribution over the touched targets.
    let mut touched_old = 0u64; let mut touched_new = 0u64;
    let mut degs: Vec<u32> = Vec::new();
    for (i, &d) in indeg.iter().enumerate().take(n_entries) { if d > 0 { degs.push(d); if i < n_door { touched_old += 1 } else { touched_new += 1 } } }
    degs.sort_unstable();
    let pct = |p: f64| degs[((degs.len() - 1) as f64 * p) as usize];
    eprintln!("[analyze] distinct targets {} ({} old of the set's {n_door} = {:.1}%, {} new); lookups per target: mean {:.1}, p50 {}, p90 {}, p99 {}, max {}",
        degs.len(), touched_old, 100.0 * touched_old as f64 / n_door as f64, touched_new, total as f64 / degs.len() as f64, pct(0.5), pct(0.9), pct(0.99), degs[degs.len() - 1]);
    // Sources per target cell pair: how many distinct (src cell, target) pairs?
    // Live windows per sweep order.
    let mut cell_weight: rustc_hash::FxHashMap<u32, u64> = rustc_hash::FxHashMap::default();
    for &c in &entry_cell { *cell_weight.entry(c).or_default() += 1; }
    let mut src_cells: Vec<u32> = src_cell.values().copied().collect();
    src_cells.sort_unstable(); src_cells.dedup();
    let orders: Vec<(&str, Box<dyn Fn(u32) -> u64>)> = vec![
        ("production (16px region, then cell)", Box::new(|c| { let (x, y) = cell_xy(c); (((x.div_euclid(16) + 16) as u64) << 40) | (((y.div_euclid(16) + 16) as u64) << 32) | c as u64 })),
        ("x-major (left to right)", Box::new(|c| { let (x, y) = cell_xy(c); (((x + 64) as u64) << 16) | (y + 64) as u64 })),
        ("y-major (top to bottom)", Box::new(|c| { let (x, y) = cell_xy(c); (((y + 64) as u64) << 16) | (x + 64) as u64 })),
        ("hilbert", Box::new(|c| { let (x, y) = cell_xy(c); hilbert(512, (x + 64) as u32, (y + 64) as u32) })),
        ("radial from the spawn (32,104)", Box::new(|c| { let (x, y) = cell_xy(c); (((x - 32) * (x - 32) + (y - 104) * (y - 104)) as u64) << 32 | c as u64 })),
        ("random", Box::new(|c| (c as u64).wrapping_mul(0x9e3779b97f4a7c15))),
    ];
    let total_w: u64 = cell_weight.values().sum();
    for (name, key) in &orders {
        let mut sorted = src_cells.clone();
        sorted.sort_by_key(|&c| key(c));
        let rank: rustc_hash::FxHashMap<u32, u32> = sorted.iter().enumerate().map(|(i, &c)| (c, i as u32)).collect();
        let n = sorted.len();
        let mut first: rustc_hash::FxHashMap<u32, u32> = rustc_hash::FxHashMap::default();
        let mut last: rustc_hash::FxHashMap<u32, u32> = rustc_hash::FxHashMap::default();
        for &(sc, e) in &pairs {
            let t = rank[&sc];
            let tc = entry_cell[e as usize];
            let f = first.entry(tc).or_insert(t); if t < *f { *f = t; }
            let l = last.entry(tc).or_insert(t); if t > *l { *l = t; }
        }
        let mut diff = vec![0i64; n + 1];
        let mut touched_w = 0u64;
        for (tc, &f) in &first { let l = last[tc]; let w = cell_weight[tc] as i64; diff[f as usize] += w; diff[l as usize + 1] -= w; touched_w += w as u64; }
        let (mut cur, mut peak, mut sum) = (0i64, 0i64, 0f64);
        for t in 0..n { cur += diff[t]; peak = peak.max(cur); sum += cur as f64; }
        let mean = sum / n as f64;
        let spans: Vec<u32> = first.iter().map(|(tc, &f)| last[tc] - f).collect();
        let mean_span = spans.iter().map(|&s| s as f64).sum::<f64>() / spans.len() as f64;
        eprintln!("[window] {name}: {n} source cells; live entries peak {peak} ({:.1}% of the set's {total_w}, {:.0} MB at 24 B), mean {:.0} ({:.1}%); touched {:.1}%; mean target-cell span {:.0} source cells ({:.1}% of the sweep)",
            100.0 * peak as f64 / total_w as f64, peak as f64 * 24.0 / 1e6, mean, 100.0 * mean / total_w as f64, 100.0 * touched_w as f64 / total_w as f64, mean_span, 100.0 * mean_span / n as f64);
    }
}


type Shard = Mutex<rustc_hash::FxHashMap<u128, u32>>;

/// v1/v2: the visited set laid out by POSITION: one small hash table per
/// (shape, cell), found through a cell directory. v1 replays production order
/// per worker file; v2 replays a global left-to-right sweep over source cells,
/// cut into one contiguous x-band per thread (equal work).
fn vcell(maps: &[memmap2::Mmap], door: &[u8], srcf: &[u8], sweep: bool) {
    let t = Instant::now();
    let n_door = door.len() / 40;
    // Directory: (shape, cell) -> shard index.
    let mut dir: rustc_hash::FxHashMap<(u64, u32), u32> = rustc_hash::FxHashMap::default();
    let mut shards: Vec<rustc_hash::FxHashMap<u128, u32>> = Vec::new();
    for i in 0..n_door {
        let b = &door[i * 40..i * 40 + 40];
        let shape = u64::from_le_bytes(b[0..8].try_into().unwrap());
        let cell = u32::from_le_bytes(b[8..12].try_into().unwrap());
        let k = (u64::from_le_bytes(b[16..24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(b[24..32].try_into().unwrap()) as u128) << 64);
        let s = *dir.entry((shape, cell)).or_insert_with(|| { shards.push(Default::default()); (shards.len() - 1) as u32 });
        shards[s as usize].insert(k, i as u32);
    }
    // Shards for target cells not yet in the set (new cells this frame).
    for m in maps { for c in m.chunks_exact(REC) { let r = rec(c); if r.flags == 0 { dir.entry((r.shape, r.cell)).or_insert_with(|| { shards.push(Default::default()); (shards.len() - 1) as u32 }); } } }
    let shards: Vec<Shard> = shards.into_iter().map(Mutex::new).collect();
    eprintln!("[v{}] {} cell shards built ({} entries): {:.1} s", if sweep { 2 } else { 1 }, shards.len(), n_door, t.elapsed().as_secs_f64());
    // The work lists: per thread, (shard, key, src index, xfer) in replay order.
    let n_src = srcf.len() / 20;
    let mut src_of: rustc_hash::FxHashMap<u64, (u32, u32)> = rustc_hash::FxHashMap::default();
    for i in 0..n_src { let id = u64::from_le_bytes(srcf[i * 20..i * 20 + 8].try_into().unwrap()); src_of.insert(id, (i as u32, u32::from_le_bytes(srcf[i * 20 + 8..i * 20 + 12].try_into().unwrap()))); }
    type Job = (u32, u128, u32, u32, u32); // shard, key, src, xfer, flags
    let threads = 16usize;
    let mut lists: Vec<Vec<Job>> = vec![Vec::new(); threads];
    if !sweep {
        for (w, m) in maps.iter().enumerate() {
            for c in m.chunks_exact(REC) {
                let r = rec(c);
                if r.flags == 2 { continue; }
                let (si, _) = src_of[&r.src];
                let sh = if r.flags == 0 { dir[&(r.shape, r.cell)] } else { 0 };
                lists[w % threads].push((sh, r.key, si, r.xfer, r.flags));
            }
        }
    } else {
        let mut all: Vec<(u64, Job)> = Vec::with_capacity(512_000_000);
        for m in maps {
            for c in m.chunks_exact(REC) {
                let r = rec(c);
                if r.flags == 2 { continue; }
                let (si, scell) = src_of[&r.src];
                let (x, y) = cell_xy(scell);
                let order = (((x + 64) as u64) << 48) | (((y + 64) as u64) << 32) | si as u64;
                let sh = if r.flags == 0 { dir[&(r.shape, r.cell)] } else { 0 };
                all.push((order, (sh, r.key, si, r.xfer, r.flags)));
            }
        }
        all.sort_unstable_by_key(|a| a.0);
        let per = all.len().div_ceil(threads);
        for (i, (_, j)) in all.into_iter().enumerate() { lists[(i / per).min(threads - 1)].push(j); }
    }
    eprintln!("[v{}] work lists ready: {:.1} s (not timed below)", if sweep { 2 } else { 1 }, t.elapsed().as_secs_f64());
    let next_id = AtomicU32::new(n_door as u32);
    let drops: Vec<AtomicU32> = (0..n_src).map(|_| AtomicU32::new(u32::MAX)).collect();
    let new_total = AtomicU64::new(0);
    let t = Instant::now();
    std::thread::scope(|s| {
        for list in &lists {
            let (shards, next_id, drops, new_total) = (&shards, &next_id, &drops, &new_total);
            s.spawn(move || {
                let mut frontier: Vec<u8> = Vec::new();
                let mut edges: Vec<(u32, u32, u32)> = Vec::new();
                let mut new = 0u64;
                for &(sh, key, src, xfer, flags) in list {
                    if flags == 1 { drops[src as usize].fetch_min(1, Ordering::Relaxed); continue; }
                    let id = {
                        let mut m = shards[sh as usize].lock().unwrap();
                        match m.get(&key) {
                            Some(&id) => id,
                            None => { let id = next_id.fetch_add(1, Ordering::Relaxed); m.insert(key, id); new += 1; frontier.extend_from_slice(&[0u8; PAYLOAD]); id }
                        }
                    };
                    edges.push((src, id, xfer));
                }
                new_total.fetch_add(new, Ordering::Relaxed);
                std::hint::black_box((&frontier, &edges));
            });
        }
    });
    let dt = t.elapsed().as_secs_f64();
    let new = new_total.load(Ordering::Relaxed);
    eprintln!("[v{}] replay {dt:.2} s: new states {new} ({})", if sweep { 2 } else { 1 }, if new == 6735699 { "OK" } else { "MISMATCH" });
}

/// The age of the old states hit this frame: frame - the layer they were
/// first reached at (the id's top 16 bits), and per past layer the share of
/// its states touched this frame.
fn ages(maps: &[memmap2::Mmap], door: &[u8]) {
    let n_door = door.len() / 40;
    let mut set: rustc_hash::FxHashMap<u128, u32> = rustc_hash::FxHashMap::default();
    set.reserve(n_door);
    let mut layer: Vec<u16> = Vec::with_capacity(n_door);
    for i in 0..n_door {
        let b = &door[i * 40..i * 40 + 40];
        let k = (u64::from_le_bytes(b[16..24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(b[24..32].try_into().unwrap()) as u128) << 64);
        set.insert(k, i as u32);
        layer.push((u64::from_le_bytes(b[32..40].try_into().unwrap()) >> 48) as u16);
    }
    let mut touched = vec![false; n_door];
    let mut hits_by_layer = [0u64; 64];
    for m in maps { for c in m.chunks_exact(REC) { let r = rec(c); if r.flags != 0 { continue; } if let Some(&i) = set.get(&r.key) { touched[i as usize] = true; hits_by_layer[layer[i as usize] as usize] += 1; } } }
    let mut size = [0u64; 64]; let mut tch = [0u64; 64];
    for i in 0..n_door { size[layer[i] as usize] += 1; if touched[i] { tch[layer[i] as usize] += 1; } }
    let total_hits: u64 = hits_by_layer.iter().sum();
    eprintln!("[ages] frame 57: per layer L (age 57-L): states, share touched this frame, share of this frame's old hits");
    for l in 0..64 { if size[l] > 0 { eprintln!("[ages]   L{l:02} age {:2}: {:9} states, {:5.1}% touched, {:5.1}% of hits", 57 - l, size[l], 100.0 * tch[l] as f64 / size[l] as f64, 100.0 * hits_by_layer[l] as f64 / total_hits as f64); } }
}


/// v3: SINGLE-THREADED. Queries in a left-to-right sweep over source cells;
/// the visited set as one flat open-addressing table per (shape, cell) in
/// one arena (keys and ids in separate arrays; the key's low bits index).
fn v3(maps: &[memmap2::Mmap], door: &[u8], srcf: &[u8]) {
    let t = Instant::now();
    let n_door = door.len() / 40;
    let n_src = srcf.len() / 20;
    let mut src_of: rustc_hash::FxHashMap<u64, (u32, u32)> = rustc_hash::FxHashMap::default();
    for i in 0..n_src { let id = u64::from_le_bytes(srcf[i * 20..i * 20 + 8].try_into().unwrap()); src_of.insert(id, (i as u32, u32::from_le_bytes(srcf[i * 20 + 8..i * 20 + 12].try_into().unwrap()))); }
    // Shards: count old entries + this frame's distinct new keys per (shape, cell).
    let mut dir: rustc_hash::FxHashMap<(u64, u32), u32> = rustc_hash::FxHashMap::default();
    let mut count: Vec<u64> = Vec::new();
    let shard = |dir: &mut rustc_hash::FxHashMap<(u64, u32), u32>, count: &mut Vec<u64>, k: (u64, u32)| -> u32 { *dir.entry(k).or_insert_with(|| { count.push(0); (count.len() - 1) as u32 }) };
    for i in 0..n_door { let b = &door[i * 40..i * 40 + 40]; let s = shard(&mut dir, &mut count, (u64::from_le_bytes(b[0..8].try_into().unwrap()), u32::from_le_bytes(b[8..12].try_into().unwrap()))); count[s as usize] += 1; }
    // Queries (sweep order) and drops.
    let mut q: Vec<(u64, u32, u32, u32, u128)> = Vec::with_capacity(260_000_000); // order, shard, src, xfer, key
    let mut drops_list: Vec<(u64, u32)> = Vec::with_capacity(260_000_000);
    let mut newkeys: rustc_hash::FxHashSet<u128> = Default::default();
    for m in maps { for c in m.chunks_exact(REC) {
        let r = rec(c);
        if r.flags == 2 { continue; }
        let (si, scell) = src_of[&r.src];
        let (x, y) = cell_xy(scell);
        let order = (((x + 64) as u64) << 48) | (((y + 64) as u64) << 32) | si as u64;
        if r.flags == 1 { drops_list.push((order, si)); continue; }
        let s = shard(&mut dir, &mut count, (r.shape, r.cell));
        if newkeys.insert(r.key) { count[s as usize] += 1; }
        q.push((order, s, si, r.xfer, r.key));
    } }
    q.sort_unstable_by_key(|a| a.0);
    drops_list.sort_unstable_by_key(|a| a.0);
    // Compact query stream: (shard u32, src u32, xfer u32, pad, key u128) = 32 B aligned.
    #[repr(C)] #[derive(Clone, Copy)] struct Q { shard: u32, src: u32, xfer: u32, _p: u32, key: u128 }
    let qs: Vec<Q> = q.iter().map(|a| Q { shard: a.1, src: a.2, xfer: a.3, _p: 0, key: a.4 }).collect();
    let ds: Vec<u32> = drops_list.iter().map(|a| a.1).collect();
    drop(q); drop(drops_list); drop(newkeys);
    // Arena: per shard capacity = next pow2 >= 2 * count (load <= 0.5).
    let mut off: Vec<(u64, u64)> = Vec::with_capacity(count.len());
    let mut total = 0u64;
    for &c in &count { let cap = (2 * c.max(1)).next_power_of_two(); off.push((total, cap - 1)); total += cap; }
    let mut keys: Vec<u128> = vec![0; total as usize]; // 0 = empty (keys are hashes; 0 does not occur)
    let mut ids: Vec<u32> = vec![0; total as usize];
    let probe_insert = |keys: &mut [u128], ids: &mut [u32], (base, mask): (u64, u64), k: u128, id: u32| -> (u32, bool) {
        let mut i = (k as u64) & mask;
        loop {
            let slot = (base + i) as usize;
            let kk = keys[slot];
            if kk == k { return (ids[slot], false); }
            if kk == 0 { keys[slot] = k; ids[slot] = id; return (id, true); }
            i = (i + 1) & mask;
        }
    };
    for i in 0..n_door {
        let b = &door[i * 40..i * 40 + 40];
        let s = dir[&(u64::from_le_bytes(b[0..8].try_into().unwrap()), u32::from_le_bytes(b[8..12].try_into().unwrap()))];
        let k = (u64::from_le_bytes(b[16..24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(b[24..32].try_into().unwrap()) as u128) << 64);
        probe_insert(&mut keys, &mut ids, off[s as usize], k, i as u32);
    }
    eprintln!("[v3] setup {:.1} s (untimed): {} shards, arena {} slots ({:.2} GB), {} queries ({:.2} GB), {} drops",
        t.elapsed().as_secs_f64(), off.len(), total, total as f64 * 20.0 / 1e9, qs.len(), qs.len() as f64 * 32.0 / 1e9, ds.len());
    // THE TIMED LOOP.
    let mut frontier: Vec<u8> = Vec::with_capacity(7_000_000 * PAYLOAD);
    let mut edges: Vec<(u32, u32, u32)> = Vec::with_capacity(qs.len());
    let mut dropmin: Vec<u32> = vec![u32::MAX; n_src];
    let mut next = n_door as u32;
    let t = Instant::now();
    for &d in &ds { let m = &mut dropmin[d as usize]; *m = (*m).min(1); }
    let t_drops = t.elapsed().as_secs_f64();
    let mut new = 0u64;
    for qq in &qs {
        let (id, ins) = probe_insert(&mut keys, &mut ids, off[qq.shard as usize], qq.key, next);
        if ins { next += 1; new += 1; frontier.extend_from_slice(&[0u8; PAYLOAD]); }
        edges.push((qq.src, id, qq.xfer));
    }
    let dt = t.elapsed().as_secs_f64();
    std::hint::black_box((&frontier, &edges, &dropmin));
    eprintln!("[v3] TIMED single thread {dt:.2} s (drops {t_drops:.2} s): new states {new} ({}), edges {}; {:.1} ns per query",
        if new == 6735699 { "OK" } else { "MISMATCH" }, edges.len(), (dt - t_drops) * 1e9 / qs.len() as f64);
}


#[repr(C)]
#[derive(Clone, Copy)]
struct Q { shard: u32, src: u32, xfer: u32, _p: u32, key: u128 }

fn as_bytes<T>(v: &[T]) -> &[u8] { unsafe { std::slice::from_raw_parts(v.as_ptr() as *const u8, std::mem::size_of_val(v)) } }
fn from_bytes<T: Copy>(b: &[u8]) -> &[T] { assert_eq!(b.as_ptr() as usize % std::mem::align_of::<T>(), 0); unsafe { std::slice::from_raw_parts(b.as_ptr() as *const T, b.len() / std::mem::size_of::<T>()) } }

/// Prepare the sweep-ordered inputs once: DIR/prep/{q.bin, d.bin, cnt.bin, dshard.bin},
/// plus the per-STATE live window of the sweep.
fn prep(dir: &str, maps: &[memmap2::Mmap], door: &[u8], srcf: &[u8]) {
    let t = Instant::now();
    let n_door = door.len() / 40;
    let n_src = srcf.len() / 20;
    let mut src_of: rustc_hash::FxHashMap<u64, (u32, u32)> = rustc_hash::FxHashMap::default();
    for i in 0..n_src { let id = u64::from_le_bytes(srcf[i * 20..i * 20 + 8].try_into().unwrap()); src_of.insert(id, (i as u32, u32::from_le_bytes(srcf[i * 20 + 8..i * 20 + 12].try_into().unwrap()))); }
    let mut dir_: rustc_hash::FxHashMap<(u64, u32), u32> = rustc_hash::FxHashMap::default();
    let mut count: Vec<u64> = Vec::new();
    let mut shard = |k: (u64, u32), count: &mut Vec<u64>| -> u32 { *dir_.entry(k).or_insert_with(|| { count.push(0); (count.len() - 1) as u32 }) };
    let mut dshard: Vec<u32> = Vec::with_capacity(n_door);
    let mut keyidx: rustc_hash::FxHashMap<u128, u32> = rustc_hash::FxHashMap::default();
    keyidx.reserve(n_door + 7_000_000);
    for i in 0..n_door {
        let b = &door[i * 40..i * 40 + 40];
        let s = shard((u64::from_le_bytes(b[0..8].try_into().unwrap()), u32::from_le_bytes(b[8..12].try_into().unwrap())), &mut count);
        count[s as usize] += 1; dshard.push(s);
        let k = (u64::from_le_bytes(b[16..24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(b[24..32].try_into().unwrap()) as u128) << 64);
        keyidx.insert(k, i as u32);
    }
    let mut q: Vec<(u64, Q)> = Vec::with_capacity(258_000_000);
    let mut d: Vec<(u64, u32)> = Vec::with_capacity(255_000_000);
    let mut next = n_door as u32;
    for m in maps { for c in m.chunks_exact(REC) {
        let r = rec(c);
        if r.flags == 2 { continue; }
        let (si, scell) = src_of[&r.src];
        let (x, y) = cell_xy(scell);
        let order = (((x + 64) as u64) << 48) | (((y + 64) as u64) << 32) | si as u64;
        if r.flags == 1 { d.push((order, si)); continue; }
        let s = shard((r.shape, r.cell), &mut count);
        keyidx.entry(r.key).or_insert_with(|| { count[s as usize] += 1; next += 1; next - 1 });
        q.push((order, Q { shard: s, src: si, xfer: r.xfer, _p: 0, key: r.key }));
    } }
    q.sort_unstable_by_key(|a| a.0);
    d.sort_unstable_by_key(|a| a.0);
    // Per-STATE live window over the sweep (time = query index).
    let n_states = next as usize;
    let mut first = vec![u32::MAX; n_states]; let mut last = vec![0u32; n_states];
    for (i, (_, qq)) in q.iter().enumerate() { let e = keyidx[&qq.key] as usize; if first[e] == u32::MAX { first[e] = i as u32; } last[e] = i as u32; }
    let n = q.len();
    let mut diff = vec![0i64; n + 1];
    let mut touched = 0u64;
    for e in 0..n_states { if first[e] != u32::MAX { diff[first[e] as usize] += 1; diff[last[e] as usize + 1] -= 1; touched += 1; } }
    let (mut cur, mut peak, mut sum) = (0i64, 0i64, 0f64);
    for t in 0..n { cur += diff[t]; peak = peak.max(cur); sum += cur as f64; }
    eprintln!("[prep] per-STATE live window over the x-major sweep: {touched} states touched; live peak {peak} ({:.0} MB at 12 B, {:.0} MB at 20 B), mean {:.0}",
        peak as f64 * 12.0 / 1e6, peak as f64 * 20.0 / 1e6, sum / n as f64);
    let pd = format!("{dir}/prep");
    std::fs::create_dir_all(&pd).unwrap();
    let qs: Vec<Q> = q.into_iter().map(|a| a.1).collect();
    let ds: Vec<u32> = d.into_iter().map(|a| a.1).collect();
    std::fs::write(format!("{pd}/q.bin"), as_bytes(&qs)).unwrap();
    std::fs::write(format!("{pd}/d.bin"), as_bytes(&ds)).unwrap();
    std::fs::write(format!("{pd}/cnt.bin"), as_bytes(&count)).unwrap();
    std::fs::write(format!("{pd}/dshard.bin"), as_bytes(&dshard)).unwrap();
    std::fs::write(format!("{pd}/nsrc.txt"), format!("{n_src}")).unwrap();
    eprintln!("[prep] wrote {pd}: {} queries, {} drops, {} shards; {:.1} s", qs.len(), ds.len(), count.len(), t.elapsed().as_secs_f64());
}

/// v3c / v4 from the prepared inputs. v3c: per shard open addressing, load
/// <= 0.5, 16-B keys + 4-B ids. v4: load <= 0.8, 8-B fingerprint + 4-B id in
/// one 12-B slot (an EXPLORATION: production needs exact keys).
fn vq(dir: &str, door: &[u8], dense: bool) {
    let t = Instant::now();
    let pd = format!("{dir}/prep");
    let map = |f: &str| unsafe { memmap2::Mmap::map(&std::fs::File::open(format!("{pd}/{f}")).unwrap()).unwrap() };
    let (qm, dm, cm, sm) = (map("q.bin"), map("d.bin"), map("cnt.bin"), map("dshard.bin"));
    let qs: &[Q] = from_bytes(&qm); let ds: &[u32] = from_bytes(&dm); let cnt: &[u64] = from_bytes(&cm); let dsh: &[u32] = from_bytes(&sm);
    let n_src: usize = std::fs::read_to_string(format!("{pd}/nsrc.txt")).unwrap().trim().parse().unwrap();
    let n_door = door.len() / 40;
    let load = if dense { 0.8 } else { 0.5 };
    let mut off: Vec<(u64, u64)> = Vec::with_capacity(cnt.len());
    let mut total = 0u64;
    for &c in cnt { let cap = ((c.max(1) as f64 / load).ceil() as u64).next_power_of_two(); off.push((total, cap - 1)); total += cap; }
    let tag = if dense { "v4" } else { "v3c" };
    let mut frontier: Vec<u8> = huge_cap(7_000_000 * PAYLOAD);
    let mut edges: Vec<(u32, u32, u32)> = huge_cap(qs.len());
    let mut dropmin: Vec<u32> = vec![u32::MAX; n_src];
    let key_of = |i: usize| (u64::from_le_bytes(door[i * 40 + 16..i * 40 + 24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(door[i * 40 + 24..i * 40 + 32].try_into().unwrap()) as u128) << 64);
    let (dt, t_drops, new, hp_tab);
    if dense {
        // slot: (fingerprint u64, id u32) packed into 12 B; fingerprint 0 = empty.
        #[repr(C, packed)] #[derive(Clone, Copy)] struct S { fp: u64, id: u32 }
        let mut tab: Vec<S> = huge_vec(total as usize, S { fp: 0, id: 0 });
        let fp_of = |k: u128| { let f = (k >> 64) as u64; if f == 0 { 1 } else { f } };
        let pi = |tab: &mut [S], (base, mask): (u64, u64), k: u128, id: u32| -> (u32, bool) {
            let f = fp_of(k); let mut i = (k as u64) & mask;
            loop { let sl = &mut tab[(base + i) as usize]; let g = sl.fp; if g == f { return (sl.id, false); } if g == 0 { *sl = S { fp: f, id }; return (id, true); } i = (i + 1) & mask; }
        };
        for i in 0..n_door { pi(&mut tab, off[dsh[i] as usize], key_of(i), i as u32); }
        eprintln!("[{tag}] setup {:.1} s: arena {} slots ({:.2} GB)", t.elapsed().as_secs_f64(), total, total as f64 * 12.0 / 1e9);
        perf_on(true);
        let t = Instant::now();
        for &d in ds { let m = &mut dropmin[d as usize]; *m = (*m).min(1); }
        t_drops = t.elapsed().as_secs_f64();
        let mut next = n_door as u32; let mut nw = 0u64;
        for qq in qs { let (id, ins) = pi(&mut tab, off[qq.shard as usize], qq.key, next); if ins { next += 1; nw += 1; frontier.extend_from_slice(&[0u8; PAYLOAD]); } edges.push((qq.src, id, qq.xfer)); }
        dt = t.elapsed().as_secs_f64(); new = nw; perf_on(false);
        hp_tab = hp_at(&tab);
    } else {
        let mut keys: Vec<u128> = huge_vec(total as usize, 0); let mut ids: Vec<u32> = huge_vec(total as usize, 0);
        let pi = |keys: &mut [u128], ids: &mut [u32], (base, mask): (u64, u64), k: u128, id: u32| -> (u32, bool) {
            let mut i = (k as u64) & mask;
            loop { let s = (base + i) as usize; let kk = keys[s]; if kk == k { return (ids[s], false); } if kk == 0 { keys[s] = k; ids[s] = id; return (id, true); } i = (i + 1) & mask; }
        };
        for i in 0..n_door { pi(&mut keys, &mut ids, off[dsh[i] as usize], key_of(i), i as u32); }
        eprintln!("[{tag}] setup {:.1} s: arena {} slots ({:.2} GB)", t.elapsed().as_secs_f64(), total, total as f64 * 20.0 / 1e9);
        perf_on(true);
        let t = Instant::now();
        for &d in ds { let m = &mut dropmin[d as usize]; *m = (*m).min(1); }
        t_drops = t.elapsed().as_secs_f64();
        let mut next = n_door as u32; let mut nw = 0u64;
        for qq in qs { let (id, ins) = pi(&mut keys, &mut ids, off[qq.shard as usize], qq.key, next); if ins { next += 1; nw += 1; frontier.extend_from_slice(&[0u8; PAYLOAD]); } edges.push((qq.src, id, qq.xfer)); }
        dt = t.elapsed().as_secs_f64(); new = nw; perf_on(false);
        hp_tab = format!("{} + {}", hp_at(&keys), hp_at(&ids));
    }
    std::hint::black_box((&frontier, &edges, &dropmin));
    eprintln!("[{tag}] TIMED single thread {dt:.2} s (drops {t_drops:.2} s): new {new} ({}), {:.1} ns per query; {}; after: {}",
        if new == 6735699 { "OK" } else { "MISMATCH" }, (dt - t_drops) * 1e9 / qs.len() as f64, hp_tab, hp());
    // UNTIMED: the per-lookup decision fingerprint (as bits::run_bits prints it).
    let mut seen = vec![false; n_door + new as usize];
    let mut fp = 0u64;
    for e in &edges { let is_new = e.1 as usize >= n_door && !seen[e.1 as usize]; seen[e.1 as usize] = true; fp = fp.wrapping_mul(0x100_0000_01b3) ^ (is_new as u64); }
    eprintln!("[{tag}] decision fingerprint {fp:016x}");
}
