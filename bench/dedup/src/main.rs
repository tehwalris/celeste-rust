//! Standalone microbenchmark of one forward frame's dedup / frontier / edge
//! pipeline, replayed from a capture (`CELESTE_EMIT_CAPTURE`, see
//! src/capture.rs in the main crate):
//!   for each emission (source, successor key): visited? -> nothing new;
//!   else insert, new id, append to the next frontier; record the edge.
//! Usage: dedup-bench CAPTURE_DIR [variant]

use std::sync::atomic::{AtomicU32, AtomicU64, Ordering};
use std::sync::Mutex;
use std::time::Instant;

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

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let dir = &args[1];
    let variant = args.get(2).map(|s| s.as_str()).unwrap_or("v0");
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
