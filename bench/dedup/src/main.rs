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
