//! `edgecensus`: do the frame's edges bundle like the states under posmask?
//! Bundle (source side) = (source shape + 8x8 region + packed key, transfer,
//! target shape + packed key, shift = target cell - source cell); its members
//! = the source cells taking it. Target side: (target shape + region + key,
//! source shape + key, transfer, shift), members = target cells.
//! Edges = the capture's non-dropped emissions (257.7M, all distinct).

use crate::bits::{packing, Fields};
use crate::structure::cell_xy;
use rustc_hash::{FxHashMap, FxHashSet};

fn varint(o: &mut Vec<u8>, mut v: u64) { while v >= 0x80 { o.push(v as u8 | 0x80); v >>= 7; } o.push(v as u8); }

fn quant(mut v: Vec<u64>, w: bool) -> String {
    v.sort_unstable();
    let n = v.len(); let tot: u64 = v.iter().sum();
    let q = |p: f64| v[((n - 1) as f64 * p) as usize];
    let wq = |p: f64| { let t = (tot as f64 * p) as u64; let mut a = 0; for &x in &v { a += x; if a >= t { return x; } } v[n - 1] };
    format!("{n} groups, mean {:.2}, median {}, p90 {}, max {}{}; single-member {:.1}% of groups ({:.1}% of edges)", tot as f64 / n as f64, q(0.5), q(0.9), v[n - 1],
        if w { format!(", edge-weighted median {} p90 {}", wq(0.5), wq(0.9)) } else { String::new() },
        100.0 * v.iter().filter(|&&x| x == 1).count() as f64 / n as f64, 100.0 * v.iter().filter(|&&x| x == 1).count() as f64 / tot as f64)
}

pub fn edgecensus(dir: &str, door: &[u8], maps: &[memmap2::Mmap]) {
    let t = std::time::Instant::now();
    let f = Fields::open(dir);
    let mut by_shape: Vec<Vec<usize>> = vec![Vec::new(); f.shapes.len()];
    for i in 0..f.n { by_shape[f.hdr(i).0].push(i); }
    let packs: Vec<_> = (0..f.shapes.len()).map(|s| packing(&f, s, &by_shape[s])).collect();
    drop(by_shape);
    // Per row: shape, cell, packed key (high * low radix + low), 8x8 region id, entry id in the region.
    let mut row_of: FxHashMap<u128, u32> = FxHashMap::default();
    row_of.reserve(f.n);
    let mut shape = vec![0u8; f.n]; let mut cell = vec![0u32; f.n]; let mut key = vec![0u32; f.n]; let mut reg = vec![0u32; f.n];
    let mut rid: FxHashMap<(u8, i32, i32), u32> = FxHashMap::default();
    for i in 0..f.n {
        let (s, c, _) = f.hdr(i);
        let (h, l) = packs[s].pack(&f, i);
        let lr = packs[s].low.map_or(1, |d| packs[s].digits[d].radix);
        shape[i] = s as u8; cell[i] = c; key[i] = u32::try_from(h as u64 * lr + l as u64).unwrap();
        let (x, y) = cell_xy(c);
        let n = rid.len() as u32;
        reg[i] = *rid.entry((s as u8, x.div_euclid(8), y.div_euclid(8))).or_insert(n);
        row_of.insert(f.key(i), i as u32);
    }
    // Entries (region, key), numbered canonically: by region, then key. Global entry id.
    let mut ents: Vec<(u32, u32)> = (0..f.n).map(|i| (reg[i], key[i])).collect();
    ents.sort_unstable(); ents.dedup();
    let entry_of = |r: u32, k: u32| ents.binary_search(&(r, k)).unwrap() as u32;
    let n_regions = rid.len();
    println!("edgecensus: {} states, {} 8x8 regions, {} entries; {:.1} s", f.n, n_regions, ents.len(), t.elapsed().as_secs_f64());
    // Sources: src id -> row via the door.
    let mut src_row: FxHashMap<u64, u32> = FxHashMap::default();
    for i in 0..door.len() / 40 {
        let b = &door[i * 40..];
        let k = (u64::from_le_bytes(b[16..24].try_into().unwrap()) as u128) | ((u64::from_le_bytes(b[24..32].try_into().unwrap()) as u128) << 64);
        src_row.insert(u64::from_le_bytes(b[32..40].try_into().unwrap()), row_of[&k]);
    }
    let mut edges: Vec<(u32, u32, u32)> = Vec::with_capacity(258_000_000);
    let mut xfers: FxHashSet<u32> = Default::default();
    for m in maps { for c in m.chunks_exact(48) {
        let r = crate::rec(c);
        if r.flags != 0 { continue; }
        let s = src_row[&r.src]; let tg = row_of[&r.key];
        debug_assert_eq!(cell[tg as usize], r.cell);
        xfers.insert(r.xfer);
        edges.push((s, tg, r.xfer));
    } }
    drop(row_of); drop(src_row);
    let xmax = *xfers.iter().max().unwrap();
    println!("  {} edges, {} distinct transfers (max id {xmax}); {:.1} s", edges.len(), xfers.len(), t.elapsed().as_secs_f64());
    let ne = edges.len() as f64;
    let sh = |s: u32, t: u32| { let (a, b) = cell_xy(cell[s as usize]); let (c, d) = cell_xy(cell[t as usize]); (c - a, d - b) };
    assert!(xmax < 1 << 24);
    // SOURCE-side bundles: key = (src region 12b, src key 31b, xfer 24b, tgt shape 3b, tgt key 31b, dx 6b, dy 6b) = 113 bits.
    let bkey = |s: u32, tg: u32, x: u32, with_x: bool| -> u128 {
        let (dx, dy) = sh(s, tg);
        assert!(dx.abs() < 512 && dy.abs() < 512);
        let mut k = reg[s as usize] as u128;
        k = k << 31 | key[s as usize] as u128;
        k = k << 24 | if with_x { x as u128 } else { 0 };
        k = k << 3 | shape[tg as usize] as u128;
        k = k << 31 | key[tg as usize] as u128;
        k = k << 10 | (dx + 512) as u128;
        k << 10 | (dy + 512) as u128
    };
    let far = edges.iter().filter(|&&(s, tg, _)| { let (dx, dy) = sh(s, tg); dx.abs() > 8 || dy.abs() > 8 }).count();
    let shapechg = edges.iter().filter(|&&(s, tg, _)| shape[s as usize] != shape[tg as usize]).count();
    println!("  edges with |shift| > 8 px: {far}; changing shape: {shapechg}");
    let mut v: Vec<(u128, u32)> = edges.iter().enumerate().map(|(i, &(s, tg, x))| (bkey(s, tg, x, true), i as u32)).collect();
    v.sort_unstable();
    let mut sizes = Vec::new(); let mut spans = [0u64; 5];
    let mut bund_raw = 0u64; let mut stream: Vec<(u32, Vec<u8>)> = Vec::new(); // (first target region, bytes)
    let mut i = 0;
    while i < v.len() {
        let mut j = i; while j < v.len() && v[j].0 == v[i].0 { j += 1; }
        sizes.push((j - i) as u64);
        let (s0, t0, x0) = edges[v[i].1 as usize];
        let mut tregs: Vec<u32> = v[i..j].iter().map(|e| { let (_, tg, _) = edges[e.1 as usize]; reg[tg as usize] }).collect();
        tregs.sort_unstable(); tregs.dedup();
        spans[tregs.len().min(4)] += 1;
        // Bundle record: src entry id, one tgt entry id a spanned region, shift, xfer, the u64 source-cell mask.
        let mut b = Vec::new();
        let (dx, dy) = sh(s0, t0);
        b.extend_from_slice(&entry_of(reg[s0 as usize], key[s0 as usize]).to_le_bytes());
        for &tr in &tregs { b.extend_from_slice(&entry_of(tr, key[t0 as usize]).to_le_bytes()); }
        if dx.abs() <= 7 && dy.abs() <= 7 { b.push(((dx + 8) as u8) << 4 | (dy + 8) as u8 & 15); } else { b.push(0); b.extend_from_slice(&(dx as i16).to_le_bytes()); b.extend_from_slice(&(dy as i16).to_le_bytes()); }
        varint(&mut b, x0 as u64);
        let mut mask = 0u64;
        for e in &v[i..j] { let (s, _, _) = edges[e.1 as usize]; let (x, y) = cell_xy(cell[s as usize]); mask |= 1 << (x.rem_euclid(8) + 8 * y.rem_euclid(8)); }
        b.extend_from_slice(&mask.to_le_bytes());
        bund_raw += b.len() as u64;
        stream.push((tregs[0], b));
        i = j;
    }
    println!("  SOURCE-side bundles: {} for {} edges ({:.2} edges a bundle)", sizes.len(), edges.len(), ne / sizes.len() as f64);
    println!("    cells a bundle: {}", quant(sizes, true));
    println!("    target 8x8 regions a bundle spans: 1: {} 2: {} 3: {} 4: {}", spans[1], spans[2], spans[3], spans[4]);
    stream.sort_by_key(|x| x.0);
    let bytes: Vec<u8> = stream.into_iter().flat_map(|x| x.1).collect();
    let bz = zstd::bulk::compress(&bytes, 1).unwrap().len();
    println!("    bundle records (src entry u32, tgt entry u32 a spanned region, shift 1 B, xfer varint, u64 mask), sorted by target region: raw {:.2} B an edge, zstd -1 {:.2} B an edge", bund_raw as f64 / ne, bz as f64 / ne);
    drop(bytes);
    // Transfer splits: the same bundle without the transfer.
    let mut nx: Vec<u128> = v.iter().map(|e| e.0 & !(((1u128 << 24) - 1) << 54)).collect();
    let n_with = { let mut c = 0; let mut p = None; for e in &v { if p != Some(e.0) { c += 1; p = Some(e.0); } } c };
    nx.sort_unstable(); nx.dedup();
    println!("    without the transfer in the key: {} bundles ({} more with it: the transfer splits {:.2}% of them)", nx.len(), n_with - nx.len(), 100.0 * (n_with - nx.len()) as f64 / nx.len() as f64);
    drop(nx);
    // TARGET-side: (tgt region, tgt key, src shape, src key, xfer, shift) -> target cells.
    let tkey = |s: u32, tg: u32, x: u32| -> u128 {
        let (dx, dy) = sh(s, tg);
        let mut k = reg[tg as usize] as u128;
        k = k << 31 | key[tg as usize] as u128;
        k = k << 24 | x as u128;
        k = k << 3 | shape[s as usize] as u128;
        k = k << 31 | key[s as usize] as u128;
        k = k << 10 | (dx + 512) as u128;
        k << 10 | (dy + 512) as u128
    };
    for (e, slot) in edges.iter().zip(v.iter_mut()) { *slot = (tkey(e.0, e.1, e.2), 0); }
    v.sort_unstable();
    let mut tsizes = Vec::new();
    let mut i = 0;
    while i < v.len() { let mut j = i; while j < v.len() && v[j].0 == v[i].0 { j += 1; } tsizes.push((j - i) as u64); i = j; }
    println!("  TARGET-side bundles: {} ({:.2} edges a bundle)", tsizes.len(), ne / tsizes.len() as f64);
    println!("    cells a bundle: {}", quant(tsizes, true));
    drop(v);
    // (a) flat edges with stable ids, per target region: sorted (tgt region, tgt local id, src global id).
    let mut fl: Vec<(u32, u32, u32, u32)> = edges.iter().map(|&(s, tg, x)| {
        let (tx, ty) = cell_xy(cell[tg as usize]); let (sx, sy) = cell_xy(cell[s as usize]);
        let tl = (entry_of(reg[tg as usize], key[tg as usize]) - ents.partition_point(|e| e.0 < reg[tg as usize]) as u32) << 6 | (tx.rem_euclid(8) + 8 * ty.rem_euclid(8)) as u32;
        let sg = entry_of(reg[s as usize], key[s as usize]) << 6 | (sx.rem_euclid(8) + 8 * sy.rem_euclid(8)) as u32;
        (reg[tg as usize], tl, sg, x)
    }).collect();
    fl.sort_unstable();
    let mut o = Vec::with_capacity(fl.len() * 6);
    let mut prev: Option<(u32, u32, u32)> = None;
    for &(r, tl, sg, x) in &fl {
        match prev {
            Some((pr, pt, ps)) if pr == r => { varint(&mut o, (tl - pt) as u64); if tl == pt { varint(&mut o, (sg - ps) as u64); } else { varint(&mut o, sg as u64); } }
            _ => { varint(&mut o, r as u64); varint(&mut o, tl as u64); varint(&mut o, sg as u64); }
        }
        varint(&mut o, x as u64);
        prev = Some((r, tl, sg));
    }
    let oz = zstd::bulk::compress(&o, 1).unwrap().len();
    println!("  (a) flat edges, stable ids (region, entry, cell), per target region: varint (tgt delta, src delta or id, xfer) {:.2} B an edge, zstd -1 {:.2} B an edge", o.len() as f64 / ne, oz as f64 / ne);
    println!("  done {:.1} s", t.elapsed().as_secs_f64());
}
