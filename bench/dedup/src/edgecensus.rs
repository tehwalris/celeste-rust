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

/// The edges with every state's packed form (shared by edgecensus and edgepairs).
pub struct Loaded { pub f: Fields, pub packs: Vec<crate::bits::Packing>, pub shape: Vec<u8>, pub cell: Vec<u32>, pub key: Vec<u32>, pub reg: Vec<u32>, pub ents: Vec<(u32, u32)>,
    pub edges: Vec<(u32, u32, u32)>, pub xfers: FxHashSet<u32>, pub pairs: Vec<[u8; 26]>, pub canon: bool, pub n_regions: usize, pub t: std::time::Instant }

pub fn load(dir: &str, cap: &str, door: &[u8], maps: &[memmap2::Mmap]) -> Loaded {
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
    // Transfers canonical by CONTENT: each worker's table (x{n}.bin, 26-B pairs) interned globally.
    let canon = std::path::Path::new(&format!("{cap}/x000.bin")).exists();
    let mut pairs: Vec<[u8; 26]> = Vec::new();
    let mut intern: FxHashMap<[u8; 26], u32> = FxHashMap::default();
    let mut fp = 0u64;
    for (n, m) in maps.iter().enumerate() {
        let local: Vec<u32> = if canon {
            let b = std::fs::read(format!("{cap}/x{n:03}.bin")).unwrap();
            b.chunks_exact(26).map(|c| { let a: [u8; 26] = c.try_into().unwrap(); let k = pairs.len() as u32; *intern.entry(a).or_insert_with(|| { pairs.push(a); k }) }).collect()
        } else { Vec::new() };
        for c in m.chunks_exact(48) {
            let r = crate::rec(c);
            if r.flags == 2 { continue; }
            fp = fp.wrapping_add(crate::bits::mix64(r.src ^ (r.key as u64).rotate_left(17) ^ ((r.key >> 64) as u64).rotate_left(31) ^ (r.cell as u64) << 40 ^ r.flags as u64));
            if r.flags != 0 { continue; }
            let s = src_row[&r.src]; let tg = row_of[&r.key];
            let x = if canon { local[r.xfer as usize] } else { r.xfer };
            xfers.insert(x);
            edges.push((s, tg, x));
        }
    }
    println!("  emissions multiset fingerprint {fp:016x}; transfers {}", if canon { "canonical by content (per-worker tables)" } else { "WORKER-LOCAL ids" });
    if std::env::var_os("EDGE_FP_ONLY").is_some() { std::process::exit(0); }
    drop(row_of); drop(src_row);
    Loaded { f, packs, shape, cell, key, reg, ents, edges, xfers, pairs, canon, n_regions, t }
}

pub fn edgecensus(dir: &str, cap: &str, door: &[u8], maps: &[memmap2::Mmap]) {
    let Loaded { shape, cell, key, reg, ents, edges, xfers, pairs, canon, t, .. } = load(dir, cap, door, maps);
    let entry_of = |r: u32, k: u32| ents.binary_search(&(r, k)).unwrap() as u32;
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
    if canon {
        // Bundles that split by transfer: how do the transfers differ? Per axis: guard (lo, hi) or action (tag, val).
        let mut w: Vec<(u128, u32, u32)> = v.iter().map(|e| { let (s, _, x) = edges[e.1 as usize]; (e.0 & !(((1u128 << 24) - 1) << 54), x, cell[s as usize]) }).collect();
        w.sort_unstable();
        let dec = |x: u32| { let b = &pairs[x as usize]; let ax = |o: usize| (u32::from_le_bytes(b[o..o + 4].try_into().unwrap()), u32::from_le_bytes(b[o + 4..o + 8].try_into().unwrap()), b[o + 8], i32::from_le_bytes(b[o + 9..o + 13].try_into().unwrap())); (ax(0), ax(13)) };
        let mut classes: FxHashMap<String, u64> = Default::default();
        let (mut split_groups, mut shown) = (0u64, 0);
        let mut i = 0;
        while i < w.len() {
            let mut j = i; while j < w.len() && w[j].0 == w[i].0 { j += 1; }
            let mut xs: Vec<u32> = w[i..j].iter().map(|e| e.1).collect(); xs.dedup();
            if xs.len() > 1 {
                split_groups += 1;
                // Does the transfer vary ACROSS cells, or only within one source state (guard pieces)?
                let mut per_cell: Vec<(u32, u32)> = w[i..j].iter().map(|e| (e.2, e.1)).collect(); per_cell.sort_unstable();
                let mut cells_x: FxHashMap<u32, Vec<u32>> = FxHashMap::default();
                for (c, x) in per_cell { cells_x.entry(c).or_default().push(x); }
                let sets: FxHashSet<Vec<u32>> = cells_x.values().cloned().collect();
                if cells_x.len() == 1 { *classes.entry("ONE source cell only".into()).or_default() += 1; }
                else if sets.len() == 1 { *classes.entry("cells share the same transfer SET (guard pieces)".into()).or_default() += 1; }
                else { *classes.entry("transfer set differs ACROSS cells".into()).or_default() += 1; }
                let (a, b) = (dec(xs[0]), dec(xs[1]));
                let what = |p: (u32, u32, u8, i32), q: (u32, u32, u8, i32)| match ((p.0, p.1) != (q.0, q.1), (p.2, p.3) != (q.2, q.3)) { (false, false) => "same", (true, false) => "guard", (false, true) => if p.2 != q.2 { "action-kind" } else { "action-val" }, (true, true) => if p.2 != q.2 { "guard+action-kind" } else { "guard+action-val" } };
                *classes.entry(format!("x {} / y {}", what(a.0, b.0), what(a.1, b.1))).or_default() += 1;
                if shown < 6 && j - i >= 3 {
                    shown += 1;
                    println!("    example split bundle ({} edges, {} transfers):", j - i, xs.len());
                    for e in &w[i..j] { let (cx, cy) = cell_xy(e.2); let p = dec(e.1); println!("      src cell ({cx},{cy}) xfer x guard [{}, {}) {} {} | y guard [{}, {}) {} {}", p.0.0, p.0.1, if p.0.2 == 0 { "rot" } else { "const" }, p.0.3, p.1.0, p.1.1, if p.1.2 == 0 { "rot" } else { "const" }, p.1.3); }
                }
            }
            i = j;
        }
        let mut cl: Vec<_> = classes.into_iter().collect(); cl.sort_by_key(|x| std::cmp::Reverse(x.1));
        println!("    groups split by the transfer: {split_groups}; how the first two transfers differ: {cl:?}");
    }
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

/// `edgepairs`: the edges between ONE (source 8x8 region, target 8x8 region)
/// pair at a time, bundled / compressed only within that set. Fields of an
/// edge inside its pair: source entry (rank of its key in the source region),
/// source cell (0..64), target entry, target cell, transfer (content-canonical).
pub fn edgepairs(dir: &str, cap: &str, door: &[u8], maps: &[memmap2::Mmap]) {
    let ld = load(dir, cap, door, maps);
    assert!(ld.canon, "edgepairs needs the per-worker transfer tables (x*.bin)");
    let (shape, cell, key, reg, ents, edges) = (&ld.shape, &ld.cell, &ld.key, &ld.reg, &ld.ents, &ld.edges);
    let rstart: Vec<u32> = (0..=ld.n_regions as u32).map(|r| ents.partition_point(|e| e.0 < r) as u32).collect();
    let local = |i: u32| { let r = reg[i as usize]; ents.binary_search(&(r, key[i as usize])).unwrap() as u32 - rstart[r as usize] };
    let pos = |i: u32| { let (x, y) = cell_xy(cell[i as usize]); (x.rem_euclid(8) + 8 * y.rem_euclid(8)) as u32 };
    // Per edge: [s_reg, t_reg, s_ent, s_pos, t_ent, t_pos, x].
    let rows: Vec<[u32; 7]> = edges.iter().map(|&(s, t, x)| [reg[s as usize], reg[t as usize], local(s), pos(s), local(t), pos(t), x]).collect();
    let ne = rows.len() as f64;
    let maxf = |k: usize| rows.iter().map(|r| r[k]).max().unwrap();
    let mx = [maxf(0), maxf(1), maxf(2), maxf(4), maxf(6)];
    assert!(mx[0] < 1 << 10 && mx[1] < 1 << 10 && mx[2] < 1 << 20 && mx[3] < 1 << 20 && mx[4] < 1 << 20, "field maxima {mx:?}");
    println!("edgepairs: {} edges; max entries a region {}; {:.1} s", rows.len(), (0..ld.n_regions).map(|r| rstart[r + 1] - rstart[r]).max().unwrap(), ld.t.elapsed().as_secs_f64());
    // Pair stats.
    let mut pc: FxHashMap<(u32, u32), u64> = FxHashMap::default();
    for r in &rows { *pc.entry((r[0], r[1])).or_default() += 1; }
    let mut per_src: FxHashMap<u32, u64> = FxHashMap::default();
    for &(s, _) in pc.keys() { *per_src.entry(s).or_default() += 1; }
    println!("  region pairs: {}; edges a pair: {}", pc.len(), quant(pc.values().copied().collect(), true));
    println!("  target regions a source region reaches: {}", quant(per_src.values().copied().collect(), false));
    let widths = [20u32, 6, 20, 6, 20]; // s_ent, s_pos, t_ent, t_pos, x
    let names = ["src entry", "src cell", "tgt entry", "tgt cell", "transfer"];
    let sort_key = |r: &[u32; 7], order: &[usize]| -> u128 { let mut k = (r[0] as u128) << 10 | r[1] as u128; for &f in order { k = k << widths[f] | r[2 + f] as u128; } k };
    let orders: Vec<(&str, Vec<usize>)> = vec![
        ("src entry > tgt entry > transfer > src cell > tgt cell", vec![0, 2, 4, 1, 3]),
        ("transfer > src entry > tgt entry > src cell > tgt cell", vec![4, 0, 2, 1, 3]),
        ("src entry > src cell > tgt entry > tgt cell > transfer (flat)", vec![0, 1, 2, 3, 4]),
        ("tgt entry > src entry > transfer > src cell > tgt cell", vec![2, 0, 4, 1, 3]),
    ];
    let mut idx: Vec<(u128, u32)> = Vec::with_capacity(rows.len());
    let mut best: (f64, Vec<usize>) = (f64::MAX, vec![]);
    for (name, order) in &orders {
        idx.clear(); idx.extend(rows.iter().enumerate().map(|(i, r)| (sort_key(r, order), i as u32)));
        idx.sort_unstable();
        // DFS first-changed varint; LOUDS estimate; flat entropy bound; all per pair.
        let (mut bytes, mut louds, mut bound) = (0u64, 0f64, 0f64);
        let mut o = Vec::new();
        let mut i = 0;
        while i < idx.len() {
            let r0 = rows[idx[i].1 as usize];
            let mut j = i; while j < idx.len() && rows[idx[j].1 as usize][..2] == r0[..2] { j += 1; }
            o.clear(); varint(&mut o, r0[1] as u64); varint(&mut o, (j - i) as u64);
            let mut nodes = [0u64; 5]; let mut alpha = [0u64; 5];
            let mut xs: FxHashSet<u32> = Default::default();
            for (t, e) in idx[i..j].iter().enumerate() {
                let r = &rows[e.1 as usize];
                xs.insert(r[6]);
                if t == 0 { for &f in order.iter() { varint(&mut o, r[2 + f] as u64); } for n in nodes.iter_mut() { *n += 1; } continue; }
                let p = &rows[idx[i + t - 1].1 as usize];
                let d = order.iter().position(|&f| r[2 + f] != p[2 + f]).unwrap();
                o.push(d as u8); varint(&mut o, (r[2 + order[d]] - p[2 + order[d]]) as u64);
                for &f in &order[d + 1..] { varint(&mut o, r[2 + f] as u64); }
                for n in nodes[d..].iter_mut() { *n += 1; }
            }
            let (sr, tr) = (r0[0] as usize, r0[1] as usize);
            for (lv, &f) in order.iter().enumerate() { alpha[lv] = match f { 0 => (rstart[sr + 1] - rstart[sr]) as u64, 2 => (rstart[tr + 1] - rstart[tr]) as u64, 4 => xs.len() as u64, _ => 64 }; }
            louds += (0..5).map(|lv| nodes[lv] as f64 * (2.0 + (alpha[lv].max(1) as f64).log2().ceil())).sum::<f64>();
            let u: f64 = alpha.iter().map(|&a| a as f64).product(); let n = (j - i) as f64;
            bound += if n * 2.0 > u { u } else { n * (u / n).log2() + n * std::f64::consts::LOG2_E };
            bytes += o.len() as u64;
            i = j;
        }
        println!("  order {name}: DFS first-changed varint {:.2} B an edge; LOUDS trie ~{:.2} B; flat bound log2 C(product, n) {:.2} B", bytes as f64 / ne, louds / 8.0 / ne, bound / 8.0 / ne);
        if (bytes as f64) < best.0 { best = (bytes as f64, order.clone()); }
    }
    // (c) the relation: per (src entry, tgt entry, transfer), the (src cell, tgt cell) pairs:
    // a uniform shift + a u64 source mask (or a short list), else an explicit list. zstd per pair block.
    let order = vec![0usize, 2, 4, 1, 3];
    idx.clear(); idx.extend(rows.iter().enumerate().map(|(i, r)| (sort_key(r, &order), i as u32)));
    idx.sort_unstable();
    let (mut rel, mut z1, mut z3, mut groups, mut uniform) = (0u64, 0u64, 0u64, 0u64, 0u64);
    let mut gsize = Vec::new();
    let mut o = Vec::new();
    let mut i = 0;
    while i < idx.len() {
        let r0 = rows[idx[i].1 as usize];
        let mut j = i; while j < idx.len() && rows[idx[j].1 as usize][..2] == r0[..2] { j += 1; }
        o.clear(); varint(&mut o, r0[1] as u64);
        let mut prev: Option<[u32; 3]> = None;
        let mut a = i;
        while a < j {
            let ra = rows[idx[a].1 as usize];
            let g = [ra[2], ra[4], ra[6]];
            let mut b = a; while b < j && { let r = rows[idx[b].1 as usize]; [r[2], r[4], r[6]] == g } { b += 1; }
            groups += 1; gsize.push((b - a) as u64);
            // header: first changed of (src entry, tgt entry, transfer) + delta + rest
            match prev { None => { o.push(255); for v in g { varint(&mut o, v as u64); } }
                Some(p) => { let d = (0..3).find(|&k| g[k] != p[k]).unwrap(); o.push(d as u8); varint(&mut o, (g[d] - p[d]) as u64); for v in &g[d + 1..] { varint(&mut o, *v as u64); } } }
            prev = Some(g);
            // members: shift in cell units between the two regions' cells (relative; uniform iff t_pos - s_pos is constant as a 2-D vector).
            let mem: Vec<(i32, i32, i32, i32)> = idx[a..b].iter().map(|e| { let r = rows[e.1 as usize]; ((r[3] % 8) as i32, (r[3] / 8) as i32, (r[5] % 8) as i32, (r[5] / 8) as i32) }).collect();
            let sh0 = (mem[0].2 - mem[0].0, mem[0].3 - mem[0].1);
            let uni = mem.iter().all(|m| (m.2 - m.0, m.3 - m.1) == sh0);
            varint(&mut o, (b - a) as u64);
            if uni {
                uniform += 1;
                o.push(((sh0.0 + 8) as u8) << 4 | (sh0.1 + 8) as u8);
                if b - a >= 8 { let mut m = 0u64; for x in &mem { m |= 1 << (x.0 + 8 * x.1); } o.extend_from_slice(&m.to_le_bytes()); }
                else { for x in &mem { o.push((x.0 + 8 * x.1) as u8); } }
            } else { for x in &mem { o.push((x.0 + 8 * x.1) as u8); o.push((x.2 + 8 * x.3) as u8); } }
            a = b;
        }
        rel += o.len() as u64;
        z1 += zstd::bulk::compress(&o, 1).unwrap().len() as u64;
        z3 += zstd::bulk::compress(&o, 3).unwrap().len() as u64;
        i = j;
    }
    println!("  (c) relation (src entry, tgt entry, transfer) -> cell pairs: {groups} groups ({:.2} edges each; {}), {:.1}% with a uniform shift; {:.2} B an edge raw, zstd -1 per pair block {:.2}, zstd -3 {:.2}",
        ne / groups as f64, quant(gsize, true), 100.0 * uniform as f64 / groups as f64, rel as f64 / ne, z1 as f64 / ne, z3 as f64 / ne);
    // zstd on the best DFS order too.
    idx.clear(); idx.extend(rows.iter().enumerate().map(|(i, r)| (sort_key(r, &best.1), i as u32)));
    idx.sort_unstable();
    let (mut d1, mut d3) = (0u64, 0u64);
    let mut i = 0;
    while i < idx.len() {
        let r0 = rows[idx[i].1 as usize];
        let mut j = i; while j < idx.len() && rows[idx[j].1 as usize][..2] == r0[..2] { j += 1; }
        o.clear();
        for (t, e) in idx[i..j].iter().enumerate() {
            let r = &rows[e.1 as usize];
            if t == 0 { for &f in &best.1 { varint(&mut o, r[2 + f] as u64); } continue; }
            let p = &rows[idx[i + t - 1].1 as usize];
            let d = best.1.iter().position(|&f| r[2 + f] != p[2 + f]).unwrap();
            o.push(d as u8); varint(&mut o, (r[2 + best.1[d]] - p[2 + best.1[d]]) as u64);
            for &f in &best.1[d + 1..] { varint(&mut o, r[2 + f] as u64); }
        }
        d1 += zstd::bulk::compress(&o, 1).unwrap().len() as u64; d3 += zstd::bulk::compress(&o, 3).unwrap().len() as u64;
        i = j;
    }
    println!("  (d) best DFS order ({}) + zstd per pair block: -1 {:.2} B, -3 {:.2} B an edge", best.1.iter().map(|&f| names[f]).collect::<Vec<_>>().join(" > "), d1 as f64 / ne, d3 as f64 / ne);
    // Readable dumps of 3 pairs: small, weighted-median, heaviest.
    let mut pv: Vec<((u32, u32), u64)> = pc.into_iter().collect(); pv.sort_by_key(|x| x.1);
    let tot: u64 = pv.iter().map(|x| x.1).sum();
    let wmed = { let mut a = 0; pv.iter().find(|x| { a += x.1; a >= tot / 2 }).unwrap().0 };
    let picks = [("small", pv[pv.len() / 4].0), ("weighted-median", wmed), ("heaviest", pv.last().unwrap().0)];
    std::fs::create_dir_all("/var/tmp/emitcap/edgepairs").unwrap();
    let f = &ld.f;
    let show_key = |s: usize, k: u32| -> String {
        let p = &ld.packs[s];
        let lr = p.low.map_or(1, |d| p.digits[d].radix) as u32;
        let (mut h, l) = (k / lr, k % lr);
        let mut parts = Vec::new();
        for &d in p.high.iter().rev() { let r = p.digits[d].radix as u32; parts.push((d, h % r)); h /= r; }
        parts.reverse();
        let mut out: Vec<String> = parts.iter().map(|&(d, v)| { let dg = &p.digits[d]; if dg.table.is_some() { format!("dash#{v}") } else { let fl = &f.shapes[s][dg.fields[0]]; format!("{}={}", dg.name, fl.shows[v as usize]) } }).collect();
        if let Some(d) = p.low { out.push(format!("spd.y={}", f.shapes[s][p.digits[d].fields[0]].shows[l as usize])); }
        out.join(" ")
    };
    let dec = |x: u32| { let b = &ld.pairs[x as usize]; let ax = |o: usize| format!("[{},{}) {} {}", u32::from_le_bytes(b[o..o + 4].try_into().unwrap()), u32::from_le_bytes(b[o + 4..o + 8].try_into().unwrap()), if b[o + 8] == 0 { "rot" } else { "const" }, i32::from_le_bytes(b[o + 9..o + 13].try_into().unwrap())); format!("x {} | y {}", ax(0), ax(13)) };
    for (label, (sr, tr)) in picks {
        let mut es: Vec<&(u32, u32, u32)> = edges.iter().zip(&rows).filter(|(_, r)| r[0] == sr && r[1] == tr).map(|(e, _)| e).collect();
        es.sort_by_key(|&&(s, t, x)| (local(s), local(t), x, pos(s), pos(t)));
        let path = format!("/var/tmp/emitcap/edgepairs/{label}_{sr}_{tr}.txt");
        let mut w = std::io::BufWriter::new(std::fs::File::create(&path).unwrap());
        use std::io::Write;
        writeln!(w, "# pair src region {sr} -> tgt region {tr} ({label}): {} edges; sorted src entry > tgt entry > transfer > src cell > tgt cell", es.len()).unwrap();
        for &&(s, t, x) in &es {
            let (sx, sy) = cell_xy(cell[s as usize]); let (tx, ty) = cell_xy(cell[t as usize]);
            writeln!(w, "src [{}] ({sx},{sy}) -> tgt [{}] ({tx},{ty}) shift ({},{}) xfer {}", show_key(shape[s as usize] as usize, key[s as usize]), show_key(shape[t as usize] as usize, key[t as usize]), tx - sx, ty - sy, dec(x)).unwrap();
        }
        println!("  dumped {label} pair ({} edges) -> {path}", es.len());
    }
}

/// `edgeregions R`: Philippe's SOURCE-side layout (DESIGNS N). Each source
/// region R (R x R, aligned) keeps ONE set: its own states plus GHOSTS - the
/// targets of its edges that live outside R, keyed alike, positioned relative
/// to R's corner (a halo around the core). Edges are (local src, local tgt,
/// transfer); ghosts are translated to their owner's stable id once each.
pub fn edgeregions(dir: &str, cap: &str, door: &[u8], maps: &[memmap2::Mmap], rr: i32) {
    let ld = load(dir, cap, door, maps);
    let (cell, key, edges) = (&ld.cell, &ld.key, &ld.edges);
    let n = ld.f.n;
    let frame: Vec<u8> = (0..n).map(|i| ld.f.hdr(i).2 as u8).collect();
    let xy: Vec<(i32, i32)> = cell.iter().map(|&c| cell_xy(c)).collect();
    // Regions: (shape, x div R, y div R).
    let mut rid: FxHashMap<(u8, i32, i32), u32> = FxHashMap::default();
    let mut corner: Vec<(i32, i32)> = Vec::new();
    let reg: Vec<u32> = (0..n).map(|i| { let (x, y) = xy[i]; let k = (ld.shape[i], x.div_euclid(rr), y.div_euclid(rr)); let m = rid.len() as u32; *rid.entry(k).or_insert_with(|| { corner.push((k.1 * rr, k.2 * rr)); m }) }).collect();
    let ng = rid.len();
    let mut rshape = vec![0u8; ng]; for (&(sh, _, _), &r) in &rid { rshape[r as usize] = sh; }
    let ne = edges.len() as f64;
    // Distance of a cell outside region r's core (0 inside).
    let outside = |r: u32, (x, y): (i32, i32)| { let (cx, cy) = corner[r as usize]; let dx = if x < cx { cx - x } else if x >= cx + rr { x - cx - rr + 1 } else { 0 }; let dy = if y < cy { cy - y } else if y >= cy + rr { y - cy - rr + 1 } else { 0 }; dx.max(dy) };
    // Per edge: the target's distance outside the source's core; self-region edges.
    let mut dhist = vec![0u64; 600];
    let mut self_edges = 0u64;
    for &(s, t, _) in edges.iter() {
        let r = reg[s as usize];
        let d = if ld.shape[t as usize] != ld.shape[s as usize] { 599 } else { outside(r, xy[t as usize]).min(598) as usize };
        dhist[d] += 1;
        if reg[t as usize] == r { self_edges += 1; }
    }
    let cover = |p: f64| { let mut a = 0u64; for (d, &c) in dhist.iter().enumerate() { a += c; if a as f64 >= p * ne { return d as i64; } } -1 };
    println!("edgeregions R = {rr}x{rr}: {ng} regions, {} edges; self-region edges {self_edges} ({:.1}%)", edges.len(), 100.0 * self_edges as f64 / ne);
    println!("  target distance outside the source's core (edge-weighted): 0: {:.1}%, <=1: {:.1}%, <=2: {:.1}%, <=4: {:.1}%, <=8: {:.1}%; halo for 99% {} px, 99.9% {} px, 100% {} (599 = another shape)",
        100.0 * dhist[0] as f64 / ne, 100.0 * dhist[..2].iter().sum::<u64>() as f64 / ne, 100.0 * dhist[..3].iter().sum::<u64>() as f64 / ne, 100.0 * dhist[..5].iter().sum::<u64>() as f64 / ne, 100.0 * dhist[..9].iter().sum::<u64>() as f64 / ne, cover(0.99), cover(0.999), cover(1.0));
    // Distinct (R, target) pairs.
    let mut rt: Vec<u64> = edges.iter().map(|&(s, t, _)| (reg[s as usize] as u64) << 32 | t as u64).collect();
    rt.sort_unstable(); rt.dedup();
    let ghosts: Vec<(u32, u32)> = rt.iter().map(|&v| ((v >> 32) as u32, v as u32)).filter(|&(r, t)| reg[t as usize] != r).collect();
    let in_r_targets = rt.len() - ghosts.len();
    println!("  distinct (R, target) = dictionary entries / global dedup lookups if every target were translated: {} (vs 257.7M edges, 10.9M distinct targets, production's unit cache 31.7M); in-R targets {in_r_targets}, ghost states {}", rt.len(), ghosts.len());
    // Classes per region, two scopes: A = frontier (f56) + new (f57) states; B = the whole visited set.
    let mut is_tgt_in_r = vec![false; n]; // a state of R targeted by R's own edges
    for &v in &rt { let (r, t) = ((v >> 32) as u32, v as u32); if reg[t as usize] == r { is_tgt_in_r[t as usize] = true; } }
    let mut ghost_cnt = vec![0u64; ng]; for &(r, _) in &ghosts { ghost_cnt[r as usize] += 1; }
    for (scope, inscope) in [("A frontier+new", Box::new(|i: usize| frame[i] >= 56) as Box<dyn Fn(usize) -> bool>), ("B whole visited set", Box::new(|_: usize| true))] {
        let (mut a, mut b, mut b_out) = (vec![0u64; ng], vec![0u64; ng], 0u64);
        for i in 0..n { let r = reg[i] as usize; if inscope(i) { if is_tgt_in_r[i] { b[r] += 1; } else { a[r] += 1; } } else if is_tgt_in_r[i] { b_out += 1; } }
        let (ta, tb, tc): (u64, u64, u64) = (a.iter().sum(), b.iter().sum(), ghost_cnt.iter().sum());
        let tot = (ta + tb + tc) as f64;
        let mut frac_c: Vec<(f64, u64, u32)> = (0..ng).filter(|&r| a[r] + b[r] + ghost_cnt[r] > 0).map(|r| { let s = a[r] + b[r] + ghost_cnt[r]; (ghost_cnt[r] as f64 / s as f64, s, r as u32) }).collect();
        let fr = |k: usize| -> String { let mut v: Vec<f64> = (0..ng).filter(|&r| a[r] + b[r] + ghost_cnt[r] > 0).map(|r| { let s = (a[r] + b[r] + ghost_cnt[r]) as f64; [a[r], b[r], ghost_cnt[r]][k] as f64 / s }).collect(); v.sort_by(|x, y| x.partial_cmp(y).unwrap()); let q = |p: f64| v[((v.len() - 1) as f64 * p) as usize]; format!("p10 {:.2} median {:.2} p90 {:.2}", q(0.1), q(0.5), q(0.9)) };
        println!("  scope {scope}: (a) own only {ta} ({:.1}%), (b) own + target {tb} ({:.1}%), (c) ghost only {tc} ({:.1}%){}; per region (a) {}, (b) {}, (c) {}",
            100.0 * ta as f64 / tot, 100.0 * tb as f64 / tot, 100.0 * tc as f64 / tot, if b_out > 0 { format!(" [+{b_out} older states of R targeted, outside the scope]") } else { String::new() }, fr(0), fr(1), fr(2));
        frac_c.sort_by_key(|x| std::cmp::Reverse(x.1));
        println!("    heaviest regions (states in the set; a / b / c): {}", frac_c.iter().take(4).map(|&(_, s, r)| format!("r{r} {s}: {}/{}/{}", a[r as usize], b[r as usize], ghost_cnt[r as usize])).collect::<Vec<_>>().join(", "));
    }
    // Entries (posmask: key -> cell mask). Own entries: R's keys. Ghost entries with halo h: per (R, key) over ghosts within h; beyond h: overflow (R, key, cell).
    let mut own_e: Vec<u64> = (0..n).map(|i| (reg[i] as u64) << 32 | key[i] as u64).collect(); own_e.sort_unstable(); own_e.dedup();
    let own_entries = own_e.len() as u64;
    println!("  own entries (all {} states): {own_entries} ({:.1} states each)", n, n as f64 / own_entries as f64);
    for h in [0i32, 2, 4, 8] {
        let mut ge: Vec<u64> = Vec::new(); let mut over = 0u64;
        for &(r, t) in &ghosts { if ld.shape[t as usize] == rshape[r as usize] && outside(r, xy[t as usize]) <= h && h > 0 { ge.push((r as u64) << 32 | key[t as usize] as u64); } else { over += 1; } }
        ge.sort_unstable(); ge.dedup();
        let w = rr + 2 * h; let mask_b = ((w * w) as f64 / 8.0).ceil();
        let own_b = own_entries as f64 * (4.0 + mask_b);
        let ghost_b = ge.len() as f64 * (4.0 + mask_b) + over as f64 * 6.0;
        println!("  halo {h}: window {w}x{w} ({mask_b} B masks); ghost entries {} + overflow {over} (key + offset, 6 B); set bytes own {:.0} MB + ghosts {:.0} MB = {:.2} B per edge for the ghosts",
            ge.len(), own_b / 1e6, ghost_b / 1e6, ghost_b / ne);
    }
    // Translation: distinct (R, owner region, key) among ghosts - one global lookup each.
    let mut tr: Vec<(u32, u32, u32)> = ghosts.iter().map(|&(r, t)| (r, reg[t as usize], key[t as usize])).collect();
    tr.sort_unstable(); tr.dedup();
    println!("  translation entries (R, owner region, key): {} = the remaining global dedup lookups (one per ghost ENTRY), ~4 B each = {:.2} B per edge", tr.len(), tr.len() as f64 * 4.0 / ne);
    // Local-index edges per R: src local = (own entry rank, cell); tgt local = (entry rank in R's unified set, cell relative to the corner).
    // Encoded per R: (a) flat sorted varints, (b) bundles (src entry, tgt entry, xfer, shift) -> source-cell mask.
    let mut ord: Vec<u32> = (0..edges.len() as u32).collect();
    ord.sort_unstable_by_key(|&e| { let (s, _, _) = edges[e as usize]; reg[s as usize] });
    let (mut flat, mut fz, mut bund, mut bz, mut groups) = (0u64, 0u64, 0u64, 0u64, 0u64);
    let mut fixed_bits = 0f64;
    let mut i = 0;
    while i < ord.len() {
        let r = reg[edges[ord[i] as usize].0 as usize];
        let mut j = i; while j < ord.len() && reg[edges[ord[j] as usize].0 as usize] == r { j += 1; }
        let es: Vec<(u32, u32, u32)> = ord[i..j].iter().map(|&e| edges[e as usize]).collect();
        // unified keys of R: own + targets' keys, ranked
        let mut keys: Vec<u32> = es.iter().flat_map(|&(s, t, _)| [key[s as usize], key[t as usize]]).collect(); keys.sort_unstable(); keys.dedup();
        let kr = |k: u32| keys.binary_search(&k).unwrap() as u64;
        let (cx, cy) = corner[r as usize];
        let rel = |i: u32| { let (x, y) = xy[i as usize]; ((x - cx + 512) as u64) << 10 | (y - cy + 512) as u64 };
        let mut v: Vec<(u64, u64, u32)> = es.iter().map(|&(s, t, x)| (kr(key[s as usize]) << 20 | rel(s), kr(key[t as usize]) << 20 | rel(t), x)).collect();
        v.sort_unstable();
        let nset = (keys.len() * (rr * rr) as usize) as f64;
        fixed_bits += es.len() as f64 * (2.0 * nset.log2().ceil() + 17.0);
        let mut o = Vec::new(); let mut p = (u64::MAX, 0u64);
        for &(a, b, x) in &v { varint(&mut o, a.wrapping_sub(p.0)); if a != p.0 { p = (a, 0); } varint(&mut o, b.wrapping_sub(p.1)); p.1 = b; varint(&mut o, x as u64); }
        flat += o.len() as u64; fz += zstd::bulk::compress(&o, 1).unwrap().len() as u64;
        // bundles
        let mut g: Vec<(u64, u64, u32, u64, u64)> = v.iter().map(|&(a, b, x)| { let (sx, sy) = ((a >> 10 & 1023) as i64, (a & 1023) as i64); let (tx, ty) = ((b >> 10 & 1023) as i64, (b & 1023) as i64); (a >> 20, b >> 20, x, (((tx - sx) + 1024) << 12 | ((ty - sy) + 1024)) as u64, ((sx - 512) + rr as i64 * (sy - 512)) as u64) }).collect();
        g.sort_unstable();
        let mut o = Vec::new(); let mut k = 0; let mut pg = (u64::MAX, u64::MAX, u32::MAX, u64::MAX);
        while k < g.len() {
            let gk = (g[k].0, g[k].1, g[k].2, g[k].3);
            let mut l = k; let mut mask = 0u64; let mut cnt = 0u64; while l < g.len() && (g[l].0, g[l].1, g[l].2, g[l].3) == gk { mask |= 1u64.wrapping_shl(g[l].4 as u32 & 63); cnt += 1; l += 1; }
            if gk.0 != pg.0 { varint(&mut o, gk.0.wrapping_sub(pg.0)); varint(&mut o, gk.1); } else { o.push(0); varint(&mut o, gk.1.wrapping_sub(pg.1)); }
            varint(&mut o, gk.2 as u64); varint(&mut o, gk.3); varint(&mut o, cnt);
            if cnt >= 8 || rr > 8 { o.extend_from_slice(&mask.to_le_bytes()); } else { for q in 0..64 { if mask >> q & 1 == 1 { o.push(q as u8); } } }
            groups += 1; pg = gk; k = l;
        }
        bund += o.len() as u64; bz += zstd::bulk::compress(&o, 1).unwrap().len() as u64;
        i = j;
    }
    println!("  local-index edges: fixed bits {:.2} B an edge; flat sorted varint {:.2} B (zstd -1 {:.2}); bundles {groups} ({:.2} edges each) {:.2} B (zstd -1 {:.2}) an edge",
        fixed_bits / 8.0 / ne, flat as f64 / ne, fz as f64 / ne, ne / groups as f64, bund as f64 / ne, bz as f64 / ne);
}
