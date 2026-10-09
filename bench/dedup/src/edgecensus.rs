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
