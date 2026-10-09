//! `structure`: the shape of the visited set at the end of f57 (door f0-56 +
//! the f57 new states: every row of the field dump), grouped by (shape,
//! cell). Sample shards as readable tables, encodings for the compressors,
//! tries per field order, and the set-coding bounds.

use crate::bits::Fields;
use std::io::Write;

/// The non-position fields, canonical order. Shapes 2, 3, 7 (99.9997% of
/// the rows); the five spawn / death shapes (~120 rows) are left out.
pub const NAMES: [&str; 13] = ["freeze", "has_dashed", "dash_effect_time", "dash_time", "djump", "grace", "dash_accel.x", "dash_accel.y", "dash_target.x", "dash_target.y", "flip.x", "spd.x", "spd.y"];
const SPDX: usize = 11;
pub const NF: usize = 13;

pub type Row = (u32, [u16; NF]);

fn num(show: &str) -> f64 {
    match show { "Bool(false)" => 0.0, "Bool(true)" => 1.0, s => s.parse().unwrap_or_else(|_| panic!("not a number: {s}")) }
}

pub struct Set { pub rows: Vec<Row>, pub shards: Vec<(usize, u32)>, pub alpha: [usize; NF], shows: Vec<[Vec<String>; NF]> }

pub fn load(dir: &str) -> Set {
    let f = Fields::open(dir);
    // Per shape: field index per canonical name, and dict index -> numeric rank.
    let ns = f.shapes.len();
    let mut map: Vec<Option<[(Option<usize>, Vec<u16>); NF]>> = Vec::new();
    let mut shows: Vec<[Vec<String>; NF]> = Vec::new();
    let mut alpha = [1usize; NF];
    for s in 0..ns {
        let fs = &f.shapes[s];
        let ok = fs.iter().all(|x| x.name == "player.x" || x.name == "player.y" || NAMES.iter().any(|n| x.name == *n || x.name.ends_with(&format!("].{n}"))));
        let mut sh: [Vec<String>; NF] = Default::default();
        if !ok || fs.is_empty() { map.push(None); shows.push(sh); continue; }
        let m: [(Option<usize>, Vec<u16>); NF] = std::array::from_fn(|k| {
            let Some(j) = f.find(s, NAMES[k]) else { sh[k] = vec!["-".into()]; return (None, vec![]) };
            let mut ord: Vec<usize> = (0..fs[j].shows.len()).collect();
            ord.sort_by(|&a, &b| num(&fs[j].shows[a]).partial_cmp(&num(&fs[j].shows[b])).unwrap());
            let mut rank = vec![0u16; ord.len()];
            for (r, &i) in ord.iter().enumerate() { rank[i] = r as u16; }
            sh[k] = ord.iter().map(|&i| fs[j].shows[i].clone()).collect();
            (Some(j), rank)
        });
        for k in 0..NF { alpha[k] = alpha[k].max(m[k].1.len()); }
        map.push(Some(m)); shows.push(sh);
    }
    let mut rows: Vec<(u64, [u16; NF])> = Vec::with_capacity(f.n);
    for i in 0..f.n {
        let (s, c, _) = f.hdr(i);
        let Some(m) = &map[s] else { continue };
        let v: [u16; NF] = std::array::from_fn(|k| m[k].0.map_or(0, |j| m[k].1[f.val(i, j) as usize]));
        rows.push(((s as u64) << 32 | c as u64, v));
    }
    // Shard ids in (shape, cell) order; rows stay in dump (layer) order within.
    let mut keys: Vec<u64> = rows.iter().map(|r| r.0).collect();
    keys.sort_unstable(); keys.dedup();
    let shards: Vec<(usize, u32)> = keys.iter().map(|&k| ((k >> 32) as usize, k as u32)).collect();
    let mut rows: Vec<Row> = rows.into_iter().map(|(k, v)| (keys.binary_search(&k).unwrap() as u32, v)).collect();
    rows.sort_by_key(|r| r.0); // stable: layer order within a shard
    Set { rows, shards, alpha, shows }
}

pub fn cell_xy(c: u32) -> (i32, i32) { (c as i32 % 512 - 64, c as i32 / 512 - 64) }

fn sort_by(rows: &mut [Row], order: &[usize]) {
    rows.sort_unstable_by(|a, b| a.0.cmp(&b.0).then_with(|| order.iter().map(|&k| a.1[k].cmp(&b.1[k])).find(|o| o.is_ne()).unwrap_or(std::cmp::Ordering::Equal)));
}

pub fn lg_choose(n: u64, k: u64) -> f64 {
    // log2 C(n, k) via lgamma-free summation for small k, Stirling otherwise.
    let k = k.min(n - k);
    if k < 64 { return (0..k).map(|i| ((n - i) as f64 / (i + 1) as f64).log2()).sum(); }
    let h = |p: f64| if p <= 0.0 || p >= 1.0 { 0.0 } else { -p * p.log2() - (1.0 - p) * (1.0 - p).log2() };
    n as f64 * h(k as f64 / n as f64) - 0.5 * (2.0 * std::f64::consts::PI * k as f64 * (n - k) as f64 / n as f64).log2()
}

/// Trie metrics over rows SORTED by (shard, order): per level nodes, the
/// subset bound sum log2 C(alphabet, children) (room alphabet), the
/// conditional entropy H(X_k | prefix) (states uniform), LOUDS-or-bitmap bits.
fn trie(rows: &[Row], order: &[usize], alpha: &[usize; NF], out: &mut dyn Write, label: &str) -> (f64, f64) {
    let n = rows.len();
    let mut lvl: Vec<u8> = vec![0; n]; // first level at which row i differs from row i-1 (0 = new shard)
    for i in 0..n {
        lvl[i] = if i == 0 || rows[i].0 != rows[i - 1].0 { 0 } else { (order.iter().position(|&k| rows[i].1[k] != rows[i - 1].1[k]).map_or(NF + 1, |p| p + 1)) as u8 };
        assert!(lvl[i] as usize <= NF, "duplicate state");
    }
    let shards = lvl.iter().filter(|&&l| l == 0).count();
    let mut prev_nodes = shards as u64;
    let (mut bound, mut succ, mut hsum) = (0f64, 0f64, 0f64);
    writeln!(out, "  {label}: order {}", order.iter().map(|&k| NAMES[k]).collect::<Vec<_>>().join(" > ")).unwrap();
    for (d, &k) in order.iter().enumerate() {
        let lv = (d + 1) as u8;
        // Walk groups at level d (parents), counting children and child sizes.
        let (mut nodes, mut b, mut h) = (0u64, 0f64, 0f64);
        let mut i = 0;
        while i < n {
            let mut j = i + 1; while j < n && lvl[j] > d as u8 { j += 1; }
            // children of parent [i, j): starts where lvl <= lv
            let mut c = 0u64; let mut a = i;
            let mut hh = 0f64;
            while a < j { let mut e = a + 1; while e < j && lvl[e] > lv { e += 1; } c += 1; let p = (e - a) as f64 / (j - i) as f64; hh -= p * p.log2(); a = e; }
            nodes += c; b += lg_choose(alpha[k] as u64, c); h += hh * (j - i) as f64;
            i = j;
        }
        let bits = (alpha[k] as f64).log2().ceil();
        let louds = nodes as f64 * (2.0 + bits);
        let bitmap = prev_nodes as f64 * alpha[k] as f64;
        succ += louds.min(bitmap);
        bound += b; hsum += h / n as f64;
        writeln!(out, "    L{:<2} {:<17} nodes {:>10} ({:>6.2}/parent)  H(X|prefix) {:>5.2} b  subset bound {:>5.2} b/state  louds {:>5.2} bitmap {:>6.2} b/state",
            d + 1, NAMES[k], nodes, nodes as f64 / prev_nodes as f64, h / n as f64, b / n as f64, louds / n as f64, bitmap / n as f64).unwrap();
        prev_nodes = nodes;
    }
    writeln!(out, "    TOTAL subset bound {:.2} bits/state ({:.2} B); succinct trie (min louds/bitmap per level) {:.2} bits/state ({:.2} B); sum H {:.2}", bound / n as f64, bound / n as f64 / 8.0, succ / n as f64, succ / n as f64 / 8.0, hsum).unwrap();
    (bound / n as f64, succ / n as f64)
}

/// Greedy order: next the field with the fewest new trie nodes.
fn greedy(rows: &[Row]) -> Vec<usize> {
    let mut pid: Vec<u32> = rows.iter().map(|r| r.0).collect();
    let mut left: Vec<usize> = (0..NF).collect();
    let mut order = Vec::new();
    while !left.is_empty() {
        let mut best = (u64::MAX, 0);
        for &k in &left {
            let mut v: Vec<u64> = rows.iter().zip(&pid).map(|(r, &p)| (p as u64) << 16 | r.1[k] as u64).collect();
            v.sort_unstable(); v.dedup();
            if (v.len() as u64) < best.0 { best = (v.len() as u64, k); }
        }
        let k = best.1;
        let mut v: Vec<u64> = rows.iter().zip(&pid).map(|(r, &p)| (p as u64) << 16 | r.1[k] as u64).collect();
        let mut s = v.clone(); s.sort_unstable(); s.dedup();
        for (i, x) in v.iter_mut().enumerate() { pid[i] = s.binary_search(x).unwrap() as u32; }
        order.push(k); left.retain(|&x| x != k);
    }
    order
}

/// Encodings: row-major (u8 a field, spd.x u16), column-major, delta to the
/// previous row of the shard, and first-changed-field.
fn encode(rows: &[Row], kind: &str) -> Vec<u8> {
    let w = |k: usize| if k == SPDX { 2 } else { 1 };
    let put = |o: &mut Vec<u8>, k: usize, v: u16| { if w(k) == 2 { o.extend_from_slice(&v.to_le_bytes()) } else { o.push(v as u8) } };
    let mut o = Vec::with_capacity(rows.len() * 14);
    match kind {
        "row" => for r in rows { for k in 0..NF { put(&mut o, k, r.1[k]); } },
        "col" => for k in 0..NF { for r in rows { put(&mut o, k, r.1[k]); } },
        "delta" => for (i, r) in rows.iter().enumerate() {
            let p = if i > 0 && rows[i - 1].0 == r.0 { rows[i - 1].1 } else { [0; NF] };
            for k in 0..NF { put(&mut o, k, r.1[k].wrapping_sub(p[k]) & if w(k) == 2 { 0xffff } else { 0xff }); }
        },
        _ => for (i, r) in rows.iter().enumerate() { // "first": index of the first changed field (orders' column order), its delta, the rest raw
            let p = if i > 0 && rows[i - 1].0 == r.0 { Some(rows[i - 1].1) } else { None };
            match p {
                None => { o.push(255); for k in 0..NF { put(&mut o, k, r.1[k]); } }
                Some(p) => {
                    let j = (0..NF).find(|&k| r.1[k] != p[k]).unwrap();
                    o.push(j as u8); put(&mut o, j, r.1[j].wrapping_sub(p[j]));
                    for k in j + 1..NF { put(&mut o, k, r.1[k]); }
                }
            }
        },
    }
    o
}

/// Rows with their fields permuted into `order` (so encodings see that order).
fn permuted(rows: &[Row], order: &[usize]) -> Vec<Row> {
    rows.iter().map(|r| (r.0, std::array::from_fn(|i| if i < order.len() { r.1[order[i]] } else { 0 }))).collect()
}

pub fn structure(dir: &str) {
    let t = std::time::Instant::now();
    let set = load(dir);
    let out_dir = format!("{dir}/structure");
    std::fs::create_dir_all(format!("{out_dir}/enc")).unwrap();
    std::fs::create_dir_all(format!("{dir}/shards")).unwrap();
    let mut rep = std::fs::File::create(format!("{out_dir}/report.txt")).unwrap();
    let n = set.rows.len();
    // Shard extents.
    let mut ext: Vec<(usize, usize)> = Vec::new();
    let mut i = 0; while i < n { let mut j = i; while j < n && set.rows[j].0 == set.rows[i].0 { j += 1; } ext.push((i, j)); i = j; }
    writeln!(rep, "{n} states (end of f57: door f0-56 + f57's new), {} shards (shapes 2, 3, 7); room alphabets {:?}; {:.1} s", ext.len(), set.alpha, t.elapsed().as_secs_f64()).unwrap();
    // Samples.
    let mut by_size: Vec<usize> = (0..ext.len()).collect();
    by_size.sort_by_key(|&s| ext[s].1 - ext[s].0);
    let wmed = { let mut acc = 0; *by_size.iter().find(|&&s| { acc += ext[s].1 - ext[s].0; acc >= n / 2 }).unwrap() };
    let at = |x: i32, y: i32| (0..ext.len()).filter(|&s| set.shards[s].0 == 2).min_by_key(|&s| { let (a, b) = cell_xy(set.shards[s].1); (a - x).abs() + (b - y).abs() }).unwrap();
    let tiny = *by_size.iter().find(|&&s| ext[s].1 - ext[s].0 >= 8).unwrap();
    let samples = [("tiny", tiny), ("median", by_size[by_size.len() / 2]), ("weighted-median", wmed), ("heaviest", *by_size.last().unwrap()), ("near-spawn", at(32, 104)), ("mid-room", at(64, 64))];
    let best_hint: Vec<usize> = vec![0, 4, 5, 10, 1, 3, 2, 8, 9, 6, 7, 12, 11];
    for (label, s) in samples {
        let (a, b) = ext[s];
        let (sh, c) = set.shards[s];
        let (x, y) = cell_xy(c);
        let mut r: Vec<Row> = set.rows[a..b].to_vec();
        sort_by(&mut r, &best_hint);
        let path = format!("{dir}/shards/{sh}_{c}.txt");
        let mut f = std::io::BufWriter::new(std::fs::File::create(&path).unwrap());
        writeln!(f, "# shape {sh} cell {c} = ({x}, {y}): {} states ({label}); sorted by {}", b - a, best_hint.iter().map(|&k| NAMES[k]).collect::<Vec<_>>().join(", ")).unwrap();
        let cellv = |k: usize, v: u16| -> String { let t = set.shows[sh][k].get(v as usize).map_or("?", |s| s.as_str()); match t { "Bool(false)" => "0".into(), "Bool(true)" => "1".into(), t => t.to_string() } };
        let head: Vec<String> = best_hint.iter().map(|&k| NAMES[k].replace("dash_effect_time", "det").replace("dash_", "d_").replace("has_dashed", "hasd")).collect();
        let wid: Vec<usize> = best_hint.iter().enumerate().map(|(i, &k)| r.iter().map(|row| cellv(k, row.1[k]).len()).max().unwrap_or(1).max(head[i].len())).collect();
        writeln!(f, "{}", head.iter().zip(&wid).map(|(h, w)| format!("{h:>w$} ")).collect::<String>()).unwrap();
        for row in &r { writeln!(f, "{}", best_hint.iter().zip(&wid).map(|(&k, w)| format!("{:>w$} ", cellv(k, row.1[k]))).collect::<String>()).unwrap(); }
        writeln!(rep, "sample {label}: shape {sh} ({x}, {y}) {} states -> {path}", b - a).unwrap();
    }
    if std::env::var_os("STRUCT_SAMPLES_ONLY").is_some() { return; }
    // Orders.
    let greedy_order = greedy(&set.rows);
    let orders: Vec<(&str, Vec<usize>)> = vec![
        ("flags+dash, spd.x, spd.y", vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]),
        ("spd.x, spd.y, flags+dash", vec![11, 12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
        ("spd.y, spd.x, flags+dash", vec![12, 11, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
        ("dash, flags, spd.y, spd.x", vec![3, 2, 8, 9, 6, 7, 1, 0, 4, 5, 10, 12, 11]),
        ("greedy (fewest nodes)", greedy_order.clone()),
    ];
    let mut sample_rows: Vec<Row> = Vec::new();
    for (_, s) in samples { sample_rows.extend_from_slice(&set.rows[ext[s].0..ext[s].1]); }
    for (scope, rows) in [("samples", &sample_rows), ("all", &set.rows)] {
        writeln!(rep, "\n== {scope}: {} states", rows.len()).unwrap();
        std::fs::write(format!("{out_dir}/enc/{scope}-row.bin"), encode(rows, "row")).unwrap();
        std::fs::write(format!("{out_dir}/enc/{scope}-col.bin"), encode(rows, "col")).unwrap();
        for (oi, (name, order)) in orders.iter().enumerate() {
            let mut r = rows.clone();
            sort_by(&mut r, order);
            trie(&r, order, &set.alpha, &mut rep, name);
            let p = permuted(&r, order);
            for kind in ["row", "col", "delta", "first"] {
                // permuted: spd.x's column moved; encode() widens column SPDX only, so re-map.
                let enc = encode_perm(&p, order, kind);
                std::fs::write(format!("{out_dir}/enc/{scope}-o{oi}-{kind}.bin"), enc).unwrap();
            }
        }
        // Flat bound: per shard log2 C(local product, n).
        let mut flat = 0f64; let mut flat_room = 0f64;
        let mut i = 0;
        while i < rows.len() {
            let mut j = i; while j < rows.len() && rows[j].0 == rows[i].0 { j += 1; }
            let local: f64 = (0..NF).map(|k| { let mut v: Vec<u16> = rows[i..j].iter().map(|r| r.1[k]).collect(); v.sort_unstable(); v.dedup(); v.len() as u64 }).product::<u64>() as f64;
            let room: f64 = set.alpha.iter().map(|&a| a as f64).product();
            let nn = (j - i) as f64;
            let lc = |m: f64| if nn * 2.0 > m { m } else { nn * (m / nn).log2() + nn * std::f64::consts::LOG2_E }; // ~ log2 C(m, n) for n << m
            flat += lc(local); flat_room += lc(room);
            i = j;
        }
        writeln!(rep, "  flat bound log2 C(product, n) per shard: local dictionaries {:.2} bits/state, room alphabets {:.2} bits/state", flat / rows.len() as f64, flat_room / rows.len() as f64).unwrap();
    }
    writeln!(rep, "greedy order: {}", greedy_order.iter().map(|&k| NAMES[k]).collect::<Vec<_>>().join(" > ")).unwrap();
    eprintln!("[structure] done {:.1} s", t.elapsed().as_secs_f64());
}

/// `encode` for rows whose columns are permuted by `order` (column i holds field order[i]).
fn encode_perm(rows: &[Row], order: &[usize], kind: &str) -> Vec<u8> {
    let wide: Vec<bool> = order.iter().map(|&k| k == SPDX).collect();
    let put = |o: &mut Vec<u8>, i: usize, v: u16| { if wide[i] { o.extend_from_slice(&v.to_le_bytes()) } else { o.push(v as u8) } };
    let mask = |i: usize| if wide[i] { 0xffffu16 } else { 0xff };
    let mut o = Vec::with_capacity(rows.len() * 14);
    match kind {
        "row" => for r in rows { for i in 0..NF { put(&mut o, i, r.1[i]); } },
        "col" => for i in 0..NF { for r in rows { put(&mut o, i, r.1[i]); } },
        "delta" => for (j, r) in rows.iter().enumerate() {
            let p = if j > 0 && rows[j - 1].0 == r.0 { rows[j - 1].1 } else { [0; NF] };
            for i in 0..NF { put(&mut o, i, r.1[i].wrapping_sub(p[i]) & mask(i)); }
        },
        _ => for (j, r) in rows.iter().enumerate() {
            let p = if j > 0 && rows[j - 1].0 == r.0 { Some(rows[j - 1].1) } else { None };
            match p {
                None => { o.push(255); for i in 0..NF { put(&mut o, i, r.1[i]); } }
                Some(p) => {
                    let f = (0..NF).find(|&i| r.1[i] != p[i]).unwrap();
                    o.push(f as u8); put(&mut o, f, r.1[f].wrapping_sub(p[f]) & mask(f));
                    for i in f + 1..NF { put(&mut o, i, r.1[i]); }
                }
            }
        },
    }
    o
}

/// `sharing`: how many of the per-node child sets repeat ACROSS shards (what
/// zstd's long window finds): per (shard, flags+dash) node its set of
/// (spd.x, spd.y), and per (shard, flags+dash, spd.x) its set of spd.y.
pub fn sharing(dir: &str) {
    let set = load(dir);
    let mut rows = set.rows;
    let order: Vec<usize> = (0..NF).collect();
    sort_by(&mut rows, &order);
    let n = rows.len();
    for (label, depth) in [("(shard, flags+dash) -> {(spd.x, spd.y)}", 11usize), ("(shard, flags+dash, spd.x) -> {spd.y}", 12)] {
        let mut sets: rustc_hash::FxHashMap<Vec<(u16, u16)>, u64> = Default::default();
        let (mut nodes, mut stored, mut stored_x) = (0u64, 0u64, 0u64);
        let mut i = 0;
        while i < n {
            let mut j = i + 1;
            while j < n && rows[j].0 == rows[i].0 && rows[j].1[..depth] == rows[i].1[..depth] { j += 1; }
            let s: Vec<(u16, u16)> = rows[i..j].iter().map(|r| if depth == 11 { (r.1[11], r.1[12]) } else { (0, r.1[12]) }).collect();
            nodes += 1;
            let e = sets.entry(s).or_insert(0);
            if *e == 0 { stored += (j - i) as u64; let mut xs: Vec<u16> = rows[i..j].iter().map(|r| r.1[11]).collect(); xs.dedup(); stored_x += xs.len() as u64; }
            *e += 1;
            i = j;
        }
        println!("{label}: {nodes} nodes, {} distinct child sets ({:.1}% of nodes); storing each distinct set once keeps {stored} of {n} leaf entries ({:.1}%), {stored_x} distinct (set, spd.x) entries in them",
            sets.len(), 100.0 * sets.len() as f64 / nodes as f64, 100.0 * stored as f64 / n as f64);
    }
}
