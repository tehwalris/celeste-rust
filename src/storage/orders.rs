//! `rewrite bundle-orders`: a finished tree's bundles re-encoded, one block
//! per (target region, frame) (its segments' bundles merged), in candidate
//! orders and with relative masks, counting bytes raw and with zstd -1 -
//! the stored order (`bundle::ORDER`) checked against the alternatives on
//! real trees (plans/edges-target.md "The order").

use anyhow::Result;

use super::bundle::{self, Bundle, F};
use super::edges::EdgeStore;
use super::visited::Key;
use super::{id_local, id_region, Geometry, GEOMETRY};

/// Level-byte flags of a relative-mask record: the mask is the reference;
/// the mask is an explicit u64.
const EQ: u8 = 0x80;
const RAW: u8 = 0x40;

/// A bundle's REFERENCE mask: its source key's cells in the frame's sources
/// (`front`: layer `f - 1`, per (region, key)), shifted by the offset into
/// the target region `slot` and clipped to it (up to four source regions).
fn ref_mask(geo: &Geometry, front: &rustc_hash2::FxHashMap<(u32, Key), u64>, slot: u32, b: &Bundle) -> Option<u64> {
    if b.o >= bundle::TO_NONE || slot == geo.no_cell_slot() {
        return None;
    }
    let side = geo.side as i32;
    let per_row = geo.per_row as i32;
    let (dx, dy) = ((b.o >> 11) as i32 - 1024, (b.o & 2047) as i32 - 1024);
    let (tx0, ty0) = ((slot as i32 % per_row) * side, (slot as i32 / per_row) * side);
    let (sx0, sy0) = (tx0 - dx, ty0 - dy);
    let mut r = 0u64;
    for sry in sy0.div_euclid(side)..=(sy0 + side - 1).div_euclid(side) {
        for srx in sx0.div_euclid(side)..=(sx0 + side - 1).div_euclid(side) {
            if !(0..per_row).contains(&srx) || !(0..per_row).contains(&sry) {
                continue;
            }
            let region = super::region_of(geo, b.s_shape, (sry * per_row + srx) as u32);
            let Some(&m) = front.get(&(region, b.s_key)) else { continue };
            let shc = sx0 - srx * side;
            for ly in 0..side {
                let gy = sy0 + ly;
                if gy.div_euclid(side) != sry {
                    continue;
                }
                let row = (m >> ((gy - sry * side) * side)) & 0xff;
                let tb = if shc >= 0 { row >> shc } else { (row << -shc) & 0xff };
                r |= tb << (ly * side);
            }
        }
    }
    Some(r)
}

/// `[s x o t]` with masks RELATIVE to the reference: `EQ` where equal,
/// the bits over its cells where inside it, `RAW` and the u64 otherwise.
/// Returns the raw block and (equal, relative, raw) counts.
fn encode_relative(bundles: &mut [Bundle], refm: impl Fn(&Bundle) -> Option<u64>) -> (Vec<u8>, [u64; 3]) {
    let mut t: Vec<Key> = bundles.iter().map(|b| b.t_key).collect();
    t.sort_unstable();
    t.dedup();
    let mut s: Vec<(u32, Key)> = bundles.iter().map(|b| (b.s_shape, b.s_key)).collect();
    s.sort_unstable();
    s.dedup();
    // Records: (s rank, x, o, t rank) sorted.
    let mut recs: Vec<([u64; 4], usize)> = bundles
        .iter()
        .enumerate()
        .map(|(i, b)| ([s.binary_search(&(b.s_shape, b.s_key)).unwrap() as u64, b.x as u64, b.o as u64, t.binary_search(&b.t_key).unwrap() as u64], i))
        .collect();
    recs.sort_unstable();
    let mut out = Vec::new();
    bundle::write_dicts(&t, &s, &mut out);
    bundle::put_varint(&mut out, recs.len() as u64);
    let mut st = [0u64; 3];
    let mut prev: Option<[u64; 4]> = None;
    for (vals, i) in &recs {
        let bu = &bundles[*i];
        let l = match prev {
            None => 0,
            Some(p) => (0..4).find(|&k| vals[k] != p[k]).unwrap_or(4),
        };
        let (flags, payload) = match refm(bu) {
            Some(r) if bu.mask & !r == 0 => {
                if bu.mask == r {
                    st[0] += 1;
                    (EQ, None)
                } else {
                    st[1] += 1;
                    (0u8, Some(r))
                }
            }
            _ => {
                st[2] += 1;
                (RAW, None)
            }
        };
        out.push(l as u8 | flags);
        for k in l..4 {
            if k == 2 {
                bundle::put_off(&mut out, vals[k] as u32);
            } else if k == l && prev.is_some() {
                bundle::put_varint(&mut out, vals[k] - prev.unwrap()[k]);
            } else {
                bundle::put_varint(&mut out, vals[k]);
            }
        }
        match (flags, payload) {
            (EQ, _) => {}
            (_, Some(r)) => {
                let k = r.count_ones() as usize;
                let (mut bits, mut j, mut rr) = (0u64, 0, r);
                while rr != 0 {
                    let c = rr.trailing_zeros();
                    rr &= rr - 1;
                    if bu.mask >> c & 1 == 1 {
                        bits |= 1 << j;
                    }
                    j += 1;
                }
                out.extend_from_slice(&bits.to_le_bytes()[..k.div_ceil(8)]);
            }
            _ => out.extend_from_slice(&bu.mask.to_le_bytes()),
        }
        prev = Some(*vals);
    }
    (out, st)
}

/// The orders compared.
const ORDERS: [(&str, [F; 5]); 4] = [
    ("[M o x s t]", [F::M, F::O, F::X, F::S, F::T]),
    ("[s x o t]", [F::S, F::X, F::O, F::T, F::M]),
    ("[s t x o]", [F::S, F::T, F::X, F::O, F::M]),
    ("[t s x o]", [F::T, F::S, F::X, F::O, F::M]),
];

#[derive(Default, Clone, Copy)]
struct Tot {
    edges: u64,
    bundles: u64,
    blocks: u64,
    raw: [u64; 5],
    z: [u64; 5],
    rel: [u64; 3],
    decode_ns: u64,
}

/// The census (module doc), frames `from..=to` of the tree in `dir`.
pub fn orders(dir: &std::path::Path, from: u32, to: u32, threads: usize) -> Result<()> {
    let geo = GEOMETRY;
    let resolver = super::marks::Resolver::load(dir, to)?;
    let store = EdgeStore::open(&dir.join("edges"), to)?;
    let mut all = Tot::default();
    for f in from.max(1)..=to {
        let t0 = std::time::Instant::now();
        // Layer f - 1: per (region, key) its cells (the relative masks' reference).
        let mut front: rustc_hash2::FxHashMap<(u32, Key), u64> = Default::default();
        for (_, file) in crate::frame::frame_files(dir, f - 1)? {
            for r in 0..file.width() {
                let id = file.id_at(r);
                let (_, k, _) = resolver.resolve(id)?;
                *front.entry((id_region(id), k)).or_default() |= 1u64 << id_local(id);
            }
        }
        let regions = store.regions(f);
        let parts: Vec<Result<Tot>> = super::wave::par_map(&regions, threads, |&region| -> Result<Tot> {
            let slot = region % geo.slots;
            let mut by: rustc_hash2::FxHashMap<(u32, Key, u32, u32, Key), u64> = Default::default();
            let mut dec = super::edges::Decoder::default();
            for seg in store.segments(f, region) {
                dec.bundles(&seg, |b| *by.entry((b.s_shape, b.s_key, b.x, b.o, b.t_key)).or_default() |= b.mask)?;
            }
            let mut bundles: Vec<Bundle> = by.into_iter().map(|((s_shape, s_key, x, o, t_key), mask)| Bundle { s_shape, s_key, x, o, t_key, mask }).collect();
            let mut t = Tot { blocks: 1, bundles: bundles.len() as u64, edges: bundles.iter().map(|b| b.mask.count_ones() as u64).sum(), ..Default::default() };
            let mut z = zstd::bulk::Compressor::new(1).expect("zstd");
            let mut raw = Vec::new();
            for (k, (_, order)) in ORDERS.iter().enumerate() {
                bundle::encode(&bundles, order, &mut raw);
                t.raw[k] = raw.len() as u64;
                t.z[k] = z.compress(&raw).expect("zstd").len() as u64;
                if k == 0 {
                    let t1 = std::time::Instant::now();
                    let mut n = 0u64;
                    bundle::Block::open(&raw, order).expect("a block").records(|r| n += r.mask.count_ones() as u64).expect("decode");
                    t.decode_ns = t1.elapsed().as_nanos() as u64;
                    assert_eq!(n, t.edges);
                }
            }
            let (rawr, st) = encode_relative(&mut bundles, |b| ref_mask(&geo, &front, slot, b));
            t.raw[4] = rawr.len() as u64;
            t.z[4] = z.compress(&rawr).expect("zstd").len() as u64;
            t.rel = st;
            Ok(t)
        });
        let mut tot = Tot::default();
        for p in parts {
            add(&mut tot, &p?);
        }
        print(&format!("f{f:03}"), &tot, t0.elapsed().as_secs_f64());
        add(&mut all, &tot);
    }
    print("all", &all, 0.0);
    Ok(())
}

fn add(a: &mut Tot, b: &Tot) {
    a.edges += b.edges;
    a.bundles += b.bundles;
    a.blocks += b.blocks;
    a.decode_ns += b.decode_ns;
    for k in 0..5 {
        a.raw[k] += b.raw[k];
        a.z[k] += b.z[k];
    }
    for k in 0..3 {
        a.rel[k] += b.rel[k];
    }
}

fn print(name: &str, t: &Tot, secs: f64) {
    let e = t.edges.max(1) as f64;
    let mut line = format!("[orders] {name}: {} edges, {} bundles ({:.2} edges each), {} blocks;", t.edges, t.bundles, e / t.bundles.max(1) as f64, t.blocks);
    for (k, (n, _)) in ORDERS.iter().enumerate() {
        line += &format!(" {n} {:.3}/{:.3}", t.raw[k] as f64 / e, t.z[k] as f64 / e);
    }
    let r = (t.rel[0] + t.rel[1] + t.rel[2]).max(1) as f64;
    line += &format!(" | [s x o t] relative {:.3}/{:.3} (eq {:.1}% rel {:.1}% raw {:.1}%)", t.raw[4] as f64 / e, t.z[4] as f64 / e, 100.0 * t.rel[0] as f64 / r, 100.0 * t.rel[1] as f64 / r, 100.0 * t.rel[2] as f64 / r);
    line += &format!(" | decode [M o x s t] {:.2} ns an edge (one thread)", t.decode_ns as f64 / e);
    if secs > 0.0 {
        line += &format!(" | {secs:.1} s");
    }
    println!("{line}");
}
