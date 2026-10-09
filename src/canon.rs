//! The CANONICAL layer: a frame's new states stored in position order.
//!
//! Within a wave each worker flushes its queues into pieces of its own
//! (`frame::ForwardSink`), and a row's id `pack_id(frame, piece seq, row)` is
//! assigned at the flush: it depends on the scheduling, and a stored piece
//! interleaves positions. At the wave's end `canonical_layer` reorders the
//! layer ONCE: per shape (ascending hash), rows sorted by (region, cell,
//! key), cut into pieces of at most `PIECE_ROWS` rows, numbered in that
//! order; a row's id is its place in it (`pack_id(frame, seq, row)`, seqs
//! from the wave's first). Keys are distinct within (shape, cell) (the
//! door), so the order - and every file of the layer - does not depend on
//! the scheduling, and the frontier the next wave reads is in position
//! order.
//!
//! `Renumber` maps the flush ids to the canonical ones. Ids of this frame's
//! states were handed out during the wave, so the door's entries admitted
//! this frame (`Door::end_frame`) and the TARGETS of the frame's raw edge
//! records (`edges::compact`, where a record is decoded; saved beside the
//! records, `edges::renumber_path`) go through it. Sources are rows of the
//! previous layer, already canonical; so are the level -1 drop notes, keyed
//! by them.

use anyhow::{ensure, Context, Result};
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};

use celeste_engine::runtime2::{collapse_uniform, Cell2, Col, Rt2, AV};

use crate::frame::{id_layer, id_row, id_seq, pack_id, Block};

/// Rows per canonical piece: a shape's rows are cut into pieces of at most
/// this many (the checkpoint writes one file per piece, in parallel).
pub const PIECE_ROWS: usize = 1 << 18;

/// A wave's renumbering, flush id -> canonical id (`canonical_layer`). Ids
/// of other layers, and of pieces below `first_seq` (a raised frame's own
/// pieces), are unchanged.
#[derive(Serialize, Deserialize, Debug)]
pub struct Renumber {
    layer: u32,
    first_seq: u32,
    /// Per flush seq from `first_seq`, its rows' first index into `ids`; one
    /// more entry at the end.
    starts: Vec<u64>,
    ids: Vec<u64>,
}

impl Renumber {
    /// The layer whose ids it maps.
    pub fn layer(&self) -> u32 {
        self.layer
    }

    /// The canonical id of `id`.
    #[inline]
    pub fn map(&self, id: u64) -> u64 {
        if id_layer(id) != self.layer || id_seq(id) < self.first_seq {
            return id;
        }
        let k = (id_seq(id) - self.first_seq) as usize;
        assert!(k + 1 < self.starts.len(), "id {id:#x}: a piece this wave did not flush");
        let at = self.starts[k] + id_row(id) as u64;
        assert!(at < self.starts[k + 1], "id {id:#x}: a row this wave did not flush");
        self.ids[at as usize]
    }

    /// `map` on a `(seq << 32 | row)` of this layer (a raw record's target).
    #[inline]
    pub fn map_local(&self, local: u64) -> u64 {
        let id = self.map(pack_id(self.layer, (local >> 32) as u32, local as u32));
        (id_seq(id) as u64) << 32 | id_row(id) as u64
    }

    pub fn save(&self, path: &std::path::Path) -> Result<()> {
        crate::search::checkpoint::save_value_to(path, self)
    }

    pub fn load(path: &std::path::Path) -> Result<Self> {
        crate::search::checkpoint::load_value_from(path).with_context(|| format!("{}: the frame's renumbering", path.display()))
    }

    /// The renumbering of a wave that flushed nothing into `layer`.
    #[cfg(test)]
    pub fn identity(layer: u32) -> Self {
        Renumber { layer, first_seq: u32::MAX, starts: vec![0], ids: Vec::new() }
    }

    /// A renumbering of one flushed piece `seq` of `layer` to `to` (by row).
    #[cfg(test)]
    pub fn of_piece(layer: u32, seq: u32, to: Vec<u64>) -> Self {
        Renumber { layer, first_seq: seq, starts: vec![0, to.len() as u64], ids: to }
    }
}

/// A worker's piece of a wave: rows of one shape in flush order (ids
/// `pack_id(frame, seq, row)`), and each row's cell.
pub struct Flushed {
    pub rt2: Rt2,
    pub seq: u32,
    pub cells: Vec<u32>,
}

/// `f` over `items` on `threads` threads, results in `items`' order.
fn par_map<T: Sync, R: Send>(items: &[T], threads: usize, f: impl Fn(&T) -> R + Sync) -> Vec<R> {
    let next = AtomicUsize::new(0);
    let mut out: Vec<(usize, R)> = std::thread::scope(|scope| {
        let hs: Vec<_> = (0..threads.clamp(1, items.len().max(1)))
            .map(|_| {
                let (next, f) = (&next, &f);
                scope.spawn(move || {
                    let mut mine = Vec::new();
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        let Some(x) = items.get(i) else { break };
                        mine.push((i, f(x)));
                    }
                    mine
                })
            })
            .collect();
        hs.into_iter().flat_map(|h| h.join().expect("canonical layer worker panicked")).collect()
    });
    out.sort_unstable_by_key(|e| e.0);
    out.into_iter().map(|e| e.1).collect()
}

/// A row in the canonical order: its key and its source `(piece << 32 | row)`.
type Placed = ((u64, u64), u64);

/// The wave's pieces as the CANONICAL layer `frame` (module doc), seqs from
/// `first_seq`, and the renumbering of their flush ids.
pub fn canonical_layer(pieces: Vec<Flushed>, frame: u32, first_seq: u32) -> Result<(Vec<Block>, Renumber)> {
    let threads = crate::frame::threads();
    // The renumbering's layout: per flush seq its rows.
    let n_seq = pieces.iter().map(|p| (p.seq + 1).saturating_sub(first_seq) as usize).max().unwrap_or(0);
    let mut starts = vec![0u64; n_seq + 1];
    for p in &pieces {
        ensure!(p.seq >= first_seq, "frame {frame}: a flushed piece seq {} below the wave's first {first_seq}", p.seq);
        ensure!(p.cells.len() == p.rt2.width && p.rt2.row_keys.len() == p.rt2.width, "frame {frame}: piece {}: one cell and key per row", p.seq);
        starts[(p.seq - first_seq) as usize + 1] = p.rt2.width as u64;
    }
    for k in 0..n_seq {
        starts[k + 1] += starts[k];
    }
    let ids: Vec<AtomicU64> = (0..starts[n_seq]).map(|_| AtomicU64::new(u64::MAX)).collect();

    let mut shapes: Vec<u64> = pieces.iter().map(|p| p.rt2.shape_hash).collect();
    shapes.sort_unstable();
    shapes.dedup();
    let grid = crate::trace::kernel::region_grid();
    // Per shape its pieces and its rows in canonical order.
    let mut orders: Vec<(Vec<usize>, Vec<Placed>)> = Vec::with_capacity(shapes.len());
    for &shape in &shapes {
        let ps: Vec<usize> = (0..pieces.len()).filter(|&i| pieces[i].rt2.shape_hash == shape).collect();
        let t = &pieces[ps[0]].rt2;
        for &i in &ps[1..] {
            let r = &pieces[i].rt2;
            ensure!(
                r.structure == t.structure && r.globals == t.globals && r.strings == t.strings && r.prints == t.prints && r.cols.len() == t.cols.len(),
                "frame {frame}: two pieces of shape {shape:016x} differ beyond their columns"
            );
        }
        // Buckets: the shape's cells by (region, cell).
        let mut cells: Vec<u32> = par_map(&ps, threads, |&i| {
            let mut c = pieces[i].cells.clone();
            c.sort_unstable();
            c.dedup();
            c
        })
        .concat();
        cells.sort_unstable_by_key(|&c| (grid.and_then(|g| g.of_cell(c)), c));
        cells.dedup();
        let max_cell = cells.iter().copied().max().unwrap_or(0) as usize;
        let mut bucket_of = vec![u32::MAX; max_cell + 1];
        for (b, &c) in cells.iter().enumerate() {
            bucket_of[c as usize] = b as u32;
        }
        let nb = cells.len();
        // Per piece its rows by bucket (a counting sort) and the buckets' starts.
        let by_bucket: Vec<(Vec<u32>, Vec<u32>)> = par_map(&ps, threads, |&i| {
            let cs = &pieces[i].cells;
            let mut at = vec![0u32; nb + 1];
            for &c in cs {
                at[bucket_of[c as usize] as usize + 1] += 1;
            }
            for b in 0..nb {
                at[b + 1] += at[b];
            }
            let mut cur = at.clone();
            let mut rows = vec![0u32; cs.len()];
            for (r, &c) in cs.iter().enumerate() {
                let b = bucket_of[c as usize] as usize;
                rows[cur[b] as usize] = r as u32;
                cur[b] += 1;
            }
            (rows, at)
        });
        // Each bucket's rows by key.
        let buckets: Vec<usize> = (0..nb).collect();
        let sorted: Vec<Result<Vec<Placed>>> = par_map(&buckets, threads, |&b| {
            let mut v: Vec<Placed> = Vec::new();
            for (k, (&i, (rows, at))) in ps.iter().zip(&by_bucket).enumerate() {
                let keys = &pieces[i].rt2.row_keys;
                v.extend(rows[at[b] as usize..at[b + 1] as usize].iter().map(|&r| (keys[r as usize], (k as u64) << 32 | r as u64)));
            }
            v.sort_unstable();
            // The door admits a key once per (shape, cell).
            ensure!(v.windows(2).all(|w| w[0].0 != w[1].0), "frame {frame}: shape {shape:016x} cell {}: a key flushed twice", cells[b]);
            Ok(v)
        });
        let order: Vec<Placed> = sorted.into_iter().collect::<Result<Vec<_>>>()?.concat();
        orders.push((ps, order));
    }

    // The canonical pieces: each shape's order cut into `PIECE_ROWS`, seqs in order.
    let mut jobs: Vec<(usize, usize, usize, u32)> = Vec::new();
    let mut seq = first_seq;
    for (s, (_, order)) in orders.iter().enumerate() {
        let mut lo = 0;
        while lo < order.len() {
            let hi = (lo + PIECE_ROWS).min(order.len());
            ensure!(seq <= u16::MAX as u32, "frame {frame}: piece seq {seq} past the 16 bits an id holds");
            jobs.push((s, lo, hi, seq));
            seq += 1;
            lo = hi;
        }
    }
    let blocks: Vec<Block> = par_map(&jobs, threads, |&(s, lo, hi, seq)| {
        let (ps, order) = &orders[s];
        let rows = &order[lo..hi];
        let srcs: Vec<&Rt2> = ps.iter().map(|&i| &pieces[i].rt2).collect();
        for (r, &(_, src)) in rows.iter().enumerate() {
            let p = &pieces[ps[(src >> 32) as usize]];
            let at = starts[(p.seq - first_seq) as usize] + (src as u32) as u64;
            ids[at as usize].store(pack_id(frame, seq, r as u32), Ordering::Relaxed);
        }
        let rt2 = gather(&srcs, rows);
        crate::compiled::asm_kernel::key_check_block(&rt2);
        let ids = (0..rt2.width as u32).map(|r| pack_id(frame, seq, r)).collect();
        Block::with_ids(rt2, ids, seq)
    });
    let ids: Vec<u64> = ids.into_iter().map(AtomicU64::into_inner).collect();
    ensure!(!ids.contains(&u64::MAX), "frame {frame}: a flushed row without its canonical id");
    Ok((blocks, Renumber { layer: frame, first_seq, starts, ids }))
}

/// The block of `rows` (sources `(piece << 32 | row)` into `srcs`, one
/// shape): a column the pieces hold alike stays uniform; otherwise numbers
/// as `N`, intervals as `I`, anything else as `V`, then agreeing columns
/// become uniform - a function of the rows' values, not of the pieces.
fn gather(srcs: &[&Rt2], rows: &[Placed]) -> Rt2 {
    let t = srcs[0];
    let at = |s: u64| (srcs[(s >> 32) as usize], (s as u32) as usize);
    let cols = (0..t.cols.len())
        .map(|c| {
            if !matches!(t.structure[c], Cell2::Val) {
                return t.cols[c].clone();
            }
            if let Col::U(v) = t.cols[c] {
                if srcs.iter().all(|s| matches!(s.cols[c], Col::U(w) if w == v)) {
                    return Col::U(v);
                }
            }
            let col = if srcs.iter().all(|s| matches!(s.cols[c], Col::N(_) | Col::U(AV::Num(_)))) {
                Col::N(rows
                    .iter()
                    .map(|&(_, s)| {
                        let (r, i) = at(s);
                        match &r.cols[c] {
                            Col::N(v) => v[i],
                            Col::U(AV::Num(n)) => *n,
                            _ => unreachable!("checked: a number column"),
                        }
                    })
                    .collect())
            } else if srcs.iter().all(|s| matches!(s.cols[c], Col::I(_) | Col::U(AV::Ival(..)))) {
                Col::I(rows
                    .iter()
                    .map(|&(_, s)| {
                        let (r, i) = at(s);
                        match &r.cols[c] {
                            Col::I(v) => v[i],
                            Col::U(AV::Ival(a, b)) => (*a, *b),
                            _ => unreachable!("checked: an interval column"),
                        }
                    })
                    .collect())
            } else {
                let vs: Vec<AV> = rows
                    .iter()
                    .map(|&(_, s)| {
                        let (r, i) = at(s);
                        r.cols[c].at(i)
                    })
                    .collect();
                if vs.iter().all(|v| matches!(v, AV::Num(_))) {
                    Col::N(vs.iter().map(|v| if let AV::Num(n) = v { *n } else { unreachable!() }).collect())
                } else if vs.iter().all(|v| matches!(v, AV::Ival(..))) {
                    Col::I(vs.iter().map(|v| if let AV::Ival(a, b) = v { (*a, *b) } else { unreachable!() }).collect())
                } else {
                    Col::V(vs)
                }
            };
            collapse_uniform(col)
        })
        .collect();
    Rt2 {
        width: rows.len(),
        structure: t.structure.clone(),
        cols,
        globals: t.globals.clone(),
        strings: t.strings.clone(),
        cart: t.cart.clone(),
        cache: t.cache.clone(),
        prints: t.prints.clone(),
        shape_hash: t.shape_hash,
        row_keys: rows.iter().map(|r| r.0).collect(),
    }
}
