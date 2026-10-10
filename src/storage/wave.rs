//! THE FRAME (plans/storage-v2.md "The forward wave"): the frontier's
//! UNITS through the kernels against the visited set (read-only), the
//! TRANSLATION of their requests into it (one worker per region; new entries
//! numbered in key order, so ids are canonical when assigned), the new
//! LAYER gathered from the units' rows in id order, the frame's edge file.
//! The result is a function of the frame, not of the scheduling.

use anyhow::{ensure, Result};
use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};

use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::runtime2::{collapse_uniform, Cell2, Col, Rt2, AV};

use super::edges::XferTable;
use super::meta::FrameMeta;
use super::unit::{RaiseCtx, UnitOut, UnitSink};
use super::visited::{Key, RegionTable, VisitedSet};
use celeste_engine::exact::{is_provisional, Code, MissKey, Provisional};
use super::{id_region, region_of, state_id, StateId};
use crate::frame::{phases, Block, Filters, FrameStats, FrameStep, Layer};

/// Rows per piece of a layer: a shape's rows are cut into pieces of at most
/// this many (the checkpoint writes one file per piece, in parallel).
pub const PIECE_ROWS: usize = 1 << 17;

/// The sources of rows level -1 dropped, per source its smallest horizon that
/// admits one of them: DENSE over the wave's input rows (an atomic min per
/// row; per-worker hash maps of tens of millions of entries were 68% of a
/// frame, room (6,2) 100% f57).
pub struct DropNotes {
    /// Per frontier block, its first slot in `mins`.
    starts: Vec<usize>,
    mins: Vec<AtomicU32>,
    ids: Vec<StateId>,
}

impl DropNotes {
    pub fn new(blocks: &[Block]) -> Self {
        let mut starts = Vec::with_capacity(blocks.len());
        let mut ids = Vec::new();
        for b in blocks {
            starts.push(ids.len());
            assert_eq!(b.ids().len(), b.lanes(), "drop notes need the rows' ids");
            ids.extend_from_slice(b.ids());
        }
        let mins = (0..ids.len()).map(|_| AtomicU32::new(u32::MAX)).collect();
        DropNotes { starts, mins, ids }
    }

    /// Lane `lane` of block `block` is the source of a row dropped until `from`.
    #[inline]
    pub fn note(&self, block: u32, lane: usize, from: u32) {
        self.mins[self.starts[block as usize] + lane].fetch_min(from, Ordering::Relaxed);
    }

    /// `(id, smallest horizon)` per noted source, by id.
    pub fn into_sorted(self) -> Vec<(StateId, u32)> {
        let mut out: Vec<(StateId, u32)> = self.ids.iter().zip(&self.mins).filter_map(|(&id, m)| {
            let m = m.load(Ordering::Relaxed);
            (m != u32::MAX).then_some((id, m))
        }).collect();
        out.sort_unstable();
        out
    }
}

/// Every state's LAYER (first frame), for a raise: a hit on a state of a
/// later layer than the raised frame is refused. Per region, `cells` u16
/// per entry.
pub struct StateLayers {
    cells: usize,
    by_region: rustc_hash::FxHashMap<u32, Vec<u16>>,
}

impl StateLayers {
    pub fn new(cells: usize) -> Self {
        StateLayers { cells, by_region: Default::default() }
    }

    /// State `id` is first reached at `layer`.
    pub fn set(&mut self, id: StateId, layer: u32) {
        let v = self.by_region.entry(id_region(id)).or_default();
        let at = super::id_entry(id) as usize * self.cells + super::id_local(id) as usize;
        if v.len() <= at {
            v.resize((at / self.cells + 1) * self.cells, u16::MAX);
        }
        v[at] = u16::try_from(layer).expect("a layer past u16");
    }

    /// The layer of a state the tree holds.
    pub fn layer(&self, id: StateId) -> u32 {
        let at = super::id_entry(id) as usize * self.cells + super::id_local(id) as usize;
        let l = self.by_region.get(&id_region(id)).and_then(|v| v.get(at)).copied().unwrap_or(u16::MAX);
        assert!(l != u16::MAX, "state {} has no layer", super::show_id(id));
        l as u32
    }
}

/// One wave's output.
pub struct Wave {
    /// The new states, as pieces with their ids, in id order.
    pub next: Vec<Block>,
    pub won: bool,
    pub stats: FrameStats,
    /// The level -1 drops' sources (`Filters::notes_drops`): `(id, the
    /// smallest horizon admitting one of its dropped successors)`, by id.
    pub dropped: Vec<(StateId, u32)>,
    /// The shapes and entries the wave created.
    pub meta: FrameMeta,
}

/// Lanes per unit (one kernel call) for a frame of `lanes` input rows on
/// `workers`: ~16 units a worker, between 1024 and 4096 lanes. A bigger unit
/// fills more of each region's slices (room (3,0) 100% r0sxhfn at 8 px, f49:
/// 1024 -> 4096 lanes, padding 22% -> 11%); a small frame keeps enough units
/// to balance. `CELESTE_UNIT_LANES` fixes it (a multiple of 64).
fn unit_lanes(lanes: usize, workers: usize) -> usize {
    static FIXED: std::sync::OnceLock<Option<usize>> = std::sync::OnceLock::new();
    let fixed = *FIXED.get_or_init(|| {
        std::env::var("CELESTE_UNIT_LANES").ok().map(|v| {
            let n: usize = v.parse().unwrap_or_else(|_| panic!("CELESTE_UNIT_LANES={v:?} is not a number"));
            assert!(n >= 64 && n % 64 == 0 && n <= super::unit::MAX_UNIT_LANES, "CELESTE_UNIT_LANES={n}: a multiple of 64, at most {}", super::unit::MAX_UNIT_LANES);
            n
        })
    });
    fixed.unwrap_or_else(|| (lanes / (workers.max(1) * 16)).clamp(1024, 4096) & !63)
}

/// A unit: block, and its range of the block's live lanes (`order`).
type UnitSpec = (usize, usize, usize);

/// Cut each block's live lanes into units of `unit` lanes, whose span of
/// block lanes stays within `MAX_UNIT_LANES` (sources are named by their
/// lane from the unit's first).
fn units_of(order: &[Vec<usize>], unit: usize) -> Vec<UnitSpec> {
    let mut units = Vec::new();
    for (bi, o) in order.iter().enumerate() {
        let mut lo = 0;
        while lo < o.len() {
            let mut hi = (lo + unit).min(o.len());
            while o[hi - 1] - o[lo] >= super::unit::MAX_UNIT_LANES {
                hi -= 1;
            }
            units.push((bi, lo, hi));
            lo = hi;
        }
    }
    units
}

/// Stack per frame worker: kernel spill frames reach ~83 MB (virtual; each
/// call checks it fits, `asm_kernel::set_thread_stack`).
const WORKER_STACK: usize = 512 << 20;

/// The share of `workers x wall` spent waiting at the barrier.
fn idle_fraction(wall: std::time::Duration, workers: usize, busy: std::time::Duration) -> f64 {
    if workers == 0 || wall.is_zero() {
        return 0.0;
    }
    (1.0 - busy.as_secs_f64() / (workers as f64 * wall.as_secs_f64())).max(0.0)
}

/// What a wave reads and writes beyond its frontier.
pub struct WaveCtx<'a> {
    pub visited: &'a mut VisitedSet,
    pub xfers: &'a mut XferTable,
    pub pos: Option<&'a crate::search::pos_graph::PosObserver>,
    pub filters: Filters<'a>,
    pub frame: u32,
    /// The level's `edges/` dir: edges recorded (and the frame's edge file
    /// written) only with one.
    pub edges_dir: Option<&'a std::path::Path>,
    pub layer: Layer,
    /// A raise's per-state layers.
    pub layers: Option<&'a StateLayers>,
}

/// ONE FORWARD FRAME (module doc).
pub fn run_wave(engine: &dyn FrameStep, frontier: Vec<Block>, cx: WaveCtx) -> Result<Wave> {
    use std::time::Instant;
    let WaveCtx { visited, xfers, pos, filters, frame, edges_dir, layer, layers } = cx;
    // A platforms-unknown level knows only `PLATFORM_WORLD_FRAMES` worlds.
    let level = crate::abstraction::current_level();
    let game_frame = frame.div_ceil(crate::frame::steps_per_frame());
    ensure!(
        !level.platforms || game_frame as usize <= crate::trace::kernel::PLATFORM_WORLD_FRAMES,
        "frame {frame} at {level}: the platform worlds cover {} frames",
        crate::trace::kernel::PLATFORM_WORLD_FRAMES
    );
    let t_warm = Instant::now();
    engine.warm();
    if let Some(m) = filters.minus_one {
        let _ = m.table();
    }
    if t_warm.elapsed().as_secs_f64() > 0.5 {
        // Not `[fwd] f...`: log readers (`export-ui`) parse those as frames.
        eprintln!("[warm] f{frame}: engine and level -1 table built in {:.1} s, before the wave", t_warm.elapsed().as_secs_f64());
    }
    let mut st = FrameStats::default();
    let workers = crate::frame::threads();
    let t_frame = Instant::now();
    st.blocks_in = frontier.len();
    st.lanes_in = frontier.iter().map(Block::lanes).sum();
    st.bytes_in = frontier.iter().map(Block::bytes).sum();
    st.rss_start = crate::metrics::current_rss_gb();
    let record = edges_dir.is_some();
    ensure!(!record || frontier.iter().all(|b| b.ids().len() == b.lanes()), "frame {frame}: recording edges needs every input row's id");
    let cells: Vec<Vec<u32>> = frontier.iter().map(Block::positions).collect::<Result<_>>()?;
    // Per block its live lanes (a skipped lane is never expanded), in id
    // order: by shape, then position.
    let order: Vec<Vec<usize>> = frontier.iter().map(|b| (0..b.lanes()).filter(|&l| b.skip().is_empty() || !b.skip()[l]).collect()).collect();
    let units = units_of(&order, unit_lanes(order.iter().map(Vec::len).sum(), workers));
    let next_unit = AtomicUsize::new(0);
    let notes = (filters.notes_drops() && record).then(|| DropNotes::new(&frontier));
    let raised = match layer {
        Layer::New => None,
        Layer::Raised(r) => Some(r),
    };
    ensure!(raised.is_none() || layers.is_some(), "frame {frame}: a raise needs the tree's layers");
    // The frame's edge file (a raise's beside the frame's own).
    let index_path = edges_dir.map(|d| super::edges::file_path(d, frame, raised.map(|r| r.first_seq)));

    // THE UNITS.
    let t = Instant::now();
    struct Done {
        outs: Vec<UnitOut>,
        xfer_tab: Vec<crate::search::arc_edges::Pair>,
        pos_edges: rustc_hash::FxHashSet<(u32, u32)>,
        emitted: u64,
        requests: u64,
        lids: u64,
        busy: std::time::Duration,
    }
    let shared: &VisitedSet = visited;
    let claims = super::unit::Claims::default();
    let prov = Provisional::default();
    let done: Vec<Done> = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..workers)
            .map(|w| {
                let (frontier, cells, order, units, next_unit, notes, claims, prov, index_path) = (&frontier, &cells, &order, &units, &next_unit, notes.as_ref(), &claims, &prov, &index_path);
                std::thread::Builder::new().stack_size(WORKER_STACK).spawn_scoped(scope, move || -> Result<Done> {
                    crate::compiled::asm_kernel::set_thread_stack(WORKER_STACK);
                    let t = Instant::now();
                    let raise = raised.map(|r| RaiseCtx { raised: r, layers: layers.expect("checked") });
                    let mut sink = UnitSink::new(shared, claims, prov, filters, frame, w as u32, record, pos.is_some(), notes, raise);
                    if let Some(p) = index_path {
                        sink.stream_blocks(p)?;
                    }
                    loop {
                        let u = next_unit.fetch_add(1, Ordering::Relaxed);
                        let Some(&(bi, lo, hi)) = units.get(u) else { break };
                        let b = &frontier[bi];
                        let (first, last) = (order[bi][lo], order[bi][hi - 1]);
                        let zeros;
                        let sources: &[StateId] = if b.ids().is_empty() {
                            zeros = vec![0; last - first + 1];
                            &zeros
                        } else {
                            &b.ids()[first..=last]
                        };
                        let old = raised.is_some_and(|r| b.seq() < r.old_seqs);
                        // A whole layer piece's rows are named by their range.
                        let range = b.is_whole().then_some((b.seq(), first as u32));
                        sink.begin(u as u32, bi as u32, first, sources, range, (!b.skip().is_empty()).then_some(b.skip()), old);
                        let t_unit = phases::start();
                        engine.run(b, &cells[bi], &order[bi][lo..hi], &mut sink)?;
                        phases::add(phases::UNIT, t_unit);
                        sink.end()?;
                    }
                    crate::compiled::dispatch::fold_hits();
                    sink.finish()?;
                    Ok(Done {
                        outs: std::mem::take(&mut sink.outs),
                        xfer_tab: std::mem::take(&mut sink.xfer_tab),
                        pos_edges: std::mem::take(&mut sink.pos_edges),
                        emitted: sink.emitted,
                        requests: sink.n_requests,
                        lids: sink.n_lids,
                        busy: t.elapsed(),
                    })
                }).expect("spawn wave worker")
            })
            .collect();
        handles.into_iter().map(|h| h.join().expect("wave worker panicked")).collect::<Result<Vec<_>>>()
    })?;
    st.t_wave = t.elapsed();
    st.wave_idle = idle_fraction(st.t_wave, workers, done.iter().map(|d| d.busy).sum());
    st.rss_wave = crate::metrics::current_rss_gb();
    drop(frontier);
    let mut outs: Vec<UnitOut> = Vec::new();
    let mut tables: Vec<Vec<crate::search::arc_edges::Pair>> = vec![Vec::new(); workers];
    for (w, d) in done.into_iter().enumerate() {
        st.lanes_raw += d.emitted as usize;
        st.requests += d.requests;
        st.lids += d.lids;
        if let Some(p) = pos {
            p.record_pairs(d.pos_edges.iter().copied());
        }
        outs.extend(d.outs);
        tables[w] = d.xfer_tab;
    }
    st.units = outs.len();
    st.edge_records = outs.iter().map(|u| u.edges).sum();

    // THE TRANSLATION.
    let t = Instant::now();
    let tr = phases::start();
    drop(claims);
    let (new_states, meta) = translate(visited, &mut outs, &prov, frame)?;
    drop(prov);
    (st.key_bits, st.key_codes, st.new_codes) = (visited.keys.max_bits(), visited.keys.codes(), meta.keys.codes.len());
    resolve_lids(visited, &outs, frame)?;
    phases::add(phases::TRANSLATE, tr);
    st.t_translate = t.elapsed();

    // THE LAYER, and the frame's edges.
    let t = Instant::now();
    let tl = phases::start();
    let first_seq = match layer {
        Layer::New => 0,
        Layer::Raised(r) => r.first_seq,
    };
    let next = gather_layer(visited, &outs, &new_states, frame, first_seq)?;
    st.lanes_kept = new_states.len();
    let mut won = false;
    for b in &next {
        won |= crate::frame::reaches_win(b.rt2())?.iter().any(|&w| w);
    }
    if let (Some(dir), Some(path)) = (edges_dir, &index_path) {
        let remaps = xfers.merge(Some(dir), &tables)?;
        st.edge_bytes = super::edges::write_file(path, frame, &outs, remaps)? + outs.iter().filter(|u| u.block_at.is_some()).map(|u| u.block_len).sum::<u64>();
    }
    drop(outs);
    phases::add(phases::LAYER, tl);
    st.t_layer = t.elapsed();
    st.visited_bytes = visited.alloc_bytes();
    st.rss_end = crate::metrics::current_rss_gb();
    st.rss_file = crate::metrics::current_file_rss_gb();
    st.blocks_out = next.len();
    st.lanes_out = next.iter().map(Block::lanes).sum();
    let dropped = notes.map_or_else(Vec::new, DropNotes::into_sorted);
    phases::print_phases(t_frame.elapsed(), workers);
    Ok(Wave { next, won, stats: st, dropped, meta })
}

/// A new state: its id and its row (unit, buffer, row).
pub(crate) type NewState = (StateId, u32, u32, u32);

/// The frame's PROVISIONAL keys made final (plans/exact-keys.md): the
/// requested states' codes the key space lacks, per (shape, field) SORTED
/// and appended (new shapes first, by hash), then every provisional key in
/// the units - lids and row buffers - replaced by its packed key. The
/// dictionaries then are a function of the frame's new states.
fn finalize_keys(visited: &mut VisitedSet, outs: &mut [UnitOut], prov: &Provisional, meta: &mut FrameMeta) {
    let mut keys: Vec<Key> = outs.iter().flat_map(|u| u.requests.iter().map(|r| u.lids[r.lid as usize].key)).filter(|&k| is_provisional(k)).collect();
    keys.sort_unstable();
    keys.dedup();
    if keys.is_empty() {
        return;
    }
    let contents: Vec<MissKey> = keys.iter().map(|&k| prov.content(k)).collect();
    let mut new_shapes: Vec<u64> = contents.iter().map(|m| m.shape).filter(|&s| visited.keys.shape(s).is_none()).collect();
    new_shapes.sort_unstable();
    new_shapes.dedup();
    for s in new_shapes {
        let r = prov.shape_record(s).unwrap_or_else(|| panic!("exact keys: the new shape {s:#x} was not noted"));
        visited.keys.add_shape(&r, &mut meta.keys);
    }
    let mut codes: std::collections::BTreeMap<(u64, u32), std::collections::BTreeSet<Code>> = Default::default();
    for m in &contents {
        for &(c, code) in &m.missing {
            codes.entry((m.shape, c)).or_default().insert(code);
        }
    }
    for ((shape, cell), set) in codes {
        visited.keys.add_codes(shape, cell, &set.into_iter().collect::<Vec<_>>(), &mut meta.keys);
    }
    let map: rustc_hash::FxHashMap<Key, Key> = keys.iter().zip(&contents).map(|(&k, m)| (k, visited.keys.complete(m))).collect();
    let fin = |k: &mut Key| {
        if is_provisional(*k) {
            if let Some(f) = map.get(k) {
                *k = *f;
            }
        }
    };
    let threads = crate::frame::threads().max(1);
    let per = outs.len().div_ceil(threads).max(1);
    std::thread::scope(|sc| {
        for part in outs.chunks_mut(per) {
            let fin = &fin;
            sc.spawn(move || {
                for u in part {
                    u.lids.iter_mut().for_each(|l| fin(&mut l.key));
                    for b in &mut u.bufs {
                        b.keys.iter_mut().for_each(fin);
                    }
                }
            });
        }
    });
}

/// THE TRANSLATION: the units' requests into the visited set, their
/// provisional keys made final first (`finalize_keys`). New shapes
/// numbered first (in hash order), then each target region on one worker:
/// its requests sorted by (key, cell, unit, request), each distinct key
/// found or appended as an entry (in key order: canonical numbers), each
/// distinct (entry, cell) a NEW state, its row the first request's; every
/// request's lid gets its owner. Returns the new states by id, and the
/// frame's metadata.
pub(crate) fn translate(visited: &mut VisitedSet, outs: &mut [UnitOut], prov: &Provisional, frame: u32) -> Result<(Vec<NewState>, FrameMeta)> {
    let mut meta = FrameMeta::new();
    finalize_keys(visited, outs, prov, &mut meta);
    let mut new_shapes: Vec<u64> = outs.iter().flat_map(|u| u.requests.iter().map(|r| u.lids[r.lid as usize].shape)).filter(|s| visited.shape_index(*s).is_none()).collect();
    new_shapes.sort_unstable();
    new_shapes.dedup();
    for s in new_shapes {
        let i = visited.add_shape(s);
        meta.shapes.push((i, s));
    }
    let geo = visited.geo;
    // Per unit its requests as `region << 32 | request`, sorted (in
    // parallel); per region the units' ranges are gathered by its job.
    ensure!(outs.len() < 1 << 32, "frame {frame}: {} units", outs.len());
    let threads = crate::frame::threads();
    let unit_reqs: Vec<Vec<u64>> = {
        let v: &VisitedSet = visited;
        par_map(outs, threads, |u| {
            let mut x: Vec<u64> = u
                .requests
                .iter()
                .enumerate()
                .map(|(k, r)| {
                    let l = &u.lids[r.lid as usize];
                    (region_of(&geo, v.shape_index(l.shape).expect("numbered above"), l.slot) as u64) << 32 | k as u64
                })
                .collect();
            x.sort_unstable();
            x
        })
    };
    // Per region its request count, heaviest first; the tables taken out to
    // be filled one region a worker.
    let mut counts: rustc_hash::FxHashMap<u32, usize> = Default::default();
    for x in &unit_reqs {
        let mut i = 0;
        while i < x.len() {
            let r = (x[i] >> 32) as u32;
            let j = i + x[i..].partition_point(|&y| (y >> 32) as u32 == r);
            *counts.entry(r).or_default() += j - i;
            i = j;
        }
    }
    let mut ranges: Vec<(u32, usize)> = counts.into_iter().collect();
    ranges.sort_by_key(|&(r, n)| (std::cmp::Reverse(n), r));
    let tables = visited.tables_mut();
    for &(r, _) in &ranges {
        tables[r as usize].get_or_insert_with(Default::default);
    }
    // `&mut` to each table in `ranges`' order (disjoint regions).
    let mut by_region: Vec<Option<&mut RegionTable>> = tables.iter_mut().map(|t| t.as_deref_mut()).collect();
    let jobs: Vec<(u32, &mut RegionTable)> = ranges.iter().map(|&(r, _)| (r, by_region[r as usize].take().expect("a table per region"))).collect();
    let jobs = std::sync::Mutex::new(jobs.into_iter().rev().collect::<Vec<_>>());
    let check = crate::compiled::asm_kernel::key_check_on();
    let words = geo.words;
    /// Per region job: the region, its new states (by id), its new entries.
    type Part = Vec<(u32, Vec<NewState>, Vec<(u32, u32, Key)>)>;
    let outs_ref: &[UnitOut] = outs;
    let parts: Vec<Part> = std::thread::scope(|scope| {
        let hs: Vec<_> = (0..threads)
            .map(|_| {
                let (jobs, unit_reqs) = (&jobs, &unit_reqs);
                scope.spawn(move || -> Result<Part> {
                    let mut done: Part = Vec::new();
                    let mut items: Vec<(Key, u32, u32, u32)> = Vec::new();
                    loop {
                        let Some((region, table)) = jobs.lock().expect("translation jobs").pop() else { break };
                        let (mut news, mut entries) = (Vec::new(), Vec::new());
                        items.clear();
                        for (ui, x) in unit_reqs.iter().enumerate() {
                            let lo = x.partition_point(|&y| ((y >> 32) as u32) < region);
                            let hi = lo + x[lo..].partition_point(|&y| (y >> 32) as u32 == region);
                            let u = &outs_ref[ui];
                            items.extend(x[lo..hi].iter().map(|&y| {
                                let k = y as u32;
                                let r = &u.requests[k as usize];
                                (u.lids[r.lid as usize].key, r.local, ui as u32, k)
                            }));
                        }
                        items.sort_unstable();
                        let mut a = 0;
                        while a < items.len() {
                            let key = items[a].0;
                            let b = a + items[a..].partition_point(|x| x.0 == key);
                            let e = match table.find(key) {
                                Some(e) => e,
                                None => {
                                    let e = table.push_key(words, key);
                                    entries.push((region, e, key));
                                    e
                                }
                            };
                            let mut c = a;
                            while c < b {
                                let local = items[c].1;
                                let d = c + items[c..b].partition_point(|x| x.1 == local);
                                ensure!(table.set(words, e, local), "frame {frame}: a requested state {} was held already", super::show_id(state_id(region, e, local)));
                                let (u0, k0) = (items[c].2, items[c].3);
                                let r0 = &outs_ref[u0 as usize].requests[k0 as usize];
                                news.push((state_id(region, e, local), u0, r0.buf, r0.row));
                                for &(_, _, ui, k) in &items[c..d] {
                                    let r = &outs_ref[ui as usize].requests[k as usize];
                                    // A lid is of one region: only this job sets it.
                                    let o = &outs_ref[ui as usize].owners[r.lid as usize];
                                    let was = o.swap(super::unit::pack_owner(region, e), Ordering::Relaxed);
                                    ensure!(was == super::unit::NO_OWNER || was == super::unit::pack_owner(region, e), "frame {frame}: a lid with two owners");
                                    if check {
                                        let (x, y) = (&outs_ref[u0 as usize].bufs[r0.buf as usize], &outs_ref[ui as usize].bufs[r.buf as usize]);
                                        ensure!(x.same_row(r0.row, y, r.row), "frame {frame}: two rows of state {} differ: a key collision", super::show_id(state_id(region, e, local)));
                                    }
                                }
                                c = d;
                            }
                            a = b;
                        }
                        news.sort_unstable();
                        done.push((region, news, entries));
                    }
                    Ok(done)
                })
            })
            .collect();
        hs.into_iter().map(|h| h.join().expect("translation worker panicked")).collect::<Result<Vec<_>>>()
    })?;
    // The regions in index order: the new states by id, the entries by
    // (region, entry).
    let mut parts: Vec<(u32, Vec<NewState>, Vec<(u32, u32, Key)>)> = parts.into_iter().flatten().collect();
    parts.sort_unstable_by_key(|p| p.0);
    let mut news: Vec<NewState> = Vec::with_capacity(parts.iter().map(|p| p.1.len()).sum());
    for (_, n, entries) in parts {
        news.extend(n);
        meta.entries.extend(entries);
    }
    Ok((news, meta))
}

/// The lids whose state another unit claimed (`unit::Claims`): their
/// owners, found in the visited set the translation filled. Each must be
/// there: its state was requested.
pub(crate) fn resolve_lids(visited: &VisitedSet, outs: &[UnitOut], frame: u32) -> Result<()> {
    let r: Vec<Result<()>> = par_map(outs, crate::frame::threads(), |u| {
        for &l in &u.pending {
            let o = &u.owners[l as usize];
            if o.load(Ordering::Relaxed) != super::unit::NO_OWNER {
                continue;
            }
            let lid = &u.lids[l as usize];
            let (region, entry) = visited.find(lid.shape, lid.slot, lid.key).ok_or_else(|| anyhow::anyhow!("frame {frame}: a requested entry is not in the visited set after the translation"))?;
            o.store(super::unit::pack_owner(region, entry), Ordering::Relaxed);
        }
        Ok(())
    });
    r.into_iter().collect()
}

/// The new LAYER: per shape its new states in id order, gathered from the
/// units' row buffers, cut into pieces (`PIECE_ROWS`) numbered from
/// `first_seq`, each row with its id.
pub(crate) fn gather_layer(visited: &VisitedSet, outs: &[UnitOut], news: &[NewState], frame: u32, first_seq: u32) -> Result<Vec<Block>> {
    let geo = visited.geo;
    // Per shape (ids sort by shape first) its range of `news`.
    let mut shapes: Vec<(usize, usize)> = Vec::new();
    let mut i = 0;
    while i < news.len() {
        let s = id_region(news[i].0) / geo.slots;
        let j = i + news[i..].partition_point(|n| id_region(n.0) / geo.slots == s);
        shapes.push((i, j));
        i = j;
    }
    let mut jobs: Vec<(usize, usize, u32)> = Vec::new();
    let mut seq = first_seq;
    for &(i, j) in &shapes {
        let mut lo = i;
        while lo < j {
            let hi = (lo + PIECE_ROWS).min(j);
            ensure!(seq <= u16::MAX as u32, "frame {frame}: piece seq {seq} past 16 bits");
            jobs.push((lo, hi, seq));
            seq += 1;
            lo = hi;
        }
    }
    par_map(&jobs, crate::frame::threads(), |&(lo, hi, seq)| -> Result<Block> {
        let part = &news[lo..hi];
        // The row buffers the piece reads: in one layout (one engine), read
        // in place; else (a shape's skeleton differing between buffers) as
        // blocks through the general gather.
        let mut bufs: Vec<(u32, u32)> = part.iter().map(|n| (n.1, n.2)).collect();
        bufs.sort_unstable();
        bufs.dedup();
        let refs: Vec<&super::unit::RowBuf> = bufs.iter().map(|&(u, b)| &outs[u as usize].bufs[b as usize]).collect();
        let at = |n: &NewState| bufs.binary_search(&(n.1, n.2)).expect("a gathered buffer") as u32;
        let rt2 = if refs.iter().all(|b| b.same_layout(refs[0])) {
            let rows: Vec<(u32, u32)> = part.iter().map(|n| (at(n), n.3)).collect();
            super::unit::RowBuf::gather_rows(&refs, &rows)
        } else {
            let srcs: Vec<Rt2> = refs.iter().map(|b| b.to_rt2()).collect();
            let src_refs: Vec<&Rt2> = srcs.iter().collect();
            let rows: Vec<u64> = part.iter().map(|n| (at(n) as u64) << 32 | n.3 as u64).collect();
            gather(&src_refs, &rows)
        };
        let ids: Vec<StateId> = part.iter().map(|n| n.0).collect();
        crate::compiled::asm_kernel::key_check_block(&rt2, &visited.keys);
        let b = Block::layer_piece(rt2, ids, seq);
        if crate::compiled::asm_kernel::key_check_on() {
            let cells = b.positions()?;
            for (id, c) in b.ids().iter().zip(&cells) {
                ensure!(super::id_cell(&geo, *id) == *c, "frame {frame}: state {} stored at cell {c}", super::show_id(*id));
            }
        }
        Ok(b)
    })
    .into_iter()
    .collect()
}

/// `f` over `items` on `threads` threads, results in `items`' order.
pub fn par_map<T: Sync, R: Send>(items: &[T], threads: usize, f: impl Fn(&T) -> R + Sync) -> Vec<R> {
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
        hs.into_iter().flat_map(|h| h.join().expect("a parallel worker panicked")).collect()
    });
    out.sort_unstable_by_key(|e| e.0);
    out.into_iter().map(|e| e.1).collect()
}

/// One source's column as the gather reads it: its rows, or one value.
enum Src<'a, T> {
    Rows(&'a [T]),
    Same(T),
}

impl<T: Copy> Src<'_, T> {
    #[inline]
    fn at(&self, i: usize) -> T {
        match self {
            Src::Rows(v) => v[i],
            Src::Same(x) => *x,
        }
    }
}

/// The block of the rows `rows` (`(source << 32 | row)` into `srcs`, one
/// shape): a column the sources hold alike stays uniform; otherwise numbers
/// as `N`, intervals as `I`, anything else as `V`, then agreeing columns
/// become uniform - a function of the rows' values, not of the sources.
fn gather(srcs: &[&Rt2], rows: &[u64]) -> Rt2 {
    let t = srcs[0];
    let split = |s: u64| ((s >> 32) as usize, (s as u32) as usize);
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
            let nums: Option<Vec<Src<P8>>> = srcs
                .iter()
                .map(|s| match &s.cols[c] {
                    Col::N(v) => Some(Src::Rows(v.as_slice())),
                    Col::U(AV::Num(x)) => Some(Src::Same(*x)),
                    _ => None,
                })
                .collect();
            let ivals: Option<Vec<Src<(P8, P8)>>> = srcs
                .iter()
                .map(|s| match &s.cols[c] {
                    Col::I(v) => Some(Src::Rows(v.as_slice())),
                    Col::U(AV::Ival(a, b)) => Some(Src::Same((*a, *b))),
                    _ => None,
                })
                .collect();
            let col = if let Some(src) = nums {
                Col::N(rows.iter().map(|&s| {
                    let (p, i) = split(s);
                    src[p].at(i)
                }).collect())
            } else if let Some(src) = ivals {
                Col::I(rows.iter().map(|&s| {
                    let (p, i) = split(s);
                    src[p].at(i)
                }).collect())
            } else {
                let vs: Vec<AV> = rows
                    .iter()
                    .map(|&s| {
                        let (p, i) = split(s);
                        srcs[p].cols[c].at(i)
                    })
                    .collect();
                if vs.iter().all(|v| matches!(v, AV::Num(_))) {
                    Col::N(vs.iter().map(|v| if let AV::Num(x) = v { *x } else { unreachable!() }).collect())
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
        row_keys: rows.iter().map(|&s| {
            let (p, i) = split(s);
            srcs[p].row_keys[i]
        }).collect(),
    }
}

/// Frame 0: the initial states into an empty visited set, each block
/// reordered by id and given its ids (seq: its place); the metadata.
pub fn seed(visited: &mut VisitedSet, blocks: &mut [Block]) -> Result<FrameMeta> {
    let geo = visited.geo;
    let mut meta = FrameMeta::new();
    // The key space: every initial code, per field sorted; then the keys.
    // A row is keyed as the forward keys a reference row (`UnitSink::row_key`:
    // the level's held buttons widened) and stored as it is.
    let ids = crate::compiled::ids();
    let level = crate::abstraction::Level { held: crate::abstraction::current_level().held, ..crate::abstraction::Level::EXACT };
    let mut canon: Vec<Rt2> = Vec::with_capacity(blocks.len());
    let mut views: Vec<Rt2> = Vec::with_capacity(blocks.len());
    for b in blocks.iter() {
        let mut r = b.rt2().clone_block();
        r.canonical();
        let mut v = r.clone_block();
        crate::frame::widen_rt2_to(&mut v, level);
        v.canonical();
        ensure!(v.shape_hash == r.shape_hash, "an initial row's key view changed its shape");
        canon.push(r);
        views.push(v);
    }
    meta.keys = visited.keys.absorb(&views.iter().collect::<Vec<_>>(), ids);
    let keys: Vec<Vec<Key>> = views
        .iter()
        .map(|r| {
            visited.keys.lookup_keys(r, ids).into_iter().map(|k| k.expect("seeded codes")).collect()
        })
        .collect();
    let mut shapes: Vec<u64> = blocks.iter().map(Block::shard_shape).filter(|s| visited.shape_index(*s).is_none()).collect();
    shapes.sort_unstable();
    shapes.dedup();
    for s in shapes {
        let i = visited.add_shape(s);
        meta.shapes.push((i, s));
    }
    // Every row as (region, key, local, block, lane), inserted in that order.
    let mut rows: Vec<(u32, Key, u32, usize, usize)> = Vec::new();
    for (bi, b) in blocks.iter().enumerate() {
        let si = visited.shape_index(b.shard_shape()).expect("numbered above");
        for (lane, (&cell, &key)) in b.positions()?.iter().zip(&keys[bi]).enumerate() {
            let (slot, local) = geo.slot_local(cell);
            rows.push((region_of(&geo, si, slot), key, local, bi, lane));
        }
    }
    rows.sort_unstable();
    let mut ids: Vec<Vec<StateId>> = blocks.iter().map(|b| vec![0; b.lanes()]).collect();
    for &(r, key, local, bi, lane) in &rows {
        let t = visited.table_mut(r);
        let e = match t.find(key) {
            Some(e) => e,
            None => {
                let e = t.push_key(geo.words, key);
                meta.entries.push((r, e, key));
                e
            }
        };
        ensure!(t.set(geo.words, e, local), "the initial states hold one state twice");
        ids[bi][lane] = state_id(r, e, local);
    }
    for (seq, (b, ids)) in blocks.iter_mut().zip(ids).enumerate() {
        let mut order: Vec<u32> = (0..b.lanes() as u32).collect();
        order.sort_unstable_by_key(|&l| ids[l as usize]);
        let sorted: Vec<StateId> = order.iter().map(|&l| ids[l as usize]).collect();
        ensure!(canon[seq].shape_hash == b.shard_shape(), "an initial block is not canonical");
        let mut rt2 = canon[seq].clone_block();
        rt2.row_keys = keys[seq].clone();
        rt2.gather_lanes(&order);
        *b = Block::layer_piece(rt2, sorted, seq as u32);
    }
    meta.entries.sort_unstable();
    Ok(meta)
}

/// ONE frame of `frontier` against an EMPTY visited set, no edges: every
/// successor is new (the diagnostics' `rerun`, the tests).
pub fn one_frame(engine: &dyn FrameStep, frontier: Vec<Block>, frame: u32) -> Result<Wave> {
    let mut visited = VisitedSet::new(*super::geometry());
    let mut xfers = XferTable::default();
    let cx = WaveCtx { visited: &mut visited, xfers: &mut xfers, pos: None, filters: Filters::default(), frame, edges_dir: None, layer: Layer::New, layers: None };
    run_wave(engine, frontier, cx)
}
