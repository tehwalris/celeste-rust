//! THE FRAME (plans/edges-target.md "The forward"): the frontier's UNITS
//! through the kernels, each emission a record shipped to its target's
//! ROUTE (`route`); the REGION PHASE there (one worker a route: the dedup
//! against the region's table and the edges into its bundle table and
//! segments); in SLICES of the frontier, so the records in flight stay
//! bounded. Then the frame's END: provisional keys made final, the new
//! entries numbered in key order (canonical), the new LAYER decoded from
//! its states' keys and cells (no row is copied), the frame's edge file.
//! The result is a function of the frame, not of the scheduling.

use anyhow::{ensure, Result};
use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};

use celeste_engine::runtime2::Rt2;
use rustc_hash::FxHashMap;

use super::decode::Skeletons;
use super::edges::XferTable;
use super::meta::FrameMeta;
use super::route::{Chunk, RegionCtx, RouteState, Routes, Sources};
use super::unit::{RaiseCtx, UnitSink};
use super::visited::{Key, VisitedSet};
use super::{id_region, region_of, state_id, StateId};
use crate::frame::{phases, Block, Filters, FrameStats, FrameStep, Layer};
use celeste_engine::exact::{is_provisional, Code, MissKey, Provisional};

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
/// to balance.
fn unit_lanes(lanes: usize, workers: usize) -> usize {
    (lanes / (workers.max(1) * 16)).clamp(1024, 4096) & !63
}

/// A unit: block, and its range of the block's live lanes (`order`).
type UnitSpec = (usize, usize, usize);

/// Cut each block's live lanes into units of `unit` lanes.
fn units_of(order: &[Vec<usize>], unit: usize) -> Vec<UnitSpec> {
    let mut units = Vec::new();
    for (bi, o) in order.iter().enumerate() {
        let mut lo = 0;
        while lo < o.len() {
            let hi = (lo + unit).min(o.len());
            units.push((bi, lo, hi));
            lo = hi;
        }
    }
    units
}

/// Input lanes per SLICE: a slice's units run, then its records go through
/// the region phase, before the next slice's units run - so a frame holds
/// one slice's records (~24 B an edge, ~45 edges a lane in room (6,2) 100%:
/// ~1.1 GB a slice), not the whole frame's. Slices are runs of units, so a
/// function of the frontier, as everything they cut is.
const SLICE_LANES: usize = 1 << 20;

/// The units cut into slices of about `SLICE_LANES` lanes.
fn slices_of(units: &[UnitSpec]) -> Vec<std::ops::Range<usize>> {
    let mut out = Vec::new();
    let (mut a, mut lanes) = (0usize, 0usize);
    for (k, u) in units.iter().enumerate() {
        lanes += u.2 - u.1;
        if lanes >= SLICE_LANES {
            out.push(a..k + 1);
            (a, lanes) = (k + 1, 0);
        }
    }
    if a < units.len() {
        out.push(a..units.len());
    }
    out
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
    /// The shapes' skeletons the new layer is decoded with (the forward's,
    /// kept across frames).
    pub skeletons: &'a Skeletons,
}

/// The frontier's rows as sources, by their index over its blocks (the
/// records' `src`): shape and cell from the id, key from the key column.
fn sources_of(frontier: &[Block]) -> Sources {
    let geo = super::GEOMETRY;
    let mut rows = Vec::with_capacity(frontier.iter().map(Block::lanes).sum());
    let mut starts = Vec::with_capacity(frontier.len());
    for b in frontier {
        starts.push(rows.len() as u32);
        rows.extend(b.ids().iter().zip(b.keys()).map(|(&id, &key)| super::route::Src { shape: id_region(id) / geo.slots, cell: super::id_cell(&geo, id), region: id_region(id), key }));
    }
    Sources { rows, starts }
}

/// ONE FORWARD FRAME (module doc).
pub fn run_wave(engine: &dyn FrameStep, frontier: Vec<Block>, cx: WaveCtx) -> Result<Wave> {
    use std::time::Instant;
    let WaveCtx { visited, xfers, pos, filters, frame, edges_dir, layer, layers, skeletons } = cx;
    // A platforms-unknown level knows only `PLATFORM_WORLD_FRAMES` worlds.
    let (level, s) = (engine.level(), &engine.settings());
    let game_frame = frame.div_ceil(s.steps_per_frame());
    ensure!(
        !level.platforms || game_frame as usize <= crate::trace::kernel::PLATFORM_WORLD_FRAMES,
        "frame {frame} at {level}: the platform worlds cover {} frames",
        crate::trace::kernel::PLATFORM_WORLD_FRAMES
    );
    let t_warm = Instant::now();
    engine.warm();
    if let Some(m) = filters.minus_one {
        let _ = m.table(s);
    }
    if t_warm.elapsed().as_secs_f64() > 0.5 {
        // Not `[fwd] f...`: log readers (`export-ui`) parse those as frames.
        eprintln!("[warm] f{frame}: engine and level -1 table built in {:.1} s, before the wave", t_warm.elapsed().as_secs_f64());
    }
    let mut st = FrameStats::default();
    let workers = s.threads();
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
    let slices = slices_of(&units);
    let notes = (filters.notes_drops() && record).then(|| DropNotes::new(&frontier));
    let raised = match layer {
        Layer::New => None,
        Layer::Raised(r) => Some(r),
    };
    ensure!(raised.is_none() || layers.is_some(), "frame {frame}: a raise needs the tree's layers");
    let sources = record.then(|| sources_of(&frontier));
    let routes = Routes::default();
    let prov = Provisional::default();
    // The routes through the frame, by id: their regions' tables taken out
    // of the visited set until the frame's end.
    let mut states: Vec<std::sync::Mutex<RouteState>> = Vec::new();
    let mut tables: Vec<Option<Box<super::visited::RegionTable>>> = std::mem::take(visited.tables_mut_vec());
    let mut sinks: Vec<UnitSink> = (0..workers)
        .map(|w| UnitSink::new(visited, &routes, skeletons, &prov, filters, frame, level, w as u32, sources.as_ref(), pos.is_some(), notes.as_ref(), raised.map(|r| RaiseCtx { raised: r }), s))
        .collect();
    let mut scratch: Vec<super::route::Scratch> = (0..workers).map(|_| super::route::Scratch::default()).collect();
    // Per worker: its transfer table's length merged so far, and its ids'
    // global ones.
    let mut merged: Vec<usize> = vec![0; workers];
    let mut remaps: Vec<Vec<u32>> = vec![Vec::new(); workers];
    // The previous slice's chunks and its region jobs `(route, chunks a..b,
    // bundles)`, heaviest first.
    type Jobs = Vec<(u32, usize, usize, usize)>;
    let mut pending: (Vec<Chunk>, Jobs) = (Vec::new(), Vec::new());
    let (mut t_tail, mut busy) = (std::time::Duration::ZERO, std::time::Duration::ZERO);
    let t_pipeline = Instant::now();
    // THE PIPELINE: step k runs slice k's units and slice k - 1's region
    // jobs together (a worker takes a job first, heaviest first, then
    // units), so a heavy route's job overlaps the next slice's kernels;
    // between steps, slice k's GATHER (its transfers, routes and jobs).
    for step in 0..=slices.len() {
        let slice = slices.get(step).cloned().unwrap_or(units.len()..units.len());
        let t = Instant::now();
        let next_unit = AtomicUsize::new(slice.start);
        let next_job = AtomicUsize::new(0);
        let rcx = RegionCtx { frame, sources: sources.as_ref(), remaps: &remaps, layers: raised.and(layers) };
        let (chunks, jobs, states_ref, rcx_ref) = (&pending.0, &pending.1, &states, &rcx);
        let spent: Vec<std::time::Duration> = std::thread::scope(|scope| {
            let handles: Vec<_> = sinks
                .iter_mut()
                .zip(scratch.iter_mut())
                .map(|(sink, sc)| {
                    let (frontier, cells, order, units, next_unit, next_job, sources) = (&frontier, &cells, &order, &units, &next_unit, &next_job, &sources);
                    let end = slice.end;
                    std::thread::Builder::new().stack_size(WORKER_STACK).spawn_scoped(scope, move || -> Result<std::time::Duration> {
                        crate::compiled::asm_kernel::set_thread_stack(WORKER_STACK);
                        let t = Instant::now();
                        loop {
                            // A region job first (the previous slice's).
                            if let Some(&(r, a, b, _)) = jobs.get(next_job.fetch_add(1, Ordering::Relaxed)) {
                                let cs: Vec<&Chunk> = chunks[a..b].iter().collect();
                                let mut route = states_ref[r as usize].lock().expect("a route");
                                let t = phases::start(s.diag.phases);
                                super::route::run_route(&mut route, &cs, rcx_ref, sc)?;
                                phases::add(phases::REGIONS, t);
                                continue;
                            }
                            let u = next_unit.fetch_add(1, Ordering::Relaxed);
                            if u >= end {
                                break;
                            }
                            let (bi, lo, hi) = units[u];
                            let b = &frontier[bi];
                            let (first, last) = (order[bi][lo], order[bi][hi - 1]);
                            let ids: &[StateId] = if b.ids().is_empty() { &[] } else { &b.ids()[first..=last] };
                            let old = raised.is_some_and(|r| b.seq() < r.old_seqs);
                            let base = sources.as_ref().map_or(0, |x| x.starts[bi]);
                            sink.begin(u as u32, bi as u32, first, base, ids, (!b.skip().is_empty()).then_some(b.skip()), old);
                            let t_unit = phases::start(s.diag.phases);
                            engine.run(b, &cells[bi], &order[bi][lo..hi], sink)?;
                            phases::add(phases::UNIT, t_unit);
                            sink.end()?;
                        }
                        crate::compiled::dispatch::fold_hits();
                        Ok(t.elapsed())
                    })
                    .expect("spawn wave worker")
                })
                .collect();
            handles.into_iter().map(|h| h.join().expect("wave worker panicked")).collect::<Result<Vec<_>>>()
        })?;
        busy += spent.iter().sum::<std::time::Duration>();
        if step == slices.len() {
            t_tail = t.elapsed();
        }
        drop(std::mem::take(&mut pending));
        st.rss_wave = st.rss_wave.max(crate::metrics::current_rss_gb());
        if step == slices.len() {
            break;
        }
        // THE GATHER of slice `step`: its transfers get their global ids
        // (each worker's NEW pairs only), new routes join with their
        // regions' tables, its chunks become jobs by route.
        let tails: Vec<&[crate::search::arc_edges::Pair]> = sinks.iter().zip(&merged).map(|(k, &m)| &k.xfer_tab[m..]).collect();
        for (w, ids) in xfers.merge(&tails).into_iter().enumerate() {
            remaps[w].extend(ids);
            merged[w] = remaps[w].len();
        }
        for (shape, slot) in routes.list().into_iter().skip(states.len()) {
            let region = visited.region(shape, slot);
            let table = region.and_then(|r| tables.get_mut(r as usize).and_then(Option::take));
            states.push(std::sync::Mutex::new(RouteState::new(shape, slot, region, table)));
        }
        let mut chunks: Vec<Chunk> = sinks.iter_mut().flat_map(|k| k.outbox.drain(..)).collect();
        chunks.sort_unstable_by_key(|c| (c.route, c.unit));
        let mut jobs: Jobs = Vec::new();
        let mut a = 0;
        while a < chunks.len() {
            let r = chunks[a].route;
            let b = a + chunks[a..].partition_point(|c| c.route == r);
            jobs.push((r, a, b, chunks[a..b].iter().map(|c| c.bundles.len()).sum()));
            a = b;
        }
        jobs.sort_by_key(|j| (std::cmp::Reverse(j.3), j.0));
        pending = (chunks, jobs);
    }
    let t_units = t_pipeline.elapsed();
    st.t_wave = t_units;
    st.wave_idle = idle_fraction(t_units, workers, busy);
    st.t_regions = t_tail;
    st.slices = slices.len();
    let mut tabs: Vec<Vec<crate::search::arc_edges::Pair>> = Vec::with_capacity(workers);
    for sink in &mut sinks {
        sink.finish()?;
        st.lanes_raw += sink.emitted as usize;
        if let Some(p) = pos {
            p.record_pairs(sink.pos_edges.iter().copied());
        }
        tabs.push(std::mem::take(&mut sink.xfer_tab));
    }
    st.units = units.len();
    drop(sinks);
    drop(tabs);
    drop(frontier);
    let mut states: Vec<RouteState> = states.into_iter().map(|m| m.into_inner().expect("a route")).collect();

    // THE FRAME'S END.
    let t = Instant::now();
    let tl = phases::start(s.diag.phases);
    let mut meta = FrameMeta::new();
    let finals = finalize_keys(visited, &states, &prov, &mut meta);
    drop(prov);
    (st.key_bits, st.key_codes, st.new_codes) = (visited.keys.max_bits(), visited.keys.codes(), meta.keys.codes.len());
    // New shapes numbered (in hash order); every route gets its region.
    let mut new_shapes: Vec<u64> = states.iter().filter(|r| r.new_states > 0).map(|r| r.shape).filter(|&h| visited.shape_index(h).is_none()).collect();
    new_shapes.sort_unstable();
    new_shapes.dedup();
    for h in new_shapes {
        let i = visited.add_shape(h);
        meta.shapes.push((i, h));
    }
    // (`add_shape` sized the visited set's emptied table list for them.)
    tables.resize_with(visited.tables_mut_vec().len().max(tables.len()), || None);
    let regions: Vec<Option<u32>> = states.iter().map(|r| visited.region(r.shape, r.slot)).collect();
    let sealed: Vec<Result<Option<super::route::Sealed>>> = {
        let next = AtomicUsize::new(0);
        let cells: Vec<std::sync::Mutex<&mut RouteState>> = states.iter_mut().map(std::sync::Mutex::new).collect();
        std::thread::scope(|scope| {
            let hs: Vec<_> = scratch
                .iter_mut()
                .map(|sc| {
                    let (next, cells, regions, finals) = (&next, &cells, &regions, &finals);
                    scope.spawn(move || {
                        let mut out = Vec::new();
                        loop {
                            let k = next.fetch_add(1, Ordering::Relaxed);
                            let Some(c) = cells.get(k) else { return out };
                            let mut st = c.lock().expect("a route");
                            let r = match regions[k] {
                                Some(region) => super::route::seal(&mut st, region, finals, sc).map(Some),
                                // A route of a new shape whose records the
                                // coarser level all dropped.
                                None if st.new_states == 0 => Ok(None),
                                None => Err(anyhow::anyhow!("frame {frame}: a route of an unnumbered shape with new states")),
                            };
                            out.push((k, r));
                        }
                    })
                })
                .collect();
            let mut all: Vec<(usize, Result<Option<super::route::Sealed>>)> = hs.into_iter().flat_map(|h| h.join().expect("a seal worker panicked")).collect();
            all.sort_by_key(|x| x.0);
            all.into_iter().map(|x| x.1).collect()
        })
    };
    let mut news: Vec<StateId> = Vec::new();
    for x in sealed {
        if let Some(sl) = x? {
            meta.entries.extend(sl.entries);
            news.extend(sl.states);
        }
    }
    meta.entries.sort_unstable();
    news.sort_unstable();
    // The tables back into the visited set; the routes' segments into the file.
    let mut segs = Vec::new();
    let mut src_index: Vec<(u32, Vec<u32>)> = Vec::new();
    for (r, region) in states.into_iter().zip(&regions) {
        let Some(region) = *region else { continue };
        st.edge_bundles += r.segs.iter().map(|g| g.bundles as u64).sum::<u64>();
        st.edge_records += r.segs.iter().map(|g| g.edges).sum::<u64>();
        st.routes += 1;
        if !r.sources.is_empty() {
            src_index.push((region, r.sources.into_iter().collect()));
        }
        segs.extend(r.segs);
        tables[region as usize] = Some(r.table);
    }
    *visited.tables_mut_vec() = tables;
    st.lanes_kept = news.len();
    if let Some(dir) = edges_dir {
        xfers.flush(dir)?;
        let path = super::edges::file_path(dir, frame, raised.map(|r| r.first_seq));
        st.edge_bytes = super::edges::write_file(&path, frame, segs, src_index)?;
    }
    let first_seq = match layer {
        Layer::New => 0,
        Layer::Raised(r) => r.first_seq,
    };
    let next = decode_layer(visited, skeletons, &news, frame, first_seq, s)?;
    phases::add(phases::LAYER, tl);
    st.t_layer = t.elapsed();
    st.visited_bytes = visited.alloc_bytes();
    st.rss_end = crate::metrics::current_rss_gb();
    st.rss_file = crate::metrics::current_file_rss_gb();
    st.blocks_out = next.len();
    st.lanes_out = next.iter().map(Block::lanes).sum();
    let dropped = notes.map_or_else(Vec::new, DropNotes::into_sorted);
    phases::print_phases(s.diag.phases, t_frame.elapsed(), workers);
    Ok(Wave { next, stats: st, dropped, meta })
}

/// The frame's PROVISIONAL keys made final (plans/exact-keys.md): the new
/// states' codes the key space lacks, per (shape, field) SORTED and
/// appended (new shapes first, by hash); returns each provisional key's
/// packed key. The dictionaries then are a function of the frame's new
/// states.
fn finalize_keys(visited: &mut VisitedSet, states: &[RouteState], prov: &Provisional, meta: &mut FrameMeta) -> FxHashMap<Key, Key> {
    let mut keys: Vec<Key> = states.iter().flat_map(|r| r.table.keys_from(r.start).iter().copied().filter(|&k| is_provisional(k))).collect();
    keys.sort_unstable();
    keys.dedup();
    if keys.is_empty() {
        return FxHashMap::default();
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
    keys.iter().zip(&contents).map(|(&k, m)| (k, visited.keys.complete(m))).collect()
}

/// The new LAYER: per shape its new states `news` (by id) DECODED from
/// their keys and cells with the shape's skeleton (`Skeletons`), cut into
/// pieces (`PIECE_ROWS`) numbered from `first_seq`, each row with its id.
/// `s`: the threads, `--key-check`.
pub(crate) fn decode_layer(visited: &VisitedSet, skeletons: &Skeletons, news: &[StateId], frame: u32, first_seq: u32, s: &crate::settings::Settings) -> Result<Vec<Block>> {
    let geo = visited.geo;
    // Per shape (ids sort by shape first) its range of `news`.
    let mut jobs: Vec<(usize, usize, u32)> = Vec::new();
    let mut seq = first_seq;
    let mut i = 0;
    while i < news.len() {
        let sh = id_region(news[i]) / geo.slots;
        let j = i + news[i..].partition_point(|&n| id_region(n) / geo.slots == sh);
        let mut lo = i;
        while lo < j {
            let hi = (lo + PIECE_ROWS).min(j);
            ensure!(seq <= u16::MAX as u32, "frame {frame}: piece seq {seq} past 16 bits");
            jobs.push((lo, hi, seq));
            seq += 1;
            lo = hi;
        }
        i = j;
    }
    par_map(&jobs, s.threads(), |&(lo, hi, seq)| -> Result<Block> {
        let part = &news[lo..hi];
        let shape = visited.shape_hash(id_region(part[0]) / geo.slots);
        let rows: Vec<(Key, u32)> = part
            .iter()
            .map(|&id| {
                let t = visited.table(id_region(id)).expect("a new state's region");
                (t.key(super::id_entry(id)), super::id_cell(&geo, id))
            })
            .collect();
        let rt2: Rt2 = skeletons.decode(&visited.keys, shape, &rows)?;
        if s.diag.key_check {
            crate::compiled::asm_kernel::key_check_block(&rt2, &visited.keys);
            let b = Block::from_rt2(rt2.clone_block());
            for (&id, c) in part.iter().zip(b.positions()?) {
                ensure!(super::id_cell(&geo, id) == c, "frame {frame}: state {} decoded at cell {c}", super::show_id(id));
            }
        }
        Ok(Block::with_ids(rt2, part.to_vec(), seq))
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

/// Frame 0: the initial states into an empty visited set, each block
/// reordered by id and given its ids (seq: its place); the metadata. Keyed
/// at `level` (its held buttons widened, as a reference row).
pub fn seed(visited: &mut VisitedSet, blocks: &mut [Block], level: crate::abstraction::Level) -> Result<FrameMeta> {
    let geo = visited.geo;
    let mut meta = FrameMeta::new();
    // The key space: every initial code, per field sorted; then the keys.
    // A row is keyed as the forward keys a reference row (`UnitSink::row_key`:
    // the level's held buttons widened) and stored as it is.
    let ids = crate::compiled::ids();
    let level = crate::abstraction::Level { held: level.held, ..crate::abstraction::Level::EXACT };
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
        *b = Block::with_ids(rt2, sorted, seq as u32);
    }
    meta.entries.sort_unstable();
    Ok(meta)
}

/// ONE frame of `frontier` against an EMPTY visited set, no edges: every
/// successor is new (the diagnostics' `rerun`, the tests).
pub fn one_frame(engine: &dyn FrameStep, frontier: Vec<Block>, frame: u32) -> Result<Wave> {
    let mut visited = VisitedSet::new(super::GEOMETRY);
    let mut xfers = XferTable::default();
    let skeletons = Skeletons::default();
    let cx = WaveCtx { visited: &mut visited, xfers: &mut xfers, pos: None, filters: Filters::default(), frame, edges_dir: None, layer: Layer::New, layers: None, skeletons: &skeletons };
    run_wave(engine, frontier, cx)
}
