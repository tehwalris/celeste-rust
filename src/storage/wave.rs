//! THE WAVE: one frame of the hull forward (plans/architecture.md "The
//! forward: hulls and storage"): NEW states through the kernels (`unit`),
//! OLD ones re-pushed along their recorded edges (read by target: a unit a
//! segment), region jobs (`route`) overlapping the units, the frame's end.
//! A function of the frame (pushes merge order-free).

use crate::search::hull::{AccOf, HullOf};
use anyhow::{anyhow, ensure, Context, Result};
use celeste_engine::runtime2::Rt2;
use std::sync::atomic::{AtomicUsize, Ordering};

use rustc_hash::FxHashMap;

use super::decode::Skeletons;
use super::edges::XferTable;
use super::hulls::Grown;
use super::layer::{bits, Layer};
use super::route::{Chunk, RegionCtx, RouteState, Routes};
use super::unit::UnitSink;
use super::visited::{Key, VisitedSet};
use super::Sk;
use super::stats::{Lap, Stats, S};
use crate::frame::{Filters, FrameStep};
use crate::search::rem::{Pair, Rem};
use celeste_core::pico8_num::Pico8Num as P8;
use celeste_engine::exact::{KeyAdditions, MissKey, Provisional};

/// An OLD frontier state: its hull grown since its last push (`was`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Old<N: Rem = P8> {
    pub(crate) sk: Sk,
    pub(crate) layer: u16,
    pub(crate) hull: HullOf<N>,
    pub(crate) was: HullOf<N>,
}

impl<N: Rem> From<Old<N>> for Grown<N> {
    fn from(o: Old<N>) -> Grown<N> {
        Grown { sk: o.sk, layer: o.layer, hull: o.hull }
    }
}

/// OLD states by layer (ascending; no empty layer), each layer's sorted.
pub(crate) type ByLayer<N = P8> = Vec<OldLayer<N>>;

/// The OLD states of one LAYER, sorted: per ENTRY (region, key) its cells
/// and first state, per state its hull and `was` (32 B an entry, 16 a state).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OldLayer<N: Rem = P8> {
    pub(crate) layer: u16,
    entries: Vec<OldEntry>,
    hulls: Vec<[HullOf<N>; 2]>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct OldEntry {
    key: Key,
    cells: u64,
    region: u32,
    first: u32,
}

impl<N: Rem> OldLayer<N> {
    pub(crate) fn new(layer: u16, states: usize) -> Self {
        OldLayer { layer, entries: Vec::new(), hulls: Vec::with_capacity(states) }
    }

    /// Append `o` (of this layer, past every state held).
    pub(crate) fn push(&mut self, o: &Old<N>) {
        debug_assert_eq!(o.layer, self.layer, "an old state of another layer");
        let bit = 1u64 << o.sk.local;
        match self.entries.last_mut() {
            Some(e) if (e.region, e.key) == (o.sk.region, o.sk.key) => {
                debug_assert!(e.cells < bit, "an old state before the last");
                e.cells |= bit;
            }
            last => {
                debug_assert!(last.is_none_or(|e| (e.region, e.key) < (o.sk.region, o.sk.key)), "an old state before the last");
                let first = u32::try_from(self.hulls.len()).expect("a layer past u32 old states");
                self.entries.push(OldEntry { key: o.sk.key, cells: bit, region: o.sk.region, first });
            }
        }
        self.hulls.push([o.hull, o.was]);
    }

    pub(crate) fn len(&self) -> usize {
        self.hulls.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.hulls.is_empty()
    }

    fn entry_olds(&self, e: usize) -> impl Iterator<Item = Old<N>> + '_ {
        let x = self.entries[e];
        super::layer::bits(x.cells).enumerate().map(move |(k, c)| {
            let [hull, was] = self.hulls[x.first as usize + k];
            Old { sk: Sk { region: x.region, key: x.key, local: c as u8 }, layer: self.layer, hull, was }
        })
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = Old<N>> + '_ {
        (0..self.entries.len()).flat_map(move |e| self.entry_olds(e))
    }

    /// States `r` (by place), in order.
    pub(crate) fn range(&self, r: std::ops::Range<usize>) -> impl Iterator<Item = Old<N>> + '_ {
        let e = self.entries.partition_point(|x| x.first as usize <= r.start).saturating_sub(1);
        let skip = r.start - self.entries.get(e).map_or(0, |x| x.first as usize);
        (e..self.entries.len()).flat_map(move |e| self.entry_olds(e)).skip(skip).take(r.len())
    }
}

/// A FRONTIER: the states whose hull grew at a frame - the NEW ones (a
/// layer, their hulls in its state order) and the OLD ones (expanded before).
#[derive(Clone)]
pub struct Frontier<N: Rem = P8> {
    pub new: Layer<N>,
    pub hulls: Vec<HullOf<N>>,
    pub old: ByLayer<N>,
}

impl<N: Rem> Default for Frontier<N> {
    fn default() -> Self {
        Frontier { new: Layer::default(), hulls: Vec::new(), old: Vec::new() }
    }
}

impl<N: Rem> Frontier<N> {
    pub(crate) fn is_empty(&self) -> bool {
        self.new.is_empty() && self.old.is_empty()
    }

    pub fn old_states(&self) -> usize {
        self.old.iter().map(OldLayer::len).sum()
    }

    /// Its states, layers, hulls, made at `frame`: the new, then the grown.
    pub fn states(&self, frame: u32) -> impl Iterator<Item = Grown<N>> + '_ {
        super::hulls::frontier_states(&self.new, &self.hulls, frame, self.old.iter().flat_map(OldLayer::<N>::iter).map(Grown::from))
    }
}

/// A WAVE's outcome: the next frontier (its new states with their
/// skeletons), the new states' wins, the frame's stats.
pub struct Wave<N: Rem = P8> {
    pub frontier: Frontier<N>,
    /// The new states' wins: per entry of `frontier.new` (ascending) its win
    /// cells.
    pub(crate) wins: Vec<(u32, u64)>,
    pub stats: Stats,
    /// The shapes the wave numbered and what the key space gained.
    pub(crate) shapes: Vec<(u32, u64)>,
    pub(crate) keys: KeyAdditions<N>,
}

/// Lanes per unit (one kernel call): a 256th of the frame, in 1024..=4096
/// (less padding; enough units to balance). A function of the frame, not of
/// the threads: the units cut the bundles.
fn unit_lanes(lanes: u64) -> usize {
    ((lanes / 256) as usize).clamp(1024, 4096) & !63
}

/// A KERNEL unit: whole entries of one shape of the new layer (at most
/// `unit_lanes` + 63 states) and its first state's place; a RE-PUSH unit:
/// for the `layer`th list of old states, whole segments `(target region,
/// segment)` of the edge file of the layer's next frame (`frame`), about
/// `REPUSH_UNIT` bundles.
enum UnitSpec {
    Kernel { entries: std::ops::Range<usize>, state: usize, lanes: u32 },
    Repush { layer: usize, frame: u32, segs: Vec<(u32, u32)>, bundles: usize },
}

impl UnitSpec {
    /// Its weight in a slice: the states it runs; a re-push unit's bundles
    /// as the lanes that emit as many (`REPUSH_PER_LANE`).
    fn lanes(&self) -> usize {
        match self {
            UnitSpec::Kernel { lanes, .. } => *lanes as usize,
            UnitSpec::Repush { bundles, .. } => bundles.div_ceil(REPUSH_PER_LANE),
        }
    }
}

/// Units of about `unit` lanes, never across a shape (entries set-wide).
fn units_of<N: Rem>(layer: &Layer<N>, unit: usize) -> Vec<UnitSpec> {
    let slots = crate::storage::cells::SLOTS;
    let mut out = Vec::new();
    let mut cur: Option<(u32, usize, usize, u32)> = None;
    let (mut e, mut st) = (0usize, 0usize);
    for (region, t) in layer.set.tables() {
        let shape = region / slots;
        for m in t.masks() {
            let n = m.count_ones();
            cur = match cur {
                Some((sh, a, s0, l)) if sh == shape && (l as usize) < unit => Some((sh, a, s0, l + n)),
                Some((_, a, s0, l)) => {
                    out.push(UnitSpec::Kernel { entries: a..e, state: s0, lanes: l });
                    Some((shape, e, st, n))
                }
                None => Some((shape, e, st, n)),
            };
            e += 1;
            st += n as usize;
        }
    }
    if let Some((_, a, s0, l)) = cur {
        out.push(UnitSpec::Kernel { entries: a..e, state: s0, lanes: l });
    }
    out
}

/// Bundles per RE-PUSH unit (whole segments: one more may pass it).
const REPUSH_UNIT: usize = 1 << 16;

/// A re-push unit's bundles per slice lane: about what a kernel lane emits
/// (26 at r62h57's f56), so a slice holds about as many records either way.
const REPUSH_PER_LANE: usize = 32;

/// The RE-PUSH units: per layer of old states, the segments of every target
/// region their source regions reach at the layer's next frame (the edge
/// file's source index), in order, cut into units of whole segments.
fn repush_units_of<N: Rem>(by: &ByLayer<N>, files: &super::edges::EdgeFiles) -> Vec<UnitSpec> {
    let mut out = Vec::new();
    for (layer, old) in by.iter().enumerate() {
        let frame = old.layer as u32 + 1;
        let mut sources: Vec<u32> = old.entries.iter().map(|e| e.region).collect();
        sources.dedup();
        let mut targets: Vec<u32> = sources.iter().flat_map(|&r| files.targets_of(frame, r)).collect();
        targets.sort_unstable();
        targets.dedup();
        let (mut segs, mut bundles) = (Vec::new(), 0usize);
        for t in targets {
            for (k, seg) in files.segments(frame, t).iter().enumerate() {
                segs.push((t, k as u32));
                bundles += seg.bundles as usize;
                if bundles >= REPUSH_UNIT {
                    out.push(UnitSpec::Repush { layer, frame, segs: std::mem::take(&mut segs), bundles });
                    bundles = 0;
                }
            }
        }
        if !segs.is_empty() {
            out.push(UnitSpec::Repush { layer, frame, segs, bundles });
        }
    }
    out
}

/// Per layer of old states, its entries by (shape, key): their regions and
/// entry numbers (what a re-push unit looks its bundles' sources up in).
fn grown_index<N: Rem>(by: &ByLayer<N>) -> Vec<rustc_hash2::FxHashMap<(u32, Key), Vec<(u32, u32)>>> {
    let slots = crate::storage::cells::SLOTS;
    by.iter()
        .map(|old| {
            let mut m: rustc_hash2::FxHashMap<(u32, Key), Vec<(u32, u32)>> = Default::default();
            for (i, e) in old.entries.iter().enumerate() {
                m.entry((e.region / slots, e.key)).or_default().push((e.region, i as u32));
            }
            m
        })
        .collect()
}

/// Input lanes per SLICE (slices cut the segments): units run at most two
/// slices past the gathered. 256k against 1M: a quarter of the records
/// held, the same wall.
const SLICE_LANES: usize = 1 << 18;

fn slices_of(units: &[UnitSpec]) -> Vec<std::ops::Range<usize>> {
    let mut out = Vec::new();
    let (mut a, mut lanes) = (0usize, 0usize);
    for (k, u) in units.iter().enumerate() {
        lanes += u.lanes();
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

/// What a wave reads and grows besides its frontier (the forward's; the
/// pipeline's workers read it all).
pub(crate) struct WaveCtx<'a, N: Rem = P8> {
    pub(crate) visited: &'a mut VisitedSet<N>,
    pub(crate) xfers: &'a mut XferTable<N>,
    pub(crate) filters: Filters<'a, N>,
    pub(crate) frame: u32,
    /// The level's `edges/` dir: edges are recorded only with one.
    pub(crate) edges_dir: Option<&'a std::path::Path>,
    /// The tree's edge files (the re-pushes read them; `None`: a frontier
    /// without old states).
    pub(crate) edges: Option<&'a super::edges::EdgeFiles>,
    /// The forward's skeletons, registered by the emissions: the next layer's.
    pub(crate) skeletons: &'a Skeletons<N>,
    /// A tool's observer of the frame's rows (`frame::RowCheck`; none in
    /// production).
    pub(crate) check: Option<&'a dyn crate::frame::RowCheck<N>>,
}

#[derive(Default)]
struct UnitScratch {
    rows: Vec<(Key, u32)>,
    cells: Vec<u32>,
    live: Vec<usize>,
}

/// ONE KERNEL UNIT: its states DECODED, `frame::not_expanded` (decided here,
/// fresh or resumed; `orb`: the tree's orb cut, `frame::OrbCut`) left
/// out, the rest through the engine with their hulls.
#[allow(clippy::too_many_arguments)]
fn run_unit<N: Rem>(engine: &dyn FrameStep<N>, u: u32, entries: std::ops::Range<usize>, state: usize, layer: &Layer<N>, hulls: &[HullOf<N>], starts: &[usize], shapes: &[u64], sink: &mut UnitSink<'_, N>, sc: &mut UnitScratch, layer_frame: u32, orb: Option<crate::frame::OrbCut>, s: &crate::settings::Settings) -> Result<()> {
    let t_dec = sink.stats.tick_start();
    sink.begin(u);
    sc.rows.clear();
    sc.cells.clear();
    let mut state = state;
    let tables = layer.set.tables();
    let (mut ti, mut local) = layer.set.locate(starts, entries.start)?;
    let mut region = tables[ti].0;
    for _ in entries {
        while local as usize >= tables[ti].1.len() {
            (ti, local) = (ti + 1, 0);
            region = tables[ti].0;
        }
        let t = &tables[ti].1;
        let (key, mask) = (t.key(local), t.mask(local));
        local += 1;
        let si = sink.push_source(region, key);
        for local in bits(mask) {
            let cell = crate::storage::cells::cell(region % crate::storage::cells::SLOTS, local);
            sc.rows.push((key, cell));
            sc.cells.push(cell);
            sink.push_lane(si, cell, *hulls.get(state).ok_or_else(|| anyhow!("unit {u}: a state past the layer's hulls"))?);
            state += 1;
        }
    }
    sink.sources_done();
    let shape = *shapes.get((region / crate::storage::cells::SLOTS) as usize).ok_or_else(|| anyhow!("a frontier region of no shape"))?;
    let skel = layer.skeletons.get(&shape).ok_or_else(|| anyhow!("ROWS FROM KEYS: the frontier holds no skeleton of shape {shape:016x}"))?;
    let block = super::decode::decode_rows(sink.keys(), skel, &sc.rows)?;
    let skip = crate::frame::not_expanded(&block, layer_frame, orb, s)?;
    sc.live.clear();
    sc.live.extend((0..block.width).filter(|&l| !skip[l]));
    sink.stats.tick(S::PhDecode, t_dec);
    // A unit of exits only (the frame after a win) runs no kernel: their
    // shapes have none.
    if !sc.live.is_empty() {
        let t_unit = sink.stats.tick_start();
        engine.run(&block, &sc.cells, &sc.live, sink)?;
        sink.stats.tick(S::PhUnit, t_unit);
    }
    sink.end()
}

/// The wave's units and slices over a frontier.
struct Plan {
    /// The kernel units, then the re-push units.
    units: Vec<UnitSpec>,
    slices: Vec<std::ops::Range<usize>>,
    /// Per unit its slice.
    slice_of: Vec<u32>,
    /// The new layer's first entry per table (`RegionSet::locate`).
    starts: Vec<usize>,
}

impl Plan {
    fn of<N: Rem>(front: &Frontier<N>, files: Option<&super::edges::EdgeFiles>) -> Plan {
        let layer = &front.new;
        let mut units = units_of(layer, unit_lanes(layer.states()));
        if let Some(files) = files {
            units.extend(repush_units_of(&front.old, files));
        }
        let slices = slices_of(&units);
        let mut slice_of = vec![0u32; units.len()];
        for (k, r) in slices.iter().enumerate() {
            slice_of[r.clone()].fill(k as u32);
        }
        Plan { units, slices, slice_of, starts: layer.set.entry_starts() }
    }
}

/// What every wave worker reads.
struct Shared<'a, N: Rem> {
    engine: &'a dyn FrameStep<N>,
    s: &'a crate::settings::Settings,
    cx: &'a WaveCtx<'a, N>,
    plan: &'a Plan,
    front: &'a Frontier<N>,
    pipe: &'a Pipe<'a, N>,
    routes: &'a Routes,
    /// The tree's shapes by index (the frontier's).
    shapes: &'a [u64],
    /// The old states by key (`grown_index`: the re-pushes').
    index: &'a [rustc_hash2::FxHashMap<(u32, Key), Vec<(u32, u32)>>],
    spool: Option<&'a super::scratch::Scratch>,
    /// The tree's transfers before the frame (new ones from here).
    xfers_before: u32,
}

/// ONE WAVE WORKER (`pipeline`): a region job first (the oldest slice's,
/// heaviest first), else a unit of the two slices past the gathered; a
/// slice's last unit's worker GATHERS (`Pipe::gather_ready`). Any way out
/// but the end of the frame - an error, a panic - stops the others.
fn worker<N: Rem>(sh: &Shared<N>, sink: &mut UnitSink<N>, sc: &mut super::route::JobScratch) -> Result<()> {
    use std::time::Instant;
    struct Stop<'p>(&'p std::sync::atomic::AtomicBool, bool);
    impl Drop for Stop<'_> {
        fn drop(&mut self) {
            if !self.1 {
                self.0.store(true, Ordering::Release);
            }
        }
    }
    crate::compiled::asm_kernel::set_thread_stack(WORKER_STACK);
    let (pipe, plan, frame) = (sh.pipe, sh.plan, sh.cx.frame);
    let visited: &VisitedSet<N> = sh.cx.visited;
    let mut stop = Stop(&pipe.failed, false);
    let mut idle_spins = 0u32;
    let mut us = UnitScratch::default();
    let mut rs = RepushScratch::default();
    loop {
        if pipe.failed.load(Ordering::Acquire) {
            stop.1 = true;
            return Err(anyhow!("frame {frame}: another wave worker failed"));
        }
        if let Some(job) = pipe.pop_job() {
            let t = Instant::now();
            let tp = sink.stats.tick_start();
            let rcx = RegionCtx { frame, record: sh.cx.edges_dir.is_some(), remaps: &job.remaps, new_xfers: sh.xfers_before, spool: sh.spool };
            let cs: Vec<&Chunk<N>> = job.chunks.iter().collect();
            let mut route = job.state.lock().expect("a route");
            let r = super::route::run_route(&mut route, job.slice, &cs, &rcx, sc);
            drop(route);
            drop(cs);
            drop(job);
            pipe.jobs_left.fetch_sub(1, Ordering::AcqRel);
            r?;
            sink.stats.tick(S::PhRegions, tp);
            sink.stats.add_time(S::Busy, t.elapsed());
            sink.stats.add_time(S::Regions, t.elapsed());
            idle_spins = 0;
            continue;
        }
        let u = pipe.next_unit.load(Ordering::Acquire);
        if u < plan.units.len() {
            if plan.slice_of[u] as usize > pipe.gathered.load(Ordering::Acquire) + 1 || pipe.next_unit.compare_exchange(u, u + 1, Ordering::AcqRel, Ordering::Acquire).is_err() {
                pipe.wait(&mut idle_spins);
                continue;
            }
            let t = Instant::now();
            match &plan.units[u] {
                UnitSpec::Kernel { entries, state, .. } => {
                    run_unit(sh.engine, u as u32, entries.clone(), *state, &sh.front.new, &sh.front.hulls, &plan.starts, sh.shapes, sink, &mut us, frame - 1, sh.cx.filters.orb, sh.s)?;
                }
                UnitSpec::Repush { layer, frame: f, segs, .. } => {
                    let tr = sink.stats.tick_start();
                    let files = sh.cx.edges.expect("re-push units without the tree's edge files");
                    let n = run_repush(u as u32, &sh.front.old[*layer], &sh.index[*layer], files, *f, segs, visited, sh.cx.xfers, sink, &mut rs)?;
                    sink.stats.add(S::RepushEdges, n);
                    sink.stats.tick(S::PhRepush, tr);
                }
            }
            let k = plan.slice_of[u] as usize;
            pipe.upto[k][sink.worker as usize].fetch_max(sink.xfers_met(), Ordering::AcqRel);
            pipe.chunks[k].lock().expect("a slice's chunks").append(&mut sink.outbox);
            sink.stats.add_time(S::Busy, t.elapsed());
            idle_spins = 0;
            if pipe.left[k].fetch_sub(1, Ordering::AcqRel) == 1 {
                let t = Instant::now();
                let tg = sink.stats.tick_start();
                pipe.gather_ready(plan.slices.len(), sh.routes, visited)?;
                sink.stats.tick(S::PhGather, tg);
                sink.stats.add_time(S::Busy, t.elapsed());
            }
            continue;
        }
        if pipe.gathered.load(Ordering::Acquire) == plan.slices.len() && pipe.jobs_left.load(Ordering::Acquire) == 0 {
            break;
        }
        pipe.wait(&mut idle_spins);
    }
    stop.1 = true;
    for (k, n) in [S::RepushSegments, S::RepushRaw, S::RepushRecords, S::RepushGrown].into_iter().zip(rs.read) {
        sink.stats.add(k, n);
    }
    Ok(())
}

/// What the pipeline leaves the frame's end.
struct Piped<N: Rem> {
    /// Per route its state (touched states, segments).
    states: Vec<RouteState<N>>,
    /// The frame's new transfers (global ids from the table's length on).
    fresh: XferTable<N>,
    /// The workers' stats, merged.
    stats: Stats,
}

/// THE PIPELINE of a wave: its units and region jobs on `workers` threads,
/// one sink each (`UnitSink`), no barrier but its end; then the workers'
/// transfers and stats collected.
fn pipeline<N: Rem>(engine: &dyn FrameStep<N>, cx: &WaveCtx<N>, front: &Frontier<N>, plan: &Plan, prov: &Provisional<N>, spool: Option<&super::scratch::Scratch>) -> Result<Piped<N>> {
    let s = &engine.settings();
    let (visited, base): (&VisitedSet<N>, &XferTable<N>) = (cx.visited, cx.xfers);
    let workers = s.threads();
    let routes = Routes::default();
    // The visited set and the tree's transfer table are read only until the
    // frame's end; the gather numbers the workers' new pairs (`Gather::fresh`).
    let xtabs: Vec<std::sync::RwLock<Vec<Pair<N>>>> = (0..workers).map(|_| Default::default()).collect();
    let xfers_before = base.pairs.len();
    let mut sinks: Vec<UnitSink<N>> = (0..workers).map(|w| UnitSink::new(visited, &routes, cx.skeletons, prov, cx.filters, cx.frame, w as u32, base, &xtabs[w], s).observed_by(cx.check)).collect();
    let mut scratch: Vec<super::route::JobScratch> = (0..workers).map(|_| super::route::JobScratch::default()).collect();
    let pipe: Pipe<N> = Pipe {
        xtabs: &xtabs,
        next_unit: AtomicUsize::new(0),
        left: plan.slices.iter().map(|r| AtomicUsize::new(r.len())).collect(),
        upto: plan.slices.iter().map(|_| (0..workers).map(|_| AtomicUsize::new(0)).collect()).collect(),
        chunks: plan.slices.iter().map(|_| Default::default()).collect(),
        gathered: AtomicUsize::new(0),
        gather: std::sync::Mutex::new(Gather { fresh: XferTable::default(), first: xfers_before as u32, merged: vec![0; workers], remaps: vec![Vec::new(); workers], routes: 0 }),
        states: Default::default(),
        jobs: Default::default(),
        jobs_left: AtomicUsize::new(0),
        failed: std::sync::atomic::AtomicBool::new(false),
    };
    let shapes: Vec<u64> = visited.shapes().to_vec();
    let index = grown_index(&front.old);
    let sh = Shared { engine, s, cx, plan, front, pipe: &pipe, routes: &routes, shapes: &shapes, index: &index, spool, xfers_before: xfers_before as u32 };
    std::thread::scope(|scope| -> Result<()> {
        let handles: Vec<_> = sinks
            .iter_mut()
            .zip(scratch.iter_mut())
            .map(|(sink, sc)| {
                let sh = &sh;
                std::thread::Builder::new().stack_size(WORKER_STACK).spawn_scoped(scope, move || worker(sh, sink, sc)).expect("spawn wave worker")
            })
            .collect();
        // The first failure's error (a worker that only stopped for it says so).
        let mut first: Option<anyhow::Error> = None;
        for h in handles {
            match h.join().expect("wave worker panicked") {
                Ok(()) => {}
                Err(e) if first.is_none() || first.as_ref().is_some_and(|f| f.to_string().contains("another wave worker failed")) => first = Some(e),
                Err(_) => {}
            }
        }
        match first {
            Some(e) => Err(e),
            None => Ok(()),
        }
    })?;
    collect_freed();
    if s.phases {
        // The wave's anonymous peak (sampled) apart from the frame end's.
        crate::metrics::mem_phase(&format!("f{} units and region jobs", cx.frame));
    }
    let Pipe { gather, states, .. } = pipe;
    let Gather { fresh, .. } = gather.into_inner().expect("the gather");
    let states: Vec<RouteState<N>> = states
        .into_inner()
        .expect("the routes")
        .into_iter()
        .map(|a| std::sync::Arc::try_unwrap(a).map_err(|_| anyhow::anyhow!("a route still shared")).and_then(|m| m.into_inner().map_err(|_| anyhow::anyhow!("a route lock poisoned"))))
        .collect::<Result<_>>()?;
    let mut stats = Stats::default();
    for sink in &mut sinks {
        sink.finish();
        stats.merge(&sink.stats);
    }
    Ok(Piped { states, fresh, stats })
}

/// The new shapes of `states` numbered (in hash order): a route's with
/// touched states or edges (a target nothing pushed into still has its
/// edges recorded).
fn number_new_shapes<N: Rem>(visited: &mut VisitedSet<N>, states: &[RouteState<N>]) -> Vec<(u32, u64)> {
    let mut new_shapes: Vec<u64> = states.iter().filter(|r| r.is_used()).map(|r| r.shape).filter(|&h| visited.shape_index(h).is_none()).collect();
    new_shapes.sort_unstable();
    new_shapes.dedup();
    new_shapes.into_iter().map(|h| (visited.add_shape(h), h)).collect()
}

/// The routes SEALED (`route::seal_sort`, `seal_part`, `sealed`): touched
/// keys sorted (a route a thread, biggest first), compared in parts (a big
/// route's on every thread), put together; by region.
fn seal<N: Rem>(states: &mut [RouteState<N>], regions: &[Option<u32>], finals: &FxHashMap<Key, Key>, visited: &VisitedSet<N>, frame: u32, workers: usize) -> Result<Vec<super::route::Sealed<N>>> {
    use super::route::{SealPart, Sealed, Touch};
    let cells: Vec<std::sync::Mutex<(usize, &mut RouteState<N>)>> = states.iter_mut().enumerate().map(std::sync::Mutex::new).collect();
    let mut order: Vec<&std::sync::Mutex<(usize, &mut RouteState<N>)>> = cells.iter().collect();
    order.sort_by_cached_key(|c| std::cmp::Reverse(c.lock().expect("a route").1.touched.len()));
    let touches = par_map(&order, workers, |c| -> Result<(usize, Option<Touch<N>>)> {
        let mut g = c.lock().expect("a route");
        let k = g.0;
        match regions[k] {
            Some(region) => Ok((k, Some(super::route::seal_sort(&mut g.1, region, finals)?))),
            // A new shape's route whose records the coarser level dropped.
            None if !g.1.is_used() => Ok((k, None)),
            None => Err(anyhow::anyhow!("frame {frame}: a used route of an unnumbered shape")),
        }
    });
    let mut touches: Vec<(usize, Option<Touch<N>>)> = touches.into_iter().collect::<Result<_>>()?;
    touches.sort_by_key(|x| x.0);
    // The parts by route, biggest first: a route's last part puts it
    // together, so its touched keys and parts go as soon as it is sealed.
    let mut by_size: Vec<usize> = (0..touches.len()).filter(|&i| touches[i].1.is_some()).collect();
    by_size.sort_by_key(|&i| std::cmp::Reverse(touches[i].1.as_ref().map_or(0, |t| t.touched.len())));
    let mut parts: Vec<(usize, std::ops::Range<usize>)> = Vec::new();
    let mut left: Vec<AtomicUsize> = (0..touches.len()).map(|_| AtomicUsize::new(0)).collect();
    for i in by_size {
        let n = touches[i].1.as_ref().map_or(0, |t| t.touched.len());
        let before = parts.len();
        parts.extend((0..n.max(1)).step_by(super::route::SEAL_PART).map(|a| (i, a..(a + super::route::SEAL_PART).min(n))));
        *left[i].get_mut() = parts.len() - before;
    }
    let touches: Vec<std::sync::RwLock<Option<Touch<N>>>> = touches.into_iter().map(|(_, t)| std::sync::RwLock::new(t)).collect();
    let got: Vec<std::sync::Mutex<Vec<(usize, SealPart<N>)>>> = (0..touches.len()).map(|_| Default::default()).collect();
    let out: Vec<std::sync::Mutex<Option<Sealed<N>>>> = (0..touches.len()).map(|_| Default::default()).collect();
    for r in par_map(&parts, workers, |(i, rg)| -> Result<()> {
        let p = {
            let t = touches[*i].read().expect("a route's touched keys");
            super::route::seal_part(t.as_ref().expect("a part of a route with touched keys"), rg.clone(), visited)?
        };
        got[*i].lock().expect("a route's parts").push((rg.start, p));
        if left[*i].fetch_sub(1, Ordering::AcqRel) == 1 {
            let t = touches[*i].write().expect("a route's touched keys").take().expect("sealed once");
            let mut p = std::mem::take(&mut *got[*i].lock().expect("a route's parts"));
            p.sort_by_key(|x| x.0);
            *out[*i].lock().expect("a sealed route") = Some(super::route::sealed(t, p.into_iter().map(|x| x.1).collect())?);
        }
        Ok(())
    }) {
        r?;
    }
    let mut new: Vec<Sealed<N>> = out.into_iter().filter_map(|o| o.into_inner().expect("a sealed route")).collect();
    new.sort_unstable_by_key(|x| x.region);
    Ok(new)
}

/// A route's segments for the edge file: its jobs' (final once sealed,
/// by (0, slice)) and its waited ones (by slice).
type RouteSegs = (u32, Vec<((u8, u32), super::edges::Segment)>, Vec<(u32, super::edges::Segment)>);

/// THE EDGE FILE, on a thread of its own beside the frame's end: the new
/// transfers flushed, the waited segments made final (`route::reencode`,
/// one job each), every route's in slice order; its bytes.
#[allow(clippy::too_many_arguments)]
fn edge_file_writer<N: Rem>(dir: std::path::PathBuf, frame: u32, flush: super::edges::Flush, routes_segs: Vec<RouteSegs>, src_index: Vec<(u32, Vec<u32>)>, finals: std::sync::Arc<FxHashMap<Key, Key>>, spool: Option<std::sync::Arc<super::scratch::Scratch>>, threads: usize) -> Result<std::thread::JoinHandle<Result<u64>>> {
    Ok(std::thread::Builder::new().name("edge file".into()).spawn(move || -> Result<u64> {
        flush.write(&dir)?;
        let waited: Vec<(usize, u32, &super::edges::Segment)> = routes_segs.iter().enumerate().flat_map(|(k, r)| r.2.iter().map(move |(slice, seg)| (k, *slice, seg))).collect();
        let again = par_map(&waited, threads, |&(k, _, seg)| super::route::reencode(seg, routes_segs[k].0, &finals, &mut super::bundle::Encoder::default(), &mut super::file::compressor()?));
        let mut per: Vec<Vec<((u8, u32), super::edges::Segment)>> = (0..routes_segs.len()).map(|_| Vec::new()).collect();
        for (&(k, slice, _), seg) in waited.iter().zip(again) {
            per[k].push(((1, slice), seg?));
        }
        drop(waited);
        let mut segs = Vec::new();
        for ((_, mut jobs, _), waited) in routes_segs.into_iter().zip(per) {
            jobs.extend(waited);
            jobs.sort_unstable_by_key(|g| g.0);
            segs.extend(jobs.into_iter().map(|g| g.1));
        }
        super::edges::write_file(&super::edges::file_path(&dir, frame), frame, segs, src_index, spool.as_deref())
    })?)
}

/// THE NEXT FRONTIER from the sealed routes `new`: their new states (with
/// their hulls, in its order; skeletons not yet) and their grown states by
/// layer, then state - each route's sorted, appended and freed a group at a
/// time (the runs and lists are never both whole).
fn next_layer<N: Rem>(new: Vec<super::route::Sealed<N>>, frame: u32, threads: usize) -> Result<Frontier<N>> {
    let mut runs: Vec<Vec<Old<N>>> = Vec::with_capacity(new.len());
    let mut next = Layer::<N>::default();
    let mut hulls: Vec<HullOf<N>> = Vec::with_capacity(new.iter().map(|sl| sl.hulls.len()).sum());
    for sl in new {
        runs.push(sl.grown);
        if !sl.table.is_empty() {
            next.set.push(sl.region, sl.table)?;
            hulls.extend(sl.hulls);
        }
    }
    let runs: Vec<std::sync::Mutex<Vec<Old<N>>>> = runs.into_iter().map(std::sync::Mutex::new).collect();
    // (layer, state): a route's are by state, so a stable sort by layer.
    par_map(&runs, threads, |r| r.lock().expect("a route's grown states").sort_unstable_by_key(|o| (o.layer, o.sk)));
    let mut runs: Vec<Vec<Old<N>>> = runs.into_iter().map(|r| r.into_inner().expect("a route's grown states")).collect();
    let mut count = vec![0usize; frame as usize + 1];
    for o in runs.iter().flatten() {
        count[o.layer as usize] += 1;
    }
    // Groups of about a 32nd of the grown states, in region order.
    let by: Vec<std::sync::Mutex<OldLayer<N>>> = count.iter().enumerate().map(|(l, &n)| std::sync::Mutex::new(OldLayer::new(l as u16, n))).collect();
    let layers: Vec<usize> = (0..count.len()).filter(|&l| count[l] > 0).collect();
    let group_states = count.iter().sum::<usize>() / 32 + 1;
    while !runs.is_empty() {
        let (mut k, mut n) = (0, 0);
        while k < runs.len() && n < group_states {
            n += runs[k].len();
            k += 1;
        }
        let group: Vec<Vec<Old<N>>> = runs.drain(..k).collect();
        par_map(&layers, threads, |&l| {
            let mut v = by[l].lock().expect("a layer's grown states");
            for r in &group {
                let (a, b) = (r.partition_point(|o| (o.layer as usize) < l), r.partition_point(|o| (o.layer as usize) <= l));
                for o in &r[a..b] {
                    v.push(o);
                }
            }
        });
    }
    let old: ByLayer<N> = by.into_iter().map(|v| v.into_inner().expect("a layer's grown states")).filter(|x| !x.is_empty()).collect();
    Ok(Frontier { new: next, hulls, old })
}

/// ONE FORWARD FRAME: frame `cx.frame` from `front` (grown the frame before):
/// the pipeline (`pipeline`), then the frame's end - keys made final, new
/// shapes numbered, the routes sealed, the edge file written beside, the
/// ladder's filter, the visited set absorbing, the next layer and its wins.
pub(crate) fn run_wave<N: Rem>(engine: &dyn FrameStep<N>, front: Frontier<N>, cx: WaveCtx<N>) -> Result<Wave<N>> {
    use std::time::Instant;
    let frame = cx.frame;
    let s = &engine.settings();
    ensure!(frame > 0, "a wave makes frame 1 on");
    ensure!(front.hulls.len() as u64 == front.new.states(), "frame {frame}: {} hulls for the frontier's {} new states", front.hulls.len(), front.new.states());
    ensure!(cx.edges.is_some() || front.old.is_empty(), "frame {frame}: old states to re-push without the tree's edge files");
    let t_warm = Instant::now();
    engine.warm();
    if t_warm.elapsed().as_secs_f64() > 0.5 {
        eprintln!("[warm] f{frame}: engine built in {:.1} s, before the wave", t_warm.elapsed().as_secs_f64());
    }
    // What the last frame's end and checkpoint freed back to the OS before
    // the wave's threads (new heaps) allocate: kept, it adds to the peak.
    collect_freed();
    let mut st = Stats::new(s.phases);
    let workers = s.threads();
    st.set(S::EntriesIn, front.new.entries() as u64);
    st.set(S::LanesIn, front.new.states());
    st.set(S::OldIn, front.old_states() as u64);
    st.set_gb(S::RssStart, crate::metrics::current_rss_gb());
    // The segments wait in the SPOOL, not in memory.
    let spool: Option<std::sync::Arc<super::scratch::Scratch>> = match cx.edges_dir {
        Some(dir) => {
            std::fs::create_dir_all(dir)?;
            Some(std::sync::Arc::new(super::scratch::Scratch::create(&dir.join(format!("f{frame:03}.spool")))?))
        }
        None => None,
    };
    let plan = Plan::of(&front, cx.edges);
    let prov = Provisional::default();
    if s.phases {
        crate::metrics::mem_phase(&format!("f{frame} start"));
    }
    let t_pipeline = Instant::now();
    let Piped { mut states, fresh, stats } = pipeline(engine, &cx, &front, &plan, &prov, spool.as_deref())?;
    let WaveCtx { visited, xfers, filters, edges_dir, skeletons, check, .. } = cx;
    st.add_time(S::Wave, t_pipeline.elapsed());
    st.merge(&stats);
    st.set_idle(workers);
    st.set(S::Units, plan.units.len() as u64);
    st.set(S::Slices, plan.slices.len() as u64);
    // The frontier is spent: dropped before the frame's end builds the next.
    drop(plan);
    drop(front);
    st.set_gb(S::RssWave, crate::metrics::current_rss_gb());
    xfers.append(fresh);
    // The exited workers' buffers back before the frame's end allocates.
    collect_freed();

    let t = Instant::now();
    let tl = st.tick_start();
    let mut adds = KeyAdditions::default();
    let mut lap = Lap::start();
    let finals = std::sync::Arc::new(finalize_keys(visited, &states, &prov, &mut adds));
    st.lap(S::EndKeys, &mut lap);
    drop(prov);
    st.set(S::KeyBits, visited.keys.max_bits() as u64);
    st.set(S::KeyCodes, visited.keys.codes() as u64);
    st.set(S::NewCodes, adds.codes.len() as u64);
    let shapes_numbered = number_new_shapes(visited, &states);
    let regions: Vec<Option<u32>> = states.iter().map(|r| visited.region(r.shape, r.slot)).collect();
    st.set(S::Pushes, states.iter().map(|r| r.pushes).sum());
    st.set(S::Touched, states.iter().map(|r| r.touched.len() as u64).sum());
    let mut new = seal(&mut states, &regions, &finals, visited, frame, workers)?;
    // The routes' touched tables, freed off their (exited) threads: back now.
    collect_freed();
    if s.phases {
        crate::metrics::mem_phase(&format!("f{frame} the routes sealed (keys final, new and grown states)"));
    }
    for sl in &new {
        for (k, d) in [S::SealSort, S::SealCompare, S::SealTable].into_iter().zip(sl.t) {
            st.add_time(k, d);
        }
        st.add(S::WaitedSegments, sl.waited.0 as u64);
        st.add(S::WaitedBundles, sl.waited.1);
    }
    st.lap(S::EndSeal, &mut lap);
    let mut routes_segs: Vec<RouteSegs> = Vec::new();
    let mut src_index: Vec<(u32, Vec<u32>)> = Vec::new();
    for (r, region) in states.into_iter().zip(&regions) {
        let Some(region) = *region else { continue };
        st.add(S::Bundles, r.segs.iter().map(|g| g.1.bundles as u64).sum::<u64>() + r.prov.iter().map(|g| g.1.bundles as u64).sum::<u64>());
        st.add(S::Edges, r.segs.iter().map(|g| g.1.edges).sum::<u64>() + r.prov.iter().map(|g| g.1.edges).sum::<u64>());
        st.add(S::Routes, 1);
        if !r.sources.is_empty() {
            src_index.push((region, r.sources.into_iter().collect()));
        }
        routes_segs.push((region, r.segs, r.prov));
    }
    let edge_file = match edges_dir {
        Some(dir) => Some(edge_file_writer::<N>(dir.to_path_buf(), frame, xfers.take_flush(), routes_segs, src_index, finals.clone(), spool.clone(), s.threads())?),
        None => None,
    };
    if let Some(f) = filters.marks {
        mark_filter(&mut new, f, visited, skeletons, frame)?;
    }
    st.lap(S::EndFilter, &mut lap);
    // Into the visited set: the new states (new: none held) with their
    // layer and hull, the grown states' hulls.
    let layer16 = u16::try_from(frame).map_err(|_| anyhow!("frame {frame}: past a u16 layer"))?;
    let parts: Vec<super::visited::Part<N, Old<N>>> = new.iter().map(|sl| super::visited::Part { region: sl.region, new: &sl.table, hulls: &sl.hulls, grown: &sl.grown, layer: layer16 }).collect();
    let up = visited.absorb(&parts, s.threads())?;
    drop(parts);
    ensure!(up.held_new == 0, "frame {frame}: {} of the next layer's states the visited set held already", up.held_new);
    for (k, n) in [(S::AbsorbAdded, up.added), (S::AbsorbKept, up.kept), (S::AbsorbDecoded, up.decoded), (S::AbsorbMade, up.made)] {
        st.set(k, n as u64);
    }
    collect_freed();
    st.lap(S::EndAbsorb, &mut lap);
    let mut next = next_layer(new, frame, s.threads())?;
    let mut shapes_in: Vec<u32> = next.new.set.tables().iter().map(|(r, _)| r / crate::storage::cells::SLOTS).collect();
    shapes_in.dedup();
    for si in shapes_in {
        let h = visited.shape_hash(si);
        next.new.skeletons.insert(h, skeletons.get(h)?);
    }
    ensure!(next.new.states() == next.hulls.len() as u64, "frame {frame}: the next layer holds {} states, {} hulls", next.new.states(), next.hulls.len());
    st.lap(S::EndLayer, &mut lap);
    let wins = layer_wins(visited, &next.new, frame, s, check)?;
    st.lap(S::EndWins, &mut lap);
    if s.phases {
        crate::metrics::mem_phase(&format!("f{frame} the next layer's wins decoded"));
    }
    st.set(S::Kept, next.new.states());
    st.set(S::Grown, next.old_states() as u64);
    // The writers joined: the files are whole when the wave returns.
    if let Some(h) = edge_file {
        st.set(S::EdgeBytes, h.join().map_err(|_| anyhow!("frame {frame}: the edge file's writer panicked"))??);
        st.lap(S::EndEdgeFile, &mut lap);
    }
    st.tick(S::PhEnd, tl);
    collect_freed();
    if s.phases {
        crate::metrics::mem_phase(&format!("f{frame} the frame's end"));
    }
    st.add_time(S::Layer, t.elapsed());
    st.set_gb(S::RssEnd, crate::metrics::current_rss_gb());
    st.set_gb(S::RssFile, crate::metrics::current_file_rss_gb());
    st.set(S::EntriesOut, next.new.entries() as u64);
    st.set(S::LanesOut, next.new.states());
    Ok(Wave { frontier: next, wins, stats: st, shapes: shapes_numbered, keys: adds })
}

/// THE OBJECTS LADDER's filter (`frame::MarkFilter`) on the new and grown
/// states: a re-push reaches a state along an edge recorded earlier, whose
/// verdict does not hold now; every state kept is checked at this frame.
fn mark_filter<N: Rem>(sealed: &mut [super::route::Sealed<N>], f: &crate::frame::MarkFilter<N>, visited: &VisitedSet<N>, skeletons: &Skeletons<N>, frame: u32) -> Result<()> {
    for sl in sealed.iter_mut() {
        let shape = visited.shape_hash(sl.region / crate::storage::cells::SLOTS);
        let slot = sl.region % crate::storage::cells::SLOTS;
        let skel = skeletons.get(shape)?;
        // The region's kept states, new then grown.
        let mut rows: Vec<(Key, u32)> = Vec::new();
        for (key, mask) in sl.table.iter() {
            rows.extend(bits(mask).map(|c| (key, crate::storage::cells::cell(slot, c))));
        }
        rows.extend(sl.grown.iter().map(|g| (g.sk.key, crate::storage::cells::cell(slot, g.sk.local as u32))));
        if rows.is_empty() {
            continue;
        }
        let rt2 = super::decode::decode_rows(&visited.keys, &skel, &rows)?;
        let ok = f.allowed(&rt2, frame)?;
        let n_new = sl.table.states() as usize;
        if ok.iter().all(|&x| x) {
            continue;
        }
        let (mut keys, mut masks, mut hulls) = (Vec::new(), Vec::new(), Vec::new());
        let mut i = 0usize;
        for (key, mask) in sl.table.iter() {
            let mut kept = 0u64;
            for c in bits(mask) {
                if ok[i] {
                    kept |= 1u64 << c;
                    hulls.push(sl.hulls[i]);
                }
                i += 1;
            }
            if kept != 0 {
                keys.push(key);
                masks.push(kept);
            }
        }
        sl.table = super::set::PlainTable::from_sorted(&keys, &masks)?;
        sl.hulls = hulls;
        let mut k = n_new;
        sl.grown.retain(|_| {
            k += 1;
            ok[k - 1]
        });
    }
    Ok(())
}

/// A re-push unit's pushes by target (route, key): its cells' merged hulls,
/// a run of `slots` by cell (moved to the end, capacity doubled, when full).
struct CellAccs<N: Rem> {
    idx: rustc_hash2::FxHashMap<(u32, Key), u32>,
    /// Per target: (route, key, cells), (its run's start, its capacity).
    targets: Vec<(u32, Key, u64)>,
    runs: Vec<(u32, u32)>,
    slots: Vec<AccOf<N>>,
}

impl<N: Rem> Default for CellAccs<N> {
    fn default() -> Self {
        CellAccs { idx: Default::default(), targets: Vec::new(), runs: Vec::new(), slots: Vec::new() }
    }
}

impl<N: Rem> CellAccs<N> {
    fn clear(&mut self) {
        self.idx.clear();
        self.targets.clear();
        self.runs.clear();
        self.slots.clear();
    }

    /// Target (route, key)'s index (new: no cell yet).
    fn target(&mut self, route: u32, key: Key) -> u32 {
        let n = self.targets.len() as u32;
        let t = *self.idx.entry((route, key)).or_insert(n);
        if t == n {
            self.targets.push((route, key, 0));
            self.runs.push((self.slots.len() as u32, 0));
        }
        t
    }

    #[inline]
    fn merge(&mut self, t: u32, c: u32, a: &AccOf<N>) {
        let m = self.targets[t as usize].2;
        let (start, cap) = self.runs[t as usize];
        let rank = (m & ((1u64 << c) - 1)).count_ones();
        if m >> c & 1 != 0 {
            self.slots[(start + rank) as usize].merge(a);
            return;
        }
        let len = m.count_ones();
        let start = if len < cap {
            start
        } else {
            let at = self.slots.len() as u32;
            let cap = (cap * 2).clamp(4, 64);
            self.slots.extend_from_within(start as usize..(start + len) as usize);
            self.slots.resize((at + cap) as usize, AccOf::<N>::default());
            self.runs[t as usize] = (at, cap);
            at
        };
        let (s, r, l) = (start as usize, rank as usize, len as usize);
        self.slots.copy_within(s + r..s + l, s + r + 1);
        self.slots[s + r] = *a;
        self.targets[t as usize].2 = m | 1u64 << c;
    }
}

struct RepushScratch<N: Rem> {
    /// The segment decoder.
    dec: super::edges::Decoder,
    accs: CellAccs<N>,
    /// Per segment target its unit target (`u32::MAX`: none yet).
    t_of: Vec<u32>,
    /// Pushes by (hull, last pushed hull, transfer).
    cache: crate::search::hull::PushCache<N>,
    /// Segments read, their raw bytes, their edge records, the records whose
    /// source (shape, key) is an old state's (`S::RepushSegments` on).
    read: [u64; 4],
}

impl<N: Rem> Default for RepushScratch<N> {
    fn default() -> Self {
        RepushScratch { dec: Default::default(), accs: CellAccs::default(), t_of: Vec::new(), cache: Default::default(), read: [0; 4] }
    }
}

/// ONE RE-PUSH UNIT: segments `segs` (target region, its segment) of the
/// edge file of `layer`'s next frame `frame`, each read whole; their edges
/// from the layer's old states (`index`) push the grown hulls
/// (`Hull::push_grown`: only where an image grows), level -1 dropping as at
/// an emission. The pushes merge order-free, so it pushes what a read by
/// source would. Returns the edges looked at.
#[allow(clippy::too_many_arguments)]
#[inline(never)]
fn run_repush<N: Rem>(u: u32, layer: &OldLayer<N>, index: &rustc_hash2::FxHashMap<(u32, Key), Vec<(u32, u32)>>, files: &super::edges::EdgeFiles, frame: u32, segs: &[(u32, u32)], visited: &VisitedSet<N>, base: &XferTable<N>, sink: &mut UnitSink<'_, N>, sc: &mut RepushScratch<N>) -> Result<u64> {
    sink.begin(u);
    sc.accs.clear();
    let mut edges = 0u64;
    for &(region, k) in segs {
        let all = files.segments(frame, region);
        let seg = &all[k as usize];
        let blk = sc.dec.open(seg)?;
        sc.read[0] += 1;
        sc.read[1] += seg.raw as u64;
        let cands: Vec<Option<&[(u32, u32)]>> = blk.sources.iter().map(|&(sh, k)| index.get(&(sh, k)).map(Vec::as_slice)).collect();
        if cands.iter().all(Option::is_none) {
            continue;
        }
        let (t_shape, t_slot) = (visited.shape_hash(region / crate::storage::cells::SLOTS), region % crate::storage::cells::SLOTS);
        let none_slot = crate::storage::cells::NO_CELL_SLOT;
        // The route: asked at the first push (a route is one pushed into).
        let mut route = None;
        let dropped = (0..64u32).filter(|&c| (t_slot != none_slot || c == 0) && sink.minus_one_drop(t_shape, crate::storage::cells::cell(t_slot, c))).fold(0u64, |a, c| a | 1u64 << c);
        let (accs, cache, t_of) = (&mut sc.accs, &mut sc.cache, &mut sc.t_of);
        let (mut seen, mut used) = (0u64, 0u64);
        t_of.clear();
        t_of.resize(blk.t_keys.len(), u32::MAX);
        blk.records(|r| {
            seen += 1;
            let Some(cs) = cands[r.s as usize] else { return };
            used += 1;
            let shape = blk.sources[r.s as usize].0;
            super::bundle::edges(r.mask & !dropped, t_slot, r.o, |c, _, s_cell| {
                let (s_slot, s_local) = crate::storage::cells::slot_local(s_cell);
                let s_region = super::region_of(shape, s_slot);
                let Some(&(_, ei)) = cs.iter().find(|x| x.0 == s_region) else { return };
                let e = layer.entries[ei as usize];
                if e.cells >> s_local & 1 == 0 {
                    return;
                }
                edges += 1;
                let [hull, was] = &layer.hulls[e.first as usize + (e.cells & ((1u64 << s_local) - 1)).count_ones() as usize];
                let Some(acc) = cache.get(hull, was, r.x, || {
                    let xfer = N::xfer(&base.pairs[r.x as usize]);
                    let mut a = AccOf::<N>::default();
                    hull.push_grown(was, &xfer, &mut a).then_some(a)
                }) else {
                    return;
                };
                let t = match t_of[r.t as usize] {
                    u32::MAX => {
                        let route = *route.get_or_insert_with(|| sink.route(t_shape, t_slot));
                        let t = accs.target(route, blk.t_keys[r.t as usize]);
                        t_of[r.t as usize] = t;
                        t
                    }
                    t => t,
                };
                accs.merge(t, c, &acc);
            });
        })?;
        sc.read[2] += seen;
        sc.read[3] += used;
    }
    sink.repush_chunks(&sc.accs.targets, &sc.accs.runs, &sc.accs.slots);
    Ok(edges)
}

/// The freed pages of the frame's (exited) worker threads back to the OS
/// now: mimalloc reclaims an exited thread's segments lazily otherwise.
fn collect_freed() {
    // SAFETY: mimalloc's documented API, callable from any thread.
    unsafe { libmimalloc_sys::mi_collect(true) };
}

/// States decoded at a time at the frame's end (a chunk of a region's).
const WIN_CHUNK: u32 = 1 << 12;

/// The new layer's WINS: per entry of `next` (ascending) its win cells;
/// each decoded piece shown to `check` (`RowCheck::stored`).
fn layer_wins<N: Rem>(visited: &VisitedSet<N>, next: &Layer<N>, frame: u32, s: &crate::settings::Settings, check: Option<&dyn crate::frame::RowCheck<N>>) -> Result<Vec<(u32, u64)>> {
    let tables = next.set.tables();
    let starts = next.set.entry_starts();
    // Chunks of a table's entries: (table, entries).
    let mut chunks: Vec<(usize, std::ops::Range<u32>)> = Vec::new();
    for (ti, (_, t)) in tables.iter().enumerate() {
        let (mut a, mut n) = (0u32, 0u32);
        for (e, m) in t.masks().enumerate() {
            n += m.count_ones();
            if n >= WIN_CHUNK {
                chunks.push((ti, a..e as u32 + 1));
                (a, n) = (e as u32 + 1, 0);
            }
        }
        if (a as usize) < t.len() {
            chunks.push((ti, a..t.len() as u32));
        }
    }
    let parts = par_map(&chunks, s.threads(), |(ti, rg)| -> Result<Vec<(u32, u64)>> {
        let (region, t) = (tables[*ti].0, &tables[*ti].1);
        let shape = visited.shape_hash(region / crate::storage::cells::SLOTS);
        let skel = next.skeletons.get(&shape).ok_or_else(|| anyhow!("no skeleton of shape {shape:016x}"))?;
        let slot = region % crate::storage::cells::SLOTS;
        let mut rows: Vec<(Key, u32)> = Vec::with_capacity(WIN_CHUNK as usize + 64);
        let mut at: Vec<(u32, u32)> = Vec::with_capacity(WIN_CHUNK as usize + 64);
        for e in rg.clone() {
            let (key, mask) = (t.key(e), t.mask(e));
            for local in bits(mask) {
                rows.push((key, crate::storage::cells::cell(slot, local)));
                at.push((starts[*ti] as u32 + e, local));
            }
        }
        let rt2 = super::decode::decode_rows(&visited.keys, skel, &rows)?;
        if let Some(c) = check {
            let cells: Vec<u32> = rows.iter().map(|r| r.1).collect();
            c.stored(&rt2, &cells, &visited.keys).with_context(|| format!("frame {frame}: a piece of r{region}"))?;
        }
        let mut wins: Vec<(u32, u64)> = Vec::new();
        for (w, &(e, local)) in crate::frame::wins_of(&rt2, s)?.into_iter().zip(&at) {
            if w {
                match wins.last_mut() {
                    Some(l) if l.0 == e => l.1 |= 1u64 << local,
                    _ => wins.push((e, 1u64 << local)),
                }
            }
        }
        Ok(wins)
    });
    let mut out = Vec::new();
    for p in parts {
        out.extend(p?);
    }
    Ok(out)
}

/// A region job: one route's chunks of one slice (in unit order), its remaps.
struct Job<N: Rem> {
    state: std::sync::Arc<std::sync::Mutex<RouteState<N>>>,
    slice: u32,
    chunks: Vec<Chunk<N>>,
    remaps: std::sync::Arc<Vec<Vec<u32>>>,
}

/// The gather's: the new transfers (global ids `first` on), per worker its
/// merged length and local -> global ids, the routes made so far.
struct Gather<N: Rem> {
    fresh: XferTable<N>,
    first: u32,
    merged: Vec<usize>,
    remaps: Vec<Vec<u32>>,
    routes: usize,
}

/// The wave's pipeline: per slice its units left and chunks, the slices
/// gathered, the routes, the jobs (by slice, heaviest first within one).
struct Pipe<'a, N: Rem> {
    xtabs: &'a [std::sync::RwLock<Vec<Pair<N>>>],
    next_unit: AtomicUsize,
    left: Vec<AtomicUsize>,
    /// Per slice, per worker its transfer table's length after its units of
    /// the slice (the pairs it met there are below).
    upto: Vec<Vec<AtomicUsize>>,
    chunks: Vec<std::sync::Mutex<Vec<Chunk<N>>>>,
    gathered: AtomicUsize,
    gather: std::sync::Mutex<Gather<N>>,
    states: std::sync::RwLock<Vec<std::sync::Arc<std::sync::Mutex<RouteState<N>>>>>,
    jobs: std::sync::Mutex<std::collections::VecDeque<Job<N>>>,
    jobs_left: AtomicUsize,
    /// A worker failed (an error or a panic): the others stop, so the
    /// frame's error comes back instead of a wait that never ends.
    failed: std::sync::atomic::AtomicBool,
}

impl<N: Rem> Pipe<'_, N> {
    fn pop_job(&self) -> Option<Job<N>> {
        self.jobs.lock().expect("the jobs").pop_front()
    }

    /// Nothing to take now: spin a little, then sleep a little.
    fn wait(&self, spins: &mut u32) {
        *spins += 1;
        if *spins < 64 {
            std::hint::spin_loop();
        } else if *spins < 256 {
            std::thread::yield_now();
        } else {
            std::thread::sleep(std::time::Duration::from_micros(50));
        }
    }

    /// GATHER the complete slices in order, one thread at a time: global ids
    /// for the pairs first met in each (tables up to `upto`: a function of
    /// the slice), the new routes, its chunks by route as jobs.
    fn gather_ready(&self, n_slices: usize, routes: &Routes, visited: &VisitedSet<N>) -> Result<()> {
        let mut g = self.gather.lock().expect("the gather");
        loop {
            let k = self.gathered.load(Ordering::Acquire);
            if k >= n_slices || self.left[k].load(Ordering::Acquire) != 0 {
                return Ok(());
            }
            let mut chunks = std::mem::take(&mut *self.chunks[k].lock().expect("a slice's chunks"));
            let g = &mut *g;
            let upto: Vec<usize> = self.upto[k].iter().zip(&g.merged).map(|(u, &m)| u.load(Ordering::Acquire).max(m)).collect();
            let tabs: Vec<std::sync::RwLockReadGuard<Vec<Pair<N>>>> = (0..g.merged.len()).map(|w| self.xtab(w)).collect();
            let tails: Vec<&[Pair<N>]> = tabs.iter().enumerate().map(|(w, t)| &t[g.merged[w]..upto[w]]).collect();
            let first = g.first;
            for (w, ids) in g.fresh.merge(&tails).into_iter().enumerate() {
                g.remaps[w].extend(ids.into_iter().map(|i| first + i));
                g.merged[w] = upto[w];
            }
            drop(tails);
            drop(tabs);
            let remaps = std::sync::Arc::new(g.remaps.clone());
            {
                let mut states = self.states.write().expect("the routes");
                for (shape, slot) in routes.list().into_iter().skip(g.routes) {
                    let region = visited.region(shape, slot);
                    states.push(std::sync::Arc::new(std::sync::Mutex::new(RouteState::new(shape, slot, region))));
                }
                g.routes = states.len();
            }
            // One job a route: its chunks in unit order; heaviest first.
            chunks.sort_unstable_by_key(|c| std::cmp::Reverse((c.route, c.unit)));
            let mut jobs: Vec<(usize, u32, Vec<Chunk<N>>)> = Vec::new();
            while let Some(c) = chunks.pop() {
                match jobs.last_mut() {
                    Some(j) if j.1 == c.route => {
                        j.0 += c.bundles.len();
                        j.2.push(c);
                    }
                    _ => jobs.push((c.bundles.len(), c.route, vec![c])),
                }
            }
            jobs.sort_by_key(|j| (std::cmp::Reverse(j.0), j.1));
            let states = self.states.read().expect("the routes");
            let mut q = self.jobs.lock().expect("the jobs");
            self.jobs_left.fetch_add(jobs.len(), Ordering::AcqRel);
            for (_, r, chunks) in jobs {
                q.push_back(Job { state: states[r as usize].clone(), slice: k as u32, chunks, remaps: remaps.clone() });
            }
            drop(q);
            drop(states);
            self.gathered.store(k + 1, Ordering::Release);
        }
    }

    fn xtab(&self, w: usize) -> std::sync::RwLockReadGuard<'_, Vec<Pair<N>>> {
        self.xtabs[w].read().expect("a transfer table")
    }
}

/// PROVISIONAL keys made final (plans/architecture.md "Keys"): the missing
/// codes per (shape, field) SORTED and appended (new shapes first, by hash),
/// so the dictionaries are a function of the frame's new states.
fn finalize_keys<N: Rem>(visited: &mut VisitedSet<N>, states: &[RouteState<N>], prov: &Provisional<N>, adds: &mut KeyAdditions<N>) -> FxHashMap<Key, Key> {
    let mut keys: Vec<Key> = states.iter().flat_map(|r| r.prov_keys.iter().copied()).collect();
    keys.sort_unstable();
    keys.dedup();
    if keys.is_empty() {
        return FxHashMap::default();
    }
    let contents: Vec<MissKey<N>> = keys.iter().map(|&k| prov.content(k)).collect();
    let mut new_shapes: Vec<u64> = contents.iter().map(|m| m.shape).filter(|&s| visited.keys.shape(s).is_none()).collect();
    new_shapes.sort_unstable();
    new_shapes.dedup();
    for s in new_shapes {
        let r = prov.shape_record(s).unwrap_or_else(|| panic!("exact keys: the new shape {s:#x} was not noted"));
        visited.keys.add_shape(&r, adds);
    }
    visited.keys.add_misses(&contents, adds);
    keys.iter().zip(&contents).map(|(&k, m)| (k, visited.keys.complete(m))).collect()
}

/// `f` over `items` on `threads` threads, in `items`' order.
pub(crate) fn par_map<T: Sync, R: Send>(items: &[T], threads: usize, f: impl Fn(&T) -> R + Sync) -> Vec<R> {
    crate::par::Pool::new(threads).map(items.len(), |i| Ok(f(&items[i]))).expect("an infallible map")
}

/// Frame 0: the initial states with the hull `start`, each KEYED AS IT IS,
/// not by its boundary view (`Rt2::widen_to`): where a `Flag::Always` entry
/// moves the view, a row keyed by it decodes to the view - the start stored
/// lossy, declined by the kernels' pins (the start's constants,
/// `trace::walk`) - and nothing looks the start up by projection
/// (`backward::Seeds::load`, `known::Pruning::start`). Each row must decode
/// to itself (`check_piece`).
pub(crate) fn seed<N: Rem>(visited: &mut VisitedSet<N>, blocks: &[Rt2<N>], start: HullOf<N>, s: &crate::settings::Settings) -> Result<Wave<N>> {
    let mut canon: Vec<celeste_engine::runtime2::Rt2<N>> = Vec::with_capacity(blocks.len());
    for b in blocks {
        let mut r = b.clone_block();
        r.canonical();
        canon.push(r);
    }
    let keys_added = visited.keys.absorb(&canon.iter().collect::<Vec<_>>());
    let mut shapes: Vec<u64> = canon.iter().map(|r| r.shape_hash).filter(|s| visited.shape_index(*s).is_none()).collect();
    shapes.sort_unstable();
    shapes.dedup();
    let mut numbered = Vec::new();
    for h in shapes {
        let i = visited.add_shape(h);
        numbered.push((i, h));
    }
    // Every row's state, checked to decode to the row; its win.
    let mut states: Vec<(Sk, bool)> = Vec::new();
    let mut skels: FxHashMap<u64, super::layer::SkelRecord<N>> = FxHashMap::default();
    for r in canon.iter_mut() {
        let keys: Vec<Key> = visited.keys.lookup_keys(r).into_iter().map(|k| k.expect("seeded codes")).collect();
        let cells = crate::storage::cells::block_cells(r)?;
        r.row_keys = keys.clone();
        super::decode::check_piece(&visited.keys, r, &cells).map_err(|e| e.context("frame 0"))?;
        let rec = super::layer::SkelRecord::of(r)?;
        ensure!(skels.get(&r.shape_hash).is_none_or(|x| *x == rec), "two initial blocks of shape {:016x} with other skeletons", r.shape_hash);
        skels.insert(r.shape_hash, rec);
        let si = visited.shape_index(r.shape_hash).expect("numbered above");
        for ((&k, &c), w) in keys.iter().zip(&cells).zip(crate::frame::wins_of(r, s)?) {
            states.push((Sk::at(si, k, c), w));
        }
    }
    states.sort_unstable();
    ensure!(states.windows(2).all(|w| w[0].0 != w[1].0), "the initial states hold one state twice");
    let mut next = Layer::<N>::default();
    let mut wins: Vec<(u32, u64)> = Vec::new();
    let mut i = 0;
    while i < states.len() {
        let region = states[i].0.region;
        let j = i + states[i..].iter().take_while(|x| x.0.region == region).count();
        let (mut keys, mut masks): (Vec<Key>, Vec<u64>) = (Vec::new(), Vec::new());
        for &(sk, w) in &states[i..j] {
            if keys.last() != Some(&sk.key) {
                keys.push(sk.key);
                masks.push(0);
            }
            *masks.last_mut().expect("pushed") |= 1u64 << sk.local;
            if w {
                let e = (next.entries() + keys.len() - 1) as u32;
                match wins.last_mut() {
                    Some(l) if l.0 == e => l.1 |= 1u64 << sk.local,
                    _ => wins.push((e, 1u64 << sk.local)),
                }
            }
        }
        next.push_region(region, &keys, &masks)?;
        i = j;
    }
    let hulls = vec![start; next.states() as usize];
    let mut parts: Vec<super::visited::Part<N, Old<N>>> = Vec::new();
    let mut at = 0usize;
    for (region, t) in next.set.tables() {
        let n = t.states() as usize;
        parts.push(super::visited::Part { region: *region, new: t, hulls: &hulls[at..at + n], grown: &[], layer: 0 });
        at += n;
    }
    let held = visited.absorb(&parts, s.threads())?.held_new;
    drop(parts);
    ensure!(held == 0, "frame 0 into a visited set that held {held} of its states");
    for (h, rec) in skels {
        next.skeletons.insert(h, std::sync::Arc::new(rec.rt2(canon[0].cache.clone())));
    }
    let n = next.states() as usize;
    let mut stats = Stats::default();
    for (k, v) in [(S::Kept, n as u64), (S::LanesOut, n as u64), (S::EntriesOut, next.entries() as u64)] {
        stats.set(k, v);
    }
    Ok(Wave { frontier: Frontier { new: next, hulls, old: Vec::new() }, wins, stats, shapes: numbered, keys: keys_added })
}

/// ONE FRAME of `rows` (widened to the engine's level) against an EMPTY
/// tree: every successor new, a row's own too (the seeds are forgotten once
/// keyed); the whole circle as their hulls. The successors, with their
/// keys, and the frame's stats.
pub fn one_frame<N: Rem>(engine: &dyn FrameStep<N>, rows: Vec<Rt2<N>>, frame: u32) -> Result<(Vec<Rt2<N>>, Stats)> {
    let mut visited = VisitedSet::<N>::new();
    let mut xfers = XferTable::<N>::default();
    let skeletons = Skeletons::<N>::default();
    let s = engine.settings();
    let start = seed(&mut visited, &rows, HullOf::<N>::full(), &s)?.frontier;
    visited.forget_states();
    let cx = WaveCtx { visited: &mut visited, xfers: &mut xfers, filters: Filters::default(), frame, edges_dir: None, edges: None, skeletons: &skeletons, check: None };
    let wave = run_wave(engine, start, cx)?;
    let rows = super::layer::decode_states(&wave.frontier.new, &visited.keys, visited.shapes(), usize::MAX, &|_| true)?.into_iter().map(|(_, rt2)| rt2).collect();
    Ok((rows, wave.stats))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::search::hull::Hull;
    use crate::settings::Settings;

    /// FRAME 0 IS THE ENGINE'S START, also where its view is another row: a
    /// strawberry's bob (room (2,0) 100%), a key's sprite (room (0,3) gemskip
    /// 100%, J 1), a balloon's phase (room (7,0), J 1).
    #[test]
    fn frame_zero_is_the_start_where_its_view_is_not() {
        let cases = [
            Settings { room: (2, 0), category: crate::settings::Category::Hundred, ..Default::default() },
            Settings { room: (0, 3), category: crate::settings::Category::Gemskip100, loading_jank: Some(1), ..Default::default() },
            Settings { room: (7, 0), loading_jank: Some(1), ..Default::default() },
        ];
        for s in cases {
            let start = crate::engines::reference::RefEngine::new(&s).expect("engine").initial().expect("start").canonicalized();
            let mut view = start.clone_block();
            view.widen_to(crate::abstraction::Level::EXACT);
            view.canonical();
            assert_ne!(celeste_engine::exact::exact_rows(&view), celeste_engine::exact::exact_rows(&start), "room {:?}: the start is its own view", s.room);
            let mut visited = VisitedSet::new();
            let wave = seed(&mut visited, std::slice::from_ref(&start), Hull::full(), &s).unwrap_or_else(|e| panic!("room {:?}: frame 0: {e:#}", s.room));
            let decoded = super::super::layer::decode_states(&wave.frontier.new, &visited.keys, visited.shapes(), usize::MAX, &|_| true).expect("decode");
            let [(sks, row)] = &decoded[..] else { panic!("room {:?}: frame 0 is {} blocks", s.room, decoded.len()) };
            assert_eq!(sks.len(), 1, "room {:?}: frame 0 is one state", s.room);
            assert_eq!(
                (celeste_engine::exact::exact_rows(row), sks[0].cell()),
                (celeste_engine::exact::exact_rows(&start), crate::storage::cells::block_cells(&start).expect("cell")[0]),
                "room {:?}: frame 0 is not the start",
                s.room
            );
        }
    }

    /// An `OldLayer` gives back its states, whole and by range.
    #[test]
    fn an_old_layer_gives_back_its_states() {
        let mut v = 7u64;
        let mut next = move || crate::testutil::lcg(&mut v) >> 11;
        let hull = |x: u64| Hull::from_bytes(&(x & 0x7fff_7fff_7fff_7fff).to_le_bytes());
        let mut olds: Vec<Old> = (0..60_000)
            .map(|_| {
                let (r, k, c) = (next() % 5, next() % 9000, next() % 64);
                Old { sk: Sk { region: r as u32, key: (0, k), local: c as u8 }, layer: 9, hull: hull(next()), was: hull(next()) }
            })
            .collect();
        olds.sort_unstable_by_key(|o| o.sk);
        olds.dedup_by_key(|o| o.sk);
        let mut l = OldLayer::new(9, olds.len());
        for o in &olds {
            l.push(o);
        }
        assert_eq!(l.len(), olds.len());
        assert!(l.entries.len() < olds.len(), "entries of several states");
        assert_eq!(l.iter().collect::<Vec<_>>(), olds);
        for r in [0..0, 0..1, 3..4, 100..2000, 17..olds.len(), olds.len() - 1..olds.len()] {
            assert_eq!(l.range(r.clone()).collect::<Vec<_>>(), olds[r.clone()], "range {r:?}");
        }
    }

    /// EXACT PLATFORMS ARE KEYED BY THE FIRST ONE (`widening::CLOCKS`):
    /// room (6,0)'s ten platforms declare the first one's `spd.x` the
    /// leader of the other `spd.x`s and its `rem.x` of every other platform
    /// cell, and frame 0 seeds and decodes with them followers; in room
    /// (2,1) (one platform) nothing is declared.
    #[test]
    fn exact_platforms_are_keyed_by_the_first() {
        use celeste_engine::widening::key_deps;
        let start_of = |s: &Settings| crate::engines::reference::RefEngine::new(s).expect("engine").initial().expect("start").canonicalized();
        let s = Settings { room: (6, 0), split_frame: true, ..Default::default() };
        let start = start_of(&s);
        let platforms = start.objects_of_type(celeste_names::global("platform"));
        assert_eq!(platforms.len(), 10, "room (6,0)'s platforms");
        let deps = key_deps(&start);
        let [speed, rest] = &deps[..] else { panic!("ten exact platforms: two dependencies, {deps:?}") };
        assert_eq!(speed.followers.len(), 9, "the first platform's `spd.x` leads the other nine");
        assert!(rest.followers.len() >= 10 * 3, "every platform's `x`, `rem.x` and `last` at least: {rest:?}");
        let mut visited = VisitedSet::new();
        seed(&mut visited, std::slice::from_ref(&start), Hull::full(), &s).expect("frame 0 decodes with its followers");
        let sk = visited.keys.shape(start.shape_hash).expect("the start's shape");
        assert_eq!(sk.follows().iter().map(|f| f.cells().to_vec()).collect::<Vec<_>>(), deps.iter().map(|d| d.followers.clone()).collect::<Vec<_>>());
        assert!(deps.iter().flat_map(|d| &d.followers).all(|&c| sk.field(c as usize).is_none()), "no follower is keyed");
        let s21 = Settings { room: (2, 1), ..Default::default() };
        assert!(key_deps(&start_of(&s21)).is_empty(), "one platform: nothing to declare");
    }
}
