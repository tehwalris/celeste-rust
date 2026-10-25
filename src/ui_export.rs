//! `rewrite export-ui`: a finished search's record (`result.json`), its
//! trees and marks -> the compact static data the web UI (`ui/`) renders.
//!
//! Per (level, frame) the states and wins per player cell; per marked set
//! (the search's MARKS RECORD, `search::result::marks_path`: W's support,
//! arc-marked nodes) the marked states per cell by distance to the win; the
//! witness and its path from the record. All from the frame files' region
//! blocks and the marks files: no row is decoded, no engine built (the
//! record is the search's: rerunning the backward over a big tree costs
//! what its arc phase did, e.g. 1,828 s and 25-83 GB over a 421 GB edge
//! store).
//!
//! STREAMED region by region (`read_tree`): a marks file is mapped, never
//! loaded; sorted by state, so a region's marked states are a merge of its
//! keys with the rows at its cells (`count_marked`), which no other region
//! reads (frame by frame would map the whole file). An 82 GB tree with 431M
//! marks peaks at 1.6 GB of heap, where loading it all took 96 GB.
//!
//! One horizon whose levels are the search's, plus with a marks record a
//! level for the arc backward over the last tree (`forward_of`). JSON fields `bwd`,
//! `reruns` and the horizons list are kept only because the UI reads them.
//!
//! Output layout (`--out DIR`):
//!
//! * `run.json` - the run: room, the cell box every binary indexes into,
//!   the room's tiles, per level the per-frame forward stats and the names of
//!   the level's binary files, the concrete paths and the download. A
//!   function of the search alone: its wall times are in
//! * `timings.json` - the run's wall time and, per forward line, its walls,
//!   idle shares and memory (`Timings`, in run.json's nesting; the UI merges
//!   them back).
//! * `l00.frames.bin` - level 0's frames; `hHHH_lLL.frames.bin` for every
//!   finer level. Format: `"CUF1" | u32 nframes | nframes x (u32 cells_off,
//!   u32 ncells, u32 wins_off, u32 nwins) | data`, where a cells record at
//!   `cells_off` (bytes from file start) is `ncells x u32 idx` followed by
//!   `ncells x u32 count`, and a wins record likewise; `idx` is
//!   `(x - box.x0) + (y - box.y0) * box.w`, or `NO_POSITION` (u32::MAX)
//!   for a state whose player object is gone. Frame f is entry f.
//! * `hHHH_lLL.marks.bin` - a marked set: `"CUM1" | u32 n | n x u32 idx |
//!   n x u32 dist | n x u32 count`, sorted by (dist, idx). `dist` is the
//!   horizon minus the last frame the state still wins from.
//! * `hHHH_lLL.mlayers.bin` - the same set by the FRAME each state was
//!   first reached at: the frames format (magic `"CUL1"`, no wins).
//!
//! Everything is little-endian u32 at 4-byte alignment, so the UI reads
//! it as `Uint32Array` views without a parser.

use anyhow::{bail, Context, Result};
use rustc_hash::FxHashMap;
use serde::Serialize;
use std::io::Write;
use std::path::Path;

use celeste_engine::exact::Key;

use crate::storage::cells::cell_xy;
use crate::storage::layer::{bits, last_frame, Tree};

/// One forward frame's stats: a `forward.tsv` row under the UI's names;
/// its wall times go to `timings.json`.
#[derive(Serialize, Clone, Debug)]
pub(crate) struct FwdLine {
    pub(crate) f: u32,
    pub(crate) in_entries: u64,
    pub(crate) in_lanes: u64,
    pub(crate) raw: u64,
    pub(crate) kept: u64,
    pub(crate) out_entries: u64,
    pub(crate) out_lanes: u64,
    pub(crate) visited: u64,
    #[serde(skip)]
    pub(crate) times: FwdTimes,
}

/// A forward line's wall times and memory (`timings.json`).
#[derive(Serialize, Clone, Debug, Default)]
pub(crate) struct FwdTimes {
    pub(crate) emit_ms: u64,
    pub(crate) emit_idle: u32,
    pub(crate) own_ms: u64,
    pub(crate) own_idle: u32,
    pub(crate) ckpt_ms: u64,
    pub(crate) total_ms: u64,
    pub(crate) rss_gb: f64,
}

#[derive(Serialize, Clone, Debug)]
pub(crate) struct LevelRun {
    pub(crate) level: usize,
    /// The level's spec (`r0sxn`), or the arc pass's name.
    pub(crate) precision: String,
    pub(crate) fwd: Vec<FwdLine>,
    pub(crate) bwd: [u32; 0],
    pub(crate) first_win: Option<u32>,
    pub(crate) marked: Option<u64>,
    pub(crate) reruns: Option<u64>,
    pub(crate) refuted: bool,
    /// Frames in the tree (f0..frames-1).
    pub(crate) frames: u32,
    pub(crate) frames_file: String,
    pub(crate) marks_file: Option<String>,
    pub(crate) frame_states: Vec<u64>,
    pub(crate) frame_wins: Vec<u64>,
    pub(crate) marks_have_dist: bool,
    pub(crate) marks_by_dist: Vec<u64>,
    pub(crate) mlayers_file: Option<String>,
    pub(crate) marks_by_layer: Vec<u64>,
    /// A backward over ANOTHER level's forward (the marks record): that level's
    /// index, whose frames it shares; no forward pass of its own.
    pub(crate) forward_of: Option<usize>,
}

#[derive(Serialize, Clone, Debug)]
pub(crate) struct HorizonRun {
    pub(crate) h: u32,
    pub(crate) levels: Vec<LevelRun>,
    pub(crate) refuted_at: Option<usize>,
    /// No backward (a search without its marks record): frames up to `h`,
    /// no verdict.
    pub(crate) partial: bool,
}

#[derive(Serialize, Clone, Debug)]
pub(crate) struct Box2 {
    pub(crate) x0: i32,
    pub(crate) y0: i32,
    pub(crate) w: u32,
    pub(crate) h: u32,
}

#[derive(Serialize, Clone, Debug)]
pub(crate) struct Tiles {
    /// `w x h` tiles of 8 px from the start room's origin, row-major.
    pub(crate) w: u32,
    pub(crate) h: u32,
    pub(crate) ids: Vec<u8>,
    pub(crate) solid: Vec<bool>,
}

#[derive(Serialize, Debug)]
pub(crate) struct Run {
    pub(crate) format: u32,
    pub(crate) room: (i16, i16),
    pub(crate) cell_box: Box2,
    pub(crate) tiles: Tiles,
    pub(crate) prebuild_s: Option<f64>,
    pub(crate) optimal: Option<u32>,
    pub(crate) horizons: Vec<HorizonRun>,
    /// Drawn over the room: the search's witness (`result.json`), or
    /// `--witness FILE`.
    pub(crate) witness: Option<Witness>,
    /// `--reference FILE`: e.g. the community TAS in the original cart.
    pub(crate) reference: Option<Witness>,
    /// The file the UI offers for download (`--download FILE`), verbatim.
    pub(crate) download: Option<Download>,
}

#[derive(Serialize, Debug)]
pub(crate) struct Download {
    pub(crate) file: String,
    pub(crate) text: String,
}

/// A concrete run: its inputs, the player's (x, y) per frame from the start
/// (`None` without a player) in the cells' pixels, and the frames a dash
/// starts at with its direction (`R`, `U`, ...; when the file lists them).
#[derive(Serialize, Debug)]
pub(crate) struct Witness {
    pub(crate) label: String,
    pub(crate) inputs: Vec<u8>,
    pub(crate) path: Vec<Option<(i32, i32)>>,
    pub(crate) dashes: Vec<(u32, String)>,
}

fn level_run(level: usize, precision: String, fwd: Vec<FwdLine>, first_win: Option<u32>) -> LevelRun {
    LevelRun {
        level,
        precision,
        fwd,
        bwd: [],
        first_win,
        marked: None,
        reruns: None,
        refuted: false,
        frames: 0,
        frames_file: String::new(),
        marks_file: None,
        frame_states: Vec::new(),
        frame_wins: Vec::new(),
        marks_have_dist: true,
        marks_by_dist: Vec::new(),
        mlayers_file: None,
        marks_by_layer: Vec::new(),
        forward_of: None,
    }
}

/// A tree's forward record (`frame::FORWARD_STATS`) by frame, a frame's
/// last row winning; empty without one.
fn read_forward_stats(dir: &Path) -> Result<Vec<FwdLine>> {
    let path = dir.join(crate::frame::FORWARD_STATS);
    let text = match std::fs::read_to_string(&path) {
        Ok(t) => t,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(e).with_context(|| format!("reading {}", path.display())),
    };
    let mut lines = text.lines();
    let header: Vec<&str> = lines.next().unwrap_or("").split('\t').collect();
    let col = |name: &str| header.iter().position(|h| *h == name).with_context(|| format!("{}: no column {name:?}", path.display()));
    let c: Vec<usize> = ["frame", "entries_in", "lanes_in", "lanes_raw", "lanes_kept", "entries_out", "lanes_out", "visited", "wave_ms", "wave_idle_pct", "layer_ms", "ckpt_ms", "total_ms", "rss_end_gb"]
        .iter()
        .map(|n| col(n))
        .collect::<Result<_>>()?;
    let mut by_frame: std::collections::BTreeMap<u32, FwdLine> = Default::default();
    for (ln, line) in lines.enumerate() {
        let t: Vec<&str> = line.split('\t').collect();
        anyhow::ensure!(t.len() == header.len(), "{} line {}: {} fields, the header {}", path.display(), ln + 2, t.len(), header.len());
        let n = |k: usize| -> Result<u64> { t[c[k]].parse().with_context(|| format!("{} line {}: {:?}", path.display(), ln + 2, t[c[k]])) };
        let x = |k: usize| -> Result<f64> { t[c[k]].parse().with_context(|| format!("{} line {}: {:?}", path.display(), ln + 2, t[c[k]])) };
        let f = n(0)? as u32;
        by_frame.insert(
            f,
            FwdLine {
                f,
                in_entries: n(1)?,
                in_lanes: n(2)?,
                raw: n(3)?,
                kept: n(4)?,
                out_entries: n(5)?,
                out_lanes: n(6)?,
                visited: n(7)?,
                times: FwdTimes {
                    emit_ms: n(8)?,
                    emit_idle: n(9)? as u32,
                    // The storage's own wall time: the frame's end (its
                    // region phase runs inside the wave).
                    own_ms: n(10)?,
                    own_idle: 0,
                    ckpt_ms: n(11)?,
                    total_ms: n(12)?,
                    rss_gb: x(13)?,
                },
            },
        );
    }
    Ok(by_frame.into_values().collect())
}

/// Sparse per-cell counts of one frame: `(cell, count)` ascending by cell.
type Sparse = Vec<(u32, u32)>;

struct LevelData {
    /// Per frame: the states per cell, the wins per cell.
    frames: Vec<(Sparse, Sparse)>,
}

/// One marked state `(shape, cell, key)`, as the frame rows are keyed.
type MarkId = (u64, u32, Key);

/// A marks file, mapped, read in place: `save_marks`' rows `(shape, cell,
/// k0, k1, dist)` (32 B each, SORTED by state), then the horizon.
struct MarksMap {
    map: crate::storage::file::Mapped,
    rows: usize,
}

/// Bytes of the value before the first row: the bincode `Vec` length.
const MARKS_ROWS_AT: usize = 8;

impl MarksMap {
    /// The marks file at `path`, in arithmetic `arith` (its tree's: a file
    /// of another is refused).
    fn open(path: &Path, arith: crate::settings::Arith) -> Result<Self> {
        let map = crate::with_arith!(arith, N => crate::storage::file::open_value::<N>(path))?;
        let payload = map.header_bytes();
        anyhow::ensure!(payload.len() >= 8, "{}: too short for a marks file", path.display());
        let n = u64::from_le_bytes(payload[0..8].try_into().unwrap());
        let body = payload.len() as u64 - 8;
        // The rows, the horizon, then the tree's key space.
        anyhow::ensure!(n.checked_mul(32).and_then(|b| b.checked_add(4)).is_some_and(|b| b <= body), "{}: {n} rows do not fit {body} payload bytes", path.display());
        Ok(MarksMap { map, rows: n as usize })
    }

    fn u64_at(&self, i: usize, o: usize) -> u64 {
        let a = MARKS_ROWS_AT + i * 32 + o;
        u64::from_le_bytes(self.map.header_bytes()[a..a + 8].try_into().unwrap())
    }

    fn u32_at(&self, i: usize, o: usize) -> u32 {
        let a = MARKS_ROWS_AT + i * 32 + o;
        u32::from_le_bytes(self.map.header_bytes()[a..a + 4].try_into().unwrap())
    }

    fn at(&self, i: usize) -> (u64, u32) {
        (self.u64_at(i, 0), self.u32_at(i, 8))
    }

    fn key(&self, i: usize) -> Key {
        (self.u64_at(i, 12), self.u64_at(i, 20))
    }

    fn id(&self, i: usize) -> MarkId {
        let (shape, cell) = self.at(i);
        (shape, cell, self.key(i))
    }

    fn dist(&self, i: usize) -> u32 {
        self.u32_at(i, 28)
    }

    /// Unmap the resident pages of rows `a..b` (the page cache keeps them),
    /// so a scan does not hold what it has read.
    fn release(&self, a: usize, b: usize) -> Result<()> {
        self.map.release(&self.map.header_bytes()[MARKS_ROWS_AT + a * 32..MARKS_ROWS_AT + b * 32])
    }
}

/// Rows of a marks file per chunk of its parallel scan (the tests' files
/// are cut every few rows).
const MARKS_CHUNK: usize = if cfg!(test) { 3 } else { 1 << 20 };

/// A marked set, SCANNED: its marks file and each `(shape, cell)`'s rows.
struct MarkedSet {
    m: MarksMap,
    cells: FxHashMap<(u64, u32), (usize, usize)>,
}

/// A marks file scanned in parallel chunks: the set and its `(cell, dist,
/// count)` by (dist, cell). Refuses rows not strictly ascending by state
/// (the order `count_marked`'s merge needs).
fn scan_marks(m: MarksMap, path: &Path, threads: usize) -> Result<(MarkedSet, Vec<(u32, u32, u32)>)> {
    let parts = crate::par::Pool::new(threads).map(m.rows.div_ceil(MARKS_CHUNK), |c| {
        let (a, b) = (c * MARKS_CHUNK, ((c + 1) * MARKS_CHUNK).min(m.rows));
        let mut hist: FxHashMap<(u32, u32), u32> = FxHashMap::default();
        // The chunk's runs of one (shape, cell): (shape, cell), start, end.
        let mut runs: Vec<((u64, u32), usize, usize)> = Vec::new();
        let mut prev = a.checked_sub(1).map(|p| m.id(p));
        for r in a..b {
            let id = m.id(r);
            anyhow::ensure!(prev < Some(id), "{}: row {r} is not after row {} (a marks file is sorted by state, each state once)", path.display(), r.wrapping_sub(1));
            prev = Some(id);
            *hist.entry((m.dist(r), id.1)).or_default() += 1;
            match runs.last_mut() {
                Some(run) if run.0 == (id.0, id.1) => run.2 = r + 1,
                _ => runs.push(((id.0, id.1), r, r + 1)),
            }
        }
        m.release(a, b)?;
        Ok((hist, runs))
    })?;
    let mut hist: FxHashMap<(u32, u32), u32> = FxHashMap::default();
    let mut cells: FxHashMap<(u64, u32), (usize, usize)> = FxHashMap::default();
    for (h, runs) in parts {
        for (dc, n) in h {
            *hist.entry(dc).or_default() += n;
        }
        // A run cut by the chunks continues the previous chunk's last.
        for (at, a, b) in runs {
            cells.entry(at).and_modify(|r| r.1 = b).or_insert((a, b));
        }
    }
    let mut v: Vec<(u32, u32, u32)> = hist.into_iter().map(|((d, c), n)| (c, d, n)).collect();
    v.sort_unstable_by_key(|&(c, d, _)| (d, c));
    Ok((MarkedSet { m, cells }, v))
}

/// The first `i` in `lo..hi` with `!before(i)`, else `hi` (`before` true on
/// a prefix): probes `lo`, `lo + 1`, `lo + 3`, ... then halves, so a
/// forward cursor pays the log of its step, not of the range.
fn gallop(lo: usize, hi: usize, before: impl Fn(usize) -> bool) -> usize {
    let (mut a, mut step) = (lo, 1usize);
    let mut b = loop {
        let p = a + (step - 1);
        if p >= hi {
            break hi;
        }
        if !before(p) {
            break p;
        }
        a = p + 1;
        step *= 2;
    };
    while a < b {
        let mid = a + (b - a) / 2;
        if before(mid) {
            a = mid + 1;
        } else {
            b = mid;
        }
    }
    a
}

/// A marked set's rows at each local cell of one storage region: `lo..hi`
/// per local (empty where none), `live` the locals with rows.
struct CellRows {
    lo: [usize; 64],
    hi: [usize; 64],
    live: u64,
}

impl CellRows {
    /// The rows of `shape` at the cells `locals` of region slot `slot`.
    fn of(set: &MarkedSet, shape: u64, slot: u32, locals: u64) -> CellRows {
        let mut out = CellRows { lo: [0; 64], hi: [0; 64], live: 0 };
        for local in bits(locals) {
            if let Some(&(a, b)) = set.cells.get(&(shape, crate::storage::cells::cell(slot, local))) {
                (out.lo[local as usize], out.hi[local as usize]) = (a, b);
                out.live |= 1 << local;
            }
        }
        out
    }

    /// Unmap the rows' pages (a region's cells are its own: no other region
    /// reads them).
    fn release(&self, m: &MarksMap) -> Result<()> {
        for l in bits(self.live) {
            m.release(self.lo[l as usize], self.hi[l as usize])?;
        }
        Ok(())
    }
}

/// Per local cell, the states of one region block a marked set holds: a
/// MERGE of the entries with `rows`, both ascending by key, galloping.
fn count_marked(m: &MarksMap, rows: &CellRows, keys: &[Key], masks: &[u64]) -> [u32; 64] {
    let mut out = [0u32; 64];
    let (mut lo, hi, mut live) = (rows.lo, &rows.hi, rows.live);
    for (key, &mask) in keys.iter().zip(masks) {
        if live == 0 {
            break;
        }
        for l in crate::storage::layer::bits(mask & live) {
            let l = l as usize;
            let i = gallop(lo[l], hi[l], |i| m.key(i) < *key);
            lo[l] = i;
            if i < hi[l] && m.key(i) == *key {
                out[l] += 1;
                lo[l] = i + 1;
            }
            if lo[l] == hi[l] {
                live &= !(1 << l);
            }
        }
    }
    out
}

/// One frame's per-cell sums: states, wins, per marked set its marked states.
struct FrameSums {
    cells: FxHashMap<u32, u64>,
    wins: FxHashMap<u32, u64>,
    marked: Vec<FxHashMap<u32, u64>>,
}

/// A storage region: its states over all frames, its blocks `(frame, block)`.
struct RegionBlocks {
    region: u32,
    states: u64,
    blocks: Vec<(usize, usize)>,
}

/// One storage region counted into `sums`, frame by frame: each block
/// decoded, merged with the marked sets' rows at its cells, and unmapped.
/// Does the region hold a win?
fn count_region<N: celeste_core::arith::Arithmetic>(tree: &Tree<N>, rb: &RegionBlocks, sets: &[MarkedSet], sums: &[std::sync::Mutex<FrameSums>]) -> Result<bool> {
    let (shape, slot) = (tree.shape_of(rb.region)?, rb.region % crate::storage::cells::SLOTS);
    let locals = if slot == crate::storage::cells::NO_CELL_SLOT { 1 } else { u64::MAX };
    let rows: Vec<CellRows> = sets.iter().map(|m| CellRows::of(m, shape, slot, locals)).collect();
    let mut won = false;
    for &(f, bi) in &rb.blocks {
        let file = tree.frames[f].as_ref().expect("a frame with a region block");
        let e = file.region(bi).with_context(|| format!("{} region {}", file.path().display(), rb.region))?;
        file.release_region(bi)?;
        let (mut states, mut wins, mut held) = ([0u32; 64], [0u32; 64], 0u64);
        won |= !e.wins.is_empty();
        for &m in &e.masks {
            held |= m;
            for local in bits(m) {
                states[local as usize] += 1;
            }
        }
        anyhow::ensure!(held & !locals == 0, "{}: region {} (no player): a cell past 0", file.path().display(), rb.region);
        for &(_, m) in &e.wins {
            for local in bits(m) {
                wins[local as usize] += 1;
            }
        }
        let marked: Vec<[u32; 64]> = sets.iter().zip(&rows).map(|(m, r)| count_marked(&m.m, r, &e.keys, &e.masks)).collect();
        let add = |to: &mut FxHashMap<u32, u64>, a: &[u32; 64]| {
            for l in bits(held).filter(|&l| a[l as usize] > 0) {
                *to.entry(crate::storage::cells::cell(slot, l)).or_default() += a[l as usize] as u64;
            }
        };
        let mut s = sums[f].lock().unwrap();
        add(&mut s.cells, &states);
        add(&mut s.wins, &wins);
        for (to, a) in s.marked.iter_mut().zip(&marked) {
            add(to, a);
        }
    }
    for (m, r) in sets.iter().zip(&rows) {
        r.release(&m.m)?;
    }
    Ok(won)
}

/// Per cell of the regions `regions` (the win states'), the cells its
/// recorded in-edges leave from, over every frame of the tree in `dir`
/// through `last`; sorted.
fn sources_into<N: crate::search::rem::Rem>(dir: &Path, last: u32, regions: &[u32], threads: usize) -> Result<FxHashMap<u32, Vec<u32>>> {
    let store = crate::storage::edges::EdgeStore::<N>::open(&dir, last)?;
    let frames: Vec<u32> = (1..=last).collect();
    let parts = crate::par::Pool::new(threads).map_with(
        frames.len(),
        || Ok(crate::storage::edges::Decoder::default()),
        |dec, k| {
            let mut pairs: Vec<(u32, u32)> = Vec::new();
            for &r in regions {
                store.bundles_into(frames[k], r, dec, |b| crate::storage::edges::expand(r, &b, |src, dst| pairs.push((dst.cell(), src.cell()))))?;
            }
            Ok(pairs)
        },
    )?;
    let mut by: FxHashMap<u32, Vec<u32>> = FxHashMap::default();
    for (dst, src) in parts.into_iter().flatten() {
        by.entry(dst).or_default().push(src);
    }
    for v in by.values_mut() {
        v.sort_unstable();
        v.dedup();
    }
    Ok(by)
}

/// Per-cell sums `(cell, n)` ascending; a count past u32 is an error.
fn sparse(m: FxHashMap<u32, u64>) -> Result<Sparse> {
    let mut v: Sparse = m.into_iter().map(|(c, n)| Ok((c, u32::try_from(n).with_context(|| format!("{n} states at one cell of one frame"))?))).collect::<Result<_>>()?;
    v.sort_unstable();
    Ok(v)
}

/// One level directory, the largest region first: per frame its states and
/// wins per cell, and `[k]`: per frame, set `k`'s marked states per cell.
fn read_tree<N: crate::search::rem::Rem>(dir: &Path, sets: &[MarkedSet], threads: usize) -> Result<(LevelData, Vec<Vec<Sparse>>)> {
    let n = last_frame(dir).map_or(0, |l| l as usize + 1);
    // Per win cell the cells its in-edges leave from, to draw wins where the
    // player left from (below).
    let mut graph: FxHashMap<u32, Vec<u32>> = FxHashMap::default();
    let sums: Vec<std::sync::Mutex<FrameSums>> =
        (0..n).map(|_| std::sync::Mutex::new(FrameSums { cells: Default::default(), wins: Default::default(), marked: vec![Default::default(); sets.len()] })).collect();
    if n > 0 {
        let tree = Tree::<N>::open(dir, n as u32 - 1)?;
        let mut by_region: FxHashMap<u32, RegionBlocks> = FxHashMap::default();
        for (f, file) in tree.frames.iter().enumerate() {
            for (bi, r) in file.iter().flat_map(|x| x.header.regions.iter().enumerate()) {
                let rb = by_region.entry(r.region).or_insert_with(|| RegionBlocks { region: r.region, states: 0, blocks: Vec::new() });
                rb.states += r.states as u64;
                rb.blocks.push((f, bi));
            }
        }
        let mut regions: Vec<RegionBlocks> = by_region.into_values().collect();
        regions.sort_unstable_by_key(|r| (std::cmp::Reverse(r.states), r.region));
        let won = crate::par::Pool::new(threads).map(regions.len(), |i| count_region(&tree, &regions[i], sets, &sums))?;
        let mut win_regions: Vec<u32> = regions.iter().zip(won).filter(|x| x.1).map(|x| x.0.region).collect();
        win_regions.sort_unstable();
        graph = sources_into::<N>(dir, n as u32 - 1, &win_regions, threads)?;
    }
    let mut frames: Vec<(Sparse, Sparse)> = Vec::with_capacity(n);
    let mut layers: Vec<Vec<Sparse>> = vec![Vec::with_capacity(n); sets.len()];
    for s in sums {
        let FrameSums { mut cells, mut wins, marked } = s.into_inner().unwrap();
        // A won state's cell is in the NEXT room: draw it where its player
        // LEFT from, the in-edges' source cell (any frame's) with the most
        // states at the previous frame. States and wins move whole: totals
        // are unchanged.
        if let Some((prev, _)) = frames.last() {
            let prev: &Sparse = prev;
            let busiest = |win: u32| -> Option<u32> {
                graph
                    .get(&win)
                    .into_iter()
                    .flatten()
                    .filter_map(|&s| prev.binary_search_by_key(&s, |&(c, _)| c).ok().map(|i| prev[i]))
                    .max_by_key(|&(c, n)| (n, std::cmp::Reverse(c)))
                    .map(|(c, _)| c)
            };
            let mut moves: Vec<(u32, u32)> = wins.keys().filter_map(|&w| busiest(w).map(|at| (w, at))).collect();
            moves.sort_unstable();
            for (from, to) in moves {
                if let Some(n) = wins.remove(&from) {
                    *wins.entry(to).or_default() += n;
                }
                if let Some(n) = cells.remove(&from) {
                    *cells.entry(to).or_default() += n;
                }
            }
        }
        frames.push((sparse(cells)?, sparse(wins)?));
        for (k, m) in marked.into_iter().enumerate() {
            layers[k].push(sparse(m)?);
        }
    }
    Ok((LevelData { frames }, layers))
}

struct Writer {
    buf: Vec<u8>,
}

impl Writer {
    fn new() -> Self {
        Writer { buf: Vec::new() }
    }
    fn u32(&mut self, v: u32) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }
    fn pos(&self) -> u32 {
        self.buf.len() as u32
    }
    fn patch(&mut self, at: u32, v: u32) {
        self.buf[at as usize..at as usize + 4].copy_from_slice(&v.to_le_bytes());
    }
}

/// A state with no player position (the player object is gone: dead, or
/// past the room transition) has no cell; the UI counts it off-grid.
pub(crate) const NO_POSITION: u32 = u32::MAX;

fn cell_idx(cell: u32, b: &Box2) -> Result<u32> {
    let Some((x, y)) = cell_xy(cell) else { return Ok(NO_POSITION) };
    let (lx, ly) = (x - b.x0, y - b.y0);
    if lx < 0 || ly < 0 || lx >= b.w as i32 || ly >= b.h as i32 {
        bail!("cell ({x}, {y}) outside the export box {b:?}");
    }
    Ok(lx as u32 + ly as u32 * b.w)
}

fn write_frames_bin(path: &Path, data: &LevelData, magic: &[u8; 4], b: &Box2) -> Result<()> {
    let mut w = Writer::new();
    w.buf.extend_from_slice(magic);
    w.u32(data.frames.len() as u32);
    let table = w.pos();
    for _ in &data.frames {
        for _ in 0..4 {
            w.u32(0);
        }
    }
    for (i, (cells, wins)) in data.frames.iter().enumerate() {
        let entry = table + i as u32 * 16;
        w.patch(entry, w.pos());
        w.patch(entry + 4, cells.len() as u32);
        for &(c, _) in cells {
            w.u32(cell_idx(c, b)?);
        }
        for &(_, n) in cells {
            w.u32(n);
        }
        w.patch(entry + 8, w.pos());
        w.patch(entry + 12, wins.len() as u32);
        for &(c, _) in wins {
            w.u32(cell_idx(c, b)?);
        }
        for &(_, n) in wins {
            w.u32(n);
        }
    }
    std::fs::write(path, &w.buf).with_context(|| format!("writing {}", path.display()))
}

fn write_marks_bin(path: &Path, marks: &[(u32, u32, u32)], b: &Box2) -> Result<()> {
    let mut w = Writer::new();
    w.buf.extend_from_slice(b"CUM1");
    w.u32(marks.len() as u32);
    for &(c, _, _) in marks {
        w.u32(cell_idx(c, b)?);
    }
    for &(_, d, _) in marks {
        w.u32(d);
    }
    for &(_, _, n) in marks {
        w.u32(n);
    }
    std::fs::write(path, &w.buf).with_context(|| format!("writing {}", path.display()))
}

fn room_tiles(room: (i16, i16)) -> Result<Tiles> {
    let cart = &celeste_core::cart_data::CART;
    let grid = cart.map_grid();
    // The start room and the one to its right (where the exit lands).
    let (w, h) = (32u32, 16u32);
    let mut ids = Vec::with_capacity((w * h) as usize);
    let mut solid = Vec::with_capacity((w * h) as usize);
    for ty in 0..h {
        for tx in 0..w {
            let (mx, my) = (room.0 as u32 * 16 + tx, room.1 as u32 * 16 + ty);
            let id = if mx < 128 && my < 64 { grid[(mx + my * 128) as usize] } else { 0 };
            ids.push(id);
            solid.push(
                cart.fget(
                    celeste_core::pico8_num::Pico8Num::from_i16(id as i16),
                    celeste_core::pico8_num::Pico8Num::from_i16(0),
                )
                .unwrap_or(false),
            );
        }
    }
    Ok(Tiles { w, h, ids, solid })
}

/// A path file: `inputs a,b,..`, then `f x y` per frame from 0 (`f - -`
/// without a player). A `--witness` / `--reference` file (`rewrite pico8
/// align`) may also carry `label TEXT` and
/// `dashes F:DIR ...` before the frames, and end early (at the exit).
fn read_witness(path: &Path) -> Result<Witness> {
    let text = std::fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
    let name = path.display();
    let mut lines = text.lines().peekable();
    let mut head: FxHashMap<&str, &str> = FxHashMap::default();
    while let Some(l) = lines.next_if(|l| !l.starts_with(|c: char| c.is_ascii_digit())) {
        let (k, v) = l.split_once(' ').unwrap_or((l, ""));
        anyhow::ensure!(["label", "inputs", "dashes"].contains(&k), "{name}: unknown line {l:?}");
        anyhow::ensure!(head.insert(k, v.trim()).is_none(), "{name}: two `{k}` lines");
    }
    let num = |t: &str| -> Result<i32> { t.parse().with_context(|| format!("{name}: {t:?} is not a number")) };
    let inputs: Vec<u8> = head
        .get("inputs")
        .with_context(|| format!("{name}: no `inputs` line"))?
        .split(',')
        .map(|b| b.trim().parse::<u8>().with_context(|| format!("{name}: an input byte")))
        .collect::<Result<_>>()?;
    let dashes = head
        .get("dashes")
        .map_or("", |v| v)
        .split_whitespace()
        .map(|d| {
            let (f, dir) = d.split_once(':').with_context(|| format!("{name}: dash {d:?} is not F:DIR"))?;
            Ok((f.parse().with_context(|| format!("{name}: dash {d:?} is not F:DIR"))?, dir.to_string()))
        })
        .collect::<Result<_>>()?;
    let mut path_xy = Vec::new();
    for (i, l) in lines.enumerate() {
        let t: Vec<&str> = l.split_whitespace().collect();
        anyhow::ensure!(t.len() == 3 && t[0].parse::<usize>().ok() == Some(i), "{name}: line {:?} is not frame {i}", l);
        path_xy.push(if t[1] == "-" { None } else { Some((num(t[1])?, num(t[2])?)) });
    }
    anyhow::ensure!((1..=inputs.len() + 1).contains(&path_xy.len()), "{name}: {} inputs, {} positions", inputs.len(), path_xy.len());
    let label = head.get("label").map_or_else(|| format!("concrete witness inside the winning sets, a win at f{}", inputs.len()), |l| l.to_string());
    Ok(Witness { label, inputs, path: path_xy, dashes })
}

/// A level's name in the UI: its spec, under its half's name in a dual
/// search's record (`fixed/level00`, `float/level00`).
fn level_label(l: &crate::search::result::LevelResult) -> String {
    match l.dir.split_once('/') {
        Some((half, _)) => format!("{half}: {}", l.spec),
        None => l.spec.clone(),
    }
}

/// THE MARKS RECORD of the search in `dir` (`result::marks_path`, over
/// `last`'s tree, in its arithmetic `arith`): the last forward level gets
/// W's support, a level after it the ARC backward over the same tree; the
/// optimum is `last`'s arc bound unless `keep_optimal` (a dual search's is
/// the pair's).
fn attach_arc(hr: &mut HorizonRun, optimal: &mut Option<u32>, dir: &Path, last: &crate::search::result::LevelResult, arith: crate::settings::Arith, keep_optimal: bool) -> Result<()> {
    use crate::search::result::{marks_path, Marks};
    let rows = |m: Marks| -> Result<u64> { Ok(MarksMap::open(&marks_path(dir, m), arith)?.rows as u64) };
    let found = last.arc_bound;
    hr.partial = false;
    let li = hr.levels.len() - 1;
    let lr = &mut hr.levels[li];
    lr.precision = format!("{}, rem free", level_label(last));
    lr.refuted = lr.first_win.is_none();
    lr.marked = Some(rows(Marks::Support)?);
    let mut arc = level_run(li + 1, format!("arc: {}, rem exact", level_label(last)), Vec::new(), found);
    arc.marked = found.map(|_| rows(Marks::Arc)).transpose()?;
    arc.refuted = found.is_none();
    arc.forward_of = Some(li);
    hr.levels.push(arc);
    hr.refuted_at = hr.levels.iter().position(|l| l.refuted);
    if !keep_optimal {
        *optimal = if hr.refuted_at.is_none() { found } else { None };
    }
    Ok(())
}

/// The search's witness as the UI draws it: its inputs and path from the
/// record (`result.json`), labelled as the search found it.
fn record_witness(res: &crate::search::result::SearchResult) -> Option<Witness> {
    let (inputs, cells) = (res.witness.clone()?, res.witness_path.as_ref()?);
    let label = match &res.dual {
        Some(d) => format!("dual witness, valid in both arithmetics: {} exits on frame {}, real play on frame {} (the float half's path)", d.float, inputs.len(), d.fixed_exit.map_or("-".to_string(), |x| x.to_string())),
        None => format!("concrete witness inside the winning sets, a win at f{}", inputs.len()),
    };
    let path = cells.iter().map(|&c| cell_xy(c)).collect();
    Some(Witness { label, inputs, path, dashes: Vec::new() })
}

/// The concrete runs drawn over the room (`read_witness`'s format;
/// `witness` replaces the search's) and the download.
pub struct Paths<'a> {
    pub witness: Option<&'a Path>,
    pub reference: Option<&'a Path>,
    pub download: Option<&'a Path>,
}

/// The export of a search's `checkpoint_dir` (`result.json`, `levelNN/`,
/// the marks record; the room is the record's).
pub fn export(checkpoint_dir: &Path, out: &Path, threads: usize, paths: Paths) -> Result<()> {
    let res = crate::search::result::SearchResult::read(checkpoint_dir)?;
    let room = res.room;
    let last = res.levels.last().with_context(|| format!("{}: the search recorded no level ({:?})", checkpoint_dir.display(), res.verdict))?;
    // `h` counts search steps (the trees' frames).
    let h = crate::settings::Clock { per_frame: res.steps_per_frame }.steps(res.horizon);
    let tree_dir = |level: usize| checkpoint_dir.join(&res.levels[level].dir);
    let mut levels = Vec::new();
    for (i, l) in res.levels.iter().enumerate() {
        levels.push(level_run(i, level_label(l), read_forward_stats(&tree_dir(i))?, l.first_win));
    }
    let nforward = levels.len();
    let (wall_s, mut optimal) = (Some(res.wall_s), res.optimal);
    let mut hr = HorizonRun { h, levels, refuted_at: None, partial: true };
    // The marks record: the last tree's, in its arithmetic.
    let marks_arith = crate::settings::recorded_arith(&tree_dir(nforward - 1))?;
    let arc = match &res.marks {
        Some(t) => {
            anyhow::ensure!(*t == last.dir, "{}: the marks record is {t}'s, not the last level's ({})", checkpoint_dir.display(), last.dir);
            attach_arc(&mut hr, &mut optimal, checkpoint_dir, last, marks_arith, res.dual.is_some())?;
            Some(checkpoint_dir)
        }
        None => None,
    };
    let witness = match paths.witness {
        Some(p) => Some(read_witness(p)?),
        None => record_witness(&res),
    };
    let reference = paths.reference.map(read_witness).transpose()?;
    let download = match paths.download {
        Some(p) => Some(Download {
            file: p.file_name().with_context(|| format!("{}: not a file", p.display()))?.to_string_lossy().into_owned(),
            text: std::fs::read_to_string(p).with_context(|| format!("reading {}", p.display()))?,
        }),
        None => None,
    };
    eprintln!("[export-ui] {}: {} levels to f{h}, optimal {optimal:?}", checkpoint_dir.display(), hr.levels.len());
    std::fs::create_dir_all(out)?;

    struct MarksOut {
        level: usize,
        by_cell_dist: Vec<(u32, u32, u32)>,
        by_layer: Vec<Sparse>,
    }
    enum Done {
        Frames { level: usize, data: LevelData },
        Marks(Vec<MarksOut>),
    }
    let mut results: Vec<Done> = Vec::new();
    let t = std::time::Instant::now();
    for level in 0..nforward {
        let tt = std::time::Instant::now();
        let dir = tree_dir(level);
        let (mut set_levels, mut maps, mut by_cell_dist) = (Vec::new(), Vec::new(), Vec::new());
        if let Some(a) = arc.filter(|_| level == nforward - 1) {
            use crate::search::result::{marks_path, Marks};
            for (l, m) in [(level, Marks::Support), (level + 1, Marks::Arc)] {
                if !hr.levels[l].refuted {
                    let path = marks_path(a, m);
                    let (m, by) = scan_marks(MarksMap::open(&path, marks_arith)?, &path, threads)?;
                    by_cell_dist.push(by);
                    set_levels.push(l);
                    maps.push(m);
                }
            }
            crate::metrics::mem_phase(&format!("export-ui {}: marks scanned", dir.display()));
        }
        // In the tree's own arithmetic (a dual search's halves differ).
        let (data, layers) = crate::with_arith!(crate::settings::recorded_arith(&dir)?, N => read_tree::<N>(&dir, &maps, threads))
        .with_context(|| format!("frames of {}", dir.display()))?;
        // Every marked state is a state of the tree.
        for ((m, l), layer) in maps.iter().zip(&set_levels).zip(&layers) {
            let held: u64 = layer.iter().flatten().map(|&(_, n)| n as u64).sum();
            anyhow::ensure!(held == m.m.rows as u64, "{}: level {l}'s marked set has {} states, {held} of them in the tree", dir.display(), m.m.rows);
        }
        eprintln!("[export-ui] {}: {} frames, {} marked sets in {:.1} s", dir.display(), data.frames.len(), maps.len(), tt.elapsed().as_secs_f64());
        crate::metrics::mem_phase(&format!("export-ui {}: frames read", dir.display()));
        results.push(Done::Frames { level, data });
        if !maps.is_empty() {
            let outs = set_levels.iter().zip(by_cell_dist.into_iter().zip(layers)).map(|(&level, (by_cell_dist, by_layer))| MarksOut { level, by_cell_dist, by_layer }).collect();
            results.push(Done::Marks(outs));
        }
    }
    eprintln!("[export-ui] read {nforward} trees in {:.1} s", t.elapsed().as_secs_f64());

    // The cell box: everything seen, and at least the start room.
    let (mut x0, mut y0, mut x1, mut y1) = (0i32, 0i32, 127i32, 127i32);
    let mut see = |cell: u32| {
        if let Some((x, y)) = cell_xy(cell) {
            x0 = x0.min(x);
            y0 = y0.min(y);
            x1 = x1.max(x);
            y1 = y1.max(y);
        }
    };
    for d in &results {
        match d {
            Done::Frames { data, .. } => {
                for (cells, wins) in &data.frames {
                    cells.iter().chain(wins).for_each(|&(c, _)| see(c));
                }
            }
            Done::Marks(outs) => {
                for o in outs {
                    o.by_cell_dist.iter().for_each(|&(c, _, _)| see(c));
                }
            }
        }
    }
    let cell_box = Box2 { x0, y0, w: (x1 - x0 + 1) as u32, h: (y1 - y0 + 1) as u32 };
    eprintln!("[export-ui] cell box {cell_box:?}");

    for d in &results {
        match d {
            Done::Frames { level, data } => {
                let name = if *level == 0 { "l00.frames.bin".to_string() } else { format!("h{h:03}_l{level:02}.frames.bin") };
                write_frames_bin(&out.join(&name), data, b"CUF1", &cell_box)?;
                let states: Vec<u64> = data.frames.iter().map(|(c, _)| c.iter().map(|&(_, n)| n as u64).sum()).collect();
                let wins: Vec<u64> = data.frames.iter().map(|(_, w)| w.iter().map(|&(_, n)| n as u64).sum()).collect();
                for lr in hr.levels.iter_mut().filter(|lr| lr.forward_of.unwrap_or(lr.level) == *level) {
                    lr.frames = data.frames.len() as u32;
                    lr.frames_file = name.clone();
                    lr.frame_states = states.clone();
                    lr.frame_wins = wins.clone();
                }
            }
            Done::Marks(outs) => {
                for o in outs {
                    let level = o.level;
                    let name = format!("h{h:03}_l{level:02}.marks.bin");
                    write_marks_bin(&out.join(&name), &o.by_cell_dist, &cell_box)?;
                    let lname = format!("h{h:03}_l{level:02}.mlayers.bin");
                    let layers = LevelData { frames: o.by_layer.iter().map(|c| (c.clone(), Vec::new())).collect() };
                    write_frames_bin(&out.join(&lname), &layers, b"CUL1", &cell_box)?;
                    let max_d = o.by_cell_dist.iter().map(|m| m.1).max().unwrap_or(0) as usize;
                    let mut by_dist = vec![0u64; max_d + 1];
                    for &(_, d, n) in &o.by_cell_dist {
                        by_dist[d as usize] += n as u64;
                    }
                    let lr = &mut hr.levels[level];
                    lr.marks_file = Some(name);
                    lr.marks_by_dist = by_dist;
                    lr.mlayers_file = Some(lname);
                    lr.marks_by_layer = o.by_layer.iter().map(|c| c.iter().map(|&(_, n)| n as u64).sum()).collect();
                }
            }
        }
    }
    for lr in &hr.levels {
        anyhow::ensure!(!lr.frames_file.is_empty(), "level {}: no frames exported", lr.level);
    }

    let run = Run {
        format: 1,
        room,
        cell_box,
        tiles: room_tiles(room)?,
        prebuild_s: None,
        optimal,
        horizons: vec![hr],
        witness,
        reference,
        download,
    };
    let timings = serde_json::json!({
        "wall_s": wall_s,
        "horizons": run.horizons.iter().map(|h| serde_json::json!({ "levels": h.levels.iter().map(|l| serde_json::json!({ "fwd": l.fwd.iter().map(|f| &f.times).collect::<Vec<_>>() })).collect::<Vec<_>>() })).collect::<Vec<_>>(),
    });
    let mut f = std::io::BufWriter::new(std::fs::File::create(out.join("run.json"))?);
    serde_json::to_writer(&mut f, &run)?;
    f.flush()?;
    std::fs::write(out.join("timings.json"), serde_json::to_vec(&timings)?)?;
    let total: u64 = std::fs::read_dir(out)?.filter_map(|e| e.ok()).filter_map(|e| e.metadata().ok()).map(|m| m.len()).sum();
    eprintln!("[export-ui] wrote {} ({:.1} MB) in {:.1} s", out.display(), total as f64 / 1e6, t.elapsed().as_secs_f64());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// `gallop` is `partition_point` from any start, at every range and
    /// threshold.
    #[test]
    fn gallop_finds_the_first_index_past_a_prefix() {
        for lo in 0..6 {
            for hi in lo..70 {
                for t in lo..=hi {
                    assert_eq!(gallop(lo, hi, |i| i < t), t, "lo {lo} hi {hi} threshold {t}");
                }
            }
        }
    }

    /// A marks file read in place: its rows by (dist, cell), a region's
    /// marked states per cell (a key marked at one cell or shape is not at
    /// another), against a set on random regions; an unsorted file refused.
    #[test]
    fn marks_files_merge_with_a_regions_keys() {
        let dir = crate::testutil::TestDir::new("ui-marks");
        std::fs::create_dir_all(&dir).unwrap();
        let save = |name: &str, rows: &[((u64, u64), u32, u64, u16)]| {
            let mut v = crate::frame::Visited::<celeste_engine::runtime2::P8>::new(Default::default());
            for &(key, cell, shape, deadline) in rows {
                v.insert_until(shape, key, cell, deadline);
            }
            let path = dir.join(name);
            v.save(&path, 9).unwrap();
            scan_marks(MarksMap::open(&path, crate::settings::Arith::Fixed).unwrap(), &path, 2).unwrap()
        };
        let slot = 5u32;
        let c = |local: u32| crate::storage::cells::cell(slot, local);
        // dist = the horizon (9) minus the deadline.
        let (a, by_cell_dist) = save("a.bin", &[((1, 2), c(3), 7, 9), ((5, 6), c(3), 7, 9), ((1, 2), c(4), 7, 8), ((1, 2), c(3), 8, 9)]);
        assert_eq!(a.m.rows, 4);
        assert_eq!(by_cell_dist, vec![(c(3), 0, 3), (c(4), 1, 1)], "sorted by (dist, cell)");
        assert_eq!(a.cells.len(), 3, "rows at (7, 3), (7, 4), (8, 3)");
        let keys = [(1, 2), (3, 3), (5, 6)];
        let masks = [1 << 3 | 1 << 4 | 1 << 9, 1 << 3, 1 << 4];
        let marked = |m: &MarkedSet, shape: u64, slot: u32, keys: &[Key], masks: &[u64]| count_marked(&m.m, &CellRows::of(m, shape, slot, u64::MAX), keys, masks);
        let got = marked(&a, 7, slot, &keys, &masks);
        assert_eq!((got[3], got[4], got[9], got.iter().sum::<u32>()), (1, 1, 0, 2), "(5, 6) is marked at cell 3, held at 4");
        let got = marked(&a, 8, slot, &keys, &masks);
        assert_eq!((got[3], got.iter().sum::<u32>()), (1, 1), "shape 8 has (1, 2) at cell 3 only");

        // Random regions of two shapes over three slots against the set.
        let mut x = 0x9e3779b97f4a7c15u64;
        let mut rnd = move || crate::testutil::xorshift(&mut x);
        let mut regions = Vec::new();
        let mut rows = Vec::new();
        for shape in [3u64, 11] {
            for slot in [0u32, 1, 9] {
                let mut keys: Vec<Key> = (0..300).map(|_| (rnd() % 4, rnd() % 64)).collect();
                keys.sort_unstable();
                keys.dedup();
                let masks: Vec<u64> = keys.iter().map(|_| rnd() & rnd() | 1 << (rnd() % 64)).collect();
                for (k, &m) in keys.iter().zip(&masks) {
                    for l in bits(m).filter(|_| rnd() % 3 == 0) {
                        rows.push((*k, crate::storage::cells::cell(slot, l), shape, 9));
                    }
                }
                // Rows at cells the region does not hold, and of keys it lacks.
                rows.push(((9, 9), crate::storage::cells::cell(slot, 0), shape, 9));
                rows.push(((0, 1000), crate::storage::cells::cell(slot, 63), shape, 9));
                regions.push((shape, slot, keys, masks));
            }
        }
        let want: rustc_hash::FxHashSet<(u64, u32, Key)> = rows.iter().map(|&(k, c, s, _)| (s, c, k)).collect();
        let (m, _) = save("r.bin", &rows);
        for (shape, slot, keys, masks) in &regions {
            let got = marked(&m, *shape, *slot, keys, masks);
            for l in 0..64 {
                let n = keys.iter().zip(masks).filter(|(k, &mk)| mk >> l & 1 == 1 && want.contains(&(*shape, crate::storage::cells::cell(*slot, l), **k))).count();
                assert_eq!(got[l as usize] as usize, n, "shape {shape} slot {slot} cell {l}");
            }
        }

        // Out of order: refused.
        let bad = dir.join("bad.bin");
        let rows: Vec<crate::frame::MarkRow> = vec![(7, 3, 5, 6, 0), (7, 3, 1, 2, 0)];
        crate::storage::file::save_value::<celeste_engine::runtime2::P8, _>(&bad, &(rows, 9u32, celeste_engine::exact::KeySpace::<celeste_engine::runtime2::P8>::default().to_additions())).unwrap();
        assert!(scan_marks(MarksMap::open(&bad, crate::settings::Arith::Fixed).unwrap(), &bad, 2).is_err());
    }

    /// The forward's record reads back: a rerun frame's last row wins.
    #[test]
    fn a_trees_forward_record_reads_back() {
        let dir = crate::testutil::TestDir::new("ui-fwd");
        std::fs::create_dir_all(&dir).unwrap();
        assert!(read_forward_stats(&dir).unwrap().is_empty(), "a tree from before the record");
        let ms = std::time::Duration::from_millis;
        let row = |f: u32, kept: usize| {
            use crate::storage::stats::S;
            let mut st = crate::storage::stats::Stats::default();
            st.set(S::Kept, kept as u64);
            st.set(S::WaveIdle, 94);
            st.add_time(S::Wave, ms(7));
            st.set_gb(S::RssEnd, 1.28);
            st.add_time(S::Ckpt, ms(1));
            st.add_time(S::Total, ms(11));
            st.set(S::Visited, 4);
            crate::frame::forward_stats_row(f, &st)
        };
        for (f, kept) in [(1, 10), (2, 20), (2, 21), (3, 30)] {
            crate::frame::append_forward_stats(&dir, &row(f, kept)).unwrap();
        }
        let got = read_forward_stats(&dir).unwrap();
        assert_eq!(got.iter().map(|l| (l.f, l.kept)).collect::<Vec<_>>(), vec![(1, 10), (2, 21), (3, 30)]);
        let t = &got[0].times;
        assert_eq!((t.emit_ms, t.emit_idle, t.ckpt_ms, t.total_ms, got[0].visited, t.rss_gb), (7, 94, 1, 11, 4, 1.28));
    }

    /// A path file and a labelled path file with dashes that ends at the
    /// exit both read; a skipped frame does not.
    #[test]
    fn witness_and_reference_files_read() {
        let dir = crate::testutil::TestDir::new("ui-witness");
        std::fs::create_dir_all(&dir).unwrap();
        let read = |text: &str| {
            let p = dir.join("w.txt");
            std::fs::write(&p, text).unwrap();
            read_witness(&p)
        };
        let w = read("inputs 0,34\n0 - -\n1 8 96\n2 13 96\n").unwrap();
        assert_eq!((w.inputs, w.path, w.dashes.len()), (vec![0, 34], vec![None, Some((8, 96)), Some((13, 96))], 0));
        assert!(w.label.contains("f2"));
        let r = read("label TAS29 (community)\ninputs 0,34,0\ndashes 2:R\n0 - -\n1 8 96\n2 13 96\n").unwrap();
        assert_eq!((r.label.as_str(), r.path.len(), r.dashes), ("TAS29 (community)", 3, vec![(2, "R".to_string())]));
        assert!(read("inputs 0,34\n0 - -\n2 8 96\n").is_err(), "frames are consecutive from 0");
        assert!(read("inputs 0,34\nprologue 1\n0 - -\n").is_err(), "an unknown line");
    }
}
