//! `rewrite export-ui`: a finished search's record (`result.json`), its
//! checkpoint trees and marks -> the compact static data the web UI (`ui/`)
//! renders.
//!
//! Per (level, frame): states and wins per player-position cell; per marked
//! set (`search --save-marks`: W's support, arc-marked nodes): the
//! marked states per cell by distance to the win. All of it comes from the
//! checkpoint HEADERS (cell index, win list), the marks files, and the key
//! column at marked cells: no row is decoded, no engine built. The forward's
//! per-frame stats are each tree's own record (`frame::FORWARD_STATS`).
//!
//! The run is one horizon whose levels are the search's, plus with `--arc` a
//! last level carrying the arc backward over the last forward tree
//! (`forward_of`). Some JSON fields (`bwd`, `reruns`, several horizons) are
//! kept only because the UI reads them.
//!
//! Output layout (`--out DIR`):
//!
//! * `run.json` - the run: room, the cell box every binary indexes into,
//!   the room's tiles, per level the per-frame forward stats and the names of
//!   the level's binary files, the concrete paths and the download.
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
//! * `hHHH_lLL.mlayers.bin` - the same marked set split by the FRAME each
//!   state was first reached at (its BFS layer): the frames format (magic
//!   `"CUL1"`, empty win records), from intersecting the marks with each
//!   frame file's `(cell, key)` rows.
//!
//! Everything is little-endian u32 at 4-byte alignment, so the UI reads
//! it as `Uint32Array` views without a parser.

use anyhow::{bail, Context, Result};
use rustc_hash::FxHashMap;
use serde::Serialize;
use std::io::Write;
use std::path::Path;

use crate::search::pos_graph::cell_xy;

/// One forward frame's stats (a row of the tree's `forward.tsv`, under the
/// UI's names).
#[derive(Serialize, Clone, Debug)]
pub struct FwdLine {
    pub f: u32,
    pub in_entries: u64,
    pub in_lanes: u64,
    pub raw: u64,
    pub kept: u64,
    pub out_entries: u64,
    pub out_lanes: u64,
    pub visited: u64,
    pub emit_ms: u64,
    pub emit_idle: u32,
    pub own_ms: u64,
    pub own_idle: u32,
    pub ckpt_ms: u64,
    pub pos_ms: u64,
    pub total_ms: u64,
    pub rss_gb: f64,
}

/// One level of the run.
#[derive(Serialize, Clone, Debug)]
pub struct LevelRun {
    pub level: usize,
    /// The level's spec (`r0sxhn`), or the arc pass's name.
    pub precision: String,
    pub fwd: Vec<FwdLine>,
    /// Always empty (kept for the UI).
    pub bwd: [u32; 0],
    pub first_win: Option<u32>,
    pub marked: Option<u64>,
    pub reruns: Option<u64>,
    pub refuted: bool,
    /// Frames present in the checkpoint tree (f0..frames-1).
    pub frames: u32,
    pub frames_file: String,
    pub marks_file: Option<String>,
    /// Per frame, the states and the win count, from the checkpoint files.
    pub frame_states: Vec<u64>,
    pub frame_wins: Vec<u64>,
    pub marks_have_dist: bool,
    pub marks_by_dist: Vec<u64>,
    pub mlayers_file: Option<String>,
    /// Per frame, the marked states of that layer.
    pub marks_by_layer: Vec<u64>,
    /// A backward over ANOTHER level's forward (`--arc`): that level's
    /// index, whose frames it shares; no forward pass of its own.
    pub forward_of: Option<usize>,
}

#[derive(Serialize, Clone, Debug)]
pub struct HorizonRun {
    pub h: u32,
    pub levels: Vec<LevelRun>,
    pub refuted_at: Option<usize>,
    /// No backward (exported without `--arc`): frames up to `h`, no verdict.
    pub partial: bool,
}

#[derive(Serialize, Clone, Debug)]
pub struct Box2 {
    pub x0: i32,
    pub y0: i32,
    pub w: u32,
    pub h: u32,
}

#[derive(Serialize, Clone, Debug)]
pub struct Tiles {
    /// Tile-space origin in start-room pixels is (0, 0); `w x h` tiles of
    /// 8 px, row-major.
    pub w: u32,
    pub h: u32,
    pub ids: Vec<u8>,
    pub solid: Vec<bool>,
}

#[derive(Serialize, Debug)]
pub struct Run {
    pub format: u32,
    pub room: (i16, i16),
    pub cell_box: Box2,
    pub tiles: Tiles,
    pub wall_s: Option<f64>,
    pub prebuild_s: Option<f64>,
    pub optimal: Option<u32>,
    pub horizons: Vec<HorizonRun>,
    /// A concrete run drawn over the room (`export-ui --arc`, `witness.txt`;
    /// `--witness FILE` replaces it).
    pub witness: Option<Witness>,
    /// Another concrete run to compare it with (`--reference FILE`: e.g. the
    /// community TAS replayed in the original cart).
    pub reference: Option<Witness>,
    /// The file the UI offers for download (`--download FILE`), verbatim.
    pub download: Option<Download>,
}

/// A file offered for download: its name and its bytes (text).
#[derive(Serialize, Debug)]
pub struct Download {
    pub file: String,
    pub text: String,
}

/// A concrete witness: its inputs and the player's (x, y) per frame (frame 0
/// the start; `None` without a player), in the cells' pixel coordinates, and
/// the frames a dash starts at with its direction (`R`, `U`, ...; only when
/// the file lists them).
#[derive(Serialize, Debug)]
pub struct Witness {
    pub label: String,
    pub inputs: Vec<u8>,
    pub path: Vec<Option<(i32, i32)>>,
    pub dashes: Vec<(u32, String)>,
}

/// A level of the run.
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

/// A tree's forward record (`frame::FORWARD_STATS`), a frame's last row
/// winning, by frame; empty for a tree from before the record.
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
    let c: Vec<usize> = ["frame", "entries_in", "lanes_in", "lanes_raw", "lanes_kept", "entries_out", "lanes_out", "visited", "wave_ms", "wave_idle_pct", "layer_ms", "ckpt_ms", "pos_ms", "total_ms", "rss_end_gb"]
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
                emit_ms: n(8)?,
                emit_idle: n(9)? as u32,
                // The storage's own wall time: the frame's end (its region
                // phase runs inside the wave).
                own_ms: n(10)?,
                own_idle: 0,
                ckpt_ms: n(11)?,
                pos_ms: n(12)?,
                total_ms: n(13)?,
                rss_gb: x(14)?,
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

/// A marks file, mapped: `Visited::save`'s `(shape, cell, k0, k1, dist)`
/// rows (32 bytes each) then the horizon (4 bytes), read in place (up to
/// ~100M rows).
struct MarksMap {
    map: memmap2::Mmap,
    rows: usize,
}

/// Bytes before the first row: the checkpoint value header (16) and the
/// bincode `Vec` length (8).
const MARKS_ROWS_AT: usize = 24;

impl MarksMap {
    fn open(path: &Path) -> Result<Self> {
        let file = std::fs::File::open(path)?;
        // SAFETY: a marks file is written once (renamed into place) and
        // never modified afterwards.
        let map = unsafe { memmap2::Mmap::map(&file)? };
        let payload = crate::search::checkpoint::value_payload(&map, path)?;
        anyhow::ensure!(payload.len() >= 8, "{}: too short for a marks file", path.display());
        let n = u64::from_le_bytes(payload[0..8].try_into().unwrap());
        let body = payload.len() as u64 - 8;
        // The rows, the horizon, then the tree's key space (the keys are the
        // tree's, as its frame rows').
        anyhow::ensure!(n.checked_mul(32).and_then(|b| b.checked_add(4)).is_some_and(|b| b <= body), "{}: {n} rows do not fit {body} payload bytes", path.display());
        Ok(MarksMap { map, rows: n as usize })
    }

    /// Row `i` as `((shape, cell, key), dist)`.
    fn row(&self, i: usize) -> ((u64, u32, (u64, u64)), u32) {
        let b = &self.map[MARKS_ROWS_AT + i * 32..MARKS_ROWS_AT + (i + 1) * 32];
        let u64_at = |o: usize| u64::from_le_bytes(b[o..o + 8].try_into().unwrap());
        let u32_at = |o: usize| u32::from_le_bytes(b[o..o + 4].try_into().unwrap());
        ((u64_at(0), u32_at(8), (u64_at(12), u64_at(20))), u32_at(28))
    }
}

/// Shards of the marked states, by (shape, cell): enough to build them in
/// parallel, and a frame file's run looks up one shard.
const MARK_SHARDS: usize = 256;

fn mark_shard(shape: u64, cell: u32) -> usize {
    use celeste_engine::runtime2::mix64;
    (mix64(shape ^ mix64(cell as u64)) % MARK_SHARDS as u64) as usize
}

/// One marked state: `(shape, cell, key)`, as the frame rows are keyed.
type MarkId = (u64, u32, (u64, u64));

/// Every marked state of up to 64 sets over one tree with the bitmask of
/// sets holding it, and the (shape, cell)s holding any, so a run at an
/// unmarked cell is skipped unread. A hash probe per row rather than a
/// merge against sorted marks: runs are a few rows, and a missing probe is
/// one cache miss.
struct MarkTable {
    shards: Vec<FxHashMap<MarkId, u64>>,
    cells: rustc_hash::FxHashSet<(u64, u32)>,
}

/// Rows of the marks files per chunk of the parallel read.
const MARKS_CHUNK: usize = 1 << 20;

/// The sets' table, and per set its `(cell, dist, count)` sorted by (dist,
/// cell).
fn mark_table(sets: &[MarksMap]) -> Result<(MarkTable, Vec<Vec<(u32, u32, u32)>>)> {
    anyhow::ensure!(sets.len() <= 64, "at most 64 marked sets per tree");
    // Every set's rows in parallel into per-shard lists and (dist, cell) counts.
    let chunks: Vec<(usize, usize)> =
        sets.iter().enumerate().flat_map(|(k, m)| (0..m.rows.div_ceil(MARKS_CHUNK)).map(move |c| (k, c))).collect();
    let read = crate::par::Pool::per_core().map(chunks.len(), |i| {
        let (k, c) = chunks[i];
        let m = &sets[k];
        let mut shards: Vec<Vec<MarkId>> = vec![Vec::new(); MARK_SHARDS];
        let mut hist: FxHashMap<(u32, u32), u32> = FxHashMap::default();
        for r in c * MARKS_CHUNK..((c + 1) * MARKS_CHUNK).min(m.rows) {
            let (id, dist) = m.row(r);
            shards[mark_shard(id.0, id.1)].push(id);
            *hist.entry((dist, id.1)).or_default() += 1;
        }
        Ok((k, shards, hist))
    })?;
    let mut hists: Vec<FxHashMap<(u32, u32), u32>> = vec![FxHashMap::default(); sets.len()];
    let per_shard: Vec<std::sync::Mutex<Vec<(u64, Vec<MarkId>)>>> =
        (0..MARK_SHARDS).map(|_| std::sync::Mutex::new(Vec::new())).collect();
    for (k, shards, hist) in read {
        for (dc, n) in hist {
            *hists[k].entry(dc).or_default() += n;
        }
        for (s, rows) in shards.into_iter().enumerate() {
            if !rows.is_empty() {
                per_shard[s].lock().unwrap().push((1u64 << k, rows));
            }
        }
    }
    // Per shard: one entry per state with its sets' bits OR-ed, and the
    // shard's (shape, cell)s.
    let built = crate::par::Pool::per_core().map(MARK_SHARDS, |s| {
        let lists = std::mem::take(&mut *per_shard[s].lock().unwrap());
        let mut map: FxHashMap<MarkId, u64> = FxHashMap::default();
        map.reserve(lists.iter().map(|l| l.1.len()).sum());
        let mut cells = rustc_hash::FxHashSet::default();
        for (bit, list) in lists {
            for id in list {
                *map.entry(id).or_default() |= bit;
                cells.insert((id.0, id.1));
            }
        }
        Ok((map, cells))
    })?;
    let mut table = MarkTable { shards: Vec::with_capacity(MARK_SHARDS), cells: rustc_hash::FxHashSet::default() };
    for (map, cells) in built {
        table.shards.push(map);
        table.cells.extend(cells);
    }
    let by_cell_dist = hists
        .into_iter()
        .map(|h| {
            let mut v: Vec<(u32, u32, u32)> = h.into_iter().map(|((d, c), n)| (c, d, n)).collect();
            v.sort_unstable_by_key(|&(c, d, _)| (d, c));
            v
        })
        .collect();
    Ok((table, by_cell_dist))
}

/// One frame of a tree: the states per cell, the wins per cell, and per
/// marked set the marked states per cell.
struct FrameCounts {
    cells: FxHashMap<u32, u32>,
    wins: FxHashMap<u32, u32>,
    marked: Vec<FxHashMap<u32, u32>>,
}

fn frame_counts(tree: &crate::storage::layer::Tree, f: u32, marks: Option<&MarkTable>, nsets: usize) -> Result<FrameCounts> {
    let geo = crate::storage::GEOMETRY;
    let mut out = FrameCounts { cells: FxHashMap::default(), wins: FxHashMap::default(), marked: vec![FxHashMap::default(); nsets] };
    let (layer, wins) = tree.layer(f, None, 1).with_context(|| format!("{} frame {f}", tree.dir.display()))?;
    for w in wins {
        *out.wins.entry(w.cell(&geo)).or_default() += 1;
    }
    for (region, key, mask) in layer.iter() {
        let shape = tree.shape_of(region)?;
        let slot = region % geo.slots;
        for local in crate::storage::layer::bits(mask) {
            let cell = geo.cell(slot, local);
            *out.cells.entry(cell).or_default() += 1;
            let Some(t) = marks else { continue };
            if !t.cells.contains(&(shape, cell)) {
                continue;
            }
            if let Some(&m) = t.shards[mark_shard(shape, cell)].get(&(shape, cell, key)) {
                let mut bits = m;
                while bits != 0 {
                    *out.marked[bits.trailing_zeros() as usize].entry(cell).or_default() += 1;
                    bits &= bits - 1;
                }
            }
        }
    }
    Ok(out)
}

fn sparse(m: FxHashMap<u32, u32>) -> Sparse {
    let mut v: Sparse = m.into_iter().collect();
    v.sort_unstable();
    v
}

/// Read one level directory: its frames from the headers and, in the same
/// pass, the sets of `marks` split by layer (`[k]`: per frame, set `k`'s
/// marked states per cell). Frames in parallel.
fn read_tree(dir: &Path, marks: Option<&MarkTable>, nsets: usize) -> Result<(LevelData, Vec<Vec<Sparse>>)> {
    let nframes = crate::storage::layer::last_frame(dir).map_or(0, |l| l + 1);
    // The pos graph, to draw wins where the player left from (below). An
    // absent graph draws wins in place; an unloadable one is an error.
    let graph_path = dir.join("posgraph.bin");
    let graph = if graph_path.exists() {
        Some(crate::search::pos_graph::PosGraph::load(&graph_path).with_context(|| format!("loading {}", graph_path.display()))?)
    } else {
        None
    };
    // The largest (latest) frames first, so the tail is small ones.
    let n = nframes as usize;
    let tree = match n {
        0 => None,
        _ => Some(crate::storage::layer::Tree::open(dir, n as u32 - 1)?),
    };
    let mut counts = match &tree {
        Some(tree) => crate::par::Pool::per_core().map(n, |i| frame_counts(tree, (n - 1 - i) as u32, marks, nsets))?,
        None => Vec::new(),
    };
    counts.reverse();
    let mut frames: Vec<(Sparse, Sparse)> = Vec::with_capacity(n);
    let mut layers: Vec<Vec<Sparse>> = vec![Vec::with_capacity(n); nsets];
    for FrameCounts { mut cells, mut wins, marked } in counts {
        // A won state's cell is in the NEXT room (one room to the right).
        // Draw it where its player LEFT from: of the pos-graph sources, the
        // one with the most states at the previous frame. The count moves
        // whole (states and wins), so totals are unchanged.
        if let (Some(g), Some((prev, _))) = (graph.as_ref(), frames.last()) {
            let prev: &Sparse = prev;
            let busiest = |win: u32| -> Option<u32> {
                g.sources(win)
                    .iter()
                    .filter_map(|&s| prev.binary_search_by_key(&s, |&(c, _)| c).ok().map(|i| prev[i]))
                    .max_by_key(|&(c, n)| (n, std::cmp::Reverse(c)))
                    .map(|(c, _)| c)
            };
            let moves: Vec<(u32, u32)> = wins.keys().filter_map(|&w| busiest(w).map(|at| (w, at))).collect();
            for (from, to) in moves {
                if let Some(n) = wins.remove(&from) {
                    *wins.entry(to).or_default() += n;
                }
                if let Some(n) = cells.remove(&from) {
                    *cells.entry(to).or_default() += n;
                }
            }
        }
        frames.push((sparse(cells), sparse(wins)));
        for (k, m) in marked.into_iter().enumerate() {
            layers[k].push(sparse(m));
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
pub const NO_POSITION: u32 = u32::MAX;

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

fn room_tiles(s: &crate::settings::Settings) -> Result<Tiles> {
    let room = s.room;
    let cart = s.cart_data().context("loading the cart (cart/: run from the repo root)")?;
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

/// `witness.txt` (`search --save-marks`): `inputs a,b,..`, then
/// `f x y` per frame from 0 (`f - -` without a player). A file for
/// `--witness` / `--reference` (`tools/align_tas.py`) may also carry, before
/// the frames and in any order, `label TEXT` and `dashes F:DIR ...`; the
/// frames may end early (at the exit).
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

/// `--arc DIR` (`search --save-marks DIR`, which holds the LAST level's
/// sets): the last forward level gets the winning sets' support, and a level
/// after it the ARC backward over the same tree; the optimum is the arc
/// search's (`last`: the record's last level). Returns the witness if present.
fn attach_arc(hr: &mut HorizonRun, optimal: &mut Option<u32>, dir: &Path, last: &super::result::LevelResult) -> Result<Option<Witness>> {
    let rows = |name: &str| -> Result<u64> { Ok(MarksMap::open(&dir.join(name))?.rows as u64) };
    let found = last.arc_bound;
    hr.partial = false;
    let li = hr.levels.len() - 1;
    let lr = &mut hr.levels[li];
    lr.precision = format!("{}, rem free", last.spec);
    lr.refuted = lr.first_win.is_none();
    lr.marked = Some(rows("level0.marks.bin")?);
    let mut arc = level_run(li + 1, format!("arc: {}, rem exact", last.spec), Vec::new(), found);
    arc.marked = found.map(|_| rows("arc.marks.bin")).transpose()?;
    arc.refuted = found.is_none();
    arc.forward_of = Some(li);
    hr.levels.push(arc);
    hr.refuted_at = hr.levels.iter().position(|l| l.refuted);
    *optimal = if hr.refuted_at.is_none() { found } else { None };
    let w = dir.join("witness.txt");
    w.exists().then(|| read_witness(&w)).transpose()
}

/// The concrete runs drawn over the room: `witness` (replacing the arc
/// directory's) and `reference`, files in `read_witness`'s format; and the
/// file the UI offers for download.
pub struct Paths<'a> {
    pub witness: Option<&'a Path>,
    pub reference: Option<&'a Path>,
    pub download: Option<&'a Path>,
}

/// The export. `checkpoint_dir` is a search's (`result.json`, `levelNN/`);
/// `room` the start room (the run's settings: the record's must agree);
/// `arc` its `search --save-marks` directory.
pub fn export(checkpoint_dir: &Path, out: &Path, s: &crate::settings::Settings, arc: Option<&Path>, paths: Paths) -> Result<()> {
    let room = s.room;
    let res = super::result::SearchResult::read(checkpoint_dir)?;
    anyhow::ensure!(res.room == room, "{}: the search ran in room {:?}, this run's --room is {room:?}", checkpoint_dir.display(), res.room);
    let last = res.levels.last().with_context(|| format!("{}: the search recorded no level ({:?})", checkpoint_dir.display(), res.verdict))?;
    // `h` counts search steps (the trees' frames).
    let h = res.horizon * res.steps_per_frame;
    let tree_dir = |level: usize| checkpoint_dir.join(&res.levels[level].dir);
    let mut levels = Vec::new();
    for (i, l) in res.levels.iter().enumerate() {
        levels.push(level_run(i, l.spec.clone(), read_forward_stats(&tree_dir(i))?, l.first_win));
    }
    let nforward = levels.len();
    let (wall_s, mut optimal) = (Some(res.wall_s), res.optimal);
    let mut hr = HorizonRun { h, levels, refuted_at: None, partial: true };
    let witness = match arc {
        Some(dir) => attach_arc(&mut hr, &mut optimal, dir, last)?,
        None => None,
    };
    let witness = match paths.witness {
        Some(p) => Some(read_witness(p)?),
        None => witness,
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

    // One pass per forward level's tree, reading its frames and its marked
    // sets by layer together.
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
        let (mut set_levels, mut maps): (Vec<usize>, Vec<MarksMap>) = (Vec::new(), Vec::new());
        if let Some(a) = arc.filter(|_| level == nforward - 1) {
            for (l, name) in [(level, "level0.marks.bin"), (level + 1, "arc.marks.bin")] {
                if !hr.levels[l].refuted {
                    set_levels.push(l);
                    maps.push(MarksMap::open(&a.join(name))?);
                }
            }
        }
        let (table, by_cell_dist) = if maps.is_empty() {
            (None, Vec::new())
        } else {
            let (table, by_cell_dist) = mark_table(&maps).with_context(|| format!("marks of {}", dir.display()))?;
            (Some(table), by_cell_dist)
        };
        let (data, layers) = read_tree(&dir, table.as_ref(), maps.len()).with_context(|| format!("frames of {}", dir.display()))?;
        eprintln!("[export-ui] {}: {} frames, {} marked sets in {:.1} s", dir.display(), data.frames.len(), maps.len(), tt.elapsed().as_secs_f64());
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
        tiles: room_tiles(s)?,
        wall_s,
        prebuild_s: None,
        optimal,
        horizons: vec![hr],
        witness,
        reference,
        download,
    };
    let mut f = std::io::BufWriter::new(std::fs::File::create(out.join("run.json"))?);
    serde_json::to_writer(&mut f, &run)?;
    f.flush()?;
    let total: u64 = std::fs::read_dir(out)?.filter_map(|e| e.ok()).filter_map(|e| e.metadata().ok()).map(|m| m.len()).sum();
    eprintln!("[export-ui] wrote {} ({:.1} MB) in {:.1} s", out.display(), total as f64 / 1e6, t.elapsed().as_secs_f64());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Marks files read in place, and a state in two sets is one entry
    /// holding both sets' bits.
    #[test]
    fn marks_files_read_in_place_into_one_table() {
        let dir = std::env::temp_dir().join(format!("celeste-ui-marks-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let save = |name: &str, rows: &[((u64, u64), u32, u64, u16)]| {
            let mut v = crate::frame::Visited::new(Default::default());
            for &(key, cell, shape, deadline) in rows {
                v.insert_until(shape, key, cell, deadline);
            }
            let path = dir.join(name);
            v.save(&path, 9).unwrap();
            MarksMap::open(&path).unwrap()
        };
        // dist = the horizon (9) minus the deadline.
        let sets = [
            save("a.bin", &[((1, 2), 3, 7, 9), ((5, 6), 3, 7, 9), ((1, 2), 4, 8, 9)]),
            save("b.bin", &[((1, 2), 3, 7, 7), ((0, 0), 3, 9, 8)]),
        ];
        assert_eq!((sets[0].rows, sets[1].rows), (3, 2));
        let (t, by_cell_dist) = mark_table(&sets).unwrap();
        let get = |s: u64, c: u32, k: (u64, u64)| t.shards[mark_shard(s, c)].get(&(s, c, k)).copied();
        assert_eq!(get(7, 3, (1, 2)), Some(0b11), "the shared state holds both sets");
        assert_eq!(get(7, 3, (5, 6)), Some(0b01));
        assert_eq!(get(9, 3, (0, 0)), Some(0b10));
        assert_eq!(get(7, 4, (1, 2)), None, "a key is looked up at its own cell");
        assert_eq!(t.shards.iter().map(|s| s.len()).sum::<usize>(), 4);
        assert!(t.cells.contains(&(8, 4)) && !t.cells.contains(&(8, 3)));
        assert_eq!(by_cell_dist[0], vec![(3, 0, 2), (4, 0, 1)]);
        assert_eq!(by_cell_dist[1], vec![(3, 1, 1), (3, 2, 1)], "sorted by (dist, cell)");
        std::fs::remove_dir_all(&dir).unwrap();
    }

    /// The forward's record as the forward writes it: a rerun frame's last
    /// row wins, the frames in order.
    #[test]
    fn a_trees_forward_record_reads_back() {
        let dir = std::env::temp_dir().join(format!("celeste-ui-fwd-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        assert!(read_forward_stats(&dir).unwrap().is_empty(), "a tree from before the record");
        let ms = std::time::Duration::from_millis;
        let row = |f: u32, kept: usize| {
            let st = crate::frame::FrameStats { lanes_kept: kept, wave_idle: 0.94, t_wave: ms(7), rss_end: 1.28, ..Default::default() };
            crate::frame::forward_stats_row(f, &st, ms(1), ms(2), ms(11), 4)
        };
        for (f, kept) in [(1, 10), (2, 20), (2, 21), (3, 30)] {
            crate::frame::append_forward_stats(&dir, &row(f, kept)).unwrap();
        }
        let got = read_forward_stats(&dir).unwrap();
        assert_eq!(got.iter().map(|l| (l.f, l.kept)).collect::<Vec<_>>(), vec![(1, 10), (2, 21), (3, 30)]);
        assert_eq!((got[0].emit_ms, got[0].emit_idle, got[0].ckpt_ms, got[0].total_ms, got[0].visited, got[0].rss_gb), (7, 94, 1, 11, 4, 1.28));
        std::fs::remove_dir_all(&dir).unwrap();
    }

    /// The search's `witness.txt` and a labelled path file with dashes that
    /// ends at the exit both read; a skipped frame does not.
    #[test]
    fn witness_and_reference_files_read() {
        let dir = std::env::temp_dir().join(format!("celeste-ui-witness-{}", std::process::id()));
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
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
