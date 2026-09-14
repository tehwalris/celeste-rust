//! `rewrite export-ui`: a finished search's checkpoint tree + run log ->
//! the compact static data the web UI (`ui/`) renders.
//!
//! What the UI shows is spatial: per (horizon, level, frame) how many
//! states sit in each player-position cell, which cells hold wins, and per
//! (horizon, level) the backward's marked states per cell by distance to
//! the win. All of it is in the checkpoint HEADERS - the cell index of a
//! frame file gives the per-cell state count as consecutive index
//! differences, the win list gives the win cells - and in the marks files
//! (`(shape, cell, key, dist)` rows). No row is decoded and no engine is
//! built, so the export is a walk over headers: ~120k files in seconds.
//!
//! Output layout (`--out DIR`):
//!
//! * `run.json` - the run: room, the cell box every binary indexes into,
//!   the room's tiles, and per horizon per level the log's per-frame
//!   forward stats, per-iteration backward stats, the ladder verdict and
//!   the names of the level's binary files. The UI's timeline, size plots
//!   and chapter list are all built from this one file.
//! * `l00.frames.bin` - level 0's frames (persistent across horizons, so
//!   exported once); `hHHH_lLL.frames.bin` for every finer level.
//!   Format: `"CUF1" | u32 nframes | nframes x (u32 cells_off, u32
//!   ncells, u32 wins_off, u32 nwins) | data`, where a cells record at
//!   `cells_off` (bytes from file start) is `ncells x u32 idx` followed by
//!   `ncells x u32 count`, and a wins record likewise; `idx` is
//!   `(x - box.x0) + (y - box.y0) * box.w`, or `NO_POSITION` (u32::MAX)
//!   for a state whose player object is gone. Frame f is entry f.
//! * `hHHH_lLL.marks.bin` - the marked set of that (horizon, level):
//!   `"CUM1" | u32 n | n x u32 idx | n x u32 dist | n x u32 count`, sorted
//!   by (dist, idx). `dist` is the state's distance to a win, i.e. the
//!   backward iteration `horizon - dist` that marked it, so an ascending
//!   prefix is the backward growing from the wins. A marks file written
//!   before `dist` was stored (4-tuple rows) exports every dist as 0 and
//!   `marks_have_dist: false` in `run.json`.
//! * `hHHH_lLL.mlayers.bin` - the same marked set split by the FRAME each
//!   state was first reached at (its BFS layer): the frames format above
//!   (magic `"CUL1"`, empty win records), entry f holding the marked
//!   states of layer f per cell. This is the marks against the forward
//!   that produced them - a level's forward under its backward - and it
//!   comes from intersecting the marks with each frame file's `(cell,
//!   key)` rows, which is the one place the export reads the data region.
//!
//! Everything is little-endian u32 at 4-byte alignment, so the UI reads
//! it as `Uint32Array` views without a parser.

use anyhow::{bail, Context, Result};
use rustc_hash::FxHashMap;
use serde::Serialize;
use std::io::Write;
use std::path::{Path, PathBuf};

use crate::search::pos_graph::cell_xy;

/// One forward frame's line of the run log.
#[derive(Serialize, Clone, Debug)]
pub struct FwdLine {
    pub f: u32,
    pub in_blocks: u64,
    pub in_lanes: u64,
    pub raw: u64,
    pub kept: u64,
    pub out_blocks: u64,
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

/// One backward iteration's line of the run log.
#[derive(Serialize, Clone, Debug)]
pub struct BwdLine {
    pub f: u32,
    pub targets: u64,
    pub cand_cells: u64,
    pub loaded: u64,
    pub rerun: u64,
    pub marked: u64,
    pub load_thread_ms: u64,
    pub par_ms: u64,
    pub par_idle: u32,
    pub total_ms: u64,
}

/// One (horizon, level) of the ladder as the log tells it.
#[derive(Serialize, Clone, Debug)]
pub struct LevelRun {
    pub level: usize,
    /// `"Bits(k)"` or `"Exact"`.
    pub precision: String,
    pub fwd: Vec<FwdLine>,
    pub bwd: Vec<BwdLine>,
    pub first_win: Option<u32>,
    pub marked: Option<u64>,
    pub reruns: Option<u64>,
    pub refuted: bool,
    /// Frames present in the checkpoint tree (f0..frames-1).
    pub frames: u32,
    pub frames_file: String,
    pub marks_file: Option<String>,
    /// Per frame, the total states in the frontier and the win count
    /// (from the checkpoint files - the log's `kept` is the same number
    /// for a fresh frame, but the tree is the truth the heatmap draws).
    pub frame_states: Vec<u64>,
    pub frame_wins: Vec<u64>,
    pub marks_have_dist: bool,
    pub marks_by_dist: Vec<u64>,
    pub mlayers_file: Option<String>,
    /// Per frame, the marked states of that layer.
    pub marks_by_layer: Vec<u64>,
}

#[derive(Serialize, Clone, Debug)]
pub struct HorizonRun {
    pub h: u32,
    pub levels: Vec<LevelRun>,
    pub refuted_at: Option<usize>,
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
}

fn num<T: std::str::FromStr>(tok: &str) -> Result<T>
where
    T::Err: std::fmt::Display,
{
    let t = tok.trim_matches(|c: char| !(c.is_ascii_digit() || c == '.' || c == '-'));
    t.parse::<T>().map_err(|e| anyhow::anyhow!("parsing {tok:?}: {e}"))
}

/// The token after the `nth` occurrence (0-based) of `key`.
fn after<'a>(toks: &[&'a str], key: &str, nth: usize) -> Result<&'a str> {
    let mut seen = 0;
    for (i, t) in toks.iter().enumerate() {
        if *t == key {
            if seen == nth {
                return toks.get(i + 1).copied().ok_or_else(|| anyhow::anyhow!("nothing after {key}"));
            }
            seen += 1;
        }
    }
    bail!("no token {key:?} (#{nth}) in line")
}

fn parse_fwd(line: &str) -> Result<FwdLine> {
    let t: Vec<&str> = line.split_whitespace().collect();
    let (ib, il) = after(&t, "in", 0)?.split_once('/').context("in b/l")?;
    let (ob, ol) = after(&t, "out", 0)?.split_once('/').context("out b/l")?;
    Ok(FwdLine {
        f: num(t[1])?,
        in_blocks: num(ib)?,
        in_lanes: num(il)?,
        raw: num(after(&t, "raw", 0)?)?,
        kept: num(after(&t, "kept", 0)?)?,
        out_blocks: num(ob)?,
        out_lanes: num(ol)?,
        visited: num(after(&t, "visited", 0)?)?,
        // The waves frame (2026-09-13) logs `wave` / `door` where the
        // two-phase frame logged `emit` / `own`; both read.
        emit_ms: num(after(&t, "wave", 0).or_else(|_| after(&t, "emit", 0))?)?,
        emit_idle: num(after(&t, "(idle", 0)?)?,
        own_ms: num(after(&t, "door", 0).or_else(|_| after(&t, "own", 0))?)?,
        own_idle: after(&t, "(idle", 1).ok().map(num).transpose()?.unwrap_or(0),
        ckpt_ms: num(after(&t, "ckpt", 0)?)?,
        pos_ms: num(after(&t, "pos", 0)?)?,
        total_ms: num(after(&t, "total", 0)?)?,
        // `rss X GB` before 2026-09-13; `rss start A wave B end C peak D
        // GB` since - the frame's end value is the one the old line gave.
        rss_gb: match after(&t, "rss", 0)? {
            "start" => num(after(&t, "end", 0)?)?,
            v => num(v)?,
        },
    })
}

fn parse_bwd(line: &str) -> Result<BwdLine> {
    let t: Vec<&str> = line.split_whitespace().collect();
    Ok(BwdLine {
        f: num(t[1])?,
        targets: num(after(&t, "targets", 0)?)?,
        cand_cells: num(after(&t, "cand-cells", 0)?)?,
        loaded: num(after(&t, "loaded", 0)?)?,
        rerun: num(after(&t, "rerun", 0)?)?,
        marked: num(after(&t, "marked", 0)?)?,
        load_thread_ms: num(after(&t, "load", 0)?)?,
        par_ms: num(after(&t, "par", 0)?)?,
        par_idle: num(after(&t, "(idle", 0)?)?,
        total_ms: num(after(&t, "total", 0)?)?,
    })
}

/// Parse the run log into horizons in execution order. A `[ladder] hH
/// level L` line closes the `[fwd]`/`[bwd]` lines since the previous one
/// as that (horizon, level); the initial level-0 forward therefore lands
/// in the first horizon's level 0, and each later horizon's level 0 holds
/// the one frame it was extended by.
pub fn parse_log(text: &str) -> Result<(Vec<HorizonRun>, Option<f64>, Option<f64>, Option<u32>)> {
    let mut horizons: Vec<HorizonRun> = Vec::new();
    let mut fwd: Vec<FwdLine> = Vec::new();
    let mut bwd: Vec<BwdLine> = Vec::new();
    let mut first_win: Option<u32> = None;
    let (mut wall, mut prebuild, mut optimal) = (None, None, None);
    for (ln, line) in text.lines().enumerate() {
        let ctx = || format!("log line {}: {line}", ln + 1);
        if line.starts_with("[fwd] first win at f") {
            first_win = Some(num(line.rsplit(' ').next().unwrap_or("")).with_context(ctx)?);
        } else if line.starts_with("[fwd] f") {
            fwd.push(parse_fwd(line).with_context(ctx)?);
        } else if line.starts_with("[bwd] f") {
            bwd.push(parse_bwd(line).with_context(ctx)?);
        } else if line.starts_with("[ladder] h") {
            let t: Vec<&str> = line.split_whitespace().collect();
            let h: u32 = num(t[1]).with_context(ctx)?;
            let level: usize = num(t[3]).with_context(ctx)?;
            // `(Bits(0)):` -> `Bits(0)`: strip exactly the wrapping parens and
            // the colon (a `trim_matches` on ')' ate the inner one too).
            let precision = t[4]
                .strip_suffix("):")
                .or_else(|| t[4].strip_suffix(')'))
                .and_then(|s| s.strip_prefix('('))
                .unwrap_or(t[4])
                .to_string();
            let refuted = line.contains("NO WIN");
            let (marked, reruns) = if refuted {
                (None, None)
            } else {
                // `N re-runs` (the kernel walk) or `N edges read` (the BFS,
                // 2026-09-13): the number before the trailing word(s).
                let work = if line.ends_with("edges read") { t[t.len() - 3] } else { t[t.len() - 2] };
                (
                    Some(num(after(&t, "marked", 0)?).with_context(ctx)?),
                    Some(num(work).with_context(ctx)?),
                )
            };
            let run = LevelRun {
                level,
                precision,
                fwd: std::mem::take(&mut fwd),
                bwd: std::mem::take(&mut bwd),
                // The ladder line carries the level's first win even when
                // this horizon only extended it (no `[fwd] first win` line).
                first_win: if refuted {
                    None
                } else {
                    Some(num(after(&t, "win", 0)?).with_context(ctx)?).or(first_win.take())
                },
                marked,
                reruns,
                refuted,
                frames: 0,
                frames_file: String::new(),
                marks_file: None,
                frame_states: Vec::new(),
                frame_wins: Vec::new(),
                marks_have_dist: false,
                marks_by_dist: Vec::new(),
                mlayers_file: None,
                marks_by_layer: Vec::new(),
            };
            first_win = None;
            match horizons.last_mut() {
                Some(hr) if hr.h == h => hr.levels.push(run),
                _ => horizons.push(HorizonRun { h, levels: vec![run], refuted_at: None }),
            }
        } else if line.starts_with("[search] horizon ") {
            let t: Vec<&str> = line.split_whitespace().collect();
            if let Some(hr) = horizons.last_mut() {
                hr.refuted_at = Some(num(t[t.len() - 1]).with_context(ctx)?);
            }
        } else if line.starts_with("[asm build] ") && line.contains("prebuilt in") {
            let t: Vec<&str> = line.split_whitespace().collect();
            prebuild = Some(num(t[t.len() - 2]).with_context(ctx)?);
        } else if line.starts_with("OPTIMAL win frame: ") {
            optimal = Some(num(line.rsplit(' ').next().unwrap_or("")).with_context(ctx)?);
        } else if line.starts_with("ELAPSED ") {
            // The search's own `ELAPSED 9098.66 s` line (no GNU time).
            wall = Some(num(line.split_whitespace().nth(1).unwrap_or("")).with_context(ctx)?);
        } else if line.contains("Elapsed (wall clock) time") {
            // `h:mm:ss or m:ss`.
            let clock = line.rsplit(' ').next().unwrap_or("");
            let mut secs = 0.0;
            for part in clock.split(':') {
                secs = secs * 60.0 + num::<f64>(part).with_context(ctx)?;
            }
            wall = Some(secs);
        }
    }
    if !fwd.is_empty() || !bwd.is_empty() {
        bail!("log ends with {} forward / {} backward lines not closed by a [ladder] line", fwd.len(), bwd.len());
    }
    Ok((horizons, wall, prebuild, optimal))
}

/// Sparse per-cell counts of one frame: `(cell, count)` ascending by cell.
type Sparse = Vec<(u32, u32)>;

struct LevelData {
    /// Per frame: the states per cell, the wins per cell.
    frames: Vec<(Sparse, Sparse)>,
}

/// Read one level directory's frames, headers only.
fn read_level_frames(dir: &Path) -> Result<LevelData> {
    let fdir = dir.join("frames");
    let mut nframes = 0u32;
    for e in std::fs::read_dir(&fdir).with_context(|| format!("listing {}", fdir.display()))? {
        let name = e?.file_name();
        let name = name.to_string_lossy();
        if let Some(n) = name.strip_prefix('f').and_then(|s| s.parse::<u32>().ok()) {
            nframes = nframes.max(n + 1);
        }
    }
    let mut frames = Vec::with_capacity(nframes as usize);
    for f in 0..nframes {
        let mut cells: FxHashMap<u32, u32> = FxHashMap::default();
        let mut wins: FxHashMap<u32, u32> = FxHashMap::default();
        let files = match crate::frame::frame_files(dir, f) {
            Ok(v) => v,
            Err(_) if !fdir.join(format!("f{f:03}")).exists() => Vec::new(),
            Err(e) => return Err(e).with_context(|| format!("{} frame {f}", dir.display())),
        };
        for file in files {
            for (cell, n) in file.cell_counts() {
                *cells.entry(cell).or_default() += n;
            }
            for (_, _, cell) in file.wins() {
                *wins.entry(cell).or_default() += 1;
            }
        }
        let mut cells: Sparse = cells.into_iter().collect();
        cells.sort_unstable();
        let mut wins: Sparse = wins.into_iter().collect();
        wins.sort_unstable();
        frames.push((cells, wins));
    }
    Ok(LevelData { frames })
}

/// One marked state: `(shape, cell, key)`, as the frame rows are keyed.
type MarkId = (u64, u32, u64, u64);

/// A marks file: the states, and their distances if the file has them.
struct MarksFile {
    ids: Vec<MarkId>,
    dist: Option<Vec<u32>>,
}

/// Read a marks file in either layout. `Marks::save` writes `(shape,
/// cell, k0, k1, dist)` rows (32 bytes each); trees written before the
/// distance was stored hold `(shape, cell, k0, k1)` rows (28 bytes). The
/// bincode `Vec` length prefix says which: the payload is `n x 32` or
/// `n x 28`, never both.
fn read_marks_file(path: &Path) -> Result<MarksFile> {
    let len = std::fs::metadata(path)?.len();
    let bytes = std::fs::read(path)?;
    anyhow::ensure!(bytes.len() >= 24, "{}: too short for a marks file", path.display());
    let n = u64::from_le_bytes(bytes[16..24].try_into().unwrap());
    let payload = len - 24;
    if payload == n * 32 {
        let rows: Vec<(u64, u32, u64, u64, u32)> = crate::search::checkpoint::load_value_from(path)?;
        let dist = rows.iter().map(|r| r.4).collect();
        Ok(MarksFile { ids: rows.into_iter().map(|(s, c, a, b, _)| (s, c, a, b)).collect(), dist: Some(dist) })
    } else if payload == n * 28 {
        let rows: Vec<(u64, u32, u64, u64)> = crate::search::checkpoint::load_value_from(path)?;
        Ok(MarksFile { ids: rows, dist: None })
    } else {
        bail!("{}: {n} rows do not fit {payload} payload bytes as 32- or 28-byte rows", path.display())
    }
}

/// `(cell, dist, count)` sorted by (dist, cell); dist 0 throughout when
/// the file has none.
fn marks_by_cell_dist(m: &MarksFile) -> Vec<(u32, u32, u32)> {
    let mut agg: FxHashMap<(u32, u32), u32> = FxHashMap::default();
    for (i, id) in m.ids.iter().enumerate() {
        let d = m.dist.as_ref().map(|v| v[i]).unwrap_or(0);
        *agg.entry((d, id.1)).or_default() += 1;
    }
    let mut v: Vec<(u32, u32, u32)> = agg.into_iter().map(|((d, c), n)| (c, d, n)).collect();
    v.sort_unstable_by_key(|&(c, d, _)| (d, c));
    v
}

/// Split several marked sets over ONE frames tree by layer: result `[k]`
/// is, per frame, the marked states of set `k` per cell. One pass over the
/// tree's `(cell, key)` rows against a state -> set-bitmask map, so level
/// 0's 150M rows are read once for all its horizons.
fn marks_by_layer(dir: &Path, sets: &[&MarksFile]) -> Result<Vec<Vec<Sparse>>> {
    anyhow::ensure!(sets.len() <= 64, "at most 64 marked sets per tree");
    let mut mask: FxHashMap<MarkId, u64> = FxHashMap::default();
    for (k, set) in sets.iter().enumerate() {
        for id in &set.ids {
            *mask.entry(*id).or_default() |= 1 << k;
        }
    }
    let fdir = dir.join("frames");
    let mut nframes = 0u32;
    for e in std::fs::read_dir(&fdir)? {
        let name = e?.file_name();
        if let Some(n) = name.to_string_lossy().strip_prefix('f').and_then(|s| s.parse::<u32>().ok()) {
            nframes = nframes.max(n + 1);
        }
    }
    let mut out: Vec<Vec<Sparse>> = vec![Vec::with_capacity(nframes as usize); sets.len()];
    for f in 0..nframes {
        let mut per_set: Vec<FxHashMap<u32, u32>> = vec![FxHashMap::default(); sets.len()];
        let files = match crate::frame::frame_files(dir, f) {
            Ok(v) => v,
            Err(_) if !fdir.join(format!("f{f:03}")).exists() => Vec::new(),
            Err(e) => return Err(e).with_context(|| format!("{} frame {f}", dir.display())),
        };
        for file in files {
            let shape = file.shape_hash();
            for (cell, key) in file.cell_keys() {
                if let Some(&m) = mask.get(&(shape, cell, key.0, key.1)) {
                    let mut bits = m;
                    while bits != 0 {
                        let k = bits.trailing_zeros() as usize;
                        bits &= bits - 1;
                        *per_set[k].entry(cell).or_default() += 1;
                    }
                }
            }
        }
        for (k, agg) in per_set.into_iter().enumerate() {
            let mut v: Sparse = agg.into_iter().collect();
            v.sort_unstable();
            out[k].push(v);
        }
    }
    Ok(out)
}

fn level_dir(base: &Path, h: u32, level: usize) -> PathBuf {
    if level == 0 {
        base.join("level00")
    } else {
        base.join(format!("h{h:03}")).join(format!("level{level:02}"))
    }
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

fn room_tiles(room: (i16, i16)) -> Result<Tiles> {
    let cart = celeste_core::cart_data::CartData::load("cart").context("loading cart/ (run from the repo root)")?;
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

/// The export. `room` is the start room the tree was searched from.
pub fn export(checkpoint_dir: &Path, log_path: &Path, out: &Path, room: (i16, i16)) -> Result<()> {
    let text = std::fs::read_to_string(log_path).with_context(|| format!("reading {}", log_path.display()))?;
    let (mut horizons, wall_s, prebuild_s, optimal) = parse_log(&text)?;
    eprintln!(
        "[export-ui] log: {} horizons, {} levels, optimal {:?}",
        horizons.len(),
        horizons.iter().map(|h| h.levels.len()).sum::<usize>(),
        optimal
    );
    std::fs::create_dir_all(out)?;

    // Work items: level 0's frames once, every finer level's frames, and
    // per frames tree the marked sets over it (level 0: one per horizon).
    enum Job {
        Frames { h: u32, level: usize },
        Marks { level: usize, hs: Vec<u32> },
    }
    let mut jobs: Vec<Job> = Vec::new();
    let mut l0_hs: Vec<u32> = Vec::new();
    let mut have_l0 = false;
    for hr in &horizons {
        for lr in &hr.levels {
            if lr.level == 0 {
                if !have_l0 {
                    jobs.push(Job::Frames { h: hr.h, level: 0 });
                    have_l0 = true;
                }
                if !lr.refuted {
                    l0_hs.push(hr.h);
                }
            } else {
                jobs.push(Job::Frames { h: hr.h, level: lr.level });
                if !lr.refuted {
                    jobs.push(Job::Marks { level: lr.level, hs: vec![hr.h] });
                }
            }
        }
    }
    if !l0_hs.is_empty() {
        jobs.push(Job::Marks { level: 0, hs: l0_hs });
    }
    struct MarksOut {
        h: u32,
        level: usize,
        have_dist: bool,
        by_cell_dist: Vec<(u32, u32, u32)>,
        by_layer: Vec<Sparse>,
    }
    enum Done {
        Frames { h: u32, level: usize, data: LevelData },
        Marks(Vec<MarksOut>),
    }
    let next = std::sync::atomic::AtomicUsize::new(0);
    let results = std::sync::Mutex::new(Vec::new());
    let t = std::time::Instant::now();
    let workers = std::thread::available_parallelism().map(|n| n.get()).unwrap_or(8).min(16);
    std::thread::scope(|s| -> Result<()> {
        let handles: Vec<_> = (0..workers)
            .map(|_| {
                s.spawn(|| -> Result<()> {
                    loop {
                        let i = next.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                        let Some(job) = jobs.get(i) else { return Ok(()) };
                        let done = match *job {
                            Job::Frames { h, level } => {
                                let dir = level_dir(checkpoint_dir, h, level);
                                let data = read_level_frames(&dir)
                                    .with_context(|| format!("frames of {}", dir.display()))?;
                                Done::Frames { h, level, data }
                            }
                            Job::Marks { level, ref hs } => {
                                let files = hs
                                    .iter()
                                    .map(|&h| {
                                        let path = crate::frame::marks_path(checkpoint_dir, h, level);
                                        read_marks_file(&path).with_context(|| format!("marks {}", path.display()))
                                    })
                                    .collect::<Result<Vec<_>>>()?;
                                let refs: Vec<&MarksFile> = files.iter().collect();
                                let dir = level_dir(checkpoint_dir, hs[0], level);
                                let layers = marks_by_layer(&dir, &refs)
                                    .with_context(|| format!("marks by layer over {}", dir.display()))?;
                                let outs = hs
                                    .iter()
                                    .zip(files.iter())
                                    .zip(layers)
                                    .map(|((&h, m), by_layer)| MarksOut {
                                        h,
                                        level,
                                        have_dist: m.dist.is_some(),
                                        by_cell_dist: marks_by_cell_dist(m),
                                        by_layer,
                                    })
                                    .collect();
                                Done::Marks(outs)
                            }
                        };
                        results.lock().unwrap().push(done);
                    }
                })
            })
            .collect();
        for h in handles {
            h.join().expect("export worker panicked")?;
        }
        Ok(())
    })?;
    let results = results.into_inner().unwrap();
    eprintln!("[export-ui] read {} jobs in {:.1} s", results.len(), t.elapsed().as_secs_f64());

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
            Done::Frames { h, level, data } => {
                let name = if *level == 0 { "l00.frames.bin".to_string() } else { format!("h{h:03}_l{level:02}.frames.bin") };
                write_frames_bin(&out.join(&name), data, b"CUF1", &cell_box)?;
                let states: Vec<u64> = data.frames.iter().map(|(c, _)| c.iter().map(|&(_, n)| n as u64).sum()).collect();
                let wins: Vec<u64> = data.frames.iter().map(|(_, w)| w.iter().map(|&(_, n)| n as u64).sum()).collect();
                for hr in horizons.iter_mut() {
                    for lr in hr.levels.iter_mut() {
                        if lr.level == *level && (*level == 0 || hr.h == *h) {
                            lr.frames = data.frames.len() as u32;
                            lr.frames_file = name.clone();
                            lr.frame_states = states.clone();
                            lr.frame_wins = wins.clone();
                        }
                    }
                }
            }
            Done::Marks(outs) => {
                for o in outs {
                    let (h, level) = (o.h, o.level);
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
                    let lr = horizons
                        .iter_mut()
                        .find(|hr| hr.h == h)
                        .and_then(|hr| hr.levels.iter_mut().find(|l| l.level == level))
                        .expect("job came from the log");
                    lr.marks_file = Some(name);
                    lr.marks_have_dist = o.have_dist;
                    lr.marks_by_dist = by_dist;
                    lr.mlayers_file = Some(lname);
                    lr.marks_by_layer = o.by_layer.iter().map(|c| c.iter().map(|&(_, n)| n as u64).sum()).collect();
                }
            }
        }
    }
    for hr in &horizons {
        for lr in &hr.levels {
            if lr.frames_file.is_empty() {
                bail!("h{} level {}: no frames exported", hr.h, lr.level);
            }
        }
    }

    let run = Run {
        format: 1,
        room,
        cell_box,
        tiles: room_tiles(room)?,
        wall_s,
        prebuild_s,
        optimal,
        horizons,
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

    #[test]
    fn the_log_lines_parse_and_close_into_horizons() {
        let log = "\
[asm build] 17 rungs prebuilt in 8.8 s
[fwd] f001 in 1/1 raw 1 kept 1 out 1/1 visited 2 | emit 1 (idle 99%) own 1 (idle 100%) ckpt 0 pos 0 total 2 ms | rss 4.24 GB
[search] level 0 has no win by f1
[fwd] f002 in 32/4129487 raw 24530998 kept 4286939 out 32/4286939 visited 146895302 | emit 1302 (idle 3%) own 197 (idle 5%) ckpt 157 pos 2 total 1667 ms | rss 12.55 GB
[fwd] first win at f2
[bwd] f001 targets 1 cand-cells 29 loaded 160 rerun 160 marked 160 | load 5 (thread-ms) par 2 (idle 54%) total 3 ms
[ladder] h2 level 0 (Bits(0)): first win f2, marked 7857 states (fingerprint cda9202a7cbb2234), 392258 re-runs
[fwd] f001 in 1/1 raw 1 kept 1 out 1/1 visited 2 | emit 1 (idle 100%) own 1 (idle 100%) ckpt 0 pos 0 total 1 ms | rss 13.35 GB
[ladder] h2 level 1 (Bits(1)): NO WIN -> Refuted
[search] horizon 2 refuted at level 1
[fwd] f003 in 1/1 raw 1 kept 1 out 1/1 visited 2 | emit 1 (idle 100%) own 1 (idle 100%) ckpt 0 pos 0 total 1 ms | rss 13.35 GB
[bwd] f002 targets 1 cand-cells 29 loaded 160 rerun 160 marked 160 | load 5 (thread-ms) par 2 (idle 54%) total 3 ms
[ladder] h3 level 0 (Bits(0)): first win f2, marked 9 states (fingerprint cda9202a7cbb2234), 10 re-runs
[fwd] f003 in 1/1 raw 1 kept 1 out 1/1 visited 4 | wave 7 (idle 94%) door 3 ckpt 0 pos 0 total 11 ms | flushes 1 (1 rows avg) | in 0.00 queues 0.00 door 0.00 GB rss start 1.26 wave 1.27 end 1.28 peak 8.25 GB
[ladder] h3 level 16 (Exact): first win f3, marked 1 states (fingerprint cda9202a7cbb2234), 2 re-runs
OPTIMAL win frame: 3
	Elapsed (wall clock) time (h:mm:ss or m:ss): 7:13.31
";
        assert_eq!(parse_log("[ladder] h3 level 0 (Bits(0)): NO WIN -> Refuted\nELAPSED 9098.66 s\n").unwrap().1, Some(9098.66));
        let (hs, wall, prebuild, optimal) = parse_log(log).unwrap();
        assert_eq!(hs.len(), 2);
        assert_eq!((hs[0].h, hs[1].h), (2, 3));
        let l0 = &hs[0].levels[0];
        assert_eq!(l0.fwd.len(), 2);
        assert_eq!(l0.fwd[1].emit_idle, 3);
        assert_eq!(l0.fwd[1].own_idle, 5);
        assert_eq!(l0.fwd[1].rss_gb, 12.55);
        assert_eq!(l0.bwd[0].par_idle, 54);
        assert_eq!(l0.first_win, Some(2));
        assert_eq!(l0.marked, Some(7857));
        assert_eq!(l0.reruns, Some(392258));
        let l1 = &hs[0].levels[1];
        assert!(l1.refuted && l1.first_win.is_none() && l1.bwd.is_empty());
        assert_eq!(hs[0].refuted_at, Some(1));
        assert_eq!(hs[1].levels[0].fwd.len(), 1);
        assert_eq!(hs[1].levels[0].first_win, Some(2));
        assert_eq!(hs[1].levels[1].precision, "Exact");
        assert_eq!(l0.precision, "Bits(0)", "the inner paren survives");
        let hs2 = parse_log("[ladder] h5 level 0 (Bits(0)): first win f5, marked 12 states (fingerprint cda9202a7cbb2234), 34 edges read\n").unwrap().0;
        assert_eq!(hs2[0].levels[0].reruns, Some(34), "the BFS's line");
        let waves = &hs[1].levels[1].fwd[0];
        assert_eq!((waves.emit_ms, waves.emit_idle, waves.own_ms, waves.own_idle, waves.total_ms), (7, 94, 3, 0, 11));
        assert_eq!(waves.rss_gb, 1.28);
        assert_eq!(wall, Some(433.31));
        assert_eq!(prebuild, Some(8.8));
        assert_eq!(optimal, Some(3));
    }
}
