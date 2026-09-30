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
//! built: the export is a walk over headers plus, for the marks by layer,
//! the key column of the runs at marked cells.
//!
//! How it reads (2026-09-30; room (6,0) h144 levels 0-5, a 1.7G-row level
//! 0 and 70-100M marks per level: 556 s and 48 GB before, ~25 s and 13 GB
//! after, on a machine a search was running on). One checkpoint tree at a
//! time, each read by one worker per core frame by frame, the latest (the
//! largest) first. A tree's marked sets are read in place off the mapped
//! marks files (never deserialized as a whole) into hash shards by
//! (shape, cell), and a frame file's rows are probed against them - a run
//! at a cell with no marks is skipped without touching its keys. The marks
//! used to be one hash map built by one thread per tree and probed by that
//! thread with every row of the tree, which for level 0 was the whole
//! export.
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
    /// A forward on its own (`export-ui --forward-only`): level 0's frames
    /// up to `h`, no backward and no verdict.
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
/// With `forward_only` the log is one forward (`rewrite forward`): its
/// `[fwd]` lines are closed into one partial horizon at its last frame.
pub fn parse_log(text: &str, forward_only: bool) -> Result<(Vec<HorizonRun>, Option<f64>, Option<f64>, Option<u32>)> {
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
                _ => horizons.push(HorizonRun { h, levels: vec![run], refuted_at: None, partial: false }),
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
    if forward_only {
        if !horizons.is_empty() || !bwd.is_empty() || fwd.is_empty() {
            bail!(
                "--forward-only wants the log of one forward: {} [ladder] horizons, {} backward lines, {} forward lines",
                horizons.len(),
                bwd.len(),
                fwd.len()
            );
        }
        let h = fwd.last().map_or(0, |l| l.f);
        let run = LevelRun {
            level: 0,
            precision: "forward only".to_string(),
            fwd: std::mem::take(&mut fwd),
            bwd: Vec::new(),
            first_win: first_win.take(),
            marked: None,
            reruns: None,
            refuted: false,
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
        horizons.push(HorizonRun { h, levels: vec![run], refuted_at: None, partial: true });
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

/// `f(i)` for every `i < n`, on one worker per core pulling indices in
/// order; the results by index. The first error stops the pulling.
fn par_map<T: Send>(n: usize, f: impl Fn(usize) -> Result<T> + Sync) -> Result<Vec<T>> {
    use std::sync::atomic::{AtomicUsize, Ordering};
    let next = AtomicUsize::new(0);
    let workers = std::thread::available_parallelism().map(|p| p.get()).unwrap_or(8).min(n.max(1));
    let parts = std::thread::scope(|s| {
        let handles: Vec<_> = (0..workers)
            .map(|_| {
                s.spawn(|| -> Result<Vec<(usize, T)>> {
                    let mut mine = Vec::new();
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        if i >= n {
                            return Ok(mine);
                        }
                        match f(i) {
                            Ok(v) => mine.push((i, v)),
                            Err(e) => {
                                next.store(n, Ordering::Relaxed);
                                return Err(e);
                            }
                        }
                    }
                })
            })
            .collect();
        handles.into_iter().map(|h| h.join().expect("export worker panicked")).collect::<Result<Vec<_>>>()
    })?;
    let mut all: Vec<(usize, T)> = parts.into_iter().flatten().collect();
    all.sort_unstable_by_key(|p| p.0);
    Ok(all.into_iter().map(|p| p.1).collect())
}

/// A marks file, mapped. `Visited::save` writes `(shape, cell, k0, k1)`
/// rows (28 bytes each); trees from when the marks carried their distance
/// hold `(shape, cell, k0, k1, dist)` rows (32 bytes). The bincode `Vec`
/// length prefix says which:
/// the payload is `n x 32` or `n x 28`, never both. Rows are read in
/// place, never deserialized as a whole: a level's marks run to 100M rows
/// (room (6,0) h144: 1.5-2.8 GB per file).
struct MarksMap {
    map: memmap2::Mmap,
    rows: usize,
    stride: usize,
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
        let stride = if n.checked_mul(32) == Some(body) {
            32
        } else if n.checked_mul(28) == Some(body) {
            28
        } else {
            bail!("{}: {n} rows do not fit {body} payload bytes as 32- or 28-byte rows", path.display())
        };
        Ok(MarksMap { map, rows: n as usize, stride })
    }

    fn have_dist(&self) -> bool {
        self.stride == 32
    }

    /// Row `i` as `((shape, cell, key), dist)`; dist 0 without distances.
    fn row(&self, i: usize) -> ((u64, u32, (u64, u64)), u32) {
        let b = &self.map[MARKS_ROWS_AT + i * self.stride..MARKS_ROWS_AT + (i + 1) * self.stride];
        let u64_at = |o: usize| u64::from_le_bytes(b[o..o + 8].try_into().unwrap());
        let u32_at = |o: usize| u32::from_le_bytes(b[o..o + 4].try_into().unwrap());
        let dist = if self.stride == 32 { u32_at(28) } else { 0 };
        ((u64_at(0), u32_at(8), (u64_at(12), u64_at(20))), dist)
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

/// Every marked state of up to 64 sets over one tree, with the bitmask of
/// the sets that hold it, and the (shape, cell)s that hold any - so a
/// frame file's run at a cell with none is skipped unread, and a row of
/// one that has some is one probe.
///
/// A hash probe per row, not a merge against sorted marks: a run is a few
/// rows (one flush's survivors at one cell) while a cell's marks span every
/// layer, so a sorted lookup started cold per run and paid a binary search
/// of cache misses per row (2026-09-30: 70% of the export's CPU at room
/// (6,0)). A probe that misses - most rows are unmarked - is one cache miss.
struct MarkTable {
    shards: Vec<FxHashMap<MarkId, u64>>,
    cells: rustc_hash::FxHashSet<(u64, u32)>,
}

/// Rows of the marks files per chunk of the parallel read.
const MARKS_CHUNK: usize = 1 << 20;

/// The sets' table, and per set its `(cell, dist, count)` sorted by (dist,
/// cell) (dist 0 throughout when the file has none).
fn mark_table(sets: &[MarksMap]) -> Result<(MarkTable, Vec<Vec<(u32, u32, u32)>>)> {
    anyhow::ensure!(sets.len() <= 64, "at most 64 marked sets per tree");
    // The chunks: every set's rows, read in parallel into per-shard lists
    // and a per-(dist, cell) count.
    let chunks: Vec<(usize, usize)> =
        sets.iter().enumerate().flat_map(|(k, m)| (0..m.rows.div_ceil(MARKS_CHUNK)).map(move |c| (k, c))).collect();
    let read = par_map(chunks.len(), |i| {
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
    let built = par_map(MARK_SHARDS, |s| {
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

fn frame_counts(dir: &Path, f: u32, marks: Option<&MarkTable>, nsets: usize) -> Result<FrameCounts> {
    let mut out = FrameCounts { cells: FxHashMap::default(), wins: FxHashMap::default(), marked: vec![FxHashMap::default(); nsets] };
    let files = match crate::frame::frame_files(dir, f) {
        Ok(v) => v,
        Err(_) if !dir.join("frames").join(format!("f{f:03}")).exists() => Vec::new(),
        Err(e) => return Err(e).with_context(|| format!("{} frame {f}", dir.display())),
    };
    let mut per_set = vec![0u32; nsets];
    for file in files {
        for (cell, n) in file.cell_counts() {
            *out.cells.entry(cell).or_default() += n;
        }
        for &(_, cell) in file.win_rows() {
            *out.wins.entry(cell).or_default() += 1;
        }
        let Some(t) = marks else { continue };
        let shape = file.shape_hash();
        // A run is a few rows (one flush's survivors at one cell: ~4 at
        // room (6,0) level 0, 564M runs over the tree), and a cell's runs
        // are adjacent: the cell is looked up once, and a run at a cell
        // without marks is skipped without reading its keys.
        let mut last: Option<(u32, bool)> = None;
        for &(cell, start, len) in file.runs() {
            let marked = match last {
                Some((c, m)) if c == cell => m,
                _ => {
                    let m = t.cells.contains(&(shape, cell));
                    last = Some((cell, m));
                    m
                }
            };
            if !marked {
                continue;
            }
            let shard = &t.shards[mark_shard(shape, cell)];
            per_set.fill(0);
            for r in start..start + len {
                if let Some(&m) = shard.get(&(shape, cell, file.key_at(r))) {
                    let mut bits = m;
                    while bits != 0 {
                        per_set[bits.trailing_zeros() as usize] += 1;
                        bits &= bits - 1;
                    }
                }
            }
            for (s, &n) in per_set.iter().enumerate() {
                if n > 0 {
                    *out.marked[s].entry(cell).or_default() += n;
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

/// Read one level directory: its frames from the headers, and - in the
/// same pass over the files - the marked sets of `marks` split by layer
/// (result `[k]` is, per frame, the marked states of set `k` per cell: the
/// tree's `(cell, key)` rows intersected with the set). Frames in parallel.
fn read_tree(dir: &Path, marks: Option<&MarkTable>, nsets: usize) -> Result<(LevelData, Vec<Vec<Sparse>>)> {
    let fdir = dir.join("frames");
    let mut nframes = 0u32;
    for e in std::fs::read_dir(&fdir).with_context(|| format!("listing {}", fdir.display()))? {
        let name = e?.file_name();
        let name = name.to_string_lossy();
        if let Some(n) = name.strip_prefix('f').and_then(|s| s.parse::<u32>().ok()) {
            nframes = nframes.max(n + 1);
        }
    }
    // The level's position graph, to draw a won state where its player left
    // from (below). A tree without one draws wins at their own cells; one
    // that exists and does not load is an error, not a silent skip.
    let graph_path = dir.join("posgraph.bin");
    let graph = if graph_path.exists() {
        Some(crate::search::pos_graph::PosGraph::load(&graph_path).with_context(|| format!("loading {}", graph_path.display()))?)
    } else {
        None
    };
    // The latest frames are the largest: pulled first, so the tail of the
    // parallel pass is the small ones.
    let n = nframes as usize;
    let mut counts = par_map(n, |i| frame_counts(dir, (n - 1 - i) as u32, marks, nsets))?;
    counts.reverse();
    let mut frames: Vec<(Sparse, Sparse)> = Vec::with_capacity(n);
    let mut layers: Vec<Vec<Sparse>> = vec![Vec::with_capacity(n); nsets];
    for FrameCounts { mut cells, mut wins, marked } in counts {
        // A won state has left the room: its cell is the NEXT room's spawn,
        // one room over (`pos_graph::room_offset` puts the room it exits to
        // one room to the right), so drawn as-is the winning states sit in the box's far
        // corner and the animation never shows the player reach the exit
        // (room (4,0) f76 at (136,128), room (1,0) f99 at (136,124),
        // 2026-09-16). Draw each win cell where its player LEFT from instead:
        // of the cells with a recorded edge into it (the level's pos graph),
        // the one holding the most states at the previous frame. The count
        // moves whole, in both the states and the wins record, so totals are
        // unchanged; with no graph or no occupied source the cell stays.
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

/// A level's directory under a search's checkpoint base; a forward's
/// tree (`forward_only`) is its own level 0.
fn level_dir(base: &Path, h: u32, level: usize, forward_only: bool) -> PathBuf {
    if forward_only {
        base.to_path_buf()
    } else if level == 0 {
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
pub fn export(checkpoint_dir: &Path, log_path: &Path, out: &Path, room: (i16, i16), forward_only: bool) -> Result<()> {
    let text = std::fs::read_to_string(log_path).with_context(|| format!("reading {}", log_path.display()))?;
    let (mut horizons, wall_s, prebuild_s, optimal) = parse_log(&text, forward_only)?;
    eprintln!(
        "[export-ui] log: {} horizons, {} levels, optimal {:?}",
        horizons.len(),
        horizons.iter().map(|h| h.levels.len()).sum::<usize>(),
        optimal
    );
    std::fs::create_dir_all(out)?;

    // One pass per checkpoint tree: level 0's once, every finer level's,
    // each with the marked sets over it (level 0: one per horizon), its
    // frames and its marks split by layer read together. The trees go one
    // at a time, each read in parallel inside, so the memory is one tree's
    // marked sets at a time.
    struct Tree {
        h: u32,
        level: usize,
        marks_hs: Vec<u32>,
    }
    let mut trees: Vec<Tree> = Vec::new();
    for hr in &horizons {
        for lr in &hr.levels {
            // A forward on its own has no backward, so no marks.
            let marked = !lr.refuted && !forward_only;
            if lr.level == 0 {
                if !trees.iter().any(|t| t.level == 0) {
                    trees.push(Tree { h: hr.h, level: 0, marks_hs: Vec::new() });
                }
                if marked {
                    trees.iter_mut().find(|t| t.level == 0).expect("level 0's tree").marks_hs.push(hr.h);
                }
            } else {
                trees.push(Tree { h: hr.h, level: lr.level, marks_hs: if marked { vec![hr.h] } else { Vec::new() } });
            }
        }
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
    let mut results: Vec<Done> = Vec::new();
    let t = std::time::Instant::now();
    for tree in &trees {
        let tt = std::time::Instant::now();
        let dir = level_dir(checkpoint_dir, tree.h, tree.level, forward_only);
        let sets = tree
            .marks_hs
            .iter()
            .map(|&h| {
                let path = crate::frame::marks_path(checkpoint_dir, h, tree.level);
                MarksMap::open(&path).with_context(|| format!("marks {}", path.display()))
            })
            .collect::<Result<Vec<_>>>()?;
        let (table, by_cell_dist) = if sets.is_empty() {
            (None, Vec::new())
        } else {
            let (table, by_cell_dist) = mark_table(&sets).with_context(|| format!("marks of {}", dir.display()))?;
            (Some(table), by_cell_dist)
        };
        let t_marks = tt.elapsed();
        let (data, layers) = read_tree(&dir, table.as_ref(), sets.len()).with_context(|| format!("frames of {}", dir.display()))?;
        eprintln!(
            "[export-ui] {}: {} frames, {} marked sets ({} states) in {:.1} s ({:.1} s marks)",
            dir.display(),
            data.frames.len(),
            sets.len(),
            sets.iter().map(|m| m.rows).sum::<usize>(),
            tt.elapsed().as_secs_f64(),
            t_marks.as_secs_f64()
        );
        results.push(Done::Frames { h: tree.h, level: tree.level, data });
        if !sets.is_empty() {
            let outs = tree
                .marks_hs
                .iter()
                .zip(&sets)
                .zip(by_cell_dist.into_iter().zip(layers))
                .map(|((&h, m), (by_cell_dist, by_layer))| MarksOut { h, level: tree.level, have_dist: m.have_dist(), by_cell_dist, by_layer })
                .collect();
            results.push(Done::Marks(outs));
        }
    }
    eprintln!("[export-ui] read {} trees in {:.1} s", trees.len(), t.elapsed().as_secs_f64());

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

    /// Both marks layouts read in place, and a state in two sets is one
    /// entry holding both sets' bits (what level 0's horizons share).
    #[test]
    fn marks_files_read_in_place_into_one_table() {
        let dir = std::env::temp_dir().join(format!("celeste-ui-marks-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let (a, b) = (dir.join("a.bin"), dir.join("b.bin"));
        // Set 0 without distances (`Visited::save`'s 28-byte rows), set 1
        // with them (32-byte rows); they share the state (7, 3, (1, 2)).
        let rows_a: Vec<(u64, u32, u64, u64)> = vec![(7, 3, 1, 2), (7, 3, 5, 6), (8, 4, 1, 2)];
        let rows_b: Vec<(u64, u32, u64, u64, u32)> = vec![(7, 3, 1, 2, 2), (9, 3, 0, 0, 1)];
        crate::search::checkpoint::save_value_to(&a, &rows_a).unwrap();
        crate::search::checkpoint::save_value_to(&b, &rows_b).unwrap();
        let sets = [MarksMap::open(&a).unwrap(), MarksMap::open(&b).unwrap()];
        assert!(!sets[0].have_dist() && sets[1].have_dist());
        assert_eq!((sets[0].rows, sets[1].rows), (3, 2));
        assert_eq!(sets[1].row(0), ((7, 3, (1, 2)), 2));
        let (t, by_cell_dist) = mark_table(&sets).unwrap();
        let get = |s: u64, c: u32, k: (u64, u64)| t.shards[mark_shard(s, c)].get(&(s, c, k)).copied();
        assert_eq!(get(7, 3, (1, 2)), Some(0b11), "the shared state holds both sets");
        assert_eq!(get(7, 3, (5, 6)), Some(0b01));
        assert_eq!(get(9, 3, (0, 0)), Some(0b10));
        assert_eq!(get(7, 4, (1, 2)), None, "a key is looked up at its own cell");
        assert_eq!(t.shards.iter().map(|s| s.len()).sum::<usize>(), 4);
        assert!(t.cells.contains(&(8, 4)) && !t.cells.contains(&(8, 3)));
        assert_eq!(by_cell_dist[0], vec![(3, 0, 2), (4, 0, 1)], "no distances: dist 0");
        assert_eq!(by_cell_dist[1], vec![(3, 1, 1), (3, 2, 1)], "sorted by (dist, cell)");
        std::fs::remove_dir_all(&dir).unwrap();
    }

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
        assert_eq!(parse_log("[ladder] h3 level 0 (Bits(0)): NO WIN -> Refuted\nELAPSED 9098.66 s\n", false).unwrap().1, Some(9098.66));
        let (hs, wall, prebuild, optimal) = parse_log(log, false).unwrap();
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
        let hs2 = parse_log("[ladder] h5 level 0 (Bits(0)): first win f5, marked 12 states (fingerprint cda9202a7cbb2234), 34 edges read\n", false).unwrap().0;
        assert_eq!(hs2[0].levels[0].reruns, Some(34), "the BFS's line");
        let waves = &hs[1].levels[1].fwd[0];
        assert_eq!((waves.emit_ms, waves.emit_idle, waves.own_ms, waves.own_idle, waves.total_ms), (7, 94, 3, 0, 11));
        assert_eq!(waves.rss_gb, 1.28);
        assert_eq!(wall, Some(433.31));
        assert_eq!(prebuild, Some(8.8));
        assert_eq!(optimal, Some(3));
    }
}
