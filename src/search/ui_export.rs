//! `rewrite export-ui`: a finished search's checkpoint trees + run log ->
//! the compact static data the web UI (`ui/`) renders.
//!
//! Per (level, frame): states and wins per player-position cell; per marked
//! set (`search --save-marks`: remainder-free marks, arc-marked nodes): the
//! marked states per cell by distance to the win. All of it comes from the
//! checkpoint HEADERS (cell index, win list), the marks files, and the key
//! column at marked cells: no row is decoded, no engine built.
//!
//! The input is a `rewrite search` checkpoint directory (`levelNN/` per
//! `--level`) or one `rewrite forward` tree, with its log: one horizon whose
//! levels are the forward levels, plus with `--arc` a last level carrying the
//! arc backward over the last forward tree (`forward_of`). Some JSON fields
//! (`bwd`, `reruns`, several horizons) are kept only because the UI reads them.
//!
//! Output layout (`--out DIR`):
//!
//! * `run.json` - the run: room, the cell box every binary indexes into,
//!   the room's tiles, and per level the log's per-frame forward stats and
//!   the names of the level's binary files.
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
}

/// A concrete witness: its inputs and the player's (x, y) per frame (frame 0
/// the start; `None` without a player), in the cells' pixel coordinates, and
/// the frames a dash starts at with its direction (`R`, `U`, ...; only when
/// the file lists them). With `db` / `prologue` / `seeds` lines, also the run
/// as a tasdatabase file (`tas`).
#[derive(Serialize, Debug)]
pub struct Witness {
    pub label: String,
    pub inputs: Vec<u8>,
    pub path: Vec<Option<(i32, i32)>>,
    pub dashes: Vec<(u32, String)>,
    pub tas: Option<TasFile>,
}

/// A run as the community's TAS tool (UniversalClassicTas) clean-saves it,
/// the file the tasdatabase upload takes: `[` each seed then `,` `]`, each
/// input byte then `,`, no newline, starting at the room's first
/// controllable frame - the spawn PROLOGUE (the frames before the player
/// exists; the earliest offset the community TAS exits at) is not in it.
/// Named `TAS<level>.tas` (2900m: level 29). `frames` is the database's
/// count: inputs after the prologue - 1.
#[derive(Serialize, Debug, PartialEq)]
pub struct TasFile {
    /// `TAS29.tas`: the database's file name for the level.
    pub file: String,
    pub text: String,
    pub frames: u32,
    pub prologue: u32,
}

impl TasFile {
    fn new(name: &str, prologue: usize, seeds: &[u32], inputs: &[u8]) -> Result<Self> {
        anyhow::ensure!(prologue < inputs.len(), "prologue {prologue}: only {} inputs", inputs.len());
        // The prologue is dropped, so it must not press anything.
        anyhow::ensure!(inputs[..prologue].iter().all(|&b| b == 0), "the prologue's {prologue} inputs are not all 0: {:?}", &inputs[..prologue]);
        let level: u32 = name.strip_suffix('m').and_then(|m| m.parse::<u32>().ok()).filter(|m| m % 100 == 0).map(|m| m / 100).with_context(|| format!("{name}: not a level name like 2900m"))?;
        let each = |v: Vec<String>| v.iter().map(|x| format!("{x},")).collect::<String>();
        let text = format!(
            "[{}]{}",
            each(seeds.iter().map(|s| s.to_string()).collect()),
            each(inputs[prologue..].iter().map(|b| b.to_string()).collect())
        );
        Ok(TasFile { file: format!("TAS{level}.tas"), text, frames: (inputs.len() - prologue - 1) as u32, prologue: prologue as u32 })
    }
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
        // Older logs say `emit` / `own` for `wave` / `door`.
        emit_ms: num(after(&t, "wave", 0).or_else(|_| after(&t, "emit", 0))?)?,
        emit_idle: num(after(&t, "(idle", 0)?)?,
        own_ms: num(after(&t, "door", 0).or_else(|_| after(&t, "own", 0))?)?,
        own_idle: after(&t, "(idle", 1).ok().map(num).transpose()?.unwrap_or(0),
        ckpt_ms: num(after(&t, "ckpt", 0)?)?,
        pos_ms: num(after(&t, "pos", 0)?)?,
        total_ms: num(after(&t, "total", 0)?)?,
        // `rss X GB` (older) or `rss start A wave B end C ...`: the end value.
        rss_gb: match after(&t, "rss", 0)? {
            "start" => num(after(&t, "end", 0)?)?,
            v => num(v)?,
        },
    })
}

/// A level with only what the log says.
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

/// The run log: `[fwd]` lines split into levels by the `[search] level i
/// (SPEC): forward to fH ...; first win W` line closing each (none in a
/// `rewrite forward` log: one level), the horizon, and the wall time and
/// optimum of the `OPTIMAL win frame: F (S s)` line.
pub fn parse_log(text: &str) -> Result<(Vec<LevelRun>, u32, Option<f64>, Option<u32>)> {
    let mut levels: Vec<LevelRun> = Vec::new();
    let mut fwd: Vec<FwdLine> = Vec::new();
    let mut first_win: Option<u32> = None;
    let (mut horizon, mut wall, mut optimal) = (None, None, None);
    for (ln, line) in text.lines().enumerate() {
        let ctx = || format!("log line {}: {line}", ln + 1);
        if line.starts_with("[fwd] first win at f") {
            first_win = Some(num(line.rsplit(' ').next().unwrap_or("")).with_context(ctx)?);
        } else if line.starts_with("[fwd] f") {
            fwd.push(parse_fwd(line).with_context(ctx)?);
        } else if let Some(rest) = line.strip_prefix("[search] level ") {
            let t: Vec<&str> = rest.split_whitespace().collect();
            let level: usize = num(t[0]).with_context(ctx)?;
            anyhow::ensure!(level == levels.len(), "{}: level {level} after {} levels", ctx(), levels.len());
            let spec = t[1].trim_start_matches('(').trim_end_matches("):").to_string();
            horizon = Some(num(after(&t, "to", 0)?).with_context(ctx)?);
            let win = line.rsplit("first win ").next().unwrap_or("");
            let win = win.strip_prefix("Some(").map(|w| num(w)).transpose().with_context(ctx)?;
            levels.push(level_run(level, spec, std::mem::take(&mut fwd), win));
            first_win = None;
        } else if let Some(rest) = line.strip_prefix("OPTIMAL win frame: ") {
            let t: Vec<&str> = rest.split_whitespace().collect();
            optimal = Some(num(t[0]).with_context(ctx)?);
            wall = t.get(1).map(|w| num(w)).transpose().with_context(ctx)?;
        }
    }
    if levels.is_empty() {
        anyhow::ensure!(!fwd.is_empty(), "the log has neither `[search] level` nor `[fwd]` lines");
        horizon = fwd.last().map(|l| l.f);
        levels.push(level_run(0, "forward".to_string(), std::mem::take(&mut fwd), first_win));
    }
    anyhow::ensure!(fwd.is_empty(), "the log ends with {} `[fwd]` lines after its last level", fwd.len());
    Ok((levels, horizon.expect("set with every level"), wall, optimal))
}

/// Sparse per-cell counts of one frame: `(cell, count)` ascending by cell.
type Sparse = Vec<(u32, u32)>;

struct LevelData {
    /// Per frame: the states per cell, the wins per cell.
    frames: Vec<(Sparse, Sparse)>,
}

/// `f(i)` for every `i < n` in parallel, results by index; the first error
/// stops the pulling.
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
        anyhow::ensure!(n.checked_mul(32).and_then(|b| b.checked_add(4)) == Some(body), "{}: {n} rows do not fit {body} payload bytes", path.display());
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
    for (_, file) in files {
        for (cell, n) in file.cell_counts() {
            *out.cells.entry(cell).or_default() += n;
        }
        for &(_, cell) in file.win_rows() {
            *out.wins.entry(cell).or_default() += 1;
        }
        let Some(t) = marks else { continue };
        let shape = file.shape_hash();
        // A cell's runs are adjacent: look the cell up once, and skip an
        // unmarked cell's runs without reading their keys.
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

/// Read one level directory: its frames from the headers and, in the same
/// pass, the sets of `marks` split by layer (`[k]`: per frame, set `k`'s
/// marked states per cell). Frames in parallel.
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
    let mut counts = par_map(n, |i| frame_counts(dir, (n - 1 - i) as u32, marks, nsets))?;
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

/// `key value` lines (`arc.txt`).
fn read_keyed(path: &Path) -> Result<FxHashMap<String, String>> {
    let text = std::fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
    Ok(text.lines().filter_map(|l| l.split_once(' ')).map(|(k, v)| (k.to_string(), v.trim().to_string())).collect())
}

/// `witness.txt` (`search --save-marks`): `inputs a,b,..`, then
/// `f x y` per frame from 0 (`f - -` without a player). A file for
/// `--witness` / `--reference` (`tools/align_tas.py`) may also carry, before
/// the frames and in any order, `label TEXT`, `dashes F:DIR ...`, and for the
/// tasdatabase file (`TasFile`) all three of `db NAME CATEGORY` (`db 2900m
/// nodiag`), `prologue N` (the spawn frames to strip) and `seeds [a,b,..]`
/// (`seeds []` without); the frames may end early (at the exit).
fn read_witness(path: &Path) -> Result<Witness> {
    let text = std::fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
    let name = path.display();
    let mut lines = text.lines().peekable();
    let mut head: FxHashMap<&str, &str> = FxHashMap::default();
    while let Some(l) = lines.next_if(|l| !l.starts_with(|c: char| c.is_ascii_digit())) {
        let (k, v) = l.split_once(' ').unwrap_or((l, ""));
        anyhow::ensure!(["label", "inputs", "dashes", "db", "prologue", "seeds"].contains(&k), "{name}: unknown line {l:?}");
        anyhow::ensure!(head.insert(k, v.trim()).is_none(), "{name}: two `{k}` lines");
    }
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
            Ok((num(f)?, dir.to_string()))
        })
        .collect::<Result<_>>()?;
    let tas = match (head.get("db"), head.get("prologue"), head.get("seeds")) {
        (None, None, None) => None,
        (Some(db), Some(prologue), Some(seeds)) => {
            let (db_name, _category) = db.split_once(' ').with_context(|| format!("{name}: `db {db}` is not `db NAME CATEGORY`"))?;
            let inner = seeds.strip_prefix('[').and_then(|s| s.strip_suffix(']')).with_context(|| format!("{name}: `seeds {seeds}` is not `seeds [a,b,..]`"))?;
            let seeds: Vec<u32> = inner
                .split(',')
                .filter(|s| !s.trim().is_empty())
                .map(|s| s.trim().parse::<u32>().with_context(|| format!("{name}: a seed")))
                .collect::<Result<_>>()?;
            let prologue: usize = prologue.parse().with_context(|| format!("{name}: `prologue {prologue}`"))?;
            Some(TasFile::new(db_name, prologue, &seeds, &inputs).with_context(|| format!("{name}: the tasdatabase file"))?)
        }
        _ => bail!("{name}: `db`, `prologue` and `seeds` go together"),
    };
    let mut path_xy = Vec::new();
    for (i, l) in lines.enumerate() {
        let t: Vec<&str> = l.split_whitespace().collect();
        anyhow::ensure!(t.len() == 3 && t[0].parse::<usize>().ok() == Some(i), "{name}: line {:?} is not frame {i}", l);
        path_xy.push(if t[1] == "-" { None } else { Some((num(t[1])?, num(t[2])?)) });
    }
    anyhow::ensure!((1..=inputs.len() + 1).contains(&path_xy.len()), "{name}: {} inputs, {} positions", inputs.len(), path_xy.len());
    let label = head.get("label").map_or_else(|| format!("concrete witness inside the winning sets, a win at f{}", inputs.len()), |l| l.to_string());
    Ok(Witness { label, inputs, path: path_xy, dashes, tas })
}

/// `--arc DIR` (`search --save-marks DIR`): the last forward level gets the
/// remainder-free marks, and a level after it the ARC backward over the same
/// tree; the optimum is the arc search's. Returns the witness if present.
fn attach_arc(hr: &mut HorizonRun, optimal: &mut Option<u32>, dir: &Path) -> Result<Option<Witness>> {
    let kv = read_keyed(&dir.join("arc.txt"))?;
    let get = |k: &str| kv.get(k).with_context(|| format!("arc.txt: no `{k}`"));
    let frame = |k: &str| -> Result<Option<u32>> {
        let v = get(k)?;
        if v == "none" {
            Ok(None)
        } else {
            Ok(Some(num(v)?))
        }
    };
    let h: u32 = num(get("horizon")?)?;
    anyhow::ensure!(hr.h == h, "--arc: the arc search ran to h{h}, the log's forward to f{}", hr.h);
    let rows = |name: &str| -> Result<u64> { Ok(MarksMap::open(&dir.join(name))?.rows as u64) };
    let found = frame("optimal")?;
    hr.partial = false;
    let last = hr.levels.len() - 1;
    let lr = &mut hr.levels[last];
    lr.precision = format!("{}, rem free", get("level")?);
    lr.first_win = frame("level0_first_win")?;
    lr.refuted = lr.first_win.is_none();
    lr.marked = Some(rows("level0.marks.bin")?);
    let mut arc = level_run(last + 1, format!("arc: {}, rem exact", get("level")?), Vec::new(), found);
    arc.marked = found.map(|_| rows("arc.marks.bin")).transpose()?;
    arc.refuted = found.is_none();
    arc.forward_of = Some(last);
    hr.levels.push(arc);
    hr.refuted_at = hr.levels.iter().position(|l| l.refuted);
    *optimal = if hr.refuted_at.is_none() { found } else { None };
    let w = dir.join("witness.txt");
    w.exists().then(|| read_witness(&w)).transpose()
}

/// The concrete runs drawn over the room: `witness` (replacing the arc
/// directory's) and `reference`, files in `read_witness`'s format.
pub struct Paths<'a> {
    pub witness: Option<&'a Path>,
    pub reference: Option<&'a Path>,
}

/// `export-ui --paths-only`: replace the concrete paths (`witness`,
/// `reference`) in an existing export's `run.json` and nothing else - for
/// path files that changed after the run's trees were deleted.
pub fn update_paths(out: &Path, paths: Paths) -> Result<()> {
    anyhow::ensure!(paths.witness.is_some() || paths.reference.is_some(), "--paths-only needs --witness or --reference");
    let p = out.join("run.json");
    let text = std::fs::read_to_string(&p).with_context(|| format!("reading {}", p.display()))?;
    let mut run: serde_json::Value = serde_json::from_str(&text).with_context(|| format!("parsing {}", p.display()))?;
    let obj = run.as_object_mut().with_context(|| format!("{}: not an object", p.display()))?;
    for (key, file) in [("witness", paths.witness), ("reference", paths.reference)] {
        if let Some(file) = file {
            obj.insert(key.to_string(), serde_json::to_value(read_witness(file)?)?);
            eprintln!("[export-ui] {}: `{key}` from {}", p.display(), file.display());
        }
    }
    std::fs::write(&p, serde_json::to_string(&run)?).with_context(|| format!("writing {}", p.display()))
}

/// The export. `checkpoint_dir` is a search's (`levelNN/` under it) or one
/// forward's tree; `room` the start room it ran from; `arc` its `search
/// --save-marks` directory.
pub fn export(checkpoint_dir: &Path, log_path: &Path, out: &Path, room: (i16, i16), arc: Option<&Path>, paths: Paths) -> Result<()> {
    let text = std::fs::read_to_string(log_path).with_context(|| format!("reading {}", log_path.display()))?;
    let (levels, h, wall_s, mut optimal) = parse_log(&text)?;
    let search = checkpoint_dir.join("level00").exists();
    anyhow::ensure!(search || levels.len() == 1, "{}: one forward's tree, but the log has {} levels", checkpoint_dir.display(), levels.len());
    let tree_dir = |level: usize| if search { checkpoint_dir.join(format!("level{level:02}")) } else { checkpoint_dir.to_path_buf() };
    let nforward = levels.len();
    let mut hr = HorizonRun { h, levels, refuted_at: None, partial: true };
    let witness = match arc {
        Some(dir) => attach_arc(&mut hr, &mut optimal, dir)?,
        None => None,
    };
    let witness = match paths.witness {
        Some(p) => Some(read_witness(p)?),
        None => witness,
    };
    let reference = paths.reference.map(read_witness).transpose()?;
    eprintln!("[export-ui] log: {} levels to f{h}, optimal {optimal:?}", hr.levels.len());
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
        tiles: room_tiles(room)?,
        wall_s,
        prebuild_s: None,
        optimal,
        horizons: vec![hr],
        witness,
        reference,
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
            let mut v = crate::frame::Visited::new();
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
        // The tasdatabase file: the prologue stripped, the seeds in front, no newline.
        let t = read("label ours\ninputs 0,0,16,0,2\ndb 2900m nodiag\nprologue 2\nseeds [0,0,0]\n0 - -\n").unwrap().tas.unwrap();
        assert_eq!(t, TasFile { file: "TAS29.tas".into(), text: "[0,0,0,]16,0,2,".into(), frames: 2, prologue: 2 });
        let t = read("inputs 0,17,0\nprologue 1\nseeds []\ndb 600m nodiag\n0 - -\n").unwrap().tas.unwrap();
        assert_eq!((t.text.as_str(), t.frames), ("[]17,0,", 1));
        assert!(read("inputs 0,17,0\nprologue 2\nseeds []\ndb 600m nodiag\n0 - -\n").is_err(), "a prologue that presses a button");
        assert!(read("inputs 0,17,0\nprologue 1\n0 - -\n").is_err(), "db, prologue and seeds go together");
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn the_search_log_splits_into_its_levels() {
        let fwd = |f: u32| format!("[fwd] f{f:03} in 1/1 raw 1 kept 1 out 1/1 visited 4 | wave 7 (idle 94%) door 3 ckpt 0 pos 0 total 11 ms | flushes 1 (1 rows avg) | in 0.00 queues 0.00 door 0.00 GB rss start 1.26 wave 1.27 end 1.28 peak 8.25 GB");
        let log = [
            "[search] room 7,0, levels r0sxhn,r0sxh, horizon 2".to_string(),
            fwd(1),
            fwd(2),
            "[fwd] first win at f2".to_string(),
            "[search] level 0 (r0sxhn): forward to f2 in 2.7 s; first win Some(2)".to_string(),
            fwd(1),
            "[search] level 1 (r0sxh): forward to f2 in 0.1 s; first win None".to_string(),
            "OPTIMAL win frame: 2 (3.5 s); witness /x/witness_frame_2.txt".to_string(),
        ]
        .join("\n");
        let (levels, h, wall, optimal) = parse_log(&log).unwrap();
        assert_eq!((levels.len(), h, wall, optimal), (2, 2, Some(3.5), Some(2)));
        assert_eq!((levels[0].precision.as_str(), levels[0].fwd.len(), levels[0].first_win), ("r0sxhn", 2, Some(2)));
        assert_eq!((levels[1].precision.as_str(), levels[1].fwd.len(), levels[1].first_win), ("r0sxh", 1, None));
        assert_eq!(levels[0].fwd[1].rss_gb, 1.28);
        // A `rewrite forward` log: one level to its last frame.
        let (levels, h, _, _) = parse_log(&[fwd(1), fwd(2), fwd(3)].join("\n")).unwrap();
        assert_eq!((levels.len(), h, levels[0].fwd.len()), (1, 3, 3));
    }
}
