//! Measurement for the time-expanded sweep: how much does PLAYER POSITION
//! narrow the candidate set?
//!
//! The edge-graph sweep (`sweep.rs`) records every `(src, dst)` row edge -
//! 10.1e9 of them for room (1,0) at horizon 100, and enough on room (0,0) to
//! OOM. The proposed replacement works in time-expanded space: nodes are
//! `(row, frame)`, so every edge goes `i -> i+1` and nothing has to be
//! stored. Writing `R(i)` for the rows with earliest arrival `<= i` (the row
//! table's watermark prefix) and
//!
//!   B(i) = { s in R(i) : g(s) <= H - i }        (B(H) = the win rows)
//!
//! the recurrence is `B(i) = { s in R(i) : some successor of s is in
//! B(i+1) }`, and B is monotone in the sense `B(i) ⊇ B(i+1) ∩ R(i)`, so at
//! frame `i` only `R(i) \ B(i+1)` has to be tested at all.
//!
//! That is still 8.2e9 row-expansions against the edge sweep's 2.1e8 (each
//! row expanded once), because a row is re-tested every frame until it
//! qualifies. The proposal is to test only rows whose player position is
//! within some radius of a position occupied by `B(i+1)` - an APPROXIMATE
//! predecessor relation, made sound by the fact that the candidates are then
//! actually expanded, so only cost depends on its tightness.
//!
//! Positions are measured relative to the START room, not room-local:
//! object coordinates reset at a room transition, so the one frame that
//! crosses into the next room looks like a 128-pixel jump and poisons every
//! learned radius that touches the exit.
//!
//! This module measures whether that filter has any power at all, from a
//! finished forward pass plus its certified `g`: per frame it reports
//! `|R(i)|`, `|B(i)|`, the number of distinct position cells `B(i)` occupies,
//! and the number of rows of `R(i) \ B(i+1)` that lie within radius `r` of a
//! `B(i+1)` cell. It computes nothing the sweep needs; it only says whether
//! the sweep is worth rewriting.

use anyhow::{anyhow, Context, Result};
use std::path::Path;

use crate::interpreter::abstraction::{player_xy_per_lane, room_xy_per_lane};
use crate::interpreter::row_table::RowTable;

use super::checkpoint;
use super::sweep::{row_keys, G_UNREACHABLE};

/// Side of the position grid, in pixels. A room is 128x128; the grid is
/// bigger because positions are measured relative to the START room, so a
/// row that has already crossed into the next room sits past 128.
pub const GRID: i32 = 256;
/// Pixel coordinate mapped to grid index 0.
const ORIGIN: i32 = -64;
/// Side of a room in pixels - the stride between rooms in the grid.
const ROOM_PX: i32 = 128;
/// `positions[id]` for a row with no player object - the countdown states
/// after a death, and any state whose objects array has neither `player`
/// nor `player_spawn`. Such rows are never candidates under a positional
/// filter, which is exactly what we want to measure.
pub const NO_CELL: u16 = u16::MAX;

/// Grid cell of a whole-pixel position, given in START-ROOM-relative
/// coordinates. Loud rather than clamping: a position outside the grid
/// would silently distort every distance.
fn cell_of(x: i32, y: i32) -> Result<u16> {
    let gx = x - ORIGIN;
    let gy = y - ORIGIN;
    if !(0..GRID).contains(&gx) || !(0..GRID).contains(&gy) {
        return Err(anyhow!("player position ({}, {}) is outside the census grid", x, y));
    }
    let cell = (gy * GRID + gx) as u16;
    if cell == NO_CELL {
        return Err(anyhow!("player position ({}, {}) collides with NO_CELL", x, y));
    }
    Ok(cell)
}

/// A census cell in `pos_graph`'s numbering.
///
/// The census keeps its own 256px cell space - it only ever runs on room
/// (1,0), whose positions all fit - while `pos_graph` numbers a 512px one,
/// because room (0,0) exits upward and its crossing lanes land 238px from
/// its origin. The two coincided once and must now be crossed explicitly.
pub fn to_pos_graph_cell(cell: u16) -> u32 {
    if cell == NO_CELL {
        return crate::rewrite::pos_graph::NO_CELL;
    }
    crate::rewrite::pos_graph::cell_of(
        (cell as i32 % GRID) + ORIGIN,
        (cell as i32 / GRID) + ORIGIN,
    )
    .expect("the census grid must fit inside the position graph's")
}

/// Player position cell per row id, from the saved frontier batches.
///
/// The forward pass is frontier-only, so every row appears in exactly one
/// batch - the one for its earliest-arrival frame - and that is checked
/// here per row: a row that arrives in the wrong batch, or twice, means the
/// batches and the row table disagree and every number below would be
/// meaningless.
pub fn build_positions(dir: &Path, frames: u32, table: &RowTable) -> Result<Vec<u16>> {
    let start = crate::game_runner::start_room();
    let n_rows = table.len();
    let mut positions = vec![NO_CELL; n_rows];
    let mut seen = vec![0u64; n_rows.div_ceil(64)];
    let mut placed = 0usize;
    let mut without_player = 0usize;
    for f in 1..=frames {
        let states = checkpoint::load_frame_states(dir, f)
            .with_context(|| format!("loading frame batch f{:03}", f))?;
        for state in &states {
            let keys = row_keys(state)?;
            let rooms = room_xy_per_lane(state)
                .ok_or_else(|| anyhow!("f{:03}: a saved state has no readable `room`", f))?;
            let xy = player_xy_per_lane(state);
            if rooms.len() != keys.len() {
                return Err(anyhow!("f{:03}: {} rooms for {} lanes", f, rooms.len(), keys.len()));
            }
            if let Some(xy) = &xy {
                if xy.len() != keys.len() {
                    return Err(anyhow!(
                        "f{:03}: {} positions for {} lanes",
                        f,
                        xy.len(),
                        keys.len()
                    ));
                }
            }
            for (lane, key) in keys.iter().enumerate() {
                let id = table
                    .id_of(*key)
                    .ok_or_else(|| anyhow!("f{:03}: a saved lane's row is not in the table", f))?;
                if table.earliest_frame(id) != Some(f) {
                    return Err(anyhow!(
                        "f{:03}: row {} is stamped with frame {:?}, not this batch's",
                        f,
                        id,
                        table.earliest_frame(id)
                    ));
                }
                let (w, b) = (id as usize / 64, 1u64 << (id % 64));
                if seen[w] & b != 0 {
                    return Err(anyhow!("row {} appears in two frontier batches", id));
                }
                seen[w] |= b;
                match &xy {
                    Some(xy) => {
                        let (rx, ry) = rooms[lane];
                        positions[id as usize] = cell_of(
                            xy[lane].0 as i32 + (rx - start.0) as i32 * ROOM_PX,
                            xy[lane].1 as i32 + (ry - start.1) as i32 * ROOM_PX,
                        )?;
                        placed += 1;
                    }
                    None => without_player += 1,
                }
            }
        }
        if f % 20 == 0 || f == frames {
            println!("  census f{:03}: {} rows positioned", f, placed + without_player);
        }
    }
    let missing = n_rows - placed - without_player;
    println!(
        "census: {} rows positioned, {} without a player object, {} never saved",
        placed, without_player, missing
    );
    if missing != 0 {
        return Err(anyhow!(
            "{} of {} rows are in the row table but in no frontier batch",
            missing,
            n_rows
        ));
    }
    Ok(positions)
}

/// `<dir>/pos.bin`: one u16 cell per row id, same container as `save_g`.
pub fn save_positions(dir: &Path, positions: &[u16]) -> Result<()> {
    use std::io::Write;
    let tmp = dir.join("tmp-pos.bin");
    {
        let mut w = std::io::BufWriter::new(std::fs::File::create(&tmp)?);
        w.write_all(b"C8PG")?;
        w.write_all(&checkpoint::FORMAT_VERSION.to_le_bytes())?;
        w.write_all(&(positions.len() as u64).to_le_bytes())?;
        let mut zw = zstd::Encoder::new(&mut w, 1)?;
        zw.include_checksum(true)?;
        for v in positions {
            zw.write_all(&v.to_le_bytes())?;
        }
        zw.finish()?;
        w.flush()?;
    }
    std::fs::rename(&tmp, dir.join("pos.bin"))?;
    Ok(())
}

/// Load `save_positions`' output, or `None` when it has not been built.
pub fn load_positions(dir: &Path, n_rows: usize) -> Result<Option<Vec<u16>>> {
    use std::io::Read;
    let path = dir.join("pos.bin");
    if !path.exists() {
        return Ok(None);
    }
    let mut file = std::io::BufReader::new(std::fs::File::open(&path)?);
    let mut magic = [0u8; 4];
    file.read_exact(&mut magic)?;
    if &magic != b"C8PG" {
        return Err(anyhow!("{}: bad magic", path.display()));
    }
    let mut buf4 = [0u8; 4];
    file.read_exact(&mut buf4)?;
    if u32::from_le_bytes(buf4) != checkpoint::FORMAT_VERSION {
        return Err(anyhow!("{}: format version mismatch", path.display()));
    }
    let mut buf8 = [0u8; 8];
    file.read_exact(&mut buf8)?;
    let count = u64::from_le_bytes(buf8) as usize;
    if count != n_rows {
        return Err(anyhow!(
            "{}: {} rows, but the row table has {} - rebuild it",
            path.display(),
            count,
            n_rows
        ));
    }
    let mut zr = zstd::Decoder::new(file)?;
    let mut buf = vec![0u8; count * 2];
    zr.read_exact(&mut buf)?;
    let out = buf.chunks_exact(2).map(|c| u16::from_le_bytes(c.try_into().unwrap())).collect();
    Ok(Some(out))
}

/// Per DESTINATION cell, the largest squared distance any predecessor of a
/// row in that cell was at - the old solver's `DistanceTracker`, pooled over
/// every frame instead of kept per frame.
///
/// Learned here from the edge chunks the current sweep already wrote, which
/// is exactly the set of transitions the forward pass took (frontier-only,
/// so every row is expanded once). A real implementation would record this
/// during the forward pass and never build the edges at all; the table is
/// 256 KB either way.
pub fn learn_radii(dir: &Path, frames: u32, positions: &[u16]) -> Result<Vec<u32>> {
    let edge_dir = dir.join("sweep-edges");
    let mut radii2 = vec![u32::MAX; (GRID * GRID) as usize];
    let mut edges = 0u64;
    let mut no_cell = 0u64;
    // Edges by whole-pixel displacement. A flat radius is only sound if it
    // covers this whole histogram, so its tail is what decides whether one
    // exists at all.
    let mut hist = vec![0u64; 2 * GRID as usize];
    for f in 1..frames {
        let path = edge_dir.join(format!("f{:03}.bin", f));
        if !path.exists() {
            return Err(anyhow!("{} is missing - the edge chunks are incomplete", path.display()));
        }
        let t = std::time::Instant::now();
        let n = checkpoint::stream_u32_pairs(&path, |src, dst| {
            let (sc, dc) = (positions[src as usize], positions[dst as usize]);
            if sc == NO_CELL || dc == NO_CELL {
                no_cell += 1;
                return;
            }
            let (sx, sy) = ((sc as i32 % GRID), (sc as i32 / GRID));
            let (dx, dy) = ((dc as i32 % GRID), (dc as i32 / GRID));
            let d2 = ((sx - dx).pow(2) + (sy - dy).pow(2)) as u32;
            let slot = &mut radii2[dc as usize];
            if *slot == u32::MAX || d2 > *slot {
                *slot = d2;
            }
            hist[(d2 as f64).sqrt().ceil() as usize] += 1;
        })?;
        edges += n;
        if f % 10 == 0 || f + 1 == frames {
            println!(
                "  radii f{:03}: {} edges ({:.1}s, {} total)",
                f,
                n,
                t.elapsed().as_secs_f64(),
                edges
            );
        }
    }
    let live = radii2.iter().filter(|&&r| r != u32::MAX).count();
    let max = radii2.iter().filter(|&&r| r != u32::MAX).max().copied().unwrap_or(0);
    println!(
        "radii: {} edges, {} cells with a learned radius, largest {:.2}px, \
         {} edges touching a row with no player object",
        edges,
        live,
        (max as f64).sqrt(),
        no_cell
    );
    // The displacement tail: for each candidate flat radius, how many edges
    // it would MISS. A radius that misses any edge is unsound on its own.
    let total: u64 = hist.iter().sum();
    let mut over = total;
    println!("  displacement tail (edges a flat radius would miss):");
    for r in 0..hist.len() {
        over -= hist[r];
        if over == 0 {
            println!("    <= {:>3}px: none", r);
            break;
        }
        if r <= 16 || hist[r] > 0 {
            println!("    <= {:>3}px: {} edges beyond ({:.3e} of all)", r, over, over as f64 / total as f64);
        }
    }
    // The cells that force a big radius, so the cause is nameable rather
    // than a number: these are the ones that make the learned mask useless.
    let mut worst: Vec<(u32, usize)> = radii2
        .iter()
        .enumerate()
        .filter(|(_, &r)| r != u32::MAX)
        .map(|(c, &r)| (r, c))
        .collect();
    worst.sort_unstable_by(|a, b| b.0.cmp(&a.0));
    println!("  cells forcing the largest radii (start-room-relative px):");
    for (r2, c) in worst.iter().take(8) {
        println!(
            "    ({:>4}, {:>4}) needs {:.1}px",
            (c % GRID as usize) as i32 + ORIGIN,
            (c / GRID as usize) as i32 + ORIGIN,
            (*r2 as f64).sqrt()
        );
    }
    Ok(radii2)
}

/// Learn `SrcCells` from the sweep's edge chunks, alongside `learn_radii`'s
/// pass so the 58 GB is streamed once rather than twice.
pub fn learn_src_cells(dir: &Path, frames: u32, positions: &[u16]) -> Result<SrcCells> {
    let edge_dir = dir.join("sweep-edges");
    let mut out =
        SrcCells { index: vec![u32::MAX; (GRID * GRID) as usize], bits: Vec::new() };
    for f in 1..frames {
        let path = edge_dir.join(format!("f{:03}.bin", f));
        let t = std::time::Instant::now();
        let n = checkpoint::stream_u32_pairs(&path, |src, dst| {
            let (sc, dc) = (positions[src as usize], positions[dst as usize]);
            if sc == NO_CELL || dc == NO_CELL {
                return;
            }
            let base = out.slot(dc);
            out.bits[base + sc as usize / 64] |= 1 << (sc % 64);
        })?;
        if f % 20 == 0 || f + 1 == frames {
            println!(
                "  src-cells f{:03}: {} edges ({:.1}s, {} destination cells)",
                f,
                n,
                t.elapsed().as_secs_f64(),
                out.occupied_cells()
            );
        }
    }
    let set: usize = out.bits.iter().map(|w| w.count_ones() as usize).sum();
    println!(
        "src-cells: {} destination cells, {:.1} source cells each on average, {} MB",
        out.occupied_cells(),
        set as f64 / out.occupied_cells().max(1) as f64,
        out.bits.len() * 8 / 1_000_000
    );
    Ok(out)
}

/// `<dir>/radii.bin`: one u32 squared radius per grid cell.
pub fn save_radii(dir: &Path, radii2: &[u32]) -> Result<()> {
    use std::io::Write;
    let tmp = dir.join("tmp-radii.bin");
    {
        let mut w = std::io::BufWriter::new(std::fs::File::create(&tmp)?);
        w.write_all(b"C8RD")?;
        w.write_all(&(radii2.len() as u64).to_le_bytes())?;
        for v in radii2 {
            w.write_all(&v.to_le_bytes())?;
        }
        w.flush()?;
    }
    std::fs::rename(&tmp, dir.join("radii.bin"))?;
    Ok(())
}

/// Load `save_radii`' output, or `None` when it has not been built.
pub fn load_radii(dir: &Path) -> Result<Option<Vec<u32>>> {
    use std::io::Read;
    let path = dir.join("radii.bin");
    if !path.exists() {
        return Ok(None);
    }
    let mut file = std::io::BufReader::new(std::fs::File::open(&path)?);
    let mut magic = [0u8; 4];
    file.read_exact(&mut magic)?;
    if &magic != b"C8RD" {
        return Err(anyhow!("{}: bad magic", path.display()));
    }
    let mut buf8 = [0u8; 8];
    file.read_exact(&mut buf8)?;
    let count = u64::from_le_bytes(buf8) as usize;
    if count != (GRID * GRID) as usize {
        return Err(anyhow!("{}: {} cells, expected {}", path.display(), count, GRID * GRID));
    }
    let mut buf = vec![0u8; count * 4];
    file.read_exact(&mut buf)?;
    Ok(Some(buf.chunks_exact(4).map(|c| u32::from_le_bytes(c.try_into().unwrap())).collect()))
}

/// Number of `u64` words in a bitmap over the whole position grid.
const GRID_WORDS: usize = (GRID * GRID) as usize / 64;

/// For each destination cell, the exact SET of cells a predecessor of it was
/// ever in. This is the tightest a purely positional filter can be: it is
/// the true predecessor relation, projected onto position and nothing else.
///
/// A disc cannot express "the right-hand edge of the previous room", which
/// is what the predecessors of a room transition actually are - so the disc
/// has to use the distance to the far corner and paints the room. The set
/// costs 8 KB per occupied destination cell (66 MB on room (1,0)), which is
/// nothing next to the 58 GB of edges it replaces.
pub struct SrcCells {
    /// Destination cell -> dense index, `u32::MAX` when never a destination.
    index: Vec<u32>,
    /// Dense index -> bitmap over source cells, `GRID_WORDS` words each.
    bits: Vec<u64>,
}

impl SrcCells {
    fn slot(&mut self, dst: u16) -> usize {
        let at = &mut self.index[dst as usize];
        if *at == u32::MAX {
            *at = (self.bits.len() / GRID_WORDS) as u32;
            self.bits.resize(self.bits.len() + GRID_WORDS, 0);
        }
        *at as usize * GRID_WORDS
    }

    /// Union of the source sets of `cells`, as a grid bitmap.
    pub fn union_into(&self, cells: &[u32], out: &mut [u64]) {
        out.fill(0);
        for &c in cells {
            let at = self.index[c as usize];
            if at == u32::MAX {
                // Never a destination, so nothing is known to reach it. It
                // is occupied by a viable row all the same, so keep the row.
                out[c as usize / 64] |= 1 << (c % 64);
                continue;
            }
            let base = at as usize * GRID_WORDS;
            for (w, o) in self.bits[base..base + GRID_WORDS].iter().zip(out.iter_mut()) {
                *o |= *w;
            }
        }
    }

    pub fn occupied_cells(&self) -> usize {
        self.bits.len() / GRID_WORDS
    }

    /// Every `(dst cell, src cell)` the edges actually contain. The
    /// conservativeness gate for a table built any other way: whatever
    /// claims to over-approximate the predecessor relation has to contain
    /// all of these.
    pub fn pairs(&self) -> impl Iterator<Item = (u16, u16)> + '_ {
        self.index.iter().enumerate().filter(|(_, &at)| at != u32::MAX).flat_map(
            move |(dst, &at)| {
                let base = at as usize * GRID_WORDS;
                self.bits[base..base + GRID_WORDS]
                    .iter()
                    .enumerate()
                    .filter(|(_, &w)| w != 0)
                    .flat_map(move |(w, &word)| {
                        (0..64).filter(move |b| word & (1 << b) != 0).map(move |b| (w * 64 + b) as u16)
                    })
                    .map(move |src| (dst as u16, src))
            },
        )
    }
}

/// The old design's candidate mask: every cell within cell `c`'s OWN
/// learned radius of `c`, for each occupied `c`.
fn learned_mask(cells: &[u32], radii2: &[u32], out: &mut [bool]) {
    out.fill(false);
    for &c in cells {
        let r2 = radii2[c as usize];
        if r2 == u32::MAX {
            // No edge into this cell was ever recorded, so nothing is known
            // to reach it. It is still occupied by a viable row, so the row
            // itself must stay a candidate.
            out[c as usize] = true;
            continue;
        }
        let r = (r2 as f64).sqrt().ceil() as i32;
        let (cx, cy) = ((c % GRID as u32) as i32, (c / GRID as u32) as i32);
        for y in (cy - r).max(0)..=(cy + r).min(GRID - 1) {
            for x in (cx - r).max(0)..=(cx + r).min(GRID - 1) {
                if ((x - cx).pow(2) + (y - cy).pow(2)) as u32 <= r2 {
                    out[(y * GRID + x) as usize] = true;
                }
            }
        }
    }
}

/// One frame's row of the census table.
pub struct FrameCensus {
    pub frame: u32,
    /// |R(i)| - rows with earliest arrival <= i.
    pub r_rows: u64,
    /// |B(i)| - of those, the ones that still reach the exit by the horizon.
    pub b_rows: u64,
    /// Distinct position cells occupied by B(i), and by R(i).
    pub b_cells: u32,
    pub r_cells: u32,
    /// |B(i) \ B(i+1)| - rows that FIRST qualify at i. A perfect predecessor
    /// oracle would still have to expand at least these.
    pub new_rows: u64,
    /// Per radius: rows of `R(i) \ B(i+1)` within that radius of a B(i+1)
    /// cell. This is what the time-expanded sweep would expand at frame i.
    pub candidates: Vec<u64>,
    /// Same, under the per-cell learned radii; `None` when not supplied.
    pub learned: Option<u64>,
    /// The largest learned radius (px) any B(i+1) cell carries - one cell
    /// with a teleport-sized radius is enough to make the mask the room.
    pub learned_max_px: Option<u32>,
    /// Same as `candidates`, under the exact per-cell source sets.
    pub exact: Option<u64>,
    /// Same, under the table the forward-pass probe actually RECORDS
    /// (`pos_graph`) - the only one an implementation can have.
    pub recorded: Option<u64>,
}

/// Squared-distance field from the set cells, capped at `rmax`. Only the
/// occupied cells are painted, so the cost is `|cells| * pi * rmax^2`.
fn distance_field(cells: &[u32], rmax: i32, out: &mut [u32]) {
    out.fill(u32::MAX);
    let disc: Vec<(i32, i32, u32)> = (-rmax..=rmax)
        .flat_map(|dy| (-rmax..=rmax).map(move |dx| (dx, dy)))
        .filter(|(dx, dy)| dx * dx + dy * dy <= rmax * rmax)
        .map(|(dx, dy)| (dx, dy, (dx * dx + dy * dy) as u32))
        .collect();
    for &c in cells {
        let (cx, cy) = ((c % GRID as u32) as i32, (c / GRID as u32) as i32);
        for &(dx, dy, d2) in &disc {
            let (x, y) = (cx + dx, cy + dy);
            if (0..GRID).contains(&x) && (0..GRID).contains(&y) {
                let at = (y * GRID + x) as usize;
                if d2 < out[at] {
                    out[at] = d2;
                }
            }
        }
    }
}

/// The whole table. `radii` are in pixels; `horizon` is the band's H.
pub fn census(
    table: &RowTable,
    g: &[u16],
    positions: &[u16],
    horizon: u32,
    radii: &[i32],
    learned_radii: Option<&[u32]>,
    src_cells: Option<&SrcCells>,
    recorded: Option<&crate::rewrite::pos_graph::PosGraph>,
) -> Result<Vec<FrameCensus>> {
    let n_rows = g.len();
    if positions.len() != n_rows {
        return Err(anyhow!("positions/g length mismatch"));
    }
    let watermarks = table.watermarks();
    let frames = watermarks.len() as u32;
    let rmax = *radii.iter().max().ok_or_else(|| anyhow!("no radii given"))?;
    let cells = (GRID * GRID) as usize;
    let mut field = vec![u32::MAX; cells];
    let mut learned_in = vec![false; cells];
    let mut exact_in = vec![0u64; GRID_WORDS];
    let mut recorded_in = vec![0u64; crate::rewrite::pos_graph::CELL_WORDS];
    let mut b_next_pg: Vec<u32> = Vec::new();
    let pg_cell: Vec<u32> = (0..cells).map(|c| to_pos_graph_cell(c as u16)).collect();
    let mut occupied = vec![false; cells];
    let mut out = Vec::new();
    // Frame `horizon` needs no expansion at all: B(horizon) is the win
    // seeds. Every earlier frame is derived from its successor.
    for i in 1..horizon.min(frames) {
        let r_end = watermarks[i as usize - 1] as usize;
        let next_end = watermarks[(i as usize).min(frames as usize - 1)] as usize;
        let g_here = (horizon - i) as u16;
        let g_next = (horizon - i - 1) as u16;

        // B(i+1)'s occupied cells.
        occupied.fill(false);
        let mut b_next_cells: Vec<u32> = Vec::new();
        for id in 0..next_end {
            if g[id] <= g_next {
                let c = positions[id];
                if c != NO_CELL && !occupied[c as usize] {
                    occupied[c as usize] = true;
                    b_next_cells.push(c as u32);
                }
            }
        }
        distance_field(&b_next_cells, rmax, &mut field);
        let learned_max_px = learned_radii.map(|r2| {
            learned_mask(&b_next_cells, r2, &mut learned_in);
            b_next_cells
                .iter()
                .map(|&c| r2[c as usize])
                .filter(|&r| r != u32::MAX)
                .max()
                .map_or(0, |r| (r as f64).sqrt().ceil() as u32)
        });
        if let Some(sc) = src_cells {
            sc.union_into(&b_next_cells, &mut exact_in);
        }
        if let Some(pg) = recorded {
            // The recorded table has a node for "no player object", which
            // the census's cell space represents as NO_CELL; B(i+1) rows
            // without a position are part of the query.
            b_next_pg.clear();
            b_next_pg.extend(b_next_cells.iter().map(|&c| pg_cell[c as usize]));
            if (0..next_end).any(|id| positions[id] == NO_CELL && g[id] <= g_next) {
                b_next_pg.push(crate::rewrite::pos_graph::NO_CELL);
            }
            pg.union_into(&b_next_pg, &mut recorded_in);
        }

        // R(i) and B(i), and the candidates.
        occupied.fill(false);
        let mut r_cells = 0u32;
        let mut b_cells_seen = vec![false; cells];
        let mut b_cells = 0u32;
        let mut b_rows = 0u64;
        let mut new_rows = 0u64;
        let mut candidates = vec![0u64; radii.len()];
        let mut learned = 0u64;
        let mut exact = 0u64;
        let mut recorded_candidates = 0u64;
        for id in 0..r_end {
            let c = positions[id];
            if c != NO_CELL && !occupied[c as usize] {
                occupied[c as usize] = true;
                r_cells += 1;
            }
            let gv = g[id];
            let viable = gv != G_UNREACHABLE && gv <= g_here;
            if viable {
                b_rows += 1;
                if c != NO_CELL && !b_cells_seen[c as usize] {
                    b_cells_seen[c as usize] = true;
                    b_cells += 1;
                }
                if gv > g_next {
                    // In B(i) but not B(i+1): first qualifies here, so the
                    // sweep does have to expand it.
                    new_rows += 1;
                }
            }
            // Already in B(i+1) - monotone, so no test is needed at i.
            if viable && gv <= g_next {
                continue;
            }
            // The recorded table has a node for a row with no player
            // object, so those rows are candidates like any other; the
            // disc-based columns cannot place them at all.
            let pc = pg_cell[c as usize] as usize;
            if recorded.is_some() && recorded_in[pc / 64] & (1 << (pc % 64)) != 0 {
                recorded_candidates += 1;
            }
            if c == NO_CELL {
                continue;
            }
            let d2 = field[c as usize];
            for (k, r) in radii.iter().enumerate() {
                if d2 <= (r * r) as u32 {
                    candidates[k] += 1;
                }
            }
            if learned_radii.is_some() && learned_in[c as usize] {
                learned += 1;
            }
            if src_cells.is_some() && exact_in[c as usize / 64] & (1 << (c % 64)) != 0 {
                exact += 1;
            }
        }
        out.push(FrameCensus {
            frame: i,
            r_rows: r_end as u64,
            b_rows,
            b_cells,
            r_cells,
            new_rows,
            candidates,
            learned: learned_radii.map(|_| learned),
            learned_max_px,
            exact: src_cells.map(|_| exact),
            recorded: recorded.map(|_| recorded_candidates),
        });
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn cells_round_trip_and_reject_out_of_grid() {
        assert_eq!(cell_of(0, 0).unwrap(), (64 * GRID + 64) as u16);
        assert!(cell_of(-64, 0).is_ok());
        assert!(cell_of(-65, 0).is_err());
        assert!(cell_of(0, 192).is_err());
    }

    #[test]
    fn distance_field_is_euclidean_and_capped() {
        let mut field = vec![0u32; (GRID * GRID) as usize];
        let centre = (100 * GRID + 100) as u32;
        distance_field(&[centre], 4, &mut field);
        assert_eq!(field[centre as usize], 0);
        assert_eq!(field[(100 * GRID + 103) as usize], 9);
        // (3, 3) is 18 away squared, past the radius-4 cap.
        assert_eq!(field[(103 * GRID + 103) as usize], u32::MAX);
        assert_eq!(field[(100 * GRID + 105) as usize], u32::MAX);
    }

    /// The census must count exactly the rows the time-expanded sweep would
    /// expand: for a radius that covers the whole grid, that is
    /// `R(i) \ B(i+1)`, and `new_rows` must be the rows whose g is exactly
    /// `H - i`.
    #[test]
    fn census_counts_match_the_definitions() {
        // Three rows: ids 0,1 arrive at frame 1, id 2 at frame 2.
        let mut t = RowTable::default();
        t.insert_new((1, 1));
        t.insert_new((2, 2));
        t.end_frame();
        t.insert_new((3, 3));
        t.end_frame();
        t.end_frame();
        // g: row 0 reaches the exit in 2, row 1 in 1, row 2 never.
        let g = vec![2u16, 1, G_UNREACHABLE];
        let positions = vec![cell_of(0, 0).unwrap(); 3];
        let rows = census(&t, &g, &positions, 3, &[GRID], None, None, None).unwrap();
        // Horizon 3, so frames 1 and 2 are reported.
        assert_eq!(rows.len(), 2);
        // f1: R = {0,1}, B(1) = g <= 2 = {0,1}; B(2) = g <= 1 over R(2) =
        // {1}. So new_rows = {0}, candidates = R(1) \ B(2) = {0}.
        assert_eq!((rows[0].r_rows, rows[0].b_rows, rows[0].new_rows), (2, 2, 1));
        assert_eq!(rows[0].candidates, vec![1]);
        // f2: R = {0,1,2}, B(2) = g <= 1 = {1}; B(3) = g <= 0 = {}.
        assert_eq!((rows[1].r_rows, rows[1].b_rows, rows[1].new_rows), (3, 1, 1));
        // B(3) is empty, so nothing is within any radius of it.
        assert_eq!(rows[1].candidates, vec![0]);
        assert!(rows[0].learned.is_none());
    }

    /// The learned mask paints each occupied cell's OWN radius, not a
    /// global one - a cell nothing ever jumped into stays a point.
    #[test]
    fn learned_mask_is_per_cell() {
        let mut radii2 = vec![u32::MAX; (GRID * GRID) as usize];
        let near = (10 * GRID + 10) as u32;
        let far = (10 * GRID + 60) as u32;
        radii2[near as usize] = 4; // radius 2
        // `far` keeps u32::MAX: no edge ever landed there.
        let mut mask = vec![false; (GRID * GRID) as usize];
        learned_mask(&[near, far], &radii2, &mut mask);
        assert!(mask[(10 * GRID + 12) as usize], "within the learned radius");
        assert!(!mask[(10 * GRID + 13) as usize], "outside it");
        assert!(mask[far as usize], "an unlearned cell is still itself");
        assert!(!mask[(10 * GRID + 61) as usize], "but nothing around it");
    }

    /// The exact source sets are what a disc cannot express: a destination
    /// whose predecessors are a distant, tight cluster.
    #[test]
    fn src_cells_keep_distant_predecessors_tight() {
        let mut sc =
            SrcCells { index: vec![u32::MAX; (GRID * GRID) as usize], bits: Vec::new() };
        let dst = (200 * GRID + 200) as u16;
        let src = (10 * GRID + 10) as u16;
        let base = sc.slot(dst);
        sc.bits[base + src as usize / 64] |= 1 << (src % 64);
        assert_eq!(sc.occupied_cells(), 1);

        let mut out = vec![0u64; GRID_WORDS];
        fn is_set(out: &[u64], c: u16) -> bool {
            out[c as usize / 64] & (1 << (c % 64)) != 0
        }
        sc.union_into(&[dst as u32], &mut out);
        assert!(is_set(&out, src), "the one recorded predecessor");
        assert!(!is_set(&out, dst), "the destination itself was never a predecessor");
        assert!(!is_set(&out, (10 * GRID + 11) as u16), "nor its neighbour");
        // A disc covering `src` from `dst` would have radius ~269 and so
        // would cover the whole grid; the set covers one cell.
        assert_eq!(out.iter().map(|w| w.count_ones()).sum::<u32>(), 1);

        // A cell that was never a destination keeps only itself.
        let unseen = (5 * GRID + 5) as u32;
        sc.union_into(&[unseen], &mut out);
        assert_eq!(out.iter().map(|w| w.count_ones()).sum::<u32>(), 1);
        assert!(is_set(&out, unseen as u16));
    }
}
