//! The backward pass: min-frames-to-goal over the reachable row graph.
//!
//! Because a row is the COMPLETE canonical boundary state and the interpreter
//! is deterministic, the successor relation is a static graph over rows - it
//! does not depend on the frame at which a row occurs. So the backward pass
//! is not a per-frame simulation: it is one origin-tagged forward replay of
//! every saved frontier batch (each row is expanded exactly once, at its
//! discovery frame), which records the edges `src row -> dst row`, followed
//! by a reverse BFS assigning
//!
//!   g(row) = minimum frames from `row` to the room exit,
//!
//! seeded by the rows already in the next room (g = 0). Together with the
//! earliest-arrival frame e(row) (free from the row-table watermarks), the
//! per-frame backward-viability set for a horizon N is
//!
//!   B(f) = { row : g(row) <= N - f },     band(f) = { e <= f } ∩ B(f),
//!
//! materialized on demand - per-frame in meaning, two scalars per row in
//! storage. `min(e + g)` over all rows is the abstract optimal win frame and
//! must equal the forward pass's first-win frame.
//!
//! Soundness: everything here over-approximates concrete reachability the
//! same way the forward pass does (widened rem, certified quotients), so an
//! empty band proves impossibility at the horizon; achievability is only
//! ever claimed from a fully concrete witness replay.

use anyhow::{anyhow, Context, Result};
use std::path::Path;

use crate::interpreter::deopt_collect;
use crate::interpreter::inspect::room_x_lane_mask;
use crate::interpreter::row_table::{RowTable, ROW_HASH_SEED2};
use crate::interpreter::state::State;
use crate::interpreter::vectorize::shape_of_state;
use crate::interpreter::virtual_merge::{collect_columns_labeled, hash_rows, hash_rows_seeded, Column};

use super::checkpoint;
use super::program::Program;
use super::state_mapping::StateMapping;
use super::verify::AbstractRun;

/// The sweep's own origin column. Distinct from the deopt machinery's
/// `__lane_origin`, which comes and goes inside the same frame execution.
pub const SWEEP_ORIGIN: &str = "__sweep_origin";

/// `g` value meaning "cannot reach the goal (within the explored graph)".
pub const G_UNREACHABLE: u16 = u16::MAX;

/// Row keys of every lane of a canonical boundary state - the same
/// computation `subtract_visited` performs on the forward pass.
pub fn row_keys(state: &State) -> Result<Vec<(u64, u64)>> {
    let shape_hash = shape_of_state(state).cached_hash();
    let Some((columns, _)) = collect_columns_labeled(std::slice::from_ref(state)) else {
        return Err(anyhow!(
            "state cannot be canonicalized for row keys (collect_columns failed)"
        ));
    };
    let refs: Vec<&Column> = columns.iter().collect();
    let h1 = hash_rows(&refs, state.vector_size);
    let h2 = hash_rows_seeded(&refs, state.vector_size, ROW_HASH_SEED2);
    Ok(h1
        .iter()
        .zip(&h2)
        .map(|(a, b)| RowTable::key(shape_hash, *a, *b))
        .collect())
}

pub struct SweepResult {
    /// Min frames to the room exit per row id; `G_UNREACHABLE` if none.
    pub g: Vec<u16>,
    /// Distinct edges recorded (after in-batch dedup).
    pub edge_count: usize,
    /// `min(e + g)` over all rows - the abstract optimal win frame.
    pub optimal_frame: Option<u32>,
}

/// Run the backward sweep over the frame batches saved by
/// `bench --checkpoint-dir D --save-frames` up to `frames`, using the row
/// table from the checkpoint at `frames`.
pub fn backward_sweep(
    dir: &Path,
    frames: u32,
    fingerprint: &str,
    program: &Program,
    plain: &Program,
    mapping: StateMapping,
    banded: bool,
) -> Result<SweepResult> {
    let ck = checkpoint::load(dir, frames, fingerprint).context("loading final checkpoint")?;
    let table = ck.visited;
    let n_rows = table.len();
    println!("sweep: {} rows, replaying frames 1..{}", n_rows, frames);

    let mut engine = AbstractRun::start_with_deopt(program, plain, mapping, false)?;
    engine.disable_frontier();

    let mut g: Vec<u16> = vec![G_UNREACHABLE; n_rows];
    // Edge chunks spill to disk per frame: at full-room scale the edge set
    // is tens of GB, and a growing Vec's doubling reallocs would spike past
    // the memory cap. They are re-read once, into an exactly-sized Vec.
    // Chunks persist across sweeps: a frame's edges depend only on its saved
    // batch (deterministic replay), so extending the horizon later only
    // replays the NEW frames. Win seeds are persisted alongside for the same
    // reason.
    let edge_dir = dir.join("sweep-edges");
    std::fs::create_dir_all(&edge_dir)?;
    let win_x = crate::game_runner::win_room_x();
    let mut edge_total: u64 = 0;
    let mut edge_chunks: Vec<std::path::PathBuf> = Vec::new();

    for f in 1..=frames {
        let t = std::time::Instant::now();
        let chunk_path = edge_dir.join(format!("f{:03}.bin", f));
        let wins_path = edge_dir.join(format!("wins-f{:03}.bin", f));
        // Reuse a previous sweep's work for this frame if present. A chunk
        // without a wins file (older sweep) triggers a cheap batch rescan
        // for win seeds below, without re-expanding.
        if chunk_path.exists() && !wins_path.exists() && f < frames {
            let states = checkpoint::load_frame_states(dir, f)
                .with_context(|| format!("loading frame batch f{:03}", f))?;
            let mut frame_wins: Vec<(u32, u32)> = Vec::new();
            for state in &states {
                let keys = row_keys(state)?;
                for (key, win) in keys.iter().zip(room_x_lane_mask(state, win_x)) {
                    if win {
                        let id = table.id_of(*key).ok_or_else(|| {
                            anyhow!("frame f{:03}: saved lane's row missing from table", f)
                        })?;
                        g[id as usize] = 0;
                        frame_wins.push((id, 0));
                    }
                }
            }
            checkpoint::write_u32_pairs(&wins_path, &frame_wins)?;
        }
        if wins_path.exists() && (chunk_path.exists() || f == frames) {
            let mut wins: Vec<(u32, u32)> = Vec::new();
            checkpoint::read_u32_pairs_into(&wins_path, &mut wins)?;
            for (id, _) in wins {
                *g.get_mut(id as usize)
                    .ok_or_else(|| anyhow!("wins-f{:03}: id out of range", f))? = 0;
            }
            if f < frames {
                let mut pairs: Vec<(u32, u32)> = Vec::new();
                // Only the count is needed here; the pairs are re-read for
                // the BFS load below. Read the header count cheaply.
                checkpoint::read_u32_pairs_into(&chunk_path, &mut pairs)?;
                edge_total += pairs.len() as u64;
                edge_chunks.push(chunk_path);
            }
            continue;
        }
        let states = checkpoint::load_frame_states(dir, f)
            .with_context(|| format!("loading frame batch f{:03}", f))?;
        if states.is_empty() {
            checkpoint::write_u32_pairs(&wins_path, &[])?;
            if f < frames {
                checkpoint::write_u32_pairs(&chunk_path, &[])?;
                edge_chunks.push(chunk_path);
            }
            continue;
        }
        let mut frame_wins: Vec<(u32, u32)> = Vec::new();
        let mut batch = Vec::with_capacity(states.len());
        let mut batch_lanes = 0usize;
        for state in states {
            let keys = row_keys(&state)?;
            let ids: Vec<u32> = keys
                .iter()
                .map(|k| {
                    table.id_of(*k).ok_or_else(|| {
                        anyhow!("frame f{:03}: a saved lane's row is not in the row table", f)
                    })
                })
                .collect::<Result<_>>()?;
            // Rows already in the next room have won: g = 0 seeds.
            for (id, win) in ids.iter().zip(room_x_lane_mask(&state, win_x)) {
                if win {
                    g[*id as usize] = 0;
                    frame_wins.push((*id, 0));
                }
            }
            batch_lanes += state.vector_size;
            if f < frames {
                // Won lanes are absorbing (their g = 0 seed is recorded
                // above); expanding them would simulate the next room.
                let keep: Vec<bool> =
                    room_x_lane_mask(&state, win_x).into_iter().map(|w| !w).collect();
                let kept = keep.iter().filter(|b| **b).count();
                let (mut state, ids) = if kept == state.vector_size {
                    (state, ids)
                } else if kept > 0 {
                    let filtered_ids: Vec<u32> = ids
                        .iter()
                        .zip(&keep)
                        .filter(|(_, k)| **k)
                        .map(|(id, _)| *id)
                        .collect();
                    (
                        state.filter_by_mask_clone(
                            &keep,
                            crate::interpreter::state::FILTER_BAND,
                        ),
                        filtered_ids,
                    )
                } else {
                    continue;
                };
                deopt_collect::inject_named(&mut state, SWEEP_ORIGIN, &ids);
                batch.push(state);
            }
        }
        checkpoint::write_u32_pairs(&wins_path, &frame_wins)?;
        if f == frames {
            break;
        }

        // Expand in chunks of bounded input-lane count: the origin column
        // prevents boundary dedup, so the successor fan-out is the FULL
        // pre-dedup lane count (~30x the frontier at depth); a whole deep
        // batch at once would spike tens of GB of transient state.
        const CHUNK_LANES: usize = 1_000_000;
        let mut accounted = 0usize;
        let mut missing_successors = 0usize;
        let mut frame_edges: Vec<(u32, u32)> = Vec::new();
        let mut chunk: Vec<State> = Vec::new();
        let mut chunk_lanes = 0usize;
        let mut queue: std::collections::VecDeque<State> = batch.into();
        while let Some(state) = queue.pop_front() {
            chunk_lanes += state.vector_size;
            chunk.push(state);
            if chunk_lanes < CHUNK_LANES && !queue.is_empty() {
                continue;
            }
            // One frame forward; the origin column carries each lane's
            // source row id through every split, merge and deopt.
            let deopt_events = engine.deopt_events();
            engine.restore(std::mem::take(&mut chunk), None, deopt_events)?;
            chunk_lanes = 0;
            engine
                .step()
                .with_context(|| format!("expanding frame batch f{:03}", f))?;
            for out in engine.states() {
                let origins = deopt_collect::read_origins_named(out, SWEEP_ORIGIN);
                let mut stripped = out.clone();
                stripped.global_env.remove(SWEEP_ORIGIN);
                stripped.gc();
                let keys = row_keys(&stripped)?;
                if keys.len() != origins.len() {
                    return Err(anyhow!("sweep: origin/key length mismatch at f{:03}", f));
                }
                for (src, key) in origins.iter().zip(&keys) {
                    match table.id_of(*key) {
                        Some(dst) => frame_edges.push((*src, dst)),
                        // A banded forward pass pruned out-of-band successors
                        // from its table; dropping their edges is sound (any
                        // finer level's winning path coarsens to in-table
                        // edges), and the count keeps it visible. For an
                        // UNbanded level a missing successor means the replay
                        // diverged - loud.
                        None if banded => missing_successors += 1,
                        None => {
                            return Err(anyhow!(
                                "frame f{:03}: a successor row is not in the row \
                                 table - the replay diverged from the forward pass",
                                f
                            ))
                        }
                    }
                }
                accounted += origins.len();
            }
            // Drop the chunk's outputs before the next chunk runs.
            let deopt_events = engine.deopt_events();
            engine.restore(Vec::new(), None, deopt_events)?;
        }
        // Most redundancy is within a frame (the same (src, dst) via many
        // input combinations); dedup, then spill the chunk to disk.
        frame_edges.sort_unstable();
        frame_edges.dedup();
        checkpoint::write_u32_pairs(&chunk_path, &frame_edges)?;
        edge_total += frame_edges.len() as u64;
        edge_chunks.push(chunk_path);
        if f % 10 == 0 || f + 1 == frames || missing_successors > 0 {
            println!(
                "  sweep f{:03}: {} lanes in, {} successor lanes -> {} distinct edges{}, {:.1}s ({} edges total)",
                f,
                batch_lanes,
                accounted,
                frame_edges.len(),
                if missing_successors > 0 {
                    format!(" ({} out-of-band successors dropped)", missing_successors)
                } else {
                    String::new()
                },
                t.elapsed().as_secs_f64(),
                edge_total
            );
        }
    }
    drop(engine);

    // Build a CSR predecessor structure by streaming the chunks twice:
    // pass 1 counts per-dst degrees, pass 2 scatter-fills the srcs array.
    // No global sort or dedup is needed - each row is expanded exactly once
    // (in its discovery frame's chunk), so (src, dst) pairs are cross-chunk
    // unique by construction, and within-chunk dedup already happened at
    // write time. 4 bytes per edge instead of 8, no sort.
    let edge_bytes_gb = edge_total as f64 * 4.0 / 1e9;
    println!(
        "sweep: building CSR over {} edges ({:.1} GB) for BFS",
        edge_total, edge_bytes_gb
    );
    let t = std::time::Instant::now();
    let mut counts: Vec<u32> = vec![0; n_rows];
    for chunk in &edge_chunks {
        checkpoint::stream_u32_pairs(chunk, |_, dst| {
            counts[dst as usize] += 1;
        })?;
    }
    let mut offsets: Vec<u64> = Vec::with_capacity(n_rows + 1);
    let mut acc: u64 = 0;
    offsets.push(0);
    for &c in &counts {
        acc += c as u64;
        offsets.push(acc);
    }
    if acc != edge_total {
        return Err(anyhow!("CSR count mismatch: {} != {}", acc, edge_total));
    }
    let mut srcs: Vec<u32> = vec![0; edge_total as usize];
    let mut cursor: Vec<u64> = offsets[..n_rows].to_vec();
    for chunk in &edge_chunks {
        checkpoint::stream_u32_pairs(chunk, |src, dst| {
            let at = cursor[dst as usize];
            srcs[at as usize] = src;
            cursor[dst as usize] = at + 1;
        })?;
    }
    drop(counts);
    drop(cursor);
    let edge_count = edge_total as usize;
    println!("sweep: CSR built in {:.1}s", t.elapsed().as_secs_f64());
    // Chunks stay on disk for incremental horizon extension.

    let t = std::time::Instant::now();
    let mut queue: std::collections::VecDeque<u32> = (0..n_rows as u32)
        .filter(|&id| g[id as usize] == 0)
        .collect();
    let seeds = queue.len();
    while let Some(v) = queue.pop_front() {
        let next = g[v as usize]
            .checked_add(1)
            .ok_or_else(|| anyhow!("g overflow"))?;
        let range = offsets[v as usize] as usize..offsets[v as usize + 1] as usize;
        for &src in &srcs[range] {
            if g[src as usize] == G_UNREACHABLE {
                g[src as usize] = next;
                queue.push_back(src);
            }
        }
    }
    println!(
        "sweep: {} edges, {} win seeds, BFS in {:.1}s",
        edge_count,
        seeds,
        t.elapsed().as_secs_f64()
    );

    // min(e + g): the abstract optimal win frame.
    let mut optimal_frame: Option<u32> = None;
    for id in 0..n_rows as u32 {
        let gv = g[id as usize];
        if gv == G_UNREACHABLE {
            continue;
        }
        if let Some(e) = table.earliest_frame(id) {
            let win = e + gv as u32;
            optimal_frame = Some(optimal_frame.map_or(win, |b| b.min(win)));
        }
    }

    Ok(SweepResult { g, edge_count, optimal_frame })
}

/// Write the g array next to the checkpoints (`<dir>/g.bin`, u16-LE columnar).
pub fn save_g(dir: &Path, g: &[u16]) -> Result<()> {
    use std::io::Write;
    let tmp = dir.join("tmp-g.bin");
    {
        let mut w = std::io::BufWriter::new(std::fs::File::create(&tmp)?);
        w.write_all(b"C8TB")?;
        w.write_all(&checkpoint::FORMAT_VERSION.to_le_bytes())?;
        w.write_all(&(g.len() as u64).to_le_bytes())?;
        let mut zw = zstd::Encoder::new(&mut w, 1)?;
        zw.include_checksum(true)?;
        for v in g {
            zw.write_all(&v.to_le_bytes())?;
        }
        zw.finish()?;
        w.flush()?;
    }
    std::fs::rename(&tmp, dir.join("g.bin"))?;
    Ok(())
}

/// Load the g array written by `save_g`.
pub fn load_g(dir: &Path) -> Result<Vec<u16>> {
    use std::io::Read;
    let path = dir.join("g.bin");
    let mut file = std::io::BufReader::new(std::fs::File::open(&path)?);
    let mut magic = [0u8; 4];
    file.read_exact(&mut magic)?;
    if &magic != b"C8TB" {
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
    let mut zr = zstd::Decoder::new(file)?;
    let mut out = vec![0u16; count];
    let mut buf = vec![0u8; count * 2];
    zr.read_exact(&mut buf)?;
    for (i, chunk) in buf.chunks_exact(2).enumerate() {
        out[i] = u16::from_le_bytes(chunk.try_into().unwrap());
    }
    Ok(out)
}

/// Per-frame band sizes for a horizon: |{row : e <= f and g <= horizon - f}|.
pub fn band_sizes(table: &RowTable, g: &[u16], horizon: u32) -> Vec<(u32, u64)> {
    let frames = table.watermarks().len() as u32;
    // Difference array over frames: each row is in the band for
    // f in [e, horizon - g] (clamped to the forward range).
    let mut diff = vec![0i64; frames as usize + 2];
    for id in 0..g.len() as u32 {
        let gv = g[id as usize];
        if gv == G_UNREACHABLE {
            continue;
        }
        let Some(e) = table.earliest_frame(id) else { continue };
        if gv as u32 > horizon {
            continue;
        }
        let last = (horizon - gv as u32).min(frames);
        if e > last {
            continue;
        }
        diff[e as usize] += 1;
        diff[last as usize + 1] -= 1;
    }
    let mut out = Vec::with_capacity(frames as usize);
    let mut acc = 0i64;
    for f in 1..=frames {
        acc += diff[f as usize];
        out.push((f, acc as u64));
    }
    out
}
