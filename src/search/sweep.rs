//! What the backward pass PRODUCES, and what everything downstream reads.
//!
//! The pass itself is `sweep_time`; this is its vocabulary and its storage:
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
//! A horizon-N sweep only ever populates `g` where `e + g <= N`: it never
//! looks past its horizon, and the band above cannot see the difference.
//! Comparing two `g` arrays across horizons has to threshold first
//! (`tools/gdiff.py --threshold`).
//!
//! Soundness: everything here over-approximates concrete reachability the
//! same way the forward pass does (widened rem, certified quotients), so an
//! empty band proves impossibility at the horizon; achievability is only
//! ever claimed from a fully concrete witness replay.

use anyhow::{anyhow, Result};
use std::path::Path;

use crate::interpreter::row_table::RowTable;
use crate::interpreter::state::State;

use super::checkpoint;

/// The sweep's own origin column. Distinct from the deopt machinery's
/// `__lane_origin`, which comes and goes inside the same frame execution.
pub const SWEEP_ORIGIN: &str = "__sweep_origin";

/// `g` value meaning "cannot reach the goal (within the explored graph)".
pub const G_UNREACHABLE: u16 = u16::MAX;

/// Row keys of every lane of a canonical boundary state - the ONE row key
/// of the search (the kernel/engine key), the same one the forward pass
/// stored in the row table. Recomputed here from the state's content so a
/// saved lane can be looked up; see `compiled::engine_row_keys`.
pub fn row_keys(state: &State) -> Result<Vec<(u64, u64)>> {
    crate::compiled::engine_row_keys(state)
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

#[cfg(test)]
mod batch_invariance_tests {
    use super::*;
    use crate::interpreter::heap::HeapId;
    use crate::interpreter::state::State;
    use crate::interpreter::value::{HeapValue, MaybeVector, Value};
    use crate::pico8_num::Pico8Num;

    /// A lane's canonical key must not depend on which other lanes happen
    /// to share its state.
    ///
    /// This is the narrowest form of Philippe's batch-invariance principle:
    /// it does not even run a frame. If it fails, the chunk dependence
    /// `simdcheck.sh` measured lives in the KEY, and everything downstream
    /// (frontier dedup, and the sweep's ability to reproduce the forward
    /// pass) inherits it.
    #[test]
    fn a_lanes_key_does_not_depend_on_its_neighbours() {
        // Two columns. `a` varies across all four lanes; `b` is chosen so
        // that some SPLITS of the state make it uniform while the whole
        // state leaves it varying - that is the situation a chunk boundary
        // creates.
        let build = |a: &[i16], b: &[i16]| -> State {
            let mut s = State::new();
            s.vector_size = a.len();
            let ca = s.heap.alloc();
            s.heap.set(
                ca,
                HeapValue::Value(Value::Number(MaybeVector::vector(
                    a.iter().map(|v| Pico8Num::from_i16(*v)).collect(),
                ))),
            );
            let cb = s.heap.alloc();
            s.heap.set(
                cb,
                HeapValue::Value(Value::Number(MaybeVector::vector(
                    b.iter().map(|v| Pico8Num::from_i16(*v)).collect(),
                ))),
            );
            s.global_env.insert("a".to_string(), ca);
            s.global_env.insert("b".to_string(), cb);
            let _ = HeapId::from_raw(0);
            s
        };

        let a = [10i16, 11, 12, 13];
        // Lanes 0-1 share b=7; lanes 2-3 share b=9. So the halves are each
        // uniform in `b` while the whole is not.
        let b = [7i16, 7, 9, 9];

        let whole = row_keys(&build(&a, &b)).expect("keys for the whole state");
        let first = row_keys(&build(&a[..2], &b[..2])).expect("keys for lanes 0-1");
        let second = row_keys(&build(&a[2..], &b[2..])).expect("keys for lanes 2-3");

        let split: Vec<(u64, u64)> = first.iter().chain(second.iter()).copied().collect();
        assert_eq!(
            whole, split,
            "a lane's key changed when its neighbours did - batch invariance is \
             broken in the KEY itself, not in the interpretation"
        );
    }
}
