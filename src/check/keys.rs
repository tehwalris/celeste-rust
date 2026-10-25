//! THE KEY CHECK (`rewrite check keys`): one forward step of a tree, F ->
//! F+1, as the forward runs it, every row the kernels emit and every piece
//! of the new layer shown to `KeyCheck` (the core's one seam,
//! `frame::RowCheck`). An emitted row's key must be what the key space makes
//! of the boundary's row (packed, or provisional of equal content), the
//! boundary must change nothing of its block (structure, shape), and its key
//! and cell must decode to it; a stored piece must be what the boundary keys
//! it as, each state decoded at its own cell. The first bad row PANICS with
//! what differs; the verdict (`differential::verdict`) fails a step that
//! checked no row.

use std::sync::atomic::{AtomicUsize, Ordering};

use anyhow::{ensure, Result};
use celeste_core::arith::Arithmetic;
use celeste_engine::exact::{is_provisional, KeySpace, Provisional, RowKey};
use celeste_engine::runtime2::Rt2;

use crate::compiled::{KernelLanes, Kernels};
use crate::frame::{ForwardState, RowCheck};
use crate::search::rem::Rem;
use crate::search::tree::TreeDir;
use crate::storage::decode::Skeletons;
use crate::storage::unit::RowBuf;

/// The key check as a forward's observer: the rows it checked.
#[derive(Default)]
pub(crate) struct KeyCheck {
    rows: AtomicUsize,
}

impl<N: Rem> RowCheck<N> for KeyCheck {
    fn emitted(&self, rows: &RowBuf<N>, keys: &KeySpace<N>, prov: &Provisional<N>, skeletons: &Skeletons<N>) -> Result<()> {
        let block = rows.to_rt2();
        emitted_keys(&block, keys, prov);
        // A packed row's key and cell decode to it.
        let keep: Vec<u32> = (0..block.width as u32).filter(|&r| !is_provisional(block.row_keys[r as usize])).collect();
        if !keep.is_empty() {
            let mut r = block.clone_block();
            r.retain_lanes(&keep);
            let cells: Vec<u32> = keep.iter().map(|&k| rows.cells[k as usize]).collect();
            skeletons.check_decodes(keys, &r, &cells)?;
        }
        self.rows.fetch_add(block.width, Ordering::Relaxed);
        Ok(())
    }

    fn stored(&self, rows: &Rt2<N>, cells: &[u32], keys: &KeySpace<N>) -> Result<()> {
        stored_keys(rows, keys);
        for (c, want) in crate::storage::cells::block_cells(rows)?.into_iter().zip(cells) {
            ensure!(c == *want, "a state decoded at cell {c}, not its own {want}");
        }
        self.rows.fetch_add(rows.width, Ordering::Relaxed);
        Ok(())
    }
}

/// An EMITTED block (`RowBuf::to_rt2`): the boundary over a copy changes
/// nothing (structure, shape hash), and every row's key is what the key
/// space makes of the boundary's row (packed, or provisional of equal
/// content). Panics with the rows and cells that differ.
fn emitted_keys<N: Arithmetic>(acc: &Rt2<N>, keys: &KeySpace<N>, prov: &Provisional<N>) {
    let mut b = acc.clone_block();
    b.boundary_canonicalize();
    let want = keys.row_keys(&b);
    let bad: Vec<usize> = (0..acc.width)
        .filter(|&i| match &want[i] {
            RowKey::Hit(k) => acc.row_keys[i] != *k,
            RowKey::Miss(m) => !is_provisional(acc.row_keys[i]) || prov.content(acc.row_keys[i]) != *m,
        })
        .collect();
    if b.shape_hash == acc.shape_hash && b.structure == acc.structure && bad.is_empty() {
        return;
    }
    // Say WHICH rows and cells differ.
    let varying: Vec<usize> = (0..acc.cols.len())
        .filter(|&c| matches!(acc.structure.get(c), Some(celeste_engine::runtime2::Cell2::Val)))
        .filter(|&c| (1..acc.width).any(|i| acc.cols[c].at(i) != acc.cols[c].at(0)))
        .collect();
    let rows: Vec<String> = bad
        .iter()
        .take(8)
        .map(|&i| {
            let cells: Vec<String> = varying.iter().map(|&c| format!("{c}={:?}", acc.cols[c].at(i))).collect();
            format!("    row {i} append key {:?}, boundary {:?}: {}", acc.row_keys[i], want[i], cells.join(" "))
        })
        .collect();
    panic!(
        "KERNEL KEY CHECK: shape {:#x}: the boundary disagrees with the append step \
         (width {}, shape {:#x} vs {:#x}, structure {}, {} keys differ)\n{}",
        acc.shape_hash,
        acc.width,
        b.shape_hash,
        acc.shape_hash,
        if b.structure == acc.structure { "same" } else { "DIFFERENT" },
        bad.len(),
        rows.join("\n"),
    );
}

/// A block the forward STORES (a layer's rows, decoded at the frame's
/// end): the boundary over a copy keeps its shape and structure, and the
/// key space keys its rows as stored. Panics otherwise.
fn stored_keys<N: Arithmetic>(blk: &Rt2<N>, keys: &KeySpace<N>) {
    let mut b = blk.clone_block();
    b.boundary_canonicalize();
    let got: Vec<Option<(u64, u64)>> = keys.lookup_keys(&b);
    let same = got.iter().zip(&blk.row_keys).all(|(g, w)| *g == Some(*w));
    assert!(
        b.shape_hash == blk.shape_hash && b.structure == blk.structure && same,
        "KERNEL KEY CHECK: a stored piece of shape {:#x} ({} rows) is not what the boundary keys it as (shape {:#x}, structure {}, keys {})",
        blk.shape_hash,
        blk.width,
        b.shape_hash,
        if b.structure == blk.structure { "same" } else { "DIFFERENT" },
        if same { "same" } else { "DIFFERENT" },
    );
}

/// `check keys` of the tree `t`'s step `step` -> `step + 1` (its edges into
/// a scratch directory, as the forward records them): the step's `ckhash`
/// lines (`f`, `h`, `e`; `ForwardState::lines`) and the rows checked, at
/// least one.
pub fn check<N: KernelLanes>(t: &TreeDir, step: u32) -> Result<(String, usize)> {
    let (cfg, level) = (&t.settings, t.level);
    let engine = Kernels::<N>::new(cfg, level)?;
    let mut st = ForwardState::<N>::at(&t.dir, step, level, cfg)?;
    // On disk, never inside the tree (the fixtures are read-only).
    let scratch = std::path::PathBuf::from(format!("/var/tmp/celeste-check-keys-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&scratch);
    let kc = KeyCheck::default();
    let lines = (|| {
        st.step_with(&engine, Some(&scratch.join("edges")), None, None, Some(&kc))?;
        st.lines(Some(&scratch), None, cfg.threads())
    })();
    let _ = std::fs::remove_dir_all(&scratch);
    let (lines, rows) = (lines?, kc.rows.load(Ordering::Relaxed));
    super::differential::verdict(rows, 0, String::new)?;
    Ok((lines, rows))
}

#[cfg(test)]
mod tests {
    use super::*;

    /// THE KEY CHECK FAILS A CORRUPTED KEY: room (1,0)'s start row, keyed
    /// into a key space (`wave::seed`), in a `RowBuf` as the emission
    /// buffers it, passes `KeyCheck` (and is counted); the same row under
    /// another key fails it.
    #[test]
    fn a_corrupted_key_fails_the_key_check() {
        use celeste_core::arith::Fixed;
        let s = crate::settings::Settings::default();
        let row = crate::engines::reference::ReferenceEngine::<Fixed>::new(&s).expect("engine").initial().expect("start").canonicalized();
        let mut visited = crate::storage::visited::VisitedSet::<Fixed>::new();
        crate::storage::wave::seed(&mut visited, std::slice::from_ref(&row), crate::search::hull::HullOf::<Fixed>::full(), &s).expect("keyed");
        let key = visited.keys.lookup_keys(&row)[0].expect("the row is keyed");
        let cell = crate::storage::cells::block_cells(&row).expect("cells")[0];
        let (prov, skeletons) = (Provisional::default(), Skeletons::default());
        skeletons.register(&row).expect("its skeleton");
        let buf = |k: (u64, u64)| {
            let mut b = RowBuf::new(row.clone_block());
            b.push_row(&row, k, cell);
            b
        };
        let kc = KeyCheck::default();
        RowCheck::<Fixed>::emitted(&kc, &buf(key), &visited.keys, &prov, &skeletons).expect("the row's own key passes");
        assert_eq!(kc.rows.load(Ordering::Relaxed), 1, "the row is counted");
        let bad = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| RowCheck::<Fixed>::emitted(&kc, &buf((key.0 ^ 1, key.1)), &visited.keys, &prov, &skeletons))).expect_err("a corrupted key passed");
        let msg = bad.downcast_ref::<String>().cloned().unwrap_or_default();
        assert!(msg.contains("KERNEL KEY CHECK") && msg.contains("1 keys differ"), "{msg}");
    }
}
