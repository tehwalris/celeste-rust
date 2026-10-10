//! The remainder-free BFS over the recorded graph (`storage::edges`): from
//! the win states back, iteration i marking the states that win by the
//! horizon from frame i but not from i + 1 - with SOME remainder: i is the
//! state's DEADLINE.

use crate::storage::edges::{EdgeStore, InEdge};
use crate::storage::marks::Marks;
use crate::storage::StateId;

pub struct BfsStats {
    pub edges_read: u64,
    pub lookups: u64,
    pub t_bfs: std::time::Duration,
}

/// The BFS backward from `seeds` (win states, each with its layer):
/// iteration i (horizon-1 down to 1) marks a predecessor of an i+1-frontier
/// state iff its layer is <= i, reading the state's in-edges recorded at
/// frames layer..=i+1 once (an edge recorded at frame f leaves a state of
/// layer f - 1). So iteration i marks exactly the states that win by the
/// horizon from frame i but not from i+1. Lookups in parallel, inserts
/// sequential in frontier order: the marks are a function of the graph.
pub fn bfs(store: &EdgeStore, horizon: u32, seeds: impl IntoIterator<Item = (StateId, u32)>) -> (Marks, BfsStats) {
    let t = std::time::Instant::now();
    let mut marks = Marks::new(crate::storage::geometry());
    let mut frontier: Vec<(StateId, u32)> = Vec::new();
    for (s, layer) in seeds {
        if layer <= horizon && marks.insert(s, horizon, layer) {
            frontier.push((s, layer));
        }
    }
    let (mut edges_read, mut lookups) = (0u64, 0u64);
    let workers = crate::frame::threads().max(1);
    for i in (1..horizon).rev() {
        let t_it = std::time::Instant::now();
        frontier.sort_unstable();
        let last_frame = (i + 1).min(horizon);
        let chunk = frontier.len().div_ceil(workers).max(1);
        let found: Vec<(Vec<(StateId, u32)>, u64, u64)> = std::thread::scope(|scope| {
            let hs: Vec<_> = frontier
                .chunks(chunk)
                .map(|part| {
                    let marks = &marks;
                    scope.spawn(move || {
                        let mut out: Vec<(StateId, u32)> = Vec::new();
                        let mut buf: Vec<InEdge> = Vec::new();
                        let (mut lk, mut ed) = (0u64, 0u64);
                        for &(tgt, layer) in part {
                            for frame in layer.max(1)..=last_frame {
                                buf.clear();
                                store.preds_at(tgt, frame, &mut buf);
                                lk += 1;
                                ed += buf.len() as u64;
                                out.extend(buf.iter().filter(|e| !marks.contains(e.src)).map(|e| (e.src, frame - 1)));
                            }
                        }
                        (out, lk, ed)
                    })
                })
                .collect();
            hs.into_iter().map(|h| h.join().expect("bfs worker panicked")).collect()
        });
        let mut next: Vec<(StateId, u32)> = Vec::new();
        for (out, lk, ed) in found {
            lookups += lk;
            edges_read += ed;
            for (p, layer) in out {
                if marks.insert(p, i, layer) {
                    next.push((p, layer));
                }
            }
        }
        eprintln!("[bfs] f{i:03} targets {} marked {} | {:.0} ms", frontier.len(), next.len(), t_it.elapsed().as_secs_f64() * 1e3);
        frontier = next;
    }
    (marks, BfsStats { edges_read, lookups, t_bfs: t.elapsed() })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::storage::edges::{encode_block, file_path, write_file, XferTable};
    use crate::storage::state_id;
    use crate::storage::unit::{pack_edge, pack_owner, Lid, UnitOut};

    /// One frame's edges `(source, target)` as one unit's file (transfer 0).
    fn write_frame(dir: &std::path::Path, frame: u32, edges: &[(StateId, StateId)]) {
        let mut sources: Vec<StateId> = edges.iter().map(|e| e.0).collect();
        sources.sort_unstable();
        sources.dedup();
        let mut targets: Vec<StateId> = edges.iter().map(|e| e.1).collect();
        targets.sort_unstable();
        targets.dedup();
        let lid_of = |t: StateId| targets.iter().position(|&x| x == t).unwrap() as u32;
        let mut packed: Vec<u64> = edges
            .iter()
            .map(|&(s, t)| pack_edge(lid_of(t), crate::storage::id_local(t), sources.iter().position(|&x| x == s).unwrap() as u32, 0))
            .collect();
        packed.sort_unstable();
        let (block, starts) = encode_block(&packed, targets.len());
        let lids = targets.iter().map(|_| Lid { shape: 0, slot: 0, key: (0, 0) }).collect();
        let owners = targets.iter().map(|&t| std::sync::atomic::AtomicU64::new(pack_owner(crate::storage::id_region(t), crate::storage::id_entry(t)))).collect();
        let u = UnitOut { worker: 0, sources, source_rows: None, lids, owners, requests: Vec::new(), pending: Vec::new(), bufs: Vec::new(), block_at: None, block_len: block.len() as u64, block, xfers: vec![0], starts, edges: packed.len() as u64 };
        write_file(&file_path(dir, frame, None), frame, &[u], vec![vec![0]]).unwrap();
    }

    /// A mark's deadline is the last frame it still reaches a win by the
    /// horizon from, through revisits of earlier-layer states too.
    #[test]
    fn a_marks_deadline_is_the_last_frame_it_still_wins_from() {
        let dir = std::env::temp_dir().join(format!("celeste-bfs-deadline-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        // A state per (layer, n): region = layer, entry n.
        let s = |layer: u32, n: u32| state_id(layer, n, 0);
        let (a, b, c, e, w, f, g) = (s(0, 0), s(1, 0), s(2, 0), s(2, 1), s(3, 0), s(3, 1), s(3, 2));
        // a -> b -> c -> w (the win, layer 3); b -> e -> f; and two REVISITS
        // at frame 4: g -> w (in time: a win at 4) and f -> c (too late: c at
        // 4 wins at 5).
        write_frame(&dir, 1, &[(a, b)]);
        write_frame(&dir, 2, &[(b, c), (b, e)]);
        write_frame(&dir, 3, &[(c, w), (e, f)]);
        write_frame(&dir, 4, &[(f, c), (g, w)]);
        let mut xt = XferTable::default();
        let p = crate::search::arc_edges::AxisXfer { lo: 0, hi: crate::search::arcs::CIRCLE, tag: 0, val: 0 };
        xt.merge(Some(&dir), &[vec![(p, p)]]).unwrap();
        let store = EdgeStore::open(&dir, 4).unwrap();
        let (marks, _) = bfs(&store, 4, [(w, 3)]);
        let (ranks, got) = marks.into_ranked();
        let mut want = vec![(a, 1u16, 0u32), (b, 2, 1), (c, 3, 2), (w, 4, 3), (g, 3, 3)];
        want.sort_unstable();
        assert_eq!(got, want, "e and f reach a win only after frame 4");
        for (k, &(id, ..)) in got.iter().enumerate() {
            assert_eq!(ranks.rank(id), Some(k as u32), "a mark's rank is its place in id order");
        }
        assert_eq!(ranks.rank(e), None, "e is not marked");
        std::fs::remove_dir_all(&dir).unwrap();
    }
}
