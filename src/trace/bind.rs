//! A traced graph in the ENGINE's numbering. The tracer numbers its input
//! cells densely, `Op::Cell(0..n)` in `Iface` order; each is the canonical
//! cell the converter made for its slot (`block::skeleton_bound`, which
//! refuses a slot the block does not hold and two slots on one cell).

use anyhow::{bail, Result};

use crate::transpile::garith::GraphArith;

/// Rebuild a traced graph with the ENGINE's cell ids: the tracer numbers
/// input cells densely, `Op::Cell(0..n)` in `Iface` order; the emitter needs
/// the engine's sparse canonical ids. `canon` must be INJECTIVE (checked), or
/// two input slots silently become one cell. Returns the graph over what
/// `roots` reach and the old -> new node map (`graph::UNBUILT` off that
/// cone).
pub fn renumber_cells<A: GraphArith>(
    g: &crate::transpile::graph::Graph<A>,
    canon: &[u32],
    roots: &[crate::transpile::graph::NodeId],
) -> Result<(crate::transpile::graph::Graph<A>, Vec<crate::transpile::graph::NodeId>)> {
    use crate::transpile::graph::{NodeId, Op};
    let mut seen: std::collections::HashMap<u32, usize> = Default::default();
    for (i, c) in canon.iter().enumerate() {
        if let Some(j) = seen.insert(*c, i) {
            bail!("input slots {} and {} both map to cell {}", j, i, c);
        }
    }
    // Only what the ROOTS reach: the arena is shared between frames, whose
    // `Op::Cell` ids are each frame's own.
    let live = g.reachable(roots);
    if let Some((id, i)) = (0..g.len()).filter(|&id| live[id]).find_map(|id| match g.get(id as NodeId).op {
        Op::Cell(i) if canon.get(i as usize).is_none() => Some((id, i)),
        _ => None,
    }) {
        bail!("Op::Cell({}) is not an interface slot ({} slots), reached by {}", i, canon.len(), crate::transpile::why::Explain::of(g).path(roots, id as NodeId));
    }
    let mut out = g.like();
    // The kinds follow the cells to their engine numbers.
    let kinds: Vec<(u32, crate::transpile::graph::Ty)> = g.cell_kinds().filter_map(|(c, k)| Some((*canon.get(c as usize)?, k))).collect();
    out.set_cell_kinds(kinds);
    let map = g.rebuild_into(Some(&live), &mut out, |_, node, _, out| match node.op {
        Op::Cell(i) => Some(out.leaf(Op::Cell(canon[i as usize]))),
        _ => None,
    });
    Ok((out, map))
}

#[cfg(test)]
mod tests {
    use celeste_core::arith::Fixed;
    use celeste_engine::runtime2::Rt2;
    use celeste_engine::shape::Cell;
    use celeste_names::gen;

    use crate::trace::block::skeleton_bound;
    use crate::trace::iface::{key, show, Path, Step};

    /// Every scalar slot the boundary can name binds to a DISTINCT `Val`
    /// cell of the block the converter makes of the state (two slots on one
    /// cell would be silent corruption); one slot twice, and slots the block
    /// does not hold, are REFUSED.
    #[test]
    fn every_traced_slot_binds_to_its_own_value_cell() {
        use crate::trace::{cart, domain::Symbolic, interp::Interp};
        let s = crate::settings::Settings::default();
        let chunks = cart::Chunks::parse(&s).expect("chunks");
        let mut it: Interp<Symbolic<Fixed>> = Interp::new(Symbolic::default());
        let mut st = cart::boot(&mut it, &chunks, s.room).expect("boot");
        for _ in 0..12 {
            st = cart::run_one(&mut it, &chunks.frame, st).expect("warm-up");
        }
        let room = it.room.expect("the room");
        // A slot the boundary cannot name was dropped on purpose.
        let nameable = |p: &Path| {
            p.iter().enumerate().all(|(i, s)| match s {
                Step::Key(k) if i == 0 => gen::GLOBAL_NAMES.contains(&k.as_str()),
                Step::Key(k) => gen::field_id(k).is_some(),
                _ => true,
            })
        };
        let paths: Vec<Path> = crate::trace::iface::scalars(&st, &[]).expect("scalars").into_iter().filter(nameable).collect();
        assert!(paths.len() > 50, "only {} slots", paths.len());
        let (rt2, cells): (Rt2<Fixed>, Vec<u32>) = skeleton_bound(&st, room, &paths).expect("every slot binds");
        assert!(cells.iter().all(|&c| rt2.cells()[c as usize] == Cell::Val), "each on a value cell");
        let x: Path = vec![key("objects"), Step::Idx(0), key("x")];
        let Err(e) = skeleton_bound::<_, Fixed>(&st, room, &[x.clone(), x]) else { panic!("one slot twice binds") };
        assert!(e.to_string().contains("merged two slots"), "{e}");
        for bad in [vec![key("no_such_global")], vec![key("objects"), Step::Idx(99), key("x")], vec![key("objects"), Step::Idx(0), key("no_such_field")]] {
            assert!(skeleton_bound::<_, Fixed>(&st, room, std::slice::from_ref(&bad)).is_err(), "{} should not bind", show(&bad));
        }
    }
}
