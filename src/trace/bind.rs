//! Bind a traced kernel to a block, BY PATH. The tracer names a heap slot by
//! its path from the globals (`objects[0].spd.x`); the engine by a canonical
//! cell id (a breadth-first walk from the globals in `GLOBAL_NAMES` order).
//! Rather than make the two numberings agree, the binder resolves the
//! kernel's paths against the block it is handed, once, at bind time
//! (`celeste_engine::slots`). A path that does not resolve is a shape
//! mismatch: a refusal.

use anyhow::{anyhow, bail, Result};

use celeste_core::arith::Arithmetic;
use celeste_engine::runtime2::{Cell2, Rt2};

use crate::transpile::garith::GraphArith;
use celeste_engine::slots;

use super::iface::{show, Path, Step};

/// The canonical cell id a traced path names in this block.
pub fn resolve<N: Arithmetic>(rt2: &Rt2<N>, p: &[Step]) -> Result<u32> {
    slots::resolve_steps(rt2, p).map_err(|e| anyhow!("{}", e))
}

fn kind<N: Arithmetic>(c: &Cell2<N>) -> &'static str {
    match c {
        Cell2::Val => "value",
        Cell2::Obj(_) => "object",
        Cell2::Arr(_) => "array",
        Cell2::Unk => "unknown table",
        Cell2::Clo(..) => "closure",
        Cell2::Bi(_) => "builtin",
    }
}

/// How the first root that reaches `target` does so, as a chain of ops.
fn why_reached<A: GraphArith>(
    g: &crate::transpile::graph::Graph<A>,
    roots: &[crate::transpile::graph::NodeId],
    target: crate::transpile::graph::NodeId,
) -> String {
    use crate::transpile::graph::NodeId;
    for (k, r) in roots.iter().enumerate() {
        let mut parent: std::collections::HashMap<NodeId, NodeId> = Default::default();
        let mut seen: std::collections::HashSet<NodeId> = Default::default();
        let mut stack = vec![*r];
        seen.insert(*r);
        let mut hit = false;
        while let Some(n) = stack.pop() {
            if n == target {
                hit = true;
                break;
            }
            for a in g.get(n).args.iter().copied() {
                if seen.insert(a) {
                    parent.insert(a, n);
                    stack.push(a);
                }
            }
        }
        if !hit {
            continue;
        }
        let mut chain = vec![target];
        let mut at = target;
        while let Some(p) = parent.get(&at) {
            chain.push(*p);
            at = *p;
        }
        chain.reverse();
        let ops: Vec<String> = chain.iter().map(|n| format!("{:?}", g.get(*n).op)).collect();
        return format!("root #{}: {}", k, ops.join(" -> "));
    }
    "no root".to_string()
}

/// Rebuild a traced graph with the ENGINE's cell ids: the tracer numbers
/// input cells densely, `Op::Cell(0..n)` in `Iface` order; the emitter needs
/// the engine's sparse canonical ids. `canon` must be INJECTIVE (checked), or
/// two input slots silently become one cell. Returns the graph over what
/// `roots` reach and the old -> new node map (defined on that cone).
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
    let mut live = vec![false; g.len()];
    let mut stack: Vec<NodeId> = roots.to_vec();
    while let Some(n) = stack.pop() {
        if live[n as usize] {
            continue;
        }
        live[n as usize] = true;
        stack.extend(g.get(n).args.iter().copied());
    }

    let mut out = g.like();
    // The kinds follow the cells to their engine numbers.
    let kinds: Vec<(u32, crate::transpile::graph::Ty)> = g.cell_kinds().filter_map(|(c, k)| Some((*canon.get(c as usize)?, k))).collect();
    out.set_cell_kinds(kinds);
    let mut map: Vec<NodeId> = Vec::with_capacity(g.len());
    for id in 0..g.len() {
        if !live[id] {
            // Never read: an arg of a live node is live, and args have
            // smaller ids, so nothing maps through this.
            map.push(0);
            continue;
        }
        let node = g.get(id as NodeId);
        let new = match node.op {
            Op::Cell(i) => {
                let c = *canon.get(i as usize).ok_or_else(|| {
                    anyhow!(
                        "Op::Cell({}) is not an interface slot ({} slots), reached by {}",
                        i,
                        canon.len(),
                        why_reached(g, roots, id as NodeId)
                    )
                })?;
                out.leaf(Op::Cell(c))
            }
            _ => {
                let args: Vec<NodeId> = node.args.iter().map(|a| map[*a as usize]).collect();
                out.fold(node.op.clone(), args)
            }
        };
        map.push(new);
    }
    Ok((out, map))
}

/// Resolve a whole interface, naming the path that failed - and REFUSE if
/// two paths land on one cell (two outputs in one column: a silently
/// well-formed wrong block; `iface::scalars` yields one path per slot).
pub fn resolve_all<N: Arithmetic>(rt2: &Rt2<N>, paths: &[Path]) -> Result<Vec<u32>> {
    let cells: Vec<u32> = paths
        .iter()
        .map(|p| resolve(rt2, p).map_err(|e| anyhow!("{}: {:#}", show(p), e)))
        .collect::<Result<_>>()?;
    let mut seen: std::collections::HashMap<u32, usize> = Default::default();
    for (i, c) in cells.iter().enumerate() {
        // A slot holds a VALUE; writing to an object cell would overwrite
        // the pointer topology.
        if !matches!(rt2.structure[*c as usize], Cell2::Val) {
            bail!("{} resolves to a {}, not a value cell", show(&paths[i]), kind(&rt2.structure[*c as usize]));
        }
        if let Some(j) = seen.insert(*c, i) {
            bail!(
                "{} and {} both resolve to cell {} - the structure merged two slots",
                show(&paths[j]),
                show(&paths[i]),
                c
            );
        }
    }
    Ok(cells)
}

#[cfg(test)]
mod tests {
    use super::*;
    use celeste_engine::runtime2::{Col, AV, NONE};
    use celeste_names::gen;
    use crate::trace::iface::key;

    /// Rebuild a shape witness (a real boundary shape with its canonical cell
    /// ids) as a block, structure only.
    fn block_from_witness(path: &str) -> Rt2 {
        let text = std::fs::read_to_string(path).expect("witness");
        let w: serde_json::Value = serde_json::from_str(&text).expect("json");
        let cells = w["cells"].as_array().expect("cells");
        let cache = crate::compiled::collision_cache(crate::settings::Settings::default().room);
        let mut rt2 = Rt2::empty(1, gen::GLOBAL_NAMES.len(), gen::STRINGS, cache);
        rt2.structure = Vec::with_capacity(cells.len());
        rt2.cols = Vec::with_capacity(cells.len());
        for c in cells {
            let (cell, col) = match c["k"].as_str().unwrap_or("") {
                "val" => match c["content"]["ptr"].as_u64() {
                    Some(t) => (Cell2::Val, Col::U(AV::Ptr(t as u32))),
                    None => (Cell2::Val, Col::U(AV::Nil)),
                },
                "obj" => (
                    Cell2::Obj(
                        c["fields"]
                            .as_object()
                            .expect("fields")
                            .iter()
                            .filter_map(|(k, v)| {
                                gen::field_id(k).map(|f| (f, v.as_u64().unwrap() as u32))
                            })
                            .collect(),
                    ),
                    Col::U(AV::Nil),
                ),
                "arr" => (
                    Cell2::Arr(
                        c["items"]
                            .as_array()
                            .expect("items")
                            .iter()
                            .map(|v| v.as_u64().unwrap() as u32)
                            .collect(),
                    ),
                    Col::U(AV::Nil),
                ),
                "clo" => (Cell2::Clo(0, Box::new([])), Col::U(AV::Nil)),
                "bi" => (Cell2::Bi(0), Col::U(AV::Nil)),
                _ => (Cell2::Unk, Col::U(AV::Nil)),
            };
            rt2.structure.push(cell);
            rt2.cols.push(col);
        }
        rt2.globals = gen::GLOBAL_NAMES
            .iter()
            .map(|n| w["globals"][*n].as_u64().map(|v| v as u32).unwrap_or(NONE))
            .collect();
        rt2
    }

    /// The witness records, for every cell, the path the boundary's walk
    /// reached it by; resolving it must land on that cell or on the slot
    /// pointing at it (a pointer and its pointee share a name).
    #[test]
    fn every_path_in_a_shape_witness_resolves_to_the_cell_it_names() {
        for w in [
            "test-fixtures/witness/steady-shape.json",
            "test-fixtures/witness/r20-steady-shape.json",
        ] {
            let rt2 = block_from_witness(w);
            let text = std::fs::read_to_string(w).expect("witness");
            let j: serde_json::Value = serde_json::from_str(&text).expect("json");
            let cells = j["cells"].as_array().expect("cells");
            let (mut ok, mut skipped) = (0usize, 0usize);
            for (id, c) in cells.iter().enumerate() {
                let name = c["name"].as_str().unwrap_or("");
                if name.is_empty() {
                    skipped += 1;
                    continue;
                }
                // Witness names use 1-based array indices; `Idx` is 0-based.
                let p: Path = name
                    .split('.')
                    .map(|s| match s.parse::<usize>() {
                        Ok(n) if n >= 1 => Step::Idx(n - 1),
                        _ => key(s),
                    })
                    .collect();
                // A field the boundary does not name cannot be resolved.
                if p.iter().any(|s| match s {
                    Step::Key(k) => gen::field_id(k).is_none() && !gen::GLOBAL_NAMES.contains(&k.as_str()),
                    _ => false,
                }) {
                    skipped += 1;
                    continue;
                }
                let got = resolve(&rt2, &p)
                    .unwrap_or_else(|e| panic!("{}: {} did not resolve: {:#}", w, name, e));
                let landed = got == id as u32 || slots::deref(&rt2, got).map(|t| t == id as u32).unwrap_or(false);
                assert!(landed, "{}: {} resolved to {} not {}", w, name, got, id);
                ok += 1;
            }
            eprintln!("[bind] {}: {} paths resolved, {} skipped", w, ok, skipped);
            assert!(ok > 100, "{}: only {} paths checked", w, ok);
        }
    }

    /// Every scalar the boundary can name lands on a DISTINCT `Val` cell of
    /// the structure the tracer builds (a merge would be silent corruption).
    #[test]
    fn a_traced_state_becomes_a_structure_every_path_can_walk() {
        use crate::trace::{cart, domain::Symbolic, interp::Interp};
        let s = crate::settings::Settings::default();
        let chunks = cart::Chunks::parse(&s).expect("chunks");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let mut st = cart::boot(&mut it, &chunks, s.room).expect("boot");
        for _ in 0..12 {
            st = cart::run_one(&mut it, &chunks.frame, st).expect("warm-up");
        }
        let cache = it.cache.clone().expect("cache");

        let rt2: Rt2 = crate::trace::block::skeleton(&st, cache).expect("structure");
        let mut seen: std::collections::HashMap<u32, Path> = Default::default();
        let (mut ok, mut unnameable) = (0usize, 0usize);
        for p in crate::trace::iface::scalars(&st, &[]).expect("scalars") {
            // A slot the boundary cannot name was dropped on purpose.
            let nameable = p.iter().enumerate().all(|(i, s)| match s {
                Step::Key(k) if i == 0 => gen::GLOBAL_NAMES.contains(&k.as_str()),
                Step::Key(k) => gen::field_id(k).is_some(),
                _ => true,
            });
            if !nameable {
                unnameable += 1;
                continue;
            }
            let c = resolve(&rt2, &p)
                .unwrap_or_else(|e| panic!("{} did not resolve: {:#}", show(&p), e));
            assert!(
                matches!(rt2.structure[c as usize], Cell2::Val),
                "{} resolved to a {}",
                show(&p),
                kind(&rt2.structure[c as usize])
            );
            if let Some(other) = seen.insert(c, p.clone()) {
                panic!("{} and {} are the same cell {}", show(&other), show(&p), c);
            }
            ok += 1;
        }
        eprintln!(
            "[bind] traced state: {} paths -> {} distinct cells ({} not nameable by the boundary)",
            ok,
            seen.len(),
            unnameable
        );
        assert!(ok > 50, "only {} paths checked", ok);
    }

    /// The guard against two outputs in one column: the same slot twice.
    #[test]
    fn two_paths_landing_on_one_cell_are_refused() {
        let rt2 = block_from_witness("test-fixtures/witness/steady-shape.json");
        let p: Path = vec![key("objects"), Step::Idx(0)];
        assert!(resolve_all(&rt2, &[p.clone()]).is_ok(), "one path is fine");
        let e = resolve_all(&rt2, &[p.clone(), p])
            .expect_err("the same cell twice must be refused")
            .to_string();
        assert!(e.contains("merged two slots"), "unexpected error: {}", e);
    }

    /// A path that is not in the shape is a REFUSAL, not a panic or a wrong
    /// cell: a kernel handed the wrong shape declines to bind.
    #[test]
    fn a_path_outside_the_shape_is_refused() {
        let rt2 = block_from_witness("test-fixtures/witness/steady-shape.json");
        for bad in [
            vec![key("no_such_global")],
            vec![key("objects"), Step::Idx(99)],
            vec![key("objects"), Step::Idx(0), key("no_such_field")],
            vec![key("freeze"), key("x")],
        ] {
            assert!(resolve(&rt2, &bad).is_err(), "{} should not resolve", show(&bad));
        }
    }
}
