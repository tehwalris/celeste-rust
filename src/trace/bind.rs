//! Bind a traced kernel to a block, BY PATH.
//!
//! The tracer names a heap slot by its path from the globals
//! (`objects[0].spd.x`); the engine by a canonical cell id (a breadth-first
//! walk from the globals in `GLOBAL_NAMES` order). Rather than make the two
//! numberings agree, the kernel names its slots by path and the binder
//! resolves them against the block it is handed, once, at bind time. A path
//! that does not resolve is a shape mismatch: a refusal.
//!
//! A pointer is a cell of its own (a `Cell2::Val` holding `AV::Ptr(t)`), so
//! a path dereferences between steps but NOT at the end: the last step lands
//! on the `Val` cell holding the scalar, which the kernel reads and writes.

use anyhow::{anyhow, bail, Result};

use celeste_engine::runtime2::{Cell2, Col, Rt2, AV, NONE};
use celeste_engine::slots;
use celeste_names::gen;

use super::iface::{show, Path, Step};

/// The tracer's path as the engine's: converting keeps ONE implementation
/// of the walk (`celeste_engine::slots`).
fn steps(p: &[Step]) -> Vec<slots::PathStep> {
    p.iter()
        .map(|s| match s {
            Step::Key(k) => slots::PathStep::Key(k.clone()),
            Step::Idx(i) => slots::PathStep::Idx(*i),
            Step::Int(i) => slots::PathStep::Int(*i),
        })
        .collect()
}

/// The canonical cell id a traced path names in this block.
pub fn resolve(rt2: &Rt2, p: &[Step]) -> Result<u32> {
    slots::resolve_steps(rt2, &steps(p)).map_err(|e| anyhow!("{}", e))
}

fn kind(c: &Cell2) -> &'static str {
    match c {
        Cell2::Val => "value",
        Cell2::Obj(_) => "object",
        Cell2::Arr(_) => "array",
        Cell2::Unk => "unknown table",
        Cell2::Clo(..) => "closure",
        Cell2::Bi(_) => "builtin",
    }
}

/// Build the ENGINE's structure for a traced state: the same boxed layout
/// `trace::refbridge::to_block` builds, over the symbolic heap. Needed
/// because an outcome that allocates ends in a shape the input block lacks.
///
/// The ids are the engine's canonical numbering by construction;
/// `the_structure_a_traced_state_becomes_is_already_canonical` checks that
/// `Rt2::canonicalize_ids` is the identity on it.
///
/// Structure only: `cols` carries pointers (the resolver follows them) and
/// `AV::Nil` everywhere else.
pub fn structure_of(
    st: &super::state::State<super::domain::Symbolic>,
    cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
) -> Result<Rt2> {
    use super::heap::Value;

    let mut rt2 = Rt2::empty(1, gen::GLOBAL_NAMES.len(), gen::STRINGS, cart, cache);
    rt2.structure = Vec::new();
    rt2.cols = Vec::new();

    // What each cell still owes: a `Val` its pointer, a placeholder table
    // cell its body.
    type V = Value<super::domain::Symbolic>;
    enum Todo {
        Val(V),
        Table(u32),
        Done,
    }
    let mut todo: Vec<Todo> = Vec::new();
    let mut table_cell: std::collections::HashMap<u32, u32> = Default::default();

    // Cells are created in DISCOVERY order and never reordered, so walking
    // the array from 0 upwards IS the breadth-first walk. The order is
    // load-bearing: a pointee is appended past the cursor, never created
    // inside the slot that points at it.
    let mut globals = vec![NONE; gen::GLOBAL_NAMES.len()];
    let groot = &st.heap.tables[&st.globals];
    for (gi, name) in gen::GLOBAL_NAMES.iter().enumerate() {
        if let Some(v) = groot.hash.get(*name) {
            rt2.structure.push(Cell2::Val);
            rt2.cols.push(Col::U(AV::Nil));
            todo.push(Todo::Val(v.clone()));
            globals[gi] = (rt2.structure.len() - 1) as u32;
        }
    }
    rt2.globals = globals;

    let mut c = 0usize;
    while c < rt2.structure.len() {
        match std::mem::replace(&mut todo[c], Todo::Done) {
            Todo::Val(Value::Table(t)) => {
                let target = match table_cell.get(&t) {
                    Some(x) => *x,
                    None => {
                        rt2.structure.push(Cell2::Unk);
                        rt2.cols.push(Col::U(AV::Nil));
                        todo.push(Todo::Table(t));
                        let x = (rt2.structure.len() - 1) as u32;
                        table_cell.insert(t, x);
                        x
                    }
                };
                rt2.cols[c] = Col::U(AV::Ptr(target));
            }
            Todo::Val(Value::Func(f)) => {
                // The engine names a closure by index into `FN_NAMES`
                // (`Interp::fn_id_of`). An unnamed one is refused: the
                // cart's anonymous functions never outlive their frame.
                let cl = st.heap.closures[&f].clone();
                let id = cl
                    .fn_id
                    .ok_or_else(|| anyhow!("closure {} has no name for FN_NAMES", f))?;
                // The captures are columns on the closure cell, hashed into
                // the row key.
                let mut caps: Vec<Col> = Vec::new();
                for name in &cl.captures {
                    let v = st
                        .heap
                        .lookup(cl.env, name)
                        .ok_or_else(|| anyhow!("capture {:?} is not in scope", name))?;
                    caps.push(Col::U(match v {
                        Value::Table(t) => {
                            // Every capture in this cart is the owning
                            // object, already visited; inventing a cell
                            // here would break the canonical order.
                            AV::Ptr(*table_cell.get(t).ok_or_else(|| {
                                anyhow!("capture {:?} points at an unvisited table", name)
                            })?)
                        }
                        other => bail!("capture {:?} holds {:?}", name, other),
                    }));
                }
                rt2.structure.push(Cell2::Clo(id, caps.into_boxed_slice()));
                rt2.cols.push(Col::U(AV::Nil));
                todo.push(Todo::Done);
                rt2.cols[c] = Col::U(AV::Ptr((rt2.structure.len() - 1) as u32));
            }
            Todo::Val(Value::Builtin(name)) => {
                // IN PLACE, not behind a pointer: a block puts the builtin's
                // `BUILTIN_NAMES` index AT the slot.
                let i = crate::builtins::BUILTIN_NAMES
                    .iter()
                    .position(|n| *n == name)
                    .ok_or_else(|| anyhow!("unknown builtin {:?}", name))?;
                rt2.structure[c] = Cell2::Bi(i as u32);
            }
            Todo::Val(_) => {}
            Todo::Table(t) => {
                let tab = &st.heap.tables[&t];
                // A table is an object OR an array to the engine, never both.
                if !tab.hash.is_empty() && !tab.arr.is_empty() {
                    bail!("table {} has both a hash and an array part", t);
                }
                // Integer keys past the end of the array (`got_fruit[1 +
                // level_index()]` on an empty table) are a sparse `ints`
                // map in the tracer's heap. Flatten them into a dense array
                // padded with nils, as `refbridge::to_block` does, or the
                // shape hash differs.
                let arr: Vec<V> = if tab.ints.is_empty() {
                    tab.arr.clone()
                } else {
                    if !tab.hash.is_empty() {
                        bail!(
                            "table {} has integer keys AND named fields - the engine's \
                             Cell2 is an object or an array, never both",
                            t
                        );
                    }
                    // 1-based: a non-positive key has no place, refused.
                    let Some(&top) = tab.ints.keys().next_back() else {
                        unreachable!("checked non-empty")
                    };
                    if *tab.ints.keys().next().unwrap() < 1 {
                        bail!(
                            "table {} has a non-positive integer key - it has no place \
                             in a dense array part",
                            t
                        );
                    }
                    let mut arr = tab.arr.clone();
                    arr.resize(top as usize, Value::Nil);
                    for (k, v) in tab.ints.iter() {
                        arr[*k as usize - 1] = v.clone();
                    }
                    arr
                };
                let slot = |rt2: &mut Rt2, todo: &mut Vec<Todo>, v: &V| -> u32 {
                    rt2.structure.push(Cell2::Val);
                    rt2.cols.push(Col::U(AV::Nil));
                    todo.push(Todo::Val(v.clone()));
                    (rt2.structure.len() - 1) as u32
                };
                // An EMPTY table has no kind yet: `Cell2::Unk`, not an
                // object with no fields (a different cell to the shape hash).
                let body = if tab.hash.is_empty() && arr.is_empty() {
                    Cell2::Unk
                } else if arr.is_empty() {
                    // Fields the boundary does not name are dropped, as
                    // `refbridge::to_block` drops them.
                    let mut fields: Vec<(u32, u32)> = Vec::new();
                    for (k, v) in tab.hash.iter() {
                        let Some(f) = gen::field_id(k) else { continue };
                        fields.push((f, slot(&mut rt2, &mut todo, v)));
                    }
                    Cell2::Obj(fields)
                } else {
                    let items: Vec<u32> =
                        arr.iter().map(|v| slot(&mut rt2, &mut todo, v)).collect();
                    Cell2::Arr(items)
                };
                rt2.structure[c] = body;
            }
            Todo::Done => {}
        }
        c += 1;
    }
    Ok(rt2)
}


/// How the first root that reaches `target` does so, as a chain of ops:
/// so an error names a path instead of a node id.
fn why_reached(
    g: &crate::transpile::graph::Graph,
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


/// Rebuild a traced graph with the ENGINE's cell ids.
///
/// The tracer numbers input cells densely, `Op::Cell(0..n)` in `Iface`
/// order (its evaluator indexes them so); the emitter needs the engine's
/// sparse canonical ids. `canon` must be INJECTIVE (checked), or two input
/// slots silently become one cell.
pub fn renumber_cells(
    g: &crate::transpile::graph::Graph,
    canon: &[u32],
    roots: &[crate::transpile::graph::NodeId],
) -> Result<(crate::transpile::graph::Graph, Vec<crate::transpile::graph::NodeId>)> {
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
    let kinds: Vec<(u32, crate::transpile::graph::CellKind)> = g.cell_kinds().collect();
    for (c, k) in kinds {
        if let Some(&e) = canon.get(c as usize) {
            out.set_cell_kind(e, k);
        }
    }
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
    Ok((out, roots.iter().map(|r| map[*r as usize]).collect()))
}

/// Resolve a traced frame's INPUT interface against a block: the canonical
/// cell for each `Op::Cell(i)`.
///
/// Inputs only: an outcome that allocates names cells the input block does
/// not have, so outputs bind against their own shape (`structure_of`).
pub fn bind_inputs(rt2: &Rt2, iface: &super::iface::Iface) -> Result<Vec<u32>> {
    resolve_all(rt2, &iface.slots)
}

/// Resolve a whole interface, naming the path that failed - and REFUSE
/// if two paths land on one cell.
///
/// Two paths sharing a cell would mean two outputs written to one column,
/// a silently well-formed wrong block. `iface::scalars` yields one path per
/// distinct slot, so a collision is a bug in the structure.
pub fn resolve_all(rt2: &Rt2, paths: &[Path]) -> Result<Vec<u32>> {
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
    use crate::trace::iface::key;

    /// Rebuild a shape witness (a real boundary shape with its canonical
    /// cell ids) as a block. Structure only: the resolver never reads a
    /// scalar.
    fn block_from_witness(path: &str) -> Rt2 {
        let text = std::fs::read_to_string(path).expect("witness");
        let w: serde_json::Value = serde_json::from_str(&text).expect("json");
        let cells = w["cells"].as_array().expect("cells");
        let cart = std::sync::Arc::new(
            celeste_core::cart_data::CartData::load("cart").expect("cart"),
        );
        let (rx, ry) = crate::game_runner::start_room();
        let cache = std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cart, rx, ry).expect("cache"),
        );
        let mut rt2 = Rt2::empty(1, gen::GLOBAL_NAMES.len(), gen::STRINGS, cart, cache);
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

    /// `structure_of` emits the engine's numbering directly: canonicalizing
    /// what it produces must be the IDENTITY. Never compared at runtime, so
    /// a divergence would silently misread every later cell.
    #[test]
    fn the_structure_a_traced_state_becomes_is_already_canonical() {
        use crate::trace::{cart, domain::Symbolic, interp::Interp, verify::run_one};

        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let frame = full_moon::parse(cart::FRAME_CODE).expect("parse frame");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cd = std::sync::Arc::new(
            celeste_core::cart_data::CartData::load("cart").expect("cart"),
        );
        let (rx, ry) = crate::game_runner::start_room();
        let cache = std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cd, rx, ry).expect("cache"),
        );
        it.cache = Some(cache.clone());
        it.cart = Some(cd.clone());
        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let mut st = run_one(&mut it, &init, st).expect("_init");
        for _ in 0..12 {
            st = run_one(&mut it, &frame, st).expect("warm-up");
        }

        let rt2 = structure_of(&st, cd.clone(), cache.clone()).expect("structure");
        // Built twice: `Rt2` is deliberately not `Clone`.
        let mut canon = structure_of(&st, cd, cache).expect("structure");
        canon.canonicalize_ids();
        assert_eq!(
            rt2.globals, canon.globals,
            "the globals point at different cells after canonicalizing"
        );
        assert_eq!(
            rt2.structure.len(),
            canon.structure.len(),
            "canonicalizing dropped {} cells as unreachable",
            rt2.structure.len() as i64 - canon.structure.len() as i64
        );
        let first = (0..rt2.structure.len())
            .find(|i| format!("{:?}", rt2.structure[*i]) != format!("{:?}", canon.structure[*i]));
        assert!(
            first.is_none(),
            "cell {} differs: {:?} vs canonical {:?}",
            first.unwrap(),
            rt2.structure[first.unwrap()],
            canon.structure[first.unwrap()]
        );
    }

    /// Every scalar the boundary can name lands on a DISTINCT `Val` cell of
    /// the structure the tracer builds (a merge would be silent corruption).
    #[test]
    fn a_traced_state_becomes_a_structure_every_path_can_walk() {
        use crate::trace::{cart, domain::Symbolic, interp::Interp, verify::run_one};

        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let frame = full_moon::parse(cart::FRAME_CODE).expect("parse frame");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cd = std::sync::Arc::new(
            celeste_core::cart_data::CartData::load("cart").expect("cart"),
        );
        let (rx, ry) = crate::game_runner::start_room();
        let cache = std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cd, rx, ry).expect("cache"),
        );
        it.cache = Some(cache.clone());
        it.cart = Some(cd.clone());
        let st = cart::fresh_state::<Symbolic>(&mut it.d);
        let mut st = run_one(&mut it, &top, st).expect("toplevel");
        cart::inject_tile_flag_at(&mut st);
        let mut st = run_one(&mut it, &init, st).expect("_init");
        for _ in 0..12 {
            st = run_one(&mut it, &frame, st).expect("warm-up");
        }

        let rt2 = structure_of(&st, cd, cache).expect("structure");
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
