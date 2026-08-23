//! Bind a traced kernel to a block, BY PATH.
//!
//! The tracer names a heap slot by its path from the globals table
//! (`objects[0].spd.x`). The engine names the same slot by a canonical
//! cell id, assigned by a breadth-first walk from the globals in
//! `GLOBAL_NAMES` order. Two numberings for one thing.
//!
//! Making them agree would mean reproducing that walk inside the tracer
//! and keeping the two implementations in step forever - and any change
//! to the tracer's numbering would change the shape hash, hence the row
//! key, hence what the search dedups on, hence every checkpoint.
//!
//! This does the other thing. The kernel says which slots it wants by
//! PATH, and the binder resolves them against whatever block it is
//! handed, once, at bind time. Nothing has to match: the engine's
//! numbering is untouched, no checkpoint is invalidated, and the shape
//! hash is unchanged. A path that does not resolve is a shape mismatch,
//! which is the all-or-nothing refusal the kernels already have.
//!
//! ## The one thing to get right
//!
//! A pointer is a cell of its own. A global holding a table is a
//! `Cell2::Val` whose column is `AV::Ptr(t)`, and `t` is the `Obj`. So
//! walking a path means dereferencing between steps but NOT at the end:
//! the last step lands on the `Val` cell that holds the scalar, which is
//! the cell the kernel reads and writes.

use anyhow::{anyhow, bail, Result};

use celeste_engine::runtime2::{Cell2, Col, Rt2, AV, NONE};
use celeste_names::gen;

use super::iface::{show, Path, Step};

/// Follow a cell that holds a pointer to the cell it points at. A cell
/// that is already a table is returned unchanged, so this is idempotent.
fn deref(rt2: &Rt2, cell: u32) -> Result<u32> {
    match (&rt2.structure[cell as usize], &rt2.cols[cell as usize]) {
        (Cell2::Val, Col::U(AV::Ptr(t))) => Ok(*t),
        (Cell2::Val, Col::V(vs)) => {
            // Pointer topology is lane-uniform in a block by
            // construction, so a per-lane column of pointers is either
            // all one target or a broken premise. Say which.
            let mut it = vs.iter().filter_map(|v| match v {
                AV::Ptr(t) => Some(*t),
                _ => None,
            });
            let first = it.next().ok_or_else(|| anyhow!("cell {} is not a pointer", cell))?;
            if it.any(|t| t != first) {
                bail!("cell {} points at different tables in different lanes", cell);
            }
            Ok(first)
        }
        (Cell2::Val, _) => bail!("cell {} holds a scalar, not a table", cell),
        _ => Ok(cell),
    }
}

/// The canonical cell id a traced path names in this block.
pub fn resolve(rt2: &Rt2, p: &[Step]) -> Result<u32> {
    let mut cur: Option<u32> = None;
    for step in p {
        cur = Some(match (cur, step) {
            (None, Step::Key(k)) => {
                let gi = gen::GLOBAL_NAMES
                    .iter()
                    .position(|n| n == k)
                    .ok_or_else(|| anyhow!("{}: not a global the boundary names", k))?;
                let c = rt2.globals[gi];
                if c == NONE {
                    bail!("{}: global absent from this block", k);
                }
                c
            }
            (None, s) => bail!("{:?}: the globals table has no such key", s),
            (Some(c), step) => {
                let t = deref(rt2, c)?;
                match (&rt2.structure[t as usize], step) {
                    (Cell2::Obj(fields), Step::Key(k)) => {
                        // Fields outside `FIELD_NAMES` were dropped on
                        // import: no generated code can name them, so a
                        // kernel asking for one is asking for something
                        // that provably is not there.
                        let f = gen::field_id(k)
                            .ok_or_else(|| anyhow!("{}: not a field the boundary names", k))?;
                        *fields
                            .iter()
                            .find(|(n, _)| *n == f)
                            .map(|(_, c)| c)
                            .ok_or_else(|| anyhow!("{}: absent from this shape", k))?
                    }
                    (Cell2::Arr(items), Step::Idx(i)) => *items
                        .get(*i)
                        .ok_or_else(|| anyhow!("index {} past the end of the array", i))?,
                    (Cell2::Unk, _) => bail!("cell {} is an unknown table", t),
                    (s, step) => {
                        bail!("cannot take {:?} of a {}", step, kind(s))
                    }
                }
            }
        });
    }
    cur.ok_or_else(|| anyhow!("the empty path names the globals table, not a cell"))
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

/// Build the ENGINE's structure for a traced state.
///
/// The mirror of `compiled::bridge::import_block`, over the tracer's
/// heap instead of the interpreter's. It exists because an outcome that
/// allocates ends in a shape the input block does not have, so its
/// output paths have nothing to resolve against until someone builds
/// that shape.
///
/// The numbering is the engine's own rule - breadth-first from the
/// globals in `GLOBAL_NAMES` order, object fields sorted by name, array
/// items in order - not because the tracer needs to agree with anything,
/// but because the block this describes will be handed to the engine,
/// which renumbers by that rule anyway. Producing it directly means the
/// ids the kernel writes and the ids the engine reads are the same ids
/// by construction rather than by a lookup table.
///
/// Structure only: `cols` carries pointers, because the resolver follows
/// them, and `AV::Nil` everywhere else. The VALUES are what the kernel
/// computes; this says where they go.
pub fn structure_of(
    st: &super::state::State<super::domain::Symbolic>,
    cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
) -> Result<Rt2> {
    use super::heap::Value;

    let mut rt2 = Rt2::empty(1, gen::GLOBAL_NAMES.len(), gen::STRINGS, cart, cache);
    rt2.structure = Vec::new();
    rt2.cols = Vec::new();
    // What each traced table/closure became, and the cells still owing a
    // body. Breadth-first, so `queue` is drained in order.
    let mut table_cell: std::collections::HashMap<u32, u32> = Default::default();
    let mut queue: std::collections::VecDeque<(u32, bool)> = Default::default();

    let new_cell = |rt2: &mut Rt2, c: Cell2, col: Col| -> u32 {
        rt2.structure.push(c);
        rt2.cols.push(col);
        (rt2.structure.len() - 1) as u32
    };

    // A slot holding `v` becomes one `Val` cell; if `v` is a table or a
    // closure, that cell points at the cell for the object, which is
    // discovered here and filled later.
    macro_rules! slot {
        ($rt2:expr, $v:expr) => {{
            let col = match $v {
                Value::Table(t) => {
                    let id = match table_cell.get(t) {
                        Some(c) => *c,
                        None => {
                            let c = new_cell($rt2, Cell2::Unk, Col::U(AV::Nil));
                            table_cell.insert(*t, c);
                            queue.push_back((*t, false));
                            c
                        }
                    };
                    Col::U(AV::Ptr(id))
                }
                Value::Func(_) => {
                    // A closure is a heap object with an identity, like a
                    // table. Its BODY is what the engine names, and no
                    // kernel reads through one, so the cell exists to
                    // keep the shape honest rather than to be used.
                    let c = new_cell($rt2, Cell2::Clo(0, Box::new([])), Col::U(AV::Nil));
                    Col::U(AV::Ptr(c))
                }
                _ => Col::U(AV::Nil),
            };
            new_cell($rt2, Cell2::Val, col)
        }};
    }

    // Globals, in the boundary's order - which is what makes the rest of
    // the numbering deterministic.
    let groot = &st.heap.tables[&st.globals];
    let mut globals = vec![NONE; gen::GLOBAL_NAMES.len()];
    for (gi, name) in gen::GLOBAL_NAMES.iter().enumerate() {
        if let Some(v) = groot.hash.get(*name) {
            globals[gi] = slot!(&mut rt2, v);
        }
    }
    rt2.globals = globals;

    while let Some((t, _)) = queue.pop_front() {
        let cell = table_cell[&t];
        let tab = &st.heap.tables[&t];
        // A table is an object OR an array to the engine, never both.
        // No table in the cart uses both parts, and a table that did
        // could not be described at all - so say so rather than pick.
        if !tab.hash.is_empty() && !tab.arr.is_empty() {
            bail!("table {} has both a hash and an array part", t);
        }
        if !tab.ints.is_empty() {
            bail!("table {} has integer keys outside the array part", t);
        }
        let body = if tab.arr.is_empty() {
            // Fields the boundary does not name are dropped, exactly as
            // `import_block` drops them: no generated code can access
            // one, so it is unreachable weight.
            let mut fields: Vec<(u32, u32)> = Vec::new();
            for (k, v) in tab.hash.iter() {
                let Some(f) = gen::field_id(k) else { continue };
                fields.push((f, slot!(&mut rt2, v)));
            }
            Cell2::Obj(fields)
        } else {
            Cell2::Arr(tab.arr.iter().map(|v| slot!(&mut rt2, v)).collect())
        };
        rt2.structure[cell as usize] = body;
    }
    Ok(rt2)
}

/// Resolve a traced frame's INPUT interface against a block.
///
/// `Iface::slots` is the tracer's input list, in the order its cells
/// were numbered, so the result is "canonical cell for `Op::Cell(i)`" -
/// which is what a kernel needs to read its inputs off a block.
///
/// Inputs only, on purpose. Each OUTCOME ends in its own heap shape, and
/// an outcome that allocates - a death making a new player, a fruit
/// leaving - names cells the input block does not have. Binding those
/// needs a shape descriptor per outcome, which is the multi-output-shape
/// work; resolving them against the input block would silently succeed
/// for the outcomes that happen not to allocate and fail confusingly for
/// the rest.
pub fn bind_inputs(rt2: &Rt2, iface: &super::iface::Iface) -> Result<Vec<u32>> {
    resolve_all(rt2, &iface.slots)
}

/// Resolve a whole interface, naming the path that failed - and REFUSE
/// if two paths land on one cell.
///
/// The distinctness check is the important half. A kernel writes an
/// output by doing `cols[resolve(path)] = value`, so two paths sharing a
/// cell means two values written to one column: last write wins, one
/// field silently wrong, the other's value gone. The block that comes
/// out is still well-formed - right cells, right kinds, hashes fine - so
/// nothing downstream notices, and the symptom is a wrong search result
/// far from the cause.
///
/// It cannot be a legitimate case. `iface::scalars` yields one path per
/// distinct slot (aliases collapse to the first path that reaches them),
/// so a collision here means the STRUCTURE merged two slots that are
/// not the same slot. That is a bug in whoever built it, and this is
/// where it is cheap to say so: once per bind, not once per lane.
pub fn resolve_all(rt2: &Rt2, paths: &[Path]) -> Result<Vec<u32>> {
    let cells: Vec<u32> = paths
        .iter()
        .map(|p| resolve(rt2, p).map_err(|e| anyhow!("{}: {:#}", show(p), e)))
        .collect::<Result<_>>()?;
    let mut seen: std::collections::HashMap<u32, usize> = Default::default();
    for (i, c) in cells.iter().enumerate() {
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

    /// Rebuild a shape witness as a block.
    ///
    /// The witness is a real boundary shape a real search produced,
    /// recorded with the canonical cell ids it had - which makes it the
    /// only ground truth available without running the search. Only the
    /// structure is rebuilt: the resolver walks pointers and fields and
    /// never looks at a scalar.
    fn block_from_witness(path: &str) -> Rt2 {
        let text = std::fs::read_to_string(path).expect("witness");
        let w: serde_json::Value = serde_json::from_str(&text).expect("json");
        let cells = w["cells"].as_array().expect("cells");
        let cart = std::sync::Arc::new(
            celeste_core::cart_data::CartData::load("cart").expect("cart"),
        );
        let (rx, ry) = celeste_interp::game_runner::start_room();
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

    /// The witness records, for every cell, the path the boundary's own
    /// walk reached it by. Resolving that path has to land on that cell.
    ///
    /// "Land on" rather than "equal", because a pointer is a cell of its
    /// own: the global slot `objects` and the array it points at BOTH
    /// carry the name `objects`, and a path names the slot. So the test
    /// is that the path resolves to the cell or to the slot pointing at
    /// it - which is also exactly the relation a kernel needs, since it
    /// reads the slot.
    #[test]
    fn every_path_in_a_shape_witness_resolves_to_the_cell_it_names() {
        for w in [
            "crates/celeste-kernels/witness/steady-shape.json",
            "crates/celeste-kernels/witness/r20-steady-shape.json",
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
                // Witness names are dotted with 1-based array indices;
                // the tracer's `Idx` is 0-based, which is also what
                // `Cell2::Arr` stores.
                let p: Path = name
                    .split('.')
                    .map(|s| match s.parse::<usize>() {
                        Ok(n) if n >= 1 => Step::Idx(n - 1),
                        _ => key(s),
                    })
                    .collect();
                // A field the boundary does not name cannot be resolved
                // by anything, and is not a failure of the resolver.
                if p.iter().any(|s| match s {
                    Step::Key(k) => gen::field_id(k).is_none() && !gen::GLOBAL_NAMES.contains(&k.as_str()),
                    _ => false,
                }) {
                    skipped += 1;
                    continue;
                }
                let got = resolve(&rt2, &p)
                    .unwrap_or_else(|e| panic!("{}: {} did not resolve: {:#}", w, name, e));
                let landed = got == id as u32 || deref(&rt2, got).map(|t| t == id as u32).unwrap_or(false);
                assert!(landed, "{}: {} resolved to {} not {}", w, name, got, id);
                ok += 1;
            }
            eprintln!("[bind] {}: {} paths resolved, {} skipped", w, ok, skipped);
            assert!(ok > 100, "{}: only {} paths checked", w, ok);
        }
    }

    /// The structure the tracer builds must be one the resolver can walk:
    /// every scalar the boundary can name lands on a distinct `Val` cell.
    ///
    /// Distinctness is the real content. A structure that merged two
    /// slots would resolve both paths happily and make the kernel write
    /// one cell twice - a silent corruption, and the only symptom would
    /// be a wrong search result much later.
    #[test]
    fn a_traced_state_becomes_a_structure_every_path_can_walk() {
        use crate::trace::{cart, domain::Symbolic, interp::Interp, verify::run_one};

        let src = cart::sources().expect("sources");
        let top = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let frame = full_moon::parse("_update()").expect("parse frame");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cd = std::sync::Arc::new(
            celeste_core::cart_data::CartData::load("cart").expect("cart"),
        );
        let (rx, ry) = celeste_interp::game_runner::start_room();
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

    /// The guard against writing two outputs into one column, checked by
    /// asking for the same slot twice - which is what a merged structure
    /// looks like from the binder's side.
    #[test]
    fn two_paths_landing_on_one_cell_are_refused() {
        let rt2 = block_from_witness("crates/celeste-kernels/witness/steady-shape.json");
        let p: Path = vec![key("objects"), Step::Idx(0)];
        assert!(resolve_all(&rt2, &[p.clone()]).is_ok(), "one path is fine");
        let e = resolve_all(&rt2, &[p.clone(), p])
            .expect_err("the same cell twice must be refused")
            .to_string();
        assert!(e.contains("merged two slots"), "unexpected error: {}", e);
    }

    /// A path that is not in the shape is a REFUSAL, not a panic and not
    /// a wrong cell. That is what makes binding by path safe: a kernel
    /// handed the wrong shape declines to bind.
    #[test]
    fn a_path_outside_the_shape_is_refused() {
        let rt2 = block_from_witness("crates/celeste-kernels/witness/steady-shape.json");
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
