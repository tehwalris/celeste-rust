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
use celeste_engine::slots;
use celeste_names::gen;

use super::iface::{show, Path, Step};

/// The tracer's path as the engine's.
///
/// The two `Step` types are the same three cases; they are separate
/// because the tracer's is part of its own interface and the engine's is
/// what generated code parses out of a string. Converting here is what
/// keeps ONE implementation of the walk - `resolve` below delegates to
/// `celeste_engine::slots`, which is also what a generated kernel calls
/// at bind time. Two walks that had to agree forever would be the same
/// mistake as two numberings that had to agree forever (T17).
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
/// That is a CLAIM about agreeing with code in another crate, so it is
/// checked rather than asserted: `Rt2::canonicalize_ids` is the one
/// implementation of the rule, and
/// `the_structure_a_traced_state_becomes_is_already_canonical` requires
/// it to be the identity on what this builds.
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

    // What each cell still owes. A `Val` cell owes the pointer to
    // whatever its value is; a placeholder object cell owes its body.
    type V = Value<super::domain::Symbolic>;
    enum Todo {
        Val(V),
        Table(u32),
        Done,
    }
    let mut todo: Vec<Todo> = Vec::new();
    let mut table_cell: std::collections::HashMap<u32, u32> = Default::default();

    // Cells are created in DISCOVERY order and never reordered, so
    // walking the array from 0 upwards IS the breadth-first walk: every
    // cell a step discovers is appended past the cursor, and the cursor
    // reaches it later. That is the same rule `Rt2::canonicalize_ids`
    // applies, which is what makes these ids canonical by construction -
    // and `the_structure_a_traced_state_becomes_is_already_canonical`
    // checks it against that one implementation rather than restating it.
    //
    // The order is load-bearing and easy to get subtly wrong: an earlier
    // version created a table's cell INSIDE the slot that points at it,
    // which put the pointee between two globals and shifted every id
    // from the first table-valued global onwards.
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
                // A closure is a heap object with an identity, like a
                // table, and the engine names it by index into
                // `FN_NAMES` - which the tracer resolved when the
                // declaration or assignment that named it was in hand
                // (`Interp::fn_id_of`).
                //
                // An unnamed one is refused rather than numbered 0. The
                // cart's four anonymous functions are `foreach`
                // callbacks that do not outlive their frame, so one
                // reaching a block means something is wrong with the
                // hint, not with the cart.
                let cl = st.heap.closures[&f].clone();
                let id = cl
                    .fn_id
                    .ok_or_else(|| anyhow!("closure {} has no name for FN_NAMES", f))?;
                // The captures are COLUMNS on the closure cell, which the
                // boundary hashes into the row key - so they are part of
                // the encoding whether or not any kernel reads one.
                let mut caps: Vec<Col> = Vec::new();
                for name in &cl.captures {
                    let v = st
                        .heap
                        .lookup(cl.env, name)
                        .ok_or_else(|| anyhow!("capture {:?} is not in scope", name))?;
                    caps.push(Col::U(match v {
                        Value::Table(t) => {
                            // The pointee already has a cell whenever the
                            // capture is the object that owns the closure,
                            // which is every capture in this cart. One
                            // that does not is a cell this walk has not
                            // reached, and inventing it here would put it
                            // at the wrong place in the canonical order.
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
                // IN PLACE, not behind a pointer. The engine names a
                // builtin by index into `BUILTIN_NAMES` - the ABI
                // `compiled::bridge` translates - and the importer puts
                // that cell AT the slot rather than adding an
                // indirection, unlike a table or a closure.
                //
                // Writing a plain `Val` here instead made every builtin
                // structurally identical to every other AND added 16
                // cells, which shifted every id past the first one:
                // 290 cells against the importer's 274, and 151
                // differences produced by one unfinished match arm.
                let i = crate::builtins::BUILTIN_NAMES
                    .iter()
                    .position(|n| *n == name)
                    .ok_or_else(|| anyhow!("unknown builtin {:?}", name))?;
                rt2.structure[c] = Cell2::Bi(i as u32);
            }
            Todo::Val(_) => {}
            Todo::Table(t) => {
                let tab = &st.heap.tables[&t];
                // A table is an object OR an array to the engine, never
                // both. No table in the cart uses both parts, and a table
                // that did could not be described at all - so say so
                // rather than pick.
                if !tab.hash.is_empty() && !tab.arr.is_empty() {
                    bail!("table {} has both a hash and an array part", t);
                }
                // Integer keys past the end of the array: `t[3] = v` on
                // an empty table, which is legal Lua and exactly what
                // room (2,0)'s fruit does
                // (`got_fruit[1 + level_index()]`, level_index() == 2).
                //
                // The INTERPRETER materialises the gap as explicit nils
                // and keeps one dense `ArrayTable`
                // (`core_interpreter`'s index-assignment arm, and
                // `test_interpret_index_assignment_past_the_end`). The
                // tracer's heap models the same table as a sparse
                // `ints` map instead, which is a finer model - and a
                // DIFFERENT one, so a block bound from it would have a
                // structure `import_block` never produces and a shape
                // hash no kernel matches.
                //
                // So flatten here, the interpreter's way. Not a choice
                // between two representations: `bind`'s whole contract
                // is to produce what the importer produces.
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
                    // 1-based and dense. A zero or negative key has no
                    // position in a Lua array part, so there is nothing
                    // to flatten it into - refused rather than guessed.
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
                // An EMPTY table has no kind yet. `got_fruit = {}` is
                // neither an object nor an array until something is put
                // in it, and the interpreter says so with
                // `HeapValue::UnknownTable`. Calling it an object with
                // no fields is the same thing operationally - no path
                // can traverse either - but it is a different cell to
                // the shape hash, which is what makes it worth matching
                // rather than reasoning about.
                let body = if tab.hash.is_empty() && arr.is_empty() {
                    Cell2::Unk
                } else if arr.is_empty() {
                    // Fields the boundary does not name are dropped,
                    // exactly as `import_block` drops them: no generated
                    // code can access one, so it is unreachable weight.
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


/// How a root reaches `target`, as a chain of ops.
///
/// A foreign cell in a frame's expression is always the same question -
/// which output carries it, and through what - and the node id alone
/// answers neither. This walks back from the first root that reaches the
/// node so the failure names a path instead of a number.
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


/// A ONE-LANE block holding a traced state's actual values.
///
/// `structure_of` describes where things go; this also puts them there.
/// It is how a run STARTS: the state after `_init` is fully concrete, and
/// the first block has to hold the values of every scalar - including the
/// ones the tracer froze as program constants, which a kernel never reads
/// but which are part of the row key and therefore part of what a run is
/// compared against.
///
/// Refuses a scalar the domain cannot decide, because a block has no
/// representation for one. The caller wanted a concrete state.
pub fn concrete_block(
    st: &super::state::State<super::domain::Symbolic>,
    d: &super::domain::Symbolic,
    cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
) -> Result<Rt2> {
    use super::domain::Domain;
    use super::heap::Value;

    let mut rt2 = structure_of(st, cart, cache)?;
    for p in super::iface::scalars(st, &[])? {
        let cell = resolve(&rt2, &p)? as usize;
        let v = match super::iface::get(st, &p) {
            Some(Value::Num(n)) => AV::Num(
                d.as_const(&n).ok_or_else(|| anyhow!("{}: not a constant", show(&p)))?,
            ),
            Some(Value::Bool(b)) => AV::Bool(
                d.decide(&b).ok_or_else(|| anyhow!("{}: not a constant", show(&p)))?,
            ),
            other => bail!("{}: {:?} is not a scalar", show(&p), other.is_some()),
        };
        rt2.cols[cell] = Col::U(v);
    }
    Ok(rt2)
}

/// Rebuild a traced graph with the ENGINE's cell ids.
///
/// The tracer numbers its input cells `Op::Cell(0..n)` in `Iface` order,
/// because that is a dense numbering its own evaluator and differential
/// tests index directly. The engine numbers the same slots canonically,
/// sparsely, and not in that order. The emitter reads `Op::Cell(id)` and
/// writes `rin.c{id}`, so the graph handed to it has to already speak
/// the engine's numbering.
///
/// Done HERE and not in `symbolize` on purpose. Numbering the graph
/// canonically from the start would make `Op::Cell` ids sparse, and the
/// tracer's own checking machinery - `trace::eval`'s `Env::cells`, and
/// every perturbation test built on it - indexes them densely. The
/// tracer keeps its dense names; the consumer accepts them and maps
/// once, which is the same choice `resolve` makes about paths.
///
/// `canon` must be INJECTIVE, or two input slots become one cell and the
/// graph silently starts reading one where it meant two. `resolve_all`
/// guarantees that, but this is a public entry point and the check costs
/// one pass, so it is checked rather than assumed.
pub fn renumber_cells(
    g: &crate::transpile::graph::Graph,
    canon: &[u32],
    roots: &[crate::transpile::graph::NodeId],
) -> Result<(crate::transpile::graph::Graph, Vec<crate::transpile::graph::NodeId>)> {
    use crate::transpile::graph::{Graph, NodeId, Op};
    let mut seen: std::collections::HashMap<u32, usize> = Default::default();
    for (i, c) in canon.iter().enumerate() {
        if let Some(j) = seen.insert(*c, i) {
            bail!("input slots {} and {} both map to cell {}", j, i, c);
        }
    }
    // Only what the ROOTS reach. Several frames are traced into one
    // arena so they share subexpressions, and their `Op::Cell` ids are
    // each frame's own dense slot indices - so the arena holds cells
    // this frame's interface does not name, and rebuilding all of it
    // would fail on someone else's.
    let mut live = vec![false; g.len()];
    let mut stack: Vec<NodeId> = roots.to_vec();
    while let Some(n) = stack.pop() {
        if live[n as usize] {
            continue;
        }
        live[n as usize] = true;
        stack.extend(g.get(n).args.iter().copied());
    }

    let mut out = Graph::new();
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
        // A slot holds a VALUE. A path that lands on an object cell
        // names the table itself, and a kernel writing there would
        // overwrite the pointer topology rather than a field.
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
                let landed = got == id as u32 || slots::deref(&rt2, got).map(|t| t == id as u32).unwrap_or(false);
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
    /// `structure_of` claims to emit the engine's numbering directly.
    /// This is that claim, checked against the ONE implementation of the
    /// rule rather than against a second copy of it: canonicalizing the
    /// structure it produces must be the IDENTITY.
    ///
    /// It matters because the two numberings are never compared at
    /// runtime. A kernel writes `cols[resolve(path)]` on a block built
    /// from this structure, and the engine reads it back by its own ids.
    /// If the two disagreed, every cell after the first divergence would
    /// be read as a different field - and the block would still be
    /// well-formed, so nothing downstream would say so.
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

        let rt2 = structure_of(&st, cd.clone(), cache.clone()).expect("structure");
        // Built twice rather than cloned: `Rt2` is deliberately not
        // `Clone` (a block is big and copying one is usually a mistake),
        // and `structure_of` is a pure function of the state.
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
        let rt2 = block_from_witness("test-fixtures/witness/steady-shape.json");
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
