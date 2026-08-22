//! Tracing the actual game.
//!
//! The sources and the chunk layout are the pipeline's
//! (`rewrite::program::Sources`): the builtin Lua, then the cart, then
//! `_init()`. What differs is that nothing is compiled to IR and nothing
//! is rewritten - the interpreter walks the AST.
//!
//! The initial heap is built by the SAME interpreter under the symbolic
//! domain, which folds everything it can. Nothing in `_init()` is unknown,
//! so the whole thing folds to constants and the heap that comes out is
//! concrete - no domain crossing, and it exercises the folding claim on a
//! real program rather than on a unit test.

use anyhow::{anyhow, Result};

use super::domain::Domain;
use super::heap::{Heap, Value};
use super::interp::{Flow, Interp};
use super::state::State;

/// The builtins that are native rather than written in Lua. The Lua-level
/// ones (`add`, `foreach`, ...) come from the builtin chunks and need no
/// help here.
pub const NATIVE: &[&str] = &[
    "__print",
    "__new_unknown_boolean",
    "__widen_rem",
    "__new_vector",
    "__array_table_drop_last",
    "error",
    "min",
    "max",
    "abs",
    "flr",
    "__split_by_flr",
    "__split_at",
    "print",
    "sin",
    "mget",
    "fget",
    "_hint_normalize",
];

/// `tile_flag_at` is defined in the CART as Lua, and has to be replaced
/// AFTER the toplevel runs or the Lua definition overwrites the builtin.
/// The interpreter does the same thing (`inject_tile_flag_at_builtin`) and
/// for the same reason: the Lua version reads the `room` global and scans
/// a tile range, which is both stale-prone and a loop over symbolic
/// bounds once the coordinates stop being constants.
pub fn inject_tile_flag_at<D: Domain>(st: &mut State<D>) {
    let g = st.globals;
    st.heap
        .tables
        .get_mut(&g)
        .unwrap()
        .hash
        .insert("tile_flag_at".to_string(), Value::Builtin("tile_flag_at"));
}

pub fn sources() -> Result<String> {
    let b3 = std::fs::read_to_string("lua/builtin_level_3.lua")?;
    let b4 = std::fs::read_to_string("lua/builtin_level_4.lua")?;
    let game = celeste_interp::game_runner::apply_start_room(&std::fs::read_to_string(
        "lua/celeste-minimal.lua",
    )?)?;
    Ok(format!("{}\n{}\n{}\n", b3, b4, game))
}

pub fn fresh_state<D: Domain>(d: &mut D) -> State<D> {
    let mut heap: Heap<D> = Heap::default();
    let globals = heap.new_table();
    let scope = heap.new_scope(None);
    let t = d.boolean(true);
    let mut st = State {
        heap,
        globals,
        scope,
        stack: Vec::new(),
        guard: t.clone(),
        ok: t,
    };
    for name in NATIVE {
        st.heap
            .tables
            .get_mut(&globals)
            .unwrap()
            .hash
            .insert((*name).to_string(), Value::Builtin(name));
    }
    st
}

/// Run a chunk to completion, requiring it to end in exactly one state.
pub fn run_chunk<'a, D: Domain>(
    it: &mut Interp<'a, D>,
    ast: &'a full_moon::ast::Ast,
    st: State<D>,
) -> Result<State<D>> {
    let out = it.exec_block(ast.nodes(), st)?;
    if out.len() != 1 {
        return Err(anyhow!("chunk ended in {} states", out.len()));
    }
    let (s, f) = out.into_iter().next().unwrap();
    match f {
        Flow::Normal | Flow::Return(_) => Ok(s),
        Flow::Break => Err(anyhow!("break at chunk toplevel")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::trace::domain::Symbolic;

    /// How far does the tracer get on the real cart? This is a PROBE, not
    /// a gate: it prints where it stopped, and each stop is the next thing
    /// to implement. It asserts only that the sources parse, so that the
    /// interesting output is the message rather than a panic.
    #[test]
    fn trace_the_cart_toplevel() {
        let src = sources().expect("sources");
        let ast = full_moon::parse(&src).expect("parse");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let st = fresh_state::<Symbolic>(&mut it.d);
        match run_chunk(&mut it, &ast, st) {
            Ok(s) => {
                let g = &s.heap.tables[&s.globals];
                eprintln!(
                    "[trace] toplevel OK: {} globals, {} tables, {} nodes",
                    g.hash.len(),
                    s.heap.tables.len(),
                    it.d.graph.len()
                );
            }
            Err(e) => eprintln!("[trace] stopped at: {:#}", e),
        }
    }

    /// The next step up: run `_init()`, which is where the cart actually
    /// builds the room. Same probe discipline - it reports rather than
    /// gates, because each stop names the next thing to implement.
    #[test]
    fn trace_the_cart_init() {
        let src = sources().expect("sources");
        let ast = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()").expect("parse _init");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        let cart = std::sync::Arc::new(
            celeste_core::cart_data::CartData::load("cart").expect("cart"),
        );
        let (rx, ry) = celeste_interp::game_runner::start_room();
        it.cache = Some(std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cart, rx, ry).expect("cache"),
        ));
        it.cart = Some(cart);
        let st = fresh_state::<Symbolic>(&mut it.d);
        let mut st = match run_chunk(&mut it, &ast, st) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("[trace] toplevel stopped at: {:#}", e);
                return;
            }
        };
        inject_tile_flag_at(&mut st);
        match run_chunk(&mut it, &init, st) {
            Ok(s) => {
                let objs = s.heap.tables[&s.globals].hash.get("objects").cloned();
                let n = match objs {
                    Some(Value::Table(t)) => s.heap.tables[&t].arr.len(),
                    _ => 0,
                };
                eprintln!(
                    "[trace] _init OK: {} objects, {} tables, {} nodes",
                    n,
                    s.heap.tables.len(),
                    it.d.graph.len()
                );
            }
            Err(e) => eprintln!("[trace] _init stopped at: {:#}", e),
        }
    }

    /// One FRAME. This is the real target: `__reset_button_states` makes
    /// the six free choices unknown, so `_update` genuinely branches on
    /// something the tracer cannot decide, and every such branch has to
    /// become a select or a second output state.
    #[test]
    fn trace_one_frame() {
        let src = sources().expect("sources");
        let ast = full_moon::parse(&src).expect("parse");
        let init = full_moon::parse("_init()\n__reset_button_states()\n").expect("parse init");
        let frame = full_moon::parse("_update()").expect("parse frame");
        let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
        // A tighter budget than the default. Six free choices held across
        // forty frames diverge about 3x per frame once the player can
        // die, so the tail of this probe is a state explosion that costs
        // minutes and says nothing a kernel needs - one frame is what
        // gets compiled. Stopping at 100k nodes keeps it in seconds while
        // still reaching the frames where shapes first diverge.
        it.max_nodes = 100_000;
        let cart = std::sync::Arc::new(
            celeste_core::cart_data::CartData::load("cart").expect("cart"),
        );
        let (rx, ry) = celeste_interp::game_runner::start_room();
        it.cache = Some(std::sync::Arc::new(
            celeste_core::collision_cache::CollisionCache::new(&cart, rx, ry).expect("cache"),
        ));
        it.cart = Some(cart);
        let st = fresh_state::<Symbolic>(&mut it.d);
        let mut st = match run_chunk(&mut it, &ast, st) {
            Ok(s) => s,
            Err(e) => return eprintln!("[trace] toplevel stopped at: {:#}", e),
        };
        inject_tile_flag_at(&mut st);
        let st = match run_chunk(&mut it, &init, st) {
            Ok(s) => s,
            Err(e) => return eprintln!("[trace] _init stopped at: {:#}", e),
        };
        // Why did the frame do nothing? `_update` returns early while
        // `freeze > 0`, so check the globals the early exits read before
        // blaming the tracer.
        for k in ["freeze", "frames", "will_restart", "delay_restart"] {
            let v = st.heap.tables[&st.globals].hash.get(k);
            let d = match v {
                Some(Value::Num(n)) => match it.d.as_const(n) {
                    Some(c) => format!("{:?}", c),
                    None => format!("<node {}>", n),
                },
                other => format!("{:?}", other),
            };
            eprintln!("[trace]   global {} = {}", k, d);
        }
        match st.heap.tables[&st.globals].hash.get("__button_states") {
            Some(Value::Table(t)) => {
                let b = &st.heap.tables[t];
                eprintln!(
                    "[trace]   __button_states: {} entries, first = {:?}",
                    b.arr.len(),
                    b.arr.first()
                );
            }
            other => eprintln!("[trace]   __button_states = {:?}", other),
        }
        eprintln!("[trace] after _init: {} nodes", it.d.graph.len());
        // What the player's position IS after the frame says whether the
        // frame did real work: a constant means nothing symbolic reached
        // it, which for a frame with six unknown buttons would be wrong.
        let describe = |it: &Interp<Symbolic>, s: &crate::trace::state::State<Symbolic>| -> String {
            let Some(Value::Table(objs)) = s.heap.tables[&s.globals].hash.get("objects").cloned()
            else {
                return "no objects".into();
            };
            let Some(Value::Table(p)) = s.heap.tables[&objs].arr.first().cloned() else {
                return "no player".into();
            };
            let f = |k: &str| match s.heap.tables[&p].hash.get(k) {
                Some(Value::Num(n)) => match it.d.as_const(n) {
                    Some(c) => format!("{}={:?}", k, c),
                    None => format!("{}=<node {}>", k, n),
                },
                other => format!("{}={:?}", k, other),
            };
            let sub = |k: &str, sk: &str| -> String {
                match s.heap.tables[&p].hash.get(k) {
                    Some(Value::Table(t)) => match s.heap.tables[t].hash.get(sk) {
                        Some(Value::Num(n)) => match it.d.as_const(n) {
                            Some(c) => format!("{}.{}={:?}", k, sk, c),
                            None => format!("{}.{}=<node {}>", k, sk, n),
                        },
                        other => format!("{}.{}={:?}", k, sk, other),
                    },
                    other => format!("{}={:?}", k, other),
                }
            };
            format!(
                "{}, {}, {}, {}, keys={:?}",
                f("x"),
                f("y"),
                sub("spd", "x"),
                sub("spd", "y"),
                s.heap.tables[&p].hash.keys().collect::<Vec<_>>()
            )
        };
        // Frame 1 is the SPAWN ANIMATION, which reads no buttons - so a
        // frame that propagates nothing symbolic is correct there and
        // proves little. Keep going until the player exists and the
        // buttons actually reach something.
        let mut cur = vec![st];
        for n in 1..=40 {
            let mut next: crate::trace::interp::Outcome<Symbolic> = Vec::new();
            let mut failed = None;
            for s in cur {
                match it.exec_block(frame.nodes(), s) {
                    Ok(out) => next.extend(out),
                    Err(e) => {
                        failed = Some(format!("{:#}", e));
                        break;
                    }
                }
            }
            if let Some(e) = failed {
                eprintln!("[trace] frame {} stopped at: {}", n, e);
                return;
            }
            // COLLAPSE ACROSS THE WHOLE FRONTIER. `exec_block` collapses
            // what one state produced, but each state here is a separate
            // call, so without this two states from different predecessors
            // are never offered to each other - and after a frame or two
            // of divergence that is most of the pairs.
            let next = match it.collapse(next) {
                Ok(o) => o.into_iter().map(|(s, _)| s).collect::<Vec<_>>(),
                Err(e) => {
                    eprintln!("[trace] frame {} collapse stopped at: {:#}", n, e);
                    return;
                }
            };
            eprintln!(
                "[trace] frame {}: {} state(s), {} nodes | {}",
                n,
                next.len(),
                it.d.graph.len(),
                describe(&it, &next[0])
            );
            // WHY are there several? Only a SHAPE difference can keep
            // two states apart, so if the distinct-shape count is below
            // the state count, `collapse` is failing to merge and that is
            // a bug rather than the program branching. Printed every
            // frame, not behind an env var: this is the check that caught
            // `intern_body` handing out a fresh id per evaluation, and it
            // caught it only because someone looked.
            if next.len() > 1 {
                let shapes: std::collections::BTreeSet<String> =
                    next.iter().map(|s| format!("{:?}", s.shape().unwrap())).collect();
                assert_eq!(
                    shapes.len(),
                    next.len(),
                    "frame {}: {} states share only {} shapes - they should have merged",
                    n,
                    next.len(),
                    shapes.len()
                );
            }
            cur = next;
        }
    }
}
