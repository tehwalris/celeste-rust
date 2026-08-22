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
    "tile_flag_at",
    "_hint_normalize",
];

pub fn sources() -> Result<String> {
    let b3 = std::fs::read_to_string("lua/builtin_level_3.lua")?;
    let b4 = std::fs::read_to_string("lua/builtin_level_4.lua")?;
    let game = celeste_interp::game_runner::apply_start_room(&std::fs::read_to_string(
        "lua/celeste-minimal.lua",
    )?)?;
    Ok(format!("{}\n{}\n{}\n", b3, b4, game))
}

pub fn fresh_state<D: Domain>(_d: &mut D) -> State<D> {
    let mut heap: Heap<D> = Heap::default();
    let globals = heap.new_table();
    let scope = heap.new_scope(None);
    let mut st = State { heap, globals, scope, stack: Vec::new(), path: Vec::new() };
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
        it.cart = Some(std::sync::Arc::new(
            celeste_core::cart_data::CartData::load("cart").expect("cart"),
        ));
        let st = fresh_state::<Symbolic>(&mut it.d);
        let st = match run_chunk(&mut it, &ast, st) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("[trace] toplevel stopped at: {:#}", e);
                return;
            }
        };
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
        it.cart = Some(std::sync::Arc::new(
            celeste_core::cart_data::CartData::load("cart").expect("cart"),
        ));
        let st = fresh_state::<Symbolic>(&mut it.d);
        let st = match run_chunk(&mut it, &ast, st) {
            Ok(s) => s,
            Err(e) => return eprintln!("[trace] toplevel stopped at: {:#}", e),
        };
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
            let mut next = Vec::new();
            let mut failed = None;
            for s in cur {
                match it.exec_block(frame.nodes(), s) {
                    Ok(out) => next.extend(out.into_iter().map(|(s, _)| s)),
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
            eprintln!(
                "[trace] frame {}: {} state(s), {} nodes | {}",
                n,
                next.len(),
                it.d.graph.len(),
                describe(&it, &next[0])
            );
            cur = next;
        }
    }
}
