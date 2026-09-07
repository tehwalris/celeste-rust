//! Tracing the actual game.
//!
//! The sources and the chunk layout are the pipeline's
//! (`program::Sources`): the builtin Lua, then the cart, then
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
    "printh",
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
    sources_in(std::path::Path::new("."))
}


/// What ONE FRAME is, for the tracer.
///
/// The same chunk `program::FRAME_CODE` gives the interpreter,
/// minus the button reset - `trace_frame` runs that itself, at the same
/// boundary, before the frame rather than after.
///
/// `_draw()` is NOT cosmetic and leaving it out was a real bug. The
/// player's screen clamp lives there:
///
/// ```lua
/// draw=function(this)
///   if this.x<-1 or this.x>121 then
///     this.x=clamp(this.x,-1,121)
///     this.spd.x=0
///   end
/// end
/// ```
///
/// so a lane that walks off the left edge is stopped by `_draw`, not by
/// `_update`. Tracing `_update()` alone let four lanes drift to x=-2 and
/// x=-3 at frame 28 of room (1,0), which the widened `rem` then doubled
/// into eight rows the interpreter did not have. The oracle test could
/// not catch it: both sides ran the same chunk, so both were wrong the
/// same way.
pub const FRAME_CODE: &str = "_update()\n_draw()";

/// The cart's Lua, read relative to `root`.
///
/// The paths used to be relative to the process's working directory,
/// which is the repo root for every test in the workspace. The traced
/// kernel's run check is a crate OUTSIDE the workspace - deliberately,
/// see its Cargo.toml - so its working directory is not the repo root.
pub fn sources_in(root: &std::path::Path) -> Result<String> {
    let read = |p: &str| std::fs::read_to_string(root.join(p));
    let b3 = read("lua/builtin_level_3.lua")?;
    let b4 = read("lua/builtin_level_4.lua")?;
    let game =
        celeste_interp::game_runner::apply_start_room(&read("lua/celeste-minimal.lua")?)?;
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
        path: Vec::new(),
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
