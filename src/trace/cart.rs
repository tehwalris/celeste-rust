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
    "rnd",
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
    // `CELESTE_SPLIT_FRAME`: the split-frame prototype, one frame as two steps
    // (lua/celeste-minimal-split.lua, plans/room60-overnight-2026-09-28.md).
    let lua = if std::env::var_os("CELESTE_SPLIT_FRAME").is_some() { "lua/celeste-minimal-split.lua" } else { "lua/celeste-minimal.lua" };
    let game = celeste_interp::game_runner::apply_start_room(&read(lua)?)?;
    Ok(format!("{}\n{}\n{}\n", b3, b4, game))
}

/// Refuse a program that could tell an `ABSENT_AS_ZERO` field's missing
/// value (nil) from the 0 every frame writes there
/// (`widen::materialize_absent_fields`). Every `.field` in the program must
/// be an assignment target or a DIRECT operand of arithmetic (`+ - * / % ^`)
/// or of an ordering comparison (`< <= > >=`) - on nil each of those is a
/// runtime error, so a path reading the missing value halts the real game.
/// Anything else (`==`, `and`/`or`, `not`, a call argument, a local,
/// parentheses, a further index) refuses. Counted rather than walked with
/// parents: all `.field` indexes must equal the targets plus such operands.
/// A bracketed `t["field"]` is refused at trace time (`Interp::index_key`).
pub fn check_absent_fields(ast: &full_moon::ast::Ast) -> Result<()> {
    use full_moon::ast;
    use full_moon::visitors::Visitor;
    fn names_field(e: &ast::VarExpression, field: &str) -> bool {
        matches!(e.suffixes().last(), Some(ast::Suffix::Index(ast::Index::Dot { name, .. })) if name.token().to_string().trim() == field)
    }
    struct Count {
        field: &'static str,
        all: usize,
        safe: usize,
    }
    impl Visitor for Count {
        fn visit_index(&mut self, node: &ast::Index) {
            if let ast::Index::Dot { name, .. } = node {
                if name.token().to_string().trim() == self.field {
                    self.all += 1;
                }
            }
        }
        fn visit_expression(&mut self, node: &ast::Expression) {
            if let ast::Expression::BinaryOperator { lhs, binop, rhs } = node {
                use ast::BinOp;
                let crashes_on_nil = matches!(
                    binop,
                    BinOp::Plus(_)
                        | BinOp::Minus(_)
                        | BinOp::Star(_)
                        | BinOp::Slash(_)
                        | BinOp::Percent(_)
                        | BinOp::Caret(_)
                        | BinOp::LessThan(_)
                        | BinOp::LessThanEqual(_)
                        | BinOp::GreaterThan(_)
                        | BinOp::GreaterThanEqual(_)
                );
                if crashes_on_nil {
                    for side in [lhs, rhs] {
                        if let ast::Expression::Var(ast::Var::Expression(ve)) = &**side {
                            if names_field(ve, self.field) {
                                self.safe += 1;
                            }
                        }
                    }
                }
            }
        }
        fn visit_assignment(&mut self, node: &ast::Assignment) {
            for v in node.variables().iter() {
                if let ast::Var::Expression(ve) = v {
                    if names_field(ve, self.field) {
                        self.safe += 1;
                    }
                }
            }
        }
    }
    for (_, field) in super::widen::ABSENT_AS_ZERO {
        let mut c = Count { field, all: 0, safe: 0 };
        c.visit_ast(ast);
        anyhow::ensure!(
            c.all == c.safe,
            "`.{field}` is absent-as-zero (widen::ABSENT_AS_ZERO), but {} of its {} uses could tell nil from 0 (not an assignment target, arithmetic or an ordering comparison)",
            c.all - c.safe,
            c.all
        );
    }
    Ok(())
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
        guard: t,
        ended: d.boolean(false),
        path: Vec::new(),
        frag: Vec::new(),
        arc: None,
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
