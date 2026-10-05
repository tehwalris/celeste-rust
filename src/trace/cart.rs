//! The game's sources and chunks for the tracer: the builtin Lua, then the
//! cart, then `_init()`.
//!
//! The initial heap is built by the same interpreter under the symbolic
//! domain; nothing in `_init()` is unknown, so it folds to a concrete heap.

use anyhow::{anyhow, Result};

use super::domain::Domain;
use super::heap::{Heap, Value};
use super::interp::{Flow, Interp};
use super::state::State;

/// The builtins that are native rather than written in Lua (`add`,
/// `foreach`, ... come from the builtin chunks).
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

/// Replace the cart's Lua `tile_flag_at` with the builtin. Must run AFTER the
/// toplevel, or the Lua definition overwrites it: the Lua version scans a
/// tile range, a loop over symbolic bounds once the coordinates are unknown.
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


/// What ONE FRAME is, for the tracer (`trace_frame` runs the button reset
/// itself, before it).
///
/// `_draw()` is NOT cosmetic: the player's screen clamp (`x` to [-1, 121],
/// `spd.x = 0`) lives in the player's `draw`.
pub const FRAME_CODE: &str = "_update()\n_draw()";

/// The cart's Lua, read relative to `root`.
pub fn sources_in(root: &std::path::Path) -> Result<String> {
    let read = |p: &str| std::fs::read_to_string(root.join(p));
    let b3 = read("lua/builtin_level_3.lua")?;
    let b4 = read("lua/builtin_level_4.lua")?;
    // `CELESTE_SPLIT_FRAME`: the split-frame prototype, one frame as two steps.
    let lua = if std::env::var_os("CELESTE_SPLIT_FRAME").is_some() { "lua/celeste-minimal-split.lua" } else { "lua/celeste-minimal.lua" };
    let mut game = crate::game_runner::apply_start_room(&read(lua)?)?;
    if nodiag() {
        game = forbid_diagonal_dashes(&game)?;
    }
    Ok(format!("{}\n{}\n{}\n", b3, b4, game))
}

/// `CELESTE_NODIAG`: the No Diagonal Dashes category. A dash may not START
/// with both a horizontal and a vertical direction held.
pub fn nodiag() -> bool {
    std::env::var_os("CELESTE_NODIAG").is_some()
}

/// Make the diagonal arm of the player's dash start RAISE (`nil > 0`): a raise
/// has no successor (`Interp::poison`), so a diagonal dash leaves the search
/// exactly, at every level and in the reference engine alike. A dash with no
/// direction held (horizontal, facing direction) stays legal.
fn forbid_diagonal_dashes(game: &str) -> Result<String> {
    const ARM: &str = "if input~=0 then\n\t\t  \tif v_input~=0 then\n";
    anyhow::ensure!(game.matches(ARM).count() == 1, "CELESTE_NODIAG: the cart's diagonal dash arm was not found exactly once");
    Ok(game.replacen(ARM, &format!("{ARM}\t\t   \tlocal nodiag_violation = nil > 0\n"), 1))
}

/// Refuse a program that could tell an `ABSENT_AS_ZERO` field's missing
/// value (nil) from the 0 every frame writes there
/// (`widen::materialize_absent_fields`). Every `.field` must be an assignment
/// target or a DIRECT operand of arithmetic or an ordering comparison: on nil
/// those are runtime errors, so reading the missing value halts the real
/// game. Checked by counting: all `.field` indexes must equal the targets
/// plus such operands. A bracketed `t["field"]` is refused at trace time
/// (`Interp::index_key`).
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
