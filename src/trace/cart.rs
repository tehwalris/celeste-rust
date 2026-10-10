//! The game's sources and chunks for the tracer: the builtin Lua, then the
//! cart, then `_init()`.
//!
//! The initial heap is built by the same interpreter under the symbolic
//! domain; nothing in `_init()` is unknown, so it folds to a concrete heap.

use anyhow::{anyhow, Context, Result};

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
    anyhow::ensure!(!(nodiag() && onlydiag()), "CELESTE_NODIAG and CELESTE_ONLYDIAG exclude each other");
    if let Some(j) = crate::game_runner::loading_jank() {
        game = apply_loading_frame(&game, j, root)?;
    }
    if nodiag() {
        game = forbid_diagonal_dashes(&game)?;
    }
    if onlydiag() {
        game = forbid_straight_dashes(&game)?;
    }
    if let Ok(seeds) = std::env::var("CELESTE_BALLOON_SEEDS") {
        game = fix_balloon_seeds(&game, &seeds, root)?;
    }
    Ok(format!("{}\n{}\n{}\n", b3, b4, game))
}

/// `CELESTE_BALLOON_SEEDS="s1,s2,.."`: the start room's balloons' phases
/// FIXED, in creation order, as the TAS tool (UniversalClassicTas) and a
/// tasdatabase file's `[seeds]` fix them - instead of `rnd(1)`, an interval
/// at every level, under which a witness may mix branches no single seed
/// takes. With them the search is exact for those seeds (missing ones: 0).
/// No new global (the name table is frozen): each balloon's seed is inlined,
/// chosen by its position; creation order is `load_room`'s tile loop
/// (x outer, y inner).
fn fix_balloon_seeds(game: &str, seeds: &str, root: &std::path::Path) -> Result<String> {
    const PAT: &str = "this.offset=rnd(1)";
    anyhow::ensure!(game.matches(PAT).count() == 1, "CELESTE_BALLOON_SEEDS: the balloon's `{PAT}` was not found exactly once");
    let seeds: Vec<f64> = seeds.split(',').map(str::trim).filter(|s| !s.is_empty()).map(|s| s.parse::<f64>().with_context(|| format!("CELESTE_BALLOON_SEEDS: {s:?}"))).collect::<Result<_>>()?;
    let cart = celeste_core::cart_data::CartData::load(root.join("cart"))?;
    let (rx, ry) = crate::game_runner::start_room();
    // The tool's seed list runs over balloons (tile 22) AND chests (tile 20)
    // in creation order; only the balloons' phases are fixed here (a chest's
    // shake decides only where its berry appears).
    let mut seeded = Vec::new();
    for tx in 0..16i16 {
        for ty in 0..16i16 {
            let tile = cart.mget_whole(rx * 16 + tx, ry * 16 + ty);
            if tile == 22 || tile == 20 {
                seeded.push((tile, tx * 8, ty * 8));
            }
        }
    }
    // More seeds than objects: the tool (UniversalClassicTas `set_seeds`)
    // applies one per balloon/chest present and ignores the rest.
    let mut expr = "0".to_string();
    for (i, &(tile, x, y)) in seeded.iter().enumerate().rev() {
        if tile == 22 {
            let v = seeds.get(i).copied().unwrap_or(0.0);
            expr = format!("((this.x=={x} and this.y=={y}) and {v} or {expr})");
        }
    }
    Ok(game.replacen(PAT, &format!("this.offset={expr}"), 1))
}

/// The tiles `load_room` makes an object of, in the ORIGINAL cart, and
/// whether the minimal cart makes it too: platforms (11, 12) and the types
/// with a `tile`. The minimal cart has no `message` (86, room (3,1)) nor
/// `flag` (118, the summit); `room_title` is the original's last object.
const OBJECT_TILES: &[(u8, bool)] = &[
    (1, true),
    (8, true),
    (11, true),
    (12, true),
    (18, true),
    (20, true),
    (22, true),
    (23, true),
    (26, true),
    (28, true),
    (64, true),
    (96, true),
    (86, false),
    (118, false),
];

/// The start room's LOADING FRAME as a real transition runs it
/// (`CELESTE_LOADING_JANK=J`, `game_runner::loading_jank`): the leaving
/// player's `_update` loads the room, and the frame's `foreach` goes on over
/// the NEW room's objects from the player's index J - one update each
/// (move, then update) - then the frame draws. J counts the ORIGINAL cart's
/// objects (pico8_diff/chain.py reports it); it is translated to the minimal
/// cart's list, which lacks `message` and `flag`. Measured against a real
/// PICO-8 chain field by field (`pico8_diff/chain.py --modes`).
fn apply_loading_frame(game: &str, j: usize, root: &std::path::Path) -> Result<String> {
    let cart = celeste_core::cart_data::CartData::load(root.join("cart"))?;
    let (rx, ry) = crate::game_runner::start_room();
    let first = minimal_index(&cart, (rx, ry), j);
    let call = format!("load_room({}, {})", rx, ry);
    anyhow::ensure!(game.matches(&call).count() == 1, "CELESTE_LOADING_JANK: the start room's `{call}` was not found exactly once");
    let frame = format!(
        "{call} for i={first},#objects do local o=objects[i] if o.spd.x ~= 0 or o.spd.y ~= 0 then o.move(o.spd.x,o.spd.y) end if o.type.update~=nil then o.type.update(o) end end _draw()"
    );
    Ok(game.replacen(&call, &frame, 1))
}

/// The minimal cart's index of room `(rx, ry)`'s first object updated by the
/// loading frame, the original's J-th: one past the minimal cart's objects
/// the original creates before it.
fn minimal_index(cart: &celeste_core::cart_data::CartData, (rx, ry): (i16, i16), j: usize) -> usize {
    let mut original = 0;
    let mut before = 0;
    for tx in 0..16i16 {
        for ty in 0..16i16 {
            let tile = cart.mget_whole(rx * 16 + tx, ry * 16 + ty);
            if let Some(&(_, minimal)) = OBJECT_TILES.iter().find(|(t, _)| *t == tile) {
                original += 1;
                if original < j && minimal {
                    before += 1;
                }
            }
        }
    }
    before + 1
}

#[cfg(test)]
mod loading_frame_tests {
    /// Room (3,1) is the spawn, the `message` (original cart only), then the
    /// fly fruit: a loading frame from the original's 3rd object (the fly
    /// fruit) starts at the minimal cart's 2nd; one from the message, too.
    /// Room (6,2) (spawn, fly fruit) needs no translation.
    #[test]
    fn the_original_cart_index_maps_past_the_missing_message() {
        let cart = celeste_core::cart_data::CartData::load(std::path::Path::new("cart")).unwrap();
        assert_eq!(super::minimal_index(&cart, (3, 1), 3), 2);
        assert_eq!(super::minimal_index(&cart, (3, 1), 2), 2);
        assert_eq!(super::minimal_index(&cart, (3, 1), 1), 1);
        assert_eq!(super::minimal_index(&cart, (6, 2), 2), 2);
    }
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

/// `CELESTE_ONLYDIAG`: every dash diagonal, the inverse of `CELESTE_NODIAG`.
/// A dash may only START with both a horizontal and a vertical direction
/// held; a dash with no direction held (horizontal, facing direction) is not
/// diagonal.
pub fn onlydiag() -> bool {
    std::env::var_os("CELESTE_ONLYDIAG").is_some()
}

/// Make the three non-diagonal arms of the player's dash start (horizontal
/// only, vertical only, no direction) RAISE, as `forbid_diagonal_dashes`
/// does the diagonal one: exact at every level and in the reference engine.
fn forbid_straight_dashes(game: &str) -> Result<String> {
    const ARMS: [&str; 3] = [
        "\t\t  \telse\n\t\t   \tthis.spd.x=input*d_full\n",
        "\t\t \telseif v_input~=0 then\n\t\t \t\tthis.spd.x=0\n",
        "\t\t \telse\n\t\t \t\tthis.spd.x=(this.flip.x and -1 or 1)\n",
    ];
    let mut game = game.to_string();
    for arm in ARMS {
        anyhow::ensure!(game.matches(arm).count() == 1, "CELESTE_ONLYDIAG: a dash arm was not found exactly once: {arm:?}");
        let head = &arm[..=arm.find('\n').expect("an arm is two lines")];
        game = game.replacen(arm, &format!("{head}\t\t   \tlocal onlydiag_violation = nil > 0\n{}", &arm[head.len()..]), 1);
    }
    Ok(game)
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

#[cfg(test)]
mod tests {
    use crate::trace::refengine::RefEngine;

    /// The dash starts each category allows, through the reference engine
    /// (the kernels trace the same patched source): room (1,0), the player
    /// standing after 40 idle frames, then a dash with up+right, right, up
    /// or no direction held. A forbidden start raises: no successor.
    #[test]
    fn nodiag_and_onlydiag_forbid_exactly_their_dashes() {
        const DASH: u8 = 32;
        const RIGHT: u8 = 2;
        const UP: u8 = 4;
        let starts = [DASH | RIGHT | UP, DASH | RIGHT, DASH | UP, DASH];
        // The cart is read when an engine is built (nextest: own process).
        let successors = |mode: Option<&str>| -> Vec<usize> {
            for m in ["CELESTE_NODIAG", "CELESTE_ONLYDIAG"] {
                std::env::remove_var(m);
            }
            if let Some(m) = mode {
                std::env::set_var(m, "1");
            }
            let mut eng = RefEngine::new().expect("engine");
            let mut row = eng.initial().expect("initial");
            for _ in 0..40 {
                row = eng.step_one(&row, 0).expect("idle frame").into_rt2();
            }
            starts.iter().map(|&b| eng.step(&row, b).expect("dash frame").len()).collect()
        };
        assert_eq!(successors(None), [1, 1, 1, 1], "any%: every dash start");
        assert_eq!(successors(Some("CELESTE_NODIAG")), [0, 1, 1, 1], "nodiag: all but the diagonal");
        assert_eq!(successors(Some("CELESTE_ONLYDIAG")), [1, 0, 0, 0], "onlydiag: the diagonal only");
        std::env::set_var("CELESTE_NODIAG", "1");
        assert!(RefEngine::new().is_err(), "both modes at once must be refused");
    }
}
