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
/// `foreach`, ... come from the builtin chunks). FROZEN: these are
/// `GLOBAL_NAMES` entries, so every block carries their cells (`Bi`) and
/// the shape hashes depend on them. `__widen_rem`, `__new_vector` and
/// `__split_at` have no implementation but stay.
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
    "__split_by_round",
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

/// The cart's Lua under settings `s`, read from the checkout.
pub fn sources(s: &crate::settings::Settings) -> Result<String> {
    sources_in(std::path::Path::new("."), s)
}


/// What ONE FRAME is, for the tracer (`trace_frame` runs the button reset
/// itself, before it).
///
/// `_draw()` is NOT cosmetic: the player's screen clamp (`x` to [-1, 121],
/// `spd.x = 0`) lives in the player's `draw`.
pub const FRAME_CODE: &str = "_update()\n_draw()";

/// The cart's Lua under settings `s` (the room, the category's patches, the
/// loading frame, the balloon seeds, the split cart), read relative to `root`.
pub fn sources_in(root: &std::path::Path, s: &crate::settings::Settings) -> Result<String> {
    let read = |p: &str| std::fs::read_to_string(root.join(p));
    let b3 = read("lua/builtin_level_3.lua")?;
    let b4 = read("lua/builtin_level_4.lua")?;
    // `--split-frame`: the split-frame prototype, one frame as two steps.
    let lua = if s.split_frame { "lua/celeste-minimal-split.lua" } else { "lua/celeste-minimal.lua" };
    let mut game = crate::game_runner::apply_start_room(&read(lua)?, s)?;
    anyhow::ensure!(!(s.nodiag && s.onlydiag), "--nodiag and --onlydiag exclude each other");
    static SAID: std::sync::Once = std::sync::Once::new();
    SAID.call_once(|| match s.loading_jank {
        Some(j) => eprintln!("[start] the room as real play enters it: its loading frame from object {j}"),
        None => eprintln!("[start] IL load: no loading frame (no --loading-jank) - not real play where the room's objects move on it"),
    });
    if let Some(j) = s.loading_jank {
        game = apply_loading_frame(&game, j, root, s.room)?;
    }
    if s.nodiag {
        game = forbid_diagonal_dashes(&game)?;
    }
    if s.onlydiag {
        game = forbid_straight_dashes(&game)?;
    }
    // The balloons' phases where `--balloon-seeds` fixes them; else each
    // balloon's `rnd(1)`, an interval at every level (sound for every seed).
    // The float tools always seed (a file without `[seeds]` is every seed 0:
    // UCT's `load_level` and Celia's `init_seed_objs` reset them so): their
    // seeds are the CONCRETE runs' (`Settings::concrete`), a tree without
    // `--balloon-seeds` holds the phase an interval (as fixed under
    // `--concrete-balloon-seeds`), so its states do not count frames.
    if let Some(seeds) = &s.balloon_seeds {
        game = fix_balloon_seeds(&game, seeds, root, s.room)?;
    }
    if let crate::settings::Arith::Float(tool) = s.arith {
        game = tool_seeds(&game, s.balloon_seeds.as_deref(), tool, root, s.room, s.loading_jank.is_some())?;
    }
    Ok(format!("{}\n{}\n{}\n", b3, b4, game))
}

/// `--balloon-seeds "s1,s2,.."`: the start room's balloons' phases
/// FIXED, in creation order, as the TAS tool (UniversalClassicTas) and a
/// tasdatabase file's `[seeds]` fix them - instead of `rnd(1)`, an interval
/// at every level, under which a witness may mix branches no single seed
/// takes. With them the search is exact for those seeds (missing ones: 0).
/// No new global (the name table is frozen): each balloon's seed is inlined,
/// chosen by its position; creation order is `load_room`'s tile loop
/// (x outer, y inner).
fn fix_balloon_seeds(game: &str, seeds: &str, root: &std::path::Path, (rx, ry): (i16, i16)) -> Result<String> {
    const PAT: &str = "this.offset=rnd(1)";
    anyhow::ensure!(game.matches(PAT).count() == 1, "--balloon-seeds: the balloon's `{PAT}` was not found exactly once");
    let seeds: Vec<f64> = seeds.split(',').map(str::trim).filter(|s| !s.is_empty()).map(|s| s.parse::<f64>().with_context(|| format!("--balloon-seeds: {s:?}"))).collect::<Result<_>>()?;
    let cart = celeste_core::cart_data::CartData::load(root.join("cart"))?;
    // The tool's seed list runs over balloons AND chests in creation order
    // (`RoomObject::seeded`); only the balloons' phases are fixed here (a
    // chest's shake decides only where its berry appears).
    let seeded: Vec<RoomObject> = room_objects(&cart, (rx, ry)).into_iter().filter(|o| o.seeded).collect();
    // More seeds than objects: the tool (UniversalClassicTas `set_seeds`)
    // applies one per balloon/chest present and ignores the rest.
    let mut expr = "0".to_string();
    for (i, o) in seeded.iter().enumerate().rev() {
        if o.tile == BALLOON {
            let (x, y, v) = (o.tx * 8, o.ty * 8, seeds.get(i).copied().unwrap_or(0.0));
            expr = format!("((this.x=={x} and this.y=={y}) and {v} or {expr})");
        }
    }
    Ok(game.replacen(PAT, &format!("this.offset={expr}"), 1))
}

/// A float tool's seeds (`--arith float`): what the tool does with a file's
/// `[seeds]` beyond the balloons' phases at their creation
/// (`fix_balloon_seeds`), in its own expressions (plans/float.md,
/// "The semantics and their authority").
/// - A CHEST's last shake: its `rnd(3)` (where its berry appears) returns
///   UCT's `offset + 1` (`TAS.lua`: the chest's update under a patched
///   `rnd` when `timer - 1 <= 0`), Celia's `seed + 1 + 0x0.0001`
///   (`vanilla_seeds.lua`, when `timer <= 1` before the update); its
///   earlier shakes only move it until then, and stay `rnd(3)`.
/// - Celia's BALLOONS: after the load (and its jank) `init_seed_objs` and
///   `set_seed` make each balloon's `offset` its seed (`0 - 0 + seed`) and
///   its `y` `start + sin(offset) * 2`, undoing the loading frame's bob.
///   Only where the balloons are seeded (`seeds`; a tree without seeds
///   holds every phase).
/// Chests take their seeds from `seeds` (every seed 0 without).
fn tool_seeds(game: &str, seeds: Option<&str>, tool: crate::settings::FloatTool, root: &std::path::Path, (rx, ry): (i16, i16), jank: bool) -> Result<String> {
    use crate::settings::FloatTool;
    let texts: Vec<&str> = seeds.unwrap_or("").split(',').map(str::trim).filter(|s| !s.is_empty()).collect();
    // A seed as the tool reads it (`tonumber`), written back exactly.
    let seed = |i: usize| -> Result<String> {
        Ok(match texts.get(i) {
            Some(t) => format!("{}", t.parse::<f64>().with_context(|| format!("--balloon-seeds: {t:?}"))?),
            None => "0".to_string(),
        })
    };
    let cart = celeste_core::cart_data::CartData::load(root.join("cart"))?;
    let seeded: Vec<RoomObject> = room_objects(&cart, (rx, ry)).into_iter().filter(|o| o.seeded).collect();
    let mut game = game.to_string();
    // The chests: by their tile position (`start` is `x - 4`).
    let mut expr = "rnd(3)".to_string();
    for (i, o) in seeded.iter().enumerate().rev() {
        if o.tile == CHEST {
            let last = match tool {
                FloatTool::Uct => format!("({}+1)", seed(i)?),
                // 0x0.0001 is 2^-16: 0.0000152587890625 exactly.
                FloatTool::Celia => format!("({}+1+0.0000152587890625)", seed(i)?),
            };
            let (x, y) = (o.tx * 8 - 4, o.ty * 8);
            expr = format!("((this.timer<=0 and this.start=={x} and this.y=={y}) and {last} or {expr})");
        }
    }
    const SHAKE: &str = "this.x=this.start-1+rnd(3)";
    anyhow::ensure!(game.matches(SHAKE).count() == 1, "the chest's `{SHAKE}` was not found exactly once");
    game = game.replacen(SHAKE, &format!("this.x=this.start-1+{expr}"), 1);
    if tool == FloatTool::Celia && seeds.is_some() {
        let mut offset = "o.offset".to_string();
        for (i, o) in seeded.iter().enumerate().rev() {
            if o.tile == BALLOON {
                offset = format!("((o.x=={} and o.start=={}) and {} or {offset})", o.tx * 8, o.ty * 8, seed(i)?);
            }
        }
        let reseed = format!(" for i=1,#objects do local o=objects[i] if o.type==balloon then o.offset={offset} o.y=o.start+sin(o.offset)*2 end end");
        // After the load: after the loading frame's draw where there is one.
        let call = format!("load_room({rx}, {ry})");
        let at = if jank { "_draw()" } else { call.as_str() };
        let pos = game.find(&call).ok_or_else(|| anyhow::anyhow!("Celia's seeds: the start room's `{call}` was not found"))?;
        let end = pos + game[pos..].find(at).ok_or_else(|| anyhow::anyhow!("Celia's seeds: no `{at}` after the load"))? + at.len();
        game.insert_str(end, &reseed);
    }
    Ok(game)
}

/// The tiles `load_room` makes an object of, in the ORIGINAL cart, with the
/// object's type and whether the minimal cart makes it too: platforms (11,
/// 12) and the types with a `tile`. The minimal cart has no `message` (86,
/// room (3,1)) nor `flag` (118, the summit); `room_title` is the original's
/// last object.
const OBJECT_TILES: &[(u8, &str, bool)] = &[
    (1, "player_spawn", true),
    (8, "key", true),
    (11, "platform", true),
    (12, "platform", true),
    (18, "spring", true),
    (20, "chest", true),
    (22, "balloon", true),
    (23, "fall_floor", true),
    (26, "fruit", true),
    (28, "fly_fruit", true),
    (64, "fake_wall", true),
    (96, "big_chest", true),
    (86, "message", false),
    (118, "flag", false),
];

const BALLOON: u8 = 22;
const CHEST: u8 = 20;

/// One object `load_room` makes in a room, in the ORIGINAL cart: its tile,
/// tile position in the room, type, whether the minimal cart makes it, and
/// whether a seed of the TAS tools' `[seeds]` list is its (balloons and
/// chests, in creation order: UniversalClassicTas `set_seeds`).
#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize)]
pub struct RoomObject {
    pub tile: u8,
    pub tx: i16,
    pub ty: i16,
    pub kind: &'static str,
    pub minimal: bool,
    pub seeded: bool,
}

/// Room `(rx, ry)`'s objects in creation order (`load_room`'s tile loop: x
/// outer, y inner), the original cart's (`room_title`, made last, left
/// out). The one scan of the map for objects: the loading frame's J, the
/// seeds, the tools (`rewrite room`, `rewrite starts`).
pub fn room_objects(cart: &celeste_core::cart_data::CartData, (rx, ry): (i16, i16)) -> Vec<RoomObject> {
    let mut out = Vec::new();
    for tx in 0..16i16 {
        for ty in 0..16i16 {
            let tile = cart.mget_whole(rx * 16 + tx, ry * 16 + ty);
            if let Some(&(_, kind, minimal)) = OBJECT_TILES.iter().find(|(t, _, _)| *t == tile) {
                out.push(RoomObject { tile, tx, ty, kind, minimal, seeded: tile == BALLOON || tile == CHEST });
            }
        }
    }
    out
}

/// The start room's LOADING FRAME as a real transition runs it
/// (`--loading-jank J`): the leaving
/// player's `_update` loads the room, and the frame's `foreach` goes on over
/// the NEW room's objects from the player's index J - one update each
/// (move, then update) - then the frame draws. J counts the ORIGINAL cart's
/// objects (pico8_diff/chain.py reports it); it is translated to the minimal
/// cart's list, which lacks `message` and `flag`. Measured against a real
/// PICO-8 chain field by field (`pico8_diff/chain.py --modes`).
fn apply_loading_frame(game: &str, j: usize, root: &std::path::Path, (rx, ry): (i16, i16)) -> Result<String> {
    let cart = celeste_core::cart_data::CartData::load(root.join("cart"))?;
    let first = minimal_index(&cart, (rx, ry), j);
    let call = format!("load_room({}, {})", rx, ry);
    anyhow::ensure!(game.matches(&call).count() == 1, "--loading-jank: the start room's `{call}` was not found exactly once");
    let frame = format!(
        "{call} for i={first},#objects do local o=objects[i] if o.spd.x ~= 0 or o.spd.y ~= 0 then o.move(o.spd.x,o.spd.y) end if o.type.update~=nil then o.type.update(o) end end _draw()"
    );
    Ok(game.replacen(&call, &frame, 1))
}

/// The minimal cart's index of room `(rx, ry)`'s first object updated by the
/// loading frame, the original's J-th: one past the minimal cart's objects
/// the original creates before it.
fn minimal_index(cart: &celeste_core::cart_data::CartData, room: (i16, i16), j: usize) -> usize {
    1 + room_objects(cart, room).iter().take(j.saturating_sub(1)).filter(|o| o.minimal).count()
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

/// Make the diagonal arm of the player's dash start RAISE (`nil > 0`): a raise
/// has no successor (`Interp::poison`), so a diagonal dash leaves the search
/// exactly, at every level and in the reference engine alike. A dash with no
/// direction held (horizontal, facing direction) stays legal.
fn forbid_diagonal_dashes(game: &str) -> Result<String> {
    const ARM: &str = "if input~=0 then\n\t\t  \tif v_input~=0 then\n";
    anyhow::ensure!(game.matches(ARM).count() == 1, "--nodiag: the cart's diagonal dash arm was not found exactly once");
    Ok(game.replacen(ARM, &format!("{ARM}\t\t   \tlocal nodiag_violation = nil > 0\n"), 1))
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
        anyhow::ensure!(game.matches(arm).count() == 1, "--onlydiag: a dash arm was not found exactly once: {arm:?}");
        let head = &arm[..=arm.find('\n').expect("an arm is two lines")];
        game = game.replacen(arm, &format!("{head}\t\t   \tlocal onlydiag_violation = nil > 0\n{}", &arm[head.len()..]), 1);
    }
    Ok(game)
}

/// Refuse a program that could tell an absent-as-zero field's missing
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
    for (_, field) in celeste_engine::widening::absent_as_zero() {
        let mut c = Count { field, all: 0, safe: 0 };
        c.visit_ast(ast);
        anyhow::ensure!(
            c.all == c.safe,
            "`.{field}` is absent-as-zero (`Stored::AbsentAsZero`), but {} of its {} uses could tell nil from 0 (not an assignment target, arithmetic or an ordering comparison)",
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
        // The cart is read when an engine is built, under its settings.
        let successors = |s: crate::settings::Settings| -> Vec<usize> {
            let mut eng = RefEngine::new(&s).expect("engine");
            let mut row = eng.initial().expect("initial");
            for _ in 0..40 {
                row = eng.step_one(&row, 0).expect("idle frame").into_rt2();
            }
            starts.iter().map(|&b| eng.step(&row, b).expect("dash frame").len()).collect()
        };
        use crate::settings::Settings;
        assert_eq!(successors(Settings::default()), [1, 1, 1, 1], "any%: every dash start");
        assert_eq!(successors(Settings { nodiag: true, ..Default::default() }), [0, 1, 1, 1], "nodiag: all but the diagonal");
        assert_eq!(successors(Settings { onlydiag: true, ..Default::default() }), [1, 0, 0, 0], "onlydiag: the diagonal only");
        assert!(RefEngine::new(&Settings { nodiag: true, onlydiag: true, ..Default::default() }).is_err(), "both modes at once must be refused");
    }
}
