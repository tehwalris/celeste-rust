//! A REAL PICO-8, headless (`pico8 -x`): the definitive check of a witness.
//! The cart is built here from the game's Lua plus `cart/`'s map and flags,
//! driven frame by frame (`btn` reads the scripted inputs), and its
//! `@P8@` lines are the result. Frame f is the f-th `_update();_draw()`
//! after `_init()`, and plays `inputs[f-1]`, as `rewrite run` counts.
//!
//! WHICH cart is explicit (`Cart`): the traced program, or the game as
//! released. Where this cart deliberately differs from the tracer's
//! (`trace::cart`), the difference is a named variant (`Fenced`), its
//! default what this replay has always done: plans/correctness.md C4 (the
//! loading frame's move) and C11 (the order seeds are handed out).

use anyhow::{bail, ensure, Context, Result};
use std::path::{Path, PathBuf};

/// The cart PICO-8 runs.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cart {
    /// The traced program (`lua/celeste-minimal.lua`, in the binary): its
    /// `_init` loads room (1,0), patched to the start room. It lacks the
    /// original's `message` and `flag` objects: a loading frame's J (the
    /// ORIGINAL cart's index) is translated.
    Minimal,
    /// The game as released (celeste_ocaml's `celeste.lua`): its `_init`
    /// shows the title, and `begin_game()` (what the first button press
    /// does) loads room (0,0), patched to the start room.
    Original(PathBuf),
}

/// The original cart's usual place.
pub fn original_cart() -> PathBuf {
    PathBuf::from(format!("{}/src/github.com/tehwalris/celeste_ocaml/celeste.lua", std::env::var("HOME").unwrap_or_default()))
}

impl Cart {
    /// Its Lua, and the call that loads the start room (patched).
    fn lua(&self) -> Result<(String, &'static str)> {
        Ok(match self {
            Cart::Minimal => (crate::trace::cart::MINIMAL_CART.to_string(), "load_room(1, 0)"),
            Cart::Original(p) => (std::fs::read_to_string(p).with_context(|| format!("the original cart {}", p.display()))?, "load_room(0,0)"),
        })
    }
}

/// C4 (plans/correctness.md, FENCED): how a loading frame moves an object.
#[derive(Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
pub enum LoadingMove {
    /// `o.move(o.spd.x, o.spd.y)` always, as the original cart's `_update`
    /// does (celeste.lua:1195): this replay's, always.
    Unguarded,
    /// Only with a nonzero speed, as the traced cart's loading frame
    /// (`trace::cart::apply_loading_frame`) and every frame of the minimal
    /// cart.
    Guarded,
}

/// C11 (plans/correctness.md, FENCED): which object a `[seeds]` entry is.
#[derive(Clone, Copy, Debug, PartialEq, Eq, clap::ValueEnum)]
pub enum SeedOrder {
    /// Each balloon's and chest's `init` takes the next seed as it runs (a
    /// chain: per room, from its first at every load): this replay's,
    /// always.
    Pop,
    /// By the object's place among the room's seeded objects in creation
    /// order (`trace::cart::room_objects`), as the search seeds them: the
    /// same seed at every load.
    Creation,
}

/// The fenced differences from the tracer's cart (module doc).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Fenced {
    pub loading_move: LoadingMove,
    pub seed_order: SeedOrder,
}

/// What this replay has always done.
pub const FENCED: Fenced = Fenced { loading_move: LoadingMove::Unguarded, seed_order: SeedOrder::Pop };

/// One room of a CHAIN: its room, its `[seeds]` as written, its inputs
/// from the frame its player is created (the tasdatabase's convention).
#[derive(Clone, Debug)]
pub struct Segment {
    pub room: (i16, i16),
    pub seeds: Vec<String>,
    pub inputs: Vec<u8>,
}

/// What the cart plays.
#[derive(Clone, Debug)]
pub enum Play {
    /// One room from its load: `inputs[f-1]` on frame f; `seeds` the
    /// balloons' and chests' (Lua number literals; `None`: the balloons'
    /// own `rnd(1)`).
    Room { room: (i16, i16), inputs: Vec<u8>, seeds: Option<Vec<String>> },
    /// THE CHAIN: the segments in order from the first room's load,
    /// THROUGH THE REAL ROOM TRANSITIONS, each segment's inputs from the
    /// frame its player is created. A room change ends a segment (its
    /// `exit` line); a death ends the chain.
    Chain(Vec<Segment>),
}

/// A cart: which, what it plays, and how.
#[derive(Clone, Debug)]
pub struct Spec {
    pub cart: Cart,
    pub play: Play,
    /// Frames run (past the inputs: no input).
    pub frames: usize,
    /// One dash past the orb (the gemskip categories).
    pub one_dash: bool,
    /// The start room's LOADING FRAME as a real transition runs it: objects
    /// J.. (the ORIGINAL cart's index) get one update, then the frame draws
    /// (Celia's IL load, cctas.lua `load_level`).
    pub jank: Option<usize>,
    /// Without `jank`: the start load's frame draws (no object updated): the
    /// game's boot, where `begin_game` runs inside the title's `_update`.
    pub load_draw: bool,
    /// `@P8@ dump` lines: the state as the last room is entered (after its
    /// loading frame).
    pub dump: bool,
    /// Every object (index, type, position, remainder, phase fields) on the
    /// frame lines.
    pub trace_objects: bool,
    pub fenced: Fenced,
}

/// The cart's object types (the original's; the minimal cart lacks some):
/// the names of the dumps and the object trace (`rewrite run
/// --dump-start`'s too).
pub const TYPES: &[&str] = &["player", "player_spawn", "spring", "balloon", "fall_floor", "smoke", "fruit", "fly_fruit", "lifeup", "fake_wall", "key", "chest", "platform", "message", "big_chest", "orb", "flag", "room_title"];

/// The globals of the room-entry dump: what a room can inherit from the
/// play before it, besides its objects (`chain::diff_dumps` compares them).
pub const DUMP_GLOBALS: &[&str] = &["max_djump", "deaths", "freeze", "shake", "has_dashed", "has_key", "will_restart", "delay_restart", "pause_player", "flash_bg", "new_bg", "frames", "seconds", "minutes", "music_timer", "sfx_timer"];

/// THE TRACER'S HOOKS AS PICO-8 STUBS: every function the tracer provides
/// that PICO-8 does not (`trace::cart::NATIVE`, `lua/builtin_level_*.lua`)
/// and the cart may call, the splitters the identity on a concrete value,
/// the normalization hint a no-op. A hook the cart calls without a stub
/// here crashes every replay (C12), and a stub of a hook it no longer calls
/// is dead: `tests::every_hook_the_cart_calls_has_a_stub`.
const HOOK_STUBS: &[(&str, &str)] = &[
    ("__split_by_round", "function __split_by_round(x) return x end"),
    ("_hint_normalize", "function _hint_normalize() end"),
];

/// The cart text (`.p8`) of `spec`.
pub fn cart(spec: &Spec) -> Result<String> {
    let (mut lua, start_call) = spec.cart.lua()?;
    let begin_game = matches!(spec.cart, Cart::Original(_));
    let replace_once = |lua: &mut String, pat: &str, new: &str, what: &str| -> Result<()> {
        ensure!(lua.matches(pat).count() == 1, "expected exactly one {pat:?}{what} in the Lua");
        *lua = lua.replacen(pat, new, 1);
        Ok(())
    };
    let (room, inputs, seeds): ((i16, i16), &[u8], Option<&[String]>) = match &spec.play {
        Play::Room { room, inputs, seeds } => (*room, inputs, seeds.as_deref()),
        // The chain's own seeds are its segments' (`__room_seeds`); the
        // last segment's inputs still name `__inputs`.
        Play::Chain(segs) => {
            let (first, last) = (segs.first().context("a chain without segments")?, segs.last().expect("one"));
            (first.room, &last.inputs, Some(&[][..]))
        }
    };
    if seeds.is_some() {
        // A balloon's phase is `offset=rnd(1)` at load, which PICO-8 seeds
        // itself, so a route past a balloon replays only under some draws.
        // The TAS tools fix each balloon's offset instead - the `[s1,s2,]`
        // header of a tasdatabase file - and so does this: the same phase
        // at the room load, but NOT the same run (`love::nudged_seeds`).
        // The tool's list runs over balloons AND chests (UniversalClassicTas
        // `set_seeds`); a chest's seed s is what its LAST shake's `rnd(3)`
        // returns minus 1 (the tool's chest update), so its berry appears at
        // x = start + s. The earlier shakes only draw it.
        let (balloon, chest) = match spec.fenced.seed_order {
            SeedOrder::Pop => ("__balloon_seed()", "__balloon_seed()"),
            // The chest's init has moved it 4 px left by then.
            SeedOrder::Creation => ("__seed_at(this.x,this.y)", "__seed_at(this.x+4,this.y)"),
        };
        replace_once(&mut lua, "this.offset=rnd(1)", &format!("this.offset={balloon}"), "")?;
        replace_once(&mut lua, "this.timer=20", &format!("this.timer=20 this.seed={chest}"), "")?;
        replace_once(&mut lua, "this.x=this.start-1+rnd(3)", "this.x=this.start-1+(this.timer<=0 and this.seed+1 or rnd(3))", "")?;
    }
    // Each dash START, as the game itself decides it: the player's dash
    // branch (entered neither during freeze nor during a dash, nor with
    // djump 0) records the held direction and the facing; the driver prints
    // it on the frame's line and clears it. A global write only: nothing
    // the game reads.
    const DASH: &str = "local v_input=(btn(k_up) and -1 or (btn(k_down) and 1 or 0))";
    replace_once(&mut lua, DASH, &format!("{DASH} __dash_start={{input,v_input,this.flip.x}}"), " (the dash branch)")?;
    // The start room: the minimal cart's `_init` call, the original's
    // `begin_game` one. Past the orb (room (5,2)'s big chest, level 21) a
    // play-through has the second dash (`game_runner::apply_start_room`,
    // the same); the gemskip categories skip the orb.
    let orb = if crate::game_runner::level_index(room.0, room.1) > crate::game_runner::ORB_LEVEL && !spec.one_dash { "max_djump=2 " } else { "" };
    replace_once(&mut lua, start_call, &format!("{orb}load_room({}, {})", room.0, room.1), "")?;

    let mut prelude = vec![format!("__begin_game = {begin_game}")];
    prelude.extend(HOOK_STUBS.iter().map(|(_, stub)| stub.to_string()));
    prelude.extend([
        format!("__balloon_seeds = {}", lua_list(seeds.unwrap_or(&[]))),
        "function __balloon_seed() return deli(__balloon_seeds, 1) or 0 end".into(),
        format!("__inputs = {}", lua_list(inputs)),
        "__frame = 0".into(),
        "function btn(i)".into(),
        "  local b = __inputs[__frame] or 0".into(),
        "  return (b & (1 << i)) ~= 0".into(),
        "end".into(),
        format!("__trace_objects = {}", spec.trace_objects),
    ]);
    if let Play::Chain(segs) = &spec.play {
        // A segment's seeds are drawn by its room's load, which a transition
        // runs inside the frame, before the driver sees the new room: keyed
        // by the level, restarted at every load (`load_room` below). Inputs
        // as one string per segment, a byte each (chr(48 + b)): a table
        // literal costs a token per input, and a boot chain's thousands
        // exceed PICO-8's 8192 (it then halts, "program too large").
        let lvl = |r: (i16, i16)| crate::game_runner::level_index(r.0, r.1);
        let text = |ins: &[u8]| ins.iter().map(|&b| char::from_u32(48 + u32::from(b)).expect("a char")).collect::<String>();
        prelude.extend([
            format!("__segs = {{{}}}", segs.iter().map(|s| format!("{{lvl={},inputs=\"{}\"}}", lvl(s.room), text(&s.inputs))).collect::<Vec<_>>().join(",")),
            format!("__room_seeds = {{{}}}", segs.iter().map(|s| format!("[{}]={}", lvl(s.room), lua_list(&s.seeds))).collect::<Vec<_>>().join(",")),
            "__seg, __seg_start, __seed_i = 1, nil, 0".into(),
            "function __balloon_seed() __seed_i += 1 local q = __room_seeds[room.x % 8 + room.y * 8] return q and q[__seed_i] or 0 end".into(),
            "function btn(i)".into(),
            "  if not __seg_start then return false end".into(),
            "  local b = (ord(__segs[__seg].inputs, __frame - __seg_start + 1) or 48) - 48".into(),
            "  return (b & (1 << i)) ~= 0".into(),
            "end".into(),
        ]);
    }
    if spec.fenced.seed_order == SeedOrder::Creation && seeds.is_some() {
        prelude.push(seed_table(&spec.play));
    }

    let dump_globals = DUMP_GLOBALS.iter().map(|g| format!("' {g} '..tostr({g})")).collect::<Vec<_>>().join("..");
    let mut driver: Vec<String> = vec![
        format!("__names = {{}} {}", TYPES.iter().map(|t| format!("if {t} then __names[{t}] = '{t}' end")).collect::<Vec<_>>().join(" ")),
        // The player's index and the object count as it leaves a room: the
        // transition's frame goes on updating the NEW room's objects from
        // that index (PICO-8's `all` resumes there), the loading jank.
        "local __next_room = next_room".into(),
        "function next_room()".into(),
        "  for i = 1, #objects do if objects[i].type == player then __exit_idx = i end end".into(),
        "  __exit_n = #objects".into(),
        "  __next_room()".into(),
        "end".into(),
        // The state as a room is entered: the globals, then every object's
        // own fields, sorted (tables of numbers inline; type, hair,
        // particles and functions left out).
        "function __fmt(v) if type(v) == 'number' then return tostr(v, true) end return tostr(v) end".into(),
        "function __dump(tag)".into(),
        "  local gf = ''".into(),
        "  for i = 1, 31 do if got_fruit[i] then gf = gf..i..';' end end".into(),
        format!("  printh('@P8@ dump '..tag..' globals'..{dump_globals}..' got_fruit '..gf)"),
    ];
    driver.extend(lines(DUMP_OBJECTS));
    if matches!(spec.play, Play::Chain(_)) {
        // A segment's inputs start on the frame its player is created: the
        // spawn creates it in its own update, and the player updates in the
        // same frame (appended to `objects`, `all` reaches it).
        driver.extend(lines(CHAIN_HOOKS));
    }
    // The ORIGINAL cart's _init shows the title screen; begin_game() is what
    // the first button press does. Frame 1 is then the first _update after
    // load_room, which is the TAS tool's convention.
    driver.extend(["_init()".to_string(), "if __begin_game then begin_game() end".to_string()]);
    if let Some(j) = spec.jank {
        ensure!(j >= 1, "a loading frame's J is an object index >= 1, not {j}");
        let j = match spec.cart {
            Cart::Minimal => minimal_index(room, j),
            Cart::Original(_) => j,
        };
        // The jank model: the loading frame of a real transition. The rest
        // of `_update`'s `foreach` runs over the new room from the leaving
        // player's index J, then the frame draws (Celia's `load_level`).
        let mv = match spec.fenced.loading_move {
            LoadingMove::Unguarded => "    o.move(o.spd.x, o.spd.y)",
            LoadingMove::Guarded => "    if o.spd.x ~= 0 or o.spd.y ~= 0 then o.move(o.spd.x, o.spd.y) end",
        };
        driver.extend([format!("for i = {j}, #objects do"), "  local o = objects[i]".into(), "  if o then".into(), mv.into(), "    if o.type.update ~= nil then o.type.update(o) end".into(), "  end".into(), "end".into(), "_draw()".into()]);
    } else if spec.load_draw {
        driver.push("_draw()".into());
    }
    let last = match &spec.play {
        Play::Chain(segs) => segs.len(),
        Play::Room { .. } => 1,
    };
    if spec.dump && last == 1 {
        driver.push("__dump('entry')".into());
    }
    driver.extend(["__deaths = deaths".to_string(), format!("for f = 1, {} do", spec.frames), "  __frame = f".into(), "  _update()".into(), "  _draw()".into()]);
    match &spec.play {
        // A room change ends a segment: its exit line (the frame, its
        // player's first frame, the count the tasdatabase gives it, the
        // leaving player's index, the berry), then the next segment. A
        // death ends the chain (a TAS that dies is a failure).
        Play::Chain(_) => {
            driver.extend(lines(CHAIN_EXIT));
            driver.push(if spec.dump { format!("    if __seg == {last} then __dump('entry') end") } else { String::new() });
            driver.extend(["  end".to_string(), format!("  if __seg == {last} then")]);
        }
        Play::Room { .. } => driver.push("  do".into()),
    }
    driver.extend(lines(FRAME_LINE));

    let mut out = String::from("pico-8 cartridge // http://www.pico-8.com\nversion 42\n__lua__\n");
    for part in [prelude.join("\n"), lua, driver.join("\n")] {
        out += &part;
        out.push('\n');
    }
    out += &data_sections();
    Ok(out)
}

/// `{a,b,..}`.
fn lua_list<T: std::fmt::Display>(xs: &[T]) -> String {
    format!("{{{}}}", xs.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(","))
}

fn lines(text: &str) -> impl Iterator<Item = String> + '_ {
    text.lines().map(str::to_string)
}

/// `__dump`'s objects.
const DUMP_OBJECTS: &str = "  for i = 1, #objects do
    local o, ks = objects[i], {}
    for k, v in pairs(o) do
      if k ~= 'type' and k ~= 'hair' and k ~= 'particles' and type(v) ~= 'function' then
        local j = #ks + 1
        while j > 1 and ks[j - 1] > k do ks[j] = ks[j - 1] j -= 1 end
        ks[j] = k
      end
    end
    local line = '@P8@ dump '..tag..' obj '..i..' '..(__names[o.type] or '?')
    for k in all(ks) do
      local v = o[k]
      if type(v) == 'table' then
        local t = ''
        for kk in all({'x', 'y', 'w', 'h'}) do if v[kk] ~= nil then t = t..kk..'='..__fmt(v[kk])..',' end end
        line = line..' '..k..'={'..t..'}'
      else
        line = line..' '..k..'='..__fmt(v)
      end
    end
    printh(line)
  end
end";

const CHAIN_HOOKS: &str = "local __load_room = load_room
function load_room(x, y) __seed_i = 0 __load_room(x, y) end
local __init_object = init_object
function init_object(t, x, y)
  local o = __init_object(t, x, y)
  if t == player and not __seg_start then __seg_start = __frame end
  return o
end";

const CHAIN_EXIT: &str = "  local lvl = room.x % 8 + room.y * 8
  if deaths ~= __deaths then printh('@P8@ death seg '..__seg..' f'..f) break end
  if lvl ~= __segs[__seg].lvl then
    local l0 = __segs[__seg].lvl
    printh('@P8@ exit seg '..__seg..' lvl '..l0..' f'..f..' start '..tostr(__seg_start)..' count '..(__seg_start and (f - __seg_start) or -1)..' player_idx '..tostr(__exit_idx)..' of '..tostr(__exit_n)..' berry '..tostr(got_fruit[1 + l0] == true)..' freeze '..freeze..' max_djump '..max_djump)
    __seg += 1 __seg_start = nil
    if __seg > #__segs then break end
    if lvl ~= __segs[__seg].lvl then printh('@P8@ unexpected room '..room.x..','..room.y) break end";

/// The frame line (`Frame`): ` dash DIR` (R L U D UR UL DR DL) is the dash
/// started this frame (with no direction held the game dashes the way it
/// faces); then the run's end: a chain whose earlier segment never exits
/// prints no frame line (they are the last segment's), and that is a result.
const FRAME_LINE: &str = "  local line = 'f'..f..' room '..room.x..','..room.y..' freeze '..freeze
  for o in all(objects) do
    if o.type == player then
      line = line..' player '..o.x..','..o.y..' spd '..tostr(o.spd.x, true)..','..tostr(o.spd.y, true)..' rem '..tostr(o.rem.x, true)..','..tostr(o.rem.y, true)
    elseif o.type == player_spawn then
      line = line..' player_spawn '..o.x..','..o.y
    elseif fly_fruit and o.type == fly_fruit then
      line = line..' fly_fruit '..o.x..','..o.y
    elseif fruit and o.type == fruit then
      line = line..' fruit '..o.x..','..o.y
    elseif lifeup and o.type == lifeup then
      line = line..' lifeup '..o.x..','..o.y
    end
  end
  if __dash_start then
    local h, v, flip = __dash_start[1], __dash_start[2], __dash_start[3]
    local d = (v < 0 and 'U' or (v > 0 and 'D' or ''))..(h > 0 and 'R' or (h < 0 and 'L' or ''))
    if d == '' then d = flip and 'L' or 'R' end
    line = line..' dash '..d
    __dash_start = nil
  end
  if __trace_objects then
    for i = 1, #objects do
      local o = objects[i]
      local n = __names[o.type] or '?'
      if n ~= 'player' and n ~= 'room_title' and n ~= 'smoke' then
        line = line..' '..n..'@'..i..' '..tostr(o.x, true)..','..tostr(o.y, true)..' rem '..tostr(o.rem.x, true)..','..tostr(o.rem.y, true)
        for k in all({'step', 'off', 'offset', 'timer', 'state', 'delay', 'spr'}) do if type(o[k]) == 'number' then line = line..' '..k..' '..tostr(o[k], true) end end
      end
    end
  end
  printh('@P8@ '..line)
  end
end
printh('@P8@ end')
_init = nil _update = nil _draw = nil";

/// C11's `Creation` order: `__seed_at(x, y)`, the seed of the seeded object
/// created at pixel (x, y) of the current room, by its place in the room's
/// creation order (`trace::cart::room_objects`); 0 where none is given.
fn seed_table(play: &Play) -> String {
    let rooms: Vec<((i16, i16), &[String])> = match play {
        Play::Room { room, seeds, .. } => vec![(*room, seeds.as_deref().unwrap_or(&[]))],
        Play::Chain(segs) => segs.iter().map(|s| (s.room, &s.seeds[..])).collect(),
    };
    let per_room: Vec<String> = rooms
        .iter()
        .map(|&(r, seeds)| {
            let seeded = crate::trace::cart::room_objects(r).into_iter().filter(|o| o.seeded);
            let at: Vec<String> = seeded.enumerate().map(|(i, o)| format!("[\"{},{}\"]={}", o.tx * 8, o.ty * 8, seeds.get(i).map_or("0", |s| s.as_str()))).collect();
            format!("[{}]={{{}}}", crate::game_runner::level_index(r.0, r.1), at.join(","))
        })
        .collect();
    format!(
        "__seeds_at = {{{}}}\nfunction __seed_at(x, y) local q = __seeds_at[room.x % 8 + room.y * 8] return q and q[x..','..y] or 0 end",
        per_room.join(",")
    )
}

/// The minimal cart's index of room `room`'s object the ORIGINAL cart
/// numbers `j`: one past the minimal cart's objects before it.
fn minimal_index(room: (i16, i16), j: usize) -> usize {
    1 + crate::trace::cart::room_objects(room).iter().take(j - 1).filter(|o| o.minimal).count()
}

/// `__gfx__`, `__gff__` and `__map__`: the map's rows 0-31 as 32 lines of
/// 128 bytes; rows 32-63 live in the sprite sheet's lower half (`__gfx__`
/// lines 64-127): a sprite-sheet line is 64 bytes (128 pixels, one hex
/// digit each, low nibble first), so each map row is TWO of them (PICO-8
/// truncates a 256-character line).
fn data_sections() -> String {
    let cart = &celeste_core::cart_data::CART;
    let (map, flags) = (cart.map_grid(), cart.flag_bytes());
    let hex = |b: &[u8]| b.iter().map(|v| format!("{v:02x}")).collect::<String>();
    let mut gfx: Vec<String> = vec!["0".repeat(128); 64];
    for r in 32..64 {
        for half in 0..2 {
            gfx.push(map[r * 128 + half * 64..r * 128 + (half + 1) * 64].iter().map(|b| format!("{:x}{:x}", b & 15, b >> 4)).collect());
        }
    }
    let map_lines: Vec<String> = (0..32).map(|r| hex(&map[r * 128..(r + 1) * 128])).collect();
    format!("__gfx__\n{}\n__gff__\n{}\n{}\n__map__\n{}\n", gfx.join("\n"), hex(&flags[..128]), hex(&flags[128..]), map_lines.join("\n"))
}

/// A Lua number literal as Python's `str(float(text))` writes it: how this
/// replay has always written a single room's seeds (`0` as `0.0`).
pub fn py_float(text: &str) -> Result<String> {
    let v: f64 = text.trim().parse().with_context(|| format!("a seed {text:?} is not a number"))?;
    ensure!(v.is_finite(), "a seed {text:?} is not finite");
    // The shortest digits that round-trip, and the decimal point's place.
    let e = format!("{:e}", v.abs());
    let (mantissa, exp) = e.split_once('e').expect("an exponent");
    let digits: String = mantissa.chars().filter(|c| *c != '.').collect();
    let point = exp.parse::<i32>().expect("an exponent") + 1;
    let n = digits.len() as i32;
    let body = if point <= -4 || point > 16 {
        let m = if n == 1 { digits.clone() } else { format!("{}.{}", &digits[..1], &digits[1..]) };
        let x = point - 1;
        format!("{m}e{}{:02}", if x < 0 { '-' } else { '+' }, x.abs())
    } else if point <= 0 {
        format!("0.{}{digits}", "0".repeat(-point as usize))
    } else if point < n {
        format!("{}.{}", &digits[..point as usize], &digits[point as usize..])
    } else {
        format!("{digits}{}.0", "0".repeat((point - n) as usize))
    };
    Ok(if v.is_sign_negative() { format!("-{body}") } else { body })
}

/// The PICO-8 binary's usual place.
pub fn default_pico8() -> PathBuf {
    PathBuf::from(format!("{}/pico-8/pico8", std::env::var("HOME").unwrap_or_default()))
}

/// Where a run's carts go: kept (`--keep DIR`) or a fresh directory removed
/// after.
pub struct Carts {
    dir: PathBuf,
    keep: bool,
    n: std::cell::Cell<usize>,
}

impl Carts {
    pub fn new(keep: Option<&Path>) -> Result<Carts> {
        let (dir, keep) = match keep {
            Some(d) => (d.to_path_buf(), true),
            None => (crate::tools::scratch_dir("celeste-replay")?, false),
        };
        std::fs::create_dir_all(&dir).with_context(|| format!("creating {}", dir.display()))?;
        Ok(Carts { dir, keep, n: Default::default() })
    }
}

impl Drop for Carts {
    fn drop(&mut self) {
        if !self.keep {
            let _ = std::fs::remove_dir_all(&self.dir);
        }
    }
}

/// How PICO-8 runs: its binary and where the carts go.
pub struct Runner {
    pub pico8: PathBuf,
    pub carts: Carts,
}

impl Runner {
    pub fn new(pico8: Option<&Path>, keep: Option<&Path>) -> Result<Runner> {
        ensure!(std::env::var_os("PICO8").is_none(), "PICO8 in the environment: the binary is --pico8");
        Ok(Runner { pico8: pico8.map_or_else(default_pico8, Path::to_path_buf), carts: Carts::new(keep)? })
    }

    /// `spec`'s cart run on PICO-8: its `@P8@` lines (`cart_lines`).
    pub fn run(&self, spec: &Spec) -> Result<Vec<String>> {
        let text = cart(spec)?;
        let n = self.carts.n.get() + 1;
        self.carts.n.set(n);
        let path = self.carts.dir.join(if self.carts.keep { format!("replay-{n:03}.p8") } else { "replay.p8".to_string() });
        std::fs::write(&path, text).with_context(|| format!("writing {}", path.display()))?;
        let inputs = match &spec.play {
            Play::Room { inputs, .. } => inputs.len(),
            Play::Chain(segs) => segs.last().map_or(0, |s| s.inputs.len()),
        };
        eprintln!("[replay] {inputs} inputs, {} frames, cart {}", spec.frames, path.display());
        let out = crate::tools::run_with_timeout(std::process::Command::new(&self.pico8).arg("-x").arg(&path), std::time::Duration::from_secs(600))?;
        // PICO-8 halts on a cart it refuses ("program too large") and waits.
        let Some(out) = out else { bail!("pico8 timed out (600 s): {}", path.display()) };
        cart_lines(&String::from_utf8_lossy(&out.stdout), &String::from_utf8_lossy(&out.stderr), &path)
    }
}

/// The `@P8@` lines of a run, its `end` line dropped. The driver prints
/// `end` last, always: without it the cart did not run to its end (a
/// runtime error, a halt), and its partial output is no result.
pub fn cart_lines(stdout: &str, stderr: &str, cart: &Path) -> Result<Vec<String>> {
    let mut lines: Vec<String> = stdout.lines().filter_map(|l| l.strip_prefix("@P8@ ")).map(str::to_string).collect();
    if lines.last().map(String::as_str) != Some("end") {
        let tail = |s: &str| s[s.len().saturating_sub(500)..].to_string();
        bail!("the cart did not run to its end ({}); pico8 said: {:?}", cart.display(), (tail(stdout) + &tail(stderr)).trim());
    }
    lines.pop();
    Ok(lines)
}

/// One frame line (`fF room X,Y freeze N` and, as the frame has them,
/// ` player X,Y spd SX,SY rem RX,RY`, ` player_spawn X,Y`, ` fly_fruit X,Y`,
/// ` fruit X,Y`, ` lifeup X,Y`, ` dash DIR`), typed.
#[derive(Clone, Debug, PartialEq)]
pub struct Frame {
    pub f: u32,
    pub room: (i16, i16),
    /// The player's position as printed.
    pub player: Option<(f64, f64)>,
    pub spawn: Option<(f64, f64)>,
    pub dash: Option<String>,
    /// A `lifeup` (a berry taken) is in the room.
    pub lifeup: bool,
}

/// The diagonal dash directions.
pub const DIAGONAL: &[&str] = &["UR", "UL", "DR", "DL"];

impl Frame {
    /// Where the player is (rising from below: its spawn).
    pub fn at(&self) -> Option<(f64, f64)> {
        self.player.or(self.spawn)
    }
}

/// The frame lines of a run's output, typed; other lines skipped.
pub fn frames(lines: &[String]) -> Vec<Frame> {
    lines.iter().filter_map(|l| frame(l)).collect()
}

fn frame(line: &str) -> Option<Frame> {
    let w: Vec<&str> = line.split(' ').collect();
    let f = w.first()?.strip_prefix('f')?.parse().ok()?;
    if w.get(1) != Some(&"room") {
        return None;
    }
    let (x, y) = w.get(2)?.split_once(',')?;
    let room = (x.parse().ok()?, y.parse().ok()?);
    // The first `NAME A,B` pair's numbers.
    let xy = |name: &str| -> Option<(f64, f64)> {
        let i = w.iter().position(|t| *t == name)?;
        let (a, b) = w.get(i + 1)?.split_once(',')?;
        Some((a.parse().ok()?, b.parse().ok()?))
    };
    let dash = w.iter().position(|t| *t == "dash").and_then(|i| w.get(i + 1)).map(|s| s.to_string());
    Some(Frame { f, room, player: xy("player"), spawn: xy("player_spawn"), dash, lifeup: w.contains(&"lifeup") })
}

/// The frame the room changes during (`None`: it never does).
pub fn exit_frame(fs: &[Frame], room: (i16, i16)) -> Option<u32> {
    fs.iter().find(|fr| fr.room != room).map(|fr| fr.f)
}

/// THE PROBE CORPORA'S GOLDENS (`trace::probe`): each `lua/probe/NAME.lua`
/// of the checkout `root` run on a real PICO-8 (with `__hex`, the exact
/// print the tracer has natively), its output (the `RUNNING:` banner
/// dropped) written to `NAME.expected` where it changed: beside it first,
/// then renamed over it, as tests read these files meanwhile. The names
/// whose golden changed. READ THE DIFF: a change means the model's
/// reference moved.
pub fn probe_goldens(pico8: &Path, root: &Path) -> Result<Vec<String>> {
    ensure!(pico8.exists(), "no PICO-8 at {} (--pico8 /path/to/pico8)", pico8.display());
    let probe = root.join("lua/probe");
    let mut names: Vec<String> = std::fs::read_dir(&probe)?.filter_map(|e| e.ok()?.path().to_str()?.strip_suffix(".lua").map(|p| p.rsplit('/').next().expect("a name").to_string())).collect();
    names.sort();
    let tmp = crate::tools::scratch_dir("celeste-probe")?;
    let mut changed = Vec::new();
    for name in &names {
        let lua = std::fs::read_to_string(probe.join(format!("{name}.lua")))?;
        let cart = format!("{name}.p8");
        std::fs::write(tmp.join(&cart), format!("pico-8 cartridge // http://www.pico-8.com\nversion 42\n__lua__\nfunction __hex(x) printh(tostr(x, true)) end\n{lua}"))?;
        // PICO-8 wants to be run from the cart's directory.
        let out = crate::tools::run_with_timeout(std::process::Command::new(pico8).arg("-x").arg(&cart).current_dir(&tmp).env("SDL_VIDEODRIVER", "dummy"), std::time::Duration::from_secs(600))?.with_context(|| format!("{name}: pico8 timed out"))?;
        let text: String = String::from_utf8_lossy(&out.stdout).lines().filter(|l| !l.starts_with("RUNNING:")).map(|l| format!("{l}\n")).collect();
        let golden = probe.join(format!("{name}.expected"));
        if std::fs::read_to_string(&golden).ok().as_deref() != Some(text.as_str()) {
            let new = probe.join(format!(".{name}.{}", std::process::id()));
            std::fs::write(&new, &text)?;
            std::fs::rename(&new, &golden)?;
            changed.push(name.clone());
        }
        println!("{name}: {} lines", text.lines().count());
    }
    let _ = std::fs::remove_dir_all(&tmp);
    Ok(changed)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Python's `str(float(s))`, as this replay wrote single-room seeds.
    #[test]
    fn seeds_are_written_as_python_wrote_them() {
        for (text, py) in [("0", "0.0"), ("0.5", "0.5"), ("-1", "-1.0"), ("0.0001", "0.0001"), ("0.00001", "1e-05"), ("1e16", "1e+16"), ("123.25", "123.25"), ("0.1", "0.1"), ("1e15", "1000000000000000.0"), ("0.3333", "0.3333"), (" 0.25", "0.25")] {
            assert_eq!(py_float(text).expect("a number"), py, "{text:?}");
        }
        assert!(py_float("x").is_err() && py_float("inf").is_err());
    }

    /// The frame lines as the driver prints them.
    #[test]
    fn frame_lines_parse() {
        let fs = frames(&["f3 room 1,0 freeze 0 player 16,96 spd 0x0001.0000,0x0000.0000 rem 0x0000.0000,0x0000.0000 dash UR".to_string(), "dump entry globals".into(), "f4 room 2,0 freeze 2 player_spawn 8,128 lifeup 3,4".into()]);
        assert_eq!(fs[0], Frame { f: 3, room: (1, 0), player: Some((16.0, 96.0)), spawn: None, dash: Some("UR".into()), lifeup: false });
        assert_eq!((fs[1].at(), fs[1].lifeup, exit_frame(&fs, (1, 0))), (Some((8.0, 128.0)), true, Some(4)));
        assert_eq!(fs.len(), 2);
    }

    /// The driver prints `end` last: without it the run is no result.
    #[test]
    fn a_run_without_its_end_is_refused() {
        let c = Path::new("c.p8");
        assert_eq!(cart_lines("RUNNING: c.p8\n@P8@ f1 room 1,0\n@P8@ end\n", "", c).expect("lines"), ["f1 room 1,0"]);
        let e = cart_lines("@P8@ f1 room 1,0\nruntime error line 663\n", "", c).expect_err("no end").to_string();
        assert!(e.contains("did not run to its end (c.p8)") && e.contains("runtime error line 663"), "{e}");
    }

    /// Every hook the minimal cart calls that PICO-8 lacks has a stub
    /// (C12: a hook added to the tracer, called by the cart and missing
    /// here, crashed every minimal-cart replay for five days); and the cart
    /// builds in every mode (each anchor found exactly once), in both
    /// variants of the fenced differences.
    #[test]
    fn every_hook_the_cart_calls_has_a_stub() {
        use full_moon::visitors::Visitor;
        // What PICO-8 itself provides, of what the tracer provides.
        const PICO8: &[&str] = &["add", "foreach", "btn", "count", "del", "print", "music", "sfx", "pal", "rectfill", "map", "spr", "circfill", "min", "max", "abs", "flr", "sin", "rnd", "mget", "fget", "printh"];
        #[derive(Default)]
        struct Names {
            called: std::collections::BTreeSet<String>,
            defined: std::collections::BTreeSet<String>,
        }
        impl Visitor for Names {
            fn visit_function_call(&mut self, call: &full_moon::ast::FunctionCall) {
                if let full_moon::ast::Prefix::Name(n) = call.prefix() {
                    self.called.insert(n.token().to_string());
                }
            }
            fn visit_function_declaration(&mut self, f: &full_moon::ast::FunctionDeclaration) {
                self.defined.insert(f.name().to_string().trim().to_string());
            }
        }
        let names = |src: &str| {
            let mut v = Names::default();
            v.visit_ast(&full_moon::parse(src).expect("Lua"));
            v
        };
        let builtins = names(&format!("{}\n{}", crate::trace::cart::BUILTIN_LEVEL_3, crate::trace::cart::BUILTIN_LEVEL_4));
        let hooks: Vec<String> = crate::trace::cart::NATIVE.iter().map(|s| s.to_string()).chain(builtins.defined).filter(|n| !PICO8.contains(&n.as_str())).collect();
        let minimal = names(crate::trace::cart::MINIMAL_CART);
        let called: Vec<&String> = hooks.iter().filter(|h| minimal.called.contains(*h) && !minimal.defined.contains(*h)).collect();
        assert!(called.iter().any(|h| *h == "__split_by_round"), "the scan finds the hooks the cart calls: {called:?}");
        let missing: Vec<&&String> = called.iter().filter(|h| !HOOK_STUBS.iter().any(|(n, _)| *n == h.as_str())).collect();
        assert!(missing.is_empty(), "the cart calls tracer hooks PICO-8 has no stub for (HOOK_STUBS): {missing:?}");
        let dead: Vec<&str> = HOOK_STUBS.iter().map(|(n, _)| *n).filter(|n| !called.iter().any(|h| h == n)).collect();
        assert!(dead.is_empty(), "stubs of hooks the cart does not call (HOOK_STUBS): {dead:?}");
        for fenced in [FENCED, Fenced { loading_move: LoadingMove::Guarded, seed_order: SeedOrder::Creation }] {
            let room = |seeds: Option<Vec<String>>| Play::Room { room: (3, 3), inputs: vec![0, 1, 2], seeds };
            let chain = Play::Chain(vec![Segment { room: (2, 3), seeds: vec![], inputs: vec![1] }, Segment { room: (3, 3), seeds: vec!["0.5".into()], inputs: vec![2] }]);
            for (play, jank, load_draw, dump) in [(room(None), None, false, false), (room(Some(vec!["0.0".into()])), Some(2), false, true), (chain, None, true, true)] {
                let spec = Spec { cart: Cart::Minimal, play, frames: 10, one_dash: false, jank, load_draw, dump, trace_objects: true, fenced };
                let text = cart(&spec).expect("the cart builds");
                assert!(text.contains("printh('@P8@ end')") && text.ends_with('\n'), "{spec:?}");
            }
        }
    }
}
