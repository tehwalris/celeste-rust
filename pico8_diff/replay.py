#!/usr/bin/env python3
"""Replay an input sequence on a REAL PICO-8 and print the per-frame state.

The definitive fidelity check for a witness: `lua/celeste-minimal.lua`
(the exact Lua the search runs) plus the cart's map and flag data, driven
frame by frame with `btn` overridden to read the scripted inputs, headless
under `pico8 -x`. Same frame convention as `concrete_run`: frame f is the
f-th `_update();_draw()` after `_init()`, and the input byte for frame f is
`inputs[f-1]` (bit0=left bit1=right bit2=up bit3=down bit4=jump bit5=dash).

    pico8_diff/replay.py tas/room_1_0_exit_frame_100.txt
    pico8_diff/replay.py --inputs 0,0,...,42 [--frames N]
    PICO8=~/pico-8/pico8 pico8_diff/replay.py ...
"""
import argparse
import os
import re
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def read_inputs(path):
    text = "".join(l for l in open(path) if not l.startswith("#"))
    return [int(x) for x in re.split(r"[,\s]+", text.strip()) if x]


def hexbytes(path, n):
    h = re.sub(r"\s", "", open(path).read())
    assert len(h) == 2 * n, f"{path}: {len(h)//2} bytes, expected {n}"
    return bytes.fromhex(h)


# The cart's object types (the original's; the minimal cart lacks some):
# names for the dumps and the object trace.
TYPES = ["player", "player_spawn", "spring", "balloon", "fall_floor", "smoke", "fruit", "fly_fruit", "lifeup", "fake_wall", "key", "chest", "platform", "message", "big_chest", "orb", "flag", "room_title"]

# The globals of the room-entry dump: what a room can inherit from the play
# before it, besides its objects (pico8_diff/chain.py compares them).
DUMP_GLOBALS = ["max_djump", "deaths", "freeze", "shake", "has_dashed", "has_key", "will_restart", "delay_restart", "pause_player", "flash_bg", "new_bg", "frames", "seconds", "minutes", "music_timer", "sfx_timer"]


# The tiles load_room makes an object of in the ORIGINAL cart, and whether
# the minimal cart makes it too (src/trace/cart.rs OBJECT_TILES): it has no
# `message` (86) nor `flag` (118).
OBJECT_TILES = {1: True, 8: True, 11: True, 12: True, 18: True, 20: True, 22: True, 23: True, 26: True, 28: True, 64: True, 96: True, 86: False, 118: False}


def minimal_jank(room, j):
    """The minimal cart's index of the original cart's J-th object (the
    loading frame's first, `--jank`): one past the minimal objects before it."""
    m = hexbytes(os.path.join(ROOT, "cart", "map-data.txt"), 8192)
    tiles = [m[(room[1] * 16 + ty) * 128 + room[0] * 16 + tx] for tx in range(16) for ty in range(16)]
    objs = [OBJECT_TILES[t] for t in tiles if t in OBJECT_TILES]
    return 1 + sum(objs[:j - 1])


def lua_list(xs):
    return "{" + ",".join(str(v) for v in xs) + "}"


def build_cart(inputs, frames, out, lua_path=None, begin_game=False, room=None, balloon_seeds=None, one_dash=False, segments=None, jank=None, dump=False, trace_objects=False, load_draw=False):
    """`segments` (the CHAIN mode, pico8_diff/chain.py): [(room, seeds,
    inputs), ...] played in order from the first room's load THROUGH THE
    REAL ROOM TRANSITIONS, each segment's inputs from the frame its player is
    created (the tasdatabase convention); `room` is the first segment's,
    `inputs` and `balloon_seeds` are unused. `jank` J: the start room's
    LOADING FRAME as a real transition runs it - objects J.. get one update
    (move, then update), then the frame draws (Celia's IL load, cctas.lua
    `load_level`). `dump`: the state as the last segment's room is entered
    (after its loading frame), as `@P8@ dump` lines. `trace_objects`: every
    object (index, type, position, remainder, phase fields) on the frame
    lines. `load_draw`: the start load's frame draws (no object updated): the
    game's boot, where begin_game runs inside the title screen's `_update`."""
    lua = open(lua_path or os.path.join(ROOT, "lua", "celeste-minimal.lua")).read()
    if segments is not None:
        balloon_seeds = []
        room = segments[0][0]
    if balloon_seeds is not None:
        # A balloon's phase is `offset=rnd(1)` at load, which PICO-8 seeds
        # itself, so a route past a balloon replays only under some draws.
        # The TAS tool (gonengazit/UniversalClassicTas) fixes each balloon's
        # offset instead - the `[s1,s2,]` header of a tasdatabase file, in
        # object creation order - and so does this. The same seed is the same
        # phase at the room load in both, but NOT the same run: UCT adds 0.01
        # a frame in Lua doubles, PICO-8 0x0.028f, so a balloon whose y meets
        # an integer can be touched here and missed there (seed 0 puts y at
        # exactly `start` at offset 0.5 in UCT). Check uploads in UCT too
        # (tools/uct/validate.sh; category_runner.nudged_seeds).
        pat = "this.offset=rnd(1)"
        assert lua.count(pat) == 1, f"expected exactly one {pat!r} in the Lua"
        lua = lua.replace(pat, "this.offset=__balloon_seed()")
        # The tool's list runs over balloons AND chests, in creation order
        # (UniversalClassicTas `set_seeds`); a chest's seed s is what its
        # LAST shake's `rnd(3)` returns minus 1 (the tool's chest update), so
        # its berry appears at x = start + s. The earlier shakes only draw it.
        for pat, new in (("this.timer=20", "this.timer=20 this.seed=__balloon_seed()"), ("this.x=this.start-1+rnd(3)", "this.x=this.start-1+(this.timer<=0 and this.seed+1 or rnd(3))")):
            assert lua.count(pat) == 1, f"expected exactly one {pat!r} in the Lua"
            lua = lua.replace(pat, new)
    # Each dash START, as the game itself decides it: the player's dash
    # branch (`if this.djump>0 and dash then`, entered neither during freeze
    # nor during a dash, nor with djump 0) records the held direction and the
    # facing, the driver prints it on the frame's line and clears it. A global
    # write only: nothing the game reads.
    pat = "local v_input=(btn(k_up) and -1 or (btn(k_down) and 1 or 0))"
    assert lua.count(pat) == 1, f"expected exactly one {pat!r} (the dash branch) in the Lua"
    lua = lua.replace(pat, pat + " __dash_start={input,v_input,this.flip.x}")
    if room is not None:
        # The minimal cart's _init loads room (1,0); the search patches the
        # same call (celeste-interp game_runner) to start elsewhere. The
        # ORIGINAL cart's start is begin_game's `load_room(0,0)` - patched the
        # same way, so a community TAS of any level replays from its room.
        pat = "load_room(0,0)" if begin_game else "load_room(1, 0)"
        assert lua.count(pat) == 1, f"expected exactly one {pat!r} in the Lua"
        # Past the orb (room (5,2)'s big chest, level 21) a play-through has
        # the second dash: max_djump=2 (celeste-interp game_runner, the same).
        # --one-dash: the gemskip categories skip the orb, one dash throughout.
        orb = "max_djump=2 " if room[0] % 8 + room[1] * 8 > 21 and not one_dash else ""
        lua = lua.replace(pat, f"{orb}load_room({room[0]}, {room[1]})")
    map_data = hexbytes(os.path.join(ROOT, "cart", "map-data.txt"), 8192)
    flags = hexbytes(os.path.join(ROOT, "cart", "flag-data.txt"), 256)
    prelude = [
        "__begin_game = " + ("true" if begin_game else "false"),
        # The interpreter's hooks: the interval splitter (identity on a
        # concrete value) and the state-normalization hint (a no-op).
        "function __split_by_flr(x) return x end",
        "function _hint_normalize() end",
        "__balloon_seeds = " + lua_list(balloon_seeds or []),
        "function __balloon_seed() return deli(__balloon_seeds, 1) or 0 end",
        "__inputs = " + lua_list(inputs),
        "__frame = 0",
        "function btn(i)",
        "  local b = __inputs[__frame] or 0",
        "  return (b & (1 << i)) ~= 0",
        "end",
        "__trace_objects = " + ("true" if trace_objects else "false"),
    ]
    if segments is not None:
        # A segment's seeds are drawn by its room's load, which a transition
        # runs inside the frame, before the driver sees the new room: keyed
        # by the level, restarted at every load (`load_room` below).
        prelude += [
            # Inputs as one string per segment, a byte each (chr(48 + b)): a
            # table literal costs a token per input, and a boot chain's
            # thousands exceed PICO-8's 8192 (it then halts, "program too large").
            "__segs = {" + ",".join(f'{{lvl={r[0] % 8 + r[1] * 8},inputs="' + "".join(chr(48 + b) for b in i) + '"}' for r, _, i in segments) + "}",
            "__room_seeds = {" + ",".join(f"[{r[0] % 8 + r[1] * 8}]={lua_list(sd)}" for r, sd, _ in segments) + "}",
            "__seg, __seg_start, __seed_i = 1, nil, 0",
            "function __balloon_seed() __seed_i += 1 local q = __room_seeds[room.x % 8 + room.y * 8] return q and q[__seed_i] or 0 end",
            "function btn(i)",
            "  if not __seg_start then return false end",
            "  local b = (ord(__segs[__seg].inputs, __frame - __seg_start + 1) or 48) - 48",
            "  return (b & (1 << i)) ~= 0",
            "end",
        ]
    dump_globals = "..".join(f"' {g} '..tostr({g})" for g in DUMP_GLOBALS)
    hooks = [
        "__names = {} " + " ".join(f"if {t} then __names[{t}] = '{t}' end" for t in TYPES),
        # The player's index and the object count as it leaves a room: the
        # transition's frame goes on updating the NEW room's objects from
        # that index (PICO-8's `all` resumes there), the loading jank.
        "local __next_room = next_room",
        "function next_room()",
        "  for i = 1, #objects do if objects[i].type == player then __exit_idx = i end end",
        "  __exit_n = #objects",
        "  __next_room()",
        "end",
        # The state as a room is entered: the globals, then every object's
        # own fields, sorted (tables of numbers inline; type, hair,
        # particles and functions left out).
        "function __fmt(v) if type(v) == 'number' then return tostr(v, true) end return tostr(v) end",
        "function __dump(tag)",
        "  local gf = ''",
        "  for i = 1, 31 do if got_fruit[i] then gf = gf..i..';' end end",
        f"  printh('@P8@ dump '..tag..' globals'..{dump_globals}..' got_fruit '..gf)",
        "  for i = 1, #objects do",
        "    local o, ks = objects[i], {}",
        "    for k, v in pairs(o) do",
        "      if k ~= 'type' and k ~= 'hair' and k ~= 'particles' and type(v) ~= 'function' then",
        "        local j = #ks + 1",
        "        while j > 1 and ks[j - 1] > k do ks[j] = ks[j - 1] j -= 1 end",
        "        ks[j] = k",
        "      end",
        "    end",
        "    local line = '@P8@ dump '..tag..' obj '..i..' '..(__names[o.type] or '?')",
        "    for k in all(ks) do",
        "      local v = o[k]",
        "      if type(v) == 'table' then",
        "        local t = ''",
        "        for kk in all({'x', 'y', 'w', 'h'}) do if v[kk] ~= nil then t = t..kk..'='..__fmt(v[kk])..',' end end",
        "        line = line..' '..k..'={'..t..'}'",
        "      else",
        "        line = line..' '..k..'='..__fmt(v)",
        "      end",
        "    end",
        "    printh(line)",
        "  end",
        "end",
    ]
    if segments is not None:
        hooks += [
            "local __load_room = load_room",
            "function load_room(x, y) __seed_i = 0 __load_room(x, y) end",
            # A segment's inputs start on the frame its player is created:
            # the spawn creates it in its own update, and the player updates
            # in the same frame (appended to `objects`, `all` reaches it).
            "local __init_object = init_object",
            "function init_object(t, x, y)",
            "  local o = __init_object(t, x, y)",
            "  if t == player and not __seg_start then __seg_start = __frame end",
            "  return o",
            "end",
        ]
    driver = hooks + [
        "_init()",
        # The ORIGINAL cart's _init shows the title screen; begin_game()
        # is what the first button press does. Frame 1 is then the first
        # _update after load_room, which is the TAS tool's convention.
        "if __begin_game then begin_game() end",
    ]
    if jank is not None and lua_path is None:
        jank = minimal_jank(room or (1, 0), jank)
    if jank is not None:
        # The jank model: the loading frame of a real transition. The rest
        # of `_update`'s `foreach` runs over the new room from the leaving
        # player's index J, then the frame draws (Celia's `load_level`).
        driver += [
            f"for i = {jank}, #objects do",
            "  local o = objects[i]",
            "  if o then",
            "    o.move(o.spd.x, o.spd.y)",
            "    if o.type.update ~= nil then o.type.update(o) end",
            "  end",
            "end",
            "_draw()",
        ]
    elif load_draw:
        driver.append("_draw()")
    last = len(segments) if segments is not None else 1
    if dump and last == 1:
        driver.append("__dump('entry')")
    driver += [
        "__deaths = deaths",
        f"for f = 1, {frames} do",
        "  __frame = f",
        "  _update()",
        "  _draw()",
    ]
    if segments is not None:
        # A room change ends a segment: its exit line (the frame, its
        # player's first frame, the count the tasdatabase gives it, the
        # leaving player's index, the berry), then the next segment. A
        # death ends the chain (a TAS that dies is a failure).
        driver += [
            "  local lvl = room.x % 8 + room.y * 8",
            "  if deaths ~= __deaths then printh('@P8@ death seg '..__seg..' f'..f) break end",
            "  if lvl ~= __segs[__seg].lvl then",
            "    local l0 = __segs[__seg].lvl",
            "    printh('@P8@ exit seg '..__seg..' lvl '..l0..' f'..f..' start '..tostr(__seg_start)..' count '..(__seg_start and (f - __seg_start) or -1)"
            "..' player_idx '..tostr(__exit_idx)..' of '..tostr(__exit_n)..' berry '..tostr(got_fruit[1 + l0] == true)..' freeze '..freeze..' max_djump '..max_djump)",
            "    __seg += 1 __seg_start = nil",
            "    if __seg > #__segs then break end",
            "    if lvl ~= __segs[__seg].lvl then printh('@P8@ unexpected room '..room.x..','..room.y) break end",
            f"    if __seg == {last} then __dump('entry') end" if dump else "",
            "  end",
            f"  if __seg == {last} then",
        ]
    else:
        driver.append("  do")
    driver += [
        "  local line = 'f'..f..' room '..room.x..','..room.y..' freeze '..freeze",
        "  for o in all(objects) do",
        "    if o.type == player then",
        "      line = line..' player '..o.x..','..o.y..' spd '..tostr(o.spd.x, true)..','..tostr(o.spd.y, true)"
        "..' rem '..tostr(o.rem.x, true)..','..tostr(o.rem.y, true)",
        "    elseif o.type == player_spawn then",
        "      line = line..' player_spawn '..o.x..','..o.y",
        # The room's fruit, and the lifeup a taken one leaves (a
        # removed fruit with no lifeup flew away instead).
        "    elseif fly_fruit and o.type == fly_fruit then",
        "      line = line..' fly_fruit '..o.x..','..o.y",
        "    elseif fruit and o.type == fruit then",
        "      line = line..' fruit '..o.x..','..o.y",
        "    elseif lifeup and o.type == lifeup then",
        "      line = line..' lifeup '..o.x..','..o.y",
        "    end",
        "  end",
        # ` dash DIR` (R L U D UR UL DR DL): the dash started this frame. With
        # no direction held the game dashes horizontally the way it faces.
        "  if __dash_start then",
        "    local h, v, flip = __dash_start[1], __dash_start[2], __dash_start[3]",
        "    local d = (v < 0 and 'U' or (v > 0 and 'D' or ''))..(h > 0 and 'R' or (h < 0 and 'L' or ''))",
        "    if d == '' then d = flip and 'L' or 'R' end",
        "    line = line..' dash '..d",
        "    __dash_start = nil",
        "  end",
        "  if __trace_objects then",
        "    for i = 1, #objects do",
        "      local o = objects[i]",
        "      local n = __names[o.type] or '?'",
        "      if n ~= 'player' and n ~= 'room_title' and n ~= 'smoke' then",
        "        line = line..' '..n..'@'..i..' '..tostr(o.x, true)..','..tostr(o.y, true)..' rem '..tostr(o.rem.x, true)..','..tostr(o.rem.y, true)",
        "        for k in all({'step', 'off', 'offset', 'timer', 'state', 'delay', 'spr'}) do if type(o[k]) == 'number' then line = line..' '..k..' '..tostr(o[k], true) end end",
        "      end",
        "    end",
        "  end",
        "  printh('@P8@ '..line)",
        "  end",
        "end",
        # The run's end: a chain whose earlier segment never exits prints no
        # frame line (they are the last segment's), and that is a result.
        "printh('@P8@ end')",
        "_init = nil _update = nil _draw = nil",
    ]
    # __map__: rows 0..31 as 32 lines of 128 bytes. Rows 32..63 live in the
    # sprite sheet's lower half (__gfx__ lines 64..127): a sprite-sheet line
    # is 64 bytes (128 pixels, one hex char each, low nibble first), so each
    # map row is TWO of them. (Until 2026-10-01 one 256-char line per row,
    # which PICO-8 truncated: rooms of map rows 2 and 3 were garbage here.)
    map_lines = [map_data[r * 128:(r + 1) * 128].hex() for r in range(32)]
    gfx_lines = ["0" * 128 for _ in range(64)]
    for r in range(32, 64):
        for half in range(2):
            part = map_data[r * 128 + half * 64:r * 128 + (half + 1) * 64]
            gfx_lines.append("".join(f"{b & 15:x}{b >> 4:x}" for b in part))
    assert len(gfx_lines) == 128 and all(len(l) == 128 for l in gfx_lines)
    gff_lines = [flags[:128].hex(), flags[128:].hex()]
    with open(out, "w") as f:
        f.write("pico-8 cartridge // http://www.pico-8.com\nversion 42\n__lua__\n")
        f.write("\n".join(prelude) + "\n")
        f.write(lua + "\n")
        f.write("\n".join(driver) + "\n")
        f.write("__gfx__\n" + "\n".join(gfx_lines) + "\n")
        f.write("__gff__\n" + "\n".join(gff_lines) + "\n")
        f.write("__map__\n" + "\n".join(map_lines) + "\n")


class NoCartOutput(RuntimeError):
    """PICO-8 ran the cart and it printed nothing (`run`)."""


def run(inputs, frames, keep=False, **kw):
    """Build the cart (`build_cart`'s keywords), run it on PICO-8 headless,
    return its `@P8@` lines."""
    pico8 = os.environ.get("PICO8", os.path.expanduser("~/pico-8/pico8"))
    work = tempfile.mkdtemp(prefix="celeste-replay-")
    cart = os.path.join(work, "replay.p8")
    build_cart(inputs, frames, cart, **kw)
    print(f"[replay] {len(inputs)} inputs, {frames} frames, cart {cart}", file=sys.stderr)
    try:
        proc = subprocess.run([pico8, "-x", cart], capture_output=True, text=True, timeout=600)
    except subprocess.TimeoutExpired as e:
        # PICO-8 halts on a cart it refuses ("program too large") and waits.
        sys.exit(f"[replay] pico8 timed out; it said: {(e.stdout or b'')[-500:]!r}")
    lines = [l[len("@P8@ "):] for l in proc.stdout.splitlines() if l.startswith("@P8@ ")]
    if not lines:
        # The driver always prints its `end` line: none means the cart did not
        # run to its end (a runtime error, a halt). Raised, not exited, so a
        # caller in the same process (category_runner, check_uploads) can
        # report it: a SystemExit passes `except Exception` and killed the
        # runner silently (gemskip100 2900m, 2026-10-09).
        raise NoCartOutput(f"no cart output ({cart}); pico8 said: {(proc.stdout[-500:] + proc.stderr[-500:]).strip()!r}")
    if not keep:
        os.remove(cart)
        os.rmdir(work)
    return lines


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("tas", nargs="?", help="witness file (comment lines start with #)")
    ap.add_argument("--inputs", help="comma-separated input bytes instead of a file")
    ap.add_argument("--frames", type=int, help="frames to run (default: number of inputs)")
    ap.add_argument("--keep", action="store_true", help="keep the generated cart")
    ap.add_argument("--lua", help="the game's Lua (default lua/celeste-minimal.lua)")
    ap.add_argument("--begin-game", action="store_true", help="call begin_game() after _init() (the original cart)")
    ap.add_argument("--balloon-seeds", help="comma-separated balloon offsets in creation order (the tasdatabase header), instead of rnd(1); missing ones are 0")
    ap.add_argument("--one-dash", action="store_true", help="rooms past the orb with ONE dash (the gemskip categories)")
    ap.add_argument("--room", help="start room \"x,y\" (minimal cart: replaces _init's load_room(1, 0); with --begin-game: begin_game's load_room(0,0))")
    ap.add_argument("--jank", type=int, help="the start room's loading frame from object J on (the jank model: a real transition's, Celia's IL load)")
    ap.add_argument("--dump", action="store_true", help="dump the start state (globals, every object's fields) before frame 1")
    ap.add_argument("--trace-objects", action="store_true", help="every object's position, remainder and phase on each frame line")
    args = ap.parse_args()
    if args.inputs:
        inputs = [int(x) for x in args.inputs.split(",")]
    elif args.tas:
        inputs = read_inputs(args.tas)
    else:
        ap.error("a witness file or --inputs is required")
    room = tuple(int(v) for v in args.room.split(",")) if args.room else None
    seeds = [float(v) for v in args.balloon_seeds.split(",") if v] if args.balloon_seeds is not None else None
    try:
        lines = run(inputs, args.frames or len(inputs), args.keep, lua_path=args.lua, begin_game=args.begin_game, room=room, balloon_seeds=seeds,
                     one_dash=args.one_dash, jank=args.jank, dump=args.dump, trace_objects=args.trace_objects)
    except NoCartOutput as e:
        sys.exit(f"[replay] {e}")
    for l in lines:
        if l != "end":
            print(l)


if __name__ == "__main__":
    main()
