#!/usr/bin/env python3
"""A PICO-8 text cartridge (.p8): its map, sprite flags and Lua, for a MAP
MOD of Celeste Classic (plans/map-mod.md). The same reading as the search's
`--cart` (crates/celeste-core cart_data.rs `CartData::from_p8`): `__map__`
holds map rows 0-31; rows 32-63 are the sprite sheet's lower half, `__gfx__`
lines 64-127, 64 bytes a line as 128 pixels LOW nibble first (two lines a
map row); `__gff__` the 256 sprite flags. Absent lines are zero.

    pico8_diff/p8cart.py extract MOD.p8 DIR    # DIR/map-data.txt, flag-data.txt, game.lua
    pico8_diff/p8cart.py rooms MOD.p8 [--show X,Y]   # per room: objects, spawn, Celia's J, changed?
    pico8_diff/p8cart.py luadiff MOD.p8 [--orig FILE]  # the mod's Lua against the original's, per function
    pico8_diff/p8cart.py check MOD.p8           # all of it, short: what differs from the original

`--orig` defaults to the original cart's Lua (celeste_ocaml/celeste.lua, the
Lua of UniversalClassicTas's carts/celeste.p8).
"""
import argparse
import difflib
import hashlib
import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ORIG_LUA = os.path.expanduser("~/src/github.com/tehwalris/celeste_ocaml/celeste.lua")

# The tiles the ORIGINAL cart's load_room makes an object of, by name
# (replay.py OBJECT_TILES, src/trace/cart.rs OBJECT_TILES).
OBJECT_NAMES = {1: "spawn", 8: "key", 11: "platform_l", 12: "platform_r", 18: "spring", 20: "chest", 22: "balloon", 23: "fall_floor", 26: "fruit", 28: "fly_fruit", 64: "fake_wall", 96: "big_chest", 86: "message", 118: "flag"}
SPIKES = {17, 27, 43, 59}


def sections(text):
    if not text.startswith("pico-8 cartridge"):
        raise SystemExit("not a PICO-8 text cartridge (.p8): the first line is not `pico-8 cartridge ...` (a .p8.png must be saved as .p8 first)")
    secs, cur = {}, None
    for line in text.split("\n"):
        line = line.rstrip("\r")
        if len(line) > 4 and line.startswith("__") and line.endswith("__"):
            cur = line[2:-2]
            secs[cur] = []
            continue
        if cur is not None:
            secs[cur].append(line)
    return secs


def _rows(secs, name, most, width):
    lines = [l for l in secs.get(name, []) if l.strip()]
    if len(lines) > most:
        raise SystemExit(f"__{name}__: {len(lines)} lines, at most {most}")
    out = []
    for i, l in enumerate(lines):
        if len(l) != 2 * width:
            raise SystemExit(f"__{name}__ line {i}: {len(l)} hex digits, expected {2 * width}")
        out.append(bytes.fromhex(l))
    return out


class Cart:
    def __init__(self, path):
        self.path = path
        raw = open(path, "rb").read()
        self.sha256 = hashlib.sha256(raw).hexdigest()
        secs = sections(raw.decode("utf-8"))
        self.lua = "\n".join(secs.get("lua", []))
        m = bytearray(8192)
        for r, row in enumerate(_rows(secs, "map", 32, 128)):
            m[r * 128:(r + 1) * 128] = row
        for i, row in enumerate(_rows(secs, "gfx", 128, 64)):
            if i >= 64:
                at = 32 * 128 + (i - 64) * 64
                m[at:at + 64] = bytes(((b & 15) << 4) | (b >> 4) for b in row)
        self.map = bytes(m)
        f = bytearray(256)
        for r, row in enumerate(_rows(secs, "gff", 2, 128)):
            f[r * 128:(r + 1) * 128] = row
        self.flags = bytes(f)

    def tile(self, x, y):
        return self.map[y * 128 + x] if 0 <= x < 128 and 0 <= y < 64 else 0

    def room_tiles(self, room):
        """(tile, tx, ty) in load_room's creation order (x outer, y inner)."""
        rx, ry = room
        return [(self.tile(rx * 16 + tx, ry * 16 + ty), tx, ty) for tx in range(16) for ty in range(16)]


def original_cart_files():
    def hexbytes(p, n):
        h = re.sub(r"\s", "", open(p).read())
        assert len(h) == 2 * n, p
        return bytes.fromhex(h)
    return hexbytes(os.path.join(ROOT, "cart", "map-data.txt"), 8192), hexbytes(os.path.join(ROOT, "cart", "flag-data.txt"), 256)


def write_hex(path, data):
    with open(path, "w") as f:
        for i in range(0, len(data), 128):
            f.write(data[i:i + 128].hex() + "\n")


def rooms_table(cart, show=None):
    om, of = original_cart_files()
    orig = Cart.__new__(Cart)
    orig.map, orig.flags = om, of
    print(f"# {cart.path} sha256 {cart.sha256}")
    print(f"# flags (__gff__): {'identical to the original' if cart.flags == of else 'DIFFERENT: ' + ', '.join(f'sprite {i}: {of[i]:#04x}->{cart.flags[i]:#04x}' for i in range(256) if cart.flags[i] != of[i])}")
    print("# level room   map   spawn    objects (load_room order)                              celia_J")
    prev = None
    for lvl in range(32):
        room = (lvl % 8, lvl // 8)
        tiles = cart.room_tiles(room)
        same = tiles == orig.room_tiles(room)
        objs = [OBJECT_NAMES[t] for t, _, _ in tiles if t in OBJECT_NAMES]
        spawns = [(tx * 8, ty * 8) for t, tx, ty in tiles if t == 1]
        # Celia's IL load (cctas.lua load_level, offset 0): the objects from
        # index = the previous room's object count (its title left out) on
        # get the loading frame; level 1 none (`--loading-jank none`).
        j = "none" if prev is None else str(prev)
        prev = len(objs)
        counts = {}
        for o in objs:
            counts[o] = counts.get(o, 0) + 1
        desc = " ".join(f"{k}{'x' + str(v) if v > 1 else ''}" for k, v in counts.items())
        print(f"{lvl + 1:5d} {room[0]},{room[1]}   {'same ' if same else 'MOD  '} {str(spawns[0]) if len(spawns) == 1 else str(spawns):9s} {desc:55s} {j}")
    if show:
        rx, ry = show
        print(f"\n# room ({rx},{ry}) tiles (hex; .. empty, ## solid by flag 0, ^^ spike):")
        for ty in range(16):
            row = []
            for tx in range(16):
                t = cart.tile(rx * 16 + tx, ry * 16 + ty)
                row.append(".." if t == 0 else ("^^" if t in SPIKES else (f"{t:02x}" if t in OBJECT_NAMES else ("##" if cart.flags[t] & 1 else f"{t:02x}"))))
            print(" ".join(row))


# ---- the Lua diff -----------------------------------------------------------

TOKEN = re.compile(r"""
    (?P<ws>[ \t\r]+) | (?P<nl>\n)
  | (?P<lcomment>--\[(?P<eq>=*)\[.*?\](?P=eq)\])
  | (?P<comment>--[^\n]*) | (?P<comment2>//[^\n]*)
  | (?P<lstr>\[(?P<eq2>=*)\[.*?\](?P=eq2)\])
  | (?P<str>"(?:\\.|[^"\\\n])*"|'(?:\\.|[^'\\\n])*')
  | (?P<num>0[xX][0-9a-fA-F]*(?:\.[0-9a-fA-F]*)?|0[bB][01]*(?:\.[01]*)?|\d+\.?\d*(?:[eE][+-]?\d+)?|\.\d+(?:[eE][+-]?\d+)?)
  | (?P<name>[A-Za-z_][A-Za-z0-9_]*)
  | (?P<op>\.\.\.|\.\.=|\.\.|==|~=|!=|<=|>=|>>>|>>|<<|\+=|-=|\*=|/=|%=|\^=|\\=|\|=|&=|\^\^=|\^\^|//|[-+*/%^#&|~<>=(){}\[\];:,.\\@$?!])
  | (?P<other>.)
""", re.S | re.X)


def num_value(t):
    t = t.lower()
    try:
        if t.startswith("0x"):
            whole, _, frac = t[2:].partition(".")
            return int(whole or "0", 16) + (int(frac, 16) / 16 ** len(frac) if frac else 0)
        if t.startswith("0b"):
            whole, _, frac = t[2:].partition(".")
            return int(whole or "0", 2) + (int(frac, 2) / 2 ** len(frac) if frac else 0)
        return float(t)
    except ValueError:
        return t


def tokens(src):
    """(token, line) with comments and whitespace dropped and the token-saving
    spellings normalized: `f""` -> `f()`, `f"x"` -> `f("x")`, `!=` -> `~=`,
    numbers by value."""
    out, line = [], 1
    for m in TOKEN.finditer(src):
        k = m.lastgroup
        s = m.group(0)
        if k == "nl":
            line += 1
            continue
        if k in ("ws", "comment", "comment2", "lcomment"):
            line += s.count("\n")
            continue
        if k == "num":
            v = num_value(s)
            s = repr(float(v)) if isinstance(v, (int, float)) else s
        elif s == "!=":
            s = "~="
        elif k == "lstr":
            line += s.count("\n")
        out.append([s, line, k])
    # f"str" / f[[str]] / f{...} call sugar: `name "x"` -> `name ( "x" )`.
    norm = []
    for i, (s, ln, k) in enumerate(out):
        prev = norm[-1] if norm else None
        if k in ("str", "lstr") and prev and (prev[2] == "name" and prev[0] not in LUA_KEYWORDS or prev[0] in (")", "]")):
            # `f""`: PICO-8's token-saving call with no argument used (a function that takes none).
            norm += [["(", ln, "op"]] + ([] if s in ('""', "''") else [[s, ln, k]]) + [[")", ln, "op"]]
        else:
            norm.append([s, ln, k])
    return norm


LUA_KEYWORDS = {"and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if", "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"}


def constants(toks):
    """Top-level `name=number` assigned once and never assigned again (the
    original's k_left.. button constants: a mod often inlines them)."""
    seen = {}
    depth = 0
    for i, (s, _, k) in enumerate(toks):
        if k == "name" and i + 2 < len(toks) and toks[i + 1][0] == "=" and (i == 0 or toks[i - 1][0] not in (".", ",", "local")):
            seen.setdefault(s, []).append((depth, toks[i + 2]))
    return {n: v[0][1][0] for n, v in seen.items() if len(v) == 1 and v[0][1][2] == "num" and n.startswith("k_")}


def substitute(toks, consts):
    out = []
    for i, t in enumerate(toks):
        nxt = toks[i + 1][0] if i + 1 < len(toks) else None
        if t[2] == "name" and t[0] in consts and nxt != "=" and (not out or out[-1][0] != "."):
            out.append([consts[t[0]], t[1], "num"])
        else:
            out.append(t)
    return out


def chunks(toks):
    """{name: tokens}: each function (top-level `function a.b(...)`, `local
    function f`, `x = function`, an object type's `init=function(this)` as
    `type.init`), and `<toplevel>` for everything outside them."""
    res = {}
    top = []
    tables = []      # names of the table constructors we are inside (by brace)
    braces = 0
    i, n = 0, len(toks)
    while i < n:
        s = toks[i][0]
        name = None
        if s == "function":
            j = i + 1
            if j < n and toks[j][2] == "name":
                parts = [toks[j][0]]
                j += 1
                while j + 1 < n and toks[j][0] in (".", ":") and toks[j + 1][2] == "name":
                    parts.append(toks[j + 1][0])
                    j += 2
                name = ".".join(parts)
            else:
                # `key = function` / `key=function` in a table or an assignment.
                if i >= 2 and toks[i - 1][0] == "=" and toks[i - 2][2] == "name":
                    k = i - 2
                    parts = [toks[k][0]]
                    while k >= 2 and toks[k - 1][0] == "." and toks[k - 2][2] == "name":
                        parts.insert(0, toks[k - 2][0])
                        k -= 2
                    owner = [t for t in tables if t]
                    name = ".".join(([owner[-1]] if owner and len(parts) == 1 else []) + parts)
                else:
                    name = f"<anonymous@{toks[i][1]}>"
            end = block_end(toks, i)
            base, k = name, 2
            while name in res:
                name = f"{base}#{k}"
                k += 1
            res[name] = toks[i:end]
            if top and top[-1][0] == "=" and len(top) >= 2:
                pass
            top.append([f"<function {name}>", toks[i][1], "fn"])
            i = end
            continue
        if s == "{":
            owner = toks[i - 2][0] if i >= 2 and toks[i - 1][0] == "=" and toks[i - 2][2] == "name" else None
            tables.append(owner)
        elif s == "}" and tables:
            tables.pop()
        top.append(toks[i])
        i += 1
    res["<toplevel>"] = top
    return res


def block_end(toks, i):
    """The index past the `end` closing the `function` at i (blocks: function,
    do, then-if, repeat..until; PICO-8's one-line `if (c) stmt` opens none)."""
    depth = 0
    n = len(toks)
    j = i
    while j < n:
        s, ln, k = toks[j]
        if k == "name":
            if s in ("function", "do", "repeat"):
                depth += 1
            elif s == "if":
                # `if ... then` opens a block; PICO-8's shorthand `if (c) x` on one line does not.
                m = j + 1
                opened = False
                while m < n and toks[m][1] == ln:
                    if toks[m][0] == "then":
                        opened = True
                        break
                    m += 1
                if not opened and m < n and toks[m][0] == "then":
                    opened = True  # `then` on a later line
                if opened:
                    depth += 1
            elif s in ("end", "until"):
                depth -= 1
                if depth == 0:
                    return j + 1
        j += 1
    return n


def luadiff(mod_lua, orig_lua, verbose=True):
    """Prints the per-function comparison; returns the changed/added/removed names."""
    ot, mt = tokens(orig_lua), tokens(mod_lua)
    consts = constants(ot)
    ot, mt = substitute(ot, consts), substitute(mt, consts)
    oc, mc = chunks(ot), chunks(mt)
    changed = [k for k in oc if k in mc and [t[0] for t in oc[k]] != [t[0] for t in mc[k]]]
    removed = [k for k in oc if k not in mc]
    added = [k for k in mc if k not in oc]
    same = [k for k in oc if k in mc and k not in changed]
    print(f"# Lua: {len(same)} functions identical (after normalizing whitespace, comments, `f\"\"` calls, number spellings, the k_* button constants), {len(changed)} changed, {len(removed)} removed, {len(added)} added")
    for k in changed:
        a, b = [t[0] for t in oc[k]], [t[0] for t in mc[k]]
        print(f"\n## CHANGED {k} (original line {oc[k][0][1] if oc[k] else '?'}, mod line {mc[k][0][1] if mc[k] else '?'})")
        if verbose:
            sm = difflib.SequenceMatcher(a=a, b=b, autojunk=False)
            for op, a0, a1, b0, b1 in sm.get_opcodes():
                if op == "equal":
                    continue
                ctx_a = " ".join(a[max(0, a0 - 6):a0])
                print(f"   ... {ctx_a}  [-{' '.join(a[a0:a1])}-] {{+{' '.join(b[b0:b1])}+}}")
    for k in removed:
        print(f"\n## REMOVED {k} (original line {oc[k][0][1] if oc[k] else '?'})")
    for k in added:
        print(f"\n## ADDED {k} (mod line {mc[k][0][1] if mc[k] else '?'})")
        if verbose:
            print("   " + " ".join(t[0] for t in mc[k])[:2000])
    return changed, removed, added


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    e = sub.add_parser("extract", help="the map, the flags (cart/ format) and the Lua of a .p8")
    e.add_argument("p8")
    e.add_argument("dir")
    r = sub.add_parser("rooms", help="per room: changed?, spawn, objects, Celia's loading-frame J")
    r.add_argument("p8")
    r.add_argument("--show", help="print room X,Y's tiles")
    d = sub.add_parser("luadiff", help="the mod's Lua against the original's, per function")
    d.add_argument("p8")
    d.add_argument("--orig", default=ORIG_LUA)
    d.add_argument("--brief", action="store_true", help="names only")
    c = sub.add_parser("check", help="rooms + luadiff --brief")
    c.add_argument("p8")
    c.add_argument("--orig", default=ORIG_LUA)
    a = ap.parse_args()
    cart = Cart(a.p8)
    if a.cmd == "extract":
        os.makedirs(a.dir, exist_ok=True)
        write_hex(os.path.join(a.dir, "map-data.txt"), cart.map)
        write_hex(os.path.join(a.dir, "flag-data.txt"), cart.flags)
        open(os.path.join(a.dir, "game.lua"), "w").write(cart.lua + "\n")
        om, of = original_cart_files()
        print(f"{a.dir}: map {'= original' if cart.map == om else 'differs from the original'}, flags {'= original' if cart.flags == of else 'differ from the original'}, Lua {len(cart.lua.splitlines())} lines; sha256 {cart.sha256}")
    elif a.cmd == "rooms":
        rooms_table(cart, tuple(int(v) for v in a.show.split(",")) if a.show else None)
    elif a.cmd == "luadiff":
        luadiff(cart.lua, open(a.orig).read(), verbose=not a.brief)
    elif a.cmd == "check":
        rooms_table(cart)
        print()
        luadiff(cart.lua, open(a.orig).read(), verbose=False)


if __name__ == "__main__":
    main()
