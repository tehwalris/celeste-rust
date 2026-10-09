#!/usr/bin/env python3
"""A room's TAS in REAL PLAY on a real PICO-8: the original cart, played
through the real room transitions from the rooms before it, with the
community's tasdatabase files for those rooms.

    pico8_diff/chain.py CAT LEVEL FILE [--from L | --boot] [--modes] [--trace-objects]

Start modes (all on the original cart, all counting a room's frames from
the frame its player is created, as UniversalClassicTas and Celia do):
 1. IL: the room loaded directly (UCT's IL load; our search before
    2026-10-08);
 2. jank: the room loaded directly, then its LOADING FRAME as a real
    transition runs it - `_update`'s `foreach` goes on over the new room's
    objects from the leaving player's index J (PICO-8's `all` resumes
    there), one update each (move, then update), then the frame draws.
    Celia's IL load does this with J = the previous room's object count;
    here J is the one the chain measured;
 3. chain (the ground truth): the previous room (`--from L`: from level L;
    `--boot`: from 100m, the game's start) played with the database's files
    (the category's, else a fallback category's: FALLBACK), then FILE.
The default runs the chain from the previous level only. Prints each
segment's exit (frame, the room's count, the leaving player's index J, the
berry), and with --modes each mode's count and the room-entry state of
modes 1 and 2 diffed against the chain's, field by field.
"""
import argparse
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import replay  # noqa: E402

DB = os.path.expanduser("~/src/github.com/CelesteClassic/tasdatabase")
ORIG = os.path.expanduser("~/src/github.com/tehwalris/celeste_ocaml/celeste.lua")
# The files a category's full run plays where the category has none: the
# 100% categories play any% where a room has no berry; the gemskip ones play
# their parent category before the orb room (level 22).
FALLBACK = {
    "any": ["any"],
    "100": ["100", "any"],
    "nodiag": ["nodiag", "any"],
    "gemskipany": ["gemskipany", "any"],
    "gemskipnodiag": ["gemskipnodiag", "nodiag", "any"],
    "gemskip100": ["gemskip100", "gemskipany", "100", "any"],
    "mindashes": ["mindashes", "any"],
    "minjumps": ["minjumps", "any"],
    "key": ["key", "any"],
}
# Globals that only draw, sound or count time (pico8_diff/replay.py
# DUMP_GLOBALS): no object's update reads them.
COSMETIC = {"frames", "seconds", "minutes", "music_timer", "sfx_timer", "new_bg", "flash_bg", "deaths", "shake"}


def room_of(level):
    return ((level - 1) % 8, (level - 1) // 8)


def parse_tas(text):
    """A tasdatabase file: `[s1,s2,]i1,i2,...` (seeds optional)."""
    seeds = []
    if "[" in text:
        seeds = [float(v) for v in text[text.index("[") + 1:text.index("]")].split(",") if v.strip()]
        text = text[text.index("]") + 1:]
    return seeds, [int(x) for x in re.findall(r"\d+", text)]


def db_file(cat, level):
    """The database file a full run of `cat` plays in `level`, and its category."""
    db = json.load(open(f"{DB}/database.json"))["classic"]
    for c in FALLBACK[cat]:
        for e in db.get(c, []):
            if e.get("file") == f"TAS{level}.tas" and os.path.exists(f"{DB}/classic/{c}/{e['file']}"):
                return c, f"{DB}/classic/{c}/{e['file']}"
    raise SystemExit(f"chain.py: no database file for level {level} in {FALLBACK[cat]}")


def parse(lines):
    exits = [dict(zip(l.split()[::2], l.split()[1::2])) for l in lines if l.startswith("exit seg ")]
    bad = [l for l in lines if l.startswith(("death ", "unexpected "))]
    dump = [l[len("dump entry "):] for l in lines if l.startswith("dump entry ")]
    return exits, bad, dump


def fields(line):
    """`obj 2 fly_fruit k=v ...` / `globals k v ...` as (head, {field: value})."""
    w = line.split()
    if w[0] == "globals":
        return "globals", dict(zip(w[1::2], w[2::2]))
    return f"{w[0]} {w[1]} {w[2]}", dict(kv.split("=", 1) for kv in w[3:])


def diff_dumps(a, b):
    """The fields that differ between two room-entry dumps (b: the reference)."""
    da, db_ = dict(fields(l) for l in a), dict(fields(l) for l in b)
    out = []
    for head in sorted(set(da) | set(db_)):
        if head not in da or head not in db_:
            out.append(f"{head}: only in {'reference' if head in db_ else 'this mode'}")
            continue
        for k in sorted(set(da[head]) | set(db_[head])):
            if da[head].get(k) != db_[head].get(k):
                out.append(f"{head} {k}: {da[head].get(k)} vs {db_[head].get(k)}")
    return out


def chain(cat, level, seeds, inputs, first, trace_objects=False, dump=True):
    """Play levels first..level-1 with the database's files, then (seeds,
    inputs) in `level`; the PICO-8 output lines."""
    segs = []
    for l in range(first, level):
        _, path = db_file(cat, l)
        sd, ins = parse_tas(open(path).read())
        segs.append((room_of(l), sd, ins))
    segs.append((room_of(level), seeds, inputs))
    frames = sum(len(s[2]) + 80 for s in segs)
    # From 100m it is the game's boot: the title screen's `_update` calls
    # begin_game AFTER its objects' updates, then the frame draws (the room
    # title's first tick) - a loading frame that updates no object.
    return replay.run(inputs, frames, lua_path=ORIG, begin_game=True, room=room_of(first) if first > 1 else None,
                      one_dash=cat.startswith("gemskip"), segments=segs, dump=dump, trace_objects=trace_objects, load_draw=first == 1)


def single(cat, level, seeds, inputs, jank=None, trace_objects=False):
    """The room alone (mode 1), or with its loading frame from object J (mode 2)."""
    return replay.run(inputs, len(inputs) + 80, lua_path=ORIG, begin_game=True, room=room_of(level), one_dash=cat.startswith("gemskip"),
                      segments=[(room_of(level), seeds, inputs)], jank=jank, dump=True, trace_objects=trace_objects)


def summary(name, lines, nsegs=1):
    """The last segment's result: its exit line (None: no exit)."""
    exits, bad, _ = parse(lines)
    last = exits[nsegs - 1] if len(exits) >= nsegs else None
    s = f"{name}: "
    if bad:
        # A death in an earlier segment: a database file that fails there.
        early = [b for b in bad if b.startswith("death seg ") and int(b.split()[2]) < nsegs]
        s += ("PREVIOUS ROOM FAILED (" if early else "FAILED (") + "; ".join(bad) + ")"
    if last:
        s += f"exit {last['count']}f berry {last['berry']}"
    elif not bad:
        s += "NO EXIT"
    return s, last


def entry_jank(cat, level):
    """The loading-frame index J real play enters `level` with: the leaving
    player's index in the category's boot chain (its database files from
    100m). A database file that dies on PICO-8, or never exits there, breaks
    the chain: it restarts at the level after it (an IL load). None for 100m (the boot's loading
    frame updates no object)."""
    if level == 1:
        return None
    first = 1
    while True:
        exits, bad, _ = parse(chain(cat, level, [], [0], first, dump=False))
        if len(exits) >= level - first:
            return int(exits[level - first - 1]["player_idx"])
        dead = [int(b.split()[2]) for b in bad if b.startswith("death seg ")]
        # A file that dies, or that never exits on PICO-8 (it finishes only
        # in UCT/Celia, which compute in doubles), breaks the chain there.
        stuck = len(exits) if not bad and len(exits) < level - first else None
        if not dead and stuck is None:
            raise SystemExit(f"chain.py: the {cat} boot chain to level {level} failed: {bad}")
        first += dead[0] if dead else stuck + 1
        if first >= level:
            # The previous room's file is what breaks: `level` is IL-loaded.
            return None


def check_start(cat, level, binary):
    """The SEARCH's start state for `level` (`concrete_run --dump-start`
    under CELESTE_LOADING_JANK = the boot chain's J) against the boot
    chain's room entry on a real PICO-8, field by field: the objects both
    carts have (the minimal one lacks room_title, message and flag), the
    fields both have. Returns (J, differences, fields only one side has)."""
    import subprocess
    j = entry_jank(cat, level)
    _, _, ref = parse(chain(cat, level, [], [0], 1))
    rx, ry = room_of(level)
    env = dict(os.environ, CELESTE_START_ROOM=f"{rx},{ry}", CELESTE_LOADING_JANK=str(j) if j else "none", CELESTE_BALLOON_SEEDS="0")
    if cat.startswith("gemskip"):
        env["CELESTE_GEMSKIP"] = "1"
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    ours = subprocess.run([binary, "-i", "0", "--dump-start"], capture_output=True, text=True, env=env, cwd=root, check=True).stdout.splitlines()
    skip = ("room_title", "message", "flag")
    pico = [fields(l) for l in ref if l.startswith("obj ") and l.split()[2] not in skip]
    mine = [fields(l) for l in ours if l.startswith("obj ")]
    diffs, only = [], set()
    if [h.split()[2] for h, _ in pico] != [h.split()[2] for h, _ in mine]:
        diffs.append(f"object lists differ: {[h for h, _ in pico]} vs {[h for h, _ in mine]}")
    for (hp, fp), (hm, fm) in zip(pico, mine):
        for k in sorted(set(fp) | set(fm)):
            if k in fp and k in fm:
                if fp[k] != fm[k]:
                    diffs.append(f"{hm} {k}: search {fm[k]} vs PICO-8 {fp[k]}")
            else:
                only.add(f"{hm.split()[2]}.{k} ({'PICO-8' if k in fp else 'search'} only)")
    return j, diffs, sorted(only)


# Object tiles whose object a play can destroy before the exit (a berry
# taken, a fly fruit taken or flown off, a key taken, a chest opened, a fake
# wall broken); the spawn is always replaced by the player.
DESTROYABLE = {26, 28, 8, 20, 64}
# The player's speed bound, px a frame (the search's region asserts it).
MAX_SPEED = 6


def room_objects(room):
    """The object tiles of a room in load_room's creation order (the original cart's)."""
    m = replay.hexbytes(os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "cart", "map-data.txt"), 8192)
    return [t for t in (m[(room[1] * 16 + ty) * 128 + room[0] * 16 + tx] for tx in range(16) for ty in range(16)) if t in replay.OBJECT_TILES]


def celia_jank(level):
    """The J Celia's IL load uses (cctas.lua `load_level`, offset 0): the
    previous room's object count at its load, the title left out - as if the
    player left it with nothing destroyed and the title gone. It differs from
    the boot chain's where the previous room's play destroys objects (any%
    levels 5, 8, 13, 18; most 100% levels after a berry): both are checked."""
    return None if level == 1 else len(room_objects(room_of(level - 1)))


def feasible_janks(level):
    """Every J a play of the previous room can enter `level` with - a
    SOUND over-approximation of the history the room-entry state depends on.
    J = 1 + the objects before the player when it leaves: the previous
    room's objects but the spawn, less any subset of the destroyable ones,
    plus the room title if it is still up (it is drawn away on the 36th frame
    after the load) and the spawn's smoke (15 frames, 6 of them before the
    player exists) if they can be: only when the exit can come that soon,
    bounded by the spawn's own frames and the climb at MAX_SPEED."""
    prev = room_of(level - 1)
    m = replay.hexbytes(os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "cart", "map-data.txt"), 8192)
    tiles = [(m[(prev[1] * 16 + ty) * 128 + prev[0] * 16 + tx], ty) for tx in range(16) for ty in range(16)]
    objs = [t for t, _ in tiles if t in replay.OBJECT_TILES]
    spawn_y = next(ty * 8 for t, ty in tiles if t == 1)
    k, d = len(objs), sum(t in DESTROYABLE for t in objs)
    # The frame the player is created on, after an IL load (no input reaches the spawn).
    lines = single("any", level - 1, [], [0])
    start = min(int(l.split()[0][1:]) for l in lines if l.startswith("f") and " player " in l)
    climb = -(-(spawn_y + 5) // MAX_SPEED)
    # start - 1: in real play the previous room's own loading frame may have
    # updated its spawn too (a frame ahead).
    title = [0, 1] if (start - 1) + climb - 1 <= 36 else [0]
    smoke = [0, 1] if climb <= 10 else [0]
    return sorted({1 + (k - 1 - x) + t + sm for x in range(d + 1) for t in title for sm in smoke})


def start_classes(level, binary, cat="any"):
    """The feasible J's grouped by the search's start state they give
    (`concrete_run --dump-start`): one class per distinct room entry."""
    import subprocess
    rx, ry = room_of(level)
    classes = {}
    for j in feasible_janks(level):
        env = dict(os.environ, CELESTE_START_ROOM=f"{rx},{ry}", CELESTE_LOADING_JANK=str(j), CELESTE_BALLOON_SEEDS="0")
        if cat.startswith("gemskip"):
            env["CELESTE_GEMSKIP"] = "1"
        root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        dump = subprocess.run([binary, "-i", "0", "--dump-start"], capture_output=True, text=True, env=env, cwd=root, check=True).stdout
        classes.setdefault(dump, []).append(j)
    return list(classes.values())


def vary(level, cats):
    """The room-entry state of `level` after each category's boot chain
    (different plays of the rooms before it): (per category: J or the
    failure, dump), and the fields that differ between the categories that
    got there."""
    runs = {}
    for c in cats:
        try:
            lines = chain(c, level, [], [0], 1)
        except SystemExit as e:
            runs[c] = (f"no files ({e})", None)
            continue
        exits, bad, dump = parse(lines)
        runs[c] = (f"J {exits[level - 2]['player_idx']} of {exits[level - 2]['of']}" if len(exits) >= level - 1 else f"failed: {bad}", dump if len(exits) >= level - 1 else None)
    got = {c: d for c, (_, d) in runs.items() if d}
    differ = {}
    for c, d in got.items():
        for line in diff_dumps(d, next(iter(got.values()))):
            differ.setdefault(line.split(":")[0], set()).add(c)
    return runs, differ


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("cat", choices=sorted(FALLBACK))
    ap.add_argument("level", type=int)
    ap.add_argument("file", help="the room's TAS (tasdatabase format)")
    g = ap.add_mutually_exclusive_group()
    g.add_argument("--from", dest="first", type=int, help="the chain's first level (default: the previous one)")
    g.add_argument("--boot", action="store_true", help="the chain from 100m (the game's start)")
    ap.add_argument("--modes", action="store_true", help="also the IL and jank modes, their entry states diffed against the chain's")
    ap.add_argument("--trace-objects", action="store_true", help="print the last room's frame lines with every object")
    ap.add_argument("--check-start", metavar="CONCRETE_RUN", help="(FILE ignored) the search's start state (this concrete_run binary's --dump-start) against the boot chain's room entry, field by field")
    ap.add_argument("--feasible", metavar="CONCRETE_RUN", help="(FILE ignored) every J a play of the previous room can enter with, grouped by the start state they give")
    ap.add_argument("--entry-jank", action="store_true", help="(FILE ignored) print the J the category's boot chain enters the level with")
    ap.add_argument("--vary", action="store_true", help="(FILE ignored) the room's entry state after every category's boot chain, and the fields that differ")
    a = ap.parse_args()
    if a.check_start:
        j, diffs, only = check_start(a.cat, a.level, a.check_start)
        print(f"{a.cat} level {a.level}: J {j}: " + ("start state MATCHES the boot chain's entry" if not diffs else f"{len(diffs)} DIFFERENCES"))
        for d in diffs:
            print("   ", d)
        if only:
            print("    (not compared, one cart only: " + ", ".join(only) + ")")
        sys.exit(1 if diffs else 0)
    if a.feasible:
        print(f"level {a.level}: feasible J {feasible_janks(a.level)}, start classes {start_classes(a.level, a.feasible, a.cat)}")
        return
    if a.entry_jank:
        print(entry_jank(a.cat, a.level))
        return
    if a.vary:
        runs, differ = vary(a.level, [c for c in FALLBACK if c != "gemskip100"])
        for c, (st, _) in runs.items():
            print(f"{c}: {st}")
        for field, cs in sorted(differ.items()):
            kind = "cosmetic" if field.startswith("globals ") and field.split()[1] in COSMETIC else "STATE"
            print(f"varies ({kind}): {field}  [{', '.join(sorted(cs))} differ from {next(c for c, (_, d) in runs.items() if d)}]")
        return
    seeds, inputs = parse_tas(open(a.file).read())
    first = 1 if a.boot else (a.first or a.level - 1)
    lines = chain(a.cat, a.level, seeds, inputs, first, a.trace_objects)
    exits, bad, ref = parse(lines)
    for l in lines:
        if l.startswith(("exit ", "death ", "unexpected ")) or (a.trace_objects and l.startswith("f")):
            print(l)
    n = a.level - first + 1
    s, last = summary(f"chain from level {first}", lines, n)
    # The jank the last room was entered with: the leaving player's index.
    j = exits[n - 2]["player_idx"] if n >= 2 and len(exits) >= n - 1 else None
    print(s + (f" (entered with J {j})" if j else ""))
    if a.modes:
        il = single(a.cat, a.level, seeds, inputs)
        print(summary("IL", il)[0])
        for d in diff_dumps(parse(il)[2], ref):
            print("   IL entry differs:", d)
        if j:
            jk = single(a.cat, a.level, seeds, inputs, int(j))
            print(summary(f"jank J={j}", jk)[0])
            for d in diff_dumps(parse(jk)[2], ref):
                print(f"   jank entry differs:", d)


if __name__ == "__main__":
    main()
