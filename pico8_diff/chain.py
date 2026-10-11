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
    (the category's, else a fallback category's; our own file where the
    database's never exits on PICO-8: `celeste.tasdb.chain_file`), then FILE.
The default runs the chain from the previous level only. Prints each
segment's exit (frame, the room's count, the leaving player's index J, the
berry), and with --modes each mode's count and the room-entry state of
modes 1 and 2 diffed against the chain's, field by field.
"""
import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import replay  # noqa: E402
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "tools"))
from celeste import categories, rooms, tasdb  # noqa: E402
from celeste.pico8 import ORIG  # noqa: E402

# Globals that only draw, sound or count time (pico8_diff/replay.py
# DUMP_GLOBALS): no object's update reads them.
COSMETIC = {"frames", "seconds", "minutes", "music_timer", "sfx_timer", "new_bg", "flash_bg", "deaths", "shake"}
room_of = rooms.room_of


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
        _, path = tasdb.chain_file(cat, l)
        sd, ins = tasdb.parse(open(path).read())
        segs.append((room_of(l), sd, ins))
    segs.append((room_of(level), seeds, inputs))
    frames = sum(len(s[2]) + 80 for s in segs)
    # From 100m it is the game's boot: the title screen's `_update` calls
    # begin_game AFTER its objects' updates, then the frame draws (the room
    # title's first tick) - a loading frame that updates no object.
    return replay.run(inputs, frames, lua_path=ORIG, begin_game=True, room=room_of(first) if first > 1 else None,
                      one_dash=categories.one_dash(cat), segments=segs, dump=dump, trace_objects=trace_objects, load_draw=first == 1)


def single(cat, level, seeds, inputs, jank=None, trace_objects=False):
    """The room alone (mode 1), or with its loading frame from object J (mode 2)."""
    return replay.run(inputs, len(inputs) + 80, lua_path=ORIG, begin_game=True, room=room_of(level), one_dash=categories.one_dash(cat),
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
    elif not bad and len(exits) < nsegs - 1:
        # An earlier segment's database file never exits on PICO-8 (it
        # finishes only in UCT/Celia, which compute in doubles): this room
        # is never reached.
        s += f"PREVIOUS ROOM DID NOT EXIT (segment {len(exits) + 1} of {nsegs})"
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
    under --loading-jank = the boot chain's J) against the boot
    chain's room entry on a real PICO-8, field by field: the objects both
    carts have (the minimal one lacks room_title, message and flag), the
    fields both have. Returns (J, differences, fields only one side has)."""
    import subprocess
    j = entry_jank(cat, level)
    _, _, ref = parse(chain(cat, level, [], [0], 1))
    rx, ry = room_of(level)
    flags = ["--room", f"{rx},{ry}", "--loading-jank", str(j) if j else "none", "--balloon-seeds", "0"] + (["--gemskip"] if categories.one_dash(cat) else [])
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    ours = subprocess.run([os.path.join(root, "safe-run.sh"), "--", binary, "-i", "0", "--dump-start"] + flags, capture_output=True, text=True, cwd=root, check=True).stdout.splitlines()
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
    ap.add_argument("cat", choices=sorted(categories.FALLBACK))
    ap.add_argument("level", type=int)
    ap.add_argument("file", help="the room's TAS (tasdatabase format)")
    g = ap.add_mutually_exclusive_group()
    g.add_argument("--from", dest="first", type=int, help="the chain's first level (default: the previous one)")
    g.add_argument("--boot", action="store_true", help="the chain from 100m (the game's start)")
    ap.add_argument("--modes", action="store_true", help="also the IL and jank modes, their entry states diffed against the chain's")
    ap.add_argument("--trace-objects", action="store_true", help="print the last room's frame lines with every object")
    ap.add_argument("--check-start", metavar="CONCRETE_RUN", help="(FILE ignored) the search's start state (this concrete_run binary's --dump-start) against the boot chain's room entry, field by field")
    ap.add_argument("--feasible", metavar="REWRITE", help="(FILE ignored) every J a play of the previous room can enter with, grouped by the start state they give (this rewrite binary's `starts`)")
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
        st = rooms.starts(room_of(a.level), categories.search_flags(a.cat) + ["--balloon-seeds", "0"], a.feasible)
        print(f"level {a.level}: feasible J {st['feasible']}, start classes {st['classes']}, Celia's J {st['celia']}")
        return
    if a.entry_jank:
        print(entry_jank(a.cat, a.level))
        return
    if a.vary:
        runs, differ = vary(a.level, [c for c in categories.FALLBACK if c != "gemskip100"])
        for c, (st, _) in runs.items():
            print(f"{c}: {st}")
        for field, cs in sorted(differ.items()):
            kind = "cosmetic" if field.startswith("globals ") and field.split()[1] in COSMETIC else "STATE"
            print(f"varies ({kind}): {field}  [{', '.join(sorted(cs))} differ from {next(c for c, (_, d) in runs.items() if d)}]")
        return
    seeds, inputs = tasdb.parse(open(a.file).read())
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
