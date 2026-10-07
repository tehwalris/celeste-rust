#!/usr/bin/env python3
"""The timing windows of `rewrite playable --shifts FILE`, in the ORIGINAL
cart on a real PICO-8 (pico8_diff/replay.py, as tools/category_runner.py
replays a witness).

    tools/playable_orig.py SHIFTS ROOM CAT SEEDS OPT [--jobs N]

SHIFTS: `name event shift inputs` per line (`name base 0 inputs` first per
name). A sequence plays right when the room changes at frame OPT, as the
search's witness does. The windows the search measures are the minimal
cart's (lua/celeste-minimal.lua); the original also buffers a jump press for
4 frames (`jbuffer`), so its window can be wider for a jump press - and an
edge that does nothing in the minimal cart can fire there. This prints, per
event, both carts' windows where they differ.
"""
import argparse, os, sys
from concurrent.futures import ThreadPoolExecutor

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import category_runner as cr

BUTTONS = ["left", "right", "up", "down", "jump", "dash"]
CAP = 8


def events(x):
    out = []
    for i in range(len(x)):
        for b in range(6):
            was = i > 0 and x[i - 1] >> b & 1
            if (x[i] >> b & 1) != was:
                out.append((i, b, not was))
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("shifts")
    ap.add_argument("room")
    ap.add_argument("cat")
    ap.add_argument("seeds")
    ap.add_argument("opt", type=int)
    ap.add_argument("--jobs", type=int, default=16)
    ap.add_argument("--prologue", type=int, default=25)
    a = ap.parse_args()
    rows = []
    for line in open(a.shifts):
        name, k, s, inputs = line.split()
        rows.append((name, k, int(s), [int(v) for v in inputs.split(",")]))
    with ThreadPoolExecutor(a.jobs) as ex:
        exits = list(ex.map(lambda r: cr.replay_exit(a.room, r[3], a.seeds, a.cat)[0], rows))
    by = {}
    for (name, k, s, x), e in zip(rows, exits):
        by.setdefault(name, {})[(k, s)] = (e, x)
    for name, res in by.items():
        e0, base = res[("base", 0)]
        print(f"[orig] {name}: original cart exit f{e0} (optimum f{a.opt}){'' if e0 == a.opt else '  NOT THE OPTIMUM'}")
        if e0 != a.opt:
            continue
        fp = 0
        for k, (i, b, press) in enumerate(events(base)):
            ok = lambda s: res.get((str(k), s), (None,))[0] == a.opt
            lo = 0
            while lo > -CAP and ok(lo - 1):
                lo -= 1
            hi = 0
            while hi < CAP and ok(hi + 1):
                hi += 1
            fp += lo == hi == 0
            print(f"[orig] {name} f{i + 1:03} | {i + 1 - a.prologue:>4} | {BUTTONS[b]:5} | {'press' if press else 'release':7} | [{lo:+}, {hi:+}] | {hi - lo + 1:>2}{'+' if lo <= -CAP or hi >= CAP else ' '} | {'FRAME-PERFECT' if lo == hi == 0 else '-'}")
        print(f"[orig] {name}: {len(events(base))} events, {fp} frame-perfect in the original cart")


if __name__ == "__main__":
    main()
