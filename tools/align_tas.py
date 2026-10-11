#!/usr/bin/env python3
"""Align a community tasdatabase TAS with one of ours, frame by frame, in the ORIGINAL cart.

    tools/align_tas.py ROOM DBNAME PROLOGUE OUR_TAS_FILE [OUTDIR] --cat CAT [--seeds S] [--jank J]

DBNAME is the database's name (2900m), PROLOGUE the frames before the
community file's first input; both runs start the room as the search does
(`--jank`: its loading frame), under the `[seeds]` list S (empty: every
balloon 0). With OUTDIR, also writes OUTDIR/ours.txt and OUTDIR/reference.txt:
the two paths for `rewrite export-ui --witness ours.txt --reference
reference.txt` (label, inputs, the dash starts as the cart reports them, then
`f x y` per frame from 0 to the exit).
"""
import argparse
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from celeste import ROOT, pico8, rooms, tasdb  # noqa: E402

ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
ap.add_argument("room")
ap.add_argument("name")
ap.add_argument("prologue", type=int)
ap.add_argument("ours", help="our witness (a tas/ file: comma lists, # comments)")
ap.add_argument("outdir", nargs="?")
ap.add_argument("--cat", required=True)
ap.add_argument("--seeds", default="", help="the [seeds] list, comma-separated")
ap.add_argument("--jank", type=int)
a = ap.parse_args()
room = rooms.parse(a.room)
seeds = [v for v in a.seeds.split(",") if v]
e = tasdb.entry(a.cat, a.name)
if not e:
    sys.exit(f"align_tas: no {a.cat} database entry {a.name}")
tas = [0] * a.prologue + tasdb.parse(open(tasdb.path(a.cat, e)).read())[1]
ours = [int(x) for line in open(os.path.join(ROOT, a.ours)) if not line.startswith("#") for x in line.strip().split(",") if x]


def run(seq):
    return {fr.f: fr for fr in pico8.replay(room, seq, len(seq) + 1, seeds, a.cat, a.jank)}


def btn(b):
    return "".join(c for c, bit in zip("LRUDJX", [1, 2, 4, 8, 16, 32]) if b & bit) or "-"


def at(fr):
    """Where the player is (rising from below: its spawn)."""
    return fr.player or fr.spawn if fr else None


def ex(R):
    # The replay runs one frame past the inputs (a path file holds at most
    # that many); a run that has not left by then is not a route under these seeds.
    out = [f for f, fr in R.items() if fr.room != room]
    if not out:
        sys.exit(f"does not leave room {a.room} by f{max(R)} (the balloon seeds {seeds}?)")
    return min(out)


A, B = run(tas), run(ours)
print(f"{a.name}: community exits f{ex(A)}, ours f{ex(B)}")
print(" f   community(input  x,y)        ours(input  x,y)")
for f in range(a.prologue, max(ex(A), ex(B)) + 1):
    pa, pb = at(A.get(f)), at(B.get(f))
    ia = btn(tas[f - 1]) if f <= len(tas) else ""
    ib = btn(ours[f - 1]) if f <= len(ours) else ""
    mark = "" if (pa == pb and ia == ib) else " *"
    show = lambda p, fr: f"{p[0]:.0f},{p[1]:.0f}" if p else (f"{fr.room[0]},{fr.room[1]}" if fr else "?")
    print(f"{f:3} {ia:>6} {show(pa, A.get(f)):>9}      {ib:>6} {show(pb, B.get(f)):>9}{mark}")


def dashes(R, end):
    return [f"{f}:{fr.dash}" for f, fr in sorted(R.items()) if fr.dash and f <= end]


def write(path, label, seq, R):
    end = ex(R)
    with open(path, "w") as o:
        o.write(f"label {label}\ninputs {','.join(map(str, seq))}\ndashes {' '.join(dashes(R, end))}\n0 - -\n")
        for f in range(1, end + 1):
            p = at(R.get(f)) if R.get(f) and R[f].room == room else None
            o.write(f"{f} {p[0]:.0f} {p[1]:.0f}\n" if p else f"{f} - -\n")
    print(f"wrote {path}: {label}, dashes {' '.join(dashes(R, end))}")


if a.outdir:
    os.makedirs(a.outdir, exist_ok=True)
    write(f"{a.outdir}/reference.txt", f"{e['file'][:-4]} (community, {a.name}, original cart): exits f{ex(A)}", tas, A)
    write(f"{a.outdir}/ours.txt", f"ours (search witness, original cart): exits f{ex(B)}", ours, B)
