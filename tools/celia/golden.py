#!/usr/bin/env python3
"""The float oracle's second goldens: Celia's exact traces of the
tasdatabase's files (cctas: the ORIGINAL cart in Lua doubles, with its
loading jank and its own seeds; plans/float.md).

    tools/celia/golden.py CAT LEVEL [LEVEL...]     one category's levels
    tools/celia/golden.py CAT all                  every level the category has
    tools/celia/golden.py random N LEVEL...|all    N random files a level
    tools/celia/golden.py witness all              our witnesses (gates/witnesses/)
    tools/celia/golden.py local all                our LOCAL witnesses (tools/uct/golden.py)

Per file: played in Celia with the exact trace (exact.lua via validate.sh:
CELIA_EXACT), its buffered jumps re-timed from Celia's own run and checked
as tools/uct/golden.py does (its `canonical`; Celia's start is not UCT's,
so neither is the run). The trace, the draw-only objects dropped, goes to
gates/celia/CAT_LEVEL.trace.zst and a line to gates/celia/manifest.txt:

    CAT LEVEL room=X,Y seeds=S1,S2|- gemskip=0|1 jank=J|- updates=N celia=STATUS retimed=K

J is the loading jank of Celia's IL load (`rewrite starts`' `celia`; `-`:
the first room, which has none). The test (`trace::oracle::tests`) replays
every line through the float reference engine under `--arith float
--loading-jank J`. A file UCT or Celia errors on is skipped.
READ THE DIFF before committing: a golden that moved means Celia's run did.
"""
import importlib.util
import json
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
from celeste import ROOT, categories, rooms  # noqa: E402

# tools/uct/golden.py by path (this file has its name).
_spec = importlib.util.spec_from_file_location("uct_golden", os.path.join(os.path.dirname(HERE), "uct", "golden.py"))
uct = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(uct)

REWRITE = os.path.join(ROOT, "target", "quick", "rewrite")


def celia(level, path, cat, trace):
    env = dict(os.environ, CELIA_EXACT=trace)
    if categories.one_dash(cat):
        env["CELIA_DASHES"] = "1"
    out = subprocess.run([f"{ROOT}/safe-run.sh", "--memory", "4G", "--", f"{HERE}/validate.sh", str(level), path], capture_output=True, text=True, env=env, cwd=ROOT)
    line = [l for l in out.stdout.splitlines() if l.startswith(f"level {level} ")]
    return line[-1].split(": ", 1)[1].split(",")[0] if line else f"no result ({out.returncode})"


def jank(room, cat):
    """The J of Celia's IL load into `room` (None: the first room)."""
    # The start depends on the dash count alone (the manifest's categories
    # include our own: random0, witness-...).
    args = [REWRITE, "starts", "--room", f"{room[0]},{room[1]}", "--category", "gemskipany" if categories.one_dash(cat) else "any"]
    out = subprocess.run([f"{ROOT}/safe-run.sh", "--memory", "4G", "--"] + args, capture_output=True, text=True, check=True, cwd=ROOT)
    return json.loads(out.stdout.strip().splitlines()[-1])["celia"]


def celia_last(updates, n):
    """The step that played Celia's file's last input (of `n`), if the
    trace goes on past it (its `kf` is the input's index)."""
    if all(int(w[3]) <= n for w in updates):
        return None
    return max(i for i, w in enumerate(updates) if int(w[3]) <= n)


def one(job, tmp, out):
    key, level, cat, path = job
    try:
        _, seeds, inputs, trace, status, retimed = uct.canonical(cat, level, tmp, celia, "celia", path)
    except uct.Skip as e:
        return None, str(e)
    room = rooms.room_of(level)
    j = jank(room, cat)
    updates = uct.golden(trace, os.path.join(out, f"{key}_{level}.trace.zst"), lambda u: celia_last(u, len(inputs)))
    seed_text = ",".join(seeds) if seeds else "-"
    line = (
        f"{key} {level} room={room[0]},{room[1]} seeds={seed_text} gemskip={int(categories.one_dash(cat))} "
        f"jank={'-' if j is None else j} updates={updates} celia={status.replace(' ', '_')} retimed={retimed}"
    )
    return line, None


def main():
    uct.run(sys.argv[1:], uct.out_dir(sys.argv[1:], "celia"), one)


if __name__ == "__main__":
    main()
