#!/usr/bin/env python3
"""The float oracle's goldens: UniversalClassicTas's exact traces of the
tasdatabase's files (plans/float.md).

    tools/uct/golden.py CAT LEVEL [LEVEL...]     one category's levels
    tools/uct/golden.py CAT all                  every level the category has
    tools/uct/golden.py random N LEVEL...|all    N random files a level
    tools/uct/golden.py witness all              our witnesses (below)
    tools/uct/golden.py local all                our LOCAL witnesses (below)

A RANDOM file (`random_inputs`: held directions, jump and dash presses at
random, every seed 0; deterministic in its index and level) is a golden of
category `randomI`: it reaches states no community file does (deaths,
walls and spikes at odd remainders).

A WITNESS is one of our validated runs, listed in gates/witnesses/witnesses.txt
(`KEY CATEGORY LEVEL FILE`): its golden is named by KEY, the category gives
its dashes. A run valid in fixed point only dies in the tool, and the
golden holds that death: exactly the divergence the float engine must
reproduce. Committed witnesses are our SUBMITTED runs only: an unsubmitted
or teased route is never revealed, so it is a LOCAL witness, listed in
/var/tmp/float-goldens/witnesses/witnesses.txt (FILE relative to that
directory) with its goldens and manifest in /var/tmp/float-goldens/uct/,
outside the repo (the test `trace::oracle::tests::the_local_goldens` replays
them, and says SKIPPED where they are absent).

Per file (the database's `classic/CAT/TAS<level>.tas`): run it in UCT with
the exact trace (tools/uct/exact.lua via validate.sh --exact), re-time
its buffered jumps (canonical_jumps.py) and, if any moved, run the re-timed
file too, checking it makes the same run (the player's position, remainder
and speed after every update, bit for bit) and leaves no buffered jump. The
trace, the draw-only objects dropped, goes to gates/uct/CAT_LEVEL.trace.zst
(zstd -19) and a line to gates/uct/manifest.txt:

    CAT LEVEL room=X,Y seeds=S1,S2|- gemskip=0|1 updates=N uct=STATUS retimed=K

The test (`trace::oracle::tests`) replays every manifest line through the
float reference engine (the tools' seeds: `cart::tool_seeds`). A file UCT
errors on is skipped.
READ THE DIFF before committing: a golden that moved means UCT's run did.
"""
import os
import random
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
from celeste import ROOT, categories, rooms, tasdb  # noqa: E402

# Our unsubmitted routes: never in the repo (the module doc).
LOCAL = "/var/tmp/float-goldens"
DRAW_ONLY = {"smoke", "room_title", "lifeup", "message", "flag"}


def uct(level, path, cat, trace):
    dashes = ["--dashes", "1"] if categories.one_dash(cat) else []
    out = subprocess.run([f"{ROOT}/safe-run.sh", "--memory", "4G", "--", f"{HERE}/validate.sh", "--exact", trace] + dashes + [str(level), path], capture_output=True, text=True, cwd=ROOT)
    line = [l for l in out.stdout.splitlines() if l.startswith(f"level {level} ")]
    return line[-1].split(": ", 1)[1].split(",")[0] if line else f"no result ({out.returncode})"


def player_path(trace):
    """The player's (x, y, rem, spd) after every update."""
    out, cur, obj = [], None, None
    for line in open(trace):
        w = line.split()
        if not w:
            continue
        if w[0] == "u":
            cur = {}
            out.append(cur)
        elif w[0] == "o":
            obj = w[2]
        elif w[0] == "f" and obj == "player" and w[1] in ("x", "y", "rem.x", "rem.y", "spd.x", "spd.y"):
            cur[w[1]] = w[2]
    return out


def strip(trace):
    """The trace without the draw-only objects (the parser drops them too)."""
    out, keep = [], True
    for line in open(trace):
        w = line.split()
        if w and w[0] == "o":
            keep = w[2] not in DRAW_ONLY
        elif w and w[0] in ("u", "g", "s"):
            keep = True
        if keep:
            out.append(line)
    return "".join(out)


def cut(text, n):
    """The trace's first `n` updates."""
    out, k = [], -1
    for line in text.splitlines(keepends=True):
        if line.startswith("u "):
            k += 1
            if k >= n:
                break
        out.append(line)
    return "".join(out)


class Skip(Exception):
    """A file no golden is made of, and why."""


def random_inputs(i, level, n=90):
    """Random file `i` of `level`: a direction held 2-10 frames (biased
    right and up, where most exits are), jump pressed on 12% of frames,
    dash on 6%."""
    r = random.Random(f"uct-random-{i}-{level}")
    out = []
    while len(out) < n:
        d = r.choice([0, 1, 2, 2, 2, 4, 5, 6, 6, 8, 9, 10])
        for _ in range(r.randint(2, 10)):
            out.append(d | (16 if r.random() < 0.12 else 0) | (32 if r.random() < 0.06 else 0))
    return out[:n]


def canonical(cat, level, tmp, play=None, tool="uct", path=None):
    """The category's file for `level` (a `randomI` category: random file
    I; `path`: that file, played as category `cat`) as the tool plays it
    (`play(level, path, cat, trace)`, its status; default UCT), its
    buffered jumps re-timed: (the file to play, its seeds, its inputs, the
    tool's trace of it, its status, the inputs re-timed). Raises `Skip`."""
    play = play or uct
    if path is None and cat.startswith("random"):
        path = os.path.join(tmp, "random.tas")
        open(path, "w").write(tasdb.text([], random_inputs(int(cat[len("random"):]), level)))
    elif path is None:
        e = [x for x in tasdb.entries(cat) if x.get("file") == tasdb.file_of_level(level)]
        if not e:
            raise Skip("no file")
        path = tasdb.path(cat, e[0])
    seeds, inputs = tasdb.parse(open(path).read())
    orig = os.path.join(tmp, "orig.trace")
    status = play(level, path, cat, orig)
    if "LUA ERROR" in status or not os.path.exists(orig) or os.path.getsize(orig) == 0:
        raise Skip(f"{tool}: {status}")
    canon = os.path.join(tmp, "canon.tas")
    with open(canon, "w") as f:
        r = subprocess.run([sys.executable, "-I", f"{HERE}/canonical_jumps.py", tool, orig, path], stdout=f, stderr=subprocess.PIPE, text=True)
    if r.returncode != 0:
        raise Skip(f"re-timing: {r.stderr.strip()}")
    _, canon_inputs = tasdb.parse(open(canon).read())
    retimed = sum(a != b for a, b in zip(inputs, canon_inputs))
    if not retimed:
        return path, seeds, inputs, orig, status, 0
    final = os.path.join(tmp, "canon.trace")
    status2 = play(level, canon, cat, final)
    a, b = player_path(orig), player_path(final)
    # A file extended past its end (a buffered jump there) runs out a
    # driver's clock later: the same run, longer, where neither finished
    # nor died.
    extended = len(canon_inputs) > len(inputs)
    clock = lambda st: "finished" not in st and "died" not in st.lower()
    n = min(len(a), len(b)) if extended and clock(status) and clock(status2) else max(len(a), len(b))
    if a[:n] != b[:n] or len(a) < n or len(b) < n or (status2 != status and n == max(len(a), len(b))):
        first = next((i for i, (x, y) in enumerate(zip(a, b)) if x != y), min(len(a), len(b)))
        raise Skip(f"the re-timed file runs differently (update {first}; {status} -> {status2})")
    again = subprocess.run([sys.executable, "-I", f"{HERE}/canonical_jumps.py", tool, final, canon], capture_output=True, text=True, check=True).stdout
    if again != open(canon).read():
        raise Skip("a buffered jump is left after re-timing")
    return canon, seeds, canon_inputs, final, status2, retimed


def uct_last(updates, n):
    """The update that played UCT's file's last input (of `n`), if the
    trace goes on past it (its `in` is -1 where no input is played)."""
    played = [i for i, w in enumerate(updates) if int(w[5]) >= 0]
    return played[n - 1] if len(played) > n else None


def golden(trace, out, last):
    """Write `trace` as a golden to `out` (zstd -19), the draw-only objects
    dropped; a playback that never ends (the summit has no exit to
    clean-save at) runs to the driver's timeout: kept to 30 updates past the
    file's end (`last(updates)`: the update playing its last input, or
    None). The updates kept."""
    text = strip(trace)
    end = last([l.split() for l in text.splitlines() if l.startswith("u ")])
    if end is not None:
        text = cut(text, end + 1 + 30)
    z = subprocess.run(["zstd", "-19", "-q", "-c"], input=text.encode(), capture_output=True, check=True).stdout
    open(out, "wb").write(z)
    return sum(1 for l in text.splitlines() if l.startswith("u "))


def one(job, tmp, out):
    key, level, cat, path = job
    try:
        _, seeds, inputs, trace, status, retimed = canonical(cat, level, tmp, path=path)
    except Skip as e:
        return None, str(e)
    room = rooms.room_of(level)
    updates = golden(trace, os.path.join(out, f"{key}_{level}.trace.zst"), lambda u: uct_last(u, len(inputs)))
    seed_text = ",".join(seeds) if seeds else "-"
    line = f"{key} {level} room={room[0]},{room[1]} seeds={seed_text} gemskip={int(categories.one_dash(cat))} updates={updates} uct={status.replace(' ', '_')} retimed={retimed}"
    return line, None


WITNESSES = os.path.join(ROOT, "gates", "witnesses", "witnesses.txt")


def jobs(argv):
    """The jobs of the command line: (manifest key, level, category, the
    file or None)."""
    cat, levels = argv[0], argv[1:]
    if cat in ("witness", "local"):
        if levels != ["all"]:
            sys.exit(f"{cat} all: the witnesses are {WITNESSES}'s, or the local {LOCAL}/witnesses/witnesses.txt's")
        listing, base = (WITNESSES, ROOT) if cat == "witness" else (os.path.join(LOCAL, "witnesses", "witnesses.txt"), LOCAL)
        out = []
        for line in open(listing):
            w = line.split()
            if w and not w[0].startswith("#"):
                out.append((w[0], int(w[2]), w[1], os.path.join(base, w[3])))
        return out
    cats = [cat]
    if cat == "random":
        cats, levels = [f"random{i}" for i in range(int(levels[0]))], levels[1:]
        if levels == ["all"]:
            levels = list(range(1, 32))
    elif levels == ["all"]:
        levels = sorted({tasdb.level_of_file(e["file"]) for e in tasdb.entries(cat) if e.get("file")})
    return [(c, int(level), c, None) for c in cats for level in levels]


def run(argv, out, one):
    """Make the goldens of `argv` into directory `out` with `one`, updating
    its manifest."""
    os.makedirs(out, exist_ok=True)
    manifest = os.path.join(out, "manifest.txt")
    lines = [l for l in open(manifest).read().splitlines()] if os.path.exists(manifest) else []
    for job in jobs(argv):
        key, level = job[0], job[1]
        with tempfile.TemporaryDirectory(dir="/var/tmp") as tmp:
            line, why = one(job, tmp, out)
        lines = [l for l in lines if l.split()[:2] != [key, str(level)]]
        if line:
            lines.append(line)
            print(line, flush=True)
        else:
            print(f"{key} {level}: skipped: {why}", flush=True)
    lines.sort(key=lambda l: (l.split()[0], int(l.split()[1])))
    open(manifest, "w").write("\n".join(lines) + "\n")


def out_dir(argv, tool):
    """Where `argv`'s goldens go: the repo's gates/TOOL, or the local
    store's TOOL for `local`."""
    return os.path.join(LOCAL, tool) if argv[:1] == ["local"] else os.path.join(ROOT, "gates", tool)


def main():
    run(sys.argv[1:], out_dir(sys.argv[1:], "uct"), one)


if __name__ == "__main__":
    main()
