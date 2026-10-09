#!/usr/bin/env python3
"""Check tasdatabase-format files in every tool and start mode we know:
UniversalClassicTas, Celia, and the original cart on a real PICO-8 - loaded
alone (the IL load), and in REAL PLAY: through the real transition from the
previous room, and from the game's boot (pico8_diff/chain.py).

    tools/uct/check_uploads.py [--nudge] [--jobs N] [--cat CAT] FILE.tas...

The level is the file name's (TAS<level>.tas), the category its directory's
(or --cat; gemskip*: one dash). One tab-separated row per file:
 - ours: the file's count (inputs - 1), and the database's;
 - IL: PICO-8, the room loaded directly (UCT's IL load), its count;
 - UCT (validate.sh): finished on the FIRST attempt (a death is a failure);
 - Celia (tools/celia/validate.sh): Celia's IL load with its loading jank;
 - chain: PICO-8 from the previous room, played with the database's file,
   through the real transition; boot: the same from 100m;
 - VALID: the boot chain (real play, the ground truth) exits by the file's
   count (100%: with the berry), and Celia agrees. The chain from the previous
   room is informative: that room itself starts there by an IL load, so its
   own database file can fail ("PREVIOUS ROOM FAILED").
--nudge, where UCT did not finish: category_runner.nudged_seeds, each checked
in PICO-8 (the IL exit must not move) and UCT.
"""
import argparse, json, os, re, subprocess, sys
from concurrent.futures import ThreadPoolExecutor

M = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.join(M, "tools"))
sys.path.insert(0, os.path.join(M, "pico8_diff"))
import category_runner as cr  # noqa: E402
import chain  # noqa: E402


def tool(script, env_key, pat, level, path, gemskip):
    """UCT or Celia: (status, the number its line reports)."""
    env = dict(os.environ)
    if gemskip:
        env[env_key] = "1"
    out = subprocess.run([f"{M}/safe-run.sh", "--memory", "8G", "--", f"{M}/tools/{script}", str(level), path], capture_output=True, text=True, env=env, timeout=1800).stdout
    m = re.search(pat, out)
    return (m.group(1) if m else "no result"), (int(m.group(2)) if m else None)


def uct(level, path, gemskip):
    st, n = tool("uct/validate.sh", "UCT_DASHES", r"(finished, clean save|DID NOT FINISH[^,]*), (\d+) inputs", level, path, gemskip)
    return st, (n - 1 if n else None)


def celia(level, path, gemskip):
    return tool("celia/validate.sh", "CELIA_DASHES", r"(finished|DID NOT FINISH[^,]*), (\d+)f", level, path, gemskip)


def cat_of(path, cat):
    c = cat or os.path.basename(os.path.dirname(os.path.abspath(path)))
    if c not in chain.FALLBACK:
        raise SystemExit(f"{path}: category {c!r} unknown (--cat)")
    return c


def chain_result(lines, nsegs):
    s, last = chain.summary("", lines, nsegs)
    return (int(last["count"]), last["berry"] == "true") if last else None, s.strip(": ")


def check(path, nudge, cat):
    cat = cat_of(path, cat)
    level = int(re.search(r"TAS(\d+)\.tas$", path).group(1))
    room = chain.room_of(level)
    gemskip = cat.startswith("gemskip")
    hundred = cat in ("100", "gemskip100")
    seeds, inputs = chain.parse_tas(open(path).read())
    ours = len(inputs) - 1
    db = [e for e in json.load(open(f"{chain.DB}/database.json"))["classic"][cat] if e.get("file") == f"TAS{level}.tas"]
    dbn = db[0].get("frames") if db else None
    il, il_s = chain_result(chain.single(cat, level, seeds, inputs), 1)
    st, n = uct(level, path, gemskip)
    cst, cn = celia(level, path, gemskip)
    ch, ch_s = chain_result(chain.chain(cat, level, seeds, inputs, max(level - 1, 1)), 2 if level > 1 else 1)
    # A database file that dies on PICO-8, or never exits there (it was made
    # in a tool computing in doubles: gemskipany 2600m dies, gemskip100 2800m
    # never exits), breaks the boot chain: it restarts at the level after it,
    # by an IL load, and says so (as chain.entry_jank does).
    first = 1
    while True:
        nsegs = level - first + 1
        lines = chain.chain(cat, level, seeds, inputs, first)
        bt, bt_s = chain_result(lines, nsegs)
        exits, bad, _ = chain.parse(lines)
        early = [int(l.split()[2]) for l in bad if l.startswith("death seg ") and int(l.split()[2]) < nsegs]
        if not early and not bad and len(exits) < nsegs - 1:
            early = [len(exits) + 1]
        if not early:
            break
        bt_s += f" -> from level {first + early[0]}"
        first += early[0]
    if first > 1:
        bt_s = f"(from level {first}) " + bt_s
    # The file's count is what the tasdatabase bot takes (inputs - 1); a
    # trailing input after the exit does not invalidate it, an exit after it does.
    ok = lambda r: r is not None and r[0] <= ours and (r[1] or not hundred)
    celia_ok = cst.startswith("finished") and cn <= ours
    valid = ok(bt) and celia_ok
    row = [f"{cat} TAS{level} room {room[0]},{room[1]}", f"ours {ours} db {dbn}", f"IL {il_s}", f"UCT {st} {n}", f"Celia {cst} {cn}",
           f"chain {ch_s}", f"boot {bt_s}", "VALID" if valid else "INVALID" + (" (boot chain broken by a database file)" if first > 1 else "")]
    if nudge and not st.startswith("finished"):
        found = None
        for sd in cr.nudged_seeds(f"{room[0]},{room[1]}", ",".join(str(v) for v in seeds) or "0"):
            if chain_result(chain.single(cat, level, [float(v) for v in sd.split(",")], inputs), 1)[0] != il:
                continue
            tmp = f"/tmp/check_uploads-{os.getpid()}-{level}.tas"
            open(tmp, "w").write(f"[{sd},]" + ",".join(map(str, inputs)))
            st2, n2 = uct(level, tmp, gemskip)
            os.remove(tmp)
            if st2.startswith("finished"):
                found = f"nudge [{sd},] finishes ({n2})"
                break
        row.append(found or "no nudge works")
    return "\t".join(row)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("files", nargs="+")
    ap.add_argument("--nudge", action="store_true")
    ap.add_argument("--cat", help="the category (default: the file's directory name)")
    ap.add_argument("--jobs", type=int, default=4)
    a = ap.parse_args()
    with ThreadPoolExecutor(a.jobs) as ex:
        for line in ex.map(lambda p: check(p, a.nudge, a.cat), a.files):
            print(line, flush=True)


if __name__ == "__main__":
    main()
