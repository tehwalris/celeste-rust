#!/usr/bin/env python3
"""Check tasdatabase-format files in UniversalClassicTas AND the original cart
on a real PICO-8, under the file's own seeds, and look for a seed nudge where
UCT dies.

    tools/uct/check_uploads.py [--nudge] [--jobs N] FILE.tas...

The level is the file name's (TAS<level>.tas), the category its path's
(gemskip*: one dash; UCT_DASHES=1 and replay.py --one-dash). Per file:
 - UCT (validate.sh): finished on the FIRST attempt (a death is a failure),
   and its input count;
 - PICO-8 (replay.py --lua ORIGINAL --begin-game): z zeros + the inputs under
   the file's seeds, the exit frame, z = the frames before the player exists
   (a file cut one frame off that, category_runner's other cuts, or one
   whose UCT run differs from PICO-8's in Lua doubles, shows no exit here);
 - --nudge, where UCT did not finish: category_runner.nudged_seeds, each
   checked in both (PICO-8 must exit at the same frame, UCT must finish).
One tab-separated line per file.
"""
import argparse, os, re, subprocess, sys
from concurrent.futures import ThreadPoolExecutor

M = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.join(M, "tools"))
import category_runner as cr  # noqa: E402


def uct(level, path, gemskip):
    env = dict(os.environ)
    if gemskip:
        env["UCT_DASHES"] = "1"
    out = subprocess.run([f"{M}/safe-run.sh", "--memory", "8G", "--", f"{M}/tools/uct/validate.sh", str(level), path], capture_output=True, text=True, env=env, timeout=1800).stdout
    m = re.search(r"(finished, clean save|DID NOT FINISH[^,]*), (\d+) inputs", out)
    return (m.group(1) if m else "no result"), (int(m.group(2)) if m else None)


def spawn(room, gemskip):
    """The frames before the player exists in the original cart: the file's
    first input is the player's first frame's (UCT counts from there)."""
    _, out = cr.replay_exit(room, [0] * 60, "0", "gemskip" if gemskip else "nodiag")
    return min(int(m.group(1)) for m in re.finditer(r"^f(\d+) .* player -?\d", out, re.M)) - 1


def pico8_exit(room, inputs, seeds, gemskip):
    return cr.replay_exit(room, inputs, seeds, "gemskip" if gemskip else "nodiag")[0]


def check(path, nudge):
    level = int(re.search(r"TAS(\d+)\.tas$", path).group(1))
    room = f"{(level - 1) % 8},{(level - 1) // 8}"
    gemskip = "gemskip" in path
    seeds, inputs = cr.parse_tas(open(path).read())
    st, n = uct(level, path, gemskip)
    z = spawn(room, gemskip)
    e = pico8_exit(room, [0] * z + inputs, seeds or "0", gemskip)
    row = [path, f"level {level} room {room}", f"seeds [{seeds}]", f"{len(inputs)} inputs", f"UCT: {st} ({n} saved, spawn {z})", f"PICO-8 exit f{e}"]
    if nudge and not st.startswith("finished"):
        found = None
        for sd in cr.nudged_seeds(room, seeds or "0"):
            if pico8_exit(room, [0] * z + inputs, sd, gemskip) != e:
                continue
            tmp = f"/tmp/check_uploads-{os.getpid()}-{level}.tas"
            open(tmp, "w").write(f"[{sd},]" + ",".join(map(str, inputs)))
            st2, n2 = uct(level, tmp, gemskip)
            os.remove(tmp)
            if st2.startswith("finished"):
                found = f"nudge [{sd},] finishes ({n2} saved), PICO-8 f{e}"
                break
        row.append(found or "no nudge works")
    return "\t".join(row)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("files", nargs="+")
    ap.add_argument("--nudge", action="store_true")
    ap.add_argument("--jobs", type=int, default=4)
    a = ap.parse_args()
    with ThreadPoolExecutor(a.jobs) as ex:
        for line in ex.map(lambda p: check(p, a.nudge), a.files):
            print(line, flush=True)


if __name__ == "__main__":
    main()
