"""UniversalClassicTas and Celia, headless (tools/uct/validate.sh,
tools/celia/validate.sh), and the balloon seed nudges a file may need there."""
import os
import re
import subprocess

from . import ROOT
from .categories import one_dash


def _run(script, dashes_env, level, path, cat):
    env = dict(os.environ)
    if one_dash(cat):
        env[dashes_env] = "1"
    return subprocess.run([f"{ROOT}/safe-run.sh", "--memory", "8G", "--", f"{ROOT}/tools/{script}", str(level), path], capture_output=True, text=True, env=env, timeout=1800).stdout


def uct(level, path, cat):
    """UniversalClassicTas plays `path` (a death is a failure): (finished,
    its status, its input count; None without a result)."""
    m = re.search(r"(finished, clean save|DID NOT FINISH[^,]*), (\d+) inputs", _run("uct/validate.sh", "UCT_DASHES", level, path, cat))
    return (bool(m) and m.group(1).startswith("finished")), (m.group(1) if m else "no result"), (int(m.group(2)) if m else None)


def celia(level, path, cat):
    """Celia's IL load (with its loading jank) plays `path`: (finished, its
    status, its frame count; None without a result)."""
    m = re.search(r"(finished|DID NOT FINISH[^,]*), (\d+)f", _run("celia/validate.sh", "CELIA_DASHES", level, path, cat))
    return (bool(m) and m.group(1).startswith("finished")), (m.group(1) if m else "no result"), (int(m.group(2)) if m else None)


def nudged_seeds(seeds, slots, steps=10):
    """The `[seeds]` list with every BALLOON's phase moved by k * 0.0001,
    k = -1, +1, -2, +2, .. (`slots`: `rooms.seed_slots`). A seed does not
    mean exactly the same in UCT and PICO-8: the phase advances 0.01 a frame
    in UCT's doubles and 0x0.028f (0.0099945) in 16.16, so where a balloon's
    y meets an integer (seed 0: y = start EXACTLY at offset 0.5) the touch
    can differ. Every candidate must be checked in BOTH; chests (a berry
    position) are kept."""
    vals = list(seeds) + ["0"] * (len(slots) - len(seeds))
    for k in range(1, steps + 1):
        for sign in (-1, 1):
            out = [(f"{(float(v) + sign * k * 1e-4) % 1:.4f}".rstrip("0").rstrip(".") or "0") if s == "balloon" else v for v, s in zip(vals, slots)]
            yield out + vals[len(slots):]
