"""The PICO-8 replay (pico8_diff/replay.py) and its `@P8@` lines as typed frames.

A frame line is `fF room X,Y freeze N` then, as the frame has them,
` player X,Y spd SX,SY rem RX,RY`, ` player_spawn X,Y`, ` fly_fruit X,Y`,
` fruit X,Y`, ` lifeup X,Y`, ` dash DIR` (the dash the cart's own dash
branch started this frame: R L U D UR UL DR DL) and with --trace-objects
every object.
"""
import os
import re
import subprocess
from dataclasses import dataclass

from . import ROOT
from .categories import replay_flags

# The ORIGINAL cart (the game as released): the replays that verify a run.
ORIG = os.path.expanduser("~/src/github.com/tehwalris/celeste_ocaml/celeste.lua")
REPLAY = os.path.join(ROOT, "pico8_diff", "replay.py")
DIAGONAL = {"UR", "UL", "DR", "DL"}


@dataclass
class Frame:
    f: int
    room: tuple
    player: tuple = None  # (x, y), numbers as printed
    spawn: tuple = None
    spd_y: str = None  # exact hex (`tostr(v, true)`)
    dash: str = None
    lifeup: bool = False


def _xy(m):
    return (float(m.group(1)), float(m.group(2))) if m else None


def frames(lines):
    """The frame lines of a replay's output, typed (other lines skipped)."""
    out = []
    for line in lines:
        m = re.match(r"f(\d+) room (-?\d+),(-?\d+)", line)
        if not m:
            continue
        spd = re.search(r" player \S+ spd \S+?,(\S+)", line)
        dash = re.search(r" dash (\S+)", line)
        out.append(Frame(int(m.group(1)), (int(m.group(2)), int(m.group(3))), _xy(re.search(r" player (-?[\d.]+),(-?[\d.]+)", line)),
                         _xy(re.search(r" player_spawn (-?[\d.]+),(-?[\d.]+)", line)), spd.group(1) if spd else None,
                         dash.group(1) if dash else None, " lifeup " in line))
    return out


def exit_frame(fs, room):
    """The frame the room changes during (None: it never does)."""
    return next((fr.f for fr in fs if fr.room != tuple(room)), None)


def replay(room, inputs, nframes, seeds, cat, jank=None, original=True):
    """Replay `inputs` on a real PICO-8 from `room`'s load, as the search
    starts it (its loading frame from object `jank`), under the `[seeds]`
    list `seeds` (`[]`: every balloon 0, as UniversalClassicTas): the
    ORIGINAL cart, or the minimal one (the search's Lua). Its frames. A
    replay that fails (the cart crashed, PICO-8 printed nothing) raises: its
    frames would read as a run that never leaves the room.

    >>> import celeste.pico8 as p
    >>> real, p.REPLAY = p.REPLAY, "/bin/false"
    >>> p.replay((1, 0), [0], 1, [], "any")
    Traceback (most recent call last):
    ...
    RuntimeError: the PICO-8 replay failed (exit 1): ''
    >>> p.REPLAY = real
    """
    cmd = [REPLAY, "--room", f"{room[0]},{room[1]}", "--inputs", ",".join(map(str, inputs)), "--frames", str(nframes), "--balloon-seeds", ",".join(seeds) or "0"]
    cmd += (["--lua", ORIG, "--begin-game"] if original else []) + replay_flags(cat) + (["--jank", str(jank)] if jank else [])
    out = subprocess.run(cmd, capture_output=True, text=True, cwd=ROOT, timeout=900)
    if out.returncode != 0:
        raise RuntimeError(f"the PICO-8 replay failed (exit {out.returncode}): {out.stderr.strip()[-500:]!r}")
    return frames(out.stdout.splitlines())
