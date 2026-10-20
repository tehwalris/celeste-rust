"""Levels and rooms; a room's objects and starts, and a category's rules, as
Rust knows them."""
import functools
import json
import subprocess

from . import REWRITE, ROOT


def room_of(level):
    """The cart's level (1-based, `TAS<level>.tas`) -> its room (x, y)."""
    return ((level - 1) % 8, (level - 1) // 8)


def level_of(room):
    x, y = room
    return x + 8 * y + 1


def parse(text):
    """`"x,y"` -> (x, y)."""
    x, y = (int(v) for v in text.split(","))
    return x, y


def text(room):
    return f"{room[0]},{room[1]}"


def _ask(args, binary):
    out = subprocess.run([f"{ROOT}/safe-run.sh", "--memory", "8G", "--", binary] + args, capture_output=True, text=True, cwd=ROOT)
    if out.returncode != 0:
        raise RuntimeError(f"rewrite {' '.join(args)}: {out.stderr.strip().splitlines()[-1:] or out.returncode}")
    return json.loads(out.stdout)


def objects(room, binary=REWRITE):
    """`rewrite room`: the room's objects in creation order (the original
    cart's), each {tile, tx, ty, kind, minimal, seeded}."""
    return _ask(["room", "--room", text(room)], binary)["objects"]


@functools.lru_cache(maxsize=None)
def rules(cat, binary=REWRITE):
    """What the category means to the search (`rewrite room --category`):
    {name, hundred (a win needs the berry), nodiag, onlydiag (the dash
    starts forbidden), gemskip (one dash past the orb)}."""
    return _ask(["room", "--category", cat], binary)["category"]


def seed_slots(room, binary=REWRITE):
    """The kinds (`balloon`, `chest`) a `[seeds]` list runs over, in order."""
    return [o["kind"] for o in objects(room, binary) if o["seeded"]]


def starts(room, flags, binary=REWRITE):
    """`rewrite starts`: every loading-frame J a play of the previous room
    can enter `room` with (`feasible`), grouped by the start state they give
    (`classes`), and Celia's IL-load J (`celia`). `flags`: the search's
    settings (the category, `--balloon-seeds`)."""
    return _ask(["starts", "--room", text(room)] + flags, binary)
