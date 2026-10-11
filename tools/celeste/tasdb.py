"""The CelesteClassic tasdatabase: its file format and database.json.

A file is `[s1,s2]i1,i2,...`: the `[seeds]` (balloons and chests in
creation order, `rooms.seed_slots`; UniversalClassicTas `set_seeds`), then
one input byte per frame from the player's first frame. The database's
count of a file is its inputs - 1.
"""
import json
import os
import re

from . import ROOT
from .categories import FALLBACK

DB = os.path.expanduser("~/src/github.com/CelesteClassic/tasdatabase")


def parse(text):
    """(seeds as written, inputs) of a file's text; no `[...]`: no seeds."""
    seeds = []
    if "[" in text:
        seeds = [v.strip() for v in text[text.index("[") + 1:text.index("]")].split(",") if v.strip()]
        text = text[text.index("]") + 1:]
    return seeds, [int(x) for x in re.findall(r"\d+", text)]


def text(seeds, inputs):
    """The file's text: the one writer of the format (every upload so far)."""
    return "[" + ",".join(seeds) + "]" + ",".join(map(str, inputs))


def entries(cat):
    return json.load(open(f"{DB}/database.json"))["classic"].get(cat, [])


def entry(cat, name):
    """The database's entry of `cat` named `name` (`2900m`), or None."""
    return next((e for e in entries(cat) if e["name"] == name), None)


def path(cat, e):
    return f"{DB}/classic/{cat}/{e['file']}"


def level_of_name(name):
    """`2900m` -> 29 (None for a name that is not a level)."""
    return int(name.rstrip("m")) // 100 if re.fullmatch(r"\d+00m", name) else None


def level_of_file(path):
    """`.../TAS29.tas` -> 29."""
    return int(re.search(r"TAS(\d+)\.tas$", path).group(1))


def file_of_level(level):
    return f"TAS{level}.tas"


# Rooms whose database file is not PICO-8 valid (made in a tool computing in
# doubles, it never exits on a real PICO-8), played in a category's chain with
# OUR file instead, so the chain runs on unbroken (it used to restart after
# such a room by an IL load): (category, level) -> a file, relative to the
# repo. Only files verified like any submission (check_uploads: VALID).
OVERRIDES = {
    # The database's 164 finishes in Celia only; ours exits on PICO-8.
    ("gemskip100", 28): "tas/tasdatabase/gemskip100/upload/TAS28.tas",
}


def chain_file(cat, level):
    """The file a full run of `cat` plays in `level` (an OVERRIDES entry,
    else the category's database file, else FALLBACK's): (its category, path)."""
    if (cat, level) in OVERRIDES:
        return cat, os.path.join(ROOT, OVERRIDES[(cat, level)])
    db = json.load(open(f"{DB}/database.json"))["classic"]
    for c in FALLBACK[cat]:
        for e in db.get(c, []):
            if e.get("file") == file_of_level(level) and os.path.exists(path(c, e)):
                return c, path(c, e)
    raise SystemExit(f"tasdb: no database file for level {level} in {FALLBACK[cat]}")
