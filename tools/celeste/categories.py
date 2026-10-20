"""The tasdatabase categories: their labels, their full runs' fallbacks, and
the dash count the external tools are given. What a category means to the
search (`--category`, the database's names) is Rust's: `rooms.rules`."""

# The category's name on the UI.
LABEL = {"nodiag": "No Diagonal Dashes", "gemskipany": "Gemskip any%", "gemskipnodiag": "Gemskip No Diagonal Dashes", "100": "100%", "gemskip100": "Gemskip 100%", "onlydiag": "Only Diagonal Dashes"}

# The files a category's full run plays where the category has none: the
# 100% categories play any% where a room has no berry; the gemskip ones play
# their parent category before the orb room (level 22).
FALLBACK = {
    "any": ["any"],
    "100": ["100", "any"],
    "nodiag": ["nodiag", "any"],
    "gemskipany": ["gemskipany", "any"],
    "gemskipnodiag": ["gemskipnodiag", "nodiag", "any"],
    "gemskip100": ["gemskip100", "gemskipany", "100", "any"],
    "mindashes": ["mindashes", "any"],
    "minjumps": ["minjumps", "any"],
    "key": ["key", "any"],
}


def one_dash(cat):
    """The gemskip categories skip the orb: one dash throughout (what
    PICO-8's replay, UCT and Celia are told)."""
    return cat.startswith("gemskip")


def replay_flags(cat):
    """pico8_diff/replay.py's category flags."""
    return ["--one-dash"] if one_dash(cat) else []
