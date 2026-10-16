"""The tasdatabase categories and what each means to the search and the tools."""

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
    """The gemskip categories skip the orb: one dash throughout."""
    return cat.startswith("gemskip")


def hundred(cat):
    """The berry is part of the exit."""
    return cat in ("100", "gemskip100")


def nodiag(cat):
    return "nodiag" in cat


def onlydiag(cat):
    return "onlydiag" in cat


def search_flags(cat):
    """`rewrite search`'s category flags."""
    return (["--nodiag"] if nodiag(cat) else []) + (["--onlydiag"] if onlydiag(cat) else []) + (["--gemskip"] if one_dash(cat) else []) + (["--hundred"] if hundred(cat) else [])


def replay_flags(cat):
    """pico8_diff/replay.py's category flags."""
    return ["--one-dash"] if one_dash(cat) else []
