"""The game model the Python tooling shares - no CLI of its own.

Python drives the external programs (PICO-8, UniversalClassicTas, Celia,
LOVE) and the tasdatabase; Rust owns the game (`rewrite room`, `rewrite
witness`, the search's `result.json`) and this package asks it rather
than reimplementing it.

    categories  the categories: labels, fallbacks, the tools' dash count
    tasdb       the tasdatabase: its file format (parsed and written once),
                database.json, level names
    rooms       levels and rooms; a room's objects and seed slots, a
                category's rules, a witness's canonical jumps (Rust's)
    pico8       the PICO-8 replay (pico8_diff/replay.py) and its `@P8@` lines
                as typed frames
    love        UniversalClassicTas and Celia runs; balloon seed nudges
"""
import os

# The repo root (this package is tools/celeste/).
ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
# The binary the tools ask about the game (`rewrite room`, `rewrite witness`).
REWRITE = os.path.join(ROOT, "target", "quick", "rewrite")
