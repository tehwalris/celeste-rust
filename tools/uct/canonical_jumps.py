#!/usr/bin/env python3
"""A TAS file with its BUFFERED jumps re-timed, from a tool's exact trace of
it (tools/uct/exact.lua, tools/celia/exact.lua: the same format):

    tools/uct/canonical_jumps.py uct|celia TRACE FILE.tas > CANONICAL.tas

The original cart buffers a jump press for 4 frames (`jbuffer`); the traced
program (lua/celeste-minimal.lua) drops a press that cannot jump at once. A
jump the buffer made - a jump on an update with no FRESH jump press (bit 16
in the file's input now, not in the previous one) - is moved to its own
frame: the press it came from and every input until the jump release the
button, the jump's input presses it. Every other input is kept. A buffered
jump past the file's end (the tool plays no buttons there) extends the file
with empty inputs up to its press. The re-timed file makes the same run in
the original cart (the golden scripts check it, update by update, and that
no buffered jump is left), and the traced program runs it the same way.

A JUMP is read from the player's state: the cart sets `jbuffer` to 4 on a
fresh press (`btn(jump) and not p_jump`), else counts it down, and a jump
(ground or wall) zeroes it - so an update jumped iff its player's `jbuffer`
would be positive by then and is 0 after it. (`pause_player` and `freeze`
skip the player's update: its `jbuffer` and `p_jump` stay, which a fresh
press tells, and nothing else reads as a jump.) UCT's traces also record
the jump sounds (`psfx` 1 and 2; Celia copies the cart every step, which
defeats the hook): there the two must agree on every update that leaves a
player.

The update that consumed file input k: UCT's (k+1)th update with an input
(`in` -1 before the player's first frame); Celia's update with `kf` k + 1
(Celia plays input 1 to the cart from the load on, uncounted until the
player's first frame).

Exits 0 printing the file (unchanged if nothing was buffered).
"""
import sys

JUMP = 16


def read_trace(path):
    """Per update: the input, `kf`, the sounds, and the first player's
    `jbuffer` and `p_jump` after it (None: no player)."""
    ups, obj = [], False
    for line in open(path):
        w = line.split()
        if not w:
            continue
        if w[0] == "u":
            ups.append({"in": int(w[5]), "kf": int(w[3]), "sounds": [], "player": None})
            obj = False
        elif w[0] == "s":
            ups[-1]["sounds"].append(int(w[1]))
        elif w[0] == "o":
            obj = w[2] == "player" and ups[-1]["player"] is None
            if obj:
                ups[-1]["player"] = {}
        elif w[0] == "f" and obj and w[1] in ("jbuffer", "p_jump"):
            ups[-1]["player"][w[1]] = w[2]
    return ups


def jumped(prev, up):
    """Whether `up` jumped (its player's `jbuffer` positive at the jump
    check, 0 after it). A player new in `up` starts with `jbuffer` 0 and no
    `p_jump` (it updates in the frame it is made)."""
    a, b = prev["player"] or {"p_jump": "false", "jbuffer": "0x0p+0"}, up["player"]
    if b is None:
        return False
    fresh = max(up["in"], 0) & JUMP != 0 and a["p_jump"] == "false"
    if fresh and b["p_jump"] == "false":
        # The player's update did not run (`freeze`, `pause_player`): it
        # would have set `p_jump`.
        return False
    jb = float.fromhex(a["jbuffer"])
    expected = 4 if fresh else max(jb - 1, 0)
    return expected > 0 and float.fromhex(b["jbuffer"]) == 0


def read_tas(path):
    text = open(path).read().strip()
    head, _, body = text.partition("]")
    inputs = [int(x) for x in body.split(",") if x.strip() != ""]
    return head + "]", inputs


def main():
    tool, trace, tas = sys.argv[1], sys.argv[2], sys.argv[3]
    if tool not in ("uct", "celia"):
        sys.exit(f"the tool: uct or celia, not {tool!r}")
    ups = read_trace(trace)
    head, inputs = read_tas(tas)
    # The file input each update consumed, and what the tool played for it
    # (0 past the file's end).
    if tool == "uct":
        k_of = {n: k for k, n in enumerate(i for i, u in enumerate(ups) if u["in"] >= 0)}
    else:
        k_of = {n: u["kf"] - 1 for n, u in enumerate(ups)}
    given = {}
    for n, k in k_of.items():
        if given.setdefault(k, ups[n]["in"]) != ups[n]["in"]:
            sys.exit(f"{tas}: input {k} played as {given[k]} and {ups[n]['in']}")
    if any(v != (inputs[k] if k < len(inputs) else 0) for k, v in given.items()):
        sys.exit(f"{tas}: the trace's inputs are not the file's")
    original = list(inputs) + [0] * max(0, max(given, default=-1) + 1 - len(inputs))
    out = list(original)
    end = len(inputs)
    # Presses as the ORIGINAL run made them (a re-timed jump must not hide
    # the press of the next one).
    held = lambda k: k >= 0 and original[k] & JUMP != 0
    for n in range(1, len(ups)):
        jump = jumped(ups[n - 1], ups[n])
        # (A player killed, or a room left, in the update may still jump:
        # no player is left to read, and nothing is to re-time.)
        if tool == "uct" and ups[n]["player"] is not None and jump != any(s in (1, 2) for s in ups[n]["sounds"]):
            sys.exit(f"{tas}: update {n}: the player's state says {'a' if jump else 'no'} jump, the sounds do not")
        k = k_of.get(n)
        if not jump or k is None or (held(k) and not held(k - 1)):
            continue
        # The press it came from: the original's latest fresh press before.
        p = next((q for q in range(k - 1, -1, -1) if held(q) and not held(q - 1)), None)
        if p is None:
            sys.exit(f"{tas}: a buffered jump at update {n} with no press before it")
        for q in range(p, k):
            out[q] &= ~JUMP
        out[k] |= JUMP
        end = max(end, k + 1)
    sys.stdout.write(head + ",".join(str(v) for v in out[:end]))


if __name__ == "__main__":
    main()
