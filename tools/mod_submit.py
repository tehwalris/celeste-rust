#!/usr/bin/env python3
"""A map mod's route from the room's load -> a validated submission
(plans/map-mod.md): the inputs' jump presses that fire nowhere in the
minimal cart are dropped (the original cart's jump buffer would fire them
later: category_runner's `canon_jumps`), the route is replayed on a real
PICO-8 with the MOD'S OWN Lua (it must leave the room on its last frame),
the tasdatabase file is written from the player's first frame on (the spawn's
frames are not in it), and Celia plays it on fixp with the mod's cart.

    tools/mod_submit.py ROUTE MOD.p8 OUT.tas [--seeds 0] [--level 1]

ROUTE: a comma list of input bytes from the load (a `route_frame_N.txt` or
`witness_frame_N.txt`; `#` lines skipped). Exits non-zero unless both
PICO-8 and Celia finish.
"""
import argparse
import os
import re
import subprocess
import sys
import tempfile

M = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(M, "pico8_diff"))
import p8cart  # noqa: E402


def replay(cart, inputs, seeds, lua=None):
    cmd = [f"{M}/pico8_diff/replay.py", "--cart", cart, "--room", "0,0", "--balloon-seeds", seeds, "--inputs", ",".join(map(str, inputs)), "--frames", str(len(inputs) + 1)]
    if lua:
        cmd += ["--lua", lua, "--begin-game"]
    out = subprocess.run(cmd, capture_output=True, text=True, cwd=M, timeout=900).stdout
    return [l for l in out.splitlines() if re.match(r"f\d+ ", l)]


def exit_frame(lines):
    for l in lines:
        m = re.match(r"f(\d+) room (\S+)", l)
        if m and m.group(2) != "0,0":
            return int(m.group(1))
    return None


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("route")
    ap.add_argument("cart")
    ap.add_argument("out")
    ap.add_argument("--seeds", default="0")
    ap.add_argument("--level", type=int, default=1)
    a = ap.parse_args()
    inputs = [int(x) for x in re.findall(r"\d+", "".join(l for l in open(a.route) if not l.startswith("#")))]
    # 1. The jump presses that fire in the minimal cart only (spd.y -2 on their frame).
    lines = replay(a.cart, inputs, a.seeds)
    spd_y = {int(m.group(1)): m.group(2) for m in (re.match(r"f(\d+) .* spd \S+,(\S+)", l) for l in lines) if m}
    canon, prev = [], 0
    for i, b in enumerate(inputs):
        fired = b & 16 and not prev & 16 and spd_y.get(i + 1) == "0xfffe.0000"
        canon.append((b & ~16) | (16 if fired else 0))
        prev = b
    dropped = [i + 1 for i in range(len(inputs)) if inputs[i] != canon[i]]
    print(f"[submit] {len(inputs)} inputs; jump presses that fire nowhere, dropped on frames {dropped}")
    # 2. Both carts on a real PICO-8.
    work = tempfile.mkdtemp(prefix="mod-submit-")
    lua = os.path.join(work, "game.lua")
    open(lua, "w").write(p8cart.Cart(a.cart).lua + "\n")
    e_min = exit_frame(replay(a.cart, canon, a.seeds))
    e_mod = exit_frame(replay(a.cart, canon, a.seeds, lua))
    print(f"[submit] PICO-8: the minimal cart leaves the room on f{e_min}, the mod's own Lua on f{e_mod} (route: {len(canon)} frames)")
    if e_mod != len(canon):
        sys.exit(f"[submit] FAILED: the mod's Lua does not leave the room on the route's last frame ({e_mod})")
    # 3. The file from the player's first frame (the spawn's frames out).
    first = next(int(re.match(r"f(\d+)", l).group(1)) for l in replay(a.cart, canon, a.seeds, lua) if " player " in l)
    body = canon[first - 1:]
    assert not any(canon[:first - 1]), "an input before the player exists"
    open(a.out, "w").write(f"[{'' if a.seeds == '0' else a.seeds}]" + ",".join(map(str, body)))
    print(f"[submit] {a.out}: {len(body)} inputs from the player's first frame f{first} (Celia's count {len(body) - 1})")
    # 4. Celia on fixp, the mod's cart.
    env = dict(os.environ, CELIA_CART=a.cart, CELIA_FIXP="1")
    out = subprocess.run([f"{M}/safe-run.sh", "--memory", "8G", "--", f"{M}/tools/celia/validate.sh", str(a.level), a.out], capture_output=True, text=True, env=env, timeout=1800).stdout
    line = next((l for l in out.splitlines() if l.startswith(f"level {a.level} ")), out.strip()[-300:])
    print(f"[submit] Celia fixp: {line}")
    if "finished" not in line:
        sys.exit("[submit] FAILED in Celia")
    print("[submit] VALID: PICO-8 (the mod's Lua) and Celia fixp")


if __name__ == "__main__":
    main()
