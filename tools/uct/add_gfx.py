#!/usr/bin/env python3
"""Fill in the sprite sheet of UCT's carts/celeste.p8 from the original cart.

The celeste.p8 that tools/uct/validate.sh runs was built from the original
cart's Lua + cart/ (map and flags): its __gfx__ upper half is all zeros, so
UCT plays correctly but draws no tiles or sprites. For videos
(tools/compare_video.py) this takes the sprite sheet from the original BBS
cart (lexaloffle cart 15133, a .p8.png: 2 bits per channel per pixel) and
writes it into __gfx__, after checking that the lower half - which is map
rows 32-63 and so part of the game - is identical to what is there already.

    tools/uct/add_gfx.py [CELESTE.p8.png]   (default: download it)
"""
import io, os, sys, urllib.request
from PIL import Image

UCT = os.environ.get("UCT", os.path.expanduser("~/src/github.com/gonengazit/UniversalClassicTas"))
CART = f"{UCT}/CelesteTAS/carts/celeste.p8"
URL = "https://www.lexaloffle.com/bbs/cposts/1/15133.p8.png"


def main():
    data = open(sys.argv[1], "rb").read() if len(sys.argv) > 1 else urllib.request.urlopen(URL).read()
    px = Image.open(io.BytesIO(data)).convert("RGBA").tobytes()
    rom = bytes(((px[i + 3] & 3) << 6) | ((px[i] & 3) << 4) | ((px[i + 1] & 3) << 2) | (px[i + 2] & 3) for i in range(0, len(px), 4))
    gfx = ["".join(f"{v & 15:x}{v >> 4:x}" for v in rom[row * 64:(row + 1) * 64]) for row in range(128)]
    lines = open(CART).read().split("\n")
    start = lines.index("__gfx__") + 1
    old = lines[start:start + 128]
    assert len(old) == 128 and all(len(l) == 128 for l in old), "unexpected __gfx__ section"
    if old[64:] != gfx[64:]:
        sys.exit("the sprite sheet's lower half (map rows 32-63) differs from the cart's: not the same cart")
    if old[:64] == gfx[:64]:
        print(f"{CART}: sprite sheet already present")
        return
    if any(l.strip("0") for l in old[:64]):
        sys.exit("the cart's upper sprite sheet is neither empty nor the original's: refusing to overwrite")
    lines[start:start + 64] = gfx[:64]
    open(CART, "w").write("\n".join(lines))
    print(f"{CART}: sprite sheet filled in from the original cart")


if __name__ == "__main__":
    main()
