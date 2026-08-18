#!/usr/bin/env python3
"""Read `posgraph.bin` (the C8PZ position-transition table).

  posgraph_pairs.py FILE              - print its pair/cell counts
  posgraph_pairs.py --subset A B      - is A's pair set contained in B's?

The subset direction is the one that matters: a table used in place of
another must be a SUPERSET of it (it only shrinks the sweep's candidate
set), so `--subset LEVEL_K LEVEL_0` is the check that level 0's table may
stand in for level k's.
"""
import struct
import sys


def load(path):
    d = open(path, "rb").read()
    if d[:4] != b"C8PZ":
        raise SystemExit(f"{path}: bad magic {d[:4]!r}")
    off = 4
    grid, origin, frames, fplen = struct.unpack_from("<iiiI", d, off)
    off += 16
    fingerprint = d[off : off + fplen].decode()
    off += fplen
    (pairs,) = struct.unpack_from("<Q", d, off)
    off += 8
    ncell = grid * grid + 1
    offsets = struct.unpack_from("<%dI" % (ncell + 1), d, off)
    off += 4 * (ncell + 1)
    srcs = struct.unpack_from("<%dI" % pairs, d, off)
    pairset = {
        (dst, srcs[i])
        for dst in range(ncell)
        for i in range(offsets[dst], offsets[dst + 1])
    }
    return {"frames": frames, "fingerprint": fingerprint, "pairs": pairset}


def main(argv):
    if len(argv) == 4 and argv[1] == "--subset":
        a, b = load(argv[2]), load(argv[3])
        extra = a["pairs"] - b["pairs"]
        print(
            f"pairs: A={len(a['pairs'])} B={len(b['pairs'])} "
            f"A-only={len(extra)} frames A={a['frames']} B={b['frames']}"
        )
        if extra:
            print("NOT A SUBSET: e.g. " + str(sorted(extra)[:5]))
            return 1
        print("SUBSET: every pair of A is in B")
        return 0
    if len(argv) == 2:
        g = load(argv[1])
        live = len({d for d, _ in g["pairs"]})
        print(
            f"{argv[1]}: {len(g['pairs'])} pairs over {live} destination "
            f"cells, frames 1..{g['frames']}, fingerprint {g['fingerprint']}"
        )
        return 0
    raise SystemExit(__doc__)


if __name__ == "__main__":
    sys.exit(main(sys.argv))
