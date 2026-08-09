"""Compare two g.bin arrays (min-frames-to-exit per row id).

Format: "C8TB", u32 format version, u64 count, then a zstd stream of
count u16-LE values. G_UNREACHABLE is u16::MAX.

The time-expanded sweep (rewrite::sweep_time) never looks past its horizon,
so it produces g only where e + g <= H. Comparing it against a full edge
sweep's array therefore needs the reference THRESHOLDED at H:

    tools/gdiff.py A/g.bin B/g.bin --threshold 100 --meta A/f100/meta.json

`--meta` supplies the watermarks that give each row its earliest-arrival
frame e (watermarks[f-1] is the id counter after frame f, so e is the first
f whose watermark exceeds the id).
"""
import sys, subprocess, struct, json, bisect, argparse

UNREACH = 0xFFFF


def load(path):
    raw = open(path, 'rb').read()
    assert raw[:4] == b'C8TB', raw[:4]
    n = struct.unpack('<Q', raw[8:16])[0]
    pay = subprocess.run(['zstd', '-d', '-c'], input=raw[16:], capture_output=True).stdout
    return n, list(struct.unpack_from(f'<{n}H', pay, 0))


def earliest_frames(watermarks, n):
    """e per row id, from the row table's watermarks."""
    return [bisect.bisect_right(watermarks, i) + 1 for i in range(n)]


def threshold(g, watermarks, horizon):
    """g as a horizon-H sweep would produce it: only where e + g <= H."""
    e = earliest_frames(watermarks, len(g))
    return [v if v != UNREACH and e[i] + v <= horizon else UNREACH
            for i, v in enumerate(g)]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('a')
    ap.add_argument('b')
    ap.add_argument('--threshold', type=int,
                    help='threshold A at this horizon before comparing')
    ap.add_argument('--meta', help='meta.json holding the watermarks')
    args = ap.parse_args()

    na, a = load(args.a)
    nb, b = load(args.b)
    print(f"rows: A={na} B={nb}")
    if na != nb:
        print("*** DIFFERENT ROW COUNTS ***")
        return 1
    if args.threshold is not None:
        if not args.meta:
            print("--threshold needs --meta")
            return 2
        watermarks = json.load(open(args.meta))['watermarks']
        kept = sum(1 for v in a if v != UNREACH)
        a = threshold(a, watermarks, args.threshold)
        print(f"A thresholded at H={args.threshold}: "
              f"{sum(1 for v in a if v != UNREACH)} of {kept} reachable rows kept")
    ra = sum(1 for v in a if v != UNREACH)
    rb = sum(1 for v in b if v != UNREACH)
    diff = [(i, x, y) for i, (x, y) in enumerate(zip(a, b)) if x != y]
    print(f"reachable: A={ra} B={rb}")
    print(f"differing entries: {len(diff)}")
    if diff:
        worse = sum(1 for _, x, y in diff if y > x)   # B more pessimistic
        better = len(diff) - worse
        print(f"  B larger (more pessimistic, prunes more): {worse}")
        print(f"  B smaller: {better}")
        for i, x, y in diff[:5]:
            print(f"  row {i}: A={x if x != UNREACH else 'unreach'} "
                  f"B={y if y != UNREACH else 'unreach'}")
        return 1
    print("g ARRAYS IDENTICAL")
    return 0


if __name__ == '__main__':
    sys.exit(main())
