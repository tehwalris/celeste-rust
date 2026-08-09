"""Compare two g.bin arrays (min-frames-to-exit per row id).

Format: "C8TB", u32 format version, u64 count, then a zstd stream of
count u16-LE values. G_UNREACHABLE is u16::MAX.
"""
import sys, subprocess, struct

def load(path):
    raw = open(path, 'rb').read()
    assert raw[:4] == b'C8TB', raw[:4]
    n = struct.unpack('<Q', raw[8:16])[0]
    pay = subprocess.run(['zstd', '-d', '-c'], input=raw[16:], capture_output=True).stdout
    return n, struct.unpack_from(f'<{n}H', pay, 0)

na, a = load(sys.argv[1])
nb, b = load(sys.argv[2])
UNREACH = 0xFFFF
print(f"rows: A={na} B={nb}")
if na != nb:
    print("*** DIFFERENT ROW COUNTS ***"); sys.exit(1)
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
        print(f"  row {i}: A={x if x!=UNREACH else 'unreach'} B={y if y!=UNREACH else 'unreach'}")
    sys.exit(1)
print("g ARRAYS IDENTICAL")
