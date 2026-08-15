"""Compare two g arrays AS FUNCTIONS OF THE ROW, not of the row id.

g.bin is indexed by row id, and ids are assignment order. Any change that
alters discovery order - a different chunking, an extra column, a different
thread count - permutes the ids while leaving the search's answer alone. So
comparing two g.bin arrays elementwise answers "were rows discovered in the
same order?", which is not a question anyone wants, and it reports a huge
difference for a run that is semantically identical. (Observed: 24,750 of
32,486 reachable rows "differing" between two runs whose g multisets were
bit-for-bit identical.)

This joins each g value to its row's 128-bit KEY via visited.bin, which is
COLUMNAR - count, then all low halves, then all high halves - see
tools/rowset.py. Then it compares the key -> g maps.

    tools/gjoin.py A/g.bin A/fNNN/visited.bin B/g.bin B/fNNN/visited.bin
"""
import sys, struct, subprocess, collections

UNREACH = 0xFFFF


def read_payload(path):
    raw = open(path, 'rb').read()
    assert raw[:4] == b'C8TB', (path, raw[:4])
    return raw, subprocess.run(['zstd', '-d', '-c'], input=raw[16:],
                               capture_output=True).stdout


def g_values(path):
    raw, pay = read_payload(path)
    n = struct.unpack('<Q', raw[8:16])[0]
    return list(struct.unpack_from(f'<{n}H', pay, 0))


def keys_in_id_order(path):
    _, pay = read_payload(path)
    n = struct.unpack('<Q', pay[:8])[0]
    los = struct.unpack_from(f'<{n}Q', pay, 8)
    his = struct.unpack_from(f'<{n}Q', pay, 8 + 8 * n)
    return [f"{lo:016x}{hi:016x}" for lo, hi in zip(los, his)]


def keyed(g_path, visited_path):
    g = g_values(g_path)
    k = keys_in_id_order(visited_path)
    if len(g) != len(k):
        sys.exit(f"{g_path} has {len(g)} g values but {visited_path} has {len(k)} rows")
    return dict(zip(k, g))


def main():
    a = keyed(sys.argv[1], sys.argv[2])
    b = keyed(sys.argv[3], sys.argv[4])
    only_a = a.keys() - b.keys()
    only_b = b.keys() - a.keys()
    shared = a.keys() & b.keys()
    diff = {k: (a[k], b[k]) for k in shared if a[k] != b[k]}
    print(f"rows: A={len(a)} B={len(b)} shared={len(shared)} "
          f"onlyA={len(only_a)} onlyB={len(only_b)}")
    reach_a = sum(1 for v in a.values() if v != UNREACH)
    reach_b = sum(1 for v in b.values() if v != UNREACH)
    print(f"reachable: A={reach_a} B={reach_b}")
    print(f"differing on shared rows: {len(diff)}")
    if diff:
        worse = sum(1 for x, y in diff.values() if y > x or (y == UNREACH and x != UNREACH))
        print(f"  B larger/unreachable (prunes MORE - the unsound direction): {worse}")
        for k, (x, y) in list(diff.items())[:5]:
            print(f"  {k}: A={x} B={y}")
    if not diff and not only_a and not only_b:
        print("IDENTICAL as a function of the row")


main()
