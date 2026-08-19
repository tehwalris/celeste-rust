"""Per-frame rowkey SET comparison between two checkpoint dirs.

Reads `frames/*.rowkeys` (magic C8RK v1: 32-byte header with a u64 count
at offset 8, then 24-byte records (lo u64, hi u64, id u32, pad u32),
SORTED by key). Row ids are assignment order and legitimately differ
between runs, so they are stripped; because records are key-sorted,
equal sets <=> equal stripped byte streams, no re-sort needed.

Usage: python3 tools/framesetdiff.py DIR_A DIR_B
Prints one line per frame and a summary; exit 1 on any mismatch. This is
the "all N per-frame rowkey sets identical" gate used for engine A/Bs
(BENCHMARK_DATA.md, "Engine adoption validation at depth").
"""
import sys, os, struct, hashlib

HEADER, RECORD = 32, 24

def keys_bytes(path):
    raw = open(path, 'rb').read()
    assert raw[:4] == b'C8RK', (path, raw[:4])
    n = struct.unpack_from('<Q', raw, 8)[0]
    assert HEADER + n * RECORD == len(raw), (path, n, len(raw))
    out = bytearray(n * 16)
    for i in range(n):
        out[i*16:(i+1)*16] = raw[HEADER + i*RECORD : HEADER + i*RECORD + 16]
    return n, bytes(out)

def main(a_dir, b_dir):
    fa = sorted(f for f in os.listdir(os.path.join(a_dir, 'frames')) if f.endswith('.rowkeys'))
    fb = sorted(f for f in os.listdir(os.path.join(b_dir, 'frames')) if f.endswith('.rowkeys'))
    if fa != fb:
        print(f"frame lists differ: onlyA={sorted(set(fa)-set(fb))} onlyB={sorted(set(fb)-set(fa))}")
        return 1
    bad, tot_a, tot_b = 0, 0, 0
    for f in fa:
        na, ka = keys_bytes(os.path.join(a_dir, 'frames', f))
        nb, kb = keys_bytes(os.path.join(b_dir, 'frames', f))
        tot_a += na; tot_b += nb
        if na == nb and ka == kb:
            print(f"{f}: OK n={na} digest={hashlib.sha256(ka).hexdigest()[:16]}")
        else:
            bad += 1
            sa = {ka[i*16:(i+1)*16] for i in range(na)}
            sb = {kb[i*16:(i+1)*16] for i in range(nb)}
            print(f"{f}: MISMATCH |A|={na} |B|={nb} shared={len(sa&sb)} "
                  f"onlyA={len(sa-sb)} onlyB={len(sb-sa)}")
    print(f"== {len(fa)} frames, {bad} mismatching; total rows A={tot_a} B={tot_b}")
    return 1 if bad else 0

if __name__ == '__main__':
    sys.exit(main(sys.argv[1], sys.argv[2]))
