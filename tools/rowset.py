"""Digest of the SET of row keys in a checkpoint's visited.bin.

The file is COLUMNAR - a u64 count, then every key's low half, then every
key's high half - and it is written in row-id order. Ids are assignment
order and legitimately differ between runs, so a set digest is the only
meaningful comparison; reading it as interleaved pairs silently produces a
digest of the id ORDER instead, which looks like a semantic difference.
"""
import sys, hashlib, subprocess, struct

def keys(path):
    raw = open(path, 'rb').read()
    assert raw[:4] == b'C8TB', raw[:4]
    pay = subprocess.run(['zstd', '-d', '-c'], input=raw[16:], capture_output=True).stdout
    n = struct.unpack('<Q', pay[:8])[0]
    los = struct.unpack_from(f'<{n}Q', pay, 8)
    his = struct.unpack_from(f'<{n}Q', pay, 8 + 8 * n)
    return {f"{lo:016x}{hi:016x}" for lo, hi in zip(los, his)}

if __name__ == '__main__':
    ks = keys(sys.argv[1])
    h = hashlib.sha256()
    for k in sorted(ks):
        h.update(k.encode())
    print(f"{len(ks)} rows, set-digest {h.hexdigest()[:16]}")
