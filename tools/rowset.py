import sys, hashlib, subprocess, struct
path = sys.argv[1]
raw = open(path,'rb').read()
assert raw[:4] == b'C8TB', raw[:4]
payload = subprocess.run(['zstd','-d','-c'], input=raw[16:], capture_output=True).stdout
n = struct.unpack('<Q', payload[:8])[0]
body = payload[8:8+16*n]
assert len(body) == 16*n, (len(body), n)
rows = sorted(body[i*16:(i+1)*16] for i in range(n))
h = hashlib.sha256()
for r in rows: h.update(r)
print(f"{n} rows, set-digest {h.hexdigest()[:16]}")
