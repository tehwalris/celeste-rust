import sys, subprocess, struct
def load(path):
    raw = open(path,'rb').read()
    payload = subprocess.run(['zstd','-d','-c'], input=raw[16:], capture_output=True).stdout
    n = struct.unpack('<Q', payload[:8])[0]
    body = payload[8:8+16*n]
    return {body[i*16:(i+1)*16] for i in range(n)}
a = load(sys.argv[1]); b = load(sys.argv[2])
print(f"|A|={len(a)} |B|={len(b)} shared={len(a&b)} onlyA={len(a-b)} onlyB={len(b-a)}")
