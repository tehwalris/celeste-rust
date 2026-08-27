import os, re, glob, hashlib, sys

# For each generated kernel, extract input SHAPE, and each OUT_SHAPE_i / OUT_GLOBALS_i block.
# Compute a normalized hash of the output STRUCTURE (SCell list) and check whether the
# same output structure is produced by more than one input-shape kernel.

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

def blocks(text, prefix):
    out = {}
    for m in re.finditer(rf'pub const {prefix}_(\d+): &\[SCell\] = &\[(.*?)\n\];', text, re.S):
        out[int(m.group(1))] = m.group(2).strip()
    return out

def globs(text, prefix):
    out = {}
    for m in re.finditer(rf'pub const {prefix}_(\d+): &\[u32\] = &(\[[^\]]*\]);', text, re.S):
        out[int(m.group(1))] = m.group(2).strip()
    return out

rooms = sorted(glob.glob(os.path.join(root, 'crates', 'celeste-kernels-*', 'src', 'traced', 'room*')))
for rdir in rooms:
    room = os.path.basename(rdir)
    files = sorted(glob.glob(os.path.join(rdir, 'kernel*.rs')))
    # map output-structure-hash -> list of (kernel input shape, outcome)
    outmap = {}
    inshapes = set()
    for f in files:
        t = open(f).read()
        m = re.search(r'pub const SHAPE: u64 = (\d+);', t)
        insh = m.group(1) if m else '?'
        inshapes.add(insh)
        outs = blocks(t, 'OUT_SHAPE')
        gl = globs(t, 'OUT_GLOBALS')
        for i, body in outs.items():
            key = hashlib.sha1((body + '||' + gl.get(i,'')).encode()).hexdigest()[:12]
            outmap.setdefault(key, []).append((os.path.basename(f), insh, i))
    shared = {k:v for k,v in outmap.items() if len(set(x[1] for x in v)) > 1}
    print(f"== {room}: {len(files)} kernels, {len(inshapes)} input shapes, {len(outmap)} distinct output structures")
    if shared:
        print(f"   {len(shared)} output structures produced by >1 INPUT-SHAPE kernel:")
        for k,v in shared.items():
            print(f"     out={k}: " + ", ".join(f"{a}#{c}(in={s[:8]})" for a,s,c in v))
    else:
        print("   NO output structure is produced by more than one input-shape kernel.")
