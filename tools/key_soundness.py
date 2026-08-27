import os, re, glob, hashlib

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

def parse_kernel(path):
    t = open(path).read()
    m = re.search(r'pub const SHAPE: u64 = (\d+);', t)
    insh = m.group(1) if m else '?'
    # output structures
    outs = {}
    for m in re.finditer(r'pub const OUT_SHAPE_(\d+): &\[SCell\] = &\[(.*?)\n\];', t, re.S):
        outs.setdefault(int(m.group(1)), {})['shape'] = m.group(2).strip()
    for m in re.finditer(r'pub const OUT_GLOBALS_(\d+): &\[u32\] = &(\[[^\]]*\]);', t, re.S):
        outs.setdefault(int(m.group(1)), {})['globals'] = m.group(2).strip()
    # acc bodies: const cell assignments
    for m in re.finditer(r'pub fn acc(\d+)\(.*?\) -> Rt2 \{(.*?)\n\}', t, re.S):
        i = int(m.group(1)); body = m.group(2)
        consts = {}
        for a in re.finditer(r'b\.cols\[(\d+)\]\s*=\s*(Col::U\(AV::\w+\([^)]*\)\)|Col::N\(Vec::new\(\)\)|Col::U\(AV::UBool\));', body):
            consts[int(a.group(1))] = a.group(2)
        # cells written per-lane in appendN: "Col::N(v) = &mut acc.cols[NN]" or Col::V etc
        outs.setdefault(i, {})['const_body'] = m.group(2)
        outs.setdefault(i, {})['consts'] = consts
    # append bodies: which cells get per-lane pushes
    for m in re.finditer(r'pub fn append(\d+)\(.*?\n\}', t, re.S):
        i = int(m.group(1)); body = m.group(0)
        perlane = set(int(x) for x in re.findall(r'acc\.cols\[(\d+)\]', body))
        outs.setdefault(i, {})['perlane'] = perlane
    return insh, outs

rooms = sorted(glob.glob(os.path.join(root, 'crates', 'celeste-kernels-*', 'src', 'traced', 'room*')))
for rdir in rooms:
    room = os.path.basename(rdir)
    files = sorted(glob.glob(os.path.join(rdir, 'kernel*.rs')))
    groups = {}  # out-struct-hash -> list of (file, insh, i, data)
    for f in files:
        insh, outs = parse_kernel(f)
        for i, d in outs.items():
            if 'shape' not in d: continue
            h = hashlib.sha1((d.get('shape','')+'||'+d.get('globals','')).encode()).hexdigest()[:12]
            groups.setdefault(h, []).append((os.path.basename(f), insh, i, d))
    unsound = 0
    checked = 0
    for h, members in groups.items():
        if len(set(m[1] for m in members)) < 2:
            continue
        checked += 1
        # compare consts and perlane sets across members
        # normalize: full effective per-cell value map. A cell is either const (from consts)
        # or per-lane (in perlane). Build signature: (perlane set, const map).
        base = None
        divergent = []
        for (fn, insh, i, d) in members:
            consts = d.get('consts', {})
            perlane = d.get('perlane', set())
            sig = (frozenset(perlane), tuple(sorted(consts.items())))
            if base is None:
                base = sig; base_desc = (fn, i)
            elif sig != base:
                divergent.append((fn, i, insh, consts, perlane))
        if divergent:
            unsound += 1
            print(f"[{room}] SHARED OUT {h}: DIVERGENT const/keycell layout across kernels!")
            (bfn, bi, binsh, bconsts, bperlane) = (members[0][0], members[0][2], members[0][1], members[0][3].get('consts',{}), members[0][3].get('perlane',set()))
            for (fn, i, insh, consts, perlane) in divergent[:3]:
                # find differing cells
                pl_diff = perlane.symmetric_difference(bperlane)
                cdiff = []
                for c in set(list(consts)+list(bconsts)):
                    if consts.get(c) != bconsts.get(c):
                        cdiff.append((c, bconsts.get(c), consts.get(c)))
                print(f"    vs base {bfn}#{bi}: {fn}#{i} perlane-diff={sorted(pl_diff)} const-diff(cell,base,this)={cdiff[:6]}")
    print(f"== {room}: {checked} shared-output groups checked, {unsound} with divergent const/key layout")
