import os, re, glob, hashlib

root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

def parse_kernel(path):
    t = open(path).read()
    m = re.search(r'pub const SHAPE: u64 = (\d+);', t)
    insh = m.group(1) if m else '?'
    outs = {}
    for m in re.finditer(r'pub const OUT_SHAPE_(\d+): &\[SCell\] = &\[(.*?)\n\];', t, re.S):
        outs.setdefault(int(m.group(1)), {})['shape'] = m.group(2).strip()
    for m in re.finditer(r'pub const OUT_GLOBALS_(\d+): &\[u32\] = &(\[[^\]]*\]);', t, re.S):
        outs.setdefault(int(m.group(1)), {})['globals'] = m.group(2).strip()
    for m in re.finditer(r'pub fn acc(\d+)\(.*?\) -> Rt2 \{(.*?)\n\}', t, re.S):
        i = int(m.group(1)); body = m.group(2)
        # classify each written cell: uniform-const (Col::U(AV::X(val))) vs per-lane placeholder Col::N(Vec::new())/Col::V
        uconst = {}
        perlane = set()
        for a in re.finditer(r'b\.cols\[(\d+)\]\s*=\s*(.+?);', body):
            c = int(a.group(1)); rhs = a.group(2)
            if 'Vec::new()' in rhs or rhs.startswith('Col::V'):
                perlane.add(c)
            elif rhs.startswith('Col::U(AV::UBool'):
                uconst[c] = 'UBool'
            elif rhs.startswith('Col::U('):
                uconst[c] = rhs
        d = outs.setdefault(i, {})
        d['uconst'] = uconst; d['perlane_acc'] = perlane
    # UBOOL cells (set in loop) — treat as uniform const 'UBool'
    for m in re.finditer(r'pub const OUT_UBOOL_(\d+): &\[\(u32, &str\)\] = &\[(.*?)\];', t, re.S):
        i = int(m.group(1))
        cells = [int(x) for x in re.findall(r'\((\d+),', m.group(2))]
        d = outs.setdefault(i, {}); d.setdefault('uconst', {})
        for c in cells: d['uconst'][c] = 'UBool'
    for m in re.finditer(r'pub fn append(\d+)\(.*?\n\}', t, re.S):
        i = int(m.group(1)); body = m.group(0)
        perlane = set(int(x) for x in re.findall(r'acc\.cols\[(\d+)\]', body))
        outs.setdefault(i, {})['perlane_append'] = perlane
    return insh, outs

rooms = sorted(glob.glob(os.path.join(root, 'crates', 'celeste-kernels-*', 'src', 'traced', 'room*')))
grand_unsound = 0
grand_reinflate = 0
for rdir in rooms:
    room = os.path.basename(rdir)
    files = sorted(glob.glob(os.path.join(rdir, 'kernel*.rs')))
    groups = {}
    for f in files:
        insh, outs = parse_kernel(f)
        for i, d in outs.items():
            if 'shape' not in d: continue
            h = hashlib.sha1((d.get('shape','')+'||'+d.get('globals','')).encode()).hexdigest()[:12]
            groups.setdefault(h, []).append((os.path.basename(f), insh, i, d))
    for h, members in groups.items():
        if len(set(m[1] for m in members)) < 2:
            continue
        # For each cell, gather across members: is it perlane (key) or uconst(value)?
        allcells = set()
        for (_,_,_,d) in members:
            allcells |= set(d.get('uconst',{})) | d.get('perlane_acc',set()) | d.get('perlane_append',set())
        for c in sorted(allcells):
            classes = []
            for (fn,insh,i,d) in members:
                pl = c in d.get('perlane_acc',set()) or c in d.get('perlane_append',set())
                uc = d.get('uconst',{}).get(c)
                if pl:
                    classes.append((fn,i,insh,'PERLANE'))
                elif uc is not None:
                    classes.append((fn,i,insh,('CONST',uc)))
                else:
                    classes.append((fn,i,insh,'ABSENT'))
            # UNSOUND: cell is CONST in >=2 members with DIFFERENT const values, and PERLANE in none-required.
            constvals = set(x[3][1] for x in classes if isinstance(x[3],tuple))
            has_perlane = any(x[3]=='PERLANE' for x in classes)
            if len(constvals) >= 2 and not has_perlane:
                grand_unsound += 1
                if grand_unsound <= 12:
                    print(f"[{room}] out={h} cell {c}: UNSOUND const-vs-const different values, PERLANE in none:")
                    for (fn,i,insh,cl) in classes:
                        print(f"      {fn}#{i}(in={insh[:8]}): {cl}")
            elif len(constvals) >= 2 and has_perlane:
                # const value differs across members AND perlane in some -> perlane member keys it, const members don't:
                # this is UNSOUND too: two const members with different values both drop it.
                grand_unsound += 1
                if grand_unsound <= 12:
                    print(f"[{room}] out={h} cell {c}: UNSOUND (>=2 distinct const values dropped; also perlane elsewhere):")
                    for (fn,i,insh,cl) in classes:
                        print(f"      {fn}#{i}(in={insh[:8]}): {cl}")
print(f"\nTOTAL: {grand_unsound} (cell,shared-shape) pairs where >=2 kernels bake DIFFERENT const values for a key-dropped cell (UNSOUND under naive kernel-key merge)")
