import sys, glob, os, re, collections
d = sys.argv[1]
tot = collections.Counter(); lanes = collections.Counter()
perk = []
fork_stats = collections.Counter(); fork_lanes = collections.Counter()
for f in sorted(glob.glob(d + '/*.smt2')):
    base = f[:-5]; sym = os.path.basename(base)
    meta = {}
    for l in open(base + '.meta'):
        j, t, e, nr, sp = l.rstrip('\n').split('\t')
        meta[int(j)] = (int(t), int(e), int(nr), eval(sp))
    forks = open(base + '.forks').read().split('\n')
    out = open(base + '.out').read() if os.path.exists(base + '.out') else ''
    res = {}
    for m in re.finditer(r'^Q (\d+)\n(\S+)', out, re.M):
        res[int(m.group(1))] = m.group(2)
    kc = collections.Counter()
    for j, (t, e, nr, sp) in meta.items():
        r = res.get(j, 'missing')
        cls = 'dup' if e == 0 else 'new'
        tot[(cls, r)] += 1; lanes[(cls, r)] += t
        kc[(cls, r)] += 1
        if cls == 'dup':
            # which fork(s) separate j from its refs: take j's splits vs nonzero
            on = [forks[k] for k, v in enumerate(sp) if v and k < len(forks)]
            fork_stats[(r, ','.join(on))] += 1; fork_lanes[(r, ','.join(on))] += t
    perk.append((sym, dict(kc)))
for k, v in sorted(tot.items()): print(k, v, 'lanes', lanes[k])
print('--- per kernel (top by dup unsat) ---')
perk.sort(key=lambda x: -x[1].get(('dup','unsat'),0))
for sym, kc in perk[:int(sys.argv[2]) if len(sys.argv)>2 else 12]: print(sym, kc)
print('--- dup bodies by result and pressed forks (top) ---')
for k, v in fork_stats.most_common(30): print(k, v, fork_lanes[k])
