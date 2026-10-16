#!/usr/bin/env python3
"""Per-line cycle attribution through INLINED frames (branch `fwd-perf`).

The release build has line tables (`debug = 1`) but no frame pointers, so
`perf record -g` call chains are unusable and `perf report` lumps every
inlined callee into its outer function (`run_slice` ~60%). This resolves
each sampled address's inline chain with `addr2line -i` and attributes the
samples by line inside a chosen function.

    perf script -i perf.data -F tid,time,ip,sym,dso -G > samples.txt
    perf script -i perf.data --show-mmap-events | grep MMAP | grep release/rewrite
      # -> [0xBASE(...) @ 0xPGOFF ...]: the binary's text mapping
    tools/perf_inline.py resolve samples.txt BINARY BASE DELTA 'TID_EXPR' out.json
      # DELTA = PGOFF + (the R E segment's vaddr - its file offset), e.g.
      # 0x11b000 + 0x1000 = 0x11c000 (`readelf -lW BINARY | grep LOAD`);
      # TID_EXPR: a Python expression over `t`, e.g. 't in {1,2,3}' or 'True'
    tools/perf_inline.py outer out.json                    # by outermost function
    tools/perf_inline.py within out.json FUNC [LOC] [DEPTH] # by line in FUNC
      # (the outermost frame whose function contains FUNC; with LOC, only
      # samples at that file:line of it, split DEPTH frames further in)
"""
import collections
import json
import re
import subprocess
import sys


def short(loc):
    m = re.search(r'([^/]+\.rs):(\d+)', loc)
    return f"{m.group(1)}:{m.group(2)}" if m else loc.split('/')[-1]


def resolve(samples, binary, base, delta, tid_expr, out):
    sel = eval('lambda t: ' + tid_expr)
    ips, other, total = collections.Counter(), collections.Counter(), 0
    for line in open(samples):
        p = line.split(None, 3)
        if len(p) < 4 or not sel(int(p[0])):
            continue
        total += 1
        if '(' + binary + ')' in p[3]:
            ips[int(p[2], 16) - base + delta] += 1
        else:
            m = re.match(r'(.*) \((.*)\)$', p[3].strip())
            dso = m.group(2) if m else '?'
            other['KERNEL .so' if 'asm-scratch' in dso else ('[k]' if 'kallsyms' in dso else (m.group(1) if m else '?'))] += 1
    addrs = sorted(ips)
    r = subprocess.run(['addr2line', '-i', '-f', '-C', '-a', '-e', binary], input='\n'.join(hex(a) for a in addrs) + '\n', capture_output=True, text=True)
    chains, cur, lines, i = {}, None, r.stdout.split('\n'), 0
    while i < len(lines):
        if lines[i].startswith('0x'):
            cur = int(lines[i], 16)
            chains[cur] = []
            i += 1
        elif cur is not None and i + 1 < len(lines):
            chains[cur].append((lines[i], lines[i + 1]))
            i += 2
        else:
            i += 1
    json.dump({'total': total, 'ips': {hex(a): c for a, c in ips.items()}, 'chains': {hex(a): v for a, v in chains.items()}, 'other': other}, open(out, 'w'))
    print(f"{total} samples, {len(addrs)} addresses")


def outer(d, n=30):
    tot, by = d['total'], collections.Counter()
    for a, c in d['ips'].items():
        ch = d['chains'].get(a, [])
        by[ch[-1][0] if ch else '??'] += c
    for k, c in d['other'].items():
        by[k] += c
    for k, c in by.most_common(n):
        print(f"{100 * c / tot:6.2f}%  {k[:120]}")


def within(d, func, loc=None, depth=1, n=40):
    tot, by, s = d['total'], collections.Counter(), 0
    for a, c in d['ips'].items():
        ch = d['chains'].get(a, [])
        js = [j for j, (f, l) in enumerate(ch) if func in f and (loc is None or loc in l)]
        if not js:
            continue
        j = max(js)
        s += c
        key = short(ch[j][1]) + '  ' + (' > '.join(f"{ch[k][0].split('::')[-1][:30]}@{short(ch[k][1])}" for k in range(j - 1, max(j - 1 - depth, -1), -1)) or '(own)')
        by[key] += c
    print(f"{func}{' at ' + loc if loc else ''}: {100 * s / tot:.2f}% of {tot} samples")
    for k, c in by.most_common(n):
        print(f"{100 * c / tot:6.2f}%  {k}")


if __name__ == '__main__':
    cmd = sys.argv[1]
    if cmd == 'resolve':
        resolve(sys.argv[2], sys.argv[3], int(sys.argv[4], 16), int(sys.argv[5], 16), sys.argv[6], sys.argv[7])
    elif cmd == 'outer':
        outer(json.load(open(sys.argv[2])))
    elif cmd == 'within':
        a = sys.argv[2:]
        within(json.load(open(a[0])), a[1], a[2] if len(a) > 2 else None, int(a[3]) if len(a) > 3 else 1)
    else:
        sys.exit(__doc__)
