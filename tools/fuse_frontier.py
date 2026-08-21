#!/usr/bin/env python3
"""P1 spike: how much extra node sharing would NORMALIZATION buy?

Reads two emitted kernel .rs files, rebuilds each member's value DAG the
way fuse.rs does (structural interning of (op, canonical operand ids)),
and reports sharing. Then applies normalization passes and re-reports, so
the delta is attributable.

Analysis only - emits nothing, changes nothing.
"""
import re
import sys
from collections import Counter

LET = re.compile(r'^\s*let (v\d+): ([A-Za-z0-9_(), ]+?) = (.*);\s*$')
IDENT = re.compile(r'[A-Za-z_][A-Za-z0-9_]*')


def parse(path):
    """-> list of (name, ty, expr) in order, for frame() + suffix()."""
    out = []
    for line in open(path):
        m = LET.match(line)
        if m:
            name, ty, expr = m.group(1), m.group(2).strip(), m.group(3).strip()
            out.append((name, ty, expr))
    return out


def canon(expr, vn):
    """Replace known member-local names by <id>, as fuse.rs canonicalize does."""
    def sub(m):
        s = m.group(0)
        return '<%d>' % vn[s] if s in vn else s
    return IDENT.sub(sub, expr)


CONST_RE = re.compile(r'^P8::from_raw\((-?\d+)i32\)$|^P8::from_i16\((-?\d+)\)$')
# splat of a uniform pair/scalar; folding these is the main normalization win
SPLAT_RE = re.compile(r'^(zn|zb|zi)_splat\((.*)\)$')
COMMUTATIVE = {'zn_add', 'zn_mul', 'zn_min', 'zn_max', 'si_add'}


def build(path, normalize=False, intern=None, consts=None):
    """Intern nodes into a SHARED table (as fuse.rs Ctx does, so operand
    ids are globally canonical and structural hashing composes bottom-up).
    Returns the set of node ids this member contains."""
    vn = {}
    mine = set()
    if intern is None:
        intern = {}
    if consts is None:
        consts = {}
    for name, ty, expr in parse(path):
        c = canon(expr, vn)

        if normalize:
            # 1. constant folding of splats over known constants:
            #    zn_splat(<k>, <k>) where k is a constant literal folds to
            #    a canonical constant-splat key, so a member that already
            #    folded it and one that did not agree.
            m = SPLAT_RE.match(c)
            if m:
                args = [a.strip() for a in m.group(2).split(',')]
                lits = [consts.get(int(a[1:-1])) for a in args
                        if a.startswith('<') and a.endswith('>')]
                if lits and all(l is not None for l in lits):
                    c = '%s_splat_const(%s)' % (m.group(1), ','.join(lits))
            # 2. canonical operand order for commutative ops
            for op in COMMUTATIVE:
                if c.startswith(op + '('):
                    inner = c[len(op) + 1:-1]
                    parts = [p.strip() for p in inner.split(',')]
                    dp = [p for p in parts if 'dp' in p]
                    val = sorted(p for p in parts if 'dp' not in p)
                    c = '%s(%s)' % (op, ', '.join(val + dp))
                    break

        key = (ty, c)
        if key in intern:
            nid = intern[key]
        else:
            nid = len(intern)
            intern[key] = nid
        vn[name] = nid
        mine.add(nid)
        cm = CONST_RE.match(expr)
        if cm:
            consts[nid] = expr
    return mine, intern


def run(a_path, b_path, normalize):
    intern, consts = {}, {}
    A, _ = build(a_path, normalize, intern, consts)
    B, _ = build(b_path, normalize, intern, consts)
    return A, B, intern


def report(label, A, B):
    print('%-22s A %5d   B %5d   shared %5d   union %5d' %
          (label, len(A), len(B), len(A & B), len(A | B)))


def frontier(A, B, intern):
    """Is the divergence CASCADING? A node is on the divergence FRONTIER if
    it is A-only but every one of its operands is shared. Everything else
    that is A-only is downstream of some frontier node - it could never
    match no matter how well we normalise, because its inputs already
    differ. A small frontier + a big cone means the fix belongs upstream
    (derivation), not in the fuser."""
    byid = {v: k for k, v in intern.items()}
    shared = A & B
    front, cascade = [], 0
    for nid in A - B:
        ty, c = byid[nid]
        ops = [int(x) for x in re.findall(r'<(\d+)>', c)]
        if all(o in shared for o in ops):
            front.append((nid, ty, c))
        else:
            cascade += 1
    print('  divergence frontier: %d nodes (all operands shared)' % len(front))
    print('  downstream cascade:  %d nodes (>=1 operand already diverged)' % cascade)
    for nid, ty, c in front[:6]:
        print('    frontier eg: %s = %s' % (ty, c[:90]))


def classify(A, B, intern):
    """Why is each A-only node unshared? key -> id, so invert."""
    byid = {v: k for k, v in intern.items()}
    ops = Counter()
    for nid in A - B:
        ty, c = byid[nid]
        op = c.split('(')[0] if '(' in c else c.split('::')[0]
        ops[op] += 1
    print('  top ops among A-only nodes:',
          ', '.join('%s %d' % (o, n) for o, n in ops.most_common(8)))


if __name__ == '__main__':
    a, b = sys.argv[1], sys.argv[2]
    print('A = %s\nB = %s\n' % (a, b))
    A, B, intern = run(a, b, False)
    report('structural (today)', A, B)
    A2, B2, intern2 = run(a, b, True)
    report('+ normalization', A2, B2)
    print()
    classify(A, B, intern)
