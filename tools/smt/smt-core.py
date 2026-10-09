# smt-core.py FILE (tier-B dump): name every range assertion, ask for an unsat core per UNSAT query.
import sys, re, subprocess, collections
f = sys.argv[1]
names = {l.split('\t')[0]: l.rstrip('\n').split('\t')[2] for l in open(f + '.names')}
res = {int(m.group(1)): m.group(2) for m in re.finditer(r'^Q (\d+)\n(\S+)', open(f + '.out').read(), re.M)}
lines = open(f + '.smt2').read().split('\n')
out = []; tags = {}; k = 0
for l in lines:
    if l.startswith('(assert ') and not l.startswith('(assert (and take') and 'define' not in l and l.count('c') and re.search(r'\bc\d+[lhvk]?\b', l) and not l.startswith('(assert (and take'):
        if l.startswith('(assert (bvsle c') and l.count(' ') == 2:  # lo<=hi of an interval: keep unnamed
            out.append(l); continue
        cell = re.search(r'\bc(\d+)[lhvk]?\b', l).group(1)
        k += 1; tags['b%d' % k] = names.get(cell, cell) + ' ' + re.sub(r'\s+', ' ', l)[8:-1]
        out.append('(assert (! %s :named b%d))' % (l[8:-1], k))
    elif l == '(check-sat)':
        out.append(l); out.append('(get-unsat-core)')
    elif l.startswith('(get-value'):
        continue
    elif l.startswith('(push') or l.startswith('(pop'):
        out.append(l)
    else:
        out.append(l)
txt = '(set-option :produce-unsat-cores true)\n(set-option :timeout 20000)\n' + '\n'.join(out)
# only the unsat queries
r = subprocess.run(['/var/tmp/z3venv/bin/z3', '-in'], input=txt, capture_output=True, text=True)
cores = collections.Counter(); n = 0
for m in re.finditer(r'^Q (\d+)\n(\S+)\n(\([^\n]*\))', r.stdout, re.M):
    if m.group(2) != 'unsat': continue
    n += 1
    for b in m.group(3).strip('()').split():
        cores[tags.get(b, b)] += 1
print(f, 'unsat cores of', n, 'queries')
for c, v in cores.most_common(): print('  %4d  %s' % (v, c))
