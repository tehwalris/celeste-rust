#!/usr/bin/env python3
"""Aggregate a CELESTE_KERNEL_MIX directory (src/compiled/mix.rs) into the
static and dynamic instruction mix of the kernels (plans/kernel-mix.md).

  tools/kernel_mix.py DIR                 # static + dynamic (slices x static)
  tools/kernel_mix.py DIR --perf PERFDATA # + cycles per category from a profile
  tools/kernel_mix.py DIR --snippet SYM --at K [--len N]

The dynamic count is exact for the kernel bodies: a kernel is straight-line
code, so each slice executes every instruction of its kernel once (a
call-out's callee is not in the kernel and not counted).
"""
import argparse
import bisect
import collections
import os
import re
import subprocess
import sys


def load_tsv(path):
    out = {}
    with open(path) as f:
        for line in f:
            k, v = line.rstrip("\n").split("\t")
            out[k] = int(v)
    return out


def load(d):
    calls = {}
    with open(os.path.join(d, "calls.tsv")) as f:
        next(f)
        for line in f:
            sym, n = line.split()
            calls[sym] = calls.get(sym, 0) + int(n)
    kern = {}
    for name in os.listdir(d):
        if name.endswith(".tsv") and name != "calls.tsv":
            kern[name[:-4]] = load_tsv(os.path.join(d, name))
    return calls, kern


def pct(a, b):
    return 100.0 * a / b if b else 0.0


def group(t, prefix, depth=1):
    """Sum keys `prefix.X...` by the first `depth` components of X."""
    out = collections.Counter()
    for k, v in t.items():
        if k.startswith(prefix + "."):
            rest = k[len(prefix) + 1:]
            out[".".join(rest.split(".")[:depth]) if depth else rest] += v
    return out


def weighted(calls, kern, syms=None):
    tot = collections.Counter()
    for sym, t in kern.items():
        if syms is not None and sym not in syms:
            continue
        w = calls.get(sym, 0)
        for k, v in t.items():
            tot[k] += w * v
    return tot


def mix_line(t, label):
    ins = t["insts"]
    cats = group(t, "cat", 1)
    s = f"{label}: {ins:,} insts | " + " ".join(
        f"{c} {pct(cats[c], ins):.1f}%" for c in "abcd")
    return s


def table(t, prefix, total, top=40, depth=0):
    rows = group(t, prefix, depth).most_common(top)
    return "\n".join(f"  {k:<40} {v:>16,} {pct(v, total):6.2f}%" for k, v in rows)


def perf_cycles(d, perfdata, kern):
    """Samples per (category, class, family) of the kernel .so's, mapped by
    instruction index (objdump order = `.lines` order)."""
    out = subprocess.run(["perf", "script", "-i", perfdata, "-F", "ip,sym,symoff,dso"],
                         capture_output=True, text=True, check=True).stdout
    per = collections.defaultdict(list)  # sym -> offsets
    other = collections.Counter()
    for line in out.splitlines():
        m = re.search(r"(kernel_k\w+)\+0x([0-9a-f]+)", line)
        if m:
            per[m.group(1)].append(int(m.group(2), 16))
        else:
            m2 = re.search(r"\(([^)]*)\)\s*$", line)
            other[os.path.basename(m2.group(1)) if m2 else "?"] += 1
    res = collections.Counter()
    res_cls = collections.Counter()
    res_fam = collections.Counter()
    res_op = collections.Counter()
    res_kern = collections.Counter()
    res_fold = collections.Counter()
    hot = collections.defaultdict(collections.Counter)
    for sym, offs in per.items():
        so = os.path.join(d, sym + ".so")
        lines_p = os.path.join(d, sym + ".lines")
        if not os.path.exists(so):
            other["(no report) " + sym] += len(offs)
            continue
        dis = subprocess.run(["objdump", "-d", "--no-show-raw-insn", so],
                             capture_output=True, text=True, check=True).stdout
        addrs, start, insym = [], None, False
        for l in dis.splitlines():
            m = re.match(r"^([0-9a-f]+) <(.*)>:", l)
            if m:
                insym = m.group(2) == sym
                if insym:
                    start = int(m.group(1), 16)
                continue
            if insym:
                m = re.match(r"^\s+([0-9a-f]+):\s+\S", l)
                if m:
                    addrs.append(int(m.group(1), 16) - start)
        rows = [l.rstrip("\n").split("\t") for l in open(lines_p)]
        if len(rows) != len(addrs):
            print(f"[mix] {sym}: {len(rows)} report rows vs {len(addrs)} instructions: skipped",
                  file=sys.stderr)
            other["(misaligned) " + sym] += len(offs)
            continue
        for o in offs:
            i = bisect.bisect_right(addrs, o) - 1
            cat, cls, fam, op, fold, spiked = rows[i]
            res[cat] += 1
            if fold == "1":
                res_fold["bit identities"] += 1
            if spiked == "1":
                res_fold["spikes_at fold"] += 1
            elif fold == "1":
                res_fold["bit identities on the rest"] += 1
            res_cls[cls] += 1
            res_kern[sym] += 1
            res_op[op] += 1
            if cls.startswith("guard"):
                res_fam[fam] += 1
            hot[sym][i] += 1
    return res, res_cls, res_fam, res_op, res_kern, other, hot, res_fold


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("dir")
    ap.add_argument("--perf")
    ap.add_argument("--top", type=int, default=12)
    ap.add_argument("--snippet")
    ap.add_argument("--at", type=int, default=0)
    ap.add_argument("--len", type=int, default=40)
    a = ap.parse_args()
    if a.snippet:
        rows = [l.rstrip("\n").split("\t") for l in open(os.path.join(a.dir, a.snippet + ".lines"))]
        insts = []
        for l in open(os.path.join(a.dir, a.snippet + ".s")):
            if l.startswith(".section"):
                break
            if l.startswith("    ") and not l.strip().startswith("."):
                insts.append(l.strip())
        for i in range(a.at, min(a.at + a.len, len(insts))):
            cat, cls, fam, op, fold, spiked = rows[i]
            print(f"{i:7} {insts[i]:<58} # {cat:<12} {cls:<9} {op:<22} {fam}{' FOLD' if fold == '1' else ''}{' SPIKE' if spiked == '1' else ''}")
        return
    calls, kern = load(a.dir)
    missing = [s for s in calls if s not in kern and calls[s]]
    if missing:
        print("kernels that ran without a report:", missing)
    ran = {s for s, n in calls.items() if n and s in kern}
    print(f"{len(kern)} kernels reported, {len(ran)} ran; {sum(calls.values()):,} slices")
    # Static, per kernel, by dynamic weight.
    order = sorted(ran, key=lambda s: -calls[s] * kern[s]["insts"])
    dyn = weighted(calls, kern)
    tot = dyn["insts"]
    print(f"\n== per kernel (top {a.top} by slices x insts) ==")
    for s in order[: a.top]:
        t = kern[s]
        share = pct(calls[s] * t["insts"], tot)
        sp = t.get("cat.c.spill", 0) + t.get("cat.c.reload", 0)
        print(f"{s[:40]:<40} slices {calls[s]:>9,} dyn {share:5.1f}% | {mix_line(t, 'static')} | "
              f"spill+reload {pct(sp, t['insts']):.1f}% const {pct(t.get('cat.c.const', 0), t['insts']):.1f}% "
              f"| nodes {t['nodes']:,} bodies {t['bodies']} slots {t.get('spill_slots', 0)}")
    stat = collections.Counter()
    for s in ran:
        stat.update(kern[s])
    print("\n== STATIC (sum over the kernels that ran) ==")
    print(mix_line(stat, "static"))
    print(table(stat, "cat", stat["insts"]))
    print("\n== DYNAMIC (slices x static) ==")
    print(mix_line(dyn, "dynamic"))
    print(table(dyn, "cat", tot))
    print("\n-- dynamic by role class --")
    print(table(dyn, "cls", tot))
    print("\n-- dynamic by category x class --")
    print(table(dyn, "catcls", tot, top=60))
    print("\n-- dynamic by (op/kind) --")
    print(table(dyn, "opkind", tot, top=40))
    print("\n-- dynamic guard-only lines by family --")
    print(table(dyn, "famlines", tot, top=40))
    print(f"\n-- foldable by bit identities: {dyn['fold.lines']:,} lines ({pct(dyn['fold.lines'], tot):.1f}%) --")
    print(table(dyn, "foldcat", tot))
    print(table(dyn, "foldcls", tot))
    print(f"\n-- spikes_at folded (every Eq(k, mget) false): {dyn['spikefold.lines']:,} lines removable ({pct(dyn['spikefold.lines'], tot):.1f}%); "
          f"kernels whose region reaches no spike tile: {sum(kern[s].get('spikefold.reach.none', 0) for s in ran)} of {len(ran)}, "
          f"{pct(sum(calls[s] * kern[s]['insts'] for s in ran if kern[s].get('spikefold.reach.none')), tot):.1f}% of the dynamic lines")
    print(f"   lines removable in kernels that reach no spike: {sum(calls[s] * kern[s].get('spikefold.lines', 0) for s in ran if kern[s].get('spikefold.reach.none')):,}")
    print(table(dyn, "spikefoldcat", tot))
    both = dyn['spikefold.lines'] + dyn['fold_after_spike.lines']
    print(f"   + bit identities on what is left: {dyn['fold_after_spike.lines']:,}; together {both:,} ({pct(both, tot):.1f}%)")
    print(f"\n-- embedded {{1to16}} constant operands: {dyn['constop']:,} ({pct(dyn['constop'], tot):.1f}% of lines)")
    print("\n== GRAPH (static, the kernels that ran; dynamic = slices-weighted) ==")
    print(f"nodes {stat['nodes']:,}; weighted {dyn['nodes']:,}")
    print(table(stat, "nodecls", stat["nodes"]))
    print("-- weighted --")
    print(table(dyn, "nodecls", dyn["nodes"]))
    print("-- weighted nodes by op.class --")
    print(table(dyn, "node", dyn["nodes"], top=50))
    print("-- weighted guard-only nodes by family --")
    gn = sum(v for k, v in dyn.items() if k.startswith("famnodes."))
    print(table(dyn, "famnodes", gn, top=40))
    print("-- weighted guard nodes private to one body vs shared --")
    print(table(dyn, "guardbodies", gn))
    print("-- weighted term / conjunct occurrences by family --")
    ft = sum(v for k, v in dyn.items() if k.startswith("famterms."))
    print(table(dyn, "famterms", ft, top=40))
    for k in ["bodies", "bodies.distinct_error", "bodies.distinct_live", "error_terms", "live_conjuncts", "roots"]:
        print(f"  {k}: static {stat[k]:,} weighted-per-slice {dyn[k] / max(1, sum(calls[s] for s in ran)):.1f}")
    print(table(stat, "rootrole", stat["roots"]))
    print("-- weighted nodes by class.leaf (what the cone reads: T tile call, M mget call, F fork, A arithmetic, - inputs only) --")
    print(table(dyn, "leafnodes", dyn["nodes"], top=30))
    print("-- dynamic lines by class.leaf --")
    print(table(dyn, "leaflines", tot, top=30))
    print(f"-- error OR trees: {stat['err_or_nodes']:,} distinct OR nodes (weighted {dyn['err_or_nodes']:,}); "
          f"with one shared premise of the {stat['err_input_terms']:,} input-only terms: {stat['err_or_hoisted']:,} (weighted {dyn['err_or_hoisted']:,})")
    if a.perf:
        res, cls, fam, op, kk, other, hot, rfold = perf_cycles(a.dir, a.perf, kern)
        n = sum(res.values())
        allsamp = n + sum(other.values())
        print(f"\n== PERF: {n:,} samples in kernels of {allsamp:,} ({pct(n, allsamp):.1f}%) ==")
        for c in "abcd":
            print(f"  {c}: {pct(sum(v for k, v in res.items() if k.startswith(c + '.')), n):.1f}%")
        for k, v in res.most_common():
            print(f"  {k:<30} {v:>10,} {pct(v, n):6.2f}%")
        print("-- samples on removable instructions --")
        for k, v in rfold.most_common():
            print(f"  {k:<30} {v:>10,} {pct(v, n):6.2f}%")
        print("-- by class --")
        for k, v in cls.most_common():
            print(f"  {k:<30} {v:>10,} {pct(v, n):6.2f}%")
        print("-- guard-only by family --")
        for k, v in fam.most_common(25):
            print(f"  {k:<50} {v:>10,} {pct(v, n):6.2f}%")
        print("-- by op/kind --")
        for k, v in op.most_common(25):
            print(f"  {k:<30} {v:>10,} {pct(v, n):6.2f}%")
        print("-- by kernel (samples, samples per slice-instruction) --")
        for k, v in kk.most_common(a.top):
            print(f"  {k:<40} {v:>10,} {pct(v, n):6.2f}%  per kinst-slice {1000.0 * v / max(1, calls.get(k, 0) * kern[k]['insts']):.3f}")
        print("-- outside the kernels (top dsos) --")
        for k, v in other.most_common(8):
            print(f"  {k:<50} {v:>10,} {pct(v, allsamp):6.2f}%")
        top = kk.most_common(1)
        if top:
            sym = top[0][0]
            print(f"-- hottest instruction windows of {sym} --")
            for i, v in hot[sym].most_common(10):
                print(f"  inst {i}: {v} samples")


if __name__ == "__main__":
    main()
