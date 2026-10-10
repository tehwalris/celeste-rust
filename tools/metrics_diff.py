#!/usr/bin/env python3
"""Compare an EXACT metrics file (`rewrite bench-frame --metrics`) with its
pinned copy (gates/fixtures/NAME.metrics): every key whose value moved, with
the percent change, the per-kernel `k.<sym>.*` lines summarized (how many
kernels appeared, disappeared or changed; the largest changes listed).
Exits 1 on any difference: a change in these numbers is always noticed and
re-pinned deliberately (`./check.sh t1 --pin`, and say why in the commit).
The `approx.*` keys (the edge file's bytes: the scheduling moves them by a
few bytes) differ only beyond APPROX (0.5%).

    tools/metrics_diff.py PINNED NEW
"""
import sys


def load(path):
    head, vals = [], {}
    with open(path) as f:
        for line in f:
            line = line.rstrip("\n")
            if not line or line.startswith("#"):
                head.append(line)
                continue
            k, v = line.rsplit(" ", 1)
            vals[k] = int(v)
    return head, vals


APPROX = 0.005


def differs(k, va, vb):
    if va == vb:
        return False
    if k.startswith("approx.") and va is not None and vb is not None:
        return abs(vb - va) > APPROX * max(abs(va), 1)
    return True


def pct(a, b):
    if a == 0:
        return "new" if b else "0%"
    return f"{100.0 * (b - a) / a:+.2f}%"


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    (ha, a), (hb, b) = load(sys.argv[1]), load(sys.argv[2])
    bad = False
    if ha != hb:
        bad = True
        print(f"  header: {ha} -> {hb}")
    top = sorted(k for k in set(a) | set(b) if not k.startswith("k."))
    rows = []
    for k in top:
        va, vb = a.get(k), b.get(k)
        if differs(k, va, vb):
            rows.append((k, va, vb))
    for k, va, vb in rows:
        sa = "-" if va is None else str(va)
        sb = "-" if vb is None else str(vb)
        p = pct(va or 0, vb or 0) if va is not None and vb is not None else ("added" if va is None else "removed")
        print(f"  {k:<40} {sa:>14} -> {sb:>14}  {p}")
    bad |= bool(rows)
    # Per kernel: symbols that came or went, and the biggest moves.
    syms = lambda d: {k.split(".")[1] for k in d if k.startswith("k.")}
    sa, sb = syms(a), syms(b)
    gone, came = sa - sb, sb - sa
    moved = []
    for s in sorted(sa & sb):
        for k in sorted(x for x in set(a) | set(b) if x.startswith(f"k.{s}.")):
            if a.get(k) != b.get(k):
                moved.append((abs((b.get(k) or 0) - (a.get(k) or 0)), k, a.get(k), b.get(k)))
    if gone or came or moved:
        bad = True
        print(f"  kernels that ran: {len(gone)} gone, {len(came)} new, {len({m[1].split('.')[1] for m in moved})} changed")
        for _, k, va, vb in sorted(moved, reverse=True)[:12]:
            print(f"    {k:<54} {va} -> {vb}  {pct(va or 0, vb or 0)}")
        for s in sorted(gone)[:5]:
            print(f"    gone: {s}")
        for s in sorted(came)[:5]:
            print(f"    new:  {s}")
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()
