#!/usr/bin/env python3
"""A whole-run PROJECTION for one room from the runs already made
(plans/map-mod.md, "Projection"): per frame, the forward's cost is
`a * emissions` (wall, the median over the busy frames), its anonymous memory
`m0 + m1 * rows in + m2 * new states`, its disk `e * edges + s * states`;
the backward's `c * in-edges` a step. The constants are FITTED to `[fwd]`
and `[arc] fNNN` lines of finished runs (calibration), and a run at another
horizon is projected from its per-frame state counts, which come from

  N_H(f) = U(f) * A(H - f)

U the UNFILTERED forward (no level -1: the same for every target), A(s)
the share level -1 admits with s frames of slack, measured per target from
filtered runs at known horizons (A = N_H / U where U is known). Beyond the
last measured U the growth is extrapolated (`--growth`: the last 10 frames'
mean rate, decaying by `--decay` a frame); THAT is the projection's big
uncertainty, and it says so.

    tools/mod_project.py fit LOG...                  # the constants, per log and pooled
    tools/mod_project.py project --unfiltered LOG... --target LOG:H... --horizon H [--growth R --decay D]

Emissions/edges per state for the projected frames are the calibration
runs' (median over their busiest frames).
"""
import argparse
import re
import statistics
import sys

FWD = re.compile(r"^\[fwd\] f(\d+) in \d+/(\d+) raw (\d+) kept (\d+) .*?total (\d+) ms .*?edges (\d+) \(([\d.]+) B\).*?rss start ([\d.]+) wave ([\d.]+)")
ARC = re.compile(r"^\[arc\] f(\d+): .*?-> (\d+) in-edges .*?read (\d+) pull (\d+) merge (\d+) ms")


def parse(path):
    """{frame: dict} of the [fwd] lines and the [arc] steps of a log's FIRST level (a ladder's
    next level's forward starts after level 0's arc; a resume repeats no frame)."""
    fwd, arc = {}, {}
    done = False  # level 0's forward printed its `[search] level 0` line: its arc follows
    for line in open(path, errors="replace"):
        if line.startswith("[search] level 0 "):
            done = True
        if done and (line.startswith("[fwd] f") or line.startswith("[search] level 1 ")):
            break
        m = FWD.match(line)
        if m:
            f = int(m.group(1))
            fwd[f] = dict(rows_in=int(m.group(2)), raw=int(m.group(3)), kept=int(m.group(4)), ms=int(m.group(5)),
                          edges=int(m.group(6)), b_edge=float(m.group(7)), rss=float(m.group(9)))
            continue
        m = ARC.match(line)
        if m:
            arc[int(m.group(1))] = dict(inedges=int(m.group(2)), ms=int(m.group(3)) + int(m.group(4)) + int(m.group(5)))
    return fwd, arc


def lstsq(xs, ys):
    """Least squares through the origin for y = sum(c_i x_i) (2 or 3 columns, normal equations)."""
    k = len(xs[0])
    a = [[sum(x[i] * x[j] for x in xs) for j in range(k)] for i in range(k)]
    b = [sum(x[i] * y for x, y in zip(xs, ys)) for i in range(k)]
    # Gaussian elimination.
    for i in range(k):
        p = max(range(i, k), key=lambda r: abs(a[r][i]))
        a[i], a[p], b[i], b[p] = a[p], a[i], b[p], b[i]
        for r in range(i + 1, k):
            q = a[r][i] / a[i][i]
            for c in range(i, k):
                a[r][c] -= q * a[i][c]
            b[r] -= q * b[i]
    x = [0.0] * k
    for i in reversed(range(k)):
        x[i] = (b[i] - sum(a[i][c] * x[c] for c in range(i + 1, k))) / a[i][i]
    return x


def fit(logs):
    rows, mem, arcs, bed = [], [], [], []
    for p in logs:
        fwd, arc = parse(p)
        big = [v for v in fwd.values() if v["raw"] > 50_000_000]
        rows += [((v["raw"], v["kept"]), v["ms"] / 1000) for v in big]
        mem += [((1.0, v["rows_in"], v["kept"]), v["rss"]) for v in big]
        bed += [v["b_edge"] for v in big]
        arcs += [((a["inedges"],), a["ms"] / 1000) for a in arc.values() if a["inedges"] > 10_000_000]
    # One constant (wall a emission, the states' share folded in): two
    # collinear columns fit to noise on a shared machine.
    t = [statistics.median(y / x[0] for x, y in rows), 0.0] if rows else [0, 0]
    m = lstsq([r[0] for r in mem], [r[1] for r in mem]) if mem else [0, 0, 0]
    c = lstsq([r[0] for r in arcs], [r[1] for r in arcs])[0] if arcs else 0
    err = [abs(t[0] * x[0] + t[1] * x[1] - y) / y for x, y in rows] if rows else [0]
    return dict(ns_emission=t[0] * 1e9, ns_state=t[1] * 1e9, fwd_err=statistics.median(err), n=len(rows),
                mem0=m[0], mem_in=m[1] * 1e9, mem_new=m[2] * 1e9, b_edge=statistics.median(bed) if bed else 0,
                ns_inedge=c * 1e9, n_arc=len(arcs))


def show_fit(name, k):
    print(f"{name}: forward {k['ns_emission']:.1f} ns an emission + {k['ns_state']:.0f} ns a new state "
          f"(median error {100 * k['fwd_err']:.0f}% over {k['n']} frames); wave rss {k['mem0']:.1f} GB + "
          f"{k['mem_in']:.0f} B a row in + {k['mem_new']:.0f} B a new state; edges {k['b_edge']:.2f} B; "
          f"backward {k['ns_inedge']:.1f} ns an in-edge ({k['n_arc']} steps)")


def project(a):
    unf = {}
    for p in a.unfiltered:
        unf.update({f: v for f, v in parse(p)[0].items()})
    k = fit(a.calibrate or a.unfiltered)
    show_fit("calibration", k)
    if a.ns_emission:
        k["ns_emission"] = a.ns_emission
    last = max(unf)
    rates = [unf[f]["kept"] / unf[f - 1]["kept"] for f in range(last - 9, last + 1)]
    r0 = a.growth or statistics.mean(rates)
    # Per-state ratios of the busiest measured frames.
    busy = sorted(unf.values(), key=lambda v: -v["kept"])[:10]
    emis = statistics.median(v["raw"] / v["kept"] for v in busy)
    edges = statistics.median(v["edges"] / v["kept"] for v in busy)
    print(f"unfiltered U(f) measured to f{last} ({unf[last]['kept'] / 1e6:.1f}M new states); beyond: x{r0:.3f} a frame, "
          f"the excess decaying by {a.decay} a frame; {emis:.1f} emissions and {edges:.1f} edges a state")
    U = {f: v["kept"] for f, v in unf.items()}
    r = r0
    for f in range(last + 1, a.horizon + 1):
        U[f] = U[f - 1] * r
        r = 1 + (r - 1) * (1 - a.decay)
    # A(s) from the target's filtered runs.
    A = {}
    for spec in a.target:
        p, h = spec.rsplit(":", 1)
        h = int(h)
        for f, v in parse(p)[0].items():
            if f in unf and unf[f]["kept"] > 1000:
                A.setdefault(h - f, []).append(v["kept"] / unf[f]["kept"])
    A = {s: min(1.0, statistics.median(v)) for s, v in A.items()}
    ss = sorted(A)
    smax = max(ss)

    def adm(s):
        if s >= smax:
            return 1.0
        if s in A:
            return A[s]
        lo = max([x for x in ss if x < s], default=None)
        hi = min([x for x in ss if x > s], default=None)
        if lo is None:
            return A[hi]
        return A[lo] + (A[hi] - A[lo]) * (s - lo) / (hi - lo)
    print(f"A(s) measured for slack {ss[0]}..{smax} (s >= {smax}: 1); A(0) {adm(0):.3f} A(10) {adm(10):.3f} A(20) {adm(20):.3f} A(40) {adm(40):.3f}")
    tot_t = tot_states = tot_edges = 0.0
    peak = (0, 0)
    for f in range(1, a.horizon + 1):
        n = U.get(f, 0) * adm(a.horizon - f)
        tot_states += n
        tot_edges += n * edges
        tot_t += (n * emis * k["ns_emission"] + n * k["ns_state"]) / 1e9
        if n > peak[1]:
            peak = (f, n)
    disk = tot_edges * k["b_edge"] / 1e9 + tot_states * a.b_state / 1e9
    mem = k["mem0"] + (k["mem_in"] + k["mem_new"]) * peak[1] / 1e9
    back = tot_edges * a.marked_share * k["ns_inedge"] / 1e9
    print(f"PROJECTION h{a.horizon}: {tot_states / 1e9:.2f}B states, peak {peak[1] / 1e6:.0f}M at f{peak[0]}; forward "
          f"{tot_t / 3600:.1f} h; disk {disk:.0f} GB ({k['b_edge']:.2f} B an edge"
          f"{'' if a.b_edge_alt is None else f'; at {a.b_edge_alt} B an edge: {tot_edges * a.b_edge_alt / 1e9 + tot_states * a.b_state / 1e9:.0f} GB'}); "
          f"wave memory at the peak ~{mem:.0f} GB; backward ~{back / 3600:.1f} h (in-edges = {a.marked_share} of the edges)")


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    f = sub.add_parser("fit")
    f.add_argument("logs", nargs="+")
    p = sub.add_parser("project")
    p.add_argument("--unfiltered", nargs="+", required=True, help="logs of the forward WITHOUT level -1 (any target)")
    p.add_argument("--target", nargs="+", required=True, help="LOG:H of filtered runs to this target")
    p.add_argument("--calibrate", nargs="*", help="logs to fit the constants on (default: the unfiltered)")
    p.add_argument("--horizon", type=int, required=True)
    p.add_argument("--growth", type=float, help="U's rate a frame past the last measured frame (default: the last 10 frames')")
    p.add_argument("--decay", type=float, default=0.0, help="the rate's excess shrinks by this share a frame")
    p.add_argument("--ns-emission", type=float, help="override the fitted wall ns an emission (a faster forward)")
    p.add_argument("--b-state", type=float, default=10.0, help="disk bytes a state (format 14 frame files ~9-10)")
    p.add_argument("--b-edge-alt", type=float, help="another storage's bytes an edge (the target-region bundles: 0.08-0.25)")
    p.add_argument("--marked-share", type=float, default=0.15, help="the share of edges the backward reads (h140: 0.16)")
    a = ap.parse_args()
    if a.cmd == "fit":
        for p in a.logs:
            show_fit(p, fit([p]))
        if len(a.logs) > 1:
            show_fit("pooled", fit(a.logs))
    else:
        project(a)


if __name__ == "__main__":
    main()
