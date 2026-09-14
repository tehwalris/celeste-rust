#!/usr/bin/env python3
"""The ladder's progress and cost per level and horizon, from a search log.

Progress of a refinement is the lower bound it proves: a level's FIRST WIN
frame (a coarser level wins earlier) and, per horizon, its MARKED set (the
states that can still reach a win by H). Cost is rows through the kernels,
one currency for every level. This prints both per horizon so two ladders
(e.g. exact vs bucketed speed) compare on one table, and extrapolates the
remaining horizons' cost from the marks' growth, capped at each level's
unfiltered size.

    tools/ladder_model.py /tmp/room10h.log [/tmp/room10s.log ...]
"""
import re, sys

def parse(path):
    lines = open(path).read().split("\n")
    fwd_rows = fwd_ms = 0
    horizons = {}   # h -> {level: dict}
    for l in lines:
        m = re.match(r"\[fwd\] f\d+ in \d+/(\d+) raw (\d+) .* total (\d+) ms", l)
        if m:
            fwd_rows += int(m.group(2)); fwd_ms += int(m.group(3)); continue
        m = re.match(r"\[ladder\] h(\d+) level (\d+) \((\w+\(?\d*\)?)\): (.*)", l)
        if m:
            h, lv, prec, rest = int(m.group(1)), int(m.group(2)), m.group(3), m.group(4)
            win = re.search(r"first win f(\d+)", rest)
            marked = re.search(r"marked (\d+)", rest)
            horizons.setdefault(h, {})[lv] = dict(
                prec=prec, win=int(win.group(1)) if win else None,
                marked=int(marked.group(1)) if marked else 0,
                refuted="NO WIN" in rest, rows=fwd_rows, ms=fwd_ms)
            fwd_rows = fwd_ms = 0
    opt = re.search(r"OPTIMAL win frame: (\d+)", "\n".join(lines))
    return horizons, int(opt.group(1)) if opt else None

def report(path):
    horizons, opt = parse(path)
    print(f"== {path}: optimum {opt}, horizons {min(horizons)}..{max(horizons)}")
    print(f"{'h':>4} {'levels':>6} {'lvl0 win':>8} {'lvl0 marks':>11} {'lvl1 marks':>11} {'rows (M)':>9} {'seconds':>8}")
    total_s = 0.0
    for h in sorted(horizons):
        lv = horizons[h]
        rows = sum(v["rows"] for v in lv.values()); ms = sum(v["ms"] for v in lv.values())
        total_s += ms / 1000
        print(f"{h:>4} {len(lv):>6} {str(lv[0]['win']):>8} {lv[0]['marked']:>11} {lv.get(1, {}).get('marked', 0):>11} {rows/1e6:>9.1f} {ms/1000:>8.1f}")
    print(f"ladder forward seconds (finer levels' forwards, level 0 extension included): {total_s:.0f}")
    # Extrapolation from the last two horizons' growth, if the run is unfinished.
    hs = sorted(horizons)
    if opt is None and len(hs) >= 3:
        a, b = horizons[hs[-2]], horizons[hs[-1]]
        ra, rb = sum(v["ms"] for v in a.values()), sum(v["ms"] for v in b.values())
        g = rb / max(ra, 1)
        print(f"unfinished: last horizon {hs[-1]} cost {rb/1000:.0f}s, growth x{g:.2f}/horizon; "
              f"at that growth the next 5 horizons cost ~{sum(rb/1000 * g**i for i in range(1, 6)):.0f}s "
              f"(the growth saturates at a level's unfiltered forward)")

for p in sys.argv[1:]:
    report(p)
