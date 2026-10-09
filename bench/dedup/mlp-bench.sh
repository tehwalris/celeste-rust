#!/bin/bash
# The MLP comparison (bench/dedup DESIGNS.md "MLP"): the baselines and every
# table x latency-hiding mode, interleaved REPS times, pinned to one cpu, with
# perf counters over the TIMED loop only (perf stat -D -1 --control fifo; the
# binary switches them around the loop, PERF_CTL). Ends with a summary table.
#
# Usage: bench.sh [CPU]            (default cpu 4: CCD0, the 96 MB L3)
#        bench.sh --summary LOG    (the table again from a finished log)
# Env:   REPS=3  BASE="v3c v4 bitcell bitintern posmask4 posmask8"
#        TABLES="v4 v4b bitcell bitcellb posmask4 posmask4b posmask8 posmask8b bitintern"
#        MODES="seq g8 g16 g32 p8 p16 p32 a8 a16 a32"
#        (the default matrix: 96 variants x 3 reps, ~1 h; the bitcell/posmask
#        precompute is cached under mlp/cache, keyed on its inputs' mtimes)
#        BIN=...  (default: the dedup-mlp worktree's release binary)
# Run it on a QUIET machine; each run logs `uptime` and the timed table's
# huge-page coverage (THP is enabled=always, defrag=madvise here; the tables
# are madvise(MADV_HUGEPAGE)d - check the "table on huge pages" column).
set -u
D=/var/tmp/emitcap
O=$D/mlp
SAFE=/home/philippe/src/github.com/tehwalris/celeste-rust/safe-run.sh
B=${BIN:-/var/tmp/dd-mlp/bench/dedup/target/release/dedup-bench}
EV=cycles,instructions,branch-misses,L1-dcache-load-misses,dTLB-load-misses,l2_cache_req_stat.ic_dc_miss_in_l2,ls_any_fills_from_sys.local_ccx,ls_any_fills_from_sys.dram_io_all

summary() {
python3 - "$1" <<'EOF'
import re, sys, statistics as st
N = 257_724_013  # lookups (the drops loop, 254M trivial updates, is inside the timed window too)
runs = {}; order = []
cur = None
for l in open(sys.argv[1]):
    m = re.match(r'=== rep (\d+) (\S+) ', l)
    if m:
        cur = {'name': m.group(2)}
        if cur['name'] not in runs: runs[cur['name']] = []; order.append(cur['name'])
        runs[cur['name']].append(cur); continue
    if cur is None: continue
    m = re.search(r'TIMED single thread ([\d.]+) s \(drops ([\d.]+) s\): new (\d+)', l)
    if m:
        cur['t'] = float(m.group(1)); cur['drops'] = float(m.group(2)); cur['new'] = int(m.group(3))
        h = re.search(r'table on huge pages ([\d.]+) of ([\d.]+) GB', l)
        if h: cur['huge'] = f"{h.group(1)}/{h.group(2)}"
        continue
    p = l.strip().split(',')
    if len(p) > 3 and p[0].replace('.', '').isdigit():
        cur[p[2]] = float(p[0])
def med(rs, k):
    v = [r[k] for r in rs if k in r]
    return st.median(v) if v else float('nan')
cols = [('cycles', 'cyc'), ('instructions', 'insn'), ('branch-misses', 'br-miss'), ('dTLB-load-misses', 'dTLB'),
        ('l2_cache_req_stat.ic_dc_miss_in_l2', 'L2miss'), ('ls_any_fills_from_sys.local_ccx', 'L3fill'), ('ls_any_fills_from_sys.dram_io_all', 'DRAM')]
print(f"{'variant':<22} {'n':>2} {'time s min/med/max':>20} {'ns/lk':>6} {'new ok':>6} " + ' '.join(f"{c[1]+'/lk':>10}" for c in cols) + f" {'IPC':>5} {'table huge GB':>14}")
base = None
for name in order:
    rs = [r for r in runs[name] if 't' in r]
    if not rs: print(f"{name:<22} no TIMED line"); continue
    ts = [r['t'] for r in rs]
    ns = (st.median(ts) - med(rs, 'drops')) * 1e9 / N
    ok = all(r.get('new') == 6735699 for r in rs)
    per = ' '.join(f"{med(rs, c[0]) / N:>10.3f}" for c in cols)
    ipc = med(rs, 'instructions') / med(rs, 'cycles')
    print(f"{name:<22} {len(rs):>2} {min(ts):>6.2f}/{st.median(ts):>5.2f}/{max(ts):>5.2f} {ns:>6.2f} {str(ok):>6} {per} {ipc:>5.2f} {rs[-1].get('huge', '-'):>14}")
EOF
}

if [ "${1:-}" = "--summary" ]; then summary "$2"; exit 0; fi
CPU=${1:-4}
REPS=${REPS:-3}
BASE=${BASE:-"v3c v4 bitcell bitintern posmask4 posmask8"}
TABLES=${TABLES:-"v4 v4b bitcell bitcellb posmask4 posmask4b posmask8 posmask8b bitintern"}
MODES=${MODES:-"seq g8 g16 g32 p8 p16 p32 a8 a16 a32"}
mkdir -p $O
LOG=$O/bench-$(date +%Y%m%d-%H%M%S).log
F=$O/perfctl.$$
rm -f $F; mkfifo $F
trap 'rm -f $F' EXIT
[ -x "$B" ] || { echo "no binary $B (cd bench/dedup && safe-run.sh -- cargo build --release)"; exit 1; }
V=()
for b in $BASE; do V+=("$b"); done
for t in $TABLES; do for m in $MODES; do V+=("mlp:$t:$m"); done; done
echo "[bench] ${#V[@]} variants x $REPS reps on cpu $CPU; log $LOG"
echo "# $(date) cpu $CPU binary $B ($(stat -c %y $B)); $(uptime)" > $LOG
# Warm the page cache with the query streams (8.2 GB each) once.
cat $D/prep/q.bin $D/prep/qb.bin $D/prep/d.bin > /dev/null
for rep in $(seq 1 $REPS); do
  for v in "${V[@]}"; do
    args=(${v//:/ })
    echo "=== rep $rep $v cpu $CPU uptime $(uptime | sed 's/.*load average: //')" | tee -a $LOG
    BENCH_NOCHECK=1 PERF_CTL=$F $SAFE -- perf stat -D -1 --control fifo:$F -x, -e $EV \
      taskset -c $CPU $B $D "${args[@]}" 2>&1 | grep -E "TIMED|setup|table |," >> $LOG
  done
done
echo "# done $(date); $(uptime)" >> $LOG
summary $LOG | tee $LOG.summary
