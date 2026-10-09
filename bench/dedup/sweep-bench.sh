#!/bin/bash
# DESIGNS.md "Sweep front": vprod (16 workers) against the sweep front at 16
# and 32 threads, shared front and one front per CCD; interleaved reps.
# Per run: wall (wave + end), the per-phase breakdown, AnonHugePages, and
# perf counters over the timed part only (the binaries gate them through
# PERF_CTL fifos):
#  - per process: cycles, instructions, L2 misses, fills from the local CCX
#    (L3), from the OTHER CCD's cache (near_cache: another CCX, same node) and from DRAM (x 64 B);
#  - system-wide (sudo, amd_uncore): UMC CAS reads/writes x 64 B = DRAM bytes
#    (includes everything else the machine does in that window: run quiet).
# Usage: REPS=3 bench.sh            (run when the machine is QUIET)
set -u
B=/var/tmp/dd-sweep/bench/dedup/target/release
D=/var/tmp/emitcap
S=$D/sweep
SAFE=/var/tmp/dd-sweep/safe-run.sh
REPS=${REPS:-3}
LOG=$S/bench-$(date +%Y%m%d-%H%M%S).log
TSV=${LOG%.log}.tsv
EV=cycles,instructions,l2_cache_req_stat.ic_dc_miss_in_l2,ls_any_fills_from_sys.local_ccx,ls_any_fills_from_sys.near_cache,ls_any_fills_from_sys.dram_io_all
UMC=amd_umc_0/umc_cas_cmd.rd/,amd_umc_0/umc_cas_cmd.wr/,amd_umc_1/umc_cas_cmd.rd/,amd_umc_1/umc_cas_cmd.wr/
F1=$S/perfctl1
F2=$S/perfctl2

lsmod | grep -q '^amd_uncore' || sudo -n modprobe amd_uncore || echo "no amd_uncore: UMC counts will be missing"
(cd /var/tmp/dd-sweep/bench/dedup && $SAFE -- cargo build --release 2>&1 | tail -1)
[ -f $S/d2.bin ] || $SAFE --memory 40G -- $B/sweep $D prep
# The default probe (pm8) reads its keys from a posmask8 dump (~2 min once).
[ -f $S/posmask8q.bin ] || PM_DUMP=$S $SAFE --memory 50G -- $B/dedup-bench $D posmask8

declare -a NAMES CMDS
add() { NAMES+=("$1"); CMDS+=("$2"); }
add vprod16      "$B/vprod $D vprod $S/vprod-edges"
add sweep16-shared "$B/sweep $D run threads=16 front=shared"
add sweep16-ccd    "$B/sweep $D run threads=16 front=ccd"
add sweep32-shared "$B/sweep $D run threads=32 front=shared"
add sweep32-ccd    "$B/sweep $D run threads=32 front=ccd"
add sweep16-shared-v4 "$B/sweep $D run threads=16 front=shared probe=v4"

run_one() { # name cmd rep
  local name=$1 cmd=$2 rep=$3 tmp=$S/.perf.$$
  echo "=== rep $rep $name; load $(cut -d' ' -f1-3 /proc/loadavg)" | tee -a $LOG
  rm -f $F1 $F2 $tmp.done; mkfifo $F1 $F2
  # (sudo'd: the user cannot signal it, so it exits when $tmp.done appears)
  sudo -n perf stat -a -x, -o $tmp.umc -D -1 --control fifo:$F2 -e $UMC -- sh -c "while [ ! -e $tmp.done ]; do sleep 0.1; done" 2>/dev/null &
  local sp=$!
  sleep 0.5
  PERF_CTL=$F1,$F2 $SAFE --memory 40G -- perf stat -x, -o $tmp.core -D -1 --control fifo:$F1 -e $EV $cmd 2>&1 \
    | grep -E '^\[(sweep|vprod|phases|stats|mem)\]' | tee -a $LOG > $tmp.out
  touch $tmp.done; wait $sp
  # Times: vprod "wave X s ... end_frame Y s"; sweep "wave X s ..., end Y s".
  local wave endt
  wave=$(grep -oP '\] .*?wave \K[0-9.]+' $tmp.out | head -1)
  endt=$(grep -oP '(end_frame|end) \K[0-9.]+(?= s)' $tmp.out | head -1)
  local c i l2 l3 far dram rd wr
  c=$(awk -F, '$3=="cycles"{print $1}' $tmp.core)
  i=$(awk -F, '$3=="instructions"{print $1}' $tmp.core)
  l2=$(awk -F, '$3 ~ /ic_dc_miss_in_l2/{print $1}' $tmp.core)
  l3=$(awk -F, '$3 ~ /local_ccx/{print $1}' $tmp.core)
  far=$(awk -F, '$3 ~ /near_cache/{print $1}' $tmp.core)
  dram=$(awk -F, '$3 ~ /dram_io_all/{print $1}' $tmp.core)
  rd=$(awk -F, '$3 ~ /cas_cmd.rd/{s+=$1} END{print s+0}' $tmp.umc 2>/dev/null)
  wr=$(awk -F, '$3 ~ /cas_cmd.wr/{s+=$1} END{print s+0}' $tmp.umc 2>/dev/null)
  local line
  line=$(awk -v n=$name -v r=$rep -v w=$wave -v e=$endt -v c=$c -v i=$i -v l2=$l2 -v l3=$l3 -v f=$far -v d=$dram -v rd=$rd -v wr=$wr 'BEGIN{
    t=w+e; printf "%s\t%d\t%.3f\t%.3f\t%.3f\t%.1f\t%.2f\t%.0f\t%.0f\t%.0f\t%.0f\t%.2f\t%.2f\t%.1f\n", n, r, w, e, t, c/1e9, i/c, l2/1e6, l3/1e6, f/1e6, d/1e6, rd*64/1e9, wr*64/1e9, (rd+wr)*64/1e9/t }')
  echo "[perf] $line" | tee -a $LOG
  echo "$line" >> $TSV
  rm -f $tmp.*
}

echo -e "name\trep\twave_s\tend_s\ttotal_s\tGcycles\tIPC\tL2miss_M\tL3fill_M\tfarCCD_M\tDRAMfill_M\tUMC_rd_GB\tUMC_wr_GB\tUMC_GB/s" > $TSV
{ echo "uptime: $(uptime)"; git -C /var/tmp/dd-sweep log -1 --oneline; } | tee -a $LOG
# The harness floors (once): reading the streams alone.
for t in 16 32; do
  echo "=== floor read$t" | tee -a $LOG
  $SAFE --memory 40G -- $B/sweep $D run threads=$t front=shared read=1 2>&1 | grep -E '^\[sweep\]' | tee -a $LOG
done
echo "=== floor vread (vprod's capture read + parse, 16 threads)" | tee -a $LOG
$SAFE -- $B/vprod $D vread 2>&1 | grep -E '^\[vread' | tee -a $LOG

for rep in $(seq 1 $REPS); do
  for k in "${!NAMES[@]}"; do run_one "${NAMES[$k]}" "${CMDS[$k]}" $rep; done
done

echo | tee -a $LOG
echo "SUMMARY (best / median of $REPS; UMC = system-wide DRAM bytes in the timed window)" | tee -a $LOG
awk -F'\t' 'NR>1{n[$1]++; t[$1,n[$1]]=$5; w[$1,n[$1]]=$3; e[$1,n[$1]]=$4; d[$1]+=$11; u[$1]+=$12+$13; f[$1]+=$10; ipc[$1]+=$7; if(!($1 in o)){o[$1]=++no; on[no]=$1}}
  END{printf "%-16s %8s %8s %8s %8s %10s %10s %9s %6s\n","config","best_s","median_s","wave_s","end_s","DRAMfill_M","farCCD_M","UMC_GB","IPC";
  for(j=1;j<=no;j++){k=on[j]; m=n[k]; for(a=1;a<=m;a++)v[a]=t[k,a]; asort(v); b=v[1]; med=v[int((m+1)/2)];
    printf "%-16s %8.3f %8.3f %8.3f %8.3f %10.0f %10.0f %9.2f %6.2f\n",k,b,med,w[k,1],e[k,1],d[k]/m,f[k]/m,u[k]/m,ipc[k]/m; delete v}}' $TSV | tee -a $LOG
echo "log $LOG, table $TSV"
