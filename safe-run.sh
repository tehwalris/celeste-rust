#!/bin/bash
# Run commands with memory limit using cgroups (default 60G, lowered to
# MemAvailable less 10 GB when that is less)
# Usage: ./safe-run.sh [--memory LIMIT] -- COMMAND...

# The cap is checked against what the machine has NOW: an explicit --memory
# above MemAvailable less HEADROOM is REFUSED (two jobs whose caps together
# exceeded the machine once took it down); the default is lowered to fit,
# saying so. MemAvailable already counts /tmp (a tmpfs) as used.
MEMORY_LIMIT=""
DEFAULT_LIMIT="60G"
HEADROOM_GB=10

while [[ $# -gt 0 ]]; do
    case $1 in
        --memory|-m)
            MEMORY_LIMIT="$2"
            shift 2
            ;;
        --)
            shift
            break
            ;;
        *)
            break
            ;;
    esac
done

if [[ $# -eq 0 ]]; then
    echo "Usage: $0 [--memory LIMIT] -- COMMAND..."
    exit 1
fi

# A size (systemd's MemoryMax syntax: bytes, or K/M/G/T) in MiB.
to_mib() {
    local v=${1^^}
    case $v in
        *T) echo $(( ${v%T} * 1024 * 1024 )) ;;
        *G) echo $(( ${v%G} * 1024 )) ;;
        *M) echo $(( ${v%M} )) ;;
        *K) echo $(( ${v%K} / 1024 )) ;;
        *[!0-9]*|"") return 1 ;;
        *) echo $(( v / 1024 / 1024 )) ;;
    esac
}
avail_mib=$(awk '/^MemAvailable:/ {print int($2 / 1024)}' /proc/meminfo)
room_mib=$(( avail_mib - HEADROOM_GB * 1024 ))
if [[ -n $MEMORY_LIMIT ]]; then
    if ! want_mib=$(to_mib "$MEMORY_LIMIT"); then
        echo "[safe-run] REFUSED: --memory $MEMORY_LIMIT is not a size (e.g. 25G, 512M)" >&2
        exit 2
    fi
    if (( want_mib > room_mib )); then
        echo "[safe-run] REFUSED: --memory $MEMORY_LIMIT, but MemAvailable is $(( avail_mib / 1024 )) GB: at most $(( room_mib / 1024 )) GB fits with ${HEADROOM_GB} GB headroom. Another heavy job is running? Wait for it, or ask for less." >&2
        exit 2
    fi
else
    MEMORY_LIMIT=$DEFAULT_LIMIT
    if (( $(to_mib "$DEFAULT_LIMIT") > room_mib )); then
        if (( room_mib < 2048 )); then
            echo "[safe-run] REFUSED: MemAvailable is $(( avail_mib / 1024 )) GB, less than ${HEADROOM_GB} GB headroom + 2 GB: nothing fits" >&2
            exit 2
        fi
        MEMORY_LIMIT="${room_mib}M"
        echo "[safe-run] the default $DEFAULT_LIMIT does not fit (MemAvailable $(( avail_mib / 1024 )) GB, ${HEADROOM_GB} GB headroom): lowered" >&2
    fi
fi
echo "[safe-run] memory limit $MEMORY_LIMIT (MemAvailable $(( avail_mib / 1024 )) GB)" >&2

# mimalloc: freed pages go back to the OS after 100 ms (mimalloc v2's own
# default is 10). A wave unit takes ~12 ms and frees its buffers (the
# chunks it hands to the region jobs, its decoded rows) on whatever thread
# finishes them; at 0 every unit faulted them in again (9x the faults): at
# 100 ms the wave is 6-10% faster for a 10% higher anonymous peak, at 10 ms
# half of both. Read by mimalloc at process start, so it has to come from
# the environment, not from `main`.
export MIMALLOC_PURGE_DELAY="${MIMALLOC_PURGE_DELAY:-100}"
systemd-run --user --scope --quiet -p MemoryMax="$MEMORY_LIMIT" -p MemorySwapMax=0 -- "$@"
EXIT_CODE=$?

if [[ $EXIT_CODE -eq 137 ]]; then
    echo "[safe-run] Process killed (OOM)"
fi

exit $EXIT_CODE
