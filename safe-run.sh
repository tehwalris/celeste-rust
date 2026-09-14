#!/bin/bash
# Run commands with memory limit using cgroups (default 60G)
# Usage: ./safe-run.sh [--memory LIMIT] -- COMMAND...

MEMORY_LIMIT="60G"

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

# mimalloc: return freed pages to the OS immediately. The door rebuilds
# every shard's sorted base each frame, and with the default 10 ms delayed
# purge the freed copies stayed resident: 2 GB less anonymous RSS at room
# (2,0) f55 for 5-8% more time (2026-09-14). Read by mimalloc at process
# start, so it has to come from the environment, not from `main`.
export MIMALLOC_PURGE_DELAY="${MIMALLOC_PURGE_DELAY:-0}"
systemd-run --user --scope --quiet -p MemoryMax="$MEMORY_LIMIT" -p MemorySwapMax=0 -- "$@"
EXIT_CODE=$?

if [[ $EXIT_CODE -eq 137 ]]; then
    echo "[safe-run] Process killed (OOM)"
fi

exit $EXIT_CODE
