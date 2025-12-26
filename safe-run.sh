#!/bin/bash
# Run commands with memory limit using cgroups (default 100G)
# Usage: ./safe-run.sh [--memory LIMIT] -- COMMAND...

MEMORY_LIMIT="100G"

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

systemd-run --user --scope --quiet -p MemoryMax="$MEMORY_LIMIT" -p MemorySwapMax=0 -- "$@"
EXIT_CODE=$?

if [[ $EXIT_CODE -eq 137 ]]; then
    echo "[safe-run] Process killed (OOM)"
fi

exit $EXIT_CODE
