#!/bin/bash
# smt-run.sh FILE.smt2 TIMEOUT_MS -> FILE.out
f=$1; t=${2:-10000}
( echo "(set-option :timeout $t)"; echo "(set-option :produce-models true)"; cat "$f" ) | systemd-run --user --scope -p MemoryMax=3G --quiet /var/tmp/z3venv/bin/z3 -in > "${f%.smt2}.out" 2>&1
