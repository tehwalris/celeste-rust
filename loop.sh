#!/bin/bash

while true; do
	TS=$(date +"%y-%m-%d-%H-%M-%S")
	COMMIT=$(git rev-parse --short=6 HEAD)
	LOGFILE="/home/philippe/agent_logs/agent_${TS}_COMMIT.log"

	claude --dangerously-skip-permissions -p "$(cat AGENT_PROMPT.md)" --model claude-opus-4-5 &> "$LOGFILE"
done
