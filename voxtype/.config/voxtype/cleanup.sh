#!/usr/bin/env bash
# Technical speech-to-text cleanup for software engineers
set -euo pipefail

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
PROMPT_FILE="$SCRIPT_DIR/cleanup-prompt.md"

INPUT=$(cat)
[[ -z "$INPUT" ]] && exit 0

PROMPT=$(<"$PROMPT_FILE")

JSON=$(jq -n --arg prompt "$PROMPT" --arg text "$INPUT" '{
  model: "llama3.2:3b",
  prompt: ($prompt + $text),
  stream: false
}')


OUTPUT=$(curl -s http://localhost:11434/api/generate -d "$JSON" \
  | jq -r '.response // empty' \
  | sed 's/^"//;s/"$//' \
  | sed 's/<think>.*<\/think>//g')

if [[ -n "$OUTPUT" ]]; then
    echo "$OUTPUT"
else
    echo "$INPUT"
fi

