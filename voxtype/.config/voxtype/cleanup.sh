#!/usr/bin/env bash
# Technical speech-to-text cleanup for software engineers
set -euo pipefail

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
PROMPT_FILE="$SCRIPT_DIR/cleanup-prompt.md"

INPUT=$(cat)
[[ -z "$INPUT" ]] && exit 0

PROMPT=$(<"$PROMPT_FILE")

JSON=$(jq -n --arg prompt "$PROMPT" --arg text "$INPUT" '{
  model: "gemma3:1b",
  messages: [
    { role: "system", content: $prompt },
    { role: "user", content: $text }
  ],
  stream: false,
  options: {
    temperature: 0.0,
    num_predict: 256
  }
}')

OUTPUT=$(curl -s http://localhost:11434/api/chat -d "$JSON" \
  | jq -r '.message.content // empty' \
  | sed 's/^"//;s/"$//' \
  | sed 's/<think>.*<\/think>//g')

if [[ -n "$OUTPUT" ]]; then
    echo "$OUTPUT"
else
    echo "$INPUT"
fi
