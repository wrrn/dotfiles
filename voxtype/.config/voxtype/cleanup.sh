#!/usr/bin/env bash
# Technical speech-to-text cleanup for software engineers
set -euo pipefail

INPUT=$(cat)
[[ -z "$INPUT" ]] && exit 0

# System prompt is baked into the "voxtype-cleaner" model via Modelfile.
# Using /api/generate instead of /api/chat to avoid message-formatting overhead.
JSON=$(jq -n --arg text "$INPUT" '{
  model: "voxtype-cleaner",
  prompt: ("<transcription>" + $text + "</transcription>"),
  stream: false,
  options: {
    num_predict: 200
  },
  keep_alive: "10m"
}')

# Capture raw response for debugging
RAW_RESPONSE=$(curl -sS http://localhost:11434/api/generate -d "$JSON" 2>&1)
echo "$(date +%H:%M:%S) RAW: $RAW_RESPONSE" >> /tmp/voxtype-cleanup.log

# /api/generate uses .response instead of .message.content
OUTPUT=$(echo "$RAW_RESPONSE" \
  | jq -r '.response // empty' \
  | sed 's/^"//;s/"$//' \
  | sed 's/ thinking.*<\/think>//g')

# Log every invocation for debugging
echo "$(date +%H:%M:%S) IN: $INPUT" >> /tmp/voxtype-cleanup.log
echo "$(date +%H:%M:%S) OUT: $OUTPUT" >> /tmp/voxtype-cleanup.log

if [[ -n "$OUTPUT" ]]; then
    echo "$OUTPUT"
else
    echo "$INPUT"
fi
