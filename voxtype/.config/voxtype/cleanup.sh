#!/bin/bash
# Technical speech-to-text cleanup for software engineers
set -euo pipefail

INPUT=$(cat)
[[ -z "$INPUT" ]] && exit 0

JSON=$(jq -n --arg text "$INPUT" '{
  model: "llama3.2:3b",
  prompt: ("You are a speech-to-text post-processor for a software engineer. Fix transcription errors and output ONLY the corrected text. No quotes, no emojis, no explanations.\n\nRules:\n- Fix technical terms: recognize CLI commands, programming languages, tools, libraries, and file paths\n- Common corrections: \"nix\" not \"nicks\", \"NixOS\" not \"nick OS\", \"git\" not \"get\", \"npm\" not \"NPM\", \"kubectl\" not \"cube cuddle\", \"sudo\" not \"pseudo\", \"tmux\" not \"tea mux\", \"grep\" not \"grip\", \"daemon\" not \"demon\", \"regex\" not \"redjects\", \"repo\" not \"reppo\", \"src\" not \"source\" (when meaning the directory)\n- Acronyms: CLI, API, SSH, YAML, JSON, HTTP, HTTPS, REST, gRPC, SQL, HTML, CSS, LLM, GPU, CPU, RAM, SSD, NVMe, PR, CI, CD\n- Languages/tools: Rust, TypeScript, JavaScript, Python, Go, Nix, Docker, Kubernetes, Terraform, Ansible, Neovim, Vim, Emacs\n- Preserve paths: \"slash home slash user\" -> \"/home/user\", \"tilde slash dot config\" -> \"~/.config\"\n- Preserve commands: \"git commit dash m\" -> \"git commit -m\", \"cargo build dash dash release\" -> \"cargo build --release\"\n- \"dash\"/\"hyphen\" in commands -> \"-\", \"dash dash\" -> \"--\"\n- \"dot\" in file contexts -> \".\", \"slash\" in paths -> \"/\", \"tilde\" at path start -> \"~\"\n- Remove filler words (um, uh, like, you know, basically)\n- Fix punctuation and capitalization\n- Do NOT rephrase or add information\n\nTranscription:\n" + $text),
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
