# Voxtype Post-Processing Pipeline

Notes on the LLM cleanup pipeline behind voxtype dictation, why it's set up
the way it is, and how to maintain or evolve it.

---

## TL;DR

- Voxtype transcribes speech (Parakeet → fast, local, streaming).
- After transcription, it pipes the raw text through `cleanup.sh` for
  LLM-powered cleanup (filler removal, punctuation, code-syntax preservation).
- The LLM is a **custom Ollama model** called `voxtype-cleaner`, built from a
  `Modelfile` that bakes in the system prompt and inference parameters.
- Ollama runs on the **Intel Arc GPU via Vulkan**, dropping cleanup latency
  from ~3–7 seconds (CPU) to ~0.4–1.5 seconds (GPU).

---

## Files in this directory

| File | Purpose |
|------|---------|
| `config.toml` | Voxtype daemon config. Points `post_process.command` at `cleanup.sh`. |
| `cleanup.sh` | Shell wrapper. Receives raw transcription on stdin, calls Ollama, prints cleaned output. |
| `cleanup-prompt.md` | **No longer used at runtime.** Historical / reference prompt. Live prompt is baked into the Modelfile. |
| `Modelfile` | Defines the `voxtype-cleaner` Ollama model: base weights + system prompt + parameters. |
| `README.md` | This file. |

---

## How the pipeline works

```
[Hotkey pressed]
       │
       ▼
┌──────────────────┐
│ Parakeet (local) │  ASR — streaming transcription
└──────────────────┘
       │  raw text on stdin
       ▼
┌──────────────────┐
│ cleanup.sh       │  Wraps text in <transcription> tags,
│                  │  sends JSON to Ollama /api/generate
└──────────────────┘
       │  HTTP POST to localhost:11434
       ▼
┌──────────────────┐
│ Ollama daemon    │  Loads voxtype-cleaner model
│ (systemd unit)   │  Runs inference on Intel Arc GPU via Vulkan
└──────────────────┘
       │  cleaned text on stdout
       ▼
[Typed / pasted via wtype/wl-copy]
```

---

## The Modelfile

`voxtype-cleaner` is a custom Ollama model created from `Modelfile`:

```dockerfile
FROM qwen2.5-coder:1.5b              # Base weights (downloaded via ollama pull)
SYSTEM """ ...prompt text... """     # Baked-in system instructions
PARAMETER temperature 0.0            # Deterministic output
PARAMETER num_predict 200            # Hard cap on response length
```

### What `ollama create` actually does

It does **NOT** train or fine-tune anything. It just:
1. Copies the base model weights (already trained).
2. Attaches the system prompt as a metadata layer.
3. Packages it under a new name (`voxtype-cleaner:latest`).

Think Docker image, not gradient descent.

### Rebuilding after edits

After modifying `Modelfile`:

```bash
ollama create voxtype-cleaner -f ~/.config/voxtype/Modelfile
```

The next call to `cleanup.sh` will pick up the new version. Existing models
already in `/var/lib/ollama/models` stay around — you're not breaking anything.

---

## Performance journey (for context)

| Setup | Warm latency | Notes |
|-------|-------------|-------|
| `kimi-k2.6:cloud` (1T params, cloud) | ~10–30s | Cold starts + network. Way too slow. |
| `qwen2.5-coder:7b` on CPU | ~7s | Reliable but glacial. |
| `qwen2.5-coder:1.5b` on CPU | ~3.7s | Faster, less reliable. |
| `qwen2.5-coder:1.5b` on **Arc GPU (Vulkan)** | **~0.4–1.5s** | ✅ Current setup |
| `qwen2.5-coder:7b` on **Arc GPU (Vulkan)** | ~1.6–2.1s | Documented upgrade path |

### Lessons learned along the way

- **Prompt size matters a lot on small models.** A 600-token prompt with
  many examples will overwhelm a 1.5B model. Tight prompts work better.
- **Ollama does NOT cache system prompts across HTTP calls.** Every
  `/api/chat` or `/api/generate` request is stateless. The Modelfile lets you
  bake in a prompt, but Ollama still tokenizes & evaluates it on every request.
- **`/api/generate` is barely faster than `/api/chat`** for our use case
  (~100ms saved, not worth the API differences).
- **The real latency killer was CPU inference**, not the architecture of
  prompts or messages. Moving to GPU was the only fix that actually mattered.

---

## Intel Arc GPU acceleration (the big win)

### What enables it

| Component | Where it lives |
|-----------|----------------|
| Ollama with Vulkan backend | `pkgs.ollama-vulkan` (overlay in nixos-config) |
| `OLLAMA_VULKAN=1` env var | `services.ollama.environmentVariables` |
| Intel Vulkan ICD driver | `/run/opengl-driver/lib/libvulkan_intel.so` (from Mesa) |
| Vulkan loader | `/nix/store/.../vulkan-loader-1.4.x/lib/libvulkan.so.1` |
| `libggml-vulkan.so` | Inside the `ollama-vulkan` package's `lib/ollama/` |
| GPU device access | `DeviceAllow=char-drm` on the ollama systemd unit (default in NixOS module) |

### NixOS configuration (in `~/loft/nixos-config`)

**`modules/fixed-packages/default.nix`** — overlay swaps in the Vulkan build:

```nix
overlay = (final: prev: {
  ollama = master.ollama-vulkan;   # was: master.ollama
  ...
});
```

**`modules/voxtype/default.nix`** — flips the runtime flag:

```nix
services.ollama = {
  enable = true;
  environmentVariables = {
    OLLAMA_KEEP_ALIVE = "8h";
    OLLAMA_VULKAN = "1";           # added
  };
  loadModels = [ "gemma3:4b" ];
};
```

### Verifying GPU is active

```bash
ollama ps
# PROCESSOR column should show "100% GPU"

journalctl -u ollama -n 50 | grep -iE "(vulkan|gpu|intel)"
# Should mention Vulkan backend / Intel device discovery
```

If `ollama ps` shows `100% CPU`, the Vulkan backend isn't loading. Check:
1. Is `OLLAMA_VULKAN=1` actually in the service env? `systemctl show ollama -p Environment`
2. Does the Ollama package contain `libggml-vulkan.so`?  
   `ls $(readlink -f $(which ollama) | sed 's|/bin/ollama||')/lib/ollama/`
3. Is the Intel ICD present? `ls /run/opengl-driver/share/vulkan/icd.d/intel*`

---

## Upgrading / swapping models

Change the `FROM` line in `Modelfile` and rebuild:

```bash
# Edit Modelfile, then:
ollama create voxtype-cleaner -f ~/.config/voxtype/Modelfile
```

The Modelfile itself contains comments about the upgrade path to
`qwen2.5-coder:7b` if quality becomes a problem.

Other candidates to consider (all open-weight, available via `ollama pull`):

- `qwen2.5-coder:3b` — middle ground between 1.5b and 7b
- `qwen2.5:7b` (generalist, not coder-specific)
- `llama3.2:3b` — already installed, weaker at code
- `phi4:14b` — slow but very obedient

---

## Debugging

### Per-call log

`cleanup.sh` writes to `/tmp/voxtype-cleanup.log` on every invocation:

```
14:32:08 RAW: {"model":"voxtype-cleaner","response":"...", "total_duration":..., ...}
14:32:08 IN: Um so I need to like check the git repo
14:32:08 OUT: I need to check the git repo.
```

If `OUT` is empty, the LLM call failed (likely model not found or Ollama
unreachable). Check the `RAW:` line for an error JSON.

To remove the logging later, delete the `echo ... >> /tmp/voxtype-cleanup.log`
lines in `cleanup.sh`.

### Key timing fields in the RAW response

- `load_duration` — time to load model into memory (≈0 if already warm)
- `prompt_eval_duration` — time to evaluate the system + user prompt
- `eval_duration` — time to generate the output tokens
- `total_duration` — total request time
- `prompt_eval_count` / `eval_count` — token counts

All durations are in nanoseconds. Divide by 1e9 for seconds.

### Common operations

```bash
# What models are pulled?
ollama list

# What's loaded in memory right now?
ollama ps

# Restart the daemon (after NixOS rebuild changes env vars)
sudo systemctl restart ollama

# Watch live logs
journalctl -u ollama -f

# Manually invoke cleanup for testing
echo "um so like I need to run git status" | ~/.config/voxtype/cleanup.sh

# Inspect the custom model's effective definition
ollama show voxtype-cleaner
ollama show voxtype-cleaner --modelfile
```

---

## Dead ends we explored (so future-you doesn't repeat them)

- **Switching from `/api/chat` to `/api/generate`** — saves negligible time
  (~100ms). Both endpoints re-evaluate the entire prompt every call.
- **Baking the system prompt into a Modelfile to "cache" it** — doesn't cache
  across HTTP calls. Still useful for organization, just not as a speed hack.
- **Building a persistent Python wrapper service to hold conversation state**
  — would actually save the ~1s prompt eval, but on GPU the prompt eval is
  already only ~60ms, so the engineering cost isn't worth it anymore.
- **Trying `kimi-k2.6:cloud` for "quality"** — 1T parameters via cloud is
  serious overkill for "remove um and fix punctuation." Cold starts dominate.

---

## Open questions / future work

- **Selective cleanup**: skip the LLM entirely for short prose, only invoke
  it for code-heavy dictations. Could be a profile in `config.toml` triggered
  by a hotkey modifier.
- **Streaming output**: currently `stream: false` blocks until the full
  response. Streaming wouldn't reduce total time but might let voxtype start
  typing earlier.
- **Eliminate `cleanup.sh`'s temp log**: useful for debugging, but adds
  filesystem writes on every dictation. Remove once stable.
