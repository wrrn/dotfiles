---
name: dispatch-rubric
description: Decide when to tackle work directly vs. dispatch a subagent, and which of the three dispatch patterns to use.
---

# Dispatch rubric

For coordinator-style agents that serve Warren and can spawn subagents via the `Agent` tool. Decide deliberately between tackling work yourself, dispatching a specialist, or punting back to Warren.

## When to dispatch vs. tackle directly

**Tackle directly** when the value is in *your* synthesis: brainstorming, thinking through a decision, summarizing a thread, drafting prose Warren will edit, retrieving and synthesizing from readable sources you already have access to.

**Dispatch** when:
- the task is in a different repo and needs codebase context;
- the task is mechanical or long-running (audit, scan, batch);
- the task fits a defined specialist.

Check `~/.pi/agent/agents/` (global) and `<repo>/.pi/agents/` (project) for what's available before deciding "tackle directly" by default — a specialist usually beats your generalist attempt.

## The three dispatch patterns

1. **Sync subagent** (`Agent` tool, `run_in_background: false`) — result returns to your context. Use when synthesis or composition is the value: you'll reason about the result, chain specialists, or iterate. **Always brief the subagent to return a concise synthesis, not raw transcript.** Heavy output landing in your context wastes Warren's budget and dilutes your judgment.

2. **Background subagent** (`Agent` tool, `run_in_background: true`) — returns an agent ID immediately; work runs concurrently and emits a styled completion notification. Use when the task is long-running, heavy, parallel-friendly, or should outlive the current exchange. Report the agent ID and a one-line description back to Warren so he can attach via `/agents` or steer with `steer_subagent`.

3. **Suggest Warren dispatches from `/agents` himself** — when fire-and-forget is the right shape and there's no value in routing through you (a quick PR review, a one-off scan he wants to monitor). Tell him exactly what to type; don't pretend you should be the middleman.

## Worktree isolation for mutating dispatches

When dispatching a background job that *mutates a repo*, set `isolation: "worktree"`. Changes land on a `pi-agent-<id>` branch, not the working tree — Warren can review and merge or discard cleanly. If the target repo has no commits yet, worktree creation will error; fall back to non-isolated dispatch with an explicit warning to Warren rather than silently dropping the flag.

## Briefing rules

When dispatching, always:
- Point the subagent at the relevant workspace context file (e.g. `~/dev/work/AGENTS.md`) and any thread file it should ground in.
- State what shape of output you want back (synthesis, list, file diff, recommendation).
- Set `max_turns` if the task is naturally bounded; let it run unlimited only when truly open-ended.
- Report back to Warren with the synthesis, not the raw transcript.

## Pre-dispatch safety check

If you have recently modified files in `~/.pi/agent/agents/` or `~/.pi/agent/skills/`, verify they parse before invoking the `Agent` tool. A broken YAML frontmatter in either directory takes out the dispatcher itself, not just the broken file — your first dispatch attempt will fail with the same error the user is trying to debug.

Quick check on the frontmatter of any `.md` file you have just edited in those directories:

```
awk '/^---$/{c++; next} c==1{print}' <file> | yq .
```

If it errors, fix before dispatching. Common cause is an unquoted colon mid-value in a scalar (e.g. `description: Fix X: do Y`). Either remove the colon or wrap the value in double quotes.

This applies to skills too — pi-subagents requires `description` in every skill's frontmatter, and a malformed or missing one will surface as a skill conflict warning rather than a hard error, but will still degrade the loaded agent.

## Rule of thumb when in doubt

Choose the pattern that minimizes your context cost without losing Warren's leverage. If your involvement wouldn't add synthesis value, push the work to background or to Warren-direct.
