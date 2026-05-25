---
name: thread-hygiene
description: Capture decisions to thread files and keep workspace state and agent memory from rotting.
---

# Thread hygiene

How to capture decisions, manage thread files in the workspace, and keep persistent state from rotting.

## Capturing what we figure out

- When a meaningful decision gets made, append the reasoning to the relevant thread file in `~/dev/work/`. The point is so future-Warren (and future-you) can pick it up cold.
- When a new thread becomes substantive enough to warrant a file, **propose** creating one — don't create files unilaterally for quick exchanges.
- After substantive thread updates, suggest updating the active-threads index in `~/dev/work/AGENTS.md` if the change matters to that summary.

## Where things belong — four buckets

A fact has one correct home. Pick deliberately.

| Bucket | Lives in | What goes there | Test |
|---|---|---|---|
| **Project / codebase facts** | `<repo>/AGENTS.md` | Build commands, conventions, repo layout, what not to touch | True for anyone working on this repo |
| **Cross-project workspace state** | `~/dev/work/*.md` | Active threads, decisions, people, repo index | True regardless of who is asking |
| **Operating heuristics about Warren** | `~/.pi/agent-memory/two/` | Style prefs, dispatch shortcuts, learned working patterns | True for Warren specifically |
| **Reusable coordinator protocols** | `~/.pi/agent/skills/` | Dispatch rubric, hygiene rules, anything another coordinator would also want | True for any coordinator agent |

**Repo-scoped Warren preferences** ("in `repo-foo`, Warren wants commits in style X") are a sub-case of the memory bucket. File them at `~/.pi/agent-memory/two/repos/<leaf-name>.md` — use the repo's leaf directory name (e.g. `pi-subagents.md`, not `gotgenes__pi-subagents.md`). Only switch to a disambiguated form (`<org>__<repo>.md`) if you ever hit a collision; until then, leaf-name is the convention. This whole pattern keeps repo-scoped prefs out of the shared `AGENTS.md` (which other tools and collaborators read).

### Decision tree

1. Would another person checking out this repo cold find it useful and correct? → **project file** in the repo.
2. Would another agent serving Warren want it? → **memory** (top-level if cross-repo, `repos/<name>.md` if scoped to one repo).
3. Would another coordinator agent serving a different user want it? → **skill**.
4. Otherwise (work state — threads, people, decisions) → **workspace**.

### Negative test for AGENTS.md

Before adding a line to any `AGENTS.md`, ask: would a stranger checking out this repo cold — no context on Warren, no instance of you — find this line useful and correct? If not, route it elsewhere via the decision tree.

This matters because `AGENTS.md` is shared-audience. Codex, pi-coding-agent, and other tools all read it, and it usually gets committed. Personal preferences land in collaborators' git logs.

## Memory entry frontmatter

pi-subagents prescribes a memory-entry frontmatter schema and injects it into your system prompt at startup (see `src/memory.ts` in the `@gotgenes/pi-subagents` package). The required fields are:

- **`name`** — the memory's name
- **`description`** — one-line description
- **`type`** — one of `user`, `feedback`, `project`, `reference`

For Two specifically, the four-bucket model narrows which `type` values you should actually use:

- `user` — facts about Warren (preferences, style, working patterns). Most common.
- `feedback` — specific feedback Warren has given about how to serve him.
- `reference` — conventions and lookup material (e.g. the leaf-name rule for `repos/`).
- `project` — allowed by pi, but your discipline says project facts belong in the repo's `AGENTS.md`, not memory. Use sparingly, and only for project-related state that genuinely can't live in the repo (rare).

### Two-local extension: `confidence`

Memory has a risk the workspace doesn't — you might write things Warren never said. Add a `confidence` field to every entry (this is a Two-local convention, not part of pi's schema, but additive frontmatter fields are fine):

- **`confidence: stated`** — Warren said "remember X." Treat as fact.
- **`confidence: demonstrated`** — Warren has repeatedly done or asked for X. Treat as default but stay open to override.
- **`confidence: inferred`** — you guessed Warren prefers X. Surface for confirmation before treating as established; raise in conversation, don't silently rely.

Proactive memory writes should always start as `inferred`. Promote to `demonstrated` after the pattern repeats; to `stated` only when Warren confirms.

### Example entry

```markdown
---
name: terse-sync-style
description: Warren prefers short Slack-style replies for quick sync exchanges.
type: user
confidence: stated
---

For quick syncs and one-off questions, default to terse Slack-style replies
— no headers, no bullet lists, no preamble. Reserve structured answers for
multi-part questions or decisions.
```

## Hygiene

- Threads complete eventually. When one wraps, suggest archiving or pruning its entry from the active-threads index.
- Memory entries can go stale. **Before recommending from memory, verify the underlying file/state still matches.** Update or remove stale memories rather than acting on them.
- `~/dev/work/repos.md` should be refreshed when repos are added or removed under `~/dev/`.
- Don't fabricate references to repos, files, threads, or people. If unsure, search or ask.
