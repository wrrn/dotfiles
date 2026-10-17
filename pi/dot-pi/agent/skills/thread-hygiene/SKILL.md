---
name: thread-hygiene
description: Capture decisions to thread files and keep workspace state and agent memory from rotting.
---

# Thread hygiene

How to capture decisions, manage thread directories in the workspace, and keep persistent state from rotting.

## Thread layout

One directory per **workstream** (not per ticket) under `~/dev/work/threads/`. A workstream is the unit you'd describe in one sentence; neighbouring tickets that share decisions belong in the same directory.

```
threads/<slug>/
  index.md     # current state — the only file read by default
  log.md       # dated, append-only journal — read when you need the why
  <artifacts>  # handoff docs, posted comments, prompts, jira/ snapshots
threads/_archive/<slug>/   # finished workstreams, moved whole
```

Every thread has `index.md`, even a tiny one. Uniform layout beats a rule with branches.

### `index.md` — fixed sections, in this order

1. One-paragraph summary: what it is, which tickets/repos, who else is involved.
2. **Status** — one bold word plus date, then a short table if there are several moving pieces.
3. **Decisions in force** — a flat, undated bullet list. Superseded decisions are *replaced*, not appended; the history lives in `log.md`.
4. **Next** — point at `todo.md`; one line of headline items at most. Don't duplicate the task list.
5. **Files** — one line per artifact saying what it is and whether it's maintained.

Target ≤ 60 lines. If it's growing past that, something belongs in `log.md` or an artifact.

### `log.md`

`## YYYY-MM-DD — <title>` entries, newest at the bottom. Never edit old entries except to add a "corrected later" note. Link to artifacts by relative path.

## Reading discipline

- When a task touches a thread, read `index.md` **first**, always. Read `log.md` only when the task needs history (why a decision was made, what was tried).
- Reading `index.md` is also how it gets checked: if it contradicts what you then learn from the log, the repo, or Warren, fix it in the same session. Staleness caught on read is the mechanism; don't rely on remembering to update it later.

## Capturing what we figure out

- When a meaningful decision gets made, append a dated entry to the thread's `log.md`. The point is so future-Warren (and future-you) can pick it up cold.
- **After every `log.md` append, update `index.md`** — status, decisions in force (replace the superseded one), next, files. An append without an index update is the main way the index rots.
- When a new thread becomes substantive enough to warrant a directory, **propose** creating one — don't create directories unilaterally for quick exchanges.
- After substantive thread updates, check the active-threads table in `~/dev/work/AGENTS.md`. It holds only name, link and a one-word status; the index owns everything else. Don't let detail creep back into the table.

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

### Provenance fields (Two-local additions)

Alongside `confidence`, add three more fields to every entry. They make the write-discipline rules below implementable.

- **`added`** — ISO date the entry was created.
- **`last_verified`** — ISO date an instance most recently confirmed the entry. Bump on confirming instances; do **not** bump on contradictions (those demote).
- **`source`** — short note on what generated the entry. For `stated`, it's "warren-said" plus session/date context. For `inferred`, the pattern that triggered it.

### Example entry

```markdown
---
name: terse-sync-style
description: Warren prefers short Slack-style replies for quick sync exchanges.
type: user
confidence: stated
added: 2026-05-25
last_verified: 2026-05-25
source: warren-said-2026-05-25
---

For quick syncs and one-off questions, default to terse Slack-style replies
— no headers, no bullet lists, no preamble. Reserve structured answers for
multi-part questions or decisions.
```

## Write discipline

When to write to memory, when to surface, when to delete.

### When to write

| Trigger | Write policy | Confidence at creation |
|---|---|---|
| **Explicit** — Warren says "remember X" / "from now on do Y" | Write immediately. Brief acknowledgment, no friction. | `stated` |
| **Strong proactive** — clear repeated pattern that passes all four gates below | Write as `inferred`. Mention once, in-band (see "Acting on inferred entries"). Do not ask permission. | `inferred` |
| **Weak proactive** — single instance, ambiguous, possibly one-off | Don't write. Hold in working memory for the session. Wait for recurrence. | — |

### Strong-pattern gates

All four must hold before a proactive write:

1. **Recurrence** — at least 2 instances in different contexts. Same task twice doesn't count.
2. **Generality** — about *how to operate*, not a specific one-off task.
3. **Stability** — not obviously time-bound. "Warren is in EST right now" fails this gate.
4. **Non-sensitive** — not about a person, a relationship, or a strategic judgment. Those require an explicit write.

If any gate fails: don't write proactively. Either ask Warren explicitly or adapt for the session and let it go.

### Acting on inferred entries

When about to *act* on an `inferred` entry (not just internally rely on it), surface it in one line, in-band, no apology, no permission-ask:

> *Defaulting to terse Slack-style for this reply since you've used that pattern — flag if wrong.*

Stakes calibration:

- **Low** (style, formatting, terseness) — act and note in one line.
- **Medium** (dispatch defaults, tool choices) — act and note, slightly more verbosely.
- **High** (consequential decisions, anything affecting other people) — do not act on `inferred` alone. Ask first, then promote to `stated` once Warren confirms.

### Promotion, demotion, deletion

| Transition | Trigger |
|---|---|
| `inferred` → `demonstrated` | Another consistent instance, no contradiction. Bump `last_verified`. |
| `demonstrated` → `stated` | Warren explicitly confirms or references the entry. |
| `stated` → `demonstrated` | Warren contradicts in a specific case. |
| `demonstrated` → `inferred` | Warren contradicts; the broader pattern is in doubt. |
| `inferred` → deleted | Warren contradicts; or `last_verified` is more than 30 days old. |
| any → deleted | Warren says "forget X", or removes the underlying thing the entry references. |

Demotion before deletion: one contradiction might be context-specific. Drop a confidence step, observe, then delete only if the pattern doesn't recover.

### Staleness

`inferred` entries with `last_verified` more than 30 days old are deleted on next encounter — they were tentative bets that didn't pan out. `demonstrated` and `stated` entries do **not** auto-decay; they require active contradiction or an explicit "forget X" to remove. The standing rule ("verify the underlying file/state before recommending") still applies on every read.

## Hygiene

- Threads complete eventually. When one wraps: set `index.md` status to **Done** with a date and a one-line outcome, `mv threads/<slug> threads/_archive/<slug>`, and drop the row from the `AGENTS.md` table. Propose it; don't do it unasked.
- `~/dev/work` is a colocated jj+git repo. Describe the working-copy change when a session makes substantive workspace edits (`jj describe -m ...` then `jj new`); no manual backup files.
- Memory entries can go stale. **Before recommending from memory, verify the underlying file/state still matches.** Update or remove stale memories rather than acting on them.
- `~/dev/work/repos.md` should be refreshed when repos are added or removed under `~/dev/`.
- Don't fabricate references to repos, files, threads, or people. If unsure, search or ask.
