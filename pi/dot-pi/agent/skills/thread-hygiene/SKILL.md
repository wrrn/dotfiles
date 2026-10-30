---
name: thread-hygiene
description: Capture decisions to thread files, decide where a fact belongs, and keep workspace state from rotting.
disable-model-invocation: true
---

# Thread hygiene

How to capture decisions, manage thread directories in the workspace, decide where a fact belongs, and keep persistent state from rotting.

## Thread layout

One directory per **workstream** (not per ticket) under `~/honeycomb/threads/`. A workstream is the unit you'd describe in one sentence; neighbouring tickets that share decisions belong in the same directory.

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
4. **Next** — point at the org log; one line of headline items at most. Don't duplicate the task list.
5. **Files** — one line per artifact saying what it is and whether it's maintained.
6. **Ticket conflicts** — *optional; include only when the thread has one.* Where a Jira ticket's description or acceptance criteria conflict with a decision Warren made. Per entry: the ticket, what it says, what we decided instead, and whether Warren has chosen what to do about it. Don't edit the ticket (see "Working norms" in `~/honeycomb/AGENTS.md`).

Target ≤ 60 lines. If it's growing past that, something belongs in `log.md` or an artifact.

### `log.md`

`## YYYY-MM-DD — <title>` entries, newest at the bottom. Never edit old entries except to add a "corrected later" note. Link to artifacts by relative path.

## Reading discipline

- When a task touches a thread, read `index.md` **first**, always. Read `log.md` only when the task needs history (why a decision was made, what was tried).
- Reading `index.md` is also how it gets checked: if it contradicts what you then learn from the log, the repo, or Warren, fix it in the same session. Staleness caught on read is the mechanism; don't rely on remembering to update it later.

## Capturing what we figure out

- When a meaningful decision gets made, append a dated entry to the thread's `log.md`. The point is so future-Warren (and future-you) can pick it up cold.
- **After every `log.md` append, update `index.md`** — status, decisions in force (replace the superseded one), next, files, ticket conflicts. An append without an index update is the main way the index rots.
- When a new thread becomes substantive enough to warrant a directory, **propose** creating one — don't create directories unilaterally for quick exchanges.
- After substantive thread updates, check the active-threads table in `~/honeycomb/AGENTS.md`. It holds only name, link and a one-word status; the index owns everything else. Don't let detail creep back into the table.

## Where things belong — three buckets

A fact has one correct home. Pick deliberately. There is no separate agent-memory store.

| Bucket | Lives in | What goes there | Test |
|---|---|---|---|
| **Project / codebase facts** | `<repo>/AGENTS.md` | Build commands, conventions, repo layout, what not to touch | True for anyone working on this repo |
| **Workspace** | `~/honeycomb/*.md` | Threads and decisions; people; the repo index. Two subsections hold facts about Warren: **"Working norms"** in `AGENTS.md` (how Warren wants to be served, across repos) and **`repos.md`** (Warren-specific preferences for one repo, next to that repo's entry) | True about Warren's work or how he works |
| **Skills** | `~/.pi/agent/skills/` (protocols) or `~/burrow/skills/` (tool skills) | Coordinator protocols; facts about a tool any agent using it needs (e.g. acli quirks in the `jira` skill) | Another agent doing the same kind of work would want it |

### Decision tree

1. Would another person checking out this repo cold find it useful and correct? → the repo's `AGENTS.md`.
2. Is it about a tool or environment, and would any agent using that tool want it? → that tool's skill (or a new skill, proposed first).
3. Is it about how Warren works or wants to be served? → "Working norms" in `~/honeycomb/AGENTS.md`; if it's scoped to one repo, that repo's entry in `~/honeycomb/repos.md`.
4. Otherwise (work state: threads, people, decisions) → the workspace.

### Negative test for a repo's AGENTS.md

Before adding a line to a repo's `AGENTS.md`, ask: would a stranger checking out this repo cold — no context on Warren, no instance of you — find this line useful and correct? If not, route it elsewhere via the decision tree. Repo `AGENTS.md` files are shared: Codex, pi and other tools read them, they usually get committed, and personal preferences end up in collaborators' git logs.

## Propose, then write

Applies to facts about Warren (Working norms, `repos.md`) and to tool or environment facts (skills, repo `AGENTS.md`).

- **Warren says "remember X" or "from now on do Y"** → write it immediately to the right bucket. Brief acknowledgement, no friction.
- **Two notices something on its own** → propose it in one line, in-band, naming the destination: *"You've corrected X twice — add 'prefer Y' to Working norms?"* Write only on yes. A no means drop it; don't re-propose the same thing in the same session.
  - **About Warren:** propose only after the pattern has recurred (at least twice, in different contexts), and only for how to operate, not a one-off task. Never propose writing down judgments about people or relationships; those are written only when Warren asks.
  - **About a tool:** propose only what you've verified (reproduced, or checked against docs). Include the date and how it was verified in the text itself, e.g. "(2026-10-17, PER-5699 comment)".
- Each entry is a sentence or short paragraph ending with its source and date in parentheses, e.g. "(Warren, 2026-10-14)". No frontmatter or confidence levels: everything written was either stated or approved.
- Edits under `~/burrow/dotfiles` or `~/burrow/skills` stay uncommitted; say which files changed so Warren can describe and commit.

## Hygiene

- Threads complete eventually. When one wraps: set `index.md` status to **Done** with a date and a one-line outcome, `mv threads/<slug> threads/_archive/<slug>`, and drop the row from the `AGENTS.md` table. Propose it; don't do it unasked.
- `~/honeycomb` is a colocated jj+git repo. Describe the working-copy change when a session makes substantive workspace edits (`jj describe -m ...` then `jj new`); no manual backup files.
- Recorded facts go stale. Before acting on a Working norm, `repos.md` preference, or skill fact, check that the file or state it describes still matches. When it doesn't, fix or remove the entry in the same session and tell Warren.
- `~/honeycomb/repos.md` should be refreshed when repos are added or removed.
- Don't fabricate references to repos, files, threads, or people. If unsure, search or ask.
