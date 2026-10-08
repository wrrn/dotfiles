---
name: todo-sync
description: Maintain todo.md and reconcile it with Warren's external to-do source.
---

# Todo sync

How to maintain `~/dev/work/todo.md` (the session-layer task list) and reconcile it with Warren's external to-do source.

## External source: org-roam monthly logs

Warren's ground truth is `~/.roam/` — an org-roam directory with one monthly log per month (`YYYY-MM.org`, e.g. `2026-10.org`) plus timestamped topic notes.

**Conventions (observed 2026-10-13 — match them exactly, never impose new ones):**

- Day headings: `* Dayname, YYYY-MM-DD` with a properties drawer `:PROPERTIES:` / `:CREATED:  YYYYMMDD` / `:END:`.
- Tasks: `** TODO <short title>`, optional free-form body lines beneath. Keywords in use: `TODO`, `DONE`, `QUESTION`.
- Done items get `CLOSED: [date]` plus a `:LOGBOOK:` closing-note drawer, and stay where they are.
- Timestamped journal entries use `** HH:MM <title>`; sub-entries nest with `***`.
- Links are org links: `[[file:...]]`, `[[id:...]]`, `[[https://...]]`.
- **Monthly migration (Bullet Journal style):** at month start, unfinished TODOs are *cut* from the old month and pasted under the new month's first day heading, subtrees intact (verified: an October TODO carries a `<2026-02-10 ...>` sub-entry; carried items exist only in the current month). So: open items live only in the current month's file; absence *after* a migration means done or deliberately dropped. Mid-month absence means nothing — the item may simply never have been logged.

## Maintaining todo.md

- When a task gets done in a session, mark it `[x]` in `todo.md`.
- When a new task is decided in a session, add it to the right section.
- When a background agent job finishes, remove or update its entry under "In progress."
- Keep the file honest: don't let it grow stale. A task that's clearly done but unmarked is noise.

## "Sync our to-do lists"

When Warren says this (or similar), do the following:

1. Read the current month's log (`~/.roam/YYYY-MM.org`). If it's within the first few days of a month, also read the previous month's — migration may not have happened yet.
2. Read `~/dev/work/todo.md`.
3. **Add** open `TODO`s from the log that are missing from `todo.md`. The log has no time buckets, so place by judgment (This week / Sooner / Later) and say where you put them; ask if genuinely unclear.
4. **Mark done or remove** `todo.md` items that are `DONE` in the log, or that are absent from the current month *after* a monthly migration.
5. **Push** session-created tasks into the log: append under a day heading for today (create it with the `:CREATED:` drawer if absent), as `** TODO <title>` with a brief body line and an org `[[file:...]]` link to the thread file where relevant. Match the existing style exactly.
6. **Never touch** sections that are agent-only additions (In progress) — those don't exist in the log and shouldn't be deleted.
7. Report a brief diff: what was added, removed, and pushed.

The external list is Warren's ground truth for personal tasks. `todo.md` adds the agent/session layer on top. Sync merges them without clobbering either side.
