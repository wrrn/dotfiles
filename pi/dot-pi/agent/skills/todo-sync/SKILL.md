---
name: todo-sync
description: Read Warren's tasks from his org-roam log, keep Two's own task state in todo.md, and push session-created tasks into the log.
disable-model-invocation: true
---

# Todo sync

Warren's tasks have **one** home: `~/.roam/YYYY-MM.org`. `~/honeycomb/todo.md` is not a copy of it; it holds only agent-owned state. Nothing lives in both.

## The org log

`~/.roam/` is an org-roam directory with one monthly log per month (`YYYY-MM.org`) plus timestamped topic notes. Warren reads and edits it in Emacs; Two reads it every session and writes to it only as described below.

**Conventions (observed 2026-10-13 — match them exactly, never impose new ones):**

- Day headings: `* Dayname, YYYY-MM-DD` with a properties drawer `:PROPERTIES:` / `:CREATED:  YYYYMMDD` / `:END:`.
- Tasks: `** TODO <short title>`, optional free-form body lines beneath; sub-tasks nest as `*** TODO`. Keywords in use: `TODO`, `DONE`, `NOT DOING`, `QUESTION`.
- Done items get `CLOSED: [date]` plus a `:LOGBOOK:` closing-note drawer, and stay where they are. `NOT DOING` items get a body line saying why.
- Timestamped journal entries use `** HH:MM <title>`; sub-entries nest with `***`.
- Links are org links: `[[file:...]]`, `[[id:...]]`, `[[https://...]]`. Thread links point at `~/honeycomb/threads/<slug>/index.md`.
- **Monthly migration (Bullet Journal style):** at month start, unfinished TODOs are *cut* from the old month and pasted under the new month's first day heading, subtrees intact. So: open items live only in the current month's file. Absence *after* a migration means done or deliberately dropped; mid-month absence means nothing.
- No time buckets. Order within the file is chronological by day heading, not by priority. Priority lives on the item (next two bullets), never in the file order.
- **Priority cookies (since 2026-10-22):** every open `TODO` carries one of `[#A]`, `[#B]`, `[#C]` right after the keyword: `** TODO [#A] <title>`. A = this is what Warren works on next, B = this week or deadline-bound, C = when there's room. Org treats a missing cookie as B, so never leave an open TODO without one. Sub-tasks get their own cookie. Cookies travel with the subtree at monthly migration.
- **Daily plan (since 2026-10-22):** when Warren sets an order for the day, it goes in a `** Plan` entry directly under today's day heading (after the properties drawer): a numbered list of org heading links, `[[*<heading text>][<short name>]]`, where the heading text excludes the keyword and cookie. The plan holds no task content; it only points. One plan per day; earlier days' plans stay as history and are never edited. Check that each link resolves (exact heading text) before saving.

## Reading

At session start, read the current month's log (and the previous month's during the first few days of a month, before migration). Open `TODO`s are the task list. Don't copy them anywhere. Lead with today's `** Plan` if there is one, then the `[#A]` items; mention B and C only when asked or when something is deadline-bound.

## `~/honeycomb/todo.md` — agent-owned state only

Three sections, all agent-only:

- **In progress** — background agent jobs and work Two started but hasn't finished. Remove on completion.
- **Pending push** — tasks decided in a session that aren't in the org log yet. Each entry is a candidate `** TODO`; push on sync, then delete here.
- **Two's own** — housekeeping for the agent setup. Never pushed to org.

Before adding to Pending push, check the org log: if the item already exists there, don't add it. The test for every line in `todo.md`: *would this be wrong to put in Warren's journal?* If it belongs in the journal, it goes to Pending push and leaves on the next sync.

## Writing to the org log

Two writes to the log in two cases:

1. **"Sync our to-do lists"** (or similar): push every Pending push item under a day heading for today — create it with the `:CREATED:` drawer if absent — as `** TODO [#X] <title>` (priority decided in the session; ask if it wasn't) with a brief body line and a `[[file:...]]` link to the thread index where relevant. Match the existing style exactly. Delete the pushed items from `todo.md`. Report what was pushed.
2. **Warren says something is done** in a session: mark it `DONE` in the log with `CLOSED:` and a one-line closing note in the logbook drawer, in the existing format. Only when he says so; don't infer completion from thread state.

Never reorder, re-bucket, or rewrite existing entries. The exceptions: set or change a priority cookie when Warren decides a priority, and write the day's `** Plan` when he sets an order. Never touch `DONE` or `NOT DOING` items except to add a closing note Warren dictates.
