---
name: todo-sync
description: Maintain todo.md and reconcile it with Warren's external to-do source.
---

# Todo sync

How to maintain `~/dev/work/todo.md` (the session-layer task list) and reconcile it with Warren's external to-do source.

> **STUB:** The external source step is unconfigured. Joey's version used Apple Notes pinned "To do" via osascript. Warren: pick one (Apple Notes, Things, plain-text file, none) and edit step 1 of "Sync our to-do lists" accordingly, or delete that subsection.

## Maintaining todo.md

- When a task gets done in a session, mark it `[x]` in `todo.md`.
- When a new task is decided in a session, add it to the right section.
- When a background agent job finishes, remove or update its entry under "In progress."
- Keep the file honest: don't let it grow stale. A task that's clearly done but unmarked is noise.

## "Sync our to-do lists"

When Warren says this (or similar), do the following:

1. Read the external to-do source. *(STUB: fill in — e.g. Apple Notes pinned "To do" via osascript.)*
2. Read `~/dev/work/todo.md`.
3. **Add** any items present externally but missing from `todo.md` — put them in the right section by time bucket (This week / Sooner / Later).
4. **Remove or mark done** any items that were previously synced into `todo.md` but are no longer in the external source — absence = complete.
5. **Never touch** sections that are your own additions (Agent tooling, In progress) — those don't exist externally and shouldn't be deleted.
6. Report a brief diff: what was added, what was removed.

The external list is Warren's ground truth for personal tasks. `todo.md` adds the agent/session layer on top. Sync merges them without clobbering either side.
