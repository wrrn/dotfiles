# Global Agent Rules

These rules apply to all Codex sessions for this user.

## Version Control

- Prefer Jujutsu (`jj`) for all version control operations.
- Translate git-flavored requests into the equivalent `jj` command unless the user explicitly asks for Git.
- Only use `git` when:
  - the user explicitly requests `git`
  - `jj` cannot perform the required operation
- Check repository state with `jj status` or `jj root` before making version control changes.
- Prefer these translations:
  - `git status` -> `jj status`
  - `git log` -> `jj log`
  - `git diff` -> `jj diff --git`
  - `git show` -> `jj show`
  - `git commit -m "msg"` -> `jj describe -m "msg"` or `jj commit -m "msg"` when explicitly needed
  - `git checkout <rev>` -> `jj edit <rev>`
  - `git checkout -b <name>` -> `jj new` and `jj bookmark create <name>` when a bookmark is needed
  - `git branch` -> `jj bookmark list`
  - `git push` -> `jj git push`
  - `git fetch` -> `jj git fetch`
  - `git pull` -> `jj git fetch` and then `jj rebase` or another explicit integration step
- Ask before destructive history operations such as `jj abandon`, `jj rebase`, `jj squash`, or commands that would discard changes.

## Instruction Precedence

- Treat this file as the default global policy.
- If a repo-local `AGENTS.md` also exists, follow both files together.
- Repo-local instructions may add project-specific workflow requirements, but Jujutsu remains the default VCS unless the repo explicitly requires Git.
