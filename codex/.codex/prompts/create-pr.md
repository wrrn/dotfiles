---
description: Create a GitHub pull request using pull_request_template.md, jj diff, and gh.
---
# /create-pr

You create a GitHub pull request for the current repo.

## Constraints
- Use `pull_request_template.md` to generate the PR description.
- Use `jj diff` (not git diff) to determine the change summary.
- When calling `gh pr create`, always include `--head <branch>`.

## Workflow
1. Confirm this is a Jujutsu repo by running `jj status` or `jj root`.
2. Determine the PR head branch (bookmark).
   - Run 
     ```
       jj log \
        --revisions 'heads(ancestors(@) & bookmarks())' \
        --no-graph \
        --template 'local_bookmarks'
     ```
   - If empty, ask the user which branch/bookmark to use for `--head`.
   - If multiple, ask which one to use.
3. Collect change details with `jj diff --from develop` (use the current change).
4. Read `pull_request_template.md` and fill it in.
   - Ask for a ticket ID if you cannot infer it; otherwise keep the `PER-` placeholder.
   - Draft a concise narrative from the `jj diff` summary.
   - Ask which tests were run; if none, say "Not run (not requested)" in Testing.
   - Keep the TODO checkboxes; check only what the user explicitly confirms.
5. Compose a PR title. If not provided, propose one and confirm with the user.
6. Create the PR with `gh pr create` using `--head <branch>` and a body file.

## Notes
- Prefer `--body-file` with a temp file for the description.
- If the repo has no changes, stop and ask whether to proceed.
- Do not use git for VCS actions unless explicitly requested.
