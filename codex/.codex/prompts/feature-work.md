---
description: Build a feature, update a feature, or make architectural changes.
---
# Feature Work

## Design log methodology

Projects follow a design log methodology for features and architectural changes.
For rationale behind these rules, see `~/design-doc-principles.md`.

### When a design log is required

**Required:**
- New features or capabilities
- Architectural changes (new patterns, dependencies, data flows)
- Changes that cross system boundaries (e.g., CSG/XCM sync, external APIs)
- When the user explicitly requests one

**Not required:**
- Bug fixes with a clear root cause
- Refactors that don't change behavior or interfaces
- Configuration changes
- Single-file changes with obvious scope

When in doubt, ask the user.

### Before making changes
1. Check `./design-log/` and `~/.roam/` (personal org-roam knowledge base) for
   existing designs and implementation notes
2. Read related design logs to understand context and constraints
3. If a design log is required: create it first, then implement in a separate
   session

### Design log template

File naming: `YYYY-MM-DD-slug.md`
Location: `./design-log/` by default, or `~/.roam/` when the user requests it

```
# Title

## Background
Narrative context. What exists today, why this work matters.
1-3 paragraphs. Link to related design logs.

## Problem
What specific problem does this solve? Why now?

## Goals
Bulleted list. What the system WILL do when this is complete.

## Non-Goals (optional for small scope)
Bulleted list. What this work explicitly WILL NOT do.

## Questions and Answers
Numbered Q/A pairs. Keep questions after answering them.
Mark unresolved items with [TBD].

1. Q: ...
   A: ...
2. Q: ...
   A: [TBD]

## Constraints and Boundaries (optional for small scope)
- **Always**: Safe defaults requiring no approval
- **Ask First**: High-impact changes requiring review
- **Never**: Hard stops

## Design
Architecture, key components, data flow.
Reference specific files and existing patterns by path.
Use mermaid diagrams inline where they clarify flow or architecture.

## Alternatives Considered (optional for small scope)
What was rejected and why. 1-2 sentences per alternative.

## Implementation Plan
Numbered phases/steps. Each step includes:
- What to do (file paths, type signatures)
- How to verify (test command, expected output)
- Dependencies on prior steps

## Examples
Realistic usage examples with code snippets.
Use ✅ / ❌ for good/bad patterns.

## Trade-offs
What this design sacrifices and what it gains.

## Implementation Results
(Appended during/after implementation)
- Summary of what was built
- Deviations from the original design and why
```

### Format conventions
- **Q&A**: Numbered pairs with `Q:` / `A:` prefixes. Keep questions after
  answering. Use `[TBD]` for unresolved items.
- **Diagrams**: Use mermaid. Inline where they clarify flow or architecture.
- **Examples**: Use `✅ / ❌` for good/bad patterns. Include realistic code.
- **Specificity**: Include file paths, type signatures, validation rules.
  Reference existing codebase patterns by path.
- **Brevity**: Write short explanations. Only include what is most relevant.
  If a design log exceeds ~500 lines, decompose into a parent document with
  linked child documents per phase.

### When implementing
1. Follow the implementation plan phases from the design log
2. Write tests first or update existing tests to match new behavior
3. Do not modify original design sections once implementation has started
4. Append to the design log with an "Implementation Results" section as you go
5. Document deviations: explain why implementation differs from the design
6. After implementation, add a summary of deviations from the original design

### When answering questions
1. Reference design logs when relevant
2. Use codebase terminology
3. Show type signatures
4. Consider backward compatibility: default to non-breaking changes
