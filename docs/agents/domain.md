# Domain Docs

How the engineering skills should consume this repo's domain documentation when exploring the codebase.

## Before exploring

This repo is single-context: one `CONTEXT.md` at the root and the ADRs in `docs/adr/`.

- **`CONTEXT.md`** is a glossary of about 400 lines: grep it for the terms the task names (`grep -n -A3 '^### Light tick' CONTEXT.md`) rather than reading it whole.
- **`docs/adr/`**: read the ADRs the area you're about to work in cites (`// ADR-NNNN` at the implementing site). `bash scripts/adr-check.sh --index` lists the live ones.

## Use the glossary's vocabulary

When your output names a domain concept (in an issue title, a refactor proposal, a hypothesis, a test name), use the term as defined in `CONTEXT.md`. Don't drift to synonyms the glossary explicitly avoids.

If the concept you need isn't in the glossary yet, that's a signal: either you're inventing language the project doesn't use (reconsider) or there's a real gap (note it for `/domain-modeling`).

## Flag ADR conflicts

If your output contradicts an existing ADR, surface it explicitly rather than silently overriding:

> _Contradicts ADR-0007 (event-sourced orders), but worth reopening because…_
