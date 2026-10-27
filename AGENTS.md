# fabot

`ARCHITECTURE.md` is the only design document. The code decides anything it does
not; no other file is a source of truth. v1 (the `v1` bookmark) is source to
port from, never a fact about v2.

Reading costs more than writing: every line is written once and read by
every agent and every review after. Prefer deleting to adding, a type or a
name to a sentence, one place for a fact to two, and short reports to long
ones.

## Boundaries

- A module's state is the row it owns in `ARCHITECTURE.md` §4. Adding a store,
  a memo, a `thread_local!` or `static`, a Memory or segment read outside
  the driver (`app/driver.rs`), a `Tuning` field, or any cooldown / latch / grace / hold-while /
  hysteresis: **stop and ask the owner.** These are architecture changes and
  never fixes. Owner away: label the issue `needs-owner`, park it, take other
  work. Never work around it.
- `Fight` and `Economy` do not import each other; only `Strategy` imports
  `Season`. Declarations flow down §3's table, never up.
- No room name, player name or object id in source. They are `declarations`.
- Line and comment-share ceilings (`scripts/ceilings.json`, `npm run ceiling`)
  are never raised. A change over either pays for itself by deleting first.
- `decide` is deterministic: no clock, no unseeded random.
- Derived facts are computed once into `Derived` and passed down. A decision
  function takes `Derived`, never `World`. The only cache is `Atlas`'s
  flood table; a new memo, `HashMap` cache or lazy cell outside
  `Derived` is an architecture change (stop and ask).
- Count ceilings (`scripts/ceilings.json`, read by `npm run scenario`) are a
  gate like line ceilings: never raised.

## Fixing behaviour

- v1 (`v1`) ran live and solved much of the economy already. Before
  designing a fix, find how v1 solved the problem (its code and
  `docs/adr/`, read with `git show v1:<path>`) and port that mechanism
  with its invariant; drop or replace it only when it was a one-incident
  patch or no longer applies, and say which in the commit. A v1 cap, floor
  or extra body bolted onto a formula marks where the formula was wrong:
  fix the formula, never drop the patch silently.
- Port a mechanism with the links it depends on. The economy is one chain
  per level (dig, carry, sinks, what spends the surplus); before a soak,
  trace it through v2 and land the missing links together. A soak of one
  link without the rest measures the missing link.

- Name the invariant the bug breaks in the commit, in one sentence that covers
  every body, room or colony. A new rule that mops up after an old one is not a fix.
- A fight fix changes a phase rule, a tactic or `Exchange`. It never adds a
  per-tick check. If the bug is "we changed our mind mid-fight", the fix is in
  the event that re-evaluated, not a latch.
- Net lines added by a fix should be ≤ 0. A positive diff says why in the
  commit.
- Simple and working beats clever, even if slower. Start with the simple
  rule (a fixed order, the engine's allowance, a plain threshold); replace it
  with a priced decision only when a soak shows the simple rule losing, and
  say by how much in the commit. A simple rule lives in one function shaped
  as its priced replacement would be (the facts it would read in, a rank or
  a choice out), so the swap changes that function and nothing else; never
  scatter a rule across its callers.
- Where a decision is priced, it is priced when a commitment is made
  (`Exchange.cheapestWin`, `Strategy`), not re-read per tick.

## Writing prose

Docs and comments are written when asked, or when the fact passes the tests
below.

Comments:
- Code says *what*; a comment says only *why*. The default is no comment.
  Reach first for a name, a type or a test; comment only what none of them
  can hold: a reason, an engine constraint, the owner's decision, a rejected
  alternative. One or two lines.
- Paraphrase test: hide the code and read the comment. If it tells you what
  the code does, it is a paraphrase; delete it. The surrounding code's
  comment density is no precedent: match its naming, never its prose.
- The invariant a fix restores, the incident and the change's story go in
  the commit. An open item is an issue, or a ❓ in `ARCHITECTURE.md`.
  `npm run ceiling` fails on an issue or ADR citation in code.

Docs:
- Docs live in `ARCHITECTURE.md` (the design), `AGENTS.md` (these rules) and
  `README.md` (setup). A `GLOSSARY.md` only once a term needs one. Do not
  create other markdown files unless asked.
- Before writing a fact, search for it. If it exists, link to it.
- To add a section, look for one to delete first.
- Plans, session notes and completion reports go in chat or the commit, not
  in files.
- No ADRs. A decision that is hard to reverse, surprising without context
  and a real trade-off changes `ARCHITECTURE.md`, with its why in one or two
  sentences there and the rest in the commit. Agreeing on a term with the
  owner is not a trade-off: that goes in `GLOSSARY.md` or nowhere.

## Verification

- A behaviour fix is verified by an xxscreeps scenario that reproduces the
  incident passing, and the scenario library not regressing
  (`ARCHITECTURE.md` §10.2). Add the scenario to the library.
- An offence scenario (siege, sweep, drain: anything that attacks a rival)
  seats a real bot as the rival, Hivemind or TooAngel (`start.opponents`):
  what an offence pays is proven against a bot that spawns, works and fights
  back. A scripted rival (`engine/rival`'s roles)
  checks only that the plan runs as planned (raze order, positioning, gate
  tile), and its `why` says why a real bot does not fit.
- An `Exchange` change is verified by the prediction error on the generated
  set not rising (§10.1).
- An economy change is read off the soak's curves over the long horizon
  (`npm run soak -- --ticks 30000`, `ARCHITECTURE.md` §5, §10.2): long-run
  growth is the objective, and an early regression is accepted when the end
  of the run improves. Before shipping, `npm run soak:many` (the
  seat extremes) over its default starts (two seeds and a late start): no room regresses,
  where a loss smaller than that room's spread across the starts on the
  base is noise.
- Unit tests only for invariants and pure arithmetic. A test that asserts one
  creep's action in one scenario is rejected in review.
- `npm test` (unit) stays under a minute. Scenarios and soak run on `jj ship`
  for the modules a change touches, and nightly in full.
- After a large fight change the owner watches its siege, sweep and defend
  scenarios in their client, lazycreep (`npm run scenario -- <name> --watch`).
  A result is reported as the scenario's name, its measured numbers and that
  command, never as a bare pass. A fault the owner sees there becomes a
  measure and a scenario that fails on the old code.

## Performance

- No performance change without the representative benchmark
  (`ARCHITECTURE.md` §9.1: live capture, engine cost model, calibrated against
  `observe cpu`). If the benchmark is not representative, fix the benchmark.
- The dialect in §9.2 is a default, not a rule: a measurement overrides it
  either way, and the commit quotes the measurement.
- A speed claim is an interleaved A/B on that benchmark with identical Memory
  fingerprints, or a count. A wall-clock number alone is not a claim.

## Version control

**jj** (colocated with git): every mutation goes through `jj`; git is read-only. Solo repo: no PRs, no feature branches, work on `main`.

### Shipping an issue

1. `/code-review` has run on the diff and its findings are resolved. Every
   added comment line passes the paraphrase test, or a finding deletes it.
2. `jj describe` with a conventional-commit subject, one `Fixes #<n>` trailer line per issue and `Reviewed: code-review` (ship.sh refuses a change without it).
3. `jj ship [-r REV]` (`scripts/ship.sh`): the gates (`npm test`: `cargo fmt --check`, line ceilings, clippy, `cargo test`; then the build, `npm run smoke` and `npm run scenario` over the scenarios whose `modules` the change touches, every one when it touches code outside `src/core/<module>/`), then the `claude.session-branch` bookmark (`main` locally, set once with `jj config set --repo claude.session-branch main`) moved to REV and pushed. Its gates are the pre-push run; don't run `npm test` again before it. A pushed change is immutable.
   Before it, `jj log -r 'main..REV'`: every change listed ships.
4. `npm run deploy` builds, smokes and uploads the bundle to the live server; the first tick after it is a cold reset.

Squash TDD slices into one change per issue; push per issue.

A live firefight the user flags may be fixed in the current session without the subagent loop; steps 1–3 still apply.

### Working alongside

- A background task or subagent notifies when it finishes: end the turn. Wait on the game with `npm run observe -- wait --ticks N`.
- Stop a background job with TaskStop. A long job runs with
  `run_in_background` and the turn ends: its exit notifies you (a
  subagent too). Inside one command, wait by pid (`wait`, `kill -0`), never
  a loop on `pgrep -f`/`pkill -f`, which matches its own shell; a
  PreToolUse hook (`no-self-pgrep.sh`) refuses one. A wait on a condition
  (a file, a count of lines) uses Monitor, or a loop under `timeout`: a bare
  `until` loop never ends if the condition never comes.
- A subagent that edits runs isolated — Claude Code passes `isolation: "worktree"`, pi passes
  `cwd: "auto"` to `subagent`, and `.pi/extensions/worktree.ts` runs the hooks Claude Code ran
  itself (pi-subagents' own `worktree: true` is a git worktree, so it is refused). The repo's WorktreeCreate hook (`.claude/hooks/worktree-create.sh`, registered in `.claude/settings.json`) gives it its own jj workspace at `.claude/worktrees/<name>` (ignored), based on `main` (`trunk()`, or `claude.worktree-base` when set), with `node_modules` linked and `.env` copied; anything machine-local goes in an untracked `.claude/worktree-setup.local.sh`. The subagent describes the workspace's own `@` (no `jj new` before its first edit, which leaves an empty change under its work) and never moves a bookmark or pushes.
- Ship a subagent's change from the default workspace: `jj new <change>` (rebase it onto `main` first if `main` moved), then steps 3–4 with `-r <change>`. A subagent's workspace is removed when it stops with no work in it and no background task running (`subagent-stop.sh`); one holding work stays, so the agent can be resumed to amend it, until session end (`worktree-sweep.sh`); by hand, `.claude/hooks/worktree-sweep.sh --stale` forgets workspaces whose directory is gone and removes directories no workspace owns. Each removal first kills what still runs in the workspace (`kill-cwd.sh`).
- Another live session works in its own workspace too, or stays read-only.
- `/tmp` is a RAM tmpfs here, shared by every agent: a build target, bundle, extracted tree or log that outlives one command goes under the workspace's `.scratch/` (ignored, on disk), and evidence a report cites under the default checkout's. Each agent deletes what it wrote there once nothing cites it.

### Cloud sessions

A cloud session (claude.ai/code) may push only the branch it was cloned on, so that branch stands in for `main` everywhere above; `main` gets the work by PR, merged by the user.

- `session-start.sh` records the branch as `claude.session-branch`, gives jj git's identity, makes what origin holds immutable, and defines `jj ship [-r REV]` (`scripts/ship.sh`: the gates, then the branch set to REV and pushed; jj starts a new `@` when REV was it). The local `main` bookmark is never moved.
- Step 3 is `jj ship`; subagent workspaces are based on the session branch, and a subagent's change is rebased onto it, not onto `main`.
- The container is reclaimed when the session idles: work not pushed is lost, so ship each issue as it lands.
- `npm run deploy` from a cloud session runs the session branch live, ahead of `main`.

## Live game

**The live server runs v2** (since 2026-10-17): `npm run deploy` builds,
smokes and uploads `dist/main.js`. Never deploy unasked. Live declarations (outposts,
allies, reserve) are set with `observe declare`.

`npm run observe -- health` first after a deploy; `npm run observe` with no
args lists the rest (engagements, rooms, creeps, regions, replay, kill
switches). `observe exchange` lands with its module.

v1 lives on the `v1` bookmark (`9677b5e8`, the code that ran live before v2),
a rollback only: deployed from a workspace on the bookmark, never changed
again. Nothing ports from v2 to v1. For v2 it is source to port from
(`git show v1:<path>`), never a fact about v2.
