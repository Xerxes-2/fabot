# A Verdict is returned, never logged; Memory is the way out

Every diagnosis so far has been done blind. The bot's whole observable
surface is two failure lines in the web console and a glyph over each
creep's head; ADR 0008 opens with a live jam diagnosed by squinting at
console spam. A coding agent's feedback loop today is `dotnet test`
against hand-written Snapshots, then deploy and hope — nothing answers
"why did this creep pick that action" from the real game.

We decided the shape of attribution end to end:

1. **A Verdict is data the Core returns.** The Matcher and Resolver
   return Verdicts beside their decisions — which Task won a creep and
   why, what became of its movement — as plain values in the decision
   result. Core stays pure: no logging, no side channel. Conclusion-level
   Verdicts are always produced; full candidate scoring is computed only
   for creeps on a verbose list the shell passes in.
2. **The shell ships Verdicts out through Memory.** The App layer folds
   each tick's Verdicts into a per-creep transition log under
   `Memory.fabot.observe` — one timeline per creep, task handovers and
   movement events interleaved, written only on change and capped per
   creep. The verbose list lives beside it, so an agent can flip it
   remotely through the same Memory API.
3. **A one-shot CLI pulls it down.** `scripts/observe.mjs` (on the
   `screeps-api` client) reads the log and the toggle over the HTTP API,
   and can subscribe to the console for a bounded window — the only
   programmatic exit the two failure lines have, since the console keeps
   no history. Intent failures stay on the console; attribution never
   goes there.

## Considered Options

- **Log from inside Core.** Rejected: Core has no side effects — that
  purity is what makes the decision layer testable from hand-built
  Snapshots, and attribution must not be the first crack in it.
- **Console as the attribution channel.** Rejected: throttled, lossy,
  unstructured, and history-free — a one-shot CLI can never ask it what
  happened last tick, which is the whole question.
- **RawMemory segment instead of a Memory path.** Deferred: the
  transition log is small (change-driven, per-creep cap) and Memory
  bindings already exist. If verbose scoring outgrows the main Memory,
  only the App writer and the CLI reader move — Core never knows.
- **Full per-tick history instead of a transition log.** Rejected: an
  unchanged tick carries no information, and N ticks × every creep
  crowds the 2MB Memory for records nobody will read. Flip-flops — the
  question history exists to answer — are by definition dense in a
  transition log.
- **Trace every decision step (Planner, spawning too).** Rejected for
  now: both real incidents to date were Matcher (flip-flop) or Resolver
  (jam) shaped; Planner and spawn decisions fire rarely and reconstruct
  easily from outcomes.

## Consequences

- `decide`'s result shape changes to carry Verdicts, and a verbosity
  input rides in with the Snapshot or beside it — a signature ripple
  through Core and every test that drives it.
- The agent's loop gains a live leg: deploy, act, then
  `observe.mjs` instead of eyeballing the client; the ERR_TIRED-style
  diagnosis of ADR 0008 becomes a query, not an archaeology session.
- Memory now carries telemetry as well as state; the observe subtree is
  disposable by construction (the bot must boot with it absent).
- `screeps-api` becomes a devDependency; the upload script stays as-is.
- Executor outcomes (`(Intent * Outcome) list`, today discarded at
  `Main.fs:52`) are not yet folded into the log — the seam noted in
  Executor.fs stays open for a future revision.
