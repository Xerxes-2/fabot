# Orchestration

How to work a queue of issues when the main model orchestrates and subagents build. One
issue in the window you already have is just `/implement`; this is for the queue.

## The queue

Fetch it per `issue-tracker.md`, with the label `triage-labels.md` owns. Take the delivery
order from GitHub's native dependencies, and treat it as a proposal until recon has checked
it against the code.

## Recon first

Before any implementer starts, send read-only agents to map the code: one per **cluster** —
the issues whose changes land in the same module or cite the same ADR — plus one over the
whole queue. Two or three cluster agents and the queue-wide one is the shape; past that,
recon costs more than the queue. A lone issue joins its nearest cluster.

Each map is written **outside the repo**, names real symbols, and carries line numbers as a
locator only: a sibling issue landing moves them, so the symbol is the identity.

The queue-wide agent checks the delivery order against the code rather than the issues'
stated blockers, and reports which pairs touch the same functions.

Recon earns its cost on the runs where it contradicts a ticket. The code and the ADRs are
the **authority**.

## Parallel where the functions don't overlap

Issues whose changes touch different functions run at once, each implementer in its own
hook-made workspace (`AGENTS.md` § Working alongside); issues that touch the same functions
run one after the other. Recon's queue-wide map says which pairs overlap; when in doubt,
serialise. A shared test file is not an overlap — both sides adding tests merges cleanly.

Every change ships alone: rebase it onto the current `main`, rerun the gates, ship, deploy,
then the next. A change that conflicts on rebase goes back to its implementer, never
resolved by hand in the default workspace.

Parallelise *inside* an issue too — the review lenses run at once.

## Where a new Decide test goes

`tests/Core.Tests/Decide/` is split **by domain, never by issue**: an issue's tests are
scattered across the files its behaviour belongs to, and no file is ever named after a
ticket. Pick the domain first from the table below, then the file within it.

| Domain | Files |
|---|---|
| shared surface | `Fixtures.fs` — snapshot builders and Decision readers used by **more than one** domain |
| the Layout — the planner's clustered ordering, the trunks, the storage, the Link footings, and the plan memo the census signature keys | `LayoutFixtures.fs`, then `LayoutPlacementTests.fs` (built kinds, planner, placement), `LayoutPlanTests.fs` (layout, storage, link footing, unrouted trunk), `LayoutMemoTests.fs` (census signature, plan memo), `LayoutRoomTests.fs` (room layer, the square ring) |
| the price — what a tick of delay costs, the value scale every rank is read off and the losses a value is made of (`Price`), as pure functions; how the pool reads them is the pool's | `PriceTests.fs` |
| the pool — which Tasks a colony offers and what caps them | `PoolFixtures.fs`, then `PoolSeatTests.fs` (seats, refill cluster, unreachable targets, repair, priced sinks), `PoolPickupTests.fs` (pickup reflex, logistics), `PoolStorageTests.fs` (storage stock and draw), `PoolPostTests.fs` (container Posts, post capacity, restock dispatch), `PoolWithdrawTests.fs` (withdraw capacity, piles and tombstones) |
| the Matcher and the Resolver — applicability, travel cost, yield arbitration, Verdicts, verbose scoring, and the Intents a decision emits | `MatcherFixtures.fs`, then `MatcherApplicabilityTests.fs`, `MatcherTravelTests.fs` (travel cost, movement, arbitration), `MatcherVerdictTests.fs` (Verdicts, chat bubbles, rank tiers, verbose scoring), `MatcherDecideTests.fs` (the `decide` list — the end-to-end cases whose frozen names keep them here — and the intake lines) |
| the rows — the cast rows, the workforce target and source output behind them, the body patterns, and the Tuning knobs | `QuotaFixtures.fs`, then `QuotaBodyTests.fs`, `QuotaWorkforceTests.fs`, `QuotaSourceTests.fs`, `QuotaReserverTests.fs`, `QuotaGuardTests.fs`, `CasterTests.fs` (which colony casts each fight ask), `QuotaMinerTests.fs`, `QuotaUpgraderTests.fs`, `QuotaTuningTests.fs` |
| the colony as a unit — stage, Claim, nursery, two colonies side by side, borrowed-room budgets | `ColonyFixtures.fs`, then `ColonyTests.fs` (the Claim, the nursery), `ColonyPairTests.fs` (the bootstrap window, two colonies side by side, energy aid), `ColonyDefenceTests.fs` (another colony's home defended, a raised home's garrison), `ColonyStageTests.fs` (the colony stage, borrowed-room budgets, a raised child's rock and its unbuffered ferry) |
| the fight — a [[squad]] holding a resident room's `Fight`: when it is pooled and how long it is held, the rally ground and the launch, which bodies it admits and what the residents do meanwhile, a member's role, and what the squad shoots. The squad rows' casts and census are the rows'; the fight record's fold is `ObserveRaidTests`'; a whole fight on captured terrain is the arena's (its row below). A squad holding an `Assault` of a rival's room (its pool, rally, launch, targets and retreat) is here too; what the view carries of the assault room is `ViewDeclaredRoomTests`' | `FightTests.fs` |
| the fighter — a body's `Fighter` read off its parts (reach, damage and heal by range, gait, hits, boosts), the weapon reflex every armed body strikes by beside its Task, and the strike it picks. Which rows cast the body is the rows'; a whole fight is the arena's | `FighterTests.fs` |
| what the colony says and does off the pipeline — the [[signature reflex]] and the other colony reflexes | `SignatureTests.fs` |
| the errand — a room a colony declares for one named object in it, and the work that declaration buys: the `Reclaim` Task, who may hold it and the act it fires, plus the courier's paired Storage→Reactor delivery whose far endpoint only that declaration supplies (ADR 0060 decision 3, ADR 0067; the declaration kind itself is decision 1's). The errand's *projection* half — what a colony may carry of that room at all — is `ViewDeclaredRoomTests`', not this | `ErrandTests.fs` |
| the salvage room — a room a colony declares to take down what still stands there: the `Dismantle` Task, who may hold it, the act it fires, and the dismantler row (ADR 0079). Which structures are targets and what the colony may carry of the room is `ViewDeclaredRoomTests`' | `SalvageTests.fs` |
| the harassment room and the sweep — a globally declared enemy remote, a candidate of the one sweep: which room the sweep works (#658), the ranger's Guard there, the enemy's unarmed creeps as targets, the ranger's ground, the room's contest and its cast (#656), and the Dismantle of the enemy's containers (ADR 0093). Which colony runs the sweep, what it may carry of a candidate and its memory of a dark one are `ViewHarassTests`'; its stand-down is `ObserveRaidTests`' | `HarassTests.fs` |
| the operation — a fight we start written as data (#663 M7): a spec's guards, phases and exits, its record's latch, the interpreter's step, the decoder that admits or refuses a Memory spec, and the sweep's shadow's comparison and log (#663 S7b; a shadow played out over a fight is the arena's). Its Memory wire is `scripts/wire-check.mjs`' | `OperationTests.fs` |
| the lab cluster — the reaction plan (which compound next, and when it stops), the parts its labs play, the fills and drains it pools, the reaction it runs, and the ore other colonies' terminals ship it (ADR 0089 S3). Where its stamp is placed is the Layout's (`LayoutPlacementTests`) | `LabTests.fs` |
| the outposts — rooms the colony mines but does not own | `OutpostFixtures.fs`, then `OutpostBorderTests.fs` (invader core, decide across a border), `OutpostDeclarationTests.fs`, `OutpostAnchorTests.fs`, `OutpostHaulTests.fs` (container, hauler quota, lead), `OutpostRaidTests.fs` (an armed hostile, the economy's entry, the container switch, the raid's memory, the exit it left by), `OutpostGuardTests.fs` (the Guard, an idle or blind guard, the Task across the Seam, the Guard's flavours), `OutpostKiteTests.fs` (the kite's seats and step, the chase call, the outpost shut on a kited raid), `OutpostTrespassTests.fs` (a player's unarmed body, the resident lent to a chase), `OutpostChildTests.fs` (a raid in a raised child's outpost), `OutpostEngagementTests.fs` (the kill's admission, the engagement, the pursuit, a contested outpost), `OutpostReserveTests.fs` (reservation, stand-down) |
| what the colony sees and flees — the towers, the ramparts over the keep, the spawn hold, safe mode, the downgrade deadline | `ThreatFixtures.fs`, then `ThreatTests.fs` (Threats by room and across the gate, the flee, keepers, a raid's parts, the ranger's ground, the observers), `ThreatTowerTests.fs` (ramparts, towers firing, healing and repairing, rampart doors, a fight-only decide's structures), `ThreatSafeModeTests.fs` (safe mode, the downgrade deadline, the spawn hold), `ThreatDefenderTests.fs` (the rampart defender, a Threat across the border) |
| the Anchor — the heavy body pinned to its rock. Its own rules live here whatever room it stands in; a case whose subject is the outpost is the outpost's | `AnchorFixtures.fs`, then `AnchorPinTests.fs` (anchor, heavy pin), `AnchorPostTests.fs` (occupancy, what a garrison digs, the Work ceiling), `AnchorArrivalTests.fs` (expiring creeps, capacity at arrival), `AnchorStandingTests.fs` (the standing body, its Refill, the Post it raises) |
| the colony view — the `World` built by hand and the `ColonyView` each colony is cut from it (`tests/Core.Tests/`) | `ViewFixtures.fs` (the pair world), then `ViewTests.fs` (room positions, the World, the colony view, terrain, exit holds, the world view), `ViewDefenceTests.fs` (a defended home, the garrison, a child's outposts, the Seam, safe-mode facts), `ViewReachTests.fs` (transit, declarations refused, the scan set over the masked layer), `ViewDeclaredRoomTests.fs` (the errand, the ally, the salvage and assault rooms, a rival's safe mode), `ViewHarassTests.fs` (the harassment room and its floor) |
| the light tick — what a quiet tick replays of the last full one, what forces a full tick instead, and which colonies a forced tick decides (`tests/Core.Tests/`) | `LightTickFixtures.fs` (the quiet world and the full tick that saw it), then `LightTickTests.fs` (the replay, the cadence), `LightTickForcedTests.fs` (every forcing fact, a raided home), `LightTickColonyTests.fs` (who can hurt us, the reset split, the deciders, a fight decided alone) |
| the arena — a whole fight played on its physics or on captured terrain (`tests/Core.Tests/`, beside `Arena.fs`). A capture another file replays sits in a captures file as `internal` | `ArenaFixtures.fs`, the captures `ArenaOutpostCaptures.fs` (raids on our outposts) and `ArenaMovementCaptures.fs` (our bodies moving round hostiles), then `ArenaPhysicsTests.fs` (bodies, NPC invaders, structures), `ArenaScenarioTests.fs` (the siege, the replay, the storage flip, the W11S28 invasion), `ArenaBorderTests.fs` (crossings, an outpost's stand-down, traffic at an exit), `ArenaRangerTests.fs` (the ranger's reflex, its line, the row's pair), `ArenaDefenceTests.fs` (perimeter, towers, safe mode, the economy's arms), `ArenaSiegeTests.fs` (W17S29 under an NPC siege), `ArenaLaneTests.fs` (the world lane, lane reach, the observer's vision), `ArenaStandDownTests.fs` (the w18s25-crippled capture; its rooms in `ArenaStandDownRooms.fs` and `ArenaStandDownOuterRooms.fs`), `ArenaChaseTests.fs` (bodies on the move under fire), and the older `ArenaClaimTests.fs`, `ArenaReactorTests.fs`, `ArenaStrongholdTests.fs` |

Fixtures sit in three tiers, and a fixture starts at the narrowest one that holds it:

- `let private` **in the test file that needs it**, where it starts;
- `<Domain>Fixtures.fs`, `internal`, the tick a second file of the same domain wants it;
- `Fixtures.fs`, the tick a second **domain** wants it — the shared surface, so everything
  in it is a name every suite must keep working.

A fixture that carries an **Atlas** is a `let private` **function** at whichever tier it
sits, and it captures no Atlas of its own: a binding that is a value compiles to a static
initialised once per process, Expecto's lists run in parallel, and an Atlas memoises onto
mutable `Dictionary` tables, so one shared between two lists is two threads writing one
table (#310, `AGENTS.md` § Code hygiene). `ParallelSafetyTests` fails the build on one.

Each suite keeps its own `[<Tests>] let …Tests = testList "…"` entries. There is no wrapping
`testList` over the directory — a wrapper would prefix every test name, and the names are
the contract `dotnet test --list-tests` is diffed on. Splitting a file therefore never
changes a test name: the `testList` blocks move whole.

No test file passes ~2,500 lines. Past that the file is what makes it expensive to edit, and
it splits again along the same rule — by domain, with the helpers only it uses following it.
`AtlasTests.fs` sat outside this directory and over the old 6,000-line line; it is now
`AtlasFixtures.fs` beside eight `Atlas…Tests.fs` files, split by the query it exercises.

ADRs written before #229 name `DecideTests` for what is now `tests/Core.Tests/Decide/`; they
are dated records and stay as written, so follow the test *name* they quote, not the file.

## The per-issue loop

1. **Implement.** One agent, test-first, until `npm run format`, `npm run build` and
   `npm test` (format check, ADR gate, `dotnet test`, wire gate) are clean.
2. **Review**, all at once over the same diff. `/code-review` carries the spec and standards
   lenses and satisfies the pre-push condition in `AGENTS.md`. Beside it run a third agent
   for the **adversarial** lens: break it, with a concrete failure scenario — inputs in,
   wrong output out — or drop the finding. Give each agent the issue body, the ADRs it
   cites, the diff against the last pushed change, and one instruction: report, never edit.
3. **Resolve.** One agent judges each finding on the merits and **rejects** what is wrong,
   out of scope, or would make the code worse, saying why.

Step 3 earns the most. Reviewers are confidently wrong often enough that applying every
finding is worse than judging them.

## Briefing an implementer

The brief overrides `/implement` on two points, so say both: the implementer does not
commit, and does not run its own review. `/implement` ends by doing both.

Name in advance every existing test the change may edit, and why. Any *other* red test is
then the alarm that the change is wrong, rather than a test to update. Carry that list
**forward**: when a refactor will shift values a later issue must update, write it into that
issue before anyone starts it, so the edit arrives expected instead of alarming.

Verify the recon map against the code before building on it.

One gotcha no config confesses: the matcher scores a candidate against its **cheapest**
rival alone, so a pool holding three rivals proves nothing about the two that lost. Pin tier
relationships pairwise, one rival at a time.

## The orchestrator keeps the point of no return

Subagents leave the work uncommitted. The orchestrator re-runs the gates itself, ships per
`AGENTS.md` § Shipping an issue, then deploys and verifies.

Deploy the halves of a split feature together when one half alone would harm the colony — a
[[storage]] that can only be filled is a sink, so #69 waited for #70.

## Verifying against the live room

A deploy is not verified by looking at the room. Build a harness that drives the **deployed**
bundle against the room's real terrain and objects, and validate the harness before trusting
it: replay the *previous* bundle against the reconstructed prior world and check it
reproduces what the server actually did.

Prove the binary's provenance rather than assuming it — rebuild the commit clean and compare
hashes against the server's code. For anything the bot never emits, such as a reservation or
a [[link footing]], a second derivation written from the rule and loading no bundle is the
only cross-check there is.

Record the comparison on the spec issue, criterion by criterion, and write the divergences
down rather than fixing them silently.

## ADRs during the run

A ticket instruction that turns out not to be **buildable** — not merely disagreeable — gets
a new ADR recording the resolution and every place it overrides its parent (ADR 0027).

A *pending* ADR is amended by the commit that implements it, when building resolves
something the ADR left open (ADR 0025's release Verdicts).

An *accepted* ADR is never rewritten, even when the run proves its stated reason false.
Correct the code comment that repeated it, add the test that pins the real invariant, and
file the successor (#82).

## Findings outlive the issue

A real defect the issue never asked about becomes its own issue, and the implementer reports
it rather than widening its diff. An adversarial finding is reproduced on the tree before it
is filed, and the trace goes in the issue.

Each opens with a one-line italic blockquote naming what found it, during which issue, and
why it was deferred — that line is what makes a night's work reconstructible afterwards. It
lands `needs-triage`: a run does not get to enqueue its own next job.

A ticket that contradicts the authority earns a correction comment quoting the offending
line, citing the documents that agree with the code, and saying what obeying it would have
cost. Implement the authority; record the divergence.
