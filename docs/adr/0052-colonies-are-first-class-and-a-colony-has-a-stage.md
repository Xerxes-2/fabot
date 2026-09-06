# Colonies are first-class, and a colony has a stage

Two days of live deploys of the second colony (W13S28, ADR 0047) broke one rule after another that had been written for the one RCL5 home the bot grew up in: pricing every row at the bank's capacity (#203), a posted source worth the room's rate rather than what the Anchor row digs at a 300 bank (#208), road and rampart sites at RCL1–2 in a room earning eight a tick (#209, #214), a trunk priced at a creep's swamp weight (#211), a standing body crossing a Seam for a pile (#206), the Post's Seat filled by the light crowd (#212), pioneers hired for the child and kept home by the tier they were pooled in (#213), and before all of these a second spawn casting into a colony-wide cascade and a Layout reading that spawn as home coordinates (#191). None was a defect in the rule's own terms; every one was a hidden constant of the single home — its level, its bank, its room — spelled as if it were a colony fact. The user's verdict (2026-09-06): "从一开始商量的时候我就不应该和你妥协，前提直接从多殖民地、多 remote 开始定".

We decided the premise is rewritten, not patched further. **The skeleton stands** — the layered Atlas and its flood tables (ADR 0041), a `decide` derived fresh every tick from facts, a pure Core under Expecto, the Verdicts and `observe.mjs` — and **the model under it is rewritten around two facts: a colony is the unit everything is computed for, and a colony has a stage.**

## Decision

**1. A colony is the unit.** The shell builds one `World` — every room seen this tick, once — and one `ColonyView` per living colony: that colony's home, the rooms it works, its own bank, controller, creeps and stock. Every function in Decide takes a `ColonyView` and nothing else. There is no `Snapshot.Controller` that is "the" controller, no `ColonyHomes` beside `Colony.declared`, no `richestCapacity` over rooms the colony does not own. What one colony may read of another is an explicit field of its view (a child's controller and sites, for the mother), never a narrowed layer.

**2. A position carries its room.** `RoomPos = { Room; X; Y }` at every boundary a tile crosses. Grids stay per room; the sets handed between functions do not drop the room. A Layout goal, a Work Area, a Seat, a Post, a trunk tile are room-joined by type, not by convention.

**3. A colony has a stage, and the rules read the stage.** `ColonyStage = Nursery | Bootstrapping | Independent`: claimed with no spawn of ours; a spawn standing and the controller under `bootstrapLevel`; at or past it. One derivation (`Colony.stageOf`), read wherever a rule today reads `isNurseryRoom`, `isBootstrapRoom`, `roadLevel`, `rampartLevel` or the pioneer addend. A rule that differs by stage says so in one place; a level number appears only inside `stageOf`.

**4. Every quota input is what this colony's row casts at this bank.** Never a nominal rate, never the bank's capacity, never a living body. `sourceOutputOf`, `castFromBank`, the hauler round trip, the upgrader drain, the reserver body, the worker floor: each priced off `bodyFor row view.Bank`, and each carrying a pairwise test at a 300 bank and an 1,800 one.

**5. Tunables are one record, parameterised by stage and bank.** `pioneerCount`, `rampartFloor`, `repairTrigger`, `pickupThreshold`, `outpostContainerBuilders`, `bootstrapLevel` and their kin move out of scattered module constants into `Tuning`, where each field states the stage and bank it was derived at and what it reads below them. A number without a pairwise test is not a tunable; it is a bug that has not happened yet.

**6. The pool carries its own priority and capacity; the Matcher is dumb.** `planTasks` emits each Task with its `Priority` and its `Capacity: BodyClass -> int`, computed by the colony that knows why. The Matcher matches, and recognises no Task kind. The exceptions that accreted on the old tier ladder — deadline rank, feeding sites, borrowed Upgrades, Post caps, light caps — become fields the planner sets.

**7. Cross-colony borrowing is an explicit, bounded exception on the view.** Today's one is the pioneers (ADR 0047 decision 4, #213). A mother's view names the child's Tasks it may take and the cap; nothing else of the child's reaches her pool.

**8. The shell boundary is testable.** `Snapshot.build` becomes a pure function of a captured, API-shaped input, run in tests on committed captures; Executor intents replay. The harness gains a `young` scenario (RCL1, 300 bank, two Posts, mini bodies) and a `pair` scenario (mother RCL5 with a bootstrapping child), with a CPU line per colony.

## Considered options

- **Keep patching by live incident.** Rejected by the user and by the count: eight tickets in two days, each a constant of the one home, each found by a creep standing in the wrong place at 3 a.m.
- **Rewrite everything, Atlas included.** Rejected: the Atlas, the per-tick derivation and the observability are what let the eight be found and fixed inside a day each; they are not where the debt is.
- **Stages as levels.** Keep reading `Level >= 3` in each rule. Rejected: four rules read the number today and a fifth would read it tomorrow; a stage is the fact, the level is how it is derived.

## Consequences

- Types.fs and the quota, pool and casting layers of Decide.fs are rewritten; Atlas.fs and Observe.fs change at their signatures (`RoomPos`, `ColonyView`) and not in their algorithms. Snapshot.fs and Main.fs are rewritten around `World`/`ColonyView`.
- Every ADR that reads "the spawn room", "the colony's controller" or "home" as a singular is re-read under decision 1; those already banner-amended (0011, 0020, 0024, 0034, 0040, 0042, 0046, 0047) keep their banners.
- The work is sequenced so the live colonies keep running: harness and captures first (8), then the view and stage (1, 3), then positions (2), then quotas and tunables (4, 5), then the pool and Matcher (6, 7). Each step ships green on the existing suite plus the pairwise tests it adds.
- CONTEXT.md gains `World`, `Colony view`, `Stage`, `Tuning`; `Nursery` and `Bootstrap window` become the first two stages.
