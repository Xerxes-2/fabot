# fabot — Domain Glossary

Screeps seasonal-server bot, written in F# and compiled to JS via Fable.

## Terms

### Snapshot
The immutable projection of the current tick's game state that the decision layer reads. Contains only the fields decisions need — never the full game object graph.

### Intent
A single described action the decision layer wants performed this tick (e.g. "creep X harvests source Y"). Intents are data; they do not touch the Screeps API.

### Executor
The thin imperative shell that turns Intents into Screeps API calls. The only layer allowed to act on the game (mutating API calls); Snapshot may call read-only game methods to build its projection.

### Task
A unit of work in the task pool (e.g. "deliver 300 energy to spawn"). Creeps are interchangeable executors that get matched to Tasks; a creep has no fixed role.

### Refill
The Task of delivering energy to any energy-hungry structure — spawn, extension, tower, or the controller [[container]]; the Planner filters by free capacity. One generalized Task, but rank layers by target (ADR 0010): spawn-feeding Refill is feeding-tier work, tower and controller-container Refill are surplus-tier — the colony feeds its own reproduction before its guns or its growth (ADR 0012).

### Withdraw
The Task of taking energy out of a stocked [[container]] (ADR 0012). Feeding-tier intake beside Harvest: an empty creep's choice between digging and collecting is made by [[travel cost]], not by rule. Applicability: a Carry part and free capacity. The intake half of the haul cycle — a filled hauler loses Withdraw applicability and rematches to [[refill]], the same emergent alternation as every other Task pair.

### Build
The Task of spending carried energy into a construction site. Surplus work: same rank tier as Upgrade, below Harvest and spawn-feeding [[refill]] — the economy is fed before anything is constructed.

### Repair
The Task of restoring a decaying structure's hits (ADR 0010) — created when a repairable structure falls below half hits, gone when it is whole. Surplus-tier like Build, same applicability (Work part, carried energy) and the same range. Only repairable kinds ([[trunk]] roads today, containers when they enter) put hits in the [[spatial projection]].

### Planner
The pure step that reads a Snapshot and generates this tick's full Task pool. Runs every tick from scratch — Tasks are never persisted.

### Matcher
The pure step that assigns creeps to Tasks (greedy matching): Assignments in, Assignments out. Current assignments are the only thing remembered between ticks (anti-thrash).

### Emitter
The pure step that turns the tick's assigned Tasks into each assigned creep's action Intent and [[chat bubble]]. Judges actions from tick-start geometry — it consults the same [[atlas]] as the Matcher and Resolver, never resolved positions.

### Seat
A walkable tile adjacent to a source. The capacity unit of Harvest: a source supports at most as many concurrent harvesters as it has Seats.

### Dual Seat
A [[seat]] that also lies inside the controller's Upgrade [[work area]]. A creep standing on one can alternate Harvest and Upgrade without ever moving. One of the two kinds of [[post]]. Derived by the [[atlas]] each tick, never persisted.

### Post
A tile worth garrisoning with a heavy-WORK body (ADR 0012): a [[dual seat]], or the [[seat]] under a source [[container]]. The capacity unit of the [[anchor]] quota — one Anchor per Post. Derived by the [[atlas]] each tick, never persisted.

### Container
A container structure as the [[layout]] places it (ADR 0012): one **source container** per source, on the [[seat]] nearest that source's [[trunk]]; one **controller container** on a buildable work-area tile adjacent to a trunk — the upgrade buffer that lets upgraders work standing still. A repairable kind: its hits enter the [[spatial projection]] and the Repair pool (ADR 0010). A stocked container is a [[withdraw]] target; the controller container is also a surplus-tier [[refill]] target.

### Work Area
The set of tiles a creep may stand on while performing its current Task, derived from the Task's target position and the action's range. Derived fresh each tick, never persisted.

### Move Intent
A creep's movement desire for one tick: candidate standing tiles plus a priority. Input to the [[resolver]] — not an Intent; the Resolver's output (a single-step move) is what becomes an Intent.

### Resolver
The pure step that arbitrates a room's Move Intents into actual single-step moves (priority first, most-constrained first, swap when contested). Fourth pure step beside Planner, Matcher, and [[emitter]]; movement is never issued outside it. A [[grounded]] creep sits arbitration out: its tile is blocked for the tick and no move Intent is issued to it (ADR 0008).

### Grounded
A creep still paying off fatigue this tick: the engine would answer any move with ERR_TIRED, so the [[resolver]] neither asks it to move nor lets anyone claim or displace through its tile (ADR 0008). Recomputed each tick from the Snapshot's fatigue — a stationary creep drains 2 fatigue a tick, so grounding is always transient.

### Travel cost
The cheapest-path cost from a creep to a Task's Work Area over the [[spatial projection]], for that creep's body: terrain weights (road 1, plain 2, swamp 10 — the engine's own fatigue costs; impassable excluded) scaled by the body's fatigue factor (ADR 0002, revised by ADRs 0006 and 0010), plus the [[occupancy surcharge]] on tiles under standing creeps (ADR 0008). Priced from the load carried right now: carried energy loads Carry parts 50 apiece, and an empty Carry generates no fatigue — the engine's own rule. Breaks rank ties in the Matcher; a Work Area with no travel cost — unreachable or empty — makes the Task inapplicable to that creep: never matched fresh, and a remembered assignment to it is released.

### Occupancy surcharge
The extra cost (10, one swamp step — ADR 0008, re-expressed by ADR 0010) the flood prices onto a step landing on a tile some creep occupies this tick. Sends travellers around standing traffic when a lane is cheaper, but the tile stays passable — traffic re-prices a route, it never makes a Task inapplicable, unlike an obstacle.

### Workforce target
The number of creeps the colony maintains, derived fresh each tick from the Snapshot, never persisted. Three addends, each a pattern row's own quota rule (ADR 0012): Anchors — one per [[post]]; haulers — the throughput arithmetic per source [[container]]; workers — unallocated income divided by one worker body's Work drain, so exactly as many upgrade mouths as the surplus feeds. Floored at 2. A source whose Post is provided for retires its other [[seat]]s — the seat count stopped being the base the moment one heavy body could drain a source alone. Spawning fills the gap between living creeps and the target.

### Room energy
One room's shared spawn-energy account (spawn + extensions) — a colony fact, not spawn state. Spawn planning allocates bodies from it in spawn order, debiting as it goes, so the same energy is never committed twice; a spawn whose room banks nothing waits.

### Spatial projection
The Snapshot's map-shaped view of the spawn room — the only one (ADR 0005): the room's name, three-state terrain (plain / swamp / wall), entity positions, what kind of thing each target is (source, controller, a structure or a site of some built kind), which tiles hold a built road (ADR 0010), and current/max hits on repairable kinds only — fields nobody decides on stay out. Raw data, always present (possibly empty); decisions consult it only through the [[atlas]]. A tile absent from the projection is impassable — absence is per-entry, never per-projection (ADR 0004).

### Atlas
The per-tick, task-aware query interface over the [[spatial projection]]: Seats, Work Areas, travel costs, first steps, action permission, standing candidates, placed creeps — and the placement queries the [[layout]] derives from — room name, target positions, buildable tiles, structure and road censuses, raw-terrain [[trunk]] paths (ADR 0005, ADR 0011). Total (ADR 0004): geometry the projection cannot place gets one documented answer per query — it never counts against a [[task]] and never blocks an action. Built fresh each tick; Matcher and Resolver consult the same one.

### Anchor
The heavy-WORK [[body pattern]] cast for a [[post]]: many Work, one Carry, minimal Move. Its slowness is the point — body-aware [[travel cost]] pins it to the nearest high-value tile, where it works in place: alternating Harvest and Upgrade on a [[dual seat]], draining the source into the container under it on a source-[[container]] Post (ADR 0006, generalized by ADR 0012). The one Carry is kept on both footings so one sizing rule serves the whole row. Exempt from [[fatigue parity]], which governs only the [[worker unit]] pattern. One Anchor per Post, inside the [[workforce target]].

### Hauler unit
The repeating [Carry; Carry; Move] block hauler bodies are built from (ADR 0012): 150 energy, full speed loaded *on roads* — the row carries its own parity declaration (road parity, not the plain parity of [[fatigue parity]]), because a hauler's whole life is the [[trunk]]. No Work part, so Harvest, Build, Upgrade and Repair are inapplicable by body; it lives in the [[withdraw]]→[[refill]] cycle. Quota: per source [[container]], round-trip ticks to the spawn (the canonical sink the trunks radiate from) times source output over carry capacity, rounded up.

### Body pattern
The repeating part block a body is generated from; capacity buys as many whole repeats as it can. The [[worker unit]] is one pattern. Which pattern a spawn casts is a colony decision; a pattern shapes what a creep is good at, never what it is assigned — creeps stay interchangeable and matching stays Task-based.

### Worker unit
The repeating [Work; Carry; Move] block worker bodies are built from — the generalist [[body pattern]]: 200 energy, full speed empty, half speed loaded. Capacity buys as many whole units as it can; what's left is remainder.

### Fatigue parity
The body-generation invariant (ADR 0003): a worker body padded beyond whole [[worker unit]]s never moves slower than the pure-unit body, empty or loaded. The remainder buys as much Carry as parity allows, then Move — never Work.

### Layout
The deterministic full structure plan (ADR 0011): every clustered structure (tower first, then extensions) picked by one ordering rule — buildable tiles on the spawn's checkerboard colour, nearest-to-spawn first — plus the [[trunk]] roads, computed whole up to the RCL4 horizon every tick from the [[atlas]], never persisted. Placement filters the Layout to what the current RCL unlocks and what is missing; the tick a level lands, its sites drop.

### Trunk
A paved line in the [[layout]]: each source to the controller, each source to the spawn, plus the swamp tiles inside the controller's Upgrade [[work area]]. Priced on raw terrain only and routed around the Layout's reserved tiles. The rule is general — short trunks still pave; room-specific exemptions don't get encoded (ADR 0011).

### Verdict
The reasoned outcome a decision step returns beside its decision — data, never a log line (ADR 0009). The [[matcher]]'s Verdict on a creep says which [[task]] won it and why (rank, [[travel cost]], load, tie-break), that a remembered assignment was kept (anti-thrash, distinct from a fresh match), why an assignment was released, or why an unassigned creep got nothing; the [[resolver]]'s says what became of its movement ([[grounded]], yielded, reroute). Conclusion-level always; a creep on the verbose list gets its full candidate scoring too. The Planner and spawn decisions return no Verdicts.

### Verbose list
The creep names owed full candidate scoring, stored beside the [[transition log]] under `Memory.fabot.observe` and read fresh each tick — flipped from the terminal through the Memory HTTP API, so an investigation needs no redeploy. Empty (or absent, or malformed) means off; each listed creep's Scoring [[verdict]] carries one Candidate per pooled [[task]]: scored on the full matching key, or rejected at the first gate it failed (inapplicable, capacity-full, unreachable).

### Transition log
The per-creep ring of recent changes — task handovers and movement events, each with its [[verdict]] and tick — written only on the tick something changed. The colony's answer to "why did this creep flip", capped per creep; a quiet creep writes nothing.

### Chat bubble
The glyph an assigned creep says over its head each tick, one fixed glyph per [[task]] (⛏ Harvest · 📥 Withdraw · 🔋 Refill · 🔨 Build · 🔧 Repair · ⚡ Upgrade). Observability only, private to our own viewer; unassigned creeps show nothing.

### Safe-mode reflex
The colony reflex (ADR 0007) that emits `ActivateSafeMode` the tick any CLAIM-part [[hostile]] stands in a spawn room — on sight, because the claim tap it is about to land would itself block activation for 1,000 ticks. Gated only on stock remaining and safe mode not already running; hostiles without CLAIM never spend the stock.

### Pickup reflex
The colony reflex that emits a pickup Intent for every creep with free carry capacity standing within range 1 of a dropped energy pile — beside its assigned [[task]]'s action, since the engine's pickup conflicts with no other action. A reflex, not a Task: no movement, no matching, no threshold — it only recaptures what is already in reach (death drops, harvest overflow). Energy only; tombstones are not covered. Piles are projected as position and kind alone — no amount, since no decision reads one. Like the [[safe-mode reflex]], it speaks no [[verdict]] and shows no [[chat bubble]].

### Hostile
A hostile creep as the Snapshot projects it: its body parts, verbatim, and nothing else. What a hostile can do is decided from what it is made of — CLAIM is the only part that threatens the controller, and the controller is the only thing safe mode is spent on (ADR 0007).

### Downgrade deadline
The hard floor on the controller's downgrade timer: half the level's full timer (ADR 0007). Inside it, Upgrade stops being surplus work and outranks even the feeding tier — a downgrade costs a level and zeroes the safe-mode stock, and the engine refuses safe-mode activation below half minus 5,000, so escalating at half keeps the [[safe-mode reflex]] fireable with the engine's whole grace intact.

### Disaster fallback
The zero-creep spawning rule: an empty colony spawns bare [[worker unit]]s from whatever [[room energy]] is banked right now — as many as the bank affords, up to the workforce deficit — rather than waiting for a full capacity it can never refill. The one body-generation path that ignores the remainder — time-to-first-creep outranks spending the bank.

## Avoided terms

- **Role** — creeps are not born with roles; work is Task-based. Don't reintroduce role-based vocabulary.
