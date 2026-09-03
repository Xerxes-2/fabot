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
The Task of delivering energy to any structure that feeds spawning (spawn or extension). One generalized Task — spawn vs extension is not a domain distinction; the Planner filters by free capacity.

### Build
The Task of spending carried energy into a construction site. Surplus work: same rank tier as Upgrade, below Harvest/Refill — the economy is fed before anything is constructed.

### Planner
The pure step that reads a Snapshot and generates this tick's full Task pool. Runs every tick from scratch — Tasks are never persisted.

### Matcher
The pure step that assigns creeps to Tasks (greedy matching): Assignments in, Assignments out. Current assignments are the only thing remembered between ticks (anti-thrash).

### Emitter
The pure step that turns the tick's assigned Tasks into each assigned creep's action Intent and [[chat bubble]]. Judges actions from tick-start geometry — it consults the same [[atlas]] as the Matcher and Resolver, never resolved positions.

### Seat
A walkable tile adjacent to a source. The capacity unit of Harvest: a source supports at most as many concurrent harvesters as it has Seats.

### Dual Seat
A [[seat]] that also lies inside the controller's Upgrade [[work area]]. A creep standing on one can alternate Harvest and Upgrade without ever moving. Derived by the [[atlas]] each tick, never persisted.

### Work Area
The set of tiles a creep may stand on while performing its current Task, derived from the Task's target position and the action's range. Derived fresh each tick, never persisted.

### Move Intent
A creep's movement desire for one tick: candidate standing tiles plus a priority. Input to the [[resolver]] — not an Intent; the Resolver's output (a single-step move) is what becomes an Intent.

### Resolver
The pure step that arbitrates a room's Move Intents into actual single-step moves (priority first, most-constrained first, swap when contested). Fourth pure step beside Planner, Matcher, and [[emitter]]; movement is never issued outside it.

### Travel cost
The cheapest-path cost from a creep to a Task's Work Area over the [[spatial projection]], in ticks for that creep's body: terrain weights (plain 1, swamp 5, impassable excluded) scaled by the body's fatigue factor (ADR 0002, revised by ADR 0006). Priced from the load carried right now: carried energy loads Carry parts 50 apiece, and an empty Carry generates no fatigue — the engine's own rule. Breaks rank ties in the Matcher; a Work Area with no travel cost — unreachable or empty — makes the Task inapplicable to that creep: never matched fresh, and a remembered assignment to it is released.

### Workforce target
The number of creeps the colony maintains: the total [[seat]] count across all sources, floored at 2. Derived fresh each tick from the Snapshot, never persisted. Spawning fills the gap between living creeps and the target; a source the projection does not place contributes no Seats, so an empty projection leaves only the floor. Seats count by terrain alone (ADR 0001), so an unreachable source still raises the target — the surplus flows to Upgrade.

### Room energy
One room's shared spawn-energy account (spawn + extensions) — a colony fact, not spawn state. Spawn planning allocates bodies from it in spawn order, debiting as it goes, so the same energy is never committed twice; a spawn whose room banks nothing waits.

### Spatial projection
The Snapshot's map-shaped view of the spawn room — the only one (ADR 0005): the room's name, three-state terrain (plain / swamp / wall), entity positions, and what kind of thing each target is (source, controller, a structure or a site of some built kind). Raw data, always present (possibly empty); decisions consult it only through the [[atlas]]. A tile absent from the projection is impassable — absence is per-entry, never per-projection (ADR 0004).

### Atlas
The per-tick, task-aware query interface over the [[spatial projection]]: Seats, Work Areas, travel costs, first steps, action permission, standing candidates, placed creeps — and the placement queries construction planning derives from (room name, target positions, buildable tiles, extension censuses; ADR 0005). Total (ADR 0004): geometry the projection cannot place gets one documented answer per query — it never counts against a [[task]] and never blocks an action. Built fresh each tick; Matcher and Resolver consult the same one.

### Anchor
The heavy-WORK [[body pattern]] cast for a [[dual seat]]: many Work, one Carry, minimal Move. Its slowness is the point — body-aware [[travel cost]] pins it to the nearest high-value tile, where it harvests and upgrades in place. Exempt from [[fatigue parity]], which governs only the [[worker unit]] pattern (ADR 0006). One Anchor per Dual Seat is part of the [[workforce target]], not on top of it.

### Body pattern
The repeating part block a body is generated from; capacity buys as many whole repeats as it can. The [[worker unit]] is one pattern. Which pattern a spawn casts is a colony decision; a pattern shapes what a creep is good at, never what it is assigned — creeps stay interchangeable and matching stays Task-based.

### Worker unit
The repeating [Work; Carry; Move] block worker bodies are built from — the generalist [[body pattern]]: 200 energy, full speed empty, half speed loaded. Capacity buys as many whole units as it can; what's left is remainder.

### Fatigue parity
The body-generation invariant (ADR 0003): a worker body padded beyond whole [[worker unit]]s never moves slower than the pure-unit body, empty or loaded. The remainder buys as much Carry as parity allows, then Move — never Work.

### Chat bubble
The glyph an assigned creep says over its head each tick, one fixed glyph per [[task]] (⛏ Harvest · 🔋 Refill · 🔨 Build · ⚡ Upgrade). Observability only, private to our own viewer; unassigned creeps show nothing.

### Safe-mode reflex
The colony reflex (ADR 0007) that emits `ActivateSafeMode` the tick any CLAIM-part [[hostile]] stands in a spawn room — on sight, because the claim tap it is about to land would itself block activation for 1,000 ticks. Gated only on stock remaining and safe mode not already running; hostiles without CLAIM never spend the stock.

### Hostile
A hostile creep as the Snapshot projects it: its body parts, verbatim, and nothing else. What a hostile can do is decided from what it is made of — CLAIM is the only part that threatens the controller, and the controller is the only thing safe mode is spent on (ADR 0007).

### Downgrade deadline
The hard floor on the controller's downgrade timer: half the level's full timer (ADR 0007). Inside it, Upgrade stops being surplus work and outranks even the feeding tier — a downgrade costs a level and zeroes the safe-mode stock, and the engine refuses safe-mode activation below half minus 5,000, so escalating at half keeps the [[safe-mode reflex]] fireable with the engine's whole grace intact.

### Disaster fallback
The zero-creep spawning rule: an empty colony spawns bare [[worker unit]]s from whatever [[room energy]] is banked right now — as many as the bank affords, up to the workforce deficit — rather than waiting for a full capacity it can never refill. The one body-generation path that ignores the remainder — time-to-first-creep outranks spending the bank.

## Avoided terms

- **Role** — creeps are not born with roles; work is Task-based. Don't reintroduce role-based vocabulary.
