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
The pure step that assigns creeps to Tasks (greedy matching) and emits Intents. Current assignments are the only thing remembered between ticks (anti-thrash).

### Seat
A walkable tile adjacent to a source. The capacity unit of Harvest: a source supports at most as many concurrent harvesters as it has Seats.

### Work Area
The set of tiles a creep may stand on while performing its current Task, derived from the Task's target position and the action's range. Derived fresh each tick, never persisted.

### Move Intent
A creep's movement desire for one tick: candidate standing tiles plus a priority. Input to the [[resolver]] — not an Intent; the Resolver's output (a single-step move) is what becomes an Intent.

### Resolver
The pure step that arbitrates a room's Move Intents into actual single-step moves (priority first, most-constrained first, swap when contested). Third pure step beside Planner and Matcher; movement is never issued outside it.

### Travel cost
The cheapest-path cost from a creep to a Task's Work Area over the [[spatial projection]] (plain 1, swamp 5, impassable excluded). Breaks rank ties in the Matcher (ADR 0002); a Work Area with no travel cost — unreachable or empty — makes the Task inapplicable to that creep: never matched fresh, and a remembered assignment to it is released.

### Workforce target
The number of creeps the colony maintains: the total [[seat]] count across all sources, floored at 2. Derived fresh each tick from the Snapshot, never persisted. Spawning fills the gap between living creeps and the target; without a spatial projection only the floor applies. Seats count by terrain alone (ADR 0001), so an unreachable source still raises the target — the surplus flows to Upgrade.

### Spatial projection
The Snapshot's map-shaped view of the spawn room: three-state terrain (plain / swamp / wall) plus entity positions. Introduced for Seat counting; grows toward the Resolver's needs (ADR 0001). A tile absent from the projection is outside the room and impassable.

### Worker unit
The repeating [Work; Carry; Move] block worker bodies are built from: 200 energy, full speed empty, half speed loaded. Capacity buys as many whole units as it can; what's left is remainder.

### Fatigue parity
The body-generation invariant (ADR 0003): a worker body padded beyond whole [[worker unit]]s never moves slower than the pure-unit body, empty or loaded. The remainder buys as much Carry as parity allows, then Move — never Work.

### Disaster fallback
The zero-creep spawning rule: an empty colony spawns one bare [[worker unit]] from whatever energy is banked right now, rather than waiting for a full capacity it can never refill. The one body-generation path that ignores the remainder — time-to-first-creep outranks spending the bank.

## Avoided terms

- **Role** — creeps are not born with roles; work is Task-based. Don't reintroduce role-based vocabulary.
