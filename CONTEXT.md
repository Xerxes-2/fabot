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

## Avoided terms

- **Role** — creeps are not born with roles; work is Task-based. Don't reintroduce role-based vocabulary.

### Planner
The pure step that reads a Snapshot and generates this tick's full Task pool. Runs every tick from scratch — Tasks are never persisted.

### Matcher
The pure step that assigns creeps to Tasks (greedy matching) and emits Intents. Current assignments are the only thing remembered between ticks (anti-thrash).
