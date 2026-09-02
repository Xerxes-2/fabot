module Fabot.Core.Types

/// A creep body part, engine vocabulary kept minimal for the MVP worker.
type BodyPart =
    | Work
    | Carry
    | Move

/// What the decision layer knows about one spawn this tick.
type SpawnInfo =
    { Name: string
      /// Energy available for spawning in the spawn's room (spawn + extensions).
      EnergyAvailable: int
      IsSpawning: bool }

/// What the decision layer knows about one owned creep this tick.
type CreepInfo =
    { Name: string }

/// Immutable projection of the current tick's game state; only what decisions need.
type Snapshot =
    { Time: int
      Spawns: SpawnInfo list
      Creeps: CreepInfo list }

/// A single described action to perform this tick; data only, never the game API.
type Intent =
    | SpawnCreep of spawnName: string * body: BodyPart list * creepName: string

/// Creep name -> task id. The only state remembered between ticks (anti-thrash).
type Assignments = Map<string, string>
