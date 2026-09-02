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
      /// Energy the spawn's own store can still take (0 = full).
      FreeCapacity: int
      IsSpawning: bool }

/// What the decision layer knows about one energy source this tick.
type SourceInfo =
    { Id: string }

/// What the decision layer knows about the room controller this tick.
type ControllerInfo =
    { Id: string }

/// What the decision layer knows about one owned creep this tick.
type CreepInfo =
    { Name: string
      /// Energy currently carried.
      Energy: int
      /// Carry capacity still free (0 = full).
      FreeCapacity: int }

/// Immutable projection of the current tick's game state; only what decisions need.
type Snapshot =
    { Time: int
      Spawns: SpawnInfo list
      Sources: SourceInfo list
      /// None when no spawn room has an owned controller (should not happen in practice).
      Controller: ControllerInfo option
      Creeps: CreepInfo list }

/// A unit of work in this tick's Task pool; creeps are interchangeable
/// executors that get matched to Tasks.
type Task =
    | Harvest of sourceId: string
    | Refill of spawnName: string
    | Upgrade of controllerId: string

/// A single described action to perform this tick; data only, never the game API.
type Intent =
    | SpawnCreep of spawnName: string * body: BodyPart list * creepName: string
    | HarvestSource of creepName: string * sourceId: string
    | TransferEnergyToSpawn of creepName: string * spawnName: string
    | UpgradeController of creepName: string * controllerId: string

/// Creep name -> task id. The only state remembered between ticks (anti-thrash).
type Assignments = Map<string, string>
