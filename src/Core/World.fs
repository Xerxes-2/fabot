/// This tick's facts, read once by the shell (`Fabot.World.ofGame`). Nothing in
/// Core reads the game any other way. It grows with the modules that read it;
/// a field nobody reads is not added ahead of its reader.
module Fabot.Core.World

[<Struct>]
type Pos = { Room: string; X: int; Y: int }

type Controller =
    {
        Id: string
        Level: int
        /// Undefined on a controller nobody owns; read as 0.
        TicksToDowngrade: int
    }

type Room =
    {
        Name: string
        /// Ours only: a room someone else owns or nobody owns has `None`.
        Controller: Controller option
        EnergyAvailable: int
        EnergyCapacity: int
    }

type Spawn =
    {
        Id: string
        Name: string
        Room: string
        /// Ticks until the body in the oven stands; 0 when idle.
        Spawning: int
    }

type Creep =
    {
        Name: string
        Pos: Pos
        Hits: int
        HitsMax: int
        TicksToLive: int
    }

type World =
    {
        Time: int
        /// `Game.cpu.limit` and `Game.cpu.bucket`.
        CpuLimit: float
        Bucket: int
        /// Visible rooms.
        Rooms: Room[]
        /// Ours.
        Spawns: Spawn[]
        /// Ours, standing (still in an oven excluded).
        Creeps: Creep[]
    }
