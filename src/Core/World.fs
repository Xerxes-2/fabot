/// This tick's facts, read once by the shell (`Fabot.World.ofGame`). Nothing in
/// Core reads the game any other way. Facts only: no conclusion is stored here,
/// and a field nobody reads is not added ahead of its reader.
module Fabot.Core.World

open Fabot.Core.Vocabulary

/// A tile of one room. Compared by hand: Fable's structural compare on a
/// record walks every field, and positions are compared per tile per tick.
[<CustomEquality; CustomComparison>]
type Pos =
    {
        Room: string
        X: int
        Y: int
    }

    override this.Equals other =
        match other with
        | :? Pos as that -> this.X = that.X && this.Y = that.Y && this.Room = that.Room
        | _ -> false

    override this.GetHashCode() =
        (this.Y * 50 + this.X) ^^^ this.Room.GetHashCode()

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Pos as that ->
                match compare this.Room that.Room with
                | 0 -> compare (this.Y * 50 + this.X) (that.Y * 50 + that.X)
                | c -> c
            | _ -> 1

/// Who an object belongs to, as far as a decision asks.
type Owner =
    | Ours
    | Theirs of username: string
    | Nobody

/// A store's contents: each resource it holds, and what it can hold in all.
type Store =
    {
        Held: (Resource * int)[]
        /// Total capacity, over every resource; 0 for no store.
        Capacity: int
    }

module Store =
    let empty = { Held = [||]; Capacity = 0 }

    let amountOf (resource: Resource) (store: Store) : int =
        let mutable n = 0

        for r, amount in store.Held do
            if r = resource then
                n <- n + amount

        n

    let used (store: Store) : int =
        let mutable n = 0

        for _, amount in store.Held do
            n <- n + amount

        n

    let free (store: Store) : int = max 0 (store.Capacity - used store)

/// The structure kinds a decision names; the rest arrive as `Other`.
[<RequireQualifiedAccess; CustomEquality; CustomComparison>]
type Kind =
    | Spawn
    | Extension
    | Tower
    | Road
    | Container
    | Storage
    | Link
    | Rampart
    | Wall
    | Extractor
    | Terminal
    | Observer
    | Lab
    | Factory
    | PowerSpawn
    | Nuker
    | KeeperLair
    | InvaderCore
    | PowerBank
    | Portal
    | Other of name: string

    member this.Ordinal =
        match this with
        | Spawn -> 0
        | Extension -> 1
        | Tower -> 2
        | Road -> 3
        | Container -> 4
        | Storage -> 5
        | Link -> 6
        | Rampart -> 7
        | Wall -> 8
        | Extractor -> 9
        | Terminal -> 10
        | Observer -> 11
        | Lab -> 12
        | Factory -> 13
        | PowerSpawn -> 14
        | Nuker -> 15
        | KeeperLair -> 16
        | InvaderCore -> 17
        | PowerBank -> 18
        | Portal -> 19
        | Other _ -> 20

    override this.Equals other =
        match other with
        | :? Kind as that ->
            match this, that with
            | Other a, Other b -> a = b
            | _ -> this.Ordinal = that.Ordinal
        | _ -> false

    override this.GetHashCode() = this.Ordinal

    interface System.IComparable with
        member this.CompareTo other =
            match other with
            | :? Kind as that ->
                match this, that with
                | Other a, Other b -> compare a b
                | _ -> compare this.Ordinal that.Ordinal
            | _ -> 1

module Kind =
    /// The engine's `STRUCTURE_*` string.
    let ofName (name: string) : Kind =
        match name with
        | "spawn" -> Kind.Spawn
        | "extension" -> Kind.Extension
        | "tower" -> Kind.Tower
        | "road" -> Kind.Road
        | "container" -> Kind.Container
        | "storage" -> Kind.Storage
        | "link" -> Kind.Link
        | "rampart" -> Kind.Rampart
        | "constructedWall" -> Kind.Wall
        | "extractor" -> Kind.Extractor
        | "terminal" -> Kind.Terminal
        | "observer" -> Kind.Observer
        | "lab" -> Kind.Lab
        | "factory" -> Kind.Factory
        | "powerSpawn" -> Kind.PowerSpawn
        | "nuker" -> Kind.Nuker
        | "keeperLair" -> Kind.KeeperLair
        | "invaderCore" -> Kind.InvaderCore
        | "powerBank" -> Kind.PowerBank
        | "portal" -> Kind.Portal
        | other -> Kind.Other other

    let name (kind: Kind) : string =
        match kind with
        | Kind.Spawn -> "spawn"
        | Kind.Extension -> "extension"
        | Kind.Tower -> "tower"
        | Kind.Road -> "road"
        | Kind.Container -> "container"
        | Kind.Storage -> "storage"
        | Kind.Link -> "link"
        | Kind.Rampart -> "rampart"
        | Kind.Wall -> "constructedWall"
        | Kind.Extractor -> "extractor"
        | Kind.Terminal -> "terminal"
        | Kind.Observer -> "observer"
        | Kind.Lab -> "lab"
        | Kind.Factory -> "factory"
        | Kind.PowerSpawn -> "powerSpawn"
        | Kind.Nuker -> "nuker"
        | Kind.KeeperLair -> "keeperLair"
        | Kind.InvaderCore -> "invaderCore"
        | Kind.PowerBank -> "powerBank"
        | Kind.Portal -> "portal"
        | Kind.Other name -> name

    /// Whether a creep may stand on it: roads, containers and ramparts (a
    /// rampart only if ours or public; `Structure.Walkable` carries that).
    let walkable (kind: Kind) : bool =
        match kind with
        | Kind.Road
        | Kind.Container
        | Kind.Rampart
        | Kind.Portal -> true
        | _ -> false

type Structure =
    {
        Id: string
        Kind: Kind
        Pos: Pos
        Owner: Owner
        Hits: int
        HitsMax: int
        Store: Store
        /// Ticks before it may act again (extractor, lab, link, terminal…).
        Cooldown: int
        /// Whether our creeps may stand on it: the kind's answer, and a
        /// rampart's only when it is ours or public.
        Walkable: bool
    }

type Site =
    {
        Id: string
        Kind: Kind
        Pos: Pos
        Owner: Owner
        Progress: int
        ProgressTotal: int
    }

type Source =
    {
        Id: string
        Pos: Pos
        Energy: int
        EnergyCapacity: int
        /// Ticks until it regenerates; 0 while the engine has no timer running.
        TicksToRegeneration: int
    }

type Mineral =
    {
        Id: string
        Pos: Pos
        Resource: Resource
        Amount: int
    }

/// A dropped pile.
type Pile =
    {
        Id: string
        Pos: Pos
        Resource: Resource
        Amount: int
    }

/// A tombstone or a ruin: one store with a clock on it.
type Remains = { Id: string; Pos: Pos; Store: Store }

type Controller =
    {
        Id: string
        Pos: Pos
        Owner: Owner
        Level: int
        Progress: int
        ProgressTotal: int
        /// 0 on a controller nobody owns.
        TicksToDowngrade: int
        /// Who reserves it and for how long; `None` when nobody does.
        Reservation: (string * int) option
        /// Ticks of safe mode left; 0 when off.
        SafeMode: int
        SafeModeAvailable: int
    }

/// One body part as it stands: its kind, the hits left in it, its boost.
[<Struct>]
type Part =
    {
        Kind: BodyPart
        Hits: int
        Boost: Boost
    }

type Creep =
    {
        Id: string
        Name: string
        Owner: Owner
        Pos: Pos
        /// Head first, as the engine holds it.
        Body: Part[]
        Hits: int
        HitsMax: int
        /// Undefined while spawning; those are not in the world.
        TicksToLive: int
        Fatigue: int
        Store: Store
    }

type Room =
    {
        Name: string
        /// `Game.map.getRoomTerrain(name).getRawBuffer()`: per `y * 50 + x`,
        /// bit 1 wall, bit 2 swamp.
        Terrain: byte[]
        Controller: Controller option
        /// Spawn energy banked and its capacity; 0 in a room without spawns.
        EnergyAvailable: int
        EnergyCapacity: int
        Sources: Source[]
        Minerals: Mineral[]
        /// Every structure, any owner.
        Structures: Structure[]
        /// Every construction site, any owner.
        Sites: Site[]
        Piles: Pile[]
        Remains: Remains[]
        /// Every creep not ours standing here.
        Hostiles: Creep[]
    }

type Spawn =
    {
        Id: string
        Name: string
        Pos: Pos
        /// Ticks until the body in the oven stands; 0 when idle.
        Spawning: int
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

/// Terrain at a tile of a room's raw buffer.
module Terrain =
    let wall = 1uy
    let swamp = 2uy

    let isWall (terrain: byte[]) (x: int) (y: int) = terrain.[y * 50 + x] &&& wall <> 0uy

    let isSwamp (terrain: byte[]) (x: int) (y: int) =
        let t = terrain.[y * 50 + x]
        t &&& wall = 0uy && t &&& swamp <> 0uy
