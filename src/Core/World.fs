/// Nothing in Core reads the game but through this. Facts only: no conclusion
/// is stored here, and a field nobody reads is not added ahead of its reader.
module Fabot.Core.World

open Fabot.Core.Vocabulary

/// Compared by hand: Fable's record compare walks every field, per tile per tick.
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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Pos =
    let range (a: Pos) (b: Pos) : int option =
        if a.Room = b.Room then
            Some(max (abs (a.X - b.X)) (abs (a.Y - b.Y)))
        else
            None

    let within (range: int) (a: Pos) (b: Pos) : bool =
        a.Room = b.Room && max (abs (a.X - b.X)) (abs (a.Y - b.Y)) <= range

type Owner =
    | Ours
    | Theirs of username: string
    | Nobody

type Store =
    {
        Held: (Resource * int)[]
        /// Over every resource; 0 for no store.
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

    /// A rampart is walkable only if ours or public: `Structure.Walkable`.
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
        Cooldown: int
        /// Ticks to an Invader core's collapse, at the latest; 0 for any other structure.
        Collapse: int
        /// An Invader core's level, 0 for an expansion core; 0 for any other structure.
        Level: int
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
        /// 0 while the engine has no timer running.
        TicksToRegeneration: int
    }

type Mineral =
    {
        Id: string
        Pos: Pos
        Resource: Resource
        Amount: int
    }

type Pile =
    {
        Id: string
        Pos: Pos
        Resource: Resource
        Amount: int
    }

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
        /// Username and ticks left.
        Reservation: (string * int) option
        SafeMode: int
        SafeModeAvailable: int
        SafeModeCooldown: int
        UpgradeBlocked: int
    }

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

type Fallen = { Creep: Creep; Died: int }

/// A tombstone or a ruin.
type Remains =
    {
        Id: string
        Pos: Pos
        Store: Store
        Fallen: Fallen option
    }

type Room =
    {
        Name: string
        /// `Game.map.getRoomTerrain(name).getRawBuffer()`.
        Terrain: byte[]
        Controller: Controller option
        /// 0 in a room without spawns.
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
        Hostiles: Creep[]
    }

type Oven =
    {
        Name: string
        Remaining: int
        Body: Part[]
    }

type Spawn =
    {
        Id: string
        Name: string
        Pos: Pos
        Spawning: Oven option
    }

type World =
    {
        Time: int
        /// Empty while we own nothing.
        Me: string
        CpuLimit: float
        Bucket: int
        /// Visible rooms.
        Rooms: Room[]
        /// Ours.
        Spawns: Spawn[]
        /// Ours, standing (still in an oven excluded).
        Creeps: Creep[]
        /// In every room, seen or not: what MAX_CONSTRUCTION_SITES counts.
        OurSites: int
    }

module Terrain =
    let wall = 1uy
    let swamp = 2uy

    let isWall (terrain: byte[]) (x: int) (y: int) = terrain.[y * 50 + x] &&& wall <> 0uy

    let isSwamp (terrain: byte[]) (x: int) (y: int) =
        let t = terrain.[y * 50 + x]
        t &&& wall = 0uy && t &&& swamp <> 0uy

module Hostiles =
    let private strikes (creep: Creep) =
        creep.Body |> Array.exists (fun p -> p.Kind.IsAttack || p.Kind.IsRangedAttack)

    let owned (rooms: Room[]) : Set<string> =
        rooms
        |> Array.choose (fun r ->
            match r.Controller with
            | Some c when c.Owner.IsOurs -> Some r.Name
            | _ -> None)
        |> Set.ofArray

    /// HEAL and WORK threaten beside a striker (owner's call): alone they are
    /// a rival's healer in transit or its remote harvester, but WORK in a room
    /// whose controller is ours can only be there to dismantle.
    let armed (owned: Set<string>) (hostiles: Creep[]) (creep: Creep) =
        let works = creep.Body |> Array.exists (fun p -> p.Kind.IsWork)

        strikes creep
        || works && owned.Contains creep.Pos.Room
        || (works || creep.Body |> Array.exists (fun p -> p.Kind.IsHeal))
           && hostiles |> Array.exists (fun o -> o.Pos.Room = creep.Pos.Room && strikes o)

type Seen = { Gone: int; Armed: bool }

type Sightings = Map<string, Map<string, Seen>>

module Sightings =

    let step (world: World) (previous: Sightings) : Sightings =
        let mutable next =
            previous
            |> Map.map (fun _ seen -> seen |> Map.filter (fun _ s -> s.Gone > world.Time))
            |> Map.filter (fun _ seen -> not seen.IsEmpty)

        let owned = Hostiles.owned world.Rooms

        // A room in sight holds what stands there now: a wave killed reopens it.
        for room in world.Rooms do
            let seen =
                Map.ofList
                    [
                        for creep in room.Hostiles do
                            if Hostiles.armed owned room.Hostiles creep then
                                creep.Id,
                                {
                                    Gone = world.Time + creep.TicksToLive
                                    Armed = true
                                }
                        for s in room.Structures do
                            if s.Kind.IsInvaderCore then
                                s.Id,
                                {
                                    Gone = world.Time + s.Collapse
                                    Armed = s.Level > 0
                                }
                    ]

            next <-
                if seen.IsEmpty then
                    Map.remove room.Name next
                else
                    Map.add room.Name seen next

        next

    let private none (counts: Seen -> bool) (sightings: Sightings) (time: int) (room: string) =
        match Map.tryFind room sightings with
        | Some seen -> seen |> Map.forall (fun _ s -> s.Gone <= time || not (counts s))
        | None -> true

    let clear = none (fun _ -> true)

    let unarmed = none _.Armed
