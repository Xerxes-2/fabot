/// Below this file nothing in `Economy` reads `World` (`ARCHITECTURE.md` §1).
module Fabot.Core.Economy.Derived

open Fabot.Core
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Economy.Bodies

[<NoEquality; NoComparison>]
type Body =
    {
        Name: string
        Pos: Pos
        /// Live parts: a part shot to no hits does nothing.
        Work: int
        Carry: int
        Move: int
        Claim: int
        /// Dead parts counted: the length a successor is spawned at.
        Size: int
        Store: Store
        Energy: int
        Free: int
        TicksToLive: int
        Pattern: Pattern
    }

let keyPartsIn (pattern: Pattern) (body: Body) : int =
    match keyPartOf pattern with
    | Carry -> body.Carry
    | Claim -> body.Claim
    | _ -> body.Work

[<NoEquality; NoComparison>]
type SourceFact =
    {
        Id: string
        Pos: Pos
        Rate: int
        Post: Pos option
        Container: string option
        Link: string option
        Seats: int option
        Energy: int
        /// What every income and flow is read off, never `Rate` alone.
        Dug: int
    }

[<NoEquality; NoComparison>]
type MineralFact =
    {
        Id: string
        Pos: Pos
        Resource: Resource
    }

[<NoEquality; NoComparison>]
type RefillFact =
    {
        Id: string
        Pos: Pos
        Kind: Kind
        Free: int
    }

[<RequireQualifiedAccess>]
type StoreKind =
    | Container
    | Storage
    | Buffer
    | Remains

[<NoEquality; NoComparison>]
type StoreFact =
    {
        Id: string
        Pos: Pos
        Kind: StoreKind
        Energy: int
        Free: int
        Fill: float
        SourceDug: int
        Owned: bool
    }

[<RequireQualifiedAccess>]
type LinkRole =
    | Source
    | Controller
    | Hub

[<NoEquality; NoComparison>]
type LinkFact =
    {
        Id: string
        Pos: Pos
        Role: LinkRole
        Energy: int
        Free: int
        Cooldown: int
    }

[<NoEquality; NoComparison>]
type SiteFact = { Id: string; Pos: Pos; Left: int }

[<NoEquality; NoComparison>]
type RepairFact =
    {
        Id: string
        Kind: Kind
        Pos: Pos
        Hits: int
        HitsMax: int
        Decay: float
        Rebuild: int
    }

[<NoEquality; NoComparison>]
type PileFact =
    {
        Id: string
        Pos: Pos
        Resource: Resource
        Amount: int
    }

/// A room the economy works: a colony's own, or one declared to it.
[<NoEquality; NoComparison>]
type Ground =
    {
        Room: string
        Controller: Controller option
        Sources: SourceFact[]
        Minerals: MineralFact[]
        Refills: RefillFact[]
        Stores: StoreFact[]
        Sites: SiteFact[]
        Repairs: RepairFact[]
        Piles: PileFact[]
        Income: int
        /// The only income a body is hired against.
        Posted: int
        /// The engine accepts our reservation or claim on its controller.
        Reservable: bool
        OurReservation: int
        UpgradeSeats: int option
        Buffer: StoreFact option
        Feed: StoreFact option
        Links: LinkFact[]
        Receiver: LinkFact option
        BufferSeats: Pos list
    }

/// A resource, not a decider (`ARCHITECTURE.md` §2).
[<NoEquality; NoComparison>]
type Colony =
    {
        Room: string
        Controller: Controller
        Spawns: Spawn[]
        Bank: int
        BankCapacity: int
    }

[<NoEquality; NoComparison>]
type Derived =
    {
        Time: int
        /// Colony `i`'s room is `Grounds.[i]`.
        Colonies: Colony[]
        Grounds: Ground[]
        Claims: Controller[]
        Bodies: Body[]
        /// Counted against the quota as if they stood, so no seat is cast twice.
        Ovens: Body[]
        Workers: Body[]
        Carriers: Body[]
        Claimers: Body[]
        Posts: Map<string, Pos>
        /// Creep lifetimes of tower fire a reserve holds.
        ReserveLifetimes: float
    }

let private bodyOf (name: string) (pos: Pos) (parts: Part[]) (store: Store) (life: int) : Body =
    let mutable work = 0
    let mutable carry = 0
    let mutable move = 0
    let mutable claim = 0

    for part in parts do
        if part.Hits > 0 then
            match part.Kind with
            | Work -> work <- work + 1
            | Carry -> carry <- carry + 1
            | Move -> move <- move + 1
            | Claim -> claim <- claim + 1
            | _ -> ()

    {
        Name = name
        Pos = pos
        Work = work
        Carry = carry
        Move = move
        Claim = claim
        Size = parts.Length
        Store = store
        Energy = Store.amountOf Energy store
        Free = Store.free store
        TicksToLive = life
        Pattern =
            patternOfName name
            |> Option.defaultWith (fun () -> patternOfParts work carry move claim)
    }

let private reservable (me: string) (c: Controller) =
    c.Owner.IsNobody
    && (match c.Reservation with
        | Some(user, _) -> user = me
        | None -> true)

/// `None` where the Atlas counts none: unknown, not zero.
let private seatsAt (atlas: Atlas) (range: int) (target: Pos) =
    match List.length (Atlas.seats atlas Grid.One range target) with
    | 0 -> None
    | n -> Some n

/// So the dig finishes the source inside its regeneration window despite the
/// ticks a body loses walking and dropping its load.
let harvestSpareWork = 1

let digWork (rate: int) =
    (rate + Engine.harvestPerWork - 1) / Engine.harvestPerWork + harvestSpareWork

let dugAt (capacity: int) (home: bool) (rate: int) (posted: bool) (seats: int option) : int =
    let body =
        if not posted then
            Bodies.sized Pattern.Worker Engine.maxBodyParts capacity
        elif home then
            homeHarvester (digWork rate) capacity
        else
            Bodies.sized Pattern.Harvester (digWork rate) capacity

    match body with
    | Some body ->
        // Not knowing a source's seats is no crew.
        let diggers = if posted then 1 else Option.defaultValue 0 seats

        min rate (diggers * partCountIn body Work * Engine.harvestPerWork)
    | None -> 0

/// Low while the room grows, so the walls never outbid its upgrading; a
/// room fallen under the first tower's level keeps that level's.
let rampartTarget (level: int) =
    match level with
    | l when l <= 3 -> 10_000
    | 4 -> 50_000
    | 5 -> 100_000
    | 6 -> 500_000
    | 7 -> 1_000_000
    | _ -> 3_000_000

/// Ten decay periods: the margin every bot keeps against losing a rampart.
let rampartSafe = 10 * Engine.rampartDecayAmount

let private groundOf (atlas: Atlas) (me: string) (capacity: int) (rate: int) (room: Room) : Ground =
    let ownedRoom = room.Controller |> Option.exists (fun c -> c.Owner.IsOurs)

    let level =
        room.Controller |> Option.map (fun c -> c.Level) |> Option.defaultValue 0

    let built = Atlas.buffer room

    let isBuffer (id: string) =
        match built with
        | Some b -> b.Id = id
        | None -> false

    let storage =
        room.Structures |> Array.tryFind (fun s -> s.Kind.IsStorage && s.Owner.IsOurs)

    let links =
        room.Structures
        |> Array.choose (fun s ->
            if not s.Kind.IsLink || not s.Owner.IsOurs then
                None
            elif Atlas.isSourceLink room s.Pos then
                Some LinkRole.Source
            elif isBuffer s.Id then
                Some LinkRole.Controller
            elif storage |> Option.exists (fun st -> Pos.within 1 st.Pos s.Pos) then
                Some LinkRole.Hub
            else
                None
            |> Option.map (fun role ->
                {
                    Id = s.Id
                    Pos = s.Pos
                    Role = role
                    Energy = Store.amountOf Energy s.Store
                    Free = Store.free s.Store
                    Cooldown = s.Cooldown
                }))

    let controllerLink = links |> Array.tryFind (fun l -> l.Role.IsController)
    let hubLink = links |> Array.tryFind (fun l -> l.Role.IsHub)

    let sources =
        room.Sources
        |> Array.map (fun s ->
            let post = Atlas.postOf room s.Pos
            let seats = seatsAt atlas 1 s.Pos

            {
                Id = s.Id
                Pos = s.Pos
                Rate = rate
                Post = post |> Option.map fst
                Container = post |> Option.bind snd
                Link =
                    links
                    |> Array.tryFind (fun l -> l.Role.IsSource && Pos.within 2 s.Pos l.Pos)
                    |> Option.map (fun l -> l.Id)
                Seats = seats
                Energy = s.Energy
                Dug = dugAt capacity ownedRoom rate (Option.isSome post) seats
            })

    let servedDug (pos: Pos) =
        sources
        |> Array.sumBy (fun s ->
            match Pos.range s.Pos pos with
            | Some 1 -> s.Dug
            | _ -> 0)

    let minerals =
        room.Minerals
        |> Array.filter (fun m ->
            m.Amount > 0
            && room.Structures
               |> Array.exists (fun s ->
                   s.Kind.IsExtractor && s.Owner.IsOurs && Pos.within 0 s.Pos m.Pos))
        |> Array.map (fun m ->
            {
                Id = m.Id
                Pos = m.Pos
                Resource = m.Resource
            })

    let refills = ResizeArray()
    let stores = ResizeArray()
    let repairs = ResizeArray()

    for s in room.Structures do
        match s.Kind with
        | Kind.Spawn
        | Kind.Extension
        | Kind.Tower when s.Owner.IsOurs && Store.free s.Store > 0 ->
            refills.Add
                {
                    Id = s.Id
                    Pos = s.Pos
                    Kind = s.Kind
                    Free = Store.free s.Store
                }
        | Kind.Container
        | Kind.Storage ->
            let energy = Store.amountOf Energy s.Store
            let owned = s.Kind.IsStorage && s.Owner.IsOurs
            let dug = if s.Kind.IsContainer then servedDug s.Pos else 0

            let kind =
                if s.Kind.IsStorage then StoreKind.Storage
                elif ownedRoom && isBuffer s.Id then StoreKind.Buffer
                else StoreKind.Container

            if energy > 0 || owned || kind.IsBuffer then
                stores.Add
                    {
                        Id = s.Id
                        Pos = s.Pos
                        Kind = kind
                        Energy = energy
                        Free = Store.free s.Store
                        Fill =
                            if s.Store.Capacity > 0 then
                                float (Store.used s.Store) / float s.Store.Capacity
                            else
                                0.0
                        SourceDug = dug
                        Owned = owned
                    }
        | _ -> ()

        match s.Kind with
        | Kind.Rampart when s.Owner.IsOurs && s.Hits < min (rampartTarget level) s.HitsMax ->
            repairs.Add
                {
                    Id = s.Id
                    Kind = s.Kind
                    Pos = s.Pos
                    Hits = s.Hits
                    HitsMax = min (rampartTarget level) s.HitsMax
                    Decay = float Engine.rampartDecayAmount / float Engine.rampartDecayTime
                    Rebuild = 0
                }
        | Kind.Road
        | Kind.Container when s.Hits < s.HitsMax ->
            let swamp = Terrain.isSwamp room.Terrain s.Pos.X s.Pos.Y

            let decay, rebuild =
                if s.Kind.IsRoad then
                    let ratio = if swamp then Engine.roadSwampRatio else 1

                    float (Engine.roadDecayAmount * ratio) / float Engine.roadDecayTime,
                    Engine.roadBuildCost * ratio
                else
                    let period =
                        if ownedRoom then
                            Engine.containerDecayTimeOwned
                        else
                            Engine.containerDecayTime

                    float Engine.containerDecayAmount / float period, Engine.containerBuildCost

            repairs.Add
                {
                    Id = s.Id
                    Kind = s.Kind
                    Pos = s.Pos
                    Hits = s.Hits
                    HitsMax = s.HitsMax
                    Decay = decay
                    Rebuild = rebuild
                }
        | _ -> ()

    for r in room.Remains do
        let energy = Store.amountOf Energy r.Store

        if energy > 0 then
            stores.Add
                {
                    Id = r.Id
                    Pos = r.Pos
                    Kind = StoreKind.Remains
                    Energy = energy
                    Free = 0
                    Fill = 1.0
                    SourceDug = 0
                    Owned = false
                }

    let storeOf (kind: StoreKind) (dug: int) (l: LinkFact) =
        {
            Id = l.Id
            Pos = l.Pos
            Kind = kind
            Energy = l.Energy
            Free = l.Free
            Fill = float l.Energy / float (max 1 (l.Energy + l.Free))
            SourceDug = dug
            Owned = false
        }

    let feed =
        match controllerLink, hubLink with
        | Some c, hub ->
            stores.Add(storeOf StoreKind.Buffer 0 c)
            hub |> Option.map (storeOf StoreKind.Container 0)
        | None, hub ->
            hub
            |> Option.iter (fun hub ->
                let dug =
                    sources |> Array.sumBy (fun s -> if Option.isSome s.Link then s.Dug else 0)

                if hub.Energy > 0 then
                    stores.Add(storeOf StoreKind.Container dug hub))

            None

    let stores = stores.ToArray()
    let buffer = stores |> Array.tryFind (fun s -> s.Kind.IsBuffer)

    {
        Room = room.Name
        Controller = room.Controller
        Sources = sources
        Minerals = minerals
        Refills = refills.ToArray()
        Stores = stores
        Sites =
            room.Sites
            |> Array.filter (fun s -> s.Owner.IsOurs)
            |> Array.map (fun s ->
                {
                    Id = s.Id
                    Pos = s.Pos
                    Left = max 0 (s.ProgressTotal - s.Progress)
                })
        Repairs = repairs.ToArray()
        Piles =
            room.Piles
            |> Array.map (fun p ->
                {
                    Id = p.Id
                    Pos = p.Pos
                    Resource = p.Resource
                    Amount = p.Amount
                })
        Income = sources |> Array.sumBy (fun s -> s.Dug)
        Posted = sources |> Array.sumBy (fun s -> if Option.isSome s.Container then s.Dug else 0)
        Reservable = room.Controller |> Option.exists (reservable me)
        OurReservation =
            match room.Controller |> Option.bind (fun c -> c.Reservation) with
            | Some(user, ticks) when user = me -> ticks
            | _ -> 0
        UpgradeSeats =
            match room.Controller with
            | Some c -> seatsAt atlas (Task.rangeOf (Task.Upgrade c.Id)) c.Pos
            | None -> Some 0
        Buffer = buffer
        Feed = feed |> Option.orElse buffer
        Links = links
        Receiver = controllerLink |> Option.orElse hubLink
        BufferSeats =
            match buffer, room.Controller with
            | Some b, Some c ->
                Atlas.seats atlas Grid.One 1 b.Pos
                |> List.filter (Pos.within Engine.upgradeRange c.Pos)
            | _ -> []
    }

let private blind (outpost: Tick.Outpost) : Room =
    {
        Name = outpost.Room
        Terrain = [||]
        Controller =
            Some
                {
                    Id = outpost.Controller.Id
                    Pos = outpost.Controller.Pos
                    Owner = Nobody
                    Level = 0
                    Progress = 0
                    ProgressTotal = 0
                    TicksToDowngrade = 0
                    Reservation = None
                    SafeMode = 0
                    SafeModeAvailable = 0
                }
        EnergyAvailable = 0
        EnergyCapacity = 0
        Sources =
            outpost.Sources
            |> List.map (fun s ->
                {
                    Id = s.Id
                    Pos = s.Pos
                    Energy = 0
                    EnergyCapacity = 0
                    TicksToRegeneration = 0
                })
            |> Array.ofList
        Minerals = [||]
        Structures = [||]
        Sites = [||]
        Piles = [||]
        Remains = [||]
        Hostiles = [||]
    }

let ofWorld
    (world: World)
    (atlas: Atlas)
    (declarations: Tick.Declarations)
    (worked: Tick.Outpost list)
    (claims: string list)
    : Derived =
    let colonies =
        world.Rooms
        |> Array.choose (fun room ->
            match room.Controller with
            | Some c when c.Owner.IsOurs ->
                let spawns = world.Spawns |> Array.filter (fun s -> s.Pos.Room = room.Name)

                if spawns.Length = 0 then
                    None
                else
                    Some(
                        room,
                        {
                            Room = room.Name
                            Controller = c
                            Spawns = spawns
                            Bank = room.EnergyAvailable
                            BankCapacity = room.EnergyCapacity
                        }
                    )
            | _ -> None)

    let isColony name =
        colonies |> Array.exists (fun (room, _) -> room.Name = name)

    // A declared room is dug by bodies its richest colony casts.
    let richest =
        colonies |> Array.fold (fun most (room, _) -> max most room.EnergyCapacity) 0

    // At the rate the reserver holds, not this tick's: a body lives longer
    // than a lapse lasts. The engine refuses a harvest on another's hold.
    let remoteOf (outpost: Tick.Outpost) =
        let room =
            world.Rooms
            |> Array.tryFind (fun r -> r.Name = outpost.Room)
            |> Option.defaultWith (fun () -> blind outpost)

        match room.Controller with
        | Some c when not (reservable world.Me c) ->
            groundOf atlas world.Me richest 0 { room with Sources = [||] }
        | _ when richest >= Engine.bodyCost (blockOf Pattern.Claimer) ->
            groundOf atlas world.Me richest Engine.heldOutputPerTick room
        | _ -> groundOf atlas world.Me richest Engine.neutralOutputPerTick room

    let grounds =
        Array.append
            (colonies
             |> Array.map (fun (room, _) ->
                 groundOf atlas world.Me room.EnergyCapacity Engine.heldOutputPerTick room))
            (worked
             |> List.filter (fun o -> not (isColony o.Room))
             |> List.map remoteOf
             |> Array.ofList)

    let bodies =
        world.Creeps
        |> Array.map (fun c -> bodyOf c.Name c.Pos c.Body c.Store c.TicksToLive)

    {
        Time = world.Time
        Colonies = colonies |> Array.map snd
        Grounds = grounds
        Claims =
            world.Rooms
            |> Array.filter (fun room -> List.contains room.Name claims)
            |> Array.choose (fun room -> room.Controller)
            |> Array.filter (reservable world.Me)
        Bodies = bodies
        Ovens =
            world.Spawns
            |> Array.choose (fun s ->
                s.Spawning
                |> Option.map (fun oven ->
                    let life =
                        if oven.Body |> Array.exists (fun p -> p.Kind.IsClaim) then
                            Engine.claimLifetime
                        else
                            Engine.creepLifetime

                    bodyOf oven.Name s.Pos oven.Body Store.empty life))
        Workers = bodies |> Array.filter (fun b -> b.Work > 0 && b.Carry > 0)
        Carriers = bodies |> Array.filter (fun b -> b.Carry > 0 && b.Work = 0)
        Claimers = bodies |> Array.filter (fun b -> b.Claim > 0)
        Posts =
            grounds
            |> Array.collect (fun g ->
                g.Sources |> Array.choose (fun s -> s.Post |> Option.map (fun p -> s.Id, p)))
            |> Map.ofArray
        ReserveLifetimes = declarations.ReserveLifetimes
    }
