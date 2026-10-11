/// The economy's slice of the tick's `Derived` (`ARCHITECTURE.md` §1): every
/// fact the pool, the quota and the matcher read, computed once from `World`.
/// Below this file nothing in `Economy` reads `World`.
module Fabot.Core.Economy.Derived

open Fabot.Core
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Economy.Bodies

/// One of our creeps as the economy reads it.
[<NoEquality; NoComparison>]
type Body =
    {
        Name: string
        Pos: Pos
        /// Live parts of each kind: a part shot to no hits does nothing.
        Work: int
        Carry: int
        Move: int
        Claim: int
        /// Every part, dead ones counted: the length a successor is spawned at.
        Size: int
        Store: Store
        Energy: int
        Free: int
        TicksToLive: int
        /// The pattern it was cast from: its name's, else its parts'.
        Pattern: Pattern
    }

/// A body's count of its pattern's key part.
let keyPartsIn (pattern: Pattern) (body: Body) : int =
    match keyPartOf pattern with
    | Carry -> body.Carry
    | Claim -> body.Claim
    | _ -> body.Work

/// A source the economy digs, and the container beside it if one stands.
[<NoEquality; NoComparison>]
type SourceFact =
    {
        Id: string
        Pos: Pos
        /// Energy a tick it yields: `Engine.heldOutputPerTick` in an owned or
        /// reserved room, `neutralOutputPerTick` elsewhere.
        Rate: int
        /// The seat a harvester digs it from: the one under a container or a
        /// container site, farthest from the controller, so a controller
        /// container on a seat is no post. A built container before a site,
        /// then the first tile.
        Post: Pos option
        /// The container standing on the post, which takes what it digs.
        Container: string option
        /// The tiles a body can stand on to dig it: what bounds its holders
        /// (`seatsAt`).
        Seats: int
        /// Energy left in it until it regenerates.
        Energy: int
        /// Energy a tick its diggers take out of it (`dugAt`): what every
        /// income and flow is read off, never the rate alone.
        Dug: int
    }

/// A mineral under our extractor, with ore left in it.
[<NoEquality; NoComparison>]
type MineralFact =
    {
        Id: string
        Pos: Pos
        Resource: Resource
    }

/// A spawn, extension or tower of ours with room for energy.
[<NoEquality; NoComparison>]
type RefillFact =
    {
        Id: string
        Pos: Pos
        Kind: Kind
        Free: int
    }

/// What kind of store a `StoreFact` is.
[<RequireQualifiedAccess>]
type StoreKind =
    | Container
    | Storage
    /// The controller's container: what the upgraders draw from.
    | Buffer
    /// A tombstone or a ruin: a store that ends.
    | Remains

/// A store holding energy, or our storage or buffer with room for it.
[<NoEquality; NoComparison>]
type StoreFact =
    {
        Id: string
        Pos: Pos
        Kind: StoreKind
        Energy: int
        Free: int
        /// Its share of its capacity held, 0 to 1.
        Fill: float
        /// The rate of the source a container serves; 0 for any other store.
        SourceRate: int
        /// Ours: the storage the colony banks in.
        Owned: bool
    }

/// A construction site of ours and what it still takes.
[<NoEquality; NoComparison>]
type SiteFact = { Id: string; Pos: Pos; Left: int }

/// A decaying structure below its full hits: a road or a container.
[<NoEquality; NoComparison>]
type RepairFact =
    {
        Id: string
        Pos: Pos
        Hits: int
        HitsMax: int
        /// Hits it loses a tick to decay, averaged over the decay period.
        Decay: float
        /// Energy to build it again once decay takes it.
        Rebuild: int
    }

/// A dropped pile.
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
        /// Energy a tick its sources' diggers take (`SourceFact.Dug`).
        Income: int
        /// Its controller is unowned and reserved by nobody or by us: a
        /// reservation or a claim the engine accepts.
        Reservable: bool
        /// Ticks left on our reservation of its controller; 0 for none.
        OurReservation: int
        /// The tiles a body can stand on to upgrade its controller
        /// (`seatsAt`); 0 for no controller.
        UpgradeSeats: int
    }

/// A colony: a room whose controller is ours with a spawn of ours in it. A
/// resource, not a decider (`ARCHITECTURE.md` §2).
[<NoEquality; NoComparison>]
type Colony =
    {
        Room: string
        Controller: Controller
        Spawns: Spawn[]
        /// Spawn energy banked, and what the bank holds full.
        Bank: int
        BankCapacity: int
    }

/// The economy's facts this tick, one record.
[<NoEquality; NoComparison>]
type Derived =
    {
        Time: int
        /// Colony `i`'s room is `Grounds.[i]`.
        Colonies: Colony[]
        /// The colonies' rooms first, in world order, then the declared ones.
        Grounds: Ground[]
        /// Controllers in the rooms declared for a claim, unowned and
        /// reserved by nobody or by us.
        Claims: Controller[]
        Bodies: Body[]
        /// The bodies in our spawns' ovens, standing at their spawn: counted
        /// against the quota as if they stood, so no seat is cast twice.
        Ovens: Body[]
        /// Bodies that can dig or spend energy and carry it: Work and Carry.
        Workers: Body[]
        /// Bodies that carry and do not dig.
        Carriers: Body[]
        /// Bodies with a CLAIM part.
        Claimers: Body[]
        /// The creep lifetimes of tower fire a reserve holds (`Declarations`).
        ReserveLifetimes: float
    }

/// Chebyshev range between two tiles of one room; `None` across rooms.
let private rangeIn (a: Pos) (b: Pos) : int option =
    if a.Room = b.Room then
        Some(max (abs (a.X - b.X)) (abs (a.Y - b.Y)))
    else
        None

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

/// An unowned controller reserved by nobody or by us: the engine accepts our
/// reservation or claim on it.
let private reservable (me: string) (c: Controller) =
    c.Owner = Nobody
    && (match c.Reservation with
        | Some(user, _) -> user = me
        | None -> true)

/// Whether the room's sources yield the held rate: its controller is owned
/// or reserved, by anyone (the engine's rule for a source's capacity).
let private held (room: Room) =
    match room.Controller with
    | Some c -> c.Owner <> Nobody || Option.isSome c.Reservation
    | None -> false

/// The tiles a one-tile body may stand on within `range` of a target;
/// unbounded where the Atlas counts none (a room it has no grid for, or a
/// target walled in, which nobody reaches anyway): not knowing is no bound.
let private seatsAt (atlas: Atlas) (range: int) (target: Pos) =
    match List.length (Atlas.seats atlas Grid.One range target) with
    | 0 -> System.Int32.MaxValue
    | n -> n

/// A harvester crew's WORK past the source's saturation: the dig finishes
/// the source inside its regeneration window despite the ticks a body loses
/// walking and dropping its load. A named margin.
let harvestSpareWork = 1

/// The WORK a source's harvester is sized to: its output's, and a spare.
let digWork (rate: int) =
    (rate + Engine.harvestPerWork - 1) / Engine.harvestPerWork + harvestSpareWork

/// What a source's diggers take out of it a tick at a bank of `capacity`:
/// the one harvester its post seats (a home one where `home`), else a worker
/// on each of its seats; never over its rate.
let dugAt (capacity: int) (home: bool) (rate: int) (posted: bool) (seats: int) : int =
    let body =
        if not posted then
            Bodies.sized Pattern.Worker Engine.maxBodyParts capacity
        elif home then
            homeHarvester (digWork rate) capacity
        else
            Bodies.sized Pattern.Harvester (digWork rate) capacity

    match body with
    | Some body ->
        let diggers = if posted then 1 else seats
        min rate (diggers * partCountIn body Work * Engine.harvestPerWork)
    | None -> 0

let private groundOf (atlas: Atlas) (me: string) (capacity: int) (room: Room) : Ground =
    let rate =
        if held room then
            Engine.heldOutputPerTick
        else
            Engine.neutralOutputPerTick

    let ownedRoom = room.Controller |> Option.exists (fun c -> c.Owner = Ours)

    let containers = room.Structures |> Array.filter (fun s -> s.Kind = Kind.Container)

    let footings =
        Array.append
            (containers |> Array.map (fun c -> c.Pos, Some c.Id))
            (room.Sites
             |> Array.filter (fun s -> s.Kind = Kind.Container && s.Owner = Ours)
             |> Array.map (fun s -> s.Pos, None))

    let fromController (p: Pos) =
        room.Controller
        |> Option.bind (fun c -> rangeIn c.Pos p)
        |> Option.defaultValue 0

    // A source's post, and the container standing on it.
    let postOf (source: Pos) =
        Atlas.seats atlas Grid.One 1 source
        |> List.choose (fun seat ->
            footings
            |> Array.tryFind (fun (p, _) -> p = seat)
            |> Option.map (fun (_, built) ->
                (-(fromController seat), Option.isNone built), (seat, built)))
        |> List.sortBy fst
        |> List.tryHead
        |> Option.map snd

    let servedRate (pos: Pos) =
        let mutable r = 0

        for s in room.Sources do
            if rangeIn s.Pos pos = Some 1 then
                r <- rate

        r

    let sources =
        room.Sources
        |> Array.map (fun s ->
            let post = postOf s.Pos
            let seats = seatsAt atlas 1 s.Pos

            {
                Id = s.Id
                Pos = s.Pos
                Rate = rate
                Post = post |> Option.map fst
                Container = post |> Option.bind snd
                Seats = seats
                Energy = s.Energy
                Dug = dugAt capacity ownedRoom rate (Option.isSome post) seats
            })

    let minerals =
        room.Minerals
        |> Array.filter (fun m ->
            m.Amount > 0
            && room.Structures
               |> Array.exists (fun s ->
                   s.Kind = Kind.Extractor && s.Owner = Ours && s.Pos = m.Pos))
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
        | Kind.Tower when s.Owner = Ours && Store.free s.Store > 0 ->
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
            let owned = s.Kind = Kind.Storage && s.Owner = Ours
            let rate = if s.Kind = Kind.Container then servedRate s.Pos else 0

            let kind =
                if s.Kind = Kind.Storage then
                    StoreKind.Storage
                elif ownedRoom && Atlas.isBuffer room s.Pos then
                    StoreKind.Buffer
                else
                    StoreKind.Container

            if energy > 0 || owned || kind = StoreKind.Buffer then
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
                        SourceRate = rate
                        Owned = owned
                    }
        | _ -> ()

        match s.Kind with
        | Kind.Road
        | Kind.Container when s.Hits < s.HitsMax ->
            let swamp = Terrain.isSwamp room.Terrain s.Pos.X s.Pos.Y

            let decay, rebuild =
                if s.Kind = Kind.Road then
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
                    SourceRate = 0
                    Owned = false
                }

    {
        Room = room.Name
        Controller = room.Controller
        Sources = sources
        Minerals = minerals
        Refills = refills.ToArray()
        Stores = stores.ToArray()
        Sites =
            room.Sites
            |> Array.filter (fun s -> s.Owner = Ours)
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
        Reservable = room.Controller |> Option.exists (reservable me)
        OurReservation =
            match room.Controller |> Option.bind (fun c -> c.Reservation) with
            | Some(user, ticks) when user = me -> ticks
            | _ -> 0
        UpgradeSeats =
            room.Controller
            |> Option.map (fun c -> seatsAt atlas (Task.rangeOf (Task.Upgrade c.Id)) c.Pos)
            |> Option.defaultValue 0
    }

/// The economy's facts this tick, seats read off the tick's `atlas`. `worked`
/// are the rooms declared to the economy beyond the colonies' own (outposts, a
/// claim's room once a spawn site stands); `claims` the rooms declared for a
/// claim. A declared room out of sight contributes nothing.
let ofWorld
    (world: World)
    (atlas: Atlas)
    (declarations: Tick.Declarations)
    (worked: string list)
    (claims: string list)
    : Derived =
    let colonies =
        world.Rooms
        |> Array.choose (fun room ->
            match room.Controller with
            | Some c when c.Owner = Ours ->
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

    let declared =
        world.Rooms
        |> Array.filter (fun room -> List.contains room.Name worked && not (isColony room.Name))

    // A declared room is dug by bodies its richest colony casts.
    let richest =
        colonies |> Array.fold (fun most (room, _) -> max most room.EnergyCapacity) 0

    let grounds =
        Array.append
            (colonies
             |> Array.map (fun (room, _) -> groundOf atlas world.Me room.EnergyCapacity room))
            (declared |> Array.map (groundOf atlas world.Me richest))

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
                        if oven.Body |> Array.exists (fun p -> p.Kind = Claim) then
                            Engine.claimLifetime
                        else
                            Engine.creepLifetime

                    bodyOf oven.Name s.Pos oven.Body Store.empty life))
        Workers = bodies |> Array.filter (fun b -> b.Work > 0 && b.Carry > 0)
        Carriers = bodies |> Array.filter (fun b -> b.Carry > 0 && b.Work = 0)
        Claimers = bodies |> Array.filter (fun b -> b.Claim > 0)
        ReserveLifetimes = declarations.ReserveLifetimes
    }
