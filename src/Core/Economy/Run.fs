module Fabot.Core.Economy.Run

open System.Collections.Generic
open Fabot.Core
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Grid
open Fabot.Core.Intents
open Fabot.Core.State
open Fabot.Core.Tick
open Fabot.Core.Move
open Fabot.Core.Economy.Task
open Fabot.Core.Economy.Derived
open Fabot.Core.Economy.Pool

/// The route search needs a bound; the economy works no room further than its
/// declarations.
let private reachHops = 3

/// The Atlas prices this room's leg only; this keeps a far task from reading
/// as near.
let private crossingTicks = Engine.roomSide

let private weightOf (priority: int) = max 1 (Price.bottomRank - priority + 1)

let private targetAt (pos: Pos) (range: int) : Target =
    {
        Room = pos.Room
        Tiles = [| Tiles.ofPos pos |]
        Range = range
        Avoid = Set.empty
        MaxHops = reachHops
    }

let private targetOf (offer: Offer) : Target =
    targetAt offer.Target (rangeOf offer.Task)

let private ticksTo (atlas: Atlas) (mover: Mover) (target: Target) (from: Pos) : int option =
    match Atlas.travelTicks atlas mover target from with
    | None -> None
    | Some leg when from.Room = target.Room -> Some leg
    | Some leg ->
        Atlas.route atlas target.Avoid target.MaxHops from.Room target.Room
        |> Option.map (fun rooms -> leg + crossingTicks * max 0 (List.length rooms - 2))

let private distance (atlas: Atlas) (a: Pos) (b: Pos) : int option =
    match Pos.range a b with
    | Some r -> Some r
    | None ->
        Atlas.route atlas Set.empty reachHops a.Room b.Room
        |> Option.map (fun rooms -> crossingTicks * (List.length rooms - 1))

/// A body digs from nowhere else, so a harvester's dig falls into the
/// container and a light body keeps off the post.
let private seatsOf (atlas: Atlas) (derived: Derived) (body: Body) (offer: Offer) =
    match offer.Task with
    | Task.Harvest id ->
        match
            Atlas.seats atlas One (rangeOf offer.Task) offer.Target, Map.tryFind id derived.Posts
        with
        | [], _ -> None
        | _, Some post when garrisons body offer -> Some [ post ]
        | seats, Some post -> Some(seats |> List.filter (fun p -> p <> post))
        | seats, None -> Some seats
    | Task.Upgrade _ when drawsBuffer body offer ->
        derived.Grounds
        |> Array.tryFind (fun g -> g.Room = offer.Target.Room)
        |> Option.bind (fun g ->
            if List.isEmpty g.BufferSeats then
                None
            else
                Some g.BufferSeats)
    | Task.Build _
    | Task.Repair _ when drawsBuffer body offer ->
        derived.Grounds
        |> Array.tryPick (fun g -> if g.Room = offer.Target.Room then g.Buffer else None)
        |> Option.map (fun b -> Atlas.seats atlas One 1 b.Pos)
        |> Option.filter (List.isEmpty >> not)
    | _ -> None

/// A seat no other body stands on, so one body on the nearest seat does not
/// hold the crew short of the rest.
let private seatOf
    (stood: HashSet<Pos>)
    (creep: Creep)
    (offer: Offer)
    (seats: Pos list option)
    : Target =
    let free (p: Pos) = p = creep.Pos || not (stood.Contains p)

    let tiles =
        match seats |> Option.map (List.filter free) with
        | Some [] -> seats
        | tiles -> tiles

    match tiles with
    | Some((_ :: _) as tiles) ->
        { targetOf offer with
            Tiles = tiles |> List.map Tiles.ofPos |> Array.ofList
            Range = 0
        }
    | _ -> targetOf offer

let private actOf (task: Task) : CreepAct =
    match task with
    | Task.Harvest id -> CreepAct.Harvest id
    | Task.Withdraw(id, r) -> CreepAct.Withdraw(id, r)
    | Task.Pickup(id, _) -> CreepAct.Pickup id
    | Task.Refill(id, r) -> Transfer(id, r)
    | Task.Build id -> CreepAct.Build id
    | Task.Repair id -> CreepAct.Repair id
    | Task.Upgrade id -> CreepAct.Upgrade id
    | Task.Reserve id -> CreepAct.Reserve id
    | Task.Claim id -> ClaimController id

/// Every body with room and no other resource aboard picks up dropped energy
/// beside it, beside its Task's own act: the engine's pickup conflicts with
/// no other, and a pile no Task reaches decays. Not a harvester, whose dig
/// falls where it stands. `taken`: the bodies whose act is already a pickup.
let private pickups (derived: Derived) (taken: HashSet<string>) : Intent list =
    let picks = ResizeArray<Intent>()

    for b in derived.Bodies do
        if
            b.Free > 0
            && not b.Pattern.IsHarvester
            && not (taken.Contains b.Name)
            && b.Store.Held |> Array.forall (fun (r, _) -> r.IsEnergy)
        then
            match
                derived.Grounds
                |> Array.tryPick (fun g ->
                    g.Piles
                    |> Array.tryFind (fun p -> p.Resource.IsEnergy && Pos.within 1 b.Pos p.Pos))
            with
            | Some p -> picks.Add(Creep(b.Name, CreepAct.Pickup p.Id))
            | None -> ()

    List.ofSeq picks

/// The smallest send: the engine credits `floor(amount × 0.97)`, so a send
/// in whole hundreds loses the 3% and nothing to the rounding.
let private linkBatch = 100

/// The sources' links first, so the hub's never takes their room. A send need
/// not wait to grow: the loss is the same share of any batch.
let private linkSends (ground: Ground) : Intent list =
    match ground.Receiver with
    | None -> []
    | Some receiver ->
        let mutable room = receiver.Free

        [
            for l in ground.Links |> Array.sortBy (fun l -> not l.Role.IsSource) do
                let amount = min l.Energy room / linkBatch * linkBatch

                if l.Id <> receiver.Id && l.Cooldown = 0 && amount > 0 then
                    room <- room - amount
                    Link(l.Id, TransferEnergy(receiver.Id, amount))
        ]

let decide (context: Context) (state: State) : Intent list * State =
    let world = context.World
    let atlas = context.Atlas
    let derived = Derived.ofWorld world atlas context.Declarations [] []
    let offers = Pool.pool derived

    let creeps = Dictionary<string, Creep>()

    for creep in world.Creeps do
        creeps.[creep.Name] <- creep

    let movers = Dictionary<string, Mover>()

    for creep in world.Creeps do
        movers.[creep.Name] <-
            {
                Footprint = One
                Factor = factorOf creep
            }

    let travel (body: Body) (offer: Offer) =
        match movers.TryGetValue body.Name with
        | true, mover -> ticksTo atlas mover (targetOf offer) body.Pos
        | _ -> None

    let assignments = Matcher.matchAll travel offers derived.Bodies state.Assignments

    let byId = Dictionary<string, Offer>()

    for offer in offers do
        byId.[offer.Id] <- offer

    // `fresh`: its successor's walk, empty from a spawn.
    let walk (fresh: bool) (body: Body) (from: Pos) =
        match creeps.TryGetValue body.Name, Map.tryFind body.Name assignments with
        | (true, creep), Some id ->
            match byId.TryGetValue id with
            | true, offer ->
                let mover =
                    if fresh then
                        {
                            Footprint = One
                            Factor =
                                creep.Body
                                |> Array.map (fun p -> p.Kind)
                                |> List.ofArray
                                |> emptyFactor
                        }
                    else
                        movers.[body.Name]

                ticksTo atlas mover (targetOf offer) from
            | _ -> None
        | _ -> None

    let leg (parts: BodyPart list) (from: Pos) (onto: Pos) (range: int) =
        ticksTo
            atlas
            {
                Footprint = One
                Factor = loadedFactor parts
            }
            (targetAt onto range)
            from

    // A dug-out source's holders stay seated until it regenerates.
    let dugOut = HashSet<string>()

    for ground in derived.Grounds do
        for s in ground.Sources do
            if s.Energy = 0 then
                dugOut.Add s.Id |> ignore

    let ready (task: Task) =
        match task with
        | Task.Harvest id -> not (dugOut.Contains id)
        | _ -> true

    let plan = Quota.plan (distance atlas) leg walk derived offers assignments

    let casts =
        plan.Casts
        |> List.map (fun cast -> Spawn(cast.Spawn, SpawnCreep(cast.Body, cast.Name)))

    let stood = HashSet<Pos>(world.Creeps |> Seq.map (fun c -> c.Pos))

    let bodies = Dictionary<string, Body>()

    for body in derived.Bodies do
        bodies.[body.Name] <- body

    let acts = ResizeArray<Intent>()
    let wishes = Dictionary<string, ResizeArray<Wish>>()

    let wishIn (room: string) (wish: Wish) =
        match wishes.TryGetValue room with
        | true, list -> list.Add wish
        | _ -> wishes.[room] <- ResizeArray [ wish ]

    for creep in world.Creeps do
        let mover = movers.[creep.Name]

        match
            Map.tryFind creep.Name assignments
            |> Option.bind (fun id ->
                match byId.TryGetValue id with
                | true, offer -> Some offer
                | _ -> None)
        with
        | Some offer ->
            let body = bodies.[creep.Name]
            let seats = seatsOf atlas derived body offer

            let buffer =
                derived.Grounds
                |> Array.tryPick (fun g -> if g.Room = creep.Pos.Room then g.Buffer else None)

            match buffer with
            | Some b when drawsBuffer body offer && Pos.within 1 creep.Pos b.Pos ->
                acts.Add(Creep(creep.Name, CreepAct.Withdraw(b.Id, Energy)))
            | _ -> ()

            if
                Pos.within (rangeOf offer.Task) creep.Pos offer.Target
                && seats |> Option.forall (List.contains creep.Pos)
                && ready offer.Task
            then
                // The API refuses an upgrade from an empty store before the
                // draw lands.
                if body.Energy > 0 || not (drawsBuffer body offer) then
                    acts.Add(Creep(creep.Name, actOf offer.Task))

            if garrisons body offer && body.Energy > 0 then
                derived.Grounds
                |> Array.tryPick (fun g ->
                    g.Links
                    |> Array.tryFind (fun l ->
                        Option.isSome g.Receiver
                        && l.Role.IsSource
                        && l.Free > 0
                        && Pos.within 1 creep.Pos l.Pos))
                |> Option.iter (fun l -> acts.Add(Creep(creep.Name, Transfer(l.Id, Energy))))

            wishIn
                creep.Pos.Room
                (Plan.toward
                    atlas
                    creep
                    mover
                    (seatOf stood creep offer seats)
                    (weightOf offer.Priority))
        | None -> wishIn creep.Pos.Room (Plan.stay atlas creep mover.Factor 1)

    let moves = ResizeArray<Intent>()

    for KeyValue(room, list) in wishes do
        for name, direction in
            Arbitrate.arbitrate (Atlas.passable atlas room) [||] (List.ofSeq list) do
            moves.Add(Creep(name, Move direction))

    let sends = derived.Grounds |> Seq.collect linkSends |> List.ofSeq

    let taken = HashSet<string>()

    for act in acts do
        match act with
        | Creep(name, CreepAct.Pickup _) -> taken.Add name |> ignore
        | _ -> ()

    casts @ List.ofSeq acts @ pickups derived taken @ sends @ List.ofSeq moves,
    { state with Assignments = assignments }

let economy: Module = { Name = "economy"; Run = decide }
