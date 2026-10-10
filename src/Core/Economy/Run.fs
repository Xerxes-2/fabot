/// The economy as one module of the tick: derive, pool, match, cast, act, and
/// hand every creep's wish to its room's movement arbitration.
module Fabot.Core.Economy.Run

open System.Collections.Generic
open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Grid
open Fabot.Core.Intents
open Fabot.Core.State
open Fabot.Core.Tick
open Fabot.Core.Move
open Fabot.Core.Economy.Task
open Fabot.Core.Economy.Derived
open Fabot.Core.Economy.Pool

/// The room crossings a walk may plan through: a bound the route search
/// needs, and the economy works no room further than its declarations.
let private reachHops = 3

/// Ticks a crossing into one more room is priced at, past the leg in this
/// room: a room's side walked on plain. The Atlas prices this room's leg
/// only; this keeps a far task from reading as near.
let private crossingTicks = Engine.roomSide

/// A Task's push in the arbitration: one more than the ranks it stands above
/// the bottom of the scale, so a dearer Task's body is the heavier.
let private weightOf (priority: int) = max 1 (Price.bottomRank - priority + 1)

let private targetOf (offer: Offer) : Target =
    {
        Room = offer.Target.Room
        Tiles = [| Tiles.ofPos offer.Target |]
        Range = rangeOf offer.Task
        Avoid = Set.empty
        MaxHops = reachHops
    }

/// Ticks from a tile to a target: this room's leg from the Atlas, and a
/// room's side for each crossing past the first.
let private ticksTo (atlas: Atlas) (mover: Mover) (target: Target) (from: Pos) : int option =
    match Atlas.travelTicks atlas mover target from with
    | None -> None
    | Some leg when from.Room = target.Room -> Some leg
    | Some leg ->
        Atlas.route atlas target.Avoid target.MaxHops from.Room target.Room
        |> Option.map (fun rooms -> leg + crossingTicks * max 0 (List.length rooms - 2))

/// Tiles between two positions for the quota's nearest-colony questions:
/// Chebyshev in one room, a room's side a crossing between rooms.
let private distance (atlas: Atlas) (a: Pos) (b: Pos) : int option =
    if a.Room = b.Room then
        Some(max (abs (a.X - b.X)) (abs (a.Y - b.Y)))
    else
        Atlas.route atlas Set.empty reachHops a.Room b.Room
        |> Option.map (fun rooms -> crossingTicks * (List.length rooms - 1))

/// The act a Task's holder does once it stands within the Task's range.
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

let private within (range: int) (a: Pos) (b: Pos) =
    a.Room = b.Room && max (abs (a.X - b.X)) (abs (a.Y - b.Y)) <= range

let decide (context: Context) (state: State) : Intent list * State =
    let world = context.World
    let atlas = context.Atlas
    let derived = Derived.ofWorld world [] []
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

    let plan = Quota.plan (distance atlas) derived offers assignments

    let casts =
        plan.Casts
        |> List.map (fun cast -> Spawn(cast.Spawn, SpawnCreep(cast.Body, cast.Name)))

    let byId = Dictionary<string, Offer>()

    for offer in offers do
        byId.[offer.Id] <- offer

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
            if within (rangeOf offer.Task) creep.Pos offer.Target then
                acts.Add(Creep(creep.Name, actOf offer.Task))

            wishIn
                creep.Pos.Room
                (Plan.toward atlas creep mover (targetOf offer) (weightOf offer.Priority))
        | None -> wishIn creep.Pos.Room (Plan.stay atlas creep mover.Factor 1)

    let moves = ResizeArray<Intent>()

    for KeyValue(room, list) in wishes do
        let held =
            world.Rooms
            |> Array.tryFind (fun r -> r.Name = room)
            |> Option.map (fun r -> r.Hostiles |> Array.map (fun h -> Tiles.ofPos h.Pos))
            |> Option.defaultValue [||]

        for name, direction in
            Arbitrate.arbitrate (Atlas.passable atlas room) held (List.ofSeq list) do
            moves.Add(Creep(name, Move direction))

    casts @ List.ofSeq acts @ List.ofSeq moves, { state with Assignments = assignments }

/// The economy as the tick runs it.
let economy: Module = { Name = "economy"; Run = decide }
