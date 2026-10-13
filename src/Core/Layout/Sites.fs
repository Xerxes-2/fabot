module Fabot.Core.Layout.Sites

open System.Collections.Generic
open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Tiles
open Fabot.Core.Grid
open Fabot.Core.Layout.Shape
open Fabot.Core.Layout.Plan

/// A level's first batch of extensions, and few enough that the builders'
/// energy goes to the plan's first items before its later ones.
let sitesInFlight = 5

/// A site of any other kind under a creep is placed but not built while the
/// creep stays.
let private standable (kind: Kind) =
    kind.IsRoad || kind.IsContainer || kind.IsRampart

[<Struct; NoEquality; NoComparison>]
type Placement = { X: int; Y: int; Kind: Kind }

[<NoEquality; NoComparison>]
type Census =
    {
        Solid: bool[]
        Road: bool[]
        Rampart: bool[]
        Site: bool[]
        Occupied: bool[]
        /// Tiles whose container's rock, else controller, has one already:
        /// one standing off the plan's tile is kept, never doubled.
        Served: bool[]
        /// Every owner's, and sites: CONTROLLER_STRUCTURES counts them all.
        Held: Dictionary<Kind, int>
        /// One under a creep is not counted, so it never holds back the rest.
        Pending: int
    }

let censusOf (room: Room) (ours: Creep[]) : Census =
    let solid = Array.create Tiles.count false
    let road = Array.create Tiles.count false
    let rampart = Array.create Tiles.count false
    let site = Array.create Tiles.count false
    let held = Dictionary<Kind, int>()
    let occupied = Array.create Tiles.count false

    for c in Array.append ours room.Hostiles do
        setAt (Tiles.ofPos c.Pos) occupied true

    let add (kind: Kind) =
        held.[kind] <-
            (match held.TryGetValue kind with
             | true, n -> n + 1
             | _ -> 1)

    for s in room.Structures do
        let t = Tiles.ofPos s.Pos

        match s.Kind with
        | Kind.Road -> setAt t road true
        | Kind.Rampart -> setAt t rampart true
        | _ -> setAt t solid true

        add s.Kind

    let mutable pending = 0

    for s in room.Sites do
        setAt (Tiles.ofPos s.Pos) site true
        add s.Kind

        if s.Owner.IsOurs then
            if standable s.Kind || not (at (Tiles.ofPos s.Pos) occupied) then
                pending <- pending + 1

    let containers =
        [|
            for s in room.Structures do
                if s.Kind.IsContainer then
                    yield s.Pos
            for s in room.Sites do
                if s.Kind.IsContainer then
                    yield s.Pos
        |]

    let served = Array.create Tiles.count false

    room.Controller
    |> Option.filter (fun _ -> containers |> Array.exists (Atlas.isBuffer room))
    |> Option.iter (fun c ->
        for t in Tiles.within Engine.upgradeRange (Tiles.ofPos c.Pos) do
            setAt t served true)

    for rock in
        Array.append
            (room.Sources |> Array.map (fun s -> s.Pos))
            (room.Minerals |> Array.map (fun m -> m.Pos)) do
        let kept = containers |> Array.exists (Pos.within 1 rock)

        for t in Tiles.within 1 (Tiles.ofPos rock) do
            setAt t served kept

    {
        Solid = solid
        Road = road
        Rampart = rampart
        Site = site
        Occupied = occupied
        Served = served
        Held = held
        Pending = pending
    }

let private heldOf (census: Census) (kind: Kind) =
    match census.Held.TryGetValue kind with
    | true, n -> n
    | _ -> 0

/// Below the haul's level no road or container is due, so only a kind short
/// of its allowance can be placed; false lets the caller skip the plan.
let mayPlace (census: Census) (level: int) =
    level >= haulLevel
    || Kind.Link :: Kind.Extractor :: sized
       |> List.exists (fun kind ->
           heldOf census kind < Engine.controllerStructures (Kind.name kind) level)

/// The engine stacks a road only with a rampart, any other kind only with a
/// road or a rampart.
let place (census: Census) (level: int) (plan: Plan) (budget: int) : Placement list =
    let room = min budget (sitesInFlight - census.Pending)
    let placed = ResizeArray<Placement>()
    let added = Dictionary<Kind, int>()

    let addedOf (kind: Kind) =
        match added.TryGetValue kind with
        | true, n -> n
        | _ -> 0

    let taken = HashSet<int>()

    for item in plan.Items do
        let t = item.Tile

        if
            placed.Count < room
            && item.Level <= level
            && not (at t census.Site)
            && (if item.Kind.IsRampart then
                    at t census.Solid && not (at t census.Rampart)
                else
                    not (at t census.Solid))
            && (standable item.Kind || not (at t census.Occupied))
            && not (taken.Contains t)
            && not (item.Kind.IsContainer && at t census.Served)
            && not (item.Kind.IsRoad && at t census.Road)
            && heldOf census item.Kind + addedOf item.Kind < Engine.controllerStructures
                (Kind.name item.Kind)
                level
        then
            taken.Add t |> ignore
            added.[item.Kind] <- addedOf item.Kind + 1

            placed.Add
                {
                    X = Tiles.xOf t
                    Y = Tiles.yOf t
                    Kind = item.Kind
                }

    List.ofSeq placed
