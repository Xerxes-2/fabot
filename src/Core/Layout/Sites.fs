/// The construction sites a plan still wants this tick: its items in plan
/// order, each once its level and the engine's allowance admit it and its
/// tile is clear, within the caps. An item that cannot go down is skipped and
/// never holds back the ones after it.
module Fabot.Core.Layout.Sites

open System.Collections.Generic
open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Grid
open Fabot.Core.Layout.Shape
open Fabot.Core.Layout.Plan

/// The sites a room holds buildable at once: a level's first batch of
/// extensions, and few enough that the builders' energy goes to the plan's
/// first items before its later ones. A named safety margin.
let sitesInFlight = 5

/// Whether a creep may stand on a kind's tile: a site of any other kind
/// under a creep is placed but not built while the creep stays.
let private standable (kind: Kind) =
    kind = Kind.Road || kind = Kind.Container || kind = Kind.Rampart

/// One site to place: tile and kind.
[<Struct; NoEquality; NoComparison>]
type Placement = { X: int; Y: int; Kind: Kind }

/// What stands in a colony room as the sites read it.
[<NoEquality; NoComparison>]
type Census =
    {
        /// Per tile: a structure stands that no other may share (any kind
        /// but a road or a rampart, any owner).
        Solid: bool[]
        /// Per tile: a road stands.
        Road: bool[]
        /// Per tile: a rampart stands, any owner's.
        Rampart: bool[]
        /// Per tile: a construction site stands, any owner's.
        Site: bool[]
        /// Per tile: a creep stands, any owner's. A site under a creep can
        /// be placed but not built while it stays.
        Occupied: bool[]
        /// Our structures and sites of each kind: what CONTROLLER_STRUCTURES
        /// counts.
        Held: Dictionary<Kind, int>
        /// Our sites in this room a builder can build now: one under a creep
        /// is not counted, so it never holds back the rest.
        Pending: int
    }

/// The census of one room, with our creeps standing in it.
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

        if s.Owner = Ours then
            add s.Kind

    let mutable pending = 0

    for s in room.Sites do
        setAt (Tiles.ofPos s.Pos) site true

        if s.Owner = Ours then
            add s.Kind

            if standable s.Kind || not (at (Tiles.ofPos s.Pos) occupied) then
                pending <- pending + 1

    {
        Solid = solid
        Road = road
        Rampart = rampart
        Site = site
        Occupied = occupied
        Held = held
        Pending = pending
    }

let private heldOf (census: Census) (kind: Kind) =
    match census.Held.TryGetValue kind with
    | true, n -> n
    | _ -> 0

/// Whether the census leaves `place` anything to place at `level`: below
/// the haul's level no road or container is due, so only a kind short of
/// its allowance can be. False lets the caller skip the plan.
let mayPlace (census: Census) (level: int) =
    level >= haulLevel
    || Kind.Link :: Kind.Extractor :: sized
       |> List.exists (fun kind ->
           heldOf census kind < Engine.controllerStructures (Kind.name kind) level)

/// The sites to place at controller `level` for `plan`, at most `budget` of
/// them (the world's MAX_CONSTRUCTION_SITES left) and at most
/// `sitesInFlight` pending in the room, in plan order. An item is skipped
/// when its level is not reached, its kind is at its allowance, or its tile
/// holds a site or a structure it cannot share (a road shares only a
/// rampart; any other kind a road or a rampart), or, for a kind no creep
/// stands on, a creep. A rampart goes only over the structure it covers,
/// standing.
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
            && (if item.Kind = Kind.Rampart then
                    at t census.Solid && not (at t census.Rampart)
                else
                    not (at t census.Solid))
            && (standable item.Kind || not (at t census.Occupied))
            && not (taken.Contains t)
            && not (item.Kind = Kind.Road && at t census.Road)
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
