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

let private kinds = (Kind.Other "").Ordinal + 1

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
        Walkable: bool[]
        /// Tiles whose container's rock, else controller, has one already:
        /// one standing off the plan's tile is kept, never doubled.
        Served: bool[]
        /// None once a link serves the source: one standing off the plan's
        /// tile is kept, never doubled.
        Posts: (int * (int * int[]) option)[]
        Buffer: (int * int) option
        /// A solid site there waits on its upgrader as under any creep.
        Seat: bool[]
        /// Every owner's, and sites: CONTROLLER_STRUCTURES counts them all.
        /// By `Kind.Ordinal`.
        Held: int[]
        /// One under a creep is not counted, so it never holds back the rest.
        Pending: int
    }

let censusOf (room: Room) (ours: Creep[]) : Census =
    let solid = Array.create Tiles.count false
    let road = Array.create Tiles.count false
    let rampart = Array.create Tiles.count false
    let site = Array.create Tiles.count false
    let held = Array.zeroCreate<int> kinds
    let occupied = Array.create Tiles.count false

    let walkable =
        Array.init Tiles.count (fun t -> at t room.Terrain &&& Terrain.wall = 0uy)

    for c in Array.append ours room.Hostiles do
        setAt (Tiles.ofPos c.Pos) occupied true

    for s in room.Structures do
        let t = Tiles.ofPos s.Pos

        match s.Kind with
        | Kind.Road -> setAt t road true
        | Kind.Rampart -> setAt t rampart true
        | _ -> setAt t solid true

        if not s.Walkable then
            setAt t walkable false

        setAt s.Kind.Ordinal held (at s.Kind.Ordinal held + 1)

    let mutable pending = 0

    for s in room.Sites do
        setAt (Tiles.ofPos s.Pos) site true
        setAt s.Kind.Ordinal held (at s.Kind.Ordinal held + 1)

        if not (Kind.walkable s.Kind) then
            setAt (Tiles.ofPos s.Pos) walkable false

        if s.Owner.IsOurs then
            if standable s.Kind || not (at (Tiles.ofPos s.Pos) occupied) then
                pending <- pending + 1

    let standing (ordinal: int) =
        let found = ResizeArray<Pos>()

        for i in 0 .. room.Structures.Length - 1 do
            if (at i room.Structures).Kind.Ordinal = ordinal then
                found.Add (at i room.Structures).Pos

        for i in 0 .. room.Sites.Length - 1 do
            if (at i room.Sites).Kind.Ordinal = ordinal then
                found.Add (at i room.Sites).Pos

        found.ToArray()

    let containers = standing Kind.Container.Ordinal

    let rocks =
        Array.append
            (room.Sources |> Array.map (fun s -> s.Pos))
            (room.Minerals |> Array.map (fun m -> m.Pos))

    let served = Array.create Tiles.count false

    room.Controller
    |> Option.filter (fun _ -> containers |> Array.exists (Atlas.isBuffer room))
    |> Option.iter (fun c ->
        for t in Tiles.within Engine.upgradeRange (Tiles.ofPos c.Pos) do
            setAt t served true)

    for rock in rocks do
        if containers |> Array.exists (Pos.within 1 rock) then
            for t in Tiles.within 1 (Tiles.ofPos rock) do
                setAt t served true

    for rock in rocks do
        setAt (Tiles.ofPos rock) walkable false

    room.Controller
    |> Option.iter (fun c -> setAt (Tiles.ofPos c.Pos) walkable false)

    let seat = Array.create Tiles.count false

    let buffer =
        Option.map2
            (fun (c: Controller) (b: Structure) -> Tiles.ofPos c.Pos, b)
            room.Controller
            (Atlas.buffer room)

    for c, b in Option.toArray buffer do
        for t in Atlas.upgraderSeats (fun t -> at t walkable) c (Tiles.ofPos b.Pos) do
            setAt t seat true

    let links = standing Kind.Link.Ordinal

    let unheld (t: int) =
        at t walkable
        && not (at t solid || at t road || at t site)
        && min (Tiles.xOf t) (Tiles.yOf t) >= 2
        && max (Tiles.xOf t) (Tiles.yOf t) < Tiles.side - 2

    let posts =
        room.Sources
        |> Array.choose (fun s ->
            if links |> Array.exists (Pos.within 2 s.Pos) then
                Some(Tiles.ofPos s.Pos, None)
            else
                Atlas.postOf room s.Pos
                |> Option.map (fun (p, _) ->
                    let post = Tiles.ofPos p
                    Tiles.ofPos s.Pos, Some(post, Tiles.around post |> Array.filter unheld)))

    {
        Solid = solid
        Road = road
        Rampart = rampart
        Site = site
        Occupied = occupied
        Walkable = walkable
        Served = served
        Posts = posts
        Buffer =
            buffer
            |> Option.filter (fun (_, b) -> b.Kind.IsContainer)
            |> Option.map (fun (c, b) -> c, Tiles.ofPos b.Pos)
        Seat = seat
        Held = held
        Pending = pending
    }

/// Whether a structure on `t` parts the tiles beside it but across `post`,
/// where a miner works and steps aside for no walker. Beside, not the whole
/// room: a walk round the room prices dearer than a step through the miner.
let private closes (census: Census) (post: int) (t: int) =
    let beside =
        Tiles.around t |> Array.filter (fun n -> n <> post && at n census.Walkable)

    let reached = ResizeArray<int>(Array.truncate 1 beside)
    let mutable i = 0

    while i < reached.Count do
        for n in beside do
            if Tiles.range n reached.[i] = 1 && not (reached.Contains n) then
                reached.Add n

        i <- i + 1

    reached.Count < beside.Length

let private heldOf (census: Census) (kind: Kind) = at kind.Ordinal census.Held

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
    let added = Array.zeroCreate<int> kinds
    let addedOf (kind: Kind) = at kind.Ordinal added

    let taken = HashSet<int>()

    let planned = Array.create Tiles.count false
    let solid = Array.create Tiles.count false

    for i in plan.Items do
        setAt i.Tile planned true

        if not (Kind.walkable i.Kind) then
            setAt i.Tile solid true

    // A source's link stands beside the post its miner works from, which
    // another bot may have set off the plan's container tile. The roads the
    // plan drew there lead to that unbuilt container, and the tile farthest
    // from the anchor lies behind the miner, off the way in. The last open
    // tile is the miner's way in.
    let tileOf (item: Item) =
        match
            census.Posts
            |> Array.tryFind (fun (rock, _) -> item.Kind.IsLink && Tiles.range item.Tile rock <= 2)
        with
        | Some(_, None) -> None
        | None when item.Kind.IsLink && at item.Tile census.Seat && census.Buffer.IsSome ->
            let c, b = census.Buffer.Value
            let walkable t = at t census.Walkable

            let open' t =
                walkable t
                && not (at t census.Solid || at t census.Road || at t census.Site)
                && not (at t planned)

            controllerLink walkable open' (fun t -> t = item.Tile || not (at t solid)) c b
        | Some(_, Some(post, beside)) when
            Tiles.range item.Tile post <> 1 || closes census post item.Tile
            ->
            beside
            |> Array.filter (fun t ->
                plan.Items |> Array.forall (fun i -> i.Tile <> t || i.Kind.IsRoad)
                && not (closes census post t))
            |> Array.sortBy (fun t ->
                plan.Items |> Array.exists (fun i -> i.Tile = t), -(Tiles.range t plan.Anchor), t)
            |> Array.tryHead
            |> Option.filter (fun _ -> beside.Length > 1)
        | _ -> Some item.Tile

    for item in plan.Items |> Array.filter (fun i -> i.Level <= level) do
        match tileOf item with
        | Some t when
            placed.Count < room
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
            ->
            taken.Add t |> ignore
            setAt item.Kind.Ordinal added (addedOf item.Kind + 1)

            placed.Add
                {
                    X = Tiles.xOf t
                    Y = Tiles.yOf t
                    Kind = item.Kind
                }
        | _ -> ()

    List.ofSeq placed
