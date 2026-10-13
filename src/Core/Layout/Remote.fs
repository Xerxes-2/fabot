/// A declared outpost has no spawn to anchor a plan on, so its container and
/// road are placed off the haul's own walk home.
module Fabot.Core.Layout.Remote

open System.Collections.Generic
open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Grid
open Fabot.Core.Intents

let private hauler: Mover =
    {
        Footprint = One
        Factor = { Weight = 2; Moves = 1 }
    }

/// Bounds a walk that never arrives.
let private longest = (Tick.reachHops + 1) * 2 * Tiles.side

/// The engine refuses a site under anyone else's hold.
let private ours (me: string) (room: Room) =
    room.Controller
    |> Option.forall (fun c ->
        match c.Owner with
        | Ours -> true
        | Nobody -> c.Reservation |> Option.forall (fun (user, _) -> user = me)
        | Theirs _ -> false)

/// The engine builds nothing on the border.
let private walk (atlas: Atlas) (target: Target) (from: Pos) : Pos list =
    let path = ResizeArray<Pos>()
    let mutable room = from.Room
    let mutable tile = Tiles.ofPos from
    let mutable field = Atlas.field atlas hauler target room

    while field.IsSome && path.Count < longest do
        match Field.step field.Value tile with
        | None -> field <- None
        | Some direction ->
            let next = Tiles.neighbour tile direction

            if Tiles.onEdge next then
                match Tiles.roomToward room (Tiles.exitOf next) with
                | Some onto ->
                    room <- onto
                    tile <- Tiles.landing next
                    field <- Atlas.field atlas hauler target room
                | None -> field <- None
            else
                tile <- next
                path.Add(Tiles.toPos room tile)

    List.ofSeq path

/// v1's seat: the shortest walk out toward home. The owner's rule: the walk
/// is paved once its container stands.
let sites
    (atlas: Atlas)
    (world: World)
    (outposts: Tick.Outpost list)
    (planned: Intent list)
    (budget: int)
    : Intent list =
    let intents = ResizeArray<Intent>()
    let mutable budget = budget
    let taken = Dictionary<string, HashSet<int>>()
    let pending = Dictionary<string, int>()
    let rooms = Dictionary<string, Room>()

    let colonies = Tick.colonies world

    for room in world.Rooms do
        if
            Array.contains room.Name colonies
            || outposts |> List.exists (fun o -> o.Room = room.Name) && ours world.Me room
        then
            rooms.[room.Name] <- room

    let open' (room: Room) =
        if not (taken.ContainsKey room.Name) then
            let tiles = HashSet<int>()

            for s in room.Structures do
                if not s.Kind.IsRampart then
                    tiles.Add(Tiles.ofPos s.Pos) |> ignore

            for s in room.Sites do
                tiles.Add(Tiles.ofPos s.Pos) |> ignore

            let mutable n = room.Sites |> Array.sumBy (fun s -> if s.Owner.IsOurs then 1 else 0)

            for i in planned do
                match i with
                | Room(r, CreateSite(x, y, _)) when r = room.Name ->
                    tiles.Add(Tiles.index x y) |> ignore
                    n <- n + 1
                | _ -> ()

            taken.[room.Name] <- tiles
            pending.[room.Name] <- n

        taken.[room.Name]

    let place (pos: Pos) (kind: Kind) =
        match rooms.TryGetValue pos.Room with
        | true, room when budget > 0 ->
            let tiles = open' room
            let tile = Tiles.ofPos pos

            if pending.[room.Name] < Sites.sitesInFlight && tiles.Add tile then
                pending.[room.Name] <- pending.[room.Name] + 1
                budget <- budget - 1
                intents.Add(Room(room.Name, CreateSite(pos.X, pos.Y, kind)))
        | _ -> ()

    let sinkFor (room: string) =
        colonies
        |> Array.map (fun c ->
            Atlas.route atlas Set.empty Tick.reachHops room c
            |> Option.map List.length
            |> Option.defaultValue System.Int32.MaxValue,
            c)
        |> Array.sortBy fst
        |> Array.tryHead
        |> Option.bind (fun (_, c) ->
            rooms.[c].Structures
            |> Array.tryFind (fun s -> s.Kind.IsStorage && s.Owner.IsOurs)
            |> Option.map (fun s -> s.Pos)
            |> Option.orElse (
                world.Spawns
                |> Array.tryFind (fun s -> s.Pos.Room = c)
                |> Option.map (fun s -> s.Pos)
            ))

    for o in outposts do
        match rooms.TryGetValue o.Room, sinkFor o.Room with
        | (true, room), Some sink ->
            let home =
                {
                    Room = sink.Room
                    Tiles = [| Tiles.ofPos sink |]
                    Range = 1
                    Avoid = Set.empty
                    MaxHops = Tick.reachHops
                }

            for s in room.Sources do
                match Atlas.postOf room s.Pos with
                | Some(post, Some _) ->
                    for p in walk atlas home post do
                        place p Kind.Road
                | Some(_, None) -> ()
                | None ->
                    let tiles = open' room

                    let seat =
                        Tiles.within 1 (Tiles.ofPos s.Pos)
                        |> Array.filter (fun t ->
                            t <> Tiles.ofPos s.Pos
                            && not (Tiles.onEdge t)
                            && not (tiles.Contains t)
                            && not (Terrain.isWall room.Terrain (Tiles.xOf t) (Tiles.yOf t)))
                        |> Array.choose (fun t ->
                            Atlas.travelTicks atlas hauler home (Tiles.toPos room.Name t)
                            |> Option.map (fun ticks -> ticks, t))
                        |> Array.sortBy (fun (ticks, t) -> ticks * Tiles.count + t)
                        |> Array.tryHead

                    seat
                    |> Option.iter (fun (_, t) -> place (Tiles.toPos room.Name t) Kind.Container)
        | _ -> ()

    List.ofSeq intents
