/// Who is out there (`ARCHITECTURE.md` §6.1).
module Fabot.Core.Fight.Tracker

open System.Collections.Generic
open Fabot.Core
open Fabot.Core.World

type Group =
    {
        Id: int
        Members: Set<string>
        Room: string
        LastSeen: int
        Centre: Pos
    }

[<RequireQualifiedAccess>]
type Event =
    | Newcomer of id: int
    /// Some of `from`'s members stand in `into`, apart from its largest fragment.
    | Split of from: int * into: int
    /// `from` ended inside the older `into`.
    | Merged of into: int * from: int
    | Lost of id: int
    | Dropped of id: int

/// Ranged reach plus one step.
let reach = 4

/// The owner's starting value, to be tuned.
let lostGroupTicks = 30

/// A seam's two tiles are one apart.
let worldOf (pos: Pos) : int * int =
    match Tiles.roomCoords pos.Room with
    | Some(rx, ry) -> rx * 50 + pos.X, ry * 50 + pos.Y
    | None -> pos.X, pos.Y

let linked (a: Pos) (b: Pos) : bool =
    if a.Room = b.Room then
        Pos.within reach a b
    else
        match Tiles.roomCoords a.Room, Tiles.roomCoords b.Room with
        | Some _, Some _ ->
            let ax, ay = worldOf a
            let bx, by = worldOf b
            abs (ax - bx) <= 1 && abs (ay - by) <= 1
        | _ -> false

let private clusters (hostiles: Creep[]) : Creep[][] =
    let sorted = hostiles |> Array.sortWith (fun a b -> compare a.Id b.Id)
    let n = sorted.Length
    let root = Array.init n id
    let armed = sorted |> Array.map (Hostiles.armed sorted)

    let rec find (i: int) =
        if root.[i] = i then
            i
        else
            let r = find root.[i]
            root.[i] <- r
            r

    for i in 0 .. n - 1 do
        if armed.[i] then
            for j in i + 1 .. n - 1 do
                if armed.[j] && linked sorted.[i].Pos sorted.[j].Pos then
                    let a = find i
                    let b = find j

                    if a <> b then
                        root.[max a b] <- min a b

    let byRoot = Dictionary<int, ResizeArray<Creep>>()
    let order = ResizeArray<ResizeArray<Creep>>()

    for i in 0 .. n - 1 do
        let r = find i

        if byRoot.ContainsKey r then
            byRoot.[r].Add sorted.[i]
        else
            let members = ResizeArray<Creep>()
            members.Add sorted.[i]
            byRoot.[r] <- members
            order.Add members

    order.ToArray() |> Array.map (fun members -> members.ToArray())

/// The member nearest the members' mean, so the centre always stands in a seen room.
let private centre (members: Creep[]) : Pos =
    let n = members.Length
    let mutable sx = 0
    let mutable sy = 0

    for m in members do
        let x, y = worldOf m.Pos
        sx <- sx + x
        sy <- sy + y

    let mutable best = members.[0].Pos
    let mutable bestD = System.Int32.MaxValue

    for m in members do
        let x, y = worldOf m.Pos
        let d = max (abs (n * x - sx)) (abs (n * y - sy))

        if d < bestD then
            best <- m.Pos
            bestD <- d

    best

let step (grace: int) (prev: Group list) (hostiles: Creep[]) (tick: int) : Group list * Event list =
    let prev = prev |> List.sortBy (fun g -> g.Id) |> Array.ofList
    let cs = clusters hostiles
    let owner = Dictionary<string, int>()

    for gi in 0 .. prev.Length - 1 do
        for m in prev.[gi].Members do
            owner.[m] <- gi

    let overlap =
        cs
        |> Array.map (fun c ->
            let o = Array.zeroCreate prev.Length

            for m in c do
                if owner.ContainsKey m.Id then
                    let gi = owner.[m.Id]
                    o.[gi] <- o.[gi] + 1

            o)

    let best =
        Array.init prev.Length (fun gi ->
            let mutable b = -1
            let mutable most = 0

            for ci in 0 .. cs.Length - 1 do
                if overlap.[ci].[gi] > most then
                    b <- ci
                    most <- overlap.[ci].[gi]

            b)

    let mutable nextId = (prev |> Array.fold (fun m g -> max m g.Id) 0) + 1

    let groups = ResizeArray<Group>()
    let events = ResizeArray<Event>()

    for ci in 0 .. cs.Length - 1 do
        let mutable id = -1

        for gi in 0 .. prev.Length - 1 do
            if best.[gi] = ci then
                if id < 0 then
                    id <- prev.[gi].Id
                else
                    events.Add(Event.Merged(id, prev.[gi].Id))

        if id < 0 then
            id <- nextId
            nextId <- nextId + 1

        let mutable seenBefore = false

        for gi in 0 .. prev.Length - 1 do
            if overlap.[ci].[gi] > 0 then
                seenBefore <- true

                if best.[gi] <> ci then
                    events.Add(Event.Split(prev.[gi].Id, id))

        if not seenBefore then
            events.Add(Event.Newcomer id)

        let c = centre cs.[ci]

        groups.Add
            {
                Id = id
                Members = cs.[ci] |> Array.map (fun m -> m.Id) |> Set.ofArray
                Room = c.Room
                LastSeen = tick
                Centre = c
            }

    for gi in 0 .. prev.Length - 1 do
        let g = prev.[gi]

        if best.[gi] < 0 then
            if tick - g.LastSeen > grace then
                events.Add(Event.Dropped g.Id)
            else
                groups.Add g

                // A skipped tick misses it; `Dropped` still comes.
                if g.LastSeen = tick - 1 then
                    events.Add(Event.Lost g.Id)

    let groups = groups.ToArray()
    groups |> Array.sortInPlaceBy (fun g -> g.Id)
    List.ofArray groups, List.ofArray (events.ToArray())
