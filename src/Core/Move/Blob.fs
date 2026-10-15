/// A set that walks as one: one field priced at its slowest body, each member
/// kept near the median member along the walk.
module Fabot.Core.Move.Blob

open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Grid

/// ⌈(n−1)/2⌉: less, and a file of n deadlocks in a corridor.
let radius (n: int) = n / 2

let private slowest (members: Creep[]) : Mover =
    let factors = members |> Array.map factorOf

    {
        Footprint = One
        Factor =
            factors
            |> Array.filter (fun f -> f.Moves > 0)
            |> Array.sortBy (fun f ->
                -(stepTicks f Engine.plainWeight),
                -(stepTicks f Engine.swampWeight),
                -(stepTicks f Engine.roadWeight))
            |> Array.tryHead
            |> Option.defaultValue factors.[0]
    }

/// None for a body that cannot move.
let private fatigueTicks (creep: Creep) =
    let moves = (factorOf creep).Moves

    if moves = 0 then
        None
    else
        Some((creep.Fatigue + 2 * moves - 1) / (2 * moves))

let wishes (atlas: Atlas) (target: Target) (weight: int) (members: Creep[]) : Wish list =
    let mover = slowest members
    let fieldIn (room: string) = Atlas.field atlas mover target room

    // Out of another room: the ticks to its exit, then the least on from a
    // landing, so a walk's every step is one tick nearer.
    let rec left (hops: int) (room: string) (tile: int) : int =
        match fieldIn room |> Option.bind (fun field -> Field.ticks field tile) with
        | None -> unreached
        | Some ticks when room = target.Room -> ticks
        | Some _ when hops = 0 -> unreached
        | Some ticks ->
            match beyond hops room with
            | rest when rest = unreached -> unreached
            | rest -> ticks + rest

    and beyond (hops: int) (room: string) : int =
        match fieldIn room with
        | None -> unreached
        | Some field ->
            [ Tiles.top; Tiles.right; Tiles.bottom; Tiles.left ]
            |> List.collect (fun exit ->
                match Tiles.roomToward room exit with
                | Some next ->
                    Tiles.exitTiles exit
                    |> Array.filter (fun tile -> Field.ticks field tile = Some 0)
                    |> Array.map (fun tile -> left (hops - 1) next (Tiles.landing tile))
                    |> List.ofArray
                | None -> [])
            |> List.fold min unreached

    let lags =
        members
        |> Array.map (fun c ->
            match left target.MaxHops c.Pos.Room (Tiles.ofPos c.Pos), fatigueTicks c with
            | ticks, Some resting when ticks <> unreached -> ticks + resting
            | _ -> unreached)

    let lag (creep: Creep) =
        lags.[Array.findIndex (fun (c: Creep) -> c.Name = creep.Name) members]

    let ranked = members |> Array.sortBy (fun c -> lag c, c.Name)
    let anchor = ranked.[(ranked.Length - 1) / 2]
    let r = radius members.Length

    let walks = members |> Array.map (fun c -> Plan.toward atlas c mover target weight)

    let anchorWish =
        walks.[Array.findIndex (fun (c: Creep) -> c.Name = anchor.Name) members]

    // Ticks behind and out of reach of its step: one behind on terrain alone,
    // in reach, closes no faster for the anchor waiting.
    let lagging =
        members
        |> Array.exists (fun c ->
            lag c <> unreached
            && lag c - lag anchor > r + 1
            && Tiles.seamRange c.Pos (Tiles.toPos anchor.Pos.Room anchorWish.Head)
               |> Option.forall (fun range -> range > r + 1))

    let headTile = if lagging then anchorWish.At else anchorWish.Head
    let head = Tiles.toPos anchor.Pos.Room headTile

    let fits (creep: Creep) (tile: int) =
        creep.Name = anchor.Name
        || Tiles.seamRange (Tiles.toPos creep.Pos.Room tile) head
           |> Option.exists (fun range -> range <= r)
        || lag creep > lag anchor

    // A step into swamp falls behind: a walker sidesteps no slower than its
    // head, one waiting is shoved at most a plain step back, so one behind passes.
    let keeps (slack: int) (creep: Creep) (tile: int) =
        match fieldIn creep.Pos.Room with
        | Some field ->
            match Field.ticks field (Tiles.ofPos creep.Pos) with
            | Some here when here > 0 ->
                Field.onto field tile |> Option.exists (fun ticks -> ticks <= here + slack)
            | _ -> true
        | None -> true

    let stepBack = 2 * stepTicks mover.Factor Engine.plainWeight

    [
        for creep, wish in Array.zip members walks do
            let waits =
                if creep.Name = anchor.Name then
                    lagging
                else
                    not (Wish.stays wish || fits creep wish.Head)

            if waits then
                let stay = Plan.stay atlas creep mover.Factor weight

                { stay with
                    Tail =
                        stay.Tail
                        |> Array.filter (fun tile -> fits creep tile && keeps stepBack creep tile)
                }
            else
                { wish with
                    Tail =
                        wish.Tail
                        |> Array.filter (fun tile -> fits creep tile && keeps 0 creep tile)
                }
    ]
