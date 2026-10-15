/// What a fight body does this tick (`ARCHITECTURE.md` §6.4): a strike and a
/// wish, read off the plan and what stands in its room. From v1's Emitter and
/// Resolver step functions.
module Fabot.Core.Fight.Tactics

open System.Collections.Generic
open Fabot.Core
open Fabot.Core.Intents
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Move
open Fabot.Core.Fight.Shape

[<NoEquality; NoComparison>]
type Sight =
    {
        Atlas: Atlas
        Hostiles: Dictionary<string, Creep[]>
        Ramparts: Dictionary<string, Pos[]>
        Homes: Pos[]
    }

/// Above every economy wish in the room's one arbitration: a fight step is
/// not shoved for a haul.
let weight = 1000

let private live (kind: BodyPart) (c: Creep) =
    c.Body
    |> Array.sumBy (fun p -> if p.Hits > 0 && samePart p.Kind kind then 1 else 0)

let private hostilesIn (sight: Sight) (room: string) =
    match sight.Hostiles.TryGetValue room with
    | true, hs -> hs
    | _ -> [||]

let private ranked (sight: Sight) (order: string list) (creep: Creep) : Creep list =
    let near = hostilesIn sight creep.Pos.Room

    let first =
        order |> List.choose (fun id -> near |> Array.tryFind (fun h -> h.Id = id))

    first
    @ (near
       |> Array.filter (fun h -> not (List.contains h.Id order))
       |> Array.sortBy (fun h -> h.Id)
       |> List.ofArray)

let strike (sight: Sight) (order: string list) (creep: Creep) : CreepAct option =
    let targets = ranked sight order creep

    match
        (if live Attack creep > 0 then
             targets |> List.tryFind (fun h -> Pos.within Engine.meleeRange creep.Pos h.Pos)
         else
             None)
    with
    | Some h -> Some(AttackCreep h.Id)
    | None when live RangedAttack creep > 0 ->
        match targets |> List.filter (fun h -> Pos.within Engine.rangedRange creep.Pos h.Pos) with
        | [] -> None
        | head :: _ as inReach ->
            let volley =
                inReach
                |> List.sumBy (fun h ->
                    Pos.range creep.Pos h.Pos
                    |> Option.map Engine.massAttackPowerAt
                    |> Option.defaultValue 0)

            if volley > Engine.rangedAttackPower then
                Some RangedMassAttack
            else
                Some(RangedAttackCreep head.Id)
    | None -> None

let private moverOf (creep: Creep) : Mover =
    {
        Footprint = Grid.One
        Factor = Grid.factorOf creep
    }

let private toward (sight: Sight) (creep: Creep) (at: Pos) (range: int) : Wish =
    Plan.toward
        sight.Atlas
        creep
        (moverOf creep)
        {
            Room = at.Room
            Tiles = [| Tiles.ofPos at |]
            Range = range
            Avoid = Set.empty
            MaxHops = Tick.reachHops
        }
        weight

let private hold (sight: Sight) (creep: Creep) : Wish =
    Plan.stay sight.Atlas creep (moverOf creep).Factor weight

let private aim (sight: Sight) (order: string list) (centre: Pos) (creep: Creep) : Pos =
    match ranked sight order creep with
    | head :: _ -> head.Pos
    | [] -> centre

let close (sight: Sight) (order: string list) (centre: Pos) (creep: Creep) =
    strike sight order creep, toward sight creep (aim sight order centre creep) Engine.meleeRange

let pursue = close

/// How far `kind` of `h` strikes next tick, a step nearer; -1 for none.
let private reach (kind: BodyPart) (h: Creep) =
    match kind, live kind h with
    | _, 0 -> -1
    | RangedAttack, _ -> Engine.rangedRange + min 1 (live Move h)
    | _ -> Engine.meleeRange + min 1 (live Move h)

let private stepTo (creep: Creep) (tile: int) : Wish =
    {
        Creep = creep.Name
        Footprint = Grid.One
        At = Tiles.ofPos creep.Pos
        Head = tile
        Onward = None
        Tail = [||]
        Working = [||]
        Weight = weight
        Moves = true
    }

/// v1's kiteStep without its threat maps: off every swing's next reach, else
/// to a shot's range of the head.
let kite (sight: Sight) (order: string list) (centre: Pos) (creep: Creep) =
    let swings =
        hostilesIn sight creep.Pos.Room
        |> Array.filter (fun h -> Pos.within (reach Attack h) creep.Pos h.Pos)

    let gap (tile: int) =
        let at = Tiles.toPos creep.Pos.Room tile

        swings
        |> Array.fold (fun least h -> min least (Pos.range at h.Pos |> Option.defaultValue 99)) 99

    let here = Tiles.ofPos creep.Pos

    let wish =
        if swings.Length = 0 then
            toward sight creep (aim sight order centre creep) Engine.rangedRange
        else
            let step =
                Tiles.around here
                |> Array.filter (fun t ->
                    not (Tiles.onEdge t) && Atlas.passable sight.Atlas creep.Pos.Room t)
                |> Array.sortBy (fun t -> -(gap t), t)
                |> Array.tryHead
                |> Option.filter (fun t -> gap t > gap here)

            match step with
            | Some t when Plan.canMove creep (moverOf creep).Factor -> stepTo creep t
            | _ -> hold sight creep

    strike sight order creep, wish

/// The raid's strikes onto `at` next tick; none outside the raid's room, which
/// costs a raid a tick to follow into.
let private threat (raid: Creep[]) (at: Pos) =
    raid
    |> Array.sumBy (fun h ->
        (if Pos.within (reach RangedAttack h) at h.Pos then
             Engine.rangedAttackPower * live RangedAttack h
         else
             0)
        + (if Pos.within (reach Attack h) at h.Pos then
               Engine.attackPower * live Attack h
           else
               0))

/// A Doomed body, whose full hits fall within the line, fights on: stepping
/// out cannot save it.
let private rescued (raid: Creep[]) (creep: Creep) =
    let focus =
        (raid
         |> Array.sumBy (fun h ->
             Engine.rangedAttackPower * live RangedAttack h
             + Engine.attackPower * live Attack h))
        - Engine.healPower * live Heal creep

    let ticksOut =
        1
        + (raid
           |> Array.fold (fun most h -> max most (reach RangedAttack h) |> max (reach Attack h)) 0)

    focus > 0 && creep.Hits < ticksOut * focus && creep.HitsMax > ticksOut * focus

/// Only while a mate fights on: the gate never empties the line.
let exposed (sight: Sight) (raidRoom: string) (members: Creep[]) (creep: Creep) =
    let raid = hostilesIn sight raidRoom

    rescued raid creep
    && members |> Array.exists (fun m -> m.Name <> creep.Name && not (rescued raid m))

/// To the nearest tile out of the raid's next reach (an exit hands the body to
/// the next room), beside the most mates that heal it.
let stepBack (sight: Sight) (raidRoom: string) (members: Creep[]) (creep: Creep) : Wish =
    let room = creep.Pos.Room
    let raid = hostilesIn sight raidRoom

    // A body crosses an exit by stepping onto it, not by standing on it.
    let crosses (t: int, first: int) = first >= 0 && Tiles.onEdge t

    let safe (t: int, first: int) =
        room <> raidRoom || crosses (t, first) || threat raid (Tiles.toPos room t) = 0

    let apart (t: int) =
        members
        |> Array.filter (fun m -> m.Name <> creep.Name && live Heal m > 0)
        |> Array.sumBy (fun m -> Pos.range (Tiles.toPos room t) m.Pos |> Option.defaultValue 9)

    let rec search (ring: (int * int) list) (seen: Set<int>) depth =
        match ring |> List.filter safe with
        | [] when depth = 0 || ring.IsEmpty -> None
        | [] ->
            let next =
                ring
                |> List.filter (crosses >> not)
                |> List.collect (fun (t, first) ->
                    Tiles.around t
                    |> Array.filter (fun n ->
                        not (seen.Contains n) && Atlas.passable sight.Atlas room n)
                    |> Array.map (fun n -> n, (if first < 0 then n else first))
                    |> List.ofArray)
                |> List.distinctBy fst

            search next (next |> List.fold (fun s (t, _) -> Set.add t s) seen) (depth - 1)
        | found -> found |> List.minBy (fun (t, _) -> apart t, t) |> snd |> Some

    let here = Tiles.ofPos creep.Pos

    match search [ here, -1 ] (Set.singleton here) (2 * Engine.rangedRange) with
    | Some t when t >= 0 && Plan.canMove creep (moverOf creep).Factor -> stepTo creep t
    | _ -> hold sight creep

/// To a shot's range of the group, as one.
let blob (sight: Sight) (centre: Pos) (members: Creep[]) : Wish list =
    Blob.wishes
        sight.Atlas
        {
            Room = centre.Room
            Tiles = [| Tiles.ofPos centre |]
            Range = Engine.rangedRange
            Avoid = Set.empty
            MaxHops = Tick.reachHops
        }
        weight
        members

let holdFast (sight: Sight) (order: string list) (creep: Creep) =
    strike sight order creep, hold sight creep

/// Onto the nearest of our ramparts, striking what comes; where none stands,
/// where it stands.
let rampart (sight: Sight) (order: string list) (creep: Creep) =
    let ramparts =
        match sight.Ramparts.TryGetValue creep.Pos.Room with
        | true, rs -> rs
        | _ -> [||]

    let wish =
        if ramparts |> Array.exists (fun p -> p = creep.Pos) then
            hold sight creep
        else
            match
                ramparts
                |> Array.sortBy (fun p -> Pos.range creep.Pos p |> Option.defaultValue 99, p.X, p.Y)
                |> Array.tryHead
            with
            | Some p -> toward sight creep p 0
            | None -> hold sight creep

    strike sight order creep, wish

let retreat (sight: Sight) (creep: Creep) : CreepAct option * Wish =
    let home =
        match sight.Homes |> Array.filter (fun p -> p.Room = creep.Pos.Room) with
        | [||] ->
            let tree = Atlas.tree sight.Atlas Tick.reachHops creep.Pos.Room

            sight.Homes
            |> Array.choose (fun p ->
                Atlas.routeIn tree creep.Pos.Room p.Room
                |> Option.map (fun rooms -> List.length rooms, p))
            |> Array.sortBy (fun (hops, p) -> hops, p.Room, p.X, p.Y)
            |> Array.tryHead
            |> Option.map snd
        | here -> here |> Array.sortBy (fun p -> p.X, p.Y) |> Array.tryHead

    None,
    match home with
    | Some p -> toward sight creep p 2
    | None -> hold sight creep

/// v1's heal reflex: a hurt healer heals itself, else the deepest wound beside
/// it, else one within three at the ranged rate; each heal counts off what it
/// closes. The engine keeps one of a swing and a heal, and one of a shot and a
/// ranged heal: a swinging body does not heal, a shooting one heals beside it.
let heals
    (healers: Creep[])
    (patients: Creep[])
    (swinging: HashSet<string>)
    (shooting: HashSet<string>)
    : Intent list =
    let owed = Dictionary<string, int>()

    for p in patients do
        owed.[p.Name] <- p.HitsMax - p.Hits

    let acts = ResizeArray<Intent>()

    for healer in healers |> Array.sortBy (fun c -> c.Name) do
        let parts = live Heal healer

        if parts > 0 && not (swinging.Contains healer.Name) then
            let patient =
                if owed.[healer.Name] > 0 then
                    Some(healer, 0)
                else
                    patients
                    |> Array.choose (fun p ->
                        match Pos.range healer.Pos p.Pos with
                        | Some r when
                            r
                            <= (if shooting.Contains healer.Name then
                                    Engine.meleeRange
                                else
                                    Engine.rangedRange)
                            && owed.[p.Name] > 0
                            ->
                            Some(p, r)
                        | _ -> None)
                    |> Array.sortBy (fun (p, r) -> r > Engine.meleeRange, -owed.[p.Name], p.Name)
                    |> Array.tryHead

            match patient with
            | Some(p, r) when r <= Engine.meleeRange ->
                owed.[p.Name] <- owed.[p.Name] - parts * Engine.healPower
                acts.Add(Creep(healer.Name, HealCreep p.Id))
            | Some(p, _) ->
                owed.[p.Name] <- owed.[p.Name] - parts * Engine.rangedHealPower
                acts.Add(Creep(healer.Name, RangedHealCreep p.Id))
            | None -> ()

    List.ofSeq acts
