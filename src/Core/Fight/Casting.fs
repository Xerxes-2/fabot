/// What a fight casts and where (`ARCHITECTURE.md` §6.3, §6.5). From v1's
/// Caster: a body goes to the spawn that lands it soonest, a home's own Defend
/// to the home's.
module Fabot.Core.Fight.Casting

open System.Collections.Generic
open Fabot.Core
open Fabot.Core.Intents
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Exchange
open Fabot.Core.Fight.Shape

// v1's guard, ranger and defender blocks.
let private blocks =
    [
        [ Tough; Move; Move; Move; Move; Move; Attack; Attack; Attack; Heal ]
        [ Move; Move; Move; RangedAttack; RangedAttack; Heal ]
        [ Move; Attack ]
    ]

/// v1's `guardCap`.
let private copies = 2

let affordable (capacity: int) : Exchange.Body list =
    blocks
    |> List.collect (fun block ->
        let most =
            min (capacity / Engine.bodyCost block) (Engine.maxBodyParts / List.length block)

        List.init most (fun n ->
            List.replicate copies (List.replicate (n + 1) block |> List.concat))
        |> List.concat)

let fighterOf (c: Creep) : Fighter.Fighter =
    let parts = List.ofArray c.Body

    { Fighter.ofParts
          (parts |> List.map (fun p -> p.Kind))
          (parts |> List.map (fun p -> p.Boost))
          c.Hits
          (Some c.Pos) with
        Invader =
            match c.Owner with
            | Theirs user -> user = Engine.invaderUser
            | _ -> false
    }

let terrainAt (room: Room) (at: Pos) : Exchange.Terrain =
    let last = Engine.roomSide - 1

    {
        Weight =
            if Terrain.isSwamp room.Terrain at.X at.Y then
                Engine.swampWeight
            else
                Engine.plainWeight
        RunOff = max (max at.X (last - at.X)) (max at.Y (last - at.Y)) - 1
    }

let sideIn (room: Room) (bodies: Fighter.Fighter list) : Exchange.Side =
    let ours (kind: Kind) =
        room.Structures |> Array.filter (fun s -> s.Kind = kind && s.Owner.IsOurs)

    {
        Bodies = bodies
        Ramparts = ours Kind.Rampart |> Array.map (fun s -> s.Pos, s.Hits) |> Map.ofArray
        Towers =
            ours Kind.Tower
            |> Array.map (fun s ->
                ({
                    Pos = s.Pos
                    Energy = Store.amountOf Energy s.Store
                }
                : Exchange.Tower))
            |> List.ofArray
    }

let wins
    (stance: Exchange.Stance)
    (ours: Exchange.Side)
    (theirs: Exchange.Side)
    (terrain: Exchange.Terrain)
    =
    match Exchange.outcome stance ours theirs terrain with
    | Exchange.Lose -> false
    | _ -> true

/// The bodies to ask beside `ours`, the outcome they land and the tactic it
/// is won by; None: no body the spawns buy wins.
let price
    (stance: Exchange.Stance)
    (ours: Exchange.Side)
    (theirs: Exchange.Side)
    (terrain: Exchange.Terrain)
    (capacity: int)
    : (Exchange.Body list * Exchange.Outcome * Tactic) option =
    (if wins stance ours theirs terrain then
         Some []
     else
         Exchange.cheapestWin stance ours (affordable capacity) theirs terrain)
    |> Option.map (fun set ->
        let landed =
            { ours with
                Bodies = ours.Bodies @ List.map Fighter.cast set
            }

        let tactic (won: Tactic) =
            if (Exchange.stanceAgainst stance landed theirs terrain).IsHold then
                Tactic.Hold
            else
                won

        match Exchange.outcome stance landed theirs terrain with
        | Exchange.Win _ as won -> set, won, tactic Tactic.Close
        | other -> set, other, tactic Tactic.Kite)

let killOrder (hostiles: Creep[]) : string list =
    hostiles
    |> Array.map (fun h ->
        let f = fighterOf h
        let damage = Engine.attackPower * f.Attack + Engine.rangedAttackPower * f.Ranged
        h.Id, (f.Heal = 0, -(float damage / float (max 1 h.Hits))))
    |> Array.sortBy (fun (id, rank) -> rank, id)
    |> Array.map fst
    |> List.ofArray

/// The colony that lands a body soonest of those whose bank buys it: its
/// queue, then the walk (v1's Caster). None: no colony pays for it.
let colonyFor
    (colonies: Room[])
    (spawns: Spawn[])
    (queued: Dictionary<string, int>)
    (walk: string -> int option)
    (body: Exchange.Body)
    : string option =
    let queue (room: string) =
        (spawns
         |> Array.filter (fun s -> s.Pos.Room = room)
         |> Array.map (fun s ->
             s.Spawning |> Option.map (fun o -> o.Remaining) |> Option.defaultValue 0)
         |> Array.fold min System.Int32.MaxValue)
        + (if queued.ContainsKey room then queued.[room] else 0)

    colonies
    |> Array.filter (fun r -> r.EnergyCapacity >= Engine.bodyCost body)
    |> Array.choose (fun r -> walk r.Name |> Option.map (fun w -> r.Name, w + queue r.Name))
    |> Array.sortBy (fun (room, ticks) -> ticks, room)
    |> Array.tryHead
    |> Option.map fst
