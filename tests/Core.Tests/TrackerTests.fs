/// Invariants of the Tracker over generated histories of hostiles.
module Fabot.Core.Tests.TrackerTests

open Expecto
open FsCheck
open FsCheck.FSharp
open Fabot.Core.Vocabulary
open Fabot.Core.World
open Fabot.Core.Fight.Tracker

let private grace = 3

let private creep (id: string) (pos: Pos) (isArmed: bool) : Creep =
    {
        Id = id
        Name = id
        Owner = Theirs "rival"
        Pos = pos
        Body =
            [|
                {
                    Kind = (if isArmed then RangedAttack else Work)
                    Hits = 100
                    Boost = Unboosted
                }
            |]
        Hits = 100
        HitsMax = 100
        TicksToLive = 1000
        Fatigue = 0
        Store = Store.empty
    }

let private pool = [ for i in 0..14 -> $"h{i}" ]

/// Two rooms sharing a seam, and tiles crowded near it so clusters touch.
let private arrivalGen (id: string) : Gen<Creep> =
    gen {
        let! room = Gen.elements [ "W1N1"; "W2N1" ]
        let! x = Gen.elements [ 0; 1; 3; 6; 9; 43; 46; 48; 49 ]
        let! y = Gen.choose (0, 12)
        let! isArmed = Gen.frequency [ 4, Gen.constant true; 1, Gen.constant false ]
        return creep id { Room = room; X = x; Y = y } isArmed
    }

/// A tick later: most step a few tiles, some vanish, absent ids may arrive.
let private nextGen (now: Creep[]) : Gen<Creep[]> =
    gen {
        let! stayed =
            now
            |> Array.map (fun c ->
                gen {
                    let! dx = Gen.choose (-3, 3)
                    let! dy = Gen.choose (-3, 3)
                    let! keep = Gen.frequency [ 6, Gen.constant true; 1, Gen.constant false ]
                    let clamp v = max 0 (min 49 v)

                    return
                        if keep then
                            Some
                                { c with
                                    Pos =
                                        { c.Pos with
                                            X = clamp (c.Pos.X + dx)
                                            Y = clamp (c.Pos.Y + dy)
                                        }
                                }
                        else
                            None
                })
            |> Array.toList
            |> Gen.sequenceToList

        let absent =
            pool |> List.filter (fun id -> now |> Array.forall (fun c -> c.Id <> id))

        let! arriving = Gen.subListOf absent |> Gen.map (List.truncate 3)
        let! arrived = arriving |> List.map arrivalGen |> Gen.sequenceToList
        return Array.append (Array.ofList (List.choose id stayed)) (Array.ofList arrived)
    }

/// Groups after one to six ticks, the next tick's hostiles, and that tick.
type History = History of prev: Group list * now: Creep[] * tick: int

let private historyGen: Gen<History> =
    gen {
        let! n = Gen.choose (1, 6)
        let! ids = Gen.subListOf pool
        let! first = ids |> List.map arrivalGen |> Gen.sequenceToList

        let rec run (t: int) (groups: Group list) (now: Creep[]) =
            if t > n then
                Gen.constant (History(groups, now, t))
            else
                nextGen now |> Gen.bind (run (t + 1) (fst (step grace groups now t)))

        return! run 1 [] (Array.ofList first)
    }

type Histories =
    static member History() = Arb.fromGen historyGen

let private config =
    { FsCheckConfig.defaultConfig with
        maxTest = 300
        arbitrary = [ typeof<Histories> ]
    }

let private seen (tick: int) (groups: Group list) =
    groups |> List.filter (fun g -> g.LastSeen = tick)

let private distance (a: Pos) (b: Pos) =
    let ax, ay = worldOf a
    let bx, by = worldOf b
    max (abs (ax - bx)) (abs (ay - by))

[<Tests>]
let tests =
    testList
        "tracker"
        [
            testProperty "tiles across a seam link only when adjacent"
            <| fun (west: int) (east: int) (y: int) (dy: int) ->
                let west, east = 49 - abs west % 3, abs east % 3
                let y, dy = 1 + abs y % 47, (abs dy % 3) - 1
                let a = { Room = "W2N1"; X = west; Y = y }
                let b = { Room = "W1N1"; X = east; Y = y + dy }
                linked a b = ((49 - west) + east + 1 <= 1)

            testPropertyWithConfig config "ids are stable under a permutation of the input"
            <| fun (History(prev, now, tick)) (seed: int) ->
                let shuffle (xs: 'a[]) =
                    let r = System.Random seed
                    xs |> Array.sortBy (fun _ -> r.Next())

                step grace prev now tick = step
                    grace
                    (prev |> Array.ofList |> shuffle |> List.ofArray)
                    (shuffle now)
                    tick

            testPropertyWithConfig
                config
                "every hostile stands in exactly one group, and no id twice"
            <| fun (History(prev, now, tick)) ->
                let groups, _ = step grace prev now tick
                let ids = groups |> List.map (fun g -> g.Id)
                let members = seen tick groups |> List.collect (fun g -> List.ofSeq g.Members)

                List.distinct ids = ids
                && List.sort members = (now
                                        |> Array.map (fun c -> c.Id)
                                        |> Array.sort
                                        |> List.ofArray)

            testPropertyWithConfig
                config
                "a group is one cluster, no wider than its diameter, reaching no other"
            <| fun (History(prev, now, tick)) ->
                let groups = seen tick (fst (step grace prev now tick))
                let at = now |> Array.map (fun c -> c.Id, c) |> Map.ofArray

                let positions (g: Group) =
                    g.Members |> Seq.map (fun m -> at.[m]) |> List.ofSeq

                let narrow (g: Group) =
                    let ps = positions g

                    ps
                    |> List.forall (fun a ->
                        ps
                        |> List.forall (fun b -> distance a.Pos b.Pos <= reach * (ps.Length - 1)))

                let apart (g: Group) (h: Group) =
                    g.Id = h.Id
                    || positions g
                       |> List.forall (fun a ->
                           positions h
                           |> List.forall (fun b -> not (armed a && armed b && linked a.Pos b.Pos)))

                let connected (g: Group) =
                    match positions g with
                    | [ _ ] -> true
                    | first :: _ as ps ->
                        let rec grow (reached: Set<string>) =
                            let more =
                                ps
                                |> List.filter (fun b ->
                                    not (reached.Contains b.Id)
                                    && ps
                                       |> List.exists (fun a ->
                                           reached.Contains a.Id
                                           && armed a
                                           && armed b
                                           && linked a.Pos b.Pos))

                            if more.IsEmpty then
                                reached
                            else
                                grow (
                                    Set.union
                                        reached
                                        (more |> List.map (fun c -> c.Id) |> Set.ofList)
                                )

                        (grow (Set.singleton first.Id)).Count = ps.Length
                    | [] -> false

                groups |> List.forall (fun g -> narrow g && connected g)
                && groups |> List.forall (fun g -> groups |> List.forall (apart g))

            testPropertyWithConfig config "a new id is none the previous tick held"
            <| fun (History(prev, now, tick)) ->
                let held = prev |> List.map (fun g -> g.Id) |> Set.ofList
                let groups, events = step grace prev now tick

                events
                |> List.forall (fun e ->
                    match e with
                    | Event.Newcomer id -> not (held.Contains id)
                    | _ -> true)
                && groups |> List.forall (fun g -> held.Contains g.Id || g.LastSeen = tick)

            testPropertyWithConfig config "a split keeps the id on the largest fragment"
            <| fun (History(prev, now, tick)) ->
                let groups, events = step grace prev now tick
                let byId = groups |> List.map (fun g -> g.Id, g) |> Map.ofList
                let before = prev |> List.map (fun g -> g.Id, g.Members) |> Map.ofList

                let share (g: Group) (members: Set<string>) =
                    Set.intersect g.Members members |> Set.count

                events
                |> List.forall (fun e ->
                    match e with
                    | Event.Split(from, into) ->
                        let fragment = share byId.[into] before.[from]

                        fragment > 0
                        && match Map.tryFind from byId with
                           | Some kept -> share kept before.[from] >= fragment
                           | None -> true
                    | _ -> true)

            testPropertyWithConfig config "a merge keeps the older id"
            <| fun (History(prev, now, tick)) ->
                let groups, events = step grace prev now tick
                let ids = groups |> List.map (fun g -> g.Id) |> Set.ofList

                events
                |> List.forall (fun e ->
                    match e with
                    | Event.Merged(into, from) ->
                        into < from && ids.Contains into && not (ids.Contains from)
                    | _ -> true)

            testPropertyWithConfig config "an unseen group is kept for the grace, then dropped"
            <| fun (History(prev, now, tick)) ->
                let groups, _ = step grace prev now tick

                groups |> List.forall (fun g -> tick - g.LastSeen <= grace)
                && prev
                   |> List.forall (fun g ->
                       tick - g.LastSeen > grace
                       || now |> Array.exists (fun c -> g.Members.Contains c.Id)
                       || List.contains g groups)
        ]
