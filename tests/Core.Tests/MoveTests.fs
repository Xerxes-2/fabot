/// Invariants of the movement arbitration over generated crowds (wishes packed
/// into a small walled box, with foreign bodies among them), and of the
/// wishes `Plan` builds over generated rooms.
module Fabot.Core.Tests.MoveTests

open Expecto
open FsCheck
open FsCheck.FSharp
open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Grid
open Fabot.Core.Move

/// A room's ground, the tiles bodies outside the wishes hold, the wishes, and
/// the same wishes and holds in another order.
type Crowd =
    {
        Passable: bool[]
        Held: int[]
        Wishes: Wish list
        Shuffled: Wish list
        HeldShuffled: int[]
    }

/// A creep in a generated room with somewhere to walk: this room's tile or
/// the room west of it.
type Errand =
    {
        Terrain: byte[]
        Creep: Creep
        Target: Target
    }

// The box every crowd stands in: eight tiles a side, so bodies jostle.
let private box =
    [|
        for y in 20..27 do
            for x in 20..27 -> Tiles.index x y
    |]

let private around (tile: int) =
    Tiles.within 1 tile |> Array.filter ((<>) tile)

let private wishGen (name: string) (at: int) =
    gen {
        let near = around at
        let! stays = Gen.frequency [ 1, Gen.constant true; 2, Gen.constant false ]
        let! step = Gen.elements near
        let! onward = Gen.elements (Array.append [| -1 |] (around step))
        let! tail = Gen.subListOf near
        let! tail = Gen.shuffle tail
        // A work area is only ever handed to a body standing inside it.
        let! working = Gen.subListOf near

        let! working =
            Gen.frequency
                [
                    1, Gen.constant [||]
                    1, Gen.constant (Array.append [| at |] (Array.ofSeq working))
                ]

        let! weight = Gen.choose (1, 6)
        let! moves = Gen.frequency [ 9, Gen.constant true; 1, Gen.constant false ]

        return
            {
                Creep = name
                Footprint = One
                At = at
                Head = (if stays then at else step)
                Onward = (if stays then -1 else onward)
                Tail = tail
                Working = working
                Weight = weight
                Moves = moves
            }
    }

let private roomName = "W1N1"

let private errandGen =
    gen {
        let! terrain =
            Gen.arrayOfLength
                Tiles.count
                (Gen.frequency
                    [
                        6, Gen.constant 0uy
                        2, Gen.constant Terrain.swamp
                        2, Gen.constant Terrain.wall
                    ])

        let! at = Gen.choose (0, Tiles.count - 1)
        let! goal = Gen.choose (0, Tiles.count - 1)
        let! range = Gen.choose (0, 3)
        let! away = Gen.frequency [ 3, Gen.constant false; 1, Gen.constant true ]
        let! parts = Gen.nonEmptyListOf (Gen.elements [ Move; Work; Carry ])
        let! fatigue = Gen.frequency [ 4, Gen.constant 0; 1, Gen.choose (1, 10) ]

        return
            {
                Terrain = terrain
                Creep =
                    {
                        Id = "c"
                        Name = "c"
                        Owner = Ours
                        Pos = Tiles.toPos roomName at
                        Body =
                            [|
                                for kind in parts ->
                                    {
                                        Kind = kind
                                        Hits = 100
                                        Boost = Unboosted
                                    }
                            |]
                        Hits = 100
                        HitsMax = 100
                        TicksToLive = 1000
                        Fatigue = fatigue
                        Store = Store.empty
                    }
                Target =
                    {
                        Room = (if away then "W2N1" else roomName)
                        Tiles = [| goal |]
                        Range = range
                        Avoid = Set.empty
                        MaxHops = 4
                    }
            }
    }

type Gens =
    static member Crowd() =
        gen {
            let! walls =
                Gen.arrayOfLength
                    box.Length
                    (Gen.frequency [ 6, Gen.constant false; 1, Gen.constant true ])

            let passable = Array.create Tiles.count true

            for i in 0 .. box.Length - 1 do
                if walls.[i] then
                    passable.[box.[i]] <- false

            let! open' = Gen.shuffle (box |> Array.filter (fun tile -> passable.[tile]))
            let! bodies = Gen.choose (0, min 30 open'.Length)
            let! foreign = Gen.choose (0, min 6 (open'.Length - bodies))

            let! wishes =
                [ for i in 0 .. bodies - 1 -> wishGen $"c{i}" open'.[i] ] |> Gen.sequenceToList

            let held = open'.[bodies .. bodies + foreign - 1]
            let! shuffled = Gen.shuffle wishes
            let! heldShuffled = Gen.shuffle held

            return
                {
                    Passable = passable
                    Held = held
                    Wishes = wishes
                    Shuffled = List.ofArray shuffled
                    HeldShuffled = heldShuffled
                }
        }
        |> Arb.fromGen

    static member Errand() = Arb.fromGen errandGen

let private config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Gens> ]
        maxTest = 500
    }

let private resolve (crowd: Crowd) =
    Arbitrate.arbitrate (fun tile -> crowd.Passable.[tile]) crowd.Held crowd.Wishes

/// Each wish with the tile it ends on.
let private ends (wishes: Wish list) (moves: (string * int) list) =
    let steps = Map.ofList moves

    [
        for wish in wishes ->
            match Map.tryFind wish.Creep steps with
            | Some direction -> wish, Tiles.neighbour wish.At direction
            | None -> wish, wish.At
    ]

let private placedIn (crowd: Crowd) = ends crowd.Wishes (resolve crowd)

let private score (placed: (Wish * int) list) =
    placed |> List.sumBy (fun (wish, tile) -> Wish.value wish tile)

let private listed (wish: Wish) (tile: int) =
    tile = wish.Head || Array.contains tile wish.Tail

/// The crowd with its first wish the only one moving, and every other body
/// standing on its work.
let private duel (crowd: Crowd) =
    crowd.Wishes
    |> List.mapi (fun i wish ->
        if i = 0 then
            wish
        else
            { wish with
                Head = wish.At
                Onward = -1
                Working =
                    (if wish.Working.Length = 0 then
                         [| wish.At |]
                     else
                         wish.Working)
            })

let private worldOf (errand: Errand) : World =
    {
        Time = 1
        Me = ""
        CpuLimit = 20.0
        Bucket = 10_000
        Rooms =
            [|
                {
                    Name = roomName
                    Terrain = errand.Terrain
                    Controller = None
                    EnergyAvailable = 0
                    EnergyCapacity = 0
                    Sources = [||]
                    Minerals = [||]
                    Structures = [||]
                    Sites = [||]
                    Piles = [||]
                    Remains = [||]
                    Hostiles = [||]
                }
            |]
        Spawns = [||]
        Creeps = [| errand.Creep |]
        OurSites = 0
    }

/// The errand's Atlas, its creep's pace, and the wish `Plan.toward` builds.
let private planned (errand: Errand) =
    let atlas = Atlas.ofWorld (worldOf errand)

    let mover =
        {
            Footprint = One
            Factor = factorOf errand.Creep
        }

    atlas, mover, Plan.toward atlas errand.Creep mover errand.Target 3

[<Tests>]
let tests =
    testList
        "Move"
        [
            testPropertyWithConfig config "no two bodies end on one tile, wished or held"
            <| fun (crowd: Crowd) ->
                let tiles = placedIn crowd |> List.map snd
                let all = tiles @ List.ofArray crowd.Held
                List.length (List.distinct all) = List.length all

            testPropertyWithConfig
                config
                "a creep steps once, onto a passable tile beside it it listed"
            <| fun (crowd: Crowd) ->
                let byName = crowd.Wishes |> List.map (fun wish -> wish.Creep, wish) |> Map.ofList

                resolve crowd
                |> List.forall (fun (name, direction) ->
                    let wish = byName.[name]
                    let tile = Tiles.neighbour wish.At direction

                    direction >= 1
                    && direction <= 8
                    && tile >= 0
                    && Tiles.range wish.At tile = 1
                    && crowd.Passable.[tile]
                    && listed wish tile)

            testPropertyWithConfig config "a creep that cannot move is never moved"
            <| fun (crowd: Crowd) ->
                let moved = resolve crowd |> List.map fst |> Set.ofList

                crowd.Wishes
                |> List.forall (fun wish -> wish.Moves || not (Set.contains wish.Creep moved))

            testPropertyWithConfig
                config
                "the room's score ends above everyone staying, and equal only when nobody moves"
            <| fun (crowd: Crowd) ->
                let moves = resolve crowd
                let before = crowd.Wishes |> List.sumBy (fun wish -> Wish.value wish wish.At)
                let after = score (ends crowd.Wishes moves)

                if List.isEmpty moves then
                    after = before
                else
                    after > before

            testPropertyWithConfig
                config
                "a displaced staying creep's tile is held by a creep that stepped onto it"
            <| fun (crowd: Crowd) ->
                let placed = placedIn crowd

                placed
                |> List.forall (fun (wish, tile) ->
                    not (Wish.stays wish)
                    || tile = wish.At
                    || placed
                       |> List.exists (fun (other, onto) -> onto = wish.At && onto <> other.At))

            testPropertyWithConfig
                config
                "a working body is pushed off its work only by a mover two weights heavier"
            <| fun (crowd: Crowd) ->
                let wishes = duel crowd

                match wishes with
                | [] -> true
                | mover :: _ ->
                    Arbitrate.arbitrate (fun tile -> crowd.Passable.[tile]) crowd.Held wishes
                    |> ends wishes
                    |> List.forall (fun (wish, tile) ->
                        obj.ReferenceEquals(wish, mover)
                        || Array.contains tile wish.Working
                        || max 1 mover.Weight >= max 1 wish.Weight + 2)

            testPropertyWithConfig
                config
                "a body shoved off a step lands on the shover's onward step only when nothing else is free"
            <| fun (crowd: Crowd) ->
                match crowd.Wishes with
                | mover :: _ when
                    not (Wish.stays mover)
                    && crowd.Passable.[mover.Head]
                    && mover.Onward >= 0
                    && mover.Onward <> mover.At
                    && crowd.Passable.[mover.Onward]
                    ->
                    let pushed =
                        {
                            Creep = "pushed"
                            Footprint = One
                            At = mover.Head
                            Head = mover.Head
                            Onward = -1
                            Tail =
                                Array.append
                                    [| mover.Onward |]
                                    (around mover.Head |> Array.filter ((<>) mover.Onward))
                            Working = [||]
                            Weight = 1
                            Moves = true
                        }

                    let wishes = [ { mover with Moves = true }; pushed ]

                    Arbitrate.arbitrate (fun tile -> crowd.Passable.[tile]) [||] wishes
                    |> ends wishes
                    |> List.forall (fun (wish, tile) ->
                        wish.Creep <> "pushed" || tile <> mover.Onward)
                | _ -> true

            testPropertyWithConfig
                config
                "no creep could raise its own value stepping alone onto a free tile it listed"
            <| fun (crowd: Crowd) ->
                let placed = placedIn crowd
                let taken = Set.ofList (List.map snd placed) + Set.ofArray crowd.Held

                placed
                |> List.forall (fun (wish, tile) ->
                    not wish.Moves
                    || Array.append [| wish.Head |] wish.Tail
                       |> Array.forall (fun free ->
                           Set.contains free taken
                           || Tiles.range wish.At free <> 1
                           || not crowd.Passable.[free]
                           || Wish.value wish free <= Wish.value wish tile))

            testPropertyWithConfig config "the same crowd in any order resolves the same"
            <| fun (crowd: Crowd) ->
                let first = resolve crowd
                let again = resolve crowd

                let reordered =
                    Arbitrate.arbitrate
                        (fun tile -> crowd.Passable.[tile])
                        crowd.HeldShuffled
                        crowd.Shuffled

                first = again && first = reordered

            testPropertyWithConfig
                config
                "a planned walk lists passable tiles beside it, its tail nearer the target or beside its head"
            <| fun (errand: Errand) ->
                let atlas, mover, wish = planned errand
                let at = wish.At

                let ticks (tile: int) =
                    Atlas.travelTicks atlas mover errand.Target (Tiles.toPos roomName tile)

                Array.append [| wish.Head |] wish.Tail
                |> Array.forall (fun tile ->
                    tile = at || (Tiles.range at tile = 1 && Atlas.passable atlas roomName tile))
                && (Wish.stays wish
                    || wish.Tail
                       |> Array.forall (fun tile ->
                           match ticks tile, ticks at with
                           | Some near, Some far -> near < far || Tiles.range tile wish.Head = 1
                           | _ -> false))

            testPropertyWithConfig
                config
                "a planned wish steps onto the border ring only where its walk leaves the room"
            <| fun (errand: Errand) ->
                let atlas, mover, wish = planned errand

                Array.append [| wish.Head |] wish.Tail
                |> Array.forall (fun tile ->
                    tile = wish.At
                    || not (Tiles.onEdge tile)
                    || (errand.Target.Room <> roomName
                        && Atlas.travelTicks atlas mover errand.Target (Tiles.toPos roomName tile) = Some
                            0))

            testPropertyWithConfig config "a planned wish's working tiles hold where it stands"
            <| fun (errand: Errand) ->
                let _, _, wish = planned errand
                wish.Working.Length = 0 || Array.contains wish.At wish.Working

            testPropertyWithConfig
                config
                "a creep that cannot move is planned to stay, without a flood"
            <| fun (errand: Errand) ->
                let atlas, mover, wish = planned errand

                Plan.canMove errand.Creep mover.Factor
                || (Wish.stays wish && not wish.Moves && (Atlas.counts atlas).Misses = 0)
        ]
