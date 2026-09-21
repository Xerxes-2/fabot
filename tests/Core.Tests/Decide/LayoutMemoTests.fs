/// The census signature and the plan memo it keys.
module Fabot.Core.Tests.Decide.LayoutMemoTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.LayoutFixtures

/// The flood arrays in a memo's three tables whose keys read `room` — a
/// spawn walk toward it, a Seam walk on either side of it, a far field whose
/// chain crosses it. Read by reference below: a flood the memo hands on is
/// the same array, and one it evicted and re-ran is a new one.
let private touchingRoom (room: string) (memo: PlanMemo) : int[] list =
    [
        for KeyValue((_, _, goalRoom), flood) in memo.Walks do
            if goalRoom = room then
                flood
        for KeyValue((fromRoom, toRoom), flood) in memo.SeamWalks do
            if fromRoom = room || toRoom = room then
                flood
        for KeyValue((chain, _, _, _, _, _), flood) in memo.FarFields do
            if List.contains room chain then
                flood
    ]

/// The spawn walks flooded over the home room alone.
let private homeWalks (memo: PlanMemo) : int[] list =
    [
        for KeyValue((_, _, goalRoom), flood) in memo.Walks do
            if goalRoom = "W1N1" then
                flood
    ]

/// The north-border colony with its outpost rock across the Seam and a spawn
/// back at home, so one tick fills both halves of the memo's tables: a spawn
/// walk over the home room alone, and the far fields of the Harvest across
/// the border, whose chains name the outpost. `control` is the outpost's
/// entry in `RoomControl`, the one census input the cases below move — a
/// reservation's rate against no vision at all — which moves the outpost's
/// signature and not the home room's.
let private borderedColony (control: RoomControlInfo option) =
    let colony =
        northBorderColony { X = 10; Y = 38 }
        |> withNorthOutpost (Some { X = 10; Y = 46 })

    { colony with
        Spawns = [ spawn ]
        Refillables = [ refillable "spawn-1" 0 BuiltKind.Spawn ]
        RoomControl =
            match control with
            | Some holder -> Map.add "W1N2" holder colony.RoomControl
            | None -> Map.remove "W1N2" colony.RoomControl
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    Map.add "spawn-1" (Structure BuiltKind.Spawn) colony.Spatial.TargetKinds
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.add "spawn-1" { X = 10; Y = 3 } layer.TargetPositions
                })
    }

[<Tests>]
let censusSignatureTests =
    testList
        "census signature"
        [
            // Every census input, perturbed alone, moves the signature: a
            // missed input would stall the Layout until a reset instead of
            // failing here.
            test "a structure appearing moves the signature" {
                let perturbed =
                    trunkColony 2
                    |> withTarget "ext-3" { X = 26; Y = 26 } (Structure BuiltKind.Extension)

                Expect.notEqual
                    (censusSignature perturbed)
                    (censusSignature (trunkColony 2))
                    "the standing census is a signature input"
            }

            test "a standing Storage is its own kind in the signature" {
                let standing kind =
                    trunkColony 2 |> withTarget "sto-1" { X = 24; Y = 24 } (Structure kind)

                Expect.notEqual
                    (censusSignature (standing BuiltKind.Storage))
                    (censusSignature (trunkColony 2))
                    "a Storage is a Structure: the standing census carries it (ADR 0022)"

                Expect.notEqual
                    (censusSignature (standing BuiltKind.Storage))
                    (censusSignature (standing BuiltKind.Other))
                    "and it is a kind of its own, not the unmodelled kind it used to project as"
            }

            test "a Thorium deposit appearing or leaving moves the signature" {
                // Leaving is the live case: the mod deletes an exhausted
                // Thorium deposit outright, and unsigned, the memo would hand
                // back a plan naming a container on the Seat of a rock that
                // is gone.
                let bare = trunkColony 6

                let standing =
                    { bare with
                        Spatial =
                            bare.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain = TerrainGrid.add mineralPos Wall layer.Terrain
                                })
                            |> withStanding "min-a" mineralPos Mineral
                    }

                Expect.notEqual
                    (censusSignature standing)
                    (censusSignature bare)
                    "the deposit is a signature input, so its deletion throws the plan away"

                let moved =
                    { standing with
                        Spatial =
                            standing.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        Map.add "min-a" { X = 26; Y = 33 } layer.TargetPositions
                                })
                    }

                Expect.notEqual
                    (censusSignature moved)
                    (censusSignature standing)
                    "and it is signed by its tile, which is the extractor's own"
            }

            test "a structure moving moves the signature" {
                let colony = trunkColony 2

                let moved =
                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        Map.add "ext-1" { X = 24; Y = 27 } layer.TargetPositions
                                })
                    }

                Expect.notEqual
                    (censusSignature moved)
                    (censusSignature colony)
                    "the census is (kind, position), not a count"
            }

            test "a pending site appearing moves the signature" {
                let perturbed =
                    trunkColony 2 |> withTarget "site-1" { X = 24; Y = 24 } (Site BuiltKind.Road)

                Expect.notEqual
                    (censusSignature perturbed)
                    (censusSignature (trunkColony 2))
                    "the pending census is a signature input"
            }

            test "a rival's site appearing moves the signature" {
                // Rival sites carry no id and no kind, so they reach the
                // signature as tiles and not through the pending census.
                // Unsigned, the memo would hand back the plan from before the
                // rival built: the `PlaceConstructionSite` the engine is
                // answering `-7`.
                let perturbed =
                    trunkColony 2
                    |> fun colony ->
                        { colony with
                            Spatial = colony.Spatial |> withRivalSites [ { X = 24; Y = 24 } ]
                        }

                Expect.notEqual
                    (censusSignature perturbed)
                    (censusSignature (trunkColony 2))
                    "the rival census is a signature input"
            }

            test "a rival's site outside the home room moves it too" {
                // The rival half is signed for every projected room, wider
                // than its one memoised reader (the Layout's tile clause
                // plans the home room alone): over-invalidating is the cheap
                // error, and a census stopping at the home layer is a
                // signature gap the tick another rule joins the memo. The
                // neighbour stands in both colonies, or `held` would move on
                // the room set alone and prove nothing about this half.
                let neighbouring rivals =
                    let colony = trunkColony 2

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    RivalSites = Set.ofList rivals
                                }
                    }

                Expect.notEqual
                    (censusSignature (neighbouring [ { X = 24; Y = 24 } ]))
                    (censusSignature (neighbouring []))
                    "an outpost's rival sites are signed as the home room's are"
            }

            test "a structure and a site of the same kind on the same tile differ" {
                let standing =
                    trunkColony 2
                    |> withTarget "can-1" { X = 16; Y = 25 } (Structure BuiltKind.Container)

                let pending =
                    trunkColony 2
                    |> withTarget "can-1" { X = 16; Y = 25 } (Site BuiltKind.Container)

                Expect.notEqual
                    (censusSignature standing)
                    (censusSignature pending)
                    "a site becoming a structure is a census change"
            }

            test "the controller level moves the signature" {
                // The level gates the placement's allowance and sizes the
                // cluster; a memo not keyed on it would hand a room that
                // levelled up yesterday's cluster (#341 through the memo).
                Expect.notEqual
                    (censusSignature (trunkColony 3))
                    (censusSignature (trunkColony 2))
                    "the level sizes the cluster and gates the placement, so it is a signature input"
            }

            test "a second room's standing container joins the signature under its own name" {
                // The hauler quota folds the containers of every projected
                // room and prices each at the rate that room is held at, so
                // both are memo inputs and the signature has to carry them.
                let colony = trunkColony 2

                let ground =
                    [
                        for x in 23..26 do
                            for y in 23..26 -> { X = x; Y = y }, Plain
                    ]

                let joined =
                    colony
                    |> withOutpost
                        "W1N2"
                        [
                            "src-out", { X = 24; Y = 24 }, Source
                            "can-out", { X = 24; Y = 25 }, Structure BuiltKind.Container
                        ]
                        ground

                Expect.notEqual
                    (censusSignature joined)
                    (censusSignature colony)
                    "an outpost's standing container is a census entry: it hires haulers"

                // The room is *in* the entry and not merely implied by the
                // room list, because two rooms hold the same coordinates.
                // Pairwise: the same container id at the same (24,25), and
                // the only thing that moves is which room's layer places it.
                let bare =
                    colony |> withOutpost "W1N2" [ "src-out", { X = 24; Y = 24 }, Source ] ground

                Expect.notEqual
                    (censusSignature joined)
                    (censusSignature bare)
                    "a standing structure in a second room is a census entry of its own"

                Expect.notEqual
                    (censusSignature joined)
                    (censusSignature (
                        bare
                        |> withTarget "can-out" { X = 24; Y = 25 } (Structure BuiltKind.Container)
                    ))
                    "the same container at the same coordinates in the other room is another census"

                // And not the container kind alone: the quota prices that
                // container by a round trip flooded over the outpost's grid,
                // so a road paved along the haul lane, or a hostile core
                // standing on it, moves a number the memo holds.
                let paved =
                    colony
                    |> withOutpost
                        "W1N2"
                        [
                            "src-out", { X = 24; Y = 24 }, Source
                            "road-out", { X = 24; Y = 26 }, Structure BuiltKind.Road
                        ]
                        ground

                Expect.notEqual
                    (censusSignature paved)
                    (censusSignature bare)
                    "a second room's road prices its haul, so it is a signature input too"

                // And the room list itself, because the rate is signed per
                // projected room.
                Expect.notEqual
                    (censusSignature (colony |> withOutpost "W1N2" [] ground))
                    (censusSignature colony)
                    "a room joining the projection brings its own held rate into the signature"

                Expect.notEqual
                    (censusSignature (
                        colony
                        |> withTarget "can-out" { X = 24; Y = 25 } (Structure BuiltKind.Container)
                    ))
                    (censusSignature colony)
                    "the home room's own container still moves it"
            }

            test "the room name moves the signature" {
                let colony = trunkColony 2

                // The same geometry, carried under the new name: a rename
                // that left the tiles filed under the old key would move the
                // signature by emptying the room rather than by naming it.
                let renamed =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                RoomName = Some "W2N2"
                                Rooms = Map.ofList [ "W2N2", homeLayer colony.Spatial ]
                            }
                    }

                Expect.notEqual
                    (censusSignature renamed)
                    (censusSignature colony)
                    "terrain is keyed by the room, so the name is a signature input"
            }

            test "who holds the home room moves the signature" {
                // The quota prices each container at its source's output,
                // read off `RoomControl`: a vision fact riding a census memo
                // has to be signed, or the memo hands back a quota sized for
                // the held rate on the tick the room stops being held.
                let colony = trunkColony 2

                let holding control =
                    { colony with
                        RoomControl = homeControl |> Map.map (fun _ _ -> control)
                    }

                Expect.notEqual
                    (censusSignature (holding neutralRoom))
                    (censusSignature (holding ownedRoom))
                    "a room that stopped being held prices its sources at half: a memo input"

                Expect.equal
                    (censusSignature (holding (reservedRoom true 4000)))
                    (censusSignature (holding ownedRoom))
                    "owned or reserved by us is one rate, so the two sign the same"

                Expect.notEqual
                    (censusSignature { colony with RoomControl = Map.empty })
                    (censusSignature (holding neutralRoom))
                    "and no vision at all is a third answer, not the neutral one (ADR 0004)"
            }

            test "the ticks left on a reservation leave the signature alone" {
                // `TicksToEnd` decays by one every tick, so signing it would
                // throw the Layout and the walk table away on every tick the
                // colony holds an outpost.
                let holding control =
                    { trunkColony 2 with
                        RoomControl = homeControl |> Map.map (fun _ _ -> control)
                    }

                Expect.equal
                    (censusSignature (holding (reservedRoom true 4000)))
                    (censusSignature (holding (reservedRoom true 3999)))
                    "the rate is the input, and the countdown under it is not"
            }

            test "everything outside the census leaves the signature alone" {
                let colony = trunkColony 2

                // The bank is perturbed in its Available alone: the Capacity
                // beside it is a function of the standing spawn/extension
                // census and the controller level, so it is covered rather
                // than absent.
                let perturbed =
                    { colony with
                        Time = colony.Time + 100
                        Bank = bank 0 300
                        Sources = [ drained "src-a" 120 ]
                        Creeps = [ worker "w1" 25 25 ]
                        Hostiles =
                            [
                                {
                                    Id = "h1"
                                    Owner = "raider"
                                    Pos = RoomPos.at "W1N1" { X = 30; Y = 25 }
                                    Body = [ Attack; Move ]
                                    TicksToLive = Engine.creepLifetime
                                }
                            ]
                        ConstructionSites = [ { Id = "site-9"; Left = siteOwes } ]
                        Spatial =
                            { colony.Spatial with
                                Hits = Map.ofList [ "ext-1", { Hits = 1; HitsMax = 3000 } ]
                                Stores = Map.ofList [ "ext-1", 50 ]
                            }
                            |> withCreepsAt [ "w1", { X = 20; Y = 25 } ]
                            |> withTargets [ "pile-1", { X = 22; Y = 25 }, (Dropped Energy) ]
                    }

                Expect.equal
                    (censusSignature perturbed)
                    (censusSignature colony)
                    "creeps, stores, hits, drops, hostiles, bank and tick are not census"

                // The inverse of every test above: the spawn walks are
                // recalled on this signature alone, so two views it calls
                // equal have to lay the same weight grid.
                Expect.sequenceEqual
                    (homeGridOf perturbed)
                    (homeGridOf colony)
                    "and the grid the walks flood over is bitwise the same"

                // The same pairing in the outpost, whose grid the far leg is
                // a pure function of. Perturbed out there and not at home, or
                // the assertion would be about the home layer twice over.
                let ground =
                    [
                        for x in 23..26 do
                            for y in 23..26 -> { X = x; Y = y }, Plain
                    ]

                let held =
                    colony |> withOutpost "W1N2" [ "src-out", { X = 24; Y = 24 }, Source ] ground

                let seen =
                    { held with
                        Hostiles =
                            [
                                {
                                    Id = "h2"
                                    Owner = "raider"
                                    Pos = RoomPos.at "W1N2" { X = 26; Y = 26 }
                                    Body = [ Attack; Move ]
                                    TicksToLive = Engine.creepLifetime
                                }
                            ]
                        Spatial =
                            { held.Spatial with
                                Rooms =
                                    Map.add
                                        "W1N2"
                                        { SpatialInfo.layerOf held.Spatial "W1N2" with
                                            CreepPositions = Map.ofList [ "w1", { X = 25; Y = 25 } ]
                                        }
                                        held.Spatial.Rooms
                            }
                    }

                Expect.equal
                    (censusSignature seen)
                    (censusSignature held)
                    "a creep and a raider in the outpost are no census of that room either"

                Expect.sequenceEqual
                    (stepGridOf "W1N2" seen)
                    (stepGridOf "W1N2" held)
                    "and the grid the far leg floods over is bitwise the same"
            }

            // The three weights inputs beside the terrain, each perturbed
            // alone. Each test asserts the pairing: the perturbation moves
            // the grid the recalled walks flood over, and it moves the
            // signature they are recalled on.
            test "a built road moves the signature" {
                let colony = trunkColony 2
                let tile = { X = 22; Y = 25 }

                let paved =
                    let placed = colony |> withTarget "road-1" tile (Structure BuiltKind.Road)

                    { placed with
                        Spatial =
                            placed.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Roads = Set.add tile layer.Roads
                                })
                    }

                Expect.notEqual
                    (homeGridOf paved)
                    (homeGridOf colony)
                    "a road discounts the ground under it"

                Expect.notEqual
                    (censusSignature paved)
                    (censusSignature colony)
                    "and the standing census carries it, so the signature moves with it (ADR 0010)"
            }

            test "an obstacle structure moves the signature" {
                let colony = trunkColony 2
                let tile = { X = 22; Y = 25 }

                let blocked =
                    let placed = colony |> withTarget "twr-1" tile (Structure BuiltKind.Tower)

                    { placed with
                        Spatial =
                            placed.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.add tile layer.Obstacles
                                })
                    }

                Expect.notEqual
                    (homeGridOf blocked)
                    (homeGridOf colony)
                    "an obstacle closes its tile to every flood"

                Expect.notEqual
                    (censusSignature blocked)
                    (censusSignature colony)
                    "and the standing census carries it, so the signature moves with it"
            }

            test "an obstacle site moves the signature" {
                let colony = trunkColony 2
                let tile = { X = 22; Y = 25 }

                let pending =
                    let placed = colony |> withTarget "twr-site" tile (Site BuiltKind.Tower)

                    { placed with
                        Spatial =
                            placed.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.add tile layer.Obstacles
                                })
                    }

                Expect.notEqual
                    (homeGridOf pending)
                    (homeGridOf colony)
                    "the engine refuses a creep its own obstacle site, so it blocks like the structure"

                Expect.notEqual
                    (censusSignature pending)
                    (censusSignature colony)
                    "and the pending census carries it, so the signature moves with it"
            }

            test "an obstacle site in an outpost moves the signature" {
                // The far-leg entry is a pure function of the goal room's
                // grid, so every input of that grid has to be in the
                // signature as the home room's are. The engine refuses a
                // creep its own site wherever it stands, so a pending census
                // read in the home layer alone would leave an outpost's
                // closed tile unsigned.
                let colony = trunkColony 2
                let tile = { X = 24; Y = 26 }

                let ground =
                    [
                        for x in 23..26 do
                            for y in 23..26 -> { X = x; Y = y }, Plain
                    ]

                let bare =
                    colony |> withOutpost "W1N2" [ "src-out", { X = 24; Y = 24 }, Source ] ground

                let sited =
                    let placed =
                        colony
                        |> withOutpost
                            "W1N2"
                            [
                                "src-out", { X = 24; Y = 24 }, Source
                                "twr-site-out", tile, Site BuiltKind.Tower
                            ]
                            ground

                    { placed with
                        Spatial =
                            { placed.Spatial with
                                Rooms =
                                    Map.add
                                        "W1N2"
                                        { SpatialInfo.layerOf placed.Spatial "W1N2" with
                                            Obstacles = Set.singleton tile
                                        }
                                        placed.Spatial.Rooms
                            }
                    }

                Expect.notEqual
                    (stepGridOf "W1N2" sited)
                    (stepGridOf "W1N2" bare)
                    "the site closes its tile in the outpost's grid, which the far leg floods"

                Expect.notEqual
                    (censusSignature sited)
                    (censusSignature bare)
                    "so the pending census reaches every projected room, not the home layer alone"

                Expect.notEqual
                    (stepGridOf "W1N2" bare)
                    (stepGridOf "W1N2" colony)
                    "the premise: a room the projection carries no layer for is all impassable (ADR 0004), so neither grid compared above is an empty one passing whatever the census did"
            }
        ]

[<Tests>]
let planMemoTests =
    testList
        "plan memo"
        [
            test "a memo with the matching signature is reused verbatim" {
                let snapshot = trunkColony 2
                let memo = sentinelMemo snapshot
                let decision = decide snapshot Map.empty Set.empty (Some memo)

                Expect.equal
                    (placementIntents decision.Intents)
                    [ "W1N1", { X = 1; Y = 1 }, Tower ]
                    "the memo's site Intents pass through, nothing recomputes"

                // Verbatim, field for field: every table on it is the one
                // this tick's Atlas was handed and wrote into, so nothing on
                // a reused memo has to be swapped out at the tick boundary.
                Expect.equal decision.Memo memo "the memo rides out unchanged for next tick"
            }

            test "an added structure invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)

                let perturbed =
                    trunkColony 2
                    |> withTarget "ext-3" { X = 26; Y = 26 } (Structure BuiltKind.Extension)

                let decision = decide perturbed Map.empty Set.empty (Some memo)
                let fresh = decideOn perturbed

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
            }

            test "an added site invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)

                let perturbed =
                    trunkColony 2 |> withTarget "site-1" { X = 24; Y = 24 } (Site BuiltKind.Road)

                let decision = decide perturbed Map.empty Set.empty (Some memo)
                let fresh = decideOn perturbed

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
            }

            test
                "the Layout's records are census-derived: recalled with the plan, recomputed with it" {
                // The sentinel says so in both directions: a memo whose
                // signature holds hands its own empty records back for a
                // room that has in fact lost a footing and reserved three,
                // and a stale one recomputes to what the room really has.
                let colony = sealedPocketColony 4

                let lost =
                    [
                        {
                            Target = plannedTile { X = 21; Y = 30 }
                            Kind = FootingKind.SourceContainer
                        }
                    ]

                let matching = decide colony Map.empty Set.empty (Some(sentinelMemo colony))

                Expect.isEmpty
                    matching.Memo.UnservedFootings
                    "a matching signature reuses the memo's record; nothing recomputes"

                Expect.isEmpty
                    matching.Memo.ServedFootings
                    "and reuses the served record with it, for a room that reserved three"

                let stale = decide colony Map.empty Set.empty (Some(sentinelMemo (trunkColony 2)))
                let memoless = decideOn colony

                Expect.equal
                    stale.Memo.UnservedFootings
                    lost
                    "a moved signature recomputes the record with the rest of the plan"

                Expect.equal
                    memoless.Memo.UnservedFootings
                    lost
                    "and a memoless tick derives the same loss"

                Expect.hasLength
                    stale.Memo.ServedFootings
                    3
                    "the served record is recomputed too, not left at the stale memo's empty"

                Expect.equal
                    stale.Memo.ServedFootings
                    memoless.Memo.ServedFootings
                    "tile for tile what a memoless tick reserves"
            }

            test "the unrouted trunks ride the memo with the rest of the plan" {
                // The same seam, over a room that has in fact lost a trunk:
                // a recalled plan reports what it reported when it was
                // computed, and a record that quietly recomputed itself
                // would break that.
                let colony = enclosedSourceColony 4

                let dropped =
                    [
                        {
                            Source = "src-b"
                            Goal = TrunkGoal.UpgradeArea
                        }
                        {
                            Source = "src-b"
                            Goal = TrunkGoal.Spawn "spawn-1"
                        }
                    ]

                let matching = decide colony Map.empty Set.empty (Some(sentinelMemo colony))

                Expect.isEmpty
                    matching.Memo.UnroutedTrunks
                    "a matching signature reuses the memo's record; nothing recomputes"

                let stale = decide colony Map.empty Set.empty (Some(sentinelMemo (trunkColony 2)))
                let memoless = decideOn colony

                Expect.equal
                    stale.Memo.UnroutedTrunks
                    dropped
                    "a moved signature recomputes the record with the rest of the plan"

                Expect.equal
                    memoless.Memo.UnroutedTrunks
                    dropped
                    "and a memoless tick derives the same loss"
            }

            test "a level-up invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)
                let decision = decide (trunkColony 3) Map.empty Set.empty (Some memo)
                let fresh = decideOn (trunkColony 3)

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"

                // And the recompute is not a formality: the two levels plan
                // differently, so a memo that survived the level-up would be
                // observably wrong rather than merely stale.
                Expect.notEqual
                    (placementIntents fresh.Intents)
                    (placementIntents (decideOn (trunkColony 2)).Intents)
                    "the level-up is a plan change, which is what the memo has to notice"
            }

            test "the memo's hauler quota feeds spawn planning; a stale one is discarded" {
                // The trunk colony has no source containers, so a fresh
                // quota is 0 and the sentinel's 3 is observable: with a
                // matching memo the hauler gap wins the casting order,
                // with a stale one the fresh quota casts a worker again.
                let snapshot =
                    { trunkColony 2 with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let memo =
                    { sentinelMemo snapshot with
                        HaulerQuota = 3
                        HaulerDemand = []
                        HaulerLoad = 0
                    }

                let castNames decision =
                    spawnIntents decision.Intents |> List.map (fun (_, _, name) -> name)

                let reused = decide snapshot Map.empty Set.empty (Some memo)

                Expect.equal
                    (castNames reused)
                    [ "hauler-42-Spawn1" ]
                    "the memo's quota opens a hauler gap the casting order fills first"

                let stale = decide (trunkColony 3) Map.empty Set.empty (Some memo)
                let fresh = decideOn (trunkColony 3)

                Expect.equal
                    (castNames stale)
                    (castNames fresh)
                    "a stale memo's quota is recomputed, never reused"
            }

            test "decide without a memo emits one keyed to this census" {
                let snapshot = trunkColony 2
                let decision = decideOn snapshot

                Expect.equal
                    decision.Memo.Signature
                    (censusSignature snapshot)
                    "the memo carries the census it was computed from"

                let next = decide snapshot Map.empty Set.empty (Some decision.Memo)

                Expect.equal
                    next.Intents
                    decision.Intents
                    "feeding the memo back reproduces the tick verbatim"
            }

            test "the spawn walks ride the memo while the census holds" {
                // The next tick under the same signature fills the table it
                // was handed rather than one of its own: a row the first tick
                // never priced lands beside the first tick's entry.
                let heavy name =
                    creepWith name 0 50 [ Work; Work; Work; Work; Carry; Move ]

                let lone =
                    trunkColony 2 |> staffedColony [ worker "w1" 0 50 ] [ "w1", { X = 22; Y = 25 } ]

                let joined =
                    trunkColony 2
                    |> staffedColony
                        [ worker "w1" 0 50; heavy "a1" ]
                        [ "w1", { X = 22; Y = 25 }; "a1", { X = 23; Y = 25 } ]

                Expect.equal
                    (censusSignature joined)
                    (censusSignature lone)
                    "a creep arriving is not a census change"

                let first = decideOn lone

                Expect.equal
                    first.Memo.Walks.Count
                    1
                    "the tick flooded once out of the spawn, into the table the memo carries"

                let workerFlood =
                    first.Memo.Walks |> Seq.map (fun entry -> entry.Value) |> Seq.exactlyOne

                let second = decide joined Map.empty Set.empty (Some first.Memo)

                Expect.isTrue
                    (obj.ReferenceEquals(second.Memo.Walks, first.Memo.Walks))
                    "an unchanged census hands the same table on"

                Expect.equal
                    second.Memo.Walks.Count
                    2
                    "and the second row's flood lands in it, beside the first tick's"

                Expect.isTrue
                    (second.Memo.Walks
                     |> Seq.exists (fun entry -> obj.ReferenceEquals(entry.Value, workerFlood)))
                    "the row the first tick priced was recalled, not flooded again"

                Expect.equal
                    second.Intents
                    (decideOn joined).Intents
                    "a recalled walk decides exactly what a fresh flood decides"
            }

            test "a moved census drops the walk table of the rooms that moved" {
                // A level-up is folded into every room's signature, so it
                // drops the lot. The table object is the memo's own and is
                // evicted in place, which is why the count is what is pinned
                // and not the reference.
                let staffed = staffedColony [ worker "w1" 0 50 ] [ "w1", { X = 22; Y = 25 } ]

                let first = decideOn (staffed (trunkColony 2))

                Expect.isNonEmpty first.Memo.Walks "the premise: the first tick flooded a walk"

                let emptied = decide (trunkColony 3) Map.empty Set.empty (Some first.Memo)

                Expect.equal
                    emptied.Memo.Walks.Count
                    0
                    "a level-up moves every room, and the table went with them"
            }

            test "an outpost's census moving keeps the home room's walks and drops the outpost's" {
                // Per room, an entry is kept while every room it reads holds
                // — a spawn walk to a far room reads home and the chain to
                // it, a Seam walk its pair, a far field its chain — and
                // dropped when one of them moves. Flat, a road appearing in
                // an outpost re-flooded everything: `reactor --census-every
                // 1` ran 109,258 heap pops a tick against 9,554 quiet
                // (2026-09-20, #388).
                let held = borderedColony (Some(reservedRoom true 4000))
                let dark = borderedColony None

                Expect.notEqual
                    (Map.find "W1N2" (roomSignatures dark))
                    (Map.find "W1N2" (roomSignatures held))
                    "the premise: vision leaving the outpost moves its room's signature"

                Expect.equal
                    (Map.find "W1N1" (roomSignatures dark))
                    (Map.find "W1N1" (roomSignatures held))
                    "and not the home room's"

                let first = decideOn held

                // Read off the first tick's tables **before** the second runs
                // over them: the tables are the memo's own and are evicted and
                // refilled in place, so a read afterwards sees the second
                // tick's contents under the first tick's name.
                let outpostFloods = touchingRoom "W1N2" first.Memo
                let homeFloods = homeWalks first.Memo

                Expect.isNonEmpty
                    outpostFloods
                    "the premise: the first tick flooded something that reads the outpost"

                Expect.isNonEmpty homeFloods "and a spawn walk over home alone"

                let second = decide dark Map.empty Set.empty (Some first.Memo)

                Expect.isTrue
                    (obj.ReferenceEquals(second.Memo.Walks, first.Memo.Walks))
                    "the table handed on is the memo's own"

                for flood in homeFloods do
                    Expect.isTrue
                        (homeWalks second.Memo
                         |> List.exists (fun kept -> obj.ReferenceEquals(kept, flood)))
                        "every home-room walk the first tick laid is still there, unflooded"

                for flood in outpostFloods do
                    Expect.isFalse
                        (touchingRoom "W1N2" second.Memo
                         |> List.exists (fun kept -> obj.ReferenceEquals(kept, flood)))
                        "and nothing that read the outpost survived its census moving"

                Expect.equal
                    second.Intents
                    (decideOn dark).Intents
                    "a recalled home walk decides exactly what a fresh flood decides"
            }

            test "the far fields ride the memo on the walk table's own terms" {
                // A far field is kept while every room its chain names holds.
                // The Harvest across the north border floods two fields here,
                // one over the outpost alone and one carried home along
                // `[W1N1; W1N2]`; a structure appearing at home moves the home
                // room, so the carried field goes and the outpost's own
                // stays. The field's contents are the Atlas suite's.
                let held = borderedColony (Some(reservedRoom true 4000))

                let first = decideOn held

                let chained (memo: PlanMemo) =
                    [
                        for KeyValue((chain, _, _, _, _, _), field) in memo.FarFields do
                            chain, field
                    ]

                let carried =
                    chained first.Memo |> List.filter (fun (chain, _) -> List.contains "W1N1" chain)

                let outpostOnly =
                    chained first.Memo |> List.filter (fun (chain, _) -> chain = [ "W1N2" ])

                Expect.isNonEmpty carried "the premise: a far field is carried home along the chain"
                Expect.isNonEmpty outpostOnly "and one is flooded over the outpost alone"

                let again = decide held Map.empty Set.empty (Some first.Memo)

                Expect.isTrue
                    (obj.ReferenceEquals(again.Memo.FarFields, first.Memo.FarFields))
                    "an unchanged census hands the same table on"

                let homeMoved =
                    decide
                        (held
                         |> withTarget "ext-3" { X = 10; Y = 20 } (Structure BuiltKind.Extension))
                        Map.empty
                        Set.empty
                        (Some first.Memo)

                for _, field in carried do
                    Expect.isFalse
                        (chained homeMoved.Memo
                         |> List.exists (fun (_, kept) -> obj.ReferenceEquals(kept, field)))
                        "a field whose chain crosses the room that moved is gone"

                for _, field in outpostOnly do
                    Expect.isTrue
                        (chained homeMoved.Memo
                         |> List.exists (fun (_, kept) -> obj.ReferenceEquals(kept, field)))
                        "and a field over the outpost alone rides on, unflooded"
            }

            test "a far field whose Task left the pool is evicted, census or no census" {
                // #392. The Harvest across the border prices a far field;
                // the tick the rock is gone from the projection, its Task is
                // pooled by nobody and the field goes — while the census
                // signature has not moved, so this is the Task rule and not
                // the room rule (#388).
                let held = borderedColony (Some(reservedRoom true 4000))

                let gone =
                    { held with
                        Sources = held.Sources |> List.filter (fun s -> s.Id <> "src-out")
                        Spatial =
                            { held.Spatial with
                                TargetKinds = Map.remove "src-out" held.Spatial.TargetKinds
                                Rooms =
                                    held.Spatial.Rooms
                                    |> Map.map (fun _ layer ->
                                        { layer with
                                            TargetPositions =
                                                Map.remove "src-out" layer.TargetPositions
                                        })
                            }
                    }

                Expect.equal
                    (censusSignature gone)
                    (censusSignature held)
                    "the premise: a rock leaving moves no census input"

                let ofRock (memo: PlanMemo) =
                    [
                        for KeyValue((_, task, _, _, _, _), field) in memo.FarFields do
                            if task = Harvest "src-out" then
                                field
                    ]

                let first = decideOn held
                let fields = ofRock first.Memo

                Expect.isNonEmpty fields "the premise: the first tick priced the rock's far leg"

                let still = decide held Map.empty Set.empty (Some first.Memo)

                for field in fields do
                    Expect.isTrue
                        (ofRock still.Memo
                         |> List.exists (fun kept -> obj.ReferenceEquals(kept, field)))
                        "a Task still pooled keeps its field, unflooded"

                let second = decide gone Map.empty Set.empty (Some first.Memo)

                Expect.isEmpty (ofRock second.Memo) "and the tick its Task is gone, so is its field"
            }

            test
                "a deferred turn stamps the tables with the census that filled them, not the plan's" {
                // On a `Waiting` turn the plan served is the stale one under
                // its own signature, while the tables on it are this tick's.
                // Stamped with the plan's signature, a census that moved and
                // moved back would recall the plan *and* the tables the dark
                // tick flooded over a grid missing that room's roads (#372).
                let held = borderedColony (Some(reservedRoom true 4000))
                let dark = borderedColony None

                let planned = decideOn held

                let waiting =
                    decideUnarbitrated
                        dark
                        Map.empty
                        Set.empty
                        (Some planned.Memo)
                        ReplanTurn.Waiting

                Expect.equal
                    waiting.Memo.Signature
                    planned.Memo.Signature
                    "the plan served on the dark tick is the stale one, under its signature"

                Expect.equal
                    waiting.Memo.RoomSignatures
                    (roomSignatures dark)
                    "while the tables are stamped with the dark tick's own rooms"

                let darkFloods = touchingRoom "W1N2" waiting.Memo

                Expect.isNonEmpty
                    darkFloods
                    "the premise: the dark tick flooded something that reads the outpost"

                let returned =
                    decideUnarbitrated held Map.empty Set.empty (Some waiting.Memo) ReplanTurn.Now

                Expect.equal
                    returned.Memo.SiteIntents
                    planned.Memo.SiteIntents
                    "the census is back, so the plan is recalled"

                for flood in darkFloods do
                    Expect.isFalse
                        (touchingRoom "W1N2" returned.Memo
                         |> List.exists (fun kept -> obj.ReferenceEquals(kept, flood)))
                        "and nothing the dark tick flooded over the outpost is served under the returned census"

                // The recalled plan restamps too: handed on as it was, every
                // held tick after this one would re-flood the outpost, and a
                // second vision loss would find the stamp equal to the dark
                // census and serve the held grid's floods on the dark tick.
                Expect.equal
                    returned.Memo.RoomSignatures
                    (roomSignatures held)
                    "the returned tick stamps the tables with its own rooms, recalled plan or not"

                let returnedFloods = touchingRoom "W1N2" returned.Memo

                let settled =
                    decideUnarbitrated held Map.empty Set.empty (Some returned.Memo) ReplanTurn.Now

                for flood in returnedFloods do
                    Expect.isTrue
                        (touchingRoom "W1N2" settled.Memo
                         |> List.exists (fun kept -> obj.ReferenceEquals(kept, flood)))
                        "so the held tick after it keeps the returned tick's outpost floods, unflooded"
            }

            // Four colonies re-planning in one tick measured 487 ms of the
            // engine's 500 ms ceiling, and a global reset empties every memo
            // at once (#357). So a colony re-plans on its turn, and what it
            // serves in between must be *old*, never wrong, and stay owed.
            test "a colony whose turn has not come serves the plan it has, and still owes a new one" {
                let staffed = staffedColony [ worker "w1" 0 50 ] [ "w1", { X = 22; Y = 25 } ]

                let planned = decideOn (staffed (trunkColony 2))

                // The census moves (a level-up) and the turn is somebody
                // else's: the stale plan stands, down to the Intents it
                // placed, because a site already in the world outlives the
                // Intent that placed it.
                let waiting =
                    decideUnarbitrated
                        (staffed (trunkColony 3))
                        Map.empty
                        Set.empty
                        (Some planned.Memo)
                        ReplanTurn.Waiting

                Expect.equal
                    waiting.Memo.SiteIntents
                    planned.Memo.SiteIntents
                    "the plan served is the one it already had"

                Expect.equal
                    waiting.Memo.Signature
                    planned.Memo.Signature
                    "and it keeps the old signature, so the plan is still owed"

                Expect.notEqual
                    (decideUnarbitrated
                        (staffed (trunkColony 3))
                        Map.empty
                        Set.empty
                        (Some planned.Memo)
                        ReplanTurn.Now)
                        .Memo.Signature
                    planned.Memo.Signature
                    "which the next turn pays: the same view, planning allowed, signs the census it planned against"
            }

            test "a colony with no memo at all defers to an empty plan, never to a signed one" {
                let staffed = staffedColony [ worker "w1" 0 50 ] [ "w1", { X = 22; Y = 25 } ]

                let blind =
                    decideUnarbitrated
                        (staffed (trunkColony 2))
                        Map.empty
                        Set.empty
                        None
                        ReplanTurn.Waiting

                Expect.isEmpty
                    blind.Memo.SiteIntents
                    "nothing is placed on a tick this colony did not plan"

                Expect.equal
                    blind.Memo.HaulerQuota
                    0
                    "and no hauler is asked for: a row of zero casts no body"

                // `censusSignature` composes its fields with `|` separators,
                // so it cannot produce the empty string: a deferred memo is
                // one no census can match. A memo stamped with the signature
                // it declined to plan against would be served forever.
                Expect.equal
                    blind.Memo.Signature
                    ""
                    "the signature is empty, which no census signature is"

                let paid = decideOn (staffed (trunkColony 2))

                Expect.notEqual paid.Memo.Signature "" "and the tick that plans signs it properly"
                Expect.isNonEmpty paid.Memo.SiteIntents "placing what the deferred tick did not"
            }
        ]
