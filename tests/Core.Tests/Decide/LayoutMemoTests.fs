/// The census signature and the plan memo it keys (ADR 0017, ADR 0044).
module Fabot.Core.Tests.Decide.LayoutMemoTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.LayoutFixtures

[<Tests>]
let censusSignatureTests =
    testList
        "census signature"
        [
            // Every census input, perturbed alone, moves the signature —
            // the test surface ADR 0017 demands: a missed input would stall
            // the Layout until a reset instead of failing here.
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
                Expect.notEqual
                    (censusSignature (trunkColony 3))
                    (censusSignature (trunkColony 2))
                    "the level gates allowances, so it is a signature input"
            }

            test "a second room's standing container joins the signature under its own name" {
                // The widening #116's forward note booked and #149 spent
                // (ADR 0042): the hauler quota folds the containers of
                // every projected room and prices each at the rate that
                // room is held at, so both of those are memo inputs now
                // and the signature that gates the memo has to carry them.
                // #121's narrowing to the home layer was right while the
                // memo held nothing but home's; this is the tick it stops
                // being.
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
                // Pairwise, one rival at a time: both sides below project
                // W1N2 with the same ground, the same source and the same
                // control, and carry the same container id at the same
                // (24,25) — the only thing that moves is which room's
                // layer places it.
                let bare =
                    colony |> withOutpost "W1N2" [ "src-out", { X = 24; Y = 24 }, Source ] ground

                // The widening itself, with nothing else moving: the same
                // room list, the same held rates, the same home layer —
                // the container standing in W1N2 is the only difference.
                // Joined against the home layer alone (#121's rule) these
                // two sign the same string, and the memo hands back the
                // quota from before the container stood.
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

                // And not the container kind alone. The quota prices that
                // container by a round trip flooded over the outpost's
                // step-weight grid, and `World.seenFacts` lays a
                // room's `Roads` and `Obstacles` out of the same
                // every-owner structure array the kind census comes from —
                // so a road paved along the haul lane, or a hostile core
                // standing on it, moves a number the memo holds. A
                // standing census filtered down to `Container` outside
                // home would be ADR 0017's signature gap.
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
                // projected room: a room that joins carrying nothing is a
                // room the quota can fold a container out of the tick one
                // stands there, and its held rate is what that container
                // would be priced at.
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

                // The same geometry, carried under the new name: the layer
                // is keyed by room (ADR 0041), so a rename that left the
                // tiles filed under the old key would move the signature by
                // emptying the room rather than by naming it — and the
                // input under test would go unmeasured.
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
                // The hauler quota's second load-bearing input since ADR
                // 0042: it prices each container at its source's own
                // output, and that output is read off `RoomControl`. A
                // vision fact riding a census memo has to be signed, or
                // the memo hands back a quota sized for the held rate on
                // the tick the room stops being held — the signature gap
                // ADR 0017 names as its failure mode.
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
                // The half of the reservation the quota does *not* read.
                // `TicksToEnd` decays by one every tick, so signing it
                // would throw the Layout and the walk table away on every
                // tick the colony holds an outpost — the memo would never
                // survive its own input.
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

                // The bank is perturbed in its Available alone: the
                // Capacity beside it is a function of the standing
                // spawn/extension census and the controller level, so it is
                // covered rather than absent (ADR 0017) — which is what
                // lets the successor body a lead is priced for ride this
                // signature too (ADR 0032).
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
                        ConstructionSites = [ { Id = "site-9" } ]
                        Spatial =
                            { colony.Spatial with
                                Hits = Map.ofList [ "ext-1", { Hits = 1; HitsMax = 3000 } ]
                                Stores = Map.ofList [ "ext-1", 50 ]
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 20; Y = 25 } ]
                                })
                            |> withTargets [ "pile-1", { X = 22; Y = 25 }, Dropped ]
                    }

                Expect.equal
                    (censusSignature perturbed)
                    (censusSignature colony)
                    "creeps, stores, hits, drops, hostiles, bank and tick are not census"

                // ADR 0032's guard, the inverse of every test above: the
                // spawn walks behind the leads are recalled on this
                // signature alone, so two views it calls equal have to
                // lay the same weight grid. A weights input the signature
                // missed would price leads off a stale grid until a global
                // reset, and would fail here rather than in the colony.
                Expect.sequenceEqual
                    (homeGridOf perturbed)
                    (homeGridOf colony)
                    "and the grid the walks flood over is bitwise the same"

                // The same pairing in the room the walk table only started
                // reading with #169: the far leg's entry is a pure function
                // of the *outpost's* grid, so a ColonyView the signature calls
                // equal has to lay that grid bitwise too. Perturbed out
                // there and not at home, or the assertion would be about the
                // home layer twice over: a creep standing in the outpost and
                // a raider beside it are vision facts, and a grid is
                // terrain, roads and obstacles alone.
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
            // alone (ADR 0032). Each test asserts the pairing rather than
            // the signature alone: the perturbation moves the grid the
            // recalled walks flood over, and it moves the signature they
            // are recalled on. A census that held still through one of them
            // would price leads off a grid the room has left.
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
                // The fourth weights input, and the one #169 made
                // load-bearing: the walk table's far-leg entry is a pure
                // function of the *goal* room's grid, so every input of
                // that grid has to be in the signature exactly as the home
                // room's are (ADR 0032). `World.seenFacts` folds
                // every scanned room's obstacle-kind construction sites
                // into that room's `Obstacles` — the engine refuses a creep
                // its own site wherever it stands — so a pending census
                // read in the home layer alone would leave an outpost's
                // closed tile unsigned, and a lead priced through ground
                // the successor cannot cross would be recalled for the life
                // of the census: ADR 0017's signature gap, in the room the
                // memo has just started reading.
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

                Expect.equal decision.Memo memo "the memo rides out unchanged for next tick"
            }

            test "an added structure invalidates the memo" {
                let memo = sentinelMemo (trunkColony 2)

                let perturbed =
                    trunkColony 2
                    |> withTarget "ext-3" { X = 26; Y = 26 } (Structure BuiltKind.Extension)

                let decision = decide perturbed Map.empty Set.empty (Some memo)
                let fresh = decide perturbed Map.empty Set.empty None

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
                let fresh = decide perturbed Map.empty Set.empty None

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
            }

            test
                "the Layout's records are census-derived: recalled with the plan, recomputed with it" {
                // #77's record, #106's and #107's join the site Intents and
                // the hauler quota under ADR 0017's standing invitation —
                // same census, same losses — so none is rederived per tick.
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
                let memoless = decide colony Map.empty Set.empty None

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
                // #107's record on the same seam, over a room that has in
                // fact lost a trunk: a matching signature hands back the
                // memo's own empty list rather than rederiving the loss,
                // and a moved one recomputes to exactly what a memoless
                // tick finds. ADR 0017's guarantee is that a recalled plan
                // reports what it reported when it was computed, and a
                // record that quietly recomputed itself would break it.
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
                let memoless = decide colony Map.empty Set.empty None

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
                let fresh = decide (trunkColony 3) Map.empty Set.empty None

                Expect.equal
                    (placementIntents decision.Intents)
                    (placementIntents fresh.Intents)
                    "a stale memo recomputes to exactly the fresh plan"
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
                let fresh = decide (trunkColony 3) Map.empty Set.empty None

                Expect.equal
                    (castNames stale)
                    (castNames fresh)
                    "a stale memo's quota is recomputed, never reused"
            }

            test "decide without a memo emits one keyed to this census" {
                let snapshot = trunkColony 2
                let decision = decide snapshot Map.empty Set.empty None

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
                // ADR 0032. The flood a lead is priced off reads nothing
                // but the census, so the next tick under the same signature
                // fills the table it was handed rather than one of its own:
                // a row the first tick never priced lands beside the first
                // tick's entry instead of in a table nobody keeps.
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

                let first = decide lone Map.empty Set.empty None

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
                    (decide joined Map.empty Set.empty None).Intents
                    "a recalled walk decides exactly what a fresh flood decides"
            }

            test "a moved census drops the whole walk table" {
                // The Layout's own granularity (ADR 0032): a moved
                // signature may have moved the weights or the body the walk
                // is priced for, and telling which is a dependency tracker
                // the memo does not have.
                let staffed = staffedColony [ worker "w1" 0 50 ] [ "w1", { X = 22; Y = 25 } ]

                let first = decide (staffed (trunkColony 2)) Map.empty Set.empty None

                let levelled =
                    decide (staffed (trunkColony 3)) Map.empty Set.empty (Some first.Memo)

                Expect.isFalse
                    (obj.ReferenceEquals(levelled.Memo.Walks, first.Memo.Walks))
                    "a level-up gets a table of its own"

                // The same moved census over a colony with no creep to lead
                // shows what "dropped whole" means: nothing at all rides
                // across, not merely a stale entry priced again.
                let emptied = decide (trunkColony 3) Map.empty Set.empty (Some first.Memo)

                Expect.equal emptied.Memo.Walks.Count 0 "the memo's table went with its signature"
            }
        ]
