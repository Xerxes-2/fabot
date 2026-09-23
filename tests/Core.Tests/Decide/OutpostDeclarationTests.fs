/// What a declared outpost is worth and what it asks for.
module Fabot.Core.Tests.Decide.OutpostDeclarationTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

/// The colony's one creep taken out of the home room and stood in the
/// outpost across the north border: where a body the concurrent-builder
/// cap has parked out there actually is, and the only place from which
/// the outpost's own site is the near target.
let private standingInOutpost (pos: Pos) (colony: ColonyView) =
    { colony with
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.empty
                })
            |> withNeighbour
                "W1N2"
                { SpatialInfo.layerOf colony.Spatial "W1N2" with
                    CreepPositions = Map.ofList [ "w", pos ]
                }
    }

[<Tests>]
let outpostTests =
    testList
        "outposts"
        [
            // An outpost's Task is ranked, not steered to the front or back of the
            // pool: both Harvests sit on the feeding tier, so what separates them
            // is travel cost, which crosses the Seam since #123.
            //
            // Pairwise, one rival at a time: this pool holds these two Tasks and
            // nothing else. The ranking and not the tick that follows it: what the
            // winner does with the tick is #142's, and the case below drives that.
            test "an outpost Harvest and a home Harvest are ranked in one pool" {
                Expect.equal
                    (matchOf (
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })
                    ))
                    (Some(taskId (Harvest "src-out"), MatchFactor.TravelCost))
                    "the outpost source is the nearer of the two, across the Seam"

                // The same fixture with the two sources swapped over.
                Expect.equal
                    (matchOf (
                        northBorderColony { X = 10; Y = 4 }
                        |> withNorthOutpost (Some { X = 10; Y = 41 })
                    ))
                    (Some(taskId (Harvest "src-home"), MatchFactor.TravelCost))
                    "the home source is the nearer of the two, and wins the same comparison"
            }

            // #235's live shape: the mother's workers matched the outpost rock's
            // Harvest across the Seam, nearer than her own, and the Seats they took
            // counted against that source's whole Total, so W12S27's own Anchor
            // read `none-free` on the Post it was standing on. A rock a six-Work
            // Anchor is already draining pays a light body nothing for the
            // crossing, so the mother's worker is not applicable to it at all.
            //
            // Pairwise on the garrison alone: a Post nobody is standing on is a
            // rock with its whole ten a tick spare.
            test "an outpost rock its Anchor drains is refused the mother's worker" {
                let colonyWith garrison =
                    let base' =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    let outpost = SpatialInfo.layerOf base'.Spatial "W1N2"

                    { base' with
                        Creeps = base'.Creeps @ garrison
                        // Reserved by us, so the rock prices at the held ten a tick, the
                        // number six Work overrun by two.
                        RoomControl = Map.add "W1N2" (reservedRoom true 4000) base'.RoomControl
                        Spatial =
                            { base'.Spatial with
                                TargetKinds =
                                    Map.add
                                        "can-out"
                                        (Structure BuiltKind.Container)
                                        base'.Spatial.TargetKinds
                            }
                            |> withNeighbour
                                "W1N2"
                                { outpost with
                                    TargetPositions =
                                        Map.add "can-out" { X = 10; Y = 45 } outpost.TargetPositions
                                    CreepPositions =
                                        garrison
                                        |> List.fold
                                            (fun acc (c: CreepInfo) ->
                                                Map.add c.Name { X = 10; Y = 45 } acc)
                                            outpost.CreepPositions
                                }
                    }

                Expect.equal
                    (matchOf (colonyWith []))
                    (Some(taskId (Harvest "src-out"), MatchFactor.TravelCost))
                    "a vacant Post leaves the whole rock spare, and the near source still wins"

                let garrisoned = colonyWith [ creepWith "a-out" 0 50 sixWork ]

                Expect.equal
                    (matchOf garrisoned)
                    (Some(taskId (Harvest "src-home"), MatchFactor.OnlyCandidate))
                    "the garrisoned rock is not the worker's to walk to; her own room is"

                let { Assignments = assignments } = decideOn garrisoned

                Expect.equal
                    (Map.tryFind "a-out" assignments)
                    (Some(taskId (Harvest "src-out")))
                    "and the Anchor keeps the Post nobody is displacing it from"
            }

            test "the winner of that comparison is walked toward the Seam, tick after tick" {
                // #142: before it, this fixture answered `Matched ("w",
                // "harvest:src-out", TravelCost)` and then a lone `SayCreep`: the
                // Task had a price and no step, and anti-thrash kept the creep there
                // for the rest of its life.
                //
                // The mover aims at the near side of the crossing the price was paid
                // at, a tile in the creep's own room, so nothing is arbitrated across
                // the border. This band is plain the whole way round and the corridor
                // meets it at x = 10, so x = 9, 10 and 11 cost the same to the tick
                // and the band's minimum takes the lowest (X, Y). The creep leaves the
                // corridor diagonally, which the engine allows onto an exit.
                let colonyAt pos =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    { colony with
                        Spatial = colony.Spatial |> withCreepsAt [ "w", pos ]
                    }

                let assigned = Map.ofList [ "w", taskId (Harvest "src-out") ]

                let {
                        Intents = opening
                        Assignments = assignments
                    } =
                    decideOn (colonyAt { X = 10; Y = 2 })

                Expect.equal
                    (Map.tryFind "w" assignments)
                    (Some(taskId (Harvest "src-out")))
                    "the premise: the outpost's Harvest wins the one worker"

                Expect.equal
                    (moveIntents opening)
                    [ "w", Top ]
                    "and it is walked up its own corridor toward the border, not parked on the Task"

                Expect.isEmpty
                    (actionIntents opening)
                    "it may not dig a source a room away, however well priced (ADR 0041)"

                // Driven the way the engine drives it: the creep stands where the last
                // tick's Intent put it, its Assignment handed back, until the step it
                // is given leaves this room's ground.
                let ground = Map.ofList (corridor 10 1 40)

                let rec drive pos walked =
                    if List.length walked > 10 then
                        failtest "the creep never reached a crossing"
                    else
                        let { Intents = intents } = decideFrom assigned (colonyAt pos)

                        match moveIntents intents with
                        | [ _, direction ] ->
                            let next = stepFrom pos direction

                            if Map.containsKey next ground then
                                drive next (next :: walked)
                            else
                                List.rev (next :: walked)
                        | _ -> List.rev walked

                Expect.equal
                    (drive { X = 10; Y = 2 } [])
                    [ { X = 10; Y = 1 }; { X = 9; Y = 0 } ]
                    "one tile up the corridor, then onto the exit the price was paid at"
            }

            test "and the tick after the crossing is the far room's: the landed creep walks on" {
                // The engine lifts the creep off (9,0) and files it in W1N2 on that
                // room's border row, and from that tick the Resolver arbitrates W1N2
                // as a room of its own (#145). Before #145 the far side was deferred,
                // and this case asserted the creep standing on its landing tile
                // holding its Task against anti-thrash: the trace #142 quotes.
                //
                // The landing tile is not ground (the ring is no room's floor) and the
                // tile beside it is; the mover answers from both, because a flood
                // seeds its start tile whatever that tile's weight.
                let landedAt pos =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.empty
                                })
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = TerrainGrid.ofList (corridor 10 40 48)
                                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 46 } ]
                                    CreepPositions = Map.ofList [ "w", pos ]
                                }
                    }

                let assigned = Map.ofList [ "w", taskId (Harvest "src-out") ]

                // This outpost carries no `RoomControl` entry, so the container rule
                // plans into no such room (`planOutpostContainers`); give the fixture
                // vision and a placement Intent joins the lines below.
                //
                // The tile the crossing above delivers to, two more ring and ground
                // tiles at the top of the corridor, and one a step from the Work Area.
                for pos, expected in
                    [
                        { X = 9; Y = 49 }, TopRight
                        { X = 10; Y = 49 }, Top
                        { X = 10; Y = 48 }, Top
                        { X = 10; Y = 44 }, Bottom
                    ] do
                    let landed = landedAt pos

                    let {
                            Intents = intents
                            Verdicts = verdicts
                        } =
                        decideFrom assigned landed

                    Expect.equal
                        intents
                        [ SayCreep("w", "⛏"); MoveCreep("w", expected) ]
                        $"out of {pos.X},{pos.Y} the creep is walked toward the source, and may not dig yet"

                    Expect.equal
                        verdicts
                        [ Verdict.Kept("w", taskId (Harvest "src-out")) ]
                        "and anti-thrash keeps the Task it is now walking to"

                // Driven from the landing tile: two steps up the corridor and the dig
                // begins.
                let rec drive pos walked =
                    if List.length walked > 10 then
                        failtest "the creep never started digging"
                    else
                        let { Intents = intents } = decideFrom assigned (landedAt pos)

                        match actionIntents intents, moveIntents intents with
                        | [ HarvestSource("w", "src-out") ], [] -> List.rev walked, pos
                        | [], [ _, direction ] ->
                            let next = stepFrom pos direction
                            drive next (next :: walked)
                        | _ -> failtest $"at {pos.X},{pos.Y} the tick neither walked nor dug"

                Expect.equal
                    (drive { X = 9; Y = 49 } [])
                    ([ { X = 10; Y = 48 }; { X = 10; Y = 47 } ], { X = 10; Y = 47 })
                    "off the landing tile onto the corridor, up to the seat, and the source is dug from there"
            }

            test "our site in the outpost is a Build the one pool holds" {
                // #150: the container rule placed a site in the outpost and nothing
                // ever built it, because the Build pool is `ColonyView.ConstructionSites`
                // mapped one to one and that list was the spawn rooms' alone.
                //
                // Nothing in the Build path is outpost-shaped: the Task names the site
                // by id, its Work Area is the site's own room's and its price sums the
                // legs over the Seam (#123). What was missing was the entry.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded

                Expect.equal
                    (matchOf sited)
                    (Some(taskId (Build "site-out"), MatchFactor.OnlyCandidate))
                    "the site a room away is a Task this worker is given"

                // The pool is that list and never the kind census: a site the shell
                // did not hand over, which is every site in a room the colony cannot
                // see this tick, names no Task at all.
                Expect.equal
                    (matchOf { sited with ConstructionSites = [] })
                    None
                    "and a site the ColonyView does not carry is no Task, however well the projection places it"

                // The memo *does* flinch at it (#169). #121 and #149 left the `pending`
                // half joined against the home layer alone because nothing the memo
                // carried read a site outside home. The walk table's far leg is now a
                // memo entry over the *goal* room's weight grid, and an obstacle-kind
                // site closes its tile in whatever room it stands in (`projectVisible`),
                // so a pending census stopping at the home layer is a signature gap out
                // there. Signing the half whole rather than only its blocking kinds
                // keeps one rule instead of a second asymmetry held in step with the
                // App's obstacle filter; the price is one Layout and one spawn walk
                // table thrown away on the tick an outpost container site appears.
                Expect.notEqual
                    (censusSignature sited)
                    (censusSignature (
                        northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None |> loaded
                    ))
                    "an outpost's pending site is a census entry of its own since #169"

                let { Intents = opening } = decideOn sited

                Expect.equal
                    (moveIntents opening)
                    [ "w", Top ]
                    "so the worker is walked up its own corridor toward the border"

                Expect.isEmpty
                    (actionIntents opening)
                    "and may not build a site a room away, however well priced (ADR 0041)"
            }

            test "the tick the outpost goes dark the worker crossing for its site is kept" {
                // #151: the reserver dies (a CLAIM body lives 600 ticks) and with it
                // goes the only vision W1N2 had: the site leaves `ConstructionSites`
                // and `build:site-out` leaves the pool. Every gate below the first in
                // the Matcher's keep cascade is about the *Task*, so the assignment
                // fell through the one gate that reads an empty lookup as a target
                // that is gone; the worker turned round with a full load, and a
                // half-built container can stand there for ever that way.
                //
                // The grace changes exactly that first gate, and it reads a fact about
                // looking rather than about the site: the room the id was last seen in
                // has not been seen since, and it went dark inside `Tuning.VisionGrace`.
                let crossing =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded

                // A tick a long way from zero, so the dark ticks the grace is read over
                // are ticks and not a fixture's arithmetic.
                let sited = { crossing with Time = 1000 }

                let held = taskId (Build "site-out")
                let assignments = Map.ofList [ "w", held ]

                // The room as the shell hands it over with no vision in it: the site
                // list is empty, the kind census and the tile go with it, and the one
                // thing left is the world's record of when the room was last looked
                // into and what stood in it then.
                let darkSince tick =
                    { sited with
                        ConstructionSites = []
                        Spatial =
                            { sited.Spatial with
                                TargetKinds = Map.remove "site-out" sited.Spatial.TargetKinds
                            }
                            |> withNeighbour
                                "W1N2"
                                { SpatialInfo.layerOf sited.Spatial "W1N2" with
                                    TargetPositions = Map.empty
                                }
                        Sightings =
                            Map.ofList
                                [
                                    "W1N2",
                                    {
                                        Tick = tick
                                        Targets = lazy (Set.singleton "site-out")
                                    }
                                ]
                    }

                let verdictsAt tick =
                    (decideFrom assignments (darkSince tick)).Verdicts

                // Pairwise on the grace and on nothing else: the only thing that moves
                // between the three readings is the tick the room was last seen at.
                Expect.contains
                    (decideFrom assignments sited).Verdicts
                    (Verdict.Kept("w", held))
                    "the premise, with the room in view: the site stands and the worker keeps the Build it is crossing for"

                Expect.contains
                    (verdictsAt 999)
                    (Verdict.Kept("w", held))
                    "the tick after the vision went, the worker is kept rather than sent home"

                Expect.contains
                    (verdictsAt 850)
                    (Verdict.Kept("w", held))
                    "and still kept at the last tick of the grace, 150 dark ticks on"

                Expect.contains
                    (verdictsAt 849)
                    (Verdict.Released("w", held, ReleaseReason.TaskGone))
                    "one tick past it the release is task-gone, as it always was: a container really destroyed is not held for ever"

                // What the grace buys, both halves: the assignment is handed to the
                // next tick, *and* the creep keeps walking the crossing, because it is
                // the arrival itself that ends the darkness.
                //
                // The mover aims it with the room the id was last seen in and asks the
                // border layer and the memoised terrain for the rest
                // (`Atlas.stepTowardRoom`), the route `cac0124` built for a crossing
                // creep. No remembered tile is laid into the layer and nothing is
                // priced off the sighting, so the Emitter still has no act to spell
                // for a target nobody can see.
                let {
                        Intents = waiting
                        Assignments = next
                    } =
                    decideFrom assignments (darkSince 999)

                Expect.equal
                    (Map.tryFind "w" next)
                    (Some held)
                    "the assignment is handed to the next tick"

                Expect.equal
                    (moveIntents waiting)
                    [ "w", Top ]
                    "and the worker walks on north for the Seam into the room it cannot see, exactly as it did with the vision"

                Expect.isEmpty
                    (waiting |> List.filter (fun intent -> moveIntents [ intent ] |> List.isEmpty))
                    "and nothing else: a target nobody can see is acted on by nobody"
            }

            test "the tick the vision comes back the Build is judged as it always was" {
                // The other half of #151's rule, and why it is a grace and not a
                // latch: nothing about a kept assignment survives the vision returning.
                let crossing =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded

                // A tick a long way from zero, as above.
                let sited = { crossing with Time = 1000 }

                let held = taskId (Build "site-out")
                let assignments = Map.ofList [ "w", held ]

                // Forty dark ticks behind it, and vision in the room this tick: the
                // sighting is stamped at the tick it is read.
                let backWithSite =
                    { sited with
                        Sightings =
                            Map.ofList
                                [
                                    "W1N2",
                                    {
                                        Tick = 1000
                                        Targets = lazy (Set.singleton "site-out")
                                    }
                                ]
                    }

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom assignments backWithSite

                Expect.contains
                    verdicts
                    (Verdict.Kept("w", held))
                    "the site is standing, so the worker keeps the Build it has been holding"

                Expect.equal
                    (moveIntents intents)
                    [ "w", Top ]
                    "and walks on up its corridor, the crossing it never turned back from"

                // The same tick with the site gone: a target that vanished under our
                // own eyes is gone.
                let backWithout =
                    { backWithSite with
                        ConstructionSites = []
                        Spatial =
                            { backWithSite.Spatial with
                                TargetKinds = Map.remove "site-out" backWithSite.Spatial.TargetKinds
                            }
                        Sightings =
                            Map.ofList
                                [
                                    "W1N2",
                                    {
                                        Tick = 1000
                                        Targets = lazy (Set.empty)
                                    }
                                ]
                    }

                Expect.contains
                    (decideFrom assignments backWithout).Verdicts
                    (Verdict.Released("w", held, ReleaseReason.TaskGone))
                    "and a site that finished or was cancelled while we watched releases on the tick it went"
            }

            test "and the worker that landed in the outpost builds it" {
                // The far half of the same walk: #145 arbitrates the outpost as a room
                // of its own, and the tick the creep stands inside the site's Work Area
                // (build reaches three tiles) the Intent is emitted.
                //
                // A slow answer and the right one: whoever holds this Task spends five
                // hits a tick per Work part into a 5,000-hit container. There is no
                // outpost builder row; which creep holds it is the ranking's answer.
                let landedAt pos =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded
                    |> standingInOutpost pos

                let assigned = Map.ofList [ "w", taskId (Build "site-out") ]

                let {
                        Intents = landing
                        Verdicts = verdicts
                    } =
                    decideFrom assigned (landedAt { X = 9; Y = 49 })

                Expect.equal
                    landing
                    [ SayCreep("w", "🔨"); MoveCreep("w", TopRight) ]
                    "off the landing tile onto the corridor, and no build from a tile out of range"

                Expect.equal
                    verdicts
                    [ Verdict.Kept("w", taskId (Build "site-out")) ]
                    "and anti-thrash keeps the Task it is now walking to"

                let rec drive pos walked =
                    if List.length walked > 10 then
                        failtest "the creep never started building"
                    else
                        let { Intents = intents } = decideFrom assigned (landedAt pos)

                        match actionIntents intents, moveIntents intents with
                        | [ BuildSite("w", "site-out") ], [] -> List.rev walked, pos
                        | [], [ _, direction ] ->
                            let next = stepFrom pos direction
                            drive next (next :: walked)
                        | _ -> failtest $"at {pos.X},{pos.Y} the tick neither walked nor built"

                Expect.equal
                    (drive { X = 9; Y = 49 } [])
                    ([ { X = 10; Y = 48 }; { X = 10; Y = 47 }; { X = 10; Y = 46 } ],
                     { X = 10; Y = 46 })
                    "off the ring onto the corridor, down to build range, and the container rises"
            }

            test "the site outranks the home Upgrade: a loaded worker crosses the Seam for it" {
                // #157, and the reverse of what this fixture asserted before it. The
                // colony that really exists has a controller, and while the site was
                // surplus work that controller took every loaded worker every tick: a
                // loaded worker at home is a corridor from its own controller and a
                // Seam plus fifty tiles from the site.
                //
                // Deployed, that was the switch laid down and never closed: the
                // reserver went out (#131), the site went up (#128), and nobody ever
                // built it. #150's answer, a creep that had walked out for this room's
                // own Harvest, never happened either, because the Storage's Withdraw
                // is feeding tier and a few tiles from home.
                //
                // So this Build is feeding tier now (`tierOf`). The factor is `Rank`
                // and not `TravelCost`: the site is still much the farther target and
                // wins anyway. Pairwise, one rival at a time.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded
                    |> withHomeController { X = 10; Y = 5 }

                Expect.equal
                    (matchOf sited)
                    (Some(taskId (Build "site-out"), MatchFactor.Rank))
                    "the switch outranks the sink, however much nearer the sink stands"

                let { Intents = opening } = decideOn sited

                Expect.equal
                    (moveIntents opening)
                    [ "w", Top ]
                    "and the worker is walked up its own corridor toward the Seam it has to cross"

                Expect.isEmpty
                    (actionIntents opening)
                    "having neither built a site a room away nor upgraded the controller beside it"
            }

            test "and the worker already in the outpost still builds it" {
                // The other half, unmoved by #157: a creep standing in the outpost held
                // this Task on the surplus tier and holds it on the feeding one.
                let landed =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded
                    |> withHomeController { X = 10; Y = 5 }
                    |> standingInOutpost { X = 10; Y = 46 }

                Expect.equal
                    (matchOf landed)
                    (Some(taskId (Build "site-out"), MatchFactor.Rank))
                    "the creep out there builds, as it did before the tier moved"
            }

            test "a hungry extension still comes first: same tier, and the nearer target wins" {
                // What keeps a starving spawn from waiting on a container fifty tiles
                // away with no special case: the spawn and the extensions were always
                // on the feeding tier and the outpost's site has joined them, so travel
                // cost separates them and a hungry extension underfoot is nearer.
                //
                // Pairwise: no controller here, so the pool is the Build, the Refill
                // and a home Harvest a full body cannot take.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded

                Expect.equal
                    (matchOf sited)
                    (Some(taskId (Build "site-out"), MatchFactor.OnlyCandidate))
                    "the premise: with nothing at home to fill, this worker crosses for the site"

                let hungry =
                    { sited with
                        Refillables = [ refillable "ext-1" 50 BuiltKind.Extension ]
                        Spatial =
                            { sited.Spatial with
                                TargetKinds =
                                    Map.add
                                        "ext-1"
                                        (Structure BuiltKind.Extension)
                                        sited.Spatial.TargetKinds
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    TargetPositions =
                                        Map.add "ext-1" { X = 10; Y = 3 } layer.TargetPositions
                                })
                    }

                Expect.equal
                    (matchOf hungry)
                    (Some(taskId (Refill("ext-1", Energy)), MatchFactor.TravelCost))
                    "and one extension with room in it takes the same worker back, on cost alone"
            }

            test "a home container site is surplus still: the room is what makes one a switch" {
                // The half of #157 that must not move: what makes the outpost's site a
                // switch is the room it stands in and not the kind, and a `Pos` carries
                // no room, so a reading by the kind census alone would lift every
                // container the Layout places onto the feeding tier.
                //
                // Against the **flow** since #234: this site stands at home, so the
                // rung reaches it and the controller underfoot stopped being an
                // instrument. A hungry extension placed **farther** than the site
                // tells the two readings apart: read as a switch the site ties that
                // Refill on the feeding tier and wins on price; read as surplus, the
                // Refill outranks it outright. Pairwise: one Build, one Refill, and an
                // Upgrade the rung leaves cheapest to neither.
                let homeSite =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> loaded
                        |> withHomeController { X = 10; Y = 5 }
                        |> withHungryExtension { X = 10; Y = 40 }

                    { colony with
                        ConstructionSites = [ { Id = "site-home"; Left = siteOwes } ]
                    }
                    |> withTarget "site-home" { X = 10; Y = 30 } (Site BuiltKind.Container)

                Expect.equal
                    (matchOf homeSite)
                    (Some(taskId (Refill("ext-1", Energy)), MatchFactor.Rank))
                    "the colony's own container site is surplus work, and the flow outranks it"
            }

            test "an ordinary outpost site keeps its travel cost: #234's rung stops at home" {
                // The other half of #234's rung, and why it reads the site's room
                // (`isHomeSite`). Out here the rung would walk the whole worker row
                // over the Seam at once; it is `Tuning.OutpostBuilders` and never the
                // rung that answers whether a body crosses (#266), and everything
                // behind the lifted sites stays surplus, priced against an Upgrade the
                // body is already standing in the Work Area of.
                //
                // So the queue is one site longer than the budget, and the third is
                // the one this case is about. Roads on purpose, so the container rule
                // cannot be what answers, and the whole row is loaded inside the home
                // controller's Work Area.
                let crowd =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostTrunk
                        [
                            "site-near", BuiltKind.Road, { X = 10; Y = 47 }
                            "site-mid", BuiltKind.Road, { X = 10; Y = 45 }
                            "site-far", BuiltKind.Road, { X = 10; Y = 41 }
                        ]
                    |> withHomeController { X = 10; Y = 5 }
                    |> threeLoadedAtHome

                Expect.equal
                    (heldBy crowd)
                    [
                        taskId (Build "site-mid"), 1
                        taskId (Build "site-near"), 1
                        taskId (Upgrade "ctrl-1"), 1
                    ]
                    "the site the budget did not reach is priced, not ranked: the body left over upgrades"
            }

            test "the trunk is paved from the Seam outward, and the container jumps the queue" {
                // #266, as W13S29 reported it: two containers standing, 45 hand-laid
                // road sites at 0/300, and the whole worker row at home. A road in an
                // outpost was a plain surplus Build, so travel cost answered 120
                // against an Upgrade underfoot costing nothing and nobody ever
                // crossed, and the trunk is what the hauler's round trip is priced on.
                //
                // The first `Tuning.OutpostBuilders` sites in the queue are
                // feeding-tier and the rest are not. The queue is the container first
                // (here deliberately the **farthest** site of the six, so nothing but
                // the kind can put it in front) and then the walk out to the Seam
                // (`Atlas.seamWalkTicks`), nearest first, because the paved tiles
                // beside the crossing are the ones every haul walks over.
                let trunk =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostTrunk
                        [
                            "site-can", BuiltKind.Container, { X = 10; Y = 40 }
                            "site-r1", BuiltKind.Road, { X = 10; Y = 47 }
                            "site-r2", BuiltKind.Road, { X = 10; Y = 46 }
                            "site-r3", BuiltKind.Road, { X = 10; Y = 45 }
                            "site-r4", BuiltKind.Road, { X = 10; Y = 44 }
                            "site-r5", BuiltKind.Road, { X = 10; Y = 43 }
                        ]
                    |> withHomeController { X = 10; Y = 5 }
                    |> threeLoadedAtHome

                Expect.equal
                    (heldBy trunk)
                    [
                        taskId (Build "site-can"), 1
                        taskId (Build "site-r1"), 1
                        taskId (Upgrade "ctrl-1"), 1
                    ]
                    "the switch and the site beside the crossing take one builder each, and the third stays home"

                // And the queue moves: nothing schedules it, the pool is recomputed
                // from the sites that are left and the head of it is the answer.
                let paved =
                    { trunk with
                        ConstructionSites =
                            trunk.ConstructionSites
                            |> List.filter (fun site ->
                                site.Id <> "site-can" && site.Id <> "site-r1")
                    }

                Expect.equal
                    (heldBy paved)
                    [
                        taskId (Build "site-r2"), 1
                        taskId (Build "site-r3"), 1
                        taskId (Upgrade "ctrl-1"), 1
                    ]
                    "two finished and the next two out take their places: the trunk grows from the Seam"
            }

            test "two builders cross for the site, and the third stays home" {
                // The cap `planPool` puts on this Build (#157): on the feeding tier the
                // site outbids the home Upgrade for every loaded worker at once, and a
                // Seam away is a Seam away from every tile of one corridor.
                //
                // Two is a tunable and the third worker is what reads it: rejected as
                // capacity-full, it falls to the Upgrade. Asserted as the whole tally.
                // Two is the whole colony's budget and not this site's; the test below
                // opens a second site and reads them apart.
                let crowd = crowdAtOutpostSite (northBorderColony { X = 10; Y = 38 })

                let { Assignments = assignments } = decideOn crowd

                Expect.equal
                    (assignments |> Map.toList |> List.map snd |> List.countBy id |> List.sort)
                    [ taskId (Build "site-out"), 2; taskId (Upgrade "ctrl-1"), 1 ]
                    "two of the three hold the site, and the one left over upgrades"
            }

            test "the whole ring closes at `decide`: cross, build it empty, dig it full, build on" {
                // The switch closing under its own power, end to end (#157): a
                // controller at home, a rock and a container site in the outpost, one
                // loaded worker at home. It crosses because the site outranks the
                // controller (`tierOf`); emptied, the Build goes inapplicable and the
                // outpost's own rock a step away is the cheapest Task it has; full
                // again, the site outranks everything once more. No "go home" act.
                //
                // Driven the way the engine drives it: a step onto the exit row is
                // handed over to the neighbour's own border row (#145). What a build
                // spends and a dig collects is the engine's arithmetic, so the two act
                // on the store at their limits, emptied and filled.
                let colonyAt room pos carrying =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })
                        |> withOutpostSite { X = 10; Y = 44 }
                        |> withHomeController { X = 10; Y = 5 }

                    let standing (name: string) (layer: RoomLayer) =
                        { layer with
                            CreepPositions =
                                if name = room then Map.ofList [ "w", pos ] else Map.empty
                        }

                    { colony with
                        Creeps = [ (if carrying then worker "w" 50 0 else worker "w" 0 50) ]
                        Spatial =
                            colony.Spatial
                            |> withHome (standing "W1N1")
                            |> withNeighbour
                                "W1N2"
                                (SpatialInfo.layerOf colony.Spatial "W1N2" |> standing "W1N2")
                    }

                let rec drive (room, pos, carrying) assigned trail ticks =
                    if ticks = 0 then
                        List.rev trail
                    else
                        let {
                                Intents = intents
                                Assignments = next
                            } =
                            decideFrom assigned (colonyAt room pos carrying)

                        let step state acted =
                            drive state next (acted :: trail) (ticks - 1)

                        match actionIntents intents, moveIntents intents with
                        | [ BuildSite("w", "site-out") ], [] -> step (room, pos, false) "build"
                        | [ HarvestSource("w", "src-out") ], [] -> step (room, pos, true) "harvest"
                        | [], [ _, direction ] ->
                            let next = stepFrom pos direction

                            // The engine's own handover: a creep ending its tick on the exit row
                            // is filed on the neighbour's opposite border row, same column.
                            if room = "W1N1" && next.Y = 0 then
                                step ("W1N2", { next with Y = 49 }, carrying) "cross"
                            else
                                step (room, next, carrying) "walk"
                        | actions, moves ->
                            failtest $"in {room} at {pos.X},{pos.Y}: {actions} and {moves}"

                Expect.equal
                    (drive ("W1N1", { X = 10; Y = 2 }, true) Map.empty [] 10)
                    [
                        "walk"
                        "cross"
                        "walk"
                        "walk"
                        "build"
                        "harvest"
                        "build"
                        "harvest"
                        "build"
                        "harvest"
                    ]
                    "up the corridor, over the Seam, down to the site — and then the ring turns"
            }

            test "the switch is light bodies' work: a full Anchor stays on its Post" {
                // What the feeding tier took away and `applicable` gives back (#157).
                // Travel cost was the only thing keeping a heavy body off a distant
                // site, and a rank the whole colony shares is what travel cost cannot
                // answer: a full Anchor whose Post carries no standing container yet
                // loses Harvest (`garrisons`), was outranked off its own controller and
                // walked fifty tiles to spend one Carry into a 5,000-progress site.
                // This one Build is inapplicable to a heavy body.
                //
                // The two bodies stand on the same tile, so what tells them apart is
                // the body and nothing geometric.
                let sited body =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> withHomeController { X = 10; Y = 5 }

                    { colony with
                        Creeps = [ body "w" 50 0 ]
                    }

                Expect.equal
                    (matchOf (sited worker))
                    (Some(taskId (Build "site-out"), MatchFactor.Rank))
                    "the premise: a generalist is walked over the Seam for the site"

                Expect.equal
                    (matchOf (sited anchor))
                    (Some(taskId (Upgrade "ctrl-1"), MatchFactor.OnlyCandidate))
                    "and the Anchor beside it has no such Task at all: it spends its Carry where it stands"

                Expect.isEmpty
                    (moveIntents (decideOn (sited anchor)).Intents)
                    "and takes no step toward a border it would spend hundreds of ticks crossing"
            }

            test "what shares this tier and what only looks like it does" {
                // The half of #157's Implementation decisions that is not true as the
                // ticket wrote it: "Refill still comes first, the home extension /
                // tower is nearer, cost decides". A **tower** Refill is surplus tier,
                // so cost never gets asked. Two pairwise cases, one rival each.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }

                // A tower with 500 free, three tiles from the worker, against a site a
                // Seam and fifty tiles away: it loses on rank and distance is never
                // reached, a real change of behaviour under a raid at home. The answer
                // for a raid is the stand-down (#136).
                let tower =
                    { (sited |> loaded) with
                        Refillables = [ refillable "tower-1" 500 BuiltKind.Tower ]
                    }
                    |> withTarget "tower-1" { X = 10; Y = 3 } (Structure BuiltKind.Tower)

                Expect.equal
                    (matchOf tower)
                    (Some(taskId (Build "site-out"), MatchFactor.Rank))
                    "a hungry tower is surplus work and is outranked outright, not beaten on distance"

                // The spawn does share the tier, so cost decides from where the creep
                // stands: for the one parked in the outpost the nearer target is the
                // site, not the spawn.
                let outThere =
                    { (sited |> loaded) with
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                    }
                    |> withTarget "spawn-1" { X = 10; Y = 2 } (Structure BuiltKind.Spawn)
                    |> standingInOutpost { X = 10; Y = 44 }

                Expect.equal
                    (matchOf outThere)
                    (Some(taskId (Build "site-out"), MatchFactor.TravelCost))
                    "and a hungry spawn does share it, so the creep already out there builds on rather than walking home"
            }

            test "the two builders are the colony's budget, not each site's" {
                // `planPool`'s cap read at colony scale (#157): `planOutpostContainers`
                // places one site per unserved outpost source on the same tick, so a
                // per-site two over three sources is a colony-wide six, the whole
                // worker row. Spread instead: two sites take one apiece.
                let crowd =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> withHomeController { X = 10; Y = 5 }

                    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                    { colony with
                        ConstructionSites =
                            colony.ConstructionSites @ [ { Id = "site-out2"; Left = siteOwes } ]
                        Creeps = [ for n in 1..5 -> worker $"w{n}" 50 0 ]
                        Spatial =
                            { colony.Spatial with
                                TargetKinds =
                                    Map.add
                                        "site-out2"
                                        (Site BuiltKind.Container)
                                        colony.Spatial.TargetKinds
                            }
                            |> withCreepsAt [ for n in 1..5 -> $"w{n}", { X = 10; Y = n + 1 } ]
                            |> withNeighbour
                                "W1N2"
                                { outpost with
                                    TargetPositions =
                                        Map.add
                                            "site-out2"
                                            { X = 10; Y = 45 }
                                            outpost.TargetPositions
                                }
                    }

                let { Assignments = assignments } = decideOn crowd

                Expect.equal
                    (assignments |> Map.toList |> List.map snd |> List.countBy id |> List.sort)
                    [
                        taskId (Build "site-out"), 1
                        taskId (Build "site-out2"), 1
                        taskId (Upgrade "ctrl-1"), 3
                    ]
                    "two switches open take one builder each, and three of the five stay home"
            }

            test
                "the two rooms are arbitrated apart: a neighbour's creep holds no tile of this room" {
                // #142's acceptance criterion 5: each room's arbitration reads that
                // room's creeps and no other's (#145). A `Map<Pos, string>` of
                // occupants has no room on its key, so a creep on the same coordinate
                // of the neighbouring room is not an occupant here. Pairwise, one
                // rival at a time: the home traveller and one creep on its next tile,
                // first in the neighbour, then at home.
                //
                // The bystanders are full, so no Harvest applies to them and they park
                // where they stand.
                let colony (homeCreeps: (string * Pos) list) (outpostCreeps: (string * Pos) list) =
                    let colonyOf =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    { colonyOf with
                        Creeps =
                            worker "w" 0 50
                            :: [ for name, _ in homeCreeps @ outpostCreeps -> worker name 50 0 ]
                        Spatial =
                            colonyOf.Spatial
                            |> withCreepsAt (("w", { X = 10; Y = 4 }) :: homeCreeps)
                            // The outpost's corridor runs the whole column, so the coordinate the
                            // rival stands on is ground in both rooms.
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = TerrainGrid.ofList (corridor 10 1 48)
                                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 46 } ]
                                    CreepPositions = Map.ofList outpostCreeps
                                }
                    }

                // The home source is the rival this time, at (10,38) with the worker
                // at (10,4) walking down to it; its next tile is (10,5).
                let assigned = Map.ofList [ "w", taskId (Harvest "src-home") ]

                let outcome homeCreeps outpostCreeps =
                    let {
                            Intents = intents
                            Verdicts = verdicts
                        } =
                        decideFrom assigned (colony homeCreeps outpostCreeps)

                    moveIntents intents,
                    verdicts
                    |> List.filter (function
                        | Verdict.Yielded _
                        | Verdict.Grounded _ -> true
                        | _ -> false)

                Expect.equal
                    (outcome [] [ "o", { X = 10; Y = 5 } ])
                    ([ "w", Bottom ], [])
                    "a neighbour's creep on the next tile's coordinate is no occupant: the traveller steps, nobody yields"

                Expect.equal
                    (outcome [ "h", { X = 10; Y = 5 } ] [])
                    ([ "w", Bottom; "h", Top ], [ Verdict.Yielded("h", "w") ])
                    "a home creep on the next tile is displaced — swapped past the traveller — and yields, as it always has"

                Expect.equal
                    (outcome [ "h", { X = 10; Y = 5 } ] [ "o", { X = 10; Y = 5 } ])
                    ([ "w", Bottom; "h", Top ], [ Verdict.Yielded("h", "w") ])
                    "and the neighbour's creep on the same coordinate changes neither the moves nor the attribution"
            }

            test
                "a grounded creep in the neighbouring room is grounded there, and pre-claims nothing here" {
                // A fatigued creep sits its own room's arbitration out, so it is
                // reported Grounded (the Verdict it was denied while only home was
                // arbitrated) and its tile is blocked in its room only. Before #145
                // this held by the creep not being arbitrated at all; now by the room.
                let colony =
                    let colonyOf =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    { colonyOf with
                        Creeps = [ worker "w" 0 50; { worker "o" 50 0 with Fatigue = 4 } ]
                        Spatial =
                            colonyOf.Spatial
                            |> withCreepsAt [ "w", { X = 10; Y = 4 } ]
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = TerrainGrid.ofList (corridor 10 1 48)
                                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 46 } ]
                                    CreepPositions = Map.ofList [ "o", { X = 10; Y = 5 } ]
                                }
                    }

                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideFrom (Map.ofList [ "w", taskId (Harvest "src-home") ]) colony

                Expect.equal
                    (moveIntents intents)
                    [ "w", Bottom ]
                    "the home traveller steps onto (10,5) of its own room"

                Expect.contains
                    verdicts
                    (Verdict.Grounded "o")
                    "and the tired creep in the neighbour is grounded there"

                Expect.isEmpty
                    (verdicts
                     |> List.filter (function
                         | Verdict.Yielded _ -> true
                         | _ -> false))
                    "nobody yields to a creep a room away"
            }

            test "a creep on the far room's ring is never settled there: it walks inward" {
                // A creep that ends its tick on the border row is moved out of the
                // room by the engine, so a landed creep the Resolver leaves standing
                // would be bounced back across the border and re-cross the next tick.
                // Two cases, one branch of the mover each. Parked: a full creep with
                // nothing applicable on the landing tile is walked onto the outpost's
                // ground. Travelling: two landed creeps whose cheapest step is the
                // same ground tile, and the one that yields is handed the other
                // ground tile beside it.
                let colonyWith creeps (outpostCreeps: (string * Pos) list) =
                    let colonyOf =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    { colonyOf with
                        Creeps = creeps
                        Spatial =
                            colonyOf.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.empty
                                })
                            // One landing tile beside the corridor's top, so the ring has two
                            // ground tiles to step onto and a contested step has somewhere to go.
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain =
                                        TerrainGrid.ofList (
                                            corridor 10 40 48 @ [ { X = 9; Y = 48 }, Plain ]
                                        )
                                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 46 } ]
                                    CreepPositions = Map.ofList outpostCreeps
                                }
                    }

                let { Intents = parked } =
                    decideOn (colonyWith [ worker "a" 50 0 ] [ "a", { X = 9; Y = 49 } ])

                Expect.equal
                    (moveIntents parked)
                    [ "a", Top ]
                    "a full creep with no Task steps off the landing tile onto the ground beside it"

                let {
                        Intents = contested
                        Verdicts = verdicts
                    } =
                    decide
                        (colonyWith
                            [ worker "a" 0 50; worker "e" 0 50 ]
                            [ "a", { X = 10; Y = 49 }; "e", { X = 9; Y = 49 } ])
                        (Map.ofList
                            [ "a", taskId (Harvest "src-out"); "e", taskId (Harvest "src-out") ])
                        Set.empty
                        None

                Expect.equal
                    (moveIntents contested)
                    [ "a", TopLeft; "e", TopRight ]
                    "both want (9,48); the first takes it and the second steps onto (10,48) rather than staying on the ring"

                Expect.contains
                    verdicts
                    (Verdict.Yielded("e", "a"))
                    "and the step it gave up is attributed as a yield, as any other is"
            }

            test "a parked outpost creep is displaced onto its own room's ground, never home's" {
                // Each room's arbitration uses that room's tiles, at the displacement
                // seam: home has ground at (9,5) and the outpost has none there, so a
                // parked outpost creep pushed off (10,5) must be swapped up the
                // corridor, never sent Left onto a coordinate only walkable at home.
                let colony =
                    let colonyOf =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    { colonyOf with
                        Creeps = [ worker "t" 0 50; worker "o" 50 0 ]
                        Spatial =
                            colonyOf.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain =
                                        TerrainGrid.ofList (
                                            corridor 10 1 40 @ [ { X = 9; Y = 5 }, Plain ]
                                        )
                                    CreepPositions = Map.empty
                                })
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = TerrainGrid.ofList (corridor 10 1 48)
                                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 46 } ]
                                    CreepPositions =
                                        Map.ofList
                                            [ "t", { X = 10; Y = 4 }; "o", { X = 10; Y = 5 } ]
                                }
                    }

                let { Intents = intents } =
                    decideFrom (Map.ofList [ "t", taskId (Harvest "src-out") ]) colony

                Expect.equal
                    (moveIntents intents)
                    [ "t", Bottom; "o", Top ]
                    "the traveller takes (10,5) and the parked creep swaps past it up the outpost's corridor"
            }

            test "a neighbour room of bare ground changes nothing" {
                // Totality over a room layer carrying terrain and a border ring and
                // nothing else: every query answers empty, so the tick decides exactly
                // what it decided with no such room at all.
                //
                // Not a blind outpost, which this test claimed to be until #148: a
                // declared room the colony cannot see carries its sources and its
                // controller all the same (`Outpost.place`), and the tests further
                // down pin what *that* decides.
                //
                // Read at the top seam over all three outputs at once, because the
                // ways this could go wrong are not local: a second room's weight grid
                // consulted for a home price, a Seam band admitting a crossing to
                // nowhere, a Task pooled off a layer with nothing in it.
                let colony = northBorderColony { X = 10; Y = 38 }

                Expect.contains
                    (let _, _, verdicts = outcomeOf colony in verdicts)
                    (Verdict.Matched("w", taskId (Harvest "src-home"), MatchFactor.OnlyCandidate))
                    "the premise: this colony decides something, so an equal outcome says something"

                Expect.equal
                    (outcomeOf (colony |> withNorthOutpost None))
                    (outcomeOf colony)
                    "a room with no geometry decides exactly what no room at all decides"
            }

            test
                "the declared outposts are ADR 0042's north room, the survey's south one and the third colony's room two hops east" {
                // #124 landed this constant empty and pinned the emptiness; this is
                // where that pin turns over: the rooms, in the order a human wrote
                // them, and the scan set the shell takes from them.
                //
                // Read through the colony that declares them: the home room is the
                // key the shell looks them up under (`ColonyView.ofWorld` takes it
                // off the first spawn). A home nobody declared answers with none.
                //
                // Which ids and which tiles is a claim about the committed captures,
                // pinned where the captures are read (`RoomOutpostTests`) and never
                // retyped here: two literals of the same ids would agree with each
                // other and with nothing else.
                let outposts = Colony.outpostsOf Colony.declared "W12S28"

                Expect.equal
                    (Colony.homes Colony.declared)
                    [ "W12S28"; "W13S28"; "W15S28"; "W11S29"; "W11S27" ]
                    "five colonies are declared, in the order a human wrote them (ADR 0047)"

                Expect.equal
                    (outposts |> List.map (fun outpost -> outpost.RoomName))
                    [ "W12S27"; "W11S28" ]
                    "the north outpost and, since 2026-09-16, the west one: the room ADR 0042's pair called west is a colony of its own now (ADR 0047), and W11S28 is a room further west again (`docs/research/outpost-wave-2.md`)"

                Expect.equal
                    (Outpost.adr0042 |> List.map (fun outpost -> outpost.RoomName))
                    [ "W12S27"; "W13S28" ]
                    "while ADR 0042's measured pair is kept whole for the real-terrain fixtures"

                Expect.equal
                    (Colony.outpostsOf Colony.declared "W13S28"
                     |> List.map (fun outpost -> outpost.RoomName))
                    [ "W13S29"; "W11S27" ]
                    "and the second colony works its south outpost, and the fifth colony's room while that Claim is pending (2026-09-24). W14S28 went to W15S28 on 2026-09-20, which stands 46 tiles from its rock where this colony stood 181 ticks from it, and five rocks were turning into 15.7 e/t of controller progress here against four rocks making 30.7 at W12S28, the difference banked and standing still"

                // What the removal cost while it was overdue (#352): W13S28 ran an
                // anchor on W11S29's rock **three crossings out** while W11S29's own
                // colony ran one on the same rock, and this colony's demand went 2,780
                // over two haulers to 5,760 over four. A claimed room in both lists
                // reads as a room we mine, and two colonies then mine it.
                Expect.isFalse
                    (Colony.outpostsOf Colony.declared "W13S28"
                     |> List.exists (fun outpost -> outpost.RoomName = "W11S29"))
                    "no colony mines a room that has a colony of its own"

                Expect.equal
                    (Outpost.w11s29.RoomName)
                    "W11S29"
                    "while the declaration itself is kept written for the record: it is how the fourth colony was taken, and ADR 0047's arrangement needs it readable"

                // W15S28 is declared and is **not** an outpost of anybody's: it is
                // owned, so its mother raises it and does not mine it, and
                // `childrenWhere` gives a room in both lists to the outpost list, the
                // classification that stalled it live on 2026-09-10 (claimed, spawn
                // site placed by hand, no body sent). What the mother projects for it
                // is `Colony.roomsProjected`'s bootstrap half, transit room included.
                Expect.isFalse
                    (Colony.declared
                     |> List.exists (fun colony ->
                         colony.Outposts
                         |> List.exists (fun outpost -> outpost.RoomName = "W15S28")))
                    "the third colony's room is nobody's outpost now that it is ours"

                // The room between home and the nursery is in the projection on the
                // *bootstrap* half's own account; since 2026-09-20 that is again the
                // only account, W14S28 being W15S28's outpost now, so from here it is
                // a transit room and sorts **after** the declared rooms. The order is
                // the proof.
                Expect.equal
                    (Colony.roomsProjected
                        (Colony.outpostsOf Colony.declared "W13S28")
                        (Colony.errandsOf Colony.declared "W13S28")
                        [ "W15S28" ]
                        "W13S28")
                    [
                        "W13S28"
                        "W13S29"
                        "W11S27"
                        "W13S27"
                        "W12S27"
                        "W12S28"
                        "W11S28"
                        "W15S28"
                        "W14S28"
                    ]
                    "the home, its outposts with the rectangle three crossings to W11S27 spans (until that Claim lands and the room leaves this list), and the child two hops out, with W14S28 behind them: it is W15S28's since 2026-09-20, so it reaches this scan set as a transit room and sorts after the declared ones"

                // Three rooms left this scan set with W11S29's declaration (#352): the
                // nursery itself, W12S29 and W11S28, plus W12S28's own home, which was
                // in here only as a corner of the rectangle `transitBetween` names for
                // a three-hop chain. Four rooms of terrain, borders and census no
                // longer read every tick, in the middle of a CPU squeeze
                // (`docs/research/cpu-headroom.md`). W11S27's Claim window
                // (2026-09-24) puts W12S28 and W11S28 back, as corners of its own
                // three-hop rectangle, until that room leaves the outpost list.
                //
                // What replaces it for the nursery is the **bootstrap** half: a room a
                // mother raises is projected because she raises it. Named as
                // bootstrapped, it and its transit room come straight back.
                Expect.equal
                    (Colony.roomsProjected
                        (Colony.outpostsOf Colony.declared "W13S28")
                        (Colony.errandsOf Colony.declared "W13S28")
                        [ "W15S28"; "W11S29" ]
                        "W13S28")
                    [
                        "W13S28"
                        "W13S29"
                        "W11S27"
                        "W13S27"
                        "W12S27"
                        "W12S28"
                        "W11S28"
                        "W15S28"
                        "W14S28"
                        "W11S29"
                        "W12S29"
                    ]
                    "the nursery rides the bootstrap half and brings the whole rectangle `transitBetween` names for a three-hop chain: W11S29 and W12S29 here, W12S28 and W11S28 being in already by W11S27's (ADR 0058)"

                // The third colony's three since 2026-09-20: W15S27 is the room the
                // delivery route crosses on the way to the Reactor, and W15S29 is the
                // one declared for its own sake (2026-09-17, `outpost-wave-2.md`) once
                // the survey's CPU refusal stopped being the price: +0.55 ms of
                // `decide` on today's code against the +1.5-2.0 it measured.
                Expect.equal
                    (Colony.outpostsOf Colony.declared "W15S28"
                     |> List.map (fun outpost -> outpost.RoomName))
                    [ "W15S27"; "W15S29"; "W14S28" ]
                    "the third colony works the room its errand crosses, the one declared for its own sake, and since 2026-09-20 the room next door that was always nearer to it than to W13S28: W15S29 was withdrawn on 2026-09-17 after one invader killed four bodies in it and re-declared on 2026-09-18 (#369), and W14S28 came across because its rock stands six tiles from this colony's border and forty-three from the other's"

                // The fourth colony's, declared the day after its spawn stood
                // (`w12s29-outpost.md`): 81 ticks of haul, the cheapest in the
                // programme, against 380 of demand over one hauler.
                Expect.equal
                    (Colony.outpostsOf Colony.declared "W11S29"
                     |> List.map (fun outpost -> outpost.RoomName))
                    [ "W12S29" ]
                    "the fourth colony works the room on its doorstep"

                Expect.isEmpty
                    (Colony.outpostsOf Colony.declared "W1N1")
                    "and a room nobody declared a colony for works no outposts at all"

                Expect.equal
                    (Outpost.roomsProjected outposts "W12S28")
                    [ "W12S28"; "W12S27"; "W11S28" ]
                    "so the mother's projection covers the home room and both its outposts — and W11S28 was in its scan set already, as a transit room of the chain to the nursery, which is exactly why that declaration is the cheapest tick of the three the wave-2 survey priced"
            }

            test "a declared outpost joins the spawn room in the set the shell scans" {
                // Written in the engine's own ids: the projection keys every target by
                // the id the server hands back, so a constant written in the captures'
                // readable short names would match nothing on a live server, in silence.
                let north =
                    {
                        RoomName = "W12S27"
                        Sources =
                            [ "6a8caabadd4872bccd3194a6", { Room = "W12S27"; X = 16; Y = 45 } ]
                        Controller = "6a8caabadd4872bccd3194a5", { Room = "W12S27"; X = 37; Y = 43 }
                    }

                let west =
                    {
                        RoomName = "W13S28"
                        Sources =
                            [
                                "6a8caaaddd4872bccd319362", { Room = "W13S28"; X = 16; Y = 7 }
                                "6a8caaaddd4872bccd319361", { Room = "W13S28"; X = 18; Y = 4 }
                            ]
                        Controller = "6a8caaaddd4872bccd319363", { Room = "W13S28"; X = 24; Y = 17 }
                    }

                Expect.equal
                    (Outpost.roomsProjected [ north; west ] "W12S28")
                    [ "W12S28"; "W12S27"; "W13S28" ]
                    "the spawn room, then the declarations in their own order"

                // A declaration naming the spawn room is a human's slip in a constant a
                // human moves, and the projection keys rooms by name: scanning that
                // room twice would file one room's geometry under one name twice over.
                Expect.equal
                    (Outpost.roomsProjected [ { north with RoomName = "W12S28" } ] "W12S28")
                    [ "W12S28" ]
                    "a room declared twice is scanned once"
            }

            test "a declaration nobody can see this tick still pools its rock, and wins on it" {
                // The deadlock (#148): a source's position needs vision, vision needs a
                // creep there, a creep goes there because a Task exists, and the Task
                // exists because the source is in the projection. #124 read per-entry
                // absence onto the declaration as well, so the outpost's rock entered
                // the pool only on a tick the colony could see the room.
                //
                // The room is shaped as the shell shapes one it cannot see
                // (`World.factsOf`): terrain and a border ring, because
                // `Game.map.getRoomTerrain` needs no vision, and not one entry more.
                let declaration =
                    {
                        RoomName = "W1N2"
                        Sources = [ "src-out", { Room = "W1N2"; X = 10; Y = 46 } ]
                        // Off the corridor on purpose: the controller stands in `Obstacles`,
                        // and one on the corridor would seal it and make the comparison below
                        // about reachability instead of distance.
                        Controller = "ctrl-out", { Room = "W1N2"; X = 11; Y = 44 }
                    }

                let blind = northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None

                let declared =
                    { blind with
                        Sources = Outpost.pooledSources [ "W1N2" ] [ declaration ] blind.Sources
                        Spatial = Outpost.place [ declaration ] blind.Spatial
                    }

                Expect.equal
                    (matchOf blind)
                    (Some(taskId (Harvest "src-home"), MatchFactor.OnlyCandidate))
                    "the premise: undeclared, the blind room offers nothing and the home rock stands alone"

                // The win has to be on the *placed* rock's price: an unplaced target
                // prices at 0, which beats every real walk on the same factor, so a
                // `place` that did nothing would hand the Verdict below the same task
                // and factor for the opposite reason. These two lines tell the reasons
                // apart: the step down to the border, the crossing itself, and two
                // down the outpost's corridor to the Seat at (10,47), four plain tiles
                // at travel cost's 2 apiece.
                let atlas = Atlas.ofView declared

                Expect.equal
                    (Atlas.targetRoom atlas "src-out")
                    (Some "W1N2")
                    "the declaration reached the projection: the rock is filed under its own room"

                Expect.equal
                    (Atlas.travelCost atlas "w" (Harvest "src-out"))
                    (Some 8)
                    "and its price is a real crossing, never the escape: four plain steps at 2 apiece"

                // The same pair the ranking test above compares, at the same two
                // tiles: only that the outpost's rock is now declared rather than
                // seen has moved.
                Expect.equal
                    (matchOf declared)
                    (Some(taskId (Harvest "src-out"), MatchFactor.TravelCost))
                    "declared, the unseen rock is a Task ranked in the one pool — and the nearer of the two"
            }

            test "where vision answers, laying the declaration in changes nothing" {
                // A declaration carries only what cannot wait for vision, the ids and
                // the tiles, and is laid *under* what the room's `find` families
                // answered, never over it.
                //
                // Asserted as an equality on the whole projection rather than field by
                // field, so a field nobody thought of cannot be overwritten unnoticed.
                //
                // The declaration below names the rock one tile off where vision put
                // it, and that disagreement is the whole test: live the two agree by
                // construction, so a matching declaration would leave this equality
                // true whichever won. The conflict that can really arise is a
                // mistyped tile in a hand-moved constant.
                let declaration =
                    {
                        RoomName = "W1N2"
                        Sources = [ "src-out", { Room = "W1N2"; X = 10; Y = 47 } ]
                        Controller = "ctrl-out", { Room = "W1N2"; X = 11; Y = 44 }
                    }

                let colony = northBorderColony { X = 10; Y = 38 }

                let seen =
                    { colony with
                        Sources = colony.Sources @ [ drained "src-out" 120 ]
                        Creeps = colony.Creeps @ [ worker "w-out" 0 50 ]
                        Spatial =
                            { colony.Spatial with
                                Borders = Map.add "W1N2" plainRing colony.Spatial.Borders
                                TargetKinds =
                                    colony.Spatial.TargetKinds
                                    |> Map.add "src-out" Source
                                    |> Map.add "ctrl-out" Controller
                                    |> Map.add "cont-out" (Structure BuiltKind.Container)
                                Hits = Map.ofList [ "cont-out", { Hits = 100; HitsMax = 250000 } ]
                                Stores = Map.ofList [ "cont-out", 300 ]
                            }
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = TerrainGrid.ofList (corridor 10 40 48)
                                    TargetPositions =
                                        Map.ofList
                                            [
                                                "src-out", { X = 10; Y = 46 }
                                                "ctrl-out", { X = 11; Y = 44 }
                                                "cont-out", { X = 10; Y = 45 }
                                            ]
                                    CreepPositions = Map.ofList [ "w-out", { X = 10; Y = 44 } ]
                                    Obstacles = Set.singleton { X = 11; Y = 44 }
                                }
                    }

                Expect.equal
                    (Outpost.place [ declaration ] seen.Spatial)
                    seen.Spatial
                    "a projection vision already filled gains nothing from the declaration"

                Expect.equal
                    (Outpost.pooledSources [ "W1N2" ] [ declaration ] seen.Sources)
                    seen.Sources
                    "and the seen rock is pooled once, at the engine's restock and not the default"
            }

            test "an unseen rock is pooled at the held-energy default, not at never" {
                // A restock is a time, and 0 is what a source holding energy reads. The
                // unknown restock takes the same 0 rather than something large, because
                // a drained source's Harvest is judged at arrival and a walk has to
                // cover the wait; any other number would be a source no walk could
                // cover, the vision deadlock in a second place. The Emitter's own gate
                // withholds the dig from a rock that turns out empty.
                let declaration =
                    {
                        RoomName = "W1N2"
                        Sources = [ "src-out", { Room = "W1N2"; X = 10; Y = 46 } ]
                        Controller = "ctrl-out", { Room = "W1N2"; X = 11; Y = 44 }
                    }

                Expect.equal
                    (Outpost.pooledSources [ "W1N2" ] [ declaration ] [ source "src-home" ])
                    [ source "src-home"; source "src-out" ]
                    "the seen rocks first, in their order, then the declared one at restock 0"
            }

            test "a declaration for a room the scan set left out places nothing and pools nothing" {
                // The scan set is the one gate on which rooms the colony works
                // (`roomsProjected`), and the stand-down narrows exactly it. A
                // declaration able to furnish a room the scan left out would be a
                // second gate free to disagree with the first.
                let declaration =
                    {
                        RoomName = "W9N9"
                        Sources = [ "src-gone", { Room = "W9N9"; X = 10; Y = 46 } ]
                        Controller = "ctrl-gone", { Room = "W9N9"; X = 11; Y = 44 }
                    }

                let blind = northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None

                Expect.equal
                    (Outpost.place [ declaration ] blind.Spatial)
                    blind.Spatial
                    "no layer for that room, so no tile of it is placed"

                // The pool passes the same gate, and has to: `Atlas.travelCost` answers
                // 0 for geometry the projection cannot place, so a rock pooled for an
                // unprojected room *wins* its tier on price and the Emitter aims a
                // Harvest at an object `Game.getObjectById` cannot answer for while
                // anti-thrash holds the creep on it (#142's stuck creep). Reachable the
                // tick the colony's last spawn dies, when the scan set is empty.
                Expect.equal
                    (Outpost.pooledSources [ "W1N1"; "W1N2" ] [ declaration ] blind.Sources)
                    blind.Sources
                    "and no rock of it is pooled, so the two readings of the constant agree"

                Expect.isEmpty
                    (Outpost.pooledSources [] (Colony.outpostsOf Colony.declared "W12S28") [])
                    "an empty scan set — no spawn, so no home room — pools nothing at all"
            }

            test "a declared rock filed under another room is neither placed nor pooled" {
                // The declaration's tiles carry their own room, so "this rock is in
                // this outpost" is a thing a human can get *wrong* in the constant.
                // `Outpost.place` drops such a tile rather than writing it onto this
                // room's coordinate (the #191 phantom), and the pool has to drop it in
                // the same breath: an unplaced target prices at 0, so a pooled-but-
                // unplaced rock *wins* its tier, takes no Seat cap and holds every
                // applicable worker on a Harvest the engine cannot resolve.
                //
                // Pairwise on the one field that moved.
                let atRoom room =
                    {
                        RoomName = "W1N2"
                        Sources = [ "src-out", { Room = room; X = 10; Y = 46 } ]
                        Controller = "ctrl-out", { Room = "W1N2"; X = 11; Y = 44 }
                    }

                let blind = northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None

                let placedIn declaration =
                    Outpost.place [ declaration ] blind.Spatial
                    |> fun spatial -> SpatialInfo.placementOf spatial "src-out"

                Expect.equal
                    (placedIn (atRoom "W1N2"))
                    (Some { Room = "W1N2"; X = 10; Y = 46 })
                    "its own room: the declared rock is placed"

                Expect.equal
                    (Outpost.pooledSources [ "W1N2" ] [ atRoom "W1N2" ] blind.Sources)
                    (blind.Sources @ [ { Id = "src-out"; TicksToRestock = 0 } ])
                    "its own room: and pooled beside the seen rocks"

                Expect.equal (placedIn (atRoom "W1N1")) None "the mother's room: nothing places it"

                Expect.equal
                    (Outpost.pooledSources [ "W1N2" ] [ atRoom "W1N1" ] blind.Sources)
                    blind.Sources
                    "the mother's room: and nothing pools it either — the two gates are one"
            }

            test "a declared controller stands in Obstacles, so no Work Area offers its tile" {
                // The controller's own tile joins `Obstacles`, as the seen half files
                // it: a reserver stands beside it and never on it, so a Work Area
                // built over ground that ignored it would offer a tile the engine
                // refuses to move onto, and #131's reserver would be held there.
                //
                // On plain ground on purpose: both declared controllers stand on
                // terrain the capture reads as wall, so the weight grid refuses their
                // tiles before `Obstacles` is consulted and the rule would be pinned by
                // the terrain rather than the code.
                let declaration =
                    {
                        RoomName = "W1N2"
                        Sources = [ "src-out", { Room = "W1N2"; X = 10; Y = 46 } ]
                        Controller = "ctrl-out", { Room = "W1N2"; X = 10; Y = 42 }
                    }

                let blind = northBorderColony { X = 10; Y = 38 } |> withNorthOutpost None

                let atlas =
                    Atlas.ofView
                        { blind with
                            Spatial = Outpost.place [ declaration ] blind.Spatial
                        }

                let area = Atlas.workArea atlas (Upgrade "ctrl-out")

                Expect.isNonEmpty
                    area
                    "the premise: the corridor gives the controller ground to be reserved from"

                Expect.isFalse
                    (Set.contains (RoomPos.at "W1N2" { X = 10; Y = 42 }) area)
                    "and the controller's own tile is not part of it, standing in Obstacles"
            }

            test "an outpost controller's Upgrade area is nobody's working ground" {
                // #241. The mover asks for the working ground room by room, and asked
                // whole the query hands back the 7x7 around an outpost controller as a
                // workplace. The colony *reserves* an outpost's controller, so a Seat
                // inside its area is ground nobody upgrades from. The outpost's Seats
                // are another matter: its Anchor really does work from those.
                let outpost creeps =
                    let colony =
                        { bareRespawn with
                            Spawns = []
                            Sources = []
                            Controller = None
                            Refillables = []
                            Creeps = creeps |> List.map fst
                        }
                        |> withOutpost
                            "W1N2"
                            [
                                "ctrl-out", { X = 25; Y = 25 }, Controller
                                "src-out", { X = 22; Y = 28 }, Source
                            ]
                            ([
                                for x in 20..30 do
                                    for y in 20..30 -> { X = x; Y = y }, Plain
                             ]
                             @ [ { X = 22; Y = 28 }, Wall ])

                    { colony with
                        Spatial =
                            colony.Spatial
                            |> withNeighbour
                                "W1N2"
                                { SpatialInfo.layerOf colony.Spatial "W1N2" with
                                    CreepPositions =
                                        creeps
                                        |> List.map (fun (creep: CreepInfo, pos) -> creep.Name, pos)
                                        |> Map.ofList
                                }
                    }

                let beside = outpost [ worker "w" 0 50, { X = 24; Y = 25 } ]

                Expect.equal
                    (Atlas.workingGroundIn (Atlas.ofView beside) "W1N2")
                    (Set.ofList
                        [
                            for x in 21..23 do
                                for y in 27..29 do
                                    if (x, y) <> (22, 28) then
                                        { X = x; Y = y }
                        ])
                    "the room's working ground is the outpost source's Seats and nothing else"

                Expect.isEmpty
                    (moveIntents (resolveOn beside []))
                    "so a body idling beside the outpost controller has nowhere it must step off"

                Expect.isNonEmpty
                    (moveIntents (resolveOn (outpost [ worker "w" 0 50, { X = 21; Y = 27 } ]) []))
                    "while one idling on the outpost's own Seat steps off it as it would at home"
            }
        ]
