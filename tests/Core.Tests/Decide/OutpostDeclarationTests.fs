/// What a declared outpost is worth and what it asks for (ADR 0042).
module Fabot.Core.Tests.Decide.OutpostDeclarationTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

/// The colony's one creep taken out of the home room and stood in the outpost
/// across the north border. Where a body the concurrent-builder cap has already
/// parked out there actually is, and the only place from which the outpost's
/// own site is the near target rather than anything at home — which is the fact
/// every case that takes this fixture is pinning.
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
            // ADR 0041's central claim, at the seam it is claimed on: an
            // outpost's Task is not steered to the front of the pool or to
            // the back of it, it is ranked. Both Harvests sit on the
            // feeding tier, so what separates them is travel cost — and
            // travel cost crosses the Seam since #123, which is what makes
            // the outpost's Task comparable at all rather than a special
            // case somewhere ahead of the ranking.
            //
            // Pairwise, one rival at a time: this pool holds these two
            // Tasks and nothing else, so the factor a Matched Verdict
            // reports is about this pair and no third candidate stands in
            // for either of them.
            //
            // The ranking and deliberately not the tick that follows it:
            // this test reads the Verdict, which is the half ADR 0041
            // delivers. What the winner does with the tick is #142's, and
            // the case below it drives that.
            test "an outpost Harvest and a home Harvest are ranked in one pool" {
                Expect.equal
                    (matchOf (
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })
                    ))
                    (Some(taskId (Harvest "src-out"), MatchFactor.TravelCost))
                    "the outpost source is the nearer of the two, across the Seam"

                // The same fixture with the two sources swapped over: only
                // how far each one is moves, and the ranking moves with it.
                Expect.equal
                    (matchOf (
                        northBorderColony { X = 10; Y = 4 }
                        |> withNorthOutpost (Some { X = 10; Y = 41 })
                    ))
                    (Some(taskId (Harvest "src-home"), MatchFactor.TravelCost))
                    "the home source is the nearer of the two, and wins the same comparison"
            }

            // #235's live shape, read from the row it evicted. The mother's
            // workers matched the outpost rock's Harvest across the Seam —
            // Feeding tier (ADR 0023), and nearer than her own — and the Seats
            // they took count against that source's whole Total (ADR 0051), so
            // W12S27's own Anchor read `none-free` on the Post it was standing
            // on. One clause answers both halves: a rock a six-Work Anchor is
            // already draining pays a light body nothing for the crossing, so
            // the mother's worker is not applicable to it at all and the
            // ranking above falls to her own room.
            //
            // Pairwise on the garrison alone. The container stands in both
            // halves, so what moves between them is a body on the Post — which
            // is also the safety valve stated as a case: a Post nobody is
            // standing on is a rock with its whole ten a tick spare, and the
            // mother's worker is welcome to it.
            test "an outpost rock its Anchor drains is refused the mother's worker" {
                let colonyWith garrison =
                    let base' =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost (Some { X = 10; Y = 46 })

                    let outpost = SpatialInfo.layerOf base'.Spatial "W1N2"

                    { base' with
                        Creeps = base'.Creeps @ garrison
                        // Reserved by us, so the rock can be priced at all
                        // (ADR 0004) and prices at the held ten a tick — the
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
                // #142's reproduction, at the seam it was reproduced on.
                // Before it, this fixture answered `Matched ("w",
                // "harvest:src-out", TravelCost)` and then a lone
                // `SayCreep`: the Task had a price and no step, so the
                // creep stood still, said its glyph, and anti-thrash kept
                // it there for the rest of its life — having given up the
                // home source it would otherwise have dug.
                //
                // Now the mover aims at the near side of the crossing the
                // price was paid at. That tile is in the creep's own room,
                // so nothing here is arbitrated across the border: the
                // Resolver settles a step of this room exactly as it always
                // has. This band is plain the whole way round and the
                // corridor meets it at x = 10, so three crossings — x = 9,
                // 10 and 11 — cost this creep the same to the tick, and the
                // band's minimum takes the lowest (X, Y) of them as every
                // other tie in the Atlas is taken. The creep therefore
                // leaves the corridor diagonally, which the engine allows
                // onto an exit exactly as it allows anywhere else.
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

                // Driven the way the engine drives it: the creep stands
                // where the last tick's Intent put it, its Assignment handed
                // back, until the step it is given leaves this room's
                // ground — the tick it crosses.
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
                // Where the drive above hands the creep to the engine, and
                // what takes it from there. The engine lifts the creep off
                // (9,0) and files it in W1N2 on that room's border row, and
                // from that tick the Resolver arbitrates W1N2 as a room of
                // its own (#145): its occupants, its blocked tiles and its
                // Move Intents, over that room's tiles and no other's, so
                // the creep that landed gets a step exactly as one standing
                // at home does. Before #145 the far side was deferred, and
                // this case asserted the creep standing on its landing tile
                // holding its Task against anti-thrash, saying its glyph —
                // the trace #142 quotes, one tile past the border.
                //
                // The landing tile is not ground — the ring is no room's
                // floor (ADR 0036) — and the tile beside it is; the mover
                // answers from both, because a flood seeds its start tile
                // whatever that tile's weight, and steps off it onto the
                // room's own ground.
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
                                    Terrain = Map.ofList (corridor 10 40 48)
                                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 46 } ]
                                    CreepPositions = Map.ofList [ "w", pos ]
                                }
                    }

                let assigned = Map.ofList [ "w", taskId (Harvest "src-out") ]

                // Nothing else leaves the colony on these ticks, and one
                // absence is worth naming: this outpost carries no
                // `RoomControl` entry, so it is a room the colony is not
                // looking into, and ADR 0042's container rule plans into
                // no such room (`planOutpostContainers`). Give the fixture
                // vision and a placement Intent joins the lines below.
                //
                // The tile the crossing above delivers to, two more ring and
                // ground tiles at the top of the corridor, and one a step
                // from the Work Area: each is walked toward the source.
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

                // Driven the way the engine drives it, from the landing
                // tile: two steps up the corridor and the dig begins.
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
                // #150's reproduction, at the seam it was reproduced on.
                // The container rule placed a site in the outpost, saw it
                // standing on the next tick and correctly declined to place
                // a second — and nothing ever built the first, because the
                // Build pool is `ColonyView.ConstructionSites` mapped one to
                // one and that list was the spawn rooms' alone. A container
                // that is never built is a source that never becomes a
                // Post, so ADR 0042's switch could not close.
                //
                // Nothing in the Build path is outpost-shaped: the Task
                // names the site by id, its Work Area is the site's own
                // room's (ADR 0041, ADR 0020) and its price sums the legs
                // over the Seam (#123), exactly as the outpost Harvest
                // above does. What was missing was the entry, and this is
                // the entry.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded

                Expect.equal
                    (matchOf sited)
                    (Some(taskId (Build "site-out"), MatchFactor.OnlyCandidate))
                    "the site a room away is a Task this worker is given"

                // The pool is that list and never the kind census: a site
                // the projection places but the shell did not hand over —
                // which is every site in a room the colony cannot see this
                // tick (ADR 0004) — names no Task at all.
                Expect.equal
                    (matchOf { sited with ConstructionSites = [] })
                    None
                    "and a site the ColonyView does not carry is no Task, however well the projection places it"

                // The memo *does* flinch at it, and this is the tick that
                // changed (#169). #121 and #149 left the `pending` half
                // joined against the home layer alone because nothing the
                // memo carried read a site outside home — this rule's own
                // site least of all, since it is recomputed every tick (ADR
                // 0042) — and the throw-away it saved was one Layout and
                // one spawn walk table on the tick the site appeared. The
                // walk table's far leg is now a memo entry over the *goal*
                // room's weight grid, and an obstacle-kind site closes its
                // tile in whatever room it stands in (`projectVisible`), so
                // a pending census stopping at the home layer is ADR 0017's
                // signature gap out there. Signing the half whole rather
                // than only its blocking kinds keeps one rule instead of a
                // second asymmetry to hold in step with the App's obstacle
                // filter; the price is exactly the throw-away above, on the
                // handful of ticks in a colony's life that an outpost
                // container site appears.
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
                // #151's reproduction, at the seam it was reproduced on. The
                // reserver dies — a CLAIM body lives 600 ticks — and with it
                // goes the only vision W1N2 had: the site leaves
                // `ConstructionSites`, the projection stops placing it, and
                // `build:site-out` leaves the pool. Every gate below the
                // first in the Matcher's keep cascade is about the *Task*,
                // so none of them is even reached; the assignment fell
                // through the one gate that reads an empty lookup as a
                // target that is gone. The worker turned round with a full
                // load, and on the tick the vision came back the whole
                // crossing began again — a half-built container can stand
                // there for ever that way.
                //
                // What the grace changes is exactly that first gate, and it
                // reads a fact about looking rather than about the site: the
                // room the id was last seen in has not been seen since, and
                // it went dark inside `Tuning.VisionGrace`.
                let crossing =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded

                // A tick a long way from zero, so the dark ticks the grace
                // is read over are ticks and not a fixture's arithmetic.
                let sited = { crossing with Time = 1000 }

                let held = taskId (Build "site-out")
                let assignments = Map.ofList [ "w", held ]

                // The room as the shell hands it over with no vision in it:
                // the site list is empty, the kind census and the tile go
                // with it (ADR 0004 is right here and this ticket does not
                // touch it), and the one thing left is the world's own
                // record of when the room was last looked into and what
                // stood in it then.
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
                                        Targets = Set.singleton "site-out"
                                    }
                                ]
                    }

                let verdictsAt tick =
                    (decideFrom assignments (darkSince tick)).Verdicts

                // Pairwise on the grace and on nothing else: one room, one
                // creep, one held Task, and the only thing that moves
                // between the three readings is the tick the room was last
                // seen at.
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

                // What the grace buys, both halves of it. The assignment is
                // handed to the next tick, *and* the creep keeps walking the
                // crossing it was released off before: a body that stops
                // where it stands arrives no sooner than the one that turned
                // round, and it is the arrival itself that ends the darkness.
                //
                // The mover aims it with what the grace already knows — the
                // room the id was last seen in — and asks the border layer
                // and the memoised terrain for the rest
                // (`Atlas.stepTowardRoom`, ADR 0031, ADR 0041), which is the
                // route `cac0124` built for a crossing creep. No remembered
                // tile is laid into the layer, nothing is placed and nothing
                // is priced off the sighting, so ADR 0004's vision gate
                // stands exactly where #151 left it: the Emitter still has
                // no act to spell for a target nobody can see.
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
                // The other half of #151's rule, and the reason it is a
                // grace and not a latch: nothing about a kept assignment
                // survives the vision returning. The relief lands, the room
                // answers again, and the site is either standing — the
                // worker walks the rest of the crossing it never abandoned —
                // or gone, and the release it was owed arrives one tick
                // late instead of never.
                let crossing =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }
                    |> loaded

                // A tick a long way from zero, so the dark ticks the grace
                // is read over are ticks and not a fixture's arithmetic.
                let sited = { crossing with Time = 1000 }

                let held = taskId (Build "site-out")
                let assignments = Map.ofList [ "w", held ]

                // Forty dark ticks behind it, and vision in the room this
                // tick: the sighting is stamped at the tick it is read, so
                // the grace can no longer fire whatever it remembers.
                let backWithSite =
                    { sited with
                        Sightings =
                            Map.ofList
                                [
                                    "W1N2",
                                    {
                                        Tick = 1000
                                        Targets = Set.singleton "site-out"
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

                // The same tick with the site gone: the room is seen, so
                // the sighting is this tick's and the grace has nothing to
                // say — a target that vanished under our own eyes is gone.
                let backWithout =
                    { backWithSite with
                        ConstructionSites = []
                        Spatial =
                            { backWithSite.Spatial with
                                TargetKinds = Map.remove "site-out" backWithSite.Spatial.TargetKinds
                            }
                        Sightings = Map.ofList [ "W1N2", { Tick = 1000; Targets = Set.empty } ]
                    }

                Expect.contains
                    (decideFrom assignments backWithout).Verdicts
                    (Verdict.Released("w", held, ReleaseReason.TaskGone))
                    "and a site that finished or was cancelled while we watched releases on the tick it went"
            }

            test "and the worker that landed in the outpost builds it" {
                // The far half of the same walk, driven the way the engine
                // drives it: #145 arbitrates the outpost as a room of its
                // own, so the creep the engine put down on W1N2's border
                // row gets a step off the ring exactly as one standing at
                // home does, and the tick it stands inside the site's Work
                // Area — build reaches three tiles — the Intent it has
                // been walking toward is emitted.
                //
                // A slow answer and the right one (ADR 0042): whoever
                // holds this Task spends five hits a tick per Work part
                // into a 5,000-hit container. There is no outpost builder
                // row, and this ticket invents none — which creep holds it
                // is the ranking's answer, pinned in the test below.
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
                // #157, and the reverse of what this very fixture asserted
                // before it. The two cases above hold one worker and no
                // controller, which is what let their factor name the
                // Build's one rival; the colony that really exists has a
                // controller, and while the site was surplus work that
                // controller took every loaded worker every tick. Build and
                // Upgrade shared the surplus tier, so nothing but travel
                // cost separated them, and a loaded worker standing at home
                // is a corridor from its own controller and a Seam plus
                // fifty tiles from the site.
                //
                // Deployed, that was ADR 0042's switch laid down and never
                // closed: the reserver went out (#131), the site went up
                // (#128), and nobody ever built it. #150's answer here —
                // that the builder would be a creep which had walked out
                // for this room's own Harvest and filled up there — never
                // happened either, because the Storage's Withdraw is
                // feeding tier and a few tiles from home while the
                // cross-Seam Harvest is fifty, so no worker made the trip
                // to fill up out there in the first place.
                //
                // So this Build is feeding tier now (`tierOf`): it decides
                // whether the room is in the economy at all, which is the
                // same kind of question the Reserve beside it settles about
                // the rate. The factor is `Rank` and deliberately not
                // `TravelCost` — the site is still much the farther of the
                // two targets and wins anyway. Pairwise, one rival at a
                // time: one Build, one Upgrade, and the home Harvest
                // inapplicable to a body with nothing free to fill.
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
                // The other half of the same colony, unmoved by #157: a
                // creep standing in the outpost is nearer the site than
                // anything at home, so it held this Task on the surplus
                // tier and holds it on the feeding one. What changed is
                // that it is no longer the *only* creep that ever could.
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
                // ADR 0010's layering is untouched by #157, and this is
                // what keeps a starving spawn from waiting on a container
                // fifty tiles away with no special case written for it. The
                // spawn and the extensions were always on the feeding tier
                // and the outpost's site has joined them, so what separates
                // the two is travel cost — and a hungry extension underfoot
                // is nearer than a site across a Seam, every time.
                //
                // Pairwise, one rival at a time: no controller in this
                // fixture, so the pool is the Build, the Refill and a home
                // Harvest a full body cannot take.
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
                // The half of #157 that must not move. What makes the
                // outpost's site a switch is the room it stands in and not
                // the kind it is, and a `Pos` carries no room (ADR 0041) —
                // so a reading that went by the kind census alone would
                // lift every container the Layout ever places (ADR 0040)
                // onto the feeding tier and pull the whole worker row off
                // the controller with it.
                //
                // Discriminating by construction, and against the **flow**
                // since #234: this site stands at home, so the rung reaches
                // it and the controller under the creep's feet stopped being
                // an instrument. What tells the two readings apart is a
                // hungry extension placed **farther** than the site. Read as
                // a switch the site ties that Refill on the feeding tier and
                // wins on price — which is exactly the failure this pins;
                // read as the surplus it is, the Refill outranks it outright
                // however near it stands. The two readings differ in the
                // winner and not merely in the factor. Pairwise: one Build,
                // one Refill, and an Upgrade the rung leaves cheapest to
                // neither.
                let homeSite =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> loaded
                        |> withHomeController { X = 10; Y = 5 }
                        |> withHungryExtension { X = 10; Y = 40 }

                    { colony with
                        ConstructionSites = [ { Id = "site-home" } ]
                    }
                    |> withTarget "site-home" { X = 10; Y = 30 } (Site BuiltKind.Container)

                Expect.equal
                    (matchOf homeSite)
                    (Some(taskId (Refill("ext-1", Energy)), MatchFactor.Rank))
                    "the colony's own container site is surplus work, and the flow outranks it"
            }

            test "an ordinary outpost site keeps its travel cost: #234's rung stops at home" {
                // The other half of #234's rung, and the reason it reads the
                // site's room (`isHomeSite`). At home the rung is the whole
                // point — a site outranks the controller a loaded body is
                // already standing beside. Out here it would be the failure
                // #157's builders' budget was invented against, "or the tier
                // would walk the whole worker row over the Seam at once", and
                // it is `Tuning.OutpostBuilders` and never the rung that
                // answers whether a body crosses (#266): the budget lifts the
                // sites nearest the Seam onto the feeding tier, and everything
                // behind them stays exactly where #234 left it — surplus, on
                // the tier's lower rung, priced against an Upgrade the body is
                // already standing in the Work Area of.
                //
                // So the queue is one site longer than the budget: the two
                // nearest are lifted and take one builder apiece, and the third
                // is the one this case is about. Roads on purpose, so the
                // container rule cannot be what answers, and the whole row is
                // loaded, standing inside the home controller's Work Area and a
                // Seam from every one of them.
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
                // #266, at the seam W13S29 was reported on: two containers
                // standing, 45 hand-laid road sites at 0/300, and the whole
                // worker row at home. A road in an outpost was a plain surplus
                // Build (#234's rung stopping at the home room), so travel cost
                // answered 120 against an Upgrade underfoot costing nothing and
                // nobody ever crossed — and the trunk is what the [[hauler
                // unit]]'s round trip is priced on, so the room went on being
                // hauled as if it were unpaved.
                //
                // What lifts them is the budget itself: the first
                // `Tuning.OutpostBuilders` sites in the queue are feeding-tier
                // and the rest are not, so the crowd that may cross and the
                // number of sites worth crossing for are one number. The queue
                // is the container first — ADR 0042's switch on whether the
                // room is in the economy at all, and here deliberately the
                // **farthest** site of the six, so nothing but the kind can be
                // putting it in front — and then the walk out to the Seam
                // (`Atlas.seamWalkTicks`), nearest first, because the paved
                // tiles beside the crossing are the ones every haul walks over.
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

                // And the queue moves: the container is built, `site-r1` with
                // it, and the two behind them are the next two out. Nothing
                // schedules that — the pool is recomputed from the sites that
                // are left, and the head of it is the answer (ADR 0013).
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
                // The cap `planPool` puts on this Build (#157). On
                // the feeding tier the site outbids the home Upgrade for
                // every loaded worker at once, and travel cost cannot thin
                // that crowd — a Seam away is a Seam away from every tile
                // of one corridor. Uncapped, the whole worker row walks out
                // together and the home room stops working for the fifty
                // ticks each of them spends crossing.
                //
                // Two is a tunable and the third worker is what reads it:
                // rejected as capacity-full, it falls to the Upgrade it
                // would have taken anyway. Asserted as the whole tally, so
                // a cap that admitted all three or only one both fail.
                // Two is the whole colony's budget and not this site's
                // alone — one site standing is what makes the two numbers
                // agree here; the test below opens a second site and reads
                // them apart.
                let crowd = crowdAtOutpostSite (northBorderColony { X = 10; Y = 38 })

                let { Assignments = assignments } = decideOn crowd

                Expect.equal
                    (assignments |> Map.toList |> List.map snd |> List.countBy id |> List.sort)
                    [ taskId (Build "site-out"), 2; taskId (Upgrade "ctrl-1"), 1 ]
                    "two of the three hold the site, and the one left over upgrades"
            }

            test "the whole ring closes at `decide`: cross, build it empty, dig it full, build on" {
                // ADR 0042's switch closing under its own power, end to
                // end and with no new concept in it (#157) — the loop the
                // ticket asks for, driven one tick at a time over the one
                // seam this repo decides at.
                //
                // The colony that really exists: a controller at home, a
                // rock and a container site in the outpost, and one loaded
                // worker standing at home. It crosses because the site now
                // outranks the controller (`tierOf`); it builds until the
                // build empties it; emptied, the Build goes inapplicable
                // and the outpost's own rock — a step away, feeding tier —
                // is the cheapest Task it has (`applicable`, ADR 0013);
                // full again, the site outranks everything once more. No
                // "go home" act and no outpost builder row: the ring is
                // the ordinary ranking, turning.
                //
                // Driven the way the engine drives it: this tick's
                // Assignments handed back as the next tick's, a Move
                // Intent stepped, and a step onto the exit row handed over
                // to the neighbour's own border row, which is exactly what
                // the engine does with a creep that ends its tick there
                // (ADR 0036, #145). What a build spends and a dig collects
                // is the engine's arithmetic and not this seam's, so the
                // two act on the store at their limits — emptied, filled —
                // which is the state the ring turns on.
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

                            // The engine's own handover: a creep ending its
                            // tick on the exit row is lifted into the
                            // neighbour and filed on that room's opposite
                            // border row, same column.
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
                // What the feeding tier took away and `applicable` gives
                // back (#157). Travel cost was the only thing keeping a
                // heavy body off a distant site — `applicable`'s own doc
                // says so, "Travel cost pins an Anchor that is at its
                // Post" — and a rank the whole colony shares is exactly
                // what travel cost cannot answer. A full Anchor whose Post
                // carries no standing container yet loses Harvest
                // (`garrisons`), and was then outranked off its own
                // controller and walked fifty tiles at four to seven ticks
                // a step to spend one Carry into a 5,000-progress site,
                // burning a builder place while it went. A heavy body's
                // cross-room work is a Post (ADR 0020), so the gate is
                // ADR 0016's shape: this one Build is inapplicable to it.
                //
                // The two bodies stand on the same tile in the same
                // colony, so what tells them apart is the body and
                // nothing geometric.
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
                // The half of #157's Implementation decisions that is not
                // true as the ticket wrote it, pinned as it really is. The
                // ticket said "Refill still comes first — the home
                // extension / tower is nearer, cost decides"; ADR 0010 is
                // the authority and it puts a **tower** Refill in the
                // surplus tier, not the feeding one, so cost never gets
                // asked. Two pairwise cases, one rival each.
                let sited =
                    northBorderColony { X = 10; Y = 38 }
                    |> withNorthOutpost None
                    |> withOutpostSite { X = 10; Y = 43 }

                // A tower with 500 free, three tiles from the worker,
                // against a site a Seam and fifty tiles away. It loses on
                // rank and distance is never reached — which is ADR 0010's
                // own "a colony feeds its own reproduction before its
                // guns" with this Build counted as reproduction, and a
                // real change of behaviour under a raid at home. The
                // answer for a raid is the stand-down (ADR 0043, #136).
                let tower =
                    { (sited |> loaded) with
                        Refillables = [ refillable "tower-1" 500 BuiltKind.Tower ]
                    }
                    |> withTarget "tower-1" { X = 10; Y = 3 } (Structure BuiltKind.Tower)

                Expect.equal
                    (matchOf tower)
                    (Some(taskId (Build "site-out"), MatchFactor.Rank))
                    "a hungry tower is surplus work and is outranked outright, not beaten on distance"

                // The spawn does share the tier, so cost decides — and
                // cost is answered from where the creep stands. For the
                // one the cap has already parked in the outpost the
                // nearer target is the site, not the spawn: the home room
                // feeds itself through the creeps standing in it and not
                // by any rule.
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
                // `planPool`'s cap read at colony scale (#157).
                // `planOutpostContainers` places one site per unserved
                // outpost source and places them all on the same tick, so
                // a per-site two over the declaration's three sources is a
                // colony-wide six — the whole worker row, which is the one
                // thing the cap exists to prevent. Spread instead: two
                // sites take one apiece, and the case above, with one site
                // standing, still takes two.
                let crowd =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> withHomeController { X = 10; Y = 5 }

                    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

                    { colony with
                        ConstructionSites = colony.ConstructionSites @ [ { Id = "site-out2" } ]
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
                // #142's acceptance criterion 5, at the seam it is decided
                // on. Each room's arbitration reads that room's creeps and
                // no other's (#145): a `Map<Pos, string>` of occupants has
                // no room on its key, so a creep standing on the same
                // coordinate of the neighbouring room is not an occupant
                // here, is not displaced by this room's travellers, and
                // attributes nothing. Pairwise, one rival at a time — the
                // home traveller and one creep on its next tile, first in
                // the neighbour, then at home — because a pool holding
                // both proves nothing about which of them the traveller
                // was settled against.
                //
                // The bystanders are full, so no Harvest applies to them
                // and they park where they stand, displaceable to any
                // adjacent tile of their own room.
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
                            // The outpost's corridor runs the whole column
                            // here, so the coordinate the rival stands on is
                            // ground in both rooms and the case is about the
                            // room, never about a tile nobody could stand on.
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = Map.ofList (corridor 10 1 48)
                                    TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 46 } ]
                                    CreepPositions = Map.ofList outpostCreeps
                                }
                    }

                // The home source is the rival this time, at (10,38) with
                // the worker at (10,4) walking down to it, a step at a time
                // — its next tile is (10,5).
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
                // ADR 0008 in the far room: a fatigued creep sits its own
                // room's arbitration out, so it is reported Grounded — the
                // Verdict it was denied while only home was arbitrated —
                // and its tile is blocked in its room only. The home
                // traveller whose next tile shares that coordinate steps
                // regardless; before #145 this half held by the creep not
                // being arbitrated at all, now it holds by the room.
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
                                    Terrain = Map.ofList (corridor 10 1 48)
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
                // The ring is no place to stay (ADR 0036, ADR 0041): a
                // creep that ends its tick on the border row is moved out
                // of the room by the engine, so a landed creep the
                // Resolver leaves standing where it is would be bounced
                // back across the border and re-cross the next tick, for
                // as long as it kept losing its step. Two cases, one
                // branch of the mover each. Parked: a full creep with
                // nothing applicable, standing on the landing tile, is
                // walked onto the outpost's ground rather than settled on
                // the ring. Travelling: two landed creeps whose cheapest
                // step is the same ground tile — the one that yields is
                // handed the other ground tile beside it, and steps off
                // the ring instead of staying on it.
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
                            // One landing tile beside the corridor's top,
                            // so the ring has two ground tiles to step
                            // onto and a contested step has somewhere
                            // else to go.
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain =
                                        Map.ofList (
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
                // The ticket's rule — each room's arbitration uses that
                // room's tiles and no other's — at the displacement seam:
                // the tile beside a parked creep is read off the room the
                // creep is filed under. Home has ground at (9,5) and the
                // outpost has none there, so a parked outpost creep pushed
                // off (10,5) by an outpost traveller must be swapped up
                // the corridor, never sent Left onto a coordinate that is
                // only walkable at home.
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
                                        Map.ofList (corridor 10 1 40 @ [ { X = 9; Y = 5 }, Plain ])
                                    CreepPositions = Map.empty
                                })
                            |> withNeighbour
                                "W1N2"
                                { RoomLayer.empty with
                                    Terrain = Map.ofList (corridor 10 1 48)
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
                // ADR 0004's totality over a room layer carrying terrain
                // and a border ring and nothing else: every query of it
                // answers empty, so it is unpriceable, enters no Task and
                // blocks no action — "empty" is not a state anything has to
                // model, and the proof is that the tick decides exactly
                // what it decided with no such room at all.
                //
                // Not a blind outpost, which this test claimed to be until
                // #148: a declared room the colony cannot see carries its
                // sources and its controller all the same (`Outpost.place`,
                // ADR 0041), and the tests further down pin what *that*
                // decides. What is left here is the totality property the
                // furniture is laid on top of.
                //
                // Read at the top seam over all three outputs at once,
                // because the ways this could go wrong are not local: a
                // second room's weight grid consulted for a home price, a
                // Seam band admitting a crossing to nowhere, a Task pooled
                // off a layer with nothing in it.
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
                // #124 landed this constant empty and pinned the emptiness,
                // because ADR 0041 ships the capability to project a
                // neighbour and deliberately no behaviour. ADR 0042 fills
                // it, and this is where that pin turns over: the rooms, in
                // the order a human wrote them, and the scan set the shell
                // takes from them.
                //
                // Read through the colony that declares them (ADR 0047):
                // the outposts are one colony's now, so the home room is
                // the key the shell looks them up under (`ColonyView.ofWorld`
                // takes it off the first spawn) and the rooms and the scan
                // set are what that lookup answers with. A home nobody
                // declared answers with none, which is the other half of
                // the same rule and the behaviour the empty declaration
                // shipped with.
                //
                // Which ids and which tiles is a claim about the committed
                // captures rather than about this list, so it is pinned
                // where the captures are read (`RoomOutpostTests`, whose
                // first case reads every outpost of this same constant) and
                // never retyped here — two literals of the same ids would
                // agree with each other and with nothing else.
                let outposts = Colony.outpostsOf Colony.declared "W12S28"

                Expect.equal
                    (Colony.homes Colony.declared)
                    [ "W12S28"; "W13S28"; "W15S28" ]
                    "three colonies are declared: the room this bot has always run, the one it raised, and today's candidate (ADR 0047)"

                Expect.equal
                    (outposts |> List.map (fun outpost -> outpost.RoomName))
                    [ "W12S27" ]
                    "the north outpost alone: the west one is a colony of its own now (ADR 0047)"

                Expect.equal
                    (Outpost.adr0042 |> List.map (fun outpost -> outpost.RoomName))
                    [ "W12S27"; "W13S28" ]
                    "while ADR 0042's measured pair is kept whole for the real-terrain fixtures"

                Expect.equal
                    (Colony.outpostsOf Colony.declared "W13S28"
                     |> List.map (fun outpost -> outpost.RoomName))
                    [ "W13S29" ]
                    "and the second colony works its south outpost alone while it raises a child (W14S28 withdrawn by hand)"

                // W15S28 is declared and is **not** an outpost of anybody's:
                // it is owned, so it is a room its mother raises and not one
                // she mines, and `childrenWhere` gives a room in both lists to
                // the outpost list — which is the classification that stalled
                // it live on 2026-09-10 (claimed, spawn site placed by hand,
                // no body sent). What the mother projects for it is
                // `Colony.roomsProjected`'s bootstrap half, and the transit
                // room on the way is in there for the same reason an
                // outpost's is (ADR 0058).
                Expect.isFalse
                    (Colony.declared
                     |> List.exists (fun colony ->
                         colony.Outposts
                         |> List.exists (fun outpost -> outpost.RoomName = "W15S28")))
                    "the third colony's room is nobody's outpost now that it is ours"

                // And with the west outpost withdrawn, this is the
                // assertion that has something to prove: the room between
                // home and the nursery is in the projection on the
                // *bootstrap* half's own account (ADR 0058), where until
                // 2026-09-10 it was there only because a declaration
                // happened to name it.
                Expect.equal
                    (Colony.roomsProjected
                        (Colony.outpostsOf Colony.declared "W13S28")
                        (Colony.errandsOf Colony.declared "W13S28")
                        [ "W15S28" ]
                        "W13S28")
                    [ "W13S28"; "W13S29"; "W15S28"; "W14S28" ]
                    "the home, its outpost, the nursery two hops out, and the transit room on the way to it"

                Expect.isEmpty
                    (Colony.outpostsOf Colony.declared "W1N1")
                    "and a room nobody declared a colony for works no outposts at all"

                Expect.equal
                    (Outpost.roomsProjected outposts "W12S28")
                    [ "W12S28"; "W12S27" ]
                    "so the mother's projection covers the home room and its one outpost"
            }

            test "a declared outpost joins the spawn room in the set the shell scans" {
                // Written in the engine's own ids, as a declaration has to
                // be: the projection keys every target by the id the server
                // hands back, so a constant written in the captures'
                // readable short names would match nothing at all on a live
                // server, and would do it in silence (ADR 0004).
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

                // A declaration naming the spawn room is a human's slip in
                // a constant a human moves (ADR 0039's precedent), and the
                // projection keys rooms by name: scanning that room twice
                // would file one room's geometry under one name twice over
                // rather than say anything about it.
                Expect.equal
                    (Outpost.roomsProjected [ { north with RoomName = "W12S28" } ] "W12S28")
                    [ "W12S28" ]
                    "a room declared twice is scanned once"
            }

            test "a declaration nobody can see this tick still pools its rock, and wins on it" {
                // ADR 0041's deadlock, read at the top seam (#148): *"A
                // source's position needs vision; vision needs a creep
                // there; a creep goes there because a Task exists; the Task
                // exists because the source is in the projection."* #124
                // read ADR 0004's per-entry absence onto the declaration
                // as well, so the outpost's rock entered the pool only on a
                // tick the colony could see the room — and nothing was ever
                // sent to make that tick happen.
                //
                // The room here is shaped exactly as the shell shapes one
                // it cannot see (`World.factsOf`): terrain and a
                // border ring, because `Game.map.getRoomTerrain` needs no
                // vision, and not one entry more. Everything the outpost
                // contributes below is the declaration's.
                let declaration =
                    {
                        RoomName = "W1N2"
                        Sources = [ "src-out", { Room = "W1N2"; X = 10; Y = 46 } ]
                        // Off the corridor on purpose: what the controller
                        // is doing to this fixture is standing in
                        // `Obstacles`, and a controller on the corridor
                        // would seal it and make the comparison below about
                        // reachability instead of about distance.
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

                // The win has to be on the *placed* rock's price, and that
                // needs saying because an unplaced target is not inactive:
                // ADR 0004's escape prices it at 0, which beats every real
                // walk on the same factor. So a `place` that did nothing at
                // all would hand the Verdict below the same task and the
                // same `TravelCost` for the opposite reason. These two
                // lines are what tell the reasons apart: the rock is filed
                // under its own room, and the price that won is a real
                // crossing rather than the escape — the step down to the
                // border, the crossing itself, and two down the outpost's
                // corridor to the Seat at (10,47), four plain tiles at
                // travel cost's 2 apiece (ADR 0010's half-ticks).
                let atlas = Atlas.ofView declared

                Expect.equal
                    (Atlas.targetRoom atlas "src-out")
                    (Some "W1N2")
                    "the declaration reached the projection: the rock is filed under its own room"

                Expect.equal
                    (Atlas.travelCost atlas "w" (Harvest "src-out"))
                    (Some 8)
                    "and its price is a real crossing, never the escape: four plain steps at 2 apiece"

                // The same pair the ranking test above compares, at the
                // same two tiles — so what moved is only that the outpost's
                // rock is now declared rather than seen, and it is still
                // travel cost that separates the two.
                Expect.equal
                    (matchOf declared)
                    (Some(taskId (Harvest "src-out"), MatchFactor.TravelCost))
                    "declared, the unseen rock is a Task ranked in the one pool — and the nearer of the two"
            }

            test "where vision answers, laying the declaration in changes nothing" {
                // The other half of the rule: a declaration carries only
                // what cannot wait for vision — the ids and the tiles — and
                // is laid *under* what the room's `find` families answered,
                // never over it (ADR 0041). The reservation remaining, the
                // hits, the stores, the creeps and every structure standing
                // are vision's alone and stay vision's.
                //
                // Asserted as an equality on the whole projection rather
                // than field by field: what has to hold is that not one
                // entry moves, and a per-field check would pass while some
                // field nobody thought of was overwritten.
                //
                // The declaration below names the rock one tile off where
                // vision put it, and that disagreement is the whole test.
                // Live the two agree by construction — the ids are the
                // engine's own and a rock does not move — so a declaration
                // that matched vision tile for tile would leave this
                // equality true whichever of the two won, and the rule
                // would be pinned by nothing. Only a conflict can say which
                // truth is authoritative. The one that can really arise is
                // a human's: the constant is moved by hand (ADR 0041), and
                // a mistyped tile must not move a rock the engine is
                // answering for out from under its Seats.
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
                                    Terrain = Map.ofList (corridor 10 40 48)
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
                // ADR 0025: a restock is a time, and 0 is what a source
                // holding energy reads. The unknown restock takes the same
                // 0 rather than something large, because a drained source's
                // Harvest is judged at the creep's arrival — a walk has to
                // cover the wait — so any other number would be a source no
                // walk could ever cover, which is the vision deadlock again
                // in a second place. What withholds the dig from a rock
                // that turns out to be empty when the creep gets there is
                // the Emitter's own gate, on the tick there is vision to
                // read it from.
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
                // The scan set is the one gate on which rooms the colony
                // works (`roomsProjected`), and the stand-down of ADR 0043
                // narrows exactly it: a room withdrawn from does not enter
                // the projection at all. A declaration able to furnish a
                // room the scan left out would be a second gate free to
                // disagree with the first — furniture standing on terrain
                // nobody read.
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

                // The pool passes the same gate, and has to: an unplaced
                // target is not inert. `Atlas.travelCost` answers 0 for
                // geometry the projection cannot place (ADR 0004's escape),
                // so a rock pooled for a room nothing was projected for
                // *wins* its tier on price, and the Emitter aims a Harvest
                // at an object `Game.getObjectById` cannot answer for while
                // anti-thrash holds the creep on it (#142's stuck creep, in
                // a second place). Reachable the tick the colony's last
                // spawn dies — the shell's scan set is empty with no home
                // room — and the shape ADR 0043's stand-down withdraws a
                // room in.
                Expect.equal
                    (Outpost.pooledSources [ "W1N1"; "W1N2" ] [ declaration ] blind.Sources)
                    blind.Sources
                    "and no rock of it is pooled, so the two readings of the constant agree"

                Expect.isEmpty
                    (Outpost.pooledSources [] (Colony.outpostsOf Colony.declared "W12S28") [])
                    "an empty scan set — no spawn, so no home room — pools nothing at all"
            }

            test "a declared rock filed under another room is neither placed nor pooled" {
                // The declaration's tiles carry their own room since ADR
                // 0052 decision 2, so "this rock is in this outpost" is a
                // thing a human can now get *wrong* in the constant — it
                // used to be true by construction, a bare `Pos` beside
                // `RoomName` with nothing to disagree with. `Outpost.place`
                // drops such a tile rather than writing it onto this room's
                // coordinate (the #191 phantom), and the pool has to drop
                // it in the same breath: an unplaced target prices at 0
                // (ADR 0004's escape), so a pooled-but-unplaced rock *wins*
                // its tier, takes no Seat cap because the Atlas can seat no
                // tile for it, and holds every applicable worker on a
                // Harvest the engine cannot resolve.
                //
                // Pairwise on the one field that moved: the same id at the
                // same coordinate, once filed under the outpost's own room
                // and once under the mother's.
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
                // The third thing a declaration puts in the projection
                // beside the tiles and the kinds: the controller's own tile
                // joins `Obstacles`, exactly as the seen half files it. A
                // controller is an obstacle structure — a reserver stands
                // beside it and never on it — so a Work Area built over
                // ground that ignored it would offer a tile the engine
                // refuses to move onto, and #131's reserver would be
                // assigned there and held there.
                //
                // On plain ground on purpose, and that is the whole reason
                // this fixture exists rather than an assertion over the
                // committed captures: both declared controllers stand on
                // terrain the capture reads as wall, so the weight grid refuses
                // their tiles before `Obstacles` is ever consulted and the
                // rule would be pinned by the terrain rather than by the
                // code (ADR 0036 supplies counterexamples, not cover).
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
                // #241 read against ADR 0042. The mover asks for the
                // [[working ground]] room by room now, and an outpost's
                // controller is filed under its room like any other: asked
                // whole, the query hands back the 7x7 around it as a
                // workplace. The colony upgrades one controller, its own,
                // and *reserves* an outpost's — a Seat inside an outpost
                // controller's area is ground nobody upgrades from — so
                // there is nothing there for an idle body to step out of.
                // The outpost's Seats are another matter: its Anchor really
                // does work from those, and they stay in.
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
