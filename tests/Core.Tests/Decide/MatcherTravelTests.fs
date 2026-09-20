/// Travel-cost matching, movement, and the yield arbitration that settles a
/// contested tile.
module Fabot.Core.Tests.Decide.MatcherTravelTests

open Expecto
open FSharp.Reflection
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.MatcherFixtures

[<Tests>]
let travelCostTests =
    testList
        "travel-cost matching"
        [
            test
                "live-bug regression: a fresh creep takes the near source regardless of ColonyView order" {
                // The creep stands three steps from the near source, seven
                // from the far one.
                let snapshotWith (sources: SourceInfo list) =
                    { bareRespawn with
                        Sources = sources
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let far: SourceInfo = source "src-far"
                let near: SourceInfo = source "src-near"

                for sources in [ [ far; near ]; [ near; far ] ] do
                    let { Assignments = assignments } = decideOn (snapshotWith sources)

                    Expect.equal
                        (Map.tryFind "w1" assignments)
                        (Some(taskId (Harvest "src-near")))
                        "the cheaper-to-reach source wins the rank tie"
            }

            test "swamp prices the route: a range-nearer target loses to a longer plain path" {
                // One corridor, a source at each end. src-swamp is 3 tiles
                // away by range but behind two swamp tiles (cost 20);
                // src-plain is 5 tiles away over plain ground (cost 8).
                let corridor =
                    [
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Swamp
                        { X = 10; Y = 14 }, Swamp
                        { X = 10; Y = 15 }, Plain
                        { X = 10; Y = 16 }, Plain
                        { X = 10; Y = 17 }, Plain
                        { X = 10; Y = 18 }, Plain
                        { X = 10; Y = 19 }, Plain
                        { X = 10; Y = 20 }, Wall
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-swamp"; source "src-plain" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial
                                [ "src-swamp", { X = 10; Y = 12 }; "src-plain", { X = 10; Y = 20 } ]
                                corridor
                            |> withCreepsAt [ "w1", { X = 10; Y = 15 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-plain")))
                    "true path cost decides, not Chebyshev range"
            }

            test "rank dominates: an adjacent Build never outbids a four-tiles-away Refill" {
                // The hungry spawn sits at the top of the corridor, four
                // steps from the creep; the construction site is close
                // enough to build without moving at all.
                let corridor = [ for y in 10..16 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        ConstructionSites = [ { Id = "site-1"; Left = siteOwes } ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            spatial
                                [ "spawn-1", { X = 10; Y = 10 }; "site-1", { X = 10; Y = 16 } ]
                                corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 15 } ]
                                    Obstacles = Set.singleton { X = 10; Y = 10 }
                                })
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "travel cost breaks ties within a rank, never across ranks"
            }

            test "a sticky assignment is kept even when a cheaper task exists this tick" {
                // Same corridor as the live-bug regression, but the creep
                // already holds the far source from an earlier tick.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let sticky = Map.ofList [ "w1", (taskId (Harvest "src-far")) ]
                let { Assignments = assignments } = decideFrom sticky snapshot

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-far")))
                    "sticky assignments are never re-evaluated for a closer target"
            }

            test "an unplaced creep is matched as today: ColonyView order decides the tie" {
                // The projection places both sources but not the creep, so
                // no flood can run — the pick falls back to (rank, load).
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor []
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-far")))
                    "without a creep position, behaviour is unchanged"
            }

            test "an unreachable Work Area makes the Task inapplicable: the creep sinks lower" {
                // The source's one Seat is walled off from the creep; the
                // controller is reachable. The half-full creep could do
                // either, but Harvest is off the table entirely — no
                // range-based fallback march at a wall.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                        { X = 10; Y = 16 }, Wall
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 16 } ]
                                terrain
                            |> withCreepsAt [ "w1", { X = 10; Y = 14 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the unreachable Harvest is not applicable to this creep at all"
            }

            test "body-aware cost: the slow heavy body is matched near, the generalist far" {
                // The near source hides behind two swamp tiles (terrain 20);
                // the far one lies nine plain steps away (terrain 18). By
                // bare terrain weight both creeps would march far. Priced
                // by body, the heavy one (5 fatigue parts on 3 Moves) wades
                // the swamps for 17 + 17 = 34 rather than walk nine plains
                // at ceil(10/3) = 4 apiece for 36 — while the generalist's
                // cost equals terrain, so it still takes the far source.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall // src-near
                        { X = 10; Y = 11 }, Swamp // its only Seat
                        { X = 10; Y = 12 }, Swamp
                        { X = 10; Y = 13 }, Plain // the heavy body stands here
                        { X = 11; Y = 13 }, Plain // the generalist beside it
                        yield! [ for y in 14..22 -> { X = 10; Y = y }, Plain ]
                        { X = 10; Y = 23 }, Wall // src-far
                    ]

                let heavy =
                    creepWith "mule" 0 50 [ Work; Work; Work; Work; Work; Carry; Move; Move; Move ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-near"; source "src-far" ]
                        Creeps = [ heavy; worker "runner" 0 50 ]
                        Spatial =

                            spatial
                                [ "src-near", { X = 10; Y = 10 }; "src-far", { X = 10; Y = 23 } ]
                                terrain
                            |> withCreepsAt
                                [ "mule", { X = 10; Y = 13 }; "runner", { X = 11; Y = 13 } ]
                    }

                let { Assignments = assignments } = decideOn snapshot

                Expect.equal
                    (Map.tryFind "mule" assignments)
                    (Some(taskId (Harvest "src-near")))
                    "the slow body's real travel time keeps it near"

                Expect.equal
                    (Map.tryFind "runner" assignments)
                    (Some(taskId (Harvest "src-far")))
                    "the generalist stays the cheaper traveller to the far source"
            }
        ]

[<Tests>]
let movementTests =
    testList
        "movement"
        [
            test "a creep outside its Work Area steps toward the source, acting not yet" {
                // A one-tile-wide plain corridor: x = 10, y = 9..15, with the
                // source tile itself a wall (sources always sit on walls).
                let snapshot = corridorColony [ worker "w1" 0 50 ] [ "w1", { X = 10; Y = 14 } ]

                let { Intents = intents } = decideOn snapshot

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Top ]
                    "one single-step move Intent up the corridor"

                Expect.isEmpty (actionIntents intents) "out of range: no action Intent yet"
            }

            test "a creep inside its Work Area acts and does not move" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] (openSeats { X = 10; Y = 10 })
                            |> withCreepsAt [ "w1", { X = 10; Y = 11 } ]
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.contains intents (HarvestSource("w1", "src-a")) "seated creep harvests"

                Expect.isEmpty (moveIntents intents) "nowhere to go: no move Intent"
            }

            test "the approach detours around swamp when a plain lane is cheaper" {
                // Straight lane x = 10 is swamp (cost 10 each); the lane at
                // x = 11 is plain and reaches a Seat in as many steps.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Swamp
                        { X = 10; Y = 13 }, Swamp
                        { X = 10; Y = 14 }, Plain
                        { X = 11; Y = 11 }, Plain
                        { X = 11; Y = 12 }, Plain
                        { X = 11; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withCreepsAt [ "w1", { X = 10; Y = 14 } ]
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.equal
                    (moveIntents intents)
                    [ "w1", TopRight ]
                    "the first step leaves the swamp lane for the plain one"
            }

            test
                "a loaded worker's first step lands on the road: the paved detour beats the terrain line" {
                // The terrain line runs straight up the plain lane x = 10,
                // three steps to the Seat at (10,11). A paved arc swings
                // out through x = 11..12 — four steps, one more than the
                // line — to the road Seat at (11,11); the unprojected gap
                // at (11,12)/(11,13) keeps the arc from being cut short.
                // The half-loaded worker prices a road step at 2 and a
                // plain step at 4, so the longer paved detour (8) beats the
                // straight terrain line (12): the road sets the first step.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                        { X = 11; Y = 14 }, Plain
                        { X = 12; Y = 13 }, Plain
                        { X = 12; Y = 12 }, Plain
                        { X = 11; Y = 11 }, Plain
                    ]

                let paved =
                    Set.ofList
                        [
                            { X = 11; Y = 14 }
                            { X = 12; Y = 13 }
                            { X = 12; Y = 12 }
                            { X = 11; Y = 11 }
                        ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 25 25 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                    Roads = paved
                                })
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Right ]
                    "the first step leaves the terrain line for the paved detour"
            }

            test "a creep in range on a tile it may not keep acts and moves in one tick" {
                // An obstacle structure now sits under the creep (built beneath
                // it), so its tile is no longer Work Area — but the engine
                // judges actions by the tick-start position, so upgrading
                // this tick is still legal while stepping off.
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            spatial
                                [ "ctrl-1", { X = 10; Y = 10 } ]
                                [
                                    { X = 10; Y = 10 }, Plain
                                    { X = 10; Y = 11 }, Plain
                                    { X = 10; Y = 12 }, Plain
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 12 } ]
                                    Obstacles =
                                        Set.ofList [ { X = 10; Y = 10 }; { X = 10; Y = 12 } ]
                                })
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.contains
                    intents
                    (UpgradeController("w1", "ctrl-1"))
                    "in range at tick start: the action stays legal"

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Top ]
                    "and the creep steps onto the one legal standing tile"
            }

            test "an unreachable Work Area yields no move Intent at all" {
                // The source's Seat exists but the tiles between creep and
                // Seat are outside the projection: no path, so the creep
                // waits instead of thrashing.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 } ]
                                [ { X = 10; Y = 11 }, Plain; { X = 10; Y = 14 }, Plain ]
                            |> withCreepsAt [ "w1", { X = 10; Y = 14 } ]
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.isEmpty (moveIntents intents) "no path: standing still beats oscillating"
                Expect.isEmpty (actionIntents intents) "and the target is out of range"
            }

            test "a builder works from range 3 without closing in" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1"; Left = siteOwes } ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            spatial
                                [ "site-1", { X = 10; Y = 10 } ]
                                [ for y in 10..13 -> { X = 10; Y = y }, Plain ]
                            |> withCreepsAt [ "w1", { X = 10; Y = 13 } ]
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.contains intents (BuildSite("w1", "site-1")) "range 3 is close enough"
                Expect.isEmpty (moveIntents intents) "no reason to walk closer"
            }

            test "a refiller two tiles out still has to walk to the structure" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            spatial
                                [ "spawn-1", { X = 10; Y = 10 } ]
                                [ for y in 10..12 -> { X = 10; Y = y }, Plain ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 12 } ]
                                    Obstacles = Set.singleton { X = 10; Y = 10 }
                                })
                    }

                let { Intents = intents } = decideOn snapshot

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Top ]
                    "transfer needs range 1, so the creep closes in"

                Expect.isEmpty (actionIntents intents) "no transfer from range 2"
            }
        ]

[<Tests>]
let arbitrationTests =
    testList
        "yield arbitration"
        [
            test
                "squatting regression: the upgrader on the sole Seat yields to the inbound harvester" {
                // Source at (10,10) with (10,11) as its only Seat; controller
                // at (10,14), so the Seat is also at upgrade range 3. The
                // upgrader squats the Seat; the harvester stands one tile out.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 14 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 9; Y = 12 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 11; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "har" 0 50; worker "upg" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 14 } ]
                                terrain
                            |> withCreepsAt [ "har", { X = 10; Y = 12 }; "upg", { X = 10; Y = 11 } ]
                    }

                let assigned = [ "har", Harvest "src-a"; "upg", Upgrade "ctrl-1" ]
                let moves = resolveOn snapshot assigned |> moveIntents

                Expect.contains moves ("har", Top) "the harvester steps onto the Seat"

                Expect.contains
                    (emitOn snapshot assigned)
                    (UpgradeController("upg", "ctrl-1"))
                    "the displaced upgrader still upgrades this tick"

                match moves |> List.filter (fun (name, _) -> name = "upg") with
                | [ (_, direction) ] ->
                    let dest = stepFrom { X = 10; Y = 11 } direction

                    Expect.isLessThanOrEqual
                        (max (abs (dest.X - 10)) (abs (dest.Y - 14)))
                        3
                        "the upgrader is displaced to a tile still inside its Work Area"
                | other -> failtest $"expected exactly one move for the upgrader, got %A{other}"
            }

            test "head-on swap: two creeps blocking each other exchange tiles" {
                let moves =
                    resolveOn headOnSwap [ "wa", Harvest "src-a"; "wb", Harvest "src-b" ]
                    |> moveIntents

                Expect.equal
                    (moves |> List.sort)
                    [ "wa", Top; "wb", Bottom ]
                    "both creeps move: they swap instead of deadlocking"
            }

            test "pipeline wiring: remembered assignments flow through match, emit, and resolve" {
                // The one arbitration test that still runs the whole decide
                // seam: sticky Assignments survive the Matcher, the Emitter
                // says their glyphs, and the Resolver settles the swap.
                let sticky =
                    Map.ofList
                        [ "wa", (taskId (Harvest "src-a")); "wb", (taskId (Harvest "src-b")) ]

                let {
                        Intents = intents
                        Assignments = next
                    } =
                    decideFrom sticky headOnSwap

                Expect.equal next sticky "the Matcher keeps both remembered assignments"

                Expect.contains
                    intents
                    (SayCreep("wa", "⛏"))
                    "the Emitter's bubbles reach decide's output"

                Expect.equal
                    (moveIntents intents |> List.sort)
                    [ "wa", Top; "wb", Bottom ]
                    "the Resolver's swap reaches decide's output"
            }

            test "an idle creep is displaced by a working creep passing through" {
                // w2 carries no assignment and idles astride the harvester's
                // path.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 50 0 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withCreepsAt [ "w1", { X = 10; Y = 13 }; "w2", { X = 10; Y = 12 } ]
                    }

                let moves = resolveOn snapshot [ "w1", Harvest "src-a" ] |> moveIntents

                Expect.contains moves ("w1", Top) "the working creep claims the idler's tile"

                Expect.isTrue
                    (moves |> List.exists (fun (name, _) -> name = "w2"))
                    "the idler is displaced out of the way"
            }

            test "a contested tile goes to the higher task rank" {
                // One gap at (10,12): the harvester's and the upgrader's
                // cheapest paths both step onto it. Harvest outranks Upgrade,
                // so the gap is the harvester's, and the loser's tail holds
                // the tile the harvester just left, so it follows up the
                // corridor.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 8 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Plain
                        { X = 10; Y = 13 }, Plain
                        { X = 11; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "h" 0 50; worker "u" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 10 }; "ctrl-1", { X = 10; Y = 8 } ]
                                terrain
                            |> withCreepsAt [ "h", { X = 10; Y = 13 }; "u", { X = 11; Y = 13 } ]
                    }

                let moves =
                    resolveOn snapshot [ "h", Harvest "src-a"; "u", Upgrade "ctrl-1" ]
                    |> moveIntents

                Expect.equal
                    moves
                    [ "h", Top; "u", Left ]
                    "the harvester takes the gap; the outranked upgrader follows into the tile it left"
            }

            test "within a rank the most-constrained creep places first" {
                // Two Seats; h1 sits on the one h2's cheapest path targets.
                // h2 (one candidate tile) outranks h1 (two) inside the same
                // priority, so h1 shuffles along to the free Seat.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 11; Y = 11 }, Plain
                        { X = 9; Y = 12 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "h1" 0 50; worker "h2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withCreepsAt [ "h1", { X = 10; Y = 11 }; "h2", { X = 9; Y = 12 } ]
                    }

                let assigned = [ "h1", Harvest "src-a"; "h2", Harvest "src-a" ]

                Expect.equal
                    (resolveOn snapshot assigned |> moveIntents |> List.sort)
                    [ "h1", Right; "h2", TopRight ]
                    "h2 claims the occupied Seat; h1 is displaced to the free one"

                Expect.contains
                    (emitOn snapshot assigned)
                    (HarvestSource("h1", "src-a"))
                    "the displaced harvester still harvests this tick"
            }

            test "a builder blocked by a seated harvester still makes progress" {
                // Corridor y=12, x 8..15. Source at (10,11) seats the
                // harvester mid-corridor; the site sits at the far end. The
                // builder's only path runs through the seated harvester's
                // tile — it must not stand idle while a swap (or an in-area
                // shuffle by the harvester) would let it pass.
                let snapshot = blockedLane [ 12 ] (worker "har" 0 50)

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]
                let moves = resolveOn snapshot assigned |> moveIntents

                Expect.isTrue
                    (moves |> List.exists (fun (name, _) -> name = "bob"))
                    "the travelling builder moves instead of stalling behind the seat"

                Expect.contains
                    (emitOn snapshot assigned)
                    (HarvestSource("har", "src-a"))
                    "the harvester still harvests this tick"
            }

            test "a fatigued creep is never asked to move, nor displaced through" {
                // The same one-lane corridor, but the seated harvester is
                // still paying off fatigue: the engine would answer any move
                // with ERR_TIRED, so the Resolver issues none — neither to
                // the harvester nor to the builder whose only path runs
                // through its blocked tile.
                let snapshot = blockedLane [ 12 ] { worker "har" 0 50 with Fatigue = 4 }

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]

                Expect.isEmpty
                    (resolveOn snapshot assigned |> moveIntents)
                    "no move Intent the engine would refuse with ERR_TIRED"

                Expect.contains
                    (emitOn snapshot assigned)
                    (HarvestSource("har", "src-a"))
                    "the tired harvester still harvests this tick"
            }

            test "a fatigued traveller stands down for the tick instead of failing a move" {
                // The live -11 spam came from loaded travellers: a creep
                // mid-journey with fatigue outstanding used to be issued its
                // next step anyway, which the engine refused every tick.
                let snapshot =
                    corridorColony
                        [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        [ "w1", { X = 10; Y = 14 } ]

                Expect.isEmpty
                    (resolveOn snapshot [ "w1", Harvest "src-a" ] |> moveIntents)
                    "a rested copy of this creep would step Top; the tired one is issued nothing"
            }

            test "a travelling builder detours around a seated harvester when a lane is open" {
                // The corridor grows a parallel lane at y = 13. The straight
                // path runs through the seated harvester's tile; the flood
                // prices that tile dearer for the standing creep, so the
                // builder sidesteps into the lane instead of displacing the
                // Seat.
                let snapshot = blockedLane [ 12; 13 ] (worker "har" 0 50)

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]

                Expect.equal
                    (resolveOn snapshot assigned |> moveIntents)
                    [ "bob", BottomRight ]
                    "the builder takes the lane; the seated harvester is left alone"
            }

            test "an occupant with no in-area alternative steps out of its Work Area" {
                // The upgrader's only in-area standing tile is the Seat
                // itself: every adjacent walkable tile is outside upgrade
                // range. Displaced, it takes the first ground beside it and
                // leaves the area. It upgrades this tick either way: the
                // Emitter judges from tick-start geometry.
                let terrain =
                    [
                        { X = 11; Y = 12 }, Wall
                        { X = 13; Y = 12 }, Wall
                        { X = 10; Y = 12 }, Plain
                        { X = 9; Y = 11 }, Plain
                        { X = 9; Y = 12 }, Plain
                        { X = 9; Y = 13 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "har" 0 50; worker "upg" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 11; Y = 12 }; "ctrl-1", { X = 13; Y = 12 } ]
                                terrain
                            |> withCreepsAt [ "har", { X = 9; Y = 12 }; "upg", { X = 10; Y = 12 } ]
                    }

                let assigned = [ "har", Harvest "src-a"; "upg", Upgrade "ctrl-1" ]

                Expect.equal
                    (resolveOn snapshot assigned |> moveIntents |> List.sort)
                    [ "har", Right; "upg", TopLeft ]
                    "the displacer takes the Seat and the occupant steps off it"

                Expect.contains
                    (emitOn snapshot assigned)
                    (UpgradeController("upg", "ctrl-1"))
                    "the swapped-out upgrader still upgrades from its tick-start tile"
            }

            test "a traveller whose step is walled steps aside where there is anywhere to" {
                // Eight creeps once stood in W13S28's north corridor for ten
                // minutes: a swap needs both parties rested on one tick, and
                // the body on the step was fatigued every other tick. The
                // pocket at (11,11) is reachable and leads nowhere, so the
                // occupancy surcharge will never route through it: what takes
                // it is the Move Intent's tail.
                let tired = { worker "wb" 0 50 with Fatigue = 4 }
                let assigned = [ "eb", Harvest "src-e"; "wb", Harvest "src-w" ]

                Expect.equal
                    (resolveOn (laneWith true [ tired ] false) assigned |> moveIntents)
                    [ "eb", Top ]
                    "the east-bound body steps out of the lane rather than standing in it"

                Expect.isEmpty
                    (resolveOn (laneWith false [ tired ] false) assigned |> moveIntents)
                    "and with no ground beside the step it waits, as a grounding that drains two a tick deserves (ADR 0008)"
            }

            test "a chain three deep settles three bodies onto three tiles" {
                // Injectivity at the one shape that can lose it: a chain
                // whose innermost creep wants the tile the chain's own
                // initiator is claiming. Two bodies judged onto one tile is
                // two `MoveCreep`s the engine resolves by deleting one in
                // silence.
                let queue positions =
                    { bareRespawn with
                        Sources = [ source "src-w"; source "src-e" ]
                        Controller = None
                        Creeps = [ worker "aa" 0 50; worker "bb" 0 50; worker "cc" 0 50 ]
                        Spatial = lane false |> withCreepsAt positions
                    }

                let places =
                    [ "aa", { X = 8; Y = 12 }; "bb", { X = 9; Y = 12 }; "cc", { X = 10; Y = 12 } ]

                // aa and bb head east and cc heads west, so aa's chain runs
                // aa → bb's tile → cc's tile, and cc's own first candidate
                // is the tile aa is taking.
                let assigned =
                    [ "aa", Harvest "src-e"; "bb", Harvest "src-e"; "cc", Harvest "src-w" ]

                let stepOf = resolveOn (queue places) assigned |> moveIntents |> Map.ofList

                let settled =
                    places
                    |> List.map (fun (creep, pos) ->
                        match Map.tryFind creep stepOf with
                        | Some direction -> stepFrom pos direction
                        | None -> pos)

                Expect.hasLength
                    (List.distinct settled)
                    (List.length settled)
                    "three bodies, three tiles: the pass is a matching"
            }

            test "a ring creep whose step is held keeps every tile beside it" {
                // A creep on the border ring cannot stay put — that is a
                // bounce back across the border every other tick — so its
                // tail is every ground tile beside it, not only those beside
                // its step. The step is (9,1) and the ground beside runs
                // (9,1) (10,1) (11,1); with the first two held by fatigued
                // bodies, (11,1) is the only tile there is.
                let ring held =
                    { bareRespawn with
                        Spawns = []
                        Controller = None
                        Refillables = []
                        Sources = [ source "src-home" ]
                        Creeps = worker "r" 0 50 :: held
                        Spatial =
                            { SpatialInfo.empty with
                                RoomName = Some "W1N1"
                            }
                            |> withHome (fun layer ->
                                { layer with
                                    Terrain =
                                        TerrainGrid.ofList
                                            [
                                                { X = 9; Y = 1 }, Plain
                                                { X = 10; Y = 1 }, Plain
                                                { X = 11; Y = 1 }, Plain
                                                { X = 9; Y = 2 }, Plain
                                                { X = 9; Y = 3 }, Plain
                                                { X = 9; Y = 4 }, Plain
                                                { X = 9; Y = 5 }, Wall
                                            ]
                                    TargetPositions = Map.ofList [ "src-home", { X = 9; Y = 5 } ]
                                    CreepPositions =
                                        Map.ofList (
                                            ("r", { X = 10; Y = 0 })
                                            :: (held
                                                |> List.map (fun c ->
                                                    c.Name,
                                                    if c.Name = "on-step" then
                                                        { X = 9; Y = 1 }
                                                    else
                                                        { X = 10; Y = 1 }))
                                        )
                                })
                    }

                let assigned = [ "r", Harvest "src-home" ]

                Expect.equal
                    (resolveOn (ring []) assigned |> moveIntents)
                    [ "r", BottomLeft ]
                    "the premise: with the ring clear the creep walks its step off the ring"

                let asleep name = { worker name 0 50 with Fatigue = 4 }

                Expect.equal
                    (resolveOn (ring [ asleep "on-step"; asleep "beside" ]) assigned |> moveIntents)
                    [ "r", BottomRight ]
                    "and with the step and the tile beside it walled it takes the third, rather than bouncing across the border"
            }

            test "head-on with alternating fatigue, both bodies arrive — over ticks, not one" {
                // Two bodies meeting head-on in a lane, tired on opposite
                // ticks so the tick both could swap never comes, walked until
                // they arrive or the run gives up. The pocket lane is the case
                // and the strictly one-wide lane the negative. Two rules clear
                // it: the east-bound body takes the pocket off its tail, and
                // the west-bound one is priced round the body in its way by
                // the occupancy surcharge.
                let assigned = [ "eb", Harvest "src-e"; "wb", Harvest "src-w" ]

                let tickOf pocket tick positions =
                    { bareRespawn with
                        Sources = [ source "src-w"; source "src-e" ]
                        Controller = None
                        Creeps =
                            [
                                { worker "eb" 0 50 with
                                    Fatigue = if tick % 2 = 1 then 4 else 0
                                }
                                { worker "wb" 0 50 with
                                    Fatigue = if tick % 2 = 0 then 4 else 0
                                }
                            ]
                        Spatial =
                            lane pocket
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = positions
                                })
                    }

                let start = Map.ofList [ "eb", { X = 11; Y = 12 }; "wb", { X = 12; Y = 12 } ]

                // The Seats at the two ends: the east rock's is (15,12) and
                // the west rock's is (8,12), so "arrived" is a tile and the
                // two bodies have to have passed each other to reach them.
                let arrived (positions: Map<string, Pos>) =
                    positions["eb"] = { X = 15; Y = 12 } && positions["wb"] = { X = 8; Y = 12 }

                let rec drive pocket tick positions =
                    if arrived positions then
                        Some tick
                    elif tick > 20 then
                        None
                    else
                        let stepped =
                            (positions,
                             resolveOn (tickOf pocket tick positions) assigned |> moveIntents)
                            ||> List.fold (fun acc (name, direction) ->
                                Map.add name (stepFrom acc[name] direction) acc)

                        drive pocket (tick + 1) stepped

                Expect.equal
                    (drive true 0 start)
                    (Some 9)
                    "with one tile of ground beside the lane, both bodies reach their Seats"

                Expect.isNone
                    (drive false 0 start)
                    "and in a lane with nothing beside it neither ever does, which is the wait ADR 0008 keeps"
            }

            test "a body with no Task parks off the working ground, and stays put beside it" {
                // Pairwise, one tile apart: the same idle body on the first
                // tile of the Upgrade Work Area and on the corridor tile
                // beside it.
                let moved pos =
                    resolveOn (pocketColony [ upgrader "u" ] [ "u", pos ]) [] |> moveIntents

                Expect.equal
                    (moved { X = 21; Y = 14 })
                    [ "u", Left ]
                    "on the pocket's mouth it steps out into the corridor"

                Expect.isEmpty
                    (moved { X = 20; Y = 14 })
                    "one tile west, off every Work Area, it has no reason to move at all"

                // Seal the mouth and the pocket's working ground has no
                // ground off it at all, so the body parks.
                let sealedPocket =
                    pocketRoom
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = TerrainGrid.remove { X = 20; Y = 14 } layer.Terrain
                        })

                Expect.isEmpty
                    (resolveOn
                        (pocketColonyIn sealedPocket [ upgrader "u" ] [ "u", { X = 21; Y = 14 } ])
                        []
                     |> moveIntents)
                    "and walled into the pocket, with nowhere off the ground to go, it parks"
            }

            test "the idle body deep in the pocket walks out of it over ticks" {
                // The same rule where the way off is more than one step: the
                // buffer's own tile is as far inside the pocket as ground
                // goes, and the head is the first step of the walk out rather
                // than a neighbour that happens to be off it — there is no
                // such neighbour.
                let rec walk tick pos =
                    if tick > 4 then
                        pos
                    else
                        match
                            resolveOn (pocketColony [ upgrader "u" ] [ "u", pos ]) [] |> moveIntents
                        with
                        | [ (_, direction) ] -> walk (tick + 1) (stepFrom pos direction)
                        | _ -> pos

                Expect.equal
                    (walk 0 { X = 22; Y = 15 })
                    { X = 20; Y = 14 }
                    "it settles on the first tile outside the working ground and stops there"
            }

            test "#241 the pocket jam: the hauler feeding the buffer gets in" {
                // The live jam: the buffer empty, two idle upgraders parked on
                // two of its five standing tiles, two loaded workers walking
                // in, and the hauler carrying the energy that would have
                // un-idled the upgraders never getting past the mouth. Driven
                // over ticks, because one tick cannot show it.
                let creeps =
                    [
                        upgrader "u1"
                        upgrader "u2"
                        worker "w1" 50 0
                        worker "w2" 50 0
                        hauler "h" 100 0
                    ]

                let assigned =
                    [
                        "w1", Upgrade "ctrl-1"
                        "w2", Upgrade "ctrl-1"
                        "h", Refill("can-buf", Energy)
                    ]

                let start =
                    Map.ofList
                        [
                            "u1", { X = 21; Y = 15 }
                            "u2", { X = 22; Y = 14 }
                            "w1", { X = 20; Y = 14 }
                            "w2", { X = 19; Y = 14 }
                            "h", { X = 18; Y = 14 }
                        ]

                // Ten ticks of it: three for the hauler's walk in and seven
                // more of the steady state the jam never reached.
                let ticks = walkedTicks (pocketColony creeps) assigned 10 start

                let besideBuffer (positions: Map<string, Pos>) =
                    range positions["h"] { X = 22; Y = 15 } <= 1

                Expect.isTrue
                    (ticks |> List.skip 3 |> List.forall besideBuffer)
                    "the hauler is standing beside the buffer by the third tick and stays there"

                // The other half of the acceptance: nobody trades tiles with
                // anybody two ticks running.
                Expect.isEmpty
                    (repeatedSwaps ticks)
                    "no pair of bodies exchanges tiles on two consecutive ticks"
            }

            test "two idle bodies in the mirrored pocket settle instead of trading its mouth" {
                // The orientation the room happens not to have: the corridor
                // running east, so the working ground sorts below the tile off
                // it. With an unordered tail the body shoved off the mouth
                // lands back inside the pocket and the pair exchanges the
                // mouth for as long as the run lasts.
                let creeps = [ upgrader "u1"; upgrader "u2" ]

                let ticks =
                    walkedTicks
                        (pocketColonyIn mirroredPocketRoom creeps)
                        []
                        10
                        (Map.ofList [ "u1", { X = 19; Y = 15 }; "u2", { X = 18; Y = 14 } ])

                Expect.isEmpty
                    (repeatedSwaps ticks)
                    "neither body is shoved back onto the ground it was told to leave"

                Expect.equal
                    (List.last ticks |> Map.toList |> List.map snd |> List.sortBy (fun p -> p.X))
                    [ { X = 20; Y = 14 }; { X = 21; Y = 14 } ]
                    "both settle on the corridor, one behind the other, and stop"
            }

            test "#267 the full pocket: an arrived body is not evicted from its Work Area" {
                // The pocket's six tiles are the whole Upgrade Work Area, five
                // upgraders and the hauler stand on them, and a sixth upgrader
                // walks up the corridor onto the mouth. Once the body holding
                // the mouth could be pushed out of the pocket for nothing.
                let creeps = [ for n in 1..6 -> worker $"w%d{n}" 50 0 ] @ [ hauler "h" 100 0 ]

                let assigned =
                    [ for n in 1..6 -> $"w%d{n}", Upgrade "ctrl-1" ]
                    @ [ "h", Refill("can-buf", Energy) ]

                let start =
                    Map.ofList
                        [
                            "h", { X = 21; Y = 15 }
                            "w1", { X = 21; Y = 14 }
                            "w2", { X = 22; Y = 14 }
                            "w3", { X = 23; Y = 14 }
                            "w4", { X = 22; Y = 15 }
                            "w5", { X = 23; Y = 15 }
                            "w6", { X = 20; Y = 14 }
                        ]

                let ticks = walkedTicks (pocketColony creeps) assigned 6 start

                Expect.isEmpty
                    (repeatedSwaps ticks)
                    "no pair of bodies exchanges tiles on two consecutive ticks"

                Expect.equal
                    (List.last ticks)
                    start
                    "the pocket is full and everybody in it stays where it works"

                Expect.isTrue
                    (ticks |> List.forall (fun p -> range p["h"] { X = 22; Y = 15 } <= 1))
                    "the hauler keeps the tile it feeds the buffer from"
            }

            test "#267 one tile short: the chain that shuffles a body inside its area still runs" {
                // The pairwise half: the same pocket with (23,15) empty, so
                // the displaced body has somewhere in its own Work Area to go
                // and the chain costs the arbitration nothing.
                let creeps = [ for n in 1..5 -> worker $"w%d{n}" 50 0 ] @ [ hauler "h" 100 0 ]

                let assigned =
                    [ for n in 1..5 -> $"w%d{n}", Upgrade "ctrl-1" ]
                    @ [ "h", Refill("can-buf", Energy) ]

                let start =
                    Map.ofList
                        [
                            "h", { X = 21; Y = 15 }
                            "w1", { X = 21; Y = 14 }
                            "w2", { X = 22; Y = 14 }
                            "w3", { X = 23; Y = 14 }
                            "w4", { X = 22; Y = 15 }
                            "w5", { X = 20; Y = 14 }
                        ]

                let settled = walkedTicks (pocketColony creeps) assigned 1 start |> List.last

                Expect.equal settled["w5"] { X = 21; Y = 14 } "the traveller takes the mouth"

                Expect.notEqual
                    settled["w1"]
                    start["w1"]
                    "the body holding it is displaced, exactly as it was before this ticket"

                Expect.isTrue
                    (range settled["w1"] { X = 24; Y = 17 } <= 3)
                    "and re-housed inside the Upgrade Work Area, which costs the chain nothing"
            }

            test "#268 the storage against a wall: the idle bodies ring it and the hauler gets in" {
                // The stock has two standing tiles and no working ground
                // among them, so a pair of idle bodies parked on them shut the
                // hauler out of the room's only Task, and a rule reading the
                // working ground alone had nothing to say to either of them.
                let creeps = [ worker "u1" 0 50; worker "u2" 0 50; hauler "h" 100 0 ]

                let ticks =
                    walkedTicks
                        (wallStorageColony creeps)
                        [ "h", Refill("sto-1", Energy) ]
                        8
                        (Map.ofList
                            [
                                "u1", { X = 10; Y = 11 }
                                "u2", { X = 11; Y = 11 }
                                "h", { X = 14; Y = 11 }
                            ])

                let besideStorage (positions: Map<string, Pos>) =
                    range positions["h"] { X = 10; Y = 10 } <= 1

                // An idle body is the lightest push there is, so the hauler's
                // chain shoves the one on (11,11) aside whatever rule the
                // mover reads.
                Expect.isTrue
                    (ticks |> List.skip 3 |> List.forall besideStorage)
                    "the hauler is standing beside the stock by the third tick and stays there"

                // The half the arbitration cannot reach: (10,11) is a dead
                // end, so the body parked on it is in nobody's head candidate
                // once the hauler holds the other tile and is never displaced.
                // It leaves because the rule tells it to.
                let idleOnTheRing (positions: Map<string, Pos>) =
                    [ "u1"; "u2" ]
                    |> List.filter (fun name -> range positions[name] { X = 10; Y = 10 } <= 1)

                Expect.isTrue
                    (ticks |> List.skip 3 |> List.forall (idleOnTheRing >> List.isEmpty))
                    "and by then neither idle body is left standing on the stock's two tiles"

                Expect.isEmpty
                    (repeatedSwaps ticks)
                    "and no pair of bodies exchanges tiles on two consecutive ticks"
            }

            test "#277 a wall-tucked tower: the idle bodies ring it and the hauler gets in" {
                // The same pocket with a tower: a hauler holding its Refill
                // queues on its two standing tiles exactly as the stock's
                // does, and once those tiles were no store's ring.
                let creeps = [ worker "u1" 0 50; worker "u2" 0 50; hauler "h" 100 0 ]

                let ticks =
                    walkedTicks
                        (wallTowerColony creeps)
                        [ "h", Refill("tow-1", Energy) ]
                        8
                        (Map.ofList
                            [
                                "u1", { X = 10; Y = 11 }
                                "u2", { X = 11; Y = 11 }
                                "h", { X = 14; Y = 11 }
                            ])

                let besideTower (positions: Map<string, Pos>) =
                    range positions["h"] { X = 10; Y = 10 } <= 1

                Expect.isTrue
                    (ticks |> List.skip 3 |> List.forall besideTower)
                    "the hauler is standing beside the tower by the third tick and stays there"

                let idleOnTheRing (positions: Map<string, Pos>) =
                    [ "u1"; "u2" ]
                    |> List.filter (fun name -> range positions[name] { X = 10; Y = 10 } <= 1)

                Expect.isTrue
                    (ticks |> List.skip 3 |> List.forall (idleOnTheRing >> List.isEmpty))
                    "and by then neither idle body is left standing on the tower's two tiles"

                Expect.isEmpty
                    (repeatedSwaps ticks)
                    "and no pair of bodies exchanges tiles on two consecutive ticks"
            }

            test "#268 two tiles off the stock, the same bodies have no reason to move" {
                // The pairwise other side: the rule is the stores' rings and
                // not the room. (12,11) and (13,11) are two and three tiles
                // from the stock, off every ring the colony draws from or
                // fills, so nothing walks them anywhere.
                Expect.isEmpty
                    (resolveOn
                        (wallStorageColony
                            [ worker "u1" 0 50; worker "u2" 0 50 ]
                            [ "u1", { X = 12; Y = 11 }; "u2", { X = 13; Y = 11 } ])
                        []
                     |> moveIntents)
                    "standing off every store's ring, an idle body stays where it is"
            }

            test "#267 the free tile is not adjacent: the row shuffles up and nobody leaves" {
                // The Work Area has free tiles, just none beside the body on
                // the mouth: four upgraders inside with (23,14) and (23,15)
                // empty, and a fifth walking in. The row steps up: the
                // traveller takes the mouth and the free tile at the back is
                // what the shuffle spends.
                let creeps = [ for n in 1..5 -> worker $"w%d{n}" 50 0 ]
                let assigned = [ for n in 1..5 -> $"w%d{n}", Upgrade "ctrl-1" ]

                let start =
                    Map.ofList
                        [
                            "w1", { X = 22; Y = 15 }
                            "w2", { X = 22; Y = 14 }
                            "w3", { X = 21; Y = 14 }
                            "w4", { X = 20; Y = 14 }
                            "w5", { X = 21; Y = 15 }
                        ]

                let ticks = walkedTicks (pocketColony creeps) assigned 6 start

                Expect.isEmpty
                    (repeatedSwaps ticks)
                    "no pair of bodies exchanges tiles on two consecutive ticks"

                Expect.equal
                    (ticks |> List.skip 1 |> List.distinct |> List.length)
                    1
                    "the pocket settles on the first tick and nothing moves again"

                let settled = List.last ticks

                Expect.equal settled["w4"] { X = 21; Y = 14 } "the traveller takes the mouth"

                Expect.isTrue
                    (settled |> Map.forall (fun _ p -> range p { X = 24; Y = 17 } <= 3))
                    "and every body it shuffled is still standing in the Upgrade Work Area"
            }

            test "#267 the free tile is not adjacent, in the mirrored pocket" {
                // The same five bodies mirrored: ties fall to the lowest x
                // then y, so one orientation can be right by accident. Here
                // the free tiles at the back of the pocket sort below the
                // mouth instead of above it.
                let creeps = [ for n in 1..5 -> worker $"w%d{n}" 50 0 ]
                let assigned = [ for n in 1..5 -> $"w%d{n}", Upgrade "ctrl-1" ]

                let start =
                    Map.ofList
                        [
                            "w1", { X = 18; Y = 15 }
                            "w2", { X = 18; Y = 14 }
                            "w3", { X = 19; Y = 14 }
                            "w4", { X = 20; Y = 14 }
                            "w5", { X = 19; Y = 15 }
                        ]

                let ticks = walkedTicks (pocketColonyIn mirroredPocketRoom creeps) assigned 6 start

                Expect.isEmpty
                    (repeatedSwaps ticks)
                    "no pair of bodies exchanges tiles on two consecutive ticks"

                Expect.isTrue
                    (List.last ticks |> Map.forall (fun _ p -> range p { X = 16; Y = 17 } <= 3))
                    "every body ends inside the Upgrade Work Area, the mouth included"
            }

            test "another colony's body is an occupant this colony cannot claim" {
                // The mother's pioneer once stood for fifteen minutes with its
                // first step the Post tile the child's Anchor was garrisoning:
                // a tile her layer did not carry, so the arbitration handed it
                // over every tick for a body the engine would never let her
                // into.
                let assigned = [ "eb", Harvest "src-e" ]

                Expect.equal
                    (resolveOn (laneWith true [] true) assigned |> moveIntents)
                    [ "eb", Top ]
                    "carried as a foreign body, the tile is one the mover steps around"

                Expect.equal
                    (resolveOn (laneWith true [] false) assigned |> moveIntents)
                    [ "eb", Right ]
                    "not carried, the tile reads free and the mover walks into a creep that never moves"
            }

            test "a foreign body's tile is priced at the occupancy surcharge" {
                // The half no arbitration can do: the walking flood charges a
                // foreign body's tile like any other occupied tile, so a
                // traveller is priced around a garrison it can never
                // displace. The way round is one swamp tile at (12,11), dearer
                // than the lane by eight units and cheaper than the lane plus
                // the ten-unit surcharge, so the pricing alone decides.
                let bypass =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                TerrainGrid.ofList (
                                    [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]
                                    @ [ { X = 12; Y = 11 }, Swamp; { X = 16; Y = 12 }, Wall ]
                                )
                            TargetPositions = Map.ofList [ "src-e", { X = 16; Y = 12 } ]
                            CreepPositions = Map.ofList [ "eb", { X = 11; Y = 12 } ]
                        })

                let colony foreign =
                    { bareRespawn with
                        Sources = [ source "src-e" ]
                        Controller = None
                        Creeps = [ worker "eb" 0 50 ]
                        Spatial = bypass
                        Foreign =
                            if foreign then
                                Set.singleton (RoomPos.at "W1N1" { X = 12; Y = 12 })
                            else
                                Set.empty
                    }

                let assigned = [ "eb", Harvest "src-e" ]

                Expect.equal
                    (resolveOn (colony true) assigned |> moveIntents)
                    [ "eb", TopRight ]
                    "priced at the surcharge, the cheapest step is the lane beside the garrison"

                Expect.equal
                    (resolveOn (colony false) assigned |> moveIntents)
                    [ "eb", Right ]
                    "priced at nothing, it is the garrison's own tile"
            }

            test "a rested creep that gets none of its candidates says which kind of nothing it got" {
                // A kept traveller that simply fails to move once emitted
                // nothing at all. Which Verdict it is turns on whether the
                // pass can name the holder: one of ours on the tile is a
                // counterpart, a foreign body is not.
                let assigned = [ "eb", Harvest "src-e"; "wb", Harvest "src-w" ]
                let tired = { worker "wb" 0 50 with Fatigue = 4 }

                Expect.contains
                    (resolveVerdictsOn (laneWith false [ tired ] false) assigned)
                    (Verdict.Yielded("eb", "wb"))
                    "a fatigued body of ours on the tile is named"

                Expect.contains
                    (resolveVerdictsOn (laneWith false [] true) [ "eb", Harvest "src-e" ])
                    (Verdict.Stalled "eb")
                    "a body this colony does not hold cannot be, so the creep is stalled and not yielded"
            }

            test "one pass per room arbitrates both colonies' bodies against each other" {
                // The mother and the child both work the child's room. Folded
                // into one pass the two bodies are ordinary occupants of one
                // room, each moving on the intent its own colony registered,
                // so the head-on pair swaps, which neither colony could have
                // settled alone.
                let mother = laneWith false [] true

                let child =
                    { bareRespawn with
                        Sources = [ source "src-w"; source "src-e" ]
                        Controller = None
                        Creeps = [ worker "an" 0 50 ]
                        Spatial = lane false |> withCreepsAt [ "an", { X = 12; Y = 12 } ]
                        Foreign = Set.singleton (RoomPos.at "W1N1" { X = 11; Y = 12 })
                    }

                let movementFor view assigned =
                    movementOf
                        view
                        (Atlas.ofView view)
                        noThreats
                        (poolOn view)
                        (Map.ofList assigned)
                        Map.empty
                        Set.empty

                let together =
                    resolveRooms
                        [
                            movementFor mother [ "eb", Harvest "src-e" ]
                            movementFor child [ "an", Harvest "src-w" ]
                        ]
                    |> fst
                    |> moveIntents

                Expect.equal
                    (together |> List.sort)
                    [ "an", Left; "eb", Right ]
                    "both bodies move: one room, one pass, two colonies' intents"

                Expect.isEmpty
                    (resolveOn mother [ "eb", Harvest "src-e" ] |> moveIntents)
                    "and the mother alone can only see a tile she must not claim"
            }
        ]

/// Every rank the ladder puts a Task on: each tier's own, read off the `Tier`
/// union so an added tier joins the walk without anybody remembering to, and
/// the downgrade deadline's beside them, read off `Pool.deadlineRank` rather
/// than re-derived. Reflection lives in the test projects alone, so none of
/// it reaches the Fable bundle.
let private ladderRanks: (string * int) list =
    let tiers =
        FSharpType.GetUnionCases typeof<Tier>
        |> Array.toList
        |> List.map (fun case ->
            case.Name, priorityOfTier (FSharpValue.MakeUnion(case, [||]) :?> Tier))

    ("the downgrade deadline", deadlineRank) :: tiers |> List.sortBy snd

/// Every rung the Planner may step a Task by inside its tier, off the `Rung`
/// union itself, so the walk below checks a new rung against
/// `weightOfRank`'s window without anybody widening a literal here.
let private ladderRungs: (string * int) list =
    FSharpType.GetUnionCases typeof<Rung>
    |> Array.toList
    |> List.map (fun case -> case.Name, rankOfRung (FSharpValue.MakeUnion(case, [||]) :?> Rung))

/// The tier a rank belongs to: the nearest one, ties going to the deeper — the
/// rounding `weightOfRank` is supposed to do, written a second way so the test
/// does not assert the formula against itself.
let private nearestRank (rank: int) =
    ladderRanks |> List.minBy (fun (_, at) -> abs (at - rank), -at)

[<Tests>]
let pushWeightTests =
    testList
        "push weight"
        [
            test "#237 every rank inside a tier pushes with that tier's own weight" {
                // Every rank the window admits, one rank short at the deep
                // end because a tie rounds to the deeper tier: a tier owns
                // `tierRungs / 2` up and `tierRungs / 2 - 1` down. The idle
                // rank is not on the ladder and is pinned below.
                let half = tierRungs / 2
                let shallowest = ladderRanks |> List.map snd |> List.min
                let deepest = ladderRanks |> List.map snd |> List.max

                for rank in (shallowest - half) .. (deepest + half - 1) do
                    let tier, at = nearestRank rank

                    Expect.equal
                        (weightOfRank rank)
                        (weightOfRank at)
                        $"rank %d{rank} is a rung of %s{tier} and pushes as %s{tier} does"
            }

            test "#237 the boundary the ladder's rule is stated against" {
                // Where the window ends, pinned from both sides: half a tier
                // up is still the tier's own push, and one rung beyond it is
                // the tier above's. Every rung steps a Task up, so this is the
                // bound a rule may take.
                let half = tierRungs / 2

                for (shallower, above), (name, at) in List.pairwise ladderRanks do
                    Expect.equal
                        (weightOfRank (at - half))
                        (weightOfRank at)
                        $"half a tier above %s{name} is still %s{name}'s own push"

                    Expect.equal
                        (weightOfRank (at - half - 1))
                        (weightOfRank above)
                        $"one rung further and it is %s{shallower}'s, which is the defect #237 cured"
            }

            test "#237 the rungs the Planner steps still push as the tier does" {
                // Walked off the `Rung` union rather than a literal. Once the
                // second rung rounded past its tier, so in a corridor a body
                // holding a full container's Withdraw pushed harder than one
                // holding the spawn's Refill; that corridor is the case below.
                for name, at in ladderRanks do
                    for rung, step in ladderRungs do
                        Expect.equal
                            (weightOfRank (at + step))
                            (weightOfRank at)
                            $"%s{rung} inside %s{name} is still %s{name}'s push"
            }

            test "one tier is one unit of push weight, and a body with no Task the least" {
                // A body is taken off its work only by a chain worth more than
                // the sidestep, which is a body one tier up and never one
                // rung, so the ladder's tiers must come out one apart, in
                // order, and an idle body must still push with something.
                let weights = ladderRanks |> List.map (fun (name, at) -> name, weightOfRank at)

                Expect.equal
                    (weights |> List.map snd)
                    (weights |> List.mapi (fun i _ -> List.length weights - i + 1))
                    $"each tier pushes one more than the tier below it: %A{weights}"

                Expect.equal
                    (weightOfRank System.Int32.MaxValue)
                    1
                    "a creep with no Task pushes with the smallest weight there is, not none"
            }
        ]

/// The push-weight corridor: a one-tile lane y = 10 from the source
/// container at (9,10) east to the spawn walled in at (19,10), a pocket tile
/// at (12,11), and the controller at (20,10) inside its downgrade deadline.
/// Three bodies and not two: `arbitrate` offers travellers in rank order, so
/// the Withdraw's body can never shove the Refill's off a tile it has not
/// asked for yet; the deadline's body behind the Refill moves it onto the
/// lane tile both of the others want.
let private fullContainerCorridor positions =
    let lane =
        spatial [] ([ for x in 9..19 -> { X = x; Y = 10 }, Plain ] @ [ { X = 12; Y = 11 }, Plain ])
        |> withTargets
            [
                "src-a", { X = 9; Y = 9 }, Source
                "can-src", { X = 9; Y = 10 }, Structure BuiltKind.Container
                "spawn-1", { X = 19; Y = 10 }, Structure BuiltKind.Spawn
                "ctrl-1", { X = 20; Y = 10 }, Controller
            ]

    { bareRespawn with
        Sources = [ source "src-a" ]
        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
        Controller =
            Some
                { controllerAt 3 with
                    TicksToDowngrade = 4000
                }
        Creeps = [ for name, _ in positions -> worker name 50 0 ]
        Spatial =
            { lane with
                Stores = Map.ofList [ "can-src", 2000 ]
            }
            |> withCreepsAt positions
            |> withObstacles [ { X = 19; Y = 10 }; { X = 20; Y = 10 } ]
    }

[<Tests>]
let pushWeightCorridorTests =
    testList
        "push weight in a corridor"
        [
            test "#237 a full container's Withdraw does not shove the spawn's Refill aside" {
                // Both bodies want the lane tile between them and the
                // deadline's body pushes the Refill onto it. Two rungs once
                // bought the Withdraw a push weight of 7 against the Refill's
                // 6, so the shove was taken; at 6 each the chain scores 0 and
                // the Withdraw falls to its tail.
                let positions =
                    [
                        "refill", { X = 11; Y = 10 }
                        "withdraw", { X = 13; Y = 10 }
                        "deadline", { X = 10; Y = 10 }
                    ]

                let snapshot = fullContainerCorridor positions

                let assigned =
                    [
                        "refill", Refill("spawn-1", Energy)
                        "withdraw", Withdraw("can-src", Energy)
                        "deadline", Upgrade "ctrl-1"
                    ]

                Expect.equal
                    (poolOn snapshot
                     |> List.choose (fun entry ->
                         match entry.Task with
                         | Withdraw("can-src", Energy)
                         | Refill("spawn-1", Energy) -> Some(taskId entry.Task, entry.Priority)
                         | _ -> None)
                     |> List.sortBy fst)
                    [
                        taskId (Refill("spawn-1", Energy)), 0
                        taskId (Withdraw("can-src", Energy)), -2
                    ]
                    "the premise: the two Tasks are one tier and two rungs apart"

                Expect.contains
                    (resolveVerdictsOn snapshot assigned)
                    (Verdict.Yielded("withdraw", "refill"))
                    "the body that stands aside is the one holding the rung, not the one holding the tier"

                Expect.equal
                    (resolveOn snapshot assigned |> moveIntents)
                    [ "refill", Right; "withdraw", BottomLeft; "deadline", Right ]
                    "so the Refill keeps the lane and the Withdraw takes the pocket beside it"
            }
        ]
