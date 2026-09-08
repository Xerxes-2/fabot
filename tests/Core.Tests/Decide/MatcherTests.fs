/// The Matcher and the Resolver: which Tasks a body may hold, the travel cost
/// that ranks them (ADR 0002), the yield arbitration that settles a contested
/// tile (ADR 0001), the Verdicts a match and a release are returned under (ADR
/// 0009), the verbose list an operator reads (ADR 0018), and the Intents the
/// whole decision emits.
module Fabot.Core.Tests.Decide.MatcherTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

[<Tests>]
let directionCodeTests =
    testList
        "direction codes"
        [
            test "matches the engine's TOP = 1, then clockwise" {
                // These constants leave the program as Creep.move arguments; the
                // table here is the engine's spec, restated so a swapped case fails.
                Expect.equal
                    ([ Top; TopRight; Right; BottomRight; Bottom; BottomLeft; Left; TopLeft ]
                     |> List.map directionCode)
                    [ 1; 2; 3; 4; 5; 6; 7; 8 ]
                    "each Direction maps to its Screeps constant"
            }
        ]

[<Tests>]
let partApplicabilityTests =
    testList
        "part-based applicability"
        [
            test "a Work-less body is never matched to Harvest, Build, or Upgrade" {
                // Energy on board and capacity free: only the missing Work
                // part can make these tasks inapplicable.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Controller = Some(controllerAt 2)
                        Creeps = [ creepWith "hauler" 25 25 [ Carry; Move ] ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (Map.toList assignments)
                    "a body with no Work part can do none of the Work-part tasks"
            }

            test "a Carry-less body is never matched to Refill" {
                // Energy crafted non-zero so only the missing Carry part
                // can make Refill inapplicable.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ creepWith "digger" 25 25 [ Work; Move ] ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (Map.toList assignments)
                    "a body with no Carry part cannot deliver energy"
            }

            test "a remembered assignment to a task the body cannot do is released" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = []
                        Controller = None
                        Creeps = [ creepWith "hauler" 0 50 [ Carry; Move ] ]
                    }

                let remembered = Map.ofList [ "hauler", taskId (Harvest "src-a") ]
                let { Assignments = assignments } = decide snapshot remembered Set.empty None

                Expect.isEmpty
                    (Map.toList assignments)
                    "applicability release covers parts the body lacks"
            }
        ]

/// The live `9W/9C/9M` generalist of #235: neither Work-heavy (ADR 0016's
/// ratio is strict) nor a standing body (ADR 0046's is too), so it answers
/// every one of the Harvest gate's light-body clauses and no exemption. Its
/// store is the caller's, because the whole of the first clause is what the
/// store holds. Named for the [[body class]] and not "commuter", which this
/// file already spends on `Capacity.Commuters` — the crowd that is merely not
/// Heavy, the Standing row included.
let lightWorker name energy freeCapacity =
    creepWith
        name
        energy
        freeCapacity
        ([ for _ in 1..9 -> Work ]
         @ [ for _ in 1..9 -> Carry ]
         @ [ for _ in 1..9 -> Move ])

/// A three-row field y = 9..11, x = 8..15, with one source walled into the
/// middle of it at (10,10), and nothing else in the world to do: the Harvest
/// is the whole pool, so an unmatched body here was refused by the gate and
/// not outranked. Three rows so the rock is walked *around* — a body on one
/// side of a one-wide lane can reach no Seat on the other, and a Seat the
/// light body cannot reach would answer this suite's questions with ADR 0002's
/// reachability instead of the applicability it is asking about. The source
/// arrives through `withTargets` and so carries its **kind**, which the Seat
/// union is read through (ADR 0041): a projection that placed the rock and
/// did not say what it was answers no Seats, and so no Post, whatever stands
/// on them.
let loneSourceRoom =
    spatial
        []
        [
            for x in 8..15 do
                for y in 9..11 -> { X = x; Y = y }, (if (x, y) = (10, 10) then Wall else Plain)
        ]
    |> withTargets [ "src-a", { X = 10; Y = 10 }, Source ]

/// The same field with a container standing on the Seat at (11,10), which
/// makes that Seat a [[post]] (ADR 0042) and leaves the rock's seven other
/// Seats as the light body's Work Area (ADR 0051). Stocked with nothing, so
/// the container pools no Withdraw of its own and the pool stays the one
/// Harvest — what refuses a body here is the gate and never a rival.
let postedSourceRoom =
    loneSourceRoom
    |> withTargets [ "can-a", { X = 11; Y = 10 }, Structure BuiltKind.Container ]

/// The same field one step earlier: the container on (11,10) is still a
/// construction site. #205 makes that Seat a [[post]] all the same — the
/// Anchor hired for it is the body that raises it — but ADR 0042's split
/// keeps it out of the economy until the structure stands, which is the
/// half `Decide.isPosted` reads and the half #235's spare-rate clause reads
/// with it.
let siteSourceRoom =
    loneSourceRoom
    |> withTargets [ "can-a", { X = 11; Y = 10 }, Site BuiltKind.Container ]

/// The colony over one of those rooms: the named bodies on the named tiles,
/// an owned home room — so the rock is priced at the held ten a tick
/// (ADR 0042) — and no controller, refillable with room or store to pool a
/// second Task.
let sourceColony room (bodies: (CreepInfo * Pos) list) =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Controller = None
        Refillables = []
        Creeps = bodies |> List.map fst
        Spatial =
            room
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        bodies |> List.map (fun (body, pos) -> body.Name, pos) |> Map.ofList
                })
    }

[<Tests>]
let harvestApplicabilityTests =
    testList
        "harvest applicability"
        [
            // The intake mirror, on the last intake that lacked one (#235).
            // Pairwise on the store alone: one body, one Task, and the only
            // thing that moves between the two halves is what it is carrying.
            test "a light body walks to a source only while it is at least half empty" {
                let matchedAt energy free =
                    let { Assignments = assignments } =
                        decide
                            (sourceColony
                                loneSourceRoom
                                [ lightWorker "w" energy free, { X = 13; Y = 10 } ])
                            Map.empty
                            Set.empty
                            None

                    Map.tryFind "w" assignments

                // The live body: 441 of 450 aboard, nine free. Before this
                // clause that was room enough, and Harvest being Feeding it
                // outranked every Surplus Task at home — so the worker
                // crossed a Seam, dug once, released full, and crossed back.
                Expect.isNone (matchedAt 441 9) "nine free of four hundred and fifty is not a trip"

                Expect.equal
                    (matchedAt 200 250)
                    (Some(taskId (Harvest "src-a")))
                    "the same body half empty digs as it always did"
            }

            // #206 shut the Pickup and every non-buffer Withdraw for a
            // standing body and spared Harvest, reasoning that travel cost
            // would keep the upgrader row beside its buffer. It did not: an
            // empty buffer leaves the row nothing else applicable at all.
            // Pairwise on the body alone — same room, same empty store, same
            // tile — because the anchor row is a standing body too and the
            // exemption has to be read at ADR 0016's ratio and not this one.
            test "a standing body is not matched to a source, and a light body still is" {
                let idleOf (body: CreepInfo) =
                    let { Verdicts = verdicts } =
                        decide
                            (sourceColony loneSourceRoom [ body, { X = 13; Y = 10 } ])
                            Map.empty
                            Set.empty
                            None

                    verdicts

                let upgrader =
                    creepWith
                        "u"
                        0
                        50
                        ([ for _ in 1..11 -> Work ] @ [ Carry ] @ [ for _ in 1..11 -> Move ])

                Expect.equal
                    (idleOf upgrader)
                    [ Verdict.Unassigned("u", IdleReason.NoneApplicable) ]
                    "the upgrader row waits at its buffer; it does not walk to the rock"

                Expect.equal
                    (idleOf (lightWorker "w" 0 450))
                    [ Verdict.Matched("w", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate) ]
                    "and the generalist beside it digs"
            }

            // ADR 0021 sizes a Post's Anchor to drain its rock whole, so a
            // manned Post ordinarily leaves a light body nothing to earn for
            // the walk — and the Seat it takes is one the garrison's own
            // Total cap counts (ADR 0051), which is how the mother's workers
            // came to evict a remote Anchor off its Post. Pairwise on the
            // garrison alone: the container stands in both halves, so what
            // moves is a body on the Post and nothing else.
            test "a light body is refused a source its garrison already drains" {
                let matchedWith bodies =
                    let { Assignments = assignments } =
                        decide
                            (sourceColony
                                postedSourceRoom
                                ((lightWorker "w" 0 450, { X = 13; Y = 10 }) :: bodies))
                            Map.empty
                            Set.empty
                            None

                    Map.tryFind "w" assignments

                Expect.equal
                    (matchedWith [])
                    (Some(taskId (Harvest "src-a")))
                    "a vacant Post is the safety valve: nobody is digging it, so the walk earns"

                Expect.isNone
                    (matchedWith [ creepWith "a1" 0 50 sixWork, { X = 11; Y = 10 } ])
                    "six Work take twelve a tick off a rock paying ten: there is no spare seat"
            }

            // And the clause is arithmetic and not "a Post with a body on
            // it": a colony too poor to cast a saturating Anchor is exactly
            // the one that cannot afford to leave the rest of the rock
            // standing. Pairwise on the garrison's Work alone.
            test "a light body still digs a rock its garrison only half drains" {
                let matchedBeside garrison =
                    let { Assignments = assignments } =
                        decide
                            (sourceColony
                                postedSourceRoom
                                [
                                    lightWorker "w" 0 450, { X = 13; Y = 10 }
                                    creepWith "a1" 0 50 garrison, { X = 11; Y = 10 }
                                ])
                            Map.empty
                            Set.empty
                            None

                    Map.tryFind "w" assignments

                Expect.equal
                    (matchedBeside threeWork)
                    (Some(taskId (Harvest "src-a")))
                    "three Work take six of the ten the held rock pays; four a tick are spare"

                Expect.isNone (matchedBeside sixWork) "six Work take all ten and more"
            }

            // The store mirror prices a **walk**, and Harvest is the one
            // intake that does not finish in the tick the body arrives — so
            // the same clause read on a holder (`applicable` is the release
            // gate too) walked a body off the Seat it was digging on the tick
            // its store crossed half, carrying half a load home for a walk it
            // had already paid whole. Pairwise on the tile alone: the same
            // body, the same store, the same held Harvest, standing on a Seat
            // in one half and four tiles off it in the other.
            test "a light body past half full digs on where it stands, and is not sent for more" {
                let verdictsAt pos =
                    let colony = sourceColony loneSourceRoom [ lightWorker "w" 226 224, pos ]

                    let { Verdicts = verdicts } =
                        decide colony (Map.ofList [ "w", taskId (Harvest "src-a") ]) Set.empty None

                    verdicts

                Expect.equal
                    (verdictsAt { X = 11; Y = 10 })
                    [ Verdict.Kept("w", taskId (Harvest "src-a")) ]
                    "on the Seat there is no walk left to price, and the dig runs to the brim"

                Expect.equal
                    (verdictsAt { X = 13; Y = 10 })
                    [
                        Verdict.Released("w", taskId (Harvest "src-a"), ReleaseReason.Inapplicable)
                        Verdict.Unassigned("w", IdleReason.NoneApplicable)
                    ]
                    "four tiles off it the walk is still ahead, and half a store is not worth it"
            }

            // A container **site** is a garrison place and not yet an economy
            // (ADR 0042 as #205 amended it): the Anchor raising one spends the
            // rock into construction progress, and there is no container
            // standing beside it to Withdraw from either. Closing the rock
            // there would leave the light row no Feeding intake at all for the
            // several hundred ticks the site stands. Pairwise on the
            // container's *state* alone — same tile, same garrison, same body.
            test "a garrison raising a container site does not close its rock" {
                let matchedOn room =
                    let { Assignments = assignments } =
                        decide
                            (sourceColony
                                room
                                [
                                    lightWorker "w" 0 450, { X = 13; Y = 10 }
                                    creepWith "a1" 0 50 sixWork, { X = 11; Y = 10 }
                                ])
                            Map.empty
                            Set.empty
                            None

                    Map.tryFind "w" assignments

                Expect.equal
                    (matchedOn siteSourceRoom)
                    (Some(taskId (Harvest "src-a")))
                    "nothing the site's Anchor digs reaches a store, and there is none to draw"

                Expect.isNone
                    (matchedOn postedSourceRoom)
                    "the clause starts biting the tick the container stands"
            }
        ]

/// Spatial projection of a plain corridor x = 10, y = 9..21 with a source
/// at each end (source tiles are walls): "src-far" at (10, 10), "src-near"
/// at (10, 20).
let nearFarCorridor creepPositions =
    spatial
        [ "src-far", { X = 10; Y = 10 }; "src-near", { X = 10; Y = 20 } ]
        [
            for y in 9..21 -> { X = 10; Y = y }, (if y = 10 || y = 20 then Wall else Plain)
        ]
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList creepPositions
        })

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
                    let { Assignments = assignments } =
                        decide (snapshotWith sources) Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 15 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                        ConstructionSites = [ { Id = "site-1" } ]
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

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Refill "spawn-1")))
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
                let { Assignments = assignments } = decide snapshot sticky Set.empty None

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

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [
                                                "mule", { X = 10; Y = 13 }
                                                "runner", { X = 11; Y = 13 }
                                            ]
                                })
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None

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
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 11 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty (moveIntents intents) "no path: standing still beats oscillating"
                Expect.isEmpty (actionIntents intents) "and the target is out of range"
            }

            test "a builder works from range 3 without closing in" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                        Spatial =

                            spatial
                                [ "site-1", { X = 10; Y = 10 } ]
                                [ for y in 10..13 -> { X = 10; Y = y }, Plain ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 13 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

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

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "w1", Top ]
                    "transfer needs range 1, so the creep closes in"

                Expect.isEmpty (actionIntents intents) "no transfer from range 2"
            }
        ]

/// The Resolver's movement Verdicts at the same seam, with the named
/// creeps on the verbose list (ADR 0018).
let resolveVerdictsVerboseOn snapshot assigned verbose =
    resolve
        snapshot
        (Atlas.ofView snapshot)
        noThreats
        (poolOn snapshot)
        (Map.ofList assigned)
        Map.empty
        (Set.ofList verbose)
    |> snd

/// The same for a quiet colony: nobody on the verbose list.
let resolveVerdictsOn snapshot assigned =
    resolveVerdictsVerboseOn snapshot assigned []

/// Run the Emitter at its own seam, over the same tick-start Atlas.
let emitOn snapshot assigned =
    emit snapshot (Atlas.ofView snapshot) noThreats (Map.ofList assigned)

/// Two single-Seat sources at the ends of a two-tile corridor; each creep
/// stands on the other's Seat.
let headOnSwap =
    let terrain =
        [
            { X = 10; Y = 10 }, Wall
            { X = 10; Y = 11 }, Plain
            { X = 10; Y = 12 }, Plain
            { X = 10; Y = 13 }, Wall
        ]

    { bareRespawn with
        Sources = [ source "src-a"; source "src-b" ]
        Creeps = [ worker "wa" 0 50; worker "wb" 0 50 ]
        Spatial =

            spatial [ "src-a", { X = 10; Y = 10 }; "src-b", { X = 10; Y = 13 } ] terrain
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        Map.ofList [ "wa", { X = 10; Y = 12 }; "wb", { X = 10; Y = 11 } ]
                })
    }

/// The lane with an east-bound body of ours on (11,12) and whatever holds
/// (12,12) in front of it — a body of ours, or a body of another colony's,
/// which is the pair #220 turns on.
let laneWith pocket ours foreign =
    { bareRespawn with
        Sources = [ source "src-w"; source "src-e" ]
        Controller = None
        Creeps = worker "eb" 0 50 :: ours
        Spatial =
            lane pocket
            |> withHome (fun layer ->
                { layer with
                    CreepPositions =
                        Map.ofList (
                            ("eb", { X = 11; Y = 12 })
                            :: (ours |> List.map (fun c -> c.Name, { X = 12; Y = 12 }))
                        )
                })
        Foreign =
            if foreign then
                Set.singleton (RoomPos.at "W1N1" { X = 12; Y = 12 })
            else
                Set.empty
    }

/// W13S28's north Upgrade pocket (#241), the geometry the jam is made of,
/// narrowed to six tiles and written either way round. The controller stands
/// at (24,17) with the whole row y = 16 walled, so the only ground inside its
/// Upgrade Work Area is the pocket north of it — (21..23,14) and (21..23,15) —
/// reached down one corridor along y = 14. The live pocket is the eight tiles
/// the ticket lists, (21..25,14) and (21..23,15); the two east tiles are left
/// out here so the corridor is ordinary ground and the pocket's mouth is one
/// tile.
///
/// `mirror` is which way that corridor runs, and it is not decoration: every
/// tie in this bot falls to the lowest x then y, so the two orientations put
/// the working ground on opposite sides of the order `arbitrate` re-houses a
/// displaced body in. One of them can be right by accident, which is why both
/// are pinned below.
///
/// The [[buffer]] container stands at (22,15), *inside* the pocket, so its
/// five standing tiles are Upgrade [[working ground]] to the last one: nowhere
/// here is a tile a body can park on without taking it from the row that works
/// there or from the hauler that feeds them. The buffer is empty, which is
/// what leaves the upgraders with no Task at all.
let private pocketFacing (mirror: int -> int) =
    let controller = { X = mirror 24; Y = 17 }
    let buffer = { X = mirror 22; Y = 15 }

    { spatial
          []
          ([ for x in 16..23 -> { X = mirror x; Y = 14 }, Plain ]
           @ [ for x in 21..23 -> { X = mirror x; Y = 15 }, Plain ]
           @ [ controller, Wall ]) with
        Stores = Map.ofList [ "can-buf", 0 ]
    }
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.singleton controller
        })
    |> withTargets
        [
            "ctrl-1", controller, Controller
            "can-buf", buffer, Structure BuiltKind.Container
        ]

/// The live room's orientation: the corridor runs west out of the mouth.
let pocketRoom = pocketFacing id

/// And the same pocket reflected about x = 20 — mouth at (20,14), corridor
/// running east.
let mirroredPocketRoom = pocketFacing (fun x -> 40 - x)

/// The pocket colony: the bodies the test puts in it, standing where it puts
/// them. No source, so the only work in the room is the controller's and its
/// buffer's.
let pocketColonyIn room creeps positions =
    { bareRespawn with
        Sources = []
        Creeps = creeps
        Spatial =
            room
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList positions
                })
    }

let pocketColony creeps positions =
    pocketColonyIn pocketRoom creeps positions

/// The pairs that exchanged tiles between two ticks. A swap is a legal answer
/// to a head-on meeting; a swap repeated is the livelock #241 forbids, and
/// what `livelock-scan` counted 54 of in 190 ticks.
let private swaps (before: Map<string, Pos>) (after: Map<string, Pos>) =
    Set.ofList
        [
            for KeyValue(a, _) in before do
                for KeyValue(b, _) in before do
                    if a < b && before[a] = after[b] && before[b] = after[a] then
                        a, b
        ]

/// The pairs that exchanged tiles on two consecutive tick boundaries of a run.
let repeatedSwaps (ticks: Map<string, Pos> list) =
    ticks
    |> List.pairwise
    |> List.map (fun (before, after) -> swaps before after)
    |> List.pairwise
    |> List.collect (fun (first, second) -> Set.intersect first second |> Set.toList)

/// The positions a colony's bodies settle into, tick by tick, each tick's move
/// Intents folded onto the tick before it.
let walkedTicks colony assigned count (start: Map<string, Pos>) =
    let step (positions: Map<string, Pos>) =
        (positions, resolveOn (colony (Map.toList positions)) assigned |> moveIntents)
        ||> List.fold (fun acc (name, direction) -> Map.add name (stepFrom acc[name] direction) acc)

    List.scan (fun positions _ -> step positions) start [ 1..count ]

/// An upgrader-shaped body: the row that stands beside the buffer, and the one
/// idling in the pocket in #241.
let upgrader name =
    creepWith name 0 50 [ Work; Work; Carry; Move ]

/// The [[storage]] tucked against a wall (#268): the stock at (10,10) with
/// wall on every side but two — (10,11) and (11,11) — and a corridor running
/// east from them along y = 11. Those two tiles are the whole of the ground
/// its [[refill]] can be made from, and they are outside ADR 0022's [[working
/// ground]] to the last one: the room holds no source and no controller, so
/// #241's set is empty here and whatever vacates them is the mover's own rule.
/// The stock's own tile is an obstacle, exactly as the engine has it, so it is
/// no third standing tile.
let private wallStorageRoom =
    { spatial
          []
          ([ { X = 10; Y = 10 }, Plain; { X = 10; Y = 11 }, Plain ]
           @ [ for x in 11..18 -> { X = x; Y = 11 }, Plain ]) with
        Stores = Map.ofList [ "sto-1", 0 ]
    }
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.singleton { X = 10; Y = 10 }
        })
    |> withTargets [ "sto-1", { X = 10; Y = 10 }, Structure BuiltKind.Storage ]

/// The colony standing on it: no source, no controller and no placed spawn, so
/// the only Task the room offers is the stock's own Refill.
let wallStorageColony creeps positions =
    { bareRespawn with
        Sources = []
        Controller = None
        Creeps = creeps
        Spatial =
            wallStorageRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList positions
                })
    }

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [
                                                "har", { X = 10; Y = 12 }
                                                "upg", { X = 10; Y = 11 }
                                            ]
                                })
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
                    decide headOnSwap sticky Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 10; Y = 13 }; "w2", { X = 10; Y = 12 } ]
                                })
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
                // so the gap is the harvester's — and since #219 the loser
                // is not walled in by losing: its tail holds the tile beside
                // the gap, which is the one the harvester just left, so it
                // follows up the corridor it was heading along anyway.
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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "h", { X = 10; Y = 13 }; "u", { X = 11; Y = 13 } ]
                                })
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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "h1", { X = 10; Y = 11 }; "h2", { X = 9; Y = 12 } ]
                                })
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
                let terrain = [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "har" 0 50; worker "bob" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                                })
                    }

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
                let terrain = [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ { worker "har" 0 50 with Fatigue = 4 }; worker "bob" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                                })
                    }

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
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

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
                let terrain =
                    [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]
                    @ [ for x in 8..15 -> { X = x; Y = 13 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "har" 0 50; worker "bob" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                                })
                    }

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
                // leaves the area — which is the tail every stayer now
                // carries (#219), where before R2b the only tile it could be
                // offered was the one its displacer vacated. It upgrades
                // this tick either way: the Emitter judges from tick-start
                // geometry.
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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 9; Y = 12 }; "upg", { X = 10; Y = 12 } ]
                                })
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
                // #219, at the seam: eight creeps stood in W13S28's north
                // corridor for ten minutes because a traveller's only
                // candidate was its step, and the body on it was fatigued
                // every other tick — a swap needs both parties rested on one
                // tick, and with a queue behind each of them neither ever
                // had a free tile to back into. The pocket at (11,11) is
                // reachable and leads nowhere, so the occupancy surcharge
                // will never route through it: what takes it is the Move
                // Intent's tail.
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
                // The injectivity #216 R2b bought, at the one shape that can
                // lose it: a chain whose innermost creep wants the tile the
                // chain's own initiator is claiming. The search must not
                // hand that tile out twice — the initiator's claim is
                // already staked when the chain is walked past it, and two
                // bodies judged onto one tile is two `MoveCreep`s the engine
                // resolves by deleting one in silence, which is the failure
                // ADR 0001's Consequences say the settle was replaced to
                // stop.
                let queue positions =
                    { bareRespawn with
                        Sources = [ source "src-w"; source "src-e" ]
                        Controller = None
                        Creeps = [ worker "aa" 0 50; worker "bb" 0 50; worker "cc" 0 50 ]
                        Spatial =
                            lane false
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList positions
                                })
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
                // #145's rule, which R2b generalised and must not narrow: a
                // creep the engine put down on the border ring cannot stay
                // where it is — "stay put" there is a bounce back across
                // the border every other tick — so its tail is every ground
                // tile beside it and not only the ones beside its step. The
                // ordinary tail's narrowing buys "no way back down the lane
                // it came up", and a ring creep has no lane behind it.
                //
                // Here the step is (9,1) and the ground beside the ring
                // creep runs (9,1) (10,1) (11,1): (11,1) is two tiles from
                // the step and would be dropped by that narrowing. With the
                // step and (10,1) both held by bodies fatigue keeps out of
                // the pass, (11,1) is the only tile there is.
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
                                        Map.ofList
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
                // #219's own acceptance test, driven the way it is written:
                // two bodies meeting head-on in a lane, tired on opposite
                // ticks so the tick both are rested and could swap never
                // comes, walked until they arrive or the run gives up. One
                // tick cannot show it — the deadlock is that every tick's
                // answer is the same one.
                //
                // The geometry is #219's own and not the phrase's: the live
                // corridor had (17,1) free beside it the whole ten minutes
                // and nobody took it. So the pocket lane is the case, and
                // the strictly one-wide lane is the negative — with nothing
                // to sidestep to the answer is still "wait", which is ADR
                // 0008's and is where a single-candidate rule and this one
                // agree.
                //
                // What clears it is two rules and not one: the east-bound
                // body takes the pocket off its Move Intent's tail, because
                // its step is a tile fatigue has walled (#219), and the
                // west-bound one is *priced* round the body in its way,
                // because the flood charges an occupied tile the occupancy
                // surcharge (ADR 0008, #220). A one-tile pocket beside a
                // one-wide lane is always both — every tile beside such a
                // lane is diagonal to the tiles either side of it — so this
                // test pins the pair's outcome and the test above pins the
                // tail on its own.
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
                // #241, pairwise and one tile apart: the same idle body on
                // the first tile of the Upgrade Work Area and on the corridor
                // tile beside it. Inside, it is standing on ground somebody
                // works from and steps off; outside, it is standing nowhere in
                // particular and the rule has nothing to say to it.
                let moved pos =
                    resolveOn (pocketColony [ upgrader "u" ] [ "u", pos ]) [] |> moveIntents

                Expect.equal
                    (moved { X = 21; Y = 14 })
                    [ "u", Left ]
                    "on the pocket's mouth it steps out into the corridor"

                Expect.isEmpty
                    (moved { X = 20; Y = 14 })
                    "one tile west, off every Work Area, it has no reason to move at all"

                // And the third case, the one the rule promises costs
                // nothing: seal the mouth and the pocket's working ground has
                // no ground off it at all, so there is nowhere to head and the
                // body parks exactly as it did before #241.
                let sealedPocket =
                    pocketRoom
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.remove { X = 20; Y = 14 } layer.Terrain
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
                // The live jam (W13S28, 190 ticks): the buffer empty, two
                // upgraders with no Task parked on two of its five standing
                // tiles, two loaded workers walking in to upgrade, and the
                // hauler carrying the energy that would have un-idled the
                // upgraders never getting past the pocket's mouth. The ring is
                // that the bodies waiting on the buffer were standing where its
                // feed had to stand.
                //
                // Driven over ticks, because one tick cannot show it: every
                // tick's answer was the same one, and the two bodies fighting
                // over the mouth traded it back and forth for as long as the
                // scan ran.
                let creeps =
                    [
                        upgrader "u1"
                        upgrader "u2"
                        worker "w1" 50 0
                        worker "w2" 50 0
                        hauler "h" 100 0
                    ]

                let assigned =
                    [ "w1", Upgrade "ctrl-1"; "w2", Upgrade "ctrl-1"; "h", Refill "can-buf" ]

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
                // The orientation the room happens not to have (#241): the
                // corridor running east, so the working ground sorts *below*
                // the tile off it. `arbitrate` re-houses a displaced body on
                // the first free tile of its list, so with an unordered tail
                // the body shoved off the mouth lands back inside the pocket,
                // steps out again next tick, and the pair exchanges the mouth
                // for as long as the run lasts — the very swap the criterion
                // above forbids, arrived at from the other side.
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
                // The other half of the W13S28 jam (#241 left it, #267 fixes
                // it): the pocket's six tiles are the whole of the Upgrade
                // Work Area, five upgraders and the hauler feeding the buffer
                // stand on them, and a sixth upgrader walks up the corridor.
                // Its step is the mouth, and before this ticket the body
                // holding the mouth could be pushed *out* of the pocket for
                // nothing — so the two traded (20,14) and (21,14) every tick
                // and the body on the mouth upgraded every other one.
                let creeps = [ for n in 1..6 -> worker $"w%d{n}" 50 0 ] @ [ hauler "h" 100 0 ]

                let assigned =
                    [ for n in 1..6 -> $"w%d{n}", Upgrade "ctrl-1" ] @ [ "h", Refill "can-buf" ]

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
                // The pairwise half. The same pocket with (23,15) empty: the
                // displaced body has somewhere in its own Work Area to go, so
                // the chain costs the arbitration nothing and still happens —
                // ADR 0001's essential rule, which prices the eviction and
                // never the shuffle.
                let creeps = [ for n in 1..5 -> worker $"w%d{n}" 50 0 ] @ [ hauler "h" 100 0 ]

                let assigned =
                    [ for n in 1..5 -> $"w%d{n}", Upgrade "ctrl-1" ] @ [ "h", Refill "can-buf" ]

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
                // #241's pocket, arrived at from the half of the geometry ADR
                // 0022's set never covered. The stock has two standing tiles
                // and no source's Seat or Upgrade tile among them, so a pair
                // of idle bodies parked on them shut the hauler holding
                // `Refill sto-1` out of the room's only Task exactly as the
                // upgraders shut the buffer's feed out of W13S28 — and #241's
                // rule, reading the working ground alone, had nothing to say
                // to either of them.
                let creeps = [ worker "u1" 0 50; worker "u2" 0 50; hauler "h" 100 0 ]

                let ticks =
                    walkedTicks
                        (wallStorageColony creeps)
                        [ "h", Refill "sto-1" ]
                        8
                        (Map.ofList
                            [
                                "u1", { X = 10; Y = 11 }
                                "u2", { X = 11; Y = 11 }
                                "h", { X = 14; Y = 11 }
                            ])

                let besideStorage (positions: Map<string, Pos>) =
                    range positions["h"] { X = 10; Y = 10 } <= 1

                // The ticket's own criterion, and the half the arbitration
                // already answered: an idle body is the lightest push there
                // is, so the hauler's chain shoves the one on (11,11) aside
                // and takes the tile whatever rule the mover reads.
                Expect.isTrue
                    (ticks |> List.skip 3 |> List.forall besideStorage)
                    "the hauler is standing beside the stock by the third tick and stays there"

                // The half the arbitration cannot reach on its own, and the
                // whole of what this ticket adds: (10,11) is a dead end, so
                // the body parked on it is in nobody's *head* candidate once
                // the hauler holds the other tile, is never displaced, and
                // keeps the stock's second standing tile for the rest of its
                // life. It leaves because the rule tells it to, not because
                // somebody pushed it.
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
                // The half of #267 the full pocket cannot show, and the case
                // its title names: the Work Area has free tiles, just none
                // beside the body on the mouth. Four upgraders inside it with
                // (23,14) and (23,15) empty, and a fifth walking in on the
                // mouth.
                // Pricing the eviction is not enough on its own — a body an
                // earlier chain has already shuffled aside inside its area
                // re-initiated and was paid its rank's whole weight for being
                // put back on the tile it never chose to leave, and three of
                // those phantom gains in one chain bought the eviction this
                // ticket forbids. The answer is the row stepping up: the
                // traveller takes the mouth, every body inside moves one tile
                // deeper into the pocket, and the free tile at the back is
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
                // The same five bodies in the orientation the room happens not
                // to have, for the reason #241 pins its own pair twice: ties
                // fall to the lowest x then y, so one orientation can be right
                // by accident. Here the free tiles at the back of the pocket
                // sort *below* the mouth instead of above it.
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
                // #220: the mother's pioneer stood on (19,2) for fifteen
                // minutes with fatigue 0, its first step the Post tile the
                // child's Anchor was garrisoning — a tile her layer did not
                // carry, so the flood charged it nothing and the arbitration
                // handed it over every tick for a body the engine would
                // never let her into.
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
                // The other half of #220, and the half no arbitration can do
                // (ADR 0008): the walking flood charges a foreign body's
                // tile like any other occupied tile, so a traveller is
                // *priced* around a garrison it can never displace. The way
                // round here is one swamp tile at (12,11) — dearer than the
                // lane by eight units and cheaper than the lane plus the
                // ten-unit surcharge, so the pricing and nothing else
                // decides which step is taken.
                let bypass =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain =
                                Map.ofList (
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
                // The Verdict gap #219 recorded: a kept traveller that
                // simply fails to move emitted nothing at all, and the
                // timeline went quiet exactly where it was worth reading.
                // Which Verdict it is turns on whether the pass can name the
                // holder: one of ours on the tile is a counterpart, a
                // foreign body is not.
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
                // #216 R2b: the mother and the child both work the child's
                // room, and each `decide` used to arbitrate its own half of
                // it with the other half read as empty. Folded into one pass
                // the two bodies are ordinary occupants of one room, each
                // moving on the intent its own colony registered — so the
                // head-on pair swaps, which neither colony could have
                // settled alone.
                let mother = laneWith false [] true

                let child =
                    { bareRespawn with
                        Sources = [ source "src-w"; source "src-e" ]
                        Controller = None
                        Creeps = [ worker "an" 0 50 ]
                        Spatial =
                            lane false
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "an", { X = 12; Y = 12 } ]
                                })
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

[<Tests>]
let resolverVerdictTests =
    testList
        "resolver verdicts"
        [
            test "a grounded creep gets a grounded Verdict; the creep behind it yields to it" {
                // The one-lane corridor with a fatigued seated harvester: har
                // sits arbitration out with its tile blocked, and bob — whose
                // only path runs through that tile — stands down for the tick.
                let terrain = [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ { worker "har" 0 50 with Fatigue = 4 }; worker "bob" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                                })
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "har", Harvest "src-a"; "bob", Build "site-1" ])
                    [ Verdict.Grounded "har"; Verdict.Yielded("bob", "har") ]
                    "har is grounded; bob's blocked step names the tired creep holding the tile"
            }

            test "a lone fatigued traveller is grounded, nothing more" {
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "w1", Harvest "src-a" ])
                    [ Verdict.Grounded "w1" ]
                    "grounding is the whole story: no move was asked, none was denied"
            }

            test "a displaced squatter's Verdict names its displacer" {
                // The squatting regression's geometry: the upgrader on the
                // sole Seat is displaced by the inbound harvester.
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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [
                                                "har", { X = 10; Y = 12 }
                                                "upg", { X = 10; Y = 11 }
                                            ]
                                })
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "har", Harvest "src-a"; "upg", Upgrade "ctrl-1" ])
                    [ Verdict.Yielded("upg", "har") ]
                    "the displaced upgrader yields to the harvester; the harvester says nothing"
            }

            test "losing a contested tile to a higher rank is a yield naming the winner" {
                // The contested-gap geometry: Harvest outranks Upgrade, so
                // the upgrader waits in place while the harvester takes the
                // gap it also wanted.
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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "h", { X = 10; Y = 13 }; "u", { X = 11; Y = 13 } ]
                                })
                    }

                Expect.equal
                    (resolveVerdictsOn snapshot [ "h", Harvest "src-a"; "u", Upgrade "ctrl-1" ])
                    [ Verdict.Yielded("u", "h") ]
                    "the outranked upgrader's wait is attributed to the harvester"
            }

            test "the reroute Verdict is manufactured only for a creep on the verbose list" {
                // The two-lane corridor: the builder's straight path runs
                // through the seated harvester's tile, and the surcharge
                // sends it into the parallel lane instead. Nobody yields —
                // the detour is a pricing event, not an arbitration one.
                let terrain =
                    [ for x in 8..15 -> { X = x; Y = 12 }, Plain ]
                    @ [ for x in 8..15 -> { X = x; Y = 13 }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "har" 0 50; worker "bob" 50 0 ]
                        Spatial =

                            spatial
                                [ "src-a", { X = 10; Y = 11 }; "site-1", { X = 15; Y = 12 } ]
                                terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "har", { X = 10; Y = 12 }; "bob", { X = 9; Y = 12 } ]
                                })
                    }

                let assigned = [ "har", Harvest "src-a"; "bob", Build "site-1" ]

                Expect.isEmpty
                    (resolveVerdictsOn snapshot assigned)
                    "a quiet colony pays for no second flood, so it records no reroute"

                Expect.isEmpty
                    (resolveVerdictsVerboseOn snapshot assigned [ "har" ])
                    "the list is read per creep: the detourer is not the one being watched"

                Expect.equal
                    (resolveVerdictsVerboseOn snapshot assigned [ "bob" ])
                    [ Verdict.Rerouted "bob" ]
                    "the lane sidestep is attributed to traffic; the seated harvester says nothing"
            }

            test "a creep simply stepping toward its Work Area produces no movement noise" {
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                Expect.isEmpty
                    (resolveVerdictsOn snapshot [ "w1", Harvest "src-a" ])
                    "conclusion level means events, not every step"
            }

            test "a clean head-on swap is silent: both creeps settle where they asked" {
                Expect.isEmpty
                    (resolveVerdictsOn headOnSwap [ "wa", Harvest "src-a"; "wb", Harvest "src-b" ])
                    "each traveller got exactly its preferred tile; nothing became of either move"
            }

            test "movement Verdicts ride behind the Matcher's in decide's output" {
                // A fatigued lone traveller at the decide seam: the Matcher
                // speaks first (the fresh match), the Resolver after (the
                // grounding) — one additive list, interleaved downstream.
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ { worker "w1" 0 50 with Fatigue = 4 } ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate)
                        Verdict.Grounded "w1"
                    ]
                    "matcher verdicts first, then the Resolver's, in one list"
            }
        ]

[<Tests>]
let sayTests =
    testList
        "chat bubbles"
        [
            test "an assigned harvester says the Harvest glyph" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.contains
                    intents
                    (SayCreep("w1", "⛏"))
                    "the bubble shows the creep's current Task"
            }

            test "each Task has its own glyph: Refill, Build, Upgrade" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0; worker "w2" 50 0; worker "w3" 50 0 ]
                    }

                let sticky =
                    Map.ofList
                        [
                            "w1", (taskId (Refill "spawn-1"))
                            "w2", (taskId (Build "site-1"))
                            "w3", (taskId (Upgrade "ctrl-1"))
                        ]

                let { Intents = intents } = decide snapshot sticky Set.empty None

                Expect.equal
                    (sayIntents intents)
                    [ "w1", "🔋"; "w2", "🔨"; "w3", "⚡" ]
                    "one bubble per assigned creep, glyph matched to its Task"
            }

            test "an unassigned creep says nothing" {
                // Nothing applicable for a full creep: no refill need, no
                // sites, no controller.
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (sayIntents intents) "no Task, no bubble"
            }

            test "a creep still walking toward its target says its glyph anyway" {
                // Out of action range: no action Intent this tick, but the
                // assignment holds — the bubble reports it every tick.
                let corridor =
                    [ for y in 9..15 -> { X = 10; Y = y }, Plain ] @ [ { X = 10; Y = 10 }, Wall ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty (actionIntents intents) "out of range: no action Intent yet"
                Expect.equal (sayIntents intents) [ "w1", "⛏" ] "the bubble still shows the Task"
            }
        ]

[<Tests>]
let verdictTests =
    testList
        "matcher verdicts"
        [
            test "a lone applicable Task wins as the only candidate" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate) ]
                    "one creep, one candidate: the Verdict names the Task and the walkover"
            }

            test "rank decides: Refill outbids Upgrade for a loaded creep" {
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the feeding tier beat the surplus tier: rank decided"
            }

            test "rank layers by target: feeding the spawn outbids feeding the tower" {
                // The tower sits first in the pool, so only the target-layered
                // rank (ADR 0010) — not pool order — can hand the spawn the win.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables =
                            [
                                refillable "tower-1" 500 BuiltKind.Tower
                                refillable "spawn-1" 50 BuiltKind.Spawn
                            ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds its own reproduction before its guns: rank decided"
            }

            test "travel cost decides: the near source wins the rank tie" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Matched("w1", taskId (Harvest "src-near"), MatchFactor.TravelCost) ]
                    "same rank, cheaper path: travel cost decided"
            }

            test "load decides: the second creep spreads to the emptier source" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.PoolOrder)
                        Verdict.Matched("w2", taskId (Harvest "src-b"), MatchFactor.Load)
                    ]
                    "w1's tie fell to pool order; w2 avoided the loaded source"
            }

            test "a remembered assignment kept is distinguishable from a fresh match" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-far"; source "src-near" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = nearFarCorridor [ "w1", { X = 10; Y = 17 } ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-far") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Kept("w1", taskId (Harvest "src-far")) ]
                    "anti-thrash speaks as Kept, never as a fresh Matched"
            }

            test "a Task that left the pool releases with TaskGone" {
                // The remembered Refill target has no free capacity this
                // tick, so the Planner never generates the Task.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Refill "spawn-1") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Refill "spawn-1"), ReleaseReason.TaskGone))
                    "the release names the vanished Task"
            }

            test "a Task that left a pool we can see releases; the vision grace is about looking" {
                // #151's line, drawn from the other side. The grace holds an
                // assignment whose target left the pool **with the vision
                // that carried it**, and the sighting a room stamps while we
                // are looking at it must not be mistaken for that: a Refill
                // that filled, a store that emptied, a pile that decayed all
                // leave the pool while their target still stands in the
                // room's census, in full view. Holding those for 150 ticks
                // would stall the haul cycle every time an extension filled.
                // So what separates the two is the sighting's own tick, and
                // this is the pair that pins it — one field of one sighting
                // moves and nothing else.
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let held = taskId (Refill "spawn-1")
                let sticky = Map.ofList [ "w1", held ]

                // The home room's own name under `SpatialInfo.empty`, which
                // is the room every fixture here files its facts under.
                let seenAt tick =
                    { snapshot with
                        Sightings =
                            Map.ofList
                                [
                                    "",
                                    {
                                        Tick = tick
                                        Targets = Set.singleton "spawn-1"
                                    }
                                ]
                    }

                Expect.contains
                    (decide (seenAt snapshot.Time) sticky Set.empty None).Verdicts
                    (Verdict.Released("w1", held, ReleaseReason.TaskGone))
                    "seen this tick, the target stands and the Task is gone all the same: released, as it always was"

                Expect.contains
                    (decide (seenAt (snapshot.Time - 1)) sticky Set.empty None).Verdicts
                    (Verdict.Kept("w1", held))
                    "and one tick of blindness later, the same disappearance is a room we cannot see and the holder is kept"
            }

            test "a drained source releases its harvester with TooEarly" {
                // Issue #48: anti-thrash must not pin a creep to a dry
                // rock. The Task stays pooled since ADR 0025, so the
                // release is the arrival gate's rather than TaskGone's, and
                // Inapplicable would make the transition log lie. No
                // projection here, so the walk prices at 0 the way ADR 0004
                // prices unplaced geometry — and the reason says so, beside
                // the wait it was compared against (#88); the same release
                // on real ground is pinned under "restock dispatch".
                let snapshot =
                    { bareRespawn with
                        Sources = [ drained "src-a" 120 ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w1",
                        taskId (Harvest "src-a"),
                        ReleaseReason.TooEarly(0, 120)
                    ))
                    "an arrival that covers no wait leaves the rock, exactly as ADR 0013 did"
            }

            test "a creep that fills up releases Harvest as Inapplicable and matches fresh" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Inapplicable)
                        Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank)
                    ]
                    "the handover carries both halves: why released, what won next"
            }

            test "a body that cannot do its remembered Task releases as Inapplicable" {
                // Part-based, not energy-state: the hauler has room to
                // harvest into but no Work part to harvest with.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ creepWith "hauler" 0 50 [ Carry; Move ] ]
                    }

                let sticky = Map.ofList [ "hauler", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "hauler",
                        taskId (Harvest "src-a"),
                        ReleaseReason.Inapplicable
                    ))
                    "the missing Work part releases the assignment as Inapplicable"
            }

            test "a walled-off Work Area releases with Unreachable" {
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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released("w1", taskId (Harvest "src-a"), ReleaseReason.Unreachable))
                    "no Seat can be reached: the release says so"
            }

            test "a remembered oversell releases with OverCapacity, the loser idles as NoneFree" {
                // One Seat at the source, two creeps remembered on it — an
                // oversell memory can carry across a redeploy. The
                // alphabetically first keeps; nothing else fits the loser.
                let corridor =
                    [ { X = 10; Y = 10 }, Wall ] @ [ for y in 11..14 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 10; Y = 12 }; "w2", { X = 10; Y = 13 } ]
                                })
                    }

                let sticky =
                    Map.ofList [ "w1", taskId (Harvest "src-a"); "w2", taskId (Harvest "src-a") ]

                let { Verdicts = verdicts } = decide snapshot sticky Set.empty None

                Expect.equal
                    verdicts
                    [
                        Verdict.Released("w2", taskId (Harvest "src-a"), ReleaseReason.OverCapacity)
                        Verdict.Kept("w1", taskId (Harvest "src-a"))
                        Verdict.Unassigned("w2", IdleReason.NoneFree)
                    ]
                    "the cap releases the oversell and explains the loser's idleness"
            }

            test "an empty pool idles a creep with NoTasks" {
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoTasks) ]
                    "the Planner generated nothing at all"
            }

            test "a full creep with only Harvest on offer idles as NoneApplicable" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoneApplicable) ]
                    "no Task fit the creep's body or energy state"
            }

            test "an applicable Task with an unreachable Work Area idles as NoneReachable" {
                // The source's one Seat is walled off; nothing else exists.
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    verdicts
                    [ Verdict.Unassigned("w1", IdleReason.NoneReachable) ]
                    "the Task fit and had room, but no path reaches its Work Area"
            }

            test "a dead creep's dropped assignment speaks no Verdict" {
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = []
                    }

                let sticky = Map.ofList [ "ghost", taskId (Harvest "src-a") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide snapshot sticky Set.empty None

                Expect.isEmpty (Map.toList assignments) "the dead creep's assignment is dropped"
                Expect.isEmpty verdicts "Verdicts attribute to living creeps only"
            }
        ]

/// The tier colony with the given hunger: one loaded Carry-only body
/// standing on the buffer, so the deepest tier costs it nothing to reach
/// and every shallower one costs more. Whatever wins, wins against travel
/// cost, and only rank can do that.
let tierColony refillables =
    { bareRespawn with
        Sources = []
        Refillables = refillables
        Creeps = [ creepWith "h1" 100 0 [ Carry; Carry; Move ] ]
        Spatial =
            tierRoom
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.ofList [ "h1", { X = 18; Y = 10 } ]
                })
    }

/// The surplus fixture: one loaded generalist and a hungry tower, in a
/// colony the projection places nothing in — unpriceable geometry never
/// counts against a Task (ADR 0004), so every candidate ties on travel
/// cost and load, and rank is the only thing left that can separate a
/// pair. Each caller adds exactly one rival, so the Verdict's factor is
/// evidence about that rival alone.
let surplusColony =
    { bareRespawn with
        Sources = []
        Controller = None
        Refillables = [ refillable "tower-1" 500 BuiltKind.Tower ]
        Creeps = [ worker "w1" 50 0 ]
    }

[<Tests>]
let rankTierTests =
    testList
        "rank tiers"
        [
            test "the tier order is one sequence: feeding, then surplus, then the buffer" {
                // The Refill target layering (ADR 0010, ADR 0012) read top to
                // bottom by one body, one step at a time: the spawn six steps
                // away outbids the tower three away, and the tower outbids the
                // buffer underfoot. The buffer loses the second step, which is
                // what puts it below the surplus tier rather than beside it —
                // a tie there would hand the win to the container it stands on.
                let hungrySpawn = refillable "spawn-1" 50 BuiltKind.Spawn
                let fullSpawn = refillable "spawn-1" 0 BuiltKind.Spawn
                let hungryTower = refillable "tower-1" 500 BuiltKind.Tower

                let feeding =
                    decide (tierColony [ hungrySpawn; hungryTower ]) Map.empty Set.empty None

                Expect.equal
                    feeding.Verdicts
                    [ Verdict.Matched("h1", taskId (Refill "spawn-1"), MatchFactor.Rank) ]
                    "the colony feeds its own reproduction first: rank decided"

                let surplus =
                    decide (tierColony [ fullSpawn; hungryTower ]) Map.empty Set.empty None

                Expect.equal
                    surplus.Verdicts
                    [ Verdict.Matched("h1", taskId (Refill "tower-1"), MatchFactor.Rank) ]
                    "reproduction fed, the guns outrank the buffer: rank decided"
            }

            test "tower Refill, Repair and Upgrade are one surplus rung; Build stands above" {
                // Pairwise, because the deciding factor is read off the winner
                // and its cheapest rival alone: pool all four at once and the
                // three-way tie hides whichever one left the tier. So each
                // surplus Task meets the tower Refill by itself, and pool order
                // — not rank — has to be what breaks the ties that remain.
                //
                // Build makes none of them any more (#234): it is the tier's
                // own top rung, so it outranks the tower's Refill on a fixture
                // where nothing is priceable and rank is the only thing that
                // can separate anything. That is the one comparison the rung
                // moves which is not Build-against-Upgrade, and it moves it for
                // the generalists alone — the row that feeds a tower is Carry
                // with no Work part, and no such body is applicable to a Build.
                //
                // The rung reads the site's room (`isHomeSite`) and this
                // fixture places nothing, which is the total resolving toward
                // home exactly as `isOutpostSite`'s does: absence
                // never counts against a Task (ADR 0004). The rung's *room*
                // is pinned where a room exists to pin it, in `OutpostTests`.
                let verdictsFor colony =
                    (decide colony Map.empty Set.empty None).Verdicts

                let tied =
                    [ Verdict.Matched("w1", taskId (Refill "tower-1"), MatchFactor.PoolOrder) ]

                Expect.equal
                    (verdictsFor
                        { surplusColony with
                            ConstructionSites = [ { Id = "site-1" } ]
                        })
                    [ Verdict.Matched("w1", taskId (Build "site-1"), MatchFactor.Rank) ]
                    "Build outranks the tower Refill: rank broke it, not pool order"

                Expect.equal
                    (verdictsFor (surplusColony |> withHits "road-1" BuiltKind.Road 100 5000))
                    tied
                    "Repair ties the tower Refill: pool order broke it, not rank"

                Expect.equal
                    (verdictsFor
                        { surplusColony with
                            Controller = Some(controllerAt 1)
                        })
                    tied
                    "Upgrade ties the tower Refill: pool order broke it, not rank"
            }

            // The lane a loaded generalist really stands in (#234, live
            // t195,8xx): it fills at the [[buffer]] and is left standing in
            // the controller's Upgrade Work Area, where the Upgrade costs it
            // one step, applies to any load and never goes task-gone — so two
            // colonies holding fifty construction sites between them upgraded
            // with every worker they had. Pairwise on the pool alone: the same
            // lane, the same worker on the same tile, the site added and taken
            // away. The site is the **farther** of the two targets, so a win
            // on travel cost is not a win this case would accept.
            let siteDownTheLane sites =
                bufferLaneColony
                    [ "site-1", { X = 20; Y = 10 }, Site BuiltKind.Extension ]
                    sites
                    (creepWith "w" 100 0 (bodyFor workerPattern 300))

            test "a site down the lane outbids the controller beside the buffer" {
                Expect.equal
                    (matchOf (siteDownTheLane [ { Id = "site-1" } ]))
                    (Some(taskId (Build "site-1"), MatchFactor.Rank))
                    "three steps out against the controller's one, and the site wins on rank"

                Expect.equal
                    (matchOf (siteDownTheLane []))
                    (Some(taskId (Upgrade "ctrl-1"), MatchFactor.Rank))
                    "with nothing standing to build, the same load goes into the controller"
            }

            test "the downgrade deadline still outranks a site" {
                // The rung is one step inside the surplus tier and the
                // deadline is a whole tier above the shallowest work there is
                // (ADR 0007, `deadlineRank`), so #234 does not reach it: a
                // controller about to lose a level takes back the load the
                // site had off it a tick before.
                let expiring =
                    { siteDownTheLane [ { Id = "site-1" } ] with
                        Controller =
                            Some
                                { controllerAt 2 with
                                    TicksToDowngrade = 4000
                                }
                    }

                Expect.equal
                    (matchOf expiring)
                    (Some(taskId (Upgrade "ctrl-1"), MatchFactor.Rank))
                    "inside the deadline the controller outranks the site outright"
            }
        ]

[<Tests>]
let verboseScoringTests =
    testList
        "verbose scoring"
        [
            test "a verbose creep's Scoring covers the whole pool, scores and rejections both" {
                // Loaded and full: Harvest cannot fit the energy state, while
                // Refill and Upgrade score on the full key — no projection, so
                // every travel cost prices at 0.
                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.Inapplicable
                                )
                                Candidate.Scored(taskId (Refill "spawn-1"), 0, 0, 0)
                                // Two tiers below the flow's zero, ten rungs
                                // apiece since #216 R5: the ladder gained
                                // room for a Task to be ordered inside its
                                // own tier, and `weightOfRank` divides the
                                // rungs back out (ADR 0052 decision 6).
                                Candidate.Scored(taskId (Upgrade "ctrl-1"), 20, 0, 0)
                            ]
                        )
                        Verdict.Matched("w1", taskId (Refill "spawn-1"), MatchFactor.Rank)
                    ]
                    "every pool Task appears once: scored on the key or rejected at its gate"
            }

            test "a full Task rejects as CapacityFull; only the listed creep gets a Scoring" {
                // One Seat at the source, claimed by w1's match before w2's
                // turn: w2's scoring shows the cap, and its upgrade row shows
                // the empty carry. w1 is off the list and speaks no Scoring.
                let corridor =
                    [ { X = 10; Y = 10 }, Wall ] @ [ for y in 11..14 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ "w1", { X = 10; Y = 12 }; "w2", { X = 10; Y = 13 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w2" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched("w1", taskId (Harvest "src-a"), MatchFactor.OnlyCandidate)
                        Verdict.Scoring(
                            "w2",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.CapacityFull
                                )
                                Candidate.Rejected(
                                    taskId (Upgrade "ctrl-1"),
                                    RejectReason.Inapplicable
                                )
                            ]
                        )
                        Verdict.Unassigned("w2", IdleReason.NoneFree)
                    ]
                    "the cap that idled w2 is named per Task; the unlisted creep stays terse"
            }

            test "a kept creep's own single-Seat Task scores as held, never capacity-full" {
                // The creep's own claim is set aside for its scoring: the
                // Task it holds must read as the winning row, not as
                // rejected against its holder's own seat.
                let corridor =
                    [ { X = 10; Y = 10 }, Wall ] @ [ for y in 11..14 -> { X = 10; Y = y }, Plain ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] corridor
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 11 } ]
                                })
                    }

                let sticky = Map.ofList [ "w1", taskId (Harvest "src-a") ]
                let { Verdicts = verdicts } = decide snapshot sticky (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Scored(taskId (Harvest "src-a"), 0, 0, 0)
                                Candidate.Rejected(
                                    taskId (Upgrade "ctrl-1"),
                                    RejectReason.Inapplicable
                                )
                            ]
                        )
                        Verdict.Kept("w1", taskId (Harvest "src-a"))
                    ]
                    "the held Task is the scoring's winning row"
            }

            test "a walled-off Work Area rejects as Unreachable" {
                let terrain =
                    [
                        { X = 10; Y = 10 }, Wall
                        { X = 10; Y = 11 }, Plain
                        { X = 10; Y = 12 }, Wall
                        { X = 10; Y = 13 }, Plain
                        { X = 10; Y = 14 }, Plain
                    ]

                let snapshot =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        Controller = None
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial =

                            spatial [ "src-a", { X = 10; Y = 10 } ] terrain
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w1", { X = 10; Y = 14 } ]
                                })
                    }

                let { Verdicts = verdicts } = decide snapshot Map.empty (Set.ofList [ "w1" ]) None

                Expect.equal
                    verdicts
                    [
                        Verdict.Scoring(
                            "w1",
                            [
                                Candidate.Rejected(
                                    taskId (Harvest "src-a"),
                                    RejectReason.Unreachable
                                )
                            ]
                        )
                        Verdict.Unassigned("w1", IdleReason.NoneReachable)
                    ]
                    "the scoring pinpoints the gate the idle reason summarises"
            }
        ]

[<Tests>]
let tests =
    testList
        "decide"
        [
            test "an empty creep is matched to a Harvest task and remembered" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.contains intents (HarvestSource("w1", "src-a")) "empty creep goes harvesting"

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "assignment is remembered"
            }

            test "bare respawn yields exactly one spawn Intent" {
                let { Intents = intents } = decide bareRespawn Map.empty Set.empty None

                match spawnIntents intents with
                | [ (spawnName, body, creepName) ] ->
                    Expect.equal spawnName "Spawn1" "spawns from the only spawn"
                    Expect.isNonEmpty body "body must not be empty"
                    Expect.isNotEmpty creepName "creep needs a name"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "spawn Intent body is affordable at bare-respawn energy" {
                let { Intents = intents } = decide bareRespawn Map.empty Set.empty None

                for (_, body, _) in spawnIntents intents do
                    Expect.isLessThanOrEqual
                        (bodyCost body)
                        300
                        "body cost within bare-respawn energy"
            }

            test "no spawn Intent when energy is below a worker body cost" {
                let snapshot = { bareRespawn with Bank = bank 100 300 }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "cannot afford a worker"
            }

            test "no spawn Intent while the spawn is already spawning" {
                let snapshot =
                    { bareRespawn with
                        Spawns = [ { spawn with IsSpawning = true } ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "spawn is busy"
            }

            // Three Seats around src-a: a target of three, so one worker
            // leaves a deficit of two — enough demand for both spawns.
            let threeSeats =
                spatial
                    [ "src-a", { X = 10; Y = 10 } ]
                    [
                        { X = 9; Y = 10 }, Plain
                        { X = 11; Y = 10 }, Plain
                        { X = 10; Y = 9 }, Plain
                    ]

            test "two idle spawns in one room spend the shared bank once" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                spawn
                                { spawn with
                                    Name = "Spawn2"
                                    Id = "spawn-2"
                                }
                            ]
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = threeSeats
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (spawnName, _, _) ] ->
                    Expect.equal spawnName "Spawn1" "the first spawn in list order takes the budget"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            // One colony, one bank, whatever room a spawn record names
            // (ADR 0052 decision 1). This pinned the opposite until R2a:
            // the projection carried a bank per room and a spawn filed
            // under a second room drew a second full one — a colony with
            // two homes, which is the shape #191 split into two colonies
            // and ADR 0047 gave one `decide` each. The spawn below is the
            // same shape it was and the answer is now the one the shared
            // bank gives above: 300 buys one body, and the second spawn
            // waits.
            test "a spawn filed under another room still draws the colony's one bank" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                spawn
                                { spawn with
                                    Name = "Spawn2"
                                    Id = "spawn-2"
                                    RoomName = "W2N2"
                                }
                            ]
                        Bank = bank 300 300
                        Creeps = [ worker "w1" 0 50 ]
                        Spatial = threeSeats
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (spawnIntents intents |> List.map (fun (name, _, _) -> name))
                    [ "Spawn1" ]
                    "one bank funds one body, and the second spawn waits"
            }

            test "with zero creeps one bank funds two minimal bodies at once" {
                let snapshot =
                    { bareRespawn with
                        Spawns =
                            [
                                spawn
                                { spawn with
                                    Name = "Spawn2"
                                    Id = "spawn-2"
                                }
                            ]
                        Bank = bank 550 550
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (spawnIntents intents |> List.map (fun (name, body, _) -> name, body))
                    [ "Spawn1", [ Work; Carry; Move ]; "Spawn2", [ Work; Carry; Move ] ]
                    "the fallback debits the bank per body instead of waiting on the engine"
            }

            test "at 550 capacity the whole capacity is spent" {
                let snapshot =
                    { bareRespawn with
                        Bank = bank 550 550
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Work; Carry; Carry; Carry; Carry; Move; Move; Move ]
                        "two units plus the 150 remainder as Carry/Carry/Move"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "at 300 capacity the remainder pads the single unit" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Carry; Carry; Move; Move ]
                        "one unit plus the 100 remainder as a Carry/Move pair"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "below minimum workforce, spawning waits for full capacity" {
                let snapshot =
                    { bareRespawn with
                        Bank = bank 400 550
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                Expect.isEmpty
                    (spawnIntents intents)
                    "a living workforce can bank up to a bigger body"
            }

            test "with zero creeps a minimal body is spawned from available energy" {
                let snapshot = { bareRespawn with Bank = bank 250 550 }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None

                match spawnIntents intents with
                | [ (_, body, _) ] ->
                    Expect.equal
                        body
                        [ Work; Carry; Move ]
                        "an empty colony cannot wait for extensions it cannot refill"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "with zero creeps and unaffordable minimal body, no spawn Intent" {
                let snapshot = { bareRespawn with Bank = bank 150 550 }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "even the fallback needs its unit cost"
            }

            test "one worker is below minimum: a second is spawned" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.hasLength (spawnIntents intents) 1 "a lone worker cannot keep the loop going"
            }

            test "no spawn Intent when workforce is at minimum" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50; worker "worker-2" 0 50 ]
                    }

                let { Intents = intents } = decide snapshot Map.empty Set.empty None
                Expect.isEmpty (spawnIntents intents) "workforce already at minimum"
            }

            test "empty creeps spread across sources instead of piling on one" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50; worker "w2" 0 50 ]
                    }

                let { Assignments = assignments } = decide snapshot Map.empty Set.empty None
                let assigned = assignments |> Map.toList |> List.map snd |> List.sort

                Expect.equal
                    assigned
                    [ (taskId (Harvest "src-a")); (taskId (Harvest "src-b")) ]
                    "greedy matching balances load per task"
            }

            test "greedy matching counts kept assignments as load" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 20 30; worker "w2" 0 50 ]
                    }

                let { Assignments = assignments } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.equal
                    (Map.tryFind "w1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "w1 keeps its source"

                Expect.equal
                    (Map.tryFind "w2" assignments)
                    (Some(taskId (Harvest "src-b")))
                    "w2 avoids the occupied source"
            }

            test "assignments pass through unchanged when no creeps died" {
                let assignments = Map.ofList [ "worker-1", (taskId (Harvest "src-a")) ]

                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "worker-1" 0 50 ]
                    }

                let { Assignments = kept } = decide snapshot assignments Set.empty None
                Expect.equal kept assignments "assignments survive the tick"
            }

            test "an assignment sticks across ticks even when greedy would rebalance" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 20 30 ]
                    }

                let assignments = Map.ofList [ "w1", (taskId (Harvest "src-b")) ]

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot assignments Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Harvest "src-b")))
                    "no thrash: creep stays on its source"

                Expect.contains
                    intents
                    (HarvestSource("w1", "src-b"))
                    "intent follows the sticky assignment"
            }

            test "a creep that fills up is reassigned from Harvest to Refill" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "full creep switches to delivering"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("w1", "spawn-1"))
                    "delivery intent emitted"
            }

            test "a loaded creep feeds a hungry tower once spawn and extensions are full" {
                // Full feeders leave the pool, so the tower Refill is the one
                // delivery on offer — the same transfer to the creep (ADR 0010).
                let snapshot =
                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables =
                            [
                                refillable "spawn-1" 0 BuiltKind.Spawn
                                refillable "ext-1" 0 BuiltKind.Extension
                                refillable "tower-1" 500 BuiltKind.Tower
                            ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "tower-1")))
                    "the tower is the delivery that remains"

                Expect.contains
                    intents
                    (TransferEnergyToStructure("w1", "tower-1"))
                    "the same transfer intent feeds a tower"
            }

            test "a creep that empties is reassigned from Refill back to Harvest" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Assignments = kept } =
                    decide
                        snapshot
                        (Map.ofList [ "w1", (taskId (Refill "spawn-1")) ])
                        Set.empty
                        None

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.contains
                        [ taskId (Harvest "src-a"); taskId (Harvest "src-b") ]
                        tid
                        "empty creep goes back to a source"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test "surplus: a full creep with a full spawn switches to upgrading" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "nothing to refill, so surplus goes to the controller"

                Expect.contains intents (UpgradeController("w1", "ctrl-1")) "upgrade intent emitted"
            }

            test "a hungry structure beats the controller for a delivering creep" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "refill outranks upgrade while a structure is missing energy"
            }

            test "an upgrading creep that empties goes back to harvest" {
                let snapshot =
                    { bareRespawn with
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Assignments = kept } =
                    decide
                        snapshot
                        (Map.ofList [ "w1", (taskId (Upgrade "ctrl-1")) ])
                        Set.empty
                        None

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.contains
                        [ taskId (Harvest "src-a"); taskId (Harvest "src-b") ]
                        tid
                        "spent creep returns to a source"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test
                "a full creep with a full spawn and no controller is left unassigned with no intent" {
                let snapshot =
                    { bareRespawn with
                        Controller = None
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.isEmpty (Map.toList kept) "no applicable task"

                let creepIntents =
                    intents
                    |> List.filter (function
                        | SpawnCreep _ -> false
                        | _ -> true)

                Expect.isEmpty creepIntents "idle creep emits nothing"
            }

            test "a full creep with a construction site and a full spawn goes building" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let {
                        Intents = intents
                        Assignments = kept
                    } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Harvest "src-a")) ]) Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Build "site-1")))
                    "surplus energy goes into construction"

                Expect.contains intents (BuildSite("w1", "site-1")) "build intent emitted"
            }

            test "an empty creep is never matched to a Build task" {
                let snapshot =
                    { bareRespawn with
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 0 50 ]
                    }

                let { Assignments = kept } =
                    decide snapshot (Map.ofList [ "w1", (taskId (Build "site-1")) ]) Set.empty None

                match Map.tryFind "w1" kept with
                | Some tid ->
                    Expect.contains
                        [ taskId (Harvest "src-a"); taskId (Harvest "src-b") ]
                        tid
                        "empty creep goes harvesting instead"
                | None -> failtest "creep should be reassigned, not idle"
            }

            test "a hungry structure beats a construction site for a delivering creep" {
                let snapshot =
                    { bareRespawn with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Creeps = [ worker "w1" 50 0 ]
                    }

                let { Assignments = kept } = decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w1" kept)
                    (Some(taskId (Refill "spawn-1")))
                    "refill outranks build while a structure is missing energy"
            }

            test "assignments of dead creeps are dropped" {
                let assignments = Map.ofList [ "ghost", "task-a" ]
                let { Assignments = kept } = decide bareRespawn assignments Set.empty None
                Expect.isEmpty (Map.toList kept) "dead creep's assignment is released"
            }
        ]

[<Tests>]
let intakeRoomTests =
    testList
        "an intake needs room"
        [
            test "a nearly full hauler delivers before it picks up, and an emptier one picks up" {
                // Live, W12S28 2026-09-07: a hauler holding 1,150 of 1,200
                // walked forty tiles into the north room to pick fifty off
                // a pile while the spawn stood at eighteen energy — the
                // pile's lifted rung beat every Refill and one free slot
                // made it applicable. An intake is for a body at least half
                // empty; pairwise on the store alone, same tile, same pool.
                let lane energy =
                    let body = List.replicate 6 Carry @ List.replicate 3 Move

                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "ext-1" 50 BuiltKind.Extension ]
                        Creeps = [ creepWith "h" energy (300 - energy) body ]
                        Spatial =
                            { spatial [] [ for x in 8..18 -> { X = x; Y = 10 }, Plain ] with
                                Stores = Map.ofList [ "pile-1", 400 ]
                            }
                            |> withTargets
                                [
                                    "pile-1", { X = 12; Y = 10 }, Dropped
                                    "ext-1", { X = 16; Y = 10 }, Structure BuiltKind.Extension
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h", { X = 13; Y = 10 } ]
                                })
                    }

                let matched energy =
                    let { Verdicts = verdicts } = decide (lane energy) Map.empty Set.empty None

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h", task, _) -> Some task
                        | _ -> None)

                Expect.equal
                    (matched 280)
                    (Some(taskId (Refill "ext-1")))
                    "twenty free of three hundred: a delivery, not an intake"

                Expect.equal
                    (matched 100)
                    (Some(taskId (Pickup "pile-1")))
                    "two hundred free: the pile is taken first"
            }
        ]

[<Tests>]
let intakeWorthTests =
    testList
        "an intake is worth the trip"
        [
            test "a container that cannot half fill the hauler is left for the stock" {
                // Live, W12S28 2026-09-07 (#232): a 24C/12M hauler matched a
                // source container holding ~200 at t194,906 and was released
                // `inapplicable` forty-two ticks later, having drained the
                // Anchor's trickle up to half a load, while the Storage held
                // 263,803 and the spawn stood at twenty-eight energy. The
                // Withdraw's own [[capacity]] admits a drawer to any store
                // with one energy in it, and the tier (ADR 0023) keeps the
                // stock behind every container that applies — so the only
                // thing that reaches the stock is a container that does not.
                //
                // Pairwise on the store's stock alone: one hauler, two
                // Withdraws, and the container is the nearer of the two at
                // every reading, so nothing but this gate can move the match.
                let lane stock =
                    let body = List.replicate 24 Carry @ List.replicate 12 Move

                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "ext-1" 50 BuiltKind.Extension ]
                        Creeps = [ creepWith "h" 0 1200 body ]
                        Spatial =
                            { spatial [] [ for x in 8..24 -> { X = x; Y = 10 }, Plain ] with
                                Stores = Map.ofList [ "can-src", stock; "stock-1", 263_803 ]
                            }
                            |> withTargets
                                [
                                    "ext-1", { X = 8; Y = 10 }, Structure BuiltKind.Extension
                                    "can-src", { X = 12; Y = 10 }, Structure BuiltKind.Container
                                    "stock-1", { X = 22; Y = 10 }, Structure BuiltKind.Storage
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h", { X = 13; Y = 10 } ]
                                })
                    }

                let matched stock =
                    let { Verdicts = verdicts } = decide (lane stock) Map.empty Set.empty None

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h", task, _) -> Some task
                        | _ -> None)

                Expect.equal
                    (matched 216)
                    (Some(taskId (Withdraw "stock-1")))
                    "the live reading: two hundred at its feet is not worth a twelve-hundred body's trip"

                Expect.equal
                    (matched 599)
                    (Some(taskId (Withdraw "stock-1")))
                    "one under half a load is still the stock's"

                Expect.equal
                    (matched 600)
                    (Some(taskId (Withdraw "can-src")))
                    "half the body's free capacity standing in the store is worth the trip"
            }

            test "the line is the asking body's free capacity and not one row's load" {
                // The gate reads the pair and not the Task (#161, #196): the
                // same store that is too thin for a twelve-hundred hauler is
                // worth a 450-carry generalist's trip at a quarter of the
                // stock. One store and one body here, so what the readings
                // separate is the line itself and nothing else.
                let lane stock =
                    let body =
                        List.replicate 9 Work @ List.replicate 9 Carry @ List.replicate 9 Move

                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = []
                        Creeps = [ creepWith "w" 0 450 body ]
                        Spatial =
                            { spatial [] [ for x in 8..24 -> { X = x; Y = 10 }, Plain ] with
                                Stores = Map.ofList [ "can-src", stock ]
                            }
                            |> withTargets
                                [ "can-src", { X = 12; Y = 10 }, Structure BuiltKind.Container ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w", { X = 13; Y = 10 } ]
                                })
                    }

                let matched stock =
                    let { Verdicts = verdicts } = decide (lane stock) Map.empty Set.empty None

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("w", task, _) -> Some task
                        | _ -> None)

                Expect.isNone (matched 224) "one under half of 450: the walk is not paid for"

                Expect.equal
                    (matched 225)
                    (Some(taskId (Withdraw "can-src")))
                    "half of 450 standing in the store is worth the trip"
            }
        ]

[<Tests>]
let intakeDecayTests =
    testList
        "the worth-the-trip line and the stores it is off"
        [
            test "a store whose energy is going away is taken by whatever body is asking" {
                // The [[pickup]] is outside #232's line because a pile
                // decays (#167, #216 R5) — and a tombstone and a ruin decay
                // too, which is the only thing CONTEXT says separates them
                // from a container. So the exemption follows the decay and
                // not the Task's name: a hundred and fifty is not worth a
                // 1,200-carry hauler's trip to a *container*, because the
                // container will still be there when a smaller body asks,
                // and it is taken off either transient store by that same
                // hauler, because nothing will.
                //
                // Pairwise on the target's kind alone: one store, one body,
                // a hundred and fifty in it at every reading.
                let lane kind =
                    let body = List.replicate 24 Carry @ List.replicate 12 Move

                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = []
                        Creeps = [ creepWith "h" 0 1200 body ]
                        Spatial =
                            { spatial [] [ for x in 8..18 -> { X = x; Y = 10 }, Plain ] with
                                Stores = Map.ofList [ "store-1", 150 ]
                            }
                            |> withTargets [ "store-1", { X = 12; Y = 10 }, kind ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h", { X = 16; Y = 10 } ]
                                })
                    }

                let matched kind =
                    let { Verdicts = verdicts } = decide (lane kind) Map.empty Set.empty None

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h", task, _) -> Some task
                        | _ -> None)

                Expect.isNone
                    (matched (Structure BuiltKind.Container))
                    "a container holding a hundred and fifty waits for a body it can half fill"

                Expect.equal
                    (matched Tombstone)
                    (Some(taskId (Withdraw "store-1")))
                    "a tombstone ends, so its hundred and fifty is drawn by the body that is asking"

                Expect.equal
                    (matched Dropped)
                    (Some(taskId (Pickup "store-1")))
                    "and the pile the line was never carried to is picked up by the same body"
            }

            test "the stock is the fall-through, so it is never the thing that refuses" {
                // What the line buys is the fall to the tier below (ADR
                // 0023), and there is no tier below the stock's own
                // Withdraw. A Storage drawn down by a build — or a young
                // RCL4 one — holding four hundred against a 1,200-carry
                // hauler is the colony's last intake, and refusing it
                // leaves the row idle with the spawn hungry and the energy
                // in reach of nobody.
                //
                // Pairwise on the store's kind alone: the same four hundred
                // in a source container is exactly the refusal #232 asked
                // for.
                let lane kind =
                    let body = List.replicate 24 Carry @ List.replicate 12 Move

                    { bareRespawn with
                        Sources = []
                        Controller = None
                        Refillables = [ refillable "ext-1" 50 BuiltKind.Extension ]
                        Creeps = [ creepWith "h" 0 1200 body ]
                        Spatial =
                            { spatial [] [ for x in 8..24 -> { X = x; Y = 10 }, Plain ] with
                                Stores = Map.ofList [ "store-1", 400 ]
                            }
                            |> withTargets
                                [
                                    "ext-1", { X = 8; Y = 10 }, Structure BuiltKind.Extension
                                    "store-1", { X = 12; Y = 10 }, kind
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "h", { X = 16; Y = 10 } ]
                                })
                    }

                let matched kind =
                    let { Verdicts = verdicts } = decide (lane kind) Map.empty Set.empty None

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h", task, _) -> Some task
                        | _ -> None)

                Expect.equal
                    (matched (Structure BuiltKind.Storage))
                    (Some(taskId (Withdraw "store-1")))
                    "the deepest intake in the colony draws whatever body asks it"

                Expect.isNone
                    (matched (Structure BuiltKind.Container))
                    "the same four hundred in a container is left for the tier below it"
            }
        ]

[<Tests>]
let selfHealTests =
    let healer =
        { creepWith "patient" 0 0 [ Move; Heal ] with
            Hits = { Hits = 199; HitsMax = 200 }
        }

    let run creep actions =
        let colony = { bareRespawn with Creeps = [ creep ] }

        match IntentPlan.create actions with
        | Error conflict -> failtestf "invalid fixture: %A" conflict
        | Ok plan -> selfHeal colony plan |> IntentPlan.intents

    testList
        "self-heal reflex"
        [
            test "injured idle bodies heal themselves with no task or energy" {
                let colony = { bareRespawn with Creeps = [ healer ] }
                let result = decide colony Map.empty Set.empty None

                Expect.contains
                    result.Intents
                    (HealCreep("patient", "patient"))
                    "one point of damage is enough"

                Expect.isOk
                    (IntentPlan.create result.Intents)
                    "the complete decision stays executable"

                Expect.isFalse
                    (Map.containsKey "patient" result.Assignments)
                    "healing needs no assignment"
            }
            test "healthy bodies and bodies without active HEAL do nothing" {
                for body in [ healer.Body; Map.empty; Map.ofList [ Heal, 0 ] ] do
                    let healthy =
                        { healer with
                            Hits = { Hits = 200; HitsMax = 200 }
                            Body = body
                        }

                    Expect.isEmpty (run healthy []) "full life needs no healing"

                for body in [ Map.empty; Map.ofList [ Heal, 0 ]; Map.ofList [ Move, 1 ] ] do
                    Expect.isEmpty
                        (run { healer with Body = body } [])
                        "destroyed or absent HEAL cannot heal"
            }
            test "every engine-conflicting action takes precedence over the reflex" {
                for action in
                    [
                        HarvestSource("patient", "source")
                        AttackCreep("patient", "hostile")
                        BuildSite("patient", "site")
                        RepairStructure("patient", "road")
                        HealCreep("patient", "other")
                    ] do
                    Expect.equal
                        (run healer [ action ])
                        [ action ]
                        "the reflex cannot replace or suppress a chosen act"
            }
            test "independent actions coexist and another creep's attack does not block healing" {
                let selected =
                    [
                        UpgradeController("patient", "controller")
                        TransferEnergyToStructure("patient", "store")
                        WithdrawEnergyFromStructure("patient", "store")
                        PickupEnergy("patient", "pile")
                        ClaimController("patient", "controller")
                        ReserveController("patient", "controller")
                        MoveCreep("patient", Top)
                        SayCreep("patient", "task")
                        AttackCreep("other", "hostile")
                    ]

                Expect.equal
                    (run healer selected)
                    (selected @ [ HealCreep("patient", "patient") ])
                    "read the shared compatibility rules"
            }
            test "the reflex is idempotent and still acts when fatigued or almost dead" {
                let exhausted =
                    { healer with
                        Fatigue = 10
                        Hits = { Hits = 1; HitsMax = 200 }
                    }

                let once = run exhausted []

                Expect.equal
                    once
                    [ HealCreep("patient", "patient") ]
                    "one active HEAL is enough regardless of fatigue"

                Expect.equal (run exhausted once) once "never duplicate an existing self-heal"
            }
        ]
