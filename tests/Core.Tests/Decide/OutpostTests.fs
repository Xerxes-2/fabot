/// The Outposts: the rooms a colony mines but does not own (ADR 0041, ADR
/// 0042) — their sources and containers, the Reservation that doubles them, the
/// garrison that stands in them, the invader core that takes one back, and the
/// stand-down that gives one up until a tick read off the threat (ADR 0043).
module Fabot.Core.Tests.Decide.OutpostTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

[<Tests>]
let invaderCoreTests =
    testList
        "the invader core the ColonyView carries"
        [
            test "a core standing in an outpost moves nothing the colony decides" {
                // ADR 0043's first step, and the whole of what it claims:
                // the threat is projected and read by nobody. The gate
                // that will read it withholds a room from the scan set
                // (#136) and the episode that will carry its deadline is
                // the raid log's (#134); until both land, a core in the
                // projection has to leave every Task, every quota, every
                // cast and every Verdict where it found them — reach and
                // flee included, which is why the comparison below is over
                // the whole decision and not over the spawn Intents alone.
                //
                // The colony under it is the posted outpost at its
                // reserved target: creeps matched, a fleet with a gap, an
                // outpost source in the pool. A quiet fixture would make
                // the equality vacuous, so the premise is asserted first.
                let colony = postedOutpostColony 19 [ "W1N2", reservedRoom true 4000 ]
                let untroubled = decide colony Map.empty Set.empty None

                Expect.isNonEmpty
                    untroubled.Verdicts
                    "the premise: this colony reaches a decision worth comparing"

                Expect.isNonEmpty untroubled.Intents "and emits something for a core to disturb"

                // The whole of `Decision` and not three of its four fields.
                // `Memo` is the field the "no reader" claim is easiest to
                // break through and hardest to notice: a reader folded
                // into `censusSignature` moves `Memo.Signature` alone, so
                // the next tick's `recalled` misses and the Layout and the
                // spawn-walk table are thrown away and reflooded (ADR
                // 0032) — a real behaviour change, and an expensive one,
                // that leaves Intents, Assignments and Verdicts identical
                // on this fixture because both calls are handed no memo
                // and recompute from scratch anyway.
                //
                // One field of the memo cannot ride the record comparison:
                // `Walks` is the mutable `Dictionary` the Atlas fills
                // through the tick, and a Dictionary compares by
                // reference, so two floods of identical walks are unequal
                // on it for a reason that has nothing to do with a core.
                // Its reference is swapped in and its *contents* are
                // compared beside it, which loses nothing.
                let walkRows (memo: PlanMemo) =
                    memo.Walks
                    |> Seq.map (fun entry -> entry.Key, List.ofArray entry.Value)
                    |> List.ofSeq
                    |> List.sortBy fst

                let unchangedWith label cores =
                    let threatened =
                        decide { colony with InvaderCores = cores } Map.empty Set.empty None

                    Expect.equal
                        { threatened with
                            Memo =
                                { threatened.Memo with
                                    Walks = untroubled.Memo.Walks
                                }
                        }
                        untroubled
                        $"{label}: the same decision, memo and census signature and all"

                    Expect.equal
                        (walkRows threatened.Memo)
                        (walkRows untroubled.Memo)
                        $"{label}: the same spawn walks flooded under it"

                unchangedWith
                    "a core whose collapse timer is readable"
                    [
                        ({
                            RoomName = "W1N2"
                            CollapseTick = Some(colony.Time + 64000)
                        }
                        : InvaderCoreInfo)
                    ]

                // The level-0 expansion core of ADR 0043: no stronghold
                // under it, so no collapse timer, so no deadline — the
                // case the reservation and the 2,500-tick fallback exist
                // for, and the one a reader might treat as "no threat".
                unchangedWith
                    "a core carrying no deadline at all"
                    [
                        ({
                            RoomName = "W1N2"
                            CollapseTick = None
                        }
                        : InvaderCoreInfo)
                    ]

                // And one at home, where no outpost gate could ever apply:
                // the list is swept over every room the colony looks into,
                // so the spawn room can hold an entry, and the reflexes
                // that do read the spawn room read hostile *creeps*.
                unchangedWith
                    "a core standing in the colony's own room"
                    [
                        ({
                            RoomName = "W1N1"
                            CollapseTick = Some colony.Time
                        }
                        : InvaderCoreInfo)
                    ]
            }

            test
                "the whole frontier case — a level-0 core and the reservation it took — decides nothing" {
                // Both halves of the fact ADR 0043 reads, together, on the
                // shape actually measured two rooms from W12S27
                // (docs/research/remote-mining.md §8.4): a level-0 core
                // carrying no collapse timer, in a room whose controller
                // it has reserved for itself. The deadline lives only in
                // that reservation, which is why `ReservationHolder`
                // separates the NPC from a rival at all.
                //
                // Read against the same room under a *rival's*
                // reservation and no core: everything either fact could
                // move today is priced off the neutral rate both of them
                // yield, so a decision that differs is a reader — of the
                // holder or of the core — that this ticket says does not
                // exist yet (#134 opens the episode, #136 gates on it).
                let withControl control cores =
                    let colony = postedOutpostColony 19 [ "W1N2", control ]
                    decide { colony with InvaderCores = cores } Map.empty Set.empty None

                let frontier =
                    withControl
                        (coreReservedRoom 4900)
                        [
                            ({
                                RoomName = "W1N2"
                                CollapseTick = None
                            }
                            : InvaderCoreInfo)
                        ]

                let rivalHeld = withControl (reservedRoom false 4900) []

                Expect.isNonEmpty
                    rivalHeld.Verdicts
                    "the premise: this colony reaches a decision worth comparing"

                Expect.equal
                    { frontier with
                        Memo =
                            { frontier.Memo with
                                Walks = rivalHeld.Memo.Walks
                            }
                    }
                    rivalHeld
                    "a core and the NPC's own reservation decide exactly what a rival's reservation does"
            }
        ]

[<Tests>]
let neighbouringRoomTests =
    testList
        "decide across a border"
        [
            test "a source in the neighbouring room is no Task this creep can be given" {
                // The seam the Atlas's own `travelCost` test cannot reach:
                // the Matcher prices through the Work Area, not through the
                // Task-shaped wrapper, so a guard that sits only on the
                // wrapper leaves the ranking price to be invented off this
                // room's flood. Priced that way the neighbour's source is
                // cost 0 or a handful of units — cheaper than every home
                // rival — and the creep is assigned a Task `mayAct` refuses
                // for the rest of its life, walking inside its own room
                // toward ground it will never stand on. Until #123 sums the
                // legs over the Seam band the honest answer is that the
                // Task does not apply to this creep.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-out", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.ofList (corridor 10 10 17)
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let outpost =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (corridor 10 10 17)
                        TargetPositions = Map.ofList [ "src-out", { X = 10; Y = 18 } ]
                    }

                let snapshot =
                    { bareRespawn with
                        Spawns = []
                        Sources = [ source "src-out" ]
                        Controller = None
                        Refillables = []
                        Creeps = [ worker "w-home" 0 50 ]
                        Spatial = home |> withNeighbour "W2N1" outpost
                    }

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decide snapshot Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "w-home" assignments)
                    None
                    "the neighbour's Harvest is inapplicable, so nothing is assigned"

                Expect.isEmpty
                    (moveIntents intents)
                    "and nobody is walked toward a border they cannot cross"
            }

            test "a grounded creep in the neighbouring room grounds nobody here" {
                // ADR 0041's Consequences keep arbitrated movement and the
                // occupancy surcharge single-room, unchanged. The Resolver
                // pre-claims a fatigued creep's tile through a `Set<Pos>`
                // that has no room dimension (ADR 0008), so a creep on the
                // same coordinate of another room would deny a step here
                // on evidence from fifty tiles away. The two projections
                // differ only in what the neighbour holds, and this room
                // decides identically.
                let home =
                    { SpatialInfo.empty with
                        RoomName = Some "W1N1"
                        TargetKinds = Map.ofList [ "src-home", Source ]
                    }
                    |> withHome (fun layer ->
                        { layer with
                            Terrain = Map.ofList (corridor 10 10 18)
                            TargetPositions = Map.ofList [ "src-home", { X = 10; Y = 18 } ]
                            CreepPositions = Map.ofList [ "w-home", { X = 10; Y = 10 } ]
                        })

                let colony creeps layer =
                    { bareRespawn with
                        Spawns = []
                        Sources = [ source "src-home" ]
                        Controller = None
                        Refillables = []
                        Creeps = creeps
                        Spatial = home |> withNeighbour "W2N1" layer
                    }

                let assigned = Map.ofList [ "w-home", "harvest:src-home" ]

                let { Intents = alone } =
                    decide (colony [ worker "w-home" 0 50 ] RoomLayer.empty) assigned Set.empty None

                let neighbour =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (corridor 10 10 18)
                        CreepPositions = Map.ofList [ "w-out", { X = 10; Y = 11 } ]
                    }

                let { Intents = crowded } =
                    decide
                        (colony
                            [ worker "w-home" 0 50; { worker "w-out" 0 50 with Fatigue = 5 } ]
                            neighbour)
                        assigned
                        Set.empty
                        None

                Expect.equal
                    (moveIntents alone)
                    [ "w-home", Bottom ]
                    "the premise: with the neighbour empty the home creep steps down its corridor"

                Expect.equal
                    (moveIntents crowded)
                    (moveIntents alone)
                    "and a creep paying off fatigue in another room changes nothing here"
            }
        ]

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

                let { Assignments = assignments } = decide garrisoned Map.empty Set.empty None

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
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w", pos ]
                                })
                    }

                let assigned = Map.ofList [ "w", taskId (Harvest "src-out") ]

                let {
                        Intents = opening
                        Assignments = assignments
                    } =
                    decide (colonyAt { X = 10; Y = 2 }) Map.empty Set.empty None

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
                        let { Intents = intents } = decide (colonyAt pos) assigned Set.empty None

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
                        decide landed assigned Set.empty None

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
                        let { Intents = intents } = decide (landedAt pos) assigned Set.empty None

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

                let { Intents = opening } = decide sited Map.empty Set.empty None

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
                    (decide (darkSince tick) assignments Set.empty None).Verdicts

                // Pairwise on the grace and on nothing else: one room, one
                // creep, one held Task, and the only thing that moves
                // between the three readings is the tick the room was last
                // seen at.
                Expect.contains
                    (decide sited assignments Set.empty None).Verdicts
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
                    decide (darkSince 999) assignments Set.empty None

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
                    decide backWithSite assignments Set.empty None

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
                    (decide backWithout assignments Set.empty None).Verdicts
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
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> loaded

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

                let assigned = Map.ofList [ "w", taskId (Build "site-out") ]

                let {
                        Intents = landing
                        Verdicts = verdicts
                    } =
                    decide (landedAt { X = 9; Y = 49 }) assigned Set.empty None

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
                        let { Intents = intents } = decide (landedAt pos) assigned Set.empty None

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

                let { Intents = opening } = decide sited Map.empty Set.empty None

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
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> loaded
                        |> withHomeController { X = 10; Y = 5 }

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
                                    CreepPositions = Map.ofList [ "w", { X = 10; Y = 46 } ]
                                }
                    }

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
                    (Some(taskId (Refill "ext-1"), MatchFactor.TravelCost))
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
                    (Some(taskId (Refill "ext-1"), MatchFactor.Rank))
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
                let crowd =
                    let colony =
                        northBorderColony { X = 10; Y = 38 }
                        |> withNorthOutpost None
                        |> withOutpostSite { X = 10; Y = 43 }
                        |> withHomeController { X = 10; Y = 5 }

                    { colony with
                        Creeps = [ for name in [ "w1"; "w2"; "w3" ] -> worker name 50 0 ]
                        Spatial =
                            colony.Spatial
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [
                                                "w1", { X = 10; Y = 2 }
                                                "w2", { X = 10; Y = 3 }
                                                "w3", { X = 10; Y = 4 }
                                            ]
                                })
                    }

                let { Assignments = assignments } = decide crowd Map.empty Set.empty None

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
                            decide (colonyAt room pos carrying) assigned Set.empty None

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
                    (moveIntents (decide (sited anchor) Map.empty Set.empty None).Intents)
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
                    let colony =
                        { (sited |> loaded) with
                            Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        }
                        |> withTarget "spawn-1" { X = 10; Y = 2 } (Structure BuiltKind.Spawn)

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
                                    CreepPositions = Map.ofList [ "w", { X = 10; Y = 44 } ]
                                }
                    }

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList
                                            [ for n in 1..5 -> $"w{n}", { X = 10; Y = n + 1 } ]
                                })
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

                let { Assignments = assignments } = decide crowd Map.empty Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions =
                                        Map.ofList (("w", { X = 10; Y = 4 }) :: homeCreeps)
                                })
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
                        decide (colony homeCreeps outpostCreeps) assigned Set.empty None

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
                            |> withHome (fun layer ->
                                { layer with
                                    CreepPositions = Map.ofList [ "w", { X = 10; Y = 4 } ]
                                })
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
                    decide colony (Map.ofList [ "w", taskId (Harvest "src-home") ]) Set.empty None

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
                    decide
                        (colonyWith [ worker "a" 50 0 ] [ "a", { X = 9; Y = 49 } ])
                        Map.empty
                        Set.empty
                        None

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
                    decide colony (Map.ofList [ "t", taskId (Harvest "src-out") ]) Set.empty None

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
                "the declared outposts are ADR 0042's north room and the survey's south one, so the shells scan four rooms" {
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
                // where the captures are read (`RoomInvariantTests`) and
                // never retyped here — two literals of the same ids would
                // agree with each other and with nothing else.
                let outposts = Colony.outpostsOf Colony.declared "W12S28"

                Expect.equal
                    (Colony.homes Colony.declared)
                    [ "W12S28"; "W13S28" ]
                    "two colonies are declared: the room this bot has always run, and the candidate (ADR 0047)"

                Expect.equal
                    (outposts |> List.map (fun outpost -> outpost.RoomName))
                    [ "W12S27" ]
                    "the north outpost alone: the west one is a colony of its own now (ADR 0047)"

                Expect.equal
                    (Outpost.adr0042 |> List.map (fun outpost -> outpost.RoomName))
                    [ "W12S27"; "W13S28" ]
                    "while ADR 0042's measured pair is kept whole for the real-terrain fixtures"

                Expect.isEmpty
                    (Colony.outpostsOf Colony.declared "W13S28")
                    "and the second colony works none: W13S29 is withdrawn by hand while its raid stands (#257)"

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

/// The colony with **two** outposts and one Anchor standing beside the
/// wrong one — the live report #159 was filed on, at the seam it is
/// decided at.
///
/// Home is the north corridor with a west arm along row 26 joining it, and
/// the Anchor stands two tiles from the west border and thirty-three from
/// the north one — the asymmetry the live colony had, both numbers walked
/// to the border tile itself. W1N2 across the north border carries a rock
/// with a container standing on one of its Seats; W2N1 across the west
/// carries a rock with nothing on it — the shape the live colony really
/// had, the north container built and the west ones not.
///
/// `northBorderColony`'s own rock is not a third one: `Sources` is
/// replaced, `src-home` is dropped from the kind census and the home
/// layer's `TargetPositions` is emptied, so the position handed to it
/// places nothing and the pool really is the two outpost Harvests.
///
/// The Anchor quota is the colony's Post count (ADR 0042): with the west
/// rock bare that count is one, so the north container hires exactly this
/// one body — and nothing in the Matcher knows which Post it was hired
/// for. It is ranked by `(rank, cost, load)` like every other body, which
/// is what sent the live one west. With a container on the west rock the
/// count is two and this body is the first of them; nothing here casts the
/// second, the fixture having no spawn.
///
/// The pool is those two Harvests and nothing else: no controller, no
/// refillable, no site, and Withdraw and Flee are inapplicable to a
/// Work-heavy body (ADR 0016, ADR 0033). Pairwise by construction, so a
/// Matched Verdict here names this pair and no third candidate stands in
/// for either side of it.
let private twoRockColony (westContainer: (string * Pos) list) =
    let colony = northBorderColony { X = 10; Y = 38 }

    { colony with
        Sources = [ source "src-north"; source "src-west" ]
        Creeps = [ creepWith "anchor" 0 50 [ Work; Work; Carry; Move ] ]
        Spatial =
            { colony.Spatial with
                Borders =
                    colony.Spatial.Borders |> Map.add "W1N2" plainRing |> Map.add "W2N1" plainRing
                TargetKinds =
                    (colony.Spatial.TargetKinds, westContainer)
                    ||> List.fold (fun kinds (id, _) ->
                        Map.add id (Structure BuiltKind.Container) kinds)
                    |> Map.remove "src-home"
                    |> Map.add "src-north" Source
                    |> Map.add "src-west" Source
                    |> Map.add "cont-north" (Structure BuiltKind.Container)
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        (layer.Terrain, [ for x in 1..10 -> { X = x; Y = 26 } ])
                        ||> List.fold (fun terrain pos -> Map.add pos Plain terrain)
                    TargetPositions = Map.empty
                    CreepPositions = Map.ofList [ "anchor", { X = 2; Y = 26 } ]
                })
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = Map.ofList (corridor 10 40 48)
                    TargetPositions =
                        Map.ofList
                            [ "src-north", { X = 10; Y = 46 }; "cont-north", { X = 10; Y = 45 } ]
                }
            |> withNeighbour
                "W2N1"
                { RoomLayer.empty with
                    Terrain = Map.ofList [ for x in 41..48 -> { X = x; Y = 26 }, Plain ]
                    TargetPositions = Map.ofList (("src-west", { X = 45; Y = 26 }) :: westContainer)
                }
    }

/// Which Task won the one Anchor, and what separated it from its closest
/// rival — `matchOf`'s reading for the body these cases hire (ADR 0009).
let private anchorMatch (colony: ColonyView) =
    let { Verdicts = verdicts } = decide colony Map.empty Set.empty None

    verdicts
    |> List.tryPick (function
        | Verdict.Matched("anchor", task, factor) -> Some(task, factor)
        | _ -> None)

[<Tests>]
let twoOutpostAnchorTests =
    testList
        "an Anchor between two outposts"
        [
            test "an unposted outpost rock is not a rival, however near it stands" {
                // The live failure: the colony's one container stood in the
                // north outpost, the Anchor its Post hired was born in the
                // spawn room, and it walked *west* — to a room with no
                // container at all — because ADR 0020's bare-Seat fallback
                // made those Seats reachable and travel cost had nothing
                // left to say but "nearer".
                //
                // The fix is geometric and not a rank or a quota: the west
                // rock's Work Area for this body is empty, so the Task has
                // no travel cost and never enters the pool. The factor
                // therefore reads `only-candidate` rather than
                // `travel-cost`, which is the whole claim — the near rock
                // is not a rival the far one beat, it is not a candidate.
                Expect.equal
                    (anchorMatch (twoRockColony []))
                    (Some(taskId (Harvest "src-north"), MatchFactor.OnlyCandidate))
                    "the posted rock a room and thirty-eight tiles away is the only Task there is"

                let { Intents = intents } = decide (twoRockColony []) Map.empty Set.empty None

                Expect.equal
                    (moveIntents intents)
                    [ "anchor", Right ]
                    "and the Anchor walks back up the west arm toward the northern Seam"

                Expect.isEmpty
                    (actionIntents intents)
                    "digging nothing on the way: it may not act on a target a room away"
            }

            test "a container standing on the west rock makes it a rival again, and it wins" {
                // The other half, and the only reading under which the case
                // above says anything: nothing here refuses an outpost, or
                // ranks a near room behind a far one. Put a container on the
                // west rock's Seat and that rock is posted, its Work Area is
                // that Post, and travel cost — the one comparison left
                // between two feeding-tier Harvests — sends the Anchor to
                // the near one exactly as it always did.
                Expect.equal
                    (anchorMatch (twoRockColony [ "cont-west", { X = 46; Y = 26 } ]))
                    (Some(taskId (Harvest "src-west"), MatchFactor.TravelCost))
                    "two posted rocks are two candidates, and the near one is cheaper"

                let { Intents = intents } =
                    decide
                        (twoRockColony [ "cont-west", { X = 46; Y = 26 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (moveIntents intents)
                    [ "anchor", Left ]
                    "and the same body walks the other way, two tiles to the western Seam"
            }
        ]

/// The two-room fixture the live report was filed on (#193): a Post in
/// each room, both rocks inside their empty window, and one body standing
/// beside the outpost's Post — the tile a hauler drawing that container
/// swaps an Anchor onto.
///
/// Home's rock is the given ticks from its restock and the outpost's its
/// own, and the crossing is a corridor, a Seam and thirty-six tiles, which
/// an Anchor walks at four ticks a step. So ADR 0025 read alone dispatches
/// it home: released from a Post it is standing beside, to walk a border
/// for one that another Anchor is standing on — and a full rock is left
/// with nobody on it while a dry one draws two.
///
/// The pool is those two Harvests and nothing else — `northBorderColony`
/// carries no controller, no spawn and no refillable, and Withdraw and
/// Flee are inapplicable to a Work-heavy body (ADR 0016, ADR 0033) — so
/// what decides here is the one comparison the case is about.
let private twoPostWindowColony ticksHome ticksOut (creep: CreepInfo) =
    let colony =
        northBorderColony { X = 10; Y = 38 }
        |> withNorthOutpost (Some { X = 10; Y = 46 })

    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Sources = [ drained "src-home" ticksHome; drained "src-out" ticksOut ]
        Creeps = [ creep ]
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    colony.Spatial.TargetKinds
                    |> Map.add "can-home" (Structure BuiltKind.Container)
                    |> Map.add "can-out" (Structure BuiltKind.Container)
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.add "can-home" { X = 10; Y = 37 } layer.TargetPositions
                    CreepPositions = Map.empty
                })
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = Map.add "can-out" { X = 10; Y = 45 } outpost.TargetPositions
                    CreepPositions = Map.ofList [ creep.Name, { X = 10; Y = 47 } ]
                }
    }

[<Tests>]
let heavyPinAcrossTests =
    testList
        "heavy pin across a border"
        [
            test "an empty window at home does not pull an Anchor out of its outpost" {
                // The live tick #193 reports, in two rooms: an Anchor
                // stepped off its container by a hauler, its own rock
                // fifty ticks from restocking, and a home Post whose rock
                // is dry too. ADR 0025 dispatched it because an Anchor's
                // walk covers any wait; ADR 0048 keeps it where it is,
                // because it is in digging range of the rock it was hired
                // for and there is nothing at the far end of that crossing
                // it can do sooner.
                let colony = twoPostWindowColony 20 30 (anchor "a1" 0 50)
                let remembered = Map.ofList [ "a1", taskId (Harvest "src-out") ]

                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide colony remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Kept("a1", taskId (Harvest "src-out")))
                    "the outpost's Post is held through its window, not surrendered"

                Expect.isEmpty
                    (harvesters assignments "src-home")
                    "and the home Post is nobody's to cross a border for"

                Expect.equal
                    (moveIntentsFor "a1" intents)
                    [ MoveCreep("a1", Top) ]
                    "and the one step it takes is back onto its own Post, away from the Seam"
            }

            test "the same geometry really does dispatch a light body home" {
                // The premise of the case above, and the pairwise half of
                // ADR 0048: nothing here is unreachable, mis-posted or out
                // of the pool. A worker on the very tile the Anchor stands
                // on is released from the outpost rock it is beside and
                // crosses the Seam for the home one, because thirty-six
                // tiles at a tick a tile cover twenty ticks of waiting —
                // which is ADR 0025 exactly as it was written.
                let colony = twoPostWindowColony 20 30 (worker "w" 0 50)
                let remembered = Map.ofList [ "w", taskId (Harvest "src-out") ]

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide colony remembered Set.empty None

                Expect.contains
                    verdicts
                    (Verdict.Released(
                        "w",
                        taskId (Harvest "src-out"),
                        ReleaseReason.TooEarly(0, 30)
                    ))
                    "a light body beside a dry rock is released as it always was"

                Expect.equal
                    (harvesters assignments "src-home")
                    [ "w" ]
                    "and the walk home covers the wait, so it sets out"
            }
        ]


/// A colony with an outpost beside it whose ground and furniture are the
/// case's own: the room's whole floor, and everything placed in it, said
/// here rather than inherited. The container pick is a choice *between*
/// Seats (ADR 0042), so a corridor with one Seat at each end proves
/// nothing about it; these cases lay a floor that makes the Seats differ.
///
/// Both rooms get a plain border ring, because the pick is measured to the
/// Seam and a projection carrying no ring answers an empty band (ADR
/// 0041). No case declares an edge: which border two rooms share is read
/// out of their names.
///
/// The outpost room gets a `RoomControl` entry, held by nobody: that map
/// is one entry per *seen* room, so an entry is how a fixture says the
/// colony is looking into the room this tick — which is what the placement
/// rule waits for, and what the Executor needs to create anything there.
/// Neutral rather than reserved because nothing here reads the rate; the
/// blind room is a case of its own below.
let private withOutpostGround room terrain placed (colony: ColonyView) =
    { colony with
        Sources = colony.Sources @ [ source "src-out" ]
        RoomControl = Map.add room neutralRoom colony.RoomControl
        Spatial =
            { colony.Spatial with
                Borders =
                    colony.Spatial.Borders
                    |> Map.add (SpatialInfo.homeName colony.Spatial) plainRing
                    |> Map.add room plainRing
                TargetKinds =
                    (colony.Spatial.TargetKinds, placed)
                    ||> List.fold (fun kinds (id, _, kind) -> Map.add id kind kinds)
            }
            |> withNeighbour
                room
                { RoomLayer.empty with
                    Terrain = Map.ofList terrain
                    TargetPositions = placed |> List.map (fun (id, pos, _) -> id, pos) |> Map.ofList
                }
    }

/// Every container site the tick asks for, room beside tile, in the order
/// the colony emits them — the whole of what this rule adds to a Decision.
let private containerSites (colony: ColonyView) =
    let { Intents = intents } = decide colony Map.empty Set.empty None

    intents
    |> List.choose (function
        | PlaceConstructionSite(tile, Container) -> Some(tile.Room, RoomPos.pos tile)
        | _ -> None)

/// `src-out` sits at (10,44), which no case lays ground on, so its Seats
/// are whichever of its eight neighbours the case does.
let private outpostSource = { X = 10; Y = 44 }

/// A road that **stands** on one of the outpost's tiles, handed over in the
/// two pieces `World.factsOf` hands one in: the id-keyed kind census, which
/// `withOutpostGround` takes as a `Structure`, and the layer's own `Roads`,
/// which is the half — and the only half — the walk prices (ADR 0010). A
/// fixture laying one piece alone would be a road the projection half
/// believes in.
let private paved room tiles (colony: ColonyView) =
    let layer =
        Map.tryFind room colony.Spatial.Rooms |> Option.defaultValue RoomLayer.empty

    { colony with
        Spatial =
            colony.Spatial
            |> withNeighbour
                room
                { layer with
                    Roads = Set.union layer.Roads (Set.ofList tiles)
                }
    }

/// Two Seats and two ways out. `(10,45)` is a row nearer the border and
/// its only run to it is three tiles of swamp; `(11,43)` is a row farther
/// and its run is five of plain. Walk and proximity therefore disagree,
/// which is the whole point of the floor: 6 ticks against 16.
let private detourGround =
    [
        { X = 10; Y = 45 }, Plain
        { X = 10; Y = 46 }, Swamp
        { X = 10; Y = 47 }, Swamp
        { X = 10; Y = 48 }, Swamp
        { X = 11; Y = 43 }, Plain
        for y in 44..48 do
            { X = 12; Y = y }, Plain
    ]

[<Tests>]
let outpostContainerTests =
    testList
        "the outpost's source container"
        [
            test "the site lands on the Seat whose walk out to the Seam is shortest" {
                // ADR 0042's own rule, at the seam it is decided on: an
                // outpost has no spawn for a trunk to anchor on, so the
                // pick is anchored on the Seam instead. Measured as a walk
                // and never as a range — the two disagree on this floor by
                // construction, and the Seat the range would pick is the
                // one three swamp tiles from the border.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                Expect.equal
                    (Atlas.seatTilesOf (Atlas.ofView colony) "src-out" |> RoomPos.inRoom "W1N2")
                    (Set.ofList [ { X = 10; Y = 45 }; { X = 11; Y = 43 } ])
                    "the premise: the rock has two Seats, and the nearer one to the border is (10,45)"

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the farther Seat wins, because the ground between it and the Seam is cheaper"
            }

            test "the Intent carries the outpost's own room, never the colony's" {
                // The trap the Layout would have walked into: a placement
                // Intent has always carried a room name, and `planLayout`
                // stamps the one room it plans onto every site it emits, so
                // an outpost pick routed through that path would drop a
                // 5,000-energy container on the *home* room's tile of the
                // same coordinates. (11,43) is a real coordinate in both
                // rooms and this asserts which one is named.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                // Asserted as the whole list and never with `Expect.all`,
                // which is vacuously true of the empty one: a rule that
                // planned nothing would pass the room-stamping case it
                // exists to pin.
                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the one site this rule places names the room its source stands in"
            }

            test "a room the colony cannot see this tick is planned nothing" {
                // ADR 0004 entry by entry, the same reading `sourceOutputOf`
                // gives the same rock: with no vision the container census
                // of that room is empty because nobody looked, not because
                // nothing stands there, and an absence is not an answer.
                // The Intent would also be one the Executor can only report
                // as `ActorMissing` — `Game.rooms` holds the seen rooms
                // alone — so a rule that fired here would file an upstream
                // bug against itself once a tick per rock, for ever.
                let seen =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                Expect.isNonEmpty
                    (containerSites seen)
                    "the premise: seen, this rock is planned a container"

                Expect.isEmpty
                    (containerSites
                        { seen with
                            RoomControl = Map.remove "W1N2" seen.RoomControl
                        })
                    "and the same tick with the room unseen plans nothing at all"
            }

            test "a source with one Seat is the same rule with one candidate" {
                // W13S28's `16,7` is a single-Seat rock, and "the shortest"
                // has to answer where there is nothing to be shorter than.
                let ground =
                    [
                        { X = 10; Y = 45 }, Swamp
                        for y in 46..48 do
                            { X = 10; Y = y }, Plain
                    ]

                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" ground [ "src-out", outpostSource, Source ]

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 10; Y = 45 } ]
                    "the one Seat there is, priced and picked like any other"
            }

            test "Seats that price alike fall to the lowest (X, Y), as every tie here does" {
                // W12S27's `16,45` has three Seats and all three are swamp,
                // so they can price identically — and a plan that answered
                // a different one of them on different ticks would not be
                // one (ADR 0011's determinism). Three swamp Seats over one
                // plain apron, so the three walks are equal by construction
                // and only the tie-break separates them.
                //
                // It also pins the subtraction the walk is measured with:
                // the Seat's own swamp step is charged to whatever walks
                // *in* to it, so three swamp Seats over identical ground
                // tie rather than each carrying five ticks of their own.
                let ground =
                    [
                        { X = 9; Y = 45 }, Swamp
                        { X = 10; Y = 45 }, Swamp
                        { X = 11; Y = 45 }, Swamp
                        for x in 8..12 do
                            for y in 46..48 do
                                { X = x; Y = y }, Plain
                    ]

                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" ground [ "src-out", outpostSource, Source ]

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 9; Y = 45 } ]
                    "the three Seats tie, and the lowest X answers"
            }

            test "a Seat's own terrain is not charged to it: the walk is the ground beyond it" {
                // The convention every walk in this colony is measured by
                // (ADR 0029): a walk charges the tiles a creep steps onto
                // and never the tile it already stands on. Here it decides
                // the pick. Two Seats over one symmetric plain apron, so
                // the ground beyond them is identical and only their own
                // terrain differs — the swamp one first in (X, Y) order. A
                // rule that charged a Seat for standing on it would price
                // the swamp Seat five ticks dearer and pick the plain one;
                // this rule ties them and lets the tie-break answer, which
                // is right because whoever hauls from that container starts
                // on it and never pays to arrive.
                let ground =
                    [
                        { X = 9; Y = 45 }, Swamp
                        { X = 11; Y = 45 }, Plain
                        for x in 8..12 do
                            for y in 46..48 do
                                { X = x; Y = y }, Plain
                    ]

                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" ground [ "src-out", outpostSource, Source ]

                Expect.equal
                    (Atlas.seatTilesOf (Atlas.ofView colony) "src-out" |> RoomPos.inRoom "W1N2")
                    (Set.ofList [ { X = 9; Y = 45 }; { X = 11; Y = 45 } ])
                    "the premise: two Seats, one swamp and one plain, over the same apron"

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 9; Y = 45 } ]
                    "the swamp Seat is no dearer than the plain one, so the tie-break answers"
            }

            test "a container already serving the source is planned for no second one" {
                // ADR 0040 holds here as it does at home, and by target
                // rather than by tile: the thing serving the rock is on
                // (10,45), which is not the tile the plan picked, and the
                // rock is served all the same. Standing and pending both,
                // because the plan asks whether another must be built and a
                // site going up answers that.
                let served kind =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [ "src-out", outpostSource, Source; "con-out", { X = 10; Y = 45 }, kind ]

                Expect.isEmpty
                    (containerSites (served (Structure BuiltKind.Container)))
                    "a container standing within range 1 of the rock, on a Seat the plan did not pick"

                Expect.isEmpty
                    (containerSites (served (Site BuiltKind.Container)))
                    "and a site pending there, which is a container already being built"
            }

            test "a Seat another kind's site already holds is no candidate at all" {
                // #244, live in W13S29: the human paved the outpost by hand
                // and his road sites landed on the two Seats this rule had
                // picked, (28,6) and (15,28). The engine takes one
                // construction site per tile, so the Executor asked for the
                // container on a taken tile and was answered
                // ERR_INVALID_TARGET once a tick, for ever — and with no
                // container the rock is no Post, hires no Anchor and enters
                // no income quota (ADR 0042), behind a road two workers
                // finish at the surplus tier. So the pick moves to the next
                // cheapest Seat rather than waiting on a site nobody
                // promised to build.
                let siteOn tile =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [ "src-out", outpostSource, Source; "road-out", tile, Site BuiltKind.Road ]

                Expect.equal
                    (containerSites (siteOn { X = 11; Y = 43 }))
                    [ "W1N2", { X = 10; Y = 45 } ]
                    "the Seat the walk picked is taken, so the dearer Seat takes the container"

                Expect.equal
                    (containerSites (siteOn { X = 10; Y = 45 }))
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "and a site on the Seat that lost moves nothing: one tile is subtracted, not a source"
            }

            test "a road that already stands is no obstruction, and is the best tile there is" {
                // The half the clause must not subtract. One construction
                // site per tile is the whole of the engine's rule: a
                // *finished* structure holds no site, and a container on a
                // paved Seat is the tile this rule would have chosen anyway,
                // since the hauler that draws it arrives over the road. A
                // built road prices the tile too, so it is laid in both
                // pieces the shell lays one in (`paved`).
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [
                            "src-out", outpostSource, Source
                            "road-out", { X = 11; Y = 43 }, Structure BuiltKind.Road
                        ]
                    |> paved "W1N2" [ { X = 11; Y = 43 } ]

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the pick is unmoved by a road that has finished going up on it"
            }

            test "a source whose every Seat is taken plans nothing and waits" {
                // Waiting is the answer and it is not a self-clearing one.
                // The colony has no vocabulary for cancelling a human's
                // site and asking the engine for a refusal once a tick is
                // not a plan — but nothing here promises the Seat comes
                // back either: a road site in an outpost is a plain
                // Surplus Build with no home rung and outside the
                // builders' budget, which is what "an ordinary outpost
                // site keeps its travel cost" above pins, so the human's
                // site is the only thing that ends this. Pinned as it
                // really is: no Intent, this tick or any other.
                let bothTaken =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround
                        "W1N2"
                        detourGround
                        [
                            "src-out", outpostSource, Source
                            "road-a", { X = 11; Y = 43 }, Site BuiltKind.Road
                            "road-b", { X = 10; Y = 45 }, Site BuiltKind.Road
                        ]

                Expect.isEmpty
                    (containerSites bothTaken)
                    "both Seats hold a site, so this rock is planned no container this tick"
            }

            test "a home container on the pick's coordinates defers nothing" {
                // The room-blind census this rule would have inherited: a
                // `Pos` carries no room (ADR 0041), so a census unioning
                // both rooms' container tiles would read the home room's
                // container as serving an outpost rock fifty tiles away —
                // and would then defer the outpost's container forever,
                // leaving the room with no switch to close (ADR 0042).
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]
                    |> withTarget "con-home" { X = 11; Y = 43 } (Structure BuiltKind.Container)

                Expect.equal
                    (containerSites colony)
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "the outpost's rock is unserved: what stands on those coordinates stands at home"
            }

            test "the home room's Layout is not moved by an outpost joining the projection" {
                // ADR 0042: "The outpost gets a container and nothing
                // else. No roads, and no Layout." This rule runs beside the
                // Layout and never inside it, so a colony that gains an
                // outpost plans the same home room it planned without one —
                // the same clustered picks, the same trunks, the same
                // containers, the same footings — and gains exactly one
                // site, in the other room.
                let alone = trunkColony 4

                let withOutpost =
                    alone
                    |> withOutpostGround "W1N2" detourGround [ "src-out", outpostSource, Source ]

                let atHome colony =
                    let { Intents = intents } = decide colony Map.empty Set.empty None

                    placementIntents intents |> List.filter (fun (room, _, _) -> room = "W1N1")

                Expect.isNonEmpty (atHome alone) "the premise: this room has a Layout to move"

                Expect.equal
                    (atHome withOutpost)
                    (atHome alone)
                    "every home site the Layout placed, unmoved and in its own order"

                Expect.equal
                    (containerSites withOutpost |> List.filter (fun (room, _) -> room <> "W1N1"))
                    [ "W1N2", { X = 11; Y = 43 } ]
                    "and the one site the outpost gained is the container, in the outpost"
            }

            test "a room home shares no border with is planned nothing" {
                // Total (ADR 0004): the Seam is read out of the two room
                // names, and two rooms four sectors apart have no band —
                // so the walk that anchors the pick has no anchor, and an
                // unpriceable rule plans nothing rather than planning
                // arbitrarily. W5N5 is not W1N1's neighbour.
                let colony =
                    northBorderColony { X = 10; Y = 38 }
                    |> withOutpostGround "W5N5" detourGround [ "src-out", outpostSource, Source ]

                Expect.isEmpty
                    (containerSites colony)
                    "no band to price a Seat against, so no Seat is picked"
            }
        ]

/// The colony's own room for the haul below: its spawn standing eleven
/// tiles down a one-wide corridor from the north border, an obstacle as a
/// spawn is, so the only tile a transfer reaches it from on the side the
/// haul arrives on is (25,9). No controller, no refillable with room and
/// no home source — what the hauler quota folds here is the outpost's one
/// container and nothing beside it, so the number this fixture answers is
/// that container's own.
///
/// **A 600 bank and not the default 300, because the rate is what these
/// cases read** (#208). A Post is worth what its garrison digs under the
/// rock's own rate, and at 300 the Anchor row casts `2W/1C/1M` and digs
/// four — under the neutral five as well as the held ten, so the two rates
/// would price alike and every pairwise case below would compare a number
/// with itself. At 600 the row casts five Work against a held rock and
/// three against a neutral one (`sourceOutputOf`), which digs ten and six:
/// the cap binds on neither and the rate is the answer, which is the fact
/// these cases are about. The bank's other effect is the divisor — this
/// row's body carries 400 here rather than 200 — and the round trips are
/// unmoved, both bodies standing at road parity.
let private haulHome =
    { bareRespawn with
        Controller = None
        Refillables = []
        Sources = [ source "src-out" ]
        Bank = bank 600 600
        Spatial =
            { SpatialInfo.empty with
                RoomName = Some "W1N1"
                Borders = Map.ofList [ "W1N1", plainRing; "W1N2", plainRing ]
                TargetKinds = Map.ofList [ "spawn-1", Structure BuiltKind.Spawn ]
            }
            |> withHome (fun layer ->
                { layer with
                    Terrain = Map.ofList (corridor 25 1 48)
                    TargetPositions = Map.ofList [ "spawn-1", { X = 25; Y = 10 } ]
                    Obstacles = Set.singleton { X = 25; Y = 10 }
                })
    }

/// The same colony with its outpost one room north: the rock at (25,40) on
/// ground the projection carries none of, and the container standing on
/// the Seat below it — the switch that admits an outpost into the economy
/// (ADR 0042). Who holds W1N2 is the caller's and is the only thing that
/// moves between two calls; `None` is the room the colony sees nobody in,
/// which is a third answer and not the neutral one (ADR 0004).
let private withHaulOutpost (control: RoomControlInfo option) (colony: ColonyView) =
    { colony with
        RoomControl =
            match control with
            | Some held -> Map.add "W1N2" held colony.RoomControl
            | None -> colony.RoomControl
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    colony.Spatial.TargetKinds
                    |> Map.add "src-out" Source
                    |> Map.add "can-out" (Structure BuiltKind.Container)
            }
            |> withNeighbour
                "W1N2"
                { RoomLayer.empty with
                    Terrain = Map.ofList (corridor 25 41 48)
                    TargetPositions =
                        Map.ofList [ "src-out", { X = 25; Y = 40 }; "can-out", { X = 25; Y = 41 } ]
                }
    }

/// The same outpost the tick before its container stands: the rock
/// projected and the room held exactly as above, and `can-out` simply
/// absent, which is what a Seat with nothing built on it is. The standing
/// census is then the only census input that moves between the two.
let private beforeHaulContainer (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = colony.Spatial.TargetKinds |> Map.remove "can-out"
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = outpost.TargetPositions |> Map.remove "can-out"
                }
    }

/// The same outpost with a **second** rock and container standing down a
/// side branch of its corridor, the container's tile the caller's. What it
/// buys is the shape acceptance criterion 1 names and one container cannot
/// exercise: more than one Seam-crossing term in the same sum, so a join
/// that priced only one of them — or collapsed two into one — moves a
/// number here where a single-container fixture would stay green. The
/// branch runs east along y = 44, so the tile alone lengthens this
/// container's haul and nothing else's.
let private withSecondHaulContainer (tile: Pos) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Sources = source "src-out2" :: colony.Sources
        Spatial =
            { colony.Spatial with
                TargetKinds =
                    colony.Spatial.TargetKinds
                    |> Map.add "src-out2" Source
                    |> Map.add "can-out2" (Structure BuiltKind.Container)
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    Terrain =
                        (outpost.Terrain, [ for x in 26..40 -> { X = x; Y = 44 } ])
                        ||> List.fold (fun acc branch -> Map.add branch Plain acc)
                    TargetPositions =
                        outpost.TargetPositions
                        |> Map.add "src-out2" { tile with Y = 43 }
                        |> Map.add "can-out2" tile
                }
    }

[<Tests>]
let outpostHaulTests =
    testList
        "the outpost's container in the hauler quota"
        [
            test "an outpost container hires haul capacity, priced at its own room's rate" {
                // ADR 0042's hauler half, which #127 could not reach: the
                // quota folds every projected room's containers, and the
                // round trip it prices this one at is the Seam join
                // (`Atlas.haulRoundTripTicks`), 51 ticks over this
                // corridor. At the 600 bank the hauler row carries 400, so
                // held the rock ships ten a tick and hires ceil(51 x 10 /
                // 400) = 2, and unheld it ships five and hires 1.
                //
                // Pairwise, one rival at a time: the two colonies differ in
                // who holds W1N2 and in nothing else.
                let held control =
                    quotaOf (haulHome |> withHaulOutpost (Some control))

                Expect.equal
                    (quotaOf haulHome)
                    0
                    "the premise: without the outpost there is no haul"

                // The rate is read off the demand and not off the quota: since
                // #279 a haul that crosses a Seam is floored at two bodies, so
                // both rocks hire two and the quota can no longer see which of
                // them ships ten a tick. What the rate moves is the sum the
                // quota divides, and that is what this case is about.
                let shipped control =
                    haulDemandOf (haulHome |> withHaulOutpost (Some control))

                Expect.equal
                    (shipped (reservedRoom true 4000))
                    510
                    "reserved, the rock ships ten a tick"

                Expect.equal
                    (shipped neutralRoom)
                    255
                    "held by nobody it ships five, and the sum the quota divides is halved with it"

                Expect.equal
                    (held (reservedRoom true 4000))
                    2
                    "and either way the crossing is never one body's to lose (#279)"

                Expect.equal
                    (held neutralRoom)
                    2
                    "including the neutral rock, whose overflow decays the same"

                // #279's floor, and the premise it rests on: a crossing worth
                // half a load or more is never one body's to lose. What a full
                // container at home does is wait; what a full one out here does
                // is drop the Anchor's next fifty on the floor to decay, forty
                // tiles from the replacement. Live it was 1,170 energy-ticks
                // against a 1,200 load — one body at 97.5% of itself.
                Expect.isTrue
                    (shipped (reservedRoom true 4000) * 2 >= 400)
                    "the premise: the crossing is worth half a 400 load or more"

                Expect.equal
                    (held ownedRoom)
                    (held (reservedRoom true 4000))
                    "owned or reserved by us is one rate, as the engine pays it"

                Expect.equal
                    (quotaOf (haulHome |> withHaulOutpost None))
                    0
                    "and a room the colony cannot see prices no rock at all (ADR 0004)"
            }

            test "two containers across the same Seam are one sum, rounded once" {
                // Acceptance criterion 1's own geometry (#194): the
                // arithmetic case `haulRoundingTests` carries has no Seam
                // in it, so until here nothing pooled *two* cross-room
                // terms and a join that mispriced the second one would
                // have moved no number in the suite.
                //
                // Both rocks are held, so each ships ten a tick, and the
                // 600 bank's hauler carries 400 — a body per 40 ticks of
                // round trip. The two crossings below come to 51 and 63,
                // so the colony's haul is 2.85 bodies and hires three; a
                // ceiling apiece bought 1.275 → 2 and 1.575 → 2 and
                // hired four.
                let colony =
                    haulHome
                    |> withHaulOutpost (Some(reservedRoom true 4000))
                    |> withSecondHaulContainer { X = 33; Y = 44 }

                let atlas = Atlas.ofView colony

                // The body the quota itself divides by — this fixture's
                // 600 bank, not `haulRoundingBody`'s 300 (#208). Road
                // parity holds at every size so the ticks are the same
                // either way, and asserting them off the row's own cast is
                // what keeps the premise and the quota one arithmetic.
                let roundTrip from =
                    Atlas.haulRoundTripTicks
                        atlas
                        (bodyFor haulerPattern 600)
                        (RoomPos.at "W1N2" from)
                        (RoomPos.at "W1N1" { X = 25; Y = 10 })

                Expect.equal
                    (roundTrip { X = 25; Y = 41 })
                    (Some 51)
                    "the premise: the near container's Seam crossing"

                Expect.equal
                    (roundTrip { X = 33; Y = 44 })
                    (Some 63)
                    "the premise: and the branch container's, eight steps out along the branch"

                Expect.equal (quotaOf colony) 3 "the two crossings are summed and rounded once"
            }

            test "one crossing container's longer haul moves the pair by a body" {
                // The pairwise half of the criterion, on the shape it
                // names: the branch container's round trip goes from 63 to
                // 72 ticks, which is 1.575 of a body to 1.8 — its own
                // ceiling is two either way, so under the old rule the
                // move was invisible. Pooled, the colony goes from 2.85 to
                // 3.075 and hires the body, because the fraction the haul
                // grew by is now spent rather than already bought.
                let atTile tile =
                    haulHome
                    |> withHaulOutpost (Some(reservedRoom true 4000))
                    |> withSecondHaulContainer tile

                Expect.equal
                    (quotaOf (atTile { X = 33; Y = 44 }))
                    3
                    "the premise: eight steps down the branch hires three"

                Expect.equal
                    (quotaOf (atTile { X = 36; Y = 44 }))
                    4
                    "three steps further out is one body more"
            }

            test "a container in a room the projection does not carry hires nobody" {
                // ADR 0004 at the fold's own edge: the container is in the
                // kind census, the colony holds the room, and the
                // projection places neither the container nor its rock —
                // so there is no tile to flood from and no room to flood
                // over. Nothing, and never the home room's arithmetic run
                // over an outpost's coordinates.
                let seen = haulHome |> withHaulOutpost (Some(reservedRoom true 4000))

                let unprojected =
                    { seen with
                        Spatial =
                            { seen.Spatial with
                                Rooms = Map.remove "W1N2" seen.Spatial.Rooms
                            }
                    }

                Expect.equal (quotaOf seen) 2 "the premise: projected, the container hires two"
                Expect.equal (quotaOf unprojected) 0 "unprojected, the same census hires none"
            }

            test "a quota memoised while the outpost was held is not handed back when it lapses" {
                // #127's memo case, in the room it was written for. The
                // hauler quota rides the census memo (ADR 0017) and now
                // reads a *second* room's held rate, so the census
                // signature had to widen to sign every projected room's —
                // and this is what the widening buys. Every census input
                // below is byte-identical between the two views: the
                // reservation is the only thing that moved.
                let lapsed = haulHome |> withHaulOutpost (Some neutralRoom)

                let previous =
                    (decide
                        (haulHome |> withHaulOutpost (Some(reservedRoom true 4000)))
                        Map.empty
                        Set.empty
                        None)
                        .Memo

                // Read off the demand and not the quota: since #279 both rates
                // hire two bodies across a Seam, so the quota can no longer
                // tell a recomputed answer from a handed-back one. The sum the
                // quota divides still halves with the rate, and that is what
                // the memo either recomputes or wrongly keeps.
                let shipped (memo: PlanMemo) =
                    memo.HaulerDemand |> List.sumBy (fun row -> row.Demand)

                Expect.equal
                    (shipped previous)
                    510
                    "the premise: held, the container ships ten a tick"

                let recalled = decide lapsed Map.empty Set.empty (Some previous)
                let fresh = decide lapsed Map.empty Set.empty None

                Expect.equal (shipped fresh.Memo) 255 "the premise: lapsed, it ships five"

                Expect.equal
                    (shipped recalled.Memo)
                    (shipped fresh.Memo)
                    "the stale memo recomputes rather than handing back the held rate's haul"

                Expect.equal
                    (spawnIntents recalled.Intents)
                    (spawnIntents fresh.Intents)
                    "so the fleet the colony casts is the one the halved haul asked for"
            }

            test
                "the quota memoised before an outpost container stood is not handed back once it does" {
                // The other half of the widening, and the one the rate
                // above cannot reach: the census entry itself. The
                // reservation case moves `held`, which is signed per room;
                // this one moves nothing but whether `can-out` stands in
                // W1N2, which only the *standing* census spanning every
                // projected room can see. Joined against the home layer
                // alone the two views sign one string, so the colony
                // would recall the container-less nothing for ever and ADR
                // 0042's switch would never fire.
                let standing = haulHome |> withHaulOutpost (Some(reservedRoom true 4000))
                let before = standing |> beforeHaulContainer

                let previous = (decide before Map.empty Set.empty None).Memo

                Expect.equal
                    previous.HaulerQuota
                    0
                    "the premise: with no container on the Seat there is no haul to hire for"

                let recalled = decide standing Map.empty Set.empty (Some previous)
                let fresh = decide standing Map.empty Set.empty None

                Expect.equal fresh.Memo.HaulerQuota 2 "the premise: standing, it hires two"

                Expect.equal
                    recalled.Memo.HaulerQuota
                    fresh.Memo.HaulerQuota
                    "the container standing up moves the signature, so the quota is recomputed"

                Expect.equal
                    (spawnIntents recalled.Intents)
                    (spawnIntents fresh.Intents)
                    "and the fleet the colony casts is the one the new haul asked for"
            }
        ]

/// The same haul fixture with an Anchor garrisoning the outpost's
/// container — the succession ADR 0026 owes an outpost's Post as much as a
/// home one (#153). Its ticks to live are the caller's, because that is the
/// whole of what moves between two calls below.
///
/// A generalist stands at home beside it, and it is the supply floor's
/// premise rather than this fixture's subject (ADR 0050): an Anchor holds
/// one Carry and is both a standing body and a Work-heavy one, so a fleet
/// of Anchors alone can put nothing into an extension and the colony hires
/// a carrier in front of every row — which is the row these cases would
/// then read instead of the one they are about.
let private withOutpostGarrison life (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Creeps = colony.Creeps @ [ anchor "a-out" 0 50 |> withLife life; worker "w-home" 0 50 ]
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.add "w-home" { X = 25; Y = 12 } layer.CreepPositions
                })
            |> withNeighbour
                "W1N2"
                { outpost with
                    CreepPositions = Map.add "a-out" { X = 25; Y = 41 } outpost.CreepPositions
                }
    }

/// The colony the cases below read: the haul fixture's two rooms, the
/// outpost held and its container standing, and one Anchor on that
/// container with the given life left.
///
/// Banked back down to 300, which is the bank the leads below are counted
/// at: a [[lead]] is the replacement's own cast time and walk (ADR 0026),
/// both read off the body this bank casts, and `haulHome` banks 600 for a
/// reason that is the *quota's* and not this fixture's (#208). Spelled
/// here rather than inherited so a case that reads a tick count says which
/// body it counted.
let private outpostSuccession life =
    { haulHome with Bank = bank 300 300 }
    |> withHaulOutpost (Some(reservedRoom true 4000))
    |> withOutpostGarrison life

[<Tests>]
let outpostSuccessionTests =
    testList
        "an outpost's Anchor and its lead"
        [
            test "an Anchor a room away is expiring, and its replacement is cast before it dies" {
                // The reproduction #153 opens on. Until it, a lead was
                // priced off the home room's flood alone, so a creep the
                // home room did not place answered 0 and was never expiring
                // — an outpost's garrison held its Post to the last tick,
                // its successor was cast only once it was dead, and the Post
                // stood empty for the cast plus the crossing in every
                // 1,500-tick life while the workforce target went on hiring
                // against the source's nominal output (ADR 0042).
                //
                // Priced over the border the lead is countable a tile at a
                // time. The Anchor row at this 300 bank is two Work over a
                // Carry and a Move (`anchorBodyFor`), so twelve ticks in the
                // spawner and four cost units — two ticks — a plain step.
                // The replacement is born on (25,9), walks eight tiles up to
                // (25,1), steps onto the exit at (25,0), is moved to (25,49)
                // for nothing, steps off onto (25,48) and walks seven more
                // down to the container at (25,41): sixteen tiles of ground
                // at two ticks each, plus the plain exit's own two — 34 of
                // walking and a lead of 46.
                Expect.equal
                    (castNames (outpostSuccession 1500))
                    (castNames (outpostSuccession 47))
                    "one tick outside its lead the garrison still counts, exactly as a fresh one does"

                match castNames (outpostSuccession 47) with
                | [ name ] ->
                    Expect.stringStarts name "hauler-" "the premise: the Anchor row is filled"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match castNames (outpostSuccession 46) with
                | [ name ] ->
                    Expect.stringStarts
                        name
                        "anchor-"
                        "at its lead the outpost's row is short and the successor is cast"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match castNames (outpostSuccession 1) with
                | [ name ] ->
                    Expect.stringStarts
                        name
                        "anchor-"
                        "and a tick from death it is being replaced, not mourned"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "an outpost no crossing reaches leads nobody, however little life is left" {
                // Total (ADR 0004) at the seam the border is joined on: with
                // no ring in the projection the two rooms share no Seam
                // band, so there is no walk to price and no lead — and a
                // lead of 0 leaves the garrison counted living to its last
                // tick, which is the answer unpriceable geometry has always
                // had. Never an arbitrary number, and never the home room's
                // arithmetic run over an outpost's coordinates.
                let unbordered life =
                    let colony = outpostSuccession life

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Borders = Map.empty
                            }
                    }

                Expect.equal
                    (castNames (unbordered 1))
                    (castNames (unbordered 1500))
                    "the same colony casts the same body whether the garrison is dying or fresh"

                // A worker and not a hauler, because the same missing band
                // leaves the container's round trip unpriceable and its
                // haul unhired (ADR 0004, `outpostHaulTests`). What this
                // case reads is the row it is *not*: the Anchor row is
                // filled, so the garrison is still counted living.
                match castNames (unbordered 1) with
                | [ name ] ->
                    Expect.stringStarts name "worker-" "and the Anchor row reads as filled"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }
        ]

/// The posted outpost with the hostiles the caller names standing in it (ADR
/// 0056): `haulHome`'s two rooms, the outpost held and its container standing,
/// one [[anchor]] garrisoning that Post and two [[hauler unit]]s on the ground
/// beside it.
///
/// A **new** builder beside the outpost fixtures above and never a Threat
/// added to one of them: every pin those carry is a quiet colony's, and a
/// [[reach]] laid over the shared fixture would re-baseline all of them at
/// once. Handed the hostiles rather than holding them, so the same geometry
/// answers the quiet tick and the raided one and a case can read the two
/// pairwise — the one hostile is the only thing that moves between them.
///
/// Its outpost is a field eleven tiles wide, x 20..30 and y 41..48, where
/// `withHaulOutpost` lays one corridor — because a raid needs somewhere to run
/// *to*: a `smallMelee` standing at (25,42) reaches five tiles around it, so
/// every row of that field but the last is inside the Reach and the y = 48 row
/// is the safe set [[flee]] walks the crew onto. Down a one-tile corridor
/// every walkable tile would be inside the Reach, Flee would price as
/// unreachable, and the case would read "nobody fled" for a reason that is
/// this fixture's and not the colony's.
///
/// Held rather than neutral, as the fixtures above are: a rate moves quotas
/// and these cases are about the Tasks.
let private raidedOutpost (hostiles: HostileInfo list) =
    let colony = haulHome |> withHaulOutpost (Some(reservedRoom true 4000))
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Creeps = [ anchor "a-out" 0 50; hauler "h-out1" 0 100; hauler "h-out2" 0 100 ]
        Hostiles = hostiles
        Spatial =
            { colony.Spatial with
                Stores = Map.add "can-out" 1000 colony.Spatial.Stores
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    Terrain =
                        Map.ofList
                            [
                                for x in 20..30 do
                                    for y in 41..48 -> { X = x; Y = y }, Plain
                            ]
                    CreepPositions =
                        Map.ofList
                            [
                                "a-out", { X = 25; Y = 41 }
                                "h-out1", { X = 24; Y = 42 }
                                "h-out2", { X = 26; Y = 42 }
                            ]
                }
    }

/// The raid itself: one invader standing a tile below the Post, on the ground
/// its crew works from. Filed under the outpost's own room, because a Reach is
/// filed under the room the Threat stands in and a hostile carrying another
/// room's name would take no tile here at all (ADR 0041, #138).
let private raiders = [ hostileIn "W1N2" { X = 25; Y = 42 } smallMelee ]

[<Tests>]
let raidedOutpostTests =
    testList
        "an outpost with an armed hostile standing in it"
        [
            test "the crew flees and the garrison, which cannot, is matched to nothing at all" {
                // What a raid costs an outpost today, before ADR 0056's
                // guard row exists to answer it: the [[hauler unit]]s run,
                // the [[anchor]] cannot and stays, and every Task worked
                // from ground inside the [[reach]] is inapplicable to
                // everyone standing there.
                //
                // Pairwise against the same fixture with no hostile in it,
                // one fact apart: the quiet tick is the yardstick, so a
                // difference below is the raid's and can be nothing else.
                let assignmentsOf hostiles =
                    (decide (raidedOutpost hostiles) Map.empty Set.empty None).Assignments

                let quiet = assignmentsOf []
                let raided = assignmentsOf raiders

                Expect.equal
                    (Map.tryFind "a-out" quiet)
                    (Some(taskId (Harvest "src-out")))
                    "the premise: on a quiet tick the garrison digs the rock it stands on"

                Expect.isEmpty
                    (quiet |> Map.filter (fun _ tid -> tid = taskId Flee))
                    "and nobody runs from a room with nothing in it (ADR 0033)"

                Expect.equal
                    (Map.tryFind "h-out1" quiet, Map.tryFind "h-out2" quiet)
                    (Some(taskId (Withdraw "can-out")), Some(taskId (Withdraw "can-out")))
                    "and the crew empties the container that Post feeds"

                Expect.equal
                    (Map.tryFind "h-out1" raided, Map.tryFind "h-out2" raided)
                    (Some(taskId Flee), Some(taskId Flee))
                    "both haulers drop that haul for Flee: the Safety tier outranks every other"

                // Neither Flee nor work: Flee is inapplicable to a
                // work-heavy body (ADR 0033), and every Seat of its rock —
                // all three of them, the row of field under it — lies
                // inside the Reach, so the one row that cannot run is left
                // holding nothing on the tile it is being killed on. That is
                // the hole ADR 0056 casts a guard into, pinned here as the
                // behaviour that decision is measured against.
                Expect.equal
                    (Map.tryFind "a-out" raided)
                    None
                    "and the Anchor is matched to nothing at all: it neither runs nor digs"
            }

            test "the garrison's Harvest is released Threatened rather than simply lost" {
                // The other half of the same tick, read off the Verdicts
                // with the assignment the quiet tick made already held: a
                // Task whose whole Work Area is in a Reach is released
                // under its own reason, so an operator reading the
                // transition log tells a raid from a Task that vanished
                // (ADR 0033). Every Seat of `src-out` — the three tiles of
                // the row under it — is inside this raid's Reach, and the
                // container the haul reads is on one of them.
                let releasesOf hostiles =
                    let held =
                        Map.ofList
                            [
                                "a-out", taskId (Harvest "src-out")
                                "h-out1", taskId (Withdraw "can-out")
                            ]

                    (decide (raidedOutpost hostiles) held Set.empty None).Verdicts
                    |> List.choose (function
                        | Verdict.Released(creep, task, reason) -> Some(creep, task, reason)
                        | _ -> None)

                Expect.isEmpty (releasesOf []) "the premise: a quiet tick releases nobody"

                Expect.equal
                    (releasesOf raiders)
                    [
                        "a-out", taskId (Harvest "src-out"), ReleaseReason.Threatened
                        "h-out1", taskId (Withdraw "can-out"), ReleaseReason.Threatened
                    ]
                    "the raid takes the rock's Seats and the container's ground, and says so"
            }
        ]

/// The raided outpost as a **declared** one: its controller projected beside
/// the rock, at a corner of the same field. That is what makes W1N2 a declared
/// [[outpost]] — the guard row hires per declared outpost and the Guard is
/// pooled per declared outpost, and a room carrying no controller of its own is
/// no candidate outpost at all (ADR 0042, ADR 0056). The builders above leave
/// it out because their subject is the haul, and it arrives here rather than
/// there for the reason `raidedOutpost` was cut beside them: a controller in
/// the projection pools a Reserve, and every quiet pin above would re-baseline
/// on it.
let private declaredRaid hostiles =
    let colony = raidedOutpost hostiles
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Spatial =
            { colony.Spatial with
                TargetKinds = Map.add "ctl-out" Controller colony.Spatial.TargetKinds
            }
            |> withNeighbour
                "W1N2"
                { outpost with
                    TargetPositions = Map.add "ctl-out" { X = 30; Y = 48 } outpost.TargetPositions
                }
    }

/// The same raided outpost with guards of ours standing in it, each on the
/// tile the case names: our own bodies are placed in the layer of the room they
/// stand in (ADR 0041), and a guard the projection places nowhere stands in no
/// room at all. Built **on top of** `declaredRaid` and never inside it, for
/// the reason that builder was cut from the quiet fixtures in the first place:
/// every pin above is a raid nobody answers, and this is the one that is
/// answered.
let private withGuards (ours: (CreepInfo * Pos) list) (colony: ColonyView) =
    let outpost = SpatialInfo.layerOf colony.Spatial "W1N2"

    { colony with
        Creeps = colony.Creeps @ List.map fst ours
        Spatial =
            colony.Spatial
            |> withNeighbour
                "W1N2"
                { outpost with
                    CreepPositions =
                        (outpost.CreepPositions, ours)
                        ||> List.fold (fun tiles (creep, pos) -> Map.add creep.Name pos tiles)
                }
    }

/// The tile a body the guard row casts stands on: `haulHome`'s corridor is one
/// wide and its spawn plugs it at (25,10), so (25,9) is the one tile beside the
/// spawn on the outpost's own side — the oven's doorstep, and every step from
/// here to the fight is a step toward the [[seam]].
let private atSpawn = { X = 25; Y = 9 }

/// The same raided outpost with one body of ours standing **at home**, on the
/// tile the caller names — `atSpawn` above for both of its readers. The
/// [[guard]] row hires at the spawn (ADR 0056 decision 1), so this and not
/// `withGuards` is where every guard the colony really buys begins its life;
/// and it is where the body #147 watched cross the [[seam]] into a raid begins
/// its too, the two cases being one geometry and two bodies. One room and one
/// Seam from the fight, which is the whole of what a Task has to carry a body
/// over.
let private withBodyAtHome (creep: CreepInfo) (pos: Pos) (colony: ColonyView) =
    { colony with
        Creeps = colony.Creeps @ [ creep ]
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    CreepPositions = Map.add creep.Name pos layer.CreepPositions
                })
    }

/// A `smallHealer` of the raid's, standing well off the fight at (22,47): it is
/// no [[threat]], so it takes no ground and adds no ring tile, and the only
/// thing it moves is the count rule's arithmetic (ADR 0056). Each one carries
/// an id of its own, a raid being a roster and not one creep.
let private healers count =
    [
        for i in 1..count ->
            { hostileIn "W1N2" { X = 22; Y = 47 } smallHealer with
                Id = $"heal-{i}"
            }
    ]

/// This tick's pool for a colony, the two Planner halves in the order `decide`
/// runs them — what reads a Task's [[priority]] and [[capacity]] without
/// asking who won it.
let private pooledOf colony =
    let atlas = Atlas.ofView colony
    planPool colony atlas (planTasks colony (threatsOf colony atlas))

let private entryFor task pool =
    pool |> List.tryFind (fun (entry: PooledTask) -> entry.Task = task)

/// The [[guard]]'s two acts, read off the tick's Intents (ADR 0056).
let private attacksOf intents =
    intents
    |> List.choose (function
        | AttackCreep(name, hostile) -> Some(name, hostile)
        | _ -> None)

let private healsOf intents =
    intents
    |> List.choose (function
        | HealCreep(name, target) -> Some(name, target)
        | _ -> None)

/// The rows a verbose scoring rejected for one creep, in pool order.
let private rejectionsFor name verdicts =
    verdicts
    |> List.tryPick (function
        | Verdict.Scoring(creep, rows) when creep = name ->
            rows
            |> List.choose (function
                | Candidate.Rejected(task, reason) -> Some(task, reason)
                | Candidate.Scored _ -> None)
            |> Some
        | _ -> None)

/// The ring tile the fixture's cases stand a guard on: south-west of the
/// invader at (25,42), inside its range-1 ring and so inside the Work Area —
/// and **free**, which the tiles due west and east of the invader are not. The
/// engine puts no two bodies on one tile and the Atlas's occupancy grid cannot
/// say that it did, so a guard stood on `h-out1`'s own (24,42) would price the
/// two of them as one body and walk these cases over a census the live colony
/// never sees.
let private beside = { X = 24; Y = 43 }

/// A second free ring tile of the same invader, for the cases that stand two
/// guards up — south-east where `beside` is south-west, and `h-out2`'s (26,42)
/// left to `h-out2`.
let private besideToo = { X = 26; Y = 43 }

[<Tests>]
let guardTaskTests =
    testList
        "the Guard of a raided outpost"
        [
            test "the raid pools one Guard, keyed on the room and ranked with Flee" {
                // ADR 0056 decision 2 at the Planner's seam: one Task per
                // declared [[outpost]] a [[threat]] stands in, keyed on the
                // **room** — `guard:W1N2` and never the invader's id, so the
                // 2% multi-creep raid pools one Task and not five. Pairwise
                // against the same geometry with nothing in it: what moves
                // between the two calls is the raid.
                let quiet = pooledOf (declaredRaid [])
                let raided = pooledOf (declaredRaid raiders)

                Expect.isNone
                    (entryFor (Guard "W1N2") quiet)
                    "the premise: a quiet outpost is no fight and pools none"

                Expect.equal
                    (raided
                     |> List.map (fun entry -> taskId entry.Task)
                     |> List.filter (fun id -> id.StartsWith "guard:")
                     |> List.distinct)
                    [ "guard:W1N2" ]
                    "the raid pools exactly one, under the room's own name"

                // The Safety tier's [[priority]] with no rung of its own: the
                // two Tasks of that tier carry one number, and nothing ever
                // asks how they order.
                Expect.equal
                    (raided |> entryFor (Guard "W1N2") |> Option.map (fun e -> e.Priority))
                    (raided |> entryFor Flee |> Option.map (fun e -> e.Priority))
                    "and it ranks exactly where Flee does — Safety, no rung"
            }

            test "the cap is the room's own quota, one guard and then two" {
                // The [[capacity]] is decision 2's "one number computed once
                // and read as both the row's quota and the Task's cap": the
                // Fighter share is `guardsWanted` for that room, so the bodies
                // the cascade hires are the bodies the Task admits. Read
                // pairwise off the raid alone — a lone `smallMelee` against the
                // same raid carrying two healers, whose 120 out-heals the 90
                // one `guardPattern` block deals (#272), with one guard of
                // ours standing in both readings so that what moves between
                // them is the raid and nothing of ours.
                let capOf colony =
                    pooledOf colony
                    |> entryFor (Guard "W1N2")
                    |> Option.map (fun entry -> entry.Capacity.Fighters)

                Expect.equal
                    (capOf (declaredRaid raiders |> withGuards [ guard "g-1", beside ]))
                    (Some(Some 1))
                    "a raid that heals nothing admits one Fighter"

                Expect.equal
                    (capOf (
                        declaredRaid (raiders @ healers 2) |> withGuards [ guard "g-1", beside ]
                    ))
                    (Some(Some 2))
                    "and a raid healing 120 against the 90 one guard block deals admits the second"
            }

            test "a Fighter standing in the raided room is matched to the Guard" {
                // Acceptance, and the hole ADR 0056 was written to fill: the
                // room the [[hauler unit]]s run out of and the [[anchor]] is
                // killed in now has one body whose Task is the fight. Pairwise
                // against the same guard standing in the same room with nothing
                // to fight, which is matched to nothing at all — a guard is
                // applicable to no other work in the pool.
                let raided =
                    decide
                        (declaredRaid raiders |> withGuards [ guard "g-1", beside ])
                        Map.empty
                        Set.empty
                        None

                let quiet =
                    decide
                        (declaredRaid [] |> withGuards [ guard "g-1", beside ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "g-1" quiet.Assignments)
                    None
                    "the premise: with no raid there is no Guard, and no other Task takes a body with no Work and no Carry"

                Expect.equal
                    (Map.tryFind "g-1" raided.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the raid gives it the one Task it is for"

                Expect.equal
                    (Map.tryFind "h-out1" raided.Assignments)
                    (Some(taskId Flee))
                    "and the crew still runs: the guard answers the raid, it does not cancel it"

                Expect.contains
                    (sayIntents raided.Intents)
                    ("g-1", "⚔️")
                    "the bubble carries the Guard's own glyph"
            }

            test "the Emitter swings at range 1 and heals every tick" {
                // ADR 0056 decision 2's Emitter, pairwise on one tile: 30 a
                // part is paid at range 1 and nothing at range 2, so the swing
                // is gated on the range and the self-heal is not — the row's
                // one HEAL part is spent every tick the body holds the Task,
                // walking or fighting.
                let intentsFrom tile =
                    (decide
                        (declaredRaid raiders |> withGuards [ guard "g-1", tile ])
                        Map.empty
                        Set.empty
                        None)

                let inSwing = intentsFrom beside

                // Off the raid's ground entirely — the y = 48 row this
                // fixture's own [[flee]] cases run onto, so the body is
                // outside the [[reach]] and four tiles from the invader.
                let walking = intentsFrom { X = 28; Y = 48 }

                Expect.equal
                    (attacksOf inSwing.Intents)
                    [ "g-1", "h-1" ]
                    "standing on the invader's ring, the guard swings at it"

                Expect.equal
                    (healsOf inSwing.Intents)
                    [ "g-1", "g-1" ]
                    "and heals itself in the same tick, the two being different acts"

                Expect.isEmpty
                    (attacksOf walking.Intents)
                    "four tiles away it swings at nothing: the act is not issued out of range"

                Expect.equal
                    (healsOf walking.Intents)
                    [ "g-1", "g-1" ]
                    "and heals all the same — every tick, not every arrival"

                Expect.equal
                    (Map.tryFind "g-1" walking.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the body still holds the Task it is walking to"

                Expect.isNonEmpty
                    (moveIntentsFor "g-1" walking.Intents)
                    "and the mover walks it there: the Emitter issues no movement of its own"
            }

            test
                "with two Threats the one beside the Post is the target, and with no Post the nearest" {
                // "Between the invader and the [[anchor]]" said in this
                // colony's vocabulary (ADR 0056): the Threat nearest a [[post]]
                // of that room, ties by id — the engine's own `findAttack.js`
                // chases the closest hostile by path, so the guard on that
                // invader's ring is between it and everything behind it.
                //
                // Two invaders, both within range 1 of the guard at (26,43):
                // `inv-2` at (25,42) stands a tile from the Post at (25,41),
                // `inv-1` at (27,44) three tiles from it — and the ids run the
                // other way, so a target chosen by id alone would name `inv-1`
                // in every reading below.
                let invader id pos =
                    { hostileIn "W1N2" pos smallMelee with
                        Id = id
                    }

                let raid =
                    [ invader "inv-2" { X = 25; Y = 42 }; invader "inv-1" { X = 27; Y = 44 } ]

                let attacksFrom tile colony =
                    (decide (colony |> withGuards [ guard "g-1", tile ]) Map.empty Set.empty None)
                        .Intents
                    |> attacksOf

                let posted = declaredRaid raid
                // The same room the tick before its container stands: no
                // container, no Post, and so nothing to stand in front of.
                let postless = beforeHaulContainer posted

                Expect.equal
                    (attacksFrom { X = 26; Y = 43 } posted)
                    [ "g-1", "inv-2" ]
                    "the Post decides it, over an id order that says otherwise"

                Expect.equal
                    (attacksFrom { X = 26; Y = 43 } postless)
                    [ "g-1", "inv-1" ]
                    "with no Post the guard's own tile decides, and equal distances tie by id"

                Expect.equal
                    (attacksFrom beside postless)
                    [ "g-1", "inv-2" ]
                    "which is a distance and not the id: one tile away wins over three"

                // **The range gate is on the candidates, not on the pick.**
                // (28,44) is a ring tile of `inv-1` and three from `inv-2`, so
                // the Threat nearest the Post is out of reach and the other one
                // is beside the body dealing 40 a tick. Ordered the ADR's
                // sentence literally — nearest the Post, then filtered by range
                // — the guard would swing at nothing here for as long as it
                // stood, which is not what "30 a part is paid at range 1" is a
                // reason for.
                Expect.equal
                    (attacksFrom { X = 28; Y = 44 } posted)
                    [ "g-1", "inv-1" ]
                    "out of reach of the Post's own invader, the guard hits the one it can reach"
            }

            test "the cap refuses the second Fighter while the room wants one" {
                // The [[capacity]] counted at the Matcher (ADR 0052 decision
                // 6): the room wants one guard this tick, so the second body
                // standing in the same ring is refused by the number the
                // Planner set — named, so a verbose reading tells "the room is
                // full" from "this body cannot fight".
                let colony =
                    declaredRaid raiders
                    |> withGuards [ guard "g-1", beside; guard "g-2", besideToo ]

                let decision = decide colony Map.empty (Set.singleton "g-2") None

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the first Fighter takes the fight"

                Expect.notEqual
                    (Map.tryFind "g-2" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and the second does not hold it beside him"

                Expect.contains
                    (rejectionsFor "g-2" decision.Verdicts |> Option.defaultValue [])
                    (taskId (Guard "W1N2"), RejectReason.CapacityFull)
                    "the cap is what refused it, and the scoring says so"
            }

            test "the escalation does not evict the reinforcement it just bought" {
                // #272, ADR 0056 decision 1 as amended: the count is a
                // function of the **raid** — the healing per tick against the
                // damage of the body the row would cast — so it cannot retract
                // on the arrival of the body it asked for. Read on the
                // reproduction that found it. Priced against the guards
                // standing there, the same two-healer raid admitted two while
                // one guard stood and one the tick the second arrived, and the
                // arriving body was `CapacityFull`-evicted onto a Flee whose
                // safe set is this same room: it never left, so its own damage
                // held the count at one and the colony had bought 750 energy
                // of body that never issues an `AttackCreep`, for as long as
                // it lived. Pairwise against the case above, whose raid heals
                // nothing and where the second body is refused for good. The
                // damage the healing is measured against is one block of the
                // row's own body, a constant, so this holds at whatever the
                // fixture banks.
                let capOf colony =
                    pooledOf colony
                    |> entryFor (Guard "W1N2")
                    |> Option.map (fun entry -> entry.Capacity.Fighters)

                let raid = declaredRaid (raiders @ healers 2)

                Expect.equal
                    (capOf (raid |> withGuards [ guard "g-1", beside ]))
                    (Some(Some 2))
                    "the premise: 120 healed against the 90 one guard block deals admits two"

                Expect.equal
                    (capOf (raid |> withGuards [ guard "g-1", beside; guard "g-2", besideToo ]))
                    (Some(Some 2))
                    "and the second guard standing in the ring does not close the cap behind it"

                let decision =
                    decide
                        (raid |> withGuards [ guard "g-1", beside; guard "g-2", besideToo ])
                        Map.empty
                        (Set.singleton "g-2")
                        None

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the first Fighter keeps the fight"

                Expect.equal
                    (Map.tryFind "g-2" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and the second holds it beside him rather than fleeing the room it is standing in"

                Expect.isEmpty
                    (rejectionsFor "g-2" decision.Verdicts
                     |> Option.defaultValue []
                     |> List.filter (fun (task, reason) ->
                         task = taskId (Guard "W1N2") && reason = RejectReason.CapacityFull))
                    "with nothing left to reject it for: the cap is the raid's number and it did not move"
            }

            test "a hauler and a worker are refused the fight they are standing in" {
                // The other half of the [[capacity]] sentence — `Fighter -> the
                // room's quota, every other class 0` — read where it is asked
                // first: the body gate (ADR 0056's applicability clause) shuts
                // every body with no ATTACK part out before the number is ever
                // counted, so neither the crowd that runs from a raid nor the
                // [[anchor]] that cannot run can be matched into it. Both
                // classes, because both stand in this room and the acceptance
                // names both: a [[hauler unit]] is a `Carrier`, the Anchor a
                // `Heavy`, and the `Fighters` share admits neither.
                let colony = declaredRaid raiders |> withGuards [ guard "g-1", beside ]

                let decision = decide colony Map.empty (Set.ofList [ "h-out1"; "a-out" ]) None

                Expect.contains
                    (rejectionsFor "h-out1" decision.Verdicts |> Option.defaultValue [])
                    (taskId (Guard "W1N2"), RejectReason.Inapplicable)
                    "no ATTACK part, no fight"

                Expect.contains
                    (rejectionsFor "a-out" decision.Verdicts |> Option.defaultValue [])
                    (taskId (Guard "W1N2"), RejectReason.Inapplicable)
                    "and the work-heavy body standing on the Post is refused it too, Work being no weapon"

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "while the one body that carries one holds it"
            }

            test "the guard the row cast at home is priced across the Seam and walks it" {
                // **The body every guard really is.** The row hires at the
                // spawn (ADR 0056 decision 1), so the guard the colony buys
                // begins a room and a [[seam]] away from the ring it was bought
                // for, and decision 2 gives it "no movement of its own — the
                // mover walks it into the Work Area like any other Task". That
                // is a claim about the *price*: a Work Area filed under the
                // raided room and priced over the creep's own room alone would
                // reject this body `Unreachable` on every tick of its 1,500,
                // and its non-decaying `Living` would suppress the next cast —
                // 750 energy standing at the oven while the outpost is emptied.
                //
                // Pairwise against the same body inside the room, which is what
                // every other case here stands: what moves between the two
                // readings is the border, and the answer must not.
                let across =
                    decide
                        (declaredRaid raiders |> withBodyAtHome (guard "g-home") atSpawn)
                        Map.empty
                        (Set.singleton "g-home")
                        None

                let inside =
                    decide
                        (declaredRaid raiders |> withGuards [ guard "g-home", beside ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "g-home" inside.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "the premise: standing on the ring, the body holds the fight"

                Expect.equal
                    (Map.tryFind "g-home" across.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "and a border away it holds the same fight — the walk is a price, not a refusal"

                Expect.isEmpty
                    (rejectionsFor "g-home" across.Verdicts
                     |> Option.defaultValue []
                     |> List.filter (fun (task, reason) ->
                         task = taskId (Guard "W1N2") && reason = RejectReason.Unreachable))
                    "and it is not rejected Unreachable: the ring across a Seam is priced over the Seam"

                // The other gate that could refuse a body a border away, read
                // off the same tick, and the ordering ADR 0056 decision 3
                // names: the cross-room threat reading (#147) sits *beneath*
                // the Safety tier, so the ring the raid laid is not read as
                // ground the raid took. A rule that judged the tier by its
                // ground would call every Guard threatened on every tick one
                // existed, and the gate that sends a body into the fight would
                // be the one thing keeping it out.
                Expect.isEmpty
                    (rejectionsFor "g-home" across.Verdicts
                     |> Option.defaultValue []
                     |> List.filter (fun (task, reason) ->
                         task = taskId (Guard "W1N2") && reason = RejectReason.Threatened))
                    "nor Threatened: its own ring is not ground the raid took from it"

                Expect.equal
                    (moveIntentsFor "g-home" across.Intents)
                    [ MoveCreep("g-home", Direction.Top) ]
                    "the mover walks it there, up the corridor toward the crossing into W1N2"
            }

            test "the room clears and the Guard goes with it: the holder is released TaskGone" {
                // The Task is a per-tick fact read off vision, exactly as the
                // row's quota is (ADR 0056): the tick nothing armed is standing
                // in that outpost the Guard leaves the pool, and its holder is
                // released under the reason that says the work itself is gone
                // rather than that a raid took its ground.
                let held = Map.ofList [ "g-1", taskId (Guard "W1N2") ]

                let releasesOf hostiles =
                    (decide
                        (declaredRaid hostiles |> withGuards [ guard "g-1", beside ])
                        held
                        Set.empty
                        None)
                        .Verdicts
                    |> List.choose (function
                        | Verdict.Released(creep, task, reason) -> Some(creep, task, reason)
                        | _ -> None)

                Expect.isEmpty
                    (releasesOf raiders)
                    "the premise: while the invader stands, the guard keeps the fight"

                Expect.equal
                    (releasesOf [])
                    [ "g-1", taskId (Guard "W1N2"), ReleaseReason.TaskGone ]
                    "and the tick it is gone the Task is gone, not merely threatened"
            }

            test
                "every other Task in the raided room reads Threatened, and the Guard is the one that does not" {
                // **The acceptance ADR 0056 decision 3 is written for**, read
                // off one verbose scoring — the one place the whole pool is
                // judged for one body, so the exemption and the rule it is an
                // exemption from are the same tick's answers.
                //
                // ADR 0033 takes every Reach out of every Work Area at
                // applicability, and this raid's Reach covers the rock's three
                // Seats and the container's ground with them. A Guard's Work
                // Area is *made* of Reach tiles — the range-1 ring of the very
                // Threat that laid them — so under that gate unamended it would
                // be inapplicable to everyone on every tick it existed, and the
                // one Task the row buys a body for would be the one Task no
                // body could ever hold. So the subtraction is skipped for the
                // **Safety tier**, both of whose areas are derived off the
                // tick's `Threats` rather than off a target's surroundings.
                //
                // Flee is the other half of that tier and is refused here for
                // the reason beside it (decision 3's first clause): the two are
                // disjoint by [[body class]], so this one body sees one Task
                // rejected for every gate the pool has and exactly one left.
                let colony = declaredRaid raiders |> withGuards [ guard "g-1", beside ]
                let decision = decide colony Map.empty (Set.singleton "g-1") None
                let rejections = rejectionsFor "g-1" decision.Verdicts |> Option.defaultValue []

                Expect.contains
                    rejections
                    (taskId (Harvest "src-out"), RejectReason.Threatened)
                    "the rock's every Seat is in the Reach, so its Harvest is gone for this body"

                Expect.contains
                    rejections
                    (taskId (Withdraw "can-out"), RejectReason.Threatened)
                    "and so is the ground the container is drawn from"

                Expect.contains
                    rejections
                    (taskId Flee, RejectReason.Inapplicable)
                    "and the tier's other Task is refused the body, not the ground: a Fighter does not run"

                Expect.isEmpty
                    (rejections |> List.filter (fun (task, _) -> task = taskId (Guard "W1N2")))
                    "the Guard is on no rejected row at all: the tier's area keeps its Reach tiles"

                Expect.equal
                    (Map.tryFind "g-1" decision.Assignments)
                    (Some(taskId (Guard "W1N2")))
                    "so the one body standing in a room where nothing else can be worked holds the fight"
            }
        ]

/// The two-room shape #147 was filed on: a body of ours **at home** and a Task
/// whose ground is a raided room across the [[seam]]. ADR 0056 decides it
/// rather than merely touching it — the reading below is the one that must sit
/// beneath the Safety tier, or it lands as the bug decision 3's exemption
/// exists to prevent. The guard half of that ordering is pinned where the
/// guard's own crossing already is, in `guardTaskTests` above: the same colony,
/// the same raid and the same tile, read for `Threatened` beside `Unreachable`.
[<Tests>]
let crossSeamThreatTests =
    testList
        "a Task across the Seam in a raided room"
        [
            test "the home worker is not sent into a raid its own crew is running out of" {
                // **#147, reproduced and fixed.** ADR 0033 makes a Task whose
                // whole Work Area lies in a Reach inapplicable to *everyone*,
                // and the word never reached a body standing in another room:
                // `threatened` read the creep-relative Work Area, which is
                // empty across a border by construction (ADR 0041), and an
                // empty area is not "threatened" but unplaceable. So on the
                // very tick this outpost's crew was fleeing off the rock's
                // Seats, a worker at home was matched to that rock and walked
                // toward the invader standing on it — a wasted crossing ending
                // in `NoneApplicable` in a room under attack.
                //
                // Pairwise on the raid and on nothing else: the same worker on
                // the same tile beside the same spawn, one hostile apart.
                let atHome hostiles =
                    decide
                        (declaredRaid hostiles |> withBodyAtHome (worker "w-home" 0 50) atSpawn)
                        Map.empty
                        Set.empty
                        None

                let quiet = atHome []
                let raided = atHome raiders

                Expect.equal
                    (Map.tryFind "w-home" quiet.Assignments)
                    (Some(taskId (Withdraw "can-out")))
                    "the premise: with the room quiet the body is offered the outpost's work and crosses for it"

                Expect.equal
                    (moveIntentsFor "w-home" quiet.Intents)
                    [ MoveCreep("w-home", Direction.Top) ]
                    "the premise is tight: that is a walk up the corridor toward the crossing"

                Expect.equal
                    (Map.tryFind "w-home" raided.Assignments)
                    None
                    "and under the raid the same Task is gone for it too: the ground is the target room's"

                Expect.isEmpty
                    (moveIntentsFor "w-home" raided.Intents)
                    "so nothing walks it across the Seam"
            }
        ]

[<Tests>]
let containerSwitchTests =
    testList
        "the container is the switch"
        [
            // Read against the fleet, one body at a time: a colony standing
            // exactly at its target casts nothing, and the same colony one
            // body short casts one — so a target that moved by n shows up as
            // n bodies and cannot hide inside a spawn's one-cast-a-tick
            // limit.
            let casts colony fleet =
                spawnIntents
                    (decide { colony with Creeps = fleet } Map.empty Set.empty None).Intents

            let short fleet =
                List.truncate (List.length fleet - 1) fleet

            test "an outpost rock with nothing built on it moves no row of the target" {
                // ADR 0042's exclusion, read forward rather than backward:
                // the room is projected, held by us and its rock is pooled
                // for Harvest, and still the colony hires exactly the fleet
                // it hired without it. Until a container stands, an outpost
                // is invisible to every quota.
                //
                // Pairwise, one rival at a time: the two colonies differ in
                // the outpost rock and in nothing else.
                Expect.isEmpty
                    (casts switchHome switchHomeFleet)
                    "the premise: six is the home room's whole target"

                Expect.hasLength
                    (casts switchHome (short switchHomeFleet))
                    1
                    "the premise is tight: one body short and the colony casts"

                Expect.equal
                    (quotaOf switchUnposted)
                    (quotaOf switchHome)
                    "the unposted rock hires no haul"

                Expect.isEmpty
                    (casts switchUnposted switchHomeFleet)
                    "and no Anchor and no worker either: the same six are the whole target"
            }

            test "the container standing is one Anchor, its own haul and its income share" {
                // The switch itself (ADR 0042). One tick's difference — a
                // container standing on the outpost rock's one Seat — and
                // the colony hires six more bodies: the Anchor for the
                // Post the container makes, the one hauler its own round
                // trip adds to the colony's rounded-once pool (ADR 0049),
                // and the four workers the rock's own output feeds once
                // those rows are amortized.
                Expect.isEmpty
                    (casts switchPosted (switchHomeFleet @ switchOutpostRows))
                    "posted, the target is the home fleet plus the outpost's own rows"

                Expect.hasLength
                    (casts switchPosted (short (switchHomeFleet @ switchOutpostRows)))
                    1
                    "and it is tight: one body short and the colony casts"

                Expect.equal
                    (quotaOf switchPosted - quotaOf switchUnposted)
                    1
                    "one of the six is what the container's own haul adds to the pool"
            }

            test "the Anchor the container hires is one, from the row the home Posts hire from" {
                // ADR 0042 pins the outpost's Anchor on the *same* row as
                // the home room's, walked to its Post by travel cost like
                // any other body — no remote-miner row, no second sizing
                // rule. So the proof is a swap at a fixed headcount: one
                // body short of the target the colony casts a worker while
                // both Anchors stand, and the same eleven bodies with the
                // outpost's Anchor spelled as a worker cast an Anchor
                // instead. Only a row gap can move between the two, because
                // the deficit is one either way.
                let shortFleet = short (switchHomeFleet @ switchOutpostRows)

                let swapped =
                    shortFleet
                    |> List.map (fun creep ->
                        if creep.Name = "a-out" then worker "w9" 0 50 else creep)

                match casts switchPosted shortFleet with
                | [ (_, _, name) ] ->
                    Expect.stringStarts
                        name
                        "worker-"
                        "the premise: with both Anchors it is a worker"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"

                match casts switchPosted swapped with
                | [ (_, _, name) ] -> Expect.stringStarts name "anchor-" "the gap is an Anchor gap"
                | other -> failtest $"expected exactly one SpawnCreep intent, got %A{other}"
            }

            test "a standing outpost container adds no Task; stocked it adds its own Withdraw" {
                // The pool's half of the switch. A rock is pooled for
                // Harvest whichever room it stands in and whether or not it
                // is posted (ADR 0041) — one pool, one ranking — so the
                // container standing adds no Task by standing. It adds one
                // the tick it holds energy, and that Task is a Withdraw on
                // the container itself: nothing here becomes a Refill
                // target, because an outpost's container is no upgrade
                // buffer of a controller a room away (ADR 0010) — the join
                // that answers that is pinned by `roomLayerTests`, on a
                // fixture that has a controller to be wrong about.
                //
                // Standing is not the container's only way into the pool,
                // and the ticket's own Trap names the other: a container is
                // a repairable kind, so once its hits fall under half its
                // max `hungryStructures` pools a cross-room `Repair` for it
                // beside this Withdraw. That is no conflict with ADR 0010 —
                // `isHungry` judges every structure against its own kind's
                // whole line, so an outpost container's decay drags no home
                // container's line with it — and nothing here is decayed.
                Expect.equal
                    (planTasks switchPosted noThreats)
                    (planTasks switchUnposted noThreats)
                    "an empty container standing changes no Task in the pool"

                let stocked =
                    { switchPosted with
                        Spatial =
                            { switchPosted.Spatial with
                                Stores = Map.add "can-out" 500 switchPosted.Spatial.Stores
                            }
                    }

                Expect.equal
                    (List.except (planTasks switchPosted noThreats) (planTasks stocked noThreats))
                    [ Withdraw "can-out" ]
                    "and stocked it adds exactly one Task, the Withdraw of its own store"

                let decayed =
                    { switchPosted with
                        Spatial =
                            { switchPosted.Spatial with
                                Hits =
                                    Map.add
                                        "can-out"
                                        { Hits = 1000; HitsMax = 2500 }
                                        switchPosted.Spatial.Hits
                            }
                    }

                Expect.equal
                    (List.except (planTasks switchPosted noThreats) (planTasks decayed noThreats))
                    [ Repair "can-out" ]
                    "and decayed it adds exactly one more, its own Repair across the Seam"
            }
        ]

/// The second outpost, across the *west* border: the declaration takes
/// two rooms at once (ADR 0042) and one of them is not enough to tell "one
/// reserver per outpost" apart from "every reserver on whichever
/// controller is nearest". Its controller is off its corridor for the same
/// reason the north one is, and the two tiles left beside it are the
/// declared shape W12S27's `37,43` really has.
let private westReserveDeclaration =
    {
        RoomName = "W2N1"
        Sources = []
        Controller = "ctrl-west", { Room = "W2N1"; X = 41; Y = 25 }
    }

/// The colony with both outposts declared and a west arm of home leading
/// to the second: home's `1,26` opens onto W2N1's `48,26` (ADR 0041 reads
/// the join out of the two room names), and the west corridor runs from
/// there to the tiles beside `ctrl-west`. The creeps stand in that arm, a
/// dozen steps from the west controller and some thirty-five from the
/// north one — so travel cost alone prefers the *same* controller for
/// every one of them, which is what makes the per-Task cap the only thing
/// that can spread them.
let private twoOutpostColony (creeps: (CreepInfo * Pos) list) =
    let colony = reserveColony []

    let spatial =
        { colony.Spatial with
            Borders = Map.add "W2N1" plainRing colony.Spatial.Borders
        }
        |> withHome (fun layer ->
            { layer with
                Terrain =
                    (layer.Terrain, [ for x in 1..10 -> { X = x; Y = 26 } ])
                    ||> List.fold (fun terrain pos -> Map.add pos Plain terrain)
                CreepPositions =
                    creeps |> List.map (fun (creep, pos) -> creep.Name, pos) |> Map.ofList
            })
        |> withNeighbour
            "W2N1"
            { RoomLayer.empty with
                Terrain = Map.ofList [ for x in 41..48 -> { X = x; Y = 26 }, Plain ]
            }
        |> Outpost.place [ westReserveDeclaration ]

    { colony with
        Creeps = creeps |> List.map fst
        Spatial = spatial
    }

[<Tests>]
let reserveTests =
    testList
        "reserve"
        [
            test
                "an outpost's controller is a Reserve; the colony's own is Upgraded, never reserved" {
                // The pool rule (ADR 0042), read off the projection's kind
                // census: every controller in it but ours. The colony's own
                // is excluded by id — the engine refuses reserveController
                // on a room it owns — so the two controllers here answer
                // the two different Tasks a controller can carry.
                let colony =
                    { bareRespawn with
                        Sources = []
                        Refillables = []
                        Spatial =
                            { SpatialInfo.empty with
                                TargetKinds =
                                    Map.ofList [ "ctrl-1", Controller; "ctrl-out", Controller ]
                            }
                    }

                let tasks = planTasks colony noThreats

                Expect.equal
                    (reserveTasks tasks)
                    [ "ctrl-out" ]
                    "the outpost's controller is the one Reserve in the pool"

                Expect.contains tasks (Upgrade "ctrl-1") "and the colony's own is still Upgraded"

                Expect.isEmpty
                    (reserveTasks (planTasks bareRespawn noThreats))
                    "a colony projecting one room reserves nothing: the pool is the pool it always was"
            }

            test "a CLAIM body is matched to the outpost's Reserve and reserves it" {
                // The whole path in one tick (ADR 0042): the Task is pooled
                // off the declaration, the CLAIM body is the one body it
                // applies to, the Matcher hands it over, and the Emitter
                // issues the reserve. The creep stands at (10,44), one tile
                // from the controller at (11,44) — inside the Work Area
                // already, so the act is this tick's and not a walk's.
                let {
                        Assignments = assignments
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decide
                        (reserveColony [ reserver "r1", { X = 10; Y = 44 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.equal
                    (Map.tryFind "r1" assignments)
                    (Some(taskId (Reserve "ctrl-out")))
                    "the reserver holds the outpost's controller"

                Expect.contains
                    verdicts
                    (Verdict.Matched("r1", taskId (Reserve "ctrl-out"), MatchFactor.OnlyCandidate))
                    "and it is the only Task in the pool it fits"

                Expect.contains
                    intents
                    (ReserveController("r1", "ctrl-out"))
                    "the Intent is the engine's reserve act, aimed at the declared controller"

                Expect.contains
                    intents
                    (SayCreep("r1", "🚩"))
                    "and the bubble carries the Reserve glyph"
            }

            test "a body with no CLAIM part is never matched to Reserve" {
                // Pairwise against the test above: the same colony, the
                // same tile beside the same controller, one body swapped.
                // A generalist can do everything else this colony ever asks
                // and cannot push a reservation up by a tick.
                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decide
                        (reserveColony [ worker "w1" 0 50, { X = 10; Y = 44 } ])
                        Map.empty
                        Set.empty
                        None

                Expect.isEmpty
                    (Map.toList assignments)
                    "the one Task in the pool asks for a part this body has none of"

                Expect.isEmpty
                    (intents
                     |> List.filter (function
                         | ReserveController _ -> true
                         | _ -> false))
                    "and nothing reserves anything"
            }

            test "a CLAIM body fits no other Task: without a Reserve it stands still" {
                // ADR 0042's pairing rule, in as many words: every other
                // Task gates on a Work part or a Carry part and a
                // `[2Claim;2Move]` body has neither, so a reserver cast
                // before this Task existed would have stood where it was
                // born for its whole 600-tick life. It is also why the
                // quota may not arrive before the Task (#131).
                let colony =
                    { bareRespawn with
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "site-1" } ]
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ reserver "r1" ]
                        Spatial =
                            { SpatialInfo.empty with
                                TargetKinds =
                                    Map.ofList
                                        [
                                            "cont-1", Structure BuiltKind.Container
                                            "pile-1", Dropped
                                        ]
                                Stores = Map.ofList [ "cont-1", 500; "pile-1", 150 ]
                            }
                    }
                    |> withHits "road-1" BuiltKind.Road 100 5000

                let pool = planTasks colony noThreats

                Expect.equal
                    (pool |> List.map taskId |> List.sort)
                    (List.sort
                        [
                            taskId (Harvest "src-a")
                            taskId (Withdraw "cont-1")
                            taskId (Pickup "pile-1")
                            taskId (Refill "spawn-1")
                            taskId (Build "site-1")
                            taskId (Repair "road-1")
                            taskId (Upgrade "ctrl-1")
                        ])
                    "the premise: every Task but Reserve and Flee is in the pool"

                let { Assignments = assignments } = decide colony Map.empty Set.empty None

                Expect.isEmpty
                    (Map.toList assignments)
                    "and the CLAIM body is applicable to none of them"
            }

            test "a reserver under fire runs: Safety outranks the tier Reserve sits on" {
                // The one comparison the tier choice actually settles
                // today. Reserve is on the feeding tier — ADR 0042's own
                // argument for casting the row first is that it decides
                // whether the income is five a tick or ten — and Safety
                // sits above every tier of work (ADR 0033), so a reserver
                // being shot at leaves the controller. Both Tasks are in
                // this creep's pool: the Reach takes two of the
                // controller's three standing tiles and leaves one, so
                // Reserve is applicable and loses on rank rather than
                // vanishing.
                let colony = reserveColony [ reserver "r1", { X = 10; Y = 44 } ]

                let raided =
                    { colony with
                        Hostiles = [ hostileIn "W1N2" { X = 10; Y = 41 } [ Attack; Move ] ]
                    }

                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide raided Map.empty Set.empty None

                Expect.equal
                    (Map.tryFind "r1" assignments)
                    (Some(taskId Flee))
                    "the reserver runs rather than holding the reservation"

                Expect.contains
                    verdicts
                    (Verdict.Matched("r1", taskId Flee, MatchFactor.Rank))
                    "and rank is what separated the two: Safety above Feeding"
            }

            test "one reserver per controller: the second is pushed to the outpost nobody holds" {
                // ADR 0042 casts one reserver *per posted outpost* — "two
                // reservers at 4.33 energy a tick buy three sources their
                // second five" — and a second body on a controller the
                // first already holds buys nothing at all, because a
                // reservation is capped and one body's CLAIM parts are
                // sized to hold it. Travel cost cannot produce that on its
                // own: both bodies stand in the west arm, both price the
                // west controller cheapest, and `load` is only the key's
                // third component, so it never separates two candidates
                // whose costs differ. The per-Task cap is what does, and
                // without it the north outpost is pooled, applicable and
                // matched by nobody for the whole 600-tick life of both
                // creeps — silently, since both report Matched.
                let colony =
                    twoOutpostColony
                        [ reserver "r1", { X = 5; Y = 26 }; reserver "r2", { X = 6; Y = 26 } ]

                let { Assignments = assignments } = decide colony Map.empty Set.empty None

                Expect.equal
                    (assignments |> Map.toList |> List.map snd |> List.sort)
                    [ taskId (Reserve "ctrl-out"); taskId (Reserve "ctrl-west") ]
                    "the two reservers hold the two declared controllers, one each"
            }

            test "a controller in a room this colony owns is not pooled at all" {
                // The other half of #181's fact, at the seam it is decided
                // on: the engine refuses reserveController on a room we
                // own, so that room's controller is not a Task. The pool
                // excluded the colony's own controller by *id*, which said
                // the same thing only while home was the only room the
                // colony owned — the tick a declared outpost is claimed it
                // stops saying it, and a Task no body can execute is one
                // the Matcher fills all the same.
                //
                // Pairwise on the one fact: the same declaration, the same
                // controller, the same projection, ownership the only
                // input that moves.
                let pooledUnder control =
                    let colony = reserveColony []

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W1N2" control
                    }
                    |> fun colony -> planTasks colony noThreats
                    |> reserveTasks

                Expect.equal
                    (pooledUnder neutralRoom)
                    [ "ctrl-out" ]
                    "a neutral outpost's controller is the Reserve it always was"

                Expect.isEmpty
                    (pooledUnder ownedRoom)
                    "the same controller, in a room this colony owns, offers a CLAIM body nothing"
            }

            test "the row's one reserver walks past the outpost we own to the one we do not" {
                // #181's live shape, at the seam the bug actually bites:
                // home, a near declared outpost the user has just claimed,
                // and a farther one still neutral. The row hires one body
                // — a room we own is not a room to reserve — and travel
                // cost alone would spend it on the near controller, which
                // is exactly the controller the engine refuses. Nothing
                // between the pool and the Matcher reads ownership, so the
                // pool is where that has to be settled, and this is the
                // test that says so: with the near room owned the body
                // must cross to the far one.
                //
                // Pairwise on ownership, one creep, so the cap cannot be
                // what spreads them: the same colony with the west room
                // neutral keeps the body on the west controller.
                let assignedUnder control =
                    let colony = twoOutpostColony [ reserver "r1", { X = 5; Y = 26 } ]

                    { colony with
                        RoomControl = colony.RoomControl |> Map.add "W2N1" control
                    }
                    |> fun colony -> decide colony Map.empty Set.empty None
                    |> fun result -> Map.tryFind "r1" result.Assignments

                Expect.equal
                    (assignedUnder neutralRoom)
                    (Some(taskId (Reserve "ctrl-west")))
                    "neutral, the near controller is the cheapest walk and the body takes it"

                Expect.equal
                    (assignedUnder ownedRoom)
                    (Some(taskId (Reserve "ctrl-out")))
                    "owned, the near controller is no Task and the body crosses to the neutral one"
            }
        ]

/// One outpost as ADR 0041 declares it, read off the very tuple the
/// reserver row's fixtures are written in — a room name, its rock and its
/// controller, ids and tiles — spelling the same ids and the same tiles
/// `withOutpostRoom` furnishes that room with. The declaration is what a
/// stand-down subtracts and the furniture is what vision pays for, and
/// the two have to name one room or the gate below would be subtracting
/// something nothing else placed. Taking the room and the rock from
/// `northOutpost`/`westOutpost` rather than retyping them is what keeps
/// that true: one source for the colony both families describe, so a rock
/// moved for one is moved for the other. The `posted` flag rides along
/// unread — `gatedColony` posts every room it works, because an unposted
/// outpost contributes nothing to three of the four rows to begin with
/// and would make the gate's subtraction unreadable.
let private gatedOutpost (room, rock: Pos, _posted) : Outpost =
    {
        RoomName = room
        Sources = [ $"src-{room}", RoomPos.at room rock ]
        Controller = $"ctrl-{room}", RoomPos.at room { rock with Y = rock.Y + 2 }
    }

/// The colony the shell assembles for a given declaration under a given
/// shut set (ADR 0043), built the way `ColonyView.ofWorld` builds one: the
/// declarations less what the gate withholds (`Outpost.worked`), and then
/// every fact of a worked room — its layer, its rock in the pool, its
/// standing container, who holds it — and *none* of a withheld one.
///
/// That second half is the shell's own rule and not this fixture's
/// invention: the scan set is taken from the declarations that survive the
/// gate, the furniture is laid only into rooms the scan set carries
/// (`Outpost.place`), the rocks are pooled only for those rooms
/// (`Outpost.pooledSources`) and every entry vision pays for is collected
/// over `seen`, which is the scan set filtered by vision. A room the
/// colony does not scan is one it never looks into, so it contributes
/// nothing at all — which is exactly what ADR 0004 has always meant by a
/// room that is not there.
///
/// The reservation stands at its 5,000 cap on every worked room, so the
/// reserver row's deficit is zero and its casts are at the floor: the
/// number of casts is then a count of rooms and never a reading of a
/// deficit.
///
/// Assembled by `reserverColony` and not beside it: the rooms, the bank
/// and the control entries are that fixture's already, so the gate reads
/// over the same colony the reserver row is pinned on rather than a second
/// one free to drift from it.
let private gatedColony declarations shut creeps =
    let worked = Outpost.worked shut declarations

    reserverColony
        (worked
         |> List.map (fun (outpost: Outpost) ->
             outpost.RoomName, outpost.Sources |> List.head |> snd |> RoomPos.pos, true))
        creeps
        (worked |> List.map (fun outpost -> outpost.RoomName, reservedRoom true 5000))

/// The two outposts the gate is read over, diagonal to each other as
/// W12S27 and W13S28 are (ADR 0042): one gate each, and one of them is not
/// enough to tell "this room is withheld" from "outposts are withheld".
/// The same two the reserver row hires for, declared instead of furnished.
let private northGated = gatedOutpost (northOutpost true)
let private westGated = gatedOutpost (westOutpost true)

/// A creep standing in a room, placed the way the shell places one: in the
/// layer of the room it stands in (ADR 0041), and nowhere at all when that
/// room is not projected. A creep does give the engine vision of its own
/// room, but the shell reads the rooms it scans and no others, so a
/// stood-down room's tiles go unread and the creep on them is unplaced —
/// unpriceable geometry, which is ADR 0004's own answer and not a state of
/// its own.
let private standingIn room (name, pos) (colony: ColonyView) =
    match Map.tryFind room colony.Spatial.Rooms with
    | None -> colony
    | Some layer ->
        { colony with
            Spatial =
                colony.Spatial
                |> withNeighbour
                    room
                    { layer with
                        CreepPositions = Map.add name pos layer.CreepPositions
                    }
        }

/// Every Task in the pool that names a room's furniture — the rock, the
/// controller and the container `withOutpostRoom` gives it, whose ids all
/// carry the room's name.
let private tasksNaming room colony =
    planTasks colony noThreats
    |> List.map taskId
    |> List.filter (fun id -> (id: string).Contains(room: string))

[<Tests>]
let standDownGateTests =
    testList
        "a stood-down outpost in the pool"
        [
            test "a stood-down outpost pools no Task, counts in no quota and is cast for by nobody" {
                // ADR 0043's whole claim, at the top seam: a room the gate
                // withholds decides exactly what a room nobody declared
                // decides. Nothing downstream was taught about stand-downs
                // — the projection, the Task pool, the four quota rows and
                // the Atlas each see a room that is not there, which is the
                // semantics ADR 0004 paid for long ago.
                //
                // The fleet stands over every row's quota but the
                // reserver's, so a `SpawnCreep` here is a reserver or it is
                // a defect, and the reserver row is the one row a
                // *declaration alone* hires for (#131): one body per
                // declared outpost, container or no container. That makes
                // it the row that can tell "the room left the projection"
                // from "the room left the economy".
                let fleet = surplusFleet 4
                let both = gatedColony [ northGated; westGated ] Set.empty fleet
                let shut = gatedColony [ northGated; westGated ] (Set.singleton "W1N2") fleet
                let never = gatedColony [ westGated ] Set.empty fleet

                Expect.isNonEmpty
                    (tasksNaming "W1N2" both)
                    "the premise: worked, the room's furniture is in the pool"

                Expect.equal
                    (reserverCasts (decide both Map.empty Set.empty None).Intents)
                    [ oneBlock; oneBlock ]
                    "and worked, it is one of two outposts each hiring its own reserver"

                Expect.isEmpty
                    (tasksNaming "W1N2" shut)
                    "shut, no Task in the pool names the room — its rock, its controller and its container are gone with it"

                Expect.equal
                    (reserverCasts (decide shut Map.empty Set.empty None).Intents)
                    [ oneBlock ]
                    "and the one cast left is the other outpost's: nothing is built for a room nothing can enter"

                // Everything else besides, and this one holds by
                // construction rather than by observation: `gatedColony`
                // subtracts the shut set before it assembles anything, so
                // the two views below are the same value and the
                // equality can only fail if `Outpost.worked` filters by
                // something other than the room's name. That is worth one
                // line and is not the criterion's quota half — a row still
                // counting the shut room could not show up here, because
                // there is no room here for it to count.
                Expect.equal
                    (outcomeOf shut)
                    (outcomeOf never)
                    "a room the gate withholds is subtracted by name, so it assembles the colony a room nobody declared assembles"
            }

            test "the quota rows stop counting the room the gate withholds" {
                // Criterion 1's other half, and the one the equality above
                // cannot reach: it is read on two colonies that really do
                // differ — both declare both rooms, and only the shut set
                // moves — so a row still folding the withheld room's
                // furniture hires a body the colony it is actually working
                // does not want.
                //
                // One row at a time, each against a fleet standing exactly
                // at the shut colony's own quota for it while every other
                // row is over its own, which is the pairwise reading the
                // matcher's cheapest-rival rule asks for everywhere else.
                let castRows anchors haulers workers shut =
                    let fleet =
                        [ for i in 1..anchors -> anchor $"a{i}" 0 50 ]
                        @ [ for i in 1..haulers -> hauler $"h{i}" 0 100 ]
                        @ [ for i in 1..workers -> worker $"w{i}" 0 50 ]

                    gatedColony [ northGated; westGated ] shut fleet
                    |> castNames
                    |> List.map (fun (name: string) -> name.Split('-') |> Array.head)

                let gated anchors haulers workers =
                    castRows anchors haulers workers Set.empty,
                    castRows anchors haulers workers (Set.singleton "W1N2")

                // The Anchor row counts Posts and the withheld room's
                // standing container was one: at three Anchors the colony
                // working both rooms is a body short and the one working
                // the west room alone is already at its target.
                Expect.equal
                    (gated 3 3 40)
                    ([ "reserver"; "reserver"; "anchor" ], [ "reserver" ])
                    "the fourth Post goes with the room, and the Anchor it would have hired goes with it"

                // The workforce target counts each posted source's output
                // and the withheld room's rock was one: at three workers
                // the colony working both rooms hires a fourth.
                Expect.equal
                    (gated 4 1 3)
                    ([ "reserver"; "reserver"; "worker" ], [ "reserver" ])
                    "the withheld rock's ten a tick leaves the income the worker row is sized off"

                // The fourth row is deliberately not pinned by a cast. At
                // ADR 0042's 1,800 capacity one hauler covers both home
                // containers' round trips together (ADR 0049), and this
                // colony's two home containers set the row at one either
                // way — the outposts move it by nothing there is a body's
                // granularity to see. What the row reads is the
                // projection's containers, and the withheld room's is gone
                // with the room, which the Task pool above already shows:
                // no Withdraw names it.
                Expect.equal
                    (gated 4 0 40)
                    ([ "reserver"; "reserver"; "hauler" ], [ "reserver"; "hauler" ])
                    "the hauler row wants its one home body on either side of the gate"
            }

            test "two outposts are two gates" {
                // ADR 0043's independent gates: W12S27 standing down does
                // not cost W13S28 its reserver. Pairwise, one room shut at
                // a time, because a gate that withheld "the outposts"
                // rather than a room would pass a test that shut only one.
                let shutting room =
                    let colony =
                        gatedColony [ northGated; westGated ] (Set.singleton room) (surplusFleet 4)

                    tasksNaming "W1N2" colony, tasksNaming "W2N2" colony

                let northShut, westWithNorthShut = shutting "W1N2"
                let northWithWestShut, westShut = shutting "W2N2"

                Expect.isEmpty northShut "the north room is withheld"

                Expect.isNonEmpty westWithNorthShut "while the west one is worked exactly as before"

                Expect.isEmpty westShut "and the other way round"
                Expect.isNonEmpty northWithWestShut "with the north one untouched"
            }

            test "the tick the clock runs out, the outpost is back in the pool" {
                // Re-entry is the clock running out and nothing else (ADR
                // 0043), so the gate is read straight off the log: the two
                // colonies below differ only in the tick `Observe.standDown`
                // was asked at, one either side of the recorded expiry.
                let log =
                    Observe.RaidState.empty
                    // No world roster: one tick folded off an empty log
                    // has no `Living` baseline, so nothing can be read as a
                    // loss whatever `Game.creeps` holds (#191).
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        { incomeColony with
                            Time = 100
                            InvaderCores =
                                [
                                    {
                                        RoomName = "W1N2"
                                        CollapseTick = Some 900
                                    }
                                ]
                        }

                let fleet = surplusFleet 4

                let atTick t =
                    gatedColony
                        [ northGated; westGated ]
                        (Observe.standDown Tuning.defaults t log).Shut
                        fleet

                Expect.isEmpty
                    (tasksNaming "W1N2" (atTick 899))
                    "one tick short of the expiry the room is still withheld"

                Expect.isNonEmpty
                    (tasksNaming "W1N2" (atTick 900))
                    "on the expiry itself its rock, its controller and its container are in the pool again"

                Expect.equal
                    (reserverCasts (decide (atTick 900) Map.empty Set.empty None).Intents)
                    [ oneBlock; oneBlock ]
                    "and the row hires for it again, the tick it may be entered"
            }

            test "a room another player holds is withheld with no clock at all" {
                // ADR 0043's other trigger, end to end: the fold remembers
                // the room the tick it is seen taken (`RaidState.RivalHeld`),
                // and the gate withholds it for ever after — there is no
                // expiry, because a room somebody else **owns** has not been
                // made dangerous, it has stopped being ours.
                //
                // Pairwise against the same room seen held by *us*, which
                // is the ordinary steady state of every outpost: one control
                // entry moves.
                let logWith control =
                    Observe.RaidState.empty
                    // No world roster: one tick folded off an empty log
                    // has no `Living` baseline, so nothing can be read as a
                    // loss whatever `Game.creeps` holds (#191).
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        { incomeColony with
                            Time = 100
                            RoomControl = Map.ofList [ "W1N2", control ]
                        }

                let fleet = surplusFleet 4

                let poolAt control t =
                    gatedColony
                        [ northGated; westGated ]
                        (Observe.standDown Tuning.defaults t (logWith control)).Shut
                        fleet
                    |> tasksNaming "W1N2"

                Expect.isNonEmpty
                    (poolAt (reservedRoom true 4000) 101)
                    "held by us the room is worked, which is what every outpost's steady state looks like"

                Expect.isEmpty
                    (poolAt rivalRoom 101)
                    "owned by another player it is withheld the tick after it was seen"

                Expect.isEmpty
                    (poolAt rivalRoom 1_000_000)
                    "and a million ticks later it is still withheld: this withdrawal carries no clock"
            }

            test "a room another player reserved is back in the pool when that hold ends" {
                // #165, end to end at the same seam as the two tests above:
                // a rival's *reservation* is a clocked stand-down and not the
                // latch beside it, so the Tasks, the furniture and the
                // reserver row all come back on the tick the engine's own
                // countdown reaches — with nobody having gone to look, which
                // is the whole point of a clock (ADR 0043).
                //
                // Pairwise against the room owned outright, one control entry
                // apart: the latch above is still a latch.
                let log =
                    Observe.RaidState.empty
                    // No world roster, for the reason the test above gives.
                    |> Observe.foldRaids
                        Observe.capEpisodes
                        Set.empty
                        { incomeColony with
                            Time = 100
                            RoomControl = Map.ofList [ "W1N2", reservedRoom false 4000 ]
                        }

                let fleet = surplusFleet 4

                let atTick t =
                    gatedColony
                        [ northGated; westGated ]
                        (Observe.standDown Tuning.defaults t log).Shut
                        fleet

                Expect.isEmpty
                    (tasksNaming "W1N2" (atTick 101))
                    "the tick after the reservation was seen the room is withheld"

                Expect.isEmpty
                    (tasksNaming "W1N2" (atTick 4099))
                    "and stays withheld for every tick of the hold the engine is counting down"

                Expect.isNonEmpty
                    (tasksNaming "W1N2" (atTick 4100))
                    "on the tick that hold ends its rock, its controller and its container are pooled again"

                Expect.equal
                    (reserverCasts (decide (atTick 4100) Map.empty Set.empty None).Intents)
                    [ oneBlock; oneBlock ]
                    "and the row hires for it again, no look having been needed"
            }

            test "the look a re-check buys decides nothing" {
                // #165's second half at the top seam. On the one tick in
                // every `Tuning.RivalRecheck` the gate re-admits a latched
                // room to the **scan**, the colony reads that room's control
                // entry — and a control entry alone is what the whole
                // re-admission amounts to: the room is in no layer, its rock
                // is in no pool and its controller is no Task, so every
                // reader of `RoomControl` asks it about a room the projection
                // already carries and finds this one nowhere (ADR 0004).
                // "Re-admitted to the scan set only, and not to the Task or
                // quota set" is that sentence, pinned where a reader that
                // widened it would go red.
                let fleet = surplusFleet 4
                let shut = gatedColony [ northGated; westGated ] (Set.singleton "W1N2") fleet

                let looked =
                    { shut with
                        RoomControl = Map.add "W1N2" rivalRoom shut.RoomControl
                    }

                let withoutLook = decide shut Map.empty Set.empty None
                let withLook = decide looked Map.empty Set.empty None

                Expect.isNonEmpty
                    withoutLook.Verdicts
                    "the premise: this colony reaches a decision worth comparing"

                Expect.equal
                    { withLook with
                        Memo =
                            { withLook.Memo with
                                Walks = withoutLook.Memo.Walks
                            }
                    }
                    withoutLook
                    "the same decision, memo and census signature and all"
            }

            test "the creep standing in a stood-down outpost is released, on the existing path" {
                // ADR 0043's re-entry rule has a mirror: nothing new
                // withdraws the creeps either. The room's Tasks stop
                // existing, and a creep holding one is released by the
                // release the Matcher has always spoken for an assignment
                // whose Task is gone — no retreat act, no new Verdict, no
                // second rule about where a creep may stand.
                // One creep and no fleet behind it: the release is the
                // subject, and a colony standing at its quotas would have
                // every home Task at capacity, so the creep would read as
                // unassigned for a reason that has nothing to do with the
                // gate.
                let colonyWith shut =
                    gatedColony [ northGated; westGated ] shut [ worker "w-out" 0 50 ]
                    |> standingIn "W1N2" ("w-out", { X = 39; Y = 41 })

                let held = taskId (Harvest "src-W1N2")
                let assignments = Map.ofList [ "w-out", held ]

                let verdictsWith shut =
                    (decide (colonyWith shut) assignments Set.empty None).Verdicts

                Expect.contains
                    (verdictsWith Set.empty)
                    (Verdict.Kept("w-out", held))
                    "the premise: worked, the creep keeps the outpost Harvest it holds"

                Expect.contains
                    (verdictsWith (Set.singleton "W1N2"))
                    (Verdict.Released("w-out", held, ReleaseReason.TaskGone))
                    "shut, the Task is gone and the creep is released by the reason that has always meant that"

                let rematched =
                    verdictsWith (Set.singleton "W1N2")
                    |> List.tryPick (function
                        | Verdict.Matched("w-out", task, _) -> Some task
                        | _ -> None)

                Expect.isSome
                    rematched
                    "and it is matched again on the same tick, not left holding nothing"

                Expect.isFalse
                    ((Option.defaultValue "" rematched).Contains "W1N2")
                    "to a Task of a room the colony is still working"

            // What is *not* pinned here is the walk back, and it is not
            // pinned because it does not happen. A withheld room is not
            // projected (ADR 0043), so it places no creep, so the creep
            // standing in it has no tile: the rematch above is priced on
            // ADR 0004's escape — an unplaced creep prices every Task at 0
            // — rather than on a crossing, `Decide.resolve` builds moves
            // only over the creeps the Atlas places, and nothing aims this
            // one home. The release path is this ticket's claim and it
            // holds; the journey home is a fact about an unplaced creep
            // that ADR 0043's own gate placement makes unreachable, and it
            // is carried out of this ticket as a finding of its own rather
            // than pinned here as if it were the behaviour.
            }
        ]
