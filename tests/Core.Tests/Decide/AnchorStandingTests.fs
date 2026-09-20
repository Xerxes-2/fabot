/// The standing body, its own Refill, and the Post it raises.
module Fabot.Core.Tests.Decide.AnchorStandingTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.AnchorFixtures

[<Tests>]
let standingBodyTests =
    testList
        "the standing body"
        [
            test "beside the buffer a standing body upgrades where the generalist builds" {
                // Pairwise on the body: the same tile, pool and tier. A
                // standing body's Carry holds fifty against eleven Work, so
                // the trip would idle eleven Work for every tick delivered.
                let site = [ "site-1", { X = 15; Y = 10 }, Site BuiltKind.Extension ]
                let sites = [ { Id = "site-1"; Left = siteOwes } ]

                Expect.equal
                    (laneAssignment site sites upgraderBody)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "the standing body spends its load where it stands"

                Expect.equal
                    (laneAssignment site sites workerBody)
                    (Some(taskId (Build "site-1")))
                    "the generalist at the same tile takes the nearer Task, as it always has"
            }

            test "the Anchor row's body is a standing body too" {
                // Pinned against the generalist in the same lane, so it is
                // the arithmetic `1 * 4 < 6` on trial and not an
                // Anchor-shaped exception. This creep stands one step
                // outside the Upgrade Work Area, so it holds no Task at all.
                let site = [ "site-1", { X = 15; Y = 10 }, Site BuiltKind.Extension ]

                Expect.equal
                    (laneAssignment site [ { Id = "site-1"; Left = siteOwes } ] anchorBody)
                    None
                    "one Carry against six Work is a commute, whichever row cast it and whichever way it walks"
            }

            test "a standing body does not repair either" {
                // A road at half hits is the ordinary Repair, one step from
                // the creep.
                let road = [ "road-1", { X = 15; Y = 10 }, Structure BuiltKind.Road ]

                let dented (colony: ColonyView) =
                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Hits =
                                    Map.add
                                        "road-1"
                                        { Hits = 100; HitsMax = 5000 }
                                        colony.Spatial.Hits
                            }
                    }

                let assignedFor creep =
                    let { Assignments = assignments } =
                        decideOn (dented (bufferLaneColony road [] creep))

                    Map.tryFind (creep: CreepInfo).Name assignments

                Expect.equal
                    (assignedFor upgraderBody)
                    (Some(taskId (Upgrade "ctrl-1")))
                    "a repair is a delivery, and this body delivers fifty at a time"

                Expect.equal
                    (assignedFor workerBody)
                    (Some(taskId (Repair "road-1")))
                    "the generalist at the same tile still repairs the road beside it"
            }

            test "the buffer's own Withdraw stays open to a standing body" {
                // The upgrader row is at `Work = Move` with a Work part, so
                // the buffer it stands beside is what it draws from. Empty,
                // the Withdraw is all that is left.
                let empty = creepWith "upgrader" 0 50 (bodyFor upgraderPattern 1800)

                Expect.equal
                    (laneAssignment [] [] empty)
                    (Some(taskId (Withdraw("can-buf", Energy))))
                    "the row drinks from the buffer at its feet, which is why it stands there"
            }

            test "a buffer under the worth-the-trip line is still this row's drink" {
                // The worth-the-trip line prices a trip, and this row makes
                // none: it lives at that store. Without the exemption a
                // buffer holding twenty-four left an `11W/1C/11M` upgrader
                // with no applicable Task at all. Pairwise on the body: the
                // generalist that would walk there is refused by the line.
                let thin creep =
                    let colony = bufferLaneColony [] [] creep

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Stores = Map.add "can-buf" 24 colony.Spatial.Stores
                            }
                    }

                let assignedIn creep =
                    let { Assignments = assignments } = decideOn (thin creep)

                    Map.tryFind (creep: CreepInfo).Name assignments

                let emptyOf pattern =
                    let body = bodyFor pattern 1800

                    creepWith
                        pattern.Name
                        0
                        (50 * (body |> List.filter ((=) Carry) |> List.length))
                        body

                Expect.equal
                    (assignedIn (emptyOf upgraderPattern))
                    (Some(taskId (Withdraw("can-buf", Energy))))
                    "twenty-four under its feet is a trip's worth for the row that never leaves"

                Expect.equal
                    (assignedIn (emptyOf workerPattern))
                    None
                    "the generalist walks to that buffer, so the line prices the walk and refuses"
            }

            test "under RCL3 the colony's own extension site joins the flow" {
                // The site is feeding-tier while the controller is under
                // `Tuning.BootstrapLevel` with a spawn standing, and surplus
                // from RCL3 up. Measured against the hungry spawn at the
                // lane's far end: on the feeding tier the site ties it and
                // wins on price; a rung below, it is outranked however near.
                let lane level =
                    bufferLaneFlow
                        [ "site-1", { X = 15; Y = 10 }, Site BuiltKind.Extension ]
                        [ { Id = "site-1"; Left = siteOwes } ]
                        (creepWith "w" 100 0 (bodyFor workerPattern 300))
                    |> withLevel level

                let matched level =
                    let { Verdicts = verdicts } = decideOn (lane level)

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("w", task, factor) -> Some(task, factor)
                        | _ -> None)

                Expect.equal
                    (matched 2)
                    (Some(taskId (Build "site-1"), MatchFactor.TravelCost))
                    "at RCL2 the extension site is feeding-tier: it ties the flow and stands nearer"

                Expect.equal
                    (matched 3)
                    (Some(taskId (Refill("spawn-1", Energy)), MatchFactor.Rank))
                    "at RCL3 it is surplus, and the flow five steps off outranks it"
            }

            test "a standing body fetches from the buffer alone: dry, it waits for the haulers" {
                // The buffer empty, a stocked Storage and a pile of 400 five
                // tiles down the lane: the generalist walks, the standing
                // body stands. Live it was fifty tiles across a Seam for
                // fifty energy. Pairwise on the body alone.
                let dryLane =
                    { bufferLane with
                        Stores = Map.ofList [ "can-buf", 0; "sto-1", 5000; "pile-1", 400 ]
                    }

                let colony creep =
                    { bufferLaneColony [] [] creep with
                        Spatial =
                            dryLane
                            |> withTargets
                                [
                                    "sto-1", { X = 18; Y = 10 }, Structure BuiltKind.Storage
                                    "pile-1", { X = 18; Y = 11 }, (Dropped Energy)
                                ]
                            |> withCreepsAt [ (creep: CreepInfo).Name, { X = 14; Y = 10 } ]
                    }

                let empty pattern =
                    let body = bodyFor pattern 1800

                    creepWith
                        pattern.Name
                        0
                        (50 * (body |> List.filter ((=) Carry) |> List.length))
                        body

                let outcome creep =
                    let {
                            Assignments = assignments
                            Verdicts = verdicts
                        } =
                        decideOn (colony creep)

                    Map.tryFind creep.Name assignments, verdicts

                let standingAssigned, standingVerdicts = outcome (empty upgraderPattern)

                Expect.equal
                    standingAssigned
                    None
                    "the standing body takes neither the Storage nor the pile"

                Expect.contains
                    standingVerdicts
                    (Verdict.Unassigned("upgrader", IdleReason.NoneApplicable))
                    "and it is inapplicability, not price: nothing in the pool is its to walk to"

                let workerAssigned, _ = outcome (empty workerPattern)

                Expect.isTrue
                    (workerAssigned = Some(taskId (Withdraw("sto-1", Energy)))
                     || workerAssigned = Some(taskId (Pickup("pile-1", Energy))))
                    "the generalist, one Carry per Work, walks to whichever intake is cheaper"
            }

            test "an Anchor with a site beside it still only Harvests" {
                // With Harvest in the pool the rank settles it and the gate
                // never gets a say. Pairwise on the pool: the same Anchor,
                // with and without the site.
                let seatRoom =
                    { spatial
                          [ "src-a", { X = 10; Y = 10 } ]
                          (openSeats { X = 10; Y = 10 }
                           @ [ { X = 12; Y = 10 }, Plain; { X = 13; Y = 10 }, Plain ]) with
                        TargetKinds = Map.ofList [ "src-a", Source ]
                    }
                    |> withCreepsAt [ "anchor", { X = 11; Y = 10 } ]

                let colony sites targets =
                    { bareRespawn with
                        Bank = bank 1800 1800
                        Sources = [ source "src-a" ]
                        Refillables = []
                        Controller = None
                        ConstructionSites = sites
                        Creeps = [ creepWith "anchor" 25 25 (bodyFor anchorPattern 1800) ]
                        Spatial = seatRoom |> withTargets targets
                    }

                let assignedWith sites targets =
                    let { Assignments = assignments } = decideOn (colony sites targets)

                    Map.tryFind "anchor" assignments

                Expect.equal
                    (assignedWith [] [])
                    (Some(taskId (Harvest "src-a")))
                    "with nothing else in the pool the Anchor digs"

                Expect.equal
                    (assignedWith
                        [ { Id = "site-1"; Left = siteOwes } ]
                        [ "site-1", { X = 12; Y = 10 }, Site BuiltKind.Extension ])
                    (Some(taskId (Harvest "src-a")))
                    "and a site one step away does not move it — the rule only writes down what the rank already did"
            }
        ]

[<Tests>]
let standingRefillTests =
    testList
        "the standing body and the Refill"
        [
            test "the hauler row is outside the gate by arithmetic, not by exception" {
                // A hauler has no Work at all, so `Carry * 4 < Work` reads
                // `8 * 4 < 0` — false — and the row keeps every delivery.
                Expect.equal
                    (deliveryAssignment (hauler "hauler" 100 0))
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "no Work part means no standing body, whatever the Carry"
            }

            test "a standing body does not refill the spawn beside it" {
                Expect.equal
                    (deliveryAssignment upgraderBody)
                    None
                    "one Carry against eleven Work delivers fifty energy a round trip"
            }

            test "a full Anchor off its Post keeps no delivery either" {
                // An Anchor whose Post has no standing container fills up,
                // and with no Harvest in its pool holds no Task here rather
                // than the spawn's Refill two steps away.
                Expect.equal
                    (deliveryAssignment anchorBody)
                    None
                    "one Carry against six Work is the same commute, and the same prohibition"
            }

            test "the row's own small cast is outside the gate with the rest of the band" {
                // `3W/1C/3M`, the upgrader row's body at the RCL2 bank of
                // 550: `1 * 4 < 3` is false, so it delivers like the
                // generalist it is read back to.
                Expect.equal
                    (deliveryAssignment (creepWith "upgrader" 50 0 (bodyFor upgraderPattern 550)))
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "three Work against a fifty-energy load is not yet a commute"
            }

            test "the generalist beside it refills as it always has" {
                // `9W/9C/9M` is one Carry per Work where the line is one
                // per four: the whole worker row is outside it at every bank.
                Expect.equal
                    (deliveryAssignment workerBody)
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "the row that carries its energy to work keeps its deliveries"
            }
        ]

[<Tests>]
let postSiteTests =
    testList
        "the Anchor raises its own Post"
        [
            test "empty on its container site the Anchor digs; full it builds" {
                // The site is a Post, so the rock has a Work Area again;
                // the site is under the body's feet, so Build is not a
                // commute. The pair alternates on the store alone: a site
                // catches no overflow, so a full store ends the dig.
                Expect.equal
                    (matchOf (
                        raisingColony BuiltKind.Container (postBody "w" 0 50) { X = 10; Y = 45 }
                    ))
                    (Some(taskId (Harvest "src-out"), MatchFactor.TravelCost))
                    "empty, it digs the rock beside it — the home rock is a Seam and thirty tiles away"

                Expect.equal
                    (matchOf (
                        raisingColony BuiltKind.Container (postBody "w" 50 0) { X = 10; Y = 45 }
                    ))
                    (Some(taskId (Build "site-out"), MatchFactor.OnlyCandidate))
                    "full, the dig is over and the site under its feet is what is left"

                Expect.equal
                    (actionIntents
                        (decide
                            (raisingColony
                                BuiltKind.Container
                                (postBody "w" 50 0)
                                { X = 10; Y = 45 })
                            Map.empty
                            Set.empty
                            None)
                            .Intents)
                    [ BuildSite("w", "site-out") ]
                    "and it spends what it dug into the progress without moving a tile"
            }

            test "swap the kind and both doors shut again" {
                // Pairwise: a road site on the same Seat. Nothing there is a
                // Post, so the outpost rock's Work Area is empty for a heavy
                // body, and the site is no Post either, so Build stays shut.
                // Empty it falls back on the home room's bare Seats thirty
                // tiles and a Seam away; full it holds no Task.
                let road energy free =
                    raisingColony BuiltKind.Road (postBody "w" energy free) { X = 10; Y = 45 }

                Expect.equal
                    (matchOf (road 0 50))
                    (Some(taskId (Harvest "src-home"), MatchFactor.OnlyCandidate))
                    "empty, the outpost rock it is standing on offers it no tile to work from"

                Expect.isNone
                    (matchOf (road 50 0))
                    "full, the site beneath it is somebody else's work"
            }

            test "the site hires the garrison that raises it" {
                // `Atlas.postCount` is the Anchor quota's input: the tile a
                // container is going up on is a garrison place.
                let count kind =
                    Atlas.postCount (
                        Atlas.ofView (raisingColony kind (postBody "w" 0 50) { X = 10; Y = 45 })
                    )

                Expect.equal (count BuiltKind.Container) 1 "the container site is a Post"
                Expect.equal (count BuiltKind.Road) 0 "and a site of any other kind is none"
            }

            test "the Work Area is the site tile and not the Seat beside it" {
                // The other Seat of the same rock is standing room for a
                // light body and not for this one: a body that dug from
                // (10,47) would put its twelve a tick on the floor.
                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideOn (
                        raisingColony BuiltKind.Container (postBody "w" 0 50) { X = 10; Y = 47 }
                    )

                Expect.equal
                    (verdicts
                     |> List.tryPick (function
                         | Verdict.Matched("w", task, _) -> Some task
                         | _ -> None))
                    (Some(taskId (Harvest "src-out")))
                    "the rock is its Task from the Seat next door"

                Expect.isEmpty (digIntentsFor "w" intents) "but it may not dig from where it stands"

                Expect.isNonEmpty
                    (moveIntentsFor "w" intents)
                    "so it walks the one tile onto its Post"
            }

            test "the rule reads the tile and never the row" {
                // The exception reads the tile, not the row: the generalist
                // on the same tile takes the same Build.
                Expect.equal
                    (matchOf (
                        raisingColony BuiltKind.Container (worker "w" 50 0) { X = 10; Y = 45 }
                    ))
                    (Some(taskId (Build "site-out"), MatchFactor.OnlyCandidate))
                    "the worker row's own Build across the Seam is untouched"
            }

            test "a home rock's container site is the same Post" {
                // Not an outpost rule: an RCL2 colony raises its own first
                // source container the same way.
                Expect.equal
                    (matchOf (
                        homeRaisingColony BuiltKind.Container (postBody "w" 0 50) { X = 9; Y = 10 }
                    ))
                    (Some(taskId (Harvest "src-a"), MatchFactor.OnlyCandidate))
                    "empty, it digs"

                Expect.equal
                    (matchOf (
                        homeRaisingColony BuiltKind.Container (postBody "w" 50 0) { X = 9; Y = 10 }
                    ))
                    (Some(taskId (Build "can-a"), MatchFactor.OnlyCandidate))
                    "full, it builds the site under its feet"

                // Pairwise with a road site: at home the empty body keeps
                // its bare-Seat dig, and the full one has nothing.
                Expect.equal
                    (matchOf (
                        homeRaisingColony BuiltKind.Road (postBody "w" 0 50) { X = 9; Y = 10 }
                    ))
                    (Some(taskId (Harvest "src-a"), MatchFactor.OnlyCandidate))
                    "the home fallback stands: an empty body digs from any Seat"

                Expect.isNone
                    (matchOf (
                        homeRaisingColony BuiltKind.Road (postBody "w" 50 0) { X = 9; Y = 10 }
                    ))
                    "and a full standing body has no delivery it may walk to"
            }

            test "a site is a Post and still no income: the quotas wait for the container" {
                // A source whose energy goes into progress pays no haul term
                // and feeds no mouth, so `Decide.isPosted` reads the
                // standing census while the garrison reads the whole one.
                let atlasOf kinds =
                    let colony = raisingColony kinds (postBody "w" 0 50) { X = 10; Y = 45 }

                    Atlas.ofView colony

                let pending = atlasOf BuiltKind.Container

                Expect.equal
                    (Atlas.postsOf pending "src-out" |> RoomPos.inRoom "W1N2")
                    (Set.singleton { X = 10; Y = 45 })
                    "the site is the rock's Post"

                Expect.isEmpty
                    (Atlas.standingPostsOf pending "src-out")
                    "and nothing stands on it, so the rock is in no quota yet"
            }

            test "full beside its site, the reprieve walks it on rather than stranding it" {
                // The Build exception asks for the exact tile, so what saves
                // a full body one step off it is the walk-home reprieve,
                // which reads `Atlas.postsOf`. Read off the standing census
                // with `isPosted`, this body would hold no Task at all.
                let {
                        Intents = intents
                        Verdicts = verdicts
                    } =
                    decideOn (
                        raisingColony BuiltKind.Container (postBody "w" 50 0) { X = 10; Y = 47 }
                    )

                Expect.equal
                    (verdicts
                     |> List.tryPick (function
                         | Verdict.Matched("w", task, _) -> Some task
                         | _ -> None))
                    (Some(taskId (Harvest "src-out")))
                    "full or empty, the rock a step away is what it holds"

                Expect.isEmpty
                    (actionIntents intents)
                    "it builds nothing from where it stands — the exception is the tile and not the range"

                Expect.isNonEmpty
                    (moveIntentsFor "w" intents)
                    "so the walk onto its Post is the whole of this tick"
            }

            test "a garrison on the rock's other Post does not strand the body raising the site" {
                // A rock with a standing container Post and a container site
                // Post beside it. Read against the standing census the rock
                // would be occupied by the garrison on the built container,
                // and the full body one step off its own site would hold no
                // Task for the rest of its life. The condition reads every
                // Post.
                let room =
                    { spatial
                          [
                              "src-a", { X = 10; Y = 10 }
                              "cont-1", { X = 11; Y = 10 }
                              "can-a", { X = 9; Y = 10 }
                          ]
                          [
                              { X = 8; Y = 10 }, Plain
                              { X = 9; Y = 10 }, Plain
                              { X = 10; Y = 10 }, Wall
                              { X = 11; Y = 10 }, Plain
                          ] with
                        TargetKinds =
                            Map.ofList
                                [
                                    "src-a", Source
                                    "cont-1", Structure BuiltKind.Container
                                    "can-a", Site BuiltKind.Container
                                ]
                    }

                let colony =
                    { bareRespawn with
                        Spawns = []
                        Refillables = []
                        Controller = None
                        Sources = [ source "src-a" ]
                        ConstructionSites = [ { Id = "can-a"; Left = siteOwes } ]
                        Creeps = [ postBody "a1" 50 0; postBody "g1" 0 50 ]
                        Spatial =
                            room
                            |> withCreepsAt [ "a1", { X = 8; Y = 10 }; "g1", { X = 11; Y = 10 } ]
                    }

                let {
                        Assignments = assignments
                        Intents = intents
                    } =
                    decideOn colony

                Expect.equal
                    (Map.tryFind "a1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "the site Post is standing room the garrison next door is not on"

                Expect.isNonEmpty
                    (moveIntentsFor "a1" intents)
                    "so the full body takes the step back onto the site it was cast to raise"

                Expect.equal
                    (Map.tryFind "g1" assignments)
                    (Some(taskId (Harvest "src-a")))
                    "and the garrison on the built container keeps its own Post"
            }

            test "the builder budget prices a commute, and the body on the site pays none" {
                // `Tuning.OutpostBuilders` prices the commute, and a body
                // standing on the site spends none of it. Counted inside the
                // budget, two loaded workers holding the Build through their
                // commute would leave the garrison full with no Task.
                let crowd extra =
                    raisingCrowd
                        BuiltKind.Container
                        [ postBody "a" 50 0, { X = 10; Y = 45 } ]
                        ([
                            worker "w1" 50 0, { X = 10; Y = 2 }
                            worker "w2" 50 0, { X = 10; Y = 3 }
                         ]
                         @ extra)

                let held =
                    Map.ofList [ "w1", taskId (Build "site-out"); "w2", taskId (Build "site-out") ]

                let matchedOf name (verdicts: Verdict list) =
                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched(who, task, _) when who = name -> Some task
                        | _ -> None)

                let { Verdicts = verdicts } = decideFrom held (crowd [])

                Expect.equal
                    (matchedOf "a" verdicts)
                    (Some(taskId (Build "site-out")))
                    "the garrison builds what is under its feet whoever else is walking to it"

                // Pairwise on the body: a third loaded worker still waits at
                // home.
                let { Verdicts = withThird } =
                    decideFrom held (crowd [ worker "w3" 50 0, { X = 10; Y = 4 } ])

                Expect.isNone
                    (matchedOf "w3" withThird)
                    "and the third commuter is still refused: #157's number is unmoved"
            }

            test "the Post is held by the body standing on it, digging or building" {
                // On a site there is no overflow, so the pair alternates and
                // a slot counted by holders would read free on every build
                // tick, admitting a second heavy body onto the one tile. The
                // slot is held by where the body stands.
                let pair (aEnergy: int) (aFree: int) =
                    raisingCrowd
                        BuiltKind.Container
                        [
                            postBody "a" aEnergy aFree, { X = 10; Y = 45 }
                            postBody "b" 0 50, { X = 10; Y = 47 }
                        ]
                        []

                let { Assignments = building } = decideOn (pair 50 0)

                Expect.equal
                    (Map.tryFind "a" building)
                    (Some(taskId (Build "site-out")))
                    "the garrison spends the load it dug"

                Expect.notEqual
                    (Map.tryFind "b" building)
                    (Some(taskId (Harvest "src-out")))
                    "and the Post it is standing on admits no second heavy body while it does"

                // Pairwise on the incumbent's store alone: empty, it holds
                // the Harvest itself and the cap answers as it always did.
                let { Assignments = digging } = decideOn (pair 0 50)

                Expect.equal
                    (Map.tryFind "a" digging)
                    (Some(taskId (Harvest "src-out")))
                    "empty, the same body holds the same Post through the dig"

                Expect.notEqual
                    (Map.tryFind "b" digging)
                    (Some(taskId (Harvest "src-out")))
                    "and the second body is refused by the cap it always was"
            }
        ]
