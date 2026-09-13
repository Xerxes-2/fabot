/// The standing body, its own Refill, and the Post it raises (ADR 0053).
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
                // ADR 0046's whole claim, pairwise on the body and nothing
                // else: the same tile, the same pool, the same tier. The
                // site is the cheaper of the two Surplus Tasks for anyone
                // allowed to take it, so the generalist takes it; for the
                // upgrader row's body Build is inapplicable and Upgrade is
                // what is left. A standing body's Carry holds fifty energy
                // against eleven Work, so the trip would spend one tick
                // delivering for every tick of the walk out and back, with
                // eleven Work idle beside the buffer meanwhile.
                let site = [ "site-1", { X = 15; Y = 10 }, Site BuiltKind.Extension ]
                let sites = [ { Id = "site-1" } ]

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
                // ADR 0046 says the gate covers `6W/1C/1M`, and says the
                // rule is read off parts and never off a row name (ADR
                // 0006). Pinned against the generalist one rival at a time
                // — the same lane, the same site, one body swapped — so it
                // is the arithmetic `1 * 4 < 6` that is on trial and not
                // some Anchor-shaped exception.
                //
                // What the ADR claims about the *colony* is that this
                // changes nothing for the Anchor row, because a rank-0
                // Harvest holds it at its Post long before travel cost gets
                // a say — which is the next case. This one is what the rule
                // says when Harvest is not in the pool at all.
                //
                // And ADR 0046's own "or holds no Task at all" is what it
                // says here since ADR 0048: this creep stands one step
                // outside the controller's Upgrade Work Area, and a
                // Work-heavy body takes Upgrade only where it can already
                // act on it. The gate on trial is unchanged — Build is
                // still inapplicable to `6W/1C/1M` by `1 * 4 < 6` — and
                // the generalist beside it still takes that Build, which
                // is the pairwise the case above spells out.
                let site = [ "site-1", { X = 15; Y = 10 }, Site BuiltKind.Extension ]

                Expect.equal
                    (laneAssignment site [ { Id = "site-1" } ] anchorBody)
                    None
                    "one Carry against six Work is a commute, whichever row cast it and whichever way it walks"
            }

            test "a standing body does not repair either" {
                // The third of ADR 0046's three deliveries. A road at half
                // hits is the ordinary Repair (ADR 0034), one step from the
                // creep and three from the controller, so the pairwise
                // reads exactly as the Build one does.
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
                // ADR 0016's gate is `Work > Move` and ADR 0019's is a Work
                // part, and ADR 0046 opens no third one (the ticket's own
                // trap): the upgrader row is at `Work = Move` with a Work
                // part, so the buffer it stands beside is exactly what it
                // draws from. Empty, so the two Surplus Tasks are
                // inapplicable for want of energy and the Withdraw is the
                // whole of what is left.
                let empty = creepWith "upgrader" 0 50 (bodyFor upgraderPattern 1800)

                Expect.equal
                    (laneAssignment [] [] empty)
                    (Some(taskId (Withdraw("can-buf", Energy))))
                    "the row drinks from the buffer at its feet, which is why it stands there"
            }

            test "a buffer under the worth-the-trip line is still this row's drink" {
                // #232 gates a Withdraw on the store holding half the
                // asking body's free room — a line that prices a *trip*,
                // and this row makes none: it lives at that store, and ADR
                // 0046 opens no new gate on its Withdraw (the same
                // exception #205 makes of a site under a creep's own feet).
                // Without the exemption a buffer holding twenty-four left
                // an `11W/1C/11M` upgrader with no applicable Task at all —
                // Build, Repair and Refill are shut to it, and Upgrade
                // needs energy it could not go and get.
                //
                // Pairwise on the body alone: the same twenty-four in the
                // same buffer, and the generalist that would have to walk
                // there is refused by the line as any walking body is.
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
                // A bootstrapping room builds its bank before its
                // controller: the site is feeding-tier while the controller
                // is under `Tuning.BootstrapLevel` with a spawn standing, and
                // surplus like any home site from RCL3 up. Pairwise on the
                // level alone: the same lane, the same worker, the same
                // site, and the match factor says which tier decided.
                //
                // Measured against the **flow** and no longer against the
                // Upgrade beside it (#234): a surplus Build outranks that
                // Upgrade by a rung now, so the site wins on rank at either
                // level and the controller has stopped being an instrument.
                // The hungry spawn `bufferLaneFlow` stands at the lane's far
                // end is one: on the feeding tier the site ties it and takes
                // the worker on price, and a rung below it the site is
                // outranked by it however near it stands.
                let lane level =
                    bufferLaneFlow
                        [ "site-1", { X = 15; Y = 10 }, Site BuiltKind.Extension ]
                        [ { Id = "site-1" } ]
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
                // #206. The buffer empty, a stocked Storage and a pile of
                // 400 both standing five tiles down the lane: the generalist
                // walks to one of them, the standing body stands. Its one
                // Carry is a trip's worth, and the trip is the commute ADR
                // 0046 refuses — live it was fifty tiles across a Seam for
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
                // ADR 0046's claim about the Anchor row, at the seam it is
                // made about: with Harvest in the pool the rank settles it
                // (feeding above surplus) and the gate never gets a say, so
                // the row's behaviour is unchanged. Pairwise on the pool
                // rather than on the body — the same Anchor, once with the
                // site and once without — because it is the site that is on
                // trial here.
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
                        [ { Id = "site-1" } ]
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
                // The trap ADR 0046's rule is written to walk into and
                // not be caught by: a hauler has no Work at all, so
                // `Carry * 4 < Work` reads `8 * 4 < 0` — false — and the
                // row whose entire life is delivery keeps every delivery
                // it ever had. Pinned because the gate's arithmetic could
                // so easily have been written the other way round.
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
                // The state ADR 0046's Anchor consequence is qualified
                // for, and the one the Build arm's own #157 comment
                // records as observed: an Anchor whose Post has no
                // standing container to catch its overflow fills up, and
                // with no Harvest left in its pool the rank that used to
                // settle everything has nothing to settle. Before the gate
                // this body took the spawn's feeding-tier Refill two steps
                // away; now it holds no Task at all here, and takes a
                // pooled Upgrade at any distance where one exists.
                //
                // Pinned rather than left to be discovered: it is the
                // whole live cost of writing the gate as a prohibition,
                // and the glossary's "the rule only writes down what a
                // rank-0 Harvest was already doing" is true only where a
                // Harvest is in the pool.
                Expect.equal
                    (deliveryAssignment anchorBody)
                    None
                    "one Carry against six Work is the same commute, and the same prohibition"
            }

            test "the row's own small cast is outside the gate with the rest of the band" {
                // `3W/1C/3M`, the upgrader row's body at the RCL2 bank of
                // 550: the row, and not a standing body — `1 * 4 < 3` is
                // false — so it delivers like the generalist it is read
                // back to. The band under an 800 bank pinned at the seam
                // rather than only in the arithmetic above, because it is
                // the gate and not the sizing rule that the band is
                // interesting for (ADR 0046, Consequences; #187 settled it
                // the other way, the quota hiring none at a bank whose cast
                // this gate reads back to the generalist — a body the row
                // could never count).
                Expect.equal
                    (deliveryAssignment (creepWith "upgrader" 50 0 (bodyFor upgraderPattern 550)))
                    (Some(taskId (Refill("spawn-1", Energy))))
                    "three Work against a fifty-energy load is not yet a commute"
            }

            test "the generalist beside it refills as it always has" {
                // The near miss, and the reason the ratio is four rather
                // than something tighter: `9W/9C/9M` is one Carry per
                // Work where the gate's line is one per four, so the whole
                // worker row is outside it at every bank (ADR 0003's
                // parity keeps it there).
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
                // #205's whole mechanism, one body and one tile. Two rules
                // had closed the door between them: ADR 0045 empties an
                // unposted outpost source's Work Area, so no heavy body
                // walks there at all, and ADR 0046 shuts Build to a
                // standing body. What was left was the worker row
                // commuting a Seam and fifty tiles at fifty energy a trip
                // against 5,000 progress — thousands of ticks of lost
                // income every time an invader demolishes a container.
                //
                // The site is a Post, so the rock has a Work Area again;
                // the site is under the body's feet, so Build is not the
                // commute ADR 0046 forbids. The pair then alternates on
                // the store alone: `FreeCapacity = 0` ends the dig (a site
                // catches no overflow, ADR 0024) and carried energy is
                // what Build asks for.
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
                // Pairwise, one rival at a time: the same body on the same
                // tile in the same colony, with a road site there instead
                // of a container site. Nothing on that Seat is a Post, so
                // the outpost rock's Work Area is empty for a heavy body
                // (ADR 0045) and Harvest is inapplicable; the site is no
                // Post either, so ADR 0046's gate stands and Build is shut
                // to a standing body. Which leaves the outpost rock
                // offering this body nothing at all: empty it is thrown
                // back on the home room's own bare-Seat fallback thirty
                // tiles and a Seam away — the theft ADR 0045 records and
                // leaves standing — and full it holds no Task, which is
                // #197's shape and stays #197's.
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
                // The Anchor quota's own input (`Atlas.postCount`, read by
                // `planSpawns`): a Post is a garrison place, and the tile a
                // container is going up on is one — otherwise the body the
                // rule is written for is never cast and never walks there.
                // Pairwise on the site and nothing else.
                let count kind =
                    Atlas.postCount (
                        Atlas.ofView (raisingColony kind (postBody "w" 0 50) { X = 10; Y = 45 })
                    )

                Expect.equal (count BuiltKind.Container) 1 "the container site is a Post"
                Expect.equal (count BuiltKind.Road) 0 "and a site of any other kind is none"
            }

            test "the Work Area is the site tile and not the Seat beside it" {
                // ADR 0020's narrowing, over the Post #205 adds: the other
                // Seat of the same rock is standing room for a light body
                // and not for this one, so a heavy body that lands there is
                // walked onto the site rather than left digging beside it.
                // Which is the whole reason the site is a Post and not a
                // second bare-Seat fallback — a body that dug from (10,47)
                // would put its twelve a tick on the floor.
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
                // ADR 0006, and #157 unmoved with it: the exception is a
                // fact about where the body is standing, so the generalist
                // standing on the same tile takes the same Build it always
                // took. Pairwise on the body alone.
                Expect.equal
                    (matchOf (
                        raisingColony BuiltKind.Container (worker "w" 50 0) { X = 10; Y = 45 }
                    ))
                    (Some(taskId (Build "site-out"), MatchFactor.OnlyCandidate))
                    "the worker row's own Build across the Seam is untouched"
            }

            test "a home rock's container site is the same Post" {
                // #205's rule is not an outpost rule: the Seat under a
                // container site is a garrison place in every room, so an
                // RCL2 colony raises its own first source container the
                // same way. The room is left out of the rule deliberately
                // — what the two halves of ADR 0045 differ on is the bare
                // Seat *fallback*, and a rock with a site on a Seat has a
                // Post and never reaches it.
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

                // Pairwise, the same body and tile with a road site there:
                // at home the bare-Seat fallback is still ADR 0020's, so
                // the empty body keeps its dig — and the full one is back
                // inside ADR 0046's gate with nothing to do.
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
                // The split #205 draws, at the seam ADR 0042 draws it on. A
                // source whose energy goes into 5,000 progress pays no haul
                // term and feeds no mouth at home, so `Decide.isPosted`
                // reads the standing census while the garrison reads the
                // whole one. Pairwise on the structure: the same Seat,
                // pending against built.
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
                // Criterion 1's own wording — a body *beside* the site,
                // full — and the case the whole mechanism rests on: the
                // Build exception asks for the exact tile, so what saves a
                // full body one step off it is ADR 0048's walk-home
                // reprieve, which reads `Atlas.postsOf`. Had that call site
                // been left on the standing census with `isPosted`, this
                // body would hold no Task at all: Harvest shut by a full
                // store on a site that catches no overflow, Build shut by
                // ADR 0046, Withdraw and Repair and Refill shut with it —
                // #197's shape in the one case this ticket says it has
                // given an exit to.
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
                // The same reprieve with #258's unmanned-Post condition on
                // it, on the one shape where the two censuses part: a rock
                // with a **standing** container Post and a container site
                // Post beside it — the shape "the Anchor quota counts
                // Posts" above declares first-class, for the whole of the
                // window #205 exists for. Read against the standing census
                // the rock would be occupied by the garrison on the built
                // container, and the full body one step off its own site
                // would lose Harvest with Build already asking for the
                // exact tile (#205, #234) and everything else shut (ADR
                // 0016, ADR 0046, ADR 0048) — no Task at all, for the rest
                // of its life, with the site left unraised. The condition
                // reads every Post, so the standing room it is walking to
                // is its own.
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
                        ConstructionSites = [ { Id = "can-a" } ]
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
                // #157's budget is `Tuning.OutpostBuilders` spread over
                // the sites it has lifted, and every word of its argument is about the
                // home room's surplus work stopping "for the fifty ticks
                // each of them spends crossing". A body standing on the
                // site spends none of those, so it is outside what the
                // number prices — counted inside it, the two loaded
                // workers holding the Build through their whole commute
                // leave the garrison full on the progress with Harvest
                // shut behind it and no Task at all, which is exactly the
                // stuck-full-Anchor shape #205 exists to end.
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

                // Pairwise on the body and nothing else: the row the
                // budget was written for is capped exactly as #157 had it,
                // so a third loaded worker still waits at home.
                let { Verdicts = withThird } =
                    decideFrom held (crowd [ worker "w3" 50 0, { X = 10; Y = 4 } ])

                Expect.isNone
                    (matchedOf "w3" withThird)
                    "and the third commuter is still refused: #157's number is unmoved"
            }

            test "the Post is held by the body standing on it, digging or building" {
                // ADR 0024's Post cap is "one Anchor per Post", and on a
                // standing container the garrison never lets it go: the
                // overflow reprieve keeps its Harvest applicable through a
                // full store. On a site there is no overflow, so the pair
                // alternates and the slot would read as free on every
                // build tick — admitting a second heavy body onto the one
                // tile the first is standing on, and releasing the
                // incumbent from Build onto a Seam-crossing walk to the
                // home rock the tick it empties. So the slot is held by
                // where the body stands and not by what it holds.
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
