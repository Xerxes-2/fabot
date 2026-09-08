/// Which Tasks a body may hold at all: the parts it carries, and the
/// harvest it may reach.
module Fabot.Core.Tests.Decide.MatcherApplicabilityTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.MatcherFixtures

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
