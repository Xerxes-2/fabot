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

                let { Assignments = assignments } = decideOn snapshot

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

                let { Assignments = assignments } = decideOn snapshot

                Expect.isEmpty
                    (Map.toList assignments)
                    "a body with no Carry part cannot deliver energy"
            }

            test "a deposit's Harvest is for a body with no Carry part, and for nobody else" {
                // ADR 0057 decision 2's gate, and the whole of it: a Work part
                // to dig with and **no Carry at all**. That is what keeps an
                // [[anchor]] off the deposit — it is Work-heavy, it is
                // applicable to every source Harvest in the pool, and standing
                // on the mine [[post]] it would fill a store that ages it by
                // `floor(log10 store.T)` ticks a tick and never empty it, ADR
                // 0016 having shut its Withdraw and ADR 0046 its Refill.
                //
                // Pairwise, one Carry part apart, both bodies standing on the
                // mine Post so neither is separated by a walk.
                let holding body =
                    { mineColony with
                        Creeps = [ creepWith "h" 0 0 body ]
                        Spatial = mineColony.Spatial |> withCreepsAt [ "h", minePost ]
                    }
                    |> decideOn
                    |> fun decision -> Map.tryFind "h" decision.Assignments

                Expect.equal
                    (holding [ Work; Work; Move ])
                    (Some(taskId (Harvest "min-a")))
                    "the store-less body takes the deposit"

                Expect.equal
                    (holding [ Work; Work; Carry; Move ])
                    None
                    "and the Anchor-shaped body beside it takes nothing at all"
            }

            test "a source's Harvest is for a body that can carry the yield, and for nobody else" {
                // The other half of the same cut (#261). A store-less
                // [[miner]] reports `FreeCapacity = 0`, so the store disjunct
                // refuses it — and the vacancy disjunct behind it then offered
                // it the walk to any source with a Post it had not reached:
                // Work-heavy, not yet arrived, and every manned Post in the
                // colony reading as somewhere to go. Live that is 2,200 energy
                // of Work dribbling into a source container for a whole
                // 1,500-tick life, `Kept` from the tick it arrives because the
                // garrison reprieve is positional, while the season's deposit
                // goes undug and the Anchor row buys a replacement for a Post
                // `Capacity.garrisoning` will not let it have.
                //
                // Pairwise, one Carry part apart, both bodies a walk away from
                // the Post so the vacancy disjunct is the one under test.
                let holding body =
                    { haulColony with
                        Creeps = [ creepWith "h" 0 0 body ]
                        Spatial = haulRoom |> withCreepsAt [ "h", { X = 13; Y = 10 } ]
                    }
                    |> decideOn
                    |> fun decision -> Map.tryFind "h" decision.Assignments

                Expect.equal
                    (holding [ Work; Work; Work; Carry; Move ])
                    (Some(taskId (Harvest "src-a")))
                    "the premise: a heavy body with somewhere to put the yield walks to the rock"

                Expect.equal
                    (holding [ Work; Work; Work; Move ])
                    None
                    "and the store-less body beside it is offered no rock at all"
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
                let { Assignments = assignments } = decideFrom remembered snapshot

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
                        decideOn (
                            sourceColony
                                loneSourceRoom
                                [ lightWorker "w" energy free, { X = 13; Y = 10 } ]
                        )

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
                        decideOn (sourceColony loneSourceRoom [ body, { X = 13; Y = 10 } ])

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
                        decideOn (
                            sourceColony
                                postedSourceRoom
                                ((lightWorker "w" 0 450, { X = 13; Y = 10 }) :: bodies)
                        )

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
                        decideOn (
                            sourceColony
                                postedSourceRoom
                                [
                                    lightWorker "w" 0 450, { X = 13; Y = 10 }
                                    creepWith "a1" 0 50 garrison, { X = 11; Y = 10 }
                                ]
                        )

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
                        decideFrom (Map.ofList [ "w", taskId (Harvest "src-a") ]) colony

                    verdicts

                Expect.equal
                    (verdictsAt { X = 11; Y = 10 })
                    [ Verdict.Kept("w", taskId (Harvest "src-a")) ]
                    "on the Seat there is no walk left to price, and the dig runs to the brim"

                Expect.equal
                    (verdictsAt { X = 13; Y = 10 })
                    [
                        Verdict.Released(
                            "w",
                            taskId (Harvest "src-a"),
                            ReleaseReason.Rejected RejectReason.Inapplicable
                        )
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
                        decideOn (
                            sourceColony
                                room
                                [
                                    lightWorker "w" 0 450, { X = 13; Y = 10 }
                                    creepWith "a1" 0 50 sixWork, { X = 11; Y = 10 }
                                ]
                        )

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

[<Tests>]
let thoriumApplicabilityTests =
    testList
        "the Thorium leg's applicability"
        [
            // ADR 0057 decision 3: the Thorium arm is applicable to an **empty**
            // carrier and not to #232's half-empty one, because a body carries
            // one resource at a time here — a mixed load pours energy into a
            // reactor that refuses it and arrives at the decade cliff with the
            // wrong count in its store. Pairwise on the body's store alone: the
            // same hauler, on the same tile beside the same container, differing
            // in nothing but what it is already carrying.
            test "an empty carrier draws the mine; a half-loaded one does not" {
                let matchedWith body =
                    let colony =
                        { mineHaulColony with
                            Creeps = [ body ]
                            Spatial =
                                mineHaulColony.Spatial |> withCreepsAt [ "h1", { X = 12; Y = 10 } ]
                        }

                    Map.tryFind "h1" (decideOn colony).Assignments

                Expect.equal
                    (matchedWith (hauler "h1" 0 200))
                    (Some(taskId (Withdraw("can-min", Thorium))))
                    "nothing aboard is what makes a body the mine's"

                Expect.equal
                    (matchedWith (hauler "h1" 100 100))
                    (Some(taskId (Refill("sto-1", Energy))))
                    "half a load of energy aboard, and the body is a delivery and not an intake"
            }

            test "a two-hundred-unit container is worth a whole hauler's trip" {
                // #232's worth-the-trip line stays, and the stock-tier disjunct
                // answers it for this arm on the line's own stated reason: what
                // the line buys is the fall to the tier below, and there is none
                // below the Storage's tier. A body refused the mine has no
                // deeper intake to fall to — it would stand idle while the
                // container fills and the miner's next dig bleeds onto the
                // ground, which is 3.33 Thorium a tick against a container that
                // holds 2,000.
                let thin = mineHaulColony |> withMineStock 200

                let colony =
                    { thin with
                        Creeps = [ hauler "h1" 0 200 ]
                        Spatial = thin.Spatial |> withCreepsAt [ "h1", { X = 12; Y = 10 } ]
                    }

                Expect.equal
                    (Map.tryFind "h1" (decideOn colony).Assignments)
                    (Some(taskId (Withdraw("can-min", Thorium))))
                    "a fifth of a container is still the only Thorium there is"
            }

            test "a loaded carrier pours into the Storage, and takes no energy on the way" {
                // The delivery half, and the invariant that makes it one trip:
                // a body holding the season's ore is applicable to the Storage's
                // Thorium Refill and to **no energy intake at all** — not the
                // container under its feet, not a pile, not a rock. Pairwise on
                // the load alone: the same body, the same tile, energy in one
                // half and Thorium in the other, beside a container stocked with
                // six hundred of each.
                let colony load =
                    { mineHaulColony with
                        Spatial =
                            { mineHaulColony.Spatial with
                                Stores = Map.add "can-min" 600 mineHaulColony.Spatial.Stores
                            }
                            |> withCreepsAt [ "h1", { X = 13; Y = 10 } ]
                        Creeps = [ load ]
                    }

                Expect.equal
                    (Map.tryFind "h1" (decideOn (colony (hauler "h1" 0 200))).Assignments)
                    (Some(taskId (Withdraw("can-min", Energy))))
                    "the premise: an empty body beside that container draws its energy"

                Expect.equal
                    (Map.tryFind
                        "h1"
                        (decideOn (colony (hauler "h1" 0 200 |> carrying 150))).Assignments)
                    (Some(taskId (Refill("sto-1", Thorium))))
                    "with the ore aboard the same body has one Task: put it down"
            }

            test "a light body carrying Thorium is offered no rock either" {
                // The third energy intake (ADR 0057 decision 3). A worker that
                // took a load off the mineral container is Work-carrying and
                // half empty, and Harvest is the Feeding tier — so without the
                // clause it would outrank its own delivery, dig energy into the
                // same store and carry the pair around for the rest of its life.
                // Pairwise on the load alone, on the source fixture where the
                // rock is the only Task there is.
                let matchedWith body =
                    let colony = sourceColony loneSourceRoom [ body, { X = 13; Y = 10 } ]

                    Map.tryFind "w" (decideOn colony).Assignments

                Expect.equal
                    (matchedWith (lightWorker "w" 0 450))
                    (Some(taskId (Harvest "src-a")))
                    "the premise: an empty generalist digs"

                Expect.isNone
                    (matchedWith (lightWorker "w" 0 450 |> carrying 50))
                    "with the season's ore aboard it digs nothing"
            }

            test "a laden carrier keeps its sink when the mine container goes" {
                // #262. The ore shuts every energy intake and the body has no
                // Work to spend, so the Storage's Thorium Refill is the **only**
                // Task a laden hauler is ever applicable to — and while that
                // Refill was pooled off a standing mineral container the two
                // could disagree. The container is destroyed or decays and the
                // Layout re-places it as a site; for the whole of that window a
                // hauler mid-haul had no applicable Task at all, while the row's
                // census counted it living and cast no replacement: one carrier
                // out of the energy economy for up to 1,500 ticks. So the sink
                // is the Storage's own fact and not the mine's.
                //
                // Three readings, pairwise on the container alone.
                let gone = mineHaulColony |> withoutMineContainer

                let matchedIn colony body =
                    let colony =
                        { colony with
                            Creeps = [ body ]
                            Spatial = colony.Spatial |> withCreepsAt [ "h1", { X = 13; Y = 10 } ]
                        }

                    Map.tryFind "h1" (decideOn colony).Assignments

                Expect.equal
                    (matchedIn gone (hauler "h1" 100 100))
                    (Some(taskId (Refill("sto-1", Energy))))
                    "the premise: with the container gone the colony still has energy work"

                Expect.equal
                    (matchedIn mineHaulColony (hauler "h1" 0 200 |> carrying 150))
                    (Some(taskId (Refill("sto-1", Thorium))))
                    "and the laden body's one Task while the mine stands is to put the ore down"

                Expect.equal
                    (matchedIn gone (hauler "h1" 0 200 |> carrying 150))
                    (Some(taskId (Refill("sto-1", Thorium))))
                    "which the ground behind it going away does not take from it"
            }
        ]
