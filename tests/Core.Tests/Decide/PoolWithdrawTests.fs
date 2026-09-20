/// Withdraw capacity, the piles and tombstones a Pickup names, and a
/// source container that is full.
module Fabot.Core.Tests.Decide.PoolWithdrawTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.PoolFixtures

[<Tests>]
let withdrawCapacityTests =
    testList
        "withdraw capacity"
        [
            test "a container that fills one hauler takes one; the rest walk to the full one" {
                // The matching key puts cost ahead of load, so without a
                // capacity every empty hauler picks the nearest stocked
                // container. The stock is the cap: `ceil(400 / 400)` is one
                // seat. The far store is 1,800 and not a full 2,000, because
                // a full source container is lifted a rung of its own.
                let { Assignments = split } = decideOn (crowdColony 400 1800 crowdOfThree)

                Expect.equal
                    (drawersOf split "can-near")
                    [ "h1" ]
                    "one hauler's worth of stock admits one hauler"

                Expect.equal
                    (drawersOf split "can-far")
                    [ "h2"; "h3" ]
                    "and the crowd it turns away walks to the store that can fill it"

                // The pairwise control: nothing changed but the near store's
                // stock.
                let { Assignments = whole } = decideOn (crowdColony 2000 2000 crowdOfThree)

                Expect.equal
                    (drawersOf whole "can-near")
                    [ "h1"; "h2"; "h3" ]
                    "stocked for five trips, the near container keeps the whole crowd"
            }

            test "the cap rounds up: one load exactly is one seat, one energy more is two" {
                // 400 is exactly the cast hauler's load; 401 is a fraction of
                // a second trip nobody would be sent for.
                let seatsAt stock =
                    let { Assignments = assignments } =
                        decideOn (crowdColony stock 1800 (List.truncate 2 crowdOfThree))

                    drawersOf assignments "can-near"

                Expect.equal (seatsAt 400) [ "h1" ] "one whole load is one seat"

                Expect.equal
                    (seatsAt 401)
                    [ "h1"; "h2" ]
                    "one energy past it is two: the cap rounds up"

                Expect.equal
                    (seatsAt 800)
                    [ "h1"; "h2" ]
                    "and two whole loads are two, with no third body to prove it wider"
            }

            test "a hauler still walking holds its seat: the second is turned away" {
                // Counted at arrival: the holder is fourteen steps out and
                // the candidate on the doorstep. A cap counting only creeps
                // on the tile would land both on 400 energy.
                let {
                        Assignments = assignments
                        Verdicts = verdicts
                    } =
                    decide
                        (crowdColony 400 2000 [ "h1", { X = 25; Y = 10 }; "h2", { X = 11; Y = 10 } ])
                        (Map.ofList [ "h1", taskId (Withdraw("can-near", Energy)) ])
                        (Set.singleton "h2")
                        None

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw("can-near", Energy))))
                    "the walking holder keeps the store it was already sent to"

                Expect.equal
                    (drawersOf assignments "can-far")
                    [ "h2" ]
                    "and the creep on the doorstep is sent to the far store instead"

                let rejections =
                    verdicts
                    |> List.tryPick (function
                        | Verdict.Scoring("h2", rows) ->
                            rows
                            |> List.filter (function
                                | Candidate.Rejected _ -> true
                                | Candidate.Scored _ -> false)
                            |> Some
                        | _ -> None)

                Expect.equal
                    rejections
                    (Some
                        [
                            Candidate.Rejected(
                                taskId (Withdraw("can-near", Energy)),
                                RejectReason.CapacityFull
                            )
                            Candidate.Rejected(taskId (Upgrade "ctrl-1"), RejectReason.Inapplicable)
                        ])
                    "the near store names the cap and no gate before it — not the body, not the price; the Upgrade it has no Work for is the pool's only other loss"
            }

            test "the Storage is not special-cased: the same formula, and at 130k no cap" {
                // A Storage down to one trip's worth admits one drawer, as a
                // container does; a real stock divides into hundreds of
                // trips, so the cap never binds.
                let { Assignments = thin } = decideOn (stockCrowdColony 400)

                Expect.equal
                    (drawersOf thin "sto-c")
                    [ "h1" ]
                    "a stock holding one trip's worth admits one hauler"

                let { Assignments = full } = decideOn (stockCrowdColony 130000)

                Expect.equal
                    (drawersOf full "sto-c")
                    [ "h1"; "h2"; "h3" ]
                    "and a colony's real stock caps at 325 trips, which is no cap at all"
            }

            test "the upgrade buffer divides by the worker row that draws from it" {
                // No body without a Work part may take the buffer, so its
                // drawers are the worker row and its 900 is two cast
                // workers' loads. Priced by the hauler's 1,200 a trip the
                // same 900 would be one seat.
                let { Assignments = split } = decideOn (bufferCrowdColony 900)

                Expect.equal
                    (drawersOf split "can-buf")
                    [ "w1"; "w2" ]
                    "two worker loads standing in the buffer admit two workers"

                // The divisor is the store's and never the candidate's: the
                // ordinary container admits one hauler load.
                Expect.equal
                    (drawersOf split "can-far")
                    [ "w3" ]
                    "and an ordinary container's 900 is one hauler load, however the body that walks to it is built"

                // The pairwise control: three loads instead of two.
                let { Assignments = whole } = decideOn (bufferCrowdColony 1350)

                Expect.equal
                    (drawersOf whole "can-buf")
                    [ "w1"; "w2"; "w3" ]
                    "three loads keep the whole crowd upgrading standing still, which is what a buffer is for"
            }

            test "the buffer's two rows are capped apart, each by its own load" {
                // One store, two rows, two loads: the generalist carries 450
                // at this bank and the standing body fifty, so a 400 buffer
                // is one trip for the first and eight for the second.
                // Pairwise on the class alone.
                let standingCrowd =
                    { bufferCrowdColony 400 with
                        Creeps =
                            [
                                for name, _ in bufferCrowd ->
                                    creepWith name 0 50 (bodyFor upgraderPattern 1800)
                            ]
                    }

                let { Assignments = standing } = decideOn standingCrowd

                Expect.equal
                    (drawersOf standing "can-buf")
                    [ "w1"; "w2"; "w3" ]
                    "fifty energy a trip divides 400 into eight seats, so the row that lives there all drinks"

                let { Assignments = generalists } = decideOn (bufferCrowdColony 400)

                Expect.equal
                    (drawersOf generalists "can-buf")
                    [ "w1" ]
                    "and the generalists' own share is unchanged: one load standing is one of them"
            }
        ]

/// The tombstone field with `units` of the season's ore beside its energy.
/// `Tombstone` names no resource, so the Thorium map alone makes this a
/// case about ore.
let private withTombOre units (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Thorium = Map.add "tomb-1" units colony.Spatial.Thorium
            }
    }

[<Tests>]
let pickupTaskTests =
    testList
        "the pile and the tombstone"
        [
            test "a pile worth a whole trip outbids the container underfoot; a smaller one does not" {
                // Nothing but travel cost separates a pile from a half-full
                // container once apart, so the ground kept its energy until
                // it decayed. A pile holding half a hauler unit's load or
                // more is a trip of its own.
                let matched colony =
                    let { Verdicts = verdicts } = decideOn colony

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h1", task, factor) -> Some(task, factor)
                        | _ -> None)

                Expect.equal
                    (matched (pileDownTheLane 800 600))
                    (Some(taskId (Pickup("pile-a", Energy)), MatchFactor.Rank))
                    "half the row's load five tiles off beats eight hundred at its feet"

                // The pairwise control: one energy under the line. A
                // priority is a colony-wide scalar, so lifting every pile
                // would send this body five tiles for a mouthful.
                Expect.equal
                    (matched (pileDownTheLane 800 599))
                    (Some(taskId (Withdraw("can-a", Energy)), MatchFactor.TravelCost))
                    "one under half a load: rank ties and the near store wins on price"

                Expect.equal
                    (matched (pileDownTheLane 800 100))
                    (Some(taskId (Withdraw("can-a", Energy)), MatchFactor.TravelCost))
                    "and a pile at the threshold is still the distance's to decide"

                // Pairwise on the stock: a full source container is two
                // rungs up against the pile's one, because the engine drops
                // a garrison's overflow onto its tile only once it is full.
                Expect.equal
                    (matched (pileDownTheLane Engine.containerCapacity 600))
                    (Some(taskId (Withdraw("can-a", Energy)), MatchFactor.Rank))
                    "two thousand full outranks the whole trip on the ground"
            }

            test "the whole trip's rung is a rung over the whole feeding tier" {
                // A rank is a colony-wide scalar: the Pickup steps up
                // against every Feeding Task. Each colony holds exactly two
                // Tasks, the rival under the body's feet.
                let matched colony =
                    let { Verdicts = verdicts } = decideOn colony

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h1", task, factor) -> Some(task, factor)
                        | _ -> None)

                // The delivery half: a half-loaded hauler one step from a
                // spawn with 300 free walks nineteen tiles for the pile.
                Expect.equal
                    (matched (pileAgainstAHungrySpawn 1800 600))
                    (Some(taskId (Pickup("pile-a", Energy)), MatchFactor.Rank))
                    "half a load on the ground outranks the hungry spawn at the body's feet"

                Expect.equal
                    (matched (pileAgainstAHungrySpawn 1800 599))
                    (Some(taskId (Refill("spawn-1", Energy)), MatchFactor.TravelCost))
                    "one energy under the line the spawn is the near Task again"

                // A tombstone is a Withdraw like any other and only a full
                // container carries a rung.
                Expect.equal
                    (matched (pileAgainstATombstone 600))
                    (Some(taskId (Pickup("pile-a", Energy)), MatchFactor.Rank))
                    "and it outranks a tombstone's 1,500 one tile away"

                Expect.equal
                    (matched (pileAgainstATombstone 599))
                    (Some(taskId (Withdraw("tomb-a", Energy)), MatchFactor.TravelCost))
                    "which under the line is travel cost's again"
            }

            test
                "under a bank of 450 the line is the pooling threshold and every pile takes the rung" {
                // `haulerLoad` is `100 * (bank / 150)`, so half a load is a
                // hundred at RCL1's bank of 300 — `Tuning.PickupThreshold`
                // itself — and the smallest pooled pile is already a whole
                // trip. The distance-only rung begins at RCL2.
                let matched colony =
                    let { Verdicts = verdicts } = decideOn colony

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h1", task, factor) -> Some(task, factor)
                        | _ -> None)

                Expect.equal
                    (matched (pileAgainstAHungrySpawn 300 100))
                    (Some(taskId (Pickup("pile-a", Energy)), MatchFactor.Rank))
                    "at RCL1 a threshold-sized pile nineteen tiles off takes the rung"

                Expect.equal
                    (matched (pileAgainstAHungrySpawn 550 100))
                    (Some(taskId (Refill("spawn-1", Energy)), MatchFactor.TravelCost))
                    "and at RCL2, where the cast has outgrown twice the threshold, it does not"
            }

            test "a pile on a container's own tile is taken before the container" {
                // The live shape (2026-09-07): a hauler at a full container
                // ignored the 1,859 lying on it. Same tier, tile and travel
                // cost, so pool order decided. The pile loses
                // `ceil(amount / 1000)` a tick and the container nothing.
                let { Assignments = together } = decideOn (sameTilePileColony { X = 10; Y = 10 })

                Expect.equal
                    (Map.tryFind "h1" together)
                    (Some(taskId (Pickup("pile-a", Energy))))
                    "one tile, two stores: the decaying one first"

                // The pairwise control: the same hauler, the same container,
                // The pairwise control: the pile ten tiles down the lane. 150
                // is an eighth of the row's load, so the trip rung is not
                // what this reads.
                let { Assignments = apart } = decideOn (sameTilePileColony { X = 20; Y = 10 })

                Expect.equal
                    (Map.tryFind "h1" apart)
                    (Some(taskId (Withdraw("can-a", Energy))))
                    "a pile ten tiles off moves nothing: the container underfoot is still the flow"

                // And a *full* container outranks the pile on its own tile
                // And a full container outranks the pile on its own tile: the
                // 2,000 is the flow, and the reflex takes the pile for free.
                let full =
                    let colony = sameTilePileColony { X = 10; Y = 10 }

                    { colony with
                        Spatial =
                            { colony.Spatial with
                                Stores =
                                    Map.add "can-a" Engine.containerCapacity colony.Spatial.Stores
                            }
                    }

                let { Assignments = brimming } = decideOn full

                Expect.equal
                    (Map.tryFind "h1" brimming)
                    (Some(taskId (Withdraw("can-a", Energy))))
                    "full, the container is drawn first and the reflex takes the pile beside it"
            }

            test "the piled container keeps its place against every other store" {
                // Travel cost never overturns a priority, so stepping the
                // Withdraw down put that container behind every other store
                // at any distance — switched on exactly when the container
                // is full and most needs emptying. Two haulers, a piled near
                // container and an unpiled far one.
                let colony =
                    { bareRespawn with
                        Bank = bank 150 150
                        Sources = []
                        Creeps = [ hauler "h1" 0 100; hauler "h2" 0 100 ]
                        Spatial =
                            { spatial [] crowdField with
                                Stores =
                                    Map.ofList [ "can-near", 400; "can-far", 400; "pile-a", 100 ]
                            }
                            |> withTargets
                                [
                                    "can-near", { X = 10; Y = 10 }, Structure BuiltKind.Container
                                    "can-far", { X = 30; Y = 10 }, Structure BuiltKind.Container
                                    "pile-a", { X = 10; Y = 10 }, (Dropped Energy)
                                ]
                            |> withCreepsAt [ "h1", { X = 10; Y = 11 }; "h2", { X = 14; Y = 10 } ]
                    }

                let { Assignments = assignments } = decideOn colony

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Pickup("pile-a", Energy))))
                    "the pile's own hauler still takes the decaying copy first"

                // The pile's capacity is one body, so the second hauler
                // chooses between the containers on travel cost alone.
                Expect.equal
                    (Map.tryFind "h2" assignments)
                    (Some(taskId (Withdraw("can-near", Energy))))
                    "and the rest of the row is not sent past the full store beside it"
            }

            test "a pile past the threshold hires a hauler ten tiles off; one under it hires nobody" {
                // 193 energy of death drop at W13S28 36,21 with nobody near
                // enough for the reflex to reach it.
                let walk = decideOn (pileTaskColony 150 [ "h1", { X = 20; Y = 10 } ])

                Expect.equal
                    (Map.tryFind "h1" walk.Assignments)
                    (Some(taskId (Pickup("pile-a", Energy))))
                    "150 on the ground is worth ten tiles of walking"

                Expect.isEmpty
                    (pickups walk.Intents)
                    "and out of reach it is walking, not picking: the act waits for arrival"

                Expect.isNonEmpty
                    (moveIntentsFor "h1" walk.Intents)
                    "what a Task buys over the reflex is exactly this step"

                // The pairwise control: the same creep on the same tile
                // with the same everything, and 80 energy on the ground.
                let small = decideOn (pileTaskColony 80 [ "h1", { X = 20; Y = 10 } ])

                Expect.equal
                    (Map.tryFind "h1" small.Assignments)
                    None
                    "under the threshold the pile is the reflex's business and nobody walks"
            }

            test "the threshold is inclusive: a hundred exactly is worth the trip" {
                // Two CARRY parts' worth is the smallest load that pays for
                // a walk made for the pile alone.
                let assignmentAt amount =
                    let { Assignments = assignments } =
                        decideOn (pileTaskColony amount [ "h1", { X = 20; Y = 10 } ])

                    Map.tryFind "h1" assignments

                Expect.equal
                    (assignmentAt 100)
                    (Some(taskId (Pickup("pile-a", Energy))))
                    "at the line, pooled"

                Expect.equal (assignmentAt 99) None "one energy short of it, not"
            }

            test "the pile that arrives is picked up once, and the bubble says so" {
                // The reflex asks for the same act on this tick, so the
                // count is the assertion: one creep's one pickup spelt twice
                // would over-report the CPU line's accepted-intent column.
                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decideOn (pileTaskColony 150 [ "h1", { X = 10; Y = 11 } ])

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Pickup("pile-a", Energy))))
                    "standing on its doorstep it still holds the Task"

                Expect.equal
                    (pickups intents)
                    [ "h1", "pile-a" ]
                    "it asks the engine for it, exactly once between the Task and the reflex"

                Expect.contains
                    intents
                    (SayCreep("h1", "🧲"))
                    "one glyph per Task, and this Task has its own"
            }

            test
                "the creep beside a hired picker still asks: the pair is deduplicated, not the pile" {
                // The deduplication drops one creep's Intent spelt twice,
                // nothing else: the pile's capacity is one body, and h2's
                // reflex pickup is free energy.
                let { Intents = intents } =
                    decideOn (
                        pileTaskColony 100 [ "h1", { X = 10; Y = 11 }; "h2", { X = 11; Y = 10 } ]
                    )

                Expect.equal
                    (pickups intents |> List.sort)
                    [ "h1", "pile-a"; "h2", "pile-a" ]
                    "one ask apiece: the hired picker's own, and the bystander's reflex"
            }

            test "a pile decaying under the threshold releases the hauler still walking to it" {
                // The threshold gates persistence as well as entry, because
                // the pool is rebuilt creep-blind every tick: one energy
                // under the line and the walk already spent bought nothing.
                let held = Map.ofList [ "h1", taskId (Pickup("pile-a", Energy)) ]

                let standing = decideFrom held (pileTaskColony 100 [ "h1", { X = 20; Y = 10 } ])

                Expect.contains
                    standing.Verdicts
                    (Verdict.Kept("h1", taskId (Pickup("pile-a", Energy))))
                    "at the line the walk stands"

                let decayed = decideFrom held (pileTaskColony 99 [ "h1", { X = 20; Y = 10 } ])

                Expect.contains
                    decayed.Verdicts
                    (Verdict.Released(
                        "h1",
                        taskId (Pickup("pile-a", Energy)),
                        ReleaseReason.TaskGone
                    ))
                    "one energy under it, ten tiles from home, and the trip is over"
            }

            test "the pile's amount is its capacity: 150 admits two of the three haulers" {
                // `ceil(150 / 100)` is two bodies, and all three stand one
                // step from the pile's Work Area.
                let { Assignments = split } = decideOn (pileTaskColony 150 crowdOfThree)

                Expect.equal
                    (pickersOf split "pile-a")
                    [ "h1"; "h2" ]
                    "one and a half loads on the ground hire two haulers"

                // The pairwise control: the same three creeps on the same
                // tiles, nothing changed but the amount.
                let { Assignments = whole } = decideOn (pileTaskColony 300 crowdOfThree)

                Expect.equal
                    (pickersOf whole "pile-a")
                    [ "h1"; "h2"; "h3" ]
                    "three loads take the whole crowd"
            }

            test "a Work-heavy body never picks a pile up (ADR 0016)" {
                // A heavy body's intake is digging, and a pile is not a dig.
                // Pairwise on the body alone.
                let bodied body =
                    let colony = pileTaskColony 150 [ "a1", { X = 12; Y = 10 } ]

                    { colony with
                        Creeps = [ creepWith "a1" 0 50 body ]
                    }

                let { Assignments = heavy } = decideOn (bodied [ Work; Work; Carry; Move ])

                Expect.equal (Map.tryFind "a1" heavy) None "more Work than Move: no Pickup"

                let { Assignments = balanced } = decideOn (bodied [ Work; Work; Carry; Move; Move ])

                Expect.equal
                    (Map.tryFind "a1" balanced)
                    (Some(taskId (Pickup("pile-a", Energy))))
                    "the same body at Work <= Move picks it up"
            }

            test "a tombstone is a store: its 408 is withdrawn, and an empty one pools nothing" {
                // 408 energy in a tombstone in the home room while the colony
                // dug. The engine's `withdraw` is one method over every
                // store.
                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decideOn (tombColony 408 [ "h1", { X = 11; Y = 10 } ])

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw("tomb-1", Energy))))
                    "a store with a clock on it is drawn like any other"

                Expect.contains
                    intents
                    (WithdrawFromStore("h1", "tomb-1", Energy, None))
                    "and the act is withdraw, never pickup"

                // The pairwise control: the same tombstone on the same
                // tile, drawn dry.
                let { Assignments = spent } = decideOn (tombColony 0 [ "h1", { X = 11; Y = 10 } ])

                Expect.equal (Map.tryFind "h1" spent) None "an empty store is no Task"
            }

            test "and the season's ore in one is drawn the same way; an energy-only one offers none" {
                // A courier that died with ore aboard left 175 T in a store
                // no rule could name. The engine's `withdraw` takes a
                // tombstone or a ruin for any resource (`@screeps/engine`
                // `src/game/creeps.js` names `globals.Tombstone` and
                // `globals.Ruin`). The projection half is `ViewTests`' "a
                // tombstone's ore in a room she owns rides whole".
                let ore = tombColony 0 [ "h1", { X = 11; Y = 10 } ] |> withTombOre 175

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decideOn ore

                Expect.equal
                    (Map.tryFind "h1" assignments)
                    (Some(taskId (Withdraw("tomb-1", Thorium))))
                    "ore in a store with a clock on it is drawn like ore in a container"

                Expect.contains
                    intents
                    (WithdrawFromStore("h1", "tomb-1", Thorium, None))
                    "and the act names the ore and no amount: the 999-unit load is the Storage draw's alone"

                // The cap is the holding over the row's cast at this bank —
                // 150 buys `[2 Carry; 1 Move]`, a hundred — read off the pool
                // so it moves with the row.
                Expect.equal
                    (partCountIn (bodyFor haulerPattern ore.Bank.Capacity) Carry
                     * Engine.carryPartCapacity)
                    100
                    "the premise: the divisor is the hauler row's own cast at this bank"

                Expect.equal
                    (poolOn ore
                     |> List.tryPick (fun pooled ->
                         if pooled.Task = Withdraw("tomb-1", Thorium) then
                             Capacity.capOf CapScope.Everyone pooled.Capacity
                         else
                             None))
                    (Some 2)
                    "capped by what it holds of the resource named, like every other Withdraw: 175 is two trips"

                // The pairwise control: energy only. `Tombstone` carries no
                // resource, so it is the holding that admits the Task.
                Expect.isFalse
                    (planTasksOn (tombColony 408 [ "h1", { X = 11; Y = 10 } ]) noThreats
                     |> List.contains (Withdraw("tomb-1", Thorium)))
                    "an energy-only tombstone pools no ore draw"
            }

            test
                "a tombstone's ore takes the pile's rung, where a container's under the cliff takes none" {
                // The draw sits on the Storage's tier and so does the bank's
                // energy Withdraw, so a rungless ore draw loses every
                // travel-cost tie to a Storage in the middle of the room. A
                // tombstone drops its whole store as piles when it decays.
                // Pairwise on the kind of store alone.
                // same 175 units, under the contact cliff both times.
                let rankOf colony task =
                    poolOn colony
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = task then Some pooled.Priority else None)

                Expect.equal
                    (rankOf (mineHaulColony |> withMineStock 175) (Withdraw("can-min", Thorium)))
                    (Some(priorityOfTier StockDraw))
                    "the premise: a container under the cliff is decision 3's rungless intake"

                Expect.equal
                    (rankOf
                        (tombColony 0 [ "h1", { X = 11; Y = 10 } ] |> withTombOre 175)
                        (Withdraw("tomb-1", Thorium)))
                    (Some(priorityOfTier StockDraw + rankOfRung OneRungUp))
                    "the same ore in a store that ends takes the Thorium pile's rung, and for the pile's reason"
            }

            test "a tombstone keeps no construction site off its tile" {
                // A tombstone stands wherever a creep happened to die, and a
                // plan that moved with it would be a function of that
                // accident.
                let bare = atLevel 2 (openRoom 3)

                let littered =
                    atLevel
                        2
                        (openRoom 3 |> withTargets [ "tomb-1", { X = 24; Y = 24 }, Tombstone ])

                let placedWith = decideOn littered
                let placedWithout = decideOn bare

                Expect.equal
                    (placedTiles placedWith.Intents)
                    (placedTiles placedWithout.Intents)
                    "the Layout does not see tombstones"
            }

            test "a pile ties a container: one tier, and the tie goes to the pile" {
                // Equal cost is how to read the tier off one match. The
                // piles stand in pool order before the Withdraws: a
                // container keeps what it holds and a pile loses
                // `ceil(amount / 1000)` a tick. At the 1,800 bank 150 is
                // well under half the row's load and carries no rung.
                let colony =
                    { bareRespawn with
                        Bank = bank 1800 1800
                        Sources = []
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { spatial [] crowdField with
                                Stores = Map.ofList [ "pile-a", 150; "can-far", 400 ]
                            }
                            |> withTargets
                                [
                                    "pile-a", { X = 10; Y = 10 }, (Dropped Energy)
                                    "can-far", { X = 30; Y = 10 }, Structure BuiltKind.Container
                                ]
                            |> withCreepsAt [ "h1", { X = 20; Y = 10 } ]
                    }

                let { Verdicts = verdicts } = decideOn colony

                Expect.equal
                    verdicts
                    [
                        Verdict.Matched(
                            "h1",
                            taskId (Pickup("pile-a", Energy)),
                            MatchFactor.PoolOrder
                        )
                    ]
                    "ten tiles either way: pool order broke the tie, not rank"
            }

            test "a pile outranks the stock underfoot" {
                // The Storage is drawn a tier below the flow, so a pile
                // sixteen tiles away beats a stock the creep stands beside.
                let colony =
                    { bareRespawn with
                        Bank = bank 150 150
                        Sources = []
                        Refillables = [ refillable "spawn-1" 300 BuiltKind.Spawn ]
                        Creeps = [ hauler "h1" 0 100 ]
                        Spatial =
                            { spatial [] crowdField with
                                Stores = Map.ofList [ "pile-a", 300; "sto-c", 400 ]
                            }
                            |> withTargets
                                [
                                    "pile-a", { X = 30; Y = 10 }, (Dropped Energy)
                                    "sto-c", { X = 13; Y = 10 }, Structure BuiltKind.Storage
                                ]
                            |> withHome (fun layer ->
                                { layer with
                                    Obstacles = Set.singleton { X = 13; Y = 10 }
                                    CreepPositions = Map.ofList [ "h1", { X = 14; Y = 10 } ]
                                })
                    }

                let { Verdicts = verdicts } = decideOn colony

                Expect.equal
                    verdicts
                    [ Verdict.Matched("h1", taskId (Pickup("pile-a", Energy)), MatchFactor.Rank) ]
                    "the feeding tier beats the stock draw whatever the distance"
            }

            test "an outpost's pile pools by the rule the home room's does" {
                // The pool is read off the kind census and the amount,
                // neither of which knows a border. The home pile keeps its
                // coordinate and no amount, so it proves the pairing is not
                // crossing.
                let colony =
                    pileColony [ hauler "h-out" 0 100 ] []
                    |> withPileRoom
                        "W1N2"
                        [ "pile-out", { X = 10; Y = 10 } ]
                        [ "h-out", { X = 12; Y = 10 } ]

                let snapshot =
                    { colony with
                        Bank = bank 150 150
                        Spatial =
                            { colony.Spatial with
                                Stores = Map.ofList [ "pile-out", 150 ]
                            }
                    }

                let {
                        Intents = intents
                        Assignments = assignments
                    } =
                    decideOn snapshot

                Expect.equal
                    (pickersOf assignments "pile-out")
                    [ "h-out" ]
                    "the outpost's pile hires the hauler standing in the outpost"

                Expect.contains
                    intents
                    (SayCreep("h-out", "🧲"))
                    "and it walks under the Pickup glyph"

                Expect.isEmpty
                    (pickups intents)
                    "two tiles out: no reflex, and no action Intent until it arrives"
            }
        ]

[<Tests>]
let fullContainerTests =
    testList
        "a full source container"
        [
            test "a full source container is drawn before a half-full one nearer to hand" {
                // can-a three tiles from the hauler at 1,000, can-b twelve
                // tiles away at 2,000 (full, so its garrison's overflow is
                // going to the ground). Pairwise on can-b's stock alone:
                // nearest-first dispatch let W13S28's north container
                // overflow for hours.
                let lane stockB =
                    let room =
                        { spatial
                              []
                              [
                                  for x in 9..28 ->
                                      { X = x; Y = 10 }, (if x = 10 || x = 27 then Wall else Plain)
                              ] with
                            Stores = Map.ofList [ "can-a", 1000; "can-b", stockB ]
                        }
                        |> withTargets
                            [
                                "src-a", { X = 10; Y = 10 }, Source
                                "can-a", { X = 11; Y = 10 }, Structure BuiltKind.Container
                                "src-b", { X = 27; Y = 10 }, Source
                                "can-b", { X = 26; Y = 10 }, Structure BuiltKind.Container
                            ]
                        |> withCreepsAt [ "h", { X = 14; Y = 10 } ]

                    { bareRespawn with
                        Sources = [ source "src-a"; source "src-b" ]
                        Refillables = []
                        Controller = None
                        Creeps = [ creepWith "h" 0 200 [ Carry; Carry; Carry; Carry; Move; Move ] ]
                        Spatial = room
                    }

                let matched stockB =
                    let { Verdicts = verdicts } = decideOn (lane stockB)

                    verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h", task, factor) -> Some(task, factor)
                        | _ -> None)

                Expect.equal
                    (matched 2000)
                    (Some(taskId (Withdraw("can-b", Energy)), MatchFactor.Rank))
                    "the full container outranks the near one"

                Expect.equal
                    (matched 1000)
                    (Some(taskId (Withdraw("can-a", Energy)), MatchFactor.TravelCost))
                    "both half-full: the near one, by travel cost"
            }
        ]

[<Tests>]
let thoriumLegTests =
    testList
        "the Thorium leg's pool"
        [
            test "the mineral container is a Withdraw and the Storage a Refill, both in Thorium" {
                // The mine-to-Storage leg is the existing pair with a
                // resource on it. The Storage is the one store nothing can
                // stand on, so the contact penalty never reaches it.
                let tasks = planTasksOn mineHaulColony noThreats

                Expect.contains
                    tasks
                    (Withdraw("can-min", Thorium))
                    "the mineral container is drawn in Thorium"

                Expect.contains tasks (Refill("sto-1", Thorium)) "and the Storage takes the load"

                // The id carries the resource and the energy spelling is
                // untouched, which is what keeps a standing assignment standing
                // across the deploy that lands this.
                Expect.equal
                    (taskId (Withdraw("can-min", Thorium)))
                    "withdraw:can-min:Thorium"
                    "the Thorium draw is its own identity at that store"
            }

            test "an empty mineral container is drawn by nobody, and the sink stands anyway" {
                // The Withdraw follows the stock; the Refill follows the
                // ground, because a hauler walking home with a load must
                // have somewhere to put it the tick the container reads
                // zero.
                let tasks = planTasksOn (mineHaulColony |> withMineStock 0) noThreats

                Expect.isFalse
                    (List.contains (Withdraw("can-min", Thorium)) tasks)
                    "nothing in the container is nothing to come for"

                Expect.contains
                    tasks
                    (Refill("sto-1", Thorium))
                    "and the sink is the mine's and not the load's"
            }

            test "a colony with no mineral container draws nothing, and still has a sink" {
                // The draw is read off `ourDeposits` and the container on
                // the deposit's Seat; the Refill off the Storage alone. A
                // body holding ore is applicable to that Task and nothing
                // else, so a sink gated on the intake's ground stranded a
                // hauler mid-haul for the window the Layout takes to
                // re-place a destroyed container.
                let tasks = planTasksOn (mineHaulColony |> withoutMineContainer) noThreats

                Expect.isFalse
                    (List.contains (Withdraw("can-min", Thorium)) tasks)
                    "no mineral container, nothing to draw"

                Expect.contains
                    tasks
                    (Refill("sto-1", Thorium))
                    "and the Storage takes a load whatever the ground behind it has become"
            }

            test "the Thorium pair ranks at the Storage's tier, one resource apart" {
                // Pairwise on the resource alone: the same container yields
                // a Feeding-tier draw in energy and a StockDraw one in
                // Thorium. On the container's own tier an empty hauler
                // would take the ore ahead of the energy the spawn is
                // waiting on.
                let colony =
                    { mineHaulColony with
                        Spatial =
                            { mineHaulColony.Spatial with
                                Stores = Map.add "can-min" 600 mineHaulColony.Spatial.Stores
                            }
                    }

                let rankOf task =
                    poolOn colony
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = task then Some pooled.Priority else None)

                Expect.equal
                    (rankOf (Withdraw("can-min", Energy)))
                    (Some(priorityOfTier Feeding))
                    "the premise: energy in a container is Feeding-tier intake"

                Expect.equal
                    (rankOf (Withdraw("can-min", Thorium)))
                    (Some(priorityOfTier StockDraw))
                    "and the Thorium beside it is drawn on the Storage's own tier"

                Expect.equal
                    (rankOf (Refill("sto-1", Thorium)))
                    (Some(priorityOfTier Stock))
                    "the sink is the Storage's deepest tier, below every energy sink"
            }

            test "past the contact cliff the mine's draw takes the full container's two rungs" {
                // The Storage's own energy Withdraw sits on the same tier,
                // so a rungless mine loses the travel-cost tie to the bank
                // every tick a colony has energy banked; live at t402,520
                // W13S28 had 486k banked and a container at its 2,000 cap.
                // The lift fires at the contact cliff, not the cap: at a
                // thousand the tile has turned `p = 3` and the miner burns
                // a fourth tick of life a tick. The Storage's Withdraw is
                // pooled only where the colony has somewhere to put the
                // energy, hence the hungry spawn.
                let banked =
                    { mineHaulColony with
                        Refillables = [ refillable "spawn-1" 50 BuiltKind.Spawn ]
                        Spatial =
                            { mineHaulColony.Spatial with
                                Stores = Map.add "sto-1" 485_916 mineHaulColony.Spatial.Stores
                            }
                    }

                let rankIn colony task =
                    poolOn colony
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = task then Some pooled.Priority else None)

                Expect.equal
                    (rankIn (banked |> withMineStock 999) (Withdraw("can-min", Thorium)))
                    (Some(priorityOfTier StockDraw))
                    "the premise: under the cliff the draw is decision 3's rungless intake"

                Expect.equal
                    (rankIn
                        (banked |> withMineStock Tuning.defaults.MineContactCliff)
                        (Withdraw("can-min", Thorium)))
                    (Some(priorityOfTier StockDraw + rankOfRung TwoRungsUp))
                    "at the cliff it takes the full energy container's own two rungs"

                Expect.equal
                    (rankIn
                        (banked |> withMineStock Tuning.defaults.MineContactCliff)
                        (Withdraw("sto-1", Energy)))
                    (Some(priorityOfTier StockDraw))
                    "and what the lift steps over is the bank's own draw, rungless on that tier"

                // The line the lift does not cross: every energy Task the
                // spawn waits on is Feeding-tier, and a rung never leaves
                // its tier.
                Expect.isGreaterThan
                    (priorityOfTier StockDraw + rankOfRung TwoRungsUp)
                    (priorityOfTier Feeding)
                    "the lifted draw is still deeper than the shallowest energy flow"
            }

            test "the Thorium draw is capped by its own column and not by the energy one" {
                // The row's cast at this bank is `[4 Carry; 2 Move]` — 200 —
                // so 600 Thorium is three seats, and the energy the
                // container holds none of is none.
                let seatsAt units =
                    poolOn (mineHaulColony |> withMineStock units)
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = Withdraw("can-min", Thorium) then
                            Capacity.capOf CapScope.Everyone pooled.Capacity
                        else
                            None)

                Expect.equal
                    (partCountIn (bodyFor haulerPattern mineHaulColony.Bank.Capacity) Carry
                     * Engine.carryPartCapacity)
                    200
                    "the premise: the divisor is the hauler row's own cast at this bank"

                Expect.equal (seatsAt 600) (Some 3) "six hundred of Thorium is three loads"
                Expect.equal (seatsAt 200) (Some 1) "one load is one seat"
            }

            test "the acts the leg emits name the resource, and the Withdraw names no amount" {
                // The amount is `None` — as much as the body has room for;
                // the one named number is the delivery's 999-unit load.
                let intentsFor load tile =
                    let colony =
                        { mineHaulColony with
                            Creeps = [ load ]
                            Spatial = mineHaulColony.Spatial |> withCreepsAt [ "h1", tile ]
                        }

                    (decideOn colony).Intents

                Expect.contains
                    (intentsFor (hauler "h1" 0 200) { X = 12; Y = 10 })
                    (WithdrawFromStore("h1", "can-min", Thorium, None))
                    "the draw names the mineral container and the season's resource"

                Expect.contains
                    (intentsFor (hauler "h1" 0 200 |> carrying 150) { X = 13; Y = 10 })
                    (TransferEnergyToStructure("h1", "sto-1", Thorium))
                    "and the pour names the Storage and the same resource back"
            }
        ]

/// `Fixtures.withMinePile`'s pile in energy rather than ore: the same tile
/// and amount under the energy column.
let private withMineEnergyPile units (colony: ColonyView) =
    { colony with
        Spatial =
            { colony.Spatial with
                Stores = Map.add "pile-min" units colony.Spatial.Stores
            }
            |> withTargets [ "pile-min", minePost, Dropped Energy ]
    }

[<Tests>]
let thoriumPileTests =
    testList
        "the Thorium pile's pool"
        [
            test "the ore on the ground is a Pickup, and the energy pile's id is untouched" {
                // A mineral container caps at 2,000 and the miner stands on
                // it, so every tick the haul lags the next dig lands on the
                // floor as a pile, decaying `ceil(amount / 1000)` a tick.
                // Pairwise on the ground alone.
                let piled = planTasksOn (mineHaulColony |> withMinePile 630) noThreats

                Expect.contains
                    piled
                    (Pickup("pile-min", Thorium))
                    "the pile on the mine post is the container's own intake off the floor"

                Expect.isFalse
                    (planTasksOn mineHaulColony noThreats
                     |> List.exists (function
                         | Pickup _ -> true
                         | _ -> false))
                    "the premise: no pile, no Pickup"

                // The id carries the resource and the energy spelling is frozen
                // where #167 left it, a Task id being a Memory key.
                Expect.equal
                    (taskId (Pickup("pile-min", Thorium)))
                    "pickup:pile-min:Thorium"
                    "the Thorium pile is its own identity on that tile"

                Expect.equal
                    (taskId (Pickup("pile-min", Energy)))
                    "pickup:pile-min"
                    "and an energy pile's id has not moved a byte"
            }

            test
                "a Thorium pile is drawn on the Storage's tier, one rung up; the energy pile is flow" {
                // Pairwise on the resource alone. A pile is the container's
                // next dig lying on the floor: one intake of one resource,
                // ranked on the same tier. The rung inside it is neither of
                // the energy pile's clauses (`drawableTiles` holds
                // Feeding-tier Withdraws alone, and a hundred units fails
                // the worth-a-trip line): ore on the floor bleeds
                // `ceil(amount / 1000)` a tick and nothing else on this tier
                // bleeds. Rungless it lost every travel-cost tie to the
                // Storage's own energy draw.
                let rankIn colony task =
                    poolOn colony
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = task then Some pooled.Priority else None)

                Expect.equal
                    (rankIn (mineHaulColony |> withMineEnergyPile 630) (Pickup("pile-min", Energy)))
                    (Some(priorityOfTier Feeding + rankOfRung OneRungUp))
                    "the premise: an energy pile feeds the economy, and 630 is a trip of its own"

                Expect.equal
                    (rankIn (mineHaulColony |> withMinePile 630) (Pickup("pile-min", Thorium)))
                    (Some(priorityOfTier StockDraw + rankOfRung OneRungUp))
                    "and the season's ore is drawn on the Storage's tier, one rung over the bank"

                // The rung is unconditional: no mineral container, so
                // nothing drawable lies under the pile and the ore is ours
                // by the room, and a hundred units is under half a hauler
                // unit's load at the 2,300 bank a colony with an extractor
                // has.
                let atMineBank (colony: ColonyView) = { colony with Bank = bank 2300 2300 }

                Expect.equal
                    (rankIn
                        (mineHaulColony |> atMineBank |> withoutMineContainer |> withMinePile 100)
                        (Pickup("pile-min", Thorium)))
                    (Some(priorityOfTier StockDraw + rankOfRung OneRungUp))
                    "ore on the floor takes the rung with no store under it and no trip's worth in it"
            }

            test "the full container is drawn before the pile it overflowed onto" {
                // With the pile above the full container the haulers chased
                // the small copy all day and never drew the store: draining
                // the container is what stops the floor filling. Pairwise
                // on the container's stock, either side of the contact
                // cliff.
                let matchIn colony =
                    let colony =
                        { colony with
                            Creeps = [ hauler "h1" 0 200 ]
                            Spatial = colony.Spatial |> withCreepsAt [ "h1", { X = 12; Y = 10 } ]
                        }

                    (decideOn colony).Verdicts
                    |> List.tryPick (function
                        | Verdict.Matched("h1", tid, factor) -> Some(tid, factor)
                        | _ -> None)

                Expect.equal
                    (matchIn (mineHaulColony |> withMinePile 600))
                    (Some(taskId (Pickup("pile-min", Thorium)), MatchFactor.Rank))
                    "a container under the cliff is not bleeding, so the decaying copy goes first"

                Expect.equal
                    (matchIn (
                        mineHaulColony
                        |> withMineStock Tuning.defaults.MineContactCliff
                        |> withMinePile 600
                    ))
                    (Some(taskId (Withdraw("can-min", Thorium)), MatchFactor.Rank))
                    "past it the store that is feeding the floor is drained first"
            }

            test "a pile past the threshold is pooled; one under it is left to decay" {
                // `Tuning.PickupThreshold`, one threshold and not a second
                // knob: it prices the trip, not the cargo. Inclusive at the
                // line.
                let pooled units =
                    planTasksOn (mineHaulColony |> withMinePile units) noThreats
                    |> List.contains (Pickup("pile-min", Thorium))

                Expect.isTrue (pooled 100) "a hundred exactly is worth the walk"
                Expect.isFalse (pooled 99) "one under it is left where it lies"
            }

            test "the pile's capacity is counted off its own column" {
                // The amount of that resource over one hauler unit's load,
                // 200 at this bank; the energy the pile holds none of
                // admits nobody.
                let seatsAt units =
                    poolOn (mineHaulColony |> withMinePile units)
                    |> List.tryPick (fun pooled ->
                        if pooled.Task = Pickup("pile-min", Thorium) then
                            Capacity.capOf CapScope.Everyone pooled.Capacity
                        else
                            None)

                Expect.equal (seatsAt 600) (Some 3) "six hundred of Thorium is three loads"
                Expect.equal (seatsAt 200) (Some 1) "one load is one seat"
            }

            test "a pile in a room somebody else owns is nobody's" {
                // A pile carries no owner and `FIND_DROPPED_RESOURCES`
                // answers for every player's, but an extractor needs an
                // owned RCL6 room, so Thorium lying in a room we own is
                // ours. Pairwise on the room's owner alone.
                let withPileIn room control =
                    { mineHaulColony with
                        RoomControl = Map.add room control mineHaulColony.RoomControl
                        Spatial =
                            { mineHaulColony.Spatial with
                                TargetKinds =
                                    Map.add
                                        "pile-r"
                                        (Dropped Thorium)
                                        mineHaulColony.Spatial.TargetKinds
                                Thorium = Map.add "pile-r" 630 mineHaulColony.Spatial.Thorium
                            }
                            |> withNeighbour
                                room
                                { RoomLayer.empty with
                                    Terrain =
                                        TerrainGrid.ofList
                                            [ for x in 8..12 -> { X = x; Y = 10 }, Plain ]
                                    TargetPositions = Map.ofList [ "pile-r", minePost ]
                                }
                    }

                let pooledIn colony =
                    planTasksOn colony noThreats |> List.contains (Pickup("pile-r", Thorium))

                Expect.isTrue
                    (pooledIn (withPileIn "W1N2" ownedRoom))
                    "the premise: a room of ours, and its floor is ours to sweep"

                Expect.isFalse
                    (pooledIn (withPileIn "W1N2" rivalRoom))
                    "another player's mine bleeding is not this colony's trip"
            }
        ]
