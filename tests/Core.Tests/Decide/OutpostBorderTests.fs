/// The invader core the view carries, and decide across a border.
module Fabot.Core.Tests.Decide.OutpostBorderTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures
open Fabot.Core.Tests.Decide.OutpostFixtures

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
                let untroubled = decideOn colony

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
                    decideOn snapshot

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
