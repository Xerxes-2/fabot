/// The Caster: which colony casts each of the world's fight asks
/// (`Caster.assign`).
module Fabot.Core.Tests.Decide.CasterTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.Decide.Fixtures

/// A colony whose home is `home`, its bank at `capacity` and `stock` energy
/// in its Storage, one idle spawn, nobody standing.
let private colonyAt (home: string) (capacity: int) (stock: int) : ColonyView =
    { bareRespawn with
        Bank = bank capacity capacity
        Spatial =
            { bareRespawn.Spatial with
                RoomName = Some home
            }
    }
    |> stocking stock

/// A colony that pays another's ask: a bank and a stock past both floors.
let private rich home =
    colonyAt home 5_600 Tuning.defaults.CasterReserve

/// One colony asking for these.
let private bidder (view: ColonyView) (asks: Cast list) : Caster.Bidder =
    {
        Home = SpatialInfo.homeName view.Spatial
        View = view
        Asks = asks
    }

let private ask (task: Task) (row: CastRow) (wanted: int) : Cast =
    {
        For = task
        Ask =
            {
                Row = row
                Wanted = wanted
                Blocks = 1
            }
    }

/// The bodies each colony is handed for a Task, the crossings off the
/// room names; a colony handed none left out.
let private bodiesFor (task: Task) (bidders: Caster.Bidder list) : Map<string, int> =
    Caster.assign (fun _ _ -> None) Map.empty Map.empty bidders
    |> Map.map (fun _ casts ->
        casts |> List.filter (fun cast -> cast.For = task) |> List.sumBy _.Ask.Wanted)
    |> Map.filter (fun _ bodies -> bodies > 0)

/// A guard-row body: one ATTACK part.
let private guardBody name = creepWith name 0 0 [ Attack; Move ]

[<Tests>]
let casterTests =
    testList
        "the Caster: who casts a fight ask"
        [
            // W1N1 asks for W4N1, three crossings out; W5N1 stands one away.
            test "the colony that lands the cast at least cost casts it, the asker's or another's" {
                Expect.equal
                    (bodiesFor
                        (Guard "W4N1")
                        [
                            bidder (rich "W1N1") [ ask (Guard "W4N1") CastRow.Guard 1 ]
                            bidder (rich "W5N1") []
                        ])
                    (Map.ofList [ "W5N1", 1 ])
                    "the walk one crossing long, where the asker's is three"
            }

            test "a colony casts another's ask only past both floors, its bank's and its stock's" {
                let asking = bidder (rich "W1N1") [ ask (Guard "W4N1") CastRow.Guard 1 ]

                let near capacity stock =
                    bodiesFor (Guard "W4N1") [ asking; bidder (colonyAt "W5N1" capacity stock) [] ]

                Expect.equal
                    (near (Tuning.defaults.CasterFloor - 1) Tuning.defaults.CasterReserve)
                    (Map.ofList [ "W1N1", 1 ])
                    "a bank under CasterFloor casts none of it"

                Expect.equal
                    (near Tuning.defaults.CasterFloor (Tuning.defaults.CasterReserve - 1))
                    (Map.ofList [ "W1N1", 1 ])
                    "nor does a stock under CasterReserve"

                Expect.equal
                    (near Tuning.defaults.CasterFloor Tuning.defaults.CasterReserve)
                    (Map.ofList [ "W5N1", 1 ])
                    "both at their floors, it casts"
            }

            test "a colony's own asks are its own to cast, under either floor" {
                Expect.equal
                    (bodiesFor
                        (Guard "W2N1")
                        [ bidder (colonyAt "W1N1" 5_600 0) [ ask (Guard "W2N1") CastRow.Guard 1 ] ])
                    (Map.ofList [ "W1N1", 1 ])
                    "a colony with no stock casts for its own room"
            }

            // Review of W6 S2: a colony asks for every beaten home it defends
            // (`BorrowedWork.Defended`), and that ask is another's, floored.
            test
                "another colony's home a colony defends and asks for it casts for only past both floors" {
                let defending stock =
                    { colonyAt "W1N1" 5_600 stock with
                        Borrowed =
                            { BorrowedWork.empty with
                                Defended = [ "W2N1" ]
                            }
                    }

                let asked stock =
                    bodiesFor
                        (Guard "W2N1")
                        [ bidder (defending stock) [ ask (Guard "W2N1") CastRow.Guard 1 ] ]

                Expect.isEmpty
                    (asked (Tuning.defaults.CasterReserve - 1))
                    "a stock under the reserve casts none"

                Expect.equal
                    (asked Tuning.defaults.CasterReserve)
                    (Map.ofList [ "W1N1", 1 ])
                    "at the reserve it casts"
            }

            test "a home's Defend rows are the home's, whoever lands sooner or banks more" {
                let poor = colonyAt "W1N1" 300 0

                Expect.equal
                    (bodiesFor
                        (Defend "W1N1")
                        [
                            bidder poor [ ask (Defend "W1N1") CastRow.Defender 1 ]
                            bidder (rich "W2N1") []
                        ])
                    (Map.ofList [ "W1N1", 1 ])
                    "the rampart defender is cast at home"
            }

            // W1N1 and W3N1 stand one crossing either side of W2N1: the
            // first body ties and goes to the asker, whose spawn it then
            // queues, so the second lands sooner from the other.
            test "an ask is split among colonies, a body each, as the queue grows" {
                Expect.equal
                    (bodiesFor
                        (Guard "W2N1")
                        [
                            bidder (rich "W1N1") [ ask (Guard "W2N1") CastRow.Guard 2 ]
                            bidder (rich "W3N1") []
                        ])
                    (Map.ofList [ "W1N1", 1; "W3N1", 1 ])
                    "one body from each"
            }

            test "one colony serves the asks of two" {
                let casts =
                    Caster.assign
                        (fun _ _ -> None)
                        Map.empty
                        Map.empty
                        [
                            bidder (rich "W1N1") [ ask (Guard "W4N1") CastRow.Guard 1 ]
                            bidder (rich "W9N1") [ ask (Guard "W6N1") CastRow.Guard 1 ]
                            bidder (rich "W5N1") []
                        ]

                Expect.equal
                    (casts.["W5N1"] |> List.map (fun cast -> cast.For, cast.Ask.Wanted))
                    [ Guard "W4N1", 1; Guard "W6N1", 1 ]
                    "the colony a crossing from both rooms casts for both"

                Expect.equal (casts.["W1N1"], casts.["W9N1"]) ([], []) "and neither asker any"
            }

            // A caster flip never casts twice: the body already bought, in
            // an oven or standing, answers the ask wherever the least cost
            // has moved since.
            test "a body of the row in an oven or standing answers the ask: nobody casts another" {
                let asking (view: ColonyView) =
                    bidder view [ ask (Guard "W4N1") CastRow.Guard 1 ]

                let baking =
                    { rich "W1N1" with
                        Casting =
                            [
                                {
                                    Name = "guard-41-Spawn1"
                                    Body = [ Attack; Move ]
                                }
                            ]
                    }

                Expect.equal
                    (bodiesFor (Guard "W4N1") [ asking baking; bidder (rich "W5N1") [] ])
                    (Map.ofList [ "W1N1", 1 ])
                    "the asker's oven answers it, though W5N1 lands a new cast sooner"

                let standing =
                    { rich "W5N1" with
                        Creeps = [ guardBody "guard-40-Spawn5" ]
                    }

                Expect.equal
                    (bodiesFor (Guard "W4N1") [ asking (rich "W1N1"); bidder standing [] ])
                    (Map.ofList [ "W5N1", 1 ])
                    "and a guard W5N1 cast answers it as W5N1's, which casts no other"
            }

            test "an Assault's squad stays its declarer's" {
                let squad: Cast =
                    {
                        For = Assault "W4N1"
                        Ask =
                            {
                                Row = CastRow.Squad Medic
                                Wanted = 1
                                Blocks = 1
                            }
                    }

                Expect.equal
                    (bodiesFor
                        (Assault "W4N1")
                        [ bidder (colonyAt "W1N1" 300 0) [ squad ]; bidder (rich "W5N1") [] ])
                    (Map.ofList [ "W1N1", 1 ])
                    "the declarer casts it, however poor"
            }
        ]
