/// The operation ledger read off a fight the arena plays out (#662): the
/// rows `Ledger.observe` lays off each tick's decide and `Ledger.fold`
/// folds, as `Main` folds them.
module Fabot.Core.Tests.ArenaLedgerTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.Arena
open Fabot.Core.Tests.ArenaFixtures

/// The sweep on one of Trepidimous' remotes (#658): W12S28 at RCL7 with a
/// bank and a stock to cast from, W12S27 worked by a 5W3M miner on its
/// source and a loaded 10C5M hauler beside it, in an observer's sight every
/// tick; the ranger row casts.
let private sweepOneRemote () : Arena =
    let trep = Side.Player "Trepidimous"
    let north = room "W12S27"

    let home =
        room "W12S28"
        |> withController Ownership.Ours None 7 6
        |> withSpawn "Spawn1" { X = 12; Y = 40 } 5_600
        |> withStructures
            [
                { structureOf "storage" (Some Side.Ours) 10_000 10_000 { X = 14; Y = 42 } with
                    Energy = 30_000
                }
            ]

    let a =
        arena
            900_000
            [ home; north ]
            [ colony "W12S28" ]
            [
                body
                    "hauler-1-Spawn1"
                    Side.Ours
                    (parts [ Carry, 32; Move, 16 ])
                    (RoomPos.at "W12S28" { X = 17; Y = 34 })
                    None
                body
                    "miner-n"
                    trep
                    (parts [ Work, 5; Move, 3 ])
                    (RoomPos.at "W12S27" { X = 16; Y = 44 })
                    (Some Hold)
                { body
                      "hauler-n"
                      trep
                      (parts [ Carry, 10; Move, 5 ])
                      (RoomPos.at "W12S27" { X = 12; Y = 45 })
                      (Some Hold) with
                    Energy = 500
                }
            ]
        |> withCasts [ "ranger" ]
        |> withHarass
            [
                {
                    RoomName = "W12S27"
                    Enemy = "Trepidimous"
                    Via = []
                }
            ]

    { a with Lookout = set [ "W12S27" ] }

[<Tests>]
let tests =
    testList
        "operation ledger in the arena (#662)"
        [
            test
                "a defended outpost raid is one hold row: the guard lent it, the invader its kill, nothing lost" {
                let invader =
                    body
                        "invader-1"
                        (Side.Npc "Invader")
                        Fabot.Core.Tests.Decide.Fixtures.smallMelee
                        (RoomPos.at "W12S27" { X = 8; Y = 48 })
                        (Some Invader)

                let final, trace = outpostRaid invader |> run 40
                let failure = describe trace
                let ledger = final.Carried.Ledger

                let rows =
                    ledger.Open @ ledger.Closed |> List.filter (fun row -> row.Key.Room = "W12S27")

                Expect.equal
                    (rows |> List.map _.Key.Kind)
                    [ Ledger.OperationKind.Hold ]
                    $"one row, a hold\n%A{ledger}\n{failure}"

                let row = List.head rows

                Expect.equal
                    (row.Kills, row.CastLost + row.LentLost)
                    (1, 0)
                    $"the invader its kill, our guard alive\n%A{row}"

                // Shot down to a bare MOVE, it is a trespasser the room's
                // worth values: its kill at that value.
                Expect.isGreaterThan row.KillValue 0.0 "at the value its worth priced"
                Expect.equal row.Joined 1 "the guard, alive before the raid, lent it"
                Expect.equal row.Predicted.Value None $"no job value read at its open\n%A{row}"
            }

            test
                "a sweep's spell is one sweep row: the ranger cast for it, the remote's workers its kills, closed as it leaves" {
                let final, trace = sweepOneRemote () |> run 400
                let failure = describe trace
                let ledger = final.Carried.Ledger

                let row =
                    ledger.Open @ ledger.Closed
                    |> List.filter (fun row -> row.Key.Kind = Ledger.OperationKind.Sweep)
                    |> fun rows ->
                        Expect.equal (List.length rows) 1 $"one sweep row\n%A{ledger}\n{failure}"
                        List.head rows

                Expect.equal row.Key.Room "W12S27" "on the remote"

                Expect.isTrue
                    (row.Predicted.Rate |> Option.exists (fun rate -> rate > 0.0))
                    $"priced at the sweep's rate\n%A{row}"

                Expect.equal (row.Cast, row.Joined) (1, 0) $"the ranger cast for it\n%A{row}"
                Expect.isGreaterThan row.CastEnergy 0 "at its energy"
                Expect.equal row.Kills 2 $"the miner and the hauler\n%A{row}\n{failure}"
                // The miner falls last: the sweep earns nothing there after
                // it, the ranger walks off, and nothing held the remote
                // unworked.
                Expect.equal
                    row.Ended
                    (diedOn "miner-n" trace |> Option.map (fun tick -> 900_000 + tick + 1))
                    $"closed as the group left\n%A{row}\n{failure}"

                Expect.equal row.Downtime 0 "never held unworked"
            }

            // The one difference S7c decides: on the remote's prey the Brief
            // seats one body per trespasser each template answers (#550),
            // where the operation's priced count seats what its group pays
            // for and its cast buys (`Facts.seatsPaid`, `Facts.fighterCast`).
            test
                "the sweep as an operation, run in shadow, answers its room, its group and its jobs every tick, and seats fewer on the prey than its trespass split (#663 S7b)" {
                let final, trace = sweepOneRemote () |> run 400
                let _, log = final.Carried.Shadow

                Expect.equal log.Compared 400 "every tick compared"
                Expect.isGreaterThan log.Working 0 $"the sweep worked its room\n%A{log}"

                Expect.equal
                    (log.Counts |> Map.keys |> List.ofSeq)
                    [ Divergence.Seats ]
                    $"only the seats differ\n%A{log}\n{describe trace}"

                Expect.all
                    log.Last
                    (fun example -> int example.Sweep > int example.Shadow)
                    $"the Brief seats more, never fewer\n%A{log}"
            }
        ]
