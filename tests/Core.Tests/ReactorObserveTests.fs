module Fabot.Core.Tests.ReactorObserveTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Observe

let private seen owner store continuous banked issued =
    Some
        {
            Owner = owner
            StoreT = store
            ContinuousWork = continuous
            BankedT = banked
            DeliveryIssued = issued
        }

[<Tests>]
let reactorTests =
    testList
        "observe fold: the Reactor programme"
        [
            test "first sight records the programme without inventing a delivery" {
                let state =
                    ReactorState.empty
                    |> foldReactor 100 (seen (ReactorOwner.Rival "Odiodin") 400 12_345 2_997 false)

                Expect.equal state.Owner (ReactorOwner.Rival "Odiodin") "the flag is attributed"
                Expect.equal state.StoreT 400 "the visible store is recorded"
                Expect.equal state.ContinuousWork 12_345 "the visible streak is recorded"
                Expect.equal state.Seen (Some 100) "the sample is dated"
                Expect.equal state.BankedT 2_997 "the feeding Storage is recorded beside it"
                Expect.equal state.LastDelivery None "the empty state's zero was not a prior sample"
                Expect.equal state.DryTicks 0 "a stocked first sight is not dry"
            }

            test "ordinary consumption advances neither delivery nor dry count" {
                let state =
                    ReactorState.empty
                    |> foldReactor 100 (seen ReactorOwner.Ours 400 12_345 2_997 false)
                    |> foldReactor 101 (seen ReactorOwner.Ours 399 12_346 2_997 false)

                Expect.equal state.LastDelivery None "one consumed unit is not a delivery"
                Expect.equal state.DryTicks 0 "the non-empty store is not dry"
                Expect.equal state.Seen (Some 101) "the newer sight replaces the sample"
            }

            test "store growth and an issued transfer are each delivery evidence" {
                let grown =
                    ReactorState.empty
                    |> foldReactor 100 (seen ReactorOwner.Ours 20 7 3_000 false)
                    |> foldReactor 101 (seen ReactorOwner.Ours 998 8 2_001 false)

                Expect.equal grown.LastDelivery (Some 101) "an observed refill dates the delivery"

                let issued = grown |> foldReactor 102 (seen ReactorOwner.Ours 998 9 2_000 true)

                Expect.equal
                    issued.LastDelivery
                    (Some 102)
                    "an issued transfer catches a delivery hidden by same-tick consumption"
            }

            test "each visible empty tick is dry regardless of owner" {
                let state =
                    ReactorState.empty
                    |> foldReactor 100 (seen (ReactorOwner.Rival "Odiodin") 0 0 2_997 false)
                    |> foldReactor 101 (seen ReactorOwner.Unowned 0 0 2_997 false)
                    |> foldReactor 102 (seen ReactorOwner.Ours 1 1 2_997 false)

                Expect.equal state.DryTicks 2 "only the two visible zero-store ticks count"
            }

            test "a vision gap retains every field and advances nothing" {
                let seenState =
                    ReactorState.empty
                    |> foldReactor 100 (seen ReactorOwner.Ours 999 40_000 12_000 true)

                let blind = seenState |> foldReactor 101 None |> foldReactor 102 None

                Expect.equal
                    blind
                    seenState
                    "blind ticks leave the last observation stale and whole"
            }
        ]
