module Fabot.Core.Tests.ListsTests

open Expecto
open Fabot.Core.Types

// Core's key-once `List.sortBy`, `sortByDescending` and `maxBy` answer what
// the library's do, ties included, and take each key once.
[<Tests>]
let listsTests =
    testList
        "key-once list order"
        [
            test "sorts are stable and agree with the library's" {
                let items = [ for i in 0..199 -> i % 7, i ]

                Expect.equal
                    (List.sortBy fst items)
                    (Microsoft.FSharp.Collections.List.sortBy fst items)
                    "sortBy"

                Expect.equal
                    (List.sortByDescending fst items)
                    (Microsoft.FSharp.Collections.List.sortByDescending fst items)
                    "sortByDescending"
            }

            test "maxBy keeps the first of equal keys" {
                Expect.equal
                    (List.maxBy fst [ 1, "a"; 3, "b"; 3, "c"; 2, "d" ])
                    (3, "b")
                    "first max"

                Expect.throws (fun () -> List.maxBy id ([]: int list) |> ignore) "empty"
            }

            test "each key is taken once" {
                let mutable asked = 0

                let key (i: int) =
                    asked <- asked + 1
                    -i

                List.sortBy key [ 1..100 ] |> ignore
                Expect.equal asked 100 "sortBy"
                asked <- 0
                List.maxBy key [ 1..100 ] |> ignore
                Expect.equal asked 100 "maxBy"
            }
        ]
