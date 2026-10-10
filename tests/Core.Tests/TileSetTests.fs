module Fabot.Core.Tests.TileSetTests

open Expecto
open Fabot.Core.Types

// A TileSet answers what the Set<Pos> it replaces answers, and
// enumerates the same tiles in the same order, for any tiles of a room.
let private side = Engine.roomSide

// The grid index's tile, X then Y: `Geometry.posAt`, which is Core's own.
let private posAt (index: int) : Pos = { X = index / side; Y = index % side }

let private randomTiles (random: System.Random) (n: int) : Pos list =
    [
        for _ in 1..n ->
            {
                X = random.Next side
                Y = random.Next side
            }
    ]

// Sparse, dense and edge-heavy sets: the word boundaries (indices 31/32, the
// last word's tail) and the grid's corners are where a bitset goes wrong.
let private shapes (random: System.Random) : Pos list list =
    [
        []
        [ { X = 0; Y = 0 } ]
        [ { X = side - 1; Y = side - 1 } ]
        [ for i in 0 .. side * side - 1 -> posAt i ]
        |> List.filter (fun p -> (p.X + p.Y) % 3 = 0)
        [ for i in [ 0; 30; 31; 32; 33; 63; 64; 2495; 2496; 2499 ] -> posAt i ]
        randomTiles random 5
        randomTiles random 60
        randomTiles random 900
        randomTiles random 2400
    ]

[<Tests>]
let tileSetTests =
    testList
        "tile set"
        [
            test "every operation answers what Set<Pos> answers, in Set<Pos>'s order" {
                let random = System.Random 97
                let all = shapes random

                for a in all do
                    let set = Set.ofList a
                    let tiles = TileSet.ofList a

                    Expect.equal (TileSet.toList tiles) (Set.toList set) "the tiles, in Pos order"
                    Expect.equal (TileSet.count tiles) set.Count "count"
                    Expect.equal (TileSet.isEmpty tiles) set.IsEmpty "isEmpty"
                    Expect.equal (TileSet.toSet tiles) set "toSet"
                    Expect.equal (TileSet.ofSet set) tiles "ofSet"

                    if not set.IsEmpty then
                        Expect.equal (TileSet.minElement tiles) (Set.minElement set) "minElement"

                    let probe = randomTiles random 200

                    Expect.equal
                        (probe |> List.map (fun p -> TileSet.contains p tiles))
                        (probe |> List.map (fun p -> Set.contains p set))
                        "contains"

                    let even (p: Pos) = (p.X * 7 + p.Y) % 2 = 0

                    Expect.equal
                        (TileSet.filter even tiles |> TileSet.toList)
                        (Set.filter even set |> Set.toList)
                        "filter"

                    Expect.equal (TileSet.exists even tiles) (Set.exists even set) "exists"
                    Expect.equal (TileSet.forall even tiles) (Set.forall even set) "forall"

                    Expect.equal
                        (TileSet.fold (fun acc p -> p :: acc) [] tiles)
                        (Set.fold (fun acc p -> p :: acc) [] set)
                        "fold, in order"

                    for p in probe |> List.truncate 20 do
                        Expect.equal
                            (TileSet.add p tiles |> TileSet.toList)
                            (Set.add p set |> Set.toList)
                            "add"

                        Expect.equal
                            (TileSet.remove p tiles |> TileSet.toList)
                            (Set.remove p set |> Set.toList)
                            "remove"

                    for b in all do
                        let other = Set.ofList b
                        let others = TileSet.ofList b

                        Expect.equal
                            (TileSet.union tiles others |> TileSet.toList)
                            (Set.union set other |> Set.toList)
                            "union"

                        Expect.equal
                            (TileSet.intersect tiles others |> TileSet.toList)
                            (Set.intersect set other |> Set.toList)
                            "intersect"

                        Expect.equal
                            (TileSet.difference tiles others |> TileSet.toList)
                            (Set.difference set other |> Set.toList)
                            "difference"

                        Expect.equal
                            (TileSet.isSubset tiles others)
                            (Set.isSubset set other)
                            "isSubset"

                        Expect.equal (tiles = others) (set = other) "equality"

                        Expect.equal
                            (compare tiles others |> sign)
                            (compare set other |> sign)
                            "comparison"

                        if tiles = others then
                            Expect.equal (hash tiles) (hash others) "equal sets hash alike"

                Expect.equal
                    (TileSet.unionMany (all |> List.map TileSet.ofList) |> TileSet.toList)
                    (Set.unionMany (all |> List.map Set.ofList) |> Set.toList)
                    "unionMany"
            }

            test "a tile off the grid is refused, never folded onto another" {
                for off in [ { X = -1; Y = 0 }; { X = 0; Y = side }; { X = side; Y = 3 } ] do
                    Expect.throws (fun () -> TileSet.add off TileSet.empty |> ignore) $"add {off}"
                    Expect.throws (fun () -> TileSet.ofList [ off ] |> ignore) $"ofList {off}"

                    Expect.isFalse
                        (TileSet.contains off (TileSet.singleton { X = 0; Y = 0 }))
                        "contains: never"

                    Expect.equal
                        (TileSet.remove off TileSet.empty)
                        TileSet.empty
                        "remove: nothing to take"
            }
        ]
