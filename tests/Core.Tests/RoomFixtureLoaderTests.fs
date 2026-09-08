/// The loader that reads a committed room capture back (ADR 0036).
module Fabot.Core.Tests.RoomFixtureLoaderTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Decide
open Fabot.Core.Tests.RoomFixtures
open Fabot.Core.Tests.Decide
open Fabot.Core.Tests.RoomInvariantFixtures

/// These pin the *capture* — that the loader reads the committed file as
/// the room the server actually answered with. They name tiles, and the
/// invariants below deliberately do not: a golden tile is unreviewable
/// when it is the Layout's pick and is the whole point when it is the
/// room's own furniture.
[<Tests>]
let loaderTests =
    testList
        "room fixture loader"
        [
            test "the header names the room the capture came from" {
                let room = load "W12S28"

                Expect.equal room.RoomName "W12S28" "the header's room name"
                Expect.equal room.Shard "shardSeason" "the shard it was captured from"
                Expect.isGreaterThan room.Tick 0 "a tick, so the capture can be placed in time"
            }

            test "terrain is trimmed to the window the shell projects" {
                // `World.terrainOf` projects x,y in 1..48 and leaves the
                // exit rows out — an absent tile is impassable, so no path
                // or Work Area ever uses one. The capture holds all 50 rows
                // verbatim; the trim is the loader's, and it is the one
                // line here that has to agree with the shell.
                let room = load "W12S28"

                Expect.hasLength room.Terrain (48 * 48) "the 48x48 interior, exits excluded"

                Expect.isTrue
                    (room.Terrain
                     |> Map.forall (fun tile _ ->
                         tile.X >= 1 && tile.X <= 48 && tile.Y >= 1 && tile.Y <= 48))
                    "no tile outside the projected window"
            }

            test "the border ring is loaded beside that window, holding the room's exits" {
                // The trim above and this are one split, not two truths: the
                // ring is delivered beside the ground and never inside it
                // (ADR 0041), so the Seam has the engine's own exit terrain
                // while nothing that stands a creep can reach it. The exits
                // are named here, with the furniture, because they are what
                // the server said this room's edges are — the invariants
                // below name no tile.
                let home = load "W12S28"

                Expect.hasLength home.Border (50 * 50 - 48 * 48) "the ring, and only the ring"

                Expect.isTrue
                    (home.Border
                     |> Map.forall (fun tile _ ->
                         tile.X = 0 || tile.X = 49 || tile.Y = 0 || tile.Y = 49))
                    "no tile off the border"

                let exitsAlong on across =
                    home.Border
                    |> Map.toList
                    |> List.filter (fun (tile, terrain) -> on tile && terrain <> Wall)
                    |> List.map (fst >> across)

                Expect.equal
                    (exitsAlong (fun tile -> tile.Y = 0) (fun tile -> tile.X))
                    [ 4..39 ]
                    "the north edge W12S27 is reached across: 36 exits, x 4..39"

                Expect.equal
                    (exitsAlong (fun tile -> tile.X = 0) (fun tile -> tile.Y))
                    [ 22..40 ]
                    "the west edge W13S28 is reached across: 19 exits, y 22..40"
            }

            test "the terrain reads row-major, as the live room's own record shows" {
                // Orientation is not a matter of taste: the encoded string
                // is not symmetric, so reading it transposed would give a
                // different room. #77 recorded that the 16,39 container's
                // footing slipped to "the swamp tile 15,40" while 15,39
                // took an extension — which is what this room is read
                // row-major, and is not what it is read any other way.
                let room = load "W12S28"

                Expect.equal
                    (Map.tryFind { X = 15; Y = 40 } room.Terrain)
                    (Some Swamp)
                    "15,40 is the swamp tile #77 names"

                Expect.equal
                    (Map.tryFind { X = 15; Y = 39 } room.Terrain)
                    (Some Plain)
                    "15,39 is the tile the RCL4 burst's extension took"
            }

            test "the furniture is the room's own, under readable ids" {
                let room = load "W12S28"

                Expect.equal
                    room.Sources
                    [ "src-0", { X = 9; Y = 44 }; "src-1", { X = 17; Y = 40 } ]
                    "both sources, in the capture's order, keyed for a person to read"

                Expect.equal
                    room.Controller
                    (Some("ctrl", { X = 5; Y = 41 }))
                    "the controller the room has"
            }

            test "the engine's own ids ride beside the readable ones" {
                // The decision #124 had to make and this pins: an outpost
                // is declared in the *engine's* ids, because a live
                // projection keys every target by the id the server hands
                // back — `TargetKinds`, `Hits`, `Stores` and
                // `ColonyView.Sources` all do. A declaration written in the
                // readable names above would match nothing online, and
                // would do it in silence: an id the projection does not
                // place is unpriceable geometry, so the outpost would never
                // enter a Task rather than fail (ADR 0004). These are what
                // ADR 0042's declaration of this very room is written from,
                // and what a ColonyView built to meet it has to carry — so
                // they are pinned here, with the furniture, against the
                // committed capture.
                let room = load "W12S27"

                Expect.equal
                    room.RealSources
                    [ "6a8caabadd4872bccd3194a6", { X = 16; Y = 45 } ]
                    "the one source, under the id the server gave it"

                Expect.equal
                    room.RealController
                    (Some("6a8caabadd4872bccd3194a5", { X = 37; Y = 43 }))
                    "and the controller a reserver would hold (ADR 0042)"

                // Read on the multi-source rooms by literal, in capture
                // order, because that is the only reading of order that can
                // fail: `Sources` and `RealSources` are two `List.map`s of
                // one list, so comparing them to each other is `x = x` and
                // a loader that sorted the objects would reorder both
                // together and stay green. The order is a claim — ADR
                // 0042's declaration of W13S28 pairs `…362` with (16,7) and
                // `…361` with (18,4), the reverse of the order its prose
                // reads in — and a reorder here would have a declaration
                // and the ColonyView built beside it name two different
                // rocks.
                Expect.equal
                    (load "W13S28").RealSources
                    [
                        "6a8caaaddd4872bccd319362", { X = 16; Y = 7 }
                        "6a8caaaddd4872bccd319361", { X = 18; Y = 4 }
                    ]
                    "the west outpost's two sources, paired as ADR 0042 declares them"

                let centre = load "W15S25"

                Expect.equal
                    centre.RealSources
                    [
                        "6a8caa95dd4872bccd319003", { X = 16; Y = 14 }
                        "6a8caa95dd4872bccd319002", { X = 32; Y = 10 }
                        "6a8caa95dd4872bccd319004", { X = 45; Y = 42 }
                    ]
                    "and the three-source room's, where the ids do not run in tile order either"

                Expect.equal
                    centre.Sources
                    [
                        "src-0", { X = 16; Y = 14 }
                        "src-1", { X = 32; Y = 10 }
                        "src-2", { X = 45; Y = 42 }
                    ]
                    "the rename renames and does not reorder — src-0 is the first row"

                Expect.isNone
                    centre.RealController
                    "and a room with no controller has no id for one"
            }

            test "a three-source room loads three sources and no controller" {
                // Measured, not assumed: claimable rooms carry one or two
                // sources, and the rooms that carry three — sector centres
                // and Source Keeper rooms — carry no controller at all.
                let room = load "W15S25"

                Expect.hasLength room.Sources 3 "the count ADR 0022's rule is stated over"
                Expect.isNone room.Controller "a sector centre has no controller to own"
            }
        ]

// ---- the rooms the sweep runs on ----------------------------------------
