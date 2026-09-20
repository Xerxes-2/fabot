/// The cross-room half of every query: a body in one room, its work in another.
module Fabot.Core.Tests.AtlasCrossRoomTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Tests.AtlasFixtures

/// Two exits north, and the near one is the wrong one: the creep at (25,10)
/// reaches (25,0) in nine steps and (27,0) in ten, but the outpost's column
/// below (25,49) is swamp all the way down while the one below (27,49) is
/// plain: 10 + 1 + 8 against 9 + 1 + 36. The far ring is the caller's, so a
/// crossing can be walled off.
let private twoExitAcross farRing =
    let home =
        { RoomLayer.empty with
            Terrain =
                TerrainGrid.ofList (
                    plainLine
                        [
                            for y in 1..10 -> { X = 25; Y = y }
                            for x in 26..27 -> { X = x; Y = 10 }
                            for y in 1..9 -> { X = 27; Y = y }
                        ]
                )
            CreepPositions = Map.ofList [ "w", { X = 25; Y = 10 } ]
        }

    let outpost =
        { RoomLayer.empty with
            Terrain =
                TerrainGrid.ofList
                    [
                        for x in 25..27 -> { X = x; Y = 41 }, Plain
                        for y in 42..48 -> { X = 25; Y = y }, Swamp
                        for y in 42..48 -> { X = 27; Y = y }, Plain
                    ]
            TargetPositions = Map.ofList [ "src-out", { X = 26; Y = 40 } ]
        }

    northOf
        home
        [ { X = 25; Y = 0 }, Plain; { X = 26; Y = 0 }, Wall; { X = 27; Y = 0 }, Plain ]
        outpost
        farRing
        [ "src-out", Source ]
        [ worker "w" ]

/// Both crossings open. A function, not a value (#310).
let private twoExitBothOpen () =
    twoExitAcross
        [
            { X = 25; Y = 49 }, Plain
            { X = 26; Y = 49 }, Wall
            { X = 27; Y = 49 }, Plain
        ]

/// The swamp shortcut against the plain detour: the line east from (10,10)
/// crosses two swamps to a goal at (13,10), and the loop south down column
/// 10 runs plain to a goal at (10, `loopEnd`). Priced as a road (plain 2,
/// swamp 3) the swamp line costs 8 against a five-step loop's 10; at the
/// walking grid's swamp it would cost 22 (the detour W13S28 was carrying, #211).
let private swampShortcut loopEnd =
    spatial
        []
        ([
            { X = 10; Y = 10 }, Plain
            { X = 11; Y = 10 }, Swamp
            { X = 12; Y = 10 }, Swamp
            { X = 13; Y = 10 }, Plain
         ]
         @ [ for y in 11..loopEnd -> { X = 10; Y = y }, Plain ])

[<Tests>]
let crossRoomTests =
    testList
        "atlas cross-room walk"
        [
            test "a walk across the border is the near leg, the exit's own price and the far leg" {
                // Nine steps up to (25,1), one onto the exit at (25,0), the
                // engine moves the creep to (25,49) for nothing at the end
                // of that tick, one step onto (25,48), and seven more down
                // to (25,41). Eighteen tiles stepped onto; the landing tile
                // is never charged.
                let atlas =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 }; "w-back", { X = 25; Y = 14 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w"; worker "w-back" ]

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (Some 18)
                    "nine near, the exit, and eight in the outpost"

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-out"))
                    (Some 36)
                    "and the same join in the ranking price's own units — two a plain step"

                // The far leg is the target's, not the creep's.
                Expect.equal
                    (walkTicks atlas "w-back" (Harvest "src-out"))
                    (Some 22)
                    "four tiles further back is four ticks dearer, the far leg unchanged"
            }

            test "a swamp exit is priced as a swamp step, not counted as one tile" {
                // The exit tile is priced like every other step:
                // `max(1, ceil(units / 2))`.
                let across ring =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, ring ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.equal
                    (walkTicks (across Plain) "w" (Harvest "src-out"))
                    (Some 18)
                    "a plain exit costs the one tick the ADR's +1 spells"

                Expect.equal
                    (walkTicks (across Swamp) "w" (Harvest "src-out"))
                    (Some 22)
                    "a swamp exit costs five, and the same walk is four ticks dearer"

                Expect.equal
                    (travelCost (across Swamp) "w" (Harvest "src-out"))
                    (Some 44)
                    "the ranking price charges the swamp exit its own ten units"
            }

            test "the tile under the creep is charged nothing, however dear it is" {
                // The walk is read off a field that charges the creep's own
                // tile like any other and takes it off again
                // (`pricedOffField`); swamp is where taking off the wrong
                // number is four or nine ticks out rather than one.
                let home =
                    { corridorHome [ "w", { X = 25; Y = 10 } ] with
                        Terrain =
                            TerrainGrid.ofList
                                [
                                    for y in 1..48 ->
                                        { X = 25; Y = y }, (if y = 10 then Swamp else Plain)
                                ]
                    }

                let atlas =
                    northOf
                        home
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (Some 18)
                    "the swamp under the creep is not a step it takes"

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-out"))
                    (Some 36)
                    "nor one the ranking price charges"
            }

            test "the walk takes the cheapest crossing in the band, not the nearest" {
                // The two-exit border stated up top; this is the price's
                // half and `crossRoomStepTests` has the mover's.
                let across = twoExitAcross
                let bothOpen = twoExitBothOpen ()

                Expect.hasLength (seams bothOpen "W1N1" "W1N2") 2 "the premise: two crossings"

                Expect.equal
                    (walkTicks bothOpen "w" (Harvest "src-out"))
                    (Some 19)
                    "the farther exit, because the ground behind it is cheaper"

                Expect.equal
                    (travelCost bothOpen "w" (Harvest "src-out"))
                    (Some 38)
                    "and the ranking price joins at that same crossing, in its own units"

                // Take the cheap crossing out and the walk falls back to the dear one.
                let swampOnly =
                    across
                        [
                            { X = 25; Y = 49 }, Plain
                            { X = 26; Y = 49 }, Wall
                            { X = 27; Y = 49 }, Wall
                        ]

                Expect.equal
                    (walkTicks swampOnly "w" (Harvest "src-out"))
                    (Some 46)
                    "nine near, the exit, and thirty-six down the swamp column"
            }

            test "a border with no crossing has no price, and the target is still placed" {
                // A walled ring: inapplicable, not the zero an unplaced
                // target gets. The same ring opened closes the case.
                let across ring =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, ring ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                let atlas = across Wall

                Expect.isEmpty (seams atlas "W1N1" "W1N2") "the premise: the exit is walled"

                Expect.isNonEmpty
                    (workArea atlas (Harvest "src-out") |> tilesIn "W1N2")
                    "the target is placed and its ground is real"

                Expect.equal (walkTicks atlas "w" (Harvest "src-out")) None "no crossing, no walk"

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-out"))
                    None
                    "and no ranking price either — one join answers both"

                Expect.equal
                    (walkTicks (across Plain) "w" (Harvest "src-out"))
                    (Some 18)
                    "and it is the wall that answers None: open that tile and the same rooms price"
            }

            test "a target in a room the projection does not carry has no walk to price" {
                let atlas =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source; "src-far", Source ]
                        [ worker "w" ]

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-far"))
                    (Some 0)
                    "a target no room places is free, never unreachable"

                Expect.isTrue
                    (mayActFor atlas "w" (Harvest "src-far"))
                    "and it blocks nothing (ADR 0004)"

                // No room to cross to, so no Seam to aim at.
                Expect.equal
                    (firstStepFor atlas "w" (Harvest "src-far"))
                    None
                    "and the mover is given no step toward a room nobody projected"
            }

            test "the price crosses the border and the standing tiles do not" {
                // Geometry crosses, arbitration does not.
                let atlas =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (Some 18)
                    "the premise: the Task is priceable across the border"

                let handed = workAreaFor atlas "w" (Harvest "src-out")

                Expect.isEmpty handed "and the creep is handed no tile of the other room"

                Expect.isFalse
                    (mayAct atlas "w" (Harvest "src-out") handed)
                    "it may not act on a target a room away"

                // A step is not a standing tile: a Task that is priced and
                // unwalkable is one the Matcher gives away and anti-thrash
                // never takes back (#142).
                Expect.equal
                    (firstStep atlas "w" (Harvest "src-out") handed |> tileHome atlas)
                    (Some { X = 25; Y = 9 })
                    "but it is walked up its own corridor toward the Seam it was priced at"
            }

            test "a heavy body's far leg is its Post's, not the source's nearest Seat" {
                // The source at (25,40) seats (25,41), one step off the
                // corridor, and (24,39), reachable only the long way round
                // through column 23: eighteen tiles to the near Seat and
                // twenty to the Post. Three Work over two Move pays two
                // ticks a plain step. Unposted, this read `Some 36` until
                // #159 took the bare-Seat fallback off the outpost.
                let outpost =
                    { RoomLayer.empty with
                        Terrain =
                            TerrainGrid.ofList (
                                plainLine
                                    [
                                        for y in 41..48 -> { X = 25; Y = y }
                                        for y in 39..48 -> { X = 23; Y = y }
                                        for x in 23..25 -> { X = x; Y = 48 }
                                        yield { X = 24; Y = 39 }
                                    ]
                            )
                        TargetPositions =
                            Map.ofList
                                [ "src-out", { X = 25; Y = 40 }; "cont-out", { X = 24; Y = 39 } ]
                    }

                let across kinds =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 }; "heavy", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        outpost
                        [ { X = 25; Y = 49 }, Plain ]
                        kinds
                        [ worker "w"; creepWith "heavy" 0 [ Work; Work; Work; Move; Move ] ]

                let bare = across [ "src-out", Source ]

                let posted = across [ "src-out", Source; "cont-out", Structure BuiltKind.Container ]

                Expect.isEmpty
                    (postsOf bare "src-out" |> tilesIn "W1N2")
                    "the premise: no container, no Post"

                Expect.equal
                    (postsOf posted "src-out" |> tilesIn "W1N2")
                    (Set.ofList [ { X = 24; Y = 39 } ])
                    "and with one standing, the far Seat is the source's Post"

                Expect.equal
                    (walkTicks bare "heavy" (Harvest "src-out"))
                    None
                    "unposted and a room away, the heavy body has nowhere to walk to at all"

                Expect.equal
                    (walkTicks bare "w" (Harvest "src-out"))
                    (Some 18)
                    "while the light body still walks the eighteen tiles to that same Seat"

                Expect.equal
                    (walkTicks posted "heavy" (Harvest "src-out"))
                    (Some 40)
                    "posted, it walks the long way to the Post — two tiles further, four ticks"

                Expect.equal
                    (walkTicks posted "w" (Harvest "src-out"))
                    (Some 18)
                    "and the light body ignores the Post, over the same border on the same tick"
            }

            test "two origin sets under one Task are two far fields, not one" {
                // The origins used to ride in as an argument rather than in
                // the memo key, so whichever of `travelCost` and
                // `travelCostToward` asked first answered for both (#358).
                let atlas =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-out"))
                    (Some 36)
                    "the premise: the Task's own area is flooded first"

                // Four tiles nearer the landing than the Seat: 9 + 1 + 4 steps.
                Expect.equal
                    (travelCostToward
                        atlas
                        "w"
                        (Harvest "src-out")
                        "W1N2"
                        (Set.singleton (at "W1N2" { X = 25; Y = 45 })))
                    (Some 28)
                    "and the caller's own tile is priced to itself, not to the Seat behind it"
            }

            test "caller-narrowed origins ride the tick's table, never the census's" {
                // Origins the decision layer narrowed move every tick (a
                // Guard's ring is cut out of this tick's `Threats`); filed
                // in the census-held table they would mint a whole chain's
                // field every tick and nothing would evict it.
                let snapshot () =
                    northOfSnapshot
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                let held = FarFieldMemo.empty ()

                // Eight ticks of a goal set moving one tile a tick, all under one census.
                for y in 41..48 do
                    let atlas = snapshot () |> ofViewRecalling (WalkTable()) held

                    Expect.isSome
                        (travelCostToward
                            atlas
                            "w"
                            (Harvest "src-out")
                            "W1N2"
                            (Set.singleton (at "W1N2" { X = 25; Y = y })))
                        "the crossing is still priced, off the tick's own table"

                Expect.equal
                    held.PerCensus.Count
                    0
                    "eight ticks of a moving goal set leave the census-held table empty"
            }

            test "every far field rides the census table, whatever the pricing" {
                // Every input of a far field is in the census under every
                // pricing (`docs/research/cpu-headroom.md` §5.1), so there
                // is one table rather than three.
                let snapshot () =
                    northOfSnapshot
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                let held = FarFieldMemo.empty ()

                // The walk files two fields: the far room's, and the same
                // field carried one hop into the creep's room (`pricedOffField`).
                let fieldOver (pricing: Pricing) (chain: string list) =
                    held.PerCensus
                    |> Seq.filter (fun entry ->
                        let filed, _, _, _, priced, _ = entry.Key
                        filed = chain && priced = pricing)
                    |> Seq.map (fun entry -> entry.Value)
                    |> Seq.exactlyOne

                let first = snapshot () |> ofViewRecalling (WalkTable()) held

                Expect.equal
                    (walkTicks first "w" (Harvest "src-out"))
                    (Some 18)
                    "the first Atlas floods the chain to price the walk"

                Expect.equal
                    held.PerCensus.Count
                    2
                    "and leaves the far field in the table it was handed, beside its carry into the creep's room"

                let flooded = fieldOver Walk [ "W1N2" ]
                let carried = fieldOver Walk [ "W1N1"; "W1N2" ]

                Expect.equal
                    (travelCost first "w" (Harvest "src-out"))
                    (Some 36)
                    "the ranking price crosses the same border"

                Expect.equal
                    held.PerCensus.Count
                    3
                    "and files its own far field in the same table: it prices no crowd, so the census signs it too"

                let ranked = fieldOver TravelCost [ "W1N2" ]

                // `Baseline` is `TravelCost` over empty ground, so the key
                // is normalised onto `TravelCost`.
                Expect.isSome
                    (firstStepBlindFor first "w" (Harvest "src-out"))
                    "the traffic-blind route crosses the same border"

                Expect.equal
                    held.PerCensus.Count
                    3
                    "and files nothing: the Baseline far leg is the TravelCost one, key and array"

                Expect.isTrue
                    (obj.ReferenceEquals(fieldOver TravelCost [ "W1N2" ], ranked))
                    "the same array, read under the ranking price's own key"

                let second = snapshot () |> ofViewRecalling (WalkTable()) held

                Expect.equal
                    (walkTicks second "w" (Harvest "src-out"))
                    (Some 18)
                    "the recalled field prices the same walk"

                Expect.equal
                    (travelCost second "w" (Harvest "src-out"))
                    (Some 36)
                    "and the recalled ranking field the same crossing"

                Expect.equal held.PerCensus.Count 3 "no second entry under the same keys"

                Expect.isTrue
                    (obj.ReferenceEquals(fieldOver Walk [ "W1N2" ], flooded)
                     && obj.ReferenceEquals(fieldOver Walk [ "W1N1"; "W1N2" ], carried)
                     && obj.ReferenceEquals(fieldOver TravelCost [ "W1N2" ], ranked))
                    "the second Atlas read the first's fields rather than running its own"

                // The tick the census moves: an empty table is flooded into.
                let fresh = FarFieldMemo.empty ()
                let dropped = snapshot () |> ofViewRecalling (WalkTable()) fresh

                Expect.equal
                    (walkTicks dropped "w" (Harvest "src-out"))
                    (Some 18)
                    "an empty table is flooded into, and prices the walk identically"

                Expect.equal
                    (travelCost dropped "w" (Harvest "src-out"))
                    (Some 36)
                    "and prices the crossing identically"

                Expect.equal fresh.PerCensus.Count 3 "the fields it ran are left in it"
            }

            test "the Seam walk rides the census table, like the walks and the far fields" {
                // `Atlas.seamWalkTicks` floods a whole room out of its Seam
                // band under a constant planning body, and everything it
                // reads is the census's (`SeamWalkTable`).
                let snapshot () =
                    northOfSnapshot
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                let held = FarFieldMemo.empty ()
                let first = snapshot () |> ofViewRecalling (WalkTable()) held

                Expect.equal
                    (seamWalkTicks first "W1N2" "W1N1" { X = 25; Y = 45 })
                    (Some 4)
                    "three plain tiles to the one beside the exit, and the step onto it"

                Expect.equal held.SeamWalks.Count 1 "the flood is left in the table it was handed"

                // Poisoned before the second Atlas reads it: a reference
                // check on a table `memoised` never overwrites would pass either way.
                let poisoned = Array.copy held.SeamWalks.[("W1N2", "W1N1")]
                poisoned.[25 * Engine.roomSide + 45] <- 99
                held.SeamWalks.[("W1N2", "W1N1")] <- poisoned
                let second = snapshot () |> ofViewRecalling (WalkTable()) held

                Expect.equal
                    (seamWalkTicks second "W1N2" "W1N1" { X = 25; Y = 45 })
                    (Some 98)
                    "the second Atlas read the handed table, poison and all, rather than running its own"

                Expect.equal held.SeamWalks.Count 1 "and filed nothing beside it"

                let fresh = FarFieldMemo.empty ()
                let dropped = snapshot () |> ofViewRecalling (WalkTable()) fresh

                Expect.equal
                    (seamWalkTicks dropped "W1N2" "W1N1" { X = 25; Y = 45 })
                    (Some 4)
                    "an empty table is flooded into, and prices the walk identically"

                Expect.equal fresh.SeamWalks.Count 1 "the flood it ran is left in it"
            }

            test "the far leg is blind to a crowd standing on it; the near leg is not" {
                // The corridor is one tile wide, so a standing body is one
                // every path walks through: `carriedAcross`, `foldChain` and
                // `chainedInto` all price `noTraffic`, while the near leg
                // pays `Grid.occupancyPenalty` (`Engine.swampWeight`).
                let snapshot homeCreeps outCreeps names =
                    northOfSnapshot
                        (corridorHome homeCreeps)
                        [ { X = 25; Y = 0 }, Plain ]
                        { corridorOutpost with
                            CreepPositions = Map.ofList outCreeps
                        }
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        names

                // In the far corridor, on the walk down to the Seat.
                let farCrowd () =
                    snapshot
                        [ "w", { X = 25; Y = 10 } ]
                        [ "out", { X = 25; Y = 45 } ]
                        [ worker "w"; worker "out" ]

                // The same body fifteen tiles further up: a crowd that
                // *moved*, which under the old key was a new field.
                let farCrowdMoved () =
                    snapshot
                        [ "w", { X = 25; Y = 10 } ]
                        [ "out", { X = 25; Y = 30 } ]
                        [ worker "w"; worker "out" ]

                // In the creep's own room, between it and the exit.
                let nearCrowd () =
                    snapshot
                        [ "w", { X = 25; Y = 10 }; "home", { X = 25; Y = 5 } ]
                        []
                        [ worker "w"; worker "home" ]

                let held = FarFieldMemo.empty ()

                Expect.equal
                    (travelCost
                        (farCrowd () |> ofViewRecalling (WalkTable()) held)
                        "w"
                        (Harvest "src-out"))
                    (Some 36)
                    "the body standing in the far corridor costs nothing: the empty corridor's own 36"

                let farFieldHeld () =
                    held.PerCensus
                    |> Seq.filter (fun entry ->
                        let chain, _, _, _, priced, _ = entry.Key
                        chain = [ "W1N2" ] && priced = TravelCost)
                    |> Seq.map (fun entry -> entry.Value)
                    |> Seq.exactlyOne

                let beforeItMoved = farFieldHeld ()

                let moved = farCrowdMoved () |> ofViewRecalling (WalkTable()) held

                Expect.equal
                    (travelCost moved "w" (Harvest "src-out"))
                    (Some 36)
                    "the body moves and the crossing is the same 36"

                Expect.isTrue
                    (obj.ReferenceEquals(farFieldHeld (), beforeItMoved))
                    "and off the very field the tick before flooded: a crowd that moves is no more in the key than one that stands"

                // Poisoned in place before the second Atlas reads it.
                let far = farFieldHeld ()

                let landing = 25 * Engine.roomSide + 48
                far.[landing] <- far.[landing] + 100

                let second = farCrowd () |> ofViewRecalling (WalkTable()) held

                Expect.equal
                    (travelCost second "w" (Harvest "src-out"))
                    (Some 136)
                    "and the next tick prices the crossing off the field this one flooded, poison and all"

                Expect.isTrue
                    (obj.ReferenceEquals(farFieldHeld (), far))
                    "the same array, because a crowd that stands still is not in the key and neither is one that moves"

                // The same body in the room the creep is walking now.
                Expect.equal
                    (travelCost
                        (nearCrowd () |> ofViewRecalling (WalkTable()) (FarFieldMemo.empty ()))
                        "w"
                        (Harvest "src-out"))
                    (Some 46)
                    "the near leg keeps the surcharge: 36 plus the one occupied tile it cannot go round"
            }
        ]

[<Tests>]
let outpostHeavyAreaTests =
    testList
        "atlas outpost heavy work area"
        [
            test "an unposted rock a room away is no ground at all for a Work-heavy body" {
                let bare = twoRockRooms [] []

                Expect.isEmpty
                    (postsOf bare "src-out" |> tilesIn "W1N2")
                    "the premise: no container, no Post"

                Expect.equal
                    (workArea bare (Harvest "src-out") |> tilesIn "W1N2")
                    outpostSeats
                    "the body-blind area is still the rock's two Seats"

                Expect.isEmpty
                    (workAreaFor bare "a-out" (Harvest "src-out") |> tilesIn "W1N2")
                    "and a heavy body standing in that room is handed none of them"

                Expect.equal
                    (workAreaFor bare "w-out" (Harvest "src-out") |> tilesIn "W1N2")
                    outpostSeats
                    "while a light body beside it keeps every Seat: the narrowing is the body's"

                Expect.equal
                    (travelCost bare "a" (Harvest "src-out"))
                    None
                    "so the Task is inapplicable to the Anchor that would have crossed for it"

                Expect.isSome
                    (travelCost bare "w" (Harvest "src-out"))
                    "and applicable to the light body, over the same Seam on the same tick"
            }

            test "the same body, the same tick, keeps the bare Seats of the rock at home" {
                // Two rocks with no container on either, and the room the
                // only difference between them.
                let bare = twoRockRooms [] []

                Expect.isEmpty
                    (postsOf bare "src-home" |> tilesHome bare)
                    "the home rock has no container either"

                Expect.equal
                    (workAreaFor bare "a" (Harvest "src-home") |> tilesHome bare)
                    (workArea bare (Harvest "src-home") |> tilesHome bare)
                    "at home the bare Seats are still the fallback (ADR 0020)"

                Expect.isSome
                    (travelCost bare "a" (Harvest "src-home"))
                    "and the Task the outpost's rock lost stays applicable here"
            }

            test "a container standing on the outpost Seat gives the heavy body its ground back" {
                let posted =
                    twoRockRooms
                        [ "cont-out", { X = 25; Y = 41 } ]
                        [ "cont-out", Structure BuiltKind.Container ]

                Expect.equal
                    (postsOf posted "src-out" |> tilesIn "W1N2")
                    (Set.singleton { X = 25; Y = 41 })
                    "the Seat under the container is the rock's Post"

                Expect.equal
                    (workAreaFor posted "a-out" (Harvest "src-out") |> tilesIn "W1N2")
                    (Set.singleton { X = 25; Y = 41 })
                    "and it is the one tile the heavy body may work from"

                Expect.isSome
                    (travelCost posted "a" (Harvest "src-out"))
                    "so the Anchor at home is priced across the border again"
            }
        ]

[<Tests>]
let crossRoomLeadTests =
    testList
        "atlas cross-room castWalkTicks"
        [
            /// The hauler unit empty: no fatigue at all, so the one-tick floor.
            let hauler = [ Carry; Carry; Move ]

            /// The Anchor row's minimal cast, empty: 2 ticks a plain step
            /// and 10 a swamp one.
            let anchorUnit = [ Work; Work; Carry; Move ]

            let openRings = [ { X = 25; Y = 0 }, Plain ], [ { X = 25; Y = 49 }, Plain ]

            test "a creep in the outpost is led across the Seam, on the join everything else uses" {
                // Born on (25,9), eight tiles up to (25,1), the exit at
                // (25,0), moved to (25,49) free, (25,48) and seven more to
                // the Seat at (25,41): seventeen.
                let homeRing, outpostRing = openRings

                let atlas =
                    leadAcross
                        homeRing
                        outpostRing
                        [ "w", { X = 25; Y = 9 } ]
                        [ creepWith "w" 0 hauler ]

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn (at "W1N2" outpostSeat))
                    (Some 17)
                    "eight near, the exit, and eight in the outpost"

                // The other clock: a creep on the birth tile is walked to
                // that Seat in exactly the ticks the lead charges.
                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (castWalkTicks atlas hauler leadSpawn (at "W1N2" outpostSeat))
                    "the Matcher's walk and the lead's walk price one crossing"
            }

            test "the exit tile is charged what the body pays to step onto it" {
                // The Anchor unit pays 2 ticks a plain tile: sixteen near,
                // the exit, sixteen in the outpost. A swamp crossing costs
                // it ten rather than two.
                let homeRing, outpostRing = openRings

                let over ring =
                    leadAcross [ { X = 25; Y = 0 }, ring ] outpostRing [] []

                Expect.equal
                    (castWalkTicks
                        (leadAcross homeRing outpostRing [] [])
                        anchorUnit
                        leadSpawn
                        (at "W1N2" outpostSeat))
                    (Some 34)
                    "sixteen near, two onto a plain exit, sixteen in the outpost"

                Expect.equal
                    (castWalkTicks (over Swamp) anchorUnit leadSpawn (at "W1N2" outpostSeat))
                    (Some 42)
                    "and a swamp exit is charged its own ten"
            }

            test "a goal in the colony's own room is led off the home flood, unchanged" {
                // Nine tiles down the corridor from (25,11), no band consulted.
                let homeRing, outpostRing = openRings
                let atlas = leadAcross homeRing outpostRing [] []

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn (at "W1N1" { X = 25; Y = 20 }))
                    (Some 9)
                    "the home leg alone, priced as it was before the border was crossable"
            }

            test "no crossing and no ring each lead nobody" {
                // Never a zero, which would leave the creep counted as
                // living for ever. Two of `seams`'s absences; the third (a
                // projected room that is no orthogonal neighbour) is
                // `RoomInvariantTests.seamTests`'.
                let homeRing, outpostRing = openRings
                let open' = leadAcross homeRing outpostRing [] []
                let walled = leadAcross [ { X = 25; Y = 0 }, Wall ] outpostRing [] []

                Expect.isEmpty (seams walled "W1N1" "W1N2") "the premise: the exit is walled"

                Expect.equal
                    (castWalkTicks walled hauler leadSpawn (at "W1N2" outpostSeat))
                    None
                    "no crossing, no lead"

                Expect.equal
                    (castWalkTicks open' hauler leadSpawn (at "W1N2" outpostSeat))
                    (Some 17)
                    "open that one tile and the same two rooms lead again"

                Expect.equal
                    (castWalkTicks open' hauler leadSpawn (at "W5N5" outpostSeat))
                    None
                    "and a room the projection carries no ring for has no band to be led over"
            }

            test "a creep on the border ring itself leads nobody, on either side of it" {
                // The tick a crossing lands the engine parks the creep on
                // the far room's ring tile: for one tick the creep is simply
                // counted living, and the next tick it is led again.
                let homeRing, outpostRing = openRings
                let atlas = leadAcross homeRing outpostRing [] []

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn (at "W1N2" { X = 25; Y = 49 }))
                    None
                    "the landing tile is the outpost's ring, and no ground of it"

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn (at "W1N1" { X = 25; Y = 0 }))
                    None
                    "and the exit tile is the home room's ring, which the flood never enters"
            }

            test "the far leg joins the walk table, under the room the goal stands in" {
                // The table holds one entry per goal *room*, not per goal
                // tile (#169).
                let homeRing, outpostRing = openRings
                let walks = WalkTable()

                let byRoom () =
                    walks
                    |> Seq.map (fun entry ->
                        let _, _, room = entry.Key
                        room, entry.Value)
                    |> Map.ofSeq

                let atlas =
                    leadAcrossSnapshot homeRing outpostRing [] []
                    |> ofViewRecalling walks (FarFieldMemo.empty ())

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn (at "W1N2" outpostSeat))
                    (Some 17)
                    "the premise: the lead is priced across the border"

                Expect.equal
                    (byRoom () |> Map.toList |> List.map fst)
                    [ "W1N1"; "W1N2" ]
                    "the near leg is filed under home and the joined far leg under the outpost"

                let flooded = byRoom ()

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn (at "W1N2" { X = 25; Y = 42 }))
                    (Some 16)
                    "a second tile of the same outpost — one back toward the border, one tick nearer — is read off that same entry"

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn (at "W1N1" { X = 25; Y = 20 }))
                    (Some 9)
                    "and the home lead reads the entry filed under home"

                Expect.equal
                    walks.Count
                    2
                    "neither adds an entry: the key names the room, never the goal"

                // The recall half, both rooms at once.
                let second =
                    leadAcrossSnapshot homeRing outpostRing [] []
                    |> ofViewRecalling walks (FarFieldMemo.empty ())

                Expect.equal
                    (castWalkTicks second hauler leadSpawn (at "W1N2" outpostSeat))
                    (Some 17)
                    "the recalled far leg prices the same lead"

                Expect.equal
                    (castWalkTicks second hauler leadSpawn (at "W1N1" { X = 25; Y = 20 }))
                    (Some 9)
                    "and the recalled near leg the same home one"

                Expect.equal walks.Count 2 "no second entry under either key"

                for room in [ "W1N1"; "W1N2" ] do
                    Expect.isTrue
                        (obj.ReferenceEquals(Map.find room (byRoom ()), Map.find room flooded))
                        $"the second Atlas read {room}'s flood rather than running its own"

                Expect.equal
                    (castWalkTicks second hauler leadSpawn (at "W5N5" outpostSeat))
                    None
                    "a room the projection carries no ring for is still led over by nobody"

                Expect.equal
                    walks.Count
                    2
                    "and leaves no entry behind: a band that answers empty is asked again, never remembered"
            }
        ]

[<Tests>]
let crossRoomHaulTests =
    testList
        "atlas cross-room haulRoundTripTicks"
        [
            test "the round trip across a border is two Seam joins, one per leg" {
                // Loaded, two ticks a step: seven up to (25,48) is 14, the
                // crossing 2, and (25,1) plus eight to (25,9) is 18. Empty,
                // every tile is on the one-tick floor: 7 + 1 + 9 = 17.
                let atlas = haulAcross [ { X = 25; Y = 0 }, Plain ] [ { X = 25; Y = 49 }, Plain ]

                Expect.equal
                    (roundTripOf atlas)
                    (Some 51)
                    "fourteen, two and eighteen out; seven, one and nine back"
            }

            test "each leg is the walk's own join, read off the same Seam band" {
                // The two legs pinned against `walkTicks`: one creep
                // carrying the load the leg out is priced for, one empty.
                let atlas = haulAcross [ { X = 25; Y = 0 }, Plain ] [ { X = 25; Y = 49 }, Plain ]

                let out = walkTicks atlas "loaded" (Refill("spawn-1", Energy))
                let back = walkTicks atlas "empty" (Refill("spawn-1", Energy))

                Expect.equal out (Some 34) "the premise: the loaded leg the Matcher would price"
                Expect.equal back (Some 17) "and the empty one"

                Expect.equal
                    (roundTripOf atlas)
                    (Option.map2 (+) out back)
                    "the round trip is those two walks and nothing else"
            }

            test "a swamp crossing is charged to each leg at its own factor" {
                // Ten ticks onto the crossing loaded and one empty: the
                // round trip gains exactly the loaded leg's eight, which a
                // single crossing priced once for both legs could not produce.
                let atlas = haulAcross [ { X = 25; Y = 0 }, Plain ] [ { X = 25; Y = 49 }, Swamp ]

                Expect.equal
                    (roundTripOf atlas)
                    (Some 59)
                    "the loaded leg pays the swamp its ten, the empty leg its one"
            }

            test "a border with no crossing prices no round trip" {
                // Never a zero, which would hire a fleet for free.
                let atlas = haulAcross [ { X = 25; Y = 0 }, Wall ] [ { X = 25; Y = 49 }, Plain ]

                Expect.isEmpty (seams atlas "W1N2" "W1N1") "the premise: the exit is walled"

                Expect.equal (roundTripOf atlas) None "unpriceable geometry hires nobody"
            }
        ]

[<Tests>]
let crossRoomStepTests =
    testList
        "atlas cross-room step"
        [
            test "the mover aims at the crossing the price was paid at, not the nearest one" {
                // The mover's half of the two-exit border stated up top: a
                // mover that minimised the near leg again would walk the
                // creep up column 25 to a crossing it was never priced at (#142).
                let across = twoExitAcross
                let bothOpen = twoExitBothOpen ()

                Expect.equal
                    (walkTicks bothOpen "w" (Harvest "src-out"))
                    (Some 19)
                    "the premise: the price crosses at (27,0), the farther of the two"

                Expect.equal
                    (firstStepFor bothOpen "w" (Harvest "src-out"))
                    (Some { X = 26; Y = 10 })
                    "so the creep leaves its column sideways, toward that crossing"

                // Wall the cheap crossing and the step falls back with the price.
                let swampOnly =
                    across
                        [
                            { X = 25; Y = 49 }, Plain
                            { X = 26; Y = 49 }, Wall
                            { X = 27; Y = 49 }, Wall
                        ]

                Expect.equal
                    (walkTicks swampOnly "w" (Harvest "src-out"))
                    (Some 46)
                    "with only the dear crossing left, the price is paid at (25,0)"

                Expect.equal
                    (firstStepFor swampOnly "w" (Harvest "src-out"))
                    (Some { X = 25; Y = 9 })
                    "and the same creep now walks straight up its own column instead"
            }

            test "a creep parked on the ring prices and crosses from the tile it stands on" {
                // A flood never relaxes onto the ring but *seeds* its origin
                // whatever that tile weighs, so a creep parked on the ring
                // is already beside every crossing next to it (#175).
                // Whether it should be aimed sideways along the ring is
                // #146's open question.
                //
                // Standing on the home ring at (24,0): priced at (25,0) it
                // pays one onto the exit and eight in the outpost, nine;
                // priced at (24,0) it would first step to (25,1) and pay
                // ten. Read the ring off the near side instead and both
                // cost ten, the tie falls to (24,0), and the creep is
                // walked inland to cross where it was never priced.
                let atlas =
                    northOf
                        (corridorHome [ "w", { X = 24; Y = 0 } ])
                        [ { X = 24; Y = 0 }, Plain; { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 24; Y = 49 }, Plain; { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.hasLength (seams atlas "W1N1" "W1N2") 2 "the premise: two crossings"

                Expect.isTrue (standsOnSeam atlas "w") "and the creep stands on the ring"

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (Some 9)
                    "no approach to pay, the exit's own tick, and eight in the outpost"

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-out"))
                    (Some 18)
                    "and the same join in the ranking price's own units"

                Expect.equal
                    (firstStepFor atlas "w" (Harvest "src-out"))
                    (Some { X = 25; Y = 0 })
                    "so the step is the crossing the price was paid at, taken off the ring"
            }

            test "two crossings tied on the sum fall to the lowest exit, not to the cheaper half" {
                // The bounded band read settles for a crossing whose lower
                // bound is *at or under* the best sum, because the answer is
                // the smallest `(sum, exit)` pair and a tie still moves the
                // exit (#176).
                //
                // The creep at (23,1) on a plain row x=20..30:
                //   west (20,0), swamp: two steps to (21,1), five onto the
                //     exit, eleven in the outpost. Eighteen.
                //   east (30,0), plain: six steps to (29,1), one onto the
                //     exit, the same eleven. Eighteen.
                // The east crossing is read first, its far leg being the
                // cheaper half; a read that stopped at "strictly better"
                // would never look at the west one.
                let home stand =
                    { RoomLayer.empty with
                        Terrain =
                            TerrainGrid.ofList (plainLine [ for x in 20..30 -> { X = x; Y = 1 } ])
                        CreepPositions = Map.ofList [ "w", stand ]
                    }

                let outpost =
                    { RoomLayer.empty with
                        Terrain =
                            TerrainGrid.ofList (
                                plainLine
                                    [
                                        for y in 41..48 -> { X = 20; Y = y }
                                        for y in 41..48 -> { X = 30; Y = y }
                                        for x in 21..29 -> { X = x; Y = 41 }
                                    ]
                            )
                        TargetPositions = Map.ofList [ "src-out", { X = 25; Y = 40 } ]
                    }

                let across stand =
                    northOf
                        (home stand)
                        [ { X = 20; Y = 0 }, Swamp; { X = 30; Y = 0 }, Plain ]
                        outpost
                        [ { X = 20; Y = 49 }, Plain; { X = 30; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                let tied = across { X = 23; Y = 1 }

                Expect.hasLength (seams tied "W1N1" "W1N2") 2 "the premise: two crossings"

                Expect.equal
                    (walkTicks tied "w" (Harvest "src-out"))
                    (Some 18)
                    "two down the swamp exit and six down the plain one come to the same walk"

                Expect.equal
                    (travelCost tied "w" (Harvest "src-out"))
                    (Some 36)
                    "and they tie in the ranking price's units too, which double every step here"

                Expect.equal
                    (firstStepFor tied "w" (Harvest "src-out"))
                    (Some { X = 22; Y = 1 })
                    "so the creep is sent west, to the lower exit the tie falls to"

                // The pairwise control, one tile east: the sums stop tying.
                let untied = across { X = 24; Y = 1 }

                Expect.equal
                    (walkTicks untied "w" (Harvest "src-out"))
                    (Some 17)
                    "three, five and eleven the west way is nineteen; five, one and eleven the east way is seventeen"

                Expect.equal
                    (firstStepFor untied "w" (Harvest "src-out"))
                    (Some { X = 25; Y = 1 })
                    "and the step follows the winner east"
            }

            test "the last step onto an exit tile is a step nothing offers to stand on" {
                // The mover may push a creep *onto* an exit; every query
                // that offers somewhere to stand still refuses to name it.
                let atlas =
                    northOf
                        (corridorHome [ "w", { X = 25; Y = 1 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        corridorOutpost
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.equal
                    (firstStepFor atlas "w" (Harvest "src-out"))
                    (Some { X = 25; Y = 0 })
                    "standing beside the exit, the step is the exit itself"

                Expect.isFalse
                    (Set.contains { X = 25; Y = 0 } (walkableTilesIn atlas (atlasHome atlas)))
                    "and that tile is no walkable ground of this room"

                Expect.isFalse
                    (List.contains
                        { X = 25; Y = 0 }
                        (adjacentWalkableIn atlas (atlasHome atlas) { X = 25; Y = 1 }))
                    "no neighbour a parked creep may be displaced onto"

                Expect.isFalse
                    (Set.contains
                        { X = 25; Y = 0 }
                        (workArea atlas (Harvest "src-out") |> tilesIn "W1N2"))
                    "and no tile of any Work Area"
            }

            test "the action gate stays shut at the Seam and opens where the creep may stand" {
                // The engine's ranges are measured inside one room; the
                // gate opens off where the projection files the creep and
                // needs no rule of its own.
                let across creepAt outpostCreeps =
                    northOf
                        (corridorHome creepAt)
                        [ { X = 25; Y = 0 }, Plain ]
                        { corridorOutpost with
                            CreepPositions = Map.ofList outpostCreeps
                        }
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                let here = across [ "w", { X = 25; Y = 1 } ] []
                let landed = across [] [ "w", { X = 25; Y = 49 } ]
                let there = across [] [ "w", { X = 25; Y = 41 } ]

                Expect.isFalse
                    (mayActFor here "w" (Harvest "src-out"))
                    "a step from the Seam is still a room away from digging"

                Expect.equal
                    (firstStepFor here "w" (Harvest "src-out"))
                    (Some { X = 25; Y = 0 })
                    "and it is walked onto the exit on the very tick it may not act"

                // The tile the crossing delivers to, (25,49): the gate is
                // still shut there (the raw-range escape a ringed creep
                // takes measures nine, not one) and the Atlas answers the
                // step that opens it. `OutpostTests` drives it from the
                // landing tile to the dig.
                Expect.isFalse
                    (mayActFor landed "w" (Harvest "src-out"))
                    "the tile the engine puts it down on is no tile of the Work Area"

                Expect.equal
                    (firstStepFor landed "w" (Harvest "src-out"))
                    (Some { X = 25; Y = 48 })
                    "and the geometry has the step off the ring that the ordinary rules ask for"

                Expect.isTrue
                    (mayActFor there "w" (Harvest "src-out"))
                    "standing in the target's own room, the gate opens off `sharesRoom` alone"

                Expect.equal
                    (firstStepFor there "w" (Harvest "src-out"))
                    None
                    "standing in the Work Area it has arrived at, it has no step left to take"
            }

            test
                "a creep on the border ring stands on a Seam, and one on ground or nowhere does not" {
                // A creep left standing on a ring tile is moved out of the
                // room again at the end of the tick (#145).
                let at homeCreeps outpostCreeps =
                    northOf
                        (corridorHome homeCreeps)
                        [ { X = 25; Y = 0 }, Plain ]
                        { corridorOutpost with
                            CreepPositions = Map.ofList outpostCreeps
                        }
                        [ { X = 25; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.isTrue
                    (standsOnSeam (at [] [ "w", { X = 25; Y = 49 } ]) "w")
                    "landed on the outpost's ring"

                Expect.isTrue
                    (standsOnSeam (at [ "w", { X = 25; Y = 0 } ] []) "w")
                    "and on the home room's exit row alike"

                Expect.isFalse
                    (standsOnSeam (at [] [ "w", { X = 25; Y = 48 } ]) "w")
                    "one step inside, the creep is on ground"

                Expect.isFalse
                    (standsOnSeam (at [] []) "w")
                    "and a creep the projection cannot place stands on no Seam"
            }

            test "a tied band is committed to, not shuttled between" {
                // The creep walks a column equidistant from two mirror-image
                // crossings; the mover has to pick the same one every tick
                // or the creep walks a diagonal back and forth below the
                // border and never crosses.
                let home pos =
                    { RoomLayer.empty with
                        Terrain =
                            TerrainGrid.ofList (
                                plainLine
                                    [
                                        for y in 1..10 -> { X = 25; Y = y }
                                        for x in 23..27 -> { X = x; Y = 1 }
                                    ]
                            )
                        CreepPositions = Map.ofList [ "w", pos ]
                    }

                let outpost =
                    { RoomLayer.empty with
                        Terrain =
                            TerrainGrid.ofList (
                                plainLine
                                    [
                                        for y in 41..48 -> { X = 25; Y = y }
                                        for x in 23..27 -> { X = x; Y = 48 }
                                    ]
                            )
                        TargetPositions = Map.ofList [ "src-out", { X = 25; Y = 40 } ]
                    }

                let across ring pos =
                    northOf
                        (home pos)
                        [ for x in ring -> { X = x; Y = 0 }, Plain ]
                        outpost
                        [ for x in ring -> { X = x; Y = 49 }, Plain ]
                        [ "src-out", Source ]
                        [ worker "w" ]

                let start = { X = 25; Y = 10 }

                Expect.equal
                    (walkTicks (across [ 23 ] start) "w" (Harvest "src-out"))
                    (walkTicks (across [ 27 ] start) "w" (Harvest "src-out"))
                    "the premise: either crossing alone costs this creep the same walk"

                Expect.hasLength
                    (seams (across [ 23; 27 ] start) "W1N1" "W1N2")
                    2
                    "and with both open the band holds two of them"

                // Driven a tick at a time; the drive stops the tick the
                // step leaves this room's ground.
                let ground = (home start).Terrain

                let rec drive pos taken =
                    if List.length taken > 20 then
                        failtest "the creep never reached a crossing"
                    else
                        match firstStepFor (across [ 23; 27 ] pos) "w" (Harvest "src-out") with
                        | Some step when TerrainGrid.containsKey step ground ->
                            drive step (step :: taken)
                        | Some step -> List.rev (step :: taken)
                        | None -> List.rev taken

                Expect.equal
                    (drive start [])
                    [
                        { X = 25; Y = 9 }
                        { X = 25; Y = 8 }
                        { X = 25; Y = 7 }
                        { X = 25; Y = 6 }
                        { X = 25; Y = 5 }
                        { X = 25; Y = 4 }
                        { X = 25; Y = 3 }
                        { X = 25; Y = 2 }
                        { X = 24; Y = 1 }
                        { X = 23; Y = 0 }
                    ]
                    "one crossing, one route, and the exit tile is the last step of it"

                // The price falls by one plain step's units every tick of the drive.
                let priced =
                    start :: List.take 9 (drive start [])
                    |> List.map (fun pos ->
                        travelCost (across [ 23; 27 ] pos) "w" (Harvest "src-out"))

                Expect.equal
                    (priced |> List.pairwise |> List.map (fun (a, b) -> Option.map2 (-) a b))
                    (List.replicate 9 (Some 2))
                    "every step of the drive buys exactly the two units a plain step costs"
            }
        ]

[<Tests>]
let trunkPricingTests =
    testList
        "atlas trunk pricing"
        [
            test "a trunk takes two swamps to save two plain steps: paved length, not the walk" {
                // Goal A three steps across two swamps (8 as a road), goal
                // B five steps over plain (10).
                let atlas = swampShortcut 15 |> snapshotWith [] |> ofView

                let path =
                    trunkPathHome
                        atlas
                        Set.empty
                        { X = 10; Y = 10 }
                        (Set.ofList [ { X = 13; Y = 10 }; { X = 10; Y = 15 } ])

                Expect.equal
                    (List.last path)
                    { X = 13; Y = 10 }
                    "the swamp line is the cheaper road"

                Expect.equal (List.length path) 3 "three tiles, two of them swamp"

                // One fewer plain step on the long way and the plain way wins.
                let shorterPlain = swampShortcut 13 |> snapshotWith [] |> ofView

                Expect.equal
                    (List.last (
                        trunkPathHome
                            shorterPlain
                            Set.empty
                            { X = 10; Y = 10 }
                            (Set.ofList [ { X = 13; Y = 10 }; { X = 10; Y = 13 } ])
                    ))
                    { X = 10; Y = 13 }
                    "three plain steps (6) beat two swamps and a plain (8): the surcharge is real"
            }

            test "the swamp surcharge is the colony's tunable, and priced at the walk it detours" {
                // `Tuning.TrunkSwampWeight` at the road's three (#211) and
                // at the walking grid's ten it replaced.
                let room = swampShortcut 15

                let paved surcharge =
                    let view = snapshotWith [] room

                    trunkPathHome
                        (ofView
                            { view with
                                Tuning =
                                    { view.Tuning with
                                        TrunkSwampWeight = surcharge
                                    }
                            })
                        Set.empty
                        { X = 10; Y = 10 }
                        (Set.ofList [ { X = 13; Y = 10 }; { X = 10; Y = 15 } ])
                    |> List.last

                Expect.equal
                    (paved 3)
                    { X = 13; Y = 10 }
                    "at the road's own surcharge the trunk takes the swamps"

                Expect.equal
                    (paved Engine.swampWeight)
                    { X = 10; Y = 15 }
                    "at a walking creep's weight it pays five plain steps to avoid two swamps"

                // The flood's step table is `Array.init (Engine.swampWeight
                // + 1)`, so a grid holding eleven indexes off the end: an
                // `IndexOutOfRangeException` here and an `undefined` price
                // through `at`'s `[<Emit>]` accessor on the bundle.
                // `trunkPath` clamps the number at the engine's own weight.
                Expect.equal
                    (paved (Engine.swampWeight + 1))
                    (paved Engine.swampWeight)
                    "and past it the walking weight is what it gets, rather than an index off the step table"

                Expect.equal
                    (paved 40)
                    (paved Engine.swampWeight)
                    "however far past it the field is moved"
            }
        ]

[<Tests>]
let multiHopTests =
    testList
        "atlas multi-hop walk"
        [
            test "a walk over two borders is the same three terms, twice" {
                // The one-hop example extended by one room:
                //   W1N1  nine steps up to (25,1), one onto the exit (25,0)
                //   W1N2  landed free on (25,49), one onto (25,48),
                //         forty-seven to (25,1), one onto the exit
                //   W1N3  landed free on (25,49), one onto (25,48),
                //         seven more to (25,41)
                // Ten, forty-nine and eight.
                let atlas =
                    chainOfThree
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        [ { X = 25; Y = 0 }, Plain; { X = 25; Y = 49 }, Plain ]
                        corridorTransit
                        [ { X = 25; Y = 49 }, Plain ]
                        corridorOutpost
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.equal
                    (route atlas "W1N1" "W1N3")
                    (Some [ "W1N1"; "W1N2"; "W1N3" ])
                    "the chain crosses the transit room, which is the only way through"

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (Some 67)
                    "ten, forty-nine and eight"

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-out"))
                    (Some 134)
                    "and the same chain in the ranking price's units — two a plain step"
            }

            test "the far leg is the chain's, so a creep further back pays only its own tiles" {
                // What the chain answers does not depend on the creep.
                let atlas =
                    chainOfThree
                        (corridorHome [ "w", { X = 25; Y = 10 }; "w-back", { X = 25; Y = 14 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        [ { X = 25; Y = 0 }, Plain; { X = 25; Y = 49 }, Plain ]
                        corridorTransit
                        [ { X = 25; Y = 49 }, Plain ]
                        corridorOutpost
                        [ "src-out", Source ]
                        [ worker "w"; worker "w-back" ]

                Expect.equal
                    (walkTicks atlas "w-back" (Harvest "src-out"))
                    (Some 71)
                    "four tiles further back is four ticks dearer, both hops unchanged"
            }

            test
                "a chain is filed beside its own suffix, so the next chain over that tail floods nothing" {
                // Two chains toward one target share a tail
                // (`docs/research/cpu-headroom.md` §5.3): the transit body's
                // chain is the home body's suffix, so it is a lookup.
                let held = FarFieldMemo.empty ()

                let chains () =
                    held.PerCensus
                    |> Seq.map (fun entry ->
                        let chain, _, _, _, _, _ = entry.Key
                        chain)
                    |> List.ofSeq
                    |> List.sortBy List.length

                let atlas =
                    chainOfThreeSnapshot
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        [ { X = 25; Y = 0 }, Plain; { X = 25; Y = 49 }, Plain ]
                        { corridorTransit with
                            CreepPositions = Map.ofList [ "mid", { X = 25; Y = 20 } ]
                        }
                        [ { X = 25; Y = 49 }, Plain ]
                        corridorOutpost
                        [ "src-out", Source ]
                        [ worker "w"; worker "mid" ]
                    |> ofViewRecalling (WalkTable()) held

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (Some 67)
                    "the premise: the home body is priced over both borders"

                // Three entries: the far room's field, it carried one hop,
                // and it carried home (`pricedOffField`).
                Expect.equal
                    (chains ())
                    [ [ "W1N3" ]; [ "W1N2"; "W1N3" ]; [ "W1N1"; "W1N2"; "W1N3" ] ]
                    "and every suffix of the chain is filed: the far room's field, it carried one hop, and it carried home"

                Expect.equal
                    (walkTicks atlas "mid" (Harvest "src-out"))
                    (Some 28)
                    "the transit room's body prices the hop it has left"

                Expect.equal
                    (chains ())
                    [ [ "W1N3" ]; [ "W1N2"; "W1N3" ]; [ "W1N1"; "W1N2"; "W1N3" ] ]
                    "off the entry the first chain left — the transit body's whole chain is the home body's suffix, so it floods nothing"
            }

            test "a creep standing in the transit room prices the hop it has left" {
                // From (25,20): nineteen to (25,1), one onto the exit, eight
                // in the far room.
                let atlas =
                    chainOfThree
                        (corridorHome [])
                        [ { X = 25; Y = 0 }, Plain ]
                        [ { X = 25; Y = 0 }, Plain; { X = 25; Y = 49 }, Plain ]
                        { corridorTransit with
                            CreepPositions = Map.ofList [ "mid", { X = 25; Y = 20 } ]
                        }
                        [ { X = 25; Y = 49 }, Plain ]
                        corridorOutpost
                        [ "src-out", Source ]
                        [ worker "mid" ]

                Expect.equal
                    (walkTicks atlas "mid" (Harvest "src-out"))
                    (Some 28)
                    "nineteen up the transit room, the exit, and eight in the far one"
            }

            test "three crossings is the budget, and it prices" {
                // The longest chain the colony will ever join: two hops
                // cannot tell a fold that runs `n-1` times from one that
                // runs twice. 10 + 49 + 49 + 8.
                let plainCorridorRing = [ { X = 25; Y = 0 }, Plain; { X = 25; Y = 49 }, Plain ]

                let atlas =
                    chainOfFour
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        plainCorridorRing
                        corridorTransit
                        plainCorridorRing
                        corridorTransit
                        [ { X = 25; Y = 49 }, Plain ]
                        corridorOutpost
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.equal
                    (route atlas "W1N1" "W1N4")
                    (Some [ "W1N1"; "W1N2"; "W1N3"; "W1N4" ])
                    "three crossings, two transit rooms"

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (Some 116)
                    "ten, forty-nine, forty-nine and eight"

                Expect.equal
                    (travelCost atlas "w" (Harvest "src-out"))
                    (Some 232)
                    "and the same chain at two units a plain step"
            }

            test "a chain longer than the hop budget is no chain at all" {
                // Three rooms is two hops and inside the budget, so this
                // pins the shape rather than the number.
                let atlas =
                    chainOfThree
                        (corridorHome [ "w", { X = 25; Y = 10 } ])
                        [ { X = 25; Y = 0 }, Plain ]
                        // The middle room's north border is walled end to end.
                        [ { X = 25; Y = 49 }, Plain ]
                        corridorTransit
                        [ { X = 25; Y = 49 }, Plain ]
                        corridorOutpost
                        [ "src-out", Source ]
                        [ worker "w" ]

                Expect.equal
                    (route atlas "W1N1" "W1N3")
                    None
                    "no band out of the transit room, no chain"

                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    None
                    "and an unpriceable target is an absence, never a number (ADR 0004)"
            }
        ]

/// The L (#288): W1N1 is world (-2,-2), the corners are W1N2 north and W2N1
/// west, the target W2N2 is (-3,-3). The two chains are symmetric but the
/// corner: the creep at (25,25) is twenty-five ticks from either exit, the
/// target's corridor is entered within a tile of the same place either way,
/// and the north corner's corridor is swamp while the west one's is plain.
/// `RoomName.adjacent` names north before west, so the compass's chain is
/// the dear one. The home ring is the caller's, which is how one corner is
/// taken away.
let private cornerOfFour homeRing =
    let home =
        { RoomLayer.empty with
            Terrain =
                TerrainGrid.ofList (
                    plainLine
                        [
                            for y in 1..25 do
                                { X = 25; Y = y }

                            for x in 1..24 do
                                { X = x; Y = 25 }
                        ]
                )
            CreepPositions = Map.ofList [ "w", { X = 25; Y = 25 } ]
        }

    // The dear corner: landed on (25,49), west along swamp to the exit at
    // (0,48), landing on (49,48) of the target.
    let northCorner =
        { RoomLayer.empty with
            Terrain = TerrainGrid.ofList [ for x in 1..25 -> { X = x; Y = 48 }, Swamp ]
        }

    // The cheap one: landed on (49,25), north up plain to the exit at
    // (48,0), landing on (48,49) of the target.
    let westCorner =
        { RoomLayer.empty with
            Terrain = TerrainGrid.ofList (plainLine [ for y in 1..25 -> { X = 48; Y = y } ])
        }

    let target =
        { RoomLayer.empty with
            Terrain =
                TerrainGrid.ofList (
                    plainLine
                        [
                            for y in 1..48 do
                                if y <> 40 then
                                    { X = 48; Y = y }
                        ]
                )
            TargetPositions = Map.ofList [ "src-far", { X = 48; Y = 40 } ]
        }

    { SpatialInfo.empty with
        RoomName = Some "W1N1"
        Borders =
            Map.ofList
                [
                    "W1N1", Map.ofList homeRing
                    "W1N2", Map.ofList [ { X = 25; Y = 49 }, Plain; { X = 0; Y = 48 }, Plain ]
                    "W2N1", Map.ofList [ { X = 49; Y = 25 }, Plain; { X = 48; Y = 0 }, Plain ]
                    "W2N2", Map.ofList [ { X = 49; Y = 48 }, Plain; { X = 48; Y = 49 }, Plain ]
                ]
        TargetKinds = Map.ofList [ "src-far", Source ]
    }
    |> withHome (fun _ -> home)
    |> withNeighbour "W1N2" northCorner
    |> withNeighbour "W2N1" westCorner
    |> withNeighbour "W2N2" target
    |> snapshotWith [ worker "w" ]
    |> ofView

/// Both corners reachable. Functions, not values (#310).
let private cornersBothOpen () =
    cornerOfFour [ { X = 25; Y = 0 }, Plain; { X = 0; Y = 25 }, Plain ]

/// Only the compass's corner, the home room's west border walled end to end.
let private cornersNorthOnly () =
    cornerOfFour [ { X = 25; Y = 0 }, Plain ]

/// Only the cheap corner, the north border walled instead.
let private cornersWestOnly () =
    cornerOfFour [ { X = 0; Y = 25 }, Plain ]

/// A body that feels a swamp on both legs.
let private walkerBody = [ Work; Carry; Move ]

/// A hauler unit's body, which the corners split: loaded it feels the swamp
/// corner, empty it generates no fatigue and crosses it at a plain tile's
/// price, so each leg is cheapest round a different corner.
let private haulerBody = [ Carry; Move ]

/// The target room's container at (48,41) to the home room's spawn tile.
let private cornerHaulWith body atlas =
    haulRoundTripTicks atlas body (at "W2N2" { X = 48; Y = 41 }) (at "W1N1" { X = 25; Y = 25 })

let private cornerHaulOf atlas = cornerHaulWith walkerBody atlas

[<Tests>]
let cornerTests =
    testList
        "atlas multi-hop corner"
        [
            test "an L-shaped target is priced on the cheapest chain, not the compass's" {
                let bothCorners = cornersBothOpen ()
                let northCornerOnly = cornersNorthOnly ()
                let westCornerOnly = cornersWestOnly ()

                // Over the cheap corner: twenty-five out of the home room,
                // twenty-five in the west corner (the diagonal onto (48,24),
                // twenty-three up its column, the exit), eight in the
                // target. Fifty-eight. The north corner pays 121 for its
                // swamp and seven in the target, its landing at (49,48)
                // having ground diagonally beside it: 153.
                Expect.equal
                    (routes bothCorners "W1N1" "W2N2")
                    [ [ "W1N1"; "W1N2"; "W2N2" ]; [ "W1N1"; "W2N1"; "W2N2" ] ]
                    "both corners are chains of two hops, the compass's north one first"

                Expect.equal
                    (walkTicks northCornerOnly "w" (Harvest "src-far"))
                    (Some 153)
                    "the premise: through the swamp corner alone the walk is 25 + 121 + 7"

                Expect.equal
                    (walkTicks westCornerOnly "w" (Harvest "src-far"))
                    (Some 58)
                    "and through the plain one alone it is 25 + 25 + 8"

                Expect.equal
                    (walkTicks bothCorners "w" (Harvest "src-far"))
                    (Some 58)
                    "so with both open the price is the cheaper chain's, not the first found"

                Expect.equal
                    (travelCost bothCorners "w" (Harvest "src-far"))
                    (Some 116)
                    "and the ranking price is the same chain at two units a plain step"
            }

            test "the mover crosses at the corner the price was paid at" {
                let bothCorners = cornersBothOpen ()
                let northCornerOnly = cornersNorthOnly ()

                // A mover that followed the compass while the price
                // followed the terrain would walk the creep out of the
                // wrong border every tick.
                Expect.equal
                    (firstStepFor bothCorners "w" (Harvest "src-far"))
                    (Some { X = 24; Y = 25 })
                    "west along the home row, toward the crossing the price was won at"

                Expect.equal
                    (firstStepFor northCornerOnly "w" (Harvest "src-far"))
                    (Some { X = 25; Y = 24 })
                    "and north up its column when the swamp corner is the only one left"
            }

            test "the vision-grace mover still walks the compass's chain" {
                let bothCorners = cornersBothOpen ()

                // #297, pinned rather than fixed: the vision-grace mover has
                // no price to choose a chain with, so it takes the compass's.
                // Red the day #297 lands.
                Expect.equal
                    (stepTowardRoom bothCorners "w" "W2N2")
                    (Some(at "W1N1" { X = 25; Y = 24 }))
                    "north, the compass's corner, while `firstStepFor` above steps west"
            }

            test "the round trip is priced on the cheapest chain, both legs of it" {
                let bothCorners = cornersBothOpen ()
                let northCornerOnly = cornersNorthOnly ()
                let westCornerOnly = cornersWestOnly ()

                // The hauler quota sums these: the dear chain hires bodies
                // for a haul nobody makes.
                let dear = cornerHaulOf northCornerOnly
                let cheap = cornerHaulOf westCornerOnly

                Expect.equal
                    (cornerHaulOf bothCorners)
                    (Some 171)
                    "the cheaper corner's round trip, loaded out and empty back"

                Expect.equal cheap (Some 171) "which is the chain's own price with no rival"

                Expect.equal
                    dear
                    (Some 456)
                    "against 456 through the swamp corner, which is what the compass was buying"
            }

            test "the round trip's two legs are cheapest on one chain, never on two" {
                let bothCorners = cornersBothOpen ()
                let northCornerOnly = cornersNorthOnly ()
                let westCornerOnly = cornersWestOnly ()

                // Priced a leg at a time the trip comes out at 113, under
                // the better of the two chains a creep could actually walk:
                // no creep goes out round one corner and back round the other.
                let both = cornerHaulWith haulerBody bothCorners

                Expect.equal
                    (cornerHaulWith haulerBody westCornerOnly)
                    (Some 114)
                    "the plain corner's own trip: 58 out loaded, 56 back empty"

                Expect.equal
                    (cornerHaulWith haulerBody northCornerOnly)
                    (Some 208)
                    "the swamp corner's own: 153 out loaded, 55 back empty over free swamp"

                Expect.equal
                    both
                    (Some 114)
                    "so with both corners open the trip is the cheaper chain's"

                Expect.isGreaterThan
                    both
                    (Some 113)
                    "and never the 113 the two legs' separate minima sum to, which is no chain's trip"
            }

            test "the cast walk takes the cheapest chain too" {
                let bothCorners = cornersBothOpen ()
                let northCornerOnly = cornersNorthOnly ()
                let westCornerOnly = cornersWestOnly ()

                // `castWalkTicks` folds the same chain forwards.
                let lead atlas =
                    castWalkTicks atlas walkerBody { X = 25; Y = 25 } (at "W2N2" { X = 48; Y = 41 })

                Expect.equal (lead bothCorners) (Some 57) "the cheap corner's lead, to the tile"

                Expect.equal (lead westCornerOnly) (Some 57) "which is that chain's own price"

                Expect.equal (lead northCornerOnly) (Some 152) "against the swamp corner's 152"
            }
        ]
