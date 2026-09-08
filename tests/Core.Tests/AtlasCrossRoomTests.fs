/// The cross-room half of every query: a body in one room, its work in
/// another (ADR 0041, ADR 0042).
module Fabot.Core.Tests.AtlasCrossRoomTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Tests.AtlasFixtures

[<Tests>]
let crossRoomTests =
    testList
        "atlas cross-room walk"
        [
            test "a walk across the border is the near leg, the exit's own price and the far leg" {
                // The worked example, countable a tile at a time. The creep
                // stands at (25,10) of a one-wide plain corridor: nine steps
                // up to (25,1), one onto the exit at (25,0), then the engine
                // moves it to (25,49) of the outpost for nothing at the end
                // of that tick, one step off the landing onto (25,48), and
                // seven more down to (25,41) — the Work Area of a source at
                // (25,40) whose own tile the projection carries no ground
                // for. Eighteen tiles stepped onto, each one tick for a body
                // at fatigue parity: the crossing charges the exit tile and
                // the far room's first tile, and never the landing tile,
                // which the creep arrives on without moving.
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

                // The far leg is the target's, not the creep's: a second
                // creep four tiles further back pays four more and not a
                // tile besides, which is what one flood out of the target
                // serving the whole colony looks like from outside.
                Expect.equal
                    (walkTicks atlas "w-back" (Harvest "src-out"))
                    (Some 22)
                    "four tiles further back is four ticks dearer, the far leg unchanged"
            }

            test "a swamp exit is priced as a swamp step, not counted as one tile" {
                // ADR 0041 writes the join as `walk_here + 1 + walk_there`,
                // and #123 narrows that `+1` to the price ADR 0029 gives the
                // exit tile itself: `max(1, ceil(units / 2))`, the same rule
                // every other step is priced by. On plain ground under a
                // body at fatigue parity the two agree, which is why this is
                // a narrowing and not an overturning; on a swamp exit they
                // do not, and the engine charges the swamp.
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

            test "the walk takes the cheapest crossing in the band, not the nearest" {
                // Two exits, and the near one is the wrong one: the creep
                // reaches (25,0) in nine steps and (27,0) in ten, but the
                // outpost's column below (25,49) is swamp all the way down
                // while the one below (27,49) is plain. The minimum is over
                // the whole band — 10 + 1 + 8 against 9 + 1 + 36 — which is
                // the arithmetic ADR 0041 pays a Seam band for.
                let home =
                    { RoomLayer.empty with
                        Terrain =
                            Map.ofList (
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
                            Map.ofList
                                [
                                    for x in 25..27 -> { X = x; Y = 41 }, Plain
                                    for y in 42..48 -> { X = 25; Y = y }, Swamp
                                    for y in 42..48 -> { X = 27; Y = y }, Plain
                                ]
                        TargetPositions = Map.ofList [ "src-out", { X = 26; Y = 40 } ]
                    }

                let across farRing =
                    northOf
                        home
                        [
                            { X = 25; Y = 0 }, Plain
                            { X = 26; Y = 0 }, Wall
                            { X = 27; Y = 0 }, Plain
                        ]
                        outpost
                        farRing
                        [ "src-out", Source ]
                        [ worker "w" ]

                let bothOpen =
                    across
                        [
                            { X = 25; Y = 49 }, Plain
                            { X = 26; Y = 49 }, Wall
                            { X = 27; Y = 49 }, Plain
                        ]

                Expect.hasLength (seams bothOpen "W1N1" "W1N2") 2 "the premise: two crossings"

                Expect.equal
                    (walkTicks bothOpen "w" (Harvest "src-out"))
                    (Some 19)
                    "the farther exit, because the ground behind it is cheaper"

                Expect.equal
                    (travelCost bothOpen "w" (Harvest "src-out"))
                    (Some 38)
                    "and the ranking price joins at that same crossing, in its own units"

                // Take the cheap crossing out and the walk does not vanish:
                // it falls back to the dear one, which is the band being a
                // minimum rather than a choice made once.
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
                // A walled ring: the band is empty, so the minimum is over
                // nothing. That is the answer an unreachable Work Area in
                // the creep's own room gets — the Task is inapplicable to
                // this creep — and not the zero an unplaced target gets,
                // which would count it as free. The same projection with
                // that one tile of ring opened closes the case at the
                // bottom: the None is the wall's answer and not something
                // the two rooms would have said anyway.
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
                // ADR 0004's totality, at the seam #123 widens: a room that
                // is not in the projection at all leaves its targets
                // unplaced, and unplaceable geometry prices at zero, counts
                // against no Task and blocks no action. The walk answers it
                // the way travel cost always has.
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

                // Nor does it move anybody: there is no room to cross to,
                // so there is no Seam to aim at and no near side of one
                // (#142). An unplaced target is free, unblocking and
                // unwalked, all three off the same absence.
                Expect.equal
                    (firstStepFor atlas "w" (Harvest "src-far"))
                    None
                    "and the mover is given no step toward a room nobody projected"
            }

            test "the price crosses the border and the standing tiles do not" {
                // ADR 0041's Consequences drawn on one ColonyView: geometry
                // crosses, arbitration does not. The creep has an honest
                // number for a Task in the outpost — that is what puts the
                // outpost's Harvest in the same pool as home's — and no tile
                // of the outpost is ever handed to it as somewhere to stand,
                // step to or act from, because a `Set<Pos>` carries no room
                // and the mover reads it as this room's.
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

                // #142's correction, and the one line of this case that
                // moved: a step is not a standing tile. The mover is given
                // the near side of the crossing the price was paid at —
                // a tile of the creep's *own* room — because a Task that is
                // priced and unwalkable is a Task the Matcher gives away and
                // anti-thrash never takes back. What stays refused is
                // everything the creep would do on the far side: nowhere to
                // stand, and no action reaching over.
                Expect.equal
                    (firstStep atlas "w" (Harvest "src-out") handed |> tileHome atlas)
                    (Some { X = 25; Y = 9 })
                    "but it is walked up its own corridor toward the Seam it was priced at"
            }

            test "a heavy body's far leg is its Post's, not the source's nearest Seat" {
                // The far leg floods out of the Work Area *for this body*
                // (ADR 0020), so the narrowing has to cross the border with
                // the price: a Work-heavy creep is walked to the Seat under
                // the outpost's container even when a nearer Seat is on the
                // way. The source at (25,40) seats (25,41), one step off the
                // corridor, and (24,39), reachable only the long way round
                // through column 23 — eighteen tiles to the near Seat and
                // twenty to the Post. Three Work over two Move is heavy by
                // ADR 0016's predicate; every plain step costs it two ticks,
                // so the light body's numbers are half of its own.
                //
                // Unposted, this case read `Some 36` until #159: the heavy
                // body took ADR 0020's bare-Seat fallback across the border
                // and walked to a rock with no container under it. That
                // fallback is home's bootstrap and an outpost has another
                // one (ADR 0042), so the far leg now floods out of nothing
                // and there is no walk at all — the same answer a blocked
                // Post gives, one room over. The light body's own walk to
                // that Seat is untouched, which is what says the narrowing
                // is still the body's and not the room's.
                let outpost =
                    { RoomLayer.empty with
                        Terrain =
                            Map.ofList (
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
        ]

[<Tests>]
let outpostHeavyAreaTests =
    testList
        "atlas outpost heavy work area"
        [
            test "an unposted rock a room away is no ground at all for a Work-heavy body" {
                // ADR 0020's fallback to the bare Seats is home's bootstrap:
                // before the first container stands, an Anchor there still
                // has to dig, and it does it a few tiles from the spawn that
                // replaces it and the haulers already working the room. An
                // outpost bootstraps through a reserver and a light builder
                // instead (ADR 0042), so the fallback would put a heavy body
                // on a rock with nothing under it to catch twelve a tick and
                // no hauler quota to collect it — the strand ADR 0042 names
                // when it makes the container the switch.
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
                // The discriminator, in one projection: two rocks with no
                // container on either, one heavy body's answer for each, and
                // the room the only difference between them. Read on the
                // home creep rather than the outpost one because that is
                // where ADR 0020's fallback has to survive — a colony whose
                // own first container is not built yet.
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
                // The switch, at the geometry (ADR 0042). Nothing here is a
                // rule about outposts and heavy bodies: it is the same
                // narrowing to the Posts the home room has always had, and
                // the outpost's answer moves the tick a container stands.
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
            /// The hauler unit empty: no fatigue-generating part at all, so
            /// it rides the walk's one-tick floor over every tile.
            let hauler = [ Carry; Carry; Move ]

            /// The Anchor row's minimal cast, empty: two Work over one Move,
            /// so 4 units and 2 ticks a plain step and 20 units and 10 ticks
            /// a swamp one. The body an outpost's garrison is actually
            /// replaced by at a 300 bank (`anchorBodyFor`).
            let anchorUnit = [ Work; Work; Carry; Move ]

            let openRings = [ { X = 25; Y = 0 }, Plain ], [ { X = 25; Y = 49 }, Plain ]

            test "a creep in the outpost is led across the Seam, on the join everything else uses" {
                // ADR 0026's succession over ADR 0041's border (#153). The
                // replacement is born on (25,9), walks eight tiles up to
                // (25,1), steps onto the exit at (25,0), is moved to (25,49)
                // for nothing at the end of that tick, steps off onto
                // (25,48) and walks seven more down to the Seat at (25,41):
                // seventeen tiles stepped onto, one tick each for a body
                // that generates no fatigue.
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

                // The same ground, the same body and the same border, read
                // by the other clock in the colony: a creep standing on the
                // birth tile is walked to that Seat in exactly the ticks the
                // lead charges for reaching it. One join, two readers (ADR
                // 0030) — a second cross-room arithmetic of the lead's own
                // would agree here and drift everywhere else.
                Expect.equal
                    (walkTicks atlas "w" (Harvest "src-out"))
                    (castWalkTicks atlas hauler leadSpawn (at "W1N2" outpostSeat))
                    "the Matcher's walk and the lead's walk price one crossing"
            }

            test "the exit tile is charged what the body pays to step onto it" {
                // #123's narrowing of ADR 0041's literal `+1`, on the lead's
                // reader too: the Anchor unit pays 2 ticks a plain tile, so
                // sixteen near, the exit, and sixteen in the outpost. A
                // swamp crossing costs it ten rather than two, and the whole
                // lead is eight ticks longer — the eight ticks a colony
                // whose Seam is swamp has to cast its replacement earlier.
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
                // The regression ADR 0026's existing cases are the rest of:
                // the room the goal stands in is the caller's since #153,
                // and naming home is the walk this rule always ran — the
                // same flood out of the birth tiles, the same lookup, no
                // band consulted. Nine tiles down the corridor from (25,11).
                let homeRing, outpostRing = openRings
                let atlas = leadAcross homeRing outpostRing [] []

                Expect.equal
                    (castWalkTicks atlas hauler leadSpawn (at "W1N1" { X = 25; Y = 20 }))
                    (Some 9)
                    "the home leg alone, priced as it was before the border was crossable"
            }

            test "no crossing and no ring each lead nobody" {
                // Total (ADR 0004), one absence at a time: an unpriceable
                // Seam is no Seam, so the lead is absent exactly as an
                // unreachable tile inside one room makes it absent — never a
                // zero, which would leave the creep counted as living for
                // ever. The open ring at the bottom is what makes the None
                // above the wall's answer rather than the fixture's.
                //
                // Two of `seams`'s absences, and the third is not here: the
                // walled ring reaches the band and finds nothing passable in
                // it, the last room reaches no band at all because the
                // projection carries no ring under its name — while a room
                // that *is* projected and simply is not an orthogonal
                // neighbour is `RoomInvariantTests.seamTests`' case, on the
                // real captures. One answer, three reasons.
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
                // The tick a crossing lands: the engine parks the creep on
                // the far room's ring tile and `ColonyView.ofWorld` files it there, so
                // a lead asked for that tick is asked about a tile that is
                // in no room's ground (ADR 0041 keeps the rings beside the
                // projection, never inside it). Unpriceable, therefore, and
                // absent rather than zero-with-a-guess — for one tick the
                // creep is simply counted living, and the next tick it
                // stands on ground and is led again.
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
                // #169: the far leg rides ADR 0032's table exactly as the
                // near leg does, because every input it reads is in the
                // census — the goal room's weight grid is signed per
                // projected room, and the Seam band is terrain, which never
                // moves. So the key grew the room that keeps two rooms'
                // coordinates apart, and the table holds one entry per goal
                // *room* rather than one per goal tile: a second tile of the
                // same outpost is a lookup, not a flood.
                let homeRing, outpostRing = openRings
                let walks = WalkTable()

                let byRoom () =
                    walks
                    |> Seq.map (fun entry ->
                        let _, _, room = entry.Key
                        room, entry.Value)
                    |> Map.ofSeq

                let atlas = leadAcrossSnapshot homeRing outpostRing [] [] |> ofViewRecalling walks

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

                // The recall half, both rooms at once: an Atlas handed a
                // filled table reads the very arrays the first one flooded,
                // so a census that has not moved pays for no second
                // Dijkstra on either side of the border (ADR 0032).
                let second = leadAcrossSnapshot homeRing outpostRing [] [] |> ofViewRecalling walks

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
                // ADR 0042's outpost haul, countable a tile at a time.
                // Loaded, a plain step costs two ticks for this body and
                // the exit tile costs the same: seven steps up the
                // outpost's corridor to (25,48) is 14, the crossing at
                // (25,49) is 2, and the far leg — the step onto (25,1)
                // plus eight more down to (25,9) — is 18. Thirty-four out.
                // Empty, every one of those tiles sits on ADR 0029's
                // one-tick floor: 7 + 1 + 9 = 17 back. Fifty-one for the
                // round trip, which is the order ADR 0042 costs an unpaved
                // outpost haul at and not a bug.
                let atlas = haulAcross [ { X = 25; Y = 0 }, Plain ] [ { X = 25; Y = 49 }, Plain ]

                Expect.equal
                    (roundTripOf atlas)
                    (Some 51)
                    "fourteen, two and eighteen out; seven, one and nine back"
            }

            test "each leg is the walk's own join, read off the same Seam band" {
                // ADR 0030's law, at the reader that used to be exempt from
                // it: the quota's round trip must be the same arithmetic
                // the Matcher ranks on and the mover walks, and not a
                // second cross-room pricing of its own. So the two legs are
                // pinned against `walkTicks` — one creep carrying the load
                // the leg out is priced for, one empty as the leg back is —
                // walking to the very tiles a transfer reaches the spawn
                // from.
                let atlas = haulAcross [ { X = 25; Y = 0 }, Plain ] [ { X = 25; Y = 49 }, Plain ]

                let out = walkTicks atlas "loaded" (Refill "spawn-1")
                let back = walkTicks atlas "empty" (Refill "spawn-1")

                Expect.equal out (Some 34) "the premise: the loaded leg the Matcher would price"
                Expect.equal back (Some 17) "and the empty one"

                Expect.equal
                    (roundTripOf atlas)
                    (Option.map2 (+) out back)
                    "the round trip is those two walks and nothing else"
            }

            test "a swamp crossing is charged to each leg at its own factor" {
                // The trap the two legs exist for. Swamp weighs five
                // times plain, so this body pays ten ticks to step onto
                // the crossing loaded and one empty — the empty leg's step
                // was already on ADR 0029's floor and cannot get dearer.
                // Against the plain-exit case above the round trip gains
                // exactly the loaded leg's eight, which a single crossing
                // priced once for both legs could not produce at any
                // factor.
                let atlas = haulAcross [ { X = 25; Y = 0 }, Plain ] [ { X = 25; Y = 49 }, Swamp ]

                Expect.equal
                    (roundTripOf atlas)
                    (Some 59)
                    "the loaded leg pays the swamp its ten, the empty leg its one"
            }

            test "a border with no crossing prices no round trip" {
                // ADR 0004, and the shape the hauler quota reads it in: an
                // unpriceable Seam is no Seam, so the haul has no price and
                // the container hires nobody — never a zero, which would
                // hire a fleet for free.
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
                // #142's trap, on the fixture the price is already pinned
                // on: (25,0) is nine steps away and (27,0) ten, but the
                // outpost's column under (25,49) is swamp and the one under
                // (27,49) is plain, so the band's minimum is the *farther*
                // exit. A mover that minimised the near leg again — its own
                // second minimisation — would walk the creep up column 25
                // to a crossing it was never priced at, and the two answers
                // would agree on every number and split on this one.
                let home =
                    { RoomLayer.empty with
                        Terrain =
                            Map.ofList (
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
                            Map.ofList
                                [
                                    for x in 25..27 -> { X = x; Y = 41 }, Plain
                                    for y in 42..48 -> { X = 25; Y = y }, Swamp
                                    for y in 42..48 -> { X = 27; Y = y }, Plain
                                ]
                        TargetPositions = Map.ofList [ "src-out", { X = 26; Y = 40 } ]
                    }

                let across farRing =
                    northOf
                        home
                        [
                            { X = 25; Y = 0 }, Plain
                            { X = 26; Y = 0 }, Wall
                            { X = 27; Y = 0 }, Plain
                        ]
                        outpost
                        farRing
                        [ "src-out", Source ]
                        [ worker "w" ]

                let bothOpen =
                    across
                        [
                            { X = 25; Y = 49 }, Plain
                            { X = 26; Y = 49 }, Wall
                            { X = 27; Y = 49 }, Plain
                        ]

                Expect.equal
                    (walkTicks bothOpen "w" (Harvest "src-out"))
                    (Some 19)
                    "the premise: the price crosses at (27,0), the farther of the two"

                Expect.equal
                    (firstStepFor bothOpen "w" (Harvest "src-out"))
                    (Some { X = 26; Y = 10 })
                    "so the creep leaves its column sideways, toward that crossing"

                // Wall the cheap crossing and the price falls back to the
                // near one; the step falls back with it, which is the pair
                // moving together rather than one of them being a constant.
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
                // The one tile off a room's ground a near leg is honestly
                // read at (#175). The near side of a crossing is the room's
                // own ground and nothing else — the ring is not ground (ADR
                // 0036) and a flood never relaxes onto it — but a flood
                // *seeds* its origin whatever that tile weighs, so a creep
                // the engine parked on the ring the tick it crossed (#142,
                // #145) is already standing beside every crossing next to
                // it, at no cost at all.
                //
                // What is pinned here is the price and the step #175 found
                // in the tree, not a ruling on #146: whether a creep on the
                // ring should be aimed sideways along it at the crossing
                // next door is that ticket's open question, and this one
                // only had to leave the answer where it was.
                //
                // Countable: the creep stands on the home room's ring at
                // (24,0), with two crossings open. Priced at (25,0) it pays
                // nothing to approach — it is standing beside it — one tick
                // onto the exit and eight in the outpost, which is nine.
                // Priced at (24,0) it would first step to (25,1), the only
                // ground beside that exit, and pay ten. So the band's
                // minimum is (25,0), the crossing the creep can reach
                // without leaving the ring, and the step is that exit
                // itself. Read the ring off the near side instead and both
                // crossings cost ten, the tie falls to (24,0), and the
                // creep is walked inland to (25,1) to cross where it was
                // never priced.
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
                // The tie the bounded band read must not turn over (#176).
                // The near leg now stops at the crossing that wins, and the
                // rule that decides whether a crossing is worth settling for
                // is "its lower bound is at or under the best sum" — *at*,
                // and not merely under, because the answer is the smallest
                // `(sum, exit)` pair and never the smallest sum alone. A
                // crossing that can only tie still moves the exit, and the
                // exit is where the mover walks (#142).
                //
                // Countable. The creep stands at (23,1) on a plain row from
                // x=20 to x=30, and the band holds two crossings:
                //
                //   west (20,0), a *swamp* exit: two steps to (21,1), the
                //     ground beside it, five ticks onto the swamp exit, and
                //     eleven in the outpost — eighteen.
                //   east (30,0), a plain exit: six steps to (29,1), one tick
                //     onto the exit, and the same eleven — eighteen.
                //
                // The outpost's eleven is the same both sides by
                // construction: from the tile beside either landing, seven
                // down its column, a diagonal onto the row at y=41, and three
                // along it to the nearest tile of the source's Work Area.
                //
                // So the sums tie, and the east crossing is the one the band
                // reads first — its exit price and far leg are the cheaper
                // half, which is the order the bound is taken in. A read that
                // stopped at "strictly better" would never look at the west
                // crossing at all and would walk the creep east, to a
                // crossing the pre-#176 minimum over the whole band never
                // picked: `List.min` over `(sum, exit)` pairs falls to the
                // lowest exit, and (20,0) is lower than (30,0).
                let home stand =
                    { RoomLayer.empty with
                        Terrain = Map.ofList (plainLine [ for x in 20..30 -> { X = x; Y = 1 } ])
                        CreepPositions = Map.ofList [ "w", stand ]
                    }

                let outpost =
                    { RoomLayer.empty with
                        Terrain =
                            Map.ofList (
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

                // The pairwise control, one tile east: nothing about the
                // rooms changes, the sums stop tying, and the cheaper
                // crossing wins outright — so the west answer above is the
                // tie-break and not a westward bias.
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
                // ADR 0036 and ADR 0041 keep the exit out of the projection's
                // ground, and #142 does not put it back: the mover may push a
                // creep *onto* one, and every query that offers somewhere to
                // stand still refuses to name it. So the tile is reachable as
                // a destination and unreachable as a Seat, a Work Area member
                // or a walkable tile — which is what stops the Matcher ever
                // seating a creep the engine will empty out from under it.
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
                // The boundary #142 pushes a creep up to and never over.
                // `mayAct` is `false` across a border because the engine's
                // ranges are measured inside one room, and the mover asking
                // for a step toward the Seam does not make it `true`: the
                // Work Area a creep is handed is still empty, and the tile
                // it is walked to is still no tile of one. The gate opens
                // when the creep is standing in the target's own room and on
                // a tile of the Work Area there, which is a fact about where
                // the projection files it and needs no rule of its own —
                // and the tile the engine lands it on is not yet one.
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

                // The tile the crossing above actually delivers to, and not
                // an interior one hand-placed past it: stepping onto (25,0)
                // lands the creep on the outpost's own ring at (25,49). The
                // gate is still shut there — the landing tile is no Work
                // Area tile, and the raw-range escape a ringed creep takes
                // measures nine, not one — and the Atlas answers the step
                // that opens it. What walks that step is the Resolver,
                // which arbitrates each projected room by itself (ADR
                // 0041, #145): the outpost's pass hands the landed creep
                // that step, and `OutpostTests` drives it from the landing
                // tile to the dig. This test's subject is the gate, which
                // the Atlas keeps shut until the creep may stand.
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
                // The fact the far-side mover reads (#145): the tile the
                // engine lands a crossing creep on is a Seam, never ground
                // (ADR 0036), and a creep left standing on it is moved out
                // of the room again at the end of the tick. Read off the
                // coordinate, in whichever room the projection files the
                // creep under; a creep it places nowhere stands on no Seam
                // (ADR 0004).
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
                // The tie is where two minimisations diverge, so the fixture
                // holds the tie open for the whole approach: the creep walks
                // a column that stays equidistant from both crossings, and
                // the two are mirror images down to the far room's ground.
                // Priced separately they cost the same to the tick, which is
                // the premise; priced together the band's minimum picks one,
                // and the mover has to pick that same one every tick or the
                // creep walks a diagonal back and forth below the border and
                // never crosses at all.
                let home pos =
                    { RoomLayer.empty with
                        Terrain =
                            Map.ofList (
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
                            Map.ofList (
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

                // Driven a tick at a time, the way the Resolver drives it:
                // the creep stands where the last step put it and is asked
                // again. The drive stops the tick the step leaves this
                // room's ground, which is the tick it crosses.
                let ground = (home start).Terrain

                let rec drive pos taken =
                    if List.length taken > 20 then
                        failtest "the creep never reached a crossing"
                    else
                        match firstStepFor (across [ 23; 27 ] pos) "w" (Harvest "src-out") with
                        | Some step when Map.containsKey step ground -> drive step (step :: taken)
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

                // The price falls by one plain step's units every tick of
                // that drive — which is the two answers being read off one
                // minimisation and not two that happen to agree today.
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
                // #211. Two goals off one origin, everything else wall.
                // Goal A is three steps away across two swamps, goal B five
                // steps away over plain. Priced as a road (plain 2, swamp 3)
                // A costs 8 and B 10, so the line goes through the swamp;
                // priced as a walk (swamp 10) A would cost 22 and the router
                // would pave the long way round — which is the twenty-one-
                // tile loop W13S28 got.
                let atlas =
                    spatial
                        []
                        [
                            { X = 10; Y = 10 }, Plain
                            { X = 11; Y = 10 }, Swamp
                            { X = 12; Y = 10 }, Swamp
                            { X = 13; Y = 10 }, Plain
                            { X = 10; Y = 11 }, Plain
                            { X = 10; Y = 12 }, Plain
                            { X = 10; Y = 13 }, Plain
                            { X = 10; Y = 14 }, Plain
                            { X = 10; Y = 15 }, Plain
                        ]
                    |> snapshotWith []
                    |> ofView

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

                // Pairwise on the surcharge alone: one more plain step on
                // the long way and the swamp line still wins; one fewer and
                // the plain way does — the ratio is one step, which is what
                // a 1,200-energy construction difference is worth against
                // a permanent detour.
                let shorterPlain =
                    spatial
                        []
                        [
                            { X = 10; Y = 10 }, Plain
                            { X = 11; Y = 10 }, Swamp
                            { X = 12; Y = 10 }, Swamp
                            { X = 13; Y = 10 }, Plain
                            { X = 10; Y = 11 }, Plain
                            { X = 10; Y = 12 }, Plain
                            { X = 10; Y = 13 }, Plain
                        ]
                    |> snapshotWith []
                    |> ofView

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
                // `Tuning.TrunkSwampWeight` (ADR 0052 decision 5), pairwise
                // over the one field on one geometry: the same five-step
                // plain loop and the same three-step swamp line, priced at
                // the road's three and then at the walking grid's ten.
                //
                // Three is what #211 landed and ten is what it replaced, so
                // this is the regression written as a tunable rather than
                // as a literal: at ten the swamp line costs 2 + 10 + 10 = 22
                // against the loop's 10 and the router paves the long way
                // round, which is the twenty-one-tile detour W13S28 was
                // carrying.
                let room =
                    spatial
                        []
                        [
                            { X = 10; Y = 10 }, Plain
                            { X = 11; Y = 10 }, Swamp
                            { X = 12; Y = 10 }, Swamp
                            { X = 13; Y = 10 }, Plain
                            { X = 10; Y = 11 }, Plain
                            { X = 10; Y = 12 }, Plain
                            { X = 10; Y = 13 }, Plain
                            { X = 10; Y = 14 }, Plain
                            { X = 10; Y = 15 }, Plain
                        ]

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

                // One past the field's stated invariant, which is where a
                // tunable stops being a number and becomes a crash: the
                // flood's step table is `Array.init (Engine.swampWeight +
                // 1)`, so a grid holding eleven indexes off the end of it —
                // an `IndexOutOfRangeException` here and an `undefined`
                // price through `at`'s `[<Emit>]` accessor on the deployed
                // bundle, on a tick `Main.loop` runs under no handler.
                // `trunkPath` holds the number at the engine's own weight,
                // so a colony that asks for a costlier swamp than a walking
                // creep pays gets the walking creep's answer.
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
