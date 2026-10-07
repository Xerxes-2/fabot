/// How far a colony's view reaches: the transit rooms a chain crosses, what a
/// declaration is refused, and the scan set narrowed over the masked layer.
module Fabot.Core.Tests.ViewReachTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.ViewFixtures

/// A two-hop declaration and the room a chain to it crosses: the mother
/// declares W10S28, two hops west, so W11S28 is in her scan set for the walk
/// alone. Both rooms are seen and furnished, because a transit room's
/// promise is trivially kept while it is blind, and #286 is what happens the
/// tick a pioneer walks through one.
let private twoHop = "W10S28"
let private crossed = "W11S28"

let private twoHopDeclaration: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home <> mother then
            colony
        else
            { colony with
                Outposts =
                    colony.Outposts
                    @ [
                        {
                            RoomName = twoHop
                            Sources = [ "src-two", { Room = twoHop; X = 5; Y = 5 } ]
                            Controller = "ctrl-two", { Room = twoHop; X = 7; Y = 7 }
                        }
                    ]
            })

/// The same chain with the room in the middle **declared** too, which is what
/// makes a stand-down on it interesting: shut, it leaves the outpost list and
/// the chain to `twoHop` keeps it in the scan set, so one gate field turns a
/// worked outpost into a transit room (#271).
let private chainDeclaration: Colony list =
    twoHopDeclaration
    |> List.map (fun colony ->
        if colony.Home <> mother then
            colony
        else
            { colony with
                Outposts =
                    colony.Outposts
                    @ [
                        {
                            RoomName = crossed
                            Sources = [ "src-crossed", { Room = crossed; X = 9; Y = 9 } ]
                            Controller = "ctrl-crossed", { Room = crossed; X = 11; Y = 11 }
                        }
                    ]
            })

let private twoHopWorld =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.add
                twoHop
                (snd (
                    roomOf twoHop Ownership.Unowned [ "src-two", { X = 5; Y = 5 }, Source ]
                    |> withSources [ "src-two" ]
                ))
            |> Map.add
                crossed
                (snd (
                    roomOf
                        crossed
                        Ownership.Unowned
                        [
                            "src-crossed", { X = 9; Y = 9 }, Source
                            "ctrl-crossed", { X = 11; Y = 11 }, Controller
                            "cont-crossed", { X = 9; Y = 10 }, Structure BuiltKind.Container
                        ]
                    |> withSources [ "src-crossed" ]
                    |> withStores [ "cont-crossed", 1_500 ]
                    |> withSites [ "site-crossed" ]
                ))
    }

/// The same chain with **ore bleeding** in the room it crosses (#360): a pile
/// on the floor and a loaded tombstone, which is what a courier that dies on
/// the loaded leg leaves behind. Beside the furniture the room already has, so
/// the cut can be asked the question that matters — not "does anything survive"
/// but "does *only* the ore survive".
let private twoHopBleeding =
    { twoHopWorld with
        Rooms =
            twoHopWorld.Rooms
            |> Map.change
                crossed
                (Option.map (fun (facts: RoomFacts) ->
                    { facts with
                        Layer =
                            { facts.Layer with
                                TargetPositions =
                                    facts.Layer.TargetPositions
                                    |> Map.add "pile-crossed" { X = 25; Y = 25 }
                                    |> Map.add "tomb-crossed" { X = 26; Y = 25 }
                            }
                        TargetKinds =
                            facts.TargetKinds
                            |> Map.add "pile-crossed" (Dropped Thorium)
                            |> Map.add "tomb-crossed" Tombstone
                        Thorium =
                            facts.Thorium
                            |> Map.add "pile-crossed" 419
                            |> Map.add "tomb-crossed" 175
                        // A tombstone is a store, so the shell files its energy
                        // column too (#360's own smaller finding). It must not
                        // ride along: a store in a transit room is the 2,020
                        // demand that started all of this.
                        Stores = Map.add "tomb-crossed" 0 facts.Stores
                        Hits = Map.add "pile-crossed" { Hits = 1; HitsMax = 1 } facts.Hits
                        Owners = Map.add "tomb-crossed" Ownership.Ours facts.Owners
                    }))
    }

[<Tests>]
let transitTests =
    testList
        "a transit room carries ground and no work"
        [
            test "the room a chain crosses is projected, and its furniture is not" {
                // #286, found live on 2026-09-10: the room between W13S28 and
                // the nursery it was raising was reserved, anchored, given a
                // container and hauled from — 2,020 of a 2,540-energy haul
                // demand for a room no declaration names — because our own
                // pioneers walking through it were the vision that filed its
                // furniture.
                let view = viewUnder twoHopDeclaration twoHopWorld mother

                Expect.isEmpty view.Refused "the premise: a two-hop declaration is not refused"

                Expect.isTrue
                    (Map.containsKey crossed view.Spatial.Rooms)
                    "the transit room is in the projection, which is what a chain is priced over"

                Expect.isNonEmpty
                    (TerrainGrid.toList (SpatialInfo.layerOf view.Spatial crossed).Terrain)
                    "carrying the ground a walk crosses"

                Expect.isNonEmpty
                    (view.Spatial.Borders |> Map.find crossed)
                    "and the border ring the Seam is read off"

                Expect.isEmpty
                    (view.Sources |> List.filter (fun source -> source.Id = "src-crossed"))
                    "its rock is not pooled: no Harvest, so no anchor row hires for it"

                for id in [ "src-crossed"; "ctrl-crossed"; "cont-crossed" ] do
                    Expect.isFalse
                        (Map.containsKey id view.Spatial.TargetKinds)
                        $"{id}: nothing in a transit room is a target a Task could name"

                Expect.isEmpty
                    (view.ConstructionSites |> List.filter (fun site -> site.Id = "site-crossed"))
                    "and its sites are nobody's Build"

                Expect.isEmpty
                    (view.Spatial.Stores |> Map.filter (fun id _ -> id = "cont-crossed"))
                    "and its container is no store to haul from — the 2,020-demand row that started this"

                // Beside it, the declaration two hops out is untouched: its
                // furniture is laid in without vision, which is what makes
                // the room above a transit room and not a refusal.
                Expect.isTrue
                    (Map.containsKey "src-two" view.Spatial.TargetKinds)
                    "the two-hop outpost's own rock is placed off the declaration"
            }

            test "but ore bleeding on that floor comes through, and only the ore" {
                // #360, and the argument is `borrowable`'s: **decaying ore is
                // not furniture**. Everything else this cut drops is a standing
                // thing, exactly as workable on the day the room is declared.
                // Ore on the floor bleeds `ceil(amount/1000)` a tick, the
                // season never makes another gram, and a transit room is where
                // it most often lands — the delivery route is three crossings
                // and the courier is oldest on the loaded leg.
                let view = viewUnder twoHopDeclaration twoHopBleeding mother

                Expect.equal
                    (Map.tryFind "pile-crossed" view.Spatial.TargetKinds)
                    (Some(Dropped Thorium))
                    "the pile is named, which is what lets a rung pool it and the breach channel alarm on it"

                Expect.equal
                    (Map.tryFind "tomb-crossed" view.Spatial.TargetKinds)
                    (Some Tombstone)
                    "and the tombstone beside it, where a courier that dies loaded leaves its ore (#359)"

                Expect.equal
                    (SpatialInfo.heldIn view.Spatial Thorium "pile-crossed",
                     SpatialInfo.heldIn view.Spatial Thorium "tomb-crossed")
                    (419, 175)
                    "with the ore in them, which is the size of the leak and what the alarm's amount reads"

                Expect.equal
                    (SpatialInfo.placementOf view.Spatial "pile-crossed")
                    (Some(RoomPos.at crossed { X = 25; Y = 25 }))
                    "and where they are, without which nothing can be walked to"

                // And nothing else. This half is the test: the room's furniture
                // is still furniture, and one admitted kind must not carry a
                // store column, a hit count or an owner in with it.
                for id in [ "src-crossed"; "ctrl-crossed"; "cont-crossed" ] do
                    Expect.isFalse
                        (Map.containsKey id view.Spatial.TargetKinds)
                        $"{id}: the furniture stays out — admitting ore is not admitting the room"

                Expect.isEmpty
                    (view.Spatial.Stores
                     |> Map.filter (fun id _ -> id = "tomb-crossed" || id = "cont-crossed"))
                    "no store column for anything in that room, the tombstone's own 0 included: a store here is the 2,020-demand row #286 was filed for"

                Expect.isEmpty
                    (view.Spatial.Hits |> Map.filter (fun id _ -> id = "pile-crossed"))
                    "no hit count, so nothing here is pooled as a Repair"

                Expect.isEmpty
                    (view.Spatial.Owners |> Map.filter (fun id _ -> id = "tomb-crossed"))
                    "and no per-object owner: ore on a floor is nobody's, and an Emitter gates acts on that map (#318)"

                Expect.isTrue
                    (Set.contains crossed view.Crossed)
                    "the room is named as crossed, which is the reach `Facts.oursToSweep` widens by (#360)"

                Expect.isFalse
                    (Set.contains twoHop view.Crossed)
                    "and the declared outpost at the far end of the chain is not crossed but worked"
            }

            test
                "and its deposit, its Thorium and its extractor's clock go with the rest of the work" {
                // Held to `transiting`'s own test, whether the field is
                // *work*: a deposit's remaining amount is what the miner row's
                // quota reads and an extractor's cooldown is what its Emitter
                // gates on, and a colony that only walks through the room
                // works neither.
                let world = withThorium twoHopWorld crossed
                let view = viewUnder twoHopDeclaration world mother

                Expect.isFalse
                    (Map.containsKey $"min-{crossed}" view.Spatial.TargetKinds)
                    "the deposit is no target of hers"

                Expect.isFalse
                    (Map.containsKey $"min-{crossed}" view.Spatial.Thorium)
                    "so neither is what it has left to give"

                Expect.isFalse
                    (Map.containsKey $"ext-{crossed}" view.Spatial.Cooldowns)
                    "nor the clock on an extractor she will never harvest through"

                Expect.isFalse
                    (Map.containsKey $"ext-{crossed}" view.Spatial.Owners)
                    "nor whose it is, which is what an act out there would be gated on (#318)"
            }

            test "and a room the chain crosses is remembered no more than it is worked" {
                // The mother declares both rooms, so `crossed` is an outpost
                // of hers one hop out and `twoHop` one hop further through
                // it. Shut, `crossed` leaves her outpost list but the chain to
                // `twoHop` keeps it in her scan set, so it arrives as a
                // transit room. Before #271 its memory rode on:
                // `withdraw:cont-crossed` answered the vision grace the tick
                // the room went dark, and the mother's hauler was Kept and
                // walked back into the room the stand-down had withdrawn it
                // from for a whole `Tuning.VisionGrace`.
                let walked =
                    { twoHopWorld with
                        Sightings =
                            twoHopWorld.Sightings
                            |> Map.add
                                crossed
                                {
                                    Tick = twoHopWorld.Time - 1
                                    Targets = lazy (Set.ofList [ "cont-crossed"; "src-crossed" ])
                                    Rival = None
                                }
                    }

                let viewWith shut =
                    let holders =
                        World.creepColonies
                            Tuning.defaults
                            chainDeclaration
                            (World.living chainDeclaration walked)
                            noneShut
                            walked

                    ColonyView.ofWorld
                        Tuning.defaults
                        chainDeclaration
                        { StandDown.none with Shut = shut }
                        holders
                        walked
                        (chainDeclaration |> List.find (fun colony -> colony.Home = mother))

                let stoodDown = viewWith (Set.singleton crossed)

                Expect.isTrue
                    (Map.containsKey crossed stoodDown.Spatial.Rooms)
                    "the premise: shut, the room is still projected — the chain to the far outpost crosses it"

                Expect.isFalse
                    (Map.containsKey "cont-crossed" stoodDown.Spatial.TargetKinds)
                    "and its container is no Task of hers: that is the withdrawal ADR 0043 spells"

                Expect.isNone
                    (Map.tryFind crossed stoodDown.Sightings)
                    "so she remembers nothing of it either, and the grace has no room to answer with"

                // Pairwise, one field of the gate apart: worked, the room is
                // an outpost like any other and its memory rides on the view
                // whole. The narrowing is the transit room's alone, and it is
                // the gate that decides which of the two this room is.
                Expect.equal
                    ((viewWith Set.empty).Sightings |> Map.tryFind crossed)
                    (Map.tryFind crossed walked.Sightings)
                    "open, it is an outpost she works and she remembers what stood in it"
            }

            test
                "a shut outpost a chain to another crosses is evacuating while a body of hers stands in it" {
                // #512: already in the scan set for the chain to `twoHop`, the room
                // is evacuating all the same — the body in it walks home.
                let gate =
                    { StandDown.none with
                        Shut = Set.singleton crossed
                    }

                let viewIn world =
                    ColonyView.ofWorld
                        Tuning.defaults
                        chainDeclaration
                        gate
                        (World.creepColonies
                            Tuning.defaults
                            chainDeclaration
                            (World.living chainDeclaration world)
                            (Map.ofList [ mother, gate.Shut ])
                            world)
                        world
                        (chainDeclaration |> List.find (fun colony -> colony.Home = mother))

                let occupied =
                    { twoHopWorld with
                        Rooms =
                            twoHopWorld.Rooms
                            |> Map.change
                                crossed
                                (Option.map (fun facts ->
                                    snd (
                                        withCreeps
                                            [ "anchor-900-Spawn1", { X = 25; Y = 25 } ]
                                            (crossed, facts)
                                    )))
                        Creeps = creep "anchor-900-Spawn1" crossed :: twoHopWorld.Creeps
                    }

                Expect.isTrue
                    (Map.containsKey crossed (viewIn twoHopWorld).Spatial.Rooms)
                    "the premise: with nobody of hers in it, the chain keeps it projected"

                Expect.isEmpty (viewIn twoHopWorld).Evacuating "and merely crossed"

                Expect.equal
                    (viewIn occupied).Evacuating
                    (Set.singleton crossed)
                    "with her body in it, it is evacuating"
            }

            test "a declaration whose every chain crosses a rival's room is refused" {
                // #444: the one chain to `twoHop` runs through `crossed`, and
                // a rival owns it as last seen — the room is dark this tick,
                // so the memory is all that says so.
                let taken =
                    { twoHopWorld with
                        Sightings =
                            twoHopWorld.Sightings
                            |> Map.add
                                crossed
                                {
                                    Tick = twoHopWorld.Time - 500
                                    Targets = lazy Set.empty
                                    Rival = Some "Trepidimous"
                                }
                    }

                let view = viewUnder twoHopDeclaration taken mother

                Expect.contains
                    (view.Refused |> List.map (fun refused -> refused.RoomName))
                    twoHop
                    "the far outpost is refused, said on the layout record"

                Expect.isFalse
                    (Map.containsKey crossed view.Spatial.Rooms)
                    "and the rival's room is no transit room of hers"
            }

            test "the rival owners a reset reads back off Memory are avoided as the heap's were" {
                // `Main` seeds an empty heap off the `rooms` leaf: the room is dark
                // after the reset, so the seed is all that says who owns it.
                let owners = Map.ofList [ crossed, "Trepidimous" ]

                let seeded = World.recalling (World.seedRivals owners Map.empty) twoHopWorld

                Expect.isTrue (World.rivalHeld seeded crossed) "the seeded owner is read as held"

                Expect.equal
                    (World.rivalOwners seeded)
                    owners
                    "and is what the leaf is written from"

                Expect.contains
                    ((viewUnder twoHopDeclaration seeded mother).Refused
                     |> List.map (fun refused -> refused.RoomName))
                    twoHop
                    "so the far outpost is refused as before the reset"

                let held =
                    Map.ofList
                        [
                            crossed,
                            {
                                Tick = twoHopWorld.Time - 1
                                Targets = lazy Set.empty
                                Rival = None
                            }
                        ]

                Expect.isNone
                    (World.seedRivals owners held |> Map.find crossed).Rival
                    "a sighting the heap already holds is never replaced by the seed"
            }
        ]

[<Tests>]
let declarationTests =
    testList
        "an outpost declared across a border its home has not got"
        [
            test "every outpost a human has declared is inside the hop budget" {
                // The invariant #243 exists for, over the live constant: red
                // here rather than live, because a declaration past the
                // budget is accepted by every rule downstream and worked by
                // none of them, and the bodies bought for it stand by the
                // spawn for their whole lives.
                Expect.isNonEmpty Colony.declared "a declaration nobody made is nothing to check"

                Expect.isNonEmpty
                    (Colony.declared |> List.collect (fun colony -> colony.Outposts))
                    "and one with no outposts checks nothing either"

                let refused =
                    Colony.declared
                    |> List.collect (fun colony ->
                        colony.Outposts
                        |> List.filter (
                            Outpost.withinHopBudget Tuning.defaults.MaxHops colony.Home >> not
                        )
                        |> List.map (fun outpost -> outpost.RoomName)
                        |> List.map (fun room -> $"{room} is out of {colony.Home}'s reach"))

                Expect.isEmpty
                    refused
                    $"""every declared outpost is inside the hop budget: {String.concat "; " refused}"""
            }

            test "a room one axis step away is the only neighbour a name has" {
                // The rule itself, pairwise, on names alone — the altitude
                // that can be asked of a declaration before any terrain is
                // read. Each false case is one a live declaration could
                // plausibly be written as.
                Expect.isTrue (RoomName.neighbouring mother outpost) "W12S27 is north of W12S28"

                Expect.isTrue (RoomName.neighbouring mother child) "and W13S28 is west of it"

                Expect.isFalse
                    (RoomName.neighbouring mother "W13S27")
                    "a diagonal is two rooms away: the engine has no diagonal exit"

                Expect.isFalse
                    (RoomName.neighbouring mother "W12S26")
                    "two rooms up the same column share no border either"

                Expect.isFalse (RoomName.neighbouring mother mother) "and a room borders no self"

                Expect.isFalse
                    (RoomName.neighbouring mother "sim")
                    "a name outside the engine's grammar places nothing (ADR 0004)"

                // The one place the arithmetic could go wrong without a
                // case: W0 and E0 are adjacent columns either side of the
                // origin, so the offset is a subtraction over signed world
                // coordinates and never over the printed numbers.
                Expect.isTrue
                    (RoomName.neighbouring "W0S1" "E0S1")
                    "W0 and E0 are the two columns beside the origin"
            }

            test "a declared outpost inside the budget that no chain reaches is refused too" {
                // The room is two hops out, so the names say it is a
                // declaration a route could join, and the terrain says
                // otherwise: the rooms between it and home carry no border a
                // creep can cross. Refused on the walk and not on the
                // arithmetic, which is what `Outpost.routable` exists for
                // (#259).
                let walledIn =
                    { overreachingWorld with
                        Rooms =
                            overreachingWorld.Rooms
                            |> Map.map (fun name facts ->
                                if name = mother || name = child then
                                    facts
                                else
                                    { facts with Border = Map.empty })
                    }

                let view = viewUnder overreaching walledIn mother

                Expect.isTrue
                    (view.Refused
                     |> List.contains
                         {
                             RoomName = tooFar
                             Kind = DeclarationKind.Outpost
                         })
                    "the room past the budget is refused on the names, as it was"

                Expect.isFalse
                    (Map.containsKey outpost view.Spatial.Rooms)
                    "and the one inside it whose ring nothing can cross is out of the scan set"

                Expect.isTrue
                    (view.Refused
                     |> List.contains
                         {
                             RoomName = outpost
                             Kind = DeclarationKind.Outpost
                         })
                    "named on the layout record rather than dropped in silence"
            }

            test "a declared outpost past the hop budget is refused, and said out loud" {
                // #243's live shape: the room is declared, seen, furnished
                // and unowned, and the one thing it has not got is a chain
                // short enough to price. Accepted, its rock would be pooled,
                // its controller pooled as a Reserve and one reserver hired
                // per tick, all for a room no body can reach. So the view
                // refuses it, and names it: silence is what the ticket was
                // filed against.
                let view = viewUnder overreaching overreachingWorld mother

                Expect.equal
                    view.Refused
                    [
                        {
                            RoomName = tooFar
                            Kind = DeclarationKind.Outpost
                        }
                    ]
                    "the refusal names the room a human declared, and the kind they declared it as"

                Expect.isFalse
                    (Map.containsKey tooFar view.Spatial.Rooms)
                    "the room is not projected"

                Expect.isFalse (List.contains "src-far" (idsOf view)) "its rock is not pooled"

                Expect.isFalse
                    (Map.containsKey "ctrl-far" view.Spatial.TargetKinds)
                    "its controller is not placed, so no Reserve is pooled on it"

                Expect.isFalse
                    (Map.containsKey tooFar view.RoomControl)
                    "and nothing of it is priced at all"

                // Beside it, the outpost that does border her: the
                // refusal is one room's and not the outpost layer's.
                Expect.isTrue
                    (Map.containsKey outpost view.Spatial.Rooms)
                    "her real outpost is projected as it was"

                Expect.isTrue (List.contains "src-out" (idsOf view)) "and its rock is still pooled"

                // The healthy answer rides the channel too: a reader has to
                // be able to tell "nothing refused" from "this bundle does
                // not record refusals".
                Expect.isEmpty
                    (viewUnder declared pairWorld mother).Refused
                    "a declaration a Seam reaches refuses nothing, and says so"
            }
        ]

// ---- the scan set over the masked layer (#317) -----------------------------

/// The chain the live errand walks, as the server has it: W15S28, the two
/// rooms a shortest walk crosses, and the sector centre the Reactor stands
/// in. Real terrain, because what every case below turns on is what the
/// keeper margin does to a border ring, and W15S26 is the one room this repo
/// declares keeper rocks for (`Keepers.centres`, `rooms/W15S26.room`).
let private liveChain = [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ]

/// The Source Keeper room the mask is declared in, and the sector centre
/// `Errand.w15s25` names.
let private keeperRoom = "W15S26"
let private reactorRoom = "W15S25"

/// The home the declarations below are declared from — the one room in this
/// world whose ground is invented, and the reason the cases can be read at
/// the margin the bot ships. Why not W15S28, the live home: at the shipped
/// six the mask reaches exactly one of W15S26's four rings. The north ring's
/// nearest declared centre is the mineral at (38,7) and the east ring's the
/// lair at (42,39), both seven away; the south ring's nearest is ten away;
/// the west ring loses y ∈ 27..41 to the source at (4,33) and the lair at
/// (5,36), and the lair at (6,17), masked at five, takes the x = 1 ground
/// behind y ∈ 12..22. So the shipped margin closes no crossing of the live
/// chain (the `List.pairwise` loop below asserts that), and the one border it
/// can close is W15S26's west one, facing W16S26.
///
/// W16S26 has no capture, so its ring is written here. The margin is
/// declared by room name and is terrain-blind by construction, which is what
/// lets invented ground under a real name say something true about the real
/// declaration; the disagreement lands at `Tuning.defaults`, so every refusal
/// below is the one this bot ships and the knob is only ever the control.
let private keeperHome = "W16S26"

/// That invented ring: W16S26's east column — the one `Seam.pairsAcross` pairs
/// with W15S26's x = 0 column — open across y ∈ 19..21 and walled everywhere
/// else. Those three tiles are the disagreement and the whole of it: W15S26's
/// own west ring carries them over raw terrain, and at the shipped margin the
/// lair at (6,17) masks every x = 1 tile beside them, so each lands a body
/// where it can never step again; the raw crossings that keep ground behind
/// them (y ∈ 3..12, 22..23) are walled on this side, so no pair is left.
let private homeRing: Map<Pos, Terrain> =
    Map.ofList
        [
            for x in 0..49 do
                for y in 0..49 do
                    if x = 0 || x = 49 || y = 0 || y = 49 then
                        { X = x; Y = y }, (if x = 49 && y >= 19 && y <= 21 then Plain else Wall)
        ]

/// The world those five rooms make: a border ring and the ground behind it
/// apiece, and nothing else at all — `scanOf` reads both, the ring for
/// whether the engine lands a body across the border, the ground for whether
/// the body can then step off the landing. The captures bring their own
/// ground; `keeperHome`'s is invented plain, like its ring.
///
/// A function, and not because #310's rule reaches it: a `World` is `Map`
/// and `list` the whole way down, so a module-level one would be safe to
/// share. It is a function so the four captures are read by the tests that
/// ask for them instead of at module load.
let private keeperWorld () : World =
    { World.empty with
        Rooms =
            (keeperHome,
             { RoomFacts.empty with
                 Border = homeRing
                 Layer =
                     { RoomLayer.empty with
                         Terrain =
                             TerrainGrid.ofList
                                 [
                                     for x in 1..48 do
                                         for y in 1..48 -> { X = x; Y = y }, Plain
                                 ]
                     }
             })
            :: (liveChain
                |> List.map (fun name ->
                    let capture = RoomFixtures.load name

                    name,
                    { RoomFacts.empty with
                        Border = capture.Border
                        Layer =
                            { RoomLayer.empty with
                                Terrain = capture.Terrain
                            }
                    }))
            |> Map.ofList
    }

/// The one tuning below that is not the server's, and it is the **control**:
/// what the refusals are read under is `Tuning.defaults`. Five is the last
/// margin at which the crossing survives — the lair at (6,17), masked at four
/// there, is five from the x = 1 column and masks nothing on it — so this is
/// the same fixture with the mask pulled off that ground and nothing else
/// moved. The
/// margin is a `Tuning` knob and is swept as one here, exactly as
/// `RoomSeamTests` sweeps it: 1 + 3 + `ReachHorizon`, so one is five.
let private reachingTuning =
    { Tuning.defaults with
        ReachHorizon = 1
    }

/// The same room declared as each kind, so the two cases below differ in the
/// clause of `scanOf` that reads them and in nothing else.
///
/// The errand is the live one. The outpost is not and could not be: W15S25
/// is a sector centre with no controller, so this is that room written into
/// `Outposts` by a human's hand, carrying the capture's own rocks with a
/// controller the fixture names because the room has none. Neither half is
/// read by the narrowing: `Outpost.routable` asks the room name.
let private reactorAsOutpost () : Outpost =
    let capture = RoomFixtures.load reactorRoom

    {
        RoomName = reactorRoom
        Sources =
            capture.RealSources
            |> List.map (fun (id, pos) ->
                id,
                {
                    Room = reactorRoom
                    X = pos.X
                    Y = pos.Y
                })
        Controller = "ctrl-W15S25", { Room = reactorRoom; X = 31; Y = 22 }
    }

/// Every room the scan set carries when the declaration is admitted: the home,
/// the declared room, and both rooms a shortest two-hop walk could pass
/// through. W16S25 is the second of those and this world holds no facts for
/// it, which is right: the transit set is answered off the names, because
/// the route needs the rooms' terrain and the terrain needs them projected
/// (`RoomName.transitBetween`).
let private admittedScan =
    [ keeperHome; reactorRoom; keeperRoom; "W16S25" ] |> List.sort

/// One colony's scan set off that world, under whichever tuning is handed in.
/// No stage, no unowned home and no [[stand-down]]: the borrowed half of the
/// answer is a child's and this colony has none, so every room in `Scanned`
/// beyond the home is one of the two narrowed clauses' doing.
let private scanUnder tuning (colony: Colony) =
    World.scanOf tuning Map.empty Set.empty [ colony ] StandDown.none (keeperWorld ()) colony

let private declaringOutpost outpost : Colony =
    {
        Home = keeperHome
        Outposts = [ outpost ]
        Errands = []
        Salvage = []
        Mother = None
        Consignee = None
        Perimeter = []
        RampartFloors = Map.empty
        FirstSpawn = None
        Assaults = []
    }

let private declaringErrand errand : Colony =
    {
        Home = keeperHome
        Outposts = []
        Errands = [ errand ]
        Salvage = []
        Mother = None
        Consignee = None
        Perimeter = []
        RampartFloors = Map.empty
        FirstSpawn = None
        Assaults = []
    }

[<Tests>]
let scanSetMaskTests =
    testList
        "the scan set is narrowed over the layer the price is taken over"
        [
            test "the keeper mask closes at the shipped margin a crossing the raw rings leave open" {
                // Everything below rests on this: a fixture that stopped
                // exhibiting the disagreement would leave every case green
                // having checked nothing. `World.linked` is the one predicate
                // `scanOf` narrows both declaration kinds by, and the routable
                // question has to be asked over the same masked layer every
                // price is taken over, or the scan set admits a chain the
                // flood cannot walk (#317). The price half is
                // `AtlasSeamTests`'; what is pinned here is the wiring.
                let world = keeperWorld ()

                Expect.isTrue
                    (World.linked 0 world keeperHome keeperRoom)
                    "over raw rings the home's three exits face open ground in the Source Keeper room"

                Expect.isFalse
                    (World.linked (Tuning.keeperMargin Tuning.defaults) world keeperHome keeperRoom)
                    "and at the margin the bot ships it faces none, the lair behind that column taking every one"

                Expect.isTrue
                    (World.linked (Tuning.keeperMargin reachingTuning) world keeperHome keeperRoom)
                    "one margin below, the same crossing is open: what closed it is the mask and not the ring"

                Expect.isTrue
                    (World.linked (Tuning.keeperMargin Tuning.defaults) world keeperRoom reactorRoom)
                    "the rest of the way is open at the shipped margin, so the closed crossing is the first alone"

                // Zero is the raw reading: it masks the eight declared centres
                // themselves and no other tile, and none lies on a ring. The
                // live chain beside the invented crossing: the line that goes
                // red if a re-capture or a new keeper rock ever closes the
                // chain the errand really walks.
                for near, far in List.pairwise liveChain do
                    Expect.isTrue
                        (World.linked (Tuning.keeperMargin Tuning.defaults) world near far)
                        $"{near} -> {far}: the shipped margin closes no crossing of the live errand's chain"
            }

            test "an outpost the raw ring reaches and the masked layer does not leaves the scan set" {
                // `scanOf`'s outpost clause, at the shipped margin. The room
                // is inside the hop budget and every chain to it is joined
                // over raw terrain, so a narrowing that asked the raw layer
                // would admit it, and a reserver would be hired every tick for
                // a room whose price is `None`. The declaration carries no
                // errand, so this case is the outpost clause's alone.
                let colony = declaringOutpost (reactorAsOutpost ())
                let scan = scanUnder Tuning.defaults colony

                Expect.isEmpty
                    scan.Outposts
                    "the outpost clause narrows it away over the masked layer"

                Expect.equal
                    scan.Scanned
                    [ keeperHome ]
                    "so the scan set is the home alone: no outpost room, and no crossing on the way to one"

                // The third call site that reads this margin:
                // `ColonyView.Refused` is built off its own `World.linked
                // (Tuning.keeperMargin tuning)`, so a margin dropped there
                // would leave the room out of the scan set *and* out of the
                // channel that names what was refused.
                Expect.equal
                    (viewUnder [ colony ] (keeperWorld ()) keeperHome).Refused
                    [
                        {
                            RoomName = reactorRoom
                            Kind = DeclarationKind.Outpost
                        }
                    ]
                    "and the view names it out loud, under the kind it was declared as"

                // And the control, which is what says the refusal is the
                // margin's doing and not a broken fixture: the same
                // declaration, the same world, one margin below the shipped
                // six — and it is admitted, with the rooms the chain could
                // cross.
                let admitted = scanUnder reachingTuning colony

                Expect.equal
                    (admitted.Outposts |> List.map (fun outpost -> outpost.RoomName))
                    [ reactorRoom ]
                    "at a margin that closes no crossing the very same declaration is worked"

                Expect.equal
                    (admitted.Scanned |> List.sort)
                    admittedScan
                    "and its room and its transit rooms enter the scan set"
            }

            test "an errand the raw ring reaches and the masked layer does not leaves the scan set" {
                // `scanOf`'s errand clause, the same case one clause over. The
                // live declaration, carrying no outpost, so this case is the
                // errand clause's alone. Unheld: the margin is the subject,
                // not a human's pause.
                let colony = declaringErrand { Errand.w15s25 with Held = false }
                let scan = scanUnder Tuning.defaults colony

                Expect.isEmpty
                    scan.Errands
                    "the errand clause narrows it away over the masked layer"

                Expect.equal
                    scan.Scanned
                    [ keeperHome ]
                    "so nothing of the Reactor's room, and nothing of the way to it, is projected"

                // The loudness is `ColonyView.Refused`'s and the refusing is
                // `scanOf`'s, and the two read the same margin off the same
                // tuning or the room vanishes in silence.
                Expect.equal
                    (viewUnder [ colony ] (keeperWorld ()) keeperHome).Refused
                    [
                        {
                            RoomName = reactorRoom
                            Kind = DeclarationKind.Errand
                        }
                    ]
                    "and the view names it out loud, under the kind it was declared as"

                let admitted = scanUnder reachingTuning colony

                Expect.equal
                    (admitted.Errands |> List.map (fun errand -> errand.RoomName))
                    [ reactorRoom ]
                    "at a margin that closes no crossing the very same declaration is run"

                Expect.equal
                    (admitted.Scanned |> List.sort)
                    admittedScan
                    "and its room and its transit rooms enter the scan set"
            }

            test "a join whose every landing is orphaned is no join, and `linked` now says so" {
                // At `World.linked`'s own altitude, over the border where the
                // mask takes the whole band: W15S26's x = 49 column, which
                // faces W14S26 (`RoomName.offsetOf`, #336). One margin above
                // the shipped (a lair masked at six), the lair at (42,39)
                // masks x = 48 for y = 33..45 and stops one tile short of
                // x = 49, so seven exits survive on the ring with nothing at
                // all behind them; at the shipped margin the lair's five
                // leaves x = 48 standing. Read off the rings alone this answered
                // true, and the flood then priced `None`: the #243/#259 silent
                // failure through a join the scan set had asserted.
                //
                // The far side is the capture's, mask and all. The near side
                // is invented as open as a room can be: nothing on this side
                // may be what closes the band.
                let capture = RoomFixtures.load keeperRoom

                let openRoom: RoomFacts =
                    { RoomFacts.empty with
                        Border =
                            Map.ofList
                                [
                                    for x in 0 .. Seam.exitEdge do
                                        for y in 0 .. Seam.exitEdge do
                                            if
                                                x = 0
                                                || x = Seam.exitEdge
                                                || y = 0
                                                || y = Seam.exitEdge
                                            then
                                                { X = x; Y = y }, Plain
                                ]
                        Layer =
                            { RoomLayer.empty with
                                Terrain =
                                    TerrainGrid.ofList
                                        [
                                            for x in 1 .. Seam.exitEdge - 1 do
                                                for y in 1 .. Seam.exitEdge - 1 ->
                                                    { X = x; Y = y }, Plain
                                        ]
                            }
                    }

                let world =
                    { World.empty with
                        Rooms =
                            Map.ofList
                                [
                                    "W14S26", openRoom
                                    keeperRoom,
                                    { RoomFacts.empty with
                                        Border = capture.Border
                                        Layer =
                                            { RoomLayer.empty with
                                                Terrain = capture.Terrain
                                            }
                                    }
                                ]
                    }

                let margin =
                    Tuning.keeperMargin
                        { Tuning.defaults with
                            ReachHorizon = Tuning.defaults.ReachHorizon + 1
                        }

                Expect.equal
                    (Seam.bandBy
                        (World.ringWalkable margin "W14S26" openRoom.Border)
                        (World.ringWalkable margin keeperRoom capture.Border)
                        (fun _ -> true)
                        "W14S26"
                        keeperRoom
                     |> List.length)
                    7
                    "the premise: the two rings leave seven crossings open at that margin"

                Expect.isFalse
                    (World.linked margin world "W14S26" keeperRoom)
                    "and every one of the seven lands a body where it can never step again, so the rooms are not joined"

                // The other way round is a different question: those same
                // exits are W15S26's to leave, and the room they land in has
                // ground behind its ring. A band is directed, because the
                // ground it asks about is the far room's.
                Expect.isTrue
                    (World.linked margin world keeperRoom "W14S26")
                    "the crossing out of the keeper room is still a crossing: the far side there has ground"
            }
        ]
