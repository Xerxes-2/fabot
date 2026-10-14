/// The rooms a colony declares beyond its own and what its view carries of
/// them: the errand, the ally, the salvage room, the assault room, and the
/// rival's safe mode the assault reads.
module Fabot.Core.Tests.ViewDeclaredRoomTests

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests.ViewFixtures

/// The same pair with the mother **shipping** to the child, and a terminal
/// standing in her room holding both of the stores a send reads (#349).
let private consigning: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home = mother then
            { colony with Consignee = Some child }
        else
            colony)

let private terminalWorld =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.change
                mother
                (Option.map (fun (facts: RoomFacts) ->
                    { facts with
                        Layer =
                            { facts.Layer with
                                TargetPositions =
                                    Map.add
                                        "term-home"
                                        { X = 6; Y = 6 }
                                        facts.Layer.TargetPositions
                            }
                        TargetKinds =
                            Map.add "term-home" (Structure BuiltKind.Terminal) facts.TargetKinds
                        // The two stores a send is priced against travel in
                        // different tables: a fixture that wrote both into one
                        // would agree with a rule reading either, which is the
                        // shape #354's first gate was green against.
                        Stores = Map.add "term-home" 4_000 facts.Stores
                        Thorium = Map.add "term-home" 19_848 facts.Thorium
                        Owners = Map.add "term-home" Ownership.Ours facts.Owners
                        // And a site of the same kind still going up beside it,
                        // carrying W13S28's live arithmetic: 3,836 paid of
                        // 100,000.
                        ConstructionSites =
                            [
                                {
                                    Id = "site-terminal"
                                    Left = 100_000 - 3_836
                                    Begun = true
                                }
                            ]
                        Hits = Map.add "term-home" { Hits = 3000; HitsMax = 3000 } facts.Hits
                    }))
    }

// ---- the errand: a declared room with no controller ------------------------

/// A room two crossings south of the mother, and the room a shortest chain to
/// it crosses. Two hops rather than one deliberately: a one-hop errand would
/// project no transit room at all and so prove nothing about the half of the
/// rule that carries the walk.
let private errandRoom = "W12S30"
let private errandCrossed = "W12S29"

/// The one object the declaration names, and the tile it names it on. An id
/// and a tile and nothing else, which is the whole of an `Errand`: what the
/// object *is* and what it holds are the projection's to answer where there
/// is vision.
let private reactor = "reactor-far"
let private reactorTile = { X = 6; Y = 6 }

/// The mother's declaration with that errand added beside her real outpost.
/// Beside and not instead, so every assertion below is read against a
/// declaration of the other kind standing in the same view: a rule that
/// narrowed *every* room this way would be indistinguishable from one that
/// narrows the errand's.
let private errandDeclared: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home <> mother then
            colony
        else
            { colony with
                Errands =
                    [
                        {
                            RoomName = errandRoom
                            Target = reactor, RoomPos.at errandRoom reactorTile
                            Held = false
                        }
                    ]
            })

/// The errand room as a tick **with vision** reads it: everything a sector
/// centre really carries — its own rocks, a container with a store in it, a
/// site, and the declared target itself standing there under a structure kind
/// and holding Thorium. The room is furnished this richly on purpose: the
/// narrowing under test is what a colony may *carry* of a room it can see, and
/// a blind room keeps the promise trivially (#286's lesson, one room further
/// out).
let private errandSeen =
    let name, facts =
        roomOf
            errandRoom
            Ownership.Unowned
            [
                "src-errand", { X = 3; Y = 3 }, Source
                "can-errand", { X = 4; Y = 3 }, Structure BuiltKind.Container
                "site-errand", { X = 4; Y = 4 }, Site BuiltKind.Extension
                reactor, reactorTile, Structure BuiltKind.Container
                // Ore on that room's floor, and beside it a pile of energy, so
                // the one admitted census entry is tested against the one that
                // stays out.
                "pile-errand", { X = 5; Y = 3 }, Dropped Thorium
                "pile-energy", { X = 5; Y = 4 }, Dropped Energy
                // And the same pair one object over: `Tombstone` names no
                // resource, so which of the two is admitted can only be read
                // off the store.
                "tomb-errand", { X = 5; Y = 5 }, Tombstone
                "tomb-spent", { X = 5; Y = 6 }, Tombstone
            ]
        |> withSources [ "src-errand" ]
        |> withStores [ "can-errand", 1_200; reactor, 40; "tomb-errand", 120; "tomb-spent", 300 ]
        |> withSites [ "site-errand" ]

    name,
    { facts with
        Thorium =
            Map.ofList [ reactor, 400; "can-errand", 90; "pile-errand", 915; "tomb-errand", 175 ]
        Cooldowns = Map.ofList [ reactor, 7; "can-errand", 3 ]
        // Whose the declared target is, which the reclaim's act is gated on.
        // A rival's, because that is the live board: W15S25 is `Odiodin`'s
        // and every Thorium delivered under his flag scores for him.
        Owners = Map.ofList [ reactor, Ownership.Rival; "can-errand", Ownership.Ours ]
    }

/// The pair world with the chain to that room in it, both rooms seen and
/// furnished. The crossing carries a controller and a rock of its own, which
/// is what a transit room's promise is about.
let private errandWorld =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.add
                errandCrossed
                (snd (
                    roomOf
                        errandCrossed
                        Ownership.Unowned
                        [
                            "src-crossing", { X = 5; Y = 5 }, Source
                            "ctrl-crossing", { X = 6; Y = 5 }, Controller
                        ]
                    |> withSources [ "src-crossing" ]
                ))
            |> Map.add (fst errandSeen) (snd errandSeen)
    }

/// The same chain with no vision anywhere on it: terrain and a border ring and
/// nothing else, which is what the shell reads for a declared room it has never
/// had a creep in (`World.factsOf`'s blind branch).
let private blindErrandWorld =
    { pairWorld with
        Rooms = pairWorld.Rooms |> unseen errandCrossed |> unseen errandRoom
    }

/// The courier standing under the flag, carrying the body the season's row
/// actually casts (`Bodies.courierPattern`: twenty Carry, ten Move) and filed
/// into the errand room. The running-dry alarm reads a courier off its shape,
/// so what has to reach the view is that its parts survive the errand
/// narrowing.
let private courierWorld =
    let courier = creep "courier-900-Spawn1" errandRoom

    { errandWorld with
        Creeps =
            errandWorld.Creeps
            @ [
                { courier with
                    Info =
                        { courier.Info with
                            Body = Map.ofList [ Carry, 20; Move, 10 ]
                            Thorium = 500
                        }
                }
            ]
        Rooms =
            errandWorld.Rooms
            |> Map.add
                errandRoom
                (snd (withCreeps [ "courier-900-Spawn1", reactorTile ] errandSeen))
    }

[<Tests>]
let allyTests =
    testList
        "an ally is no hostile"
        [
            test "an ally's creep never reaches the view's hostiles, and a stranger's does" {
                let armed owner : HostileInfo =
                    {
                        Id = $"h-{owner}"
                        Owner = owner
                        Pos = RoomPos.at mother { X = 20; Y = 20 }
                        Body = [ RangedAttack; Heal; Move ]
                        Boosts = []
                        Hits = 300
                        TicksToLive = 1_000
                    }

                let world =
                    { pairWorld with
                        Rooms =
                            pairWorld.Rooms
                            |> Map.change
                                mother
                                (Option.map (fun facts ->
                                    { facts with
                                        Hostiles = [ armed "Odiodin"; armed "Shibdib" ]
                                    }))
                    }

                Expect.isTrue (Set.contains "Odiodin" Colony.allies) "the premise: declared by hand"

                Expect.equal
                    ((viewOf world mother).Hostiles |> List.map (fun hostile -> hostile.Owner))
                    [ "Shibdib" ]
                    "no tower, guard, stand-down or flee rule reads an ally's body; every one reads a stranger's"
            }
        ]

[<Tests>]
let errandTests =
    testList
        "an errand carries the ground, the walk, and the one thing declared in it"
        [
            test
                "a held errand pauses the Reactor's work, keeps its room for the ore, and is refused by nothing" {
                let held =
                    errandDeclared
                    |> List.map (fun colony ->
                        { colony with
                            Errands =
                                colony.Errands
                                |> List.map (fun errand -> { errand with Held = true })
                        })

                let worked = viewUnder errandDeclared errandWorld mother
                let paused = viewUnder held errandWorld mother

                Expect.equal
                    (worked.Errands |> List.map (fun errand -> errand.RoomName))
                    [ errandRoom ]
                    "the premise: unheld, the same declaration is worked"

                Expect.isEmpty
                    paused.Errands
                    "held, no rule sees it: no Reclaim, no courier, no delivery"

                // User, 2026-10-07: the hold pauses the Reactor alone; the
                // room's ore is dug through it, so the room stays projected.
                Expect.isTrue
                    (Map.containsKey errandRoom paused.Spatial.Rooms)
                    "its room stays projected"

                Expect.equal paused.OreRooms (Set.singleton errandRoom) "and is an ore room"

                // A declaration no chain reaches is refused loudly (#243) —
                // unless a human has held it, which is not the same fact.
                let unreachable held =
                    errandDeclared
                    |> List.map (fun colony ->
                        if colony.Home <> mother then
                            colony
                        else
                            { colony with
                                Errands =
                                    [
                                        {
                                            RoomName = "W9N9"
                                            Target =
                                                "far-reactor", RoomPos.at "W9N9" { X = 5; Y = 5 }
                                            Held = held
                                        }
                                    ]
                            })

                Expect.equal
                    (viewUnder (unreachable false) errandWorld mother).Refused
                    [
                        {
                            RoomName = "W9N9"
                            Kind = DeclarationKind.Errand
                        }
                    ]
                    "the premise: unheld, an errand no chain reaches is named"

                Expect.isEmpty
                    (viewUnder (unreachable true) errandWorld mother).Refused
                    "held, it is a pause and not a refusal"
            }

            test
                "an ally's loot never reaches the view: its creep's tombstone, and every drop in a room it reserves" {
                // The ally rule of what we may strike, applied to what we may
                // take: an Odiodin courier dying on the walk leaves its score
                // to Odiodin.
                let crossing tombs piles reserver =
                    let name, facts =
                        roomOf
                            errandCrossed
                            Ownership.Unowned
                            ([ for id in tombs -> id, { X = 7; Y = 7 }, Tombstone ]
                             @ [ for id in piles -> id, { X = 8; Y = 8 }, Dropped Thorium ])

                    name,
                    { facts with
                        Thorium = (tombs @ piles) |> List.map (fun id -> id, 300) |> Map.ofList
                        AllyGraves = Set.singleton "tomb-ally"
                        Control =
                            facts.Control
                            |> Option.map (fun control ->
                                { control with
                                    Reservation =
                                        reserver
                                        |> Option.map (fun who ->
                                            {
                                                Holder = ReservationHolder.Rival
                                                Username = who
                                                TicksToEnd = 3_000
                                            })
                                })
                    }

                let kindsWith room =
                    (viewUnder
                        errandDeclared
                        { errandWorld with
                            Rooms = Map.add (fst room) (snd room) errandWorld.Rooms
                        }
                        mother)
                        .Spatial.TargetKinds

                let open' = kindsWith (crossing [ "tomb-ally"; "tomb-other" ] [ "pile-other" ] None)

                Expect.isFalse
                    (Map.containsKey "tomb-ally" open')
                    "an ally's creep's tombstone: not ours"

                Expect.isTrue
                    (Map.containsKey "tomb-other" open')
                    "anyone else's still is, as before"

                Expect.isTrue (Map.containsKey "pile-other" open') "and the pile beside it"

                let allyHeld =
                    kindsWith (crossing [ "tomb-other" ] [ "pile-other" ] (Some "Odiodin"))

                Expect.isFalse
                    (Map.containsKey "tomb-other" allyHeld || Map.containsKey "pile-other" allyHeld)
                    "in a room an ally reserves, nothing on the floor is ours"
            }

            test
                "an errand room's ore mine and its rocks reach the view, and nothing else of the room does" {
                // The Reactor's room's utrium: the deposit, the
                // neutral extractor on its tile, the container and our site on
                // its Seats. The room's own source and the container beside
                // it stay out, as before.
                let name, facts = errandSeen

                let mined =
                    roomOf
                        name
                        Ownership.Unowned
                        [
                            "min-u", { X = 20; Y = 20 }, Mineral(Ore Utrium)
                            "ext-u", { X = 20; Y = 20 }, Structure BuiltKind.Extractor
                            "can-u", { X = 21; Y = 20 }, Structure BuiltKind.Container
                            "site-u", { X = 20; Y = 21 }, Site BuiltKind.Container
                            "can-far", { X = 23; Y = 20 }, Structure BuiltKind.Container
                        ]
                    |> withSites [ "site-u" ]
                    |> snd

                let furnished =
                    { facts with
                        Layer =
                            { facts.Layer with
                                TargetPositions =
                                    Map.fold
                                        (fun placed id pos -> Map.add id pos placed)
                                        facts.Layer.TargetPositions
                                        mined.Layer.TargetPositions
                            }
                        TargetKinds =
                            Map.fold
                                (fun kinds id kind -> Map.add id kind kinds)
                                facts.TargetKinds
                                mined.TargetKinds
                        ConstructionSites = facts.ConstructionSites @ mined.ConstructionSites
                        Minerals =
                            Map.ofList
                                [
                                    "min-u", Map.ofList [ Ore Utrium, 35_000 ]
                                    "can-u", Map.ofList [ Ore Utrium, 500 ]
                                ]
                        Hits =
                            Map.ofList
                                [
                                    "can-u", { Hits = 200_000; HitsMax = 250_000 }
                                    "can-far", { Hits = 200_000; HitsMax = 250_000 }
                                    "can-errand", { Hits = 200_000; HitsMax = 250_000 }
                                ]
                        Cooldowns = Map.add "ext-u" 2 facts.Cooldowns
                    }

                let view =
                    viewUnder
                        errandDeclared
                        { errandWorld with
                            Rooms = Map.add name furnished errandWorld.Rooms
                        }
                        mother

                let kinds = view.Spatial.TargetKinds

                for id in [ "min-u"; "ext-u"; "can-u"; "site-u"; "src-errand" ] do
                    Expect.isTrue
                        (Map.containsKey id kinds)
                        $"{id}: the mine's, or the rock its builders dig"

                for id in [ "can-far"; "can-errand"; "site-errand" ] do
                    Expect.isFalse (Map.containsKey id kinds) $"{id}: not the mine's"

                Expect.equal
                    (SpatialInfo.heldIn view.Spatial (Ore Utrium) "can-u")
                    500
                    "the container's ore"

                Expect.equal
                    (Map.tryFind "ext-u" view.Spatial.Cooldowns)
                    (Some 2)
                    "the extractor's clock"

                Expect.equal
                    (view.Spatial.Hits
                     |> Map.keys
                     |> List.ofSeq
                     |> List.filter (fun id ->
                         List.contains id [ "can-u"; "can-far"; "can-errand" ]))
                    [ "can-u" ]
                    "the mine's container is repaired like an outpost's, and no other"

                Expect.equal
                    (view.ConstructionSites
                     |> List.map (fun site -> site.Id)
                     |> List.filter (fun id -> id.EndsWith "-u"))
                    [ "site-u" ]
                    "our site on its Seat is built"
            }

            test
                "what a site still owes reaches the view, which is what tells a road from a terminal" {
                // The projection-side counterpart of the backlog term: a
                // projection that dropped what a site owes leaves the row
                // unable to tell a road's 300 from a terminal's 100,000.
                let view = viewUnder consigning terminalWorld mother

                Expect.equal
                    (view.ConstructionSites |> List.map (fun site -> site.Id, site.Left))
                    [ "site-terminal", 96_164; "site-child", siteOwes ]
                    "every site arrives with what it owes, which is `progressTotal - progress` and not either half: the mother's own and the one in the room she raises, summed by the backlog term as one bill"
            }

            test "a terminal's two stores and the consignment it is for both reach the view" {
                // Three facts have to survive `ofWorld` for `planConsignment`
                // to be worth anything: the terminal's kind, its two stores in
                // their two separate tables, and the declaration naming the
                // far end.
                let view = viewUnder consigning terminalWorld mother

                Expect.equal
                    (Map.tryFind "term-home" view.Spatial.TargetKinds)
                    (Some(Structure BuiltKind.Terminal))
                    "the terminal is a target of a kind, which is how the rule finds it at all"

                Expect.equal
                    (SpatialInfo.heldIn view.Spatial Thorium "term-home")
                    19_848
                    "the ore waiting to be shipped, off the Thorium table"

                Expect.equal
                    (SpatialInfo.storedIn view.Spatial "term-home")
                    4_000
                    "and the energy the fee is paid out of, off the store table beside it — the two are never one number (ADR 0057 decision 3)"

                Expect.equal
                    view.Consignee
                    (Some child)
                    "the declaration rides onto the view unnarrowed: there is no scan set for a room three crossings out"

                // And the far end reads none of it. This is what makes the
                // send's blindness deliberate rather than an oversight: the
                // receiving colony cannot see the terminal that is about to
                // ship to it, so no rule of its own can wait for a consignment.
                let far = viewUnder consigning terminalWorld child

                Expect.equal far.Consignee None "the receiving colony declares nothing"

                Expect.isFalse
                    (Map.containsKey "term-home" far.Spatial.TargetKinds)
                    "and cannot see the sender's terminal at all"
            }

            test
                "the courier under the flag reaches the view as a body with parts, and the home it walked from is named" {
                // The projection-side half of the running-dry alarm: three
                // facts, a creep of ours standing in an errand room, its
                // body's parts, and the home room name the lead time is
                // measured from.
                let view = viewUnder errandDeclared courierWorld mother

                let courier =
                    view.Creeps |> List.tryFind (fun creep -> creep.Name = "courier-900-Spawn1")

                Expect.equal
                    (courier |> Option.map (fun creep -> Map.tryFind Carry creep.Body))
                    (Some(Some 20))
                    "the Carry count survives the errand narrowing, which is what the alarm matches a courier by"

                Expect.equal
                    (courier |> Option.map (fun creep -> Map.tryFind Move creep.Body))
                    (Some(Some 10))
                    "and the Move count beside it: a shape match on one part alone would take a hauler for a courier"

                Expect.equal
                    view.Spatial.RoomName
                    (Some mother)
                    "and the home is named, without which `RoomName.hopsBetween` has no origin and the alarm stays silent by design"

                Expect.equal
                    (view.Errands |> List.map (fun errand -> errand.RoomName))
                    [ errandRoom ]
                    "against the errand's own room, which is the other end of that hop count"

                // The draw gate reads the ore afloat off exactly this field:
                // if the narrowing dropped its `Thorium` the gate would read 0
                // afloat and open behind every carrier already walking.
                Expect.equal
                    (courier |> Option.map (fun creep -> creep.Thorium))
                    (Some 500)
                    "and the load aboard it reaches the view, which is what the draw gate subtracts — and, since #367, what the running-dry alarm accepts as an answer instead of the body's mere existence"
            }

            test "the declared target is placed before any body of ours has stood there" {
                // A courier has to hold `Deliver of reactorId` before there
                // is vision, vision needs a creep there, a creep goes there
                // because a Task exists, and the Task exists because the
                // target is in the projection. So the id and the tile are the
                // declaration's and wait for nothing.
                let view = viewUnder errandDeclared blindErrandWorld mother

                Expect.isEmpty view.Refused "the premise: a two-hop errand is not refused"

                Expect.isTrue
                    (Map.containsKey errandRoom view.Spatial.Rooms)
                    "the errand room is projected"

                Expect.isNonEmpty
                    (TerrainGrid.toList (SpatialInfo.layerOf view.Spatial errandRoom).Terrain)
                    "carrying the terrain a walk is floodable over"

                Expect.isNonEmpty
                    (Map.tryFind errandRoom view.Spatial.Borders |> Option.defaultValue Map.empty)
                    "and the border ring a Seam is read off"

                Expect.equal
                    (SpatialInfo.placementOf view.Spatial reactor)
                    (Some(RoomPos.at errandRoom reactorTile))
                    "the declared target stands on the tile the declaration names, with no vision at all"

                Expect.isTrue
                    (Map.containsKey errandCrossed view.Spatial.Rooms)
                    "and the room the chain crosses is in the set for the walk (ADR 0058)"
            }

            test "nothing else in the errand room is work, however much vision answers" {
                // The narrowing that makes an errand less than an outpost:
                // this room has rocks, a stocked container and a site of its
                // own, and the colony may work none of them.
                let view = viewUnder errandDeclared errandWorld mother

                Expect.isFalse
                    (List.contains "src-errand" (idsOf view))
                    "its rock is not pooled, whatever vision answered"

                // The bodies building its ore mine dig the rock
                // where they stand, as a child's pioneers dig hers.
                Expect.contains
                    (view.BorrowedSources |> List.map (fun s -> s.Id))
                    "src-errand"
                    "it is dug only by a body already standing there"

                Expect.isFalse
                    (Map.containsKey "can-errand" view.Spatial.TargetKinds)
                    "its container is classified by nothing, so no Refill or Withdraw is pooled on it"

                Expect.isFalse
                    (Map.containsKey "can-errand" view.Spatial.Stores)
                    "and its store does not ride either"

                Expect.isEmpty
                    (view.ConstructionSites |> List.filter (fun site -> site.Id = "site-errand"))
                    "its site is no Build of hers"

                Expect.isEmpty
                    (Map.tryFind errandRoom view.Spatial.Rooms
                     |> Option.map (fun layer -> layer.TargetPositions)
                     |> Option.defaultValue Map.empty
                     |> Map.filter (fun id _ ->
                         id <> reactor
                         && id <> "pile-errand"
                         && id <> "tomb-errand"
                         && id <> "src-errand"))
                    "four tiles are placed in that room: the declared one's, the ore on its floor (#356), the ore in the tombstone on it (#359) and its rock"
            }

            test "ore on the errand room's floor is the one thing beside the declaration that rides" {
                // #354 widened `Facts.ourThoriumPiles` to reach an errand
                // room's floor and it reached nothing: this narrowing had
                // already taken the pile's kind and amount out, and the unit
                // test agreed because its fixture wrote the pile straight into
                // the projection (#356). The ore is ours by the errand's own
                // argument: nobody owns the room, no other colony walks a body
                // to it, and the pile decays at 1 T a tick. Everything else
                // stays out, so this is one resource on the floor and not a
                // door for `Dropped` things.
                let view = viewUnder errandDeclared errandWorld mother

                Expect.equal
                    (Map.tryFind "pile-errand" view.Spatial.TargetKinds)
                    (Some(Dropped Thorium))
                    "the pile is classified, which is what makes a kind-swept Pickup able to find it"

                Expect.equal
                    (Map.tryFind "pile-errand" view.Spatial.Thorium)
                    (Some 915)
                    "and the amount rides with it, since the threshold is read off it"

                Expect.contains
                    (SpatialInfo.idsOfKind view.Spatial (Dropped Thorium))
                    "pile-errand"
                    "and the kind census answers it, which is the sweep `Facts.ourThoriumPiles` runs before it filters by room — the reach #354 claimed and did not have"

                Expect.isFalse
                    (Map.containsKey "pile-energy" view.Spatial.TargetKinds)
                    "a pile of energy out there is nobody's errand: the filter is one resource, not a kind of object"
            }

            test "and the ore in a tombstone on that floor rides on the same argument" {
                // #356 one object over: a courier that dies loaded leaves its
                // ore in its tombstone, and W15S25 had 175 T standing in one
                // at (43,6). The clock is shorter: a tombstone drops its whole
                // store as piles when it decays
                // (`processor/intents/tombstones/tick.js`), so what the pile
                // case catches is this ore later and smaller. The projection
                // half of a pair whose rules are pinned in `PoolWithdrawTests`,
                // `ErrandTests` and `ObserveTests` against hand-written views.
                let view = viewUnder errandDeclared errandWorld mother

                Expect.equal
                    (Map.tryFind "tomb-errand" view.Spatial.TargetKinds)
                    (Some Tombstone)
                    "the tombstone is classified, which is what makes a kind-swept Withdraw able to find it"

                Expect.equal
                    (Map.tryFind "tomb-errand" view.Spatial.Thorium)
                    (Some 175)
                    "and its ore rides beside the kind: the Withdraw's capacity is counted off this map"

                Expect.equal
                    (SpatialInfo.placementOf view.Spatial "tomb-errand")
                    (Some(RoomPos.at errandRoom { X = 5; Y = 5 }))
                    "and its tile, without which no body can be priced to it"

                Expect.contains
                    (SpatialInfo.idsOfKind view.Spatial Tombstone)
                    "tomb-errand"
                    "the kind census answers it, which is the sweep `Facts.ourThoriumTombstones` runs before it filters by room"

                Expect.isFalse
                    (Map.containsKey "tomb-spent" view.Spatial.TargetKinds)
                    "a tombstone holding energy alone is nobody's errand: the admission is read off the ore and not off the object"

                // What the tombstone is admitted for is its ore: `Stores` is
                // what an energy Withdraw's own filter reads, so leaving it
                // out is what keeps that Task unpooled three crossings from
                // home.
                Expect.equal
                    (Map.tryFind "tomb-errand" view.Spatial.Stores)
                    None
                    "the 120 energy in it does not ride: one resource is the errand's, and it is not that one"

                Expect.equal
                    (Map.tryFind reactor view.Spatial.Stores)
                    (Some 40)
                    "while the declared target's own store is untouched by that cut"
            }

            test "and the one target it names is: its store rides, its kind does not" {
                // The changing half of the declared object — its store, its
                // Thorium — is vision-paid and absent entry by entry where
                // there is none. What does *not* ride is the kind: every pool
                // is built by sweeping `TargetKinds`, so an id classified by
                // nothing is priceable by a Task that names it and enumerable
                // by no pool at all.
                let seen = viewUnder errandDeclared errandWorld mother
                let blind = viewUnder errandDeclared blindErrandWorld mother

                Expect.equal
                    (Map.tryFind reactor seen.Spatial.Stores)
                    (Some 40)
                    "what the declared target holds is read where there is vision"

                Expect.equal
                    (Map.tryFind reactor seen.Spatial.Thorium)
                    (Some 400)
                    "its Thorium beside it, which is the column the programme is scored out of"

                Expect.isNone
                    (Map.tryFind reactor blind.Spatial.Stores)
                    "and absent entry by entry the tick the relay gaps, never zero (ADR 0004)"

                Expect.equal
                    (SpatialInfo.placementOf blind.Spatial reactor)
                    (SpatialInfo.placementOf seen.Spatial reactor)
                    "while the tile is the declaration's either way"

                Expect.isNone
                    (Map.tryFind reactor seen.Spatial.TargetKinds)
                    "the kind vision gave it is dropped: no pool that sweeps a kind can name it"

                Expect.isNone
                    (Map.tryFind reactor seen.Spatial.Hits)
                    "and its hit count with it, a Repair being pooled off one"

                // The owner is the third changing entry: the act that takes
                // the flag back is gated on it, and it rides for the declared
                // id and for nothing else in that room.
                Expect.equal
                    (Map.tryFind reactor seen.Spatial.Owners)
                    (Some Ownership.Rival)
                    "whose the declared target is, read where there is vision"

                Expect.isNone
                    (Map.tryFind "can-errand" seen.Spatial.Owners)
                    "and nothing else of that room's is carried, ownership included"

                Expect.isNone
                    (Map.tryFind reactor blind.Spatial.Owners)
                    "absent the tick the relay gaps, which the act reads as *not ours*"
            }

            test "the errand is projected by the colony that declares it and by no other" {
                // The room the live errand names is five and six crossings
                // from the other two homes, so a price into it from either is
                // `None`. The rule is not "the near colony gets it": a room's
                // name in one colony's list is what makes it that colony's.
                let hers = viewUnder errandDeclared errandWorld mother
                let his = viewUnder errandDeclared errandWorld child

                Expect.isTrue
                    (Map.containsKey errandRoom hers.Spatial.Rooms)
                    "the premise: the declaring colony projects it"

                Expect.isFalse
                    (Map.containsKey errandRoom his.Spatial.Rooms)
                    "the colony that declares no errand projects the room of nobody's"

                Expect.isFalse
                    (Map.containsKey errandCrossed his.Spatial.Rooms)
                    "nor one room of the way there"

                Expect.isFalse
                    (List.contains "src-errand" (idsOf his))
                    "and pools nothing that stands in it"

                // And the list itself, which the Task pool and the re-claimer's
                // seat are read off.
                Expect.equal
                    (hers.Errands |> List.map (fun errand -> errand.RoomName))
                    [ errandRoom ]
                    "the declaring colony carries the errand on its view"

                Expect.isEmpty his.Errands "and the colony that declares none carries none"
            }

            test "a stand-down withholds its target-room errand, not a route crossing" {
                let viewWith shut =
                    let colony = errandDeclared |> List.find (fun colony -> colony.Home = mother)

                    let gate = { StandDown.none with Shut = shut }

                    let holders =
                        World.creepColonies
                            Tuning.defaults
                            errandDeclared
                            (World.living errandDeclared errandWorld)
                            (Map.ofList [ mother, shut ])
                            errandWorld

                    ColonyView.ofWorld
                        Tuning.defaults
                        errandDeclared
                        gate
                        holders
                        errandWorld
                        colony

                let withheld = viewWith (Set.singleton errandRoom)

                Expect.isEmpty
                    withheld.Errands
                    "the target-room gate withholds the declaration as a unit"

                Expect.isFalse
                    (Map.containsKey errandRoom withheld.Spatial.Rooms)
                    "so its target room leaves the projection"

                Expect.isNone
                    (SpatialInfo.placementOf withheld.Spatial reactor)
                    "and the declared target is placed nowhere"

                let transitShut = viewWith (Set.singleton errandCrossed)

                Expect.equal
                    (transitShut.Errands |> List.map (fun errand -> errand.RoomName))
                    [ errandRoom ]
                    "the stand-down withdraws work in its room; it is no lock on a route crossing it (#325)"

                Expect.isTrue
                    (Map.containsKey errandRoom transitShut.Spatial.Rooms)
                    "so the same unchanged declaration remains projected"
            }

            // The one carve-out from the line above: a stronghold's four
            // towers under million-hit ramparts reach every tile, so the loss
            // is the walk rather than the withheld work. Live, a `bunker4` in
            // W15S26 killed two 650-energy re-claimers on the same entry tile
            // 161 ticks apart while the gate had that room correctly shut
            // (#382).
            test
                "a stronghold on the only route withholds the errand, where an ordinary stand-down does not" {
                let viewWith gate =
                    let colony = errandDeclared |> List.find (fun colony -> colony.Home = mother)

                    let holders =
                        World.creepColonies
                            Tuning.defaults
                            errandDeclared
                            (World.living errandDeclared errandWorld)
                            (Map.ofList [ mother, gate.Shut ])
                            errandWorld

                    ColonyView.ofWorld
                        Tuning.defaults
                        errandDeclared
                        gate
                        holders
                        errandWorld
                        colony

                let crossing = Set.singleton errandCrossed

                let ordinary = viewWith { StandDown.none with Shut = crossing }

                Expect.equal
                    (ordinary.Errands |> List.map (fun errand -> errand.RoomName))
                    [ errandRoom ]
                    "the premise, and ADR 0066: an ordinary stand-down on a crossing is no route lock"

                let bunkered =
                    viewWith
                        { StandDown.none with
                            Shut = crossing
                            Impassable = crossing
                        }

                Expect.isEmpty
                    bunkered.Errands
                    "a stronghold on the way takes the declaration as a unit: the walk is what it costs"

                Expect.isFalse
                    (Map.containsKey errandRoom bunkered.Spatial.Rooms)
                    "so its target room leaves the projection and nothing is pooled three rooms out"

                Expect.contains
                    (bunkered.Refused |> List.map (fun refusal -> refusal.RoomName))
                    errandRoom
                    "and the refusal is named, or a declaration would vanish with nothing said about why"

                Expect.isTrue
                    (Set.contains errandCrossed bunkered.Spatial.RivalRooms
                     && not (Set.contains errandCrossed ordinary.Spatial.RivalRooms))
                    "and the bunker's room is entered by no chain, a fighter's included: its towers reach every tile (live t980,650)"
            }

            test "an errand no chain reaches leaves the scan set and is named, with its kind" {
                // The refusal says *which kind* it refused, because "W12S30"
                // under a heading that reads "declared outposts" is a second
                // silence wearing the first one's clothes.
                let walledIn =
                    { errandWorld with
                        Rooms =
                            errandWorld.Rooms
                            |> Map.map (fun name facts ->
                                if name = mother || name = outpost || name = child then
                                    facts
                                else
                                    { facts with Border = Map.empty })
                    }

                let view = viewUnder errandDeclared walledIn mother

                Expect.equal
                    view.Refused
                    [
                        {
                            RoomName = errandRoom
                            Kind = DeclarationKind.Errand
                        }
                    ]
                    "the errand is named on the layout record as the errand it was declared as"

                Expect.isFalse
                    (Map.containsKey errandRoom view.Spatial.Rooms)
                    "and it is out of the scan set, so nothing is projected for it"

                Expect.isFalse
                    (Map.containsKey errandCrossed view.Spatial.Rooms)
                    "nor is the room a chain to it would have crossed"

                Expect.isNone
                    (SpatialInfo.placementOf view.Spatial reactor)
                    "and the declared target is placed nowhere at all"

                Expect.isEmpty
                    view.Errands
                    "and it is out of the errand list, so no Reclaim is pooled and no body hired (#318)"

                // Pairwise against the same declaration over an unwalled
                // world: what refuses the room is the terrain and not the
                // declaration's shape.
                Expect.isEmpty
                    (viewUnder errandDeclared errandWorld mother).Refused
                    "an errand a chain reaches refuses nothing, and says so"
            }

            test "a refused outpost and a refused errand ride together, each under its own kind" {
                // The whole reason the kind is on the record: the two
                // declarations are moved in two different lists, and a reader
                // told only the room name has to guess which. Both refused in
                // one view, so the encoder cannot be answering with a constant.
                let bothWrong =
                    overreaching
                    |> List.map (fun colony ->
                        if colony.Home <> mother then
                            colony
                        else
                            { colony with
                                Errands =
                                    [
                                        {
                                            RoomName = "W12S34"
                                            Target = reactor, RoomPos.at "W12S34" reactorTile
                                            Held = false
                                        }
                                    ]
                            })

                Expect.equal
                    (viewUnder bothWrong overreachingWorld mother).Refused
                    [
                        {
                            RoomName = tooFar
                            Kind = DeclarationKind.Outpost
                        }
                        {
                            RoomName = "W12S34"
                            Kind = DeclarationKind.Errand
                        }
                    ]
                    "the outposts a human wrote first, then the errands, each said as what it is"
            }

            test "the room a chain to an errand crosses is a transit room and nothing more" {
                // An errand room is more than a transit room; the rooms on the
                // way to it are not. A rule that narrowed the whole chain the
                // errand's way would pool a crossing's controller for a colony
                // that declared nothing there.
                let view = viewUnder errandDeclared errandWorld mother

                Expect.isTrue
                    (Map.containsKey errandCrossed view.Spatial.Rooms)
                    "the crossing is projected, which is what the chain is priced over"

                Expect.isNonEmpty
                    (TerrainGrid.toList (SpatialInfo.layerOf view.Spatial errandCrossed).Terrain)
                    "carrying its ground"

                Expect.isFalse
                    (Map.containsKey "ctrl-crossing" view.Spatial.TargetKinds)
                    "and not its controller, which no declaration of hers names"

                Expect.isFalse (List.contains "src-crossing" (idsOf view)) "nor its rock"
            }

            test "every errand a human has declared is inside the hop budget" {
                // Red here rather than live, because a declaration past the
                // budget is accepted by every rule downstream and worked by
                // none of them. The other half — whether the terrain leaves a
                // chain — needs the captures (`RoomOutpostTests`).
                Expect.isNonEmpty
                    (Colony.declared |> List.collect (fun colony -> colony.Errands))
                    "a declaration nobody made is nothing to check"

                let refused =
                    Colony.declared
                    |> List.collect (fun colony ->
                        colony.Errands
                        |> List.filter (
                            Errand.withinHopBudget Tuning.defaults.MaxHops colony.Home >> not
                        )
                        |> List.map (fun errand ->
                            $"{errand.RoomName} is out of {colony.Home}'s reach"))

                Expect.isEmpty
                    refused
                    $"""every declared errand is inside the hop budget: {String.concat "; " refused}"""
            }

            test "every declared errand names a tile of its own room" {
                // A tile filed under another room's name is dropped rather
                // than written onto this room's coordinate, which would place
                // the target nowhere, price it at 0 and *win* its tier.
                // Dropped, the errand has no target at all, the quieter
                // failure and one only this line catches.
                for colony in Colony.declared do
                    for errand in colony.Errands do
                        let _, tile = errand.Target

                        Expect.equal
                            tile.Room
                            errand.RoomName
                            $"{colony.Home}: the errand's target is a tile of {errand.RoomName}"
            }

            test "no room is declared as both an outpost and an errand" {
                // The invariant `ColonyView.ofWorld`'s branch order rests on:
                // the chain is `bootstrap → transit → errand → worked`, so a
                // room in both lists takes the errand branch and is narrowed
                // where the outpost wanted it widened, while the reserver row
                // goes on hiring for a room whose haul chain has silently gone
                // and nothing on `Refused` says so. The two kinds are disjoint
                // by definition — `Outpost.Controller` is mandatory and an
                // errand exists for the room with none — so a room in both is
                // a human writing a contradiction, caught red before deploy.
                for colony in Colony.declared do
                    let outposts = colony.Outposts |> List.map (fun o -> o.RoomName) |> Set.ofList
                    let errands = colony.Errands |> List.map (fun e -> e.RoomName) |> Set.ofList

                    Expect.isEmpty
                        (Set.intersect outposts errands |> Set.toList)
                        $"{colony.Home}: a room declared as both would be narrowed to the errand's one target and mined by nobody"

                // And across colonies: the room would be widened by its
                // declaring colony and narrowed by the other, and the two
                // projections would disagree about what is in it.
                let allOutposts =
                    Colony.declared
                    |> List.collect (fun colony ->
                        colony.Outposts |> List.map (fun o -> o.RoomName))
                    |> Set.ofList

                let allErrands =
                    Colony.declared
                    |> List.collect (fun colony -> colony.Errands |> List.map (fun e -> e.RoomName))
                    |> Set.ofList

                Expect.isEmpty
                    (Set.intersect allOutposts allErrands |> Set.toList)
                    "no room is any colony's outpost and any colony's errand at once"
            }
        ]

// ---- the salvage room ------------------------------------------------------

/// A room given up two crossings south of the mother, by the room the errand
/// case above crosses. Two hops for that case's reason: a one-hop room would
/// project no crossing and prove nothing about the walk.
let private salvageRoom = "W12S30"

/// The mother's declaration with that room to salvage beside her real
/// outpost, for `errandDeclared`'s reason.
let private salvageDeclared: Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home <> mother then
            colony
        else
            { colony with
                Salvage = [ salvageRoom ]
            })

/// The ownable structures still standing in it, each on its own tile but the
/// rampart, which stands over the spawn as the Keep's do.
let private ownedLeft =
    [
        "spawn-old", { X = 3; Y = 3 }, BuiltKind.Spawn
        "ext-old", { X = 4; Y = 3 }, BuiltKind.Extension
        "tower-old", { X = 5; Y = 3 }, BuiltKind.Tower
        "sto-old", { X = 6; Y = 3 }, BuiltKind.Storage
        "term-old", { X = 7; Y = 3 }, BuiltKind.Terminal
        "extr-old", { X = 8; Y = 3 }, BuiltKind.Extractor
        "link-old", { X = 9; Y = 3 }, BuiltKind.Link
        "ramp-old", { X = 3; Y = 3 }, BuiltKind.Rampart
    ]

/// The room as vision reads it the tick after its controller was let go: our
/// structures standing, with the hits and stores the shell files for them,
/// beside the kinds nobody owns — a road, a container, a rock and the
/// controller itself.
let private salvageSeen =
    let name, facts =
        roomOf
            salvageRoom
            Ownership.Unowned
            ((ownedLeft |> List.map (fun (id, tile, kind) -> id, tile, Structure kind))
             @ [
                 "road-old", { X = 3; Y = 5 }, Structure BuiltKind.Road
                 "can-old", { X = 4; Y = 5 }, Structure BuiltKind.Container
                 "src-old", { X = 6; Y = 6 }, Source
                 "ctrl-old", { X = 8; Y = 8 }, Controller
             ])
        |> withSources [ "src-old" ]
        |> withStores [ "sto-old", 5_000; "term-old", 4; "can-old", 800; "spawn-old", 300 ]

    name,
    { facts with
        Hits =
            facts.Hits
            |> Map.add "ramp-old" { Hits = 25_000; HitsMax = 300_000 }
            |> Map.add "road-old" { Hits = 2_000; HitsMax = 5_000 }
            |> Map.add "tower-old" { Hits = 1_000; HitsMax = 3_000 }
    }

/// The pair world with the chain to that room in it, both rooms seen.
let private salvageWorld =
    { pairWorld with
        Rooms =
            pairWorld.Rooms
            |> Map.add
                errandCrossed
                (snd (
                    roomOf
                        errandCrossed
                        Ownership.Unowned
                        [ "ctrl-crossing", { X = 6; Y = 5 }, Controller ]
                ))
            |> Map.add (fst salvageSeen) (snd salvageSeen)
    }

/// The same chain gone dark: the last structure has fallen.
let private darkSalvageWorld =
    { pairWorld with
        Rooms = pairWorld.Rooms |> unseen errandCrossed |> unseen salvageRoom
    }

/// The id a Task names, and None for the three that name none.
let private namedBy task =
    match task with
    | Harvest id
    | Build id
    | Repair id
    | Upgrade id
    | Reserve id
    | Claim id
    | Reclaim id
    | Dismantle id -> Some id
    | Pickup(id, _)
    | Withdraw(id, _)
    | Refill(id, _) -> Some id
    | Guard _
    | Defend _
    | Assault _
    | Operate _
    | Flee -> None

[<Tests>]
let salvageTests =
    testList
        "a salvage room carries the ground, the walk, and the tiles of what is to come down"
        [
            test
                "a seen salvage room lists its ownable structures to dismantle, and classifies none of them" {
                let view = viewUnder salvageDeclared salvageWorld mother
                let targets = ownedLeft |> List.map (fun (id, _, _) -> id)

                Expect.equal
                    (List.sort view.Dismantles)
                    (List.sort targets)
                    "every kind a player owns, the rampart and the extractor among them"

                for id, tile, _ in ownedLeft do
                    Expect.equal
                        (SpatialInfo.placementOf view.Spatial id)
                        (Some(RoomPos.at salvageRoom tile))
                        $"{id} is placed, so a Task naming it can be walked to and acted on"

                    Expect.isFalse
                        (Map.containsKey id view.Spatial.TargetKinds)
                        $"{id} is classified by nothing, so no pool that sweeps a kind names it"

                    Expect.isFalse
                        (Map.containsKey id view.Spatial.Hits)
                        $"{id} carries no hits: no Repair, and no raid charged for our own dismantling"

                    Expect.isFalse
                        (Map.containsKey id view.Spatial.Stores)
                        $"{id} carries no store: nothing is drawn from or poured into it"

                for id in [ "road-old"; "can-old"; "src-old"; "ctrl-old" ] do
                    Expect.isNone
                        (SpatialInfo.placementOf view.Spatial id)
                        $"{id} is nobody's to take down and the room's to keep: not in the view at all"
            }

            test "nothing in a salvage room is pooled but a Dismantle of each target" {
                let view = viewUnder salvageDeclared salvageWorld mother

                let room =
                    (World.roomOf salvageWorld salvageRoom).TargetKinds |> Map.keys |> Set.ofSeq

                let pooledThere =
                    planTasks
                        view
                        (Fabot.Core.Atlas.ofView view)
                        (noThreats ())
                        HeldTaskFacts.empty
                        (outpostFactsOf view (Fabot.Core.Atlas.ofView view))
                    |> List.filter (fun task ->
                        namedBy task |> Option.exists (fun id -> Set.contains id room))

                Expect.equal
                    (pooledThere |> List.sortBy string)
                    (ownedLeft |> List.map (fun (id, _, _) -> Dismantle id) |> List.sortBy string)
                    "one Dismantle each, and no Repair, Refill, Withdraw or Harvest out there"
            }

            test "a salvage room anybody else holds lists nothing to dismantle" {
                // The tick a claim or a reservation lands, and not a tick later
                // by way of the stand-down's latch: a structure in a room
                // another player holds is theirs to lose, and an ally's too.
                let heldAs (held: RoomControlInfo) =
                    { salvageWorld with
                        Rooms =
                            salvageWorld.Rooms
                            |> Map.change
                                salvageRoom
                                (Option.map (fun facts -> { facts with Control = Some held }))
                    }

                let reservedBy holder =
                    { control Ownership.Unowned with
                        Reservation =
                            Some
                                {
                                    Holder = holder
                                    TicksToEnd = 4_000
                                    Username = "rival"
                                }
                    }

                for label, held in
                    [
                        "claimed by another player", control Ownership.Rival
                        "reserved by another player", reservedBy ReservationHolder.Rival
                        "reserved by the Invader", reservedBy ReservationHolder.Invader
                        "claimed back by us", control Ownership.Ours
                    ] do
                    let view = viewUnder salvageDeclared (heldAs held) mother

                    Expect.isEmpty view.Dismantles $"{label}: nothing to take down"

                    for id, _, _ in ownedLeft do
                        Expect.isNone
                            (SpatialInfo.placementOf view.Spatial id)
                            $"{label}: {id} is not placed for a Task to name"

                let ours =
                    viewUnder salvageDeclared (heldAs (reservedBy ReservationHolder.Ours)) mother

                Expect.equal
                    (List.length ours.Dismantles)
                    (List.length ownedLeft)
                    "our own reservation leaves the room ours to take down"
            }

            test "a dark salvage room is projected with nothing to dismantle" {
                // The last structure fell and the room's vision went with it:
                // the declaration is inert until a human takes it out.
                let view = viewUnder salvageDeclared darkSalvageWorld mother

                Expect.isEmpty view.Dismantles "nothing seen standing, nothing to take down"

                Expect.isTrue
                    (Map.containsKey salvageRoom view.Spatial.Rooms)
                    "the room is still projected for its ground"

                Expect.isEmpty
                    view.Refused
                    "and a room a chain reaches is not refused for being dark"
            }

            test "a salvage room no chain reaches is refused under its own kind" {
                let unreachable =
                    salvageDeclared
                    |> List.map (fun colony ->
                        if colony.Home <> mother then
                            colony
                        else
                            { colony with Salvage = [ "W9N9" ] })

                let view = viewUnder unreachable salvageWorld mother

                Expect.equal
                    view.Refused
                    [
                        {
                            RoomName = "W9N9"
                            Kind = DeclarationKind.Salvage
                        }
                    ]
                    "named on the layout record as the kind a human declared it as"

                Expect.isEmpty
                    view.Dismantles
                    "and nothing is dismantled in a room nothing walks to"

                Expect.isFalse (Map.containsKey "W9N9" view.Spatial.Rooms) "nor is it projected"
            }

            test "no salvage room is declared as a home, an outpost or an errand" {
                // The branch order in `ColonyView.ofWorld` reads an errand room
                // before a salvage one, and a home or an outpost is never
                // narrowed at all: a room in two lists would be salvaged by
                // nobody, or salvaged where a human meant it kept.
                let salvage =
                    Colony.declared |> List.collect (fun colony -> colony.Salvage) |> Set.ofList

                let kept =
                    Colony.declared
                    |> List.collect (fun colony ->
                        colony.Home :: (colony.Outposts |> List.map (fun o -> o.RoomName))
                        @ (colony.Errands |> List.map (fun e -> e.RoomName)))
                    |> Set.ofList

                Expect.isEmpty
                    (Set.intersect salvage kept |> Set.toList)
                    "no room is taken down by one list and kept by another"
            }
        ]

/// One of Trepidimous' rooms as vision showed it at tick 1,000, able to
/// activate: stock banked, no clock running, its downgrade timer full.
let private trepController level : RivalController =
    {
        Owner = "Trepidimous"
        Level = level
        SafeModeUntil = 0
        SafeModeCooldownUntil = 0
        SafeModeAvailable = 3
        UpgradeBlockedUntil = 0
        TicksToDowngrade = Engine.controllerDowngrade level
        Seen = 1_000
    }

let private trepRooms =
    Map.ofList [ "W17S24", trepController 4; "W18S26", trepController 6 ]

/// `RivalSafeMode.canActivate` for W17S24 over Trepidimous' rooms with one of
/// them changed, at `now`.
let private gateAt now room (change: RivalController -> RivalController) =
    let known = trepRooms |> Map.change room (Option.map change)
    RivalSafeMode.canActivate Tuning.defaults.RivalIntelTicks known "Trepidimous" "W17S24" now

[<Tests>]
let rivalSafeModeTests =
    testList
        "a rival controller's safe mode"
        [
            test "W18S26 in safe mode: W17S24 cannot activate until it ends (#489)" {
                let provoked (c: RivalController) = { c with SafeModeUntil = 1_200 }

                Expect.equal
                    (gateAt 1_100 "W18S26" provoked)
                    (SafeModeActivation.Cannot(SafeModeRefusal.Busy "W18S26"))
                    "one safe mode per player: ERR_BUSY"

                Expect.equal
                    (gateAt 1_200 "W18S26" provoked)
                    SafeModeActivation.Can
                    "open the tick it ends"

                Expect.equal
                    (gateAt 1_100 "W18S26" id)
                    SafeModeActivation.Can
                    "and open while none runs"
            }

            test
                "a cooldown, an empty stock, an upgrade block, the downgrade line each refuse it (#489)" {
                let refusal change = gateAt 1_100 "W17S24" change

                Expect.equal
                    (refusal (fun c -> { c with SafeModeCooldownUntil = 1_101 }))
                    (SafeModeActivation.Cannot SafeModeRefusal.Cooldown)
                    "cooldown"

                Expect.equal
                    (refusal (fun c -> { c with SafeModeAvailable = 0 }))
                    (SafeModeActivation.Cannot SafeModeRefusal.NoStock)
                    "zero stock"

                Expect.equal
                    (refusal (fun c -> { c with UpgradeBlockedUntil = 1_101 }))
                    (SafeModeActivation.Cannot SafeModeRefusal.UpgradeBlocked)
                    "a tap's upgrade block"

                // RCL4: 40,000 / 2 - 5,000.
                Expect.equal
                    (refusal (fun c -> { c with TicksToDowngrade = 14_999 }))
                    (SafeModeActivation.Cannot SafeModeRefusal.Downgrading)
                    "below the downgrade line"

                Expect.equal
                    (refusal (fun c -> { c with TicksToDowngrade = 15_000 }))
                    SafeModeActivation.Can
                    "on it"
            }

            test "a stale sighting reads Unknown, but a clock it saw still refuses (#489)" {
                let stale = 1_000 + Tuning.defaults.RivalIntelTicks

                Expect.equal
                    (gateAt stale "W17S24" id)
                    SafeModeActivation.Unknown
                    "stock may have come back"

                Expect.equal
                    (gateAt stale "W17S24" (fun c ->
                        { c with
                            SafeModeCooldownUntil = stale + 1
                        }))
                    (SafeModeActivation.Cannot SafeModeRefusal.Cooldown)
                    "a cooldown never ends early"

                Expect.equal
                    (RivalSafeMode.canActivate
                        Tuning.defaults.RivalIntelTicks
                        trepRooms
                        "Trepidimous"
                        "W22S28"
                        1_100)
                    SafeModeActivation.Unknown
                    "a room never seen"
            }

            test
                "the window: the tick every refusal it saw has lifted, None while it may activate (#491)" {
                let barred now room change =
                    let known = trepRooms |> Map.change room (Option.map change)

                    RivalSafeMode.barredUntil
                        Tuning.defaults.RivalIntelTicks
                        known
                        "Trepidimous"
                        "W17S24"
                        now

                Expect.equal
                    (barred 1_100 "W18S26" (fun c -> { c with SafeModeUntil = 21_000 }))
                    (Some 21_000)
                    "another room's safe mode: until it ends"

                Expect.equal
                    (barred 1_100 "W17S24" (fun c ->
                        { c with
                            SafeModeCooldownUntil = 5_000
                            UpgradeBlockedUntil = 2_000
                        }))
                    (Some 5_000)
                    "the last of the clocks running"

                Expect.equal
                    (barred 1_100 "W17S24" (fun c -> { c with SafeModeAvailable = 0 }))
                    (Some(1_000 + Tuning.defaults.RivalIntelTicks))
                    "no stock: while the sighting is trusted"

                Expect.equal (barred 1_100 "W17S24" id) None "it can activate"

                Expect.equal
                    (barred (1_000 + Tuning.defaults.RivalIntelTicks) "W17S24" id)
                    None
                    "stale: Unknown is no window"

                for now, room, change in
                    [
                        1_100,
                        "W18S26",
                        (fun (c: RivalController) -> { c with SafeModeUntil = 1_200 })
                        1_200, "W18S26", (fun c -> { c with SafeModeUntil = 1_200 })
                        1_100, "W17S24", (fun c -> { c with SafeModeAvailable = 0 })
                        2_600, "W17S24", (fun c -> { c with SafeModeAvailable = 0 })
                    ] do
                    let known = trepRooms |> Map.change room (Option.map change)

                    let cannot =
                        match
                            RivalSafeMode.canActivate
                                Tuning.defaults.RivalIntelTicks
                                known
                                "Trepidimous"
                                "W17S24"
                                now
                        with
                        | SafeModeActivation.Cannot _ -> true
                        | _ -> false

                    Expect.equal
                        (Option.isSome (barred now room change))
                        cannot
                        $"a window exactly while canActivate says Cannot, at {now}"
            }

            test
                "a rival controller is remembered while dark and forgotten once seen unowned (#489)" {
                let facts controller =
                    { RoomFacts.empty with
                        Control =
                            Some
                                {
                                    Owner =
                                        if Option.isSome controller then
                                            Ownership.Rival
                                        else
                                            Ownership.Unowned
                                    Reservation = None
                                    SafeMode = false
                                    SafeModeCooldownUntil = 0
                                    Sign = None
                                }
                        RivalController = controller
                    }

                let worldAt time rooms =
                    { World.empty with
                        Time = time
                        Rooms = Map.ofList rooms
                    }

                let fresh = { trepController 6 with Seen = 1_100 }

                let seen =
                    World.recallRivalControllers
                        trepRooms
                        (worldAt 1_100 [ "W18S26", facts (Some fresh); "W17S24", RoomFacts.empty ])

                Expect.equal
                    seen.RivalControllers
                    (Map.ofList [ "W17S24", trepController 4; "W18S26", fresh ])
                    "the seen room restamped, the dark one kept"

                Expect.equal
                    (World.recallRivalControllers
                        seen.RivalControllers
                        (worldAt 1_200 [ "W18S26", facts None ]))
                        .RivalControllers
                    (Map.ofList [ "W17S24", trepController 4 ])
                    "a room seen with nobody's controller is dropped"
            }
        ]

// ---- the assault room -------------------------------------------------------

/// A rival's room two crossings south of the mother, where the salvage room
/// stands, for the salvage case's reason.
let private assaultRoom = salvageRoom

/// The mother's declaration with that room assaulted, its breach at (10,20),
/// naming these targets past it.
let private assaultNaming (targets: Pos list) (mode: AssaultMode) (active: bool) : Colony list =
    declared
    |> List.map (fun colony ->
        if colony.Home <> mother then
            colony
        else
            { colony with
                Assaults =
                    [
                        {
                            RoomName = assaultRoom
                            Enemy = "Trepidimous"
                            Breach = [ { X = 10; Y = 20 } ]
                            Squad = Assault.breachers
                            Mode = mode
                            Targets = targets
                            Entry = None
                            MaxHops = None
                            Active = active
                        }
                    ]
            })

let private assaultDeclared = assaultNaming []

/// The rival's link inside the line, under a rampart of its own.
let private linkTile = { X = 12; Y = 24 }

/// The room as vision reads it: the rival's line, a tower and a spawn.
let private assaultWorld =
    { salvageWorld with
        Rooms =
            salvageWorld.Rooms
            |> Map.add
                assaultRoom
                (snd (
                    roomOf
                        assaultRoom
                        Ownership.Rival
                        [
                            "ramp-breach", { X = 10; Y = 20 }, Structure BuiltKind.Rampart
                            "ramp-line", { X = 10; Y = 21 }, Structure BuiltKind.Rampart
                            "tower-rival", { X = 30; Y = 30 }, Structure BuiltKind.Tower
                            "spawn-rival", { X = 31; Y = 31 }, Structure BuiltKind.Spawn
                            "link-rival", linkTile, Structure BuiltKind.Link
                            "ramp-link", linkTile, Structure BuiltKind.Rampart
                        ]
                ))
    }

[<Tests>]
let assaultViewTests =
    testList
        "an assault room carries the ground, the rival's line as walls, and what the sappers take down"
        [
            test
                "no declared assault is switched on: the link raid waits out Dread358 (2026-10-04); the probe stays off (#493)" {
                let worked =
                    Colony.declared
                    |> List.collect (fun colony ->
                        Assault.worked colony.Assaults
                        |> List.map (fun assault -> colony.Home, assault))

                Expect.equal
                    worked
                    []
                    "the user paused the link raid until its defender dies; the probe answered and stays off"

                Expect.equal
                    (Assault.w18s26.RoomName, Assault.w18s26.Squad, Assault.w18s26.Mode)
                    ("W18S26", Assault.probe, Provoke)
                    "a probe's Provoke on W18S26"
            }

            // The Brief's S2 (#518): W19S26, a harassment room and the
            // entry of the parked W18S26 probe, was read live as kept by the
            // assault. An assault switched off claims no room, and one switched
            // on works its own room alone: its entry and the rooms beside it
            // are crossed, never worked.
            test
                "an assault switched off claims no room; one switched on works its own room and crosses its entry" {
                let entry = "W12S29"

                let viewOf active =
                    let colonies =
                        assaultDeclared Provoke active
                        |> List.map (fun colony ->
                            { colony with
                                Assaults =
                                    colony.Assaults
                                    |> List.map (fun assault ->
                                        { assault with Entry = Some entry })
                            })

                    viewUnder colonies assaultWorld mother

                let worked (view: ColonyView) =
                    (view.Spatial.Rooms |> Map.keys |> Set.ofSeq) - view.Crossed

                let off = viewOf false
                let bare = viewUnder declared assaultWorld mother

                Expect.isEmpty off.Assaults "nothing is pooled or cast for it"

                Expect.equal
                    (worked off, off.Crossed)
                    (worked bare, bare.Crossed)
                    "its room, its entry and the rooms beside it are neither worked nor crossed"

                let on = viewOf true

                Expect.isTrue
                    (Set.contains assaultRoom (worked on))
                    "switched on, its room is worked"

                Expect.equal
                    (Set.contains entry on.Crossed, Set.contains entry (worked on))
                    (true, false)
                    "and its entry crossed, never worked"
            }

            test
                "W18S26's controller is declared from W15S28, (10,22) then (11,22) in by W17S26, two sappers and two escorts, paused" {
                let declaring =
                    Colony.declared
                    |> List.filter (fun colony ->
                        List.contains Assault.w18s26Controller colony.Assaults)
                    |> List.map (fun colony -> colony.Home)

                Expect.equal declaring [ "W15S28" ] "declared once, by W15S28"

                let assault = Assault.w18s26Controller

                Expect.equal
                    (assault.RoomName,
                     assault.Breach,
                     assault.Targets,
                     assault.Entry,
                     assault.MaxHops)
                    ("W18S26", [], [ { X = 10; Y = 22 }; { X = 11; Y = 22 } ], Some "W17S26", None)
                    "no breach, (10,22) then (11,22), in from W17S26 on the tuning's budget"

                Expect.equal
                    (assault.Squad, assault.Mode, assault.Active)
                    ([ "sapper"; "sapper"; "escort"; "escort" ], Provoke, false)
                    "two sappers and two escorts, paused"
            }

            test
                "an assault's own hop budget widens its projection to every room a walk inside it could cross; the default's is the rectangle (#494)" {
                let link = Assault.w18s26Link
                let projected = Assault.roomsProjected [ link ] "W15S28"

                for room in [ "W15S29"; "W16S29"; "W17S29"; "W18S29"; "W18S28"; "W18S27"; "W18S26" ] do
                    Expect.contains projected room $"{room}, on the walk by the south"

                let defaulted = Assault.roomsProjected [ { link with MaxHops = None } ] "W15S28"

                Expect.equal
                    (Set.ofList defaulted)
                    (Set.ofList (
                        "W18S26" :: RoomName.transitBetween "W15S28" "W18S26"
                        @ RoomName.adjacent "W18S26"
                        @ RoomName.transitBetween "W15S28" "W18S27"
                     )
                     |> Set.remove "W15S28")
                    "no budget of its own: the rectangle to it and to its entry, and the rooms beside it"
            }

            test
                "an assault naming its targets takes down the breach, then each one's rampart and the structure under it, and no tower (#496)" {
                let view = viewUnder (assaultNaming [ linkTile ] Strike true) assaultWorld mother

                match view.Assaults with
                | [ facts ] ->
                    Expect.equal
                        facts.Targets
                        (Some
                            [
                                "ramp-breach", { X = 10; Y = 20 }
                                "ramp-link", linkTile
                                "link-rival", linkTile
                            ])
                        "breach, the link's rampart, the link"

                    Expect.isFalse facts.Taken "the link stands"
                | other -> failtest $"one assault, not {other}"
            }

            test
                "its targets all down, the assault is taken: seen so, or remembered by the raid log while the room is dark (#496)" {
                let razed =
                    { assaultWorld with
                        Rooms =
                            assaultWorld.Rooms
                            |> Map.change
                                assaultRoom
                                (Option.map (fun facts ->
                                    { facts with
                                        TargetKinds =
                                            facts.TargetKinds
                                            |> Map.remove "link-rival"
                                            |> Map.remove "ramp-link"
                                    }))
                    }

                let factsUnder gate world =
                    let colonies = assaultNaming [ linkTile ] Provoke true
                    let colony = colonies |> List.find (fun colony -> colony.Home = mother)

                    let holders =
                        World.creepColonies
                            Tuning.defaults
                            colonies
                            (World.living colonies world)
                            noneShut
                            world

                    match
                        (ColonyView.ofWorld Tuning.defaults colonies gate holders world colony)
                            .Assaults
                    with
                    | [ facts ] -> facts
                    | other -> failtest $"one assault, not {other}"

                let takenGate =
                    { StandDown.none with
                        Taken = Set.singleton assaultRoom
                    }

                let seen = factsUnder StandDown.none razed
                Expect.isTrue seen.Taken "seen with the link down"

                Expect.equal
                    seen.Targets
                    (Some [ "ramp-breach", { X = 10; Y = 20 } ])
                    "the breach is all that stands"

                Expect.isFalse
                    (factsUnder takenGate assaultWorld).Taken
                    "seen standing again: not taken, whatever the log says"

                let dark =
                    { assaultWorld with
                        Rooms = assaultWorld.Rooms |> unseen assaultRoom
                    }

                Expect.isTrue (factsUnder takenGate dark).Taken "dark: the log's answer"
                Expect.isFalse (factsUnder StandDown.none dark).Taken "dark, and never seen taken"

                let undeclared =
                    match (viewUnder (assaultDeclared Provoke true) razed mother).Assaults with
                    | [ facts ] -> facts.Taken
                    | other -> failtest $"one assault, not {other}"

                Expect.isFalse undeclared "an assault naming no targets is never taken"
            }

            test "W17S24's Strike is declared, and off until the user switches it on" {
                let declared =
                    Colony.declared
                    |> List.collect (fun colony -> colony.Assaults)
                    |> List.filter (fun assault -> assault.RoomName = "W17S24")

                Expect.equal declared [ Assault.w17s24 ] "declared once"

                Expect.equal
                    (Assault.w17s24.Mode, Assault.w17s24.Squad, Assault.w17s24.Active)
                    (Strike, Assault.breachers, false)
                    "the default squad's Strike, off"

                Expect.equal
                    Assault.w17s24.Breach
                    [ { X = 2; Y = 20 } ]
                    "its west line, x2 y18-21, opened at y20"

                Expect.equal Assault.w17s24.Entry (Some "W18S24") "walked in from the west"
            }

            test "an assault switched off projects nothing and runs nothing" {
                let view = viewUnder (assaultDeclared Provoke false) assaultWorld mother

                Expect.isEmpty view.Assaults "no assault"

                Expect.isFalse
                    (Map.containsKey assaultRoom view.Spatial.Rooms)
                    "and its room is in no scan set"
            }

            test
                "a Provoke's sappers take down the breach's rampart and nothing more, round the rest of the line" {
                let view = viewUnder (assaultDeclared Provoke true) assaultWorld mother

                match view.Assaults with
                | [ facts ] ->
                    Expect.equal
                        facts.Targets
                        (Some [ "ramp-breach", { X = 10; Y = 20 } ])
                        "the declared tile's rampart"

                    Expect.isFalse facts.SafeMode "no safe mode in sight"
                | other -> failtest $"one assault, not {other}"

                let layer = SpatialInfo.layerOf view.Spatial assaultRoom

                for tile in [ { X = 10; Y = 20 }; { X = 10; Y = 21 } ] do
                    Expect.isTrue
                        (Set.contains tile layer.Obstacles)
                        $"the rival's rampart at {tile} is a wall to our walk"

                Expect.isFalse
                    (Map.containsKey "tower-rival" view.Spatial.TargetKinds)
                    "and nothing there is classified, as in any room we cross"
            }

            test "a Strike's go on to the towers and then the spawns; a dark room names none" {
                let view = viewUnder (assaultDeclared Strike true) assaultWorld mother

                Expect.equal
                    (view.Assaults |> List.map (fun facts -> facts.Targets))
                    [
                        Some
                            [
                                "ramp-breach", { X = 10; Y = 20 }
                                "tower-rival", { X = 30; Y = 30 }
                                "spawn-rival", { X = 31; Y = 31 }
                            ]
                    ]
                    "breach, tower, spawn"

                let dark =
                    { assaultWorld with
                        Rooms = assaultWorld.Rooms |> unseen assaultRoom
                    }

                Expect.equal
                    ((viewUnder (assaultDeclared Strike true) dark mother).Assaults
                     |> List.map (fun facts -> facts.Targets))
                    [ None ]
                    "dark: nothing known"
            }

            test
                "its room's safe mode and the window it cannot raise one in are read off memory, dark or seen (#491)" {
                let now = assaultWorld.Time

                let remembering controllers =
                    { assaultWorld with
                        Rooms = assaultWorld.Rooms |> unseen assaultRoom
                        RivalControllers = Map.ofList controllers
                    }

                let factsUnder world =
                    match (viewUnder (assaultDeclared Strike true) world mother).Assaults with
                    | [ facts ] -> facts
                    | other -> failtest $"one assault, not {other}"

                let trep = { trepController 4 with Seen = now }

                let provoked =
                    factsUnder (
                        remembering
                            [
                                assaultRoom, trep
                                "W18S26",
                                { trepController 6 with
                                    SafeModeUntil = now + 20_000
                                }
                            ]
                    )

                Expect.equal provoked.BarredUntil (Some(now + 20_000)) "barred while W18S26's runs"
                Expect.isFalse provoked.SafeMode "and none runs in its own room"

                let moded =
                    factsUnder (
                        remembering [ assaultRoom, { trep with SafeModeUntil = now + 500 } ]
                    )

                Expect.isTrue moded.SafeMode "safe mode remembered in the dark"

                let open' = factsUnder (remembering [ assaultRoom, trep ])
                Expect.isNone open'.BarredUntil "it can raise one: no window"
            }
        ]
