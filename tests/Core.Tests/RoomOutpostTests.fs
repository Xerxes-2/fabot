/// The declared outposts against their captures, and the outpost
/// container on real terrain.
module Fabot.Core.Tests.RoomOutpostTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Decide
open Fabot.Core.Tests.RoomFixtures
open Fabot.Core.Tests.Decide
open Fabot.Core.Tests.RoomInvariantFixtures

/// The captured rooms another player owned at the capture's tick.
let private capturedRivals =
    Map.ofList [ "W18S26", "Trepidimous"; "W19S29", "giaco" ]

/// A capture is furniture and carries no owner, so who owned each captured
/// room at its tick is written here, as the sightings the World would hold.
let private rivalSightings (rooms: string list) =
    capturedRivals
    |> Map.filter (fun room _ -> List.contains room rooms)
    |> Map.map (fun room owner ->
        {
            Tick = (load room).Tick
            Targets = lazy Set.empty
            Rival = Some owner
        })

/// `World.linked` itself over a World built from the real captures of the
/// given rooms, so no predicate of it is copied here to move in lockstep
/// (#317, #438). A room no capture is loaded for carries no border and is
/// joined to nothing, which keeps the search inside the rooms the projection
/// would hold.
let private shippedLinked (rooms: string list) =
    let world =
        { World.empty with
            Rooms =
                rooms
                |> List.distinct
                |> List.map (fun room ->
                    let capture = load room

                    room,
                    { RoomFacts.empty with
                        Border = capture.Border
                        Layer =
                            { RoomLayer.empty with
                                Terrain = capture.Terrain
                            }
                    })
                |> Map.ofList
            Sightings = rivalSightings rooms
        }

    World.linked (Tuning.keeperMargin Tuning.defaults) world

[<Tests>]
let outpostDeclarationTests =
    testList
        "the declared outposts against their captures"
        [
            test
                "every outpost the live declaration names is its capture's, whichever colony declares it" {
                // `Outpost.adr0042` is frozen at the day it was measured, so it says
                // nothing about the declarations added since (`w13s29`, `w15s28`); a
                // mistyped id there would reach the server as a target nothing places,
                // in silence. This is the same check over the *live* constant.
                //
                // Sorted: what a live declaration must get right is which tile each id
                // belongs to, never the order. `w13s29` is written in survey order and
                // `w15s28` in capture order, and nothing downstream may read a source
                // by its index.
                let declared = Colony.declared |> List.collect (fun colony -> colony.Outposts)

                Expect.isNonEmpty declared "a declaration nobody made is nothing to check"

                for outpost in declared do
                    let capture = load outpost.RoomName

                    Expect.equal
                        (outpost.Sources
                         |> List.map (fun (id, tile) -> id, RoomPos.pos tile)
                         |> List.sort)
                        (capture.RealSources |> List.sort)
                        $"{outpost.RoomName}: every source the server answered with, each under its own id"

                    Expect.equal
                        (Some(fst outpost.Controller, RoomPos.pos (snd outpost.Controller)))
                        capture.RealController
                        $"{outpost.RoomName}: the controller a reserver or a claimer would hold"

                    Expect.equal
                        (outpost.Sources
                         |> List.map (fun (_, tile) -> tile.Room)
                         |> List.append [ (snd outpost.Controller).Room ]
                         |> List.distinct)
                        [ outpost.RoomName ]
                        $"{outpost.RoomName}: every declared tile is filed under the room it is a tile of (ADR 0052 decision 2)"
            }

            test "a chain of real border rings joins every declared outpost to its home" {
                // `ViewTests` asks this of the **names**; this half needs terrain. The
                // first declaration to need it is W15S28: two hops out, joined only if
                // W14S28's two rings are both crossable.
                // No room declared today is a Source Keeper room, so the mask takes
                // nothing here.
                let linked =
                    shippedLinked (
                        Colony.declared
                        |> List.collect (fun colony ->
                            Outpost.roomsProjected colony.Outposts colony.Home)
                    )

                let unreachable =
                    Colony.declared
                    |> List.collect (fun colony ->
                        Outpost.refused linked Tuning.defaults.MaxHops colony.Home colony.Outposts
                        |> List.map (fun entry ->
                            $"{entry.RoomName} is unreachable from {colony.Home}"))

                Expect.isEmpty
                    unreachable
                    $"""every declared outpost is joined to its home by a chain of Seams: {String.concat "; " unreachable}"""
            }

            test "a chain of real border rings joins every declared salvage room to its home" {
                // The outpost case above, over the third declaration kind, the salvage,
                // through the same shipped predicates: a salvage room no chain
                // reaches is refused, and its dismantler is never cast.
                let linked =
                    shippedLinked (
                        Colony.declared
                        |> List.collect (fun colony ->
                            colony.Home :: Salvage.roomsProjected colony.Salvage colony.Home)
                    )

                Expect.isNonEmpty
                    (Colony.declared |> List.collect (fun colony -> colony.Salvage))
                    "a declaration nobody made is nothing to check"

                let unreachable =
                    Colony.declared
                    |> List.collect (fun colony ->
                        Salvage.refused linked Tuning.defaults.MaxHops colony.Home colony.Salvage
                        |> List.map (fun entry ->
                            $"{entry.RoomName} is unreachable from {colony.Home}"))

                Expect.isEmpty
                    unreachable
                    $"""every declared salvage room is joined to its home by a chain of Seams: {String.concat "; " unreachable}"""

                // W11S29 from W12S28: two crossings, by W12S29. W11S28 is the other
                // room a shortest walk could cross, and its seam with W11S29 has no
                // tile to cross by.
                Expect.equal
                    (RoomName.routesBy linked Tuning.defaults.MaxHops "W12S28" "W11S29")
                    [ [ "W12S28"; "W12S29"; "W11S29" ] ]
                    "W11S29 is two crossings from W12S28, by W12S29"
            }

            test
                "a chain of real border rings joins every harassment room to the colonies in the running for it" {
                // Who reaches each room over the captures; who of those casts it
                // is `World.harassCasters`, asked below at the live banks.
                let homes =
                    Colony.declared
                    |> List.map (fun colony -> colony.Home)
                    |> List.filter (fun home ->
                        Colony.harass
                        |> List.exists (fun h ->
                            Declaration.withinHopBudget Tuning.defaults.MaxHops home h.RoomName))

                // Every room `World.ofGame` puts in the world for these homes'
                // harassment (`worldRooms`): each room, its rectangle and its Via.
                let rooms =
                    homes
                    |> List.collect (fun home ->
                        home
                        :: Harass.roomsProjected
                            (Colony.harass
                             |> List.filter (fun h ->
                                 Declaration.withinHopBudget
                                     Tuning.defaults.MaxHops
                                     home
                                     h.RoomName))
                            home)
                    |> List.distinct

                let linked = shippedLinked rooms

                let castersOf room =
                    homes
                    |> List.filter (fun home ->
                        Declaration.routable linked Tuning.defaults.MaxHops home room)

                Expect.equal
                    (Colony.harass |> List.map (fun h -> h.RoomName, castersOf h.RoomName))
                    [
                        // W17S29's every chain to W17S26 runs through W18S26,
                        // which Trepidimous owns (#444), and W12S26, five
                        // columns east, has no chain to it inside the budget.
                        "W18S27", [ "W15S28"; "W17S29" ]
                        "W17S26", [ "W13S28"; "W15S28" ]
                        // W15S28's walk dips south round the wall that
                        // W18S27's does, eight crossings, past the budget.
                        "W19S26", [ "W17S29" ]
                    ]
                    "each harassment room is reached, both ways, by exactly the colonies the ground allows"

                let detours = RoomName.routesBy linked Tuning.defaults.MaxHops "W15S28" "W18S27"

                Expect.contains
                    detours
                    [ "W15S28"; "W15S29"; "W16S29"; "W16S28"; "W17S28"; "W17S27"; "W18S27" ]
                    "W18S27 is six crossings from W15S28, south round the wall and back north"

                for chain in detours do
                    Expect.equal
                        (List.truncate 3 chain, List.length chain)
                        ([ "W15S28"; "W15S29"; "W16S29" ], 7)
                        $"every shortest chain is six crossings and leaves by the declared detour: {chain}"

                // W16S26's mask (#438) closes neither entry: the south band
                // lands on the column that climbs to its west exits at y 16..18.
                Expect.equal
                    (RoomName.routesBy linked Tuning.defaults.MaxHops "W15S28" "W17S26")
                    [
                        [ "W15S28"; "W15S27"; "W15S26"; "W16S26"; "W17S26" ]
                        [ "W15S28"; "W15S27"; "W16S27"; "W16S26"; "W17S26" ]
                    ]
                    "W17S26 is four crossings from W15S28, by W15S26 or W16S27 and both into W16S26"

                Expect.equal
                    (Declaration.hops linked Tuning.defaults.MaxHops "W13S28" "W17S26")
                    (Some 6)
                    "and six from W13S28"

                // Neither W18S26 (Trepidimous) nor W19S29 (giaco) is entered.
                Expect.equal
                    (RoomName.routesBy linked Tuning.defaults.MaxHops "W17S29" "W19S26")
                    [
                        [ "W17S29"; "W17S28"; "W17S27"; "W18S27"; "W19S27"; "W19S26" ]
                        [ "W17S29"; "W17S28"; "W18S28"; "W18S27"; "W19S27"; "W19S26" ]
                        [ "W17S29"; "W17S28"; "W18S28"; "W19S28"; "W19S27"; "W19S26" ]
                        [ "W17S29"; "W18S29"; "W18S28"; "W18S27"; "W19S27"; "W19S26" ]
                        [ "W17S29"; "W18S29"; "W18S28"; "W19S28"; "W19S27"; "W19S26" ]
                    ]
                    "W19S26 is five crossings from W17S29, every chain into it by W19S27"

                Expect.equal
                    (Declaration.hops linked 10 "W15S28" "W19S26")
                    (Some 8)
                    "and eight from W15S28, two past the budget"

                // The live banks (2026-09-29; W17S29's is the call's); W12S28
                // and W11S27 are past the budget of every room. The floor is
                // 2,100, three ranger blocks, and 1,400 for W19S26, declared
                // at two (#457).
                let banks w13s28 w17s29 =
                    [
                        "W12S28", 5_600
                        "W13S28", w13s28
                        "W15S28", 5_600
                        "W11S27", 1_800
                        "W17S29", w17s29
                    ]

                // No sighting of W19S26: nothing of ours has ever seen it.
                let casting w13s28 w17s29 =
                    let banks = banks w13s28 w17s29

                    let world =
                        { World.empty with
                            Rooms =
                                (rooms @ List.map fst banks)
                                |> List.distinct
                                |> List.map (fun name ->
                                    let capture = load name

                                    let facts =
                                        { RoomFacts.empty with
                                            Border = capture.Border
                                            Layer =
                                                { RoomLayer.empty with
                                                    Terrain = capture.Terrain
                                                }
                                        }

                                    name,
                                    match List.tryFind (fst >> (=) name) banks with
                                    | None -> facts
                                    | Some(_, bank) ->
                                        { facts with
                                            Control =
                                                Some
                                                    {
                                                        Owner = Ownership.Ours
                                                        Reservation = None
                                                        SafeMode = false
                                                        Sign = None
                                                    }
                                            Spawns =
                                                [
                                                    {
                                                        Name = $"spawn-{name}"
                                                        Id = $"spawn-{name}"
                                                        RoomName = name
                                                        IsSpawning = false
                                                    }
                                                ]
                                            Energy = { Available = bank; Capacity = bank }
                                        })
                                |> Map.ofList
                            Sightings = rivalSightings rooms
                        }

                    World.harassCasters
                        (JoinTable())
                        Tuning.defaults
                        Colony.declared
                        {
                            Rooms = Colony.harass
                            BlockCost = Bodies.rangerBlockCost
                        }
                        world

                let casters w13s28 w17s29 =
                    (casting w13s28 w17s29).Casters
                    |> List.map (fun (h, caster) -> h.RoomName, caster)

                Expect.equal
                    (casters 5_600 1_800)
                    [ "W18S27", Some "W15S28"; "W17S26", Some "W15S28"; "W19S26", Some "W17S29" ]
                    "W15S28 casts the first two, which W17S29 at RCL5 (2026-10-02) cannot buy the full floor of; W17S29 casts W19S26 unseen, which W15S28 is past the budget of"

                Expect.equal
                    (casting 5_600 1_800).Floors
                    (Map.ofList
                        [
                            "W18S27", Tuning.defaults.HarassBlocks
                            "W17S26", Tuning.defaults.HarassBlocks
                            "W19S26", 2
                        ])
                    "W18S27 and W17S26 keep the full floor; W19S26 is floored at its declared two blocks"

                Expect.equal
                    (casters 5_650 1_800)
                    [ "W18S27", Some "W15S28"; "W17S26", Some "W15S28"; "W19S26", Some "W17S29" ]
                    "and W13S28's larger bank does not take W17S26 from the nearer W15S28"

                Expect.equal
                    (casters 5_600 1_300)
                    [ "W18S27", Some "W15S28"; "W17S26", Some "W15S28"; "W19S26", None ]
                    "W17S29's RCL4 bank buys no floor, and W19S26 is refused"

                Expect.equal
                    (casters 5_600 2_300)
                    [ "W18S27", Some "W17S29"; "W17S26", Some "W15S28"; "W19S26", Some "W17S29" ]
                    "W17S29's RCL6 bank buys the full floor: it casts W19S26 and the nearer W18S27, and never W17S26 behind W18S26"

                for h in Colony.harass do
                    Expect.contains
                        (List.map snd (load h.RoomName).RealSources)
                        (RoomPos.pos h.Stand)
                        $"{h.RoomName}: the Stand is the enemy's source tile"

                    Expect.equal
                        h.Stand.Room
                        h.RoomName
                        $"{h.RoomName}: the Stand is a tile of its own room"

                    Expect.equal
                        (Some h.Controller)
                        ((load h.RoomName).RealController
                         |> Option.map (fun (_, pos) -> RoomPos.at h.RoomName pos))
                        $"{h.RoomName}: the Controller is the capture's controller tile"
            }

            test "each declaration names its own capture's furniture, id and tile alike" {
                // Compared against the capture rather than a literal: two literals of
                // the same ids agree with each other and with nothing the server said.
                //
                // Order included: W13S28's sources are `16,7` then `18,4`, the reverse
                // of ADR-0042's prose, so a declaration written from the prose would
                // pair each id with the other rock.
                Expect.isNonEmpty declaredOutposts "a declaration nobody made is nothing to check"

                for outpost in declaredOutposts do
                    let capture = load outpost.RoomName

                    Expect.equal
                        outpost.RoomName
                        capture.RoomName
                        "the capture read is the room the declaration names"

                    Expect.equal
                        (outpost.Sources |> List.map (fun (id, tile) -> id, RoomPos.pos tile))
                        capture.RealSources
                        $"{outpost.RoomName}: every source the server answered with, in its order"

                    Expect.equal
                        (Some(fst outpost.Controller, RoomPos.pos (snd outpost.Controller)))
                        capture.RealController
                        $"{outpost.RoomName}: the controller a reserver would hold (ADR 0042)"
            }

            test "every declared source and controller is geometry the projection can price" {
                // Seats for a source, an Upgrade Work Area for a controller, each read
                // in the target's own room off its id. Named as properties, never as
                // tiles: the capture supplies the terrain and the test no expected value.
                for outpost in declaredOutposts do
                    let capture = load outpost.RoomName
                    let atlas = declaredAtlas outpost

                    let walkable tile =
                        match TerrainGrid.tryFind tile capture.Terrain with
                        | Some terrain -> terrain <> Wall
                        | None -> false

                    for id, tile in outpost.Sources do
                        let pos = RoomPos.pos tile
                        let seatTiles = seatTilesOf atlas id |> RoomPos.inRoom outpost.RoomName
                        let where = $"{outpost.RoomName} source {id}"

                        Expect.equal
                            (targetRoom atlas id)
                            (Some outpost.RoomName)
                            $"{where}: filed under its own room, so its Seats are that room's ground"

                        Expect.isNonEmpty
                            seatTiles
                            $"{where}: a source nobody can stand beside is no outpost"

                        Expect.equal
                            (seats atlas id)
                            (Some(Set.count seatTiles))
                            $"{where}: the Seat count is the Seat tiles'"

                        Expect.all
                            seatTiles
                            (fun tile -> range tile pos = 1 && walkable tile)
                            $"{where}: every Seat a walkable neighbour of the rock"

                        // No container stands in either room yet, so every one of these
                        // sources is unposted.
                        Expect.isEmpty
                            (postsOf atlas id)
                            $"{where}: no container stands, so the source has no Post"

                    let controllerId, controllerTile = outpost.Controller
                    let controllerPos = RoomPos.pos controllerTile

                    let area =
                        workArea atlas (Upgrade controllerId) |> RoomPos.inRoom outpost.RoomName

                    let where = $"{outpost.RoomName} controller {controllerId}"

                    Expect.equal
                        (targetRoom atlas controllerId)
                        (Some outpost.RoomName)
                        $"{where}: filed under its own room"

                    Expect.isNonEmpty
                        area
                        $"{where}: a controller with no ground around it is unreservable"

                    Expect.all
                        area
                        (fun tile -> range tile controllerPos <= 3 && walkable tile)
                        $"{where}: every Work Area tile walkable within the Upgrade range"

                    // reserveController acts at range 1 and a controller's own tile is an
                    // obstacle, so this is its walkable neighbours and nothing else;
                    // W12S27's is two tiles of swamp. An empty set is silent: the Task
                    // stays pooled, `threatened` reads it as unthreatened, and the reserver
                    // matched to it is rejected as unreachable for its whole life.
                    let reserveArea =
                        workArea atlas (Reserve controllerId) |> RoomPos.inRoom outpost.RoomName

                    Expect.isNonEmpty
                        reserveArea
                        $"{where}: a controller nobody can stand beside can never be reserved"

                    Expect.all
                        reserveArea
                        (fun tile -> range tile controllerPos = 1 && walkable tile)
                        $"{where}: every Reserve Work Area tile a walkable neighbour of the controller"
            }
        ]

[<Tests>]
let outpostContainerTests =
    testList
        "the outpost container on real terrain"
        [
            test "each declared source is planned one container, on the Seat nearest the Seam" {
                // Stated as a property: the pick is on that rock's *own* Seats, and no
                // other Seat of that rock walks out to the Seam in fewer ticks. Neither
                // half implies the other. The captures hold a single-Seat rock
                // (`16,7`), a plain-and-swamp two-Seat rock (`18,4`) and a three-Seat
                // rock of nothing but swamp (`16,45`).
                let colony = declaredColony 5
                let atlas = ofView colony
                let home = SpatialInfo.homeName colony.Spatial
                let { Intents = intents } = decide colony Map.empty Set.empty None

                let sites =
                    intents
                    |> List.choose (function
                        | PlaceConstructionSite(tile, Container) ->
                            Some(tile.Room, RoomPos.pos tile)
                        | _ -> None)

                let declaredSources =
                    [
                        for outpost in declaredOutposts do
                            for id, tile in outpost.Sources ->
                                outpost.RoomName, id, RoomPos.pos tile
                    ]

                // Everything below is derived from the declaration, and an empty one
                // would leave this case green having checked nothing.
                Expect.isNonEmpty declaredSources "a declaration nobody made is nothing to check"

                Expect.hasLength
                    (sites |> List.filter (fun (room, _) -> room <> home))
                    (List.length declaredSources)
                    "one container planned per declared outpost rock, and not one more"

                for room, id, pos in declaredSources do
                    let where = $"{room} source {id}"
                    let seats = seatTilesOf atlas id |> RoomPos.inRoom room

                    // Attributed by the geometry a source container is, range 1 of the
                    // rock, so standing on a Seat is asserted rather than assumed.
                    let mine =
                        sites
                        |> List.filter (fun (siteRoom, tile) ->
                            siteRoom = room && range tile pos <= 1)

                    Expect.hasLength mine 1 $"{where}: exactly one container planned for this rock"

                    let _, pick = List.head mine

                    Expect.isTrue
                        (Set.contains pick seats)
                        $"{where}: the pick is one of this rock's own Seats, in its own room"

                    match seamWalkTicks atlas room home pick with
                    | None -> failtest $"{where}: the pick is a tile no walk reaches the Seam from"
                    | Some picked ->
                        for seat in seats do
                            match seamWalkTicks atlas room home seat with
                            | None -> ()
                            | Some other ->
                                Expect.isLessThanOrEqual
                                    picked
                                    other
                                    $"{where}: no Seat of this rock walks out to the Seam in fewer ticks"
            }

            test "the candidate colony's controller is the pool's one Claim, on the real rooms" {
                // W13S28 declared a colony of its own while still one of W12S28's
                // outposts: the tick a human writes the second entry its controller
                // stops being a Reserve and becomes a Claim, while W12S27 is untouched.
                //
                // Read off the declaration rather than typed out, so the ids stay
                // pinned against the captures above.
                let candidate = "W13S28"

                let controllerOf room =
                    declaredOutposts
                    |> List.tryFind (fun outpost -> outpost.RoomName = room)
                    |> Option.map (fun outpost -> fst outpost.Controller)

                let colony = declaredColony 5

                // Sorted, because what is asserted is which Task stands on
                // which controller and never the order a Map's keys came
                // out in.
                let pooled homes =
                    let view = { colony with Declared = homes }

                    planTasks
                        view
                        (Fabot.Core.Atlas.ofView view)
                        noThreats
                        HeldTaskFacts.empty
                        (outpostFactsOf view)
                    |> List.filter (function
                        | Reserve _
                        | Claim _ -> true
                        | _ -> false)
                    |> List.sort

                match controllerOf candidate, controllerOf "W12S27" with
                | Some west, Some north ->
                    Expect.equal
                        (pooled [])
                        (List.sort [ Reserve north; Reserve west ])
                        "undeclared, both outpost controllers are Reserves and neither is claimed"

                    Expect.equal
                        (pooled [ "W12S28"; candidate ])
                        (List.sort [ Reserve north; Claim west ])
                        "declared, the candidate colony's controller is a Claim and the other outpost is unmoved"
                | _ -> failtest "the declaration names a controller for each of its outposts"
            }
        ]

[<Tests>]
let errandDeclarationTests =
    testList
        "the declared errands against their captures"
        [
            test "a chain of real border rings joins every declared errand to its home" {
                // `ViewTests` asks the **names**; this asks the ground. The reactor is
                // three crossings out, joined only if W15S27's and W15S26's rings are
                // both crossable.
                //
                // `linked` is built the way `World.linked` builds it, and a room no
                // capture is loaded for is joined to nothing.
                //
                // NOTE: these rings and this ground are the **raw** captures, as a
                // deliberate control: the keeper margin takes tiles out of W15S26's
                // layers and is re-checked over the **masked** layer in `RoomSeamTests`
                // and `ViewTests`. A red line here is the terrain moving and a red line
                // there is the mask moving.
                //
                // The hand-rolled predicates below are #337's to delete: the outpost
                // case above calls the shipped predicates, and this one cannot, because
                // those take a margin and what is wanted here is no mask at all.
                let rings =
                    Colony.declared
                    |> List.collect (fun colony ->
                        Outpost.roomsProjected colony.Outposts colony.Home
                        @ Errand.roomsProjected colony.Errands colony.Home)
                    |> List.distinct
                    |> List.map (fun room -> room, ((load room).Border, (load room).Terrain))
                    |> Map.ofList

                let linked fromRoom toRoom =
                    let nonWall (layer: Map<Pos, Terrain>) tile =
                        match Map.tryFind tile layer with
                        | Some terrain -> terrain <> Wall
                        | None -> false

                    let walkableIn room tile =
                        match Map.tryFind room rings with
                        | Some(border, _) -> nonWall border tile
                        | None -> false

                    // The far room's ground beside the landing, raw like the ring above it.
                    let groundIn room tile =
                        match Map.tryFind room rings with
                        | Some(_, terrain) ->
                            match TerrainGrid.tryFind tile terrain with
                            | Some ground -> ground <> Wall
                            | None -> false
                        | None -> false

                    Seam.joinedBy
                        (walkableIn fromRoom)
                        (walkableIn toRoom)
                        (groundIn toRoom)
                        fromRoom
                        toRoom

                Expect.isNonEmpty
                    (Colony.declared |> List.collect (fun colony -> colony.Errands))
                    "a declaration nobody made is nothing to check"

                let unreachable =
                    Colony.declared
                    |> List.collect (fun colony ->
                        Errand.refused linked Tuning.defaults.MaxHops colony.Home colony.Errands
                        |> List.map (fun entry ->
                            $"{entry.RoomName} is unreachable from {colony.Home}"))

                Expect.isEmpty
                    unreachable
                    $"""every declared errand is joined to its home by a chain of Seams: {String.concat "; " unreachable}"""

                // The chain itself: three crossings by W15S27 and the Source Keeper room
                // W15S26, the 154-step walk the third colony stands where it does for.
                // Every shortest chain and not one of them: this says there is exactly one.
                Expect.equal
                    (RoomName.routesBy linked Tuning.defaults.MaxHops "W15S28" "W15S25")
                    [ [ "W15S28"; "W15S27"; "W15S26"; "W15S25" ] ]
                    "the reactor is three crossings from W15S28, by W15S27 and W15S26"

                // W13S28 could run it inside the budget, five crossings out; W15S28's
                // three is the shortest walk to the reactor, which is why it declares it.
                Expect.isTrue
                    (Errand.routable linked Tuning.defaults.MaxHops "W13S28" Errand.w15s25)
                    "W13S28 could run the errand"

                Expect.equal
                    ([ "W15S28"; "W13S28" ]
                     |> List.map (fun home ->
                         Declaration.hops linked Tuning.defaults.MaxHops home "W15S25"))
                    [ Some 3; Some 5 ]
                    "and W15S28's chain is the shorter"
            }

            test "every declared errand names a tile its own capture holds as ground" {
                // `capture-room.mjs` takes a room's *fixed* furniture (sources,
                // controller, mineral) and a reactor is none of those, so no committed
                // file carries the id the declaration names; what it carries is the ground.
                for colony in Colony.declared do
                    for errand in colony.Errands do
                        let capture = load errand.RoomName
                        let id, tile = errand.Target
                        let pos = RoomPos.pos tile

                        Expect.equal
                            errand.RoomName
                            capture.RoomName
                            "the capture read is the room the declaration names"

                        Expect.equal
                            (TerrainGrid.tryFind pos capture.Terrain)
                            (Some Plain)
                            $"{errand.RoomName}: the declared target stands on plain ground the server answered with"

                        Expect.isNonEmpty
                            (neighbourhood pos
                             |> List.filter (fun tile ->
                                 match TerrainGrid.tryFind tile capture.Terrain with
                                 | Some terrain -> terrain <> Wall
                                 | None -> false))
                            $"{errand.RoomName}: with ground beside it for the body that acts on it to stand on"

                        Expect.isFalse
                            (capture.RealSources
                             @ (capture.RealController |> Option.toList)
                             @ capture.RealMinerals
                             |> List.exists (fun (other, _) -> other = id))
                            $"{errand.RoomName}: and the id is the errand's own object, not a rock the capture already names"
            }
        ]

// ---- the flood settled on demand (#174) ---------------------------------
