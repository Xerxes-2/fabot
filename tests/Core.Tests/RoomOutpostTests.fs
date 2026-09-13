/// The declared outposts against their captures, and the outpost
/// container on real terrain (ADR 0042).
module Fabot.Core.Tests.RoomOutpostTests

open Expecto
open Fabot.Core.Types
open Fabot.Core.Atlas
open Fabot.Core.Decide
open Fabot.Core.Tests.RoomFixtures
open Fabot.Core.Tests.Decide
open Fabot.Core.Tests.RoomInvariantFixtures

[<Tests>]
let outpostDeclarationTests =
    testList
        "the declared outposts against their captures"
        [
            test
                "every outpost the live declaration names is its capture's, whichever colony declares it" {
                // The test above reads `Outpost.adr0042`, which is the pair
                // the real-terrain fixtures are built on and deliberately
                // frozen at the day it was measured — so it says nothing
                // about the declarations a human has added since (`w13s29`,
                // `w15s28`), and a mistyped id in one of those would reach
                // the server as a target nothing places, in silence (ADR
                // 0004). This is the same check over the *live* constant,
                // and it grows by itself: a declaration added to
                // `Colony.declared` is checked here the day it is written,
                // and the only thing it needs is its room's committed
                // capture.
                //
                // Sorted, and that is the one way it differs from the pair
                // above: what a live declaration must get right is which
                // tile each id belongs to, never the order the pairs stand
                // in. `w13s29` is written in the order the survey ranked
                // its rocks and `w15s28` in the order its capture lists
                // them, and both are correct — nothing downstream may read
                // a source by its index (`Outpost`), so an order asserted
                // here would be a rule invented by its own test.
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
                // `ViewTests` asks this of the **names** over the live
                // constant (`Outpost.withinHopBudget`), which is all that
                // altitude can answer; this is the other half, #259's, and
                // it needs terrain — so it belongs here, where the captures
                // are. The first declaration to need it is W15S28: two hops
                // out, joined only if W14S28's two rings are both crossable,
                // and a room the names accept while the ground refuses is
                // projected, pooled and hired for by a row that hires per
                // declared outpost.
                //
                // `linked` is built the way the shell builds it
                // (`World.linked`): a ring tile the capture carries whose
                // terrain is not wall, and a room no capture is loaded for
                // is joined to nothing — which is what keeps the search
                // inside the rooms the projection would hold.
                let rings =
                    Colony.declared
                    |> List.collect (fun colony ->
                        Outpost.roomsProjected colony.Outposts colony.Home)
                    |> List.distinct
                    |> List.map (fun room -> room, (load room).Border)
                    |> Map.ofList

                let linked fromRoom toRoom =
                    let walkableIn room tile =
                        match Map.tryFind room rings with
                        | Some border ->
                            match Map.tryFind tile border with
                            | Some terrain -> terrain <> Wall
                            | None -> false
                        | None -> false

                    Seam.joinedBy (walkableIn fromRoom) (walkableIn toRoom) fromRoom toRoom

                let unreachable =
                    Colony.declared
                    |> List.collect (fun colony ->
                        Outpost.refused linked Tuning.defaults.MaxHops colony.Home colony.Outposts
                        |> List.map (fun room -> $"{room} is unreachable from {colony.Home}"))

                Expect.isEmpty
                    unreachable
                    $"""every declared outpost is joined to its home by a chain of Seams: {String.concat "; " unreachable}"""
            }

            test "each declaration names its own capture's furniture, id and tile alike" {
                // ADR 0042 declares W12S27 and W13S28 in the engine's own
                // ids (ADR 0041's decision, pinned in the loader tests
                // above), and this is where the constant and the committed
                // capture are made to agree. Compared against the capture
                // rather than against a literal: two literals of the same
                // ids agree with each other and with nothing the server
                // ever said, and a re-capture that moved a rock would leave
                // both of them green.
                //
                // Order included, and it carries a claim: W13S28's sources
                // are `16,7` then `18,4`, the reverse of ADR 0042's prose,
                // so a declaration written from the prose would pair each
                // id with the other rock. Nothing downstream may read a
                // source by its index — the tile is the identity — and this
                // is the line that says which tile each id is.
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
                // ADR 0042's first acceptance: the three outpost sources
                // and the two outpost controllers are *in* the projection
                // and answerable by the geometry queries — Seats for a
                // source, an Upgrade Work Area for a controller. Both are
                // read in the target's own room off its id (ADR 0041), so
                // an empty answer here would be a declaration the colony
                // can see and never work.
                //
                // Named as properties, never as tiles: a Seat is a
                // walkable neighbour of its source, and a Work Area tile is
                // walkable within the Upgrade range of its controller. The
                // capture supplies the terrain and the test supplies no
                // expected value (ADR 0036).
                for outpost in declaredOutposts do
                    let capture = load outpost.RoomName
                    let atlas = declaredAtlas outpost

                    let walkable tile =
                        match Map.tryFind tile capture.Terrain with
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

                        // The container is the switch that admits an
                        // outpost into the economy (ADR 0042), and no
                        // container stands in either room yet — so every
                        // one of these sources is unposted, which is
                        // exactly what makes it worth nothing to the
                        // workforce target this ticket narrowed.
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

                    // The area the reserver actually stands on (ADR 0042):
                    // reserveController acts at range 1 and a controller's
                    // own tile is an obstacle, so this is its walkable
                    // neighbours and nothing else — a much narrower set
                    // than the Upgrade area above, and W12S27's is two
                    // tiles of swamp. Named as a property and never as
                    // those tiles (ADR 0036): what must hold is that the
                    // set is non-empty, because an empty one is silent —
                    // the Task stays pooled, `threatened` reads an empty
                    // area as unthreatened, and the reserver matched to it
                    // is rejected as unreachable for its whole life.
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
                // ADR 0042's placement rule, stated as a property and never
                // as a tile: the pick is on that rock's *own* Seats, and no
                // other Seat of that rock walks out to the Seam in fewer
                // ticks. Both halves matter and neither implies the other —
                // the first would hold for a rule that read another room's
                // geometry into this one, the second for a rule that picked
                // any tile at all.
                //
                // Real terrain is the counterexample generator here (ADR
                // 0036): the two captures hold a single-Seat rock (`16,7`),
                // a two-Seat rock split between plain and swamp (`18,4`)
                // and a three-Seat rock of nothing but swamp (`16,45`), and
                // no expected value below comes from any of them.
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

                // Everything below is derived from the declaration, and an
                // empty one would leave this case green having checked
                // nothing — the guard the sweep above this file uses, for
                // the same reason.
                Expect.isNonEmpty declaredSources "a declaration nobody made is nothing to check"

                Expect.hasLength
                    (sites |> List.filter (fun (room, _) -> room <> home))
                    (List.length declaredSources)
                    "one container planned per declared outpost rock, and not one more"

                for room, id, pos in declaredSources do
                    let where = $"{room} source {id}"
                    let seats = seatTilesOf atlas id |> RoomPos.inRoom room

                    // Attributed by the geometry a source container *is* —
                    // range 1 of the rock (ADR 0012) — so that standing on
                    // a Seat is something this asserts rather than
                    // something it assumed to find the site.
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
                // ADR 0047's arrangement on the ground the colony actually
                // stands on: W13S28 declared a colony of its own while it
                // is still one of W12S28's outposts. The room is projected
                // because the mother declares it, its controller is in the
                // kind census under the engine's own id, and the tick a
                // human writes the second entry that controller stops being
                // a Reserve and becomes a Claim — while the *other*
                // outpost, W12S27, is untouched beside it.
                //
                // Read off the declaration rather than typed out: the ids
                // are the engine's and are pinned against the captures
                // above, so naming one here would be a second literal
                // agreeing with the first and with nothing the server said.
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
                    planTasks { colony with Declared = homes } noThreats Set.empty
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

// ---- the flood settled on demand (#174) ---------------------------------
