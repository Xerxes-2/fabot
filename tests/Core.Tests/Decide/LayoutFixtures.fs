/// The Layout: what the Planner places and where — the clustered ordering, the
/// trunks, the Storage, the Link footings, the room layer a site is filed under
/// — and the plan memo the census signature keys, which is the Layout's own
/// cache and moves with every input the plan reads (ADR 0017, ADR 0044).
/// The Layout suite's fixtures: the colonies and pockets the Planner is run
/// over, and the helpers that read a placement back.
module Fabot.Core.Tests.Decide.LayoutFixtures

open Expecto
open Fabot.Core
open Fabot.Core.Types
open Fabot.Core.Decide
open Fabot.Core.Tests
open Fabot.Core.Tests.Decide.Fixtures

/// The trunk fixture without its source: nothing to pave a trunk from.
let noSourceColony level =
    { trunkColony level with
        Sources = []
        Spatial =
            { trunkRoom with
                TargetKinds = Map.remove "src-a" trunkRoom.TargetKinds
            }
            |> withHome (fun layer ->
                { layer with
                    TargetPositions = Map.remove "src-a" layer.TargetPositions
                })
    }

/// The trunk fixture with a second source walled into a pocket: every
/// neighbour of (20,30) is wall terrain except the single Seat east of
/// it — the W12S28-source-B shape (ADR 0012).
let pocketColony level =
    let srcB = { X = 20; Y = 30 }
    let seat = { X = 21; Y = 30 }

    let walled =
        [
            for dx in -1 .. 1 do
                for dy in -1 .. 1 do
                    { X = srcB.X + dx; Y = srcB.Y + dy }
        ]
        |> List.filter (fun tile -> tile <> seat)

    let room =
        trunkRoom
        |> withHome (fun layer ->
            { layer with
                Terrain = (layer.Terrain, walled) ||> List.fold (fun acc t -> Map.add t Wall acc)
            })
        |> withTargets [ "src-b", srcB, Source ]

    { trunkColony level with
        Sources = [ source "src-a"; source "src-b" ]
        Spatial = room
    }

/// The colony with its own road plan already standing: the state the
/// source containers drop in — a container defers to a road site on its
/// tile (one construction site per tile) and coexists with the built road.
///
/// It stands the roads the plan *places*, not the roads it plans, so
/// below the road gate it stands none (#209) and the premise is empty:
/// every caller that needs a road under its container asks from RCL3 up.
let withRoadsBuilt colony =
    let { Intents = intents } = decideOn colony

    { colony with
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Roads = sitesOfKind Road intents |> Set.ofList
                })
    }

let chebyshev a b = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

/// The clustered ordering's sort key for a fixture whose spawn stands at
/// (25,25): nearest-to-spawn first, ties by x then y (ADR 0011).
let orderKey tile =
    chebyshev tile { X = 25; Y = 25 }, tile.X, tile.Y

/// A tile of the room the Layout fixtures below plan. `openRoom` names
/// its projection "W1N1", and every tile the Layout records carries the
/// room it planned since #216 R3 (ADR 0052 decision 2) — so an
/// expectation written as a grid coordinate joins it back here, and a
/// footing recorded in any other room fails it.
let plannedTile (tile: Pos) : RoomPos = RoomPos.at "W1N1" tile

/// Synthetic footing fixture: the source stands three tiles north of the
/// spawn, so its trunk leaves by (24,23) and the source container is
/// planned there, while the Storage takes the ordering's first pick at
/// (24,24). The tile the container's Link footing wants, (23,23), is one
/// of the cluster's own same-colour tiles — the collision the reservation
/// exists to settle (ADR 0022).
let footingRoom = openRoom 6 |> withTargets [ "src-a", { X = 25; Y = 22 }, Source ]

/// The two tiles `footingRoom` holds as Link footings: one beside the
/// planned source container at (24,23), one beside the Storage at (24,24).
/// Two, not four — the count is one per planned source container plus the
/// controller container and the Storage, and this room projects no
/// controller position.
let footingTiles = Set.ofList [ { X = 23; Y = 23 }; { X = 24; Y = 25 } ]

/// The room with a target standing on a tile and blocking it, the way the
/// projection carries a built Storage or link: a target and an obstacle.
let withStanding id pos kind room =
    withTargets [ id, pos, kind ] room
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.add pos layer.Obstacles
        })

/// The tile the trunk fixture's Thorium deposit stands on (ADR 0057): a
/// wall tile well south of the trunk line, so its eight Seats are open
/// ground and the Seat nearest the trunk is a choice and not the only
/// candidate. South rather than beside the spawn because the pick is a
/// comparison and a deposit inside the cluster would make it a tie.
let mineralPos = { X = 25; Y = 33 }

/// The trunk fixture with a Thorium deposit standing in it: a wall tile
/// carrying a `TargetKind.Mineral`, blocking its own tile the way the shell
/// projects one (a mineral is one of Screeps' OBSTACLE_OBJECT_TYPES) and
/// off every trunk. The colony's roads are stood first, because the
/// container pick is priced against the trunks and a room below the road
/// gate has paved none (#209).
let mineralColonyAt pos level =
    let colony = trunkColony level

    { colony with
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Terrain = Map.add pos Wall layer.Terrain
                })
            |> withStanding "min-a" pos Mineral
    }
    |> withRoadsBuilt

let mineralColony level = mineralColonyAt mineralPos level

/// The tile a second deposit fixture stands on: the one place in this room
/// where the two readings of "the Seat nearest the trunk" disagree, and the
/// premise of the test that tells them apart (ADR 0057 decision 1). The trunk
/// fixture's source→controller leg **arcs north over the spawn** — the spawn
/// and its two built extensions are in the way — so a deposit here has one Seat
/// standing on that arc at (22,19) and another one step off the source→spawn
/// leg at (22,21). Priced against every trunk the room paved, the arc's tile
/// wins at range 0 and the container is seated on the side facing *away* from
/// the Storage; priced against the Storage's trunk, which is what ADR 0057
/// says, (22,21) wins.
let trunkSplitMineralPos = { X = 23; Y = 20 }

/// The Seat of `trunkSplitMineralPos` that stands on the controller-bound arc,
/// and the Seat one step off the spawn-bound leg: the wrong answer and the
/// right one, named so a metric that widened back would fail by name.
let controllerSideSeat = { X = 22; Y = 19 }
let storageSideSeat = { X = 22; Y = 21 }

/// The deposit's Seats in `mineralColony`: the walkable neighbours of its
/// tile, by terrain alone (ADR 0001), derived rather than written down so
/// the set follows the fixture's terrain if that ever moves.
let mineralSeats (colony: ColonyView) =
    Atlas.seatTilesOf (Atlas.ofView colony) "min-a" |> Set.map RoomPos.pos

/// A footing fixture whose trunks run through the clustered ring: the
/// source stands against the room's east edge and the controller against
/// its west edge, so the source→controller trunk crosses the whole
/// cluster and the tiles a footing pushes the cluster onto are the ones
/// the trunk would otherwise want (ADR 0022, ADR 0027).
let crossedRoom =
    openRoom 6
    |> withTargets [ "src-a", { X = 30; Y = 26 }, Source ]
    |> withStanding "ctrl-1" { X = 19; Y = 25 } Controller

/// The colony one tick on: every site the Layout just asked for now
/// standing in the projection as a construction site, the obstacle kinds
/// blocking their tile exactly as the engine's own sites do — the state
/// the next tick's plan is computed against. Both halves go through the
/// Core's own tables (#75): a .NET-side projection builder that restated
/// them would drift from the one `buildSpatial` really builds, and the
/// tests would stay green describing a room the bot never sees.
let withPlanPending colony =
    let { Intents = intents } = decideOn colony

    let sites =
        placementIntents intents
        |> List.mapi (fun i (_, pos, kind) -> $"site-{i}", pos, builtKindOfPlaceable kind)

    { colony with
        Spatial =
            withTargets [ for id, pos, kind in sites -> id, pos, Site kind ] colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Obstacles =
                        (layer.Obstacles, sites)
                        ||> List.fold (fun acc (_, pos, kind) ->
                            if isWalkable kind then acc else Set.add pos acc)
                })
    }

/// W12S28's `10,43` shape, synthesised (#77): the pocket source's only
/// Seat is its container's pick, and every one of the eight tiles beside
/// that pick is spoken for — four wall, the source itself, the one trunk
/// road out, and two standing extensions, which is the live room's own
/// tile table in proportion. The extensions are the live loss: `11,43`
/// took one in the RCL4 burst, planned by a bundle that did not yet hold
/// footings back. Nothing is left for the fold to reserve, so this room's
/// guarantee is short by one — and `pocketColony`, the same room without
/// the seal, is the control that serves all four.
let sealedPocketColony level =
    let colony = pocketColony level
    let sealedTiles = [ { X = 21; Y = 29 }; { X = 21; Y = 31 } ]

    let standingExtensions =
        [ "ext-3", { X = 22; Y = 29 }; "ext-4", { X = 22; Y = 31 } ]

    let walled =
        colony.Spatial
        |> withHome (fun layer ->
            { layer with
                Terrain =
                    (layer.Terrain, sealedTiles)
                    ||> List.fold (fun acc tile -> Map.add tile Wall acc)
            })

    { colony with
        Spatial =
            (walled, standingExtensions)
            ||> List.fold (fun room (id, tile) ->
                withStanding id tile (Structure BuiltKind.Extension) room)
    }

/// The trunk fixture cut in two: a wall ridge down x=30 severs the
/// controller and its whole Upgrade Work Area from the rest of the room,
/// leaving the source and the spawn together on the west side. `src-a`
/// still routes its trunk to the spawn and can route none to the Work
/// Area — the per-goal shape #107 records, and the one a record keyed on
/// the source alone would get wrong.
let severedControllerColony level =
    let colony = trunkColony level

    let ridge = [ for y in 15..35 -> { X = 30; Y = y } ]

    { colony with
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Terrain =
                        (layer.Terrain, ridge)
                        ||> List.fold (fun acc tile -> Map.add tile Wall acc)
                })
    }

/// The pocket fixture with its one Seat walled shut: every one of `src-b`'s
/// eight neighbours is wall, so no goal is reachable from it at all and
/// both its trunks are dropped. `src-a`, in the open, keeps both — the
/// record names the source as well as the goal.
let enclosedSourceColony level =
    let colony = pocketColony level

    { colony with
        Spatial =
            colony.Spatial
            |> withHome (fun layer ->
                { layer with
                    Terrain = Map.add { X = 21; Y = 30 } Wall layer.Terrain
                })
    }

/// The step-weight grid ADR 0032's guard compares, for the room the caller
/// names. The room is the caller's rather than the fixture's home since
/// #169: the walk table now holds an entry per *goal* room and the far
/// leg's entry is a pure function of that room's grid, so a guard that
/// could only ask about home would pin the pairing in one room while the
/// memo reads every projected one — which is the asymmetry
/// `Atlas.stepWeights` was already given a room parameter for. Read off
/// the projection rather than retyped as a literal: `stepWeights` answers
/// every tile impassable for a room the projection does not carry (ADR
/// 0004, ADR 0041), so a literal that drifted from its fixture would leave
/// the one `sequenceEqual` in the group comparing two empty grids and
/// passing whatever the census did.
let internal stepGridOf (room: string) (snapshot: ColonyView) =
    Atlas.stepWeights (Atlas.ofView snapshot) room

/// The same grid for the colony's own room — the reading every home-room
/// perturbation in the guard group is compared through.
let internal homeGridOf (snapshot: ColonyView) =
    stepGridOf (SpatialInfo.homeName snapshot.Spatial) snapshot

/// The colony with the given creeps standing on the given tiles. A placed
/// creep beside a placed spawn is all it takes to price a lead, and pricing
/// one floods out of the spawner (ADR 0026).
let staffedColony creeps positions colony =
    { colony with
        Creeps = creeps
        Spatial = colony.Spatial |> withCreepsAt positions
    }

/// A memo whose site Intents are a sentinel no computation would produce:
/// reuse is then observable verbatim at the decide seam.
let sentinelMemo snapshot =
    {
        Signature = censusSignature snapshot
        SiteIntents = [ PlaceConstructionSite(RoomPos.at "W1N1" { X = 1; Y = 1 }, Tower) ]
        UnservedFootings = []
        ServedFootings = []
        UnroutedTrunks = []
        DeferredContainers = []
        HaulerQuota = 0
        HaulerDemand = []
        HaulerLoad = 0
        Walks = WalkTable()
    }

/// Two rooms whose coordinates collide on purpose (ADR 0041). At home:
/// the controller at (25,22), the buffer container "can-home" two tiles
/// off it, and a source far away at (20,30) — so the buffer is the
/// controller's and no source's. In the outpost: a source at (25,25),
/// range 1 from the home buffer's coordinates, and a container at
/// (25,23), range 1 from the home controller's. Nothing here is nearer
/// than a room boundary to anything it collides with.
let collidingRooms =
    { atLevel
          2
          (openRoom 8
           |> withTargets
               [
                   "ctrl-1", { X = 25; Y = 22 }, Controller
                   "can-home", { X = 25; Y = 24 }, Structure BuiltKind.Container
                   "src-a", { X = 20; Y = 30 }, Source
               ]) with
        Sources = [ source "src-a"; source "src-out" ]
    }
    |> withOutpost
        "W1N2"
        [
            "src-out", { X = 25; Y = 25 }, Source
            "can-out", { X = 25; Y = 23 }, Structure BuiltKind.Container
        ]
        [
            for x in 20..30 do
                for y in 20..30 -> { X = x; Y = y }, Plain
        ]

/// A home container stranded six tiles from the spawn and serving no home
/// source, with an outpost source one tile from its coordinates — the
/// hauler quota's half of the same collision. Six tiles because the quota
/// is a round trip: a container beside the spawn prices at zero and would
/// hire nobody whichever room the source stood in.
let strandedContainer sourceRoom =
    let home =
        openRoom 8
        |> withTargets
            [
                "can-far", { X = 25; Y = 31 }, Structure BuiltKind.Container
                "src-a", { X = 19; Y = 19 }, Source
            ]

    let colony =
        { bareRespawn with
            Sources = [ source "src-a"; source "src-out" ]
            Spatial = home
        }

    let strayed = "src-out", { X = 25; Y = 32 }, Source

    if Some sourceRoom = home.RoomName then
        { colony with
            Spatial = colony.Spatial |> withTargets [ strayed ]
        }
    else
        colony |> withOutpost sourceRoom [ strayed ] []

/// The mirror of the collision: a home source at (25,31) and an outpost
/// container one tile off its coordinates, in the outpost. The quota is
/// flooded over the home room's grid, so a container it does not place
/// must never reach the arithmetic at all.
let outpostContainerColony =
    { bareRespawn with
        Sources = [ source "src-a" ]
        Spatial = openRoom 8 |> withTargets [ "src-a", { X = 25; Y = 31 }, Source ]
    }
    |> withOutpost "W1N2" [ "can-out", { X = 25; Y = 32 }, Structure BuiltKind.Container ] []
