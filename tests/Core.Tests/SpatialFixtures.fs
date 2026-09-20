/// The projection builders every suite shares: the home room's layer, the
/// funnels that change it, and the neighbour room beside it.
[<AutoOpen>]
module Fabot.Core.Tests.SpatialFixtures

open Fabot.Core.Types

/// What a fixture's construction site still owes. One number shared by every
/// fixture that stands a site; the cases that care name their own figure.
///
/// Deliberately not 0: a site owing nothing is a shape `World` cannot
/// produce, and a fixture built on it would let a rule read "no backlog" off
/// a room full of sites.
let siteOwes = 1_000

/// The home room's geometry, read back off a projection: the room `RoomName`
/// names, and the one the empty name files when it names none
/// (`SpatialInfo.homeName`). Absent geometry reads as an empty layer, never
/// as a lookup that throws.
let homeLayer (spatial: SpatialInfo) : RoomLayer =
    SpatialInfo.layerOf spatial (SpatialInfo.homeName spatial)

/// The same projection with the home room's layer changed. It merges into
/// whatever layer is already there, so composing it with the target funnels
/// is order-blind. Apply it after `RoomName` is final: a projection layered
/// then renamed leaves its geometry filed under the old name.
let withHome (change: RoomLayer -> RoomLayer) (spatial: SpatialInfo) : SpatialInfo =
    { spatial with
        Rooms = Map.add (SpatialInfo.homeName spatial) (change (homeLayer spatial)) spatial.Rooms
    }

/// The home layer with these creeps standing on these tiles. It **replaces**
/// that field where `withHome` merges the layer: two of these in a row leave
/// the second one's crowd alone on the tiles. A test that needs to add to a
/// crowd should reach for `withHome`.
let withCreepsAt (placed: (string * Pos) list) (spatial: SpatialInfo) : SpatialInfo =
    spatial
    |> withHome (fun layer ->
        { layer with
            CreepPositions = Map.ofList placed
        })

/// The home layer with these tiles blocked. The Obstacles twin of
/// `withCreepsAt`, replacing rather than merging for the same reason.
let withObstacles (tiles: Pos list) (spatial: SpatialInfo) : SpatialInfo =
    spatial
    |> withHome (fun layer ->
        { layer with
            Obstacles = Set.ofList tiles
        })

/// The home layer with a construction site of somebody else's on each of
/// these tiles. Replaces likewise; tiles are all the projection carries.
let withRivalSites (tiles: Pos list) (spatial: SpatialInfo) : SpatialInfo =
    spatial
    |> withHome (fun layer ->
        { layer with
            RivalSites = Set.ofList tiles
        })

/// The home layer with these tiles paved. The Roads twin, replacing likewise.
let withRoads (tiles: Pos list) (spatial: SpatialInfo) : SpatialInfo =
    spatial |> withHome (fun layer -> { layer with Roads = Set.ofList tiles })

/// Projection with the given target positions and terrain tiles; no creeps,
/// no obstacles — tests layer those on top. It starts from
/// `SpatialInfo.empty`, so a projection built by this and *then* given a
/// `RoomName` carries its geometry under the empty name: every reader that
/// asks by room answers off `RoomLayer.empty`, while the target-keyed queries
/// still find it (`placementOf` scans every layer), which is what makes the
/// mistake quiet. Name the room first, then build.
let spatial targets tiles =
    SpatialInfo.empty
    |> withHome (fun layer ->
        { layer with
            Terrain = TerrainGrid.ofList tiles
            TargetPositions = Map.ofList targets
        })

/// The same projection with a second room's layer beside the colony's own,
/// under that room's name — the shape an outpost arrives in. It adds an
/// entry and never replaces the map.
let withNeighbour room layer (spatial: SpatialInfo) =
    { spatial with
        Rooms = Map.add room layer spatial.Rooms
    }

/// **What shape the projection can actually have** (#355): which kinds of
/// object `World.ofGame` can file under each of its id-keyed maps, and the
/// check that asks a colony whether it is one of them.
///
/// `src/App/World.fs` is Fable/JS and has no test by construction, so what
/// the fixtures confirm is the code's belief about the projection. The
/// first of #355's incidents came through that gap: `Facts.reactorTakesALoad`
/// read the Reactor's store out of `SpatialInfo.Thorium`, where the sweep
/// never writes it (it rides `RoomFacts.Reactors`), so a store holding 999
/// read as 0 and 915 T reached the floor. The unit test agreed, because the
/// fixture had written the store where the gate looked.
///
/// The table below is read off `World.fs` block by block, by inspection —
/// the sweep cannot be run from .NET — and each rule names the block it came
/// from so the next reader can check.
/// One entry of a projection whose key is an object the sweep could not have
/// filed under that map.
type ShapeViolation =
    {
        /// The map's name, as `SpatialInfo` spells it.
        Map: string
        /// The id filed under it.
        Id: string
        /// The kind the projection gives that id, and `None` for an id it
        /// gives no kind at all — which is what an [[errand]]'s declared
        /// target has, the Reactor included.
        Kind: TargetKind option
    }

/// Which kinds of object `World.ofGame` files an **ore** holding under (its
/// `Thorium` block): a structure with a store (`isStored`), the [[thorium]]
/// deposit, ore on the floor, and a tombstone or ruin holding some. A
/// **Reactor is not one of them**.
let private admitsOre =
    function
    | Some(Structure built) -> isStored built
    | Some Mineral
    | Some Tombstone
    | Some(Dropped Thorium) -> true
    | _ -> false

/// The same read down the energy column (the `Stores` block): a structure with
/// a store, a tombstone or ruin, and energy on the floor.
let private admitsEnergy =
    function
    | Some(Structure built) -> isStored built
    | Some Tombstone
    | Some(Dropped Energy) -> true
    | _ -> false

/// The `Cooldowns` block, which is one structure kind wide.
let private admitsCooldown = (=) (Some(Structure BuiltKind.Extractor))

/// The `Hits` block: a structure with a repair line. **Permissive on purpose**
/// — the sweep narrows further by `needsOwner`/`ourIds`, which is a fact about
/// the object and not about its kind, so this under-reports rather than lies.
let private admitsHits =
    function
    | Some(Structure built) -> (wholeLine built).IsSome
    | _ -> false

/// Every entry of a projection whose key is an object the sweep could not
/// have filed under that map. Asked of a whole `ColonyView`, because the
/// `Owners` block is the odd one: the sweep files an owner for a **Reactor**
/// and for nothing else.
let shapeViolations (view: ColonyView) : ShapeViolation list =
    let spatial = view.Spatial
    let kindOf id = Map.tryFind id spatial.TargetKinds
    let reactors = view.Reactors |> List.map (fun reactor -> reactor.Id) |> Set.ofList

    let check name admits (map: Map<string, 'v>) =
        map
        |> Map.toList
        |> List.choose (fun (id, _) ->
            let kind = kindOf id

            if admits id kind then
                None
            else
                Some { Map = name; Id = id; Kind = kind })

    let byKind admits = fun _ kind -> admits kind

    check "Thorium" (byKind admitsOre) spatial.Thorium
    @ check "Stores" (byKind admitsEnergy) spatial.Stores
    @ check "Cooldowns" (byKind admitsCooldown) spatial.Cooldowns
    @ check "Hits" (byKind admitsHits) spatial.Hits
    @ check "Owners" (fun id _ -> Set.contains id reactors) spatial.Owners

/// A violation as a line a failing test can print.
let shapeViolationLine (name: string) (violation: ShapeViolation) =
    let what =
        match violation.Kind with
        | Some kind -> string kind
        | None -> "an object the projection gives no kind"

    $"{name}: {violation.Map}[{violation.Id}] is {what}, which `World.ofGame` never files there"
