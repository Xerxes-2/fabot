/// The projection builders every suite shares: the home room's layer, the
/// funnels that change it, and the neighbour room beside it. They live here
/// rather than in each suite's own fixture module because they are one
/// projection's shape and not one suite's — `AtlasFixtures` and
/// `Decide.Fixtures` had carried byte-identical copies of all four, each
/// documenting the other as its twin.
[<AutoOpen>]
module Fabot.Core.Tests.SpatialFixtures

open Fabot.Core.Types

/// What a fixture's construction site still owes (#364). One number shared by
/// every fixture that stands a site, because almost none of them care: the
/// Build pool is one Task per site whatever the site costs, and the cases that
/// *do* care — the worker row's backlog term — name their own figure.
///
/// Deliberately not 0. A site owing nothing is a site the engine would have
/// turned into a structure on the tick it was paid, so it is a shape `World`
/// cannot produce, and a fixture built on it would let a rule read "no backlog"
/// off a room full of sites.
let siteOwes = 1_000

/// The home room's geometry, read back off a projection: the room `RoomName`
/// names, and the one the empty name files when it names none
/// (`SpatialInfo.homeName`). Absent geometry reads as an empty layer, never as
/// a lookup that throws (ADR 0004).
let homeLayer (spatial: SpatialInfo) : RoomLayer =
    SpatialInfo.layerOf spatial (SpatialInfo.homeName spatial)

/// The same projection with the home room's layer changed. Since ADR 0041's
/// contract step the tile-shaped containers live under a room name and nowhere
/// else, so a test that used to copy-update the projection itself — `{ spatial
/// … with CreepPositions = … }` — reaches through this instead. It merges into
/// whatever layer is already there, so composing it with the target funnels is
/// order-blind. Apply it after `RoomName` is final: the home name is resolved
/// when it runs, and a projection layered then renamed leaves its geometry
/// filed under the old name.
let withHome (change: RoomLayer -> RoomLayer) (spatial: SpatialInfo) : SpatialInfo =
    { spatial with
        Rooms = Map.add (SpatialInfo.homeName spatial) (change (homeLayer spatial)) spatial.Rooms
    }

/// The home layer with these creeps standing on these tiles — `withHome` over
/// the one field a test names most, written here so the hundred-odd sites that
/// place a crowd say what they place and not how a layer is copy-updated.
///
/// It **replaces** that field where `withHome` above merges the layer, which is
/// what the blocks it stands in for did: two of these in a row leave the second
/// one's crowd alone on the tiles, not both. That is the only shape any caller
/// wanted, and a test that needs to add to a crowd should reach for `withHome`
/// and say so.
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

/// The home layer with a construction site of somebody else's on each of these
/// tiles (#248). The `RivalSites` twin of the two funnels above, replacing
/// likewise. Tiles and nothing more is the whole of what the projection carries
/// about one, so there is no id or kind for a case to name.
let withRivalSites (tiles: Pos list) (spatial: SpatialInfo) : SpatialInfo =
    spatial
    |> withHome (fun layer ->
        { layer with
            RivalSites = Set.ofList tiles
        })

/// The home layer with these tiles paved. The Roads twin, replacing likewise.
let withRoads (tiles: Pos list) (spatial: SpatialInfo) : SpatialInfo =
    spatial |> withHome (fun layer -> { layer with Roads = Set.ofList tiles })

/// Projection with the given target positions and terrain tiles; no creeps, no
/// obstacles — tests layer those on top. It files them through `withHome` and
/// inherits its ordering rule, which bites hardest here because this funnel
/// starts from `SpatialInfo.empty`: the home name it resolves is the empty one,
/// so a projection built by this and *then* given a `RoomName` carries its
/// geometry under the empty name while `RoomName` says another, and every
/// reader that asks by *room* — the weight grid, the census signature, the
/// hauler quota — answers off `RoomLayer.empty`. The target-keyed queries still
/// find it, because `SpatialInfo.placementOf` scans every layer, which is what
/// makes the mistake quiet. Name the room first, then build.
let spatial targets tiles =
    SpatialInfo.empty
    |> withHome (fun layer ->
        { layer with
            Terrain = TerrainGrid.ofList tiles
            TargetPositions = Map.ofList targets
        })

/// The same projection with a second room's layer beside the colony's own,
/// under that room's name — the shape an outpost arrives in, and the only one
/// there is since the tile-shaped containers moved under a room name (ADR
/// 0041). It adds an entry and never replaces the map, so the home room's
/// geometry survives a neighbour joining after it.
let withNeighbour room layer (spatial: SpatialInfo) =
    { spatial with
        Rooms = Map.add room layer spatial.Rooms
    }

/// **What shape the projection can actually have** (#355): which kinds of
/// object `World.ofGame` can file under each of its id-keyed maps, and the
/// check that asks a colony whether it is one of them.
///
/// Sixty-three of this suite's sixty-five files run on fixtures we author, so
/// what they confirm is the code's belief about the projection rather than the
/// engine's behaviour, and `src/App/World.fs` — the sweep that builds the
/// thing — is Fable/JS and has no test at all by construction. Four live
/// incidents in one day came through that gap, and the first of them is this
/// shape: `Facts.reactorTakesALoad` read the Reactor's store out of
/// `SpatialInfo.Thorium`, where the sweep never writes it (it rides
/// `RoomFacts.Reactors`), so the projection answered 0 for a store holding
/// 999, the draw gate never closed, and 915 T reached the floor. The unit test
/// agreed, because the fixture had written the store where the gate looked.
///
/// The table below is read off `World.fs` **block by block, by inspection** —
/// the sweep cannot be run from .NET, and standing the real `World.ofGame`
/// against a fake `Game` is the profile harness's to do (#294, #308). So a
/// change to one of those blocks that is not made here is a drift no test can
/// see, and each rule names the block it came from so the next reader can
/// check. What it buys is the half that caught the incident: the two sides of
/// a number have to agree about which objects can carry it.
///
/// It lives here, with the projection builders and ahead of every suite, so
/// that any test can ask it of a colony it built; `ProjectionShapeTests` asks
/// it of every shared fixture and of `ColonyView.ofWorld`'s own output.
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
/// `Thorium` block): a structure with a store (`isStored`, the Core fact the
/// sweep itself reads), the [[thorium]] deposit, ore on the floor, and a
/// tombstone or ruin holding some (#359). A **Reactor is not one of them**.
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

/// Every entry of a projection whose key is an object the sweep could not have
/// filed under that map. Asked of a whole `ColonyView` and not of its
/// `SpatialInfo` alone, because the `Owners` block is the odd one: the sweep
/// files an owner for a **Reactor** and for nothing else, and the only place
/// the projection names a Reactor is `ColonyView.Reactors`.
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
