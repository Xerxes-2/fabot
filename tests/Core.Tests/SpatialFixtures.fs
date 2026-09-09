/// The projection builders every suite shares: the home room's layer, the
/// funnels that change it, and the neighbour room beside it. They live here
/// rather than in each suite's own fixture module because they are one
/// projection's shape and not one suite's — `AtlasFixtures` and
/// `Decide.Fixtures` had carried byte-identical copies of all four, each
/// documenting the other as its twin.
[<AutoOpen>]
module Fabot.Core.Tests.SpatialFixtures

open Fabot.Core.Types

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
            Terrain = Map.ofList tiles
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
