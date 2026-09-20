/// The [[keeper margin]]: the ground a body of ours does not walk on in a
/// Source Keeper room. ADR-0060
///
/// The one place the colony edits a fact about the world before a rule reads
/// it, and the exception is argued on the class: a keeper is stationary,
/// known before the room is ever seen, and geometry rather than a reaction —
/// a hostile the map can answer before the tick begins is answered by the
/// map, and one that has to be reacted to is a [[reach]].
///
/// **The hostile list is untouched; the ground is.** A keeper still enters
/// `World.factsOf`'s hostiles, still derives a Reach and is still recorded by
/// the [[raid log]]. What changes is that no walkable tile of ours lies inside
/// that Reach, so the answer [[flee]] owes is already given by the ground.
///
/// The mask covers the **steady state**, not every tick: a freshly respawned
/// keeper walks from its lair to the rock it adopted, and for those two to
/// four ticks its Reach covers unmasked ground (#327 carries the sweep and the
/// three ways out; none is a wider margin, because past 7 the room cannot be
/// crossed). That is why nothing here filters a keeper out of any list.
module Fabot.Core.Types.Keepers

/// The rocks a keeper is pinned to, per room name — its **lairs, its sources
/// and its mineral**. `keepers/pretick.js` binds each keeper to
/// `memory_sourceId` and moves it to range 1 of that source or mineral, never
/// of its lair, so a mask around the lairs alone would leave every source
/// unmasked.
///
/// Keyed by **room name** and hung off no declaration: two colonies that ever
/// project W15S26 must mask it identically.
///
/// The tiles are the engine's own, read off `/api/game/room-objects` once,
/// terrain and lairs being fixed for the life of the server. W15S26's three
/// sources and its mineral are checked against the committed capture
/// (`RoomSeamTests`, `rooms/W15S26.room`); the four **lairs** are not, because
/// `scripts/capture-room.mjs` keeps sources, controllers and minerals alone
/// (widening the capture is #316's).
///
/// The kinds live in the comments beside the tiles, deliberately: a test that
/// could then check "every lair is within the margin of the rock it feeds" is
/// not the property that fails (the walk after a respawn is), and structure
/// added for the weaker check would read as if the stronger one held. A room
/// declared here whose lairs sit further from their rocks than W15S26's (3 to
/// 5 tiles) makes that window wider, and nothing in this file goes red about
/// it.
let centres: Map<string, Pos list> =
    Map.ofList
        [
            // W15S26, the Source Keeper room the chain from W15S28 to the
            // sector Reactor in W15S25 crosses, read at tick 404,835 on
            // 2026-09-13: four keeper lairs, three sources and one mineral,
            // in the API's own order.
            "W15S26",
            [
                { X = 35; Y = 11 } // lair
                { X = 6; Y = 17 } // lair
                { X = 5; Y = 36 } // lair
                { X = 42; Y = 39 } // lair
                { X = 11; Y = 16 } // source
                { X = 4; Y = 33 } // source
                { X = 39; Y = 34 } // source
                { X = 38; Y = 7 } // mineral (X, under an owner-less extractor)
            ]
        ]

/// One room's declared centres; the empty list for a room with none.
let centresIn (room: string) : Pos list =
    Map.tryFind room centres |> Option.defaultValue []

/// Whether a tile of this room is masked ground: within `margin` of a declared
/// centre, in the Chebyshev measure (`Geometry.range`). The pointwise form,
/// for a reader that holds its own terrain and asks per tile.
let masked (margin: int) (room: string) (tile: Pos) : bool =
    centresIn room |> List.exists (fun centre -> range centre tile <= margin)

/// The same answer with the room resolved **once**, for a reader that asks it
/// about many tiles of one room (`World.ringWalkable` asks forty-eight times
/// a band). A room the declaration names none of answers false without a
/// comparison, which is every room but the handful above.
let maskIn (margin: int) (room: string) : Pos -> bool =
    match centresIn room with
    | [] -> fun _ -> false
    | centres -> fun tile -> centres |> List.exists (fun centre -> range centre tile <= margin)

/// The same rule in bulk: every masked tile of this room inside the
/// fifty-by-fifty grid, border ring included, de-duplicated and in
/// `tilesWithin` order. What the [[atlas]] lays its grids from.
///
/// The ball is walked here rather than built as a list per centre, and
/// de-duplicated through the grid's own index rather than `List.distinct`:
/// the answer is identical, tile for tile and in the same order, and what it
/// drops is `8 × 169` intermediate `Pos` values and a `HashSet<Pos>` every
/// tick. The atlas calls it once per projected room per tick and it was the
/// single largest attributable cost in a `reactor` run: 44.5 ms of a 316 ms
/// `decide`, of which the de-duplication alone was 10.0 ms (sampled
/// 2026-09-17; W15S26's answer is 937 tiles).
let maskedTilesIn (margin: int) (room: string) : Pos list =
    match centresIn room with
    | [] -> []
    | centres ->
        let seen = Array.zeroCreate<bool> tileCount
        let tiles = ResizeArray<Pos>()

        for centre in centres do
            for x in centre.X - margin .. centre.X + margin do
                if x >= 0 && x < Engine.roomSide then
                    for y in centre.Y - margin .. centre.Y + margin do
                        if y >= 0 && y < Engine.roomSide then
                            let index = x * Engine.roomSide + y

                            if not seen.[index] then
                                seen.[index] <- true
                                tiles.Add { X = x; Y = y }

        List.ofSeq tiles
