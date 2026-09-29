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
/// terrain and lairs being fixed for the life of the server. Each room's
/// three sources and its mineral are checked against its committed capture
/// (`RoomSeamTests`, `rooms/<name>.room`); the four **lairs** are not, because
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
            // The other keeper rooms of the sector (#438), read at tick 837,686
            // (W16S26 at 837,547, the one the chains from W15S28 to W17S26
            // cross): each four lairs, three sources and one mineral.
            "W14S24",
            [
                { X = 16; Y = 3 } // lair
                { X = 43; Y = 5 } // lair
                { X = 40; Y = 37 } // lair
                { X = 10; Y = 42 } // lair
                { X = 38; Y = 3 } // source
                { X = 45; Y = 39 } // source
                { X = 11; Y = 44 } // source
                { X = 14; Y = 4 } // mineral (O)
            ]
            "W15S24",
            [
                { X = 19; Y = 2 } // lair
                { X = 29; Y = 14 } // lair
                { X = 15; Y = 33 } // lair
                { X = 48; Y = 48 } // lair
                { X = 33; Y = 11 } // source
                { X = 17; Y = 34 } // source
                { X = 46; Y = 43 } // source
                { X = 17; Y = 5 } // mineral (H)
            ]
            "W16S24",
            [
                { X = 9; Y = 6 } // lair
                { X = 33; Y = 17 } // lair
                { X = 10; Y = 46 } // lair
                { X = 33; Y = 48 } // lair
                { X = 10; Y = 4 } // source
                { X = 36; Y = 17 } // source
                { X = 14; Y = 44 } // source
                { X = 38; Y = 44 } // mineral (U)
            ]
            "W14S25",
            [
                { X = 16; Y = 14 } // lair
                { X = 43; Y = 16 } // lair
                { X = 48; Y = 43 } // lair
                { X = 13; Y = 45 } // lair
                { X = 13; Y = 12 } // source
                { X = 44; Y = 40 } // source
                { X = 10; Y = 43 } // source
                { X = 41; Y = 17 } // mineral (H)
            ]
            "W16S25",
            [
                { X = 5; Y = 9 } // lair
                { X = 42; Y = 16 } // lair
                { X = 15; Y = 39 } // lair
                { X = 44; Y = 41 } // lair
                { X = 6; Y = 7 } // source
                { X = 40; Y = 17 } // source
                { X = 46; Y = 45 } // source
                { X = 12; Y = 37 } // mineral (O)
            ]
            "W14S26",
            [
                { X = 42; Y = 9 } // lair
                { X = 1; Y = 17 } // lair
                { X = 12; Y = 37 } // lair
                { X = 31; Y = 37 } // lair
                { X = 42; Y = 6 } // source
                { X = 3; Y = 14 } // source
                { X = 32; Y = 39 } // source
                { X = 10; Y = 38 } // mineral (K)
            ]
            "W16S26",
            [
                { X = 4; Y = 9 } // lair
                { X = 41; Y = 12 } // lair
                { X = 39; Y = 37 } // lair
                { X = 15; Y = 41 } // lair
                { X = 41; Y = 8 } // source
                { X = 37; Y = 39 } // source
                { X = 13; Y = 44 } // source
                { X = 5; Y = 5 } // mineral (O)
            ]
        ]

/// One room's declared centres; the empty list for a room with none.
let centresIn (room: string) : Pos list =
    Map.tryFind room centres |> Option.defaultValue []

/// Whether a room's **name** makes it a Source Keeper room: inside its sector,
/// 4 to 6 on both axes, the sector centre (5, 5) excepted.
let isKeeperRoom (room: string) : bool =
    match RoomName.inSector room with
    | Some(x, y) -> x >= 4 && x <= 6 && y >= 4 && y <= 6 && not (x = 5 && y = 5)
    | None -> false

/// Whether a chain may step **into** this room: false for a keeper room this
/// file declares no rocks for, whose keepers nothing would mask (#438). Asked
/// of the far room of a hop only, so no chain crosses or ends in one and a
/// body already standing in one may still leave.
let enterable (room: string) : bool =
    not (isKeeperRoom room) || Map.containsKey room centres

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

/// The ball walk behind `maskedIndicesIn`: every tile within `margin` of
/// one of `rocks`, clamped to the grid, each once, as grid indices.
///
/// Walked here rather than built as a list per centre, and de-duplicated
/// through the grid's own index rather than `List.distinct`: it drops `8 ×
/// 169` intermediate `Pos` values and a `HashSet<Pos>` (sampled 2026-09-17,
/// 10.0 ms of a 316 ms `reactor` `decide` was the de-duplication alone).
let private walkMask (margin: int) (rocks: Pos list) : int[] =
    let seen = Array.zeroCreate<bool> tileCount
    let indices = ResizeArray<int>()

    for centre in rocks do
        for x in centre.X - margin .. centre.X + margin do
            if x >= 0 && x < Engine.roomSide then
                for y in centre.Y - margin .. centre.Y + margin do
                    if y >= 0 && y < Engine.roomSide then
                        let index = x * Engine.roomSide + y

                        if not seen.[index] then
                            seen.[index] <- true
                            indices.Add index

    indices.ToArray()

/// The shipped margin's masks, laid once per process (#438): a mask reads
/// only the declared rocks and the margin, and both are static. Built whole
/// at module load and never written again, so Expecto's parallel lists share
/// it safely where a memo table would not (#310).
let private shippedMargin = Tuning.keeperMargin Tuning.defaults

let private shippedMasks: Map<string, int[]> =
    centres |> Map.map (fun _ rocks -> walkMask shippedMargin rocks)

/// The same rule in bulk: every masked tile of this room inside the
/// fifty-by-fifty grid, border ring included, each once, as its grid index
/// (`Geometry.indexOf`). What the [[atlas]] lays its grids from, twice per
/// projected room per tick (W15S26's answer is 937 tiles).
///
/// At the shipped margin the answer is the **shared** array laid at load:
/// a reader iterates it and never writes to it. Any other margin (a test's)
/// is walked afresh. A room the declaration names none of answers empty.
let maskedIndicesIn (margin: int) (room: string) : int[] =
    if margin = shippedMargin then
        match Map.tryFind room shippedMasks with
        | Some indices -> indices
        | None -> [||]
    else
        match centresIn room with
        | [] -> [||]
        | rocks -> walkMask margin rocks
