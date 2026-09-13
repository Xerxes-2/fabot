/// The [[keeper margin]]: the ground a body of ours does not walk on in a
/// Source Keeper room (ADR 0060 decision 2).
///
/// This is the **one** place the colony edits a fact about the world before a
/// rule reads it, which is what ADR 0004 usually refuses, and the exception is
/// argued on the class rather than on convenience:
///
/// - **It is stationary.** A keeper is pinned by `keepers/pretick.js` to within
///   range 1 of its assigned source or mineral, the lairs and the rocks are
///   fixed for the life of the server, and there is no state here to go stale.
/// - **It is known before the room is ever seen.** The centres are read off the
///   API once and written down, exactly as an outpost's sources are (ADR 0041),
///   so the mask never waits on vision and never blinks. Deriving it from
///   vision needs a creep in the room, which needs a Task, which needs the room
///   in the projection — ADR 0057's rejected option, and its reason survives.
/// - **It is geometry and not a reaction.** A [[reach]] is derived each tick
///   from seen Threats, appears and vanishes with them, and is deliberately
///   never a change to the map (ADR 0033). This is the opposite on every one of
///   those axes, and that difference **is** the test for the class: a hostile
///   the map can answer before the tick begins is answered by the map; a
///   hostile that has to be reacted to is ADR 0033's.
///
/// **The hostile list is untouched; the ground is.** A keeper still enters
/// `World.factsOf`'s hostiles, still derives a Reach and is still recorded by
/// the [[raid log]] like any other — nothing is filtered out of any list. What
/// changes is that no walkable tile of ours lies inside that Reach, so the
/// answer [[flee]] owes is already given by the ground and no row is exempted
/// from anything. This is the precise sense in which it overrides #286's
/// reasoning for this **one** class of hostile and for no other.
///
/// The failure mode, and it is **not** the one ADR 0060 decision 2 stated. That
/// ADR wrote that a keeper leaving range 1 of its rock would take "a change in
/// the engine rather than a bad tick". It takes a **respawn**: a keeper is cast
/// on its lair with no `memory_sourceId`, adopts any source or mineral within
/// range **5** of where it stands, and then *walks* to range 1 of it
/// (`keepers/pretick.js`, read verbatim). Range 1 is the steady state and not
/// the invariant, and the margin below is derived from the steady state. Being
/// inside a centre's ball masks the keeper's own **tile**; it masks the
/// keeper's **Reach** only while the keeper is within 1 of a centre. Over
/// W15S26's own terrain, 24 of the tiles a keeper stands on between its lair
/// and its rock have unmasked walkable ground inside their Reach — a courier
/// out there is a creep inside a Reach after all, for the two to four ticks a
/// walk takes. It is not repairable by widening: past a margin of 7 the room
/// cannot be crossed at all (#327 carries the sweep, the table and the three
/// ways out). What the mask buys is therefore the **steady state**, which is
/// almost every tick and not every tick, and ADR 0033 is still the answer for
/// the rest — which is exactly why nothing here filters a keeper out of any
/// hostile list.
module Fabot.Core.Types.Keepers

/// The rocks a keeper is pinned to, per room name — its **lairs, its sources
/// and its mineral**, which is the whole of "the centres are the rocks and not
/// only the lairs" (ADR 0060 decision 2). `keepers/pretick.js` binds each
/// keeper to `memory_sourceId` and moves it to range 1 of that source or
/// mineral, never of its lair, so a mask around the lairs alone would leave
/// every source unmasked and the safe-crossing measurement does not cover that.
///
/// Keyed by **room name** and hung off no declaration, which is the other half
/// of the decision: two colonies that ever project W15S26 must mask it
/// identically, and a mask that lived on a declaration would let them disagree.
/// A room named here is masked wherever it is projected, by whoever projects
/// it.
///
/// The tiles are the engine's own, read off `/api/game/room-objects` once — the
/// day they were declared and not a tick later, terrain and lairs being fixed
/// for the life of the server. W15S26's three sources and its mineral are
/// checked against the committed capture rather than trusted
/// (`RoomSeamTests`, `rooms/W15S26.room`); the four **lairs** are not, because
/// `scripts/capture-room.mjs` keeps sources, controllers and minerals alone, so
/// those four tiles are still a hand-read fact and widening the capture is
/// #316's.
///
/// **The precondition a new room in this list has to meet, and it is not one a
/// flat tile list can state.** The kinds live in the comments beside the tiles,
/// so no test here can say "every lair is within the margin of the rock it
/// feeds" — and the mask's whole claim rests on the keeper being within 1 of a
/// *declared* tile. `keepers/pretick.js` casts a keeper on its lair, adopts any
/// source or mineral within range **5**, and walks it to range 1 of that; so
/// for the ticks of that walk the keeper is near neither, and the Reach it
/// derives out there reaches ground the margin left walkable. It is measured
/// and filed rather than guessed at: over W15S26, every lair adopts a rock 3 to
/// 5 tiles off, and 24 of the tiles on those four walks reach walkable ground
/// (#327, which carries the table and the three ways out — none of them a wider
/// margin, because past 7 the room cannot be crossed). A room declared here
/// whose lairs sit further from their rocks than W15S26's makes that window
/// wider, and nothing in this file will go red about it. Carrying the kind in
/// the data would let a test say *part* of it; it is deliberately not done
/// here, because the property a test could then check — lair within the margin
/// of its rock — is not the property that fails, and structure added for a
/// weaker check would read as if the stronger one held.
let centres: Map<string, Pos list> =
    Map.ofList
        [
            // W15S26, the Source Keeper room the chain from W15S28 to the
            // sector Reactor in W15S25 crosses (ADR 0060 decision 3), read at
            // tick 404,835 on 2026-09-13: four keeper lairs, three sources and
            // one mineral, in the API's own order.
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

/// One room's declared centres; the empty list for a room with none, which is
/// every room but the handful above (ADR 0004 — absence is an answer and never
/// a lookup that throws).
let centresIn (room: string) : Pos list =
    Map.tryFind room centres |> Option.defaultValue []

/// Whether a tile of this room is masked ground: within `margin` of a declared
/// centre, in the Chebyshev measure every range in this colony is taken in
/// (`Geometry.range`). The pointwise form, for a reader that holds its own
/// terrain and asks per tile — the route search's border rings (`World.linked`)
/// ask it forty-eight times a band.
let masked (margin: int) (room: string) (tile: Pos) : bool =
    centresIn room |> List.exists (fun centre -> range centre tile <= margin)

/// The same answer with the room resolved **once**, for a reader that asks it
/// about many tiles of one room: the route search walks a band forty-eight
/// tiles at a time and `masked` would take the declaration's lookup on every
/// one of them (`World.ringWalkable`). A room the declaration names none of
/// answers false without a comparison, which is every room but the handful
/// above and so is the case worth having (ADR 0004: the absence is the answer,
/// and it is answered once).
let maskIn (margin: int) (room: string) : Pos -> bool =
    match centresIn room with
    | [] -> fun _ -> false
    | centres -> fun tile -> centres |> List.exists (fun centre -> range centre tile <= margin)

/// The same rule in bulk: every masked tile of this room that is a coordinate
/// of the fifty-by-fifty grid, border ring included. What the [[atlas]] lays
/// its grids from, because a grid pass that asked `masked` per tile would ask
/// it 2,500 times for an answer that covers a few hundred. The two agree by
/// construction: `tilesWithin` is the Chebyshev ball `range` is the measure of.
let maskedTilesIn (margin: int) (room: string) : Pos list =
    centresIn room
    |> List.collect (tilesWithin margin)
    |> List.filter (fun tile ->
        tile.X >= 0
        && tile.X < Engine.roomSide
        && tile.Y >= 0
        && tile.Y < Engine.roomSide)
    |> List.distinct
