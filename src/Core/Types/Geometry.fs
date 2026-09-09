/// Position and distance. `Pos` is a tile of no particular room and `RoomPos`
/// the same tile once it is named (ADR 0041), plus the room-name arithmetic
/// that walks the world grid.
[<AutoOpen>]
module Fabot.Core.Types.Geometry

/// A tile of a **named** room: the coordinate a value carries once it leaves
/// the grid it indexes (ADR 0052 decision 2). Two rooms hold the same
/// fifty-by-fifty coordinates, so a bare `Pos` handed between functions is
/// joined to a room by convention alone — the convention that produced a
/// phantom [[post]] and a hostile in an [[outpost]] measured at range 0 from
/// home. Declared **before** `Pos` and never after it: F# resolves a bare `.X`
/// on an un-annotated value to the *last* record type declaring that field.
[<CustomEquality; CustomComparison>]
type RoomPos =
    {
        Room: string
        X: int
        Y: int
    }

    /// Hand-written for the reason `Pos` below carries: a `RoomPos` keys every
    /// Work Area, occupancy map and Move Intent candidate list, and Fable's
    /// generic walk over a record's fields is the single largest library cost
    /// in a tick.
    override this.Equals other =
        match other with
        | :? RoomPos as that -> this.X = that.X && this.Y = that.Y && this.Room = that.Room
        | _ -> false

    override this.GetHashCode() =
        (this.Room.GetHashCode() * 2503) + (this.X * 50) + this.Y

    interface System.IComparable with
        /// **Room, then X, then Y** — the field order the record's default
        /// comparison used, ordinal on the room name as .NET's own record
        /// comparison is, so no tie anywhere in the colony reorders.
        member this.CompareTo other =
            match other with
            | :? RoomPos as that ->
                let byRoom = System.String.CompareOrdinal(this.Room, that.Room)

                if byRoom <> 0 then byRoom
                elif this.X < that.X then -1
                elif this.X > that.X then 1
                elif this.Y < that.Y then -1
                elif this.Y > that.Y then 1
                else 0
            | _ -> 1

/// A tile coordinate inside a room. Kept as the **grid** coordinate (ADR 0052
/// decision 2): a key of `RoomLayer.Terrain`, of `Obstacles`, of the flood
/// arrays and of every Seat, Reach and Work-Area grid the Atlas lays per room.
[<CustomEquality; CustomComparison>]
type Pos =
    {
        X: int
        Y: int
    }

    /// Structural equality by hand. Fable's generic `equals` walks a record's
    /// fields through `compare`, and a `Pos` is a key of every grid the Atlas
    /// lays, so that walk is 25% of a tick (measured, `pair` scenario). Two
    /// ints compared inline answer the same question.
    override this.Equals other =
        match other with
        | :? Pos as that -> this.X = that.X && this.Y = that.Y
        | _ -> false

    /// The grid index, which is injective over a room's tiles.
    override this.GetHashCode() = this.X * Engine.roomSide + this.Y

    interface System.IComparable with
        /// **X then Y**, which is the field order the record's default
        /// comparison used and which every "ties by (X, Y) order" rule in the
        /// colony rests on (ADR 0011, ADR 0042, #244).
        member this.CompareTo other =
            match other with
            | :? Pos as that ->
                if this.X < that.X then -1
                elif this.X > that.X then 1
                elif this.Y < that.Y then -1
                elif this.Y > that.Y then 1
                else 0
            | _ -> 1

/// Screeps range: Chebyshev distance between two tiles of **one** room. The
/// one definition — the Atlas's geometry, the two hostile reflexes and the
/// Raid log's closest approach all measure with it. Takes grid coordinates;
/// `RoomPos.range` is the same measure for tiles that carry their own rooms.
let range (a: Pos) (b: Pos) = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

/// The Chebyshev ball of a tile: every grid coordinate within `radius` of it —
/// the neighbourhood `range` above is the measure of, written beside the metric
/// it belongs to rather than open-coded as a double `for` at each of the four
/// places that wanted one. **Unclamped**: a centre near a room edge yields
/// coordinates off the grid, and every caller drops those through the
/// membership test it was applying anyway, so clamping here would quietly
/// change what a doorstep or a Reach means at a border.
let tilesWithin (radius: int) (center: Pos) : Pos list =
    [
        for x in center.X - radius .. center.X + radius do
            for y in center.Y - radius .. center.Y + radius -> { X = x; Y = y }
    ]

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RoomPos =
    /// The grid coordinate, for indexing that room's own tables — always
    /// safe, at the moment a reader has decided which room's grid it reads.
    let pos (tile: RoomPos) : Pos = { X = tile.X; Y = tile.Y }

    /// A room's grid coordinate joined to that room — the conversion every
    /// Atlas query spells as it hands a tile out.
    let at (room: string) (tile: Pos) : RoomPos = { Room = room; X = tile.X; Y = tile.Y }

    /// The whole set of a room's grid tiles, joined to it.
    let setAt (room: string) (tiles: Set<Pos>) : Set<RoomPos> = tiles |> Set.map (at room)

    /// The tiles of one room out of a mixed set, back as grid
    /// coordinates: the read at the other end of `setAt`, for a grid or a
    /// flood that indexes one room.
    let inRoom (room: string) (tiles: Set<RoomPos>) : Set<Pos> =
        tiles |> Set.filter (fun tile -> tile.Room = room) |> Set.map pos

    /// The same narrowing as `inRoom`, answered as a **list** and not a second
    /// set — the shape every flood takes its goals in, and the hot one:
    /// building a `Set<Pos>` per ask would copy the area and pay a tree of
    /// comparisons for an answer the callers only index a grid array with. The
    /// order is the set's reversed, which every caller settles with a total
    /// key.
    let tilesIn (room: string) (tiles: Set<RoomPos>) : Pos list =
        ([], tiles)
        ||> Set.fold (fun acc tile -> if tile.Room = room then pos tile :: acc else acc)

    /// Chebyshev range between two tiles that carry their rooms, and **None**
    /// across a border (ADR 0052 decision 2). Not a large number and not an
    /// error: two rooms' coordinate systems are not one metric space, and
    /// every reader that decided it by accident decided "range 0" (#204).
    let range (a: RoomPos) (b: RoomPos) : int option =
        if a.Room = b.Room then
            Some(max (abs (a.X - b.X)) (abs (a.Y - b.Y)))
        else
            None

/// What a room's **name** says about where the room is, and nothing else it
/// says: the engine's own grammar, read here so that the two questions the
/// colony asks of a pair of names — which border they share, and whether they
/// share one at all — are one subtraction and not two rules (ADR 0041).
module RoomName =
    /// A room's place on the world grid, read off its name — `W12S28` is
    /// (-13, 28). West and North count outward from the origin, so they run
    /// negative (`W n` is x = -n-1, `N n` is y = -n-1) and East and South run
    /// straight up, which turns "are these two rooms neighbours, and across
    /// which border" into subtraction. None for a name outside the engine's
    /// grammar, which is unplaceable geometry like any other (ADR 0004).
    let private worldCoordsOf (roomName: string) : (int * int) option =
        let isDigit index =
            index < roomName.Length && roomName.[index] >= '0' && roomName.[index] <= '9'

        let rec endOfDigits index =
            if isDigit index then endOfDigits (index + 1) else index

        let number start stop =
            if stop <= start then
                None
            else
                let mutable value = 0

                for index in start .. stop - 1 do
                    value <- value * 10 + (int roomName.[index] - int '0')

                Some value

        // Outward from the origin is negative, towards it positive.
        let axis letter outward inward value =
            if letter = outward then Some(-value - 1)
            elif letter = inward then Some value
            else None

        let xEnd = endOfDigits 1
        let yEnd = endOfDigits (xEnd + 1)

        if yEnd <> roomName.Length then
            None
        else
            match number 1 xEnd, number (xEnd + 1) yEnd with
            | Some x, Some y ->
                match axis roomName.[0] 'W' 'E' x, axis roomName.[xEnd] 'N' 'S' y with
                | Some worldX, Some worldY -> Some(worldX, worldY)
                | _ -> None
            | _ -> None

    /// The name a world-grid position spells — `worldCoordsOf` read backwards,
    /// with the same convention: an outward axis counts from -1, so x = -13 is
    /// `W12`. Private, because a coordinate pair is this module's own currency
    /// and what leaves it is always a name.
    let private nameOfCoords (x: int) (y: int) : string =
        let axis outward inward value =
            if value < 0 then
                outward + string (-value - 1)
            else
                inward + string value

        axis "W" "E" x + axis "N" "S" y

    /// The four rooms a name grid puts next to this one, in a fixed order —
    /// north, east, south, west — whatever terrain has to say about them. The
    /// order is the route search's tie-break and nothing else: two chains of
    /// the same length are the same price to a walk that has not been priced
    /// yet, and a search that broke the tie on the heap's whim would answer a
    /// different room on two ticks that read the same world.
    let adjacent (roomName: string) : string list =
        match worldCoordsOf roomName with
        | Some(x, y) ->
            [ 0, -1; 1, 0; 0, 1; -1, 0 ]
            |> List.map (fun (dx, dy) -> nameOfCoords (x + dx) (y + dy))
        | None -> []

    /// The step from one room to another on that grid — the neighbour's world
    /// position minus this room's, which is what says *which* border they
    /// share. None where either name is outside the grammar.
    let offsetOf (fromRoom: string) (toRoom: string) : (int * int) option =
        match worldCoordsOf fromRoom, worldCoordsOf toRoom with
        | Some(hereX, hereY), Some(thereX, thereY) -> Some(thereX - hereX, thereY - hereY)
        | _ -> None

    /// Whether two rooms share a border: exactly one axis apart by one, which
    /// is the whole of what a [[seam]] can join (ADR 0041). Screeps has no
    /// diagonal exit, so a room a single axis step away is the only kind a
    /// creep reaches without crossing a third room — `Atlas.borderPairs` names
    /// tiles for those four offsets and for no other, so a pair this refuses
    /// has an empty Seam band by construction. That an empty band once meant an
    /// unpriceable pair is ADR 0058's business now and no longer this
    /// predicate's: a walk crosses a **chain** of Seams, so a pair with no band
    /// of its own may still be joined through the rooms between them, and what
    /// this answers is one Seam and never the walk. The implication runs
    /// **one way only**, and the
    /// difference is load-bearing: this reading is over names, so it can be
    /// asked of a declaration before any terrain is read; the Seam is over
    /// tiles, so a bordering pair whose shared column or row the engine walled
    /// end to end is a neighbour here and has no band there — W12S27's west
    /// column in `tests/Core.Tests/rooms/` is exactly that, and `AtlasTests`'
    /// "a walled border is a neighbour with no band" pins it. So this answers
    /// whether the declaration is *shaped* like one a Seam could join, never
    /// whether one does. A room is not its own neighbour, and a name outside
    /// the grammar neighbours nothing.
    let neighbouring (fromRoom: string) (toRoom: string) : bool =
        offsetOf fromRoom toRoom |> Option.exists (fun (dx, dy) -> abs dx + abs dy = 1)

    /// The fewest [[seam]]s a walk between two rooms could possibly cross: the
    /// grid distance between the names, one crossing per border (ADR 0058).
    /// A floor and never the route — terrain can only make a walk longer, and
    /// only `Atlas.route` says whether one exists at all — but a floor is what
    /// a hop budget is asked against, and it is answerable off the names
    /// alone, which is what lets a declaration be judged before any terrain is
    /// read. Zero for a room and itself. None outside the grammar.
    let hopsBetween (fromRoom: string) (toRoom: string) : int option =
        offsetOf fromRoom toRoom |> Option.map (fun (dx, dy) -> abs dx + abs dy)

    /// Every room a walk of the fewest possible hops could pass through, the
    /// two ends excluded: the interior of the name-grid rectangle the two
    /// names span (ADR 0058). A room in that rectangle lies on some monotone
    /// path between them and a room outside it lies on none, so this is the
    /// exact set — no smaller one covers every shortest chain, and a larger
    /// one projects a room no shortest walk can use.
    ///
    /// This is what decides which **transit rooms** enter the projection, and
    /// it is deliberately answered off the names: the route itself needs the
    /// rooms' terrain, the terrain needs them projected, and the circle is cut
    /// here, by the one question the names can answer on their own. A detour
    /// around a walled border therefore lies outside what this projects and is
    /// not found — the route is `None`, the declaration is refused loudly
    /// (`ColonyView.Refused`), and widening this set is what would buy it.
    let transitBetween (fromRoom: string) (toRoom: string) : string list =
        match worldCoordsOf fromRoom, worldCoordsOf toRoom with
        | Some(fromX, fromY), Some(toX, toY) ->
            [
                for x in min fromX toX .. max fromX toX do
                    for y in min fromY toY .. max fromY toY do
                        let name = nameOfCoords x y

                        if name <> fromRoom && name <> toRoom then
                            yield name
            ]
        | _ -> []

    /// The chain of rooms a walk crosses, ends included, or `None` where the
    /// hop budget or the terrain leaves none (ADR 0058): a breadth-first search
    /// over the name grid, `linked` deciding which of the four steps out of a
    /// room a creep can actually take, stopped at `maxHops` crossings.
    ///
    /// `linked` is the caller's because the two askers answer it differently
    /// and must not: the Atlas reads a Seam band off two rooms' border rings
    /// (`Atlas.route`), and a room the projection does not carry is joined to
    /// nothing — which is what keeps this search inside the rooms
    /// `transitBetween` put there rather than wandering the sector. Breadth
    /// first, so the chain that comes back crosses the fewest borders any
    /// chain could; `RoomName.adjacent`'s fixed order breaks the ties, so the
    /// answer is a function of the world and not of the search.
    let routeBy
        (linked: string -> string -> bool)
        (maxHops: int)
        (fromRoom: string)
        (toRoom: string)
        : string list option =
        if fromRoom = toRoom then
            Some [ fromRoom ]
        elif maxHops < 1 then
            None
        else
            // The chain is carried on the queue reversed, so extending it is a
            // cons: the rooms are at most `maxHops` and the reverse is paid
            // once, on the one chain that wins.
            let rec search (frontier: (string * string list) list) (seen: Set<string>) =
                match frontier with
                | [] -> None
                | _ ->
                    // The chain's length and not its hop count: a chain of
                    // n rooms crosses n-1 borders, and this guards *before*
                    // the frontier is expanded, so the step about to be taken
                    // is the one being budgeted for.
                    let chainLength = List.length (snd (List.head frontier))

                    if chainLength > maxHops then
                        None
                    else
                        // The goal first, and off one `linked` where expanding
                        // the whole frontier would pay for four apiece: a
                        // one-hop route is what every declaration in force asks
                        // for, and it is asked several times a tick by every
                        // reader of the scan set.
                        let arrived =
                            frontier
                            |> List.tryPick (fun (room, chain) ->
                                if List.contains toRoom (adjacent room) && linked room toRoom then
                                    Some(List.rev (toRoom :: chain))
                                else
                                    None)

                        match arrived with
                        | Some chain -> Some chain
                        | None ->

                            let steps =
                                [
                                    for room, chain in frontier do
                                        for next in adjacent room do
                                            if not (Set.contains next seen) && linked room next then
                                                yield next, next :: chain
                                ]

                            match steps |> List.tryFind (fun (room, _) -> room = toRoom) with
                            | Some(_, chain) -> Some(List.rev chain)
                            | None ->
                                // One room enters the frontier once, under the
                                // first chain that reached it: a second chain of
                                // the same length is a tie this search has already
                                // broken, and a longer one can only lose.
                                let fresh =
                                    steps
                                    |> List.fold
                                        (fun (kept, taken) (room, chain) ->
                                            if Set.contains room taken then
                                                kept, taken
                                            else
                                                (room, chain) :: kept, Set.add room taken)
                                        ([], seen)

                                search (List.rev (fst fresh)) (snd fresh)

            search [ fromRoom, [ fromRoom ] ] (Set.singleton fromRoom)

/// The border two rooms share, as tiles — the half of a [[seam]] that
/// `RoomName` answers over names alone (ADR 0041). Written here rather than
/// beside the [[atlas]]'s own grids because two readers ask it and they hold
/// their terrain differently: the Atlas over the ring grids it lays per tick,
/// and the scan set over the [[world]]'s own border maps, before any grid
/// exists (ADR 0058). One definition, so the two cannot disagree about which
/// pair of rooms a creep can walk between.
module Seam =
    /// The far exit row and column of a room — index 49, the outer of the two
    /// the projection's ground stops short of (ADR 0036).
    let exitEdge = Engine.roomSide - 1

    /// The tile pairs the engine joins across the border two rooms share,
    /// before terrain has a say: this room's exit tile beside the tile a creep
    /// stepping onto it lands on, the same coordinate on the opposite row or
    /// column. `offset` is the neighbour's world position minus this room's
    /// (`RoomName.offsetOf`), so only the four unit steps name a shared border
    /// — which is the tile half of the rule `RoomName.neighbouring` states over
    /// the names alone. The four corner tiles are left out of every row and
    /// column: a corner lies on two borders at once, and the engine makes at
    /// most one landing.
    let pairsAcross offset : (Pos * Pos) list =
        let alongEdge = [ 1 .. exitEdge - 1 ]

        match offset with
        | 0, -1 -> [ for x in alongEdge -> { X = x; Y = 0 }, { X = x; Y = exitEdge } ]
        | 0, 1 -> [ for x in alongEdge -> { X = x; Y = exitEdge }, { X = x; Y = 0 } ]
        | -1, 0 -> [ for y in alongEdge -> { X = 0; Y = y }, { X = exitEdge; Y = y } ]
        | 1, 0 -> [ for y in alongEdge -> { X = exitEdge; Y = y }, { X = 0; Y = y } ]
        | _ -> []

    /// The Seam band joining two rooms: the passable exit-tile pairs, each the
    /// first room's border tile beside the tile it lands a creep on in the
    /// second. `walkable` is the caller's reading of one room's border ring —
    /// a tile the ring carries and whose terrain is not wall. Deterministic
    /// (X, Y) order, total (ADR 0004).
    let bandBy
        (nearWalkable: Pos -> bool)
        (farWalkable: Pos -> bool)
        (fromRoom: string)
        (toRoom: string)
        : (Pos * Pos) list =
        match RoomName.offsetOf fromRoom toRoom with
        | Some offset ->
            pairsAcross offset
            |> List.filter (fun (here, there) -> nearWalkable here && farWalkable there)
        | None -> []

    /// Whether *any* crossing joins the two rooms — the band's existence
    /// without the band. What a route search asks at every edge it considers
    /// (ADR 0058), and it short-circuits on the first passable pair, where
    /// `bandBy` would build all forty-eight and then be asked if the list is
    /// empty.
    let joinedBy
        (nearWalkable: Pos -> bool)
        (farWalkable: Pos -> bool)
        (fromRoom: string)
        (toRoom: string)
        : bool =
        match RoomName.offsetOf fromRoom toRoom with
        | Some offset ->
            pairsAcross offset
            |> List.exists (fun (here, there) -> nearWalkable here && farWalkable there)
        | None -> false
