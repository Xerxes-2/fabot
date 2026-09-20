/// Position and distance. `Pos` is a tile of no particular room and `RoomPos`
/// the same tile once it is named, plus the room-name arithmetic that walks
/// the world grid.
[<AutoOpen>]
module Fabot.Core.Types.Geometry

/// A tile of a **named** room: the coordinate a value carries once it leaves
/// the grid it indexes (ADR-0052). Two rooms hold the same fifty-by-fifty
/// coordinates, so a bare `Pos` handed between functions is joined to a room
/// by convention alone. Declared **before** `Pos` and never after it: F#
/// resolves a bare `.X` on an un-annotated value to the *last* record type
/// declaring that field.
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

/// A tile coordinate inside a room. Kept as the **grid** coordinate: a key of
/// `RoomLayer.Terrain`, of `Obstacles`, of the flood arrays and of every Seat,
/// Reach and Work-Area grid the Atlas lays per room.
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
        /// colony rests on.
        member this.CompareTo other =
            match other with
            | :? Pos as that ->
                if this.X < that.X then -1
                elif this.X > that.X then 1
                elif this.Y < that.Y then -1
                elif this.Y > that.Y then 1
                else 0
            | _ -> 1

/// The grid index of a tile, and its inverse — one room's fifty-by-fifty laid
/// out as `x * roomSide + y`. This is the stride every flat per-room array in
/// the colony is indexed by (`Grid`, the Atlas's grids, `TerrainGrid`), and
/// `Pos.GetHashCode` is this same expression, which is what makes it
/// injective over a room.
///
/// `inline`, all four: these are the innermost expressions of the tick.
let internal tileCount = Engine.roomSide * Engine.roomSide

let inline internal indexOf (pos: Pos) = pos.X * Engine.roomSide + pos.Y

let inline internal posAt (index: int) =
    {
        X = index / Engine.roomSide
        Y = index % Engine.roomSide
    }

/// Whether a tile is one of the room's own fifty-by-fifty — the guard every
/// flat-array read passes through, because a `Pos` off the grid indexes off
/// the array: under Fable that reads `undefined`, which a weight comparison
/// would call walkable and a terrain match would call absent in one case and
/// crash on in another, while .NET throws outright.
let inline internal inGrid (tile: Pos) =
    tile.X >= 0
    && tile.X < Engine.roomSide
    && tile.Y >= 0
    && tile.Y < Engine.roomSide

/// Screeps range: Chebyshev distance between two tiles of **one** room.
/// Takes grid coordinates; `RoomPos.range` is the same measure for tiles
/// that carry their own rooms.
let range (a: Pos) (b: Pos) = max (abs (a.X - b.X)) (abs (a.Y - b.Y))

/// The Chebyshev ball of a tile: every grid coordinate within `radius` of it.
/// **Unclamped**: a centre near a room edge yields coordinates off the grid,
/// and every caller drops those through the membership test it was applying
/// anyway, so clamping here would quietly change what a doorstep or a Reach
/// means at a border.
let tilesWithin (radius: int) (center: Pos) : Pos list =
    [
        for x in center.X - radius .. center.X + radius do
            for y in center.Y - radius .. center.Y + radius -> { X = x; Y = y }
    ]

/// The eight tiles touching this one, in (X, Y) order — the order every answer
/// derived from them is listed in. **Unclamped**, like `tilesWithin`. It lives
/// here rather than beside the Atlas's grids because the Seam needs it too:
/// "beside" has to be the same eight tiles the flood steps through or the band
/// and the price would be free to disagree about a diagonal.
let internal neighbours (pos: Pos) : Pos list =
    let x = pos.X
    let y = pos.Y

    [
        { X = x - 1; Y = y - 1 }
        { X = x - 1; Y = y }
        { X = x - 1; Y = y + 1 }
        { X = x; Y = y - 1 }
        { X = x; Y = y + 1 }
        { X = x + 1; Y = y - 1 }
        { X = x + 1; Y = y }
        { X = x + 1; Y = y + 1 }
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
    /// across a border. Not a large number and not an error: two rooms'
    /// coordinate systems are not one metric space, and every reader that decided
    /// it by accident decided "range 0" (#204).
    let range (a: RoomPos) (b: RoomPos) : int option =
        if a.Room = b.Room then
            Some(range (pos a) (pos b))
        else
            None

/// What a room's **name** says about where the room is: the engine's own
/// grammar, read here so that which border two rooms share, and whether they
/// share one at all, are one subtraction and not two rules.
module RoomName =
    /// A room's place on the world grid, read off its name — `W12S28` is
    /// (-13, 28). West and North count outward from the origin, so they run
    /// negative (`W n` is x = -n-1, `N n` is y = -n-1) and East and South run
    /// straight up. None for a name outside the engine's grammar.
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
    /// order is the order the route search *offers* its chains in; a search that
    /// broke the remaining ties on the heap's whim would offer them in a
    /// different order on two ticks that read the same world.
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

    /// Whether two rooms share a border: exactly one axis apart by one. Screeps
    /// has no diagonal exit, so `Atlas.borderPairs` names tiles for those four
    /// offsets and no other. The implication runs **one way only**: this reading
    /// is over names, so it can be asked of a declaration before any terrain is
    /// read; the Seam is over tiles, so a bordering pair whose shared column the
    /// engine walled end to end is a neighbour here and has no band there
    /// (W12S27's west column in `tests/Core.Tests/rooms/`). A room is not its own
    /// neighbour, and a name outside the grammar neighbours nothing.
    let neighbouring (fromRoom: string) (toRoom: string) : bool =
        offsetOf fromRoom toRoom |> Option.exists (fun (dx, dy) -> abs dx + abs dy = 1)

    /// The fewest Seams a walk between two rooms could possibly cross: the grid
    /// distance between the names, one crossing per border. A floor and never the
    /// route — only `Atlas.route` says whether one exists — but answerable off
    /// the names alone. Zero for a room and itself. None outside the grammar.
    let hopsBetween (fromRoom: string) (toRoom: string) : int option =
        offsetOf fromRoom toRoom |> Option.map (fun (dx, dy) -> abs dx + abs dy)

    /// Every room a walk of the fewest possible hops could pass through, the two
    /// ends excluded: the interior of the name-grid rectangle the two names span
    /// (ADR-0058). A room in that rectangle lies on some monotone path between
    /// them and a room outside it lies on none, so this is the exact set.
    ///
    /// This is what decides which **transit rooms** enter the projection, and it
    /// is answered off the names because the route needs the rooms' terrain, the
    /// terrain needs them projected, and the circle is cut here. A detour around
    /// a walled border therefore lies outside what this projects and is not found
    /// (`ColonyView.Refused`); widening this set is what would buy it.
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

    /// **Every** chain of rooms a walk of the fewest possible crossings could
    /// take, ends included, and an empty list where the hop budget or the terrain
    /// leaves none (ADR-0059): a breadth-first search over the name grid,
    /// `linked` deciding which of the four steps out of a room a creep can take,
    /// stopped at `maxHops` crossings.
    ///
    /// `linked` is the caller's because the two askers answer it differently:
    /// the Atlas reads a Seam band off two rooms' border rings (`Atlas.routes`),
    /// and a room the projection does not carry is joined to nothing — which
    /// keeps this search inside the rooms `transitBetween` put there.
    ///
    /// All chains and not the first: at two hops there is more than one
    /// shortest chain and they are not the same walk (measured at up to +91% on
    /// rooms this colony works). The caller prices them; the fixed order here
    /// decides only the order the candidates come in, so a tie on the price falls
    /// the way it always fell.
    let routesBy
        (linked: string -> string -> bool)
        (maxHops: int)
        (fromRoom: string)
        (toRoom: string)
        : string list list =
        if fromRoom = toRoom then
            [ [ fromRoom ] ]
        elif maxHops < 1 then
            []
        else
            // The chains are carried on the queue reversed, so extending one is
            // a cons: the rooms are at most `maxHops` and the reverse is paid
            // once, on the chains that arrive.
            let rec search (frontier: (string * string list) list) (seen: Set<string>) =
                match frontier with
                | [] -> []
                | _ ->
                    // The chain's length and not its hop count: a chain of
                    // n rooms crosses n-1 borders, and this guards *before*
                    // the frontier is expanded. Every entry of a frontier
                    // is the same length, so the first answers for all.
                    let chainLength = List.length (snd (List.head frontier))

                    if chainLength > maxHops then
                        []
                    else
                        // The goal is tested before the frontier is expanded:
                        // one `linked` per room where expanding pays for four.
                        // A layer that reaches the goal at all is the last layer
                        // there is, so the arrivals are the whole answer. Per
                        // frontier *entry* and not per room: a room two chains
                        // reach stands in the frontier twice and is asked twice;
                        // at three hops it is not worth a set to dedupe.
                        let arrived =
                            frontier
                            |> List.choose (fun (room, chain) ->
                                if List.contains toRoom (adjacent room) && linked room toRoom then
                                    Some(List.rev (toRoom :: chain))
                                else
                                    None)

                        match arrived with
                        | _ :: _ -> arrived
                        | [] ->
                            // A room may enter the frontier under **several** chains
                            // of the same length; what it may not enter under is a
                            // *longer* one, which `seen` forbids, so the frontier
                            // stays one breadth and the chains stay simple.
                            let steps =
                                [
                                    for room, chain in frontier do
                                        for next in adjacent room do
                                            if not (Set.contains next seen) && linked room next then
                                                yield next, next :: chain
                                ]

                            search
                                steps
                                (steps |> List.fold (fun taken (room, _) -> Set.add room taken) seen)

            search [ fromRoom, [ fromRoom ] ] (Set.singleton fromRoom)

    /// The first of those chains, under `adjacent`'s own order. No production
    /// caller today: `AtlasSeamTests` pins that the head of `routesBy` is this
    /// answer, which is what holds the binding in place. Retire it when that
    /// contract has a better home.
    let routeBy
        (linked: string -> string -> bool)
        (maxHops: int)
        (fromRoom: string)
        (toRoom: string)
        : string list option =
        routesBy linked maxHops fromRoom toRoom |> List.tryHead

/// The border two rooms share, as tiles — the half of a Seam that `RoomName`
/// answers over names alone. Written here rather than beside the Atlas's own
/// grids because two readers ask it and hold their terrain differently: the
/// Atlas over the ring grids it lays per tick, and the scan set over the
/// world's own border maps, before any grid exists. A band is a fact about
/// two rings **and the far room's ground** (`landsOnGround`).
module Seam =
    /// The far exit row and column of a room — index 49, the outer of the two
    /// the projection's ground stops short of.
    let exitEdge = Engine.roomSide - 1

    /// The tile pairs the engine joins across the border two rooms share,
    /// before terrain has a say: this room's exit tile beside the tile a creep
    /// stepping onto it lands on. `offset` is the neighbour's world position
    /// minus this room's (`RoomName.offsetOf`), so only the four unit steps name
    /// a shared border. The four corner tiles are left out: a corner lies on two
    /// borders at once, and the engine makes at most one landing.
    /// The four lists are built once: nothing about a border depends on the tick,
    /// and the callers ask per room pair per tick (#365 found the keeper mask
    /// rebuilding a constant the same way). Private, so the only way to reach one
    /// is through the `offset` match below.
    let private alongEdge = [ 1 .. exitEdge - 1 ]

    let private northPairs =
        [ for x in alongEdge -> { X = x; Y = 0 }, { X = x; Y = exitEdge } ]

    let private southPairs =
        [ for x in alongEdge -> { X = x; Y = exitEdge }, { X = x; Y = 0 } ]

    let private westPairs =
        [ for y in alongEdge -> { X = 0; Y = y }, { X = exitEdge; Y = y } ]

    let private eastPairs =
        [ for y in alongEdge -> { X = exitEdge; Y = y }, { X = 0; Y = y } ]

    let pairsAcross offset : (Pos * Pos) list =
        match offset with
        | 0, -1 -> northPairs
        | 0, 1 -> southPairs
        | -1, 0 -> westPairs
        | 1, 0 -> eastPairs
        | _ -> []

    /// ADR-0062
    /// Whether a body the engine puts down on a landing tile has anywhere to go:
    /// one tile of the far room's own **ground** beside it. The border ring is
    /// not ground, so a landing with no ground beside it is a tile a body arrives
    /// on and never leaves.
    ///
    /// `farGround` is the caller's reading of the far room's ground. The two
    /// readings the band is built on are the Atlas's raw terrain grid and the
    /// world's terrain map, each with the keeper margin taken off and neither
    /// carrying a structure; `Atlas.stepTowardRoom` hands in the **walking** grid
    /// instead, being the one mover with no far leg to drop a built-over landing
    /// for it (#317). Diagonals count, because the engine lets a creep step off
    /// its landing tile diagonally.
    let landsOnGround (farGround: Pos -> bool) (landing: Pos) : bool =
        neighbours landing |> List.exists farGround

    /// The Seam band joining two rooms: the passable exit-tile pairs, each the
    /// first room's border tile beside the tile it lands a creep on in the
    /// second. `nearWalkable` and `farWalkable` are the caller's reading of one
    /// room's border ring, and `farGround` its reading of the far room's ground.
    /// Deterministic (X, Y) order, total.
    let bandBy
        (nearWalkable: Pos -> bool)
        (farWalkable: Pos -> bool)
        (farGround: Pos -> bool)
        (fromRoom: string)
        (toRoom: string)
        : (Pos * Pos) list =
        match RoomName.offsetOf fromRoom toRoom with
        | Some offset ->
            pairsAcross offset
            |> List.filter (fun (here, there) ->
                nearWalkable here && farWalkable there && landsOnGround farGround there)
        | None -> []

    /// Whether *any* crossing joins the two rooms — the band's existence without
    /// the band, short-circuiting on the first passable pair. The ring tests run
    /// before the ground one: eight lookups are paid only for a pair both rings
    /// already passed, which on a walled border is none of the forty-eight.
    let joinedBy
        (nearWalkable: Pos -> bool)
        (farWalkable: Pos -> bool)
        (farGround: Pos -> bool)
        (fromRoom: string)
        (toRoom: string)
        : bool =
        match RoomName.offsetOf fromRoom toRoom with
        | Some offset ->
            pairsAcross offset
            |> List.exists (fun (here, there) ->
                nearWalkable here && farWalkable there && landsOnGround farGround there)
        | None -> false
