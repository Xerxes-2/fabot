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
    /// has an empty Seam band by construction and every cross-room price over
    /// it is `None` (ADR 0004). The implication runs **one way only**, and the
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
