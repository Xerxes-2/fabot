/// ADR-0088
/// Arrival time: by tile, the ticks until a hostile can strike it, and the
/// ticks until one of our bodies can stand on it, both timed by fatigue and
/// distance alone. One integer bucket-queue flood a hostile class a room,
/// each body seeded with the fatigue it waits out; ours one a body, on demand.
module Fabot.Core.Arrival

open Fabot.Core.Types
open Fabot.Core.Grid

/// No body ever gets there.
let private never = System.Int32.MaxValue

/// One of our bodies' arrival over its room (`ofBody`): by tile index, the
/// tick it can stand on the tile, and the tick it can step off it.
type BodyArrival =
    {
        Room: string
        Ticks: int[]
        Leaves: int[]
    }

/// The whole ticks a tile takes a gait on each ground; None with no MOVE.
let paceOf (gait: Gait) : Pace option =
    match
        Gait.ticks Gait.road gait, Gait.ticks Gait.plain gait, Gait.ticks Engine.swampWeight gait
    with
    | Some road, Some plain, Some swamp ->
        Some
            {
                Road = road
                Plain = plain
                Swamp = swamp
            }
    | _ -> None

/// One body of ours' gait as the engine walks it (#524): its live MOVE
/// against the weight of every part but MOVE and empty CARRY
/// (`fatigueFactorOf`), and of every part shot off, which the engine still
/// weighs. A part shot off reads as weight even where it was a MOVE, the
/// slower and so the safe direction.
let gaitOf (creep: CreepInfo) : Gait =
    let factor = fatigueFactorOf creep
    let live = creep.Body |> Map.fold (fun total _ count -> total + count) 0

    {
        Weight = factor.FatigueParts + max 0 (creep.Hits.HitsMax / Engine.partHits - live)
        Moves = factor.MoveParts
    }

/// The ticks a body waits its fatigue out before it may step: each live MOVE
/// pays two a tick.
let waitOf (fatigue: int) (gait: Gait) : int =
    if gait.Moves <= 0 || fatigue <= 0 then
        0
    else
        (fatigue + 2 * gait.Moves - 1) / (2 * gait.Moves)

/// A bucket's last entry, taken off it: a bare `pop` under Fable, where
/// `RemoveAt` is a splice.
[<Fable.Core.Emit("$0.pop()")>]
let private pop (bucket: ResizeArray<int>) : int =
    let last = bucket.[bucket.Count - 1]
    bucket.RemoveAt(bucket.Count - 1)
    last

/// A tile's ticks by its step weight (road 1, plain 2, swamp 10).
let private stepTicks (pace: Pace) : int[] =
    Array.init (Engine.swampWeight + 1) (fun weight ->
        if weight = Gait.road then pace.Road
        elif weight = Engine.swampWeight then pace.Swamp
        else pace.Plain)

/// One multi-source flood (Dial's buckets, keyed by the tick a body can step
/// off a tile). Each seed is a tile, the tick a body is on it and the tick it
/// can leave. A body that leaves a tile at `t` stands on the next at `t + 1`
/// and leaves it once that tile's ticks are paid, but for a step from inside
/// onto the ring, which the engine charges no fatigue (`movement.js`). A body
/// that crosses exits walks on from a ring tile only as a seed (it landed
/// there), the engine carrying it over the border the tick it steps on; one
/// that does not (`walksRing`: an Invader's, a keeper's) walks the ring.
/// Answers the arrival and the departure, by tile.
let private floodWithDepartures
    (ground: int[])
    (pace: Pace option)
    (walksRing: bool)
    (seeds: (int * int * int) list)
    : int[] * int[] =
    let arrive = Array.create tileCount never
    let depart = Array.create tileCount never
    let seeds = seeds |> List.sortBy (fun (_, _, leaves) -> leaves) |> Array.ofList

    for tile, on, leaves in seeds do
        if on < arrive.[tile] then
            arrive.[tile] <- on

        if leaves < depart.[tile] then
            depart.[tile] <- leaves

    match pace with
    | None -> ()
    | Some pace when seeds.Length > 0 ->
        let ticks = stepTicks pace
        let span = max pace.Road (max pace.Plain pace.Swamp) + 1
        let buckets = Array.init span (fun _ -> ResizeArray<int>())
        let edge = Engine.roomSide - 1
        let mutable pending = 0
        let mutable next = 0

        let leavesOf index =
            let _, _, leaves = seeds.[index] in leaves

        let mutable now = leavesOf 0

        while pending > 0 || next < seeds.Length do
            if pending = 0 && leavesOf next > now then
                now <- leavesOf next

            while next < seeds.Length && leavesOf next = now do
                let tile, _, _ = seeds.[next]

                if at tile depart = now then
                    buckets.[now % span].Add tile
                    pending <- pending + 1

                next <- next + 1

            let bucket = buckets.[now % span]

            if bucket.Count = 0 then
                now <- now + 1
            else
                let tile = pop bucket
                pending <- pending - 1

                if at tile depart = now then
                    let x = tile / Engine.roomSide
                    let y = tile % Engine.roomSide
                    let inside = x > 0 && y > 0 && x < edge && y < edge
                    let on = now + 1
                    let left = if x > 0 then x - 1 else 0
                    let right = if x < edge then x + 1 else edge
                    let top = if y > 0 then y - 1 else 0
                    let bottom = if y < edge then y + 1 else edge

                    for nx in left..right do
                        for ny in top..bottom do
                            let n = nx * Engine.roomSide + ny
                            let weight = at n ground

                            if weight > 0 && n <> tile then
                                if on < at n arrive then
                                    setAt n arrive on

                                let ring = nx = 0 || ny = 0 || nx = edge || ny = edge

                                let leaves = if ring && inside then on else on + at weight ticks - 1

                                if leaves < at n depart then
                                    setAt n depart leaves

                                    if walksRing || not ring then
                                        buckets.[leaves % span].Add n
                                        pending <- pending + 1
    | Some _ -> ()

    arrive, depart

/// `floodWithDepartures`' arrival alone.
let private flood ground pace walksRing seeds =
    fst (floodWithDepartures ground pace walksRing seeds)

/// The soonest of each tile's square of `strike` about it (Chebyshev): what a
/// weapon of that range strikes from where the body can stand. Separable,
/// along each axis in turn.
let private dilate (strike: int) (arrive: int[]) : int[] =
    let side = Engine.roomSide
    let edge = side - 1

    // Along a line of tiles `stride` apart from `first`, each tile the
    // soonest within `strike` of it on the line.
    let along (from: int[]) (into: int[]) (first: int) (stride: int) =
        for i in 0..edge do
            let low = if i > strike then i - strike else 0
            let high = if i + strike < edge then i + strike else edge
            let mutable soonest = never

            for k in low..high do
                let ticks = at (first + k * stride) from

                if ticks < soonest then
                    soonest <- ticks

            setAt (first + i * stride) into soonest

    if strike <= 0 then
        Array.copy arrive
    else
        let down = Array.create tileCount never
        let both = Array.create tileCount never

        for x in 0..edge do
            along arrive down (x * side) 1

        for y in 0..edge do
            along down both y side

        both

/// Each class a hostile among `side` is in, by the parts it fights and walks
/// with in #531's window (`HostileInfo.effectiveParts`), with the ticks its
/// fatigue holds it: one class a weapon, a melee's and a shooter's apart.
let private classesOf
    (side: HostileInfo list)
    (fatigue: Map<string, int>)
    (hostile: HostileInfo)
    : (HostileClass * int) list =
    let parts = HostileInfo.effectiveParts side hostile
    let gait = HostileInfo.gait side hostile
    let pace = paceOf gait

    let wait = waitOf (Map.tryFind hostile.Id fatigue |> Option.defaultValue 0) gait

    [
        if List.exists (samePart Attack) parts then
            {
                Strike = Engine.meleeRange
                Pace = pace
            },
            wait
        if List.exists (samePart RangedAttack) parts then
            {
                Strike = Engine.rangedRange
                Pace = pace
            },
            wait
    ]

/// The hostiles a room's maps are timed for: every one not an ally's.
let private threatsIn (world: WorldView) (room: string) : (HostileInfo * HostileClass * int) list =
    match Map.tryFind room world.Rooms with
    | None -> []
    | Some seen ->
        seen.Hostiles
        |> List.filter (fun hostile -> not (Colony.isAlly hostile.Owner))
        |> List.collect (fun hostile ->
            classesOf seen.Hostiles seen.HostileFatigue hostile
            |> List.map (fun (cls, wait) -> hostile, cls, wait))

/// A room's hostile towers' class: a weapon whose range is the room, on no
/// legs.
let towerClass =
    {
        Strike = Engine.roomSide
        Pace = None
    }

/// The pace no unseen body beats: a tile a tick on any ground.
let private quickest = Some { Road = 1; Plain = 1; Swamp = 1 }

/// The exit pairs a body crosses from a neighbour into a room by, read off
/// both rooms' hostile ground (`Atlas.arrivalGroundIn`, the ring laid in):
/// passable on both sides, or on this side alone where the projection carries
/// no ground for the neighbour.
let private crossings
    (ground: int[])
    (across: int[] option)
    (neighbour: string)
    (room: string)
    : (Pos * Pos) list =
    RoomName.offsetOf neighbour room
    |> Option.map Seam.pairsAcross
    |> Option.defaultValue []
    |> List.filter (fun (exit, landing) ->
        at (indexOf landing) ground > 0
        && (match across with
            | Some far -> at (indexOf exit) far > 0
            | None -> true))

/// A room's floods, shared through the world's table where it is laid
/// (`WorldView.ArrivalGround`), keyed by everything they read: the room's
/// hostile ground and each neighbour's, so whichever colony asks first, the
/// floods are the same. None for a room the projection does not carry.
let rec private roomOf (atlas: Atlas.Atlas) (world: WorldView) (room: string) : RoomArrival option =
    Atlas.arrivalGroundIn atlas room true
    |> Option.map (fun ground ->
        let around =
            RoomName.adjacent room
            |> List.map (fun neighbour -> neighbour, Atlas.arrivalGroundIn atlas neighbour true)

        let build () =
            {
                Own =
                    lazy
                        (threatsIn world room
                         |> List.filter (fun (hostile, cls, _) ->
                             HostileInfo.crossesExits hostile && Option.isSome cls.Pace)
                         |> List.groupBy (fun (_, cls, _) -> cls)
                         |> List.map (fun (cls, members) ->
                             {
                                 Class = cls
                                 Members = members |> List.map (fun (hostile, _, _) -> hostile.Id)
                                 Arrive =
                                     members
                                     |> List.map (fun (hostile, _, wait) ->
                                         indexOf (RoomPos.pos hostile.Pos), 0, wait)
                                     |> flood ground cls.Pace false
                             }))
                Maps = lazy (mapsOf atlas world room ground around)
            }

        match world.ArrivalGround with
        | Some share -> share room (Some ground :: List.map snd around) build
        | None -> build ())

/// A room's maps: its own hostiles and what its neighbours carry across, one
/// flood a class dilated by its strike, and the risk an unseen neighbour of
/// remembered danger (`WorldView.Danger`) lands on its exits until it is seen
/// again (Decided 2).
and private mapsOf
    (atlas: Atlas.Atlas)
    (world: WorldView)
    (room: string)
    (ground: int[])
    (around: (string * int[] option) list)
    : ArrivalMaps =
    // Each source of a class's seeds: the class, the hostiles it names, the
    // tiles they stand on or land on (each with the tick it is on the tile
    // and the tick it may leave), and whether they walk the ring.
    let own =
        threatsIn world room
        |> List.map (fun (hostile, cls, wait) ->
            cls,
            [ hostile.Id ],
            [ indexOf (RoomPos.pos hostile.Pos), 0, wait ],
            not (HostileInfo.crossesExits hostile))

    // A seen neighbour's own crossing hostiles, landing on this room's exits
    // the tick they step onto the neighbour's. A step from inside onto the
    // ring costs no fatigue, so a body leaves its landing the tick it lands.
    let carriedFrom (neighbour, across) =
        let band = crossings ground across neighbour room

        match across |> Option.bind (fun _ -> roomOf atlas world neighbour) with
        | Some floods ->
            floods.Own.Force()
            |> List.map (fun f ->
                f.Class,
                f.Members,
                band
                |> List.choose (fun (exit, landing) ->
                    let on = f.Arrive.[indexOf exit]
                    if on < never then Some(indexOf landing, on, on) else None),
                false)
        | None ->
            // No ground there to flood: the fewest ticks its pace allows over
            // the tiles to each exit, the road's.
            threatsIn world neighbour
            |> List.filter (fun (hostile, _, _) -> HostileInfo.crossesExits hostile)
            |> List.choose (fun (hostile, cls, wait) ->
                cls.Pace
                |> Option.map (fun pace ->
                    cls,
                    [ hostile.Id ],
                    band
                    |> List.map (fun (exit, landing) ->
                        let tiles = range (RoomPos.pos hostile.Pos) exit
                        let on = if tiles = 0 then 0 else wait + 1 + (tiles - 1) * pace.Road
                        indexOf landing, on, on),
                    false))

    let carried =
        around
        |> List.filter (fun (neighbour, _) -> WorldView.sees world neighbour)
        |> List.collect carriedFrom

    let unseen =
        around
        |> List.filter (fun (neighbour, _) ->
            not (WorldView.sees world neighbour) && Set.contains neighbour world.Danger)
        |> List.collect (fun (neighbour, across) ->
            crossings ground across neighbour room
            |> List.map (fun (_, landing) -> indexOf landing, 0, 0))

    // One flood for the bodies that walk the ring and one for those that
    // cross it, the soonest of the two.
    let floodOf (cls: HostileClass) sources =
        [ false; true ]
        |> List.choose (fun walksRing ->
            match
                sources
                |> List.filter (fun (_, _, _, walks) -> walks = walksRing)
                |> List.collect (fun (_, _, seeds, _) -> seeds)
            with
            | [] -> None
            | seeds -> Some(flood ground cls.Pace walksRing seeds))
        |> List.reduce (Array.map2 min)

    // The room's hostile towers (#566): one class striking every tile of it
    // now, which nothing carries across a border.
    let towers =
        match Map.tryFind room world.Rooms |> Option.map (fun seen -> seen.Towers) with
        | Some(_ :: _ as towers) ->
            [
                {
                    Class = towerClass
                    Members = towers |> List.map fst
                    Ticks = Array.create tileCount 0
                }
            ]
        | _ -> []

    {
        Classes =
            (own @ carried
             |> List.groupBy (fun (cls, _, _, _) -> cls)
             |> List.filter (fun (_, sources) ->
                 sources |> List.exists (fun (_, _, seeds, _) -> not (List.isEmpty seeds)))
             |> List.map (fun (cls, sources) ->
                 {
                     Class = cls
                     Members = sources |> List.collect (fun (_, ids, _, _) -> ids) |> List.distinct
                     Ticks = dilate cls.Strike (floodOf cls sources)
                 }))
            @ towers
        Unseen =
            match unseen with
            | [] -> None
            | seeds -> Some(dilate Engine.rangedRange (flood ground quickest false seeds))
    }

/// A room's hostile maps, built once a tick a room where the world's table is
/// laid; none for a room the projection does not carry.
let inRoom (atlas: Atlas.Atlas) (world: WorldView) (room: string) : ArrivalMaps =
    match roomOf atlas world room with
    | Some floods -> floods.Maps.Force()
    | None -> { Classes = []; Unseen = None }

/// One map's ticks at a tile; None where nothing ever gets there.
let ticksAt (ticks: int[]) (tile: Pos) : int option =
    if inGrid tile && ticks.[indexOf tile] < never then
        Some ticks.[indexOf tile]
    else
        None

/// The soonest any class, or an unseen neighbour's risk, strikes a tile.
let soonestAt (maps: ArrivalMaps) (tile: Pos) : int option =
    (Option.toList maps.Unseen @ (maps.Classes |> List.map (fun map -> map.Ticks)))
    |> List.choose (fun ticks -> ticksAt ticks tile)
    |> function
        | [] -> None
        | ticks -> Some(List.min ticks)

/// One member of one class of a room's maps, priced: its class's ticks, where
/// it stands, how far its weapon strikes and what it lands a tick on a tile.
type Striker =
    {
        Ticks: int[]
        At: RoomPos
        Strike: int
        Power: Pos -> int
    }

/// Each member of a room's classes as a `Striker` (Decided 3's price): a
/// creep at its live weapon of that class in #531's window
/// (`HostileInfo.effectivePower`), a tower at its falloff from the tile. The
/// members are read off the world's rooms, this one's and its neighbours'.
let strikersOf (world: WorldView) (room: string) (maps: ArrivalMaps) : Striker list =
    let seen =
        room :: RoomName.adjacent room
        |> List.choose (fun name -> Map.tryFind name world.Rooms |> Option.map (fun r -> name, r))

    let creeps =
        seen
        |> List.collect (fun (_, r) -> r.Hostiles |> List.map (fun h -> h.Id, (h, r.Hostiles)))
        |> Map.ofList

    let towers =
        seen
        |> List.filter (fun (name, _) -> name = room)
        |> List.collect (fun (name, r) ->
            r.Towers |> List.map (fun (id, tile) -> id, RoomPos.at name tile))
        |> Map.ofList

    maps.Classes
    |> List.collect (fun map ->
        map.Members
        |> List.choose (fun id ->
            match Map.tryFind id towers, Map.tryFind id creeps with
            | Some at, _ ->
                Some
                    {
                        Ticks = map.Ticks
                        At = at
                        Strike = map.Class.Strike
                        Power = fun tile -> Engine.towerAttackAt (range tile (RoomPos.pos at))
                    }
            | None, Some(hostile, side) ->
                let power = HostileInfo.effectivePower side hostile

                let damage =
                    if map.Class.Strike = Engine.meleeRange then
                        Engine.attackPower * power Attack
                    else
                        Engine.rangedAttackPower * power RangedAttack

                Some
                    {
                        Ticks = map.Ticks
                        At = hostile.Pos
                        Strike = map.Class.Strike
                        Power = fun _ -> damage
                    }
            | None, None -> None))

/// What the strikers of a room land a tick on one of its tiles that their
/// class can strike within `ticks`: each one counted where it stands near
/// enough to, at a tile a tick and one more over a border (its exit pair is
/// one crossing). An overcount where one class is many bodies, the direction
/// a body runs in.
let damageAt (room: string) (strikers: Striker list) (ticks: int) (tile: Pos) : int =
    let here = RoomPos.at room tile
    let index = indexOf tile

    strikers
    |> List.sumBy (fun striker ->
        if
            at index striker.Ticks <= ticks
            && (if striker.At.Room = room then
                    range (RoomPos.pos striker.At) tile <= striker.Strike + ticks
                else
                    RoomName.tileRange striker.At here
                    |> Option.exists (fun r -> r <= striker.Strike + ticks + 1))
        then
            striker.Power tile
        else
            0)

/// One body of ours' arrival over its room by its gait (`gaitOf`: its live
/// MOVE, its load and its parts shot off), and the fatigue it waits out now;
/// None for a body the projection does not place.
let ofBody (atlas: Atlas.Atlas) (creep: CreepInfo) : BodyArrival option =
    Atlas.creepTile atlas creep.Name
    |> Option.bind (fun tile ->
        Atlas.arrivalGroundIn atlas tile.Room false
        |> Option.map (fun ground ->
            let gait = gaitOf creep

            let ticks, leaves =
                floodWithDepartures
                    ground
                    (paceOf gait)
                    false
                    [ indexOf (RoomPos.pos tile), 0, waitOf creep.Fatigue gait ]

            {
                Room = tile.Room
                Ticks = ticks
                Leaves = leaves
            }))
