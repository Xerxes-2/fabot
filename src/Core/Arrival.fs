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

/// One body of ours' gait as the engine walks it (#524, `movement.js`): its
/// live MOVE against the weight of every part but MOVE and empty CARRY
/// (`fatigueFactorOf`), and of every part shot off but a MOVE or a CARRY
/// (`CreepInfo.ShotOff`), which the engine still weighs.
let gaitOf (creep: CreepInfo) : Gait =
    let factor = fatigueFactorOf creep

    {
        Weight = factor.FatigueParts + creep.ShotOff
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

/// A room's maps: its own hostiles and what its neighbours carry across, one
/// flood a class dilated by its strike, and the risk an unseen neighbour of
/// remembered danger (`WorldView.Danger`) lands on its exits until it is seen
/// again (Decided 2). A neighbour's floods are `floodsOf`'s.
let private mapsOf
    (floodsOf: string -> RoomArrival option)
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

        match across |> Option.bind (fun _ -> floodsOf neighbour) with
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

/// Each room's floods over one hostile ground (`ground`, by room, None for a
/// room it carries none for: `Atlas.hostileGroundOf`), as a table read by
/// room: a room's floods, and its ground, built on first read and kept for
/// every later one, a neighbour's carried across included. The world's
/// danger reads one table a tick (`WorldDanger`).
let floodsOver (ground: string -> int[] option) (world: WorldView) : string -> RoomArrival option =
    let grounds = System.Collections.Generic.Dictionary<string, int[] option>()

    let groundOf (room: string) =
        if grounds.ContainsKey room then
            grounds.[room]
        else
            let laid = ground room
            grounds.[room] <- laid
            laid

    let built = System.Collections.Generic.Dictionary<string, RoomArrival option>()

    let rec floodsOf (room: string) : RoomArrival option =
        if built.ContainsKey room then
            built.[room]
        else
            let floods =
                groundOf room
                |> Option.map (fun ground ->
                    let around =
                        RoomName.adjacent room
                        |> List.map (fun neighbour -> neighbour, groundOf neighbour)

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
                                         Members =
                                             members
                                             |> List.map (fun (hostile, _, _) -> hostile.Id)
                                         Arrive =
                                             members
                                             |> List.map (fun (hostile, _, wait) ->
                                                 indexOf (RoomPos.pos hostile.Pos), 0, wait)
                                             |> flood ground cls.Pace false
                                     }))
                        Maps = lazy (mapsOf floodsOf world room ground around)
                    })

            built.[room] <- floods
            floods

    floodsOf

/// A room's hostile maps over one projection's ground
/// (`Atlas.arrivalGroundIn`); none for a room the projection does not carry.
let inRoom (atlas: Atlas.Atlas) (world: WorldView) (room: string) : ArrivalMaps =
    match floodsOver (fun room -> Atlas.arrivalGroundIn atlas room true) world room with
    | Some floods -> floods.Maps.Force()
    | None -> { Classes = []; Unseen = None }

/// The soonest any class is on a tile, by tile index: the per-tile reads ask
/// it of every tile of a room, and a `List.exists` closure there is one
/// allocation a tile.
let rec soonestAmong (index: int) (classes: ClassArrival list) (soonest: int) : int =
    match classes with
    | [] -> soonest
    | map :: rest -> soonestAmong index rest (min soonest (at index map.Ticks))

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

/// By tile index of the room a body stands in, whether it reaches the tile
/// by a walk through tiles it is on and gone from before any of `classes`
/// strikes them (`Leaves < A`, ties unsafe; our ramparts never struck): its
/// own tile, and each tile its flood reaches from a neighbour so reached
/// that it arrives on sooner (movement S4). A tile the flood reaches only
/// through a strike is not one it reaches first, however late the hostiles
/// arrive on it.
let reachedFirst (walk: BodyArrival) (classes: ClassArrival list) (ramparts: Set<Pos>) : bool[] =
    let first = Array.create tileCount false

    // The reached tiles in arrival order, by a counting sort: a closure
    // sort a body a tick cost the siege half a millisecond.
    let mutable most = -1
    let mutable count = 0

    for index in 0 .. tileCount - 1 do
        let ticks = at index walk.Ticks

        if ticks < never then
            most <- max most ticks
            count <- count + 1

    let starts = Array.zeroCreate<int> (most + 2)

    for index in 0 .. tileCount - 1 do
        let ticks = at index walk.Ticks

        if ticks < never then
            starts.[ticks + 1] <- starts.[ticks + 1] + 1

    for t in 1 .. most + 1 do
        starts.[t] <- starts.[t] + starts.[t - 1]

    let order = Array.zeroCreate<int> count

    for index in 0 .. tileCount - 1 do
        let ticks = at index walk.Ticks

        if ticks < never then
            order.[starts.[ticks]] <- index
            starts.[ticks] <- starts.[ticks] + 1

    let edge = Engine.roomSide - 1

    for index in order do
        let ticks = at index walk.Ticks

        if ticks = 0 then
            first.[index] <- true
        elif
            at index walk.Leaves < soonestAmong index classes never
            || Set.contains (posAt index) ramparts
        then
            let x = index / Engine.roomSide
            let y = index % Engine.roomSide

            for nx in max 0 (x - 1) .. min edge (x + 1) do
                for ny in max 0 (y - 1) .. min edge (y + 1) do
                    let n = nx * Engine.roomSide + ny

                    // No walk goes on from an exit tile but the one it
                    // stands on: the engine carries it over the border.
                    let ring = nx = 0 || ny = 0 || nx = edge || ny = edge

                    if
                        n <> index
                        && first.[n]
                        && (not ring || at n walk.Ticks = 0)
                        && at n walk.Ticks < ticks
                    then
                        first.[index] <- true

    first

/// The run-off ground a kiter has against `classes` (#625: every class that
/// strikes it before it strikes back, and an unseen neighbour's risk), in
/// its own ticks: the latest it can step off a tile it reaches before any
/// of them strikes it (`reachedFirst`), walking from `seeds` (each a tile
/// index and the ticks its fatigue holds it there) over `walking` at its
/// `pace`. Never an exit, whose crossing the engine carries it over and
/// back; `refuge` where a tile it reaches first is one none of them ever
/// strikes, or our rampart.
let runOff
    (walking: int[])
    (classes: ClassArrival list)
    (unseen: int[] option)
    (ramparts: Set<Pos>)
    (pace: Pace)
    (seeds: (int * int) list)
    (refuge: int)
    : int =
    let classes =
        match unseen with
        | None -> classes
        | Some ticks ->
            {
                Class =
                    {
                        Strike = Engine.rangedRange
                        Pace = None
                    }
                Members = []
                Ticks = ticks
            }
            :: classes

    let ticks, leaves =
        floodWithDepartures
            walking
            (Some pace)
            false
            (seeds |> List.map (fun (tile, wait) -> tile, 0, wait))

    let first =
        reachedFirst
            {
                Room = ""
                Ticks = ticks
                Leaves = leaves
            }
            classes
            ramparts

    let edge = Engine.roomSide - 1
    let mutable run = 0

    for index in 0 .. tileCount - 1 do
        let x = index / Engine.roomSide
        let y = index % Engine.roomSide

        if first.[index] && x > 0 && y > 0 && x < edge && y < edge then
            let held =
                if
                    soonestAmong index classes never = never || Set.contains (posAt index) ramparts
                then
                    refuge
                else
                    min refuge (at index leaves)

            run <- max run held

    run

/// By tile index, the latest a body standing on the tile can step off it and
/// still hold `need` ticks of run-off against `classes` and an unseen
/// neighbour's risk, each class's clock started as it stands there (#615):
/// stepping off every tile it walks onto, at `pace` over `walking`, before
/// any of them strikes it, to a tile none strikes before `need` (it may
/// stand there that long) or a refuge (none ever strikes it, or our
/// rampart). `need` is held no longer than the body holds on the tile it
/// stands on, `own` (its index, and the ticks its fatigue holds it there),
/// so that tile always holds it. Below 0 where none is held: a pocket its
/// retreat is struck in first, or a tile no body of ours stands on. No
/// run-off is walked on from an exit, which carries the body over the
/// border: there the latest before it is struck alone. One flood from
/// `own`, then one pass over the room, latest first.
let heldFor
    (walking: int[])
    (classes: ClassArrival list)
    (unseen: int[] option)
    (ramparts: Set<Pos>)
    (pace: Pace)
    (own: int * int)
    (need: int)
    : int[] =
    let classes =
        match unseen with
        | None -> classes
        | Some ticks ->
            {
                Class =
                    {
                        Strike = Engine.rangedRange
                        Pace = None
                    }
                Members = []
                Ticks = ticks
            }
            :: classes

    let edge = Engine.roomSide - 1

    // Its own tile, an exit it landed on among them.
    let inside index =
        let x = index / Engine.roomSide
        let y = index % Engine.roomSide

        index = fst own
        || x > 0 && y > 0 && x < edge && y < edge && at index walking > 0

    let struck =
        Array.init tileCount (fun index ->
            if Set.contains (posAt index) ramparts then
                never
            else
                soonestAmong index classes never)

    // What the body holds where it stands: the latest it is struck on a tile
    // it reaches first, standing there until then.
    let ownHeld =
        let tile, wait = own

        let ticks, leaves = floodWithDepartures walking (Some pace) false [ tile, 0, wait ]

        let first =
            reachedFirst
                {
                    Room = ""
                    Ticks = ticks
                    Leaves = leaves
                }
                classes
                ramparts

        let mutable most = -1

        for index in 0 .. tileCount - 1 do
            if first.[index] && inside index then
                most <- max most (if struck.[index] = never then never else struck.[index] - 1)

        most

    // Past the latest any tile is struck, only a refuge holds it.
    let latest =
        struck |> Array.fold (fun most at -> if at < never then max most at else most) 0

    let need = min latest (min need ownHeld)
    let soonest index = struck.[index]
    let ticks = stepTicks pace
    let longest = max pace.Road (max pace.Plain pace.Swamp)
    // A tile held `need` and a step more passes `need` on to every
    // neighbour whatever the step costs: the buckets stop there.
    let cap = max 0 need + longest
    let held = Array.create tileCount -1
    let buckets = Array.init (cap + 1) (fun _ -> ResizeArray<int>())

    for index in 0 .. tileCount - 1 do
        if inside index then
            let struck = soonest index

            if struck = never then
                held.[index] <- never
                buckets.[cap].Add index
            elif struck - 1 >= max 0 need then
                held.[index] <- struck - 1
                buckets.[min cap (struck - 1)].Add index

    for key in cap .. -1 .. 0 do
        let bucket = buckets.[key]
        let mutable i = 0

        while i < bucket.Count do
            let index = bucket.[i]
            i <- i + 1

            if min cap held.[index] = key then
                // Its own tile is stepped back onto at plain's pace.
                let weight = at index walking

                let onto =
                    held.[index]
                    - (if weight > 0 && weight < ticks.Length then
                           at weight ticks
                       else
                           pace.Plain)

                let x = index / Engine.roomSide
                let y = index % Engine.roomSide

                for nx in max 0 (x - 1) .. min edge (x + 1) do
                    for ny in max 0 (y - 1) .. min edge (y + 1) do
                        let n = nx * Engine.roomSide + ny

                        if n <> index && inside n then
                            let from = min (soonest n - 1) onto

                            if from >= 0 && from > held.[n] then
                                held.[n] <- from
                                buckets.[min cap from].Add n

    // An exit carries the body over the border: no run-off is walked on from
    // it, only its strike reads there.
    for index in 0 .. tileCount - 1 do
        if not (inside index) && at index walking > 0 then
            held.[index] <- if struck.[index] = never then never else struck.[index] - 1

    held

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
