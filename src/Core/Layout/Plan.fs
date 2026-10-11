/// The colony's whole plan, drawn once for the eighth level on the room's
/// ground (`Ground`) around its anchor: every structure the engine allows a
/// room a place, so nothing moves or is rebuilt as the room levels up. A
/// level places the prefix of each kind it allows (`Sites`).
///
/// The shape is a diagonal lattice: roads on the two diagonal families
/// through the anchor, four tiles apart, and structures in the plus-shaped
/// cells between them, so every structure stands beside a road and a hauler
/// on one road tile reaches up to four. The anchor is a road crossing; its
/// four orthogonal neighbours are the hub, which one standing body reaches
/// all of. Slots go to kinds nearest first by the walk along the lattice,
/// so the structures filled every tick are the ones closest to the storage.
module Fabot.Core.Layout.Plan

open Fable.Core
open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Grid
open Fabot.Core.Layout.Ground
open Fabot.Core.Layout.Shape

/// The lattice's period: roads every fourth diagonal, which leaves cells of
/// five tiles, each beside a road. A design constant, not an engine fact.
let private period = 4

let private wrap (v: int) = ((v % period) + period) % period

/// Whether a tile lies on the lattice's roads drawn through `anchor`.
let onLattice (anchor: int) (tile: int) =
    let dx = Tiles.xOf tile - Tiles.xOf anchor
    let dy = Tiles.yOf tile - Tiles.yOf anchor
    wrap (dx + dy) = 0 || wrap (dx - dy) = 0


/// The labs the eighth level allows: two inputs and the outputs.
let private labs = Engine.controllerStructures "lab" Engine.controllerMaxLevel

/// The level from which the engine allows the `rank`-th structure of a kind
/// (0-based); past the eighth level's count, the eighth plus one.
let levelOf (kind: Kind) (rank: int) =
    let name = Kind.name kind
    let mutable level = 0

    while level <= Engine.controllerMaxLevel
          && Engine.controllerStructures name level <= rank do
        level <- level + 1

    level

/// The order kinds are placed in while several are open: spawn capacity
/// first, then defence, then what feeds them, then the rest.
let priority (kind: Kind) =
    match kind with
    | Kind.Spawn -> 0
    | Kind.Extension -> 1
    | Kind.Tower -> 2
    | Kind.Storage -> 3
    | Kind.Container -> 4
    | Kind.Link -> 5
    | Kind.Road -> 6
    | Kind.Terminal -> 7
    | Kind.Extractor -> 8
    | Kind.Lab -> 9
    | Kind.Factory -> 10
    | Kind.PowerSpawn -> 11
    | Kind.Nuker -> 12
    | Kind.Observer -> 13
    | _ -> 14

// The reaction cluster among the open slots: two adjacent inputs (the pair
// that leaves the widest window within reach of both) and the outputs within
// reaction range of both, the cluster whose slots' ranks (`rank`, the walk
// order) sum least; ties to the earlier input. The first input is one of
// `inputs`. Inputs first; None when no pair has room for every output.
let private labCluster (inputs: int[]) (rank: int[]) (isOpen: int -> bool) : int[] option =
    let reach = Engine.labReactionRange
    let mutable best: int[] option = None
    let mutable bestCost = System.Int32.MaxValue

    for a in inputs do
        // Every cluster through a later input costs at least its rank.
        if isOpen a && (at (a) rank) < bestCost then
            for direction in [| Tiles.right; Tiles.bottomRight; Tiles.bottom; Tiles.bottomLeft |] do
                let b = Tiles.neighbour a direction

                if b >= 0 && isOpen b then
                    let x0 = max (Tiles.xOf a) (Tiles.xOf b) - reach
                    let x1 = min (Tiles.xOf a) (Tiles.xOf b) + reach
                    let y0 = max (Tiles.yOf a) (Tiles.yOf b) - reach
                    let y1 = min (Tiles.yOf a) (Tiles.yOf b) + reach
                    let window = ResizeArray<int>()

                    for y in max 0 y0 .. min (Tiles.side - 1) y1 do
                        for x in max 0 x0 .. min (Tiles.side - 1) x1 do
                            let t = Tiles.index x y

                            if t <> a && t <> b && isOpen t then
                                window.Add t

                    if window.Count >= labs - 2 then
                        let outputs =
                            window.ToArray()
                            |> Array.sortBy (fun t -> at t rank)
                            |> Array.truncate (labs - 2)

                        let cost =
                            (at (a) rank)
                            + (at (b) rank)
                            + Array.sumBy (fun t -> (at (t) rank)) outputs

                        if cost < bestCost then
                            bestCost <- cost
                            best <- Some(Array.append [| a; b |] outputs)

    best

/// The open slots nearest the core the lab search tries as a first input
/// before it tries them all: a few labs' worth, enough on open ground.
let private labSearch = 3 * labs

/// The hub's kinds, in the order they take the anchor's orthogonal
/// neighbours.
let private hub = [| Kind.Storage; Kind.Link; Kind.Terminal; Kind.Factory |]

/// The eighth level's allowance of a kind.
let private count (kind: Kind) =
    Engine.controllerStructures (Kind.name kind) Engine.controllerMaxLevel

/// The kinds a plan seats its whole allowance of; a plan short of one is
/// unfit. Links, containers and roads are seated by what they serve.
let sized =
    [
        Kind.Spawn
        Kind.Extension
        Kind.Tower
        Kind.Storage
        Kind.Terminal
        Kind.Factory
        Kind.PowerSpawn
        Kind.Nuker
        Kind.Observer
        Kind.Lab
    ]

/// What a step off the lattice pays over the ground's weight: a road leaves
/// the lattice only to at round a wall the lattice cannot. A design
/// constant.
let private detour = 2 * Engine.plainWeight

/// The walk's step prices: a step costs the walk weight of the tile it lands
/// on, lattice or detour.
let private steps = Array.init (Engine.swampWeight + detour + 1) id

let private none = System.Int32.MaxValue

/// The level roads and containers are laid from: the first extensions'.
/// At the first level a room's energy goes to its controller alone.
let haulLevel = levelOf Kind.Extension 0

/// A source's container as planted: its rock, its tile, the link held beside
/// a dead end, and the rock's way out.
type private Posted =
    {
        Rock: int
        Container: int
        Link: int option
        Path: int list
    }

/// Ints sorted ascending in place: the engine's numeric sort under Fable,
/// which the library's comparer-driven sort is several times slower than.
#if FABLE_COMPILER_JAVASCRIPT
[<Emit("$0.sort((a, b) => a - b)")>]
#endif
let private sortInts (_keys: int[]) : unit = Array.sortInPlace _keys

/// The walks a road's sort key tells apart, so level, walk and tile share
/// one int; longer walks sort together, by tile.
let private roadSpan = 1 <<< 14

/// The next tile down a flood from `t` toward its goal: the neighbour whose
/// distance plus its own weight is least, ties to the lowest direction, a
/// ring tile short of the goal never stepped on; -1 at a goal or where none
/// is reached.
let down (weights: int[]) (dist: int[]) (t: int) =
    let mutable next = -1
    let mutable best = unreached

    if at t dist <> 0 then
        for direction in 1..8 do
            let n = Tiles.neighbour t direction

            if n >= 0 && at n weights >= 0 then
                let d = at n dist

                if d <> unreached && not (d > 0 && Tiles.onEdge n) && d + at n weights < best then
                    best <- d + at n weights
                    next <- n

    next

/// The cheapest walk down a flood from `start` (`down` each step): the tiles
/// after `start` up to and with the goal; empty when no goal is reached.
let descend (weights: int[]) (dist: int[]) (start: int) : int list =
    let path = ResizeArray()
    let mutable here = down weights dist start

    while here >= 0 && path.Count < Tiles.count do
        path.Add here
        here <- if at here dist = 0 then -1 else down weights dist here

    if path.Count > 0 && at path.[path.Count - 1] dist = 0 then
        List.ofSeq path
    else
        []

/// The plan of a room around `anchor`. The spawns standing (`spawns`) only
/// order the anchor's spawn slots, those they hold taken first, so a spawn
/// on a slot never moves the plan.
///
/// One flood from the anchor prices the room: a lattice tile at the ground's
/// weight, any other at `detour` more. Walked down, it gives every road: the
/// way out to each source, the controller and each mineral, and the way in
/// to each slot, which leaves the lattice only round a wall. Those ways are
/// laid before any slot is handed out, so no structure ever stands on one.
let draw (ground: Ground) (anchor: int) (spawns: int[]) : Plan =
    let weights = ground.Weights
    let unfit = ResizeArray<string>()

    let onLat = Array.zeroCreate Tiles.count
    let lane = Array.zeroCreate Tiles.count

    for t in 0 .. Tiles.count - 1 do
        let on = onLattice anchor t
        setAt t onLat on
        let w = at t weights

        setAt
            t
            lane
            (if w < 0 || Tiles.onEdge t then -1
             elif on then w
             else w + detour)

    setAt (anchor) lane (max Engine.plainWeight (at (anchor) weights))
    let walk = flood One lane steps [| anchor |]

    // Kept clear: every tile a lattice tile's way down crosses off the
    // lattice, and every tile of a way out.
    let clear = Array.create Tiles.count false
    let lattice t = at t onLat && at t lane >= 0

    let walked = Array.create Tiles.count false

    for t in 0 .. Tiles.count - 1 do
        if lattice t && (at (t) walk) <> unreached then
            let mutable step = down lane walk t

            while step >= 0 && not (at step walked) do
                setAt step walked true

                if not (lattice step) then
                    setAt step clear true

                step <- down lane walk step

    let wayOut (rock: int) =
        let path = descend lane walk rock

        for t in path do
            setAt (t) clear (true)

        path

    let sourceWalks =
        ground.Sources
        |> Array.choose (fun s ->
            match wayOut s with
            | [] ->
                unfit.Add "source unreached"
                None
            | path -> Some(s, path))

    let controllerWalk =
        if ground.Controller >= 0 then
            match wayOut ground.Controller with
            | [] ->
                unfit.Add "controller unreached"
                []
            | path -> path
        else
            []

    let mineralWalks =
        ground.Minerals
        |> Array.choose (fun m ->
            match wayOut m with
            | [] ->
                unfit.Add "mineral unreached"
                None
            | path -> Some(m, path))

    // Each slot: a free tile off the lattice and every way, beside a reached
    // lattice tile (its access), keyed by the access's walk, then by tile.
    let access = Array.create Tiles.count -1
    // A slot's key: its access's walk, then its tile, in one int.
    let slotList = ResizeArray<int>()

    for t in 0 .. Tiles.count - 1 do
        if (at (t) ground.Free) && not (at (t) clear) && not (at t onLat) then
            let mutable best = unreached
            let mutable road = -1

            for direction in 1..8 do
                let n = Tiles.neighbour t direction

                if n >= 0 && lattice n && (at (n) walk) < best then
                    best <- (at (n) walk)
                    road <- n

            if road >= 0 then
                setAt (t) access (road)
                slotList.Add(best * Tiles.count + t)

    let ordered =
        let keys = slotList.ToArray()
        sortInts keys
        keys |> Array.map (fun key -> key % Tiles.count)

    let rank = Array.create Tiles.count none
    ordered |> Array.iteri (fun i t -> setAt (t) rank (i))

    let taken = Array.create Tiles.count false
    let picked = ResizeArray<Kind * int>()

    let mutable cursor = 0

    let fill (kind: Kind) (n: int) =
        for _ in 1..n do
            while cursor < ordered.Length && (at ((at (cursor) ordered)) taken) do
                cursor <- cursor + 1

            if cursor < ordered.Length then
                let t = (at (cursor) ordered)
                setAt (t) taken (true)
                picked.Add(kind, t)

    // The hub: the anchor's orthogonal neighbours, where they are slots.
    let mutable hubFilled = 0

    for direction in [| Tiles.top; Tiles.right; Tiles.bottom; Tiles.left |] do
        let t = Tiles.neighbour anchor direction

        if hubFilled < hub.Length && t >= 0 && (at (t) rank) <> none && not (at (t) taken) then
            setAt (t) taken (true)
            picked.Add((at (hubFilled) hub), t)
            hubFilled <- hubFilled + 1

    for k in hubFilled .. hub.Length - 1 do
        fill (at (k) hub) 1

    // The spawns on the anchor's spawn slots, those with a spawn standing
    // first, then any slot left nearest.
    let slots = spawnSlots anchor

    let order =
        Array.append
            (slots |> Array.filter (fun t -> t >= 0 && Array.contains t spawns))
            (slots |> Array.filter (fun t -> t >= 0 && not (Array.contains t spawns)))

    let mutable placedSpawns = 0

    for t in order do
        if placedSpawns < count Kind.Spawn && at t rank <> none && not (at t taken) then
            setAt t taken true
            picked.Add(Kind.Spawn, t)
            placedSpawns <- placedSpawns + 1

    fill Kind.Spawn (count Kind.Spawn - placedSpawns)
    fill Kind.Tower (count Kind.Tower)
    fill Kind.Extension (count Kind.Extension)
    fill Kind.PowerSpawn (count Kind.PowerSpawn)
    fill Kind.Nuker (count Kind.Nuker)

    let isOpen t =
        (at (t) rank) <> none && not (at (t) taken)

    let nearest = ordered |> Array.filter isOpen

    match
        labCluster (Array.truncate labSearch nearest) rank isOpen
        |> Option.orElseWith (fun () -> labCluster nearest rank isOpen)
    with
    | Some cluster ->
        for t in cluster do
            setAt (t) taken (true)
            picked.Add(Kind.Lab, t)
    | None -> ()

    fill Kind.Observer (count Kind.Observer)

    // Each slot's way in, from its access down to the anchor, a road at the
    // lowest level of the slots it serves, and never before the haul's; a
    // standing spawn off the slots enters by its nearest reached neighbour.
    let roadLevel = Array.create Tiles.count none
    let ranks = System.Collections.Generic.Dictionary<Kind, int>()
    let items = ResizeArray<Item>()

    for kind, t in picked do
        let r =
            match ranks.TryGetValue kind with
            | true, r -> r
            | _ -> 0

        ranks.[kind] <- r + 1
        let level = levelOf kind r
        items.Add { Kind = kind; Tile = t; Level = level }

        let mutable road =
            if (at (t) access) >= 0 then
                (at (t) access)
            else
                let mutable best = -1

                for direction in 1..8 do
                    let n = Tiles.neighbour t direction

                    if
                        n >= 0
                        && (at (n) lane) >= 0
                        && (at (n) walk) <> unreached
                        && not (at (n) taken)
                        && (best < 0 || (at (n) walk) < (at (best) walk))
                    then
                        best <- n

                best

        while road >= 0 do
            setAt road roadLevel (min (at road roadLevel) (max level haulLevel))
            road <- down lane walk road

    setAt anchor roadLevel (min (at anchor roadLevel) haulLevel)

    let extra = ResizeArray<Item>()

    let plant (kind: Kind) (t: int) (level: int) =
        setAt (t) taken (true)
        setAt (t) roadLevel (none)
        extra.Add { Kind = kind; Tile = t; Level = level }

    let pave (path: int list) (level: int) =
        for t in path do
            if not (at (t) taken) then
                setAt (t) roadLevel (min (at (t) roadLevel) level)

    // A container is walkable and takes a dead end beside its rock, off the
    // head of its way out: a tile no way crosses, so no body walking past
    // shoves its harvester off it. Where no such tile is, the head itself.
    // One container serves every rock it stands beside.
    let rockLevel = haulLevel

    // A tile a link may take: free of the plan, off the two rings.
    let linkable (n: int) =
        n >= 0
        && at n weights >= 0
        && not (at n taken)
        && at n roadLevel = none
        && Tiles.xOf n >= 2
        && Tiles.yOf n >= 2
        && Tiles.xOf n < Tiles.side - 2
        && Tiles.yOf n < Tiles.side - 2

    let linkBeside (onto: int) (key: int -> int) : int option =
        let mutable best = -1

        for direction in 1..8 do
            let n = Tiles.neighbour onto direction

            if linkable n && (best < 0 || key n < key best || (key n = key best && n < best)) then
                best <- n

        if best < 0 then None else Some best

    let sourceLink (container: int) =
        linkBeside container (fun t -> Tiles.range t anchor)

    // Every walk's tiles, the ones paved after a dead end is chosen included.
    let onWay = Array.create Tiles.count false

    for _, path in sourceWalks do
        for t in path do
            setAt t onWay true

    for t in controllerWalk do
        setAt t onWay true

    for _, path in mineralWalks do
        for t in path do
            setAt t onWay true

    // Off every way, with room beside it for its link.
    let deadEnd (rock: int) (head: int) (t: int) =
        t <> head
        && Tiles.range t rock = 1
        && Tiles.range t head = 1
        && at t weights >= 0
        && not (Tiles.onEdge t)
        && not (at t taken)
        && not (at t clear)
        && not (at t onWay)
        && at t roadLevel = none
        && Tiles.within 1 t
           |> Array.exists (fun n -> n <> t && linkable n && not (at n clear))

    // Each source, its container's tile, its link's where held, and its way
    // out. A dead end's link is held as it is planted, so no other container
    // or link takes the room it was chosen for.
    let posted =
        sourceWalks
        |> Array.map (fun (rock, path) ->
            let head = List.head path

            match Tiles.within 1 rock |> Array.tryFind (deadEnd rock head) with
            | Some t when not (at head taken) ->
                plant Kind.Container t rockLevel
                pave path rockLevel
                let link = sourceLink t
                link |> Option.iter (fun l -> setAt l taken true)

                {
                    Rock = rock
                    Container = t
                    Link = link
                    Path = path
                }
            | _ ->
                if not (at head taken) then
                    plant Kind.Container head rockLevel

                pave (List.tail path) rockLevel

                {
                    Rock = rock
                    Container = head
                    Link = None
                    Path = path
                })

    // The controller's container a tile further out than the first, where
    // upgraders stand round it within range 3 of the controller; the first
    // tile is left to stand on.
    let seat (t: int) =
        if not (at t taken) then
            plant Kind.Container t rockLevel

    let controllerSeat =
        match controllerWalk with
        | first :: second :: rest when Tiles.range second ground.Controller <= 3 ->
            seat second
            pave rest rockLevel
            Some(first, second)
        | first :: rest ->
            seat first
            pave rest rockLevel
            Some(first, first)
        | [] -> None

    let digLevel = levelOf Kind.Extractor 0

    for m, path in mineralWalks do
        plant Kind.Extractor m digLevel

        if not (at (List.head path) taken) then
            plant Kind.Container (List.head path) digLevel

        pave (List.tail path) digLevel

    // The links past the hub's: the controller's, where upgraders reach it,
    // then each source's, the longest way out first; each beside its
    // container on a tile no plan holds, a dead end's held since it was
    // planted.
    let links = ResizeArray<int>()

    let open' (t: int) =
        let mutable n = 0

        for direction in 1..8 do
            let m = Tiles.neighbour t direction

            if m >= 0 && (at (m) weights) >= 0 then
                n <- n + 1

        n

    match controllerSeat with
    | Some(first, container) ->
        // Off the walk's first tile, which is left for an upgrader.
        match
            linkBeside container (fun t ->
                if t = first then 100
                elif Tiles.range t ground.Controller <= 3 then -(open' t)
                else 0)
        with
        | Some t when t <> first ->
            setAt (t) taken (true)
            links.Add t
        | _ -> ()
    | None -> ()

    for p in posted |> Array.sortBy (fun p -> -(List.length p.Path), p.Rock) do
        match p.Link |> Option.orElseWith (fun () -> sourceLink p.Container) with
        | Some t ->
            setAt (t) taken (true)
            links.Add t
        | None -> ()

    let hubLinks = items |> Seq.sumBy (fun i -> if i.Kind = Kind.Link then 1 else 0)

    links
    |> Seq.truncate (count Kind.Link - hubLinks)
    |> Seq.iteri (fun i t ->
        extra.Add
            {
                Kind = Kind.Link
                Tile = t
                Level = levelOf Kind.Link (hubLinks + i)
            })

    // Roads by level, then nearest the anchor, then tile, keyed in one int.
    let roadKeys = ResizeArray<int>()

    for t in 0 .. Tiles.count - 1 do
        if at t roadLevel <> none && not (at t taken) then
            roadKeys.Add(
                (at t roadLevel * roadSpan + min (roadSpan - 1) (at t walk)) * Tiles.count + t
            )

    let roads =
        let keys = roadKeys.ToArray()
        sortInts keys

        keys
        |> Array.map (fun key ->
            let t = key % Tiles.count

            {
                Kind = Kind.Road
                Tile = t
                Level = at t roadLevel
            })

    let all = Array.concat [ items.ToArray(); extra.ToArray(); roads ]

    for kind in sized do
        if all |> Array.sumBy (fun i -> if i.Kind = kind then 1 else 0) < count kind then
            unfit.Add(Kind.name kind)

    {
        Room = ground.Room
        Anchor = anchor
        Items =
            let keys =
                Array.init all.Length (fun i -> priority (at i all).Kind * all.Length + i)

            sortInts keys
            keys |> Array.map (fun key -> at (key % all.Length) all)
        Unfit = List.ofSeq unfit
    }

/// The plan of a room with the spawns that stand in it (tiles): around the
/// first of `Ground.anchors` whose plan stands its first spawn on a spawn
/// slot, a standing spawn where any stands, so the spawn a room is founded
/// with, built where the plan says, gives the plan again; else around the
/// first. None when the room offers no anchor.
let ofGround (ground: Ground) (spawns: int[]) : Plan option =
    let candidates = anchors ground spawns

    let seated (plan: Plan) =
        plan.Items
        |> Array.tryFind (fun i -> i.Kind = Kind.Spawn)
        |> Option.exists (fun i ->
            Array.contains i.Tile (spawnSlots plan.Anchor)
            && (spawns.Length = 0 || Array.contains i.Tile spawns))

    candidates
    |> Seq.map (fun anchor -> draw ground anchor spawns)
    |> Seq.tryFind seated
    |> Option.orElseWith (fun () ->
        Array.tryHead candidates |> Option.map (fun a -> draw ground a spawns))
