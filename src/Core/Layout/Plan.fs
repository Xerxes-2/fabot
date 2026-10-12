/// Drawn once for the eighth level, so nothing moves or is rebuilt as the
/// room levels up. A diagonal lattice, so a hauler on one road tile reaches up to four.
module Fabot.Core.Layout.Plan

open Fable.Core
open Fabot.Core
open Fabot.Core.World
open Fabot.Core.Grid
open Fabot.Core.Layout.Ground
open Fabot.Core.Layout.Shape

/// Leaves cells of five tiles, each beside a road. A design constant, not an
/// engine fact.
let private period = 4

let private wrap (v: int) = ((v % period) + period) % period

let onLattice (anchor: int) (tile: int) =
    let dx = Tiles.xOf tile - Tiles.xOf anchor
    let dy = Tiles.yOf tile - Tiles.yOf anchor
    wrap (dx + dy) = 0 || wrap (dx - dy) = 0


let private labs = Engine.controllerStructures "lab" Engine.controllerMaxLevel

let levelOf (kind: Kind) (rank: int) =
    let name = Kind.name kind
    let mutable level = 0

    while level <= Engine.controllerMaxLevel
          && Engine.controllerStructures name level <= rank do
        level <- level + 1

    level

let priority (kind: Kind) =
    match kind with
    | Kind.Spawn -> 0
    | Kind.Extension -> 1
    | Kind.Tower -> 2
    | Kind.Rampart -> 3
    | Kind.Storage -> 4
    | Kind.Container -> 5
    | Kind.Link -> 6
    | Kind.Road -> 7
    | Kind.Terminal -> 8
    | Kind.Extractor -> 9
    | Kind.Lab -> 10
    | Kind.Factory -> 11
    | Kind.PowerSpawn -> 12
    | Kind.Nuker -> 13
    | Kind.Observer -> 14
    | _ -> 15

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

/// A few labs' worth of first inputs: enough on open ground.
let private labSearch = 3 * labs

let private hub = [| Kind.Storage; Kind.Link; Kind.Terminal; Kind.Factory |]

let private count (kind: Kind) =
    Engine.controllerStructures (Kind.name kind) Engine.controllerMaxLevel

/// Not links, containers or roads: they are seated by what they serve.
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

/// What a raid comes for.
let private keep = [| Kind.Spawn; Kind.Tower; Kind.Storage; Kind.Terminal |]

/// The first tower: the room's first defence.
let rampartLevel = levelOf Kind.Tower 0

/// A road leaves the lattice only to go round a wall. A design constant.
let private detour = 2 * Engine.plainWeight

let private steps = Array.init (Engine.swampWeight + detour + 1) id

let private none = System.Int32.MaxValue

/// At the first level a room's energy goes to its controller alone.
let haulLevel = levelOf Kind.Extension 0

type private Posted =
    {
        Rock: int
        Container: int
        Link: int option
        Path: int list
    }

/// The engine's numeric sort under Fable: the library's comparer-driven sort
/// is several times slower.
#if FABLE_COMPILER_JAVASCRIPT
[<Emit("$0.sort((a, b) => a - b)")>]
#endif
let private sortInts (_keys: int[]) : unit = Array.sortInPlace _keys

/// So level, walk and tile share one sort key; longer walks sort together.
let private roadSpan = 1 <<< 14

let down (weights: int[]) (dist: int[]) (t: int) =
    if at t dist = 0 then
        -1
    else
        Tiles.leastAround t unreached (fun n ->
            let d = at n dist

            if at n weights < 0 || d = unreached || (d > 0 && Tiles.onEdge n) then
                unreached
            else
                d + at n weights)

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

/// `spawns` only order the spawn slots, so a spawn on a slot never moves the
/// plan. Ways are laid before slots are handed out, so no structure stands on one.
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
        match ground.Controller |> Option.map wayOut with
        | Some [] ->
            unfit.Add "controller unreached"
            []
        | Some path -> path
        | None -> []

    let mineralWalks =
        ground.Minerals
        |> Array.choose (fun m ->
            match wayOut m with
            | [] ->
                unfit.Add "mineral unreached"
                None
            | path -> Some(m, path))

    let access = Array.create Tiles.count -1
    let slotList = ResizeArray<int>()

    for t in 0 .. Tiles.count - 1 do
        if (at (t) ground.Free) && not (at (t) clear) && not (at t onLat) then
            let road =
                Tiles.leastAround t unreached (fun n -> if lattice n then at n walk else unreached)

            if road >= 0 then
                setAt t access road
                slotList.Add(at road walk * Tiles.count + t)

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

    let mutable hubFilled = 0

    for direction in [| Tiles.top; Tiles.right; Tiles.bottom; Tiles.left |] do
        let t = Tiles.neighbour anchor direction

        if hubFilled < hub.Length && t >= 0 && (at (t) rank) <> none && not (at (t) taken) then
            setAt (t) taken (true)
            picked.Add((at (hubFilled) hub), t)
            hubFilled <- hubFilled + 1

    for k in hubFilled .. hub.Length - 1 do
        fill (at (k) hub) 1

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
                Tiles.leastAround t unreached (fun n ->
                    if at n lane >= 0 && not (at n taken) then
                        at n walk
                    else
                        unreached)

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

    // A container takes a dead end off its way out, so no body walking past
    // shoves its harvester off it.
    let rockLevel = haulLevel

    let onWay = Array.create Tiles.count false

    for _, path in sourceWalks do
        for t in path do
            setAt t onWay true

    for t in controllerWalk do
        setAt t onWay true

    for _, path in mineralWalks do
        for t in path do
            setAt t onWay true

    let linkable (n: int) =
        at n weights >= 0
        && not (at n taken)
        && not (at n onWay)
        && at n roadLevel = none
        && Tiles.xOf n >= 2
        && Tiles.yOf n >= 2
        && Tiles.xOf n < Tiles.side - 2
        && Tiles.yOf n < Tiles.side - 2

    let linkBeside (onto: int) (key: int -> int) : int option =
        match Tiles.around onto |> Array.filter linkable with
        | [||] -> None
        | ns -> Some(ns |> Array.minBy (fun n -> key n, n))

    let sourceLink (container: int) =
        linkBeside container (fun t -> Tiles.range t anchor)

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
        && Tiles.around t |> Array.exists (fun n -> linkable n && not (at n clear))

    // A dead end's link is held as it is planted, so no other container or
    // link takes the room it was chosen for.
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

    // The controller's container a tile further out, leaving the first for
    // an upgrader to stand on.
    let seat (t: int) =
        if not (at t taken) then
            plant Kind.Container t rockLevel

    let controllerSeat =
        match ground.Controller, controllerWalk with
        | Some c, first :: second :: rest when Tiles.range second c <= 3 ->
            seat second
            pave rest rockLevel
            Some(c, first, second)
        | Some c, first :: rest ->
            seat first
            pave rest rockLevel
            Some(c, first, first)
        | _ -> None

    let digLevel = levelOf Kind.Extractor 0

    for m, path in mineralWalks do
        plant Kind.Extractor m digLevel

        if not (at (List.head path) taken) then
            plant Kind.Container (List.head path) digLevel

        pave (List.tail path) digLevel

    let links = ResizeArray<int>()

    let open' (t: int) =
        Tiles.around t |> Array.sumBy (fun m -> if at m weights >= 0 then 1 else 0)

    match controllerSeat with
    | Some(controller, first, container) ->
        // Off the walk's first tile, which is left for an upgrader.
        match
            linkBeside container (fun t ->
                if t = first then 100
                elif Tiles.range t controller <= 3 then -(open' t)
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

    let hubLinks = items |> Seq.sumBy (fun i -> if i.Kind.IsLink then 1 else 0)
    let outer = links |> Seq.truncate (count Kind.Link - hubLinks) |> Array.ofSeq

    outer
    |> Array.iteri (fun i t ->
        extra.Add
            {
                Kind = Kind.Link
                Tile = t
                Level = levelOf Kind.Link i
            })

    // The hub's link comes last: it pays the 3% loss on all the storage's
    // energy to save a walk, where a source's link saves a whole haul.
    for k in 0 .. items.Count - 1 do
        if items.[k].Kind = Kind.Link then
            items.[k] <-
                { items.[k] with
                    Level = levelOf Kind.Link outer.Length
                }

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

    // A source's container too: its harvester cannot leave it.
    let ramparts =
        let covered =
            [|
                for i in items do
                    if Array.contains i.Kind keep then
                        yield i.Tile, i.Level
                for p in posted -> p.Container, rockLevel
            |]

        covered
        |> Array.distinctBy fst
        |> Array.map (fun (t, level) ->
            {
                Kind = Kind.Rampart
                Tile = t
                Level = max rampartLevel level
            })

    let all = Array.concat [ items.ToArray(); extra.ToArray(); roads; ramparts ]

    for kind in sized do
        if all |> Array.sumBy (fun i -> if i.Kind = kind then 1 else 0) < count kind then
            unfit.Add(Kind.name kind)

    {
        Room = ground.Room
        Anchor = anchor
        Items =
            // A container whose rock other seats ring goes before the
            // extensions: its harvester digs beside the crews. A rock's only
            // seat waits for the bank, or a small harvester holds it a life.
            let crowded =
                posted
                |> Array.filter (fun p ->
                    Tiles.around p.Rock
                    |> Array.filter (fun n -> at n weights >= 0)
                    |> Array.length > 1)
                |> Array.map (fun p -> p.Container)

            let rank (i: Item) =
                if i.Kind.IsContainer && Array.contains i.Tile crowded then
                    2 * priority Kind.Extension - 1
                else
                    2 * priority i.Kind

            let keys = Array.init all.Length (fun i -> rank (at i all) * all.Length + i)

            sortInts keys
            keys |> Array.map (fun key -> at (key % all.Length) all)
        Unfit = List.ofSeq unfit
    }

/// Scored so the spawns a plan places, built where it says, give the plan
/// again: an anchor before it in the order seats no spawn, standing or not.
let ofGround (ground: Ground) (spawns: int[]) : Plan option =
    let candidates = anchors ground spawns

    let score (plan: Plan) =
        let placed =
            plan.Items
            |> Array.filter (fun i -> i.Kind.IsSpawn)
            |> Array.map (fun i -> i.Tile)

        let kept = spawns |> Array.sumBy (fun s -> if Array.contains s placed then 1 else 0)

        let seated =
            placed
            |> Array.tryHead
            |> Option.exists (fun s -> Array.contains s (spawnSlots plan.Anchor))

        2 * kept + (if seated then 1 else 0)

    let tried =
        if spawns.Length = 0 then
            min spawnOffsets.Length candidates.Length
        else
            candidates.Length

    let mutable best: (int * Plan) option = None
    let mutable i = 0

    while i < tried && best |> Option.forall (fun (s, _) -> s <= 2 * spawns.Length) do
        let plan = draw ground candidates.[i] spawns
        let s = score plan

        if best |> Option.forall (fun (b, _) -> s > b) then
            best <- Some(s, plan)

        i <- i + 1

    best |> Option.map snd
