/// An augmenting search over the room's whole matching, so swaps and
/// rotations fall out of it rather than being special cases.
module Fabot.Core.Move.Arbitrate

open Fabot.Core
open Fabot.Core.Grid

/// `Searches` is the count `scripts/counts` reads.
type Resolution =
    {
        Steps: (string * int) list
        Searches: int
    }

let private covered (footprint: Footprint) (anchor: int) : int[] =
    match footprint with
    | One -> [| anchor |]
    | Quad -> [| anchor; anchor + 1; anchor + Tiles.side; anchor + Tiles.side + 1 |]

let private inRoom (tile: int) = tile >= 0 && tile < Tiles.count

// A stayer only ever asks back onto its work or its own tile, so movers go
// first. Names are unique, so the order is total and the input's never shows.
let private offered (a: Wish) (b: Wish) =
    match compare (Wish.stays a) (Wish.stays b) with
    | 0 ->
        match compare (max 1 b.Weight) (max 1 a.Weight) with
        | 0 -> System.String.CompareOrdinal(a.Creep, b.Creep)
        | c -> c
    | c -> c

let inline private above (s: int) (h: int) (s0: int) (h0: int) = s > s0 || (s = s0 && h > h0)

/// The four tables a resolution reads, kept across the rooms of one tick: a
/// room refills them from empty anyway, so they are allocations otherwise.
type Scratch =
    {
        Blocked: bool[]
        StepsOnto: int[]
        Holder: int[]
        ChangedAt: int[]
    }

let scratch () : Scratch =
    {
        Blocked = Array.zeroCreate<bool> Tiles.count
        StepsOnto = Array.create Tiles.count -1
        Holder = Array.create Tiles.count -1
        ChangedAt = Array.zeroCreate<int> Tiles.count
    }

/// `held`: the tiles of bodies outside the wishes (theirs, and ours with none).
let resolve
    (scratch: Scratch)
    (passable: int -> bool)
    (held: int[])
    (wishes: Wish list)
    : Resolution =
    let blocked = scratch.Blocked
    let stepsOnto = scratch.StepsOnto
    let holder = scratch.Holder
    let changedAt = scratch.ChangedAt

    Array.fill blocked 0 Tiles.count false
    Array.fill stepsOnto 0 Tiles.count -1
    Array.fill holder 0 Tiles.count -1
    Array.fill changedAt 0 Tiles.count 0

    for tile in held do
        if inRoom tile then
            blocked.[tile] <- true

    // A swap a tick or two away beats the way round: with a held body that
    // steps onto this tile, or a mover through the same head onto it.
    let wishes = Array.ofList wishes

    for wish in wishes do
        if not (Wish.movable wish) then
            for tile in covered wish.Footprint wish.At do
                if inRoom tile then
                    blocked.[tile] <- true

            if inRoom wish.At then
                stepsOnto.[wish.At] <- if Wish.stays wish then -1 else wish.Head

    let placed = ResizeArray<Wish>()

    for wish in wishes |> Array.filter Wish.movable |> Array.sortWith offered do
        if inRoom wish.At && not blocked.[wish.At] && holder.[wish.At] < 0 then
            holder.[wish.At] <- placed.Count
            placed.Add wish

    let order = placed.ToArray()
    let n = order.Length
    let tileOf = order |> Array.map (fun wish -> wish.At)

    let headOn (wish: Wish) =
        order
        |> Array.exists (fun other ->
            other.Head = wish.Head
            && other.Onward = Some wish.At
            && other.Creep <> wish.Creep)

    let candidates =
        order
        |> Array.map (fun wish ->
            let at = wish.At
            let found = ResizeArray<int>()

            let consider (tile: int) =
                if
                    tile <> at
                    && inRoom tile
                    && Tiles.range at tile = 1
                    && not blocked.[tile]
                    && passable tile
                    && not (found.Contains tile)
                then
                    found.Add tile

            if not (inRoom wish.Head && stepsOnto.[wish.Head] = at) then
                consider wish.Head

                if found.Count = 0 || not (headOn wish) then
                    for tile in wish.Tail do
                        consider tile

            found.Add at
            found.ToArray())

    let reachable =
        Array.init n (fun i -> candidates.[i] |> Array.map (Wish.value order.[i]) |> Array.max)

    let onPath = Array.zeroCreate<bool> n
    let labelStamp = Array.zeroCreate<int> n
    let labelScore = Array.zeroCreate<int> n
    let labelHome = Array.zeroCreate<int> n
    let path = ResizeArray<struct (int * int)>()
    let bestChain = ResizeArray<struct (int * int)>()
    let mutable bestScore = 0
    let mutable bestHome = 0
    let mutable taken = 0
    let mutable stamp = 0

    let finish (s: int) (h: int) =
        if above s h 0 0 then
            taken <- taken + 1

            if
                above s h bestScore bestHome
                || (s = bestScore && h = bestHome && path.Count < bestChain.Count)
            then
                bestScore <- s
                bestHome <- h
                bestChain.Clear()
                bestChain.AddRange path

    // A creep reached before is explored again only at a better (score, home):
    // what a chain makes past it depends only on the creeps already on it.
    let rec explore (i: int) (from: int) (score: int) (home: int) (onward: int option) =
        onPath.[i] <- true
        let wish = order.[i]
        let leaving = Wish.value wish from
        let leavingHome = if from = wish.At then 1 else 0

        let step (tile: int) =
            let s = score + Wish.value wish tile - leaving
            let h = home + (if tile = wish.At then 1 else 0) - leavingHome
            let occupant = holder.[tile]
            path.Add(struct (i, tile))

            if occupant < 0 then
                finish s h
            elif
                not onPath.[occupant]
                && (labelStamp.[occupant] <> stamp
                    || above s h labelScore.[occupant] labelHome.[occupant])
            then
                labelStamp.[occupant] <- stamp
                labelScore.[occupant] <- s
                labelHome.[occupant] <- h
                explore occupant tile s h wish.Onward

            path.RemoveAt(path.Count - 1)

        let before = taken

        let skip = defaultArg onward from

        for tile in candidates.[i] do
            if tile <> from && tile <> skip then
                step tile

        match onward with
        | Some o when o <> from && taken = before && Array.contains o candidates.[i] -> step o
        | _ -> ()

        onPath.[i] <- false

    let searchedAt = Array.create n -1
    let mutable clock = 0
    let mutable searches = 0
    let mutable changed = true

    while changed do
        changed <- false

        for i in 0 .. n - 1 do
            let from = tileOf.[i]
            let wish = order.[i]

            let stale =
                searchedAt.[i] < 0
                || candidates.[i] |> Array.exists (fun tile -> changedAt.[tile] > searchedAt.[i])

            let content =
                Wish.value wish from = reachable.[i]
                && (from = wish.At || Wish.value wish wish.At < Wish.value wish from)

            if stale then
                searchedAt.[i] <- clock

            if stale && not content then
                searches <- searches + 1
                stamp <- stamp + 1
                bestChain.Clear()
                bestScore <- 0
                bestHome <- 0
                taken <- 0
                holder.[from] <- -1
                explore i from 0 0 None

                if bestChain.Count > 0 then
                    clock <- clock + 1

                    for struct (m, _) in bestChain do
                        changedAt.[tileOf.[m]] <- clock
                        holder.[tileOf.[m]] <- -1

                    for struct (m, tile) in bestChain do
                        changedAt.[tile] <- clock
                        tileOf.[m] <- tile
                        holder.[tile] <- m

                    changed <- true
                else
                    holder.[from] <- i

    {
        Steps =
            [
                for i in 0 .. n - 1 do
                    if tileOf.[i] <> order.[i].At then
                        yield order.[i].Creep, Tiles.direction order.[i].At tileOf.[i]
            ]
            |> List.sortWith (fun (a, _) (b, _) -> System.String.CompareOrdinal(a, b))
        Searches = searches
    }

let arbitrateWith
    (scratch: Scratch)
    (passable: int -> bool)
    (held: int[])
    (wishes: Wish list)
    : (string * int) list =
    (resolve scratch passable held wishes).Steps

let arbitrate (passable: int -> bool) (held: int[]) (wishes: Wish list) : (string * int) list =
    arbitrateWith (scratch ()) passable held wishes
