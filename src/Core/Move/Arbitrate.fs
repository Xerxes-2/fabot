/// A room's wishes resolved at once into an injective assignment of creeps to
/// tiles: no creep moves outside it. A weighted augmenting search over the
/// room's whole matching: each wish in turn looks for chains of displacements
/// ending on a free tile and takes the best, if it raises the room's score
/// (`Wish.value` summed over every wish) or, at an equal score, puts more
/// bodies back on the tiles they stand on. Swaps and rotations fall out of the
/// search; a body standing in its work area is shuffled within it for free
/// and pushed out of it at its weight and one.
module Fabot.Core.Move.Arbitrate

open Fabot.Core
open Fabot.Core.Grid

/// A room's steps, each moving creep's direction by name, and the searches
/// it took: one per wish offered a turn, the count `scripts/counts` reads.
type Resolution =
    {
        Steps: (string * int) list
        Searches: int
    }

// The tiles a footprint anchored at `anchor` covers.
let private covered (footprint: Footprint) (anchor: int) : int[] =
    match footprint with
    | One -> [| anchor |]
    | Quad -> [| anchor; anchor + 1; anchor + Tiles.side; anchor + Tiles.side + 1 |]

let private inRoom (tile: int) = tile >= 0 && tile < Tiles.count

// The order wishes are offered in: a wish that moves before one that stays
// (a stayer only ever asks back onto its work or its own tile), heavier
// first, then by name, ordinal. The names are unique, so the order is total
// and the input's own order never shows.
let private offered (a: Wish) (b: Wish) =
    match compare (Wish.stays a) (Wish.stays b) with
    | 0 ->
        match compare (max 1 b.Weight) (max 1 a.Weight) with
        | 0 -> System.String.CompareOrdinal(a.Creep, b.Creep)
        | c -> c
    | c -> c

// Whether a chain's (score, bodies home) beats another's: score first, then
// bodies back on their own tiles.
let inline private above (s: int) (h: int) (s0: int) (h0: int) = s > s0 || (s = s0 && h > h0)

/// The room's resolution: `passable` says where a body may stand, `held` the
/// tiles of bodies outside the wishes (theirs, and ours with none). A wish
/// that cannot move, or covers more than a tile, is an obstacle where it
/// stands, as is any wish standing on a tile already held. Guarantees, for
/// every input:
/// - injective: no two bodies, wished or held, end on one tile;
/// - a creep moves only one step, onto a passable tile it listed (head or
///   tail), or back onto the tile it stands on;
/// - every chain taken raises (room score, bodies on their own tile)
///   lexicographically, so the score of the result is above everyone staying
///   unless nobody moves;
/// - of a search's chains the best is taken: highest score, then most bodies
///   home, then fewest bodies displaced, then the first in candidate order;
/// - a displaced body lands on its displacer's `Onward` only when no chain
///   through its other tiles is taken;
/// - at the end no wish could raise (its value, home) by stepping alone onto
///   a free tile it listed: a wish is offered again whenever a tile it lists
///   changed holder since its last search, until a pass takes no chain;
/// - deterministic, and the same for any order of `wishes` and `held`.
let resolve (passable: int -> bool) (held: int[]) (wishes: Wish list) : Resolution =
    let blocked = Array.zeroCreate<bool> Tiles.count

    for tile in held do
        if inRoom tile then
            blocked.[tile] <- true

    for wish in wishes do
        if not (Wish.movable wish) then
            for tile in covered wish.Footprint wish.At do
                if inRoom tile then
                    blocked.[tile] <- true

    let holder = Array.create Tiles.count -1
    let placed = ResizeArray<Wish>()

    for wish in wishes |> List.filter Wish.movable |> List.sortWith offered do
        if inRoom wish.At && not blocked.[wish.At] && holder.[wish.At] < 0 then
            holder.[wish.At] <- placed.Count
            placed.Add wish

    let order = placed.ToArray()
    let n = order.Length
    let tileOf = order |> Array.map (fun wish -> wish.At)

    // Where each may be placed, in its order: head, tail, then the tile it
    // stands on, which is always open to it.
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

            consider wish.Head

            for tile in wish.Tail do
                consider tile

            found.Add at
            found.ToArray())

    // The most a tile it can reach is worth to each.
    let reachable =
        Array.init n (fun i -> candidates.[i] |> Array.map (Wish.value order.[i]) |> Array.max)

    // One search's state: the creeps on the chain being walked, the best
    // (score, home) each creep was reached at, and the best chain so far.
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

    // A chain ended on a free tile at (s, h): counted when it would be taken,
    // kept when it beats the best so far.
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

    // Wish `i` leaving `from` with the chain at (score, home). A creep on the
    // chain is not displaced again; one reached before in this search is
    // explored again only when reached at a better (score, home), since what
    // a chain makes past a creep depends on nothing but the creeps already on
    // it. `onward` is tried last, only when no chain through its other tiles
    // would be taken.
    let rec explore (i: int) (from: int) (score: int) (home: int) (onward: int) =
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

        for tile in candidates.[i] do
            if tile <> from && tile <> onward then
                step tile

        if
            onward >= 0
            && onward <> from
            && taken = before
            && Array.contains onward candidates.[i]
        then
            step onward

        onPath.[i] <- false

    let changedAt = Array.zeroCreate<int> Tiles.count
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
                explore i from 0 0 -1

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

/// Each moving creep's step, by name: `resolve`'s steps.
let arbitrate (passable: int -> bool) (held: int[]) (wishes: Wish list) : (string * int) list =
    (resolve passable held wishes).Steps
